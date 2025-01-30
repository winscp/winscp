/*
 * conf.c: implementation of the internal storage format used for
 * the configuration of a PuTTY session.
 */

#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#include "tree234.h"
#include "putty.h"

/*
 * Configuration keys are primarily integers (big enum of all the
 * different configurable options); some keys have string-designated
 * subkeys, such as the list of environment variables (subkeys
 * defined by the variable names); some have integer-designated
 * subkeys (wordness, colours, preference lists).
 */
struct key {
    int primary;
    union {
        int i;
        char *s;
    } secondary;
};

/* Variant form of struct key which doesn't contain dynamic data, used
 * for lookups. */
struct constkey {
    int primary;
    union {
        int i;
        const char *s;
    } secondary;
};

struct value {
    union {
        bool boolval;
        int intval;
        struct {
            char *str;
            bool utf8;
        } stringval;
        Filename *fileval;
        FontSpec *fontval;
    } u;
};

struct conf_entry {
    struct key key;
    struct value value;
};

struct conf_tag {
    tree234 *tree;
};

/*
 * Because 'struct key' is the first element in 'struct conf_entry',
 * it's safe (guaranteed by the C standard) to cast arbitrarily back
 * and forth between the two types. Therefore, we only need one
 * comparison function, which can double as a main sort function for
 * the tree (comparing two conf_entry structures with each other)
 * and a search function (looking up an externally supplied key).
 */
static int conf_cmp(void *av, void *bv)
{
    struct key *a = (struct key *)av;
    struct key *b = (struct key *)bv;

    if (a->primary < b->primary)
        return -1;
    else if (a->primary > b->primary)
        return +1;
    switch (conf_key_info[a->primary].subkey_type) {
      case CONF_TYPE_INT:
        if (a->secondary.i < b->secondary.i)
            return -1;
        else if (a->secondary.i > b->secondary.i)
            return +1;
        return 0;
      case CONF_TYPE_STR:
      case CONF_TYPE_UTF8:
        return strcmp(a->secondary.s, b->secondary.s);
      case CONF_TYPE_NONE:
        return 0;
      default:
        unreachable("Unsupported subkey type");
    }
}

static int conf_cmp_constkey(void *av, void *bv)
{
    struct key *a = (struct key *)av;
    struct constkey *b = (struct constkey *)bv;

    if (a->primary < b->primary)
        return -1;
    else if (a->primary > b->primary)
        return +1;
    switch (conf_key_info[a->primary].subkey_type) {
      case CONF_TYPE_INT:
        if (a->secondary.i < b->secondary.i)
            return -1;
        else if (a->secondary.i > b->secondary.i)
            return +1;
        return 0;
      case CONF_TYPE_STR:
      case CONF_TYPE_UTF8:
        return strcmp(a->secondary.s, b->secondary.s);
      case CONF_TYPE_NONE:
        return 0;
      default:
        unreachable("Unsupported subkey type");
    }
}

/*
 * Free any dynamic data items pointed to by a 'struct key'. We
 * don't free the structure itself, since it's probably part of a
 * larger allocated block.
 */
static void free_key(struct key *key)
{
    if (conf_key_info[key->primary].subkey_type == CONF_TYPE_STR ||
        conf_key_info[key->primary].subkey_type == CONF_TYPE_UTF8)
        sfree(key->secondary.s);
}

/*
 * Copy a 'struct key' into another one, copying its dynamic data
 * if necessary.
 */
static void copy_key(struct key *to, struct key *from)
{
    to->primary = from->primary;
    switch (conf_key_info[to->primary].subkey_type) {
      case CONF_TYPE_INT:
        to->secondary.i = from->secondary.i;
        break;
      case CONF_TYPE_STR:
      case CONF_TYPE_UTF8:
        to->secondary.s = dupstr(from->secondary.s);
        break;
    }
}

/*
 * Free any dynamic data items pointed to by a 'struct value'. We
 * don't free the value itself, since it's probably part of a larger
 * allocated block.
 */
static void free_value(struct value *val, int type)
{
    if (type == CONF_TYPE_STR || type == CONF_TYPE_UTF8 ||
        type == CONF_TYPE_STR_AMBI)
        sfree(val->u.stringval.str);
    else if (type == CONF_TYPE_FILENAME)
        filename_free(val->u.fileval);
    else if (type == CONF_TYPE_FONT)
        fontspec_free(val->u.fontval);
}

/*
 * Copy a 'struct value' into another one, copying its dynamic data
 * if necessary.
 */
static void copy_value(struct value *to, struct value *from, int type)
{
    switch (type) {
      case CONF_TYPE_BOOL:
        to->u.boolval = from->u.boolval;
        break;
      case CONF_TYPE_INT:
        to->u.intval = from->u.intval;
        break;
      case CONF_TYPE_STR:
      case CONF_TYPE_UTF8:
      case CONF_TYPE_STR_AMBI:
        to->u.stringval.str = dupstr(from->u.stringval.str);
        to->u.stringval.utf8 = from->u.stringval.utf8;
        break;
      case CONF_TYPE_FILENAME:
        to->u.fileval = filename_copy(from->u.fileval);
        break;
      case CONF_TYPE_FONT:
        to->u.fontval = fontspec_copy(from->u.fontval);
        break;
    }
}

/*
 * Free an entire 'struct conf_entry' and its dynamic data.
 */
static void free_entry(struct conf_entry *entry)
{
    free_key(&entry->key);
    free_value(&entry->value, conf_key_info[entry->key.primary].value_type);
    sfree(entry);
}

Conf *conf_new(void)
{
    Conf *conf = snew(struct conf_tag);

    conf->tree = newtree234(conf_cmp);

    return conf;
}

void conf_clear(Conf *conf)
{
    struct conf_entry *entry;

    while ((entry = delpos234(conf->tree, 0)) != NULL)
        free_entry(entry);
}

void conf_free(Conf *conf)
{
    conf_clear(conf);
    freetree234(conf->tree);
    sfree(conf);
}

static void conf_insert(Conf *conf, struct conf_entry *entry)
{
    struct conf_entry *oldentry = add234(conf->tree, entry);
    if (oldentry && oldentry != entry) {
        del234(conf->tree, oldentry);
        free_entry(oldentry);
        oldentry = add234(conf->tree, entry);
        assert(oldentry == entry);
    }
}

void conf_copy_into(Conf *newconf, Conf *oldconf)
{
    struct conf_entry *entry, *entry2;
    int i;

    conf_clear(newconf);

    for (i = 0; (entry = index234(oldconf->tree, i)) != NULL; i++) {
        entry2 = snew(struct conf_entry);
        copy_key(&entry2->key, &entry->key);
        copy_value(&entry2->value, &entry->value,
                   conf_key_info[entry->key.primary].value_type);
        add234(newconf->tree, entry2);
    }
}

Conf *conf_copy(Conf *oldconf)
{
    Conf *newconf = conf_new();

    conf_copy_into(newconf, oldconf);

    return newconf;
}

bool conf_get_bool(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_BOOL);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.boolval;
}

int conf_get_int(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_INT);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.intval;
}

int conf_get_int_int(Conf *conf, int primary, int secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_INT);
    assert(conf_key_info[primary].value_type == CONF_TYPE_INT);
    key.primary = primary;
    key.secondary.i = secondary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.intval;
}

char *conf_get_str(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.stringval.str;
}

char *conf_get_utf8(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_UTF8);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.stringval.str;
}

char *conf_get_str_ambi(Conf *conf, int primary, bool *utf8)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR ||
           conf_key_info[primary].value_type == CONF_TYPE_UTF8 ||
           conf_key_info[primary].value_type == CONF_TYPE_STR_AMBI);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    if (utf8)
        *utf8 = entry->value.u.stringval.utf8;
    return entry->value.u.stringval.str;
}

char *conf_get_str_str_opt(Conf *conf, int primary, const char *secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_STR);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    key.primary = primary;
    key.secondary.s = (char *)secondary;
    entry = find234(conf->tree, &key, NULL);
    return entry ? entry->value.u.stringval.str : NULL;
}

char *conf_get_str_str(Conf *conf, int primary, const char *secondary)
{
    char *ret = conf_get_str_str_opt(conf, primary, secondary);
    assert(ret);
    return ret;
}

char *conf_get_str_strs(Conf *conf, int primary,
                        char *subkeyin, char **subkeyout)
{
    struct constkey key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_STR);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    key.primary = primary;
    if (subkeyin) {
        key.secondary.s = subkeyin;
        entry = findrel234(conf->tree, &key, NULL, REL234_GT);
    } else {
        key.secondary.s = "";
        entry = findrel234(conf->tree, &key, conf_cmp_constkey, REL234_GE);
    }
    if (!entry || entry->key.primary != primary)
        return NULL;
    *subkeyout = entry->key.secondary.s;
    return entry->value.u.stringval.str;
}

char *conf_get_str_nthstrkey(Conf *conf, int primary, int n)
{
    struct constkey key;
    struct conf_entry *entry;
    int index;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_STR);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    key.primary = primary;
    key.secondary.s = "";
    entry = findrelpos234(conf->tree, &key, conf_cmp_constkey,
                          REL234_GE, &index);
    if (!entry || entry->key.primary != primary)
        return NULL;
    entry = index234(conf->tree, index + n);
    if (!entry || entry->key.primary != primary)
        return NULL;
    return entry->key.secondary.s;
}

Filename *conf_get_filename(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_FILENAME);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.fileval;
}

FontSpec *conf_get_fontspec(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_FONT);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.fontval;
}

void conf_set_bool(Conf *conf, int primary, bool value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_BOOL);
    entry->key.primary = primary;
    entry->value.u.boolval = value;
    conf_insert(conf, entry);
}

void conf_set_int(Conf *conf, int primary, int value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_INT);
    entry->key.primary = primary;
    entry->value.u.intval = value;
    conf_insert(conf, entry);
}

void conf_set_int_int(Conf *conf, int primary,
                      int secondary, int value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_INT);
    assert(conf_key_info[primary].value_type == CONF_TYPE_INT);
    entry->key.primary = primary;
    entry->key.secondary.i = secondary;
    entry->value.u.intval = value;
    conf_insert(conf, entry);
}

bool conf_try_set_str(Conf *conf, int primary, const char *value)
{
    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    if (conf_key_info[primary].value_type == CONF_TYPE_UTF8)
        return false;
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR ||
           conf_key_info[primary].value_type == CONF_TYPE_STR_AMBI);

    { // WINSCP
    struct conf_entry *entry = snew(struct conf_entry);
    entry->key.primary = primary;
    entry->value.u.stringval.str = dupstr(value);
    entry->value.u.stringval.utf8 = false;
    conf_insert(conf, entry);
    return true;
    } // WINSCP
}

void conf_set_str(Conf *conf, int primary, const char *value)
{
    bool success = conf_try_set_str(conf, primary, value);
    assert(success && "conf_set_str on CONF_TYPE_UTF8");
}

bool conf_try_set_utf8(Conf *conf, int primary, const char *value)
{
    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    if (conf_key_info[primary].value_type == CONF_TYPE_STR)
        return false;
    assert(conf_key_info[primary].value_type == CONF_TYPE_UTF8 ||
           conf_key_info[primary].value_type == CONF_TYPE_STR_AMBI);

    { // WINSCP
    struct conf_entry *entry = snew(struct conf_entry);
    entry->key.primary = primary;
    entry->value.u.stringval.str = dupstr(value);
    entry->value.u.stringval.utf8 = true;
    conf_insert(conf, entry);
    return true;
    } // WINSCP
}

void conf_set_utf8(Conf *conf, int primary, const char *value)
{
    bool success = conf_try_set_utf8(conf, primary, value);
    assert(success && "conf_set_utf8 on CONF_TYPE_STR");
}

void conf_set_str_str(Conf *conf, int primary, const char *secondary,
                      const char *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_STR);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    entry->key.primary = primary;
    entry->key.secondary.s = dupstr(secondary);
    entry->value.u.stringval.str = dupstr(value);
    entry->value.u.stringval.utf8 = false;
    conf_insert(conf, entry);
}

void conf_del_str_str(Conf *conf, int primary, const char *secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_STR);
    assert(conf_key_info[primary].value_type == CONF_TYPE_STR);
    key.primary = primary;
    key.secondary.s = (char *)secondary;
    entry = find234(conf->tree, &key, NULL);
    if (entry) {
        del234(conf->tree, entry);
        free_entry(entry);
    }
}

void conf_set_filename(Conf *conf, int primary, const Filename *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_FILENAME);
    entry->key.primary = primary;
    entry->value.u.fileval = filename_copy(value);
    conf_insert(conf, entry);
}

void conf_set_fontspec(Conf *conf, int primary, const FontSpec *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(conf_key_info[primary].subkey_type == CONF_TYPE_NONE);
    assert(conf_key_info[primary].value_type == CONF_TYPE_FONT);
    entry->key.primary = primary;
    entry->value.u.fontval = fontspec_copy(value);
    conf_insert(conf, entry);
}

void conf_serialise(BinarySink *bs, Conf *conf)
{
    int i;
    struct conf_entry *entry;

    for (i = 0; (entry = index234(conf->tree, i)) != NULL; i++) {
        put_uint32(bs, entry->key.primary);

        switch (conf_key_info[entry->key.primary].subkey_type) {
          case CONF_TYPE_INT:
            put_uint32(bs, entry->key.secondary.i);
            break;
          case CONF_TYPE_STR:
            put_asciz(bs, entry->key.secondary.s);
            break;
        }
        switch (conf_key_info[entry->key.primary].value_type) {
          case CONF_TYPE_BOOL:
            put_bool(bs, entry->value.u.boolval);
            break;
          case CONF_TYPE_INT:
            put_uint32(bs, entry->value.u.intval);
            break;
          case CONF_TYPE_STR:
          case CONF_TYPE_UTF8:
            put_asciz(bs, entry->value.u.stringval.str);
            break;
          case CONF_TYPE_STR_AMBI:
            put_asciz(bs, entry->value.u.stringval.str);
            put_bool(bs, entry->value.u.stringval.utf8);
            break;
          case CONF_TYPE_FILENAME:
            filename_serialise(bs, entry->value.u.fileval);
            break;
          case CONF_TYPE_FONT:
            fontspec_serialise(bs, entry->value.u.fontval);
            break;
        }
    }

    put_uint32(bs, 0xFFFFFFFFU);
}

bool conf_deserialise(Conf *conf, BinarySource *src)
{
    struct conf_entry *entry;
    unsigned primary;

    while (1) {
        primary = get_uint32(src);

        if (get_err(src))
            return false;
        if (primary == 0xFFFFFFFFU)
            return true;
        if (primary >= N_CONFIG_OPTIONS)
            return false;

        entry = snew(struct conf_entry);
        entry->key.primary = primary;

        switch (conf_key_info[entry->key.primary].subkey_type) {
          case CONF_TYPE_INT:
            entry->key.secondary.i = toint(get_uint32(src));
            break;
          case CONF_TYPE_STR:
            entry->key.secondary.s = dupstr(get_asciz(src));
            break;
        }

        switch (conf_key_info[entry->key.primary].value_type) {
          case CONF_TYPE_BOOL:
            entry->value.u.boolval = get_bool(src);
            break;
          case CONF_TYPE_INT:
            entry->value.u.intval = toint(get_uint32(src));
            break;
          case CONF_TYPE_STR:
            entry->value.u.stringval.str = dupstr(get_asciz(src));
            entry->value.u.stringval.utf8 = false;
            break;
          case CONF_TYPE_UTF8:
            entry->value.u.stringval.str = dupstr(get_asciz(src));
            entry->value.u.stringval.utf8 = true;
            break;
          case CONF_TYPE_STR_AMBI:
            entry->value.u.stringval.str = dupstr(get_asciz(src));
            entry->value.u.stringval.utf8 = get_bool(src);
            break;
          case CONF_TYPE_FILENAME:
            entry->value.u.fileval = filename_deserialise(src);
            break;
          case CONF_TYPE_FONT:
            entry->value.u.fontval = fontspec_deserialise(src);
            break;
        }

        if (get_err(src)) {
            free_entry(entry);
            return false;
        }

        conf_insert(conf, entry);
    }
}
