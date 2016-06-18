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
 * Enumeration of types used in keys and values.
 */
typedef enum { TYPE_NONE, TYPE_INT, TYPE_STR, TYPE_FILENAME, TYPE_FONT } Type;

/*
 * Arrays which allow us to look up the subkey and value types for a
 * given primary key id.
 */
#define CONF_SUBKEYTYPE_DEF(valtype, keytype, keyword) TYPE_ ## keytype,
static int subkeytypes[] = { CONFIG_OPTIONS(CONF_SUBKEYTYPE_DEF) };
#define CONF_VALUETYPE_DEF(valtype, keytype, keyword) TYPE_ ## valtype,
static int valuetypes[] = { CONFIG_OPTIONS(CONF_VALUETYPE_DEF) };

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

struct value {
    union {
	int intval;
	char *stringval;
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
    switch (subkeytypes[a->primary]) {
      case TYPE_INT:
	if (a->secondary.i < b->secondary.i)
	    return -1;
	else if (a->secondary.i > b->secondary.i)
	    return +1;
	return 0;
      case TYPE_STR:
	return strcmp(a->secondary.s, b->secondary.s);
      default:
	return 0;
    }
}

/*
 * Free any dynamic data items pointed to by a 'struct key'. We
 * don't free the structure itself, since it's probably part of a
 * larger allocated block.
 */
static void free_key(struct key *key)
{
    if (subkeytypes[key->primary] == TYPE_STR)
	sfree(key->secondary.s);
}

/*
 * Copy a 'struct key' into another one, copying its dynamic data
 * if necessary.
 */
static void copy_key(struct key *to, struct key *from)
{
    to->primary = from->primary;
    switch (subkeytypes[to->primary]) {
      case TYPE_INT:
	to->secondary.i = from->secondary.i;
	break;
      case TYPE_STR:
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
    if (type == TYPE_STR)
	sfree(val->u.stringval);
    else if (type == TYPE_FILENAME)
	filename_free(val->u.fileval);
    else if (type == TYPE_FONT)
	fontspec_free(val->u.fontval);
}

/*
 * Copy a 'struct value' into another one, copying its dynamic data
 * if necessary.
 */
static void copy_value(struct value *to, struct value *from, int type)
{
    switch (type) {
      case TYPE_INT:
	to->u.intval = from->u.intval;
	break;
      case TYPE_STR:
	to->u.stringval = dupstr(from->u.stringval);
	break;
      case TYPE_FILENAME:
	to->u.fileval = filename_copy(from->u.fileval);
	break;
      case TYPE_FONT:
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
    free_value(&entry->value, valuetypes[entry->key.primary]);
    sfree(entry);
}

Conf *conf_new(void)
{
    Conf *conf = snew(struct conf_tag);

    conf->tree = newtree234(conf_cmp);

    return conf;
}

static void conf_clear(Conf *conf)
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
		   valuetypes[entry->key.primary]);
	add234(newconf->tree, entry2);
    }
}

Conf *conf_copy(Conf *oldconf)
{
    Conf *newconf = conf_new();

    conf_copy_into(newconf, oldconf);

    return newconf;
}

int conf_get_int(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_INT);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.intval;
}

int conf_get_int_int(Conf *conf, int primary, int secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_INT);
    assert(valuetypes[primary] == TYPE_INT);
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

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_STR);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.stringval;
}

char *conf_get_str_str_opt(Conf *conf, int primary, const char *secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_STR);
    assert(valuetypes[primary] == TYPE_STR);
    key.primary = primary;
    key.secondary.s = (char *)secondary;
    entry = find234(conf->tree, &key, NULL);
    return entry ? entry->value.u.stringval : NULL;
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
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_STR);
    assert(valuetypes[primary] == TYPE_STR);
    key.primary = primary;
    if (subkeyin) {
	key.secondary.s = subkeyin;
	entry = findrel234(conf->tree, &key, NULL, REL234_GT);
    } else {
	key.secondary.s = "";
	entry = findrel234(conf->tree, &key, NULL, REL234_GE);
    }
    if (!entry || entry->key.primary != primary)
	return NULL;
    *subkeyout = entry->key.secondary.s;
    return entry->value.u.stringval;
}

char *conf_get_str_nthstrkey(Conf *conf, int primary, int n)
{
    struct key key;
    struct conf_entry *entry;
    int index;

    assert(subkeytypes[primary] == TYPE_STR);
    assert(valuetypes[primary] == TYPE_STR);
    key.primary = primary;
    key.secondary.s = "";
    entry = findrelpos234(conf->tree, &key, NULL, REL234_GE, &index);
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

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_FILENAME);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.fileval;
}

FontSpec *conf_get_fontspec(Conf *conf, int primary)
{
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_FONT);
    key.primary = primary;
    entry = find234(conf->tree, &key, NULL);
    assert(entry);
    return entry->value.u.fontval;
}

void conf_set_int(Conf *conf, int primary, int value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_INT);
    entry->key.primary = primary;
    entry->value.u.intval = value; 
    conf_insert(conf, entry);
}

void conf_set_int_int(Conf *conf, int primary, int secondary, int value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(subkeytypes[primary] == TYPE_INT);
    assert(valuetypes[primary] == TYPE_INT);
    entry->key.primary = primary;
    entry->key.secondary.i = secondary;
    entry->value.u.intval = value;
    conf_insert(conf, entry);
}

void conf_set_str(Conf *conf, int primary, const char *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_STR);
    entry->key.primary = primary;
    entry->value.u.stringval = dupstr(value);
    conf_insert(conf, entry);
}

void conf_set_str_str(Conf *conf, int primary, const char *secondary,
		      const char *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(subkeytypes[primary] == TYPE_STR);
    assert(valuetypes[primary] == TYPE_STR);
    entry->key.primary = primary;
    entry->key.secondary.s = dupstr(secondary);
    entry->value.u.stringval = dupstr(value);
    conf_insert(conf, entry);
}

void conf_del_str_str(Conf *conf, int primary, const char *secondary)
{
    struct key key;
    struct conf_entry *entry;

    assert(subkeytypes[primary] == TYPE_STR);
    assert(valuetypes[primary] == TYPE_STR);
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

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_FILENAME);
    entry->key.primary = primary;
    entry->value.u.fileval = filename_copy(value);
    conf_insert(conf, entry);
}

void conf_set_fontspec(Conf *conf, int primary, const FontSpec *value)
{
    struct conf_entry *entry = snew(struct conf_entry);

    assert(subkeytypes[primary] == TYPE_NONE);
    assert(valuetypes[primary] == TYPE_FONT);
    entry->key.primary = primary;
    entry->value.u.fontval = fontspec_copy(value);
    conf_insert(conf, entry);
}

int conf_serialised_size(Conf *conf)
{
    int i;
    struct conf_entry *entry;
    int size = 0;

    for (i = 0; (entry = index234(conf->tree, i)) != NULL; i++) {
	size += 4;   /* primary key */
	switch (subkeytypes[entry->key.primary]) {
	  case TYPE_INT:
	    size += 4;
	    break;
	  case TYPE_STR:
	    size += 1 + strlen(entry->key.secondary.s);
	    break;
	}
	switch (valuetypes[entry->key.primary]) {
	  case TYPE_INT:
	    size += 4;
	    break;
	  case TYPE_STR:
	    size += 1 + strlen(entry->value.u.stringval);
	    break;
	  case TYPE_FILENAME:
	    size += filename_serialise(entry->value.u.fileval, NULL);
	    break;
	  case TYPE_FONT:
	    size += fontspec_serialise(entry->value.u.fontval, NULL);
	    break;
	}
    }

    size += 4;			       /* terminator value */

    return size;
}

void conf_serialise(Conf *conf, void *vdata)
{
    unsigned char *data = (unsigned char *)vdata;
    int i, len;
    struct conf_entry *entry;

    for (i = 0; (entry = index234(conf->tree, i)) != NULL; i++) {
	PUT_32BIT_MSB_FIRST(data, entry->key.primary);
	data += 4;

	switch (subkeytypes[entry->key.primary]) {
	  case TYPE_INT:
	    PUT_32BIT_MSB_FIRST(data, entry->key.secondary.i);
	    data += 4;
	    break;
	  case TYPE_STR:
	    len = strlen(entry->key.secondary.s);
	    memcpy(data, entry->key.secondary.s, len);
	    data += len;
	    *data++ = 0;
	    break;
	}
	switch (valuetypes[entry->key.primary]) {
	  case TYPE_INT:
	    PUT_32BIT_MSB_FIRST(data, entry->value.u.intval);
	    data += 4;
	    break;
	  case TYPE_STR:
	    len = strlen(entry->value.u.stringval);
	    memcpy(data, entry->value.u.stringval, len);
	    data += len;
	    *data++ = 0;
	    break;
	  case TYPE_FILENAME:
            data += filename_serialise(entry->value.u.fileval, data);
	    break;
	  case TYPE_FONT:
            data += fontspec_serialise(entry->value.u.fontval, data);
	    break;
	}
    }

    PUT_32BIT_MSB_FIRST(data, 0xFFFFFFFFU);
}

int conf_deserialise(Conf *conf, void *vdata, int maxsize)
{
    unsigned char *data = (unsigned char *)vdata;
    unsigned char *start = data;
    struct conf_entry *entry;
    unsigned primary;
    int used;
    unsigned char *zero;

    while (maxsize >= 4) {
	primary = GET_32BIT_MSB_FIRST(data);
	data += 4, maxsize -= 4;

	if (primary >= N_CONFIG_OPTIONS)
	    break;

	entry = snew(struct conf_entry);
	entry->key.primary = primary;

	switch (subkeytypes[entry->key.primary]) {
	  case TYPE_INT:
	    if (maxsize < 4) {
		sfree(entry);
		goto done;
	    }
	    entry->key.secondary.i = toint(GET_32BIT_MSB_FIRST(data));
	    data += 4, maxsize -= 4;
	    break;
	  case TYPE_STR:
	    zero = memchr(data, 0, maxsize);
	    if (!zero) {
		sfree(entry);
		goto done;
	    }
	    entry->key.secondary.s = dupstr((char *)data);
	    maxsize -= (zero + 1 - data);
	    data = zero + 1;
	    break;
	}

	switch (valuetypes[entry->key.primary]) {
	  case TYPE_INT:
	    if (maxsize < 4) {
		if (subkeytypes[entry->key.primary] == TYPE_STR)
		    sfree(entry->key.secondary.s);
		sfree(entry);
		goto done;
	    }
	    entry->value.u.intval = toint(GET_32BIT_MSB_FIRST(data));
	    data += 4, maxsize -= 4;
	    break;
	  case TYPE_STR:
	    zero = memchr(data, 0, maxsize);
	    if (!zero) {
		if (subkeytypes[entry->key.primary] == TYPE_STR)
		    sfree(entry->key.secondary.s);
		sfree(entry);
		goto done;
	    }
	    entry->value.u.stringval = dupstr((char *)data);
	    maxsize -= (zero + 1 - data);
	    data = zero + 1;
	    break;
	  case TYPE_FILENAME:
            entry->value.u.fileval =
                filename_deserialise(data, maxsize, &used);
            if (!entry->value.u.fileval) {
		if (subkeytypes[entry->key.primary] == TYPE_STR)
		    sfree(entry->key.secondary.s);
		sfree(entry);
		goto done;
	    }
	    data += used;
	    maxsize -= used;
	    break;
	  case TYPE_FONT:
            entry->value.u.fontval =
                fontspec_deserialise(data, maxsize, &used);
            if (!entry->value.u.fontval) {
		if (subkeytypes[entry->key.primary] == TYPE_STR)
		    sfree(entry->key.secondary.s);
		sfree(entry);
		goto done;
	    }
	    data += used;
	    maxsize -= used;
	    break;
	}
	conf_insert(conf, entry);
    }

    done:
    return (int)(data - start);
}
