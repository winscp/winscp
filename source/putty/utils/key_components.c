#include "ssh.h"
#include "mpint.h"

key_components *key_components_new(void)
{
    key_components *kc = snew(key_components);
    kc->ncomponents = 0;
    kc->componentsize = 0;
    kc->components = NULL;
    return kc;
}

static void key_components_add_str(key_components *kc, const char *name,
                                   KeyComponentType type, ptrlen data)
{
    sgrowarray(kc->components, kc->componentsize, kc->ncomponents);
    { // WINSCP
    size_t n = kc->ncomponents++;
    kc->components[n].name = dupstr(name);
    kc->components[n].type = type;
    kc->components[n].str = strbuf_dup_nm(data);
    } // WINSCP
}

void key_components_add_text(key_components *kc,
                             const char *name, const char *value)
{
    key_components_add_str(kc, name, KCT_TEXT, ptrlen_from_asciz(value));
}

void key_components_add_text_pl(key_components *kc,
                                const char *name, ptrlen value)
{
    key_components_add_str(kc, name, KCT_TEXT, value);
}

void key_components_add_binary(key_components *kc,
                               const char *name, ptrlen value)
{
    key_components_add_str(kc, name, KCT_BINARY, value);
}

void key_components_add_mp(key_components *kc,
                           const char *name, mp_int *value)
{
    sgrowarray(kc->components, kc->componentsize, kc->ncomponents);
    { // WINSCP
    size_t n = kc->ncomponents++;
    kc->components[n].name = dupstr(name);
    kc->components[n].type = KCT_MPINT;
    kc->components[n].mp = mp_copy(value);
    } // WINSCP
}

void key_components_add_uint(key_components *kc,
                             const char *name, uintmax_t value)
{
    mp_int *mpvalue = mp_from_integer(value);
    key_components_add_mp(kc, name, mpvalue);
    mp_free(mpvalue);
}

void key_components_add_copy(key_components *kc,
                             const char *name, const key_component *value)
{
    switch (value->type) {
      case KCT_TEXT:
      case KCT_BINARY:
        key_components_add_str(kc, name, value->type,
                               ptrlen_from_strbuf(value->str));
        break;
      case KCT_MPINT:
        key_components_add_mp(kc, name, value->mp);
        break;
    }
}

void key_components_free(key_components *kc)
{
    size_t i; // WINSCP
    for (i = 0; i < kc->ncomponents; i++) {
        key_component *comp = &kc->components[i];
        sfree(comp->name);
        switch (comp->type) {
          case KCT_MPINT:
            mp_free(comp->mp);
            break;
          case KCT_TEXT:
          case KCT_BINARY:
            strbuf_free(comp->str);
            break;
          default:
            unreachable("bad key component type");
        }
    }
    sfree(kc->components);
    sfree(kc);
}
