/*
 * Functions to deal with ptrlens.
 */

#include "defs.h"
#include "misc.h"

bool ptrlen_eq_string(ptrlen pl, const char *str)
{
    size_t len = strlen(str);
    return (pl.len == len && !memcmp(pl.ptr, str, len));
}

bool ptrlen_eq_ptrlen(ptrlen pl1, ptrlen pl2)
{
    return (pl1.len == pl2.len && !memcmp(pl1.ptr, pl2.ptr, pl1.len));
}

int ptrlen_strcmp(ptrlen pl1, ptrlen pl2)
{
    size_t minlen = pl1.len < pl2.len ? pl1.len : pl2.len;
    if (minlen) {  /* tolerate plX.ptr==NULL as long as plX.len==0 */
        int cmp = memcmp(pl1.ptr, pl2.ptr, minlen);
        if (cmp)
            return cmp;
    }
    return pl1.len < pl2.len ? -1 : pl1.len > pl2.len ? +1 : 0;
}

bool ptrlen_startswith(ptrlen whole, ptrlen prefix, ptrlen *tail)
{
    if (whole.len >= prefix.len &&
        !memcmp(whole.ptr, prefix.ptr, prefix.len)) {
        if (tail) {
            tail->ptr = (const char *)whole.ptr + prefix.len;
            tail->len = whole.len - prefix.len;
        }
        return true;
    }
    return false;
}

bool ptrlen_endswith(ptrlen whole, ptrlen suffix, ptrlen *tail)
{
    if (whole.len >= suffix.len &&
        !memcmp((char *)whole.ptr + (whole.len - suffix.len),
                suffix.ptr, suffix.len)) {
        if (tail) {
            tail->ptr = whole.ptr;
            tail->len = whole.len - suffix.len;
        }
        return true;
    }
    return false;
}

ptrlen ptrlen_get_word(ptrlen *input, const char *separators)
{
    const char *p = input->ptr, *end = p + input->len;
    ptrlen toret;

    while (p < end && strchr(separators, *p))
        p++;
    toret.ptr = p;
    while (p < end && !strchr(separators, *p))
        p++;
    toret.len = p - (const char *)toret.ptr;

    size_t to_consume = p - (const char *)input->ptr;
    assert(to_consume <= input->len);
    input->ptr = (const char *)input->ptr + to_consume;
    input->len -= to_consume;

    return toret;
}

char *mkstr(ptrlen pl)
{
    char *p = snewn(pl.len + 1, char);
    memcpy(p, pl.ptr, pl.len);
    p[pl.len] = '\0';
    return p;
}

bool strstartswith(const char *s, const char *t)
{
    return !strncmp(s, t, strlen(t));
}

bool strendswith(const char *s, const char *t)
{
    size_t slen = strlen(s), tlen = strlen(t);
    return slen >= tlen && !strcmp(s + (slen - tlen), t);
}
