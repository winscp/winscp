/*
 * ldisc.h: defines the Ldisc data structure used by ldisc.c and
 * ldiscucs.c. (Unfortunately it was necessary to split the ldisc
 * module in two, to avoid unnecessarily linking in the Unicode
 * stuff in tools that don't require it.)
 */

#ifndef PUTTY_LDISC_H
#define PUTTY_LDISC_H

typedef struct ldisc_tag {
    Terminal *term;
    Backend *back;
    Config *cfg;
    void *backhandle;
    void *frontend;

    char *buf;
    int buflen, bufsiz, quotenext;
} *Ldisc;

#endif /* PUTTY_LDISC_H */
