/*
 * Wrapper around the Windows FormatMessage system for retrieving the
 * text of a system error code, with a simple API similar to strerror.
 *
 * Works by keeping a tree234 containing mappings from system error
 * codes to strings. Entries allocated in this tree are simply never
 * freed.
 *
 * Also, the returned string has its trailing newline removed (so it
 * can go in places like the Event Log that never want a newline), and
 * is prefixed with the error number (so that if a user sends an error
 * report containing a translated error message we can't read, we can
 * still find out what the error actually was).
 */

#include "putty.h"

struct errstring {
    int error;
    char *text;
};

static int errstring_find(void *av, void *bv)
{
    int *a = (int *)av;
    struct errstring *b = (struct errstring *)bv;
    if (*a < b->error)
        return -1;
    if (*a > b->error)
        return +1;
    return 0;
}
static int errstring_compare(void *av, void *bv)
{
    struct errstring *a = (struct errstring *)av;
    return errstring_find(&a->error, bv);
}

static tree234 *errstrings = NULL;

const char *win_strerror(int error)
{
    struct errstring *es;

    if (!errstrings)
        errstrings = newtree234(errstring_compare);

    es = find234(errstrings, &error, errstring_find);

    if (!es) {
        char msgtext[65536]; /* maximum size for FormatMessage is 64K */

        es = snew(struct errstring);
        es->error = error;
        if (!FormatMessage((FORMAT_MESSAGE_FROM_SYSTEM |
                            FORMAT_MESSAGE_IGNORE_INSERTS), NULL, error,
                           MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                           msgtext, lenof(msgtext)-1, NULL)) {
            sprintf(msgtext,
                    "(unable to format: FormatMessage returned %u)",
                    (unsigned int)GetLastError());
        } else {
            int len = strlen(msgtext);
            if (len > 0 && msgtext[len-1] == '\n')
                msgtext[len-1] = '\0';
        }
        es->text = dupprintf("Error %d: %s", error, msgtext);
        add234(errstrings, es);
    }

    return es->text;
}
