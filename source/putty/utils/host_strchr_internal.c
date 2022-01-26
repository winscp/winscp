/*
 * Find a character in a string, unless it's a colon contained within
 * square brackets. Used for untangling strings of the form
 * 'host:port', where host can be an IPv6 literal.
 *
 * This internal function provides an API that's a bit like strchr (in
 * that it returns a pointer to the character it found, or NULL), and
 * a bit like strcspn (in that you can give it a set of characters to
 * look for, not just one). Also it has an option to return the first
 * or last character it finds. Other functions in the utils directory
 * provide wrappers on it with APIs more like familiar <string.h>
 * functions.
 */

#include <stdbool.h>
#include <string.h>

#include "defs.h"
#include "misc.h"
#include "utils/utils.h"

const char *host_strchr_internal(const char *s, const char *set, bool first)
{
    int brackets = 0;
    const char *ret = NULL;

    while (1) {
        if (!*s)
            return ret;

        if (*s == '[')
            brackets++;
        else if (*s == ']' && brackets > 0)
            brackets--;
        else if (brackets && *s == ':')
            /* never match */ ;
        else if (strchr(set, *s)) {
            ret = s;
            if (first)
                return ret;
        }

        s++;
    }
}

#ifdef TEST

int main(void)
{
    int passes = 0, fails = 0;

#define TEST1(func, string, arg2, suffix, result) do                    \
    {                                                                   \
        const char *str = string;                                       \
        unsigned ret = func(str, arg2) suffix;                          \
        if (ret == result) {                                            \
            passes++;                                                   \
        } else {                                                        \
            printf("fail: %s(%s,%s)%s = %u, expected %u\n",             \
                   #func, #string, #arg2, #suffix, ret,                 \
                   (unsigned)result);                                   \
            fails++;                                                    \
        }                                                               \
} while (0)

    TEST1(host_strchr, "[1:2:3]:4:5", ':', -str, 7);
    TEST1(host_strrchr, "[1:2:3]:4:5", ':', -str, 9);
    TEST1(host_strcspn, "[1:2:3]:4:5", "/:",, 7);
    TEST1(host_strchr, "[1:2:3]", ':', == NULL, 1);
    TEST1(host_strrchr, "[1:2:3]", ':', == NULL, 1);
    TEST1(host_strcspn, "[1:2:3]", "/:",, 7);
    TEST1(host_strcspn, "[1:2/3]", "/:",, 4);
    TEST1(host_strcspn, "[1:2:3]/", "/:",, 7);

    printf("passed %d failed %d total %d\n", passes, fails, passes+fails);
    return fails != 0 ? 1 : 0;
}

#endif /* TEST */
