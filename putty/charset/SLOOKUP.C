/*
 * slookup.c - static lookup of character sets.
 */

#include "charset.h"
#include "internal.h"

#define ENUM_CHARSET(x) extern charset_spec const charset_##x;
#include "enum.c"
#undef ENUM_CHARSET

static charset_spec const *const cs_table[] = {

#define ENUM_CHARSET(x) &charset_##x,
#include "enum.c"
#undef ENUM_CHARSET

};

charset_spec const *charset_find_spec(int charset)
{
    int i;

    for (i = 0; i < (int)lenof(cs_table); i++)
	if (cs_table[i]->charset == charset)
	    return cs_table[i];

    return NULL;
}
