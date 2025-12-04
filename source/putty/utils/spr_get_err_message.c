/*
 * Construct the error message from a SeatPromptResult, and return it
 * in a dynamically allocated string.
 */

#include "putty.h"

char *spr_get_error_message(SeatPromptResult spr)
{
    strbuf *sb = strbuf_new();
    spr.errfn(spr, BinarySink_UPCAST(sb));
    return strbuf_to_str(sb);
}
