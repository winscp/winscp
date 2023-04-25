/*
 * Constructor function for a SeatPromptResult of the 'software abort'
 * category, whose error message is in the simplest possible form of a
 * static string constant.
 */

#include "putty.h"

static void spr_static_errfn(SeatPromptResult spr, BinarySink *bs)
{
    put_dataz(bs, spr.errdata_lit);
}

SeatPromptResult make_spr_sw_abort_static(const char *str)
{
    SeatPromptResult spr;
    spr.kind = SPRK_SW_ABORT;
    spr.errfn = spr_static_errfn;
    spr.errdata_lit = str;
    return spr;
}
