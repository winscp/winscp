/*
 * Helper routines for dealing with SeatDialogText structures.
 */

#include <stdarg.h>

#include "putty.h"

SeatDialogText *seat_dialog_text_new(void)
{
    SeatDialogText *sdt = snew(SeatDialogText);
    sdt->nitems = sdt->itemsize = 0;
    sdt->items = NULL;
    return sdt;
}

void seat_dialog_text_free(SeatDialogText *sdt)
{
    for (size_t i = 0; i < sdt->nitems; i++)
        sfree(sdt->items[i].text);
    sfree(sdt->items);
    sfree(sdt);
}

static void seat_dialog_text_append_v(
    SeatDialogText *sdt, SeatDialogTextType type, const char *fmt, va_list ap)
{
    sgrowarray(sdt->items, sdt->itemsize, sdt->nitems);
    SeatDialogTextItem *item = &sdt->items[sdt->nitems++];
    item->type = type;
    item->text = dupvprintf(fmt, ap);
}

void seat_dialog_text_append(SeatDialogText *sdt, SeatDialogTextType type,
                             const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    seat_dialog_text_append_v(sdt, type, fmt, ap);
    va_end(ap);
}
