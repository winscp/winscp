/*
 * Helpful wrapper functions around the raw logevent().
 *
 * This source file lives in 'utils' because it's conceptually a
 * convenience utility rather than core functionality. But it can't
 * live in the utils _library_, because then it might refer to
 * logevent() in an earlier library after Unix ld had already finished
 * searching that library, and cause a link failure. So it must live
 * alongside logging.c.
 */

#include "putty.h"

void logevent_and_free(LogContext *ctx, char *event)
{
    logevent(ctx, event);
    sfree(event);
}

void logeventvf(LogContext *ctx, const char *fmt, va_list ap)
{
    logevent_and_free(ctx, dupvprintf(fmt, ap));
}

void logeventf(LogContext *ctx, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    logeventvf(ctx, fmt, ap);
    va_end(ap);
}
