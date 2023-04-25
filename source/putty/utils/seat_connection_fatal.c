/*
 * Wrapper function for the connection_fatal() method of a Seat,
 * providing printf-style formatting.
 */

#include "putty.h"

void seat_connection_fatal(Seat *seat, const char *fmt, ...)
{
    va_list ap;
    char *msg;

    va_start(ap, fmt);
    msg = dupvprintf(fmt, ap);
    va_end(ap);

    seat->vt->connection_fatal(seat, msg);
    sfree(msg);                        /* if we return */
}

