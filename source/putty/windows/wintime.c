/*
 * wintime.c - Avoid trouble with time() returning (time_t)-1 on Windows.
 */

#include "putty.h"
#include <time.h>

struct tm ltime(void)
{
    SYSTEMTIME st;
    struct tm tm;

    memset(&tm, 0, sizeof(tm));        /* in case there are any other fields */

    GetLocalTime(&st);
    tm.tm_sec=st.wSecond;
    tm.tm_min=st.wMinute;
    tm.tm_hour=st.wHour;
    tm.tm_mday=st.wDay;
    tm.tm_mon=st.wMonth-1;
    tm.tm_year=(st.wYear>=1900?st.wYear-1900:0);
    tm.tm_wday=st.wDayOfWeek;
    tm.tm_yday=-1; /* GetLocalTime doesn't tell us */
    tm.tm_isdst=0; /* GetLocalTime doesn't tell us */
    return tm;
}
