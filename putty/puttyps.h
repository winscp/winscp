#ifndef PUTTY_PUTTYPS_H
#define PUTTY_PUTTYPS_H

#ifdef _WINDOWS

#include <windows.h>
#include "winstuff.h"

#elif defined(macintosh)

#include "macstuff.h"

#else

#include "unix.h"

#endif

#endif
