#ifndef PUTTY_PUTTY_H

#include "putty.org.h"

#ifdef PUTTY_LIB

long reg_open_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);
long reg_create_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);

#pragma option push -w-dup
#define RegOpenKey reg_open_winscp_key
#define RegCreateKey reg_create_winscp_key
#pragma option pop

#endif

#endif
