/*
                            __  __            _
                         ___\ \/ /_ __   __ _| |_
                        / _ \\  /| '_ \ / _` | __|
                       |  __//  \| |_) | (_| | |_
                        \___/_/\_\ .__/ \__,_|\__|
                                 |_| XML parser

   Copyright (c) 2019      David Loffredo <loffredo@steptools.com>
   Copyright (c) 2019-2026 Sebastian Pipping <sebastian@pipping.org>
   Copyright (c) 2019      Ben Wagner <bungeman@chromium.org>
   Copyright (c) 2019      Vadim Zeitlin <vadim@zeitlins.org>
   Copyright (c) 2026      Matthew Fernandez <matthew.fernandez@gmail.com>
   Licensed under the MIT license:

   Permission is  hereby granted,  free of charge,  to any  person obtaining
   a  copy  of  this  software   and  associated  documentation  files  (the
   "Software"),  to  deal in  the  Software  without restriction,  including
   without  limitation the  rights  to use,  copy,  modify, merge,  publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons  to whom  the Software  is  furnished to  do so,  subject to  the
   following conditions:

   The above copyright  notice and this permission notice  shall be included
   in all copies or substantial portions of the Software.

   THE  SOFTWARE  IS  PROVIDED  "AS  IS",  WITHOUT  WARRANTY  OF  ANY  KIND,
   EXPRESS  OR IMPLIED,  INCLUDING  BUT  NOT LIMITED  TO  THE WARRANTIES  OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR  OTHER LIABILITY, WHETHER  IN AN  ACTION OF CONTRACT,  TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include "random_rand_s.h"

// WINSCP

// rand_s is not in C++Builder
// Have to call semi-documented RtlGenRandom/SystemFunction036 directly
// (what allegedly rand_s does internally anyway)
#include <windows.h>
typedef BOOLEAN (APIENTRY *RTLGENRANDOM_FUNC)(PVOID, ULONG);
HMODULE _Expat_LoadLibrary(LPCTSTR filename);  /* see loadlibrary.c */

bool
writeRandomBytes_rand_s(void *target, size_t count) {
  int success = false;  /* full count bytes written? */
  const HMODULE advapi32 = _Expat_LoadLibrary(TEXT("ADVAPI32.DLL"));

  if (advapi32) {
    const RTLGENRANDOM_FUNC RtlGenRandom
        = (RTLGENRANDOM_FUNC)GetProcAddress(advapi32, "SystemFunction036");
    if (RtlGenRandom) {
      if (RtlGenRandom((PVOID)target, (ULONG)count) == TRUE) {
        success = true;
      }
    }
    FreeLibrary(advapi32);
  }

  return success;
}

// /WINSCP
