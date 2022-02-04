/*
 * XOR two pieces of memory, copying the result into a third, which
 * may precisely alias one of the input pair (but no guarantees if it
 * partially overlaps).
 */

#include "defs.h"
#include "misc.h"

void memxor(uint8_t *out, const uint8_t *in1, const uint8_t *in2, size_t size)
{
    switch (size & 15) {
      case 0:
        while (size >= 16) {
            size -= 16;
                   *out++ = *in1++ ^ *in2++;
          case 15: *out++ = *in1++ ^ *in2++;
          case 14: *out++ = *in1++ ^ *in2++;
          case 13: *out++ = *in1++ ^ *in2++;
          case 12: *out++ = *in1++ ^ *in2++;
          case 11: *out++ = *in1++ ^ *in2++;
          case 10: *out++ = *in1++ ^ *in2++;
          case 9:  *out++ = *in1++ ^ *in2++;
          case 8:  *out++ = *in1++ ^ *in2++;
          case 7:  *out++ = *in1++ ^ *in2++;
          case 6:  *out++ = *in1++ ^ *in2++;
          case 5:  *out++ = *in1++ ^ *in2++;
          case 4:  *out++ = *in1++ ^ *in2++;
          case 3:  *out++ = *in1++ ^ *in2++;
          case 2:  *out++ = *in1++ ^ *in2++;
          case 1:  *out++ = *in1++ ^ *in2++;
        }
    }
}
