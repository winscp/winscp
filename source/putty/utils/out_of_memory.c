/*
 * Standard implementation of the out_of_memory function called by our
 * malloc wrappers.
 */

#include "putty.h"

void out_of_memory(void)
{
    modalfatalbox("Out of memory");
}
