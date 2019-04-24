/*
 * Header for int64.c.
 */

#ifndef PUTTY_INT64_H
#define PUTTY_INT64_H

#include "defs.h"

typedef struct {
    unsigned long hi, lo;
} uint64;

uint64 uint64_div10(uint64 x, int *remainder);
void uint64_decimal(uint64 x, char *buffer);
uint64 uint64_make(unsigned long hi, unsigned long lo);
uint64 uint64_add(uint64 x, uint64 y);
uint64 uint64_add32(uint64 x, unsigned long y);
int uint64_compare(uint64 x, uint64 y);
uint64 uint64_subtract(uint64 x, uint64 y);
double uint64_to_double(uint64 x);
uint64 uint64_shift_right(uint64 x, int shift);
uint64 uint64_shift_left(uint64 x, int shift);
uint64 uint64_from_decimal(const char *str);

void BinarySink_put_uint64(BinarySink *, uint64);
uint64 BinarySource_get_uint64(BinarySource *);

#endif
