// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

/////////////////////////////////////////////////////////////////////////////
// Platform specific defines

#ifdef _X86_
#define _STACK_INT      int
#define _STACK_LONG     long
#define _STACK_FLOAT    float
#define _STACK_DOUBLE   double
#define _STACK_PTR      void*
#define _SCRATCH_SIZE   16
#define _STACK_OFFSET   0
#define _STACK_MIN      0
#endif

#ifdef _MIPS_
#define _ALIGN_DOUBLES  8
#define _STACK_INT      int
#define _STACK_LONG     long
#define _STACK_FLOAT    float
#define _STACK_DOUBLE   double
#define _STACK_PTR      void*
#define _SCRATCH_SIZE   0
#define _STACK_OFFSET   0
#define _STACK_MIN      32      // 4 32-bit registers
#endif

#ifdef _ALPHA_
#define _ALIGN_STACK    8
#define _STACK_INT      __int64
#define _STACK_LONG     __int64
#define _STACK_FLOAT    double
#define _STACK_DOUBLE   double
#define _STACK_PTR      __int64
#define _SCRATCH_SIZE   0
#define _STACK_OFFSET   48
#define _STACK_MIN      (48+32) // 6 32-bit registers, 32 bytes param space
#endif

#ifdef _PPC_
#define _ALIGN_DOUBLES  8
#define _STACK_INT      int
#define _STACK_LONG     long
#define _STACK_FLOAT    float
#define _STACK_DOUBLE   double
#define _STACK_PTR      void*
#define _SHADOW_DOUBLES 13
#define _SCRATCH_SIZE   (_SHADOW_DOUBLES*sizeof(double))
#define _STACK_OFFSET   0
#define _STACK_MIN      (64+32) // 8 32-bit registers, 32 bytes param space
#define _RETVAL_FIRST
#endif

/////////////////////////////////////////////////////////////////////////////
