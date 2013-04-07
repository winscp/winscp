// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"

#ifdef AFX_OLE5_SEG
#pragma code_seg(AFX_OLE5_SEG)
#endif

// Note: because of the nature of these functions, it is not possible
//  to create a 'C' or 'C++' version of them.  These functions are used
//  for the lowest level of the OLE IDispatch implementation, and need
//  to be ported to each supported platform.

extern "C" {

/////////////////////////////////////////////////////////////////////////////
// Intel 386 version

#ifdef _X86_

__declspec(naked) void AFXAPI
_AfxDispatchCall(AFX_PMSG /*pfn*/, void* /*pArgs*/, UINT /*nSizeArgs*/)
{
#if defined(__BORLANDC__)
/*

  Note: This routine is specific to the Borland C++ 5.x
  Pointer-to-Member-Function (PFM) binary layout.  By default, -Vmv is on
  (member pointers have no restrictions) and thus the layout in 32-bit apps
  is 12-bytes.  If any of the other -Vm... switches are used, this routine
  may have to be modified to support a different size PMF.  We recommend
  that -Vmv always be used.

*/
    __emit__(0x5A,             	    // pop  edx         ; edx = return address
             0x58,             	    // pop  eax         ; eax = pfn (PMF addr)
             0x59,             	    // pop  ecx         ; (extra PMF dword #1)
             0x59,             	    // pop  ecx         ; (extra PMF dword #2)
             0x59,             	    // pop  ecx         ; ecx = pArgs
	     0x50,             	    // push eax
	     0x8B,0x44,0x24,0x04,   // mov  eax,[esp+4] ; eax = nSizeArgs
	     0x03,0xC8,        	    // add  ecx,eax     ; ecx += nSizeArgs (=scratch area)
	     0x89,0x19,        	    // mov  [ecx],ebx   ; scratch[0] = ebx
	     0x89,0x51,0x04,   	    // mov  [ecx+4],edx ; scratch[1] = return address
	     0x8B,0xD8,             // mov  ebx,eax     ; ebx = nSizeArgs (saved across call)
	     0x58,             	    // pop  eax
             0x2B,0x0C,0x24,   	    // sub  ecx,[esp]   ; ecx = pArgs (again)
             0x8B,0xE1,        	    // mov  esp,ecx     ; esp = pArgs (usually already correct)
             '\xFF',0xD0,      	    // call eax         ; call member function
	     0x03,0xE3,             // add  esp,ebx     ; cleanup arguments
	     0x5B,                  // pop  ebx         ; restore ebx
             0xC3                   // ret              ; esp[0] should = scratch[0] = return address
            );
#else
	_asm
	{
		pop     edx         // edx = return address
		pop     eax         // eax = pfn
		pop     ecx         // ecx = pArgs
		add     ecx,[esp]   // ecx += nSizeArgs (=scratch area)
		mov     [ecx],edx   // scratch[0] = return address
		sub     ecx,[esp]   // ecx = pArgs (again)
		mov     esp,ecx     // esp = pArgs (usually already correct)
		pop     ecx         // ecx = this pointer (the CCmdTarget*)
		call    eax         // call member function
		ret                 // esp[0] should = scratch[0] = return address
	}
#endif
}
#endif // _X86_

/////////////////////////////////////////////////////////////////////////////
// MIPS R4000 version

#ifdef _MIPS_

extern "C" void _asm(char *, ...);
void AFXAPI
_AfxDispatchCall(AFX_PMSG /*pfn*/, void* /*pArgs*/, UINT /*nSizeArgs*/)
{
	_asm("addiu     %sp,%a1,0x0");      // sp = pArgs
	_asm("addiu     %t6,%a0,0x0");      // t6 = pfn (save it)
	_asm("lw        %a0,0x0(%sp)");     // a0 = param0
	_asm("lw        %a1,0x4(%sp)");     // a1 = param1
	_asm("lw        %a2,0x8(%sp)");     // a2 = param2
	_asm("lw        %a3,0xc(%sp)");     // a3 = param3
	_asm("j         %t6");              // ip = pfn (jump to target function)
}

#endif // _MIPS_

/////////////////////////////////////////////////////////////////////////////
// DEC Alpha AXP version

#ifdef _ALPHA_

// Note: ALPHA version is in src\alpha\olecall_.s

// The ALPHA compiler does not support inline assembly, so it
//  must be build separately with the ASAXP assembler.

#endif // _ALPHA_

} // end extern "C" block

/////////////////////////////////////////////////////////////////////////////
