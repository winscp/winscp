// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifdef _WIN32
#error This files is not necessary for Win32 build.
#endif

#include "windows.h"

// _WEP for the resource only DLL.

extern "C" int CALLBACK _WEP(int)
{
	return 0;
}
