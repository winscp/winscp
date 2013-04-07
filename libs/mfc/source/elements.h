// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Collection implementation helpers

/*
* The short story:
*   this file contains inline functions that make up the building blocks
*   for implementing the string versions of standard parameterized
*   collection shapes
*
* The long story:
*   Because the implementation of collection classes moves objects around
*   in various ways, it is very inefficient to use only generic C++ constructs.
*   For example, in order to grow an array of FOO objects by one element,
*   you would be forced to allocate a new array of appropriate size, calling
*   the FOO constructor on every element.  Then copy the original array, element
*   by element using a possibly overloaded assignment operator.  Finally destroy
*   the original array element by element.
*   For built-in data types (WORD, DWORD, pointer types), this is complete
*   overkill.  For non-trivial classes (eg: CString in particular) this is
*   a terrible implementation.
*
*   The bottom line: we have two special routines for doing construction
*   and destruction of arrays of special elements - in particular CStrings.
*   The standard templates are parameterized on 'HAS_CREATE' which is
*   non-zero if the collection implementation requires a special
*   construct and destruct function.
*
*   Please note that these are inline overloaded operators, and do not have
*   any form of runtime polymorphism (i.e. nothing is 'virtual').
*/

/////////////////////////////////////////////////////////////////////////////
// Special implementations for CStrings
// it is faster to bit-wise copy a CString than to call an official
//   constructor - since an empty CString can be bit-wise copied

static inline void ConstructElement(CString* pNewData)
{
	memcpy(pNewData, &afxEmptyString, sizeof(CString));
}

static inline void DestructElement(CString* pOldData)
{
	pOldData->~CString();
}

static inline void CopyElement(CString* pSrc, CString* pDest)
{
	*pSrc = *pDest;
}

/////////////////////////////////////////////////////////////////////////////
