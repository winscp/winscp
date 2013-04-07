// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXCOLL.H

#ifdef _AFXCOLL_INLINE

////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CByteArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CByteArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CByteArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE BYTE CByteArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CByteArray::SetAt(int nIndex, BYTE newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE BYTE& CByteArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const BYTE* CByteArray::GetData() const
	{ return (const BYTE*)m_pData; }
_AFXCOLL_INLINE BYTE* CByteArray::GetData()
	{ return (BYTE*)m_pData; }
_AFXCOLL_INLINE int CByteArray::Add(BYTE newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE BYTE CByteArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE BYTE& CByteArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CWordArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CWordArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CWordArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE WORD CWordArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CWordArray::SetAt(int nIndex, WORD newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE WORD& CWordArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const WORD* CWordArray::GetData() const
	{ return (const WORD*)m_pData; }
_AFXCOLL_INLINE WORD* CWordArray::GetData()
	{ return (WORD*)m_pData; }
_AFXCOLL_INLINE int CWordArray::Add(WORD newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE WORD CWordArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE WORD& CWordArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CDWordArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CDWordArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CDWordArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE DWORD CDWordArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CDWordArray::SetAt(int nIndex, DWORD newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE DWORD& CDWordArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const DWORD* CDWordArray::GetData() const
	{ return (const DWORD*)m_pData; }
_AFXCOLL_INLINE DWORD* CDWordArray::GetData()
	{ return (DWORD*)m_pData; }
_AFXCOLL_INLINE int CDWordArray::Add(DWORD newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE DWORD CDWordArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE DWORD& CDWordArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CUIntArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CUIntArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CUIntArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE UINT CUIntArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CUIntArray::SetAt(int nIndex, UINT newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE UINT& CUIntArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const UINT* CUIntArray::GetData() const
	{ return (const UINT*)m_pData; }
_AFXCOLL_INLINE UINT* CUIntArray::GetData()
	{ return (UINT*)m_pData; }
_AFXCOLL_INLINE int CUIntArray::Add(UINT newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE UINT CUIntArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE UINT& CUIntArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CPtrArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CPtrArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CPtrArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE void* CPtrArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CPtrArray::SetAt(int nIndex, void* newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE void*& CPtrArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const void** CPtrArray::GetData() const
	{ return (const void**)m_pData; }
_AFXCOLL_INLINE void** CPtrArray::GetData()
	{ return (void**)m_pData; }
_AFXCOLL_INLINE int CPtrArray::Add(void* newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE void* CPtrArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE void*& CPtrArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CObArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CObArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CObArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE CObject* CObArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CObArray::SetAt(int nIndex, CObject* newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE CObject*& CObArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const CObject** CObArray::GetData() const
	{ return (const CObject**)m_pData; }
_AFXCOLL_INLINE CObject** CObArray::GetData()
	{ return (CObject**)m_pData; }
_AFXCOLL_INLINE int CObArray::Add(CObject* newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE CObject* CObArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE CObject*& CObArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CStringArray::GetSize() const
	{ return m_nSize; }
_AFXCOLL_INLINE int CStringArray::GetUpperBound() const
	{ return m_nSize-1; }
_AFXCOLL_INLINE void CStringArray::RemoveAll()
	{ SetSize(0); }
_AFXCOLL_INLINE CString CStringArray::GetAt(int nIndex) const
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE void CStringArray::SetAt(int nIndex, LPCTSTR newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE void CStringArray::SetAt(int nIndex, const CString& newElement)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		m_pData[nIndex] = newElement; }

_AFXCOLL_INLINE CString& CStringArray::ElementAt(int nIndex)
	{ ASSERT(nIndex >= 0 && nIndex < m_nSize);
		return m_pData[nIndex]; }
_AFXCOLL_INLINE const CString* CStringArray::GetData() const
	{ return (const CString*)m_pData; }
_AFXCOLL_INLINE CString* CStringArray::GetData()
	{ return (CString*)m_pData; }
_AFXCOLL_INLINE int CStringArray::Add(LPCTSTR newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE int CStringArray::Add(const CString& newElement)
	{ int nIndex = m_nSize;
		SetAtGrow(nIndex, newElement);
		return nIndex; }

_AFXCOLL_INLINE CString CStringArray::operator[](int nIndex) const
	{ return GetAt(nIndex); }
_AFXCOLL_INLINE CString& CStringArray::operator[](int nIndex)
	{ return ElementAt(nIndex); }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CPtrList::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CPtrList::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void*& CPtrList::GetHead()
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE void* CPtrList::GetHead() const
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE void*& CPtrList::GetTail()
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE void* CPtrList::GetTail() const
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE POSITION CPtrList::GetHeadPosition() const
	{ return (POSITION) m_pNodeHead; }
_AFXCOLL_INLINE POSITION CPtrList::GetTailPosition() const
	{ return (POSITION) m_pNodeTail; }
_AFXCOLL_INLINE void*& CPtrList::GetNext(POSITION& rPosition) // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE void* CPtrList::GetNext(POSITION& rPosition) const // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE void*& CPtrList::GetPrev(POSITION& rPosition) // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE void* CPtrList::GetPrev(POSITION& rPosition) const // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE void*& CPtrList::GetAt(POSITION position)
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE void* CPtrList::GetAt(POSITION position) const
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE void CPtrList::SetAt(POSITION pos, void* newElement)
	{ CNode* pNode = (CNode*) pos;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode->data = newElement; }



////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CObList::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CObList::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE CObject*& CObList::GetHead()
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE CObject* CObList::GetHead() const
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE CObject*& CObList::GetTail()
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE CObject* CObList::GetTail() const
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE POSITION CObList::GetHeadPosition() const
	{ return (POSITION) m_pNodeHead; }
_AFXCOLL_INLINE POSITION CObList::GetTailPosition() const
	{ return (POSITION) m_pNodeTail; }
_AFXCOLL_INLINE CObject*& CObList::GetNext(POSITION& rPosition) // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE CObject* CObList::GetNext(POSITION& rPosition) const // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE CObject*& CObList::GetPrev(POSITION& rPosition) // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE CObject* CObList::GetPrev(POSITION& rPosition) const // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE CObject*& CObList::GetAt(POSITION position)
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE CObject* CObList::GetAt(POSITION position) const
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE void CObList::SetAt(POSITION pos, CObject* newElement)
	{ CNode* pNode = (CNode*) pos;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode->data = newElement; }



////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CStringList::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CStringList::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE CString& CStringList::GetHead()
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE CString CStringList::GetHead() const
	{ ASSERT(m_pNodeHead != NULL);
		return m_pNodeHead->data; }
_AFXCOLL_INLINE CString& CStringList::GetTail()
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE CString CStringList::GetTail() const
	{ ASSERT(m_pNodeTail != NULL);
		return m_pNodeTail->data; }
_AFXCOLL_INLINE POSITION CStringList::GetHeadPosition() const
	{ return (POSITION) m_pNodeHead; }
_AFXCOLL_INLINE POSITION CStringList::GetTailPosition() const
	{ return (POSITION) m_pNodeTail; }
_AFXCOLL_INLINE CString& CStringList::GetNext(POSITION& rPosition) // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE CString CStringList::GetNext(POSITION& rPosition) const // return *Position++
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pNext;
		return pNode->data; }
_AFXCOLL_INLINE CString& CStringList::GetPrev(POSITION& rPosition) // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE CString CStringList::GetPrev(POSITION& rPosition) const // return *Position--
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pPrev;
		return pNode->data; }
_AFXCOLL_INLINE CString& CStringList::GetAt(POSITION position)
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE CString CStringList::GetAt(POSITION position) const
	{ CNode* pNode = (CNode*) position;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		return pNode->data; }
_AFXCOLL_INLINE void CStringList::SetAt(POSITION pos, LPCTSTR newElement)
	{ CNode* pNode = (CNode*) pos;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode->data = newElement; }

_AFXCOLL_INLINE void CStringList::SetAt(POSITION pos, const CString& newElement)
	{ CNode* pNode = (CNode*) pos;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode->data = newElement; }



////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CMapWordToPtr::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapWordToPtr::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapWordToPtr::SetAt(WORD key, void* newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapWordToPtr::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapWordToPtr::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CMapPtrToWord::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapPtrToWord::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapPtrToWord::SetAt(void* key, WORD newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapPtrToWord::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapPtrToWord::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CMapPtrToPtr::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapPtrToPtr::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapPtrToPtr::SetAt(void* key, void* newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapPtrToPtr::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapPtrToPtr::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////

_AFXCOLL_INLINE int CMapWordToOb::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapWordToOb::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapWordToOb::SetAt(WORD key, CObject* newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapWordToOb::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapWordToOb::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////
_AFXCOLL_INLINE int CMapStringToPtr::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapStringToPtr::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapStringToPtr::SetAt(LPCTSTR key, void* newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapStringToPtr::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapStringToPtr::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////
_AFXCOLL_INLINE int CMapStringToOb::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapStringToOb::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapStringToOb::SetAt(LPCTSTR key, CObject* newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapStringToOb::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapStringToOb::GetHashTableSize() const
	{ return m_nHashTableSize; }


////////////////////////////////////////////////////////////////////////////
_AFXCOLL_INLINE int CMapStringToString::GetCount() const
	{ return m_nCount; }
_AFXCOLL_INLINE BOOL CMapStringToString::IsEmpty() const
	{ return m_nCount == 0; }
_AFXCOLL_INLINE void CMapStringToString::SetAt(LPCTSTR key, LPCTSTR newValue)
	{ (*this)[key] = newValue; }
_AFXCOLL_INLINE POSITION CMapStringToString::GetStartPosition() const
	{ return (m_nCount == 0) ? NULL : BEFORE_START_POSITION; }
_AFXCOLL_INLINE UINT CMapStringToString::GetHashTableSize() const
	{ return m_nHashTableSize; }

/////////////////////////////////////////////////////////////////////////////

#endif //_AFXCOLL_INLINE
