
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
//
// Implementation of parameterized List
//
/////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#ifdef AFX_COLL_SEG
#pragma code_seg(AFX_COLL_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#include "elements.h"  // used for special creation


#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////

CStringList::CStringList(int nBlockSize)
{
	ASSERT(nBlockSize > 0);

	m_nCount = 0;
	m_pNodeHead = m_pNodeTail = m_pNodeFree = NULL;
	m_pBlocks = NULL;
	m_nBlockSize = nBlockSize;
}

void CStringList::RemoveAll()
{
	ASSERT_VALID(this);

	// destroy elements

	CNode* pNode;
	for (pNode = m_pNodeHead; pNode != NULL; pNode = pNode->pNext)
		DestructElement(&pNode->data);


	m_nCount = 0;
	m_pNodeHead = m_pNodeTail = m_pNodeFree = NULL;
	m_pBlocks->FreeDataChain();
	m_pBlocks = NULL;
}

CStringList::~CStringList()
{
	RemoveAll();
	ASSERT(m_nCount == 0);
}

/////////////////////////////////////////////////////////////////////////////
// Node helpers
/*
 * Implementation note: CNode's are stored in CPlex blocks and
 *  chained together. Free blocks are maintained in a singly linked list
 *  using the 'pNext' member of CNode with 'm_pNodeFree' as the head.
 *  Used blocks are maintained in a doubly linked list using both 'pNext'
 *  and 'pPrev' as links and 'm_pNodeHead' and 'm_pNodeTail'
 *   as the head/tail.
 *
 * We never free a CPlex block unless the List is destroyed or RemoveAll()
 *  is used - so the total number of CPlex blocks may grow large depending
 *  on the maximum past size of the list.
 */

CStringList::CNode*
CStringList::NewNode(CStringList::CNode* pPrev, CStringList::CNode* pNext)
{
	if (m_pNodeFree == NULL)
	{
		// add another block
		CPlex* pNewBlock = CPlex::Create(m_pBlocks, m_nBlockSize,
				 sizeof(CNode));

		// chain them into free list
		CNode* pNode = (CNode*) pNewBlock->data();
		// free in reverse order to make it easier to debug
		pNode += m_nBlockSize - 1;
		for (int i = m_nBlockSize-1; i >= 0; i--, pNode--)
		{
			pNode->pNext = m_pNodeFree;
			m_pNodeFree = pNode;
		}
	}
	ASSERT(m_pNodeFree != NULL);  // we must have something

	CStringList::CNode* pNode = m_pNodeFree;
	m_pNodeFree = m_pNodeFree->pNext;
	pNode->pPrev = pPrev;
	pNode->pNext = pNext;
	m_nCount++;
	ASSERT(m_nCount > 0);  // make sure we don't overflow


	ConstructElement(&pNode->data);



	return pNode;
}

void CStringList::FreeNode(CStringList::CNode* pNode)
{

	DestructElement(&pNode->data);

	pNode->pNext = m_pNodeFree;
	m_pNodeFree = pNode;
	m_nCount--;
	ASSERT(m_nCount >= 0);  // make sure we don't underflow

	// if no more elements, cleanup completely
	if (m_nCount == 0)
		RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////

POSITION CStringList::AddHead(LPCTSTR newElement)
{

	return AddHead(CString(newElement));

}


POSITION CStringList::AddHead(const CString& newElement)
{
	ASSERT_VALID(this);

	CNode* pNewNode = NewNode(NULL, m_pNodeHead);
	pNewNode->data = newElement;
	if (m_pNodeHead != NULL)
		m_pNodeHead->pPrev = pNewNode;
	else
		m_pNodeTail = pNewNode;
	m_pNodeHead = pNewNode;
	return (POSITION) pNewNode;
}


POSITION CStringList::AddTail(LPCTSTR newElement)
{

	return AddTail(CString(newElement));

}


POSITION CStringList::AddTail(const CString& newElement)
{
	ASSERT_VALID(this);

	CNode* pNewNode = NewNode(m_pNodeTail, NULL);
	pNewNode->data = newElement;
	if (m_pNodeTail != NULL)
		m_pNodeTail->pNext = pNewNode;
	else
		m_pNodeHead = pNewNode;
	m_pNodeTail = pNewNode;
	return (POSITION) pNewNode;
}


void CStringList::AddHead(CStringList* pNewList)
{
	ASSERT_VALID(this);

	ASSERT(pNewList != NULL);
	ASSERT_KINDOF(CStringList, pNewList);
	ASSERT_VALID(pNewList);

	// add a list of same elements to head (maintain order)
	POSITION pos = pNewList->GetTailPosition();
	while (pos != NULL)
		AddHead(pNewList->GetPrev(pos));
}

void CStringList::AddTail(CStringList* pNewList)
{
	ASSERT_VALID(this);
	ASSERT(pNewList != NULL);
	ASSERT_KINDOF(CStringList, pNewList);
	ASSERT_VALID(pNewList);

	// add a list of same elements
	POSITION pos = pNewList->GetHeadPosition();
	while (pos != NULL)
		AddTail(pNewList->GetNext(pos));
}

CString CStringList::RemoveHead()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeHead != NULL);  // don't call on empty list !!!
	ASSERT(AfxIsValidAddress(m_pNodeHead, sizeof(CNode)));

	CNode* pOldNode = m_pNodeHead;
	CString returnValue = pOldNode->data;

	m_pNodeHead = pOldNode->pNext;
	if (m_pNodeHead != NULL)
		m_pNodeHead->pPrev = NULL;
	else
		m_pNodeTail = NULL;
	FreeNode(pOldNode);
	return returnValue;
}

CString CStringList::RemoveTail()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeTail != NULL);  // don't call on empty list !!!
	ASSERT(AfxIsValidAddress(m_pNodeTail, sizeof(CNode)));

	CNode* pOldNode = m_pNodeTail;
	CString returnValue = pOldNode->data;

	m_pNodeTail = pOldNode->pPrev;
	if (m_pNodeTail != NULL)
		m_pNodeTail->pNext = NULL;
	else
		m_pNodeHead = NULL;
	FreeNode(pOldNode);
	return returnValue;
}

POSITION CStringList::InsertBefore(POSITION position, LPCTSTR newElement)
{

	return InsertBefore(position, CString(newElement));

}


POSITION CStringList::InsertBefore(POSITION position, const CString& newElement)
{
	ASSERT_VALID(this);

	if (position == NULL)
		return AddHead(newElement); // insert before nothing -> head of the list

	// Insert it before position
	CNode* pOldNode = (CNode*) position;
	CNode* pNewNode = NewNode(pOldNode->pPrev, pOldNode);
	pNewNode->data = newElement;

	if (pOldNode->pPrev != NULL)
	{
		ASSERT(AfxIsValidAddress(pOldNode->pPrev, sizeof(CNode)));
		pOldNode->pPrev->pNext = pNewNode;
	}
	else
	{
		ASSERT(pOldNode == m_pNodeHead);
		m_pNodeHead = pNewNode;
	}
	pOldNode->pPrev = pNewNode;
	return (POSITION) pNewNode;
}


POSITION CStringList::InsertAfter(POSITION position, LPCTSTR newElement)
{

	return InsertAfter(position, CString(newElement));

}


POSITION CStringList::InsertAfter(POSITION position, const CString& newElement)
{
	ASSERT_VALID(this);

	if (position == NULL)
		return AddTail(newElement); // insert after nothing -> tail of the list

	// Insert it before position
	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));
	CNode* pNewNode = NewNode(pOldNode, pOldNode->pNext);
	pNewNode->data = newElement;

	if (pOldNode->pNext != NULL)
	{
		ASSERT(AfxIsValidAddress(pOldNode->pNext, sizeof(CNode)));
		pOldNode->pNext->pPrev = pNewNode;
	}
	else
	{
		ASSERT(pOldNode == m_pNodeTail);
		m_pNodeTail = pNewNode;
	}
	pOldNode->pNext = pNewNode;
	return (POSITION) pNewNode;
}


void CStringList::RemoveAt(POSITION position)
{
	ASSERT_VALID(this);

	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));

	// remove pOldNode from list
	if (pOldNode == m_pNodeHead)
	{
		m_pNodeHead = pOldNode->pNext;
	}
	else
	{
		ASSERT(AfxIsValidAddress(pOldNode->pPrev, sizeof(CNode)));
		pOldNode->pPrev->pNext = pOldNode->pNext;
	}
	if (pOldNode == m_pNodeTail)
	{
		m_pNodeTail = pOldNode->pPrev;
	}
	else
	{
		ASSERT(AfxIsValidAddress(pOldNode->pNext, sizeof(CNode)));
		pOldNode->pNext->pPrev = pOldNode->pPrev;
	}
	FreeNode(pOldNode);
}


/////////////////////////////////////////////////////////////////////////////
// slow operations

POSITION CStringList::FindIndex(int nIndex) const
{
	ASSERT_VALID(this);

	if (nIndex >= m_nCount || nIndex < 0)
		return NULL;  // went too far

	CNode* pNode = m_pNodeHead;
	while (nIndex--)
	{
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode = pNode->pNext;
	}
	return (POSITION) pNode;
}

POSITION CStringList::Find(LPCTSTR searchValue, POSITION startAfter) const
{
	ASSERT_VALID(this);

	CNode* pNode = (CNode*) startAfter;
	if (pNode == NULL)
	{
		pNode = m_pNodeHead;  // start at head
	}
	else
	{
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		pNode = pNode->pNext;  // start after the one specified
	}

	for (; pNode != NULL; pNode = pNode->pNext)
		if (pNode->data == searchValue)
			return (POSITION) pNode;
	return NULL;
}


/////////////////////////////////////////////////////////////////////////////
// Serialization

void CStringList::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CObject::Serialize(ar);

	if (ar.IsStoring())
	{
		ar.WriteCount(m_nCount);
		for (CNode* pNode = m_pNodeHead; pNode != NULL; pNode = pNode->pNext)
		{
			ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
			ar << pNode->data;
		}
	}
	else
	{
		DWORD nNewCount = ar.ReadCount();
		CString newData;
		while (nNewCount--)
		{
			ar >> newData;
			AddTail(newData);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void CStringList::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with " << m_nCount << " elements";
	if (dc.GetDepth() > 0)
	{
		POSITION pos = GetHeadPosition();
		while (pos != NULL)
			dc << "\n\t" << GetNext(pos);
	}

	dc << "\n";
}

void CStringList::AssertValid() const
{
	CObject::AssertValid();

	if (m_nCount == 0)
	{
		// empty list
		ASSERT(m_pNodeHead == NULL);
		ASSERT(m_pNodeTail == NULL);
	}
	else
	{
		// non-empty list
		ASSERT(AfxIsValidAddress(m_pNodeHead, sizeof(CNode)));
		ASSERT(AfxIsValidAddress(m_pNodeTail, sizeof(CNode)));
	}
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif


IMPLEMENT_SERIAL(CStringList, CObject, 0)

/////////////////////////////////////////////////////////////////////////////
