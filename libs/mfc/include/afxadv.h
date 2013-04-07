// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Note: This header file contains useful classes that are documented only
//  in the MFC Technical Notes.  These classes may change from version to
//  version, so be prepared to change your code accordingly if you utilize
//  this header.  In the future, commonly used portions of this header
//  may be moved and officially documented.

#ifndef __AFXADV_H__
#define __AFXADV_H__

#ifndef __AFXWIN_H__
	#include <afxwin.h>
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXADV - MFC Advanced Classes

// Classes declared in this file

//CObject
	//CFile
		//CMemFile
			class CSharedFile;          // Shared memory file

	class CRecentFileList;              // used in CWinApp for MRU list
	class CDockState;                   // state of docking toolbars

/////////////////////////////////////////////////////////////////////////////

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////
// Shared file support

class CSharedFile : public CMemFile
{
	DECLARE_DYNAMIC(CSharedFile)

public:
// Constructors
	CSharedFile(UINT nAllocFlags = GMEM_DDESHARE|GMEM_MOVEABLE,
		UINT nGrowBytes = 4096);

// Attributes
	HGLOBAL Detach();
	void SetHandle(HGLOBAL hGlobalMemory, BOOL bAllowGrow = TRUE);

// Implementation
public:
	virtual ~CSharedFile();
protected:
	virtual BYTE* Alloc(DWORD nBytes);
	virtual BYTE* Realloc(BYTE* lpMem, DWORD nBytes);
	virtual void Free(BYTE* lpMem);

	UINT m_nAllocFlags;
	HGLOBAL m_hGlobalMemory;
	BOOL m_bAllowGrow;
};

/////////////////////////////////////////////////////////////////////////////
// CRecentFileList

#define AFX_ABBREV_FILENAME_LEN 30

class CRecentFileList
{
// Constructors
public:
	CRecentFileList(UINT nStart, LPCTSTR lpszSection,
		LPCTSTR lpszEntryFormat, int nSize,
		int nMaxDispLen = AFX_ABBREV_FILENAME_LEN);

// Attributes
	int GetSize() const;
	CString& operator[](int nIndex);

// Operations
	virtual void Remove(int nIndex);
	virtual void Add(LPCTSTR lpszPathName);
	BOOL GetDisplayName(CString& strName, int nIndex,
		LPCTSTR lpszCurDir, int nCurDir, BOOL bAtLeastName = TRUE) const;
	virtual void UpdateMenu(CCmdUI* pCmdUI);
	virtual void ReadList();    // reads from registry or ini file
	virtual void WriteList();   // writes to registry or ini file

// Implementation
	virtual ~CRecentFileList();

	int m_nSize;                // contents of the MRU list
	CString* m_arrNames;
	CString m_strSectionName;   // for saving
	CString m_strEntryFormat;
	UINT m_nStart;              // for displaying
	int m_nMaxDisplayLength;
	CString m_strOriginal;      // original menu item contents
};

AFX_INLINE int CRecentFileList::GetSize() const
	{ return m_nSize; }
AFX_INLINE CString& CRecentFileList::operator[](int nIndex)
	{ ASSERT(nIndex < m_nSize); return m_arrNames[nIndex]; }

/////////////////////////////////////////////////////////////////////////////
// CDockState - used for docking serialization

class CDockState : public CObject
{
	DECLARE_SERIAL(CDockState)
	CDockState();

public:
// Attributes
	CPtrArray m_arrBarInfo;

public:
// Operations
	void LoadState(LPCTSTR lpszProfileName);
	void SaveState(LPCTSTR lpszProfileName);
	void Clear(); //deletes all the barinfo's
	DWORD GetVersion();

// Implementation
protected:
	BOOL m_bScaling;
	CRect m_rectDevice;
	CRect m_rectClip;
	CSize m_sizeLogical;
	DWORD m_dwVersion;

public:
	~CDockState();
	virtual void Serialize(CArchive& ar);

	// scaling implementation
	void ScalePoint(CPoint& pt);
	void ScaleRectPos(CRect& rect);
	CSize GetScreenSize();
	void SetScreenSize(CSize& size);
};

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif // __AFXADV_H__

/////////////////////////////////////////////////////////////////////////////
