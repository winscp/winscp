// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFX_H__
#define __AFX_H__

#ifndef __cplusplus
	#error MFC requires C++ compilation (use a .cpp suffix)
#endif

/////////////////////////////////////////////////////////////////////////////

#include <afxver_.h>        // Target version control

/////////////////////////////////////////////////////////////////////////////
// Other includes from standard "C" runtimes

#include <System.hpp>

/////////////////////////////////////////////////////////////////////////////
// Diagnostic support

#define ASSERT(f)          ((void)0)
#define ASSERT_VALID(pOb)  ((void)0)

/////////////////////////////////////////////////////////////////////////////
// Basic object model

/////////////////////////////////////////////////////////////////////////////
// Strings

class CString
{
public:
// Constructors

	// constructs empty CString
	CString();
	// copy constructor
	CString(const CString& stringSrc);
	// copy constructor
	explicit CString(const UnicodeString& str);
	// from an ANSI string (converts to TCHAR)
	CString(LPCSTR lpsz);
	// from a UNICODE string (converts to TCHAR)
	CString(LPCWSTR lpsz);
	// subset of characters from an ANSI string (converts to TCHAR)
	CString(LPCSTR lpch, int nLength);
	// subset of characters from a UNICODE string (converts to TCHAR)
	CString(LPCWSTR lpch, int nLength);
	// from unsigned characters
	CString(const unsigned char* psz);

// Attributes & Operations

	// get data length
	int GetLength() const;
	// TRUE if zero length
	BOOL IsEmpty() const;
	// clear contents to empty
	void Empty();

	// return single character at zero-based index
	TCHAR GetAt(int nIndex) const;
	// return single character at zero-based index
	TCHAR operator[](int nIndex) const;
	// set a single character at zero-based index
	void SetAt(int nIndex, TCHAR ch);
	// return pointer to const string
	operator LPCTSTR() const;
	LPCTSTR c_str() const { return operator LPCTSTR(); }

	// overloaded assignment

	// ref-counted copy from another CString
	const CString& operator=(const CString& stringSrc);
	// set string content to single character
	const CString& operator=(TCHAR ch);
	const CString& operator=(char ch);
	// copy string content from ANSI string (converts to TCHAR)
	const CString& operator=(LPCSTR lpsz);
	// copy string content from UNICODE string (converts to TCHAR)
	const CString& operator=(LPCWSTR lpsz);
	// copy string content from unsigned chars
	const CString& operator=(const unsigned char* psz);

	// string concatenation

	// concatenate from another CString
	const CString& operator+=(const CString& string);

	// concatenate a single character
	const CString& operator+=(TCHAR ch);
	// concatenate an ANSI character after converting it to TCHAR
	const CString& operator+=(char ch);
	// concatenate a UNICODE character after converting it to TCHAR
	const CString& operator+=(LPCTSTR lpsz);

	friend CString AFXAPI operator+(const CString& string1,
			const CString& string2);
	friend CString AFXAPI operator+(const CString& string, TCHAR ch);
	friend CString AFXAPI operator+(TCHAR ch, const CString& string);
	friend CString AFXAPI operator+(const CString& string, char ch);
	friend CString AFXAPI operator+(char ch, const CString& string);
	friend CString AFXAPI operator+(const CString& string, LPCTSTR lpsz);
	friend CString AFXAPI operator+(LPCTSTR lpsz, const CString& string);

	// string comparison

	// straight character comparison
	int Compare(LPCTSTR lpsz) const;
	// compare ignoring case
	int CompareNoCase(LPCTSTR lpsz) const;

	// simple sub-string extraction

	// return nCount characters starting at zero-based nFirst
	CString Mid(int nFirst, int nCount) const;
	// return all characters starting at zero-based nFirst
	CString Mid(int nFirst) const;
	// return first nCount characters in string
	CString Left(int nCount) const;
	// return nCount characters from end of string
	CString Right(int nCount) const;

	// NLS aware conversion to lowercase
	void MakeLower();

	// trimming anything (either side)

	// remove continuous occurrences of chTarget starting from right
	void TrimRight(TCHAR chTarget);
	// remove continuous occcurrences of characters in passed string,
	// starting from right
	void TrimRight(LPCTSTR lpszTargets);
	// remove continuous occurrences of chTarget starting from left
	void TrimLeft(TCHAR chTarget);
	// remove continuous occcurrences of characters in
	// passed string, starting from left
	void TrimLeft(LPCTSTR lpszTargets);

	// advanced manipulation

	// replace occurrences of chOld with chNew
	int Replace(TCHAR chOld, TCHAR chNew);
	// replace occurrences of substring lpszOld with lpszNew;
	// empty lpszNew removes instances of lpszOld
	BOOL Replace(LPCTSTR lpszOld, LPCTSTR lpszNew);
	// delete nCount characters starting at zero-based index
	int Delete(int nIndex, int nCount = 1);

	// searching

	// find character starting at left, -1 if not found
	int Find(TCHAR ch) const;
	// find character starting at right
	int ReverseFind(TCHAR ch) const;
	// find character starting at zero-based index and going right
	int Find(TCHAR ch, int nStart) const;
	// find first instance of any character in passed string
	int FindOneOf(LPCTSTR lpszCharSet) const;
	// find first instance of substring
	int Find(LPCTSTR lpszSub) const;
	// find first instance of substring starting at zero-based index
	int Find(LPCTSTR lpszSub, int nStart) const;

	// simple formatting

	// printf-like formatting using passed string
	void Format(LPCTSTR lpszFormat, ...);
	// printf-like formatting using referenced string resource
	void Format(UINT nFormatID, ...);
	// printf-like formatting using variable arguments parameter
	void FormatV(LPCTSTR lpszFormat, va_list argList);

// Implementation
protected:
	UnicodeString m_Data;
};

// Compare helpers
bool AFXAPI operator==(const CString& s1, const CString& s2);
bool AFXAPI operator==(const CString& s1, LPCTSTR s2);
bool AFXAPI operator==(LPCTSTR s1, const CString& s2);
bool AFXAPI operator!=(const CString& s1, const CString& s2);
bool AFXAPI operator!=(const CString& s1, LPCTSTR s2);
bool AFXAPI operator!=(LPCTSTR s1, const CString& s2);
bool AFXAPI operator<(const CString& s1, const CString& s2);
bool AFXAPI operator<(const CString& s1, LPCTSTR s2);
bool AFXAPI operator<(LPCTSTR s1, const CString& s2);

/////////////////////////////////////////////////////////////////////////////
// Standard Exception classes

class CFileException
{
public:
	enum {
		none,
		generic,
		fileNotFound,
		badPath,
		tooManyOpenFiles,
		accessDenied,
		invalidFile,
		removeCurrentDir,
		directoryFull,
		badSeek,
		hardIO,
		sharingViolation,
		lockViolation,
		diskFull,
		endOfFile
	};

// Constructor
	CFileException(int cause = CFileException::none, LONG lOsError = -1,
		LPCTSTR lpszArchiveName = NULL);

// Attributes
	int     m_cause;
	LONG    m_lOsError;
	UnicodeString m_strFileName;

// Operations
	// convert a OS dependent error code to a Cause
	static int OsErrorToException(LONG lOsError);

	// helper functions to throw exception after converting to a Cause
	static void ThrowOsError(LONG lOsError, LPCTSTR lpszFileName = NULL);

// Implementation
public:
	UnicodeString GetErrorMessage();
};

/////////////////////////////////////////////////////////////////////////////
// Standard exception throws

void AFXAPI AfxThrowFileException(int cause, LONG lOsError = -1,
	LPCTSTR lpszFileName = NULL);

/////////////////////////////////////////////////////////////////////////////
// File - raw unbuffered disk file I/O

class CFile
{
public:
// Flag values
	enum OpenFlags {
		modeRead =          0x0000,
		modeWrite =         0x0001,
		modeReadWrite =     0x0002,
		shareCompat =       0x0000,
		shareExclusive =    0x0010,
		shareDenyWrite =    0x0020,
		shareDenyRead =     0x0030,
		shareDenyNone =     0x0040,
		modeNoInherit =     0x0080,
		modeCreate =        0x1000,
		modeNoTruncate =    0x2000,
		};

	enum { hFileNull = -1 };

// Constructors
	CFile();

// Attributes
	HANDLE m_hFile;

// Operations
	BOOL Open(LPCTSTR lpszFileName, UINT nOpenFlags,
		CFileException* pError = NULL);

	static BOOL IsValid(LPCTSTR lpszFileName);

// Overridables
	UINT Read(void* lpBuf, UINT nCount);
	void Write(const void* lpBuf, UINT nCount);

	void Close();

// Implementation
public:
	~CFile();

protected:
	BOOL m_bCloseOnDelete;
	CString m_strFileName;
};

/////////////////////////////////////////////////////////////////////////////
// CTimeSpan and CTime

class CTimeSpan
{
public:

// Constructors
	CTimeSpan();
	CTimeSpan(time_t time);

	CTimeSpan(const CTimeSpan& timeSpanSrc);
	const CTimeSpan& operator=(const CTimeSpan& timeSpanSrc);

// Attributes
	// extract parts
	LONG GetTotalSeconds() const;

// Operations
	// time math
	BOOL operator==(CTimeSpan timeSpan) const;
	BOOL operator!=(CTimeSpan timeSpan) const;

private:
	time_t m_timeSpan;
};

class CTime
{
public:

// Constructors
	static CTime CreateForCurrentTime();

	CTime();
	CTime(time_t time);
	CTime(int nYear, int nMonth, int nDay, int nHour, int nMin, int nSec,
		int nDST = -1);
	CTime(const CTime& timeSrc);

	CTime(const SYSTEMTIME& sysTime, int nDST = -1);
	CTime(const FILETIME& fileTime, int nDST = -1);
	const CTime& operator=(const CTime& timeSrc);
	const CTime& operator=(time_t t);

// Attributes
	struct tm* GetLocalTm(struct tm* ptm = NULL) const;

	time_t GetTime() const;
	int GetYear() const;
	int GetMonth() const;       // month of year (1 = Jan)
	int GetDay() const;         // day of month
	int GetHour() const;
	int GetMinute() const;

// Operations
	// time math
	CTimeSpan operator-(CTime time) const;
	BOOL operator==(CTime time) const;
	BOOL operator!=(CTime time) const;

private:
	time_t m_time;
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_ENABLE_INLINES
#define _AFX_INLINE AFX_INLINE

#include <afx.inl>
#endif

#endif // __AFX_H__

/////////////////////////////////////////////////////////////////////////////
