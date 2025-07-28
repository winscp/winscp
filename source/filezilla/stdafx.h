//---------------------------------------------------------------------------
#ifndef StdAfxH
#define StdAfxH
//---------------------------------------------------------------------------
#define _int64 __int64
//---------------------------------------------------------------------------
#define MPEXT_NO_ZLIB
#define MPEXT_NO_GSS
#define _AFX_ENABLE_INLINES
#define _AFX_NOFORCE_LIBS
//---------------------------------------------------------------------------
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
//---------------------------------------------------------------------------
#include <afx.h>
#include "wtypes.h"

// STL includes
#include <list>
#include <map>
#include <vector>
#include <deque>
#include <set>
#include <algorithm>
//---------------------------------------------------------------------------
#pragma hdrstop
//---------------------------------------------------------------------------
#include <Global.h>
// these create conflict with afxwin.h
#undef BEGIN_MESSAGE_MAP
#undef END_MESSAGE_MAP
//---------------------------------------------------------------------------
#include "MFC64bitFix.h"
#include <ApiLog.h>
#include <TextsFileZilla.h>
//---------------------------------------------------------------------------
#include <oleauto.h>
#include <afxconv.h>
//---------------------------------------------------------------------------
#define _strlwr strlwr
//---------------------------------------------------------------------------
const int FILEEXISTS_OVERWRITE = 0;
const int FILEEXISTS_RESUME = 1;
const int FILEEXISTS_RENAME = 2;
const int FILEEXISTS_SKIP = 3;
const int FILEEXISTS_COMPLETE = 4;
//---------------------------------------------------------------------------
class t_ffam_statusmessage
{
public:
  CString status;
  int type;
  BOOL post;
};
//---------------------------------------------------------------------------
typedef struct
{
  __int64 bytes;
  __int64 transfersize;
  BOOL bFileTransfer;
} t_ffam_transferstatus;
//---------------------------------------------------------------------------
struct CStringDataA
{
  long nRefs;             // reference count
  int nDataLength;        // length of data (including terminator)
  int nAllocLength;       // length of allocation
  // char data[nAllocLength];

  CHAR * data()           // CHAR* to managed data
  {
    return (CHAR *)(this+1);
  }
};
//---------------------------------------------------------------------------
extern LPCSTR _afxPchNilA;
extern CStringDataA* _afxDataNilA;
#define afxEmptyStringA ((CStringA&)*(CStringA*)&_afxPchNilA)
//---------------------------------------------------------------------------
class CStringA
{
public:
  CStringA()
  {
    m_pchData = afxEmptyStringA.m_pchData;
  }

  CStringA(const CStringA& stringSrc)
  {
    DebugAssert(stringSrc.GetData()->nRefs != 0);
    if (stringSrc.GetData()->nRefs >= 0)
    {
      DebugAssert(stringSrc.GetData() != _afxDataNilA);
      m_pchData = stringSrc.m_pchData;
      InterlockedIncrement(&GetData()->nRefs);
    }
    else
    {
      Init();
      *this = stringSrc.m_pchData;
    }
  }

  CStringA(LPCSTR lpsz)
  {
    Init();
    if (lpsz != NULL && HIWORD(lpsz) == NULL)
    {
      DebugFail();
    }
    else
    {
      int nLen = SafeStrlen(lpsz);
      if (nLen != 0)
      {
        AllocBuffer(nLen);
        memcpy(m_pchData, lpsz, nLen*sizeof(char));
      }
    }
  }

  ~CStringA()
  {
    if (GetData() != _afxDataNilA)
    {
      if (InterlockedDecrement(&GetData()->nRefs) <= 0)
      {
        FreeData(GetData());
      }
    }
  }

  int GetLength() const
  {
    return GetData()->nDataLength;
  }

  char operator[](int nIndex) const
  {
    // same as GetAt
    DebugAssert(nIndex >= 0);
    DebugAssert(nIndex < GetData()->nDataLength);
    return m_pchData[nIndex];
  }

  // ref-counted copy from another CString
  CStringA& operator=(const CStringA& stringSrc)
  {
    if (m_pchData != stringSrc.m_pchData)
    {
      if ((GetData()->nRefs < 0 && GetData() != _afxDataNilA) ||
          stringSrc.GetData()->nRefs < 0)
      {
        // actual copy necessary since one of the strings is locked
        AssignCopy(stringSrc.GetData()->nDataLength, stringSrc.m_pchData);
      }
      else
      {
        // can just copy references around
        Release();
        DebugAssert(stringSrc.GetData() != _afxDataNilA);
        m_pchData = stringSrc.m_pchData;
        InterlockedIncrement(&GetData()->nRefs);
      }
    }
    return *this;
  }

  const CStringA & operator=(LPCSTR lpsz)
  {
    AssignCopy(SafeStrlen(lpsz), lpsz);
    return *this;
  }

  const CStringA & operator+=(char ch)
  {
    ConcatInPlace(1, &ch);
    return *this;
  }

  friend CStringA AFXAPI operator+(const CStringA & string, char ch);

  operator LPCSTR() const
  {
    return m_pchData;
  }

  int Compare(LPCSTR lpsz) const
  {
    return strcmp(m_pchData, lpsz);
  }

  CStringA Mid(int nFirst, int nCount) const
  {
    // out-of-bounds requests return sensible things
    if (nFirst < 0)
    {
      nFirst = 0;
    }
    if (nCount < 0)
    {
      nCount = 0;
    }

    if (nFirst + nCount > GetData()->nDataLength)
    {
      nCount = GetData()->nDataLength - nFirst;
    }
    if (nFirst > GetData()->nDataLength)
    {
      nCount = 0;
    }

    DebugAssert(nFirst >= 0);
    DebugAssert(nFirst + nCount <= GetData()->nDataLength);

    // optimize case of returning entire string
    if (nFirst == 0 && nFirst + nCount == GetData()->nDataLength)
    {
      return *this;
    }

    CStringA dest;
    AllocCopy(dest, nCount, nFirst, 0);
    return dest;
  }

  CStringA Left(int nCount) const
  {
    if (nCount < 0)
    {
      nCount = 0;
    }
    if (nCount >= GetData()->nDataLength)
    {
      return *this;
    }

    CStringA dest;
    AllocCopy(dest, nCount, 0, 0);
    return dest;
  }

  int Find(char ch) const
  {
    return Find(ch, 0);
  }

  int Find(char ch, int nStart) const
  {
    int nLength = GetData()->nDataLength;
    if (nStart >= nLength)
    {
      return -1;
    }

    // find first single character
    LPSTR lpsz = strchr(m_pchData + nStart, (unsigned char)ch);

    // return -1 if not found and index otherwise
    return (lpsz == NULL) ? -1 : (int)(lpsz - m_pchData);
  }

  // find a sub-string (like strstr)
  int Find(LPCSTR lpszSub) const
  {
    return Find(lpszSub, 0);
  }

  int Find(LPCSTR lpszSub, int nStart) const
  {
    int nLength = GetData()->nDataLength;
    if (nStart > nLength)
    {
      return -1;
    }

    // find first matching substring
    LPSTR lpsz = strstr(m_pchData + nStart, lpszSub);

    // return -1 for not found, distance from beginning otherwise
    return (lpsz == NULL) ? -1 : (int)(lpsz - m_pchData);
  }

  void MakeUpper()
  {
    CopyBeforeWrite();
    strupr(m_pchData);
  }

protected:
  LPSTR m_pchData;   // pointer to ref counted string data

  CStringDataA * GetData() const
  {
    DebugAssert(m_pchData != NULL); return ((CStringDataA*)m_pchData)-1;
  }

  void Init()
  {
    m_pchData = afxEmptyStringA.m_pchData;
  }

  void AllocCopy(CStringA & dest, int nCopyLen, int nCopyIndex, int nExtraLen) const
  {
    // will clone the data attached to this string
    // allocating 'nExtraLen' characters
    // Places results in uninitialized string 'dest'
    // Will copy the part or all of original data to start of new string

    int nNewLen = nCopyLen + nExtraLen;
    if (nNewLen == 0)
    {
      dest.Init();
    }
    else
    {
      dest.AllocBuffer(nNewLen);
      memcpy(dest.m_pchData, m_pchData+nCopyIndex, nCopyLen*sizeof(char));
    }
  }

  void AllocBuffer(int nLen)
  // always allocate one extra character for '\0' termination
  // assumes [optimistically] that data length will equal allocation length
  {
    DebugAssert(nLen >= 0);
    DebugAssert(nLen <= INT_MAX-1);    // max size (enough room for 1 extra)

    if (nLen == 0)
    {
      Init();
    }
    else
    {
      CStringDataA* pData;
      {
        pData = (CStringDataA*)
          new BYTE[sizeof(CStringDataA) + (nLen+1)*sizeof(char)];
        pData->nAllocLength = nLen;
      }
      pData->nRefs = 1;
      pData->data()[nLen] = '\0';
      pData->nDataLength = nLen;
      m_pchData = pData->data();
    }
  }

  void AssignCopy(int nSrcLen, LPCSTR lpszSrcData)
  {
    AllocBeforeWrite(nSrcLen);
    memcpy(m_pchData, lpszSrcData, nSrcLen*sizeof(char));
    GetData()->nDataLength = nSrcLen;
    m_pchData[nSrcLen] = '\0';
  }

  void FASTCALL FreeData(CStringDataA * pData)
  {
    delete[] (BYTE*)pData;
  }

  void PASCAL Release(CStringDataA * pData)
  {
    if (pData != _afxDataNilA)
    {
      DebugAssert(pData->nRefs != 0);
      if (InterlockedDecrement(&pData->nRefs) <= 0)
      {
        FreeData(pData);
      }
    }
  }

  void Release()
  {
    if (GetData() != _afxDataNilA)
    {
      DebugAssert(GetData()->nRefs != 0);
      if (InterlockedDecrement(&GetData()->nRefs) <= 0)
      {
        FreeData(GetData());
      }
      Init();
    }
  }

  void ConcatCopy(int nSrc1Len, LPCSTR lpszSrc1Data, int nSrc2Len, LPCSTR lpszSrc2Data)
  {
    // -- master concatenation routine
    // Concatenate two sources
    // -- assume that 'this' is a new CString object

    int nNewLen = nSrc1Len + nSrc2Len;
    if (nNewLen != 0)
    {
      AllocBuffer(nNewLen);
      memcpy(m_pchData, lpszSrc1Data, nSrc1Len*sizeof(char));
      memcpy(m_pchData+nSrc1Len, lpszSrc2Data, nSrc2Len*sizeof(char));
    }
  }

  void ConcatInPlace(int nSrcLen, LPCSTR lpszSrcData)
  {
    //  -- the main routine for += operators

    // concatenating an empty string is a no-op!
    if (nSrcLen == 0)
    {
      return;
    }

    // if the buffer is too small, or we have a width mis-match, just
    //   allocate a new buffer (slow but sure)
    if (GetData()->nRefs > 1 || GetData()->nDataLength + nSrcLen > GetData()->nAllocLength)
    {
      // we have to grow the buffer, use the ConcatCopy routine
      CStringDataA* pOldData = GetData();
      ConcatCopy(GetData()->nDataLength, m_pchData, nSrcLen, lpszSrcData);
      DebugAssert(pOldData != NULL);
      CStringA::Release(pOldData);
    }
    else
    {
      // fast concatenation when buffer big enough
      memcpy(m_pchData+GetData()->nDataLength, lpszSrcData, nSrcLen*sizeof(char));
      GetData()->nDataLength += nSrcLen;
      DebugAssert(GetData()->nDataLength <= GetData()->nAllocLength);
      m_pchData[GetData()->nDataLength] = '\0';
    }
  }

  void CopyBeforeWrite()
  {
    if (GetData()->nRefs > 1)
    {
      CStringDataA* pData = GetData();
      Release();
      AllocBuffer(pData->nDataLength);
      memcpy(m_pchData, pData->data(), (pData->nDataLength+1)*sizeof(char));
    }
    DebugAssert(GetData()->nRefs <= 1);
  }

  void AllocBeforeWrite(int nLen)
  {
    if (GetData()->nRefs > 1 || nLen > GetData()->nAllocLength)
    {
      Release();
      AllocBuffer(nLen);
    }
    DebugAssert(GetData()->nRefs <= 1);
  }

  static int PASCAL SafeStrlen(LPCSTR lpsz)
  {
    return (lpsz == NULL) ? 0 : strlen(lpsz);
  }
};
//---------------------------------------------------------------------------
inline bool AFXAPI operator==(const CStringA & s1, LPCSTR s2)
{
  return s1.Compare(s2) == 0;
}
//---------------------------------------------------------------------------
inline bool AFXAPI operator!=(const CStringA & s1, LPCSTR s2)
{
  return s1.Compare(s2) != 0;
}
//---------------------------------------------------------------------------
inline CStringA AFXAPI operator+(const CStringA & string1, char ch)
{
  CStringA s;
  s.ConcatCopy(string1.GetData()->nDataLength, string1.m_pchData, 1, &ch);
  return s;
}
//---------------------------------------------------------------------------
#include <FileZillaApi.h>
#include <FileZillaOpt.h>
//---------------------------------------------------------------------------
#endif
