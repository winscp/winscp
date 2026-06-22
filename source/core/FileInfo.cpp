//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Exceptions.h>
#include <Windows.hpp>
#include <Math.hpp>
#include "FileInfo.h"
#include "FileBuffer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Return pointer to file version info block
void * __fastcall CreateFileInfo(UnicodeString FileName)
{
  unsigned long Handle;
  unsigned int Size;
  char * Result = NULL;

  // Get file version info block size
  Size = GetFileVersionInfoSize(FileName.c_str(), &Handle);
  // If size is valid
  if (Size > 0)
  {
    Result = new char[Size];
    // Get file version info block
    if (!GetFileVersionInfo(FileName.c_str(), Handle, Size, Result))
    {
      delete[] Result;
      Result = NULL;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
// Free file version info block memory
void __fastcall FreeFileInfo(void * FileInfo)
{
  delete[] static_cast<char *>(FileInfo);
}
//---------------------------------------------------------------------------
typedef TTranslation TTranslations[65536];
typedef TTranslation *PTranslations;
//---------------------------------------------------------------------------
// Return pointer to fixed file version info
PVSFixedFileInfo __fastcall GetFixedFileInfo(void * FileInfo)
{
  UINT Len;
  PVSFixedFileInfo Result;
  if (!VerQueryValue(FileInfo, L"\\", (void**)&Result, &Len))
  {
    throw Exception(L"Fixed file info not available");
  }
  return Result;
}
//---------------------------------------------------------------------------
// Return number of available file version info translations
unsigned __fastcall GetTranslationCount(void * FileInfo)
{
  PTranslations P;
  UINT Len;
  if (!VerQueryValue(FileInfo, L"\\VarFileInfo\\Translation", (void**)&P, &Len))
  {
    throw Exception(L"File info translations not available");
  }
  return Len / 4;
}
//---------------------------------------------------------------------------
// Return i-th translation in the file version info translation list
TTranslation __fastcall GetTranslation(void * FileInfo, unsigned i)
{
  PTranslations P;
  UINT Len;

  if (!VerQueryValue(FileInfo, L"\\VarFileInfo\\Translation", (void**)&P, &Len))
  {
    throw Exception(L"File info translations not available");
  }
  if (i * sizeof(TTranslation) >= Len)
  {
    throw Exception(L"Specified translation not available");
  }
  return P[i];
}
//---------------------------------------------------------------------------
// Return the value of the specified file version info string using the
// specified translation
UnicodeString __fastcall GetFileInfoString(void * FileInfo,
  TTranslation Translation, UnicodeString StringName, bool AllowEmpty)
{
  UnicodeString Result;
  wchar_t * P;
  UINT Len;

  UnicodeString SubBlock =
    UnicodeString(L"\\StringFileInfo\\") + IntToHex(Translation.Language, 4) + IntToHex(Translation.CharSet, 4) + L"\\" + StringName;
  if (!VerQueryValue(FileInfo, SubBlock.c_str(), (void**)&P, &Len))
  {
    if (!AllowEmpty)
    {
      throw Exception("Specified file info string not available");
    }
  }
  else
  {
    Result = UnicodeString(P, Len);
    PackStr(Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall CalculateCompoundVersion(int MajorVer, int MinorVer, int Release)
{
  int CompoundVer = 10000 * (Release + 100 * (MinorVer + 100 * MajorVer));
  return CompoundVer;
}
//---------------------------------------------------------------------------
int ZeroBuildNumber(int CompoundVersion)
{
  return (CompoundVersion / 10000 * 10000);
}
//---------------------------------------------------------------------------
int __fastcall StrToCompoundVersion(UnicodeString S)
{
  int MajorVer = Min(StrToInt(CutToChar(S, L'.', false)), 99);
  int MinorVer = Min(StrToInt(CutToChar(S, L'.', false)), 99);
  int Release = S.IsEmpty() ? 0 : Min(StrToInt(CutToChar(S, L'.', false)), 99);
  return CalculateCompoundVersion(MajorVer, MinorVer, Release);
}
//---------------------------------------------------------------------------
int __fastcall CompareVersion(UnicodeString V1, UnicodeString V2)
{
  int Result = 0;
  while ((Result == 0) && (!V1.IsEmpty() || !V2.IsEmpty()))
  {
    int C1 = StrToIntDef(CutToChar(V1, L'.', false), 0);
    int C2 = StrToIntDef(CutToChar(V2, L'.', false), 0);
    Result = CompareValue(C1, C2);
  }
  return Result;
}
