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
#define DWORD_ALIGN( base, ptr ) \
    ( (LPBYTE)(base) + ((((LPBYTE)(ptr) - (LPBYTE)(base)) + 3) & ~3) )
struct VS_VERSION_INFO_STRUCT32
{
  WORD  wLength;
  WORD  wValueLength;
  WORD  wType;
  WCHAR szKey[1];
};
//---------------------------------------------------------------------------
unsigned int VERSION_GetFileVersionInfo_PE(const wchar_t * FileName, unsigned int DataSize, void * Data)
{
  unsigned int Len;

  bool NeedFree = false;
  HMODULE Module = GetModuleHandle(FileName);
  if (Module == NULL)
  {
    Module = LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
    NeedFree = true;
  }
  if (Module == NULL)
  {
    Len = 0;
  }
  else
  {
    try
    {
      HRSRC Rsrc = FindResource(Module, MAKEINTRESOURCE(VS_VERSION_INFO),
        MAKEINTRESOURCE(VS_FILE_INFO));
      if (Rsrc != NULL)
      {
        Len = SizeofResource(Module, Rsrc);
        HANDLE Mem = LoadResource(Module, Rsrc);
        if (Mem != NULL)
        {
          try
          {
            VS_VERSION_INFO_STRUCT32 * VersionInfo = (VS_VERSION_INFO_STRUCT32 *)LockResource(Mem);
            const VS_FIXEDFILEINFO * FixedInfo =
              (VS_FIXEDFILEINFO *)DWORD_ALIGN(VersionInfo, VersionInfo->szKey + wcslen(VersionInfo->szKey) + 1);

            if (FixedInfo->dwSignature != VS_FFI_SIGNATURE)
            {
              Len = 0;
            }
            else
            {
              if (Data != NULL)
              {
                if (DataSize < Len)
                {
                  Len = DataSize;
                }
                if (Len > 0)
                {
                  memmove(Data, VersionInfo, Len);
                }
              }
            }
          }
          __finally
          {
            FreeResource(Mem);
          }
        }
      }
    }
    __finally
    {
      if (NeedFree)
      {
        FreeLibrary(Module);
      }
    }
  }

  return Len;
}
//---------------------------------------------------------------------------
unsigned int GetFileVersionInfoSizeFix(const wchar_t * FileName, unsigned long * Handle)
{
  unsigned int Len;
  if (IsWin7())
  {
    *Handle = 0;
    Len = VERSION_GetFileVersionInfo_PE(FileName, 0, NULL);

    if (Len != 0)
    {
      Len = (Len * 2) + 4;
    }
  }
  else
  {
    Len = GetFileVersionInfoSize(FileName, Handle);
  }

  return Len;
}
//---------------------------------------------------------------------------
bool GetFileVersionInfoFix(const wchar_t * FileName, unsigned long Handle,
  unsigned int DataSize, void * Data)
{
  bool Result;

  if (IsWin7())
  {
    VS_VERSION_INFO_STRUCT32 * VersionInfo = (VS_VERSION_INFO_STRUCT32*)Data;

    unsigned int Len = VERSION_GetFileVersionInfo_PE(FileName, DataSize, Data);

    Result = (Len != 0);
    if (Result)
    {
      static const char Signature[] = "FE2X";
      unsigned int BufSize = VersionInfo->wLength + strlen(Signature);
      unsigned int ConvBuf;

      if (DataSize >= BufSize)
      {
        ConvBuf = DataSize - VersionInfo->wLength;
        memmove(((char*)(Data)) + VersionInfo->wLength, Signature, ConvBuf > 4 ? 4 : ConvBuf );
      }
    }
  }
  else
  {
    Result = GetFileVersionInfo(FileName, Handle, DataSize, Data);
  }

  return Result;
}
//---------------------------------------------------------------------------
// Return pointer to file version info block
void * __fastcall CreateFileInfo(UnicodeString FileName)
{
  unsigned long Handle;
  unsigned int Size;
  void * Result = NULL;

  // Get file version info block size
  Size = GetFileVersionInfoSizeFix(FileName.c_str(), &Handle);
  // If size is valid
  if (Size > 0)
  {
    Result = new char[Size];
    // Get file version info block
    if (!GetFileVersionInfoFix(FileName.c_str(), Handle, Size, Result))
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
  delete[] FileInfo;
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
// Return the name of the specified language
UnicodeString __fastcall GetLanguage(Word Language)
{
  UINT Len;
  wchar_t P[256];

  Len = VerLanguageName(Language, P, LENOF(P));
  if (Len > LENOF(P))
  {
    throw Exception(L"Language not available");
  }
  return UnicodeString(P, Len);
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
