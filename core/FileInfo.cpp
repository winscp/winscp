//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Windows.hpp>
#include "FileInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Return pointer to file version info block
void * __fastcall CreateFileInfo(AnsiString FileName)
{
  DWORD Handle, Size;
  void * Result = NULL;

  // Get file version info block size
  Size = GetFileVersionInfoSize(FileName.c_str(), &Handle);
  // If size is valid
  if (Size)
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
  if (!VerQueryValue(FileInfo, "\\", (void**)&Result, &Len))
    throw Exception("Fixed file info not available");
  return Result;
};
//---------------------------------------------------------------------------
// Return number of available file version info translations
unsigned __fastcall GetTranslationCount(void * FileInfo)
{
  PTranslations P;
  UINT Len;
  if (!VerQueryValue(FileInfo, "\\VarFileInfo\\Translation", (void**)&P, &Len))
    throw Exception("File info translations not available");
  return Len / 4;
}
//---------------------------------------------------------------------------
// Return i-th translation in the file version info translation list
TTranslation __fastcall GetTranslation(void * FileInfo, unsigned i)
{
  PTranslations P;
  UINT Len;

  if (!VerQueryValue(FileInfo, "\\VarFileInfo\\Translation", (void**)&P, &Len))
    throw Exception("File info translations not available");
  if (i * sizeof(TTranslation) >= Len)
    throw Exception("Specified translation not available");
  return P[i];
};
//---------------------------------------------------------------------------
// Return the name of the specified language
AnsiString __fastcall GetLanguage(Word Language)
{
  UINT Len;
  Char P[256];

  Len = VerLanguageName(Language, P, sizeof(P));
  if (Len > sizeof(P))
    throw Exception("Language not available");
  return AnsiString(P, Len);
};
//---------------------------------------------------------------------------
// Return the value of the specified file version info string using the
// specified translation
AnsiString __fastcall GetFileInfoString(void * FileInfo,
  TTranslation Translation, AnsiString StringName)
{
  PChar P;
  UINT Len;

  if (!VerQueryValue(FileInfo, (AnsiString("\\StringFileInfo\\") +
    IntToHex(Translation.Language, 4) +
    IntToHex(Translation.CharSet, 4) +
    "\\" + StringName).c_str(), (void**)&P, &Len))
  {
    throw Exception("Specified file info string not available");
  }
  // c_str() makes sure that returned string has only necessary bytes allocated
  return AnsiString(P, Len).c_str();
};
