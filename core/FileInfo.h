#ifndef FileInfoH
#define FileInfoH

struct TTranslation {
  Word Language, CharSet;
};

// Return pointer to file version info block
void * __fastcall CreateFileInfo(AnsiString FileName);

// Free file version info block memory
void __fastcall FreeFileInfo(void * FileInfo);

// Return pointer to fixed file version info
PVSFixedFileInfo __fastcall GetFixedFileInfo(void * FileInfo);

// Return number of available file version info translations
unsigned __fastcall GetTranslationCount(void * FileInfo);

// Return i-th translation in the file version info translation list
TTranslation __fastcall GetTranslation(void * FileInfo, unsigned i);

// Return the name of the specified language
AnsiString __fastcall GetLanguage(Word Language);

// Return the value of the specified file version info string using the
// specified translation
AnsiString __fastcall GetFileInfoString(void * FileInfo,
  TTranslation Translation, AnsiString StringName);

int __fastcall CalculateCompoundVersion(int MajorVer,
  int MinorVer, int Release, int Build);

#endif // FileInfoH
