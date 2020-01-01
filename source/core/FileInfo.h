#ifndef FileInfoH
#define FileInfoH

struct TTranslation {
  Word Language, CharSet;
};

// Return pointer to file version info block
void * __fastcall CreateFileInfo(UnicodeString FileName);

// Free file version info block memory
void __fastcall FreeFileInfo(void * FileInfo);

// Return pointer to fixed file version info
PVSFixedFileInfo __fastcall GetFixedFileInfo(void * FileInfo);

// Return number of available file version info translations
unsigned __fastcall GetTranslationCount(void * FileInfo);

// Return i-th translation in the file version info translation list
TTranslation __fastcall GetTranslation(void * FileInfo, unsigned i);

// Return the name of the specified language
UnicodeString __fastcall GetLanguage(Word Language);

// Return the value of the specified file version info string using the
// specified translation
UnicodeString __fastcall GetFileInfoString(void * FileInfo,
  TTranslation Translation, UnicodeString StringName, bool AllowEmpty = false);

int __fastcall CalculateCompoundVersion(int MajorVer, int MinorVer, int Release);
int ZeroBuildNumber(int CompoundVersion);

int __fastcall StrToCompoundVersion(UnicodeString S);

int __fastcall CompareVersion(UnicodeString V1, UnicodeString V2);

#endif // FileInfoH
