//---------------------------------------------------------------------------
#ifndef GUIToolsH
#define GUIToolsH
//---------------------------------------------------------------------------
// from shlobj.h
#define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
#define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
#define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
#define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
#define CSIDL_PROGRAM_FILES             0x0026        // C:\Program Files
#define CSIDL_PERSONAL                  0x0005        // My Documents
//---------------------------------------------------------------------------
class TSessionData;
//---------------------------------------------------------------------------
bool __fastcall FindFile(AnsiString & Path);
bool __fastcall FileExistsEx(AnsiString Path);
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params);
void __fastcall OpenSessionInPutty(TSessionData * SessionData);
bool __fastcall SpecialFolderLocation(int PathID, AnsiString & Path);
AnsiString __fastcall ItemsFormatString(const AnsiString SingleItemFormat,
  const AnsiString MultiItemsFormat, int Count, const AnsiString FirstItem);
AnsiString __fastcall ItemsFormatString(const AnsiString SingleItemFormat,
  const AnsiString MultiItemsFormat, TStrings * Items);
AnsiString __fastcall FileNameFormatString(const AnsiString SingleFileFormat,
  const AnsiString MultiFileFormat, TStrings * Files, bool Remote);
void __fastcall CopyToClipboard(AnsiString Text);
AnsiString __fastcall UniqTempDir(const AnsiString BaseDir, const AnsiString Identity);
bool __fastcall DeleteDirectory(const AnsiString DirName);
//---------------------------------------------------------------------------
#endif
