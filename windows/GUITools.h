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
//---------------------------------------------------------------------------
#endif
