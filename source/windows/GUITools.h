//---------------------------------------------------------------------------
#ifndef GUIToolsH
#define GUIToolsH
//---------------------------------------------------------------------------
// from shlobj.h
#define CSIDL_DESKTOP                   0x0000        // <desktop>
#define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
#define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
#define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
#define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
#define CSIDL_PROGRAM_FILES             0x0026        // C:\Program Files
#define CSIDL_PERSONAL                  0x0005        // My Documents
//---------------------------------------------------------------------------
#include <FileMasks.H>
//---------------------------------------------------------------------------
class TSessionData;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessMessagesEvent)();
//---------------------------------------------------------------------------
bool __fastcall FindFile(UnicodeString & Path);
bool __fastcall FindTool(const UnicodeString & Name, UnicodeString & Path);
bool __fastcall FileExistsEx(UnicodeString Path);
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params);
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params,
  HANDLE & Handle);
bool __fastcall ExecuteShellAndWait(HWND Handle, const UnicodeString Path,
  const UnicodeString Params, TProcessMessagesEvent ProcessMessages);
bool __fastcall ExecuteShellAndWait(HWND Handle, const UnicodeString Command,
  TProcessMessagesEvent ProcessMessages);
void __fastcall OpenSessionInPutty(const UnicodeString PuttyPath,
  TSessionData * SessionData, UnicodeString Password);
bool __fastcall SpecialFolderLocation(int PathID, UnicodeString & Path);
UnicodeString __fastcall ItemsFormatString(const UnicodeString SingleItemFormat,
  const UnicodeString MultiItemsFormat, int Count, const UnicodeString FirstItem);
UnicodeString __fastcall ItemsFormatString(const UnicodeString SingleItemFormat,
  const UnicodeString MultiItemsFormat, TStrings * Items);
UnicodeString __fastcall FileNameFormatString(const UnicodeString SingleFileFormat,
  const UnicodeString MultiFileFormat, TStrings * Files, bool Remote);
UnicodeString __fastcall UniqTempDir(const UnicodeString BaseDir,
  const UnicodeString Identity, bool Mask = false);
bool __fastcall DeleteDirectory(const UnicodeString DirName);
UnicodeString __fastcall FormatDateTimeSpan(const UnicodeString TimeFormat, TDateTime DateTime);
void __fastcall AddSessionColorImage(TCustomImageList * ImageList, TColor Color, bool Disconnected);
//---------------------------------------------------------------------------
class TLocalCustomCommand : public TFileCustomCommand
{
public:
  TLocalCustomCommand();
  TLocalCustomCommand(const TCustomCommandData & Data, const UnicodeString & Path);
  TLocalCustomCommand(const TCustomCommandData & Data, const UnicodeString & Path,
    const UnicodeString & FileName, const UnicodeString & LocalFileName,
    const UnicodeString & FileList);

  virtual bool __fastcall IsFileCommand(const UnicodeString & Command);
  bool __fastcall HasLocalFileName(const UnicodeString & Command);

protected:
  virtual int __fastcall PatternLen(const UnicodeString & Command, int Index);
  virtual bool __fastcall PatternReplacement(const UnicodeString & Pattern,
    UnicodeString & Replacement, bool & Delimit);
  virtual void __fastcall DelimitReplacement(UnicodeString & Replacement, wchar_t Quote);

private:
  UnicodeString FLocalFileName;
};
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool;
extern const UnicodeString PuttygenTool;
//---------------------------------------------------------------------------
#endif
