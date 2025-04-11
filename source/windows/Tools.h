//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
#include <WinInterface.h>
#include <HelpIntfs.hpp>
#include <stdio.h>
#include <SessionData.h>
#include <Vcl.Graphics.hpp>
//---------------------------------------------------------------------------
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
void ExecuteProcessAndReadOutput(const UnicodeString & Command, UnicodeString & Output, DWORD & ExitCode, bool ReadStdErr);
void __fastcall ExecuteProcessChecked(
  const UnicodeString & Command, const UnicodeString & HelpKeyword, UnicodeString * Output);
void __fastcall ExecuteProcessCheckedAndWait(
  const UnicodeString & Command, const UnicodeString & HelpKeyword, UnicodeString * Output);
bool __fastcall IsKeyPressed(int VirtualKey);
bool __fastcall UseAlternativeFunction();
bool __fastcall OpenInNewWindow();
void ExecuteSelf(const UnicodeString & Params);
void __fastcall ExecuteNewInstance(const UnicodeString & Param, const UnicodeString & AdditionalParams = UnicodeString());
IShellLink * __fastcall CreateAppDesktopShortCut(
  const UnicodeString & Name, const UnicodeString & Params, const UnicodeString & Description,
  int SpecialFolder = -1, int IconIndex = 0, bool Return = false);
IShellLink * __fastcall CreateDesktopSessionShortCut(
  const UnicodeString & SessionName, UnicodeString Name,
  const UnicodeString & AdditionalParams,
  int SpecialFolder = -1, int IconIndex = SITE_ICON, bool Return = false);
UnicodeString __fastcall GetListViewStr(TListView * ListView);
void __fastcall LoadListViewStr(TListView * ListView, UnicodeString LayoutStr);
void RestoreForm(const UnicodeString & Data, TForm * Form, bool PositionOnly = false, const UnicodeString & DefaultData = EmptyStr);
UnicodeString __fastcall StoreForm(TForm * Form);
void __fastcall RestoreFormSize(UnicodeString Data, TForm * Form);
UnicodeString __fastcall StoreFormSize(TForm * Form);
TFontStyles __fastcall IntToFontStyles(int value);
int __fastcall FontStylesToInt(const TFontStyles value);
bool __fastcall SameFont(TFont * Font1, TFont * Font2);
TColor __fastcall GetWindowTextColor(TColor BackgroundColor, TColor Color = static_cast<TColor>(0));
TColor __fastcall GetWindowColor(TColor Color = static_cast<TColor>(0));
TColor __fastcall GetBtnFaceColor();
TColor __fastcall GetNonZeroColor(TColor Color);
void ValidateMask(const UnicodeString & Mask, int ForceDirectoryMasks = -1);
void __fastcall ValidateMaskEdit(TComboBox * Edit);
void __fastcall ValidateMaskEdit(TEdit * Edit);
void __fastcall ValidateMaskEdit(TMemo * Edit, bool Directory);
bool __fastcall IsWinSCPUrl(const UnicodeString & Url);
UnicodeString __fastcall SecureUrl(const UnicodeString & Url);
void __fastcall OpenBrowser(UnicodeString URL);
void __fastcall OpenFileInExplorer(const UnicodeString & Path);
void __fastcall OpenFolderInExplorer(const UnicodeString & Path);
void __fastcall ShowHelp(const UnicodeString & HelpKeyword);
bool __fastcall IsFormatInClipboard(unsigned int Format);
bool __fastcall NonEmptyTextFromClipboard(UnicodeString & Text);
HANDLE __fastcall OpenTextFromClipboard(const wchar_t *& Text);
void __fastcall CloseTextFromClipboard(HANDLE Handle);
void __fastcall ExitActiveControl(TForm * Form);
UnicodeString __fastcall ReadResource(const UnicodeString ResName);
bool __fastcall DumpResourceToFile(const UnicodeString ResName,
  const UnicodeString FileName);
void __fastcall BrowseForExecutable(TEdit * Control, UnicodeString Title,
  UnicodeString Filter, bool FileNameCommand, bool Escape);
void __fastcall BrowseForExecutable(TComboBox * Control, UnicodeString Title,
  UnicodeString Filter, bool FileNameCommand, bool Escape);
bool __fastcall FontDialog(TFont * Font);
bool __fastcall SaveDialog(UnicodeString Title, UnicodeString Filter,
  UnicodeString DefaultExt, UnicodeString & FileName);
bool __fastcall AutodetectProxy(UnicodeString & HostName, int & PortNumber);
void __fastcall CopyToClipboard(UnicodeString Text);
void __fastcall CopyToClipboard(TStrings * Strings);
void __fastcall ShutDownWindows();
void __fastcall SuspendWindows();
void __fastcall EditSelectBaseName(HWND Edit);
UnicodeString GetConvertedKeyFileName(const UnicodeString & FileName);
struct TPrivateKey;
UnicodeString AddMatchingKeyCertificate(TPrivateKey * PrivateKey, const UnicodeString & FileName);
void __fastcall VerifyAndConvertKey(UnicodeString & FileName, bool CanIgnore);
void __fastcall VerifyKey(const UnicodeString & FileName);
void __fastcall VerifyCertificate(const UnicodeString & FileName);
TStrings * __fastcall GetUnwrappedMemoLines(TMemo * Memo);
bool __fastcall DetectSystemExternalEditor(
  bool AllowDefaultEditor,
  UnicodeString & Executable, UnicodeString & ExecutableDescription,
  UnicodeString & UsageState, bool & TryNextTime);
//---------------------------------------------------------------------------
#define IUNKNOWN \
  virtual HRESULT __stdcall QueryInterface(const GUID& IID, void **Obj) \
  { \
    return TInterfacedObject::QueryInterface(IID, (void *)Obj); \
  } \
  \
  virtual ULONG __stdcall AddRef() \
  { \
    return TInterfacedObject::_AddRef(); \
  } \
  \
  virtual ULONG __stdcall Release() \
  { \
    return TInterfacedObject::_Release(); \
  }
//---------------------------------------------------------------------------
void __fastcall InitializeCustomHelp(ICustomHelpViewer * HelpViewer);
void __fastcall FinalizeCustomHelp();
//---------------------------------------------------------------------------
#endif
