//---------------------------------------------------------------------------
#ifndef GenerateUrlH
#define GenerateUrlH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.StdActns.hpp>
//---------------------------------------------------------------------------
#include "SessionData.h"
#include <WinInterface.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TGenerateUrlDialog : public TForm
{
__published:
  TGroupBox *ResultGroup;
  TMemo *ResultMemo;
  TButton *CancelBtn;
  TButton *HelpButton;
  TButton *ClipboardButton;
  TPageControl *OptionsPageControl;
  TTabSheet *UrlSheet;
  TTabSheet *ScriptSheet;
  TTabSheet *AssemblySheet;
  TCheckBox *UserNameCheck;
  TCheckBox *HostKeyCheck;
  TCheckBox *WinSCPSpecificCheck;
  TCheckBox *SaveExtensionCheck;
  TCheckBox *RemoteDirectoryCheck;
  TCheckBox *PasswordCheck;
  TLabel *Label2;
  TComboBox *ScriptFormatCombo;
  TLabel *Label1;
  TComboBox *AssemblyLanguageCombo;
  TLabel *ScriptDescriptionLabel;
  TLabel *AssemblyDescriptionLabel;
  TCheckBox *RawSettingsCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ClipboardButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall WMNCCreate(TWMNCCreate & Message);
  void __fastcall FormShow(TObject *Sender);

private:
  TSessionData * FData;
  std::unique_ptr<TStrings> FPaths;
  bool FPathsSample;
  bool FChanging;
  TRichEdit * FResultMemoWithLinks;
  bool FTransfer;
  bool FToRemote;
  bool FMove;
  int FCopyParamAttrs;
  UnicodeString FPath;
  TFilesSelected FFilesSelected;
  UnicodeString FSourcePath;
  TCopyParamType FCopyParam;
  bool FUrlCounted;
  bool FScriptCounted;
  bool FAssemblyCounted;

protected:
  void __fastcall UpdateControls();
  UnicodeString __fastcall GenerateUrl(UnicodeString Path);
  bool __fastcall IsFileUrl();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall Dispatch(void * AMessage);
  UnicodeString __fastcall GenerateUrl();
  UnicodeString __fastcall GenerateScript(UnicodeString & ScriptDescription);
  UnicodeString __fastcall GenerateAssemblyCode(UnicodeString & AssemblyDescription);
  void __fastcall AddSampleDescription(UnicodeString & Description);

  INTERFACE_HOOK

public:
  __fastcall TGenerateUrlDialog(
    TComponent * Owner, TSessionData * Data, TFilesSelected FilesSelected, TStrings * Paths,
    bool Transfer, bool ToRemote, bool Move, int CopyParamAttrs, const UnicodeString & Path, const TCopyParamType & CopyParam);
  void __fastcall Execute();
};
//---------------------------------------------------------------------------
#endif
