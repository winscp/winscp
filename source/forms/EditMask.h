//---------------------------------------------------------------------------
#ifndef EditMaskH
#define EditMaskH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
#include <WinInterface.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TEditMaskDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TGroupBox *FilesGroup;
  TButton *HelpButton;
  TButton *ClearButton;
  TLabel *Label3;
  TMemo *IncludeFileMasksMemo;
  TLabel *Label1;
  TMemo *ExcludeFileMasksMemo;
  TGroupBox *DirectoriesGroup;
  TLabel *Label2;
  TLabel *Label4;
  TMemo *IncludeDirectoryMasksMemo;
  TMemo *ExcludeDirectoryMasksMemo;
  TGroupBox *MaskGroup;
  TMemo *MaskMemo;
  TStaticText *MaskHintText;
  TCheckBox *ExcludeDirectoryAllCheck;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ClearButtonClick(TObject *Sender);
  void __fastcall FileMasksMemoExit(TObject *Sender);
  void __fastcall DirectoryMasksMemoExit(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall ExcludeDirectoryAllCheckClick(TObject *Sender);
  void __fastcall ExcludeDirectoryMasksMemoChange(TObject *Sender);

public:
  __fastcall TEditMaskDialog(TComponent* Owner);

  bool __fastcall Execute(TFileMasks & Mask);

protected:
  void __fastcall LoadFileMasks(const TFileMasks & Mask);
  void __fastcall SaveFileMasks(TFileMasks & Mask);
  void __fastcall LoadFileMasks(TMemo * Memo, TStrings * MasksStr);
  void __fastcall UpdateControls();

  INTERFACE_HOOK;

private:
  UnicodeString FExcludeDirectoryMasks;
  bool FChanging;
};
//---------------------------------------------------------------------------
#endif
