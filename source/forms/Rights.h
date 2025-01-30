//---------------------------------------------------------------------------
#ifndef RightsH
#define RightsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ActnList.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "GrayedCheckBox.hpp"
#include "PngImageList.hpp"
#include <System.Actions.hpp>
//---------------------------------------------------------------------------
#include <RemoteFiles.h>
#include <GUITools.h>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TRightsFrame : public TFrame
{
__published:
  TGrayedCheckBox *OwnerReadCheck;
  TGrayedCheckBox *OwnerWriteCheck;
  TGrayedCheckBox *OwnerExecuteCheck;
  TGrayedCheckBox *GroupReadCheck;
  TGrayedCheckBox *GroupWriteCheck;
  TGrayedCheckBox *GroupExecuteCheck;
  TGrayedCheckBox *OthersReadCheck;
  TGrayedCheckBox *OthersWriteCheck;
  TGrayedCheckBox *OthersExecuteCheck;
  TCheckBox *DirectoriesXCheck;
  TSpeedButton *OwnerButton;
  TSpeedButton *GroupButton;
  TSpeedButton *OthersButton;
  TPopupMenu *RightsPopup;
  TMenuItem *Norights1;
  TMenuItem *Defaultrights1;
  TMenuItem *Allrights1;
  TMenuItem *Leaveasis1;
  TActionList *RightsActions;
  TAction *NoRightsAction;
  TAction *DefaultRightsAction;
  TAction *AllRightsAction;
  TAction *LeaveRightsAsIsAction;
  TPngImageList *RightsImages;
  TMenuItem *N1;
  TAction *CopyTextAction;
  TAction *CopyOctalAction;
  TAction *PasteAction;
  TMenuItem *CopyAsText1;
  TMenuItem *CopyAsOctal1;
  TMenuItem *Paste1;
  TPngImageList *RightsImages120;
  TPngImageList *RightsImages144;
  TPngImageList *RightsImages192;
  TLabel *OctalLabel;
  TEdit *OctalEdit;
  TGrayedCheckBox *SetUidCheck;
  TGrayedCheckBox *SetGIDCheck;
  TGrayedCheckBox *StickyBitCheck;
  TButton *CloseButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall RightsButtonsClick(TObject *Sender);
  void __fastcall RightsActionsExecute(TBasicAction *Action,
          bool &Handled);
  void __fastcall RightsActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall RightsPopupPopup(TObject *Sender);
  void __fastcall FrameContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall OctalEditChange(TObject * Sender);
  void __fastcall OctalEditExit(TObject * Sender);
  void __fastcall CloseButtonClick(TObject * Sender);
private:
  bool FAllowAddXToDirectories;
  TNotifyEvent FOnChange;
  bool FPopup;
  TWinControl * FPopupParent;
  TButton * FDefaultButton;
  TButton * FCancelButton;
  bool FPopingContextMenu;
  UnicodeString FAddXToDirectoriesSuffix;
  bool FInitialized;
  bool FAcl;
  UnicodeString FOwnerCaption;
  UnicodeString FGroupCaption;
  UnicodeString FOthersCaption;

  void CycleRights(TRights::TRightGroup RightGroup);
  bool __fastcall GetAddXToDirectories();
  bool __fastcall GetAllowUndef();
  TCheckBox * __fastcall GetChecks(TRights::TRight Right);
  TRights __fastcall GetRights();
  TRights::TState __fastcall GetStates(TRights::TRight Right);
  void __fastcall SetAddXToDirectories(bool value);
  void __fastcall SetAllowAddXToDirectories(bool value);
  void __fastcall SetAllowUndef(bool value);
  void __fastcall SetRights(const TRights & value);
  void __fastcall SetStates(TRights::TRight Right, TRights::TState value);
  UnicodeString __fastcall GetText();
  void __fastcall SetText(UnicodeString value);

public:
  virtual __fastcall ~TRightsFrame();
  __fastcall TRightsFrame(TComponent* Owner);
  void __fastcall DropDown();
  void __fastcall CloseUp();
  void DisplayAsAcl();

  __property bool AddXToDirectories = { read = GetAddXToDirectories, write = SetAddXToDirectories };
  __property bool AllowAddXToDirectories = { read = FAllowAddXToDirectories, write = SetAllowAddXToDirectories };
  __property bool AllowUndef = { read = GetAllowUndef, write = SetAllowUndef };
  __property TCheckBox * Checks[TRights::TRight Right] = { read = GetChecks };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property TRights Rights = { read = GetRights, write = SetRights };
  __property UnicodeString Text = { read = GetText, write = SetText };
  __property bool Popup = { read = FPopup, write = SetPopup };
  __property TWinControl * PopupParent = { read = FPopupParent, write = FPopupParent };

protected:
  void __fastcall DoChange();
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(bool Value);
  void __fastcall ForceUpdate();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall CreateWnd();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall CMCancelMode(TCMCancelMode & Message);
  void __fastcall CMDialogKey(TCMDialogKey & Message);
  void __fastcall WMContextMenu(TWMContextMenu & Message);
  void CMDPIChanged(TMessage & Message);
  void CMDialogChar(TCMDialogChar & Message);
  void WMUpdateUIState(TMessage & Message);
  bool __fastcall IsAncestor(TControl * Control, TControl * Ancestor);
  DYNAMIC void __fastcall DoExit();
  void __fastcall SetPopup(bool value);
  void __fastcall DoCloseUp();
  bool __fastcall HasFocus();
  bool __fastcall DirectoriesXEffective();
  void __fastcall UpdateOctalEdit();
  void __fastcall UpdateByOctal();
  void DisplayAsAcl(TRights::TRight ReadRight, TRights::TRight WriteRight, TRights::TRight ExecRight, TRights::TRight SpecialRight);
  void UpdateButtons();
  void UpdateButton(TSpeedButton * Button, UnicodeString & Caption);
  bool IsButtonAccel(TCMDialogChar & Message, TSpeedButton * Button, TWinControl * FocusControl);

  INTERFACE_HOOK_CUSTOM(TFrame);

  __property TRights::TState States[TRights::TRight Right] = { read = GetStates, write = SetStates };
};
//---------------------------------------------------------------------------
#endif
