//---------------------------------------------------------------------------
#ifndef RightsH
#define RightsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>

#include <RemoteFiles.h>
#include <ActnList.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TRightsFrame : public TFrame
{
__published:
  TLabel *GroupLabel;
  TLabel *OthersLabel;
  TLabel *OwnerLabel;
  TCheckBox *OwnerReadCheck;
  TCheckBox *OwnerWriteCheck;
  TCheckBox *OwnerExecuteCheck;
  TCheckBox *GroupReadCheck;
  TCheckBox *GroupWriteCheck;
  TCheckBox *GroupExecuteCheck;
  TCheckBox *OthersReadCheck;
  TCheckBox *OthersWriteCheck;
  TCheckBox *OthersExecuteCheck;
  TCheckBox *DirectoriesXCheck;
  TSpeedButton *OwnerButton;
  TSpeedButton *GroupButton;
  TSpeedButton *OthersButton;
  TLabel *OctalLabel;
  TEdit *OctalEdit;
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
  TImageList *RightsImages;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall RightsButtonsClick(TObject *Sender);
  void __fastcall OctalEditExit(TObject *Sender);
  void __fastcall OctalEditChange(TObject *Sender);
  void __fastcall RightsActionsExecute(TBasicAction *Action,
          bool &Handled);
  void __fastcall RightsActionsUpdate(TBasicAction *Action, bool &Handled);
private:
  bool FAllowAddXToDirectories;
  TNotifyEvent FOnChange;

  void __fastcall CycleRights(int Group);
  bool __fastcall GetAddXToDirectories();
  bool __fastcall GetAllowUndef();
  int __fastcall GetCheckCount();
  TCheckBox * __fastcall GetChecks(TRightsFlag Flag);
  TRights __fastcall GetRights();
  TRightState __fastcall GetStates(TRightsFlag Flag);
  void __fastcall SetAddXToDirectories(bool value);
  void __fastcall SetAllowAddXToDirectories(bool value);
  void __fastcall SetAllowUndef(bool value);
  void __fastcall SetRights(TRights value);
  void __fastcall SetStates(TRightsFlag Flag, TRightState value);

public:
  virtual __fastcall ~TRightsFrame();
  __fastcall TRightsFrame(TComponent* Owner);
  __property bool AddXToDirectories = { read = GetAddXToDirectories, write = SetAddXToDirectories };
  __property bool AllowAddXToDirectories = { read = FAllowAddXToDirectories, write = SetAllowAddXToDirectories };
  __property bool AllowUndef = { read = GetAllowUndef, write = SetAllowUndef };
  __property int CheckCount = { read = GetCheckCount };
  __property TCheckBox * Checks[TRightsFlag Flag] = { read = GetChecks };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property TRights Rights = { read = GetRights, write = SetRights };
  __property TRightState States[TRightsFlag Flag] = { read = GetStates, write = SetStates };

protected:
  void __fastcall DoChange();
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(bool Value);
  void __fastcall UpdateByOctal();
};
//---------------------------------------------------------------------------
#endif
