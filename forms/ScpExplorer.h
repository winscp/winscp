//---------------------------------------------------------------------------
#ifndef ScpExplorerH
#define ScpExplorerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <AssociatedStatusBar.hpp>
#include <CustomDirView.hpp>
#include <CustomScpExplorer.h>
#include <CustomUnixDirView.hpp>
#include <IEListView.hpp>
#include <NortonLikeListView.hpp>
#include <UnixDirView.h>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <CustomPathComboBox.hpp>
#include <IEComboBox.hpp>
#include <UnixPathComboBox.h>
#include <IEPathComboBox.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TScpExplorerForm : public TCustomScpExplorerForm
{
__published:
  TToolBar *MenuToolBar;
  TToolButton *ToolButton2;
  TToolButton *ToolButton19;
  TToolButton *ToolButton16;
  TToolButton *ToolButton17;
  TToolBar *ButtonsToolBar;
  TToolButton *BackButton;
  TToolButton *ForwardButton;
  TToolButton *ToolButton12;
  TToolButton *ToolButton4;
  TToolButton *ToolButton9;
  TToolButton *ToolButton10;
  TToolButton *ToolButton11;
  TToolButton *ToolButton18;
  TToolButton *ToolButton3;
  TToolButton *ToolButton5;
  TToolButton *ToolButton6;
  TToolButton *ToolButton7;
  TToolButton *ToolButton14;
  TToolButton *ToolButton15;
  TToolButton *ToolButton13;
  TToolBar *SelectionToolbar;
  TToolButton *ToolButton23;
  TToolButton *ToolButton24;
  TToolButton *ToolButton25;
  TToolButton *ToolButton26;
  TToolButton *ToolButton20;
  TToolButton *ToolButton21;
  TToolButton *ToolButton27;
  TToolBar *SessionToolbar;
  TToolButton *ToolButton28;
  TToolButton *ToolButton29;
  TToolButton *ToolButton30;
  TToolButton *SavedSessionsButton;
  TToolBar *PreferencesToolbar;
  TToolButton *ToolButton31;
  TToolButton *ToolButton35;
  TToolButton *ToolButton32;
  TToolButton *ToolButton33;
  TToolBar *SortToolbar;
  TToolButton *ToolButton8;
  TToolButton *ToolButton1;
  TToolButton *ToolButton22;
  TToolButton *ToolButton36;
  TToolButton *ToolButton37;
  TToolButton *ToolButton38;
  TToolButton *ToolButton39;
  TToolButton *ToolButton40;
  TToolButton *ToolButton34;
  TToolButton *ToolButton41;
  TToolBar *ToolBar5;
  TUnixPathComboBox *UnixPathComboBox;
  TToolButton *ToolButton42;
  TToolButton *ToolButton43;
  TToolButton *ToolButton44;
  TToolButton *ToolButton45;
  TToolButton *ToolButton46;
  TToolButton *ToolButton47;
  TComboBox *SessionCombo;
  TToolButton *ToolButton48;
  TToolButton *ToolButton49;
  TToolButton *ToolButton50;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall RemoteStatusBarDblClick(TObject *Sender);
private:
protected:
  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool DragDrop, TStrings * FileList,
    AnsiString & TargetDirectory, TCopyParamType & CopyParam, bool Confirm);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall ConfigurationChanged();
  virtual TControl * __fastcall GetComponent(Byte Component);
  virtual void __fastcall FixControlsPlacement();

public:
  __fastcall TScpExplorerForm(TComponent* Owner);
  virtual Boolean __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  virtual void __fastcall StoreParams();
  virtual void __fastcall FullSynchronizeDirectories();
};
//---------------------------------------------------------------------------
#endif
