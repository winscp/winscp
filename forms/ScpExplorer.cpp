//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop                     

#include "ScpExplorer.h"

#include <Common.h>
#include <ScpMain.h>

#include "NonVisual.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AssociatedStatusBar"
#pragma link "CustomDirView"
#pragma link "CustomScpExplorer"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "CustomPathComboBox"
#pragma link "IEComboBox"
#pragma link "UnixPathComboBox"
#pragma link "IEPathComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TScpExplorerForm::TScpExplorerForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{          
  BackButton->DropdownMenu = RemoteDirView->BackMenu;
  ForwardButton->DropdownMenu = RemoteDirView->ForwardMenu;
  SavedSessionsButton->OnClick = DropDownButtonMenu;

  TopCoolBar->PopupMenu = NonVisualDataModule->ExplorerBarPopup;
  RemoteStatusBar->PopupMenu = NonVisualDataModule->ExplorerBarPopup;

  // set common explorer shorcuts to our actions
  NonVisualDataModule->ExplorerShortcuts();
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RestoreFormParams()
{
  assert(Configuration);

  TCustomScpExplorerForm::RestoreFormParams();
  Configuration->RestoreForm(Configuration->ScpExplorer.WindowParams, this);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  UnixPathComboBox->ShowFullPath = Configuration->ScpExplorer.ShowFullAddress;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RestoreParams()
{
  assert(Configuration);

  SetCoolBandsMinWidth(TopCoolBar);

  TCustomScpExplorerForm::RestoreParams();

  RemoteDirView->UnixColProperties->ParamsStr = Configuration->ScpExplorer.DirViewParams;
  RemoteDirView->ViewStyle = (TViewStyle)Configuration->ScpExplorer.ViewStyle;
  LoadCoolbarLayoutStr(TopCoolBar, Configuration->ScpExplorer.CoolBarLayout);
  RemoteStatusBar->Visible = Configuration->ScpExplorer.StatusBar;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::StoreParams()
{
  assert(Configuration);

  Configuration->BeginUpdate();
  try {
    Configuration->ScpExplorer.CoolBarLayout = GetCoolbarLayoutStr(TopCoolBar);
    Configuration->ScpExplorer.StatusBar = RemoteStatusBar->Visible;

    Configuration->ScpExplorer.WindowParams = Configuration->StoreForm(this);;
    Configuration->ScpExplorer.DirViewParams = RemoteDirView->UnixColProperties->ParamsStr;
    Configuration->ScpExplorer.ViewStyle = RemoteDirView->ViewStyle;
    TCustomScpExplorerForm::StoreParams();
  } __finally {
    Configuration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TScpExplorerForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, Boolean DragDrop, TStrings * FileList,
  AnsiString & TargetDirectory, TCopyParamType & CopyParam, Boolean Confirm)
{
  if ((Direction == tdToLocal) && !DragDrop)
    TargetDirectory = Configuration->ScpExplorer.LastLocalTargetDirectory;
  Boolean Result = TCustomScpExplorerForm::CopyParamDialog(
    Direction, Type, DragDrop, FileList, TargetDirectory, CopyParam, Confirm);
  if (Result && (Direction == tdToLocal) && !DragDrop)
    Configuration->ScpExplorer.LastLocalTargetDirectory = TargetDirectory;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::FormShow(TObject * /*Sender*/)
{
  FLastDirView = RemoteDirView; // Only dir view
  RemoteDirView->SetFocus();
}
//---------------------------------------------------------------------------
Boolean __fastcall TScpExplorerForm::AllowedAction(TAction * Action, TActionAllowed Allowed)
{
  #define FLAG ((TActionFlag)(Action->Tag))
  return
    // always require Explorer flag
    (FLAG & afExplorer) &&
    // if action is execution or update, we don't require any other flag
    // if we check for shortcut, we require that action is designed for remote panel
    ((Allowed != aaShortCut) || (FLAG & afRemote));
  #undef FLAG
}
//---------------------------------------------------------------------------



