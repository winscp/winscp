//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "VCLCommon.h"

#include <Common.h>
#include <TextsWin.h>
#include <RemoteFiles.h>

#include <FileCtrl.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView)
{
  int OriginalWidth, NewWidth, i, CWidth;

  OriginalWidth = 0;
  for (i = 0; i < ListView->Columns->Count; i++)
  {
    OriginalWidth += ListView->Columns->Items[i]->Width;
  }

  NewWidth = 0;
  CWidth = ListView->ClientWidth;
  if ((ListView->VisibleRowCount < ListView->Items->Count) &&
      (ListView->Width - ListView->ClientWidth < GetSystemMetrics(SM_CXVSCROLL)))
  {
    CWidth -= GetSystemMetrics(SM_CXVSCROLL);
  }
  for (i = 0; i < ListView->Columns->Count-1;i++)
  {
    if (ListView->Columns->Items[i]->Tag == 0)
    {
      ListView->Columns->Items[i]->Width =
        (CWidth * ListView->Columns->Items[i]->Width) / OriginalWidth;
    }
    NewWidth += ListView->Columns->Items[i]->Width;
  }
  ListView->Columns->Items[ListView->Columns->Count-1]->Width = CWidth-NewWidth;
}
//---------------------------------------------------------------------------
void __fastcall EnableControl(TControl * Control, bool Enable)
{
  if (Control->Enabled != Enable)
  {
    if (Control->InheritsFrom(__classid(TWinControl)) &&
        (((TWinControl*)Control)->ControlCount > 0))
    {
      for (Integer Index = 0; Index < ((TWinControl*)Control)->ControlCount; Index++)
        EnableControl(((TWinControl*)Control)->Controls[Index], Enable);
    }
    Control->Enabled = Enable;
  }
  if (Control->InheritsFrom(__classid(TCustomEdit)) ||
            Control->InheritsFrom(__classid(TCustomComboBox)))
  {
    if (Enable) ((TEdit*)Control)->Color = clWindow;
      else ((TEdit*)Control)->Color = clBtnFace;
  }
};
//---------------------------------------------------------------------------
struct TSavedSystemSettings
{
  AnsiString FontName;
  bool Flipped;
};
//---------------------------------------------------------------------------
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings)
{
  bool Flip;
  AnsiString FlipStr = LoadStr(FLIP_CHILDREN);
  Flip = !FlipStr.IsEmpty() && static_cast<bool>(StrToInt(FlipStr));

  if (Settings)
  {
    TSavedSystemSettings * SSettings = new TSavedSystemSettings();
    *Settings = static_cast<void*>(SSettings);
    SSettings->FontName = Control->Font->Name;
    SSettings->Flipped = Flip;
  }
  assert(Control && Control->Font);
  Control->Font->Name = "MS Shell Dlg";
  if (Flip)
  {
    Control->FlipChildren(true);
  }
};
//---------------------------------------------------------------------------
void __fastcall RevokeSystemSettings(TCustomForm * Control,
  void * Settings)
{
  assert(Settings);
  if (static_cast<TSavedSystemSettings*>(Settings)->Flipped)
  {
    Control->FlipChildren(true);
  }
  delete Settings;
};
//---------------------------------------------------------------------------
void __fastcall LinkLabel(TLabel * Label)
{
  Label->ParentFont = true;
  Label->Font->Style = Label->Font->Style << fsUnderline;
  Label->Font->Color = clBlue;
}
//---------------------------------------------------------------------------
class TPublicForm : public TForm
{
friend void __fastcall ShowAsModal(TForm * Form, void *& Storage);
friend void __fastcall HideAsModal(TForm * Form, void *& Storage);
};
//---------------------------------------------------------------------------
struct TShowAsModalStorage
{
  void * FocusWindowList;
  void * FocusActiveWindow;
};
//---------------------------------------------------------------------------
void __fastcall ShowAsModal(TForm * Form, void *& Storage)
{
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  (static_cast<TPublicForm*>(Form))->FFormState << fsModal;

  TShowAsModalStorage * AStorage = new TShowAsModalStorage;

  AStorage->FocusActiveWindow = GetActiveWindow();

  AStorage->FocusWindowList = DisableTaskWindows(0);
  Form->Show();
  SendMessage(Form->Handle, CM_ACTIVATE, 0, 0);

  Storage = AStorage;
}
//---------------------------------------------------------------------------
void __fastcall HideAsModal(TForm * Form, void *& Storage)
{
  assert((static_cast<TPublicForm*>(Form))->FFormState.Contains(fsModal));
  TShowAsModalStorage * AStorage = static_cast<TShowAsModalStorage *>(Storage);
  Storage = NULL;

  SendMessage(Form->Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Form->Handle)
  {
    AStorage->FocusActiveWindow = 0;
  }
  Form->Hide();

  EnableTaskWindows(AStorage->FocusWindowList);

  if (AStorage->FocusActiveWindow != 0)
  {
    SetActiveWindow(AStorage->FocusActiveWindow);
  }

  (static_cast<TPublicForm*>(Form))->FFormState >> fsModal;

  delete AStorage;
}
//---------------------------------------------------------------------------
void __fastcall ReleaseAsModal(TForm * Form, void *& Storage)
{
  if (Storage != NULL)
  {
    HideAsModal(Form, Storage);
  }
}
//---------------------------------------------------------------------------
bool __fastcall SelectDirectory(AnsiString & Path, const AnsiString Prompt,
  bool PreserveFileName)
{
  bool Result;
  unsigned int ErrorMode;
  ErrorMode = SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);

  try
  {
    AnsiString Directory;
    AnsiString FileName;
    if (!PreserveFileName || DirectoryExists(Path))
    {
      Directory = Path;
    }
    else
    {
      Directory = ExtractFilePath(Path);
      FileName = ExtractFileName(Path);
    }
    Result = SelectDirectory(Prompt, "", Directory);
    if (Result)
    {
      Path = Directory;
      if (!FileName.IsEmpty())
      {
        Path = IncludeTrailingBackslash(Path) + FileName;
      }
    }
  }
  __finally
  {
    SetErrorMode(ErrorMode);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ListViewAnyChecked(TListView * ListView, bool Checked)
{
  bool AnyChecked = false;
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    if (ListView->Items->Item[Index]->Checked == Checked)
    {
      AnyChecked = true;
      break;
    }
  }
  return AnyChecked;
}
//---------------------------------------------------------------------------
void __fastcall ListViewCheckAll(TListView * ListView,
  TListViewCheckAll CheckAll)
{
  bool Check;

  if (CheckAll == caToggle)
  {
    Check = ListViewAnyChecked(ListView, false);
  }
  else
  {
    Check = (CheckAll == caCheck);
  }

  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    ListView->Items->Item[Index]->Checked = Check;
  }
}
//---------------------------------------------------------------------------
void __fastcall PathComboBoxKeyDown(
  TCustomComboBox * ComboBox, WORD & Key, TShiftState Shift, bool Unix)
{
  assert(ComboBox != NULL);

  if (((Key == VK_LEFT) || (Key == VK_RIGHT)) && Shift.Contains(ssCtrl))
  {
    int SelStart = ComboBox->SelStart;
    int SelLength = ComboBox->SelLength;
    SkipPathComponent(reinterpret_cast<TComboBox*>(ComboBox)->Text,
      SelStart, SelLength, (Key == VK_LEFT), Unix);
    ComboBox->SelStart = SelStart;
    ComboBox->SelLength = SelLength;
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall PathEditKeyDown(
  TCustomEdit * Edit, WORD & Key, TShiftState Shift, bool Unix)
{
  assert(Edit != NULL);

  if (((Key == VK_LEFT) || (Key == VK_RIGHT)) && Shift.Contains(ssCtrl))
  {
    int SelStart = Edit->SelStart;
    int SelLength = Edit->SelLength;
    SkipPathComponent(Edit->Text,
      SelStart, SelLength, (Key == VK_LEFT), Unix);
    Edit->SelStart = SelStart;
    Edit->SelLength = SelLength;
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall RepaintStatusBar(TCustomStatusBar * StatusBar)
{
  StatusBar->SimplePanel = !StatusBar->SimplePanel;
  StatusBar->SimplePanel = !StatusBar->SimplePanel;
}
//---------------------------------------------------------------------------
void __fastcall SetVerticalControlsOrder(TControl ** ControlsOrder, int Count)
{
  for (int Index = Count - 1; Index > 0; Index--)
  {
    if ((ControlsOrder[Index]->Top < ControlsOrder[Index - 1]->Top) &&
        ControlsOrder[Index - 1]->Visible)
    {
      ControlsOrder[Index]->Top = ControlsOrder[Index - 1]->Top +
        ControlsOrder[Index - 1]->Height;
      Index = Count;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall SetHorizontalControlsOrder(TControl ** ControlsOrder, int Count)
{
  for (int Index = Count - 1; Index > 0; Index--)
  {
    if ((ControlsOrder[Index]->Left < ControlsOrder[Index - 1]->Left) &&
        ControlsOrder[Index - 1]->Visible)
    {
      ControlsOrder[Index]->Left = ControlsOrder[Index - 1]->Left +
        ControlsOrder[Index - 1]->Width;
      Index = Count;
    }
  }
}
//---------------------------------------------------------------------------
TPoint __fastcall GetAveCharSize(TCanvas* Canvas)
{
  Integer I;
  Char Buffer[52];
  TSize Result;
  for (I = 0; I <= 25; I++) Buffer[I] = (Char)('A' + I);
  for (I = 0; I <= 25; I++) Buffer[I+26] = (Char)('a' + I);
  GetTextExtentPoint(Canvas->Handle, Buffer, 52, &Result);
  return TPoint(Result.cx / 52, Result.cy);
}
//---------------------------------------------------------------------------
void __fastcall MakeNextInTabOrder(TWinControl * Control, TWinControl * After)
{
  if (After->TabOrder > Control->TabOrder)
  {
    After->TabOrder = Control->TabOrder;
  }
  else if (After->TabOrder < Control->TabOrder - 1)
  {
    After->TabOrder = static_cast<TTabOrder>(Control->TabOrder - 1);
  }
}
