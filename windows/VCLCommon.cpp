//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "VCLCommon.h"

#include <Common.h>
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
  ListView->Columns->Items[ListView->Columns->Count-1]->Width =	CWidth-NewWidth;
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
void __fastcall UseSystemFont(TCustomForm * Control)
{
  assert(Control && Control->Font);
  Control->Font->Name = "MS Shell Dlg";
};
//---------------------------------------------------------------------------
void __fastcall LinkLabel(TLabel * Label)
{
  Label->ParentFont = true;
  Label->Font->Style = Label->Font->Style << fsUnderline;
  Label->Font->Color = clBlue;
}
//---------------------------------------------------------------------------
/*void __fastcall ShowAsModal(TCustomForm * Form)
{
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  FFormState << fsModal;
  FFocusActiveWindow = GetActiveWindow();

  FFocusWindowList = DisableTaskWindows(0);
  Show();
  SendMessage(Handle, CM_ACTIVATE, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::HideAsModal()
{
  SendMessage(Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Handle)
  {
    FFocusActiveWindow = 0;
  }
  Hide();

  EnableTaskWindows(FFocusWindowList);

  if (FFocusActiveWindow != 0)
  {
    SetActiveWindow(FFocusActiveWindow);
  }

  FFormState >> fsModal;
} */

