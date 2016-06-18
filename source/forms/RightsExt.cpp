//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "RightsExt.h"
#include <Common.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Rights"
#pragma link "GrayedCheckBox"
#pragma link "PngImageList"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
__fastcall TRightsExtFrame::TRightsExtFrame(TComponent* Owner)
  : TRightsFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::UpdateControls()
{
  if (!OctalEdit->Focused())
  {
    UpdateOctalEdit();
  }
  TRightsFrame::UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::UpdateByOctal()
{
  if (!OctalEdit->Text.IsEmpty())
  {
    TRights R = Rights;
    R.Octal = OctalEdit->Text;
    Rights = R;
  }
  UpdateControls();
  OctalEdit->Modified = false;
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::UpdateOctalEdit()
{
  TRights R = Rights;
  OctalEdit->Text = R.IsUndef ? UnicodeString() : R.Octal;
  OctalEdit->Modified = false;
  OctalEdit->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::ForceUpdate()
{
  TRightsFrame::ForceUpdate();
  UpdateOctalEdit();
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::OctalEditChange(TObject * /*Sender*/)
{
  if (OctalEdit->Modified && OctalEdit->Text.Length() >= 3)
  {
    try
    {
      UpdateByOctal();
    }
    catch(...)
    {
      OctalEdit->Modified = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::OctalEditExit(TObject * /*Sender*/)
{
  if (!Visible)
  {
    // should happen only if popup is closed by esc key
    assert(Popup);

    // cancel changes
    ForceUpdate();
  }
  else if (OctalEdit->Modified)
  {
    // Now the text in OctalEdit is almost necessarily invalid, otherwise
    // OctalEditChange would have already cleared Modified flag
    try
    {
      UpdateByOctal();
    }
    catch(...)
    {
      OctalEdit->SelectAll();
      OctalEdit->SetFocus();
      throw;
    }
  }
  else
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::SetPopup(bool value)
{
  if (Popup != value)
  {
    TRightsFrame::SetPopup(value);
    CloseButton->Visible = value;
    CloseButton->Cancel = value;
    CloseButton->Default = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRightsExtFrame::CloseButtonClick(TObject * /*Sender*/)
{
  CloseUp();
}
//---------------------------------------------------------------------------
