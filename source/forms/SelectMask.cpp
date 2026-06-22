//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <FileMasks.h>
#include "SelectMask.h"
//---------------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoSelectMaskDialog(TControl * Parent, bool Select, TFileFilter & Filter)
{
  std::unique_ptr<TSelectMaskDialog> Dialog(new TSelectMaskDialog(Application));
  Dialog->Init((Select ? TSelectMaskDialog::smSelect : TSelectMaskDialog::smDeselect), Parent);
  DefaultFileFilter(Filter);
  Filter.Masks = WinConfiguration->SelectMask;
  Filter.Directories = WinConfiguration->SelectDirectories;
  TColor Color = TColor();
  bool Result = Dialog->Execute(Filter, Color);
  if (Result)
  {
    WinConfiguration->SelectMask = Filter.Masks;
    WinConfiguration->SelectDirectories = Filter.Directories;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoFilterMaskDialog(TControl * Parent, UnicodeString & Mask)
{
  std::unique_ptr<TSelectMaskDialog> Dialog(new TSelectMaskDialog(Application));
  Dialog->Init(TSelectMaskDialog::smFilter, Parent);
  TColor Color = TColor();
  return Dialog->Execute(Mask, Color);
}
//---------------------------------------------------------------------------
bool __fastcall DoFileColorDialog(TFileColorData & FileColorData)
{
  std::unique_ptr<TSelectMaskDialog> Dialog(new TSelectMaskDialog(Application));
  Dialog->Init(TSelectMaskDialog::smFileColor, NULL);
  UnicodeString Mask = FileColorData.FileMask.Masks;
  bool Result = Dialog->Execute(Mask, FileColorData.Color);
  if (Result)
  {
    FileColorData.FileMask.Masks = Mask;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSelectMaskDialog::TSelectMaskDialog(TComponent * Owner) :
  TForm(Owner)
{
  UseSystemSettings(this);
  FColor = TColor();
  HintLabel(HintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2), LoadStr(FILE_MASK_EX_HINT),
      LoadStr(COMBINING_MASKS_HINT), LoadStr(MASK_HELP))));
  ColorFileNamesLabel->Font = Screen->IconFont;
  ColorSizesLabel->Font = ColorFileNamesLabel->Font;
  ColorSizesLabel->Caption =
    FormatPanelBytes(10723, WinConfiguration->FormatSizeBytes) + sLineBreak +
    FormatPanelBytes(25835, WinConfiguration->FormatSizeBytes) + sLineBreak +
    FormatPanelBytes(276445, WinConfiguration->FormatSizeBytes) + sLineBreak;
  MenuButton(ColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::Init(TMode Mode, TControl * Parent)
{
  int CaptionStr;
  switch (Mode)
  {
    case smSelect:
      CaptionStr = SELECT_MASK_SELECT_CAPTION;
      ClearButton->Hide();
      ColorButton->Hide();
      break;

    case smDeselect:
      CaptionStr = SELECT_MASK_DESELECT_CAPTION;
      ClearButton->Hide();
      ColorButton->Hide();
      break;

    case smFilter:
      CaptionStr = FILTER_MASK_CAPTION;
      ApplyToDirectoriesCheck->Hide();
      ColorButton->Hide();
      HelpKeyword = HELP_FILTER;
      break;

    case smFileColor:
      CaptionStr = FILE_COLOR_CAPTION;
      ApplyToDirectoriesCheck->Hide();
      ClearButton->Hide();
      HelpKeyword = HELP_FILE_COLORS;
      break;
  }

  if (!ColorButton->Visible)
  {
    ColorFileNamesLabel->Visible = false;
    ColorSizesLabel->Visible = false;
    ColorPaddingLabel->Visible = false;
    ColorButton->Visible = false;
    int Diff = ((ColorFileNamesLabel->Top + ColorFileNamesLabel->Height) - (ApplyToDirectoriesCheck->Top + ApplyToDirectoriesCheck->Height));
    ClientHeight = ClientHeight - Diff;
  }

  Caption = LoadStr(CaptionStr);
  FParent = Parent;
  if (FParent == NULL)
  {
    Position = poOwnerFormCenter;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormCloseQuery(TObject *, bool & DebugUsedArg(CanClose))
{
  if (ModalResult == DefaultResult(this))
  {
    if (MaskEdit->Focused())
    {
      MaskEditExit(NULL);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSelectMaskDialog::Execute(TFileFilter & FileFilter, TColor & Color)
{
  ApplyToDirectoriesCheck->Checked = FileFilter.Directories;
  MaskEdit->Text = FileFilter.Masks;
  ActiveControl = MaskEdit;
  FColor = Color;
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    MaskEdit->SaveToHistory();
    FileFilter.Directories = ApplyToDirectoriesCheck->Checked;
    FileFilter.Masks = MaskEdit->Text;
    Color = FColor;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSelectMaskDialog::Execute(UnicodeString & Mask, TColor & Color)
{
  TFileFilter Filter;
  DefaultFileFilter(Filter);
  Filter.Masks = Mask;
  bool Result = Execute(Filter, Color);
  if (Result)
  {
    Mask = Filter.Masks;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::MaskEditExit(TObject * /*Sender*/)
{
  ValidateMaskEdit(MaskEdit);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::ClearButtonClick(TObject * /*Sender*/)
{
  MaskEdit->Text = L"";
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(MaskEdit);
  if (FParent != NULL)
  {
    // Only now it is scaled
    TRect Bounds = BoundsRect;
    CenterFormOn(Bounds, FParent, nullptr);
    BoundsRect = Bounds;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::MaskButtonClick(TObject * /*Sender*/)
{
  TFileMasks Masks = MaskEdit->Text;
  if (DoEditMaskDialog(Masks))
  {
    MaskEdit->Text = Masks.Masks;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::ColorChange(TColor Color)
{
  FColor = Color;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::ColorButtonClick(TObject *)
{
  // Reason for separate Menu variable is given in TPreferencesDialog::EditorFontColorButtonClick
  TPopupMenu * Menu = CreateColorPopupMenu(FColor, ColorChange);
  // Popup menu has to survive the popup as TBX calls click handler asynchronously (post).
  FColorPopupMenu.reset(Menu);
  MenuPopup(Menu, ColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::UpdateControls()
{
  EnableControl(OKBtn, (!ColorButton->Visible || !MaskEdit->Text.IsEmpty()));
  ColorFileNamesLabel->Font->Color = FColor;
  ColorSizesLabel->Font->Color = ColorFileNamesLabel->Font->Color;
}
//---------------------------------------------------------------------------
void __fastcall TSelectMaskDialog::MaskEditChange(TObject *)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
