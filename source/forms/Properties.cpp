//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "WinInterface.h"
#include "Properties.h"

#include <VCLCommon.h>
#include <Common.h>
#include <Terminal.h>
#include <TextsWin.h>
#include <GUITools.h>
#include <CoreMain.h>
#include <Tools.h>
#include <BaseUtils.hpp>
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma link "Rights"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPropertiesDialog(TStrings * FileList,
  const UnicodeString Directory, const TRemoteTokenList * GroupList,
  const TRemoteTokenList * UserList, TStrings * ChecksumAlgs,
  TRemoteProperties * Properties,
  int AllowedChanges, int Options, TCalculateSizeEvent OnCalculateSize,
  TCalculateChecksumEvent OnCalculateChecksum)
{
  bool Result;
  TPropertiesDialog * PropertiesDialog = new TPropertiesDialog(Application,
    FileList, Directory, GroupList, UserList, ChecksumAlgs, AllowedChanges, Options,
    OnCalculateSize, OnCalculateChecksum);
  try
  {
    Result = PropertiesDialog->Execute(*Properties);
  }
  __finally
  {
    delete PropertiesDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPropertiesDialog::TPropertiesDialog(TComponent* AOwner,
  TStrings * FileList, const UnicodeString Directory,
  const TRemoteTokenList * GroupList, const TRemoteTokenList * UserList,
  TStrings * ChecksumAlgs,
  int AllowedChanges, int Options, TCalculateSizeEvent OnCalculateSize,
  TCalculateChecksumEvent OnCalculateChecksum)
  : TForm(AOwner)
{
  FOnCalculateSize = OnCalculateSize;
  FOnCalculateChecksum = OnCalculateChecksum;
  RightsFrame->OnChange = ControlChange;

  FFileList = new TStringList();
  FFileList->Assign(FileList);
  FAllowedChanges = AllowedChanges;
  if (FLAGSET(FAllowedChanges, cpAcl))
  {
    DebugAssert(FLAGCLEAR(FAllowedChanges, cpMode));
    RightsLabel->Caption = LoadStr(PROPERTIES_ACL);
    RightsFrame->DisplayAsAcl();
  }
  FOptions = Options;

  FAllowCalculateStats = false;
  FStatsNotCalculated = false;
  FChecksumLoaded = false;
  FMultipleChecksum = false;

  LocationLabel->Text = Directory;

  FGroupList = GroupList;
  FUserList = UserList;
  FChecksumAlgs = ChecksumAlgs;

  ReadOnlyControl(ChecksumEdit);
  ReadOnlyControl(OwnerView);
  ReadOnlyControl(GroupView);
  ReadOnlyControl(FileLabel);
  ReadOnlyControl(LocationLabel);
  ReadOnlyControl(SizeLabel);
  ReadOnlyControl(LinksToLabel);
  ChecksumUnknownLabel->Caption = LoadStr(PROPERTIES_CHECKSUM_UNKNOWN);
  UseSystemSettings(this);
  LoadInfo();
}
//---------------------------------------------------------------------------
__fastcall TPropertiesDialog::~TPropertiesDialog()
{
  delete FFileList;
  FFileList = NULL;
}
//---------------------------------------------------------------------
bool __fastcall TPropertiesDialog::Execute(TRemoteProperties & Properties)
{
  SetFileProperties(Properties);

  PageControl->ActivePage = CommonSheet;
  if (OwnerComboBox->Visible && OwnerComboBox->Enabled)
  {
    ActiveControl = OwnerComboBox;
  }
  else if (GroupComboBox->Visible && GroupComboBox->Enabled)
  {
    ActiveControl = GroupComboBox;
  }
  else if (RightsFrame->Visible && RightsFrame->Enabled)
  {
    ActiveControl = RightsFrame;
  }
  else if (DebugAlwaysTrue(CancelButton->Visible && CancelButton->Enabled))
  {
    ActiveControl = CancelButton;
  }

  if (DebugAlwaysTrue(FChecksumAlgs != NULL))
  {
    ChecksumAlgEdit->Items->Assign(FChecksumAlgs);
    int ChecksumIndex = FChecksumAlgs->IndexOf(GUIConfiguration->ChecksumAlg);
    if (ChecksumIndex < 0)
    {
      ChecksumIndex = 0;
    }
    ChecksumAlgEdit->ItemIndex = ChecksumIndex;
  }
  ResetChecksum();

  UpdateControls();

  bool Result = (ShowModal() == DefaultResult());

  if (Result)
  {
    Properties = GetFileProperties();
  }

  return Result;
}
//---------------------------------------------------------------------------
TModalResult __fastcall TPropertiesDialog::DefaultResult()
{
  // Fallback when ChecksumButton is default
  return ::DefaultResult(this, OkButton);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TPropertiesDialog::LoadRemoteToken(
  const TRemoteToken & Token)
{
  UnicodeString Result;
  if (FLAGSET(FOptions, poUserGroupByID))
  {
    if (Token.IDValid)
    {
      if (Token.NameValid)
      {
        Result = FORMAT(L"%s [%d]", (Token.Name, int(Token.ID)));
      }
      else
      {
        Result = FORMAT(L"[%d]", (int(Token.ID)));
      }
    }
    else
    {
      // be it valid or not
      Result = Token.Name;
    }
  }
  else
  {
    // what if name is not filled in?
    Result = Token.Name;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadRemoteToken(
  TComboBox * ComboBox, TEdit * View, TLabel * Label, bool Valid, const TRemoteToken & Token, int Change)
{
  UnicodeString Value = Valid ? LoadRemoteToken(Token) : EmptyStr;
  ComboBox->Text = Value;
  View->Text = Value;
  bool AllowedChange = FLAGSET(FAllowedChanges, Change);
  ComboBox->Visible = AllowedChange;
  View->Visible = Valid && !AllowedChange;
  Label->FocusControl = AllowedChange ? static_cast<TWinControl *>(ComboBox) : static_cast<TWinControl *>(View);
  Label->Visible = Label->FocusControl->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadRemoteTokens(TComboBox * ComboBox,
  const TRemoteTokenList * List)
{
  TStrings * Items = ComboBox->Items;
  Items->BeginUpdate();
  try
  {
    Items->Clear();
    if (List != NULL)
    {
      int Count = List->Count();
      for (int Index = 0; Index < Count; Index++)
      {
        Items->Add(LoadRemoteToken(*List->Token(Index)));
      }
    }
  }
  __finally
  {
    Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadInfo()
{
  DebugAssert(FFileList->Count > 0);
  FMultiple = FFileList->Count > 1;
  FMultipleChecksum = FMultiple;

  FAllowCalculateStats = false;
  FStatsNotCalculated = false;

  __int64 FilesSize = 0;
  TCalculateSizeStats Stats;

  for (int Index = 0; Index < FFileList->Count; Index++)
  {
    TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[Index]);
    if (File->IsDirectory)
    {
      Stats.Directories++;
      // we should use TTerminal::CanRecurseToDirectory instead
      if (!File->IsSymLink)
      {
        FAllowCalculateStats = true;
        FStatsNotCalculated = true;
        FMultipleChecksum = true;
      }
    }
    else
    {
      Stats.Files++;
    }

    if (File->IsSymLink)
    {
      Stats.SymLinks++;
    }
    FilesSize += File->Size;
  }

  // before it gets eventualy cleared if !FMultiple
  bool ShowTags = FLAGSET(FOptions, poTags) && (Stats.Files == 1) && (Stats.Directories == 0);

  LoadRemoteTokens(GroupComboBox, FGroupList);
  LoadRemoteTokens(OwnerComboBox, FUserList);

  FAnyDirectories = (Stats.Directories > 0);
  RightsFrame->AllowAddXToDirectories = FAnyDirectories;

  if (!FMultiple)
  {
    // Show only file name, if we have only single file/directory.
    // For directory, this changes, once "Calculate" button is pressed
    Stats = TCalculateSizeStats();
  }

  LoadStats(FilesSize, Stats);

  RightsFrame->AllowUndef = FMultiple;

  if (!FMultiple)
  {
    TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);
    DebugAssert(File);

    UpdateFileImage();

    LinksToLabelLabel->Visible = File->IsSymLink;
    LinksToLabel->Visible = File->IsSymLink;
    if (File->IsSymLink)
    {
      LinksToLabel->Text = File->LinkTo;
    }

    Caption = FMTLOAD(PROPERTIES_FILE_CAPTION, (File->FileName));
  }
  else
  {
    Caption = FMTLOAD(PROPERTIES_FILES_CAPTION, (FFileList->Strings[0]));
    LinksToLabelLabel->Hide();
    LinksToLabel->Hide();
    LoadDialogImage(FileIconImage, L"Multiple Files");
  }

  ChecksumGroup->Visible = !FMultipleChecksum;
  ChecksumView->Visible = FMultipleChecksum;

  TagsSheet->TabVisible = ShowTags;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::UpdateFileImage()
{
  TImageList * ImageList = ShellImageListForControl(this, ilsLarge);

  FileIconImage->Picture->Bitmap = NULL;

  TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);

  // shell image list does not have fixed large icon size
  // (it is probably 32x32 min, but can be larger, on xp it is 48x48 if
  // large icons are enabled, on vista can be even larger).
  // so we stretch (shrink) the icon to 32x32 here to be sure.
  Graphics::TBitmap * Bitmap = new Graphics::TBitmap;
  try
  {
    ImageList->GetBitmap(File->IconIndex, Bitmap);
    int Size = DialogImageSize(this);
    // Use exact DPI-scaled size, not approximate scaling by font size.
    // Otherwise we stretch icons unnecessarily because the canvas
    // is one or two pixels off the icon size
    FileIconImage->Width = Size;
    FileIconImage->Height = Size;
    FileIconImage->Picture->Bitmap->Width = Size;
    FileIconImage->Picture->Bitmap->Height = Size;
    FileIconImage->Picture->Bitmap->Canvas->StretchDraw(
      TRect(0, 0, Size, Size),
      Bitmap);
  }
  __finally
  {
    delete Bitmap;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadStats(__int64 FilesSize,
  const TCalculateSizeStats & Stats)
{
  UnicodeString SizeStr;
  UnicodeString FilesStr;
  if (FStatsNotCalculated)
  {
    SizeStr = LoadStr(PROPERTIES_UNKNOWN_SIZE);
  }
  else
  {
    SizeStr = FormatBytes(FilesSize);
    UnicodeString SizeUnorderedStr = FormatBytes(FilesSize, fbNone);
    if (SizeStr != SizeUnorderedStr)
    {
      SizeStr = FORMAT(L"%s (%s)", (SizeStr, SizeUnorderedStr));
    }
  }

  if (((Stats.Files + Stats.Directories) == 0) && !FMultiple)
  {
    TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);
    DebugAssert(File != NULL);
    FilesStr = File->FileName;
  }
  else
  {
    if (Stats.Files > 0)
    {
      FilesStr = (Stats.Files == 1) ? FMTLOAD(PROPERTIES_FILE, (Stats.Files)) :
        FMTLOAD(PROPERTIES_FILES, (Stats.Files));
      if (Stats.Directories > 0)
      {
        FilesStr = FORMAT(L"%s, ", (FilesStr));
      }
    }
    if (Stats.Directories > 0)
    {
      FilesStr += (Stats.Directories == 1) ? FMTLOAD(PROPERTIES_DIRECTORY, (Stats.Directories)) :
        FMTLOAD(PROPERTIES_DIRECTORIES, (Stats.Directories));
    }
    if (Stats.SymLinks > 0)
    {
      UnicodeString SymLinksStr;
      SymLinksStr = (Stats.SymLinks == 1) ? FMTLOAD(PROPERTIES_SYMLINK, (Stats.SymLinks)) :
        FMTLOAD(PROPERTIES_SYMLINKS, (Stats.SymLinks));
      FilesStr = FORMAT(L"%s (%s)", (FilesStr, SymLinksStr));
    }
  }

  SizeLabel->Text = SizeStr;
  FileLabel->Text = FilesStr;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetFileProperties(const TRemoteProperties & value)
{
  TValidProperties Valid;
  if (value.Valid.Contains(vpRights) && (FLAGSET(FAllowedChanges, cpMode) || FLAGSET(FAllowedChanges, cpAcl)))
  {
    Valid << vpRights;
  }
  if (value.Valid.Contains(vpOwner) && FLAGSET(FAllowedChanges, cpOwner))
  {
    Valid << vpOwner;
  }
  if (value.Valid.Contains(vpGroup) && FLAGSET(FAllowedChanges, cpGroup))
  {
    Valid << vpGroup;
  }
  FOrigProperties = value;
  FOrigProperties.Valid = Valid;
  FOrigProperties.Recursive = false;

  bool HasRights = value.Valid.Contains(vpRights);
  RightsFrame->Visible = HasRights;
  RightsLabel->Visible = RightsFrame->Visible;
  if (HasRights)
  {
    RightsFrame->Rights = value.Rights;
    RightsFrame->AddXToDirectories = value.AddXToDirectories;
  }
  else
  {
    RightsFrame->Rights = TRights();
    RightsFrame->AddXToDirectories = false;
  }

  // Not necesarily true, let's find the scenario when it is not and then decide how to render that (shift group up?)
  DebugAssert(value.Valid.Contains(vpOwner) || !value.Valid.Contains(vpGroup));
  LoadRemoteToken(GroupComboBox, GroupView, GroupLabel, value.Valid.Contains(vpGroup), value.Group, cpGroup);
  LoadRemoteToken(OwnerComboBox, OwnerView, OwnerLabel, value.Valid.Contains(vpOwner), value.Owner, cpOwner);

  bool HasAnything = GroupLabel->Visible || OwnerLabel->Visible || HasRights;
  // Not necesarily true, let's find the scenario when it is not and then decide how to render that (shift rights up?)
  DebugAssert((GroupLabel->Visible || OwnerLabel->Visible) || !HasRights);
  GroupOwnerRightsBevel->Visible = HasAnything;

  RecursiveCheck2->Checked = value.Recursive;
  RecursiveCheck2->Visible =
    (GroupComboBox->Visible ||
     OwnerComboBox->Visible ||
     // Recursion is always supported for permissions and never for ACL.
     // If this ever changes we will have to introduce respective capability check.
     (HasRights && !FLAGSET(FAllowedChanges, cpAcl))) &&
    FAnyDirectories;
  RecursiveBevel->Visible = RecursiveCheck2->Visible || HasRights;

  if (TagsSheet->TabVisible)
  {
    TagsView->Clear();
    if (value.Valid.Contains(vpTags))
    {
      std::unique_ptr<TStrings> Tags(TextToStringList(value.Tags));
      for (int Index = 0; Index < Tags->Count; Index += 2)
      {
        AddTag(Tags->Strings[Index], Tags->Strings[Index + 1]);
      }
    }
    AutoSizeTagsView();
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::StoreRemoteToken(unsigned int ID,
  const UnicodeString & Text, const TRemoteTokenList * List, TRemoteToken & Result)
{
  DebugAssert(List != NULL);
  const TRemoteToken * Token = List->Find(ID);
  if (Token == NULL)
  {
    Result.ID = ID;
    Result.Name = Text;
  }
  else
  {
    Result = *Token;
  }
}
//---------------------------------------------------------------------------
TRemoteToken __fastcall TPropertiesDialog::StoreRemoteToken(const TRemoteToken & Orig,
  UnicodeString Text, int Message, const TRemoteTokenList * List)
{
  TRemoteToken Result;
  Text = Text.Trim();
  if (!Text.IsEmpty())
  {
    if (FLAGSET(FOptions, poUserGroupByID))
    {
      DebugAssert(List != NULL);
      int IDStart = Text.LastDelimiter(L"[");
      if (!Text.IsEmpty() && (IDStart >= 0) && (Text[Text.Length()] == L']'))
      {
        int ID;
        UnicodeString IDStr = Text.SubString(IDStart + 1, Text.Length() - IDStart - 1);
        if (!TryStrToInt(IDStr, ID))
        {
          throw Exception(Message);
        }
        else
        {
          StoreRemoteToken(ID, Text.SubString(1, IDStart - 1).Trim(), List, Result);
        }
      }
      else
      {
        const TRemoteToken * Token = List->Find(Text);
        if (Token == NULL)
        {
          int ID;
          if (TryStrToInt(Text, ID))
          {
            StoreRemoteToken(ID, Text, List, Result);
          }
          else
          {
            throw Exception(MainInstructions(FMTLOAD(PROPERTIES_UNKNOWN_TOKEN, (Text))));
          }
        }
        else
        {
          Result = *Token;
        }
      }
    }
    else
    {
      Result.Name = Text;
    }
  }

  if (LoadRemoteToken(Result) == LoadRemoteToken(Orig))
  {
    Result = Orig;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::StoreRemoteToken(TComboBox * ComboBox,
  int ChangeFlag, TValidProperty PropertyFlag, const TRemoteToken & Orig,
  TRemoteToken & Token, int Message, const TRemoteTokenList * List,
  TRemoteProperties & Properties)
{
  UnicodeString Text = ComboBox->Text.Trim();
  if (FLAGSET(FAllowedChanges, ChangeFlag))
  {
    Token = StoreRemoteToken(Orig, Text, Message, List);
    if (Token.IsSet)
    {
      Properties.Valid << PropertyFlag;
    }
  }
}
//---------------------------------------------------------------------------
TRemoteProperties __fastcall TPropertiesDialog::GetFileProperties()
{
  TRemoteProperties Result;

  if (FLAGSET(FAllowedChanges, cpMode) || FLAGSET(FAllowedChanges, cpAcl))
  {
    Result.Valid << vpRights;
    Result.Rights = RightsFrame->Rights;
    Result.AddXToDirectories = RightsFrame->AddXToDirectories;
  }

  StoreRemoteToken(GroupComboBox, cpGroup, vpGroup, FOrigProperties.Group,
    Result.Group, PROPERTIES_INVALID_GROUP, FGroupList, Result);
  StoreRemoteToken(OwnerComboBox, cpOwner, vpOwner, FOrigProperties.Owner,
    Result.Owner, PROPERTIES_INVALID_OWNER, FUserList, Result);

  Result.Recursive = RecursiveCheck2->Checked;

  if (TagsSheet->TabVisible)
  {
    std::unique_ptr<TStrings> Tags(new TStringList());
    TagsView->HandleNeeded(); // Count does not work otherwise
    for (int Index = 0; Index < TagsView->Items->Count; Index++)
    {
      TListItem * Item = TagsView->Items->Item[Index];
      Tags->Add(Item->Caption);
      Tags->Add(Item->SubItems->Strings[0]);
    }
    Result.Tags = Tags->Text;
    Result.Valid << vpTags;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ControlChange(TObject * /*Sender*/)
{
  if (Visible)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::UpdateControls()
{
  // No point enabling recursive check if there's no change allowed (supported),
  // i.e. with WebDAV.
  bool AnyAllowedChanges =
    FLAGSET(FAllowedChanges, cpGroup) || FLAGSET(FAllowedChanges, cpOwner) ||
    FLAGSET(FAllowedChanges, cpMode) || FLAGSET(FAllowedChanges, cpAcl);
  EnableControl(RecursiveCheck2, AnyAllowedChanges);

  bool Allow;
  try
  {
    Allow =
      !TRemoteProperties::ChangedProperties(FOrigProperties, GetFileProperties()).Valid.Empty() ||
      (RecursiveCheck2->Enabled && RecursiveCheck2->Checked);
  }
  catch(...)
  {
    // when properties are invalid allow submitting the form,
    // because that reveals the cause to the user, otherwise he/she
    // may not be able to tell what is wrong

    Allow = true;
  }
  EnableControl(OkButton, Allow);

  EnableControl(GroupComboBox, FLAGSET(FAllowedChanges, cpGroup));
  EnableControl(OwnerComboBox, FLAGSET(FAllowedChanges, cpOwner));
  EnableControl(RightsFrame, FLAGSET(FAllowedChanges, cpMode) || FLAGSET(FAllowedChanges, cpAcl));
  CalculateSizeButton->Visible = FAllowCalculateStats;

  if (!FMultiple)
  {
    // when setting properties for one file only, allow undef state
    // only when the input right explicitly requires it or
    // when "recursive" is on (possible for directory only).
    bool AllowUndef =
      (FOrigProperties.Valid.Contains(vpRights) &&
       FOrigProperties.Rights.AllowUndef) ||
      (RecursiveCheck2->Checked);
    if (!AllowUndef)
    {
      // when disallowing undef state, make sure, all undef are turned into unset
      RightsFrame->Rights = TRights(RightsFrame->Rights.NumberSet);
    }
    RightsFrame->AllowUndef = AllowUndef;
  }

  EnableControl(ChecksumSheet, ChecksumSupported());
  EnableControl(ChecksumButton, ChecksumSheet->Enabled &&
    !ChecksumAlgEdit->Text.IsEmpty());
  ChecksumEdit->Visible = !ChecksumEdit->Text.IsEmpty();
  ChecksumUnknownLabel->Visible = !ChecksumEdit->Visible;

  EnableControl(EditTagButton, (TagsView->ItemIndex >= 0));
  EnableControl(RemoveTagButton, (TagsView->ItemIndex >= 0));

  DefaultButton(ChecksumButton, ChecksumAlgEdit->Focused());
  DefaultButton(OkButton, !ChecksumAlgEdit->Focused());
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::FormCloseQuery(TObject * /*Sender*/,
      bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult())
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CalculateSizeButtonClick(
      TObject * /*Sender*/)
{
  DebugAssert(FOnCalculateSize != NULL);

  bool DoClose = false;
  Enabled = false;
  try
  {
    __int64 Size;
    TCalculateSizeStats Stats;
    FOnCalculateSize(FFileList, Size, Stats, DoClose);
    FStatsNotCalculated = false;
    LoadStats(Size, Stats);
  }
  __finally
  {
    Enabled = true;
    if (DoClose)
    {
      Close();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ResetChecksum()
{
  ChecksumView->Items->Clear();
  ChecksumEdit->Text = UnicodeString();
  AutoSizeListColumnsWidth(ChecksumView);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CalculateChecksum()
{
  DebugAssert(FOnCalculateChecksum != NULL);

  ResetChecksum();
  FChecksumLoaded = true;
  FAlgUsed = UnicodeString();

  bool DoClose = false;
  try
  {
    FOnCalculateChecksum(ChecksumAlgEdit->Text, FFileList, CalculatedChecksum, DoClose);
  }
  __finally
  {
    if (DoClose)
    {
      Close();
    }
  }

  // If we successfully used the selected checksum, remember it (in normalized form)
  if (!FAlgUsed.IsEmpty())
  {
    GUIConfiguration->ChecksumAlg = FAlgUsed;
  }

  AutoSizeListColumnsWidth(ChecksumView);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CalculatedChecksum(
  const UnicodeString & FileName, const UnicodeString & Alg,
  const UnicodeString & Hash)
{
  if (FMultipleChecksum)
  {
    TListItem * Item = ChecksumView->Items->Add();
    Item->Caption = FileName;
    Item->SubItems->Add(Hash);

    // optimization
    int TopIndex = ListView_GetTopIndex(ChecksumView->Handle);
    int Index = Item->Index;
    if ((TopIndex <= Index) &&
        (Index <= TopIndex + ChecksumView->VisibleRowCount))
    {
      AutoSizeListColumnsWidth(ChecksumView);
    }
  }
  else
  {
    ChecksumEdit->Text = Hash;
  }
  FAlgUsed = Alg;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::NeedChecksum()
{
  if (!FChecksumLoaded && ChecksumSupported())
  {
    CalculateChecksum();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TPropertiesDialog::ChecksumSupported()
{
  return (FOnCalculateChecksum != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ChecksumButtonClick(TObject * /*Sender*/)
{
  CalculateChecksum();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::PageControlChange(TObject * /*Sender*/)
{
  if (PageControl->ActivePage == ChecksumSheet)
  {
    NeedChecksum();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ChecksumAlgEditChange(TObject * /*Sender*/)
{
  ResetChecksum();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CopyClick(TObject * Sender)
{
  TInstantOperationVisualizer Visualizer;

  TListView * ListView = dynamic_cast<TListView *>(GetPopupComponent(Sender));
  DebugAssert(ListView != NULL);

  int Count = 0;
  UnicodeString SingleText;
  std::unique_ptr<TStrings> Lines(new TStringList());
  TListItem * Item = ListView->GetNextItem(NULL, sdAll, TItemStates() << isSelected);
  while (Item != NULL)
  {
    DebugAssert(Item->Selected);

    SingleText = Item->SubItems->Strings[0];
    UnicodeString Value = Item->SubItems->Strings[0];
    UnicodeString Entry = Item->Caption;
    if (!Value.IsEmpty())
    {
      Entry += FORMAT(L" = %s", (Value));
    }
    Lines->Add(Entry);
    Count++;

    Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
  }

  if ((ListView == ChecksumView) && (Count == 1))
  {
    CopyToClipboard(SingleText);
  }
  else
  {
    CopyToClipboard(Lines.get());
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ListViewContextPopup(
  TObject * Sender, TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ValidateRemoteToken(
  const TRemoteToken & Orig, int Message, TComboBox * ComboBox,
  const TRemoteTokenList * List)
{
  if (!IsCancelButtonBeingClicked(this))
  {
    try
    {
      ComboBox->Text =
        LoadRemoteToken(StoreRemoteToken(Orig, ComboBox->Text, Message, List));
    }
    catch(...)
    {
      ComboBox->SetFocus();
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::GroupComboBoxExit(TObject * Sender)
{
  ValidateRemoteToken(FOrigProperties.Group, PROPERTIES_INVALID_GROUP,
    dynamic_cast<TComboBox *>(Sender), FGroupList);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::OwnerComboBoxExit(TObject * Sender)
{
  ValidateRemoteToken(FOrigProperties.Owner, PROPERTIES_INVALID_OWNER,
    dynamic_cast<TComboBox *>(Sender), FUserList);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CMDpiChanged(TMessage & Message)
{
  TForm::Dispatch(&Message);
  if (!FMultiple)
  {
    UpdateFileImage();
  }

  // WORKAROUND: Mere presence of the RightsFrame breaks automatic layout on DPI change in some situation, fixing it manually.
  // (opening on a secondary display with 100%, while primary display has 150%)
  SizeLabel->Width = CalculateSizeButton->Left - ScaleByTextHeight(this, 8) - SizeLabel->Left;
  Bevel1->Width = CommonSheet->ClientWidth - (Bevel1->Left * 2);
  Bevel2->Width = Bevel1->Width;
  GroupOwnerRightsBevel->Width = Bevel1->Width;
  RecursiveBevel->Width = Bevel1->Width;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == CM_DPICHANGED)
  {
    CMDpiChanged(*M);
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::TagsViewKeyDown(TObject *, WORD & Key, TShiftState)
{
  if (RemoveTagButton->Enabled && (Key == VK_DELETE))
  {
    RemoveTagButton->OnClick(NULL);
  }

  if (AddTagButton->Enabled && (Key == VK_INSERT))
  {
    AddTagButton->OnClick(NULL);
  }
}
//---------------------------------------------------------------------------
TListItem * TPropertiesDialog::AddTag(const UnicodeString & Key, const UnicodeString & Value)
{
  TListItem * Item = TagsView->Items->Add();
  Item->Caption = Key;
  Item->SubItems->Add(Value);
  return Item;
}
//---------------------------------------------------------------------------
void TPropertiesDialog::AutoSizeTagsView()
{
  AutoSizeListColumnsWidth(TagsView, 1);
}
//---------------------------------------------------------------------------
void TPropertiesDialog::AddEditTag(bool Add)
{
  std::unique_ptr<TStrings> Tags(CreateSortedStringList(true));
  TListItem * ItemFocused = TagsView->ItemFocused;
  for (int Index = 0; Index < TagsView->Items->Count; Index++)
  {
    TListItem * Item = TagsView->Items->Item[Index];
    if (Add || (Item != ItemFocused))
    {
      Tags->Add(Item->Caption);
    }
  }

  UnicodeString Key, Value;
  if (!Add)
  {
    Key = ItemFocused->Caption;
    Value = ItemFocused->SubItems->Strings[0];
  }
  if (DoTagDialog(Add, Tags.get(), Key, Value))
  {
    if (Add)
    {
      TagsView->ItemFocused = AddTag(Key, Value);
      TagsView->ItemFocused->MakeVisible(false);
    }
    else
    {
      ItemFocused->Caption = Key;
      ItemFocused->SubItems->Strings[0] = Value;
    }
    AutoSizeTagsView();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::AddTagButtonClick(TObject *)
{
  AddEditTag(true);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::TagsViewSelectItem(TObject *, TListItem *, bool Selected)
{
  DebugUsedParam(Selected);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::EditTagButtonClick(TObject *)
{
  AddEditTag(false);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::RemoveTagButtonClick(TObject *)
{
  int Index = TagsView->ItemIndex;

  TagsView->ItemFocused->Delete();

  int Count = TagsView->Items->Count;
  TagsView->ItemIndex = (Index < Count ? Index : Count - 1);
  AutoSizeTagsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::TagsViewDblClick(TObject *)
{
  if (EditTagButton->Enabled)
  {
    EditTagButton->OnClick(NULL);
  }
}
//---------------------------------------------------------------------------
