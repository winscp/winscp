//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Properties.h"

#include <AssociatedStatusBar.hpp> // FormatBytes()

#include <VCLCommon.h>
#include <Common.h>
#include <TextsWin.h>

#include "WinInterface.h"
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma link "Rights"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPropertiesDialog(TStrings * FileList,
	const AnsiString Directory, TStrings * GroupList,
	TRemoteProperties * Properties, int AllowedChanges)
{
	bool Result;
  TPropertiesDialog * PropertiesDialog = new TPropertiesDialog(Application);
  try
  {
    PropertiesDialog->AllowedChanges = AllowedChanges;
    PropertiesDialog->Directory = Directory;
    PropertiesDialog->FileList = FileList;
    PropertiesDialog->GroupList = GroupList;
    PropertiesDialog->FileProperties = *Properties;
    Result = PropertiesDialog->Execute();
    if (Result)
    {
      *Properties = PropertiesDialog->FileProperties;
    }
  }
  __finally
  {
  	delete PropertiesDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPropertiesDialog::TPropertiesDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  RightsFrame->OnChange = ControlChange;

  FGroupsSet = False;

  TSHFileInfo FileInfo;
  FShellImageList = new TImageList(this);
  FShellImageList->Handle = SHGetFileInfo("", 0, &FileInfo, sizeof(FileInfo),
      SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
  FShellImageList->ShareImages = True;

  FFileList = new TStringList();

  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TPropertiesDialog::~TPropertiesDialog()
{
  delete FFileList;
  FFileList = NULL;
  delete FShellImageList;
  FShellImageList = NULL;
}
//---------------------------------------------------------------------
Boolean __fastcall TPropertiesDialog::Execute()
{
  if (AllowedChanges & cpGroup) ActiveControl = GroupComboBox;
    else
  if (AllowedChanges & cpOwner) ActiveControl = OwnerComboBox;
    else
  if (AllowedChanges & cpMode) ActiveControl = RightsFrame;
    else ActiveControl = OkButton; 
  UpdateControls();
  return (ShowModal() == mrOk);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetFileList(TStrings * value)
{
  if (FFileList != value)
  {
    FFileList->Assign(value);
    LoadInfo();
    FGroupsSet = False;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadInfo()
{
  if (FFileList)
  {
    assert(FFileList > 0);
    Boolean Multiple = (FFileList->Count != 1);
    FileIconImage->Picture->Bitmap = NULL;
    __int64 FilesSize;

    RightsFrame->AllowUndef = Multiple;

    if (!Multiple)
    {
      TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);
      assert(File && FShellImageList);
      FShellImageList->GetIcon(File->IconIndex, FileIconImage->Picture->Icon);
      FileLabel->Caption = File->FileName;
      OwnerComboBox->Items->Text = File->Owner;
      if (!FGroupsSet) GroupComboBox->Items->Text = File->Owner;
      FilesSize = File->Size;

      LinksToLabelLabel->Visible = File->IsSymLink;
      LinksToLabel->Visible = File->IsSymLink;
      if (File->IsSymLink) LinksToLabel->Caption = File->LinkTo;

      RightsFrame->AllowAddXToDirectories = File->IsDirectory;
      Caption = FMTLOAD(PROPERTIES_FILE_CAPTION, (File->FileName));
      RecursiveCheck->Visible = File->IsDirectory;
      RecursiveBevel->Visible = File->IsDirectory;
    }
      else
    {
      if (FFileList->Count) Caption = FMTLOAD(PROPERTIES_FILES_CAPTION, (FFileList->Strings[0]));
        else Caption = "";
      LinksToLabelLabel->Hide();
      LinksToLabel->Hide();

      TStrings *GroupList = new TStringList();
      ((TStringList*)GroupList)->Duplicates = dupIgnore;
      ((TStringList*)GroupList)->Sorted = True;
      TStrings *OwnerList = new TStringList();
      ((TStringList*)OwnerList)->Duplicates = dupIgnore;
      ((TStringList*)OwnerList)->Sorted = True;

      try {

        Integer Directories = 0;
        Integer Files = 0;
        Integer SymLinks = 0;
        FilesSize = 0;

        for (Integer Index = 0; Index < FFileList->Count; Index++)
        {
          TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[Index]);
          assert(File);
          GroupList->Add(File->Group);
          OwnerList->Add(File->Owner);
          if (File->IsDirectory) Directories++;
            else Files++;
          if (File->IsSymLink) SymLinks++;
          FilesSize += File->Size;
        }

        AnsiString FilesStr = "";
        if (Files)
        {
          if (Files == 1) FilesStr = FMTLOAD(PROPERTIES_FILE, (Files));
            else FilesStr = FMTLOAD(PROPERTIES_FILES, (Files));
          if (Directories)
            FilesStr = FORMAT("%s, ", (FilesStr));
        }
        if (Directories)
        {
          if (Directories == 1) FilesStr += FMTLOAD(PROPERTIES_DIRECTORY, (Directories));
            else FilesStr += FMTLOAD(PROPERTIES_DIRECTORIES, (Directories));
        }
        if (SymLinks)
        {
          AnsiString SymLinksStr;
          if (SymLinks == 1) SymLinksStr = FMTLOAD(PROPERTIES_SYMLINK, (SymLinks));
            else SymLinksStr = FMTLOAD(PROPERTIES_SYMLINKS, (SymLinks));
          FilesStr = FORMAT("%s (%s)", (FilesStr, SymLinksStr));
        }
        FileLabel->Caption = FilesStr;

        OwnerComboBox->Items = OwnerList;
        if (!FGroupsSet) GroupComboBox->Items = GroupList;
        RightsFrame->AllowAddXToDirectories = (Directories > 0);
        RecursiveCheck->Visible = (Directories > 0);
        RecursiveBevel->Visible = (Directories > 0);

      } __finally {
        delete GroupList;
        delete OwnerList;
      }
    }

    AnsiString SizeStr = FormatBytes(FilesSize);
    if (FilesSize >= FormatBytesAbove)
    {
      __int64 PrevFormatBytesAbove = FormatBytesAbove;
      FormatBytesAbove = FilesSize + 1;
      try {
        SizeStr = FORMAT("%s (%s)", (SizeStr, FormatBytes(FilesSize)));
      } __finally {
        FormatBytesAbove = PrevFormatBytesAbove;
      }
    }
    SizeLabel->Caption = SizeStr;

    FilesIconImage->Visible = Multiple;
    FileIconImage->Visible = !Multiple;
  }
}

//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetDirectory(AnsiString value)
{
  LocationLabel->Caption = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TPropertiesDialog::GetDirectory()
{
  return LocationLabel->Caption;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetFileProperties(TRemoteProperties value)
{
  TValidProperties Valid;
  if (value.Valid.Contains(vpRights) && FAllowedChanges & cpMode) Valid << vpRights;
  if (value.Valid.Contains(vpOwner) && FAllowedChanges & cpOwner) Valid << vpOwner;
  if (value.Valid.Contains(vpGroup) && FAllowedChanges & cpGroup) Valid << vpGroup;
  FOrigProperties = value;
  FOrigProperties.Valid = Valid;
  FOrigProperties.Recursive = false;
  
  if (value.Valid.Contains(vpRights))
  {
    RightsFrame->Rights = value.Rights;
    RightsFrame->AddXToDirectories = value.AddXToDirectories;
  }
    else
  {
    RightsFrame->Rights = TRights();
    RightsFrame->AddXToDirectories = false;
  }
  if (value.Valid.Contains(vpGroup)) GroupComboBox->Text = value.Group;
    else GroupComboBox->Text = "";
  if (value.Valid.Contains(vpOwner)) OwnerComboBox->Text = value.Owner;
    else OwnerComboBox->Text = "";
  RecursiveCheck->Checked = value.Recursive;
}
//---------------------------------------------------------------------------
TRemoteProperties __fastcall TPropertiesDialog::GetFileProperties()
{
  TRemoteProperties Result;

  if (AllowedChanges & cpMode)
  {
    Result.Valid << vpRights;
    Result.Rights = RightsFrame->Rights;
    Result.AddXToDirectories = RightsFrame->AddXToDirectories;
  }

  #define STORE_NAME(PROPERTY) \
    if (!PROPERTY ## ComboBox->Text.IsEmpty() && \
        AllowedChanges & cp ## PROPERTY) \
    { \
      Result.Valid << vp ## PROPERTY; \
      Result.PROPERTY = PROPERTY ## ComboBox->Text.Trim(); \
    }
  STORE_NAME(Group);
  STORE_NAME(Owner);
  #undef STORE_NAME

  Result.Recursive = RecursiveCheck->Checked;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::UpdateControls()
{
  EnableControl(OkButton,
    // group name is specified or we set multiple-file properties and
    // no valid group was specified (there are at least two different groups)
    (!GroupComboBox->Text.IsEmpty() ||
     (Multiple && !FOrigProperties.Valid.Contains(vpGroup)) ||
     (FOrigProperties.Group == GroupComboBox->Text)) &&
    // same but with owner
    (!OwnerComboBox->Text.IsEmpty() ||
     (Multiple && !FOrigProperties.Valid.Contains(vpOwner)) ||
     (FOrigProperties.Owner == OwnerComboBox->Text)) &&
    ((FileProperties != FOrigProperties) || RecursiveCheck->Checked)
  );
  EnableControl(GroupComboBox, FAllowedChanges & cpGroup);
  EnableControl(OwnerComboBox, FAllowedChanges & cpOwner);
  EnableControl(RightsFrame, FAllowedChanges & cpMode);
}
//---------------------------------------------------------------------------
Boolean __fastcall TPropertiesDialog::GetMultiple()
{
  return (FFileList->Count != 1);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetGroupList(TStrings * value)
{
  GroupComboBox->Items = value;
  FGroupsSet = True;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TPropertiesDialog::GetGroupList()
{
  return GroupComboBox->Items;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::FormCloseQuery(TObject * /*Sender*/,
      bool & /*CanClose*/)
{
  if (ModalResult == mrOk)
  {
    #define CHECK_VALID_NAME(Property, Message) \
      if (FOrigProperties.Valid.Contains(vp ## Property) && Property ## ComboBox->Text.Trim().IsEmpty()) { \
        Property ## ComboBox->SetFocus(); throw Exception(Message); }
    CHECK_VALID_NAME(Group, PROPERTIES_INVALID_GROUP);
    CHECK_VALID_NAME(Owner, PROPERTIES_INVALID_OWNER);
    #undef CHECK_VALID_NAME
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetAllowedChanges(int value)
{
  if (FAllowedChanges != value)
  {
    FAllowedChanges = value;
    UpdateControls();
  }
}

