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
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma link "Rights"
#pragma link "RightsExt"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPropertiesDialog(TStrings * FileList,
	const AnsiString Directory, TStrings * GroupList, TStrings * UserList,
	TRemoteProperties * Properties, int AllowedChanges,
  TTerminal * Terminal)
{
	bool Result;
  TPropertiesDialog * PropertiesDialog = new TPropertiesDialog(Application);
  try
  {
    PropertiesDialog->AllowedChanges = AllowedChanges;
    PropertiesDialog->Directory = Directory;
    PropertiesDialog->FileList = FileList;
    PropertiesDialog->GroupList = GroupList;
    PropertiesDialog->UserList = UserList;
    PropertiesDialog->FileProperties = *Properties;
    PropertiesDialog->Terminal = Terminal;
    
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
  FAllowCalculateStats = false;
  FStatsNotCalculated = false;

  UseSystemSettings(this);
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
bool __fastcall TPropertiesDialog::Execute()
{
  bool Result;

  FPrevTerminalClose = NULL;;
  if (FTerminal)
  {
    FPrevTerminalClose = FTerminal->OnClose;
    // used instead of previous TTerminalManager::OnChangeTerminal
    FTerminal->OnClose = TerminalClose;
  }
  
  try
  {
    if (AllowedChanges & cpGroup) ActiveControl = GroupComboBox;
      else
    if (AllowedChanges & cpOwner) ActiveControl = OwnerComboBox;
      else
    if (AllowedChanges & cpMode) ActiveControl = RightsFrame;
      else ActiveControl = OkButton;
    UpdateControls();
    
    Result = (ShowModal() == mrOk);
  }
  __finally
  {
    if (FTerminal)
    {
      assert(FTerminal->OnClose == TerminalClose);
      FTerminal->OnClose = FPrevTerminalClose;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::TerminalClose(TObject * Sender)
{
  Close();
  Terminal = NULL;
  if (FPrevTerminalClose)
  {
    FPrevTerminalClose(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetFileList(TStrings * value)
{
  if (FFileList != value)
  {
    FFileList->Assign(value);
    LoadInfo();
    FGroupsSet = false;
    FUsersSet = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadInfo()
{
  if (FFileList)
  {
    assert(FFileList->Count > 0);
    FAllowCalculateStats = false;
    FStatsNotCalculated = false;
    FileIconImage->Picture->Bitmap = NULL;
    __int64 FilesSize;
    TCalculateSizeStats Stats;

    RightsFrame->AllowUndef = Multiple;

    if (!Multiple)
    {
      TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);
      assert(File && FShellImageList);
      FShellImageList->GetIcon(File->IconIndex, FileIconImage->Picture->Icon);
      if (!FUsersSet)
      {
        OwnerComboBox->Items->Text = File->Owner;
      }
      if (!FGroupsSet)
      {
        GroupComboBox->Items->Text = File->Group;
      }
      FilesSize = File->Size;

      LinksToLabelLabel->Visible = File->IsSymLink;
      LinksToLabel->Visible = File->IsSymLink;
      if (File->IsSymLink)
      {
        LinksToLabel->Caption = File->LinkTo;
      }
      if (File->IsDirectory && !File->IsSymLink)
      {
        FAllowCalculateStats = true;
        FStatsNotCalculated = true;
      }

      RightsFrame->AllowAddXToDirectories = File->IsDirectory;
      Caption = FMTLOAD(PROPERTIES_FILE_CAPTION, (File->FileName));
      RecursiveCheck->Visible = File->IsDirectory;
      RecursiveBevel->Visible = File->IsDirectory;
    }
    else
    {
      Caption = FFileList->Count ?
        FMTLOAD(PROPERTIES_FILES_CAPTION, (FFileList->Strings[0])) : AnsiString();
      LinksToLabelLabel->Hide();
      LinksToLabel->Hide();

      TStrings *GroupList = new TStringList();
      ((TStringList*)GroupList)->Duplicates = dupIgnore;
      ((TStringList*)GroupList)->Sorted = True;
      TStrings *OwnerList = new TStringList();
      ((TStringList*)OwnerList)->Duplicates = dupIgnore;
      ((TStringList*)OwnerList)->Sorted = True;

      try
      {
        FilesSize = 0;

        for (int Index = 0; Index < FFileList->Count; Index++)
        {
          TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[Index]);
          assert(File);
          if (!File->Group.IsEmpty())
          {
            GroupList->Add(File->Group);
          }
          if (!File->Owner.IsEmpty())
          {
            OwnerList->Add(File->Owner);
          }
          if (File->IsDirectory)
          {
            Stats.Directories++;
            if (!File->IsSymLink)
            {
              FAllowCalculateStats = true;
              FStatsNotCalculated = true;
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

        if (!FUsersSet)
        {
          OwnerComboBox->Items = OwnerList;
        }
        if (!FGroupsSet)
        {
          GroupComboBox->Items = GroupList;
        }
        RightsFrame->AllowAddXToDirectories = (Stats.Directories > 0);
        RecursiveCheck->Visible = (Stats.Directories > 0);
        RecursiveBevel->Visible = (Stats.Directories > 0);

      }
      __finally
      {
        delete GroupList;
        delete OwnerList;
      }
    }

    LoadStats(FilesSize, Stats);

    FilesIconImage->Visible = Multiple;
    FileIconImage->Visible = !Multiple;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::LoadStats(__int64 FilesSize,
  const TCalculateSizeStats & Stats)
{
  AnsiString SizeStr;
  AnsiString FilesStr;
  if (FStatsNotCalculated)
  {
    SizeStr = LoadStr(PROPERTIES_UNKNOWN_SIZE);
  }
  else
  {
    SizeStr = FormatBytes(FilesSize);
    AnsiString SizeUnorderedStr = FormatBytes(FilesSize, false);
    if (SizeStr != SizeUnorderedStr)
    {
      SizeStr = FORMAT("%s (%s)", (SizeStr, SizeUnorderedStr)); 
    }
  }

  if (((Stats.Files + Stats.Directories) == 0) && !Multiple)
  {
    TRemoteFile * File = (TRemoteFile *)(FFileList->Objects[0]);
    assert(File != NULL);
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
        FilesStr = FORMAT("%s, ", (FilesStr));
      }
    }
    if (Stats.Directories > 0)
    {
      FilesStr += (Stats.Directories == 1) ? FMTLOAD(PROPERTIES_DIRECTORY, (Stats.Directories)) :
        FMTLOAD(PROPERTIES_DIRECTORIES, (Stats.Directories));
    }
    if (Stats.SymLinks > 0)
    {
      AnsiString SymLinksStr;
      SymLinksStr = (Stats.SymLinks == 1) ? FMTLOAD(PROPERTIES_SYMLINK, (Stats.SymLinks)) :
        FMTLOAD(PROPERTIES_SYMLINKS, (Stats.SymLinks));
      FilesStr = FORMAT("%s (%s)", (FilesStr, SymLinksStr));
    }
  }

  SizeLabel->Caption = SizeStr;
  FileLabel->Caption = FilesStr;
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
  GroupComboBox->Text = value.Valid.Contains(vpGroup) ? value.Group : AnsiString();
  OwnerComboBox->Text = value.Valid.Contains(vpOwner) ? value.Owner : AnsiString();
  RecursiveCheck->Checked = value.Recursive;
  UpdateControls();
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
    if (!PROPERTY ## ComboBox->Text.Trim().IsEmpty() && \
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
    (!GroupComboBox->Text.Trim().IsEmpty() ||
     (Multiple && !FOrigProperties.Valid.Contains(vpGroup)) ||
     (FOrigProperties.Group == GroupComboBox->Text)) &&
    // same but with owner
    (!OwnerComboBox->Text.Trim().IsEmpty() ||
     (Multiple && !FOrigProperties.Valid.Contains(vpOwner)) ||
     (FOrigProperties.Owner == OwnerComboBox->Text)) &&
    ((FileProperties != FOrigProperties) || RecursiveCheck->Checked)
  );
  EnableControl(GroupComboBox, FAllowedChanges & cpGroup);
  EnableControl(OwnerComboBox, FAllowedChanges & cpOwner);
  EnableControl(RightsFrame, FAllowedChanges & cpMode);
  CalculateSizeButton->Visible = Terminal && FAllowCalculateStats;

  if (!Multiple)
  {
    // when setting properties for one file only, allow undef state
    // only when the input right explicitly requires it or
    // when "recursive" is on (possible for directory only).
    bool AllowUndef =
      (FOrigProperties.Valid.Contains(vpRights) &&
       FOrigProperties.Rights.AllowUndef) ||
      (RecursiveCheck->Checked);
    if (!AllowUndef)
    {
      // when disallowing undef state, make sure, all undef are turned into unset
      RightsFrame->Rights = TRights(RightsFrame->Rights.NumberSet);
    }
    RightsFrame->AllowUndef = AllowUndef;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TPropertiesDialog::GetMultiple()
{
  return (FFileList->Count != 1);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetGroupList(TStrings * value)
{
  if (FGroupsSet || ((value != NULL) && (value->Count > 0)))
  {
    GroupComboBox->Items = value;
    FGroupsSet = true;
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TPropertiesDialog::GetGroupList()
{
  return GroupComboBox->Items;
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::SetUserList(TStrings * value)
{
  if (FUsersSet || ((value != NULL) && (value->Count > 0)))
  {
    OwnerComboBox->Items = value;
    FUsersSet = true;
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TPropertiesDialog::GetUserList()
{
  return OwnerComboBox->Items;
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
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::CalculateSizeButtonClick(
      TObject * /*Sender*/)
{
  assert(Terminal);
  __int64 Size;
  TCalculateSizeStats Stats;
  Terminal->CalculateFilesSize(FileList, Size, 0, NULL, &Stats);
  FStatsNotCalculated = false;
  LoadStats(Size, Stats);
}
//---------------------------------------------------------------------------
void __fastcall TPropertiesDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

