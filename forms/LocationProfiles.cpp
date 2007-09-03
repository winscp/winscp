//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <CoreMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <Common.h>

#include "LocationProfiles.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "IEComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall LocationProfilesDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TStrings * LocalDirectories, TStrings * RemoteDirectories, TTerminal * Terminal)
{
  bool Result;
  TLocationProfilesDialog * Dialog = new TLocationProfilesDialog(Application);
  try
  {
    Dialog->LocalDirectory = LocalDirectory;
    Dialog->RemoteDirectory = RemoteDirectory;
    Dialog->OperationSide = Side;
    Dialog->Terminal = Terminal;
    Dialog->RemoteDirectories = RemoteDirectories;
    Dialog->LocalDirectories = LocalDirectories;
    Dialog->Mode = Mode;

    Result = Dialog->Execute();
    if (Result)
    {
      LocalDirectory = Dialog->LocalDirectory;
      RemoteDirectory = Dialog->RemoteDirectory;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TLocationProfilesDialog::TLocationProfilesDialog(TComponent * AOwner):
  TForm(AOwner)
{
  FOperationSide = osLocal;
  FBookmarkDragSource = NULL;
  FTerminal = NULL;
  FBookmarkList = new TBookmarkList();
  FChanging = false;

  FFolders = new TStringList;
  FFolders->CaseSensitive = false;
  FFolders->Sorted = true;
  FFolders->Duplicates = dupIgnore;

  UseSystemSettings(this);

  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
}
//---------------------------------------------------------------------
__fastcall TLocationProfilesDialog::~TLocationProfilesDialog()
{
  SAFE_DESTROY(FBookmarkList);
  SAFE_DESTROY(FFolders);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::SetLocalDirectory(AnsiString value)
{
  if (LocalDirectory != value)
  {
    LocalDirectoryEdit->Text = value;
    FindProfile();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLocationProfilesDialog::GetLocalDirectory()
{
  return ExcludeTrailingBackslash(LocalDirectoryEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::SetRemoteDirectory(AnsiString value)
{
  if (RemoteDirectory != value)
  {
    RemoteDirectoryEdit->Text = value;
    FindProfile();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLocationProfilesDialog::GetRemoteDirectory()
{
  return UnixExcludeTrailingBackslash(RemoteDirectoryEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::SetRemoteDirectories(TStrings * value)
{
  RemoteDirectoryEdit->Items = value;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TLocationProfilesDialog::GetRemoteDirectories()
{
  return RemoteDirectoryEdit->Items;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::SetLocalDirectories(TStrings * value)
{
  LocalDirectoryEdit->Items = value;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TLocationProfilesDialog::GetLocalDirectories()
{
  return LocalDirectoryEdit->Items;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::FindProfile()
{
  TTreeNode * Match = NULL;
  for (int Index = 0; Index < ProfilesView->Items->Count; Index++)
  {
    TTreeNode * Node = ProfilesView->Items->Item[Index];
    if (Node->Data)
    {
      TBookmark * Bookmark = (TBookmark *)Node->Data;
      if (Bookmark->Local == LocalDirectory &&
          Bookmark->Remote == RemoteDirectory)
      {
        Match = Node;
        break;
      }
    }
  }

  if (Match)
  {
    ProfilesView->Selected = Match;
    Match->MakeVisible();
  }
  else
  {
    ProfilesView->Selected = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::UpdateControls()
{
  EnableControl(OKBtn, !LocalDirectory.IsEmpty() || !RemoteDirectory.IsEmpty());
  EnableControl(AddBookmarkButton,
    !LocalDirectory.IsEmpty() || !RemoteDirectory.IsEmpty());
  EnableControl(RemoveBookmarkButton, ProfilesView->Selected);
  EnableControl(RenameButton, ProfilesView->Selected);
  EnableControl(MoveToButton, ProfilesView->Selected && ProfilesView->Selected->Data);
  EnableControl(UpBookmarkButton, ProfilesView->Selected &&
    ProfilesView->Selected->Data && ProfilesView->Selected->getPrevSibling() &&
    ProfilesView->Selected->getPrevSibling()->Data);
  EnableControl(DownBookmarkButton, ProfilesView->Selected &&
    ProfilesView->Selected->Data && ProfilesView->Selected->getNextSibling() &&
    ProfilesView->Selected->getNextSibling()->Data);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::LoadBookmarks()
{
  assert(FBookmarkList);

  FFolders->Clear();
  for (int Index = 0; Index < FBookmarkList->Count; Index++)
  {
    TBookmark * Bookmark = FBookmarkList->Bookmarks[Index];
    if (!Bookmark->Node.IsEmpty())
    {
      FFolders->Add(Bookmark->Node);
    }
  }

  ProfilesView->Items->Clear();

  for (int Index = 0; Index < FFolders->Count; Index++)
  {
    FFolders->Objects[Index] = ProfilesView->Items->Add(NULL, FFolders->Strings[Index]);
  }

  for (int Index = 0; Index < FBookmarkList->Count; Index++)
  {
    TBookmark * Bookmark = FBookmarkList->Bookmarks[Index];
    TTreeNode * Parent = NULL;
    if (!Bookmark->Node.IsEmpty())
    {
      assert(FFolders->IndexOf(Bookmark->Node) >= 0);
      Parent = dynamic_cast<TTreeNode *>(FFolders->Objects[FFolders->IndexOf(Bookmark->Node)]);
    }
    ProfilesView->Items->AddChildObject(Parent, Bookmark->Name, Bookmark);
    if ((Parent != NULL) && (Parent->Count == 1))
    {
      // only now, when folder node has its first child, we can eventually expand it
      Parent->Expanded = FBookmarkList->NodeOpened[Parent->Text];
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLocationProfilesDialog::Execute()
{
  bool Result;
  AnsiString SessionKey;
  if (Terminal)
  {
    TBookmarkList * BookmarkList;
    // cache session key, in case terminal is closed while the window is open
    SessionKey = Terminal->SessionData->SessionKey;
    BookmarkList = WinConfiguration->Bookmarks[SessionKey];
    if (BookmarkList)
    {
      FBookmarkList->Assign(BookmarkList);
    }
    else
    {
      FBookmarkList->Clear();
    }
    LoadBookmarks();
  }
  Result = ((Mode != odAddBookmark) || AddAsBookmark()) && (ShowModal() == mrOk);
  if (Terminal)
  {
    WinConfiguration->Bookmarks[SessionKey] = FBookmarkList;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TLocationProfilesDialog::AddAsBookmark()
{
  assert(!LocalDirectory.IsEmpty() || !RemoteDirectory.IsEmpty());

  bool Result;
  AnsiString BookmarkName;
  if ((OperationSide == osLocal && !LocalDirectory.IsEmpty()) ||
      RemoteDirectory.IsEmpty())
  {
    BookmarkName = LocalDirectory;
  }
  else
  {
    BookmarkName = RemoteDirectory;
  }

  TTreeNode * Selected = ProfilesView->Selected;
  TBookmark * SelectedBookmark = NULL;
  AnsiString SelectedNode;
  if (Selected != NULL)
  {
    SelectedBookmark = (TBookmark *)Selected->Data;
    if (SelectedBookmark != NULL)
    {
      SelectedNode = SelectedBookmark->Node;
    }
    else
    {
      SelectedNode = Selected->Text;
    }
  }

  TStrings * PeerBookmarks = new TStringList();
  try
  {
    for (int Index = 0; Index < FBookmarkList->Count; Index++)
    {
      TBookmark * Bookmark = FBookmarkList->Bookmarks[Index];
      if (Bookmark->Node == SelectedNode)
      {
        PeerBookmarks->Add(Bookmark->Name);
      }
    }

    Result = DoComboInputDialog(LoadStr(ADD_BOOKMARK_CAPTION), LoadStr(ADD_BOOKMARK_PROMPT),
      BookmarkName, PeerBookmarks, NULL, false, HELP_LOCATION_PROFILE_ADD);
    if (Result)
    {
      if (BookmarkName.IsEmpty() || (StrToIntDef(BookmarkName, -123) != -123))
      {
        throw Exception(FMTLOAD(BOOKMARK_INVALID_NAME, (BookmarkName)));
      }

      TBookmark * Bookmark = FBookmarkList->FindByName(SelectedNode, BookmarkName);
      if (Bookmark != NULL)
      {
        Bookmark->Local = LocalDirectory;
        Bookmark->Remote = RemoteDirectory;

        for (int Index = 0; Index < ProfilesView->Items->Count; Index++)
        {
          TTreeNode * Node = ProfilesView->Items->Item[Index];
          if (Node->Data == Bookmark)
          {
            Selected = Node;
            break;
          }
        }
      }
      else
      {
        Bookmark = new TBookmark();
        Bookmark->Name = BookmarkName;
        Bookmark->Local = LocalDirectory;
        Bookmark->Remote = RemoteDirectory;
        if (SelectedBookmark != NULL)
        {
          Bookmark->Node = SelectedBookmark->Node;
          FBookmarkList->InsertBefore(SelectedBookmark, Bookmark);
          Selected = ProfilesView->Items->InsertObject(Selected, Bookmark->Name, Bookmark);
        }
        else if ((Selected != NULL) && (SelectedBookmark == NULL))
        {
          // must be a folder
          assert(!Selected->Parent); // more than one level of folders is not supported
          Bookmark->Node = Selected->Text;
          FBookmarkList->Add(Bookmark);
          Selected = ProfilesView->Items->AddChildObject(Selected, Bookmark->Name, Bookmark);
        }
        else
        {
          FBookmarkList->Add(Bookmark);
          Selected = ProfilesView->Items->AddObject(NULL, Bookmark->Name, Bookmark);
        }
      }
      ProfilesView->Selected = Selected;
    }
  }
  __finally
  {
    delete PeerBookmarks;
  }

  UpdateControls();
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::AddBookmarkButtonClick(TObject */*Sender*/)
{
  AddAsBookmark();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::RemoveBookmarkButtonClick(TObject * /*Sender*/)
{
  assert(ProfilesView->Selected);
  TTreeNode * Node = ProfilesView->Selected;
  if (Node->Data)
  {
    FBookmarkList->Delete((TBookmark *)Node->Data);
    TTreeNode * ParentNode = Node->Parent;
    Node->Delete();
    if (ParentNode && !ParentNode->Count)
    {
      assert(FFolders->IndexOfObject(ParentNode) >= 0);
      FFolders->Delete(FFolders->IndexOfObject(ParentNode));
      ParentNode->Delete();
    }
  }
  else
  {
    if (MessageDialog(LoadStr(DELETE_BOOKMARK_FOLDER), qtConfirmation,
          qaYes | qaNo, HELP_LOCATION_PROFILE_DELETE) == qaYes)
    {
      assert(Node->Count);
      for (int i = 0; i < Node->Count; i++)
      {
        FBookmarkList->Delete((TBookmark *)Node->Item[i]->Data);
      }
      assert(FFolders->IndexOfObject(Node) >= 0);
      FFolders->Delete(FFolders->IndexOfObject(Node));
      Node->Delete();
    }
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::BookmarkMove(
  TTreeNode * Source, TTreeNode * Dest)
{
  assert(Source && Source->Data);

  TBookmark * Bookmark = (TBookmark *)Source->Data;
  TTreeNode * PrevFolderNode = Source->Parent;

  if (!Dest || !Dest->Data)
  {
    Bookmark->Node = Dest ? Dest->Text : AnsiString("");
    FBookmarkList->MoveTo(FBookmarkList->Bookmarks[FBookmarkList->Count - 1],
      Bookmark, false);
    ProfilesView->Selected->MoveTo(Dest, naAddChild);
  }
  else
  {
    TBookmark * DestBookmark = (TBookmark *)Dest->Data;

    Bookmark->Node = DestBookmark->Node;
    FBookmarkList->MoveTo(DestBookmark, Bookmark,
      Source->AbsoluteIndex > Dest->AbsoluteIndex);
    if (Source->AbsoluteIndex > Dest->AbsoluteIndex)
    {
      Source->MoveTo(Dest, naInsert);
    }
    else if (Dest->getNextSibling() != NULL)
    {
      Source->MoveTo(Dest->getNextSibling(), naInsert);
    }
    else
    {
      Source->MoveTo(Dest, naAdd);
    }
  }

  if (PrevFolderNode && !PrevFolderNode->Count)
  {
    assert(FFolders->IndexOfObject(PrevFolderNode) >= 0);
    FFolders->Delete(FFolders->IndexOfObject(PrevFolderNode));
    PrevFolderNode->Delete();
  }

  Source->MakeVisible();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::BookmarkButtonClick(TObject *Sender)
{
  TTreeNode * Node = ProfilesView->Selected;
  assert(Node);
  assert(Node->Data);

  TTreeNode * TargetNode;
  if (Sender == UpBookmarkButton)
  {
    TargetNode = Node->getPrevSibling();
    assert(TargetNode);
  }
  else
  {
    TargetNode = Node->getNextSibling();
  }

  BookmarkMove(Node, TargetNode ? TargetNode : Node->Parent);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewStartDrag(
      TObject * /*Sender*/ , TDragObject *& /*DragObject*/)
{
  if (!ProfilesView->Selected->Data)
  {
    Abort();
  }
  FBookmarkDragSource = ProfilesView->Selected;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewDragOver(
    TObject */*Sender*/, TObject *Source, int /*X*/, int /*Y*/,
    TDragState /*State*/, bool &Accept)
{
  if (Source == ProfilesView)
  {
    Accept = (ProfilesView->DropTarget != NULL) &&
      (FBookmarkDragSource != ProfilesView->DropTarget);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewDragDrop(
      TObject * /*Sender*/, TObject * Source, int /*X*/, int /*Y*/)
{
  if ((Source == ProfilesView) && (ProfilesView->DropTarget != NULL) &&
      (FBookmarkDragSource != ProfilesView->DropTarget))
  {
    assert(FBookmarkDragSource);

    TTreeNode * Target = ProfilesView->DropTarget;
    BookmarkMove(FBookmarkDragSource, Target);
    FBookmarkDragSource = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewDblClick(TObject * /*Sender*/)
{
  TPoint P = ProfilesView->ScreenToClient(Mouse->CursorPos);
  TTreeNode * Node = ProfilesView->GetNodeAt(P.x, P.y);
  if (OKBtn->Enabled && Node && Node->Data && Node->Selected)
  {
    ModalResult = mrOk;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::FormShow(TObject * /*Sender*/)
{
  FindProfile();
  if (OperationSide == osLocal)
  {
    LocalDirectoryEdit->SetFocus();
  }
  else
  {
    RemoteDirectoryEdit->SetFocus();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (!ProfilesView->IsEditing())
  {
    if (RemoveBookmarkButton->Enabled && (Key == VK_DELETE))
    {
      RemoveBookmarkButtonClick(NULL);
      Key = 0;
    }
    else if (RenameButton->Enabled && (Key == VK_F2))
    {
      RenameButtonClick(NULL);
      Key = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::DirectoryEditChange(
      TObject * /*Sender*/)
{
  if (!FChanging)
  {
    FindProfile();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewChange(
      TObject * /*Sender*/, TTreeNode * Node)
{
  if (Node && Node->Data)
  {
    assert(!FChanging);
    FChanging = true;
    try
    {
      LocalDirectoryEdit->Text = ((TBookmark *)Node->Data)->Local;
      RemoteDirectoryEdit->Text = ((TBookmark *)Node->Data)->Remote;
    }
    __finally
    {
      FChanging = false;
    }
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::MoveToButtonClick(TObject * /*Sender*/)
{
  assert(ProfilesView->Selected->Data);
  TBookmark * Bookmark = (TBookmark *)ProfilesView->Selected->Data;
  AnsiString Name = Bookmark->Node;
  if (DoComboInputDialog(LoadStr(MOVE_BOOKMARK_CAPTION), LoadStr(MOVE_BOOKMARK_PROMPT),
      Name, FFolders, NULL, true, HELP_LOCATION_PROFILE_MOVE) &&
      (Name != Bookmark->Node))
  {
    if (Name.Pos("\\"))
    {
      throw Exception(FMTLOAD(BOOKMARK_FOLDER_INVALID_NAME, (Name)));
    }
    TTreeNode * FolderNode;
    int I = FFolders->IndexOf(Name);
    if (Name.IsEmpty())
    {
      FolderNode = NULL;
    }
    else if (I >= 0)
    {
      FolderNode = dynamic_cast<TTreeNode *>(FFolders->Objects[I]);
      assert(FolderNode);
    }
    else
    {
      I = FFolders->Add(Name);
      TTreeNode * NextNode;
      // duplicated in RenameButtonClick()
      if (I < FFolders->Count-1)
      {
        NextNode = dynamic_cast<TTreeNode *>(FFolders->Objects[I+1]);
        assert(NextNode);
      }
      else if (FFolders->Count > 1)
      {
        NextNode = (dynamic_cast<TTreeNode *>(FFolders->Objects[I-1]))->getNextSibling();
      }
      else
      {
        assert(ProfilesView->Items->Count);
        NextNode = ProfilesView->Items->Item[0];
      }
      FolderNode = ProfilesView->Items->Insert(NextNode, Name);
      assert(FolderNode);
      FFolders->Objects[I] = FolderNode;
    }

    BookmarkMove(ProfilesView->Selected, FolderNode);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::RenameButtonClick(TObject * /*Sender*/)
{
  assert(ProfilesView->Selected != NULL);
  if (ProfilesView->Selected != NULL)
  {
    ProfilesView->SetFocus();
    ProfilesView->Selected->EditText();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewGetImageIndex(
      TObject * /*Sender*/, TTreeNode * Node)
{
  Node->ImageIndex = Node->Data ? 0 : (Node->Expanded ? 1 : 2);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewGetSelectedIndex(
      TObject * /*Sender*/, TTreeNode * Node)
{
  Node->SelectedIndex = Node->Data ? 0 : (Node->Expanded ? 1 : 2);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::LocalDirectoryBrowseButtonClick(
  TObject * /*Sender*/)
{
  AnsiString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), true))
  {
    LocalDirectoryEdit->Text = Directory;
    DirectoryEditChange(LocalDirectoryEdit);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::SwitchButtonClick(TObject * /*Sender*/)
{
  WinConfiguration->UseLocationProfiles = false;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewCollapsed(
  TObject * /*Sender*/, TTreeNode * Node)
{
  assert(Node != NULL);
  assert(Node->Data == NULL);
  FBookmarkList->NodeOpened[Node->Text] = false;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewExpanded(
  TObject * /*Sender*/, TTreeNode * Node)
{
  assert(Node != NULL);
  assert(Node->Data == NULL);
  FBookmarkList->NodeOpened[Node->Text] = true;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewEdited(
  TObject * /*Sender*/, TTreeNode * Node, AnsiString & S)
{
  if (Node->Data != NULL)
  {
    if (S.IsEmpty() || (StrToIntDef(S, -123) != -123))
    {
      throw Exception(FMTLOAD(BOOKMARK_INVALID_NAME, (S)));
    }
    // raises exception in case of duplicate name??
    ((TBookmark *)Node->Data)->Name = S;
  }
  else
  {
    if (S.IsEmpty() || S.Pos("\\"))
    {
      throw Exception(FMTLOAD(BOOKMARK_FOLDER_INVALID_NAME, (S)));
    }
    if ((FFolders->IndexOf(S) >= 0) && AnsiCompareText(S, Node->Text))
    {
      throw Exception(FMTLOAD(DUPLICATE_BOOKMARK_FOLDER, (S)));
    }
    assert(Node->Count);
    FFolders->Delete(FFolders->IndexOf(Node->Text));
    int I = FFolders->AddObject(S, Node);

    TTreeNode * NextNode;
    // duplicated in MoveToButtonClick()
    if (I < FFolders->Count-1)
    {
      NextNode = dynamic_cast<TTreeNode *>(FFolders->Objects[I+1]);
      assert(NextNode);
    }
    else if (FFolders->Count > 1)
    {
      NextNode = (dynamic_cast<TTreeNode *>(FFolders->Objects[I-1]))->getNextSibling();
    }
    else
    {
      assert(ProfilesView->Items->Count);
      NextNode = ProfilesView->Items->Item[0];
    }

    if (NextNode != Node)
    {
      Node->MoveTo(NextNode, NextNode ? naInsert : naAddChild);
    }

    for (int i = 0; i < Node->Count; i++)
    {
      ((TBookmark *)Node->Item[i]->Data)->Node = S;
    }
    Node->MakeVisible();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewEditing(
  TObject * /*Sender*/, TTreeNode * /*Node*/, bool & /*AllowEdit*/)
{
  OKBtn->Default = false;
  CancelBtn->Cancel = false;
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::UpdateActions()
{
  TForm::UpdateActions();

  if ((!OKBtn->Default || !CancelBtn->Cancel) && !ProfilesView->IsEditing())
  {
    OKBtn->Default = true;
    CancelBtn->Cancel = true;
  }
}
//---------------------------------------------------------------------------
