//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <Common.h>

#include "LocationProfiles.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "XPGroupBox"
#pragma link "IEComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall LocationProfilesDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TStrings * RemoteDirectories, TTerminal * Terminal)
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
}
//---------------------------------------------------------------------
__fastcall TLocationProfilesDialog::~TLocationProfilesDialog()
{
  SAFE_DESTROY(FBookmarkList);
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
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLocationProfilesDialog::Execute()
{
  bool Result;
  if (Terminal)
  {
    TBookmarkList * BookmarkList;
    BookmarkList = WinConfiguration->Bookmarks[Terminal->SessionData->SessionKey];
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
    WinConfiguration->Bookmarks[Terminal->SessionData->SessionKey] = FBookmarkList;
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
  Result = InputDialog(LoadStr(ADD_BOOKMARK_CAPTION), LoadStr(ADD_BOOKMARK_PROMPT), BookmarkName);
  if (Result)
  {
    if (BookmarkName.IsEmpty() || (StrToIntDef(BookmarkName, -123) != -123))
    {
      throw Exception(FMTLOAD(BOOKMARK_INVALID_NAME, (BookmarkName)));
    }

    TBookmark * Bookmark = new TBookmark();
    Bookmark->Name = BookmarkName;
    Bookmark->Local = LocalDirectory;
    Bookmark->Remote = RemoteDirectory;
    TTreeNode * Selected = ProfilesView->Selected;
    if (Selected && Selected->Data)
    {
      TBookmark * SelectedBookmark = (TBookmark *)Selected->Data;
      Bookmark->Node = SelectedBookmark->Node;
      FBookmarkList->InsertBefore(SelectedBookmark, Bookmark);
      Selected = ProfilesView->Items->InsertObject(Selected, Bookmark->Name, Bookmark);
    }
    else if (Selected && !Selected->Data)
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
    ProfilesView->Selected = Selected;
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
          qaYes | qaNo, 0) == qaYes)
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
    FBookmarkList->MoveAtEnd(Bookmark);
    ProfilesView->Selected->MoveTo(Dest, naAddChild);
  }
  else
  {
    TBookmark * DestBookmark = (TBookmark *)Dest->Data;
    Bookmark->Node = DestBookmark->Node;
    FBookmarkList->MoveBefore(DestBookmark, Bookmark);
    Source->MoveTo(Dest, naInsert);
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
    assert(TargetNode);
    TargetNode = TargetNode->getNextSibling();
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
    Accept = FBookmarkDragSource != ProfilesView->DropTarget;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::ProfilesViewDragDrop(
      TObject * /*Sender*/, TObject * Source, int /*X*/, int /*Y*/)
{
  if ((Source == ProfilesView) && (FBookmarkDragSource != ProfilesView->DropTarget))
  {
    assert(FBookmarkDragSource);

    TTreeNode * Target = ProfilesView->DropTarget;
    if (Target->Data && (Target->AbsoluteIndex > FBookmarkDragSource->AbsoluteIndex))
    {
      Target = Target->getNextSibling() ? Target->getNextSibling() : Target->Parent;
    }

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
      WORD &Key, TShiftState /*Shift*/)
{
  if (RemoveBookmarkButton->Enabled && (Key == VK_DELETE))
  {
    RemoveBookmarkButtonClick(NULL);
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
      Name, FFolders, NULL, true) && Name != Bookmark->Node)
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
  assert(ProfilesView->Selected);
  TTreeNode * Node = ProfilesView->Selected;
  AnsiString Name = Node->Text;
  if (InputDialog(LoadStr(RENAME_BOOKMARK_CAPTION), LoadStr(RENAME_BOOKMARK_PROMPT),
      Name) && (Name != Node->Text))
  {
    if (Node->Data)
    {
      if (Name.IsEmpty() || (StrToIntDef(Name, -123) != -123))
      {
        throw Exception(FMTLOAD(BOOKMARK_INVALID_NAME, (Name)));
      }
      // raises exception in case of duplicate name??
      ((TBookmark *)Node->Data)->Name = Name;
      Node->Text = Name;
    }
    else
    {
      if (Name.IsEmpty() || Name.Pos("\\"))
      {
        throw Exception(FMTLOAD(BOOKMARK_FOLDER_INVALID_NAME, (Name)));
      }
      if ((FFolders->IndexOf(Name) >= 0) && AnsiCompareText(Name, Node->Text))
      {
        throw Exception(FMTLOAD(DUPLICATE_BOOKMARK_FOLDER, (Name)));
      }
      assert(Node->Count);
      FFolders->Delete(FFolders->IndexOf(Node->Text));
      int I = FFolders->AddObject(Name, Node);

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

      Node->Text = Name;
      for (int i = 0; i < Node->Count; i++)
      {
        ((TBookmark *)Node->Item[i]->Data)->Node = Name;
      }
      Node->MakeVisible();
    }
  }
  UpdateControls();
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
void __fastcall TLocationProfilesDialog::LocalDirectoryEditKeyDown(
  TObject * /*Sender*/, WORD & Key, TShiftState Shift)
{
  PathEditKeyDown(LocalDirectoryEdit, Key, Shift, false);
}
//---------------------------------------------------------------------------
void __fastcall TLocationProfilesDialog::RemoteDirectoryEditKeyDown(
  TObject * /*Sender*/, WORD & Key, TShiftState Shift)
{
  PathComboBoxKeyDown(RemoteDirectoryEdit, Key, Shift, true);
}
//---------------------------------------------------------------------------

