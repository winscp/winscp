//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>

#include "OpenDirectory.h"
//---------------------------------------------------------------------
#pragma link "XPGroupBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
Boolean __fastcall OpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  AnsiString & Directory, TStrings * Directories, TTerminal * Terminal)
{
  Boolean Result;
  TOpenDirectoryDialog * Dialog = new TOpenDirectoryDialog(Application);
  try {
    Dialog->Mode = Mode;
    Dialog->OperationSide = Side;
    Dialog->Directory = Directory;
    Dialog->Directories = Directories;
    Dialog->Terminal = Terminal;
    Result = Dialog->Execute();
    if (Result)
      Directory = Dialog->Directory;
  } __finally {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TOpenDirectoryDialog::TOpenDirectoryDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  FOperationSide = osCurrent;
  OperationSide = osLocal;
  FBookmarkDragDest = -1;
  FTerminal = NULL;
  UseSystemFont(this);
}
//---------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SetOperationSide(TOperationSide value)
{
  if (OperationSide != value)
  {
    AnsiString ADirectory = Directory;
    FOperationSide = value;
    Directory = ADirectory;
    RemoteDirectoryEdit->Visible = False;
    LocalDirectoryEdit->Visible = False;
    CurrentEdit->Visible = True;
    EditLabel->FocusControl = CurrentEdit;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SetDirectory(AnsiString value)
{
  if (OperationSide == osRemote) RemoteDirectoryEdit->Text = value;
    else LocalDirectoryEdit->Text = value;
  DirectoryEditChange(NULL);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TOpenDirectoryDialog::GetDirectory()
{
  if (OperationSide == osRemote)
    return UnixExcludeTrailingBackslash(RemoteDirectoryEdit->Text);
  else
    return ExcludeTrailingBackslash(LocalDirectoryEdit->Text);
}
//---------------------------------------------------------------------------
TWinControl * __fastcall TOpenDirectoryDialog::GetCurrentEdit()
{
  if (OperationSide == osRemote) return RemoteDirectoryEdit;
    else return LocalDirectoryEdit;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::UpdateControls()
{
  EnableControl(OKBtn, !Directory.IsEmpty());
  EnableControl(AddBookmarkButton,
    !Directory.IsEmpty() && (FindBookmark(Directory) < 0));
  EnableControl(RemoveBookmarkButton, BookmarksList->ItemIndex >= 0);
  EnableControl(UpBookmarkButton, BookmarksList->ItemIndex > 0);
  EnableControl(DownBookmarkButton, BookmarksList->ItemIndex >= 0 &&
    BookmarksList->ItemIndex < BookmarksList->Items->Count-1);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SetDirectories(TStrings * value)
{
  RemoteDirectoryEdit->Items = value;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TOpenDirectoryDialog::GetDirectories()
{
  return RemoteDirectoryEdit->Items;
}
//---------------------------------------------------------------------------
bool __fastcall TOpenDirectoryDialog::Execute()
{
  bool Result;
  if (Terminal)
  {
    TStrings * Bookmarks;
    Bookmarks = Terminal->Configuration->Bookmarks[OperationSide][Terminal->SessionData->SessionKey];
    if (Bookmarks) BookmarksList->Items = Bookmarks;
      else BookmarksList->Items->Clear();
    DirectoryEditChange(NULL);
    if (Mode == odAddBookmark) AddAsBookmark();
  }
  Result = (ShowModal() == mrOk);
  if (Terminal)
    Terminal->Configuration->Bookmarks[OperationSide][Terminal->SessionData->SessionKey] = BookmarksList->Items;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddAsBookmark()
{
  if (Directory.IsEmpty() || (FindBookmark(Directory) >= 0)) return;
  if (BookmarksList->ItemIndex >= 0)
  {
    int PrevItemIndex = BookmarksList->ItemIndex;
    BookmarksList->Items->Insert(BookmarksList->ItemIndex, Directory);
    BookmarksList->ItemIndex = PrevItemIndex;
  }
  else
  {
    BookmarksList->Items->Add(Directory);
    BookmarksList->ItemIndex = BookmarksList->Items->Count-1;
  }
  if (OperationSide == osRemote) RemoteDirectoryEdit->SelectAll();
    else LocalDirectoryEdit->SelectAll();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddBookmarkButtonClick(TObject */*Sender*/)
{
  AddAsBookmark();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::RemoveBookmarkButtonClick(TObject * /*Sender*/)
{
  Integer PrevItemIndex = BookmarksList->ItemIndex;
  BookmarksList->Items->Delete(BookmarksList->ItemIndex);
  if (PrevItemIndex < BookmarksList->Items->Count)
    BookmarksList->ItemIndex = PrevItemIndex;
  else
    BookmarksList->ItemIndex = BookmarksList->Items->Count-1;
  BookmarksListClick(NULL);
}
//---------------------------------------------------------------------------
Integer __fastcall TOpenDirectoryDialog::FindBookmark(const AnsiString Bookmark)
{
  if (OperationSide == osRemote)
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
      if (AnsiCompareStr(BookmarksList->Items->Strings[Index], Bookmark) == 0)
        return Index;
  }
    else
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
      if (AnsiCompareText(BookmarksList->Items->Strings[Index], Bookmark) == 0)
        return Index;
  }
  return -1;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListClick(TObject * /*Sender*/)
{
  if (BookmarksList->ItemIndex >= 0)
    Directory = BookmarksList->Items->Strings[BookmarksList->ItemIndex];
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarkMove(int Source, int Dest)
{
  if (Source >= 0 && Source < BookmarksList->Items->Count &&
      Dest >= 0 && Dest < BookmarksList->Items->Count)
  {
    BookmarksList->Items->Move(Source, Dest);
    BookmarksList->ItemIndex = Dest;
    BookmarksList->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarkButtonClick(TObject *Sender)
{
  BookmarkMove(BookmarksList->ItemIndex,
    BookmarksList->ItemIndex + (Sender == UpBookmarkButton ? -1 : 1));
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TOpenDirectoryDialog::AllowBookmarkDrag(int X, int Y)
{
  FBookmarkDragDest = BookmarksList->ItemAtPos(TPoint(X, Y), true);
  return (FBookmarkDragDest >= 0) && (FBookmarkDragDest != FBookmarkDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListStartDrag(
      TObject * /*Sender*/ , TDragObject *& /*DragObject*/)
{
  FBookmarkDragSource = BookmarksList->ItemIndex;
  FBookmarkDragDest = -1;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDragOver(
      TObject */*Sender*/, TObject *Source, int X, int Y, TDragState /*State*/,
      bool &Accept)
{
  if (Source == BookmarksList) Accept = AllowBookmarkDrag(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDragDrop(
      TObject * /*Sender*/, TObject *Source, int X, int Y)
{
  if (Source == BookmarksList)
  {
    if (AllowBookmarkDrag(X, Y)) BookmarkMove(FBookmarkDragSource, FBookmarkDragDest);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::DirectoryEditChange(TObject * /*Sender*/)
{
  int ItemIndex = FindBookmark(Directory);
  if (ItemIndex >= 0) BookmarksList->ItemIndex = ItemIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDblClick(TObject * /*Sender*/)
{
  if (BookmarksList->ItemIndex >= 0) ModalResult = mrOk;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SetMode(TOpenDirectoryMode value)
{
  FMode = value;
  Caption = LoadStr(Mode == odBrowse ?
    OPEN_DIRECTORY_BROWSE_CAPTION : OPEN_DIRECTORY_ADD_BOOMARK_ACTION );
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::FormShow(TObject * /*Sender*/)
{
  if (Mode == odBrowse) CurrentEdit->SetFocus();
    else BookmarksList->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListKeyDown(TObject * /*Sender*/,
      WORD &Key, TShiftState /*Shift*/)
{
  if ((BookmarksList->ItemIndex >= 0) &&
      (Key == VK_DELETE)) RemoveBookmarkButtonClick(NULL);
}
//---------------------------------------------------------------------------
