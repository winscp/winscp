//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <Common.h>

#include "OpenDirectory.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "XPThemes"
#pragma link "IEComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  AnsiString & Directory, TStrings * Directories, TTerminal * Terminal,
  bool AllowSwitch)
{
  bool Result;
  TOpenDirectoryDialog * Dialog = new TOpenDirectoryDialog(Application);
  try
  {
    Dialog->Mode = Mode;
    Dialog->OperationSide = Side;
    Dialog->Directory = Directory;
    Dialog->Directories = Directories;
    Dialog->Terminal = Terminal;
    Dialog->AllowSwitch = AllowSwitch;
    Result = Dialog->Execute();
    if (Result)
    {
      Directory = Dialog->Directory;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TOpenDirectoryDialog::TOpenDirectoryDialog(TComponent * AOwner):
  TForm(AOwner)
{
  FOperationSide = osCurrent;
  OperationSide = osLocal;
  FBookmarkDragDest = -1;
  FTerminal = NULL;
  FBookmarkList = new TBookmarkList();
  UseSystemSettings(this);

  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
}
//---------------------------------------------------------------------
__fastcall TOpenDirectoryDialog::~TOpenDirectoryDialog()
{
  SAFE_DESTROY(FBookmarkList);
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
  if (OperationSide == osRemote)
  {
    RemoteDirectoryEdit->Text = value;
  }
  else
  {
    LocalDirectoryEdit->Text = value;
  }
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
void __fastcall TOpenDirectoryDialog::UpdateControls(bool ListBoxUpdate)
{
  EnableControl(OKBtn, !Directory.IsEmpty());
  EnableControl(AddBookmarkButton,
    !Directory.IsEmpty() && (FindBookmark(Directory) < 0));
  EnableControl(RemoveBookmarkButton, BookmarksList->ItemIndex >= 0);
  EnableControl(UpBookmarkButton, BookmarksList->ItemIndex > 0);
  EnableControl(DownBookmarkButton, BookmarksList->ItemIndex >= 0 &&
    BookmarksList->ItemIndex < BookmarksList->Items->Count-1);
  LocalDirectoryBrowseButton->Visible = (OperationSide == osLocal);
  SwitchButton->Visible = AllowSwitch;

  if (ListBoxUpdate)
  {
    int MaxWidth = 0;
    for (int i = 0; i < BookmarksList->Items->Count; i++)
    {
      int Width = BookmarksList->Canvas->TextExtent(BookmarksList->Items->Strings[i]).cx;
      if (Width > MaxWidth)
      {
        MaxWidth = Width;
      }
    }
    BookmarksList->ScrollWidth = MaxWidth;
    MaxWidth += 6;
    if (BookmarksList->Items->Count > BookmarksList->ClientHeight / BookmarksList->ItemHeight)
    {
      MaxWidth += GetSystemMetrics(SM_CXVSCROLL);
    }
    if (MaxWidth > BookmarksList->Width)
    {
      int CWidth = ClientWidth + (MaxWidth - BookmarksList->Width);
      ClientWidth = CWidth > 600 ? 600 : CWidth;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SetDirectories(TStrings * value)
{
  dynamic_cast<TCustomComboBox*>(CurrentEdit)->Items = value;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TOpenDirectoryDialog::GetDirectories()
{
  return RemoteDirectoryEdit->Items;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::LoadBookmarks()
{
  BookmarksList->Items->Clear();
  for (int i = 0; i < FBookmarkList->Count; i++)
  {
    TBookmark * Bookmark = FBookmarkList->Bookmarks[i];
    AnsiString Directory = OperationSide == osLocal ? Bookmark->Local : Bookmark->Remote;
    if (!Directory.IsEmpty() && (BookmarksList->Items->IndexOf(Directory) < 0))
    {
      BookmarksList->Items->AddObject(Directory, Bookmark);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TOpenDirectoryDialog::Execute()
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
    DirectoryEditChange(NULL);
    if (Mode == odAddBookmark)
    {
      AddAsBookmark();
    }
  }
  Result = (ShowModal() == mrOk);
  if (Terminal)
  {
    WinConfiguration->Bookmarks[SessionKey] = FBookmarkList;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddAsBookmark()
{
  if (Directory.IsEmpty() || (FindBookmark(Directory) >= 0))
  {
    return;
  }

  TBookmark * Bookmark = new TBookmark;

  if (OperationSide == osRemote)
  {
    RemoteDirectoryEdit->SelectAll();
    Bookmark->Remote = Directory;
  }
  else
  {
    LocalDirectoryEdit->SelectAll();
    Bookmark->Local = Directory;
  }
  Bookmark->Name = Directory;

  if (BookmarksList->ItemIndex >= 0)
  {
    int PrevItemIndex = BookmarksList->ItemIndex;
    FBookmarkList->InsertBefore(
      dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[BookmarksList->ItemIndex]),
      Bookmark);
    BookmarksList->Items->InsertObject(BookmarksList->ItemIndex, Directory, Bookmark);
    BookmarksList->ItemIndex = PrevItemIndex;
  }
  else
  {
    FBookmarkList->Add(Bookmark);
    BookmarksList->Items->AddObject(Directory, Bookmark);
    BookmarksList->ItemIndex = BookmarksList->Items->Count-1;
  }

  UpdateControls(true);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddBookmarkButtonClick(TObject */*Sender*/)
{
  AddAsBookmark();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::RemoveBookmarkButtonClick(TObject * /*Sender*/)
{
  int PrevItemIndex = BookmarksList->ItemIndex;
  TBookmark * Bookmark;
  Bookmark = dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[PrevItemIndex]);
  assert(Bookmark);
  FBookmarkList->Delete(Bookmark);
  BookmarksList->Items->Delete(PrevItemIndex);
  if (PrevItemIndex < BookmarksList->Items->Count)
  {
    BookmarksList->ItemIndex = PrevItemIndex;
  }
  else
  {
    BookmarksList->ItemIndex = BookmarksList->Items->Count-1;
  }
  UpdateControls(true);
  BookmarksListClick(NULL);
}
//---------------------------------------------------------------------------
Integer __fastcall TOpenDirectoryDialog::FindBookmark(const AnsiString Bookmark)
{
  if (OperationSide == osRemote)
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
    {
      if (AnsiCompareStr(BookmarksList->Items->Strings[Index], Bookmark) == 0)
      {
        return Index;
      }
    }
  }
  else
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
    {
      if (AnsiCompareText(BookmarksList->Items->Strings[Index], Bookmark) == 0)
      {
        return Index;
      }
    }
  }
  return -1;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListClick(TObject * /*Sender*/)
{
  if (BookmarksList->ItemIndex >= 0)
  {
    Directory = BookmarksList->Items->Strings[BookmarksList->ItemIndex];
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarkMove(int Source, int Dest)
{
  if (Source >= 0 && Source < BookmarksList->Items->Count &&
      Dest >= 0 && Dest < BookmarksList->Items->Count)
  {
    FBookmarkList->MoveTo(
      dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[Dest]),
      dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[Source]),
      Source > Dest);
    BookmarksList->Items->Move(Source, Dest);
    BookmarksList->ItemIndex = Dest;
    BookmarksList->SetFocus();
    UpdateControls();
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
  UpdateControls(true);
  if (Mode == odBrowse)
  {
    CurrentEdit->SetFocus();
  }
  else
  {
    BookmarksList->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListKeyDown(TObject * /*Sender*/,
      WORD &Key, TShiftState /*Shift*/)
{
  if ((BookmarksList->ItemIndex >= 0) && (Key == VK_DELETE))
  {
    RemoveBookmarkButtonClick(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::LocalDirectoryBrowseButtonClick(
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
void __fastcall TOpenDirectoryDialog::SwitchButtonClick(TObject * /*Sender*/)
{
  WinConfiguration->UseLocationProfiles = true;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

