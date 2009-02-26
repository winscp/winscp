//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <CoreMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <Common.h>

#include "OpenDirectory.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "IEComboBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
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
  UseSystemSettings(this);

  FOperationSide = osCurrent;
  OperationSide = osLocal;
  FBookmarkDragDest = -1;
  FTerminal = NULL;
  FSessionBookmarkList = new TBookmarkList();
  FSharedBookmarkList = new TBookmarkList();
  FSessionScrollOnDragOver = new TListBoxScrollOnDragOver(SessionBookmarksList, true);
  FSharedScrollOnDragOver = new TListBoxScrollOnDragOver(SharedBookmarksList, true);

  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
}
//---------------------------------------------------------------------
__fastcall TOpenDirectoryDialog::~TOpenDirectoryDialog()
{
  SAFE_DESTROY(FSessionScrollOnDragOver);
  SAFE_DESTROY(FSharedScrollOnDragOver);
  SAFE_DESTROY(FSessionBookmarkList);
  SAFE_DESTROY(FSharedBookmarkList);
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
void __fastcall TOpenDirectoryDialog::UpdateBookmarkControls(
  TButton * AddBookmarkButton, TButton * RemoveBookmarkButton,
  TButton * UpBookmarkButton, TButton * DownBookmarkButton,
  TListBox * BookmarksList, bool ListBoxUpdate)
{
  EnableControl(AddBookmarkButton,
    !Directory.IsEmpty() && (FindBookmark(BookmarksList, Directory) < 0));
  EnableControl(RemoveBookmarkButton, BookmarksList->ItemIndex >= 0);
  EnableControl(UpBookmarkButton, BookmarksList->ItemIndex > 0);
  EnableControl(DownBookmarkButton, BookmarksList->ItemIndex >= 0 &&
    BookmarksList->ItemIndex < BookmarksList->Items->Count-1);

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
      ClientWidth = CWidth > 700 ? 700 : CWidth;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::UpdateControls(bool ListBoxUpdate)
{
  EnableControl(OKBtn, !Directory.IsEmpty());
  LocalDirectoryBrowseButton->Visible = (OperationSide == osLocal);
  SwitchButton->Visible = AllowSwitch;

  UpdateBookmarkControls(AddSessionBookmarkButton, RemoveSessionBookmarkButton,
    UpSessionBookmarkButton, DownSessionBookmarkButton,
    SessionBookmarksList, ListBoxUpdate);
  UpdateBookmarkControls(AddSharedBookmarkButton, RemoveSharedBookmarkButton,
    UpSharedBookmarkButton, DownSharedBookmarkButton,
    SharedBookmarksList, ListBoxUpdate);
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
void __fastcall TOpenDirectoryDialog::LoadBookmarks(TListBox * ListBox,
  TBookmarkList * BookmarkList, TBookmarkList * Source)
{
  if (Source != NULL)
  {
    BookmarkList->Assign(Source);
  }
  else
  {
    BookmarkList->Clear();
  }

  ListBox->Items->Clear();
  for (int i = 0; i < BookmarkList->Count; i++)
  {
    TBookmark * Bookmark = BookmarkList->Bookmarks[i];
    AnsiString Directory = OperationSide == osLocal ? Bookmark->Local : Bookmark->Remote;
    if (!Directory.IsEmpty() && (ListBox->Items->IndexOf(Directory) < 0))
    {
      ListBox->Items->AddObject(Directory, Bookmark);
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
    // cache session key, in case terminal is closed while the window is open
    SessionKey = Terminal->SessionData->SessionKey;
    LoadBookmarks(SessionBookmarksList, FSessionBookmarkList, WinConfiguration->Bookmarks[SessionKey]);
    LoadBookmarks(SharedBookmarksList, FSharedBookmarkList, WinConfiguration->SharedBookmarks);
    PageControl->ActivePage =
      WinConfiguration->UseSharedBookmarks ? SharedBookmarksSheet : SessionBookmarksSheet;
    DirectoryEditChange(NULL);
    if (Mode == odAddBookmark)
    {
      AddAsBookmark(PageControl->ActivePage);
    }
  }
  Result = (ShowModal() == mrOk);
  if (Terminal)
  {
    WinConfiguration->Bookmarks[SessionKey] = FSessionBookmarkList;
    WinConfiguration->SharedBookmarks = FSharedBookmarkList;
    WinConfiguration->UseSharedBookmarks = (PageControl->ActivePage == SharedBookmarksSheet);
  }
  return Result;
}
//---------------------------------------------------------------------------
template<class T>
typename T * GetBookmarkObject(TObject * Sender, T * SessionObject, T * SharedObject)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  assert(Control != NULL);
  switch (abs(Control->Tag))
  {
    case 1: return SessionObject;
    case 2: return SharedObject;
    default: assert(false); return NULL;
  }
}
//---------------------------------------------------------------------------
TBookmarkList * TOpenDirectoryDialog::GetBookmarkList(TObject * Sender)
{
  return GetBookmarkObject(Sender, FSessionBookmarkList, FSharedBookmarkList);
}
//---------------------------------------------------------------------------
TListBox * TOpenDirectoryDialog::GetBookmarksList(TObject * Sender)
{
  return GetBookmarkObject(Sender, SessionBookmarksList, SharedBookmarksList);
}
//---------------------------------------------------------------------------
TListBoxScrollOnDragOver * TOpenDirectoryDialog::GetScrollOnDragOver(TObject * Sender)
{
  return GetBookmarkObject(Sender, FSessionScrollOnDragOver, FSharedScrollOnDragOver);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddAsBookmark(TObject * Sender)
{
  TBookmarkList * BookmarkList = GetBookmarkList(Sender);
  TListBox * BookmarksList = GetBookmarksList(Sender);

  if (Directory.IsEmpty() || (FindBookmark(BookmarksList, Directory) >= 0))
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
    BookmarkList->InsertBefore(
      dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[BookmarksList->ItemIndex]),
      Bookmark);
    BookmarksList->Items->InsertObject(BookmarksList->ItemIndex, Directory, Bookmark);
    BookmarksList->ItemIndex = PrevItemIndex;
  }
  else
  {
    BookmarkList->Add(Bookmark);
    BookmarksList->Items->AddObject(Directory, Bookmark);
    BookmarksList->ItemIndex = BookmarksList->Items->Count-1;
  }

  UpdateControls(true);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::AddBookmarkButtonClick(TObject * Sender)
{
  AddAsBookmark(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::RemoveBookmark(TObject * Sender)
{
  TBookmarkList * BookmarkList = GetBookmarkList(Sender);
  TListBox * BookmarksList = GetBookmarksList(Sender);

  int PrevItemIndex = BookmarksList->ItemIndex;
  TBookmark * Bookmark;
  Bookmark = dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[PrevItemIndex]);
  assert(Bookmark);
  BookmarkList->Delete(Bookmark);
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
  BookmarkSelected(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::RemoveBookmarkButtonClick(TObject * Sender)
{
  RemoveBookmark(Sender);
}
//---------------------------------------------------------------------------
Integer __fastcall TOpenDirectoryDialog::FindBookmark(TListBox * BookmarksList, const AnsiString Bookmark)
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
void __fastcall TOpenDirectoryDialog::BookmarkSelected(TObject * Sender)
{
  TListBox * BookmarksList = GetBookmarksList(Sender);
  if (BookmarksList->ItemIndex >= 0)
  {
    Directory = BookmarksList->Items->Strings[BookmarksList->ItemIndex];
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListClick(TObject * Sender)
{
  BookmarkSelected(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarkMove(TObject * Sender,
  int Source, int Dest)
{
  TBookmarkList * BookmarkList = GetBookmarkList(Sender);
  TListBox * BookmarksList = GetBookmarksList(Sender);

  if (Source >= 0 && Source < BookmarksList->Items->Count &&
      Dest >= 0 && Dest < BookmarksList->Items->Count)
  {
    BookmarkList->MoveTo(
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
void __fastcall TOpenDirectoryDialog::BookmarkButtonClick(TObject * Sender)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  BookmarkMove(Sender,
    GetBookmarksList(Sender)->ItemIndex,
    GetBookmarksList(Sender)->ItemIndex + (Control->Tag / abs(Control->Tag)));
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TOpenDirectoryDialog::AllowBookmarkDrag(TObject * Sender, int X, int Y)
{
  FBookmarkDragDest = GetBookmarksList(Sender)->ItemAtPos(TPoint(X, Y), true);
  return (FBookmarkDragDest >= 0) && (FBookmarkDragDest != FBookmarkDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListStartDrag(
  TObject * Sender , TDragObject *& /*DragObject*/)
{
  FBookmarkDragSource = GetBookmarksList(Sender)->ItemIndex;
  FBookmarkDragDest = -1;
  GetScrollOnDragOver(Sender)->StartDrag();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDragOver(
  TObject * Sender, TObject *Source, int X, int Y, TDragState /*State*/,
  bool &Accept)
{
  if (Source == GetBookmarksList(Sender))
  {
    Accept = AllowBookmarkDrag(Sender, X, Y);
    GetScrollOnDragOver(Sender)->DragOver(TPoint(X, Y));
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDragDrop(
  TObject * Sender, TObject *Source, int X, int Y)
{
  if (Source == GetBookmarksList(Sender))
  {
    if (AllowBookmarkDrag(Sender, X, Y))
    {
      BookmarkMove(Sender, FBookmarkDragSource, FBookmarkDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::SelectBookmark(TListBox * BookmarksList)
{
  int ItemIndex = FindBookmark(BookmarksList, Directory);
  if (ItemIndex >= 0)
  {
    BookmarksList->ItemIndex = ItemIndex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::DirectoryEditChange(TObject * /*Sender*/)
{
  SelectBookmark(SessionBookmarksList);
  SelectBookmark(SharedBookmarksList);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDblClick(TObject * Sender)
{
  if (GetBookmarksList(Sender)->ItemIndex >= 0)
  {
    ModalResult = mrOk;
  }
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
    ActiveControl = CurrentEdit;
  }
  else
  {
    ActiveControl = GetBookmarksList(PageControl->ActivePage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListKeyDown(TObject * Sender,
  WORD & Key, TShiftState /*Shift*/)
{
  if ((GetBookmarksList(Sender)->ItemIndex >= 0) && (Key == VK_DELETE))
  {
    RemoveBookmark(Sender);
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
    DirectoryEditChange(NULL);
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
void __fastcall TOpenDirectoryDialog::BookmarksListEndDrag(TObject * Sender,
  TObject * /*Target*/, int /*X*/, int /*Y*/)
{
  GetScrollOnDragOver(Sender)->EndDrag();
}
//---------------------------------------------------------------------------
