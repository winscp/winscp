//---------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <RemoteFiles.h>

#include "OpenDirectory.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  UnicodeString & Directory, TStrings * Directories, TTerminal * Terminal,
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

  FixComboBoxResizeBug(LocalDirectoryEdit);
  FixComboBoxResizeBug(RemoteDirectoryEdit);
  LoadDialogImage(Image, L"Open folder");
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
    UnicodeString ADirectory = Directory;
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
void __fastcall TOpenDirectoryDialog::SetDirectory(UnicodeString value)
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
UnicodeString __fastcall TOpenDirectoryDialog::GetDirectory()
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
  TButton * ShortCutBookmarkButton,
  TButton * UpBookmarkButton, TButton * DownBookmarkButton,
  TListBox * BookmarksList, bool ListBoxUpdate)
{
  EnableControl(AddBookmarkButton,
    !Directory.IsEmpty() && (FindBookmark(BookmarksList, Directory) < 0));
  EnableControl(RemoveBookmarkButton, BookmarksList->ItemIndex >= 0);
  if (ShortCutBookmarkButton != NULL)
  {
    EnableControl(ShortCutBookmarkButton, BookmarksList->ItemIndex >= 0);
  }
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
    MaxWidth += ScaleByTextHeight(this, 6);
    if (BookmarksList->Items->Count > BookmarksList->ClientHeight / BookmarksList->ItemHeight)
    {
      MaxWidth += GetSystemMetricsForControl(this, SM_CXVSCROLL);
    }
    if (MaxWidth > BookmarksList->Width)
    {
      int CWidth = ClientWidth + (MaxWidth - BookmarksList->Width);
      ClientWidth = Min(CWidth, ScaleByTextHeight(this, 700));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::UpdateControls(bool ListBoxUpdate)
{
  EnableControl(OKBtn, !Directory.IsEmpty());
  LocalDirectoryBrowseButton->Visible = (OperationSide == osLocal);
  SwitchButton->Visible = AllowSwitch;
  SessionBookmarksSheet->TabVisible = (Terminal != NULL);

  UpdateBookmarkControls(AddSessionBookmarkButton, RemoveSessionBookmarkButton,
    NULL, UpSessionBookmarkButton, DownSessionBookmarkButton,
    SessionBookmarksList, ListBoxUpdate);
  UpdateBookmarkControls(AddSharedBookmarkButton, RemoveSharedBookmarkButton,
    ShortCutSharedBookmarkButton, UpSharedBookmarkButton, DownSharedBookmarkButton,
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
UnicodeString __fastcall TOpenDirectoryDialog::BookmarkDirectory(TBookmark * Bookmark)
{
  return Bookmark->GetSideDirectory(OperationSide);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOpenDirectoryDialog::BookmarkText(TBookmark * Bookmark)
{
  UnicodeString Result = BookmarkDirectory(Bookmark);
  if (!Result.IsEmpty() && (Bookmark->ShortCut != 0))
  {
    Result = FORMAT(L"%s (%s)", (Result, ShortCutToText(Bookmark->ShortCut)));
  }
  return Result;
}
//---------------------------------------------------------------------------
TBookmark * __fastcall TOpenDirectoryDialog::GetBookmark(TListBox * BookmarksList, int Index)
{
  return dynamic_cast<TBookmark *>(BookmarksList->Items->Objects[Index]);
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

  Configuration->Usage->SetMax(L"MaxBookmarks", BookmarkList->Count);

  ListBox->Items->Clear();
  for (int i = 0; i < BookmarkList->Count; i++)
  {
    TBookmark * Bookmark = BookmarkList->Bookmarks[i];
    UnicodeString Directory = BookmarkDirectory(Bookmark);
    if (!Directory.IsEmpty() && (FindBookmark(ListBox, Directory) < 0))
    {
      UnicodeString Text = BookmarkText(Bookmark);
      ListBox->Items->AddObject(Text, Bookmark);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TOpenDirectoryDialog::Execute()
{
  bool Result;
  UnicodeString SessionKey;
  if (Terminal)
  {
    // cache session key, in case terminal is closed while the window is open
    SessionKey = Terminal->SessionData->SessionKey;
    LoadBookmarks(SessionBookmarksList, FSessionBookmarkList, WinConfiguration->Bookmarks[SessionKey]);
  }

  LoadBookmarks(SharedBookmarksList, FSharedBookmarkList, WinConfiguration->SharedBookmarks);
  TTabSheet * Page =
    (Terminal == NULL) || WinConfiguration->UseSharedBookmarks ? SharedBookmarksSheet : SessionBookmarksSheet;
  PageControl->ActivePage = Page;
  DirectoryEditChange(NULL);
  if (Mode == odAddBookmark)
  {
    AddAsBookmark(PageControl->ActivePage);
  }

  FBookmarkSelected = false;
  Result = (ShowModal() == DefaultResult(this));
  WinConfiguration->SharedBookmarks = FSharedBookmarkList;
  if (Terminal != NULL)
  {
    WinConfiguration->Bookmarks[SessionKey] = FSessionBookmarkList;
    // Do not remember that shared page was selected,
    // if it was selected implicitly because there's no site page.
    WinConfiguration->UseSharedBookmarks = (PageControl->ActivePage == SharedBookmarksSheet);
  }
  if (Result)
  {
    if (FBookmarkSelected)
    {
      Configuration->Usage->Inc(L"OpenedBookmark");
    }
    else
    {
      Configuration->Usage->Inc(L"OpenedPath");
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
template<typename T>
T * GetBookmarkObject(TObject * Sender, T * SessionObject, T * SharedObject)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  DebugAssert(Control != NULL);
  switch (abs(Control->Tag))
  {
    case 1: return SessionObject;
    case 2: return SharedObject;
    default: DebugFail(); return NULL;
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

  // would always be equal to Directory atm,
  // as only difference can be a shorcut, which is not set
  UnicodeString Text = BookmarkText(Bookmark);

  if (BookmarksList->ItemIndex >= 0)
  {
    int PrevItemIndex = BookmarksList->ItemIndex;
    BookmarkList->InsertBefore(
      GetBookmark(BookmarksList, BookmarksList->ItemIndex), Bookmark);
    BookmarksList->Items->InsertObject(BookmarksList->ItemIndex, Text, Bookmark);
    BookmarksList->ItemIndex = PrevItemIndex;
  }
  else
  {
    BookmarkList->Add(Bookmark);
    BookmarksList->Items->AddObject(Text, Bookmark);
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
  TBookmark * Bookmark = GetBookmark(BookmarksList, PrevItemIndex);
  DebugAssert(Bookmark != NULL);
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
Integer __fastcall TOpenDirectoryDialog::FindBookmark(TListBox * BookmarksList, const UnicodeString Bookmark)
{
  if (OperationSide == osRemote)
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
    {
      TBookmark * ABookmark = GetBookmark(BookmarksList, Index);
      if (AnsiCompareStr(BookmarkDirectory(ABookmark), Bookmark) == 0)
      {
        return Index;
      }
    }
  }
  else
  {
    for (int Index = 0; Index < BookmarksList->Items->Count; Index++)
    {
      TBookmark * ABookmark = GetBookmark(BookmarksList, Index);
      if (AnsiCompareText(BookmarkDirectory(ABookmark), Bookmark) == 0)
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
    Directory = BookmarkDirectory(GetBookmark(BookmarksList, BookmarksList->ItemIndex));
  }
  UpdateControls();
  FBookmarkSelected = true;
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
      GetBookmark(BookmarksList, Dest),
      GetBookmark(BookmarksList, Source),
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
  FBookmarkSelected = false;
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::BookmarksListDblClick(TObject * Sender)
{
  if (GetBookmarksList(Sender)->ItemIndex >= 0)
  {
    ModalResult = DefaultResult(this);
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
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);

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
  SelectDirectoryForEdit(LocalDirectoryEdit);
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
void __fastcall TOpenDirectoryDialog::ShortCutBookmarkButtonClick(
  TObject * Sender)
{
  TBookmarkList * BookmarkList = GetBookmarkList(Sender);
  TListBox * BookmarksList = GetBookmarksList(Sender);

  int Index = BookmarksList->ItemIndex;
  TBookmark * Bookmark = GetBookmark(BookmarksList, Index);
  DebugAssert(Bookmark != NULL);

  TShortCuts ShortCuts;
  WinConfiguration->CustomCommandShortCuts(ShortCuts);
  BookmarkList->ShortCuts(ShortCuts);
  TShortCut ShortCut = Bookmark->ShortCut;
  if (DoShortCutDialog(ShortCut, ShortCuts, HelpKeyword))
  {
    Bookmark->ShortCut = ShortCut;
    BookmarksList->Items->Strings[Index] = BookmarkText(Bookmark);
    UpdateControls(true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TOpenDirectoryDialog::PageControlChange(TObject * /*Sender*/)
{
  BookmarkSelected(GetBookmarksList(PageControl->ActivePage));
}
//---------------------------------------------------------------------------
