//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <CoreMain.h>
#include <Common.h>
#include <PuttyTools.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>
#include <VCLCommon.h>

#include "WinInterface.h"
#include "Login.h"
#include "GUITools.h"
#include "Tools.h"
#include "Setup.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "ComboEdit"
#pragma link "PasswordEdit"
#pragma link "UpDownEdit"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
const int OpenFolderStateIndex = 2;
const int ClosedFolderStateIndex = 3;
const int WorkspaceStateIndex = 4;
const int NewSiteStateIndex = 6;
//---------------------------------------------------------------------------
bool __fastcall DoLoginDialog(TStoredSessionList *SessionList,
  TList * DataList, int Options)
{
  assert(DataList != NULL);
  TLoginDialog * LoginDialog = SafeFormCreate<TLoginDialog>();
  bool Result;
  try
  {
    LoginDialog->Init(SessionList, Options);
    Result = LoginDialog->Execute(DataList);
  }
  __finally
  {
    delete LoginDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
static const TFSProtocol FSOrder[] = { fsSFTPonly, fsSCPonly, fsFTP, fsWebDAV };
//---------------------------------------------------------------------
__fastcall TLoginDialog::TLoginDialog(TComponent* AOwner)
        : TForm(AOwner)
{
  FNewSiteData = new TSessionData(L"");
  FInitialized = false;
  FOptions = loStartup;
  FLocaleChanging = false;
  FHintNode = NULL;
  FScrollOnDragOver = new TTreeViewScrollOnDragOver(SessionTree, true);
  FDataList = NULL;
  FUpdatePortWithProtocol = true;
  FIncrementalSearching = 0;
  FSitesIncrementalSearchHaveNext = false;
  FEditing = false;
  FRenaming = false;

  // we need to make sure that window procedure is set asap
  // (so that CM_SHOWINGCHANGED handling is applied)
  UseSystemSettingsPre(this, &FSystemSettings);

  FOriginalSize = BoundsRect.GetSize();
  FBasicGroupBaseHeight = BasicGroup->Height - BasicSshPanel->Height - BasicFtpPanel->Height;
  InitControls();
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::~TLoginDialog()
{
  delete FScrollOnDragOver;
  assert(FSystemSettings);
  DeleteSystemSettings(this, FSystemSettings);
  FSystemSettings = NULL;
  delete FNewSiteData;
  InvalidateSessionData();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InvalidateSessionData()
{
  delete FSessionData;
  FSessionData = NULL;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init(TStoredSessionList *SessionList,
  int Options)
{
  FStoredSessions = SessionList;
  LoadSessions();
  FOptions = Options;
  UnicodeString Dummy;
  RunPageantAction->Visible = FindTool(PageantTool, Dummy);
  RunPuttygenAction->Visible = FindTool(PuttygenTool, Dummy);
  UpdateControls();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitControls()
{
  if (SessionTree->WindowProc != SessionTreeProc)
  {
    FOldSessionTreeProc = SessionTree->WindowProc;
    SessionTree->WindowProc = SessionTreeProc;
  }

  int FtpsNoneIndex = FtpsToIndex(ftpsNone);
  int FtpsImplicitIndex = FtpsToIndex(ftpsImplicit);
  FtpsCombo->Items->Strings[FtpsImplicitIndex] = LoadStr(FTPS_IMPLICIT);
  FtpsCombo->Items->Strings[FtpsToIndex(ftpsExplicitTls)] = LoadStr(FTPS_EXPLICIT_TLS);
  FtpsCombo->Items->Strings[FtpsToIndex(ftpsExplicitSsl)] = LoadStr(FTPS_EXPLICIT_SSL);
  WebDavsCombo->Items->Strings[FtpsNoneIndex] = FtpsCombo->Items->Strings[FtpsNoneIndex];
  WebDavsCombo->Items->Strings[FtpsImplicitIndex] = FtpsCombo->Items->Strings[FtpsImplicitIndex];

  BasicSshPanel->Top = BasicFtpPanel->Top;

  SitesIncrementalSearchLabel->AutoSize = false;
  SitesIncrementalSearchLabel->Left = SessionTree->Left;
  SitesIncrementalSearchLabel->Width = SessionTree->Width;
  SitesIncrementalSearchLabel->Top = SessionTree->BoundsRect.Bottom - SitesIncrementalSearchLabel->Height;
  SitesIncrementalSearchLabel->Visible = false;

  ReadOnlyControl(TransferProtocolView);
  ReadOnlyControl(EncryptionView);

  MenuButton(ToolsMenuButton);
  MenuButton(ManageButton);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init()
{
  if (!FInitialized)
  {
    UseSystemSettingsPost(this, FSystemSettings);
  }
  else
  {
    UseSystemSettings(this, &FSystemSettings);
  }
  FInitialized = true;

  Caption = FORMAT(L"%s %s", (AppName, Caption));

  InitControls();

  #ifdef NO_FILEZILLA
  assert(TransferProtocolCombo->Items->Count == FSPROTOCOL_COUNT - 2 - 1);
  TransferProtocolCombo->Items->Delete(TransferProtocolCombo->Items->Count - 1);
  #endif

  ReadOnlyControl(ContentsNameEdit);
  ReadOnlyControl(ContentsMemo);

  if (ALWAYS_FALSE(SessionTree->Items->Count == 0) ||
      ((SessionTree->Items->Count == 1) &&
       ALWAYS_TRUE(IsNewSiteNode(SessionTree->Items->GetFirstNode()))))
  {
    ActiveControl = HostNameEdit;
  }
  else
  {
    ActiveControl = SessionTree;
  }

  UpdateControls();
}
//---------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::AddSessionPath(UnicodeString Path,
  bool CanCreate, bool IsWorkspace)
{
  TTreeNode * Parent = NULL;
  while (!Path.IsEmpty())
  {
    UnicodeString Folder = ::CutToChar(Path, L'/', false);
    TTreeNode * Node =
      ((Parent == NULL) ? SessionTree->Items->GetFirstNode() : Parent->getFirstChild());
    // note that we allow folder with the same name as existing session
    // on the same level (see also SessionTreeEdited)
    while ((Node != NULL) && (IsSessionNode(Node) || !AnsiSameText(Node->Text, Folder)))
    {
      Node = Node->getNextSibling();
    }

    if (Node == NULL)
    {
      if (!CanCreate)
      {
        return NULL;
      }
      else
      {
        TTreeNode * AParent = Parent;
        Parent = SessionTree->Items->AddChild(Parent, Folder);
        // once workspace, forever workspace
        if (!IsWorkspaceNode(Parent))
        {
          if (IsWorkspace)
          {
            Parent->StateIndex = WorkspaceStateIndex;
          }
          else
          {
            UpdateFolderNode(Parent);
          }
        }
        // folders seem not to be sorted automatically (not having set the data property)
        if (AParent == NULL)
        {
          SessionTree->Items->AlphaSort();
        }
        else
        {
          AParent->AlphaSort();
        }
      }
    }
    else
    {
      Parent = Node;
    }
  }
  return Parent;
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsFolderNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data == NULL) && (Node->StateIndex != WorkspaceStateIndex);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsWorkspaceNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data == NULL) && (Node->StateIndex == WorkspaceStateIndex);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsFolderOrWorkspaceNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data == NULL);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsSiteNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data != NULL) && (Node->Data != FNewSiteData);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsNewSiteNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data != NULL) && (Node->Data == FNewSiteData);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsSessionNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data != NULL);
}
//---------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetNodeSession(TTreeNode * Node)
{
  return NOT_NULL(static_cast<TSessionData *>(Node->Data));
}
//---------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::AddSession(TSessionData * Data)
{
  TTreeNode * Parent = AddSessionPath(UnixExtractFilePath(Data->Name), true, Data->IsWorkspace);
  TTreeNode * Node = SessionTree->Items->AddChild(Parent, UnixExtractFileName(Data->Name));
  Node->Data = Data;

  return Node;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::DestroySession(TSessionData * Data)
{
  StoredSessions->Remove(Data);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessions()
{
  SessionTree->Items->BeginUpdate();
  try
  {
    SessionTree->Items->Clear();

    TTreeNode * Node = SessionTree->Items->AddChild(NULL, LoadStr(LOGIN_NEW_SITE_NODE));
    Node->Data = FNewSiteData;
    Node->StateIndex = NewSiteStateIndex;

    assert(StoredSessions != NULL);
    for (int Index = 0; Index < StoredSessions->Count; Index++)
    {
      AddSession(StoredSessions->Sessions[Index]);
    }
  }
  __finally
  {
    // folders seem not to be sorted automatically (not having set the data property)
    SessionTree->AlphaSort();
    SessionTree->Items->EndUpdate();
  }
  SessionTree->Selected = SessionTree->Items->GetFirstNode();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateFolderNode(TTreeNode * Node)
{
  assert((Node->StateIndex == -1) ||
    (Node->StateIndex == OpenFolderStateIndex) || (Node->StateIndex == ClosedFolderStateIndex));
  Node->StateIndex = (Node->Expanded ? OpenFolderStateIndex : ClosedFolderStateIndex);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Default()
{
  if (ALWAYS_TRUE(StoredSessions != NULL))
  {
    FNewSiteData->Assign(StoredSessions->DefaultSettings);
  }

  TTreeNode * NewSiteNode = SessionTree->Items->GetFirstNode();
  if (ALWAYS_TRUE(IsNewSiteNode(NewSiteNode)))
  {
    SessionTree->Selected = NewSiteNode;
  }

  LoadContents();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadContents()
{
  bool UseContentsPanel;
  TTreeNode * Node = SessionTree->Selected;
  if (IsSessionNode(Node))
  {
    LoadSession(SelectedSession);
    UseContentsPanel = false;
  }
  else if (ALWAYS_TRUE(IsFolderOrWorkspaceNode(Node)))
  {
    UnicodeString NodePath = SessionNodePath(Node);
    ContentsNameEdit->Text = NodePath;
    UnicodeString CommonRoot = IsFolderNode(Node) ? UnixIncludeTrailingBackslash(NodePath) : UnicodeString();
    ContentsMemo->Lines->Text =
      GetFolderOrWorkspaceContents(Node, L"", CommonRoot);
    UseContentsPanel = true;
    if (IsFolderNode(Node))
    {
      ContentsGroupBox->Caption = LoadStr(LOGIN_SITE_FOLDER_CAPTION);
    }
    else if (ALWAYS_TRUE(IsWorkspaceNode(Node)))
    {
      ContentsGroupBox->Caption = LoadStr(LOGIN_WORKSPACE_CAPTION);
    }
  }

  SitePanel->Visible = !UseContentsPanel;
  ContentsPanel->Visible = UseContentsPanel;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSession(TSessionData * SessionData)
{
  WinConfiguration->BeginMasterPasswordSession();
  try
  {
    UserNameEdit->Text = SessionData->UserName;
    PortNumberEdit->AsInteger = SessionData->PortNumber;
    HostNameEdit->Text = SessionData->HostName;

    bool Editable = FEditing || (SessionData == FNewSiteData);
    if (Editable)
    {
      PasswordEdit->Text = SessionData->Password;
    }
    else
    {
      PasswordEdit->Text =
        SessionData->HasPassword() ?
          UnicodeString::StringOfChar(L'?', 16) : UnicodeString();
    }

    int FtpsIndex = FtpsToIndex(SessionData->Ftps);
    FtpsCombo->ItemIndex = FtpsIndex;
    WebDavsCombo->ItemIndex = FtpsIndex;
    EncryptionView->Text =
      ALWAYS_TRUE(FtpsCombo->ItemIndex >= WebDavsCombo->ItemIndex) ? FtpsCombo->Text : WebDavsCombo->Text;

    bool AllowScpFallback;
    TransferProtocolCombo->ItemIndex = FSProtocolToIndex(SessionData->FSProtocol, AllowScpFallback);
    TransferProtocolView->Text = TransferProtocolCombo->Text;

    // just in case TransferProtocolComboChange is not triggered
    FDefaultPort = DefaultPort();
    FUpdatePortWithProtocol = true;

    // advanced
    InvalidateSessionData();
    // close advanced settings only when really needed,
    // see also note in SessionAdvancedActionExecute
    if (Editable)
    {
      FSessionData = new TSessionData(L"");
      FSessionData->Assign(SessionData);
    }
  }
  __finally
  {
    WinConfiguration->EndMasterPasswordSession();
  }

  UpdateControls();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSession(TSessionData * SessionData)
{
  // advanced
  if (ALWAYS_TRUE(FSessionData != NULL))
  {
    SessionData->Assign(FSessionData);
  }

  // Basic page
  SessionData->UserName = UserNameEdit->Text.Trim();
  SessionData->PortNumber = PortNumberEdit->AsInteger;
  // must be loaded after UserName, because HostName may be in format user@host
  SessionData->HostName = HostNameEdit->Text.Trim();
  SessionData->Password = PasswordEdit->Text;
  SessionData->Ftps = GetFtps();

  SessionData->FSProtocol =
    // requiring SCP fallback distinction
    GetFSProtocol(true);

  TSessionData * EditingSessionData = GetEditingSessionData();
  SessionData->Name =
    (EditingSessionData != NULL) ? EditingSessionData->Name : SessionData->DefaultSessionName;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateControls()
{
  // without FLocaleChanging test, some button controls get lost, when changing language
  if (Visible && FInitialized && !FLocaleChanging)
  {
    bool NewSiteSelected = IsNewSiteNode(SessionTree->Selected);

    bool Editable = NewSiteSelected || FEditing;

    TFSProtocol FSProtocol = GetFSProtocol(false);
    bool SshProtocol = IsSshProtocol(FSProtocol);
    bool FtpProtocol = (FSProtocol == fsFTP);
    bool WebDavProtocol = (FSProtocol == fsWebDAV);

    // session
    FtpsCombo->Visible = Editable && FtpProtocol;
    FtpsLabel->Visible = FtpProtocol;
    WebDavsCombo->Visible = Editable && WebDavProtocol;
    WebDavsLabel->Visible = WebDavProtocol;
    EncryptionView->Visible = !Editable && (FtpProtocol || WebDavProtocol);

    BasicSshPanel->Visible = SshProtocol;
    BasicFtpPanel->Visible = FtpProtocol && Editable;
    // we do not support both at the same time
    assert(!BasicSshPanel->Visible || !BasicFtpPanel->Visible);
    BasicGroup->Height =
      FBasicGroupBaseHeight +
      (BasicSshPanel->Visible ? BasicSshPanel->Height : 0) +
      (BasicFtpPanel->Visible ? BasicFtpPanel->Height : 0);
    AnonymousLoginCheck->Checked =
      SameText(UserNameEdit->Text, AnonymousUserName) &&
      SameText(PasswordEdit->Text, AnonymousPassword);
    TransferProtocolCombo->Visible = Editable;
    TransferProtocolView->Visible = !TransferProtocolCombo->Visible;
    ReadOnlyControl(HostNameEdit, !Editable);
    ReadOnlyControl(PortNumberEdit, !Editable);
    PortNumberEdit->ButtonsVisible = Editable;
    ReadOnlyControl(UserNameEdit, !Editable);
    ReadOnlyControl(PasswordEdit, !Editable);

    // sites
    if (SitesIncrementalSearchLabel->Visible != !FSitesIncrementalSearch.IsEmpty())
    {
      if (FSitesIncrementalSearch.IsEmpty())
      {
        SitesIncrementalSearchLabel->Visible = false;
        SessionTree->Height = SitesIncrementalSearchLabel->BoundsRect.Bottom - SessionTree->Top;
      }
      else
      {
        SitesIncrementalSearchLabel->Visible = true;
        SessionTree->Height = SitesIncrementalSearchLabel->BoundsRect.Top - SessionTree->Top;
      }
    }

    if (!FSitesIncrementalSearch.IsEmpty())
    {
      SitesIncrementalSearchLabel->Caption =
        L" " + FMTLOAD(LOGIN_SITES_INC_SEARCH, (FSitesIncrementalSearch)) +
        (FSitesIncrementalSearchHaveNext ? L" " + LoadStr(LOGIN_SITES_NEXT_SEARCH) : UnicodeString());
    }

    EnableControl(ManageButton, !FEditing);
    EnableControl(ToolsMenuButton, !FEditing);
    EnableControl(CloseButton, !FEditing);

    DefaultButton(LoginButton, !FEditing && !FRenaming);
    CloseButton->Cancel = !FEditing && !FRenaming;
    DefaultButton(SaveButton, FEditing);
    EditCancelButton->Cancel = FEditing;

    UpdateButtonVisibility(SaveButton);
    UpdateButtonVisibility(EditButton);
    UpdateButtonVisibility(EditCancelButton);

    bool CanSaveSssion = FEditing;
    TAction * SaveButtonAction =
      CanSaveSssion && SupportsSplitButton() ? SaveSessionAction : SaveAsSessionAction;
    if (SaveButton->Action != SaveButtonAction)
    {
      SaveButton->Action = SaveButtonAction;
    }
    SaveSessionMenuItem->Visible = CanSaveSssion;
    SaveSessionMenuItem->Default = CanSaveSssion;
    SaveAsSessionMenuItem->Default = !CanSaveSssion;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DefaultButton(TButton * Button, bool Default)
{
  // default property setter does not have guard for "the same value"
  if (Button->Default != Default)
  {
    Button->Default = Default;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateButtonVisibility(TButton * Button)
{
  TAction * Action = NOT_NULL(dynamic_cast<TAction *>(Button->Action));
  // when all action targets are hidden, action does not get updated,
  // so we need to do it manually
  Action->Update();
  // button visibility cannot be bound to action visibility,
  // so we do not bother setting action visibility, instead we manually
  // bind visibility to enabled state
  Button->Visible = Action->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DataChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormShow(TObject * /*Sender*/)
{
  if (!FInitialized || FLocaleChanging)
  {
    Init();
    LoadContents();
  }

  // among other this makes the expanded nodes look like expanded,
  // because the LoadState call in Execute is too early,
  // and some stray call to collapsed event during showing process,
  // make the image be set to collapsed
  LoadState();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeChange(TObject * /*Sender*/,
  TTreeNode * /*Node*/)
{
  if (FIncrementalSearching <= 0)
  {
    // Make sure UpdateControls is called here, no matter what,
    // now it is always called from ResetSitesIncrementalSearch.
    // For the "else" scenario, UpdateControls is called later from SitesIncrementalSearch.
    ResetSitesIncrementalSearch();
  }

  if (FInitialized)
  {
    LoadContents();
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSessionData()
{
  if (SelectedSession == FNewSiteData)
  {
    SaveSession(FNewSiteData);
  }
  return SelectedSession;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeDblClick(TObject * /*Sender*/)
{
  TPoint P = SessionTree->ScreenToClient(Mouse->CursorPos);
  TTreeNode * Node = SessionTree->GetNodeAt(P.x, P.y);

  if (IsSessionNode(Node))
  {
    TSessionData * Data = GetNodeSession(Node);
    // this may be false, when collapsed folder was double-clicked,
    // it got expanded, view was shifted to accommodate folder contents,
    // so that cursor now points to a different node (site)
    if (Data == SelectedSession)
    {
      if (FStoredSessions->CanLogin(Data))
      {
        ModalResult = mrOk;
      }
    }
  }
  else if (IsWorkspaceNode(Node))
  {
    if (HasNodeAnySession(Node, true))
    {
      ModalResult = mrOk;
    }
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSelectedSession()
{
  // Selected can be temporarily NULL, e.g. while deleting selected node
  TTreeNode * Node = SessionTree->Selected;
  if ((Node != NULL) &&
      (IsSiteNode(Node) || IsNewSiteNode(Node)))
  {
    return GetNodeSession(Node);
  }
  else
  {
    return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (!SessionTree->IsEditing())
  {
    if (Key == VK_DELETE)
    {
      DeleteSessionAction->Execute();
      Key = 0;
    }
    else if (Key == VK_F2)
    {
      RenameSessionAction->Execute();
      Key = 0;
    }
    else if (Key == VK_BACK)
    {
      Key = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeKeyPress(TObject * /*Sender*/, System::WideChar & Key)
{
  if (!SessionTree->IsEditing())
  {
    // filter control sequences
    if (Key >= VK_SPACE)
    {
      if (!SitesIncrementalSearch(FSitesIncrementalSearch + Key, false, false))
      {
        MessageBeep(0);
      }
      Key = 0;
    }
    else if (Key == VK_BACK)
    {
      if (!FSitesIncrementalSearch.IsEmpty())
      {
        if (FSitesIncrementalSearch.Length() == 1)
        {
          ResetSitesIncrementalSearch();
        }
        else
        {
          UnicodeString NewText =
            FSitesIncrementalSearch.SubString(1, FSitesIncrementalSearch.Length() - 1);
          SitesIncrementalSearch(NewText, false, false);
        }
        Key = 0;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditSession()
{
  HostNameEdit->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditSessionActionExecute(TObject * /*Sender*/)
{
  if (ALWAYS_TRUE(SelectedSession != NULL))
  {
    FEditing = true;
    EditSession();
    // reload session, to make sure we load decrypted password
    LoadContents();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::FindSessionNode(TSessionData * SessionData, bool ByName)
{
  TTreeNode * Node = SessionTree->Items->GetFirstNode();
  while (
    (Node != NULL) &&
    ((!ByName && (Node->Data != SessionData)) ||
     (ByName && (!IsSiteNode(Node) || (GetNodeSession(Node)->Name != SessionData->Name)))))
  {
    Node = Node->GetNext();
  }
  return Node;
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetEditingSessionData()
{
  return FEditing ? NOT_NULL(SelectedSession) : NULL;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveAsSession(bool ForceDialog)
{
  std::auto_ptr<TSessionData> SessionData(new TSessionData(L""));
  SaveSession(SessionData.get());

  TSessionData * EditingSessionData = GetEditingSessionData();

  TSessionData * NewSession = DoSaveSession(SessionData.get(), EditingSessionData, ForceDialog);
  if (NewSession != NULL)
  {
    TTreeNode * ParentNode = AddSessionPath(UnixExtractFilePath(NewSession->SessionName), false, false);
    CheckIsSessionFolder(ParentNode);

    FEditing = false;

    TTreeNode * Node = FindSessionNode(NewSession, false);

    if (Node == NULL)
    {
      Node = AddSession(NewSession);
    }

    SessionTree->Selected = Node;
    SessionTree->SetFocus();

    // this
    // - updates TransferProtocolView and EncryptionView
    // - clears the password box, if user has not opted to save password
    // - reloads fake password
    LoadContents();

    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSessionActionExecute(TObject * /*Sender*/)
{
  SaveAsSession(false);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveAsSessionActionExecute(TObject * /*Sender*/)
{
  SaveAsSession(true);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TLoginDialog::SessionNodePath(TTreeNode * Node)
{
  UnicodeString Path;
  if ((Node != NULL) && !IsNewSiteNode(Node))
  {
    Path = Node->Text;
    Node = Node->Parent;
    while (Node != NULL)
    {
      Path.Insert(UnixIncludeTrailingBackslash(Node->Text), 1);
      Node = Node->Parent;
    }
  }

  return Path;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DeleteSessionActionExecute(TObject * /*Sender*/)
{
  assert(SessionTree->Selected != NULL);

  TTreeNode * Node = SessionTree->Selected;
  if (IsSiteNode(Node))
  {
    TSessionData * Session = SelectedSession;
    if (MessageDialog(FMTLOAD(CONFIRM_DELETE_SESSION, (Session->SessionName)),
          qtConfirmation, qaOK | qaCancel, HELP_DELETE_SESSION) == qaOK)
    {
      WinConfiguration->DeleteSessionFromJumpList(Session->SessionName);
      Session->Remove();
      DestroySession(Session);
      SessionTree->Selected->Delete();
    }
  }
  else if (IsFolderNode(Node) || IsWorkspaceNode(Node))
  {
    int Sessions = 0;
    TTreeNode * ANode = Node->GetNext();
    while ((ANode != NULL) && ANode->HasAsParent(Node))
    {
      if (IsSessionNode(ANode))
      {
        TSessionData * Session = GetNodeSession(ANode);
        if (Session->Special)
        {
          SessionTree->Selected = ANode;
          ANode->MakeVisible();
          throw Exception(FMTLOAD(LOGIN_DELETE_SPECIAL_SESSION, (Session->SessionName)));
        }
        Sessions++;
      }
      ANode = ANode->GetNext();
    }

    UnicodeString Path = SessionNodePath(Node);

    int Prompt;
    UnicodeString HelpKeyword;
    if (IsFolderNode(Node))
    {
      Prompt = LOGIN_DELETE_SESSION_FOLDER;
      HelpKeyword = HELP_DELETE_SESSION_FOLDER;
    }
    else
    {
      Prompt = LOGIN_DELETE_WORKSPACE;
      HelpKeyword = HELP_DELETE_WORKSPACE;
    }

    if ((Sessions == 0) ||
        (MessageDialog(FMTLOAD(Prompt, (Path, Sessions)),
          qtConfirmation, qaOK | qaCancel, HelpKeyword) == qaOK))
    {
      if (IsWorkspaceNode(Node))
      {
        WinConfiguration->DeleteWorkspaceFromJumpList(Path);
      }

      Node = SessionTree->Selected;
      TTreeNode * ANode = Node->GetNext();
      while ((ANode != NULL) && ANode->HasAsParent(Node))
      {
        if (IsSessionNode(ANode))
        {
          TSessionData * Session = GetNodeSession(ANode);
          Session->Remove();
          DestroySession(Session);
        }
        ANode = ANode->GetNext();
      }

      SessionTree->Selected->Delete();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ReloadSessions()
{
  SaveState();
  LoadSessions();
  LoadState();
  if (SessionTree->Items->Count > 0)
  {
    SessionTree->Items->GetFirstNode()->MakeVisible();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportSessionsActionExecute(TObject * /*Sender*/)
{
  if (DoImportSessionsDialog())
  {
    ReloadSessions();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CleanUpActionExecute(TObject * /*Sender*/)
{
  if (DoCleanupDialog(StoredSessions, Configuration))
  {
    SaveState();
    LoadSessions();
    LoadState();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AboutActionExecute(TObject * /*Sender*/)
{
  DoAboutDialog(Configuration);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ActionListUpdate(TBasicAction * BasicAction,
      bool &Handled)
{
  bool NewSiteSelected = IsNewSiteNode(SessionTree->Selected);
  bool SiteSelected = IsSiteNode(SessionTree->Selected);
  bool FolderOrWorkspaceSelected = IsFolderOrWorkspaceNode(SessionTree->Selected);

  TAction * Action = NOT_NULL(dynamic_cast<TAction *>(BasicAction));
  bool PrevEnabled = Action->Enabled;

  if (Action == EditSessionAction)
  {
    EditSessionAction->Enabled = SiteSelected && !FEditing;
  }
  else if (Action == EditCancelAction)
  {
    EditCancelAction->Enabled = FEditing;
  }
  else if (Action == DeleteSessionAction)
  {
    DeleteSessionAction->Enabled =
      ((SiteSelected && !SelectedSession->Special && !FEditing) ||
       FolderOrWorkspaceSelected);
  }
  else if (Action == RenameSessionAction)
  {
    RenameSessionAction->Enabled =
      ((SiteSelected && !SelectedSession->Special && !FEditing) ||
       FolderOrWorkspaceSelected);
  }
  else if (Action == DesktopIconAction)
  {
    DesktopIconAction->Enabled =
      (SiteSelected && !FEditing) ||
      (FolderOrWorkspaceSelected && HasNodeAnySession(SessionTree->Selected));
  }
  else if (Action == SendToHookAction)
  {
    SendToHookAction->Enabled = SiteSelected && !FEditing;
  }
  else if (Action == LoginAction)
  {
    TSessionData * Data = GetSessionData();
    LoginAction->Enabled =
      ((Data != NULL) && FStoredSessions->CanLogin(Data) && !FEditing) ||
      (FolderOrWorkspaceSelected && HasNodeAnySession(SessionTree->Selected, true));
  }
  else if (Action == SaveSessionAction)
  {
    SaveSessionAction->Enabled = FEditing;
  }
  else if (Action == SessionAdvancedAction)
  {
    SessionAdvancedAction->Enabled = NewSiteSelected || FEditing;
  }
  else if (Action == SaveAsSessionAction)
  {
    SaveAsSessionAction->Enabled = NewSiteSelected || FEditing;
  }
  else if (Action == NewSessionFolderAction)
  {
    NewSessionFolderAction->Enabled = !FEditing;
  }
  else if (Action == ImportSessionsAction)
  {
    ImportSessionsAction->Enabled =
      GUIConfiguration->AnyPuttySessionForImport(StoredSessions) ||
      GUIConfiguration->AnyFilezillaSessionForImport(StoredSessions);
  }
  else if ((Action == ImportAction) ||
           (Action == AboutAction) || (Action == CleanUpAction) ||
           (Action == CheckForUpdatesAction) || (Action == PreferencesAction))
  {
    Action->Visible = FLAGSET(FOptions, loExternalTools);
  }
  Handled = true;

  // to update buttons visibility
  if (PrevEnabled != Action->Enabled)
  {
    UpdateControls();
  }

  Idle();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Idle()
{
  if (SessionTree->IsEditing() != FRenaming)
  {
    FRenaming = SessionTree->IsEditing();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::Execute(TList * DataList)
{
  FDataList = DataList;
  if (DataList->Count > 0)
  {
    TSessionData * SessionData = reinterpret_cast<TSessionData * >(DataList->Items[0]);
    TTreeNode * Node = FindSessionNode(SessionData, true);
    if (Node != NULL)
    {
      SessionTree->Selected = Node;
      ActiveControl = SessionTree;
    }
    else
    {
      // This is actualy bit pointless, as we focus the last selected site anyway
      // in LoadState(). As of now, we hardly get any useful data
      // in ad-hoc DataList anyway, so it is not a big deal
      FNewSiteData->Assign(SessionData);
      FNewSiteData->Special = false;
      LoadContents();
    }
  }
  else
  {
    Default();
  }
  LoadState();
  bool Result = (ShowModal() == mrOk);
  SaveState();
  if (Result)
  {
    SaveConfiguration();
    // DataList saved already from FormCloseQuery
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveDataList(TList * DataList)
{
  // Normally we would call this from Execute,
  // but at that point the windows is already hidden.
  // Cloning session data may pop up master password dialog:
  // - if it happens between closing and destroyiong login dialog
  //   the next window will appear in background for some reason
  // - and its actually even nicer when master password dialog pops up over
  //   the login dialog

  DataList->Clear();

  if (!IsNewSiteNode(SessionTree->Selected))
  {
    TTreeNode * Node = SessionTree->Selected;
    if (IsFolderOrWorkspaceNode(Node))
    {
      UnicodeString Name = SessionNodePath(Node);

      if (IsWorkspaceNode(Node))
      {
        WinConfiguration->AddWorkspaceToJumpList(Name);
      }

      StoredSessions->GetFolderOrWorkspace(Name, DataList);
    }
    else if (ALWAYS_TRUE(IsSiteNode(Node)))
    {
      TSessionData * Data2 = new TSessionData(L"");
      Data2->Assign(GetNodeSession(Node));
      DataList->Add(Data2);
    }
  }
  else
  {
    TSessionData * Data = GetSessionData();
    TSessionData * Data2 = new TSessionData(L"");
    Data2->Assign(Data);
    // we carry the name of the edited stored session around while on the dialog,
    // but we do not want it to leave the dialog, so that we can distinguish
    // stored and ad-hoc sessions
    Data2->Name = L"";
    DataList->Add(Data2);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveOpenedStoredSessionFolders(
  TTreeNode * Node, TStrings * OpenedStoredSessionFolders)
{
  if (IsFolderNode(Node))
  {
    if (Node->Expanded)
    {
      OpenedStoredSessionFolders->Add(SessionNodePath(Node));
    }

    for (int Index = 0; Index < Node->Count; Index++)
    {
      SaveOpenedStoredSessionFolders(Node->Item[Index], OpenedStoredSessionFolders);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveState()
{
  assert(WinConfiguration != NULL);

  TStringList * OpenedStoredSessionFolders = new TStringList();
  WinConfiguration->BeginUpdate();
  try
  {
    OpenedStoredSessionFolders->CaseSensitive = false;
    OpenedStoredSessionFolders->Sorted = true;

    for (int Index = 0; Index < SessionTree->Items->Count; Index++)
    {
      SaveOpenedStoredSessionFolders(
        SessionTree->Items->Item[Index], OpenedStoredSessionFolders);
    }

    WinConfiguration->OpenedStoredSessionFolders = OpenedStoredSessionFolders->CommaText;
  }
  __finally
  {
    WinConfiguration->EndUpdate();
    delete OpenedStoredSessionFolders;
  }

  WinConfiguration->LastStoredSession = SessionNodePath(SessionTree->Selected);

  // used only when changing locale
  FSavedBounds = BoundsRect;

  TLoginDialogConfiguration DialogConfiguration = CustomWinConfiguration->LoginDialog;
  DialogConfiguration.WindowSize = StoreFormSize(this);
  CustomWinConfiguration->LoginDialog = DialogConfiguration;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadOpenedStoredSessionFolders(
  TTreeNode * Node, TStrings * OpenedStoredSessionFolders)
{
  if (IsFolderNode(Node))
  {
    UnicodeString Path = SessionNodePath(Node);
    if (OpenedStoredSessionFolders->IndexOf(Path) >= 0)
    {
      Node->Expand(false);
      UpdateFolderNode(Node);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadState()
{
  TStringList * OpenedStoredSessionFolders = new TStringList();
  try
  {
    OpenedStoredSessionFolders->CaseSensitive = false;
    OpenedStoredSessionFolders->Sorted = true;
    OpenedStoredSessionFolders->CommaText = WinConfiguration->OpenedStoredSessionFolders;

    for (int Index = 0; Index < SessionTree->Items->Count; Index++)
    {
      LoadOpenedStoredSessionFolders(
        SessionTree->Items->Item[Index], OpenedStoredSessionFolders);
    }

    // tree view tried to make expanded node children all visible, what
    // may scroll the selected node (what should be the first one here),
    // out of the view
    if (SessionTree->Selected != NULL)
    {
      SessionTree->Selected->MakeVisible();
    }
  }
  __finally
  {
    delete OpenedStoredSessionFolders;
  }

  if (!WinConfiguration->LastStoredSession.IsEmpty())
  {
    UnicodeString Path = WinConfiguration->LastStoredSession;

    UnicodeString ParentPath = UnixExtractFilePath(Path);
    TTreeNode * Node;
    if (ParentPath.IsEmpty())
    {
      Node = SessionTree->Items->GetFirstNode();
    }
    else
    {
      TTreeNode * Parent = AddSessionPath(ParentPath, false, false);
      Node = (Parent != NULL) ? Parent->getFirstChild() : NULL;
    }

    if (Node != NULL)
    {
      UnicodeString Name = UnixExtractFileName(Path);
      // actually we cannot distinguish folder and session here
      // (note that we allow folder and session with the same name),
      // this is pending for future improvements
      while ((Node != NULL) && !AnsiSameText(Node->Text, Name))
      {
        Node = Node->getNextSibling();
      }

      if (Node != NULL)
      {
        SessionTree->Selected = Node;
        SessionTree->Selected->MakeVisible();
      }
    }
  }

  if (FLocaleChanging)
  {
    BoundsRect = FSavedBounds;
  }
  else
  {
    RestoreFormSize(CustomWinConfiguration->LoginDialog.WindowSize, this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveConfiguration()
{
  assert(CustomWinConfiguration);
  TTreeNode * Node = SessionTree->Selected;
  if (IsWorkspaceNode(Node))
  {
    WinConfiguration->LastWorkspace = SessionNodePath(Node);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PreferencesActionExecute(TObject * /*Sender*/)
{
  ShowPreferencesDialog(::pmDefault);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PreferencesLoggingActionExecute(TObject * /*Sender*/)
{
  ShowPreferencesDialog(pmLogging);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::MasterPasswordRecrypt(TObject * /*Sender*/)
{
  FNewSiteData->RecryptPasswords();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShowPreferencesDialog(TPreferencesMode PreferencesMode)
{
  assert(CustomWinConfiguration->OnMasterPasswordRecrypt == NULL);
  CustomWinConfiguration->OnMasterPasswordRecrypt = MasterPasswordRecrypt;
  try
  {
    DoPreferencesDialog(PreferencesMode);
  }
  __finally
  {
    assert(CustomWinConfiguration->OnMasterPasswordRecrypt == MasterPasswordRecrypt);
    CustomWinConfiguration->OnMasterPasswordRecrypt = NULL;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ResetNewSessionActionExecute(TObject * /*Sender*/)
{
  Default();
  EditSession();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    if (!FSitesIncrementalSearch.IsEmpty())
    {
      TShiftState Shift = KeyDataToShiftState(Message.KeyData);
      bool Reverse = Shift.Contains(ssShift);
      if (!SitesIncrementalSearch(FSitesIncrementalSearch, true, Reverse))
      {
        MessageBeep(0);
      }
      Message.Result = 1;
      return;
    }
  }
  else if (Message.CharCode == VK_ESCAPE)
  {
    if (!FSitesIncrementalSearch.IsEmpty())
    {
      ResetSitesIncrementalSearch();
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  assert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else if (M->Msg == WM_LOCALE_CHANGE)
  {
    if (M->WParam == 0)
    {
      SaveConfiguration();
      SaveState();
      PersistNewSiteIfNeeded();

      // restore sizes to design-time state,
      // otherwise layout is lost while reloading the form
      BasicGroup->Height = FBasicGroupBaseHeight + BasicSshPanel->Height + BasicFtpPanel->Height;
      SetBounds(Left, Top, FOriginalSize.cx, FOriginalSize.cy);

      assert(FSystemSettings);
      RevokeSystemSettings(this, FSystemSettings);
      FSystemSettings = NULL;

      Hide();
    }
    else
    {
      FLocaleChanging = true;
      try
      {
        Show();
      }
      __finally
      {
        FLocaleChanging = false;
      }
    }
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetDefaultSessionActionExecute(
      TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(SET_DEFAULT_SESSION_SETTINGS), qtConfirmation,
        qaOK | qaCancel, HELP_SESSION_SAVE_DEFAULT) == qaOK)
  {
    std::auto_ptr<TSessionData> SessionData(new TSessionData(L""));
    SaveSession(SessionData.get());
    CustomWinConfiguration->AskForMasterPasswordIfNotSetAndNeededToPersistSessionData(SessionData.get());
    StoredSessions->DefaultSettings = SessionData.get();

    Default();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ToolsMenuButtonClick(TObject * /*Sender*/)
{
  MenuPopup(ToolsPopupMenu, ToolsMenuButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DesktopIconActionExecute(TObject * /*Sender*/)
{
  TTreeNode * Node = SessionTree->Selected;

  UnicodeString Message;
  UnicodeString Name;
  UnicodeString AdditionalParams;
  int IconIndex = 0;
  if (IsSiteNode(Node))
  {
    Name = GetNodeSession(Node)->Name;
    Message = FMTLOAD(CONFIRM_CREATE_SHORTCUT, (Name));
    AdditionalParams = L"/UploadIfAny";
    IconIndex = SITE_ICON;
  }
  else if (IsFolderNode(Node))
  {
    Name = SessionNodePath(SessionTree->Selected);
    Message = FMTLOAD(CONFIRM_CREATE_SHORTCUT_FOLDER, (Name));
    IconIndex = SITE_FOLDER_ICON;
  }
  else if (IsWorkspaceNode(Node))
  {
    Name = SessionNodePath(SessionTree->Selected);
    Message = FMTLOAD(CONFIRM_CREATE_SHORTCUT_WORKSPACE, (Name));
    IconIndex = WORKSPACE_ICON;
  }
  else
  {
    FAIL;
  }

  if (MessageDialog(Message, qtConfirmation, qaYes | qaNo, HELP_CREATE_SHORTCUT) == qaYes)
  {
    CreateDesktopSessionShortCut(Name, L"", AdditionalParams, -1, IconIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SendToHookActionExecute(TObject * /*Sender*/)
{
  assert(IsSiteNode(SessionTree->Selected));
  assert(SelectedSession != NULL);
  if (MessageDialog(FMTLOAD(CONFIRM_CREATE_SENDTO, (SelectedSession->Name)),
        qtConfirmation, qaYes | qaNo, HELP_CREATE_SENDTO) == qaYes)
  {
    CreateDesktopSessionShortCut(SelectedSession->Name,
      FMTLOAD(SESSION_SENDTO_HOOK_NAME, (SelectedSession->LocalName)),
      L"/Upload", CSIDL_SENDTO, SITE_ICON);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::HasNodeAnySession(TTreeNode * Node, bool NeedCanLogin)
{
  bool Result = false;
  TTreeNode * ANode = Node->GetNext();
  while (!Result && (ANode != NULL) && ANode->HasAsParent(Node))
  {
    Result =
      IsSessionNode(ANode) &&
      (!NeedCanLogin || FStoredSessions->CanLogin(GetNodeSession(ANode)));
    ANode = ANode->GetNext();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeCustomDrawItem(
  TCustomTreeView * Sender, TTreeNode * Node, TCustomDrawState State,
  bool & DefaultDraw)
{
  TFontStyles Styles = Sender->Canvas->Font->Style;
  if (IsSessionNode(Node) && GetNodeSession(Node)->Special)
  {
    Styles = Styles << fsBold << fsUnderline;
  }
  else
  {
    Styles = Styles >> fsBold >> fsUnderline;
  }

  if (State.Empty() && !Node->DropTarget)
  {
    if (IsFolderOrWorkspaceNode(Node))
    {
      if (!HasNodeAnySession(Node))
      {
        Sender->Canvas->Font->Color = clGrayText;
      }
    }
    else if (ALWAYS_TRUE(IsSessionNode(Node)))
    {
      TSessionData * Data = GetNodeSession(Node);
      if (Data->Color != 0)
      {
        Sender->Canvas->Brush->Color = (TColor)Data->Color;
      }
    }
  }

  Sender->Canvas->Font->Style = Styles;
  DefaultDraw = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckForUpdatesActionExecute(TObject * /*Sender*/)
{
  CheckForUpdates(false);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    SaveDataList(FDataList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEditing(TObject * /*Sender*/,
  TTreeNode * Node, bool & AllowEdit)
{
  assert(!FRenaming);
  AllowEdit =
    IsFolderOrWorkspaceNode(Node) ||
    (ALWAYS_TRUE(IsSiteNode(Node)) && !GetNodeSession(Node)->Special);
  FRenaming = AllowEdit;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::RenameSessionActionExecute(TObject * /*Sender*/)
{
  if (SessionTree->Selected != NULL)
  {
    // would be more appropriate in SessionTreeEditing, but it does not work there
    ResetSitesIncrementalSearch();
    SessionTree->Selected->EditText();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckDuplicateFolder(TTreeNode * Parent,
  UnicodeString Text, TTreeNode * Node)
{
  TTreeNode * ANode =
    ((Parent == NULL) ? SessionTree->Items->GetFirstNode() :
     Parent->getFirstChild());
  // note that we allow folder with the same name as existing session
  // on the same level (see also AddSession)
  while ((ANode != NULL) &&
    ((ANode == Node) || IsSessionNode(ANode) || !AnsiSameText(ANode->Text, Text)))
  {
    ANode = ANode->getNextSibling();
  }

  if (ANode != NULL)
  {
    throw Exception(FMTLOAD(LOGIN_DUPLICATE_SESSION_FOLDER_WORKSPACE, (Text)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckIsSessionFolder(TTreeNode * Node)
{
  if ((Node != NULL) && (Node->Parent != NULL))
  {
    CheckIsSessionFolder(Node->Parent);
  }

  if (IsWorkspaceNode(Node))
  {
    throw Exception(FMTLOAD(WORKSPACE_NOT_FOLDER, (SessionNodePath(Node))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEdited(TObject * /*Sender*/,
  TTreeNode * Node, UnicodeString & S)
{
  if (Node->Text != S)
  {
    TSessionData * Session = SelectedSession;

    TSessionData::ValidateName(S);
    if (Session != NULL)
    {
      UnicodeString Path = UnixExtractFilePath(Session->Name) + S;

      SessionNameValidate(Path, Session->Name);

      // remove from storage
      Session->Remove();

      TSessionData * NewSession = StoredSessions->NewSession(Path, Session);
      // modified, only explicit
      StoredSessions->Save(false, true);
      // the session may be the same, if only letter case has changed
      if (Session != NewSession)
      {
        // if we overwrite existing session, remove the original item
        // (we must not delete the node we are editing)
        TTreeNode * ANode =
          ((Node->Parent == NULL) ? SessionTree->Items->GetFirstNode() :
           Node->Parent->getFirstChild());
        while ((ANode != NULL) && (ANode->Data != NewSession))
        {
          ANode = ANode->getNextSibling();
        }

        if (ANode != NULL)
        {
          ANode->Delete();
        }

        Node->Data = NewSession;

        DestroySession(Session);
      }
    }
    else
    {
      CheckDuplicateFolder(Node->Parent, S, Node);

      UnicodeString ParentPath = UnixIncludeTrailingBackslash(SessionNodePath(Node->Parent));
      UnicodeString OldRoot = ParentPath + Node->Text;
      UnicodeString NewRoot = ParentPath + S;

      bool AnySession = false;

      TSortType PrevSortType = SessionTree->SortType;
      // temporarily disable automatic sorting, so that nodes are kept in order
      // while we traverse them. otherwise it may happen that we omit some.
      SessionTree->SortType = Comctrls::stNone;
      try
      {
        TTreeNode * ANode = Node->GetNext();
        while ((ANode != NULL) && ANode->HasAsParent(Node))
        {
          if (IsSessionNode(ANode))
          {
            AnySession = true;
            TSessionData * Session = GetNodeSession(ANode);

            // remove from storage
            Session->Remove();

            UnicodeString Path = Session->Name;
            assert(Path.SubString(1, OldRoot.Length()) == OldRoot);
            Path.Delete(1, OldRoot.Length());
            Path.Insert(NewRoot, 1);

            TSessionData * NewSession = StoredSessions->NewSession(Path, Session);

            // the session may be the same, if only letter case has changed
            if (NewSession != Session)
            {
              ANode->Data = NewSession;
              DestroySession(Session);
            }
          }

          ANode = ANode->GetNext();
        }
      }
      __finally
      {
        SessionTree->SortType = PrevSortType;
      }

      if (AnySession)
      {
        // modified, only explicit
        StoredSessions->Save(false, true);
      }
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TLoginDialog::FSProtocolToIndex(TFSProtocol FSProtocol,
  bool & AllowScpFallback)
{
  if (FSProtocol == fsSFTP)
  {
    AllowScpFallback = true;
    bool Dummy;
    return FSProtocolToIndex(fsSFTPonly, Dummy);
  }
  else
  {
    AllowScpFallback = false;
    for (int Index = 0; Index < TransferProtocolCombo->Items->Count; Index++)
    {
      if (FSOrder[Index] == FSProtocol)
      {
        return Index;
      }
    }
    // SFTP is always present
    return FSProtocolToIndex(fsSFTP, AllowScpFallback);
  }
}
//---------------------------------------------------------------------------
TFSProtocol __fastcall TLoginDialog::IndexToFSProtocol(int Index, bool AllowScpFallback)
{
  bool InBounds = (Index >= 0) && (Index < static_cast<int>(LENOF(FSOrder)));
  // can be temporary "unselected" while new language is being loaded
  assert(InBounds || (Index == -1));
  TFSProtocol Result = fsSFTP;
  if (InBounds)
  {
    Result = FSOrder[Index];
    if ((Result == fsSFTPonly) && AllowScpFallback)
    {
      Result = fsSFTP;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TLoginDialog::FtpsToIndex(TFtps Ftps)
{
  switch (Ftps)
  {
    default:
      FAIL;
    case ftpsNone:
      return 0;

    case ftpsImplicit:
      return 1;

    case ftpsExplicitSsl:
      return 3;

    case ftpsExplicitTls:
      return 2;
  }
}
//---------------------------------------------------------------------------
TFtps __fastcall TLoginDialog::GetFtps()
{
  int Index = ((GetFSProtocol(false) == fsWebDAV) ? WebDavsCombo->ItemIndex : FtpsCombo->ItemIndex);
  TFtps Ftps;
  switch (Index)
  {
    default:
      FAIL;
    case 0:
      Ftps = ftpsNone;
      break;

    case 1:
      Ftps = ftpsImplicit;
      break;

    case 2:
      Ftps = ftpsExplicitTls;
      break;

    case 3:
      Ftps = ftpsExplicitSsl;
      break;
  }
  return Ftps;
}
//---------------------------------------------------------------------------
TFSProtocol __fastcall TLoginDialog::GetFSProtocol(bool RequireScpFallbackDistinction)
{
  bool AllowScpFallback = false;
  if (RequireScpFallbackDistinction && ALWAYS_TRUE(FSessionData != NULL))
  {
    FSProtocolToIndex(FSessionData->FSProtocol, AllowScpFallback);
  }
  return IndexToFSProtocol(TransferProtocolCombo->ItemIndex, AllowScpFallback);
}
//---------------------------------------------------------------------------
int __fastcall TLoginDialog::DefaultPort()
{
  TFSProtocol FSProtocol = GetFSProtocol(false);
  TFtps Ftps = GetFtps();
  int Result;
  switch (FSProtocol)
  {
    case fsFTP:
      if (Ftps == ftpsImplicit)
      {
        Result = FtpsImplicitPortNumber;
      }
      else
      {
        Result = FtpPortNumber;
      }
      break;

    case fsWebDAV:
      if (Ftps == ftpsNone)
      {
        Result = HTTPPortNumber;
      }
      else
      {
        Result = HTTPSPortNumber;
      }
      break;

    default:
      if (IsSshProtocol(FSProtocol))
      {
        Result = SshPortNumber;
      }
      else
      {
        FAIL;
        Result = -1;
      }
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::TransferProtocolComboChange(TObject * Sender)
{
  int ADefaultPort = DefaultPort();
  if (!NoUpdate && FUpdatePortWithProtocol)
  {
    NoUpdate++;
    try
    {
      if (PortNumberEdit->AsInteger == FDefaultPort)
      {
        PortNumberEdit->AsInteger = ADefaultPort;
      }
    }
    __finally
    {
      NoUpdate--;
    }
  }
  FDefaultPort = ADefaultPort;
  DataChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NavigationTreeCollapsing(
  TObject * /*Sender*/, TTreeNode * /*Node*/, bool & AllowCollapse)
{
  AllowCollapse = false;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeExpandedCollapsed(TObject * /*Sender*/,
  TTreeNode * Node)
{
  UpdateFolderNode(Node);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeCompare(TObject * /*Sender*/,
  TTreeNode * Node1, TTreeNode * Node2, int /*Data*/, int & Compare)
{
  bool Node1IsNewSite = IsNewSiteNode(Node1);
  bool Node2IsNewSite = IsNewSiteNode(Node2);
  bool Node1IsWorkspace = IsWorkspaceNode(Node1);
  bool Node2IsWorkspace = IsWorkspaceNode(Node2);
  bool Node1IsFolder = IsFolderNode(Node1);
  bool Node2IsFolder = IsFolderNode(Node2);

  if (Node1IsNewSite && !Node2IsNewSite)
  {
    Compare = -1;
  }
  else if (!Node1IsNewSite && Node2IsNewSite)
  {
    Compare = 1;
  }
  else if (Node1IsWorkspace && !Node2IsWorkspace)
  {
    Compare = -1;
  }
  else if (!Node1IsWorkspace && Node2IsWorkspace)
  {
    Compare = 1;
  }
  else if (Node1IsFolder && !Node2IsFolder)
  {
    Compare = -1;
  }
  else if (!Node1IsFolder && Node2IsFolder)
  {
    Compare = 1;
  }
  else if (Node1IsWorkspace || Node1IsFolder)
  {
    Compare = AnsiCompareText(Node1->Text, Node2->Text);
  }
  else
  {
    Compare = NamedObjectSortProc(Node1->Data, Node2->Data);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionFolderInputDialogInitialize(
  TObject * /*Sender*/, TInputDialogData * Data)
{
  TEdit * Edit = Data->Edit;
  int P = Edit->Text.LastDelimiter(L"/");
  if (P > 0)
  {
    Edit->SetFocus();
    Edit->SelStart = P;
    Edit->SelLength = Edit->Text.Length() - P;
  }
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::SessionFolderNode(TTreeNode * Node)
{
  TTreeNode * Parent;
  if (IsSessionNode(Node))
  {
    Parent = Node->Parent;
  }
  else if (IsFolderNode(Node))
  {
    Parent = Node;
  }
  else if (IsWorkspaceNode(Node))
  {
    Parent = NULL;
  }
  else
  {
    assert(Node == NULL);
    Parent = NULL;
  }
  return Parent;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::CurrentSessionFolderNode()
{
  return SessionFolderNode(SessionTree->Selected);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionFolderActionExecute(
  TObject * /*Sender*/)
{
  UnicodeString Name =
    UnixIncludeTrailingBackslash(SessionNodePath(CurrentSessionFolderNode())) +
    LoadStr(NEW_FOLDER);
  if (InputDialog(LoadStr(LOGIN_NEW_SESSION_FOLDER_CAPTION),
        LoadStr(LOGIN_NEW_SESSION_FOLDER_PROMPT), Name, HELP_NEW_SESSION_FOLDER,
        NULL, true, NewSessionFolderInputDialogInitialize))
  {
    Name = UnixExcludeTrailingBackslash(Name);
    if (!Name.IsEmpty())
    {
      TTreeNode * Parent = AddSessionPath(UnixExtractFilePath(Name), true, false);
      // this does not prevent creation of subfolder under workspace,
      // if user creates more levels at once (but it does not show up anyway)
      CheckIsSessionFolder(Parent);
      CheckDuplicateFolder(Parent, UnixExtractFileName(Name), NULL);

      TTreeNode * Node = AddSessionPath(Name, true, false);
      SessionTree->Selected = Node;
      Node->MakeVisible();
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::SessionAllowDrop(TTreeNode * DropTarget)
{
  return
    (SessionTree->Selected != NULL) &&
    (SessionTree->Selected->Parent != DropTarget) &&
    IsFolderNode(DropTarget) || IsSiteNode(DropTarget);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeProc(TMessage & AMessage)
{
  if (AMessage.Msg == CM_DRAG)
  {
    TCMDrag & Message = reinterpret_cast<TCMDrag &>(AMessage);
    // reimplement dmDragMove to avoid TCustomTreeView.DoDragOver,
    // which resets DropTarget to pointed-to node
    // (note that this disables OnDragOver event handler)
    if ((Message.DragMessage == dmDragMove) ||
        (Message.DragMessage == dmDragEnter) ||
        (Message.DragMessage == dmDragLeave))
    {
      if (Message.DragMessage != dmDragMove)
      {
        // must call it at least for dmDragLeave, because it does some cleanup,
        // but we need to override result below, as it defaults to "not accepted"
        FOldSessionTreeProc(AMessage);
      }

      TDragControlObject * DragObject = dynamic_cast<TDragControlObject *>(Message.DragRec->Source);
      if ((DragObject != NULL) && (DragObject->Control == SessionTree))
      {
        TPoint P = SessionTree->ScreenToClient(Message.DragRec->Pos);
        TTreeNode * Node = SessionTree->GetNodeAt(P.x, P.y);
        TTreeNode * DropTarget =
          IsWorkspaceNode(Node) ? Node : SessionFolderNode(Node);
        if (!SessionAllowDrop(DropTarget))
        {
          DropTarget = NULL;
          Message.Result = 0;
        }
        else
        {
          Message.Result = 1;
        }

        if (Message.DragMessage == dmDragMove)
        {
          SessionTree->DropTarget = DropTarget;
        }
        FScrollOnDragOver->DragOver(P);
      }
      else
      {
        Message.Result = 0;
      }
    }
    else
    {
      FOldSessionTreeProc(AMessage);
    }
  }
  else
  {
    FOldSessionTreeProc(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeStartDrag(TObject * /*Sender*/,
  TDragObject *& /*DragObject*/)
{
  assert(SessionTree->Selected != NULL);
  // neither session folders/workspaces, nor special sessions can be dragged
  if ((SessionTree->Selected == NULL) ||
      IsFolderOrWorkspaceNode(SessionTree->Selected) ||
      IsNewSiteNode(SessionTree->Selected) ||
      (IsSiteNode(SessionTree->Selected) && SelectedSession->Special))
  {
    Abort();
  }

  FScrollOnDragOver->StartDrag();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeDragDrop(TObject * Sender,
  TObject * Source, int /*X*/, int /*Y*/)
{
  TTreeNode * DropTarget = SessionTree->DropTarget;
  if (ALWAYS_TRUE((Sender == Source) && SessionAllowDrop(DropTarget)))
  {
    TSessionData * Session = SelectedSession;
    UnicodeString Path =
      UnixIncludeTrailingBackslash(SessionNodePath(DropTarget)) +
      UnixExtractFileName(Session->SessionName);

    SessionNameValidate(Path, Session->SessionName);

    // remove from storage
    Session->Remove();

    TSessionData * NewSession = StoredSessions->NewSession(Path, Session);
    // modified, only explicit
    StoredSessions->Save(false, true);
    // this should aways be the case
    if (ALWAYS_TRUE(Session != NewSession))
    {
      TTreeNode * Node = SessionTree->Selected;

      // look for overwritten node (if any)
      TTreeNode * ANode = SessionTree->Items->GetFirstNode();
      while (ANode != NULL)
      {
        if (ANode->Data == NewSession)
        {
          ANode->Delete();
          break;
        }
        ANode = ANode->GetNext();
      }

      Node->MoveTo(DropTarget, naAddChild);
      Node->Data = NewSession;
      // try to make both visible
      if (DropTarget != NULL)
      {
        DropTarget->MakeVisible();
      }
      Node->MakeVisible();

      DestroySession(Session);
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TLoginDialog::GetFolderOrWorkspaceContents(
  TTreeNode * Node, const UnicodeString & Indent, const UnicodeString & CommonRoot)
{
  UnicodeString Contents;

  UnicodeString Path = SessionNodePath(Node);
  std::auto_ptr<TStrings> Names(FStoredSessions->GetFolderOrWorkspaceList(Path));
  for (int Index = 0; Index < Names->Count; Index++)
  {
    UnicodeString Name = Names->Strings[Index];
    if (StartsStr(CommonRoot, Name))
    {
      Name.Delete(1, CommonRoot.Length());
    }
    Contents += Indent + Name + L"\n";
  }

  return Contents;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeMouseMove(TObject * /*Sender*/,
  TShiftState /*Shift*/, int X, int Y)
{
  TTreeNode * Node = SessionTree->GetNodeAt(X, Y);
  THitTests HitTest = SessionTree->GetHitTestInfoAt(X, Y);

  if (Node != FHintNode)
  {
    Application->CancelHint();

    UnicodeString Hint;
    if (HitTest.Contains(htOnItem) || HitTest.Contains(htOnIcon) ||
        HitTest.Contains(htOnLabel) || HitTest.Contains(htOnStateIcon))
    {
      FHintNode = Node;
      if (IsSiteNode(Node))
      {
        Hint = GetNodeSession(Node)->InfoTip;
      }
      else if (IsWorkspaceNode(Node))
      {
        UnicodeString Path = SessionNodePath(Node);
        Hint =
          FMTLOAD(WORKSPACE_INFO_TIP, (Path)) + L"\n" +
          // trim the trailing new line
          TrimRight(GetFolderOrWorkspaceContents(Node, L"  ", UnicodeString()));
      }
      else
      {
        Hint = L"";
      }
    }
    else
    {
      FHintNode = NULL;
      Hint = L"";
    }

    SessionTree->Hint = Hint;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEndDrag(TObject * /*Sender*/,
  TObject * /*Target*/, int /*X*/, int /*Y*/)
{
  FScrollOnDragOver->EndDrag();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AnonymousLoginCheckClick(TObject * /*Sender*/)
{
  if (AnonymousLoginCheck->Checked)
  {
    UserNameEdit->Text = AnonymousUserName;
    PasswordEdit->Text = AnonymousPassword;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveButtonDropDownClick(TObject * /*Sender*/)
{
  MenuPopup(SaveDropDownMenu, SaveButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeExpanding(TObject * /*Sender*/,
  TTreeNode * Node, bool & AllowExpansion)
{
  // to prevent workspace expansion
  AllowExpansion = IsFolderNode(Node);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ExecuteTool(const UnicodeString & Name)
{
  UnicodeString Path;
  if (!FindTool(Name, Path) ||
      !ExecuteShell(Path, L""))
  {
    throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Name)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::RunPageantActionExecute(TObject * /*Sender*/)
{
  ExecuteTool(PageantTool);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::RunPuttygenActionExecute(TObject * /*Sender*/)
{
  ExecuteTool(PuttygenTool);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PortNumberEditChange(TObject * Sender)
{
  if (!NoUpdate)
  {
    bool WellKnownPort = false;
    TFSProtocol FSProtocol;
    TFtps Ftps = ftpsNone;

    int PortNumber = PortNumberEdit->AsInteger;
    if (PortNumber == SshPortNumber)
    {
      FSProtocol = fsSFTP;
      WellKnownPort = true;
    }
    else if (PortNumber == FtpPortNumber)
    {
      FSProtocol = fsFTP;
      WellKnownPort = true;
    }
    else if (PortNumber == FtpsImplicitPortNumber)
    {
      FSProtocol = fsFTP;
      Ftps = ftpsImplicit;
      WellKnownPort = true;
    }
    else if (PortNumber == HTTPPortNumber)
    {
      FSProtocol = fsWebDAV;
      WellKnownPort = true;
    }
    else if (PortNumber == HTTPSPortNumber)
    {
      FSProtocol = fsWebDAV;
      Ftps = ftpsImplicit;
      WellKnownPort = true;
    }

    if (WellKnownPort)
    {
      bool AllowScpFallback;
      int ProtocolIndex = FSProtocolToIndex(FSProtocol, AllowScpFallback);
      if ((TransferProtocolCombo->ItemIndex == ProtocolIndex) &&
          (GetFtps() == Ftps))
      {
        FUpdatePortWithProtocol = true;
      }
      else
      {
        FUpdatePortWithProtocol = false;

        NoUpdate++;
        try
        {
          TransferProtocolCombo->ItemIndex = ProtocolIndex;

          int FtpsIndex = FtpsToIndex(Ftps);
          FtpsCombo->ItemIndex = FtpsIndex;
          WebDavsCombo->ItemIndex = FtpsIndex;
        }
        __finally
        {
          NoUpdate--;
        }
      }
    }
  }

  DataChange(Sender);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TLoginDialog::ImportExportIniFilePath()
{
  UnicodeString PersonalDirectory;
  ::SpecialFolderLocation(CSIDL_PERSONAL, PersonalDirectory);
  UnicodeString FileName = IncludeTrailingBackslash(PersonalDirectory) +
    ExtractFileName(ExpandEnvironmentVariables(Configuration->IniFileStorageName));
  return FileName;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ExportActionExecute(TObject * /*Sender*/)
{
  UnicodeString FileName = ImportExportIniFilePath();
  if (SaveDialog(LoadStr(EXPORT_CONF_TITLE), LoadStr(EXPORT_CONF_FILTER), L"ini", FileName))
  {
    Configuration->Export(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(IMPORT_CONFIGURATION),
        qtWarning, qaOK | qaCancel, HELP_IMPORT_CONFIGURATION) == qaOK)
  {
    std::auto_ptr<TOpenDialog> OpenDialog(new TOpenDialog(Application));
    OpenDialog->Title = LoadStr(IMPORT_CONF_TITLE);
    OpenDialog->Filter = LoadStr(EXPORT_CONF_FILTER);
    OpenDialog->DefaultExt = L"ini";
    OpenDialog->FileName = ImportExportIniFilePath();

    if (OpenDialog->Execute())
    {
      // before the session list gets destroyed
      SessionTree->Items->Clear();
      Configuration->Import(OpenDialog->FileName);
      ReloadSessions();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ResetSitesIncrementalSearch()
{
  FSitesIncrementalSearch = L"";
  // this is to prevent active tree node being set back to Sites tab
  // (from UpdateNavigationTree) when we are called from SessionTreeExit,
  // while tab is changing
  if (NoUpdate == 0)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::SitesIncrementalSearch(const UnicodeString & Text,
  bool SkipCurrent, bool Reverse)
{
  TTreeNode * Node = SearchSite(Text, false, SkipCurrent, Reverse);
  if (Node == NULL)
  {
    Node = SearchSite(Text, true, SkipCurrent, Reverse);
    if (Node != NULL)
    {
      TTreeNode * Parent = Node->Parent;
      while (Parent != NULL)
      {
        Parent->Expand(false);
        Parent = Parent->Parent;
      }
    }
  }

  bool Result = (Node != NULL);
  if (Result)
  {
    {
      TAutoNestingCounter Guard(FIncrementalSearching);
      SessionTree->Selected = Node;
    }
    FSitesIncrementalSearch = Text;

    // Tab always searches even in collapsed nodes
    TTreeNode * NextNode = SearchSite(Text, true, true, Reverse);
    FSitesIncrementalSearchHaveNext =
      (NextNode != NULL) && (NextNode != Node);

    UpdateControls();

    // make visible only after search panel is shown, what may obscure the node
    Node->MakeVisible();
  }
  return Result;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::GetNextNode(TTreeNode * Node, bool Reverse)
{
  if (Reverse)
  {
    Node = Node->GetPrev();
    if (Node == NULL)
    {
      // GetLastNode
      // http://stackoverflow.com/questions/6257348/how-should-i-implement-getlastnode-for-ttreenodes
      Node = SessionTree->Items->GetFirstNode();
      TTreeNode * Node2 = Node;
      if (Node2 != NULL)
      {
        do
        {
          Node2 = Node;
          if (Node != NULL)
          {
            Node = Node2->getNextSibling();
          }
          if (Node == NULL)
          {
            Node = Node2->getFirstChild();
          }
        }
        while (Node != NULL);
      }

      Node = Node2;
    }
  }
  else
  {
    Node = Node->GetNext();
    if (Node == NULL)
    {
      Node = SessionTree->Items->GetFirstNode();
    }
  }

  return Node;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::SearchSite(const UnicodeString & Text,
  bool AllowExpanding, bool SkipCurrent, bool Reverse)
{
  TTreeNode * CurrentNode =
    (SessionTree->Selected != NULL) ? SessionTree->Selected : SessionTree->Items->GetFirstNode();
  if (CurrentNode == NULL)
  {
    return NULL;
  }
  else
  {
    TTreeNode * Node = CurrentNode;
    if (SkipCurrent)
    {
      Node = GetNextNode(Node, Reverse);
      if (Node == NULL)
      {
        return NULL;
      }
    }

    while (true)
    {
      bool Eligible = true;
      TTreeNode * Parent = Node->Parent;
      while (Eligible && (Parent != NULL))
      {
        Eligible =
          IsFolderNode(Parent) &&
          (Parent->Expanded || AllowExpanding);
        Parent = Parent->Parent;
      }
      if (Eligible && ContainsText(Node->Text, Text))
      {
        return Node;
      }

      Node = GetNextNode(Node, Reverse);

      if (Node == CurrentNode)
      {
        return NULL;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeExit(TObject * /*Sender*/)
{
  ResetSitesIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeChanging(TObject * /*Sender*/,
  TTreeNode * /*Node*/, bool & AllowChange)
{
  if (FEditing &&
      (MessageDialog(LoadStr(LOGIN_CANCEL_EDITING), qtConfirmation, qaOK | qaCancel) == qaCancel))
  {
    AllowChange = false;
  }
  else
  {
    PersistNewSiteIfNeeded();
    FEditing = false;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PersistNewSiteIfNeeded()
{
  // Visible: Never try to save data if we did not load any yet,
  // otherwise we get "HostNameEdit" and such
  if (Visible && IsNewSiteNode(SessionTree->Selected))
  {
    SaveSession(FNewSiteData);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionAdvancedActionExecute(TObject * /*Sender*/)
{
  // If we ever allow showing advanced settings, while read-only,
  // we must make sure that FSessionData actually holds the advanced settings,
  // what is currently does not, in order to avoid master password prompt,
  // while cloning the session data in LoadSession.
  // To implement this, we may delegate the cloning to TWinConfiguration and
  // make use of FDontDecryptPasswords
  if (ALWAYS_TRUE(FSessionData != NULL))
  {
    SaveSession(FSessionData);
    DoSiteAdvancedDialog(FSessionData, FOptions);
  }
}
//---------------------------------------------------------------------------
TPopupMenu * __fastcall TLoginDialog::GetSelectedNodePopupMenu()
{
  TPopupMenu * PopupMenu = NULL;

  TTreeNode * Selected = SessionTree->Selected;
  if (IsNewSiteNode(Selected))
  {
    PopupMenu = ManageNewSitePopupMenu;
  }
  else if (IsSiteNode(Selected))
  {
    PopupMenu = ManageSitePopupMenu;
  }
  else if (IsFolderNode(Selected))
  {
    PopupMenu = ManageFolderPopupMenu;
  }
  else if (IsWorkspaceNode(Selected))
  {
    PopupMenu = ManageWorkspacePopupMenu;
  }

  return NOT_NULL(PopupMenu);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ManageButtonClick(TObject * /*Sender*/)
{
  MenuPopup(GetSelectedNodePopupMenu(), ManageButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeMouseDown(TObject * /*Sender*/,
  TMouseButton Button, TShiftState /*Shift*/, int X, int Y)
{
  TTreeNode * Node = SessionTree->GetNodeAt(X, Y);
  if ((Button == mbRight) && (Node != NULL))
  {
    SessionTree->Selected = Node;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeContextPopup(TObject * /*Sender*/,
  TPoint & MousePos, bool & Handled)
{
  TTreeNode * Node = SessionTree->GetNodeAt(MousePos.X, MousePos.Y);
  // This is mostly to prevent context menu from poping up,
  // while there is prompt to confirm cancelling session edit,
  // when right mouse is clicked on non-selected node
  if (Node != SessionTree->Selected)
  {
    Handled = true;
  }
  else
  {
    if (SessionTree->Selected != NULL)
    {
      SessionTree->PopupMenu = GetSelectedNodePopupMenu();
      if (NOT_NULL(SessionTree->PopupMenu))
      {
        MenuPopup(SessionTree, MousePos, Handled);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditCancelActionExecute(TObject * /*Sender*/)
{
  FEditing = false;
  // reset back to saved settings
  LoadContents();
  UpdateControls();
  // we do not want to see blinking cursor in read-only edit box
  SessionTree->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AdvancedButtonDropDownClick(TObject * /*Sender*/)
{
  MenuPopup(SessionAdvancedPopupMenu, AdvancedButton);
}
//---------------------------------------------------------------------------
