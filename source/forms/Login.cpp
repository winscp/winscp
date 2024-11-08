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
#include <limits>

#include "WinInterface.h"
#include "Login.h"
#include "GUITools.h"
#include "Tools.h"
#include "Setup.h"
#include "WinConfiguration.h"
#include "ProgParams.h"
#include "WinApi.h"
#include "S3FileSystem.h"
//---------------------------------------------------------------------
#pragma link "ComboEdit"
#pragma link "PasswordEdit"
#pragma link "UpDownEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const int SiteImageIndex = 1;
const int OpenedFolderImageIndex = 2;
const int ClosedFolderImageIndex = 3;
const int WorkspaceImageIndex = 4;
const int NewSiteImageIndex = 6;
const int SiteColorMaskImageIndex = 8;
//---------------------------------------------------------------------------
const int LoginImageIndex = 0;
const int OpenWorkspaceImageIndex = 5;
const int OpenFolderImageIndex = 6;
//---------------------------------------------------------------------------
bool __fastcall DoLoginDialog(TList * DataList, TForm * LinkedForm)
{
  DebugAssert(DataList != NULL);
  TLoginDialog * LoginDialog = SafeFormCreate<TLoginDialog>();
  bool Result;
  try
  {
    LoginDialog->Init(LinkedForm);
    Result = LoginDialog->Execute(DataList);
  }
  __finally
  {
    delete LoginDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
static const TFSProtocol FSOrder[] = { fsSFTPonly, fsSCPonly, fsFTP, fsWebDAV, fsS3 };
//---------------------------------------------------------------------
__fastcall TLoginDialog::TLoginDialog(TComponent* AOwner)
        : TForm(AOwner)
{
  FNewSiteData = new TSessionData(L"");
  FInitialized = false;
  FHintNode = NULL;
  FScrollOnDragOver = new TTreeViewScrollOnDragOver(SessionTree, true);
  FDataList = NULL;
  FUpdatePortWithProtocol = true;
  FIncrementalSearching = 0;
  FEditing = false;
  FRenaming = false;
  FNewSiteKeepName = false;
  FForceNewSite = false;
  FLoading = false;
  FSortEnablePending = false;
  FSiteSearch = isName;
  FLinkedForm = NULL;
  FRestoring = false;
  FPrevPos = TPoint(std::numeric_limits<LONG>::min(), std::numeric_limits<LONG>::min());

  // we need to make sure that window procedure is set asap
  // (so that CM_SHOWINGCHANGED handling is applied)
  UseSystemSettingsPre(this);

  FFixedSessionImages = SessionImageList->Count;
  DebugAssert(SiteColorMaskImageIndex == FFixedSessionImages - 1);

  FBasicGroupBaseHeight = BasicGroup->Height - BasicSshPanel->Height - BasicFtpPanel->Height - BasicS3Panel->Height;
  FNoteGroupOffset = NoteGroup->Top - (BasicGroup->Top + BasicGroup->Height);
  FUserNameLabel = UserNameLabel->Caption;
  FPasswordLabel = PasswordLabel->Caption;

  AutoSizeCheckBox(ShowAgainCheck);
  SessionTree->Images = TreeViewImageList(SessionImageList);
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::~TLoginDialog()
{
  delete FScrollOnDragOver;
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
void __fastcall TLoginDialog::Init(TForm * LinkedForm)
{
  FLinkedForm = LinkedForm;
  LoadSessions();
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
  // Items item setter is implemented as deleting and re-adding the item. If we do it for the last item
  // (explicit for FTP, implicit for WebDAV/S3), the ItemIndex is effectively reset to -1.
  // This happens when TLS is set in the default session settings.
  // Also as TransferProtocolComboChange is not triggered it results in currupted state in respect to protocol/tls to port number sync.
  int Index = FtpsCombo->ItemIndex;
  FtpsCombo->Items->Strings[FtpsImplicitIndex] = LoadStr(FTPS_IMPLICIT);
  FtpsCombo->Items->Strings[FtpsToIndex(ftpsExplicitTls)] = LoadStr(FTPS_EXPLICIT);
  FtpsCombo->ItemIndex = Index;
  Index = WebDavsCombo->ItemIndex;
  WebDavsCombo->Items->Strings[FtpsNoneIndex] = FtpsCombo->Items->Strings[FtpsNoneIndex];
  WebDavsCombo->Items->Strings[FtpsImplicitIndex] = FtpsCombo->Items->Strings[FtpsImplicitIndex];
  WebDavsCombo->ItemIndex = Index;

  BasicSshPanel->Top = BasicFtpPanel->Top;
  BasicS3Panel->Top = BasicFtpPanel->Top;

  SitesIncrementalSearchPanel->Left = SessionTree->Left;
  SitesIncrementalSearchPanel->Width = SessionTree->Width;
  SitesIncrementalSearchPanel->Height =
    (2 * SitesIncrementalSearchLabel->Top) + SitesIncrementalSearchLabel->Height - ScaleByTextHeight(SitesIncrementalSearchPanel, 2);
  SitesIncrementalSearchPanel->Top = SessionTree->BoundsRect.Bottom - SitesIncrementalSearchPanel->Height;
  SitesIncrementalSearchPanel->Visible = false;

  ReadOnlyControl(TransferProtocolView);
  ReadOnlyControl(EncryptionView);
  ReadOnlyControl(NoteMemo);

  MenuButton(ToolsMenuButton);
  MenuButton(ManageButton);

  FixButtonImage(LoginButton);

  SelectScaledImageList(SessionImageList);
  SelectScaledImageList(ActionImageList);

  GenerateImages();

  if (SessionTree->Items->Count > 0)
  {
    SetNewSiteNodeLabel();
  }
}
//---------------------------------------------------------------------
void TLoginDialog::UpdateLoginButton()
{
  TAction * Action = DebugNotNull(dynamic_cast<TAction *>(LoginButton->Action));
  int ImageIndex = Action->ImageIndex;
  if (FButtonImagesMap.find(ImageIndex) == FButtonImagesMap.end())
  {
    int LoginIndex = AddLoginButtonImage(ImageIndex, true);
    FButtonImagesMap.insert(std::make_pair(ImageIndex, LoginIndex));
    AddLoginButtonImage(ImageIndex, false);
  }

  LoginButton->ImageIndex = FButtonImagesMap[ImageIndex];
  LoginButton->DisabledImageIndex = FButtonImagesMap[ImageIndex] + 1;

  CenterButtonImage(LoginButton);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::GenerateImages()
{
  // Generate button images.
  // The button does not support alpha channel,
  // so we have to copy the PNG's to BMP's and use plain transparent color
  FButtonImageList.reset(new TImageList(this));
  FButtonImageList->SetSize(ActionImageList->Width, ActionImageList->Height);
  LoginButton->Images = FButtonImageList.get();
  FButtonImagesMap.clear();
  UpdateLoginButton();

  SessionImageList->BeginUpdate();
  try
  {
    while (SessionImageList->Count > FFixedSessionImages)
    {
      SessionImageList->Delete(SessionImageList->Count - 1);
    }
    RegenerateSessionColorsImageList(SessionImageList, SiteColorMaskImageIndex);
  }
  __finally
  {
    SessionImageList->EndUpdate();
  }
}
//---------------------------------------------------------------------
void TLoginDialog::FloodFill(TBitmap * Bitmap, int X, int Y)
{
  // A background is white, but there's also white used on the image itself.
  // So we first replace the background white with a unique color,
  // setting it as a transparent later.
  // This is obviously a hack specific to this particular image.
  // 16x16 version does not have any background
  if (Bitmap->Canvas->Pixels[X][Y] == clWhite)
  {
    Bitmap->Canvas->FloodFill(X, Y, clWhite, fsSurface);
  }
}
//---------------------------------------------------------------------
int TLoginDialog::AddLoginButtonImage(int Index, bool Enabled)
{
  std::unique_ptr<TBitmap> Bitmap(new TBitmap());
  Bitmap->SetSize(ActionImageList->Width, ActionImageList->Height);

  ActionImageList->Draw(Bitmap->Canvas, 0, 0, Index, Enabled);

  const TColor TransparentColor = clFuchsia;

  Bitmap->Canvas->Brush->Color = TransparentColor;
  FloodFill(Bitmap.get(), 0, 0);
  FloodFill(Bitmap.get(), Bitmap->Width - 1, Bitmap->Height - 1);
  FloodFill(Bitmap.get(), Bitmap->Width - 1, 0);

  return FButtonImageList->AddMasked(Bitmap.get(), TransparentColor);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init()
{
  FInitialized = true;
  UseSystemSettingsPost(this);

  Caption = FormatFormCaption(this, Caption);

  InitControls();

  ReadOnlyControl(ContentsNameEdit);
  ReadOnlyControl(ContentsMemo);

  if (DebugAlwaysFalse(SessionTree->Items->Count == 0) ||
      ((SessionTree->Items->Count == 1) &&
       DebugAlwaysTrue(IsNewSiteNode(SessionTree->Items->GetFirstNode()))) ||
      FForceNewSite)
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
    UnicodeString Folder = CutToChar(Path, L'/', false);
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
            SetNodeImage(Parent, WorkspaceImageIndex);
          }
          else
          {
            UpdateFolderNode(Parent);
          }
        }
        // optimization
        if (!FLoading)
        {
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
  return (Node != NULL) && (Node->Data == NULL) && (Node->ImageIndex != WorkspaceImageIndex);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsWorkspaceNode(TTreeNode * Node)
{
  return (Node != NULL) && (Node->Data == NULL) && (Node->ImageIndex == WorkspaceImageIndex);
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
  return DebugNotNull(static_cast<TSessionData *>(Node->Data));
}
//---------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::AddSession(TSessionData * Data)
{
  TTreeNode * Parent = AddSessionPath(UnixExtractFilePath(Data->Name), true, Data->IsWorkspace);
  TTreeNode * Node = SessionTree->Items->AddChild(Parent, UnixExtractFileName(Data->Name));
  Node->Data = Data;
  UpdateNodeImage(Node);

  return Node;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateNodeImage(TTreeNode * Node)
{
  SetNodeImage(Node, GetSessionImageIndex(GetNodeSession(Node)));
}
//---------------------------------------------------------------------
int __fastcall TLoginDialog::GetSessionImageIndex(TSessionData * Data)
{
  int Result;
  if (Data->Color != 0)
  {
    Result = GetSessionColorImage(SessionTree->Images, static_cast<TColor>(Data->Color), SiteColorMaskImageIndex);
  }
  else
  {
    Result = SiteImageIndex;
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SetNodeImage(TTreeNode * Node, int ImageIndex)
{
  Node->ImageIndex = ImageIndex;
  Node->SelectedIndex = ImageIndex;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::DestroySession(TSessionData * Data)
{
  StoredSessions->Remove(Data);
}
//---------------------------------------------------------------------
TTreeNode * __fastcall TLoginDialog::GetNewSiteNode()
{
  TTreeNode * Result = SessionTree->Items->GetFirstNode();
  DebugAssert(IsNewSiteNode(Result));
  return Result;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SetNewSiteNodeLabel()
{
  GetNewSiteNode()->Text = LoadStr(LOGIN_NEW_SITE_NODE);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessions()
{
  {
    // Otherwise, once the selected node is deleted, another node is selected and we get failure
    // while trying to access its data somewhere in LoadContents
    TAutoFlag LoadingFlag(FLoading);
    SessionTree->Items->BeginUpdate();
    SessionTree->Images->BeginUpdate();
    try
    {
      // optimization
      SessionTree->SortType = Comctrls::stNone;

      SessionTree->Items->Clear();

      TTreeNode * Node = SessionTree->Items->AddChild(NULL, L"");
      Node->Data = FNewSiteData;
      SetNewSiteNodeLabel();
      SetNodeImage(Node, NewSiteImageIndex);

      DebugAssert(StoredSessions != NULL);
      for (int Index = 0; Index < StoredSessions->Count; Index++)
      {
        AddSession(StoredSessions->Sessions[Index]);
      }
    }
    __finally
    {
      // Restore sorting. Moreover, folders would not be sorted automatically even when
      // SortType is set (not having set the data property), so we would have to
      // call AlphaSort here explicitly
      SessionTree->SortType = Comctrls::stBoth;
      SessionTree->Images->EndUpdate();
      SessionTree->Items->EndUpdate();
    }
  }
  SessionTree->Selected = SessionTree->Items->GetFirstNode();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateFolderNode(TTreeNode * Node)
{
  DebugAssert((Node->ImageIndex == 0) ||
    (Node->ImageIndex == OpenedFolderImageIndex) || (Node->ImageIndex == ClosedFolderImageIndex));
  SetNodeImage(Node, (Node->Expanded ? OpenedFolderImageIndex : ClosedFolderImageIndex));
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSite()
{
  TTreeNode * NewSiteNode = GetNewSiteNode();
  if (DebugAlwaysTrue(IsNewSiteNode(NewSiteNode)))
  {
    SessionTree->Selected = NewSiteNode;
  }

  LoadContents();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ResetNewSiteData()
{
  if (DebugAlwaysTrue(StoredSessions != NULL))
  {
    FNewSiteData->CopyData(StoredSessions->DefaultSettings);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Default()
{
  ResetNewSiteData();

  NewSite();
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
  else if (DebugAlwaysTrue(IsFolderOrWorkspaceNode(Node)))
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
    else if (DebugAlwaysTrue(IsWorkspaceNode(Node)))
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
    PortNumberEdit->AsInteger = SessionData->PortNumber;

    int FtpsIndex = FtpsToIndex(SessionData->Ftps);
    FtpsCombo->ItemIndex = FtpsIndex;
    WebDavsCombo->ItemIndex = FtpsIndex;
    EncryptionView->Text =
      DebugAlwaysTrue(FtpsCombo->ItemIndex >= WebDavsCombo->ItemIndex) ? FtpsCombo->Text : WebDavsCombo->Text;

    bool AllowScpFallback;
    TransferProtocolCombo->ItemIndex = FSProtocolToIndex(SessionData->FSProtocol, AllowScpFallback);
    TransferProtocolView->Text = TransferProtocolCombo->Text;

    // Only after loading TransferProtocolCombo, so that we do not overwrite it with S3 defaults in TransferProtocolComboChange
    HostNameEdit->Text = SessionData->HostName;
    UserNameEdit->Text = SessionData->UserName;

    bool Editable = IsEditable();
    if (Editable)
    {
      PasswordEdit->Text = NormalizeString(SessionData->Password);
    }
    else
    {
      PasswordEdit->Text =
        SessionData->HasPassword() ?
          UnicodeString::StringOfChar(L'?', 16) : UnicodeString();
    }

    S3CredentialsEnvCheck3->Checked = SessionData->S3CredentialsEnv;
    S3ProfileCombo->Text = DefaultStr(SessionData->S3Profile, GetS3GeneralName());
    UpdateS3Credentials();

    NoteGroup->Visible = !Trim(SessionData->Note).IsEmpty();
    NoteMemo->Lines->Text = SessionData->Note;

    // just in case TransferProtocolComboChange is not triggered
    FDefaultPort = DefaultPort();
    FUpdatePortWithProtocol = true;

    if (SessionData != FSessionData)
    {
      // advanced
      InvalidateSessionData();
      // clone advanced settings only when really needed,
      // see also note in SessionAdvancedActionExecute
      if (Editable)
      {
        FSessionData = new TSessionData(L"");
        FSessionData->Assign(SessionData);
      }
    }
    else
    {
      // we should get here only when called from SessionAdvancedActionExecute
      DebugAssert(Editable);
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
  if (DebugAlwaysTrue(FSessionData != NULL))
  {
    SessionData->Assign(FSessionData);
  }

  SessionData->FSProtocol =
    // requiring SCP fallback distinction
    GetFSProtocol(true);

  if (SessionData->FSProtocol == fsS3)
  {
    SessionData->S3CredentialsEnv = S3CredentialsEnvCheck3->Checked;
    SessionData->S3Profile = GetS3Profile();
  }

  if (SessionData->HasAutoCredentials())
  {
    SessionData->UserName = UnicodeString();
    SessionData->Password = UnicodeString();
    SessionData->S3SessionToken = UnicodeString();
  }
  else
  {
    SessionData->UserName = UserNameEdit->Text.Trim();
    SessionData->Password = PasswordEdit->Text;
  }

  SessionData->PortNumber = PortNumberEdit->AsInteger;
  // Must be set after UserName, because HostName may be in format user@host,
  // Though now we parse the hostname right on this dialog (see HostNameEditExit), this is unlikely to ever be triggered.
  SessionData->HostName = HostNameEdit->Text.Trim();
  SessionData->Ftps = GetFtps();

  TSessionData * EditingSessionData = GetEditingSessionData();
  SessionData->Name =
    (EditingSessionData != NULL) ? EditingSessionData->Name :
      (FNewSiteKeepName ? SessionData->Name : SessionData->DefaultSessionName);
}
//---------------------------------------------------------------------
bool __fastcall TLoginDialog::IsEditable()
{
  return IsNewSiteNode(SessionTree->Selected) || FEditing;
}
//---------------------------------------------------------------------
UnicodeString TLoginDialog::GetS3GeneralName()
{
  return LoadStr(LOGIN_S3_GENERAL_CREDENTIALS);
}
//---------------------------------------------------------------------
UnicodeString TLoginDialog::GetS3Profile()
{
  UnicodeString Result;
  if (S3ProfileCombo->Text != GetS3GeneralName())
  {
    Result = S3ProfileCombo->Text;
  }
  return Result;
}
//---------------------------------------------------------------------
void TLoginDialog::LoadS3Profiles()
{
  std::unique_ptr<TStringList> Items(new TStringList());
  std::unique_ptr<TStrings> Profiles(GetS3Profiles());
  Items->AddStrings(Profiles.get());
  Items->Sort();
  Items->Insert(0, GetS3GeneralName());
  S3ProfileCombo->Items = Items.get();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateControls()
{
  if (Visible && FInitialized)
  {
    bool Editable = IsEditable();

    TFSProtocol FSProtocol = GetFSProtocol(false);
    bool SshProtocol = IsSshProtocol(FSProtocol);
    bool FtpProtocol = (FSProtocol == fsFTP);
    bool WebDavProtocol = (FSProtocol == fsWebDAV);
    bool S3Protocol = (FSProtocol == fsS3);

    // session
    FtpsCombo->Visible = Editable && FtpProtocol;
    FtpsLabel->Visible = FtpProtocol;
    WebDavsCombo->Visible = Editable && (WebDavProtocol || S3Protocol);
    WebDavsLabel->Visible = WebDavProtocol || S3Protocol;
    EncryptionView->Visible = !Editable && (FtpProtocol || WebDavProtocol || S3Protocol);

    BasicSshPanel->Visible = SshProtocol;
    BasicFtpPanel->Visible = FtpProtocol && Editable;
    BasicS3Panel->Visible = S3Protocol && Editable;
    if (BasicS3Panel->Visible && (S3ProfileCombo->Items->Count == 0))
    {
      LoadS3Profiles();
    }
    // we do not support more than one at the same time
    DebugAssert((int(BasicSshPanel->Visible) + int(BasicFtpPanel->Visible) + int(BasicS3Panel->Visible)) <= 1);
    BasicGroup->Height =
      FBasicGroupBaseHeight +
      (BasicSshPanel->Visible ? BasicSshPanel->Height : 0) +
      (BasicFtpPanel->Visible ? BasicFtpPanel->Height : 0) +
      (BasicS3Panel->Visible ? BasicS3Panel->Height : 0);
    int NoteGroupTop = (BasicGroup->Top + BasicGroup->Height) + FNoteGroupOffset;
    NoteGroup->SetBounds(
      NoteGroup->Left, (BasicGroup->Top + BasicGroup->Height) + FNoteGroupOffset,
      NoteGroup->Width, NoteGroup->Top + NoteGroup->Height - NoteGroupTop);
    AnonymousLoginCheck->Checked =
      SameText(UserNameEdit->Text, AnonymousUserName) &&
      SameText(PasswordEdit->Text, AnonymousPassword);
    TransferProtocolCombo->Visible = Editable;
    TransferProtocolView->Visible = !TransferProtocolCombo->Visible;
    ReadOnlyControl(HostNameEdit, !Editable);
    ReadOnlyControl(PortNumberEdit, !Editable);
    PortNumberEdit->ButtonsVisible = Editable;
    // FSessionData may be NULL temporary even when Editable while switching nodes
    bool S3CredentialsEnv = S3Protocol && S3CredentialsEnvCheck3->Checked;
    bool NoAuth =
      Editable && (FSessionData != NULL) &&
      ((SshProtocol && FSessionData->SshNoUserAuth) ||
       S3CredentialsEnv);
    ReadOnlyAndEnabledControl(UserNameEdit, !Editable, !NoAuth);
    EnableControl(UserNameLabel, UserNameEdit->Enabled);
    ReadOnlyAndEnabledControl(PasswordEdit, !Editable, !NoAuth);
    EnableControl(PasswordLabel, PasswordEdit->Enabled);
    UserNameLabel->Caption = S3Protocol ? LoadStr(S3_ACCESS_KEY_ID_PROMPT) : FUserNameLabel;
    PasswordLabel->Caption = S3Protocol ? LoadStr(S3_SECRET_ACCESS_KEY_PROMPT) : FPasswordLabel;
    EnableControl(S3ProfileCombo, S3CredentialsEnv);

    // sites
    if (SitesIncrementalSearchPanel->Visible != FIncrementalSearchState.Searching)
    {
      if (!FIncrementalSearchState.Searching)
      {
        SitesIncrementalSearchPanel->Visible = false;
        SessionTree->Height = SitesIncrementalSearchPanel->BoundsRect.Bottom - SessionTree->Top;
      }
      else
      {
        SitesIncrementalSearchPanel->Visible = true;
        SessionTree->Height = SitesIncrementalSearchPanel->BoundsRect.Top - SessionTree->Top + 1;
      }
    }

    if (FIncrementalSearchState.Searching)
    {
      SitesIncrementalSearchLabel->Caption = FormatIncrementalSearchStatus(FIncrementalSearchState);
    }

    EnableControl(ManageButton, !FEditing);
    EnableControl(ToolsMenuButton, !FEditing);
    EnableControl(CloseButton, !FEditing);

    DefaultButton(LoginButton, !FEditing && !FRenaming && !IsCloneToNewSiteDefault());
    CloseButton->Cancel = !FEditing && !FRenaming;
    DefaultButton(SaveButton, FEditing);
    EditCancelButton->Cancel = FEditing;
    SiteClonetoNewSiteMenuItem->Default = IsCloneToNewSiteDefault();
    SiteLoginMenuItem->Default = LoginButton->Default;

    UpdateButtonVisibility(SaveButton);
    UpdateButtonVisibility(EditButton);
    UpdateButtonVisibility(EditCancelButton);

    SaveAsSessionMenuItem->Visible = FEditing;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateButtonVisibility(TButton * Button)
{
  TAction * Action = DebugNotNull(dynamic_cast<TAction *>(Button->Action));
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
  // this is called twice on startup, first with ControlState = [csRecreating]
  // we should probably filter this out, it would avoid need for explicit
  // LoadContents call below
  bool NeedInitialize = !FInitialized;
  if (NeedInitialize)
  {
    Init();
  }

  // Bit of a hack: Assume an auto open, when we are linked to the main form
  ShowAgainPanel->Visible = (FLinkedForm != NULL);

  // among other this makes the expanded nodes look like expanded,
  // because the LoadState call in Execute would be too early,
  // and some stray call to collapsed event during showing process,
  // make the image be set to collapsed.
  // Also LoadState calls RestoreFormSize that has to be
  // called only after DoFormWindowProc(CM_SHOWINGCHANGED).
  // See also comment about MakeVisible in LoadState().
  LoadState();
  if (NeedInitialize)
  {
    // Need to load contents only after state (as that selects initial node).
    // Explicit call is needed, as we get here during csRecreating phase,
    // when SessionTreeChange is not triggered, see initial method comment
    LoadContents();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeChange(TObject * /*Sender*/,
  TTreeNode * /*Node*/)
{
  if (!FLoading)
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

  // This may be false, when collapsed folder was double-clicked,
  // it got expanded, view was shifted to accommodate folder contents,
  // so that cursor now points to a different node (site).
  // This has to be evaluated before EnsureNotEditing,
  // as that may pop-up modal box.
  if (Node == SessionTree->Selected)
  {
    // EnsureNotEditing must be before CanOpen, as CanOpen checks for FEditing
    if (EnsureNotEditing())
    {
      if (IsCloneToNewSiteDefault())
      {
        CloneToNewSite();
      }
      // this can hardly be false
      // (after editing and clone tests above)
      // (except for empty folders, but those do not pass a condition below)
      else if (CanOpen())
      {
        if (IsSessionNode(Node) || IsWorkspaceNode(Node))
        {
          Login();
        }
      }
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
      if (!FIncrementalSearchState.Searching)
      {
        Configuration->Usage->Inc(L"SiteIncrementalSearches");
      }
      if (!SitesIncrementalSearch(FIncrementalSearchState.Text + Key, false, false, false))
      {
        MessageBeep(MB_ICONHAND);
      }
      Key = 0;
    }
    else if (Key == VK_BACK)
    {
      if (FIncrementalSearchState.Searching)
      {
        if (FIncrementalSearchState.Text.Length() <= 1)
        {
          ResetSitesIncrementalSearch();
        }
        else
        {
          UnicodeString NewText = LeftStr(FIncrementalSearchState.Text, FIncrementalSearchState.Text.Length() - 1);
          SitesIncrementalSearch(NewText, false, false, false);
        }
        Key = 0;
      }
    }
    else if ((Key == VK_RETURN) && IsCloneToNewSiteDefault())
    {
      CloneToNewSite();
      Key = 0;
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
  if (DebugAlwaysTrue(SelectedSession != NULL))
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
  return FEditing ? DebugNotNull(SelectedSession) : NULL;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveAsSession(bool ForceDialog)
{
  // Parse hostname before saving
  // (HostNameEditExit is not triggered when child dialog pops up when it is invoked by accelerator)
  // We should better handle this automaticaly when focus is moved to another dialog.
  ParseHostName();

  std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));
  SaveSession(SessionData.get());

  TSessionData * EditingSessionData = GetEditingSessionData();

  // collect list of empty folders (these are not persistent and known to login dialog only)
  std::unique_ptr<TStrings> NewFolders(new TStringList());
  TTreeNode * Node = SessionTree->Items->GetFirstNode();
  while (Node != NULL)
  {
    if (IsFolderNode(Node) && !Node->HasChildren)
    {
      NewFolders->Add(SessionNodePath(Node));
    }
    Node = Node->GetNext();
  }

  TSessionData * NewSession =
    DoSaveSession(SessionData.get(), EditingSessionData, ForceDialog, NewFolders.get());
  if (NewSession != NULL)
  {
    TTreeNode * ParentNode = AddSessionPath(UnixExtractFilePath(NewSession->SessionName), false, false);
    CheckIsSessionFolder(ParentNode);

    TTreeNode * Node = FindSessionNode(NewSession, false);

    if (Node == NULL)
    {
      Node = AddSession(NewSession);
    }

    if ((SessionTree->Selected != Node) &&
        IsSiteNode(SessionTree->Selected))
    {
      CancelEditing();
    }
    else
    {
      FEditing = false;
    }

    SessionTree->Selected = Node;
    SessionTree->SetFocus();

    // this
    // - updates TransferProtocolView and EncryptionView
    // - clears the password box, if user has not opted to save password
    // - reloads fake password
    LoadContents();

    UpdateControls();

    ResetNewSiteData();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSessionActionExecute(TObject * /*Sender*/)
{
  bool NewSiteSelected = IsNewSiteNode(SessionTree->Selected);
  // for new site, the "save" command is actually "save as"
  SaveAsSession(NewSiteSelected);
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
  DebugAssert(SessionTree->Selected != NULL);

  TMessageParams Params;
  Params.ImageName = L"Delete file";
  TTreeNode * Node = SessionTree->Selected;
  if (IsSiteNode(Node))
  {
    TSessionData * Session = SelectedSession;
    UnicodeString Message = MainInstructions(FMTLOAD(CONFIRM_DELETE_SESSION, (Session->SessionName)));
    if (MessageDialog(Message,
          qtConfirmation, qaOK | qaCancel, HELP_DELETE_SESSION, &Params) == qaOK)
    {
      try
      {
        WinConfiguration->DeleteSessionFromJumpList(Session->SessionName);
      }
      catch (Exception & E)
      {
        ShowExtendedException(&E);
      }
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
        (MessageDialog(MainInstructions(FMTLOAD(Prompt, (Path, Sessions))),
          qtConfirmation, qaOK | qaCancel, HelpKeyword, &Params) == qaOK))
    {
      if (IsWorkspaceNode(Node))
      {
        try
        {
          WinConfiguration->DeleteWorkspaceFromJumpList(Path);
        }
        catch (Exception & E)
        {
          ShowExtendedException(&E);
        }
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
void __fastcall TLoginDialog::ReloadSessions(const UnicodeString & SelectSite)
{
  SaveState();
  if (!SelectSite.IsEmpty())
  {
    WinConfiguration->LastStoredSession = SelectSite;
  }
  LoadSessions();
  LoadState();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportSessionsActionExecute(TObject * /*Sender*/)
{
  std::unique_ptr<TList> Imported(new TList());
  if (DoImportSessionsDialog(Imported.get()) &&
      // Can be empty when imported known_hosts
      (Imported->Count > 0))
  {
    // Focus the first imported session.
    // We should also consider expanding all newly created folders
    UnicodeString SelectSite = static_cast<TSessionData *>(Imported->Items[0])->Name;
    ReloadSessions(SelectSite);

    // Focus the tree with focused imported session(s).
    SessionTree->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CleanUpActionExecute(TObject * /*Sender*/)
{
  if (DoCleanupDialog())
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
  bool SiteSelected = IsSiteNode(SessionTree->Selected);
  bool FolderOrWorkspaceSelected = IsFolderOrWorkspaceNode(SessionTree->Selected);
  bool WorkspaceSelected = IsWorkspaceNode(SessionTree->Selected);

  TAction * Action = DebugNotNull(dynamic_cast<TAction *>(BasicAction));
  bool PrevEnabled = Action->Enabled;
  bool Editable = IsEditable();

  if ((Action == EditSessionAction) ||
      (Action == CloneToNewSiteAction))
  {
    Action->Enabled = SiteSelected && !FEditing;
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
    LoginAction->Enabled = CanOpen();
    LoginAction->Caption = FolderOrWorkspaceSelected ? LoadStr(LOGIN_OPEN) : LoadStr(LOGIN_LOGIN);
    LoginAction->ImageIndex = FolderOrWorkspaceSelected ? (WorkspaceSelected ? OpenWorkspaceImageIndex : OpenFolderImageIndex) : LoginImageIndex;
    UpdateLoginButton();
  }
  else if (Action == PuttyAction)
  {
    TSessionData * Data = GetSessionData();
    Action->Enabled =
      (IsSiteAndCanOpen() && !Data->IsLocalBrowser && !Data->Tunnel) ||
      (IsFolderOrWorkspaceAndCanOpen() && IsFolderNode(SessionTree->Selected));
  }
  else if (Action == SaveSessionAction)
  {
    SaveSessionAction->Enabled = Editable;
  }
  else if (Action == SessionAdvancedAction)
  {
    SessionAdvancedAction->Enabled = Editable;
  }
  else if (Action == SessionRawAction)
  {
    SessionRawAction->Enabled = Editable;
  }
  else if (Action == SaveAsSessionAction)
  {
    // Save as is needed for new site only when !SupportsSplitButton()
    SaveAsSessionAction->Enabled = Editable;
  }
  else if (Action == NewSessionFolderAction)
  {
    NewSessionFolderAction->Enabled = !FEditing;
  }
  else if (Action == PasteUrlAction)
  {
    UnicodeString ClipboardUrl;
    Action->Enabled =
      NonEmptyTextFromClipboard(ClipboardUrl) &&
      StoredSessions->IsUrl(ClipboardUrl);
  }
  else if (Action == GenerateUrlAction2)
  {
    TSessionData * Data = GetSessionData();
    // URL without hostname is pointless
    Action->Enabled = (Data != NULL) && !Data->HostNameExpanded.IsEmpty() && !FEditing;
  }
  else if (Action == CopyParamRuleAction)
  {
    TSessionData * Data = GetSessionData();
    // without hostname it's pointless
    Action->Enabled = (Data != NULL) && !Data->HostNameExpanded.IsEmpty();
  }
  else if (Action == SearchSiteStartAction)
  {
    Action->Enabled = !SessionTree->IsEditing() && !FEditing;
  }
  else if (Action == SearchSiteNameStartOnlyAction)
  {
    Action->Checked = (FSiteSearch == isNameStartOnly);
  }
  else if (Action == SearchSiteNameAction)
  {
    Action->Checked = (FSiteSearch == isName);
  }
  else if (Action == SearchSiteAction)
  {
    Action->Checked = (FSiteSearch == isAll);
  }
  else if (Action == CheckForUpdatesAction)
  {
    Action->Visible = !IsUWP();
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
bool __fastcall TLoginDialog::IsCloneToNewSiteDefault()
{
  return !FEditing && !FRenaming && IsSiteNode(SessionTree->Selected) && !StoredSessions->CanOpen(GetSessionData());
}
//---------------------------------------------------------------------------
bool TLoginDialog::IsSiteAndCanOpen()
{
  TSessionData * Data = GetSessionData();
  return
    ((Data != NULL) && StoredSessions->CanOpen(Data) && !FEditing);
}
//---------------------------------------------------------------------------
bool TLoginDialog::IsFolderOrWorkspaceAndCanOpen()
{
  return IsFolderOrWorkspaceNode(SessionTree->Selected) && HasNodeAnySession(SessionTree->Selected, true);
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::CanOpen()
{
  return
    IsSiteAndCanOpen() ||
    IsFolderOrWorkspaceAndCanOpen();
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
      FNewSiteData->CopyData(SessionData);
      FNewSiteData->Special = false;

      if (!FNewSiteData->IsSameDecrypted(StoredSessions->DefaultSettings))
      {
        // we want to start with new site page
        FForceNewSite = true;
      }

      LoadContents();
    }
  }
  else
  {
    Default();
  }
  // Optimization. List view is recreated while showing the form,
  // causing nodes repopulation and in a consequence a huge number of
  // nodes comparison
  SessionTree->SortType = Comctrls::stNone;
  FSortEnablePending = true;
  // Not calling LoadState here.
  // It's redundant and does not work anyway, see comment in the method.
  int AResult = ShowModal();
  // When CanOpen is false, the DefaultResult() will fail finding a default button.
  bool Result = CanOpen() && (AResult == DefaultResult());
  SaveState();
  if (Result)
  {
    SaveConfiguration();
    // DataList saved already from FormCloseQuery
  }

  if (!ShowAgainCheck->Checked)
  {
    WinConfiguration->ShowLoginWhenNoSession = false;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveDataList(TList * DataList)
{
  // Normally we would call this from Execute,
  // but at that point the windows is already hidden.
  // Cloning session data may pop up master password dialog:
  // - if it happens between closing and destroying login dialog
  //   the next window will appear in background for some reason
  // - and its actually even nicer when master password dialog pops up over
  //   the login dialog

  DataList->Clear();

  TTreeNode * Node = SessionTree->Selected;
  if (IsFolderOrWorkspaceNode(Node))
  {
    UnicodeString Name = SessionNodePath(Node);

    if (IsWorkspaceNode(Node))
    {
      try
      {
        WinConfiguration->AddWorkspaceToJumpList(Name);
      }
      catch (Exception & E)
      {
        ShowExtendedException(&E);
      }
    }

    StoredSessions->GetFolderOrWorkspace(Name, DataList);
  }
  else
  {
    ParseHostName();
    DataList->Add(CloneSelectedSession());
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::CloneSelectedSession()
{
  TTreeNode * Node = SessionTree->Selected;
  std::unique_ptr<TSessionData> Data2(new TSessionData(L""));
  if (IsSiteNode(Node))
  {
    Data2->Assign(GetNodeSession(Node));
  }
  else if (DebugAlwaysTrue(IsNewSiteNode(Node)))
  {
    TSessionData * Data = GetSessionData();
    Data2->Assign(Data);
    // we carry the name of the edited stored session around while on the dialog,
    // but we do not want it to leave the dialog, so that we can distinguish
    // stored and ad-hoc sessions
    Data2->Name = L"";
  }
  return Data2.release();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveState()
{
  DebugAssert(WinConfiguration != NULL);

  WinConfiguration->BeginUpdate();
  try
  {
    std::unique_ptr<TStringList> OpenedStoredSessionFolders(CreateSortedStringList());
    for (int Index = 0; Index < SessionTree->Items->Count; Index++)
    {
      TTreeNode * Node = SessionTree->Items->Item[Index];
      if (IsFolderNode(Node))
      {
        if (Node->Expanded)
        {
          OpenedStoredSessionFolders->Add(SessionNodePath(Node));
        }
      }
    }

    WinConfiguration->OpenedStoredSessionFolders = OpenedStoredSessionFolders->CommaText;

    UnicodeString LastStoredSession = SessionNodePath(SessionTree->Selected);
    if (IsFolderNode(SessionTree->Selected))
    {
      LastStoredSession = UnixIncludeTrailingBackslash(LastStoredSession);
    }
    WinConfiguration->LastStoredSession = LastStoredSession;

    TLoginDialogConfiguration DialogConfiguration = CustomWinConfiguration->LoginDialog;
    DialogConfiguration.WindowSize = StoreFormSize(this);
    DialogConfiguration.SiteSearch = FSiteSearch;
    CustomWinConfiguration->LoginDialog = DialogConfiguration;
  }
  __finally
  {
    WinConfiguration->EndUpdate();
  }
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
  // it does not make any sense to call this before
  // DoFormWindowProc(CM_SHOWINGCHANGED), we would end up on wrong monitor
  if (DebugAlwaysTrue(Visible))
  {
    RestoreFormSize(CustomWinConfiguration->LoginDialog.WindowSize, this);
  }

  FSiteSearch = CustomWinConfiguration->LoginDialog.SiteSearch;

  TStringList * OpenedStoredSessionFolders = CreateSortedStringList();
  try
  {
    OpenedStoredSessionFolders->CommaText = WinConfiguration->OpenedStoredSessionFolders;

    for (int Index = 0; Index < SessionTree->Items->Count; Index++)
    {
      LoadOpenedStoredSessionFolders(
        SessionTree->Items->Item[Index], OpenedStoredSessionFolders);
    }

    // tree view tries to make expanded node children all visible, what
    // may scroll the selected node (what should be the first one here),
    // out of the view
    if (SessionTree->Selected != NULL)
    {
      // see comment for LastStoredSession branch below
      DebugAssert(Visible);
      SessionTree->Selected->MakeVisible();
    }
  }
  __finally
  {
    delete OpenedStoredSessionFolders;
  }

  // calling TTreeNode::MakeVisible() when tree view is not visible yet,
  // sometimes scrolls view horizontally when not needed
  // (seems like it happens for sites that are at the same level
  // as site folders, e.g. for the very last root-level site, as long as
  // there are any folders)
  if (!FForceNewSite &&
      !WinConfiguration->LastStoredSession.IsEmpty() && DebugAlwaysTrue(Visible))
  {
    UnicodeString Path = UnixExcludeTrailingBackslash(WinConfiguration->LastStoredSession);
    bool Folder = (Path != WinConfiguration->LastStoredSession);

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
      while ((Node != NULL) && (!SameText(Node->Text, Name) || (IsFolderNode(Node) != Folder)))
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
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveConfiguration()
{
  DebugAssert(CustomWinConfiguration);
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
  DebugAssert(CustomWinConfiguration->OnMasterPasswordRecrypt == NULL);
  CustomWinConfiguration->OnMasterPasswordRecrypt = MasterPasswordRecrypt;
  try
  {
    DoPreferencesDialog(PreferencesMode);
  }
  __finally
  {
    DebugAssert(CustomWinConfiguration->OnMasterPasswordRecrypt == MasterPasswordRecrypt);
    CustomWinConfiguration->OnMasterPasswordRecrypt = NULL;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ResetNewSessionActionExecute(TObject * /*Sender*/)
{
  Default();
  EditSession();
  FNewSiteKeepName = false;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    if (!FIncrementalSearchState.Text.IsEmpty())
    {
      TShiftState Shift = KeyDataToShiftState(Message.KeyData);
      bool Reverse = Shift.Contains(ssShift);
      if (!SitesIncrementalSearch(FIncrementalSearchState.Text, true, Reverse, true))
      {
        MessageBeep(MB_ICONHAND);
      }
      Message.Result = 1;
      return;
    }
  }
  else if (Message.CharCode == VK_ESCAPE)
  {
    if (FIncrementalSearchState.Searching)
    {
      ResetSitesIncrementalSearch();
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::WMWindowPosChanged(TWMWindowPosChanged & Message)
{
  TForm::Dispatch(&Message);
  if (FLAGCLEAR(Message.WindowPos->flags, SWP_NOMOVE) && (FLinkedForm != NULL))
  {
    if (FPrevPos.X == std::numeric_limits<LONG>::min())
    {
      FPrevPos = BoundsRect.TopLeft();
    }
    TPoint P = TPoint(Message.WindowPos->x, Message.WindowPos->y);
    if (FPrevPos != P)
    {
      if (!FRestoring)
      {
        FLinkedForm->SetBounds(
          FLinkedForm->Left + (P.X - FPrevPos.X),
          FLinkedForm->Top + (P.Y - FPrevPos.Y),
          FLinkedForm->Width, FLinkedForm->Height);
      }
      FPrevPos = P;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CMVisibleChanged(TMessage & Message)
{
  TAutoFlag RestoringSwitch(FRestoring);
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  DebugAssert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else if (M->Msg == WM_MANAGES_CAPTION)
  {
    // caption managed in TLoginDialog::Init()
    M->Result = 1;
  }
  else if (M->Msg == WM_WANTS_MOUSEWHEEL)
  {
    M->Result = 1;
  }
  else if (M->Msg == WM_CAN_DISPLAY_UPDATES)
  {
    M->Result = 1;
  }
  else if (M->Msg == CM_ACTIVATE)
  {
    // Called from TCustomForm.ShowModal
    if (FSortEnablePending)
    {
      FSortEnablePending = false;
      SessionTree->SortType = Comctrls::stBoth;
    }
    TForm::Dispatch(Message);
  }
  else if (M->Msg == WM_SYSCOMMAND)
  {
    if (!HandleMinimizeSysCommand(*M))
    {
      TForm::Dispatch(Message);
    }
  }
  else if (M->Msg == WM_WINDOWPOSCHANGED)
  {
    WMWindowPosChanged(*reinterpret_cast<TWMWindowPosChanged *>(M));
  }
  else if (M->Msg == CM_VISIBLECHANGED)
  {
    CMVisibleChanged(*M);
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
  UnicodeString Message = MainInstructions(LoadStr(SET_DEFAULT_SESSION_SETTINGS));
  if (MessageDialog(Message, qtConfirmation,
        qaOK | qaCancel, HELP_SESSION_SAVE_DEFAULT) == qaOK)
  {
    std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));
    SaveSession(SessionData.get());
    // See the comment to the other use of the method in DoSaveSession.
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
  UnicodeString AdditionalParams = TProgramParams::FormatSwitch(DESKTOP_SWITCH);
  int IconIndex = 0;
  if (IsSiteNode(Node))
  {
    Name = GetNodeSession(Node)->Name;
    Message = FMTLOAD(CONFIRM_CREATE_SHORTCUT, (Name));
    AddToList(AdditionalParams, TProgramParams::FormatSwitch(UPLOAD_IF_ANY_SWITCH), L" ");
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
    DebugFail();
  }

  Message = MainInstructions(Message);
  if (MessageDialog(Message, qtConfirmation, qaYes | qaNo, HELP_CREATE_SHORTCUT) == qaYes)
  {
    TInstantOperationVisualizer Visualizer;
    CreateDesktopSessionShortCut(Name, L"", AdditionalParams, -1, IconIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SendToHookActionExecute(TObject * /*Sender*/)
{
  DebugAssert(IsSiteNode(SessionTree->Selected));
  DebugAssert(SelectedSession != NULL);
  UnicodeString Message = MainInstructions(FMTLOAD(CONFIRM_CREATE_SENDTO, (SelectedSession->Name)));
  if (MessageDialog(Message,
        qtConfirmation, qaYes | qaNo, HELP_CREATE_SENDTO) == qaYes)
  {
    TInstantOperationVisualizer Visualizer;
    UnicodeString AdditionalParams =
      TProgramParams::FormatSwitch(SEND_TO_HOOK_SWITCH) + L" " +
      TProgramParams::FormatSwitch(UPLOAD_SWITCH);
    CreateDesktopSessionShortCut(SelectedSession->Name,
      FMTLOAD(SESSION_SENDTO_HOOK_NAME2, (SelectedSession->LocalName, AppName)),
      AdditionalParams,
      CSIDL_SENDTO, SITE_ICON);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::HasNodeAnySession(TTreeNode * Node, bool NeedCanOpen)
{
  bool Result = false;
  TTreeNode * ANode = Node->GetNext();
  while (!Result && (ANode != NULL) && ANode->HasAsParent(Node))
  {
    Result =
      IsSessionNode(ANode) &&
      (!NeedCanOpen || StoredSessions->CanOpen(GetNodeSession(ANode)));
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
TModalResult __fastcall TLoginDialog::DefaultResult()
{
  return ::DefaultResult(this, LoginButton);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & CanClose)
{
  // CanClose test is now probably redundant,
  // once we have a fallback to LoginButton in DefaultResult
  CanClose = EnsureNotEditing();
  // When CanOpen is false, the DefaultResult() will fail finding a default button
  if (CanClose && CanOpen() && (ModalResult == DefaultResult()))
  {
    SaveDataList(FDataList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeEditing(TObject * /*Sender*/,
  TTreeNode * Node, bool & AllowEdit)
{
  DebugAssert(!FRenaming);
  AllowEdit =
    !FEditing &&
    (IsFolderOrWorkspaceNode(Node) ||
     (IsSiteNode(Node) && !GetNodeSession(Node)->Special));
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
    SessionTree->SetFocus();
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
    throw Exception(MainInstructions(FMTLOAD(LOGIN_DUPLICATE_SESSION_FOLDER_WORKSPACE, (Text))));
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
  if ((Node->Text != S) && !S.IsEmpty())
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
      // modified only, explicit
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
            DebugAssert(Path.SubString(1, OldRoot.Length()) == OldRoot);
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
        // modified only, explicit
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
  DebugAssert(InBounds || (Index == -1));
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
      DebugFail();
    case ftpsNone:
      return 0;

    case ftpsImplicit:
      return 1;

    case ftpsExplicitTls:
    case ftpsExplicitSsl:
      return 2;
  }
}
//---------------------------------------------------------------------------
TFtps __fastcall TLoginDialog::GetFtps()
{
  TFSProtocol FSProtocol = GetFSProtocol(false);
  int Index = (((FSProtocol == fsWebDAV) || (FSProtocol == fsS3)) ? WebDavsCombo->ItemIndex : FtpsCombo->ItemIndex);
  TFtps Ftps;
  switch (Index)
  {
    default:
      DebugFail();
    case 0:
      Ftps = ftpsNone;
      break;

    case 1:
      Ftps = ftpsImplicit;
      break;

    case 2:
      Ftps = ftpsExplicitTls;
      break;
  }
  return Ftps;
}
//---------------------------------------------------------------------------
TFSProtocol __fastcall TLoginDialog::GetFSProtocol(bool RequireScpFallbackDistinction)
{
  bool AllowScpFallback = false;
  if (RequireScpFallbackDistinction && DebugAlwaysTrue(FSessionData != NULL))
  {
    FSProtocolToIndex(FSessionData->FSProtocol, AllowScpFallback);
  }
  return IndexToFSProtocol(TransferProtocolCombo->ItemIndex, AllowScpFallback);
}
//---------------------------------------------------------------------------
int __fastcall TLoginDialog::DefaultPort()
{
  return ::DefaultPort(GetFSProtocol(false), GetFtps());
}
//---------------------------------------------------------------------------
void TLoginDialog::UpdateS3Credentials()
{
  if (S3CredentialsEnvCheck3->Checked)
  {
    UnicodeString S3Profile = GetS3Profile();
    UserNameEdit->Text = S3EnvUserName(S3Profile);
    PasswordEdit->Text = S3EnvPassword(S3Profile);
    // Is not set when viewing stored session.
    // We do this, so that when the checkbox is checked and unchecked, the token is preserved, the way username and password are.
    if (FSessionData != NULL)
    {
      FSessionData->S3SessionToken = S3EnvSessionToken(S3Profile);
      FSessionData->S3RoleArn = S3EnvRoleArn(S3Profile);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::TransferProtocolComboChange(TObject * Sender)
{
  if (!NoUpdate)
  {
    if (GetFSProtocol(false) == fsS3)
    {
      // Note that this happens even when loading the session
      // But the values will get overwritten.
      WebDavsCombo->ItemIndex = FtpsToIndex(ftpsImplicit);
      HostNameEdit->Text = S3HostName;
    }
    else
    {
      try
      {
        UnicodeString S3Profile = GetS3Profile();
        if (HostNameEdit->Text == S3HostName)
        {
          HostNameEdit->Clear();
        }
        if (UserNameEdit->Text == S3EnvUserName(S3Profile, NULL, true))
        {
          UserNameEdit->Clear();
        }
        if (PasswordEdit->Text == S3EnvPassword(S3Profile, NULL, true))
        {
          PasswordEdit->Clear();
        }
        if (FSessionData != NULL)
        {
          if (FSessionData->S3SessionToken == S3EnvSessionToken(S3Profile, NULL, true))
          {
            FSessionData->S3SessionToken = EmptyStr;
          }
          if (FSessionData->S3RoleArn == S3EnvRoleArn(S3Profile, NULL, true))
          {
            FSessionData->S3RoleArn = EmptyStr;
          }
        }
      }
      catch (...)
      {
        // noop
      }
    }

    S3CredentialsEnvCheck3->Checked = false;
  }

  UpdatePortWithProtocol();
  DataChange(Sender);
}
//---------------------------------------------------------------------------
void TLoginDialog::UpdatePortWithProtocol()
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
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EncryptionComboChange(TObject * Sender)
{
  UpdatePortWithProtocol();
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
    Compare = CompareLogicalText(Node1->Text, Node2->Text, true);
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
  TCustomEdit * Edit = Data->Edit;
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
    DebugAssert(Node == NULL);
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
TTreeNode * __fastcall TLoginDialog::NormalizeDropTarget(TTreeNode * DropTarget)
{
  return IsWorkspaceNode(DropTarget) ? DropTarget : SessionFolderNode(DropTarget);
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::SessionAllowDrop(TTreeNode * DropTarget)
{
  DropTarget = NormalizeDropTarget(DropTarget);
  return
    (SessionTree->Selected != NULL) &&
    (SessionTree->Selected->Parent != DropTarget) &&
    ((DropTarget == NULL) || IsFolderNode(DropTarget));
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
        if (!SessionAllowDrop(Node))
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
          SessionTree->DropTarget = NormalizeDropTarget(Node);
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
  DebugAssert(SessionTree->Selected != NULL);
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
  if (DebugAlwaysTrue((Sender == Source) && SessionAllowDrop(DropTarget)) &&
      // calling EnsureNotEditing only on drop, not on drag start,
      // to avoid getting popup during unintended micro-dragging
      EnsureNotEditing())
  {
    TSessionData * Session = SelectedSession;
    UnicodeString Path =
      UnixIncludeTrailingBackslash(SessionNodePath(DropTarget)) +
      UnixExtractFileName(Session->SessionName);

    SessionNameValidate(Path, Session->SessionName);

    // remove from storage
    Session->Remove();

    TSessionData * NewSession = StoredSessions->NewSession(Path, Session);
    // modified only, explicit
    StoredSessions->Save(false, true);
    // this should aways be the case
    if (DebugAlwaysTrue(Session != NewSession))
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
  std::unique_ptr<TStrings> Names(StoredSessions->GetFolderOrWorkspaceList(Path));
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

    // For ambiguous port numbers, keep the current protocol, even if it is not the default protocol for the port
    // (e.g. HTTPS vs S3 or FTP vs FTPES).
    // So for example, when user selects FTPES and then types 2121 port, the protocol is not reset to FTP,
    // once partial 21 is entered
    TFSProtocol CurrentFSProtocol = GetFSProtocol(false);

    int PortNumber = PortNumberEdit->AsInteger;
    if (PortNumber == SshPortNumber)
    {
      FSProtocol = fsSFTP;
      WellKnownPort = true;
    }
    else if (PortNumber == FtpPortNumber)
    {
      FSProtocol = fsFTP;
      if ((CurrentFSProtocol == FSProtocol) && (GetFtps() == ftpsExplicitTls))
      {
        Ftps = ftpsExplicitTls;
      }
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
      FSProtocol = (CurrentFSProtocol == fsS3) ? fsS3 : fsWebDAV;
      WellKnownPort = true;
    }
    else if (PortNumber == HTTPSPortNumber)
    {
      FSProtocol = (CurrentFSProtocol == fsS3) ? fsS3 : fsWebDAV;
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
void __fastcall TLoginDialog::ExportActionExecute(TObject * /*Sender*/)
{
  UnicodeString FileName = Configuration->GetDefaultIniFileExportPath();
  if (SaveDialog(LoadStr(EXPORT_CONF_TITLE), LoadStr(EXPORT_CONF_FILTER), L"ini", FileName))
  {
    Configuration->Export(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(IMPORT_CONFIGURATION2),
        qtWarning, qaOK | qaCancel, HELP_IMPORT_CONFIGURATION) == qaOK)
  {
    std::unique_ptr<TOpenDialog> OpenDialog(new TOpenDialog(Application));
    OpenDialog->Title = LoadStr(IMPORT_CONF_TITLE);
    OpenDialog->Filter = LoadStr(EXPORT_CONF_FILTER);
    OpenDialog->DefaultExt = L"ini";
    OpenDialog->FileName = Configuration->GetDefaultIniFileExportPath();

    if (OpenDialog->Execute())
    {
      // Before the session list gets destroyed
      SessionTree->Items->Clear();
      Configuration->Import(OpenDialog->FileName);
      // Similar to TPreferencesDialog::CustomIniFileStorageChanged
      ExecuteSelf(EmptyStr);
      TerminateApplication();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ResetSitesIncrementalSearch()
{
  FIncrementalSearchState.Reset();
  // this is to prevent active tree node being set back to Sites tab
  // (from UpdateNavigationTree) when we are called from SessionTreeExit,
  // while tab is changing
  if (NoUpdate == 0)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::SitesIncrementalSearch(
  const UnicodeString & Text, bool SkipCurrent, bool Reverse, bool Expanding)
{
  TTreeNode * Node = NULL;
  if (!Expanding)
  {
    Node = SearchSite(Text, false, SkipCurrent, Reverse);
  }
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
    FIncrementalSearchState.Searching = true;
    FIncrementalSearchState.Text = Text;

    // Tab always searches even in collapsed nodes
    TTreeNode * NextNode = SearchSite(Text, true, true, Reverse);
    FIncrementalSearchState.HaveNext =
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
      // https://stackoverflow.com/q/6257348/850848
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
      if (Eligible)
      {
        bool Matches = false;

        switch (FSiteSearch)
        {
          case isNameStartOnly:
            Matches = ContainsTextSemiCaseSensitive(Node->Text.SubString(1, Text.Length()), Text);
            break;
          case isName:
            Matches = ContainsTextSemiCaseSensitive(Node->Text, Text);
            break;
          case isAll:
            Matches = ContainsTextSemiCaseSensitive(Node->Text, Text);
            if (!Matches && IsSiteNode(Node))
            {
              TSessionData * Data = GetNodeSession(Node);
              Matches =
                ContainsTextSemiCaseSensitive(Data->HostName, Text) ||
                ContainsTextSemiCaseSensitive(Data->UserName, Text) ||
                ContainsTextSemiCaseSensitive(Data->Note, Text);
            }
            break;
        }

        if (Matches)
        {
          return Node;
        }
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
bool __fastcall TLoginDialog::EnsureNotEditing()
{
  bool Result = !FEditing;
  if (!Result)
  {
    UnicodeString Message = MainInstructions(LoadStr(LOGIN_SAVE_EDITING));
    unsigned int Answer = MessageDialog(Message, qtConfirmation, qaYes |qaNo | qaCancel);
    switch (Answer)
    {
      case qaYes:
        SaveAsSession(false);
        Result = true;
        break;

      case qaNo:
        CancelEditing();
        // Make sure OK button gets enabled
        UpdateControls();
        Result = true;
        break;

      default:
        // noop;
        break;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionTreeChanging(TObject * /*Sender*/,
  TTreeNode * /*Node*/, bool & AllowChange)
{
  if (!EnsureNotEditing())
  {
    AllowChange = false;
  }
  else
  {
    PersistNewSiteIfNeeded();
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
void __fastcall TLoginDialog::SessionAdvancedActionExecute(TObject * Sender)
{
  // If we ever allow showing advanced settings, while read-only,
  // we must make sure that FSessionData actually holds the advanced settings,
  // what it currently does not, in order to avoid master password prompt,
  // while cloning the session data in LoadSession.
  // To implement this, we may delegate the cloning to TWinConfiguration and
  // make use of FDontDecryptPasswords
  if (DebugAlwaysTrue(FSessionData != NULL))
  {
    // parse hostname (it may change protocol particularly) before opening advanced settings
    // (HostNameEditExit is not triggered when child dialog pops up when it is invoked by accelerator)
    // We should better handle this automaticaly when focus is moved to another dialog.
    ParseHostName();

    SaveSession(FSessionData);
    if (Sender == SessionAdvancedAction)
    {
      DoSiteAdvancedDialog(FSessionData);
    }
    else
    {
      DoSiteRawDialog(FSessionData);
    }
    // Needed only for Note.
    // The only other property visible on Login dialog that Advanced site dialog
    // can change is protocol (between fsSFTP and fsSFTPonly),
    // difference of the two not being visible on Login dialog anyway.
    LoadSession(FSessionData);
    if (DebugAlwaysTrue(SessionTree->Selected != NULL) &&
        IsSiteNode(SessionTree->Selected))
    {
      SetNodeImage(SessionTree->Selected, GetSessionImageIndex(FSessionData));
    }
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

  return DebugNotNull(PopupMenu);
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
  ResetSitesIncrementalSearch();
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
      if (DebugNotNull(SessionTree->PopupMenu))
      {
        MenuPopup(SessionTree, MousePos, Handled);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::EditCancelActionExecute(TObject * /*Sender*/)
{
  CancelEditing();
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
void __fastcall TLoginDialog::CancelEditing()
{
  FEditing = false;
  // reset back the color
  UpdateNodeImage(SessionTree->Selected);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CloneToNewSite()
{
  FNewSiteData->CopyData(SelectedSession);
  FNewSiteData->MakeUniqueIn(StoredSessions);
  FNewSiteKeepName = true;
  NewSite();
  EditSession();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CloneToNewSiteActionExecute(TObject * /*Sender*/)
{
  CloneToNewSite();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Login()
{
  if (OpenInNewWindow() && !IsNewSiteNode(SessionTree->Selected))
  {
    UnicodeString Path = SessionNodePath(SessionTree->Selected);
    ExecuteNewInstance(EncodeUrlString(Path));
    // prevent closing the window, see below
    ModalResult = mrNone;
  }
  else
  {
    // this is not needed when used from LoginButton,
    // but is needed when used from popup menus
    ModalResult = LoginButton->ModalResult;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoginActionExecute(TObject * /*Sender*/)
{
  Login();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PuttyActionExecute(TObject * /*Sender*/)
{
  std::unique_ptr<TAutoFlag> AutoFlag;
  if (PuttyAction->ActionComponent == NULL)
  {
    // Resolving conflict between copying command to clipboard on Ctrl+Shift and Ctrl+Shift+P keyboard shortcut.
    // Ctrl+Shift to copy to clipboard now works with menu invocations only.
    AutoFlag.reset(new TAutoFlag(DontCopyCommandToClipboard));
  }
  else
  {
    // does not clear on its own
    PuttyAction->ActionComponent = NULL;
  }
  // following may take some time, so cache the shift key state,
  // in case user manages to release it before following finishes
  bool Close = !OpenInNewWindow();

  std::unique_ptr<TList> DataList(new TList());
  SaveDataList(DataList.get());
  for (int Index = 0; Index < DataList->Count; Index++)
  {
    TSessionData * Data = reinterpret_cast<TSessionData *>(DataList->Items[Index]);
    OpenSessionInPutty(Data);
  }

  if (Close)
  {
    ModalResult = CloseButton->ModalResult;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoginButtonDropDownClick(TObject * /*Sender*/)
{
  MenuPopup(LoginDropDownMenu, LoginButton);
}
//---------------------------------------------------------------------------
void TLoginDialog::DoParseUrl(TSessionData * SessionData, const UnicodeString & Url)
{
  // We do not want to pass in StoredSessions as we do not want the URL be
  // parsed as pointing to a stored site.
  bool DefaultsOnly; // unused
  SessionData->ParseUrl(Url, NULL, NULL, DefaultsOnly, NULL, NULL, NULL, pufPreferProtocol);
  SessionData->RequireDirectories = false;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ParseUrl(const UnicodeString & Url)
{
  std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));

  SaveSession(SessionData.get());

  // Otherwise the colons would be misinterpreted as URL syntax
  if (IsIPv6Literal(Url))
  {
    UnicodeString IPv6Literal = Url;
    if (HasIP6LiteralBrackets(IPv6Literal))
    {
      IPv6Literal = StripIP6LiteralBrackets(IPv6Literal);
    }
    SessionData->HostName = IPv6Literal;
  }
  else
  {
    DoParseUrl(SessionData.get(), Url);
  }

  LoadSession(SessionData.get());
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PasteUrlActionExecute(TObject * /*Sender*/)
{
  UnicodeString ClipboardUrl;
  if (NonEmptyTextFromClipboard(ClipboardUrl))
  {
    if (!IsEditable())
    {
      // select new site node, when other node is selected and not in editing mode
      SessionTree->Selected = GetNewSiteNode();
    }

    // sanity check
    if (DebugAlwaysTrue(IsEditable()))
    {
      ParseUrl(ClipboardUrl);
    }

    // visualize the pasting
    HostNameEdit->SetFocus();
    HostNameEdit->SelectAll();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ParseHostName()
{
  UnicodeString HostName = HostNameEdit->Text.Trim();
  if (!HostName.IsEmpty())
  {
    // All this check is probably unnecessary, keeping it just to be safe
    std::unique_ptr<TSessionData> SessionData(new TSessionData(EmptyStr));
    DoParseUrl(SessionData.get(), HostName);
    std::unique_ptr<TSessionData> HostNameSessionData(new TSessionData(EmptyStr));
    HostNameSessionData->HostName = HostName;
    if ((HostNameSessionData->HostName != HostName) || // Has legacy HostName property parsing intervened?
       (!SessionData->IsSameDecrypted(HostNameSessionData.get())))
    {
      ParseUrl(HostName);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::HostNameEditExit(TObject * /*Sender*/)
{
  ParseHostName();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::GenerateUrlAction2Execute(TObject * /*Sender*/)
{
  if (DebugAlwaysTrue(SelectedSession != NULL))
  {
    PersistNewSiteIfNeeded();

    std::unique_ptr<TSessionData> Data(SelectedSession->Clone());
    Data->LookupLastFingerprint();

    DoGenerateUrlDialog(Data.get(), NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CopyParamRuleActionExecute(TObject * /*Sender*/)
{
  TSessionData * Data = GetSessionData();
  std::unique_ptr<TCopyParamList> CopyParamList(new TCopyParamList());
  (*CopyParamList) = *GUIConfiguration->CopyParamList;

  TCopyParamRuleData RuleData;
  RuleData.HostName = Data->HostNameExpanded;
  RuleData.UserName = Data->UserNameExpanded;
  int CopyParamIndex = CopyParamList->Find(RuleData);
  if (CopyParamIndex < 0)
  {
    TCopyParamRuleData RuleDataHostNameOnly;
    RuleDataHostNameOnly.HostName = Data->HostNameExpanded;
    CopyParamIndex = CopyParamList->Find(RuleDataHostNameOnly);
  }

  TCopyParamPresetMode Mode;
  TCopyParamRuleData * CurrentRuleData = NULL;

  if (CopyParamIndex < 0)
  {
    Mode = cpmAddCurrent;
    CurrentRuleData = &RuleData;
  }
  else
  {
    Mode = cpmEdit;
  }

  TCopyParamType DefaultCopyParams;

  if (DoCopyParamPresetDialog(
        CopyParamList.get(), CopyParamIndex, Mode, CurrentRuleData, DefaultCopyParams))
  {
    GUIConfiguration->CopyParamList = CopyParamList.get();
  }
}
//---------------------------------------------------------------------------
void TLoginDialog::SetSiteSearch(TIncrementalSearch SiteSearch)
{
  if (FSiteSearch != SiteSearch)
  {
    FSiteSearch = SiteSearch;
    if (!SitesIncrementalSearch(FIncrementalSearchState.Text, false, false, false))
    {
      ResetSitesIncrementalSearch();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SearchSiteNameStartOnlyActionExecute(TObject * /*Sender*/)
{
  SetSiteSearch(isNameStartOnly);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SearchSiteNameActionExecute(TObject * /*Sender*/)
{
  SetSiteSearch(isName);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SearchSiteActionExecute(TObject * /*Sender*/)
{
  SetSiteSearch(isAll);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  FBasicGroupBaseHeight = MulDiv(FBasicGroupBaseHeight, NewDPI, OldDPI);
  FNoteGroupOffset = MulDiv(FNoteGroupOffset, NewDPI, OldDPI);
  GenerateImages();
  CenterButtonImage(LoginButton);
  AutoSizeCheckBox(ShowAgainCheck);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PanelMouseDown(TObject *, TMouseButton, TShiftState, int, int)
{
  CountClicksForWindowPrint(this);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::S3CredentialsEnvCheck3Click(TObject *)
{
  UpdateS3Credentials();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::S3ProfileComboChange(TObject *)
{
  UpdateS3Credentials();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShowAgainCheckClick(TObject *)
{
  if (!ShowAgainCheck->Checked)
  {
    if (MessageDialog(LoadStr(LOGIN_NOT_SHOWING_AGAIN), qtConfirmation, qaOK | qaCancel, HELP_SHOW_LOGIN) != qaOK)
    {
      ShowAgainCheck->Checked = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SearchSiteStartActionExecute(TObject *)
{
  SessionTree->SetFocus();
  if (!FIncrementalSearchState.Searching)
  {
    FIncrementalSearchState.Searching = true;
    FIncrementalSearchState.HaveNext = false;
    DebugAssert(FIncrementalSearchState.Text.IsEmpty());
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SitesIncrementalSearchPanelContextPopup(TObject * Sender, TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
