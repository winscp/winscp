//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>
#include <Common.h>
#include <TextsWin.h>
#include <VCLCommon.h>

#include "Login.h"
#include "WinInterface.h"
#include "GUITools.h"
#include "Tools.h"
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "ComboEdit"
#pragma link "LogSettings"
#pragma link "GeneralSettings"
#pragma link "UpDownEdit"
#pragma link "XPGroupBox"
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoLoginDialog(TStoredSessionList *SessionList,
  TSessionData * Data, int Options)
{
  assert(Data);
  TLoginDialog *LoginDialog = new TLoginDialog(Application);
  bool Result;
  try
  {
    LoginDialog->StoredSessions = SessionList;
    LoginDialog->SessionData = Data;
    LoginDialog->Options = Options;
    Result = LoginDialog->Execute();
    if (Result)
    {
      Data->Assign(LoginDialog->SessionData);
    };
  }
  __finally
  {
    delete LoginDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::TLoginDialog(TComponent* AOwner)
        : TForm(AOwner)
{
  FSessionData = new TSessionData("");
  NoUpdate = 0;
  FLanguagesPopupMenu = NULL;
  FInitialized = false;
  FSavedTab = NULL;
  FSavedSession = -1;
  FOptions = loStartup;
  FLocaleChanging = false;
  InitControls();
}
//---------------------------------------------------------------------
__fastcall TLoginDialog::~TLoginDialog()
{
  LoggingFrame->OnGetDefaultLogFileName = NULL;
  // SelectItem event is called after destructor! Why?
  SessionListView->Selected = NULL;
  delete FSystemSettings;
  FSystemSettings = NULL;
  delete FSessionData;
  delete FLanguagesPopupMenu;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::ShowTabs(bool Show)
{
  for (int Index = 0; Index < PageControl->PageCount; Index++)
  {
    PageControl->Pages[Index]->TabVisible = Show;
  }
  // change form height by height of hidden tabs
  ClientHeight += (Show ? 1 : -1) * 50;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitControls()
{
  InitializeBugsCombo(BugIgnore1Combo);
  InitializeBugsCombo(BugPlainPW1Combo);
  InitializeBugsCombo(BugRSA1Combo);
  InitializeBugsCombo(BugHMAC2Combo);
  InitializeBugsCombo(BugDeriveKey2Combo);
  InitializeBugsCombo(BugRSAPad2Combo);
  InitializeBugsCombo(BugDHGEx2Combo);
  InitializeBugsCombo(BugPKSessID2Combo);
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::Init()
{
  LoggingFrame->OnGetDefaultLogFileName = LoggingGetDefaultLogFileName;
  UseSystemSettings(this, &FSystemSettings);
  Caption = FORMAT("%s %s", (AppName, Caption));

  InitControls();

  PrepareNavigationTree(SimpleNavigationTree);
  PrepareNavigationTree(AdvancedNavigationTree);

  if ((Options & loLocalDirectory) == 0)
  {
    LocalDirectoryLabel->Visible = false;
    LocalDirectoryEdit->Visible = false;
    LocalDirectoryDescLabel->Visible = false;
    DirectoriesGroup->Height = RemoteDirectoryEdit->Top + RemoteDirectoryEdit->Height + 12;
  }

  ShowTabs(false);

  if (StoredSessions && StoredSessions->Count && 
      (FSessionData->Name == StoredSessions->DefaultSettings->Name))
  {
    ChangePage(SessionListSheet);
    SessionListView->SetFocus();
  }
  else
  {
    ChangePage(BasicSheet);
    HostNameEdit->SetFocus();
  }

  UpdateControls();
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::InitializeBugsCombo(TComboBox * BugsCombo)
{
  int PrevIndex = BugsCombo->ItemIndex;
  BugsCombo->Clear();
  BugsCombo->Items->Add(LoadStr(LOGIN_BUG_AUTO));
  BugsCombo->Items->Add(LoadStr(LOGIN_BUG_OFF));
  BugsCombo->Items->Add(LoadStr(LOGIN_BUG_ON));
  assert(PrevIndex < BugsCombo->Items->Count);
  BugsCombo->ItemIndex = PrevIndex;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessions()
{
  SessionListView->Items->BeginUpdate();
  try
  {
    SessionListView->Items->Clear();
    if (StoredSessions)
    {
      for (int Index = 0; Index < StoredSessions->Count; Index++)
      {
        TListItem *Item;
        Item = SessionListView->Items->Add();
        LoadSessionItem(Item);
      }
    }
  }
  __finally
  {
    SessionListView->Items->EndUpdate();
  }
  SelectedSession = StoredSessions->Count > 0 ?
    dynamic_cast<TSessionData*>(StoredSessions->AtObject(0)) : NULL;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Default()
{
  if (StoredSessions)
  {
    FSessionData->Assign(StoredSessions->DefaultSettings);
  }
  else
  {
    FSessionData->Default();
  }
  LoadSession(FSessionData);
  FCurrentSessionName = "";
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSession(TSessionData * aSessionData)
{
  NoUpdate++;
  try
  {
    // Basic tab
    UserNameEdit->Text = aSessionData->UserName;
    PortNumberEdit->AsInteger = aSessionData->PortNumber;
    HostNameEdit->Text = aSessionData->HostName;
    PasswordEdit->Text = aSessionData->Password;
    PrivateKeyEdit->Text = aSessionData->PublicKeyFile;

    switch (aSessionData->FSProtocol) {
      case fsSCPonly: SCPonlyButton->Checked = true; break;
      case fsSFTP: SFTPButton->Checked = true; break;
      case fsSFTPonly:
      default: SFTPonlyButton->Checked = true; break;
    }

    // Directories tab
    LocalDirectoryEdit->Text = aSessionData->LocalDirectory;
    RemoteDirectoryEdit->Text = aSessionData->RemoteDirectory;
    UpdateDirectoriesCheck->Checked = aSessionData->UpdateDirectories;
    CacheDirectoriesCheck->Checked = aSessionData->CacheDirectories;
    CacheDirectoryChangesCheck->Checked = aSessionData->CacheDirectoryChanges;
    PreserveDirectoryChangesCheck->Checked = aSessionData->PreserveDirectoryChanges;
    ResolveSymlinksCheck->Checked = aSessionData->ResolveSymlinks;

    // Environment tab
    ConsiderDSTOnCheck->Checked = aSessionData->ConsiderDST;
    ConsiderDSTOffCheck->Checked = !aSessionData->ConsiderDST;
    if (aSessionData->EOLType == eolLF)
    {
      EOLTypeLFButton->Checked = true;
    }
    else
    {                
      EOLTypeCRLFButton->Checked = true;
    }

    // Authentication tab
    AuthTISCheck->Checked = aSessionData->AuthTIS;
    AuthKICheck->Checked = aSessionData->AuthKI;
    AuthKIPasswordCheck->Checked = aSessionData->AuthKIPassword;
    AuthGSSAPICheck->Checked = aSessionData->AuthGSSAPI;
    AgentFwdCheck->Checked = aSessionData->AgentFwd;

    // SSH tab
    Ssh2LegacyDESCheck->Checked = aSessionData->Ssh2DES;
    CompressionCheck->Checked = aSessionData->Compression;

    switch (aSessionData->SshProt) {
      case ssh1only:  SshProt1onlyButton->Checked = true; break;
      case ssh1:      SshProt1Button->Checked = true; break;
      case ssh2:      SshProt2Button->Checked = true; break;
      case ssh2only:  SshProt2onlyButton->Checked = true; break;
    }

    CipherListBox->Items->Clear();
    assert(CIPHER_NAME_WARN+CIPHER_COUNT-1 == CIPHER_NAME_DES);
    for (int Index = 0; Index < CIPHER_COUNT; Index++)
    {
      CipherListBox->Items->AddObject(
        LoadStr(CIPHER_NAME_WARN+aSessionData->Cipher[Index]),
        (TObject*)aSessionData->Cipher[Index]);
    }

    // Connection tab
    switch (aSessionData->PingType)
    {
      case ptNullPacket:
        PingNullPacketButton->Checked = true;
        break;

      case ptDummyCommand:
        PingDummyCommandButton->Checked = true;
        break;

      default:
        PingOffButton->Checked = true;
        break;
    }
    PingIntervalSecEdit->AsInteger = aSessionData->PingInterval;
    TimeoutEdit->AsInteger = aSessionData->Timeout;

    // Shell tab
    if (aSessionData->DefaultShell)
      DefaultShellButton->Checked = true;
    else
      ShellEnterButton->Checked = true;

    ShellEdit->Text = aSessionData->Shell;
    if (aSessionData->DetectReturnVar)
      ReturnVarAutodetectButton->Checked = true;
    else
      ReturnVarEnterButton->Checked = true;
    ReturnVarEdit->Text = aSessionData->ReturnVar;
    LookupUserGroupsCheck->Checked = aSessionData->LookupUserGroups;
    ClearAliasesCheck->Checked = aSessionData->ClearAliases;
    IgnoreLsWarningsCheck->Checked = aSessionData->IgnoreLsWarnings;
    Scp1CompatibilityCheck->Checked = aSessionData->Scp1Compatibility;
    UnsetNationalVarsCheck->Checked = aSessionData->UnsetNationalVars;
    AliasGroupListCheck->Checked = aSessionData->AliasGroupList;
    int TimeDifferenceMin = DateTimeToTimeStamp(aSessionData->TimeDifference).Time / 60000;
    if (double(aSessionData->TimeDifference) < 0)
    {
      TimeDifferenceMin = -TimeDifferenceMin;
    }
    TimeDifferenceEdit->AsInteger = TimeDifferenceMin / 60;
    TimeDifferenceMinutesEdit->AsInteger = TimeDifferenceMin % 60;

    // Proxy tab
    switch (aSessionData->ProxyMethod) {
      case pmHTTP: ProxyHTTPButton->Checked = true; break;
      case pmSocks4: ProxySocks4Button->Checked = true; break;
      case pmSocks5: ProxySocks5Button->Checked = true; break;
      case pmTelnet: ProxyTelnetButton->Checked = true; break;
      default: ProxyNoneButton->Checked = true; break;
    }
    ProxyHostEdit->Text = aSessionData->ProxyHost;
    ProxyPortEdit->AsInteger = aSessionData->ProxyPort;
    ProxyUsernameEdit->Text = aSessionData->ProxyUsername;
    ProxyPasswordEdit->Text = aSessionData->ProxyPassword;
    ProxyTelnetCommandEdit->Text = aSessionData->ProxyTelnetCommand;
    ProxyLocalhostCheck->Checked = aSessionData->ProxyLocalhost;
    switch (aSessionData->ProxyDNS) {
      case asOn: ProxyDNSOnButton->Checked = true; break;
      case asOff: ProxyDNSOffButton->Checked = true; break;
      default: ProxyDNSAutoButton->Checked = true; break;
    }

    // Bugs tab
    #define LOAD_BUG_COMBO(BUG) Bug ## BUG ## Combo->ItemIndex = 2 - aSessionData->Bug[sb ## BUG]
    LOAD_BUG_COMBO(Ignore1);
    LOAD_BUG_COMBO(PlainPW1);
    LOAD_BUG_COMBO(RSA1);
    LOAD_BUG_COMBO(HMAC2);
    LOAD_BUG_COMBO(DeriveKey2);
    LOAD_BUG_COMBO(RSAPad2);
    LOAD_BUG_COMBO(DHGEx2);
    LOAD_BUG_COMBO(PKSessID2);
    #undef LOAD_BUG_COMBO
  }
  __finally
  {
    NoUpdate--;
    UpdateControls();
  }

  FCurrentSessionName = aSessionData->Name;
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSession(TSessionData * aSessionData)
{
  aSessionData->Name = FCurrentSessionName;
  // Basic tab
  aSessionData->UserName = UserNameEdit->Text;
  aSessionData->PortNumber = PortNumberEdit->AsInteger;
  // must be loaded after UserName, because HostName may be in format user@host
  aSessionData->HostName = HostNameEdit->Text;
  aSessionData->Password = PasswordEdit->Text;
  aSessionData->PublicKeyFile = PrivateKeyEdit->Text;

  if (SCPonlyButton->Checked) aSessionData->FSProtocol = fsSCPonly;
    else
  if (SFTPButton->Checked) aSessionData->FSProtocol = fsSFTP;
    else aSessionData->FSProtocol = fsSFTPonly;

  // SSH tab
  aSessionData->Compression = CompressionCheck->Checked;
  aSessionData->Ssh2DES = Ssh2LegacyDESCheck->Checked;

  if (SshProt1onlyButton->Checked) aSessionData->SshProt = ssh1only;
    else
  if (SshProt1Button->Checked) aSessionData->SshProt = ssh1;
    else
  if (SshProt2Button->Checked) aSessionData->SshProt = ssh2;
    else aSessionData->SshProt = ssh2only;

  for (int Index = 0; Index < CIPHER_COUNT; Index++)
  {
    aSessionData->Cipher[Index] = (TCipher)CipherListBox->Items->Objects[Index];
  }

  // Authentication tab
  aSessionData->AuthTIS = AuthTISCheck->Checked;
  aSessionData->AuthKI = AuthKICheck->Checked;
  aSessionData->AuthKIPassword = AuthKIPasswordCheck->Checked;
  aSessionData->AuthGSSAPI = AuthGSSAPICheck->Checked;
  aSessionData->AgentFwd = AgentFwdCheck->Checked;

  // Connection tab
  if (PingNullPacketButton->Checked)
  {
    aSessionData->PingType = ptNullPacket;
  }
  else if (PingDummyCommandButton->Checked)
  {
    aSessionData->PingType = ptDummyCommand;
  }
  else
  {
    aSessionData->PingType = ptOff;
  }
  aSessionData->PingInterval = PingIntervalSecEdit->AsInteger;
  aSessionData->Timeout = TimeoutEdit->AsInteger;

  // Directories tab
  aSessionData->LocalDirectory = LocalDirectoryEdit->Text;
  aSessionData->RemoteDirectory = RemoteDirectoryEdit->Text;
  aSessionData->UpdateDirectories = UpdateDirectoriesCheck->Checked;
  aSessionData->CacheDirectories = CacheDirectoriesCheck->Checked;
  aSessionData->CacheDirectoryChanges = CacheDirectoryChangesCheck->Checked;
  aSessionData->PreserveDirectoryChanges = PreserveDirectoryChangesCheck->Checked;
  aSessionData->ResolveSymlinks = ResolveSymlinksCheck->Checked;

  // Environment tab
  aSessionData->ConsiderDST = ConsiderDSTOnCheck->Checked;
  if (EOLTypeLFButton->Checked) aSessionData->EOLType = eolLF;
    else aSessionData->EOLType = eolCRLF;

  // Shell tab
  aSessionData->DefaultShell = DefaultShellButton->Checked;
  if (ShellEnterButton->Checked)
    aSessionData->Shell = ShellEdit->Text;
  aSessionData->DetectReturnVar = ReturnVarAutodetectButton->Checked;
  if (ReturnVarEnterButton->Checked)
    aSessionData->ReturnVar = ReturnVarEdit->Text;
  aSessionData->LookupUserGroups = LookupUserGroupsCheck->Checked;
  aSessionData->ClearAliases = ClearAliasesCheck->Checked;
  aSessionData->IgnoreLsWarnings = IgnoreLsWarningsCheck->Checked;
  aSessionData->Scp1Compatibility = Scp1CompatibilityCheck->Checked;
  aSessionData->UnsetNationalVars = UnsetNationalVarsCheck->Checked;
  aSessionData->AliasGroupList = AliasGroupListCheck->Checked;
  aSessionData->TimeDifference =
    (double(TimeDifferenceEdit->AsInteger) / 24) +
    (double(TimeDifferenceMinutesEdit->AsInteger) / 24 / 60);

  // Proxy tab
  if (ProxyHTTPButton->Checked) aSessionData->ProxyMethod = pmHTTP;
    else
  if (ProxySocks4Button->Checked) aSessionData->ProxyMethod = pmSocks4;
    else
  if (ProxySocks5Button->Checked) aSessionData->ProxyMethod = pmSocks5;
    else
  if (ProxyTelnetButton->Checked) aSessionData->ProxyMethod = pmTelnet;
    else aSessionData->ProxyMethod = pmNone;

  aSessionData->ProxyHost = ProxyHostEdit->Text;
  aSessionData->ProxyPort = ProxyPortEdit->AsInteger;
  aSessionData->ProxyUsername = ProxyUsernameEdit->Text;
  aSessionData->ProxyPassword = ProxyPasswordEdit->Text;
  aSessionData->ProxyTelnetCommand = ProxyTelnetCommandEdit->Text;
  aSessionData->ProxyLocalhost = ProxyLocalhostCheck->Checked;

  if (ProxyDNSOnButton->Checked) aSessionData->ProxyDNS = asOn;
    else
  if (ProxyDNSOffButton->Checked) aSessionData->ProxyDNS = asOff;
    else aSessionData->ProxyDNS = asAuto;

  // Bugs tab
  #define SAVE_BUG_COMBO(BUG) aSessionData->Bug[sb ## BUG] = (TAutoSwitch)(2 - Bug ## BUG ## Combo->ItemIndex);
  SAVE_BUG_COMBO(Ignore1);
  SAVE_BUG_COMBO(PlainPW1);
  SAVE_BUG_COMBO(RSA1);
  SAVE_BUG_COMBO(HMAC2);
  SAVE_BUG_COMBO(DeriveKey2);
  SAVE_BUG_COMBO(RSAPad2);
  SAVE_BUG_COMBO(DHGEx2);
  SAVE_BUG_COMBO(PKSessID2);
  #undef SAVE_BUG_COMBO
}
//---------------------------------------------------------------------
void __fastcall TLoginDialog::UpdateControls()
{
  if (Visible)
  {
    NoUpdate++;
    try
    {
      #define SHOW_NAVIGATION(TREE, SHOW) if ((TREE)->Visible != (SHOW)) { \
        (TREE)->Visible = (SHOW); PageControlChange(PageControl); }
      SHOW_NAVIGATION(SimpleNavigationTree, !ShowAdvancedLoginOptionsCheck->Checked);
      SHOW_NAVIGATION(AdvancedNavigationTree, ShowAdvancedLoginOptionsCheck->Checked);
      #undef SHOW_NAVIGATION

      EnableControl(ShellIconsButton, SessionListView->Selected);

      if (!PrivateKeyEdit->Text.IsEmpty())
      {
        PasswordEdit->Clear();
      }
      EnableControl(PasswordEdit, PrivateKeyEdit->Text.IsEmpty());

      if (!PasswordEdit->Text.IsEmpty()) PrivateKeyEdit->Clear();
      EnableControl(PrivateKeyEdit, PasswordEdit->Text.IsEmpty());

      EnableControl(PingIntervalSecEdit, !PingOffButton->Checked);

      EnableControl(SessionListView, SessionListView->Items->Count);
      AdjustListColumnsWidth(SessionListView);
      SessionListView->Columns->Items[0]->Width -= 2;

      EnableControl(AuthTISCheck, !SshProt2onlyButton->Checked);
      EnableControl(AuthKICheck, !SshProt1onlyButton->Checked);
      EnableControl(AuthKIPasswordCheck,
        AuthTISCheck->Checked || AuthKICheck->Checked);
      EnableControl(AuthGSSAPICheck, !SshProt1onlyButton->Checked);

      EnableControl(CipherUpButton, CipherListBox->ItemIndex > 0);
      EnableControl(CipherDownButton, CipherListBox->ItemIndex >= 0 &&
        CipherListBox->ItemIndex < CipherListBox->Items->Count-1);
      EnableControl(Ssh2LegacyDESCheck, !SshProt1onlyButton->Checked);

      EnableControl(BugIgnore1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugPlainPW1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugRSA1Combo, !SshProt2onlyButton->Checked);
      EnableControl(BugHMAC2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugDeriveKey2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugRSAPad2Combo, !SshProt1onlyButton->Checked);
      EnableControl(BugDHGEx2Combo, !SshProt1onlyButton->Checked);

      EnableControl(ShellEdit, ShellEnterButton->Checked);
      EnableControl(ReturnVarEdit, ReturnVarEnterButton->Checked);

      EnableControl(ProxyHostEdit, !ProxyNoneButton->Checked);
      EnableControl(ProxyPortEdit, !ProxyNoneButton->Checked);
      EnableControl(ProxyUsernameEdit, !ProxyNoneButton->Checked);
      EnableControl(ProxyPasswordEdit, !ProxyNoneButton->Checked);
      EnableControl(ProxySettingsGroup, !ProxyNoneButton->Checked);
      EnableControl(ProxyTelnetCommandEdit, ProxyTelnetButton->Checked);

      EnableControl(PreserveDirectoryChangesCheck, CacheDirectoryChangesCheck->Checked);

      AboutButton->Visible = (Options & loAbout);
      LanguagesButton->Visible = (Options & loLanguage);
      ShellIconsButton->Visible = (Options & loTools);
      ToolsMenuButton->Visible = (Options & loTools);
      LoggingFrame->EnableLogWindow = (Options & loLogWindow);
    }
    __finally
    {
      NoUpdate--;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DataChange(TObject * /*Sender*/)
{
  if (!NoUpdate) UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PrepareNavigationTree(TTreeView * Tree)
{
  Tree->FullExpand();
  int i = 0;
  while (i < Tree->Items->Count)
  {
    if ((Tree->Items->Item[i]->StateIndex > 0) &&
        ((Options & Tree->Items->Item[i]->StateIndex) == 0))
    {
      Tree->Items->Delete(Tree->Items->Item[i]);
    }
    else
    {
      for (int pi = 0; pi < PageControl->PageCount; pi++)
      {
        if (PageControl->Pages[pi]->Tag == Tree->Items->Item[i]->SelectedIndex)
        {
          Tree->Items->Item[i]->Text = PageControl->Pages[pi]->Hint;
          break;
        }
      }
      i++;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::FormShow(TObject * /*Sender*/)
{
  if (!FInitialized)
  {
    FInitialized = true;
    Init();
    TSessionData * Data = GetSessionData();
    if (Data == FSessionData)
    {
      LoadSession(Data);
    }
    else
    {
      Default();
    }
  }
  if (FLocaleChanging)
  {
    Init();
    LoadSession(FSessionData);
    ChangePage(FSavedTab);
    SessionListView->ItemIndex = FSavedSession;
    LoadConfiguration();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewSelectItem(TObject * /*Sender*/,
      TListItem * /*Item*/, bool /*Selected*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::StoreSessions()
{
  StoredSessions->Save();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetSessionData(TSessionData * value)
{
  FSessionData->Assign(value);
  FSessionData->Special = false;
  LoadSession(FSessionData);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSessionData()
{
  if (PageControl->ActivePage == SessionListSheet)
  {
    return SelectedSession;
  }
  else
  {
    SaveSession(FSessionData);
    return FSessionData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetStoredSessions(TStoredSessionList * value)
{
  if (FStoredSessions != value)
  {
    FStoredSessions = value;
    LoadSessions();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessionItem(TListItem * Item)
{
  Item->Data = StoredSessions->AtObject(Item->Index);
  Item->Caption = ((TSessionData*)Item->Data)->Name;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewDblClick(TObject * /*Sender*/)
{
  if (SelectedSession)
  {
    if (SelectedSession->CanLogin) ModalResult = mrOk;
      else
    {
      SessionData = SelectedSession;
      ChangePage(BasicSheet);
      if (HostNameEdit->Text.IsEmpty()) HostNameEdit->SetFocus();
        else
      if (UserNameEdit->Text.IsEmpty()) UserNameEdit->SetFocus();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetSelectedSession(TSessionData * value)
{
  if (value)
  {
    int Index = StoredSessions->IndexOf(value);
    if (Index >= 0)
    {
      TListItem *Item = SessionListView->Items->Item[Index];
      Item->Focused = true;
      Item->Selected = true;
      Item->MakeVisible(false);
    }
  }
  else
  {
    SessionListView->Selected = NULL;
  }
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TLoginDialog::GetSelectedSession()
{
  if (SessionListView->Selected)
    return (TSessionData *)SessionListView->Selected->Data;
  else
    return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewInfoTip(TObject * /*Sender*/,
      TListItem * Item, AnsiString & InfoTip)
{
  InfoTip = ((TSessionData*)Item->Data)->InfoTip;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewKeyDown(TObject * /*Sender*/,
      WORD &Key, TShiftState /*Shift*/)
{
  if (Key == VK_DELETE) DeleteSessionAction->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadSessionActionExecute(TObject * /*Sender*/)
{
  if (SelectedSession)
  {
    SessionData = SelectedSession;
    ChangePage(BasicSheet);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveSessionActionExecute(TObject * /*Sender*/)
{
  AnsiString SessionName;
  SaveSession(FSessionData);
  SessionName = DoSaveSessionDialog(StoredSessions, FSessionData->SessionName);
  if (!SessionName.IsEmpty())
  {
    TListItem * Item;
    TSessionData *NewSession =
      StoredSessions->NewSession(SessionName, FSessionData);
    StoredSessions->Save();
    // by now list must contais same number of items or one less
    assert(StoredSessions->Count == SessionListView->Items->Count ||
      StoredSessions->Count == SessionListView->Items->Count+1);
    if (StoredSessions->Count > SessionListView->Items->Count)
      Item = SessionListView->Items->Insert(StoredSessions->IndexOf(NewSession));
    else
      Item = SessionListView->Items->Item[StoredSessions->IndexOf(NewSession)];

    LoadSessionItem(Item);
    SelectedSession = NewSession;
    SessionData = NewSession;

    ChangePage(SessionListSheet);
    SessionListView->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DeleteSessionActionExecute(TObject * /*Sender*/)
{
  if (SelectedSession)
  {
    int PrevSelectedIndex = SessionListView->Selected->Index;
    SelectedSession->Remove();
    StoredSessions->Remove(SelectedSession);
    SessionListView->Selected->Delete();
    if (SessionListView->Items->Count)
    {
      if (PrevSelectedIndex >= SessionListView->Items->Count)
        PrevSelectedIndex = SessionListView->Items->Count - 1;
      SelectedSession =
        (TSessionData *)StoredSessions->AtObject(PrevSelectedIndex);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ImportSessionsActionExecute(TObject * /*Sender*/)
{
  if (DoImportSessionsDialog(StoredSessions))
  {
    LoadSessions();
    if (SessionListView->Items->Count)
      SessionListView->Items->Item[0]->MakeVisible(False);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CleanUpActionExecute(TObject * /*Sender*/)
{
  if (DoCleanupDialog(StoredSessions, Configuration))
    LoadSessions();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AboutActionExecute(TObject * /*Sender*/)
{
  DoAboutDialog(Configuration);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ActionListUpdate(TBasicAction *Action,
      bool &Handled)
{
  if (Action == LoadSessionAction)
  {
    LoadSessionAction->Enabled = SessionListView->Selected;
  }
  else if (Action == DeleteSessionAction)
  {
    TSessionData * Data = SessionData;
    DeleteSessionAction->Enabled =
      SessionListView->Selected && Data && !Data->Special;
  }
  else if (Action == DesktopIconAction)
  {
    DesktopIconAction->Enabled = SessionListView->Selected;
  }
  else if (Action == SendToHookAction)
  {
    SendToHookAction->Enabled = SessionListView->Selected;
  }
  else if (Action == LoginAction)
  {
    TSessionData * Data = SessionData;
    LoginAction->Enabled = Data && Data->CanLogin;
  }
  else if (Action == SaveSessionAction)
  {
    SaveSessionAction->Enabled = (PageControl->ActivePage != SessionListSheet);
  }
  Handled = true;
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::Execute()
{
  LoadConfiguration();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    SaveConfiguration();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SaveConfiguration()
{
  assert(CustomWinConfiguration);
  CustomWinConfiguration->BeginUpdate();
  try
  {
    LoggingFrame->SaveConfiguration();
    GeneralSettingsFrame->SaveConfiguration();
    CustomWinConfiguration->ShowAdvancedLoginOptions = ShowAdvancedLoginOptionsCheck->Checked;
  }
  __finally
  {
    CustomWinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoadConfiguration()
{
  assert(CustomWinConfiguration);
  LoggingFrame->LoadConfiguration();
  GeneralSettingsFrame->LoadConfiguration();
  ShowAdvancedLoginOptionsCheck->Checked = CustomWinConfiguration->ShowAdvancedLoginOptions;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LoggingGetDefaultLogFileName(
  TObject* /*Sender*/, AnsiString & DefaultLogFileName)
{
  assert(FSessionData);
  DefaultLogFileName = FSessionData->DefaultLogFileName;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PreferencesButtonClick(TObject * /*Sender*/)
{
  ShowPreferencesDialog();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShowPreferencesDialog()
{
  DoPreferencesDialog(pmLogin);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NewSessionActionExecute(TObject * /*Sender*/)
{
  Default();
  ChangePage(BasicSheet);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::NavigationTreeChange(TObject * /*Sender*/,
      TTreeNode *Node)
{
  if (Node->SelectedIndex)
  {
    for (Integer Index = 0; Index < PageControl->PageCount; Index++)
    {
      if (PageControl->Pages[Index]->Tag == Node->SelectedIndex)
      {
        PageControl->ActivePage = PageControl->Pages[Index];
        return;
      }
    }
  }
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ChangePage(TTabSheet * Tab)
{
  PageControl->ActivePage = Tab;
  PageControlChange(PageControl);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PageControlChange(TObject *Sender)
{
  bool Found = false;
  if (PageControl->ActivePage->Tag)
  {
    for (int Index = 0; Index < NavigationTree->Items->Count; Index++)
    {
      if (NavigationTree->Items->Item[Index]->SelectedIndex ==
            PageControl->ActivePage->Tag)
      {
        NavigationTree->Items->Item[Index]->Selected = true;
        Found = true;
      }
    }
  }

  if (!Found)
  {
    ChangePage(BasicSheet);
  }
  else
  {
    DataChange(Sender);
  }
}
//---------------------------------------------------------------------------
TTreeView * __fastcall TLoginDialog::GetNavigationTree()
{
  return (ShowAdvancedLoginOptionsCheck->Checked ?
    AdvancedNavigationTree : SimpleNavigationTree);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    TShiftState Shift = KeyDataToShiftState(Message.KeyData);
    if (Shift.Contains(ssCtrl))
    {
      TTreeNode * Node = NavigationTree->Selected;
      if (!Shift.Contains(ssShift))
      {
        Node = Node->GetNext();
        if (!Node) Node = NavigationTree->Items->GetFirstNode();
      }
      else
      {
        if (Node->GetPrev()) Node = Node->GetPrev();
          else
        while (Node->GetNext()) Node = Node->GetNext();
      }
      Node->Selected = True;
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::Dispatch(void *Message)
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
      SaveSession(FSessionData);
      FSavedTab = PageControl->ActivePage;
      FSavedSession = SessionListView->ItemIndex;

      assert(FSystemSettings);
      RevokeSystemSettings(this, FSystemSettings);
      FSystemSettings = NULL;
      ShowTabs(true);

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
void __fastcall TLoginDialog::CipherListBoxStartDrag(TObject * /*Sender*/,
      TDragObject *& /*DragObject*/)
{
  FCipherDragSource = CipherListBox->ItemIndex;
  FCipherDragDest = -1;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CipherListBoxDragOver(TObject * /*Sender*/,
      TObject *Source, int X, int Y, TDragState /*State*/, bool &Accept)
{
  if (Source == CipherListBox) Accept = AllowCipherDrag(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CipherListBoxDragDrop(TObject * /*Sender*/,
      TObject *Source, int X, int Y)
{
  if (Source == CipherListBox)
  {
    if (AllowCipherDrag(X, Y)) CipherMove(FCipherDragSource, FCipherDragDest);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CipherButtonClick(TObject *Sender)
{
  CipherMove(CipherListBox->ItemIndex,
    CipherListBox->ItemIndex + (Sender == CipherUpButton ? -1 : 1));
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TLoginDialog::AllowCipherDrag(int X, int Y)
{
  FCipherDragDest = CipherListBox->ItemAtPos(TPoint(X, Y), true);
  return (FCipherDragDest >= 0) && (FCipherDragDest != FCipherDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CipherMove(int Source, int Dest)
{
  if (Source >= 0 && Source < CipherListBox->Items->Count &&
      Dest >= 0 && Dest < CipherListBox->Items->Count)
  {
    CipherListBox->Items->Move(Source, Dest);
    CipherListBox->ItemIndex = Dest;
    CipherListBox->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetDefaultSessionActionExecute(
      TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(SET_DEFAULT_SESSION_SETTINGS), qtConfirmation,
        qaOK | qaCancel, 0) == qaOK)
  {
    SaveSession(FSessionData);
    StoredSessions->DefaultSettings = FSessionData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ToolsMenuButtonClick(TObject * /*Sender*/)
{
  TPoint PopupPoint = ToolsMenuButton->ClientToScreen(TPoint(0, ToolsMenuButton->Height));
  ToolsPopupMenu->Popup(PopupPoint.x, PopupPoint.y);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::ShellIconsButtonClick(TObject * /*Sender*/)
{
  TPoint PopupPoint = ShellIconsButton->ClientToScreen(TPoint(0, ShellIconsButton->Height));
  IconsPopupMenu->Popup(PopupPoint.x, PopupPoint.y);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::DesktopIconActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(FMTLOAD(CONFIRM_CREATE_SHORTCUT, (SelectedSession->Name)),
        qtConfirmation, qaYes | qaNo, 0) == qaYes)
  {
    assert(SelectedSession);
    CreateDesktopShortCut(SelectedSession->Name, Application->ExeName,
      FORMAT("\"%s\"", (SelectedSession->Name)),
      FMTLOAD(SHORTCUT_INFO_TIP, (SelectedSession->Name, SelectedSession->InfoTip)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SendToHookActionExecute(TObject * /*Sender*/)
{
  if (MessageDialog(FMTLOAD(CONFIRM_CREATE_SENDTO, (SelectedSession->Name)),
        qtConfirmation, qaYes | qaNo, 0) == qaYes)
  {
    assert(SelectedSession);
    CreateDesktopShortCut(FMTLOAD(SESSION_SENDTO_HOOK_NAME, (SelectedSession->Name)),
      Application->ExeName,
      FORMAT("\"%s\" /upload", (SelectedSession->Name)), "",
      CSIDL_SENDTO);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SessionListViewCustomDrawItem(
      TCustomListView *Sender, TListItem *Item, TCustomDrawState /*State*/,
      bool &DefaultDraw)
{
  TSessionData * Data = (TSessionData *)Item->Data;
  TFontStyles Styles = Sender->Canvas->Font->Style;
  if (Data->Special) Styles = Styles /*< fsItalic*/ << fsBold << fsUnderline;
    else Styles = Styles /*>> fsItalic*/ >> fsBold >> fsUnderline;
  Sender->Canvas->Font->Style = Styles;
  DefaultDraw = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::CheckForUpdatesActionExecute(TObject * /*Sender*/)
{
  CheckForUpdates();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::SetOptions(int value)
{
  if (Options != value)
  {
    FOptions = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LanguagesButtonClick(TObject * /*Sender*/)
{
  TPoint PopupPoint = LanguagesButton->ClientToScreen(TPoint(0, LanguagesButton->Height));
  delete FLanguagesPopupMenu;
  FLanguagesPopupMenu = new TPopupMenu(this);

  TStrings * Locales = GUIConfiguration->Locales;
  for (int Index = 0; Index < Locales->Count; Index++)
  {
    TMenuItem * Item = new TMenuItem(FLanguagesPopupMenu);
    FLanguagesPopupMenu->Items->Add(Item);
    Item->Caption = Locales->Strings[Index];
    Item->Tag = reinterpret_cast<int>(Locales->Objects[Index]);
    Item->OnClick = LocaleClick;
    Item->Checked = (reinterpret_cast<LCID>(Locales->Objects[Index]) ==
      GUIConfiguration->Locale);
  }

  FLanguagesPopupMenu->Popup(PopupPoint.x, PopupPoint.y);
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::LocaleClick(TObject * Sender)
{
  assert(Sender);
  GUIConfiguration->Locale =
    static_cast<LCID>(dynamic_cast<TMenuItem*>(Sender)->Tag);
  LanguagesButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::PathEditsKeyDown(TObject * Sender,
  WORD & Key, TShiftState Shift)
{
  PathEditKeyDown(dynamic_cast<TCustomEdit*>(Sender), Key, Shift,
    (Sender == RemoteDirectoryEdit));
}
//---------------------------------------------------------------------------
void __fastcall TLoginDialog::AuthGSSAPICheckClick(TObject * /*Sender*/)
{
  if (!NoUpdate)
  {
    UpdateControls();
    if (AuthGSSAPICheck->Checked && !Configuration->GSSAPIInstalled)
    {
      throw Exception(LoadStr(GSSAPI_NOT_INSTALLED));
    }
  }
}
//---------------------------------------------------------------------------

