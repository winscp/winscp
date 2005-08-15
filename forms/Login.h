//----------------------------------------------------------------------------
#ifndef LoginH
#define LoginH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <Mask.hpp>
#include <ComboEdit.hpp>
#include <ActnList.hpp>
#include <UpDownEdit.hpp>
#include <XPThemes.hpp>
#include <PasswordEdit.hpp>
#include <Menus.hpp>
//----------------------------------------------------------------------------
#include <Configuration.h>
#include <SessionData.h>

#include "LogSettings.h"
#include "GeneralSettings.h"
//----------------------------------------------------------------------------
class TLoginDialog : public TForm
{
__published:
  TButton *LoginButton;
  TButton *CloseButton;
  TButton *AboutButton;
  TActionList *ActionList;
  TAction *LoadSessionAction;
  TAction *SaveSessionAction;
  TAction *DeleteSessionAction;
  TAction *ImportSessionsAction;
  TAction *LoginAction;
  TAction *AboutAction;
  TAction *CleanUpAction;
  TAction *NewSessionAction;
  TPanel *MainPanel;
  TPageControl *PageControl;
  TTabSheet *SessionListSheet;
  TButton *LoadButton;
  TButton *DeleteButton;
  TListView *SessionListView;
  TButton *NewButton;
  TTabSheet *BasicSheet;
  TTabSheet *AdvancedSheet;
  TXPGroupBox *ProtocolGroup;
  TLabel *Label7;
  TRadioButton *SshProt1Button;
  TRadioButton *SshProt2Button;
  TCheckBox *CompressionCheck;
  TTabSheet *EnvironmentSheet;
  TTabSheet *ScpSheet;
  TXPGroupBox *OtherShellOptionsGroup;
  TCheckBox *LookupUserGroupsCheck;
  TCheckBox *ClearAliasesCheck;
  TCheckBox *UnsetNationalVarsCheck;
  TCheckBox *Scp1CompatibilityCheck;
  TXPGroupBox *ReturnVarGroup;
  TRadioButton *ReturnVarAutodetectButton;
  TRadioButton *ReturnVarEnterButton;
  TComboBox *ReturnVarEdit;
  TXPGroupBox *ShellGroup;
  TRadioButton *DefaultShellButton;
  TRadioButton *ShellEnterButton;
  TComboBox *ShellEdit;
  TTabSheet *LogSheet;
  TLoggingFrame *LoggingFrame;
  TTabSheet *GeneralSheet;
  TLabel *Label13;
  TButton *PreferencesButton;
  TGeneralSettingsFrame *GeneralSettingsFrame;
  TPanel *LeftPanel;
  TTreeView *AdvancedNavigationTree;
  TCheckBox *ShowAdvancedLoginOptionsCheck;
  TXPGroupBox *BasicGroup;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TEdit *HostNameEdit;
  TEdit *UserNameEdit;
  TPasswordEdit *PasswordEdit;
  TUpDownEdit *PortNumberEdit;
  TFilenameEdit *PrivateKeyEdit;
  TTreeView *SimpleNavigationTree;
  TTabSheet *ConnSheet;
  TXPGroupBox *TimeoutGroup;
  TLabel *Label11;
  TLabel *Label12;
  TUpDownEdit *TimeoutEdit;
  TTabSheet *ProxySheet;
  TXPGroupBox *ProxyTypeGroup;
  TRadioButton *ProxyNoneButton;
  TRadioButton *ProxyHTTPButton;
  TRadioButton *ProxySocks4Button;
  TRadioButton *ProxyTelnetButton;
  TLabel *Label15;
  TLabel *Label18;
  TUpDownEdit *ProxyPortEdit;
  TEdit *ProxyHostEdit;
  TEdit *ProxyUsernameEdit;
  TLabel *Label19;
  TLabel *Label20;
  TPasswordEdit *ProxyPasswordEdit;
  TXPGroupBox *ProxySettingsGroup;
  TLabel *Label21;
  TEdit *ProxyTelnetCommandEdit;
  TTabSheet *BugsSheet;
  TXPGroupBox *BugsGroupBox;
  TLabel *Label22;
  TComboBox *BugIgnore1Combo;
  TLabel *Label23;
  TComboBox *BugPlainPW1Combo;
  TLabel *Label24;
  TComboBox *BugRSA1Combo;
  TLabel *Label25;
  TComboBox *BugHMAC2Combo;
  TLabel *Label26;
  TComboBox *BugDeriveKey2Combo;
  TLabel *Label27;
  TComboBox *BugRSAPad2Combo;
  TRadioButton *SshProt1onlyButton;
  TRadioButton *SshProt2onlyButton;
  TTabSheet *AuthSheet;
  TXPGroupBox *AuthenticationGroup;
  TCheckBox *AuthTISCheck;
  TCheckBox *AgentFwdCheck;
  TCheckBox *AuthKICheck;
  TXPGroupBox *EncryptionGroup;
  TListBox *CipherListBox;
  TLabel *Label8;
  TCheckBox *Ssh2LegacyDESCheck;
  TButton *CipherUpButton;
  TButton *CipherDownButton;
  TButton *SetDefaultSessionButton;
  TAction *SetDefaultSessionAction;
  TButton *ToolsMenuButton;
  TPopupMenu *ToolsPopupMenu;
  TMenuItem *Import1;
  TMenuItem *Cleanup1;
  TButton *ShellIconsButton;
  TAction *DesktopIconAction;
  TXPGroupBox *EOLTypeGroup;
  TRadioButton *EOLTypeLFButton;
  TRadioButton *EOLTypeCRLFButton;
  TXPGroupBox *TransferProtocolGroup;
  TRadioButton *SFTPButton;
  TRadioButton *SCPonlyButton;
  TRadioButton *SFTPonlyButton;
  TPopupMenu *IconsPopupMenu;
  TMenuItem *Desktopicon1;
  TAction *SendToHookAction;
  TMenuItem *ExplorersSendtoshortcut1;
  TLabel *Label14;
  TComboBox *BugPKSessID2Combo;
  TRadioButton *ProxySocks5Button;
  TCheckBox *ProxyLocalhostCheck;
  TLabel *Label17;
  TRadioButton *ProxyDNSOffButton;
  TRadioButton *ProxyDNSAutoButton;
  TRadioButton *ProxyDNSOnButton;
  TUpDownEdit *TimeDifferenceEdit;
  TLabel *Label29;
  TLabel *Label30;
  TAction *CheckForUpdatesAction;
  TMenuItem *CheckForUpdates1;
  TButton *SaveButton;
  TButton *LanguagesButton;
  TXPGroupBox *PingGroup;
  TLabel *Label6;
  TUpDownEdit *PingIntervalSecEdit;
  TRadioButton *PingOffButton;
  TRadioButton *PingNullPacketButton;
  TRadioButton *PingDummyCommandButton;
  TUpDownEdit *TimeDifferenceMinutesEdit;
  TLabel *Label9;
  TCheckBox *AuthKIPasswordCheck;
  TTabSheet *DirectoriesSheet;
  TXPGroupBox *DirectoriesGroup;
  TLabel *LocalDirectoryLabel;
  TLabel *RemoteDirectoryLabel;
  TLabel *LocalDirectoryDescLabel;
  TDirectoryEdit *LocalDirectoryEdit;
  TEdit *RemoteDirectoryEdit;
  TCheckBox *UpdateDirectoriesCheck;
  TCheckBox *CacheDirectoriesCheck;
  TCheckBox *ResolveSymlinksCheck;
  TCheckBox *CacheDirectoryChangesCheck;
  TCheckBox *PreserveDirectoryChangesCheck;
  TXPGroupBox *ConsiderDSTGroup;
  TRadioButton *ConsiderDSTOnCheck;
  TRadioButton *ConsiderDSTOffCheck;
  TCheckBox *AuthGSSAPICheck;
  TXPGroupBox *RecycleBinGroup;
  TCheckBox *DeleteToRecycleBinCheck;
  TCheckBox *OverwrittenToRecycleBinCheck;
  TLabel *RecycleBinPathLabel;
  TEdit *RecycleBinPathEdit;
  TXPGroupBox *ScpLsOptionsGroup;
  TCheckBox *IgnoreLsWarningsCheck;
  TCheckBox *AliasGroupListCheck;
  TCheckBox *SCPLsFullTimeAutoCheck;
  TTabSheet *SftpSheet;
  TXPGroupBox *SFTPBugsGroupBox;
  TLabel *Label10;
  TLabel *Label36;
  TComboBox *SFTPBugSymlinkCombo;
  TTabSheet *KexSheet;
  TXPGroupBox *KexOptionsGroup;
  TLabel *Label28;
  TListBox *KexListBox;
  TButton *KexUpButton;
  TButton *KexDownButton;
  TXPGroupBox *KexReexchangeGroup;
  TLabel *Label31;
  TUpDownEdit *RekeyTimeEdit;
  TLabel *Label32;
  TEdit *RekeyDataEdit;
  TXPGroupBox *IPvGroup;
  TRadioButton *IPAutoButton;
  TRadioButton *IPv4Button;
  TRadioButton *IPv6Button;
  TLabel *Label33;
  TComboBox *BugRekey2Combo;
  TXPGroupBox *SFTPProtocolGroup;
  TLabel *Label34;
  TLabel *Label35;
  TComboBox *SFTPMaxVersionCombo;
  TComboBox *SFTPBugUtfCombo;
  TComboBox *SFTPBugSignedTSCombo;
  TButton *HelpButton;
  void __fastcall DataChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall SessionListViewSelectItem(TObject *Sender,
  TListItem *Item, bool Selected);
  void __fastcall SessionListViewDblClick(TObject *Sender);
  void __fastcall SessionListViewInfoTip(TObject *Sender,
    TListItem *Item, AnsiString &InfoTip);
  void __fastcall SessionListViewKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall LoadSessionActionExecute(TObject *Sender);
  void __fastcall SaveSessionActionExecute(TObject *Sender);
  void __fastcall DeleteSessionActionExecute(TObject *Sender);
  void __fastcall ImportSessionsActionExecute(TObject *Sender);
  void __fastcall CleanUpActionExecute(TObject *Sender);
  void __fastcall AboutActionExecute(TObject *Sender);
  void __fastcall ActionListUpdate(TBasicAction *Action,
    bool &Handled);
  void __fastcall PreferencesButtonClick(TObject *Sender);
  void __fastcall NewSessionActionExecute(TObject *Sender);
  void __fastcall NavigationTreeChange(TObject *Sender, TTreeNode *Node);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall AlgListBoxStartDrag(TObject *Sender,
          TDragObject *&DragObject);
  void __fastcall AlgListBoxDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
  void __fastcall AlgListBoxDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall CipherButtonClick(TObject *Sender);
  void __fastcall SetDefaultSessionActionExecute(TObject *Sender);
  void __fastcall ToolsMenuButtonClick(TObject *Sender);
  void __fastcall DesktopIconActionExecute(TObject *Sender);
  void __fastcall SessionListViewCustomDrawItem(TCustomListView *Sender,
          TListItem *Item, TCustomDrawState State, bool &DefaultDraw);
  void __fastcall ShellIconsButtonClick(TObject *Sender);
  void __fastcall SendToHookActionExecute(TObject *Sender);
  void __fastcall CheckForUpdatesActionExecute(TObject *Sender);
  void __fastcall LanguagesButtonClick(TObject *Sender);
  void __fastcall AuthGSSAPICheckClick(TObject *Sender);
  void __fastcall KexButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall PrivateKeyEditAfterDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

private:
  int NoUpdate;
  TSessionData * FSessionData;
  TStoredSessionList * FStoredSessions;
  int FAlgDragSource, FAlgDragDest;
  int FOptions;
  TPopupMenu * FLanguagesPopupMenu;
  AnsiString FOrigCaption;
  bool FInitialized;
  TTabSheet * FSavedTab;
  int FSavedSession;
  bool FLocaleChanging;
  void * FSystemSettings;
  AnsiString FCurrentSessionName;  

  void __fastcall LoadSession(TSessionData * aSessionData);
  void __fastcall UpdateControls();
  void __fastcall SetSessionData(TSessionData * value);
  TSessionData * __fastcall GetSessionData();
  void __fastcall StoreSessions();
  void __fastcall SaveSession(TSessionData * aStoredSession);
  void __fastcall SetStoredSessions(TStoredSessionList * value);
  void __fastcall LoadSessions();
  void __fastcall LoadSessionItem(TListItem * Item);
  void __fastcall SetSelectedSession(TSessionData * value);
  TSessionData * __fastcall GetSelectedSession();
  TTreeView * __fastcall GetNavigationTree();
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall InitializeBugsCombo(TComboBox * BugsCombo);

protected:
  void __fastcall Default();
  void __fastcall LoadConfiguration();
  void __fastcall LoggingGetDefaultLogFileName(System::TObject* Sender,
    AnsiString & DefaultLogFileName);
  void __fastcall SaveConfiguration();
  void __fastcall ShowPreferencesDialog();
  void __fastcall ChangePage(TTabSheet * Tab);
  virtual void __fastcall Dispatch(void * Message);
  bool __fastcall AllowAlgDrag(TListBox * AlgListBox, int X, int Y);
  void __fastcall AlgMove(TListBox * AlgListBox, int Source, int Dest);
  void __fastcall PrepareNavigationTree(TTreeView * Tree, bool ClearHints);
  void __fastcall SetOptions(int value);
  void __fastcall LocaleClick(TObject * Sender);
  void __fastcall LocaleGetClick(TObject * Sender);
  void __fastcall Init();
  void __fastcall InitControls();
  void __fastcall ShowTabs(bool Show);
  void __fastcall VerifyKey(AnsiString FileName, bool TypeOnly);

  __property TTreeView * NavigationTree = { read=GetNavigationTree };

public:
  virtual __fastcall TLoginDialog(TComponent* AOwner);
  __fastcall ~TLoginDialog();
  bool __fastcall Execute();

  __property TSessionData * SessionData  = { read=GetSessionData, write=SetSessionData };
  __property TStoredSessionList * StoredSessions  = { read=FStoredSessions, write=SetStoredSessions };
  __property TSessionData * SelectedSession  = { read=GetSelectedSession, write=SetSelectedSession };
  __property int Options = { read=FOptions, write=SetOptions };
};
//----------------------------------------------------------------------------
#endif
