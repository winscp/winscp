//----------------------------------------------------------------------------
#ifndef LoginH
#define LoginH
//----------------------------------------------------------------------------
#include "ComboEdit.hpp"
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "PasswordEdit.hpp"
#include "UpDownEdit.hpp"
#include <System.Classes.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Mask.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include <Configuration.h>
#include <SessionData.h>
#include <PasTools.hpp>

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
  TAction *EditSessionAction;
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
  TTreeView *SessionTree;
  TButton *NewButton;
  TTabSheet *BasicSheet;
  TTabSheet *AdvancedSheet;
  TGroupBox *ProtocolGroup;
  TLabel *Label7;
  TRadioButton *SshProt1Button;
  TRadioButton *SshProt2Button;
  TCheckBox *CompressionCheck;
  TTabSheet *EnvironmentSheet;
  TTabSheet *ScpSheet;
  TGroupBox *OtherShellOptionsGroup;
  TCheckBox *LookupUserGroupsCheck;
  TCheckBox *ClearAliasesCheck;
  TCheckBox *UnsetNationalVarsCheck;
  TCheckBox *Scp1CompatibilityCheck;
  TGroupBox *ShellGroup;
  TComboBox *ShellEdit;
  TTabSheet *LogSheet;
  TLoggingFrame *LoggingFrame;
  TTabSheet *GeneralSheet;
  TLabel *Label13;
  TButton *PreferencesButton;
  TGeneralSettingsFrame *GeneralSettingsFrame;
  TPanel *LeftPanel;
  TTreeView *NavigationTree;
  TCheckBox *ShowAdvancedLoginOptionsCheck;
  TGroupBox *BasicGroup;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *PrivateKeyLabel;
  TEdit *HostNameEdit;
  TEdit *UserNameEdit;
  TPasswordEdit *PasswordEdit;
  TUpDownEdit *PortNumberEdit;
  TFilenameEdit *PrivateKeyEdit;
  TTabSheet *ConnSheet;
  TGroupBox *TimeoutGroup;
  TLabel *Label11;
  TLabel *Label12;
  TUpDownEdit *TimeoutEdit;
  TTabSheet *ProxySheet;
  TGroupBox *ProxyTypeGroup;
  TLabel *ProxyHostLabel;
  TLabel *ProxyPortLabel;
  TUpDownEdit *ProxyPortEdit;
  TEdit *ProxyHostEdit;
  TEdit *ProxyUsernameEdit;
  TLabel *ProxyUsernameLabel;
  TLabel *ProxyPasswordLabel;
  TPasswordEdit *ProxyPasswordEdit;
  TGroupBox *ProxySettingsGroup;
  TLabel *ProxyTelnetCommandLabel;
  TEdit *ProxyTelnetCommandEdit;
  TTabSheet *BugsSheet;
  TGroupBox *BugsGroupBox;
  TLabel *BugIgnore1Label;
  TComboBox *BugIgnore1Combo;
  TLabel *BugPlainPW1Label;
  TComboBox *BugPlainPW1Combo;
  TLabel *BugRSA1Label;
  TComboBox *BugRSA1Combo;
  TLabel *BugHMAC2Label;
  TComboBox *BugHMAC2Combo;
  TLabel *BugDeriveKey2Label;
  TComboBox *BugDeriveKey2Combo;
  TLabel *BugRSAPad2Label;
  TComboBox *BugRSAPad2Combo;
  TRadioButton *SshProt1onlyButton;
  TRadioButton *SshProt2onlyButton;
  TTabSheet *AuthSheet;
  TGroupBox *AuthenticationGroup;
  TCheckBox *AuthTISCheck;
  TCheckBox *AuthKICheck;
  TGroupBox *EncryptionGroup;
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
  TGroupBox *EnvironmentGroup;
  TPopupMenu *IconsPopupMenu;
  TMenuItem *Desktopicon1;
  TAction *SendToHookAction;
  TMenuItem *ExplorersSendtoshortcut1;
  TLabel *BugPKSessID2Label;
  TComboBox *BugPKSessID2Combo;
  TCheckBox *ProxyLocalhostCheck;
  TLabel *Label17;
  TAction *CheckForUpdatesAction;
  TMenuItem *CheckForUpdates1;
  TButton *SaveButton;
  TButton *LanguagesButton;
  TGroupBox *PingGroup;
  TLabel *PingIntervalLabel;
  TUpDownEdit *PingIntervalSecEdit;
  TRadioButton *PingOffButton;
  TRadioButton *PingNullPacketButton;
  TRadioButton *PingDummyCommandButton;
  TCheckBox *AuthKIPasswordCheck;
  TTabSheet *DirectoriesSheet;
  TGroupBox *DirectoriesGroup;
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
  TGroupBox *DSTModeGroup;
  TRadioButton *DSTModeUnixCheck;
  TRadioButton *DSTModeWinCheck;
  TGroupBox *ScpLsOptionsGroup;
  TCheckBox *IgnoreLsWarningsCheck;
  TCheckBox *SCPLsFullTimeAutoCheck;
  TTabSheet *SftpSheet;
  TGroupBox *SFTPBugsGroupBox;
  TLabel *Label10;
  TLabel *Label36;
  TComboBox *SFTPBugSymlinkCombo;
  TTabSheet *KexSheet;
  TGroupBox *KexOptionsGroup;
  TLabel *Label28;
  TListBox *KexListBox;
  TButton *KexUpButton;
  TButton *KexDownButton;
  TGroupBox *KexReexchangeGroup;
  TLabel *Label31;
  TUpDownEdit *RekeyTimeEdit;
  TLabel *Label32;
  TEdit *RekeyDataEdit;
  TGroupBox *IPvGroup;
  TRadioButton *IPAutoButton;
  TRadioButton *IPv4Button;
  TRadioButton *IPv6Button;
  TLabel *BugRekey2Label;
  TComboBox *BugRekey2Combo;
  TGroupBox *SFTPProtocolGroup;
  TLabel *Label34;
  TComboBox *SFTPMaxVersionCombo;
  TComboBox *SFTPBugSignedTSCombo;
  TButton *HelpButton;
  TButton *ColorButton;
  TPopupMenu *ColorPopupMenu;
  TMenuItem *ColorDefaultItem;
  TMenuItem *PickColorItem;
  TImageList *ColorImageList;
  TButton *RenameButton;
  TAction *RenameSessionAction;
  TGroupBox *DirectoryOptionsGroup;
  TTabSheet *TunnelSheet;
  TGroupBox *TunnelSessionGroup;
  TLabel *Label6;
  TLabel *Label14;
  TLabel *Label15;
  TLabel *Label16;
  TLabel *Label18;
  TEdit *TunnelHostNameEdit;
  TEdit *TunnelUserNameEdit;
  TPasswordEdit *TunnelPasswordEdit;
  TUpDownEdit *TunnelPortNumberEdit;
  TFilenameEdit *TunnelPrivateKeyEdit;
  TLabel *Label19;
  TComboBox *ReturnVarEdit;
  TLabel *Label20;
  TGroupBox *AuthenticationParamsGroup;
  TCheckBox *AgentFwdCheck;
  TCheckBox *TunnelCheck;
  TGroupBox *TunnelOptionsGroup;
  TLabel *Label21;
  TComboBox *TunnelLocalPortNumberEdit;
  TRadioButton *DSTModeKeepCheck;
  TGroupBox *ConnectionGroup;
  TCheckBox *FtpPasvModeCheck;
  TTabSheet *RecycleBinSheet;
  TGroupBox *RecycleBinGroup;
  TLabel *RecycleBinPathLabel;
  TCheckBox *DeleteToRecycleBinCheck;
  TCheckBox *OverwrittenToRecycleBinCheck;
  TEdit *RecycleBinPathEdit;
  TLabel *EOLTypeLabel;
  TComboBox *EOLTypeCombo;
  TLabel *UtfLabel;
  TComboBox *UtfCombo;
  TLabel *TimeDifferenceLabel;
  TUpDownEdit *TimeDifferenceEdit;
  TLabel *TimeDifferenceHoursLabel;
  TUpDownEdit *TimeDifferenceMinutesEdit;
  TLabel *TimeDifferenceMinutesLabel;
  TAction *ShellIconSessionAction;
  TLabel *Label9;
  TComboBox *ListingCommandEdit;
  TCheckBox *SshNoUserAuthCheck;
  TCheckBox *TryAgentCheck;
  TLabel *ProxyMethodLabel;
  TComboBox *SshProxyMethodCombo;
  TComboBox *ProxyDNSCombo;
  TComboBox *FtpProxyMethodCombo;
  TLabel *ProxyLocalCommandLabel;
  TEdit *ProxyLocalCommandEdit;
  TButton *ProxyLocalCommandBrowseButton;
  TStaticText *ProxyTelnetCommandHintText;
  TStaticText *ProxyLocalCommandHintText;
  TImageList *SessionImageList;
  TButton *NewFolderButton;
  TAction *NewSessionFolderAction;
  TGroupBox *FtpPingGroup;
  TLabel *FtpPingIntervalLabel;
  TUpDownEdit *FtpPingIntervalSecEdit;
  TRadioButton *FtpPingOffButton;
  TRadioButton *FtpPingNullPacketButton;
  TRadioButton *FtpPingDummyCommandButton;
  TLabel *Label23;
  TComboBox *SftpServerEdit;
  TTabSheet *FtpSheet;
  TGroupBox *FtpGroup;
  TLabel *Label25;
  TMemo *PostLoginCommandsMemo;
  TLabel *BugMaxPkt2Label;
  TComboBox *BugMaxPkt2Combo;
  TLabel *Label5;
  TComboBox *FtpListAllCombo;
  TCheckBox *FtpForcePasvIpCheck;
  TLabel *Label22;
  TComboBox *TransferProtocolCombo;
  TComboBox *FtpsCombo;
  TCheckBox *AllowScpFallbackCheck;
  TLabel *FtpsLabel;
  TEdit *FtpAccountEdit;
  TLabel *FtpAccountLabel;
  TCheckBox *AnonymousLoginCheck;
  TCheckBox *BufferSizeCheck;
  TCheckBox *SynchronizeBrowsingCheck;
  TGroupBox *GSSAPIGroup;
  TCheckBox *AuthGSSAPICheck3;
  TCheckBox *GSSAPIFwdTGTCheck;
  TLabel *BugIgnore2Label;
  TComboBox *BugIgnore2Combo;
  TPopupMenu *SaveDropDownMenu;
  TMenuItem *Save1;
  TMenuItem *Setdefaults1;
  TMenuItem *N1;
  TMenuItem *N2;
  TMenuItem *Pageant1;
  TMenuItem *Puttygen1;
  TAction *PageantAction;
  TAction *PuttygenAction;
  void __fastcall DataChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall SessionTreeDblClick(TObject *Sender);
  void __fastcall SessionTreeKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall EditSessionActionExecute(TObject *Sender);
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
  void __fastcall SendToHookActionExecute(TObject *Sender);
  void __fastcall CheckForUpdatesActionExecute(TObject *Sender);
  void __fastcall LanguagesButtonClick(TObject *Sender);
  void __fastcall AuthGSSAPICheck3Click(TObject *Sender);
  void __fastcall KexButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall PrivateKeyEditAfterDialog(TObject *Sender,
    UnicodeString &Name, bool &Action);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall ColorButtonClick(TObject *Sender);
  void __fastcall ColorDefaultItemClick(TObject *Sender);
  void __fastcall PickColorItemClick(TObject *Sender);
  void __fastcall RenameSessionActionExecute(TObject * Sender);
  void __fastcall PathEditBeforeDialog(TObject *Sender, UnicodeString &Name,
    bool &Action);
  void __fastcall TransferProtocolComboChange(TObject *Sender);
  void __fastcall NavigationTreeCollapsing(TObject *Sender,
    TTreeNode *Node, bool &AllowCollapse);
  void __fastcall ShellIconSessionActionExecute(TObject *Sender);
  void __fastcall ProxyLocalCommandBrowseButtonClick(TObject *Sender);
  void __fastcall SessionTreeChange(TObject *Sender, TTreeNode *Node);
  void __fastcall SessionTreeEdited(TObject *Sender, TTreeNode *Node,
    UnicodeString &S);
  void __fastcall SessionTreeEditing(TObject *Sender, TTreeNode *Node,
    bool &AllowEdit);
  void __fastcall SessionTreeCustomDrawItem(TCustomTreeView *Sender,
    TTreeNode *Node, TCustomDrawState State, bool &DefaultDraw);
  void __fastcall SessionTreeExpandedCollapsed(TObject *Sender, TTreeNode *Node);
  void __fastcall SessionTreeCompare(TObject *Sender, TTreeNode *Node1,
          TTreeNode *Node2, int Data, int &Compare);
  void __fastcall NewSessionFolderActionExecute(TObject *Sender);
  void __fastcall SessionTreeStartDrag(TObject *Sender,
          TDragObject *&DragObject);
  void __fastcall SessionTreeDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall SessionTreeMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y);
  void __fastcall SessionTreeEndDrag(TObject *Sender, TObject *Target,
          int X, int Y);
  void __fastcall AnonymousLoginCheckClick(TObject *Sender);
  void __fastcall SaveButtonDropDownClick(TObject *Sender);
  void __fastcall PageantActionExecute(TObject *Sender);
  void __fastcall PuttygenActionExecute(TObject *Sender);

private:
  int NoUpdate;
  TSessionData * FSessionData;
  TSessionData * FEditingSessionData;
  TStoredSessionList * FStoredSessions;
  int FAlgDragSource, FAlgDragDest;
  int FOptions;
  TPopupMenu * FLanguagesPopupMenu;
  bool FInitialized;
  TTabSheet * FSavedTab;
  int FSavedSession;
  bool FLocaleChanging;
  void * FSystemSettings;
  UnicodeString FCurrentSessionName;
  TColor FColor;
  UnicodeString FBeforeDialogPath;
  TStringList * FTreeLabels;
  TWndMethod FOldSessionTreeProc;
  TTreeNode * FHintNode;
  TTreeViewScrollOnDragOver * FScrollOnDragOver;
  int FDefaultPort;

  void __fastcall LoadSession(TSessionData * aSessionData);
  void __fastcall UpdateControls();
  void __fastcall SetSessionData(TSessionData * value);
  TSessionData * __fastcall GetSessionData();
  void __fastcall SaveSession(TSessionData * aStoredSession);
  void __fastcall LoadSessions();
  TSessionData * __fastcall GetSelectedSession();
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall WMHelp(TWMHelp & Message);
  int __fastcall FSProtocolToIndex(TFSProtocol FSProtocol, bool & AllowScpFallback);
  TFSProtocol __fastcall IndexToFSProtocol(int Index, bool AllowScpFallback);
  int __fastcall LastSupportedFtpProxyMethod();
  bool __fastcall SupportedFtpProxyMethod(int Method);
  TProxyMethod __fastcall GetProxyMethod();
  int __fastcall GetFtpProxyLogonType();
  void __fastcall UpdateNavigationTree();
  void __fastcall UpdateTree(TTreeNode * ANode, bool Recursive);
  void __fastcall UpdateFolderNode(TTreeNode * Node);
  TTreeNode * __fastcall AddSession(TSessionData * Data);
  TTreeNode * __fastcall AddSessionPath(UnicodeString Path, bool CanCreate = true);
  void __fastcall DestroySession(TSessionData * Data);
  void __fastcall CheckDuplicateFolder(TTreeNode * Parent, UnicodeString Text,
    TTreeNode * Node);
  void __fastcall NewSessionFolderInputDialogInitialize(
    TObject * Sender, TInputDialogData * Data);
  UnicodeString __fastcall SessionNodePath(TTreeNode * Node);
  TTreeNode * __fastcall SessionFolderNode(TTreeNode * Node);
  TTreeNode * __fastcall CurrentSessionFolderNode();
  void __fastcall SessionTreeProc(TMessage & Message);
  bool __fastcall SessionAllowDrop(TTreeNode * DropTarget);
  int __fastcall DefaultPort();
  void __fastcall MasterPasswordRecrypt(TObject * Sender);
  void __fastcall SaveOpenedStoredSessionFolders(
    TTreeNode * Node, TStrings * OpenedStoredSessionFolders);
  void __fastcall LoadOpenedStoredSessionFolders(
    TTreeNode * Node, TStrings * OpenedStoredSessionFolders);
  void __fastcall ExecuteTool(const UnicodeString & Name);

protected:
  void __fastcall Default();
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  void __fastcall LoadState();
  void __fastcall SaveState();
  void __fastcall ShowPreferencesDialog();
  void __fastcall ChangePage(TTabSheet * Tab);
  virtual void __fastcall Dispatch(void * Message);
  bool __fastcall AllowAlgDrag(TListBox * AlgListBox, int X, int Y);
  void __fastcall AlgMove(TListBox * AlgListBox, int Source, int Dest);
  void __fastcall LocaleClick(TObject * Sender);
  void __fastcall LocaleGetClick(TObject * Sender);
  void __fastcall Init();
  void __fastcall InitControls();
  void __fastcall VerifyKey(UnicodeString FileName, bool TypeOnly);
  void __fastcall EditSession();
  __property TSessionData * SelectedSession  = { read=GetSelectedSession };

public:
  virtual __fastcall TLoginDialog(TComponent* AOwner);
  __fastcall ~TLoginDialog();
  void __fastcall Init(TStoredSessionList *SessionList, int Options);
  bool __fastcall Execute(TSessionData *& Data, bool & Owned);
};
//----------------------------------------------------------------------------
#endif
