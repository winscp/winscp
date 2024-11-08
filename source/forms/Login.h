//----------------------------------------------------------------------------
#ifndef LoginH
#define LoginH
//----------------------------------------------------------------------------
#include "ComboEdit.hpp"
#include "PasswordEdit.hpp"
#include "UpDownEdit.hpp"
#include "PngImageList.hpp"
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
#include <CustomWinConfiguration.h>
#include <SessionData.h>
#include <PasTools.hpp>
#include <System.Actions.hpp>
#include <GUITools.h>
#include <System.ImageList.hpp>
//----------------------------------------------------------------------------
class TLoginDialog : public TForm
{
__published:
  TButton *LoginButton;
  TButton *CloseButton;
  TActionList *ActionList;
  TAction *EditSessionAction;
  TAction *SaveSessionAction;
  TAction *DeleteSessionAction;
  TAction *ImportSessionsAction;
  TAction *LoginAction;
  TAction *AboutAction;
  TAction *CleanUpAction;
  TAction *ResetNewSessionAction;
  TPanel *MainPanel;
  TTreeView *SessionTree;
  TAction *SetDefaultSessionAction;
  TButton *ToolsMenuButton;
  TPopupMenu *ToolsPopupMenu;
  TMenuItem *Import1;
  TMenuItem *Cleanup1;
  TAction *DesktopIconAction;
  TAction *SendToHookAction;
  TAction *CheckForUpdatesAction;
  TMenuItem *CheckForUpdates1;
  TButton *SaveButton;
  TButton *HelpButton;
  TAction *RenameSessionAction;
  TPngImageList *SessionImageList;
  TAction *NewSessionFolderAction;
  TPopupMenu *SaveDropDownMenu;
  TMenuItem *SaveSessionMenuItem;
  TMenuItem *Setdefaults1;
  TMenuItem *N1;
  TMenuItem *N2;
  TMenuItem *Pageant1;
  TMenuItem *Puttygen1;
  TAction *RunPageantAction;
  TAction *RunPuttygenAction;
  TAction *ImportAction;
  TAction *ExportAction;
  TMenuItem *N3;
  TMenuItem *ImportConfiguration1;
  TMenuItem *ExportConfiguration1;
  TAction *PreferencesAction;
  TMenuItem *N4;
  TMenuItem *Preferences1;
  TMenuItem *About1;
  TPanel *SitesPanel;
  TPanel *ContentsPanel;
  TGroupBox *ContentsGroupBox;
  TLabel *ContentsLabel;
  TEdit *ContentsNameEdit;
  TMemo *ContentsMemo;
  TPanel *SitePanel;
  TGroupBox *BasicGroup;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *UserNameLabel;
  TLabel *PasswordLabel;
  TLabel *Label22;
  TLabel *FtpsLabel;
  TLabel *WebDavsLabel;
  TEdit *HostNameEdit;
  TEdit *UserNameEdit;
  TPasswordEdit *PasswordEdit;
  TUpDownEdit *PortNumberEdit;
  TComboBox *TransferProtocolCombo;
  TComboBox *FtpsCombo;
  TComboBox *WebDavsCombo;
  TPanel *BasicFtpPanel;
  TCheckBox *AnonymousLoginCheck;
  TPanel *BasicSshPanel;
  TButton *AdvancedButton;
  TButton *ManageButton;
  TPopupMenu *ManageSitePopupMenu;
  TMenuItem *Edit1;
  TMenuItem *Delete1;
  TMenuItem *Rename1;
  TMenuItem *Newfolder1;
  TMenuItem *DesktopIcon2;
  TMenuItem *ExplorersSendToShortcut2;
  TMenuItem *Shellicon2;
  TMenuItem *N5;
  TMenuItem *Shellicon1;
  TMenuItem *N6;
  TPopupMenu *ManageFolderPopupMenu;
  TMenuItem *MenuItem1;
  TMenuItem *MenuItem3;
  TMenuItem *MenuItem4;
  TMenuItem *MenuItem5;
  TMenuItem *MenuItem6;
  TMenuItem *MenuItem7;
  TMenuItem *MenuItem8;
  TPopupMenu *ManageNewSitePopupMenu;
  TMenuItem *MenuItem12;
  TMenuItem *MenuItem13;
  TMenuItem *MenuItem16;
  TMenuItem *MenuItem17;
  TMenuItem *MenuItem21;
  TMenuItem *MenuItem22;
  TMenuItem *Setdefaults2;
  TMenuItem *Reset1;
  TPopupMenu *ManageWorkspacePopupMenu;
  TMenuItem *MenuItem2;
  TMenuItem *MenuItem10;
  TMenuItem *MenuItem11;
  TMenuItem *MenuItem18;
  TMenuItem *MenuItem19;
  TButton *EditCancelButton;
  TAction *EditCancelAction;
  TButton *EditButton;
  TAction *SaveAsSessionAction;
  TMenuItem *SaveAsSessionMenuItem;
  TMenuItem *N7;
  TEdit *TransferProtocolView;
  TEdit *EncryptionView;
  TPanel *ButtonPanel;
  TPopupMenu *SessionAdvancedPopupMenu;
  TMenuItem *MenuItem9;
  TMenuItem *MenuItem14;
  TAction *SessionAdvancedAction;
  TAction *PreferencesLoggingAction;
  TMenuItem *PreferencesLoggingAction1;
  TMenuItem *Session1;
  TPngImageList *ActionImageList;
  TAction *CloneToNewSiteAction;
  TMenuItem *SiteClonetoNewSiteMenuItem;
  TAction *PuttyAction;
  TPopupMenu *LoginDropDownMenu;
  TMenuItem *Login1;
  TMenuItem *OpeninPuTTY1;
  TMenuItem *Login2;
  TMenuItem *N8;
  TMenuItem *Login3;
  TMenuItem *N9;
  TMenuItem *SiteLoginMenuItem;
  TMenuItem *N10;
  TMenuItem *Login5;
  TMenuItem *N11;
  TMenuItem *OpeninPuTTY2;
  TMenuItem *OpeninPuTTY3;
  TAction *PasteUrlAction;
  TMenuItem *Paste1;
  TAction *GenerateUrlAction2;
  TMenuItem *GenerateSessionURL1;
  TMenuItem *GenerateSessionURL2;
  TAction *CopyParamRuleAction;
  TMenuItem *TransferSettingsRule1;
  TGroupBox *NoteGroup;
  TMemo *NoteMemo;
  TPngImageList *SessionImageList120;
  TPngImageList *ActionImageList120;
  TAction *SearchSiteNameStartOnlyAction;
  TAction *SearchSiteNameAction;
  TAction *SearchSiteAction;
  TMenuItem *Search1;
  TMenuItem *SearchOptions1;
  TMenuItem *SearchSiteNameStartOnly1;
  TMenuItem *SearchSiteName1;
  TMenuItem *SearchSite1;
  TAction *SearchSiteStartAction;
  TMenuItem *FindSite1;
  TMenuItem *Search3;
  TMenuItem *FindSite3;
  TMenuItem *SearchOptions3;
  TMenuItem *AllMajorSiteFields2;
  TMenuItem *AnyPartofSiteName2;
  TMenuItem *BeginningofSiteNameOnly2;
  TMenuItem *Search4;
  TMenuItem *FindSite4;
  TMenuItem *SearchOptions4;
  TMenuItem *AllMajorSiteFields3;
  TMenuItem *AnyPartofSiteName3;
  TMenuItem *BeginningofSiteNameOnly3;
  TMenuItem *Search2;
  TMenuItem *FindSite2;
  TMenuItem *SearchOptions2;
  TMenuItem *AllMajorSiteFields1;
  TMenuItem *AnyPartofSiteName1;
  TMenuItem *BeginningofSiteNameOnly1;
  TPngImageList *SessionImageList144;
  TPngImageList *SessionImageList192;
  TPngImageList *ActionImageList144;
  TPngImageList *ActionImageList192;
  TAction *SessionRawAction;
  TMenuItem *EditRawSettings1;
  TPanel *ShowAgainPanel;
  TCheckBox *ShowAgainCheck;
  TPanel *BasicS3Panel;
  TCheckBox *S3CredentialsEnvCheck3;
  TMenuItem *OpeninPuTTY4;
  TComboBox *S3ProfileCombo;
  TPanel *SitesIncrementalSearchPanel;
  TStaticText *SitesIncrementalSearchBorderLabel;
  TStaticText *SitesIncrementalSearchLabel;
  TPopupMenu *SitesIncrementalSearchPopupMenu;
  TMenuItem *MenuItem36;
  TMenuItem *MenuItem37;
  TMenuItem *MenuItem38;
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
  void __fastcall PreferencesActionExecute(TObject *Sender);
  void __fastcall ResetNewSessionActionExecute(TObject *Sender);
  void __fastcall SetDefaultSessionActionExecute(TObject *Sender);
  void __fastcall ToolsMenuButtonClick(TObject *Sender);
  void __fastcall DesktopIconActionExecute(TObject *Sender);
  void __fastcall SendToHookActionExecute(TObject *Sender);
  void __fastcall CheckForUpdatesActionExecute(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall RenameSessionActionExecute(TObject * Sender);
  void __fastcall TransferProtocolComboChange(TObject *Sender);
  void __fastcall NavigationTreeCollapsing(TObject *Sender,
    TTreeNode *Node, bool &AllowCollapse);
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
  void __fastcall SessionTreeExpanding(TObject *Sender, TTreeNode *Node, bool &AllowExpansion);
  void __fastcall RunPageantActionExecute(TObject *Sender);
  void __fastcall RunPuttygenActionExecute(TObject *Sender);
  void __fastcall PortNumberEditChange(TObject *Sender);
  void __fastcall ExportActionExecute(TObject *Sender);
  void __fastcall ImportActionExecute(TObject *Sender);
  void __fastcall SessionTreeKeyPress(TObject *Sender, System::WideChar &Key);
  void __fastcall SessionTreeExit(TObject *Sender);
  void __fastcall SessionTreeChanging(TObject *Sender, TTreeNode *Node, bool &AllowChange);
  void __fastcall SessionAdvancedActionExecute(TObject *Sender);
  void __fastcall ManageButtonClick(TObject *Sender);
  void __fastcall SessionTreeMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
  void __fastcall SessionTreeContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall EditCancelActionExecute(TObject *Sender);
  void __fastcall SaveAsSessionActionExecute(TObject *Sender);
  void __fastcall AdvancedButtonDropDownClick(TObject *Sender);
  void __fastcall PreferencesLoggingActionExecute(TObject *Sender);
  void __fastcall CloneToNewSiteActionExecute(TObject *Sender);
  void __fastcall LoginActionExecute(TObject *Sender);
  void __fastcall PuttyActionExecute(TObject *Sender);
  void __fastcall LoginButtonDropDownClick(TObject *Sender);
  void __fastcall PasteUrlActionExecute(TObject *Sender);
  void __fastcall HostNameEditExit(TObject *Sender);
  void __fastcall GenerateUrlAction2Execute(TObject *Sender);
  void __fastcall CopyParamRuleActionExecute(TObject *Sender);
  void __fastcall SearchSiteNameStartOnlyActionExecute(TObject *Sender);
  void __fastcall SearchSiteNameActionExecute(TObject *Sender);
  void __fastcall SearchSiteActionExecute(TObject *Sender);
  void __fastcall PanelMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall S3CredentialsEnvCheck3Click(TObject *Sender);
  void __fastcall EncryptionComboChange(TObject *Sender);
  void __fastcall S3ProfileComboChange(TObject *Sender);
  void __fastcall ShowAgainCheckClick(TObject *Sender);
  void __fastcall SearchSiteStartActionExecute(TObject *Sender);
  void __fastcall SitesIncrementalSearchPanelContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

private:
  int NoUpdate;
  TSessionData * FNewSiteData;
  bool FNewSiteKeepName;
  TSessionData * FSessionData;
  bool FInitialized;
  TWndMethod FOldSessionTreeProc;
  TTreeNode * FHintNode;
  TTreeViewScrollOnDragOver * FScrollOnDragOver;
  int FDefaultPort;
  TList * FDataList;
  bool FUpdatePortWithProtocol;
  TIncrementalSearchState FIncrementalSearchState;
  int FIncrementalSearching;
  int FBasicGroupBaseHeight;
  int FNoteGroupOffset;
  bool FEditing;
  bool FRenaming;
  bool FForceNewSite;
  bool FLoading;
  bool FSortEnablePending;
  std::unique_ptr<TImageList> FButtonImageList;
  std::map<int, int> FButtonImagesMap;
  TIncrementalSearch FSiteSearch;
  TForm * FLinkedForm;
  TPoint FPrevPos;
  UnicodeString FUserNameLabel;
  UnicodeString FPasswordLabel;
  int FFixedSessionImages;
  bool FRestoring;

  void __fastcall LoadSession(TSessionData * SessionData);
  void __fastcall LoadContents();
  void __fastcall UpdateControls();
  void __fastcall SetSessionData(TSessionData * value);
  TSessionData * __fastcall GetSessionData();
  void __fastcall SaveSession(TSessionData * aStoredSession);
  void __fastcall LoadSessions();
  TSessionData * __fastcall GetSelectedSession();
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  int __fastcall FSProtocolToIndex(TFSProtocol FSProtocol, bool & AllowScpFallback);
  TFSProtocol __fastcall IndexToFSProtocol(int Index, bool AllowScpFallback);
  int __fastcall FtpsToIndex(TFtps Ftps);
  TFtps __fastcall GetFtps();
  TFSProtocol __fastcall GetFSProtocol(bool RequireScpFallbackDistinction);
  void __fastcall UpdateFolderNode(TTreeNode * Node);
  TTreeNode * __fastcall AddSession(TSessionData * Data);
  TTreeNode * __fastcall AddSessionPath(UnicodeString Path,
    bool CanCreate, bool IsWorkspace);
  void __fastcall DestroySession(TSessionData * Data);
  void __fastcall CheckDuplicateFolder(TTreeNode * Parent, UnicodeString Text,
    TTreeNode * Node);
  void __fastcall CheckIsSessionFolder(TTreeNode * Node);
  void __fastcall NewSessionFolderInputDialogInitialize(
    TObject * Sender, TInputDialogData * Data);
  UnicodeString __fastcall SessionNodePath(TTreeNode * Node);
  TTreeNode * __fastcall SessionFolderNode(TTreeNode * Node);
  TTreeNode * __fastcall CurrentSessionFolderNode();
  void __fastcall SessionTreeProc(TMessage & Message);
  TTreeNode * __fastcall NormalizeDropTarget(TTreeNode * DropTarget);
  bool __fastcall SessionAllowDrop(TTreeNode * DropTarget);
  int __fastcall DefaultPort();
  void __fastcall MasterPasswordRecrypt(TObject * Sender);
  void __fastcall LoadOpenedStoredSessionFolders(
    TTreeNode * Node, TStrings * OpenedStoredSessionFolders);
  bool __fastcall HasNodeAnySession(TTreeNode * Node, bool NeedCanOpen = false);
  void __fastcall SaveDataList(TList * DataList);
  inline bool __fastcall IsFolderNode(TTreeNode * Node);
  inline bool __fastcall IsWorkspaceNode(TTreeNode * Node);
  inline bool __fastcall IsFolderOrWorkspaceNode(TTreeNode * Node);
  inline bool __fastcall IsSessionNode(TTreeNode * Node);
  inline bool __fastcall IsSiteNode(TTreeNode * Node);
  inline bool __fastcall IsNewSiteNode(TTreeNode * Node);
  TTreeNode * __fastcall GetNewSiteNode();
  void __fastcall SetNewSiteNodeLabel();
  inline TSessionData * __fastcall GetNodeSession(TTreeNode * Node);
  void __fastcall ReloadSessions(const UnicodeString & SelectSite);
  void __fastcall ResetSitesIncrementalSearch();
  bool __fastcall SitesIncrementalSearch(const UnicodeString & Text, bool SkipCurrent, bool Reverse, bool Expanding);
  TTreeNode * __fastcall SearchSite(const UnicodeString & Text,
    bool AllowExpanding, bool SkipCurrent, bool Reverse);
  TTreeNode * __fastcall GetNextNode(TTreeNode * Node, bool Reverse);
  UnicodeString __fastcall GetFolderOrWorkspaceContents(
    TTreeNode * Node, const UnicodeString & Indent, const UnicodeString & CommonRoot);
  TPopupMenu * __fastcall GetSelectedNodePopupMenu();
  void __fastcall PersistNewSiteIfNeeded();
  TTreeNode * __fastcall FindSessionNode(TSessionData * SessionData, bool ByName);
  void __fastcall UpdateButtonVisibility(TButton * Button);
  void __fastcall Idle();
  TSessionData * __fastcall GetEditingSessionData();
  void __fastcall SaveAsSession(bool ForceDialog);
  void __fastcall InvalidateSessionData();
  bool __fastcall CanOpen();
  bool IsSiteAndCanOpen();
  bool IsFolderOrWorkspaceAndCanOpen();
  bool __fastcall IsCloneToNewSiteDefault();
  bool __fastcall IsDefaultResult(TModalResult Result);
  void __fastcall UpdateNodeImage(TTreeNode * Node);
  int __fastcall GetSessionImageIndex(TSessionData * Data);
  void __fastcall SetNodeImage(TTreeNode * Node, int ImageIndex);
  void __fastcall CancelEditing();
  bool __fastcall EnsureNotEditing();
  bool __fastcall IsEditable();
  TSessionData * __fastcall CloneSelectedSession();
  void __fastcall CloneToNewSite();
  void DoParseUrl(TSessionData * SessionData, const UnicodeString & Url);
  void __fastcall ParseUrl(const UnicodeString & Url);
  void __fastcall ParseHostName();
  void __fastcall ResetNewSiteData();
  TModalResult __fastcall DefaultResult();
  int AddLoginButtonImage(int Index, bool Enabled);
  void __fastcall WMWindowPosChanged(TWMWindowPosChanged & Message);
  void __fastcall GenerateImages();
  void __fastcall CMVisibleChanged(TMessage & Message);
  void UpdateS3Credentials();
  void UpdateLoginButton();
  void FloodFill(TBitmap * Bitmap, int X, int Y);
  void UpdatePortWithProtocol();
  void LoadS3Profiles();
  UnicodeString GetS3GeneralName();
  UnicodeString GetS3Profile();

protected:
  void __fastcall Default();
  void __fastcall NewSite();
  void __fastcall SaveConfiguration();
  void __fastcall LoadState();
  void __fastcall SaveState();
  void __fastcall ShowPreferencesDialog(TPreferencesMode PreferencesMode);
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall Init();
  void __fastcall InitControls();
  void __fastcall EditSession();
  void __fastcall Login();
  void SetSiteSearch(TIncrementalSearch SiteSearch);
  __property TSessionData * SelectedSession  = { read=GetSelectedSession };

  INTERFACE_HOOK;

public:
  virtual __fastcall TLoginDialog(TComponent* AOwner);
  __fastcall ~TLoginDialog();
  void __fastcall Init(TForm * LinkedForm);
  bool __fastcall Execute(TList * DataList);
};
//----------------------------------------------------------------------------
#endif
