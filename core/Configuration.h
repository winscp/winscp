//---------------------------------------------------------------------------
#ifndef ConfigurationH
#define ConfigurationH

#include "CopyParam.h"
#include "FileBuffer.h"
#include "HierarchicalStorage.h"
//---------------------------------------------------------------------------
enum TInterface { ifCommander, ifExplorer };
enum TLogView { lvNone, lvWindow, pvPanel };
enum TOperationSide { osLocal, osRemote, osCurrent };
enum TEditor { edInternal, edExternal };
//---------------------------------------------------------------------------
#define C(Property) (Property != rhc.Property) ||
struct TScpExplorerConfiguration {
  AnsiString WindowParams;
  AnsiString DirViewParams;
  AnsiString CoolBarLayout;
  bool StatusBar;
  AnsiString LastLocalTargetDirectory;
  int ViewStyle;
  bool ShowFullAddress;
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(CoolBarLayout) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderPanelConfiguration {
  AnsiString DirViewParams;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool __fastcall operator !=(TScpCommanderPanelConfiguration & rhc)
    { return C(DirViewParams) C(CoolBarLayout) C(StatusBar) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderConfiguration {
  AnsiString WindowParams;
  float LocalPanelWidth;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool ToolBar;
  TOperationSide CurrentPanel;
  bool ExplorerStyleSelection;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(CoolBarLayout) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel) C(ToolBar)
      C(ExplorerStyleSelection) 0; };
};
//---------------------------------------------------------------------------
struct TEditorConfiguration {
  TEditor Editor;
  AnsiString ExternalEditor;
  AnsiString FontName;
  int FontHeight;
  int FontCharset;
  int FontStyle;
  bool WordWrap;
  AnsiString FindText;
  AnsiString ReplaceText;
  bool FindMatchCase;
  bool FindWholeWord;
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(Editor) C(ExternalEditor) C(FontName) C(FontHeight)
      C(FontCharset) C(FontStyle) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) 0; };
};
#undef C
//---------------------------------------------------------------------------
class TConfiguration : public TObject
{
private:
  bool FDontSave;
  bool FRandomSeedSave;
  TInterface FInterface;
  void * FApplicationInfo;
  AnsiString FAutoStartSession;
  bool FCopyOnDoubleClick;
  bool FCopyOnDoubleClickConfirmation;
  TCopyParamType FCopyParam;
  bool FCopyParamDialogExpanded;
  bool FDDAllowMove;
  bool FDDTransferConfirmation;
  bool FDefaultDirIsHome;
  bool FDeleteToRecycleBin;
  bool FDimmHiddenFiles;
  bool FErrorDialogExpanded;
  bool FChanged;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FLogging;
  AnsiString FLogFileName;
  int FLogWindowLines;
  bool FLogFileAppend;
  TLogView FLogView;
  bool FLogWindowOnStartup;
  AnsiString FLogWindowParams;
  AnsiString FMaskHistory;
  TNotifyEvent FOnChange;
  TScpCommanderConfiguration FScpCommander;
  TScpExplorerConfiguration FScpExplorer;
  bool FSelectDirectories;
  AnsiString FSelectMask;
  bool FShowHiddenFiles;
  bool FShowInaccesibleDirectories;
  bool FShowAdvancedLoginOptions;
  bool FConfirmOverwriting;
  bool FConfirmDeleting;
  int FUpdating;
  TStorage FStorage;
  AnsiString FIniFileStorageName;
  AnsiString FDDTemporaryDirectory;
  bool FDDWarnLackOfTempSpace;
  bool FConfirmClosingSession;
  double FDDWarnLackOfTempSpaceRatio;
  AnsiString FTemporarySessionFile;
  AnsiString FTemporaryKeyFile;
  TStrings * FBookmarks[2];
  TStrings * FCommandsHistory;
  TEditorConfiguration FEditor;
  bool FEmbeddedSessions;
  bool FExpertMode;

  AnsiString __fastcall GetStoredSessionsSubKey();
  AnsiString __fastcall GetPuttySessionsKey();
  AnsiString __fastcall GetPuttyRegistryStorageKey();
  void __fastcall SetRandomSeedFile(AnsiString value);
  AnsiString __fastcall GetRandomSeedFile();
  AnsiString __fastcall GetSshHostKeysSubKey();
  AnsiString __fastcall GetRootKeyStr();
  AnsiString __fastcall GetConfigurationSubKey();
  void __fastcall CleanupRegistry(AnsiString CleanupSubKey);
  TVSFixedFileInfo *__fastcall GetFixedApplicationInfo();
  void * __fastcall GetApplicationInfo();
  TEOLType __fastcall GetLocalEOLType();
  AnsiString __fastcall GetVersionStr();
  void __fastcall SetLogging(bool value);
  void __fastcall SetLogFileName(AnsiString value);
  void __fastcall SetLogToFile(bool value);
  bool __fastcall GetLogToFile();
  void __fastcall SetLogWindowLines(int value);
  void __fastcall SetLogWindowComplete(bool value);
  bool __fastcall GetLogWindowComplete();
  void __fastcall SetLogFileAppend(bool value);
  AnsiString __fastcall GetDefaultLogFileName();
  AnsiString __fastcall GetTimeFormat();
  void __fastcall SetCopyOnDoubleClick(bool value);
  void __fastcall SetCopyOnDoubleClickConfirmation(bool value);
  void __fastcall SetCopyParam(TCopyParamType value);
  void __fastcall SetDDAllowMove(bool value);
  void __fastcall SetDDTransferConfirmation(bool value);
  void __fastcall SetDefaultDirIsHome(bool value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetDimmHiddenFiles(bool value);
  void __fastcall SetIgnoreCancelBeforeFinish(TDateTime value);
  void __fastcall SetInterface(TInterface value);
  void __fastcall SetLogView(TLogView value);
  void __fastcall SetLogWindowOnStartup(bool value);
  void __fastcall SetLogWindowParams(AnsiString value);
  void __fastcall SetMaskHistory(AnsiString value);
  void __fastcall SetScpCommander(TScpCommanderConfiguration value);
  void __fastcall SetScpExplorer(TScpExplorerConfiguration value);
  void __fastcall SetSelectDirectories(bool value);
  void __fastcall SetShowHiddenFiles(bool value);
  void __fastcall SetShowInaccesibleDirectories(bool value);
  void __fastcall SetShowAdvancedLoginOptions(bool value);
  void __fastcall SetConfirmOverwriting(bool value);
  void __fastcall SetConfirmDeleting(bool value);
  void __fastcall SetStorage(TStorage value);
  TStorage __fastcall GetStorage();
  AnsiString __fastcall GetRegistryStorageKey();
  AnsiString __fastcall GetIniFileStorageName();
  void __fastcall SetIniFileStorageName(AnsiString value);
  void __fastcall SetDDTemporaryDirectory(AnsiString value);
  void __fastcall SetDDWarnLackOfTempSpace(bool value);
  void __fastcall SetConfirmClosingSession(bool value);
  void __fastcall SetDDWarnLackOfTempSpaceRatio(double value);
  void __fastcall SetBookmarks(TOperationSide Side, AnsiString Key, TStrings * value);
  TStrings * __fastcall GetBookmarks(TOperationSide Side, AnsiString Key);
  void __fastcall SetAutoStartSession(AnsiString value);
  void __fastcall SetCommandsHistory(TStrings * value);
  void __fastcall SetExpertMode(bool value);
  AnsiString __fastcall GetPartialExt() const;
  void __fastcall SetEditor(TEditorConfiguration value);
protected:
  virtual void __fastcall Changed();
  void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  void __fastcall FreeBookmarks();
  bool __fastcall DumpResourceToFile(
    const AnsiString ResName, const AnsiString FileName);
public:
  __fastcall TConfiguration();
  __fastcall ~TConfiguration();
  void __fastcall Default();
  void __fastcall Load();
  void __fastcall Save();
  void __fastcall CleanupConfiguration();
  void __fastcall CleanupIniFile();
  void __fastcall CleanupHostKeys();
  void __fastcall CleanupRandomSeedFile();
  void __fastcall RestoreForm(AnsiString Data, TCustomForm * Form);
  AnsiString __fastcall StoreForm(TCustomForm * Form);
  void __fastcall BeginUpdate();
  void __fastcall ClearTemporaryLoginData();
  void __fastcall EndUpdate();
  THierarchicalStorage * CreateScpStorage(bool SessionList);

  __property AnsiString StoredSessionsSubKey = {read=GetStoredSessionsSubKey};
  __property AnsiString PuttyRegistryStorageKey  = { read=GetPuttyRegistryStorageKey };
  __property AnsiString PuttySessionsKey  = { read=GetPuttySessionsKey };
  __property AnsiString RandomSeedFile  = { read=GetRandomSeedFile, write=SetRandomSeedFile };
  __property AnsiString SshHostKeysSubKey  = { read=GetSshHostKeysSubKey };
  __property AnsiString RootKeyStr  = { read=GetRootKeyStr };
  __property AnsiString ConfigurationSubKey  = { read=GetConfigurationSubKey };
  __property bool DontSave  = { read=FDontSave, write=FDontSave };
  __property bool RandomSeedSave  = { read=FRandomSeedSave, write=FRandomSeedSave };
  __property TVSFixedFileInfo *FixedApplicationInfo  = { read=GetFixedApplicationInfo };
  __property void * ApplicationInfo  = { read=GetApplicationInfo };
  __property AnsiString AutoStartSession = { read = FAutoStartSession, write = SetAutoStartSession };
  __property bool CopyOnDoubleClick = { read = FCopyOnDoubleClick, write = SetCopyOnDoubleClick };
  __property bool CopyOnDoubleClickConfirmation = { read = FCopyOnDoubleClickConfirmation, write = SetCopyOnDoubleClickConfirmation };
  __property TCopyParamType CopyParam = { read = FCopyParam, write = SetCopyParam };
  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool DDAllowMove = { read = FDDAllowMove, write = SetDDAllowMove };
  __property bool DDTransferConfirmation = { read = FDDTransferConfirmation, write = SetDDTransferConfirmation };
  __property bool DefaultDirIsHome = { read = FDefaultDirIsHome, write = SetDefaultDirIsHome };
  __property TEOLType LocalEOLType = { read = GetLocalEOLType };
  __property AnsiString VersionStr  = { read=GetVersionStr };
  __property bool Logging  = { read=FLogging, write=SetLogging };
  __property AnsiString LogFileName  = { read=FLogFileName, write=SetLogFileName };
  __property bool LogToFile  = { read=GetLogToFile, write=SetLogToFile };
  __property bool LogFileAppend  = { read=FLogFileAppend, write=SetLogFileAppend };
  __property TLogView LogView = { read = FLogView, write = SetLogView };
  __property bool LogWindowOnStartup = { read = FLogWindowOnStartup, write = SetLogWindowOnStartup };
  __property int LogWindowLines  = { read=FLogWindowLines, write=SetLogWindowLines };
  __property bool LogWindowComplete  = { read=GetLogWindowComplete, write=SetLogWindowComplete };
  __property AnsiString DefaultLogFileName  = { read=GetDefaultLogFileName };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool DimmHiddenFiles = { read = FDimmHiddenFiles, write = SetDimmHiddenFiles };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = SetIgnoreCancelBeforeFinish };
  __property TInterface Interface = { read = FInterface, write = SetInterface };
  __property AnsiString LogWindowParams = { read = FLogWindowParams, write = SetLogWindowParams };
  __property AnsiString MaskHistory = { read = FMaskHistory, write = SetMaskHistory };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property TScpCommanderConfiguration ScpCommander = { read = FScpCommander, write = SetScpCommander };
  __property TScpExplorerConfiguration ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool SelectDirectories = { read = FSelectDirectories, write = SetSelectDirectories };
  __property AnsiString SelectMask = { read = FSelectMask, write = FSelectMask };
  __property bool ShowHiddenFiles = { read = FShowHiddenFiles, write = SetShowHiddenFiles };
  __property bool ShowInaccesibleDirectories = { read = FShowInaccesibleDirectories, write = SetShowInaccesibleDirectories };
  __property bool ShowAdvancedLoginOptions = { read = FShowAdvancedLoginOptions, write = SetShowAdvancedLoginOptions};
  __property bool ConfirmOverwriting = { read = FConfirmOverwriting, write = SetConfirmOverwriting};
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
  __property AnsiString PartialExt = {read=GetPartialExt};

  __property AnsiString TimeFormat = { read = GetTimeFormat };
  __property TStorage Storage  = { read=GetStorage, write=SetStorage };
  __property AnsiString RegistryStorageKey  = { read=GetRegistryStorageKey };
  __property AnsiString IniFileStorageName  = { read=GetIniFileStorageName, write=SetIniFileStorageName };
  __property AnsiString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=SetDDTemporaryDirectory };
  __property bool DDWarnLackOfTempSpace  = { read=FDDWarnLackOfTempSpace, write=SetDDWarnLackOfTempSpace };
  __property bool ConfirmClosingSession  = { read=FConfirmClosingSession, write=SetConfirmClosingSession };
  __property double DDWarnLackOfTempSpaceRatio  = { read=FDDWarnLackOfTempSpaceRatio, write=SetDDWarnLackOfTempSpaceRatio };
  __property TStrings * Bookmarks[TOperationSide Side][AnsiString Key] = { read = GetBookmarks, write = SetBookmarks };
  __property TStrings * CommandsHistory = { read = FCommandsHistory, write = SetCommandsHistory };
  __property TEditorConfiguration Editor = { read = FEditor, write = SetEditor };
  __property bool EmbeddedSessions = { read = FEmbeddedSessions };
  __property AnsiString EmbeddedKeyFile = { read = FTemporaryKeyFile };
  __property bool ExpertMode = { read = FExpertMode, write = SetExpertMode };
};
//---------------------------------------------------------------------------
bool SpecialFolderLocation(int PathID, AnsiString & Path);
//---------------------------------------------------------------------------
#endif
