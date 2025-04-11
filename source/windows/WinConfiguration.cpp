//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "Common.h"
#include "WinConfiguration.h"
#include "Exceptions.h"
#include "Bookmarks.h"
#include "Terminal.h"
#include "TextsWin.h"
#include "WinInterface.h"
#include "GUITools.h"
#include "Tools.h"
#include "Setup.h"
#include "Security.h"
#include "TerminalManager.h"
#include "Cryptography.h"
#include <VCLCommon.h>
#include <InitGUID.h>
#include <DragExt.h>
#include <Math.hpp>
#include <StrUtils.hpp>
#include <OperationWithTimeout.hpp>
#include "FileInfo.h"
#include "CoreMain.h"
#include "DriveView.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TWinConfiguration * WinConfiguration = NULL;
//---------------------------------------------------------------------------
static UnicodeString NotepadName(L"notepad.exe");
static UnicodeString ToolbarsLayoutKey(L"ToolbarsLayout2");
static UnicodeString ToolbarsLayoutOldKey(L"ToolbarsLayout");
TDateTime DefaultUpdatesPeriod(7);
// WORKAROUND (the semicolon, see TCustomListViewColProperties.GetParamsStr, and see other instances below)
const UnicodeString ScpExplorerDirViewParamsDefault =
  L"0;1;0|150,1;70,1;150,1;79,1;62,1;55,0;20,0;150,0;125,0;@" + SaveDefaultPixelsPerInch() + L"|6;7;8;0;1;2;3;4;5";
const UnicodeString ScpCommanderRemotePanelDirViewParamsDefault = ScpExplorerDirViewParamsDefault;
const UnicodeString ScpCommanderLocalPanelDirViewParamsDefault =
  L"0;1;0|150,1;70,1;120,1;150,1;55,0;55,0;@" + SaveDefaultPixelsPerInch() + L"|5;0;1;2;3;4";
UnicodeString QueueViewLayoutDefault;
UnicodeString ScpCommanderWindowParamsDefault;
UnicodeString ScpExplorerWindowParamsDefault;
//---------------------------------------------------------------------------
static const wchar_t FileColorDataSeparator = L':';
TFileColorData::TFileColorData() :
  Color(TColor())
{
}
//---------------------------------------------------------------------------
void TFileColorData::Load(const UnicodeString & S)
{
  UnicodeString Buf(S);
  Color = RestoreColor(CutToChar(Buf, FileColorDataSeparator, true));
  FileMask = Buf;
}
//---------------------------------------------------------------------------
UnicodeString TFileColorData::Save() const
{
  UnicodeString Result = StoreColor(Color) + FileColorDataSeparator + FileMask.Masks;
  return Result;
}
//---------------------------------------------------------------------------
void TFileColorData::LoadList(const UnicodeString & S, TList & List)
{
  std::unique_ptr<TStringList> Strings(CommaTextToStringList(S));

  List.clear();
  for (int Index = 0; Index < Strings->Count; Index++)
  {
    TFileColorData FileColorData;
    FileColorData.Load(Strings->Strings[Index]);
    List.push_back(FileColorData);
  }
}
//---------------------------------------------------------------------------
UnicodeString TFileColorData::SaveList(const TList & List)
{
  std::unique_ptr<TStringList> Strings(new TStringList());
  for (TFileColorData::TList::const_iterator Iter = List.begin(); Iter != List.end(); Iter++)
  {
    Strings->Add((*Iter).Save());
  }
  return Strings->CommaText;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TEditorData::TEditorData() :
  FileMask(AnyMask),
  Editor(edInternal),
  ExternalEditor(L""),
  ExternalEditorText(false),
  SDIExternalEditor(false),
  DetectMDIExternalEditor(false)
{
}
//---------------------------------------------------------------------------
__fastcall TEditorData::TEditorData(const TEditorData & Source) :
  FileMask(Source.FileMask),
  Editor(Source.Editor),
  ExternalEditor(Source.ExternalEditor),
  ExternalEditorText(Source.ExternalEditorText),
  SDIExternalEditor(Source.SDIExternalEditor),
  DetectMDIExternalEditor(Source.DetectMDIExternalEditor)
{
}
//---------------------------------------------------------------------------
#define C(Property) (Property == rhd.Property)
bool __fastcall TEditorData::operator==(const TEditorData & rhd) const
{
  return
    C(FileMask) &&
    C(Editor) &&
    C(ExternalEditor) &&
    C(ExternalEditorText) &&
    C(SDIExternalEditor) &&
    C(DetectMDIExternalEditor) &&
    true;
}
#undef C
//---------------------------------------------------------------------------
void __fastcall TEditorData::ExternalEditorOptionsAutodetect()
{
  UnicodeString Command = ExternalEditor;
  ReformatFileNameCommand(Command);
  UnicodeString ProgramName = ExtractProgramName(Command);
  // We explicitly do not use TEditorPreferences::GetDefaultExternalEditor(),
  // as we need to explicitly refer to the Notepad, even if the default external
  // editor ever changes
  UnicodeString NotepadProgramName = ExtractProgramName(NotepadName);
  if (SameText(ProgramName, NotepadProgramName))
  {
    // By default we use default transfer mode (binary),
    // as all reasonable 3rd party editors support all EOL styles.
    // A notable exception is Windows Notepad before Windows 10 1809, so here's an exception for it.
    ExternalEditorText = !IsWin10Build(17763);

    // While on Windows 11, the notepad.exe open the MDI Store Windows Notepad,
    // the notepad.exe process runs as long as the Windows Notepad tab is opened,
    // so technically the notepad.exe is stil SDI
    SDIExternalEditor = true;
  }
}
//---------------------------------------------------------------------------
__fastcall TEditorPreferences::TEditorPreferences()
{
}
//---------------------------------------------------------------------------
__fastcall TEditorPreferences::TEditorPreferences(const TEditorData & Data) :
  FData(Data)
{
}
//---------------------------------------------------------------------------
bool __fastcall TEditorPreferences::operator==(const TEditorPreferences & rhp) const
{
  return (FData == rhp.FData);
}
#undef C
//---------------------------------------------------------------------------
bool __fastcall TEditorPreferences::Matches(const UnicodeString FileName,
  bool Local, const TFileMasks::TParams & Params) const
{
  return FData.FileMask.Matches(FileName, Local, false, &Params);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TEditorPreferences::GetDefaultExternalEditor()
{
  return NotepadName;
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::LegacyDefaults()
{
  FData.ExternalEditor = GetDefaultExternalEditor();
  FData.ExternalEditorOptionsAutodetect();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TEditorPreferences::ExtractExternalEditorName() const
{
  DebugAssert(FData.Editor == edExternal);
  UnicodeString ExternalEditor = FData.ExternalEditor;
  ReformatFileNameCommand(ExternalEditor);
  // Trim is a workaround for unknown problem with "notepad  " (2 trailing spaces)
  return ExtractProgramName(ExternalEditor).Trim();
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::Load(THierarchicalStorage * Storage, bool Legacy)
{
  if (!Legacy)
  {
    FData.FileMask = Storage->ReadString(L"FileMask", FData.FileMask.Masks);
  }
  FData.Editor = (TEditor)Storage->ReadInteger(L"Editor", FData.Editor);
  FData.ExternalEditor = Storage->ReadString(L"ExternalEditor", FData.ExternalEditor);
  FData.ExternalEditorText = Storage->ReadBool(L"ExternalEditorText", FData.ExternalEditorText);
  FData.SDIExternalEditor = Storage->ReadBool(L"SDIExternalEditor", FData.SDIExternalEditor);
  FData.DetectMDIExternalEditor = Storage->ReadBool(L"DetectMDIExternalEditor", FData.DetectMDIExternalEditor);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteString(L"FileMask", FData.FileMask.Masks);
  Storage->WriteInteger(L"Editor", FData.Editor);
  Storage->WriteString(L"ExternalEditor", FData.ExternalEditor);
  Storage->WriteBool(L"ExternalEditorText", FData.ExternalEditorText);
  Storage->WriteBool(L"SDIExternalEditor", FData.SDIExternalEditor);
  Storage->WriteBool(L"DetectMDIExternalEditor", FData.DetectMDIExternalEditor);
}
//---------------------------------------------------------------------------
TEditorData * __fastcall TEditorPreferences::GetData()
{
  // returning non-const data, possible data change, invalidate cached name
  FName = L"";
  return &FData;
};
//---------------------------------------------------------------------------
UnicodeString __fastcall TEditorPreferences::GetName() const
{
  if (FName.IsEmpty())
  {
    if (FData.Editor == edInternal)
    {
      // StripHotkey is relic from times when INTERNAL_EDITOR_NAME was used
      // also for the menu item caption
      FName = StripHotkey(LoadStr(INTERNAL_EDITOR_NAME));
    }
    else if (FData.Editor == edOpen)
    {
      FName = StripHotkey(LoadStr(OPEN_EDITOR_NAME));
    }
    else
    {
      UnicodeString Program, Params, Dir;
      UnicodeString ExternalEditor = FData.ExternalEditor;
      ReformatFileNameCommand(ExternalEditor);
      SplitCommand(ExternalEditor, Program, Params, Dir);
      FName = ExtractFileName(Program);
      int P = FName.LastDelimiter(L".");
      if (P > 0)
      {
        FName.SetLength(P - 1);
      }

      if (FName.ByteType(1) == mbSingleByte)
      {
        if (FName.UpperCase() == FName)
        {
          FName = FName.LowerCase();
        }

        if (FName.LowerCase() == FName)
        {
          FName = FName.SubString(1, 1).UpperCase() +
            FName.SubString(2, FName.Length() - 1);
        }
      }
    }
  }

  return FName;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TEditorList::TEditorList()
{
  Init();
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Init()
{
  FEditors = new TList();
  FModified = false;
}
//---------------------------------------------------------------------------
__fastcall TEditorList::~TEditorList()
{
  Clear();
  delete FEditors;
}
//---------------------------------------------------------------------
void __fastcall TEditorList::Modify()
{
  FModified = true;
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Saved()
{
  FModified = false;
}
//---------------------------------------------------------------------------
TEditorList & __fastcall TEditorList::operator=(const TEditorList & rhl)
{
  Clear();

  for (int Index = 0; Index < rhl.Count; Index++)
  {
    Add(new TEditorPreferences(*rhl.Editors[Index]));
  }
  // there should be comparison of with the assigned list, but we rely on caller
  // to do it instead (TWinConfiguration::SetEditorList)
  Modify();
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TEditorList::operator==(const TEditorList & rhl) const
{
  bool Result = (Count == rhl.Count);
  if (Result)
  {
    int i = 0;
    while ((i < Count) && Result)
    {
      Result = (*Editors[i]) == (*rhl.Editors[i]);
      i++;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Clear()
{
  for (int i = 0; i < Count; i++)
  {
    delete Editors[i];
  }
  FEditors->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Add(TEditorPreferences * Editor)
{
  Insert(Count, Editor);
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Insert(int Index, TEditorPreferences * Editor)
{
  FEditors->Insert(Index, reinterpret_cast<TObject *>(Editor));
  Modify();
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Change(int Index, TEditorPreferences * Editor)
{
  if (!((*Editors[Index]) == *Editor))
  {
    delete Editors[Index];
    FEditors->Items[Index] = (reinterpret_cast<TObject *>(Editor));
    Modify();
  }
  else
  {
    delete Editor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Move(int CurIndex, int NewIndex)
{
  if (CurIndex != NewIndex)
  {
    FEditors->Move(CurIndex, NewIndex);
    Modify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Delete(int Index)
{
  DebugAssert((Index >= 0) && (Index < Count));
  delete Editors[Index];
  FEditors->Delete(Index);
  Modify();
}
//---------------------------------------------------------------------------
const TEditorPreferences * __fastcall TEditorList::Find(
  const UnicodeString FileName, bool Local, const TFileMasks::TParams & Params) const
{
  const TEditorPreferences * Result = NULL;
  int i = 0;
  while ((i < FEditors->Count) && (Result == NULL))
  {
    Result = Editors[i];
    if (!Result->Matches(FileName, Local, Params))
    {
      Result = NULL;
    }
    i++;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Load(THierarchicalStorage * Storage)
{
  int Index = 0;
  bool Next;

  do
  {
    UnicodeString Name = IntToStr(Index);
    TEditorPreferences * Editor = NULL;
    try
    {
      Next = Storage->OpenSubKey(Name, false);
      if (Next)
      {
        try
        {
          Editor = new TEditorPreferences();
          Editor->Load(Storage, false);
        }
        __finally
        {
          Storage->CloseSubKey();
        }
      }
    }
    catch(...)
    {
      delete Editor;
      throw;
    }

    if (Editor != NULL)
    {
      FEditors->Add(reinterpret_cast<TObject *>(Editor));
    }

    Index++;
  }
  while (Next);

  FModified = false;
}
//---------------------------------------------------------------------------
void __fastcall TEditorList::Save(THierarchicalStorage * Storage) const
{
  Storage->ClearSubKeys();
  for (int Index = 0; Index < Count; Index++)
  {
    if (Storage->OpenSubKey(IntToStr(Index), true))
    {
      try
      {
        Editors[Index]->Save(Storage);
      }
      __finally
      {
        Storage->CloseSubKey();
      }
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TEditorList::GetCount() const
{
  int X = FEditors->Count;
  return X;
}
//---------------------------------------------------------------------------
const TEditorPreferences * __fastcall TEditorList::GetEditor(int Index) const
{
  return reinterpret_cast<TEditorPreferences *>(FEditors->Items[Index]);
}
//---------------------------------------------------------------------------
bool __fastcall TEditorList::IsDefaultList() const
{
  bool Result = true;
  for (int Index = 0; Result && (Index < Count); Index++)
  {
    const TEditorPreferences * Editor = GetEditor(Index);
    if (Editor->Data->Editor == edInternal)
    {
      // noop (keeps Result true)
    }
    else if (Editor->Data->Editor == edExternal)
    {
      UnicodeString ExternalEditor = Editor->ExtractExternalEditorName();
      UnicodeString DefaultExternalEditor = ExtractProgramName(TEditorPreferences::GetDefaultExternalEditor());
      Result = SameText(ExternalEditor, DefaultExternalEditor);
    }
    else
    {
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::TWinConfiguration(): TCustomWinConfiguration()
{
  FInvalidDefaultTranslationMessage = L"";
  FDDExtInstalled = -1;
  FBookmarks = new TBookmarks();
  FCustomCommandList = new TCustomCommandList();
  FExtensionList = new TCustomCommandList();
  FEditorList = new TEditorList();
  FDefaultUpdatesPeriod = 0;
  FDontDecryptPasswords = 0;
  FMasterPasswordSession = 0;
  FMasterPasswordSessionAsked = false;
  FCustomCommandOptions.reset(new TStringList());
  FCustomCommandOptionsModified = false;
  FExtensionTranslations.reset(new TStringList());
  Default();

  // This matters only if the translations are in the executable folder and auto-loaded by VCL (System.Pas - DelayLoadResourceModule)
  try
  {
    UnicodeString ResourceModuleName = GetResourceModuleName(Application->Name, ModuleFileName().c_str());
    CheckTranslationVersion(ResourceModuleName, true);
  }
  catch(Exception & E)
  {
    FInvalidDefaultTranslationMessage = E.Message;
  }

  // Load complete locale according to the UI language
  SetLocaleInternal(NULL, true, true);
  FDefaultLocale = AppliedLocale;
}
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::~TWinConfiguration()
{
  if (!FTemporarySessionFile.IsEmpty()) DeleteFile(ApiPath(FTemporarySessionFile));
  ClearTemporaryLoginData();
  ReleaseExtensionTranslations();

  delete FBookmarks;
  delete FCustomCommandList;
  delete FExtensionList;
  delete FEditorList;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Default()
{
  FCustomCommandsDefaults = true;
  FCustomCommandOptionsModified = false;

  TCustomWinConfiguration::Default();

  int WorkAreaWidthScaled = DimensionToDefaultPixelsPerInch(Screen->WorkAreaWidth);
  int WorkAreaHeightScaled = DimensionToDefaultPixelsPerInch(Screen->WorkAreaHeight);
  UnicodeString PixelsPerInchToolbarValue = "PixelsPerInch=" + SaveDefaultPixelsPerInch();

  FDDDisableMove = false;
  FDDTransferConfirmation = asAuto;
  FDDTemporaryDirectory = L"";
  FDDDrives = L"";
  FDDWarnLackOfTempSpace = true;
  FDDWarnLackOfTempSpaceRatio = 1.1;
  FDDFakeFile = true;
  FDDExtTimeout = MSecsPerSec;
  FDeleteToRecycleBin = true;
  FSelectDirectories = false;
  FSelectMask = AnyMask;
  FShowHiddenFiles = false;
  FFormatSizeBytes = fbKilobytes;
  FPanelSearch = isNameStartOnly;
  FShowInaccesibleDirectories = true;
  FConfirmTransferring = true;
  FConfirmDeleting = true;
  FConfirmRecycling = true;
  FConfirmClosingSession = true;
  FDoubleClickAction = dcaEdit;
  FCopyOnDoubleClickConfirmation = false;
  FAlwaysRespectDoubleClickAction = false;
  FDimmHiddenFiles = true;
  FRenameWholeName = false;
  FAutoStartSession = L"";
  FExpertMode = true;
  FUseLocationProfiles = false;
  FUseSharedBookmarks = false;
  FDefaultDirIsHome = true;
  FDDDeleteDelay = 120;
  FTemporaryDirectoryAppendSession = false;
  FTemporaryDirectoryDeterministic = false;
  FTemporaryDirectoryAppendPath = true;
  FTemporaryDirectoryCleanup = true;
  FConfirmTemporaryDirectoryCleanup = true;
  FPreservePanelState = true;
  FDarkTheme = asAuto;
  FLastStoredSession = L"";
  // deliberately not being saved, so that when saving ad-hoc workspace,
  // we do not offer to overwrite the last saved workspace, what may be undesirable
  FLastWorkspace = L"";
  FAutoSaveWorkspace = false;
  FAutoSaveWorkspacePasswords = false;
  FAutoWorkspace = L"";
  FPathInCaption = picShort;
  FSessionTabNameFormat = stnfShortPathTrunc;
  FMinimizeToTray = false;
  FBalloonNotifications = true;
  FNotificationsTimeout = 10;
  FNotificationsStickTime = 2;
  FCopyParamAutoSelectNotice = true;
  FLockToolbars = false;
  FSelectiveToolbarText = true;
  FLargerToolbar = 1;
  FAutoOpenInPutty = false;
  FRefreshRemotePanel = false;
  FRefreshRemotePanelInterval = TDateTime(0, 1, 0, 0);
  FPanelFont.FontName = L"";
  FPanelFont.FontSize = 0;
  FPanelFont.FontStyle = 0;
  FPanelFont.FontCharset = DEFAULT_CHARSET;
  FNaturalOrderNumericalSorting = true;
  FAlwaysSortDirectoriesByName = false;
  FFullRowSelect = false;
  FOfferedEditorAutoConfig = false;
  FVersionHistory = L"";
  AddVersionToHistory();
  FUseMasterPassword = false;
  FPlainMasterPasswordEncrypt = L"";
  FPlainMasterPasswordDecrypt = L"";
  FMasterPasswordVerifier = L"";
  FOpenedStoredSessionFolders = L"";
  FAutoImportedFromPuttyOrFilezilla = false;
  FGenerateUrlComponents = -1;
  FGenerateUrlCodeTarget = guctUrl;
  FGenerateUrlScriptFormat = sfScriptFile;
  FGenerateUrlAssemblyLanguage = alCSharp;
  FExternalSessionInExistingInstance = true;
  FShowLoginWhenNoSession = true;
  FKeepOpenWhenNoSession = true;
  FDefaultToNewRemoteTab = true;
  FLocalIconsByExt = false;
  FFlashTaskbar = true;
  FMaxSessions = 100;
  FBidiModeOverride = lfoLanguageIfRecommended;
  FFlipChildrenOverride = lfoLanguageIfRecommended;
  FShowTips = true;
  FTipsSeen = L"";
  FTipsShown = Now();
  FFileColors = L"";
  FRunsSinceLastTip = 0;
  FExtensionsDeleted = L"";
  FLockedInterface = false;

  HonorDrivePolicy = 1;
  UseABDrives = true;
  TimeoutShellOperations = true;
  TimeoutShellIconRetrieval = false;
  UseIconUpdateThread = true;
  AllowWindowPrint = false;
  StoreTransition = stInit;
  QueueTransferLimitMax = 9;
  HiContrast = false;
  EditorCheckNotModified = false;
  SessionTabCaptionTruncation = true;
  LoadingTooLongLimit = 15;
  RemoteThumbnailMask = EmptyStr;
  RemoteThumbnailSizeLimit = 50 * 1024;
  FirstRun = StandardDatestamp();

  FEditor.Font.FontName = DefaultFixedWidthFontName;
  FEditor.Font.FontSize = DefaultFixedWidthFontSize;
  FEditor.Font.FontStyle = 0;
  FEditor.Font.FontCharset = DEFAULT_CHARSET;
  FEditor.FontColor = TColor(0);
  FEditor.BackgroundColor = TColor(0);
  FEditor.WordWrap = false;
  FEditor.FindText = L"";
  FEditor.ReplaceText = L"";
  FEditor.FindMatchCase = false;
  FEditor.FindWholeWord = false;
  FEditor.FindDown = true;
  FEditor.TabSize = 8;
  FEditor.MaxEditors = 500;
  FEditor.EarlyClose = 2; // seconds
  FEditor.SDIShellEditor = false;
  FEditor.WindowParams = L"";
  FEditor.Encoding = CP_ACP;
  FEditor.WarnOnEncodingFallback = true;
  FEditor.WarnOrLargeFileSize = true;
  FEditor.AutoFont = true;
  FEditor.DisableSmoothScroll = false;

  FQueueView.Height = 140;
  FQueueView.HeightPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  if (QueueViewLayoutDefault.IsEmpty())
  {
    // with 1000 pixels wide screen, both interfaces are wide enough to fit wider queue
    QueueViewLayoutDefault =
      UnicodeString((WorkAreaWidthScaled > 1000) ? L"70,250,250,80,80,80,100" : L"70,160,160,80,80,80,100") +
      // WORKAROUND (the comma), see GetListViewStr
      L",;" + SaveDefaultPixelsPerInch();
  }
  FQueueView.Layout = QueueViewLayoutDefault;
  FQueueView.Show = qvHideWhenEmpty;
  FQueueView.LastHideShow = qvHideWhenEmpty;
  FQueueView.ToolBar = true;
  FQueueView.Label = true;
  FQueueView.FileList = false;
  FQueueView.FileListHeight = 90;
  FQueueView.FileListHeightPixelsPerInch = USER_DEFAULT_SCREEN_DPI;

  FEnableQueueByDefault = true;

  FUpdates.Period = FDefaultUpdatesPeriod;
  FUpdates.LastCheck = 0;
  FUpdates.HaveResults = false;
  FUpdates.ShownResults = false;
  FUpdates.BetaVersions = asAuto;
  FUpdates.ShowOnStartup = true;
  FUpdates.AuthenticationEmail = L"";
  FUpdates.Mode = EmptyStr;
  // for backward compatibility the default is decided based on value of ProxyHost
  FUpdates.ConnectionType = (TConnectionType)-1;
  FUpdates.ProxyHost = L""; // keep empty (see above)
  FUpdates.ProxyPort = 8080;
  FUpdates.Results.Reset();
  FUpdates.DotNetVersion = L"";
  FUpdates.ConsoleVersion = L"";

  int ExplorerWidth = Min(WorkAreaWidthScaled - 40, 960);
  int ExplorerHeight = Min(WorkAreaHeightScaled - 30, 720);
  if (ScpExplorerWindowParamsDefault.IsEmpty())
  {
    ScpExplorerWindowParamsDefault = FormatDefaultWindowParams(ExplorerWidth, ExplorerHeight);
  }
  FScpExplorer.WindowParams = ScpExplorerWindowParamsDefault;

  FScpExplorer.DirViewParams = ScpExplorerDirViewParamsDefault;
  FScpExplorer.ToolbarsLayout =
    UnicodeString(
      L"Queue=1::0+-1,"
       "Menu=1:TopDock:0+0,"
       "Buttons=1:TopDock:2+0,"
       "Selection=0:TopDock:3+0,"
       "Session=0:TopDock:6+0,"
       "Preferences=1:TopDock:4+0,"
       "Sort=0:TopDock:5+0,"
       "Address=1:TopDock:1+0,"
       "Updates=1:TopDock:4+393,"
       "Transfer=1:TopDock:4+171,"
       "CustomCommands=0:TopDock:7+0,") +
    PixelsPerInchToolbarValue;
  FScpExplorer.ToolbarsButtons = UnicodeString();
  FScpExplorer.SessionsTabs = true;
  FScpExplorer.StatusBar = true;
  FScpExplorer.LastLocalTargetDirectory = GetPersonalFolder();
  FScpExplorer.ViewStyle = dvsIcon;
  FScpExplorer.ShowFullAddress = true;
  FScpExplorer.DriveView = true;
  FScpExplorer.DriveViewWidth = 180;
  FScpExplorer.DriveViewWidthPixelsPerInch = USER_DEFAULT_SCREEN_DPI;

  int CommanderWidth = Min(WorkAreaWidthScaled - 40, 1090);
  int CommanderHeight = Min(WorkAreaHeightScaled - 30, 700);
  if (ScpCommanderWindowParamsDefault.IsEmpty())
  {
    ScpCommanderWindowParamsDefault = FormatDefaultWindowParams(CommanderWidth, CommanderHeight);
  }
  FScpCommander.WindowParams = ScpCommanderWindowParamsDefault;

  FScpCommander.LocalPanelWidth = 0.5;
  FScpCommander.SwappedPanels = false;
  FScpCommander.SessionsTabs = true;
  FScpCommander.StatusBar = true;
  FScpCommander.NortonLikeMode = nlKeyboard;
  FScpCommander.PreserveLocalDirectory = false;
  FScpCommander.ToolbarsLayout =
    UnicodeString(
      L"Queue=1::0+-1,"
       "Menu=1:TopDock:0+0,"
       "Preferences=1:TopDock:1+228,"
       "Session=0:TopDock:1+602,"
       "Sort=0:TopDock:2+0,"
       "Commands=1:TopDock:1+0,"
       "Updates=1:TopDock:1+596,"
       "Transfer=1:TopDock:1+341,"
       "CustomCommands=0:TopDock:3+0,"
       "RemoteHistory=1:RemoteTopDock:0+172,"
       "RemoteNavigation=1:RemoteTopDock:0+252,"
       "RemotePath=1:RemoteTopDock:0+0,"
       "RemoteFile=1:RemoteTopDock:1+0,"
       "RemoteSelection=1:RemoteTopDock:1+345,"
       "LocalHistory=1:LocalTopDock:0+207,"
       "LocalNavigation=1:LocalTopDock:0+287,"
       "LocalPath=1:LocalTopDock:0+0,"
       "LocalFile=1:LocalTopDock:1+0,"
       "LocalSelection=1:LocalTopDock:1+329,"
       "Toolbar2=0:BottomDock:1+0,"
       "CommandLine=0:BottomDock:0+0,") +
    PixelsPerInchToolbarValue;
  FScpCommander.ToolbarsButtons = UnicodeString();
  FScpCommander.CurrentPanel = osLocal;
  FScpCommander.CompareByTime = true;
  FScpCommander.CompareBySize = false;
  FScpCommander.TreeOnLeft = false;
  FScpCommander.ExplorerKeyboardShortcuts = false;
  FScpCommander.SystemContextMenu = false;
  FScpCommander.RemotePanel.DirViewParams = ScpCommanderRemotePanelDirViewParamsDefault;
  FScpCommander.RemotePanel.ViewStyle = dvsReport;
  FScpCommander.RemotePanel.StatusBar = true;
  FScpCommander.RemotePanel.DriveView = false;
  FScpCommander.RemotePanel.DriveViewHeight = 100;
  FScpCommander.RemotePanel.DriveViewHeightPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  FScpCommander.RemotePanel.DriveViewWidth = 100;
  FScpCommander.RemotePanel.DriveViewWidthPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  FScpCommander.RemotePanel.LastPath = UnicodeString();
  FScpCommander.LocalPanel.DirViewParams = ScpCommanderLocalPanelDirViewParamsDefault;
  FScpCommander.LocalPanel.ViewStyle = dvsReport;
  FScpCommander.LocalPanel.StatusBar = true;
  FScpCommander.LocalPanel.DriveView = false;
  FScpCommander.LocalPanel.DriveViewHeight = 100;
  FScpCommander.LocalPanel.DriveViewHeightPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  FScpCommander.LocalPanel.DriveViewWidth = 100;
  FScpCommander.LocalPanel.DriveViewWidthPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  FScpCommander.LocalPanel.LastPath = UnicodeString();
  FScpCommander.OtherLocalPanelDirViewParams = FScpCommander.LocalPanel.DirViewParams;
  FScpCommander.OtherLocalPanelViewStyle = FScpCommander.LocalPanel.ViewStyle;
  FScpCommander.OtherLocalPanelLastPath = UnicodeString();

  FBookmarks->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::DefaultLocalized()
{
  TGUIConfiguration::DefaultLocalized();

  if (FCustomCommandsDefaults)
  {
    FCustomCommandList->Clear();
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_EXECUTE), L"\"./!\"", 0);
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_TOUCH), L"touch \"!\"", ccApplyToDirectories | ccRecursive);
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_TAR),
      FORMAT(L"tar -cz  -f \"!?%s?archive.tgz!\" !&",
        (LoadStr(CUSTOM_COMMAND_TAR_ARCHIVE))), ccApplyToDirectories);
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_UNTAR),
      FORMAT(L"tar -xz --directory=\"!?%s?.!\" -f \"!\"",
        (LoadStr(CUSTOM_COMMAND_UNTAR_DIRECTORY))), 0);
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_GREP),
      FORMAT(L"grep \"!?%s?!\" !&", (LoadStr(CUSTOM_COMMAND_GREP_PATTERN))),
      ccShowResults);
    FCustomCommandList->Add(LoadStr(CUSTOM_COMMAND_PRINT), L"notepad.exe /p \"!\"", ccLocal);
    FCustomCommandList->Reset();
    FCustomCommandsDefaults = true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::DetectRegistryStorage(HKEY RootKey)
{
  bool Result = false;
  TRegistryStorage * Storage = new TRegistryStorage(RegistryStorageKey, RootKey);
  try
  {
    if (Storage->OpenRootKey(false))
    {
      Result = true;
      Storage->CloseSubKey();
    }
  }
  __finally
  {
    delete Storage;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::CanWriteToStorage()
{
  bool Result = false;
  try
  {
    THierarchicalStorage * Storage = CreateConfigStorage();
    try
    {
      Storage->AccessMode = smReadWrite;
      // This is actually not very good test, as we end up potentially with
      // the very same config, and TIniFileStorage file won't even try to
      // write the file then. Luckily, we use this for empty config only,
      // so we end up with at least an empty section.
      if (Storage->OpenSubKey(ConfigurationSubKey, true))
      {
        Storage->WriteBool(L"WriteTest", true);
        Storage->DeleteValue(L"WriteTest");
      }
      Storage->Flush();
    }
    __finally
    {
      delete Storage;
    }
    Result = true;
  }
  catch(...)
  {
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TWinConfiguration::DetectStorage(bool SafeOnly)
{
  bool Result;
  UnicodeString IniFile = IniFileStorageNameForReading;
  if (FileExists(ApiPath(IniFile)))
  {
    Result = !SafeOnly;
    if (Result)
    {
      FStorage = stIniFile;
      AppLogFmt(L"Will use existing INI file %s for configuration", (IniFile));
    }
  }
  else
  {
    if (IsUWP() ||
        DetectRegistryStorage(HKEY_CURRENT_USER) ||
        DetectRegistryStorage(HKEY_LOCAL_MACHINE))
    {
      FStorage = stRegistry;
      if (IsUWP())
      {
        AppLog(L"Will use registry for configuration as this is store installation");
      }
      else
      {
        AppLog(L"Will use existing registry for configuration");
      }
      Result = true;
    }
    else
    {
      if (SafeOnly)
      {
        Result = false;
        AppLog(L"No configuration found");
      }
      else
      {
        FStorage = stIniFile;
        // As we fall back to user profile folder, when application folder
        // is not writtable, it is actually unlikely that the below test ever fails.
        if (!CanWriteToStorage())
        {
          FStorage = stRegistry;
          AppLog(L"Will use registry configuration as no configuration was found and default INI file location is not writtable");
        }
        else
        {
          AppLog(L"Will use (not yet existing) INI file for configuration");
        }
        // With !SafeOnly we always return true, so this result is actually never considered
        Result = true;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TStorage __fastcall TWinConfiguration::GetStorage()
{
  if (FStorage == stDetect)
  {
    if (FindResourceEx(NULL, RT_RCDATA, L"WINSCP_SESSION",
      MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL)))
    {
      FTemporarySessionFile =
        IncludeTrailingBackslash(SystemTemporaryDirectory()) + L"winscps.tmp";
      DumpResourceToFile(L"WINSCP_SESSION", FTemporarySessionFile);
      FEmbeddedSessions = true;
      FTemporaryKeyFile =
        IncludeTrailingBackslash(SystemTemporaryDirectory()) + L"winscpk.tmp";
      if (!DumpResourceToFile(L"WINSCP_KEY", FTemporaryKeyFile))
      {
        FTemporaryKeyFile = L"";
      }
    }

    DebugCheck(DetectStorage(false));
  }
  // Meaning any inherited autodetection basically does not happen, so the below call returns FStorage
  DebugAssert(FStorage != stDetect);
  TStorage Result = TCustomWinConfiguration::GetStorage();
  return Result;
}
//---------------------------------------------------------------------------
bool TWinConfiguration::TrySetSafeStorage()
{
  return DetectStorage(true);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Saved()
{
  TCustomWinConfiguration::Saved();
  FBookmarks->ModifyAll(false);
  FCustomCommandList->Reset();
  FEditorList->Saved();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::RecryptPasswords(TStrings * RecryptPasswordErrors)
{
  TCustomWinConfiguration::RecryptPasswords(RecryptPasswordErrors);

  try
  {
    TTerminalManager * Manager = TTerminalManager::Instance(false);
    DebugAssert(Manager != NULL);
    if (Manager != NULL)
    {
      Manager->RecryptPasswords();
    }
  }
  catch (Exception & E)
  {
    UnicodeString Message;
    if (ExceptionMessage(&E, Message))
    {
      // we do not expect this really to happen,
      // so we do not bother providing context
      RecryptPasswordErrors->Add(Message);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetUseMasterPassword()
{
  return FUseMasterPassword;
}
//---------------------------------------------------------------------------
THierarchicalStorage * TWinConfiguration::CreateScpStorage(bool & SessionList)
{
  // Detect embedded session, if not checked yet
  GetStorage();

  THierarchicalStorage * Result;
  if (SessionList && !FTemporarySessionFile.IsEmpty())
  {
    Result = TIniFileStorage::CreateFromPath(FTemporarySessionFile);
    // This is session-list specific store, so the only instance,
    // we do not reset the SessionList argument
    // (compare TConfiguration::CreateScpStorage)
  }
  else
  {
    Result = TCustomWinConfiguration::CreateScpStorage(SessionList);
  }
  return Result;
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKeyPath(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKeyPath(); }
#define KEY(TYPE, VAR) KEYEX(TYPE, VAR, PropertyToKey(TEXT(#VAR)))
#define REGCONFIG(CANCREATE) \
  BLOCK(L"Interface", CANCREATE, \
    KEYEX(Integer,DoubleClickAction, L"CopyOnDoubleClick"); \
    KEY(Bool,     CopyOnDoubleClickConfirmation); \
    KEY(Bool,     AlwaysRespectDoubleClickAction); \
    KEY(Bool,     DDDisableMove); \
    KEYEX(Integer, DDTransferConfirmation, L"DDTransferConfirmation2"); \
    KEY(String,   DDTemporaryDirectory); \
    KEY(String,   DDDrives); \
    KEY(Bool,     DDWarnLackOfTempSpace); \
    KEY(Float,    DDWarnLackOfTempSpaceRatio); \
    KEY(Bool,     DeleteToRecycleBin); \
    KEY(Bool,     DimmHiddenFiles); \
    KEY(Bool,     RenameWholeName); \
    KEY(Bool,     SelectDirectories); \
    KEY(String,   SelectMask); \
    KEY(Bool,     ShowHiddenFiles); \
    KEY(Integer,  FormatSizeBytes); \
    KEY(Integer,  PanelSearch); \
    KEY(Bool,     ShowInaccesibleDirectories); \
    KEY(Bool,     ConfirmTransferring); \
    KEY(Bool,     ConfirmDeleting); \
    KEY(Bool,     ConfirmRecycling); \
    KEY(Bool,     ConfirmClosingSession); \
    KEY(String,   AutoStartSession); \
    KEY(Bool,     UseLocationProfiles); \
    KEY(Bool,     UseSharedBookmarks); \
    KEY(Integer,  LocaleSafe); \
    KEY(Bool,     DDFakeFile); \
    KEY(Integer,  DDExtTimeout); \
    KEY(Bool,     DefaultDirIsHome); \
    KEY(Bool,     TemporaryDirectoryAppendSession); \
    KEY(Bool,     TemporaryDirectoryAppendPath); \
    KEY(Bool,     TemporaryDirectoryDeterministic); \
    KEY(Bool,     TemporaryDirectoryCleanup); \
    KEY(Bool,     ConfirmTemporaryDirectoryCleanup); \
    KEY(Bool,     PreservePanelState); \
    KEY(Integer,  DarkTheme); \
    KEY(String,   LastStoredSession); \
    KEY(Bool,     AutoSaveWorkspace); \
    KEY(Bool,     AutoSaveWorkspacePasswords); \
    KEY(String,   AutoWorkspace); \
    KEY(Integer,  PathInCaption); \
    KEY(Integer,  SessionTabNameFormat); \
    KEY(Bool,     MinimizeToTray); \
    KEY(Bool,     BalloonNotifications); \
    KEY(Integer,  NotificationsTimeout); \
    KEY(Integer,  NotificationsStickTime); \
    KEY(Bool,     CopyParamAutoSelectNotice); \
    KEY(Bool,     LockToolbars); \
    KEY(Bool,     SelectiveToolbarText); \
    KEY(Integer,  LargerToolbar); \
    KEY(Bool,     AutoOpenInPutty); \
    KEY(Bool,     RefreshRemotePanel); \
    KEY(DateTime, RefreshRemotePanelInterval); \
    KEYEX(String, PanelFont.FontName, L"PanelFontName"); \
    KEYEX(Integer,PanelFont.FontSize, L"PanelFontSize"); \
    KEYEX(Integer,PanelFont.FontStyle, L"PanelFontStyle"); \
    KEYEX(Integer,PanelFont.FontCharset, L"PanelFontCharset"); \
    KEY(Bool,     NaturalOrderNumericalSorting); \
    KEY(Bool,     AlwaysSortDirectoriesByName); \
    KEY(Bool,     FullRowSelect); \
    KEY(Bool,     OfferedEditorAutoConfig); \
    KEY(Integer,  LastMonitor); \
    KEY(String,   VersionHistory); \
    KEY(Bool,     EnableQueueByDefault); \
    KEY(String,   OpenedStoredSessionFolders); \
    KEY(Bool,     AutoImportedFromPuttyOrFilezilla); \
    KEY(Integer,  GenerateUrlComponents); \
    KEY(Integer,  GenerateUrlCodeTarget); \
    KEY(Integer,  GenerateUrlScriptFormat); \
    KEY(Integer,  GenerateUrlAssemblyLanguage); \
    KEY(Bool,     ExternalSessionInExistingInstance); \
    KEY(Bool,     ShowLoginWhenNoSession); \
    KEY(Bool,     KeepOpenWhenNoSession); \
    KEY(Bool,     DefaultToNewRemoteTab); \
    KEY(Bool,     LocalIconsByExt); \
    KEY(Bool,     FlashTaskbar); \
    KEY(Integer,  MaxSessions); \
    KEY(Integer,  BidiModeOverride); \
    KEY(Integer,  FlipChildrenOverride); \
    KEY(Bool,     ShowTips); \
    KEY(String,   TipsSeen); \
    KEY(DateTime, TipsShown); \
    KEY(String,   FileColors); \
    KEY(Integer,  RunsSinceLastTip); \
    KEY(Integer,  HonorDrivePolicy); \
    KEY(Bool,     UseABDrives); \
    KEY(Integer,  LastMachineInstallations); \
    KEY(String,   FExtensionsDeleted); \
    KEY(String,   FExtensionsOrder); \
    KEY(String,   FExtensionsShortCuts); \
    KEY(Bool,     TimeoutShellOperations); \
    KEY(Bool,     TimeoutShellIconRetrieval); \
    KEY(Bool,     UseIconUpdateThread); \
    KEY(Bool,     AllowWindowPrint); \
    KEY(Integer,  StoreTransition); \
    KEY(Integer,  QueueTransferLimitMax); \
    KEY(Bool,     HiContrast); \
    KEY(Bool,     EditorCheckNotModified); \
    KEY(Bool,     SessionTabCaptionTruncation); \
    KEY(Integer,  LoadingTooLongLimit); \
    KEY(String,   RemoteThumbnailMask); \
    KEY(Integer,  RemoteThumbnailSizeLimit); \
    KEY(String,   FirstRun); \
  ); \
  BLOCK(L"Interface\\Editor", CANCREATE, \
    KEYEX(String,   Editor.Font.FontName, L"FontName2"); \
    KEY(Integer,  Editor.Font.FontSize); \
    KEY(Integer,  Editor.Font.FontStyle); \
    KEY(Integer,  Editor.Font.FontCharset); \
    KEY(Integer,  Editor.FontColor); \
    KEY(Integer,  Editor.BackgroundColor); \
    KEY(Bool,     Editor.WordWrap); \
    KEY(String,   Editor.FindText); \
    KEY(String,   Editor.ReplaceText); \
    KEY(Bool,     Editor.FindMatchCase); \
    KEY(Bool,     Editor.FindWholeWord); \
    KEY(Bool,     Editor.FindDown); \
    KEY(Integer,  Editor.TabSize); \
    KEY(Integer,  Editor.MaxEditors); \
    KEY(Integer,  Editor.EarlyClose); \
    KEY(Bool,     Editor.SDIShellEditor); \
    KEY(String,   Editor.WindowParams); \
    KEY(Integer,  Editor.Encoding); \
    KEY(Bool,     Editor.WarnOnEncodingFallback); \
    KEY(Bool,     Editor.WarnOrLargeFileSize); \
    KEY(Bool,     Editor.AutoFont); \
    KEY(Bool,     Editor.DisableSmoothScroll); \
  ); \
  BLOCK(L"Interface\\QueueView", CANCREATE, \
    KEY(Integer,  QueueView.Height); \
    KEY(Integer,  QueueView.HeightPixelsPerInch); \
    KEY(String,   QueueView.Layout); \
    KEY(Integer,  QueueView.Show); \
    KEY(Integer,  QueueView.LastHideShow); \
    KEY(Bool,     QueueView.ToolBar); \
    KEY(Bool,     QueueView.Label); \
    KEY(Bool,     QueueView.FileList); \
    KEY(Integer,  QueueView.FileListHeight); \
    KEY(Integer,  QueueView.FileListHeightPixelsPerInch); \
  ); \
  BLOCK(L"Interface\\Updates", CANCREATE, \
    KEY(Integer,  FUpdates.Period); \
    KEY(DateTime, FUpdates.LastCheck); \
    KEY(Integer,  FUpdates.HaveResults); \
    KEY(Integer,  FUpdates.ShownResults); \
    KEY(Integer,  FUpdates.BetaVersions); \
    KEY(Bool,     FUpdates.ShowOnStartup); \
    KEY(String,   FUpdates.AuthenticationEmail); \
    KEY(String,   FUpdates.Mode); \
    KEY(Integer,  FUpdates.ConnectionType); \
    KEY(String,   FUpdates.ProxyHost); \
    KEY(Integer,  FUpdates.ProxyPort); \
    KEY(Integer,  FUpdates.Results.ForVersion); \
    KEY(Integer,  FUpdates.Results.Version); \
    KEY(String,   FUpdates.Results.Message); \
    KEY(Integer,  FUpdates.Results.Critical); \
    KEY(String,   FUpdates.Results.Release); \
    KEY(Bool,     FUpdates.Results.Disabled); \
    KEY(String,   FUpdates.Results.Url); \
    KEY(String,   FUpdates.Results.UrlButton); \
    KEY(String,   FUpdates.Results.NewsUrl); \
    KEYEX(Integer,FUpdates.Results.NewsSize.Width, L"NewsWidth"); \
    KEYEX(Integer,FUpdates.Results.NewsSize.Height, L"NewsHeight"); \
    KEY(String,   FUpdates.Results.DownloadUrl); \
    KEY(Int64,    FUpdates.Results.DownloadSize); \
    KEY(String,   FUpdates.Results.DownloadSha256); \
    KEY(String,   FUpdates.Results.AuthenticationError); \
    KEY(Bool,     FUpdates.Results.OpenGettingStarted); \
    KEY(String,   FUpdates.Results.DownloadingUrl); \
    KEYEX(Integer,FUpdates.Results.TipsSize.Width, L"TipsWidth"); \
    KEYEX(Integer,FUpdates.Results.TipsSize.Height, L"TipsHeight"); \
    KEY(String,   FUpdates.Results.TipsUrl); \
    KEY(String,   FUpdates.Results.Tips); \
    KEY(Integer,  FUpdates.Results.TipsIntervalDays); \
    KEY(Integer,  FUpdates.Results.TipsIntervalRuns); \
    KEY(String,   FUpdates.DotNetVersion); \
    KEY(String,   FUpdates.ConsoleVersion); \
  ); \
  BLOCK(L"Interface\\Explorer", CANCREATE, \
    KEYEX(String,  ScpExplorer.ToolbarsLayout, ToolbarsLayoutKey); \
    KEY(String,  ScpExplorer.ToolbarsButtons); \
    KEY(String,  ScpExplorer.DirViewParams); \
    KEY(String,  ScpExplorer.LastLocalTargetDirectory); \
    KEY(Bool,    ScpExplorer.SessionsTabs); \
    KEY(Bool,    ScpExplorer.StatusBar); \
    KEY(String,  ScpExplorer.WindowParams); \
    KEY(Integer, ScpExplorer.ViewStyle); \
    KEY(Bool,    ScpExplorer.ShowFullAddress); \
    KEY(Bool,    ScpExplorer.DriveView); \
    KEY(Integer, ScpExplorer.DriveViewWidth); \
    KEY(Integer, ScpExplorer.DriveViewWidthPixelsPerInch); \
  ); \
  BLOCK(L"Interface\\Commander", CANCREATE, \
    KEYEX(String,  ScpCommander.ToolbarsLayout, ToolbarsLayoutKey); \
    KEY(String,  ScpCommander.ToolbarsButtons); \
    KEY(Integer, ScpCommander.CurrentPanel); \
    KEY(Float,   ScpCommander.LocalPanelWidth); \
    KEY(Bool,    ScpCommander.SwappedPanels); \
    KEY(Bool,    ScpCommander.SessionsTabs); \
    KEY(Bool,    ScpCommander.StatusBar); \
    KEY(String,  ScpCommander.WindowParams); \
    KEYEX(Integer, ScpCommander.NortonLikeMode, L"ExplorerStyleSelection"); \
    KEY(Bool,    ScpCommander.PreserveLocalDirectory); \
    KEY(Bool,    ScpCommander.CompareByTime); \
    KEY(Bool,    ScpCommander.CompareBySize); \
    KEY(Bool,    ScpCommander.TreeOnLeft); \
    KEY(Bool,    ScpCommander.ExplorerKeyboardShortcuts); \
    KEY(Bool,    ScpCommander.SystemContextMenu); \
  ); \
  BLOCK(L"Interface\\Commander\\LocalPanel", CANCREATE, \
    KEY(String,  ScpCommander.LocalPanel.DirViewParams); \
    KEY(Integer, ScpCommander.LocalPanel.ViewStyle); \
    KEY(Bool,    ScpCommander.LocalPanel.StatusBar); \
    KEY(Bool,    ScpCommander.LocalPanel.DriveView); \
    KEY(Integer, ScpCommander.LocalPanel.DriveViewHeight); \
    KEY(Integer, ScpCommander.LocalPanel.DriveViewHeightPixelsPerInch); \
    KEY(Integer, ScpCommander.LocalPanel.DriveViewWidth); \
    KEY(Integer, ScpCommander.LocalPanel.DriveViewWidthPixelsPerInch); \
    KEY(String,  ScpCommander.LocalPanel.LastPath); \
  ); \
  BLOCK(L"Interface\\Commander\\RemotePanel", CANCREATE, \
    KEY(String,  ScpCommander.RemotePanel.DirViewParams); \
    KEY(Integer, ScpCommander.RemotePanel.ViewStyle); \
    KEY(Bool,    ScpCommander.RemotePanel.StatusBar); \
    KEY(Bool,    ScpCommander.RemotePanel.DriveView); \
    KEY(Integer, ScpCommander.RemotePanel.DriveViewHeight); \
    KEY(Integer, ScpCommander.RemotePanel.DriveViewHeightPixelsPerInch); \
    KEY(Integer, ScpCommander.RemotePanel.DriveViewWidth); \
    KEY(Integer, ScpCommander.RemotePanel.DriveViewWidthPixelsPerInch); \
    KEY(String,  ScpCommander.RemotePanel.LastPath); \
  ); \
  BLOCK(L"Interface\\Commander\\OtherLocalPanel", CANCREATE, \
    KEYEX(String,  ScpCommander.OtherLocalPanelDirViewParams, L"DirViewParams"); \
    KEYEX(Integer, ScpCommander.OtherLocalPanelViewStyle, L"ViewStyle"); \
    KEYEX(String,  ScpCommander.OtherLocalPanelLastPath, L"LastPath"); \
  ); \
  BLOCK(L"Security", CANCREATE, \
    KEY(Bool,    FUseMasterPassword); \
    KEY(String,  FMasterPasswordVerifier); \
  );
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SaveData(THierarchicalStorage * Storage, bool All)
{
  TCustomWinConfiguration::SaveData(Storage, All);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) Storage->Write ## TYPE(NAME, VAR)
  REGCONFIG(true);
  #undef KEYEX

  if (Storage->OpenSubKey(L"Bookmarks", true))
  {
    FBookmarks->Save(Storage, All);

    Storage->CloseSubKey();
  }
  if ((All && !FCustomCommandsDefaults) || FCustomCommandList->Modified)
  {
    FCustomCommandList->Save(Storage);

    if (FCustomCommandList->Count == 0)
    {
      Storage->WriteBool(L"CustomCommandsNone", true);
    }
    else
    {
      Storage->DeleteValue(L"CustomCommandsNone");
    }
  }

  if ((All || FCustomCommandOptionsModified) &&
      Storage->OpenSubKey(L"CustomCommandOptions", true))
  {
    Storage->ClearValues();
    Storage->WriteValues(FCustomCommandOptions.get(), true);
    Storage->CloseSubKey();
    FCustomCommandOptionsModified = false;
  }

  if ((All || FEditorList->Modified) &&
      Storage->OpenSubKeyPath(L"Interface\\Editor", true))
  try
  {
    FEditorList->Save(Storage);
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadFrom(THierarchicalStorage * Storage)
{
  FLegacyEditor = new TEditorPreferences();
  try
  {
    FLegacyEditor->LegacyDefaults();

    TCustomWinConfiguration::LoadFrom(Storage);

    // Following needs to be done even if there's no Configuration key in the storage,
    // so it cannot be in LoadData

    int EditorCount = FEditorList->Count;
    if (EditorCount == 0)
    {
      TEditorPreferences * AlternativeEditor = NULL;
      try
      {
        if (FLegacyEditor->Data->Editor == edInternal)
        {
          if (!FLegacyEditor->Data->ExternalEditor.IsEmpty())
          {
            AlternativeEditor = new TEditorPreferences(*FLegacyEditor);
            AlternativeEditor->GetData()->Editor = edExternal;
            FLegacyEditor->GetData()->ExternalEditor = L"";
          }
        }
        else
        {
          if (FLegacyEditor->Data->ExternalEditor.IsEmpty())
          {
            FLegacyEditor->GetData()->Editor = edInternal;
          }
          else
          {
            AlternativeEditor = new TEditorPreferences(*FLegacyEditor);
            AlternativeEditor->GetData()->Editor = edInternal;
          }
        }
      }
      catch(...)
      {
        delete AlternativeEditor;
        throw;
      }

      FEditorList->Add(FLegacyEditor);
      FLegacyEditor = NULL;
      if (AlternativeEditor != NULL)
      {
        FEditorList->Add(AlternativeEditor);
      }
    }

    // Additionally, this needs to be after Locale is loaded
    LoadExtensionTranslations();
    // and this after the ExtensionsDeleted, ExtensionsOrder and ExtensionsShortCuts are loaded
    LoadExtensionList();
  }
  __finally
  {
    delete FLegacyEditor;
    FLegacyEditor = NULL;
  }

  if (FUpdates.ConnectionType == -1)
  {
    FUpdates.ConnectionType = (FUpdates.ProxyHost.IsEmpty() ? ctAuto : ctProxy);
  }
  AddVersionToHistory();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::DoLoadExtensionList(
  const UnicodeString & Path, const UnicodeString & PathId, TStringList * DeletedExtensions)
{
  TSearchRecOwned SearchRec;
  int FindAttrs = faReadOnly | faArchive;
  if (FindFirstUnchecked(IncludeTrailingBackslash(Path) + L"*.*", FindAttrs, SearchRec) == 0)
  {
    do
    {
      UnicodeString Id = TCustomCommandType::GetExtensionId(SearchRec.Name);
      if (!Id.IsEmpty())
      {
        Id = IncludeTrailingBackslash(PathId) + Id;
        if (DeletedExtensions->IndexOf(Id) >= 0)
        {
          // reconstruct the list, so that we remove the commands that no longer exists
          AddToList(FExtensionsDeleted, Id, L"|");
        }
        else
        {
          std::unique_ptr<TCustomCommandType> CustomCommand(new TCustomCommandType());
          CustomCommand->Id = Id;

          try
          {
            CustomCommand->LoadExtension(SearchRec.GetFilePath());
            FExtensionList->Add(CustomCommand.release());
          }
          catch (...)
          {
            // skip invalid extension files
          }
        }
      }
    }
    while (FindNextChecked(SearchRec) == 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall ParseExtensionList(TStrings * Strings, UnicodeString S)
{
  while (!S.IsEmpty())
  {
    UnicodeString Extension = CutToChar(S, L'|', false);
    Strings->Add(Extension);
  }
}
//---------------------------------------------------------------------------
const UnicodeString ExtensionsSubFolder(L"Extensions");
const UnicodeString ExtensionsCommonPathId(L"common");
const UnicodeString ExtensionsCommonExtPathId(L"commonext");
const UnicodeString ExtensionsUserExtPathId(L"userext");
//---------------------------------------------------------------------------
static UnicodeString __fastcall ExtractExtensionBaseName(const UnicodeString & FileName)
{
  UnicodeString S = ExtractFileName(FileName);
  // ExtractFileNameOnly trims the last extension only, we want to trim all extensions
  UnicodeString Result = CutToChar(S, L'.', true);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::GetProvisionaryExtensionId(const UnicodeString & FileName)
{
  return IncludeTrailingBackslash(ExtensionsUserExtPathId) + ExtractExtensionBaseName(FileName);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::GetUserExtensionsPath()
{
  return IncludeTrailingBackslash(GetShellFolderPath(CSIDL_APPDATA)) + L"WinSCP\\" + ExtensionsSubFolder;
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWinConfiguration::GetExtensionsPaths()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  UnicodeString ExeParentPath = ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName));
  Result->Values[ExtensionsCommonPathId] = ExeParentPath;
  UnicodeString CommonExtensions = IncludeTrailingBackslash(ExeParentPath) + ExtensionsSubFolder;
  Result->Values[ExtensionsCommonExtPathId] = CommonExtensions;
  Result->Values[ExtensionsUserExtPathId] = GetUserExtensionsPath();
  return Result.release();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::GetExtensionId(const UnicodeString & ExtensionPath)
{
  UnicodeString Path = ExcludeTrailingBackslash(ExtractFilePath(ExtensionPath));

  UnicodeString NameId = TCustomCommandType::GetExtensionId(ExtractFileName(ExtensionPath));
  if (!NameId.IsEmpty())
  {
    std::unique_ptr<TStrings> ExtensionsPaths(GetExtensionsPaths());
    for (int Index = 0; Index < ExtensionsPaths->Count; Index++)
    {
      if (IsPathToSameFile(Path, ExtensionsPaths->ValueFromIndex[Index]))
      {
        return IncludeTrailingBackslash(ExtensionsPaths->Names[Index]) + NameId;
      }
    }
  }
  return L"";
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ReleaseExtensionTranslations()
{
  for (int Index = 0; Index < FExtensionTranslations->Count; Index++)
  {
    delete FExtensionTranslations->Objects[Index];
  }
  FExtensionTranslations->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadExtensionTranslations()
{
  ReleaseExtensionTranslations();
  FExtensionTranslations.reset(new TStringList());
  int Index = EXTENSION_STRINGS;
  UnicodeString S;
  while (!((S = LoadStr(Index)).IsEmpty()))
  {
    UnicodeString ExtensionName = CutToChar(S, L'.', false);
    UnicodeString Original = CutToChar(S, L'=', false);
    UnicodeString Translation = S;

    int ExtensionIndex = FExtensionTranslations->IndexOf(ExtensionName);
    if (ExtensionIndex < 0)
    {
      ExtensionIndex = FExtensionTranslations->AddObject(ExtensionName, new TStringList());
    }
    TStringList * ExtensionTranslation = DebugNotNull(dynamic_cast<TStringList *>(FExtensionTranslations->Objects[ExtensionIndex]));
    ExtensionTranslation->Values[Original] = Translation;
    Index++;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::UniqueExtensionName(const UnicodeString & ExtensionName, int Counter)
{
  // See how the digits are removed in the ExtensionStringTranslation
  return ExtensionName + IntToStr(Counter);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::ExtensionStringTranslation(const UnicodeString & ExtensionId, const UnicodeString & S)
{
  UnicodeString Result = S;
  if (!ExtensionId.IsEmpty())
  {
    UnicodeString ExtensionName = ExtractFileName(ExtensionId);
    int ExtensionIndex;
    bool Retry;
    do
    {
      ExtensionIndex = FExtensionTranslations->IndexOf(ExtensionName);

      // Try without trailing digits, possibly added by UniqueExtensionName
      Retry = (ExtensionIndex < 0) && !ExtensionName.IsEmpty() && IsDigit(*ExtensionName.LastChar());
      if (Retry)
      {
        ExtensionName.SetLength(ExtensionName.Length() - 1);
      }
    }
    while (Retry);

    if (ExtensionIndex >= 0)
    {
      TStrings * ExtensionTranslation = DebugNotNull(dynamic_cast<TStrings *>(FExtensionTranslations->Objects[ExtensionIndex]));
      int StringIndex = ExtensionTranslation->IndexOfName(S);
      if (StringIndex >= 0)
      {
        Result = ExtensionTranslation->ValueFromIndex[StringIndex];
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadExtensionList()
{
  FExtensionList->Clear();

  std::unique_ptr<TStringList> DeletedExtensions(CreateSortedStringList());
  ParseExtensionList(DeletedExtensions.get(), FExtensionsDeleted);
  // will reconstruct the list in DoLoadExtensionList
  FExtensionsDeleted = L"";

  std::unique_ptr<TStrings> ExtensionsPaths(GetExtensionsPaths());
  for (int Index = 0; Index < ExtensionsPaths->Count; Index++)
  {
    DoLoadExtensionList(ExtensionsPaths->ValueFromIndex[Index], ExtensionsPaths->Names[Index], DeletedExtensions.get());
  }

  std::unique_ptr<TStringList> OrderedExtensions(new TStringList());
  ParseExtensionList(OrderedExtensions.get(), FExtensionsOrder);
  FExtensionList->SortBy(OrderedExtensions.get());

  UnicodeString ShortCuts = FExtensionsShortCuts;
  while (!ShortCuts.IsEmpty())
  {
    UnicodeString S = CutToChar(ShortCuts, L'|', false);
    TShortCut ShortCut = static_cast<TShortCut>(StrToInt(CutToChar(S, L'=', false)));
    for (int Index = 0; Index < FExtensionList->Count; Index++)
    {
      if (FExtensionList->Commands[Index]->Id == S)
      {
        const_cast<TCustomCommandType *>(FExtensionList->Commands[Index])->ShortCut = ShortCut;
      }
    }
  }
}
//---------------------------------------------------------------------------
static UnicodeString KeyName(THierarchicalStorage * Storage, const UnicodeString & Name)
{
  UnicodeString Result = Name;
  if ((Result == ToolbarsLayoutKey) && !Storage->KeyExists(Result) && Storage->KeyExists(ToolbarsLayoutOldKey))
  {
    Result = ToolbarsLayoutOldKey;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadData(THierarchicalStorage * Storage)
{
  TCustomWinConfiguration::LoadData(Storage);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) VAR = Storage->Read ## TYPE(KeyName(Storage, NAME), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEYEX

  // to reflect changes to PanelFont
  UpdateIconFont();

  if (Storage->OpenSubKey(L"Bookmarks", false))
  {
    FBookmarks->Load(Storage);
    Storage->CloseSubKey();
  }

  if (Storage->KeyExists(L"CustomCommands") ||
      Storage->ReadBool(L"CustomCommandsNone", false))
  {
    FCustomCommandList->Load(Storage);
    FCustomCommandsDefaults = false;
  }
  else if (FCustomCommandList->Modified)
  {
    // can this (=reloading of configuration) even happen?
    // if it does, shouldn't we reset default commands?
    DebugFail();
    FCustomCommandList->Clear();
    FCustomCommandsDefaults = false;
  }
  FCustomCommandList->Reset();

  if (Storage->OpenSubKey(L"CustomCommandOptions", false))
  {
    Storage->ReadValues(FCustomCommandOptions.get(), true);
    Storage->CloseSubKey();
    FCustomCommandOptionsModified = false;
  }

  if (Storage->OpenSubKeyPath(L"Interface\\Editor", false))
  try
  {
    FEditorList->Clear();
    FEditorList->Load(Storage);
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }

  // load legacy editor configuration
  DebugAssert(FLegacyEditor != NULL);
  if (Storage->OpenSubKeyPath(L"Interface\\Editor", false))
  {
    try
    {
      FLegacyEditor->Load(Storage, true);
    }
    __finally
    {
      Storage->CloseSubKeyPath();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  TCustomWinConfiguration::LoadAdmin(Storage);
  FDisableOpenEdit = Storage->ReadBool(L"DisableOpenEdit", FDisableOpenEdit);
  FDefaultUpdatesPeriod = Storage->ReadInteger(L"DefaultUpdatesPeriod", FDefaultUpdatesPeriod);
  FMachineInstallations = Storage->ReadInteger(L"Installations", 0);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CopyData(THierarchicalStorage * Source, THierarchicalStorage * Target)
{
  TCustomWinConfiguration::CopyData(Source, Target);

  if (CopySubKey(Source, Target, ConfigurationSubKey))
  {
    Target->WriteString(L"JumpList", Source->ReadString(L"JumpList", L""));
    Target->WriteString(L"JumpListWorkspaces", Source->ReadString(L"JumpListWorkspaces", L""));

    Target->CloseSubKey();
    Source->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ClearTemporaryLoginData()
{
  if (!FTemporaryKeyFile.IsEmpty())
  {
    DeleteFile(ApiPath(FTemporaryKeyFile));
    FTemporaryKeyFile = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::AddVersionToHistory()
{
  int CurrentVersion = CompoundVersion;
  DebugAssert(ZeroBuildNumber(CurrentVersion) == CurrentVersion);

  int From = 1;
  bool CurrentVersionPresent = false;
  while (!CurrentVersionPresent && (From < FVersionHistory.Length()))
  {
    UnicodeString VersionInfo = CopyToChars(FVersionHistory, From, L";", true);
    UnicodeString VersionStr = CutToChar(VersionInfo, L',', true);
    int Version;

    if (TryStrToInt(VersionStr, Version))
    {
      Version = ZeroBuildNumber(Version);
      if (Version == CurrentVersion)
      {
        CurrentVersionPresent = true;
      }
    }
  }

  if (!CurrentVersionPresent)
  {
    UnicodeString CurrentVersionInfo =
      IntToStr(CurrentVersion) + L"," + GetReleaseType();
    AddToList(FVersionHistory, CurrentVersionInfo, L';');
  }

  Usage->Set(L"AnyBetaUsed", AnyBetaInVersionHistory);
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::DoIsBeta(const UnicodeString & ReleaseType)
{
  // What about "Development" release type?
  return SameText(ReleaseType, L"beta") || SameText(ReleaseType, L"rc");
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetIsBeta()
{
  return DoIsBeta(GetReleaseType());
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetAnyBetaInVersionHistory()
{
  int From = 1;
  bool AnyBeta = false;
  while (!AnyBeta && (From < VersionHistory.Length()))
  {
    UnicodeString VersionInfo = CopyToChars(VersionHistory, From, L";", true);
    CutToChar(VersionInfo, L',', true);
    UnicodeString ReleaseType = CutToChar(VersionInfo, ',', true);

    if (DoIsBeta(ReleaseType))
    {
      AnyBeta = true;
    }
  }
  return AnyBeta;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetDDExtInstalled()
{
  if (FDDExtInstalled < 0)
  {
    if (IsWin64())
    {
      // WORKAROUND
      // We cannot load 64-bit COM class into 32-bit process,
      // so we fallback to querying registration keys
      #define CLSID_SIZE 39
      wchar_t ClassID[CLSID_SIZE];
      StringFromGUID2(CLSID_ShellExtension, ClassID, LENOF(ClassID));
      NULL_TERMINATE(ClassID);
      UnicodeString SubKey = UnicodeString(L"CLSID\\") + ClassID;
      HKEY HKey;
      LONG Result = RegOpenKeyEx(HKEY_CLASSES_ROOT, SubKey.c_str(), 0, KEY_READ | KEY_WOW64_64KEY, &HKey);
      if (Result == ERROR_SUCCESS)
      {
        RegCloseKey(HKey);
        FDDExtInstalled = 1;
      }
      else
      {
        FDDExtInstalled = 0;
      }
    }
    else
    {
      void * DragExtRef;
      int CreateResult =
        CoCreateInstance(CLSID_ShellExtension, NULL,
          CLSCTX_INPROC_SERVER | CLSCTX_LOCAL_SERVER, IID_IUnknown,
          &DragExtRef);
      bool Result = (CreateResult == S_OK);
      FDDExtInstalled = (Result ? 1 : 0);
      if (Result)
      {
        reinterpret_cast<IUnknown *>(DragExtRef)->Release();
        CoFreeUnusedLibraries();
      }
    }
  }
  return (FDDExtInstalled > 0);
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::IsDDExtRunning()
{
  bool Result;
  if (!DDExtInstalled)
  {
    Result = false;
  }
  else
  {
    HANDLE H = OpenMutex(SYNCHRONIZE, False, DRAG_EXT_RUNNING_MUTEX);
    Result = (H != NULL);
    CloseHandle(H);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::IsDDExtBroken()
{
  int Build = Win32BuildNumber();
  return (Build >= 17134) && (Build < 17763);
}
//---------------------------------------------------------------------------
RawByteString __fastcall TWinConfiguration::StronglyRecryptPassword(RawByteString Password, UnicodeString Key)
{
  RawByteString Dummy;
  RawByteString Result;
  if (GetExternalEncryptedPassword(Password, Dummy) ||
      !FUseMasterPassword)
  {
    // already-strongly encrypted
    // or no master password set, so we cannot strongly-encrypt it
    Result = Password;
  }
  else
  {
    UnicodeString PasswordText =
      TCustomWinConfiguration::DecryptPassword(Password, Key);
    if (!PasswordText.IsEmpty())
    {
      // Can be not set for instance, when editing=>saving site with no prior password.
      // Though it should not actually happen, as we call AskForMasterPasswordIfNotSetAndNeededToPersistSessionData in DoSaveSession.
      AskForMasterPasswordIfNotSet();
      Password = ScramblePassword(PasswordText);
      AES256EncyptWithMAC(Password, FPlainMasterPasswordEncrypt, Result);
      Result = SetExternalEncryptedPassword(Result);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::DecryptPassword(RawByteString Password, UnicodeString Key)
{
  UnicodeString Result;
  RawByteString Buf;
  if (GetExternalEncryptedPassword(Password, Buf))
  {
    if (FDontDecryptPasswords == 0)
    {
      // As opposite to AskForMasterPasswordIfNotSet, we test here
      // for decrypt password. This is important while recrypting password,
      // when clearing master password, when encrypt password is already empty.
      if (FPlainMasterPasswordDecrypt.IsEmpty())
      {
        AskForMasterPassword();
      }
      if (!AES256DecryptWithMAC(Buf, FPlainMasterPasswordDecrypt, Buf) ||
          !UnscramblePassword(Buf, Result))
      {
        throw Exception(LoadStr(DECRYPT_PASSWORD_ERROR));
      }
    }
    else
    {
      Abort();
    }
  }
  else
  {
    Result = TCustomWinConfiguration::DecryptPassword(Password, Key);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetMasterPassword(UnicodeString value)
{
  // just stores the plain-text version of the password

  // we can get here even if master password is off,
  // when we encounter stray encrypted password (e.g. manually imported site),
  // make sure we do not set encrypt password to avoid starting encrypting
  // new passwords
  // (this is bit of edge case, not really well tested)
  if (!FUseMasterPassword)
  {
    FPlainMasterPasswordDecrypt = value;
  }
  else if (DebugAlwaysTrue(FUseMasterPassword) &&
      DebugAlwaysTrue(ValidateMasterPassword(value)))
  {
    FPlainMasterPasswordEncrypt = value;
    FPlainMasterPasswordDecrypt = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ChangeMasterPassword(
  UnicodeString value, TStrings * RecryptPasswordErrors)
{
  RawByteString Verifier;
  AES256CreateVerifier(value, Verifier);
  FMasterPasswordVerifier = BytesToHex(Verifier);
  FPlainMasterPasswordEncrypt = value;
  FUseMasterPassword = true;
  try
  {
    RecryptPasswords(RecryptPasswordErrors);
  }
  __finally
  {
    FPlainMasterPasswordDecrypt = value;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::ValidateMasterPassword(UnicodeString value)
{
  DebugAssert(UseMasterPassword);
  DebugAssert(!FMasterPasswordVerifier.IsEmpty());
  bool Result = AES256Verify(value, HexToBytes(FMasterPasswordVerifier));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ClearMasterPassword(TStrings * RecryptPasswordErrors)
{
  FMasterPasswordVerifier = L"";
  FUseMasterPassword = false;
  Shred(FPlainMasterPasswordEncrypt);
  try
  {
    RecryptPasswords(RecryptPasswordErrors);
  }
  __finally
  {
    Shred(FPlainMasterPasswordDecrypt);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::AskForMasterPassword()
{
  if (FMasterPasswordSession > 0)
  {
    if (FMasterPasswordSessionAsked)
    {
      Abort();
    }

    // set before call to OnMasterPasswordPrompt as it may abort
    FMasterPasswordSessionAsked = true;
  }

  if (FOnMasterPasswordPrompt == NULL)
  {
    throw Exception(L"Master password handler not set");
  }
  else
  {
    FOnMasterPasswordPrompt();

    DebugAssert(!FPlainMasterPasswordDecrypt.IsEmpty());
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::AskForMasterPasswordIfNotSet()
{
  if (FPlainMasterPasswordEncrypt.IsEmpty())
  {
    AskForMasterPassword();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::BeginMasterPasswordSession()
{
  // We do not expect nesting
  DebugAssert(FMasterPasswordSession == 0);
  DebugAssert(!FMasterPasswordSessionAsked || (FMasterPasswordSession > 0));
  // This should better be thread-specific
  FMasterPasswordSession++;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::EndMasterPasswordSession()
{
  if (DebugAlwaysTrue(FMasterPasswordSession > 0))
  {
    FMasterPasswordSession--;
  }
  FMasterPasswordSessionAsked = false;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDDisableMove(bool value)
{
  SET_CONFIG_PROPERTY(DDDisableMove);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTransferConfirmation(TAutoSwitch value)
{
  SET_CONFIG_PROPERTY(DDTransferConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTemporaryDirectory(UnicodeString value)
{
  SET_CONFIG_PROPERTY(DDTemporaryDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDDrives(UnicodeString value)
{
  SET_CONFIG_PROPERTY(DDDrives);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDFakeFile(bool value)
{
  SET_CONFIG_PROPERTY(DDFakeFile);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDExtTimeout(int value)
{
  SET_CONFIG_PROPERTY(DDExtTimeout);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDWarnLackOfTempSpace(bool value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpace);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDWarnLackOfTempSpaceRatio(double value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpaceRatio);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetScpExplorer(TScpExplorerConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpExplorer);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetScpCommander(TScpCommanderConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpCommander);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetEditor(TEditorConfiguration value)
{
  SET_CONFIG_PROPERTY(Editor);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetQueueView(TQueueViewConfiguration value)
{
  SET_CONFIG_PROPERTY(QueueView);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetEnableQueueByDefault(bool value)
{
  SET_CONFIG_PROPERTY(EnableQueueByDefault);
}
//---------------------------------------------------------------------------
TUpdatesConfiguration __fastcall TWinConfiguration::GetUpdates()
{
  TUpdatesConfiguration Result;
  {
    TGuard Guard(FCriticalSection);
    Result = FUpdates;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUpdates(TUpdatesConfiguration value)
{
  TGuard Guard(FCriticalSection);
  // do not use SET_CONFIG_PROPERTY to avoid OnChange handler call (not synchronized)
  FUpdates = value;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetVersionHistory(UnicodeString value)
{
  SET_CONFIG_PROPERTY(VersionHistory);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDeleteToRecycleBin(bool value)
{
  SET_CONFIG_PROPERTY(DeleteToRecycleBin);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetSelectDirectories(bool value)
{
  SET_CONFIG_PROPERTY(SelectDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(ShowHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetFormatSizeBytes(TFormatBytesStyle value)
{
  SET_CONFIG_PROPERTY(FormatSizeBytes);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetPanelSearch(TIncrementalSearch value)
{
  SET_CONFIG_PROPERTY(PanelSearch);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowInaccesibleDirectories(bool value)
{
  SET_CONFIG_PROPERTY(ShowInaccesibleDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmTransferring(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmTransferring);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmDeleting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmDeleting);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmRecycling(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmRecycling);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseLocationProfiles(bool value)
{
  SET_CONFIG_PROPERTY(UseLocationProfiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseSharedBookmarks(bool value)
{
  SET_CONFIG_PROPERTY(UseSharedBookmarks);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmClosingSession(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmClosingSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDoubleClickAction(TDoubleClickAction value)
{
  SET_CONFIG_PROPERTY(DoubleClickAction);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCopyOnDoubleClickConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(CopyOnDoubleClickConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAlwaysRespectDoubleClickAction(bool value)
{
  SET_CONFIG_PROPERTY(AlwaysRespectDoubleClickAction);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDimmHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(DimmHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetRenameWholeName(bool value)
{
  SET_CONFIG_PROPERTY(RenameWholeName);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoStartSession(UnicodeString value)
{
  SET_CONFIG_PROPERTY(AutoStartSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetExpertMode(bool value)
{
  SET_CONFIG_PROPERTY(ExpertMode);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDefaultDirIsHome(bool value)
{
  SET_CONFIG_PROPERTY(DefaultDirIsHome);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTemporaryDirectoryAppendSession(bool value)
{
  SET_CONFIG_PROPERTY(TemporaryDirectoryAppendSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTemporaryDirectoryAppendPath(bool value)
{
  SET_CONFIG_PROPERTY(TemporaryDirectoryAppendPath);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTemporaryDirectoryDeterministic(bool value)
{
  SET_CONFIG_PROPERTY(TemporaryDirectoryDeterministic);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTemporaryDirectoryCleanup(bool value)
{
  SET_CONFIG_PROPERTY(TemporaryDirectoryCleanup);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmTemporaryDirectoryCleanup(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmTemporaryDirectoryCleanup);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetPreservePanelState(bool value)
{
  SET_CONFIG_PROPERTY(PreservePanelState);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDarkTheme(TAutoSwitch value)
{
  SET_CONFIG_PROPERTY_EX(DarkTheme, ConfigureInterface());
}
//---------------------------------------------------------------------------
static int __fastcall SysDarkTheme(HKEY RootKey)
{
  std::unique_ptr<TRegistry> Registry(new TRegistry());
  Registry->RootKey = RootKey;
  UnicodeString ThemesPersonalizeKey = L"Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize";
  UnicodeString AppsUseLightThemeValue = L"AppsUseLightTheme";
  int Result = -1;
  if (Registry->OpenKeyReadOnly(ThemesPersonalizeKey) &&
      Registry->ValueExists(AppsUseLightThemeValue))
  {
    Result = Registry->ReadBool(AppsUseLightThemeValue) ? 0 : 1;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::UseDarkTheme()
{
  switch (WinConfiguration->DarkTheme)
  {
    case asOn:
      return true;
    case asOff:
      return false;
    default:
      return (GetSysDarkTheme() > 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLastStoredSession(UnicodeString value)
{
  SET_CONFIG_PROPERTY(LastStoredSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoSaveWorkspace(bool value)
{
  SET_CONFIG_PROPERTY(AutoSaveWorkspace);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoSaveWorkspacePasswords(bool value)
{
  SET_CONFIG_PROPERTY(AutoSaveWorkspacePasswords);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoWorkspace(UnicodeString value)
{
  SET_CONFIG_PROPERTY(AutoWorkspace);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetPathInCaption(TPathInCaption value)
{
  SET_CONFIG_PROPERTY(PathInCaption);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetMinimizeToTray(bool value)
{
  SET_CONFIG_PROPERTY(MinimizeToTray);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetBalloonNotifications(bool value)
{
  SET_CONFIG_PROPERTY(BalloonNotifications);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetNotificationsTimeout(unsigned int value)
{
  SET_CONFIG_PROPERTY(NotificationsTimeout);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetNotificationsStickTime(unsigned int value)
{
  SET_CONFIG_PROPERTY(NotificationsStickTime);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCopyParamAutoSelectNotice(bool value)
{
  SET_CONFIG_PROPERTY(CopyParamAutoSelectNotice);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLockToolbars(bool value)
{
  SET_CONFIG_PROPERTY(LockToolbars);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetSelectiveToolbarText(bool value)
{
  SET_CONFIG_PROPERTY(SelectiveToolbarText);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetLargerToolbar(int value)
{
  SET_CONFIG_PROPERTY(LargerToolbar);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoOpenInPutty(bool value)
{
  SET_CONFIG_PROPERTY(AutoOpenInPutty);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetRefreshRemotePanel(bool value)
{
  SET_CONFIG_PROPERTY(RefreshRemotePanel);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetRefreshRemotePanelInterval(TDateTime value)
{
  SET_CONFIG_PROPERTY(RefreshRemotePanelInterval);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::UpdateIconFont()
{
  UpdateDesktopFont();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetPanelFont(const TFontConfiguration & value)
{
  SET_CONFIG_PROPERTY_EX(PanelFont, UpdateIconFont());
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetNaturalOrderNumericalSorting(bool value)
{
  SET_CONFIG_PROPERTY(NaturalOrderNumericalSorting);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAlwaysSortDirectoriesByName(bool value)
{
  SET_CONFIG_PROPERTY(AlwaysSortDirectoriesByName);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetFullRowSelect(bool value)
{
  SET_CONFIG_PROPERTY(FullRowSelect);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetOfferedEditorAutoConfig(bool value)
{
  SET_CONFIG_PROPERTY(OfferedEditorAutoConfig);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetOpenedStoredSessionFolders(UnicodeString value)
{
  SET_CONFIG_PROPERTY(OpenedStoredSessionFolders);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoImportedFromPuttyOrFilezilla(bool value)
{
  SET_CONFIG_PROPERTY(AutoImportedFromPuttyOrFilezilla);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetGenerateUrlComponents(int value)
{
  SET_CONFIG_PROPERTY(GenerateUrlComponents);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetGenerateUrlCodeTarget(TGenerateUrlCodeTarget value)
{
  SET_CONFIG_PROPERTY(GenerateUrlCodeTarget);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetGenerateUrlScriptFormat(TScriptFormat value)
{
  SET_CONFIG_PROPERTY(GenerateUrlScriptFormat);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetGenerateUrlAssemblyLanguage(TAssemblyLanguage value)
{
  SET_CONFIG_PROPERTY(GenerateUrlAssemblyLanguage);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetExternalSessionInExistingInstance(bool value)
{
  SET_CONFIG_PROPERTY(ExternalSessionInExistingInstance);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowLoginWhenNoSession(bool value)
{
  SET_CONFIG_PROPERTY(ShowLoginWhenNoSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetKeepOpenWhenNoSession(bool value)
{
  SET_CONFIG_PROPERTY(KeepOpenWhenNoSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDefaultToNewRemoteTab(bool value)
{
  SET_CONFIG_PROPERTY(DefaultToNewRemoteTab);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLocalIconsByExt(bool value)
{
  SET_CONFIG_PROPERTY(LocalIconsByExt);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetFlashTaskbar(bool value)
{
  SET_CONFIG_PROPERTY(FlashTaskbar);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetBidiModeOverride(TLocaleFlagOverride value)
{
  SET_CONFIG_PROPERTY(BidiModeOverride);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetFlipChildrenOverride(TLocaleFlagOverride value)
{
  SET_CONFIG_PROPERTY(FlipChildrenOverride);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowTips(bool value)
{
  SET_CONFIG_PROPERTY(ShowTips);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTipsSeen(UnicodeString value)
{
  SET_CONFIG_PROPERTY(TipsSeen);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTipsShown(TDateTime value)
{
  SET_CONFIG_PROPERTY(TipsShown);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetFileColors(UnicodeString value)
{
  SET_CONFIG_PROPERTY(FileColors);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetRunsSinceLastTip(int value)
{
  SET_CONFIG_PROPERTY(RunsSinceLastTip);
}
//---------------------------------------------------------------------------
int __fastcall TWinConfiguration::GetHonorDrivePolicy()
{
  return DriveInfo->HonorDrivePolicy;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetHonorDrivePolicy(int value)
{
  if (HonorDrivePolicy != value)
  {
    DriveInfo->HonorDrivePolicy = value;
    Changed();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetUseABDrives()
{
  return DriveInfo->UseABDrives;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseABDrives(bool value)
{
  if (UseABDrives != value)
  {
    DriveInfo->UseABDrives = value;
    Changed();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCustomCommandList(TCustomCommandList * value)
{
  DebugAssert(FCustomCommandList);
  if (!FCustomCommandList->Equals(value))
  {
    FCustomCommandList->Assign(value);
    FCustomCommandsDefaults = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetExtensionList(TCustomCommandList * value)
{
  if (!ExtensionList->Equals(value))
  {
    std::unique_ptr<TStringList> DeletedExtensions(CreateSortedStringList());
    ParseExtensionList(DeletedExtensions.get(), FExtensionsDeleted);

    for (int Index = 0; Index < ExtensionList->Count; Index++)
    {
      const TCustomCommandType * CustomCommand = ExtensionList->Commands[Index];
      int Index = value->FindIndexByFileName(CustomCommand->FileName);
      if (Index < 0)
      {
        if (FileExists(CustomCommand->FileName) &&
            !DeleteFile(CustomCommand->FileName))
        {
          DeletedExtensions->Add(CustomCommand->Id);
        }
      }
    }

    FExtensionsOrder = L"";
    for (int Index = 0; Index < value->Count; Index++)
    {
      const TCustomCommandType * CustomCommand = value->Commands[Index];
      AddToList(FExtensionsOrder, CustomCommand->Id, L"|");

      int DeletedIndex = DeletedExtensions->IndexOf(CustomCommand->Id);
      if (DeletedIndex >= 0)
      {
        DeletedExtensions->Delete(DeletedIndex);
      }
    }

    FExtensionList->Assign(value);

    FExtensionsDeleted = L"";
    for (int Index = 0; Index < DeletedExtensions->Count; Index++)
    {
      AddToList(FExtensionsDeleted, DeletedExtensions->Strings[Index], L"|");
    }

    FExtensionsShortCuts = L"";
    for (int Index = 0; Index < value->Count; Index++)
    {
      const TCustomCommandType * Extension = value->Commands[Index];
      if (Extension->HasCustomShortCut())
      {
        AddToList(FExtensionsShortCuts, FORMAT(L"%d=%s", (Extension->ShortCut, Extension->Id)), L"|");
      }
    }

    Changed();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CustomCommandShortCuts(TShortCuts & ShortCuts) const
{
  CustomCommandList->ShortCuts(ShortCuts);
  ExtensionList->ShortCuts(ShortCuts);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetBookmarks(UnicodeString Key,
  TBookmarkList * value)
{
  FBookmarks->Bookmarks[Key] = value;
  Changed();
}
//---------------------------------------------------------------------------
TBookmarkList * __fastcall TWinConfiguration::GetBookmarks(UnicodeString Key)
{
  return FBookmarks->Bookmarks[Key];
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetSharedBookmarks(TBookmarkList * value)
{
  FBookmarks->SharedBookmarks = value;
  Changed();
}
//---------------------------------------------------------------------------
TBookmarkList * __fastcall TWinConfiguration::GetSharedBookmarks()
{
  return FBookmarks->SharedBookmarks;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::GetDefaultKeyFile()
{
  return (!FDefaultKeyFile.IsEmpty() ? FDefaultKeyFile : FTemporaryKeyFile);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLastMonitor(int value)
{
  ::SetLastMonitor(value);
}
//---------------------------------------------------------------------------
int __fastcall TWinConfiguration::GetLastMonitor()
{
  return ::GetLastMonitor();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinConfiguration::ExpandedTemporaryDirectory()
{
  UnicodeString Result =
    ExpandFileName(ExpandEnvironmentVariables(DDTemporaryDirectory));
  if (Result.IsEmpty())
  {
    Result = SystemTemporaryDirectory();
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TWinConfiguration::TemporaryDir(bool Mask)
{
  return UniqTempDir(ExpandedTemporaryDirectory(), L"scp", Mask);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWinConfiguration::DoFindTemporaryFolders(bool OnlyFirst)
{
  std::unique_ptr<TStrings> Result(new TStringList());
  TSearchRecOwned SRec;
  UnicodeString Mask = TemporaryDir(true);
  if (FindFirstUnchecked(Mask, faDirectory | faHidden, SRec) == 0)
  {
    do
    {
      if (SRec.IsDirectory())
      {
        Result->Add(SRec.GetFilePath());
      }
    }
    while ((FindNextChecked(SRec) == 0) && (!OnlyFirst || Result->Count == 0));
  }

  if (Result->Count == 0)
  {
    Result.reset(NULL);
  }

  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWinConfiguration::FindTemporaryFolders()
{
  return DoFindTemporaryFolders(false);
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::AnyTemporaryFolders()
{
  std::unique_ptr<TStrings> Folders(DoFindTemporaryFolders(true));
  return (Folders.get() != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CleanupTemporaryFolders()
{
  std::unique_ptr<TStrings> Folders(FindTemporaryFolders());
  if (Folders.get() != NULL)
  {
    CleanupTemporaryFolders(Folders.get());
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CleanupTemporaryFolders(TStrings * Folders)
{
  if (DebugAlwaysTrue(Folders->Count > 0))
  {
    Usage->Inc(L"TemporaryDirectoryCleanups");
  }

  UnicodeString ErrorList;
  for (int i = 0; i < Folders->Count; i++)
  {
    if (!RecursiveDeleteFile(Folders->Strings[i]))
    {
      if (!ErrorList.IsEmpty())
      {
        ErrorList += L"\n";
      }
      ErrorList += Folders->Strings[i];
    }
  }

  if (!ErrorList.IsEmpty())
  {
    throw ExtException(LoadStr(CLEANUP_TEMP_ERROR), ErrorList);
  }
}
//---------------------------------------------------------------------------
int __fastcall TWinConfiguration::GetResourceModuleCompleteness(HINSTANCE Module)
{
  UnicodeString CompletenessStr = LoadStrFrom(Module, TRANSLATION_COMPLETENESS);
  return StrToIntDef(CompletenessStr, -1);
}
//---------------------------------------------------------------------------
int __fastcall TWinConfiguration::GetLocaleCompletenessTreshold()
{
  return 80;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::IsTranslationComplete(HINSTANCE Module)
{
  return (GetResourceModuleCompleteness(Module) >= LocaleCompletenessTreshold);
}
//---------------------------------------------------------------------------
HINSTANCE __fastcall TWinConfiguration::LoadNewResourceModule(LCID ALocale,
  UnicodeString & FileName)
{
  HINSTANCE Instance = TCustomWinConfiguration::LoadNewResourceModule(ALocale, FileName);
  if (Instance != NULL)
  {
    try
    {
      CheckTranslationVersion(FileName, false);
    }
    catch(...)
    {
      FreeResourceModule(Instance);
      throw;
    }
  }
  return Instance;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CheckTranslationVersion(const UnicodeString FileName,
  bool InternalLocaleOnError)
{
  UnicodeString TranslationProductVersion = GetFileProductVersion(FileName);
  UnicodeString TranslationProductName = GetFileProductName(FileName);
  if ((ProductName != TranslationProductName) ||
      (ProductVersion != TranslationProductVersion))
  {
    if (InternalLocaleOnError)
    {
      LocaleSafe = InternalLocale();
    }

    if (TranslationProductName.IsEmpty() || TranslationProductVersion.IsEmpty())
    {
      throw Exception(FMTLOAD(UNKNOWN_TRANSLATION, (FileName)));
    }
    else
    {
      throw Exception(FMTLOAD(INCOMPATIBLE_TRANSLATION,
        (FileName, TranslationProductName, TranslationProductVersion)));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CheckDefaultTranslation()
{
  if (!FInvalidDefaultTranslationMessage.IsEmpty())
  {
    MoreMessageDialog(FMTLOAD(INVALID_DEFAULT_TRANSLATION,
      (FInvalidDefaultTranslationMessage)), NULL, qtWarning, qaOK, HELP_NONE);
  }
}
//---------------------------------------------------------------------------
const TEditorPreferences * __fastcall TWinConfiguration::DefaultEditorForFile(
  const UnicodeString FileName, bool Local, const TFileMasks::TParams & MaskParams)
{
  return FEditorList->Find(FileName, Local, MaskParams);
}
//---------------------------------------------------------------------------
const TEditorList * __fastcall TWinConfiguration::GetEditorList()
{
  return FEditorList;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetEditorList(const TEditorList * value)
{
  if (!(*FEditorList == *value))
  {
    *FEditorList = *value;
    Changed();
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWinConfiguration::GetCustomCommandOptions()
{
  return FCustomCommandOptions.get();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCustomCommandOptions(TStrings * value)
{
  if (!FCustomCommandOptions->Equals(value))
  {
    FCustomCommandOptions->Assign(value);
    FCustomCommandOptionsModified = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLockedInterface(bool value)
{
  SET_CONFIG_PROPERTY(LockedInterface);
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetTimeoutShellOperations()
{
  return ::TimeoutShellOperations;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTimeoutShellOperations(bool value)
{
  ::TimeoutShellOperations = value;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetTimeoutShellIconRetrieval(bool value)
{
  SET_CONFIG_PROPERTY(TimeoutShellIconRetrieval);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseIconUpdateThread(bool value)
{
  SET_CONFIG_PROPERTY(UseIconUpdateThread);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAllowWindowPrint(bool value)
{
  SET_CONFIG_PROPERTY(AllowWindowPrint);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetStoreTransition(TStoreTransition value)
{
  SET_CONFIG_PROPERTY(StoreTransition);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetQueueTransferLimitMax(int value)
{
  SET_CONFIG_PROPERTY(QueueTransferLimitMax);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetHiContrast(bool value)
{
  SET_CONFIG_PROPERTY(HiContrast);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetEditorCheckNotModified(bool value)
{
  SET_CONFIG_PROPERTY(EditorCheckNotModified);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetSessionTabCaptionTruncation(bool value)
{
  SET_CONFIG_PROPERTY(SessionTabCaptionTruncation);
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetLoadingTooLongLimit(int value)
{
  DriveViewLoadingTooLongLimit = value;
}
//---------------------------------------------------------------------------
int TWinConfiguration::GetLoadingTooLongLimit()
{
  return DriveViewLoadingTooLongLimit;
}
//---------------------------------------------------------------------------
void TWinConfiguration::SetFirstRun(const UnicodeString & value)
{
  SET_CONFIG_PROPERTY(FirstRun);
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWinConfiguration::LoadJumpList(
  THierarchicalStorage * Storage, UnicodeString Name)
{
  UnicodeString JumpList = Storage->ReadString(Name, L"");

  std::unique_ptr<TStringList> List(new TStringList());
  List->CaseSensitive = false;
  List->CommaText = JumpList;
  return List.release();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SaveJumpList(
  THierarchicalStorage * Storage, UnicodeString Name, TStringList * List)
{
  UnicodeString JumpList = Storage->ReadString(Name, L"");

  UnicodeString NewJumpList = List->CommaText;
  if (NewJumpList != JumpList)
  {
    Storage->WriteString(Name, NewJumpList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::TrimJumpList(TStringList * List)
{
  while (List->Count > 30)
  {
    List->Delete(List->Count - 1);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::UpdateEntryInJumpList(
  bool Session, const UnicodeString & Name, bool Add)
{
  try
  {
    std::unique_ptr<THierarchicalStorage> Storage(CreateConfigStorage());
    TAutoNestingCounter DontDecryptPasswordsCounter(FDontDecryptPasswords);

    Storage->AccessMode = smReadWrite;

    // For initial call from UpdateJumpList, do not create the key if it does ot exist yet.
    // To avoid creating the key if we are being started just for a maintenance task.
    if (Storage->OpenSubKey(ConfigurationSubKey, !Name.IsEmpty()))
    {
      std::unique_ptr<TStringList> ListSessions(LoadJumpList(Storage.get(), L"JumpList"));
      std::unique_ptr<TStringList> ListWorkspaces(LoadJumpList(Storage.get(), L"JumpListWorkspaces"));

      if (!Name.IsEmpty())
      {
        TStringList * List = (Session ? ListSessions : ListWorkspaces).get();
        int Index = List->IndexOf(Name);
        if (Index >= 0)
        {
          List->Delete(Index);
        }

        if (Add)
        {
          List->Insert(0, Name);
        }
      }

      TrimJumpList(ListSessions.get());
      TrimJumpList(ListWorkspaces.get());

      ::UpdateJumpList(ListSessions.get(), ListWorkspaces.get());

      SaveJumpList(Storage.get(), L"JumpList", ListSessions.get());
      SaveJumpList(Storage.get(), L"JumpListWorkspaces", ListWorkspaces.get());
    }
  }
  catch (Exception & E)
  {
    throw ExtException(&E, MainInstructions(LoadStr(JUMPLIST_ERROR)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::AddSessionToJumpList(UnicodeString SessionName)
{
  UpdateEntryInJumpList(true, SessionName, true);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::DeleteSessionFromJumpList(UnicodeString SessionName)
{
  UpdateEntryInJumpList(true, SessionName, false);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::AddWorkspaceToJumpList(UnicodeString Workspace)
{
  UpdateEntryInJumpList(false, Workspace, true);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::DeleteWorkspaceFromJumpList(UnicodeString Workspace)
{
  UpdateEntryInJumpList(false, Workspace, false);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::UpdateJumpList()
{
  AddSessionToJumpList(L"");
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::UpdateStaticUsage()
{
  // This is here because of TStoredSessionList::UpdateStaticUsage() call
  // from TConfiguration::UpdateStaticUsage()
  TAutoNestingCounter DontDecryptPasswordsAutoCounter(FDontDecryptPasswords);

  TCustomWinConfiguration::UpdateStaticUsage();
  Usage->Set(L"Beta", IsBeta);

  Usage->Set(L"Interface", Interface);
  Usage->Set(L"ThemeDark", UseDarkTheme());
  Usage->Set(L"CustomCommandsCount", (FCustomCommandsDefaults ? 0 : FCustomCommandList->Count));
  Usage->Set(L"UsingLocationProfiles", UseLocationProfiles);
  Usage->Set(L"UsingMasterPassword", UseMasterPassword);
  Usage->Set(L"UsingAutoSaveWorkspace", AutoSaveWorkspace);
  Usage->Set(L"TreeVisible",
    (Interface == ifExplorer) ?
      ScpExplorer.DriveView :
      (ScpCommander.LocalPanel.DriveView || ScpCommander.RemotePanel.DriveView));
  Usage->Set(L"MinimizeToTray", MinimizeToTray);
  UnicodeString ToolbarsButtons = (Interface == ifExplorer) ? ScpExplorer.ToolbarsButtons : ScpCommander.ToolbarsButtons;
  Usage->Set(L"AnyHiddenToolbarButtons", !ToolbarsButtons.IsEmpty());
  Usage->Set(L"LargerToolbar", LargerToolbar);
  Usage->Set(L"FileColors", !FileColors.IsEmpty());
  Usage->Set(L"DragDropDrives", !DDDrives.IsEmpty());
  Usage->Set(L"ShowingTips", ShowTips);
  Usage->Set(L"KeepingOpenWhenNoSession", KeepOpenWhenNoSession);
  Usage->Set(L"ShowingLoginWhenNoSession", ShowLoginWhenNoSession);
  Usage->Set(L"DefaultingToNewRemoteTab", DefaultToNewRemoteTab);
  TipsUpdateStaticUsage();

  Usage->Set(L"CommanderNortonLikeMode", int(ScpCommander.NortonLikeMode));
  Usage->Set(L"CommanderExplorerKeyboardShortcuts", ScpCommander.ExplorerKeyboardShortcuts);

  Usage->Set(L"ExplorerViewStyle", ScpExplorer.ViewStyle);

  Usage->Set(L"LastMonitor", LastMonitor);

  UnicodeString ExternalEditors;
  for (int Index = 0; Index < EditorList->Count; Index++)
  {
    const TEditorPreferences * Editor = EditorList->Editors[Index];
    if (Editor->Data->Editor == edExternal)
    {
      AddToList(ExternalEditors, Editor->ExtractExternalEditorName(), ",");
    }
  }
  Usage->Set(L"ExternalEditors", ExternalEditors);

  if (LastMachineInstallations < FMachineInstallations)
  {
    Usage->Inc(L"InstallationsMachine", (FMachineInstallations - LastMachineInstallations));
    LastMachineInstallations = FMachineInstallations;
  }

  int ExtensionsPortable = 0;
  int ExtensionsInstalled = 0;
  int ExtensionsUser = 0;
  for (int Index = 0; Index < FExtensionList->Count; Index++)
  {
    const TCustomCommandType * CustomCommand = FExtensionList->Commands[Index];
    UnicodeString PathId = ExcludeTrailingBackslash(ExtractFilePath(CustomCommand->Id));
    if (SameText(PathId, ExtensionsCommonPathId))
    {
      ExtensionsPortable++;
    }
    else if (SameText(PathId, ExtensionsCommonExtPathId))
    {
      ExtensionsInstalled++;
    }
    else if (SameText(PathId, ExtensionsUserExtPathId))
    {
      ExtensionsUser++;
    }
  }
  Usage->Set(L"ExtensionsPortableCount", (ExtensionsPortable));
  Usage->Set(L"ExtensionsInstalledCount", (ExtensionsInstalled));
  Usage->Set(L"ExtensionsUserCount", (ExtensionsUser));

  std::unique_ptr<TStringList> DeletedExtensions(CreateSortedStringList());
  ParseExtensionList(DeletedExtensions.get(), FExtensionsDeleted);
  Usage->Set(L"ExtensionsDeleted", (DeletedExtensions->Count));
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::RestoreFont(
  const TFontConfiguration & Configuration, TFont * Font)
{
  Font->Name = Configuration.FontName;
  Font->Size = Configuration.FontSize;
  Font->Charset = Configuration.FontCharset;
  Font->Style = IntToFontStyles(Configuration.FontStyle);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::StoreFont(
  TFont * Font, TFontConfiguration & Configuration)
{
  Configuration.FontName = Font->Name;
  Configuration.FontSize = Font->Size;
  Configuration.FontCharset = Font->Charset;
  Configuration.FontStyle = FontStylesToInt(Font->Style);
}
//---------------------------------------------------------------------------
TResolvedDoubleClickAction TWinConfiguration::ResolveDoubleClickAction(bool IsDirectory, TTerminal * Terminal)
{
  TResolvedDoubleClickAction Result;
  // Anything special is done on files only (not directories)
  if (IsDirectory)
  {
    Result = rdcaChangeDir;
  }
  else
  {
    Result = rdcaNone;
    if (Terminal != NULL)
    {
      if (!Terminal->ResolvingSymlinks && !Terminal->IsEncryptingFiles() && !AlwaysRespectDoubleClickAction)
      {
        Result = rdcaChangeDir;
      }
    }

    if (Result == rdcaNone)
    {
      switch (DoubleClickAction)
      {
        case dcaOpen:
          Result = rdcaOpen;
          break;

        case dcaCopy:
          Result = rdcaCopy;
          break;

        case dcaEdit:
          Result = rdcaEdit;
          break;

        default:
          DebugFail();
          Abort();
          break;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCustomCommandType::TCustomCommandType() :
  FParams(0), FShortCut(0), FShortCutOriginal(0)
{
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandType::TCustomCommandType(const TCustomCommandType & Other) :
  FName(Other.FName),
  FCommand(Other.FCommand),
  FParams(Other.FParams),
  FShortCut(Other.FShortCut),
  FShortCutOriginal(Other.FShortCutOriginal),
  FId(Other.FId),
  FFileName(Other.FFileName),
  FDescription(Other.FDescription),
  FHomePage(Other.FHomePage),
  FOptionsPage(Other.FOptionsPage),
  FOptions(Other.FOptions)
{
}
//---------------------------------------------------------------------------
TCustomCommandType & TCustomCommandType::operator=(const TCustomCommandType & Other)
{
  FName = Other.FName;
  FCommand = Other.FCommand;
  FParams = Other.FParams;
  FShortCut = Other.FShortCut;
  FShortCutOriginal = Other.FShortCutOriginal;
  FId = Other.FId;
  FFileName = Other.FFileName;
  FDescription = Other.FDescription;
  FHomePage = Other.FHomePage;
  FOptionsPage = Other.FOptionsPage;
  FOptions = Other.FOptions;
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandType::Equals(const TCustomCommandType * Other) const
{
  return
    (FName == Other->FName) &&
    (FCommand == Other->FCommand) &&
    (FParams == Other->FParams) &&
    (FShortCut == Other->FShortCut) &&
    (FShortCutOriginal == Other->FShortCutOriginal) &&
    (FId == Other->FId) &&
    (FFileName == Other->FFileName) &&
    (FDescription == Other->FDescription) &&
    (FHomePage == Other->FHomePage) &&
    (FOptionsPage == Other->FOptionsPage) &&
    (FOptions == Other->FOptions);
}
//---------------------------------------------------------------------------
const UnicodeString ExtensionNameDirective(L"name");
const UnicodeString ExtensionCommandDirective(L"command");
const wchar_t ExtensionMark = L'@';
const UnicodeString WinSCPExtensionExt(L".WinSCPextension");
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandType::GetExtensionId(const UnicodeString & Name)
{
  UnicodeString Result;
  int P = Pos(UpperCase(WinSCPExtensionExt), UpperCase(Name));
  // Ends with Ext or there's another extension after it
  if ((P > 1) &&
      ((Name.Length() == (P + WinSCPExtensionExt.Length() - 1)) || (Name[P + WinSCPExtensionExt.Length()] == L'.')))
  {
    Result = Name.SubString(1, P - 1);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandType::LoadExtension(const UnicodeString & Path)
{
  std::unique_ptr<TStringList> Lines(new TStringList());
  FileName = Path;
  LoadScriptFromFile(Path, Lines.get());
  LoadExtension(Lines.get(), Path);
  Command = ReplaceStr(Command, L"%EXTENSION_PATH%", Path);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandType::LoadExtension(TStrings * Lines, const UnicodeString & PathForBaseName)
{
  Params = ccLocal;
  bool AnythingFound = false;
  std::set<UnicodeString> OptionIds;

  UnicodeString ExtensionBaseName = ExtractExtensionBaseName(PathForBaseName);

  UnicodeString ExtensionLine;
  bool Break = false;
  for (int Index = 0; !Break && (Index < Lines->Count); Index++)
  {
    UnicodeString Line = Lines->Strings[Index].Trim();
    if (!Line.IsEmpty())
    {
      Line = ReplaceChar(Line, L'\t', L' ');

      bool IsComment = false;
      if (StartsText(ExtensionMark, Line))
      {
        IsComment = true;
      }
      else if (StartsText(L"rem ", Line))
      {
        IsComment = true;
        Line.Delete(1, 4);
      }
      else if (StartsText(L"#", Line) || StartsText(L";", Line) || StartsText(L"'", Line))
      {
        IsComment = true;
        Line.Delete(1, 1);
      }
      else if (StartsText(L"//", Line))
      {
        IsComment = true;
        Line.Delete(1, 2);
      }

      if (!IsComment)
      {
        Break = true;
        // ignore this and later lines, but finish processing the previous line with ^, if any
        Line = L"";
      }
      else
      {
        Line = Line.Trim();
      }
    }

    bool Continuation = (Line.Length() > 0) && (Line[Line.Length()] == L'^');
    if (Continuation)
    {
      Line = Line.SubString(1, Line.Length() - 1).Trim();
    }

    AddToList(ExtensionLine, Line, L" ");

    if (!Continuation)
    {
      int P;
      if (!ExtensionLine.IsEmpty() && (ExtensionLine[1] == ExtensionMark) && ((P = Pos(L" ", ExtensionLine)) >= 2))
      {
        UnicodeString Key = ExtensionLine.SubString(2, P - 2).LowerCase();
        UnicodeString Directive = UnicodeString(ExtensionMark) + Key;
        UnicodeString Value = ExtensionLine.SubString(P + 1, ExtensionLine.Length() - P).Trim();
        bool KnownKey = true;
        if (Key == ExtensionNameDirective)
        {
          Name = WinConfiguration->ExtensionStringTranslation(Id, Value);
        }
        else if (Key == ExtensionCommandDirective)
        {
          Command = Value;
        }
        else if (Key == L"require")
        {
          UnicodeString DependencyVersion = Value;
          UnicodeString Dependency = CutToChar(Value, L' ', true).LowerCase();
          Value = Value.Trim();
          bool Failed;
          if (Dependency == L"winscp")
          {
            int Version = StrToCompoundVersion(Value);
            Failed = (Version > WinConfiguration->CompoundVersion);
          }
          else if (Dependency == L".net")
          {
            Failed = (CompareVersion(Value, GetNetVersionStr()) > 0);
          }
          else if (Dependency == L".netcore")
          {
            Failed = (CompareVersion(Value, GetNetCoreVersionStr()) > 0);
          }
          else if (Dependency == L"powershell")
          {
            Failed = (CompareVersion(Value, GetPowerShellVersionStr()) > 0);
          }
          else if (Dependency == L"pwsh")
          {
            Failed = (CompareVersion(Value, GetPowerShellCoreVersionStr()) > 0);
          }
          else if (Dependency == L"windows")
          {
            Failed = (CompareVersion(Value, WindowsVersion()) > 0);
          }
          else
          {
            Failed = true;
          }

          if (Failed)
          {
            throw Exception(MainInstructions(FMTLOAD(EXTENSION_DEPENDENCY_ERROR, (DependencyVersion))));
          }
        }
        else if (Key == L"side")
        {
          if (SameText(Value, L"Local"))
          {
            Params |= ccLocal;
          }
          else if (SameText(Value, L"Remote"))
          {
            Params &= ~ccLocal;
          }
          else
          {
            throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_ERROR, (Value, Directive))));
          }
        }
        else if (Key == L"flag")
        {
          if (SameText(Value, L"ApplyToDirectories"))
          {
            Params |= ccApplyToDirectories;
          }
          else if (SameText(Value, L"Recursive"))
          {
            Params |= ccRecursive;
          }
          else if (SameText(Value, L"ShowResults"))
          {
            Params |= ccShowResults;
          }
          else if (SameText(Value, L"CopyResults"))
          {
            Params |= ccCopyResults;
          }
          else if (SameText(Value, L"RemoteFiles"))
          {
            Params |= ccRemoteFiles;
          }
          else if (SameText(Value, L"ShowResultsInMsgBox"))
          {
            Params |= ccShowResultsInMsgBox;
          }
          else
          {
            throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_ERROR, (Value, Directive))));
          }
        }
        else if (Key == L"shortcut")
        {
          TShortCut AShortCut = TextToShortCut(Value);
          if (IsCustomShortCut(AShortCut))
          {
            ShortCut = AShortCut;
            FShortCutOriginal = AShortCut;
          }
          else
          {
            throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_ERROR, (Value, Directive))));
          }
        }
        else if (Key == L"option")
        {
          TOption Option;
          if (!ParseOption(Value, Option, ExtensionBaseName) ||
              (Option.IsControl && (OptionIds.find(Option.Id.LowerCase()) != OptionIds.end())))
          {
            throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_ERROR, (Value, Directive))));
          }
          else
          {
            FOptions.push_back(Option);
            if (!Option.IsControl)
            {
              OptionIds.insert(Option.Id.LowerCase());
            }
          }
        }
        else if (Key == L"description")
        {
          Description = WinConfiguration->ExtensionStringTranslation(Id, Value);
        }
        else if (Key == L"author")
        {
          // noop
        }
        else if (Key == L"version")
        {
          // noop
        }
        else if (Key == L"homepage")
        {
          HomePage = Value;
        }
        else if (Key == L"optionspage")
        {
          OptionsPage = Value;
        }
        else if (Key == L"source")
        {
          // noop
        }
        else
        {
          KnownKey = false;
        }

        if (KnownKey)
        {
          AnythingFound = true;
        }
      }

      ExtensionLine = L"";
    }
  }

  if (!AnythingFound)
  {
    throw Exception(MainInstructions(LoadStr(EXTENSION_NOT_FOUND)));
  }

  if (Name.IsEmpty())
  {
    throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_MISSING, (UnicodeString(ExtensionMark) + ExtensionNameDirective))));
  }

  if (Command.IsEmpty())
  {
    throw Exception(MainInstructions(FMTLOAD(EXTENSION_DIRECTIVE_MISSING, (UnicodeString(ExtensionMark) + ExtensionCommandDirective))));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandType::ParseOption(const UnicodeString & Value, TOption & Option, const UnicodeString & ExtensionBaseName)
{
  UnicodeString Buf = Value;
  UnicodeString KindName;
  bool Result = CutTokenEx(Buf, Option.Id);

  if (Result)
  {
    Option.Flags = 0;

    UnicodeString FlagName;
    while (CutTokenEx(Buf, FlagName) && (FlagName.SubString(1, 1) == L"-"))
    {
      FlagName = FlagName.LowerCase();
      if (FlagName == L"-run")
      {
        Option.Flags |= ofRun;
      }
      else if (FlagName == L"-config")
      {
        Option.Flags |= ofConfig;
      }
      else if (FlagName == L"-site")
      {
        Option.Flags |= ofSite;
      }
      else
      {
        Result = false;
      }
    }

    if ((Option.Flags & (ofRun | ofConfig)) == 0)
    {
      Option.Flags |= ofConfig;
    }

    KindName = FlagName;

    if (Result)
    {
      UnicodeString DefaultCaption;
      UnicodeString DefaultDefault;
      TOption::TParams DefaultParams;

      KindName = KindName.LowerCase();
      if (KindName == L"label")
      {
        Option.Kind = okLabel;
        Result = !Option.IsControl;
      }
      else if (KindName == L"link")
      {
        Option.Kind = okLink;
        Result = !Option.IsControl;
      }
      else if (KindName == L"separator")
      {
        Option.Kind = okSeparator;
        Result = !Option.IsControl;
      }
      else if (KindName == L"group")
      {
        Option.Kind = okGroup;
        Result = !Option.IsControl;
      }
      else if (KindName == L"textbox")
      {
        Option.Kind = okTextBox;
        Result = Option.IsControl;
      }
      else if (KindName == L"file")
      {
        Option.Kind = okFile;
        Result = Option.IsControl;
      }
      else if (KindName == L"sessionlogfile")
      {
        Option.Kind = okFile;
        Result = Option.IsControl;
        DefaultCaption = LoadStr(EXTENSION_SESSIONLOG_FILE);
        Option.FileCaption = LoadStr(EXTENSION_SESSIONLOG_CAPTION);
        Option.FileFilter = LoadStr(EXTENSION_SESSIONLOG_FILTER);
        // Similar to TConfiguration::GetDefaultLogFileName
        Option.FileInitial = L"%TEMP%\\" + ExtensionBaseName + L".log";
        Option.FileExt = L"log";
      }
      else if (KindName == L"dropdownlist")
      {
        Option.Kind = okDropDownList;
        Result = Option.IsControl;
      }
      else if (KindName == L"combobox")
      {
        Option.Kind = okComboBox;
        Result = Option.IsControl;
      }
      else if (KindName == L"checkbox")
      {
        Option.Kind = okCheckBox;
        Result = Option.IsControl;
      }
      else if (KindName == L"pausecheckbox")
      {
        Option.Kind = okCheckBox;
        Result = Option.IsControl;
        DefaultCaption = LoadStr(EXTENSION_PAUSE_CHECKBOX);
        DefaultDefault = L"-pause";
        DefaultParams.push_back(L"-pause");
      }
      else
      {
        Option.Kind = okUnknown;
      }

      if ((Option.Kind != okUnknown) &&
          (Option.Kind != okSeparator))
      {
        Result =
          CutTokenEx(Buf, Option.Caption);

        if (!Result && !DefaultCaption.IsEmpty())
        {
          Option.Caption = DefaultCaption;
          Result = true;
        }

        if (Result)
        {
          Option.Caption = WinConfiguration->ExtensionStringTranslation(Id, Option.Caption);

          if (CutTokenEx(Buf, Option.Default))
          {
            UnicodeString Param;
            while (CutTokenEx(Buf, Param))
            {
              Option.Params.push_back(Param);
            }
          }
          else
          {
            Option.Default = DefaultDefault;
          }

          if (Option.Params.size() == 0)
          {
            Option.Params = DefaultParams;
          }
        }
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandType::GetOptionsCount() const
{
  return FOptions.size();
}
//---------------------------------------------------------------------------
const TCustomCommandType::TOption & __fastcall TCustomCommandType::GetOption(int Index) const
{
  return FOptions[Index];
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandType::GetOptionKey(
  const TCustomCommandType::TOption & Option, const UnicodeString & Site) const
{
  UnicodeString Result = Id + L"\\" + Option.Id;
  if (FLAGSET(Option.Flags, ofSite))
  {
    Result += L"\\" + Site;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandType::AnyOptionWithFlag(unsigned int Flag) const
{
  bool Result = false;
  for (int Index = 0; !Result && (Index < OptionsCount); Index++)
  {
    const TCustomCommandType::TOption & Option = GetOption(Index);
    Result = FLAGSET(Option.Flags, Flag);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandType::GetCommandWithExpandedOptions(
  TStrings * CustomCommandOptions, const UnicodeString & Site) const
{
  UnicodeString Result = Command;
  for (int Index = 0; Index < OptionsCount; Index++)
  {
    const TCustomCommandType::TOption & Option = GetOption(Index);
    if (Option.IsControl)
    {
      UnicodeString OptionKey = GetOptionKey(Option, Site);
      UnicodeString OptionValue;
      bool NeedEscape;
      if (CustomCommandOptions->IndexOfName(OptionKey) >= 0)
      {
        OptionValue = CustomCommandOptions->Values[OptionKey];
        NeedEscape = true;
      }
      else
      {
        OptionValue = Option.Default;
        NeedEscape = !Option.CanHavePatterns(); // approximation only?
      }
      UnicodeString OptionCommand = GetOptionCommand(Option, OptionValue);
      if (NeedEscape)
      {
        OptionCommand = TCustomCommand::Escape(OptionCommand);
      }
      Result = ReplaceText(Result, FORMAT(L"%%%s%%", (Option.Id)), OptionCommand);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandType::GetOptionCommand(const TOption & Option, const UnicodeString & Value) const
{
  UnicodeString Result = Value;

  switch (Option.Kind)
  {
    case okUnknown:
    case okTextBox:
    case okDropDownList:
    case okComboBox:
    case okCheckBox:
      // noop
      break;

    case okFile:
      Result = ExpandEnvironmentVariables(Result);
      break;

    case okLabel:
    case okLink:
    case okSeparator:
    case okGroup:
    default:
      DebugFail();
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandType::HasCustomShortCut() const
{
  return (ShortCut != FShortCutOriginal);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandType::TOption::GetIsControl() const
{
  return (Id != L"-");
}
//---------------------------------------------------------------------------
bool TCustomCommandType::TOption::CanHavePatterns() const
{
  bool Result;
  switch (Kind)
  {
    case okTextBox:
    case okFile:
      Result = true;
      break;

    default:
      Result = false;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TCustomCommandType::TOption::HasPatterns(TCustomCommand * CustomCommandForOptions) const
{
  bool Result =
    CanHavePatterns() &&
    FLAGSET(Flags, TCustomCommandType::ofRun) &&
    FLAGCLEAR(Flags, TCustomCommandType::ofConfig) &&
    CustomCommandForOptions->HasAnyPatterns(Default);
  return Result;
}
//---------------------------------------------------------------------------
bool TCustomCommandType::TOption::operator==(const TCustomCommandType::TOption & Other) const
{
  // needed by vector<> but probably never used
  return
    (Id == Other.Id) &&
    (Flags == Other.Flags) &&
    (Kind == Other.Kind) &&
    (Caption == Other.Caption) &&
    (Default == Other.Default) &&
    (Params == Other.Params) &&
    (FileCaption == Other.FileCaption) &&
    (FileFilter == Other.FileFilter) &&
    (FileInitial == Other.FileInitial) &&
    (FileExt == Other.FileExt);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCustomCommandList::TCustomCommandList()
{
  FCommands = new TList();
  FModified = false;
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandList::~TCustomCommandList()
{
  Clear();
  delete FCommands;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Reset()
{
  FModified = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Modify()
{
  FModified = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Load(THierarchicalStorage * Storage)
{
  Clear();

  if (Storage->OpenSubKey(L"CustomCommands", false))
  {
    TStrings * Names = new TStringList();
    try
    {
      Storage->ReadValues(Names, true);
      for (int Index = 0; Index < Names->Count; Index++)
      {
        TCustomCommandType * Command = new TCustomCommandType();
        Command->Name = Names->Names[Index];
        Command->Command = Names->Values[Names->Names[Index]];
        FCommands->Add(Command);
      }
      Storage->CloseSubKey();
    }
    __finally
    {
      delete Names;
    }
  }

  if (Storage->OpenSubKey(L"CustomCommandsParams", false))
  {
    for (int Index = 0; Index < FCommands->Count; Index++)
    {
      TCustomCommandType * Command = GetCommand(Index);
      Command->Params = Storage->ReadInteger(Command->Name, Command->Params);
    }
    Storage->CloseSubKey();
  }

  if (Storage->OpenSubKey(L"CustomCommandsShortCuts", false))
  {
    for (int Index = 0; Index < FCommands->Count; Index++)
    {
      TCustomCommandType * Command = GetCommand(Index);
      Command->ShortCut = (Word)Storage->ReadInteger(Command->Name, Command->ShortCut);
    }
    Storage->CloseSubKey();
  }
  Reset();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Save(THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(L"CustomCommands", true))
  {
    Storage->ClearValues();
    for (int Index = 0; Index < FCommands->Count; Index++)
    {
      const TCustomCommandType * Command = Commands[Index];
      Storage->WriteString(Command->Name, Command->Command);
    }
    Storage->CloseSubKey();
  }
  if (Storage->OpenSubKey(L"CustomCommandsParams", true))
  {
    Storage->ClearValues();
    for (int Index = 0; Index < FCommands->Count; Index++)
    {
      const TCustomCommandType * Command = Commands[Index];
      Storage->WriteInteger(Command->Name, Command->Params);
    }
    Storage->CloseSubKey();
  }
  if (Storage->OpenSubKey(L"CustomCommandsShortCuts", true))
  {
    Storage->ClearValues();
    for (int Index = 0; Index < FCommands->Count; Index++)
    {
      const TCustomCommandType * Command = Commands[Index];
      if (Command->ShortCut != 0)
      {
        Storage->WriteInteger(Command->Name, Command->ShortCut);
      }
    }
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Clear()
{
  for (int Index = 0; Index < FCommands->Count; Index++)
  {
    delete Commands[Index];
  }
  FCommands->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Add(const UnicodeString Name,
  const UnicodeString ACommand, int Params)
{
  TCustomCommandType * Command = new TCustomCommandType();
  Command->Name = Name;
  Command->Command = ACommand;
  Command->Params = Params;
  Add(Command);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Add(TCustomCommandType * Command)
{
  Insert(Count, Command);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Insert(int Index, TCustomCommandType * Command)
{
  FCommands->Insert(Index, Command);
  Modify();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Change(int Index, TCustomCommandType * ACommand)
{
  TCustomCommandType * Command = GetCommand(Index);
  if (!Command->Equals(ACommand))
  {
    delete Command;
    FCommands->Items[Index] = ACommand;
    Modify();
  }
  else
  {
    delete ACommand;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Move(int CurIndex, int NewIndex)
{
  if (CurIndex != NewIndex)
  {
    FCommands->Move(CurIndex, NewIndex);
    Modify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Delete(int Index)
{
  DebugAssert((Index >= 0) && (Index < Count));
  delete GetCommand(Index);
  FCommands->Delete(Index);
  Modify();
}
//---------------------------------------------------------------------------
class TCustomCommandCompareFunc : public TCppInterfacedObject<TListSortCompareFunc>
{
public:
  TCustomCommandCompareFunc(TStrings * Ids)
  {
    FIds = Ids;
  }

  virtual int __fastcall Invoke(void * Item1, void * Item2)
  {
    TCustomCommandType * CustomCommand1 = static_cast<TCustomCommandType *>(Item1);
    TCustomCommandType * CustomCommand2 = static_cast<TCustomCommandType *>(Item2);
    int Index1 = FIds->IndexOf(CustomCommand1->Id);
    int Index2 = FIds->IndexOf(CustomCommand2->Id);
    int Result;
    // new items to the end
    if ((Index1 < 0) && (Index2 >= 0))
    {
      Result = 1;
    }
    else if ((Index2 < 0) && (Index1 >= 0))
    {
      Result = -1;
    }
    // fallback to comparing by name
    else if ((Index1 < 0) && (Index2 < 0))
    {
      Result = TComparer__1<UnicodeString>::Default()->Compare(CustomCommand1->Name, CustomCommand2->Name);
    }
    else
    {
      Result = TComparer__1<int>::Default()->Compare(Index1, Index2);
    }
    return Result;
  }

private:
  TStrings * FIds;
};
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::SortBy(TStrings * Ids)
{
  TCustomCommandCompareFunc * Func = new TCustomCommandCompareFunc(Ids);
  FCommands->SortList(Func);
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandList::GetCount() const
{
  return FCommands->Count;
}
//---------------------------------------------------------------------------
const TCustomCommandType * __fastcall TCustomCommandList::GetConstCommand(int Index) const
{
  return static_cast<TCustomCommandType *>(FCommands->Items[Index]);
}
//---------------------------------------------------------------------------
TCustomCommandType * __fastcall TCustomCommandList::GetCommand(int Index)
{
  return static_cast<TCustomCommandType *>(FCommands->Items[Index]);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandList::Equals(const TCustomCommandList * Other) const
{
  bool Result = (Count == Other->Count);
  for (int Index = 0; Result && (Index < Count); Index++)
  {
    Result = Commands[Index]->Equals(Other->Commands[Index]);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::Assign(const TCustomCommandList * Other)
{
  Clear();
  for (int Index = 0; Index < Other->Count; Index++)
  {
    Add(new TCustomCommandType(*Other->Commands[Index]));
  }
  // there should be comparison of with the assigned list, be we rely on caller
  // to do it instead (TGUIConfiguration::SetCopyParamList)
  Modify();
}
//---------------------------------------------------------------------------
const TCustomCommandType * TCustomCommandList::Find(const UnicodeString Name) const
{
  for (int Index = 0; Index < FCommands->Count; Index++)
  {
    if (Commands[Index]->Name == Name)
    {
      return Commands[Index];
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
const TCustomCommandType * TCustomCommandList::Find(TShortCut ShortCut) const
{
  for (int Index = 0; Index < FCommands->Count; Index++)
  {
    if (Commands[Index]->ShortCut == ShortCut)
    {
      return Commands[Index];
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
int TCustomCommandList::FindIndexByFileName(const UnicodeString & FileName) const
{
  for (int Index = 0; Index < FCommands->Count; Index++)
  {
    if (IsPathToSameFile(Commands[Index]->FileName, FileName))
    {
      return Index;
    }
  }
  return -1;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandList::ShortCuts(TShortCuts & ShortCuts) const
{
  for (int Index = 0; Index < FCommands->Count; Index++)
  {
    const TCustomCommandType * Command = Commands[Index];
    if (Command->ShortCut != 0)
    {
      ShortCuts.Add(Command->ShortCut);
    }
  }
}
