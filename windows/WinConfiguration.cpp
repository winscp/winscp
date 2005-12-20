//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "WinConfiguration.h"
#include "Common.h"
#include "Bookmarks.h"
#include "Terminal.h"
#include "TextsWin.h"
#include "WinInterface.h"
#include "GUITools.h"
#include "Tools.h"
#include <ResourceModule.hpp>
#include <InitGUID.h>
#include <DragExt.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TEditorPreferences::TEditorPreferences()
{
  FData.FileMask = "*.*";
  FData.Editor = edInternal;
  FData.ExternalEditor = "";
  FData.ExternalEditorText = true;
  FData.MDIExternalEditor = false;
  FData.DetectMDIExternalEditor = true;
}
//---------------------------------------------------------------------------
__fastcall TEditorPreferences::TEditorPreferences(const TEditorPreferences & Source)
{
  FData.FileMask = Source.FData.FileMask;
  FData.Editor = Source.FData.Editor;
  FData.ExternalEditor = Source.FData.ExternalEditor;
  FData.ExternalEditorText = Source.FData.ExternalEditorText;
  FData.MDIExternalEditor = Source.FData.MDIExternalEditor;
  FData.DetectMDIExternalEditor = Source.FData.DetectMDIExternalEditor;
}
//---------------------------------------------------------------------------
#define C(Property) (Property == rhp.Property)
bool __fastcall TEditorPreferences::operator==(const TEditorPreferences & rhp) const
{
  return
    C(FData.FileMask) &&
    C(FData.Editor) &&
    C(FData.ExternalEditor) &&
    C(FData.ExternalEditorText) &&
    C(FData.MDIExternalEditor) &&
    C(FData.DetectMDIExternalEditor) &&
    true;
}
#undef C
//---------------------------------------------------------------------------
bool __fastcall TEditorPreferences::Matches(const AnsiString FileName, bool Local) const
{
  return FData.FileMask.Matches(FileName, Local, false);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::LegacyDefaults()
{
  FData.ExternalEditor = "notepad.exe";
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::Load(THierarchicalStorage * Storage, bool Legacy)
{
  if (!Legacy)
  {
    FData.FileMask = Storage->ReadString("FileMask", FData.FileMask.Masks);
  }
  FData.Editor = (TEditor)Storage->ReadInteger("Editor", FData.Editor);
  FData.ExternalEditor = Storage->ReadString("ExternalEditor", FData.ExternalEditor);
  FData.ExternalEditorText = Storage->ReadBool("ExternalEditorText", FData.ExternalEditorText);
  FData.MDIExternalEditor = Storage->ReadBool("MDIExternalEditor", FData.MDIExternalEditor);
  FData.DetectMDIExternalEditor = Storage->ReadBool("DetectMDIExternalEditor", FData.DetectMDIExternalEditor);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferences::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteString("FileMask", FData.FileMask.Masks);
  Storage->WriteInteger("Editor", FData.Editor);
  Storage->WriteString("ExternalEditor", FData.ExternalEditor);
  Storage->WriteBool("ExternalEditorText", FData.ExternalEditorText);
  Storage->WriteBool("MDIExternalEditor", FData.MDIExternalEditor);
  Storage->WriteBool("DetectMDIExternalEditor", FData.DetectMDIExternalEditor);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TEditorPreferences::GetName() const
{
  if (FName.IsEmpty())
  {
    if (FData.Editor == edInternal)
    {
      FName = StripHotkey(LoadStr(INTERNAL_EDITOR_NAME));
    }
    else
    {
      AnsiString Program, Params, Dir;
      AnsiString ExternalEditor = FData.ExternalEditor;
      ReformatFileNameCommand(ExternalEditor);
      SplitCommand(ExternalEditor, Program, Params, Dir);
      FName = ExtractFileName(Program);
      int P = FName.LastDelimiter(".");
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
void __fastcall TEditorList::operator=(const TEditorList & rhl)
{
  Clear();

  for (int Index = 0; Index < rhl.Count; Index++)
  {
    Add(new TEditorPreferences(*rhl.Editors[Index]));
  }
  // there should be comparison of with the assigned list, be we rely on caller
  // to do it instead (TWinConfiguration::SetEditorList)
  Modify();
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
  assert((Index >= 0) && (Index < Count));
  delete Editors[Index];
  FEditors->Delete(Index);
  Modify();
}
//---------------------------------------------------------------------------
const TEditorPreferences * __fastcall TEditorList::Find(
  const AnsiString FileName, bool Local) const
{
  const TEditorPreferences * Result = NULL;
  int i = 0;
  while ((i < FEditors->Count) && (Result == NULL))
  {
    Result = Editors[i];
    if (!Result->Matches(FileName, Local))
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
    AnsiString Name = IntToStr(Index);
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
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::TWinConfiguration(): TCustomWinConfiguration()
{
  FInvalidDefaultTranslation = false;
  FDDExtInstalled = -1;
  FBookmarks = new TBookmarks();
  FCustomCommands = new TCustomCommands();
  FEditorList = new TEditorList();
  Default();

  try
  {
    FDefaultTranslationFile = GetResourceModule(ModuleFileName().c_str());
    CheckTranslationVersion(FDefaultTranslationFile, true);
  }
  catch(Exception & E)
  {
    FInvalidDefaultTranslationMessage = E.Message;
    FInvalidDefaultTranslation = true;
  }
}
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::~TWinConfiguration()
{
  if (!FTemporarySessionFile.IsEmpty()) DeleteFile(FTemporarySessionFile);
  ClearTemporaryLoginData();

  delete FBookmarks;
  delete FCustomCommands;
  delete FEditorList;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Default()
{
  FCustomCommandsDefaults = true;

  TCustomWinConfiguration::Default();

  FDDAllowMove = false;
  FDDAllowMoveInit = false;
  FDDTransferConfirmation = true;
  FDDTemporaryDirectory = "";
  FDDWarnLackOfTempSpace = true;
  FDDWarnLackOfTempSpaceRatio = 1.1;
  FDDExtEnabled = DDExtInstalled;
  FDDExtTimeout = 1000;
  FDeleteToRecycleBin = true;
  FSelectDirectories = false;
  FSelectMask = "*.*";
  FShowHiddenFiles = true;
  FShowInaccesibleDirectories = true;
  FConfirmDeleting = true;
  FConfirmClosingSession = true;
  FConfirmExitOnCompletion = true;
  FForceDeleteTempFolder = true;
  FDoubleClickAction = dcaOpen;
  FCopyOnDoubleClickConfirmation = false;
  FDimmHiddenFiles = true;
  FAutoStartSession = "";
  FExpertMode = true;
  FUseLocationProfiles = false;
  FDefaultDirIsHome = true;
  FDDDeleteDelay = 120;
  FTemporaryDirectoryCleanup = true;
  FConfirmTemporaryDirectoryCleanup = true;
  FPreservePanelState = true;
  FTheme = "OfficeXP";
  FPathInCaption = picShort;
  FCopyParamAutoSelectNotice = true;
  FSessionToolbarAutoShown = false;
  FLockToolbars = false;
  FAutoOpenInPutty = false;

  FEditor.FontName = "Courier New";
  FEditor.FontHeight = -12;
  FEditor.FontStyle = 0;
  FEditor.FontCharset = DEFAULT_CHARSET;
  FEditor.WordWrap = false;
  FEditor.FindText = "";
  FEditor.ReplaceText = "";
  FEditor.FindMatchCase = false;
  FEditor.FindWholeWord = false;
  FEditor.SingleEditor = false;
  FEditor.MaxEditors = 500;
  FEditor.EarlyClose = 2; // seconds

  FQueueView.Height = 100;
  FQueueView.Layout = "70,160,160,80,80,80";
  FQueueView.Show = qvHideWhenEmpty;
  FQueueView.LastHideShow = qvHideWhenEmpty;
  FQueueView.ToolBar = false;

  FUpdates.Period = 0;
  FUpdates.LastCheck = 0;
  FUpdates.HaveResults = false;
  FUpdates.ShownResults = false;
  FUpdates.ProxyHost = "";
  FUpdates.ProxyPort = 8080;
  FUpdates.Results.Reset();

  FLogWindowOnStartup = true;
  FLogWindowParams = "-1;-1;500;400";

  FScpExplorer.WindowParams = "-1;-1;600;400;0";
  FScpExplorer.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,1;20,0;150,0;125,0|0;1;8;2;3;4;5;6;7";
  FScpExplorer.ToolbarsLayout =
    "Queue_Visible=1,Queue_LastDock=QueueDock,Queue_DockRow=0,Queue_DockPos=-1,Queue_FloatLeft=0,Queue_FloatTop=0,Queue_FloatRightX=0,"
    "Menu_Visible=1,Menu_DockedTo=TopDock,Menu_LastDock=TopDock,Menu_DockRow=0,Menu_DockPos=0,Menu_FloatLeft=0,Menu_FloatTop=0,Menu_FloatRightX=0,"
    "Buttons_Visible=1,Buttons_DockedTo=TopDock,Buttons_LastDock=TopDock,Buttons_DockRow=2,Buttons_DockPos=0,Buttons_FloatLeft=0,Buttons_FloatTop=0,Buttons_FloatRightX=0,"
    "Selection_Visible=0,Selection_DockedTo=TopDock,Selection_LastDock=TopDock,Selection_DockRow=3,Selection_DockPos=0,Selection_FloatLeft=227,Selection_FloatTop=445,Selection_FloatRightX=0,"
    "Session_Visible=0,Session_DockedTo=TopDock,Session_LastDock=TopDock,Session_DockRow=6,Session_DockPos=0,Session_FloatLeft=39,Session_FloatTop=160,Session_FloatRightX=0,"
    "Preferences_Visible=1,Preferences_DockedTo=TopDock,Preferences_LastDock=TopDock,Preferences_DockRow=4,Preferences_DockPos=0,Preferences_FloatLeft=0,Preferences_FloatTop=0,Preferences_FloatRightX=0,"
    "Sort_Visible=0,Sort_DockedTo=TopDock,Sort_LastDock=TopDock,Sort_DockRow=5,Sort_DockPos=0,Sort_FloatLeft=0,Sort_FloatTop=0,Sort_FloatRightX=0,"
    "Address_Visible=1,Address_DockedTo=TopDock,Address_LastDock=TopDock,Address_DockRow=1,Address_DockPos=0,Address_FloatLeft=0,Address_FloatTop=0,Address_FloatRightX=0,"
    "Updates_Visible=1,Updates_DockedTo=TopDock,Updates_LastDock=TopDock,Updates_DockRow=4,Updates_DockPos=302,Updates_FloatLeft=0,Updates_FloatTop=0,Updates_FloatRightX=0,"
    "Transfer_Visible=1,Transfer_DockedTo=TopDock,Transfer_LastDock=TopDock,Transfer_DockRow=4,Transfer_DockPos=155,Transfer_FloatLeft=0,Transfer_FloatTop=0,Transfer_FloatRightX=0"
    "CustomCommands_Visible=0,CustomCommands_DockedTo=TopDock,CustomCommands_LastDock=TopDock,CustomCommands_DockRow=7,CustomCommands_DockPos=0,CustomCommands_FloatLeft=0,CustomCommands_FloatTop=0,CustomCommands_FloatRightX=0";
  FScpExplorer.StatusBar = true;
  AnsiString PersonalFolder;
  ::SpecialFolderLocation(CSIDL_PERSONAL, PersonalFolder);
  FScpExplorer.LastLocalTargetDirectory = PersonalFolder;
  FScpExplorer.ViewStyle = 0; /* vsIcon */
  FScpExplorer.ShowFullAddress = true;
  FScpExplorer.DriveView = true;
  FScpExplorer.DriveViewWidth = 180;
  FScpExplorer.SessionComboWidth = 114;

  FScpCommander.WindowParams = ((Screen->Width > 800) && (Screen->Height > 650)) ?
    "-1;-1;750;600;0" : "-1;-1;600;400;0";
  FScpCommander.LocalPanelWidth = 0.5;
  FScpCommander.SwappedPanels = false;
  FScpCommander.StatusBar = true;
  FScpCommander.CommandLine = false;
  FScpCommander.NortonLikeMode = nlOn;
  FScpCommander.PreserveLocalDirectory = false;
  // Toolbar_FloatRightX=1 makes keybar apper initialy "in column" when undocked
  FScpCommander.ToolbarsLayout =
    "Queue_Visible=1,Queue_LastDock=QueueDock,Queue_DockRow=0,Queue_DockPos=-1,Queue_FloatLeft=0,Queue_FloatTop=0,Queue_FloatRightX=0,"
    "Session_Visible=0,Session_DockedTo=TopDock,Session_LastDock=TopDock,Session_DockRow=4,Session_DockPos=0,Session_FloatLeft=380,Session_FloatTop=197,Session_FloatRightX=0,"
    "Preferences_Visible=1,Preferences_DockedTo=TopDock,Preferences_LastDock=TopDock,Preferences_DockRow=1,Preferences_DockPos=0,Preferences_FloatLeft=0,Preferences_FloatTop=0,Preferences_FloatRightX=0,"
    "Selection_Visible=1,Selection_DockedTo=TopDock,Selection_LastDock=TopDock,Selection_DockRow=1,Selection_DockPos=257,Selection_FloatLeft=0,Selection_FloatTop=0,Selection_FloatRightX=0,"
    "Command_Visible=0,Command_DockedTo=TopDock,Command_LastDock=TopDock,Command_DockRow=2,Command_DockPos=0,Command_FloatLeft=0,Command_FloatTop=0,Command_FloatRightX=0,"
    "Sort_Visible=0,Sort_DockedTo=TopDock,Sort_LastDock=TopDock,Sort_DockRow=3,Sort_DockPos=0,Sort_FloatLeft=0,Sort_FloatTop=0,Sort_FloatRightX=0,"
    "Commands_Visible=1,Commands_DockedTo=TopDock,Commands_LastDock=TopDock,Commands_DockRow=1,Commands_DockPos=97,Commands_FloatLeft=0,Commands_FloatTop=0,Commands_FloatRightX=0,"
    "Menu_Visible=1,Menu_DockedTo=TopDock,Menu_LastDock=TopDock,Menu_DockRow=0,Menu_DockPos=0,Menu_FloatLeft=0,Menu_FloatTop=0,Menu_FloatRightX=0,"
    "Updates_Visible=1,Updates_DockedTo=TopDock,Updates_LastDock=TopDock,Updates_DockRow=1,Updates_DockPos=535,Updates_FloatLeft=0,Updates_FloatTop=0,Updates_FloatRightX=0,"
    "Transfer_Visible=1,Transfer_DockedTo=TopDock,Transfer_LastDock=TopDock,Transfer_DockRow=1,Transfer_DockPos=388,Transfer_FloatLeft=0,Transfer_FloatTop=0,Transfer_FloatRightX=0,"
    "UploadDownload_Visible=0,UploadDownload_DockedTo=TopDock,UploadDownload_LastDock=TopDock,UploadDownload_DockRow=5,UploadDownload_DockPos=0,UploadDownload_FloatLeft=0,UploadDownload_FloatTop=0,UploadDownload_FloatRightX=0,"
    "CustomCommands_Visible=0,CustomCommands_DockedTo=TopDock,CustomCommands_LastDock=TopDock,CustomCommands_DockRow=6,CustomCommands_DockPos=0,CustomCommands_FloatLeft=0,CustomCommands_FloatTop=0,CustomCommands_FloatRightX=0"
    "RemotePath_Visible=1,RemotePath_DockedTo=RemoteTopDock,RemotePath_LastDock=RemoteTopDock,RemotePath_DockRow=0,RemotePath_DockPos=0,RemotePath_FloatLeft=0,RemotePath_FloatTop=0,RemotePath_FloatRightX=0,"
    "RemoteHistory_Visible=1,RemoteHistory_DockedTo=RemoteTopDock,RemoteHistory_LastDock=RemoteTopDock,RemoteHistory_DockRow=0,RemoteHistory_DockPos=135,RemoteHistory_FloatLeft=0,RemoteHistory_FloatTop=0,RemoteHistory_FloatRightX=0,"
    "RemoteNavigation_Visible=1,RemoteNavigation_DockedTo=RemoteTopDock,RemoteNavigation_LastDock=RemoteTopDock,RemoteNavigation_DockRow=0,RemoteNavigation_DockPos=215,RemoteNavigation_FloatLeft=0,RemoteNavigation_FloatTop=0,RemoteNavigation_FloatRightX=0,"
    "LocalPath_Visible=1,LocalPath_DockedTo=LocalTopDock,LocalPath_LastDock=LocalTopDock,LocalPath_DockRow=0,LocalPath_DockPos=0,LocalPath_FloatLeft=0,LocalPath_FloatTop=0,LocalPath_FloatRightX=0,"
    "LocalHistory_Visible=1,LocalHistory_DockedTo=LocalTopDock,LocalHistory_LastDock=LocalTopDock,LocalHistory_DockRow=0,LocalHistory_DockPos=135,LocalHistory_FloatLeft=0,LocalHistory_FloatTop=0,LocalHistory_FloatRightX=0,"
    "LocalNavigation_Visible=1,LocalNavigation_DockedTo=LocalTopDock,LocalNavigation_LastDock=LocalTopDock,LocalNavigation_DockRow=0,LocalNavigation_DockPos=215,LocalNavigation_FloatLeft=0,LocalNavigation_FloatTop=0,LocalNavigation_FloatRightX=0,"
    "Toolbar_Visible=1,Toolbar_DockedTo=BottomDock,Toolbar_LastDock=BottomDock,Toolbar_DockRow=0,Toolbar_DockPos=0,Toolbar_FloatLeft=0,Toolbar_FloatTop=0,Toolbar_FloatRightX=1";
  FScpCommander.CurrentPanel = osLocal;
  FScpCommander.CompareByTime = true;
  FScpCommander.CompareBySize = false;
  FScpCommander.SessionComboWidth = 114;
  FScpCommander.FullRowSelect = true;
  FScpCommander.RemotePanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0;20,0;150,0;125,0|0;1;8;2;3;4;5;6;7";
  FScpCommander.RemotePanel.StatusBar = true;
  FScpCommander.RemotePanel.DriveView = false;
  FScpCommander.RemotePanel.DriveViewHeight = 100;
  FScpCommander.LocalPanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0|0;1;2;3;4;5";
  FScpCommander.LocalPanel.StatusBar = true;
  FScpCommander.LocalPanel.DriveView = false;
  FScpCommander.LocalPanel.DriveViewHeight = 100;

  FBookmarks->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::DefaultLocalized()
{
  TGUIConfiguration::DefaultLocalized();

  if (FCustomCommandsDefaults)
  {
    FCustomCommands->Clear();
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_EXECUTE)] = "\"!\"";
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_EXECUTE)] = 0;
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_TOUCH)] = "touch \"!\"";
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_TOUCH)] = ccApplyToDirectories | ccRecursive;
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_TAR)] =
      FORMAT("tar -cz  -f \"!?%s?archive.tgz!\" !&",
        (LoadStr(CUSTOM_COMMAND_TAR_ARCHIVE)));
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_TAR)] = ccApplyToDirectories;
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_UNTAR)] =
      FORMAT("tar -xz --directory=\"!?%s?.!\" -f \"!\"",
        (LoadStr(CUSTOM_COMMAND_UNTAR_DIRECTORY)));
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_UNTAR)] = 0;
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_GREP)] =
      FORMAT("grep \"!?%s?!\" !&", (LoadStr(CUSTOM_COMMAND_GREP_PATTERN)));
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_GREP)] = ccShowResults;
    if (Win32Platform == VER_PLATFORM_WIN32_NT)
    {
      FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_FC)] =
        "cmd /c fc \"!\" \"\!^!\" | more && pause";
      FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_FC)] = ccLocal;
    }
    FCustomCommands->Values[LoadStr(CUSTOM_COMMAND_DF)] = "df";
    FCustomCommands->Params[LoadStr(CUSTOM_COMMAND_DF)] = ccShowResults;
    FCustomCommandsDefaults = true;
    FCustomCommandsModified = false;
  }
}
//---------------------------------------------------------------------------
TStorage __fastcall TWinConfiguration::GetStorage()
{
  if (FStorage == stDetect)
  {
    if (FindResourceEx(NULL, RT_RCDATA, "WINSCP_SESSION",
      MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL)))
    {
      FTemporarySessionFile =
        IncludeTrailingBackslash(SystemTemporaryDirectory()) + "winscp3s.tmp";
      DumpResourceToFile("WINSCP_SESSION", FTemporarySessionFile);
      FEmbeddedSessions = true;
      FTemporaryKeyFile =
        IncludeTrailingBackslash(SystemTemporaryDirectory()) + "winscp3k.tmp";
      if (!DumpResourceToFile("WINSCP_KEY", FTemporaryKeyFile))
      {
        FTemporaryKeyFile = "";
      }
    }
  }
  return TCustomWinConfiguration::GetStorage();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ModifyAll()
{
  TCustomWinConfiguration::ModifyAll();
  FBookmarks->ModifyAll(true);
  if (!FCustomCommandsDefaults)
  {
    FCustomCommandsModified = true;
  }
  FEditorList->Modify();
}
//---------------------------------------------------------------------------
THierarchicalStorage * TWinConfiguration::CreateScpStorage(bool SessionList)
{
  if (SessionList && !FTemporarySessionFile.IsEmpty())
  {
    return new TIniFileStorage(FTemporarySessionFile);
  }
  else
  {
    return TCustomWinConfiguration::CreateScpStorage(SessionList);
  }
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(".>")+1, ELEM.Length() - ELEM.LastDelimiter(".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define KEY(TYPE, VAR) KEYEX(TYPE, VAR, VAR)
#define REGCONFIG(CANCREATE) \
  BLOCK("Interface", CANCREATE, \
    KEYEX(Integer,DoubleClickAction, CopyOnDoubleClick); \
    KEY(Bool,     CopyOnDoubleClickConfirmation); \
    KEY(Bool,     DDAllowMove); \
    KEY(Bool,     DDAllowMoveInit); \
    KEY(Bool,     DDTransferConfirmation); \
    KEY(String,   DDTemporaryDirectory); \
    KEY(Bool,     DDWarnLackOfTempSpace); \
    KEY(Float,    DDWarnLackOfTempSpaceRatio); \
    KEY(Bool,     DeleteToRecycleBin); \
    KEY(Bool,     DimmHiddenFiles); \
    KEY(Bool,     SelectDirectories); \
    KEY(String,   SelectMask); \
    KEY(Bool,     ShowHiddenFiles); \
    KEY(Bool,     ShowInaccesibleDirectories); \
    KEY(Bool,     ConfirmDeleting); \
    KEY(Bool,     ConfirmClosingSession); \
    KEY(Bool,     ConfirmExitOnCompletion); \
    KEY(String,   AutoStartSession); \
    KEY(Bool,     UseLocationProfiles); \
    KEY(Bool,     ForceDeleteTempFolder); \
    KEY(Integer,  LocaleSafe); \
    KEY(Bool,     DDExtEnabled); \
    KEY(Integer,  DDExtTimeout); \
    KEY(Bool,     DefaultDirIsHome); \
    KEY(Bool,     TemporaryDirectoryCleanup); \
    KEY(Bool,     ConfirmTemporaryDirectoryCleanup); \
    KEY(Bool,     PreservePanelState); \
    KEY(String,   Theme); \
    KEY(Integer,  PathInCaption); \
    KEY(Bool,     CopyParamAutoSelectNotice); \
    KEY(Bool,     SessionToolbarAutoShown); \
    KEY(Bool,     LockToolbars); \
    KEY(Bool,     AutoOpenInPutty); \
  ); \
  BLOCK("Interface\\Editor", CANCREATE, \
    KEY(String,   Editor.FontName); \
    KEY(Integer,  Editor.FontHeight); \
    KEY(Integer,  Editor.FontStyle); \
    KEY(Integer,  Editor.FontCharset); \
    KEY(Bool,     Editor.WordWrap); \
    KEY(String,   Editor.FindText); \
    KEY(String,   Editor.ReplaceText); \
    KEY(Bool,     Editor.FindMatchCase); \
    KEY(Bool,     Editor.FindWholeWord); \
    KEY(Bool,     Editor.SingleEditor); \
    KEY(Integer,  Editor.MaxEditors); \
    KEY(Integer,  Editor.EarlyClose); \
  ); \
  BLOCK("Interface\\QueueView", CANCREATE, \
    KEY(Integer,  QueueView.Height); \
    KEY(String,   QueueView.Layout); \
    KEY(Integer,  QueueView.Show); \
    KEY(Integer,  QueueView.LastHideShow); \
    KEY(Bool,     QueueView.ToolBar); \
  ); \
  BLOCK("Interface\\Updates", CANCREATE, \
    KEY(Integer,  FUpdates.Period); \
    KEY(DateTime, FUpdates.LastCheck); \
    KEY(Integer,  FUpdates.HaveResults); \
    KEY(Integer,  FUpdates.ShownResults); \
    KEY(String,   FUpdates.ProxyHost); \
    KEY(Integer,  FUpdates.ProxyPort); \
    KEY(Integer,  FUpdates.Results.ForVersion); \
    KEY(Integer,  FUpdates.Results.Version); \
    KEY(String,   FUpdates.Results.Message); \
    KEY(Integer,  FUpdates.Results.Critical); \
  ); \
  BLOCK("Interface\\Explorer", CANCREATE, \
    KEY(String,  ScpExplorer.ToolbarsLayout); \
    KEY(String,  ScpExplorer.DirViewParams); \
    KEY(String,  ScpExplorer.LastLocalTargetDirectory); \
    KEY(Bool,    ScpExplorer.StatusBar); \
    KEY(String,  ScpExplorer.WindowParams); \
    KEY(Integer, ScpExplorer.ViewStyle); \
    KEY(Bool,    ScpExplorer.ShowFullAddress); \
    KEY(Bool,    ScpExplorer.DriveView); \
    KEY(Integer, ScpExplorer.DriveViewWidth); \
    KEY(Integer, ScpExplorer.SessionComboWidth); \
  ); \
  BLOCK("Interface\\Commander", CANCREATE, \
    KEY(String,  ScpCommander.ToolbarsLayout); \
    KEY(Integer, ScpCommander.CurrentPanel); \
    KEY(Float,   ScpCommander.LocalPanelWidth); \
    KEY(Bool,    ScpCommander.SwappedPanels); \
    KEY(Bool,    ScpCommander.StatusBar); \
    KEY(Bool,    ScpCommander.CommandLine); \
    KEY(String,  ScpCommander.WindowParams); \
    KEYEX(Integer, ScpCommander.NortonLikeMode, ExplorerStyleSelection); \
    KEY(Bool,    ScpCommander.PreserveLocalDirectory); \
    KEY(Bool,    ScpCommander.CompareByTime); \
    KEY(Bool,    ScpCommander.CompareBySize); \
    KEY(Integer, ScpCommander.SessionComboWidth); \
    KEY(Bool,    ScpCommander.FullRowSelect); \
  ); \
  BLOCK("Interface\\Commander\\LocalPanel", CANCREATE, \
    KEY(String,  ScpCommander.LocalPanel.DirViewParams); \
    KEY(Bool,    ScpCommander.LocalPanel.StatusBar); \
    KEY(Bool,    ScpCommander.LocalPanel.DriveView); \
    KEY(Integer, ScpCommander.LocalPanel.DriveViewHeight); \
  ); \
  BLOCK("Interface\\Commander\\RemotePanel", CANCREATE, \
    KEY(String,  ScpCommander.RemotePanel.DirViewParams); \
    KEY(Bool,    ScpCommander.RemotePanel.StatusBar); \
    KEY(Bool,    ScpCommander.RemotePanel.DriveView); \
    KEY(Integer, ScpCommander.RemotePanel.DriveViewHeight); \
  ); \
  BLOCK("Logging", CANCREATE, \
    KEY(Bool,    LogWindowOnStartup); \
    KEY(String,  LogWindowParams); \
  );
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  TCustomWinConfiguration::SaveSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) Storage->Write ## TYPE(LASTELEM(AnsiString(#NAME)), VAR)
  REGCONFIG(true);
  #undef KEYEX

  if (Storage->OpenSubKey("Bookmarks", true))
  {
    FBookmarks->Save(Storage);

    Storage->CloseSubKey();
  }
  if (FCustomCommandsModified)
  {
    if (Storage->OpenSubKey("CustomCommands", true))
    {
      Storage->WriteValues(FCustomCommands, true);
      Storage->CloseSubKey();
    }
    if (Storage->OpenSubKey("CustomCommandsParams", true))
    {
      Storage->ClearValues();
      for (int Index = 0; Index < FCustomCommands->Count; Index++)
      {
        Storage->WriteInteger(FCustomCommands->Names[Index],
          FCustomCommands->Params[FCustomCommands->Names[Index]]);
      }
      Storage->CloseSubKey();
    }
    FCustomCommandsModified = false;
  }

  if (FEditorList->Modified &&
      Storage->OpenSubKey("Interface\\Editor", true))
  try
  {
    FEditorList->Save(Storage);
  }
  __finally
  {
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Load()
{
  FLegacyEditor = new TEditorPreferences();
  try
  {
    FLegacyEditor->LegacyDefaults();

    TCustomWinConfiguration::Load();

    int EditorCount = FEditorList->Count;
    if (EditorCount == 0)
    {
      TEditorPreferences * AlternativeEditor = NULL;
      try
      {
        if (FLegacyEditor->Data.Editor == edInternal)
        {
          if (!FLegacyEditor->Data.ExternalEditor.IsEmpty())
          {
            AlternativeEditor = new TEditorPreferences(*FLegacyEditor);
            AlternativeEditor->Data.Editor = edExternal;
            FLegacyEditor->Data.ExternalEditor = "";
          }
        }
        else
        {
          if (FLegacyEditor->Data.ExternalEditor.IsEmpty())
          {
            FLegacyEditor->Data.Editor = edInternal;
          }
          else
          {
            AlternativeEditor = new TEditorPreferences(*FLegacyEditor);
            AlternativeEditor->Data.Editor = edInternal;
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
  }
  __finally
  {
    delete FLegacyEditor;
    FLegacyEditor = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadSpecial(THierarchicalStorage * Storage)
{
  TCustomWinConfiguration::LoadSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#NAME)), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEYEX

  if (Storage->OpenSubKey("Bookmarks", false))
  {
    FBookmarks->Load(Storage);
    Storage->CloseSubKey();
  }

  if (Storage->OpenSubKey("CustomCommands", false))
  {
    FCustomCommands->Clear();
    Storage->ReadValues(FCustomCommands, true);
    Storage->CloseSubKey();

    if (Storage->OpenSubKey("CustomCommandsParams", false))
    {
      for (int Index = 0; Index < FCustomCommands->Count; Index++)
      {
        AnsiString Name = FCustomCommands->Names[Index];
        FCustomCommands->Params[Name] =
          Storage->ReadInteger(Name, FCustomCommands->Params[Name]);
      }
      Storage->CloseSubKey();
    }
    FCustomCommandsDefaults = false;
  }
  else if (FCustomCommandsModified)
  {
    FCustomCommands->Clear();
    FCustomCommandsDefaults = false;
  }
  FCustomCommandsModified = false;

  if (Storage->OpenSubKey("Interface\\Editor", false))
  try
  {
    FEditorList->Clear();
    FEditorList->Load(Storage);
  }
  __finally
  {
    Storage->CloseSubKey();
  }

  // load legacy editor configuration
  assert(FLegacyEditor != NULL);
  if (Storage->OpenSubKey("Interface\\Editor", false))
  {
    try
    {
      FLegacyEditor->Load(Storage, true);
    }
    __finally
    {
      Storage->CloseSubKey();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  TConfiguration::LoadAdmin(Storage);
  FDisableOpenEdit = Storage->ReadBool("DisableOpenEdit", FDisableOpenEdit);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ClearTemporaryLoginData()
{
  if (!FTemporaryKeyFile.IsEmpty())
  {
    DeleteFile(FTemporaryKeyFile);
    FTemporaryKeyFile = "";
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::GetDDExtInstalled()
{
  if (FDDExtInstalled < 0)
  {
    void* DragExtRef;
    bool Result;
    Result = (CoCreateInstance(CLSID_ShellExtension, NULL,
      CLSCTX_INPROC_SERVER | CLSCTX_LOCAL_SERVER, IID_IUnknown,
      &DragExtRef) == S_OK);
    FDDExtInstalled = (Result ? 1 : 0);
  }
  return (FDDExtInstalled > 0);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLogWindowOnStartup(bool value)
{
  SET_CONFIG_PROPERTY(LogWindowOnStartup);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLogWindowParams(AnsiString value)
{
  SET_CONFIG_PROPERTY(LogWindowParams);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDAllowMove(bool value)
{
  SET_CONFIG_PROPERTY(DDAllowMove);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDAllowMoveInit(bool value)
{
  SET_CONFIG_PROPERTY(DDAllowMoveInit);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTransferConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(DDTransferConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTemporaryDirectory(AnsiString value)
{
  SET_CONFIG_PROPERTY(DDTemporaryDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDExtEnabled(bool value)
{
  SET_CONFIG_PROPERTY(DDExtEnabled);
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
  SET_CONFIG_PROPERTY(Updates);
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
void __fastcall TWinConfiguration::SetShowInaccesibleDirectories(bool value)
{
  SET_CONFIG_PROPERTY(ShowInaccesibleDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmDeleting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmDeleting);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseLocationProfiles(bool value)
{
  SET_CONFIG_PROPERTY(UseLocationProfiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmClosingSession(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmClosingSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmExitOnCompletion(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmExitOnCompletion);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetForceDeleteTempFolder(bool value)
{
  SET_CONFIG_PROPERTY(ForceDeleteTempFolder);
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
void __fastcall TWinConfiguration::SetDimmHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(DimmHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoStartSession(AnsiString value)
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
void __fastcall TWinConfiguration::SetTheme(AnsiString value)
{
  SET_CONFIG_PROPERTY(Theme);
  ConfigureInterface();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetPathInCaption(TPathInCaption value)
{
  SET_CONFIG_PROPERTY(PathInCaption);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCopyParamAutoSelectNotice(bool value)
{
  SET_CONFIG_PROPERTY(CopyParamAutoSelectNotice);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetSessionToolbarAutoShown(bool value)
{
  SET_CONFIG_PROPERTY(SessionToolbarAutoShown);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLockToolbars(bool value)
{
  SET_CONFIG_PROPERTY(LockToolbars);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoOpenInPutty(bool value)
{
  SET_CONFIG_PROPERTY(AutoOpenInPutty);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCustomCommands(TCustomCommands * value)
{
  assert(FCustomCommands);
  if (!FCustomCommands->Equals(value))
  {
    FCustomCommands->Assign(value);
    FCustomCommandsModified = true;
    FCustomCommandsDefaults = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetBookmarks(AnsiString Key,
  TBookmarkList * value)
{
  FBookmarks->Bookmarks[Key] = value;
  Changed();
}
//---------------------------------------------------------------------------
TBookmarkList * __fastcall TWinConfiguration::GetBookmarks(AnsiString Key)
{
  return FBookmarks->Bookmarks[Key];
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinConfiguration::GetDefaultKeyFile()
{
  return (!FDefaultKeyFile.IsEmpty() ? FDefaultKeyFile : FTemporaryKeyFile);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinConfiguration::TemporaryDir(bool Mask)
{
  return UniqTempDir(DDTemporaryDirectory, "scp", Mask);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TWinConfiguration::FindTemporaryFolders()
{
  TStrings * Result = new TStringList();
  try
  {
    TSearchRec SRec;
    AnsiString Mask = TemporaryDir(true);
    AnsiString Directory = ExtractFilePath(Mask);
    if (FindFirst(Mask, faDirectory, SRec) == 0)
    {
      do
      {
        if (FLAGSET(SRec.Attr, faDirectory))
        {
          Result->Add(Directory + SRec.Name);
        }
      }
      while (FindNext(SRec) == 0);
    }

    if (Result->Count == 0)
    {
      delete Result;
      Result = NULL;
    }
  }
  catch(...)
  {
    delete Result;
    throw;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CleanupTemporaryFolders(TStrings * Folders)
{
  AnsiString ErrorList;
  TStrings * F;
  if (Folders == NULL)
  {
    F = FindTemporaryFolders();
  }
  else
  {
    F = Folders;
  }

  if (F != NULL)
  {
    try
    {
      for (int i = 0; i < F->Count; i++)
      {
        if (!DeleteDirectory(F->Strings[i]))
        {
          if (!ErrorList.IsEmpty())
          {
            ErrorList += "\n";
          }
          ErrorList += F->Strings[i];
        }
      }
    }
    __finally
    {
      if (Folders == NULL)
      {
        delete F;
      }
    }

    if (!ErrorList.IsEmpty())
    {
      throw ExtException(LoadStr(CLEANUP_TEMP_ERROR), ErrorList);
    }
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#pragma warn -inl
//---------------------------------------------------------------------------
class TAsInheritedReader : public TReader
{
public:
  __fastcall TAsInheritedReader(TStream * Stream, int BufSize) :
    TReader(Stream, BufSize)
  {
    OnAncestorNotFound = AncestorNotFound;
  }

  virtual void __fastcall ReadPrefix(TFilerFlags & Flags, int & AChildPos)
  {
    TReader::ReadPrefix(Flags, AChildPos);
    Flags << ffInherited;
  }

  void __fastcall AncestorNotFound(TReader * Reader,
    const AnsiString ComponentName, TMetaClass * ComponentClass,
    TComponent *& Component)
  {
    assert(!Component);
    if (ComponentName.IsEmpty())
    {
      for (int Index = 0; Index < LookupRoot->ComponentCount; Index++)
      {
        Component = LookupRoot->Components[Index];
        if (Component->Name.IsEmpty())
        {
          return;
        }
      }
      Component = NULL;
    }
  }
};
//---------------------------------------------------------------------------
#pragma warn .inl
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::InternalReloadComponentRes(const AnsiString ResName,
  HANDLE HInst, TComponent * Instance)
{
  HANDLE HRsrc;
  bool Result;

  if (!HInst)
  {
    HInst = HInstance;
  }
  HRsrc = FindResource(HInst, ResName.c_str(), RT_RCDATA);
  Result = (HRsrc != 0);
  if (Result)
  {
    TResourceStream * ResStream = new TResourceStream(
      reinterpret_cast<int>(HInst), ResName, RT_RCDATA);
    try
    {
      TReader * Reader;
      Reader = new TAsInheritedReader(ResStream, 4096);

      try
      {
        /*Instance =*/ Reader->ReadRootComponent(Instance);
      }
      __finally
      {
        delete Reader;
      }
    }
    __finally
    {
      delete ResStream;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::InitComponent(TComponent * Instance,
  TClass RootAncestor, TClass ClassType)
{
  bool Result = false;
  if ((ClassType != __classid(TComponent)) && (ClassType != RootAncestor))
  {
    if (InitComponent(Instance, RootAncestor, ClassType->ClassParent()))
    {
      Result = true;
    }
    if (InternalReloadComponentRes(ClassType->ClassName(),
          reinterpret_cast<HANDLE>(FindResourceHInstance(FindClassHInstance(ClassType))),
          Instance))
    {
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
LCID __fastcall TWinConfiguration::GetLocale()
{
  if (!FLocale)
  {
    AnsiString ResourceModule = GetResourceModule(ModuleFileName().c_str());
    if (!ResourceModule.IsEmpty())
    {
      AnsiString ResourceExt = ExtractFileExt(ResourceModule).UpperCase();
      ResourceExt.Delete(1, 1);

      TLanguages * Langs = Languages();
      int Index, Count;

      Count = Langs->Count;
      Index = 0;
      while ((Index < Count) && !FLocale)
      {
        if (Langs->Ext[Index] == ResourceExt)
        {
          FLocale = Langs->LocaleID[Index];
        }
        else if (Langs->Ext[Index].SubString(1, 2) == ResourceExt)
        {
          FLocale = MAKELANGID(PRIMARYLANGID(Langs->LocaleID[Index]),
            SUBLANG_DEFAULT);
        }
        Index++;
      }
    }
  }

  return TCustomWinConfiguration::GetLocale();
}
//---------------------------------------------------------------------------
HANDLE __fastcall TWinConfiguration::LoadNewResourceModule(LCID ALocale,
  AnsiString * FileName)
{
  AnsiString FileNameStorage;
  if (FileName == NULL)
  {
    FileName = &FileNameStorage;
  }

  HANDLE Instance = TCustomWinConfiguration::LoadNewResourceModule(ALocale, FileName);
  if (Instance != NULL)
  {
    try
    {
      CheckTranslationVersion(*FileName, false);
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
void __fastcall TWinConfiguration::SetResourceModule(HANDLE Instance)
{
  TCustomWinConfiguration::SetResourceModule(Instance);

  Busy(true);
  try
  {
    int Count;
    AnsiString OrigName;
    int OrigLeft;
    int OrigTop;

    TForm * Form;
    Count = Screen->FormCount;

    for (int Index = 0; Index < Count; Index++)
    {
      Form = Screen->Forms[Index];
      SendMessage(Form->Handle, WM_LOCALE_CHANGE, 0, 1);
    }

    ConfigureInterface();

    for (int Index = 0; Index < Count; Index++)
    {
      Form = Screen->Forms[Index];
      TComponent * Component;
      for (int Index = 0; Index < Form->ComponentCount; Index++)
      {
        Component = Form->Components[Index];
        if (dynamic_cast<TFrame*>(Component))
        {
          OrigName = Component->Name;
          InitComponent(Component, __classid(TFrame), Component->ClassType());
          Component->Name = OrigName;
        }
      }

      OrigLeft = Form->Left;
      OrigTop = Form->Top;
      OrigName = Form->Name;
      InitComponent(Form, __classid(TForm), Form->ClassType());
      Form->Name = OrigName;

      Form->Position = poDesigned;
      Form->Left = OrigLeft;
      Form->Top = OrigTop;
      SendMessage(Form->Handle, WM_LOCALE_CHANGE, 1, 1);
    }

    TDataModule * DataModule;
    Count = Screen->DataModuleCount;
    for (int Index = 0; Index < Count; Index++)
    {
      DataModule = Screen->DataModules[Index];
      OrigName = DataModule->Name;
      InitComponent(DataModule, __classid(TDataModule), DataModule->ClassType());
      DataModule->Name = OrigName;
    }
  }
  __finally
  {
    Busy(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::CheckTranslationVersion(const AnsiString FileName,
  bool InternalLocaleOnError)
{
  AnsiString TranslationProductVersion = GetFileProductVersion(FileName);
  AnsiString TranslationProductName = GetFileProductName(FileName);
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
  if (InvalidDefaultTranslation)
  {
    MoreMessageDialog(FMTLOAD(INVALID_DEFAULT_TRANSLATION,
      (FInvalidDefaultTranslationMessage)), NULL, qtWarning, qaOK, HELP_NONE);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::ConfirmRemoveDefaultTranslation()
{
  bool Result =
    InvalidDefaultTranslation &&
    (MoreMessageDialog(FMTLOAD(REMOVE_DEFAULT_TRANSLATION,
      (FInvalidDefaultTranslationMessage)), NULL, qtWarning,
      qaOK | qaCancel, HELP_NONE) == qaOK);
  return Result;
}
//---------------------------------------------------------------------------
const TEditorPreferences * __fastcall TWinConfiguration::DefaultEditorForFile(
  const AnsiString FileName, bool Local)
{
  return FEditorList->Find(FileName, Local);
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
//---------------------------------------------------------------------------
int __fastcall TCustomCommands::GetParam(const AnsiString & Name)
{
  int Index = IndexOfName(Name);
  if (Index >= 0)
  {
    return int(Objects[Index]);
  }
  else
  {
    return 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommands::SetParam(const AnsiString & Name, int value)
{
  int Index = IndexOfName(Name);
  if (Index >= 0)
  {
    Objects[Index] = (TObject *)value;
  }
  else
  {
    Values[Name] = "";
    Index = IndexOfName(Name);
    assert(Index >= 0);
    Objects[Index] = (TObject *)value;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommands::Equals(TCustomCommands * Commands)
{
  bool Result = TStringList::Equals(Commands);
  if (Result)
  {
    int Index = 0;
    while ((Index < Count) && Result)
    {
      if (Objects[Index] != Commands->Objects[Index])
      {
        Result = false;
      }
      Index++;
    }
  }
  return Result;
}
