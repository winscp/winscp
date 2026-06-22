//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <CoreMain.h>

#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>

#include "CustomScpExplorer.h"
#include "TerminalManager.h"
#include "NonVisual.h"
#include "Glyphs.h"
#include "ProgParams.h"
#include "Setup.h"
#include "WinConfiguration.h"
#include "GUITools.h"
#include "Tools.h"
#include "WinApi.h"
#include <DateUtils.hpp>
#include <StrUtils.hpp>
#include <Xml.Win.msxmldom.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
UnicodeString GetFolderOrWorkspaceName(const UnicodeString & SessionName)
{
  UnicodeString FolderOrWorkspaceName = DecodeUrlChars(SessionName);
  UnicodeString Result;
  if (StoredSessions->IsFolderOrWorkspace(FolderOrWorkspaceName))
  {
    Result = FolderOrWorkspaceName;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall GetLoginData(UnicodeString SessionName, TOptions * Options,
  TObjectList * DataList, UnicodeString & DownloadFile, bool NeedSession, TForm * LinkedForm, int Flags)
{
  bool DefaultsOnly = false;

  UnicodeString FolderOrWorkspaceName = GetFolderOrWorkspaceName(SessionName);
  if (!FolderOrWorkspaceName.IsEmpty())
  {
    StoredSessions->GetFolderOrWorkspace(FolderOrWorkspaceName, DataList);
  }
  else
  {
    TSessionData * SessionData =
      StoredSessions->ParseUrl(SessionName, Options, DefaultsOnly, &DownloadFile, NULL, NULL, Flags);
    DataList->Add(SessionData);

    if (DataList->Count == 1)
    {
      TSessionData * SessionData = DebugNotNull(dynamic_cast<TSessionData *>(DataList->Items[0]));
      if (SessionData->SaveOnly)
      {
        Configuration->Usage->Inc(L"CommandLineSessionSave");
        TSessionData * SavedSession = DoSaveSession(SessionData, NULL, true, NULL);
        if (SavedSession == NULL)
        {
          Abort();
        }
        WinConfiguration->LastStoredSession = SavedSession->Name;
        DataList->Clear();
      }
      else if (!SessionData->PuttyProtocol.IsEmpty())
      {
        OpenSessionInPutty(SessionData);
        DataList->Clear();
        Abort();
      }
    }
  }

  if (DefaultsOnly && !NeedSession)
  {
    // No URL specified on command-line and no explicit command-line parameter
    // that requires session was specified => noop
    DataList->Clear();
  }
  else if ((DataList->Count == 0) ||
      !dynamic_cast<TSessionData *>(DataList->Items[0])->CanOpen ||
      DefaultsOnly)
  {
    // Note that GetFolderOrWorkspace never returns sites that !CanOpen,
    // so we should not get here with more than one site.
    // Though we should be good, if we ever do.

    // We get here when:
    // - we need session for explicit command-line operation
    // - after we handle "save" URL.
    // - the specified session does not contain enough information to open [= not even hostname nor local browser]

    DebugAssert(DataList->Count <= 1);
    if (!DoLoginDialog(DataList, LinkedForm))
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
int GetCommandLineParseUrlFlags(TProgramParams * Params)
{
  return
    pufAllowStoredSiteWithProtocol |
    FLAGMASK(!CheckSafe(Params), pufUnsafe);
}
//---------------------------------------------------------------------------
void __fastcall Upload(TTerminal * Terminal, TStrings * FileList, int UseDefaults)
{
  UnicodeString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;

  TargetDirectory = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);

  std::unique_ptr<TSessionData> Data(Terminal->SessionData->Clone());
  Terminal->FillSessionDataForCode(Data.get());

  int Options = coDisableQueue;
  int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Upload;
  if ((UseDefaults == 0) ||
      DoCopyDialog(true, false, FileList, TargetDirectory, &CopyParam, Options,
        CopyParamAttrs, Data.get(), NULL, UseDefaults))
  {
    // Setting parameter overrides only now, otherwise the dialog would present the parametes as non-default
    CopyParam.OnceDoneOperation = odoDisconnect;
    CopyParam.IncludeFileMask.SetRoots(FileList, TargetDirectory);

    Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, 0, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall Download(TTerminal * Terminal, const UnicodeString FileName, int UseDefaults, bool & Browse, UnicodeString & BrowseFile)
{
  TRemoteFile * File = NULL;

  try
  {
    Terminal->ExceptionOnFail = true;
    try
    {
      File = Terminal->ReadFile(FileName);
    }
    __finally
    {
      Terminal->ExceptionOnFail = false;
    }
    File->FullFileName = FileName;
    UnicodeString LocalDirectory = Terminal->SessionData->LocalDirectoryExpanded;
    if (LocalDirectory.IsEmpty())
    {
      LocalDirectory = GetPersonalFolder();
    }
    UnicodeString TargetDirectory = IncludeTrailingBackslash(LocalDirectory);

    TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;
    UnicodeString DisplayName = File->FileName;

    bool CustomDisplayName =
      !File->DisplayName.IsEmpty() &&
      (File->DisplayName != DisplayName);
    if (CustomDisplayName)
    {
      DisplayName = File->DisplayName;
    }

    UnicodeString FriendyFileName = UnixIncludeTrailingBackslash(UnixExtractFilePath(FileName)) + DisplayName;
    std::unique_ptr<TStrings> FileListFriendly(new TStringList());
    FileListFriendly->AddObject(FriendyFileName, File);

    int Options = coDisableQueue | coBrowse;
    int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Download;
    int OutputOptions = 0;
    if ((UseDefaults == 0) ||
        DoCopyDialog(false, false, FileListFriendly.get(), TargetDirectory, &CopyParam,
          Options, CopyParamAttrs, NULL, &OutputOptions, UseDefaults))
    {
      if (FLAGCLEAR(OutputOptions, cooBrowse))
      {
        // Setting parameter overrides only now, otherwise the dialog would present the parametes as non-default

        if (CustomDisplayName)
        {
          // Set only now, so that it is not redundantly displayed on the copy dialog.
          // We should escape the * and ?'s.
          CopyParam.FileMask = DisplayName;
        }

        CopyParam.OnceDoneOperation = odoDisconnect;

        std::unique_ptr<TStrings> FileList(new TStringList());
        FileList->AddObject(FileName, File);
        CopyParam.IncludeFileMask.SetRoots(TargetDirectory, FileList.get());

        Terminal->CopyToLocal(FileList.get(), TargetDirectory, &CopyParam, 0, NULL);
      }
      else
      {
        UnicodeString Directory = UnixExtractFilePath(FileName);
        BrowseFile = UnixExtractFileName(FileName);
        Browse = true;
        Terminal->AutoReadDirectory = true;
        Terminal->ChangeDirectory(Directory);
      }
    }
    else
    {
      Abort();
    }
  }
  __finally
  {
    delete File;
  }
}
//---------------------------------------------------------------------------
void __fastcall Edit(TCustomScpExplorerForm * ScpExplorer, TStrings * FileList)
{
  ScpExplorer->StandaloneEdit(FileList->Strings[0]);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall SynchronizeDirectories(
  TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer, TStrings * CommandParams,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory)
{
  if (CommandParams->Count >= 1)
  {
    LocalDirectory = CommandParams->Strings[0];
  }
  else if (!Terminal->SessionData->LocalDirectory.IsEmpty())
  {
    LocalDirectory = Terminal->SessionData->LocalDirectoryExpanded;
  }
  else
  {
    LocalDirectory = ScpExplorer->DefaultDownloadTargetDirectory();
  }

  if (CommandParams->Count >= 2)
  {
    RemoteDirectory = CommandParams->Strings[1];
  }
  else
  {
    RemoteDirectory = Terminal->CurrentDirectory;
  }
}
//---------------------------------------------------------------------------
void __fastcall FullSynchronize(
  TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer, TStrings * CommandParams, int UseDefaults)
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;

  SynchronizeDirectories(Terminal, ScpExplorer, CommandParams, LocalDirectory, RemoteDirectory);

  bool SaveMode = true;
  // bit ugly
  TSynchronizeMode Mode = (TSynchronizeMode)GUIConfiguration->SynchronizeMode;
  int Params = GUIConfiguration->SynchronizeParams;

  // Undocumented syntax for "Start in New Window"
  if (CommandParams->Count >= 4)
  {
    Mode = (TSynchronizeMode)StrToIntDef(CommandParams->Strings[2], Mode);
    Params = StrToIntDef(CommandParams->Strings[3], Params);
  }

  int Result =
    ScpExplorer->DoFullSynchronizeDirectories(LocalDirectory, RemoteDirectory, Mode, Params, SaveMode, UseDefaults);
  if ((Result >= 0) && SaveMode)
  {
    GUIConfiguration->SynchronizeMode = Mode;
  }

  Abort();
}
//---------------------------------------------------------------------------
void __fastcall Synchronize(
  TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer, TStrings * CommandParams, int UseDefaults)
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;

  SynchronizeDirectories(Terminal, ScpExplorer, CommandParams, LocalDirectory, RemoteDirectory);

  // Undocumented syntax for "Start in New Window"
  if (CommandParams->Count >= 4)
  {
    GUIConfiguration->SynchronizeParams = StrToIntDef(CommandParams->Strings[2], -1);
    GUIConfiguration->SynchronizeOptions = StrToIntDef(CommandParams->Strings[3], -1);

    Configuration->DontSave();
  }

  ScpExplorer->DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, UseDefaults);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall ImportSitesIfAny()
{
  if (!WinConfiguration->AutoImportedFromPuttyOrFilezilla)
  {
    bool AnyPuttySession = GUIConfiguration->AnyPuttySessionForImport(StoredSessions);
    bool AnyFilezillaSession = GUIConfiguration->AnyFilezillaSessionForImport(StoredSessions);

    if (AnyPuttySession || AnyFilezillaSession)
    {
      UnicodeString PuttySource = LoadStrPart(IMPORT_SESSIONS2, 2);
      UnicodeString FilezillaSource = LoadStrPart(IMPORT_SESSIONS2, 3);
      UnicodeString Source;
      if (AnyPuttySession && AnyFilezillaSession)
      {
        Source = FORMAT(LoadStrPart(IMPORT_SESSIONS2, 4), (PuttySource, FilezillaSource));
      }
      else if (AnyPuttySession)
      {
        Source = PuttySource;
      }
      else if (AnyFilezillaSession)
      {
        Source = FilezillaSource;
      }
      else
      {
        DebugFail();
      }

      UnicodeString Message = FORMAT(LoadStrPart(IMPORT_SESSIONS2, 1), (Source));

      if (MessageDialog(Message, qtConfirmation,
            qaYes | qaNo, HELP_IMPORT_SESSIONS) == qaYes)
      {
        DoImportSessionsDialog(NULL);
      }

      WinConfiguration->AutoImportedFromPuttyOrFilezilla = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall Usage(UnicodeString Param)
{
  while (!Param.IsEmpty())
  {
    UnicodeString Pair = CutToChar(Param, L',', true);
    if (!Pair.IsEmpty())
    {
      if (Pair[Pair.Length()] == L'+')
      {
        UnicodeString Key = Pair.SubString(1, Pair.Length() - 1).Trim();
        Configuration->Usage->Inc(Key);
      }
      else if (Pair[Pair.Length()] == L'@')
      {
        UnicodeString Key = Pair.SubString(1, Pair.Length() - 1).Trim();
        UnicodeString Value;
        if (SameText(Key, L"InstallationParentProcess"))
        {
          Value = GetAncestorProcessName(3).LowerCase();
        }
        else
        {
          Value = L"err-unknown-key";
        }
        Configuration->Usage->Set(Key, Value);
      }
      else
      {
        UnicodeString Key = CutToChar(Pair, L':', true);
        UnicodeString Value = Pair.Trim();
        Configuration->Usage->Set(Key, Value);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall RecordWrapperVersions(UnicodeString ConsoleVersion, UnicodeString DotNetVersion)
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  if (!DotNetVersion.IsEmpty())
  {
    Updates.DotNetVersion = DotNetVersion;
  }
  if (!ConsoleVersion.IsEmpty())
  {
    Updates.ConsoleVersion = ConsoleVersion;
  }
  WinConfiguration->Updates = Updates;

  if ((WinConfiguration->Storage == stNul) &&
      WinConfiguration->TrySetSafeStorage())
  {
    try
    {
      THierarchicalStorage * Storage = WinConfiguration->CreateConfigStorage();
      try
      {
        Storage->AccessMode = smReadWrite;
        if (Storage->OpenSubKey(Configuration->ConfigurationSubKey, true) &&
            Storage->OpenSubKeyPath(L"Interface\\Updates", true))
        {
          if (!DotNetVersion.IsEmpty())
          {
            Storage->WriteString(L"DotNetVersion", DotNetVersion);
          }
          if (!ConsoleVersion.IsEmpty())
          {
            Storage->WriteString(L"ConsoleVersion", ConsoleVersion);
          }
        }
      }
      __finally
      {
        delete Storage;
      }
    }
    __finally
    {
      Configuration->SetNulStorage();
    }
  }
}
//---------------------------------------------------------------------------
static UnicodeString ColorToRGBStr(TColor Color)
{
  int RGB = ColorToRGB(Color);
  int R = GetRValue(RGB);
  int G = GetGValue(RGB);
  int B = GetBValue(RGB);
  UnicodeString Result = FORMAT(L"%.2x%.2x%.2x", (R, G, B));
  return Result;
}
//---------------------------------------------------------------------------
class TStartupThread : public TSimpleThread
{
public:
  TStartupThread();
  virtual __fastcall ~TStartupThread();

  int GetStartupSeconds();

  virtual void __fastcall Terminate();

protected:
  virtual void __fastcall Execute();

  int FMilliseconds;
  bool FStop;
};
//---------------------------------------------------------------------------
TStartupThread::TStartupThread()
{
  FMilliseconds = 0;
  FStop = false;
  Start();
}
//---------------------------------------------------------------------------
__fastcall TStartupThread::~TStartupThread()
{
  Close();
}
//---------------------------------------------------------------------------
int TStartupThread::GetStartupSeconds()
{
  DebugAssert(!FStop);
  return FMilliseconds / 1000;
}
//---------------------------------------------------------------------------
void __fastcall TStartupThread::Terminate()
{
  FStop = true;
}
//---------------------------------------------------------------------------
void __fastcall TStartupThread::Execute()
{
  while (!FStop)
  {
    const int Step = 250;
    Sleep(Step);
    FMilliseconds += Step;
  }
}
//---------------------------------------------------------------------------
TStartupThread * StartupThread(new TStartupThread());
TDateTime Started(Now());
TDateTime LastStartupStartupSequence(Now());
UnicodeString StartupSequence;
int LifetimeRuns = -1;
//---------------------------------------------------------------------------
void InterfaceStartDontMeasure()
{
  Started = TDateTime();
  StartupThread->Terminate();
}
//---------------------------------------------------------------------------
void AddStartupSequence(const UnicodeString & Tag)
{
  int SequenceTensOfSecond = static_cast<int>(MilliSecondsBetween(Now(), LastStartupStartupSequence) / 100);
  LastStartupStartupSequence = Now();
  AddToList(StartupSequence, FORMAT(L"%s:%d", (Tag, SequenceTensOfSecond)), L",");
}
//---------------------------------------------------------------------------
void InterfaceStarted()
{
  if ((Started != TDateTime()) && (LifetimeRuns > 0))
  {
    // deliberate downcast
    int StartupSeconds = static_cast<int>(SecondsBetween(Now(), Started));
    int StartupSecondsReal = DebugNotNull(StartupThread)->GetStartupSeconds();
    if (LifetimeRuns == 1)
    {
      Configuration->Usage->Set(L"StartupSeconds1", StartupSeconds);
    }
    else if (LifetimeRuns == 2)
    {
      Configuration->Usage->Set(L"StartupSeconds2", StartupSeconds);
    }
    Configuration->Usage->Set(L"StartupSecondsLast", StartupSeconds);
    Configuration->Usage->Set(L"StartupSecondsLastReal", StartupSecondsReal);
    AddStartupSequence(L"I");
    Configuration->Usage->Set(L"StartupSequenceLast", StartupSequence);
  }
  StartupThread->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall UpdateStaticUsage()
{
  Configuration->Usage->Inc(L"Runs");

  Configuration->Usage->UpdateCurrentVersion();

  Configuration->Usage->Set(L"WindowsVersion", (WindowsVersionLong()));
  Configuration->Usage->Set(L"WindowsProductName", (WindowsProductName()));
  Configuration->Usage->Set(L"WindowsProductType", (static_cast<int>(GetWindowsProductType())));
  Configuration->Usage->Set(L"Windows64", IsWin64());
  Configuration->Usage->Set(L"UWP", IsUWP());
  Configuration->Usage->Set(L"PackageName", GetPackageName());
  Configuration->Usage->Set(L"DefaultLocale",
    // See TGUIConfiguration::GetAppliedLocaleHex()
    IntToHex(static_cast<int>(GetDefaultLCID()), 4));
  Configuration->Usage->Set(L"Locale", WinConfiguration->AppliedLocaleHex);
  Configuration->Usage->Set(L"EncodingMultiByteAnsi", !TEncoding::Default->IsSingleByte);
  Configuration->Usage->Set(L"PixelsPerInch", Screen->PixelsPerInch);

  int PrimaryPixelsPerInch = Screen->PrimaryMonitor->PixelsPerInch;
  bool PixelsPerInchMonitorsDiffer = false;
  for (int Index = 0; Index < Screen->MonitorCount; Index++)
  {
    if (Screen->Monitors[Index]->PixelsPerInch != PrimaryPixelsPerInch)
    {
      PixelsPerInchMonitorsDiffer = true;
    }
  }

  if (PrimaryPixelsPerInch != Screen->PixelsPerInch)
  {
    Configuration->Usage->Inc(L"PixelsPerInchSystemDiffered2");
  }
  Configuration->Usage->Set(L"PixelsPerInchMonitorsDiffer", PixelsPerInchMonitorsDiffer);

  Configuration->Usage->Set(L"WorkAreaWidth", Screen->WorkAreaWidth);
  Configuration->Usage->Set(L"WorkAreaHeight", Screen->WorkAreaHeight);
  HDC DC = GetDC(NULL);
  int Planes = GetDeviceCaps(DC, PLANES);
  int BitsPixel = GetDeviceCaps(DC, BITSPIXEL);
  Configuration->Usage->Set(L"ColorDepth", Planes * BitsPixel);
  Configuration->Usage->Set(L"MonitorCount", Screen->MonitorCount);
  Configuration->Usage->Set(L"NotUseThemes", !UseThemes());
  Configuration->Usage->Set(L"ThemeDefaultFontSize", std::unique_ptr<TFont>(new TFont())->Size);
  Configuration->Usage->Set(L"ThemeIconFontSize", Screen->IconFont->Size);

  Configuration->Usage->Set(L"SysColorWindow", ColorToRGBStr(clWindow));
  Configuration->Usage->Set(L"SysColorBtnFace", ColorToRGBStr(clBtnFace));
  Configuration->Usage->Set(L"SysColorWindowText", ColorToRGBStr(clWindowText));

  UnicodeString ProgramsFolder;
  ::SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  ProgramsFolder = IncludeTrailingBackslash(ExpandFileName(ProgramsFolder));
  UnicodeString ExeName = ExpandFileName(Application->ExeName);
  bool InProgramFiles = AnsiSameText(ExeName.SubString(1, ProgramsFolder.Length()), ProgramsFolder);
  Configuration->Usage->Set(L"InProgramFiles", InProgramFiles);
  Configuration->Usage->Set(L"IsInstalled", IsInstalled());
  Configuration->Usage->Set(L"IsInstalledMsi", IsInstalledMsi());
  Configuration->Usage->Set(L"Wine", IsWine());
  Configuration->Usage->Set(L"NetFrameworkVersion", GetNetVersionStr());
  Configuration->Usage->Set(L"NetCoreVersion", GetNetCoreVersionStr());
  Configuration->Usage->Set(L"PowerShellVersion", GetPowerShellVersionStr());
  Configuration->Usage->Set(L"PwshVersion", GetPowerShellCoreVersionStr());

  bool MsXmlInstalled;
  try
  {
    TMSXMLDOMDocumentFactory::CreateDOMDocument();
    MsXmlInstalled = true;
  }
  catch (...)
  {
    MsXmlInstalled = false;
  }
  Configuration->Usage->Set(L"MsXmlInstalled", MsXmlInstalled);

  UnicodeString ParentProcess = GetAncestorProcessName();
  // do not record the installer as a parent process
  if (!ParentProcess.IsEmpty() &&
      (!StartsText(L"winscp-", ParentProcess) || !ContainsText(ParentProcess, L"-setup")))
  {
    UnicodeString ParentProcesses = Configuration->Usage->Get(L"ParentProcesses");
    std::unique_ptr<TStringList> ParentProcessesList(CreateSortedStringList());
    ParentProcessesList->CommaText = ParentProcesses;
    ParentProcessesList->Add(ParentProcess.LowerCase());
    Configuration->Usage->Set(L"ParentProcesses", ParentProcessesList->CommaText);
  }

  WinConfiguration->UpdateStaticUsage();

}
//---------------------------------------------------------------------------
void __fastcall UpdateFinalStaticUsage()
{
  CoreUpdateFinalStaticUsage();
}
//---------------------------------------------------------------------------
void __fastcall MaintenanceTask()
{
  CoreMaintenanceTask();
  InterfaceStartDontMeasure();
}
//---------------------------------------------------------------------------
typedef std::vector<HWND> THandles;
typedef std::map<unsigned long, THandles> TProcesses;
//---------------------------------------------------------------------------
BOOL __stdcall EnumOtherInstances(HWND Handle, LPARAM AParam)
{
  TProcesses & Processes = *reinterpret_cast<TProcesses *>(AParam);

  // This should be optimized to query class name already here
  unsigned long ProcessId;
  if (GetWindowThreadProcessId(Handle, &ProcessId) != 0)
  {
    Processes[ProcessId].push_back(Handle);
  }

  return TRUE;
}
//---------------------------------------------------------------------------
static bool __fastcall SendCopyDataMessage(HWND Window, TCopyDataMessage & Message)
{
  COPYDATASTRUCT CopyData;
  CopyData.cbData = sizeof(Message);
  CopyData.lpData = &Message;

  LRESULT SendResult =
    SendMessage(Window, WM_COPYDATA,
       reinterpret_cast<WPARAM>(HInstance), reinterpret_cast<LPARAM>(&CopyData));
  bool Result = (SendResult > 0);
  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall FindOtherInstances(THandles & OtherInstances)
{
  TProcesses Processes;

  // FindWindow is optimization (if there's no hidden window, no point enumerating all windows to find some)
  if ((FindWindow(HIDDEN_WINDOW_NAME, NULL) != NULL) &&
      EnumWindows(EnumOtherInstances, reinterpret_cast<LPARAM>(&Processes)))
  {
    TCopyDataMessage Message;

    Message.Command = TCopyDataMessage::MainWindowCheck;

    TProcesses::const_iterator ProcessI = Processes.begin();
    while (ProcessI != Processes.end())
    {
      HWND HiddenWindow = NULL;
      THandles::const_iterator WindowI = ProcessI->second.begin();

      while ((HiddenWindow == NULL) && (WindowI != ProcessI->second.end()))
      {
        wchar_t ClassName[1024];
        if (GetClassName(*WindowI, ClassName, LENOF(ClassName)) != 0)
        {
          NULL_TERMINATE(ClassName);

          if (wcscmp(ClassName, HIDDEN_WINDOW_NAME) == 0)
          {
            HiddenWindow = *WindowI;
          }
        }
        WindowI++;
      }

      if (HiddenWindow != NULL)
      {
        WindowI = ProcessI->second.begin();

        while (WindowI != ProcessI->second.end())
        {
          if (*WindowI != HiddenWindow) // optimization
          {
            if (SendCopyDataMessage(*WindowI, Message))
            {
              OtherInstances.push_back(*WindowI);
              break;
            }
          }
          WindowI++;
        }
      }

      ProcessI++;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall SendToAnotherInstance()
{
  THandles OtherInstances;
  FindOtherInstances(OtherInstances);

  bool Result = false;
  THandles::const_iterator I = OtherInstances.begin();
  while (!Result && (I != OtherInstances.end()))
  {
    HWND Handle = *I;

    TCopyDataMessage Message;
    Message.Command = TCopyDataMessage::CommandCanCommandLine;

    if (SendCopyDataMessage(Handle, Message))
    {
      // Restore window, if minimized
      ShowWindow(Handle, SW_RESTORE);
      // bring it to foreground
      SetForegroundWindow(Handle);

      Message.Command = TCopyDataMessage::CommandCommandLine;
      wcsncpy(Message.CommandLine, CmdLine, LENOF(Message.CommandLine));
      NULL_TERMINATE(Message.CommandLine);

      Result = SendCopyDataMessage(Handle, Message);
    }

    I++;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall Refresh(const UnicodeString & Session, const UnicodeString & Path)
{
  THandles OtherInstances;
  FindOtherInstances(OtherInstances);

  THandles::const_iterator I = OtherInstances.begin();
  while (I != OtherInstances.end())
  {
    HWND Handle = *I;

    TCopyDataMessage Message;
    Message.Command = TCopyDataMessage::RefreshPanel;
    wcsncpy(Message.Refresh.Session, Session.c_str(), LENOF(Message.Refresh.Session));
    NULL_TERMINATE(Message.Refresh.Session);
    wcsncpy(Message.Refresh.Path, Path.c_str(), LENOF(Message.Refresh.Path));
    NULL_TERMINATE(Message.Refresh.Path);

    SendCopyDataMessage(Handle, Message);

    I++;
  }
}
//---------------------------------------------------------------------------
bool __fastcall ShowUpdatesIfAvailable()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  int CurrentCompoundVer = Configuration->CompoundVersion;
  bool NoPopup = true;
  bool Result =
    !IsUWP() &&
    Updates.ShowOnStartup &&
    Updates.HaveValidResultsForVersion(CurrentCompoundVer) &&
    !Updates.Results.Disabled &&
    ((Updates.Results.Version > CurrentCompoundVer) || !Updates.Results.Message.IsEmpty()) &&
    !Updates.ShownResults;
  if (Result)
  {
    Configuration->Usage->Inc(L"UpdateStartup");
    Result = CheckForUpdates(true);
    if (Result)
    {
      Configuration->Usage->Inc(L"UpdateDownloadOpensStartup");
    }
    NoPopup = false;
  }
  else if (WinConfiguration->ShowTips)
  {
    int Days = DaysBetween(WinConfiguration->TipsShown, Now());
    if ((Days >= Updates.Results.TipsIntervalDays) &&
        (WinConfiguration->RunsSinceLastTip >= Updates.Results.TipsIntervalDays))
    {
      UnicodeString Tip = FirstUnshownTip();
      if (!Tip.IsEmpty())
      {
        AutoShowNewTip();
        NoPopup = false;
      }
      else
      {
        Configuration->Usage->Inc(L"TipsNoUnseen");
      }
    }
  }

  if (NoPopup)
  {
    WinConfiguration->RunsSinceLastTip = WinConfiguration->RunsSinceLastTip + 1;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall Execute()
{
  std::unique_ptr<TStartupThread> StartupThreadOwner(StartupThread);
  AddStartupSequence(L"X");
  DebugAssert(StoredSessions);
  TProgramParams * Params = TProgramParams::Instance();
  DebugAssert(Params);

  // do not flash message boxes on startup
  SetOnForeground(true);

  // let installer know, that some instance of application is running
  CreateMutex(NULL, False, AppName.c_str());
  bool OnlyInstance = (GetLastError() == 0);

  UpdateStaticUsage();

  UnicodeString KeyFile;
  if (Params->FindSwitch(PRIVATEKEY_SWITCH, KeyFile))
  {
    WinConfiguration->DefaultKeyFile = KeyFile;
  }

  UnicodeString ConsoleVersion;
  UnicodeString DotNetVersion;
  Params->FindSwitch(L"Console", ConsoleVersion);
  Params->FindSwitch(L"DotNet", DotNetVersion);
  if (!ConsoleVersion.IsEmpty() || !DotNetVersion.IsEmpty())
  {
    RecordWrapperVersions(ConsoleVersion, DotNetVersion);
  }
  if (!DotNetVersion.IsEmpty())
  {
    Configuration->Usage->Inc(L"ConsoleDotNet");
  }

  UnicodeString SwitchValue;
  if (Params->FindSwitch(L"loglevel", SwitchValue))
  {
    int StarPos = SwitchValue.Pos(L"*");
    if (StarPos > 0)
    {
      bool LogSensitive = true;
      SwitchValue.Delete(StarPos, 1);

      if ((StarPos <= SwitchValue.Length()) &&
          (SwitchValue[StarPos] == L'-'))
      {
        LogSensitive = false;
        SwitchValue.Delete(StarPos, 1);
      }

      SwitchValue = SwitchValue.Trim();

      Configuration->TemporaryLogSensitive(LogSensitive);
    }
    int LogProtocol;
    if (!SwitchValue.IsEmpty() && TryStrToInt(SwitchValue, LogProtocol) && (LogProtocol >= -1))
    {
      Configuration->TemporaryLogProtocol(LogProtocol);
    }
  }

  if (Params->FindSwitch(LOGSIZE_SWITCH, SwitchValue))
  {
    int StarPos = SwitchValue.Pos(LOGSIZE_SEPARATOR);
    int LogMaxCount = 0;
    if (StarPos > 1)
    {
      if (!TryStrToInt(SwitchValue.SubString(1, StarPos - 1), LogMaxCount))
      {
        LogMaxCount = -1;
      }
      SwitchValue.Delete(1, StarPos);
      SwitchValue = SwitchValue.Trim();
    }

    __int64 LogMaxSize;
    if ((LogMaxCount >= 0) &&
        !SwitchValue.IsEmpty() &&
        TryStrToSize(SwitchValue, LogMaxSize))
    {
      Configuration->TemporaryLogMaxCount(LogMaxCount);
      Configuration->TemporaryLogMaxSize(LogMaxSize);
    }
  }

  std::unique_ptr<TStrings> RawSettings(new TStringList());
  if (Params->FindSwitch(RAWTRANSFERSETTINGS_SWITCH, RawSettings.get()))
  {
    std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(RawSettings.get(), false));
    GUIConfiguration->LoadDefaultCopyParam(OptionsStorage.get());
  }

  TConsoleMode Mode = cmNone;
  if (Params->FindSwitch(L"help") || Params->FindSwitch(L"h") || Params->FindSwitch(L"?"))
  {
    Mode = cmHelp;
  }
  else if (Params->FindSwitch(L"batchsettings"))
  {
    Mode = cmBatchSettings;
  }
  else if (Params->FindSwitch(KEYGEN_SWITCH))
  {
    Mode = cmKeyGen;
  }
  else if (Params->FindSwitch(COPYID_SWITCH))
  {
    Mode = cmCopyId;
  }
  else if (Params->FindSwitch(FINGERPRINTSCAN_SWITCH))
  {
    Mode = cmFingerprintScan;
  }
  else if (Params->FindSwitch(DUMPCALLSTACK_SWITCH))
  {
    Mode = cmDumpCallstack;
  }
  else if (Params->FindSwitch(INFO_SWITCH))
  {
    Mode = cmInfo;
  }
  else if (Params->FindSwitch(COMREGISTRATION_SWITCH))
  {
    Mode = cmComRegistration;
  }
  // We have to check for /console only after the other options,
  // as the /console is always used when we are run by winscp.com
  // (ambiguous use to pass console version)
  else if (Params->FindSwitch(L"Console") || Params->FindSwitch(SCRIPT_SWITCH) ||
      Params->FindSwitch(COMMAND_SWITCH))
  {
    Mode = cmScripting;
  }

  if (Mode != cmNone)
  {
    InterfaceStartDontMeasure();
    return Console(Mode);
  }

  TTerminalManager * TerminalManager = NULL;
  GlyphsModule = NULL;
  NonVisualDataModule = NULL;
  TStrings * CommandParams = new TStringList;
  AddStartupSequence(L"C");
  try
  {
    TerminalManager = TTerminalManager::Instance();
    HANDLE ResourceModule = GUIConfiguration->ChangeToDefaultResourceModule();
    try
    {
      GlyphsModule = new TGlyphsModule(Application);
    }
    __finally
    {
      GUIConfiguration->ChangeResourceModule(ResourceModule);
    }
    AddStartupSequence(L"G");
    NonVisualDataModule = new TNonVisualDataModule(Application);
    AddStartupSequence(L"N");

    // The default is 2.5s.
    // 20s is used by Office 2010 and Windows 10 Explorer.
    // Some applications use an infinite (Thunderbird, Firefox).
    // Overriden for some controls using THintInfo.HideTimeout
    Application->HintHidePause = 20000;
    HintWindowClass = __classid(TScreenTipHintWindow);

    UnicodeString IniFileName = Params->SwitchValue(INI_SWITCH);
    if (!IniFileName.IsEmpty() && (IniFileName != INI_NUL))
    {
      UnicodeString IniFileNameExpanded = ExpandEnvironmentVariables(IniFileName);
      if (!FileExists(ApiPath(IniFileNameExpanded)))
      {
        // this should be displayed rather at the very beginning.
        // however for simplicity (GUI-only), we do it only here.
        MessageDialog(FMTLOAD(FILE_NOT_EXISTS, (IniFileNameExpanded)), qtError, qaOK);
      }
    }

    if (Params->FindSwitch(L"UninstallCleanup"))
    {
      MaintenanceTask();
      Configuration->DontSave();
      // The innosetup cannot skip UninstallCleanup run task for silent uninstalls,
      // workaround is that we create mutex in uninstaller, if it runs silent, and
      // ignore the UninstallCleanup, when the mutex exists.
      if (OpenMutex(SYNCHRONIZE, false, L"WinSCPSilentUninstall") == NULL)
      {
        DoCleanupDialogIfAnyDataAndWanted();
      }
    }
    else if (Params->FindSwitch(L"RegisterForDefaultProtocols") ||
             Params->FindSwitch(L"RegisterAsUrlHandler")) // BACKWARD COMPATIBILITY
    {
      MaintenanceTask();
      if (CheckSafe(Params))
      {
        RegisterForDefaultProtocols();
        Configuration->DontSave();
      }
    }
    else if (Params->FindSwitch(L"UnregisterForProtocols"))
    {
      MaintenanceTask();
      if (CheckSafe(Params))
      {
        UnregisterForProtocols();
        Configuration->DontSave();
      }
    }
    else if (Params->FindSwitch(L"AddSearchPath"))
    {
      MaintenanceTask();
      if (CheckSafe(Params))
      {
        AddSearchPath(ExtractFilePath(Application->ExeName));
        Configuration->DontSave();
      }
    }
    else if (Params->FindSwitch(L"RemoveSearchPath"))
    {
      MaintenanceTask();
      if (CheckSafe(Params))
      {
        try
        {
          RemoveSearchPath(ExtractFilePath(Application->ExeName));
        }
        catch(...)
        {
          // ignore errors
          // (RemoveSearchPath is called always on uninstallation,
          // even if AddSearchPath was not used, so we would get the error
          // always for non-priviledged user)
        }
        Configuration->DontSave();
      }
    }
    else if (Params->FindSwitch(L"ImportSitesIfAny"))
    {
      MaintenanceTask();
      ImportSitesIfAny();
    }
    else if (Params->FindSwitch(L"Usage", SwitchValue))
    {
      MaintenanceTask();
      Usage(SwitchValue);
    }
    else if (Params->FindSwitch(L"Update"))
    {
      MaintenanceTask();
      CheckForUpdates(false);
    }
    else if (ShowUpdatesIfAvailable())
    {
      // noop
    }
    else if (Params->FindSwitch(L"Exit"))
    {
      // noop
      MaintenanceTask();
      Configuration->DontSave();
    }
    else if (Params->FindSwitch(L"MaintenanceTask"))
    {
      // Parameter /MaintenanceTask can be added to command-line when executing maintenance tasks
      // (e.g. from installer) just in case old version of WinSCP is called by mistake
      MaintenanceTask();
      Configuration->DontSave();
    }
    else
    {
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize, pcEdit, pcRefresh } ParamCommand;
      ParamCommand = pcNone;
      UnicodeString AutoStartSession;
      UnicodeString DownloadFile;
      int UseDefaults = -1;

      // do not check for temp dirs for service tasks (like RegisterAsUrlHandler)
      if (OnlyInstance &&
          WinConfiguration->TemporaryDirectoryCleanup)
      {
        TemporaryDirectoryCleanup();
      }

      WinConfiguration->CheckDefaultTranslation();
      // Loading shell image lists here (rather than only on demand when file controls are being created)
      // reduces risk of an occasional crash.
      // It seems that the point is to load the lists before any call to SHGetFileInfoWithTimeout.
      InitFileControls();

      if (!Params->Empty)
      {
        UnicodeString Value;
        if (Params->FindSwitch(DEFAULTS_SWITCH, Value) && CheckSafe(Params))
        {
          UseDefaults = StrToIntDef(Value, 0);
        }

        if (Params->FindSwitch(UPLOAD_SWITCH, CommandParams))
        {
          ParamCommand = pcUpload;
          if (CommandParams->Count == 0)
          {
            throw Exception(NO_UPLOAD_LIST_ERROR);
          }
        }
        if (Params->FindSwitch(UPLOAD_IF_ANY_SWITCH, CommandParams))
        {
          if (CommandParams->Count > 0)
          {
            ParamCommand = pcUpload;
          }
        }
        else if (Params->FindSwitch(SYNCHRONIZE_SWITCH, CommandParams, 4))
        {
          ParamCommand = pcFullSynchronize;
        }
        else if (Params->FindSwitch(KEEP_UP_TO_DATE_SWITCH, CommandParams, 4))
        {
          ParamCommand = pcSynchronize;
        }
        else if (Params->FindSwitch(L"Edit", CommandParams, 1) &&
                 (CommandParams->Count == 1))
        {
          ParamCommand = pcEdit;
        }
        else if (Params->FindSwitch(REFRESH_SWITCH, CommandParams, 1))
        {
          ParamCommand = pcRefresh;
        }
      }

      bool NewInstance = Params->FindSwitch(NEWINSTANCE_SWICH);
      if (Params->ParamCount > 0)
      {
        AutoStartSession = Params->ConsumeParam();

        bool TrySendToAnotherInstance =
          (ParamCommand == pcNone) &&
          (WinConfiguration->ExternalSessionInExistingInstance != OpenInNewWindow()) &&
          !NewInstance &&
          // With /rawconfig before session url, parsing commandline does not work correctly,
          // when opening session in the other instance.
          // And as it is not clear what it should do anyway, let's ban it and
          // never send to the existing instance, whenever /rawconfig is used.
          !Params->FindSwitch(RAW_CONFIG_SWITCH);

        if (TrySendToAnotherInstance &&
            !AutoStartSession.IsEmpty() &&
            (AutoStartSession.Pos(L"/") > 0) && // optimization
            GetFolderOrWorkspaceName(AutoStartSession).IsEmpty())
        {
          bool DummyDefaultsOnly = false;
          UnicodeString DownloadFile2;
          int Flags = GetCommandLineParseUrlFlags(Params) | pufParseOnly;
          // Make copy, as ParseUrl consumes /rawsettings
          TOptions Options(*Params);
          std::unique_ptr<TSessionData> SessionData(
            StoredSessions->ParseUrl(AutoStartSession, &Options, DummyDefaultsOnly, &DownloadFile2, NULL, NULL, Flags));
          if (!DownloadFile2.IsEmpty())
          {
            TrySendToAnotherInstance = false;
          }
        }

        if (TrySendToAnotherInstance &&
            SendToAnotherInstance())
        {
          Configuration->Usage->Inc(L"SendToAnotherInstance");
          return 0;
        }
        UnicodeString CounterName;
        if (Params->FindSwitch(JUMPLIST_SWITCH))
        {
          CounterName = L"CommandLineJumpList";
        }
        else if (Params->FindSwitch(DESKTOP_SWITCH))
        {
          CounterName = L"CommandLineDesktop";
        }
        else if (Params->FindSwitch(SEND_TO_HOOK_SWITCH))
        {
          CounterName = L"CommandLineSendToHook";
        }
        else
        {
          CounterName = L"CommandLineSession2";
        }
        Configuration->Usage->Inc(CounterName);
      }
      else if (NewInstance)
      {
        // no autostart
      }
      else if (WinConfiguration->EmbeddedSessions && StoredSessions->Count)
      {
        AutoStartSession = StoredSessions->Sessions[0]->Name;
      }
      else
      {
        AutoStartSession = WinConfiguration->AutoStartSession;
      }

      if (ParamCommand == pcRefresh)
      {
        Refresh(AutoStartSession, (CommandParams->Count > 0 ? CommandParams->Strings[0] : UnicodeString()));
        return 0;
      }

      // from now flash message boxes in background
      SetOnForeground(false);

      bool NeedSession = NewInstance || (ParamCommand != pcNone);

      bool Retry;
      do
      {
        Retry = false;
        std::unique_ptr<TObjectList> DataList(new TObjectList());
        try
        {
          int Flags = GetCommandLineParseUrlFlags(Params);
          AddStartupSequence(L"B");
          GetLoginData(AutoStartSession, Params, DataList.get(), DownloadFile, NeedSession, NULL, Flags);
          // GetLoginData now Aborts when session is needed and none is selected
          if (DebugAlwaysTrue(!NeedSession || (DataList->Count > 0)))
          {
            if (CheckSafe(Params))
            {
              UnicodeString LogFile;
              if (Params->FindSwitch(LOG_SWITCH, LogFile))
              {
                Configuration->TemporaryLogging(LogFile);
              }
              if (Params->FindSwitch(L"XmlLog", LogFile))
              {
                Configuration->TemporaryActionsLogging(LogFile);
              }
            }

            try
            {
              DebugAssert(TerminalManager->ActiveSession == NULL);

              bool CanStart;
              bool Browse = false;
              UnicodeString BrowseFile;
              if (DataList->Count > 0)
              {
                TManagedTerminal * Session = TerminalManager->NewSessions(DataList.get());
                if (Params->FindSwitch(BROWSE_SWITCH, BrowseFile) &&
                    (!BrowseFile.IsEmpty() || !DownloadFile.IsEmpty()))
                {
                  if (BrowseFile.IsEmpty())
                  {
                    BrowseFile = DownloadFile;
                  }
                  DownloadFile = UnicodeString();
                  Browse = true;
                }
                if (!DownloadFile.IsEmpty())
                {
                  Session->AutoReadDirectory = false;
                  DownloadFile = UnixIncludeTrailingBackslash(Session->SessionData->RemoteDirectory) + DownloadFile;
                  Session->SessionData->RemoteDirectory = L"";
                  Session->StateData->RemoteDirectory = Session->SessionData->RemoteDirectory;
                }
                TerminalManager->ActiveSession = Session;
                CanStart = (TerminalManager->Count > 0);
              }
              else
              {
                DebugAssert(!NeedSession);
                CanStart = true;
              }

              if (!CanStart)
              {
                // do not prompt with login dialog, if connection of
                // auto-start session (typically from command line) failed
                if (AutoStartSession.IsEmpty())
                {
                  Retry = true;
                }
              }
              else
              {
                // from now on, we do not support runtime interface change
                CustomWinConfiguration->CanApplyInterfaceImmediately = false;
                AddStartupSequence(L"A");
                TCustomScpExplorerForm * ScpExplorer = CreateScpExplorer();
                AddStartupSequence(L"E");
                CustomWinConfiguration->AppliedInterface = CustomWinConfiguration->Interface;
                try
                {
                  // moved inside try .. __finally, because it can fail as well
                  TerminalManager->ScpExplorer = ScpExplorer;

                  if ((ParamCommand != pcNone) || !DownloadFile.IsEmpty())
                  {
                    Configuration->Usage->Inc(L"CommandLineOperation");
                    ScpExplorer->StandaloneOperation = true;
                    InterfaceStartDontMeasure();
                  }

                  if (ParamCommand == pcUpload)
                  {
                    Upload(TerminalManager->ActiveSession, CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcFullSynchronize)
                  {
                    FullSynchronize(TerminalManager->ActiveSession, ScpExplorer,
                      CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcSynchronize)
                  {
                    Synchronize(TerminalManager->ActiveSession, ScpExplorer,
                      CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcEdit)
                  {
                    Edit(ScpExplorer, CommandParams);
                  }
                  else if (!DownloadFile.IsEmpty())
                  {
                    Download(
                      TerminalManager->ActiveSession, DownloadFile, UseDefaults, Browse, BrowseFile);
                  }
                  else
                  {
                    if (DataList->Count == 0)
                    {
                      LifetimeRuns = Configuration->Usage->Inc(L"RunsNormal");
                    }
                  }

                  ScpExplorer->StandaloneOperation = false;

                  if (Browse)
                  {
                    ScpExplorer->BrowseFile(BrowseFile);
                  }

                  AddStartupSequence(L"R");
                  Application->Run();
                  // to allow dialog boxes show later (like from CheckConfigurationForceSave)
                  SetAppTerminated(False);
                }
                __finally
                {
                  TerminalManager->ScpExplorer = NULL;
                  SAFE_DESTROY(ScpExplorer);
                }
              }
            }
            catch (Exception &E)
            {
              ShowExtendedException(&E);
            }
          }
        }
        // Catch EAbort from Synchronize() and similar functions, so that CheckConfigurationForceSave is processed
        catch (EAbort & E)
        {
          Retry = false; // unlikely to be true, but just in case
        }
      }
      while (Retry);
    }

    // In GUI mode only
    CheckConfigurationForceSave();

    UpdateFinalStaticUsage();
  }
  __finally
  {
    delete NonVisualDataModule;
    NonVisualDataModule = NULL;
    ReleaseImagesModules();
    delete GlyphsModule;
    GlyphsModule = NULL;
    TTerminalManager::DestroyInstance();
    delete CommandParams;
  }

  return 0;
}
