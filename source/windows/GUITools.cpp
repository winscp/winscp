//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <shlobj.h>
#include <mshtmhst.h>
#include <Common.h>

#include "GUITools.h"
#include "WinConfiguration.h"
#include <TextsCore.h>
#include <TextsWin.h>
#include <CoreMain.h>
#include <SessionData.h>
#include <WinInterface.h>
#include <TbxUtils.hpp>
#include <Math.hpp>
#include <Tools.h>
#include "PngImageList.hpp"
#include <StrUtils.hpp>
#include <limits>
#include <Glyphs.h>
#include <PasTools.hpp>
#include <VCLCommon.h>
#include <WinApi.h>
#include <HistoryComboBox.hpp>
#include <vssym32.h>
#include <DateUtils.hpp>
#include <IOUtils.hpp>
#include <Queue.h>

#include "Animations96.h"
#include "Animations120.h"
#include "Animations144.h"
#include "Animations192.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool = L"pageant.exe";
extern const UnicodeString PuttygenTool = L"puttygen.exe";
//---------------------------------------------------------------------------
bool __fastcall FindFile(UnicodeString & Path)
{
  bool Result = FileExistsFix(Path);

  if (!Result)
  {
    UnicodeString ProgramFiles32 = IncludeTrailingBackslash(GetEnvironmentVariable(L"ProgramFiles"));
    UnicodeString ProgramFiles64 = IncludeTrailingBackslash(GetEnvironmentVariable(L"ProgramW6432"));
    if (!ProgramFiles32.IsEmpty() &&
        SameText(Path.SubString(1, ProgramFiles32.Length()), ProgramFiles32) &&
        !ProgramFiles64.IsEmpty())
    {
      UnicodeString Path64 =
        ProgramFiles64 + Path.SubString(ProgramFiles32.Length() + 1, Path.Length() - ProgramFiles32.Length());
      if (FileExists(ApiPath(Path64)))
      {
        Path = Path64;
        Result = true;
      }
    }
  }

  if (!Result && SameText(ExtractFileName(Path), Path))
  {
    UnicodeString Paths = GetEnvironmentVariable(L"PATH");
    if (!Paths.IsEmpty())
    {
      UnicodeString NewPath = FileSearch(Path, Paths);
      Result = !NewPath.IsEmpty();
      if (Result)
      {
        Path = NewPath;
      }
      else
      {
        // Basically the same what FileSearch does, except for FileExistsFix.
        // Once this proves working, we can ged rid of the FileSearch call.
        while (!Result && !Paths.IsEmpty())
        {
          UnicodeString P = CutToChar(Paths, L';', false);
          // Not using CombinePaths as it throws on an invalid path and PATH is not under our control
          NewPath = IncludeTrailingBackslash(P) + Path;
          Result = FileExistsFix(NewPath);
          if (Result)
          {
            Path = NewPath;
          }
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool DoesSessionExistInPutty(const UnicodeString & StorageKey)
{
  std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(Configuration->PuttySessionsKey));
  Storage->ConfigureForPutty();
  return Storage->OpenRootKey(true) && Storage->KeyExists(StorageKey);
}
//---------------------------------------------------------------------------
bool __fastcall ExportSessionToPutty(TSessionData * SessionData, bool ReuseExisting, const UnicodeString & SessionName)
{
  bool Result = true;
  std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(Configuration->PuttySessionsKey));
  Storage->AccessMode = smReadWrite;
  Storage->ConfigureForPutty();
  if (Storage->OpenRootKey(true))
  {
    Result = ReuseExisting && Storage->KeyExists(SessionData->StorageKey);
    if (Result)
    {
      AppLogFmt(L"Reusing existing PuTTY session: %s", (SessionData->StorageKey));
    }
    else
    {
      std::unique_ptr<TRegistryStorage> SourceStorage(new TRegistryStorage(Configuration->PuttySessionsKey));
      SourceStorage->ConfigureForPutty();
      if (SourceStorage->OpenSubKey(StoredSessions->DefaultSettings->Name, false) &&
          Storage->OpenSubKey(SessionName, true))
      {
        Storage->Copy(SourceStorage.get());
        Storage->CloseSubKey();
      }

      std::unique_ptr<TSessionData> ExportData(new TSessionData(L""));
      ExportData->Assign(SessionData);
      ExportData->Modified = true;
      ExportData->Name = SessionName;
      ExportData->WinTitle = SessionData->SessionName;
      ExportData->Password = L"";

      if (SessionData->FSProtocol == fsFTP)
      {
        if (GUIConfiguration->TelnetForFtpInPutty)
        {
          ExportData->PuttyProtocol = PuttyTelnetProtocol;
          ExportData->PortNumber = TelnetPortNumber;
        }
        else
        {
          ExportData->PuttyProtocol = PuttySshProtocol;
          ExportData->PortNumber = SshPortNumber;
        }
      }

      ExportData->Save(Storage.get(), true);
      AppLogFmt(L"Exported site settings to PuTTY session: %s", (SessionName));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
class TPuttyCleanupThread : public TSimpleThread
{
public:
  static void Schedule();
  static void Finalize();

protected:
  virtual void __fastcall Execute();
  virtual void __fastcall Terminate();
  bool __fastcall Finished();
  void DoSchedule();

private:
  TDateTime FTimer;
  static TPuttyCleanupThread * FInstance;
  static std::unique_ptr<TCriticalSection> FSection;
};
//---------------------------------------------------------------------------
std::unique_ptr<TCriticalSection> TPuttyCleanupThread::FSection(TraceInitPtr(new TCriticalSection()));
TPuttyCleanupThread * TPuttyCleanupThread::FInstance;
//---------------------------------------------------------------------------
void TPuttyCleanupThread::Schedule()
{
  TGuard Guard(FSection.get());
  if (FInstance == NULL)
  {
    FInstance = new TPuttyCleanupThread();
    FInstance->DoSchedule();
    FInstance->Start();
  }
  else
  {
    FInstance->DoSchedule();
  }
}
//---------------------------------------------------------------------------
void TPuttyCleanupThread::Finalize()
{
  while (true)
  {
    {
      TGuard Guard(FSection.get());
      if (FInstance == NULL)
      {
        return;
      }
    }
    Sleep(100);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPuttyCleanupThread::Execute()
{
  try
  {
    std::unique_ptr<TStrings> Sessions(new TStringList());
    bool Continue = true;

    do
    {
      {
        TGuard Guard(FSection.get());
        std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(Configuration->PuttySessionsKey));
        Storage->AccessMode = smReadWrite;
        Storage->ConfigureForPutty();

        std::unique_ptr<TStringList> Sessions2(new TStringList());
        if (Storage->OpenRootKey(true))
        {
          std::unique_ptr<TStrings> SubKeys(new TStringList());
          Storage->GetSubKeyNames(SubKeys.get());
          for (int Index = 0; Index < SubKeys->Count; Index++)
          {
            UnicodeString SessionName = SubKeys->Strings[Index];
            if (StartsStr(GUIConfiguration->PuttySession, SessionName))
            {
              Sessions2->Add(SessionName);
            }
          }

          Sessions2->Sort();
          if (!Sessions->Equals(Sessions2.get()))
          {
            // Just in case new sessions from another WinSCP instance are added, delay the cleanup
            // (to avoid having to implement some inter-process communication).
            // Both instances will attempt to do the cleanup, but that not a problem
            Sessions->Assign(Sessions2.get());
            DoSchedule();
          }
        }

        if (FTimer < Now())
        {
          for (int Index = 0; Index < Sessions->Count; Index++)
          {
            UnicodeString SessionName = Sessions->Strings[Index];
            Storage->RecursiveDeleteSubKey(SessionName);
          }

          Continue = false;
        }
      }

      if (Continue)
      {
        Sleep(1000);
      }
    }
    while (Continue);
  }
  __finally
  {
    TGuard Guard(FSection.get());
    FInstance = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPuttyCleanupThread::Terminate()
{
  DebugFail();
}
//---------------------------------------------------------------------------
bool __fastcall TPuttyCleanupThread::Finished()
{
  // self-destroy
  return TSimpleThread::Finished() || true;
}
//---------------------------------------------------------------------------
void TPuttyCleanupThread::DoSchedule()
{
  FTimer = IncSecond(Now(), 10);
}
//---------------------------------------------------------------------------
class TPuttyPasswordThread : public TSimpleThread
{
public:
  TPuttyPasswordThread(const UnicodeString & Password, const UnicodeString & PipeName);
  virtual __fastcall ~TPuttyPasswordThread();

protected:
  virtual void __fastcall Execute();
  virtual void __fastcall Terminate();
  virtual bool __fastcall Finished();

private:
  HANDLE FPipe;
  AnsiString FPassword;

  void DoSleep(int & Timeout);
};
//---------------------------------------------------------------------------
TPuttyPasswordThread::TPuttyPasswordThread(const UnicodeString & Password, const UnicodeString & PipeName)
{
  DWORD OpenMode = PIPE_ACCESS_OUTBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE;
  DWORD PipeMode = PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_NOWAIT | PIPE_REJECT_REMOTE_CLIENTS;
  DWORD BufferSize = 16 * 1024;
  FPipe = CreateNamedPipe(PipeName.c_str(), OpenMode, PipeMode, 1, BufferSize, BufferSize, NMPWAIT_USE_DEFAULT_WAIT, NULL);
  if (FPipe == INVALID_HANDLE_VALUE)
  {
    throw EOSExtException(L"Cannot create password pipe");
  }
  FPassword = AnsiString(Password);
  Start();
}
//---------------------------------------------------------------------------
__fastcall TPuttyPasswordThread::~TPuttyPasswordThread()
{
  DebugAlwaysTrue(FFinished);
  AppLog(L"Disconnecting and closing password pipe");
  DisconnectNamedPipe(FPipe);
  CloseHandle(FPipe);
}
//---------------------------------------------------------------------------
void __fastcall TPuttyPasswordThread::Terminate()
{
  // noop - the thread always self-terminates
}
//---------------------------------------------------------------------------
bool __fastcall TPuttyPasswordThread::Finished()
{
  return true;
}
//---------------------------------------------------------------------------
void TPuttyPasswordThread::DoSleep(int & Timeout)
{
  unsigned int Step = 50;
  Sleep(Step);
  Timeout -= Step;
  if (Timeout <= 0)
  {
    AppLog(L"Timeout waiting for PuTTY");
  }
}
//---------------------------------------------------------------------------
void __fastcall TPuttyPasswordThread::Execute()
{
  AppLog(L"Waiting for PuTTY to connect to password pipe");
  int Timeout = MSecsPerSec * SecsPerMin;
  while (Timeout > 0)
  {
    if (ConnectNamedPipe(FPipe, NULL))
    {
      AppLog(L"Password pipe is ready");
    }
    else
    {
      int Error = GetLastError();
      if (Error == ERROR_PIPE_CONNECTED)
      {
        AppLog(L"PuTTY has connected to password pipe");
        int Pos = 0;
        while ((Timeout > 0) && (Pos < FPassword.Length()))
        {
          DWORD Written = 0;
          if (!WriteFile(FPipe, FPassword.c_str() + Pos, FPassword.Length() - Pos, &Written, NULL))
          {
            AppLog(L"Error writting password pipe");
            Timeout = 0;
          }
          else
          {
            Pos += Written;
            if (Pos >= FPassword.Length())
            {
              FlushFileBuffers(FPipe);
              AppLog(L"Complete password was written to pipe");
              Timeout = 0;
            }
            else
            {
              AppLog(L"Part of password was written to pipe");
              DoSleep(Timeout);
            }
          }
        }
      }
      else if (Error == ERROR_PIPE_LISTENING)
      {
        DoSleep(Timeout);
      }
      else
      {
        AppLogFmt(L"Password pipe error %d", (Error));
        Timeout = 0;
      }
    }
  }
}
//---------------------------------------------------------------------------
void SplitPuttyCommand(UnicodeString & Program, UnicodeString & Params)
{
  // See also TSiteAdvancedDialog::PuttySettingsButtonClick
  UnicodeString Dir;
  SplitCommand(GUIConfiguration->PuttyPath, Program, Params, Dir);
  Program = ExpandEnvironmentVariables(Program);
  Params = ExpandEnvironmentVariables(Params);
}
//---------------------------------------------------------------------------
UnicodeString FindPuttyPath()
{
  UnicodeString Program, Params;
  SplitPuttyCommand(Program, Params);
  if (!FindFile(Program))
  {
    throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Program)));
  }
  return Program;
}
//---------------------------------------------------------------------------
unsigned int PipeCounter = 0;
//---------------------------------------------------------------------------
void OpenSessionInPutty(TSessionData * SessionData)
{
  // putty does not support resolving environment variables in session settings
  SessionData->ExpandEnvironmentVariables();
  UnicodeString Program, AParams;
  SplitPuttyCommand(Program, AParams);
  AppLogFmt(L"PuTTY program: %s", (Program));
  AppLogFmt(L"Params: %s", (AParams));
  if (FindFile(Program))
  {

    UnicodeString Password;
    if (GUIConfiguration->PuttyPassword)
    {
      // Passphrase has precendence, as it's more likely entered by user during authentication, hence more likely really needed.
      if (!SessionData->Passphrase.IsEmpty())
      {
        Password = SessionData->Passphrase;
      }
      else if (!SessionData->Password.IsEmpty())
      {
        Password = SessionData->Password;
      }
    }
    TCustomCommandData Data(SessionData, SessionData->UserName, Password);
    TLocalCustomCommand LocalCustomCommand(Data, SessionData->RemoteDirectory, SessionData->LocalDirectory);
    TWinInteractiveCustomCommand InteractiveCustomCommand(
      &LocalCustomCommand, L"PuTTY", UnicodeString());

    UnicodeString Params =
      LocalCustomCommand.Complete(InteractiveCustomCommand.Complete(AParams, false), true);
    UnicodeString PuttyParams;
    AppLogFmt(L"Expanded params: %s", (Params));

    if (!LocalCustomCommand.IsSiteCommand(AParams))
    {
      {
        bool SessionList = false;
        std::unique_ptr<THierarchicalStorage> SourceHostKeyStorage(Configuration->CreateScpStorage(SessionList));
        std::unique_ptr<THierarchicalStorage> TargetHostKeyStorage(new TRegistryStorage(Configuration->PuttyRegistryStorageKey));
        TargetHostKeyStorage->Explicit = true;
        TargetHostKeyStorage->AccessMode = smReadWrite;
        std::unique_ptr<TStoredSessionList> HostKeySessionList(new TStoredSessionList());
        HostKeySessionList->OwnsObjects = false;
        HostKeySessionList->Add(SessionData);
        int Imported = TStoredSessionList::ImportHostKeys(SourceHostKeyStorage.get(), TargetHostKeyStorage.get(), HostKeySessionList.get(), false);
        AppLogFmt(L"Imported host keys: %d", (Imported));
      }

      if (IsUWP())
      {
        bool Telnet = (SessionData->FSProtocol == fsFTP) && GUIConfiguration->TelnetForFtpInPutty;
        if (Telnet)
        {
          AddToList(PuttyParams, L"-telnet", L" ");
          // PuTTY  does not allow -pw for telnet
          Password = L"";
        }
        AddToList(PuttyParams, EscapePuttyCommandParam(SessionData->HostName), L" ");
        if (!SessionData->UserName.IsEmpty())
        {
          AddToList(PuttyParams, FORMAT(L"-l %s", (EscapePuttyCommandParam(SessionData->UserName))), L" ");
        }
        if ((SessionData->FSProtocol != fsFTP) && (SessionData->PortNumber != SshPortNumber))
        {
          AddToList(PuttyParams, FORMAT(L"-P %d", (SessionData->PortNumber)), L" ");
        }

        if (!Telnet)
        {
          UnicodeString PublicKeyFile = SessionData->ResolvePublicKeyFile();
          if (!PublicKeyFile.IsEmpty())
          {
            AddToList(PuttyParams, FORMAT(L"-i \"%s\"", (PublicKeyFile)), L" ");
          }
          AddToList(PuttyParams, (SessionData->TryAgent ? L"-agent" : L"-noagent"), L" ");
          if (SessionData->TryAgent)
          {
            AddToList(PuttyParams, (SessionData->AgentFwd ? L"-A" : L"-a"), L" ");
          }
          if (SessionData->Compression)
          {
            AddToList(PuttyParams, L"-C", L" ");
          }
          AddToList(PuttyParams, L"-2", L" ");
          if (!SessionData->LogicalHostName.IsEmpty())
          {
            AddToList(PuttyParams, FORMAT(L"-loghost \"%s\"", (SessionData->LogicalHostName)), L" ");
          }
        }

        if (SessionData->AddressFamily == afIPv4)
        {
          AddToList(PuttyParams, L"-4", L" ");
        }
        else if (SessionData->AddressFamily == afIPv6)
        {
          AddToList(PuttyParams, L"-6", L" ");
        }

        AppLogFmt(L"Using command-line instead of stored session in UWP: %s", (PuttyParams));
      }
      else
      {
        UnicodeString SessionName;

        UnicodeString PuttySession = GUIConfiguration->PuttySession;
        int Uniq = 1;
        while (DoesSessionExistInPutty(PuttySession))
        {
          Uniq++;
          PuttySession = FORMAT(L"%s (%d)", (GUIConfiguration->PuttySession, Uniq));
        }

        if (ExportSessionToPutty(SessionData, true, PuttySession))
        {
          SessionName = SessionData->SessionName;
        }
        else
        {
          SessionName = PuttySession;
          TPuttyCleanupThread::Schedule();
          if ((SessionData->FSProtocol == fsFTP) &&
              GUIConfiguration->TelnetForFtpInPutty)
          {
            // PuTTY  does not allow -pw for telnet
            Password = L"";
          }
        }

        UnicodeString LoadSwitch = L"-load";
        int P = Params.LowerCase().Pos(LoadSwitch + L" ");
        if ((P == 0) || ((P > 1) && (Params[P - 1] != L' ')))
        {
          AddToList(PuttyParams, FORMAT(L"%s %s", (LoadSwitch, EscapePuttyCommandParam(SessionName))), L" ");
        }
      }
    }

    if (!Password.IsEmpty() && !LocalCustomCommand.IsPasswordCommand(AParams))
    {
      Password = NormalizeString(Password); // if password is empty, we should quote it always
      bool UsePuttyPwFile;
      if (GUIConfiguration->UsePuttyPwFile == asAuto)
      {
        UsePuttyPwFile = false;
        if (SameText(ExtractFileName(Program), OriginalPuttyExecutable))
        {
          unsigned int Version = GetFileVersion(Program);
          if (Version != static_cast<unsigned int>(-1))
          {
            int MajorVersion = HIWORD(Version);
            int MinorVersion = LOWORD(Version);
            if (CalculateCompoundVersion(MajorVersion, MinorVersion) >= CalculateCompoundVersion(0, 77))
            {
              UsePuttyPwFile = true;
            }
          }
        }
      }
      else
      {
        UsePuttyPwFile = (GUIConfiguration->UsePuttyPwFile == asOn);
      }

      UnicodeString PasswordParam;
      if (UsePuttyPwFile)
      {
        PipeCounter++;
        UnicodeString PipeName = FORMAT(L"\\\\.\\PIPE\\WinSCPPuTTYPassword.%.8x.%.8x.%.4x", (GetCurrentProcessId(), PipeCounter, rand()));
        new TPuttyPasswordThread(Password, PipeName);
        PasswordParam = FORMAT(L"-pwfile \"%s\"", (PipeName));
      }
      else
      {
        PasswordParam = FORMAT(L"-pw %s", (EscapePuttyCommandParam(Password)));
      }
      AddToList(PuttyParams, PasswordParam, L" ");
    }

    AddToList(PuttyParams, Params, L" ");

    // PuTTY is started in its binary directory to allow relative paths in private key,
    // when opening PuTTY's own stored session.
    ExecuteShellChecked(Program, PuttyParams, true);
  }
  else
  {
    throw Exception(FMTLOAD(FILE_NOT_FOUND, (Program)));
  }
}
//---------------------------------------------------------------------------
bool __fastcall FindTool(const UnicodeString & Name, UnicodeString & Path)
{
  UnicodeString AppPath = IncludeTrailingBackslash(ExtractFilePath(Application->ExeName));
  Path = AppPath + Name;
  bool Result = true;
  if (!FileExists(ApiPath(Path)))
  {
    Path = AppPath + L"PuTTY\\" + Name;
    if (!FileExists(ApiPath(Path)))
    {
      Path = Name;
      if (!FindFile(Path))
      {
        Result = false;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall ExecuteTool(const UnicodeString & Name)
{
  UnicodeString Path;
  if (!FindTool(Name, Path))
  {
    throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Name)));
  }

  ExecuteShellChecked(Path, L"");
}
//---------------------------------------------------------------------------
TObjectList * StartCreationDirectoryMonitorsOnEachDrive(unsigned int Filter, TFileChangedEvent OnChanged)
{
  std::unique_ptr<TStrings> Drives(new TStringList());

  std::unique_ptr<TStrings> DDDrives(CommaTextToStringList(WinConfiguration->DDDrives));
  UnicodeString ExcludedDrives;
  for (int Index = 0; Index < DDDrives->Count; Index++)
  {
    UnicodeString S = Trim(DDDrives->Strings[Index]);
    if (!S.IsEmpty() && (S[1] == L'-'))
    {
      S = Trim(S.SubString(2, S.Length() - 1));
      if (!S.IsEmpty())
      {
        ExcludedDrives += S[1];
      }
    }
    else
    {
      Drives->Add(S);
    }
  }

  for (char Drive = FirstDrive; Drive <= LastDrive; Drive++)
  {
    if (ExcludedDrives.Pos(Drive) == 0)
    {
      // Not calling ReadDriveStatus(... dsSize), relying on drive ready status cached by the background thread
      TDriveInfoRec * DriveInfoRec = DriveInfo->Get(Drive);
      if (DriveInfoRec->Valid && DriveInfoRec->DriveReady &&
          (DriveInfoRec->DriveType != DRIVE_CDROM) &&
          ((DriveInfoRec->DriveType != DRIVE_REMOVABLE) || (Drive >= DriveInfo->FirstFixedDrive)))
      {
        Drives->Add(Drive);
      }
    }
  }

  std::unique_ptr<TObjectList> Result(new TObjectList());
  for (int Index = 0; Index < Drives->Count; Index++)
  {
    UnicodeString Drive = Drives->Strings[Index];
    std::unique_ptr<TDirectoryMonitor> Monitor(new TDirectoryMonitor(Application));
    try
    {
      Monitor->Path = DriveInfo->GetDriveRoot(Drive);
      Monitor->WatchSubtree = true;
      Monitor->WatchFilters = Filter;
      Monitor->OnCreated = OnChanged;
      Monitor->OnModified = OnChanged;
      Monitor->Active = true;
      Result->Add(Monitor.release());
    }
    catch (Exception & E)
    {
      // Ignore errors watching not-ready drives
    }
  }
  return Result.release();
}
//---------------------------------------------------------------------------
bool DontCopyCommandToClipboard = false;
//---------------------------------------------------------------------------
bool __fastcall CopyCommandToClipboard(const UnicodeString & Command)
{
  bool Result = !DontCopyCommandToClipboard && UseAlternativeFunction() && IsKeyPressed(VK_CONTROL);
  if (Result)
  {
    TInstantOperationVisualizer Visualizer;
    AppLogFmt(L"Copied command to the clipboard: %s", (Command));
    CopyToClipboard(Command);
  }
  return Result;
}
//---------------------------------------------------------------------------
static bool __fastcall DoExecuteShell(const UnicodeString Path, const UnicodeString Params,
  bool ChangeWorkingDirectory, HANDLE * Handle)
{
  bool Result = CopyCommandToClipboard(FormatCommand(Path, Params));

  if (Result)
  {
    if (Handle != NULL)
    {
      *Handle = NULL;
    }
  }
  else
  {
    UnicodeString Directory = ExtractFilePath(Path);

    TShellExecuteInfoW ExecuteInfo;
    memset(&ExecuteInfo, 0, sizeof(ExecuteInfo));
    ExecuteInfo.cbSize = sizeof(ExecuteInfo);
    ExecuteInfo.fMask =
      SEE_MASK_FLAG_NO_UI |
      FLAGMASK((Handle != NULL), SEE_MASK_NOCLOSEPROCESS);
    ExecuteInfo.hwnd = Application->Handle;
    ExecuteInfo.lpFile = (wchar_t*)Path.data();
    ExecuteInfo.lpParameters = (wchar_t*)Params.data();
    ExecuteInfo.lpDirectory = (ChangeWorkingDirectory ? Directory.c_str() : NULL);
    ExecuteInfo.nShow = SW_SHOW;

    AppLogFmt(L"Executing program \"%s\" with params: %s", (Path, Params));
    Result = (ShellExecuteEx(&ExecuteInfo) != 0);
    if (Result)
    {
      if (Handle != NULL)
      {
        *Handle = ExecuteInfo.hProcess;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall ExecuteShellChecked(const UnicodeString Path, const UnicodeString Params, bool ChangeWorkingDirectory)
{
  if (!DoExecuteShell(Path, Params, ChangeWorkingDirectory, NULL))
  {
    throw EOSExtException(FMTLOAD(EXECUTE_APP_ERROR, (Path)));
  }
}
//---------------------------------------------------------------------------
void __fastcall ExecuteShellChecked(const UnicodeString Command)
{
  UnicodeString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  ExecuteShellChecked(Program, Params);
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params,
  HANDLE & Handle)
{
  return DoExecuteShell(Path, Params, false, &Handle);
}
//---------------------------------------------------------------------------
void __fastcall ExecuteShellCheckedAndWait(const UnicodeString Command,
  TProcessMessagesEvent ProcessMessages)
{
  UnicodeString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  HANDLE ProcessHandle;
  bool Result = DoExecuteShell(Program, Params, false, &ProcessHandle);
  if (!Result)
  {
    throw EOSExtException(FMTLOAD(EXECUTE_APP_ERROR, (Program)));
  }
  else
  {
    if (ProcessHandle != NULL) // only if command was copied to clipboard only
    {
      if (ProcessMessages != NULL)
      {
        unsigned long WaitResult;
        do
        {
          // Same as in ExecuteProcessAndReadOutput
          WaitResult = WaitForSingleObject(ProcessHandle, 50);
          if (WaitResult == WAIT_FAILED)
          {
            throw Exception(LoadStr(DOCUMENT_WAIT_ERROR));
          }
          ProcessMessages();
        }
        while (WaitResult == WAIT_TIMEOUT);
      }
      else
      {
        WaitForSingleObject(ProcessHandle, INFINITE);
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall SpecialFolderLocation(int PathID, UnicodeString & Path)
{
  LPITEMIDLIST Pidl;
  wchar_t Buf[256];
  if (SHGetSpecialFolderLocation(NULL, PathID, &Pidl) == NO_ERROR &&
      SHGetPathFromIDList(Pidl, Buf))
  {
    Path = UnicodeString(Buf);
    return true;
  }
  return false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall UniqTempDir(const UnicodeString BaseDir, const UnicodeString Identity,
  bool Mask)
{
  DebugAssert(!BaseDir.IsEmpty());
  UnicodeString TempDir;
  do
  {
    TempDir = IncludeTrailingBackslash(BaseDir) + Identity;
    if (Mask)
    {
      TempDir += L"?????";
    }
    else
    {
      TempDir += IncludeTrailingBackslash(FormatDateTime(L"nnzzz", Now()));
    }
  }
  while (!Mask && DirectoryExists(ApiPath(TempDir)));

  return TempDir;
}
//---------------------------------------------------------------------------
class TSessionColors : public TComponent
{
public:
  __fastcall TSessionColors(TComponent * Owner) : TComponent(Owner)
  {
    Name = QualifiedClassName();
  }

  static TSessionColors * __fastcall Retrieve(TComponent * Component)
  {
    TSessionColors * SessionColors = dynamic_cast<TSessionColors *>(Component->FindComponent(QualifiedClassName()));
    if (SessionColors == NULL)
    {
      SessionColors = new TSessionColors(Component);
    }
    return SessionColors;
  }

  typedef std::map<TColor, int> TColorMap;
  TColorMap ColorMap;
};
//---------------------------------------------------------------------------
int __fastcall GetSessionColorImage(
  TCustomImageList * ImageList, TColor Color, int MaskIndex)
{

  TSessionColors * SessionColors = TSessionColors::Retrieve(ImageList);

  int Result;
  TSessionColors::TColorMap::const_iterator I = SessionColors->ColorMap.find(Color);
  if (I != SessionColors->ColorMap.end())
  {
    Result = I->second;
  }
  else
  {
    // This overly complex drawing is here to support color button on SiteAdvanced
    // dialog. There we use plain TImageList, instead of TPngImageList,
    // TButton does not work with transparent images
    // (not even TBitmap with Transparent = true)
    std::unique_ptr<TBitmap> MaskBitmap(new TBitmap());
    ImageList->GetBitmap(MaskIndex, MaskBitmap.get());

    std::unique_ptr<TPngImage> MaskImage(new TPngImage());
    MaskImage->Assign(MaskBitmap.get());

    std::unique_ptr<TPngImage> ColorImage(new TPngImage(COLOR_RGB, 16, ImageList->Width, ImageList->Height));

    TColor MaskTransparentColor = MaskImage->Pixels[0][0];
    TColor TransparentColor = MaskTransparentColor;
    // Expecting that the color to be replaced is in the centre of the image (HACK)
    TColor MaskColor = MaskImage->Pixels[ImageList->Width / 2][ImageList->Height / 2];

    for (int Y = 0; Y < ImageList->Height; Y++)
    {
      for (int X = 0; X < ImageList->Width; X++)
      {
        TColor SourceColor = MaskImage->Pixels[X][Y];
        TColor DestColor;
        // this branch is pointless as long as MaskTransparentColor and
        // TransparentColor are the same
        if (SourceColor == MaskTransparentColor)
        {
          DestColor = TransparentColor;
        }
        else if (SourceColor == MaskColor)
        {
          DestColor = Color;
        }
        else
        {
          DestColor = SourceColor;
        }
        ColorImage->Pixels[X][Y] = DestColor;
      }
    }

    std::unique_ptr<TBitmap> Bitmap(new TBitmap());
    Bitmap->SetSize(ImageList->Width, ImageList->Height);
    ColorImage->AssignTo(Bitmap.get());

    Result = ImageList->AddMasked(Bitmap.get(), TransparentColor);

    SessionColors->ColorMap.insert(std::make_pair(Color, Result));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall RegenerateSessionColorsImageList(TCustomImageList * ImageList, int MaskIndex)
{
  TSessionColors * SessionColors = TSessionColors::Retrieve(ImageList);

  std::vector<TColor> Colors;
  size_t FixedImages = static_cast<size_t>(ImageList->Count);
  Colors.resize(FixedImages + SessionColors->ColorMap.size());
  TSessionColors::TColorMap::const_iterator I = SessionColors->ColorMap.begin();
  while (I != SessionColors->ColorMap.end())
  {
    DebugAssert(Colors[I->second] == TColor());
    Colors[I->second] = I->first;
    I++;
  }

  TSessionColors::TColorMap ColorMap = SessionColors->ColorMap;
  SessionColors->ColorMap.clear();

  for (size_t Index = 0; Index < Colors.size(); Index++)
  {
    bool IsFixedImageIndex = (Index < FixedImages);
    DebugAssert((Colors[Index] == TColor()) == IsFixedImageIndex);
    if (!IsFixedImageIndex)
    {
      GetSessionColorImage(ImageList, Colors[Index], MaskIndex);
    }
  }

  DebugAssert(SessionColors->ColorMap == ColorMap);
}
//---------------------------------------------------------------------------
void __fastcall SetSubmenu(TTBXCustomItem * Item, bool Enable)
{
  class TTBXPublicItem : public TTBXCustomItem
  {
  public:
    __property ItemStyle;
  };
  TTBXPublicItem * PublicItem = reinterpret_cast<TTBXPublicItem *>(Item);
  DebugAssert(PublicItem != NULL);
  // See TTBItemViewer.IsPtInButtonPart (called from TTBItemViewer.MouseDown)
  if (Enable)
  {
    PublicItem->ItemStyle = PublicItem->ItemStyle << tbisSubmenu;
  }
  else
  {
    PublicItem->ItemStyle = PublicItem->ItemStyle >> tbisSubmenu;
  }
}
//---------------------------------------------------------------------------
bool __fastcall IsEligibleForApplyingTabs(
  UnicodeString Line, int & TabPos, UnicodeString & Start, UnicodeString & Remaining)
{
  bool Result = false;
  TabPos = Line.Pos(L"\t");
  if (TabPos > 0)
  {
    Remaining = Line.SubString(TabPos + 1, Line.Length() - TabPos);
    // WORKAROUND
    // Some translations still use obsolete hack of consecutive tabs to aling the contents.
    // Delete these, so that the following check does not fail on this
    while (Remaining.SubString(1, 1) == L"\t")
    {
      Remaining.Delete(1, 1);
    }

    // We do not have, not support, mutiple tabs on a single line
    if (DebugAlwaysTrue(Remaining.Pos(L"\t") == 0))
    {
      Start = Line.SubString(1, TabPos - 1);
      // WORKAROUND
      // Previously we padded the string before tab with spaces,
      // to aling the contents across multiple lines
      Start = Start.TrimRight();
      // at least two normal spaces for separation
      Start += L"  ";
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static int __fastcall CalculateWidthByLength(UnicodeString Text, void * /*Arg*/)
{
  return Text.Length();
}
//---------------------------------------------------------------------------
void __fastcall ApplyTabs(
  UnicodeString & Text, wchar_t Padding,
  TCalculateWidth CalculateWidth, void * CalculateWidthArg)
{
  if (CalculateWidth == NULL)
  {
    DebugAssert(CalculateWidthArg == NULL);
    CalculateWidth = CalculateWidthByLength;
  }

  std::unique_ptr<TStringList> Lines(TextToStringList(Text));

  int MaxWidth = -1;
  for (int Index = 0; Index < Lines->Count; Index++)
  {
    UnicodeString Line = Lines->Strings[Index];
    int TabPos;
    UnicodeString Start;
    UnicodeString Remaining;
    if (IsEligibleForApplyingTabs(Line, TabPos, Start, Remaining))
    {
      int Width = CalculateWidth(Start, CalculateWidthArg);
      MaxWidth = Max(MaxWidth, Width);
    }
  }

  // Optimization and also to prevent potential regression for texts without tabs
  if (MaxWidth >= 0)
  {
    for (int Index = 0; Index < Lines->Count; Index++)
    {
      UnicodeString Line = Lines->Strings[Index];
      int TabPos;
      UnicodeString Start;
      UnicodeString Remaining;
      if (IsEligibleForApplyingTabs(Line, TabPos, Start, Remaining))
      {
        int Width;
        int Iterations = 0;
        while ((Width = CalculateWidth(Start, CalculateWidthArg)) < MaxWidth)
        {
          int Wider = CalculateWidth(Start + Padding, CalculateWidthArg);
          // If padded string is wider than max width by more pixels
          // than non-padded string is shorter than max width
          if ((Wider > MaxWidth) && ((Wider - MaxWidth) > (MaxWidth - Width)))
          {
            break;
          }
          Start += Padding;
          Iterations++;
          // In rare case a tab is zero-width with some strange font (like HYLE)
          if (Iterations > 100)
          {
            break;
          }
        }
        Lines->Strings[Index] = Start + Remaining;
      }
    }

    Text = Lines->Text;
    // remove trailing newline
    Text = Text.TrimRight();
  }
}
//---------------------------------------------------------------------------
static void __fastcall DoSelectScaledImageList(TImageList * ImageList)
{
  TImageList * MatchingList = NULL;
  int MachingPixelsPerInch = 0;
  int PixelsPerInch = LargerPixelsPerInch(GetComponentPixelsPerInch(ImageList), WinConfiguration->LargerToolbar);

  for (int Index = 0; Index < ImageList->Owner->ComponentCount; Index++)
  {
    TImageList * OtherList = dynamic_cast<TImageList *>(ImageList->Owner->Components[Index]);

    if ((OtherList != NULL) &&
        (OtherList != ImageList) &&
        StartsStr(ImageList->Name, OtherList->Name))
    {
      UnicodeString OtherListPixelsPerInchStr =
        OtherList->Name.SubString(
          ImageList->Name.Length() + 1, OtherList->Name.Length() - ImageList->Name.Length());
      int OtherListPixelsPerInch = StrToInt(OtherListPixelsPerInchStr);
      if ((OtherListPixelsPerInch <= PixelsPerInch) &&
          ((MatchingList == NULL) ||
           (MachingPixelsPerInch < OtherListPixelsPerInch)))
      {
        MatchingList = OtherList;
        MachingPixelsPerInch = OtherListPixelsPerInch;
      }
    }
  }

  if (MatchingList != NULL)
  {
    UnicodeString ImageListBackupName = ImageList->Name + IntToStr(USER_DEFAULT_SCREEN_DPI);

    if (ImageList->Owner->FindComponent(ImageListBackupName) == NULL)
    {
      TImageList * ImageListBackup;
      TPngImageList * PngImageList = dynamic_cast<TPngImageList *>(ImageList);
      if (PngImageList != NULL)
      {
        ImageListBackup = new TPngImageList(ImageList->Owner);
      }
      else
      {
        ImageListBackup = new TImageList(ImageList->Owner);
      }

      ImageListBackup->Name = ImageListBackupName;
      ImageList->Owner->InsertComponent(ImageListBackup);
      CopyImageList(ImageListBackup, ImageList);
    }

    CopyImageList(ImageList, MatchingList);
  }
}
//---------------------------------------------------------------------------
static void __fastcall ImageListRescale(TComponent * Sender, TObject * /*Token*/)
{
  TImageList * ImageList = DebugNotNull(dynamic_cast<TImageList *>(Sender));
  DoSelectScaledImageList(ImageList);
}
//---------------------------------------------------------------------------
void __fastcall SelectScaledImageList(TImageList * ImageList)
{
  DoSelectScaledImageList(ImageList);

  SetRescaleFunction(ImageList, ImageListRescale);
}
//---------------------------------------------------------------------------
void __fastcall CopyImageList(TImageList * TargetList, TImageList * SourceList)
{
  // Maybe this is not necessary, once the TPngImageList::Assign was fixed.
  // But if we ever use Assign, make sure the target keeps its Scaled property.
  TPngImageList * PngTargetList = dynamic_cast<TPngImageList *>(TargetList);
  TPngImageList * PngSourceList = dynamic_cast<TPngImageList *>(SourceList);

  TargetList->Clear();
  TargetList->Height = SourceList->Height;
  TargetList->Width = SourceList->Width;

  if ((PngTargetList != NULL) && (PngSourceList != NULL))
  {
    // AddImages won't copy the names and we need them for
    // LoadDialogImage and TFrameAnimation
    PngTargetList->PngImages->Assign(PngSourceList->PngImages);
  }
  else
  {
    TargetList->AddImages(SourceList);
  }
}
//---------------------------------------------------------------------------
static bool __fastcall DoLoadDialogImage(TImage * Image, const UnicodeString & ImageName)
{
  bool Result = false;
  if (GlyphsModule != NULL)
  {
    TPngImageList * DialogImages = GetDialogImages(Image);

    int Index;
    for (Index = 0; Index < DialogImages->PngImages->Count; Index++)
    {
      TPngImageCollectionItem * PngItem = DialogImages->PngImages->Items[Index];
      if (SameText(PngItem->Name, ImageName))
      {
        Image->Picture->Assign(PngItem->PngImage);
        break;
      }
    }

    DebugAssert(Index < DialogImages->PngImages->Count);
    Result = true;
  }
  // When showing an exception from wWinMain, the images are released already.
  // Non-existence of the glyphs module is just a good indication of that.
  // We expect errors only.
  else if (ImageName == L"Error")
  {
    Image->Picture->Icon->Handle = LoadIcon(0, IDI_HAND);
  }
  // For showing an information about trace files
  else if (DebugAlwaysTrue(ImageName == L"Information"))
  {
    Image->Picture->Icon->Handle = LoadIcon(0, IDI_APPLICATION);
  }
  return Result;
}
//---------------------------------------------------------------------------
class TDialogImageName : public TObject
{
public:
  UnicodeString ImageName;
};
//---------------------------------------------------------------------------
static void __fastcall DialogImageRescale(TComponent * Sender, TObject * Token)
{
  TImage * Image = DebugNotNull(dynamic_cast<TImage *>(Sender));
  TDialogImageName * DialogImageName = DebugNotNull(dynamic_cast<TDialogImageName *>(Token));
  DoLoadDialogImage(Image, DialogImageName->ImageName);
}
//---------------------------------------------------------------------------
void __fastcall LoadDialogImage(TImage * Image, const UnicodeString & ImageName)
{
  if (DoLoadDialogImage(Image, ImageName))
  {
    TDialogImageName * DialogImageName = new TDialogImageName();
    DialogImageName->ImageName = ImageName;
    SetRescaleFunction(Image, DialogImageRescale, DialogImageName, true);
  }
}
//---------------------------------------------------------------------------
int __fastcall DialogImageSize(TForm * Form)
{
  return ScaleByPixelsPerInch(32, Form);
}
//---------------------------------------------------------------------------
void __fastcall HideComponentsPanel(TForm * Form)
{
  TComponent * Component = DebugNotNull(Form->FindComponent(L"ComponentsPanel"));
  TPanel * Panel = DebugNotNull(dynamic_cast<TPanel *>(Component));
  DebugAssert(Panel->Align == alBottom);
  int Offset = Panel->Height;
  Panel->Visible = false;
  Panel->Height = 0;
  Form->Height -= Offset;

  for (int Index = 0; Index < Form->ControlCount; Index++)
  {
    TControl * Control = Form->Controls[Index];

    // Shift back bottom-anchored controls
    // (needed for toolbar panel on Progress window and buttons on Preferences dialog),
    if ((Control->Align == alNone) &&
        Control->Anchors.Contains(akBottom) &&
        !Control->Anchors.Contains(akTop))
    {
      Control->Top += Offset;
    }

    // Resize back all-anchored controls
    // (needed for main panel on Preferences dialog),
    if (Control->Anchors.Contains(akBottom) &&
        Control->Anchors.Contains(akTop))
    {
      Control->Height += Offset;
    }
  }
}
//---------------------------------------------------------------------------
TIncrementalSearchState::TIncrementalSearchState()
{
  Reset();
}
//---------------------------------------------------------------------------
void TIncrementalSearchState::Reset()
{
  Searching = false;
  Text = EmptyStr;
  HaveNext = false;
}
//---------------------------------------------------------------------------
UnicodeString FormatIncrementalSearchStatus(const TIncrementalSearchState & SearchState)
{
  UnicodeString Result =
    FMTLOAD(INC_SEARCH, (DefaultStr(SearchState.Text, LoadStr(INC_SEARCH_TYPE)))) +
    ((SearchState.HaveNext && DebugAlwaysTrue(!SearchState.Text.IsEmpty())) ? L" " + LoadStr(INC_NEXT_SEARCH) : EmptyStr);
  return Result;
}
//---------------------------------------------------------------------------
class TCustomDocHandler : public TComponent, public ::IDocHostUIHandler
{
public:
  __fastcall TCustomDocHandler(TComponent * Owner, ICustomDoc * CustomDoc) :
    TComponent(Owner), FCustomDoc(CustomDoc)
  {
  }

  virtual __fastcall ~TCustomDocHandler()
  {
    // Something keeps the reference behind otherwise and calls it on WM_SETTINGCHANGE.
    // Would make sense with TWebBrowserEx, which leaks. But it happens even with TWebBrowser.
    FCustomDoc->SetUIHandler(NULL);
    FCustomDoc->Release();
  }

protected:
  ICustomDoc * FCustomDoc;

  #pragma warn -hid
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID ClassId, void ** Intf)
  {
    HRESULT Result = S_OK;
    if (ClassId == IID_IUnknown)
    {
      *Intf = (IUnknown *)this;
    }
    else if (ClassId == ::IID_IDocHostUIHandler)
    {
      *Intf = (::IDocHostUIHandler *)this;
    }
    else
    {
      Result = E_NOINTERFACE;
    }
    return Result;
  }
  #pragma warn .hid

  virtual ULONG STDMETHODCALLTYPE AddRef()
  {
    return -1;
  }

  virtual ULONG STDMETHODCALLTYPE Release()
  {
    return -1;
  }

  virtual HRESULT STDMETHODCALLTYPE ShowContextMenu(
    DWORD dwID, POINT * ppt, IUnknown * pcmdtReserved, IDispatch * pdispReserved)
  {
    // No context menu
    // (implementing IDocHostUIHandler reenabled context menu disabled by TBrowserViewer::DoContextPopup)
    return S_OK;
  }

  virtual HRESULT STDMETHODCALLTYPE GetHostInfo(::_DOCHOSTUIINFO * Info)
  {
    // Setting ControlBorder is ignored with IDocHostUIHandler.
    // DOCHOSTUIFLAG_DPI_AWARE does not seem to have any effect
    Info->dwFlags |= DOCHOSTUIFLAG_SCROLL_NO | DOCHOSTUIFLAG_NO3DBORDER | DOCHOSTUIFLAG_DPI_AWARE;
    return S_OK;
  }

  virtual HRESULT STDMETHODCALLTYPE ShowUI(
    DWORD dwID, IOleInPlaceActiveObject * pActiveObject, IOleCommandTarget * pCommandTarget, IOleInPlaceFrame * pFrame,
    IOleInPlaceUIWindow * pDoc)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE HideUI()
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE UpdateUI()
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE EnableModeless(BOOL fEnable)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE OnDocWindowActivate(BOOL fActivate)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE OnFrameWindowActivate(BOOL fActivate)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE ResizeBorder(LPCRECT prcBorder, IOleInPlaceUIWindow * pUIWindow, BOOL fRameWindow)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE TranslateAccelerator(LPMSG lpMsg, const GUID * pguidCmdGroup, DWORD nCmdID)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE GetOptionKeyPath(LPOLESTR * pchKey, DWORD dw)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE GetDropTarget(IDropTarget * pDropTarget, IDropTarget ** ppDropTarget)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE GetExternal(IDispatch ** ppDispatch)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE TranslateUrl(DWORD dwTranslate, OLECHAR * pchURLIn, OLECHAR ** ppchURLOut)
  {
    return E_NOTIMPL;
  }

  virtual HRESULT STDMETHODCALLTYPE FilterDataObject(IDataObject * pDO, IDataObject ** ppDORet)
  {
    return E_NOTIMPL;
  }
};
//---------------------------------------------------------------------------
// Included only here as it defines ambiguous LONG_PTR, causing INVALID_HANDLE_VALUE to be unusable
#include <WebBrowserEx.hpp>
//---------------------------------------------------------------------------
class TBrowserViewer : public TWebBrowserEx
{
public:
  __fastcall virtual TBrowserViewer(TComponent* AOwner);

  void __fastcall AddLinkHandler(
    const UnicodeString & Url, TNotifyEvent Handler);
  void __fastcall NavigateToUrl(const UnicodeString & Url);

  TControl * LoadingPanel;

protected:
  DYNAMIC void __fastcall DoContextPopup(const TPoint & MousePos, bool & Handled);
  void __fastcall DocumentComplete(
    TObject * Sender, const _di_IDispatch Disp, const OleVariant & URL);
  void __fastcall BeforeNavigate2(
    TObject * Sender, const _di_IDispatch Disp, const OleVariant & URL,
    const OleVariant & Flags, const OleVariant & TargetFrameName,
    const OleVariant & PostData, const OleVariant & Headers, WordBool & Cancel);

  bool FComplete;
  std::map<UnicodeString, TNotifyEvent> FHandlers;
};
//---------------------------------------------------------------------------
__fastcall TBrowserViewer::TBrowserViewer(TComponent* AOwner) :
  TWebBrowserEx(AOwner)
{
  FComplete = false;

  OnDocumentComplete = DocumentComplete;
  OnBeforeNavigate2 = BeforeNavigate2;
  LoadingPanel = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::AddLinkHandler(
  const UnicodeString & Url, TNotifyEvent Handler)
{
  FHandlers.insert(std::make_pair(Url, Handler));
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::DoContextPopup(const TPoint & MousePos, bool & Handled)
{
  // Suppress built-in context menu.
  // Is ignored with IDocHostUIHandler. Needs to be overriden by ShowContextMenu.
  Handled = true;
  TWebBrowserEx::DoContextPopup(MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::DocumentComplete(
    TObject * /*Sender*/, const _di_IDispatch /*Disp*/, const OleVariant & /*URL*/)
{
  SetBrowserDesignModeOff(this);

  FComplete = true;

  if (LoadingPanel != NULL)
  {
    LoadingPanel->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::BeforeNavigate2(
  TObject * /*Sender*/, const _di_IDispatch /*Disp*/, const OleVariant & AURL,
  const OleVariant & /*Flags*/, const OleVariant & /*TargetFrameName*/,
  const OleVariant & /*PostData*/, const OleVariant & /*Headers*/, WordBool & Cancel)
{
  // If OnDocumentComplete was not called yet, is has to be our initial message URL,
  // opened using TWebBrowserEx::Navigate(), allow it.
  // Otherwise it's user navigating, block that and open link
  // in an external browser, possibly adding campaign parameters on the way.
  if (FComplete)
  {
    Cancel = 1;

    UnicodeString URL = AURL;
    if (FHandlers.count(URL) > 0)
    {
      FHandlers[URL](this);
    }
    else
    {
      OpenBrowser(URL);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::NavigateToUrl(const UnicodeString & Url)
{
  FComplete = false;
  Navigate(Url.c_str());
}
//---------------------------------------------------------------------------
TPanel * __fastcall CreateLabelPanel(TPanel * Parent, const UnicodeString & Label)
{
  TPanel * Result = CreateBlankPanel(Parent);
  Result->Parent = Parent;
  Result->Align = alClient;
  Result->Caption = Label;
  return Result;
}
//---------------------------------------------------------------------------
TWebBrowserEx * __fastcall CreateBrowserViewer(TPanel * Parent, const UnicodeString & LoadingLabel)
{
  TBrowserViewer * Result = new TBrowserViewer(Parent);
  // TWebBrowserEx has its own unrelated Name and Parent properties.
  // The name is used in DownloadUpdate().
  static_cast<TWinControl *>(Result)->Name = L"BrowserViewer";
  static_cast<TWinControl *>(Result)->Parent = Parent;
  Result->Align = alClient;
  // Is ignored with IDocHostUIHandler. Needs to be overriden by DOCHOSTUIFLAG_NO3DBORDER in GetHostInfo.
  Result->ControlBorder = cbNone;

  Result->LoadingPanel = CreateLabelPanel(Parent, LoadingLabel);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SetBrowserDesignModeOff(TWebBrowserEx * WebBrowser)
{
  if (DebugAlwaysTrue(WebBrowser->Document2 != NULL))
  {
    WebBrowser->Document2->designMode = L"Off";
  }
}
//---------------------------------------------------------------------------
void __fastcall AddBrowserLinkHandler(TWebBrowserEx * WebBrowser,
  const UnicodeString & Url, TNotifyEvent Handler)
{
  TBrowserViewer * BrowserViewer = dynamic_cast<TBrowserViewer *>(WebBrowser);
  if (DebugAlwaysTrue(BrowserViewer != NULL))
  {
    BrowserViewer->AddLinkHandler(Url, Handler);
  }
}
//---------------------------------------------------------------------------
void __fastcall NavigateBrowserToUrl(TWebBrowserEx * WebBrowser, const UnicodeString & Url)
{
  TBrowserViewer * BrowserViewer = dynamic_cast<TBrowserViewer *>(WebBrowser);
  if (DebugAlwaysTrue(BrowserViewer != NULL))
  {
    BrowserViewer->NavigateToUrl(Url);
  }
}
//---------------------------------------------------------------------------
void ReadyBrowserForStreaming(TWebBrowserEx * WebBrowser)
{
  // This creates TWebBrowserEx::Document, which we need to stream in an in-memory document
  NavigateBrowserToUrl(WebBrowser, L"about:blank");
  // Needs to be followed by WaitBrowserToIdle
}
//---------------------------------------------------------------------------
void WaitBrowserToIdle(TWebBrowserEx * WebBrowser)
{
  while (WebBrowser->ReadyState < ::READYSTATE_INTERACTIVE)
  {
    Application->ProcessMessages();
  }
}
//---------------------------------------------------------------------------
void HideBrowserScrollbars(TWebBrowserEx * WebBrowser)
{
  ICustomDoc * CustomDoc = NULL;
  if (DebugAlwaysTrue(WebBrowser->Document != NULL) &&
      SUCCEEDED(WebBrowser->Document->QueryInterface(&CustomDoc)) &&
      DebugAlwaysTrue(CustomDoc != NULL))
  {
    TCustomDocHandler * Handler = new TCustomDocHandler(WebBrowser, CustomDoc);
    CustomDoc->SetUIHandler(Handler);
  }
}
//---------------------------------------------------------------------------
bool CopyTextFromBrowser(TWebBrowserEx * WebBrowser, UnicodeString & Text)
{
  WebBrowser->SelectAll();
  WebBrowser->CopyToClipBoard();
  bool Result = NonEmptyTextFromClipboard(Text);
  WebBrowser->DoCommand(L"UNSELECT");
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GenerateAppHtmlPage(TFont * Font, TPanel * Parent, const UnicodeString & Body, bool Seamless)
{
  UnicodeString Result =
    L"<!DOCTYPE html>\n"
    L"<meta charset=\"utf-8\">\n"
    L"<html>\n"
    L"<head>\n"
    L"<style>\n"
    L"\n"
    L"body\n"
    L"{\n"
    L"    font-family: '" + Font->Name + L"';\n"
    L"    margin: " + UnicodeString(Seamless ? L"0" : L"0.5em") + L";\n"
    L"    background-color: " + ColorToWebColorStr(Parent->Color) + L";\n" +
    UnicodeString(Seamless ? L"    overflow: hidden;\n" : L"") +
    L"}\n"
    L"\n"
    L"body\n"
    L"{\n"
    L"    font-size: " + IntToStr(Font->Size) + L"pt;\n"
    L"}\n"
    L"\n"
    L"p\n"
    L"{\n"
    L"    margin-top: 0;\n"
    L"    margin-bottom: 1em;\n"
    L"}\n"
    L"\n"
    L"a, a:visited, a:hover, a:visited, a:current\n"
    L"{\n"
    L"    color: " + ColorToWebColorStr(LinkColor) + L";\n"
    L"}\n"
    L"</style>\n"
    L"</head>\n"
    L"<body>\n" +
    Body +
    L"</body>\n"
    L"</html>\n";
  return Result;
}
//---------------------------------------------------------------------------
void LoadBrowserDocument(TWebBrowserEx * WebBrowser, const UnicodeString & Document)
{
  std::unique_ptr<TMemoryStream> DocumentStream(new TMemoryStream());
  UTF8String DocumentUTF8 = UTF8String(Document);
  DocumentStream->Write(DocumentUTF8.c_str(), DocumentUTF8.Length());
  DocumentStream->Seek(0, 0);

  // For stream-loaded document, when set only after loading from OnDocumentComplete,
  // browser stops working
  SetBrowserDesignModeOff(WebBrowser);

  TStreamAdapter * DocumentStreamAdapter = new TStreamAdapter(DocumentStream.get(), soReference);
  IPersistStreamInit * PersistStreamInit = NULL;
  if (DebugAlwaysTrue(WebBrowser->Document != NULL) &&
      SUCCEEDED(WebBrowser->Document->QueryInterface(IID_IPersistStreamInit, (void **)&PersistStreamInit)) &&
      DebugAlwaysTrue(PersistStreamInit != NULL))
  {
    PersistStreamInit->Load(static_cast<_di_IStream>(*DocumentStreamAdapter));
    PersistStreamInit->Release();
  }
}
//---------------------------------------------------------------------------
TComponent * __fastcall FindComponentRecursively(TComponent * Root, const UnicodeString & Name)
{
  for (int Index = 0; Index < Root->ComponentCount; Index++)
  {
    TComponent * Component = Root->Components[Index];
    if (CompareText(Component->Name, Name) == 0)
    {
      return Component;
    }

    Component = FindComponentRecursively(Component, Name);
    if (Component != NULL)
    {
      return Component;
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall GetInstrutionsTheme(
  TColor & MainInstructionColor, HFONT & MainInstructionFont, HFONT & InstructionFont)
{
  MainInstructionColor = Graphics::clNone;
  MainInstructionFont = 0;
  InstructionFont = 0;
  HTHEME Theme = OpenThemeData(0, L"TEXTSTYLE");
  if (Theme != NULL)
  {
    LOGFONT AFont;
    COLORREF AColor;

    memset(&AFont, 0, sizeof(AFont));
    // Using Canvas->Handle in the 2nd argument we can get scaled font,
    // but at this point the form is sometime not scaled yet (difference is particularly for standalone messages like
    // /UninstallCleanup), so the results are inconsistent.
    if (GetThemeFont(Theme, NULL, TEXT_MAININSTRUCTION, 0, TMT_FONT, &AFont) == S_OK)
    {
      MainInstructionFont = CreateFontIndirect(&AFont);
    }
    if (GetThemeColor(Theme, TEXT_MAININSTRUCTION, 0, TMT_TEXTCOLOR, &AColor) == S_OK)
    {
      MainInstructionColor = (TColor)AColor;
    }

    memset(&AFont, 0, sizeof(AFont));
    if (GetThemeFont(Theme, NULL, TEXT_INSTRUCTION, 0, TMT_FONT, &AFont) == S_OK)
    {
      InstructionFont = CreateFontIndirect(&AFont);
    }

    CloseThemeData(Theme);
  }
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand()
{
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand(
    const TCustomCommandData & Data, const UnicodeString & RemotePath, const UnicodeString & LocalPath) :
  TFileCustomCommand(Data, RemotePath)
{
  FLocalPath = LocalPath;
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand(const TCustomCommandData & Data,
  const UnicodeString & RemotePath, const UnicodeString & LocalPath, const UnicodeString & FileName,
  const UnicodeString & LocalFileName, const UnicodeString & FileList) :
  TFileCustomCommand(Data, RemotePath, FileName, FileList)
{
  FLocalPath = LocalPath;
  FLocalFileName = LocalFileName;
}
//---------------------------------------------------------------------------
int __fastcall TLocalCustomCommand::PatternLen(const UnicodeString & Command, int Index)
{
  int Len;
  if ((Index < Command.Length()) && (Command[Index + 1] == L'\\'))
  {
    Len = 2;
  }
  else if ((Index < Command.Length()) && (Command[Index + 1] == L'^'))
  {
    Len = 3;
  }
  else
  {
    Len = TFileCustomCommand::PatternLen(Command, Index);
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::PatternReplacement(
  int Index, const UnicodeString & Pattern, UnicodeString & Replacement, bool & Delimit)
{
  bool Result;
  if (Pattern == L"!\\")
  {
    // When used as "!\" in an argument to PowerShell, the trailing \ would escape the ",
    // so we exclude it
    Replacement = ExcludeTrailingBackslash(FLocalPath);
    Result = true;
  }
  else if (Pattern == L"!^!")
  {
    Replacement = FLocalFileName;
    Result = true;
  }
  else
  {
    Result = TFileCustomCommand::PatternReplacement(Index, Pattern, Replacement, Delimit);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLocalCustomCommand::DelimitReplacement(
  UnicodeString & /*Replacement*/, wchar_t /*Quote*/)
{
  // never delimit local commands
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::HasLocalFileName(const UnicodeString & Command)
{
  return FindPattern(Command, L'^');
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::IsFileCommand(const UnicodeString & Command)
{
  return TFileCustomCommand::IsFileCommand(Command) || HasLocalFileName(Command);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
typedef std::set<TDataModule *> TImagesModules;
static TImagesModules ImagesModules;
static std::map<int, TPngImageList *> AnimationsImages;
static std::map<int, TImageList *> ButtonImages;
static std::map<int, TPngImageList *> DialogImages;
//---------------------------------------------------------------------------
int __fastcall DoNormalizePixelsPerInch(int PixelsPerInch, bool Larger)
{
  if (PixelsPerInch >= 192)
  {
    PixelsPerInch = 192;
  }
  else if (PixelsPerInch >= 144)
  {
    PixelsPerInch = Larger ? 192 : 144;
  }
  else if (PixelsPerInch >= 120)
  {
    PixelsPerInch = Larger ? 144 : 120;
  }
  else
  {
    PixelsPerInch = Larger ? 120 : 96;
  }
  return PixelsPerInch;
}
//---------------------------------------------------------------------------
int NormalizePixelsPerInch(int PixelsPerInch)
{
  return DoNormalizePixelsPerInch(PixelsPerInch, false);
}
//---------------------------------------------------------------------------
int LargerPixelsPerInch(int PixelsPerInch, int Larger)
{
  int Result = PixelsPerInch;
  while (Larger > 0)
  {
    Result = DoNormalizePixelsPerInch(Result, true);
    Larger--;
  }
  return Result;
}
//---------------------------------------------------------------------------
static int __fastcall NeedImagesModule(TControl * Control)
{
  int PixelsPerInch = NormalizePixelsPerInch(GetControlPixelsPerInch(Control));

  if (AnimationsImages.find(PixelsPerInch) == AnimationsImages.end())
  {
    TDataModule * ImagesModule;
    HANDLE ResourceModule = GUIConfiguration->ChangeToDefaultResourceModule();
    try
    {
      if (PixelsPerInch == 192)
      {
        ImagesModule = new TAnimations192Module(Application);
      }
      else if (PixelsPerInch == 144)
      {
        ImagesModule = new TAnimations144Module(Application);
      }
      else if (PixelsPerInch == 120)
      {
        ImagesModule = new TAnimations120Module(Application);
      }
      else
      {
        DebugAssert(PixelsPerInch == 96);
        ImagesModule = new TAnimations96Module(Application);
      }

      ImagesModules.insert(ImagesModule);

      TPngImageList * AAnimationImages =
        DebugNotNull(dynamic_cast<TPngImageList *>(ImagesModule->FindComponent(L"AnimationImages")));
      AnimationsImages.insert(std::make_pair(PixelsPerInch, AAnimationImages));

      TImageList * AButtonImages =
        DebugNotNull(dynamic_cast<TImageList *>(ImagesModule->FindComponent(L"ButtonImages")));
      ButtonImages.insert(std::make_pair(PixelsPerInch, AButtonImages));

      TPngImageList * ADialogImages =
        DebugNotNull(dynamic_cast<TPngImageList *>(ImagesModule->FindComponent(L"DialogImages")));
      DialogImages.insert(std::make_pair(PixelsPerInch, ADialogImages));
    }
    __finally
    {
      GUIConfiguration->ChangeResourceModule(ResourceModule);
    }
  }

  return PixelsPerInch;
}
//---------------------------------------------------------------------------
TPngImageList * __fastcall GetAnimationsImages(TControl * Control)
{
  int PixelsPerInch = NeedImagesModule(Control);
  return DebugNotNull(AnimationsImages[PixelsPerInch]);
}
//---------------------------------------------------------------------------
TImageList * __fastcall GetButtonImages(TControl * Control)
{
  int PixelsPerInch = NeedImagesModule(Control);
  return DebugNotNull(ButtonImages[PixelsPerInch]);
}
//---------------------------------------------------------------------------
TPngImageList * __fastcall GetDialogImages(TControl * Control)
{
  int PixelsPerInch = NeedImagesModule(Control);
  return DebugNotNull(DialogImages[PixelsPerInch]);
}
//---------------------------------------------------------------------------
void __fastcall ReleaseImagesModules()
{

  TImagesModules::iterator i = ImagesModules.begin();
  while (i != ImagesModules.end())
  {
    delete (*i);
    i++;
  }
  ImagesModules.clear();
}
//---------------------------------------------------------------------------
__fastcall TFrameAnimation::TFrameAnimation()
{
  FFirstFrame = -1;
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Init(TPaintBox * PaintBox, const UnicodeString & Name)
{
  FName = Name;
  FPaintBox = PaintBox;

  FPaintBox->ControlStyle = FPaintBox->ControlStyle << csOpaque;
  DebugAssert((FPaintBox->OnPaint == NULL) || (FPaintBox->OnPaint == PaintBoxPaint));
  FPaintBox->OnPaint = PaintBoxPaint;
  SetRescaleFunction(FPaintBox, PaintBoxRescale, reinterpret_cast<TObject *>(this));

  DoInit();
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::DoInit()
{
  FImageList = GetAnimationsImages(FPaintBox);
  FFirstFrame = -1;
  FFirstLoopFrame = -1;
  FPaintBox->Width = FImageList->Width;
  FPaintBox->Height = FImageList->Height;

  if (!FName.IsEmpty())
  {
    int Frame = 0;
    while (Frame < FImageList->PngImages->Count)
    {
      UnicodeString FrameData = FImageList->PngImages->Items[Frame]->Name;
      UnicodeString FrameName;
      FrameName = CutToChar(FrameData, L'_', false);

      if (SameText(FName, FrameName))
      {
        int FrameIndex = StrToInt(CutToChar(FrameData, L'_', false));
        if (FFirstFrame < 0)
        {
          FFirstFrame = Frame;
        }
        if ((FFirstLoopFrame < 0) && (FrameIndex > 0))
        {
          FFirstLoopFrame = Frame;
        }
        FLastFrame = Frame;
      }
      else
      {
        if (FFirstFrame >= 0)
        {
          // optimization
          break;
        }
      }
      Frame++;
    }

    DebugAssert(FFirstFrame >= 0);
    DebugAssert(FFirstLoopFrame >= 0);
  }

  Stop();
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::PaintBoxRescale(TComponent * /*Sender*/, TObject * Token)
{
  TFrameAnimation * FrameAnimation = reinterpret_cast<TFrameAnimation *>(Token);
  FrameAnimation->Rescale();
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Rescale()
{
  bool Started = (FTimer != NULL) && FTimer->Enabled;
  DoInit();
  if (Started)
  {
    Start();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Start()
{
  if (FFirstFrame >= 0)
  {
    FNextFrameTick = GetTickCount();
    CalculateNextFrameTick();

    if (FTimer == NULL)
    {
      FTimer = new TTimer(GetParentForm(FPaintBox));
      FTimer->Interval = static_cast<int>(GUIUpdateInterval);
      FTimer->OnTimer = Timer;
    }
    else
    {
      // reset timer
      FTimer->Enabled = false;
      FTimer->Enabled = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Timer(TObject * /*Sender*/)
{
  Animate();
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::PaintBoxPaint(TObject * Sender)
{
  if (FFirstFrame >= 0)
  {
    // Double-buffered drawing to prevent flicker (as the images are transparent)
    DebugUsedParam(Sender);
    DebugAssert(FPaintBox == Sender);
    DebugAssert(FPaintBox->ControlStyle.Contains(csOpaque));
    std::unique_ptr<TBitmap> Bitmap(new TBitmap());
    Bitmap->SetSize(FPaintBox->Width, FPaintBox->Height);
    Bitmap->Canvas->Brush->Color = FPaintBox->Color;
    TRect Rect(0, 0, Bitmap->Width, Bitmap->Height);
    Bitmap->Canvas->FillRect(Rect);
    TGraphic * Graphic = GetCurrentImage()->PngImage;
    DebugAssert(Graphic->Width == FPaintBox->Width);
    DebugAssert(Graphic->Height == FPaintBox->Height);
    Bitmap->Canvas->Draw(0, 0, Graphic);
    FPaintBox->Canvas->Draw(0, 0, Bitmap.get());
  }
  FPainted = true;
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Repaint()
{
  FPainted = false;
  // If the form is not showing yet, the Paint() is not even called
  FPaintBox->Repaint();
  if (!FPainted)
  {
    // Paint later, alternatively we may keep trying Repaint() in Animate().
    // See also a hack in TAuthenticateForm::Log.
    FPaintBox->Invalidate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Stop()
{
  FNextFrameTick = std::numeric_limits<DWORD>::max();
  FCurrentFrame = FFirstFrame;
  Repaint();

  if (FTimer != NULL)
  {
    FTimer->Enabled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::Animate()
{
  if (FFirstFrame >= 0)
  {
    // UPGRADE: Use GetTickCount64() when we stop supporting Windows XP.
    DWORD TickCount = GetTickCount();

    // Keep in sync with an opposite condition at the end of the loop.
    // We may skip some frames if we got stalled for a while
    while (TickCount >= FNextFrameTick)
    {
      if (FCurrentFrame >= FLastFrame)
      {
        FCurrentFrame = FFirstLoopFrame;
      }
      else
      {
        FCurrentFrame++;
      }

      CalculateNextFrameTick();

      Repaint();
    }
  }
}
//---------------------------------------------------------------------------
TPngImageCollectionItem * __fastcall TFrameAnimation::GetCurrentImage()
{
  return FImageList->PngImages->Items[FCurrentFrame];
}
//---------------------------------------------------------------------------
void __fastcall TFrameAnimation::CalculateNextFrameTick()
{
  TPngImageCollectionItem * ImageItem = GetCurrentImage();
  UnicodeString Duration = ImageItem->Name;
  CutToChar(Duration, L'_', false);
  // skip index (is not really used)
  CutToChar(Duration, L'_', false);
  // This should overflow, when tick count wraps.
  FNextFrameTick += StrToInt(Duration) * 10;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Hints use:
// - Cleanup list tooltip (multi line)
// - Combo edit button
// - Transfer settings label (multi line, follows label size and font)
// - HintLabel (hint and persistent hint, multi line)
// - status bar hints
//---------------------------------------------------------------------------
__fastcall TScreenTipHintWindow::TScreenTipHintWindow(TComponent * Owner) :
  THintWindow(Owner)
{
  FParentPainting = false;
}
//---------------------------------------------------------------------------
int __fastcall TScreenTipHintWindow::GetTextFlags(TControl * Control)
{
  return DT_LEFT | DT_WORDBREAK | DT_NOPREFIX | Control->DrawTextBiDiModeFlagsReadingOnly();
}
//---------------------------------------------------------------------------
bool __fastcall TScreenTipHintWindow::UseBoldShortHint(TControl * HintControl)
{
  return
    (dynamic_cast<TTBCustomDockableWindow *>(HintControl) != NULL) ||
    (dynamic_cast<TTBPopupWindow *>(HintControl) != NULL) ||
    (HintControl->Perform(WM_WANTS_SCREEN_TIPS, 0, 0) == 1);
}
//---------------------------------------------------------------------------
bool __fastcall TScreenTipHintWindow::IsPathLabel(TControl * HintControl)
{
  return (dynamic_cast<TPathLabel *>(HintControl) != NULL);
}
//---------------------------------------------------------------------------
int __fastcall TScreenTipHintWindow::GetMargin(TControl * HintControl, const UnicodeString & Hint)
{
  int Result;

  if (HasLabelHintPopup(HintControl, Hint) || IsPathLabel(HintControl))
  {
    Result = 3;
  }
  else
  {
    Result = 6;
  }

  Result = ScaleByTextHeight(HintControl, Result);

  return Result;
}
//---------------------------------------------------------------------------
TFont * __fastcall TScreenTipHintWindow::GetFont(TControl * HintControl, const UnicodeString & Hint)
{
  TFont * Result;
  if (HasLabelHintPopup(HintControl, Hint) || IsPathLabel(HintControl))
  {
    Result = reinterpret_cast<TLabel *>(dynamic_cast<TCustomLabel *>(HintControl))->Font;
  }
  else
  {
    FScaledHintFont.reset(new TFont());
    FScaledHintFont->Assign(Screen->HintFont);
    FScaledHintFont->Size = ScaleByPixelsPerInchFromSystem(FScaledHintFont->Size, HintControl);
    Result = FScaledHintFont.get();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScreenTipHintWindow::CalcHintTextRect(TControl * Control, TCanvas * Canvas, TRect & Rect, const UnicodeString & Hint)
{
  const int Flags = DT_CALCRECT | GetTextFlags(Control);
  DrawText(Canvas->Handle, Hint.c_str(), -1, &Rect, Flags);
}
//---------------------------------------------------------------------------
TRect __fastcall TScreenTipHintWindow::CalcHintRect(int MaxWidth, const UnicodeString AHint, void * AData)
{
  TControl * HintControl = GetHintControl(AData);
  int Margin = GetMargin(HintControl, AHint);
  UnicodeString ShortHint;
  UnicodeString LongHint;
  SplitHint(HintControl, AHint, ShortHint, LongHint);

  Canvas->Font->Assign(GetFont(HintControl, AHint));

  // from XE6 Vcl.ScreenTips.pas, but absent in 11
  const cScreenTipTextOnlyWidth = 210;
  const int ScreenTipTextOnlyWidth = ScaleByTextHeight(HintControl, cScreenTipTextOnlyWidth);

  int LongHintMargin = 0; // shut up
  bool HasLongHint = !LongHint.IsEmpty();
  if (HasLongHint)
  {
    // double-margin on the right
    LongHintMargin = (3 * Margin);
    MaxWidth = ScreenTipTextOnlyWidth - LongHintMargin;
  }

  // Multi line short hints can be twice as wide, to not break the individual lines unless really necessary.
  // (login site tree, clean up dialog list, preferences custom command list, persistent hint, etc).
  // And they also can be twice as wide, to not break the individual lines unless really necessary.
  if (ShortHint.Pos(L"\n") > 0)
  {
    MaxWidth *= 2;
  }

  bool HintPopup = HasLabelHintPopup(HintControl, AHint);
  if (HintPopup)
  {
    MaxWidth = HintControl->Width;
  }

  if (UseBoldShortHint(HintControl))
  {
    Canvas->Font->Style = TFontStyles() << fsBold;
  }
  TRect ShortRect(0, 0, MaxWidth, 0);
  CalcHintTextRect(this, Canvas, ShortRect, ShortHint);
  Canvas->Font->Style = TFontStyles();

  TRect Result;

  if (!HasLongHint)
  {
    Result = ShortRect;

    if (HintPopup)
    {
      Result.Right = MaxWidth + 2 * Margin;
    }
    else
    {
      Result.Right += 3 * Margin;
    }

    Result.Bottom += 2 * Margin;
  }
  else
  {
    const int LongIndentation = Margin * 3 / 2;
    TRect LongRect(0, 0, MaxWidth - LongIndentation, 0);
    CalcHintTextRect(this, Canvas, LongRect, LongHint);

    Result.Right = Max(ScreenTipTextOnlyWidth, LongRect.Right + LongIndentation + LongHintMargin);
    Result.Bottom = Margin + ShortRect.Height() + (Margin / 3 * 5) + LongRect.Height() + Margin;
  }

  // VCLCOPY: To counter the increase in THintWindow::ActivateHintData
  Result.Bottom -= 4;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScreenTipHintWindow::SplitHint(
  TControl * HintControl, const UnicodeString & Hint, UnicodeString & ShortHint, UnicodeString & LongHint)
{
  if (HasLabelHintPopup(HintControl, Hint))
  {
    ShortHint = HintControl->Hint;
  }
  else
  {
    ShortHint = GetShortHint(Hint);
    LongHint = GetLongHintIfAny(Hint);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScreenTipHintWindow::ActivateHintData(const TRect & ARect, const UnicodeString AHint, void * AData)
{
  FHintControl = GetHintControl(AData);
  SplitHint(FHintControl, AHint, FShortHint, FLongHint);
  FMargin = GetMargin(FHintControl, AHint);
  FHintPopup = HasLabelHintPopup(FHintControl, AHint);

  Canvas->Font->Assign(GetFont(FHintControl, AHint));

  TRect Rect = ARect;

  if (FHintPopup)
  {
    Rect.SetLocation(FHintControl->ClientToScreen(TPoint(-FMargin, -FMargin)));
  }
  if (IsPathLabel(FHintControl))
  {
    Rect.Offset(-FMargin, -FMargin);
  }

  THintWindow::ActivateHintData(Rect, FShortHint, AData);
}
//---------------------------------------------------------------------------
TControl * __fastcall TScreenTipHintWindow::GetHintControl(void * Data)
{
  return reinterpret_cast<TControl *>(DebugNotNull(Data));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScreenTipHintWindow::GetLongHintIfAny(const UnicodeString & AHint)
{
  UnicodeString Result;
  int P = Pos(L"|", AHint);
  if (P > 0)
  {
    Result = GetLongHint(AHint);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScreenTipHintWindow::Dispatch(void * AMessage)
{
  TMessage * Message = static_cast<TMessage*>(AMessage);
  switch (Message->Msg)
  {
    case WM_GETTEXTLENGTH:
      if (FParentPainting)
      {
        // make THintWindow::Paint() not paint the Caption
        Message->Result = 0;
      }
      else
      {
        THintWindow::Dispatch(AMessage);
      }
      break;

    default:
      THintWindow::Dispatch(AMessage);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScreenTipHintWindow::Paint()
{
  // paint frame/background
  {
    TAutoFlag ParentPaintingFlag(FParentPainting);
    THintWindow::Paint();
  }

  const int Flags = GetTextFlags(this);
  const int Margin = FMargin - 1; // 1 = border

  TRect Rect = ClientRect;
  Rect.Inflate(-Margin, -Margin);
  if (!FHintPopup)
  {
    Rect.Right -= FMargin;
  }
  if (UseBoldShortHint(FHintControl))
  {
    Canvas->Font->Style = TFontStyles() << fsBold;
  }
  DrawText(Canvas->Handle, FShortHint.c_str(), -1, &Rect, Flags);
  TRect ShortRect = Rect;
  DrawText(Canvas->Handle, FShortHint.c_str(), -1, &ShortRect, DT_CALCRECT | Flags);
  Canvas->Font->Style = TFontStyles();

  if (!FLongHint.IsEmpty())
  {
    Rect.Left += FMargin * 3 / 2;
    Rect.Top += ShortRect.Height() + (FMargin / 3 * 5);
    DrawText(Canvas->Handle, FLongHint.c_str(), -1, &Rect, Flags);
  }
}
//---------------------------------------------------------------------------
static int HideAccelFlag(TControl * Control)
{
  //ask the top level window about its UI state
  while (Control->Parent != NULL)
  {
    Control = Control->Parent;
  }
  int Result;
  if (FLAGSET(Control->Perform(WM_QUERYUISTATE, 0, 0), UISF_HIDEACCEL))
  {
    Result = DT_HIDEPREFIX;
  }
  else
  {
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TUIStateAwareLabel::TUIStateAwareLabel(TComponent * AOwner) :
  TLabel(AOwner)
{
}
//---------------------------------------------------------------------------
void __fastcall TUIStateAwareLabel::DoDrawText(TRect & Rect, int Flags)
{
  if (ShowAccelChar)
  {
    Flags = Flags | HideAccelFlag(this);
  }
  TLabel::DoDrawText(Rect, Flags);
}
//---------------------------------------------------------------------------
void __fastcall TUIStateAwareLabel::Dispatch(void * AMessage)
{
  TMessage * Message = static_cast<TMessage*>(AMessage);
  // WORKAROUND: Particularly when focusing csDropDownList-style combobox via label, there's no visual feedback
  // that the combobox was selected (strangely, there is, when the previous focus was on TTreeView).
  // For consistency, we enable focus display for all controls types (like checkboxes).
  if (Message->Msg == CM_DIALOGCHAR)
  {
    bool WasFocused = (FocusControl != NULL) ? FocusControl->Focused() : false;
    TLabel::Dispatch(AMessage);
    if (!WasFocused && (FocusControl != NULL) && FocusControl->Focused())
    {
      TCustomForm * ParentForm = GetParentForm(this);
      if (ParentForm != NULL)
      {
        ParentForm->Perform(WM_CHANGEUISTATE, MAKELONG(UIS_CLEAR, UISF_HIDEFOCUS), 0);
      }
    }
  }
  else
  {
    TLabel::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall FindComponentClass(
  void *, TReader *, const UnicodeString DebugUsedArg(ClassName), TComponentClass & ComponentClass)
{
  if (ComponentClass == __classid(TLabel))
  {
    ComponentClass = __classid(TUIStateAwareLabel);
  }
  else if (ComponentClass == __classid(TComboBox))
  {
    ComponentClass = __classid(TUIStateAwareComboBox);
  }
}
//---------------------------------------------------------------------------
bool CanShowTimeEstimate(TDateTime StartTime)
{
  return (SecondsBetween(StartTime, Now()) >= 3);
}
//---------------------------------------------------------------------------
class TSystemRequiredThread : public TSignalThread
{
public:
  TSystemRequiredThread();
  void Required();
protected:
  virtual bool __fastcall WaitForEvent();
  virtual void __fastcall ProcessEvent();
private:
  bool FRequired;
  TDateTime FLastRequired;
};
//---------------------------------------------------------------------------
std::unique_ptr<TCriticalSection> SystemRequiredThreadSection(TraceInitPtr(new TCriticalSection()));
TSystemRequiredThread * SystemRequiredThread = NULL;
//---------------------------------------------------------------------------
TSystemRequiredThread::TSystemRequiredThread() :
  TSignalThread(true), FRequired(false)
{
}
//---------------------------------------------------------------------------
void TSystemRequiredThread::Required()
{
  // guarded in SystemRequired()
  FLastRequired = Now();
  TriggerEvent();
}
//---------------------------------------------------------------------------
bool __fastcall TSystemRequiredThread::WaitForEvent()
{
  const int ExpireInterval = 5000;
  bool Result = (TSignalThread::WaitForEvent(ExpireInterval) > 0);
  if (!Result && !FTerminated)
  {
    TGuard Guard(SystemRequiredThreadSection.get());
    if (!FTerminated && FRequired &&
        (MilliSecondsBetween(Now(), FLastRequired) > ExpireInterval))
    {
      AppLog(L"System is not required");
      SetThreadExecutionState(ES_CONTINUOUS);
      FLastRequired = TDateTime();
      FRequired = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSystemRequiredThread::ProcessEvent()
{
  TGuard Guard(SystemRequiredThreadSection.get());
  if (!FRequired &&
      (FLastRequired != TDateTime()))
  {
    AppLog(L"System is required");
    SetThreadExecutionState(ES_SYSTEM_REQUIRED | ES_CONTINUOUS);
    FRequired = true;
  }
}
//---------------------------------------------------------------------------
void SystemRequired()
{
  if (IsWin11())
  {
    TGuard Guard(SystemRequiredThreadSection.get());
    if (SystemRequiredThread == NULL)
    {
      AppLog(L"Starting system required thread");
      SystemRequiredThread = new TSystemRequiredThread();
      SystemRequiredThread->Start();
    }
    SystemRequiredThread->Required();
  }
  else
  {
    SetThreadExecutionState(ES_SYSTEM_REQUIRED);
  }
}
//---------------------------------------------------------------------------
void GUIFinalize()
{
  TPuttyCleanupThread::Finalize();

  TSystemRequiredThread * Thread;
  {
    TGuard Guard(SystemRequiredThreadSection.get());
    Thread = SystemRequiredThread;
    SystemRequiredThread = NULL;
  }

  if (Thread != NULL)
  {
    AppLog(L"Stopping system required thread");
    Thread->Terminate();
    Thread->WaitFor();
    delete Thread;
  }
}
//---------------------------------------------------------------------------
TCustomImageList * TreeViewImageList(TPngImageList * ImageList)
{
  // WORKAROUND Prevent DPI scaling, see TCustomTreeView.SetImages
  ImageList->Scaled = true;
  return ImageList;
}
