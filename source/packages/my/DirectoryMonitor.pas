unit DirectoryMonitor;

// Based on DirMon by phaeteon and LoLa

interface

uses
  Windows, Messages, SysUtils, Classes, CompThread;

type
  EDirectoryMonitorError = class(Exception);

  TFileChangedEvent = procedure(Sender: TObject; const FileName: string) of Object;
  TFileRenamedEvent = procedure(Sender: TObject; const FromFileName: string; const ToFileName: string) of Object;

  TDirectoryMonitor = class(TComponent)
  private
    FDirectoryHandle: THandle;
    FNotificationBuffer: array[0..4096] of Byte;
    FWatchThread: TCompThread;
    FWatchFilters: DWord;
    FOverlapped: TOverlapped;
    FPOverlapped: POverlapped;
    FBytesWritten: DWORD;
    FCompletionPort: THandle;
    FPath: string;
    FActive: Boolean;
    FOnCreated: TFileChangedEvent;
    FOnDeleted: TFileChangedEvent;
    FOnModified: TFileChangedEvent;
    FOnRenamed: TFileRenamedEvent;
    FWatchSubTree: Boolean;

    procedure SetActive(AActive: Boolean);
    procedure SetPath(aPath: string);

  protected
    procedure Start;
    procedure Stop;
    procedure DoCreated(Sender: TObject; FileName: string);
    procedure DoDeleted(Sender: TObject; FileName: string);
    procedure DoModified(Sender: TObject; FileName: string);
    procedure DoRenamed(Sender: TObject; FromFileName: string; ToFileName: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Active: Boolean read FActive write SetActive;
    property Path: string read FPath write SetPath;
    property OnCreated: TFileChangedEvent read FOnCreated write FOnCreated;
    property OnDeleted: TFileChangedEvent read FOnDeleted write FOnDeleted;
    property OnModified: TFileChangedEvent read FOnModified write FOnModified;
    property OnRenamed: TFileRenamedEvent read FOnRenamed write FOnRenamed;
    property WatchSubtree: Boolean read FWatchSubTree write FWatchSubtree;
    property WatchFilters: DWord read FWatchFilters write FWatchFilters;
  end;

implementation

type
  // See Windows API help
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWord;
    Action: DWord;
    FileNameLength: DWord;
    FileName: array[0..0] of WideChar;
  end;

const
  FILE_LIST_DIRECTORY = $0001;

type
  TDirectoryMonitorThread = class(TCompThread)
  private
    FParent: TDirectoryMonitor;
    FRenamedFrom: string;
    procedure HandleEvent;

  protected
    procedure Execute; override;

  public
    constructor Create(AParent: TDirectoryMonitor);
  end;

constructor TDirectoryMonitorThread.Create(AParent: TDirectoryMonitor);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FParent := AParent;
end;

procedure TDirectoryMonitorThread.HandleEvent;
var
  FileOpNotification: PFileNotifyInformation;
  Offset: Longint;
begin
  Pointer(FileOpNotification) := @FParent.FNotificationBuffer[0];
  repeat
    Offset := FileOpNotification^.NextEntryOffset;
    case FileOpNotification^.Action of
      1: FParent.DoCreated(FParent, WideCharTostring(@(FileOpNotification^.FileName)));
      2: FParent.DoDeleted(FParent, WideCharTostring(@(FileOpNotification^.FileName)));
      3: FParent.DoModified(FParent, WideCharTostring(@(FileOpNotification^.FileName)));
      4: FRenamedFrom := WideCharTostring(@(FileOpNotification^.FileName));
      5: FParent.DoRenamed(FParent, FRenamedFrom, WideCharToString(@(FileOpNotification^.FileName)));
    end;
    PChar(FileOpNotification) := PChar(FileOpNotification)+Offset;
  until (Offset = 0);
end;

procedure TDirectoryMonitorThread.Execute;
var
  NumBytes: DWord;
  CompletionKey: ULONG_PTR;
begin
  while (not Terminated) do
  begin
    GetQueuedCompletionStatus(FParent.FCompletionPort, NumBytes, CompletionKey, FParent.FPOverlapped, INFINITE);
    if CompletionKey <> 0 then
    begin
      Synchronize(HandleEvent);
      with FParent do
      begin
        FBytesWritten := 0;
        ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
        ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FParent.WatchSubtree, WatchFilters, @FBytesWritten, @FOverlapped, nil);
      end;
    end
      else
    begin
      Terminate;
    end;
  end;
end;

{ TDirectoryMonitor }

constructor TDirectoryMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FCompletionPort := 0;
  FDirectoryHandle := 0;
  FPOverlapped := @FOverlapped;
  ZeroMemory(@FOverlapped, SizeOf(FOverlapped));
  FWatchFilters := FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_CREATION;
end;

destructor TDirectoryMonitor.Destroy;
begin
  if FActive then Stop;
  inherited;
end;

procedure TDirectoryMonitor.SetActive(AActive: Boolean);
begin
  if csDesigning in ComponentState then Exit;

  if AActive <> FActive then
  begin
    if AActive then Start
      else Stop;
  end;
end;

procedure TDirectoryMonitor.Start;
begin
  FDirectoryHandle :=
    CreateFile(
      PChar(FPath),
      FILE_LIST_DIRECTORY,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
      0);

  if FDirectoryHandle = INVALID_HANDLE_VALUE then
  begin
    FDirectoryHandle := 0;
    raise EDirectoryMonitorError.Create(SysErrorMessage(GetLastError));
    exit;
  end;

  FCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, LongInt(Pointer(Self)), 0);
  ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
  FBytesWritten := 0;
  if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FWatchSubTree, WatchFilters, @FBytesWritten, @FOverlapped, nil) then
  begin
    CloseHandle(FDirectoryHandle);
    FDirectoryHandle := 0;
    CloseHandle(FCompletionPort);
    FCompletionPort := 0;
    raise EDirectoryMonitorError.Create(SysErrorMessage(GetLastError));
    exit;
  end;
  FWatchThread := TDirectoryMonitorThread.Create(Self);
  TDirectoryMonitorThread(FWatchThread).Resume;
  FActive := True;
end;

procedure TDirectoryMonitor.Stop;
begin
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  FWatchThread.WaitFor;
  FWatchThread.Free;
  CloseHandle(FDirectoryHandle);
  FDirectoryHandle := 0;
  CloseHandle(FCompletionPort);
  FCompletionPort := 0;
  FActive := False;
end;

procedure TDirectoryMonitor.DoCreated(Sender: TObject; FileName: string);
begin
  if Assigned(FOnCreated) then FOnCreated(Sender, FPath + FileName);
end;

procedure TDirectoryMonitor.DoDeleted(Sender: TObject; FileName: string);
begin
  if Assigned(FOnDeleted) then FOnDeleted(Sender, FPath + FileName);
end;

procedure TDirectoryMonitor.DoModified(Sender: TObject; FileName: string);
begin
  if Assigned(FOnModified) then FOnModified(Sender, FPath + FileName);
end;

procedure TDirectoryMonitor.DoRenamed(Sender: TObject; FromFileName: string; ToFileName: string);
begin
  if Assigned(FOnRenamed) then FOnRenamed(Sender, FPath + FromFileName, FPath + ToFileName);
end;

procedure TDirectoryMonitor.SetPath(APath: string);
begin
  APath := IncludeTrailingPathDelimiter(APath);
  if APath <> FPath then
  begin
    FPath := APath;
    if FActive then
    begin
      Stop;
      Start;
    end;
  end;
end;

end.
