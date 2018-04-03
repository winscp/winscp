unit OperationWithTimeout;

interface

uses
  Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI, ActiveX;

function ShellFolderGetAttributesOfWithTimeout(
  ShellFolder: IShellFolder; cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT; Timeout: Integer): HResult;

function SHGetFileInfoWithTimeout(
  pszPath: LPCWSTR; dwFileAttributes: DWORD; var psfi: TSHFileInfoW;
  cbFileInfo, uFlags: UINT; Timeout: Integer): DWORD_PTR;

function ShellFolderParseDisplayNameWithTimeout(
  ShellFolder: IShellFolder; hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR;
  out pchEaten: ULONG; out ppidl: PItemIDList; var dwAttributes: ULONG; Timeout: Integer): HResult;

implementation

uses
  System.Classes, System.Types, System.SysUtils, System.SyncObjs, System.Contnrs, CompThread;

type
  TOperation = class;

  TOperationEvent = procedure(Operation: TOperation);

  TOperation = class(TObject)
  public
    OperationEvent: TOperationEvent;

    // ShellFolderGetAttributesOfWithTimeout
    ShellFolder: IShellFolder;
    cidl: UINT;
    apidl: PItemIDList;
    rgfInOut: UINT;
    Timeout: Integer;
    ResultHResult: HResult;

    // SHGetFileInfoWithTimeout
    PIDL: PItemIDList;
    Path: string;
    dwFileAttributes: DWORD;
    psfi: TSHFileInfoW;
    cbFileInfo, uFlags: UINT;
    ResultDWordPtr: DWORD_PTR;

    // ShellFolderParseDisplayNameWithTimeout
    // Uses ShellFolder and ResultHResult
    hwndOwner: HWND;
    pbcReserved: Pointer;
    DisplayName: string;
    pchEaten: ULONG;
    ppidl: PItemIDList;
    dwAttributes: ULONG;

    constructor Create(AOperationEvent: TOperationEvent);
  end;

constructor TOperation.Create(AOperationEvent: TOperationEvent);
begin
  OperationEvent := AOperationEvent;
end;

type
  TOperationWithTimeoutThread = class(TCompThread)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate; override;

    procedure Queue(Operation: TOperation);
    function WaitForOperation(Milliseconds: Cardinal): Boolean;
    procedure Remove(Operation: TOperation);

  protected
    procedure Execute; override;

  private
    FCriticalSection: TCriticalSection;
    FRequestEvent: THandle;
    FResultEvent: THandle;
    FQueue: TObjectList;
    FResults: TObjectList;
  end;

constructor TOperationWithTimeoutThread.Create;
begin
  inherited Create(True);
  FRequestEvent := CreateEvent(nil, False, False, nil);
  FResultEvent := CreateEvent(nil, False, False, nil);
  FCriticalSection := TCriticalSection.Create;
  FQueue := TObjectList.Create;
  FResults := TObjectList.Create;
  Resume;
end;

destructor TOperationWithTimeoutThread.Destroy;
begin
  inherited;
  FQueue.Free;
  FResults.Free;
  FCriticalSection.Free;
  CloseHandle(FRequestEvent);
  CloseHandle(FResultEvent);
end;

procedure TOperationWithTimeoutThread.Terminate;
begin
  inherited;
  SetEvent(FRequestEvent);
end;

procedure TOperationWithTimeoutThread.Execute;
var
  Operation: TOperation;
begin
  // Needed for various API, particularly:
  // - SHGetFileInfo fails to return icon index on some systems;
  // - ICustomDestinationList.BeginList returns invalid "removed" array.
  CoInitialize(nil);

  while WaitForSingleObject(FRequestEvent, INFINITE) = WAIT_OBJECT_0 do
  begin
    if Terminated then
    begin
      break;
    end
      else
    begin
      FCriticalSection.Enter;
      try
        Operation := TOperation(FQueue[0]);
        FQueue.Extract(Operation);
      finally
        FCriticalSection.Leave;
      end;

      Operation.OperationEvent(Operation);
      FResults.Add(Operation);
      SetEvent(FResultEvent);
    end;
  end;
end;

procedure TOperationWithTimeoutThread.Queue(Operation: TOperation);
begin
  FCriticalSection.Enter;
  try
    FQueue.Add(Operation);
  finally
    FCriticalSection.Leave;
  end;

  SetEvent(FRequestEvent);
end;

function TOperationWithTimeoutThread.WaitForOperation(Milliseconds: Cardinal): Boolean;
begin
  ResetEvent(FResultEvent);
  Result := (WaitForSingleObject(FResultEvent, Milliseconds) = WAIT_OBJECT_0);
end;

procedure TOperationWithTimeoutThread.Remove(Operation: TOperation);
begin
  FCriticalSection.Enter;
  try
    FResults.Remove(Operation);
  finally
    FCriticalSection.Leave;
  end;
end;

var
  Thread: TOperationWithTimeoutThread = nil;

procedure NeedThread;
begin
  if not Assigned(Thread) then
  begin
    Thread := TOperationWithTimeoutThread.Create;
  end;
end;

function WaitForOperation(Milliseconds: Cardinal): Boolean;
begin
  Result := Thread.WaitForOperation(Milliseconds);
  if not Result then
  begin
    // There's a chance for memory leak, if thread is terminated
    // between WaitFor() and this line
    Thread.FreeOnTerminate := True;
    Thread.Terminate;
    Thread := nil;
  end;
end;

procedure ShellFolderGetAttributesOfOperation(Operation: TOperation);
var
  ErrorMode: Word;
begin
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    Operation.ResultHResult := Operation.ShellFolder.GetAttributesOf(Operation.cidl, Operation.apidl, Operation.rgfInOut);
  except
    Operation.ResultHResult := E_FAIL;
  end;
  SetErrorMode(ErrorMode);
end;

function ShellFolderGetAttributesOfWithTimeout(
  ShellFolder: IShellFolder; cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT; Timeout: Integer): HResult;
var
  Operation: TOperation;
begin
  NeedThread;
  Operation := TOperation.Create(ShellFolderGetAttributesOfOperation);
  Operation.ShellFolder := ShellFolder;
  Operation.cidl := cidl;
  Operation.apidl := apidl;
  Operation.rgfInOut := rgfInOut;
  Thread.Queue(Operation);
  if WaitForOperation(Timeout) then
  begin
    apidl := Operation.apidl;
    rgfInOut := Operation.rgfInOut;
    Result := Operation.ResultHResult;
    Thread.Remove(Operation);
  end
    else
  begin
    rgfInOut := 0;
    Result := E_FAIL;
  end;
end;

procedure SHGetFileInfoOperation(Operation: TOperation);
var
  pszPath: LPCWSTR;
begin
  if Operation.uFlags and SHGFI_PIDL <> 0 then
  begin
    pszPath := LPCWSTR(Operation.PIDL);
  end
    else
  begin
    pszPath := LPCWSTR(Operation.Path);
  end;

  Operation.ResultDWordPtr :=
    SHGetFileInfo(pszPath, Operation.dwFileAttributes, Operation.psfi, Operation.cbFileInfo, Operation.uFlags);
end;

function SHGetFileInfoWithTimeout(
  pszPath: LPCWSTR; dwFileAttributes: DWORD; var psfi: TSHFileInfoW;
  cbFileInfo, uFlags: UINT; Timeout: Integer): DWORD_PTR;
var
  Operation: TOperation;
begin
  NeedThread;
  Operation := TOperation.Create(SHGetFileInfoOperation);
  if uFlags and SHGFI_PIDL <> 0 then
  begin
    Operation.PIDL := PItemIDList(pszPath);
  end
    else
  begin
    Operation.Path := pszPath;
  end;
  Operation.dwFileAttributes := dwFileAttributes;
  Operation.psfi := psfi;
  Operation.cbFileInfo := cbFileInfo;
  Operation.uFlags := uFlags;
  Thread.Queue(Operation);
  if WaitForOperation(Timeout) then
  begin
    psfi := Operation.psfi;
    Result := Operation.ResultDWordPtr;
    Thread.Remove(Operation);
  end
    else
  begin
    FillChar(psfi, SizeOf(psfi), 0);
    Result := 0;
  end;
end;

procedure ShellFolderParseDisplayNameOperation(Operation: TOperation);
begin
  Operation.ResultHResult :=
    Operation.ShellFolder.ParseDisplayName(
      Operation.hwndOwner, Operation.pbcReserved, PChar(Operation.DisplayName),
      Operation.pchEaten, Operation.ppidl, Operation.dwAttributes);
end;

function ShellFolderParseDisplayNameWithTimeout(
  ShellFolder: IShellFolder; hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR; out pchEaten: ULONG;
  out ppidl: PItemIDList; var dwAttributes: ULONG; Timeout: Integer): HResult;
var
  Operation: TOperation;
begin
  NeedThread;
  Operation := TOperation.Create(ShellFolderParseDisplayNameOperation);
  Operation.ShellFolder := ShellFolder;
  Operation.hwndOwner := hwndOwner;
  Operation.pbcReserved := pbcReserved;
  Operation.DisplayName := lpszDisplayName;
  Operation.pchEaten := 0;
  Operation.ppidl := nil;
  Operation.dwAttributes := 0;
  Thread.Queue(Operation);
  if WaitForOperation(Timeout) then
  begin
    ppidl := Operation.ppidl;
    dwAttributes := Operation.dwAttributes;
    Result := Operation.ResultHResult;
    Thread.Remove(Operation);
  end
    else
  begin
    ppidl := nil;
    dwAttributes := 0;
    Result := E_FAIL;
  end;
end;

initialization

finalization
  if Assigned(Thread) then
  begin
    Thread.Free;
  end;
end.
