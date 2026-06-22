unit OperationWithTimeout;

interface

uses
  Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI, ActiveX;

function SHGetFileInfoWithTimeout(
  pszPath: LPCWSTR; dwFileAttributes: DWORD; var psfi: TSHFileInfoW;
  cbFileInfo, uFlags: UINT; Timeout: Integer): DWORD_PTR;

function ShellFolderParseDisplayNameWithTimeout(
  ShellFolder: IShellFolder; hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR;
  out pchEaten: ULONG; out ppidl: PItemIDList; var dwAttributes: ULONG; Timeout: Integer): HResult;

function DestinationListBeginList(
  DestinationList: ICustomDestinationList; var pcMaxSlots: UINT; const riid: TIID; out ppv: Pointer; Timeout: Integer): HRESULT;

var
  TimeoutShellOperations: Boolean = True;

implementation

uses
  System.Classes, System.Types, System.SysUtils, CompThread;

type
  TOperation = class;

  TOperationEvent = procedure(Operation: TOperation);

  TOperation = class(TObject)
  public
    // SHGetFileInfoWithTimeout
    PIDL: PItemIDList;
    Path: string;
    dwFileAttributes: DWORD;
    psfi: TSHFileInfoW;
    cbFileInfo, uFlags: UINT;
    ResultDWordPtr: DWORD_PTR;

    // ShellFolderParseDisplayNameWithTimeout
    ShellFolder: IShellFolder;
    ResultHResult: HResult;
    hwndOwner: HWND;
    pbcReserved: Pointer;
    DisplayName: string;
    pchEaten: ULONG;
    ppidl: PItemIDList;
    dwAttributes: ULONG;

    // DestinationListBeginList uses ResultHResult
    DestinationList: ICustomDestinationList;
    pcMaxSlots: UINT;
    riid: TIID;
    ppv: Pointer;
  end;

type
  TOperationWithTimeoutThread = class(TCompThread)
  public
    constructor Create(Operation: TOperation; OperationEvent: TOperationEvent);

  protected
    procedure Execute; override;

  private
    FOperation: TOperation;
    FOperationEvent: TOperationEvent;
  end;

constructor TOperationWithTimeoutThread.Create(Operation: TOperation; OperationEvent: TOperationEvent);
begin
  inherited Create(True);
  FOperation := Operation;
  FOperationEvent := OperationEvent;
end;

procedure TOperationWithTimeoutThread.Execute;
begin
  // Needed for various API, particularly:
  // - SHGetFileInfo fails to return icon index on some systems;
  // - ICustomDestinationList.BeginList returns invalid "removed" array.
  CoInitialize(nil);
  FOperationEvent(FOperation);
end;

function WaitForOperation(
  Operation: TOperation; OperationEvent: TOperationEvent; Milliseconds: Cardinal): Boolean;
{$IFNDEF IDE}
var
  Thread: TOperationWithTimeoutThread;
{$ENDIF}
begin
// When running from IDE, it triggers starting/exiting the thread taking ages.
// So in IDE we revert to single-thread approach.
{$IFNDEF IDE}
  if not TimeoutShellOperations then
{$ENDIF}
  begin
    OperationEvent(Operation);
    Result := True;
  end
{$IFNDEF IDE}
    else
  begin
    // Have to start new thread for each request. When shared thread is used, it eventually hangs.
    // Most probably do to the fact that we violate COM threading model.
    // So using a new thread for each request, is only a hack that happens to work by pure luck.
    // We may want to use shared thread at least for COM-free operations, like SHGetFileInfo.
    Thread := TOperationWithTimeoutThread.Create(Operation, OperationEvent);
    Thread.Resume;
    Result := Thread.WaitFor(MSecsPerSec);
    if Result then
    begin
      Thread.Free;
    end
      else
    begin
      // There's a chance for memory leak, if thread is terminated
      // between WaitFor() and this line
      Thread.FreeOnTerminate := True;
    end;
  end;
{$ENDIF}
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
  Operation := TOperation.Create;
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
  if WaitForOperation(Operation, SHGetFileInfoOperation, Timeout) then
  begin
    psfi := Operation.psfi;
    Result := Operation.ResultDWordPtr;
    Operation.Free;
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
  Operation := TOperation.Create;
  Operation.ShellFolder := ShellFolder;
  Operation.hwndOwner := hwndOwner;
  Operation.pbcReserved := pbcReserved;
  Operation.DisplayName := lpszDisplayName;
  Operation.pchEaten := 0;
  Operation.ppidl := nil;
  Operation.dwAttributes := 0;
  if WaitForOperation(Operation, ShellFolderParseDisplayNameOperation, Timeout) then
  begin
    ppidl := Operation.ppidl;
    dwAttributes := Operation.dwAttributes;
    Result := Operation.ResultHResult;
    Operation.Free;
  end
    else
  begin
    ppidl := nil;
    dwAttributes := 0;
    Result := E_FAIL;
  end;
end;

procedure DestinationListBeginListOperation(Operation: TOperation);
begin
  Operation.ResultHResult := Operation.DestinationList.BeginList(Operation.pcMaxSlots, Operation.riid, Operation.ppv);
end;

function DestinationListBeginList(
  DestinationList: ICustomDestinationList; var pcMaxSlots: UINT; const riid: TIID; out ppv: Pointer; Timeout: Integer): HRESULT;
var
  Operation: TOperation;
begin
  Operation := TOperation.Create;
  Operation.DestinationList := DestinationList;
  Operation.pcMaxSlots := pcMaxSlots;
  Operation.riid := riid;
  Operation.ppv := ppv;
  if WaitForOperation(Operation, DestinationListBeginListOperation, Timeout) then
  begin
    pcMaxSlots := Operation.pcMaxSlots;
    ppv := Operation.ppv;
    Result := Operation.ResultHResult;
    Operation.Free;
  end
    else
  begin
    ppv := nil;
    Result := E_FAIL;
  end;
end;

end.
