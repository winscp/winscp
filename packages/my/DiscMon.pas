{

Modifications:
==============

 ie01: OnChange delayed for 500 ms.
 ie02: No usage of BrowseDirectory component
 ie03: Resume suspended thread when terminating the thread

 ie04: Martin Prikryl, option to watch multiple directories

 }


// ==================== DISC DRIVE MONITOR =====================================
//
// Class and Component to encapsulate the FindXXXXChangeNotification API calls
//
// The FindXXXXChangeNotification API calls set up a disc contents change
// notification handle.  You can set a filter to control which change types
// are notified, the directory which is monitored and set whether subdirectories
// from the monitored directory are monitored as well.
//
//------------------------------------------------------------------------------
// This file contains a class derived from TThread which undertakes the disc
// monitoring and a simple component which encapsulates the thread to make
// a non-visual VCL component.  This component works at design time, monitoring
// and notifying changes live if required.
//
// Version 1.00 - Grahame Marsh 14 January 1997
// Version 1.01 - Grahame Marsh 30 December 1997
//      Bug fix - really a Win 95 bug but only surfaces in D3, not D2
//              - see notes in execute method
// Version 1.02 - Grahame Marsh 30 January 1998
//              - adapted to work with version 2.30 TBrowseDirectoryDlg

//
// Freeware - you get it for free, I take nothing, I make no promises!
//
// Please feel free to contact me: grahame.s.marsh@courtaulds.com

{$DEFINE BUGFIX}

unit DiscMon;

interface

uses
  Windows, SysUtils, Classes, CompThread;

//=== DISC MONITORING THREAD ===================================================
// This thread will monitor a given directory and subdirectories (if required)
// for defined filtered changes.  When a change occurs the OnChange event will
// be fired, if an invalid condition is found (eg non-existent path) then
// the OnInvalid event is fired. Each event is called via the Sychronize method
// and so are VCL thread safe.
//
// The thread is created suspended, so after setting the required properties
// you must call the Resume method.

type
  TDiscMonitorNotify = procedure(Sender: TObject; const Directory: string) of object;
  TDiscMonitorSynchronize = procedure(Sender: TObject; Method: TThreadMethod) of object;

  TDiscMonitorThread = class(TCompThread)
  private
    FOnChange: TDiscMonitorNotify;
    FOnInvalid: TDiscMonitorNotify;
    FOnSynchronize: TDiscMonitorSynchronize;
    FDirectories: TStrings;
    FFilters: DWORD;
    FDestroyEvent,
    FChangeEvent: THandle;
    FSubTree: Boolean;
    FChangeDelay: Integer;  {ie01}
    FNotifiedDirectory: string;
    procedure InformChange;
    procedure InformInvalid;
    procedure SetDirectories(const Value: TStrings);
    procedure SetFilters(Value: DWORD);
    procedure SetSubTree(Value: boolean);
    function GetMaxDirectories: Integer;
  protected
    procedure Execute; override;
    procedure Update;
    procedure DoSynchronize(Method: TThreadMethod);
  public
    constructor Create;
    destructor Destroy; override;
    property MaxDirectories: Integer read GetMaxDirectories;
    // The directory to monitor
    property Directories: TStrings read FDirectories write SetDirectories;
    // Filter condition, may be any of the FILE_NOTIFY_CHANGE_XXXXXXX constants
    // ORed together.  Zero is invalid.
    property Filters: DWORD read FFilters write SetFilters;
    // Event called when change noted in directory
    property OnChange: TDiscMonitorNotify read FOnChange write FOnChange;
    // Event called for invalid parameters
    property OnInvalid: TDiscMonitorNotify read FOnInvalid write FOnInvalid;
    property OnSynchronize: TDiscMonitorSynchronize read FOnSynchronize write FOnSynchronize;
    // Include subdirectories below specified directory.
    property SubTree: Boolean read FSubTree write SetSubTree;
    // specify, how long the thread should wait, before the event OnChange is fired:
    property ChangeDelay: Integer read FChangeDelay write FChangeDelay
      default 500;
  end;

  //===================== DISC MONITORING COMPONENT ==============================

  // enumerated type for filter conditions (not directly usable in thread class)
  // see the SetFilters procedure for the translation of these filter conditions
  // into FILE_NOTIFY_CHANGE_XXXXXX constants.
  TMonitorFilter = (moFilename, moDirName, moAttributes, moSize,
                    moLastWrite, moSecurity);

  // set of filter conditions
  TMonitorFilters = set of TMonitorFilter;

  TDiscMonitor = class(TComponent)
  private
    FActive: Boolean;
    FMonitor: TDiscMonitorThread;
    FFilters: TMonitorFilters;
    FOnChange: TDiscMonitorNotify;
    FOnInvalid: TDiscMonitorNotify;
    FOnSynchronize: TDiscMonitorSynchronize;
    FShowMsg: Boolean;
    FPending: Boolean;
    function GetDirectories: TStrings;
    function GetSubTree: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetDirectories(Value: TStrings);
    procedure SetFilters(Value: TMonitorFilters);
    procedure SetSubTree(Value: Boolean);
    function  GetChangeDelay: Integer;
    procedure SetChangeDelay(Value: Integer);
    procedure AddDirectory(Dirs: TStrings; Directory: string); overload;
    function GetMaxDirectories: Integer;
  protected
    procedure Change(Sender: TObject; const Directory: string);
    procedure Invalid(Sender: TObject; const Directory: string);
    procedure DoSynchronize(Sender: TObject; Method: TThreadMethod);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    // stop the monitoring thread running
    procedure Close;
    // start the monitoring thread running
    procedure Open;
    procedure AddDirectory(Directory: string; SubDirs: Boolean); overload;
    procedure SetDirectory(Directory: string);
    // read-only property to access the thread directly
    property Thread: TDiscMonitorThread read FMonitor;
    property MaxDirectories: Integer read GetMaxDirectories;
  published
    // the directories to monitor
    property Directories: TStrings read GetDirectories write SetDirectories;
    // control the appearance of information messages at design time (only)
    property ShowDesignMsg: Boolean read FShowMsg write FShowMsg default False;
    // event called when a change is notified
    property OnChange: TDiscMonitorNotify read FOnChange write FOnChange;
    // event called if an invalid condition is found
    property OnInvalid: TDiscMonitorNotify read FOnInvalid write FOnInvalid;
    property OnSynchronize: TDiscMonitorSynchronize read FOnSynchronize write FOnSynchronize;
    // notification filter conditions
    property Filters: TMonitorFilters read FFilters write SetFilters default [moFilename];
    // include subdirectories below the specified directory
    property SubTree: Boolean read GetSubTree write SetSubTree default True;
    // specify if the monitoring thread is active
    property Active: Boolean read FActive write SetActive default False;
    // specify, how long the thread should wait, before the event OnChange is fired:
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay
      default 500;
  end;

procedure Register;

resourcestring
  STooManyWatchDirectories = 'Cannot watch for changes in more then %d directories and subdirectories.';

implementation

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF BUGFIX}
{$Z4}
type TWinBool = (winFalse, winTrue);
function FixFindFirstChangeNotification(const lpPathName: PChar;
  bWatchSubtree: TWinBool; dwNotifyFilter: DWORD): THandle stdcall;
  external kernel32 name 'FindFirstChangeNotificationA';
{$ENDIF}

//=== MONITOR THREAD ===========================================================

// Create the thread suspended.  Create two events, each are created using
// standard security, in the non-signalled state, with auto-reset and without
// names.  The FDestroyEvent will be used to signal the thread that it is to close
// down.  The FChangeEvent will be used to signal the thread when the monitoring
// conditions (directory, filters or sub-directory search) have changed.
// OnTerminate is left as false, so the user must Free the thread.

constructor TDiscMonitorThread.Create;
begin
  inherited Create(True);
  FDirectories := TStringList.Create;
  FDestroyEvent := CreateEvent(nil, True,  False, nil);
  FChangeEvent  := CreateEvent(nil, False, False, nil);
end;

// close OnXXXXX links, signal the thread that it is to close down
destructor TDiscMonitorThread.Destroy;
var
  D: TStrings;
begin
  FOnChange := nil;
  FOnInvalid := nil;
  if Suspended then Resume;
  SetEvent(FDestroyEvent);
  D := FDirectories;
  inherited Destroy;
  // cannot free before destroy as the thread is using it
  D.Free;
end;

// called by the Execute procedure via Synchronize.  So this is VCL thread safe
procedure TDiscMonitorThread.InformChange;
begin
  if Assigned(FOnChange) then FOnChange(Self, FNotifiedDirectory);
end;

// called by the Execute procedure via Synchronize.  So this is VCL thread safe
procedure TDiscMonitorThread.InformInvalid;
begin
  if Assigned(FOnInvalid) then FOnInvalid(Self, FNotifiedDirectory);
end;

function TDiscMonitorThread.GetMaxDirectories: Integer;
begin
  Result := MAXIMUM_WAIT_OBJECTS - 2;
end;

// Change the current directory
procedure TDiscMonitorThread.SetDirectories(const Value: TStrings);
begin
  if Value <> nil then
  begin
    if Value.Count > MaxDirectories then
      raise Exception.CreateFmt(STooManyWatchDirectories, [MaxDirectories]);

    FDirectories.Assign(Value)
  end
    else FDirectories.Clear;
  Update;
end;

// Change the current filters
procedure TDiscMonitorThread.SetFilters(Value: DWORD);
begin
  if Value <> FFilters then
  begin
    FFilters := Value;
    Update;
  end
end;

// Change the current sub-tree condition
procedure TDiscMonitorThread.SetSubTree(Value: Boolean);
begin
  if Value <> FSubTree then
  begin
    FSubtree := Value;
    Update;
  end
end;

function TDiscMonitor.GetChangeDelay: Integer;  {ie01}
begin
  Result := FMonitor.ChangeDelay;
end;

procedure TDiscMonitor.SetChangeDelay(Value: Integer);  {ie01}
begin
  FMonitor.ChangeDelay := Value;
end;

// On any of the above three changes, if the thread is running then
// signal it that a change has occurred.
procedure TDiscMonitorThread.Update;
begin
  if not Suspended then
    SetEvent(FChangeEvent)
end;

procedure TDiscMonitorThread.DoSynchronize(Method: TThreadMethod);
begin
  if Assigned(OnSynchronize) then OnSynchronize(Self, Method);
end;

// The EXECUTE procedure
//     -------
// Execute needs to:
// 1. Call FindFirstChangeNotification and use the Handle in a WaitFor...
//    to wait until the thread become signalled that a notification has occurred.
//    The OnChange event is called and then the FindNextChangeNotification is
//    the called and Execute loops back to the WaitFor
// 2. If an invalid handle is obtained from the above call, the the OnInvalid
//    event is called and then Execute waits until valid conditions are set.
// 3. If a ChangeEvent is signalled then FindCloseChangeNotification is called,
//    followed by a new FindFirstChangeNotification to use the altered
//    conditions.
// 4. If a DestroyEvent is signalled then FindCloseChangeNotification is
//    called and the two events are closed and the thread terminates.
//
// In practice WaitForMultipleObjects is used to wait for any of the conditions
// to be signalled, and the returned value used to determine which event occurred.

procedure TDiscMonitorThread.Execute;
type
  THandles = array[0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
  PHandles = ^THandles;
var
  // used to give the handles to WaitFor...
  Handles: PHandles;

  function StartMonitor(const Directory: string): THandle;
  // There appears to be a bug in win 95 where the bWatchSubTree parameter
  // of FindFirstChangeNotification which is a BOOL only accepts values of
  // 0 and 1 as valid, rather than 0 and any non-0 value as it should.  In D2
  // BOOL was defined as 0..1 so the code worked, in D3 it is 0..-1 so
  // fails. The result is FindF... produces and error message.  This fix (bodge) is
  // needed to produce a 0,1 bool pair, rather that 0,-1 as declared in D3
  {$IFDEF BUGFIX}
  const R : array[False..True] of TWinBOOL = (WinFalse, WinTrue);
  {$ELSE}
  const R : array[False..True] of LongBool = (LongBool(0), LongBool(1));
  {$ENDIF}
  var
    Again: Boolean;
  begin
    repeat
      {$IFDEF BUGFIX}
      Result := FixFindFirstChangeNotification(PChar(Directory), R[FSubTree], FFilters);
      {$ELSE}
      Result := FindFirstChangeNotification(PChar(Directory), R[FSubTree], FFilters);
      {$ENDIF}

      Again := (Result = INVALID_HANDLE_VALUE);
      if Again then
      begin
        // call the OnInvalid event
        FNotifiedDirectory := Directory;
        DoSynchronize(InformInvalid);

        // wait until either DestroyEvent or the ChangeEvents are signalled
        Result := WaitForMultipleObjects(2, PWOHandleArray(Handles), False, INFINITE);
        if Result = WAIT_FAILED then
        begin
          FNotifiedDirectory := '';
          DoSynchronize(InformInvalid);
          Again := False;
        end
          else Again := (Result - WAIT_OBJECT_0 = 1);
      end

    until (not Again);
  end; {StartMonitor}

var
  Count: Word;
  I: Integer;
  Result: DWORD;
begin {Execute}
  Count := 2 + FDirectories.Count;
  GetMem(Handles, sizeof(THandle) * Count);
  try
    Handles^[0] := FDestroyEvent;      // put DestroyEvent handle in slot 0
    Handles^[1] := FChangeEvent;       // put ChangeEvent handle in slot 1

    repeat

      for I := 2 to Count - 1 do
      begin
        Handles^[I] := StartMonitor(FDirectories[I - 2]);
        if Handles^[I] = INVALID_HANDLE_VALUE then Exit;
      end;

      repeat
        // handle is valid so wait for any of the change notification, destroy or
        // change events to be signalled
        Result := WaitForMultipleObjects(Count, PWOHandleArray(Handles),
          False, INFINITE);

        if Result = WAIT_FAILED then
        begin
          FNotifiedDirectory := '';
          DoSynchronize(InformInvalid);
        end
          else
        if (Result - WAIT_OBJECT_0 >= 2) and
           (Result - WAIT_OBJECT_0 < Count) then
        begin
          // Notification signalled, so fire the OnChange event and then FindNext..
          // loop back to re-WaitFor... the thread
          Sleep(FChangeDelay);
          FNotifiedDirectory := FDirectories[Result - WAIT_OBJECT_0 - 2];
          DoSynchronize(InformChange);
          FindNextChangeNotification(Handles^[Result - WAIT_OBJECT_0])
        end;
      until (Result = WAIT_FAILED) or (Result - WAIT_OBJECT_0 < 2) or
        (Result - WAIT_OBJECT_0 >= Count);

      for I := 2 to Count - 1 do
      begin
        FindCloseChangeNotification(Handles^[I]);
      end;

      // loop back to restart if ChangeEvent was signalled
    until (Result - WAIT_OBJECT_0 <> 1) or Self.Terminated;

    // closing down so chuck the two events
    CloseHandle(FChangeEvent);
    CloseHandle(FDestroyEvent)
  finally
    FreeMem(Handles);
  end;
end;

//=== MONITOR COMPONENT ========================================================

// This component encapsulates the above thread.  It has properties for
// directory, sub-directory conditions, filters, whether information messages
// should be given at design time and if the thread is active.
constructor TDiscMonitor.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FMonitor := TDiscMonitorThread.Create;  // create a monitor thread
  FMonitor.ChangeDelay := 500;            {ie01}
  FMonitor.OnChange := Change;            // hook into its event handlers
  FMonitor.OnInvalid := Invalid;
  FMonitor.OnSynchronize := DoSynchronize;
  Filters := [moFilename];                // default filters to moFilename
  SubTree := True;                        // default sub-tree search to on
  FPending := True;
end;

destructor TDiscMonitor.Destroy;
begin
  FMonitor.Free;                          // chuck the thread
  inherited Destroy;
end;

// Change notification from the thread has occurred. Call the component's event
// handler and then, if in design mode, and if desired, put up a simple
// notification message
procedure TDiscMonitor.Change(Sender: TObject; const Directory: string);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Directory)
end;

// Invalid notification from the thread has occurred. Call the component's event
// handler and then, if in design mode, and if desired, put up a simple
// notification message
procedure TDiscMonitor.Invalid(Sender: TObject; const Directory: string);
begin
  if Assigned(FOnInvalid) then
    FOnInvalid(Self, Directory)
end;

procedure TDiscMonitor.DoSynchronize(Sender: TObject; Method: TThreadMethod);
begin
  if Assigned(FOnSynchronize) then FOnSynchronize(Self, Method)
    else FMonitor.Synchronize(Method);
end;

// Stop the monitor running
procedure TDiscMonitor.Close;
begin
  Active := False;
end;

// Run the monitor
procedure TDiscMonitor.Open;
begin
  Active := True
end;

function TDiscMonitor.GetMaxDirectories: Integer;
begin
  Result := FMonitor.MaxDirectories;
end;

// Control the thread by using it's resume and suspend methods
procedure TDiscMonitor.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if Active then
    begin
      FMonitor.Resume;
      if not FPending then FMonitor.Update;
      FPending := False;
    end else
      FMonitor.Suspend;
  end
end;

// get the current directory from the thread
function TDiscMonitor.GetDirectories: TStrings;
begin
  Result := FMonitor.Directories;
end;

// get the current sub-tree status from the thread
function TDiscMonitor.GetSubTree: Boolean;
begin
  Result := FMonitor.SubTree;
end;

// set the directory to monitor
procedure TDiscMonitor.SetDirectories(Value: TStrings);
begin
  FMonitor.Directories := Value;
end;

procedure TDiscMonitor.AddDirectory(Dirs: TStrings; Directory: string);
var
  Found: Boolean;
  SearchRec: TSearchRec;
  FileName: string;
  FindAttrs: Integer;
begin
  FindAttrs := faReadOnly or faHidden or faSysFile or faDirectory or faArchive;
  Directory := IncludeTrailingBackslash(Directory);
  Found := (FindFirst(Directory + '*.*', FindAttrs, SearchRec) = 0);

  if Found then
  begin
    try
      while Found do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
        begin
          FileName := Directory + SearchRec.Name;
          Dirs.Add(FileName);
          if Dirs.Count > MaxDirectories then
            raise Exception.CreateFmt(STooManyWatchDirectories, [MaxDirectories]);
          AddDirectory(Dirs, FileName);
        end;

        Found := (FindNext(SearchRec) = 0);
      end;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TDiscMonitor.AddDirectory(Directory: string; SubDirs: Boolean);
var
  Dirs: TStringList;
begin
  if Directory <> '' then
  begin
    Dirs := TStringList.Create;

    try

      Dirs.Assign(Directories);
      
      Dirs.Add(Directory);
      if SubDirs then AddDirectory(Dirs, Directory);

      Directories := Dirs;

    finally
      Dirs.Free;
    end;
  end
    else
  begin
    Directories := nil;
  end;
end;

procedure TDiscMonitor.SetDirectory(Directory: string);
var
  Dirs: TStringList;
begin
  if Directory <> '' then
  begin
    Dirs := TStringList.Create;
    try
      Dirs.Add(Directory);

      Directories := Dirs;

    finally
      Dirs.Free;
    end;
  end
    else
  begin
    Directories := nil;
  end;
end;

// Change the filter conditions.  The thread uses the raw windows constants
// (FILE_NOTIFY_CHANGE_XXXX) but the components uses a set of enumurated type.
// It is therefore necessary to translate from the component format into
// an integer value for the thread.
procedure TDiscMonitor.SetFilters(Value: TMonitorFilters);
const
  XlatFileNotify: array [moFilename..moSecurity] of DWORD =
    (FILE_NOTIFY_CHANGE_FILE_NAME,  FILE_NOTIFY_CHANGE_DIR_NAME,
     FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
     FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY);
var
  L: TMonitorFilter;
  I: DWORD;
begin
  if Value <> FFilters then
    if Value = [] then
      raise Exception.Create('Some filter condition must be set.')
    else begin
      FFilters := Value;
      I := 0;
      for L := moFilename to moSecurity do
        if L in Value then
          I := I or XlatFileNotify [L];
      FMonitor.Filters := I;
    end
end;

// set the sub-tree status in the thread
procedure TDiscMonitor.SetSubTree(Value: Boolean);
begin
  FMonitor.SubTree := Value;
end;

procedure Register;
begin
  RegisterComponents('Martin', [TDiscMonitor]);
end;

end.


