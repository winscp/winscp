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
  TDiscMonitorNotify = procedure(Sender: TObject; const Directory: string; var SubdirsChanged: Boolean) of object;
  TDiscMonitorInvalid = procedure(Sender: TObject; const Directory: string; const ErrorStr: string) of object;
  TDiscMonitorSynchronize = procedure(Sender: TObject; Method: TThreadMethod) of object;
  TDiscMonitorFilter = procedure(Sender: TObject; const DirectoryName: string; var Add: Boolean) of object;
  TDiscMonitorTooManyDirectories = procedure(Sender: TObject; var MaxDirectories: Integer) of object;
  TDiscMonitorDirectoriesChange = procedure(Sender: TObject; Directories: Integer) of object;

  TDiscMonitorThread = class(TCompThread)
  private
    FOnChange: TDiscMonitorNotify;
    FOnInvalid: TDiscMonitorInvalid;
    FOnSynchronize: TDiscMonitorSynchronize;
    FOnFilter: TDiscMonitorFilter;
    FOnDirectoriesChange: TDiscMonitorDirectoriesChange;
    FDirectories: TStrings;
    FFilters: DWORD;
    FDestroyEvent,
    FChangeEvent,
    FChangedEvent: THandle;
    FSubTree: Boolean;
    FChangeDelay: Integer;  {ie01}
    FNotifiedDirectory: string;
    FSubdirsChanged: Boolean;
    FInvalidMessage: string;
    FNotifiedDirectories: Integer;
    FEnabled: Boolean;
    procedure InformChange;
    procedure InformInvalid;
    procedure InformDirectoriesChange;
    procedure SetDirectories(const Value: TStrings);
    procedure SetFilters(Value: DWORD);
    procedure SetSubTree(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SaveOSError;
  protected
    procedure Execute; override;
    procedure Update;
    procedure DoSynchronize(Method: TThreadMethod);
  public
    constructor Create;
    destructor Destroy; override;
    // The directory to monitor
    property Directories: TStrings read FDirectories write SetDirectories;
    // Filter condition, may be any of the FILE_NOTIFY_CHANGE_XXXXXXX constants
    // ORed together.  Zero is invalid.
    property Filters: DWORD read FFilters write SetFilters;
    // Event called when change noted in directory
    property OnChange: TDiscMonitorNotify read FOnChange write FOnChange;
    // Event called for invalid parameters
    property OnInvalid: TDiscMonitorInvalid read FOnInvalid write FOnInvalid;
    property OnSynchronize: TDiscMonitorSynchronize read FOnSynchronize write FOnSynchronize;
    property OnFilter: TDiscMonitorFilter read FOnFilter write FOnFilter;
    property OnDirectoriesChange: TDiscMonitorDirectoriesChange read FOnDirectoriesChange write FOnDirectoriesChange;
    // Include subdirectories below specified directory.
    property SubTree: Boolean read FSubTree write SetSubTree;
    // specify, how long the thread should wait, before the event OnChange is fired:
    property ChangeDelay: Integer read FChangeDelay write FChangeDelay
      default 500;
    property Enabled: Boolean read FEnabled write SetEnabled;
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
    FOnInvalid: TDiscMonitorInvalid;
    FOnSynchronize: TDiscMonitorSynchronize;
    FOnFilter: TDiscMonitorFilter;
    FOnTooManyDirectories: TDiscMonitorTooManyDirectories;
    FOnDirectoriesChange: TDiscMonitorDirectoriesChange;

    FShowMsg: Boolean;
    FPending: Boolean;
    FMaxDirectories: Integer;
    function GetDirectories: TStrings;
    function GetSubTree: Boolean;
    function GetEnabled: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetDirectories(Value: TStrings);
    procedure SetFilters(Value: TMonitorFilters);
    procedure SetSubTree(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    function  GetChangeDelay: Integer;
    procedure SetChangeDelay(Value: Integer);
  protected
    procedure Change(Sender: TObject; const Directory: string;
      var SubdirsChanged: Boolean);
    procedure Invalid(Sender: TObject; const Directory: string; const ErrorStr: string);
    procedure Filter(Sender: TObject; const DirectoryName: string; var Add: Boolean);
    procedure DirectoriesChange(Sender: TObject; Directories: Integer);
    procedure DoSynchronize(Sender: TObject; Method: TThreadMethod);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    // stop the monitoring thread running
    procedure Close;
    // start the monitoring thread running
    procedure Open;
    procedure AddDirectory(Directory: string; SubDirs: Boolean);
    procedure SetDirectory(Directory: string);
    // read-only property to access the thread directly
    property Thread: TDiscMonitorThread read FMonitor;
    property MaxDirectories: Integer read FMaxDirectories write FMaxDirectories;
  published
    // the directories to monitor
    property Directories: TStrings read GetDirectories;
    // control the appearance of information messages at design time (only)
    property ShowDesignMsg: Boolean read FShowMsg write FShowMsg default False;
    // event called when a change is notified
    property OnChange: TDiscMonitorNotify read FOnChange write FOnChange;
    // event called if an invalid condition is found
    property OnInvalid: TDiscMonitorInvalid read FOnInvalid write FOnInvalid;
    property OnSynchronize: TDiscMonitorSynchronize read FOnSynchronize write FOnSynchronize;
    property OnFilter: TDiscMonitorFilter read FOnFilter write FOnFilter;
    property OnTooManyDirectories: TDiscMonitorTooManyDirectories read FOnTooManyDirectories write FOnTooManyDirectories;
    property OnDirectoriesChange: TDiscMonitorDirectoriesChange read FOnDirectoriesChange write FOnDirectoriesChange;

    // notification filter conditions
    property Filters: TMonitorFilters read FFilters write SetFilters default [moFilename];
    // include subdirectories below the specified directory
    property SubTree: Boolean read GetSubTree write SetSubTree default True;
    // specify if the monitoring thread is active
    property Active: Boolean read FActive write SetActive default False;
    // specify, how long the thread should wait, before the event OnChange is fired:
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay
      default 500;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
  end;

procedure Register;

implementation

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF BUGFIX}
{$Z4}
type TWinBool = (winFalse, winTrue);
function FixFindFirstChangeNotification(const lpPathName: PChar;
  bWatchSubtree: TWinBool; dwNotifyFilter: DWORD): THandle stdcall;
  external kernel32 name 'FindFirstChangeNotificationW';
{$ENDIF}

procedure AddDirectory(Dirs: TStrings; Directory: string;
  var MaxDirectories: Integer; OnFilter: TDiscMonitorFilter;
  OnTooManyDirectories: TDiscMonitorTooManyDirectories; Tag: Boolean);
var
  Found: Boolean;
  SearchRec: TSearchRec;
  FileName: string;
  FindAttrs: Integer;
  Add: Boolean;
  Index: Integer;
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
          Add := True;
          if Assigned(OnFilter) then OnFilter(nil, FileName, Add);

          if Add then
          begin
            if Tag then
            begin
              Index := Dirs.IndexOf(FileName);
              if Index >= 0 then
              begin
                Assert(Dirs.Objects[Index] = TObject(1));
                Dirs.Objects[Index] := TObject(0);
              end;
            end
              else Index := -1;

            if Index < 0 then
            begin
              if Tag then Dirs.AddObject(FileName, TObject(2))
                else Dirs.Add(FileName);
              if (MaxDirectories >= 0) and (Dirs.Count > MaxDirectories) and
                 Assigned(OnTooManyDirectories) then
                   OnTooManyDirectories(nil, MaxDirectories);
            end;
            // note that we are not re-scaning subdirectories of duplicate directory
            AddDirectory(Dirs, FileName, MaxDirectories, OnFilter, OnTooManyDirectories, Tag);
          end;
        end;

        Found := (FindNext(SearchRec) = 0);
      end;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

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
  (FDirectories as TStringList).CaseSensitive := False;
  (FDirectories as TStringList).Sorted := True;
  FDestroyEvent := CreateEvent(nil, True,  False, nil);
  FChangeEvent  := CreateEvent(nil, False, False, nil);
  FChangedEvent := CreateEvent(nil, False, False, nil);
  FOnFilter := nil;
  FOnDirectoriesChange := nil;
  FEnabled := True;
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
  if Assigned(FOnChange) then FOnChange(Self, FNotifiedDirectory, FSubdirsChanged);
end;

// called by the Execute procedure via Synchronize.  So this is VCL thread safe
procedure TDiscMonitorThread.InformInvalid;
begin
  if Assigned(FOnInvalid) then FOnInvalid(Self, FNotifiedDirectory, FInvalidMessage);
end;

procedure TDiscMonitorThread.InformDirectoriesChange;
begin
  if Assigned(FOnDirectoriesChange) then FOnDirectoriesChange(Self, FNotifiedDirectories);
end;

procedure TDiscMonitorThread.SaveOSError;
begin
  try
    RaiseLastOSError;
  except
    on E: Exception do FInvalidMessage := E.Message;
  end;
end;

procedure TDiscMonitorThread.SetDirectories(const Value: TStrings);
begin
  if Value <> nil then FDirectories.Assign(Value)
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

procedure TDiscMonitorThread.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
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
  begin
    ResetEvent(FChangedEvent);
    SetEvent(FChangeEvent);
    WaitForSingleObject(FChangedEvent, INFINITE);
  end;
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
var
  // used to give the handles to WaitFor...
  Handles: PWOHandleArray;
  BaseHandles: Word;
  SysHandles: Word;
  Count: Word;

  function StartMonitor(const Directory: string; ForceSubTree: Boolean): THandle;
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
    SubTree: Boolean;
  begin
    repeat
      SubTree := FSubTree or ForceSubTree;
      {$IFDEF BUGFIX}
      Result := FixFindFirstChangeNotification(PChar(Directory), R[SubTree], FFilters);
      {$ELSE}
      Result := FindFirstChangeNotification(PChar(Directory), R[SubTree], FFilters);
      {$ENDIF}

      Again := (Result = INVALID_HANDLE_VALUE);
      if Again then
      begin
        // call the OnInvalid event
        FNotifiedDirectory := Directory;
        SaveOSError;
        DoSynchronize(InformInvalid);

        // wait until either DestroyEvent or the ChangeEvents are signalled
        Result := WaitForMultipleObjects(2, Handles, False, INFINITE);
        if Result = WAIT_FAILED then
        begin
          FNotifiedDirectory := '';
          SaveOSError;
          DoSynchronize(InformInvalid);
          Again := False;
        end
          else Again := (Result - WAIT_OBJECT_0 = 1);
      end

    until (not Again);
  end; {StartMonitor}

  function UpdateSubdirectories(Directory: Integer): Cardinal;
  var
    Path: string;
    NewHandles: PWOHandleArray;
    OrigDirectory: Integer;
    NewAlloc: Integer;
    MaxDirectories: Integer;
    Changed: Boolean;
  begin
    Assert(Directory >= 0);
    Path := ExcludeTrailingBackslash(FDirectories[Directory]);

    while ((Directory + 1 < FDirectories.Count) and
           SameText(Copy(FDirectories.Strings[Directory + 1], 1, Length(Path)), Path)) do
    begin
      FDirectories.Objects[Directory + 1] := TObject(1);
      Inc(Directory);
    end;

    MaxDirectories := -1;
    AddDirectory(FDirectories, Path, MaxDirectories, OnFilter, nil, True);

    Result := WAIT_OBJECT_0;

    // worst case limit
    NewAlloc := SysHandles + FDirectories.Count;
    GetMem(NewHandles, SizeOf(THandle) * NewAlloc);
    Move(Handles^, NewHandles^, SizeOf(THandle) * SysHandles);

    OrigDirectory := SysHandles;
    Directory := SysHandles;
    Changed := False;

    while Directory < SysHandles + FDirectories.Count do
    begin
      // removed directory
      if Integer(FDirectories.Objects[Directory - SysHandles]) = 1 then
      begin
        FDirectories.Delete(Directory - SysHandles);
        Assert(OrigDirectory < Count);
        FindCloseChangeNotification(Handles^[OrigDirectory]);
        Handles^[OrigDirectory] := INVALID_HANDLE_VALUE;
        Inc(OrigDirectory);
        Changed := True;
      end
        else
      // newly added
      if Integer(FDirectories.Objects[Directory - SysHandles]) = 2 then
      begin
        Assert(Directory < NewAlloc);
        NewHandles^[Directory] := StartMonitor(FDirectories[Directory - SysHandles], False);
        if NewHandles^[Directory] = INVALID_HANDLE_VALUE then
        begin
          // currently we resign on correct resource freeing
          Result := WAIT_FAILED;
          Break;
        end;
        FDirectories.Objects[Directory - SysHandles] := TObject(0);
        Inc(Directory);
        Changed := True;
      end
        else
      begin
        Assert(Integer(FDirectories.Objects[Directory - SysHandles]) = 0);
        Assert(Directory < NewAlloc);
        Assert(OrigDirectory < Count);
        NewHandles^[Directory] := Handles^[OrigDirectory];
        Inc(Directory);
        Inc(OrigDirectory);
      end;
    end;

    if Result <> WAIT_FAILED then
    begin
      Assert(Count = OrigDirectory);

      FreeMem(Handles);
      Handles := NewHandles;
      Count := SysHandles + FDirectories.Count;

      Assert(Count = Directory);
      Assert(Count <= NewAlloc);
    end;

    if Changed and Assigned(OnDirectoriesChange) then
    begin
      FNotifiedDirectories := FDirectories.Count;
      DoSynchronize(InformDirectoriesChange);
    end;
  end;

  function Notify(Directory: Integer; Handle: THandle): Cardinal;
  begin
    // Notification signalled, so fire the OnChange event and then FindNext..
    // loop back to re-WaitFor... the thread
    Sleep(FChangeDelay);
    FNotifiedDirectory := FDirectories[Directory];
    FSubdirsChanged := False;

    DoSynchronize(InformChange);

    if FSubdirsChanged then
      Result := UpdateSubDirectories(Directory)
    else
      Result := WAIT_TIMEOUT;

    FindNextChangeNotification(Handle);
  end;

  function CheckAllObjects(Count: Integer; DirHandles: PWOHandleArray): Cardinal;
  const
    Offset = MAXIMUM_WAIT_OBJECTS;
  var
    C, Start, Directory: Cardinal;
  begin
    Result := WAIT_TIMEOUT;
    Start := 0;
    while Start < Cardinal(Count) do
    begin
      if Cardinal(Count) - Start > Offset then C := Offset
        else C := Cardinal(Count) - Start;
      Result := WaitForMultipleObjects(C, @DirHandles[Start], false, 0);
      Directory := Start + Result - WAIT_OBJECT_0;
      // (Result - WAIT_OBJECT_0 >= 0) is always true
      if Result - WAIT_OBJECT_0 < C then
      begin
        Result := Notify(Directory, DirHandles^[Directory]);
        // when new directory is found, restart,
        // if not check the same range again for possibly different notification
        if Result = WAIT_OBJECT_0 then Start := 0;
      end
        else Inc(Start, C);
      if Result <> WAIT_TIMEOUT then Break;

      Result := WaitForMultipleObjects(2, Handles, False, 0);
      if Result <> WAIT_TIMEOUT then Break;
    end;
  end;

const
  DestroySlot = 0;
  ChangeSlot = 1;
  HierNotifySlot = 2;
var
  HierMode: Boolean;
  WaitCount: Word;
  I: Integer;
  Result: Cardinal;
  WasEnabled: Boolean;
begin {Execute}
  BaseHandles := 2;
  SysHandles := BaseHandles;
  Count := SysHandles + FDirectories.Count;
  HierMode := (Count > MAXIMUM_WAIT_OBJECTS);
  if HierMode then
  begin
    Inc(SysHandles);
    Inc(Count);
  end;
  GetMem(Handles, SizeOf(THandle) * Count);
  try
    Handles^[DestroySlot] := FDestroyEvent;      // put DestroyEvent handle in slot 0
    Handles^[ChangeSlot] := FChangeEvent;       // put ChangeEvent handle in slot 1

    repeat

      WasEnabled := Enabled;

      if WasEnabled then
      begin
        if HierMode then
        begin
          // expect that the first directory is the top level one
          Handles^[HierNotifySlot] := StartMonitor(FDirectories[0], True);
          if Handles^[HierNotifySlot] = INVALID_HANDLE_VALUE then Exit;
        end;

        for I := SysHandles to Count - 1 do
        begin
          Handles^[I] := StartMonitor(FDirectories[I - SysHandles], False);
          if Handles^[I] = INVALID_HANDLE_VALUE then Exit;
        end;
      end;

      repeat
        if WasEnabled then
        begin
          if HierMode then WaitCount := SysHandles
            else WaitCount := Count;
        end
          else WaitCount := BaseHandles;

        // wait for any of the change notification, destroy or
        // change events to be signalled
        Result := WaitForMultipleObjects(WaitCount, Handles, False, INFINITE);

        if Result = WAIT_FAILED then
        begin
          FNotifiedDirectory := '';
          SaveOSError;
          DoSynchronize(InformInvalid);
        end
          else
        if HierMode and (Result - WAIT_OBJECT_0 = HierNotifySlot) then
        begin
          FindNextChangeNotification(Handles[HierNotifySlot]);
          Result := CheckAllObjects(Count - SysHandles, @Handles[SysHandles]);
          // (Result >= WAIT_OBJECT_0) = always true
          if Result < Cardinal(WAIT_OBJECT_0 + (Count - SysHandles)) then
          begin
            Result := WAIT_OBJECT_0 + HierNotifySlot;
          end;
        end
          else
        if (Result >= WAIT_OBJECT_0 + SysHandles) and
           (Result < WAIT_OBJECT_0 + WaitCount) then
        begin
          Result := Notify(Result - WAIT_OBJECT_0 - SysHandles,
            Handles^[Result - WAIT_OBJECT_0]);
          if Result = WAIT_OBJECT_0 then Result := WAIT_TIMEOUT;
        end;
      // note that WaitCount can be different here than when
      // WaitForMultipleObjects  was called, but it should not matter as it is
      until (Result = WAIT_FAILED) or (Result = WAIT_OBJECT_0 + DestroySlot) or
        (Result = WAIT_OBJECT_0 + ChangeSlot) or
        ((Result >= WAIT_ABANDONED_0) and (Result < WAIT_ABANDONED_0 + WaitCount));

      if WasEnabled then
      begin
        if HierMode then
        begin
          FindCloseChangeNotification(Handles^[HierNotifySlot]);
        end;

        for I := SysHandles to Count - 1 do
        begin
          FindCloseChangeNotification(Handles^[I]);
        end;
      end;

      SetEvent(FChangedEvent);

      // loop back to restart if ChangeEvent was signalled
    until (Result - WAIT_OBJECT_0 <> ChangeSlot) or Self.Terminated;

    // closing down so chuck the two events
    CloseHandle(FChangeEvent);
    CloseHandle(FDestroyEvent);
    CloseHandle(FChangedEvent);
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
  FOnFilter := nil;
  FOnTooManyDirectories := nil;
  FOnDirectoriesChange := nil;
  FMonitor := TDiscMonitorThread.Create;  // create a monitor thread
  FMonitor.ChangeDelay := 500;            {ie01}
  FMonitor.OnChange := Change;            // hook into its event handlers
  FMonitor.OnInvalid := Invalid;
  FMonitor.OnSynchronize := DoSynchronize;
  FMonitor.OnFilter := Filter;
  FMonitor.OnDirectoriesChange := DirectoriesChange;
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
// handler
procedure TDiscMonitor.Change(Sender: TObject; const Directory: string;
  var SubdirsChanged: Boolean);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Directory, SubdirsChanged)
end;

// Invalid notification from the thread has occurred. Call the component's event
// handler
procedure TDiscMonitor.Invalid(Sender: TObject; const Directory: string; const ErrorStr: string);
begin
  if Assigned(FOnInvalid) then
    FOnInvalid(Self, Directory, ErrorStr)
end;

procedure TDiscMonitor.Filter(Sender: TObject; const DirectoryName: string; var Add: Boolean);
begin
  if Assigned(FOnFilter) then
    FOnFilter(Self, DirectoryName, Add)
end;

procedure TDiscMonitor.DirectoriesChange(Sender: TObject; Directories: Integer);
begin
  if Assigned(FOnDirectoriesChange) then
    FOnDirectoriesChange(Self, Directories)
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

function TDiscMonitor.GetEnabled: Boolean;
begin
  Result := FMonitor.Enabled;
end;

// set the directory to monitor
procedure TDiscMonitor.SetDirectories(Value: TStrings);
begin
  FMonitor.Directories := Value;
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
      if SubDirs then
        DiscMon.AddDirectory(Dirs, Directory, FMaxDirectories, OnFilter,
          OnTooManyDirectories, False);

      SetDirectories(Dirs);

    finally
      Dirs.Free;
    end;
  end
    else
  begin
    SetDirectories(nil);
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

      SetDirectories(Dirs);

    finally
      Dirs.Free;
    end;
  end
    else
  begin
    SetDirectories(nil);
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

procedure TDiscMonitor.SetEnabled(Value: Boolean);
begin
  FMonitor.Enabled := Value;
end;

procedure Register;
begin
  RegisterComponents('Martin', [TDiscMonitor]);
end;

end.
