{

Modifications:
==============

 ie01: OnChange delayed for 500 ms.
 ie02: No usage of BrowseDirectory component
 ie03: Resume suspended thread when terminating the thread

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ShlObj, ActiveX, CompThread {, BrowseDr, DsgnIntf ie02};

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
  TDiscMonitorThread = class(TCompThread)
  private
    FOnChange : TNotifyEvent;
    FOnInvalid : TNotifyEvent;
    FDirectory : string;
    FFilters : DWORD;
    FDestroyEvent,
    FChangeEvent : THandle;
    FSubTree : boolean;
    fChangeDelay : Integer;  {ie01}
    procedure InformChange;
    procedure InformInvalid;
    procedure SetDirectory (const Value : string);
    procedure SetFilters (Value : DWORD);
    procedure SetSubTree (Value : boolean);
  protected
    procedure Execute; override;
    procedure Update;
  public
    constructor Create;
    destructor Destroy; override;
// The directory to monitor
    property Directory : string read FDirectory write SetDirectory;
// Filter condition, may be any of the FILE_NOTIFY_CHANGE_XXXXXXX constants
// ORed together.  Zero is invalid.
    property Filters : DWORD read FFilters write SetFilters;
// Event called when change noted in directory
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
// Event called for invalid parameters
    property OnInvalid : TNotifyEvent read FOnInvalid write FOnInvalid;
// Include subdirectories below specified directory.
    property SubTree : boolean read FSubTree write SetSubTree;
// specify, how long the thread should wait, before the event OnChange is fired:
    Property ChangeDelay : Integer Read fChangeDelay Write fChangeDelay  {ie01}
                                   Default 500;                          {ie01}
  end;

//===================== DISC MONITORING COMPONENT ==============================

// specify directory string as type string so we can have our own property editor
  TDiscMonitorDirStr = type string;

// enumerated type for filter conditions (not directly usable in thread class)
// see the SetFilters procedure for the translation of these filter conditions
// into FILE_NOTIFY_CHANGE_XXXXXX constants.
  TMonitorFilter = (moFilename, moDirName, moAttributes, moSize,
                    moLastWrite, moSecurity);
// set of filter conditions
  TMonitorFilters = set of TMonitorFilter;

  TDiscMonitor = class(TComponent)
  private
    FActive : boolean;
    FMonitor : TDiscMonitorThread;
    FFilters : TMonitorFilters;
    FOnChange : TNotifyEvent;
    FOnInvalid : TNotifyEvent;
    FShowMsg : boolean;
    function GetDirectory : TDiscMonitorDirStr;
    function GetSubTree : boolean;
    procedure SetActive (Value : boolean);
    procedure SetDirectory (Value : TDiscMonitorDirStr);
    procedure SetFilters (Value : TMonitorFilters);
    procedure SetSubTree (Value : boolean);
    Function  GetChangeDelay : Integer;         {ie01}
    Procedure SetChangeDelay(Value : Integer);  {ie01}
  protected
    procedure Change (Sender : TObject);
    procedure Invalid (Sender : TObject);
  public
    constructor Create (AOwner : TComponent);  Override;
    destructor Destroy; override;
// stop the monitoring thread running
    procedure Close;
// start the monitoring thread running
    procedure Open;
// read-only property to access the thread directly
    property Thread : TDiscMonitorThread read FMonitor;
  published
// the directory to monitor
    property Directory : TDiscMonitorDirStr read GetDirectory write SetDirectory;
// control the appearance of information messages at design time (only)
    property ShowDesignMsg : boolean read FShowMsg write FShowMsg default false;
// event called when a change is notified
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
// event called if an invalid condition is found
    property OnInvalid : TNotifyEvent read FOnInvalid write FOnInvalid;
// notification filter conditions
    property Filters : TMonitorFilters read FFilters write SetFilters default [moFilename];
// include subdirectories below the specified directory
    property SubTree : boolean read GetSubTree write SetSubTree default true;
// specify if the monitoring thread is active
    property Active : boolean read FActive write SetActive default false;
// specify, how long the thread should wait, before the event OnChange is fired:
    Property ChangeDelay : Integer Read GetChangeDelay Write SetChangeDelay  {ie01}
                                   Default 500;                              {ie01}
  end;


procedure Register;


{$IFDEF BUGFIX}
{$Z4}

type TWinBool = (winFalse, winTrue);
function FixFindFirstChangeNotification(const lpPathName: PChar;
                                        bWatchSubtree: TWinBool;
                                        dwNotifyFilter: DWORD): THandle stdcall; external kernel32 name 'FindFirstChangeNotificationA';
{$ENDIF}

implementation

//=== MONITOR THREAD ===========================================================

// Create the thread suspended.  Create two events, each are created using
// standard security, in the non-signalled state, with auto-reset and without
// names.  The FDestroyEvent will be used to signal the thread that it is to close
// down.  The FChangeEvent will be used to signal the thread when the monitoring
// conditions (directory, filters or sub-directory search) have changed.
// OnTerminate is left as false, so the user must Free the thread.

constructor TDiscMonitorThread.Create;
begin
  inherited Create (true);
  FDestroyEvent := CreateEvent (nil, true,  false, NIL);
  FChangeEvent  := CreateEvent (nil, false, false, NIL);
end;

// close OnXXXXX links, signal the thread that it is to close down
destructor TDiscMonitorThread.Destroy;
begin
  FOnChange := nil;
  FOnInvalid := nil;
  IF Suspended Then  {ie03}
  Resume;            {ie03}
  SetEvent (FDestroyEvent);
  FDirectory := '';
  inherited Destroy
end;

// called by the Execute procedure via Synchronize.  So this is VCL thread safe
procedure TDiscMonitorThread.InformChange;
begin
  if Assigned (FOnChange) then
    FOnChange (Self)
end;

// called by the Execute procedure via Synchronize.  So this is VCL thread safe
procedure TDiscMonitorThread.InformInvalid;
begin
  if Assigned (FOnInvalid) then
    FOnInvalid (Self)
end;

// Change the current directory
procedure TDiscMonitorThread.SetDirectory (const Value : string);
begin
  if Value <> FDirectory then
  begin
    FDirectory := Value;
    Update
  end
end;

// Change the current filters
procedure TDiscMonitorThread.SetFilters (Value : DWORD);
begin
  if Value <> FFilters then
  begin
    FFilters := Value;
    Update
  end
end;

// Change the current sub-tree condition
procedure TDiscMonitorThread.SetSubTree (Value : boolean);
begin
  if Value <> FSubTree then
  begin
    FSubtree := Value;
    Update
  end
end;


Function TDiscMonitor.GetChangeDelay : Integer;  {ie01}
begin
  Result := FMonitor.ChangeDelay;
end;


Procedure TDiscMonitor.SetChangeDelay(Value : Integer);  {ie01}
begin
  FMonitor.ChangeDelay := Value;
end;


// On any of the above three changes, if the thread is running then
// signal it that a change has occurred.
procedure TDiscMonitorThread.Update;
begin
  if not Suspended then
    SetEvent (FChangeEvent)
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
// There appears to be a bug in win 95 where the bWatchSubTree parameter
// of FindFirstChangeNotification which is a BOOL only accepts values of
// 0 and 1 as valid, rather than 0 and any non-0 value as it should.  In D2
// BOOL was defined as 0..1 so the code worked, in D3 it is 0..-1 so
// fails. The result is FindF... produces and error message.  This fix (bodge) is
// needed to produce a 0,1 bool pair, rather that 0,-1 as declared in D3

{$IFDEF BUGFIX}
const R : Array [false..true] of TWinBOOL = (WinFalse, WinTrue);
{$ELSE}
const R : Array [false..true] of LongBool = (LongBool(0), LongBool(1));
{$ENDIF}

var A : Array [0..2] of THandle; // used to give the handles to WaitFor...
    B : boolean;                 // set to true when the thread is to terminate

Function StartMonitor : THandle;
Begin
{$IFDEF BUGFIX}
  Result := FixFindFirstChangeNotification (PChar(FDirectory), R[fSubTree], FFilters);
{$ELSE}
  Result := FindFirstChangeNotification (PChar(FDirectory), R[fSubTree], FFilters);
{$ENDIF}
End; {StartMonitor}

begin {Execute}
  B := false;
  A [0] := FDestroyEvent;      // put DestroyEvent handle in slot 0
  A [1] := FChangeEvent;       // put ChangeEvent handle in slot 1
// make the first call to the change notification system and put the returned
// handle in slot 2.

  A [2] := StartMonitor;

  repeat

// if the change notification handle is invalid then:
    if A[2] = INVALID_HANDLE_VALUE then
    begin
  // call the OnInvalid event
      Synchronize (InformInvalid);
  // wait until either DestroyEvent or the ChangeEvents are signalled
      case WaitForMultipleObjects (2, PWOHandleArray (@A), false, INFINITE) - WAIT_OBJECT_0 of
  // DestroyEvent - close down by setting B to true
        0 : B := true;
  // try new conditions and loop back to the invalid handle test
        1 : A [2] := StartMonitor;
      end
    end else
// handle is valid so wait for any of the change notification, destroy or
// change events to be signalled
      case WaitForMultipleObjects (3, PWOHandleArray (@A), false, INFINITE) - WAIT_OBJECT_0 of
        0 : begin
  // DestroyEvent signalled so use FindClose... and close down by setting B to true
              FindCloseChangeNotification (A [2]);
              B := true
            end;
        1 : begin
  // ChangeEvent signalled so close old conditions by FindClose... and start
  // off new conditions.  Loop back to invalid test in case new conditions are
  // invalid
              FindCloseChangeNotification (A [2]);
              A [2] := StartMonitor;
            end;
        2 : begin
  // Notification signalled, so fire the OnChange event and then FindNext..
  // loop back to re-WaitFor... the thread
              Sleep(fChangeDelay);  {ie01 ins}
              Synchronize (InformChange);
              FindNextChangeNotification (A [2])
            end;
      end
  until B Or Self.Terminated;

// closing down so chuck the two events
  CloseHandle (FChangeEvent);
  CloseHandle (FDestroyEvent)
end;

//=== MONITOR COMPONENT ========================================================

// This component encapsulates the above thread.  It has properties for
// directory, sub-directory conditions, filters, whether information messages
// should be given at design time and if the thread is active.
constructor TDiscMonitor.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FMonitor := TDiscMonitorThread.Create;  // create a monitor thread
  FMonitor.ChangeDelay := 500;            {ie01}
  FMonitor.OnChange := Change;            // hook into its event handlers
  FMonitor.OnInvalid := Invalid;
  Filters := [moFilename];                // default filters to moFilename
  SubTree := true                         // default sub-tree search to on
end;

destructor TDiscMonitor.Destroy;
begin
  FMonitor.Free;                          // chuck the thread
  inherited Destroy
end;

// Change notification from the thread has occurred. Call the component's event
// handler and then, if in design mode, and if desired, put up a simple
// notification message
procedure TDiscMonitor.Change;
begin
  if Assigned (FOnChange) then
    FOnChange (Self)
  else
    if (csDesigning in ComponentState) and FShowMsg then
      ShowMessage ('Change signalled')
end;

// Invalid notification from the thread has occurred. Call the component's event
// handler and then, if in design mode, and if desired, put up a simple
// notification message
procedure TDiscMonitor.Invalid;
begin
  if Assigned (FOnInvalid) then
    FOnInvalid (Self)
  else
    if (csDesigning in ComponentState) and FShowMsg then
      ShowMessage ('Invalid parameter signalled')
end;

// Stop the monitor running
procedure TDiscMonitor.Close;
begin
  Active := false
end;

// Run the monitor
procedure TDiscMonitor.Open;
begin
  Active := true
end;

// Control the thread by using it's resume and suspend methods
procedure TDiscMonitor.SetActive (Value : boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if Active then
    begin
      FMonitor.Resume;
      FMonitor.Update
    end else
      FMonitor.Suspend
  end
end;

// get the current directory from the thread
function TDiscMonitor.GetDirectory : TDiscMonitorDirStr;
begin
  Result := FMonitor.Directory
end;

// get the current sub-tree status from the thread
function TDiscMonitor.GetSubTree : boolean;
begin
  Result := FMonitor.SubTree
end;

// set the directory to monitor
procedure TDiscMonitor.SetDirectory (Value : TDiscMonitorDirStr);
begin
  FMonitor.Directory := Value
end;

// Change the filter conditions.  The thread uses the raw windows constants
// (FILE_NOTIFY_CHANGE_XXXX) but the components uses a set of enumurated type.
// It is therefore necessary to translate from the component format into
// an integer value for the thread.
procedure TDiscMonitor.SetFilters (Value : TMonitorFilters);
const
  XlatFileNotify : array [moFilename..moSecurity] of DWORD =
    (FILE_NOTIFY_CHANGE_FILE_NAME,  FILE_NOTIFY_CHANGE_DIR_NAME,
     FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
     FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY);
var
  L : TMonitorFilter;
  I : DWORD;
begin
  if Value <> FFilters then
    if Value = [] then
      ShowMessage ('Some filter condition must be set.')
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
procedure TDiscMonitor.SetSubTree (Value : boolean);
begin
  FMonitor.SubTree := Value
end;

{ie02 TBrowseDirectoryDlg deleted.}

procedure Register;
begin
  {MP}RegisterComponents ({'Tools'}'DriveDir', [TDiscMonitor]);
  {RegisterPropertyEditor (TypeInfo (TDiscMonitorDirStr), nil, '', TDiscMonitorDirStrProperty);}
end;

end.


