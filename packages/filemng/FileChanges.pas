unit FileChanges;
{==================================================================

 Component TDeleteThread  /  Version 1.0  / 01.1999
 ==================================================


    Description:
    ============
    Runs a separate thread, wich monitors multiple files or directories
    for existance. When one of these is deleted, the event OnSignalDelete
    is fired.
    This component uses findfirst() for searching the monitored files. So
    it will also work with network drives. (The WINAPI function
    FINDFIRSTCHANGENOTIFICATION does not work with novell network drives.)

    Author:
    =======
    (c) Ingo Eckel 12/1998
    Sodener Weg 38
    65812 Bad Soden
    Germany

{==================================================================}


interface

uses
  SysUtils, Windows, Classes, CompThread;


type
  TFileDeleteEvent = procedure (Sender: TObject; Files : TStringList) of object;

{==============================================================}
{ FileDeleteThread:                                            }
{==============================================================}
  TFileDeleteThread = class(TCompThread)
  private
    fOnSignalDelete: TFileDeleteEvent;
    fFiles         : TStringList;
    fDelFiles      : TStringList;
    fEndTime       : FILETIME;

  protected
    procedure Execute;           override;
    procedure DoTerminate;       override;
    Procedure DoOnSignalDelete;
  public
    constructor Create(Files: TstringList; TimeOut : DWord; SignalProc : TFileDeleteEvent);
    Property Terminated;
  Published
    Property OnSignalDelete : TFileDeleteEvent Read fOnSignalDelete
                                                     Write fOnSignalDelete;
  end;


{==============================================================}
implementation
{==============================================================}



{==============================================================}
{ FileDeleteThread:                                            }
{==============================================================}
constructor TFileDeleteThread.Create(Files: TstringList; TimeOut : DWord; SignalProc : TFileDeleteEvent);
Begin
  inherited Create(True);
  FreeOnTerminate := TRUE;
  fOnSignalDelete := SignalProc;
  fFiles    := TStringList.Create;
  fDelFiles := TStringList.Create;

  FFiles.Assign(Files);
  GetSystemTimeAsFileTime(fEndTime);
  INT64(fEndTime) := INT64(fEndTime) + TimeOut * 10000000;
  Resume;
End; {TFileDeleteThread.Create}


procedure TFileDeleteThread.Execute;
Var SRec     : TSearchRec;
    i        : Integer;
    DosError : Integer;
    ChangeDetected : Boolean;
    ThisTime : FILETIME;

Begin
  repeat
    {Wait 500 Milliseconds before starting next polling cyclus:}
    Sleep(500);
    ChangeDetected := False;
    i := 0;
    While i <= Pred(fFiles.Count) Do
    Begin
      DosError := SysUtils.FindFirst(fFiles[i], faAnyFile, SRec);
      IF DosError <> 0 Then
      Begin
        ChangeDetected := True;
        fDelFiles.Add(fFiles[i]);
        fFiles.Delete(i);
      End
      Else
      INC(i);
      SysUtils.FindClose(SRec);
    End;

    IF ChangeDetected And Assigned(fOnSignalDelete) Then
    Begin
       Synchronize(DoOnSignalDelete);
       fDelFiles.Clear;
    End;

    {Timeout reached? }
    GetSystemTimeAsFileTime(ThisTime);
    IF INT64(ThisTime) >= INT64(fEndTime) Then
      Break;

  until Terminated Or (fFiles.Count = 0);
End; {TFileDeleteThread.Execute}


procedure TFileDeleteThread.DoTerminate;
Begin
  fFiles.Free;
  fDelFiles.Free;
  Inherited DoTerminate;
End; {TFileDeleteThread.DoTerminate}


Procedure TFileDeleteThread.DoOnSignalDelete;
Begin
  If Assigned(fOnSignalDelete) Then
  fOnSignalDelete(Self, fDelFiles);
End; {TFileDeleteThread.DoSignalDelete}




end.
