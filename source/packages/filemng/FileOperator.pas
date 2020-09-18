unit FileOperator;

{
+------------------------------------------------------------------------------+
| TFileOperator Component Version 1.02 / 1999                                  |
+------------------------------------------------------------------------------+
| Author: Ingo Eckel                                                           |
|                                                                              |
| Based on the TFileOperator component written by Glen Why 1996                |
| Enhancements: uses TStringList as operands, undo functionality implemented.  |
+------------------------------------------------------------------------------+
| Description:                                                                 |
| This component encapsulates the ShFileOperation API of Microsoft Windows.    |
| Performs a copy, move, rename, or delete operation on a file system object.  |
| It also allows to undo the last operation, if a copy or move operation       |
| was performed.                                                               |
+------------------------------------------------------------------------------+
| Properties:                                                                  |
|                                                                              |
|   OperandFrom:                                                               |
|        Stringlist, that contains the names of the source files,              |
|        wildcard filename (*.*) is accepted.                                  |
|                                                                              |
|   OperandTo:                                                                 |
|         Stringlist that specifies the destination for the moved,             |
|         copied or renamed file. Should be the target directory, when         |
|         performing a copy or move operation.                                 |
|                                                                              |
|   WantMappingHandle: True, if shFileOperation should create a filename-      |
|         mapping of the processed files. This mapping is allocated by         |
|         shFileOperation and will be automatically deallocated by the         |
|         component.                                                           |
|                                                                              |
|   Operation:                                                                 |
|    -foCopy    Copies the files specified by OperandFrom to the location      |
|               specified by OperandTo.                                        |
|    -foDelete  Deletes the files specified by OperandFrom (OperandTo          |
|                 ignored).                                                    |
|    -foMove    Moves the files specified by OperandFrom to the location       |
|               specified by OperandTo.                                        |
|    -foRename  Renames the files specified by OperandFrom.                    |
|                                                                              |
|   Options:                                                                   |
|    -foAllowUndo         Preserves undo information, if possible.             |
|    -foConfirmMouse      Not implemented.                                     |
|    -foFilesOnly         Performs the operation only on files if a wildcard   |
|                         filename (*.*) is specified.                         |
|    -foMultiDestFiles    Indicates that the OperandTo member specifies multiple|
|                         destination files (one for each source file) rather  |
|                         than one directory where all source files are        |
|                         to be deposited.                                     |
|                         Note: not compatible with undo operation.            |
|    -foNoConfirmation    Responds with "yes to all" for any dialog box that   |
|                         is displayed.                                        |
|    -fofNoConfirmMkDir   Does not confirm the creation of a new directory     |
|                         if the operation requires one to be created.         |
|    -foRenameOnCollision Gives the file being operated on a new name          |
|                         (such as "Copy #1 of...") in a move, copy,           |
|                         or rename operation if a file of the target name     |
|                         already exists.                                      |
|                         Note: not compatible with undo operation.            |
|    -foSilent            Does not display a progress dialog box.              |
|    -foSimpleProgress    Displays a progress dialog box, but does not show    |
|                         the filenames.                                       |
|                                                                              |
|   Title: String to use as the title for a progress dialog box.               |
|          This member is used only if Options includes fofSimpleProgress.     |
|                                                                              |
|   OperationAborted: Value that receives True if the user aborted any file    |
|          operations before they were completed or FALSE otherwise.           |
|                                                                              |
|   LastOperandFrom        Stringlist of last performed value of Operandfrom.  |
|                                                                              |
|   LastOperandTo          Stringlist of last performed value of OperandTo.    |
|                                                                              |
|   LastOperation          Value of last performed operation.                  |
|                                                                              |
|   WantMappingHandle      After execution should a file mapping be returned.  |
|                          Works only, when option foMultiDestFiles used.      |
|                          Note: don't know, how to use this.                  |
|                                                                              |
|   NameMappings           Pointer to namemappings, if property wantmapping-   |
|                          handle set.                                         |
+------------------------------------------------------------------------------+
| Methods:                                                                     |
|                                                                              |
|   Function Execute : Boolean;                                                |
|     Performs the copy, move, rename, or delete operation.                    |
|     Returns zero if successful or nonzero value if an error occurs.          |
|                                                                              |
|   Function UndoExecute : Boolean;                                            |
|     Reverses the last copy, move or rename operation.                        |
|     Note: works currently only, if only a single OperandTo is used. This     |
|     OperandTo must be the target directory.                                  |
|                                                                              |
|   Function CanUndo : Boolean;                                                |
|     Returns TRUE, if undo of last operation is possible.                     |
|                                                                              |
|   Procedure ClearUndo;                                                       |
|     Clears the preserved undo informations. After that, CanUndo allways      |
|     returns false.                                                           |
+------------------------------------------------------------------------------+
| Events:                                                                      |
+------------------------------------------------------------------------------+
}

{==============================================================}
interface
{==============================================================}
uses
 Windows, Classes, Forms, Controls, ShellAPI, BaseUtils, SysUtils;


Type

  TFileOperation = ( foCopy, foDelete, foMove, foRename );
  TFileOperationFlag = ( foAllowUndo, foConfirmMouse, foFilesOnly,
    foMultiDestFiles, foNoConfirmation, foNoConfirmMkDir,
     foRenameOnCollision, foSilent, foSimpleProgress);
  TFileOperationFlags = set of TFileOperationFlag;

{==============================================================}
  TFileOperator = class( TComponent )
{==============================================================}
  private
{==============================================================}
    FData          : TShFileOpStruct;
    FFrom          : TStringList;
    FTo            : TStringList;
    FLastFrom      : TStringList;
    FLastTo        : TStringList;
    FLastOperation : TFileOperation;
    fLastFlags     : TFileOperationFlags;
    fCanUndo       : Boolean;
    Procedure SetOperation( Value :TFileOperation );
    Function  GetOperation :TFileOperation;
    Function  GetWantMappingHandle :Boolean;
    Procedure SetWantMappingHandle ( Value :Boolean );
    Procedure SetFlags( Value :TFileOperationFlags );
    Function  GetFlags :TFileOperationFlags;
    Function  GetOperFlag( F :Cardinal ) :Boolean;
    Procedure SetOperFlag( F :Cardinal; V :Boolean );
    Procedure ReadData( Reader :TReader );
    Procedure WriteData( Writer :TWriter );
    Procedure SwapStringList(Var FromL, ToL : TStringList);
    Function  GetOperationAborted: Bool;

{==============================================================}
  protected
{==============================================================}
    Procedure DefineProperties( Filer :TFiler ); override;

{==============================================================}
  public
{==============================================================}
    Property OperationAborted : Bool        Read GetOperationAborted;
    Property OperandFrom      : TStringList Read fFrom Write fFrom;
    Property OperandTo        : TStringList Read FTo   Write fTo;
    Property CanUndo          : Boolean     Read fCanUndo;
    Property LastOperation    : TFileOperation Read fLastOperation;
    Property LastOperandFrom  : TStringList Read fLastFrom;
    Property LastOperandTo    : TStringList Read fLastTo;

    Constructor Create(aOwner :TComponent); Override;
    Destructor  Destroy; override;
    Function    Execute          : Boolean;
    Function    UndoExecute      : Boolean;
    Procedure   ClearUndo;
{==============================================================}
  published
{==============================================================}
    Property Operation : TFileOperation  Read GetOperation Write SetOperation Stored false;
    Property Flags : TFileOperationFlags Read GetFlags     Write SetFlags     Stored false;
    Property WantMappingHandle :Boolean  Read GetWantMappingHandle Write SetWantMappingHandle Stored false;
  end;

const
  FileOperatorDefaultFlags = [foAllowUndo, foNoConfirmMkDir];

procedure Register;

resourcestring
  SFileOperation = 'File Operation';
{==============================================================}
implementation
{==============================================================}

uses
  PasTools;

{ TFileOperator }

procedure TFileOperator.SetOperation( Value :TFileOperation );
begin
 with FData do
  case Value of
    foCopy : wFunc := FO_COPY;
    foDelete : wFunc := FO_DELETE;
    foRename : wFunc := FO_RENAME;
    foMove : wFunc := FO_MOVE;
  end;
end; {SetOperation}


function TFileOperator.GetOperation :TFileOperation;
begin
  result := foCopy;
  case FData.wFunc of
    FO_COPY   : result := foCopy;
    FO_DELETE : result := foDelete;
    FO_RENAME : result := foRename;
    FO_MOVE   : result := foMove;
  end;
end; {GetOperation}


function TFileOperator.GetWantMappingHandle :Boolean;
begin
 result := GetOperFlag( FOF_WANTMAPPINGHANDLE );
end;


procedure TFileOperator.SetWantMappingHandle ( Value :Boolean );
begin
 SetOperFlag( FOF_WANTMAPPINGHANDLE, Value );
end;


procedure TFileOperator.SetFlags( Value :TFileOperationFlags );
begin
  SetOperFlag( FOF_ALLOWUNDO, foAllowUndo in Value );
  SetOperFlag( FOF_CONFIRMMOUSE, foConfirmMouse in Value );
  SetOperFlag( FOF_FILESONLY, foFilesOnly in Value );
  SetOperFlag( FOF_MULTIDESTFILES, foMultiDestFiles in Value );
  SetOperFlag( FOF_NOCONFIRMATION, foNoConfirmation in Value );
  SetOperFlag( FOF_NOCONFIRMMKDIR, foNoConfirmMkDir in Value );
  SetOperFlag( FOF_RENAMEONCOLLISION, foRenameOnCollision in Value );
  SetOperFlag( FOF_SILENT, foSilent in Value );
  SetOperFlag( FOF_SIMPLEPROGRESS, foSimpleProgress in Value );
end; {SetFlags}


function TFileOperator.GetFlags :TFileOperationFlags;
begin
  result := [];
  if GetOperFlag( FOF_ALLOWUNDO ) then include( result, foAllowUndo );
  if GetOperFlag( FOF_CONFIRMMOUSE ) then include( result, foConfirmMouse );
  if GetOperFlag( FOF_FILESONLY ) then include( result, foFilesOnly );
  if GetOperFlag( FOF_MULTIDESTFILES ) then include( result, foMultiDestFiles );
  if GetOperFlag( FOF_NOCONFIRMATION ) then include( result, foNoConfirmation );
  if GetOperFlag( FOF_NOCONFIRMMKDIR ) then include( result, foNoConfirmMkDir );
  if GetOperFlag( FOF_RENAMEONCOLLISION ) then include( result, foRenameOnCollision );
  if GetOperFlag( FOF_SILENT ) then include( result, foSilent );
  if GetOperFlag( FOF_SIMPLEPROGRESS ) then include( result, foSimpleProgress );
end; {GetFlags}


function TFileOperator.GetOperFlag( F :Cardinal ):boolean;
begin
  result := ( FData.fFlags and F ) <> 0;
end;


procedure TFileOperator.SetOperFlag( F :Cardinal; V :Boolean );
begin
 with FData do
  if V then
  fFlags := fFlags or F
  else fFlags := fFlags and ( not F );
end;


procedure TFileOperator.DefineProperties( Filer :TFiler );
begin
 Inherited DefineProperties( Filer );
 Filer.DefineProperty( 'data', ReadData, WriteData, true );
end;


procedure TFileOperator.ReadData( Reader :TReader );
begin
  Reader.Read( FData, SizeOf( FData ) );
end;


procedure TFileOperator.WriteData( Writer :TWriter );
begin
  writer.write( FData, SizeOf( FData ) );
end;


Constructor TFileOperator.Create(aOwner :TComponent);
begin
 inherited Create(aOwner);
 fFrom     := TStringList.Create;
 fTo       := TStringList.Create;
 fLastFrom := TStringList.Create;
 fLastTo   := TStringList.Create;
 fCanUndo  := False;
 FData.fFlags := 0;
 Flags := FileOperatorDefaultFlags;
end; {Create}


function TFileOperator.Execute : Boolean;
Var SFrom : String;
    sTo   : String;

Function ConvertOperand(List : TStringList) : String;
Var i : Integer;
Begin
  Result := '';
  For i := 0 to Pred(List.Count) Do
  Begin
    // SHFileOperation does not support long paths anyway
    Result := Result + ApiPath(List[i]);
    SetLength(Result, Succ(Length(Result)));
    Result[Length(Result)] := #0;
  End;
  SetLength(Result, Succ(Length(Result)));
  Result[Length(Result)] := #0;
End; {ConvertOperand}


begin {Execute}
  SFrom := ConvertOperand(FFrom);
  STo   := ConvertOperand(FTo);
  FData.pFrom := PChar( SFrom );
  FData.pTo := PChar( STo );
  IF (Owner is TWinControl) And TWinControl(Owner).HandleAllocated Then
    FData.Wnd := GetParentForm(TWinControl(Owner)).Handle
  Else
    FData.Wnd := Application.Handle;

  Try
    IF Assigned(FData.hNameMappings) Then
    shFreeNameMappings(THandle(FData.hNameMappings));
  Finally
    FData.hNameMappings := NIL;
  End;

  Try
    Try
      IF Operation = foRename Then
      Result := RenameFile(FFrom[0], FTo[0])
      Else
      Result := ShFileOperation( FData ) = 0;
    Finally
      IF GetOperFlag(FOF_ALLOWUNDO) And
         Not GetOperFlag(FOF_MULTIDESTFILES) And
         Not GetOperFlag(FOF_RENAMEONCOLLISION) And
        (Operation <> foDelete) Then
      Begin
        SwapStringList(fLastFrom, fFrom);
        SwapStringList(fLastTo,   fTo);
        fLastFlags := Flags;
        fCanUndo   := True;
        fLastOperation := Operation;
      End
      Else
      Begin
        FLastFrom.Clear;
        FLastTo.Clear;
        fCanUndo := False;
      End;
      FFrom.Clear;
      FTo.Clear;
    End;
  Except
    Result := False;
  End;
end; {Execute}


destructor TFileOperator.Destroy;
begin
  IF Assigned(FFrom) Then
  FFrom.Free;
  IF Assigned(FTo) Then
  FTo.Free;
  IF Assigned(FLastFrom) Then
  FLastFrom.Free;
  IF Assigned(FLastTo) Then
  FLastTo.Free;
  IF Assigned(FData.hNameMappings) Then
  shFreeNameMappings(THandle(FData.hNameMappings));
  inherited Destroy;
end; {Destroy}



Procedure TFileOperator.SwapStringList(Var FromL, ToL : TStringList);
Var StrL  : TStringList;
Begin
    StrL  := FromL;
    FromL := ToL;
    ToL   := StrL;
End; {SwapStringList}

Function  TFileOperator.GetOperationAborted: Bool;
Begin
  Result := FData.fAnyOperationsAborted;
End;

Function TFileOperator.UndoExecute : Boolean;
Var SaveFlags : TFileOperationFlags;
    SaveOperation : TFileOperation;
    i             : Integer;

Begin
  Result := False;
  IF Not fCanUndo Or
     (fLastFrom.Count = 0) Or
     (FLastTo.Count  <> 1) Then
  Exit;

  SaveFlags := Flags;
  SaveOperation := Operation;
  Flags := fLastFlags;
  Case SaveOperation OF
    foCopy  : IF fLastTo.Count = 1 Then
              Begin
                 Operation := foDelete;
                 Flags := Flags - [foAllowUndo] + [foNoConfirmation];
                 OperandFrom.Clear;
                 For i := 0 To fLastFrom.Count - 1 Do
                   OperandFrom.Add(IncludeTrailingPathDelimiter(fLastTo[0]) + ExtractFilename(fLastFrom[i]));
                 Result := Execute;
              End;
    foMove  : IF fLastTo.Count = 1 Then
              Begin
                 Operation := foMove;
                 Flags := Flags + [foAllowUndo, foNoConfirmation];
                 OperandFrom.Clear;
                 OperandTo.Clear;
                 OperandTo.Add(ExtractFilePath(fLastFrom[0]));
                 For i := 0 To fLastFrom.Count - 1 Do
                   OperandFrom.Add(IncludeTrailingPathDelimiter(fLastTo[0]) + ExtractFilename(fLastFrom[i]));
                 Result := Execute;
              End;

    foRename: IF (FLastFrom.Count = 1) And (FLastTo.Count = 1) Then
              Begin
                Operation := foRename;
                Flags := Flags + [foAllowUndo, foNoConfirmation];
                OperandFrom.Clear;
                OperandTo.Clear;
                OperandFrom.Add(fLastTo[0]);
                OperandTo.Add(fLastFrom[0]);
                Result := Execute;
              End;
  End; {Case}

  Flags := SaveFlags;
  Operation := SaveOperation;
End; {UndoExecute}


Procedure TFileOperator.ClearUndo;
Begin
  fCanUndo := False;
  fLastFrom.Clear;
  fLastTo.Clear;
End; {ClearUndo}


procedure Register;
begin
  {MP}RegisterComponents( {'Tools'}'DriveDir', [ TFileOperator ] );
end;


end.
