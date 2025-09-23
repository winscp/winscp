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
|   WantMappingHandle      After execution should a file mapping be returned.  |
|                          Works only, when option foMultiDestFiles used.      |
|                          Note: don't know, how to use this.                  |
|                                                                              |
+------------------------------------------------------------------------------+
| Methods:                                                                     |
|                                                                              |
|   Function Execute : Boolean;                                                |
|     Performs the copy, move, rename, or delete operation.                    |
|     Returns zero if successful or nonzero value if an error occurs.          |
|                                                                              |
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
  TFileOperator = class
{==============================================================}
  private
{==============================================================}
    FData          : TShFileOpStruct;
    FFrom          : TStringList;
    FTo            : TStringList;
    fOwner         : TWinControl;
    Procedure SetOperation( Value :TFileOperation );
    Function  GetOperation :TFileOperation;
    Function  GetWantMappingHandle :Boolean;
    Procedure SetWantMappingHandle ( Value :Boolean );
    Procedure SetFlags( Value :TFileOperationFlags );
    Function  GetFlags :TFileOperationFlags;
    Function  GetOperFlag( F :Cardinal ) :Boolean;
    Procedure SetOperFlag( F :Cardinal; V :Boolean );

{==============================================================}
  public
{==============================================================}
    Property OperandFrom      : TStringList Read fFrom Write fFrom;
    Property OperandTo        : TStringList Read FTo   Write fTo;

    Constructor Create(Owner: TWinControl);
    Destructor  Destroy; override;
    Function    Execute          : Boolean;
{==============================================================}
  published
{==============================================================}
    Property Operation : TFileOperation  Read GetOperation Write SetOperation Stored false;
    Property Flags : TFileOperationFlags Read GetFlags     Write SetFlags     Stored false;
    Property WantMappingHandle :Boolean  Read GetWantMappingHandle Write SetWantMappingHandle Stored false;
  end;

const
  FileOperatorDefaultFlags = [foAllowUndo, foNoConfirmMkDir];

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


Constructor TFileOperator.Create(Owner: TWinControl);
begin
 inherited Create;
 fFrom     := TStringList.Create;
 fTo       := TStringList.Create;
 fOwner    := Owner;
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
  IF Assigned(fOwner) and fOwner.HandleAllocated Then
    FData.Wnd := fOwner.Handle
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
  IF Assigned(FData.hNameMappings) Then
  shFreeNameMappings(THandle(FData.hNameMappings));
  inherited Destroy;
end; {Destroy}

end.
