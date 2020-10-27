Unit ShellDialogs;
{==================================================================
 Unit ShellDialogs / Version 1.0 / 06.1999
 ==================================================================


    Description:
    ============
    ShellDisplayContextMenu displays the shell's contextmenu for a
    file or directory or for multiple files.

    ShellExecuteContextCommand performs a contextmenu-action for a
    file or directory or for multiple files.


    Author:
    =======
    (c) Ingo Eckel 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany

    Credits:
    ========
    This unit is partly based on the work of
    Gerald Nunn (GXExplorer)
    and
    Brad Stowers (ItemProp)


 ==================================================================}


{------------------------------------------------------------------
 You must pass fully qualified path names to all of these functions.
 If you are calling for a subdirectory (i.e. no filename), it is your
 responsibility to insure that subdirectories contain !NO! trailing
 backslash. Root-directories must be passed as 'C:\'.

 ShellDisplayContextMenu displays the right click menu for the given file or
 directory and processes the item selected, if any.  Parent is the window
 handle for the owning window of any error messages that may need to be
 displayed by the system, MyForm.Handle is generally fine.  Pos is the X, Y
 position to display the menu at given in screen (absolute) coordinates.
 ------------------------------------------------------------------}

Interface

Uses Windows, ShlObj, SysUtils, Classes, Messages, Menus,
     PIDL;

{Commands for ShellExecuteContextCommand:}
Const shcProperties = 'properties';
      shcCut        = 'cut';
      shcCopy       = 'copy';
      shcPaste      = 'paste';
      shcDelete     = 'delete';
      shcLink       = 'link';
      shcrename     = 'rename';
      shcDefault    = '';

Type PPIDLArray = ^TPIDLArray;
     TPIDLArray = Array [0..0] of PItemIDList;

{Display the shell's contextmenu to a file or directory.
 Requires the iShellFolder-interface to the parent directory and the
 PIDLs of the files or directories:}
Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             ShellFolder: IShellFolder;
                             PIDLCount: Integer;
                             Var PIDL: PItemIDList;
                             AllowRename : Boolean;
                             Var Verb: String;
                             PerformPaste : Boolean = True); Overload;

{Display the shell's contextmenu to single file or directory.
 Requires the full qualified name of the file or directory:}
Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             FileName : String;
                             AllowRename : Boolean;
                             Var Verb: String;
                             PerformPaste : Boolean = True); Overload;

{Display the shell's contextmenu to muliple files or directories.
 Requires the full qualified name of the parent directory and the
 filenames of the files or directories as TStringList:}
Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             Path  : String;
                             Files : TStringList;
                             Var Verb: String;
                             PerformPaste : Boolean = True); Overload;


{Performs a contextmenu-command (properties, copy, cut, paste) for files or directories.
 Requires the iShellFolder-interface to the parent directory and the
 PIDLs of the files or directories:}
Function ShellExecuteContextCommand(Handle: THandle;
                                    Command: String;
                                    ShellFolder: IShellFolder;
                                    PIDLCount: Integer;
                                    Var PIDL: PItemIDList): Boolean; Overload;

{Performs a contextmenu-command (properties, copy, cut, paste) for a file or directory.
 Requires the full qualified name of the file or directory:}
Function ShellExecuteContextCommand(Handle: THandle;
                                    Command: String;
                                    FileName : String): Boolean; Overload;


{Performs a contextmenu-command (properties, copy, cut, paste) for
 multiple files or directories.
 Requires the full qualified name of the parent directory and the
 filenames of the files or directories as TStringList:}
Function ShellExecuteContextCommand(Handle: THandle;
                                     Command : String;
                                     Path  : String;
                                     Files : TStringList) : Boolean; Overload;

Var CustomContextMenu : TPopupMenu;

{------------------------------------------------------------------}
Implementation
{------------------------------------------------------------------}

{$R-}



function MenuCallbackProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LResult; stdcall; export;
var CM2: IContextMenu2;

begin
  case Msg of
    WM_CREATE:
      begin
        CM2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
        SetWindowLong(Wnd, GWL_USERDATA, LongInt(CM2));
        Result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;

    // these are the biggies -- the messages that IContextMenu2::HandlMenuMsg is
    // supposed to handle.
    WM_DRAWITEM,
    WM_MEASUREITEM,
    WM_INITMENUPOPUP:
      begin
        begin
          CM2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
          Assert(CM2 <> NIL, 'NIL Context Menu!');
          CM2.HandleMenuMsg(Msg, wParam, lParam);
        end;
        if Msg = WM_INITMENUPOPUP then
          Result := 0
        else
          Result := 1;
      end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end; {MenuCallBackProc}


Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             ShellFolder: IShellFolder;
                             PIDLCount: Integer;
                             Var PIDL: PItemIDList;
                             AllowRename : Boolean;
                             Var Verb: String;
                             PerformPaste : Boolean = True);

Const CallBackClassName = 'ShellDialogsCallBack';
      CallBackProcessor = 'ShellDialogsCallBackProcessor';
      MaxStdEntries     = 1000;

Var PopupMenu    : HMenu;
    Cmd          : Cardinal;
    ICM          : TCMInvokeCommandInfo;
    ContextMenu  : IContextMenu;
    ContextMenu2 : IContextMenu2;
    Flags        : UINT;
    AWndClass    : TWndClass;
    CallbackWnd  : HWnd;
    MenuHandle   : HWnd;
    i            : Integer;

Begin
  Verb := EmptyStr;
  CallBackWnd := 0;
  If AllowRename then
    Flags := CMF_EXPLORE Or CMF_CANRENAME
  Else
    Flags := CMF_EXPLORE;

  PopupMenu := CreatePopupMenu;
  Try
    If Succeeded(ShellFolder.GetUIObjectOf(Handle, PIDLCount, PIDL, IID_IContextMenu, NIL, Pointer(ContextMenu))) Then
    Begin
      ContextMenu._AddRef;
      If Succeeded(ContextMenu.QueryInterface(IID_IContextMenu2, ContextMenu2)) Then
      Begin
        ContextMenu2._AddRef;
        Try
          ContextMenu2.QueryContextMenu(PopupMenu, 0, 1, MaxStdEntries, Flags);
        Except
          Exit;
        End;
        FillChar(AWndClass, SizeOf(AWndClass), #0);
        AWndClass.lpszClassName := CallBackClassName;
        AWndClass.Style := CS_PARENTDC;
        AWndClass.lpfnWndProc := @MenuCallbackProc;
        AWndClass.hInstance := HInstance;
        Windows.RegisterClass(AWndClass);
        CallbackWnd := CreateWindow(CallBackClassName, CallBackProcessor, WS_POPUPWINDOW, 0, 0, 0, 0, 0, 0, HInstance, Pointer(ContextMenu2));
      End
      Else
      Try
        ContextMenu.QueryContextMenu(PopupMenu, 0, 1, $7FFF, Flags);
      Except
        Exit;
      End;

      If CallbackWnd = 0 then
      MenuHandle := Handle
      else
      MenuHandle := CallbackWnd;


      IF Assigned(CustomContextMenu) And (CustomContextMenu.Items.Count > 0) Then
      Begin
        AppendMenu(PopupMenu, MF_SEPARATOR, 0, NIL);
        For i := 0 To CustomContextMenu.Items.Count -1 Do
        With CustomContextMenu.Items[i] Do
        IF Visible And Assigned(OnClick) Then
        Begin
          Flags := MF_STRING;
          IF Checked Then
            Flags := Flags Or MF_CHECKED;
          AppendMenu(PopUpMenu, Flags, MaxStdEntries + i + 1, PChar(Caption));
        End;
      End;

      Cmd := Cardinal(TrackPopupMenuEx(PopupMenu,TPM_LEFTALIGN Or TPM_RIGHTBUTTON Or TPM_RETURNCMD, P.X, P.Y, MenuHandle, NIL));
      IF Cmd > MaxStdEntries Then
      Begin
        IF Assigned(CustomContextMenu.Items[Cmd - MaxStdEntries - 1].OnClick) Then
          CustomContextMenu.Items[Cmd - MaxStdEntries - 1].OnClick(NIL);
      End
      Else
      if Cmd > 0 then
      Begin
        SetLength(Verb, 255);
        If Assigned(ContextMenu2) then
          ContextMenu2.GetCommandString(Cardinal(MakeIntResource(Cmd-1)), GCS_VERB, NIL,PAnsiChar(PChar(Verb)), Length(Verb))
        Else
          ContextMenu.GetCommandString(Cardinal(MakeIntResource(Cmd-1)),GCS_VERB, NIL, PAnsiChar(PChar(Verb)), Length(Verb));

        SetLength(Verb, strlen(PChar(Verb)));
        Verb := LowerCase(Verb);

        If (Verb <> shcRename) And (PerformPaste Or (Verb <> shcPaste)) Then
        Begin
          FillChar(ICM,SizeOf(TCMInvokeCommandInfo),#0);
          ICM.cbSize := Sizeof(TCMInvokeCommandInfo);
          ICM.hwnd   := Handle;
          ICM.lpVerb := LPCSTR(MakeIntResource(Cmd-1));
          ICM.nShow  := SW_SHOWNORMAL;
          Try
            If Assigned(ContextMenu2) Then
              ContextMenu2.InvokeCommand(ICM)
            Else
              ContextMenu.InvokeCommand(ICM);
          Except
            // eat any dammned shell exceptions!
            Exit;
          End;
        End;
      End;
    End;
  Finally
    DestroyMenu(PopupMenu);
    If CallbackWnd <> 0 Then
      DestroyWindow(CallbackWnd);
    IF Assigned(ContextMenu2) Then
    ContextMenu2._Release;
    IF Assigned(ContextMenu) Then
    ContextMenu._Release;
  End;
End; {ShellDisplayContextMenu (PIDL) }


Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             FileName : String;
                             AllowRename : Boolean;
                             Var Verb: String;
                             PerformPaste : Boolean = True);

Var ShellFolder       : iShellFolder;
    DirPIDL           : PItemIDList;
    DirPIDLFQ         : PItemIDList;
    ParentPIDL        : PItemIDList;

Begin
  DirPIDL    := NIL;
  DirPIDLFQ  := NIL;
  ParentPIDL := NIL;

  DirPidlFQ := PIDL_GetFromPath(PChar(FileName));
  IF Assigned(DirPIDLFQ) Then
  Begin
    PIDL_GetRelative(DirPIDLFQ, ParentPIDL, DirPIDL);
    Try
      If PIDL_GetFileFolder(ParentPIDL, ShellFolder) Then
        ShellDisplayContextMenu(Handle, P, ShellFolder, 1, DirPIDL, AllowRename, Verb, PerformPaste);
    Finally
      PIDL_Free(DirPIDL);
      PIDL_Free(DirPIDLFQ);
      PIDL_Free(ParentPIDL);
    End;
  End;
End; {ShellDisplayContextMenu (Filename) }


Procedure ShellDisplayContextMenu(Handle: THandle;
                             P: TPoint;
                             Path  : String;
                             Files : TStringList;
                             Var Verb: String;
                             PerformPaste : Boolean = True);

Var ShellFolder       : iShellFolder;
    PathPIDL          : PItemIDList;
    PIDLArray         : PPIDLArray;
    Index             : Integer;
    i                 : Integer;

Begin
  IF Files.Count = 0 Then
  Exit;
  Index := 0;
  GetMem(PIDLArray, SizeOf(PItemIDList) * Files.Count);
  FillChar(PIDLArray^, Sizeof(PItemIDList) * Files.Count, #0);
  Try
    PathPIDL := PIDL_GetFromPath(PChar(Path));
    IF Assigned(PathPIDL) Then
    Begin
      Try
        If PIDL_GetFileFolder(PathPIDL, ShellFolder) Then
        Begin
          For i := 0 To Files.Count - 1 Do
          Begin
            PIDLArray^[i] := PIDL_GetFromParentFolder(ShellFolder, PChar(Files[i]));
            IF Assigned(PIDLArray^[i]) Then
            INC(Index);
          End;
          IF Index > 0 Then
          Begin
            Try
              ShellDisplayContextMenu(Handle, P, ShellFolder, Index, PIDLArray^[0], False, Verb, PerformPaste);
            Finally
              For i := 0 To Index - 1 Do
              PIDL_Free(PIDLArray[i]);
            End;
          End;
        End;
      Finally
        PIDL_Free(PathPIDL);
      End;
    End;
  Finally
    FreeMem(PIDLArray);
  End;
End; {ShellDisplayContextMenu (TStringList) }


Function ShellExecuteContextCommand(Handle: THandle; Command: String; ShellFolder: IShellFolder; PIDLCount: Integer; Var PIDL: PItemIDList): Boolean;
Var ICM         : TCMInvokeCommandInfoEx;
    ContextMenu : IContextMenu;
    ContextMenu2: IContextMenu2;
    Popup       : HMenu;
    MenuCmd     : Cardinal;
    HRes        : HResult;

Begin
  Result := False;
  IF Succeeded(ShellFolder.GetUIObjectOf(Handle, PIDLCount, PIDL, IID_IContextMenu, NIL, Pointer(ContextMenu))) Then
  Begin
    ContextMenu.QueryInterface(IID_IContextMenu2, ContextMenu2);
    FillChar(ICM,SizeOf(TCMInvokeCommandInfo), #0);
    ICM.hwnd   := Handle;
    ICM.cbSize := SizeOf(TCMInvokeCommandInfo);
    ICM.nShow  := SW_SHOWNORMAL;
    IF Command <> shcDefault Then
    begin
      ICM.fMask := CMIC_MASK_UNICODE;
      ICM.lpVerb := PAnsiChar(AnsiString(Command));
      ICM.lpVerbW := PChar(Command);
    end
    Else
    Begin
      {Locate the menuitem for the default action:}
      Popup := CreatePopupMenu;
      Try
        Try
          IF Assigned(ContextMenu2) Then
          Hres := ContextMenu2.QueryContextMenu(Popup, 0, 1, $7FFF, CMF_DEFAULTONLY)
          Else
          Hres := ContextMenu.QueryContextMenu(Popup, 0, 1, $7FFF, CMF_DEFAULTONLY);
        Except
          Exit;
        End;
        If Succeeded(HRes) Then
        Begin
          MenuCmd := GetMenuDefaultItem(Popup, 0, 0);
          If MenuCmd <> $FFFFFFFF then
            ICM.lpVerb := LPCSTR(MakeIntResource(MenuCmd-1))
          Else
            ICM.lpVerb := NIL;
        end;
      finally
        DestroyMenu(Popup);
      end;
    End;

    Try
      If Assigned(ContextMenu2) then
        Result := Succeeded(ContextMenu2.InvokeCommand(PCMInvokeCommandInfo(@ICM)^))
      Else
        Result := Succeeded(ContextMenu.InvokeCommand(PCMInvokeCommandInfo(@ICM)^));
    Except
      // eat any dammned shell exceptions.
    End;
  End;
End; {ShellExecuteContextCommand (PIDL)}



Function ShellExecuteContextCommand(Handle: THandle; Command: String; FileName : String): Boolean;
Var ShellFolder       : iShellFolder;
    DirPIDL           : PItemIDList;
    DirPIDLFQ         : PItemIDList;
    ParentPIDL        : PItemIDList;

Begin
  DirPIDL    := NIL;
  DirPIDLFQ  := NIL;
  ParentPIDL := NIL;
  Result := False;

  DirPidlFQ := PIDL_GetFromPath(PChar(FileName));
  IF Assigned(DirPIDLFQ) Then
  Begin
    PIDL_GetRelative(DirPIDLFQ, ParentPIDL, DirPIDL);
    Try
      If PIDL_GetFileFolder(ParentPIDL, ShellFolder) Then
        Result := ShellExecuteContextCommand(Handle, Command, ShellFolder, 1, DirPIDL);
    Finally
      PIDL_Free(DirPIDL);
      PIDL_Free(DirPIDLFQ);
      PIDL_Free(ParentPIDL);
    End;
  End;
End; {ShellExecuteContextCommand (FileName)}


Function ShellExecuteContextCommand(Handle: THandle;
                                    Command : String;
                                    Path  : String;
                                    Files : TStringList) : Boolean;

Var ShellFolder       : iShellFolder;
    PathPIDL          : PItemIDList;
    PIDLArray         : PPIDLArray;
    Index             : Integer;
    i                 : Integer;

Begin
  Result := False;
  IF Files.Count = 0 Then
  Exit;
  Index := 0;
  GetMem(PIDLArray, SizeOf(PItemIDList) * Files.Count);
  FillChar(PIDLArray^, Sizeof(PItemIDList) * Files.Count, #0);
  Try
    PathPIDL := PIDL_GetFromPath(PChar(Path));
    IF Assigned(PathPIDL) Then
    Begin
      Try
        If PIDL_GetFileFolder(PathPIDL, ShellFolder) Then
        Begin
          For i := 0 To Files.Count - 1 Do
          Begin
            PIDLArray^[i] := PIDL_GetFromParentFolder(ShellFolder, PChar(Files[i]));
            IF Assigned(PIDLArray^[i]) Then
            INC(Index);
          End;
          IF Index > 0 Then
          Begin
            Try
              Result := ShellExecuteContextCommand(Handle, Command, ShellFolder, Index, PIDLArray^[0]);
            Finally
              For i := 0 To Index - 1 Do
              PIDL_Free(PIDLArray^[i]);
            End;
          End;
        End;
      Finally
        PIDL_Free(PathPIDL);
      End;
    End;
  Finally
    FreeMem(PIDLArray);
  End;
End; {ShellExecuteContextCommand (TStringList) }


End.
