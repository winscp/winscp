unit DragDropFilesEx;
{
  Description
  ===========
     TDragDropFilesEx is a comfortable and powerful component for COM/OLE
     drag&drop operations with files and supports completely the namespace of
     Windows (PIDL). The component is a child-class from TDragDrop.


  Disclaimer
  ==========
    The author disclaims all warranties, expressed or implied, including,
    without limitation, the warranties of merchantability and of fitness
    for any purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this component/unit.


  Restrictions on Using the Unit / Components
  ===========================================
      This unit/component is copyright 1998 by Dieter Steinwedel. ALL RIGHTS
    ARE RESERVED BY DIETER STEINWEDEL. You are allowed to use it freely
    subject to the following restrictions:

    • You are not allowed delete or alter the author's name and
      copyright in any manner

    • You are not allowed to publish a copy, modified version or
      compilation neither for payment in any kind nor freely

    • You are allowed to create a link to the download in the WWW

    • These restrictions and terms apply to you as long as until
      I alter them. Changes can found on my homepage
}

{$ALIGN ON}
{$ASSERTIONS OFF}
{$BOOLEVAL OFF}
{$DENYPACKAGEUNIT OFF}
{$EXTENDEDSYNTAX ON}
{$HINTS ON}
{$IMPORTEDDATA ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$TYPEDADDRESS OFF}
{$TYPEINFO OFF}
{$WARNINGS ON}

interface

uses
  DragDrop, Windows, Classes, SysUtils, ActiveX, PIDL, ShlObj, ComObj, Registry;

type
  PDropFiles = ^TDropFiles;
  TDropFiles = packed record
    pFiles: DWORD;                       { offset of file list }
    pt: TPoint;                          { drop point (client coords) }
    fNC: BOOL;                           { is it on NonClient area }
    fWide: BOOL;                         { WIDE character switch }
  end;

  PItemIDList = ShlObj.PItemIDList;
  TFileExMustDnD = (nvFilename, nvPIDL);
  TFileExMustDnDSet = set of TFileExMustDnD;
  TOnSpecifyDropTarget =
    procedure(Sender: TObject; DragDropHandler: Boolean; pt: TPoint; var pidlFQ: PItemIDList; var Filename: string) of object;

  PFDDListItem = ^TFDDListItem;
  TFDDListItem = record
    pidlFQ: PItemIDList;
    Name: string;
    MappedName: string;
  end;

  PCMListItem = ^TCMListItem;
  TCMListItem = record
    FirstCmd: Integer;
    LastCmd: Integer;
    CM: IContextMenu;
  end;

  TFileList = class(TList)
  private
    function Get(Index: Integer): PFDDListItem;
    procedure Put(Index: Integer; Item: PFDDListItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function Remove(Item: PFDDListItem): Integer;
    function First: PFDDListItem;
    function Last: PFDDListItem;
    function AddItem(ApidlFQ: PItemIDList; AName: string):Integer;
    function AddItemEx(ApidlFQ: PItemIDList; AName, AMappedName: string): Integer;
    function RenderPIDLs: Boolean;
    function RenderNames: Boolean;
    property Items[Index: Integer]: PFDDListItem read Get write Put;
  end;

  TDataObjectFilesEx = class(TDataObject)
  private
    pidlStream: TMemoryStream;
    HDropStream: TMemoryStream;
    FilenameMapList: TStringList;
    FilenamesAreMapped: Boolean;
    FOnRelease: TNotifyEvent;
    FPreferCopy: Boolean;
  public
    constructor Create(AFileList: TFileList; RenderPIDL, RenderFilename, PreferCopy: Boolean);
    destructor Destroy; override;
    function RenderData(FormatEtc: TFormatEtc; var StgMedium: TStgMedium): HResult; override;
    function IsValid(FormatPidl, FormatHDrop: Boolean): Boolean;
    property OnRelease: TNotifyEvent read FOnRelease write FOnRelease;
  end;

  TDropTargetFilesEx = class(TDropTarget)
  protected
    procedure AcceptDataObject(DataObj: IDataObject; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TDragDrop);
    destructor Destroy; override;
    procedure RenderDropped(DataObj: IDataObject; grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt); override;
  end;

  TShellExtension = class(TPersistent)
  private
    FDropHandler: Boolean;
    FDragDropHandler: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property DropHandler: Boolean read FDropHandler write FDropHandler default False;
    property DragDropHandler: Boolean read FDragDropHandler write FDragDropHandler default False;
  end;

  TDragDropFilesEx = class(TDragDrop)
  private
    FFileList: TFileList;
    FNeedValid: TFileExMustDnDSet;
    FCompleteFileList: Boolean;
    FFileNamesAreMapped: Boolean;
    FOnSpecifyDropTarget: TOnSpecifyDropTarget;
    FShellExtension: TShellExtension;
    FCMList: TList;
    FOnDataObjectRelease: TNotifyEvent;
    FPreferCopy: Boolean;
  protected
    function CreateDataObject:TDataObject; override;
    procedure DataObjectRelease(Sender: TObject);
    procedure DoMenuPopup(Sender: TObject; AMenu: HMenu; DataObj: IDataObject;
       AMinCustCmd: Integer; grfKeyState: LongInt; pt: TPoint); override;
    function DoMenuExecCmd(Sender: TObject; AMenu: HMenu; DataObj: IDataObject;
      Command: Integer; var dwEffect: LongInt): Boolean; override;
    procedure DoMenuDestroy(Sender: TObject; AMenu: HMenu); override;
    function DropHandler(const dataObj: IDataObject; grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TargetHasDropHandler(pidlFQ: PItemIDList; Filename: string; var dwEffect: LongInt): Boolean;
    property FileList: TFileList read FFileList write FFileList;
    property FileNamesAreMapped: Boolean read FFileNamesAreMapped;
    property PreferCopy: Boolean read FPreferCopy write FPreferCopy;
  published
    property NeedValid: TFileExMustDnDSet read FNeedValid write FNeedValid;
    property CompleteFileList: Boolean read FCompleteFileList write FCompleteFileList default True;
    property ShellExtensions: TShellExtension read FShellExtension write FShellExtension;
    property OnSpecifyDropTarget: TOnSpecifyDropTarget read FOnSpecifyDropTarget write FOnSpecifyDropTarget;
    property OnDropHandlerSucceeded;
    property OnDataObjectRelease: TNotifyEvent read FOnDataObjectRelease write FOnDataObjectRelease;
  end;

procedure Register;

implementation

uses
  Types;

const
  {$EXTERNALSYM IID_IDropTarget}
  IID_IDropTarget: TGUID = (
    D1:$00000122;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

type
  PByte = ^Byte;

// some local functions --------------------------------------------------------

procedure CopyHDropToFilelist(var List: TFileList; HDropPtr: PAnsiChar; HDropSize: LongInt);
var
  s: string;
  DropFiles: PDropFiles;
  ws: WideString;
// List must be empty, before calling ...
begin
  if (HDropPtr <> nil) and (HDropSize > 0) then
  begin
    PAnsiChar(DropFiles) := HDropPtr;
    Inc(HDropPtr,DropFiles^.pFiles);
    if DropFiles^.FWide then
    begin
      while HDropPtr^ <> #0 do
      begin
        ws := PWideChar(HDropPtr);
        Inc(HDropPtr, (Length(ws) + 1) * 2);
        List.AddItem(nil, ws);
      end;
    end
      else
    begin
      while HDropPtr^ <> #0 do
      begin
        s := string(HDropPtr);
        Inc(HDropPtr, Length(s) + 1);
        List.AddItem(nil, s);
      end;
    end;
  end;
end;

procedure CopyFilenameMapToFilelist(
  var List: TFileList; FilenameMapPtr: PChar; FilenameMapSize: LongInt; IsWideChar: Boolean);
var
  s: string;
  idx: LongInt;
  ws: WideString;
// should be only called after "CopyHDropToFilelist" ...
begin
  if (FilenameMapPtr<>nil) and (FilenameMapSize>0) then
  begin
    idx := 0;
    if IsWideChar then
    begin
      while FilenameMapPtr^ <> #0 do
      begin
        ws := WideCharToString(PWideChar(FilenameMapPtr));
        Inc(FilenameMapPtr, (Length(ws) + 1) * 2);
        if Idx >= 0 then List.Items[Idx]^.MappedName := ws
          else raise Exception.Create('A non-existing filename is mapped');
        Inc(Idx);
      end;
    end
      else
    begin
      while FilenameMapPtr^ <> #0 do
      begin
        s := StrPas(FilenameMapPtr);
        Inc(FilenameMapPtr, Length(s) + 1);
        if Idx >= 0 then List.Items[Idx]^.MappedName := s
          else raise Exception.Create('A non-existing filename is mapped');
        Inc(Idx);
      end;
    end;
  end;
end;

procedure CopyPIDLsToFilelist(var List: TFileList; pidlPtr: PByte; pidlSize: LongInt);
var
  size,i,Idx,Count: LongInt;
  pidl, pidlRoot: PItemIDList;
  LIPtr: ^LongInt;
  AddToList: Boolean;
begin
  if (pidlPtr <> nil) and (pidlSize > 0) then
  begin
    PByte(LIPtr) := pidlPtr;
    count := LIPtr^;
    AddToList := (List.Count = 0);
    Idx := 0;
    Inc(LIPtr);
    Inc(pidlPtr, LIPtr^);
    i := LIPtr^; // mempos
    pidlRoot := nil;
    Inc(LIPtr);
    while (Count > Idx) and (i < pidlSize) do
    begin
      PByte(pidl) := pidlPtr;
      size := PIDL_GetSize(pidl);
      if i = LIPtr^ then
      begin // is an item pidl ...
        if AddToList then List.AddItem(PIDL_Concatenate(pidlRoot, pidl), '')
          // PIDL_Concatenate --> waste of memory
          else List.Items[Idx]^.pidlFQ := PIDL_Concatenate(pidlRoot, pidl);
        Inc(Idx);
        Inc(LIPtr);
      end
        else pidlRoot := pidl; // is a root pidl ...
      Inc(i, size);
      Inc(pidlPtr, size);
    end;
  end;
end;

// TFileList -------------------------------------------------------------------

constructor TFileList.Create;
begin
  inherited Create;
end;

destructor TFileList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileList.Get(Index: Integer): PFDDListItem;
begin
  Result := inherited Items[Index];
end;

procedure TFileList.Put(Index: Integer; Item: PFDDListItem);
begin
  inherited Items[Index] := Item;
end;


procedure TFileList.Clear;
var
  Item: PFDDListItem;
  i: Integer;
begin
  if Count > 0 then
  begin
    for i:=0 to Count-1 do
    begin
      Item := inherited Items[i];
      if Item <> nil then
      begin
        PIDL_Free(Item^.pidlFQ);
        Dispose(Item);
      end;
    end;
  end;
  inherited Clear;
end;

procedure TFileList.Delete(Index: Integer);
var
  Item: PFDDListItem;
begin
  Item := inherited Items[Index];
  if Item <> nil then
  begin
    PIDL_Free(Item^.pidlFQ);
    Dispose(Item);
  end;
  inherited Delete(Index);
end;

function TFileList.Remove(Item: PFDDListItem): Integer;
begin
  Result := inherited Remove(Item);
  if Item <> nil then
  begin
    PIDL_Free(Item^.pidlFQ);
    Dispose(Item);
  end;
end;

function TFileList.First: PFDDListItem;
begin
  Result := inherited First;
end;

function TFileList.Last: PFDDListItem;
begin
  Result := inherited Last;
end;

function TFileList.AddItem(ApidlFQ: PItemIDList; AName: string): Integer;
var
  LI: PFDDListItem;
begin
  New(LI);
  LI^.Name := AName;
  LI^.MappedName := '';
  LI^.pidlFQ := PIDL_Copy(ApidlFQ);
  Result := Add(LI);
end;

function TFileList.AddItemEx(ApidlFQ:PItemIDList; AName, AMappedName: string): Integer;
var
  LI: PFDDListItem;
begin
  New(LI);
  LI^.Name := AName;
  LI^.MappedName := AMappedName;
  LI^.pidlFQ := PIDL_Copy(ApidlFQ);
  Result := Add(LI);
end;

function TFileList.RenderPIDLs:Boolean;
var
  i: Integer;
  piDesktop: IShellFolder;
  olePath: WideString;
  ulEaten, ulAttribs: ULong;
begin
  if Failed(SHGetDesktopFolder(piDesktop)) then
  begin
    Result := False;
  end
    else
  begin
    Result := True;
    if Count > 0 then
    begin
      for i:=0 to Count-1 do
      begin
        if (Items[i] <> nil) and (Items[i]^.pidlFQ = nil) then
        begin
          if Items[i]^.Name = '' then Result := False
            else
          begin
            olePath := Items[i]^.Name;
            ulAttribs := 0;
            if Failed(piDesktop.ParseDisplayName(0, nil, POleStr(olePath), ulEaten, Items[i]^.pidlFQ, ulAttribs)) then
            begin
              Result := False;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TFileList.RenderNames: Boolean;
var
  i: Integer;
  SF: IShellFolder;
  pc: array[0..1024] of char;
  ppidlRoot, ppidlItem: PItemIDList;
begin
  Result:=True;
  if Count>0 then
  begin
    for i:=0 to Count-1 do
    begin
      if (Items[i] <> nil) and (Items[i]^.Name = '') then
      begin
        if Items[i]^.pidlFQ = nil then Result := False
          else
        begin
          PIDL_GetRelative(Items[i]^.pidlFQ, ppidlRoot, ppidlItem);
          if PIDL_GetFileFolder(ppidlRoot, SF) then
          begin
            if PIDL_GetDisplayName(SF, ppidlItem, SHGDN_FORPARSING, pc, SizeOf(pc)) then Items[i]^.Name := StrPas(pc)
              else
            begin
              Items[i]^.Name := '';
              Result := False;
            end;
            PIDL_Free(ppidlRoot);
            PIDL_Free(ppidlItem);
          end
            else Result := False;
        end;
      end;
    end;
  end;
end;

// TDataObjectFilesEx -------------------------------------------------------------

constructor TDataObjectFilesEx.Create(AFileList: TFileList; RenderPIDL, RenderFilename, PreferCopy: Boolean);
var
  i: DWORD;
  FE: TFormatEtc;
  SM: TStgMedium;
  LastpidlRoot, pidlRoot, pidlItem: PItemIDList;
  Pos: DWORD;
  df: TDropFiles;
  pc: array[0..1024] of Char;
begin
  inherited Create;
  pidlStream := TMemoryStream.Create;
  HDropStream := TMemoryStream.Create;
  FilenameMapList := TStringList.Create;
  FilenamesAreMapped := False;
  if RenderPIDL then
  begin
    LastpidlRoot := nil;
    pidlStream.SetSize(AFileList.Count * 4 + 8);
    pidlStream.Seek(0, 0);
    i := AFileList.Count;
    pidlStream.Write(i, 4);
    i := pidlStream.Size;
    pidlStream.Write(i, 4);
    pidlStream.Seek(0, 2);
    for i := 0 to AFileList.Count - 1 do
    begin
      if AFileList.Items[i]^.pidlFQ = nil then
      begin
        pidlStream.SetSize(0);
        break;
      end;
      PIDL_GetRelative(AFileList.Items[i]^.pidlFQ, pidlRoot, pidlItem);
      if (LastpidlRoot = nil) or (not PIDL_Equal(LastpidlRoot,pidlRoot)) then
      begin
        if LastpidlRoot <> nil then PIDL_Free(LastpidlRoot);
        LastpidlRoot := PIDL_Copy(pidlRoot);
        pidlStream.Write(pidlRoot^, PIDL_GetSize(pidlRoot));
      end;
      pos := pidlStream.Position;
      pidlStream.Write(pidlItem^, PIDL_GetSize(pidlItem));
      pidlStream.seek(8 + 4 * i, soBeginning);
      pidlStream.Write(pos, 4);
      pidlStream.Seek(0, 2);
      PIDL_Free(pidlRoot);
      PIDL_Free(pidlItem);
    end;
    PIDL_Free(LastpidlRoot);
    if pidlStream.Size <> 0 then
    begin
      with FE do
      begin
        cfFormat := CF_SHELLIDLIST;
        ptd := nil;
        dwAspect := DVASPECT_CONTENT;
        lindex := -1;
        tymed := TYMED_HGLOBAL;
      end;
      SetData(FE, SM, False);
    end;
  end;
  if RenderFilename then
  begin
    with df do
    begin
      pfiles := SizeOf(TDropFiles);
      pt.x := 0;
      pt.y := 0;
      LongInt(fnc) := 0;
      LongInt(Fwide) := 1;
    end;
    HDropStream.Write(df, SizeOf(df));
    for i := 0 to AFileList.Count - 1 do
    begin
      if AFileList.Items[i]^.Name='' then
      begin
        HDropStream.SetSize(0);
        break;
      end;
      strPcopy(pc, AFileList.Items[i]^.Name + #0);
      HDropStream.Write(pc, (Length(AFileList.Items[i]^.Name) + 1) * SizeOf(pc[0]));
      FilenameMapList.Add(AFileList.Items[i]^.MappedName);
      if FilenameMapList[i]<>'' then FilenamesAreMapped := True;
    end;
    if HDropStream.Size <> 0 then
    begin
      with FE do
      begin
        cfFormat := CF_HDROP;
        ptd := nil;
        dwAspect := DVASPECT_CONTENT;
        lindex := -1;
        tymed := TYMED_HGLOBAL;
      end;
      SetData(FE, SM, False);
      pc[0] := #0;
      HDropStream.Write(pc, SizeOf(pc[0]));
    end;
    if FilenamesAreMapped then
    begin
      with FE do
      begin
        cfFormat := CF_FILENAMEMAPW;
        ptd := nil;
        dwAspect := DVASPECT_CONTENT;
        lindex := -1;
        tymed := TYMED_HGLOBAL;
      end;
      SetData(FE, SM, False);
    end;
  end;
  FPreferCopy := PreferCopy;
  if PreferCopy then
  begin
    with FE do
    begin
      cfFormat := CF_PREFERREDDROPEFFECT;
      ptd := nil;
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;
    SetData(FE, SM, False);
  end;
end;

destructor TDataObjectFilesEx.Destroy;
begin
  if Assigned(OnRelease) then OnRelease(Self);
  pidlStream.Free;
  HDropStream.Free;
  FilenameMapList.Free;
  inherited Destroy;
end;

function TDataObjectFilesEx.RenderData(FormatEtc: TFormatEtc; var StgMedium: TStgMedium): HResult;
var
  h: HGlobal;
  p: Pointer;
  FilenameMapStream: TMemoryStream;
  i: Integer;
  pc: array[0..1024] of Char;
begin
  Result := E_FAIL;
  if FormatEtc.cfFormat = CF_SHELLIDLIST then
  begin
    h := GlobalAlloc(GHND or GMEM_SHARE, pidlStream.Size);
    if h = 0 then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;
    p := GlobalLock(h);
    pidlStream.Seek(0,0);
    pidlStream.Read(p^, pidlStream.Size);
    GlobalUnlock(h);
    with StgMedium do
    begin
      tymed := TYMED_HGLOBAL;
      hGlobal := h;
      unkForRelease := nil;
    end;
    Result := S_OK;
  end;
  if FormatEtc.cfFormat = CF_HDROP then
  begin
    h := GlobalAlloc(GHND or GMEM_SHARE, HDropStream.Size);
    if h = 0 then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;
    p := GlobalLock(h);
    HDropStream.Seek(0,0);
    HDropStream.Read(p^, HDropStream.Size);
    GlobalUnlock(h);
    with StgMedium do
    begin
      tymed :=TYMED_HGLOBAL;
      hGlobal := h;
      unkForRelease := nil;
    end;
    Result := S_OK;
  end;
  if (FormatEtc.cfFormat = CF_FILENAMEMAP) or (FormatEtc.cfFormat = CF_FILENAMEMAPW) then
  begin
    FilenameMapStream := TMemoryStream.Create;
    if (FormatEtc.cfFormat=CF_FILENAMEMAPW) then
    begin
      for i := 0 to FilenameMapList.Count - 1 do
      begin
        StringToWideChar(FilenameMapList[i], PWideChar(@pc), SizeOf(pc));
        FilenameMapStream.Write(pc, Length(WideString(PWideChar(@pc))) * 2 + 2);
      end;
      pc[0]:=#0;
      pc[1]:=#0;
      FilenameMapStream.Write(pc, 2);
    end
      else
    begin
      for i := 0 to FilenameMapList.count-1 do
      begin
        strPcopy(pc,FilenameMapList[i] + #0);
        FilenameMapStream.Write(pc, Length(FilenameMapList[i]) + 1);
      end;
      pc[0] := #0;
      FilenameMapStream.Write(pc, 1);
    end;
    h := GlobalAlloc(GHND or GMEM_SHARE, FilenameMapStream.Size);
    if h = 0 then
    begin
      Result := E_OUTOFMEMORY;
      FilenameMapStream.Free;
      Exit;
    end;
    p := GlobalLock(h);
    FilenameMapStream.Seek(0,0);
    FilenameMapStream.Read(p^, FilenameMapStream.Size);
    FilenameMapStream.Free;
    GlobalUnlock(h);
    with StgMedium do
    begin
      tymed := TYMED_HGLOBAL;
      hGlobal := h;
      unkForRelease := nil;
    end;
    Result := S_OK;
  end;
  if (FormatEtc.cfFormat = CF_PREFERREDDROPEFFECT) and FPreferCopy then
  begin
    h := GlobalAlloc(GHND or GMEM_SHARE, SizeOf(DWORD));
    if h = 0 then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;
    p := GlobalLock(h);
    PDWORD(p)^ := DROPEFFECT_COPY;
    GlobalUnlock(h);
    with StgMedium do
    begin
      tymed := TYMED_HGLOBAL;
      hGlobal := h;
      unkForRelease := nil;
    end;
    Result := S_OK;
  end;
end;

function TDataObjectFilesEx.IsValid(FormatPidl, FormatHDrop: Boolean): Boolean;
begin
  Result:= not ((FormatPidl and (pidlStream.Size = 0)) or (FormatHDrop and (HDropStream.Size = 0)));
end;

// TDropTargetFilesEx -------------------------------------------------------------

constructor TDropTargetFilesEx.Create(AOwner: TDragDrop);
begin
  inherited Create(AOwner);
end;

destructor TDropTargetFilesEx.Destroy;
begin
  inherited Destroy;
end;

procedure TDropTargetFilesEx.AccepTDataObject(DataObj: IDataObject; var Accept: Boolean);
var
  FE: TFormatEtc;
  HasHDrop, HasIDList: Boolean;
begin
  Accept := False;
  with FE do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  HasHDrop := (DataObj.QueryGetData(FE) = S_OK);
  if HasHDrop or (not (nvFilename in TDragDropFilesEx(FOwner).FNeedValid)) then
  begin
    with FE do
    begin
      cfFormat := CF_SHELLIDLIST;
      ptd := nil;
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;
    HasIDList := (DataObj.QueryGetData(FE) = S_OK);
    if HasIDList or (not (nvPIDL in TDragDropFilesEx(FOwner).FNeedValid)) then
    begin
      Accept := HasIDList or HasHDrop;
    end;
  end;
end;

procedure TDropTargetFilesEx.RenderDropped(
  DataObj: IDataObject; grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt);
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  HDropSize, pidlSize, FileNameMapSize: LongInt;
  HDropPtr, pidlPtr, FileNameMapPtr: Pointer;
  HDropHandle,pidlHandle, FileNameMapHandle: THandle;
  IsWideChar: Boolean;
begin
  TDragDropFilesEx(FOwner).FFileList.Clear;
  // get "CF_HDROP" items
  with FormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  if DataObj.GetData(FormatEtc, StgMedium) = S_OK then HDropHandle := StgMedium.HGlobal
    else HDropHandle := 0;
  if HDropHandle <> 0 then
  begin
    try
      HDropSize := GlobalSize(HDropHandle);
      HDropPtr := GlobalLock(HDropHandle);
      CopyHDropToFilelist(TDragDropFilesEx(FOwner).FFileList, HDropPtr, HDropSize);
    finally
      GlobalUnLock(HDropHandle);
      ReleaseStgMedium(StgMedium);
    end;
    // CF_FILENAMEMAP makes only sense if CF_HDROP exists ...
    // get "CF_FILENAMEMAP" or "CF_FILENAMEMAPW" items
    with FormatEtc do
    begin
      cfFormat := CF_FILENAMEMAP;
      ptd := nil;
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;
    IsWideChar := False;
    FileNameMapHandle := 0;
    if DataObj.GetData(FormatEtc, StgMedium) = S_OK then FileNameMapHandle := StgMedium.HGlobal
      else
    begin
      with FormatEtc do
      begin
        cfFormat := CF_FILENAMEMAPW;
        ptd := nil;
        dwAspect := DVASPECT_CONTENT;
        lindex := -1;
        tymed := TYMED_HGLOBAL;
      end;
      if DataObj.GetData(FormatEtc, StgMedium) = S_OK then
      begin
        FileNameMapHandle := StgMedium.HGlobal;
        IsWideChar := True;
      end;
    end;
    if FileNameMapHandle <> 0 then
    begin
      TDragDropFilesEx(FOwner).FFileNamesAreMapped := True;
      try
        FileNameMapSize := GlobalSize(FileNameMapHandle);
        FileNameMapPtr := GlobalLock(FileNameMapHandle);
        CopyFileNameMapToFilelist(TDragDropFilesEx(FOwner).FFileList, FileNameMapPtr, FileNameMapSize, IsWideChar);
     finally
        GlobalUnLock(FileNameMapHandle);
        ReleaseStgMedium(StgMedium);
      end;
    end
      else TDragDropFilesEx(FOwner).FFileNamesAreMapped := False;
  end
    else TDragDropFilesEx(FOwner).FFileNamesAreMapped := False;
  // get "CF_SHELLIDLIST" items
  with FormatEtc do
  begin
    cfFormat := CF_SHELLIDLIST;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  if DataObj.GetData(FormatEtc, StgMedium) = S_OK then pidlHandle := StgMedium.HGlobal
    else pidlHandle := 0;
  if pidlHandle <> 0 then
  begin
    try
      pidlSize := GlobalSize(pidlHandle);
      PidlPtr := GlobalLock(pidlHandle);
      CopyPIDLsToFilelist(TDragDropFilesEx(FOwner).FFileList, pidlPtr, pidlSize);
    finally
      GlobalUnLock(pidlHandle);
      ReleaseStgMedium(StgMedium);
    end;
  end;

  dwEffect := GetPreferredDropEffect(DataObj);
end;

// TShellExtension ---------------------------------------------------

procedure TShellExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TShellExtension then
  begin
    with TShellExtension(Dest) do
    begin
      FDropHandler := Self.FDropHandler;
      FDragDropHandler := Self.FDragDropHandler;
    end;
  end
    else inherited AssignTo(Dest);
end;

// TDragDropFilesEx ---------------------------------------------------------------

constructor TDragDropFilesEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileList := TFileList.Create;
  FDropTarget._Release;
  FDropTarget := TDropTargetFilesEx.Create(self);
  FCompleteFileList := True;
  SourceCompatibility := [];
  FFileNamesAreMapped := False;
  FCMList := TList.Create;
  FShellExtension := TShellExtension.Create;
  FPreferCopy := False;
end;

destructor TDragDropFilesEx.Destroy;
begin
  FCMList.Free;
  FFileList.Free;
  FShellExtension.Free;
  inherited;
end;

procedure TDragDropFilesEx.DataObjectRelease(Sender: TObject);
begin
  if Assigned(OnDataObjectRelease) then OnDataObjectRelease(Self);
end;

function TDragDropFilesEx.CreateDataObject: TDataObject;
var
  DataObject: TDataObjectFilesEx;
  RFName, RPidl: Boolean;
begin
  Result := nil;
  if FCompleteFileList then
  begin
    RFName := FFileList.RenderNames;
    RPidl := FFileList.RenderPIDLs;
    if ((nvFilename in FNeedValid) and (not RFName)) or
       ((nvPIDL in FNeedValid) and (not RPidl)) then
    begin
      exit;
    end;
  end
    else
  begin
    RFName := True;
    RPidl := True;
  end;
  if FFileList.Count > 0 then
  begin
    DataObject := TDataObjectFilesEx.Create(FFileList, RPidl, RFname, FPreferCopy);
    DataObject.OnRelease := DataObjectRelease;
    if not DataObject.IsValid((nvPIDL in FNeedValid), (nvFilename in FNeedValid)) then DataObject._Release
      else Result := DataObject;
  end;
end;

procedure TDragDropFilesEx.DoMenuPopup(
  Sender: TObject; AMenu: HMenu; DataObj: IDataObject; AMinCustCmd:Integer; grfKeyState: LongInt; pt: TPoint);
var
  StringList: TStringList;
  Reg: TRegistry;
  pidlFQ: PItemIDList;
  FileName: string;

  procedure CreateDragDropHandler(GUID:string);
  var
    Unknown: IUnknown;
    ShellExtInit: IShellExtInit;
    CMListItem: PCMListItem;
  begin
    try
      Unknown := CreateComObject(StringToGUID(GUID));
    except
      Unknown := nil;
    end;
    try
      if Assigned(Unknown) and
         (Unknown.QueryInterface(IID_IShellExtInit, ShellExtInit) = S_OK) then
      begin
        if ShellExtInit.Initialize(pidlFQ, DataObj, 0) = NoError then
        begin
          New(CMListItem);
          if ShellExtInit.QueryInterface(IID_IContextMenu, CMListItem^.CM) = S_OK then
          begin
            CMListItem^.FirstCmd := AMinCustCmd;
            CMListItem^.LastCmd := AMinCustCmd;
            Inc(CMListItem^.LastCmd, CMListItem^.CM.QueryContextMenu(AMenu, 0, CMListItem^.FirstCmd, $7FFF, CMF_NORMAL));
            if CMListItem^.LastCmd = CMListItem^.FirstCmd then Dispose(CMListItem)
              else
            begin
              AMinCustCmd := CMListItem^.LastCmd;
              FCMList.Add(CMListItem);
            end;
          end;
        end;
      end;
    finally
      Unknown := nil;
      ShellExtInit := nil;
    end;
  end;

begin
  if Assigned(FOnSpecifyDropTarget) and FShellExtension.FDragDropHandler then
  begin
    pidlFQ := nil;
    FOnSpecifyDropTarget(self, True, DragDropControl.ScreenToClient(pt),
       pidlFQ, Filename);
    if pidlFQ=nil then pidlFQ:=PIDL_GetFromPath(PChar(Filename))
    else pidlFQ:=PIDL_Copy(pidlFQ);
    StringList:=TStringList.Create;
    Reg:=TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKey('Folder\ShellEx\DragDropHandlers', False) then
      begin
        Reg.GetKeyNames(StringList);
        Reg.CloseKey;
        while StringList.Count>0 do
        begin
          { The documentation for the drag-and-drop handlers varies
            between many registry-keys, where you find the handlers.
            I think, the correct position is "Folder"; "Directory"
            should be the key for system-folders! Even, I have found
            in the documentation, that you can define drag-and-drop
            handlers for the system-folder "printers". Till now, it
            doesn't make sense to me. Therefore, I haven't implemented
            it }
          if Reg.OpenKey('Folder\ShellEx\DragDropHandlers\' + StringList[StringList.Count - 1], False) then
          begin
            CreateDragDropHandler(Reg.ReadString(''));
            Reg.CloseKey;
          end;
          StringList.Delete(StringList.Count - 1);
        end;
      end;
    finally
      Stringlist.Free;
      Reg.Free;
      PIDL_Free(pidlFQ);
    end;
  end;
  inherited DoMenuPopup(Sender, AMenu, DataObj, AMinCustCmd, grfKeyState, pt);
end;

function TDragDropFilesEx.DoMenuExecCmd(
  Sender: TObject; AMenu: HMenu; DataObj: IDataObject; Command: Integer; var dwEffect: LongInt): Boolean;
var
  ICM: TCMInvokeCommandInfo;
  i: Integer;
  CMListItem: PCMListItem;
begin
  Result := False;
  try
    if FCMList.Count>0 then
    begin
      for i := 0 to FCMList.Count-1 do
      begin
        CMListItem := FCMList.Items[i];
        if (CMListItem^.FirstCmd <= Command) and
           (CMListItem^.LastCmd > Command) then
        begin
          FillChar(ICM, SizeOf(TCMInvokeCommandInfo), #0);
          ICM.cbSize := SizeOf(TCMInvokeCommandInfo);
          ICM.hwnd := DragDropControl.Handle;
          ICM.lpVerb := MakeIntResourceA(Command-CMListItem^.FirstCmd);
          ICM.nShow := SW_SHOWNORMAL;
          Result := (CMListItem^.CM.InvokeCommand(ICM) = NOERROR);
          break;
        end;
      end;
    end;
  finally
    if not Result then
    begin
      Result := inherited DoMenuExecCmd(Sender, AMenu, DataObj, Command, dwEffect);
    end;
  end;
end;

procedure TDragDropFilesEx.DoMenuDestroy(Sender: TObject; AMenu: HMenu);
var
  CMListItem: PCMListItem;
begin
  while FCMList.Count > 0 do
  begin
    CMListItem := FCMList.Items[FCMList.Count - 1];
    CMListItem^.CM := nil;
    Dispose(CMListItem);
    FCMList.Delete(FCMList.Count - 1);
  end;
  inherited DoMenuDestroy(Sender, AMenu);
end;

function TDragDropFilesEx.TargetHasDropHandler(
  pidlFQ: PItemIDList; Filename: string; var dwEffect: LongInt): Boolean;
var
  ppidlFQ, ppidlRoot, ppidlItem: PItemIDList;
  SF: IShellFolder;
  DT: IDropTarget;
begin
  try
    Result := False;
    ppidlFQ := nil;
    ppidlRoot := nil;
    ppidlItem := nil;
    if pidlFQ = nil then ppidlFQ := PIDL_GetFromPath(PChar(Filename))
      else ppidlFQ := PIDL_Copy(pidlFQ);
    PIDL_GetRelative(ppidlFQ, ppidlRoot, ppidlItem);
    PIDL_GetFileFolder(ppidlRoot, SF);
    if Assigned(SF) then
    begin
      SF.GetUIObjectOf(0,1,ppidlItem,IID_IDropTarget,nil,Pointer(DT));
      Result := Assigned(DT) and FileExists(Filename);
      if Assigned(DT) and (dwEffect and not (DROPEFFECT_SCROLL or DROPEFFECT_LINK) = DROPEFFECT_NONE) then
      begin
        dwEffect := dwEffect and not DROPEFFECT_LINK;
      end;
    end;
  finally
    SF := nil;
    DT := nil;
    PIDL_Free(ppidlRoot);
    PIDL_Free(ppidlItem);
    PIDL_Free(ppidlFQ);
  end;
end;

function TDragDropFilesEx.DropHandler(
  const dataObj: IDataObject; grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt): Boolean;
var
  pidlFQ, ppidlFQ, ppidlRoot, ppidlItem: PItemIDList;
  SF: IShellFolder;
  DT: IDropTarget;
  Filename: string;
  pc: array[0..1024] of Char;
begin
  try
    Result := False;
    ppidlRoot := nil;
    ppidlItem := nil;
    ppidlFQ := nil;
    if ShellExtensions.FDropHandler and Assigned(FOnSpecifyDropTarget) then
    begin
      pidlFQ := nil;
      FOnSpecifyDropTarget(Self, False, DragDropControl.ScreenToClient(pt), pidlFQ, Filename);
      if pidlFQ = nil then ppidlFQ := PIDL_GetFromPath(PChar(Filename))
        else ppidlFQ := PIDL_Copy(pidlFQ);
      PIDL_GetRelative(ppidlFQ, ppidlRoot, ppidlItem);
      PIDL_GetFileFolder(ppidlRoot, SF);
      if Assigned(SF) then
      begin
        if (Filename = '') and
           PIDL_GetDisplayName(SF, ppidlItem, SHGDN_FORPARSING, pc, SizeOf(pc)) then
        begin
          Filename := StrPas(pc);
        end;
        SF.GetUIObjectOf(0, 1, ppidlItem, IID_IDropTarget, nil, Pointer(DT));
        if FileExists(Filename) and Assigned(DT) then
        begin
          DT.DragEnter(DataObj, grfKeyState, pt, dwEffect);
          Result := (DT.Drop(DataObj, grfKeyState, pt, dwEffect) = NOERROR);
        end;
      end;
    end;
  finally
    SF := nil;
    DT := nil;
    PIDL_Free(ppidlRoot);
    PIDL_Free(ppidlItem);
    PIDL_Free(ppidlFQ);
  end;
end;

// Register Component ----------------------------------------------------------

procedure Register;
begin
  RegisterComponents('DragDrop', [TDragDropFilesEx]);
end;

end.
