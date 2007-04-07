unit DragDropFiles;
{
  Description
  ===========
    TDragDropFiles is a component for simple OLE drag-and-drop operations
    with files. The component is a child-class from TDragDrop.

    I publish this component simultaneously with TDragDropFilesEx to avoid
    a brutal code breaking that could discourage some programmers. Probably,
    I won't enhance the component any longer.

    Shortcuts does only support the component TDragDropFilesEx!


  Disclaimer
  ==========
    The author disclaims all warranties, expressed or implied, including,
    without limitation, the warranties of merchantability and of fitness
    for any purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this component/unit.


  Restrictions on Using the Unit / Component
  ==========================================
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


  Contact
  =======
    homepage: http://godard.oec.uni-osnabrueck.de/student_home/dsteinwe/delphi/DietersDelphiSite.htm
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

uses DragDrop, Windows, Classes, SysUtils, ActiveX;

type
  PDropFiles = ^TDropFiles;
  TDropFiles = packed record
    pFiles: DWORD;                       { offset of file list }
    pt: TPoint;                          { drop point (client coords) }
    fNC: BOOL;                           { is it on NonClient area }
    fWide: BOOL;                         { WIDE character switch }
  end;

  TDataObjectFiles = class(TDataObject)
  private
     HDropStream:TMemoryStream;
  public
     constructor Create(StringList: TStringList);
     destructor Destroy; override;
     function RenderData(FormatEtc:TFormatEtc;
       var StgMedium: TStgMedium): HResult; override;
  end;

  TDropTargetFiles = class(TDropTarget)
  protected
     procedure AcceptDataObject(DataObj: IDataObject; var Accept:boolean); override;
  public
     constructor Create(AOwner: TDragDrop);
     destructor Destroy; override;
     procedure RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
        pt: TPoint; var dwEffect: longint); override;
  end;

  TDragDropFiles = class(TDragDrop)
  private
     FFileList:TStringList;
  protected
     function CreateDataObject:TDataObject; override;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property FileList: TStringList read FFileList write FFileList;
  end;

procedure Register;

implementation

// some local functions --------------------------------------------------------

procedure CopyAsFileList(Strs:TStrings; DataPtr: PChar; DataSize:longint);
var s:string;
    DropFiles: PDropFiles;
begin
     PChar(DropFiles):=DataPtr;
     inc(DataPtr,DropFiles^.pFiles);
     while DataPtr^<>#0 do
     begin
          if DropFiles^.FWide then
          begin
               s:=WideCharToString(PWideChar(DataPtr));
               inc(DataPtr,(Length(s)+1)*2);
          end
          else
          begin
               s:=StrPas(DataPtr);
               inc(DataPtr, Length(s)+1);
          end;
          Strs.Add(s);
     end;
end;

// TDataObjectFiles -------------------------------------------------------------

constructor TDataObjectFiles.Create(StringList: TStringList);
var FE:TFormatEtc;
    SM:TStgMedium;
    i: integer;
    df:Tdropfiles;
    pc:array[0..255] of char;
begin
     inherited Create;
     with FE do
     begin
          cfFormat:=cf_HDrop;
          ptd:=nil;
          dwAspect:=DVAspect_Content;
          lindex:=-1;
          tymed:=tymed_HGlobal;
     end;
     SetData(FE,SM,false);
     HDropStream:=TMemoryStream.Create;
     with df do
     begin
          pfiles:=sizeof(Tdropfiles);
          pt.x:=0;
          pt.y:=0;
          longint(fnc) := 0;
          longint(Fwide) := 0;
     end;
     HDropStream.Write(df,sizeof(df));
     for i:=0 to StringList.count-1 do
     begin
          strPcopy(pc,StringList[i]+#0);
          HDropStream.Write(pc,length(StringList[i])+1);
     end;
     pc[0]:=#0;
     HDropStream.Write(pc,1);
end;

destructor TDataObjectFiles.Destroy;
begin
     HDropStream.free;
     inherited Destroy;
end;

function TDataObjectFiles.RenderData(FormatEtc:TFormatEtc;
   var StgMedium: TStgMedium):HResult;
var h: HGlobal;
    p:pointer;
begin
     Result:=E_Fail;
     if FormatEtc.cfFormat=cf_HDrop then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, HDropStream.Size);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          HDropStream.Seek(0,0);
          HDropStream.Read(p^,HDropStream.Size);
          globalunlock(h);
          with StgMedium do
          begin
               tymed:=TYMED_HGLOBAL;
               hGlobal := h;
               unkForRelease := nil;
          end;
          Result:=S_OK;
     end;
end;

// TDropTargetFiles -------------------------------------------------------------

constructor TDropTargetFiles.Create(AOwner: TDragDrop);
begin
     inherited Create(AOwner);
end;

destructor TDropTargetFiles.Destroy;
begin
     inherited Destroy;
end;

procedure TDropTargetFiles.AcceptDataObject(DataObj: IDataObject;
   var Accept:boolean);
var  FE:TFormatEtc;
begin
     with FE do
     begin
          cfFormat:=cf_HDrop;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     Accept:=DataObj.QueryGetData(FE)=S_OK;
end;

procedure TDropTargetFiles.RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
   pt: TPoint; var dwEffect: longint);
var FE: TFormatEtc;
    SM: TStgMedium;
    DataSize: longint;
    DataPtr: pointer;
begin
     with FE do
     begin
          cfFormat:=CF_HDROP;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     if DataObj.GetData(FE,SM)=S_Ok then
     begin
          DataSize:=GlobalSize(SM.HGlobal);
          try
             DataPtr:=GlobalLock(SM.HGlobal);
             TDragDropFiles(FOwner).FFileList.Clear;
             CopyAsFileList(TDragDropFiles(FOwner).FFileList, DataPtr,
                DataSize);
          finally
             GlobalUnLock(SM.HGlobal);
             ReleaseStgMedium(SM);
          end;
     end;
end;

// TDragDropFiles ---------------------------------------------------------------

constructor TDragDropFiles.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FFileList:=TStringList.Create;
     FFileList.sorted:=false;
     FFileList.Duplicates:=dupIgnore;
     FDropTarget._Release;
     FDropTarget:=TDropTargetFiles.Create(self);
     SourceCompatibility:=[];
end;

destructor TDragDropFiles.Destroy;
begin
     FFileList.Free;
     inherited destroy;
end;

function TDragDropFiles.CreateDataObject:TDataObject;
begin
     if FFileList.Count>0 then Result:=TDataObjectFiles.Create(FFileList)
     else Result:=nil;
end;

// Register Component ----------------------------------------------------------

procedure Register;
begin
  {MP}RegisterComponents({'Shell32'}'DragDrop', [TDragDropFiles]);
end;

end.
