unit DragDropURL;
{
  Description
  ===========
    TDragDropURL is a component for simple OLE drag-and-drop operations
    with URLs. The component is a child-class from TDragDrop.


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

uses DragDrop, Windows, Classes, SysUtils, ActiveX, PIDL, ShlObj;

type
  TDataObjectURL = class(TDataObject)
  private
     URLStream:TMemoryStream;
     FGDStream:TMemoryStream;
  public
     constructor Create(ScrapFileName, URL:string; Scrap:boolean);
     destructor Destroy; override;
     function RenderData(FormatEtc:TFormatEtc;
        var StgMedium: TStgMedium):HResult; override;
  end;

  TDropTargetURL = class(TDropTarget)
  protected
     procedure AcceptDataObject(DataObj: IDataObject; var Accept:boolean); override;
  public
     constructor Create(AOwner: TDragDrop);
     destructor Destroy; override;
     procedure RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
        pt: TPoint; var dwEffect: longint); override;
  end;

  TDragDropURL = class(TDragDrop)
  private
     FURL:String;
     FScrapFileName:string;
  protected
     function CreateDataObject:TDataObject; override;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property URL: String read FURL write FURL;
     property ScrapFileName: string read FScrapFileName write FScrapFileName;
  end;

procedure Register;

implementation

// TDataObjectURL -------------------------------------------------------------

const HLineSize=24;

constructor TDataObjectURL.Create(ScrapFileName, URL:string; Scrap:boolean);
var FE:TFormatEtc;
    SM:TStgMedium;
    pc:array[0..255] of char;
    FDescriptor:TFILEGROUPDESCRIPTOR;
begin
     inherited Create;
     with FE do
     begin
          cfFormat:=CF_SHELLURL;
          ptd:=nil;
          dwAspect:=DVAspect_Content;
          lindex:=-1;
          tymed:=tymed_HGlobal;
     end;
     SetData(FE,SM,false);
     FE.cfFormat:=cf_Text;
     SetData(FE,SM,false);
     if Scrap then
     begin
          FE.cfFormat:=CF_FILEDESCRIPTOR;
          SetData(FE,SM,false);
          FE.cfFormat:=CF_FILECONTENTS;
          FE.lindex:=0;
          SetData(FE,SM,false);
     end;
     URLStream:=TMemoryStream.Create;
     Fillchar(pc,sizeof(pc),#0);
     pc:='[InternetShortcut]'#13#10'URL=';
     URLStream.Write(pc,HLineSize);
     Fillchar(pc,sizeof(pc),#0);
     strPcopy(pc,URL+#0);
     URLStream.Write(pc,length(URL)+1);
     FGDStream:=TMemoryStream.Create;
     FDescriptor.cItems:=1;
     with FDescriptor.fgd[0] do
     begin
          dwFlags:=FD_LinkUI;
          FillChar(cFileName,sizeof(cFileName),#0);
          if ScrapFileName<>'' then
          begin
               if CompareText(ExtractFileExt(ScrapFileName),'.url')<>0 then
                  ScrapFileName:=ScrapFileName+'.url';
               strPcopy(cFileName,ScrapFileName+#0);
          end
          else cFileName:='URL Link.url';
     end;
     FGDStream.Write(FDescriptor,SizeOf(FDescriptor));
end;

destructor TDataObjectURL.Destroy;
begin
     URLStream.free;
     FGDStream.free;
     inherited Destroy;
end;

function TDataObjectURL.RenderData(FormatEtc:TFormatEtc;
        var StgMedium: TStgMedium):HResult;
var h: HGlobal;
    p:pointer;
begin
     Result:=E_Fail;
     if (FormatEtc.cfFormat=cf_Text) or (FormatEtc.cfFormat=CF_SHELLURL) then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, URLStream.Size-HLineSize);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          URLStream.Seek(HLineSize,0);
          URLStream.Read(p^,URLStream.Size-HLineSize);
          globalunlock(h);
          with StgMedium do
          begin
               tymed:=TYMED_HGLOBAL;
               hGlobal := h;
               unkForRelease := nil;
          end;
          Result:=S_OK;
     end;
     if (FormatEtc.cfFormat=CF_FILECONTENTS) then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, URLStream.Size);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          URLStream.Seek(0,0);
          URLStream.Read(p^,URLStream.Size);
          globalunlock(h);
          with StgMedium do
          begin
               tymed:=TYMED_HGLOBAL;
               hGlobal := h;
               unkForRelease := nil;
          end;
          Result:=S_OK;
     end;
     if (FormatEtc.cfFormat=CF_FILEDESCRIPTOR) then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, FGDStream.Size);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          FGDStream.Seek(0,0);
          FGDStream.Read(p^,FGDStream.Size);
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

// TDropTargetURL -------------------------------------------------------------

constructor TDropTargetURL.Create(AOwner: TDragDrop);
begin
     inherited Create(AOwner);
end;

destructor TDropTargetURL.Destroy;
begin
     inherited Destroy;
end;

procedure TDropTargetURL.AccepTDataObject(DataObj: IDataObject;
   var Accept:boolean);
var  FE:TFormatEtc;
begin
     with FE do
     begin
          cfFormat:=CF_SHELLURL;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     Accept:=DataObj.QueryGetData(FE)=S_OK;
end;

procedure TDropTargetURL.RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
   pt: TPoint; var dwEffect: longint);
var FE: TFormatEtc;
    SM: TStgMedium;
    DataPtr: pchar;
begin
     with FE do
     begin
          cfFormat:=CF_SHELLURL;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     if DataObj.GetData(FE,SM)=S_Ok then
     begin
          try
             DataPtr:=GlobalLock(SM.HGlobal);
             TDragDropURL(FOwner).FURL:=StrPas(DataPtr);
          finally
             GlobalUnLock(SM.HGlobal);
             ReleaseStgMedium(SM);
          end;
     end;
end;

// TDragDropURL ---------------------------------------------------------------

constructor TDragDropURL.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FURL:='';
     FScrapFileName:='';
     FDropTarget._Release;
     FDropTarget:=TDropTargetURL.Create(self);
end;

destructor TDragDropURL.Destroy;
begin
     inherited destroy;
end;

function TDragDropURL.CreateDataObject:TDataObject;
begin
     if FURL<>'' then Result:=TDataObjectURL.Create(FScrapFileName,FURL,true)
     else Result:=nil;
end;

// Register Component ----------------------------------------------------------

procedure Register;
begin
  {MP}RegisterComponents({'Shell32'}'DragDrop', [TDragDropURL]);
end;

end.
