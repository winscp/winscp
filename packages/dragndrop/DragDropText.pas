unit DragDropText;
{
  Description
  ===========
    TDragDropText is a component for simple OLE drag-and-drop operations
    with text. The component is a child-class from TDragDrop. 


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
  TDataObjectText = class(TDataObject)
  private
     TextStream:TMemoryStream;
  public
     constructor Create(StringList: TStringList);
     destructor Destroy; override;
     function RenderData(FormatEtc:TFormatEtc;
        var StgMedium: TStgMedium):HResult; override;
  end;

  TDropTargetText = class(TDropTarget)
  protected
     procedure AcceptDataObject(DataObj: IDataObject; var Accept:boolean); override;
  public
     constructor Create(AOwner: TDragDrop);
     destructor Destroy; override;
     procedure RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
        pt: TPoint; var dwEffect: longint); override;
  end;

  TDragDropText = class(TDragDrop)
  private
     FLines:TStringList;
  protected
     function CreateDataObject:TDataObject; override;
  public
     constructor Create(AOwner: TComponent); override; 
     destructor Destroy; override;
     property Lines: TStringList read FLines write FLines;
  end;

procedure Register;

implementation

// some local functions --------------------------------------------------------

procedure CopyAsText(Strs:TStrings; DataPtr: PChar; DataSize:longint);
var lStrs:TStringList;
    s:string;
    i:longint;
begin
     lStrs:=TStringList.Create;
     s:='';
     i:=0;
     while i<DataSize do
     begin
          if (DataPtr[i]=#13) and (DataPtr[i+1]=#10) and (i+1<DataSize) then
          begin
               lstrs.add(s);
               s:='';
               inc(i);
          end else s:=s+DataPtr[i];
          inc(i);
     end;
     if s<>'' then lstrs.add(s);
     Strs.assign(lStrs);
     lStrs.free;
end;

// TDataObjectText -------------------------------------------------------------

constructor TDataObjectText.Create(StringList: TStringList);
var i: integer;
    FE:TFormatEtc;
    SM:TStgMedium;
    pc:array[0..255] of char;
begin
     inherited Create;
     with FE do
     begin
          cfFormat:=cf_Text;
          ptd:=nil;
          dwAspect:=DVAspect_Content;
          lindex:=-1;
          tymed:=tymed_HGlobal;
     end;
     SetData(FE,SM,false);
     TextStream:=TMemoryStream.Create;
     for i:=0 to StringList.count-1 do
     begin
          if i=StringList.count-1 then
          begin
               strPcopy(pc, StringList[i]+#0);
               TextStream.Write(pc,length(StringList[i])+1);
          end
          else
          begin
               strPcopy(pc, StringList[i]+#13#10);
               TextStream.Write(pc,length(StringList[i])+2);
          end;
     end;
end;

destructor TDataObjectText.Destroy;
begin
     TextStream.free;
     inherited Destroy;
end;

function TDataObjectText.RenderData(FormatEtc:TFormatEtc;
        var StgMedium: TStgMedium):HResult;
var h: HGlobal;
    p:pointer;
begin
     Result:=E_Fail;
     if FormatEtc.cfFormat=cf_Text then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, TextStream.Size);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          TextStream.Seek(0,0);
          TextStream.Read(p^,TextStream.Size);
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

// TDropTargetText -------------------------------------------------------------

constructor TDropTargetText.Create(AOwner: TDragDrop);
begin
     inherited Create(AOwner);
end;

destructor TDropTargetText.Destroy;
begin
     inherited Destroy;
end;

procedure TDropTargetText.AccepTDataObject(DataObj: IDataObject;
   var Accept:boolean);
var  FE:TFormatEtc;
begin
     with FE do
     begin
          cfFormat:=cf_Text;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     Accept:=DataObj.QueryGetData(FE)=S_OK;
end;

procedure TDropTargetText.RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
   pt: TPoint; var dwEffect: longint); 
var FE: TFormatEtc;
    SM: TStgMedium;
    DataSize: longint;
    DataPtr: pointer;
begin
     with FE do
     begin
          cfFormat:=CF_Text;
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
             CopyAsText(TDragDropText(FOwner).FLines, DataPtr,
                DataSize);
          finally
             GlobalUnLock(SM.HGlobal);
             ReleaseStgMedium(SM);
          end;
     end;
end;

// TDragDropText ---------------------------------------------------------------

constructor TDragDropText.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FLines:=TStringList.Create;
     FLines.sorted:=false;
     FLines.Duplicates:=dupAccept;
     FDropTarget._Release;
     FDropTarget:=TDropTargetText.Create(self);
end;

destructor TDragDropText.Destroy;
begin
     FLines.Free;
     inherited destroy;
end;

function TDragDropText.CreateDataObject:TDataObject;
begin
     if FLines.Count>0 then Result:=TDataObjectText.Create(FLines)
     else Result:=nil;
end;

// Register Component ----------------------------------------------------------

procedure Register;
begin
  {MP}RegisterComponents({'Shell32'}'DragDrop', [TDragDropText]);
end;

end.
