unit DragDropBitmap;
{
  Description
  ===========
    TDragDropBitmap is a component for simple OLE drag-and-drop operations
    with bitmaps. The component is a child-class from TDragDrop.


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

uses DragDrop, Windows, Classes, SysUtils, ActiveX, Graphics, Controls, Forms;

type
  TDataObjectBitmap = class(TDataObject)
  private
     DIBStream:TMemoryStream;
  public
     constructor Create(const Bitmap: TBitmap);
     destructor Destroy; override;
     function RenderData(FormatEtc:TFormatEtc;
        var StgMedium: TStgMedium):HResult; override;
  end;

  TDropTargetBitmap = class(TDropTarget)
  protected
     procedure AcceptDataObject(DataObj: IDataObject; var Accept:boolean); override;
  public
     constructor Create(AOwner: TDragDrop);
     destructor Destroy; override;
     procedure RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
        pt: TPoint; var dwEffect: longint); override;
  end;

  TDragDropBitmap = class(TDragDrop)
  private
     FBitmap:TBitmap;
     procedure SetBitmap(Bitmap:TBitmap);
  protected
     function CreateDataObject:TDataObject; override;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

procedure Register;

implementation

// some local functions --------------------------------------------------------

{procedure CopyAsBitmap(Bitmap:TBitmap; DataPtr: PChar; DataSize:longint);
var  MemoryStream: TMemoryStream;
     BMF: TBitmapFileheader;
begin
     ZeroMemory(@BMF, sizeof (TBitmapFileheader));
     BMF.bfType:=$4D42;
     MemoryStream:=TMemoryStream.Create;
     try
        MemoryStream.Write(BMF, sizeof (BMF));
        MemoryStream.Write(DataPtr^, DataSize);
        MemoryStream.Seek(0,0);
        Bitmap.LoadFromStream(MemoryStream);
     finally
        MemoryStream.Free;
     end;
end;}

procedure CopyAsBitmap(Bitmap:TBitmap; DataPtr: PChar; DataSize:longint);
var BitmapInfoHeader: TBitmapInfoHeader;
    BitmapInfo: PBitmapInfo;
    Size: Word;
    Pal: HPALETTE;
    BitsMem: Pointer;
    Focus: HWND;
    DC: HDC;
    OldPal: HPALETTE;
    ImagePtr:pchar;
    ImageSize:DWord;

    function GetDInColors(BitCount: Word): Integer;
    begin
         case BitCount of
              1, 4, 8: Result := 1 shl BitCount;
              else Result := 0;
         end;
    end;

    function PaletteFromW3DIB(const BI: TBitmapInfo): HPALETTE;
    var DstPal: PLogPalette;
        Colors, n: Integer;
        Size: Longint;
        DC: HDC;
        Focus: HWND;
        SysPalSize: Integer;
        I: Integer;
    begin
         Result := 0;
         { If the ClrUsed field of the header is non-zero, it means that we could
           have a short color table }
         with BI.bmiHeader do
              if biClrUsed <> 0 then Colors := biClrUsed
              else Colors := GetDInColors(biBitCount);
         if Colors <= 2 then Exit;
         Size := SizeOf(TLogPalette) + ((Colors - 1) * SizeOf(TPaletteEntry));
         DstPal := AllocMem(Size);
         try
            FillChar(DstPal^, Size, 0);
            with DstPal^ do
            begin
                 palNumEntries := Colors;
                 palVersion := $300;
                 Focus := GetFocus;
                 DC := GetDC(Focus);
                 try
                    SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
                    if (Colors = 16) and (SysPalSize >= 16) then
                    begin
                         { Ignore the disk image of the palette for 16 color bitmaps use
                           instead the first 8 and last 8 of the current system palette }
                         GetSystemPaletteEntries(DC, 0, 8, palPalEntry);
                         I := 8;
                         GetSystemPaletteEntries(DC, SysPalSize - I, I, palPalEntry[I]);
                    end
                    else
                        { Copy the palette for all others (i.e. 256 colors) }
                        for N := 0 to Colors - 1 do
                        begin
                             palPalEntry[N].peRed := BI.bmiColors[N].rgbRed;
                             palPalEntry[N].peGreen := BI.bmiColors[N].rgbGreen;
                             palPalEntry[N].peBlue := BI.bmiColors[N].rgbBlue;
                             palPalEntry[N].peFlags := 0;
                        end;
                 finally
                    ReleaseDC(Focus, DC);
                 end;
            end;
         Result := CreatePalette(DstPal^);
         finally
            FreeMem(DstPal, Size);
         end;
    end;

begin
     ImagePtr:=DataPtr;
     ImageSize:=DataSize;
     CopyMemory(@BitmapInfoHeader,ImagePtr,SizeOf(TBitmapInfoHeader));
     ImagePtr:=ImagePtr+SizeOf(TBitmapInfoHeader);
     with BitmapInfoHeader do
     begin
          if biClrUsed = 0 then
             biClrUsed := GetDInColors(biBitCount);
          Size := biClrUsed * SizeOf(TRgbQuad);
     end;
     BitmapInfo := AllocMem(Size + SizeOf(TBitmapInfoHeader));
     try
        with BitmapInfo^ do
        begin
             bmiHeader := BitmapInfoHeader;
             CopyMemory(@bmiColors, ImagePtr,Size);
             ImagePtr:=ImagePtr+Size;
             { now we've got the color table. Create a palette from it }
             Pal := PaletteFromW3DIB(BitmapInfo^);
             { some applications do not fill in the SizeImage field in the header.
               (Actually the truth is more likely that some drivers do not fill the field
               in and the apps do not compensate for these buggy drivers.) Therefore, if
               this field is 0, we will compute the size. }
             with bmiHeader do
             begin
                  Dec(ImageSize, SizeOf(TBitmapInfoHeader) + Size);
                  if biSizeImage <> 0 then
                     if biSizeImage < ImageSize then ImageSize := biSizeImage;
                  BitsMem := AllocMem(ImageSize);
                  try
                     CopyMemory(BitsMem, ImagePtr,ImageSize);
                     { we use the handle of the window with the focus (which, if this routine
                       is called from a menu command, will be this window) in order to guarantee
                       that the realized palette will have first priority on the system palette }
                     Focus := GetFocus;
                     DC := GetDC(Focus);
                     if DC <>0 then
                     try
                        if Pal <> 0 then
                        begin
                             { select and realize our palette we have gotten the DC of the focus
                             window just to make sure that all our colors are mapped }
                             OldPal := SelectPalette(DC, Pal, False);
                             RealizePalette(DC);
                        end
                        else OldPal := 0;
                        try
                           Bitmap.Handle:=CreateDIBitmap(DC, BitmapInfo^.bmiHeader,  CBM_INIT, BitsMem,
                           BitmapInfo^, DIB_RGB_COLORS);
                           finally
                           if OldPal <> 0 then
                              SelectPalette(DC, OldPal, False);
                        end;
                     finally
                        ReleaseDC(Focus, DC);
                     end;
                  finally
                     FreeMem(BitsMem, ImageSize);
                  end;
             end;
        end;
     finally
       FreeMem(BitmapInfo, Size + SizeOf(TBitmapInfoHeader));
     end;
end;

function WidthBytes(I: Longint): Longint;
begin
     Result := ((I + 31) div 32) * 4;
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var BM: Windows.TBitmap;
begin
     GetObject(Bitmap, SizeOf(BM), @BM);
     with BI do
     begin
          biSize := SizeOf(BI);
          biWidth := BM.bmWidth;
          biHeight := BM.bmHeight;
          if Colors <> 0 then
             case Colors of
                  2: biBitCount := 1;
                  16: biBitCount := 4;
                  256: biBitCount := 8;
             end
             else biBitCount := BM.bmBitsPixel * BM.bmPlanes;
          biPlanes := 1;
          biXPelsPerMeter := 0;
          biYPelsPerMeter := 0;
          biClrUsed := 0;
          biClrImportant := 0;
          biCompression := BI_RGB;
          if biBitCount in [16, 32] then biBitCount := 24;
          biSizeImage := WidthBytes(biWidth * biBitCount) * biHeight;
     end;
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: DWORD; Colors: Integer);
var BI: TBitmapInfoHeader;
begin
     InitializeBitmapInfoHeader(Bitmap, BI, Colors);
     with BI do
     begin
          if biBitCount=24 then InfoHeaderSize := SizeOf(TBitmapInfoHeader)
          else InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) *
             (1 shl biBitCount);
     end;
     ImageSize := BI.biSizeImage;
end;

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; Colors: Integer): Boolean;
var OldPal: HPALETTE;
    Focus: HWND;
    DC: HDC;
begin
     InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
     OldPal := 0;
     Focus := GetFocus;
     DC := GetDC(Focus);
     try
        if Palette <> 0 then
        begin
             OldPal := SelectPalette(DC, Palette, False);
             RealizePalette(DC);
        end;
        Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight,
           @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
     finally
        if OldPal <> 0 then SelectPalette(DC, OldPal, False);
        ReleaseDC(Focus, DC);
     end;
end;

procedure DIBFromBit(Stream: TMemoryStream; Src: HBITMAP;
  Pal: HPALETTE; Colors: Integer; var DIBHeader, DIBBits: Pointer);
var HeaderSize: Integer;
    ImageSize: DWORD;
begin
     if Src=0 then exit;
     InternalGetDIBSizes(Src, HeaderSize, ImageSize, Colors);
     Stream.SetSize(HeaderSize + integer(ImageSize));
     DIBHeader:=Stream.Memory;
     DIBBits:=Pointer(Longint(DIBHeader) + HeaderSize);
     InternalGetDIB(Src, Pal, DIBHeader^, DIBBits^, Colors);
end;

// TDataObjectBitmap -----------------------------------------------------------

constructor TDataObjectBitmap.Create(const Bitmap: TBitmap);
var FE:TFormatEtc;
    SM:TStgMedium;
    DIBHeader, DIBBits: Pointer;
begin
     inherited Create;
     with FE do
     begin
          cfFormat:=CF_DIB;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     SetData(FE,SM,false);
     DIBStream:=TMemoryStream.Create;
     DIBFromBit(DIBStream, Bitmap.Handle, Bitmap.Palette,0,DIBHeader, DIBBits);
     // Don't release DIBHeader and DIBBits; both points to a position in the stream
end;

destructor TDataObjectBitmap.Destroy;
begin
     DIBStream.free;
     inherited Destroy;
end;

function TDataObjectBitmap.RenderData(FormatEtc:TFormatEtc;
   var StgMedium: TStgMedium):HResult;
var h: HGlobal;
    p:pointer;
begin
     Result:=E_Fail;
     if FormatEtc.cfFormat=cf_DIB then
     begin
          h:=GlobalAlloc(GHND or GMEM_SHARE, DIBStream.Size);
          if h=0 then
          begin
               Result:=E_OUTOFMEMORY;
               exit;
          end;
          p:=globallock(h);
          DIBStream.Seek(0,0);
          DIBStream.Read(p^,DIBStream.Size);
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

// TDropTargetBitmap -----------------------------------------------------------

constructor TDropTargetBitmap.Create(AOwner: TDragDrop);
begin
     inherited Create(AOwner);
end;

destructor TDropTargetBitmap.Destroy;
begin
     inherited Destroy;
end;

procedure TDropTargetBitmap.AcceptDataObject(DataObj: IDataObject; var Accept:boolean);
var  FE:TFormatEtc;
begin
     with FE do
     begin
          cfFormat:=cf_DIB;
          ptd:=nil;
          dwAspect:=DVASPECT_CONTENT;
          lindex:=-1;
          tymed:=TYMED_HGLOBAL;
     end;
     Accept:=DataObj.QueryGetData(FE)=S_OK;
end;

procedure TDropTargetBitmap.RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
   pt: TPoint; var dwEffect: longint);
var FE: TFormatEtc;
    SM: TStgMedium;
    DataSize: longint;
    DataPtr: pointer;
begin
     with FE do
     begin
          cfFormat:=CF_DIB;
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
             CopyAsBitmap(TDragDropBitmap(FOwner).FBitmap,DataPtr, DataSize);
          finally
             GlobalUnLock(SM.HGlobal);
             ReleaseStgMedium(SM);
          end;
     end;
end;

// TDragDropBitmap -------------------------------------------------------------

constructor TDragDropBitmap.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FBitmap:=TBitmap.Create;
     FDropTarget._Release;
     FDropTarget:=TDropTargetBitmap.Create(self);
end;

destructor TDragDropBitmap.Destroy;
begin
     FBitmap.free;
     inherited destroy;
end;


procedure TDragDropBitmap.SetBitmap(Bitmap:TBitmap);
var MS:TMemoryStream;
begin
     // A little bit dirty but short ...
     MS:=TMemoryStream.Create;
     Bitmap.SaveToStream(MS);
     MS.Seek(0,0);
     FBitmap.LoadFromStream(MS);
     MS.Free;
end;

function TDragDropBitmap.CreateDataObject:TDataObject;
begin
     if FBitmap.Empty=false then Result:=TDataObjectBitmap.Create(FBitmap)
     else Result:=nil;
end;

// Register --------------------------------------------------------------------

procedure Register;
begin
  {MP}RegisterComponents({'Shell32'}'DragDrop', [TDragDropBitmap]);
end;

end.
