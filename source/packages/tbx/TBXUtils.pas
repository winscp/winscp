unit TBXUtils;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXUtils.pas 11 2004-04-01 07:22:56Z Alex@ZEISS

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, Forms, ImgList;

{$IFDEF TBX_UNICODE}
function GetTextHeightW(DC: HDC): Integer;
function GetTextWidthW(DC: HDC; const S: WideString; StripAccelChar: Boolean): Integer;
procedure DrawRotatedTextW(DC: HDC; AText: WideString; const ARect: TRect; const AFormat: Cardinal);
function EscapeAmpersandsW(const S: WideString): WideString;
function FindAccelCharW(const S: WideString): WideChar;
function StripAccelCharsW(const S: WideString): WideString;
function StripTrailingPunctuationW(const S: WideString): WideString;
{$ENDIF}

{ MP (the function in VCL is buggy even in C++Builder 6) }
{//$IFNDEF JR_D6}
{ MP }
function TBXCheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean; {vb+}
{//$ENDIF}
procedure GetRGB(C: TColor; out R, G, B: Integer);
function  MixColors(C1, C2: TColor; W1: Integer): TColor;
function  SameColors(C1, C2: TColor): Boolean;
function  Lighten(C: TColor; Amount: Integer): TColor;
function  NearestLighten(C: TColor; Amount: Integer): TColor;
function  NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
function  ColorIntensity(C: TColor): Integer;
function  IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
function  Blend(C1, C2: TColor; W1: Integer): TColor;
procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
procedure RGBtoHSL(RGB: TColor; out H, S, L : Single);
function  HSLtoRGB(H, S, L: Single): TColor;
function  GetBGR(C: TColorRef): Cardinal;

{ A few drawing functions }
{ these functions recognize clNone value of TColor }

procedure SetPixelEx(DC: HDC; X, Y: Integer; C: TColorRef; Alpha: Longword = $FF);
function  CreatePenEx(Color: TColor): HPen;
function  CreateBrushEx(Color: TColor): HBrush;
function  CreateDitheredBrush(C1, C2: TColor): HBrush;
function  FillRectEx(DC: HDC; const Rect: TRect; Color: TColor): Boolean; {$IFDEF COMPATIBLE_GFX}overload;{$ENDIF}
function  FrameRectEx(DC: HDC; var Rect: TRect; Color: TColor; Adjust: Boolean): Boolean; {$IFDEF COMPATIBLE_GFX}overload;{$ENDIF}
procedure DrawLineEx(DC: HDC; X1, Y1, X2, Y2: Integer; Color: TColor); {$IFDEF COMPATIBLE_GFX}overload;{$ENDIF}
function  PolyLineEx(DC: HDC; const Points: array of TPoint; Color: TColor): Boolean;
procedure PolygonEx(DC: HDC; const Points: array of TPoint; OutlineColor, FillColor: TColor);
procedure RoundRectEx(DC: HDC; Left, Top, Right, Bottom: Integer; EllipseWidth, EllipseHeight, OutlineColor, FillColor: TColor); overload; {vb+}
procedure RoundRectEx(DC: HDC; const R: TRect; EllipseWidth, EllipseHeight, OutlineColor, FillColor: TColor); overload; {vb+}
procedure DitherRect(DC: HDC; const R: TRect; C1, C2: TColor); {$IFDEF COMPATIBLE_GFX}overload;{$ENDIF}
procedure Frame3D(DC: HDC; var Rect: TRect; TopColor, BottomColor: TColor; Adjust: Boolean); {$IFDEF COMPATIBLE_GFX}overload;{$ENDIF}
procedure DrawDraggingOutline(DC: HDC; const NewRect, OldRect: TRect);

{ Gradients }
type
  TGradientKind = (gkHorz, gkVert);

procedure GradFill(DC: HDC; ARect: TRect; ClrTopLeft, ClrBottomRight: TColor; Kind: TGradientKind);
procedure BrushedFill(DC: HDC; Origin: PPoint; ARect: TRect; Color: TColor; Roughness: Integer);
procedure ResetBrushedFillCache;

{ drawing functions for compatibility with previous versions }
{$IFDEF COMPATIBLE_GFX}
function  FillRectEx(Canvas: TCanvas; const Rect: TRect; Color: TColor): Boolean; overload;
function  FrameRectEx(Canvas: TCanvas; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean; overload;
procedure DrawLineEx(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor); overload;
procedure DitherRect(Canvas: TCanvas; const R: TRect; C1, C2: TColor); overload;
procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor); overload;
function  FillRectEx2(DC: HDC; const Rect: TRect; Color: TColor): Boolean; deprecated;
function  FrameRectEx2(DC: HDC; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean; deprecated;
{$ENDIF}

{ alternatives to fillchar and move routines what work with 32-bit aligned memory blocks }
procedure FillLongword(var X; Count: Integer; Value: Longword);
procedure MoveLongword(const Source; var Dest; Count: Integer);

{ extended icon painting routines }
procedure DrawTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HiContrast: Boolean);
procedure BlendTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Opacity: Byte);
procedure HighlightTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HighlightColor: TColor; Amount: Byte);
procedure DrawTBXIconShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Density: Integer);
procedure DrawTBXIconFlatShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
procedure DrawTBXIconFullShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);

procedure DrawGlyph(DC: HDC; X, Y: Integer; ImageList: TCustomImageList; ImageIndex: Integer; Color: TColor); overload;
procedure DrawGlyph(DC: HDC; const R: TRect; ImageList: TCustomImageList; ImageIndex: Integer; Color: TColor); overload;
procedure DrawGlyph(DC: HDC; X, Y: Integer; const Bits; Color: TColor); overload;
procedure DrawGlyph(DC: HDC; const R: TRect; Width, Height: Integer; const Bits; Color: TColor); overload;

function GetClientSizeEx(Control: TWinControl): TPoint;

const
  SHD_DENSE = 0;
  SHD_LIGHT = 1;

{ An additional declaration for D4 compiler }
type
  PColor = ^TColor;

{ Stock Objects }
var
  StockBitmap1, StockBitmap2: TBitmap;
  StockMonoBitmap, StockCompatibleBitmap: TBitmap;
  SmCaptionFont: TFont;

const
  ROP_DSPDxax = $00E20746;

{ Support for window shadows }
type
  TShadowEdges = set of (seTopLeft, seBottomRight);
  TShadowStyle = (ssFlat, ssLayered, ssAlphaBlend);

  TShadow = class(TCustomControl)
  protected
    FOpacity: Byte;
    FBuffer: TBitmap;
    FClearRect: TRect;
    FEdges: TShadowEdges;
    FStyle: TShadowStyle;
    FSaveBits: Boolean;
    procedure GradR(const R: TRect);
    procedure GradB(const R: TRect);
    procedure GradBR(const R: TRect);
    procedure GradTR(const R: TRect);
    procedure GradBL(const R: TRect);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FillBuffer; virtual; abstract;
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(const Bounds: TRect; Opacity: Byte; LoColor: Boolean; Edges: TShadowEdges); reintroduce;
    procedure Clear(const R: TRect);
    procedure Render;
    procedure Show(ParentHandle: HWND);
  end;

  THorzShadow = class(TShadow)
  protected
    procedure FillBuffer; override;
  end;

  TVertShadow = class(TShadow)
  protected
    procedure FillBuffer; override;
  end;

  TShadows = class
  private
    FSaveBits: Boolean;
    procedure SetSaveBits(Value: Boolean);
  protected
    V1: TShadow;
    H1: TShadow;
    V2: TShadow;
    H2: TShadow;
    V3: TShadow;
    H3: TShadow;
  public
    constructor Create(R1, R2: TRect; TheSize: Integer; Opacity: Byte; LoColor: Boolean);
    destructor Destroy; override;
    procedure Show(ParentHandle: HWND);
    property SaveBits: Boolean read FSaveBits write SetSaveBits;
  end;

procedure RecreateStock;

type
  PBlendFunction = ^TBlendFunction;
  TBlendFunction = packed record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

  TUpdateLayeredWindow = function(hWnd : hWnd; hdcDst : hDC; pptDst : PPoint;
    psize : PSize; hdcSrc : hDC; pptSrc : PPoint; crKey : TColorRef;
    pblend : PBlendFunction; dwFlags : Integer): Integer; stdcall;

  TAlphaBlend = function(hdcDest: HDC; nXOriginDest, nYOriginDest,
    nWidthDest, nHeightDest: Integer; hdcSrc: HDC;
    nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
    blendFunction: TBlendFunction): BOOL; stdcall;

  TGradientFill = function(Handle: HDC; pVertex: Pointer; dwNumVertex: DWORD;
    pMesh: Pointer; dwNumMesh: DWORD; dwMode: DWORD): DWORD; stdcall;

var
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
  AlphaBlend: TAlphaBlend = nil;
  GradientFill: TGradientFill = nil;


implementation

{$R-}{$Q-}

uses TB2Common, Math;

{$IFDEF TBX_UNICODE}

function GetTextHeightW(DC: HDC): Integer;
var
  TextMetric: TTextMetricW;
begin
  GetTextMetricsW(DC, TextMetric);
  Result := TextMetric.tmHeight;
end;

function GetTextWidthW(DC: HDC; const S: WideString; StripAccelChar: Boolean): Integer;
var
  Size: TSize;
  S2: WideString;
begin
  if StripAccelChar then
  begin
    S2 := StripAccelCharsW(S);
    GetTextExtentPoint32W(DC, PWideChar(S2), Length(S2), Size);
  end
  else GetTextExtentPoint32W(DC, PWideChar(S), Length(S), Size);
  Result := Size.cx;
end;

procedure DrawRotatedTextW(DC: HDC; AText: WideString; const ARect: TRect; const AFormat: Cardinal);
{ Like DrawText, but draws the text at a 270 degree angle.
  The format flag this function respects are
  DT_NOPREFIX, DT_HIDEPREFIX, DT_CENTER, DT_END_ELLIPSIS, DT_NOCLIP }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetricW;
  X, Y, P, I, SU, FU, W: Integer;
  SaveAlign: UINT;
  Clip: Boolean;

  function GetSize(DC: HDC; const S: WideString): Integer;
  var
    Size: TSize;
  begin
    GetTextExtentPoint32W(DC, PWideChar(S), Length(S), Size);
    Result := Size.cx;
  end;

begin
  if Length(AText) = 0 then Exit;

  RotatedFont := CreateRotatedFont(DC);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetricsW(DC, TextMetrics);
  X := ARect.Left + ((ARect.Right - ARect.Left) - TextMetrics.tmHeight) div 2;

  Clip := AFormat and DT_NOCLIP = 0;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  if AFormat and DT_NOPREFIX = 0 then
    while I <= Length(AText) do
    begin
      if AText[I] = '&' then
      begin
        Delete(AText, I, 1);
        if PWideChar(AText)[I - 1] <> '&' then P := I;
      end;
      Inc(I);
    end;

  if AFormat and DT_END_ELLIPSIS <> 0 then
  begin
    if (Length(AText) > 1) and (GetSize(DC, AText) > ARect.Bottom - ARect.Top) then
    begin
      W := ARect.Bottom - ARect.Top;
      if W > 2 then
      begin
        Delete(AText, Length(AText), 1);
        while (Length(AText) > 1) and (GetSize(DC, AText + '...') > W) do
          Delete(AText, Length(AText), 1);
      end
      else AText := AText[1];
      if P > Length(AText) then P := 0;
      AText := AText + '...';
    end;
  end;

  if AFormat and DT_CENTER <> 0 then
    Y := ARect.Top + ((ARect.Bottom - ARect.Top) - GetSize(DC, AText)) div 2
  else
    Y := ARect.Top;

  if Clip then
  begin
    SaveDC(DC);
    with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);
  end;

  SaveAlign := SetTextAlign(DC, TA_BOTTOM);
  TextOutW(DC, X, Y, PWideChar(AText), Length(AText));
  SetTextAlign(DC, SaveAlign);

  { Underline }
  if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then
  begin
    SU := GetTextWidthW(DC, Copy(AText, 1, P - 1), False);
    FU := SU + GetTextWidthW(DC, PWideChar(AText)[P - 1], False);
    Inc(X, TextMetrics.tmDescent - 2);
    DrawLineEx(DC, X, Y + SU, X, Y + FU, GetTextColor(DC));
  end;

  if Clip then RestoreDC(DC, -1);

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function EscapeAmpersandsW(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = '&' then
    begin
      Inc(I);
      Insert('&', Result, I);
    end;
    Inc(I);
  end;
end;

function FindAccelCharW(const S: WideString): WideChar;
var
  PStart, P: PWideChar;
begin
  { locate the last char with '&' prefix }
  Result := #0;
  if Length(S) > 0 then
  begin
    PStart := PWideChar(S);
    P := PStart;
    Inc(P, Length(S) - 2);
    while P >= PStart do
    begin
      if P^ = '&' then
      begin
        if (P = PStart) or (PWideChar(Integer(P) - 2)^ <> '&') then
        begin
          Result := PWideChar(Integer(P) + 2)^;
          Exit;
        end
        else Dec(P);
      end;
      Dec(P);
    end;
  end;
end;

function StripAccelCharsW(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = '&' then
      System.Delete(Result, I, 1);
    Inc(I);
  end;
end;

function StripTrailingPunctuationW(const S: WideString): WideString;
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') then SetLength(Result, L - 1)
  else if (L > 3) and (Result[L - 2] = '.') and (Result[L - 1] = '.') and
     (Result[L] = '.') then SetLength(Result, L - 3);
end;

{$ENDIF}

{ MP (the function in VCL is buggy even in C++Builder 6) }
{//$IFNDEF JR_D6}
{ MP }
function TBXCheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean; {vb+}
begin
  Result := (Win32MajorVersion > AMajor) or
    ((Win32MajorVersion = AMajor) and (Win32MinorVersion >= AMinor));
end;
{//$ENDIF}

type
  PPoints = ^TPoints;
  TPoints = array [0..0] of TPoint;

const
  WeightR: single = 0.764706;
  WeightG: single = 1.52941;
  WeightB: single = 0.254902;

procedure GetRGB(C: TColor; out R, G, B: Integer);
begin
  if Integer(C) < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF;
  G := C shr 8 and $FF;
  B := C shr 16 and $FF;
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

function SameColors(C1, C2: TColor): Boolean;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $000000FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := C1 = C2;
end;

function Lighten(C: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  if C < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function NearestLighten(C: TColor; Amount: Integer): TColor;
begin
  Result := GetNearestColor(StockCompatibleBitmap.Canvas.Handle, Lighten(C, Amount));
end;

function NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
begin
  Result := MixColors(C1, C2, W1);
  Result := GetNearestColor(StockCompatibleBitmap.Canvas.Handle, Result);
end;

function ColorIntensity(C: TColor): Integer;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Result := ((C shr 16 and $FF) * 30 + (C shr 8 and $FF) * 150 + (C and $FF) * 76) shr 8;
end;

function IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Threshold := Threshold shl 8;
  Result := ((C and $FF) * 76 + (C shr 8 and $FF) * 150 + (C shr 16 and $FF) * 30 ) < Threshold;
end;

function Blend(C1, C2: TColor; W1: Integer): TColor;
var
  W2, A1, A2, D, F, G: Integer;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $FF);

  if W1 >= 100 then D := 1000
  else D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * W2;
  A1 := C1 shr 16 * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * W2;
  A1 := (C1 shr 8 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * W2;
  A1 := (C1 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function ColorDistance(C1, C2: Integer): Single;
var
  DR, DG, DB: Integer;
begin
  DR := (C1 and $FF) - (C2 and $FF);
  Result := Sqr(DR * WeightR);
  DG := (C1 shr 8 and $FF) - (C2 shr 8 and $FF);
  Result := Result + Sqr(DG * WeightG);
  DB := (C1 shr 16) - (C2 shr 16);
  Result := Result + Sqr(DB * WeightB);
  Result := SqRt(Result);
end;

function GetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
  if BkgndIntensity < 220 then Result := (2 - BkgndIntensity / 220) * Threshold
  else Result := Threshold;
end;

function IsContrastEnough(AColor, ABkgndColor: Integer;
  DoAdjustThreshold: Boolean; Threshold: Single): Boolean;
begin
  if DoAdjustThreshold then
    Threshold := GetAdjustedThreshold(ColorDistance(ABkgndColor, $000000), Threshold);
  Result := ColorDistance(ABkgndColor, AColor) > Threshold;
end;

procedure AdjustContrast(var AColor: Integer; ABkgndColor: Integer; Threshold: Single);
var
  x, y, z: Single;
  r, g, b: Single;
  RR, GG, BB: Integer;
  i1, i2, s, q, w: Single;
  DoInvert: Boolean;
begin
  i1 := ColorDistance(AColor, $000000);
  i2 := ColorDistance(ABkgndColor, $000000);
  Threshold := GetAdjustedThreshold(i2, Threshold);

  if i1 > i2 then DoInvert := i2 < 442 - Threshold
  else DoInvert := i2 < Threshold;

  x := (ABkgndColor and $FF) * WeightR;
  y := (ABkgndColor shr 8 and $FF) * WeightG;
  z := (ABkgndColor shr 16) * WeightB;

  r := (AColor and $FF) * WeightR;
  g := (AColor shr 8 and $FF) * WeightG;
  b := (AColor shr  16) * WeightB;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b := 65 - b;
    x := 195 - x;
    y := 390 - y;
    z := 65 - z;
  end;

  s := Sqrt(Sqr(b) + Sqr(g) + Sqr(r));
  if s < 0.01 then s := 0.01;

  q := (r * x + g * y + b * z) / S;

  x := Q / S * r - x;
  y := Q / S * g - y;
  z := Q / S * b - z;

  w :=  Sqrt(Sqr(Threshold) - Sqr(x) - Sqr(y) - Sqr(z));

  r := (q - w) * r / s;
  g := (q - w) * g / s;
  b := (q - w) * b / s;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b :=  65 - b;
  end;

  if r < 0 then r := 0 else if r > 195 then r := 195;
  if g < 0 then g := 0 else if g > 390 then g := 390;
  if b < 0 then b := 0 else if b >  65 then b :=  65;

  RR := Trunc(r * (1 / WeightR) + 0.5);
  GG := Trunc(g * (1 / WeightG) + 0.5);
  BB := Trunc(b * (1 / WeightB) + 0.5);

  if RR > $FF then RR := $FF else if RR < 0 then RR := 0;
  if GG > $FF then GG := $FF else if GG < 0 then GG := 0;
  if BB > $FF then BB := $FF else if BB < 0 then BB := 0;

  AColor := (BB and $FF) shl 16 or (GG and $FF) shl 8 or (RR and $FF);
end;

procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
var
  t: Single;
begin
  if Color < 0 then Color := GetSysColor(Color and $FF);
  if BkgndColor < 0 then BkgndColor := GetSysColor(BkgndColor and $FF);
  t := Threshold;
  if not IsContrastEnough(Color, BkgndColor, True, t) then
    AdjustContrast(Integer(Color), BkgndColor, t);
end;

procedure RGBtoHSL(RGB: TColor; out H, S, L : Single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  if RGB < 0 then RGB := GetSysColor(RGB and $FF);
  R := GetRValue(RGB) / 255;
  G := GetGValue(RGB) / 255;
  B := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D
    else
      if G = Cmax then H  := 2 + (B - R) / D
      else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1
  end;
end;

function HSLtoRGB(H, S, L: Single): TColor;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;

begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result := RGB(R, G, B);
end;

{ Drawing routines }

function GetBGR(C: TColorRef): Cardinal;
asm
        MOV     ECX,EAX         // this function swaps R and B bytes in ABGR
        SHR     EAX,16
        XCHG    AL,CL
        MOV     AH,$00          // and writes $FF into A component
        SHL     EAX,16
        MOV     AX,CX
end;

procedure SetPixelEx(DC: HDC; X, Y: Integer; C: TColorRef; Alpha: Longword = $FF);
var
  W2: Cardinal;
  B: TColorRef;
begin
  if Alpha <= 0 then Exit
  else if Alpha >= 255 then SetPixelV(DC, X, Y, C)
  else
  begin
    B := GetPixel(DC, X, Y);
    if B <> CLR_INVALID then
    begin
      Inc(Alpha, Integer(Alpha > 127));
      W2 := 256 - Alpha;
      B :=
        ((C and $FF00FF) * Alpha + (B and $FF00FF) * W2 + $007F007F) and $FF00FF00 +
        ((C and $00FF00) * Alpha + (B and $00FF00) * W2 + $00007F00) and $00FF0000;
      SetPixelV(DC, X, Y, B shr 8);
    end;
  end;
end;

function CreatePenEx(Color: TColor): HPen;
begin
  if Color = clNone then Result := CreatePen(PS_NULL, 1, 0)
  else if Color < 0 then Result := CreatePen(PS_SOLID, 1, GetSysColor(Color and $000000FF))
  else Result := CreatePen(PS_SOLID, 1, Color);
end;

function CreateBrushEx(Color: TColor): HBrush;
var
  LB: TLogBrush;
begin
  if Color = clNone then
  begin
    LB.lbStyle := BS_HOLLOW;
    Result := CreateBrushIndirect(LB);
  end
  {else if Color < 0 then Result := GetSysColorBrush(Color and $000000FF)} {vb-}
  else begin {vb+}
    if Color < 0 then Color := GetSysColor(Color and $000000FF);
    Result := CreateSolidBrush(Color);
  end;
end;

function FillRectEx(DC: HDC; const Rect: TRect; Color: TColor): Boolean;
var
  Brush: HBRUSH;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then Brush := GetSysColorBrush(Color and $000000FF)
    else Brush := CreateSolidBrush(Color);
    Windows.FillRect(DC, Rect, Brush);
    {DeleteObject(Brush);} {vb-}
    if Color >= 0 then DeleteObject(Brush); {vb+}
  end;
end;

function FrameRectEx(DC: HDC; var Rect: TRect; Color: TColor; Adjust: Boolean): Boolean;
var
  Brush: HBRUSH;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then Brush := GetSysColorBrush(Color and $000000FF)
    else Brush := CreateSolidBrush(Color);
    Windows.FrameRect(DC, Rect, Brush);
    {DeleteObject(Brush);} {vb-}
    if Color >= 0 then DeleteObject(Brush); {vb+}
  end;
  if Adjust then with Rect do
  begin
    Inc(Left); Dec(Right);
    Inc(Top); Dec(Bottom);
  end;
end;

procedure DrawLineEx(DC: HDC; X1, Y1, X2, Y2: Integer; Color: TColor);
var
  OldPen, Pen: HPen;
begin
  Pen := CreatePen(PS_SOLID, 1, ColorToRGB(Color));
  OldPen := SelectObject(DC, Pen);
  Windows.MoveToEx(DC, X1, Y1, nil);
  Windows.LineTo(DC, X2, Y2);
  SelectObject(DC, OldPen);
  DeleteObject(Pen);
end;

function PolyLineEx(DC: HDC; const Points: array of TPoint; Color: TColor): Boolean; overload;
var
  Pen, OldPen: HPEN;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then Color := GetSysColor(Color and $FF);
    Pen := CreatePen(PS_SOLID, 1, Color);
    OldPen := SelectObject(DC, Pen);
    Windows.Polyline(DC, PPoints(@Points[0])^, Length(Points));
    SelectObject(DC, OldPen);
    DeleteObject(Pen);
  end;
end;

procedure PolygonEx(DC: HDC; const Points: array of TPoint; OutlineColor, FillColor: TColor);
var
  OldBrush, Brush: HBrush;
  OldPen, Pen: HPen;
begin
  if (OutlineColor = clNone) and (FillColor = clNone) then Exit;
  Pen := CreatePenEx(OutlineColor);
  Brush := CreateBrushEx(FillColor);
  OldPen := SelectObject(DC, Pen);
  OldBrush := SelectObject(DC, Brush);
  Windows.Polygon(DC, PPoints(@Points[0])^, Length(Points));
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
  DeleteObject(Brush);
  DeleteObject(Pen);
end;

procedure RoundRectEx(DC: HDC; Left, Top, Right, Bottom: Integer;
  EllipseWidth, EllipseHeight, OutlineColor, FillColor: TColor); {vb+}
var
  OldBrush, Brush: HBrush;
  OldPen, Pen: HPen;
begin
  if (OutlineColor = clNone) and (FillColor = clNone) then Exit;
  Pen := CreatePenEx(OutlineColor);
  Brush := CreateBrushEx(FillColor);
  OldPen := SelectObject(DC, Pen);
  OldBrush := SelectObject(DC, Brush);
  Windows.RoundRect(DC, Left, Top, Right, Bottom, EllipseWidth, EllipseHeight);
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
  DeleteObject(Brush);
  DeleteObject(Pen);
end;

procedure RoundRectEx(DC: HDC; const R: TRect; EllipseWidth, EllipseHeight,
  OutlineColor, FillColor: TColor); {vb+}
begin
  with R do
    RoundRectEx(DC, Left, Top, Right, Bottom, EllipseWidth,
      EllipseHeight, OutlineColor, FillColor);
end;

function CreateDitheredBrush(C1, C2: TColor): HBrush;
var
  B: TBitmap;
begin
  B := AllocPatternBitmap(C1, C2);
  B.HandleType := bmDDB;
  Result := CreatePatternBrush(B.Handle);
end;

procedure DitherRect(DC: HDC; const R: TRect; C1, C2: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateDitheredBrush(C1, C2);
  FillRect(DC, R, Brush);
  DeleteObject(Brush);
end;

procedure Frame3D(DC: HDC; var Rect: TRect; TopColor, BottomColor: TColor; Adjust: Boolean);
var
  TopRight, BottomLeft: TPoint;
begin
  with Rect do
  begin
    Dec(Bottom); Dec(Right);
    TopRight.X := Right;
    TopRight.Y := Top;
    BottomLeft.X := Left;
    BottomLeft.Y := Bottom;
    PolyLineEx(DC, [BottomLeft, TopLeft, TopRight], TopColor);
    Dec(BottomLeft.X);
    PolyLineEx(DC, [TopRight, BottomRight, BottomLeft], BottomColor);
    if Adjust then
    begin
      Inc(Left);
      Inc(Top);
    end
    else
    begin
      Dec(Right);
      Dec(Bottom);
    end;
  end;
end;


{$IFDEF COMPATIBLE_GFX}
procedure DitherRect(Canvas: TCanvas; const R: TRect; C1, C2: TColor);
begin
  DitherRect(Canvas.Handle, R, C1, C2);
end;

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor);
var
  TopRight, BottomLeft: TPoint;
begin
  with Canvas, Rect do
  begin
    Pen.Width := 1;
    Dec(Bottom); Dec(Right);
    TopRight.X := Right;
    TopRight.Y := Top;
    BottomLeft.X := Left;
    BottomLeft.Y := Bottom;
    Pen.Color := TopColor;
    PolyLine([BottomLeft, TopLeft, TopRight]);
    Pen.Color := BottomColor;
    Dec(BottomLeft.X);
    PolyLine([TopRight, BottomRight, BottomLeft]);
    Inc(Left); Inc(Top);
  end;
end;

function FillRectEx(Canvas: TCanvas; const Rect: TRect; Color: TColor): Boolean;
begin
  Result := FillRectEx(Canvas.Handle, Rect, Color);
end;

function  FillRectEx2(DC: HDC; const Rect: TRect; Color: TColor): Boolean; deprecated;
begin
  Result := FillRectEx(DC, Rect, Color);
end;

function FrameRectEx(Canvas: TCanvas; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean;
begin
  Result := FrameRectEx(Canvas.Handle, Rect, Color, Adjust);
end;

function FrameRectEx2(DC: HDC; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean; deprecated;
begin
  Result := FrameRectEx(DC, Rect, Color, Adjust);
end;

procedure DrawLineEx(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
begin
  DrawLineEx(Canvas.Handle, X1, Y1, X2, Y2, Color);
end;
{$ENDIF}

procedure DrawDraggingOutline(DC: HDC; const NewRect, OldRect: TRect);
var
  Sz: TSize;
begin
  Sz.CX := 3; Sz.CY := 2;
  DrawHalftoneInvertRect(DC, @NewRect, @OldRect, Sz, Sz);
end;

procedure FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X;  EDX = Count; ECX = Value
        PUSH    EDI
        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit
        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;

procedure MoveLongword(const Source; var Dest; Count: Integer);
asm
// EAX = Source; EDX = Dest; ECX = Count
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX         // Source
        MOV     EDI,EDX         // Destination
        MOV     EAX,ECX         // Counter
        CMP     EDI,ESI
        JE      @exit
        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
end;

procedure DrawTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HiContrast: Boolean);
{const
  CWeirdColor = $00203241;} {vb -}
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: PColor;
  S, C: TColor;
begin
  if not HiContrast then
  begin
    ImageList.Draw(Canvas, R.Left, R.Top, ImageIndex);
    Exit;
  end;
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  {for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);} {vb -}
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY); {vb +}
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      {S := Src^ and $00FFFFFF;} {vb -}
      S := Src^; {vb +}
      {if S <> CWeirdColor then} {vb -}
      if S <> Dst^ then {vb +}
      begin
        {C := (S and $FF0000) shr 16 * 76 + (S and $00FF00) shr 8 * 150 +
          (S and $0000FF) * 29;} {vb -}
        C := (S and $00FF0000) shr 16 * 76 + (S and $0000FF00) shr 8 * 150 +
          (S and $000000FF) * 29; {vb +}
        if C > $FD00 then S := $000000
        else if C < $6400 then S := $FFFFFF;
        Dst^ := Lighten(S, 32);
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure BlendTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Opacity: Byte);
{const
  CWeirdColor = $00203241;} {vb -}
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
  Wt1, Wt2: Cardinal;
begin
  Wt2 := Opacity;
  Wt1 := 255 - Wt2;
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  {BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);} {vb -}
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY); {vb +}
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^;
      if S <> Dst^ then
      begin
        CBRB := (Dst^ and $00FF00FF) * Wt1;
        CBG  := (Dst^ and $0000FF00) * Wt1;
        {C := ((S and $FF00FF) * Wt2 + CBRB) and $FF00FF00 +
          ((S and $00FF00) * Wt2 + CBG) and $00FF0000;} {vb -}
        C := ((S and $00FF00FF) * Wt2 + CBRB) and $FF00FF00 +
          ((S and $0000FF00) * Wt2 + CBG) and $00FF0000; {vb +}
        Dst^ := C shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure HighlightTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HighlightColor: TColor; Amount: Byte);
{const
  CWeirdColor = $00203241;} {vb -}
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  {Src, Dst: PColor;} {vb -}
  Src, Dst: ^Cardinal; {vb +}
  S, C: Cardinal;
  CBRB, CBG: Cardinal;
  W1, W2: Cardinal;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  {for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);} {vb -}
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY); {vb +}
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex);

  W2 := Amount;
  W1 := 255 - W2;
  HighlightColor := GetBGR(ColorToRGB(HighlightColor));
  CBRB := (Cardinal(HighlightColor) and $00FF00FF) * W1;
  CBG  := (Cardinal(HighlightColor) and $0000FF00) * W1;

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      {S := Src^ and $00FFFFFF;} {vb -}
      S := Src^; {vb +}
      {if S <> CWeirdColor then} {vb -}
      if S <> Dst^ then {vb +}
      begin
        {C := ((S and $FF00FF) * W2 + CBRB) and $FF00FF00 +
          ((S and $00FF00) * W2 + CBG) and $00FF0000;} {vb -}
        C := ((S and $00FF00FF) * W2 + CBRB) and $FF00FF00 +
          ((S and $0000FF00) * W2 + CBG) and $00FF0000; {vb +}
        Dst^ := C shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure DrawTBXIconShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Density: Integer);
const
  D_DIV: array [0..2] of Cardinal = (3, 8, 20);
  D_ADD: array [0..2] of Cardinal = (255 - 255 div 3, 255 - 255 div 8, 255 - 255 div 20);
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
begin
  Assert(Density in [0..2]);

  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  {BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);} {vb -}
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY); {vb +}
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^;
      if S <> Dst^ then
      begin
        CBRB := Dst^ and $00FF00FF;
        CBG  := Dst^ and $0000FF00;
        {C := ((S and $FF0000) shr 16 * 29 + (S and $00FF00) shr 8 * 150 +
          (S and $0000FF) * 76) shr 8;} {vb -}
        C := ((S and $00FF0000) shr 16 * 29 + (S and $0000FF00) shr 8 * 150 +
          (S and $000000FF) * 76) shr 8; {vb +}
        C := C div D_DIV[Density] + D_ADD[Density];
        Dst^ := ((CBRB * C and $FF00FF00) or (CBG * C and $00FF0000)) shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure DrawTBXIconFlatShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
const
  CShadowThreshold = 180 * 256;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  P: ^Cardinal;
  C: Cardinal;
  SrcDC, DstDC: HDC;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;
  StockBitmap2.Canvas.Brush.Color := clWhite;
  StockBitmap2.Canvas.FillRect(Rect(0, 0, ImageWidth, ImageHeight));
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    P := StockBitmap2.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      C := P^ and $00FFFFFF;
      if C <> $0 then
      begin
        C := (C and $FF0000) shr 16 * 76 + (C and $00FF00) shr 8 * 150 + (C and $0000FF) * 29;
        if C > CShadowThreshold then P^ := $00FFFFFF
        else P^ := $00000000;
      end;
      Inc(P);
    end;
  end;

  StockMonoBitmap.Width := ImageWidth;
  StockMonoBitmap.Height := ImageHeight;
  StockMonoBitmap.Canvas.Brush.Color := clBlack;
  BitBlt(StockMonoBitmap.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap2.Canvas.Handle, 0, 0, SRCCOPY);

  SrcDC := StockMonoBitmap.Canvas.Handle;
  Canvas.Brush.Color := ColorToRGB(ShadowColor);
  DstDC := Canvas.Handle;
  Windows.SetTextColor(DstDC, clWhite);
  Windows.SetBkColor(DstDC, clBlack);
  BitBlt(DstDC, R.Left, R.Top, ImageWidth, ImageHeight, SrcDC, 0, 0, ROP_DSPDxax);
end;

procedure DrawTBXIconFullShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
const
  CWeirdColor = $00203241;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  P: ^Cardinal;
  C: Cardinal;
  SrcDC, DstDC: HDC;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;
  for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    P := StockBitmap2.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      C := P^ and $00FFFFFF;
      if C <> CWeirdColor then P^ := $00000000
      else P^ := $00FFFFFF;
      Inc(P);
    end;
  end;

  StockMonoBitmap.Width := ImageWidth;
  StockMonoBitmap.Height := ImageHeight;
  StockMonoBitmap.Canvas.Brush.Color := clBlack;
  BitBlt(StockMonoBitmap.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap2.Canvas.Handle, 0, 0, SRCCOPY);

  SrcDC := StockMonoBitmap.Canvas.Handle;
  Canvas.Brush.Color := ColorToRGB(ShadowColor);
  DstDC := Canvas.Handle;
  Windows.SetTextColor(DstDC, clWhite);
  Windows.SetBkColor(DstDC, clBlack);
  BitBlt(DstDC, R.Left, R.Top, ImageWidth, ImageHeight, SrcDC, 0, 0, ROP_DSPDxax);
end;

procedure DrawGlyph(DC: HDC; X, Y: Integer; ImageList: TCustomImageList; ImageIndex: Integer; Color: TColor);
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
begin
  if Color = clNone then Exit;
  B := TBitmap.Create;
  B.Monochrome := True;
  ImageList.GetBitmap(ImageIndex, B);
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  if Color < 0 then Brush := GetSysColorBrush(Color and $FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);
  BitBlt(DC, X, Y, ImageList.Width, ImageList.Height, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
  SelectObject(DC, OldBrush);
  if Color >= 0 then DeleteObject(Brush);
  SetTextColor(DC, OldTextColor);
  SetBkColor(DC, OldBkColor);
  B.Free;
end;

procedure DrawGlyph(DC: HDC; const R: TRect; ImageList: TCustomImageList; ImageIndex: Integer; Color: TColor); overload;
begin
  DrawGlyph(DC, (R.Left + R.Right + 1 - ImageList.Width) div 2, (R.Top + R.Bottom + 1 - ImageList.Height) div 2, ImageList, ImageIndex, Color);
end;

procedure DrawGlyph(DC: HDC; X, Y: Integer; const Bits; Color: TColor); overload;
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
begin
  B := TBitmap.Create;
  B.Handle := CreateBitmap(8, 8, 1, 1, @Bits);
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  if Color < 0 then Brush := GetSysColorBrush(Color and $FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);
  BitBlt(DC, X, Y, 8, 8, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
  SelectObject(DC, OldBrush);
  if Color >= 0 then DeleteObject(Brush);
  SetTextColor(DC, OldTextColor);
  SetBkColor(DC, OldBkColor);
  B.Free;
end;

procedure DrawGlyph(DC: HDC; const R: TRect; Width, Height: Integer; const Bits; Color: TColor); overload;
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
begin
  B := TBitmap.Create;
  B.Handle := CreateBitmap(8, 8, 1, 1, @Bits);
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  if Color < 0 then Brush := GetSysColorBrush(Color and $FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);
  BitBlt(DC, (R.Left + R.Right + 1 - Width) div 2, (R.Top + R.Bottom  + 1 - Height) div 2, Width, Height, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
  SelectObject(DC, OldBrush);
  if Color >= 0 then DeleteObject(Brush);
  SetTextColor(DC, OldTextColor);
  SetBkColor(DC, OldBkColor);
  B.Free;
end;

type
  TCustomFormAccess = class(TCustomForm);

function GetClientSizeEx(Control: TWinControl): TPoint;
var
  R: TRect;
begin
  if (Control is TCustomForm) and (TCustomFormAccess(Control).FormStyle = fsMDIForm)
    and not (csDesigning in Control.ComponentState) then
    GetWindowRect(TCustomFormAccess(Control).ClientHandle, R)
  else
    R := Control.ClientRect;
  Result.X := R.Right - R.Left;
  Result.Y := R.Bottom - R.Top;
end;

procedure InitializeStock;
var
  NonClientMetrics: TNonClientMetrics;
begin
  StockBitmap1 := TBitmap.Create;
  StockBitmap1.PixelFormat := pf32bit;
  StockBitmap2 := TBitmap.Create;
  StockBitmap2.PixelFormat := pf32bit;
  StockMonoBitmap := TBitmap.Create;
  StockMonoBitmap.Monochrome := True;
  StockCompatibleBitmap := TBitmap.Create;
  StockCompatibleBitmap.Width := 8;
  StockCompatibleBitmap.Height := 8;
  SmCaptionFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    SmCaptionFont.Handle := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);
end;

procedure FinalizeStock;
begin
  SmCaptionFont.Free;
  SmCaptionFont := nil;
  StockCompatibleBitmap.Free;
  StockMonoBitmap.Free;
  StockBitmap2.Free;
  StockBitmap1.Free;
end;

procedure RecreateStock;
begin
  FinalizeStock;
  InitializeStock;
end;

{ TShadow } ////////////////////////////////////////////////////////////////////

procedure TShadow.Clear(const R: TRect);
begin
  FClearRect := R;
end;

constructor TShadow.Create(const Bounds: TRect; Opacity: Byte; LoColor: Boolean; Edges: TShadowEdges);
begin
  inherited Create(nil);
  Hide;
  ParentWindow := Application.Handle;
  BoundsRect := Bounds;
  Color := clBtnShadow;
  FOpacity := Opacity;
  FEdges := Edges;
  FSaveBits := False;

  if LoColor then FStyle := ssFlat
  else if (@UpdateLayeredWindow <> nil) and (@AlphaBlend <> nil) then
    FStyle := ssLayered
  else if @AlphaBlend <> nil then
    FStyle := ssAlphaBlend
  else
    FStyle := ssFlat;
end;

procedure TShadow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := (Style and not (WS_CHILD or WS_GROUP or WS_TABSTOP)) or WS_POPUP;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    if FSaveBits then WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TShadow.GradB(const R: TRect);
var
  J, W, H: Integer;
  V: Cardinal;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    V := (255 - J shl 8 div H) shl 24;
    FillLongword(P^, W, V);
  end;
end;

procedure TShadow.GradBL(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := DMax - 1;
  CY := H - DMax;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.GradBR(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := W - DMax;
  CY := H - DMax;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.GradR(const R: TRect);
var
  I, J, W: Integer;
  P: ^Cardinal;
  ScanLine: array of Cardinal;
begin
  W := R.Right - R.Left;
  SetLength(ScanLine, W);
  for I := 0 to W - 1 do
    ScanLine[I] :=(255 - I shl 8 div W) shl 24;

  for J := R.Top to R.Bottom - 1 do
  begin
    P := FBuffer.ScanLine[J];
    Inc(P, R.Left);
    MoveLongword(ScanLine[0], P^, W);
  end;
end;

procedure TShadow.GradTR(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := W - DMax;
  CY := DMax - 1;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.Render;
var
  DstDC: HDC;
  SrcPos, DstPos: TPoint;
  TheSize: TSize;
  BlendFunc: TBlendFunction;
begin
  if FStyle <> ssLayered then Exit;
  Assert(Assigned(UpdateLayeredWindow));

  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or $00080000{WS_EX_LAYERED});
  DstDC := GetDC(0);
  try
    SrcPos := Point(0, 0);
    with BoundsRect do
    begin
      DstPos := Point(Left, Top);
      TheSize.cx := Right - Left;
      TheSize.cy := Bottom - Top;
    end;
    BlendFunc.BlendOp := 0;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := FOpacity;
    BlendFunc.AlphaFormat := 1;

    FBuffer := TBitmap.Create;
    FBuffer.PixelFormat := pf32bit;
    FBuffer.Width := TheSize.cx;
    FBuffer.Height := TheSize.cy;

    FillBuffer;

    UpdateLayeredWindow(
      Handle,
      DstDC,
      @DstPos,
      @TheSize,
      FBuffer.Canvas.Handle,
      @SrcPos,
      0,
      @BlendFunc,
      $00000002{ULW_ALPHA});

    FBuffer.Free;
  finally
    ReleaseDC(0, DstDC);
  end;
end;

procedure TShadow.Show(ParentHandle: HWND);
begin
  SetWindowPos(Handle, ParentHandle, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOSENDCHANGING or SWP_NOMOVE or
    SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_SHOWWINDOW);
end;

procedure TShadow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  SrcPos, DstPos: TPoint;
  TheSize: TSize;
  BlendFunc: TBlendFunction;
begin
  if FStyle = ssAlphaBlend then
  begin
    Assert(Assigned(AlphaBlend));

    { Dispatch all the painting messages }
    ProcessPaintMessages;

    SrcPos := Point(0, 0);
    with BoundsRect do
    begin
      DstPos := Point(Left, Top);
      TheSize.cx := Right - Left;
      TheSize.cy := Bottom - Top;
    end;

    FBuffer := TBitmap.Create;
    FBuffer.PixelFormat := pf32bit;
    FBuffer.Width := TheSize.cx;
    FBuffer.Height := TheSize.cy;

    FillBuffer;

    { Blend the buffer directly into the screen }
    BlendFunc.BlendOp := 0;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := FOpacity;
    BlendFunc.AlphaFormat := 1;
    AlphaBlend(Message.DC, 0, 0, TheSize.cx, TheSize.cy,
      FBuffer.Canvas.Handle, 0, 0, TheSize.cx, TheSize.cy, BlendFunc);
    FBuffer.Free;

    Message.Result := 1;
  end
  else inherited;
end;

procedure TShadow.WMNCHitTest(var Message: TMessage);
begin
  Message.Result := HTTRANSPARENT;
end;

{ THorzShadow }

procedure THorzShadow.FillBuffer;
var
  R: TRect;
  L1, L2, L3: Integer;
begin
  if seTopLeft in FEdges then L1 := Height else L1 := 0;
  if seBottomRight in FEdges then L3 := Height else L3 := 0;
  if L1 + L3 > Width then
  begin
    if (L1 > 0) and (L3 > 0) then
    begin
      L1 := Width div 2;
      L3 := L1;
    end
    else if L1 > 0 then L1 := Width
    else if L3 > 0 then L3 := Width;
  end;
  L2 := Width - L1 - L3;
  R := Rect(0, 0, Width, Height);
  R.Right := R.Left + L1;
  if L1 > 0 then GradBL(R);
  R.Left := R.Right;
  R.Right := R.Left + L2;
  if L2 > 0 then GradB(R);
  if L3 > 0 then
  begin
    R.Left := R.Right;
    R.Right := R.Left + L3;
    GradBR(R);
  end;
end;

{ TVertShadow }

procedure TVertShadow.FillBuffer;
var
  R: TRect;
  L1, L2, L3: Integer;
begin
  if seTopLeft in FEdges then L1 := Width else L1 := 0;
  if seBottomRight in FEdges then L3 := Width else L3 := 0;
  if L1 + L3 > Height then
  begin
    if (L1 > 0) and (L3 > 0) then
    begin
      L1 := Height div 2;
      L3 := L1;
    end
    else if L1 > 0 then L1 := Height
    else if L3 > 0 then L3 := Height;
  end;
  L2 := Height - L1 - L3;

  R := Rect(0, 0, Width, Height);
  R.Bottom := R.Top + L1;
  if L1 > 0 then GradTR(R);
  R.Top := R.Bottom;
  R.Bottom :=  R.Top + L2;
  if L2 > 0 then GradR(R);
  if L3 > 0 then
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Top + L3;
    GradBR(R);
  end;
end;

{ TShadows }

constructor TShadows.Create(R1, R2: TRect; TheSize: Integer; Opacity: Byte; LoColor: Boolean);
var
  R: TRect;
  R1Valid, R2Valid: Boolean;
begin
  if LoColor or
    ((@UpdateLayeredWindow = nil) and (@AlphaBlend = nil)) then
  begin
    TheSize := TheSize div 2;
  end;

  R1Valid := not IsRectEmpty(R1);
  R2Valid := not IsRectEmpty(R2);
  if not (R1Valid or R2Valid) then Exit;

  if R1Valid xor R2Valid then
  begin
    { A simple square shadow }
    if R1Valid then R := R1 else R:= R2;
    with R do
    begin
      V1 := TVertShadow.Create(Rect(Right, Top + TheSize, Right + TheSize, Bottom), Opacity, LoColor, [seTopLeft]);
      H1 := THorzShadow.Create(Rect(Left + TheSize, Bottom, Right + TheSize, Bottom + TheSize), Opacity, LoColor, [seTopLeft, seBottomRight])
    end;
  end
  else
  begin

    if (R1.Bottom <= R2.Top + 2) or (R1.Top >= R2.Bottom - 2) then
    begin
      if R1.Top > R2.Top then
      begin
        R := R2;
        R2 := R1;
        R1 := R;
      end;
      if R1.Left + TheSize < R2.Left then
        H1 := THorzShadow.Create(Rect(R1.Left + TheSize, R1.Bottom, R2.Left, R1.Bottom + TheSize), Opacity, LoColor, [seTopLeft]);
      H2 := THorzShadow.Create(Rect(R2.Left + TheSize, R2.Bottom, R2.Right + TheSize, R2.Bottom + TheSize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      V1 := TVertShadow.Create(Rect(R1.Right, R1.Top + TheSize, R1.Right + TheSize, R1.Bottom), Opacity, LoColor, [seTopLeft]);
      if R1.Right > R2.Right then
        H3 := THorzShadow.Create(Rect(R2.Right, R1.Bottom, R1.Right + TheSize, R1.Bottom + TheSize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      if R1.Right + TheSize < R2.Right then
        V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + TheSize, R2.Right + TheSize, R2.Bottom), Opacity, LoColor, [seTopLeft])
      else
        V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + 1, R2.Right + TheSize, R2.Bottom), Opacity, LoColor, []);
    end
    else if (R1.Right <= R2.Left + 2) or (R1.Left >= R2.Right - 2) then
    begin
      if R1.Left > R2.Left then
      begin
        R := R2;
        R2 := R1;
        R1 := R;
      end;
      if R1.Top + TheSize < R2.Top then
        V1 := TVertShadow.Create(Rect(R1.Right, R1.Top + TheSize, R1.Right + TheSize, R2.Top), Opacity, LoColor, [seTopLeft]);
      V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + TheSize, R2.Right + TheSize, R2.Bottom + TheSize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      H1 := THorzShadow.Create(Rect(R1.Left + TheSize, R1.Bottom, R1.Right, R1.Bottom + TheSize), Opacity, LoColor, [seTopLeft]);
      if R1.Bottom > R2.Bottom then
        V3 := TVertShadow.Create(Rect(R1.Right, R2.Bottom, R1.Right + TheSize, R1.Bottom + TheSize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      if R1.Bottom + TheSize < R2.Bottom then
        H2 := THorzShadow.Create(Rect(R2.Left + TheSize, R2.Bottom, R2.Right, R2.Bottom + TheSize), Opacity, LoColor, [seTopLeft])
      else
        H2 := THorzShadow.Create(Rect(R2.Left, R2.Bottom, R2.Right, R2.Bottom + TheSize), Opacity, LoColor, []);
    end;
  end;

  if V1 <> nil then V1.Render;
  if H1 <> nil then H1.Render;
  if V2 <> nil then V2.Render;
  if H2 <> nil then H2.Render;
  if V3 <> nil then V3.Render;
  if H3 <> nil then H3.Render;

  SetSaveBits(True);
end;

destructor TShadows.Destroy;
begin
  H3.Free;
  V3.Free;
  H2.Free;
  V2.Free;
  H1.Free;
  V1.Free;
  inherited;
end;

procedure TShadows.SetSaveBits(Value: Boolean);
begin
  FSaveBits := Value;
  if V1 <> nil then V1.FSaveBits := Value;
  if H1 <> nil then H1.FSaveBits := Value;
  if V2 <> nil then V2.FSaveBits := Value;
  if H2 <> nil then H2.FSaveBits := Value;
  if V3 <> nil then V3.FSaveBits := Value;
  if H3 <> nil then H3.FSaveBits := Value;
end;

procedure TShadows.Show(ParentHandle: HWND);
begin
  if V1 <> nil then V1.Show(ParentHandle);
  if H1 <> nil then H1.Show(ParentHandle);
  if V2 <> nil then V2.Show(ParentHandle);
  if H2 <> nil then H2.Show(ParentHandle);
  if V3 <> nil then V3.Show(ParentHandle);
  if H3 <> nil then H3.Show(ParentHandle);
end;

{ Gradients } //////////////////////////////////////////////////////////////////

const
  GRADIENT_CACHE_SIZE = 16;

type
  PRGBQuad = ^TRGBQuad;
  TRGBQuad = Integer;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0..0] of TRGBQuad;


var
  GradientCache: array [0..GRADIENT_CACHE_SIZE] of array of TRGBQuad;
  NextCacheIndex: Integer = 0;

function FindGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
begin
  Assert(Size > 0);
  Result := GRADIENT_CACHE_SIZE - 1;
  while Result >= 0 do
  begin
    if (Length(GradientCache[Result]) = Size) and
      (GradientCache[Result][0] = CL) and
      (GradientCache[Result][Length(GradientCache[Result]) - 1] = CR) then Exit;
    Dec(Result);
  end;
end;

function MakeGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
var
  R1, G1, B1: Integer;
  R2, G2, B2: Integer;
  R, G, B: Integer;
  I: Integer;
  Bias: Integer;
begin
  Assert(Size > 0);
  Result := NextCacheIndex;
  Inc(NextCacheIndex);
  if NextCacheIndex >= GRADIENT_CACHE_SIZE then NextCacheIndex := 0;
  R1 := CL and $FF;
  G1 := CL shr 8 and $FF;
  B1 := CL shr 16 and $FF;
  R2 := CR and $FF - R1;
  G2 := CR shr 8 and $FF - G1;
  B2 := CR shr 16 and $FF - B1;
  SetLength(GradientCache[Result], Size);
  Dec(Size);
  Bias := Size div 2;
  if Size > 0 then
    for I := 0 to Size do
    begin
      R := R1 + (R2 * I + Bias) div Size;
      G := G1 + (G2 * I + Bias) div Size;
      B := B1 + (B2 * I + Bias) div Size;
      GradientCache[Result][I] := R + G shl 8 + B shl 16;
    end
  else
  begin
    R := R1 + R2 div 2;
    G := G1 + G2 div 2;
    B := B1 + B2 div 2;
    GradientCache[Result][0] := R + G shl 8 + B shl 16;
  end;
end;

function GetGradient(Size: Integer; CL, CR: TRGBQuad): Integer;
begin
  Result := FindGradient(Size, CL, CR);
  if Result < 0 then Result := MakeGradient(Size, CL, CR);
end;

{ GradFill function }

procedure GradFill(DC: HDC; ARect: TRect; ClrTopLeft, ClrBottomRight: TColor; Kind: TGradientKind);
const
  GRAD_MODE: array [TGradientKind] of DWORD = (GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V);
  W: array [TGradientKind] of Integer = (2, 1);
  H: array [TGradientKind] of Integer = (1, 2);
type
  TriVertex = packed record
    X, Y: Longint;
    R, G, B, A: Word;
  end;
var
  V: array [0..1] of TriVertex;
  GR: GRADIENT_RECT;
  Size, I, Start, Finish: Integer;
  GradIndex: Integer;
  R, CR: TRect;
  Brush: HBRUSH;
begin
  if not RectVisible(DC, ARect) then Exit;

  ClrTopLeft := ColorToRGB(ClrTopLeft);
  ClrBottomRight := ColorToRGB(ClrBottomRight);
  if @GradientFill <> nil then
  begin
    { Use msimg32.dll }
    with V[0] do
    begin
      X := ARect.Left;
      Y := ARect.Top;
      R := ClrTopLeft shl 8 and $FF00;
      G := ClrTopLeft and $FF00;
      B := ClrTopLeft shr 8 and $FF00;
      A := 0;
    end;
    with V[1] do
    begin
      X := ARect.Right;
      Y := ARect.Bottom;
      R := ClrBottomRight shl 8 and $FF00;
      G := ClrBottomRight and $FF00;
      B := ClrBottomRight shr 8 and $FF00;
      A := 0;
    end;
    GR.UpperLeft := 0; GR.LowerRight := 1;
    GradientFill(DC, @V, 2, @GR, 1, GRAD_MODE[Kind]);
  end
  else
  begin
    { Have to do it manually if msimg32.dll is not available }
    GetClipBox(DC, CR);

    if Kind = gkHorz then
    begin
      Size := ARect.Right - ARect.Left;
      if Size <= 0 then Exit;
      Start := 0; Finish := Size - 1;
      if CR.Left > ARect.Left then Inc(Start, CR.Left - ARect.Left);
      if CR.Right < ARect.Right then Dec(Finish, ARect.Right - CR.Right);
      R := ARect; Inc(R.Left, Start); R.Right := R.Left + 1;
    end
    else
    begin
      Size := ARect.Bottom - ARect.Top;
      if Size <= 0 then Exit;
      Start := 0; Finish := Size - 1;
      if CR.Top > ARect.Top then Inc(Start, CR.Top - ARect.Top);
      if CR.Bottom < ARect.Bottom then Dec(Finish, ARect.Bottom - CR.Bottom);
      R := ARect; Inc(R.Top, Start); R.Bottom := R.Top + 1;
    end;
    GradIndex := GetGradient(Size, ClrTopLeft, ClrBottomRight);
    for I := Start to Finish do
    begin
      Brush := CreateSolidBrush(GradientCache[GradIndex][I]);
      Windows.FillRect(DC, R, Brush);
      OffsetRect(R, Integer(Kind = gkHorz), Integer(Kind = gkVert));
      DeleteObject(Brush);
    end;
  end;
end;

{ Brushed Fill } ///////////////////////////////////////////////////////////////

{ Templates }

const
  NUM_TEMPLATES = 8;
  MIN_TEMPLATE_SIZE = 100;
  MAX_TEMPLATE_SIZE = 200;

var
  ThreadTemplates: array [0..NUM_TEMPLATES - 1] of array of Integer;
  RandThreadIndex: array [0..1023] of Integer;
  RandThreadPositions: array [0..1023] of Integer;

procedure InitializeBrushedFill;
const
  Pi = 3.14159265358987;
var
  TemplateIndex, Size, I, V, V1, V2: Integer;
  T, R12, R13, R14, R21, R22, R23, R24: Single;
begin
  { Make thread templates }
  for TemplateIndex := 0 to NUM_TEMPLATES - 1 do
  begin
    Size := (MIN_TEMPLATE_SIZE + Random(MAX_TEMPLATE_SIZE - MIN_TEMPLATE_SIZE + 1)) div 2;
    SetLength(ThreadTemplates[TemplateIndex], Size * 2);
    R12 := Random * 2 * Pi;
    R13 := Random * 2 * Pi;
    R14 := Random * 2 * Pi;
    R21 := Random * 2 * Pi;
    R22 := Random * 2 * Pi;
    R23 := Random * 2 * Pi;
    R24 := Random * 2 * Pi;
    for I := 0 to Size - 1 do
    begin
      T := 2 * Pi * I / Size;
      V1 := Round(150 * Sin(T) + 100 * Sin(2 * T + R12) + 50 * Sin(3 * T + R13) + 20 * Sin(4 * T + R14));
      if V1 > 255 then V1 := 255;
      if V1 < -255 then V1 := -255;

      V2 := Round(150 * Sin(T + R21) + 100 * Sin(2 * T + R22) + 50 * Sin(3 * T + R23) + 20 * Sin(4 * T + R24));
      if V2 > 255 then V2 := 255;
      if V2 < -255 then V2 := -255;

      if Abs(V2 - V1) > 300 then
      begin
        V := (V1 + V2) div 2;
        V1 := V - 150;
        V2 := V + 150;
      end;

      ThreadTemplates[TemplateIndex][I * 2] := Min(V1, V2);
      ThreadTemplates[TemplateIndex][I * 2 + 1] := Max(V1, V2);
    end;
  end;

  { Initialize Rand arrays }
  for I := 0 to 1023 do
  begin
    RandThreadIndex[I] := Random(NUM_TEMPLATES);
    V1 := Random(Length(ThreadTemplates[RandThreadIndex[I]])) and not $1;
    if Odd(I) then Inc(V1);
    RandThreadPositions[I] := V1;
  end;
end;

{ Cache }

const
  THREAD_CACHE_SIZE = 16;

type
  TThreadCacheItem = record
    BaseColor: TColorRef;
    Roughness: Integer;
    Bitmaps: array [0..NUM_TEMPLATES - 1] of HBITMAP;
  end;

var
  ThreadCache: array [0..THREAD_CACHE_SIZE] of TThreadCacheItem;
  NextCacheEntry: Integer = 0;

procedure ClearCacheItem(var CacheItem: TThreadCacheItem);
var
  I: Integer;
begin
  with CacheItem do
  begin
    BaseColor := $FFFFFFFF;
    Roughness := -1;
    for I := NUM_TEMPLATES - 1 downto 0 do
    begin
      if Bitmaps[I] <> 0 then
      begin
        DeleteObject(Bitmaps[I]);
        Bitmaps[I] := 0;
      end;
    end;
  end;
end;

procedure ResetBrushedFillCache;
var
  I: Integer;
begin
  { Should be called each time the screen parameters change }
  for I := THREAD_CACHE_SIZE - 1 downto 0 do ClearCacheItem(ThreadCache[I]);
end;

procedure FinalizeBrushedFill;
begin
  ResetBrushedFillCache;
end;

procedure MakeCacheItem(var CacheItem: TThreadCacheItem; Color: TColorRef; Roughness: Integer);
var
  TemplateIndex, Size, I, V: Integer;
  CR, CG, CB: Integer;
  R, G, B: Integer;
  ScreenDC: HDC;
  BMI: TBitmapInfo;
  Bits: PRGBQuadArray;
  DIBSection: HBITMAP;
  DIBDC, CacheDC: HDC;
begin
  ScreenDC := GetDC(0);
  FillChar(BMI, SizeOf(TBitmapInfo), 0);
  with BMI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biCompression := BI_RGB;
    biWidth := MAX_TEMPLATE_SIZE;
    biHeight := -1;
    biBitCount := 32;
  end;
  DIBSection := CreateDIBSection(0, BMI, DIB_RGB_COLORS, Pointer(Bits), 0, 0);
  DIBDC := CreateCompatibleDC(0);
  SelectObject(DIBDC, DIBSection);
  CacheDC := CreateCompatibleDC(0);

  CR := Color shl 8 and $FF00;
  CG := Color and $FF00;
  CB := Color shr 8 and $FF00;

  try
  for TemplateIndex := 0 to NUM_TEMPLATES - 1 do
  begin
    CacheItem.BaseColor := Color;
    CacheItem.Roughness := Roughness;
    Size := Length(ThreadTemplates[TemplateIndex]);

      if CacheItem.Bitmaps[TemplateIndex] = 0 then
        CacheItem.Bitmaps[TemplateIndex] := CreateCompatibleBitmap(ScreenDC, Size, 1);
      SelectObject(CacheDC, CacheItem.Bitmaps[TemplateIndex]);

    for I := 0 to Size - 1 do
    begin
      V := ThreadTemplates[TemplateIndex][I];
      R := CR + V * Roughness;
      G := CG + V * Roughness;
      B := CB + V * Roughness;
      if R < 0 then R := 0;
      if G < 0 then G := 0;
      if B < 0 then B := 0;
      if R > $EF00 then R := $EF00;
      if G > $EF00 then G := $EF00;
      if B > $EF00 then B := $EF00;
      Bits^[I] := (R and $FF00 + (G and $FF00) shl 8 + (B and $FF00) shl 16) shr 8;
    end;

      BitBlt(CacheDC, 0, 0, Size, 1, DIBDC, 0, 0, SRCCOPY);
    end;

  finally
    DeleteDC(CacheDC);
    DeleteDC(DIBDC);
    DeleteObject(DIBSection);
    ReleaseDC(0, ScreenDC);
  end;
end;

function FindCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := THREAD_CACHE_SIZE - 1;
  while Result >= 0 do
    if (ThreadCache[Result].BaseColor = Color) and (ThreadCache[Result].Roughness = Roughness) then Exit
    else Dec(Result);
end;

function GetCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := FindCacheItem(Color, Roughness);
  if Result >= 0 then Exit
  else
  begin
    Result := NextCacheEntry;
    MakeCacheItem(ThreadCache[Result], Color, Roughness);
    NextCacheEntry := (NextCacheEntry + 1) mod THREAD_CACHE_SIZE;
  end;
end;

procedure BrushedFill(DC: HDC; Origin: PPoint; ARect: TRect; Color: TColor; Roughness: Integer);
const
  ZeroOrigin: TPoint = (X: 0; Y: 0);
var
  CR: TColorRef;
  X, Y: Integer;
  CacheIndex: Integer;
  TemplateIndex: Integer;
  CacheDC: HDC;
  Size: Integer;
  BoxR: TRect;
begin
  if (Color = clNone) or not RectVisible(DC, ARect) then Exit;
  CR := GetBGR(ColorToRGB(Color));
  if Origin = nil then Origin := @ZeroOrigin;
  CacheIndex := GetCacheItem(CR, Roughness);
  GetClipBox(DC, BoxR);
  IntersectRect(ARect, ARect, BoxR);
  SaveDC(DC);
  with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);

  CacheDC := CreateCompatibleDC(0);
  for Y := ARect.Top to ARect.Bottom - 1 do
  begin
    TemplateIndex := RandThreadIndex[(65536 + Y - Origin.Y) mod 1024];
    Size := Length(ThreadTemplates[TemplateIndex]);
    X := -RandThreadPositions[(65536 + Y - Origin.Y) mod 1024] + Origin.X;
    SelectObject(CacheDC, ThreadCache[CacheIndex].Bitmaps[TemplateIndex]);
    while X < ARect.Right do
    begin
      if X + Size >= ARect.Left then BitBlt(DC, X, Y, Size, 1, CacheDC, 0, 0, SRCCOPY);
      Inc(X, Size);
    end;
  end;
  DeleteDC(CacheDC);

  RestoreDC(DC, -1);
end;

var
  hMSImg: HModule;

initialization
  @UpdateLayeredWindow := GetProcAddress(
    GetModuleHandle('user32.dll'), 'UpdateLayeredWindow');
  hMSImg := LoadLibrary('msimg32.dll');
  if hMSImg <> 0 then
  begin
    @AlphaBlend   := GetProcAddress(hMSImg, 'AlphaBlend');
    @GradientFill := GetProcAddress(hMSImg, 'GradientFill');
  end;
  InitializeStock;
  InitializeBrushedFill;
  ResetBrushedFillCache;
finalization
  FinalizeBrushedFill;
  FinalizeStock;
  if hMSImg <> 0 then FreeLibrary(hMSImg);
end.
