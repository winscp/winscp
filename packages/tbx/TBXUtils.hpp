// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXUtils.pas' rev: 6.00

#ifndef TBXUtilsHPP
#define TBXUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxutils
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TGradientKind { gkHorz, gkVert };
#pragma option pop

typedef Graphics::TColor *PColor;

#pragma option push -b-
enum TBXUtils__1 { seTopLeft, seBottomRight };
#pragma option pop

typedef Set<TBXUtils__1, seTopLeft, seBottomRight>  TShadowEdges;

#pragma option push -b-
enum TShadowStyle { ssFlat, ssLayered, ssAlphaBlend };
#pragma option pop

class DELPHICLASS TShadow;
class PASCALIMPLEMENTATION TShadow : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
protected:
	Byte FOpacity;
	Graphics::TBitmap* FBuffer;
	#pragma pack(push, 1)
	Types::TRect FClearRect;
	#pragma pack(pop)
	
	TShadowEdges FEdges;
	TShadowStyle FStyle;
	bool FSaveBits;
	void __fastcall GradR(const Types::TRect &R);
	void __fastcall GradB(const Types::TRect &R);
	void __fastcall GradBR(const Types::TRect &R);
	void __fastcall GradTR(const Types::TRect &R);
	void __fastcall GradBL(const Types::TRect &R);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall FillBuffer(void) = 0 ;
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	
public:
	__fastcall TShadow(const Types::TRect &Bounds, Byte Opacity, bool LoColor, TShadowEdges Edges);
	void __fastcall Clear(const Types::TRect &R);
	void __fastcall Render(void);
	HIDESBASE void __fastcall Show(HWND ParentHandle);
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TShadow(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TShadow(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS THorzShadow;
class PASCALIMPLEMENTATION THorzShadow : public TShadow 
{
	typedef TShadow inherited;
	
protected:
	virtual void __fastcall FillBuffer(void);
public:
	#pragma option push -w-inl
	/* TShadow.Create */ inline __fastcall THorzShadow(const Types::TRect &Bounds, Byte Opacity, bool LoColor, TShadowEdges Edges) : TShadow(Bounds, Opacity, LoColor, Edges) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~THorzShadow(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall THorzShadow(HWND ParentWindow) : TShadow(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TVertShadow;
class PASCALIMPLEMENTATION TVertShadow : public TShadow 
{
	typedef TShadow inherited;
	
protected:
	virtual void __fastcall FillBuffer(void);
public:
	#pragma option push -w-inl
	/* TShadow.Create */ inline __fastcall TVertShadow(const Types::TRect &Bounds, Byte Opacity, bool LoColor, TShadowEdges Edges) : TShadow(Bounds, Opacity, LoColor, Edges) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TVertShadow(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TVertShadow(HWND ParentWindow) : TShadow(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TShadows;
class PASCALIMPLEMENTATION TShadows : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	bool FSaveBits;
	void __fastcall SetSaveBits(bool Value);
	
protected:
	TShadow* V1;
	TShadow* H1;
	TShadow* V2;
	TShadow* H2;
	TShadow* V3;
	TShadow* H3;
	
public:
	__fastcall TShadows(const Types::TRect &R1, const Types::TRect &R2, int Size, Byte Opacity, bool LoColor);
	__fastcall virtual ~TShadows(void);
	void __fastcall Show(HWND ParentHandle);
	__property bool SaveBits = {read=FSaveBits, write=SetSaveBits, nodefault};
};


struct TBlendFunction;
typedef TBlendFunction *PBlendFunction;

#pragma pack(push, 1)
struct TBlendFunction
{
	Byte BlendOp;
	Byte BlendFlags;
	Byte SourceConstantAlpha;
	Byte AlphaFormat;
} ;
#pragma pack(pop)

typedef int __stdcall (*TUpdateLayeredWindow)(HWND hWnd, HDC hdcDst, Types::PPoint pptDst, Types::PSize psize, HDC hdcSrc, Types::PPoint pptSrc, unsigned crKey, PBlendFunction pblend, int dwFlags);

typedef BOOL __stdcall (*TAlphaBlend)(HDC hdcDest, int nXOriginDest, int nYOriginDest, int nWidthDest, int nHeightDest, HDC hdcSrc, int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc, TBlendFunction blendFunction);

typedef unsigned __stdcall (*TGradientFill)(HDC Handle, void * pVertex, unsigned dwNumVertex, void * pMesh, unsigned dwNumMesh, unsigned dwMode);

//-- var, const, procedure ---------------------------------------------------
static const Shortint SHD_DENSE = 0x0;
static const Shortint SHD_LIGHT = 0x1;
extern PACKAGE Graphics::TBitmap* StockBitmap1;
extern PACKAGE Graphics::TBitmap* StockBitmap2;
extern PACKAGE Graphics::TBitmap* StockMonoBitmap;
extern PACKAGE Graphics::TBitmap* StockCompatibleBitmap;
extern PACKAGE Graphics::TFont* SmCaptionFont;
static const int ROP_DSPDxax = 0xe20746;
extern PACKAGE TUpdateLayeredWindow UpdateLayeredWindow;
extern PACKAGE TAlphaBlend AlphaBlend;
extern PACKAGE TGradientFill GradientFill;
extern PACKAGE int __fastcall GetTextHeightW(HDC DC);
extern PACKAGE int __fastcall GetTextWidthW(HDC DC, const WideString S, bool StripAccelChar);
extern PACKAGE void __fastcall DrawRotatedTextW(HDC DC, WideString AText, const Types::TRect &ARect, const unsigned AFormat);
extern PACKAGE WideString __fastcall EscapeAmpersandsW(const WideString S);
extern PACKAGE wchar_t __fastcall FindAccelCharW(const WideString S);
extern PACKAGE WideString __fastcall StripAccelCharsW(const WideString S);
extern PACKAGE WideString __fastcall StripTrailingPunctuationW(const WideString S);
extern PACKAGE bool __fastcall TBXCheckWin32Version(int AMajor, int AMinor = 0x0);
extern PACKAGE void __fastcall GetRGB(Graphics::TColor C, /* out */ int &R, /* out */ int &G, /* out */ int &B);
extern PACKAGE Graphics::TColor __fastcall MixColors(Graphics::TColor C1, Graphics::TColor C2, int W1);
extern PACKAGE bool __fastcall SameColors(Graphics::TColor C1, Graphics::TColor C2);
extern PACKAGE Graphics::TColor __fastcall Lighten(Graphics::TColor C, int Amount);
extern PACKAGE Graphics::TColor __fastcall NearestLighten(Graphics::TColor C, int Amount);
extern PACKAGE Graphics::TColor __fastcall NearestMixedColor(Graphics::TColor C1, Graphics::TColor C2, int W1);
extern PACKAGE int __fastcall ColorIntensity(Graphics::TColor C);
extern PACKAGE bool __fastcall IsDarkColor(Graphics::TColor C, int Threshold = 0x64);
extern PACKAGE Graphics::TColor __fastcall Blend(Graphics::TColor C1, Graphics::TColor C2, int W1);
extern PACKAGE void __fastcall SetContrast(Graphics::TColor &Color, Graphics::TColor BkgndColor, int Threshold);
extern PACKAGE void __fastcall RGBtoHSL(Graphics::TColor RGB, /* out */ float &H, /* out */ float &S, /* out */ float &L);
extern PACKAGE Graphics::TColor __fastcall HSLtoRGB(float H, float S, float L);
extern PACKAGE unsigned __fastcall GetBGR(unsigned C);
extern PACKAGE void __fastcall SetPixelEx(HDC DC, int X, int Y, unsigned C, unsigned Alpha = (unsigned)(0xff));
extern PACKAGE HPEN __fastcall CreatePenEx(Graphics::TColor Color);
extern PACKAGE HBRUSH __fastcall CreateBrushEx(Graphics::TColor Color);
extern PACKAGE bool __fastcall FillRectEx(HDC DC, const Types::TRect &Rect, Graphics::TColor Color);
extern PACKAGE bool __fastcall FrameRectEx(HDC DC, Types::TRect &Rect, Graphics::TColor Color, bool Adjust);
extern PACKAGE void __fastcall DrawLineEx(HDC DC, int X1, int Y1, int X2, int Y2, Graphics::TColor Color);
extern PACKAGE bool __fastcall PolyLineEx(HDC DC, const Types::TPoint * Points, const int Points_Size, Graphics::TColor Color)/* overload */;
extern PACKAGE void __fastcall PolygonEx(HDC DC, const Types::TPoint * Points, const int Points_Size, Graphics::TColor OutlineColor, Graphics::TColor FillColor);
extern PACKAGE void __fastcall RoundRectEx(HDC DC, int Left, int Top, int Right, int Bottom, Graphics::TColor EllipseWidth, Graphics::TColor EllipseHeight, Graphics::TColor OutlineColor, Graphics::TColor FillColor)/* overload */;
extern PACKAGE void __fastcall RoundRectEx(HDC DC, const Types::TRect &R, Graphics::TColor EllipseWidth, Graphics::TColor EllipseHeight, Graphics::TColor OutlineColor, Graphics::TColor FillColor)/* overload */;
extern PACKAGE HBRUSH __fastcall CreateDitheredBrush(Graphics::TColor C1, Graphics::TColor C2);
extern PACKAGE void __fastcall DitherRect(HDC DC, const Types::TRect &R, Graphics::TColor C1, Graphics::TColor C2);
extern PACKAGE void __fastcall Frame3D(HDC DC, Types::TRect &Rect, Graphics::TColor TopColor, Graphics::TColor BottomColor, bool Adjust);
extern PACKAGE void __fastcall DrawDraggingOutline(HDC DC, const Types::TRect &NewRect, const Types::TRect &OldRect);
extern PACKAGE void __fastcall FillLongword(void *X, int Count, unsigned Value);
extern PACKAGE void __fastcall MoveLongword(const void *Source, void *Dest, int Count);
extern PACKAGE void __fastcall DrawTBXIcon(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, bool HiContrast);
extern PACKAGE void __fastcall BlendTBXIcon(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, Byte Opacity);
extern PACKAGE void __fastcall HighlightTBXIcon(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, Graphics::TColor HighlightColor, Byte Amount);
extern PACKAGE void __fastcall DrawTBXIconShadow(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, int Density);
extern PACKAGE void __fastcall DrawTBXIconFlatShadow(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, Graphics::TColor ShadowColor);
extern PACKAGE void __fastcall DrawTBXIconFullShadow(Graphics::TCanvas* Canvas, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, Graphics::TColor ShadowColor);
extern PACKAGE void __fastcall DrawGlyph(HDC DC, int X, int Y, Imglist::TCustomImageList* ImageList, int ImageIndex, Graphics::TColor Color)/* overload */;
extern PACKAGE void __fastcall DrawGlyph(HDC DC, const Types::TRect &R, Imglist::TCustomImageList* ImageList, int ImageIndex, Graphics::TColor Color)/* overload */;
extern PACKAGE void __fastcall DrawGlyph(HDC DC, int X, int Y, const void *Bits, Graphics::TColor Color)/* overload */;
extern PACKAGE void __fastcall DrawGlyph(HDC DC, const Types::TRect &R, int Width, int Height, const void *Bits, Graphics::TColor Color)/* overload */;
extern PACKAGE Types::TPoint __fastcall GetClientSizeEx(Controls::TWinControl* Control);
extern PACKAGE void __fastcall RecreateStock(void);
extern PACKAGE void __fastcall GradFill(HDC DC, const Types::TRect &ARect, Graphics::TColor ClrTopLeft, Graphics::TColor ClrBottomRight, TGradientKind Kind);
extern PACKAGE void __fastcall ResetBrushedFillCache(void);
extern PACKAGE void __fastcall BrushedFill(HDC DC, Types::PPoint Origin, const Types::TRect &ARect, Graphics::TColor Color, int Roughness);

}	/* namespace Tbxutils */
using namespace Tbxutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXUtils
