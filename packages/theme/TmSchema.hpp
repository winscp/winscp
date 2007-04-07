// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TmSchema.pas' rev: 6.00

#ifndef TmSchemaHPP
#define TmSchemaHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

#include "tmschema.h"


namespace Tmschema
{
//-- type declarations -------------------------------------------------------
typedef unsigned BGTYPE;

typedef unsigned IMAGELAYOUT;

typedef unsigned BORDERTYPE;

typedef unsigned FILLTYPE;

typedef unsigned SIZINGTYPE;

typedef unsigned HALIGN;

typedef unsigned CONTENTALIGNMENT;

typedef unsigned VALIGN;

typedef unsigned OFFSETTYPE;

typedef unsigned ICONEFFECT;

typedef unsigned TEXTSHADOWTYPE;

typedef unsigned GLYPHTYPE;

typedef unsigned IMAGESELECTTYPE;

typedef unsigned TRUESIZESCALINGTYPE;

typedef unsigned GLYPHFONTSIZINGTYPE;

typedef unsigned WINDOWPARTS;

typedef unsigned FRAMESTATES;

typedef unsigned CAPTIONSTATES;

typedef unsigned MAXCAPTIONSTATES;

typedef unsigned MINCAPTIONSTATES;

typedef unsigned HORZSCROLLSTATES;

typedef unsigned HORZTHUMBSTATES;

typedef unsigned VERTSCROLLSTATES;

typedef unsigned VERTTHUMBSTATES;

typedef unsigned SYSBUTTONSTATES;

typedef unsigned MINBUTTONSTATES;

typedef unsigned MAXBUTTONSTATES;

typedef unsigned RESTOREBUTTONSTATES;

typedef unsigned HELPBUTTONSTATES;

typedef unsigned CLOSEBUTTONSTATES;

typedef unsigned BUTTONPARTS;

typedef unsigned PUSHBUTTONSTATES;

typedef unsigned RADIOBUTTONSTATES;

typedef unsigned CHECKBOXSTATES;

typedef unsigned GROUPBOXSTATES;

typedef unsigned REBARPARTS;

typedef unsigned CHEVRONSTATES;

typedef unsigned TOOLBARPARTS;

typedef unsigned TOOLBARSTATES;

typedef unsigned STATUSPARTS;

typedef unsigned MENUPARTS;

typedef unsigned MENUSTATES;

typedef unsigned LISTVIEWPARTS;

typedef unsigned LISTITEMSTATES;

typedef unsigned HEADERPARTS;

typedef unsigned HEADERITEMSTATES;

typedef unsigned HEADERITEMLEFTSTATES;

typedef unsigned HEADERITEMRIGHTSTATES;

typedef unsigned HEADERSORTARROWSTATES;

typedef unsigned PROGRESSPARTS;

typedef unsigned TABPARTS;

typedef unsigned TABITEMSTATES;

typedef unsigned TABITEMLEFTEDGESTATES;

typedef unsigned TABITEMRIGHTEDGESTATES;

typedef unsigned TABITEMBOTHEDGESSTATES;

typedef unsigned TOPTABITEMSTATES;

typedef unsigned TOPTABITEMLEFTEDGESTATES;

typedef unsigned TOPTABITEMRIGHTEDGESTATES;

typedef unsigned TOPTABITEMBOTHEDGESSTATES;

typedef unsigned TRACKBARPARTS;

typedef unsigned TRACKBARSTATES;

typedef unsigned TRACKSTATES;

typedef unsigned TRACKVERTSTATES;

typedef unsigned THUMBSTATES;

typedef unsigned THUMBBOTTOMSTATES;

typedef unsigned THUMBTOPSTATES;

typedef unsigned THUMBVERTSTATES;

typedef unsigned THUMBLEFTSTATES;

typedef unsigned THUMBRIGHTSTATES;

typedef unsigned TICSSTATES;

typedef unsigned TICSVERTSTATES;

typedef unsigned TOOLTIPPARTS;

typedef unsigned CLOSESTATES;

typedef unsigned STANDARDSTATES;

typedef unsigned BALLOONSTATES;

typedef unsigned TREEVIEWPARTS;

typedef unsigned TREEITEMSTATES;

typedef unsigned GLYPHSTATES;

typedef unsigned SPINPARTS;

typedef unsigned UPSTATES;

typedef unsigned DOWNSTATES;

typedef unsigned UPHORZSTATES;

typedef unsigned DOWNHORZSTATES;

typedef unsigned PAGEPARTS;

typedef unsigned SCROLLBARPARTS;

typedef unsigned ARROWBTNSTATES;

typedef unsigned SCROLLBARSTATES;

typedef unsigned SIZEBOXSTATES;

typedef unsigned EDITPARTS;

typedef unsigned EDITTEXTSTATES;

typedef unsigned COMBOBOXPARTS;

typedef unsigned COMBOBOXSTATES;

typedef unsigned CLOCKPARTS;

typedef unsigned CLOCKSTATES;

typedef unsigned TRAYNOTIFYPARTS;

typedef unsigned TASKBARPARTS;

typedef unsigned TASKBANDPARTS;

typedef unsigned STARTPANELPARTS;

typedef unsigned MOREPROGRAMSARROWSTATES;

typedef unsigned LOGOFFBUTTONSSTATES;

typedef unsigned EXPLORERBARPARTS;

typedef unsigned HEADERCLOSESTATES;

typedef unsigned HEADERPINSTATES;

typedef unsigned IEBARMENUSTATES;

typedef unsigned NORMALGROUPCOLLAPSESTATES;

typedef unsigned NORMALGROUPEXPANDSTATES;

typedef unsigned SPECIALGROUPCOLLAPSESTATES;

typedef unsigned SPECIALGROUPEXPANDSTATES;

typedef unsigned MENUBANDPARTS;

typedef unsigned MENUBANDSTATES;

//-- var, const, procedure ---------------------------------------------------
static const Shortint BT_IMAGEFILE = 0x0;
static const Shortint BT_BORDERFILL = 0x1;
static const Shortint BT_NONE = 0x2;
static const Shortint IL_VERTICAL = 0x0;
static const Shortint IL_HORIZONTAL = 0x1;
static const Shortint BT_RECT = 0x0;
static const Shortint BT_ROUNDRECT = 0x1;
static const Shortint BT_ELLIPSE = 0x2;
static const Shortint FT_SOLID = 0x0;
static const Shortint FT_VERTGRADIENT = 0x1;
static const Shortint FT_HORZGRADIENT = 0x2;
static const Shortint FT_RADIALGRADIENT = 0x3;
static const Shortint FT_TILEIMAGE = 0x4;
static const Shortint ST_TRUESIZE = 0x0;
static const Shortint ST_STRETCH = 0x1;
static const Shortint ST_TILE = 0x2;
static const Shortint HA_LEFT = 0x0;
static const Shortint HA_CENTER = 0x1;
static const Shortint HA_RIGHT = 0x2;
static const Shortint CA_LEFT = 0x0;
static const Shortint CA_CENTER = 0x1;
static const Shortint CA_RIGHT = 0x2;
static const Shortint VA_TOP = 0x0;
static const Shortint VA_CENTER = 0x1;
static const Shortint VA_BOTTOM = 0x2;
static const Shortint OT_TOPLEFT = 0x0;
static const Shortint OT_TOPRIGHT = 0x1;
static const Shortint OT_TOPMIDDLE = 0x2;
static const Shortint OT_BOTTOMLEFT = 0x3;
static const Shortint OT_BOTTOMRIGHT = 0x4;
static const Shortint OT_BOTTOMMIDDLE = 0x5;
static const Shortint OT_MIDDLELEFT = 0x6;
static const Shortint OT_MIDDLERIGHT = 0x7;
static const Shortint OT_LEFTOFCAPTION = 0x8;
static const Shortint OT_RIGHTOFCAPTION = 0x9;
static const Shortint OT_LEFTOFLASTBUTTON = 0xa;
static const Shortint OT_RIGHTOFLASTBUTTON = 0xb;
static const Shortint OT_ABOVELASTBUTTON = 0xc;
static const Shortint OT_BELOWLASTBUTTON = 0xd;
static const Shortint ICE_NONE = 0x0;
static const Shortint ICE_GLOW = 0x1;
static const Shortint ICE_SHADOW = 0x2;
static const Shortint ICE_PULSE = 0x3;
static const Shortint ICE_ALPHA = 0x4;
static const Shortint TST_NONE = 0x0;
static const Shortint TST_SINGLE = 0x1;
static const Shortint TST_CONTINUOUS = 0x2;
static const Shortint GT_NONE = 0x0;
static const Shortint GT_IMAGEGLYPH = 0x1;
static const Shortint GT_FONTGLYPH = 0x2;
static const Shortint IST_NONE = 0x0;
static const Shortint IST_SIZE = 0x1;
static const Shortint IST_DPI = 0x2;
static const Shortint TSST_NONE = 0x0;
static const Shortint TSST_SIZE = 0x1;
static const Shortint TSST_DPI = 0x2;
static const Shortint GFST_NONE = 0x0;
static const Shortint GFST_SIZE = 0x1;
static const Shortint GFST_DPI = 0x2;
static const Byte TMT_STRING = 0xc9;
static const Byte TMT_INT = 0xca;
static const Byte TMT_BOOL = 0xcb;
static const Byte TMT_COLOR = 0xcc;
static const Byte TMT_MARGINS = 0xcd;
static const Byte TMT_FILENAME = 0xce;
static const Byte TMT_SIZE = 0xcf;
static const Byte TMT_POSITION = 0xd0;
static const Byte TMT_RECT = 0xd1;
static const Byte TMT_FONT = 0xd2;
static const Byte TMT_INTLIST = 0xd3;
static const Word TMT_COLORSCHEMES = 0x191;
static const Word TMT_SIZES = 0x192;
static const Word TMT_CHARSET = 0x193;
static const Word TMT_DISPLAYNAME = 0x259;
static const Word TMT_TOOLTIP = 0x25a;
static const Word TMT_COMPANY = 0x25b;
static const Word TMT_AUTHOR = 0x25c;
static const Word TMT_COPYRIGHT = 0x25d;
static const Word TMT_URL = 0x25e;
static const Word TMT_VERSION = 0x25f;
static const Word TMT_DESCRIPTION = 0x260;
static const Word TMT_CAPTIONFONT = 0x321;
static const Word TMT_SMALLCAPTIONFONT = 0x322;
static const Word TMT_MENUFONT = 0x323;
static const Word TMT_STATUSFONT = 0x324;
static const Word TMT_MSGBOXFONT = 0x325;
static const Word TMT_ICONTITLEFONT = 0x326;
static const Word TMT_FLATMENUS = 0x3e9;
static const Word TMT_SIZINGBORDERWIDTH = 0x4b1;
static const Word TMT_SCROLLBARWIDTH = 0x4b2;
static const Word TMT_SCROLLBARHEIGHT = 0x4b3;
static const Word TMT_CAPTIONBARWIDTH = 0x4b4;
static const Word TMT_CAPTIONBARHEIGHT = 0x4b5;
static const Word TMT_SMCAPTIONBARWIDTH = 0x4b6;
static const Word TMT_SMCAPTIONBARHEIGHT = 0x4b7;
static const Word TMT_MENUBARWIDTH = 0x4b8;
static const Word TMT_MENUBARHEIGHT = 0x4b9;
static const Word TMT_MINCOLORDEPTH = 0x515;
static const Word TMT_CSSNAME = 0x579;
static const Word TMT_XMLNAME = 0x57a;
static const Word TMT_SCROLLBAR = 0x641;
static const Word TMT_BACKGROUND = 0x642;
static const Word TMT_ACTIVECAPTION = 0x643;
static const Word TMT_INACTIVECAPTION = 0x644;
static const Word TMT_MENU = 0x645;
static const Word TMT_WINDOW = 0x646;
static const Word TMT_WINDOWFRAME = 0x647;
static const Word TMT_MENUTEXT = 0x648;
static const Word TMT_WINDOWTEXT = 0x649;
static const Word TMT_CAPTIONTEXT = 0x64a;
static const Word TMT_ACTIVEBORDER = 0x64b;
static const Word TMT_INACTIVEBORDER = 0x64c;
static const Word TMT_APPWORKSPACE = 0x64d;
static const Word TMT_HIGHLIGHT = 0x64e;
static const Word TMT_HIGHLIGHTTEXT = 0x64f;
static const Word TMT_BTNFACE = 0x650;
static const Word TMT_BTNSHADOW = 0x651;
static const Word TMT_GRAYTEXT = 0x652;
static const Word TMT_BTNTEXT = 0x653;
static const Word TMT_INACTIVECAPTIONTEXT = 0x654;
static const Word TMT_BTNHIGHLIGHT = 0x655;
static const Word TMT_DKSHADOW3D = 0x656;
static const Word TMT_LIGHT3D = 0x657;
static const Word TMT_INFOTEXT = 0x658;
static const Word TMT_INFOBK = 0x659;
static const Word TMT_BUTTONALTERNATEFACE = 0x65a;
static const Word TMT_HOTTRACKING = 0x65b;
static const Word TMT_GRADIENTACTIVECAPTION = 0x65c;
static const Word TMT_GRADIENTINACTIVECAPTION = 0x65d;
static const Word TMT_MENUHILIGHT = 0x65e;
static const Word TMT_MENUBAR = 0x65f;
static const Word TMT_FROMHUE1 = 0x709;
static const Word TMT_FROMHUE2 = 0x70a;
static const Word TMT_FROMHUE3 = 0x70b;
static const Word TMT_FROMHUE4 = 0x70c;
static const Word TMT_FROMHUE5 = 0x70d;
static const Word TMT_TOHUE1 = 0x70e;
static const Word TMT_TOHUE2 = 0x70f;
static const Word TMT_TOHUE3 = 0x710;
static const Word TMT_TOHUE4 = 0x711;
static const Word TMT_TOHUE5 = 0x712;
static const Word TMT_FROMCOLOR1 = 0x7d1;
static const Word TMT_FROMCOLOR2 = 0x7d2;
static const Word TMT_FROMCOLOR3 = 0x7d3;
static const Word TMT_FROMCOLOR4 = 0x7d4;
static const Word TMT_FROMCOLOR5 = 0x7d5;
static const Word TMT_TOCOLOR1 = 0x7d6;
static const Word TMT_TOCOLOR2 = 0x7d7;
static const Word TMT_TOCOLOR3 = 0x7d8;
static const Word TMT_TOCOLOR4 = 0x7d9;
static const Word TMT_TOCOLOR5 = 0x7da;
static const Word TMT_TRANSPARENT = 0x899;
static const Word TMT_AUTOSIZE = 0x89a;
static const Word TMT_BORDERONLY = 0x89b;
static const Word TMT_COMPOSITED = 0x89c;
static const Word TMT_BGFILL = 0x89d;
static const Word TMT_GLYPHTRANSPARENT = 0x89e;
static const Word TMT_GLYPHONLY = 0x89f;
static const Word TMT_ALWAYSSHOWSIZINGBAR = 0x8a0;
static const Word TMT_MIRRORIMAGE = 0x8a1;
static const Word TMT_UNIFORMSIZING = 0x8a2;
static const Word TMT_INTEGRALSIZING = 0x8a3;
static const Word TMT_SOURCEGROW = 0x8a4;
static const Word TMT_SOURCESHRINK = 0x8a5;
static const Word TMT_IMAGECOUNT = 0x961;
static const Word TMT_ALPHALEVEL = 0x962;
static const Word TMT_BORDERSIZE = 0x963;
static const Word TMT_ROUNDCORNERWIDTH = 0x964;
static const Word TMT_ROUNDCORNERHEIGHT = 0x965;
static const Word TMT_GRADIENTRATIO1 = 0x966;
static const Word TMT_GRADIENTRATIO2 = 0x967;
static const Word TMT_GRADIENTRATIO3 = 0x968;
static const Word TMT_GRADIENTRATIO4 = 0x969;
static const Word TMT_GRADIENTRATIO5 = 0x96a;
static const Word TMT_PROGRESSCHUNKSIZE = 0x96b;
static const Word TMT_PROGRESSSPACESIZE = 0x96c;
static const Word TMT_SATURATION = 0x96d;
static const Word TMT_TEXTBORDERSIZE = 0x96e;
static const Word TMT_ALPHATHRESHOLD = 0x96f;
static const Word TMT_WIDTH = 0x970;
static const Word TMT_HEIGHT = 0x971;
static const Word TMT_GLYPHINDEX = 0x972;
static const Word TMT_TRUESIZESTRETCHMARK = 0x973;
static const Word TMT_MINDPI1 = 0x974;
static const Word TMT_MINDPI2 = 0x975;
static const Word TMT_MINDPI3 = 0x976;
static const Word TMT_MINDPI4 = 0x977;
static const Word TMT_MINDPI5 = 0x978;
static const Word TMT_GLYPHFONT = 0xa29;
static const Word TMT_IMAGEFILE = 0xbb9;
static const Word TMT_IMAGEFILE1 = 0xbba;
static const Word TMT_IMAGEFILE2 = 0xbbb;
static const Word TMT_IMAGEFILE3 = 0xbbc;
static const Word TMT_IMAGEFILE4 = 0xbbd;
static const Word TMT_IMAGEFILE5 = 0xbbe;
static const Word TMT_STOCKIMAGEFILE = 0xbbf;
static const Word TMT_GLYPHIMAGEFILE = 0xbc0;
static const Word TMT_TEXT = 0xc81;
static const Word TMT_OFFSET = 0xd49;
static const Word TMT_TEXTSHADOWOFFSET = 0xd4a;
static const Word TMT_MINSIZE = 0xd4b;
static const Word TMT_MINSIZE1 = 0xd4c;
static const Word TMT_MINSIZE2 = 0xd4d;
static const Word TMT_MINSIZE3 = 0xd4e;
static const Word TMT_MINSIZE4 = 0xd4f;
static const Word TMT_MINSIZE5 = 0xd50;
static const Word TMT_NORMALSIZE = 0xd51;
static const Word TMT_SIZINGMARGINS = 0xe11;
static const Word TMT_CONTENTMARGINS = 0xe12;
static const Word TMT_CAPTIONMARGINS = 0xe13;
static const Word TMT_BORDERCOLOR = 0xed9;
static const Word TMT_FILLCOLOR = 0xeda;
static const Word TMT_TEXTCOLOR = 0xedb;
static const Word TMT_EDGELIGHTCOLOR = 0xedc;
static const Word TMT_EDGEHIGHLIGHTCOLOR = 0xedd;
static const Word TMT_EDGESHADOWCOLOR = 0xede;
static const Word TMT_EDGEDKSHADOWCOLOR = 0xedf;
static const Word TMT_EDGEFILLCOLOR = 0xee0;
static const Word TMT_TRANSPARENTCOLOR = 0xee1;
static const Word TMT_GRADIENTCOLOR1 = 0xee2;
static const Word TMT_GRADIENTCOLOR2 = 0xee3;
static const Word TMT_GRADIENTCOLOR3 = 0xee4;
static const Word TMT_GRADIENTCOLOR4 = 0xee5;
static const Word TMT_GRADIENTCOLOR5 = 0xee6;
static const Word TMT_SHADOWCOLOR = 0xee7;
static const Word TMT_GLOWCOLOR = 0xee8;
static const Word TMT_TEXTBORDERCOLOR = 0xee9;
static const Word TMT_TEXTSHADOWCOLOR = 0xeea;
static const Word TMT_GLYPHTEXTCOLOR = 0xeeb;
static const Word TMT_GLYPHTRANSPARENTCOLOR = 0xeec;
static const Word TMT_FILLCOLORHINT = 0xeed;
static const Word TMT_BORDERCOLORHINT = 0xeee;
static const Word TMT_ACCENTCOLORHINT = 0xeef;
static const Word TMT_BGTYPE = 0xfa1;
static const Word TMT_BORDERTYPE = 0xfa2;
static const Word TMT_FILLTYPE = 0xfa3;
static const Word TMT_SIZINGTYPE = 0xfa4;
static const Word TMT_HALIGN = 0xfa5;
static const Word TMT_CONTENTALIGNMENT = 0xfa6;
static const Word TMT_VALIGN = 0xfa7;
static const Word TMT_OFFSETTYPE = 0xfa8;
static const Word TMT_ICONEFFECT = 0xfa9;
static const Word TMT_TEXTSHADOWTYPE = 0xfaa;
static const Word TMT_IMAGELAYOUT = 0xfab;
static const Word TMT_GLYPHTYPE = 0xfac;
static const Word TMT_IMAGESELECTTYPE = 0xfad;
static const Word TMT_GLYPHFONTSIZINGTYPE = 0xfae;
static const Word TMT_TRUESIZESCALINGTYPE = 0xfaf;
static const Word TMT_USERPICTURE = 0x1389;
static const Word TMT_DEFAULTPANESIZE = 0x138a;
static const Word TMT_BLENDCOLOR = 0x138b;
static const Shortint WINDOWPartFiller0 = 0x0;
static const Shortint WP_CAPTION = 0x1;
static const Shortint WP_SMALLCAPTION = 0x2;
static const Shortint WP_MINCAPTION = 0x3;
static const Shortint WP_SMALLMINCAPTION = 0x4;
static const Shortint WP_MAXCAPTION = 0x5;
static const Shortint WP_SMALLMAXCAPTION = 0x6;
static const Shortint WP_FRAMELEFT = 0x7;
static const Shortint WP_FRAMERIGHT = 0x8;
static const Shortint WP_FRAMEBOTTOM = 0x9;
static const Shortint WP_SMALLFRAMELEFT = 0xa;
static const Shortint WP_SMALLFRAMERIGHT = 0xb;
static const Shortint WP_SMALLFRAMEBOTTOM = 0xc;
static const Shortint WP_SYSBUTTON = 0xd;
static const Shortint WP_MDISYSBUTTON = 0xe;
static const Shortint WP_MINBUTTON = 0xf;
static const Shortint WP_MDIMINBUTTON = 0x10;
static const Shortint WP_MAXBUTTON = 0x11;
static const Shortint WP_CLOSEBUTTON = 0x12;
static const Shortint WP_SMALLCLOSEBUTTON = 0x13;
static const Shortint WP_MDICLOSEBUTTON = 0x14;
static const Shortint WP_RESTOREBUTTON = 0x15;
static const Shortint WP_MDIRESTOREBUTTON = 0x16;
static const Shortint WP_HELPBUTTON = 0x17;
static const Shortint WP_MDIHELPBUTTON = 0x18;
static const Shortint WP_HORZSCROLL = 0x19;
static const Shortint WP_HORZTHUMB = 0x1a;
static const Shortint WP_VERTSCROLL = 0x1b;
static const Shortint WP_VERTTHUMB = 0x1c;
static const Shortint WP_DIALOG = 0x1d;
static const Shortint WP_CAPTIONSIZINGTEMPLATE = 0x1e;
static const Shortint WP_SMALLCAPTIONSIZINGTEMPLATE = 0x1f;
static const Shortint WP_FRAMELEFTSIZINGTEMPLATE = 0x20;
static const Shortint WP_SMALLFRAMELEFTSIZINGTEMPLATE = 0x21;
static const Shortint WP_FRAMERIGHTSIZINGTEMPLATE = 0x22;
static const Shortint WP_SMALLFRAMERIGHTSIZINGTEMPLATE = 0x23;
static const Shortint WP_FRAMEBOTTOMSIZINGTEMPLATE = 0x24;
static const Shortint WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE = 0x25;
static const Shortint FRAMEStateFiller0 = 0x0;
static const Shortint FS_ACTIVE = 0x1;
static const Shortint FS_INACTIVE = 0x2;
static const Shortint CAPTIONStateFiller0 = 0x0;
static const Shortint CS_ACTIVE = 0x1;
static const Shortint CS_INACTIVE = 0x2;
static const Shortint CS_DISABLED = 0x3;
static const Shortint MAXCAPTIONStateFiller0 = 0x0;
static const Shortint MXCS_ACTIVE = 0x1;
static const Shortint MXCS_INACTIVE = 0x2;
static const Shortint MXCS_DISABLED = 0x3;
static const Shortint MINCAPTIONStateFiller0 = 0x0;
static const Shortint MNCS_ACTIVE = 0x1;
static const Shortint MNCS_INACTIVE = 0x2;
static const Shortint MNCS_DISABLED = 0x3;
static const Shortint HORZSCROLLStateFiller0 = 0x0;
static const Shortint HSS_NORMAL = 0x1;
static const Shortint HSS_HOT = 0x2;
static const Shortint HSS_PUSHED = 0x3;
static const Shortint HSS_DISABLED = 0x4;
static const Shortint HORZTHUMBStateFiller0 = 0x0;
static const Shortint HTS_NORMAL = 0x1;
static const Shortint HTS_HOT = 0x2;
static const Shortint HTS_PUSHED = 0x3;
static const Shortint HTS_DISABLED = 0x4;
static const Shortint VERTSCROLLStateFiller0 = 0x0;
static const Shortint VSS_NORMAL = 0x1;
static const Shortint VSS_HOT = 0x2;
static const Shortint VSS_PUSHED = 0x3;
static const Shortint VSS_DISABLED = 0x4;
static const Shortint VERTTHUMBStateFiller0 = 0x0;
static const Shortint VTS_NORMAL = 0x1;
static const Shortint VTS_HOT = 0x2;
static const Shortint VTS_PUSHED = 0x3;
static const Shortint VTS_DISABLED = 0x4;
static const Shortint SYSBUTTONStateFiller0 = 0x0;
static const Shortint SBS_NORMAL = 0x1;
static const Shortint SBS_HOT = 0x2;
static const Shortint SBS_PUSHED = 0x3;
static const Shortint SBS_DISABLED = 0x4;
static const Shortint MINBUTTONStateFiller0 = 0x0;
static const Shortint MINBS_NORMAL = 0x1;
static const Shortint MINBS_HOT = 0x2;
static const Shortint MINBS_PUSHED = 0x3;
static const Shortint MINBS_DISABLED = 0x4;
static const Shortint MINBS_INACTIVE = 0x5;
static const Shortint MAXBUTTONStateFiller0 = 0x0;
static const Shortint MAXBS_NORMAL = 0x1;
static const Shortint MAXBS_HOT = 0x2;
static const Shortint MAXBS_PUSHED = 0x3;
static const Shortint MAXBS_DISABLED = 0x4;
static const Shortint MAXBS_INACTIVE = 0x5;
static const Shortint RESTOREBUTTONStateFiller0 = 0x0;
static const Shortint RBS_NORMAL = 0x1;
static const Shortint RBS_HOT = 0x2;
static const Shortint RBS_PUSHED = 0x3;
static const Shortint RBS_DISABLED = 0x4;
static const Shortint RBS_INACTIVE = 0x5;
static const Shortint HELPBUTTONStateFiller0 = 0x0;
static const Shortint HBS_NORMAL = 0x1;
static const Shortint HBS_HOT = 0x2;
static const Shortint HBS_PUSHED = 0x3;
static const Shortint HBS_DISABLED = 0x4;
static const Shortint HBS_INACTIVE = 0x5;
static const Shortint CLOSEBUTTONStateFiller0 = 0x0;
static const Shortint CBS_NORMAL = 0x1;
static const Shortint CBS_HOT = 0x2;
static const Shortint CBS_PUSHED = 0x3;
static const Shortint CBS_DISABLED = 0x4;
static const Shortint CBS_INACTIVE = 0x5;
static const Shortint BUTTONPartFiller0 = 0x0;
static const Shortint BP_PUSHBUTTON = 0x1;
static const Shortint BP_RADIOBUTTON = 0x2;
static const Shortint BP_CHECKBOX = 0x3;
static const Shortint BP_GROUPBOX = 0x4;
static const Shortint BP_USERBUTTON = 0x5;
static const Shortint PUSHBUTTONStateFiller0 = 0x0;
static const Shortint PBS_NORMAL = 0x1;
static const Shortint PBS_HOT = 0x2;
static const Shortint PBS_PRESSED = 0x3;
static const Shortint PBS_DISABLED = 0x4;
static const Shortint PBS_DEFAULTED = 0x5;
static const Shortint RADIOBUTTONStateFiller0 = 0x0;
static const Shortint RBS_UNCHECKEDNORMAL = 0x1;
static const Shortint RBS_UNCHECKEDHOT = 0x2;
static const Shortint RBS_UNCHECKEDPRESSED = 0x3;
static const Shortint RBS_UNCHECKEDDISABLED = 0x4;
static const Shortint RBS_CHECKEDNORMAL = 0x5;
static const Shortint RBS_CHECKEDHOT = 0x6;
static const Shortint RBS_CHECKEDPRESSED = 0x7;
static const Shortint RBS_CHECKEDDISABLED = 0x8;
static const Shortint CHECKBOXStateFiller0 = 0x0;
static const Shortint CBS_UNCHECKEDNORMAL = 0x1;
static const Shortint CBS_UNCHECKEDHOT = 0x2;
static const Shortint CBS_UNCHECKEDPRESSED = 0x3;
static const Shortint CBS_UNCHECKEDDISABLED = 0x4;
static const Shortint CBS_CHECKEDNORMAL = 0x5;
static const Shortint CBS_CHECKEDHOT = 0x6;
static const Shortint CBS_CHECKEDPRESSED = 0x7;
static const Shortint CBS_CHECKEDDISABLED = 0x8;
static const Shortint CBS_MIXEDNORMAL = 0x9;
static const Shortint CBS_MIXEDHOT = 0xa;
static const Shortint CBS_MIXEDPRESSED = 0xb;
static const Shortint CBS_MIXEDDISABLED = 0xc;
static const Shortint GROUPBOXStateFiller0 = 0x0;
static const Shortint GBS_NORMAL = 0x1;
static const Shortint GBS_DISABLED = 0x2;
static const Shortint REBARPartFiller0 = 0x0;
static const Shortint RP_GRIPPER = 0x1;
static const Shortint RP_GRIPPERVERT = 0x2;
static const Shortint RP_BAND = 0x3;
static const Shortint RP_CHEVRON = 0x4;
static const Shortint RP_CHEVRONVERT = 0x5;
static const Shortint CHEVRONStateFiller0 = 0x0;
static const Shortint CHEVS_NORMAL = 0x1;
static const Shortint CHEVS_HOT = 0x2;
static const Shortint CHEVS_PRESSED = 0x3;
static const Shortint TOOLBARPartFiller0 = 0x0;
static const Shortint TP_BUTTON = 0x1;
static const Shortint TP_DROPDOWNBUTTON = 0x2;
static const Shortint TP_SPLITBUTTON = 0x3;
static const Shortint TP_SPLITBUTTONDROPDOWN = 0x4;
static const Shortint TP_SEPARATOR = 0x5;
static const Shortint TP_SEPARATORVERT = 0x6;
static const Shortint TOOLBARStateFiller0 = 0x0;
static const Shortint TS_NORMAL = 0x1;
static const Shortint TS_HOT = 0x2;
static const Shortint TS_PRESSED = 0x3;
static const Shortint TS_DISABLED = 0x4;
static const Shortint TS_CHECKED = 0x5;
static const Shortint TS_HOTCHECKED = 0x6;
static const Shortint STATUSPartFiller0 = 0x0;
static const Shortint SP_PANE = 0x1;
static const Shortint SP_GRIPPERPANE = 0x2;
static const Shortint SP_GRIPPER = 0x3;
static const Shortint MENUPartFiller0 = 0x0;
static const Shortint MP_MENUITEM = 0x1;
static const Shortint MP_MENUDROPDOWN = 0x2;
static const Shortint MP_MENUBARITEM = 0x3;
static const Shortint MP_MENUBARDROPDOWN = 0x4;
static const Shortint MP_CHEVRON = 0x5;
static const Shortint MP_SEPARATOR = 0x6;
static const Shortint MENUStateFiller0 = 0x0;
static const Shortint MS_NORMAL = 0x1;
static const Shortint MS_SELECTED = 0x2;
static const Shortint MS_DEMOTED = 0x3;
static const Shortint LISTVIEWPartFiller0 = 0x0;
static const Shortint LVP_LISTITEM = 0x1;
static const Shortint LVP_LISTGROUP = 0x2;
static const Shortint LVP_LISTDETAIL = 0x3;
static const Shortint LVP_LISTSORTEDDETAIL = 0x4;
static const Shortint LVP_EMPTYTEXT = 0x5;
static const Shortint LISTITEMStateFiller0 = 0x0;
static const Shortint LIS_NORMAL = 0x1;
static const Shortint LIS_HOT = 0x2;
static const Shortint LIS_SELECTED = 0x3;
static const Shortint LIS_DISABLED = 0x4;
static const Shortint LIS_SELECTEDNOTFOCUS = 0x5;
static const Shortint HEADERPartFiller0 = 0x0;
static const Shortint HP_HEADERITEM = 0x1;
static const Shortint HP_HEADERITEMLEFT = 0x2;
static const Shortint HP_HEADERITEMRIGHT = 0x3;
static const Shortint HP_HEADERSORTARROW = 0x4;
static const Shortint HEADERITEMStateFiller0 = 0x0;
static const Shortint HIS_NORMAL = 0x1;
static const Shortint HIS_HOT = 0x2;
static const Shortint HIS_PRESSED = 0x3;
static const Shortint HEADERITEMLEFTStateFiller0 = 0x0;
static const Shortint HILS_NORMAL = 0x1;
static const Shortint HILS_HOT = 0x2;
static const Shortint HILS_PRESSED = 0x3;
static const Shortint HEADERITEMRIGHTStateFiller0 = 0x0;
static const Shortint HIRS_NORMAL = 0x1;
static const Shortint HIRS_HOT = 0x2;
static const Shortint HIRS_PRESSED = 0x3;
static const Shortint HEADERSORTARROWStateFiller0 = 0x0;
static const Shortint HSAS_SORTEDUP = 0x1;
static const Shortint HSAS_SORTEDDOWN = 0x2;
static const Shortint PROGRESSPartFiller0 = 0x0;
static const Shortint PP_BAR = 0x1;
static const Shortint PP_BARVERT = 0x2;
static const Shortint PP_CHUNK = 0x3;
static const Shortint PP_CHUNKVERT = 0x4;
static const Shortint TABPartFiller0 = 0x0;
static const Shortint TABP_TABITEM = 0x1;
static const Shortint TABP_TABITEMLEFTEDGE = 0x2;
static const Shortint TABP_TABITEMRIGHTEDGE = 0x3;
static const Shortint TABP_TABITEMBOTHEDGE = 0x4;
static const Shortint TABP_TOPTABITEM = 0x5;
static const Shortint TABP_TOPTABITEMLEFTEDGE = 0x6;
static const Shortint TABP_TOPTABITEMRIGHTEDGE = 0x7;
static const Shortint TABP_TOPTABITEMBOTHEDGE = 0x8;
static const Shortint TABP_PANE = 0x9;
static const Shortint TABP_BODY = 0xa;
static const Shortint TABITEMStateFiller0 = 0x0;
static const Shortint TIS_NORMAL = 0x1;
static const Shortint TIS_HOT = 0x2;
static const Shortint TIS_SELECTED = 0x3;
static const Shortint TIS_DISABLED = 0x4;
static const Shortint TIS_FOCUSED = 0x5;
static const Shortint TABITEMLEFTEDGEStateFiller0 = 0x0;
static const Shortint TILES_NORMAL = 0x1;
static const Shortint TILES_HOT = 0x2;
static const Shortint TILES_SELECTED = 0x3;
static const Shortint TILES_DISABLED = 0x4;
static const Shortint TILES_FOCUSED = 0x5;
static const Shortint TABITEMRIGHTEDGEStateFiller0 = 0x0;
static const Shortint TIRES_NORMAL = 0x1;
static const Shortint TIRES_HOT = 0x2;
static const Shortint TIRES_SELECTED = 0x3;
static const Shortint TIRES_DISABLED = 0x4;
static const Shortint TIRES_FOCUSED = 0x5;
static const Shortint TABITEMBOTHEDGESStateFiller0 = 0x0;
static const Shortint TIBES_NORMAL = 0x1;
static const Shortint TIBES_HOT = 0x2;
static const Shortint TIBES_SELECTED = 0x3;
static const Shortint TIBES_DISABLED = 0x4;
static const Shortint TIBES_FOCUSED = 0x5;
static const Shortint TOPTABITEMStateFiller0 = 0x0;
static const Shortint TTIS_NORMAL = 0x1;
static const Shortint TTIS_HOT = 0x2;
static const Shortint TTIS_SELECTED = 0x3;
static const Shortint TTIS_DISABLED = 0x4;
static const Shortint TTIS_FOCUSED = 0x5;
static const Shortint TOPTABITEMLEFTEDGEStateFiller0 = 0x0;
static const Shortint TTILES_NORMAL = 0x1;
static const Shortint TTILES_HOT = 0x2;
static const Shortint TTILES_SELECTED = 0x3;
static const Shortint TTILES_DISABLED = 0x4;
static const Shortint TTILES_FOCUSED = 0x5;
static const Shortint TOPTABITEMRIGHTEDGEStateFiller0 = 0x0;
static const Shortint TTIRES_NORMAL = 0x1;
static const Shortint TTIRES_HOT = 0x2;
static const Shortint TTIRES_SELECTED = 0x3;
static const Shortint TTIRES_DISABLED = 0x4;
static const Shortint TTIRES_FOCUSED = 0x5;
static const Shortint TOPTABITEMBOTHEDGESStateFiller0 = 0x0;
static const Shortint TTIBES_NORMAL = 0x1;
static const Shortint TTIBES_HOT = 0x2;
static const Shortint TTIBES_SELECTED = 0x3;
static const Shortint TTIBES_DISABLED = 0x4;
static const Shortint TTIBES_FOCUSED = 0x5;
static const Shortint TRACKBARPartFiller0 = 0x0;
static const Shortint TKP_TRACK = 0x1;
static const Shortint TKP_TRACKVERT = 0x2;
static const Shortint TKP_THUMB = 0x3;
static const Shortint TKP_THUMBBOTTOM = 0x4;
static const Shortint TKP_THUMBTOP = 0x5;
static const Shortint TKP_THUMBVERT = 0x6;
static const Shortint TKP_THUMBLEFT = 0x7;
static const Shortint TKP_THUMBRIGHT = 0x8;
static const Shortint TKP_TICS = 0x9;
static const Shortint TKP_TICSVERT = 0xa;
static const Shortint TRACKBARStateFiller0 = 0x0;
static const Shortint TKS_NORMAL = 0x1;
static const Shortint TRACKStateFiller0 = 0x0;
static const Shortint TRS_NORMAL = 0x1;
static const Shortint TRACKVERTStateFiller0 = 0x0;
static const Shortint TRVS_NORMAL = 0x1;
static const Shortint THUMBStateFiller0 = 0x0;
static const Shortint TUS_NORMAL = 0x1;
static const Shortint TUS_HOT = 0x2;
static const Shortint TUS_PRESSED = 0x3;
static const Shortint TUS_FOCUSED = 0x4;
static const Shortint TUS_DISABLED = 0x5;
static const Shortint THUMBBOTTOMStateFiller0 = 0x0;
static const Shortint TUBS_NORMAL = 0x1;
static const Shortint TUBS_HOT = 0x2;
static const Shortint TUBS_PRESSED = 0x3;
static const Shortint TUBS_FOCUSED = 0x4;
static const Shortint TUBS_DISABLED = 0x5;
static const Shortint THUMBTOPStateFiller0 = 0x0;
static const Shortint TUTS_NORMAL = 0x1;
static const Shortint TUTS_HOT = 0x2;
static const Shortint TUTS_PRESSED = 0x3;
static const Shortint TUTS_FOCUSED = 0x4;
static const Shortint TUTS_DISABLED = 0x5;
static const Shortint THUMBVERTStateFiller0 = 0x0;
static const Shortint TUVS_NORMAL = 0x1;
static const Shortint TUVS_HOT = 0x2;
static const Shortint TUVS_PRESSED = 0x3;
static const Shortint TUVS_FOCUSED = 0x4;
static const Shortint TUVS_DISABLED = 0x5;
static const Shortint THUMBLEFTStateFiller0 = 0x0;
static const Shortint TUVLS_NORMAL = 0x1;
static const Shortint TUVLS_HOT = 0x2;
static const Shortint TUVLS_PRESSED = 0x3;
static const Shortint TUVLS_FOCUSED = 0x4;
static const Shortint TUVLS_DISABLED = 0x5;
static const Shortint THUMBRIGHTStateFiller0 = 0x0;
static const Shortint TUVRS_NORMAL = 0x1;
static const Shortint TUVRS_HOT = 0x2;
static const Shortint TUVRS_PRESSED = 0x3;
static const Shortint TUVRS_FOCUSED = 0x4;
static const Shortint TUVRS_DISABLED = 0x5;
static const Shortint TICSStateFiller0 = 0x0;
static const Shortint TSS_NORMAL = 0x1;
static const Shortint TICSVERTStateFiller0 = 0x0;
static const Shortint TSVS_NORMAL = 0x1;
static const Shortint TOOLTIPPartFiller0 = 0x0;
static const Shortint TTP_STANDARD = 0x1;
static const Shortint TTP_STANDARDTITLE = 0x2;
static const Shortint TTP_BALLOON = 0x3;
static const Shortint TTP_BALLOONTITLE = 0x4;
static const Shortint TTP_CLOSE = 0x5;
static const Shortint CLOSEStateFiller0 = 0x0;
static const Shortint TTCS_NORMAL = 0x1;
static const Shortint TTCS_HOT = 0x2;
static const Shortint TTCS_PRESSED = 0x3;
static const Shortint STANDARDStateFiller0 = 0x0;
static const Shortint TTSS_NORMAL = 0x1;
static const Shortint TTSS_LINK = 0x2;
static const Shortint BALLOONStateFiller0 = 0x0;
static const Shortint TTBS_NORMAL = 0x1;
static const Shortint TTBS_LINK = 0x2;
static const Shortint TREEVIEWPartFiller0 = 0x0;
static const Shortint TVP_TREEITEM = 0x1;
static const Shortint TVP_GLYPH = 0x2;
static const Shortint TVP_BRANCH = 0x3;
static const Shortint TREEITEMStateFiller0 = 0x0;
static const Shortint TREIS_NORMAL = 0x1;
static const Shortint TREIS_HOT = 0x2;
static const Shortint TREIS_SELECTED = 0x3;
static const Shortint TREIS_DISABLED = 0x4;
static const Shortint TREIS_SELECTEDNOTFOCUS = 0x5;
static const Shortint GLYPHStateFiller0 = 0x0;
static const Shortint GLPS_CLOSED = 0x1;
static const Shortint GLPS_OPENED = 0x2;
static const Shortint SPINPartFiller0 = 0x0;
static const Shortint SPNP_UP = 0x1;
static const Shortint SPNP_DOWN = 0x2;
static const Shortint SPNP_UPHORZ = 0x3;
static const Shortint SPNP_DOWNHORZ = 0x4;
static const Shortint UPStateFiller0 = 0x0;
static const Shortint UPS_NORMAL = 0x1;
static const Shortint UPS_HOT = 0x2;
static const Shortint UPS_PRESSED = 0x3;
static const Shortint UPS_DISABLED = 0x4;
static const Shortint DOWNStateFiller0 = 0x0;
static const Shortint DNS_NORMAL = 0x1;
static const Shortint DNS_HOT = 0x2;
static const Shortint DNS_PRESSED = 0x3;
static const Shortint DNS_DISABLED = 0x4;
static const Shortint UPHORZStateFiller0 = 0x0;
static const Shortint UPHZS_NORMAL = 0x1;
static const Shortint UPHZS_HOT = 0x2;
static const Shortint UPHZS_PRESSED = 0x3;
static const Shortint UPHZS_DISABLED = 0x4;
static const Shortint DOWNHORZStateFiller0 = 0x0;
static const Shortint DNHZS_NORMAL = 0x1;
static const Shortint DNHZS_HOT = 0x2;
static const Shortint DNHZS_PRESSED = 0x3;
static const Shortint DNHZS_DISABLED = 0x4;
static const Shortint PAGEPartFiller0 = 0x0;
static const Shortint PGRP_UP = 0x1;
static const Shortint PGRP_DOWN = 0x2;
static const Shortint PGRP_UPHORZ = 0x3;
static const Shortint PGRP_DOWNHORZ = 0x4;
static const Shortint SCROLLBARPartFiller0 = 0x0;
static const Shortint SBP_ARROWBTN = 0x1;
static const Shortint SBP_THUMBBTNHORZ = 0x2;
static const Shortint SBP_THUMBBTNVERT = 0x3;
static const Shortint SBP_LOWERTRACKHORZ = 0x4;
static const Shortint SBP_UPPERTRACKHORZ = 0x5;
static const Shortint SBP_LOWERTRACKVERT = 0x6;
static const Shortint SBP_UPPERTRACKVERT = 0x7;
static const Shortint SBP_GRIPPERHORZ = 0x8;
static const Shortint SBP_GRIPPERVERT = 0x9;
static const Shortint SBP_SIZEBOX = 0xa;
static const Shortint ARROWBTNStateFiller0 = 0x0;
static const Shortint ABS_UPNORMAL = 0x1;
static const Shortint ABS_UPHOT = 0x2;
static const Shortint ABS_UPPRESSED = 0x3;
static const Shortint ABS_UPDISABLED = 0x4;
static const Shortint ABS_DOWNNORMAL = 0x5;
static const Shortint ABS_DOWNHOT = 0x6;
static const Shortint ABS_DOWNPRESSED = 0x7;
static const Shortint ABS_DOWNDISABLED = 0x8;
static const Shortint ABS_LEFTNORMAL = 0x9;
static const Shortint ABS_LEFTHOT = 0xa;
static const Shortint ABS_LEFTPRESSED = 0xb;
static const Shortint ABS_LEFTDISABLED = 0xc;
static const Shortint ABS_RIGHTNORMAL = 0xd;
static const Shortint ABS_RIGHTHOT = 0xe;
static const Shortint ABS_RIGHTPRESSED = 0xf;
static const Shortint ABS_RIGHTDISABLED = 0x10;
static const Shortint SCROLLBARStateFiller0 = 0x0;
static const Shortint SCRBS_NORMAL = 0x1;
static const Shortint SCRBS_HOT = 0x2;
static const Shortint SCRBS_PRESSED = 0x3;
static const Shortint SCRBS_DISABLED = 0x4;
static const Shortint SIZEBOXStateFiller0 = 0x0;
static const Shortint SZB_RIGHTALIGN = 0x1;
static const Shortint SZB_LEFTALIGN = 0x2;
static const Shortint EDITPartFiller0 = 0x0;
static const Shortint EP_EDITTEXT = 0x1;
static const Shortint EP_CARET = 0x2;
static const Shortint EDITTEXTStateFiller0 = 0x0;
static const Shortint ETS_NORMAL = 0x1;
static const Shortint ETS_HOT = 0x2;
static const Shortint ETS_SELECTED = 0x3;
static const Shortint ETS_DISABLED = 0x4;
static const Shortint ETS_FOCUSED = 0x5;
static const Shortint ETS_READONLY = 0x6;
static const Shortint ETS_ASSIST = 0x7;
static const Shortint COMBOBOXPartFiller0 = 0x0;
static const Shortint CP_DROPDOWNBUTTON = 0x1;
static const Shortint COMBOBOXStateFiller0 = 0x0;
static const Shortint CBXS_NORMAL = 0x1;
static const Shortint CBXS_HOT = 0x2;
static const Shortint CBXS_PRESSED = 0x3;
static const Shortint CBXS_DISABLED = 0x4;
static const Shortint CLOCKPartFiller0 = 0x0;
static const Shortint CLP_TIME = 0x1;
static const Shortint CLOCKStateFiller0 = 0x0;
static const Shortint CLS_NORMAL = 0x1;
static const Shortint TRAYNOTIFYPartFiller0 = 0x0;
static const Shortint TNP_BACKGROUND = 0x1;
static const Shortint TNP_ANIMBACKGROUND = 0x2;
static const Shortint TASKBARPartFiller0 = 0x0;
static const Shortint TBP_BACKGROUNDBOTTOM = 0x1;
static const Shortint TBP_BACKGROUNDRIGHT = 0x2;
static const Shortint TBP_BACKGROUNDTOP = 0x3;
static const Shortint TBP_BACKGROUNDLEFT = 0x4;
static const Shortint TBP_SIZINGBARBOTTOM = 0x5;
static const Shortint TBP_SIZINGBARRIGHT = 0x6;
static const Shortint TBP_SIZINGBARTOP = 0x7;
static const Shortint TBP_SIZINGBARLEFT = 0x8;
static const Shortint TASKBANDPartFiller0 = 0x0;
static const Shortint TDP_GROUPCOUNT = 0x1;
static const Shortint TDP_FLASHBUTTON = 0x2;
static const Shortint TDP_FLASHBUTTONGROUPMENU = 0x3;
static const Shortint STARTPANELPartFiller0 = 0x0;
static const Shortint SPP_USERPANE = 0x1;
static const Shortint SPP_MOREPROGRAMS = 0x2;
static const Shortint SPP_MOREPROGRAMSARROW = 0x3;
static const Shortint SPP_PROGLIST = 0x4;
static const Shortint SPP_PROGLISTSEPARATOR = 0x5;
static const Shortint SPP_PLACESLIST = 0x6;
static const Shortint SPP_PLACESLISTSEPARATOR = 0x7;
static const Shortint SPP_LOGOFF = 0x8;
static const Shortint SPP_LOGOFFBUTTONS = 0x9;
static const Shortint SPP_USERPICTURE = 0xa;
static const Shortint SPP_PREVIEW = 0xb;
static const Shortint MOREPROGRAMSARROWStateFiller0 = 0x0;
static const Shortint SPS_NORMAL = 0x1;
static const Shortint SPS_HOT = 0x2;
static const Shortint SPS_PRESSED = 0x3;
static const Shortint LOGOFFBUTTONSStateFiller0 = 0x0;
static const Shortint SPLS_NORMAL = 0x1;
static const Shortint SPLS_HOT = 0x2;
static const Shortint SPLS_PRESSED = 0x3;
static const Shortint EXPLORERBARPartFiller0 = 0x0;
static const Shortint EBP_HEADERBACKGROUND = 0x1;
static const Shortint EBP_HEADERCLOSE = 0x2;
static const Shortint EBP_HEADERPIN = 0x3;
static const Shortint EBP_IEBARMENU = 0x4;
static const Shortint EBP_NORMALGROUPBACKGROUND = 0x5;
static const Shortint EBP_NORMALGROUPCOLLAPSE = 0x6;
static const Shortint EBP_NORMALGROUPEXPAND = 0x7;
static const Shortint EBP_NORMALGROUPHEAD = 0x8;
static const Shortint EBP_SPECIALGROUPBACKGROUND = 0x9;
static const Shortint EBP_SPECIALGROUPCOLLAPSE = 0xa;
static const Shortint EBP_SPECIALGROUPEXPAND = 0xb;
static const Shortint EBP_SPECIALGROUPHEAD = 0xc;
static const Shortint HEADERCLOSEStateFiller0 = 0x0;
static const Shortint EBHC_NORMAL = 0x1;
static const Shortint EBHC_HOT = 0x2;
static const Shortint EBHC_PRESSED = 0x3;
static const Shortint HEADERPINStateFiller0 = 0x0;
static const Shortint EBHP_NORMAL = 0x1;
static const Shortint EBHP_HOT = 0x2;
static const Shortint EBHP_PRESSED = 0x3;
static const Shortint EBHP_SELECTEDNORMAL = 0x4;
static const Shortint EBHP_SELECTEDHOT = 0x5;
static const Shortint EBHP_SELECTEDPRESSED = 0x6;
static const Shortint IEBARMENUStateFiller0 = 0x0;
static const Shortint EBM_NORMAL = 0x1;
static const Shortint EBM_HOT = 0x2;
static const Shortint EBM_PRESSED = 0x3;
static const Shortint NORMALGROUPCOLLAPSEStateFiller0 = 0x0;
static const Shortint EBNGC_NORMAL = 0x1;
static const Shortint EBNGC_HOT = 0x2;
static const Shortint EBNGC_PRESSED = 0x3;
static const Shortint NORMALGROUPEXPANDStateFiller0 = 0x0;
static const Shortint EBNGE_NORMAL = 0x1;
static const Shortint EBNGE_HOT = 0x2;
static const Shortint EBNGE_PRESSED = 0x3;
static const Shortint SPECIALGROUPCOLLAPSEStateFiller0 = 0x0;
static const Shortint EBSGC_NORMAL = 0x1;
static const Shortint EBSGC_HOT = 0x2;
static const Shortint EBSGC_PRESSED = 0x3;
static const Shortint SPECIALGROUPEXPANDStateFiller0 = 0x0;
static const Shortint EBSGE_NORMAL = 0x1;
static const Shortint EBSGE_HOT = 0x2;
static const Shortint EBSGE_PRESSED = 0x3;
static const Shortint MENUBANDPartFiller0 = 0x0;
static const Shortint MDP_NEWAPPBUTTON = 0x1;
static const Shortint MDP_SEPERATOR = 0x2;
static const Shortint MENUBANDStateFiller0 = 0x0;
static const Shortint MDS_NORMAL = 0x1;
static const Shortint MDS_HOT = 0x2;
static const Shortint MDS_PRESSED = 0x3;
static const Shortint MDS_DISABLED = 0x4;
static const Shortint MDS_CHECKED = 0x5;
static const Shortint MDS_HOTCHECKED = 0x6;

}	/* namespace Tmschema */
using namespace Tmschema;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TmSchema
