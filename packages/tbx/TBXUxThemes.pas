unit TBXUxThemes;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Conversion of original visual styles API tmschema.h and
// uxtheme.h (Copyright Microsoft Corporation 1991-2000)
//
// $Id: TBXUxThemes.pas 7 2004-02-21 06:07:53Z  $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows;

function InitXPThemes: Boolean;
procedure FreeXPThemes;
function CanUseXPThemes: Boolean;

{$DEFINE TMSCHEMA_H}

const
  THEMEMGR_VERSION                     = 1;
  WM_THEMECHANGED                      = $031A;

{ Enumerators }

const
  { BGTYPE }
  BT_IMAGEFILE                         = 0;
  BT_BORDERFILL                        = 1;
  BT_NONE                              = 2;

  { IMAGELAYOUT }
  IL_VERTICAL                          = 0;
  IL_HORIZONTAL                        = 1;

  { BORDERTYPE }
  BT_RECT                              = 0;
  BT_ROUNDRECT                         = 1;
  BT_ELLIPSE                           = 2;

  { FILLTYPE }
  FT_SOLID                             = 0;
  FT_VERTGRADIENT                      = 1;
  FT_HORZGRADIENT                      = 2;
  FT_RADIALGRADIENT                    = 3;
  FT_TILEIMAGE                         = 4;

  { SIZINGTYPE }
  ST_TRUESIZE                          = 0;
  ST_STRETCH                           = 1;
  ST_TILE                              = 2;

  { HALIGN }
  HA_LEFT                              = 0;
  HA_CENTER                            = 1;
  HA_RIGHT                             = 2;

  { CONTENTALIGNMENT }
  CA_LEFT                              = 0;
  CA_CENTER                            = 1;
  CA_RIGHT                             = 2;

  { VALIGN }
  VA_TOP                               = 0;
  VA_CENTER                            = 1;
  VA_BOTTOM                            = 2;

  { OFFSETTYPE }
  OT_TOPLEFT                           = 0;
  OT_TOPRIGHT                          = 1;
  OT_TOPMIDDLE                         = 2;
  OT_BOTTOMLEFT                        = 3;
  OT_BOTTOMRIGHT                       = 4;
  OT_BOTTOMMIDDLE                      = 5;
  OT_MIDDLELEFT                        = 6;
  OT_MIDDLERIGHT                       = 7;
  OT_LEFTOFCAPTION                     = 8;
  OT_RIGHTOFCAPTION                    = 9;
  OT_LEFTOFLASTBUTTON                  = 10;
  OT_RIGHTOFLASTBUTTON                 = 11;
  OT_ABOVELASTBUTTON                   = 12;
  OT_BELOWLASTBUTTON                   = 13;

  { ICONEFFECT }
  ICE_NONE                             = 0;
  ICE_GLOW                             = 1;
  ICE_SHADOW                           = 2;
  ICE_PULSE                            = 3;
  ICE_ALPHA                            = 4;

  { TEXTSHADOWTYPE }
  TST_NONE                             = 0;
  TST_SINGLE                           = 1;
  TST_CONTINUOUS                       = 2;

  { GLYPHTYPE }
  GT_NONE                              = 0;
  GT_IMAGEGLYPH                        = 1;
  GT_FONTGLYPH                         = 2;

  { IMAGESELECTTYPE }
  IST_NONE                             = 0;
  IST_SIZE                             = 1;
  IST_DPI                              = 2;

  { TRUESIZESCALINGTYPE }
  TSST_NONE                            = 0;
  TSST_SIZE                            = 1;
  TSST_DPI                             = 2;

 { GLYPHFONTSIZINGTYPE }
  GFST_NONE                            = 0;
  GFST_SIZE                            = 1;
  GFST_DPI                             = 2;



{ PROPERTIES - used by uxtheme rendering and controls }

const
  { primitive types }
  TMT_STRING                           = 201;
  TMT_INT                              = 202;
  TMT_BOOL                             = 203;
  TMT_COLOR                            = 204;
  TMT_MARGINS                          = 205;
  TMT_FILENAME                         = 206;
  TMT_SIZE                             = 207;
  TMT_POSITION                         = 208;
  TMT_RECT                             = 209;
  TMT_FONT                             = 210;
  TMT_INTLIST                          = 211;

  { special misc. properties }
  TMT_COLORSCHEMES                     = 401;
  TMT_SIZES                            = 402;
  TMT_CHARSET                          = 403;

  { [documentation] properties }
  TMT_DISPLAYNAME                      = 601;
  TMT_TOOLTIP                          = 602;
  TMT_COMPANY                          = 603;
  TMT_AUTHOR                           = 604;
  TMT_COPYRIGHT                        = 605;
  TMT_URL                              = 606;
  TMT_VERSION                          = 607;
  TMT_DESCRIPTION                      = 608;

  { theme metrics: fonts }
  TMT_CAPTIONFONT                      = 801;
  TMT_SMALLCAPTIONFONT                 = 802;
  TMT_MENUFONT                         = 803;
  TMT_STATUSFONT                       = 804;
  TMT_MSGBOXFONT                       = 805;
  TMT_ICONTITLEFONT                    = 806;

  { theme metrics: bools }
  TMT_FLATMENUS                        = 1001;

  { theme metrics: sizes }
  TMT_SIZINGBORDERWIDTH                = 1201;
  TMT_SCROLLBARWIDTH                   = 1202;
  TMT_SCROLLBARHEIGHT                  = 1203;
  TMT_CAPTIONBARWIDTH                  = 1204;
  TMT_CAPTIONBARHEIGHT                 = 1205;
  TMT_SMCAPTIONBARWIDTH                = 1206;
  TMT_SMCAPTIONBARHEIGHT               = 1207;
  TMT_MENUBARWIDTH                     = 1208;
  TMT_MENUBARHEIGHT                    = 1209;

  { theme metrics: ints }
  TMT_MINCOLORDEPTH                    = 1301;


  { theme metrics: strings }
  TMT_CSSNAME                          = 1401;
  TMT_XMLNAME                          = 1402;

  { theme metrics: colors }
  TMT_SCROLLBAR                        = 1601;
  TMT_BACKGROUND                       = 1602;
  TMT_ACTIVECAPTION                    = 1603;
  TMT_INACTIVECAPTION                  = 1604;
  TMT_MENU                             = 1605;
  TMT_WINDOW                           = 1606;
  TMT_WINDOWFRAME                      = 1607;
  TMT_MENUTEXT                         = 1608;
  TMT_WINDOWTEXT                       = 1609;
  TMT_CAPTIONTEXT                      = 1610;
  TMT_ACTIVEBORDER                     = 1611;
  TMT_INACTIVEBORDER                   = 1612;
  TMT_APPWORKSPACE                     = 1613;
  TMT_HIGHLIGHT                        = 1614;
  TMT_HIGHLIGHTTEXT                    = 1615;
  TMT_BTNFACE                          = 1616;
  TMT_BTNSHADOW                        = 1617;
  TMT_GRAYTEXT                         = 1618;
  TMT_BTNTEXT                          = 1619;
  TMT_INACTIVECAPTIONTEXT                = 1620;
  TMT_BTNHIGHLIGHT                     = 1621;
  TMT_DKSHADOW3D                       = 1622;
  TMT_LIGHT3D                          = 1623;
  TMT_INFOTEXT                         = 1624;
  TMT_INFOBK                           = 1625;
  TMT_BUTTONALTERNATEFACE              = 1626;
  TMT_HOTTRACKING                      = 1627;
  TMT_GRADIENTACTIVECAPTION            = 1628;
  TMT_GRADIENTINACTIVECAPTION          = 1629;
  TMT_MENUHILIGHT                      = 1630;
  TMT_MENUBAR                          = 1631;

  { hue substitutions }
  TMT_FROMHUE1                         = 1801;
  TMT_FROMHUE2                         = 1802;
  TMT_FROMHUE3                         = 1803;
  TMT_FROMHUE4                         = 1804;
  TMT_FROMHUE5                         = 1805;
  TMT_TOHUE1                           = 1806;
  TMT_TOHUE2                           = 1807;
  TMT_TOHUE3                           = 1808;
  TMT_TOHUE4                           = 1809;
  TMT_TOHUE5                           = 1810;

  { color substitutions }
  TMT_FROMCOLOR1                       = 2001;
  TMT_FROMCOLOR2                       = 2002;
  TMT_FROMCOLOR3                       = 2003;
  TMT_FROMCOLOR4                       = 2004;
  TMT_FROMCOLOR5                       = 2005;
  TMT_TOCOLOR1                         = 2006;
  TMT_TOCOLOR2                         = 2007;
  TMT_TOCOLOR3                         = 2008;
  TMT_TOCOLOR4                         = 2009;
  TMT_TOCOLOR5                         = 2010;

  { rendering BOOL properties }
  TMT_TRANSPARENT                      = 2201;
  TMT_AUTOSIZE                         = 2202;
  TMT_BORDERONLY                       = 2203;
  TMT_COMPOSITED                       = 2204;
  TMT_BGFILL                           = 2205;
  TMT_GLYPHTRANSPARENT                 = 2206;
  TMT_GLYPHONLY                        = 2207;
  TMT_ALWAYSSHOWSIZINGBAR              = 2208;
  TMT_MIRRORIMAGE                      = 2209;
  TMT_UNIFORMSIZING                    = 2210;
  TMT_INTEGRALSIZING                   = 2211;
  TMT_SOURCEGROW                       = 2212;
  TMT_SOURCESHRINK                     = 2213;

  { rendering INT properties }
  TMT_IMAGECOUNT                       = 2401;
  TMT_ALPHALEVEL                       = 2402;
  TMT_BORDERSIZE                       = 2403;
  TMT_ROUNDCORNERWIDTH                 = 2404;
  TMT_ROUNDCORNERHEIGHT                = 2405;
  TMT_GRADIENTRATIO1                   = 2406;
  TMT_GRADIENTRATIO2                   = 2407;
  TMT_GRADIENTRATIO3                   = 2408;
  TMT_GRADIENTRATIO4                   = 2409;
  TMT_GRADIENTRATIO5                   = 2410;
  TMT_PROGRESSCHUNKSIZE                = 2411;
  TMT_PROGRESSSPACESIZE                = 2412;
  TMT_SATURATION                       = 2413;
  TMT_TEXTBORDERSIZE                   = 2414;
  TMT_ALPHATHRESHOLD                   = 2415;
  TMT_WIDTH                            = 2416;
  TMT_HEIGHT                           = 2417;
  TMT_GLYPHINDEX                       = 2418;
  TMT_TRUESIZESTRETCHMARK              = 2419;
  TMT_MINDPI1                          = 2420;
  TMT_MINDPI2                          = 2421;
  TMT_MINDPI3                          = 2422;
  TMT_MINDPI4                          = 2423;
  TMT_MINDPI5                          = 2424;

  { rendering FONT properties }
  TMT_GLYPHFONT                        = 2601;

  { rendering INTLIST properties }
  TMT_FILESIZES                        = 2801;

  { rendering FILENAME properties }
  TMT_IMAGEFILE                        = 3001;
  TMT_IMAGEFILE1                       = 3002;
  TMT_IMAGEFILE2                       = 3003;
  TMT_IMAGEFILE3                       = 3004;
  TMT_IMAGEFILE4                       = 3005;
  TMT_IMAGEFILE5                       = 3006;
  TMT_STOCKIMAGEFILE                   = 3007;
  TMT_GLYPHIMAGEFILE                   = 3008;

  { rendering STRING properties }
  TMT_TEXT                             = 3201;

  { rendering POSITION (x and y values) properties }
  TMT_OFFSET                           = 3401;
  TMT_TEXTSHADOWOFFSET                 = 3402;
  TMT_MINSIZE                          = 3403;
  TMT_MINSIZE1                         = 3404;
  TMT_MINSIZE2                         = 3405;
  TMT_MINSIZE3                         = 3406;
  TMT_MINSIZE4                         = 3407;
  TMT_MINSIZE5                         = 3408;
  TMT_NORMALSIZE                       = 3409;

  { rendering MARGIN properties }
  TMT_SIZINGMARGINS                    = 3601;
  TMT_CONTENTMARGINS                   = 3602;
  TMT_CAPTIONMARGINS                   = 3603;

  { rendering COLOR properties }
  TMT_BORDERCOLOR                      = 3801;
  TMT_FILLCOLOR                        = 3802;
  TMT_TEXTCOLOR                        = 3803;
  TMT_EDGELIGHTCOLOR                   = 3804;
  TMT_EDGEHIGHLIGHTCOLOR               = 3805;
  TMT_EDGESHADOWCOLOR                  = 3806;
  TMT_EDGEDKSHADOWCOLOR                = 3807;
  TMT_EDGEFILLCOLOR                    = 3808;
  TMT_TRANSPARENTCOLOR                 = 3809;
  TMT_GRADIENTCOLOR1                   = 3810;
  TMT_GRADIENTCOLOR2                   = 3811;
  TMT_GRADIENTCOLOR3                   = 3812;
  TMT_GRADIENTCOLOR4                   = 3813;
  TMT_GRADIENTCOLOR5                   = 3814;
  TMT_SHADOWCOLOR                      = 3815;
  TMT_GLOWCOLOR                        = 3816;
  TMT_TEXTBORDERCOLOR                  = 3817;
  TMT_TEXTSHADOWCOLOR                  = 3818;
  TMT_GLYPHTEXTCOLOR                   = 3819;
  TMT_GLYPHTRANSPARENTCOLOR            = 3820;
  TMT_FILLCOLORHINT                    = 3821;
  TMT_BORDERCOLORHINT                  = 3822;
  TMT_ACCENTCOLORHINT                  = 3823;

  { rendering enum properties  }
  TMT_BGTYPE                           = 4001;
  TMT_BORDERTYPE                       = 4002;
  TMT_FILLTYPE                         = 4003;
  TMT_SIZINGTYPE                       = 4004;
  TMT_HALIGN                           = 4005;
  TMT_CONTENTALIGNMENT                 = 4006;
  TMT_VALIGN                           = 4007;
  TMT_OFFSETTYPE                       = 4008;
  TMT_ICONEFFECT                       = 4009;
  TMT_TEXTSHADOWTYPE                   = 4010;
  TMT_IMAGELAYOUT                      = 4011;
  TMT_GLYPHTYPE                        = 4012;
  TMT_IMAGESELECTTYPE                  = 4013;
  TMT_GLYPHFONTSIZINGTYPE              = 4014;
  TMT_TRUESIZESCALINGTYPE              = 4015;

  { custom properties (used only by controls/shell) }
  TMT_USERPICTURE                      = 5001;
  TMT_DEFAULTPANESIZE                  = 5002;
  TMT_BLENDCOLOR                       = 5003;


{  "Window" (i.e., non-client) Parts & States }

  { WINDOWPARTS }
  WP_CAPTION                           = 1;
  WP_SMALLCAPTION                      = 2;
  WP_MINCAPTION                        = 3;
  WP_SMALLMINCAPTION                   = 4;
  WP_MAXCAPTION                        = 5;
  WP_SMALLMAXCAPTION                   = 6;
  WP_FRAMELEFT                         = 7;
  WP_FRAMERIGHT                        = 8;
  WP_FRAMEBOTTOM                       = 9;
  WP_SMALLFRAMELEFT                    = 10;
  WP_SMALLFRAMERIGHT                   = 11;
  WP_SMALLFRAMEBOTTOM                  = 12;
  WP_SYSBUTTON                         = 13;
  WP_MDISYSBUTTON                      = 14;
  WP_MINBUTTON                         = 15;
  WP_MDIMINBUTTON                      = 16;
  WP_MAXBUTTON                         = 17;
  WP_CLOSEBUTTON                       = 18;
  WP_SMALLCLOSEBUTTON                  = 19;
  WP_MDICLOSEBUTTON                    = 20;
  WP_RESTOREBUTTON                     = 21;
  WP_MDIRESTOREBUTTON                  = 22;
  WP_HELPBUTTON                        = 23;
  WP_MDIHELPBUTTON                     = 24;
  WP_HORZSCROLL                        = 25;
  WP_HORZTHUMB                         = 26;
  WP_VERTSCROLL                        = 27;
  WP_VERTTHUMB                         = 28;
  WP_DIALOG                            = 29;
  WP_CAPTIONSIZINGTEMPLATE             = 30;
  WP_SMALLCAPTIONSIZINGTEMPLATE        = 31;
  WP_FRAMELEFTSIZINGTEMPLATE           = 32;
  WP_SMALLFRAMELEFTSIZINGTEMPLATE      = 33;
  WP_FRAMERIGHTSIZINGTEMPLATE          = 34;
  WP_SMALLFRAMERIGHTSIZINGTEMPLATE     = 35;
  WP_FRAMEBOTTOMSIZINGTEMPLATE         = 36;
  WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE    = 37;

  { FRAMESTATES }
  FS_ACTIVE                            = 1;
  FS_INACTIVE                          = 2;

  { CAPTIONSTATES }
  CS_ACTIVE                            = 1;
  CS_INACTIVE                          = 2;
  CS_DISABLED                          = 3;

  { MAXCAPTIONSTATES }
  MXCS_ACTIVE                          = 1;
  MXCS_INACTIVE                        = 2;
  MXCS_DISABLED                        = 3;

  { MINCAPTIONSTATES }
  MNCS_ACTIVE                          = 1;
  MNCS_INACTIVE                        = 2;
  MNCS_DISABLED                        = 3;

  { HORZSCROLLSTATES }
  HSS_NORMAL                           = 1;
  HSS_HOT                              = 2;
  HSS_PUSHED                           = 3;
  HSS_DISABLED                         = 4;

  { HORZTHUMBSTATES }
  HTS_NORMAL                           = 1;
  HTS_HOT                              = 2;
  HTS_PUSHED                           = 3;
  HTS_DISABLED                         = 4;

  { VERTSCROLLSTATES }
  VSS_NORMAL                           = 1;
  VSS_HOT                              = 2;
  VSS_PUSHED                           = 3;
  VSS_DISABLED                         = 4;

  { VERTTHUMBSTATES }
  VTS_NORMAL                           = 1;
  VTS_HOT                              = 2;
  VTS_PUSHED                           = 3;
  VTS_DISABLED                         = 4;

  { SYSBUTTONSTATES }
  SBS_NORMAL                           = 1;
  SBS_HOT                              = 2;
  SBS_PUSHED                           = 3;
  SBS_DISABLED                         = 4;

  { MINBUTTONSTATES }
  MINBS_NORMAL                         = 1;
  MINBS_HOT                            = 2;
  MINBS_PUSHED                         = 3;
  MINBS_DISABLED                       = 4;

  { MAXBUTTONSTATES }
  MAXBS_NORMAL                         = 1;
  MAXBS_HOT                            = 2;
  MAXBS_PUSHED                         = 3;
  MAXBS_DISABLED                       = 4;

  { RESTOREBUTTONSTATES }
  RBS_NORMAL                           = 1;
  RBS_HOT                              = 2;
  RBS_PUSHED                           = 3;
  RBS_DISABLED                         = 4;

  { HELPBUTTONSTATES }
  HBS_NORMAL                           = 1;
  HBS_HOT                              = 2;
  HBS_PUSHED                           = 3;
  HBS_DISABLED                         = 4;

  { CLOSEBUTTONSTATES }
  CBS_NORMAL                           = 1;
  CBS_HOT                              = 2;
  CBS_PUSHED                           = 3;
  CBS_DISABLED                         = 4;


{ "Button" Parts & States }


  { BUTTONPARTS }
  BP_PUSHBUTTON                        = 1;
  BP_RADIOBUTTON                       = 2;
  BP_CHECKBOX                          = 3;
  BP_GROUPBOX                          = 4;
  BP_USERBUTTON                        = 5;

  { PUSHBUTTONSTATES }
  PBS_NORMAL                           = 1;
  PBS_HOT                              = 2;
  PBS_PRESSED                          = 3;
  PBS_DISABLED                         = 4;
  PBS_DEFAULTED                        = 5;

  {  RADIOBUTTONSTATES }
  RBS_UNCHECKEDNORMAL                  = 1;
  RBS_UNCHECKEDHOT                     = 2;
  RBS_UNCHECKEDPRESSED                 = 3;
  RBS_UNCHECKEDDISABLED                = 4;
  RBS_CHECKEDNORMAL                    = 5;
  RBS_CHECKEDHOT                       = 6;
  RBS_CHECKEDPRESSED                   = 7;
  RBS_CHECKEDDISABLED                  = 8;

  { CHECKBOXSTATES }
  CBS_UNCHECKEDNORMAL                  = 1;
  CBS_UNCHECKEDHOT                     = 2;
  CBS_UNCHECKEDPRESSED                 = 3;
  CBS_UNCHECKEDDISABLED                = 4;
  CBS_CHECKEDNORMAL                    = 5;
  CBS_CHECKEDHOT                       = 6;
  CBS_CHECKEDPRESSED                   = 7;
  CBS_CHECKEDDISABLED                  = 8;
  CBS_MIXEDNORMAL                      = 9;
  CBS_MIXEDHOT                         = 10;
  CBS_MIXEDPRESSED                     = 11;
  CBS_MIXEDDISABLED                    = 12;

  { GROUPBOXSTATES }
  GBS_NORMAL                           = 1;
  GBS_DISABLED                         = 2;


{ "Rebar" Parts & States }

  { REBARPARTS }
  RP_GRIPPER                           = 1;
  RP_GRIPPERVERT                       = 2;
  RP_BAND                              = 3;
  RP_CHEVRON                           = 4;
  RP_CHEVRONVERT                       = 5;

  { CHEVRONSTATES }
  CHEVS_NORMAL                         = 1;
  CHEVS_HOT                            = 2;
  CHEVS_PRESSED                        = 3;


{ "Toolbar" Parts & States }

  { TOOLBARPARTS }
  TP_BUTTON                            = 1;
  TP_DROPDOWNBUTTON                    = 2;
  TP_SPLITBUTTON                       = 3;
  TP_SPLITBUTTONDROPDOWN               = 4;
  TP_SEPARATOR                         = 5;
  TP_SEPARATORVERT                     = 6;

  { TOOLBARSTATES }
  TS_NORMAL                            = 1;
  TS_HOT                               = 2;
  TS_PRESSED                           = 3;
  TS_DISABLED                          = 4;
  TS_CHECKED                           = 5;
  TS_HOTCHECKED                        = 6;


{ "Status" Parts & States }

  { STATUSPARTS }
  SP_PANE                              = 1;
  SP_GRIPPERPANE                       = 2;
  SP_GRIPPER                           = 3;


{ "Menu" Parts & States }

  { MENUPARTS }
  MP_MENUITEM                          = 1;
  MP_MENUDROPDOWN                      = 2;
  MP_MENUBARITEM                       = 3;
  MP_MENUBARDROPDOWN                   = 4;
  MP_CHEVRON                           = 5;
  MP_SEPARATOR                         = 6;

  { MENUSTATES }
  MS_NORMAL                            = 1;
  MS_SELECTED                          = 2;
  MS_DEMOTED                           = 3;


{ "ListView" Parts & States }

  { LISTVIEWPARTS }
  LVP_LISTITEM                         = 1;
  LVP_LISTGROUP                        = 2;
  LVP_LISTDETAIL                       = 3;
  LVP_LISTSORTEDDETAIL                 = 4;
  LVP_EMPTYTEXT                        = 5;

  { LISTITEMSTATES }
  LIS_NORMAL                           = 1;
  LIS_HOT                              = 2;
  LIS_SELECTED                         = 3;
  LIS_DISABLED                         = 4;
  LIS_SELECTEDNOTFOCUS                 = 5;


{ "Header" Parts & States }

 { HEADERPARTS }
  HP_HEADERITEM                        = 1;
  HP_HEADERITEMLEFT                    = 2;
  HP_HEADERITEMRIGHT                   = 3;
  HP_HEADERSORTARROW                   = 4;

  { HEADERITEMSTATES }
  HIS_NORMAL                           = 1;
  HIS_HOT                              = 2;
  HIS_PRESSED                          = 3;

  { HEADERITEMLEFTSTATES }
  HILS_NORMAL                          = 1;
  HILS_HOT                             = 2;
  HILS_PRESSED                         = 3;

  { HEADERITEMRIGHTSTATES }
  HIRS_NORMAL                          = 1;
  HIRS_HOT                             = 2;
  HIRS_PRESSED                         = 3;

  { HEADERSORTARROWSTATES }
  HSAS_SORTEDUP                        = 1;
  HSAS_SORTEDDOWN                      = 2;


{ "Progress" Parts & States }

  { PROGRESSPARTS }
  PP_BAR                               = 1;
  PP_BARVERT                           = 2;
  PP_CHUNK                             = 3;
  PP_CHUNKVERT                         = 4;


{ "Tab" Parts & States }

  { TABPARTS }
  TABP_TABITEM                         = 1;
  TABP_TABITEMLEFTEDGE                 = 2;
  TABP_TABITEMRIGHTEDGE                = 3;
  TABP_TABITEMBOTHEDGE                 = 4;
  TABP_TOPTABITEM                      = 5;
  TABP_TOPTABITEMLEFTEDGE              = 6;
  TABP_TOPTABITEMRIGHTEDGE             = 7;
  TABP_TOPTABITEMBOTHEDGE              = 8;
  TABP_PANE                            = 9;
  TABP_BODY                            = 10;

  { TABITEMSTATES }
  TIS_NORMAL                           = 1;
  TIS_HOT                              = 2;
  TIS_SELECTED                         = 3;
  TIS_DISABLED                         = 4;
  TIS_FOCUSED                          = 5;

  { TABITEMLEFTEDGESTATES }
  TILES_NORMAL                         = 1;
  TILES_HOT                            = 2;
  TILES_SELECTED                       = 3;
  TILES_DISABLED                       = 4;
  TILES_FOCUSED                        = 5;

  { TABITEMRIGHTEDGESTATES }
  TIRES_NORMAL                         = 1;
  TIRES_HOT                            = 2;
  TIRES_SELECTED                       = 3;
  TIRES_DISABLED                       = 4;
  TIRES_FOCUSED                        = 5;

  { TABITEMBOTHEDGESSTATES }
  TIBES_NORMAL                         = 1;
  TIBES_HOT                            = 2;
  TIBES_SELECTED                       = 3;
  TIBES_DISABLED                       = 4;
  TIBES_FOCUSED                        = 5;

  { TOPTABITEMSTATES }
  TTIS_NORMAL                          = 1;
  TTIS_HOT                             = 2;
  TTIS_SELECTED                        = 3;
  TTIS_DISABLED                        = 4;
  TTIS_FOCUSED                         = 5;

  { TOPTABITEMLEFTEDGESTATES }
  TTILES_NORMAL                        = 1;
  TTILES_HOT                           = 2;
  TTILES_SELECTED                      = 3;
  TTILES_DISABLED                      = 4;
  TTILES_FOCUSED                       = 5;

  { TOPTABITEMRIGHTEDGESTATES }
  TTIRES_NORMAL                        = 1;
  TTIRES_HOT                           = 2;
  TTIRES_SELECTED                      = 3;
  TTIRES_DISABLED                      = 4;
  TTIRES_FOCUSED                       = 5;

  { TOPTABITEMBOTHEDGESSTATES }
  TTIBES_NORMAL                        = 1;
  TTIBES_HOT                           = 2;
  TTIBES_SELECTED                      = 3;
  TTIBES_DISABLED                      = 4;
  TTIBES_FOCUSED                       = 5;


{"Trackbar" Parts & States }

  { TRACKBARPARTS }
  TKP_TRACK                            = 1;
  TKP_TRACKVERT                        = 2;
  TKP_THUMB                            = 3;
  TKP_THUMBBOTTOM                      = 4;
  TKP_THUMBTOP                         = 5;
  TKP_THUMBVERT                        = 6;
  TKP_THUMBLEFT                        = 7;
  TKP_THUMBRIGHT                       = 8;
  TKP_TICS                             = 9;
  TKP_TICSVERT                         = 10;

  { TRACKBARSTATES }
  TKS_NORMAL                           = 1;

  { TRACKSTATES }
  TRS_NORMAL                           = 1;

  { TRACKVERTSTATES }
  TRVS_NORMAL                          = 1;

  { THUMBSTATES }
  TUS_NORMAL                           = 1;
  TUS_HOT                              = 2;
  TUS_PRESSED                          = 3;
  TUS_FOCUSED                          = 4;
  TUS_DISABLED                         = 5;

  { THUMBBOTTOMSTATES }
  TUBS_NORMAL                          = 1;
  TUBS_HOT                             = 2;
  TUBS_PRESSED                         = 3;
  TUBS_FOCUSED                         = 4;
  TUBS_DISABLED                        = 5;

  { THUMBTOPSTATES }
  TUTS_NORMAL                          = 1;
  TUTS_HOT                             = 2;
  TUTS_PRESSED                         = 3;
  TUTS_FOCUSED                         = 4;
  TUTS_DISABLED                        = 5;

  { THUMBVERTSTATES }
  TUVS_NORMAL                          = 1;
  TUVS_HOT                             = 2;
  TUVS_PRESSED                         = 3;
  TUVS_FOCUSED                         = 4;
  TUVS_DISABLED                        = 5;

  { THUMBLEFTSTATES }
  TUVLS_NORMAL                         = 1;
  TUVLS_HOT                            = 2;
  TUVLS_PRESSED                        = 3;
  TUVLS_FOCUSED                        = 4;
  TUVLS_DISABLED                       = 5;

  { THUMBRIGHTSTATES }
  TUVRS_NORMAL                         = 1;
  TUVRS_HOT                            = 2;
  TUVRS_PRESSED                        = 3;
  TUVRS_FOCUSED                        = 4;
  TUVRS_DISABLED                       = 5;

  { TICSSTATES }
  TSS_NORMAL                           = 1;

  { TICSVERTSTATES }
  TSVS_NORMAL                          = 1;


{ "Tooltips" Parts & States }

  { TOOLTIPPARTS }
  TTP_STANDARD                         = 1;
  TTP_STANDARDTITLE                    = 2;
  TTP_BALLOON                          = 3;
  TTP_BALLOONTITLE                     = 4;
  TTP_CLOSE                            = 5;

  { CLOSESTATES }
  TTCS_NORMAL                          = 1;
  TTCS_HOT                             = 2;
  TTCS_PRESSED                         = 3;

  { STANDARDSTATES }
  TTSS_NORMAL                          = 1;
  TTSS_LINK                            = 2;

  { BALLOONSTATES }
  TTBS_NORMAL                          = 1;
  TTBS_LINK                            = 2;


{ "TreeView" Parts & States }

  { TREEVIEWPARTS }
  TVP_TREEITEM                         = 1;
  TVP_GLYPH                            = 2;
  TVP_BRANCH                           = 3;

  { TREEITEMSTATES }
  TREIS_NORMAL                         = 1;
  TREIS_HOT                            = 2;
  TREIS_SELECTED                       = 3;
  TREIS_DISABLED                       = 4;
  TREIS_SELECTEDNOTFOCUS               = 5;

  { GLYPHSTATES }
  GLPS_CLOSED                          = 1;
  GLPS_OPENED                          = 2;


{ "Spin" Parts & States }

  { SPINPARTS }
  SPNP_UP                              = 1;
  SPNP_DOWN                            = 2;
  SPNP_UPHORZ                          = 3;
  SPNP_DOWNHORZ                        = 4;

  { UPSTATES }
  UPS_NORMAL                           = 1;
  UPS_HOT                              = 2;
  UPS_PRESSED                          = 3;
  UPS_DISABLED                         = 4;

  { DOWNSTATES }
  DNS_NORMAL                           = 1;
  DNS_HOT                              = 2;
  DNS_PRESSED                          = 3;
  DNS_DISABLED                         = 4;

  { UPHORZSTATES }
  UPHZS_NORMAL                         = 1;
  UPHZS_HOT                            = 2;
  UPHZS_PRESSED                        = 3;
  UPHZS_DISABLED                       = 4;

  { DOWNHORZSTATES }
  DNHZS_NORMAL                         = 1;
  DNHZS_HOT                            = 2;
  DNHZS_PRESSED                        = 3;
  DNHZS_DISABLED                       = 4;


{ "Page" Parts & States }

  { PAGEPARTS }
  PGRP_UP                              = 1;
  PGRP_DOWN                            = 2;
  PGRP_UPHORZ                          = 3;
  PGRP_DOWNHORZ                        = 4;

{ Pager uses same states as Spin }


{ "Scrollbar" Parts & States }

  { SCROLLBARPARTS }
  SBP_ARROWBTN                         = 1;
  SBP_THUMBBTNHORZ                     = 2;
  SBP_THUMBBTNVERT                     = 3;
  SBP_LOWERTRACKHORZ                   = 4;
  SBP_UPPERTRACKHORZ                   = 5;
  SBP_LOWERTRACKVERT                   = 6;
  SBP_UPPERTRACKVERT                   = 7;
  SBP_GRIPPERHORZ                      = 8;
  SBP_GRIPPERVERT                      = 9;
  SBP_SIZEBOX                          = 10;

  { ARROWBTNSTATES }
  ABS_UPNORMAL                         = 1;
  ABS_UPHOT                            = 2;
  ABS_UPPRESSED                        = 3;
  ABS_UPDISABLED                       = 4;
  ABS_DOWNNORMAL                       = 5;
  ABS_DOWNHOT                          = 6;
  ABS_DOWNPRESSED                      = 7;
  ABS_DOWNDISABLED                     = 8;
  ABS_LEFTNORMAL                       = 9;
  ABS_LEFTHOT                          = 10;
  ABS_LEFTPRESSED                      = 11;
  ABS_LEFTDISABLED                     = 12;
  ABS_RIGHTNORMAL                      = 13;
  ABS_RIGHTHOT                         = 14;
  ABS_RIGHTPRESSED                     = 15;
  ABS_RIGHTDISABLED                    = 16;

  { SCROLLBARSTATES }
  SCRBS_NORMAL                         = 1;
  SCRBS_HOT                            = 2;
  SCRBS_PRESSED                        = 3;
  SCRBS_DISABLED                       = 4;

  { SIZEBOXSTATES }
  SZB_RIGHTALIGN                       = 1;
  SZB_LEFTALIGN                        = 2;


{ "Edit" Parts & States }

  { EDITPARTS }
  EP_EDITTEXT                          = 1;
  EP_CARET                             = 2;

  { EDITTEXTSTATES }
  ETS_NORMAL                           = 1;
  ETS_HOT                              = 2;
  ETS_SELECTED                         = 3;
  ETS_DISABLED                         = 4;
  ETS_FOCUSED                          = 5;
  ETS_READONLY                         = 6;
  ETS_ASSIST                           = 7;

{ "ComboBox" Parts & States }

  { COMBOBOXPARTS }
  CP_DROPDOWNBUTTON                    = 1;

  { COMBOBOXSTATES }
  CBXS_NORMAL                          = 1;
  CBXS_HOT                             = 2;
  CBXS_PRESSED                         = 3;
  CBXS_DISABLED                        = 4;

{ "Taskbar Clock" Parts & States }

  { CLOCKPARTS }
  CLP_TIME                             = 1;

  { CLOCKSTATES }
  CLS_NORMAL                           = 1;


{ "Tray Notify" Parts & States }

  { TRAYNOTIFYPARTS }
  TNP_BACKGROUND                       = 1;
  TNP_ANIMBACKGROUND                   = 2;


{ "TaskBar" Parts & States }

  { TASKBARPARTS }
  TBP_BACKGROUNDBOTTOM                 = 1;
  TBP_BACKGROUNDRIGHT                  = 2;
  TBP_BACKGROUNDTOP                    = 3;
  TBP_BACKGROUNDLEFT                   = 4;
  TBP_SIZINGBARBOTTOM                  = 5;
  TBP_SIZINGBARRIGHT                   = 6;
  TBP_SIZINGBARTOP                     = 7;
  TBP_SIZINGBARLEFT                    = 8;


{ "TaskBand" Parts & States }

  { TASKBANDPARTS }
  TDP_GROUPCOUNT                       = 1;
  TDP_FLASHBUTTON                      = 2;
  TDP_FLASHBUTTONGROUPMENU             = 3;


{ "StartPanel" Parts & States }

  { STARTPANELPARTS }
  SPP_USERPANE                         = 1;
  SPP_MOREPROGRAMS                     = 2;
  SPP_MOREPROGRAMSARROW                = 3;
  SPP_PROGLIST                         = 4;
  SPP_PROGLISTSEPARATOR                = 5;
  SPP_PLACESLIST                       = 6;
  SPP_PLACESLISTSEPARATOR              = 7;
  SPP_LOGOFF                           = 8;
  SPP_LOGOFFBUTTONS                    = 9;
  SPP_USERPICTURE                      = 10;
  SPP_PREVIEW                          = 11;

  { MOREPROGRAMSARROWSTATES }
  SPS_NORMAL                           = 1;
  SPS_HOT                              = 2;
  SPS_PRESSED                          = 3;

  { LOGOFFBUTTONSSTATES }
  SPLS_NORMAL                          = 1;
  SPLS_HOT                             = 2;
  SPLS_PRESSED                         = 3;


{ "ExplorerBar" Parts & States }

  { EXPLORERBARPARTS }
  EBP_HEADERBACKGROUND                 = 1;
  EBP_HEADERCLOSE                      = 2;
  EBP_HEADERPIN                        = 3;
  EBP_IEBARMENU                        = 4;
  EBP_NORMALGROUPBACKGROUND            = 5;
  EBP_NORMALGROUPCOLLAPSE              = 6;
  EBP_NORMALGROUPEXPAND                = 7;
  EBP_NORMALGROUPHEAD                  = 8;
  EBP_SPECIALGROUPBACKGROUND           = 9;
  EBP_SPECIALGROUPCOLLAPSE             = 10;
  EBP_SPECIALGROUPEXPAND               = 11;
  EBP_SPECIALGROUPHEAD                 = 12;

  { HEADERCLOSESTATES }
  EBHC_NORMAL                          = 1;
  EBHC_HOT                             = 2;
  EBHC_PRESSED                         = 3;

  { HEADERPINSTATES }
  EBHP_NORMAL                          = 1;
  EBHP_HOT                             = 2;
  EBHP_PRESSED                         = 3;
  EBHP_SELECTEDNORMAL                  = 4;
  EBHP_SELECTEDHOT                     = 5;
  EBHP_SELECTEDPRESSED                 = 6;

  { IEBARMENUSTATES }
  EBM_NORMAL                           = 1;
  EBM_HOT                              = 2;
  EBM_PRESSED                          = 3;

  { NORMALGROUPCOLLAPSESTATES }
  EBNGC_NORMAL                         = 1;
  EBNGC_HOT                            = 2;
  EBNGC_PRESSED                        = 3;

  { NORMALGROUPEXPANDSTATES }
  EBNGE_NORMAL                         = 1;
  EBNGE_HOT                            = 2;
  EBNGE_PRESSED                        = 3;

  { SPECIALGROUPCOLLAPSESTATES }
  EBSGC_NORMAL                         = 1;
  EBSGC_HOT                            = 2;
  EBSGC_PRESSED                        = 3;

  { SPECIALGROUPEXPANDSTATES }
  EBSGE_NORMAL                         = 1;
  EBSGE_HOT                            = 2;
  EBSGE_PRESSED                        = 3;


{ "TaskBand" Parts & States }

  { MENUBANDPARTS }
  MDP_NEWAPPBUTTON                     = 1;
  MDP_SEPERATOR                        = 2;

  { MENUBANDSTATES }
  MDS_NORMAL                           = 1;
  MDS_HOT                              = 2;
  MDS_PRESSED                          = 3;
  MDS_DISABLED                         = 4;
  MDS_CHECKED                          = 5;
  MDS_HOTCHECKED                       = 6;



{ Access to uxtheme.dll }

type
  HIMAGELIST = THandle;
  HTHEME = THandle;

const
  DTT_GRAYED                 = $1;

  HTTB_BACKGROUNDSEG         = $0000;
  HTTB_FIXEDBORDER           = $0002;
  HTTB_CAPTION               = $0004;
  HTTB_RESIZINGBORDER_LEFT   = $0010;
  HTTB_RESIZINGBORDER_TOP    = $0020;
  HTTB_RESIZINGBORDER_RIGHT  = $0040;
  HTTB_RESIZINGBORDER_BOTTOM = $0080;
  HTTB_SIZINGTEMPLATE        = $0100;
  HTTB_SYSTEMSIZINGMARGINS   = $0200;
  HTTB_RESIZINGBORDER = HTTB_RESIZINGBORDER_LEFT or HTTB_RESIZINGBORDER_TOP or
     HTTB_RESIZINGBORDER_RIGHT or HTTB_RESIZINGBORDER_BOTTOM;




//------------------------------------------------------------------------------
//  OpenThemeData()     - Open the theme data for the specified HWND and
//                        semi-colon separated list of class names.
//
//                        OpenThemeData() will try each class name, one at
//                        a time, and use the first matching theme info
//                        found.  If a match is found, a theme handle
//                        to the data is returned.  If no match is found,
//                        a "NULL" handle is returned.
//
//                        When the window is destroyed or a WM_THEMECHANGED
//                        msg is received, "CloseThemeData()" should be
//                        called to close the theme handle.
//
//  hwnd                - window handle of the control/window to be themed
//
//  pszClassList        - class name (or list of names) to match to theme data
//                        section.  if the list contains more than one name,
//                        the names are tested one at a time for a match.
//                        If a match is found, OpenThemeData() returns a
//                        theme handle associated with the matching class.
//                        This param is a list (instead of just a single
//                        class name) to provide the class an opportunity
//                        to get the "best" match between the class and
//                        the current theme.  For example, a button might
//                        pass L"OkButton, Button" if its ID=ID_OK.  If
//                        the current theme has an entry for OkButton,
//                        that will be used.  Otherwise, we fall back on
//                        the normal Button entry.
//------------------------------------------------------------------------------

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;


//------------------------------------------------------------------------------
//  CloseThemeData()    - closes the theme data handle.  This should be done
//                        when the window being themed is destroyed or
//                        whenever a WM_THEMECHANGED msg is received
//                        (followed by an attempt to create a new Theme data
//                        handle).
//
//  hTheme              - open theme data handle (returned from prior call
//                        to OpenThemeData() API).
//------------------------------------------------------------------------------

  CloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;


//------------------------------------------------------------------------------
//    functions for basic drawing support
//------------------------------------------------------------------------------
// The following methods are the theme-aware drawing services.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//------------------------------------------------------------------------------
// For the list of all themed classes and the definition of all
// parts and states, see the file "tmschmea.h".
//------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId"                = "0" refers to
// the root class.
//------------------------------------------------------------------------------
// Note: draw operations are always scaled to fit (and not to exceed)
// the specified "Rect".
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  DrawThemeBackground()
//                      - draws the theme-specified border and fill for
//                        the "iPartId" and "iStateId".  This could be
//                        based on a bitmap file, a border and fill, or
//                        other image description.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pRect               - defines the size/location of the part
//  pClipRect           - optional clipping rect (don't draw outside it)
//------------------------------------------------------------------------------

  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const Rect: TRect; pClipRect: PRect): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  DrawThemeText()     - draws the text using the theme-specified
//                        color and font for the "iPartId" and
//                        "iStateId".
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pszText             - actual text to draw
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  dwTextFlags2        - additional drawing options
//  pRect               - defines the size/location of the part
//------------------------------------------------------------------------------

  DrawThemeText: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
    dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemeBackgroundContentRect()
//                      - gets the size of the content for the theme-defined
//                        background.  This is usually the area inside
//                        the borders or Margins.
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pBoundingRect   - the outer RECT of the part being drawn
//      pContentRect    - RECT to receive the content area
//------------------------------------------------------------------------------

  GetThemeBackgroundContentRect: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pBoundingRect: TRect; pContentRect: PRECT): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemeBackgroundExtent() - calculates the size/location of the theme-
//                               specified background based on the
//                               "pContentRect".
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pContentRect    - RECT that defines the content area
//      pBoundingRect   - RECT to receive the overall size/location of part
//------------------------------------------------------------------------------

  GetThemeBackgroundExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pContentRect: TRect;
    var pExtentRect: TRect): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemePartSize() - returns the specified size of the theme part
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font into & measure against
//  iPartId             - part number to retrieve size for
//  iStateId            - state number (of the part)
//  prc                 - (optional) rect for part drawing destination
//  eSize               - the type of size to be retreived
//  psz                 - receives the specified size of the part
//------------------------------------------------------------------------------

type
  THEMESIZE = (
    TS_MIN,             // minimum size of a visual style part
    TS_TRUE,            // size of the visual style part that will best fit the available space
    TS_DRAW);           // size that the theme manager uses to draw a part
  TThemeSize = THEMESIZE;

var
  GetThemePartSize: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; prc: PRECT; eSize: THEMESIZE;
    var psz: TSize): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeTextExtent() - calculates the size/location of the specified
//                         text when rendered in the Theme Font.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font & measure into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pszText             - the text to be measured
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  pszBoundingRect     - optional: to control layout of text
//  pszExtentRect       - receives the RECT for text size/location
//------------------------------------------------------------------------------

  GetThemeTextExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR;
    iCharCount: Integer; dwTextFlags: DWORD; pBoundingRect: PRECT; var pExtentRect: TRect): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemeTextMetrics()
//                      - returns info about the theme-specified font
//                        for the part/state passed in.
//
//  hTheme              - theme data handle
//  hdc                 - optional: HDC for screen context
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  ptm                 - receives the font info
//------------------------------------------------------------------------------

  GetThemeTextMetrics: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    var ptm: TEXTMETRIC): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemeBackgroundRegion()
//                      - computes the region for a regular or partially
//                        transparent theme-specified background that is
//                        bound by the specified "pRect".
//                        If the rectangle is empty, sets the HRGN to NULL
//                        and return S_FALSE.
//
//  hTheme              - theme data handle
//  hdc                 - optional HDC to draw into (DPI scaling)
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pRect               - the RECT used to draw the part
//  pRegion             - receives handle to calculated region
//------------------------------------------------------------------------------

  GetThemeBackgroundRegion: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    var pRegion: HRGN): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  HitTestThemeBackground()
//                      - returns a HitTestCode (a subset of the values
//                        returned by WM_NCHITTEST) for the point "ptTest"
//                        within the theme-specified background
//                        (bound by pRect).  "pRect" and "ptTest" should
//                        both be in the same coordinate system
//                        (client, screen, etc).
//
//      hTheme          - theme data handle
//      hdc             - HDC to draw into
//      iPartId         - part number to test against
//      iStateId        - state number (of the part)
//      pRect           - the RECT used to draw the part
//      hrgn            - optional region to use; must be in same coordinates as
//                      -    pRect and pTest.
//      ptTest          - the hit point to be tested
//      dwOptions       - HTTB_xxx constants
//      pwHitTestCode   - receives the returned hit test code - one of:
//
//                        HTNOWHERE, HTLEFT, HTTOPLEFT, HTBOTTOMLEFT,
//                        HTRIGHT, HTTOPRIGHT, HTBOTTOMRIGHT,
//                        HTTOP, HTBOTTOM, HTCLIENT
//------------------------------------------------------------------------------

  HitTestThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; dwOptions: DWORD;
    const pRect: TRect; hrgn: HRGN; ptTest: TPoint; var pwHitTestCode: WORD): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  DrawThemeEdge()     - Similar to the DrawEdge() API, but uses part colors
//                        and is high-DPI aware
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pDestRect           - the RECT used to draw the line(s)
//  uEdge               - Same as DrawEdge() API
//  uFlags              - Same as DrawEdge() API
//  pContentRect        - Receives the interior rect if (uFlags & BF_ADJUST)
//------------------------------------------------------------------------------

  DrawThemeEdge: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge,
    uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  DrawThemeIcon()     - draws an image within an imagelist based on
//                        a (possible) theme-defined effect.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pRect               - the RECT to draw the image within
//  himl                - handle to IMAGELIST
//  iImageIndex         - index into IMAGELIST (which icon to draw)
//------------------------------------------------------------------------------

  DrawThemeIcon: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; himl: HIMAGELIST;
    iImageIndex: Integer): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  IsThemePartDefined() - returns TRUE if the theme has defined parameters
//                         for the specified "iPartId" and "iStateId".
//
//  hTheme              - theme data handle
//  iPartId             - part number to find definition for
//  iStateId            - state number of part
//------------------------------------------------------------------------------

  IsThemePartDefined: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;


//------------------------------------------------------------------------------
//  IsThemeBackgroundPartiallyTransparent()
//                      - returns TRUE if the theme specified background for
//                        the part/state has transparent pieces or
//                        alpha-blended pieces.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//------------------------------------------------------------------------------

  IsThemeBackgroundPartiallyTransparent: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;


//------------------------------------------------------------------------------
//    lower-level theme information services
//------------------------------------------------------------------------------
// The following methods are getter routines for each of the Theme Data types.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId"                = "0" refers to
// the root class.
//------------------------------------------------------------------------------
// Each method also take a "iPropId" param because multiple instances of
// the same primitive type can be defined in the theme schema.
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//  GetThemeColor()     - Get the value for the specified COLOR property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pColor              - receives the value of the property
//------------------------------------------------------------------------------

var
  GetThemeColor: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeMetric()    - Get the value for the specified metric/size
//                        property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn into (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//------------------------------------------------------------------------------

  GetThemeMetric: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeString()    - Get the value for the specified string property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pszBuff             - receives the string property value
//  cchMaxBuffChars     - max. number of chars allowed in pszBuff
//------------------------------------------------------------------------------

  GetThemeString: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszBuff: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeBool()      - Get the value for the specified BOOL property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pfVal               - receives the value of the property
//------------------------------------------------------------------------------

  GetThemeBool: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pfVal: BOOL): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeInt()       - Get the value for the specified int property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//------------------------------------------------------------------------------

  GetThemeInt: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeEnumValue() - Get the value for the specified ENUM property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the enum (cast to int*)
//------------------------------------------------------------------------------

  GetThemeEnumValue: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemePosition()  - Get the value for the specified position
//                        property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pPoint              - receives the value of the position property
//------------------------------------------------------------------------------

  GetThemePosition: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;var pPoint: TPoint): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeFont()      - Get the value for the specified font property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn to (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pFont               - receives the value of the LOGFONT property
//                        (scaled for the current logical screen dpi)
//------------------------------------------------------------------------------

  GetThemeFont: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var pFont: LOGFONT): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeRect()      - Get the value for the specified RECT property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pRect               - receives the value of the RECT property
//------------------------------------------------------------------------------

  GetThemeRect: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pRect: TRect): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeMargins()   - Get the value for the specified MARGINS property
//
//      hTheme          - theme data handle
//      hdc             - (optional) hdc to be used for drawing
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      prc             - RECT for area to be drawn into
//      pMargins        - receives the value of the MARGINS property
//------------------------------------------------------------------------------

type
  _MARGINS = record
    cxLeftWidth: Integer;      // width of left border that retains its size
    cxRightWidth: Integer;     // width of right border that retains its size
    cyTopHeight: Integer;      // height of top border that retains its size
    cyBottomHeight: Integer;   // height of bottom border that retains its size
  end;
  MARGINS = _MARGINS;
  PMARGINS = ^MARGINS;
  TMargins = MARGINS;

var
  GetThemeMargins: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer; prc: PRECT;
    var pMargins: MARGINS): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemeIntList()   - Get the value for the specified INTLIST struct
//
//      hTheme          - theme data handle
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      pIntList        - receives the value of the INTLIST property
//------------------------------------------------------------------------------

const
  MAX_INTLIST_COUNT = 10;

type
  _INTLIST = record
    iValueCount: Integer;
    iValues: array [0..MAX_INTLIST_COUNT - 1] of Integer;
  end;
  INTLIST = _INTLIST;
  PINTLIST = ^INTLIST;
  TIntList = INTLIST;

var
  GetThemeIntList: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pIntList: INTLIST): HRESULT; stdcall;


//------------------------------------------------------------------------------
//  GetThemePropertyOrigin()
//                      - searches for the specified theme property
//                        and sets "pOrigin" to indicate where it was
//                        found (or not found)
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pOrigin             - receives the value of the property origin
//------------------------------------------------------------------------------

type
  PROPERTYORIGIN = (
    PO_STATE,           // property was found in the state section
    PO_PART,            // property was found in the part section
    PO_CLASS,           // property was found in the class section
    PO_GLOBAL,          // property was found in [globals] section
    PO_NOTFOUND);       // property was not found
  TPropertyOrigin = PROPERTYORIGIN;

var
  GetThemePropertyOrigin: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pOrigin: PROPERTYORIGIN): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  SetWindowTheme()
//                      - redirects an existing Window to use a different
//                        section of the current theme information than its
//                        class normally asks for.
//
//  hwnd                - the handle of the window (cannot be NULL)
//
//  pszSubAppName       - app (group) name to use in place of the calling
//                        app's name.  If NULL, the actual calling app
//                        name will be used.
//
//  pszSubIdList        - semicolon separated list of class Id names to
//                        use in place of actual list passed by the
//                        window's class.  if NULL, the id list from the
//                        calling class is used.
//------------------------------------------------------------------------------
// The Theme Manager will remember the "pszSubAppName" and the
// "pszSubIdList" associations thru the lifetime of the window (even
// if themes are subsequently changed).  The window is sent a
// "WM_THEMECHANGED" msg at the end of this call, so that the new
// theme can be found and applied.
//------------------------------------------------------------------------------
// When "pszSubAppName" or "pszSubIdList" are NULL, the Theme Manager
// removes the previously remember association.  To turn off theme-ing for
// the specified window, you can pass an empty string (L"") so it
// won't match any section entries.
//------------------------------------------------------------------------------

  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeFilename()  - Get the value for the specified FILENAME property.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pszThemeFileName    - output buffer to receive the filename
//  cchMaxBuffChars     - the size of the return buffer, in chars
//------------------------------------------------------------------------------

  GetThemeFilename: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszThemeFileName: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysColor()  - Get the value of the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        color from [SysMetrics] section of theme.
//                        if NULL, will return the global system color.
//
//  iColorId            - the system color index defined in winuser.h
//------------------------------------------------------------------------------

  GetThemeSysColor: function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysColorBrush()
//                      - Get the brush for the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        brush matching color from [SysMetrics] section of
//                        theme.  if NULL, will return the brush matching
//                        global system color.
//
//  iColorId            - the system color index defined in winuser.h
//------------------------------------------------------------------------------

  GetThemeSysColorBrush: function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysBool()   - Get the boolean value of specified System metric.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        BOOL from [SysMetrics] section of theme.
//                        if NULL, will return the specified system boolean.
//
//  iBoolId             - the TMT_XXX BOOL number (first BOOL
//                        is TMT_FLATMENUS)
//------------------------------------------------------------------------------

  GetThemeSysBool: function(hTheme: HTHEME; iBoolId: Integer): BOOL; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysSize()   - Get the value of the specified System size metric.
//                        (scaled for the current logical screen dpi)
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        size from [SysMetrics] section of theme.
//                        if NULL, will return the global system metric.
//
//  iSizeId             - the following values are supported when
//                        hTheme is non-NULL:
//
//                          SM_CXBORDER   (border width)
//                          SM_CXVSCROLL  (scrollbar width)
//                          SM_CYHSCROLL  (scrollbar height)
//                          SM_CXSIZE     (caption width)
//                          SM_CYSIZE     (caption height)
//                          SM_CXSMSIZE   (small caption width)
//                          SM_CYSMSIZE   (small caption height)
//                          SM_CXMENUSIZE (menubar width)
//                          SM_CYMENUSIZE (menubar height)
//
//                        when hTheme is NULL, iSizeId is passed directly
//                        to the GetSystemMetrics() function
//------------------------------------------------------------------------------

  GetThemeSysSize: function(hTheme: HTHEME; iSizeId: Integer): Integer; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysFont()   - Get the LOGFONT for the specified System font.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        font from [SysMetrics] section of theme.
//                        if NULL, will return the specified system font.
//
//  iFontId             - the TMT_XXX font number (first font
//                        is TMT_CAPTIONFONT)
//
//  plf                 - ptr to LOGFONT to receive the font value.
//                        (scaled for the current logical screen dpi)
//------------------------------------------------------------------------------

  GetThemeSysFont: function(hTheme: HTHEME; iFontId: Integer; var plf: LOGFONT): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysString() - Get the value of specified System string metric.
//
//  hTheme              - the theme data handle (required)
//
//  iStringId           - must be one of the following values:
//
//                          TMT_CSSNAME
//                          TMT_XMLNAME
//
//  pszStringBuff       - the buffer to receive the string value
//
//  cchMaxStringChars   - max. number of chars that pszStringBuff can hold
//------------------------------------------------------------------------------

  GetThemeSysString: function(hTheme: HTHEME; iStringId: Integer; pszStringBuff: LPWSTR;
    cchMaxStringChars: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeSysInt() - Get the value of specified System int.
//
//  hTheme              - the theme data handle (required)
//
//  iIntId              - must be one of the following values:
//
//                          TMT_DPIX
//                          TMT_DPIY
//                          TMT_MINCOLORDEPTH
//
//  piValue             - ptr to int to receive value
//------------------------------------------------------------------------------

  GetThemeSysInt: function(hTheme: HTHEME; iIntId: Integer; var piValue: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  IsThemeActive()     - can be used to test if a system theme is active
//                        for the current user session.
//
//                        use the API "IsAppThemed()" to test if a theme is
//                        active for the calling process.
//------------------------------------------------------------------------------

  IsThemeActive: function: BOOL; stdcall;

//------------------------------------------------------------------------------
//  IsAppThemed()       - returns TRUE if a theme is active and available to
//                        the current process
//------------------------------------------------------------------------------

  IsAppThemed: function: BOOL; stdcall;

//------------------------------------------------------------------------------
//  GetWindowTheme()    - if window is themed, returns its most recent
//                        HTHEME from OpenThemeData() - otherwise, returns
//                        NULL.
//
//      hwnd            - the window to get the HTHEME of
//------------------------------------------------------------------------------

  GetWindowTheme: function(hwnd: HWND): HTHEME; stdcall;

//------------------------------------------------------------------------------
//  EnableThemeDialogTexture()
//
//  - Enables/disables dialog background theme.  This method can be used to
//    tailor dialog compatibility with child windows and controls that
//    may or may not coordinate the rendering of their client area backgrounds
//    with that of their parent dialog in a manner that supports seamless
//    background texturing.
//
//      hdlg         - the window handle of the target dialog
//      dwFlags      - ETDT_ENABLE to enable the theme-defined dialog background texturing,
//                     ETDT_DISABLE to disable background texturing,
//                     ETDT_ENABLETAB to enable the theme-defined background
//                          texturing using the Tab texture
//------------------------------------------------------------------------------

const
  ETDT_DISABLE       = $00000001;
  ETDT_ENABLE        = $00000002;
  ETDT_USETABTEXTURE = $00000004;
  ETDT_ENABLETAB = ETDT_ENABLE or ETDT_USETABTEXTURE;

var
  EnableThemeDialogTexture: function(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  IsThemeDialogTextureEnabled()
//
//  - Reports whether the dialog supports background texturing.
//
//      hdlg         - the window handle of the target dialog
//------------------------------------------------------------------------------

var
  IsThemeDialogTextureEnabled: function(hwnd: HWND): BOOL; stdcall;

//------------------------------------------------------------------------------
//  GetThemeAppProperties()
//                      - returns the app property flags that control theming
//------------------------------------------------------------------------------

const
  STAP_ALLOW_NONCLIENT  = 1;
  STAP_ALLOW_CONTROLS   = 2;
  STAP_ALLOW_WEBCONTENT = 4;

var
  GetThemeAppProperties: function: DWORD; stdcall;

//------------------------------------------------------------------------------
//  SetThemeAppProperties()
//                      - sets the flags that control theming within the app
//
//      dwFlags         - the flag values to be set
//------------------------------------------------------------------------------

var
  SetThemeAppProperties: procedure(dwFlags: DWORD); stdcall;

//------------------------------------------------------------------------------
//  GetCurrentThemeName()
//                      - Get the name of the current theme in-use.
//                        Optionally, return the ColorScheme name and the
//                        Size name of the theme.
//
//  pszThemeFileName    - receives the theme path & filename
//  cchMaxNameChars     - max chars allowed in pszNameBuff
//
//  pszColorBuff        - (optional) receives the canonical color scheme name
//                        (not the display name)
//  cchMaxColorChars    - max chars allowed in pszColorBuff
//
//  pszSizeBuff         - (optional) receives the canonical size name
//                        (not the display name)
//  cchMaxSizeChars     - max chars allowed in pszSizeBuff
//------------------------------------------------------------------------------

var
  GetCurrentThemeName: function(pszThemeFileName: LPWSTR; cchMaxNameChars: Integer; pszColorBuff: LPWSTR;
    cchMaxColorChars: Integer; pszSizeBuff: LPWSTR; cchMaxSizeChars: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  GetThemeDocumentationProperty()
//                      - Get the value for the specified property name from
//                        the [documentation] section of the themes.ini file
//                        for the specified theme.  If the property has been
//                        localized in the theme files string table, the
//                        localized version of the property value is returned.
//
//  pszThemeFileName    - filename of the theme file to query
//  pszPropertyName     - name of the string property to retreive a value for
//  pszValueBuff        - receives the property string value
//  cchMaxValChars      - max chars allowed in pszValueBuff
//------------------------------------------------------------------------------

{ commented out for D4 compatibility
const
  SZ_THDOCPROP_DISPLAYNAME   = WideString('DisplayName');
  SZ_THDOCPROP_CANONICALNAME = WideString('ThemeName');
  SZ_THDOCPROP_TOOLTIP       = WideString('ToolTip');
  SZ_THDOCPROP_AUTHOR        = WideString('author'); }

var
  GetThemeDocumentationProperty: function(pszThemeName, pszPropertyName: LPCWSTR; pszValueBuff: LPWSTR;
    cchMaxValChars: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  Theme API Error Handling
//
//      All functions in the Theme API not returning an HRESULT (THEMEAPI_)
//      use the WIN32 function "SetLastError()" to record any call failures.
//
//      To retreive the error code of the last failure on the
//      current thread for these type of API's, use the WIN32 function
//      "GetLastError()".
//
//      All Theme API error codes (HRESULT's and GetLastError() values)
//      should be normal win32 errors which can be formatted into
//      strings using the Win32 API FormatMessage().
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DrawThemeParentBackground()
//                      - used by partially-transparent or alpha-blended
//                        child controls to draw the part of their parent
//                        that they appear in front of.
//
//  hwnd                - handle of the child control
//  hdc                 - hdc of the child control
//  prc                 - (optional) rect that defines the area to be
//                        drawn (CHILD coordinates)
//------------------------------------------------------------------------------

var
  DrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;

//------------------------------------------------------------------------------
//  EnableTheming()     - enables or disables themeing for the current user
//                        in the current and future sessions.
//
//  fEnable             - if FALSE, disable theming & turn themes off.
//                      - if TRUE, enable themeing and, if user previously
//                        had a theme active, make it active now.
//------------------------------------------------------------------------------

var
  EnableTheming: function(fEnable: BOOL): HRESULT; stdcall;

implementation

const
  UXTHEME_DLL = 'uxtheme.dll';

var
  DllHandle: THandle;

function InitXPThemes: Boolean;
begin
  if DllHandle = 0 then
  begin
    DllHandle := LoadLibrary(UXTHEME_DLL);
    if DllHandle > 0 then
    begin
      OpenThemeData := GetProcAddress(DllHandle, 'OpenThemeData');
      CloseThemeData := GetProcAddress(DllHandle, 'CloseThemeData');
      DrawThemeBackground := GetProcAddress(DllHandle, 'DrawThemeBackground');
      DrawThemeText := GetProcAddress(DllHandle, 'DrawThemeText');
      GetThemeBackgroundContentRect := GetProcAddress(DllHandle, 'GetThemeBackgroundContentRect');
      GetThemeBackgroundExtent := GetProcAddress(DllHandle, 'GetThemeBackgroundContentRect');
      GetThemePartSize := GetProcAddress(DllHandle, 'GetThemePartSize');
      GetThemeTextExtent := GetProcAddress(DllHandle, 'GetThemeTextExtent');
      GetThemeTextMetrics := GetProcAddress(DllHandle, 'GetThemeTextMetrics');
      GetThemeBackgroundRegion := GetProcAddress(DllHandle, 'GetThemeBackgroundRegion');
      HitTestThemeBackground := GetProcAddress(DllHandle, 'HitTestThemeBackground');
      DrawThemeEdge := GetProcAddress(DllHandle, 'DrawThemeEdge');
      DrawThemeIcon := GetProcAddress(DllHandle, 'DrawThemeIcon');
      IsThemePartDefined := GetProcAddress(DllHandle, 'IsThemePartDefined');
      IsThemeBackgroundPartiallyTransparent := GetProcAddress(DllHandle, 'IsThemeBackgroundPartiallyTransparent');
      GetThemeColor := GetProcAddress(DllHandle, 'GetThemeColor');
      GetThemeMetric := GetProcAddress(DllHandle, 'GetThemeMetric');
      GetThemeString := GetProcAddress(DllHandle, 'GetThemeString');
      GetThemeBool := GetProcAddress(DllHandle, 'GetThemeBool');
      GetThemeInt := GetProcAddress(DllHandle, 'GetThemeInt');
      GetThemeEnumValue := GetProcAddress(DllHandle, 'GetThemeEnumValue');
      GetThemePosition := GetProcAddress(DllHandle, 'GetThemePosition');
      GetThemeFont := GetProcAddress(DllHandle, 'GetThemeFont');
      GetThemeRect := GetProcAddress(DllHandle, 'GetThemeRect');
      GetThemeMargins := GetProcAddress(DllHandle, 'GetThemeMargins');
      GetThemeIntList := GetProcAddress(DllHandle, 'GetThemeIntList');
      GetThemePropertyOrigin := GetProcAddress(DllHandle, 'GetThemePropertyOrigin');
      SetWindowTheme := GetProcAddress(DllHandle, 'SetWindowTheme');
      GetThemeFilename := GetProcAddress(DllHandle, 'GetThemeFilename');
      GetThemeSysColor := GetProcAddress(DllHandle, 'GetThemeSysColor');
      GetThemeSysColorBrush := GetProcAddress(DllHandle, 'GetThemeSysColorBrush');
      GetThemeSysBool := GetProcAddress(DllHandle, 'GetThemeSysBool');
      GetThemeSysSize := GetProcAddress(DllHandle, 'GetThemeSysSize');
      GetThemeSysFont := GetProcAddress(DllHandle, 'GetThemeSysFont');
      GetThemeSysString := GetProcAddress(DllHandle, 'GetThemeSysString');
      GetThemeSysInt := GetProcAddress(DllHandle, 'GetThemeSysInt');
      IsThemeActive := GetProcAddress(DllHandle, 'IsThemeActive');
      IsAppThemed := GetProcAddress(DllHandle, 'IsAppThemed');
      GetWindowTheme := GetProcAddress(DllHandle, 'GetWindowTheme');
      EnableThemeDialogTexture := GetProcAddress(DllHandle, 'EnableThemeDialogTexture');
      IsThemeDialogTextureEnabled := GetProcAddress(DllHandle, 'IsThemeDialogTextureEnabled');
      GetThemeAppProperties := GetProcAddress(DllHandle, 'GetThemeAppProperties');
      SetThemeAppProperties := GetProcAddress(DllHandle, 'SetThemeAppProperties');
      GetCurrentThemeName := GetProcAddress(DllHandle, 'GetCurrentThemeName');
      GetThemeDocumentationProperty := GetProcAddress(DllHandle, 'GetThemeDocumentationProperty');
      DrawThemeParentBackground := GetProcAddress(DllHandle, 'DrawThemeParentBackground');
      EnableTheming := GetProcAddress(DllHandle, 'EnableTheming');
    end;
  end;
  Result := DllHandle > 0;
end;

procedure FreeXPThemes;
begin
  if DllHandle <> 0 then
  begin
    FreeLibrary(DllHandle);
    DllHandle := 0;

    OpenThemeData := nil;
    CloseThemeData := nil;
    DrawThemeBackground := nil;
    DrawThemeText := nil;
    GetThemeBackgroundContentRect := nil;
    GetThemeBackgroundExtent := nil;
    GetThemePartSize := nil;
    GetThemeTextExtent := nil;
    GetThemeTextMetrics := nil;
    GetThemeBackgroundRegion := nil;
    HitTestThemeBackground := nil;
    DrawThemeEdge := nil;
    DrawThemeIcon := nil;
    IsThemePartDefined := nil;
    IsThemeBackgroundPartiallyTransparent := nil;
    GetThemeColor := nil;
    GetThemeMetric := nil;
    GetThemeString := nil;
    GetThemeBool := nil;
    GetThemeInt := nil;
    GetThemeEnumValue := nil;
    GetThemePosition := nil;
    GetThemeFont := nil;
    GetThemeRect := nil;
    GetThemeMargins := nil;
    GetThemeIntList := nil;
    GetThemePropertyOrigin := nil;
    SetWindowTheme := nil;
    GetThemeFilename := nil;
    GetThemeSysColor := nil;
    GetThemeSysColorBrush := nil;
    GetThemeSysBool := nil;
    GetThemeSysSize := nil;
    GetThemeSysFont := nil;
    GetThemeSysString := nil;
    GetThemeSysInt := nil;
    IsThemeActive := nil;
    IsAppThemed := nil;
    GetWindowTheme := nil;
    EnableThemeDialogTexture := nil;
    IsThemeDialogTextureEnabled := nil;
    GetThemeAppProperties := nil;
    SetThemeAppProperties := nil;
    GetCurrentThemeName := nil;
    GetThemeDocumentationProperty := nil;
    DrawThemeParentBackground := nil;
    EnableTheming := nil;
  end;
end;

function CanUseXPThemes: Boolean;
begin
  Result := (DllHandle > 0) and IsAppThemed and IsThemeActive;
end;

end.
