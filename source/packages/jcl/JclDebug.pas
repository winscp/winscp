{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclDebug.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are Petr Vones and Marcel van Brakel.                }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Flier Lu (flier)                                                                               }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{   Petr Vones (pvones)                                                                            }
{   Soeren Muehlbauer                                                                              }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various debugging support routines and classes. This includes: Diagnostics routines, Trace       }
{ routines, Stack tracing and Source Locations a la the C/C++ __FILE__ and __LINE__ macros.        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebug;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Classes, System.SysUtils, System.Contnrs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, Contnrs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclFileUtils, JclPeImage,
  {$IFDEF BORLAND}
  {$IFNDEF WINSCP}
  JclTD32,
  {$ENDIF ~WINSCP}
  {$ENDIF BORLAND}
  JclSynch;

// Diagnostics
procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;

// use TraceMsg
// procedure Trace(const Msg: string);
procedure TraceMsg(const Msg: string);
{$IFNDEF WINSCP}
procedure TraceFmt(const Fmt: string; const Args: array of const);
{$ENDIF}
procedure TraceLoc(const Msg: string);
procedure TraceLocFmt(const Fmt: string; const Args: array of const);

// Optimized functionality of JclSysInfo functions ModuleFromAddr and IsSystemModule
type
  TJclModuleInfo = class(TObject)
  private
    FSize: Cardinal;
    FEndAddr: Pointer;
    FStartAddr: Pointer;
    FSystemModule: Boolean;
  public
    property EndAddr: Pointer read FEndAddr;
    property Size: Cardinal read FSize;
    property StartAddr: Pointer read FStartAddr;
    property SystemModule: Boolean read FSystemModule;
  end;

  TJclModuleInfoList = class(TObjectList)
  private
    FDynamicBuild: Boolean;
    FSystemModulesOnly: Boolean;
    FRefCount: Integer;
    function GetItems(Index: Integer): TJclModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TJclModuleInfo;
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TJclModuleInfo;
  public
    constructor Create(ADynamicBuild, ASystemModulesOnly: Boolean);
    function AddModule(Module: HMODULE; SystemModule: Boolean): Boolean;
    function IsSystemModuleAddress(Addr: Pointer): Boolean;
    function IsValidModuleAddress(Addr: Pointer): Boolean;
    property DynamicBuild: Boolean read FDynamicBuild;
    property Items[Index: Integer]: TJclModuleInfo read GetItems;
    property ModuleFromAddress[Addr: Pointer]: TJclModuleInfo read GetModuleFromAddress;
  end;

function JclValidateModuleAddress(Addr: Pointer): Boolean;

// MAP file abstract parser
type
  PJclMapAddress = ^TJclMapAddress;
  TJclMapAddress = packed record
    Segment: Word;
    Offset: TJclAddr;
  end;

  PJclMapString = PAnsiChar;

  TJclAbstractMapParser = class(TObject)
  private
    FLinkerBug: Boolean;
    FLinkerBugUnitName: PJclMapString;
    FStream: TJclFileMappingStream;
    function GetLinkerBugUnitName: string;
  protected
    FModule: HMODULE;
    FLastUnitName: PJclMapString;
    FLastUnitFileName: PJclMapString;
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); virtual; abstract;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); virtual; abstract;
    function CanHandlePublicsByName: Boolean; virtual; abstract;
    function CanHandlePublicsByValue: Boolean; virtual; abstract;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); virtual; abstract;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); virtual; abstract;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); overload; virtual;
    constructor Create(const MapFileName: TFileName); overload;
    destructor Destroy; override;
    procedure Parse;
    class function MapStringToFileName(MapString: PJclMapString): string;
    class function MapStringToModuleName(MapString: PJclMapString): string;
    class function MapStringToStr(MapString: PJclMapString; IgnoreSpaces: Boolean = False): string;
    property LinkerBug: Boolean read FLinkerBug;
    property LinkerBugUnitName: string read GetLinkerBugUnitName;
    property Stream: TJclFileMappingStream read FStream;
  end;

  // MAP file parser
  TJclMapClassTableEvent = procedure(Sender: TObject; const Address: TJclMapAddress; Len: Integer; const SectionName, GroupName: string) of object;
  TJclMapSegmentEvent = procedure(Sender: TObject; const Address: TJclMapAddress; Len: Integer; const GroupName, UnitName: string) of object;
  TJclMapPublicsEvent = procedure(Sender: TObject; const Address: TJclMapAddress; const Name: string) of object;
  TJclMapLineNumberUnitEvent = procedure(Sender: TObject; const UnitName, UnitFileName: string) of object;
  TJclMapLineNumbersEvent = procedure(Sender: TObject; LineNumber: Integer; const Address: TJclMapAddress) of object;

  TJclMapParser = class(TJclAbstractMapParser)
  private
    FOnClassTable: TJclMapClassTableEvent;
    FOnLineNumbers: TJclMapLineNumbersEvent;
    FOnLineNumberUnit: TJclMapLineNumberUnitEvent;
    FOnPublicsByValue: TJclMapPublicsEvent;
    FOnPublicsByName: TJclMapPublicsEvent;
    FOnSegmentItem: TJclMapSegmentEvent;
  protected
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    function CanHandlePublicsByName: Boolean; override;
    function CanHandlePublicsByValue: Boolean; override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
  public
    property OnClassTable: TJclMapClassTableEvent read FOnClassTable write FOnClassTable;
    property OnSegment: TJclMapSegmentEvent read FOnSegmentItem write FOnSegmentItem;
    property OnPublicsByName: TJclMapPublicsEvent read FOnPublicsByName write FOnPublicsByName;
    property OnPublicsByValue: TJclMapPublicsEvent read FOnPublicsByValue write FOnPublicsByValue;
    property OnLineNumberUnit: TJclMapLineNumberUnitEvent read FOnLineNumberUnit write FOnLineNumberUnit;
    property OnLineNumbers: TJclMapLineNumbersEvent read FOnLineNumbers write FOnLineNumbers;
  end;

  TJclMapStringCache = record
    CachedValue: string;
    RawValue: PJclMapString;
    TLS: Boolean;
  end;

  // MAP file scanner
  PJclMapSegmentClass = ^TJclMapSegmentClass;
  TJclMapSegmentClass = record
    Segment: Word; // segment ID
    Start: DWORD;  // start as in the map file
    Addr: DWORD;   // start as in process memory
    VA: DWORD;     // position relative to module base adress
    Len: DWORD;    // segment length
    SectionName: TJclMapStringCache;
    GroupName: TJclMapStringCache;
  end;

  PJclMapSegment = ^TJclMapSegment;
  TJclMapSegment = record
    Segment: Word;
    StartVA: DWORD; // VA relative to (module base address + $10000)
    EndVA: DWORD;
    UnitName: TJclMapStringCache;
  end;

  PJclMapProcName = ^TJclMapProcName;
  TJclMapProcName = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    ProcName: TJclMapStringCache;
  end;

  PJclMapLineNumber = ^TJclMapLineNumber;
  TJclMapLineNumber = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    LineNumber: Integer;
    UnitName: PJclMapString;
  end;

  TJclMapScanner = class(TJclAbstractMapParser)
  private
    FSegmentClasses: array of TJclMapSegmentClass;
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclMapProcName;
    FSegments: array of TJclMapSegment;
    FSourceNames: array of TJclMapProcName;
    FLineNumbersCnt: Integer;
    FLineNumberErrors: Integer;
    FNewUnitFileName: PJclMapString;
    FCurrentUnitName: PJclMapString;
    FProcNamesCnt: Integer;
    FSegmentCnt: Integer;
    FLastAccessedSegementIndex: Integer;
    function IndexOfSegment(Addr: DWORD): Integer;
  protected
    function MAPAddrToVA(const Addr: DWORD): DWORD;
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    function CanHandlePublicsByName: Boolean; override;
    function CanHandlePublicsByValue: Boolean; override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;
    procedure Scan;
    function GetLineNumberByIndex(Index: Integer): TJCLMapLineNumber;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); override;

    class function MapStringCacheToFileName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToModuleName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToStr(var MapString: TJclMapStringCache; IgnoreSpaces: Boolean = False): string;

    // Addr are virtual addresses relative to (module base address + $10000)
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer; overload;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function ProcNameFromAddr(Addr: DWORD): string; overload;
    function ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string; overload;
    function SourceNameFromAddr(Addr: DWORD): string;
    function VAFromUnitAndProcName(const UnitName, ProcName: string): DWORD;
    property LineNumberErrors: Integer read FLineNumberErrors;
    property LineNumbersCnt: Integer read FLineNumbersCnt;
    property LineNumberByIndex[Index: Integer]: TJclMapLineNumber read GetLineNumberByIndex;
  end;

type
  PJclDbgHeader = ^TJclDbgHeader;
  TJclDbgHeader = packed record
    Signature: DWORD;
    Version: Byte;
    Units: Integer;
    SourceNames: Integer;
    Symbols: Integer;
    LineNumbers: Integer;
    Words: Integer;
    ModuleName: Integer;
    CheckSum: Integer;
    CheckSumValid: Boolean;
  end;

  TJclBinDebugGenerator = class(TJclMapScanner)
  private
    FDataStream: TMemoryStream;
    FMapFileName: TFileName;
  protected
    procedure CreateData;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); override;
    destructor Destroy; override;
    function CalculateCheckSum: Boolean;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TJclBinDbgNameCache = record
    Addr: DWORD;
    FirstWord: Integer;
    SecondWord: Integer;
    Text: string;
  end;

  TJclBinDebugScanner = class(TObject)
  private
    FCacheData: Boolean;
    FCacheProcNames: Boolean;
    FStream: TCustomMemoryStream;
    FValidFormat: Boolean;
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclBinDbgNameCache;
    function GetModuleName: string;
  protected
    procedure CacheLineNumbers;
    procedure CacheProcNames;
    procedure CheckFormat;
    function DataToStr(A: Integer): string;
    function MakePtr(A: Integer): Pointer;
    class function ReadValue(var P: Pointer; var Value: Integer): Boolean; {$IFDEF SUPPORTS_STATIC}static;{$ENDIF}
  public
    constructor Create(AStream: TCustomMemoryStream; CacheData, CacheProcNames: Boolean);
    function IsModuleNameValid(const Name: TFileName): Boolean;
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer; overload;
    function ProcNameFromAddr(Addr: DWORD): string; overload;
    function ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string; overload;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function SourceNameFromAddr(Addr: DWORD): string;
    property ModuleName: string read GetModuleName;
    property ValidFormat: Boolean read FValidFormat;
    function VAFromUnitAndProcName(const UnitName, ProcName: string): DWORD;
  end;

function ConvertMapFileToJdbgFile(const MapFileName: TFileName): Boolean; overload;
function ConvertMapFileToJdbgFile(const MapFileName: TFileName; out LinkerBugUnit: string;
  out LineNumberErrors: Integer): Boolean; overload;
function ConvertMapFileToJdbgFile(const MapFileName: TFileName; out LinkerBugUnit: string;
  out LineNumberErrors, MapFileSize, JdbgFileSize: Integer): Boolean; overload;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName,
  MapFileName: TFileName; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize: Integer): Boolean; overload;
function InsertDebugDataIntoExecutableFile(const ExecutableFileName,
  MapFileName: TFileName; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean; overload;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize: Integer): Boolean; overload;
function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean; overload;

// Source Locations
type
  TJclDebugInfoSource = class;

  PJclLocationInfo = ^TJclLocationInfo;
  TJclLocationInfo = record
    Address: Pointer;               // Error address
    UnitName: string;               // Name of Delphi unit
    ProcedureName: string;          // Procedure name
    OffsetFromProcName: Integer;    // Offset from Address to ProcedureName symbol location
    LineNumber: Integer;            // Line number
    OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
    SourceName: string;             // Module file name
    DebugInfo: TJclDebugInfoSource; // Location object
    BinaryFileName: string;         // Name of the binary file containing the symbol
  end;

  TJclLocationInfoExValues = set of (lievLocationInfo, lievProcedureStartLocationInfo, lievUnitVersionInfo);

  TJclCustomLocationInfoList = class;

  TJclLocationInfoListOptions = set of (liloAutoGetAddressInfo, liloAutoGetLocationInfo, liloAutoGetUnitVersionInfo);

  TJclLocationInfoEx = class(TPersistent)
  private
    FAddress: Pointer;
    FBinaryFileName: string;
    FDebugInfo: TJclDebugInfoSource;
    FLineNumber: Integer;
    FLineNumberOffsetFromProcedureStart: Integer;
    FModuleName: string;
    FOffsetFromLineNumber: Integer;
    FOffsetFromProcName: Integer;
    FParent: TJclCustomLocationInfoList;
    FProcedureName: string;
    FSourceName: string;
    FSourceUnitName: string;
    FUnitVersionDateTime: TDateTime;
    FUnitVersionExtra: string;
    FUnitVersionLogPath: string;
    FUnitVersionRCSfile: string;
    FUnitVersionRevision: string;
    FVAddress: Pointer;
    FValues: TJclLocationInfoExValues;
    procedure Fill(AOptions: TJclLocationInfoListOptions);
    function GetAsString: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AParent: TJclCustomLocationInfoList; Address: Pointer);
    procedure Clear; virtual;
    property Address: Pointer read FAddress write FAddress;
    property AsString: string read GetAsString;
    property BinaryFileName: string read FBinaryFileName write FBinaryFileName;
    property DebugInfo: TJclDebugInfoSource read FDebugInfo write FDebugInfo;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property LineNumberOffsetFromProcedureStart: Integer read FLineNumberOffsetFromProcedureStart write FLineNumberOffsetFromProcedureStart;
    property ModuleName: string read FModuleName write FModuleName;
    property OffsetFromLineNumber: Integer read FOffsetFromLineNumber write FOffsetFromLineNumber;
    property OffsetFromProcName: Integer read FOffsetFromProcName write FOffsetFromProcName;
    property ProcedureName: string read FProcedureName write FProcedureName;
    property SourceName: string read FSourceName write FSourceName;
    { this is equal to TJclLocationInfo.UnitName, but has been renamed because
      UnitName is a class function in TObject since Delphi 2009 }
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property UnitVersionDateTime: TDateTime read FUnitVersionDateTime write FUnitVersionDateTime;
    property UnitVersionExtra: string read FUnitVersionExtra write FUnitVersionExtra;
    property UnitVersionLogPath: string read FUnitVersionLogPath write FUnitVersionLogPath;
    property UnitVersionRCSfile: string read FUnitVersionRCSfile write FUnitVersionRCSfile;
    property UnitVersionRevision: string read FUnitVersionRevision write FUnitVersionRevision;
    property VAddress: Pointer read FVAddress write FVAddress;
    property Values: TJclLocationInfoExValues read FValues write FValues;
  end;

  TJclLocationInfoClass = class of TJclLocationInfoEx;

  TJclCustomLocationInfoListClass = class of TJclCustomLocationInfoList;

  TJclCustomLocationInfoList = class(TPersistent)
  protected
    FItemClass: TJclLocationInfoClass;
    FItems: TObjectList;
    FOptions: TJclLocationInfoListOptions;
    function GetAsString: string;
    function GetCount: Integer;
    function InternalAdd(Addr: Pointer): TJclLocationInfoEx;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddStackInfoList(AStackInfoList: TObject);
    procedure Clear;
    property AsString: string read GetAsString;
    property Count: Integer read GetCount;
    property Options: TJclLocationInfoListOptions read FOptions write FOptions;
  end;

  TJclLocationInfoList = class(TJclCustomLocationInfoList)
  private
    function GetItems(AIndex: Integer): TJclLocationInfoEx;
  public
    constructor Create; override;
    function Add(Addr: Pointer): TJclLocationInfoEx;
    property Items[AIndex: Integer]: TJclLocationInfoEx read GetItems; default;
  end;

  TJclDebugInfoSource = class(TObject)
  private
    FModule: HMODULE;
    FModuleCodeSize: SizeInt;
    function GetFileName: TFileName;
  protected
    function VAFromAddr(const Addr: Pointer): DWORD; virtual;
    function AddrFromVA(const VA: DWORD): Pointer; virtual;
  public
    constructor Create(AModule: HMODULE); virtual;
    function InitializeSource: Boolean; virtual; abstract;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; virtual; abstract;
    function GetAddress(const UnitName, ProcName: string): Pointer; virtual; abstract;
    property Module: HMODULE read FModule;
    property FileName: TFileName read GetFileName;
    property ModuleCodeSize: SizeInt read FModuleCodeSize;
  end;

  TJclDebugInfoSourceClass = class of TJclDebugInfoSource;

  TJclDebugInfoList = class(TObjectList)
  private
    function GetItemFromModule(const Module: HMODULE): TJclDebugInfoSource;
    function GetItems(Index: Integer): TJclDebugInfoSource;
  protected
    function CreateDebugInfo(const Module: HMODULE): TJclDebugInfoSource;
  public
    class procedure RegisterDebugInfoSource(
      const InfoSourceClass: TJclDebugInfoSourceClass);
    class procedure UnRegisterDebugInfoSource(
      const InfoSourceClass: TJclDebugInfoSourceClass);
    class procedure RegisterDebugInfoSourceFirst(
      const InfoSourceClass: TJclDebugInfoSourceClass);
    class procedure NeedInfoSourceClassList;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
    property ItemFromModule[const Module: HMODULE]: TJclDebugInfoSource read GetItemFromModule;
    property Items[Index: Integer]: TJclDebugInfoSource read GetItems;
  end;

  // Various source location implementations
  TJclDebugInfoMap = class(TJclDebugInfoSource)
  private
    FScanner: TJclMapScanner;
  public
    destructor Destroy; override;
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
    function GetAddress(const UnitName, ProcName: string): Pointer; override;
  end;

  TJclDebugInfoBinary = class(TJclDebugInfoSource)
  private
    FScanner: TJclBinDebugScanner;
    FStream: TCustomMemoryStream;
  public
    destructor Destroy; override;
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
    function GetAddress(const UnitName, ProcName: string): Pointer; override;
  end;

  TJclDebugInfoExports = class(TJclDebugInfoSource)
  private
    {$IFDEF BORLAND}
    FImage: TJclPeBorImage;
    {$ENDIF BORLAND}
    {$IFDEF FPC}
    FImage: TJclPeImage;
    {$ENDIF FPC}
    function IsAddressInThisExportedFunction(Addr: PByteArray; FunctionStartAddr: TJclAddr): Boolean;
  public
    destructor Destroy; override;
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
    function GetAddress(const UnitName, ProcName: string): Pointer; override;
  end;

  {$IFDEF BORLAND}
  {$IFNDEF WINSCP}
  TJclDebugInfoTD32 = class(TJclDebugInfoSource)
  private
    FImage: TJclPeBorTD32Image;
  public
    destructor Destroy; override;
    function InitializeSource: Boolean; override;
    procedure GenerateUnmangledNames;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
    function GetAddress(const UnitName, ProcName: string): Pointer; override;
  end;
  {$ENDIF ~WINSCP}
  {$ENDIF BORLAND}

  TJclDebugInfoSymbols = class(TJclDebugInfoSource)
  public
    class function LoadDebugFunctions: Boolean;
    class function UnloadDebugFunctions: Boolean;
    class function InitializeDebugSymbols: Boolean;
    class function CleanupDebugSymbols: Boolean;
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
    function GetAddress(const UnitName, ProcName: string): Pointer; override;
  end;

// Source location functions
function Caller(Level: Integer = 0; FastStackWalk: Boolean = False): Pointer;

procedure BeginGetLocationInfoCache;
procedure EndGetLocationInfoCache;

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo; overload;
function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; overload;
function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False;
  IncludeVAddress: Boolean = False): string;
function DebugInfoAvailable(const Module: HMODULE): Boolean;
procedure ClearLocationData;

function FileByLevel(const Level: Integer = 0): string;
function ModuleByLevel(const Level: Integer = 0): string;
function ProcByLevel(const Level: Integer = 0; OnlyProcedureName: boolean =false): string;
function LineByLevel(const Level: Integer = 0): Integer;
function MapByLevel(const Level: Integer; var File_, Module_, Proc_: string; var Line_: Integer): Boolean;

function FileOfAddr(const Addr: Pointer): string;
function ModuleOfAddr(const Addr: Pointer): string;
function ProcOfAddr(const Addr: Pointer): string;
function LineOfAddr(const Addr: Pointer): Integer;
function MapOfAddr(const Addr: Pointer; var File_, Module_, Proc_: string; var Line_: Integer): Boolean;

function ExtractClassName(const ProcedureName: string): string;
function ExtractMethodName(const ProcedureName: string): string;

// Original function names, deprecated will be removed in V2.0; do not use!

function __FILE__(const Level: Integer = 0): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __MODULE__(const Level: Integer = 0): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __PROC__(const Level: Integer  = 0): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __LINE__(const Level: Integer = 0): Integer; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __MAP__(const Level: Integer; var _File, _Module, _Proc: string; var _Line: Integer): Boolean; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __FILE_OF_ADDR__(const Addr: Pointer): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __MODULE_OF_ADDR__(const Addr: Pointer): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __PROC_OF_ADDR__(const Addr: Pointer): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __LINE_OF_ADDR__(const Addr: Pointer): Integer; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}

// Stack info routines base list
type
  TJclStackBaseList = class(TObjectList)
  private
    FThreadID: DWORD;
    FTimeStamp: TDateTime;
  protected
    FOnDestroy: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    property ThreadID: DWORD read FThreadID;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

// Stack info routines
type
  PDWORD_PTRArray = ^TDWORD_PTRArray;
  TDWORD_PTRArray = array [0..(MaxInt - $F) div SizeOf(DWORD_PTR)] of DWORD_PTR;
  {$IFNDEF FPC}
  PDWORD_PTR = ^DWORD_PTR;
  {$ENDIF ~FPC}

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: TJclAddr;
    CallerAddr: TJclAddr;
  end;

  PStackInfo = ^TStackInfo;
  TStackInfo = record
    CallerAddr: TJclAddr;
    Level: Integer;
    CallerFrame: TJclAddr;
    DumpSize: DWORD;
    ParamSize: DWORD;
    ParamPtr: PDWORD_PTRArray;
    case Integer of
      0:
        (StackFrame: PStackFrame);
      1:
        (DumpPtr: PJclByteArray);
  end;

  TJclStackInfoItem = class(TObject)
  private
    FStackInfo: TStackInfo;
    function GetCallerAddr: Pointer;
    function GetLogicalAddress: TJclAddr;
  public
    property CallerAddr: Pointer read GetCallerAddr;
    property LogicalAddress: TJclAddr read GetLogicalAddress;
    property StackInfo: TStackInfo read FStackInfo;
  end;

  TJclStackInfoList = class(TJclStackBaseList)
  private
    FIgnoreLevels: Integer;
    TopOfStack: TJclAddr;
    BaseOfStack: TJclAddr;
    FStackData: PPointer;
    FFramePointer: Pointer;
    FModuleInfoList: TJclModuleInfoList;
    FCorrectOnAccess: Boolean;
    FSkipFirstItem: Boolean;
    FDelayedTrace: Boolean;
    FInStackTracing: Boolean;
    FRaw: Boolean;
    FStackOffset: Int64;
    {$IFDEF CPU64}
    procedure CaptureBackTrace;
    {$ENDIF CPU64}
    function GetItems(Index: Integer): TJclStackInfoItem;
    function NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
    procedure StoreToList(const StackInfo: TStackInfo);
    procedure TraceStackFrames;
    procedure TraceStackRaw;
    {$IFDEF CPU32}
    procedure DelayStoreStack;
    {$ENDIF CPU32}
    function ValidCallSite(CodeAddr: TJclAddr; out CallInstructionSize: Cardinal): Boolean;
    function ValidStackAddr(StackAddr: TJclAddr): Boolean;
    function GetCount: Integer;
    procedure CorrectOnAccess(ASkipFirstItem: Boolean);
  public
    constructor Create(ARaw: Boolean; AIgnoreLevels: Integer;
      AFirstCaller: Pointer); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: Integer;
      AFirstCaller: Pointer; ADelayedTrace: Boolean); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: Integer;
      AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack: Pointer); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: Integer;
      AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack, ATopOfStack: Pointer); overload;
    destructor Destroy; override;
    procedure ForceStackTracing;
    procedure AddToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
      IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False;
      IncludeVAddress: Boolean = False);
    property DelayedTrace: Boolean read FDelayedTrace;
    property Items[Index: Integer]: TJclStackInfoItem read GetItems; default;
    property IgnoreLevels: Integer read FIgnoreLevels;
    property Count: Integer read GetCount;
    property Raw: Boolean read FRaw;
  end;

{$IFDEF WINSCP}
procedure DoExceptionStackTrace(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean;
  BaseOfStack: Pointer);
procedure DoExceptFrameTrace;
{$ENDIF}

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList; overload;
function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean): TJclStackInfoList; overload;
function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean; BaseOfStack: Pointer): TJclStackInfoList; overload;
function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean; BaseOfStack, TopOfStack: Pointer): TJclStackInfoList; overload;

function JclCreateThreadStackTrace(Raw: Boolean; const ThreadHandle: THandle): TJclStackInfoList;
function JclCreateThreadStackTraceFromID(Raw: Boolean; ThreadID: DWORD): TJclStackInfoList;

function JclLastExceptStackList: TJclStackInfoList;
function JclLastExceptStackListToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False;
  IncludeVAddress: Boolean = False): Boolean;

function JclGetExceptStackList(ThreadID: DWORD): TJclStackInfoList;
function JclGetExceptStackListToStrings(ThreadID: DWORD; Strings: TStrings;
  IncludeModuleName: Boolean = False; IncludeAddressOffset: Boolean = False;
  IncludeStartProcLineOffset: Boolean = False; IncludeVAddress: Boolean = False): Boolean;

// helper function for DUnit runtime memory leak check
procedure JclClearGlobalStackData;

// Exception frame info routines
type
  PJmpInstruction = ^TJmpInstruction;
  TJmpInstruction = packed record // from System.pas
    OpCode: Byte;
    Distance: Longint;
  end;

  TExcDescEntry = record // from System.pas
    VTable: Pointer;
    Handler: Pointer;
  end;

  PExcDesc = ^TExcDesc;
  TExcDesc = packed record // from System.pas
    JMP: TJmpInstruction;
    case Integer of
      0:
        (Instructions: array [0..0] of Byte);
      1:
       (Cnt: Integer;
        ExcTab: array [0..0] of TExcDescEntry);
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame =  record // from System.pas
    Next: PExcFrame;
    Desc: PExcDesc;
    FramePointer: Pointer;
    case Integer of
      0:
        ();
      1:
        (ConstructedObject: Pointer);
      2:
        (SelfOfMethod: Pointer);
  end;

  PJmpTable = ^TJmpTable;
  TJmpTable = packed record
    OPCode: Word; // FF 25 = JMP DWORD PTR [$xxxxxxxx], encoded as $25FF
    Ptr: Pointer;
  end;

  TExceptFrameKind =
    (efkUnknown, efkFinally, efkAnyException, efkOnException, efkAutoException);

  TJclExceptFrame = class(TObject)
  private
    FFrameKind: TExceptFrameKind;
    FFrameLocation: Pointer;
    FCodeLocation: Pointer;
    FExcTab: array of TExcDescEntry;
  protected
    procedure AnalyseExceptFrame(AExcDesc: PExcDesc);
  public
    constructor Create(AFrameLocation: Pointer; AExcDesc: PExcDesc);
    function Handles(ExceptObj: TObject): Boolean;
    function HandlerInfo(ExceptObj: TObject; out HandlerAt: Pointer): Boolean;
    property CodeLocation: Pointer read FCodeLocation;
    property FrameLocation: Pointer read FFrameLocation;
    property FrameKind: TExceptFrameKind read FFrameKind;
  end;

  TJclExceptFrameList = class(TJclStackBaseList)
  private
    FIgnoreLevels: Integer;
    function GetItems(Index: Integer): TJclExceptFrame;
  protected
    function AddFrame(AFrame: PExcFrame): TJclExceptFrame;
  public
    constructor Create(AIgnoreLevels: Integer);
    procedure TraceExceptionFrames;
    property Items[Index: Integer]: TJclExceptFrame read GetItems;
    property IgnoreLevels: Integer read FIgnoreLevels write FIgnoreLevels;
  end;

function JclCreateExceptFrameList(AIgnoreLevels: Integer): TJclExceptFrameList;
function JclLastExceptFrameList: TJclExceptFrameList;
function JclGetExceptFrameList(ThreadID: DWORD): TJclExceptFrameList;

function JclStartExceptionTracking: Boolean;
function JclStopExceptionTracking: Boolean;
function JclExceptionTrackingActive: Boolean;

function JclTrackExceptionsFromLibraries: Boolean;

// Thread exception tracking support
type
  TJclDebugThread = class(TThread)
  private
    FSyncException: TObject;
    FThreadName: string;
    procedure DoHandleException;
    function GetThreadInfo: string;
  protected
    procedure DoNotify;
    procedure DoSyncHandleException; dynamic;
    procedure HandleException(Sender: TObject = nil);
  public
    constructor Create(ASuspended: Boolean; const AThreadName: string = '');
    destructor Destroy; override;
    property SyncException: TObject read FSyncException;
    property ThreadInfo: string read GetThreadInfo;
    property ThreadName: string read FThreadName;
  end;

  TJclDebugThreadNotifyEvent = procedure(Thread: TJclDebugThread) of object;
  TJclThreadIDNotifyEvent = procedure(ThreadID: DWORD) of object;

  TJclDebugThreadList = class(TObject)
  private
    FList: TObjectList;
    FLock: TJclCriticalSection;
    FReadLock: TJclCriticalSection;
    FRegSyncThreadID: DWORD;
    FSaveCreationStack: Boolean;
    FUnregSyncThreadID: DWORD;
    FOnSyncException: TJclDebugThreadNotifyEvent;
    FOnThreadRegistered: TJclThreadIDNotifyEvent;
    FOnThreadUnregistered: TJclThreadIDNotifyEvent;
    function GetThreadClassNames(ThreadID: DWORD): string;
    function GetThreadInfos(ThreadID: DWORD): string;
    function GetThreadNames(ThreadID: DWORD): string;
    procedure DoSyncThreadRegistered;
    procedure DoSyncThreadUnregistered;
    function GetThreadCreationTime(ThreadID: DWORD): TDateTime;
    function GetThreadHandle(Index: Integer): THandle;
    function GetThreadID(Index: Integer): DWORD;
    function GetThreadIDCount: Integer;
    function GetThreadParentID(ThreadID: DWORD): DWORD;
    function GetThreadValues(ThreadID: DWORD; Index: Integer): string;
    function IndexOfThreadID(ThreadID: DWORD): Integer;
  protected
    procedure DoSyncException(Thread: TJclDebugThread);
    procedure DoThreadRegistered(Thread: TThread);
    procedure DoThreadUnregistered(Thread: TThread);
    procedure InternalRegisterThread(Thread: TThread; ThreadID: DWORD; const ThreadName: string);
    procedure InternalUnregisterThread(Thread: TThread; ThreadID: DWORD);
  public
    constructor Create;
    destructor Destroy; override;
    function AddStackListToLocationInfoList(ThreadID: DWORD; AList: TJclLocationInfoList): Boolean;
    procedure RegisterThread(Thread: TThread; const ThreadName: string);
    procedure RegisterThreadID(AThreadID: DWORD; const ThreadName: string = '');
    procedure UnregisterThread(Thread: TThread);
    procedure UnregisterThreadID(AThreadID: DWORD);
    property Lock: TJclCriticalSection read FLock;
    //property ThreadClassNames[ThreadID: DWORD]: string index 1 read GetThreadValues;
    property SaveCreationStack: Boolean read FSaveCreationStack write FSaveCreationStack;
    property ThreadClassNames[ThreadID: DWORD]: string read GetThreadClassNames;
    property ThreadCreationTime[ThreadID: DWORD]: TDateTime read GetThreadCreationTime;
    property ThreadHandles[Index: Integer]: THandle read GetThreadHandle;
    property ThreadIDs[Index: Integer]: DWORD read GetThreadID;
    property ThreadIDCount: Integer read GetThreadIDCount;
    //property ThreadInfos[ThreadID: DWORD]: string index 2 read GetThreadValues;
    property ThreadInfos[ThreadID: DWORD]: string read GetThreadInfos;
    //property ThreadNames[ThreadID: DWORD]: string index 0 read GetThreadValues;
    property ThreadNames[ThreadID: DWORD]: string read GetThreadNames;
    property ThreadParentIDs[ThreadID: DWORD]: DWORD read GetThreadParentID;
    property OnSyncException: TJclDebugThreadNotifyEvent read FOnSyncException write FOnSyncException;
    property OnThreadRegistered: TJclThreadIDNotifyEvent read FOnThreadRegistered write FOnThreadRegistered;
    property OnThreadUnregistered: TJclThreadIDNotifyEvent read FOnThreadUnregistered write FOnThreadUnregistered;
  end;

  TJclDebugThreadInfo = class(TObject)
  private
    FCreationTime: TDateTime;
    FParentThreadID: DWORD;
    FStackList: TJclStackInfoList;
    FThreadClassName: string;
    FThreadID: DWORD;
    FThreadHandle: THandle;
    FThreadName: string;
  public
    constructor Create(AParentThreadID, AThreadID: DWORD; AStack: Boolean);
    destructor Destroy; override;
    property CreationTime: TDateTime read FCreationTime;
    property ParentThreadID: DWORD read FParentThreadID;
    property StackList: TJclStackInfoList read FStackList;
    property ThreadClassName: string read FThreadClassName write FThreadClassName;
    property ThreadID: DWORD read FThreadID;
    property ThreadHandle: THandle read FThreadHandle write FThreadHandle;
    property ThreadName: string read FThreadName write FThreadName;
  end;

  TJclThreadInfoOptions = set of (tioIsMainThread, tioName, tioCreationTime, tioParentThreadID, tioStack, tioCreationStack);

  TJclCustomThreadInfo = class(TPersistent)
  protected
    FCreationTime: TDateTime;
    FCreationStack: TJclCustomLocationInfoList;
    FName: string;
    FParentThreadID: DWORD;
    FStack: TJclCustomLocationInfoList;
    FThreadID: DWORD;
    FValues: TJclThreadInfoOptions;
    procedure AssignTo(Dest: TPersistent); override;
    function GetStackClass: TJclCustomLocationInfoListClass; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property Name: string read FName write FName;
    property ParentThreadID: DWORD read FParentThreadID write FParentThreadID;
    property ThreadID: DWORD read FThreadID write FThreadID;
    property Values: TJclThreadInfoOptions read FValues write FValues;
  end;

  TJclThreadInfo = class(TJclCustomThreadInfo)
  private
    function GetAsString: string;
    procedure InternalFill(AThreadHandle: THandle; AThreadID: DWORD; AGatherOptions: TJclThreadInfoOptions; AExceptThread: Boolean);
    function GetStack(const AIndex: Integer): TJclLocationInfoList;
  protected
    function GetStackClass: TJclCustomLocationInfoListClass; override;
  public
    procedure Fill(AThreadHandle: THandle; AThreadID: DWORD; AGatherOptions: TJclThreadInfoOptions);
    procedure FillFromExceptThread(AGatherOptions: TJclThreadInfoOptions);
    property AsString: string read GetAsString;
    property CreationStack: TJclLocationInfoList index 1 read GetStack;
    property Stack: TJclLocationInfoList index 2 read GetStack;
  end;

  TJclThreadInfoList = class(TPersistent)
  private
    FGatherOptions: TJclThreadInfoOptions;
    FItems: TObjectList;
    function GetAsString: string;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJclThreadInfo;
    procedure InternalGather(AIncludeThreadIDs, AExcludeThreadIDs: array of DWORD);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJclThreadInfo;
    procedure Clear;
    procedure Gather(AExceptThreadID: DWORD);
    procedure GatherExclude(AThreadIDs: array of DWORD);
    procedure GatherInclude(AThreadIDs: array of DWORD);
    property AsString: string read GetAsString;
    property Count: Integer read GetCount;
    property GatherOptions: TJclThreadInfoOptions read FGatherOptions write FGatherOptions;
    property Items[AIndex: Integer]: TJclThreadInfo read GetItems; default;
  end;

function JclDebugThreadList: TJclDebugThreadList;

function JclHookThreads: Boolean;
function JclUnhookThreads: Boolean;
function JclThreadsHooked: Boolean;

// Miscellanuous
{$IFDEF MSWINDOWS}
{$IFNDEF WINSCP}
function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;
{$ENDIF ~WINSCP}
function IsDebuggerAttached: Boolean;
function IsHandleValid(Handle: THandle): Boolean;
{$ENDIF MSWINDOWS}

{$IFDEF SUPPORTS_EXTSYM}
{$EXTERNALSYM __FILE__}
{$EXTERNALSYM __LINE__}
{$ENDIF SUPPORTS_EXTSYM}

const
  EnvironmentVarNtSymbolPath = '_NT_SYMBOL_PATH';                    // do not localize
  EnvironmentVarAlternateNtSymbolPath = '_NT_ALTERNATE_SYMBOL_PATH'; // do not localize
  MaxStackTraceItems = 4096;

// JCL binary debug data generator and scanner
const
  JclDbgDataSignature = $4742444A; // JDBG
  JclDbgDataResName   = AnsiString('JCLDEBUG'); // do not localize
  JclDbgHeaderVersion = 1; // JCL 1.11 and 1.20

  JclDbgFileExtension = '.jdbg'; // do not localize
  JclMapFileExtension = '.map';  // do not localize
  DrcFileExtension = '.drc';  // do not localize

// Global exceptional stack tracker enable routines and variables
type
  TJclStackTrackingOption =
    (stStack, stExceptFrame, stRawMode, stAllModules, stStaticModuleList,
     stDelayedTrace, stTraceAllExceptions, stMainThreadOnly, stDisableIfDebuggerAttached
     {$IFDEF HAS_EXCEPTION_STACKTRACE}
     // Resolves the Exception.Stacktrace string when the exception is raised. This is more
     // exact if modules are unloaded before the delayed resolving happens, but it slows down
     // the exception handling if no stacktrace is needed for the exception.
     , stImmediateExceptionStacktraceResolving
     {$ENDIF HAS_EXCEPTION_STACKTRACE}
     // stCleanRawStack does a deeper analysis of the callstack by evaluating the instructions
     // that manipulate the stack.
     // It removes many cases of false positives but may also remove valid entries if it runs
     // into a function that does non-standard stack pointer manipulation.
     , stCleanRawStack // experimental
    );
  TJclStackTrackingOptions = set of TJclStackTrackingOption;

  {$IFDEF HAS_EXCEPTION_STACKTRACE}
  TJclExceptionStacktraceOption = (
    estoIncludeModuleName,
    estoIncludeAdressOffset,
    estoIncludeStartProcLineOffset,
    estoIncludeVAddress
    );
  TJclExceptionStacktraceOptions = set of TJclExceptionStacktraceOption;
  {$ENDIF HAS_EXCEPTION_STACKTRACE}

var
  JclStackTrackingOptions: TJclStackTrackingOptions = [stStack];

  {$IFDEF HAS_EXCEPTION_STACKTRACE}
  // JclExceptionStacktraceOptions controls the Exception.Stacktrace string's format
  JclExceptionStacktraceOptions: TJclExceptionStacktraceOptions =
    [estoIncludeModuleName, estoIncludeAdressOffset, estoIncludeStartProcLineOffset, estoIncludeVAddress];
  {$ENDIF HAS_EXCEPTION_STACKTRACE}

  { JclDebugInfoSymbolPaths specifies a list of paths, separated by ';', in
    which the DebugInfoSymbol scanner should look for symbol information. }
  JclDebugInfoSymbolPaths: string = '';

// functions to add/remove exception classes to be ignored if StTraceAllExceptions is not set
procedure AddIgnoredException(const ExceptionClass: TClass);
procedure AddIgnoredExceptionByName(const AExceptionClassName: string);
procedure RemoveIgnoredException(const ExceptionClass: TClass);
procedure RemoveIgnoredExceptionByName(const AExceptionClassName: string);
function IsIgnoredException(const ExceptionClass: TClass): Boolean;
// function to add additional system modules to be included in the stack trace
procedure AddModule(const ModuleName: string);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.RTLConsts,
  System.Types, // for inlining TList.Remove
  {$IFDEF HAS_UNIT_CHARACTER}
  System.Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  {$IFDEF SUPPORTS_GENERICS}
  System.Generics.Collections,
  {$ENDIF SUPPORTS_GENERICS}
  {$ELSE ~HAS_UNITSCOPE}
  RTLConsts,
  {$IFDEF HAS_UNIT_CHARACTER}
  Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  {$IFDEF SUPPORTS_GENERICS}
  Generics.Collections,
  {$ENDIF SUPPORTS_GENERICS}
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  {$IFNDEF WINSCP}
  JclRegistry,
  {$ELSE}
  System.AnsiStrings,
  {$ENDIF ~WINSCP}
  {$ENDIF MSWINDOWS}
  JclHookExcept, JclAnsiStrings, JclStrings, JclSysInfo, JclSysUtils, JclWin32,
  {$IFNDEF WINSCP}JclStringConversions,{$ENDIF ~WINSCP} JclResources;

//=== Helper assembler routines ==============================================

const
  ModuleCodeOffset = $1000;

var
  HexMap: array[AnsiChar] of Byte;
  JclDebugFinalized: Boolean;
  GlobalStackListLiveCount: Integer;

procedure FreeJclDebugGlobals;
  forward;

{$STACKFRAMES OFF}

function GetFramePointer: Pointer;
asm
        {$IFDEF CPU32}
        MOV     EAX, EBP
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        MOV     RAX, RBP
        {$ENDIF CPU64}
end;

function GetStackPointer: Pointer;
asm
        {$IFDEF CPU32}
        MOV     EAX, ESP
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        MOV     RAX, RSP
        {$ENDIF CPU64}
end;

{$IFDEF CPU32}
function GetExceptionPointer: Pointer;
asm
        XOR     EAX, EAX
        MOV     EAX, FS:[EAX]
end;
{$ENDIF CPU32}

// Reference: Matt Pietrek, MSJ, Under the hood, on TIBs:
// http://www.microsoft.com/MSJ/archive/S2CE.HTM

function GetStackTop: TJclAddr;
asm
        {$IFDEF CPU32}
        MOV     EAX, FS:[0].NT_TIB32.StackBase
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        {$IFDEF DELPHI64_TEMPORARY}
        //TODO: check if the FS version doesn't work in general in 64-bit mode
        MOV     RAX, GS:[ABS 8]
        {$ELSE ~DELPHI64_TEMPORARY}
        MOV     RAX, FS:[0].NT_TIB64.StackBase
        {$ENDIF ~DELPHI64_TEMPORARY}
        {$ENDIF CPU64}
end;

{$IFDEF STACKFRAMES_ON}
{$STACKFRAMES ON}
{$ENDIF STACKFRAMES_ON}

//===  Diagnostics ===========================================================

procedure AssertKindOf(const ClassName: string; const Obj: TObject);
var
  C: TClass;
begin
  if not Obj.ClassNameIs(ClassName) then
  begin
    C := Obj.ClassParent;
    while (C <> nil) and (not C.ClassNameIs(ClassName)) do
      C := C.ClassParent;
    Assert(C <> nil);
  end;
end;

procedure AssertKindOf(const ClassType: TClass; const Obj: TObject);
begin
  Assert(Obj.InheritsFrom(ClassType));
end;

procedure TraceMsg(const Msg: string);
begin
  OutputDebugString(PChar(StrDoubleQuote(Msg)));
end;

{$IFNDEF WINSCP}
procedure TraceFmt(const Fmt: string; const Args: array of const);
begin
  OutputDebugString(PChar(Format(StrDoubleQuote(Fmt), Args)));
end;
{$ENDIF}

procedure TraceLoc(const Msg: string);
begin
  OutputDebugString(PChar(Format('%s:%u (%s) "%s"',
    [FileByLevel(1), LineByLevel(1), ProcByLevel(1), Msg])));
end;

procedure TraceLocFmt(const Fmt: string; const Args: array of const);
var
  S: string;
begin
  S := Format('%s:%u (%s) ', [FileByLevel(1), LineByLevel(1), ProcByLevel(1)]) +
    Format(StrDoubleQuote(Fmt), Args);
  OutputDebugString(PChar(S));
end;

//=== { TJclModuleInfoList } =================================================

constructor TJclModuleInfoList.Create(ADynamicBuild, ASystemModulesOnly: Boolean);
begin
  inherited Create(True);
  FDynamicBuild := ADynamicBuild;
  FSystemModulesOnly := ASystemModulesOnly;
  if not FDynamicBuild then
    BuildModulesList;
end;

function TJclModuleInfoList.AddModule(Module: HMODULE; SystemModule: Boolean): Boolean;
begin
  Result := not IsValidModuleAddress(Pointer(Module)) and
    (CreateItemForAddress(Pointer(Module), SystemModule) <> nil);
end;

{function SortByStartAddress(Item1, Item2: Pointer): Integer;
begin
  Result := INT_PTR(TJclModuleInfo(Item2).StartAddr) - INT_PTR(TJclModuleInfo(Item1).StartAddr);
end;}

procedure TJclModuleInfoList.BuildModulesList;
var
  List: TStringList;
  I: Integer;
  CurModule: PLibModule;
begin
  if FSystemModulesOnly then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      CreateItemForAddress(Pointer(CurModule.Instance), True);
      CurModule := CurModule.Next;
    end;
  end
  else
  begin
    List := TStringList.Create;
    try
      LoadedModulesList(List, GetCurrentProcessId, True);
      for I := 0 to List.Count - 1 do
        CreateItemForAddress(List.Objects[I], False);
    finally
      List.Free;
    end;
  end;
  //Sort(SortByStartAddress);
end;

function TJclModuleInfoList.CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TJclModuleInfo;
var
  Module: HMODULE;
  ModuleSize: DWORD;
begin
  Result := nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then
  begin
    ModuleSize := PeMapImgSize(Pointer(Module));
    if ModuleSize <> 0 then
    begin
      Result := TJclModuleInfo.Create;
      Result.FStartAddr := Pointer(Module);
      Result.FSize := ModuleSize;
      Result.FEndAddr := Pointer(Module + ModuleSize - 1);
      if SystemModule then
        Result.FSystemModule := True
      else
        Result.FSystemModule := IsSystemModule(Module);
    end;
  end;
  if Result <> nil then
    Add(Result);
end;

function TJclModuleInfoList.GetItems(Index: Integer): TJclModuleInfo;
begin
  Result := TJclModuleInfo(Get(Index));
end;

function TJclModuleInfoList.GetModuleFromAddress(Addr: Pointer): TJclModuleInfo;
var
  I: Integer;
  Item: TJclModuleInfo;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (TJclAddr(Item.StartAddr) <= TJclAddr(Addr)) and (TJclAddr(Item.EndAddr) > TJclAddr(Addr)) then
    begin
      Result := Item;
      Break;
    end;
  end;
  if DynamicBuild and (Result = nil) then
    Result := CreateItemForAddress(Addr, False);
end;

function TJclModuleInfoList.IsSystemModuleAddress(Addr: Pointer): Boolean;
var
  Item: TJclModuleInfo;
begin
  Item := ModuleFromAddress[Addr];
  Result := (Item <> nil) and Item.SystemModule;
end;

function TJclModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := ModuleFromAddress[Addr] <> nil;
end;

//=== { TJclAbstractMapParser } ==============================================

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create;
  FModule := Module;
  if FileExists(MapFileName) then
    FStream := TJclFileMappingStream.Create(MapFileName, fmOpenRead or fmShareDenyWrite);
end;

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName);
begin
  Create(MapFileName, 0);
end;

destructor TJclAbstractMapParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TJclAbstractMapParser.GetLinkerBugUnitName: string;
begin
  Result := MapStringToStr(FLinkerBugUnitName);
end;

class function TJclAbstractMapParser.MapStringToFileName(MapString: PJclMapString): string;
var
  PEnd: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> #0) and not (PEnd^ in ['=', #10, #13]) do
    Inc(PEnd);
  if (PEnd^ = '=') then
  begin
    while (PEnd >= MapString) and (PEnd^ <> ' ') do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd-1)^ = ' ') do
      Dec(PEnd);
  end;
  SetString(Result, MapString, PEnd - MapString);
end;

class function TJclAbstractMapParser.MapStringToModuleName(MapString: PJclMapString): string;
var
  PStart, PEnd, PExtension: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> #0) and not (PEnd^ in ['=', #10, #13]) do
    Inc(PEnd);
  if (PEnd^ = '=') then
  begin
    while (PEnd >= MapString) and (PEnd^ <> ' ') do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd-1)^ = ' ') do
      Dec(PEnd);
  end;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '.') and (PExtension^ <> '|') do
    Dec(PExtension);
  if (StrLICompA(PExtension, '.pas ', 5) = 0) or
     (StrLICompA(PExtension, '.obj ', 5) = 0) then
    PEnd := PExtension;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '|') and (PExtension^ <> '\') do
    Dec(PExtension);
  if PExtension >= MapString then
    PStart := PExtension + 1
  else
    PStart := MapString;
  SetString(Result, PStart, PEnd - PStart);
end;

class function TJclAbstractMapParser.MapStringToStr(MapString: PJclMapString;
  IgnoreSpaces: Boolean): string;
var
  P: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  if MapString^ = '(' then
  begin
    Inc(MapString);
    P := MapString;
    while (P^ <> #0) and not (P^ in [')', #10, #13]) do
      Inc(P);
  end
  else
  begin
    P := MapString;
    if IgnoreSpaces then
      while (P^ <> #0) and not (P^ in ['(', #10, #13]) do
        Inc(P)
    else
      while (P^ <> #0) and (P^ <> '(') and (P^ > ' ') do
        Inc(P);
  end;
  SetString(Result, MapString, P - MapString);
end;

function IsDecDigit(P: PJclMapString): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := False;
  case P^ of
    '0'..'9':
      Result := True;
  end;
end;

function SkipMapBlock(P, EndPos: PJclMapString): PJclMapString;
begin
  Result := P;
  while Result < EndPos do
  begin
    if not IsDecDigit(Result) then
      Break;
    Inc(Result);

    // Skip to the end of the line
    while Result < EndPos do
    begin
      case Result^ of
        #10, #13:
          Break;
      end;
      Inc(Result);
    end;
    // Skip WhiteSpaces
    while (Result < EndPos) and (Result^ <= ' ') do
      Inc(Result);
  end;
end;

function AnsiStrPosIdxLen(const SubStr, S: PAnsiChar; Len: Integer): Integer;
var
  I: Integer;
  SubStrLen: Integer;
  FirstCh: AnsiChar;
begin
  Result := 0;
  if Len = 0 then
    Exit;
  I := 0;
  FirstCh := SubStr[0];
  if FirstCh = #0 then
    Exit;
  SubStrLen := StrLenA(SubStr);
  while I < Len do
  begin
    while (I < Len) and (S[I] <> FirstCh) do
      Inc(I);
    if I = Len then
      Break;
    if StrLCompA(SubStr, @S[I], SubStrLen) = 0 then
    begin
      Result := I + 1;
      Exit;
    end;
    Inc(I);
  end;
end;

procedure TJclAbstractMapParser.Parse;
const
  TableHeader          : array [0..3] of PJclMapString = ('Start', 'Length', 'Name', 'Class');
  SegmentsHeader       : array [0..3] of PJclMapString = ('Detailed', 'map', 'of', 'segments');
  PublicsByNameHeader  : array [0..3] of PJclMapString = ('Address', 'Publics', 'by', 'Name');
  PublicsByValueHeader : array [0..3] of PJclMapString = ('Address', 'Publics', 'by', 'Value');
  LineNumbersPrefix    : PJclMapString = 'Line numbers for';
var
  CurrPos, EndPos: PJclMapString;
{$IFNDEF COMPILER9_UP}
  PreviousA,
{$ENDIF COMPILER9_UP}
  A: TJclMapAddress;
  L: Integer;
  P1, P2: PJclMapString;

  function Eof: Boolean;
  begin
    Result := CurrPos >= EndPos;
  end;

  function SkipWhiteSpace: PJclMapString;
  var
    LEndPos: PJclMapString;
  begin
    Result := CurrPos;
    LEndPos := EndPos;
    while (Result < LEndPos) and (Result^ <= ' ') do
      Inc(Result);
    CurrPos := Result;
  end;

  procedure SkipEndLine;
  var
    P, LEndPos: PJclMapString;
  begin
    P := CurrPos;
    LEndPos := EndPos;
    while P < LEndPos do
    begin
      case P^ of
        #10, #13:
          Break;
      end;
      Inc(P);
    end;
    // Skip WhiteSpaces
    while (P < LEndPos) and (P^ <= ' ') do
      Inc(P);
    CurrPos := P;
  end;

  function ReadTrimmedTextLine(var Len: Integer): PJclMapString;
  var
    Start, P: PJclMapString;
  begin
    Start := CurrPos;
    P := Start;
    while (P^ <> #0) and not (P^ in [#10, #13]) do
      Inc(P);
    CurrPos := P;

    // Trim
    while (Start < P) and (Start^ <> #0) and (Start^ <= ' ') do
      Inc(Start);
    Dec(P);
    while (P > Start) and (P^ <= ' ') do
      Dec(P);
    Inc(P);

    Result := Start;
    Len := P - Start;
    if Len < 0 then
      Len := 0;
  end;

  function ReadDecValue: Integer;
  var
    P: PJclMapString;
  begin
    P := CurrPos;
    Result := 0;
    while P^ in ['0'..'9'] do
    begin
      Result := Result * 10 + (Ord(P^) - Ord('0'));
      Inc(P);
    end;
    CurrPos := P;
  end;

  function ReadHexValue: DWORD;
  var
    C: AnsiChar;
    V: Byte;
    P: PJclMapString;
  begin
    P := CurrPos;
    Result := 0;
    repeat
      C := P^;
      V := HexMap[C];
      if V and $80 <> 0 then
        Break;
      Result := (Result shl 4) or V;
      Inc(P);
    until False;
    if (C = 'H') or (C = 'h') then
      Inc(P);
    CurrPos := P;
  end;

  procedure ReadAddress(var Result: TJclMapAddress);
  begin
    Result.Segment := ReadHexValue;
    if CurrPos^ = ':' then
    begin
      Inc(CurrPos);
      Result.Offset := ReadHexValue;
    end
    else
      Result.Offset := 0;
  end;

  function ReadString: PJclMapString;
  var
    P, LEndPos: PJclMapString;
  begin
    // Skip WhiteSpaces
    LEndPos := EndPos;
    P := CurrPos;
    while (P < LEndPos) and (P^ <= ' ') do
      Inc(P);

    Result := P;
    while {(P^ <> #0) and} (P^ > ' ') do
      Inc(P);
    CurrPos := P;
  end;

  procedure FindParam(Param: AnsiChar);
  var
    P: PJclMapString;
  begin
    P := CurrPos;
    while not ((P^ = Param) and (P[1] = '=')) do
      Inc(P);
    CurrPos := P + 2;
  end;

  function SyncToHeader(const Header: array of PJclMapString): Boolean;
  var
    S: PJclMapString;
    SLen: Integer;
    TokenIndex, OldPosition, CurrentPosition: Integer;
  begin
    Result := False;
    while not Eof do
    begin
      S := ReadTrimmedTextLine(SLen);
      if SLen > 0 then
      begin
        TokenIndex := Low(Header);
        CurrentPosition := 0;
        OldPosition := 0;
        while (TokenIndex <= High(Header)) do
        begin
          CurrentPosition := AnsiStrPosIdxLen(Header[TokenIndex], S, SLen);
          if (CurrentPosition <= OldPosition) then
          begin
            CurrentPosition := 0;
            Break;
          end;
          OldPosition := CurrentPosition;
          Inc(TokenIndex);
        end;
        Result := CurrentPosition <> 0;
        if Result then
          Break;
      end;
      SkipEndLine;
    end;
    if not Eof then
      SkipWhiteSpace;
  end;

  function SyncToPrefix(const Prefix: PJclMapString): Boolean;
  var
    P: PJclMapString;
    PrefixLen: Integer;
  begin
    if Eof then
    begin
      Result := False;
      Exit;
    end;
    SkipWhiteSpace;
    P := CurrPos;
    PrefixLen := StrLenA(Prefix);
    Result := StrLCompA(Prefix, P, PrefixLen) = 0;
    if Result then
      CurrPos := P + PrefixLen;
    SkipWhiteSpace;
  end;

begin
  if FStream <> nil then
  begin
    FLinkerBug := False;
{$IFNDEF COMPILER9_UP}
    PreviousA.Segment := 0;
    PreviousA.Offset := 0;
{$ENDIF COMPILER9_UP}
    CurrPos := FStream.Memory;
    EndPos := CurrPos + FStream.Size;
    if SyncToHeader(TableHeader) then
      while IsDecDigit(CurrPos) do
      begin
        ReadAddress(A);
        SkipWhiteSpace;
        L := ReadHexValue;
        P1 := ReadString;
        P2 := ReadString;
        SkipEndLine;
        ClassTableItem(A, L, P1, P2);
      end;
    if SyncToHeader(SegmentsHeader) then
      while IsDecDigit(CurrPos) do
      begin
        ReadAddress(A);
        SkipWhiteSpace;
        L := ReadHexValue;
        FindParam('C');
        P1 := ReadString;
        FindParam('M');
        P2 := ReadString;
        SkipEndLine;
        SegmentItem(A, L, P1, P2);
      end;
    if SyncToHeader(PublicsByNameHeader) then
    begin
      if not CanHandlePublicsByName then
        CurrPos := SkipMapBlock(CurrPos, EndPos)
      else
      begin
        while IsDecDigit(CurrPos) do
        begin
          ReadAddress(A);
          P1 := ReadString;
          SkipEndLine; // compatibility with C++Builder MAP files
          PublicsByNameItem(A, P1);
        end;
      end;
    end;
    if SyncToHeader(PublicsByValueHeader) then
      if not CanHandlePublicsByValue then
        CurrPos := SkipMapBlock(CurrPos, EndPos)
      else
      begin
        while not Eof and IsDecDigit(CurrPos) do
        begin
          ReadAddress(A);
          P1 := ReadString;
          SkipEndLine; // compatibility with C++Builder MAP files
          PublicsByValueItem(A, P1);
        end;
      end;
    while SyncToPrefix(LineNumbersPrefix) do
    begin
      FLastUnitName := CurrPos;
      FLastUnitFileName := CurrPos;
      while FLastUnitFileName^ <> '(' do
        Inc(FLastUnitFileName);
      SkipEndLine;
      LineNumberUnitItem(FLastUnitName, FLastUnitFileName);
      repeat
        SkipWhiteSpace;
        L := ReadDecValue;
        SkipWhiteSpace;
        ReadAddress(A);
        SkipWhiteSpace;
        LineNumbersItem(L, A);
{$IFNDEF COMPILER9_UP}
        if not FLinkerBug and (A.Offset < PreviousA.Offset) then
        begin
          FLinkerBugUnitName := FLastUnitName;
          FLinkerBug := True;
        end;
        PreviousA := A;
{$ENDIF COMPILER9_UP}
      until not IsDecDigit(CurrPos);
    end;
  end;
end;

//=== { TJclMapParser } ======================================================

procedure TJclMapParser.ClassTableItem(const Address: TJclMapAddress;
  Len: Integer; SectionName, GroupName: PJclMapString);
begin
  if Assigned(FOnClassTable) then
    FOnClassTable(Self, Address, Len, MapStringToStr(SectionName), MapStringToStr(GroupName));
end;

procedure TJclMapParser.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
begin
  if Assigned(FOnLineNumbers) then
    FOnLineNumbers(Self, LineNumber, Address);
end;

procedure TJclMapParser.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  if Assigned(FOnLineNumberUnit) then
    FOnLineNumberUnit(Self, MapStringToStr(UnitName), MapStringToStr(UnitFileName));
end;

function TJclMapParser.CanHandlePublicsByName: Boolean;
begin
  Result := Assigned(FOnPublicsByName);
end;

function TJclMapParser.CanHandlePublicsByValue: Boolean;
begin
  Result := Assigned(FOnPublicsByValue);
end;

procedure TJclMapParser.PublicsByNameItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByName) then
    // MAP files generated by C++Builder have spaces in their identifier names
    FOnPublicsByName(Self, Address, MapStringToStr(Name, True));
end;

procedure TJclMapParser.PublicsByValueItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByValue) then
    // MAP files generated by C++Builder have spaces in their identifier names
    FOnPublicsByValue(Self, Address, MapStringToStr(Name, True));
end;

procedure TJclMapParser.SegmentItem(const Address: TJclMapAddress;
  Len: Integer; GroupName, UnitName: PJclMapString);
begin
  if Assigned(FOnSegmentItem) then
    FOnSegmentItem(Self, Address, Len, MapStringToStr(GroupName), MapStringToModuleName(UnitName));
end;

//=== { TJclMapScanner } =====================================================

constructor TJclMapScanner.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create(MapFileName, Module);
  Scan;
end;

function TJclMapScanner.MAPAddrToVA(const Addr: DWORD): DWORD;
begin
  // MAP file format was changed in Delphi 2005
  // before Delphi 2005: segments started at offset 0
  //                     only one segment of code
  // after Delphi 2005: segments started at code base address (module base address + $10000)
  //                    2 segments of code
  if (Length(FSegmentClasses) > 0) and (FSegmentClasses[0].Start > 0) and (Addr >= FSegmentClasses[0].Start) then
    // Delphi 2005 and later
    // The first segment should be code starting at module base address + $10000
    Result := Addr - FSegmentClasses[0].Start
  else
    // before Delphi 2005
    Result := Addr;
end;

class function TJclMapScanner.MapStringCacheToFileName(
  var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToFileName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TJclMapScanner.MapStringCacheToModuleName(
  var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToModuleName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TJclMapScanner.MapStringCacheToStr(var MapString: TJclMapStringCache;
  IgnoreSpaces: Boolean): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToStr(MapString.RawValue, IgnoreSpaces);
    MapString.CachedValue := Result;
  end;
end;

procedure TJclMapScanner.ClassTableItem(const Address: TJclMapAddress; Len: Integer;
  SectionName, GroupName: PJclMapString);
var
  C: Integer;
  SectionHeader: PImageSectionHeader;
begin
  C := Length(FSegmentClasses);
  SetLength(FSegmentClasses, C + 1);
  FSegmentClasses[C].Segment := Address.Segment;
  FSegmentClasses[C].Start := Address.Offset;
  FSegmentClasses[C].Addr := Address.Offset; // will be fixed below while considering module mapped address
  // test GroupName because SectionName = '.tls' in Delphi and '_tls' in BCB
  if StrLICompA(GroupName, 'TLS', 3) = 0 then
  begin
    FSegmentClasses[C].VA := FSegmentClasses[C].Start;
    FSegmentClasses[C].GroupName.TLS := True;
  end
  else
  begin
    FSegmentClasses[C].VA := MAPAddrToVA(FSegmentClasses[C].Start);
    FSegmentClasses[C].GroupName.TLS := False;
  end;
  FSegmentClasses[C].Len := Len;
  FSegmentClasses[C].SectionName.RawValue := SectionName;
  FSegmentClasses[C].GroupName.RawValue := GroupName;

  if FModule <> 0 then
  begin
    { Fix the section addresses }
    SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(SectionName));
    if SectionHeader = nil then
      { before Delphi 2005 the class names where used for the section names }
      SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(GroupName));

    if SectionHeader <> nil then
    begin
      FSegmentClasses[C].Addr := TJclAddr(FModule) + SectionHeader.VirtualAddress;
      FSegmentClasses[C].VA := SectionHeader.VirtualAddress;
    end;
  end;
end;

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function Search_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapLineNumber(Item1)^.VA) - PInteger(Item2)^;
end;

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := 0;
  Offset := 0;
  I := SearchDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Search_MapLineNumber, @Addr, True);
  if (I <> -1) and (FLineNumbers[I].VA >= ModuleStartAddr) then
  begin
    Result := FLineNumbers[I].LineNumber;
    Offset := Addr - FLineNumbers[I].VA;
  end;
end;

procedure TJclMapScanner.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
var
  SegIndex, C: Integer;
  VA: DWORD;
  Added: Boolean;
begin
  Added := False;
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if FSegmentClasses[SegIndex].GroupName.TLS then
      Va := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    { Starting with Delphi 2005, "empty" units are listes with the last line and
      the VA 0001:00000000. When we would accept 0 VAs here, System.pas functions
      could be mapped to other units and line numbers. Discaring such items should
      have no impact on the correct information, because there can't be a function
      that starts at VA 0. }
    if VA = 0 then
      Continue;
    if FLineNumbersCnt = Length(FLineNumbers)  then
    begin
      if FLineNumbersCnt < 512 then
        SetLength(FLineNumbers, FLineNumbersCnt + 512)
      else
        SetLength(FLineNumbers, FLineNumbersCnt * 2);
    end;
    FLineNumbers[FLineNumbersCnt].Segment := FSegmentClasses[SegIndex].Segment;
    FLineNumbers[FLineNumbersCnt].VA := VA;
    FLineNumbers[FLineNumbersCnt].LineNumber := LineNumber;
    FLineNumbers[FLineNumbersCnt].UnitName := FCurrentUnitName;
    Inc(FLineNumbersCnt);
    Added := True;
    if FNewUnitFileName <> nil then
    begin
      C := Length(FSourceNames);
      SetLength(FSourceNames, C + 1);
      FSourceNames[C].Segment := FSegmentClasses[SegIndex].Segment;
      FSourceNames[C].VA := VA;
      FSourceNames[C].ProcName.RawValue := FNewUnitFileName;
      FNewUnitFileName := nil;
    end;
    Break;
  end;
  if not Added then
    Inc(FLineNumberErrors);
end;

procedure TJclMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  FNewUnitFileName := UnitFileName;
  FCurrentUnitName := UnitName;
end;

function TJclMapScanner.GetLineNumberByIndex(Index: Integer): TJCLMapLineNumber;
begin
  Result := FLineNumbers[Index];
end;

function TJclMapScanner.IndexOfSegment(Addr: DWORD): Integer;
var
  L, R: Integer;
  S: PJclMapSegment;
begin
  R := Length(FSegments) - 1;
  Result := FLastAccessedSegementIndex;
  if Result <= R then
  begin
    S := @FSegments[Result];
    if (S.StartVA <= Addr) and (Addr < S.EndVA) then
      Exit;
  end;

  // binary search
  L := 0;
  while L <= R do
  begin
    Result := L + (R - L) div 2;
    S := @FSegments[Result];
    if Addr >= S.EndVA then
      L := Result + 1
    else
    begin
      R := Result - 1;
      if (S.StartVA <= Addr) and (Addr < S.EndVA) then
      begin
        FLastAccessedSegementIndex := Result;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function TJclMapScanner.ModuleNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  if I <> -1 then
    Result := MapStringCacheToModuleName(FSegments[I].UnitName)
  else
    Result := '';
end;

function TJclMapScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  Result := DWORD(-1);
  if I <> -1 then
    Result := FSegments[I].StartVA;
end;

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function Search_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapProcName(Item1)^.VA) - PInteger(Item2)^;
end;

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := '';
  Offset := 0;
  I := SearchDynArray(FProcNames, SizeOf(FProcNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FProcNames[I].VA >= ModuleStartAddr) then
  begin
    Result := MapStringCacheToStr(FProcNames[I].ProcName, True);
    Offset := Addr - FProcNames[I].VA;
  end;
end;

function TJclMapScanner.CanHandlePublicsByName: Boolean;
begin
  Result := False;
end;

function TJclMapScanner.CanHandlePublicsByValue: Boolean;
begin
  Result := True;
end;

procedure TJclMapScanner.PublicsByNameItem(const Address: TJclMapAddress;  Name: PJclMapString);
begin
end;

procedure TJclMapScanner.PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString);
var
  SegIndex: Integer;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if FProcNamesCnt = Length(FProcNames)  then
    begin
      if FProcNamesCnt < 512 then
        SetLength(FProcNames, FProcNamesCnt + 512)
      else
        SetLength(FProcNames, FProcNamesCnt * 2);
    end;
    FProcNames[FProcNamesCnt].Segment := FSegmentClasses[SegIndex].Segment;
    if FSegmentClasses[SegIndex].GroupName.TLS then
      FProcNames[FProcNamesCnt].VA := Address.Offset
    else
      FProcNames[FProcNamesCnt].VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    FProcNames[FProcNamesCnt].ProcName.RawValue := Name;
    Inc(FProcNamesCnt);
    Break;
  end;
end;

{function Sort_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapLineNumber(Item1)^.VA) - Integer(PJclMapLineNumber(Item2)^.VA);
end;}

function Sort_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapProcName(Item1)^.VA) - Integer(PJclMapProcName(Item2)^.VA);
end;

function Sort_MapSegment(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapSegment(Item1)^.EndVA) - Integer(PJclMapSegment(Item2)^.EndVA);
  if Result = 0 then
    Result := Integer(PJclMapSegment(Item1)^.StartVA) - Integer(PJclMapSegment(Item2)^.StartVA);
end;

type
  PJclMapLineNumberArray = ^TJclMapLineNumberArray;
  TJclMapLineNumberArray = array[0..MaxInt div SizeOf(TJclMapLineNumber) - 1] of TJclMapLineNumber;

  PJclMapProcNameArray = ^TJclMapProcNameArray;
  TJclMapProcNameArray = array[0..MaxInt div SizeOf(TJclMapProcName) - 1] of TJclMapProcName;

// specialized quicksort functions
procedure SortLineNumbers(ArrayVar: PJclMapLineNumberArray; L, R: Integer);
var
  I, J, P: Integer;
  Temp: TJclMapLineNumber;
  AV: PJclMapLineNumber;
  V: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      V := Integer(ArrayVar[P].VA);
      AV := @ArrayVar[I];
      while Integer(AV.VA) - V < 0 do begin Inc(I); Inc(AV); end;
      AV := @ArrayVar[J];
      while Integer(AV.VA) - V > 0 do begin Dec(J); Dec(AV); end;
      if I <= J then
      begin
        if I <> J then
        begin
          Temp := ArrayVar[I];
          ArrayVar[I] := ArrayVar[J];
          ArrayVar[J] := Temp;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortLineNumbers(ArrayVar, L, J);
    L := I;
  until I >= R;
end;

procedure SortProcNames(ArrayVar: PJclMapProcNameArray; L, R: Integer);
var
  I, J, P: Integer;
  Temp: TJclMapProcName;
  V: Integer;
  AV: PJclMapProcName;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      V := Integer(ArrayVar[P].VA);
      AV := @ArrayVar[I];
      while Integer(AV.VA) - V < 0 do begin Inc(I); Inc(AV); end;
      AV := @ArrayVar[J];
      while Integer(AV.VA) - V > 0 do begin Dec(J); Dec(AV); end;
      if I <= J then
      begin
        if I <> J then
        begin
          Temp := ArrayVar[I];
          ArrayVar[I] := ArrayVar[J];
          ArrayVar[J] := Temp;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortProcNames(ArrayVar, L, J);
    L := I;
  until I >= R;
end;

procedure TJclMapScanner.Scan;
begin
  FLineNumberErrors := 0;
  FSegmentCnt := 0;
  FProcNamesCnt := 0;
  FLastAccessedSegementIndex := 0;
  Parse;
  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
  SetLength(FSegments, FSegmentCnt);
  //SortDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Sort_MapLineNumber);
  if FLineNumbers <> nil then
    SortLineNumbers(PJclMapLineNumberArray(FLineNumbers), 0, Length(FLineNumbers) - 1);
  //SortDynArray(FProcNames, SizeOf(FProcNames[0]), Sort_MapProcName);
  if FProcNames <> nil then
    SortProcNames(PJclMapProcNameArray(FProcNames), 0, Length(FProcNames) - 1);
  SortDynArray(FSegments, SizeOf(FSegments[0]), Sort_MapSegment);
  SortDynArray(FSourceNames, SizeOf(FSourceNames[0]), Sort_MapProcName);
end;

procedure TJclMapScanner.SegmentItem(const Address: TJclMapAddress; Len: Integer;
  GroupName, UnitName: PJclMapString);
var
  SegIndex: Integer;
  VA: DWORD;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if FSegmentClasses[SegIndex].GroupName.TLS then
      VA := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    if FSegmentCnt mod 16 = 0 then
      SetLength(FSegments, FSegmentCnt + 16);
    FSegments[FSegmentCnt].Segment := FSegmentClasses[SegIndex].Segment;
    FSegments[FSegmentCnt].StartVA := VA;
    FSegments[FSegmentCnt].EndVA := VA + DWORD(Len);
    FSegments[FSegmentCnt].UnitName.RawValue := UnitName;
    Inc(FSegmentCnt);
    Break;
  end;
end;

function TJclMapScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
  ModuleStartVA: DWORD;
begin
  // try with line numbers first (Delphi compliance)
  ModuleStartVA := ModuleStartFromAddr(Addr);
  Result := '';
  I := SearchDynArray(FSourceNames, SizeOf(FSourceNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FSourceNames[I].VA >= ModuleStartVA) then
    Result := MapStringCacheToStr(FSourceNames[I].ProcName);
  if Result = '' then
  begin
    // try with module names (C++Builder compliance)
    I := IndexOfSegment(Addr);
    if I <> -1 then
      Result := MapStringCacheToFileName(FSegments[I].UnitName);
  end;
end;

function TJclMapScanner.VAFromUnitAndProcName(const UnitName, ProcName: string): DWORD;
var
  I: Integer;
  QualifiedName: string;
begin
  Result := 0;
  if (UnitName = '') or (ProcName = '') then
    Exit;
  QualifiedName := UnitName + '.' + ProcName;

  for I := Low(FProcNames) to High(FProcNames) do
  begin
    if CompareText(MapStringCacheToStr(FProcNames[I].ProcName, True), QualifiedName) = 0 then
    begin
      Result := FProcNames[i].VA;
      Break;
    end;
  end;
end;

// JCL binary debug format string encoding/decoding routines
{ Strings are compressed to following 6bit format (A..D represents characters) and terminated with }
{ 6bit #0 char. First char = #1 indicates non compressed text, #2 indicates compressed text with   }
{ leading '@' character                                                                            }
{                                                                                                  }
{ 7   6   5   4   3   2   1   0  |                                                                 }
{---------------------------------                                                                 }
{ B1  B0  A5  A4  A3  A2  A1  A0 | Data byte 0                                                     }
{---------------------------------                                                                 }
{ C3  C2  C1  C0  B5  B4  B3  B2 | Data byte 1                                                     }
{---------------------------------                                                                 }
{ D5  D4  D3  D2  D1  D0  C5  C4 | Data byte 2                                                     }
{---------------------------------                                                                 }

function SimpleCryptString(const S: TUTF8String): TUTF8String;
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  SetLength(Result, Length(S));
  P := PByte(Result);
  for I := 1 to Length(S) do
  begin
    C := Ord(S[I]);
    if C <> $AA then
      C := C xor $AA;
    P^ := C;
    Inc(P);
  end;
end;

function DecodeNameString(const S: PAnsiChar): string;
var
  I, B: Integer;
  C: Byte;
  P: PByte;
  Buffer: array [0..255] of AnsiChar;
begin
  Result := '';
  B := 0;
  P := PByte(S);
  case P^ of
    1:
      begin
        Inc(P);
        Result := UTF8ToString(SimpleCryptString(PAnsiChar(P)));
        Exit;
      end;
    2:
      begin
        Inc(P);
        Buffer[B] := '@';
        Inc(B);
      end;
  end;
  I := 0;
  C := 0;
  repeat
    case I and $03 of
      0:
        C := P^ and $3F;
      1:
        begin
          C := (P^ shr 6) and $03;
          Inc(P);
          Inc(C, (P^ and $0F) shl 2);
        end;
      2:
        begin
          C := (P^ shr 4) and $0F;
          Inc(P);
          Inc(C, (P^ and $03) shl 4);
        end;
      3:
        begin
          C := (P^ shr 2) and $3F;
          Inc(P);
        end;
    end;
    case C of
      $00:
        Break;
      $01..$0A:
        Inc(C, Ord('0') - $01);
      $0B..$24:
        Inc(C, Ord('A') - $0B);
      $25..$3E:
        Inc(C, Ord('a') - $25);
      $3F:
        C := Ord('_');
    end;
    Buffer[B] := AnsiChar(C);
    Inc(B);
    Inc(I);
  until B >= SizeOf(Buffer) - 1;
  Buffer[B] := #0;
  Result := UTF8ToString(Buffer);
end;

function EncodeNameString(const S: string): AnsiString;
var
  I, StartIndex, EndIndex: Integer;
  C: Byte;
  P: PByte;
begin
  if (Length(S) > 1) and (S[1] = '@') then
    StartIndex := 1
  else
    StartIndex := 0;
  for I := StartIndex + 1 to Length(S) do
    if not CharIsValidIdentifierLetter(Char(S[I])) then
    begin
      {$IFDEF SUPPORTS_UNICODE}
      Result := #1 + SimpleCryptString(UTF8Encode(S)) + #0; // UTF8Encode is much faster than StringToUTF8
      {$ELSE}
      Result := #1 + SimpleCryptString(StringToUTF8(S)) + #0;
      {$ENDIF SUPPORTS_UNICODE}
      Exit;
    end;
  SetLength(Result, Length(S) + StartIndex);
  P := Pointer(Result);
  if StartIndex = 1 then
    P^ := 2 // store '@' leading char information
  else
    Dec(P);
  EndIndex := Length(S) - StartIndex;
  for I := 0 to EndIndex do // including null char
  begin
    if I = EndIndex then
      C := 0
    else
      C := Byte(S[I + 1 + StartIndex]);
    case AnsiChar(C) of
      #0:
        C := 0;
      '0'..'9':
        Dec(C, Ord('0') - $01);
      'A'..'Z':
        Dec(C, Ord('A') - $0B);
      'a'..'z':
        Dec(C, Ord('a') - $25);
      '_':
        C := $3F;
    else
      C := $3F;
    end;
    case I and $03 of
      0:
        begin
          Inc(P);
          P^ := C;
        end;
      1:
        begin
          P^ := P^ or (C and $03) shl 6;
          Inc(P);
          P^ := (C shr 2) and $0F;
        end;
      2:
        begin
          P^ := P^ or Byte(C shl 4);
          Inc(P);
          P^ := (C shr 4) and $03;
        end;
      3:
        P^ := P^ or (C shl 2);
    end;
  end;
  SetLength(Result, TJclAddr(P) - TJclAddr(Pointer(Result)) + 1);
end;

function ConvertMapFileToJdbgFile(const MapFileName: TFileName): Boolean;
var
  Dummy1: string;
  Dummy2, Dummy3, Dummy4: Integer;
begin
  Result := ConvertMapFileToJdbgFile(MapFileName, Dummy1, Dummy2, Dummy3, Dummy4);
end;

function ConvertMapFileToJdbgFile(const MapFileName: TFileName; out LinkerBugUnit: string;
  out LineNumberErrors: Integer): Boolean;
var
  Dummy1, Dummy2: Integer;
begin
  Result := ConvertMapFileToJdbgFile(MapFileName, LinkerBugUnit, LineNumberErrors,
    Dummy1, Dummy2);
end;

function ConvertMapFileToJdbgFile(const MapFileName: TFileName; out LinkerBugUnit: string;
  out LineNumberErrors, MapFileSize, JdbgFileSize: Integer): Boolean;
var
  JDbgFileName: TFileName;
  Generator: TJclBinDebugGenerator;
begin
  JDbgFileName := ChangeFileExt(MapFileName, JclDbgFileExtension);
  Generator := TJclBinDebugGenerator.Create(MapFileName, 0);
  try
    MapFileSize := Generator.Stream.Size;
    JdbgFileSize := Generator.DataStream.Size;
    Result := (Generator.DataStream.Size > 0) and Generator.CalculateCheckSum;
    if Result then
      Generator.DataStream.SaveToFile(JDbgFileName);
    LinkerBugUnit := Generator.LinkerBugUnitName;
    LineNumberErrors := Generator.LineNumberErrors;
  finally
    Generator.Free;
  end;
end;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName, MapFileName: TFileName;
  out LinkerBugUnit: string; out MapFileSize, JclDebugDataSize: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName, LinkerBugUnit,
    MapFileSize, JclDebugDataSize, Dummy);
end;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName, MapFileName: TFileName;
  out LinkerBugUnit: string; out MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean;
var
  BinDebug: TJclBinDebugGenerator;
begin
  BinDebug := TJclBinDebugGenerator.Create(MapFileName, 0);
  try
    Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, BinDebug,
      LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
  finally
    BinDebug.Free;
  end;
end;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, BinDebug, LinkerBugUnit,
    MapFileSize, JclDebugDataSize, Dummy);
end;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; out LinkerBugUnit: string;
  out MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean;
var
  ImageStream: TStream;
  NtHeaders32: TImageNtHeaders32;
  NtHeaders64: TImageNtHeaders64;
  ImageSectionHeaders: TImageSectionHeaderArray;
  NtHeadersPosition, ImageSectionHeadersPosition, JclDebugSectionPosition: Int64;
  JclDebugSection: TImageSectionHeader;
  LastSection: PImageSectionHeader;
  VirtualAlignedSize: DWORD;
  NeedFill: Integer;

  procedure RoundUpToAlignment(var Value: DWORD; Alignment: DWORD);
  begin
    if (Value mod Alignment) <> 0 then
      Value := ((Value div Alignment) + 1) * Alignment;
  end;

  procedure MovePointerToRawData(AOffset: DWORD);
  var
    I: Integer;
  begin
    for I := Low(ImageSectionHeaders) to High(ImageSectionHeaders) do
      ImageSectionHeaders[I].PointerToRawData := ImageSectionHeaders[I].PointerToRawData + AOffset;
  end;

  procedure FillZeros(AStream: TStream; ACount: Integer);
  var
    I: Integer;
    X: array[0..511] of Byte;
  begin
    if ACount > 0 then
    begin
      if ACount > Length(X) then
        FillChar(X, SizeOf(X), 0)
      else
        FillChar(X, ACount, 0);

      while ACount > 0 do
      begin
        I := ACount;
        if I > SizeOf(X) then
          I := SizeOf(X);
        AStream.WriteBuffer(X, I);
        Dec(ACount, I);
      end;
    end;
  end;

  procedure WriteSectionHeaders(AStream: TStream; APosition: Integer);
  var
    HeaderSize: Integer;
  begin
    HeaderSize := SizeOf(TImageSectionHeader) * Length(ImageSectionHeaders);
    if (AStream.Seek(APosition, soFromBeginning) <> APosition) or
       (AStream.Write(ImageSectionHeaders[0], HeaderSize) <> HeaderSize) then
      raise EJclPeImageError.CreateRes(@SWriteError);
    FillZeros(AStream, ImageSectionHeaders[0].PointerToRawData - AStream.Position);
  end;

  procedure MoveData(AStream: TStream; AStart, AOffset: Integer);
  var
    CurPos: Integer;
    CurSize: Integer;
    Buffer: array of Byte;
    StartPos: Integer;
  begin
    SetLength(Buffer, 1024 * 1024);
    CurPos := AStream.Size - Length(Buffer);
    StartPos := ImageSectionHeaders[0].PointerToRawData;
    while CurPos > StartPos do
    begin
      if (AStream.Seek(CurPos, soBeginning) <> CurPos) or
         (AStream.Read(Buffer[0], Length(Buffer)) <> Length(Buffer)) then
        raise EJclPeImageError.CreateRes(@SReadError);
      if (AStream.Seek(CurPos + AOffset, soBeginning) <> CurPos + AOffset) or
         (AStream.Write(Buffer[0], Length(Buffer)) <> Length(Buffer)) then
        raise EJclPeImageError.CreateRes(@SWriteError);
      Dec(CurPos, Length(Buffer));
    end;
    CurSize := Length(Buffer) + CurPos - StartPos;
    if (AStream.Seek(StartPos, soBeginning) <> StartPos) or
       (AStream.Read(Buffer[0], CurSize) <> CurSize) then
      raise EJclPeImageError.CreateRes(@SReadError);
    if (AStream.Seek(StartPos + AOffset, soBeginning) <> StartPos + AOffset) or
       (AStream.Write(Buffer[0], CurSize) <> CurSize) then
      raise EJclPeImageError.CreateRes(@SWriteError);
  end;

  procedure CheckHeadersSpace(AStream: TStream);
  begin
    if ImageSectionHeaders[0].PointerToRawData < ImageSectionHeadersPosition +
       (SizeOf(TImageSectionHeader) * (Length(ImageSectionHeaders) + 1)) then
    begin
      MoveData(AStream, ImageSectionHeaders[0].PointerToRawData, NtHeaders64.OptionalHeader.FileAlignment);
      MovePointerToRawData(NtHeaders64.OptionalHeader.FileAlignment);
      WriteSectionHeaders(AStream, ImageSectionHeadersPosition);
    end;
  end;

begin
  MapFileSize := 0;
  JclDebugDataSize := 0;
  LineNumberErrors := 0;
  LinkerBugUnit := '';
  if BinDebug.Stream <> nil then
  begin
    Result := True;
    if BinDebug.LinkerBug then
    begin
      LinkerBugUnit := BinDebug.LinkerBugUnitName;
      LineNumberErrors := BinDebug.LineNumberErrors;
    end;
  end
  else
    Result := False;
  if not Result then
    Exit;

  ImageStream := TFileStream.Create(ExecutableFileName, fmOpenReadWrite or fmShareExclusive);
  try
    try
      MapFileSize := BinDebug.Stream.Size;
      JclDebugDataSize := BinDebug.DataStream.Size;
      VirtualAlignedSize := JclDebugDataSize;

      // JCLDEBUG
      ResetMemory(JclDebugSection, SizeOf(JclDebugSection));
      // JCLDEBUG Virtual Size
      JclDebugSection.Misc.VirtualSize := JclDebugDataSize;
      // JCLDEBUG Raw data size
      JclDebugSection.SizeOfRawData := JclDebugDataSize;
      // JCLDEBUG Section name
      Move(JclDbgDataResName, JclDebugSection.Name, IMAGE_SIZEOF_SHORT_NAME);
      // JCLDEBUG Characteristics flags
      JclDebugSection.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

      case PeMapImgTarget(ImageStream, 0) of
        taWin32:
          begin
            NtHeadersPosition := PeMapImgNtHeaders32(ImageStream, 0, NtHeaders32);
            Assert(NtHeadersPosition <> -1);
            ImageSectionHeadersPosition := PeMapImgSections32(ImageStream, NtHeadersPosition, NtHeaders32, ImageSectionHeaders);
            Assert(ImageSectionHeadersPosition <> -1);
            // Check whether there is not a section with the name already. If so, return True (0000069)
            if PeMapImgFindSection(ImageSectionHeaders, JclDbgDataResName) <> -1 then
            begin
              Result := True;
              Exit;
            end;

            JclDebugSectionPosition := ImageSectionHeadersPosition + (SizeOf(ImageSectionHeaders[0]) * Length(ImageSectionHeaders));
            LastSection := @ImageSectionHeaders[High(ImageSectionHeaders)];

            // Increase the number of sections
            Inc(NtHeaders32.FileHeader.NumberOfSections);

            // JCLDEBUG Virtual Address
            JclDebugSection.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
            // JCLDEBUG Physical Offset
            JclDebugSection.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;

            // JCLDEBUG section rounding :
            RoundUpToAlignment(JclDebugSection.VirtualAddress, NtHeaders32.OptionalHeader.SectionAlignment);
            RoundUpToAlignment(JclDebugSection.PointerToRawData, NtHeaders32.OptionalHeader.FileAlignment);
            RoundUpToAlignment(JclDebugSection.SizeOfRawData, NtHeaders32.OptionalHeader.FileAlignment);

            // Size of virtual data area
            RoundUpToAlignment(VirtualAlignedSize, NtHeaders32.OptionalHeader.SectionAlignment);
            // Update Size of Image
            Inc(NtHeaders32.OptionalHeader.SizeOfImage, VirtualAlignedSize);
            // Update Initialized data size
            Inc(NtHeaders32.OptionalHeader.SizeOfInitializedData, JclDebugSection.SizeOfRawData);

            // write NT Headers 32
            if (ImageStream.Seek(NtHeadersPosition, soBeginning) <> NtHeadersPosition) or
              (ImageStream.Write(NtHeaders32, SizeOf(NtHeaders32)) <> SizeOf(NtHeaders32)) then
              raise EJclPeImageError.CreateRes(@SWriteError);
          end;

        taWin64:
          begin
            NtHeadersPosition := PeMapImgNtHeaders64(ImageStream, 0, NtHeaders64);
            Assert(NtHeadersPosition <> -1);
            ImageSectionHeadersPosition := PeMapImgSections64(ImageStream, NtHeadersPosition, NtHeaders64, ImageSectionHeaders);
            Assert(ImageSectionHeadersPosition <> -1);
            // Check whether there is not a section with the name already. If so, return True (0000069)
            if PeMapImgFindSection(ImageSectionHeaders, JclDbgDataResName) <> -1 then
            begin
              Result := True;
              Exit;
            end;

            // Check if there is enough space for additional header
            CheckHeadersSpace(ImageStream);

            JclDebugSectionPosition := ImageSectionHeadersPosition + (SizeOf(ImageSectionHeaders[0]) * Length(ImageSectionHeaders));
            LastSection := @ImageSectionHeaders[High(ImageSectionHeaders)];

            // Increase the number of sections
            Inc(NtHeaders64.FileHeader.NumberOfSections);

            // JCLDEBUG Virtual Address
            JclDebugSection.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
            // JCLDEBUG Physical Offset
            JclDebugSection.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;

            // JCLDEBUG section rounding :
            RoundUpToAlignment(JclDebugSection.VirtualAddress, NtHeaders64.OptionalHeader.SectionAlignment);
            RoundUpToAlignment(JclDebugSection.PointerToRawData, NtHeaders64.OptionalHeader.FileAlignment);
            RoundUpToAlignment(JclDebugSection.SizeOfRawData, NtHeaders64.OptionalHeader.FileAlignment);

            // Size of virtual data area
            RoundUpToAlignment(VirtualAlignedSize, NtHeaders64.OptionalHeader.SectionAlignment);
            // Update Size of Image
            Inc(NtHeaders64.OptionalHeader.SizeOfImage, VirtualAlignedSize);
            // Update Initialized data size
            Inc(NtHeaders64.OptionalHeader.SizeOfInitializedData, JclDebugSection.SizeOfRawData);

            // write NT Headers 64
            if (ImageStream.Seek(NtHeadersPosition, soBeginning) <> NtHeadersPosition) or
              (ImageStream.Write(NtHeaders64, SizeOf(NtHeaders64)) <> SizeOf(NtHeaders64)) then
              raise EJclPeImageError.CreateRes(@SWriteError);
          end;
      else
        Result := False;
        Exit;
      end;

      // write section header
      if (ImageStream.Seek(JclDebugSectionPosition, soBeginning) <> JclDebugSectionPosition) or
        (ImageStream.Write(JclDebugSection, SizeOf(JclDebugSection)) <> SizeOf(JclDebugSection)) then
        raise EJclPeImageError.CreateRes(@SWriteError);

      // Fill data to alignment
      NeedFill := INT_PTR(JclDebugSection.SizeOfRawData) - JclDebugDataSize;

      // Note: Delphi linker seems to generate incorrect (unaligned) size of
      // the executable when adding TD32 debug data so the position could be
      // behind the size of the file then.
      ImageStream.Seek({0 +} JclDebugSection.PointerToRawData, soBeginning);
      ImageStream.CopyFrom(BinDebug.DataStream, 0);
      FillZeros(ImageStream, NeedFill);
    except
      Result := False;
    end;
  finally
    ImageStream.Free;
  end;
end;

//=== { TJclBinDebugGenerator } ==============================================

constructor TJclBinDebugGenerator.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create(MapFileName, Module);
  FDataStream := TMemoryStream.Create;
  FMapFileName := MapFileName;
  if FStream <> nil then
    CreateData;
end;

destructor TJclBinDebugGenerator.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

{$OVERFLOWCHECKS OFF}

function TJclBinDebugGenerator.CalculateCheckSum: Boolean;
var
  Header: PJclDbgHeader;
  P, EndData: PAnsiChar;
  CheckSum: Integer;
begin
  Result := DataStream.Size >= SizeOf(TJclDbgHeader);
  if Result then
  begin
    P := DataStream.Memory;
    EndData := P + DataStream.Size;
    Header := PJclDbgHeader(P);
    CheckSum := 0;
    Header^.CheckSum := 0;
    Header^.CheckSumValid := True;
    while P < EndData do
    begin
      Inc(CheckSum, PInteger(P)^);
      Inc(PInteger(P));
    end;
    Header^.CheckSum := CheckSum;
  end;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

procedure TJclBinDebugGenerator.CreateData;
var
  {$IFDEF SUPPORTS_GENERICS}
  WordList: TDictionary<string, Integer>;
  {$ELSE}
  WordList: TStringList;
  {$ENDIF SUPPORTS_GENERICS}
  WordStream: TMemoryStream;
  LastSegmentID: Word;
  LastSegmentStored: Boolean;

  function PosLastNameSep(const S: string): Integer;
  var
    InGeneric: Integer;
  begin
    // Unit.Name.ProcName                             => "Unit.Name" + "ProcName"
    // Unit.Name..ClassName                           => "UnitName" + ".ClassName"
    // Unit.Name.Class<Unit.Name.OtherClass>.ProcName => "Unit.Name.Class<Unit.Name.OtherClass>" + "ProcName"
    InGeneric := 0;
    for Result := Length(S) downto 1 do
    begin
      case S[Result] of
        '.':
          if InGeneric = 0 then
            if (Result = 1) or (S[Result - 1] <> '.') then
              Exit;
        '>':
          Inc(InGeneric);
        '<':
          Dec(InGeneric);
      end;
    end;
    Result := 0;
  end;

  function AddWord(const S: string): Integer;
  var
    {$IFDEF SUPPORTS_GENERICS}
    LowerS: string;
    {$ELSE}
    N: Integer;
    {$ENDIF SUPPORTS_GENERICS}
    E: AnsiString;
  begin
    if S = '' then
    begin
      Result := 0;
      Exit;
    end;
    {$IFDEF SUPPORTS_GENERICS}
    LowerS := AnsiLowerCase(S);
    if not WordList.TryGetValue(LowerS, Result) then
    begin
      Result := WordStream.Position;
      E := EncodeNameString(S);
      WordStream.Write(E[1], Length(E));
      WordList.Add(LowerS, Result);
    end;
    {$ELSE} // for large map files this is very slow
    N := WordList.IndexOf(S);
    if N = -1 then
    begin
      Result := WordStream.Position;
      E := EncodeNameString(S);
      WordStream.Write(E[1], Length(E));
      WordList.AddObject(S, TObject(Result));
    end
    else
      Result := DWORD(WordList.Objects[N]);
    {$ENDIF SUPPORTS_GENERICS}
    Inc(Result);
  end;

  procedure WriteValue(Value: Integer);
  var
    L: Integer;
    D: DWORD;
    P: array [1..5] of Byte;
  begin
    D := Value and $FFFFFFFF;
    L := 0;
    while D > $7F do
    begin
      Inc(L);
      P[L] := (D and $7F) or $80;
      D := D shr 7;
    end;
    Inc(L);
    P[L] := (D and $7F);
    FDataStream.Write(P, L);
  end;

  procedure WriteValueOfs(Value: Integer; var LastValue: Integer);
  begin
    WriteValue(Value - LastValue);
    LastValue := Value;
  end;

  function IsSegmentStored(SegID: Word): Boolean;
  var
    SegIndex: Integer;
    GroupName: string;
  begin
    if SegID <> LastSegmentID then
    begin
      LastSegmentID := $FFFF;
      LastSegmentStored := False;
      for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
        if FSegmentClasses[SegIndex].Segment = SegID then
      begin
        LastSegmentID := FSegmentClasses[SegIndex].Segment;
        GroupName := MapStringCacheToStr(FSegmentClasses[SegIndex].GroupName);
        LastSegmentStored := (GroupName = 'CODE') or (GroupName = 'ICODE');
        Break;
      end;
    end;
    Result := LastSegmentStored;
  end;

const
  AlignBytes: array[0..2] of Byte = (0, 0, 0);
var
  FileHeader: TJclDbgHeader;
  I, D: Integer;
  S: string;
  L1, L2, L3: Integer;
  FirstWord, SecondWord: Integer;
  WordStreamSize, DataStreamSize: Int64;
begin
  LastSegmentID := $FFFF;
  WordStream := TMemoryStream.Create;
  {$IFDEF SUPPORTS_GENERICS}
  WordList := TDictionary<string, Integer>.Create(Length(FSourceNames) + Length(FProcNames));
  {$ELSE}
  WordList := TStringList.Create;
  {$ENDIF SUPPORTS_GENERICS}
  try
    {$IFNDEF SUPPORTS_GENERICS}
    WordList.Sorted := True;
    WordList.Duplicates := dupError;
    {$ENDIF ~SUPPORTS_GENERICS}
    WordStream.SetSize((Length(FSourceNames) + Length(FProcNames)) * 40); // take an average of 40 chars per identifier

    FileHeader.Signature := JclDbgDataSignature;
    FileHeader.Version := JclDbgHeaderVersion;
    FileHeader.CheckSum := 0;
    FileHeader.CheckSumValid := False;
    FileHeader.ModuleName := AddWord(PathExtractFileNameNoExt(FMapFileName));
    FDataStream.WriteBuffer(FileHeader, SizeOf(FileHeader));

    FileHeader.Units := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FSegments) - 1 do
      if IsSegmentStored(FSegments[I].Segment) then
      begin
        WriteValueOfs(FSegments[I].StartVA, L1);
        WriteValueOfs(AddWord(MapStringCacheToModuleName(FSegments[I].UnitName)), L2);
      end;
    WriteValue(MaxInt);

    FileHeader.SourceNames := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FSourceNames) - 1 do
      if IsSegmentStored(FSourceNames[I].Segment) then
      begin
        // FSourceNames[] is sorted by VA, so if the source file name is the same as the previous
        // we don't need to store it because the VA will be matched by the previous entry.
        // This removes a lot of "Generics.Collections.pas" entries.
        S := MapStringCacheToStr(FSourceNames[I].ProcName);
        if (I = 0) or (FSourceNames[I - 1].ProcName.CachedValue <> S) then
        begin
          WriteValueOfs(FSourceNames[I].VA, L1);
          WriteValueOfs(AddWord(S), L2);
        end;
      end;
    WriteValue(MaxInt);

    FileHeader.Symbols := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    L3 := 0;
    for I := 0 to Length(FProcNames) - 1 do
      if IsSegmentStored(FProcNames[I].Segment) then
      begin
        WriteValueOfs(FProcNames[I].VA, L1);
        // MAP files generated by C++Builder have spaces in their names
        S := MapStringCacheToStr(FProcNames[I].ProcName, True);
        D := PosLastNameSep(S);
        if D = 1 then
        begin
          FirstWord := 0;
          SecondWord := 0;
        end
        else
        if D = 0 then
        begin
          FirstWord := AddWord(S);
          SecondWord := 0;
        end
        else
        begin
          FirstWord := AddWord(Copy(S, 1, D - 1));
          SecondWord := AddWord(Copy(S, D + 1, Length(S)));
        end;
        WriteValueOfs(FirstWord, L2);
        WriteValueOfs(SecondWord, L3);
      end;
    WriteValue(MaxInt);

    FileHeader.LineNumbers := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FLineNumbers) - 1 do
      if IsSegmentStored(FLineNumbers[I].Segment) then
      begin
        WriteValueOfs(FLineNumbers[I].VA, L1);
        WriteValueOfs(FLineNumbers[I].LineNumber, L2);
      end;
    WriteValue(MaxInt);

    FileHeader.Words := FDataStream.Position;

    // Calculate and allocate the required size in advance instead of reallocating on the fly.
    WordStreamSize := WordStream.Position;
    DataStreamSize := FDataStream.Position + WordStreamSize;
    DataStreamSize := DataStreamSize + (4 - (DataStreamSize and $3));
    FDataStream.Size := DataStreamSize; // set capacity

    WordStream.Position := 0;
    FDataStream.CopyFrom(WordStream, WordStreamSize);

    // Align to 4 bytes
    FDataStream.WriteBuffer(AlignBytes, 4 - (FDataStream.Position and $3));
    if FDataStream.Size <> FDataStream.Position then // just in case something changed without adjusting the size calculation
      FDataStream.Size := FDataStream.Position;

    // Update the file header
    FDataStream.Seek(0, soBeginning);
    FDataStream.WriteBuffer(FileHeader, SizeOf(FileHeader));
  finally
    WordStream.Free;
    WordList.Free;
  end;
end;

//=== { TJclBinDebugScanner } ================================================

constructor TJclBinDebugScanner.Create(AStream: TCustomMemoryStream; CacheData, CacheProcNames: Boolean);
begin
  inherited Create;
  FCacheData := CacheData;
  FCacheProcNames := CacheProcNames;
  FStream := AStream;
  CheckFormat;
end;

procedure TJclBinDebugScanner.CacheLineNumbers;
var
  P: Pointer;
  Value, LineNumber, C, Ln: Integer;
  CurrVA: DWORD;
begin
  if FLineNumbers = nil then
  begin
    LineNumber := 0;
    CurrVA := 0;
    C := 0;
    Ln := 0;
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.LineNumbers);
    Value := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrVA, Value);
      ReadValue(P, Value);
      Inc(LineNumber, Value);
      if C = Ln then
      begin
        if Ln < 64 then
          Ln := 64
        else
          Ln := Ln + Ln div 4;
        SetLength(FLineNumbers, Ln);
      end;
      FLineNumbers[C].VA := CurrVA;
      FLineNumbers[C].LineNumber := LineNumber;
      Inc(C);
    end;
    SetLength(FLineNumbers, C);
  end;
end;

procedure TJclBinDebugScanner.CacheProcNames;
var
  P: Pointer;
  Value, FirstWord, SecondWord, C, Ln: Integer;
  CurrAddr: DWORD;
begin
  if FProcNames = nil then
  begin
    FirstWord := 0;
    SecondWord := 0;
    CurrAddr := 0;
    C := 0;
    Ln := 0;
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    Value := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      ReadValue(P, Value);
      Inc(FirstWord, Value);
      ReadValue(P, Value);
      Inc(SecondWord, Value);
      if C = Ln then
      begin
        if Ln < 64 then
          Ln := 64
        else
          Ln := Ln + Ln div 4;
        SetLength(FProcNames, Ln);
      end;
      FProcNames[C].Addr := CurrAddr;
      FProcNames[C].FirstWord := FirstWord;
      FProcNames[C].SecondWord := SecondWord;
      if FCacheProcNames then
      begin
        if (FirstWord <> 0) and (SecondWord <> 0) then
          FProcNames[C].Text := DataToStr(FirstWord) + '.' + DataToStr(SecondWord)
        else if FirstWord <> 0 then
          FProcNames[C].Text := DataToStr(FirstWord)
        else
          FProcNames[C].Text := '';
      end
      else
        FProcNames[C].Text := '';

      Inc(C);
    end;
    SetLength(FProcNames, C);
  end;
end;

{$OVERFLOWCHECKS OFF}

procedure TJclBinDebugScanner.CheckFormat;
var
  CheckSum: Integer;
  Data, EndData: PAnsiChar;
  Header: PJclDbgHeader;
begin
  Data := FStream.Memory;
  Header := PJclDbgHeader(Data);
  FValidFormat := (Data <> nil) and (FStream.Size > SizeOf(TJclDbgHeader)) and
    (FStream.Size mod 4 = 0) and
    (Header^.Signature = JclDbgDataSignature) and (Header^.Version = JclDbgHeaderVersion);
  if FValidFormat and Header^.CheckSumValid then
  begin
    CheckSum := -Header^.CheckSum;
    EndData := Data + FStream.Size;
    while Data < EndData do
    begin
      Inc(CheckSum, PInteger(Data)^);
      Inc(PInteger(Data));
    end;
    CheckSum := (CheckSum shr 8) or (CheckSum shl 24);
    FValidFormat := (CheckSum = Header^.CheckSum);
  end;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

function TJclBinDebugScanner.DataToStr(A: Integer): string;
var
  P: PAnsiChar;
begin
  if A = 0 then
    Result := ''
  else
  begin
    P := PAnsiChar(TJclAddr(FStream.Memory) + TJclAddr(A) + TJclAddr(PJclDbgHeader(FStream.Memory)^.Words) - 1);
    Result := DecodeNameString(P);
  end;
end;

function TJclBinDebugScanner.GetModuleName: string;
begin
  Result := DataToStr(PJclDbgHeader(FStream.Memory)^.ModuleName);
end;

function TJclBinDebugScanner.IsModuleNameValid(const Name: TFileName): Boolean;
begin
  Result := AnsiSameText(ModuleName, PathExtractFileNameNoExt(Name));
end;

function TJclBinDebugScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function TJclBinDebugScanner.LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer;
var
  P: Pointer;
  Value, LineNumber: Integer;
  CurrVA, ModuleStartVA, ItemVA: DWORD;
begin
  ModuleStartVA := ModuleStartFromAddr(Addr);
  LineNumber := 0;
  Offset := 0;
  if FCacheData then
  begin
    CacheLineNumbers;
    for Value := Length(FLineNumbers) - 1 downto 0 do
      if FLineNumbers[Value].VA <= Addr then
      begin
        if FLineNumbers[Value].VA >= ModuleStartVA then
        begin
          LineNumber := FLineNumbers[Value].LineNumber;
          Offset := Addr - FLineNumbers[Value].VA;
        end;
        Break;
      end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.LineNumbers);
    CurrVA := 0;
    ItemVA := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrVA, Value);
      if Addr < CurrVA then
      begin
        if ItemVA < ModuleStartVA then
        begin
          LineNumber := 0;
          Offset := 0;
        end;
        Break;
      end
      else
      begin
        ItemVA := CurrVA;
        ReadValue(P, Value);
        Inc(LineNumber, Value);
        Offset := Addr - CurrVA;
      end;
    end;
  end;
  Result := LineNumber;
end;

function TJclBinDebugScanner.MakePtr(A: Integer): Pointer;
begin
  Result := Pointer(TJclAddr(FStream.Memory) + TJclAddr(A));
end;

function TJclBinDebugScanner.ModuleNameFromAddr(Addr: DWORD): string;
var
  Value, Name: Integer;
  StartAddr: DWORD;
  P: Pointer;
begin
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.Units);
  Name := 0;
  StartAddr := 0;
  Value := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else
    begin
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  Result := DataToStr(Name);
end;

function TJclBinDebugScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  Value: Integer;
  StartAddr, ModuleStartAddr: DWORD;
  P: Pointer;
begin
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.Units);
  StartAddr := 0;
  ModuleStartAddr := DWORD(-1);
  Value := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else
    begin
      ReadValue(P, Value);
      ModuleStartAddr := StartAddr;
    end;
  end;
  Result := ModuleStartAddr;
end;

function TJclBinDebugScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function TJclBinDebugScanner.ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string;
var
  P: Pointer;
  Value, FirstWord, SecondWord: Integer;
  CurrAddr, ModuleStartAddr, ItemAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  FirstWord := 0;
  SecondWord := 0;
  Offset := 0;
  if FCacheData then
  begin
    CacheProcNames;
    for Value := Length(FProcNames) - 1 downto 0 do
      if FProcNames[Value].Addr <= Addr then
      begin
        if FProcNames[Value].Addr >= ModuleStartAddr then
        begin
          FirstWord := FProcNames[Value].FirstWord;
          SecondWord := FProcNames[Value].SecondWord;
          Offset := Addr - FProcNames[Value].Addr;
        end;
        Break;
      end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    CurrAddr := 0;
    ItemAddr := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then
      begin
        if ItemAddr < ModuleStartAddr then
        begin
          FirstWord := 0;
          SecondWord := 0;
          Offset := 0;
        end;
        Break;
      end
      else
      begin
        ItemAddr := CurrAddr;
        ReadValue(P, Value);
        Inc(FirstWord, Value);
        ReadValue(P, Value);
        Inc(SecondWord, Value);
        Offset := Addr - CurrAddr;
      end;
    end;
  end;
  if FirstWord <> 0 then
  begin
    Result := DataToStr(FirstWord);
    if SecondWord <> 0 then
      Result := Result + '.' + DataToStr(SecondWord);
  end
  else
    Result := '';
end;

class function TJclBinDebugScanner.ReadValue(var P: Pointer; var Value: Integer): Boolean;
var
  N: Integer;
  I: Integer;
  B: Byte;
begin
  N := 0;
  I := 0;
  repeat
    B := PByte(P)^;
    Inc(PByte(P));
    Inc(N, (B and $7F) shl I);
    Inc(I, 7);
  until B and $80 = 0;
  Value := N;
  Result := (N <> MaxInt);
end;

function TJclBinDebugScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  Value, Name: Integer;
  StartAddr, ModuleStartAddr, ItemAddr: DWORD;
  P: Pointer;
  Found: Boolean;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.SourceNames);
  Name := 0;
  StartAddr := 0;
  ItemAddr := 0;
  Found := False;
  Value := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
    begin
      if ItemAddr < ModuleStartAddr then
        Name := 0
      else
        Found := True;
      Break;
    end
    else
    begin
      ItemAddr := StartAddr;
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  if Found then
    Result := DataToStr(Name)
  else
    Result := '';
end;

function TJclBinDebugScanner.VAFromUnitAndProcName(const UnitName, ProcName: string): DWORD;
var
  P: Pointer;
  VA: DWORD;
  I, Value: Integer;
  FirstWord, SecondWord: Integer;
  QualifiedName, S: string;
begin
  Result := 0;
  if (UnitName = '') or (ProcName = '') then
    Exit;
  QualifiedName := UnitName + '.' + ProcName;

  if FCacheData then
  begin
    CacheProcNames;
    for I := Low(FProcNames) to High(FProcNames) do
    begin
      if FProcNames[I].Text <> '' then
        S := FProcNames[I].Text
      else
      begin
        if FProcNames[I].FirstWord = 0 then
          Continue;
        if (FProcNames[I].FirstWord <> 0) and (FProcNames[I].SecondWord <> 0) then
          FProcNames[I].Text := DataToStr(FProcNames[I].FirstWord ) + '.' + DataToStr(FProcNames[I].SecondWord)
        else if FProcNames[I].FirstWord <> 0 then
          FProcNames[I].Text := DataToStr(FProcNames[I].FirstWord)
        else
          FProcNames[I].Text := '';
      end;
      if CompareText(FProcNames[I].Text, QualifiedName) = 0 then
      begin
        Result := FProcNames[i].Addr;
        Break;
      end;
    end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    VA := 0;
    FirstWord := 0;
    SecondWord := 0;
    while ReadValue(P, Value) do
    begin
      Inc(VA, Value);
      ReadValue(P, Value);
      Inc(FirstWord, Value);
      ReadValue(P, Value);
      Inc(SecondWord, Value);
      if FirstWord = 0 then
        Continue;
      S := DataToStr(FirstWord);
      if SecondWord <> 0 then
        S := S + '.' + DataToStr(SecondWord);
      if CompareText(S, QualifiedName) = 0 then
      begin
        Result := VA;
        Break;
      end;
    end;
  end;
end;

//=== { TJclLocationInfoEx } =================================================

constructor TJclLocationInfoEx.Create(AParent: TJclCustomLocationInfoList; Address: Pointer);
var
  Options: TJclLocationInfoListOptions;
begin
  inherited Create;
  FAddress := Address;
  FParent := AParent;
  if Assigned(FParent) then
    Options := FParent.Options
  else
    Options := [];
  Fill(Options);
end;

procedure TJclLocationInfoEx.AssignTo(Dest: TPersistent);
begin
  if Dest is TJclLocationInfoEx then
  begin
    TJclLocationInfoEx(Dest).FAddress := FAddress;
    TJclLocationInfoEx(Dest).FBinaryFileName := FBinaryFileName;
    TJclLocationInfoEx(Dest).FDebugInfo := FDebugInfo;
    TJclLocationInfoEx(Dest).FLineNumber := FLineNumber;
    TJclLocationInfoEx(Dest).FLineNumberOffsetFromProcedureStart := FLineNumberOffsetFromProcedureStart;
    TJclLocationInfoEx(Dest).FModuleName := FModuleName;
    TJclLocationInfoEx(Dest).FOffsetFromLineNumber := FOffsetFromLineNumber;
    TJclLocationInfoEx(Dest).FOffsetFromProcName := FOffsetFromProcName;
    TJclLocationInfoEx(Dest).FProcedureName := FProcedureName;
    TJclLocationInfoEx(Dest).FSourceName := FSourceName;
    TJclLocationInfoEx(Dest).FSourceUnitName := FSourceUnitName;
    TJclLocationInfoEx(Dest).FUnitVersionDateTime := FUnitVersionDateTime;
    TJclLocationInfoEx(Dest).FUnitVersionExtra := FUnitVersionExtra;
    TJclLocationInfoEx(Dest).FUnitVersionLogPath := FUnitVersionLogPath;
    TJclLocationInfoEx(Dest).FUnitVersionRCSfile := FUnitVersionRCSfile;
    TJclLocationInfoEx(Dest).FUnitVersionRevision := FUnitVersionRevision;
    TJclLocationInfoEx(Dest).FVAddress := FVAddress;
    TJclLocationInfoEx(Dest).FValues := FValues;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJclLocationInfoEx.Clear;
begin
  FAddress := nil;
  Fill([]);
end;

procedure TJclLocationInfoEx.Fill(AOptions: TJclLocationInfoListOptions);
var
  Info, StartProcInfo: TJclLocationInfo;
  FixedProcedureName: string;
  Module: HMODULE;
  {$IFDEF UNITVERSIONING}
  I: Integer;
  UnitVersion: TUnitVersion;
  UnitVersioning: TUnitVersioning;
  UnitVersioningModule: TUnitVersioningModule;
  {$ENDIF UNITVERSIONING}
begin
  FValues := [];
  if liloAutoGetAddressInfo in AOptions then
  begin
    Module := ModuleFromAddr(FAddress);
    FVAddress := Pointer(TJclAddr(FAddress) - TJclAddr(Module) - ModuleCodeOffset);
    FModuleName := ExtractFileName(GetModulePath(Module));
  end
  else
  begin
    {$IFDEF UNITVERSIONING}
    Module := 0;
    {$ENDIF UNITVERSIONING}
    FVAddress := nil;
    FModuleName := '';
  end;
  if (liloAutoGetLocationInfo in AOptions) and GetLocationInfo(FAddress, Info) then
  begin
    FValues := FValues + [lievLocationInfo];
    FOffsetFromProcName := Info.OffsetFromProcName;
    FSourceUnitName := Info.UnitName;
    FixedProcedureName := Info.ProcedureName;
    if Pos(Info.UnitName + '.', FixedProcedureName) = 1 then
      FixedProcedureName := Copy(FixedProcedureName, Length(Info.UnitName) + 2, Length(FixedProcedureName) - Length(Info.UnitName) - 1);
    FProcedureName := FixedProcedureName;
    FSourceName := Info.SourceName;
    FLineNumber := Info.LineNumber;
    if FLineNumber > 0 then
      FOffsetFromLineNumber := Info.OffsetFromLineNumber
    else
      FOffsetFromLineNumber := 0;
    if GetLocationInfo(Pointer(TJclAddr(Info.Address) -
      Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
    begin
      FLineNumberOffsetFromProcedureStart := Info.LineNumber - StartProcInfo.LineNumber;
      FValues := FValues + [lievProcedureStartLocationInfo];
    end
    else
      FLineNumberOffsetFromProcedureStart := 0;
    FDebugInfo := Info.DebugInfo;
    FBinaryFileName := Info.BinaryFileName;
  end
  else
  begin
    FOffsetFromProcName := 0;
    FSourceUnitName := '';
    FProcedureName := '';
    FSourceName := '';
    FLineNumber := 0;
    FOffsetFromLineNumber := 0;
    FLineNumberOffsetFromProcedureStart := 0;
    FDebugInfo := nil;
    FBinaryFileName := '';
  end;
  FUnitVersionDateTime := 0;
  FUnitVersionLogPath := '';
  FUnitVersionRCSfile := '';
  FUnitVersionRevision := '';
  {$IFDEF UNITVERSIONING}
  if (liloAutoGetUnitVersionInfo in AOptions) and (FSourceName <> '') then
  begin
    if not (liloAutoGetAddressInfo in AOptions) then
      Module := ModuleFromAddr(FAddress);
    UnitVersioning := GetUnitVersioning;
    for I := 0 to UnitVersioning.ModuleCount - 1 do
    begin
      UnitVersioningModule := UnitVersioning.Modules[I];
      if UnitVersioningModule.Instance = Module then
      begin
        UnitVersion := UnitVersioningModule.FindUnit(FSourceName);
        if Assigned(UnitVersion) then
        begin
          FUnitVersionDateTime := UnitVersion.DateTime;
          FUnitVersionLogPath := UnitVersion.LogPath;
          FUnitVersionRCSfile := UnitVersion.RCSfile;
          FUnitVersionRevision := UnitVersion.Revision;
          FValues := FValues + [lievUnitVersionInfo];
          Break;
        end;
      end;
      if lievUnitVersionInfo in FValues then
        Break;
    end;
  end;
  {$ENDIF UNITVERSIONING}
end;

{ TODO -oUSc : Include... better as function than property? }
function TJclLocationInfoEx.GetAsString: string;
const
  IncludeStartProcLineOffset = True;
  IncludeAddressOffset = True;
  IncludeModuleName = True;
var
  IncludeVAddress: Boolean;
  OffsetStr, StartProcOffsetStr: string;
begin
  IncludeVAddress := True;
  OffsetStr := '';
  if lievLocationInfo in FValues then
  begin
    if LineNumber > 0 then
    begin
      if IncludeStartProcLineOffset and (lievProcedureStartLocationInfo in FValues) then
        StartProcOffsetStr := Format(' + %d', [LineNumberOffsetFromProcedureStart])
      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Address, SourceUnitName, ProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      if SourceUnitName <> '' then
        Result := Format('[%p] %s.%s%s', [Address, SourceUnitName, ProcedureName, OffsetStr])
      else
        Result := Format('[%p] %s%s', [Address, ProcedureName, OffsetStr]);
    end;
  end
  else
  begin
    Result := Format('[%p]', [Address]);
    IncludeVAddress := True;
  end;
  if IncludeVAddress or IncludeModuleName then
  begin
    if IncludeVAddress then
    begin
      OffsetStr := Format('(%p) ', [VAddress]);
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ModuleName]), Result, 11 {$IFDEF CPUX64}+ 8{$ENDIF});
  end;
end;

//=== { TJclCustomLocationInfoList } =========================================

constructor TJclCustomLocationInfoList.Create;
begin
  inherited Create;
  FItemClass := TJclLocationInfoEx;
  FItems := TObjectList.Create;
  FOptions := [];
end;

destructor TJclCustomLocationInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJclCustomLocationInfoList.AddStackInfoList(AStackInfoList: TObject);
var
  I: Integer;
begin
  TJclStackInfoList(AStackInfoList).ForceStackTracing;
  for I := 0 to TJclStackInfoList(AStackInfoList).Count - 1 do
    InternalAdd(TJclStackInfoList(AStackInfoList)[I].CallerAddr);
end;

procedure TJclCustomLocationInfoList.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TJclCustomLocationInfoList then
  begin
    TJclCustomLocationInfoList(Dest).Clear;
    for I := 0 to Count - 1 do
      TJclCustomLocationInfoList(Dest).InternalAdd(nil).Assign(TJclLocationInfoEx(FItems[I]));
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJclCustomLocationInfoList.Clear;
begin
  FItems.Clear;
end;

function TJclCustomLocationInfoList.GetAsString: string;
var
  I: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      Strings.Add(TJclLocationInfoEx(FItems[I]).AsString);
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TJclCustomLocationInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclCustomLocationInfoList.InternalAdd(Addr: Pointer): TJclLocationInfoEx;
begin
  FItems.Add(FItemClass.Create(Self, Addr));
  Result := TJclLocationInfoEx(FItems.Last);
end;

//=== { TJclLocationInfoList } ===============================================

function TJclLocationInfoList.Add(Addr: Pointer): TJclLocationInfoEx;
begin
  Result := InternalAdd(Addr);
end;

constructor TJclLocationInfoList.Create;
begin
  inherited Create;
  FOptions := [liloAutoGetAddressInfo, liloAutoGetLocationInfo, liloAutoGetUnitVersionInfo];
end;

function TJclLocationInfoList.GetItems(AIndex: Integer): TJclLocationInfoEx;
begin
  Result := TJclLocationInfoEx(FItems[AIndex]);
end;

//=== { TJclDebugInfoSource } ================================================

constructor TJclDebugInfoSource.Create(AModule: HMODULE);
var
  MemInfo: TMemoryBasicInformation;
begin
  FModule := AModule;
  FModuleCodeSize := 0;
  if VirtualQuery(Pointer(TJclAddr(FModule) + ModuleCodeOffset), MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
    FModuleCodeSize := MemInfo.RegionSize;
end;

function TJclDebugInfoSource.GetFileName: TFileName;
begin
  Result := GetModulePath(FModule);
end;

function TJclDebugInfoSource.VAFromAddr(const Addr: Pointer): DWORD;
begin
  Result := DWORD(TJclAddr(Addr) - TJclAddr(FModule) - ModuleCodeOffset);
end;

function TJclDebugInfoSource.AddrFromVA(const VA: DWORD): Pointer;
begin
  Result := Pointer(TJclAddr(VA) + TJclAddr(FModule) + ModuleCodeOffset);
end;

//=== { TJclDebugInfoList } ==================================================

var
  DebugInfoList: TJclDebugInfoList = nil;
  InfoSourceClassList: TList = nil;
  DebugInfoCritSect: TJclCriticalSection;

procedure NeedDebugInfoList;
begin
  if DebugInfoList = nil then
    DebugInfoList := TJclDebugInfoList.Create;
end;

function TJclDebugInfoList.CreateDebugInfo(const Module: HMODULE): TJclDebugInfoSource;
var
  I: Integer;
begin
  NeedInfoSourceClassList;

  Result := nil;
  for I := 0 to InfoSourceClassList.Count - 1 do
  begin
    Result := TJclDebugInfoSourceClass(InfoSourceClassList.Items[I]).Create(Module);
    try
      if Result.InitializeSource then
        Break
      else
        FreeAndNil(Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TJclDebugInfoList.GetItemFromModule(const Module: HMODULE): TJclDebugInfoSource;
var
  I: Integer;
  TempItem: TJclDebugInfoSource;
begin
  Result := nil;
  if Module = 0 then
    Exit;
  for I := 0 to Count - 1 do
  begin
    TempItem := Items[I];
    if TempItem.Module = Module then
    begin
      Result := TempItem;
      Break;
    end;
  end;
  if Result = nil then
  begin
    Result := CreateDebugInfo(Module);
    if Result <> nil then
      Add(Result);
  end;
end;

function TJclDebugInfoList.GetItems(Index: Integer): TJclDebugInfoSource;
begin
  Result := TJclDebugInfoSource(Get(Index));
end;

function TJclDebugInfoList.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
var
  Item: TJclDebugInfoSource;
begin
  ResetMemory(Info, SizeOf(Info));
  Item := ItemFromModule[CachedModuleFromAddr(Addr)];
  if Item <> nil then
    Result := Item.GetLocationInfo(Addr, Info)
  else
    Result := False;
end;

class procedure TJclDebugInfoList.NeedInfoSourceClassList;
begin
  if not Assigned(InfoSourceClassList) then
  begin
    InfoSourceClassList := TList.Create;
    {$IFNDEF DEBUG_NO_BINARY}
    InfoSourceClassList.Add(Pointer(TJclDebugInfoBinary));
    {$ENDIF !DEBUG_NO_BINARY}
    {$IFNDEF DEBUG_NO_TD32}
    {$IFNDEF WINSCP}
    InfoSourceClassList.Add(Pointer(TJclDebugInfoTD32));
    {$ENDIF ~WINSCP}
    {$ENDIF !DEBUG_NO_TD32}
    {$IFNDEF DEBUG_NO_MAP}
    InfoSourceClassList.Add(Pointer(TJclDebugInfoMap));
    {$ENDIF !DEBUG_NO_MAP}
    {$IFNDEF DEBUG_NO_SYMBOLS}
    InfoSourceClassList.Add(Pointer(TJclDebugInfoSymbols));
    {$ENDIF !DEBUG_NO_SYMBOLS}
    {$IFNDEF DEBUG_NO_EXPORTS}
    InfoSourceClassList.Add(Pointer(TJclDebugInfoExports));
    {$ENDIF !DEBUG_NO_EXPORTS}
  end;
end;

class procedure TJclDebugInfoList.RegisterDebugInfoSource(
  const InfoSourceClass: TJclDebugInfoSourceClass);
begin
  NeedInfoSourceClassList;

  InfoSourceClassList.Add(Pointer(InfoSourceClass));
end;

class procedure TJclDebugInfoList.RegisterDebugInfoSourceFirst(
  const InfoSourceClass: TJclDebugInfoSourceClass);
begin
  NeedInfoSourceClassList;

  InfoSourceClassList.Insert(0, Pointer(InfoSourceClass));
end;

class procedure TJclDebugInfoList.UnRegisterDebugInfoSource(
  const InfoSourceClass: TJclDebugInfoSourceClass);
begin
  if Assigned(InfoSourceClassList) then
    InfoSourceClassList.Remove(Pointer(InfoSourceClass));
end;

//=== { TJclDebugInfoMap } ===================================================

destructor TJclDebugInfoMap.Destroy;
begin
  FreeAndNil(FScanner);
  inherited Destroy;
end;

function TJclDebugInfoMap.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  with FScanner do
  begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := Info.UnitName <> '';
    if Result then
    begin
      Info.Address := Addr;
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
      Info.BinaryFileName := FileName;
    end;
  end;
end;

function TJclDebugInfoMap.GetAddress(const UnitName, ProcName: string): Pointer;
var
  VA: DWORD;
begin
  Result := nil;
  VA := FScanner.VAFromUnitAndProcName(UnitName, ProcName);
  if VA <> 0 then
    Result := AddrFromVA(VA);
end;

function TJclDebugInfoMap.InitializeSource: Boolean;
var
  MapFileName: TFileName;
begin
  MapFileName := ChangeFileExt(FileName, JclMapFileExtension);
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TJclMapScanner.Create(MapFileName, Module);
end;

//=== { TJclDebugInfoBinary } ================================================

destructor TJclDebugInfoBinary.Destroy;
begin
  FreeAndNil(FScanner);
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TJclDebugInfoBinary.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  with FScanner do
  begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := Info.UnitName <> '';
    if Result then
    begin
      Info.Address := Addr;
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
      Info.BinaryFileName := FileName;
    end;
  end;
end;

function TJclDebugInfoBinary.GetAddress(const UnitName, ProcName: string): Pointer;
var
  VA: DWORD;
begin
  Result := nil;
  VA := FScanner.VAFromUnitAndProcName(UnitName, ProcName);
  if VA <> 0 then
    Result := AddrFromVA(VA);
end;

function TJclDebugInfoBinary.InitializeSource: Boolean;
var
  JdbgFileName: TFileName;
  VerifyFileName: Boolean;
begin
  VerifyFileName := False;
  Result := (PeMapImgFindSectionFromModule(Pointer(Module), JclDbgDataResName) <> nil);
  if Result then
    FStream := TJclPeSectionStream.Create(Module, JclDbgDataResName)
  else
  begin
    JdbgFileName := ChangeFileExt(FileName, JclDbgFileExtension);
    Result := FileExists(JdbgFileName);
    if Result then
    begin
      FStream := TJclFileMappingStream.Create(JdbgFileName, fmOpenRead or fmShareDenyWrite);
      VerifyFileName := True;
    end;
  end;
  if Result then
  begin
    FScanner := TJclBinDebugScanner.Create(FStream, True, False);
    Result := FScanner.ValidFormat and
      (not VerifyFileName or FScanner.IsModuleNameValid(FileName));
  end;
end;

//=== { TJclDebugInfoExports } ===============================================

destructor TJclDebugInfoExports.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TJclDebugInfoExports.IsAddressInThisExportedFunction(Addr: PByteArray; FunctionStartAddr: TJclAddr): Boolean;
begin
  Dec(TJclAddr(Addr), 6);
  Result := False;

  while TJclAddr(Addr) > FunctionStartAddr do
  begin
    if IsBadReadPtr(Addr, 6) then
      Exit;

    if (Addr[0] = $C2) and // ret $xxxx
         (((Addr[3] = $90) and (Addr[4] = $90) and (Addr[5] = $90)) or // nop
          ((Addr[3] = $CC) and (Addr[4] = $CC) and (Addr[5] = $CC))) then // int 3
      Exit;

    if (Addr[0] = $C3) and // ret
         (((Addr[1] = $90) and (Addr[2] = $90) and (Addr[3] = $90)) or // nop
          ((Addr[1] = $CC) and (Addr[2] = $CC) and (Addr[3] = $CC))) then // int 3
      Exit;

    if (Addr[0] = $E9) and // jmp rel-far
         (((Addr[5] = $90) and (Addr[6] = $90) and (Addr[7] = $90)) or // nop
          ((Addr[5] = $CC) and (Addr[6] = $CC) and (Addr[7] = $CC))) then // int 3
      Exit;

    if (Addr[0] = $EB) and // jmp rel-near
         (((Addr[2] = $90) and (Addr[3] = $90) and (Addr[4] = $90)) or // nop
          ((Addr[2] = $CC) and (Addr[3] = $CC) and (Addr[4] = $CC))) then // int 3
      Exit;

    Dec(TJclAddr(Addr));
  end;
  Result := True;
end;

function TJclDebugInfoExports.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
var
  I, BasePos: Integer;
  VA: DWORD;
  Desc: TJclBorUmDescription;
  Unmangled: string;
  RawName: Boolean;
begin
  Result := False;
  VA := DWORD(TJclAddr(Addr) - TJclAddr(FModule));
  {$IFDEF BORLAND}
  RawName := not FImage.IsPackage;
  {$ENDIF BORLAND}
  {$IFDEF FPC}
  RawName := True;
  {$ENDIF FPC}
  Info.OffsetFromProcName := 0;
  Info.OffsetFromLineNumber := 0;
  Info.BinaryFileName := FileName;
  with FImage.ExportList do
  begin
    SortList(esAddress, False);
    for I := Count - 1 downto 0 do
      if Items[I].Address <= VA then
      begin
        if RawName then
        begin
          Info.ProcedureName := Items[I].Name;
          Info.OffsetFromProcName := VA - Items[I].Address;
          Result := True;
        end
        else
        begin
          case PeBorUnmangleName(Items[I].Name, Unmangled, Desc, BasePos) of
            urOk:
              begin
                Info.UnitName := Copy(Unmangled, 1, BasePos - 2);
                if not (Desc.Kind in [skRTTI, skVTable]) then
                begin
                  Info.ProcedureName := Copy(Unmangled, BasePos, Length(Unmangled));
                  if smLinkProc in Desc.Modifiers then
                    Info.ProcedureName := '@' + Info.ProcedureName;
                  Info.OffsetFromProcName := VA - Items[I].Address;
                end;
                Result := True;
              end;
            urNotMangled:
              begin
                Info.ProcedureName := Items[I].Name;
                Info.OffsetFromProcName := VA - Items[I].Address;
                Result := True;
              end;
          end;
        end;
        if Result then
        begin
          Info.Address := Addr;
          Info.DebugInfo := Self;

          { Check if we have a valid address in an exported function. }
          if not IsAddressInThisExportedFunction(Addr, FModule + Items[I].Address) then
          begin
            //Info.UnitName := '[' + AnsiLowerCase(ExtractFileName(GetModulePath(FModule))) + ']'
            {$IFNDEF WINSCP}
            Info.ProcedureName := Format(LoadResString(@RsUnknownFunctionAt), [Info.ProcedureName]);
            {$ELSE}
            Info.ProcedureName := '';
            {$ENDIF ~WINSCP}
          end;

          Break;
        end;
      end;
  end;
end;

function TJclDebugInfoExports.GetAddress(const UnitName, ProcName: string): Pointer;
var
  I, BasePos: Integer;
  Desc: TJclBorUmDescription;
  RawName: Boolean;
  ItemUnitName: string;
  Unmangled: string;
begin
  Result := nil;
  {$IFDEF BORLAND}
  RawName := not FImage.IsPackage;
  {$ENDIF BORLAND}
  {$IFDEF FPC}
  RawName := True;
  {$ENDIF FPC}
  with FImage.ExportList do
  begin
//    SortList(esAddress, False);
    for I := 0 to Count - 1 do
    begin
      if RawName then
      begin
        ItemUnitName := '';
        Unmangled := Items[I].Name;
      end
      else
      begin
        case PeBorUnmangleName(Items[I].Name, Unmangled, Desc, BasePos) of
          urOk:
            begin
              ItemUnitName := Copy(Unmangled, 1, BasePos - 2);
              if not (Desc.Kind in [skRTTI, skVTable]) then
              begin
                Unmangled := Copy(Unmangled, BasePos, Length(Unmangled));
                if smLinkProc in Desc.Modifiers then
                  Unmangled := '@' + Unmangled;
              end;
            end;

          urNotMangled:
            Unmangled := Items[I].Name;
        end;
      end;

      if ((ItemUnitName = '') or (CompareStr(ItemUnitName, UnitName) = 0)) and (CompareStr(Unmangled, ProcName) = 0) then
      begin
        Result := AddrFromVA(Items[I].Address);
        Break;
      end;
    end;
  end;
end;

function TJclDebugInfoExports.InitializeSource: Boolean;
begin
  {$IFDEF BORLAND}
  FImage := TJclPeBorImage.Create(True);
  {$ENDIF BORLAND}
  {$IFDEF FPC}
  FImage := TJclPeImage.Create(True);
  {$ENDIF FPC}
  FImage.AttachLoadedModule(FModule);
  Result := FImage.StatusOK and (FImage.ExportList.Count > 0);
end;

{$IFDEF BORLAND}

{$IFNDEF WINSCP}

//=== { TJclDebugInfoTD32 } ==================================================

destructor TJclDebugInfoTD32.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TJclDebugInfoTD32.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  Info.UnitName := FImage.TD32Scanner.ModuleNameFromAddr(VA);
  Result := Info.UnitName <> '';
  if Result then
    with Info do
    begin
      Address := Addr;
      ProcedureName := FImage.TD32Scanner.ProcNameFromAddr(VA, OffsetFromProcName);
      LineNumber := FImage.TD32Scanner.LineNumberFromAddr(VA, OffsetFromLineNumber);
      SourceName := FImage.TD32Scanner.SourceNameFromAddr(VA);
      DebugInfo := Self;
      BinaryFileName := FileName;
    end;
end;

function TJclDebugInfoTD32.GetAddress(const UnitName, ProcName: string): Pointer;
var
  VA: DWORD;
begin
  Result := nil;
  VA := FImage.TD32Scanner.VAFromUnitAndProcName(UnitName, ProcName);
  if VA <> 0 then
    Result := AddrFromVA(VA);
end;

function TJclDebugInfoTD32.InitializeSource: Boolean;
begin
  FImage := TJclPeBorTD32Image.Create(True);
  try
    FImage.AttachLoadedModule(Module);
    Result := FImage.IsTD32DebugPresent;
  except
    Result := False;
  end;
end;

procedure TJclDebugInfoTD32.GenerateUnmangledNames;
begin
  FImage.TD32Scanner.GenerateUnmangledNames;
end;

{$ENDIF ~WINSCP}

{$ENDIF BORLAND}

//=== { TJclDebugInfoSymbols } ===============================================

type
  TSymInitializeAFunc = function (hProcess: THandle; UserSearchPath: LPSTR;
    fInvadeProcess: Bool): Bool; stdcall;
  TSymInitializeWFunc = function (hProcess: THandle; UserSearchPath: LPWSTR;
    fInvadeProcess: Bool): Bool; stdcall;
  TSymGetOptionsFunc = function: DWORD; stdcall;
  TSymSetOptionsFunc = function (SymOptions: DWORD): DWORD; stdcall;
  TSymCleanupFunc = function (hProcess: THandle): Bool; stdcall;
  {$IFDEF CPU32}
  TSymGetSymFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD;
    pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolA): Bool; stdcall;
  TSymGetSymFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD;
    pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;
  TSymGetModuleInfoAFunc = function (hProcess: THandle; dwAddr: DWORD;
    var ModuleInfo: JclWin32.TImagehlpModuleA): Bool; stdcall;
  TSymGetModuleInfoWFunc = function (hProcess: THandle; dwAddr: DWORD;
    var ModuleInfo: JclWin32.TImagehlpModuleW): Bool; stdcall;
  TSymLoadModuleFunc = function (hProcess: THandle; hFile: THandle; ImageName,
    ModuleName: LPSTR; BaseOfDll: DWORD; SizeOfDll: DWORD): DWORD; stdcall;
  TSymGetLineFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD;
    pdwDisplacement: PDWORD; var Line: JclWin32.TImageHlpLineA): Bool; stdcall;
  TSymGetLineFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD;
    pdwDisplacement: PDWORD; var Line: JclWin32.TImageHlpLineW): Bool; stdcall;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  TSymGetSymFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD64;
    pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbolA64): Bool; stdcall;
  TSymGetSymFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD64;
    pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbolW64): Bool; stdcall;
  TSymGetModuleInfoAFunc = function (hProcess: THandle; dwAddr: DWORD64;
    var ModuleInfo: JclWin32.TImagehlpModuleA64): Bool; stdcall;
  TSymGetModuleInfoWFunc = function (hProcess: THandle; dwAddr: DWORD64;
    var ModuleInfo: JclWin32.TImagehlpModuleW64): Bool; stdcall;
  TSymLoadModuleFunc = function (hProcess: THandle; hFile: THandle; ImageName,
    ModuleName: LPSTR; BaseOfDll: DWORD64; SizeOfDll: DWORD): DWORD; stdcall;
  TSymGetLineFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD64;
    pdwDisplacement: PDWORD; var Line: JclWin32.TImageHlpLineA64): Bool; stdcall;
  TSymGetLineFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD64;
    pdwDisplacement: PDWORD; var Line: JclWin32.TImageHlpLineW64): Bool; stdcall;
  {$ENDIF CPU64}

var
  DebugSymbolsInitialized: Boolean = False;
  DebugSymbolsLoadFailed: Boolean = False;
  ImageHlpDllHandle: THandle = 0;
  SymInitializeAFunc: TSymInitializeAFunc = nil;
  SymInitializeWFunc: TSymInitializeWFunc = nil;
  SymGetOptionsFunc: TSymGetOptionsFunc = nil;
  SymSetOptionsFunc: TSymSetOptionsFunc = nil;
  SymCleanupFunc: TSymCleanupFunc = nil;
  SymGetSymFromAddrAFunc: TSymGetSymFromAddrAFunc = nil;
  SymGetSymFromAddrWFunc: TSymGetSymFromAddrWFunc = nil;
  SymGetModuleInfoAFunc: TSymGetModuleInfoAFunc = nil;
  SymGetModuleInfoWFunc: TSymGetModuleInfoWFunc = nil;
  SymLoadModuleFunc: TSymLoadModuleFunc = nil;
  SymGetLineFromAddrAFunc: TSymGetLineFromAddrAFunc = nil;
  SymGetLineFromAddrWFunc: TSymGetLineFromAddrWFunc = nil;

const
  ImageHlpDllName = 'imagehlp.dll';                          // do not localize
  SymInitializeAFuncName = 'SymInitialize';                  // do not localize
  SymInitializeWFuncName = 'SymInitializeW';                 // do not localize
  SymGetOptionsFuncName = 'SymGetOptions';                   // do not localize
  SymSetOptionsFuncName = 'SymSetOptions';                   // do not localize
  SymCleanupFuncName = 'SymCleanup';                         // do not localize
  {$IFDEF CPU32}
  SymGetSymFromAddrAFuncName = 'SymGetSymFromAddr';          // do not localize
  SymGetSymFromAddrWFuncName = 'SymGetSymFromAddrW';         // do not localize
  SymGetModuleInfoAFuncName = 'SymGetModuleInfo';            // do not localize
  SymGetModuleInfoWFuncName = 'SymGetModuleInfoW';           // do not localize
  SymLoadModuleFuncName = 'SymLoadModule';                   // do not localize
  SymGetLineFromAddrAFuncName = 'SymGetLineFromAddr';        // do not localize
  SymGetLineFromAddrWFuncName = 'SymGetLineFromAddrW';       // do not localize
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  SymGetSymFromAddrAFuncName = 'SymGetSymFromAddr64';        // do not localize
  SymGetSymFromAddrWFuncName = 'SymGetSymFromAddrW64';       // do not localize
  SymGetModuleInfoAFuncName = 'SymGetModuleInfo64';          // do not localize
  SymGetModuleInfoWFuncName = 'SymGetModuleInfoW64';         // do not localize
  SymLoadModuleFuncName = 'SymLoadModule64';                 // do not localize
  SymGetLineFromAddrAFuncName = 'SymGetLineFromAddr64';      // do not localize
  SymGetLineFromAddrWFuncName = 'SymGetLineFromAddrW64';     // do not localize
  {$ENDIF CPU64}

function StrRemoveEmptyPaths(const Paths: string): string;
var
  List: TStrings;
  I: Integer;
begin
  List := TStringList.Create;
  try
    StrToStrings(Paths, DirSeparator, List, False);
    for I := 0 to List.Count - 1 do
      if Trim(List[I]) = '' then
        List[I] := '';
    Result := StringsToStr(List, DirSeparator, False);
  finally
    List.Free;
  end;
end;

class function TJclDebugInfoSymbols.InitializeDebugSymbols: Boolean;
var
  EnvironmentVarValue, SearchPath: string;
  SymOptions: Cardinal;
  ProcessHandle: THandle;
begin
  Result := DebugSymbolsInitialized;
  if not DebugSymbolsLoadFailed then
  begin
    Result := LoadDebugFunctions;
    DebugSymbolsLoadFailed := not Result;

    if Result then
    begin
      if JclDebugInfoSymbolPaths <> '' then
      begin
        SearchPath := StrEnsureSuffix(DirSeparator, JclDebugInfoSymbolPaths);
        SearchPath := StrEnsureNoSuffix(DirSeparator, SearchPath + GetCurrentFolder);

        if GetEnvironmentVar(EnvironmentVarNtSymbolPath, EnvironmentVarValue) and (EnvironmentVarValue <> '') then
          SearchPath := StrEnsureNoSuffix(DirSeparator, StrEnsureSuffix(DirSeparator, EnvironmentVarValue) + SearchPath);
        if GetEnvironmentVar(EnvironmentVarAlternateNtSymbolPath, EnvironmentVarValue) and (EnvironmentVarValue <> '') then
          SearchPath := StrEnsureNoSuffix(DirSeparator, StrEnsureSuffix(DirSeparator, EnvironmentVarValue) + SearchPath);

        // DbgHelp.dll crashes when an empty path is specified.
        // This also means that the SearchPath must not end with a DirSeparator. }
        SearchPath := StrRemoveEmptyPaths(SearchPath);
      end
      else
        // Fix crash SymLoadModuleFunc on WinXP SP3 when SearchPath=''
        SearchPath := GetCurrentFolder;

      if IsWinNT then
        // in Windows NT, first argument is a process handle
        ProcessHandle := GetCurrentProcess
      else
        // in Windows 95, 98, ME first argument is a process identifier
        ProcessHandle := GetCurrentProcessId;

      // Debug(WinXPSP3): SymInitializeWFunc==nil
      if Assigned(SymInitializeWFunc) then
        Result := SymInitializeWFunc(ProcessHandle, PWideChar(WideString(SearchPath)), False)
      else
      if Assigned(SymInitializeAFunc) then
        Result := SymInitializeAFunc(ProcessHandle, PAnsiChar(AnsiString(SearchPath)), False)
      else
        Result := False;

      if Result then
      begin
        SymOptions := SymGetOptionsFunc or SYMOPT_DEFERRED_LOADS
          or SYMOPT_FAIL_CRITICAL_ERRORS or SYMOPT_INCLUDE_32BIT_MODULES or SYMOPT_LOAD_LINES;
        SymOptions := SymOptions and (not (SYMOPT_NO_UNQUALIFIED_LOADS or SYMOPT_UNDNAME));
        SymSetOptionsFunc(SymOptions);
      end;

      DebugSymbolsInitialized := Result;
    end
    else
      UnloadDebugFunctions;
  end;
end;

class function TJclDebugInfoSymbols.CleanupDebugSymbols: Boolean;
begin
  Result := True;

  if DebugSymbolsInitialized then
    Result := SymCleanupFunc(GetCurrentProcess);

  UnloadDebugFunctions;
end;

function TJclDebugInfoSymbols.GetLocationInfo(const Addr: Pointer;
  out Info: TJclLocationInfo): Boolean;
const
  SymbolNameLength = 1000;
  {$IFDEF CPU32}
  SymbolSizeA = SizeOf(TImagehlpSymbolA) + SymbolNameLength * SizeOf(AnsiChar);
  SymbolSizeW = SizeOf(TImagehlpSymbolW) + SymbolNameLength * SizeOf(WideChar);
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  SymbolSizeA = SizeOf(TImagehlpSymbolA64) + SymbolNameLength * SizeOf(AnsiChar);
  SymbolSizeW = SizeOf(TImagehlpSymbolW64) + SymbolNameLength * SizeOf(WideChar);
  {$ENDIF CPU64}
var
  Displacement: DWORD;
  ProcessHandle: THandle;
  {$IFDEF CPU32}
  SymbolA: PImagehlpSymbolA;
  SymbolW: PImagehlpSymbolW;
  LineA: TImageHlpLineA;
  LineW: TImageHlpLineW;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  SymbolA: PImagehlpSymbolA64;
  SymbolW: PImagehlpSymbolW64;
  LineA: TImageHlpLineA64;
  LineW: TImageHlpLineW64;
  {$ENDIF CPU64}
begin
  ProcessHandle := GetCurrentProcess;

  if Assigned(SymGetSymFromAddrWFunc) then
  begin
    GetMem(SymbolW, SymbolSizeW);
    try
      ZeroMemory(SymbolW, SymbolSizeW);
      SymbolW^.SizeOfStruct := SizeOf(SymbolW^);
      SymbolW^.MaxNameLength := SymbolNameLength;
      Displacement := 0;

      Result := SymGetSymFromAddrWFunc(ProcessHandle, TJclAddr(Addr), @Displacement, SymbolW^);
      if Result then
      begin
        Info.DebugInfo := Self;
        Info.Address := Addr;
        Info.BinaryFileName := FileName;
        Info.OffsetFromProcName := Displacement;
        JclPeImage.UnDecorateSymbolName(string(PWideChar(@SymbolW^.Name[0])), Info.ProcedureName, UNDNAME_NAME_ONLY or UNDNAME_NO_ARGUMENTS);
      end;
    finally
      FreeMem(SymbolW);
    end;
  end
  else
  if Assigned(SymGetSymFromAddrAFunc) then
  begin
    GetMem(SymbolA, SymbolSizeA);
    try
      ZeroMemory(SymbolA, SymbolSizeA);
      SymbolA^.SizeOfStruct := SizeOf(SymbolA^);
      SymbolA^.MaxNameLength := SymbolNameLength;
      Displacement := 0;

      Result := SymGetSymFromAddrAFunc(ProcessHandle, TJclAddr(Addr), @Displacement, SymbolA^);
      if Result then
      begin
        Info.DebugInfo := Self;
        Info.Address := Addr;
        Info.BinaryFileName := FileName;
        Info.OffsetFromProcName := Displacement;
        JclPeImage.UnDecorateSymbolName(string(PAnsiChar(@SymbolA^.Name[0])), Info.ProcedureName, UNDNAME_NAME_ONLY or UNDNAME_NO_ARGUMENTS);
      end;
    finally
      FreeMem(SymbolA);
    end;
  end
  else
    Result := False;

  // line number is optional
  if Result and Assigned(SymGetLineFromAddrWFunc) then
  begin
    ZeroMemory(@LineW, SizeOf(LineW));
    LineW.SizeOfStruct := SizeOf(LineW);
    Displacement := 0;

    if SymGetLineFromAddrWFunc(ProcessHandle, TJclAddr(Addr), @Displacement, LineW) then
    begin
      Info.LineNumber := LineW.LineNumber;
      Info.UnitName := string(LineW.FileName);
      Info.OffsetFromLineNumber := Displacement;
    end;
  end
  else
  if Result and Assigned(SymGetLineFromAddrAFunc) then
  begin
    ZeroMemory(@LineA, SizeOf(LineA));
    LineA.SizeOfStruct := SizeOf(LineA);
    Displacement := 0;

    if SymGetLineFromAddrAFunc(ProcessHandle, TJclAddr(Addr), @Displacement, LineA) then
    begin
      Info.LineNumber := LineA.LineNumber;
      Info.UnitName := string(LineA.FileName);
      Info.OffsetFromLineNumber := Displacement;
    end;
  end;
end;

function TJclDebugInfoSymbols.GetAddress(const UnitName, ProcName: string): Pointer;
var
  VA: DWORD;
begin
  Result := nil;
  VA := 0; // FScanner.VAFromUnitAndProcName(UnitName, ProcName);
  if VA <> 0 then
    Result := AddrFromVA(VA);
end;

function TJclDebugInfoSymbols.InitializeSource: Boolean;
var
  ModuleFileName: TFileName;
  {$IFDEF CPU32}
  ModuleInfoA: TImagehlpModuleA;
  ModuleInfoW: TImagehlpModuleW;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  ModuleInfoA: TImagehlpModuleA64;
  ModuleInfoW: TImagehlpModuleW64;
  {$ENDIF CPU64}
  ProcessHandle: THandle;
begin
  Result := InitializeDebugSymbols;
  if Result then
  begin
    if IsWinNT then
      // in Windows NT, first argument is a process handle
      ProcessHandle := GetCurrentProcess
    else
      // in Windows 95, 98, ME, first argument is a process identifier
      ProcessHandle := GetCurrentProcessId;

    if Assigned(SymGetModuleInfoWFunc) then
    begin
      ZeroMemory(@ModuleInfoW, SizeOf(ModuleInfoW));
      ModuleInfoW.SizeOfStruct := SizeOf(ModuleInfoW);
      Result := SymGetModuleInfoWFunc(ProcessHandle, Module, ModuleInfoW);
      if not Result then
      begin
        // the symbols for this module are not loaded yet: load the module and query for the symbol again
        ModuleFileName := GetModulePath(Module);
        ZeroMemory(@ModuleInfoW, SizeOf(ModuleInfoW));
        ModuleInfoW.SizeOfStruct := SizeOf(ModuleInfoW);
        // warning: crash on WinXP SP3 when SymInitializeAFunc is called with empty SearchPath
        // OF: possible loss of data
        Result := (SymLoadModuleFunc(ProcessHandle, 0, PAnsiChar(AnsiString(ModuleFileName)), nil, 0, 0) <> 0) and
                  SymGetModuleInfoWFunc(ProcessHandle, Module, ModuleInfoW);
      end;
      Result := Result and (ModuleInfoW.BaseOfImage <> 0) and
                not (ModuleInfoW.SymType in [SymNone, SymExport]);
    end
    else
    if Assigned(SymGetModuleInfoAFunc) then
    begin
      ZeroMemory(@ModuleInfoA, SizeOf(ModuleInfoA));
      ModuleInfoA.SizeOfStruct := SizeOf(ModuleInfoA);
      Result := SymGetModuleInfoAFunc(ProcessHandle, Module, ModuleInfoA);
      if not Result then
      begin
        // the symbols for this module are not loaded yet: load the module and query for the symbol again
        ModuleFileName := GetModulePath(Module);
        ZeroMemory(@ModuleInfoA, SizeOf(ModuleInfoA));
        ModuleInfoA.SizeOfStruct := SizeOf(ModuleInfoA);
        // warning: crash on WinXP SP3 when SymInitializeAFunc is called with empty SearchPath
        // OF: possible loss of data
        Result := (SymLoadModuleFunc(ProcessHandle, 0, PAnsiChar(AnsiString(ModuleFileName)), nil, 0, 0) <> 0) and
                  SymGetModuleInfoAFunc(ProcessHandle, Module, ModuleInfoA);
      end;
      Result := Result and (ModuleInfoA.BaseOfImage <> 0) and
                not (ModuleInfoA.SymType in [SymNone, SymExport]);
    end
    else
      Result := False;
  end;
end;

class function TJclDebugInfoSymbols.LoadDebugFunctions: Boolean;
begin
  ImageHlpDllHandle := SafeLoadLibrary(ImageHlpDllName);

  if ImageHlpDllHandle <> 0 then
  begin
    SymInitializeAFunc := GetProcAddress(ImageHlpDllHandle, SymInitializeAFuncName);
    SymInitializeWFunc := GetProcAddress(ImageHlpDllHandle, SymInitializeWFuncName);
    SymGetOptionsFunc := GetProcAddress(ImageHlpDllHandle, SymGetOptionsFuncName);
    SymSetOptionsFunc := GetProcAddress(ImageHlpDllHandle, SymSetOptionsFuncName);
    SymCleanupFunc := GetProcAddress(ImageHlpDllHandle, SymCleanupFuncName);
    SymGetSymFromAddrAFunc := GetProcAddress(ImageHlpDllHandle, SymGetSymFromAddrAFuncName);
    SymGetSymFromAddrWFunc := GetProcAddress(ImageHlpDllHandle, SymGetSymFromAddrWFuncName);
    SymGetModuleInfoAFunc := GetProcAddress(ImageHlpDllHandle, SymGetModuleInfoAFuncName);
    SymGetModuleInfoWFunc := GetProcAddress(ImageHlpDllHandle, SymGetModuleInfoWFuncName);
    SymLoadModuleFunc := GetProcAddress(ImageHlpDllHandle, SymLoadModuleFuncName);
    SymGetLineFromAddrAFunc := GetProcAddress(ImageHlpDllHandle, SymGetLineFromAddrAFuncName);
    SymGetLineFromAddrWFunc := GetProcAddress(ImageHlpDllHandle, SymGetLineFromAddrWFuncName);
  end;

  // SymGetLineFromAddrFunc is optional
  Result := (ImageHlpDllHandle <> 0) and
    Assigned(SymGetOptionsFunc) and Assigned(SymSetOptionsFunc) and
    Assigned(SymCleanupFunc) and Assigned(SymLoadModuleFunc) and
    (Assigned(SymInitializeAFunc) or Assigned(SymInitializeWFunc)) and
    (Assigned(SymGetSymFromAddrAFunc) or Assigned(SymGetSymFromAddrWFunc)) and
    (Assigned(SymGetModuleInfoAFunc) or Assigned(SymGetModuleInfoWFunc));
end;

class function TJclDebugInfoSymbols.UnloadDebugFunctions: Boolean;
begin
  Result := ImageHlpDllHandle <> 0;

  if Result then
    FreeLibrary(ImageHlpDllHandle);

  ImageHlpDllHandle := 0;

  SymInitializeAFunc := nil;
  SymInitializeWFunc := nil;
  SymGetOptionsFunc := nil;
  SymSetOptionsFunc := nil;
  SymCleanupFunc := nil;
  SymGetSymFromAddrAFunc := nil;
  SymGetSymFromAddrWFunc := nil;
  SymGetModuleInfoAFunc := nil;
  SymGetModuleInfoWFunc := nil;
  SymLoadModuleFunc := nil;
  SymGetLineFromAddrAFunc := nil;
  SymGetLineFromAddrWFunc := nil;
end;

//=== Source location functions ==============================================

{$STACKFRAMES ON}

function Caller(Level: Integer; FastStackWalk: Boolean): Pointer;
var
  TopOfStack: TJclAddr;
  BaseOfStack: TJclAddr;
  StackFrame: PStackFrame;
begin
  Result := nil;
  try
    if FastStackWalk then
    begin
      StackFrame := GetFramePointer;
      BaseOfStack := TJclAddr(StackFrame) - 1;
      TopOfStack := GetStackTop;
      while (BaseOfStack < TJclAddr(StackFrame)) and (TJclAddr(StackFrame) < TopOfStack) do
      begin
        if Level = 0 then
        begin
          Result := Pointer(StackFrame^.CallerAddr - 1);
          Break;
        end;
        StackFrame := PStackFrame(StackFrame^.CallerFrame);
        Dec(Level);
      end;
    end
    else
    with TJclStackInfoList.Create(False, 1, nil, False, nil, nil) do
    try
      if Level < Count then
        Result := Items[Level].CallerAddr;
    finally
      Free;
    end;
  except
    Result := nil;
  end;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

procedure BeginGetLocationInfoCache;
begin
  BeginModuleFromAddrCache;
end;

procedure EndGetLocationInfoCache;
begin
  EndModuleFromAddrCache;
end;

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      DebugInfoList.GetLocationInfo(Addr, Result)
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    Finalize(Result);
    ResetMemory(Result, SizeOf(Result));
  end;
end;

function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      Result := DebugInfoList.GetLocationInfo(Addr, Info);
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    Result := False;
  end;
end;

function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean; IncludeVAddress: Boolean): string;
var
  Info, StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr, FixedProcedureName, UnitNameWithoutUnitscope: string;
  Module: HMODULE;
  {$IFDEF WINSCP}
  MainModule: HMODULE;
  ModuleName: string;
  ModulePosition: Integer;
  {$ENDIF ~WINSCP}
begin
  OffsetStr := '';
  if GetLocationInfo(Addr, Info) then
  with Info do
  begin
    FixedProcedureName := ProcedureName;
    if Pos(UnitName + '.', FixedProcedureName) = 1 then
      FixedProcedureName := Copy(FixedProcedureName, Length(UnitName) + 2, Length(FixedProcedureName) - Length(UnitName) - 1)
    else
    if Pos('.', UnitName) > 1 then
    begin
      UnitNameWithoutUnitscope := UnitName;
      Delete(UnitNameWithoutUnitscope, 1, Pos('.', UnitNameWithoutUnitscope));
      if Pos(StrLower(UnitNameWithoutUnitscope) + '.', StrLower(FixedProcedureName)) = 1 then
        FixedProcedureName := Copy(FixedProcedureName, Length(UnitNameWithoutUnitscope) + 2, Length(FixedProcedureName) - Length(UnitNameWithoutUnitscope) - 1);
    end;

    if LineNumber > 0 then
    begin
      if IncludeStartProcLineOffset and GetLocationInfo(Pointer(TJclAddr(Info.Address) -
        Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      {$IFDEF WINSCP}
      Result := Format('[%p] %s (Line %u, "%s"%s)%s', [Addr, FixedProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
      {$ELSE}
      Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Addr, UnitName, FixedProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
      {$ENDIF}
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      {$IFNDEF WINSCP}
      if UnitName <> '' then
        Result := Format('[%p] %s.%s%s', [Addr, UnitName, FixedProcedureName, OffsetStr])
      else
      {$ENDIF}
        Result := Format('[%p] %s%s', [Addr, FixedProcedureName, OffsetStr]);
    end;
  end
  else
  begin
    Result := Format('[%p]', [Addr]);
    IncludeVAddress := True;
  end;
  if IncludeVAddress or IncludeModuleName then
  begin
    Module := ModuleFromAddr(Addr);
    if IncludeVAddress then
    begin
{$OVERFLOWCHECKS OFF} // Mantis #6104
      OffsetStr := Format('(%p) ', [Pointer(TJclAddr(Addr) - TJclAddr(Module) - ModuleCodeOffset)]);
{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_OFF}
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName then
    {$IFDEF WINSCP}
    begin
      MainModule := GetModuleHandle(nil);
      if MainModule <> Module then
      begin
        ModuleName := ExtractFileName(GetModulePath(Module));
        ModulePosition := 12 {$IFDEF CPU64}+8{$ENDIF};
        if IncludeVAddress then
          ModulePosition := 2 * (ModulePosition - 1) + 1;
        if ModulePosition < Length(Result) then
          ModuleName := ModuleName + '.';
        Insert(ModuleName, Result, ModulePosition);
      end;
    end;
    {$ELSE}
      Insert(Format('{%-12s}', [ExtractFileName(GetModulePath(Module))]), Result, 11 {$IFDEF CPU64}+8{$ENDIF});
    {$ENDIF ~WINSCP}
  end;
end;

function DebugInfoAvailable(const Module: HMODULE): Boolean;
begin
  DebugInfoCritSect.Enter;
  try
    NeedDebugInfoList;
    Result := (DebugInfoList.ItemFromModule[Module] <> nil);
  finally
    DebugInfoCritSect.Leave;
  end;
end;

procedure ClearLocationData;
begin
  DebugInfoCritSect.Enter;
  try
    if DebugInfoList <> nil then
      DebugInfoList.Clear;
  finally
    DebugInfoCritSect.Leave;
  end;
end;

{$STACKFRAMES ON}

function FileByLevel(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).SourceName;
end;

function ModuleByLevel(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).UnitName;
end;

function ProcByLevel(const Level: Integer; OnlyProcedureName: boolean): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).ProcedureName;
  if OnlyProcedureName = true then
  begin
    if StrILastPos('.', Result) > 0 then
      Result :=StrRestOf(Result, StrILastPos('.', Result)+1);
  end;
end;

function LineByLevel(const Level: Integer): Integer;
begin
  Result := GetLocationInfo(Caller(Level + 1)).LineNumber;
end;

function MapByLevel(const Level: Integer; var File_, Module_, Proc_: string;
  var Line_: Integer): Boolean;
begin
  Result := MapOfAddr(Caller(Level + 1), File_, Module_, Proc_, Line_);
end;

function ExtractClassName(const ProcedureName: string): string;
var
  D: Integer;
begin
  D := Pos('.', ProcedureName);
  if D < 2 then
    Result := ''
  else
    Result := Copy(ProcedureName, 1, D - 1);
end;

function ExtractMethodName(const ProcedureName: string): string;
begin
  Result := Copy(ProcedureName, Pos('.', ProcedureName) + 1, Length(ProcedureName));
end;

function __FILE__(const Level: Integer): string;
begin
  Result := FileByLevel(Level + 1);
end;

function __MODULE__(const Level: Integer): string;
begin
  Result := ModuleByLevel(Level + 1);
end;

function __PROC__(const Level: Integer): string;
begin
  Result := ProcByLevel(Level + 1);
end;

function __LINE__(const Level: Integer): Integer;
begin
  Result := LineByLevel(Level + 1);
end;

function __MAP__(const Level: Integer; var _File, _Module, _Proc: string; var _Line: Integer): Boolean;
begin
  Result := MapByLevel(Level + 1, _File, _Module, _Proc, _Line);
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

function FileOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).SourceName;
end;

function ModuleOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).UnitName;
end;

function ProcOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).ProcedureName;
end;

function LineOfAddr(const Addr: Pointer): Integer;
begin
  Result := GetLocationInfo(Addr).LineNumber;
end;

function MapOfAddr(const Addr: Pointer; var File_, Module_, Proc_: string;
  var Line_: Integer): Boolean;
var
  LocInfo: TJclLocationInfo;
begin
  NeedDebugInfoList;
  Result := DebugInfoList.GetLocationInfo(Addr, LocInfo);
  if Result then
  begin
    File_ := LocInfo.SourceName;
    Module_ := LocInfo.UnitName;
    Proc_ := LocInfo.ProcedureName;
    Line_ := LocInfo.LineNumber;
  end;
end;

function __FILE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := FileOfAddr(Addr);
end;

function __MODULE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := ModuleOfAddr(Addr);
end;

function __PROC_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := ProcOfAddr(Addr);
end;

function __LINE_OF_ADDR__(const Addr: Pointer): Integer;
begin
  Result := LineOfAddr(Addr);
end;

function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean;
begin
  Result := MapOfAddr(Addr, _File, _Module, _Proc, _Line);
end;

//=== { TJclStackBaseList } ==================================================

constructor TJclStackBaseList.Create;
begin
  inherited Create(True);
  FThreadID := GetCurrentThreadId;
  FTimeStamp := Now;
end;

destructor TJclStackBaseList.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited Destroy;
end;

//=== { TJclGlobalStackList } ================================================

type
  TJclStackBaseListClass = class of TJclStackBaseList;

  TJclGlobalStackList = class(TThreadList)
  private
    FLockedTID: DWORD;
    FTIDLocked: Boolean;
    function GetExceptStackInfo(TID: DWORD): TJclStackInfoList;
    function GetLastExceptFrameList(TID: DWORD): TJclExceptFrameList;
    procedure ItemDestroyed(Sender: TObject);
  public
    destructor Destroy; override;
    procedure AddObject(AObject: TJclStackBaseList);
    procedure Clear;
    procedure LockThreadID(TID: DWORD);
    procedure UnlockThreadID;
    function FindObject(TID: DWORD; AClass: TJclStackBaseListClass): TJclStackBaseList;
    property ExceptStackInfo[TID: DWORD]: TJclStackInfoList read GetExceptStackInfo;
    property LastExceptFrameList[TID: DWORD]: TJclExceptFrameList read GetLastExceptFrameList;
  end;

var
  GlobalStackList: TJclGlobalStackList;

destructor TJclGlobalStackList.Destroy;
begin
  with LockList do
  try
    while Count > 0 do
      TObject(Items[0]).Free;
  finally
    UnlockList;
  end;
  inherited Destroy;
end;

procedure TJclGlobalStackList.AddObject(AObject: TJclStackBaseList);
var
  ReplacedObj: TObject;
begin
  AObject.FOnDestroy := ItemDestroyed;
  with LockList do
  try
    ReplacedObj := FindObject(AObject.ThreadID, TJclStackBaseListClass(AObject.ClassType));
    if ReplacedObj <> nil then
    begin
      Remove(ReplacedObj);
      ReplacedObj.Free;
    end;
    Add(AObject);
  finally
    UnlockList;
  end;
end;

procedure TJclGlobalStackList.Clear;
begin
  with LockList do
  try
    while Count > 0 do
      TObject(Items[0]).Free;
    { The following call to Clear seems to be useless, but it deallocates memory
      by setting the lists capacity back to zero. For the runtime memory leak check
      within DUnit it is important that the allocated memory before and after the
      test is equal. }
    Clear; // do not remove
  finally
    UnlockList;
  end;
end;

function TJclGlobalStackList.FindObject(TID: DWORD; AClass: TJclStackBaseListClass): TJclStackBaseList;
var
  I: Integer;
  Item: TJclStackBaseList;
begin
  Result := nil;
  with LockList do
  try
    if FTIDLocked and (GetCurrentThreadId = MainThreadID) then
      TID := FLockedTID;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if (Item.ThreadID = TID) and (Item is AClass) then
      begin
        Result := Item;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

function TJclGlobalStackList.GetExceptStackInfo(TID: DWORD): TJclStackInfoList;
begin
  Result := TJclStackInfoList(FindObject(TID, TJclStackInfoList));
end;

function TJclGlobalStackList.GetLastExceptFrameList(TID: DWORD): TJclExceptFrameList;
begin
  Result := TJclExceptFrameList(FindObject(TID, TJclExceptFrameList));
end;

procedure TJclGlobalStackList.ItemDestroyed(Sender: TObject);
begin
  with LockList do
  try
    Remove(Sender);
  finally
    UnlockList;
  end;
end;

procedure TJclGlobalStackList.LockThreadID(TID: DWORD);
begin
  with LockList do
  try
    if GetCurrentThreadId = MainThreadID then
    begin
      FTIDLocked := True;
      FLockedTID := TID;
    end
    else
      FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

procedure TJclGlobalStackList.UnlockThreadID;
begin
  with LockList do
  try
    FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

//=== { TJclGlobalModulesList } ==============================================

type
  TJclGlobalModulesList = class(TObject)
  private
    FAddedModules: TStringList;
    FHookedModules: TJclModuleArray;
    FLock: TJclCriticalSection;
    FModulesList: TJclModuleInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddModule(const ModuleName: string);
    function CreateModulesList: TJclModuleInfoList;
    procedure FreeModulesList(var ModulesList: TJclModuleInfoList);
    function ValidateAddress(Addr: Pointer): Boolean;
  end;

var
  GlobalModulesList: TJclGlobalModulesList;

constructor TJclGlobalModulesList.Create;
begin
  FLock := TJclCriticalSection.Create;
end;

destructor TJclGlobalModulesList.Destroy;
begin
  FreeAndNil(FLock);
  // Keep FModulesList alive if there are still TJclStackInfoLists referencing it. The
  // last JclStackInfoList will destroy it through FreeModulesList.
  if (FModulesList <> nil) and (FModulesList.FRefCount = 0) then
    FreeAndNil(FModulesList);
  FreeAndNil(FAddedModules);
  inherited Destroy;
end;

procedure TJclGlobalModulesList.AddModule(const ModuleName: string);
var
  IsMultiThreaded: Boolean;
begin
  IsMultiThreaded := IsMultiThread;
  if IsMultiThreaded then
    FLock.Enter;
  try
    if not Assigned(FAddedModules) then
    begin
      FAddedModules := TStringList.Create;
      FAddedModules.Sorted := True;
      FAddedModules.Duplicates := dupIgnore;
    end;
    FAddedModules.Add(ModuleName);
  finally
    if IsMultiThreaded then
      FLock.Leave;
  end;
end;

function TJclGlobalModulesList.CreateModulesList: TJclModuleInfoList;
var
  I: Integer;
  SystemModulesOnly: Boolean;
  IsMultiThreaded: Boolean;
  AddedModuleHandle: HMODULE;
begin
  IsMultiThreaded := IsMultiThread;
  if IsMultiThreaded then
    FLock.Enter;
  try
    if FModulesList = nil then
    begin
      SystemModulesOnly := not (stAllModules in JclStackTrackingOptions);
      Result := TJclModuleInfoList.Create(False, SystemModulesOnly);
      // Add known Borland modules collected by DLL exception hooking code
      if SystemModulesOnly and JclHookedExceptModulesList(FHookedModules) then
        for I := Low(FHookedModules) to High(FHookedModules) do
          Result.AddModule(FHookedModules[I], True);
      if Assigned(FAddedModules) then
        for I := 0 to FAddedModules.Count - 1 do
        begin
          AddedModuleHandle := GetModuleHandle(PChar(FAddedModules[I]));
          if (AddedModuleHandle <> 0) and
            not Assigned(Result.ModuleFromAddress[Pointer(AddedModuleHandle)]) then
            Result.AddModule(AddedModuleHandle, True);
        end;
      if stStaticModuleList in JclStackTrackingOptions then
        FModulesList := Result;
    end
    else
      Result := FModulesList;
  finally
    if IsMultiThreaded then
      FLock.Leave;
  end;
  // RefCount the "global" FModulesList so that if GlobalModulesList is destroyed we can keep
  // the FModulesList alive and let it be destroyed by the last TJclStackInfoList.
  if Result = FModulesList then
    InterlockedIncrement(FModulesList.FRefCount);
end;

procedure TJclGlobalModulesList.FreeModulesList(var ModulesList: TJclModuleInfoList);
var
  IsMultiThreaded: Boolean;
begin
  if Self <> nil then // happens when finalization already ran but a TJclStackInfoList is still alive
  begin
    if FModulesList <> ModulesList then
    begin
      IsMultiThreaded := IsMultiThread;
      if IsMultiThreaded then
        FLock.Enter;
      try
        FreeAndNil(ModulesList);
      finally
        if IsMultiThreaded then
          FLock.Leave;
      end;
    end
    else if FModulesList <> nil then
      InterlockedDecrement(FModulesList.FRefCount);
  end
  else
    if InterlockedDecrement(ModulesList.FRefCount) = 0 then
      FreeAndNil(ModulesList);
end;

function TJclGlobalModulesList.ValidateAddress(Addr: Pointer): Boolean;
var
  TempList: TJclModuleInfoList;
begin
  TempList := CreateModulesList;
  try
    Result := TempList.IsValidModuleAddress(Addr);
  finally
    FreeModulesList(TempList);
  end;
end;

function JclValidateModuleAddress(Addr: Pointer): Boolean;
begin
  Result := GlobalModulesList.ValidateAddress(Addr);
end;

//=== Stack info routines ====================================================

{$STACKFRAMES OFF}

function ValidCodeAddr(CodeAddr: DWORD; ModuleList: TJclModuleInfoList): Boolean;
begin
  if stAllModules in JclStackTrackingOptions then
    Result := ModuleList.IsValidModuleAddress(Pointer(CodeAddr))
  else
    Result := ModuleList.IsSystemModuleAddress(Pointer(CodeAddr));
end;

procedure CorrectExceptStackListTop(List: TJclStackInfoList; SkipFirstItem: Boolean);
var
  TopItem, I, FoundPos: Integer;
begin
  FoundPos := -1;
  if SkipFirstItem then
    TopItem := 1
  else
    TopItem := 0;
  with List do
  begin
    for I := Count - 1 downto TopItem do
      if JclBelongsHookedCode(Items[I].CallerAddr) then
      begin
        FoundPos := I;
        Break;
      end;
    if FoundPos <> -1 then
      for I := FoundPos downto TopItem do
        Delete(I);
  end;
end;

{$STACKFRAMES ON}

procedure DoExceptionStackTrace(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean;
  BaseOfStack: Pointer);
var
  IgnoreLevels: Integer;
  FirstCaller: Pointer;
  RawMode: Boolean;
  Delayed: Boolean;
begin
  RawMode := stRawMode in JclStackTrackingOptions;
  Delayed := stDelayedTrace in JclStackTrackingOptions;
  if BaseOfStack = nil then
  begin
    BaseOfStack := GetFramePointer;
    IgnoreLevels := 1;
  end
  else
    IgnoreLevels := -1; // because of the "IgnoreLevels + 1" in TJclStackInfoList.StoreToList()
  if OSException then
  begin
    if IgnoreLevels = -1 then
      IgnoreLevels := 0
    else
      Inc(IgnoreLevels); // => HandleAnyException
    FirstCaller := ExceptAddr;
  end
  else
    FirstCaller := nil;
  JclCreateStackList(RawMode, IgnoreLevels, FirstCaller, Delayed, BaseOfStack).CorrectOnAccess(OSException);
end;

function JclLastExceptStackList: TJclStackInfoList;
begin
  Result := GlobalStackList.ExceptStackInfo[GetCurrentThreadID];
end;

function JclLastExceptStackListToStrings(Strings: TStrings; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset, IncludeVAddress: Boolean): Boolean;
var
  List: TJclStackInfoList;
begin
  List := JclLastExceptStackList;
  Result := Assigned(List);
  if Result then
    List.AddToStrings(Strings, IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset,
      IncludeVAddress);
end;

function JclGetExceptStackList(ThreadID: DWORD): TJclStackInfoList;
begin
  Result := GlobalStackList.ExceptStackInfo[ThreadID];
end;

function JclGetExceptStackListToStrings(ThreadID: DWORD; Strings: TStrings;
  IncludeModuleName: Boolean = False; IncludeAddressOffset: Boolean = False;
  IncludeStartProcLineOffset: Boolean = False; IncludeVAddress: Boolean = False): Boolean;
var
  List: TJclStackInfoList;
begin
  List := JclGetExceptStackList(ThreadID);
  Result := Assigned(List);
  if Result then
    List.AddToStrings(Strings, IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset,
      IncludeVAddress);
end;

procedure JclClearGlobalStackData;
begin
  GlobalStackList.Clear;
end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList;
begin
  Result := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller, False, nil, nil);
  GlobalStackList.AddObject(Result);
end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean): TJclStackInfoList;
begin
  Result := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller, DelayedTrace, nil, nil);
  GlobalStackList.AddObject(Result);
end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean; BaseOfStack: Pointer): TJclStackInfoList;
begin
  Result := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller, DelayedTrace, BaseOfStack, nil);
  GlobalStackList.AddObject(Result);
end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer;
  DelayedTrace: Boolean; BaseOfStack, TopOfStack: Pointer): TJclStackInfoList;
begin
  Result := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller, DelayedTrace, BaseOfStack, TopOfStack);
  GlobalStackList.AddObject(Result);
end;

function GetThreadTopOfStack(ThreadHandle: THandle): TJclAddr;
var
  TBI: THREAD_BASIC_INFORMATION;
  ReturnedLength: ULONG;
begin
  Result := 0;
  ReturnedLength := 0;
  if (NtQueryInformationThread(ThreadHandle, ThreadBasicInformation, @TBI, SizeOf(TBI), @ReturnedLength) < $80000000) and
     (ReturnedLength = SizeOf(TBI)) then
    {$IFDEF CPU32}
    Result := TJclAddr(PNT_TIB32(TBI.TebBaseAddress)^.StackBase)
    {$ENDIF CPU32}
    {$IFDEF CPU64}
    Result := TJclAddr(PNT_TIB64(TBI.TebBaseAddress)^.StackBase)
    {$ENDIF CPU64}
  else
    RaiseLastOSError;
end;

function JclCreateThreadStackTrace(Raw: Boolean; const ThreadHandle: THandle): TJclStackInfoList;
var
  ContextMemory: Pointer;
  AlignedContext: PContext;
begin
  Result := nil;
  ContextMemory := AllocMem(SizeOf(TContext) + 15);
  try
    if (TJclAddr(ContextMemory) and 15) <> 0 then
      // PAnsiChar: TJclAddr is signed and would cause an int overflow for half the address space
      AlignedContext := PContext(TJclAddr(PAnsiChar(ContextMemory) + 16) and -16)
    else
      AlignedContext := ContextMemory;
    AlignedContext^.ContextFlags := CONTEXT_FULL;
    {$IFDEF CPU32}
    if GetThreadContext(ThreadHandle, AlignedContext^) then
    begin
      Result := JclCreateStackList(Raw, -1, Pointer(AlignedContext^.Eip), False, Pointer(AlignedContext^.Ebp),
                  Pointer(GetThreadTopOfStack(ThreadHandle)));
    end;
    {$ENDIF CPU32}
    {$IFDEF CPU64}
    if GetThreadContext(ThreadHandle, AlignedContext^) then
      Result := JclCreateStackList(Raw, -1, Pointer(AlignedContext^.Rip), False, Pointer(AlignedContext^.Rbp),
                  Pointer(GetThreadTopOfStack(ThreadHandle)));
    {$ENDIF CPU64}
  finally
    FreeMem(ContextMemory);
  end;
end;

function JclCreateThreadStackTraceFromID(Raw: Boolean; ThreadID: DWORD): TJclStackInfoList;
type
  TOpenThreadFunc = function(DesiredAccess: DWORD; InheritHandle: BOOL; ThreadID: DWORD): THandle; stdcall;
const
  THREAD_GET_CONTEXT       = $0008;
  THREAD_QUERY_INFORMATION = $0040;
var
  Kernel32Lib, ThreadHandle: THandle;
  OpenThreadFunc: TOpenThreadFunc;
begin
  Result := nil;
  Kernel32Lib := GetModuleHandle(kernel32);
  if Kernel32Lib <> 0 then
  begin
    // OpenThread only exists since Windows ME
    OpenThreadFunc := GetProcAddress(Kernel32Lib, 'OpenThread');
    if Assigned(OpenThreadFunc) then
    begin
      ThreadHandle := OpenThreadFunc(THREAD_GET_CONTEXT or THREAD_QUERY_INFORMATION, False, ThreadID);
      if ThreadHandle <> 0 then
      try
        Result := JclCreateThreadStackTrace(Raw, ThreadHandle);
      finally
        CloseHandle(ThreadHandle);
      end;
    end;
  end;
end;

//=== { TJclStackInfoItem } ==================================================

function TJclStackInfoItem.GetCallerAddr: Pointer;
begin
  Result := Pointer(FStackInfo.CallerAddr);
end;

function TJclStackInfoItem.GetLogicalAddress: TJclAddr;
begin
  Result := FStackInfo.CallerAddr - TJclAddr(ModuleFromAddr(CallerAddr));
end;

//=== { TJclStackInfoList } ==================================================

constructor TJclStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: Integer;
  AFirstCaller: Pointer);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, False, nil, nil);
end;

constructor TJclStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: Integer;
  AFirstCaller: Pointer; ADelayedTrace: Boolean);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, ADelayedTrace, nil, nil);
end;

constructor TJclStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: Integer;
  AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack: Pointer);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, ADelayedTrace, ABaseOfStack, nil);
end;

constructor TJclStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: Integer;
  AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack, ATopOfStack: Pointer);
var
  Item: TJclStackInfoItem;
begin
  inherited Create;
  InterlockedIncrement(GlobalStackListLiveCount);
  FIgnoreLevels := AIgnoreLevels;
  FDelayedTrace := ADelayedTrace;
  FRaw := ARaw;
  BaseOfStack := TJclAddr(ABaseOfStack);
  FStackOffset := 0;
  FFramePointer := ABaseOfStack;

  if ATopOfStack = nil then
    TopOfStack := GetStackTop
  else
    TopOfStack := TJclAddr(ATopOfStack);

  FModuleInfoList := GlobalModulesList.CreateModulesList;
  if AFirstCaller <> nil then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo.CallerAddr := TJclAddr(AFirstCaller);
    Add(Item);
  end;
  {$IFDEF CPU32}
  if DelayedTrace then
    DelayStoreStack
  else
  if Raw then
    TraceStackRaw
  else
    TraceStackFrames;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  CaptureBackTrace;
  {$ENDIF CPU64}
end;

destructor TJclStackInfoList.Destroy;
begin
  if Assigned(FStackData) then
    FreeMem(FStackData);
  GlobalModulesList.FreeModulesList(FModuleInfoList);
  inherited Destroy;
  if (InterlockedDecrement(GlobalStackListLiveCount) = 0) and JclDebugFinalized then
    FreeJclDebugGlobals;
end;

{$IFDEF CPU64}
procedure TJclStackInfoList.CaptureBackTrace;
const
  InternalSkipFrames = 1; // skip this method
var
  BackTrace: array [0..127] of Pointer;
  MaxFrames: Integer;
  Hash: DWORD;
  I: Integer;
  StackInfo: TStackInfo;
  CapturedFramesCount: Word;
begin
  if JclCheckWinVersion(6, 0) then
    MaxFrames := Length(BackTrace)
  else
  begin
    // For XP and 2003 sum of FramesToSkip and FramesToCapture must be lower than 63
    MaxFrames := 62 - InternalSkipFrames;
  end;

  ResetMemory(BackTrace, SizeOf(BackTrace));
  CapturedFramesCount := CaptureStackBackTrace(InternalSkipFrames, MaxFrames, @BackTrace, Hash);

  ResetMemory(StackInfo, SizeOf(StackInfo));
  for I := 0 to CapturedFramesCount - 1 do
  begin
    StackInfo.CallerAddr := TJclAddr(BackTrace[I]);
    StackInfo.Level := I;
    StoreToList(StackInfo); // skips all frames with a level less than "IgnoreLevels"
  end;
end;
{$ENDIF CPU64}

procedure TJclStackInfoList.ForceStackTracing;
begin
  if DelayedTrace and Assigned(FStackData) and not FInStackTracing then
  begin
    FInStackTracing := True;
    try
      if Raw then
        TraceStackRaw
      else
        TraceStackFrames;
      if FCorrectOnAccess then
        CorrectExceptStackListTop(Self, FSkipFirstItem);
    finally
      FInStackTracing := False;
      FDelayedTrace := False;
    end;
  end;
end;

function TJclStackInfoList.GetCount: Integer;
begin
  ForceStackTracing;
  Result := inherited Count;
end;

procedure TJclStackInfoList.CorrectOnAccess(ASkipFirstItem: Boolean);
begin
  FCorrectOnAccess := True;
  FSkipFirstItem := ASkipFirstItem;
end;

procedure TJclStackInfoList.AddToStrings(Strings: TStrings; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset, IncludeVAddress: Boolean);
var
  I: Integer;
  S: string;
begin
  ForceStackTracing;
  Strings.BeginUpdate;
  try
    BeginGetLocationInfoCache;
    try
      for I := 0 to Count - 1 do
      begin
        S := GetLocationInfoStr(Items[I].CallerAddr, IncludeModuleName, IncludeAddressOffset,
          IncludeStartProcLineOffset, IncludeVAddress);
        Strings.Add(S);
      end;
    finally
      EndGetLocationInfoCache;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TJclStackInfoList.GetItems(Index: Integer): TJclStackInfoItem;
begin
  ForceStackTracing;
  Result := TJclStackInfoItem(Get(Index));
end;

function TJclStackInfoList.NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
var
  CallInstructionSize: Cardinal;
  StackFrameCallerFrame, NewFrame: TJclAddr;
  StackFrameCallerAddr: TJclAddr;
begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, the frame pointer and the return address on the stack
  // are valid addresses
  StackFrameCallerFrame := StackInfo.CallerFrame;
  while ValidStackAddr(TJclAddr(StackFrame)) do
  begin
    // CallersEBP above the previous CallersEBP
    NewFrame := StackFrame^.CallerFrame;
    if NewFrame <= StackFrameCallerFrame then
      Break;
    StackFrameCallerFrame := NewFrame;

    // CallerAddr within current process space, code segment etc.
    // CallerFrame within current thread stack. Added Mar 12 2002 per Hallvard's suggestion
    StackFrameCallerAddr := StackFrame^.CallerAddr;
    if ValidCodeAddr(StackFrameCallerAddr, FModuleInfoList) and ValidStackAddr(StackFrameCallerFrame + FStackOffset) then
    begin
      Inc(StackInfo.Level);
      StackInfo.StackFrame := StackFrame;
      StackInfo.ParamPtr := PDWORD_PTRArray(TJclAddr(StackFrame) + SizeOf(TStackFrame));

      if StackFrameCallerFrame > StackInfo.CallerFrame then
        StackInfo.CallerFrame := StackFrameCallerFrame
      else
        // the frame pointer points to an address that is below
        // the last frame pointer, so it must be invalid
        Break;

      // Calculate the address of caller by subtracting the CALL instruction size (if possible)
      if ValidCallSite(StackFrameCallerAddr, CallInstructionSize) then
        StackInfo.CallerAddr := StackFrameCallerAddr - CallInstructionSize
      else
        StackInfo.CallerAddr := StackFrameCallerAddr;
      // the stack may be messed up in big projects, avoid overflow in arithmetics
      if StackFrameCallerFrame + FStackOffset < TJclAddr(StackFrame) then
        Break;
      StackInfo.DumpSize := StackFrameCallerFrame + FStackOffset - TJclAddr(StackFrame);
      StackInfo.ParamSize := (StackInfo.DumpSize - SizeOf(TStackFrame)) div 4;
      if PStackFrame(StackFrame^.CallerFrame + FStackOffset) = StackFrame then
        Break;
      // Step to the next stack frame by following the frame pointer
      StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
      Result := True;
      Exit;
    end;
    // Step to the next stack frame by following the frame pointer
    StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
  end;
  Result := False;
end;

procedure TJclStackInfoList.StoreToList(const StackInfo: TStackInfo);
var
  Item: TJclStackInfoItem;
begin
  if ((IgnoreLevels = -1) and (StackInfo.Level > 0)) or
     (StackInfo.Level > (IgnoreLevels + 1)) then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo := StackInfo;
    Add(Item);
  end;
end;

procedure TJclStackInfoList.TraceStackFrames;
var
  StackFrame: PStackFrame;
  StackInfo: TStackInfo;
begin
  Capacity := 32; // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  // Start at level 0
  StackInfo.Level := 0;
  StackInfo.CallerFrame := 0;
  if DelayedTrace then
    // Get the current stack frame from the frame register
    StackFrame := FFramePointer
  else
  begin
    // We define the bottom of the valid stack to be the current ESP pointer
    if BaseOfStack = 0 then
      BaseOfStack := TJclAddr(GetFramePointer);
    // Get a pointer to the current bottom of the stack
    StackFrame := PStackFrame(BaseOfStack);
  end;

  // We define the bottom of the valid stack to be the current frame Pointer
  // There is a TIB field called pvStackUserBase, but this includes more of the
  // stack than what would define valid stack frames.
  BaseOfStack := TJclAddr(StackFrame) - 1;
  // Loop over and report all valid stackframes
  while NextStackFrame(StackFrame, StackInfo) and (inherited Count <> MaxStackTraceItems) do
    StoreToList(StackInfo);
end;


function TraceStackInstuctions(Proc, InstructionAddr: Pointer; ModuleEndAddr: TJclAddr;
  var LocalVarStackOffset, ParamStackOffset: Integer): Boolean;
const
  PointerSize = SizeOf(Pointer);

  function ParseSaveRegisters(ProcAddr, CallAddr: TJclAddr; var RegisterStackOffset: Integer): TJclAddr;
  var
    P: PByteArray;
  begin
    Result := ProcAddr;
    while Result < TJclAddr(CallAddr) do
    begin
      P := PByteArray(Result);
      if (P[0] and $F8) = $50 then // PUSH r32
      begin
        Inc(RegisterStackOffset, PointerSize);
        Inc(Result);
        Continue;
      end;
      Break;
    end;
  end;

  function CheckRegisterRestoreBackwards(ProcAddr, CallAddr: TJclAddr; var RegisterStackOffset: Integer): Boolean;
  var
    Count: Integer;
  begin
    if RegisterStackOffset > 0 then
    begin
      Count := 0;
      while (ProcAddr > CallAddr) and (PByte(ProcAddr)^ and $F8 = $58) do // POP r32
      begin
        Dec(ProcAddr);
        Inc(Count);
      end;
      if (Count > 0) and (Cardinal(Count) <= Cardinal(RegisterStackOffset) div PointerSize) then
      begin
        // We may have used a "function call push" in the prolog analysis so fix this
        RegisterStackOffset := Count * PointerSize;
        Result := True;
      end
      else
        Result := False;
    end
    else
      Result := True;
  end;

  function ParseEspChange(ProcAddr, CallAddr: TJclAddr; var LocalVarStackOffset: Integer; var EspChangeFound: Boolean): TJclAddr;
  var
    P: PByteArray;
  begin
    Result := ProcAddr;
    P := PByteArray(Result);
    if (Result + 3 < TJclAddr(CallAddr)) and (P[0] = $83) and (P[1] = $C4) then // 83C4F8           add esp,imm8
    begin
      Inc(LocalVarStackOffset, -Integer(ShortInt(P[2])));
      EspChangeFound := True;
      Inc(Result, 3);
    end
    else if (Result + 6 < TJclAddr(CallAddr)) and (P[0] = $81) and (P[1] = $C4) then // 81C408000100     add esp,imm32
    begin
      Inc(LocalVarStackOffset, -PInteger(@P[2])^);
      EspChangeFound := True;
      Inc(Result, 6);
    end;
  end;

  function CheckEspChangeBackwards(ProcAddr, CallAddr: TJclAddr): Boolean;
  var
    Offset: Integer;
  begin
    Inc(ProcAddr);
    Result := False;

    if ProcAddr - 3 >= CallAddr then
    begin
      ParseEspChange(ProcAddr - 3, ProcAddr + 1, Offset, Result);
      if Result then
        Exit;
    end;

    if ProcAddr - 6 >= CallAddr then
    begin
      ParseEspChange(ProcAddr - 6, ProcAddr + 1, Offset, Result);
      if Result then
        Exit;
    end;
  end;

  function CheckStackAddressValidation(ProcAddr, CallAddr: TJclAddr; var LocalVarStackOffset: Integer;
    var EspChangeFound: Boolean): Integer;
  var
    P: PByteArray;
  begin
    // The compiler emits multiple functino prologues to probe the stack frame memory pages.

    P := PByteArray(ProcAddr);
    if (ProcAddr + 6 < CallAddr) and
       (P[0] = $81) and (P[1] = $C4) and (PInteger(@P[2])^ = -4092) and     // 81C404F0FFFF     add esp,$fffff004
       (P[6] = $50) then                                                    // 50               push eax
    begin
      Inc(LocalVarStackOffset, (4092+4));
      EspChangeFound := True;
      Result := 7;
    end
    else if (ProcAddr + 8 < CallAddr) and                                   // CompilerSpeedPack option -x-fpr
       (P[0] = $81) and (P[1] = $C4) and (PInteger(@P[2])^ = -4096) and     // 81C404F0FFFF     add esp,$fffff000
       (P[6] = $85) and (P[7] = $24) and (P[8] = $24) then                  // 852424           test [esp],esp
    begin
      Inc(LocalVarStackOffset, 4096);
      EspChangeFound := True;
      Result := 9;
    end

    else if (ProcAddr + 17 + 4 < CallAddr) and
       (P[0] = $50) and                                                     // 50               push eax
       (P[1] = $B8) and                                                     // B804000000       mov eax,imm32
       (P[6] = $81) and (P[7] = $C4) and (PInteger(@P[8])^ = -4092) and     // 81C404F0FFFF     add esp,$fffff004
       (P[12] = $50) and                                                    // 50               push eax
       (P[13] = $48) and                                                    // 48               dec eax
       (P[14] = $75) and (P[15] = $F6) and                                  // 75F6             jnz -10
       (P[16] = $8B) and ((PWord(@P[16])^ = $2484) or (P[17] = $45)) then   // 8B842400000100   mov eax,[esp+imm32] / 8B45FC mov eax,[ebp-imm8]
   begin
      Inc(LocalVarStackOffset, PInteger(@P[2])^ * (4092+4));
      EspChangeFound := True;
      Result := 19;
      if P[17] = $45 then
        Inc(Result, 1)  // 8B45FC           mov eax,[ebp-imm8]
      else
        Inc(Result, 4); // 8B842400000100   mov eax,[esp+imm32]
    end

    else if (ProcAddr + 20 + 4 < CallAddr) and                              // CompilerSpeedPack option -x-fpr
       (P[0] = $50) and                                                     // 50               push eax
       (P[1] = $B8) and                                                     // B804000000       mov eax,imm32
       (P[6] = $81) and (P[7] = $C4) and (PInteger(@P[8])^ = -4096) and     // 81C404F0FFFF     add esp,$fffff000
       (P[12] = $85) and (P[13] = $24) and (P[14] = $24) and                // 852424           test [esp],esp
       (P[15] = $48) and                                                    // 48               dec eax
       (P[16] = $75) and (P[17] = $F6) and                                  // 75F6             jnz -10
       (P[18] = $8B) and ((PWord(@P[19])^ = $2484) or (P[19] = $45)) then   // 8B842400000100   mov eax,[esp+imm32] / 8B45FC mov eax,[ebp-imm8]
    begin
      Inc(LocalVarStackOffset, PInteger(@P[2])^ * 4096);
      EspChangeFound := True;
      Result := 21;
      if P[19] = $45 then
        Inc(Result, 1)  // 8B45FC           mov eax,[ebp-imm8]
      else
        Inc(Result, 4); // 8B842400000100   mov eax,[esp+imm32]
    end

    else if (ProcAddr + 2 < CallAddr) and
       (P[0] = $33) and (P[1] = $C9) and                                   // 33C9             xor ecx,ecx
       (P[2] = $51) then                                                   // 51               push ecx
    begin
      Inc(LocalVarStackOffset, 4);
      EspChangeFound := True;

      Result := 1;
      Inc(ProcAddr, 3);
      while (ProcAddr + 2 < CallAddr) and (PByte(ProcAddr)^ = $51) do
      begin
        Inc(ProcAddr);
        Inc(Result);
      end;
      Inc(LocalVarStackOffset, 4 * Result);

      Inc(Result, 2); // xor ecx, ecx
    end

    // Compiler sets the stack for managed local variables to zero
    else if (ProcAddr + 12 < CallAddr) and
       (P[0] = $51) and                                                     // 51               push ecx
       (P[1] = $B9) and // imm32                                            // B906000000       mov ecx,imm32
       (P[6] = $6A) and (P[7] = $00) and                                    // 6A00             push $00
       (P[8] = $6A) and (P[9] = $00) and                                    // 6A00             push $00
       (P[10] = $49) and                                                    // 49               dec ecx
       (P[11] = $75) and (P[12] = $F9) then                                 // 75F9             jnz -7
    begin
      Inc(LocalVarStackOffset, PInteger(@P[2])^ * PointerSize * 2);
      EspChangeFound := True;
      Result := 13;

      // For an odd number of local DWORDs the compiler emits an additional "push ecx"
      if (ProcAddr + 13 < CallAddr) and
         (P[13] = $51) then                                                 // 51               push ecx
      begin
        Inc(LocalVarStackOffset, PointerSize);
        Inc(Result, 1);
      end;

      if (ProcAddr + TJclAddr(Result) + 3 < CallAddr) and
         (P[Result + 0] = $87) and (P[Result + 1] = $4D) then // imm8      // 874DFC           xchg [ebp-imm8],ecx
      begin
        Inc(Result, 3);
      end
      else if (ProcAddr + TJclAddr(Result) + 10 < CallAddr) and                        // CompilerSpeedPack option -x-fpr
         (P[Result + 0] = $8B) and (P[Result + 1] = $4D) and //imm8                    // 8B4DFC           mov ecx,[ebp-imm8]
         (P[Result + 3] = $C7) and (P[Result + 4] = $45) and (P[Result + 5] = $FC) and // C745FC00000000   mov [ebp-$04],$00000000
         (PInteger(@P[Result + 6])^ = 0) then
      begin
        Inc(Result, 10);
      end;
    end
    else
      Result := 0;
  end;

var
  P: PByteArray;
  ProcAddr, CallAddr, EpilogAddr: TJclAddr;
  StackFrameFound: Integer;
  RegisterStackOffset: Integer;
  EspChangeFound: Boolean;
  Size: Integer;
  PossibleEndFound: Boolean;
  EpilogInfo: TJclLocationInfo;
  RegStackOffset: Integer;
begin
  LocalVarStackOffset := 0;
  ParamStackOffset := 0;
  RegisterStackOffset := 0;

  Result := False;
  if Proc = nil then
    Exit;

  ProcAddr := TJclAddr(Proc);
  CallAddr := TJclAddr(InstructionAddr);

  // Prolog: stackframe
  StackFrameFound := 0;
  EspChangeFound := False;
  if ProcAddr < CallAddr then
  begin
    P := PByteArray(ProcAddr);
    if (P[0] = $55) and                                           // PUSH EBP
       (P[1] = $8B) and (P[2] = $EC) then                         // MOV EBP,ESP
    begin
      LocalVarStackOffset := PointerSize; // EBP
      StackFrameFound := 1; // Epilog must end with "POP EBP"
      Inc(ProcAddr, 3);
    end
    else if (P[0] = $C8) and (ProcAddr + 4 < CallAddr) then  // ENTER Size(Word), NestingLevel(Byte)
    begin
      LocalVarStackOffset := PointerSize + PWord(@P[1])^ + PointerSize*P[3]; // EBP + Size + 4*NestingLevel
      StackFrameFound := -1; // Epilog must end with "LEAVE"
      Inc(ProcAddr, 4);
    end;
  end;

  if StackFrameFound = 0 then
  begin
    // Prolog: save registers

    // If we have no stackframe, then the compiler saves the registers before allocating stack variables.

    // RegisterStackOffset is preliminary because it may be reset by Epilog's POP code that is more
    // accurate because we can't distinguish between the save register and an immediatelly following
    // function parameter "PUSH".
    ProcAddr := ParseSaveRegisters(ProcAddr, CallAddr, {var} RegisterStackOffset);

    // Prolog: no stackframe + stack address validation
    Size := 0;
    if RegisterStackOffset >= PointerSize then
    begin
      // If there is a "push eax", then the ParseSaveRegisters handled it, but it may be the
      // stack validation's "push eax".
      Size := CheckStackAddressValidation(ProcAddr - 1{push eax}, CallAddr, {var} LocalVarStackOffset, {var} EspChangeFound);
      if Size > 0 then
      begin
        Dec(RegisterStackOffset, PointerSize);
        Dec(ProcAddr);
      end;
    end;
    if Size = 0 then
      Size := CheckStackAddressValidation(ProcAddr, CallAddr, {var} LocalVarStackOffset, {var} EspChangeFound);
    Inc(ProcAddr, Size);

    ProcAddr := ParseEspChange(ProcAddr, CallAddr, LocalVarStackOffset, {var} EspChangeFound);
  end
  else
  begin
    // Prolog: stackframe + stack address validation
    Size := CheckStackAddressValidation(ProcAddr, CallAddr, {var} LocalVarStackOffset, {var} EspChangeFound);
    Inc(ProcAddr, Size);

    ProcAddr := ParseEspChange(ProcAddr, CallAddr, LocalVarStackOffset, {var} EspChangeFound);

    // If we have a stackframe, then the compiler saves the registers after allocating stack variables.
    ProcAddr := ParseSaveRegisters(ProcAddr, CallAddr, {var} RegisterStackOffset);
  end;

  // Find not closed try/finally/except blocks and add them the LocalVarStackOffset
  while (ProcAddr < CallAddr) and (ProcAddr < ModuleEndAddr) do
  begin
    // fast forward find for XOR EAX,EAX
    while (ProcAddr < CallAddr) and (ProcAddr < ModuleEndAddr) and (PByteArray(ProcAddr)[0] <> $33) do
      Inc(ProcAddr);

    P := PByteArray(ProcAddr);
    // Find all occurrences above the CallAddr and add to LocalVarStackOffset (3*PointerSize)
    // "try"
    // 33C0             xor eax,eax
    // 55               push ebp
    // 68E9E05000       push $0050e0e9
    // 64FF30           push dword ptr fs:[eax]
    // 648920           mov fs:[eax],esp
    if (ProcAddr + 13 < CallAddr) and
       (P[0] = $33) and (P[1] = $C0) and
       (P[2] = $55) and
       (P[3] = $68) and
       (P[8] = $64) and (P[9] = $FF) and (P[10] = $30) and
       (P[11] = $64) and (P[12] = $89) and (P[13] = $20) then
    begin
      Inc(LocalVarStackOffset, 3 * PointerSize);
    end
    // "finally"/"except"
    // Find all occurrences above the CallAddr and substract from LocalVarStackOffset (3*PointerSize)
    // 33C0             xor eax,eax
    // 5A               pop edx
    // 59               pop ecx
    // 59               pop ecx
    // 648910           mov fs:[eax],edx
    else if (ProcAddr + 7 < CallAddr) and
       (P[0] = $33) and (P[1] = $C0) and
       (P[2] = $5A) and
       (P[3] = $59) and
       (P[4] = $59) and
       (P[5] = $64) and (P[6] = $89) and (P[7] = $10) then
    begin
      Dec(LocalVarStackOffset, 3 * PointerSize);
    end;

    Inc(ProcAddr);
  end;


  // Find the epilog to obtain the ParamStackOffset (would be much easier and less guess work
  // if we knew the exact function's end address)
  ProcAddr := CallAddr;
  while ProcAddr < ModuleEndAddr do
  begin
    // fast forward find for RET / RET imm16
    while (ProcAddr < ModuleEndAddr) and not (PByteArray(ProcAddr)[0] in [$C3, $C2]) do
      Inc(ProcAddr);

    P := PByteArray(ProcAddr);
    // We may have found the RET of a finally clause
    if (ProcAddr + 7 < ModuleEndAddr) and // skip "finally" code
       (P[0] = $C3) and                            // C3               ret
       (P[1] = $E9) and                            // E91821FAFF       jmp @HandleFinally
       (P[6] = $EB) and (ShortInt(P[7]) < 0) then  // EBF8             jmp imm8
    begin
      Inc(ProcAddr, 8);
    end
    else if (ProcAddr + 10 < ModuleEndAddr) and // skip "finally" code
       (P[0] = $C3) and                               // C3               ret
       (P[1] = $E9) and                               // E91821FAFF       jmp @HandleFinally
       (P[6] = $E9) and (PInteger(@P[7])^ < 0) then   // E9xxxxxxxx       jmp imm32
    begin
      Inc(ProcAddr, 11);
    end
    else if (P[0] = $C3) or ((P[0] = $C2) and (ProcAddr + 3 < ModuleEndAddr)) then
    begin
      EpilogAddr := ProcAddr;
      PossibleEndFound := False;
      if StackFrameFound = 1 then
      begin
        // If we have a stackframe, then we verify that the stackframe is cleared to check
        // if we found a valid "RET"
        if EspChangeFound then
          EpilogAddr := EpilogAddr - 3
        else
          EpilogAddr := EpilogAddr - 1;

        if EpilogAddr >= CallAddr then
        begin
          P := PByteArray(EpilogAddr);
          if EspChangeFound and
             (P[0] = $8B) and (P[1] = $E5) and  // 8BE5             mov esp,ebp
             (P[2] = $5D) then                  // 5D               pop ebp
          begin
            Dec(EpilogAddr);
            PossibleEndFound := True;
          end
          else if not EspChangeFound and
             (P[0] = $5D) then                  // 5D               pop ebp
          begin
            Dec(EpilogAddr);
            PossibleEndFound := True;
          end;
        end;
      end
      else if StackFrameFound = -1 then
      begin
        // If we have a ENTER/LEAVE stackframe, then we verify that the stackframe is cleared
        // to check if we found a valid "RET"
        Dec(EpilogAddr);
        P := PByteArray(EpilogAddr);
        if (EpilogAddr >= CallAddr) and (P[0] = $C9) then  // LEAVE
        begin
          Dec(EpilogAddr);
          PossibleEndFound := True;
        end;
      end
      else
      begin
        // If we have no stackframe, then we can't verify the validity of the "RET" here
        EpilogAddr := EpilogAddr - 1;
        PossibleEndFound := True;
      end;

      if PossibleEndFound then
      begin
        if GetLocationInfo(Pointer(EpilogAddr), EpilogInfo) and
           (TJclAddr(EpilogInfo.OffsetFromProcName) <> EpilogAddr - TJclAddr(Proc)) then
        begin
          // If we didn't find a RET in the same procedure then the analysis failed
          Exit;
        end;

        if PossibleEndFound then
        begin
          // If we have registers saved on the stack, we can use those to verify if the
          // found "RET" is valid.
          RegStackOffset := RegisterStackOffset;
          if CheckRegisterRestoreBackwards(EpilogAddr, CallAddr, {var} RegStackOffset) then
          begin
            if (StackFrameFound = 0) and EspChangeFound then
            begin
              // If we have local variables (ESP was changed in the prolog) we can use that
              // information to verify the "RET"
              EpilogAddr := EpilogAddr - TJclAddr(RegStackOffset) div PointerSize;
              if not CheckEspChangeBackwards(EpilogAddr, CallAddr) then
                PossibleEndFound := False;
            end;

            if PossibleEndFound then
            begin
              RegisterStackOffset := RegStackOffset;
              if PByte(ProcAddr)^ = $C2 then
                ParamStackOffset := PWord(ProcAddr + 1)^
              else
              begin
                // TODO: if we only have a "RET" at the end we need to look at the call instruction
                // if it is followed by a "sub/add esp,xx" for a "cdecl" function. (What if the add/sub
                // is for the caller's epilog?)
              end;
              Break;
            end;
          end;
        end;
      end;
    end;
    Inc(ProcAddr);
  end;

  Inc(LocalVarStackOffset, RegisterStackOffset);

  Result := True;
end;

procedure TJclStackInfoList.TraceStackRaw;
var
  StackInfo: TStackInfo;
  StackPtr: PJclAddr;
  PrevCaller: TJclAddr;
  CallInstructionSize: Cardinal;
  StackTop: TJclAddr;
  ProcInfo: TJclLocationInfo;
  ProcStart: Pointer;
  CallInstructionPtr: Pointer;
  LocalVarStackOffset, ParamStackOffset: Integer;
  ModuleEndAddr: TJclAddr;
begin
  Capacity := 32; // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  if DelayedTrace then
  begin
    if not Assigned(FStackData) then
      Exit;
    StackPtr := PJclAddr(FStackData);
  end
  else
  begin
    // We define the bottom of the valid stack to be the current ESP pointer
    if BaseOfStack = 0 then
      BaseOfStack := TJclAddr(GetStackPointer);
    // Get a pointer to the current bottom of the stack
    StackPtr := PJclAddr(BaseOfStack);
  end;

  StackTop := TopOfStack;

  // We will not be able to fill in all the fields in the StackInfo record,
  // so just blank it all out first
  ResetMemory(StackInfo, SizeOf(StackInfo));
  // Clear the previous call address
  PrevCaller := 0;
  // stCleanRawStack: We don't know the number of parameters for the "initial" function
  ParamStackOffset := 0;
  if stCleanRawStack in JclStackTrackingOptions then
    BeginGetLocationInfoCache; // speed up the GetLocationInfo calls
  // Loop through all of the valid stack space
  try
    while (TJclAddr(StackPtr) < StackTop) and (inherited Count <> MaxStackTraceItems) do
    begin
      // If the current DWORD on the stack refers to a valid call site...
      if ValidCallSite(StackPtr^, CallInstructionSize) and (StackPtr^ <> PrevCaller) then
      begin
        // then pick up the callers address
        StackInfo.CallerAddr := StackPtr^ - CallInstructionSize;
        // remember to callers address so that we don't report it repeatedly
        PrevCaller := StackPtr^;
        // increase the stack level
        Inc(StackInfo.Level);
        // then report it back to our caller
        StoreToList(StackInfo);

        if stCleanRawStack in JclStackTrackingOptions then
        begin
          // Skip all stack parameters of the last called function
          Inc(PByte(StackPtr), ParamStackOffset);
          ParamStackOffset := 0;

          CallInstructionPtr := Pointer(StackInfo.CallerAddr);
          if GetLocationInfo(CallInstructionPtr, ProcInfo) then
          begin
            if ProcInfo.ProcedureName <> '' then
            begin
              if (ProcInfo.ProcedureName[1] = '@') and (ProcInfo.ProcedureName = '@RaiseExcept$qqrv') then
              begin
                // Special handling for _RaiseExcept because it does a lot to the stack including
                // putting the ExceptAddr multiple times on the stack causing TraceStackInstuctions to
                // change the StackPtr to the wrong locations.
                LocalVarStackOffset := 17 * SizeOf(Pointer);
                ParamStackOffset := 6 * SizeOf(Pointer);

                Inc(PByte(StackPtr), LocalVarStackOffset);
              end
              else
              begin
                ProcStart := Pointer(TJclAddr(CallInstructionPtr) - TJclAddr(ProcInfo.OffsetFromProcName));
                ModuleEndAddr := TJclAddr(ProcInfo.DebugInfo.Module) + ModuleCodeOffset + TJclAddr(ProcInfo.DebugInfo.ModuleCodeSize);
                if TraceStackInstuctions(ProcStart, CallInstructionPtr, ModuleEndAddr, LocalVarStackOffset, ParamStackOffset) then
                  Inc(PByte(StackPtr), LocalVarStackOffset) // skip all local variables (and saved registers)
                else
                  ParamStackOffset := 0; // Don't skip stack entries if TraceStackInstuctions failed
              end;
            end;
          end;
        end;
      end;
      // Look at the next DWORD on the stack
      Inc(StackPtr);
    end;
  finally
    if stCleanRawStack in JclStackTrackingOptions then
      EndGetLocationInfoCache;

    if Assigned(FStackData) then
    begin
      FreeMem(FStackData);
      FStackData := nil;
    end;
  end;
end;

{$IFDEF CPU32}
procedure TJclStackInfoList.DelayStoreStack;
var
  StackPtr: PJclAddr;
  StackDataSize: Cardinal;
begin
  if Assigned(FStackData) then
  begin
    FreeMem(FStackData);
    FStackData := nil;
  end;
  // We define the bottom of the valid stack to be the current ESP pointer
  if BaseOfStack = 0 then
  begin
    BaseOfStack := TJclAddr(GetStackPointer);
    FFramePointer := GetFramePointer;
  end;

  // Get a pointer to the current bottom of the stack
  StackPtr := PJclAddr(BaseOfStack);
  if TJclAddr(StackPtr) < TopOfStack then
  begin
    StackDataSize := TopOfStack - TJclAddr(StackPtr);
    GetMem(FStackData, StackDataSize);
    System.Move(StackPtr^, FStackData^, StackDataSize);
  end;

  FStackOffset := Int64(FStackData) - Int64(StackPtr);
  FFramePointer := Pointer(TJclAddr(FFramePointer) + FStackOffset);
  TopOfStack := TopOfStack + FStackOffset;
end;
{$ENDIF CPU32}

// Validate that the code address is a valid code site
//
// Information from Intel Manual 24319102(2).pdf, Download the 6.5 MBs from:
// http://developer.intel.com/design/pentiumii/manuals/243191.htm
// Instruction format, Chapter 2 and The CALL instruction: page 3-53, 3-54

function TJclStackInfoList.ValidCallSite(CodeAddr: TJclAddr; out CallInstructionSize: Cardinal): Boolean;
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
  RM1, RM2, RM5: Byte;
begin
  // todo: 64 bit version

  // First check that the address is within range of our code segment!
  Result := CodeAddr > 8;
  if Result then
  begin
    C8P := PDWORD(CodeAddr - 8);
    C4P := PDWORD(CodeAddr - 4);
    Result := ValidCodeAddr(TJclAddr(C8P), FModuleInfoList) and not IsBadReadPtr(C8P, 8);

    // Now check to see if the instruction preceding the return address
    // could be a valid CALL instruction
    if Result then
    begin
      try
        CodeDWORD8 := PDWORD(C8P)^;
        CodeDWORD4 := PDWORD(C4P)^;
        // CodeDWORD8 = (ReturnAddr-5):(ReturnAddr-6):(ReturnAddr-7):(ReturnAddr-8)
        // CodeDWORD4 = (ReturnAddr-1):(ReturnAddr-2):(ReturnAddr-3):(ReturnAddr-4)

        // ModR/M bytes contain the following bits:
        // Mod        = (76)
        // Reg/Opcode = (543)
        // R/M        = (210)
        RM1 := (CodeDWORD4 shr 24) and $7;
        RM2 := (CodeDWORD4 shr 16) and $7;
        //RM3 := (CodeDWORD4 shr 8)  and $7;
        //RM4 :=  CodeDWORD4         and $7;
        RM5 := (CodeDWORD8 shr 24) and $7;
        //RM6 := (CodeDWORD8 shr 16) and $7;
        //RM7 := (CodeDWORD8 shr 8)  and $7;

        // Check the instruction prior to the potential call site.
        // We consider it a valid call site if we find a CALL instruction there
        // Check the most common CALL variants first
        if ((CodeDWORD8 and $FF000000) = $E8000000) then
          // 5 bytes, "CALL NEAR REL32" (E8 cd)
          CallInstructionSize := 5
        else
        if ((CodeDWORD4 and $F8FF0000) = $10FF0000) and not (RM1 in [4, 5]) then
          // 2 bytes, "CALL NEAR [EAX]" (FF /2) where Reg = 010, Mod = 00, R/M <> 100 (1 extra byte)
          // and R/M <> 101 (4 extra bytes)
          CallInstructionSize := 2
        else
        if ((CodeDWORD4 and $F8FF0000) = $D0FF0000) then
          // 2 bytes, "CALL NEAR EAX" (FF /2) where Reg = 010 and Mod = 11
          CallInstructionSize := 2
        else
        if ((CodeDWORD4 and $00FFFF00) = $0014FF00) then
          // 3 bytes, "CALL NEAR [EAX+EAX*i]" (FF /2) where Reg = 010, Mod = 00 and RM = 100
          // SIB byte not validated
          CallInstructionSize := 3
        else
        if ((CodeDWORD4 and $00F8FF00) = $0050FF00) and (RM2 <> 4) then
          // 3 bytes, "CALL NEAR [EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM <> 100 (1 extra byte)
          CallInstructionSize := 3
        else
        if ((CodeDWORD4 and $0000FFFF) = $000054FF) then
          // 4 bytes, "CALL NEAR [EAX+EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM = 100
          // SIB byte not validated
          CallInstructionSize := 4
        else
        if ((CodeDWORD8 and $FFFF0000) = $15FF0000) then
          // 6 bytes, "CALL NEAR [$12345678]" (FF /2) where Reg = 010, Mod = 00 and RM = 101
          CallInstructionSize := 6
        else
        if ((CodeDWORD8 and $F8FF0000) = $90FF0000) and (RM5 <> 4) then
          // 6 bytes, "CALL NEAR [EAX+$12345678]" (FF /2) where Reg = 010, Mod = 10 and RM <> 100 (1 extra byte)
          CallInstructionSize := 6
        else
        if ((CodeDWORD8 and $00FFFF00) = $0094FF00) then
          // 7 bytes, "CALL NEAR [EAX+EAX+$1234567]" (FF /2) where Reg = 010, Mod = 10 and RM = 100
          CallInstructionSize := 7
        else
        if ((CodeDWORD8 and $0000FF00) = $00009A00) then
          // 7 bytes, "CALL FAR $1234:12345678" (9A ptr16:32)
          CallInstructionSize := 7
        else
          Result := False;
        // Because we're not doing a complete disassembly, we will potentially report
        // false positives. If there is odd code that uses the CALL 16:32 format, we
        // can also get false negatives.
      except
        Result := False;
      end;
    end;
  end;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

function TJclStackInfoList.ValidStackAddr(StackAddr: TJclAddr): Boolean;
begin
  Result := (BaseOfStack < StackAddr) and (StackAddr < TopOfStack);
end;

//=== Exception frame info routines ==========================================

function JclCreateExceptFrameList(AIgnoreLevels: Integer): TJclExceptFrameList;
begin
  Result := TJclExceptFrameList.Create(AIgnoreLevels);
  GlobalStackList.AddObject(Result);
end;

function JclLastExceptFrameList: TJclExceptFrameList;
begin
  Result := GlobalStackList.LastExceptFrameList[GetCurrentThreadID];
end;

function JclGetExceptFrameList(ThreadID: DWORD): TJclExceptFrameList;
begin
  Result := GlobalStackList.LastExceptFrameList[ThreadID];
end;

procedure DoExceptFrameTrace;
begin
  // Ignore first 2 levels; the First level is an undefined frame (I haven't a
  // clue as to where it comes from. The second level is the try..finally block
  // in DoExceptNotify.
  JclCreateExceptFrameList(4);
end;

{$OVERFLOWCHECKS OFF}

function GetJmpDest(Jmp: PJmpInstruction): Pointer;
begin
  // TODO : 64 bit version
  if Jmp^.opCode = $E9 then
    Result := Pointer(TJclAddr(Jmp) + TJclAddr(Jmp^.distance) + 5)
  else
  if Jmp.opCode = $EB then
    Result := Pointer(TJclAddr(Jmp) + TJclAddr(ShortInt(Jmp^.distance)) + 2)
  else
    Result := nil;
  if (Result <> nil) and (PJmpTable(Result).OPCode = $25FF) then
    if not IsBadReadPtr(PJmpTable(Result).Ptr, SizeOf(Pointer)) then
      Result := Pointer(PJclAddr(PJmpTable(Result).Ptr)^);
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//=== { TJclExceptFrame } ====================================================

constructor TJclExceptFrame.Create(AFrameLocation: Pointer; AExcDesc: PExcDesc);
begin
  inherited Create;
  FFrameKind := efkUnknown;
  FFrameLocation := AFrameLocation;
  FCodeLocation := nil;
  AnalyseExceptFrame(AExcDesc);
end;

{$RANGECHECKS OFF}

procedure TJclExceptFrame.AnalyseExceptFrame(AExcDesc: PExcDesc);
var
  Dest: Pointer;
  LocInfo: TJclLocationInfo;
  FixedProcedureName: string;
  DotPos, I: Integer;
begin
  Dest := GetJmpDest(@AExcDesc^.Jmp);
  if Dest <> nil then
  begin
    // get frame kind
    LocInfo := GetLocationInfo(Dest);
    if CompareText(LocInfo.UnitName, 'system') = 0 then
    begin
      FixedProcedureName := LocInfo.ProcedureName;
      DotPos := Pos('.', FixedProcedureName);
      if DotPos > 0 then
        FixedProcedureName := Copy(FixedProcedureName, DotPos + 1, Length(FixedProcedureName) - DotPos);
      if CompareText(FixedProcedureName, '@HandleAnyException') = 0 then
        FFrameKind := efkAnyException
      else
      if CompareText(FixedProcedureName, '@HandleOnException') = 0 then
        FFrameKind := efkOnException
      else
      if CompareText(FixedProcedureName, '@HandleAutoException') = 0 then
        FFrameKind := efkAutoException
      else
      if CompareText(FixedProcedureName, '@HandleFinally') = 0 then
        FFrameKind := efkFinally;
    end;

    // get location
    if FFrameKind <> efkUnknown then
    begin
      FCodeLocation := GetJmpDest(PJmpInstruction(TJclAddr(@AExcDesc^.Instructions)));
      if FCodeLocation = nil then
        FCodeLocation := @AExcDesc^.Instructions;
    end
    else
    begin
      FCodeLocation := GetJmpDest(PJmpInstruction(TJclAddr(AExcDesc)));
      if FCodeLocation = nil then
        FCodeLocation := AExcDesc;
    end;

    // get on handlers
    if FFrameKind = efkOnException then
    begin
      SetLength(FExcTab, AExcDesc^.Cnt);
      for I := 0 to AExcDesc^.Cnt - 1 do
      begin
        if AExcDesc^.ExcTab[I].VTable = nil then
        begin
          SetLength(FExcTab, I);
          Break;
        end
        else
          FExcTab[I] := AExcDesc^.ExcTab[I];
      end;
    end;
  end;
end;

{$IFDEF RANGECHECKS_ON}
{$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ON}

function TJclExceptFrame.Handles(ExceptObj: TObject): Boolean;
var
  Handler: Pointer;
begin
  Result := HandlerInfo(ExceptObj, Handler);
end;

{$OVERFLOWCHECKS OFF}

function TJclExceptFrame.HandlerInfo(ExceptObj: TObject; out HandlerAt: Pointer): Boolean;
var
  I: Integer;
  ObjVTable, VTable, ParentVTable: Pointer;
begin
  Result := FrameKind in [efkAnyException, efkAutoException];
  if not Result and (FrameKind = efkOnException) then
  begin
    HandlerAt := nil;
    ObjVTable := Pointer(ExceptObj.ClassType);
    for I := Low(FExcTab) to High(FExcTab) do
    begin
      VTable := ObjVTable;
      Result := FExcTab[I].VTable = nil;
      while (not Result) and (VTable <> nil) do
      begin
        Result := (FExcTab[I].VTable = VTable) or
          (PShortString(PPointer(PJclAddr(FExcTab[I].VTable)^ + TJclAddr(vmtClassName))^)^ =
           PShortString(PPointer(TJclAddr(VTable) + TJclAddr(vmtClassName))^)^);
        if Result then
          HandlerAt := FExcTab[I].Handler
        else
        begin
          ParentVTable := TClass(VTable).ClassParent;
          if ParentVTable = VTable then
            VTable := nil
          else
            VTable := ParentVTable;
        end;
      end;
      if Result then
        Break;
    end;
  end
  else
  if Result then
    HandlerAt := FCodeLocation
  else
    HandlerAt := nil;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//=== { TJclExceptFrameList } ================================================

constructor TJclExceptFrameList.Create(AIgnoreLevels: Integer);
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  TraceExceptionFrames;
end;

function TJclExceptFrameList.AddFrame(AFrame: PExcFrame): TJclExceptFrame;
begin
  Result := TJclExceptFrame.Create(AFrame, AFrame^.Desc);
  Add(Result);
end;

function TJclExceptFrameList.GetItems(Index: Integer): TJclExceptFrame;
begin
  Result := TJclExceptFrame(Get(Index));
end;

procedure TJclExceptFrameList.TraceExceptionFrames;
{$IFDEF CPU32}
var
  ExceptionPointer: PExcFrame;
  Level: Integer;
  ModulesList: TJclModuleInfoList;
begin
  Clear;
  ModulesList := GlobalModulesList.CreateModulesList;
  try
    Level := 0;
    ExceptionPointer := GetExceptionPointer;
    while TJclAddr(ExceptionPointer) <> High(TJclAddr) do
    begin
      if (Level >= IgnoreLevels) and ValidCodeAddr(TJclAddr(ExceptionPointer^.Desc), ModulesList) then
        AddFrame(ExceptionPointer);
      Inc(Level);
      ExceptionPointer := ExceptionPointer^.next;
    end;
  finally
    GlobalModulesList.FreeModulesList(ModulesList);
  end;
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
begin
  // TODO: 64-bit version
end;
{$ENDIF CPU64}

//=== Exception hooking ======================================================

var
  TrackingActiveCount: Integer;
  IgnoredExceptions: TThreadList = nil;
  IgnoredExceptionClassNames: TStringList = nil;
  IgnoredExceptionClassNamesCritSect: TJclCriticalSection = nil;

procedure AddIgnoredException(const ExceptionClass: TClass);
begin
  if Assigned(ExceptionClass) then
  begin
    if not Assigned(IgnoredExceptions) then
      IgnoredExceptions := TThreadList.Create;

    IgnoredExceptions.Add(ExceptionClass);
  end;
end;

procedure AddIgnoredExceptionByName(const AExceptionClassName: string);
begin
  if AExceptionClassName <> '' then
  begin
    if not Assigned(IgnoredExceptionClassNamesCritSect) then
      IgnoredExceptionClassNamesCritSect := TJclCriticalSection.Create;
    if not Assigned(IgnoredExceptionClassNames) then
    begin
      IgnoredExceptionClassNames := TStringList.Create;
      IgnoredExceptionClassNames.Duplicates := dupIgnore;
      IgnoredExceptionClassNames.Sorted := True;
    end;
    IgnoredExceptionClassNamesCritSect.Enter;
    try
      IgnoredExceptionClassNames.Add(AExceptionClassName);
    finally
      IgnoredExceptionClassNamesCritSect.Leave;
    end;
  end;
end;

procedure RemoveIgnoredException(const ExceptionClass: TClass);
var
  ClassList: TList;
begin
  if Assigned(ExceptionClass) and Assigned(IgnoredExceptions) then
  begin
    ClassList := IgnoredExceptions.LockList;
    try
      ClassList.Remove(ExceptionClass);
    finally
      IgnoredExceptions.UnlockList;
    end;
  end;
end;

procedure RemoveIgnoredExceptionByName(const AExceptionClassName: string);
var
  Index: Integer;
begin
  if Assigned(IgnoredExceptionClassNames) and (AExceptionClassName <> '') then
  begin
    IgnoredExceptionClassNamesCritSect.Enter;
    try
      Index := IgnoredExceptionClassNames.IndexOf(AExceptionClassName);
      if Index <> -1 then
        IgnoredExceptionClassNames.Delete(Index);
    finally
      IgnoredExceptionClassNamesCritSect.Leave;
    end;
  end;
end;

function IsIgnoredException(const ExceptionClass: TClass): Boolean;
var
  ClassList: TList;
  Index: Integer;
begin
  Result := False;
  if Assigned(IgnoredExceptions) and not (stTraceAllExceptions in JclStackTrackingOptions) then
  begin
    ClassList := IgnoredExceptions.LockList;
    try
      for Index := 0 to ClassList.Count - 1 do
        if ExceptionClass.InheritsFrom(TClass(ClassList.Items[Index])) then
      begin
        Result := True;
        Break;
      end;
    finally
      IgnoredExceptions.UnlockList;
    end;
  end;
  if not Result and Assigned(IgnoredExceptionClassNames) and not (stTraceAllExceptions in JclStackTrackingOptions) then
  begin
    IgnoredExceptionClassNamesCritSect.Enter;
    try
      Result := IgnoredExceptionClassNames.IndexOf(ExceptionClass.ClassName) <> -1;
      if not Result then
        for Index := 0 to IgnoredExceptionClassNames.Count - 1 do
          if InheritsFromByName(ExceptionClass, IgnoredExceptionClassNames[Index]) then
          begin
            Result := True;
            Break;
          end;
    finally
      IgnoredExceptionClassNamesCritSect.Leave;
    end;
  end;
end;

procedure AddModule(const ModuleName: string);
begin
  GlobalModulesList.AddModule(ModuleName);
end;

procedure DoExceptNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean;
  BaseOfStack: Pointer);
begin
  if (TrackingActiveCount > 0) and (not (stDisableIfDebuggerAttached in JclStackTrackingOptions) or (not IsDebuggerAttached)) and
    Assigned(ExceptObj) and (not IsIgnoredException(ExceptObj.ClassType)) and
    (not (stMainThreadOnly in JclStackTrackingOptions) or (GetCurrentThreadId = MainThreadID)) then
  begin
    if stStack in JclStackTrackingOptions then
      DoExceptionStackTrace(ExceptObj, ExceptAddr, OSException, BaseOfStack);
    if stExceptFrame in JclStackTrackingOptions then
      DoExceptFrameTrace;
  end;
end;

function JclStartExceptionTracking: Boolean;
begin
  {Increment the tracking count only if exceptions are already being tracked or tracking can be started
   successfully.}
  if TrackingActiveCount = 0 then
  begin
    if JclHookExceptions and JclAddExceptNotifier(DoExceptNotify, npFirstChain) then
    begin
      TrackingActiveCount := 1;
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    Inc(TrackingActiveCount);
    Result := False;
  end;
end;

function JclStopExceptionTracking: Boolean;
begin
  {If the current tracking count is 1, an attempt is made to stop tracking exceptions. If successful the
   tracking count is set back to 0. If the current tracking count is > 1 it is simply decremented.}
  if TrackingActiveCount = 1 then
  begin
    Result := JclRemoveExceptNotifier(DoExceptNotify) and JclUnhookExceptions;
    if Result then
      Dec(TrackingActiveCount);
  end
  else
  begin
    if TrackingActiveCount > 0 then
      Dec(TrackingActiveCount);
    Result := False;
  end;
end;

function JclExceptionTrackingActive: Boolean;
begin
  Result := TrackingActiveCount > 0;
end;

function JclTrackExceptionsFromLibraries: Boolean;
begin
  Result := TrackingActiveCount > 0;
  if Result then
    JclInitializeLibrariesHookExcept;
end;

//=== Thread exception tracking support ======================================

var
  RegisteredThreadList: TJclDebugThreadList;

function JclDebugThreadList: TJclDebugThreadList;
begin
  if RegisteredThreadList = nil then
    RegisteredThreadList := TJclDebugThreadList.Create;
  Result := RegisteredThreadList;
end;

type
  TKernel32_CreateThread = function(SecurityAttributes: Pointer; StackSize: LongWord;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;
  TKernel32_ExitThread = procedure(ExitCode: Integer); stdcall;

var
  ThreadsHooked: Boolean;
  Kernel32_CreateThread: TKernel32_CreateThread = nil;
  Kernel32_ExitThread: TKernel32_ExitThread = nil;

function HookedCreateThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer;
  CreationFlags: LongWord; ThreadId: PLongWord): Integer; stdcall;
var
  LocalThreadId: LongWord;
begin
  Result := Kernel32_CreateThread(SecurityAttributes, StackSize, ThreadFunc, Parameter, CreationFlags, LocalThreadId);
  if Result <> 0 then
  begin
    JclDebugThreadList.RegisterThreadID(LocalThreadId);
    if ThreadId <> nil then
    begin
      ThreadId^ := LocalThreadId;
    end;
  end;
end;

procedure HookedExitThread(ExitCode: Integer); stdcall;
begin
  JclDebugThreadList.UnregisterThreadID(GetCurrentThreadID);
  Kernel32_ExitThread(ExitCode);
end;

function JclHookThreads: Boolean;
var
  ProcAddrCache: Pointer;
begin
  if not ThreadsHooked then
  begin
    ProcAddrCache := GetProcAddress(GetModuleHandle(kernel32), 'CreateThread');
    with TJclPeMapImgHooks do
      Result := ReplaceImport(SystemBase, kernel32, ProcAddrCache, @HookedCreateThread);
    if Result then
    begin
      @Kernel32_CreateThread := ProcAddrCache;

      ProcAddrCache := GetProcAddress(GetModuleHandle(kernel32), 'ExitThread');
      with TJclPeMapImgHooks do
        Result := ReplaceImport(SystemBase, kernel32, ProcAddrCache, @HookedExitThread);
      if Result then
        @Kernel32_ExitThread := ProcAddrCache
      else
      with TJclPeMapImgHooks do
        ReplaceImport(SystemBase, kernel32, @HookedCreateThread, @Kernel32_CreateThread);
    end;
    ThreadsHooked := Result;
  end
  else
    Result := True;
end;

function JclUnhookThreads: Boolean;
begin
  if ThreadsHooked then
  begin
    with TJclPeMapImgHooks do
    begin
      ReplaceImport(SystemBase, kernel32, @HookedCreateThread, @Kernel32_CreateThread);
      ReplaceImport(SystemBase, kernel32, @HookedExitThread, @Kernel32_ExitThread);
    end;
    Result := True;
    ThreadsHooked := False;
  end
  else
    Result := True;
end;

function JclThreadsHooked: Boolean;
begin
  Result := ThreadsHooked;
end;

//=== { TJclDebugThread } ====================================================

constructor TJclDebugThread.Create(ASuspended: Boolean; const AThreadName: string);
begin
  FThreadName := AThreadName;
  inherited Create(True);
  JclDebugThreadList.RegisterThread(Self, AThreadName);
  if not ASuspended then
    {$IFDEF RTL210_UP}
    Suspended := False;
    {$ELSE ~RTL210_UP}
    Resume;
    {$ENDIF ~RTL210_UP}
end;

destructor TJclDebugThread.Destroy;
begin
  JclDebugThreadList.UnregisterThread(Self);
  inherited Destroy;
end;

procedure TJclDebugThread.DoHandleException;
begin
  GlobalStackList.LockThreadID(ThreadID);
  try
    DoSyncHandleException;
  finally
    GlobalStackList.UnlockThreadID;
  end;
end;

procedure TJclDebugThread.DoNotify;
begin
  JclDebugThreadList.DoSyncException(Self);
end;

procedure TJclDebugThread.DoSyncHandleException;
begin
  // Note: JclLastExceptStackList and JclLastExceptFrameList returns information
  // for this Thread ID instead of MainThread ID here to allow use a common
  // exception handling routine easily.
  // Any other call of those JclLastXXX routines from another thread at the same
  // time will return expected information for current Thread ID.
  DoNotify;
end;

function TJclDebugThread.GetThreadInfo: string;
begin
  Result := JclDebugThreadList.ThreadInfos[ThreadID];
end;

procedure TJclDebugThread.HandleException(Sender: TObject);
begin
  FSyncException := Sender;
  try
    if not Assigned(FSyncException) then
      FSyncException := Exception(ExceptObject);
    if Assigned(FSyncException) and not IsIgnoredException(FSyncException.ClassType) then
      Synchronize(DoHandleException);
  finally
    FSyncException := nil;
  end;
end;

//=== { TJclDebugThreadList } ================================================

type
  TThreadAccess = class(TThread);

constructor TJclDebugThreadList.Create;
begin
  FLock := TJclCriticalSection.Create;
  FReadLock := TJclCriticalSection.Create;
  FList := TObjectList.Create;
  FSaveCreationStack := False;
end;

destructor TJclDebugThreadList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLock);
  FreeAndNil(FReadLock);
  inherited Destroy;
end;

function TJclDebugThreadList.AddStackListToLocationInfoList(ThreadID: DWORD; AList: TJclLocationInfoList): Boolean;
var
  I: Integer;
  List: TJclStackInfoList;
begin
  Result := False;
  FReadLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if (I <> -1) and Assigned(TJclDebugThreadInfo(FList[I]).StackList) then
    begin
      List := TJclDebugThreadInfo(FList[I]).StackList;
      AList.AddStackInfoList(List);
      Result := True;
    end;
  finally
    FReadLock.Leave;
  end;
end;

procedure TJclDebugThreadList.DoSyncException(Thread: TJclDebugThread);
begin
  if Assigned(FOnSyncException) then
    FOnSyncException(Thread);
end;

procedure TJclDebugThreadList.DoSyncThreadRegistered;
begin
  if Assigned(FOnThreadRegistered) then
    FOnThreadRegistered(FRegSyncThreadID);
end;

procedure TJclDebugThreadList.DoSyncThreadUnregistered;
begin
  if Assigned(FOnThreadUnregistered) then
    FOnThreadUnregistered(FUnregSyncThreadID);
end;

procedure TJclDebugThreadList.DoThreadRegistered(Thread: TThread);
begin
  if Assigned(FOnThreadRegistered) then
  begin
    FRegSyncThreadID := Thread.ThreadID;
    TThreadAccess(Thread).Synchronize(DoSyncThreadRegistered);
  end;
end;

procedure TJclDebugThreadList.DoThreadUnregistered(Thread: TThread);
begin
  if Assigned(FOnThreadUnregistered) then
  begin
    FUnregSyncThreadID := Thread.ThreadID;
    TThreadAccess(Thread).Synchronize(DoSyncThreadUnregistered);
  end;
end;

function TJclDebugThreadList.GetThreadClassNames(ThreadID: DWORD): string;
begin
  Result := GetThreadValues(ThreadID, 1);
end;

function TJclDebugThreadList.GetThreadCreationTime(ThreadID: DWORD): TDateTime;
var
  I: Integer;
begin
  FReadLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if I <> -1 then
      Result := TJclDebugThreadInfo(FList[I]).CreationTime
    else
      Result := 0;
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.GetThreadIDCount: Integer;
begin
  FReadLock.Enter;
  try
    Result := FList.Count;
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.GetThreadHandle(Index: Integer): THandle;
begin
  FReadLock.Enter;
  try
    Result := TJclDebugThreadInfo(FList[Index]).ThreadHandle;
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.GetThreadID(Index: Integer): DWORD;
begin
  FReadLock.Enter;
  try
    Result := TJclDebugThreadInfo(FList[Index]).ThreadID;
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.GetThreadInfos(ThreadID: DWORD): string;
begin
  Result := GetThreadValues(ThreadID, 2);
end;

function TJclDebugThreadList.GetThreadNames(ThreadID: DWORD): string;
begin
  Result := GetThreadValues(ThreadID, 0);
end;

function TJclDebugThreadList.GetThreadParentID(ThreadID: DWORD): DWORD;
var
  I: Integer;
begin
  FReadLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if I <> -1 then
      Result := TJclDebugThreadInfo(FList[I]).ParentThreadID
    else
      Result := 0;
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.GetThreadValues(ThreadID: DWORD; Index: Integer): string;
var
  I: Integer;
begin
  FReadLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if I <> -1 then
    begin
      case Index of
        0:
          Result := TJclDebugThreadInfo(FList[I]).ThreadName;
        1:
          Result := TJclDebugThreadInfo(FList[I]).ThreadClassName;
        2:
          Result := Format('%.8x [%s] "%s"', [ThreadID, TJclDebugThreadInfo(FList[I]).ThreadClassName,
            TJclDebugThreadInfo(FList[I]).ThreadName]);
      end;
    end
    else
      Result := '';
  finally
    FReadLock.Leave;
  end;
end;

function TJclDebugThreadList.IndexOfThreadID(ThreadID: DWORD): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := FList.Count - 1 downto 0 do
    if TJclDebugThreadInfo(FList[I]).ThreadID = ThreadID then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJclDebugThreadList.InternalRegisterThread(Thread: TThread; ThreadID: DWORD; const ThreadName: string);
var
  I: Integer;
  ThreadInfo: TJclDebugThreadInfo;
begin
  FLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if I = -1 then
    begin
      FReadLock.Enter;
      try
        FList.Add(TJclDebugThreadInfo.Create(GetCurrentThreadId, ThreadID, FSaveCreationStack));
        ThreadInfo := TJclDebugThreadInfo(FList.Last);
        if Assigned(Thread) then
        begin
          ThreadInfo.ThreadHandle := Thread.Handle;
          ThreadInfo.ThreadClassName := Thread.ClassName;
        end
        else
        begin
          ThreadInfo.ThreadHandle := 0;
          ThreadInfo.ThreadClassName := '';
        end;
        ThreadInfo.ThreadName := ThreadName;
      finally
        FReadLock.Leave;
      end;
      if Assigned(Thread) then
        DoThreadRegistered(Thread);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TJclDebugThreadList.InternalUnregisterThread(Thread: TThread; ThreadID: DWORD);
var
  I: Integer;
begin
  FLock.Enter;
  try
    I := IndexOfThreadID(ThreadID);
    if I <> -1 then
    begin
      if Assigned(Thread) then
        DoThreadUnregistered(Thread);
      FReadLock.Enter;
      try
        FList.Delete(I);
      finally
        FReadLock.Leave;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TJclDebugThreadList.RegisterThread(Thread: TThread; const ThreadName: string);
begin
  InternalRegisterThread(Thread, Thread.ThreadID, ThreadName);
end;

procedure TJclDebugThreadList.RegisterThreadID(AThreadID: DWORD; const ThreadName: string);
begin
  InternalRegisterThread(nil, AThreadID, ThreadName);
end;

procedure TJclDebugThreadList.UnregisterThread(Thread: TThread);
begin
  InternalUnregisterThread(Thread, Thread.ThreadID);
end;

procedure TJclDebugThreadList.UnregisterThreadID(AThreadID: DWORD);
begin
  InternalUnregisterThread(nil, AThreadID);
end;

//=== { TJclDebugThreadInfo } ================================================

constructor TJclDebugThreadInfo.Create(AParentThreadID, AThreadID: DWORD; AStack: Boolean);
begin
  FCreationTime := Now;
  FParentThreadID := AParentThreadID;
  try
  { TODO -oUSc : ... }
//    FStackList := JclCreateStackList(True, 0, nil, True);//probably IgnoreLevels = 11
    if AStack then
      FStackList := TJclStackInfoList.Create(True, 0, nil, True, nil, nil)
    else
      FStackList := nil;
  except
    FStackList := nil;
  end;
  FThreadID := AThreadID;
end;

destructor TJclDebugThreadInfo.Destroy;
begin
  FStackList.Free;
  inherited Destroy;
end;

//=== { TJclCustomThreadInfo } ===============================================

constructor TJclCustomThreadInfo.Create;
var
  StackClass: TJclCustomLocationInfoListClass;
begin
  inherited Create;
  StackClass := GetStackClass;
  FCreationTime := 0;
  FCreationStack := StackClass.Create;
  FName := '';
  FParentThreadID := 0;
  FStack := StackClass.Create;
  FThreadID := 0;
  FValues := [];
end;

destructor TJclCustomThreadInfo.Destroy;
begin
  FCreationStack.Free;
  FStack.Free;
  inherited Destroy;
end;

procedure TJclCustomThreadInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TJclCustomThreadInfo then
  begin
    TJclCustomThreadInfo(Dest).FCreationTime := FCreationTime;
    TJclCustomThreadInfo(Dest).FCreationStack.Assign(FCreationStack);
    TJclCustomThreadInfo(Dest).FName := FName;
    TJclCustomThreadInfo(Dest).FParentThreadID := FParentThreadID;
    TJclCustomThreadInfo(Dest).FStack.Assign(FStack);
    TJclCustomThreadInfo(Dest).FThreadID := FThreadID;
    TJclCustomThreadInfo(Dest).FValues := FValues;
  end
  else
    inherited AssignTo(Dest);
end;

function TJclCustomThreadInfo.GetStackClass: TJclCustomLocationInfoListClass;
begin
  Result := TJclLocationInfoList;
end;

//=== { TJclThreadInfo } =====================================================

procedure TJclThreadInfo.Fill(AThreadHandle: THandle; AThreadID: DWORD; AGatherOptions: TJclThreadInfoOptions);
begin
  InternalFill(AThreadHandle, AThreadID, AGatherOptions, False);
end;

procedure TJclThreadInfo.FillFromExceptThread(AGatherOptions: TJclThreadInfoOptions);
begin
  InternalFill(0, GetCurrentThreadID, AGatherOptions, True);
end;

function TJclThreadInfo.GetAsString: string;
var
  ExceptInfo, ThreadName, ThreadInfoStr: string;
begin
  if tioIsMainThread in Values then
    ThreadName := ' [MainThread]'
  else
  if tioName in Values then
    ThreadName := Name
  else
    ThreadName := '';
  ThreadInfoStr := '';
  if tioCreationTime in Values then
    ThreadInfoStr := ThreadInfoStr + Format(' CreationTime: %s', [DateTimeToStr(CreationTime)]);
  if tioParentThreadID in Values then
    ThreadInfoStr := ThreadInfoStr + Format(' ParentThreadID: %d', [ParentThreadID]);
  ExceptInfo := Format('ThreadID: %d%s%s', [ThreadID, ThreadName, ThreadInfoStr]) + #13#10;
  if tioStack in Values then
    ExceptInfo := ExceptInfo + Stack.AsString;
  if tioCreationStack in Values then
    ExceptInfo := ExceptInfo + 'Created at:' + #13#10 + CreationStack.AsString + #13#10;
  Result := ExceptInfo + #13#10;
end;

function TJclThreadInfo.GetStack(const AIndex: Integer): TJclLocationInfoList;
begin
  case AIndex of
    1: Result := TJclLocationInfoList(FCreationStack);
    2: Result := TJclLocationInfoList(FStack);
    else
      Result := nil;
  end;
end;

function TJclThreadInfo.GetStackClass: TJclCustomLocationInfoListClass;
begin
  Result := TJclLocationInfoList;
end;

procedure TJclThreadInfo.InternalFill(AThreadHandle: THandle; AThreadID: DWORD; AGatherOptions: TJclThreadInfoOptions; AExceptThread: Boolean);
var
  Idx: Integer;
  List: TJclStackInfoList;
begin
  if tioStack in AGatherOptions then
  begin
    if AExceptThread then
      List := JclLastExceptStackList
    else
      List := JclCreateThreadStackTrace(True, AThreadHandle);
    try
      Stack.AddStackInfoList(List);
      Values := Values + [tioStack];
    except
    { TODO -oUSc : ... }
    end;
  end;
  ThreadID := AThreadID;
  if tioIsMainThread in AGatherOptions then
  begin
    if MainThreadID = AThreadID then
      Values := Values + [tioIsMainThread];
  end;
  if AGatherOptions * [tioName, tioCreationTime, tioParentThreadID, tioCreationStack] <> [] then
    Idx := JclDebugThreadList.IndexOfThreadID(AThreadID)
  else
    Idx := -1;
  if (tioName in AGatherOptions) and (Idx <> -1) then
  begin
    Name := JclDebugThreadList.ThreadNames[AThreadID];
    Values := Values + [tioName];
  end;
  if (tioCreationTime in AGatherOptions) and (Idx <> -1) then
  begin
    CreationTime := JclDebugThreadList.ThreadCreationTime[AThreadID];
    Values := Values + [tioCreationTime];
  end;
  if (tioParentThreadID in AGatherOptions) and (Idx <> -1) then
  begin
    ParentThreadID := JclDebugThreadList.ThreadParentIDs[AThreadID];
    Values := Values + [tioParentThreadID];
  end;
  if (tioCreationStack in AGatherOptions) and (Idx <> -1) then
  begin
    try
      if JclDebugThreadList.AddStackListToLocationInfoList(AThreadID, CreationStack) then
        Values := Values + [tioCreationStack];
    except
      { TODO -oUSc : ... }
    end;
  end;
end;

//=== { TJclThreadInfoList } =================================================

constructor TJclThreadInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  FGatherOptions := [tioIsMainThread, tioName, tioCreationTime, tioParentThreadID, tioStack, tioCreationStack];
end;

destructor TJclThreadInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclThreadInfoList.Add: TJclThreadInfo;
begin
  FItems.Add(TJclThreadInfo.Create);
  Result := TJclThreadInfo(FItems.Last);
end;

procedure TJclThreadInfoList.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TJclThreadInfoList then
  begin
    TJclThreadInfoList(Dest).Clear;
    for I := 0 to Count - 1 do
      TJclThreadInfoList(Dest).Add.Assign(Items[I]);
    TJclThreadInfoList(Dest).GatherOptions := FGatherOptions;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJclThreadInfoList.Clear;
begin
  FItems.Clear;
end;

function TJclThreadInfoList.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].AsString + #13#10;
end;

procedure TJclThreadInfoList.Gather(AExceptThreadID: DWORD);
begin
  InternalGather([], [AExceptThreadID]);
end;

procedure TJclThreadInfoList.GatherExclude(AThreadIDs: array of DWORD);
begin
  InternalGather([], AThreadIDs);
end;

procedure TJclThreadInfoList.GatherInclude(AThreadIDs: array of DWORD);
begin
  InternalGather(AThreadIDs, []);
end;

function TJclThreadInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclThreadInfoList.GetItems(AIndex: Integer): TJclThreadInfo;
begin
  Result := TJclThreadInfo(FItems[AIndex]);
end;

procedure TJclThreadInfoList.InternalGather(AIncludeThreadIDs, AExcludeThreadIDs: array of DWORD);

  function OpenThread(ThreadID: DWORD): THandle;
  type
    TOpenThreadFunc = function(DesiredAccess: DWORD; InheritHandle: BOOL; ThreadID: DWORD): THandle; stdcall;
  const
    THREAD_SUSPEND_RESUME    = $0002;
    THREAD_GET_CONTEXT       = $0008;
    THREAD_QUERY_INFORMATION = $0040;
  var
    Kernel32Lib: THandle;
    OpenThreadFunc: TOpenThreadFunc;
  begin
    Result := 0;
    Kernel32Lib := GetModuleHandle(kernel32);
    if Kernel32Lib <> 0 then
    begin
      // OpenThread only exists since Windows ME
      OpenThreadFunc := GetProcAddress(Kernel32Lib, 'OpenThread');
      if Assigned(OpenThreadFunc) then
        Result := OpenThreadFunc(THREAD_SUSPEND_RESUME or THREAD_GET_CONTEXT or THREAD_QUERY_INFORMATION, False, ThreadID);
    end;
  end;

  function SearchThreadInArray(AThreadIDs: array of DWORD; AThreadID: DWORD): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if Length(AThreadIDs) > 0 then
      for I := Low(AThreadIDs) to High(AThreadIDs) do
        if AThreadIDs[I] = AThreadID then
        begin
          Result := True;
          Break;
        end;
  end;

var
  SnapProcHandle: THandle;
  ThreadEntry: TThreadEntry32;
  NextThread: Boolean;
  ThreadIDList, ThreadHandleList: TList;
  I: Integer;
  PID, TID: DWORD;
  ThreadHandle: THandle;
  ThreadInfo: TJclThreadInfo;
begin
  ThreadIDList := TList.Create;
  ThreadHandleList := TList.Create;
  try
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if SnapProcHandle <> INVALID_HANDLE_VALUE then
    try
      PID := GetCurrentProcessId;
      ThreadEntry.dwSize := SizeOf(ThreadEntry);
      NextThread := Thread32First(SnapProcHandle, ThreadEntry);
      while NextThread do
      begin
        if ThreadEntry.th32OwnerProcessID = PID then
        begin
          if SearchThreadInArray(AIncludeThreadIDs, ThreadEntry.th32ThreadID) or
            not SearchThreadInArray(AExcludeThreadIDs, ThreadEntry.th32ThreadID) then
            ThreadIDList.Add(Pointer(ThreadEntry.th32ThreadID));
        end;
        NextThread := Thread32Next(SnapProcHandle, ThreadEntry);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
    for I := 0 to ThreadIDList.Count - 1 do
    begin
      ThreadHandle := OpenThread(TJclAddr(ThreadIDList[I]));
      ThreadHandleList.Add(Pointer(ThreadHandle));
      if ThreadHandle <> 0 then
        SuspendThread(ThreadHandle);
    end;
    try
      for I := 0 to ThreadIDList.Count - 1 do
      begin
        ThreadHandle := THandle(ThreadHandleList[I]);
        TID := TJclAddr(ThreadIDList[I]);

        ThreadInfo := Add;
        ThreadInfo.Fill(ThreadHandle, TID, FGatherOptions);
      end;
    finally
      for I := 0 to ThreadHandleList.Count - 1 do
        if ThreadHandleList[I] <> nil then
        begin
          ThreadHandle := THandle(ThreadHandleList[I]);
          ResumeThread(ThreadHandle);
          CloseHandle(ThreadHandle);
        end;
    end;
  finally
    ThreadIDList.Free;
    ThreadHandleList.Free;
  end;
end;

//== Miscellanuous ===========================================================

{$IFDEF MSWINDOWS}

{$IFNDEF WINSCP}
function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;
const
  CrashCtrlScrollKey = 'SYSTEM\CurrentControlSet\Services\i8042prt\Parameters';
  CrashCtrlScrollName = 'CrashOnCtrlScroll';
var
  Enabled: Integer;
begin
  Enabled := 0;
  if Enable then
    Enabled := 1;
  RegWriteInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName, Enabled);
  Result := RegReadInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName) = Enabled;
end;
{$ENDIF ~WINSCP}

function IsDebuggerAttached: Boolean;
var
  IsDebuggerPresent: function: Boolean; stdcall;
  KernelHandle: THandle;
  P: Pointer;
begin
  KernelHandle := GetModuleHandle(kernel32);
  @IsDebuggerPresent := GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if @IsDebuggerPresent <> nil then
  begin
    // Win98+ / NT4+
    Result := IsDebuggerPresent
  end
  else
  begin
    // Win9x uses thunk pointer outside the module when under a debugger
    P := GetProcAddress(KernelHandle, 'GetProcAddress');
    Result := TJclAddr(P) < KernelHandle;
  end;
end;

function IsHandleValid(Handle: THandle): Boolean;
var
  Duplicate: THandle;
  Flags: DWORD;
begin
  if IsWinNT then
  begin
    Flags := 0;
    Result := GetHandleInformation(Handle, Flags);
  end
  else
    Result := False;
  if not Result then
  begin
    // DuplicateHandle is used as an additional check for those object types not
    // supported by GetHandleInformation (e.g. according to the documentation,
    // GetHandleInformation doesn't support window stations and desktop although
    // tests show that it does). GetHandleInformation is tried first because its
    // much faster. Additionally GetHandleInformation is only supported on NT...
    Result := DuplicateHandle(GetCurrentProcess, Handle, GetCurrentProcess,
      @Duplicate, 0, False, DUPLICATE_SAME_ACCESS);
    if Result then
      Result := CloseHandle(Duplicate);
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF HAS_EXCEPTION_STACKTRACE}
type
  PJclStackInfoRec = ^TJclStackInfoRec;
  TJclStackInfoRec = record
    Stack: TJclStackInfoList;
    Stacktrace: string;
  end;

procedure ResolveStackInfoRec(Info: PJclStackInfoRec);
var
  Str: TStringList;
begin
  if (Info <> nil) and (Info.Stack <> nil) then
  begin
    Str := TStringList.Create;
    try
      Info.Stack.AddToStrings(Str,
        estoIncludeModuleName in JclExceptionStacktraceOptions,
        estoIncludeAdressOffset in JclExceptionStacktraceOptions,
        estoIncludeStartProcLineOffset in JclExceptionStacktraceOptions,
        estoIncludeVAddress in JclExceptionStacktraceOptions
      );
      FreeAndNil(Info.Stack);
      Info.Stacktrace := Str.Text;
    finally
      FreeAndNil(Str);
    end;
  end;
end;

procedure CleanUpStackInfo(Info: Pointer);
begin
  if Info <> nil then
  begin
    PJclStackInfoRec(Info).Stack.Free;
    Dispose(PJclStackInfoRec(Info));
  end;
end;

{$STACKFRAMES ON}
// We use the StackFrame's Base-Pointer to skip all local variables from this function
function GetExceptionStackInfo(P: PExceptionRecord): Pointer;
const
  cDelphiException = $0EEDFADE;
  cSetThreadNameException = $406D1388;
var
  Stack: TJclStackInfoList;
  Info: PJclStackInfoRec;
  RawMode: Boolean;
  Delayed: Boolean;
  IgnoreLevels: Integer;
begin
  if P^.ExceptionCode = cSetThreadNameException then
  begin
    Result := nil;
    Exit;
  end;

  RawMode := stRawMode in JclStackTrackingOptions;
  Delayed := stDelayedTrace in JclStackTrackingOptions;

  IgnoreLevels := 0;
  if RawMode then
  begin
    // Skip RaiseExceptionObject, System.@RaiseExcept and the function causing the exception.
    // The causing function is added again as the first stack item through P.ExceptionAddress.
    if (P.ExceptionAddress <> nil) and (P^.ExceptionCode = cDelphiException) then
      Inc(IgnoreLevels, 3)
    else
      Inc(IgnoreLevels, 2);
  end;

  if P^.ExceptionCode = cDelphiException then
  begin
    if (P^.ExceptObject <> nil) and (Exception(P.ExceptObject).StackInfo <> nil) then
    begin
      // This method is called twice for the same exception object if the user calls
      // AcquireExceptionObject and then throws this exception again. In this case the
      // StackInfo is already allocated and by overwriting it we produce a memory leak.
      // Example: "E := AcquireExceptionObject; raise E;"
      Result := Exception(P.ExceptObject).StackInfo;
      Exit;
    end;
    if (P^.ExceptObject <> nil) and
       not (stTraceAllExceptions in JclStackTrackingOptions) and
       IsIgnoredException(TObject(P^.ExceptObject).ClassType) then
    begin
      Result := nil;
      Exit;
    end;
    Stack := TJclStackInfoList.Create(RawMode, IgnoreLevels, P^.ExceptAddr, Delayed, GetFramePointer); // Don't add it to the GlobalStackList
  end
  else
    Stack := TJclStackInfoList.Create(RawMode, IgnoreLevels, P^.ExceptionAddress, Delayed, GetFramePointer); // Don't add it to the GlobalStackList

  New(Info);
  Info.Stack := Stack;
  if stImmediateExceptionStacktraceResolving in JclStackTrackingOptions then
  begin
    try
      ResolveStackInfoRec(Info);
    except
      CleanUpStackInfo(Info);
      Info := nil;
    end;
  end;

  Result := Info;
end;
{$IFDEF STACKFRAMES_ON}
  {$STACKFRAMES ON}
{$ENDIF STACKFRAMES_ON}

function GetStackInfoString(Info: Pointer): string;
var
  Rec: PJclStackInfoRec;
begin
  Rec := Info;
  if Rec <> nil then
  begin
    if Rec.Stack <> nil then
      ResolveStackInfoRec(Rec);
    Result := Rec.Stacktrace;
  end
  else
    Result := '';
end;

procedure SetupExceptionProcs;
begin
  if not Assigned(Exception.GetExceptionStackInfoProc) then
  begin
    Exception.GetExceptionStackInfoProc := GetExceptionStackInfo;
    Exception.GetStackInfoStringProc := GetStackInfoString;
    Exception.CleanUpStackInfoProc := CleanUpStackInfo;
  end;
end;

procedure ResetExceptionProcs;
begin
  if @Exception.GetExceptionStackInfoProc = @GetExceptionStackInfo then
  begin
    Exception.GetExceptionStackInfoProc := nil;
    Exception.GetStackInfoStringProc := nil;
    Exception.CleanUpStackInfoProc := nil;
  end;
end;
{$ENDIF HAS_EXCEPTION_STACKTRACE}

procedure InitHexMap;
var
  Ch: AnsiChar;
begin
  FillChar(HexMap, SizeOf(HexMap), $80);
  for Ch := '0' to '9' do
    HexMap[Ch] := Ord(Ch) - Ord('0');
  for Ch := 'a' to 'f' do
    HexMap[Ch] := Ord(Ch) - (Ord('a') - 10);
  for Ch := 'A' to 'F' do
    HexMap[Ch] := Ord(Ch) - (Ord('A') - 10);
end;

procedure FreeJclDebugGlobals;
begin
  {$IFDEF HAS_EXCEPTION_STACKTRACE}
  ResetExceptionProcs;
  {$ENDIF HAS_EXCEPTION_STACKTRACE}

  FreeAndNil(RegisteredThreadList);
  FreeAndNil(DebugInfoList);
  FreeAndNil(GlobalStackList);
  FreeAndNil(GlobalModulesList);
  FreeAndNil(DebugInfoCritSect);
  FreeAndNil(InfoSourceClassList);
  FreeAndNil(IgnoredExceptions);
  FreeAndNil(IgnoredExceptionClassNames);
  FreeAndNil(IgnoredExceptionClassNamesCritSect);

  TJclDebugInfoSymbols.CleanupDebugSymbols;
end;

initialization
  InitHexMap;
  DebugInfoCritSect := TJclCriticalSection.Create;
  GlobalModulesList := TJclGlobalModulesList.Create;
  GlobalStackList := TJclGlobalStackList.Create;
  AddIgnoredException(EAbort);
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_EXCEPTION_STACKTRACE}
  SetupExceptionProcs;
  {$ENDIF HAS_EXCEPTION_STACKTRACE}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

  { TODO -oPV -cInvestigate : Calling JclStopExceptionTracking causes linking of various classes to
    the code without a real need. Although there doesn't seem to be a way to unhook exceptions
    safely because we need to be covered by JclHookExcept.Notifiers critical section }
  JclStopExceptionTracking;

  GlobalStackList.Clear;
  JclDebugFinalized := True;
  if GlobalStackListLiveCount = 0 then
    FreeJclDebugGlobals;

end.
