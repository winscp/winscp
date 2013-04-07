unit LanguagesDEPfix;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

{ Patch for Data Execution Prevention (DEP) problems with SysUtils.TLanguages. }
{ In the Delphi RTL the TLanguages constructor dynamically builds and executes }
{ machine code in the stack segment without setting the PAGE_EXECUTE_READWRITE }
{ or PAGE_EXECUTE_WRITECOPY bits. It appears that that code doesn't run on     }
{ systems that have DEP enabled - e.g. WinXP SP2, Win2003 SP1. Note that       }
{ Win2003 SP1 has DEP activated by default.                                    }
{ This problem is fixed in the Delphi 2005 RTL.                                }
{                                                                              }
{ Usage:                                                                       }
{ Put this unit as the first unit in the uses clause of the source of the      }
{ project. At least make sure this unit is listed *before* the first SysUtils. }
{ Tested with D5, D6 and D7.                                                   }
{                                                                              }
{ Author: Sasan Adami - s.adami@gmx.net                                        }
{ Date  : 20 july 2005                                                         }
{ Disclaimer: use at your own risk!                                            }

interface

uses
  Windows, Classes, SysUtils;

function LanguagesDEPF: TLanguages;

implementation

uses
  SysConst;

type
  PJumpRec = ^TJumpRec;
  TJumpRec = packed record
    OpCode: Byte;
    Address: DWord;
  end;

  PLongJumpRec = ^TLongJumpRec;
  TLongJumpRec = packed record
    OpCode: Word;
    Address: DWord;
  end;

  // hack for access to private members of TLanguage
  THackLanguages = class
  private
    FSysLangs: array of TLangRec;
  end;

  // replacement for TLanguages.Create
  TLanguagesDEPfix = class(TLanguages)
  private
    function LocalesCallback(LocaleID: PChar): integer; stdcall;
  public
    constructor Create;
  end;

var
  FTempLanguages: TLanguagesDEPfix;

function EnumLocalesCallback(LocaleID: PChar): integer; stdcall;
begin
  Result := FTempLanguages.LocalesCallback(LocaleID);
end;

function GetLocaleDataW(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of WideChar;
begin
  Buffer[0] := #0;
  GetLocaleInfoW(ID, Flag, Buffer, SizeOf(Buffer) div 2);
  Result := Buffer;
end;

function GetLocaleDataA(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of Char;
begin
  Buffer[0] := #0;
  SetString(Result, Buffer, GetLocaleInfo(ID, Flag, Buffer, Length(Buffer)) - 1);
end;

{ TLanguagesDEPfix }

function TLanguagesDEPfix.LocalesCallback(LocaleID: PChar): Integer; stdcall;
type
  PSysLangs = ^TSysLangs;
  TSysLangs = array of TLangRec;
var
  AID: LCID;
  ShortLangName: string;
  GetLocaleDataProc: function (ID: LCID; Flag: DWORD): string;
  PSLangs: PSysLangs;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    GetLocaleDataProc := @GetLocaleDataW
  else
    GetLocaleDataProc := @GetLocaleDataA;
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  ShortLangName := GetLocaleDataProc(AID, LOCALE_SABBREVLANGNAME);
  if ShortLangName <> '' then
  begin
    PSLangs := @THackLanguages(Self).FSysLangs;
    SetLength(PSLangs^, Length(PSLangs^) + 1);
    with PSLangs^[High(PSLangs^)] do
    begin
      FName := GetLocaleDataProc(AID, LOCALE_SLANGUAGE);
      FLCID := AID;
      FExt := ShortLangName;
    end;
  end;
  Result := 1;
end;

constructor TLanguagesDEPfix.Create;
begin
  FTempLanguages := Self;
  EnumSystemLocales(@EnumLocalesCallback, LCID_SUPPORTED);
end;

var
  FLanguages: TLanguagesDEPfix;

function LanguagesDEPF: TLanguages;
begin
  if FLanguages = nil then
    FLanguages := TLanguagesDEPfix.Create;
  Result := FLanguages;
end;

initialization
  FLanguages := nil;
finalization
  FLanguages.Free;
end.
