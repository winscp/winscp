unit ResourceModule;

interface

function GetResourceModule(ModuleName: PChar): string;

implementation

const
  advapi32 = 'advapi32.dll';
  kernel = 'kernel32.dll';
  user = 'user32.dll';
  HKEY_CURRENT_USER = $80000001;
  HKEY_LOCAL_MACHINE = $80000002;
  OldLocaleOverrideKey = 'Software\Borland\Delphi\Locales'; // do not localize
  NewLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize
  KEY_READ = $000F0019;
  LOCALE_SABBREVLANGNAME = $00000003;   { abbreviated language name }
  LOAD_LIBRARY_AS_DATAFILE = 2;

type
  TWin32FindData = packed record
    dwFileAttributes: Integer;
    ftCreationTime: Int64;
    ftLastAccessTime: Int64;
    ftLastWriteTime: Int64;
    nFileSizeHigh: Integer;
    nFileSizeLow: Integer;
    dwReserved0: Integer;
    dwReserved1: Integer;
    cFileName: array[0..259] of Char;
    cAlternateFileName: array[0..13] of Char;
  end;

function GetModuleHandle(ModuleName: PChar): Integer; stdcall;
  external kernel name 'GetModuleHandleA';

function CharNext(lpsz: PChar): PChar; stdcall;
  external user name 'CharNextA';

function GetProcAddress(Module: Integer; ProcName: PChar): Pointer; stdcall;
  external kernel name 'GetProcAddress';

function FindClose(FindFile: Integer): LongBool; stdcall;
  external kernel name 'FindClose';

function _strlen(lpString: PChar): Integer; stdcall;
  external kernel name 'lstrlenA';

function lstrcpyn(lpString1, lpString2: PChar;
  iMaxLength: Integer): PChar; stdcall;
  external kernel name 'lstrcpynA';

function FindFirstFile(FileName: PChar; var FindFileData: TWIN32FindData): Integer; stdcall;
  external kernel name 'FindFirstFileA';

function GetModuleFileName(Module: Integer; Filename: PChar;
  Size: Integer): Integer; stdcall;
  external kernel name 'GetModuleFileNameA';

function RegOpenKeyEx(hKey: LongWord; lpSubKey: PChar; ulOptions,
  samDesired: LongWord; var phkResult: LongWord): Longint; stdcall;
  external advapi32 name 'RegOpenKeyExA';

function RegQueryValueEx(hKey: LongWord; lpValueName: PChar;
  lpReserved: Pointer; lpType: Pointer; lpData: PChar; lpcbData: Pointer): Integer; stdcall;
  external advapi32 name 'RegQueryValueExA';

function RegCloseKey(hKey: Integer): Longint; stdcall;
  external advapi32 name 'RegCloseKey';

function GetLocaleInfo(Locale: Longint; LCType: Longint; lpLCData: PChar; cchData: Integer): Integer; stdcall;
  external kernel name 'GetLocaleInfoA';

function GetThreadLocale: Longint; stdcall;
  external kernel name 'GetThreadLocale';

function LoadLibraryEx(LibName: PChar; hFile: Longint; Flags: Longint): Longint; stdcall;
  external kernel name 'LoadLibraryExA';

function GetResourceModule(ModuleName: PChar): string;
var
  FileName: array[0..260] of Char;
  Key: LongWord;
  LocaleName, LocaleOverride: array[0..4] of Char;
  Size: Integer;
  P: PChar;
  R: Integer;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar; BufSize: Integer): PChar;
  var
    CurrBS, NextBS: PChar;
    Handle, L: Integer;
    FindData: TWin32FindData;
    Buffer: array[0..260] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  begin
    Result := AFileName;
    Handle := GetModuleHandle(kernel);
    if Handle <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Handle, 'GetLongPathNameA');
      if Assigned(GetLongPathName) and
        (GetLongPathName(AFileName, Buffer, SizeOf(Buffer)) <> 0) then
      begin
        lstrcpyn(AFileName, Buffer, BufSize);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > SizeOf(Buffer) then Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if (Handle = -1) then Exit;
      FindClose(Handle);

      if L + 1 + _strlen(FindData.cFileName) + 1 > SizeOf(Buffer) then Exit;
      Buffer[L] := '\';
      lstrcpyn(Buffer + L + 1, FindData.cFileName, Sizeof(Buffer) - L - 1);
      Inc(L, _strlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpyn(AFileName, Buffer, BufSize);
  end;

begin
  GetModuleFileName(0, FileName, SizeOf(FileName)); // Get host application name
  LocaleOverride[0] := #0;
  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_READ, Key) = 0) then
  try
    Size := sizeof(LocaleOverride);
    ToLongPath(FileName, sizeof(FileName));
    if RegQueryValueEx(Key, FileName, nil, nil, LocaleOverride, @Size) <> 0 then
      if RegQueryValueEx(Key, '', nil, nil, LocaleOverride, @Size) <> 0 then
        LocaleOverride[0] := #0;
    LocaleOverride[sizeof(LocaleOverride)-1] := #0;
  finally
    RegCloseKey(Key);
  end;
  lstrcpyn(FileName, ModuleName, sizeof(FileName));
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, sizeof(LocaleName));
  R := 0;
  if (FileName[0] <> #0) and ((LocaleName[0] <> #0) or (LocaleOverride[0] <> #0)) then
  begin
    P := PChar(@FileName) + _strlen(FileName);
    while (P^ <> '.') and (P <> @FileName) do Dec(P);
    if P <> @FileName then
    begin
      Inc(P);
      // First look for a locale registry override
      if LocaleOverride[0] <> #0 then
      begin
        lstrcpyn(P, LocaleOverride, sizeof(FileName) - (P - FileName));
        R := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
      end;
      if (R = 0) and (LocaleName[0] <> #0) then
      begin
        // Then look for a potential language/country translation
        lstrcpyn(P, LocaleName, sizeof(FileName) - (P - FileName));
        R := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
        if R = 0 then
        begin
          // Finally look for a language only translation
          LocaleName[2] := #0;
          lstrcpyn(P, LocaleName, sizeof(FileName) - (P - FileName));
          R := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
        end;
      end
    end;
  end;

  if R = 0 then Result := ''
    else Result := FileName;
end;

end.
