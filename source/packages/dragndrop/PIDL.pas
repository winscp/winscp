unit PIDL;
{
  Description
  ===========
    Some methods to pidls. Purpose of methods are described in the code.


  Disclaimer
  ==========
    The author disclaims all warranties, expressed or implied, including,
    without limitation, the warranties of merchantability and of fitness
    for any purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this unit.


  Restrictions on Using the Unit
  ==============================
    This unit is copyright 1998 by Dieter Steinwedel. ALL RIGHTS
    ARE RESERVED BY DIETER STEINWEDEL. You are allowed to use it freely
    subject to the following restrictions:

    • You are not allowed delete or alter the author's name and
      copyright in any manner

    • You are not allowed to publish a copy, modified version or
      compilation neither for payment in any kind nor freely

    • You are allowed to create a link to the download in the WWW

    • These restrictions and terms apply to you as long as until
      I alter them. Changes can found on my homepage
}
interface

uses
  ShlObj, Windows, ActiveX;

function PIDL_GetSize(PIDL: PITEMIDLIST): Integer;
function PIDL_Create(Size: UINT): PItemIDList;
function PIDL_Concatenate(PIDL1, PIDL2: PItemIDList): PItemIDList;
function PIDL_Copy(PIDLSource: PItemIDList): PItemIDList;
function PIDL_GetDisplayName(piFolder: IShellFolder; PIDL: PItemIDList;
  dwFlags: DWORD; pszName: PChar; cchMax: UINT): Boolean;
procedure PIDL_GetRelative(var PIDLFQ, PPIDLRoot, PPIDLItem: PItemIDList);
function PIDL_GetFromPath(pszFile: PChar): PItemIDList;
function PIDL_GetFileFolder(PIDL: PItemIDList; var piFolder: IShellFolder): Boolean;
function PIDL_GetFromParentFolder(pParentFolder: IShellFolder; pszFile: PChar): PItemIDList;
procedure PIDL_Free(PIDL: PItemIDList);
function PIDL_Equal(PIDL1, PIDL2: PItemIDList): Boolean;

var
  ShellMalloc: IMalloc;

  CF_FILENAMEMAP: UINT;
  CF_FILENAMEMAPW: UINT;
  CF_SHELLIDLIST: UINT;
  CF_PREFERREDDROPEFFECT: UINT;

implementation

uses
  SysUtils, CompThread, OperationWithTimeout;

const NullTerm=2;

function PIDL_GetNextItem(PIDL: PItemIDList): PItemIDList;
//  PURPOSE:    Returns a pointer to the next item in the ITEMIDLIST.
//  PARAMETERS:
//      pidl - Pointer to an ITEMIDLIST to walk through
begin
  if PIDL<>nil then Result := PItemIDList(PAnsiChar(PIDL) + PIDL^.mkid.cb)
     else Result := nil;
end;

function PIDL_GetSize(PIDL: PITEMIDLIST): Integer;
//  PURPOSE:    Returns the total number of bytes in an ITEMIDLIST.
//  PARAMETERS:
//      pidl - Pointer to the ITEMIDLIST that you want the size of.
begin
  Result := 0;
  if PIDL <> nil then
  begin
    Inc(Result, SizeOf(PIDL^.mkid.cb));
    while PIDL^.mkid.cb <> 0 do
    begin
      Inc(Result, PIDL^.mkid.cb);
      Inc(LongInt(PIDL), PIDL^.mkid.cb);
    end;
  end;
end;

function PIDL_Create(Size: UINT): PItemIDList;
//  PURPOSE:    Creates a new ITEMIDLIST of the specified size.
//  PARAMETERS:
//      piMalloc - Pointer to the allocator interface that should allocate memory.
//  cbSize   - Size of the ITEMIDLIST to create.
//  RETURN VALUE:
//      Returns a pointer to the new ITEMIDLIST, or NULL if a problem occured.
begin
  Result := ShellMalloc.Alloc(Size);
  if Result <> nil then
    FillChar(Result^, Size, #0);
end;

function PIDL_Concatenate(PIDL1, PIDL2: PItemIDList): PItemIDList;
//  PURPOSE:    Creates a new ITEMIDLIST with pidl2 appended to pidl1.
//  PARAMETERS:
//  piMalloc - Pointer to the allocator interface that should create the new ITEMIDLIST.
//      pidl1- Pointer to an ITEMIDLIST that contains the root.
//  pidl2    - Pointer to an ITEMIDLIST that contains what should be appended to the root.
//  RETURN VALUE:
//      Returns a new ITEMIDLIST if successful, NULL otherwise.
var
  cb1, cb2: UINT;
begin
  if (PIDL1 <> nil) then cb1 := PIDL_GetSize(PIDL1) - NullTerm else cb1 := 0;
  cb2 := PIDL_GetSize(PIDL2);
  Result := PIDL_Create(cb1 + cb2);
  if Result <> nil then
  begin
    if PIDL1 <> nil then CopyMemory(Result, PIDL1, cb1);
    CopyMemory(PAnsiChar(Result) + cb1, PIDL2, cb2);
  end;
end;

function PIDL_Copy(PIDLSource: PItemIDList): PItemIDList;
//  PURPOSE:    Creates a new copy of an ITEMIDLIST.
//  PARAMETERS:
//      piMalloc - Pointer to the allocator interfaced to be used to allocate the new ITEMIDLIST.
//  RETURN VALUE:
//      Returns a pointer to the new ITEMIDLIST, or NULL if an error occurs.
var
  cbSource: UINT;
begin
  Result := nil;
  if pidlSource = nil then Exit;
  cbSource := PIDL_GetSize(PIDLSource);
  Result := PIDL_Create(cbSource);
  if Result = nil then Exit;
  CopyMemory(Result, PIDLSource, cbSource);
end;

function PIDL_GetDisplayName(piFolder: IShellFolder; PIDL: PItemIDList;
   dwFlags: DWORD; pszName: PChar; cchMax: UINT): Boolean;
//  PURPOSE:    Returns the display name for the item pointed to by pidl.  The
//              function assumes the pidl is relative to piFolder.  If piFolder
//              is NULL, the function assumes the item is fully qualified.
//  PARAMETERS:
//  piFolder - Pointer to the IShellFolder for the folder containing the item.
//  pidl     - Pointer to an ITEMIDLIST relative to piFolder that we want
//             the display name for.
//  dwFlags  - Flags to pass to ISF::GetDisplayNameOf().
//  pszName  - Pointer to the string where the display name is returned.
//  cchMax   - Maximum number of characters in pszName.
//  RETURN VALUE:
//      Returns TRUE if successful, FALSE otherwise.
var
  Str: TStrRet;
begin
  if (piFolder = nil) and (Failed(SHGetDesktopFolder(piFolder))) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;

  if piFolder.GetDisplayNameOf(PIDL, dwFlags, Str) = NOERROR then
  begin
    case Str.uType of
      STRRET_WSTR:
        lstrcpyn(pszName, str.pOleStr, cchMax);
      STRRET_OFFSET:
        MultiByteToWideChar(CP_ACP, 0, PAnsiChar(PIDL) + str.uOffset, -1, pszName, cchMax);
      STRRET_CSTR:
        MultiByteToWideChar(CP_ACP, 0, str.cStr, -1, pszName, cchMax);
      else
        Result := False;
    end;
  end
    else Result := False;
  // piFolder._Release; -> automaticly done by D4
end;

procedure PIDL_GetRelative(var pidlFQ, PPIDLRoot, PPIDLItem: PItemIDList);
//  PURPOSE:    Takes a fully qualified pidl and returns the the relative pidl
//  and the root part of that pidl.
//  PARAMETERS:
//  pidlFQ   - Pointer to the fully qualified ITEMIDLIST that needs to be parsed.
//  pidlRoot - Points to the pidl that will contain the root after parsing.
//  pidlItem - Points to the item relative to pidlRoot after parsing.
var
  PIDLTemp, PIDLNext: PItemIDList;
begin
  if PIDLFQ = nil then
  begin
    PPIDLRoot := nil;
    PPIDLItem := nil;
    Exit;
  end;

  PPIDLItem := nil;
  PPIDLRoot := PIDL_Copy(PIDLFQ);
  PIDLTemp := PPIDLRoot;

  while PIDLTemp^.mkid.cb>0 do
  begin
    PIDLNext := PIDL_GetNextItem(PIDLTemp);
    if PIDLNext^.mkid.cb = 0 then
    begin
      PPIDLItem := PIDL_Copy(PIDLTemp);
      PIDLTemp^.mkid.cb := 0;
      PIDLTemp^.mkid.abID[0] := 0;
    end;

    PIDLTemp := PIDLNext;
  end;
end;

function PIDL_GetFromPath(pszFile: PChar): PItemIDList;
//  PURPOSE:    This routine takes a full path to a file and converts that
//  to a fully qualified ITEMIDLIST.
//  PARAMETERS:
//      pszFile  - Full path to the file.
//  RETURN VALUE:
//      Returns a fully qualified ITEMIDLIST, or NULL if an error occurs.
var
  piDesktop: IShellFolder;
  ulEaten, ulAttribs: ULong;
begin
  Result := nil;
  if Failed(SHGetDesktopFolder(piDesktop)) then Exit;
  piDesktop._AddRef;
  ulAttribs := 0;
  if Failed(piDesktop.ParseDisplayName(0, nil, pszFile, ulEaten, Result, ulAttribs)) then Result := nil;
  // piDesktop._Release; -> automaticly done by D4
end;

function PIDL_GetFileFolder(PIDL: PItemIDList; var piFolder: IShellFolder): Boolean;
//  PURPOSE:    This routine takes a fully qualified pidl for a folder and returns
//  the IShellFolder pointer for that pidl
//  PARAMETERS:
//  pidl     - Pointer to a fully qualified ITEMIDLIST for the folder
//      piParentFolder - Pointer to the IShellFolder of the folder (Return value).
//  RETURN VALUE:
//      Returns TRUE if successful, FALSE otherwise.
var
  piDesktopFolder: IShellFolder;
begin
  Result:=false;
  if Failed(SHGetDesktopFolder(piDesktopFolder)) then Exit;
  if (not Assigned(PiFolder)) and Failed(SHGetDesktopFolder(PiFolder)) then Exit;
  if not Failed(piDesktopFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(PiFolder))) then Result := True;
  //piDesktopFolder._Release; -> automaticly done by D4
end;

function PIDL_GetFromParentFolder(pParentFolder: IShellFolder; pszFile: PChar): PItemIDList;
//  PURPOSE:    This routine takes a Shell folder for the parent and the FileName in the folder
//  and converts that to a relative ITEMIDLIST.
//  PARAMETERS:
//      pParentFolder - Pointer to the IShellFolder for the folder containing the
//                  fileName.
//      pszFile       - file name in the folder.
//  RETURN VALUE:
//      Returns a relative ITEMIDLIST, or NULL if an error occurs.
var
  Eaten: ULONG;
  ShAttr: ULONG;
begin
  ShellFolderParseDisplayNameWithTimeout(pParentFolder, 0, nil, pszFile, Eaten, Result, ShAttr, 2 * MSecsPerSec);
end;

procedure PIDL_Free(PIDL: PItemIDList);
begin
  if PIDL <> nil then
    ShellMalloc.Free(PIDL);
end;

function PIDL_Equal(PIDL1,PIDL2: PItemIDList): Boolean;
var
  I, Size: Integer;
  P1, P2: PChar;
begin
  Result := False;
  if (PIDL1 = nil) or (PIDL2 = nil) then Exit;
  Size := PIDL_GetSize(PIDL1);
  if Size <> PIDL_GetSize(PIDL2) then Exit;
  I := 0;
  P1 := PChar(PIDL1);
  P2 := PChar(PIDL2);
  while I < Size do
  begin
    if P1[I] <> P2[I] then Exit
      else Inc(I);
  end;
  Result := True;
end;

initialization

  SHGetMalloc(ShellMalloc);

  CF_FILENAMEMAP := RegisterClipboardFormat('FileNameMap');
  CF_FILENAMEMAPW := RegisterClipboardFormat('FileNameMapW');
  CF_SHELLIDLIST := RegisterClipboardFormat('Shell IDList Array');
  CF_PREFERREDDROPEFFECT := RegisterClipboardFormat('Preferred DropEffect');

finalization
  // ShellMalloc._Release; -> automaticly done by D4

end.
