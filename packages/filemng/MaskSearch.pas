{
  Basic pattern matching.  Supports '*' and '?' only.

  This code is based on a unit by Markus Stephany (mirbir.st@t-online.de) that
  I found on DSP (http://sunsite.icm.edu.pl/delphi/).  Please DO NOT email
  Markus about any problems you may find in it.  All problems with it should be
  reported to me, since I'm the last one who mucked with it.

  The original did a bunch of stuff that I didn't need (like searching inside
  of files), didn't do some stuff that I did need (like handling spaces in the
  filename), and had a few bugs, too.  In order to help me find the bugs more
  quickly, I stripped out the stuff I didn't need, renamed things to be a little
  more clear, and generally tried to clean up the mess that comes from stripping
  out stuff you don't need.  :)
}
unit MaskSearch;

interface

uses
  Classes, SysUtils;


procedure BuildMask(Str: string; MaskList: TStringList);

function FileMatches(AFile: string; MaskList: TStringList): boolean;


implementation


// fills the grep_list with the parts of 's' (divided by ';')
procedure BuildMask(Str: string; MaskList: TStringList);
var
  ct: integer;
begin
  MaskList.clear;
  MaskList.sorted := false;
  if Str = '' then
  begin
    MaskList.add('*');
  end else begin
    if Str[length(Str)] <> ';' then
      Str := Str + ';';

    // divide the string
    ct := Pos(';', Str);
    while ct > 0 do
    begin
      MaskList.Add(AnsiLowerCase(Copy(Str, 1, ct-1)));
      Delete(Str, 1, ct);
      ct := Pos(';', Str);
    end;

    MaskList.sorted := TRUE;
    MaskList.duplicates := dupIgnore;
  end;
end;

// tests whether the string 'Str' fits to the search mask in 'Mask'
function SimpleGrep(Str, Mask: string):boolean;
var
  sr, s2: string;
  ps1,ps2,ps3: integer;
  DontCare: boolean;
  OneChar: char;
  TmpList: TStringList;
begin
  if (Mask = '*') or // fits always
     ((Mask = '*.*') and (Pos('.', Str) > 0)) then // always fits, too
    Result := TRUE
  else begin
    if (Pos('*', Mask) = 0) and (Pos('?', Mask) = 0) and (Mask = Str) then
      // searched text was found (searchstring IN text)
      Result := TRUE
    else begin
      Result := FALSE;
      if Mask = '' then
        exit;

      TmpList := TStringList.Create;
      try
        // divide partial strings ('?','*' or text) to TmpList
        repeat
          OneChar := Mask[1];
          if (OneChar in ['*', '?']) then
          begin
            TmpList.Add(OneChar);
            Delete(Mask, 1, 1);
          end else begin
            ps1 := Pos('?', Mask);
            if ps1 = 0 then
              ps1 := MaxInt;
            ps2 := Pos('*', Mask);
            if ps2 = 0 then
              ps2 := MaxInt;
            if ps2 > ps1 then
              ps2 := ps1;

            TmpList.Add(Copy(Mask, 1, ps2-1));
            Delete(Mask, 1, ps2-1);
          end;
        until Mask = '';

        // now compare the string with the partial search masks
        DontCare := FALSE;
        ps2 := 1;
        if TmpList.Count > 0 then
        begin
          for ps1 := 0 to pred(TmpList.Count) do
          begin
            sr := TmpList[ps1];
            if sr = '?' then
            begin
              inc(ps2);
              if ps2 > length(Str)+1 then
                exit;
            end else begin
              if sr = '*' then
                DontCare := TRUE
              else begin
                if DontCare then
                begin
                  if ps1 = pred(TmpList.Count) then
                  begin
                    s2 := Copy(Str, ps2, maxint);
                    ps2 := length(Str); // just something to make the thing fail
                    if Length(s2) >= Length(SR) then
                      if sr = Copy(s2, Length(s2)-Length(SR)+1, MaxInt) then
                        ps2 := length(Str) + 1;
                  end else begin
                    ps3:= Pos(sr, Copy(Str, ps2, maxint));
                    if ps3 = 0 then
                      exit;
                    ps2 := ps2 + ps3 + length(sr) - 1;
                  end;
                  DontCare := FALSE;
                end else begin
                  if Copy(Str, ps2, length(sr)) <> sr then
                    exit;
                  ps2 := ps2 + length(sr);
                end;
              end;
            end;
          end;
        end;

        if (not DontCare) and (ps2 <> length(Str)+1) then
          Result := FALSE
        else
          Result := TRUE;
      finally
        TmpList.free;
      end;
    end;
  end;
end;

// tests whether the filename fits the search masks in MaskList
function FileMatches(AFile: string; MaskList: TStringList): boolean;
var
  ct: integer;
begin
  AFile := AnsiLowerCase(AFile);
  if (MaskList = NIL) or (MaskList.Count = 0) then
    Result := TRUE // if no search AFileing, the always return TRUE
  else begin
    if Pos('.', AFile) = 0 then
      AFile := AFile + '.'; // '.' is implied for filenames

    Result := FALSE;
    // compare with the whole MaskList until one fits
    for ct := 0 to Pred(MaskList.Count) do
    begin
      if SimpleGrep(AFile, MaskList[ct]) then
      begin
        Result := TRUE;
        break;
      end;
    end;
  end;
end;


end.
