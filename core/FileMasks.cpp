//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileMasks.h"

#include <Masks.hpp>

#include "Common.h"
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFilePart(const AnsiString Part, const AnsiString Mask)
{
  AnsiString Result;
  int RestStart = 1;
  for (int Index = 1; Index <= Mask.Length(); Index++)
  {
    switch (Mask[Index])
    {
      case '*':
        Result += Part.SubString(RestStart, Part.Length() - RestStart + 1);
        RestStart = Part.Length() + 1; 
        break;

      case '?':
        if (RestStart <= Part.Length())
        {
          Result += Part[RestStart];
          RestStart++;
        }
        break;

      default:
        Result += Mask[Index];
        RestStart++;
        break; 
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFileName(AnsiString FileName, const AnsiString Mask)
{
  if (!Mask.IsEmpty() && (Mask != "*") && (Mask != "*.*"))
  {
    int P = Mask.LastDelimiter(".");
    if (P > 0)
    {
      int P2 = FileName.LastDelimiter(".");
      // only dot at beginning of file name is not considered as
      // name/ext separator
      AnsiString FileExt = P2 > 1 ?
        FileName.SubString(P2 + 1, FileName.Length() - P2) : AnsiString();
      FileExt = MaskFilePart(FileExt, Mask.SubString(P + 1, Mask.Length() - P));
      if (P2 > 1)
      {
        FileName.SetLength(P2 - 1);
      }
      FileName = MaskFilePart(FileName, Mask.SubString(1, P - 1));
      if (!FileExt.IsEmpty())
      {
        FileName += "." + FileExt;
      }
    }
    else
    {
      FileName = MaskFilePart(FileName, Mask);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks()
{
  FMasks = "";
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const TFileMasks & Source)
{
  Masks = Source.Masks;
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const AnsiString AMasks)
{
  FMasks = AMasks;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(AnsiString FileName) const
{
  AnsiString S = Masks;
  FileName = ExtractFileName(FileName);
  while (!S.IsEmpty())
  {
    AnsiString M;
    M = CutToChar(S, ';', True);
    if (MatchesMask(FileName, M)) return true;
  }
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::IsValid()
{
  int Start, Length;
  return IsValid(Start, Length);
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::IsValid(int & Start, int & Length)
{
  AnsiString S = Masks;
  int IStart = 1;

  while (!S.IsEmpty())
  {
    AnsiString M;
    int P = S.Pos(';');
    M = CutToChar(S, ';', False);

    if (!M.IsEmpty())
    {
      try
      {
        TMask * Mask = new TMask(Trim(M));
        try
        {
          Mask->Matches("*.*");
        }
        __finally
        {
          delete Mask;
        }
      }
      catch (Exception &E)
      {
        // Ignore leading/trainling spaces
        while (!M.IsEmpty() && (M[1] == ' '))
        {
          IStart++;
          M.Delete(1, 1);
        }
        Start = IStart-1;
        Length = M.Trim().Length();
        return False;
      }
    }
    if (P) IStart += P;
  }
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const TFileMasks & rhm) const
{
  return (Masks == rhm.Masks);
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const AnsiString rhs)
{
  Masks = rhs;
  return *this;
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const TFileMasks & rhm)
{
  Masks = rhm.Masks;
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const AnsiString rhs) const
{
  return (Masks == rhs);
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const char * rhs)
{
  Masks = rhs;
  return *this;
}

