//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileMasks.h"

#include <Masks.hpp>

#include "Common.h"
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

