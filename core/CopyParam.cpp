//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "CopyParam.h"
//---------------------------------------------------------------------------
__fastcall TCopyParamType::TCopyParamType()
{
  Default();
}
//---------------------------------------------------------------------------
__fastcall TCopyParamType::TCopyParamType(const TCopyParamType & Source)
{
  Assign(Source);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Default()
{
  FileNameCase = ncNoChange;
  PreserveReadOnly = true;
  PreserveTime = true;
  Rights.Number = 0644;
  PreserveRights = false; // Was True until #106
  AsciiFileMask.Masks = "*.*htm*; *.txt; *.php*; *.cgi; *.c; *.cpp; *.h; *.pas; "
    "*.bas; *.tex; *.pl; .htaccess; *.xtml; *.css; *.cfg; *.ini; *.sh; *.xml";
  TransferMode = tmAutomatic;
  AddXToDirectories = true;
  ResumeSupport = rsSmart;
  ResumeThreshold = 10 * 1024; // !!! DEBUG only
  //ResumeThreshold = 100 * 1024; // (100 kB)
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Assign(const TCopyParamType & Source)
{
  #define COPY(Prop) Prop = Source.Prop
  COPY(FileNameCase);
  COPY(PreserveReadOnly);
  COPY(PreserveTime);
  COPY(Rights);
  COPY(AsciiFileMask);
  COPY(TransferMode);
  COPY(AddXToDirectories);
  COPY(PreserveRights);
  COPY(ResumeSupport);
  COPY(ResumeThreshold);
  #undef COPY
}
//---------------------------------------------------------------------------
TCopyParamType & __fastcall TCopyParamType::operator =(const TCopyParamType & rhp)
{
  Assign(rhp);
  return *this;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::ChangeFileNameCase(AnsiString FileName) const
{
  switch (FileNameCase) {
    case ncUpperCase: return FileName.UpperCase();
    case ncLowerCase: return FileName.LowerCase();
    case ncFirstUpperCase: return FileName.SubString(1, 1).UpperCase() +
      FileName.SubString(2, FileName.Length()-1).LowerCase();
    case ncNoChange:
    default:
      return FileName;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::UseAsciiTransfer(const AnsiString FileName) const
{
  switch (TransferMode) {
    case tmBinary: return false;
    case tmAscii: return true;
    case tmAutomatic: return AsciiFileMask.Matches(FileName);
    default: assert(false); return false;
  }
}
//---------------------------------------------------------------------------
TRights __fastcall TCopyParamType::RemoteFileRights(Integer Attrs) const
{
  TRights R = Rights;
/*  if ((Attrs & faReadOnly) && PreserveReadOnly)
    R.ReadOnly = True;*/
  if ((Attrs & faDirectory) && AddXToDirectories)
    R.AddExecute();
  return R;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::GetLogStr() const
{
  char CaseC[] = "NULF";
  char ModeC[] = "BAM";
  char ResumeC[] = "YSN";
  return FORMAT(
    "  PrTime: %s; PrRO: %s; Rght: %s; PrR: %s; FnCs: %s; Resume: %s (%d)\n"
    "  TM: %s; AscM: %s ",
    (BooleanToEngStr(PreserveTime),
     BooleanToEngStr(PreserveReadOnly),
     Rights.Text,
     BooleanToEngStr(PreserveRights),
     CaseC[FileNameCase],
     ResumeC[ResumeSupport],
     (int)ResumeThreshold,
     ModeC[TransferMode],
     AsciiFileMask.Masks));
}
//---------------------------------------------------------------------------
int __fastcall TCopyParamType::LocalFileAttrs(const TRights & Rights) const
{
  int Result = 0;
  if (PreserveReadOnly && !Rights.Right[rfUserWrite])
  {
    Result |= faReadOnly;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowResume(__int64 Size) const
{
  switch (ResumeSupport) {
    case rsOn: return true;
    case rsOff: return false;
    case rsSmart: return (Size >= ResumeThreshold);
    default: assert(false); return false;
  }
}
//---------------------------------------------------------------------------

