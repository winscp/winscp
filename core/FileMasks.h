//---------------------------------------------------------------------------
#ifndef FileMasksH
#define FileMasksH
//---------------------------------------------------------------------------
class TFileMasks
{
public:
  __fastcall TFileMasks();
  __fastcall TFileMasks(const TFileMasks & Source);
  __fastcall TFileMasks(const AnsiString AMasks);
  TFileMasks & __fastcall operator =(const TFileMasks & rhm);
  TFileMasks & __fastcall operator =(const char * rhs);
  TFileMasks & __fastcall operator =(const AnsiString rhs);
  bool __fastcall operator ==(const TFileMasks & rhm) const;
  bool __fastcall operator ==(const AnsiString rhs) const;
  bool __fastcall Matches(AnsiString FileName) const;
  bool __fastcall IsValid();
  bool __fastcall IsValid(int & Start, int & Length);

  __property AnsiString Masks = { read = FMasks, write = FMasks };

private:
  AnsiString FMasks;
};
//---------------------------------------------------------------------------
#endif
