//---------------------------------------------------------------------------
#ifndef FileBufferH
#define FileBufferH

#include <classes.hpp>
//---------------------------------------------------------------------------
enum TEOLType { eolLF /* \n */, eolCRLF /* \r\n */, eolCR /* \r */ };
//---------------------------------------------------------------------------
class TFileBuffer
{
public:
  __fastcall TFileBuffer();
  virtual __fastcall ~TFileBuffer();
  DWORD __fastcall LoadFile(const HANDLE File, const DWORD Len, bool ForceLen);
  void __fastcall ConvertEOL(char * Source, char * Dest);
  void __fastcall ConvertEOL(TEOLType Source, TEOLType Dest);
  void __fastcall ConvertEOL(char * Source, TEOLType Dest);
  void __fastcall ConvertEOL(TEOLType Source, char * Dest);
  void __fastcall Insert(int Index, const char * Buf, int Len);
  void __fastcall Delete(int Index, int Len);
  DWORD __fastcall ReadFile(const HANDLE File, const DWORD Len, bool ForceLen);
  DWORD __fastcall ReadStream(TStream * Stream, const DWORD Len, bool ForceLen);
  void __fastcall WriteToStream(TStream * Stream, const DWORD Len);
  void __fastcall WriteToFile(const HANDLE File, const DWORD Len);
  __property TMemoryStream * Memory  = { read=FMemory, write=SetMemory };
  __property char * Data = { read=GetData };
  __property int Size = { read=FSize, write=SetSize };
  __property int Position = { read=GetPosition, write=SetPosition };

private:
  TMemoryStream * FMemory;
  int FSize;
  
  void __fastcall SetMemory(TMemoryStream * value);
  char * __fastcall GetData() const { return (char *)FMemory->Memory; }
  void __fastcall SetSize(int value);
  void __fastcall SetPosition(int value);
  int __fastcall GetPosition() const;
};
//---------------------------------------------------------------------------
char * __fastcall EOLToStr(TEOLType EOLType);
//---------------------------------------------------------------------------
#endif
