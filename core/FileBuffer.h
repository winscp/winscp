//---------------------------------------------------------------------------
#ifndef FileBufferH
#define FileBufferH

#include <classes.hpp>
//---------------------------------------------------------------------------
enum TEOLType { eolLF /* \n */, eolCRLF /* \r\n */ };
//---------------------------------------------------------------------------
class TFileBuffer
{
public:
  __fastcall TFileBuffer();
  virtual __fastcall ~TFileBuffer();
  void __fastcall LoadFile(const HANDLE File, const DWORD Len);
  void __fastcall ConvertEOL(TEOLType EOLType);
  void __fastcall Insert(int Index, const char * Buf, int Len);
  void __fastcall Delete(int Index, int Len);
  void __fastcall ReadFile(const HANDLE File, const DWORD Len);
  void __fastcall ReadStream(TStream * Stream, const DWORD Len);
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
#endif
