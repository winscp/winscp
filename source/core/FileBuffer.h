//---------------------------------------------------------------------------
#ifndef FileBufferH
#define FileBufferH

#include <classes.hpp>
//---------------------------------------------------------------------------
extern const wchar_t * EOLTypeNames;
enum TEOLType { eolLF /* \n */, eolCRLF /* \r\n */, eolCR /* \r */ };
const int cpRemoveCtrlZ = 0x01;
const int cpRemoveBOM =   0x02;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TTransferOutEvent)(TObject * Sender, const unsigned char * Data, size_t Len);
//---------------------------------------------------------------------------
class TFileBuffer
{
public:
  __fastcall TFileBuffer();
  virtual __fastcall ~TFileBuffer();
  void __fastcall Convert(char * Source, char * Dest, int Params, bool & Token);
  void __fastcall Convert(TEOLType Source, TEOLType Dest, int Params, bool & Token);
  void __fastcall Convert(char * Source, TEOLType Dest, int Params, bool & Token);
  void __fastcall Convert(TEOLType Source, char * Dest, int Params, bool & Token);
  void __fastcall Insert(int Index, const char * Buf, int Len);
  void __fastcall Delete(int Index, int Len);
  DWORD __fastcall LoadStream(TStream * Stream, const DWORD Len, bool ForceLen);
  DWORD __fastcall ReadStream(TStream * Stream, const DWORD Len, bool ForceLen);
  void __fastcall WriteToStream(TStream * Stream, const DWORD Len);
  void __fastcall WriteToOut(TTransferOutEvent OnTransferOut, TObject * Sender, const DWORD Len);
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
class TSafeHandleStream : public THandleStream
{
public:
  __fastcall TSafeHandleStream(int AHandle);
  virtual int __fastcall Read(void * Buffer, int Count);
  virtual int __fastcall Write(const void * Buffer, int Count);
  virtual int __fastcall Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count);
  virtual int __fastcall Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count);
};
//---------------------------------------------------------------------------
char * __fastcall EOLToStr(TEOLType EOLType);
//---------------------------------------------------------------------------
#endif
