//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileBuffer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TFileBuffer::TFileBuffer()
{
  FMemory = new TMemoryStream();
  FSize = 0;
}
//---------------------------------------------------------------------------
__fastcall TFileBuffer::~TFileBuffer()
{
  delete FMemory;
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::SetSize(int value)
{
  if (FSize != value)
  {
    FMemory->Size = value;
    FSize = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::SetPosition(int value)
{
  FMemory->Position = value;
}
//---------------------------------------------------------------------------
int __fastcall TFileBuffer::GetPosition() const
{
  return (int)FMemory->Position;
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::SetMemory(TMemoryStream * value)
{
  if (FMemory != value)
  {
    if (FMemory) delete FMemory;
    FMemory = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::ReadStream(TStream * Stream, const DWORD Len)
{
  Size = Position + Len;
  // C++5
  // FMemory->SetSize(FMemory->Position + Len);
  Stream->ReadBuffer(Data + Position, Len);
  FMemory->Seek(Len, soFromCurrent);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::ReadFile(const HANDLE File, const DWORD Len)
{
  TStream *Stream = NULL;
  try
  {
    Stream = new THandleStream((THandle)File);
    ReadStream(Stream, Len);
  }
  __finally
  {
    delete Stream;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::LoadFile(const HANDLE File, const DWORD Len)
{
  FMemory->Seek(0, soFromBeginning);
  ReadFile(File, Len);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::ConvertEOL(TEOLType EOLType)
{
  char *Ptr = Data;
  Boolean PrevWasCR = False;

  for (Integer Index = 0; Index < Size; Index++)
  {
    if (*Ptr == '\n') /* LF */
    {
      if ((EOLType == eolCRLF) && !PrevWasCR)
      {
        Insert(Index, "\r", 1);
        Index++; Ptr++;
      }
    }
    else if (*Ptr == '\r') /* CR */
    {
      if (EOLType == eolLF)
      {
        Delete(Index, 1);
        Index--; Ptr--;
      }
    };

    PrevWasCR = (Index && (*Ptr == '\r'));
    Ptr++;
  }
  // Never allow CR at end of buffer,
  // it would be duplicated in next buffer starting with LF
  if (PrevWasCR) Delete(Size-1, 1);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Insert(int Index, const char * Buf, int Len)
{
  Size += Len;
  memmove(Data + Index + Len, Data + Index, Size - Index - Len);
  memmove(Data + Index, Buf, Len);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Delete(int Index, int Len)
{
  memmove(Data + Index, Data + Index + Len, Size - Index - Len);
  Size -= Len;
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::WriteToStream(TStream * Stream, const DWORD Len)
{
  Stream->WriteBuffer(Data + Position, Len);
  FMemory->Seek(Len, soFromCurrent);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::WriteToFile(const HANDLE File, const DWORD Len) 
{
  TStream *Stream = NULL;
  try
  {
    Stream = new THandleStream((THandle)File);
    WriteToStream(Stream, Len);
  }
  __finally
  {
    delete Stream;
  }
}
