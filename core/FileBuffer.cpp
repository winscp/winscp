//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "FileBuffer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
char * __fastcall EOLToStr(TEOLType EOLType)
{
  switch (EOLType) {
    case eolLF: return "\n";
    case eolCRLF: return "\r\n";
    case eolCR: return "\r";
    default: assert(false); return "";
  }
}
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
DWORD __fastcall TFileBuffer::ReadStream(TStream * Stream, const DWORD Len, bool ForceLen)
{
  Size = Position + Len;
  // C++5
  // FMemory->SetSize(FMemory->Position + Len);
  DWORD Result;
  if (ForceLen)
  {
    Stream->ReadBuffer(Data + Position, Len);
    Result = Len;
  }
  else
  {
    Result = Stream->Read(Data + Position, Len);
  }
  if (Result != Len)
  {
    Size = Size - Len + Result;
  }
  FMemory->Seek(Len, soFromCurrent);
  return Result;
}
//---------------------------------------------------------------------------
DWORD __fastcall TFileBuffer::ReadFile(const HANDLE File, const DWORD Len, bool ForceLen)
{
  DWORD Result;
  TStream *Stream = NULL;
  try
  {
    Stream = new THandleStream((THandle)File);
    Result = ReadStream(Stream, Len, ForceLen);
  }
  __finally
  {
    delete Stream;
  }
  return Result;
}
//---------------------------------------------------------------------------
DWORD __fastcall TFileBuffer::LoadFile(const HANDLE File, const DWORD Len, bool ForceLen)
{
  FMemory->Seek(0, soFromBeginning);
  return ReadFile(File, Len, ForceLen);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Convert(char * Source, char * Dest, int Params)
{
  assert(strlen(Source) <= 2);
  assert(strlen(Dest) <= 2);
  bool RemoveCtrlZ = ((Params & cpRemoveCtrlZ) != 0);

  if (strcmp(Source, Dest) == 0)
  {
    return;
  }

  char * Ptr = Data;

  // one character source EOL
  if (!Source[1])
  {
    for (int Index = 0; Index < Size; Index++)
    {
      if (*Ptr == Source[0])
      {
        *Ptr = Dest[0];
        if (Dest[1])
        {
          Insert(Index+1, Dest+1, 1);
          Index++;
          Ptr = Data + Index;
        }
      }
      // this should fix LF -> CR/LF conversion "bug" on CR/FL files,
      // which led to CR/CR/FL
      else if (*Ptr == Dest[0] || *Ptr == Dest[1])
      {
        Delete(Index, 1);
        Index--;
        Ptr = Data + Index;
      }
      Ptr++;
    }
  }
  // two character source EOL
  else
  {
    int Index;
    for (Index = 0; Index < Size - 1; Index++)
    {
      if ((*Ptr == Source[0]) && (*(Ptr+1) == Source[1]))
      {
        *Ptr = Dest[0];
        if (Dest[1])
        {
          *(Ptr+1) = Dest[1];
          Index++; Ptr++;
        }
        else
        {
          Delete(Index+1, 1);
          Ptr = Data + Index;
        }
      }
      Ptr++;
    }
    if ((Index < Size) && (*Ptr == Source[0]))
    {
      Delete(Index, 1);
    }
  }

  if (RemoveCtrlZ && (Size > 0) && ((*(Data + Size - 1)) == '\x1A'))
  {
    Delete(Size-1, 1);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Convert(TEOLType Source, TEOLType Dest, int Params)
{
  Convert(EOLToStr(Source), EOLToStr(Dest), Params);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Convert(char * Source, TEOLType Dest, int Params)
{
  Convert(Source, EOLToStr(Dest), Params);
}
//---------------------------------------------------------------------------
void __fastcall TFileBuffer::Convert(TEOLType Source, char * Dest, int Params)
{
  Convert(EOLToStr(Source), Dest, Params);
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
