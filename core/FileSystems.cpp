//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileSystems.h"
#include "RemoteFiles.h"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TCustomFileSystem::TCustomFileSystem(TTerminal * ATerminal):
  TObject(), FTerminal(ATerminal)
{
  assert(FTerminal);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomFileSystem::CompleteCustomCommand(AnsiString Command,
  const AnsiString FileName, TGetParamValueEvent OnGetParamValue)
{
  char * Ptr = Command.c_str();
  
  do
  {
    Ptr = strchr(Ptr, '!');
    if (Ptr)
    {
      int P = Ptr - Command.c_str() + 1;
      if (*(Ptr+1) == '!')
      {
        Command.Delete(P, 1);
      }
      /*else if (*(Ptr+1) == '/')
      {
        if (File)
        {
          Command.Delete(P, 2);
          Command.Insert(File->Directory->FullDirectory, P);
          P += File->Directory->FullDirectory.Length() - 1;
        }
      } */
      else if (*(Ptr+1) == '?')
      {
        char * Ptr2 = strchr(Ptr + 2, '?');
        char * Ptr3 = strchr(Ptr + 1, '!');
        if (Ptr2 && Ptr3 && Ptr2 < Ptr3)
        {
          if (OnGetParamValue)
          {
            AnsiString Title(Ptr + 2, Ptr2 - Ptr - 2);
            AnsiString Value(Ptr2 + 1, Ptr3 - Ptr2 - 1);
            OnGetParamValue(Title, Value);
            Command.Delete(P, Ptr3 - Ptr + 1);
            Command.Insert(Value, P);
            P += Value.Length() - 1;
          }
          else
          {
            P += Ptr3 - Ptr;
          }
        }
        else
        {
          P++;
        }
      }
      else
      {
        if (!FileName.IsEmpty())
        {
          Command.Delete(P, 1);
          Command.Insert(FileName, P);
          P += FileName.Length() - 1;
        }
      }
      Ptr = Command.c_str() + P;
    }
  }
  while (Ptr);
  
  return Command;
}

