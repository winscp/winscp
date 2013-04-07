//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "Configuration.h"
#include "CoreMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
bool __fastcall ExceptionMessage(Exception * E, UnicodeString & Message)
{
  bool Result = true;
  if (Configuration->Usage != NULL)
  {
    if (dynamic_cast<EAccessViolation*>(E) != NULL)
    {
      Configuration->Usage->Inc(L"AccessViolations");
    }
    else if (dynamic_cast<EExternal*>(E) != NULL)
    {
      Configuration->Usage->Inc(L"ExternalExceptions");
    }
    else if (dynamic_cast<EHeapException*>(E) != NULL)
    {
      Configuration->Usage->Inc(L"HeapExceptions");
    }
  }

  if (dynamic_cast<EAbort *>(E) != NULL)
  {
    Result = false;
  }
  else if (dynamic_cast<EAccessViolation*>(E) != NULL)
  {
    Message = LoadStr(ACCESS_VIOLATION_ERROR2);
  }
  else if (E->Message.IsEmpty())
  {
    Result = false;
  }
  else
  {
    Message = E->Message;
  }
  return Result;
}
//---------------------------------------------------------------------------
TStrings * ExceptionToMoreMessages(Exception * E)
{
  TStrings * Result = NULL;
  UnicodeString Message;
  if (ExceptionMessage(E, Message))
  {
    Result = new TStringList();
    Result->Add(Message);
    ExtException * ExtE = dynamic_cast<ExtException *>(E);
    if (ExtE != NULL)
    {
      Result->AddStrings(ExtE->MoreMessages);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception * E) :
  Exception("")
{
  AddMoreMessages(E);
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception* E, UnicodeString Msg):
  Exception(Msg)
{
  AddMoreMessages(E);
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(UnicodeString Msg, Exception* E) :
  Exception("")
{
  // "copy exception"
  AddMoreMessages(E);
  // and append message to the end to more messages
  if (!Msg.IsEmpty())
  {
    if (Message.IsEmpty())
    {
      Message = Msg;
    }
    else
    {
      if (FMoreMessages == NULL)
      {
        FMoreMessages = new TStringList();
      }
      FMoreMessages->Append(Msg);
    }
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(UnicodeString Msg, UnicodeString MoreMessages,
    UnicodeString HelpKeyword) :
  Exception(Msg),
  FHelpKeyword(HelpKeyword)
{
  if (!MoreMessages.IsEmpty())
  {
    FMoreMessages = new TStringList();
    FMoreMessages->Text = MoreMessages;
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(UnicodeString Msg, TStrings* MoreMessages,
  bool Own, UnicodeString HelpKeyword) :
  Exception(Msg),
  FHelpKeyword(HelpKeyword)
{
  if (Own)
  {
    FMoreMessages = MoreMessages;
  }
  else
  {
    FMoreMessages = new TStringList();
    FMoreMessages->Assign(MoreMessages);
  }
}
//---------------------------------------------------------------------------
void __fastcall ExtException::AddMoreMessages(Exception* E)
{
  if (E != NULL)
  {
    if (FMoreMessages == NULL)
    {
      FMoreMessages = new TStringList();
    }

    ExtException * ExtE = dynamic_cast<ExtException *>(E);
    if (ExtE != NULL)
    {
      if (!ExtE->HelpKeyword.IsEmpty())
      {
        // we have to yet decide what to do now
        assert(HelpKeyword.IsEmpty());

        FHelpKeyword = ExtE->HelpKeyword;
      }

      if (ExtE->MoreMessages != NULL)
      {
        FMoreMessages->Assign(ExtE->MoreMessages);
      }
    }

    UnicodeString Msg;
    ExceptionMessage(E, Msg);

    // new exception does not have own message, this is in fact duplication of
    // the exception data, but the exception class may being changed
    if (Message.IsEmpty())
    {
      Message = Msg;
    }
    else if (!Msg.IsEmpty())
    {
      FMoreMessages->Insert(0, Msg);
    }

    if (FMoreMessages->Count == 0)
    {
      delete FMoreMessages;
      FMoreMessages = NULL;
    }
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::~ExtException()
{
  delete FMoreMessages;
}
//---------------------------------------------------------------------------
ExtException * __fastcall ExtException::Clone()
{
  return new ExtException(this, L"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall LastSysErrorMessage()
{
  int LastError = GetLastError();
  UnicodeString Result;
  if (LastError != 0)
  {
    Result = FORMAT(System_Sysconst_SOSError, (LastError, SysErrorMessage(LastError)));
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall EOSExtException::EOSExtException(UnicodeString Msg) :
  ExtException(Msg, LastSysErrorMessage())
{
}
//---------------------------------------------------------------------------
__fastcall EFatal::EFatal(Exception* E, UnicodeString Msg) :
  ExtException(Msg, E),
  FReopenQueried(false)
{
  EFatal * F = dynamic_cast<EFatal *>(E);
  if (F != NULL)
  {
    FReopenQueried = F->ReopenQueried;
  }
}
//---------------------------------------------------------------------------
ExtException * __fastcall EFatal::Clone()
{
  return new EFatal(this, L"");
}
//---------------------------------------------------------------------------
ExtException * __fastcall ESshTerminate::Clone()
{
  return new ESshTerminate(this, L"", Operation);
}
//---------------------------------------------------------------------------
__fastcall ECallbackGuardAbort::ECallbackGuardAbort() : EAbort(L"callback abort")
{
}
//---------------------------------------------------------------------------
Exception * __fastcall CloneException(Exception * E)
{
  ExtException * Ext = dynamic_cast<ExtException *>(E);
  if (Ext != NULL)
  {
    return Ext->Clone();
  }
  else if (dynamic_cast<ECallbackGuardAbort *>(E) != NULL)
  {
    return new ECallbackGuardAbort();
  }
  else if (dynamic_cast<EAbort *>(E) != NULL)
  {
    return new EAbort(E->Message);
  }
  else
  {
    return new Exception(E->Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall RethrowException(Exception * E)
{
  if (dynamic_cast<EFatal *>(E) != NULL)
  {
    throw EFatal(E, L"");
  }
  else if (dynamic_cast<ECallbackGuardAbort *>(E) != NULL)
  {
    throw ECallbackGuardAbort();
  }
  else if (dynamic_cast<EAbort *>(E) != NULL)
  {
    throw EAbort(E->Message);
  }
  else
  {
    throw ExtException(E, L"");
  }
}
