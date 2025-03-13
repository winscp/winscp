//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>
#include <lmcons.h>
#define SECURITY_WIN32
#include <sspi.h>
#include <secext.h>

#include "Common.h"
#include "SessionInfo.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "CoreMain.h"
#include "Script.h"
#include <System.IOUtils.hpp>
#include <DateUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
UnicodeString __fastcall DoXmlEscape(UnicodeString Str, bool NewLine)
{
  for (int i = 1; i <= Str.Length(); i++)
  {
    UnicodeString Repl;
    wchar_t Ch = Str[i];
    switch (Ch)
    {
      case L'\x00': // \0 Is not valid in XML anyway
      case L'\x01':
      case L'\x02':
      case L'\x03':
      case L'\x04':
      case L'\x05':
      case L'\x06':
      case L'\x07':
      case L'\x08':
      // \n is handled below
      case L'\x0B':
      case L'\x0C':
      // \r is handled below
      case L'\x0E':
      case L'\x0F':
      case L'\x10':
      case L'\x11':
      case L'\x12':
      case L'\x13':
      case L'\x14':
      case L'\x15':
      case L'\x16':
      case L'\x17':
      case L'\x18':
      case L'\x19':
      case L'\x1A':
      case L'\x1B':
      case L'\x1C':
      case L'\x1D':
      case L'\x1E':
      case L'\x1F':
        Repl = L"#x" + ByteToHex((unsigned char)Ch) + L";";
        break;

      case L'\xFFFE':
      case L'\xFFFF':
        Repl = L"#x" + CharToHex(Ch) + L";";
        break;

      case L'&':
        Repl = L"amp;";
        break;

      case L'>':
        Repl = L"gt;";
        break;

      case L'<':
        Repl = L"lt;";
        break;

      case L'"':
        Repl = L"quot;";
        break;

      case L'\n':
        if (NewLine)
        {
          Repl = L"#10;";
        }
        break;

      case L'\r':
        Str.Delete(i, 1);
        i--;
        break;
    }

    if (!Repl.IsEmpty())
    {
      Str[i] = L'&';
      Str.Insert(Repl, i + 1);
      i += Repl.Length();
    }
  }
  return Str;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall XmlEscape(UnicodeString Str)
{
  return DoXmlEscape(Str, false);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall XmlAttributeEscape(UnicodeString Str)
{
  return DoXmlEscape(Str, true);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#pragma warn -inl
class TSessionActionRecord
{
public:
  __fastcall TSessionActionRecord(TActionLog * Log, TLogAction Action) :
    FLog(Log),
    FAction(Action),
    FState(Opened),
    FRecursive(false),
    FErrorMessages(NULL),
    FNames(new TStringList()),
    FValues(new TStringList()),
    FFileList(NULL),
    FFile(NULL)
  {
    FLog->AddPendingAction(this);
  }

  __fastcall ~TSessionActionRecord()
  {
    delete FErrorMessages;
    delete FNames;
    delete FValues;
    delete FFileList;
    delete FFile;
  }

  void __fastcall Restart()
  {
    FState = Opened;
    FRecursive = false;
    delete FErrorMessages;
    FErrorMessages = NULL;
    delete FFileList;
    FFileList = NULL;
    delete FFile;
    FFile = NULL;
    FNames->Clear();
    FValues->Clear();
  }

  bool __fastcall Record()
  {
    bool Result = (FState != Opened);
    if (Result)
    {
      if (FLog->FLogging && (FState != Cancelled))
      {
        const wchar_t * Name = ActionName();
        UnicodeString Attrs;
        if (FRecursive)
        {
          Attrs = L" recursive=\"true\"";
        }
        FLog->AddIndented(FORMAT(L"<%s%s>", (Name,  Attrs)));
        for (int Index = 0; Index < FNames->Count; Index++)
        {
          UnicodeString Value = FValues->Strings[Index];
          if (Value.IsEmpty())
          {
            FLog->AddIndented(FORMAT(L"  <%s />", (FNames->Strings[Index])));
          }
          else
          {
            FLog->AddIndented(FORMAT(L"  <%s value=\"%s\" />",
              (FNames->Strings[Index], XmlAttributeEscape(Value))));
          }
        }
        if (FFileList != NULL)
        {
          FLog->AddIndented(L"  <files>");
          for (int Index = 0; Index < FFileList->Count; Index++)
          {
            TRemoteFile * File = FFileList->Files[Index];
            RecordFile(L"    ", File, true);
          }
          FLog->AddIndented(L"  </files>");
        }
        if (FFile != NULL)
        {
          RecordFile(L"  ", FFile, false);
        }
        if (FState == RolledBack)
        {
          if (FErrorMessages != NULL)
          {
            FLog->AddIndented(L"  <result success=\"false\">");
            FLog->AddMessages(L"    ", FErrorMessages);
            FLog->AddIndented(L"  </result>");
          }
          else
          {
            FLog->AddIndented(L"  <result success=\"false\" />");
          }
        }
        else
        {
          FLog->AddIndented(L"  <result success=\"true\" />");
        }
        FLog->AddIndented(FORMAT(L"</%s>", (Name)));
      }
      delete this;
    }
    return Result;
  }

  void __fastcall Commit()
  {
    Close(Committed);
  }

  void __fastcall Rollback(Exception * E)
  {
    DebugAssert(FErrorMessages == NULL);
    FErrorMessages = ExceptionToMoreMessages(E);
    Close(RolledBack);
  }

  void __fastcall Cancel()
  {
    Close(Cancelled);
  }

  void __fastcall FileName(const UnicodeString & FileName)
  {
    Parameter(L"filename", FileName);
  }

  void __fastcall Destination(const UnicodeString & Destination)
  {
    Parameter(L"destination", Destination);
  }

  void Size(__int64 Size)
  {
    Parameter(L"size", Size);
  }

  void __fastcall Rights(const TRights & Rights)
  {
    Parameter(L"permissions", Rights.Text);
  }

  void __fastcall Modification(const TDateTime & DateTime)
  {
    Parameter(L"modification", StandardTimestamp(DateTime));
  }

  void __fastcall Recursive()
  {
    FRecursive = true;
  }

  void __fastcall Command(const UnicodeString & Command)
  {
    Parameter(L"command", Command);
  }

  void __fastcall AddOutput(UnicodeString Output, bool StdError)
  {
    const wchar_t * Name = (StdError ? L"erroroutput" : L"output");
    int Index = FNames->IndexOf(Name);
    if (Index >= 0)
    {
      FValues->Strings[Index] = FValues->Strings[Index] + L"\r\n" + Output;
    }
    else
    {
      Parameter(Name, Output);
    }
  }

  void __fastcall ExitCode(int ExitCode)
  {
    Parameter(L"exitcode", IntToStr(ExitCode));
  }

  void __fastcall Checksum(const UnicodeString & Alg, const UnicodeString & Checksum)
  {
    Parameter(L"algorithm", Alg);
    Parameter(L"checksum", Checksum);
  }

  void __fastcall Cwd(const UnicodeString & Path)
  {
    Parameter(L"cwd", Path);
  }

  void __fastcall FileList(TRemoteFileList * FileList)
  {
    if (FFileList == NULL)
    {
      FFileList = new TRemoteFileList();
    }
    FileList->DuplicateTo(FFileList);
  }

  void __fastcall File(TRemoteFile * File)
  {
    if (FFile != NULL)
    {
      delete FFile;
    }
    FFile = File->Duplicate(true);
  }

  void __fastcall SynchronizeChecklistItem(const TSynchronizeChecklist::TItem * Item)
  {
    UnicodeString Action;
    bool RecordLocal = false;
    bool RecordRemote = false;
    switch (Item->Action)
    {
      case TSynchronizeChecklist::saUploadNew:
        Action = L"uploadnew";
        RecordLocal = true;
        break;
      case TSynchronizeChecklist::saDownloadNew:
        Action = L"downloadnew";
        RecordRemote = true;
        break;
      case TSynchronizeChecklist::saUploadUpdate:
        Action = L"uploadupdate";
        RecordLocal = true;
        RecordRemote = true;
        break;
      case TSynchronizeChecklist::saDownloadUpdate:
        Action = L"downloadupdate";
        RecordLocal = true;
        RecordRemote = true;
        break;
      case TSynchronizeChecklist::saDeleteRemote:
        Action = L"deleteremote";
        RecordRemote = true;
        break;
      case TSynchronizeChecklist::saDeleteLocal:
        Action = L"deletelocal";
        RecordLocal = true;
        break;
      default:
        DebugFail();
        break;
    }

    Parameter(L"action", Action);

    if (RecordLocal)
    {
      SynchronizeChecklistItemFileInfo(Item->GetLocalPath(), Item->IsDirectory, Item->Local);
    }
    if (RecordRemote)
    {
      SynchronizeChecklistItemFileInfo(Item->GetRemotePath(), Item->IsDirectory, Item->Remote);
    }
  }


protected:
  enum TState { Opened, Committed, RolledBack, Cancelled };

  inline void __fastcall Close(TState State)
  {
    DebugAssert(FState == Opened);
    FState = State;
    FLog->RecordPendingActions();
  }

  const wchar_t * __fastcall ActionName()
  {
    switch (FAction)
    {
      case laUpload: return L"upload";
      case laDownload: return L"download";
      case laTouch: return L"touch";
      case laChmod: return L"chmod";
      case laMkdir: return L"mkdir";
      case laRm: return L"rm";
      case laMv: return L"mv";
      case laCp: return L"cp";
      case laCall: return L"call";
      case laLs: return L"ls";
      case laStat: return L"stat";
      case laChecksum: return L"checksum";
      case laCwd: return L"cwd";
      case laDifference: return L"difference";
      default: DebugFail(); return L"";
    }
  }

  void __fastcall Parameter(const UnicodeString & Name, const UnicodeString & Value = L"")
  {
    FNames->Add(Name);
    FValues->Add(Value);
  }

  void __fastcall RecordFile(const UnicodeString & Indent, TRemoteFile * File, bool IncludeFileName)
  {
    FLog->AddIndented(Indent + L"<file>");
    if (IncludeFileName)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <filename value=\"%s\" />", (XmlAttributeEscape(File->FileName))));
    }
    FLog->AddIndented(Indent + FORMAT(L"  <type value=\"%s\" />", (XmlAttributeEscape(towupper(File->Type)))));
    if (!File->IsDirectory)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <size value=\"%s\" />", (IntToStr(File->Size))));
    }
    if (File->ModificationFmt != mfNone)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <modification value=\"%s\" />", (StandardTimestamp(File->Modification))));
    }
    if (!File->Rights->Unknown)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <permissions value=\"%s\" />", (XmlAttributeEscape(File->Rights->Text))));
    }
    if (File->Owner.IsSet)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <owner value=\"%s\" />", (XmlAttributeEscape(File->Owner.DisplayText))));
    }
    if (File->Group.IsSet)
    {
      FLog->AddIndented(Indent + FORMAT(L"  <group value=\"%s\" />", (XmlAttributeEscape(File->Group.DisplayText))));
    }
    FLog->AddIndented(Indent + L"</file>");
  }

  void __fastcall SynchronizeChecklistItemFileInfo(
    const UnicodeString & AFileName, bool IsDirectory, const TSynchronizeChecklist::TItem::TFileInfo FileInfo)
  {
    Parameter(L"type", (IsDirectory ? L'D' : L'-'));
    FileName(AFileName);
    if (!IsDirectory)
    {
      Parameter(L"size", IntToStr(FileInfo.Size));
    }
    if (FileInfo.ModificationFmt != mfNone)
    {
      Modification(FileInfo.Modification);
    }
  }

private:
  TActionLog * FLog;
  TLogAction FAction;
  TState FState;
  bool FRecursive;
  TStrings * FErrorMessages;
  TStrings * FNames;
  TStrings * FValues;
  TRemoteFileList * FFileList;
  TRemoteFile * FFile;
};
#pragma warn .inl
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSessionAction::TSessionAction(TActionLog * Log, TLogAction Action)
{
  FCancelled = false;
  if (Log->FLogging)
  {
    FRecord = new TSessionActionRecord(Log, Action);
  }
  else
  {
    FRecord = NULL;
  }
}
//---------------------------------------------------------------------------
__fastcall TSessionAction::~TSessionAction()
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    Record->Commit();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Restart()
{
  if (FRecord != NULL)
  {
    FRecord->Restart();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Rollback(Exception * E)
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    Record->Rollback(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionAction::Cancel()
{
  if (FRecord != NULL)
  {
    TSessionActionRecord * Record = FRecord;
    FRecord = NULL;
    FCancelled = true;
    Record->Cancel();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSessionAction::IsValid()
{
  return !FCancelled;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileSessionAction::TFileSessionAction(TActionLog * Log, TLogAction Action) :
  TSessionAction(Log, Action)
{
}
//---------------------------------------------------------------------------
__fastcall TFileSessionAction::TFileSessionAction(
    TActionLog * Log, TLogAction Action, const UnicodeString & AFileName) :
  TSessionAction(Log, Action)
{
  FileName(AFileName);
}
//---------------------------------------------------------------------------
void __fastcall TFileSessionAction::FileName(const UnicodeString & FileName)
{
  if (FRecord != NULL)
  {
    FRecord->FileName(FileName);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TActionLog * Log, TLogAction Action) :
  TFileSessionAction(Log, Action)
{
}
//---------------------------------------------------------------------------
__fastcall TFileLocationSessionAction::TFileLocationSessionAction(
    TActionLog * Log, TLogAction Action, const UnicodeString & FileName) :
  TFileSessionAction(Log, Action, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TFileLocationSessionAction::Destination(const UnicodeString & Destination)
{
  if (FRecord != NULL)
  {
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TTransferSessionAction::TTransferSessionAction(TActionLog * Log, TLogAction Action) :
  TFileLocationSessionAction(Log, Action)
{
}
//---------------------------------------------------------------------------
void TTransferSessionAction::Size(__int64 Size)
{
  if (FRecord != NULL)
  {
    FRecord->Size(Size);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TUploadSessionAction::TUploadSessionAction(TActionLog * Log) :
  TTransferSessionAction(Log, laUpload)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TDownloadSessionAction::TDownloadSessionAction(TActionLog * Log) :
  TTransferSessionAction(Log, laDownload)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TChmodSessionAction::TChmodSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laChmod, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TChmodSessionAction::Recursive()
{
  if (FRecord != NULL)
  {
    FRecord->Recursive();
  }
}
//---------------------------------------------------------------------------
__fastcall TChmodSessionAction::TChmodSessionAction(
    TActionLog * Log, const UnicodeString & FileName, const TRights & ARights) :
  TFileSessionAction(Log, laChmod, FileName)
{
  Rights(ARights);
}
//---------------------------------------------------------------------------
void __fastcall TChmodSessionAction::Rights(const TRights & Rights)
{
  if (FRecord != NULL)
  {
    FRecord->Rights(Rights);
  }
}
//---------------------------------------------------------------------------
__fastcall TTouchSessionAction::TTouchSessionAction(
    TActionLog * Log, const UnicodeString & FileName, const TDateTime & Modification) :
  TFileSessionAction(Log, laTouch, FileName)
{
  if (FRecord != NULL)
  {
    FRecord->Modification(Modification);
  }
}
//---------------------------------------------------------------------------
__fastcall TMkdirSessionAction::TMkdirSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laMkdir, FileName)
{
}
//---------------------------------------------------------------------------
__fastcall TRmSessionAction::TRmSessionAction(
    TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laRm, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TRmSessionAction::Recursive()
{
  if (FRecord != NULL)
  {
    FRecord->Recursive();
  }
}
//---------------------------------------------------------------------------
__fastcall TMvSessionAction::TMvSessionAction(TActionLog * Log,
    const UnicodeString & FileName, const UnicodeString & ADestination) :
  TFileLocationSessionAction(Log, laMv, FileName)
{
  Destination(ADestination);
}
//---------------------------------------------------------------------------
__fastcall TCpSessionAction::TCpSessionAction(TActionLog * Log,
    const UnicodeString & FileName, const UnicodeString & ADestination) :
  TFileLocationSessionAction(Log, laCp, FileName)
{
  Destination(ADestination);
}
//---------------------------------------------------------------------------
__fastcall TCallSessionAction::TCallSessionAction(TActionLog * Log,
    const UnicodeString & Command, const UnicodeString & Destination) :
  TSessionAction(Log, laCall)
{
  if (FRecord != NULL)
  {
    FRecord->Command(Command);
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCallSessionAction::AddOutput(const UnicodeString & Output, bool StdError)
{
  if (FRecord != NULL)
  {
    FRecord->AddOutput(Output, StdError);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCallSessionAction::ExitCode(int ExitCode)
{
  if (FRecord != NULL)
  {
    FRecord->ExitCode(ExitCode);
  }
}
//---------------------------------------------------------------------------
__fastcall TLsSessionAction::TLsSessionAction(TActionLog * Log,
    const UnicodeString & Destination) :
  TSessionAction(Log, laLs)
{
  if (FRecord != NULL)
  {
    FRecord->Destination(Destination);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLsSessionAction::FileList(TRemoteFileList * FileList)
{
  if (FRecord != NULL)
  {
    FRecord->FileList(FileList);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TStatSessionAction::TStatSessionAction(TActionLog * Log, const UnicodeString & FileName) :
  TFileSessionAction(Log, laStat, FileName)
{
}
//---------------------------------------------------------------------------
void __fastcall TStatSessionAction::File(TRemoteFile * File)
{
  if (FRecord != NULL)
  {
    FRecord->File(File);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TChecksumSessionAction::TChecksumSessionAction(TActionLog * Log) :
  TFileSessionAction(Log, laChecksum)
{
}
//---------------------------------------------------------------------------
void __fastcall TChecksumSessionAction::Checksum(const UnicodeString & Alg, const UnicodeString & Checksum)
{
  if (FRecord != NULL)
  {
    FRecord->Checksum(Alg, Checksum);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCwdSessionAction::TCwdSessionAction(TActionLog * Log, const UnicodeString & Path) :
  TSessionAction(Log, laCwd)
{
  if (FRecord != NULL)
  {
    FRecord->Cwd(Path);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TDifferenceSessionAction::TDifferenceSessionAction(TActionLog * Log, const TSynchronizeChecklist::TItem * Item) :
  TSessionAction(Log, laDifference)
{
  if (FRecord != NULL)
  {
    FRecord->SynchronizeChecklistItem(Item);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSessionInfo::TSessionInfo()
{
  LoginTime = Now();
  CertificateVerifiedManually = false;
}
//---------------------------------------------------------------------------
TFileSystemInfo::TFileSystemInfo()
{
  memset(&IsCapable, false, sizeof(IsCapable));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
FILE * __fastcall OpenFile(UnicodeString LogFileName, TDateTime Started, TSessionData * SessionData, bool Append, UnicodeString & NewFileName)
{
  FILE * Result;
  UnicodeString ANewFileName = GetExpandedLogFileName(LogFileName, Started, SessionData);
  Result = _wfopen(ApiPath(ANewFileName).c_str(), (Append ? L"ab" : L"wb"));
  if (Result != NULL)
  {
    setvbuf(Result, NULL, _IONBF, BUFSIZ);
    NewFileName = ANewFileName;
  }
  else
  {
    throw ECRTExtException(FMTLOAD(LOG_OPENERROR, (ANewFileName)));
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static const wchar_t *LogLineMarks = L"<>!.*";
__fastcall TSessionLog::TSessionLog(TSessionUI* UI, TDateTime Started, TSessionData * SessionData,
  TConfiguration * Configuration)
{
  FCriticalSection = new TCriticalSection;
  FLogging = false;
  FConfiguration = Configuration;
  FParent = NULL;
  FUI = UI;
  FSessionData = SessionData;
  FStarted = Started;
  FFile = NULL;
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
  FClosed = false;
}
//---------------------------------------------------------------------------
__fastcall TSessionLog::~TSessionLog()
{
  FClosed = true;
  ReflectSettings();
  DebugAssert(FFile == NULL);
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetParent(TSessionLog * Parent, const UnicodeString & Name)
{
  FParent = Parent;
  FName = Name;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToParent(TLogLineType Type, const UnicodeString & Line)
{
  DebugAssert(FParent != NULL);
  FParent->Add(Type, Line);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddToSelf(TLogLineType Type, const UnicodeString & Line)
{
  if (LogToFile())
  {
    if (FFile == NULL)
    {
      OpenLogFile();
    }

    if (FFile != NULL)
    {
      UnicodeString Timestamp = FormatDateTime(L" yyyy-mm-dd hh:nn:ss.zzz ", Now());
      UTF8String UtfLine = UTF8String(UnicodeString(LogLineMarks[Type]) + Timestamp + Line + L"\r\n");
      for (int Index = 1; Index <= UtfLine.Length(); Index++)
      {
        if ((UtfLine[Index] == '\n') &&
            ((Index == 1) || (UtfLine[Index - 1] != '\r')))
        {
          UtfLine.Insert('\r', Index);
        }
      }
      int Writing = UtfLine.Length();
      CheckSize(Writing);
      FCurrentFileSize += fwrite(UtfLine.c_str(), 1, Writing, (FILE *)FFile);
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::LogPartFileName(const UnicodeString & BaseName, int Index)
{
  UnicodeString Result;
  if (Index >= 1)
  {
    Result = FORMAT(L"%s.%d", (BaseName, Index));
  }
  else
  {
    Result = BaseName;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::CheckSize(__int64 Addition)
{
  __int64 MaxSize = FConfiguration->LogMaxSize;
  if ((MaxSize > 0) && (FCurrentFileSize + Addition >= MaxSize))
  {
    // Before we close it
    UnicodeString BaseName = FCurrentFileName;
    CloseLogFile();
    FCurrentFileSize = 0;

    int Index = 0;

    while (FileExists(LogPartFileName(BaseName, Index + 1)))
    {
      Index++;
    }

    int MaxCount = FConfiguration->LogMaxCount;

    do
    {
      UnicodeString LogPart = LogPartFileName(BaseName, Index);
      if ((MaxCount > 0) && (Index >= MaxCount))
      {
        DeleteFileChecked(LogPart);
      }
      else
      {
        THROWOSIFFALSE(RenameFile(LogPart, LogPartFileName(BaseName, Index + 1)));
      }
      Index--;
    }
    while (Index >= 0);

    OpenLogFile();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAdd(TLogLineType Type, UnicodeString Line,
  void __fastcall (__closure *f)(TLogLineType Type, const UnicodeString & Line))
{
  UnicodeString Prefix;

  if (!FName.IsEmpty())
  {
    Prefix = L"[" + FName + L"] ";
  }

  while (!Line.IsEmpty())
  {
    f(Type, Prefix + CutToChar(Line, L'\n', false));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Add(TLogLineType Type, const UnicodeString & Line)
{
  DebugAssert(FConfiguration);
  if (Logging)
  {
    try
    {
      if (FParent != NULL)
      {
        DoAdd(Type, Line, DoAddToParent);
      }
      else
      {
        TGuard Guard(FCriticalSection);

        DoAdd(Type, Line, DoAddToSelf);
      }
    }
    catch (Exception &E)
    {
      // We failed logging, turn it off and notify user.
      FConfiguration->Logging = false;
      try
      {
        throw ExtException(&E, MainInstructions(LoadStr(LOG_GEN_ERROR)));
      }
      catch (Exception &E)
      {
        AddException(&E);
        FUI->HandleExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddException(Exception * E)
{
  if (E != NULL)
  {
    Add(llException, ExceptionLogString(E));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::ReflectSettings()
{
  TGuard Guard(FCriticalSection);

  FLogging =
    !FClosed &&
    ((FParent != NULL) || FConfiguration->Logging);

  // if logging to file was turned off or log file was changed -> close current log file
  if ((FFile != NULL) &&
      (!LogToFile() || (FCurrentLogFileName != FConfiguration->LogFileName)))
  {
    CloseLogFile();
  }

  if (FFile != NULL)
  {
    CheckSize(0);
  }

}
//---------------------------------------------------------------------------
bool __fastcall TSessionLog::LogToFile()
{
  return Logging && FConfiguration->LogToFile && (FParent == NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::CloseLogFile()
{
  if (FFile != NULL)
  {
    fclose((FILE *)FFile);
    FFile = NULL;
  }
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::OpenLogFile()
{
  try
  {
    DebugAssert(FFile == NULL);
    DebugAssert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->LogFileName;
    FFile = OpenFile(FCurrentLogFileName, FStarted, FSessionData, FConfiguration->LogFileAppend, FCurrentFileName);
    TSearchRec SearchRec;
    if (FileSearchRec(FCurrentFileName, SearchRec))
    {
      FCurrentFileSize = SearchRec.Size;
    }
    else
    {
      FCurrentFileSize = 0;
    }
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = L"";
    FCurrentFileName = L"";
    FConfiguration->LogFileName = UnicodeString();
    try
    {
      throw ExtException(&E, MainInstructions(LoadStr(LOG_GEN_ERROR)));
    }
    catch (Exception & E)
    {
      AddException(&E);
      // not to deadlock with TSessionLog::ReflectSettings invoked by FConfiguration->LogFileName setter above
      TUnguard Unguard(FCriticalSection);
      FUI->HandleExtendedException(&E);
    }
  }

  // in case we are appending and the existing log file is already too large
  if (FFile != NULL)
  {
    CheckSize(0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddSystemInfo()
{
  AddStartupInfo(true);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddStartupInfo()
{
  AddStartupInfo(false);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddStartupInfo(bool System)
{
  TSessionData * Data = (System ? NULL : FSessionData);
  if (Logging)
  {
    if (FParent != NULL)
    {
      // do not add session info for secondary session
      // (this should better be handled in the TSecondaryTerminal)
    }
    else
    {
      DoAddStartupInfo(Data);
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::LogSensitive(const UnicodeString & Str)
{
  if (FConfiguration->LogSensitive && !Str.IsEmpty())
  {
    return NormalizeString(Str);
  }
  else
  {
    return BooleanToEngStr(!Str.IsEmpty());
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSessionLog::GetCmdLineLog(TConfiguration * AConfiguration)
{
  UnicodeString Result = CmdLine;

  if (!AConfiguration->LogSensitive)
  {
    TManagementScript Script(StoredSessions, false);
    Script.MaskPasswordInCommandLine(Result, true);
  }

  return Result;
}
//---------------------------------------------------------------------------
template <typename T>
UnicodeString __fastcall EnumName(T Value, UnicodeString Names)
{
  int N = int(Value);

  do
  {
    UnicodeString Name = CutToChar(Names, L';', true);
    if (N == 0)
    {
      return Name;
    }
    N--;
  }
  while ((N >= 0) && !Names.IsEmpty());

  return L"(unknown)";
}
#define ADSTR(S) AddLogEntry(S)
#define ADF(S, F) ADSTR(FORMAT(S, F));
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddStartupInfo(TAddLogEntryEvent AddLogEntry, TConfiguration * AConfiguration, bool DoNotMaskPaswords)
{
  ADSTR(GetEnvironmentInfo());
  THierarchicalStorage * Storage = AConfiguration->CreateConfigStorage();
  try
  {
    ADF(L"Configuration: %s", (Storage->Source));
  }
  __finally
  {
    delete Storage;
  }

  wchar_t UserName[UNLEN + 1];
  unsigned long UserNameSize = LENOF(UserName);
  if (DebugAlwaysFalse(!GetUserNameEx(NameSamCompatible, UserName, &UserNameSize)))
  {
    wcscpy(UserName, L"<Failed to retrieve username>");
  }
  UnicodeString LogStr;
  if (AConfiguration->LogProtocol <= -1)
  {
    LogStr = L"Reduced";
  }
  else if (AConfiguration->LogProtocol <= 0)
  {
    LogStr = L"Normal";
  }
  else if (AConfiguration->LogProtocol == 1)
  {
    LogStr = L"Debug 1";
  }
  else if (AConfiguration->LogProtocol >= 2)
  {
    LogStr = L"Debug 2";
  }
  if (AConfiguration->LogSensitive)
  {
    LogStr += L", Logging passwords";
  }
  if (AConfiguration->LogMaxSize > 0)
  {
    LogStr += FORMAT(L", Rotating after: %s", (SizeToStr(AConfiguration->LogMaxSize)));
    if (AConfiguration->LogMaxCount > 0)
    {
      LogStr += FORMAT(L", Keeping at most %d logs", (AConfiguration->LogMaxCount));
    }
  }
  ADF(L"Log level: %s", (LogStr));
  ADF(L"Local account: %s", (UserName));
  ADF(L"Working directory: %s", (GetCurrentDir()));
  ADF(L"Process ID: %d", (int(GetCurrentProcessId())));
  ADF(L"Ancestor processes: %s", (GetAncestorProcessNames()));
  // This logs even passwords, contrary to a session log.
  // GetCmdLineLog requires master password, but we do not know it yet atm.
  UnicodeString ACmdLine;
  if (DoNotMaskPaswords)
  {
    ACmdLine = CmdLine;
  }
  else
  {
    ACmdLine = GetCmdLineLog(AConfiguration);
  }
  ADF(L"Command-line: %s", (ACmdLine));
  if (AConfiguration->ActualLogProtocol >= 1)
  {
    GetGlobalOptions()->LogOptions(AddLogEntry);
  }
  ADF(L"Time zone: %s", (GetTimeZoneLogString()));
  if (!AdjustClockForDSTEnabled())
  {
    ADSTR(L"Warning: System option \"Automatically adjust clock for Daylight Saving Time\" is disabled, timestamps will not be represented correctly");
  }
}
//---------------------------------------------------------------------------
#undef ADSTR
#define ADSTR(S) DoAdd(llMessage, S, DoAddToSelf);
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddStartupInfoEntry(const UnicodeString & S)
{
  ADSTR(S);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddStartupInfo(TSessionData * Data)
{
  if (Data == NULL)
  {
    AddSeparator();
    DoAddStartupInfo(DoAddStartupInfoEntry, FConfiguration, false);
    ADF(L"Login time: %s", (FormatDateTime(L"dddddd tt", Now())));
    AddSeparator();
  }
  else
  {
    ADF(L"Session name: %s (%s)", (Data->SessionName, Data->SourceName));
    UnicodeString AddressFamily;
    if (Data->AddressFamily != afAuto)
    {
      AddressFamily = FORMAT(L"%s, ", (Data->AddressFamily == afIPv4 ? L"IPv4" : L"IPv6"));
    }
    UnicodeString HostName = Data->HostNameExpanded;
    if (!Data->HostNameSource.IsEmpty())
    {
      HostName = FORMAT(L"%s [%s]", (HostName, Data->HostNameSource));
    }
    ADF(L"Host name: %s (%sPort: %d)", (HostName, AddressFamily, Data->PortNumber));
    UnicodeString UserName = Data->UserNameExpanded;
    if (!Data->UserNameSource.IsEmpty())
    {
      UserName = FORMAT(L"%s [%s]", (UserName, Data->UserNameSource));
    }
    ADF(L"User name: %s (Password: %s, Key file: %s, Passphrase: %s)",
      (UserName, LogSensitive(Data->Password),
       LogSensitive(Data->ResolvePublicKeyFile()), LogSensitive(Data->Passphrase)));
    if (Data->UsesSsh)
    {
      ADF(L"Tunnel: %s", (BooleanToEngStr(Data->Tunnel)));
      if (Data->Tunnel)
      {
        ADF(L"Tunnel: Host name: %s (Port: %d)", (Data->TunnelHostName, Data->TunnelPortNumber));
        ADF(L"Tunnel: User name: %s (Password: %s, Key file: %s)",
          (Data->TunnelUserName,
           LogSensitive(Data->TunnelPassword),
           LogSensitive(Data->TunnelPublicKeyFile)));
        ADF(L"Tunnel: Local port number: %d", (Data->TunnelLocalPortNumber));
      }
    }
    ADF(L"Transfer Protocol: %s", (Data->FSProtocolStr));
    if (Data->UsesSsh || (Data->FSProtocol == fsFTP))
    {
      UnicodeString PingType;
      int PingInterval;
      if (Data->FSProtocol == fsFTP)
      {
        PingType = EnumName(Data->FtpPingType, FtpPingTypeNames);
        PingInterval = Data->FtpPingInterval;
      }
      else
      {
        PingType = EnumName(Data->PingType, PingTypeNames);
        PingInterval = Data->PingInterval;
      }
      ADF(L"Ping type: %s, Ping interval: %d sec; Timeout: %d sec",
        (PingType, PingInterval, Data->Timeout));
      ADF(L"Disable Nagle: %s",
        (BooleanToEngStr(Data->TcpNoDelay)));
    }
    ADF(L"Proxy: %s",
      ((Data->FtpProxyLogonType != 0) ?
        FORMAT(L"FTP proxy %d", (Data->FtpProxyLogonType)) :
        EnumName(Data->ProxyMethod, ProxyMethodNames)));
    if ((Data->FtpProxyLogonType != 0) || (Data->ProxyMethod != ::pmNone))
    {
      ADF(L"HostName: %s (Port: %d); Username: %s; Passwd: %s",
        (Data->ProxyHost, Data->ProxyPort,
         Data->ProxyUsername, LogSensitive(Data->ProxyPassword)));
      if (Data->ProxyMethod == pmTelnet)
      {
        ADF(L"Telnet command: %s", (Data->ProxyTelnetCommand));
      }
      if (Data->ProxyMethod == pmCmd)
      {
        ADF(L"Local command: %s", (Data->ProxyLocalCommand));
      }
    }
    if (Data->UsesSsh || (Data->FSProtocol == fsFTP) || (Data->FSProtocol == fsS3))
    {
      ADF(L"Send buffer: %d", (Data->SendBuf));
    }
    if (Data->UsesSsh && !Data->SourceAddress.IsEmpty())
    {
      ADF(L"Source address: %s", (Data->SourceAddress));
    }
    if (Data->UsesSsh)
    {
      ADF(L"Compression: %s", (BooleanToEngStr(Data->Compression)));
      ADF(L"Bypass authentication: %s",
       (BooleanToEngStr(Data->SshNoUserAuth)));
      ADF(L"Try agent: %s; Agent forwarding: %s; KI: %s; GSSAPI: %s",
        (BooleanToEngStr(Data->TryAgent), BooleanToEngStr(Data->AgentFwd),
         BooleanToEngStr(Data->AuthKI), BooleanToEngStr(Data->AuthGSSAPI)));
      if (Data->AuthGSSAPI)
      {
        ADF(L"GSSAPI: KEX: %s; Forwarding: %s; Libs: %s; Custom: %s",
          (BooleanToEngStr(Data->AuthGSSAPIKEX), BooleanToEngStr(Data->GSSAPIFwdTGT), Data->GssLibList, Data->GssLibCustom));
      }
      ADF(L"Ciphers: %s; Ssh2DES: %s",
        (Data->CipherList, BooleanToEngStr(Data->Ssh2DES)));
      ADF(L"KEX: %s", (Data->KexList));
      UnicodeString Bugs;
      for (int Index = 0; Index < BUG_COUNT; Index++)
      {
        AddToList(Bugs, EnumName(Data->Bug[(TSshBug)Index], AutoSwitchNames), L",");
      }
      ADF(L"SSH Bugs: %s", (Bugs));
      ADF(L"Simple channel: %s", (BooleanToEngStr(Data->SshSimple)));
      ADF(L"Return code variable: %s; Lookup user groups: %s",
        ((Data->DetectReturnVar ? UnicodeString(L"Autodetect") : Data->ReturnVar),
         EnumName(Data->LookupUserGroups, AutoSwitchNames)));
      ADF(L"Shell: %s", ((Data->Shell.IsEmpty()? UnicodeString(L"default") : Data->Shell)));
      ADF(L"EOL: %s, UTF: %s", (EnumName(Data->EOLType, EOLTypeNames), EnumName(Data->NotUtf, NotAutoSwitchNames))); // NotUtf duplicated in FTP branch
      ADF(L"Clear aliases: %s, Unset nat.vars: %s, Resolve symlinks: %s; Follow directory symlinks: %s",
        (BooleanToEngStr(Data->ClearAliases), BooleanToEngStr(Data->UnsetNationalVars),
         BooleanToEngStr(Data->ResolveSymlinks), BooleanToEngStr(Data->FollowDirectorySymlinks)));
      ADF(L"LS: %s, Ign LS warn: %s, Scp1 Comp: %s; Exit code 1 is error: %s",
        (Data->ListingCommand,
         BooleanToEngStr(Data->IgnoreLsWarnings),
         BooleanToEngStr(Data->Scp1Compatibility),
         BooleanToEngStr(Data->ExitCode1IsError)));
    }
    if ((Data->FSProtocol == fsSFTP) || (Data->FSProtocol == fsSFTPonly))
    {
      UnicodeString Bugs;
      for (int Index = 0; Index < SFTP_BUG_COUNT; Index++)
      {
        AddToList(Bugs, EnumName(Data->SFTPBug[(TSftpBug)Index], AutoSwitchNames), L",");
      }
      ADF(L"SFTP Bugs: %s", (Bugs));
      ADF(L"SFTP Server: %s", ((Data->SftpServer.IsEmpty()? UnicodeString(L"default") : Data->SftpServer)));
      if (Data->SFTPRealPath != asAuto)
      {
        ADF(L"SFTP Real path: %s", (EnumName(Data->SFTPRealPath, AutoSwitchNames)));
      }
      if (Data->UsePosixRename)
      {
        ADF(L"Use POSIX rename: %s", (BooleanToEngStr(Data->UsePosixRename)));
      }
    }
    bool FtpsOn = false;
    if (Data->FSProtocol == fsFTP)
    {
      ADF(L"UTF: %s", (EnumName(Data->NotUtf, NotAutoSwitchNames))); // duplicated in UsesSsh branch
      UnicodeString Ftps;
      switch (Data->Ftps)
      {
        case ftpsImplicit:
          Ftps = L"Implicit TLS/SSL";
          FtpsOn = true;
          break;

        case ftpsExplicitSsl:
          Ftps = L"Explicit SSL/TLS";
          FtpsOn = true;
          break;

        case ftpsExplicitTls:
          Ftps = L"Explicit TLS/SSL";
          FtpsOn = true;
          break;

        default:
          DebugAssert(Data->Ftps == ftpsNone);
          Ftps = L"None";
          break;
      }
      // kind of hidden option, so do not reveal it unless it is set
      if (Data->FtpTransferActiveImmediately != asAuto)
      {
        ADF(L"Transfer active immediately: %s", (EnumName(Data->FtpTransferActiveImmediately, AutoSwitchNames)));
      }
      ADF(L"FTPS: %s [Client certificate: %s]",
        (Ftps, LogSensitive(Data->TlsCertificateFile)));
      ADF(L"FTP: Passive: %s [Force IP: %s]; MLSD: %s [List all: %s]; HOST: %s",
        (BooleanToEngStr(Data->FtpPasvMode),
         EnumName(Data->FtpForcePasvIp, AutoSwitchNames),
         EnumName(Data->FtpUseMlsd, AutoSwitchNames),
         EnumName(Data->FtpListAll, AutoSwitchNames),
         EnumName(Data->FtpHost, AutoSwitchNames)));
      if (Data->FtpWorkFromCwd != asAuto)
      {
        ADF(L"FTP: Relative paths: %s", (EnumName(Data->FtpWorkFromCwd, AutoSwitchNames)));
      }
    }
    if (Data->FSProtocol == fsWebDAV)
    {
      FtpsOn = (Data->Ftps != ftpsNone);
      ADF(L"HTTPS: %s [Client certificate: %s]",
        (BooleanToEngStr(FtpsOn), LogSensitive(Data->TlsCertificateFile)));
      ADF(L"WebDAV: Tolerate non-encoded: %s", (BooleanToEngStr(Data->WebDavLiberalEscaping)));
    }
    if (Data->FSProtocol == fsS3)
    {
      FtpsOn = (Data->Ftps != ftpsNone);
      ADF(L"HTTPS: %s", (BooleanToEngStr(FtpsOn)));
      ADF(L"S3: URL Style: %s", (EnumName(Data->S3UrlStyle, L"Virtual Host;Path")));
      if (!Data->S3DefaultRegion.IsEmpty())
      {
        ADF(L"S3: Default region: %s", (Data->S3DefaultRegion));
      }
      if (!Data->S3SessionToken.IsEmpty())
      {
        ADF(L"S3: Session token: %s", (Data->S3SessionToken));
      }
      if (!Data->S3RoleArn.IsEmpty())
      {
        ADF(L"S3: Role ARN: %s (session name: %s)", (Data->S3RoleArn, DefaultStr(Data->S3RoleSessionName, L"default")));
      }
      if (Data->S3CredentialsEnv)
      {
        ADF(L"S3: Credentials from AWS environment: %s", (DefaultStr(Data->S3Profile, L"General")));
      }
    }
    if (FtpsOn)
    {
      if (Data->FSProtocol == fsFTP)
      {
        ADF(L"Session reuse: %s", (BooleanToEngStr(Data->SslSessionReuse)));
      }
      ADF(L"TLS/SSL versions: %s-%s", (GetTlsVersionName(Data->MinTlsVersion), GetTlsVersionName(Data->MaxTlsVersion)));
    }
    ADF(L"Local directory: %s, Remote directory: %s, Update: %s, Cache: %s",
      ((Data->LocalDirectory.IsEmpty() ? UnicodeString(L"default") : Data->LocalDirectory),
       (Data->RemoteDirectory.IsEmpty() ? UnicodeString(L"home") : Data->RemoteDirectory),
       BooleanToEngStr(Data->UpdateDirectories),
       BooleanToEngStr(Data->CacheDirectories)));
    ADF(L"Cache directory changes: %s, Permanent: %s",
      (BooleanToEngStr(Data->CacheDirectoryChanges),
       BooleanToEngStr(Data->PreserveDirectoryChanges)));
    ADF(L"Recycle bin: Delete to: %s, Overwritten to: %s, Bin path: %s",
      (BooleanToEngStr(Data->DeleteToRecycleBin),
       BooleanToEngStr(Data->OverwrittenToRecycleBin),
       Data->RecycleBinPath));
    if (Data->TrimVMSVersions || Data->VMSAllRevisions)
    {
      ADF(L"Trim VMS versions: %s; VMS all revisions: %s",
        (BooleanToEngStr(Data->TrimVMSVersions), BooleanToEngStr(Data->VMSAllRevisions)));
    }
    UnicodeString TimeInfo;
    if ((Data->FSProtocol == fsSFTP) || (Data->FSProtocol == fsSFTPonly) || (Data->FSProtocol == fsSCPonly) || (Data->FSProtocol == fsWebDAV))
    {
      AddToList(TimeInfo, FORMAT(L"DST mode: %s", (EnumName(Data->DSTMode, DSTModeNames))), L";");
    }
    if ((Data->FSProtocol == fsSCPonly) || (Data->FSProtocol == fsFTP))
    {
      int TimeDifferenceMin = TimeToMinutes(Data->TimeDifference);
      AddToList(TimeInfo, FORMAT(L"Timezone offset: %dh %dm", ((TimeDifferenceMin / MinsPerHour), (TimeDifferenceMin % MinsPerHour))), L";");
    }
    ADSTR(TimeInfo);

    if (Data->FSProtocol == fsWebDAV)
    {
      ADF(L"Compression: %s",
        (BooleanToEngStr(Data->Compression)));
    }

    if ((Data->FSProtocol == fsFTP) && !Data->FtpPasvMode)
    {
      if (!Configuration->ExternalIpAddress.IsEmpty() || Configuration->HasLocalPortNumberLimits())
      {
        AddSeparator();
        ADF(L"FTP active mode interface: %s:%d-%d", (DefaultStr(Configuration->ExternalIpAddress, L"<system address>"), Configuration->LocalPortNumberMin, Configuration->LocalPortNumberMax));
      }
    }
    AddSeparator();
  }
}
//---------------------------------------------------------------------------
#undef ADF
#undef ADSTR
//---------------------------------------------------------------------------
UnicodeString TSessionLog::GetSeparator()
{
  return L"--------------------------------------------------------------------------";
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddSeparator()
{
  Add(llMessage, GetSeparator());
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TActionLog::TActionLog(TSessionUI * UI, TDateTime Started, TSessionData * SessionData,
  TConfiguration * Configuration)
{
  DebugAssert(UI != NULL);
  DebugAssert(SessionData != NULL);
  Init(UI, Started, SessionData, Configuration);
}
//---------------------------------------------------------------------------
__fastcall TActionLog::TActionLog(TDateTime Started, TConfiguration * Configuration)
{
  Init(NULL, Started, NULL, Configuration);
  // not associated with session, so no need to waiting for anything
  ReflectSettings();
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::Init(TSessionUI * UI, TDateTime Started, TSessionData * SessionData,
  TConfiguration * Configuration)
{
  FCriticalSection = new TCriticalSection;
  FConfiguration = Configuration;
  FUI = UI;
  FSessionData = SessionData;
  FStarted = Started;
  FFile = NULL;
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
  FLogging = false;
  FClosed = false;
  FFailed = false;
  FPendingActions = new TList();
  FIndent = L"  ";
  FInGroup = false;
  FEnabled = true;
}
//---------------------------------------------------------------------------
__fastcall TActionLog::~TActionLog()
{
  DebugAssert(FPendingActions->Count == 0);
  delete FPendingActions;
  FClosed = true;
  ReflectSettings();
  DebugAssert(FFile == NULL);
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::Add(const UnicodeString & Line)
{
  DebugAssert(FConfiguration);
  if (FLogging)
  {
    TGuard Guard(FCriticalSection);
    if (FFile == NULL)
    {
      OpenLogFile();
    }

    if (FFile != NULL)
    {
      try
      {
        UTF8String UtfLine = UTF8String(Line);
        size_t Written =
          fwrite(UtfLine.c_str(), 1, UtfLine.Length(), (FILE *)FFile);
        if (Written != static_cast<size_t>(UtfLine.Length()))
        {
          throw ECRTExtException(L"");
        }
        #ifdef _DEBUG
        #endif
        Written =
          fwrite("\n", 1, 1, (FILE *)FFile);
        if (Written != 1)
        {
          throw ECRTExtException(L"");
        }
      }
      catch (Exception &E)
      {
        FCriticalSection->Release();

        // avoid endless loop when trying to close tags when closing log, when logging has failed
        if (!FFailed)
        {
          FFailed = true;
          // We failed logging, turn it off and notify user.
          FConfiguration->LogActions = false;
          if (FConfiguration->LogActionsRequired)
          {
            throw EFatal(&E, LoadStr(LOG_FATAL_ERROR));
          }
          else
          {
            try
            {
              throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
            }
            catch (Exception &E)
            {
              if (FUI != NULL)
              {
                FUI->HandleExtendedException(&E);
              }
            }
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddIndented(const UnicodeString & Line)
{
  Add(FIndent + Line);
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddFailure(TStrings * Messages)
{
  AddIndented(L"<failure>");
  AddMessages(L"  ", Messages);
  AddIndented(L"</failure>");
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddFailure(Exception * E)
{
  TStrings * Messages = ExceptionToMoreMessages(E);
  if (Messages != NULL)
  {
    try
    {
      AddFailure(Messages);
    }
    __finally
    {
      delete Messages;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddMessages(UnicodeString Indent, TStrings * Messages)
{
  for (int Index = 0; Index < Messages->Count; Index++)
  {
    AddIndented(
      FORMAT(Indent + L"<message>%s</message>", (XmlEscape(Messages->Strings[Index]))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::ReflectSettings()
{
  TGuard Guard(FCriticalSection);

  bool ALogging =
    !FClosed && FConfiguration->LogActions && Enabled;

  if (ALogging && !FLogging)
  {
    FLogging = true;
    Add(XmlDeclaration);
    UnicodeString SessionName =
      (FSessionData != NULL) ? XmlAttributeEscape(FSessionData->SessionName) : UnicodeString(L"nosession");
    Add(FORMAT(L"<session xmlns=\"http://winscp.net/schema/session/1.0\" name=\"%s\" start=\"%s\">",
      (SessionName, StandardTimestamp())));
  }
  else if (!ALogging && FLogging)
  {
    if (FInGroup)
    {
      EndGroup();
    }
    // do not try to close the file, if it has not been opened, to avoid recursion
    if (FFile != NULL)
    {
      Add(L"</session>");
    }
    CloseLogFile();
    FLogging = false;
  }

}
//---------------------------------------------------------------------------
void __fastcall TActionLog::CloseLogFile()
{
  if (FFile != NULL)
  {
    fclose((FILE *)FFile);
    FFile = NULL;
  }
  FCurrentLogFileName = L"";
  FCurrentFileName = L"";
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::OpenLogFile()
{
  try
  {
    DebugAssert(FFile == NULL);
    DebugAssert(FConfiguration != NULL);
    FCurrentLogFileName = FConfiguration->ActionsLogFileName;
    FFile = OpenFile(FCurrentLogFileName, FStarted, FSessionData, false, FCurrentFileName);
  }
  catch (Exception & E)
  {
    // We failed logging to file, turn it off and notify user.
    FCurrentLogFileName = L"";
    FCurrentFileName = L"";
    FConfiguration->LogActions = false;
    if (FConfiguration->LogActionsRequired)
    {
      throw EFatal(&E, LoadStr(LOG_FATAL_ERROR));
    }
    else
    {
      try
      {
        throw ExtException(&E, LoadStr(LOG_GEN_ERROR));
      }
      catch (Exception & E)
      {
        if (FUI != NULL)
        {
          // not to deadlock with TSessionLog::ReflectSettings invoked by FConfiguration->LogFileName setter above
          TUnguard Unguard(FCriticalSection);
          FUI->HandleExtendedException(&E);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::AddPendingAction(TSessionActionRecord * Action)
{
  FPendingActions->Add(Action);
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::RecordPendingActions()
{
  while ((FPendingActions->Count > 0) &&
         static_cast<TSessionActionRecord *>(FPendingActions->Items[0])->Record())
  {
    FPendingActions->Delete(0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::BeginGroup(UnicodeString Name)
{
  DebugAssert(!FInGroup);
  FInGroup = true;
  DebugAssert(FIndent == L"  ");
  AddIndented(FORMAT(L"<group name=\"%s\" start=\"%s\">",
    (XmlAttributeEscape(Name), StandardTimestamp())));
  FIndent = L"    ";
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::EndGroup()
{
  DebugAssert(FInGroup);
  FInGroup = false;
  DebugAssert(FIndent == L"    ");
  FIndent = L"  ";
  // this is called from ReflectSettings that in turn is called when logging fails,
  // so do not try to close the group, if it has not been opened, to avoid recursion
  if (FFile != NULL)
  {
    AddIndented(L"</group>");
  }
}
//---------------------------------------------------------------------------
void __fastcall TActionLog::SetEnabled(bool value)
{
  if (Enabled != value)
  {
    FEnabled = value;
    ReflectSettings();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TApplicationLog::TApplicationLog()
{
  FFile = NULL;
  FLogging = false;
  FPeekReservedMemory = 0;
  FCriticalSection.reset(new TCriticalSection());
}
//---------------------------------------------------------------------------
TApplicationLog::~TApplicationLog()
{
  if (FFile != NULL)
  {
    Log(L"Closing log");
    fclose(static_cast<FILE *>(FFile));
    FFile = NULL;
  }
  FLogging = false;
}
//---------------------------------------------------------------------------
void TApplicationLog::Enable(const UnicodeString & Path)
{
  UnicodeString Dummy;
  FPath = Path;
  FFile = OpenFile(FPath, Now(), NULL, false, Dummy);
  FLogging = true;
}
//---------------------------------------------------------------------------
void TApplicationLog::AddStartupInfo()
{
  if (Logging)
  {
    TSessionLog::DoAddStartupInfo(Log, Configuration, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TApplicationLog::Log(const UnicodeString & S)
{
  if (FFile != NULL)
  {
    TDateTime N = Now();
    UnicodeString Timestamp = FormatDateTime(L"yyyy-mm-dd hh:nn:ss.zzz", N);
    UnicodeString Line = FORMAT(L"[%s] [%x] %s\r\n", (Timestamp, static_cast<int>(GetCurrentThreadId()), S));
    UTF8String UtfLine = UTF8String(Line);
    int Writting = UtfLine.Length();

    bool CheckMemory;

    {
      TGuard Guard(FCriticalSection.get());
      fwrite(UtfLine.c_str(), 1, Writting, static_cast<FILE *>(FFile));

      __int64 SecondsSinceLastMemoryCheck = SecondsBetween(N, FLastMemoryCheck);
      CheckMemory = (SecondsSinceLastMemoryCheck >= 10);
      if (CheckMemory)
      {
        FLastMemoryCheck = N;
      }
    }

    if (CheckMemory)
    {
      BYTE * Address = NULL;
      MEMORY_BASIC_INFORMATION MemoryInfo;
      size_t ReservedMemory = 0;
      size_t CommittedMemory = 0;
      while (VirtualQuery(Address, &MemoryInfo, sizeof(MemoryInfo)) == sizeof(MemoryInfo))
      {
        if ((MemoryInfo.State == MEM_RESERVE) || (MemoryInfo.State == MEM_COMMIT))
        {
          ReservedMemory += MemoryInfo.RegionSize;
        }
        if ((MemoryInfo.State == MEM_COMMIT) && (MemoryInfo.Type == MEM_PRIVATE))
        {
          CommittedMemory += MemoryInfo.RegionSize;
        }

        Address += MemoryInfo.RegionSize;
      }

      bool NewMemoryPeek;
      {
        TGuard Guard(FCriticalSection.get());
        const size_t Threshold = 10 * 1024 * 1024;
        NewMemoryPeek =
          ((ReservedMemory > FPeekReservedMemory) &&
           ((ReservedMemory - FPeekReservedMemory) > Threshold)) |
          ((CommittedMemory > FPeekCommittedMemory) &&
           ((CommittedMemory - FPeekCommittedMemory) > Threshold));
        if (NewMemoryPeek)
        {
          FPeekReservedMemory = ReservedMemory;
          FPeekCommittedMemory = CommittedMemory;
        }
      }

      if (NewMemoryPeek)
      {
        Log(FORMAT(L"Memory increased: Reserved address space: %s, Committed private: %s",
              (FormatNumber(__int64(ReservedMemory)), FormatNumber(__int64(CommittedMemory)))));
      }
    }
  }
}
