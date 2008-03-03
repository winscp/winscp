//---------------------------------------------------------------------------
#ifndef CopyParamH
#define CopyParamH

#include "FileMasks.h"
#include "RemoteFiles.h"
//---------------------------------------------------------------------------
// When adding new options, mind TCopyParamType::GetLogStr()
enum TOperationSide { osLocal, osRemote, osCurrent };
enum TFileNameCase { ncNoChange, ncUpperCase, ncLowerCase, ncFirstUpperCase, ncLowerCaseShort };
// TScript::OptionProc depend on the order
enum TTransferMode { tmBinary, tmAscii, tmAutomatic };
enum TResumeSupport { rsOn, rsSmart, rsOff };
class THierarchicalStorage;
const int cpaExcludeMaskOnly = 0x01;
const int cpaNoTransferMode =  0x02;
const int cpaNoExcludeMask =   0x04;
const int cpaNoClearArchive =  0x08;
const int cpaNoPreserveTime =  0x10;
const int cpaNoRights =        0x20;
const int cpaNoPreserveReadOnly = 0x40;
const int cpaNoIgnorePermErrors = 0x80;
//---------------------------------------------------------------------------
struct TUsableCopyParamAttrs
{
  int General;
  int Upload;
  int Download;
};
//---------------------------------------------------------------------------
class TCopyParamType {
private:
  TFileMasks FAsciiFileMask;
  TFileNameCase FFileNameCase;
  bool FPreserveReadOnly;
  bool FPreserveTime;
  TRights FRights;
  TTransferMode FTransferMode;
  bool FAddXToDirectories;
  bool FPreserveRights;
  bool FIgnorePermErrors;
  TResumeSupport FResumeSupport;
  __int64 FResumeThreshold;
  AnsiString __fastcall GetLogStr() const;
  char FInvalidCharsReplacement;
  AnsiString FLocalInvalidChars;
  AnsiString FTokenizibleChars;
  bool FCalculateSize;
  AnsiString FFileMask;
  TFileMasks FExcludeFileMask;
  bool FNegativeExclude;
  bool FClearArchive;
  static const char TokenPrefix = '%';
  static const char NoReplacement = char(false);
  static const char TokenReplacement = char(true);

  static AnsiString __fastcall Untokenize(AnsiString FileName);
  void __fastcall SetLocalInvalidChars(AnsiString value);
  bool __fastcall GetReplaceInvalidChars() const;
  void __fastcall SetReplaceInvalidChars(bool value);
  char * __fastcall ReplaceChar(AnsiString & FileName, char * InvalidChar) const;

public:
  __fastcall TCopyParamType();
  __fastcall TCopyParamType(const TCopyParamType & Source);
  virtual __fastcall ~TCopyParamType();
  TCopyParamType & __fastcall operator =(const TCopyParamType & rhp);
  virtual void __fastcall Assign(const TCopyParamType * Source);
  virtual void __fastcall Default();
  AnsiString __fastcall ChangeFileName(AnsiString FileName,
    TOperationSide Side, bool FirstLevel) const;
  int __fastcall LocalFileAttrs(const TRights & Rights) const;
  TRights __fastcall RemoteFileRights(int Attrs) const;
  bool __fastcall UseAsciiTransfer(AnsiString FileName, TOperationSide Side,
    const TFileMasks::TParams & Params) const;
  bool __fastcall AllowResume(__int64 Size) const;
  AnsiString __fastcall ValidLocalFileName(AnsiString FileName) const;
  bool __fastcall AllowTransfer(AnsiString FileName, TOperationSide Side,
    bool Directory, const TFileMasks::TParams & Params) const;

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;
  AnsiString __fastcall GetInfoStr(AnsiString Separator, int Attrs) const;

  bool __fastcall operator==(const TCopyParamType & rhp) const;

  __property TFileMasks AsciiFileMask = { read = FAsciiFileMask, write = FAsciiFileMask };
  __property TFileNameCase FileNameCase = { read = FFileNameCase, write = FFileNameCase };
  __property bool PreserveReadOnly = { read = FPreserveReadOnly, write = FPreserveReadOnly };
  __property bool PreserveTime = { read = FPreserveTime, write = FPreserveTime };
  __property TRights Rights = { read = FRights, write = FRights };
  __property TTransferMode TransferMode = { read = FTransferMode, write = FTransferMode };
  __property AnsiString LogStr  = { read=GetLogStr };
  __property bool AddXToDirectories  = { read=FAddXToDirectories, write=FAddXToDirectories };
  __property bool PreserveRights = { read = FPreserveRights, write = FPreserveRights };
  __property bool IgnorePermErrors = { read = FIgnorePermErrors, write = FIgnorePermErrors };
  __property TResumeSupport ResumeSupport = { read = FResumeSupport, write = FResumeSupport };
  __property __int64 ResumeThreshold = { read = FResumeThreshold, write = FResumeThreshold };
  __property char InvalidCharsReplacement = { read = FInvalidCharsReplacement, write = FInvalidCharsReplacement };
  __property bool ReplaceInvalidChars = { read = GetReplaceInvalidChars, write = SetReplaceInvalidChars };
  __property AnsiString LocalInvalidChars = { read = FLocalInvalidChars, write = SetLocalInvalidChars };
  __property bool CalculateSize = { read = FCalculateSize, write = FCalculateSize };
  __property AnsiString FileMask = { read = FFileMask, write = FFileMask };
  __property TFileMasks ExcludeFileMask = { read = FExcludeFileMask, write = FExcludeFileMask };
  __property bool NegativeExclude = { read = FNegativeExclude, write = FNegativeExclude };
  __property bool ClearArchive = { read = FClearArchive, write = FClearArchive };
};
//---------------------------------------------------------------------------
#endif
