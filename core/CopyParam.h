//---------------------------------------------------------------------------
#ifndef CopyParamH
#define CopyParamH

#include "FileMasks.h"
#include "RemoteFiles.h"
//---------------------------------------------------------------------------
// When adding new options, mind TCopyParamType::GetLogStr()
enum TFileNameCase { ncNoChange, ncUpperCase, ncLowerCase, ncFirstUpperCase };
enum TTransferMode { tmBinary, tmAscii, tmAutomatic };
enum TResumeSupport { rsOn, rsSmart, rsOff };
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
  TResumeSupport FResumeSupport;
  __int64 FResumeThreshold;
  AnsiString __fastcall GetLogStr() const;

public:
  __fastcall TCopyParamType();
  __fastcall TCopyParamType(const TCopyParamType & Source);
  TCopyParamType & __fastcall operator =(const TCopyParamType & rhp);
  void __fastcall Assign(const TCopyParamType & Source);
  void __fastcall Default();
  AnsiString __fastcall ChangeFileNameCase(AnsiString FileName) const;
  int __fastcall LocalFileAttrs(const TRights & Rights) const;
  TRights __fastcall RemoteFileRights(int Attrs) const;
  bool __fastcall UseAsciiTransfer(const AnsiString FileName) const;
  bool __fastcall AllowResume(__int64 Size) const;

  __property TFileMasks AsciiFileMask = { read = FAsciiFileMask, write = FAsciiFileMask };
  __property TFileNameCase FileNameCase = { read = FFileNameCase, write = FFileNameCase };
  __property bool PreserveReadOnly = { read = FPreserveReadOnly, write = FPreserveReadOnly };
  __property bool PreserveTime = { read = FPreserveTime, write = FPreserveTime };
  __property TRights Rights = { read = FRights, write = FRights };
  __property TTransferMode TransferMode = { read = FTransferMode, write = FTransferMode };
  __property AnsiString LogStr  = { read=GetLogStr };
  __property bool AddXToDirectories  = { read=FAddXToDirectories, write=FAddXToDirectories };
  __property bool PreserveRights = { read = FPreserveRights, write = FPreserveRights };
  __property TResumeSupport ResumeSupport = { read = FResumeSupport, write = FResumeSupport };
  __property __int64 ResumeThreshold = { read = FResumeThreshold, write = FResumeThreshold };
};
//---------------------------------------------------------------------------
#endif
