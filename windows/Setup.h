//---------------------------------------------------------------------------
#ifndef SetupH
#define SetupH
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const AnsiString Path);
void __fastcall RemoveSearchPath(const AnsiString Path);
void __fastcall GetUpdatesMessage(AnsiString & Message, bool & New);
void __fastcall CheckForUpdates(bool CachedResults);
void __fastcall RegisterAsUrlHandler();
void __fastcall TemporaryDirectoryCleanup();
int __fastcall CalculateCompoundVersion(int MajorVer,
  int MinorVer, int Release, int Build);
int __fastcall CurrentCompoundVersion();
void __fastcall StartUpdateThread(TThreadMethod OnUpdatesChecked);
void __fastcall StopUpdateThread();
//---------------------------------------------------------------------------
#endif
