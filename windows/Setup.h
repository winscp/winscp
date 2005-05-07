//---------------------------------------------------------------------------
#ifndef SetupH
#define SetupH
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const AnsiString Path);
void __fastcall RemoveSearchPath(const AnsiString Path);
void __fastcall CheckForUpdates(bool CachedResults);
void __fastcall RegisterAsUrlHandler();
void __fastcall TemporaryDirectoryCleanup();
int __fastcall CalculateCompoundVersion(int MajorVer,
  int MinorVer, int Release, int Build);
void __fastcall StartUpdateThread();
void __fastcall StopUpdateThread();
//---------------------------------------------------------------------------
#endif
