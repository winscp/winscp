//---------------------------------------------------------------------------
#ifndef SetupH
#define SetupH
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const AnsiString Path);
void __fastcall RemoveSearchPath(const AnsiString Path);
void __fastcall CheckForUpdates();
void __fastcall RegisterAsUrlHandler();
void __fastcall TemporaryDirectoryCleanup();
//---------------------------------------------------------------------------
#endif
