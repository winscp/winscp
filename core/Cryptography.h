//---------------------------------------------------------------------------
#ifndef CryptographyH
#define CryptographyH
//---------------------------------------------------------------------------
void __fastcall CryptographyInitialize();
void __fastcall CryptographyFinalize();
void __fastcall ScramblePassword(AnsiString & Password);
bool __fastcall UnscramblePassword(AnsiString & Password);
void __fastcall AES256EncyptWithMAC(AnsiString Input, AnsiString Password,
  AnsiString & Salt, AnsiString & Output, AnsiString & Mac);
void __fastcall AES256EncyptWithMAC(AnsiString Input, AnsiString Password,
  AnsiString & Output);
bool __fastcall AES256DecryptWithMAC(AnsiString Input, AnsiString Password,
  AnsiString Salt, AnsiString & Output, AnsiString Mac);
bool __fastcall AES256DecryptWithMAC(AnsiString Input, AnsiString Password,
  AnsiString & Output);
void __fastcall AES256CreateVerifier(AnsiString Input, AnsiString & Verifier);
bool __fastcall AES256Verify(AnsiString Input, AnsiString Verifier);
int __fastcall IsValidPassword(AnsiString Password);
int __fastcall PasswordMaxLength();
//---------------------------------------------------------------------------
#endif
