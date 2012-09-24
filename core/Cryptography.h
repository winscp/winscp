//---------------------------------------------------------------------------
#ifndef CryptographyH
#define CryptographyH
//---------------------------------------------------------------------------
void __fastcall CryptographyInitialize();
void __fastcall CryptographyFinalize();
RawByteString __fastcall ScramblePassword(UnicodeString Password);
bool __fastcall UnscramblePassword(RawByteString Scrambled, UnicodeString & Password);
void __fastcall AES256EncyptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString & Output);
bool __fastcall AES256DecryptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString & Output);
void __fastcall AES256CreateVerifier(UnicodeString Input, RawByteString & Verifier);
bool __fastcall AES256Verify(UnicodeString Input, RawByteString Verifier);
int __fastcall IsValidPassword(UnicodeString Password);
int __fastcall PasswordMaxLength();
//---------------------------------------------------------------------------
#endif
