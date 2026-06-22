//---------------------------------------------------------------------------
#ifndef CryptographyH
#define CryptographyH
//---------------------------------------------------------------------------
void __fastcall CryptographyInitialize();
void __fastcall CryptographyFinalize();
void RequireTls();
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
RawByteString GenerateEncryptKey();
void ValidateEncryptKey(const RawByteString & Key);
//---------------------------------------------------------------------------
class TFileBuffer;
typedef void AESContext;
//---------------------------------------------------------------------------
class TEncryption
{
public:
  TEncryption(const RawByteString & Key);
  ~TEncryption() EXCEPT;

  static bool IsEncryptedFileName(const UnicodeString & FileName);

  void Encrypt(TFileBuffer & Buffer, bool Last);
  void Decrypt(TFileBuffer & Buffer);
  bool DecryptEnd(TFileBuffer & Buffer);
  UnicodeString EncryptFileName(const UnicodeString & FileName);
  UnicodeString DecryptFileName(const UnicodeString & FileName);

  static int GetOverhead();
  static int RoundToBlock(int Size);
  static int RoundToBlockDown(int Size);

private:
  RawByteString FKey;
  RawByteString FSalt;
  RawByteString FInputHeader;
  RawByteString FOverflowBuffer;
  bool FOutputtedHeader;
  AESContext * FContext;

  void Init(const RawByteString & Key, const RawByteString & Salt);
  void Aes(char * Buffer, int Size);
  void Aes(RawByteString & Buffer);
  void Aes(TFileBuffer & Buffer, bool Last);
  void NeedSalt();
  void SetSalt();
};
//---------------------------------------------------------------------------
#endif
