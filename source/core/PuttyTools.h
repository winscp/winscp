//---------------------------------------------------------------------------
#ifndef PuttyToolsH
#define PuttyToolsH
//---------------------------------------------------------------------------
enum TKeyType { ktUnopenable, ktUnknown, ktSSH1, ktSSH2, ktOpenSSH, ktSSHCom };
TKeyType KeyType(UnicodeString FileName);
UnicodeString KeyTypeName(TKeyType KeyType);
bool IsKeyEncrypted(TKeyType KeyType, const UnicodeString & FileName, UnicodeString & Comment);
struct TPrivateKey;
TPrivateKey * LoadKey(TKeyType KeyType, const UnicodeString & FileName, const UnicodeString & Passphrase);
void ChangeKeyComment(TPrivateKey * PrivateKey, const UnicodeString & Comment);
void SaveKey(TKeyType KeyType, const UnicodeString & FileName,
  const UnicodeString & Passphrase, TPrivateKey * PrivateKey);
void FreeKey(TPrivateKey * PrivateKey);
//---------------------------------------------------------------------------
__int64 __fastcall ParseSize(UnicodeString SizeStr);
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI(UnicodeString CustomPath);
//---------------------------------------------------------------------------
void __fastcall AES256EncodeWithMAC(char * Data, size_t Len, const char * Password,
  size_t PasswordLen, const char * Salt);
//---------------------------------------------------------------------------
UnicodeString __fastcall NormalizeFingerprint(UnicodeString Fingerprint);
UnicodeString __fastcall KeyTypeFromFingerprint(UnicodeString Fingerprint);
//---------------------------------------------------------------------------
UnicodeString __fastcall GetPuTTYVersion();
//---------------------------------------------------------------------------
UnicodeString __fastcall Sha256(const char * Data, size_t Size);
//---------------------------------------------------------------------------
#endif
