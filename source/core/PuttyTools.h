//---------------------------------------------------------------------------
#ifndef PuttyToolsH
#define PuttyToolsH
//---------------------------------------------------------------------------
#include <SessionData.h>
//---------------------------------------------------------------------------
enum TKeyType
{
  ktUnopenable, ktUnknown,
  ktSSH1, ktSSH2,
  ktOpenSSHAuto, ktOpenSSHPEM, ktOpenSSHNew, ktSSHCom,
  ktSSH1Public, ktSSH2PublicRFC4716, ktSSH2PublicOpenSSH
};
TKeyType KeyType(UnicodeString FileName);
bool IsKeyEncrypted(TKeyType KeyType, const UnicodeString & FileName, UnicodeString & Comment);
struct TPrivateKey;
TPrivateKey * LoadKey(TKeyType KeyType, const UnicodeString & FileName, const UnicodeString & Passphrase);
UnicodeString TestKey(TKeyType KeyType, const UnicodeString & FileName);
void ChangeKeyComment(TPrivateKey * PrivateKey, const UnicodeString & Comment);
void AddCertificateToKey(TPrivateKey * PrivateKey, const UnicodeString & CertificateFileName);
void SaveKey(TKeyType KeyType, const UnicodeString & FileName,
  const UnicodeString & Passphrase, TPrivateKey * PrivateKey);
void FreeKey(TPrivateKey * PrivateKey);
RawByteString LoadPublicKey(
  const UnicodeString & FileName, UnicodeString & Algorithm, UnicodeString & Comment, bool & HasCertificate);
UnicodeString GetPublicKeyLine(const UnicodeString & FileName, UnicodeString & Comment, bool & HasCertificate);
extern const UnicodeString PuttyKeyExt;
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI(UnicodeString CustomPath);
//---------------------------------------------------------------------------
void __fastcall AES256EncodeWithMAC(char * Data, size_t Len, const char * Password,
  size_t PasswordLen, const char * Salt);
//---------------------------------------------------------------------------
void __fastcall NormalizeFingerprint(UnicodeString & Fingerprint, UnicodeString & KeyName);
UnicodeString __fastcall KeyTypeFromFingerprint(UnicodeString Fingerprint);
//---------------------------------------------------------------------------
UnicodeString __fastcall GetPuTTYVersion();
//---------------------------------------------------------------------------
UnicodeString __fastcall Sha256(const char * Data, size_t Size);
UnicodeString CalculateFileChecksum(TStream * Stream, const UnicodeString & Alg);
//---------------------------------------------------------------------------
UnicodeString __fastcall ParseOpenSshPubLine(const UnicodeString & Line, const struct ssh_keyalg *& Algorithm);
void ParseCertificatePublicKey(const UnicodeString & Str, RawByteString & PublicKey, UnicodeString & Fingerprint);
bool IsCertificateValidityExpressionValid(
  const UnicodeString & Str, UnicodeString & Error, int & ErrorStart, int & ErrorLen);
//---------------------------------------------------------------------------
bool IsOpenSSH(const UnicodeString & SshImplementation);
//---------------------------------------------------------------------------
TStrings * SshCipherList();
TStrings * SshKexList();
int HostKeyToPutty(THostKey HostKey);
TStrings * SshHostKeyList();
TStrings * SshMacList();
//---------------------------------------------------------------------------
class TSessionData;
void SaveAsPutty(const UnicodeString & Name, TSessionData * Data);
class THierarchicalStorage;
void WritePuttySettings(THierarchicalStorage * Storage, const UnicodeString & Settings);
void SavePuttyDefaults(const UnicodeString & Name);
//---------------------------------------------------------------------------
bool RandomSeedExists();
//---------------------------------------------------------------------------
#endif
