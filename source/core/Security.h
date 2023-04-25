//---------------------------------------------------------------------------
#ifndef SecurityH
#define SecurityH
//---------------------------------------------------------------------------
#define PWALG_SIMPLE 1
#define PWALG_SIMPLE_MAGIC 0xA3
#define PWALG_SIMPLE_MAXLEN 50
#define PWALG_SIMPLE_FLAG 0xFF
RawByteString EncryptPassword(UnicodeString Password, UnicodeString Key, Integer Algorithm = PWALG_SIMPLE);
UnicodeString DecryptPassword(RawByteString Password, UnicodeString Key, Integer Algorithm = PWALG_SIMPLE);
RawByteString SetExternalEncryptedPassword(RawByteString Password);
bool GetExternalEncryptedPassword(RawByteString Encrypted, RawByteString & Password);
//---------------------------------------------------------------------------
bool WindowsValidateCertificate(const unsigned char * Certificate, size_t Len, UnicodeString & Error);
//---------------------------------------------------------------------------
#endif
