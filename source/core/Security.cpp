//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <limits>
#include "Common.h"
#include "Security.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define PWALG_SIMPLE_INTERNAL 0x00
#define PWALG_SIMPLE_EXTERNAL 0x01
#define PWALG_SIMPLE_INTERNAL2 0x02
static RawByteString PWALG_SIMPLE_STRING("0123456789ABCDEF");
//---------------------------------------------------------------------------
RawByteString SimpleEncryptChar(unsigned char Ch)
{
  Ch = (unsigned char)((~Ch) ^ PWALG_SIMPLE_MAGIC);
  RawByteString Result("..");
  Result[1] = PWALG_SIMPLE_STRING[((Ch & 0xF0) >> 4) + 1];
  Result[2] = PWALG_SIMPLE_STRING[((Ch & 0x0F) >> 0) + 1];
  return Result;
}
//---------------------------------------------------------------------------
unsigned char SimpleDecryptNextChar(RawByteString &Str)
{
  if (Str.Length() > 0)
  {
    unsigned char Result = (unsigned char)
      ~((((PWALG_SIMPLE_STRING.Pos(Str.c_str()[0])-1) << 4) +
         ((PWALG_SIMPLE_STRING.Pos(Str.c_str()[1])-1) << 0)) ^ PWALG_SIMPLE_MAGIC);
    Str.Delete(1, 2);
    return Result;
  }
  else return 0x00;
}
//---------------------------------------------------------------------------
RawByteString EncryptPassword(UnicodeString UnicodePassword, UnicodeString UnicodeKey, Integer /* Algorithm */)
{
  UTF8String Password = UnicodePassword;
  UTF8String Key = UnicodeKey;

  RawByteString Result;

  if (!RandSeed) Randomize();
  Password = Key + Password;
  Result += SimpleEncryptChar((unsigned char)PWALG_SIMPLE_FLAG); // Flag
  int Len = Password.Length();
  if (Len > std::numeric_limits<unsigned char>::max())
  {
    Result += SimpleEncryptChar((unsigned char)PWALG_SIMPLE_INTERNAL2);
    Result += SimpleEncryptChar((unsigned char)(Len >> 8));
    Result += SimpleEncryptChar((unsigned char)(Len & 0xFF));
  }
  else
  {
    Result += SimpleEncryptChar((unsigned char)PWALG_SIMPLE_INTERNAL);
    Result += SimpleEncryptChar((unsigned char)Len);
  }
  int DataLen =
    (Result.Length() / 2) +
    1 + // Shift
    Password.Length();
  int Shift = (DataLen < PWALG_SIMPLE_MAXLEN) ? random(PWALG_SIMPLE_MAXLEN - DataLen) : 0;
  Result += SimpleEncryptChar((unsigned char)Shift);
  for (int Index = 0; Index < Shift; Index++)
    Result += SimpleEncryptChar((unsigned char)random(256));
  for (int Index = 0; Index < Password.Length(); Index++)
    Result += SimpleEncryptChar(Password.c_str()[Index]);
  while (Result.Length() < PWALG_SIMPLE_MAXLEN * 2)
    Result += SimpleEncryptChar((unsigned char)random(256));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString DecryptPassword(RawByteString Password, UnicodeString UnicodeKey, Integer /* Algorithm */)
{
  int Length;
  unsigned char Flag = SimpleDecryptNextChar(Password);
  if (Flag == PWALG_SIMPLE_FLAG)
  {
    unsigned char Version = SimpleDecryptNextChar(Password);
    if (Version == PWALG_SIMPLE_INTERNAL)
    {
      Length = SimpleDecryptNextChar(Password);
    }
    else if (Version == PWALG_SIMPLE_INTERNAL2)
    {
      Length = (int(SimpleDecryptNextChar(Password)) << 8) + SimpleDecryptNextChar(Password);
    }
    else
    {
      Length = -1;
    }
  }
  else
  {
    Length = Flag;
  }

  UTF8String Result;
  if (Length >= 0)
  {
    Password.Delete(1, ((Integer)SimpleDecryptNextChar(Password))*2);
    for (int Index = 0; Index < Length; Index++)
    {
      Result += (char)SimpleDecryptNextChar(Password);
    }
    if (Flag == PWALG_SIMPLE_FLAG)
    {
      UTF8String Key = UnicodeKey;
      if (Result.SubString(1, Key.Length()) != Key)
      {
        Result = UTF8String();
      }
      else
      {
        Result.Delete(1, Key.Length());
      }
    }
  }
  return UnicodeString(Result);
}
//---------------------------------------------------------------------------
RawByteString SetExternalEncryptedPassword(RawByteString Password)
{
  RawByteString Result;
  Result += SimpleEncryptChar((unsigned char)PWALG_SIMPLE_FLAG);
  Result += SimpleEncryptChar((unsigned char)PWALG_SIMPLE_EXTERNAL);
  Result += UTF8String(BytesToHex(reinterpret_cast<const unsigned char *>(Password.c_str()), Password.Length()));
  return Result;
}
//---------------------------------------------------------------------------
bool GetExternalEncryptedPassword(RawByteString Encrypted, RawByteString & Password)
{
  bool Result =
    (SimpleDecryptNextChar(Encrypted) == PWALG_SIMPLE_FLAG) &&
    (SimpleDecryptNextChar(Encrypted) == PWALG_SIMPLE_EXTERNAL);
  if (Result)
  {
    Password = HexToBytes(UTF8ToString(Encrypted));
  }
  return Result;
}
//---------------------------------------------------------------------------
bool WindowsValidateCertificate(const unsigned char * Certificate, size_t Len, UnicodeString & Error)
{
  bool Result = false;

  // Parse the certificate into a context.
  const CERT_CONTEXT * CertContext =
    CertCreateCertificateContext(
      X509_ASN_ENCODING | PKCS_7_ASN_ENCODING, Certificate, Len);

  if (CertContext == NULL)
  {
    Error = L"Cannot create certificate context";
  }
  else
  {
    CERT_CHAIN_PARA ChainPara;
    // Retrieve the certificate chain of the certificate
    // (a certificate without a valid root does not have a chain).
    memset(&ChainPara, 0, sizeof(ChainPara));
    ChainPara.cbSize = sizeof(ChainPara);

    CERT_CHAIN_ENGINE_CONFIG ChainConfig;

    memset(&ChainConfig, 0, sizeof(ChainConfig));
    const size_t ChainConfigSize =
      reinterpret_cast<const char *>(&ChainConfig.CycleDetectionModulus) + sizeof(ChainConfig.CycleDetectionModulus) -
      reinterpret_cast<const char *>(&ChainConfig);
    // The hExclusiveRoot and hExclusiveTrustedPeople were added in Windows 7.
    // The CertGetCertificateChain fails with E_INVALIDARG when we include them to ChainConfig.cbSize.
    // The dwExclusiveFlags was added in Windows 8
    DebugAssert(ChainConfigSize == 40);
    DebugAssert(ChainConfigSize == sizeof(CERT_CHAIN_ENGINE_CONFIG) - sizeof(ChainConfig.hExclusiveRoot) - sizeof(ChainConfig.hExclusiveTrustedPeople) - sizeof(ChainConfig.dwExclusiveFlags));
    ChainConfig.cbSize = ChainConfigSize;
    ChainConfig.hRestrictedRoot = NULL;
    ChainConfig.hRestrictedTrust = NULL;
    ChainConfig.hRestrictedOther = NULL;
    ChainConfig.cAdditionalStore = 0;
    ChainConfig.rghAdditionalStore = NULL;
    ChainConfig.dwFlags = CERT_CHAIN_CACHE_END_CERT;
    ChainConfig.dwUrlRetrievalTimeout = 0;
    ChainConfig.MaximumCachedCertificates =0;
    ChainConfig.CycleDetectionModulus = 0;

    HCERTCHAINENGINE ChainEngine;
    bool ChainEngineResult = CertCreateCertificateChainEngine(&ChainConfig, &ChainEngine);
    if (!ChainEngineResult)
    {
      Error = L"Cannot create certificate chain engine";
    }
    else
    {
      const CERT_CHAIN_CONTEXT * ChainContext = NULL;
      if (!CertGetCertificateChain(ChainEngine, CertContext, NULL, NULL, &ChainPara,
            CERT_CHAIN_CACHE_END_CERT |
            CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT,
            NULL, &ChainContext))
      {
        Error = L"Cannot get certificate chain";
      }
      else
      {
        CERT_CHAIN_POLICY_PARA PolicyPara;

        PolicyPara.cbSize = sizeof(PolicyPara);
        PolicyPara.dwFlags = 0;
        PolicyPara.pvExtraPolicyPara = NULL;

        CERT_CHAIN_POLICY_STATUS PolicyStatus;
        PolicyStatus.cbSize = sizeof(PolicyStatus);

        if (!CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL, ChainContext, &PolicyPara, &PolicyStatus))
        {
          Error = L"Cannot verify certificate chain policy";
        }
        else
        {
          int PolicyError = PolicyStatus.dwError;
          // Windows thinks the certificate is valid.
          Result = (PolicyError == S_OK);
          if (!Result)
          {
            UnicodeString ErrorStr = SysErrorMessage(PolicyError);
            Error = FORMAT(L"Error: %x (%s), Chain index: %d, Element index: %d", (PolicyError, ErrorStr, PolicyStatus.lChainIndex, PolicyStatus.lElementIndex));
          }
        }

        CertFreeCertificateChain(ChainContext);
      }
      CertFreeCertificateChainEngine(ChainEngine);
    }
    CertFreeCertificateContext(CertContext);
  }
  return Result;
}
//---------------------------------------------------------------------------
