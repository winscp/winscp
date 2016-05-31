//---------------------------------------------------------------------------
#ifndef NeonIntfH
#define NeonIntfH
//---------------------------------------------------------------------------
#include <ne_uri.h>
#include <ne_session.h>
#include <SessionData.h>
//---------------------------------------------------------------------------
#define StrToNeon(S) UTF8String(S).c_str()
#define StrFromNeon(S) UnicodeString(UTF8String(S))
//---------------------------------------------------------------------------
void NeonParseUrl(const UnicodeString & Url, ne_uri & uri);
bool IsTlsUri(const ne_uri & uri);
ne_session * CreateNeonSession(const ne_uri & uri, TProxyMethod ProxyMethod, const UnicodeString & ProxyHost,
  int ProxyPort, const UnicodeString & ProxyUsername, const UnicodeString & ProxyPassword);
void DestroyNeonSession(ne_session * Session);
UnicodeString GetNeonError(ne_session * Session);
void CheckNeonStatus(ne_session * Session, int NeonStatus,
  const UnicodeString & HostName, const UnicodeString & CustomError = L"");
UnicodeString GetNeonRedirectUrl(ne_session * Session);
void CheckRedirectLoop(const UnicodeString & RedirectUrl, TStrings * AttemptedUrls);
typedef void (*TNeonTlsInit)(struct ssl_st * Ssl, ne_session * Session);
void SetNeonTlsInit(ne_session * Session, TNeonTlsInit OnNeonTlsInit);
AnsiString NeonExportCertificate(const ne_ssl_certificate * Certificate);
bool NeonWindowsValidateCertificate(int & Failures, const AnsiString & AsciiCert, UnicodeString & Error);
UnicodeString NeonCertificateFailuresErrorStr(int Failures, const UnicodeString & HostName);
//---------------------------------------------------------------------------
#endif
