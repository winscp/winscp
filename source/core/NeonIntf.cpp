//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "NeonIntf.h"
#include "Interface.h"
#include "CoreMain.h"
#include "Exceptions.h"
#include "Security.h"
#include "Terminal.h"
#include <TextsCore.h>
#include <ne_auth.h>
#define WINSCP
extern "C"
{
#include <ne_redirect.h>
}
#include <StrUtils.hpp>
//---------------------------------------------------------------------------
#define SESSION_PROXY_AUTH_KEY "proxyauth"
#define SESSION_TLS_INIT_KEY "tlsinit"
#define SESSION_TERMINAL_KEY "terminal"
//---------------------------------------------------------------------------
void NeonParseUrl(const UnicodeString & Url, ne_uri & uri)
{
  if (ne_uri_parse(StrToNeon(Url), &uri) != 0)
  {
    // should never happen
    throw Exception(FMTLOAD(INVALID_URL, (Url)));
  }

  // Will never happen for initial URL, but may happen for redirect URLs
  if (uri.port == 0)
  {
    uri.port = ne_uri_defaultport(uri.scheme);
  }
}
//---------------------------------------------------------------------------
bool IsTlsUri(const ne_uri & uri)
{
  return SameText(StrFromNeon(uri.scheme), HttpsProtocol);
}
//---------------------------------------------------------------------------
struct TProxyAuthData
{
  UnicodeString UserName;
  UnicodeString Password;
};
//------------------------------------------------------------------------------
static int NeonProxyAuth(
  void * UserData, const char * /*Realm*/, int Attempt, char * UserName, char * Password)
{
  TProxyAuthData * ProxyAuthData = static_cast<TProxyAuthData *>(UserData);

  int Result;
  // no point trying too many times as we always return the same credentials
  // (maybe just one would be enough)
  if (Attempt >= 2)
  {
    Result = 1;
  }
  else
  {
    strncpy(UserName, StrToNeon(ProxyAuthData->UserName), NE_ABUFSIZ);
    strncpy(Password, StrToNeon(ProxyAuthData->Password), NE_ABUFSIZ);
    Result = 0;
  }

  return Result;
}
//---------------------------------------------------------------------------
ne_session * CreateNeonSession(const ne_uri & uri)
{
  return ne_session_create(uri.scheme, uri.host, uri.port);
}
//---------------------------------------------------------------------------
void InitNeonSession(ne_session * Session, TProxyMethod ProxyMethod, const UnicodeString & ProxyHost,
  int ProxyPort, const UnicodeString & ProxyUsername, const UnicodeString & ProxyPassword, TTerminal * Terminal)
{
  if (ProxyMethod != ::pmNone)
  {
    if ((ProxyMethod == pmSocks4) || (ProxyMethod == pmSocks5))
    {
      enum ne_sock_sversion vers = (ProxyMethod == pmSocks4) ? NE_SOCK_SOCKSV4A : NE_SOCK_SOCKSV5;
      ne_session_socks_proxy(Session, vers, StrToNeon(ProxyHost), ProxyPort, StrToNeon(ProxyUsername), StrToNeon(ProxyPassword));
    }
    else if (!ProxyHost.IsEmpty())
    {
      ne_session_proxy(Session, StrToNeon(ProxyHost), ProxyPort);

      if (!ProxyUsername.IsEmpty())
      {
        TProxyAuthData * ProxyAuthData = new TProxyAuthData();
        ProxyAuthData->UserName = ProxyUsername;
        ProxyAuthData->Password = ProxyPassword;
        ne_set_session_private(Session, SESSION_PROXY_AUTH_KEY, ProxyAuthData);
        ne_set_proxy_auth(Session, NeonProxyAuth, ProxyAuthData);
      }
      else
      {
        // Enable (only) the Negotiate scheme for proxy
        // authentication, if no username/password is
        // configured.
        ne_add_proxy_auth(Session, NE_AUTH_NEGOTIATE, NULL, NULL);
      }
    }
  }

  ne_redirect_register(Session);
  ne_set_useragent(Session, StrToNeon(FORMAT(L"%s/%s", (AppNameString(), Configuration->Version))));

  if (Terminal != NULL)
  {
    ne_set_session_private(Session, SESSION_TERMINAL_KEY, Terminal);
  }
}
//---------------------------------------------------------------------------
void DestroyNeonSession(ne_session * Session)
{
  TProxyAuthData * ProxyAuthData =
    static_cast<TProxyAuthData *>(ne_get_session_private(Session, SESSION_PROXY_AUTH_KEY));
  if (ProxyAuthData != NULL)
  {
    delete ProxyAuthData;
  }
  ne_session_destroy(Session);
}
//---------------------------------------------------------------------------
UnicodeString GetNeonError(ne_session * Session)
{
  return StrFromNeon(ne_get_error(Session));
}
//---------------------------------------------------------------------------
void CheckNeonStatus(ne_session * Session, int NeonStatus,
  const UnicodeString & HostName, const UnicodeString & CustomError)
{
  if (NeonStatus == NE_OK)
  {
    // noop
  }
  else
  {
    UnicodeString NeonError = GetNeonError(Session);
    UnicodeString Error;
    if (!CustomError.IsEmpty())
    {
      Error = CustomError;
    }
    else
    {
      switch (NeonStatus)
      {
        case NE_ERROR:
          // noop
          DebugAssert(!NeonError.IsEmpty());
          Error = NeonError;
          NeonError = L"";
          break;

        case NE_LOOKUP:
          Error = ReplaceStr(LoadStr(NET_TRANSL_HOST_NOT_EXIST2), L"%HOST%", HostName);
          break;

        case NE_AUTH:
          Error = LoadStr(AUTHENTICATION_FAILED);
          break;

        case NE_PROXYAUTH:
          Error = LoadStr(PROXY_AUTHENTICATION_FAILED);
          break;

        case NE_CONNECT:
          Error = LoadStr(CONNECTION_FAILED);
          break;

        case NE_TIMEOUT:
          Error = ReplaceStr(LoadStr(NET_TRANSL_TIMEOUT2), L"%HOST%", HostName);
          break;

        case NE_REDIRECT:
          {
            char * Uri = ne_uri_unparse(ne_redirect_location(Session));
            Error = FMTLOAD(REQUEST_REDIRECTED, (Uri));
            ne_free(Uri);
          }
          break;

        case NE_FAILED: // never used by neon as of 0.30.0
        case NE_RETRY: // not sure if this is a public API
        default:
          DebugFail();
          Error = FORMAT(L"Unexpected neon error %d", (NeonStatus));
          break;
      }
    }

    throw ExtException(Error, NeonError);
  }
}
//---------------------------------------------------------------------------
UnicodeString GetNeonRedirectUrl(ne_session * Session)
{
  const ne_uri * RedirectUri = ne_redirect_location(Session);
  char * RedirectUriStr = ne_uri_unparse(RedirectUri);
  UnicodeString Result = StrFromNeon(RedirectUriStr);
  ne_free(RedirectUriStr);
  return Result;
}
//---------------------------------------------------------------------------
#define MAX_REDIRECT_ATTEMPTS 5
//---------------------------------------------------------------------------
void CheckRedirectLoop(const UnicodeString & RedirectUrl, TStrings * AttemptedUrls)
{
  if (AttemptedUrls->Count > MAX_REDIRECT_ATTEMPTS)
  {
    throw Exception(LoadStr(TOO_MANY_REDIRECTS));
  }
  else
  {
    // Make sure we've not attempted this URL before.
    if (AttemptedUrls->IndexOf(RedirectUrl) >= 0)
    {
      throw Exception(LoadStr(REDIRECT_LOOP));
    }
    AttemptedUrls->Add(RedirectUrl);
  }
}
//---------------------------------------------------------------------------
extern "C"
{

void ne_init_ssl_session(struct ssl_st * Ssl, ne_session * Session)
{
  TNeonTlsInit OnNeonTlsInit =
    reinterpret_cast<TNeonTlsInit>(ne_get_session_private(Session, SESSION_TLS_INIT_KEY));
  if (DebugAlwaysTrue(OnNeonTlsInit != NULL))
  {
    OnNeonTlsInit(Ssl, Session);
  }
}

} // extern "C"
//---------------------------------------------------------------------------
void SetNeonTlsInit(ne_session * Session, TNeonTlsInit OnNeonTlsInit)
{
  ne_set_session_private(Session, SESSION_TLS_INIT_KEY, OnNeonTlsInit);
}
//---------------------------------------------------------------------------
AnsiString NeonExportCertificate(const ne_ssl_certificate * Certificate)
{
  char * AsciiCert = ne_ssl_cert_export(Certificate);
  AnsiString Result = AsciiCert;
  ne_free(AsciiCert);
  return Result;
}
//---------------------------------------------------------------------------
bool NeonWindowsValidateCertificate(int & Failures, const AnsiString & AsciiCert, UnicodeString & Error)
{
  bool Result = false;
  // We can accept only unknown certificate authority.
  if (FLAGSET(Failures, NE_SSL_UNTRUSTED))
  {
    unsigned char * Certificate;
    size_t CertificateLen = ne_unbase64(AsciiCert.c_str(), &Certificate);

    if (CertificateLen > 0)
    {
      if (WindowsValidateCertificate(Certificate, CertificateLen, Error))
      {
        Failures &= ~NE_SSL_UNTRUSTED;
        Result = true;
      }
      ne_free(Certificate);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString NeonCertificateFailuresErrorStr(int Failures, const UnicodeString & HostName)
{
  int FailuresToList = Failures;

  UnicodeString Result;
  if (FLAGSET(FailuresToList, NE_SSL_NOTYETVALID))
  {
    AddToList(Result, LoadStr(CERT_ERR_CERT_NOT_YET_VALID), L" ");
    FailuresToList &= ~NE_SSL_NOTYETVALID;
  }
  if (FLAGSET(FailuresToList, NE_SSL_EXPIRED))
  {
    AddToList(Result, LoadStr(CERT_ERR_CERT_HAS_EXPIRED), L" ");
    FailuresToList &= ~NE_SSL_EXPIRED;
  }
  // NEON checks certificate host name on its own
  if (FLAGSET(FailuresToList, NE_SSL_IDMISMATCH))
  {
    AddToList(Result, FMTLOAD(CERT_NAME_MISMATCH, (HostName)), L" ");
    FailuresToList &= ~NE_SSL_IDMISMATCH;
  }
  if (FLAGSET(FailuresToList, NE_SSL_UNTRUSTED))
  {
    AddToList(Result, LoadStr(CERT_ERR_CERT_UNTRUSTED), L" ");
    FailuresToList &= ~NE_SSL_UNTRUSTED;
  }
  if (FLAGSET(FailuresToList, NE_SSL_BADCHAIN))
  {
    AddToList(Result, LoadStr(CERT_ERR_BAD_CHAIN), L" ");
    FailuresToList &= ~NE_SSL_BADCHAIN;
  }
  // nb, NE_SSL_REVOKED is never used by OpenSSL implementation
  if (FailuresToList != 0)
  {
    AddToList(Result, LoadStr(CERT_ERR_UNKNOWN), L" ");
  }
  return Result;
}
//---------------------------------------------------------------------------
static std::unique_ptr<TCriticalSection> DebugSection(TraceInitPtr(new TCriticalSection));
static std::set<TTerminal *> NeonTerminals;
//---------------------------------------------------------------------------
extern "C"
{

void ne_debug(void * Context, int Channel, const char * Format, ...)
{
  bool DoLog;

  if (FLAGSET(Channel, NE_DBG_SOCKET) ||
      FLAGSET(Channel, NE_DBG_HTTP) ||
      FLAGSET(Channel, NE_DBG_HTTPAUTH) ||
      FLAGSET(Channel, NE_DBG_SSL))
  {
    DoLog = true;
  }
  else if (FLAGSET(Channel, NE_DBG_XML) ||
           FLAGSET(Channel, NE_DBG_WINSCP_HTTP_DETAIL))
  {
    DoLog = (Configuration->ActualLogProtocol >= 1);
  }
  else if (FLAGSET(Channel, NE_DBG_LOCKS) ||
           FLAGSET(Channel, NE_DBG_XMLPARSE) ||
           FLAGSET(Channel, NE_DBG_HTTPBODY))
  {
    DoLog = (Configuration->ActualLogProtocol >= 2);
  }
  else
  {
    DoLog = false;
    DebugFail();
  }

  #ifndef _DEBUG
  if (DoLog)
  #endif
  {
    va_list Args;
    va_start(Args, Format);
    UTF8String UTFMessage;
    UTFMessage.vprintf(Format, Args);
    va_end(Args);

    UnicodeString Message = TrimRight(UTFMessage);

    if (DoLog)
    {
      // Note that this gets called for THttp sessions too.
      // It does no harm atm.
      TTerminal * Terminal = NULL;
      if (Context != NULL)
      {
        ne_session * Session = static_cast<ne_session *>(Context);

        Terminal =
          static_cast<TTerminal *>(ne_get_session_private(Session, SESSION_TERMINAL_KEY));
      }
      else
      {
        TGuard Guard(DebugSection.get());

        if (NeonTerminals.size() == 1)
        {
          Terminal = *NeonTerminals.begin();
        }
      }

      if (Terminal != NULL)
      {
        Terminal->LogEvent(Message);
      }
    }
  }
}

} // extern "C"
//---------------------------------------------------------------------------
void __fastcall RegisterForNeonDebug(TTerminal * Terminal)
{
  TGuard Guard(DebugSection.get());
  NeonTerminals.insert(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall UnregisterFromNeonDebug(TTerminal * Terminal)
{
  TGuard Guard(DebugSection.get());
  NeonTerminals.erase(Terminal);
}
