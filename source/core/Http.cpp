//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Http.h"
#include "NeonIntf.h"
#include "Exceptions.h"
#include "ne_request.h"
#include "TextsCore.h"
#include <openssl/ssl.h>
//---------------------------------------------------------------------------
THttp::THttp()
{
  FProxyPort = 0;
  FOnDownload = NULL;
  FResponseLimit = -1;

  FRequestHeaders = NULL;
  FResponseHeaders = new TStringList();
}
//---------------------------------------------------------------------------
THttp::~THttp()
{
  delete FResponseHeaders;
}
//---------------------------------------------------------------------------
void THttp::SendRequest(const char * Method, const UnicodeString & Request)
{
  std::unique_ptr<TStringList> AttemptedUrls(CreateSortedStringList());
  AttemptedUrls->Add(URL);
  UnicodeString RequestUrl = URL;
  bool WasTlsUri = false; // shut up

  bool Retry;

  do
  {
    ne_uri uri;
    NeonParseUrl(RequestUrl, uri);

    bool IsTls = IsTlsUri(uri);
    if (RequestUrl == URL)
    {
      WasTlsUri = IsTls;
    }
    else
    {
      if (!IsTls && WasTlsUri)
      {
        throw Exception(LoadStr(UNENCRYPTED_REDIRECT));
      }
    }

    FHostName = StrFromNeon(uri.host);

    UnicodeString Uri = StrFromNeon(uri.path);
    if (uri.query != NULL)
    {
      Uri += L"?" + StrFromNeon(uri.query);
    }

    FResponse.SetLength(0);
    FCertificateError.SetLength(0);
    FException.reset(NULL);

    TProxyMethod ProxyMethod = ProxyHost.IsEmpty() ? ::pmNone : pmHTTP;

    ne_session_s * NeonSession =
      CreateNeonSession(
        uri, ProxyMethod, ProxyHost, ProxyPort, UnicodeString(), UnicodeString());
    try
    {
      if (IsTls)
      {
        SetNeonTlsInit(NeonSession, InitSslSession);

        ne_ssl_set_verify(NeonSession, NeonServerSSLCallback, this);

        ne_ssl_trust_default_ca(NeonSession);
      }

      ne_request_s * NeonRequest = ne_request_create(NeonSession, Method, StrToNeon(Uri));
      try
      {
        if (FRequestHeaders != NULL)
        {
          for (int Index = 0; Index < FRequestHeaders->Count; Index++)
          {
            ne_add_request_header(
              NeonRequest, StrToNeon(FRequestHeaders->Names[Index]), StrToNeon(FRequestHeaders->Values[Index]));
          }
        }

        UTF8String RequestUtf;
        if (!Request.IsEmpty())
        {
          RequestUtf = UTF8String(Request);
          ne_set_request_body_buffer(NeonRequest, RequestUtf.c_str(), RequestUtf.Length());
        }

        ne_add_response_body_reader(NeonRequest, ne_accept_2xx, NeonBodyReader, this);

        int Status = ne_request_dispatch(NeonRequest);

        // Exception has precedence over status as status will always be NE_ERROR,
        // as we returned 1 from NeonBodyReader
        if (FException.get() != NULL)
        {
          RethrowException(FException.get());
        }

        if (Status == NE_REDIRECT)
        {
          Retry = true;
          RequestUrl = GetNeonRedirectUrl(NeonSession);
          CheckRedirectLoop(RequestUrl, AttemptedUrls.get());
        }
        else
        {
          Retry = false;
          CheckNeonStatus(NeonSession, Status, FHostName, FCertificateError);

          const ne_status * NeonStatus = ne_get_status(NeonRequest);
          if (NeonStatus->klass != 2)
          {
            throw Exception(FMTLOAD(HTTP_ERROR2, (NeonStatus->code, StrFromNeon(NeonStatus->reason_phrase), FHostName)));
          }

          void * Cursor = NULL;
          const char * HeaderName;
          const char * HeaderValue;
          while ((Cursor = ne_response_header_iterate(NeonRequest, Cursor, &HeaderName, &HeaderValue)) != NULL)
          {
            FResponseHeaders->Values[StrFromNeon(HeaderName)] = StrFromNeon(HeaderValue);
          }
        }
      }
      __finally
      {
        ne_request_destroy(NeonRequest);
      }
    }
    __finally
    {
      DestroyNeonSession(NeonSession);
      ne_uri_free(&uri);
    }
  }
  while (Retry);
}
//---------------------------------------------------------------------------
void THttp::Get()
{
  SendRequest("GET", UnicodeString());
}
//---------------------------------------------------------------------------
void THttp::Post(const UnicodeString & Request)
{
  SendRequest("POST", Request);
}
//---------------------------------------------------------------------------
UnicodeString THttp::GetResponse()
{
  UTF8String UtfResponse(FResponse);
  return UnicodeString(UtfResponse);
}
//---------------------------------------------------------------------------
int THttp::NeonBodyReaderImpl(const char * Buf, size_t Len)
{
  bool Result = true;
  if ((FResponseLimit < 0) ||
      (FResponse.Length() + Len <= FResponseLimit))
  {
    FResponse += RawByteString(Buf, Len);

    if (FOnDownload != NULL)
    {
      bool Cancel = false;

      try
      {
        FOnDownload(this, ResponseLength, Cancel);
      }
      catch (Exception & E)
      {
        FException.reset(CloneException(&E));
        Result = false;
      }

      if (Cancel)
      {
        FException.reset(new EAbort(UnicodeString()));
        Result = false;
      }
    }
  }

  // neon wants 0 for success
  return Result ? 0 : 1;
}
//---------------------------------------------------------------------------
int THttp::NeonBodyReader(void * UserData, const char * Buf, size_t Len)
{
  THttp * Http = static_cast<THttp *>(UserData);
  return Http->NeonBodyReaderImpl(Buf, Len);
}
//---------------------------------------------------------------------------
__int64 THttp::GetResponseLength()
{
  return FResponse.Length();
}
//------------------------------------------------------------------------------
void THttp::InitSslSession(ssl_st * Ssl, ne_session * /*Session*/)
{
  int Options = SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1 | SSL_OP_NO_TLSv1_1;
  SSL_ctrl(Ssl, SSL_CTRL_OPTIONS, Options, NULL);
}
//---------------------------------------------------------------------------
int THttp::NeonServerSSLCallback(void * UserData, int Failures, const ne_ssl_certificate * Certificate)
{
  THttp * Http = static_cast<THttp *>(UserData);
  return Http->NeonServerSSLCallbackImpl(Failures, Certificate);
}
//---------------------------------------------------------------------------
int THttp::NeonServerSSLCallbackImpl(int Failures, const ne_ssl_certificate * Certificate)
{
  AnsiString AsciiCert = NeonExportCertificate(Certificate);

  // winscp.net 31.05.2015 - 02.06.2016
  const AnsiString WebCert = "MIIEqzCCA5OgAwIBAgIDBMLgMA0GCSqGSIb3DQEBCwUAMEcxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMSAwHgYDVQQDExdSYXBpZFNTTCBTSEEyNTYgQ0EgLSBHMzAeFw0xNTA1MzEwMDA1NTRaFw0xNjA2MDIwNTUxNTNaMIGSMRMwEQYDVQQLEwpHVDUyNTA2NDcyMTEwLwYDVQQLEyhTZWUgd3d3LnJhcGlkc3NsLmNvbS9yZXNvdXJjZXMvY3BzIChjKTE1MS8wLQYDVQQLEyZEb21haW4gQ29udHJvbCBWYWxpZGF0ZWQgLSBSYXBpZFNTTChSKTEXMBUGA1UEAxMOd3d3LndpbnNjcC5uZXQwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDRHP+5FLE+q+oeu0Qlx2eX1nV8Vc0jOkwam95Vr6JV9Ar5ZZIbR8a/jBJv/tDlQ8nvx38gPoFCcPX3qBjRhxIBuNETbLi+7Yd69CXyZKNrbo0pxAbn5C/JKFgog5EkSdR65gia0J6YGPDw/iq180MpkNIBAFInq8Pc36hLz4jrAunFjxJ18pkmBlOVSr7/4Ppd15Co9fhF4A3QI2wcrAOjEfGhssfk7aRp8x36/GI3rs/Or1SbO6/ZSg9h7a5uvJFgQPDVRmqytd03EC9HLVOvRK/70gcQfYcOSEWkEkEO8jQ7eXv0u1y5E20Te0Zk9hVXll2RpCO8u5RyyzhSKW1DAgMBAAGjggFSMIIBTjAfBgNVHSMEGDAWgBTDnPP800YINLvORn+gfFvz4gjLWTBXBggrBgEFBQcBAQRLMEkwHwYIKwYBBQUHMAGGE2h0dHA6Ly9ndi5zeW1jZC5jb20wJgYIKwYBBQUHMAKGGmh0dHA6Ly9ndi5zeW1jYi5jb20vZ3YuY3J0MA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwJQYDVR0RBB4wHIIOd3d3LndpbnNjcC5uZXSCCndpbnNjcC5uZXQwKwYDVR0fBCQwIjAgoB6gHIYaaHR0cDovL2d2LnN5bWNiLmNvbS9ndi5jcmwwDAYDVR0TAQH/BAIwADBBBgNVHSAEOjA4MDYGBmeBDAECATAsMCoGCCsGAQUFBwIBFh5odHRwczovL3d3dy5yYXBpZHNzbC5jb20vbGVnYWwwDQYJKoZIhvcNAQELBQADggEBAHzQ0JNQOtaYUbcw6OCyyiV5O5VemJSDagD5VG5W2gkLfxa9v1ly9hxbdlkKv4d9ruq36ZiHxqKBLoBzw68RNOG/CFY85Xam5Wygo8afSIuhLOYgSUMriH8aoBUXssG15t54z1em58Qh5xMp0crQAbY7D4opo0pEzkaTaS8ulwOxu5SSpK3DF12VKUdKhBs7T0QY+oOJZsqkx7GdmeKxoKRpBQvnRegCszR7vkdWsY0fFZHeNdThkY1iRxI6b4tyDYgZ0COj8WXCBia9wZm/czMmq/d7KED2V06w3Ttc4hDOHl+Onm/gQFLs++1f2Z/X6iPNhIEMNKKH51y/eHJQMxE=";
  // cdn.winscp.net 02.06.2015 - 04.06.2016
  const AnsiString CdnCert = "MIIEnzCCA4egAwIBAgIDBMxPMA0GCSqGSIb3DQEBCwUAMEcxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMSAwHgYDVQQDExdSYXBpZFNTTCBTSEEyNTYgQ0EgLSBHMzAeFw0xNTA2MDIwNjIzMDFaFw0xNjA2MDQwMDU0NTVaMIGSMRMwEQYDVQQLEwpHVDUwMzQ0NzgyMTEwLwYDVQQLEyhTZWUgd3d3LnJhcGlkc3NsLmNvbS9yZXNvdXJjZXMvY3BzIChjKTE1MS8wLQYDVQQLEyZEb21haW4gQ29udHJvbCBWYWxpZGF0ZWQgLSBSYXBpZFNTTChSKTEXMBUGA1UEAxMOY2RuLndpbnNjcC5uZXQwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC5ykjjmhjnd6hMmb7WNlQwSzjGCiLA3b+U2xfUYGsR+0o8frU3bv50vNp+IlTFXn32Qt3QtskuoLdi/nip0ABBtgEUt4FAoZ/AMKPyT0hD95ZEFlaclCzUANmSJswWdQIZltbLulMAqt618Snkog3Z3nkjzAMZuKvEYvPV9ujuM2kzkWZr0/OoPHINUkaI6mZwTxCvxwmazBtyIx5tXqmfiOJ1R+viAZm4Av5nqaXz1dTqSjrgpvFtwkTu8YQpK3qrpjN+H67PnClOAS1GY0oaSMccxq0bYL1w0hORa+NyQ+ZTIiXVwiXFJ0w23qHadaoKOiG8WnQ49YvCpbDhMU+3AgMBAAGjggFGMIIBQjAfBgNVHSMEGDAWgBTDnPP800YINLvORn+gfFvz4gjLWTBXBggrBgEFBQcBAQRLMEkwHwYIKwYBBQUHMAGGE2h0dHA6Ly9ndi5zeW1jZC5jb20wJgYIKwYBBQUHMAKGGmh0dHA6Ly9ndi5zeW1jYi5jb20vZ3YuY3J0MA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwGQYDVR0RBBIwEIIOY2RuLndpbnNjcC5uZXQwKwYDVR0fBCQwIjAgoB6gHIYaaHR0cDovL2d2LnN5bWNiLmNvbS9ndi5jcmwwDAYDVR0TAQH/BAIwADBBBgNVHSAEOjA4MDYGBmeBDAECATAsMCoGCCsGAQUFBwIBFh5odHRwczovL3d3dy5yYXBpZHNzbC5jb20vbGVnYWwwDQYJKoZIhvcNAQELBQADggEBAF9BN1+whMrK/HCDggLi/76+zDkzqVvNjdTgOLLSBCZaTJV1xNwIfxrPVwECEKkqV0D3eYx53aMxudVq+QuQ7VlS2k0Nu12Bfr0LFcUIOOO55jfjXUnJ8QLWoZQIJ6spOk0Bilxos4yzcORxOfASjkECEg1XUK3THnNgVLKZS92JSdxzsRkZvkpKCSNlX4ftoaPyDsgYWv0gBBA4RPAjAJB5qQTUeu0xIO/r1IhqLqnhlnJ3ewE68ScHbE5sMpl5RLEiaVRBeNw/whVEDPe2TY+JNzk6NdsGWq6uPCFsCXTGzX2QkAwR+rk2y4PPoY2vnj2cQoYWXF5pBjpMPYyRu8Q=";

  if ((AsciiCert == WebCert) ||
      (AsciiCert == CdnCert))
  {
    Failures &= ~NE_SSL_UNTRUSTED;
  }

  if (Failures != 0)
  {
    NeonWindowsValidateCertificate(Failures, AsciiCert);
  }

  if (Failures != 0)
  {
    FCertificateError = NeonCertificateFailuresErrorStr(Failures, FHostName);
  }

  return (Failures == 0) ? NE_OK : NE_ERROR;
}
//---------------------------------------------------------------------------
