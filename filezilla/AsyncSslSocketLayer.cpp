/*           CAsyncSslSocketLayer by Tim Kosse 
          mailto: tim.kosse@filezilla-project.org)
                 Version 2.0 (2005-02-27)
-------------------------------------------------------------

Introduction
------------

CAsyncSslSocketLayer is a layer class for CAsyncSocketEx which allows you to establish SSL secured
connections. Support for both client and server side is provided.

How to use
----------

Using this class is really simple. In the easiest case, just add an instance of
CAsyncSslSocketLayer to your socket and call InitClientSsl after creation of the socket.

This class only has a couple of public functions:
- InitSSLConnection(bool clientMode);
  This functions establishes an SSL connection. The clientMode parameter specifies wether the SSL connection 
  is in server or in client mode.
  Most likely you want to call this function right after calling Create for the socket.
  But sometimes, you'll need to call this function later. One example is for an FTP connection
  with explicit SSL: In this case you would have to call InitSSLConnection after receiving the reply
  to an 'AUTH SSL' command.
- Is UsingSSL();
  Returns true if you've previously called InitClientSsl()
- SetNotifyReply(SetNotifyReply(int nID, int nCode, int result);
  You can call this function only after receiving a layerspecific callback with the SSL_VERIFY_CERT 
  id. Set result to 1 if you trust the certificate and 0 if you don't trust it.
  nID has to be the priv_data element of the t_SslCertData structure and nCode has to be SSL_VERIFY_CERT.
- CreateSslCertificate(LPCTSTR filename, int bits, unsigned char* country, unsigned char* state,
			unsigned char* locality, unsigned char* organization, unsigned char* unit, unsigned char* cname,
			unsigned char *email, CString& err);
  Creates a new self-signed SSL certificate and stores it in the given file
- SendRaw(const void* lpBuf, int nBufLen, int nFlags = 0)
  Sends a raw, unencrypted message. This may be useful after successful initialization to tell the other
  side that can use SSL.

This layer sends some layerspecific notifications to your socket instance, you can handle them in
OnLayerCallback of your socket class.
Valid notification IDs are:
- SSL_INFO 0
  There are two possible values for param2:
	SSL_INFO_ESTABLISHED 0 - You'll get this notification if the SSL negotiation was successful
	SSL_INFO_SHUTDOWNCOMPLETE 1 - You'll get this notification if the SSL connection has been shut 
                                  down sucessfully. See below for details.
- SSL_FAILURE 1
  This notification is sent if the SSL connection could not be established or if an existing 
  connection failed. Valid values for param2 are:
  - SSL_FAILURE_UNKNOWN 0 - Details may have been sent with a SSL_VERBOSE_* notification.
  - SSL_FAILURE_ESTABLISH 1 - Problem during SSL negotiation
  - SSL_FAILURE_LOADDLLS 2
  - SSL_FAILURE_INITSSL 4
  - SSL_FAILURE_VERIFYCERT 8 - The remote SSL certificate was invalid
  - SSL_FAILURE_CERTREJECTED 16 - The remote SSL certificate was rejected by user
- SSL_VERBOSE_WARNING 3
  SSL_VERBOSE_INFO 4
  This two notifications contain some additional information. The value given by param2 is a 
  pointer to a null-terminated char string (char *) with some useful information.
- SSL_VERIFY_CERT 2
  This notification is sent each time a remote certificate has to be verified.
  param2 is a pointer to a t_SslCertData structure which contains some information
  about the remote certificate.
  You have to set the reply to this message using the SetNotifyReply function.
  
Be careful with closing the connection after sending data, not all data may have been sent already.
Before closing the connection, you should call Shutdown() and wait for the SSL_INFO_SHUTDOWNCOMPLETE
notification. This assures that all encrypted data really has been sent.

License
-------

Feel free to use this class, as long as you don't claim that you wrote it
and this copyright notice stays intact in the source files.
If you want to use this class in a commercial application, a short message
to tim.kosse@filezilla-project.org would be appreciated but is not required.

This product includes software developed by the OpenSSL Project
for use in the OpenSSL Toolkit. (http://www.openssl.org/)

Version history
---------------

Version 2.0:
- Add server support
- a lot of bug fixes

*/

#include "stdafx.h"
#include "AsyncSslSocketLayer.h"

#if defined _DEBUG && defined _AFX
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// Simple macro to declare function type and function pointer based on the
// three given parametrs:
// r - return type,
// n - function name
// a - argument list
//
// Example:
// def(int, foo, (int x)) becomes the following:
// typedef int (*tfoo)(int x);
// static tfoo pfoo;

#define def(r, n, a) \
	typedef r (*t##n) a; \
	static t##n p##n;

// Macro to load the given macro from a dll:
#ifdef MPEXT_NO_SSLDLL
#include <openssl/err.h>
#define load(dll, n) \
	p##n = n;
#else
#define load(dll, n) \
	p##n = (t##n) GetProcAddress(dll, #n); \
	if (!p##n) \
		bError = true;
#endif

//The following functions from the SSL libraries are used:
def(int, SSL_state, (const SSL *s));
def(const char*, SSL_state_string_long, (const SSL *s));
def(void, SSL_set_info_callback, (SSL *ssl, void (*cb)(const SSL *ssl,int type,int val)));
def(void, SSL_set_bio, (SSL *s, BIO *rbio, BIO *wbio));
def(void, SSL_set_connect_state, (SSL *s));
def(int, SSL_set_session, (SSL *to, SSL_SESSION *session));
def(BIO_METHOD*, BIO_f_ssl, (void));
def(SSL*, SSL_new, (SSL_CTX *ctx));
def(SSL_CTX*, SSL_CTX_new, (SSL_METHOD *meth));
def(SSL_METHOD*, SSLv23_method, (void));
def(void, SSL_load_error_strings, (void));
def(int, SSL_library_init, (void));
def(void, SSL_CTX_free, (SSL_CTX *));
def(void, SSL_free, (SSL *ssl));
def(int, SSL_get_error, (const SSL *s, int retcode));
def(int, SSL_shutdown, (SSL *s));
def(int, SSL_get_shutdown, (const SSL *ssl));
def(const char*, SSL_alert_type_string_long, (int value));
def(const char*, SSL_alert_desc_string_long, (int value));
def(void, SSL_CTX_set_verify, (SSL_CTX *ctx, int mode, int (*callback)(int, X509_STORE_CTX *)));
def(X509_STORE*, SSL_CTX_get_cert_store, (const SSL_CTX *));
def(long, SSL_get_verify_result, (const SSL *ssl));
def(X509*, SSL_get_peer_certificate, (const SSL *s));
def(const char*, SSL_get_version, (const SSL *ssl));
def(SSL_CIPHER*, SSL_get_current_cipher, (const SSL *ssl));
def(const char*, SSL_CIPHER_get_name, (const SSL_CIPHER *cipher));
def(char*, SSL_CIPHER_get_version, (const SSL_CIPHER *cipher));
def(int, SSL_get_ex_data_X509_STORE_CTX_idx, (void));
def(int, SSL_CTX_load_verify_locations, (SSL_CTX *ctx, const char *CAfile, const char *CApath));
def(long, SSL_ctrl, (SSL *ssl, int cmd, long larg, void *parg));
def(void, SSL_set_accept_state, (SSL *ssl));
def(int, SSL_CTX_use_PrivateKey_file, (SSL_CTX *ctx, const char *file, int type));
def(int, SSL_CTX_use_certificate_file, (SSL_CTX *ctx, const char *file, int type));
def(int, SSL_CTX_check_private_key, (const SSL_CTX *ctx));
def(void, SSL_CTX_set_default_passwd_cb, (SSL_CTX *ctx, pem_password_cb *cb));
def(void, SSL_CTX_set_default_passwd_cb_userdata, (SSL_CTX *ctx, void *u));
def(int, SSL_CTX_use_certificate_chain_file, (SSL_CTX *ctx, const char *file));

def(size_t, BIO_ctrl_pending, (BIO *b));
def(int, BIO_read, (BIO *b, void *data, int len));
def(long, BIO_ctrl, (BIO *bp, int cmd, long larg, void *parg));
def(int, BIO_write, (BIO *b, const void *data, int len));
def(size_t, BIO_ctrl_get_write_guarantee, (BIO *b));
def(int, BIO_new_bio_pair, (BIO **bio1, size_t writebuf1, BIO **bio2, size_t writebuf2));
def(BIO*, BIO_new, (BIO_METHOD *type));
def(int, BIO_free, (BIO *a));
def(int, i2t_ASN1_OBJECT, (char *buf, int buf_len, ASN1_OBJECT *a));
def(int, OBJ_obj2nid, (const ASN1_OBJECT *o));
def(ASN1_OBJECT*, X509_NAME_ENTRY_get_object, (X509_NAME_ENTRY *ne));
def(X509_NAME_ENTRY*, X509_NAME_get_entry, (X509_NAME *name, int loc));
def(int, X509_NAME_entry_count, (X509_NAME *name));
def(X509_NAME*, X509_get_subject_name, (X509 *a));
def(X509_NAME*, X509_get_issuer_name, (X509 *a));
def(const char*, OBJ_nid2sn, (int n));
def(ASN1_STRING*, X509_NAME_ENTRY_get_data, (X509_NAME_ENTRY *ne));
def(void, X509_STORE_CTX_set_error, (X509_STORE_CTX *ctx, int s));
def(int, X509_digest, (const X509 *data, const EVP_MD *type, unsigned char *md, unsigned int *len));
def(const EVP_MD*, EVP_sha1, (void));
def(X509*, X509_STORE_CTX_get_current_cert, (X509_STORE_CTX *ctx));
def(int, X509_STORE_CTX_get_error, (X509_STORE_CTX *ctx));
def(void, X509_free, (X509 *a));
def(EVP_PKEY*, X509_get_pubkey, (X509 *x));
def(int, BN_num_bits, (const BIGNUM *a));
def(void, EVP_PKEY_free, (EVP_PKEY *pkey));
def(void*, X509_STORE_CTX_get_ex_data, (X509_STORE_CTX *ctx, int idx));
def(char*, X509_NAME_oneline, (X509_NAME *a, char *buf, int size));
def(const char*, X509_verify_cert_error_string, (long n));
def(int, X509_STORE_CTX_get_error_depth, (X509_STORE_CTX *ctx));
def(unsigned long, ERR_get_error, (void));
#ifdef MPEXT
def(char*, ERR_error_string, (unsigned long e, char *buf));
#else
def(const char*, ERR_error_string, (unsigned long e, char *buf));
#endif
def(int, ASN1_STRING_to_UTF8, (unsigned char **out, ASN1_STRING *in));
def(void, CRYPTO_free, (void *p));
def(RSA*, RSA_generate_key, (int bits, unsigned long e, void (*callback)(int,int,void *), void *cb_arg));
def(int, X509_set_version, (X509 *x,long version));
def(ASN1_TIME*, X509_gmtime_adj, (ASN1_TIME *s, long adj));
def(int, X509_set_pubkey, (X509 *x, EVP_PKEY *pkey));
def(int, X509_NAME_add_entry_by_txt, (X509_NAME *name, const char *field, int type, const unsigned char *bytes, int len, int loc, int set));
def(int, X509_NAME_add_entry_by_NID, (X509_NAME *name, int nid, int type, unsigned char *bytes, int len, int loc, int set));
def(int, X509_set_issuer_name, (X509 *x, X509_NAME *name));
def(int, X509_sign, (X509 *x, EVP_PKEY *pkey, const EVP_MD *md));
def(EVP_PKEY*, EVP_PKEY_new, (void));
def(int, EVP_PKEY_assign, (EVP_PKEY *pkey, int type, char *key));
def(X509*, X509_new, (void));
def(int, ASN1_INTEGER_set, (ASN1_INTEGER *a, long v));
def(ASN1_INTEGER*, X509_get_serialNumber, (X509 *x));
#ifdef MPEXT
def(int, PEM_ASN1_write_bio, (i2d_of_void *i2d,const char *name,BIO *bp,char *x, const EVP_CIPHER *enc,unsigned char *kstr,int klen, pem_password_cb *callback, void *u));
#else
def(int, PEM_ASN1_write_bio, (int (*i2d)(),const char *name,BIO *bp,char *x, const EVP_CIPHER *enc,unsigned char *kstr,int klen, pem_password_cb *callback, void *u));
#endif
def(int, i2d_X509, (X509 *x, unsigned char **out));
def(BIO_METHOD *, BIO_s_mem, (void));
def(int, i2d_PrivateKey, (EVP_PKEY *a, unsigned char **pp));

// Critical section wrapper class
#ifndef CCRITICALSECTIONWRAPPERINCLUDED
class CCriticalSectionWrapper
{
public:
	CCriticalSectionWrapper()
	{
		m_bInitialized = TRUE;
		InitializeCriticalSection(&m_criticalSection);
	}

	~CCriticalSectionWrapper()
	{
		if (m_bInitialized)
			DeleteCriticalSection(&m_criticalSection);
		m_bInitialized = FALSE;
	}

	void Lock()
	{
		if (m_bInitialized)
			EnterCriticalSection(&m_criticalSection);
	}
	void Unlock()
	{
		if (m_bInitialized)
			LeaveCriticalSection(&m_criticalSection);
	}
protected:
	CRITICAL_SECTION m_criticalSection;
	BOOL m_bInitialized;
};
#define CCRITICALSECTIONWRAPPERINCLUDED
#endif

/////////////////////////////////////////////////////////////////////////////
// CAsyncSslSocketLayer
CCriticalSectionWrapper CAsyncSslSocketLayer::m_sCriticalSection;

CAsyncSslSocketLayer::t_SslLayerList* CAsyncSslSocketLayer::m_pSslLayerList = 0;
int CAsyncSslSocketLayer::m_nSslRefCount = 0;
#ifndef MPEXT_NO_SSLDLL
HMODULE CAsyncSslSocketLayer::m_hSslDll1 = 0;
HMODULE CAsyncSslSocketLayer::m_hSslDll2 = 0;
#endif
std::map<SSL_CTX *, int> CAsyncSslSocketLayer::m_contextRefCount;

CAsyncSslSocketLayer::CAsyncSslSocketLayer()
{
	m_ssl = 0;
	m_sslbio = 0;
	m_ibio = 0;
	m_nbio = 0;
	m_ssl_ctx = 0;

	m_bUseSSL = false;
	m_bSslInitialized = FALSE;
	m_bSslEstablished = FALSE;
	m_nNetworkSendBufferLen = 0;
	m_nNetworkSendBufferMaxLen = 0;
	m_pNetworkSendBuffer = NULL;
	m_pRetrySendBuffer = 0;
	m_nNetworkError = 0;
	m_nShutDown = 0;

	m_bBlocking = FALSE;
	m_nSslAsyncNotifyId = 0;
	m_bFailureSent = FALSE;
	m_nVerificationResult = 0;
	m_nVerificationDepth = 0;
	m_mayTriggerRead = true;
	m_mayTriggerWrite = true;
	m_mayTriggerReadUp = true;
	m_mayTriggerWriteUp = true;

	m_onCloseCalled = false;
	m_pKeyPassword = 0;
}

CAsyncSslSocketLayer::~CAsyncSslSocketLayer()
{
	UnloadSSL();
	delete [] m_pNetworkSendBuffer;
	delete [] m_pRetrySendBuffer;
	delete [] m_pKeyPassword;
}

int CAsyncSslSocketLayer::InitSSL()
{
	if (m_bSslInitialized)
		return 0;

	m_sCriticalSection.Lock();

	if (!m_nSslRefCount)
	{
#ifndef MPEXT_NO_SSLDLL
		m_hSslDll2=
			LoadLibrary(_T("libeay32.dll"));
		if (!m_hSslDll2)
		{
			if (m_hSslDll1)
				FreeLibrary(m_hSslDll1);
			m_hSslDll1=0;

			m_sCriticalSection.Unlock();
			return SSL_FAILURE_LOADDLLS;
		}

		bool bError = false;
#endif
		load(m_hSslDll2, BIO_ctrl_pending);
		load(m_hSslDll2, BIO_ctrl_pending);
		load(m_hSslDll2, BIO_read);
		load(m_hSslDll2, BIO_ctrl);
		load(m_hSslDll2, BIO_write);
		load(m_hSslDll2, BIO_ctrl_get_write_guarantee);
		load(m_hSslDll2, BIO_new_bio_pair);
		load(m_hSslDll2, BIO_new);
		load(m_hSslDll2, BIO_free);
		load(m_hSslDll2, i2t_ASN1_OBJECT);
		load(m_hSslDll2, OBJ_obj2nid);
		load(m_hSslDll2, X509_NAME_ENTRY_get_object);
		load(m_hSslDll2, X509_NAME_get_entry);
		load(m_hSslDll2, X509_NAME_entry_count);
		load(m_hSslDll2, X509_get_subject_name);
		load(m_hSslDll2, X509_get_issuer_name);
		load(m_hSslDll2, OBJ_nid2sn);
		load(m_hSslDll2, X509_NAME_ENTRY_get_data);
		load(m_hSslDll2, X509_STORE_CTX_set_error);
		load(m_hSslDll2, X509_digest);
		load(m_hSslDll2, EVP_sha1);
		load(m_hSslDll2, X509_STORE_CTX_get_current_cert);
		load(m_hSslDll2, X509_STORE_CTX_get_error);
		load(m_hSslDll2, X509_free);
		load(m_hSslDll2, X509_get_pubkey);
		load(m_hSslDll2, BN_num_bits);
		load(m_hSslDll2, EVP_PKEY_free);
		load(m_hSslDll2, X509_STORE_CTX_get_ex_data);
		load(m_hSslDll2, X509_NAME_oneline);
		load(m_hSslDll2, X509_verify_cert_error_string);
		load(m_hSslDll2, X509_STORE_CTX_get_error_depth);
		load(m_hSslDll2, ERR_get_error);
		load(m_hSslDll2, ERR_error_string);
		load(m_hSslDll2, ASN1_STRING_to_UTF8);
		load(m_hSslDll2, CRYPTO_free);
		load(m_hSslDll2, RSA_generate_key);
		load(m_hSslDll2, X509_set_version);
		load(m_hSslDll2, X509_gmtime_adj);
		load(m_hSslDll2, X509_set_pubkey);
		load(m_hSslDll2, X509_NAME_add_entry_by_txt);
		load(m_hSslDll2, X509_NAME_add_entry_by_NID);
		load(m_hSslDll2, X509_set_issuer_name);
		load(m_hSslDll2, X509_sign);
		load(m_hSslDll2, EVP_PKEY_new);
		load(m_hSslDll2, EVP_PKEY_assign);
		load(m_hSslDll2, X509_new);
		load(m_hSslDll2, ASN1_INTEGER_set);
		load(m_hSslDll2, X509_get_serialNumber);
		load(m_hSslDll2, PEM_ASN1_write_bio);
		load(m_hSslDll2, i2d_X509);
		load(m_hSslDll2, BIO_s_mem);
		load(m_hSslDll2, i2d_PrivateKey);

#ifndef MPEXT_NO_SSLDLL
		if (bError)
		{
			FreeLibrary(m_hSslDll1);
			m_hSslDll1 = 0;
			FreeLibrary(m_hSslDll2);
			m_hSslDll2 = 0;

			m_sCriticalSection.Unlock();
			return SSL_FAILURE_LOADDLLS;
		}

		m_hSslDll1 = LoadLibrary(_T("ssleay32.dll"));
		if (!m_hSslDll1)
		{
			if (m_hSslDll2)
				FreeLibrary(m_hSslDll2);
			m_hSslDll2 = NULL;
			
			m_sCriticalSection.Unlock();
			return SSL_FAILURE_LOADDLLS;
		}
#endif    
		load(m_hSslDll1, SSL_state_string_long);
		load(m_hSslDll1, SSL_state);
		load(m_hSslDll1, SSL_set_info_callback);
		load(m_hSslDll1, SSL_set_bio);
		load(m_hSslDll1, SSL_set_connect_state);
		load(m_hSslDll1, SSL_set_session);
		load(m_hSslDll1, BIO_f_ssl);
		load(m_hSslDll1, SSL_new);
		load(m_hSslDll1, SSL_CTX_new);
		load(m_hSslDll1, SSLv23_method);
		load(m_hSslDll1, SSL_load_error_strings);
		load(m_hSslDll1, SSL_library_init);
		load(m_hSslDll1, SSL_CTX_free);
		load(m_hSslDll1, SSL_free);
		load(m_hSslDll1, SSL_get_error);
		load(m_hSslDll1, SSL_shutdown);
		load(m_hSslDll1, SSL_get_shutdown);
		load(m_hSslDll1, SSL_alert_type_string_long);
		load(m_hSslDll1, SSL_alert_desc_string_long);
		load(m_hSslDll1, SSL_CTX_set_verify);
		load(m_hSslDll1, SSL_CTX_get_cert_store);
		load(m_hSslDll1, SSL_get_verify_result);
		load(m_hSslDll1, SSL_get_peer_certificate);
		load(m_hSslDll1, SSL_get_version);
		load(m_hSslDll1, SSL_get_current_cipher);
		load(m_hSslDll1, SSL_CIPHER_get_name);
		load(m_hSslDll1, SSL_CIPHER_get_version);
		load(m_hSslDll1, SSL_get_ex_data_X509_STORE_CTX_idx);
		load(m_hSslDll1, SSL_CTX_load_verify_locations);
		load(m_hSslDll1, SSL_ctrl);
		load(m_hSslDll1, SSL_set_accept_state);
		load(m_hSslDll1, SSL_CTX_use_PrivateKey_file);
		load(m_hSslDll1, SSL_CTX_use_certificate_file);
		load(m_hSslDll1, SSL_CTX_check_private_key);
		load(m_hSslDll1, SSL_CTX_set_default_passwd_cb_userdata);
		load(m_hSslDll1, SSL_CTX_set_default_passwd_cb);
		load(m_hSslDll1, SSL_CTX_use_certificate_chain_file);

#ifndef MPEXT_NO_SSLDLL
		if (bError)
		{
			FreeLibrary(m_hSslDll1);
			m_hSslDll1=0;
			if (m_hSslDll2)
				FreeLibrary(m_hSslDll2);
			m_hSslDll2=0;

			m_sCriticalSection.Unlock();
			return SSL_FAILURE_LOADDLLS;
		}

#endif
		pSSL_load_error_strings();
		if (!pSSL_library_init())
		{
#ifndef MPEXT_NO_SSLDLL
			FreeLibrary(m_hSslDll1);
			m_hSslDll1=0;
			FreeLibrary(m_hSslDll2);
			m_hSslDll2=0;

			m_sCriticalSection.Unlock();
#endif
			return SSL_FAILURE_INITSSL;
		}
	}

	m_nSslRefCount++;
	m_sCriticalSection.Unlock();

	m_bSslInitialized = true;

	return 0;
}

void CAsyncSslSocketLayer::OnReceive(int nErrorCode)
{
	if (m_bUseSSL)
	{
		if (m_bBlocking)
		{
			m_mayTriggerRead = true;
			return;
		}
		if (m_nNetworkError)
			return;

		char buffer[16384];

		m_mayTriggerRead = false;
		
		//Get number of bytes we can receive and store in the network input bio
		int len = pBIO_ctrl_get_write_guarantee(m_nbio);
		if (len > 16384)
			len = 16384;
		else if (!len)
		{
			m_mayTriggerRead = true;
			TriggerEvents();
			return;
		}
		
		int numread = 0;
		
		// Receive data
		numread = ReceiveNext(buffer, len);
		if (numread > 0)
		{
			//Store it in the network input bio and process data
			int numwritten = pBIO_write(m_nbio, buffer, numread);
			pBIO_ctrl(m_nbio, BIO_CTRL_FLUSH, 0, NULL);

			// I have no idea why this call is needed, but without it, connections
			// will stall. Perhaps it triggers some internal processing.
			// Also, ignore return value, don't do any error checking. This function
			// can report errors, even though a later call can succeed.
			char buffer;
			pBIO_read(m_sslbio, &buffer, 0);
		}
		if (!numread)
		{
			if (GetLayerState() == connected)
				TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
		}
		else if (numread == SOCKET_ERROR)
		{
			int nError = GetLastError();
			if (nError != WSAEWOULDBLOCK && nError != WSAENOTCONN)
			{
				m_nNetworkError = GetLastError();
				TriggerEvent(FD_CLOSE, 0, TRUE);
				return;
			}
		}

		if (m_pRetrySendBuffer)
		{
			int numwrite = pBIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
			if (numwrite >= 0)
			{
				pBIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
				delete [] m_pRetrySendBuffer;
				m_pRetrySendBuffer = 0;
			}
			else if (numwrite == -1)
			{
				if (!BIO_should_retry(m_sslbio))
				{
					delete [] m_pRetrySendBuffer;
					m_pRetrySendBuffer = 0;

					SetLastError(WSAECONNABORTED);
					TriggerEvent(FD_CLOSE, 0, TRUE);
					return;
				}
			}
		}

		if (!m_nShutDown && pSSL_get_shutdown(m_ssl))
		{
			if (pBIO_ctrl_pending(m_sslbio) <= 0)
			{
				if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
				{
					if (ShutDownComplete())
						TriggerEvent(FD_CLOSE, 0, TRUE);
				}
				else
				{
					m_nNetworkError = WSAECONNABORTED;
					WSASetLastError(WSAECONNABORTED);
					TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
				}
				return;
			}
		}

		if (ShutDownComplete() && m_nShutDown == 1)
		{
			//Send shutdown notification if all pending data has been sent
			DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_SHUTDOWNCOMPLETE);
			m_nShutDown++;
		}

		TriggerEvents();
	}
	else
		TriggerEvent(FD_READ, nErrorCode, TRUE);
}

void CAsyncSslSocketLayer::OnSend(int nErrorCode)
{
	if (m_bUseSSL)
	{
		if (m_nNetworkError)
			return;

		m_mayTriggerWrite = false;

		//Send data in the send buffer
		while (m_nNetworkSendBufferLen)
		{
			int numsent = SendNext(m_pNetworkSendBuffer, m_nNetworkSendBufferLen);
			if (numsent == SOCKET_ERROR)
			{
				int nError = GetLastError();
				if (nError != WSAEWOULDBLOCK && nError != WSAENOTCONN)
				{
					m_nNetworkError = nError;
					TriggerEvent(FD_CLOSE, 0, TRUE);
				}
				return;
			}
			else if (!numsent)
			{
				if (GetLayerState() == connected)
					TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
			}
			if (numsent == m_nNetworkSendBufferLen)
				m_nNetworkSendBufferLen = 0;
			else
			{
				memmove(m_pNetworkSendBuffer, m_pNetworkSendBuffer + numsent, m_nNetworkSendBufferLen - numsent);
				m_nNetworkSendBufferLen -= numsent;
			}
		}

		//Send the data waiting in the network bio
		char buffer[16384];
		int len = pBIO_ctrl_pending(m_nbio);
		int numread = pBIO_read(m_nbio, buffer, len);
		if (numread <= 0)
			m_mayTriggerWrite = true;
		while (numread > 0)
		{
			int numsent = SendNext(buffer, numread);
			if (!numsent)
			{
				if (GetLayerState() == connected)
					TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
			}
			if (numsent == SOCKET_ERROR || numsent < numread)
			{
				if (numsent == SOCKET_ERROR)
					if (GetLastError() != WSAEWOULDBLOCK && GetLastError() != WSAENOTCONN)
					{
						m_nNetworkError = GetLastError();
						TriggerEvent(FD_CLOSE, 0, TRUE);
						return;
					}
					else
						numsent = 0;

				// Add all data that was retrieved from the network bio but could not be sent to the send buffer.
				if (m_nNetworkSendBufferMaxLen < (m_nNetworkSendBufferLen + numread - numsent))
				{
					char * tmp = m_pNetworkSendBuffer;
					m_nNetworkSendBufferMaxLen = static_cast<int>((m_nNetworkSendBufferLen + numread - numsent) * 1.5);
					m_pNetworkSendBuffer = new char[m_nNetworkSendBufferMaxLen];					
					if (tmp)
					{
						memcpy(m_pNetworkSendBuffer, tmp, m_nNetworkSendBufferLen);
						delete [] tmp;
					}
				}
				ASSERT(m_pNetworkSendBuffer);
				memcpy(m_pNetworkSendBuffer + m_nNetworkSendBufferLen, buffer, numread-numsent);
				m_nNetworkSendBufferLen += numread - numsent;
			}
			if (!numsent)
				break;
			len = pBIO_ctrl_pending(m_nbio);
			if (!len)
			{
				m_mayTriggerWrite = true;
				break;
			}
			numread = pBIO_read(m_nbio, buffer, len);
			if (numread <= 0)
				m_mayTriggerWrite = true;
		}

		if (m_pRetrySendBuffer)
		{
			int numwrite = pBIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
			if (numwrite >= 0)
			{
				pBIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
				delete [] m_pRetrySendBuffer;
				m_pRetrySendBuffer = 0;
			}
			else if (numwrite == -1)
			{
				if (!BIO_should_retry(m_sslbio))
				{
					delete [] m_pRetrySendBuffer;
					m_pRetrySendBuffer = 0;

					SetLastError(WSAECONNABORTED);
					TriggerEvent(FD_CLOSE, 0, TRUE);
					return;
				}
			}
		}

		// No more data available, ask for more.
		TriggerEvents();
		if (m_nShutDown == 1 && ShutDownComplete())
		{
			//Send shutdown notification if all pending data has been sent
			DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_SHUTDOWNCOMPLETE);
			m_nShutDown++;
		}
	}
	else
		TriggerEvent(FD_WRITE, nErrorCode, TRUE);
}

int CAsyncSslSocketLayer::Send(const void* lpBuf, int nBufLen, int nFlags)
{
	if (m_bUseSSL)
	{
		if (!lpBuf)
			return 0;
		if (m_bBlocking || m_pRetrySendBuffer)
		{
			m_mayTriggerWriteUp = true;
			SetLastError(WSAEWOULDBLOCK);
			return SOCKET_ERROR;
		}
		if (m_nNetworkError)
		{
			SetLastError(m_nNetworkError);
			return SOCKET_ERROR;
		}
		if (m_nShutDown)
		{
			SetLastError(WSAESHUTDOWN);
			return SOCKET_ERROR;
		}
		if (!m_bSslEstablished)
		{
			m_mayTriggerWriteUp = true;
			SetLastError(WSAEWOULDBLOCK);
			return SOCKET_ERROR;
		}
		if (!nBufLen)
			return 0;

		if (m_onCloseCalled)
		{
			TriggerEvent(FD_CLOSE, 0, TRUE);
			return 0;
		}

		int len = pBIO_ctrl_get_write_guarantee(m_sslbio);
		if (nBufLen > len)
			nBufLen = len;
		if (!len)
		{
			m_mayTriggerWriteUp = true;
			TriggerEvents();				
			SetLastError(WSAEWOULDBLOCK);
		}

		m_pRetrySendBuffer = new char[nBufLen];
		m_nRetrySendBufferLen = nBufLen;
		memcpy(m_pRetrySendBuffer, lpBuf, nBufLen);

		int numwrite = pBIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
		if (numwrite >= 0)
		{
			pBIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
			delete [] m_pRetrySendBuffer;
			m_pRetrySendBuffer = 0;
		}
		else if (numwrite == -1)
		{
			if (BIO_should_retry(m_sslbio))
			{
				if (GetLayerState() == closed)
					return 0;
				else if (GetLayerState() != connected)
				{
					SetLastError(m_nNetworkError);
					return SOCKET_ERROR;
				}

				TriggerEvents();

				return nBufLen;
			}
			else
			{
				delete [] m_pRetrySendBuffer;
				m_pRetrySendBuffer = 0;

				SetLastError(WSAECONNABORTED);
			}
			return SOCKET_ERROR;
		}

		m_mayTriggerWriteUp = true;
		TriggerEvents();

		return numwrite;
	}
	else
	{
		return SendNext(lpBuf, nBufLen, nFlags);
	}
}

int CAsyncSslSocketLayer::Receive(void* lpBuf, int nBufLen, int nFlags)
{
	if (m_bUseSSL)
	{
		if (m_bBlocking)
		{
			m_mayTriggerReadUp = true;
			SetLastError(WSAEWOULDBLOCK);
			return SOCKET_ERROR;
		}
		if (m_nNetworkError)
		{
			if (pBIO_ctrl(m_sslbio, BIO_CTRL_PENDING, 0, NULL) && !m_nShutDown)
			{
				m_mayTriggerReadUp = true;
				TriggerEvents();
				return pBIO_read(m_sslbio, lpBuf,nBufLen);
			}
			WSASetLastError(m_nNetworkError);
			return SOCKET_ERROR;
		}
		if (m_nShutDown)
		{
			SetLastError(WSAESHUTDOWN);
			return SOCKET_ERROR;
		}
		if (!nBufLen)
			return 0;
		if (!pBIO_ctrl(m_sslbio, BIO_CTRL_PENDING, 0, NULL))
		{
			if (GetLayerState() == closed)
				return 0;
			if (m_onCloseCalled)
			{
				TriggerEvent(FD_CLOSE, 0, TRUE);
				return 0;
			}
			else if (GetLayerState() != connected)
			{
				SetLastError(m_nNetworkError);
				return SOCKET_ERROR;
			}
			else
			{
				if (pSSL_get_shutdown(m_ssl))
				{
					if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
					{
						if (ShutDownComplete())
						{
							TriggerEvent(FD_CLOSE, 0, TRUE);
							return 0;
						}
						else
							WSASetLastError(WSAEWOULDBLOCK);
					}
					else
					{
						m_nNetworkError = WSAECONNABORTED;
						WSASetLastError(WSAECONNABORTED);
						TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
					}
					return SOCKET_ERROR;
				}
			}
			m_mayTriggerReadUp = true;
			TriggerEvents();
			SetLastError(WSAEWOULDBLOCK);
			return SOCKET_ERROR;
		}
		int numread = pBIO_read(m_sslbio, lpBuf, nBufLen);
		if (!numread)
		{
			if (pSSL_get_shutdown(m_ssl))
			{
				if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
				{
					if (ShutDownComplete())
					{
						TriggerEvent(FD_CLOSE, 0, TRUE);
						return 0;
					}
					else
						WSASetLastError(WSAEWOULDBLOCK);
				}
				else
				{
					m_nNetworkError = WSAECONNABORTED;
					WSASetLastError(WSAECONNABORTED);
					TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
				}
				return SOCKET_ERROR;
			}
			m_mayTriggerReadUp = true;
			TriggerEvents();
			SetLastError(WSAEWOULDBLOCK);
			return SOCKET_ERROR;
		}
		if (numread < 0)
		{
			if (!BIO_should_retry(m_sslbio))
			{
				PrintLastErrorMsg();
				m_nNetworkError = WSAECONNABORTED;
				WSASetLastError(WSAECONNABORTED);
				TriggerEvent(FD_CLOSE, 0, TRUE);
				return SOCKET_ERROR;
			}
			else
			{
				if (pSSL_get_shutdown(m_ssl))
				{
					if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
					{
						if (ShutDownComplete())
						{
							TriggerEvent(FD_CLOSE, 0, TRUE);
							return 0;
						}
						else
							WSASetLastError(WSAEWOULDBLOCK);
					}
					else
					{
						m_nNetworkError = WSAECONNABORTED;
						WSASetLastError(WSAECONNABORTED);
						TriggerEvent(FD_CLOSE, 0, TRUE);
					}
					return SOCKET_ERROR;
				}
				m_mayTriggerReadUp = true;
				TriggerEvents();
				SetLastError(WSAEWOULDBLOCK);
				return SOCKET_ERROR;
			}
		}

		m_mayTriggerReadUp = true;
		TriggerEvents();
		return numread;
	}
	else
		return ReceiveNext(lpBuf, nBufLen, nFlags);
}

void CAsyncSslSocketLayer::Close()
{
	m_nShutDown = 0;
	m_onCloseCalled = false;
	ResetSslSession();
	CloseNext();
}

BOOL CAsyncSslSocketLayer::Connect(const SOCKADDR *lpSockAddr, int nSockAddrLen)
{
	BOOL res = ConnectNext(lpSockAddr, nSockAddrLen);
	if (!res)
		if (GetLastError() != WSAEWOULDBLOCK)
			ResetSslSession();
	return res;
}

BOOL CAsyncSslSocketLayer::Connect(LPCTSTR lpszHostAddress, UINT nHostPort)
{
	BOOL res = ConnectNext(lpszHostAddress, nHostPort);
	if (!res)
		if (GetLastError()!=WSAEWOULDBLOCK)
			ResetSslSession();
	return res;
}

int CAsyncSslSocketLayer::InitSSLConnection(bool clientMode, void* pSslContext /*=0*/)
{
	if (m_bUseSSL)
		return 0;
	int res = InitSSL();
	if (res)
		return res;

	m_sCriticalSection.Lock();
	if ((SSL_CTX*)pSslContext)
	{
		if (m_ssl_ctx)
		{
			m_sCriticalSection.Unlock();
			ResetSslSession();
			return SSL_FAILURE_INITSSL;
		}

#ifdef MPEXT
		std::map<SSL_CTX *, int>::iterator iter = m_contextRefCount.find((SSL_CTX*)pSslContext);
#else
		std::map<SSL_CTX *, int>::iterator& iter = m_contextRefCount.find((SSL_CTX*)pSslContext);
#endif
		if (iter == m_contextRefCount.end() || iter->second < 1)
		{
			m_sCriticalSection.Unlock();
			ResetSslSession();
			return SSL_FAILURE_INITSSL;
		}
		m_ssl_ctx = (SSL_CTX*)pSslContext;
		iter->second++;
	}
	else if (!m_ssl_ctx)
	{
		// Create new context if none given
		if (!(m_ssl_ctx = pSSL_CTX_new( pSSLv23_method())))
		{
			m_sCriticalSection.Unlock();
			ResetSslSession();
			return SSL_FAILURE_INITSSL;
		}
		m_contextRefCount[m_ssl_ctx] = 1;

		if (clientMode)
		{
			USES_CONVERSION;
			pSSL_CTX_set_verify(m_ssl_ctx, SSL_VERIFY_PEER, verify_callback);
			pSSL_CTX_load_verify_locations(m_ssl_ctx, T2CA(m_CertStorage), 0);
		}
	}

	//Create new SSL session
	if (!(m_ssl = pSSL_new(m_ssl_ctx)))
	{
		m_sCriticalSection.Unlock();
		ResetSslSession();
		return SSL_FAILURE_INITSSL;
	}

	//Add current instance to list of active instances
	t_SslLayerList *tmp = m_pSslLayerList;
	m_pSslLayerList = new t_SslLayerList;
	m_pSslLayerList->pNext = tmp;
	m_pSslLayerList->pLayer = this;
	m_sCriticalSection.Unlock();

	pSSL_set_info_callback(m_ssl, apps_ssl_info_callback);

	//Create bios
	m_sslbio = pBIO_new(pBIO_f_ssl());
	pBIO_new_bio_pair(&m_ibio, 4096, &m_nbio, 4096);

	if (!m_sslbio || !m_nbio || !m_ibio)
	{
		ResetSslSession();
		return SSL_FAILURE_INITSSL;
	}

	long options = pSSL_ctrl(m_ssl, SSL_CTRL_OPTIONS, 0, NULL);
	options |= SSL_OP_ALL;
	pSSL_ctrl(m_ssl, SSL_CTRL_OPTIONS, options, NULL);

	//Init SSL connection
	pSSL_set_session(m_ssl, NULL);
	if (clientMode)
	{
		pSSL_set_connect_state(m_ssl);
	}
	else
	{
		pSSL_set_accept_state(m_ssl);
	}
	pSSL_set_bio(m_ssl, m_ibio, m_ibio);
	pBIO_ctrl(m_sslbio, BIO_C_SET_SSL, BIO_NOCLOSE, m_ssl);
	pBIO_read(m_sslbio, (void *)1, 0);

	// Trigger FD_WRITE so that we can initialize SSL negotiation
	if (GetLayerState() == connected || GetLayerState() == attached)
	{
		TriggerEvent(FD_READ, 0);
		TriggerEvent(FD_WRITE, 0);
		TriggerEvent(FD_READ, 0, TRUE);
		TriggerEvent(FD_WRITE, 0, TRUE);
	}

	m_bUseSSL = true;

	return 0;
}

void CAsyncSslSocketLayer::ResetSslSession()
{
	if (m_pRetrySendBuffer)
	{
		delete [] m_pRetrySendBuffer;
		m_pRetrySendBuffer = 0;
	}

	m_bFailureSent = FALSE;
	m_bBlocking = FALSE;
	m_nSslAsyncNotifyId++;
	m_nNetworkError = 0;
	m_bUseSSL = FALSE;
	m_nVerificationResult = 0;
	m_nVerificationDepth = 0;

	m_bSslEstablished = FALSE;
	if (m_sslbio)
	{
		pBIO_free(m_sslbio);
	}
	if (m_ssl)
	{
		pSSL_set_session(m_ssl,NULL);
	}
	if (m_nbio)
	{
		pBIO_free(m_nbio);
	}
	if (m_ibio)
	{
		pBIO_free(m_ibio);
	}

	m_nNetworkSendBufferLen = 0;

	m_nbio = 0;
	m_ibio = 0;
	m_sslbio = 0;

	if (m_ssl)
	{
		pSSL_free(m_ssl);
	}

	m_sCriticalSection.Lock();

	if (m_ssl_ctx)
	{
#ifdef MPEXT
		std::map<SSL_CTX *, int>::iterator iter = m_contextRefCount.find(m_ssl_ctx);
#else
		std::map<SSL_CTX *, int>::iterator& iter = m_contextRefCount.find(m_ssl_ctx);
#endif
		if (iter != m_contextRefCount.end())
		{
			if (iter->second <= 1)
			{
				pSSL_CTX_free(m_ssl_ctx);
				m_contextRefCount.erase(iter);
			}
			else
				iter->second--;
		}
		m_ssl_ctx = 0;
	}

	delete [] m_pKeyPassword;
	m_pKeyPassword = 0;

	m_ssl = 0;
	t_SslLayerList *cur = m_pSslLayerList;
	if (!cur)
	{
		m_sCriticalSection.Unlock();
		return;
	}

	if (cur->pLayer == this)
	{
		m_pSslLayerList = cur->pNext;
		delete cur;
	}
	else
		while (cur->pNext)
		{
			if (cur->pNext->pLayer == this)
			{
				t_SslLayerList *tmp = cur->pNext;
				cur->pNext = cur->pNext->pNext;
				delete tmp;

				m_sCriticalSection.Unlock();
				return;
			}
			cur = cur->pNext;
		}
	m_sCriticalSection.Unlock();
}

bool CAsyncSslSocketLayer::IsUsingSSL()
{
	return m_bUseSSL;
}

BOOL CAsyncSslSocketLayer::ShutDown(int nHow /*=sends*/)
{
	if (m_bUseSSL)
	{
		if (m_pRetrySendBuffer)
		{
			if (!m_nShutDown)
				m_nShutDown = 1;
			WSASetLastError(WSAEWOULDBLOCK);
			return false;
		}
		if (!m_nShutDown)
			m_nShutDown = 1;
		else
		{
			if (ShutDownComplete())
				return ShutDownNext();
			else
			{
				TriggerEvents();
				WSASetLastError(WSAEWOULDBLOCK);
				return false;
			}
		}
		
		int res = pSSL_shutdown(m_ssl);
		if (res != -1)
		{
			if (!res)
			{
				pSSL_shutdown(m_ssl);
			}
			if (ShutDownComplete())
				return ShutDownNext();
			else
			{
				TriggerEvents();
				WSASetLastError(WSAEWOULDBLOCK);
				return FALSE;
			}
		}
		else
		{
			int error = pSSL_get_error(m_ssl, -1);
			if (error == SSL_ERROR_WANT_READ || error == SSL_ERROR_WANT_WRITE)
			{
				TriggerEvents();
				WSASetLastError(WSAEWOULDBLOCK);
				return FALSE;
			}
			else if (ShutDownComplete())
				return ShutDownNext();
			else
			{
				TriggerEvents();
				WSASetLastError(WSAEWOULDBLOCK);
				return FALSE;
			}
		}
	}
	else
	{
		if (!m_nShutDown)
			m_nShutDown = 1;
		return ShutDownNext(nHow);
	}
}

BOOL CAsyncSslSocketLayer::ShutDownComplete()
{
	//If a ShutDown was issued, has the connection already been shut down?
	if (!m_nShutDown)
		return FALSE;
	else if (!m_bUseSSL)
		return FALSE;
	else if (m_nNetworkSendBufferLen)
		return FALSE;
	else if (m_pRetrySendBuffer)
		return FALSE;
	
	// Empty read buffer
	char buffer[1000];
	int numread;
	do
	{
		numread = pBIO_read(m_sslbio, buffer, 1000);
	} while (numread > 0);

	if (pBIO_ctrl_pending(m_nbio))
		return FALSE;
	else
		return TRUE;
}

void CAsyncSslSocketLayer::apps_ssl_info_callback(const SSL *s, int where, int ret)
{
	CAsyncSslSocketLayer *pLayer = 0;
	m_sCriticalSection.Lock();
	t_SslLayerList *cur = m_pSslLayerList;
	while (cur)
	{
		if (cur->pLayer->m_ssl == s)
			break;
		cur = cur->pNext;
	}
	if (!cur)
	{
		m_sCriticalSection.Unlock();
		MessageBox(0, _T("Can't lookup SSL session!"), _T("Critical error"), MB_ICONEXCLAMATION);
		return;
	}
	else
		pLayer = cur->pLayer;
	m_sCriticalSection.Unlock();

	// Called while unloading?
	if (!pLayer->m_bUseSSL)
		return;

	char * str;
	int w;

	w = where& ~SSL_ST_MASK;

	if (w & SSL_ST_CONNECT)
		str = "SSL_connect";
	else if (w & SSL_ST_ACCEPT)
		str = "SSL_accept";
	else
		str = "undefined";

	if (where & SSL_CB_LOOP)
	{
		char *buffer = new char[4096];
		sprintf(buffer, "%s: %s",
				str,
				pSSL_state_string_long(s));
		pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_INFO, 0, buffer);
	}
	else if (where & SSL_CB_ALERT)
	{
		str=(where & SSL_CB_READ)? "read" : "write";
		const char* desc = pSSL_alert_desc_string_long(ret);

		// Don't send close notify warning
		if (desc && strcmp(desc, "close notify"))
		{
			char *buffer = new char[4096];
			sprintf(buffer, "SSL3 alert %s: %s: %s",
					str,
					pSSL_alert_type_string_long(ret),
					desc);
			pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_WARNING, 0, buffer);
		}
	}

	else if (where & SSL_CB_EXIT)
	{
		if (ret == 0)
		{
			char *buffer = new char[4096];
			sprintf(buffer, "%s: failed in %s",
					str,
					pSSL_state_string_long(s));
			pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_WARNING, 0, buffer);
			if (!pLayer->m_bFailureSent)
			{
				pLayer->m_bFailureSent=TRUE;
				pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, pLayer->m_bSslEstablished ? SSL_FAILURE_UNKNOWN : SSL_FAILURE_ESTABLISH);
			}
		}
		else if (ret < 0)
		{
			int error = pSSL_get_error(s,ret);
			if (error != SSL_ERROR_WANT_READ && error != SSL_ERROR_WANT_WRITE)
			{
				char *buffer = new char[4096];
				sprintf(buffer, "%s: error in %s",
						str,
						pSSL_state_string_long(s));
				pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_WARNING, 0, buffer);
				if (!pLayer->m_bFailureSent)
				{
					pLayer->m_bFailureSent=TRUE;
					pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, pLayer->m_bSslEstablished ? SSL_FAILURE_UNKNOWN : SSL_FAILURE_ESTABLISH);
				}
			}
		}
	}
	if (where & SSL_CB_HANDSHAKE_DONE)
	{
		int error = pSSL_get_verify_result(pLayer->m_ssl);
		if (error)
		{
			pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERIFY_CERT, error);
			pLayer->m_bBlocking = TRUE;
			return;
		}
		pLayer->m_bSslEstablished = TRUE;
		pLayer->PrintSessionInfo();
		pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_ESTABLISHED);
		pLayer->TriggerEvents();
	}
}


void CAsyncSslSocketLayer::UnloadSSL()
{
	if (!m_bSslInitialized)
		return;
	ResetSslSession();

	m_bSslInitialized = false;

	m_sCriticalSection.Lock();
	m_nSslRefCount--;
	if (m_nSslRefCount)
	{
		m_sCriticalSection.Unlock();
		return;
	}

#ifndef MPEXT_NO_SSLDLL
	if (m_hSslDll1)
		FreeLibrary(m_hSslDll1);
	if (m_hSslDll2)
	{
		FreeLibrary(m_hSslDll2);
		FreeLibrary(m_hSslDll2);
	}
	m_hSslDll1 = NULL;
	m_hSslDll2 = NULL;
#endif
	m_sCriticalSection.Unlock();
}

BOOL CAsyncSslSocketLayer::GetPeerCertificateData(t_SslCertData &SslCertData)
{
	X509 *pX509=pSSL_get_peer_certificate(m_ssl);
	if (!pX509)
		return FALSE;

	//Reset the contents of SslCertData
	memset(&SslCertData, 0, sizeof(t_SslCertData));

	//Set subject data fields
	X509_NAME *pX509Name=pX509_get_subject_name(pX509);

	if (pX509Name)
	{
		int count=pX509_NAME_entry_count(pX509Name);
		for (int i=0;i<count;i++)
		{
			X509_NAME_ENTRY *pX509NameEntry=pX509_NAME_get_entry(pX509Name,i);
			if (!pX509NameEntry)
				continue;
			ASN1_OBJECT *pObject = pX509_NAME_ENTRY_get_object(pX509NameEntry);
			ASN1_STRING *pString = pX509_NAME_ENTRY_get_data(pX509NameEntry);
			CString str;

			unsigned char *out;
			int len = pASN1_STRING_to_UTF8(&out, pString);
			if (len > 0)
			{
				// Keep it huge
				LPWSTR unicode = new WCHAR[len * 10];
				memset(unicode, 0, sizeof(WCHAR) * len * 10);
				int unicodeLen = MultiByteToWideChar(CP_UTF8, 0, (const char *)out, len, unicode, len * 10);
				if (unicodeLen > 0)
				{
#ifdef _UNICODE
					str = unicode;
#else
					LPSTR ansi = new CHAR[len * 10];
					memset(ansi, 0, sizeof(CHAR) * len * 10);
					int ansiLen = WideCharToMultiByte(CP_ACP, 0, unicode, unicodeLen, ansi, len * 10, 0, 0);
					if (ansiLen > 0)
						str = ansi;

					delete [] ansi;
#endif
				}
				delete [] unicode;
				pCRYPTO_free(out);
			}

			switch(pOBJ_obj2nid(pObject))
			{
			case NID_organizationName:
				_tcsncpy(SslCertData.subject.Organization, str, 255);
				SslCertData.subject.Organization[255] = 0;
				break;
			case NID_organizationalUnitName:
				_tcsncpy(SslCertData.subject.Unit, str, 255);
				SslCertData.subject.Unit[255] = 0;
				break;
			case NID_commonName:
				_tcsncpy(SslCertData.subject.CommonName, str, 255);
				SslCertData.subject.CommonName[255] = 0;
				break;
			case NID_pkcs9_emailAddress:
				_tcsncpy(SslCertData.subject.Mail, str, 255);
				SslCertData.subject.Mail[255] = 0;
				break;
			case NID_countryName:
				_tcsncpy(SslCertData.subject.Country, str, 255);
				SslCertData.subject.Country[255] = 0;
				break;
			case NID_stateOrProvinceName:
				_tcsncpy(SslCertData.subject.StateProvince, str, 255);
				SslCertData.subject.StateProvince[255] = 0;
				break;
			case NID_localityName:
				_tcsncpy(SslCertData.subject.Town, str, 255);
				SslCertData.subject.Town[255] = 0;
				break;
			default:
				if ( !pOBJ_nid2sn(pOBJ_obj2nid(pObject)) )
				{
					TCHAR tmp[20];
					_stprintf(tmp, _T("%d"), pOBJ_obj2nid(pObject));
					int maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), tmp, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), _T("="), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), str, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), _T(";"), maxlen);
				}
				else
				{
					int maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;

					USES_CONVERSION;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), A2CT(pOBJ_nid2sn(pOBJ_obj2nid(pObject))), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), _T("="), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), str, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
					_tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), _T(";"), maxlen);
				}
				break;
			}
		}
	}

	//Set issuer data fields
	pX509Name=pX509_get_issuer_name(pX509);
	if (pX509Name)
	{
		int count=pX509_NAME_entry_count(pX509Name);
		for (int i=0;i<count;i++)
		{
			X509_NAME_ENTRY *pX509NameEntry=pX509_NAME_get_entry(pX509Name,i);
			if (!pX509NameEntry)
				continue;
			ASN1_STRING *pString=pX509_NAME_ENTRY_get_data(pX509NameEntry);
			ASN1_OBJECT *pObject=pX509_NAME_ENTRY_get_object(pX509NameEntry);

			CString str;

			unsigned char *out;
			int len = pASN1_STRING_to_UTF8(&out, pString);
			if (len > 0)
			{
				// Keep it huge
				LPWSTR unicode = new WCHAR[len * 10];
				memset(unicode, 0, sizeof(WCHAR) * len * 10);
				int unicodeLen = MultiByteToWideChar(CP_UTF8, 0, (const char *)out, len, unicode, len * 10);
				if (unicodeLen > 0)
				{
#ifdef _UNICODE
					str = unicode;
#else
					LPSTR ansi = new CHAR[len * 10];
					memset(ansi, 0, sizeof(CHAR) * len * 10);
					int ansiLen = WideCharToMultiByte(CP_ACP, 0, unicode, unicodeLen, ansi, len * 10, 0, 0);
					if (ansiLen > 0)
						str = ansi;

					delete [] ansi;
#endif
				}
				delete [] unicode;
				pCRYPTO_free(out);
			}

			switch(pOBJ_obj2nid(pObject))
			{
			case NID_organizationName:
				_tcsncpy(SslCertData.issuer.Organization, str, 255);
				SslCertData.issuer.Organization[255] = 0;
				break;
			case NID_organizationalUnitName:
				_tcsncpy(SslCertData.issuer.Unit, str, 255);
				SslCertData.issuer.Unit[255] = 0;
				break;
			case NID_commonName:
				_tcsncpy(SslCertData.issuer.CommonName, str, 255);
				SslCertData.issuer.CommonName[255] = 0;
				break;
			case NID_pkcs9_emailAddress:
				_tcsncpy(SslCertData.issuer.Mail, str, 255);
				SslCertData.issuer.Mail[255] = 0;
				break;
			case NID_countryName:
				_tcsncpy(SslCertData.issuer.Country, str, 255);
				SslCertData.issuer.Country[255] = 0;
				break;
			case NID_stateOrProvinceName:
				_tcsncpy(SslCertData.issuer.StateProvince, str, 255);
				SslCertData.issuer.StateProvince[255] = 0;
				break;
			case NID_localityName:
				_tcsncpy(SslCertData.issuer.Town, str, 255);
				SslCertData.issuer.Town[255] = 0;
				break;
			default:
				if ( !pOBJ_nid2sn(pOBJ_obj2nid(pObject)) )
				{
					TCHAR tmp[20];
					_stprintf(tmp, _T("%d"), pOBJ_obj2nid(pObject));
					int maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), tmp, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), _T("="), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), str, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), _T(";"), maxlen);
				}
				else
				{
					int maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;

					USES_CONVERSION;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), A2CT(pOBJ_nid2sn(pOBJ_obj2nid(pObject))), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), _T("="), maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), str, maxlen);

					maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
					_tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), _T(";"), maxlen);
				}
				break;
			}
		}
	}

	//Set date fields

	static const char *mon[12]=
    {
    "Jan","Feb","Mar","Apr","May","Jun",
    "Jul","Aug","Sep","Oct","Nov","Dec"
    };

	//Valid from
	ASN1_UTCTIME *pTime=X509_get_notBefore(pX509);
	if (!pTime)
	{
		pX509_free(pX509);
		return FALSE;
	}

	char *v;
	int gmt = 0;
	int i;
	int y=0, M=0, d=0, h=0, m=0, s=0;

	i = pTime->length;
	v = (char *)pTime->data;

	if (i < 10)
	{
		pX509_free(pX509);
		return FALSE;
	}
	if (v[i-1] == 'Z') gmt=1;
	for (i=0; i<10; i++)
		if ((v[i] > '9') || (v[i] < '0'))
		{
			pX509_free(pX509);
			return FALSE;
		}
	y= (v[0]-'0')*10+(v[1]-'0');
	if (y < 50) y+=100;
	M= (v[2]-'0')*10+(v[3]-'0');
	if ((M > 12) || (M < 1))
	{
		pX509_free(pX509);
		return FALSE;
	}
	d= (v[4]-'0')*10+(v[5]-'0');
	h= (v[6]-'0')*10+(v[7]-'0');
	m=  (v[8]-'0')*10+(v[9]-'0');
	if (	(v[10] >= '0') && (v[10] <= '9') &&
		(v[11] >= '0') && (v[11] <= '9'))
		s=  (v[10]-'0')*10+(v[11]-'0');

	SslCertData.validFrom.y = y+1900;
	SslCertData.validFrom.M = M;
	SslCertData.validFrom.d = d;
	SslCertData.validFrom.h = h;
	SslCertData.validFrom.m = m;
	SslCertData.validFrom.s = s;

	//Valid until
	pTime = X509_get_notAfter(pX509);
	if (!pTime)
	{
		pX509_free(pX509);
		return FALSE;
	}

	gmt = 0;
	i;
	y=0,M=0,d=0,h=0,m=0,s=0;

	i=pTime->length;
	v=(char *)pTime->data;

	if (i < 10)
	{
		pX509_free(pX509);
		return FALSE;
	}
	if (v[i-1] == 'Z') gmt=1;
	for (i=0; i<10; i++)
		if ((v[i] > '9') || (v[i] < '0'))
		{
			pX509_free(pX509);
			return FALSE;
		}
	y= (v[0]-'0')*10+(v[1]-'0');
	if (y < 50) y+=100;
	M= (v[2]-'0')*10+(v[3]-'0');
	if ((M > 12) || (M < 1))
	{
		pX509_free(pX509);
		return FALSE;
	}
	d= (v[4]-'0')*10+(v[5]-'0');
	h= (v[6]-'0')*10+(v[7]-'0');
	m=  (v[8]-'0')*10+(v[9]-'0');
	if (	(v[10] >= '0') && (v[10] <= '9') &&
		(v[11] >= '0') && (v[11] <= '9'))
		s=  (v[10]-'0')*10+(v[11]-'0');

	SslCertData.validUntil.y = y+1900;
	SslCertData.validUntil.M = M;
	SslCertData.validUntil.d = d;
	SslCertData.validUntil.h = h;
	SslCertData.validUntil.m = m;
	SslCertData.validUntil.s = s;

	unsigned int length = 20;
	pX509_digest(pX509, pEVP_sha1(), SslCertData.hash, &length);

	SslCertData.priv_data = m_nSslAsyncNotifyId;

	pX509_free(pX509);

	SslCertData.verificationResult = m_nVerificationResult;
	SslCertData.verificationDepth = m_nVerificationDepth;

	return TRUE;
}

void CAsyncSslSocketLayer::SetNotifyReply(int nID, int nCode, int result)
{
	if (!m_bBlocking)
		return;
	if (nID != m_nSslAsyncNotifyId)
		return;
	if (nCode != SSL_VERIFY_CERT)
		return;

	m_bBlocking = FALSE;

	if (!result)
	{
		m_nNetworkError = WSAECONNABORTED;
		WSASetLastError(WSAECONNABORTED);
		if (!m_bFailureSent)
		{
			m_bFailureSent = TRUE;
			DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, SSL_FAILURE_CERTREJECTED);
		}
		TriggerEvent(FD_CLOSE, 0, TRUE);
		return;
	}
	m_bSslEstablished = TRUE;
	PrintSessionInfo();
	DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_ESTABLISHED);

	TriggerEvents();
}

void CAsyncSslSocketLayer::PrintSessionInfo()
{
	SSL_CIPHER *ciph;
	X509 *cert;

	ciph = pSSL_get_current_cipher(m_ssl);
	char enc[4096] = {0};
	cert=pSSL_get_peer_certificate(m_ssl);

	if (cert != NULL)
	{
		EVP_PKEY *pkey = pX509_get_pubkey(cert);
		if (pkey != NULL)
		{
			if (0)
				;
#ifndef NO_RSA
			else if (pkey->type == EVP_PKEY_RSA && pkey->pkey.rsa != NULL
				&& pkey->pkey.rsa->n != NULL)
				sprintf(enc,	"%d bit RSA", pBN_num_bits(pkey->pkey.rsa->n));
#endif
#ifndef NO_DSA
			else if (pkey->type == EVP_PKEY_DSA && pkey->pkey.dsa != NULL
					&& pkey->pkey.dsa->p != NULL)
				sprintf(enc,	"%d bit DSA", pBN_num_bits(pkey->pkey.dsa->p));
#endif
			pEVP_PKEY_free(pkey);
		}
		pX509_free(cert);
		/* The SSL API does not allow us to look at temporary RSA/DH keys,
		 * otherwise we should print their lengths too */
	}

	char *buffer = new char[4096];
	sprintf(buffer, "Using %s, cipher %s: %s, %s",
			pSSL_get_version(m_ssl),
			pSSL_CIPHER_get_version(ciph),
			pSSL_CIPHER_get_name(ciph),
			enc);
	DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_INFO, 0, buffer);
}

void CAsyncSslSocketLayer::OnConnect(int nErrorCode)
{
	if (m_bUseSSL && nErrorCode)
		TriggerEvent(FD_WRITE, 0);
	TriggerEvent(FD_CONNECT, nErrorCode, TRUE);
}

int CAsyncSslSocketLayer::verify_callback(int preverify_ok, X509_STORE_CTX *ctx)
{
	X509   *err_cert;
    int     err, depth;
    SSL    *ssl;

    err_cert = pX509_STORE_CTX_get_current_cert(ctx);
    err = pX509_STORE_CTX_get_error(ctx);
    depth = pX509_STORE_CTX_get_error_depth(ctx);

    /*
     * Retrieve the pointer to the SSL of the connection currently treated
     * and the application specific data stored into the SSL object.
     */
    ssl = (SSL *)pX509_STORE_CTX_get_ex_data(ctx, pSSL_get_ex_data_X509_STORE_CTX_idx());

	// Lookup CAsyncSslSocketLayer instance
	CAsyncSslSocketLayer *pLayer = 0;
	m_sCriticalSection.Lock();
	t_SslLayerList *cur = m_pSslLayerList;
	while (cur)
	{
		if (cur->pLayer->m_ssl == ssl)
			break;
		cur = cur->pNext;
	}
	if (!cur)
	{
		m_sCriticalSection.Unlock();
		MessageBox(0, _T("Can't lookup SSL session!"), _T("Critical error"), MB_ICONEXCLAMATION);
		return 1;
	}
	else
		pLayer = cur->pLayer;
	m_sCriticalSection.Unlock();

    /*
     * Catch a too long certificate chain. The depth limit set using
     * SSL_CTX_set_verify_depth() is by purpose set to "limit+1" so
     * that whenever the "depth>verify_depth" condition is met, we
     * have violated the limit and want to log this error condition.
     * We must do it here, because the CHAIN_TOO_LONG error would not
     * be found explicitly; only errors introduced by cutting off the
     * additional certificates would be logged.
     */
    if (depth > 10) {//mydata->verify_depth) {
        preverify_ok = 0;
        err = X509_V_ERR_CERT_CHAIN_TOO_LONG;
        pX509_STORE_CTX_set_error(ctx, err);
    }

	if (!preverify_ok)
	{
		if (!pLayer->m_nVerificationResult)
		{
			pLayer->m_nVerificationDepth = depth;
			pLayer->m_nVerificationResult = err;
		}
	}
	return 1;
}

BOOL CAsyncSslSocketLayer::SetCertStorage(CString file)
{
	m_CertStorage = file;
	return TRUE;
}

void CAsyncSslSocketLayer::OnClose(int nErrorCode)
{
	m_onCloseCalled = true;
	if (m_bUseSSL && pBIO_ctrl)
	{
		if (pBIO_ctrl(m_sslbio, BIO_CTRL_PENDING, 0, NULL) > 0)
		{
			TriggerEvents();
		}
		else TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
	}
	else
		TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
}

void CAsyncSslSocketLayer::PrintLastErrorMsg()
{
	int err = pERR_get_error();
	while (err)
	{
		char *buffer = new char[512];
		pERR_error_string(err, buffer);
		err = pERR_get_error();
		DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERBOSE_WARNING, 0, buffer);
	}
}

bool CAsyncSslSocketLayer::CreateSslCertificate(LPCTSTR filename, int bits, unsigned char* country, unsigned char* state,
			unsigned char* locality, unsigned char* organization, unsigned char* unit, unsigned char* cname,
			unsigned char *email, CString& err)
{
	// Certificate valid for a year
	int days = 365;

	CAsyncSslSocketLayer layer;
	if (layer.InitSSL())
	{
		err = _T("Failed to initialize SSL library");
		return false;
	}

	X509 *x;
	EVP_PKEY *pk;
	RSA *rsa;
	X509_NAME *name = NULL;
	
	if ((pk = pEVP_PKEY_new()) == NULL)
	{
		err = _T("Could not create key object");
		return false;
	}

	if ((x = pX509_new()) == NULL)
	{
		err = _T("Could not create certificate object");
		return false;
	}

	rsa = pRSA_generate_key(bits, RSA_F4, 0/*callback*/, NULL);
	
	if (!pEVP_PKEY_assign(pk, EVP_PKEY_RSA, (char *)(rsa)))
	{
		err = _T("Failed to assign rsa key to key object");
		return false;
	}

	rsa = NULL;

	pX509_set_version(x,2);
	pASN1_INTEGER_set(pX509_get_serialNumber(x), 0/*serial*/);
	pX509_gmtime_adj(X509_get_notBefore(x),0);
	pX509_gmtime_adj(X509_get_notAfter(x),(long)60*60*24*days);
	pX509_set_pubkey(x,pk);

	name = pX509_get_subject_name(x);

	/* This function creates and adds the entry, working out the
	 * correct string type and performing checks on its length.
	 * Normally we'd check the return value for errors...
	 */
	pX509_NAME_add_entry_by_txt(name, "CN",
				MBSTRING_ASC, cname, -1, -1, 0);
	pX509_NAME_add_entry_by_txt(name, "C",
				MBSTRING_ASC, country, -1, -1, 0);
	pX509_NAME_add_entry_by_txt(name, "ST",
				MBSTRING_ASC, state, -1, -1, 0);
	pX509_NAME_add_entry_by_txt(name, "L",
				MBSTRING_ASC, locality, -1, -1, 0);
	pX509_NAME_add_entry_by_txt(name, "O",
				MBSTRING_ASC, organization, -1, -1, 0);
	pX509_NAME_add_entry_by_txt(name, "OU",
				MBSTRING_ASC, unit, -1, -1, 0);
	pX509_NAME_add_entry_by_NID(name, NID_pkcs9_emailAddress,
				MBSTRING_ASC, email, -1, -1, 0);

	/* Its self signed so set the issuer name to be the same as the
 	 * subject.
	 */
	pX509_set_issuer_name(x,name);

	if (!pX509_sign(x, pk, pEVP_sha1()))
	{
		err = _T("Failed to sign certificate");
		return false;
	}

	// Write key and certificate to file
	// We use a memory bio, since the OpenSSL functions accepting a filepointer 
	// do crash for no obvious reason.

#ifndef _UNICODE
	FILE* file = fopen(filename, "w+");
#else
	FILE* file = _wfopen(filename, _T("w+"));
#endif

	if (!file)
	{
		err = _T("Failed to open output file");
		return false;
	}

	BIO* bio = pBIO_new(pBIO_s_mem());
#ifdef MPEXT
	pPEM_ASN1_write_bio((i2d_of_void*)pi2d_PrivateKey, (((pk)->type == EVP_PKEY_DSA)?PEM_STRING_DSA:PEM_STRING_RSA), bio, (char *)pk, NULL, NULL, 0, NULL, NULL);
	pPEM_ASN1_write_bio((i2d_of_void*)pi2d_X509, PEM_STRING_X509, bio, (char *)x, NULL, NULL, 0, NULL, NULL);
#else
	pPEM_ASN1_write_bio((int (*)())pi2d_PrivateKey, (((pk)->type == EVP_PKEY_DSA)?PEM_STRING_DSA:PEM_STRING_RSA), bio, (char *)pk, NULL, NULL, 0, NULL, NULL);
	pPEM_ASN1_write_bio((int (*)())pi2d_X509, PEM_STRING_X509, bio, (char *)x, NULL, NULL, 0, NULL, NULL);
#endif
	
	char buffer[1001];
	int len;
	while ((len = pBIO_read(bio, buffer, 1000)) > 0)
	{
		buffer[len] = 0;
		fprintf(file, buffer);
	}

	fclose(file);

	pX509_free(x);
	pEVP_PKEY_free(pk);

	pBIO_free(bio);

	layer.UnloadSSL();

	return true;
}

int CAsyncSslSocketLayer::SetCertKeyFile(const char* cert, const char* key, const char* pass, CString* error /*=0*/)
{
	int res = InitSSL();
	if (res)
		return res;

	m_sCriticalSection.Lock();
	
	if (!m_ssl_ctx)
	{
		// Create new context
		if (!(m_ssl_ctx = pSSL_CTX_new( pSSLv23_method())))
		{
			m_sCriticalSection.Unlock();
			return SSL_FAILURE_INITSSL;
		}
		m_contextRefCount[m_ssl_ctx] = 1;
	}

	pSSL_CTX_set_default_passwd_cb(m_ssl_ctx, pem_passwd_cb);
	pSSL_CTX_set_default_passwd_cb_userdata(m_ssl_ctx, this);

	if (pass)
	{
		size_t len = strlen(pass);
		m_pKeyPassword = new char[len + 1];
		strcpy(m_pKeyPassword, pass);
	}
	else
	{
		delete [] m_pKeyPassword;
		m_pKeyPassword = 0;
	}

	if (pSSL_CTX_use_certificate_chain_file(m_ssl_ctx, cert) <= 0)
	//if (pSSL_CTX_use_certificate_file(m_ssl_ctx, cert, SSL_FILETYPE_PEM) <= 0)
	{
		if (error)
			*error = _T("Could not load certificate file.");
		m_sCriticalSection.Unlock();
		return SSL_FAILURE_VERIFYCERT;
	}

	if (pSSL_CTX_use_PrivateKey_file(m_ssl_ctx, key, SSL_FILETYPE_PEM) <= 0)
	{
		if (error)
			*error = _T("Could not load key file.");
		m_sCriticalSection.Unlock();
		return SSL_FAILURE_VERIFYCERT;
	}

	if (!pSSL_CTX_check_private_key(m_ssl_ctx))
	{
		if (error)
			*error = _T("Private key does not match the certificate public key.");
		m_sCriticalSection.Unlock();
		return SSL_FAILURE_VERIFYCERT;
	}

	m_sCriticalSection.Unlock();

	return 0;
}

int CAsyncSslSocketLayer::SendRaw(const void* lpBuf, int nBufLen, int nFlags)
{
	if (!m_bUseSSL)
	{
		SetLastError(WSANOTINITIALISED);
		return SOCKET_ERROR;
	}

	if (!lpBuf)
		return 0;
	
	if (m_nNetworkError)
	{
		SetLastError(m_nNetworkError);
		return SOCKET_ERROR;
	}
	if (m_nShutDown)
	{
		SetLastError(WSAESHUTDOWN);
		return SOCKET_ERROR;
	}
	if (m_nNetworkSendBufferLen)
	{
		SetLastError(WSAEINPROGRESS);
		return SOCKET_ERROR;
	}
	if (!nBufLen)
		return 0;

	if (m_nNetworkSendBufferMaxLen < nBufLen)
		m_nNetworkSendBufferMaxLen = nBufLen;
	delete [] m_pNetworkSendBuffer;
	m_pNetworkSendBuffer = new char[m_nNetworkSendBufferMaxLen];
	memcpy(m_pNetworkSendBuffer, lpBuf, nBufLen);
	m_nNetworkSendBufferLen = nBufLen;
	TriggerEvent(FD_WRITE, 0);

	return nBufLen;
}

void CAsyncSslSocketLayer::TriggerEvents()
{
	if (pBIO_ctrl_pending(m_nbio) > 0)
	{
		if (m_mayTriggerWrite)
		{
			m_mayTriggerWrite = false;
			TriggerEvent(FD_WRITE, 0);
		}
	}
	else if (!m_nNetworkSendBufferLen && m_bSslEstablished && !m_pRetrySendBuffer)
	{
		if (pBIO_ctrl_get_write_guarantee(m_sslbio) > 0 && m_mayTriggerWriteUp)
		{
			m_mayTriggerWriteUp = false;
			TriggerEvent(FD_WRITE, 0, TRUE);
		}
	}

	if (m_bSslEstablished && pBIO_ctrl_pending(m_sslbio) > 0)
	{
		if (m_mayTriggerReadUp && !m_bBlocking)
		{
			m_mayTriggerReadUp = false;
			TriggerEvent(FD_READ, 0, TRUE);
		}
	}
	else
	{
		if (pBIO_ctrl_get_write_guarantee(m_nbio) > 0 && m_mayTriggerRead)
		{
			m_mayTriggerRead = false;
			TriggerEvent(FD_READ, 0);
		}
	}

	if (m_onCloseCalled && m_bSslEstablished)
	{
		if (pBIO_ctrl_pending(m_sslbio) <= 0)
			TriggerEvent(FD_CLOSE, 0, TRUE);
	}
}

int CAsyncSslSocketLayer::pem_passwd_cb(char *buf, int size, int rwflag, void *userdata)
{
	CAsyncSslSocketLayer* pThis = (CAsyncSslSocketLayer*)userdata;

	if (!pThis || !pThis->m_pKeyPassword)
		return 0;

	int len = strlen(pThis->m_pKeyPassword);
	if (len >= size)
		len = size - 1;

	memcpy(buf, pThis->m_pKeyPassword, len);
	buf[len] = 0;

	return len;
}
