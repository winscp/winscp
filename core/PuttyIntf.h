//---------------------------------------------------------------------------
#ifndef PuttyIntfH
#define PuttyIntfH

#ifdef PUTTY_PUTTY_H
  #error Should be included sooner than "Putty.h" !!
#endif
//---------------------------------------------------------------------------
struct charset_spec;
#include "charset\Charset.h"
//---------------------------------------------------------------------------
extern "C"
{
  void sk_init();
  int select_result(WPARAM wParam, LPARAM lParam);
  void random_save_seed(void);
  int verify_host_key(char * hostname, int port, char * keytype, char * key);
  void store_host_key(char * hostname, int port, char * keytype, char * key);
  void * saferealloc(void * ptr, size_t size);
  void * safemalloc(size_t size);
  void safefree(void * ptr);
  void noise_regular(void);
  void * log_init(void * frontend, void * cfg);
  void putty_mungestr(const char * in, char * out);
  void putty_unmungestr(const char * in, char * out, int outlen);
  char * seedpath_ptr();
  int seedpath_size();
  void putty_get_seedpath(void);
  void agent_schedule_callback(void (* callback)(void *, void *, int),
    void * callback_ctx, void * data, int len);
  void read_utf8(charset_spec const *charset, long int input_chr,
    charset_state *state, void (*emit)(void *ctx, long int output), void *emitctx);
  // -------------

  void ssh_close(void * handle);
  int get_ssh_version(void * handle);
  int get_ssh1_compressing(void * handle);
  const struct ssh_cipher * get_cipher(void * handle);
  const struct ssh2_cipher * get_cscipher(void * handle);
  const struct ssh2_cipher * get_sccipher(void * handle);
  const struct ssh_compress * get_cscomp(void * handle);
  const struct ssh_compress * get_sccomp(void * handle);
  int get_ssh_state(void * handle);
  int get_ssh_state_closed(void * handle);
  int get_ssh_exitcode(void * handle);
  int ssh_fallback_cmd(void * handle);
  unsigned int ssh2_remmaxpkt(void * handle);
  unsigned int ssh2_remwindow(void * handle);

  // -------------
  int from_backend(void * frontend, int is_stderr, char * data, int datalen);

  // from ssh.h for key generation
  struct RSAKey;
  struct dss_key;
  typedef void (* progfn_t) (void * param, int action, int phase, int progress);
  void random_add_heavynoise(void * noise, int length);
  int dsa_generate(struct dss_key * key, int bits, progfn_t pfn, void * pfnparam);
  int rsa_generate(struct RSAKey * key, int bits, progfn_t pfn, void * pfnparam);

  void rsa_fingerprint(char * str, int len, struct RSAKey * key);
  void base64_encode_atom(unsigned char * data, int n, char * out);
  int ssh2_save_userkey(char * filename, struct ssh2_userkey * key, char * passphrase);
  int export_ssh2(char * filename, int type, struct ssh2_userkey * key, char * passphrase);
  int saversakey(char * filename, struct RSAKey * key, char * passphrase);

  // -------------
  void SSHVerifyHostKey(void * frontend, char * host, int port, char * keytype,
    char * keystr, char * fingerprint);
  void SSHLogEvent(void * frontend, char * string);
  void SSHConnectionFatal(void * frontend, char * string);
  void SSHFatalError(char * string);
  void SSHAskCipher(void * frontend, char * CipherName, int CipherType);
  void SSHOldKeyfileWarning(void);
  long RegOpenWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
  long RegCreateWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
}
//---------------------------------------------------------------------------
extern const struct ssh_compress ssh_zlib;

extern const struct ssh_cipher ssh_3des;
extern const struct ssh_cipher ssh_des;
extern const struct ssh_cipher ssh_blowfish_ssh1;
extern const struct ssh2_ciphers ssh2_3des;
extern const struct ssh2_ciphers ssh2_des;
extern const struct ssh2_ciphers ssh2_aes;
extern const struct ssh2_ciphers ssh2_blowfish;
//---------------------------------------------------------------------------
#include "Putty.h"
#include <SSH.h>
//---------------------------------------------------------------------------
#endif
