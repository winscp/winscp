//---------------------------------------------------------------------------
#ifndef PuttyIntfH
#define PuttyIntfH

#ifdef PUTTY_PUTTY_H
  #error Should be included sooner than "Putty.h" !!
#endif
//---------------------------------------------------------------------------
#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
//---------------------------------------------------------------------------
extern "C"
{
  void sk_init();
  int select_result(WPARAM wParam, LPARAM lParam);
  void random_save_seed(void);
  int verify_host_key(char *hostname, int port, char *keytype, char *key);
  void store_host_key(char *hostname, int port, char *keytype, char *key);
  void *saferealloc(void *ptr, size_t size);
  void *safemalloc(size_t size);
  void safefree(void *ptr);
  void noise_regular(void);
  void putty_mungestr(char *in, char *out);
  void putty_unmungestr(char *in, char *out, int outlen);
  char *seedpath_ptr();
  int seedpath_size();
  void putty_get_seedpath(void);
  // -------------

  void ssh_close();
  int get_ssh_version();
  int get_ssh1_compressing();
  struct ssh_cipher *get_cipher();
  struct ssh2_cipher *get_cscipher();
  struct ssh2_cipher *get_sccipher();
  struct ssh_compress *get_cscomp();
  struct ssh_compress *get_sccomp();
  int get_ssh_state();
  int get_ssh_state_closed();
  int get_ssh_exitcode();

  // -------------
  char *do_select(SOCKET skt, int startup);
  void fatalbox(char *fmt, ...);
  int from_backend(int is_stderr, char *data, int datalen);

  // from ssh.h for key generation
  struct RSAKey;
  struct dss_key;
  typedef void (*progfn_t) (void *param, int action, int phase, int progress);
  void random_add_heavynoise(void *noise, int length);
  int dsa_generate(struct dss_key *key, int bits, progfn_t pfn, void *pfnparam);
  int rsa_generate(struct RSAKey *key, int bits, progfn_t pfn, void *pfnparam);

  void rsa_fingerprint(char *str, int len, struct RSAKey *key);
  void base64_encode_atom(unsigned char *data, int n, char *out);
  int ssh2_save_userkey(char *filename, struct ssh2_userkey *key, char *passphrase);
  int export_ssh2(char *filename, int type, struct ssh2_userkey *key, char *passphrase);
  int saversakey(char *filename, struct RSAKey *key, char *passphrase);

  // -------------
  void SSHVerifyHostKey(char *host, int port, char *keytype, char *keystr, char *fingerprint);
  void SSHLogEvent(char *string);
  void SSHFatalError(char *string);
  //void SSHGotHostKey(void);
  void SSHAskCipher(char * CipherName, int CipherType);
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
