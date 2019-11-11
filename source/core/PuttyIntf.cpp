//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#define PUTTY_DO_GLOBALS
#include "PuttyIntf.h"
#include "Interface.h"
#include "SecureShell.h"
#include "Exceptions.h"
#include "CoreMain.h"
#include "TextsCore.h"
#include <StrUtils.hpp>
#include <Soap.EncdDecd.hpp>
//---------------------------------------------------------------------------
char sshver[50];
extern const char commitid[] = "";
const int platform_uses_x11_unix_by_default = TRUE;
CRITICAL_SECTION putty_section;
bool SaveRandomSeed;
char appname_[50];
const char *const appname = appname_;
extern const int share_can_be_downstream = FALSE;
extern const int share_can_be_upstream = FALSE;
//---------------------------------------------------------------------------
extern "C"
{
#include <winstuff.h>
}
const UnicodeString OriginalPuttyRegistryStorageKey(_T(PUTTY_REG_POS));
const UnicodeString KittyRegistryStorageKey(L"Software\\9bis.com\\KiTTY");
const UnicodeString OriginalPuttyExecutable("putty.exe");
const UnicodeString KittyExecutable("kitty.exe");
const UnicodeString PuttyKeyExt(L"ppk");
//---------------------------------------------------------------------------
void __fastcall PuttyInitialize()
{
  SaveRandomSeed = true;

  InitializeCriticalSection(&putty_section);

  // make sure random generator is initialised, so random_save_seed()
  // in destructor can proceed
  random_ref();

  flags = FLAG_VERBOSE | FLAG_SYNCAGENT; // verbose log

  sk_init();

  AnsiString VersionString = AnsiString(SshVersionString());
  DebugAssert(!VersionString.IsEmpty() && (static_cast<size_t>(VersionString.Length()) < LENOF(sshver)));
  strcpy(sshver, VersionString.c_str());
  AnsiString AppName = AnsiString(AppNameString());
  DebugAssert(!AppName.IsEmpty() && (static_cast<size_t>(AppName.Length()) < LENOF(appname_)));
  strcpy(appname_, AppName.c_str());
}
//---------------------------------------------------------------------------
void __fastcall PuttyFinalize()
{
  if (SaveRandomSeed)
  {
    random_save_seed();
  }
  random_unref();

  sk_cleanup();
  win_misc_cleanup();
  win_secur_cleanup();
  ec_cleanup();
  DeleteCriticalSection(&putty_section);
}
//---------------------------------------------------------------------------
void __fastcall DontSaveRandomSeed()
{
  SaveRandomSeed = false;
}
//---------------------------------------------------------------------------
extern "C" char * do_select(Plug plug, SOCKET skt, int startup)
{
  void * frontend;

  if (!is_ssh(plug) && !is_pfwd(plug))
  {
    // If it is not SSH/PFwd plug, then it must be Proxy plug.
    // Get SSH/PFwd plug which it wraps.
    Proxy_Socket ProxySocket = ((Proxy_Plug)plug)->proxy_socket;
    plug = ProxySocket->plug;
  }

  bool pfwd = is_pfwd(plug);
  if (pfwd)
  {
    plug = (Plug)get_pfwd_backend(plug);
  }

  frontend = get_ssh_frontend(plug);
  DebugAssert(frontend);

  TSecureShell * SecureShell = reinterpret_cast<TSecureShell*>(frontend);
  if (!pfwd)
  {
    SecureShell->UpdateSocket(skt, startup);
  }
  else
  {
    SecureShell->UpdatePortFwdSocket(skt, startup);
  }

  return NULL;
}
//---------------------------------------------------------------------------
int from_backend(void * frontend, int is_stderr, const char * data, int datalen)
{
  DebugAssert(frontend);
  if (is_stderr >= 0)
  {
    DebugAssert((is_stderr == 0) || (is_stderr == 1));
    ((TSecureShell *)frontend)->FromBackend((is_stderr == 1), reinterpret_cast<const unsigned char *>(data), datalen);
  }
  else
  {
    DebugAssert(is_stderr == -1);
    ((TSecureShell *)frontend)->CWrite(data, datalen);
  }
  return 0;
}
//---------------------------------------------------------------------------
int from_backend_untrusted(void * /*frontend*/, const char * /*data*/, int /*len*/)
{
  // currently used with authentication banner only,
  // for which we have own interface display_banner
  return 0;
}
//---------------------------------------------------------------------------
int from_backend_eof(void * /*frontend*/)
{
  return FALSE;
}
//---------------------------------------------------------------------------
int get_userpass_input(prompts_t * p, const unsigned char * /*in*/, int /*inlen*/)
{
  DebugAssert(p != NULL);
  TSecureShell * SecureShell = reinterpret_cast<TSecureShell *>(p->frontend);
  DebugAssert(SecureShell != NULL);

  int Result;
  TStrings * Prompts = new TStringList();
  TStrings * Results = new TStringList();
  try
  {
    UnicodeString Name = UTF8ToString(p->name);
    UnicodeString AName = Name;
    TPromptKind PromptKind = SecureShell->IdentifyPromptKind(AName);
    bool UTF8Prompt = (PromptKind != pkPassphrase);

    for (int Index = 0; Index < int(p->n_prompts); Index++)
    {
      prompt_t * Prompt = p->prompts[Index];
      UnicodeString S;
      if (UTF8Prompt)
      {
        S = UTF8ToString(Prompt->prompt);
      }
      else
      {
        S = UnicodeString(AnsiString(Prompt->prompt));
      }
      Prompts->AddObject(S, (TObject *)(FLAGMASK(Prompt->echo, pupEcho)));
      // this fails, when new passwords do not match on change password prompt,
      // and putty retries the prompt
      DebugAssert(Prompt->resultsize == 0);
      Results->Add(L"");
    }

    UnicodeString Instructions = UTF8ToString(p->instruction);
    if (SecureShell->PromptUser(p->to_server, Name, p->name_reqd,
          Instructions, p->instr_reqd, Prompts, Results))
    {
      for (int Index = 0; Index < int(p->n_prompts); Index++)
      {
        prompt_t * Prompt = p->prompts[Index];
        RawByteString S;
        if (UTF8Prompt)
        {
          S = RawByteString(UTF8String(Results->Strings[Index]));
        }
        else
        {
          S = RawByteString(AnsiString(Results->Strings[Index]));
        }
        prompt_set_result(Prompt, S.c_str());
      }
      Result = 1;
    }
    else
    {
      Result = 0;
    }
  }
  __finally
  {
    delete Prompts;
    delete Results;
  }

  return Result;
}
//---------------------------------------------------------------------------
char * get_ttymode(void * /*frontend*/, const char * /*mode*/)
{
  // should never happen when Config.nopty == TRUE
  DebugFail();
  return NULL;
}
//---------------------------------------------------------------------------
void logevent(void * frontend, const char * string)
{
  // Frontend maybe NULL here
  if (frontend != NULL)
  {
    ((TSecureShell *)frontend)->PuttyLogEvent(string);
  }
}
//---------------------------------------------------------------------------
void connection_fatal(void * frontend, const char * fmt, ...)
{
  va_list Param;
  char Buf[200];
  va_start(Param, fmt);
  vsnprintf(Buf, LENOF(Buf), fmt, Param); \
  Buf[LENOF(Buf) - 1] = '\0'; \
  va_end(Param);

  DebugAssert(frontend != NULL);
  ((TSecureShell *)frontend)->PuttyFatalError(Buf);
}
//---------------------------------------------------------------------------
int verify_ssh_host_key(void * frontend, char * host, int port, const char * keytype,
  char * keystr, char * fingerprint, void (*/*callback*/)(void * ctx, int result),
  void * /*ctx*/)
{
  DebugAssert(frontend != NULL);
  static_cast<TSecureShell *>(frontend)->VerifyHostKey(host, port, keytype, keystr, fingerprint);

  // We should return 0 when key was not confirmed, we throw exception instead.
  return 1;
}
//---------------------------------------------------------------------------
int have_ssh_host_key(void * frontend, const char * hostname, int port,
  const char * keytype)
{
  DebugAssert(frontend != NULL);
  return static_cast<TSecureShell *>(frontend)->HaveHostKey(hostname, port, keytype) ? 1 : 0;
}
//---------------------------------------------------------------------------
int askalg(void * frontend, const char * algtype, const char * algname,
  void (*/*callback*/)(void * ctx, int result), void * /*ctx*/)
{
  DebugAssert(frontend != NULL);
  ((TSecureShell *)frontend)->AskAlg(algtype, algname);

  // We should return 0 when alg was not confirmed, we throw exception instead.
  return 1;
}
//---------------------------------------------------------------------------
int askhk(void * /*frontend*/, const char * /*algname*/, const char * /*betteralgs*/,
  void (*/*callback*/)(void *ctx, int result), void * /*ctx*/)
{
  return 1;
}
//---------------------------------------------------------------------------
void old_keyfile_warning(void)
{
  // no reference to TSecureShell instance available
}
//---------------------------------------------------------------------------
void display_banner(void * frontend, const char * banner, int size)
{
  DebugAssert(frontend);
  UnicodeString Banner(UTF8String(banner, size));
  ((TSecureShell *)frontend)->DisplayBanner(Banner);
}
//---------------------------------------------------------------------------
static void SSHFatalError(const char * Format, va_list Param)
{
  char Buf[200];
  vsnprintf(Buf, LENOF(Buf), Format, Param);
  Buf[LENOF(Buf) - 1] = '\0';

  // Only few calls from putty\winnet.c might be connected with specific
  // TSecureShell. Otherwise called only for really fatal errors
  // like 'out of memory' from putty\ssh.c.
  throw ESshFatal(NULL, Buf);
}
//---------------------------------------------------------------------------
void fatalbox(const char * fmt, ...)
{
  va_list Param;
  va_start(Param, fmt);
  SSHFatalError(fmt, Param);
  va_end(Param);
}
//---------------------------------------------------------------------------
void modalfatalbox(const char * fmt, ...)
{
  va_list Param;
  va_start(Param, fmt);
  SSHFatalError(fmt, Param);
  va_end(Param);
}
//---------------------------------------------------------------------------
void nonfatal(const char * fmt, ...)
{
  va_list Param;
  va_start(Param, fmt);
  SSHFatalError(fmt, Param);
  va_end(Param);
}
//---------------------------------------------------------------------------
void cleanup_exit(int /*code*/)
{
  throw ESshFatal(NULL, "");
}
//---------------------------------------------------------------------------
int askappend(void * /*frontend*/, Filename * /*filename*/,
  void (*/*callback*/)(void * ctx, int result), void * /*ctx*/)
{
  // this is called from logging.c of putty, which is never used with WinSCP
  DebugFail();
  return 0;
}
//---------------------------------------------------------------------------
void ldisc_echoedit_update(void * /*handle*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void agent_schedule_callback(void (* /*callback*/)(void *, void *, int),
  void * /*callback_ctx*/, void * /*data*/, int /*len*/)
{
  DebugFail();
}
//---------------------------------------------------------------------------
void notify_remote_exit(void * /*frontend*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void update_specials_menu(void * /*frontend*/)
{
  // nothing
}
//---------------------------------------------------------------------------
unsigned long schedule_timer(int ticks, timer_fn_t /*fn*/, void * /*ctx*/)
{
  return ticks + GetTickCount();
}
//---------------------------------------------------------------------------
void expire_timer_context(void * /*ctx*/)
{
  // nothing
}
//---------------------------------------------------------------------------
Pinger pinger_new(Conf * /*conf*/, Backend * /*back*/, void * /*backhandle*/)
{
  return NULL;
}
//---------------------------------------------------------------------------
void pinger_reconfig(Pinger /*pinger*/, Conf * /*oldconf*/, Conf * /*newconf*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void pinger_free(Pinger /*pinger*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void set_busy_status(void * /*frontend*/, int /*status*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void platform_get_x11_auth(struct X11Display * /*display*/, Conf * /*conf*/)
{
  // nothing, therefore no auth.
}
//---------------------------------------------------------------------------
// Based on PuTTY's settings.c
char * get_remote_username(Conf * conf)
{
  char * username = conf_get_str(conf, CONF_username);
  char * result;
  if (*username)
  {
    result = dupstr(username);
  }
  else
  {
    result = NULL;
  }
  return result;
}
//---------------------------------------------------------------------------
static long OpenWinSCPKey(HKEY Key, const char * SubKey, HKEY * Result, bool CanCreate)
{
  long R;
  DebugAssert(Configuration != NULL);

  DebugAssert(Key == HKEY_CURRENT_USER);
  DebugUsedParam(Key);

  UnicodeString RegKey = SubKey;
  int PuttyKeyLen = OriginalPuttyRegistryStorageKey.Length();
  DebugAssert(RegKey.SubString(1, PuttyKeyLen) == OriginalPuttyRegistryStorageKey);
  RegKey = RegKey.SubString(PuttyKeyLen + 1, RegKey.Length() - PuttyKeyLen);
  if (!RegKey.IsEmpty())
  {
    DebugAssert(RegKey[1] == L'\\');
    RegKey.Delete(1, 1);
  }

  if (RegKey.IsEmpty())
  {
    *Result = static_cast<HKEY>(NULL);
    R = ERROR_SUCCESS;
  }
  else
  {
    // we expect this to be called only from verify_host_key() or store_host_key()
    DebugAssert(RegKey == L"SshHostKeys");

    THierarchicalStorage * Storage = Configuration->CreateConfigStorage();
    Storage->AccessMode = (CanCreate ? smReadWrite : smRead);
    if (Storage->OpenSubKey(RegKey, CanCreate))
    {
      *Result = reinterpret_cast<HKEY>(Storage);
      R = ERROR_SUCCESS;
    }
    else
    {
      delete Storage;
      R = ERROR_CANTOPEN;
    }
  }

  return R;
}
//---------------------------------------------------------------------------
long reg_open_winscp_key(HKEY Key, const char * SubKey, HKEY * Result)
{
  return OpenWinSCPKey(Key, SubKey, Result, false);
}
//---------------------------------------------------------------------------
long reg_create_winscp_key(HKEY Key, const char * SubKey, HKEY * Result)
{
  return OpenWinSCPKey(Key, SubKey, Result, true);
}
//---------------------------------------------------------------------------
long reg_query_winscp_value_ex(HKEY Key, const char * ValueName, unsigned long * /*Reserved*/,
  unsigned long * Type, unsigned char * Data, unsigned long * DataSize)
{
  long R;
  DebugAssert(Configuration != NULL);

  THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
  AnsiString Value;
  if (Storage == NULL)
  {
    if (UnicodeString(ValueName) == L"RandSeedFile")
    {
      Value = AnsiString(Configuration->RandomSeedFileName);
      R = ERROR_SUCCESS;
    }
    else
    {
      DebugFail();
      R = ERROR_READ_FAULT;
    }
  }
  else
  {
    if (Storage->ValueExists(ValueName))
    {
      Value = AnsiString(Storage->ReadStringRaw(ValueName, L""));
      R = ERROR_SUCCESS;
    }
    else
    {
      R = ERROR_READ_FAULT;
    }
  }

  if (R == ERROR_SUCCESS)
  {
    DebugAssert(Type != NULL);
    *Type = REG_SZ;
    char * DataStr = reinterpret_cast<char *>(Data);
    strncpy(DataStr, Value.c_str(), *DataSize);
    DataStr[*DataSize - 1] = '\0';
    *DataSize = strlen(DataStr);
  }

  return R;
}
//---------------------------------------------------------------------------
long reg_set_winscp_value_ex(HKEY Key, const char * ValueName, unsigned long /*Reserved*/,
  unsigned long Type, const unsigned char * Data, unsigned long DataSize)
{
  DebugAssert(Configuration != NULL);

  DebugAssert(Type == REG_SZ);
  DebugUsedParam(Type);
  THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
  DebugAssert(Storage != NULL);
  if (Storage != NULL)
  {
    UnicodeString Value(reinterpret_cast<const char*>(Data), DataSize - 1);
    Storage->WriteStringRaw(ValueName, Value);
  }

  return ERROR_SUCCESS;
}
//---------------------------------------------------------------------------
long reg_close_winscp_key(HKEY Key)
{
  DebugAssert(Configuration != NULL);

  THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
  if (Storage != NULL)
  {
    delete Storage;
  }

  return ERROR_SUCCESS;
}
//---------------------------------------------------------------------------
TKeyType KeyType(UnicodeString FileName)
{
  DebugAssert(ktUnopenable == SSH_KEYTYPE_UNOPENABLE);
  DebugAssert(ktSSHCom == SSH_KEYTYPE_SSHCOM);
  DebugAssert(ktSSH2PublicOpenSSH == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH);
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_str(UtfFileName.c_str());
  TKeyType Result = (TKeyType)key_type(KeyFile);
  filename_free(KeyFile);
  return Result;
}
//---------------------------------------------------------------------------
bool IsKeyEncrypted(TKeyType KeyType, const UnicodeString & FileName, UnicodeString & Comment)
{
  UTF8String UtfFileName = UTF8String(FileName);
  bool Result;
  char * CommentStr = NULL;
  Filename * KeyFile = filename_from_str(UtfFileName.c_str());
  try
  {
    switch (KeyType)
    {
      case ktSSH2:
        Result = (ssh2_userkey_encrypted(KeyFile, &CommentStr) != 0);
        break;

      case ktOpenSSHPEM:
      case ktOpenSSHNew:
      case ktSSHCom:
        Result = (import_encrypted(KeyFile, KeyType, &CommentStr) != NULL);
        break;

      default:
        DebugFail();
        Result = false;
        break;
    }
  }
  __finally
  {
    filename_free(KeyFile);
  }

  if (CommentStr != NULL)
  {
    Comment = UnicodeString(AnsiString(CommentStr));
    // ktOpenSSH has no comment, PuTTY defaults to file path
    if (Comment == FileName)
    {
      Comment = ExtractFileName(FileName);
    }
    sfree(CommentStr);
  }

  return Result;
}
//---------------------------------------------------------------------------
TPrivateKey * LoadKey(TKeyType KeyType, const UnicodeString & FileName, const UnicodeString & Passphrase)
{
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_str(UtfFileName.c_str());
  struct ssh2_userkey * Ssh2Key = NULL;
  const char * ErrorStr = NULL;
  AnsiString AnsiPassphrase = Passphrase;
  try
  {
    switch (KeyType)
    {
      case ktSSH2:
        Ssh2Key = ssh2_load_userkey(KeyFile, AnsiPassphrase.c_str(), &ErrorStr);
        break;

      case ktOpenSSHPEM:
      case ktOpenSSHNew:
      case ktSSHCom:
        Ssh2Key = import_ssh2(KeyFile, KeyType, AnsiPassphrase.c_str(), &ErrorStr);
        break;

      default:
        DebugFail();
        break;
    }
  }
  __finally
  {
    Shred(AnsiPassphrase);
    filename_free(KeyFile);
  }

  if (Ssh2Key == NULL)
  {
    UnicodeString Error = AnsiString(ErrorStr);
    // While theoretically we may get "unable to open key file" and
    // so we should check system error code,
    // we actully never get here unless we call KeyType previously
    // and handle ktUnopenable accordingly.
    throw Exception(Error);
  }
  else if (Ssh2Key == SSH2_WRONG_PASSPHRASE)
  {
    throw Exception(LoadStr(AUTH_TRANSL_WRONG_PASSPHRASE));
  }

  return reinterpret_cast<TPrivateKey *>(Ssh2Key);
}
//---------------------------------------------------------------------------
void ChangeKeyComment(TPrivateKey * PrivateKey, const UnicodeString & Comment)
{
  AnsiString AnsiComment(Comment);
  struct ssh2_userkey * Ssh2Key = reinterpret_cast<struct ssh2_userkey *>(PrivateKey);
  sfree(Ssh2Key->comment);
  Ssh2Key->comment = dupstr(AnsiComment.c_str());
}
//---------------------------------------------------------------------------
void SaveKey(TKeyType KeyType, const UnicodeString & FileName,
  const UnicodeString & Passphrase, TPrivateKey * PrivateKey)
{
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_str(UtfFileName.c_str());
  try
  {
    struct ssh2_userkey * Ssh2Key = reinterpret_cast<struct ssh2_userkey *>(PrivateKey);
    AnsiString AnsiPassphrase = Passphrase;
    char * PassphrasePtr = (AnsiPassphrase.IsEmpty() ? NULL : AnsiPassphrase.c_str());
    switch (KeyType)
    {
      case ktSSH2:
        if (!ssh2_save_userkey(KeyFile, Ssh2Key, PassphrasePtr))
        {
          int Error = errno;
          throw EOSExtException(FMTLOAD(KEY_SAVE_ERROR, (FileName)), Error);
        }
        break;

      default:
        DebugFail();
        break;
    }
  }
  __finally
  {
    filename_free(KeyFile);
  }
}
//---------------------------------------------------------------------------
void FreeKey(TPrivateKey * PrivateKey)
{
  struct ssh2_userkey * Ssh2Key = reinterpret_cast<struct ssh2_userkey *>(PrivateKey);
  Ssh2Key->alg->freekey(Ssh2Key->data);
  sfree(Ssh2Key->comment);
  sfree(Ssh2Key);
}
//---------------------------------------------------------------------------
RawByteString LoadPublicKey(const UnicodeString & FileName, UnicodeString & Algorithm, UnicodeString & Comment)
{
  RawByteString Result;
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_str(UtfFileName.c_str());
  try
  {
    char * AlgorithmStr = NULL;
    int PublicKeyLen = 0;
    char * CommentStr = NULL;
    const char * ErrorStr = NULL;
    unsigned char * PublicKeyPtr =
      ssh2_userkey_loadpub(KeyFile, &AlgorithmStr, &PublicKeyLen, &CommentStr, &ErrorStr);
    if (PublicKeyPtr == NULL)
    {
      UnicodeString Error = UnicodeString(AnsiString(ErrorStr));
      throw Exception(Error);
    }
    Algorithm = UnicodeString(AnsiString(AlgorithmStr));
    sfree(AlgorithmStr);
    Comment = UnicodeString(AnsiString(CommentStr));
    sfree(CommentStr);
    Result = RawByteString(reinterpret_cast<char *>(PublicKeyPtr), PublicKeyLen);
    free(PublicKeyPtr);
  }
  __finally
  {
    filename_free(KeyFile);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetPublicKeyLine(const UnicodeString & FileName, UnicodeString & Comment)
{
  UnicodeString Algorithm;
  RawByteString PublicKey = LoadPublicKey(FileName, Algorithm, Comment);
  UnicodeString PublicKeyBase64 = EncodeBase64(PublicKey.c_str(), PublicKey.Length());
  PublicKeyBase64 = ReplaceStr(PublicKeyBase64, L"\r", L"");
  PublicKeyBase64 = ReplaceStr(PublicKeyBase64, L"\n", L"");
  UnicodeString Result = FORMAT(L"%s %s %s", (Algorithm, PublicKeyBase64, Comment));
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI(UnicodeString CustomPath)
{
  static int has = -1;
  if (has < 0)
  {
    Conf * conf = conf_new();
    ssh_gss_liblist * List = NULL;
    try
    {
      Filename * filename = filename_from_str(UTF8String(CustomPath).c_str());
      conf_set_filename(conf, CONF_ssh_gss_custom, filename);
      filename_free(filename);
      List = ssh_gss_setup(conf, NULL);
      for (int Index = 0; (has <= 0) && (Index < List->nlibraries); Index++)
      {
        ssh_gss_library * library = &List->libraries[Index];
        Ssh_gss_ctx ctx;
        memset(&ctx, 0, sizeof(ctx));
        has =
          ((library->acquire_cred(library, &ctx) == SSH_GSS_OK) &&
           (library->release_cred(library, &ctx) == SSH_GSS_OK)) ? 1 : 0;
      }
    }
    __finally
    {
      ssh_gss_cleanup(List);
      conf_free(conf);
    }

    if (has < 0)
    {
      has = 0;
    }
  }
  return (has > 0);
}
//---------------------------------------------------------------------------
static void __fastcall DoNormalizeFingerprint(UnicodeString & Fingerprint, UnicodeString & KeyType)
{
  const wchar_t NormalizedSeparator = L'-';
  const int MaxCount = 10;
  const ssh_signkey * SignKeys[MaxCount];
  int Count = LENOF(SignKeys);
  // We may use find_pubkey_alg, but it gets complicated with normalized fingerprint
  // as the names have different number of dashes
  get_hostkey_algs(&Count, SignKeys);

  for (int Index = 0; Index < Count; Index++)
  {
    const ssh_signkey * SignKey = SignKeys[Index];
    UnicodeString Name = UnicodeString(SignKey->name);
    if (StartsStr(Name + L" ", Fingerprint))
    {
      int LenStart = Name.Length() + 1;
      Fingerprint[LenStart] = NormalizedSeparator;
      int Space = Fingerprint.Pos(L" ");
      // If not a number, it's an invalid input,
      // either something completelly wrong, or it can be OpenSSH base64 public key,
      // that got here from TPasteKeyHandler::Paste
      if (IsNumber(Fingerprint.SubString(LenStart + 1, Space - LenStart - 1)))
      {
        Fingerprint.Delete(LenStart + 1, Space - LenStart);
        // noop for SHA256 fingerprints
        Fingerprint = ReplaceChar(Fingerprint, L':', NormalizedSeparator);
        KeyType = UnicodeString(SignKey->keytype);
        return;
      }
    }
    else if (StartsStr(Name + NormalizedSeparator, Fingerprint))
    {
      KeyType = UnicodeString(SignKey->keytype);
      return;
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall NormalizeFingerprint(UnicodeString Fingerprint)
{
  UnicodeString KeyType; // unused
  DoNormalizeFingerprint(Fingerprint, KeyType);
  return Fingerprint;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall KeyTypeFromFingerprint(UnicodeString Fingerprint)
{
  UnicodeString KeyType;
  DoNormalizeFingerprint(Fingerprint, KeyType);
  return KeyType;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetPuTTYVersion()
{
  // "Release 0.64"
  // "Pre-release 0.65:2015-07-20.95501a1"
  // "Development snapshot 2015-12-22.51465fa"
  UnicodeString Result = get_putty_version();
  // Skip "Release", "Pre-release", "Development snapshot"
  int P = Result.LastDelimiter(L" ");
  Result.Delete(1, P);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall Sha256(const char * Data, size_t Size)
{
  unsigned char Digest[32];
  SHA256_Simple(Data, Size, Digest);
  UnicodeString Result(BytesToHex(Digest, LENOF(Digest)));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall DllHijackingProtection()
{
  dll_hijacking_protection();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ParseOpenSshPubLine(const UnicodeString & Line, const struct ssh_signkey *& Algorithm)
{
  UTF8String UtfLine = UTF8String(Line);
  char * AlgorithmName = NULL;
  int PubBlobLen = 0;
  char * CommentPtr = NULL;
  const char * ErrorStr = NULL;
  unsigned char * PubBlob = openssh_loadpub_line(UtfLine.c_str(), &AlgorithmName, &PubBlobLen, &CommentPtr, &ErrorStr);
  UnicodeString Result;
  if (PubBlob == NULL)
  {
    throw Exception(UnicodeString(ErrorStr));
  }
  else
  {
    try
    {
      Algorithm = find_pubkey_alg(AlgorithmName);
      if (Algorithm == NULL)
      {
        throw Exception(FORMAT(L"Unknown public key algorithm \"%s\".", (AlgorithmName)));
      }

      void * Key = Algorithm->newkey(Algorithm, reinterpret_cast<const char*>(PubBlob), PubBlobLen);
      if (Key == NULL)
      {
        throw Exception(L"Invalid public key.");
      }
      char * FmtKey = Algorithm->fmtkey(Key);
      Result = UnicodeString(FmtKey);
      sfree(FmtKey);
      Algorithm->freekey(Key);
    }
    __finally
    {
      sfree(PubBlob);
      sfree(AlgorithmName);
      sfree(CommentPtr);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetKeyTypeHuman(const UnicodeString & KeyType)
{
  UnicodeString Result;
  if (KeyType == ssh_dss.keytype)
  {
    Result = L"DSA";
  }
  else if (KeyType == ssh_rsa.keytype)
  {
    Result = L"RSA";
  }
  else if (KeyType == ssh_ecdsa_ed25519.keytype)
  {
    Result = L"Ed25519";
  }
  else if (KeyType == ssh_ecdsa_nistp256.keytype)
  {
    Result = L"ECDSA/nistp256";
  }
  else if (KeyType == ssh_ecdsa_nistp384.keytype)
  {
    Result = L"ECDSA/nistp384";
  }
  else if (KeyType == ssh_ecdsa_nistp521.keytype)
  {
    Result = L"ECDSA/nistp521";
  }
  else
  {
    DebugFail();
    Result = KeyType;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool IsOpenSSH(const UnicodeString & SshImplementation)
{
  return
    // e.g. "OpenSSH_5.3"
    (SshImplementation.Pos(L"OpenSSH") == 1) ||
    // Sun SSH is based on OpenSSH (suffers the same bugs)
    (SshImplementation.Pos(L"Sun_SSH") == 1);
}
//---------------------------------------------------------------------------
TStrings * SshCipherList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  // Same order as DefaultCipherList
  const ssh2_ciphers * Ciphers[] = { &ssh2_aes, &ssh2_ccp, &ssh2_blowfish, &ssh2_3des, &ssh2_arcfour, &ssh2_des };
  for (unsigned int Index = 0; Index < LENOF(Ciphers); Index++)
  {
    for (int Index2 = 0; Index2 < Ciphers[Index]->nciphers; Index2++)
    {
      UnicodeString Name = UnicodeString(Ciphers[Index]->list[Index2]->name);
      Result->Add(Name);
    }
  }
  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * SshKexList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  // Same order as DefaultKexList
  const ssh_kexes * Kexes[] = { &ssh_ecdh_kex, &ssh_diffiehellman_gex, &ssh_diffiehellman_group14, &ssh_rsa_kex, &ssh_diffiehellman_group1 };
  for (unsigned int Index = 0; Index < LENOF(Kexes); Index++)
  {
    for (int Index2 = 0; Index2 < Kexes[Index]->nkexes; Index2++)
    {
      UnicodeString Name = UnicodeString(Kexes[Index]->list[Index2]->name);
      Result->Add(Name);
    }
  }
  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * SshHostKeyList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  const int MaxCount = 10;
  const ssh_signkey * SignKeys[MaxCount];
  int Count = LENOF(SignKeys);
  get_hostkey_algs(&Count, SignKeys);

  for (int Index = 0; Index < Count; Index++)
  {
    const ssh_signkey * SignKey = SignKeys[Index];
    UnicodeString Name = UnicodeString(SignKey->name);
    Result->Add(Name);
  }
  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * SshMacList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  const struct ssh_mac ** Macs = NULL;
  int Count = 0;
  get_macs(&Count, &Macs);

  for (int Index = 0; Index < Count; Index++)
  {
    UnicodeString Name = UnicodeString(Macs[Index]->name);
    Result->Add(Name);
  }
  return Result.release();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
