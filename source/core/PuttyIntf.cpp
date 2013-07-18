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
//---------------------------------------------------------------------------
char sshver[50];
const int platform_uses_x11_unix_by_default = TRUE;
CRITICAL_SECTION noise_section;
bool SaveRandomSeed;
char appname_[50];
const char *const appname = appname_;
//---------------------------------------------------------------------------
extern "C"
{
#include <winstuff.h>
}
const UnicodeString OriginalPuttyRegistryStorageKey(_T(PUTTY_REG_POS));
const UnicodeString KittyRegistryStorageKey(L"Software\\9bis.com\\KiTTY");
const UnicodeString OriginalPuttyExecutable("putty.exe");
const UnicodeString KittyExecutable("kitty.exe");
//---------------------------------------------------------------------------
void __fastcall PuttyInitialize()
{
  SaveRandomSeed = true;

  InitializeCriticalSection(&noise_section);

  // make sure random generator is initialised, so random_save_seed()
  // in destructor can proceed
  random_ref();

  flags = FLAG_VERBOSE | FLAG_SYNCAGENT; // verbose log

  sk_init();

  AnsiString VersionString = SshVersionString();
  assert(!VersionString.IsEmpty() && (static_cast<size_t>(VersionString.Length()) < LENOF(sshver)));
  strcpy(sshver, VersionString.c_str());
  AnsiString AppName = AppNameString();
  assert(!AppName.IsEmpty() && (static_cast<size_t>(AppName.Length()) < LENOF(appname_)));
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
  DeleteCriticalSection(&noise_section);
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
  assert(frontend);

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
  assert(frontend);
  if (is_stderr >= 0)
  {
    assert((is_stderr == 0) || (is_stderr == 1));
    ((TSecureShell *)frontend)->FromBackend((is_stderr == 1), reinterpret_cast<const unsigned char *>(data), datalen);
  }
  else
  {
    assert(is_stderr == -1);
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
int get_userpass_input(prompts_t * p, unsigned char * /*in*/, int /*inlen*/)
{
  assert(p != NULL);
  TSecureShell * SecureShell = reinterpret_cast<TSecureShell *>(p->frontend);
  assert(SecureShell != NULL);

  int Result;
  TStrings * Prompts = new TStringList();
  TStrings * Results = new TStringList();
  try
  {
    for (int Index = 0; Index < int(p->n_prompts); Index++)
    {
      prompt_t * Prompt = p->prompts[Index];
      Prompts->AddObject(Prompt->prompt, (TObject *)(FLAGMASK(Prompt->echo, pupEcho)));
      Results->AddObject(L"", (TObject *)Prompt->result_len);
    }

    if (SecureShell->PromptUser(p->to_server, p->name, p->name_reqd,
          p->instruction, p->instr_reqd, Prompts, Results))
    {
      for (int Index = 0; Index < int(p->n_prompts); Index++)
      {
        prompt_t * Prompt = p->prompts[Index];
        strncpy(Prompt->result, AnsiString(Results->Strings[Index]).c_str(), Prompt->result_len);
        Prompt->result[Prompt->result_len - 1] = '\0';
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
  assert(false);
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
void connection_fatal(void * frontend, char * fmt, ...)
{
  va_list Param;
  char Buf[200];
  va_start(Param, fmt);
  vsnprintf(Buf, LENOF(Buf), fmt, Param); \
  Buf[LENOF(Buf) - 1] = '\0'; \
  va_end(Param);

  assert(frontend != NULL);
  ((TSecureShell *)frontend)->PuttyFatalError(Buf);
}
//---------------------------------------------------------------------------
int verify_ssh_host_key(void * frontend, char * host, int port, char * keytype,
  char * keystr, char * fingerprint, void (*/*callback*/)(void * ctx, int result),
  void * /*ctx*/)
{
  assert(frontend != NULL);
  ((TSecureShell *)frontend)->VerifyHostKey(host, port, keytype, keystr, fingerprint);

  // We should return 0 when key was not confirmed, we throw exception instead.
  return 1;
}
//---------------------------------------------------------------------------
int askalg(void * frontend, const char * algtype, const char * algname,
  void (*/*callback*/)(void * ctx, int result), void * /*ctx*/)
{
  assert(frontend != NULL);
  ((TSecureShell *)frontend)->AskAlg(algtype, algname);

  // We should return 0 when alg was not confirmed, we throw exception instead.
  return 1;
}
//---------------------------------------------------------------------------
void old_keyfile_warning(void)
{
  // no reference to TSecureShell instace available
}
//---------------------------------------------------------------------------
void display_banner(void * frontend, const char * banner, int size)
{
  assert(frontend);
  UnicodeString Banner(banner, size);
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
void fatalbox(char * fmt, ...)
{
  va_list Param;
  va_start(Param, fmt);
  SSHFatalError(fmt, Param);
  va_end(Param);
}
//---------------------------------------------------------------------------
void modalfatalbox(char * fmt, ...)
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
int askappend(void * /*frontend*/, Filename /*filename*/,
  void (*/*callback*/)(void * ctx, int result), void * /*ctx*/)
{
  // this is called from logging.c of putty, which is never used with WinSCP
  assert(false);
  return 0;
}
//---------------------------------------------------------------------------
void ldisc_send(void * /*handle*/, char * /*buf*/, int len, int /*interactive*/)
{
  // This is only here because of the calls to ldisc_send(NULL,
  // 0) in ssh.c. Nothing in PSCP actually needs to use the ldisc
  // as an ldisc. So if we get called with any real data, I want
  // to know about it.
  assert(len == 0);
  USEDPARAM(len);
}
//---------------------------------------------------------------------------
void agent_schedule_callback(void (* /*callback*/)(void *, void *, int),
  void * /*callback_ctx*/, void * /*data*/, int /*len*/)
{
  assert(false);
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
typedef void (*timer_fn_t)(void *ctx, long now);
long schedule_timer(int ticks, timer_fn_t /*fn*/, void * /*ctx*/)
{
  return ticks + GetTickCount();
}
//---------------------------------------------------------------------------
void expire_timer_context(void * /*ctx*/)
{
  // nothing
}
//---------------------------------------------------------------------------
Pinger pinger_new(Config * /*cfg*/, Backend * /*back*/, void * /*backhandle*/)
{
  return NULL;
}
//---------------------------------------------------------------------------
void pinger_reconfig(Pinger /*pinger*/, Config * /*oldcfg*/, Config * /*newcfg*/)
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
void platform_get_x11_auth(struct X11Display * /*display*/, const Config * /*cfg*/)
{
  // nothing, therefore no auth.
}
//---------------------------------------------------------------------------
int get_remote_username(Config * cfg, char *user, size_t len)
{
  if (*cfg->username)
  {
    strncpy(user, cfg->username, len);
    user[len-1] = '\0';
  }
  else
  {
    *user = '\0';
  }
  return (*user != '\0');
}
//---------------------------------------------------------------------------
static long OpenWinSCPKey(HKEY Key, const char * SubKey, HKEY * Result, bool CanCreate)
{
  long R;
  assert(Configuration != NULL);

  assert(Key == HKEY_CURRENT_USER);
  USEDPARAM(Key);

  UnicodeString RegKey = SubKey;
  int PuttyKeyLen = OriginalPuttyRegistryStorageKey.Length();
  assert(RegKey.SubString(1, PuttyKeyLen) == OriginalPuttyRegistryStorageKey);
  RegKey = RegKey.SubString(PuttyKeyLen + 1, RegKey.Length() - PuttyKeyLen);
  if (!RegKey.IsEmpty())
  {
    assert(RegKey[1] == L'\\');
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
    assert(RegKey == L"SshHostKeys");

    THierarchicalStorage * Storage = Configuration->CreateScpStorage(false);
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
  assert(Configuration != NULL);

  THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
  AnsiString Value;
  if (Storage == NULL)
  {
    if (UnicodeString(ValueName) == L"RandSeedFile")
    {
      Value = Configuration->RandomSeedFileName;
      R = ERROR_SUCCESS;
    }
    else
    {
      assert(false);
      R = ERROR_READ_FAULT;
    }
  }
  else
  {
    if (Storage->ValueExists(ValueName))
    {
      Value = Storage->ReadStringRaw(ValueName, L"");
      R = ERROR_SUCCESS;
    }
    else
    {
      R = ERROR_READ_FAULT;
    }
  }

  if (R == ERROR_SUCCESS)
  {
    assert(Type != NULL);
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
  assert(Configuration != NULL);

  assert(Type == REG_SZ);
  USEDPARAM(Type);
  THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
  assert(Storage != NULL);
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
  assert(Configuration != NULL);

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
  assert(ktUnopenable == SSH_KEYTYPE_UNOPENABLE);
  assert(ktSSHCom == SSH_KEYTYPE_SSHCOM);
  Filename KeyFile;
  ASCOPY(KeyFile.path, FileName);
  return (TKeyType)key_type(&KeyFile);
}
//---------------------------------------------------------------------------
UnicodeString KeyTypeName(TKeyType KeyType)
{
  return key_type_to_str(KeyType);
}
//---------------------------------------------------------------------------
__int64 __fastcall ParseSize(UnicodeString SizeStr)
{
  AnsiString AnsiSizeStr = SizeStr;
  return parse_blocksize(AnsiSizeStr.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI()
{
  static int has = -1;
  if (has < 0)
  {
    Config cfg;
    memset(&cfg, 0, sizeof(cfg));
    ssh_gss_liblist * List = ssh_gss_setup(&cfg);
    try
    {
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
    }

    if (has < 0)
    {
      has = 0;
    }
  }
  return (has > 0);
}
//---------------------------------------------------------------------------
