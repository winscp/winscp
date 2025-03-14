//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

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
const bool platform_uses_x11_unix_by_default = true;
CRITICAL_SECTION putty_section;
static bool SaveRandomSeed;
static bool HadRandomSeed;
static char appname_[50];
const char *const appname = appname_;
extern const bool share_can_be_downstream = false;
extern const bool share_can_be_upstream = false;
//---------------------------------------------------------------------------
extern "C"
{
#include <windows\platform.h>
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

  HadRandomSeed = FileExists(ApiPath(Configuration->RandomSeedFileName));
  if (HadRandomSeed)
  {
    AppLog(L"Random seed file exists");
  }
  // make sure random generator is initialised, so random_save_seed()
  // in destructor can proceed
  random_ref();

  sk_init();

  AnsiString VersionString = AnsiString(SshVersionString());
  DebugAssert(!VersionString.IsEmpty() && (static_cast<size_t>(VersionString.Length()) < LENOF(sshver)));
  strcpy(sshver, VersionString.c_str());
  AnsiString AppName = AnsiString(AppNameString());
  DebugAssert(!AppName.IsEmpty() && (static_cast<size_t>(AppName.Length()) < LENOF(appname_)));
  strcpy(appname_, AppName.c_str());
}
//---------------------------------------------------------------------------
static bool DeleteRandomSeedOnExit()
{
  return !HadRandomSeed && !SaveRandomSeed;
}
//---------------------------------------------------------------------------
void __fastcall PuttyFinalize()
{
  if (SaveRandomSeed)
  {
    AppLog(L"Saving random seed file");
    random_save_seed();
  }
  random_unref();
  // random_ref in PuttyInitialize creates the seed file. Delete it, if we didn't want to create it.
  if (DeleteRandomSeedOnExit())
  {
    AppLog(L"Deleting unwanted random seed file");
    DeleteFile(ApiPath(Configuration->RandomSeedFileName));
  }

  sk_cleanup();
  win_misc_cleanup();
  win_secur_cleanup();
  ec_cleanup();
  wingss_cleanup();
  DeleteCriticalSection(&putty_section);
}
//---------------------------------------------------------------------------
void __fastcall DontSaveRandomSeed()
{
  SaveRandomSeed = false;
}
//---------------------------------------------------------------------------
bool RandomSeedExists()
{
  return
    !DeleteRandomSeedOnExit() &&
    FileExists(ApiPath(Configuration->RandomSeedFileName));
}
//---------------------------------------------------------------------------
TSecureShell * GetSeatSecureShell(Seat * seat)
{
  DebugAssert(seat != NULL);
  if (is_tempseat(seat))
  {
    seat = tempseat_get_real(seat);
  }

  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  return SecureShell;
}
//---------------------------------------------------------------------------
TSecureShell * GetSecureShell(Plug * plug, bool & pfwd)
{
  if (!is_ssh(plug) && !is_pfwd(plug))
  {
    // If it is not SSH/PFwd plug, then it must be Proxy plug.
    // Get SSH/PFwd plug which it wraps.
    ProxySocket * AProxySocket = get_proxy_plug_socket(plug);
    plug = AProxySocket->plug;
  }

  pfwd = is_pfwd(plug);
  Seat * seat;
  if (pfwd)
  {
    seat = get_pfwd_seat(plug);
  }
  else
  {
    seat = get_ssh_seat(plug);
  }
  DebugAssert(seat != NULL);
  return GetSeatSecureShell(seat);
}
//---------------------------------------------------------------------------
struct callback_set * get_callback_set(Plug * plug)
{
  bool pfwd;
  TSecureShell * SecureShell = GetSecureShell(plug, pfwd);
  return SecureShell->GetCallbackSet();
}
//---------------------------------------------------------------------------
struct callback_set * get_seat_callback_set(Seat * seat)
{
  TSecureShell * SecureShell = GetSeatSecureShell(seat);
  return SecureShell->GetCallbackSet();
}
//---------------------------------------------------------------------------
extern "C" const char * do_select(Plug * plug, SOCKET skt, bool enable)
{
  bool pfwd;
  TSecureShell * SecureShell = GetSecureShell(plug, pfwd);
  if (!pfwd)
  {
    SecureShell->UpdateSocket(skt, enable);
  }
  else
  {
    SecureShell->UpdatePortFwdSocket(skt, enable);
  }

  return NULL;
}
//---------------------------------------------------------------------------
static size_t output(Seat * seat, SeatOutputType type, const void * data, size_t len)
{
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  if (static_cast<int>(static_cast<char>(type)) == -1)
  {
    SecureShell->CWrite(reinterpret_cast<const char *>(data), len);
  }
  else if (type != SEAT_OUTPUT_STDERR)
  {
    SecureShell->FromBackend(reinterpret_cast<const unsigned char *>(data), len);
  }
  else
  {
    SecureShell->AddStdError(reinterpret_cast<const char *>(data), len);
  }
  return 0;
}
//---------------------------------------------------------------------------
static bool eof(Seat *)
{
  return false;
}
//---------------------------------------------------------------------------
static SeatPromptResult get_userpass_input(Seat * seat, prompts_t * p)
{
  DebugAssert(p != NULL);
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  DebugAssert(SecureShell != NULL);

  SeatPromptResult Result;
  TStrings * Prompts = new TStringList();
  TStrings * Results = new TStringList();
  try
  {
    UnicodeString Name = UTF8ToString(p->name);

    for (int Index = 0; Index < int(p->n_prompts); Index++)
    {
      prompt_t * Prompt = p->prompts[Index];
      UnicodeString S;
      if (p->utf8)
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
      DebugAssert(strlen(prompt_get_result_ref(Prompt)) == 0);
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
        if (p->utf8)
        {
          S = RawByteString(UTF8String(Results->Strings[Index]));
        }
        else
        {
          S = RawByteString(AnsiString(Results->Strings[Index]));
        }
        prompt_set_result(Prompt, S.c_str());
      }
      Result = SPR_OK;
    }
    else
    {
      Result = SPR_USER_ABORT;
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
static void connection_fatal(Seat * seat, const char * message)
{

  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  SecureShell->PuttyFatalError(UnicodeString(AnsiString(message)));
}
//---------------------------------------------------------------------------
static void nonfatal(Seat *, const char * message)
{
  // there's no place in our putty code, where this is called
  DebugFail();
  AppLog(UnicodeString(AnsiString(message)));
}
//---------------------------------------------------------------------------
SeatPromptResult confirm_ssh_host_key(Seat * seat, const char * host, int port, const char * keytype,
  char * keystr, SeatDialogText *, HelpCtx,
  void (*DebugUsedArg(callback))(void *ctx, SeatPromptResult result), void * DebugUsedArg(ctx),
  char **key_fingerprints, bool is_certificate, int ca_count, bool already_verified)
{
  UnicodeString FingerprintSHA256, FingerprintMD5;
  if (key_fingerprints[SSH_FPTYPE_SHA256] != NULL)
  {
    FingerprintSHA256 = key_fingerprints[SSH_FPTYPE_SHA256];
  }
  if (DebugAlwaysTrue(key_fingerprints[SSH_FPTYPE_MD5] != NULL))
  {
    FingerprintMD5 = key_fingerprints[SSH_FPTYPE_MD5];
  }
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  SecureShell->VerifyHostKey(
    host, port, keytype, keystr, FingerprintSHA256, FingerprintMD5, is_certificate, ca_count, already_verified);

  // We should return 0 when key was not confirmed, we throw exception instead.
  return SPR_OK;
}
//---------------------------------------------------------------------------
bool have_ssh_host_key(Seat * seat, const char * hostname, int port,
  const char * keytype)
{
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  return SecureShell->HaveHostKey(hostname, port, keytype) ? 1 : 0;
}
//---------------------------------------------------------------------------
SeatPromptResult confirm_weak_crypto_primitive(
    Seat * seat, SeatDialogText *,
    void (*DebugUsedArg(callback))(void * ctx, SeatPromptResult result), void * DebugUsedArg(ctx),
    const char * algtype, const char *algname, int wcr)
{
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  SecureShell->AskAlg(algtype, algname, wcr);

  // We should return 0 when alg was not confirmed, we throw exception instead.
  return SPR_OK;
}
//---------------------------------------------------------------------------
SeatPromptResult confirm_weak_cached_hostkey(
  Seat *, SeatDialogText *,
  void (*DebugUsedArg(callback))(void * ctx, SeatPromptResult result), void * DebugUsedArg(ctx))
{
  return SPR_OK;
}
//---------------------------------------------------------------------------
const SeatDialogPromptDescriptions * prompt_descriptions(Seat *)
{
    static const SeatDialogPromptDescriptions descs = {
        /*.hk_accept_action =*/ "",
        /*.hk_connect_once_action =*/ "",
        /*.hk_cancel_action =*/ "",
        /*.hk_cancel_action_Participle =*/ "",
        NULL, NULL,
    };
    return &descs;
}
//---------------------------------------------------------------------------
void old_keyfile_warning(void)
{
  // no reference to TSecureShell instance available - and we already warn on Login dialog
}
//---------------------------------------------------------------------------
size_t banner(Seat * seat, const void * data, size_t len)
{
  TSecureShell * SecureShell = static_cast<ScpSeat *>(seat)->SecureShell;
  UnicodeString Banner(UTF8String(static_cast<const char *>(data), len));
  SecureShell->DisplayBanner(Banner);
  return 0; // PuTTY never uses the value
}
//---------------------------------------------------------------------------
NORETURN static void SSHFatalError(const char * Format, va_list Param)
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
void ldisc_echoedit_update(Ldisc * /*handle*/)
{
  DebugFail();
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
Pinger * pinger_new(Conf * /*conf*/, Backend * /*back*/)
{
  return NULL;
}
//---------------------------------------------------------------------------
void pinger_reconfig(Pinger * /*pinger*/, Conf * /*oldconf*/, Conf * /*newconf*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void pinger_free(Pinger * /*pinger*/)
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
static const SeatVtable ScpSeatVtable =
  {
    output,
    eof,
    nullseat_sent,
    banner,
    get_userpass_input,
    nullseat_notify_session_started,
    nullseat_notify_remote_exit,
    nullseat_notify_remote_disconnect,
    connection_fatal,
    nonfatal,
    nullseat_update_specials_menu,
    nullseat_get_ttymode,
    nullseat_set_busy_status,
    confirm_ssh_host_key,
    confirm_weak_crypto_primitive,
    confirm_weak_cached_hostkey,
    prompt_descriptions,
    nullseat_is_always_utf8,
    nullseat_echoedit_update,
    nullseat_get_x_display,
    nullseat_get_windowid,
    nullseat_get_window_pixel_size,
    nullseat_stripctrl_new,
    nullseat_set_trust_status,
    nullseat_can_set_trust_status_yes,
    nullseat_has_mixed_input_stream_yes,
    nullseat_verbose_yes,
    nullseat_interactive_no,
    nullseat_get_cursor_position,
  };
//---------------------------------------------------------------------------
ScpSeat::ScpSeat(TSecureShell * ASecureShell)
{
  SecureShell = ASecureShell;
  vt = &ScpSeatVtable;
}
//---------------------------------------------------------------------------
std::unique_ptr<TCriticalSection> PuttyStorageSection(TraceInitPtr(new TCriticalSection()));
THierarchicalStorage * PuttyStorage = NULL;
enum TPuttyRegistryMode { prmPass, prmRedirect, prmCollect, prmFail };
static TPuttyRegistryMode PuttyRegistryMode = prmRedirect;
typedef std::map<UnicodeString, unsigned long> TPuttyRegistryTypes;
static TPuttyRegistryTypes PuttyRegistryTypes;
static HKEY RandSeedFileStorage = reinterpret_cast<HKEY>(1);
//---------------------------------------------------------------------------
int reg_override_winscp()
{
  return (PuttyRegistryMode != prmPass);
}
//---------------------------------------------------------------------------
void putty_registry_pass(bool enable)
{
  if (enable)
  {
    PuttyStorageSection->Enter();
    DebugAssert(PuttyRegistryMode == prmRedirect);
    PuttyRegistryMode = prmPass;
  }
  else
  {
    DebugAssert(PuttyRegistryMode == prmPass);
    PuttyRegistryMode = prmRedirect;
    PuttyStorageSection->Leave();
  }
}
//---------------------------------------------------------------------------
HKEY open_regkey_fn_winscp(bool Create, bool Write, HKEY Key, const char * Path, ...)
{
  DebugUsedParam(Write);
  HKEY Result;
  if (PuttyRegistryMode == prmCollect)
  {
    // Note that for prmCollect even !Write mode is supported (behaving like prmFail) - needed for do_defaults
    Result = reinterpret_cast<HKEY>(1);
  }
  else if (PuttyRegistryMode == prmFail)
  {
    Result = reinterpret_cast<HKEY>(false);
  }
  else if (PuttyRegistryMode == prmRedirect)
  {
    DebugAssert(Key == HKEY_CURRENT_USER);
    DebugUsedParam(Key);

    UnicodeString SubKey;
    va_list ap;
    va_start(ap, Path);

    for (; Path; Path = va_arg(ap, const char *))
    {
      if (!SubKey.IsEmpty())
      {
        SubKey = IncludeTrailingBackslash(SubKey);
      }
      SubKey += UnicodeString(UTF8String(Path));
    }

    int PuttyKeyLen = OriginalPuttyRegistryStorageKey.Length();
    DebugAssert(SubKey.SubString(1, PuttyKeyLen) == OriginalPuttyRegistryStorageKey);
    UnicodeString RegKey = SubKey.SubString(PuttyKeyLen + 1, SubKey.Length() - PuttyKeyLen);
    if (!RegKey.IsEmpty())
    {
      DebugAssert(RegKey[1] == L'\\');
      RegKey.Delete(1, 1);
    }

    if (RegKey.IsEmpty())
    {
      // Called from access_random_seed()
      Result = RandSeedFileStorage;
    }
    else
    {
      // we expect this to be called only from retrieve_host_key() or store_host_key()
      DebugAssert(RegKey == L"SshHostKeys");

      DebugAssert(PuttyStorage != NULL);
      DebugAssert(PuttyStorage->AccessMode == (Write ? smReadWrite : smRead));
      if (PuttyStorage->OpenSubKey(RegKey, Create))
      {
        Result = reinterpret_cast<HKEY>(PuttyStorage);
      }
      else
      {
        Result = NULL;
      }
    }
  }
  else
  {
    DebugFail();
    Result = NULL;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool get_reg_dword_winscp(HKEY, const char * DebugUsedArg(Name), DWORD * DebugUsedArg(Out))
{
  bool Result;
  if (PuttyRegistryMode == prmFail)
  {
    Result = false;
  }
  else if (PuttyRegistryMode == prmCollect)
  {
    Result = false;
  }
  else
  {
    DebugFail();
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
char * get_reg_sz_winscp(HKEY Key, const char * Name)
{
  char * Result;
  if (PuttyRegistryMode == prmCollect)
  {
    Result = NULL;
  }
  else if (DebugAlwaysTrue(PuttyRegistryMode == prmRedirect))
  {
    DebugAssert(Configuration != NULL);

    UnicodeString ValueName = UTF8String(Name);
    bool Success;
    UnicodeString Value;
    if (Key == RandSeedFileStorage)
    {
      if (ValueName == L"RandSeedFile")
      {
        Value = Configuration->RandomSeedFileName;
        Success = true;
      }
      else
      {
        DebugFail();
        Success = false;
      }
    }
    else
    {
      THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
      if (Storage->ValueExists(ValueName))
      {
        Value = Storage->ReadStringRaw(ValueName, L"");
        Success = true;
      }
      else
      {
        Success = false;
      }
    }

    if (!Success)
    {
      Result = NULL;
    }
    else
    {
      AnsiString ValueAnsi = AnsiString(Value);
      Result = snewn(ValueAnsi.Length() + 1, char);
      strcpy(Result, ValueAnsi.c_str());
    }
  }
  else
  {
    Result = NULL;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool put_reg_dword_winscp(HKEY DebugUsedArg(Key), const char * Name, DWORD DebugUsedArg(Value))
{
  bool Result;
  if (PuttyRegistryMode == prmCollect)
  {
    UnicodeString ValueName = UTF8String(Name);
    PuttyRegistryTypes[ValueName] = REG_DWORD;
    Result = true;
  }
  else if (PuttyRegistryMode == prmRedirect)
  {
    // Might need to implement this for CA
    DebugFail();
    Result = false;
  }
  else
  {
    DebugFail();
    return false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool put_reg_sz_winscp(HKEY Key, const char * Name, const char * Str)
{
  UnicodeString ValueName = UTF8String(Name);
  bool Result;
  if (PuttyRegistryMode == prmCollect)
  {
    PuttyRegistryTypes[ValueName] = REG_SZ;
    Result = true;
  }
  else if (PuttyRegistryMode == prmRedirect)
  {
    UnicodeString Value = UTF8String(Str);
    DebugAssert(Key != RandSeedFileStorage);
    THierarchicalStorage * Storage = reinterpret_cast<THierarchicalStorage *>(Key);
    DebugAssert(Storage != NULL);
    if (Storage != NULL)
    {
      Storage->WriteStringRaw(ValueName, Value);
    }

    Result = true;
  }
  else
  {
    DebugFail();
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void close_regkey_winscp(HKEY)
{
  DebugAssert((PuttyRegistryMode == prmCollect) || (PuttyRegistryMode == prmRedirect));
}
//---------------------------------------------------------------------------
strbuf * get_reg_multi_sz_winscp(HKEY, const char * DebugUsedArg(name))
{
  // Needed for CA
  DebugFail();
  return NULL;
}
//---------------------------------------------------------------------------
TKeyType KeyType(UnicodeString FileName)
{
  DebugAssert(ktUnopenable == SSH_KEYTYPE_UNOPENABLE);
  DebugAssert(ktSSHCom == SSH_KEYTYPE_SSHCOM);
  DebugAssert(ktSSH2PublicOpenSSH == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH);
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_utf8(UtfFileName.c_str());
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
  Filename * KeyFile = filename_from_utf8(UtfFileName.c_str());
  try
  {
    switch (KeyType)
    {
      case ktSSH2:
        Result = (ppk_encrypted_f(KeyFile, &CommentStr) != 0);
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
TPrivateKey * LoadKey(TKeyType KeyType, const UnicodeString & FileName, const UnicodeString & Passphrase, UnicodeString & Error)
{
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_utf8(UtfFileName.c_str());
  struct ssh2_userkey * Ssh2Key = NULL;
  const char * ErrorStr = NULL;
  AnsiString AnsiPassphrase = Passphrase;
  try
  {
    switch (KeyType)
    {
      case ktSSH2:
        Ssh2Key = ppk_load_f(KeyFile, AnsiPassphrase.c_str(), &ErrorStr);
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
    // While theoretically we may get "unable to open key file" and
    // so we should check system error code,
    // we actually never get here unless we call KeyType previously
    // and handle ktUnopenable accordingly.
    Error = AnsiString(ErrorStr);
  }
  else if (Ssh2Key == SSH2_WRONG_PASSPHRASE)
  {
    Error = EmptyStr;
    Ssh2Key = NULL;
  }

  return reinterpret_cast<TPrivateKey *>(Ssh2Key);
}
//---------------------------------------------------------------------------
TPrivateKey * LoadKey(TKeyType KeyType, const UnicodeString & FileName, const UnicodeString & Passphrase)
{
  UnicodeString Error;
  TPrivateKey * Result = LoadKey(KeyType, FileName, Passphrase, Error);
  if (Result == NULL)
  {
    if (!Error.IsEmpty())
    {
      throw Exception(Error);
    }
    else
    {
      throw Exception(LoadStr(AUTH_TRANSL_WRONG_PASSPHRASE));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TestKey(TKeyType KeyType, const UnicodeString & FileName)
{
  UnicodeString Result;
  TPrivateKey * Key = LoadKey(KeyType, FileName, EmptyStr, Result);
  if (Key != NULL)
  {
    FreeKey(Key);
  }
  return Result;
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
// Based on cmdgen.c
void AddCertificateToKey(TPrivateKey * PrivateKey, const UnicodeString & CertificateFileName)
{
  struct ssh2_userkey * Ssh2Key = reinterpret_cast<struct ssh2_userkey *>(PrivateKey);

  TKeyType Type = KeyType(CertificateFileName);
  int Error = errno;
  if ((Type != SSH_KEYTYPE_SSH2_PUBLIC_RFC4716) &&
      (Type != SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH))
  {
    if (Type == ktUnopenable)
    {
      throw EOSExtException(FMTLOAD(CERTIFICATE_UNOPENABLE, (CertificateFileName)), Error);
    }
    else
    {
      throw Exception(FMTLOAD(KEYGEN_NOT_PUBLIC, (CertificateFileName)));
    }
  }

  UTF8String UtfCertificateFileName = UTF8String(CertificateFileName);
  Filename * CertFilename = filename_from_utf8(UtfCertificateFileName.c_str());

  LoadedFile * CertLoadedFile;
  try
  {
    const char * ErrorStr = NULL;
    CertLoadedFile = lf_load_keyfile(CertFilename, &ErrorStr);
    if (CertLoadedFile == NULL)
    {
      // not capturing errno, as this in unlikely file access error, after we have passed KeyType above
      throw ExtException(FMTLOAD(CERTIFICATE_UNOPENABLE, (CertificateFileName)), Error);
    }
  }
  __finally
  {
    filename_free(CertFilename);
  }

  strbuf * Pub = strbuf_new();
  char * AlgorithmName = NULL;
  try
  {
    const char * ErrorStr = NULL;
    char * CommentStr = NULL;
    if (!ppk_loadpub_s(BinarySource_UPCAST(CertLoadedFile), &AlgorithmName,
                       BinarySink_UPCAST(Pub), &CommentStr, &ErrorStr))
    {
      UnicodeString Error = AnsiString(ErrorStr);
      throw ExtException(FMTLOAD(CERTIFICATE_LOAD_ERROR, (CertificateFileName)), Error);
    }
    sfree(CommentStr);
  }
  __finally
  {
    lf_free(CertLoadedFile);
  }

  const ssh_keyalg * KeyAlg;
  try
  {
    KeyAlg = find_pubkey_alg(AlgorithmName);
    if (KeyAlg == NULL)
    {
      throw Exception(FMTLOAD(PUB_KEY_UNKNOWN, (AlgorithmName)));
    }

    // Check the two public keys match apart from certificates
    strbuf * OldBasePub = strbuf_new();
    ssh_key_public_blob(ssh_key_base_key(Ssh2Key->key), BinarySink_UPCAST(OldBasePub));

    ssh_key * NewPubKey = ssh_key_new_pub(KeyAlg, ptrlen_from_strbuf(Pub));
    strbuf * NewBasePub = strbuf_new();
    ssh_key_public_blob(ssh_key_base_key(NewPubKey), BinarySink_UPCAST(NewBasePub));
    ssh_key_free(NewPubKey);

    bool Match = ptrlen_eq_ptrlen(ptrlen_from_strbuf(OldBasePub), ptrlen_from_strbuf(NewBasePub));
    strbuf_free(OldBasePub);
    strbuf_free(NewBasePub);

    if (!Match)
    {
      throw Exception(FMTLOAD(CERTIFICATE_NOT_MATCH, (CertificateFileName)));
    }

    strbuf * Priv = strbuf_new_nm();
    ssh_key_private_blob(Ssh2Key->key, BinarySink_UPCAST(Priv));
    ssh_key * NewKey = ssh_key_new_priv(KeyAlg, ptrlen_from_strbuf(Pub), ptrlen_from_strbuf(Priv));
    strbuf_free(Priv);

    if (NewKey == NULL)
    {
      throw Exception(FMTLOAD(CERTIFICATE_CANNOT_COMBINE, (CertificateFileName)));
    }

    ssh_key_free(Ssh2Key->key);
    Ssh2Key->key = NewKey;
  }
  __finally
  {
    strbuf_free(Pub);
    sfree(AlgorithmName);
  }
}
//---------------------------------------------------------------------------
void SaveKey(TKeyType KeyType, const UnicodeString & FileName,
  const UnicodeString & Passphrase, TPrivateKey * PrivateKey)
{
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_utf8(UtfFileName.c_str());
  try
  {
    struct ssh2_userkey * Ssh2Key = reinterpret_cast<struct ssh2_userkey *>(PrivateKey);
    AnsiString AnsiPassphrase = Passphrase;
    char * PassphrasePtr = (AnsiPassphrase.IsEmpty() ? NULL : AnsiPassphrase.c_str());
    switch (KeyType)
    {
      case ktSSH2:
        {
          ppk_save_parameters Params = ppk_save_default_parameters;
          if (Configuration->KeyVersion != 0)
          {
            Params.fmt_version = Configuration->KeyVersion;
          }
          if (!ppk_save_f(KeyFile, Ssh2Key, PassphrasePtr, &Params))
          {
            int Error = errno;
            throw EOSExtException(FMTLOAD(KEY_SAVE_ERROR, (FileName)), Error);
          }
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
  ssh_key_free(Ssh2Key->key);
  sfree(Ssh2Key->comment);
  sfree(Ssh2Key);
}
//---------------------------------------------------------------------------
RawByteString StrBufToString(strbuf * StrBuf)
{
  return RawByteString(reinterpret_cast<char *>(StrBuf->s), StrBuf->len);
}
//---------------------------------------------------------------------------
RawByteString LoadPublicKey(
  const UnicodeString & FileName, UnicodeString & Algorithm, UnicodeString & Comment, bool & HasCertificate)
{
  RawByteString Result;
  UTF8String UtfFileName = UTF8String(FileName);
  Filename * KeyFile = filename_from_utf8(UtfFileName.c_str());
  try
  {
    char * AlgorithmStr = NULL;
    char * CommentStr = NULL;
    const char * ErrorStr = NULL;
    strbuf * PublicKeyBuf = strbuf_new();
    if (!ppk_loadpub_f(KeyFile, &AlgorithmStr, BinarySink_UPCAST(PublicKeyBuf), &CommentStr, &ErrorStr))
    {
      UnicodeString Error = UnicodeString(AnsiString(ErrorStr));
      throw Exception(Error);
    }
    Algorithm = UnicodeString(AnsiString(AlgorithmStr));
    const ssh_keyalg * KeyAlg = find_pubkey_alg(AlgorithmStr);
    HasCertificate = (KeyAlg != NULL) && KeyAlg->is_certificate;
    sfree(AlgorithmStr);
    Comment = UnicodeString(AnsiString(CommentStr));
    sfree(CommentStr);
    Result = StrBufToString(PublicKeyBuf);
    strbuf_free(PublicKeyBuf);
  }
  __finally
  {
    filename_free(KeyFile);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetPublicKeyLine(const UnicodeString & FileName, UnicodeString & Comment, bool & HasCertificate)
{
  UnicodeString Algorithm;
  RawByteString PublicKey = LoadPublicKey(FileName, Algorithm, Comment, HasCertificate);
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
      Filename * filename = filename_from_utf8(UTF8String(CustomPath).c_str());
      conf_set_filename(conf, CONF_ssh_gss_custom, filename);
      filename_free(filename);
      List = ssh_gss_setup(conf, NULL);
      for (int Index = 0; (has <= 0) && (Index < List->nlibraries); Index++)
      {
        ssh_gss_library * library = &List->libraries[Index];
        Ssh_gss_ctx ctx;
        memset(&ctx, 0, sizeof(ctx));
        has =
          ((library->acquire_cred(library, &ctx, NULL) == SSH_GSS_OK) &&
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
static void __fastcall DoNormalizeFingerprint(UnicodeString & Fingerprint, UnicodeString & KeyName, UnicodeString & KeyType)
{
  cp_ssh_keyalg * SignKeys;
  int Count;
  // We may use find_pubkey_alg, but it gets complicated with normalized fingerprint
  // as the names have different number of dashes
  get_hostkey_algs(-1, &Count, &SignKeys);
  try
  {
    for (int Index = 0; Index < Count; Index++)
    {
      cp_ssh_keyalg SignKey = SignKeys[Index];
      UnicodeString Name = UnicodeString(SignKey->ssh_id);
      if (StartsStr(Name + L" ", Fingerprint))
      {
        UnicodeString Rest = Fingerprint.SubString(Name.Length() + 2, Fingerprint.Length() - Name.Length() - 1);
        int Space = Rest.Pos(L" ");
        // If not a number, it's an invalid input,
        // either something completely wrong, or it can be OpenSSH base64 public key,
        // that got here from TPasteKeyHandler::Paste
        if (IsNumber(Rest.SubString(1, Space - 1)))
        {
          KeyName = Name;
          Fingerprint = Rest.SubString(Space + 1, Fingerprint.Length() - Space);
          Fingerprint = Base64ToUrlSafe(Fingerprint);
          Fingerprint = MD5ToUrlSafe(Fingerprint);
          KeyType = UnicodeString(SignKey->cache_id);
          return;
        }
      }
      else if (StartsStr(Name + NormalizedFingerprintSeparator, Fingerprint))
      {
        KeyType = UnicodeString(SignKey->cache_id);
        KeyName = Name;
        Fingerprint.Delete(1, Name.Length() + 1);
        return;
      }
    }
  }
  __finally
  {
    sfree(SignKeys);
  }
}
//---------------------------------------------------------------------------
void __fastcall NormalizeFingerprint(UnicodeString & Fingerprint, UnicodeString & KeyName)
{
  UnicodeString KeyType;
  DoNormalizeFingerprint(Fingerprint, KeyName, KeyType);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall KeyTypeFromFingerprint(UnicodeString Fingerprint)
{
  UnicodeString KeyType;
  UnicodeString KeyName; // unused
  DoNormalizeFingerprint(Fingerprint, KeyName, KeyType);
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
  hash_simple(&ssh_sha256, make_ptrlen(Data, Size), Digest);
  UnicodeString Result(BytesToHex(Digest, LENOF(Digest)));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString CalculateFileChecksum(TStream * Stream, const UnicodeString & Alg)
{
  const ssh_hashalg * HashAlg;
  if (SameIdent(Alg, Sha256ChecksumAlg))
  {
    HashAlg = &ssh_sha256;
  }
  else if (SameIdent(Alg, Sha1ChecksumAlg))
  {
    HashAlg = &ssh_sha1;
  }
  else if (SameIdent(Alg, Md5ChecksumAlg))
  {
    HashAlg = &ssh_md5;
  }
  else
  {
    throw Exception(FMTLOAD(UNKNOWN_CHECKSUM, (Alg)));
  }

  RawByteString Buf;
  ssh_hash * Hash = ssh_hash_new(HashAlg);
  try
  {
    const int BlockSize = 32 * 1024;
    TFileBuffer Buffer;
    DWORD Read;
    do
    {
      Buffer.Reset();
      Read = Buffer.LoadStream(Stream, BlockSize, false);
      if (Read > 0)
      {
        put_datapl(Hash, make_ptrlen(Buffer.Data, Read));
      }
    }
    while (Read > 0);
  }
  __finally
  {
    Buf.SetLength(ssh_hash_alg(Hash)->hlen);
    ssh_hash_final(Hash, reinterpret_cast<unsigned char *>(Buf.c_str()));
  }

  UnicodeString Result = BytesToHex(Buf);

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ParseOpenSshPubLine(const UnicodeString & Line, const struct ssh_keyalg *& Algorithm)
{
  UTF8String UtfLine = UTF8String(Line);
  char * AlgorithmName = NULL;
  char * CommentPtr = NULL;
  const char * ErrorStr = NULL;
  strbuf * PubBlobBuf = strbuf_new();
  BinarySource Source[1];
  BinarySource_BARE_INIT(Source, UtfLine.c_str(), UtfLine.Length());
  UnicodeString Result;
  try
  {
    if (!openssh_loadpub(Source, &AlgorithmName, BinarySink_UPCAST(PubBlobBuf), &CommentPtr, &ErrorStr))
    {
      throw Exception(UnicodeString(ErrorStr));
    }
    else
    {
      Algorithm = find_pubkey_alg(AlgorithmName);
      if (Algorithm == NULL)
      {
        throw Exception(FMTLOAD(PUB_KEY_UNKNOWN, (AlgorithmName)));
      }

      ptrlen PtrLen = { PubBlobBuf->s, PubBlobBuf->len };
      ssh_key * Key = Algorithm->new_pub(Algorithm, PtrLen);
      if (Key == NULL)
      {
        throw Exception(L"Invalid public key.");
      }
      char * FmtKey = Algorithm->cache_str(Key);
      Result = UnicodeString(FmtKey);
      sfree(FmtKey);
      Algorithm->freekey(Key);
    }
  }
  __finally
  {
    strbuf_free(PubBlobBuf);
    sfree(AlgorithmName);
    sfree(CommentPtr);
  }
  return Result;
}
//---------------------------------------------------------------------------
// Based on ca_refresh_pubkey_info
void ParseCertificatePublicKey(const UnicodeString & Str, RawByteString & PublicKey, UnicodeString & Fingerprint)
{
  AnsiString AnsiStr = AnsiString(Str);
  ptrlen Data = ptrlen_from_asciz(AnsiStr.c_str());
  strbuf * Blob = strbuf_new();
  try
  {
    // See if we have a plain base64-encoded public key blob.
    if (base64_valid(Data))
    {
      base64_decode_bs(BinarySink_UPCAST(Blob), Data);
    }
    else
    {
      // Otherwise, try to decode as if it was a public key _file_.
      BinarySource Src[1];
      BinarySource_BARE_INIT_PL(Src, Data);
      const char * Error;
      if (!ppk_loadpub_s(Src, NULL, BinarySink_UPCAST(Blob), NULL, &Error))
      {
        throw Exception(FMTLOAD(SSH_HOST_CA_DECODE_ERROR, (Error)));
      }
    }

    ptrlen AlgNamePtrLen = pubkey_blob_to_alg_name(ptrlen_from_strbuf(Blob));
    if (!AlgNamePtrLen.len)
    {
      throw Exception(LoadStr(SSH_HOST_CA_NO_KEY_TYPE));
    }

    UnicodeString AlgName = UnicodeString(AnsiString(static_cast<const char *>(AlgNamePtrLen.ptr), AlgNamePtrLen.len));
    const ssh_keyalg * Alg = find_pubkey_alg_len(AlgNamePtrLen);
    if (Alg == NULL)
    {
      throw Exception(FMTLOAD(PUB_KEY_UNKNOWN, (AlgName)));
    }
    if (Alg->is_certificate)
    {
      throw Exception(FMTLOAD(SSH_HOST_CA_CERTIFICATE, (AlgName)));
    }

    ssh_key * Key = ssh_key_new_pub(Alg, ptrlen_from_strbuf(Blob));
    if (Key == NULL)
    {
      throw Exception(FMTLOAD(SSH_HOST_CA_INVALID, (AlgName)));
    }

    char * FingerprintPtr = ssh2_fingerprint(Key, SSH_FPTYPE_DEFAULT);
    Fingerprint = UnicodeString(FingerprintPtr);
    sfree(FingerprintPtr);
    ssh_key_free(Key);

    PublicKey = StrBufToString(Blob);
  }
  __finally
  {
    strbuf_free(Blob);
  }
}
//---------------------------------------------------------------------------
bool IsCertificateValidityExpressionValid(
  const UnicodeString & Str, UnicodeString & Error, int & ErrorStart, int & ErrorLen)
{
  char * ErrorMsg;
  ptrlen ErrorLoc;
  AnsiString StrAnsi(Str);
  const char * StrPtr = StrAnsi.c_str();
  bool Result = cert_expr_valid(StrPtr, &ErrorMsg, &ErrorLoc);
  if (!Result)
  {
    Error = UnicodeString(ErrorMsg);
    sfree(ErrorMsg);
    ErrorStart = static_cast<const char *>(ErrorLoc.ptr) - StrPtr;
    ErrorLen = ErrorLoc.len;
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
// Same order as DefaultCipherList
struct TCipherGroup
{
  int CipherGroup;
  const ssh2_ciphers * Cipher;
};
static TCipherGroup Ciphers[] =
  {
    { CIPHER_AES, &ssh2_aes },
    { CIPHER_CHACHA20, &ssh2_ccp },
    { CIPHER_AESGCM, &ssh2_aesgcm },
    { CIPHER_3DES, &ssh2_3des },
    { CIPHER_DES, &ssh2_des },
    { CIPHER_BLOWFISH, &ssh2_blowfish },
    { CIPHER_ARCFOUR, &ssh2_arcfour },
  };
//---------------------------------------------------------------------------
TStrings * SshCipherList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  for (unsigned int Index = 0; Index < LENOF(Ciphers); Index++)
  {
    const ssh2_ciphers * Cipher = Ciphers[Index].Cipher;
    for (int Index2 = 0; Index2 < Cipher->nciphers; Index2++)
    {
      UnicodeString Name = UnicodeString(Cipher->list[Index2]->ssh2_id);
      Result->Add(Name);
    }
  }
  return Result.release();
}
//---------------------------------------------------------------------------
int GetCipherGroup(const ssh_cipher * TheCipher)
{
  DebugAssert(strlen(TheCipher->vt->ssh2_id) > 0);
  for (unsigned int Index = 0; Index < LENOF(Ciphers); Index++)
  {
    TCipherGroup & CipherGroup = Ciphers[Index];
    const ssh2_ciphers * Cipher = CipherGroup.Cipher;
    for (int Index2 = 0; Index2 < Cipher->nciphers; Index2++)
    {
      if (strcmp(TheCipher->vt->ssh2_id, Cipher->list[Index2]->ssh2_id) == 0)
      {
        return CipherGroup.CipherGroup;
      }
    }
  }
  DebugFail();
  return -1;
}
//---------------------------------------------------------------------------
TStrings * SshKexList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  // Same order as DefaultKexList
  const ssh_kexes * Kexes[] = {
    &ssh_gssk5_ecdh_kex, &ssh_gssk5_sha2_kex, &ssh_gssk5_sha1_kex,
    &ssh_ntru_hybrid_kex, &ssh_mlkem_curve25519_hybrid_kex, &ssh_mlkem_nist_hybrid_kex, &ssh_ecdh_kex, &ssh_diffiehellman_gex,
    &ssh_diffiehellman_group18, &ssh_diffiehellman_group17, &ssh_diffiehellman_group16, &ssh_diffiehellman_group15, &ssh_diffiehellman_group14,
    &ssh_rsa_kex, &ssh_diffiehellman_group1 };
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
int HostKeyToPutty(THostKey HostKey)
{
  int Result;
  switch (HostKey)
  {
    case hkWarn: Result = HK_WARN; break;
    case hkRSA: Result = HK_RSA; break;
    case hkDSA: Result = hkDSA; break;
    case hkECDSA: Result = HK_ECDSA; break;
    case hkED25519: Result = HK_ED25519; break;
    case hkED448: Result = HK_ED448; break;
    default: Result = -1; DebugFail();
  }
  return Result;
}
//---------------------------------------------------------------------------
TStrings * SshHostKeyList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  for (int DefaultIndex = 0; DefaultIndex < HOSTKEY_COUNT; DefaultIndex++)
  {
    int Type = HostKeyToPutty(DefaultHostKeyList[DefaultIndex]);
    cp_ssh_keyalg * SignKeys;
    int Count;
    get_hostkey_algs(Type, &Count, &SignKeys);
    try
    {
      for (int Index = 0; Index < Count; Index++)
      {
        cp_ssh_keyalg SignKey = SignKeys[Index];
        UnicodeString Name = UnicodeString(SignKey->ssh_id);
        Result->Add(Name);
      }
    }
    __finally
    {
      sfree(SignKeys);
    }
  }
  return Result.release();
}
//---------------------------------------------------------------------------
TStrings * SshMacList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  const struct ssh2_macalg * const * Macs = NULL;
  int Count = 0;
  get_macs(&Count, &Macs);

  for (int Index = 0; Index < Count; Index++)
  {
    UnicodeString Name = UnicodeString(Macs[Index]->name);
    UnicodeString S = Name;
    UnicodeString ETMName = UnicodeString(Macs[Index]->etm_name);
    if (!ETMName.IsEmpty())
    {
      S = FORMAT(L"%s (%s)", (S, ETMName));
    }
    Result->Add(S);
  }
  return Result.release();
}
//---------------------------------------------------------------------------
UnicodeString GetCipherName(const ssh_cipher * Cipher)
{
  return UnicodeString(UTF8String(Cipher->vt->text_name));
}
//---------------------------------------------------------------------------
UnicodeString GetCompressorName(const ssh_compressor * Compressor)
{
  UnicodeString Result;
  if (Compressor != NULL)
  {
    Result = UnicodeString(UTF8String(Compressor->vt->name));
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetDecompressorName(const ssh_decompressor * Decompressor)
{
  UnicodeString Result;
  if (Decompressor != NULL)
  {
    Result = UnicodeString(UTF8String(Decompressor->vt->name));
  }
  return Result;
}
//---------------------------------------------------------------------------
void WritePuttySettings(THierarchicalStorage * Storage, const UnicodeString & ASettings)
{
  if (PuttyRegistryTypes.empty())
  {
    TGuard Guard(PuttyStorageSection.get());
    TValueRestorer<TPuttyRegistryMode> PuttyRegistryModeRestorer(PuttyRegistryMode, prmCollect);
    Conf * conf = conf_new();
    try
    {
      do_defaults(NULL, conf);
      save_settings(NULL, conf);
    }
    __finally
    {
      conf_free(conf);
    }
  }

  std::unique_ptr<TStrings> Settings(new TStringList());
  UnicodeString Buf = ASettings;
  UnicodeString Setting;
  while (CutToken(Buf, Setting))
  {
    Settings->Add(Setting);
  }

  for (int Index = 0; Index < Settings->Count; Index++)
  {
    UnicodeString Name = Settings->Names[Index];
    TPuttyRegistryTypes::const_iterator IType = PuttyRegistryTypes.find(Name);
    if (IType != PuttyRegistryTypes.end())
    {
      UnicodeString Value = Settings->ValueFromIndex[Index];
      int I = 0; // shut up
      if (IType->second == REG_SZ)
      {
        Storage->WriteStringRaw(Name, Value);
      }
      else if (DebugAlwaysTrue(IType->second == REG_DWORD) &&
               TryStrToInt(Value, I))
      {
        Storage->WriteInteger(Name, I);
      }
    }
  }
}
//---------------------------------------------------------------------------
void PuttyDefaults(Conf * conf)
{
  TGuard Guard(PuttyStorageSection.get());
  TValueRestorer<TPuttyRegistryMode> PuttyRegistryModeRestorer(PuttyRegistryMode, prmFail);
  do_defaults(NULL, conf);
}
//---------------------------------------------------------------------------
void SavePuttyDefaults(const UnicodeString & Name)
{
  TGuard Guard(PuttyStorageSection.get());
  TValueRestorer<TPuttyRegistryMode> PuttyRegistryModeRestorer(PuttyRegistryMode, prmPass);
  Conf * conf = conf_new();
  try
  {
    PuttyDefaults(conf);
    AnsiString PuttyName = PuttyStr(Name);
    save_settings(PuttyName.c_str(), conf);
  }
  __finally
  {
    conf_free(conf);
  }
}
//---------------------------------------------------------------------------
struct host_ca_enum
{
  int Index;
};
//---------------------------------------------------------------------------
host_ca_enum * enum_host_ca_start()
{
  Configuration->RefreshPuttySshHostCAList();
  host_ca_enum * Result = new host_ca_enum();
  Result->Index = 0;
  return Result;
}
//---------------------------------------------------------------------------
bool enum_host_ca_next(host_ca_enum * Enum, strbuf * StrBuf)
{
  const TSshHostCAList * SshHostCAList = Configuration->ActiveSshHostCAList;
  bool Result = (Enum->Index < SshHostCAList->GetCount());
  if (Result)
  {
    put_asciz(StrBuf, UTF8String(SshHostCAList->Get(Enum->Index)->Name).c_str());
    Enum->Index++;
  }
  return Result;
}
//---------------------------------------------------------------------------
void enum_host_ca_finish(host_ca_enum * Enum)
{
  delete Enum;
}
//---------------------------------------------------------------------------
host_ca * host_ca_load(const char * NameStr)
{
  host_ca * Result = NULL;
  UnicodeString Name = UTF8String(NameStr);
  const TSshHostCA * SshHostCA = Configuration->ActiveSshHostCAList->Find(Name);
  if (DebugAlwaysTrue(SshHostCA != NULL))
  {
    Result = host_ca_new();
    Result->name = dupstr(NameStr);
    Result->ca_public_key = strbuf_dup(make_ptrlen(SshHostCA->PublicKey.c_str(), SshHostCA->PublicKey.Length()));
    Result->validity_expression = dupstr(UTF8String(SshHostCA->ValidityExpression).c_str());
    Result->opts.permit_rsa_sha1 = SshHostCA->PermitRsaSha1;
    Result->opts.permit_rsa_sha256 = SshHostCA->PermitRsaSha256;
    Result->opts.permit_rsa_sha512 = SshHostCA->PermitRsaSha512;
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
