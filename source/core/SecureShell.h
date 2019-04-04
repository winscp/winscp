//---------------------------------------------------------------------------
#ifndef SecureShellH
#define SecureShellH

#include <set>
#include "Configuration.h"
#include "SessionData.h"
#include "SessionInfo.h"
//---------------------------------------------------------------------------
#ifndef PuttyIntfH
struct Backend_vtable;
struct Backend;
struct Conf;
#endif
//---------------------------------------------------------------------------
struct _WSANETWORKEVENTS;
typedef struct _WSANETWORKEVENTS WSANETWORKEVENTS;
typedef UINT_PTR SOCKET;
typedef std::set<SOCKET> TSockets;
struct TPuttyTranslation;
struct callback_set;
enum TSshImplementation { sshiUnknown, sshiOpenSSH, sshiProFTPD, sshiBitvise, sshiTitan, sshiOpenVMS, sshiCerberus };
//---------------------------------------------------------------------------
class TSecureShell
{
friend class TPoolForDataEvent;

private:
  SOCKET FSocket;
  HANDLE FSocketEvent;
  TSockets FPortFwdSockets;
  TSessionUI * FUI;
  TSessionData * FSessionData;
  bool FActive;
  TSessionInfo FSessionInfo;
  bool FSessionInfoValid;
  TDateTime FLastDataSent;
  const Backend_vtable * FBackend;
  Backend * FBackendHandle;
  const unsigned int * FMaxPacketSize;
  TNotifyEvent FOnReceive;
  bool FFrozen;
  bool FDataWhileFrozen;
  bool FStoredPasswordTried;
  bool FStoredPasswordTriedForKI;
  bool FStoredPassphraseTried;
  int FSshVersion;
  bool FOpened;
  int FWaiting;
  bool FSimple;
  bool FNoConnectionResponse;
  bool FCollectPrivateKeyUsage;
  int FWaitingForData;
  TSshImplementation FSshImplementation;

  unsigned PendLen;
  unsigned PendSize;
  unsigned OutLen;
  unsigned char * OutPtr;
  unsigned char * Pending;
  TSessionLog * FLog;
  TConfiguration * FConfiguration;
  bool FAuthenticating;
  bool FAuthenticated;
  UnicodeString FStdErrorTemp;
  UnicodeString FStdError;
  UnicodeString FCWriteTemp;
  UnicodeString FAuthenticationLog;
  UnicodeString FLastTunnelError;
  UnicodeString FUserName;
  bool FUtfStrings;
  DWORD FLastSendBufferUpdate;
  int FSendBuf;
  std::auto_ptr<callback_set> FCallbackSet;

  static TCipher __fastcall FuncToSsh1Cipher(const void * Cipher);
  static TCipher __fastcall FuncToSsh2Cipher(const void * Cipher);
  UnicodeString __fastcall FuncToCompression(int SshVersion, const void * Compress) const;
  void __fastcall Init();
  void __fastcall SetActive(bool value);
  void inline __fastcall CheckConnection(int Message = -1);
  void __fastcall WaitForData();
  void __fastcall Discard();
  void __fastcall FreeBackend();
  void __fastcall PoolForData(WSANETWORKEVENTS & Events, unsigned int & Result);
  inline void __fastcall CaptureOutput(TLogLineType Type,
    const UnicodeString & Line);
  void __fastcall ResetConnection();
  void __fastcall ResetSessionInfo();
  void __fastcall SocketEventSelect(SOCKET Socket, HANDLE Event, bool Startup);
  bool __fastcall EnumNetworkEvents(SOCKET Socket, WSANETWORKEVENTS & Events);
  void __fastcall HandleNetworkEvents(SOCKET Socket, WSANETWORKEVENTS & Events);
  bool __fastcall ProcessNetworkEvents(SOCKET Socket);
  bool __fastcall EventSelectLoop(unsigned int MSec, bool ReadEventRequired,
    WSANETWORKEVENTS * Events);
  void __fastcall UpdateSessionInfo();
  bool __fastcall GetReady();
  void __fastcall DispatchSendBuffer(int BufSize);
  void __fastcall SendBuffer(unsigned int & Result);
  unsigned int __fastcall TimeoutPrompt(TQueryParamsTimerEvent PoolEvent);
  bool __fastcall TryFtp();
  UnicodeString __fastcall ConvertInput(const RawByteString & Input);
  void __fastcall GetRealHost(UnicodeString & Host, int & Port);
  UnicodeString __fastcall RetrieveHostKey(UnicodeString Host, int Port, const UnicodeString KeyType);

protected:
  TCaptureOutputEvent FOnCaptureOutput;

  void __fastcall GotHostKey();
  int __fastcall TranslatePuttyMessage(const TPuttyTranslation * Translation,
    size_t Count, UnicodeString & Message, UnicodeString * HelpKeyword = NULL);
  int __fastcall TranslateAuthenticationMessage(UnicodeString & Message, UnicodeString * HelpKeyword = NULL);
  int __fastcall TranslateErrorMessage(UnicodeString & Message, UnicodeString * HelpKeyword = NULL);
  void __fastcall AddStdError(UnicodeString Str);
  void __fastcall AddStdErrorLine(const UnicodeString & Str);
  void __fastcall inline LogEvent(const UnicodeString & Str);
  void __fastcall FatalError(UnicodeString Error, UnicodeString HelpKeyword = L"");
  UnicodeString __fastcall FormatKeyStr(UnicodeString KeyStr);
  static Conf * __fastcall StoreToConfig(TSessionData * Data, bool Simple);

public:
  __fastcall TSecureShell(TSessionUI * UI, TSessionData * SessionData,
    TSessionLog * Log, TConfiguration * Configuration);
  __fastcall ~TSecureShell();
  void __fastcall Open();
  void __fastcall Close();
  void __fastcall KeepAlive();
  int __fastcall Receive(unsigned char * Buf, int Len);
  bool __fastcall Peek(unsigned char *& Buf, int Len);
  UnicodeString __fastcall ReceiveLine();
  void __fastcall Send(const unsigned char * Buf, int Len);
  void __fastcall SendSpecial(int Code);
  void __fastcall Idle(unsigned int MSec = 0);
  void __fastcall SendEOF();
  void __fastcall SendLine(const UnicodeString & Line);
  void __fastcall SendNull();

  const TSessionInfo & __fastcall GetSessionInfo();
  void __fastcall GetHostKeyFingerprint(UnicodeString & SHA256, UnicodeString & MD5);
  bool __fastcall SshFallbackCmd() const;
  unsigned long __fastcall MaxPacketSize();
  void __fastcall ClearStdError();
  bool __fastcall GetStoredCredentialsTried();
  void __fastcall CollectUsage();
  bool __fastcall CanChangePassword();

  void __fastcall RegisterReceiveHandler(TNotifyEvent Handler);
  void __fastcall UnregisterReceiveHandler(TNotifyEvent Handler);

  // interface to PuTTY core
  void __fastcall UpdateSocket(SOCKET value, bool Startup);
  void __fastcall UpdatePortFwdSocket(SOCKET value, bool Startup);
  void __fastcall PuttyFatalError(UnicodeString Error);
  TPromptKind __fastcall IdentifyPromptKind(UnicodeString & Name);
  bool __fastcall PromptUser(bool ToServer,
    UnicodeString AName, bool NameRequired,
    UnicodeString Instructions, bool InstructionsRequired,
    TStrings * Prompts, TStrings * Results);
  void __fastcall FromBackend(bool IsStdErr, const unsigned char * Data, int Length);
  void __fastcall CWrite(const char * Data, int Length);
  const UnicodeString & __fastcall GetStdError();
  void __fastcall VerifyHostKey(
    const UnicodeString & Host, int Port, const UnicodeString & KeyType, const UnicodeString & KeyStr,
    const UnicodeString & Fingerprint);
  bool __fastcall HaveHostKey(UnicodeString Host, int Port, const UnicodeString KeyType);
  void __fastcall AskAlg(UnicodeString AlgType, UnicodeString AlgName);
  void __fastcall DisplayBanner(const UnicodeString & Banner);
  void __fastcall OldKeyfileWarning();
  void __fastcall PuttyLogEvent(const char * Str);
  UnicodeString __fastcall ConvertFromPutty(const char * Str, int Length);
  struct callback_set * GetCallbackSet();

  __property bool Active = { read = FActive, write = SetActive };
  __property bool Ready = { read = GetReady };
  __property TCaptureOutputEvent OnCaptureOutput = { read = FOnCaptureOutput, write = FOnCaptureOutput };
  __property TDateTime LastDataSent = { read = FLastDataSent };
  __property UnicodeString LastTunnelError = { read = FLastTunnelError };
  __property UnicodeString UserName = { read = FUserName };
  __property bool Simple = { read = FSimple, write = FSimple };
  __property TSshImplementation SshImplementation = { read = FSshImplementation };
  __property bool UtfStrings = { read = FUtfStrings, write = FUtfStrings };
};
//---------------------------------------------------------------------------
#endif
