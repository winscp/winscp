//---------------------------------------------------------------------------
#ifndef SecureShellH
#define SecureShellH

#include <set>
#include "Configuration.h"
#include "SessionData.h"
#include "SessionInfo.h"
//---------------------------------------------------------------------------
#ifndef PuttyIntfH
struct Backend;
struct Config;
#endif
//---------------------------------------------------------------------------
struct _WSANETWORKEVENTS;
typedef struct _WSANETWORKEVENTS WSANETWORKEVENTS;
typedef UINT_PTR SOCKET;
typedef std::set<SOCKET> TSockets;
struct TPuttyTranslation;
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
  Backend * FBackend;
  void * FBackendHandle;
  const unsigned int * FMaxPacketSize;
  Config * FConfig;
  TNotifyEvent FOnReceive;
  bool FFrozen;
  bool FDataWhileFrozen;
  bool FStoredPasswordTried;
  bool FStoredPasswordTriedForKI;
  int FSshVersion;
  bool FOpened;
  int FWaiting;
  bool FSimple;

  unsigned PendLen;
  unsigned PendSize;
  unsigned OutLen;
  char * OutPtr;
  char * Pending;
  TSessionLog * FLog;
  TConfiguration * FConfiguration;
  bool FAuthenticating;
  bool FAuthenticated;
  AnsiString FStdErrorTemp;
  AnsiString FStdError;
  AnsiString FCWriteTemp;
  AnsiString FAuthenticationLog;
  AnsiString FLastTunnelError;
  AnsiString FUserName;

  static TCipher __fastcall FuncToSsh1Cipher(const void * Cipher);
  static TCipher __fastcall FuncToSsh2Cipher(const void * Cipher);
  AnsiString __fastcall FuncToCompression(int SshVersion, const void * Compress) const;
  void __fastcall Init();
  void __fastcall SetActive(bool value);
  void inline __fastcall CheckConnection(int Message = -1);
  void __fastcall WaitForData();
  void __fastcall Discard();
  void __fastcall FreeBackend();
  void __fastcall PoolForData(WSANETWORKEVENTS & Events, unsigned int & Result);
  inline void __fastcall CaptureOutput(TLogLineType Type,
    const AnsiString & Line);
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
  int __fastcall TimeoutPrompt(TQueryParamsTimerEvent PoolEvent);

protected:
  TCaptureOutputEvent FOnCaptureOutput;

  void __fastcall GotHostKey();
  int __fastcall TranslatePuttyMessage(const TPuttyTranslation * Translation,
    size_t Count, AnsiString & Message);
  int __fastcall TranslateAuthenticationMessage(AnsiString & Message);
  int __fastcall TranslateErrorMessage(AnsiString & Message);
  void __fastcall AddStdError(AnsiString Str);
  void __fastcall AddStdErrorLine(const AnsiString & Str);
  void __fastcall FatalError(Exception * E, AnsiString Msg);
  void __fastcall inline LogEvent(const AnsiString & Str);
  void __fastcall FatalError(AnsiString Error);
  static void __fastcall ClearConfig(Config * cfg);
  static void __fastcall StoreToConfig(TSessionData * Data, Config * cfg, bool Simple);

public:
  __fastcall TSecureShell(TSessionUI * UI, TSessionData * SessionData,
    TSessionLog * Log, TConfiguration * Configuration);
  __fastcall ~TSecureShell();
  void __fastcall Open();
  void __fastcall Close();
  void __fastcall KeepAlive();
  int __fastcall Receive(char * Buf, int Len);
  bool __fastcall Peek(char *& Buf, int Len);
  AnsiString __fastcall ReceiveLine();
  void __fastcall Send(const char * Buf, int Len);
  void __fastcall SendStr(AnsiString Str);
  void __fastcall SendSpecial(int Code);
  void __fastcall Idle(unsigned int MSec = 0);
  void __fastcall SendEOF();
  void __fastcall SendLine(AnsiString Line);
  void __fastcall SendNull();

  const TSessionInfo & __fastcall GetSessionInfo();
  bool __fastcall SshFallbackCmd() const;
  unsigned long __fastcall MaxPacketSize();
  void __fastcall ClearStdError();
  bool __fastcall GetStoredCredentialsTried();

  void __fastcall RegisterReceiveHandler(TNotifyEvent Handler);
  void __fastcall UnregisterReceiveHandler(TNotifyEvent Handler);

  // interface to PuTTY core
  void __fastcall UpdateSocket(SOCKET value, bool Startup);
  void __fastcall UpdatePortFwdSocket(SOCKET value, bool Startup);
  void __fastcall PuttyFatalError(AnsiString Error);
  bool __fastcall PromptUser(bool ToServer,
    AnsiString AName, bool NameRequired,
    AnsiString Instructions, bool InstructionsRequired,
    TStrings * Prompts, TStrings * Results);
  void __fastcall FromBackend(bool IsStdErr, const char * Data, int Length);
  void __fastcall CWrite(const char * Data, int Length);
  const AnsiString & __fastcall GetStdError();
  void __fastcall VerifyHostKey(AnsiString Host, int Port,
    const AnsiString KeyType, AnsiString KeyStr, const AnsiString Fingerprint);
  void __fastcall AskAlg(const AnsiString AlgType, const AnsiString AlgName);
  void __fastcall DisplayBanner(const AnsiString & Banner);
  void __fastcall OldKeyfileWarning();
  void __fastcall PuttyLogEvent(const AnsiString & Str);

  __property bool Active = { read = FActive, write = SetActive };
  __property bool Ready = { read = GetReady };
  __property TCaptureOutputEvent OnCaptureOutput = { read = FOnCaptureOutput, write = FOnCaptureOutput };
  __property TDateTime LastDataSent = { read = FLastDataSent };
  __property AnsiString LastTunnelError = { read = FLastTunnelError };
  __property AnsiString UserName = { read = FUserName };
  __property bool Simple = { read = FSimple, write = FSimple };
};
//---------------------------------------------------------------------------
#endif
