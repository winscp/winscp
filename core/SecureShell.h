//---------------------------------------------------------------------------
#ifndef SecureShellH
#define SecureShellH

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif

#include "Interface.h"
#include "Configuration.h"
#include "Exceptions.h"
#include "SessionData.h"
#define SSH_ERROR(x) throw ESsh(NULL, x)
#define SSH_FATAL_ERROR_EXT(E, x) throw ESshFatal(E, x)
#define SSH_FATAL_ERROR(x) SSH_FATAL_ERROR_EXT(NULL, x)

#define sshClosed 0
#define sshInitWinSock 1
#define sshLookupHost 2
#define sshConnect 3
#define sshAuthenticate 4
#define sshAuthenticated 5
#define sshStartup 6
#define sshOpenDirectory 7
#define sshReady 8
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TQueryUserEvent)
  (TObject* Sender, const AnsiString Query, TStrings * MoreMessages, int Answers,
   int Params, int & Answer, TQueryType QueryType);
//---------------------------------------------------------------------------
class TSessionLog;
class TConfiguration;
enum TCompressionType { ctNone, ctZLib };
//---------------------------------------------------------------------------
// Duplicated in LogMemo.h for design-time-only purposes
enum TLogLineType {llOutput, llInput, llStdError, llMessage, llException};
typedef Set<TLogLineType, llOutput, llException> TLogLineTypes;
extern const TColor LogLineColors[];
typedef void __fastcall (__closure *TLogAddLineEvent)(System::TObject* Sender, const AnsiString AddedLine);
//---------------------------------------------------------------------------
class TSessionLog : public TStringList
{
private:
  TConfiguration * FConfiguration;
  TSessionData * FData;
  void * FFile;
  AnsiString FFileName;
  Integer FLoggedLines;
  TLogAddLineEvent FOnAddLine;
  Integer FTopIndex;
  void __fastcall SetLine(Integer Index, AnsiString value);
  AnsiString __fastcall GetLine(Integer Index);
  void __fastcall SetType(Integer Index, TLogLineType value);
  TLogLineType __fastcall GetType(Integer Index);
  void SetData(TSessionData * value);
  void DeleteUnnecessary();
  void OpenLogFile();
  TColor __fastcall GetColor(Integer Index);
  void __fastcall DoAddLine(const AnsiString AddedLine);
  Integer __fastcall GetBottomIndex();
  Integer __fastcall GetIndexes(Integer Index);
  AnsiString __fastcall GetLogFileName();
  Boolean __fastcall GetLoggingToFile();
  Boolean __fastcall GetLogToFile();
  void __fastcall SetConfiguration(TConfiguration * value);

public:
  HIDESBASE void __fastcall Add(TLogLineType aType, AnsiString aLine);
  void __fastcall AddStartupInfo();
  __fastcall TSessionLog();
  __fastcall ~TSessionLog();
  void __fastcall AddException(Exception * E);
  void __fastcall AddSeparator();
  virtual void __fastcall Clear();
  void __fastcall ReflectSettings();
  __property Integer BottomIndex = { read = GetBottomIndex };
  __property TSessionData * Data  = { read=FData, write=SetData };
  __property AnsiString Line[Integer Index]  = { read=GetLine, write=SetLine };
  __property TLogLineType Type[Integer Index]  = { read=GetType, write=SetType };
  __property TColor Color[Integer Index]  = { read=GetColor };
  __property TConfiguration * Configuration = { read = FConfiguration, write = SetConfiguration };
  __property OnChange;
  __property Integer Indexes[Integer Index] = { read = GetIndexes };
  __property AnsiString LogFileName = { read = GetLogFileName };
  __property Integer LoggedLines = { read = FLoggedLines };
  __property Boolean LoggingToFile = { read = GetLoggingToFile };
  __property TLogAddLineEvent OnAddLine = { read = FOnAddLine, write = FOnAddLine };
  __property Integer TopIndex = { read = FTopIndex };
protected:
  void __fastcall CloseLogFile();
  __property Boolean LogToFile = { read = GetLogToFile };
};
//---------------------------------------------------------------------------
#ifndef PuttyIntfH
struct Backend;
struct Config;
#endif
//---------------------------------------------------------------------------
class TSecureShell : public TObject
{
private:
  bool FPasswordTried;
  SOCKET FSocket;
  TSessionData * FSessionData;
  bool FActive;
  __int64 FBytesReceived;
  __int64 FBytesSent;
  AnsiString FRealHost;
  TDateTime FLastDataSent;
  TQueryUserEvent FOnQueryUser;
  Backend * FBackend;
  void * FBackendHandle;
  Config * FConfig;

  unsigned PendLen;
  unsigned PendSize;
  unsigned OutLen;
  char * OutPtr;
  char * Pending;
  TSessionLog * FLog;
  TConfiguration *FConfiguration;
  TDateTime FLoginTime;
  TNotifyEvent FOnUpdateStatus;
  TNotifyEvent FOnClose;
  int FStatus;
  int FReachedStatus;
  AnsiString FStdErrorTemp;
  AnsiString FAuthenticationLog;
  TObject * FUserObject;

  TCipher FCSCipher;
  TCipher FSCCipher;

  TCipher __fastcall FuncToSsh1Cipher(const void * Cipher);
  TCipher __fastcall FuncToSsh2Cipher(const void * Cipher);
  TCompressionType __fastcall FuncToCompression(const void * Compress);
  void __fastcall Init();
  void __fastcall SetSocket(SOCKET value);
  void __fastcall SetSessionData(TSessionData * value);
  void __fastcall SetActive(Boolean value);
  bool __fastcall GetActive();
  TCipher __fastcall GetCSCipher();
  TCompressionType __fastcall GetCSCompression();
  TDateTime __fastcall GetDuration();
  TCipher __fastcall GetSCCipher();
  TCompressionType __fastcall GetSCCompression();
  int __fastcall GetSshVersion();
  int __fastcall GetStatus();
  void inline __fastcall CheckConnection(int Message = -1);
  void __fastcall WaitForData();
  void __fastcall SetLog(TSessionLog * value);
  void __fastcall SetConfiguration(TConfiguration * value);
  void __fastcall SetUserObject(TObject * value);

protected:
  AnsiString StdError;
  void __fastcall Error(AnsiString Error);
  virtual void __fastcall UpdateStatus(Integer Value);
  bool __fastcall SshFallbackCmd();
  void __fastcall GotHostKey();

public:
  __fastcall TSecureShell();
  __fastcall ~TSecureShell();
  virtual void __fastcall Open();
  virtual void __fastcall Close();
  Integer __fastcall GetPassword(AnsiString &Password);
  Integer __fastcall Receive(char * Buf, Integer Len);
  AnsiString __fastcall ReceiveLine();
  void __fastcall Send(const char * Buf, Integer Len);
  void __fastcall SendStr(AnsiString Str);
  void __fastcall SendSpecial(int Code);
  void __fastcall AddStdError(AnsiString Str);
  void __fastcall ClearStdError();
  void __fastcall Idle();
  void __fastcall SendEOF();
  void __fastcall SendLine(AnsiString Line);
  void __fastcall FatalError(Exception * E, AnsiString Msg);
  void __fastcall SendNull();

  void __fastcall FatalError(AnsiString Error);
  void __fastcall FromBackend(Boolean IsStdErr, char * Data, Integer Length);
  void __fastcall VerifyHostKey(const AnsiString Host, int Port,
    const AnsiString KeyType, const AnsiString KeyStr, const AnsiString Fingerprint);
  void __fastcall AskCipher(const AnsiString CipherName, int CipherType);
  void __fastcall OldKeyfileWarning();

  int __fastcall DoQueryUser(const AnsiString Query, const AnsiString OtherMessage,
    int Answers, int Params);
  int __fastcall DoQueryUser(const AnsiString Query, int Answers, int Params);
  int __fastcall DoQueryUser(const AnsiString Query, TStrings * MoreMessages,
    int Answers, int Params, TQueryType Type = qtConfirmation);
  int __fastcall DoQueryUser(const AnsiString Query, Exception * E,
    int Answers, int Params);

  bool __fastcall inline IsLogging()
  {
    return Configuration->Logging || Log->OnAddLine;
  }
  void __fastcall inline LogEvent(const AnsiString & Str)
  {
    if (IsLogging()) Log->Add(llMessage, Str);
  }

  __property SOCKET Socket = { read = FSocket, write = SetSocket };
  __property TSessionData * SessionData = { read = FSessionData, write = SetSessionData };
  __property bool Active = { read = GetActive, write = SetActive };
  __property __int64 BytesReceived = { read = FBytesReceived };
  __property __int64 BytesSent = { read = FBytesSent };
  __property AnsiString RealHost = { read = FRealHost };
  __property TSessionLog * Log  = { read=FLog, write=SetLog };
  __property TConfiguration * Configuration  = { read=FConfiguration, write=SetConfiguration };
  __property TCipher CSCipher = { read = GetCSCipher };
  __property TCompressionType CSCompression = { read = GetCSCompression };
  __property TDateTime Duration = { read = GetDuration };
  __property TDateTime LoginTime = { read = FLoginTime };
  __property TCipher SCCipher = { read = GetSCCipher };
  __property TCompressionType SCCompression = { read = GetSCCompression };
  __property int SshVersion = { read = GetSshVersion };
  __property TQueryUserEvent OnQueryUser = { read = FOnQueryUser, write = FOnQueryUser };
  __property TNotifyEvent OnUpdateStatus = { read = FOnUpdateStatus, write = FOnUpdateStatus };
  __property TNotifyEvent OnClose = { read = FOnClose, write = FOnClose };
  __property int Status = { read = GetStatus };
  __property TObject * UserObject = { read = FUserObject, write = SetUserObject };
};
//---------------------------------------------------------------------------
#endif
