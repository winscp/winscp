//---------------------------------------------------------------------------
#ifndef EditorManagerH
#define EditorManagerH
//---------------------------------------------------------------------------
#include <vector>
//---------------------------------------------------------------------------
class TManagedTerminal;
class TTerminalQueue;
//---------------------------------------------------------------------------
struct TEditedFileData
{
  TEditedFileData();
  ~TEditedFileData();

  UnicodeString LocalRootDirectory;
  UnicodeString RemoteDirectory;
  bool ForceText;
  TManagedTerminal * Terminal;
  TSessionData * SessionData;
  UnicodeString SessionName;
  UnicodeString OriginalFileName;
  UnicodeString Command;
  TDateTime SourceTimestamp;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileChangedEvent)
  (const UnicodeString & FileName, const TDateTime & Timestamp, TEditedFileData * Data, HANDLE CompleteEvent, bool & Retry);
typedef void __fastcall (__closure * TEditedFileReloadEvent)
  (const UnicodeString & FileName, TEditedFileData * Data);
typedef void __fastcall (__closure * TEditedFileEarlyClosedEvent)
  (const TEditedFileData * Data, bool & KeepOpen);
typedef void __fastcall (__closure * TEditedFileUploadComplete)
  (TObject * Token);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileProcessEvent)
  (const UnicodeString FileName, TEditedFileData * Data, TObject * Token, void * Arg);
//---------------------------------------------------------------------------
class TEditorManager
{
public:
  __fastcall TEditorManager();
  __fastcall ~TEditorManager();

  bool __fastcall Empty(bool IgnoreClosed);
  bool __fastcall CanAddFile(const UnicodeString RemoteDirectory,
    const UnicodeString OriginalFileName, const UnicodeString SessionName,
    TObject *& Token, UnicodeString & ExistingLocalRootDirectory,
    UnicodeString & ExistingLocalDirectory);
  bool __fastcall CloseInternalEditors(TNotifyEvent CloseCallback);
  bool __fastcall CloseExternalFilesWithoutProcess();

  void __fastcall AddFileInternal(const UnicodeString FileName,
    TEditedFileData * Data, TObject * Token);
  void __fastcall AddFileExternal(const UnicodeString FileName,
    TEditedFileData * Data, HANDLE Process);

  void __fastcall Check();

  void __fastcall FileChanged(TObject * Token);
  void __fastcall FileReload(TObject * Token);
  void __fastcall FileClosed(TObject * Token, bool Forced);

  void __fastcall ProcessFiles(TEditedFileProcessEvent Callback, void * Arg);
  TEditedFileData * FindByUploadCompleteEvent(HANDLE UploadCompleteEvent);

  __property TEditedFileChangedEvent OnFileChange = { read = FOnFileChange, write = FOnFileChange };
  __property TEditedFileReloadEvent OnFileReload = { read = FOnFileReload, write = FOnFileReload };
  __property TEditedFileEarlyClosedEvent OnFileEarlyClosed = { read = FOnFileEarlyClosed, write = FOnFileEarlyClosed };
  __property TEditedFileUploadComplete OnFileUploadComplete = { read = FOnFileUploadComplete, write = FOnFileUploadComplete };
  __property TCriticalSection * Section = { read = FSection };

private:
  struct TFileData
  {
    UnicodeString FileName;
    bool External;
    HANDLE Process;
    TObject * Token;
    TDateTime Timestamp;
    TEditedFileData * Data;
    bool Closed;
    HANDLE UploadCompleteEvent;
    TDateTime Opened;
    bool Reupload;
    bool Reloading;
    unsigned int Saves;
  };

  std::vector<TFileData> FFiles;
  std::vector<HANDLE> FProcesses;
  std::vector<HANDLE> FUploadCompleteEvents;
  TEditedFileChangedEvent FOnFileChange;
  TEditedFileReloadEvent FOnFileReload;
  TEditedFileEarlyClosedEvent FOnFileEarlyClosed;
  TEditedFileUploadComplete FOnFileUploadComplete;
  TCriticalSection * FSection;

  void __fastcall AddFile(TFileData & FileData, TEditedFileData * Data);
  void UploadComplete(int Index, bool Retry);
  bool __fastcall CloseFile(int Index, bool IgnoreErrors, bool Delete);
  void __fastcall CloseProcess(int Index);
  bool __fastcall EarlyClose(int Index);
  bool __fastcall HasFileChanged(int Index, TDateTime & NewTimestamp);
  void __fastcall CheckFileChange(int Index, bool Force);
  int __fastcall FindFile(const TObject * Token);
  void __fastcall ReleaseFile(int Index);
  TDateTime NormalizeTimestamp(const TDateTime & Timestamp);
  bool GetFileTimestamp(const UnicodeString & FileName, TDateTime & Timestamp);

  enum TWaitHandle { PROCESS, EVENT };
  int __fastcall WaitFor(unsigned int Count, const HANDLE * Handles,
    TWaitHandle WaitFor);
};
//---------------------------------------------------------------------------
#endif
