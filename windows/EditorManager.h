//---------------------------------------------------------------------------
#ifndef EditorManagerH
#define EditorManagerH
//---------------------------------------------------------------------------
#include <vector>
//---------------------------------------------------------------------------
class TTerminal;
class TTerminalQueue;
//---------------------------------------------------------------------------
struct TEditedFileData
{
  UnicodeString LocalRootDirectory;
  UnicodeString RemoteDirectory;
  bool ForceText;
  TTerminal * Terminal;
  TTerminalQueue * Queue;
  UnicodeString SessionName;
  UnicodeString OriginalFileName;
  UnicodeString Command;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileChangedEvent)
  (const UnicodeString FileName, const TEditedFileData & Data, HANDLE CompleteEvent);
typedef void __fastcall (__closure * TEditedFileReloadEvent)
  (const UnicodeString FileName, const TEditedFileData & Data);
typedef void __fastcall (__closure * TEditedFileEarlyClosedEvent)
  (const TEditedFileData & Data, bool & KeepOpen);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileProcessEvent)
  (const UnicodeString FileName, TEditedFileData & Data, TObject * Token, void * Arg);
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
    const TEditedFileData & Data, TObject * Token);
  void __fastcall AddFileExternal(const UnicodeString FileName,
    const TEditedFileData & Data, HANDLE Process);

  void __fastcall Check();

  void __fastcall FileChanged(TObject * Token);
  void __fastcall FileReload(TObject * Token);
  void __fastcall FileClosed(TObject * Token, bool Forced);

  void __fastcall ProcessFiles(TEditedFileProcessEvent Callback, void * Arg);

  __property TEditedFileChangedEvent OnFileChange = { read = FOnFileChange, write = FOnFileChange };
  __property TEditedFileReloadEvent OnFileReload = { read = FOnFileReload, write = FOnFileReload };
  __property TEditedFileEarlyClosedEvent OnFileEarlyClosed = { read = FOnFileEarlyClosed, write = FOnFileEarlyClosed };

private:
  struct TFileData
  {
    UnicodeString FileName;
    HANDLE Monitor;
    bool External;
    HANDLE Process;
    TObject * Token;
    TDateTime Timestamp;
    TEditedFileData Data;
    bool Closed;
    HANDLE UploadCompleteEvent;
    TDateTime Opened;
    bool Reupload;
    unsigned int Saves;
  };

  std::vector<TFileData> FFiles;
  std::vector<HANDLE> FMonitors;
  std::vector<HANDLE> FProcesses;
  std::vector<HANDLE> FUploadCompleteEvents;
  TEditedFileChangedEvent FOnFileChange;
  TEditedFileReloadEvent FOnFileReload;
  TEditedFileEarlyClosedEvent FOnFileEarlyClosed;

  void __fastcall AddFile(TFileData & FileData);
  void __fastcall UploadComplete(int Index);
  void __fastcall CloseFile(int Index, bool IgnoreErrors, bool Delete);
  void __fastcall CloseProcess(int Index);
  bool __fastcall EarlyClose(int Index);
  void __fastcall CheckFileChange(int Index, bool Force);
  int __fastcall FindFile(const TObject * Token);

  enum TWaitHandle { MONITOR, PROCESS, EVENT };
  int __fastcall WaitFor(unsigned int Count, const HANDLE * Handles,
    TWaitHandle WaitFor);
};
//---------------------------------------------------------------------------
#endif
