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
  AnsiString RemoteDirectory;
  bool ForceText;
  TTerminal * Terminal;
  TTerminalQueue * Queue;
  AnsiString SessionName;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileChangedEvent)
  (const AnsiString FileName, const TEditedFileData & Data, HANDLE CompleteEvent);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TEditedFileProcessEvent)
  (const AnsiString FileName, TEditedFileData & Data, void * Arg);
//---------------------------------------------------------------------------
class TEditorManager
{
public:
  __fastcall TEditorManager();
  __fastcall ~TEditorManager();

  bool __fastcall Empty(bool IgnoreClosed);
  bool __fastcall CanAddFile();
  bool __fastcall CloseInternalEditors(TNotifyEvent CloseCallback);

  void __fastcall AddFileInternal(const AnsiString FileName,
    const TEditedFileData & Data, bool * CloseFlag, TObject * Token);
  void __fastcall AddFileExternal(const AnsiString FileName,
    const TEditedFileData & Data, bool * CloseFlag, HANDLE Process);

  void __fastcall Check();

  void __fastcall FileChanged(TObject * Token);
  void __fastcall FileClosed(TObject * Token);

  void __fastcall ProcessFiles(TEditedFileProcessEvent Callback, void * Arg);

  __property TEditedFileChangedEvent OnFileChange = { read = FOnFileChange, write = FOnFileChange };

private:
  struct TFileData
  {
    AnsiString FileName;
    AnsiString LocalDirectory;
    HANDLE Monitor;
    HANDLE Process;
    TObject * Token;
    int Timestamp;
    int ErrorTimestamp;
    TEditedFileData Data;
    bool Closed;
    HANDLE UploadCompleteEvent;
    bool * CloseFlag;
  };

  std::vector<TFileData> FFiles;
  std::vector<HANDLE> FMonitors;
  std::vector<HANDLE> FProcesses;
  std::vector<HANDLE> FUploadCompleteEvents;
  TEditedFileChangedEvent FOnFileChange;

  void __fastcall AddFile(TFileData & FileData);
  void __fastcall UploadComplete(int Index);
  void __fastcall CloseFile(int Index, bool IgnoreErrors);
  void __fastcall CheckFileChange(int Index, bool Force);
  int __fastcall FindFile(const TObject * Token);

  enum TWaitHandle { MONITOR, PROCESS, EVENT };
  int __fastcall WaitFor(unsigned int Count,	const HANDLE * Handles,
    TWaitHandle WaitFor);
};
//---------------------------------------------------------------------------
#endif
