//---------------------------------------------------------------------------
#ifndef GUIToolsH
#define GUIToolsH
//---------------------------------------------------------------------------
#include <FileMasks.H>
#include <Tbx.hpp>
#include <DirectoryMonitor.hpp>
//---------------------------------------------------------------------------
class TSessionData;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessMessagesEvent)();
//---------------------------------------------------------------------------
void GUIFinalize();
bool __fastcall FindFile(UnicodeString & Path);
UnicodeString FindPuttyPath();
bool __fastcall FindTool(const UnicodeString & Name, UnicodeString & Path);
void __fastcall ExecuteTool(const UnicodeString & Name);
void __fastcall ExecuteShellChecked(const UnicodeString Path, const UnicodeString Params,
  bool ChangeWorkingDirectory = false);
void __fastcall ExecuteShellChecked(const UnicodeString Command);
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params,
  HANDLE & Handle);
void __fastcall ExecuteShellCheckedAndWait(const UnicodeString Command, TProcessMessagesEvent ProcessMessages);
TObjectList * StartCreationDirectoryMonitorsOnEachDrive(unsigned int Filter, TFileChangedEvent OnChanged);
extern bool DontCopyCommandToClipboard;
bool __fastcall CopyCommandToClipboard(const UnicodeString & Command);
bool DoesSessionExistInPutty(const UnicodeString & StorageKey);
bool __fastcall ExportSessionToPutty(TSessionData * SessionData, bool ReuseExisting, const UnicodeString & SessionName);
void OpenSessionInPutty(TSessionData * SessionData);
bool __fastcall SpecialFolderLocation(int PathID, UnicodeString & Path);
UnicodeString __fastcall UniqTempDir(const UnicodeString BaseDir,
  const UnicodeString Identity, bool Mask = false);
int __fastcall GetSessionColorImage(TCustomImageList * ImageList, TColor Color, int MaskIndex);
void __fastcall RegenerateSessionColorsImageList(TCustomImageList * ImageList, int MaskIndex);
void __fastcall SetSubmenu(TTBXCustomItem * Item, bool Enable);
typedef int __fastcall (*TCalculateWidth)(UnicodeString Text, void * Arg);
void __fastcall ApplyTabs(
  UnicodeString & Text, wchar_t Padding,
  TCalculateWidth CalculateWidth, void * CalculateWidthArg);
TPanel * __fastcall CreateLabelPanel(TPanel * Parent, const UnicodeString & Label);
void __fastcall SelectScaledImageList(TImageList * ImageList);
void __fastcall CopyImageList(TImageList * TargetList, TImageList * SourceList);
void __fastcall LoadDialogImage(TImage * Image, const UnicodeString & ImageName);
int __fastcall DialogImageSize(TForm * Form);
int NormalizePixelsPerInch(int PixelsPerInch);
int LargerPixelsPerInch(int PixelsPerInch, int Larger);
void __fastcall HideComponentsPanel(TForm * Form);
struct TIncrementalSearchState
{
  TIncrementalSearchState();
  void Reset();

  bool Searching;
  UnicodeString Text;
  bool HaveNext;
};
UnicodeString FormatIncrementalSearchStatus(const TIncrementalSearchState & SearchState);
namespace Webbrowserex
{
  class TWebBrowserEx;
}
using namespace Webbrowserex;
TWebBrowserEx * __fastcall CreateBrowserViewer(TPanel * Parent, const UnicodeString & LoadingLabel);
void __fastcall SetBrowserDesignModeOff(TWebBrowserEx * WebBrowser);
void __fastcall AddBrowserLinkHandler(TWebBrowserEx * WebBrowser,
  const UnicodeString & Url, TNotifyEvent Handler);
void __fastcall NavigateBrowserToUrl(TWebBrowserEx * WebBrowser, const UnicodeString & Url);
void ReadyBrowserForStreaming(TWebBrowserEx * WebBrowser);
void WaitBrowserToIdle(TWebBrowserEx * WebBrowser);
void HideBrowserScrollbars(TWebBrowserEx * WebBrowser);
bool CopyTextFromBrowser(TWebBrowserEx * WebBrowser, UnicodeString & Text);
UnicodeString GenerateAppHtmlPage(TFont * Font, TPanel * Parent, const UnicodeString & Body, bool Seamless);
void LoadBrowserDocument(TWebBrowserEx * WebBrowser, const UnicodeString & Document);
TComponent * __fastcall FindComponentRecursively(TComponent * Root, const UnicodeString & Name);
void __fastcall GetInstrutionsTheme(
  TColor & MainInstructionColor, HFONT & MainInstructionFont, HFONT & InstructionFont);
bool CanShowTimeEstimate(TDateTime StartTime);
//---------------------------------------------------------------------------
class TLocalCustomCommand : public TFileCustomCommand
{
public:
  TLocalCustomCommand();
  TLocalCustomCommand(
    const TCustomCommandData & Data, const UnicodeString & RemotePath, const UnicodeString & LocalPath);
  TLocalCustomCommand(
    const TCustomCommandData & Data, const UnicodeString & RemotePath, const UnicodeString & LocalPath,
    const UnicodeString & FileName, const UnicodeString & LocalFileName,
    const UnicodeString & FileList);

  virtual bool __fastcall IsFileCommand(const UnicodeString & Command);
  bool __fastcall HasLocalFileName(const UnicodeString & Command);

protected:
  virtual int __fastcall PatternLen(const UnicodeString & Command, int Index);
  virtual bool __fastcall PatternReplacement(int Index, const UnicodeString & Pattern,
    UnicodeString & Replacement, bool & Delimit);
  virtual void __fastcall DelimitReplacement(UnicodeString & Replacement, wchar_t Quote);

private:
  UnicodeString FLocalPath;
  UnicodeString FLocalFileName;
};
//---------------------------------------------------------------------------
namespace Pngimagelist
{
  class TPngImageList;
  class TPngImageCollectionItem;
}
using namespace Pngimagelist;
//---------------------------------------------------------------------------
TPngImageList * __fastcall GetAnimationsImages(TControl * Control);
TImageList * __fastcall GetButtonImages(TControl * Control);
TPngImageList * __fastcall GetDialogImages(TControl * Control);
TCustomImageList * TreeViewImageList(TPngImageList * ImageList);
void __fastcall ReleaseImagesModules();
//---------------------------------------------------------------------------
class TFrameAnimation
{
public:
  __fastcall TFrameAnimation();
  void __fastcall Init(TPaintBox * PaintBox, const UnicodeString & Name);
  void __fastcall Start();
  void __fastcall Stop();

private:
  UnicodeString FName;
  TPaintBox * FPaintBox;
  TPngImageList * FImageList;
  int FFirstFrame;
  int FFirstLoopFrame;
  int FLastFrame;
  int FCurrentFrame;
  DWORD FNextFrameTick;
  TTimer * FTimer;
  bool FPainted;

  void __fastcall DoInit();
  void __fastcall PaintBoxPaint(TObject * Sender);
  void __fastcall CalculateNextFrameTick();
  TPngImageCollectionItem * __fastcall GetCurrentImage();
  void __fastcall Animate();
  void __fastcall Timer(TObject * Sender);
  void __fastcall Repaint();
  void __fastcall Rescale();
  static void __fastcall PaintBoxRescale(TComponent * Sender, TObject * Token);
};
//---------------------------------------------------------------------------
class TScreenTipHintWindow : public THintWindow
{
public:
  __fastcall TScreenTipHintWindow(TComponent * Owner);
  virtual TRect __fastcall CalcHintRect(int MaxWidth, const UnicodeString AHint, void * AData);
  virtual void __fastcall ActivateHintData(const TRect & Rect, const UnicodeString AHint, void * AData);

  static void __fastcall CalcHintTextRect(TControl * Control, TCanvas * Canvas, TRect & Rect, const UnicodeString & Hint);

protected:
  virtual void __fastcall Paint();
  virtual void __fastcall Dispatch(void * AMessage);

private:
  bool FParentPainting;
  int FMargin;
  UnicodeString FShortHint;
  UnicodeString FLongHint;
  TControl * FHintControl;
  bool FHintPopup;
  std::unique_ptr<TFont> FScaledHintFont;

  UnicodeString __fastcall GetLongHintIfAny(const UnicodeString & AHint);
  static int __fastcall GetTextFlags(TControl * Control);
  bool __fastcall IsPathLabel(TControl * HintControl);
  bool __fastcall UseBoldShortHint(TControl * HintControl);
  int __fastcall GetMargin(TControl * HintControl, const UnicodeString & Hint);
  TFont * __fastcall GetFont(TControl * HintControl, const UnicodeString & Hint);
  TControl * __fastcall GetHintControl(void * Data);
  void __fastcall SplitHint(
    TControl * HintControl, const UnicodeString & Hint, UnicodeString & ShortHint, UnicodeString & LongHint);
};
//---------------------------------------------------------------------------
// Based on:
// https://stackoverflow.com/q/6912424/850848
// https://stackoverflow.com/q/4685863/850848
class TUIStateAwareLabel : public TLabel
{
public:
  __fastcall virtual TUIStateAwareLabel(TComponent * AOwner);

protected:
  DYNAMIC void __fastcall DoDrawText(TRect & Rect, int Flags);
  virtual void __fastcall Dispatch(void * AMessage);
};
// FindComponentClass takes parameter by reference and as such it cannot be implemented in
// an inline method without a compiler warning, which we cannot suppress in a macro.
// And having the implementation in a real code (not macro) also allows us to debug the code.
void __fastcall FindComponentClass(
  void * Data, TReader * Reader, const UnicodeString ClassName, TComponentClass & ComponentClass);
#define INTERFACE_HOOK_CUSTOM(PARENT) \
  protected: \
    virtual void __fastcall ReadState(TReader * Reader) \
    { \
      Reader->OnFindComponentClass = MakeMethod<TFindComponentClassEvent>(NULL, FindComponentClass); \
      PARENT::ReadState(Reader); \
    }
#define INTERFACE_HOOK INTERFACE_HOOK_CUSTOM(TForm)
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool;
extern const UnicodeString PuttygenTool;
//---------------------------------------------------------------------------
#endif
