//---------------------------------------------------------------------------
#ifndef GUIToolsH
#define GUIToolsH
//---------------------------------------------------------------------------
#include <FileMasks.H>
#include <Tbx.hpp>
//---------------------------------------------------------------------------
class TSessionData;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessMessagesEvent)();
//---------------------------------------------------------------------------
bool __fastcall FindFile(UnicodeString & Path);
bool __fastcall FindTool(const UnicodeString & Name, UnicodeString & Path);
void __fastcall ExecuteShellChecked(const UnicodeString Path, const UnicodeString Params,
  bool ChangeWorkingDirectory = false);
void __fastcall ExecuteShellChecked(const UnicodeString Command);
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params,
  HANDLE & Handle);
void __fastcall ExecuteShellCheckedAndWait(const UnicodeString Command, TProcessMessagesEvent ProcessMessages);
bool __fastcall CopyCommandToClipboard(const UnicodeString & Command);
void __fastcall OpenSessionInPutty(const UnicodeString PuttyPath,
  TSessionData * SessionData);
bool __fastcall SpecialFolderLocation(int PathID, UnicodeString & Path);
UnicodeString __fastcall UniqTempDir(const UnicodeString BaseDir,
  const UnicodeString Identity, bool Mask = false);
bool __fastcall DeleteDirectory(const UnicodeString DirName);
void __fastcall AddSessionColorImage(TCustomImageList * ImageList, TColor Color, int MaskIndex);
void __fastcall SetSubmenu(TTBXCustomItem * Item);
typedef int __fastcall (*TCalculateWidth)(UnicodeString Text, void * Arg);
void __fastcall ApplyTabs(
  UnicodeString & Text, wchar_t Padding,
  TCalculateWidth CalculateWidth, void * CalculateWidthArg);
TPanel * __fastcall CreateLabelPanel(TPanel * Parent, const UnicodeString & Label);
void __fastcall SelectScaledImageList(TImageList * ImageList);
void __fastcall CopyDataModule(TDataModule * TargetModule, TDataModule * SourceModule);
void __fastcall CopyImageList(TImageList * TargetList, TImageList * SourceList);
void __fastcall LoadDialogImage(TImage * Image, const UnicodeString & ImageName);
int __fastcall DialogImageSize();
void __fastcall HideComponentsPanel(TForm * Form);
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
TComponent * __fastcall FindComponentRecursively(TComponent * Root, const UnicodeString & Name);
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
class TFrameAnimation
{
public:
  __fastcall TFrameAnimation();
  void __fastcall Init(TPaintBox * PaintBox, const UnicodeString & Name);
  void __fastcall Start();
  void __fastcall Stop();

private:
  TPaintBox * FPaintBox;
  TPngImageList * FImageList;
  int FFirstFrame;
  int FFirstLoopFrame;
  int FLastFrame;
  int FCurrentFrame;
  DWORD FNextFrameTick;
  TTimer * FTimer;
  bool FPainted;

  void __fastcall DoInit(TPaintBox * PaintBox, TPngImageList * ImageList, const UnicodeString & Name, bool Null);
  void __fastcall PaintBoxPaint(TObject * Sender);
  void __fastcall CalculateNextFrameTick();
  TPngImageCollectionItem * __fastcall GetCurrentImage();
  void __fastcall Animate();
  void __fastcall Timer(TObject * Sender);
  void __fastcall Repaint();
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

  UnicodeString __fastcall GetLongHintIfAny(const UnicodeString & AHint);
  static int __fastcall GetTextFlags(TControl * Control);
  bool __fastcall IsHintPopup(TControl * HintControl, const UnicodeString & Hint);
  bool __fastcall IsPathLabel(TControl * HintControl);
  bool __fastcall UseBoldShortHint(TControl * HintControl);
  int __fastcall GetMargin(TControl * HintControl, const UnicodeString & Hint);
  TFont * __fastcall GetFont(TControl * HintControl, const UnicodeString & Hint);
  TControl * __fastcall GetHintControl(void * Data);
};
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool;
extern const UnicodeString PuttygenTool;
//---------------------------------------------------------------------------
#endif
