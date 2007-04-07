//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <HelpIntfs.hpp>
#include <WinHelpViewer.hpp>

#include <Common.h>
#include <Tools.h>
#include <TextsWin.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define IUNKNOWN \
  virtual HRESULT __stdcall QueryInterface(const GUID& IID, void **Obj) \
  { \
    return TInterfacedObject::QueryInterface(IID, (void *)Obj); \
  } \
  \
  virtual ULONG __stdcall AddRef() \
  { \
    return TInterfacedObject::_AddRef(); \
  } \
  \
  virtual ULONG __stdcall Release() \
  { \
    return TInterfacedObject::_Release(); \
  }
//---------------------------------------------------------------------------
class TWebHelpSystem : public TInterfacedObject, public ICustomHelpViewer
{
public:
  virtual AnsiString __fastcall GetViewerName();
  virtual int __fastcall UnderstandsKeyword(const AnsiString HelpString);
  virtual TStringList * __fastcall GetHelpStrings(const AnsiString HelpString);
  virtual bool __fastcall CanShowTableOfContents();
  virtual void __fastcall ShowTableOfContents();
  virtual void __fastcall ShowHelp(const AnsiString HelpString);
  virtual void __fastcall NotifyID(const int ViewerID);
  virtual void __fastcall SoftShutDown();
  virtual void __fastcall ShutDown();

  void __fastcall InternalShutdown();

  int __fastcall ViewerID();

  IUNKNOWN

private:
  int FViewerID;
};
//---------------------------------------------------------------------------
class TWinHelpTester : public TInterfacedObject, public IWinHelpTester
{
public:
  virtual __fastcall ~TWinHelpTester();
  virtual bool __fastcall CanShowALink(const AnsiString ALink, const AnsiString FileName);
  virtual bool __fastcall CanShowTopic(const AnsiString Topic, const AnsiString FileName);
  virtual bool __fastcall CanShowContext(const int Context, const AnsiString FileName);
  virtual TStringList * __fastcall GetHelpStrings(const AnsiString ALink);
  virtual AnsiString __fastcall GetHelpPath();
  virtual AnsiString __fastcall GetDefaultHelpFile();

  IUNKNOWN
};
//---------------------------------------------------------------------------
TWebHelpSystem * WebHelpSystem;
_di_IHelpManager * PHelpManager = NULL;
IUnknown * HelpManagerUnknown = NULL;
TObjectList * ViewerList = NULL;
ICustomHelpViewer * WinHelpViewer = NULL;
//---------------------------------------------------------------------------
void __fastcall InitializeWinHelp()
{
  assert(PHelpManager == NULL);

  // workaround
  // _di_IHelpManager cannot be instantiated due to either bug in compiler or
  // VCL code
  PHelpManager = (_di_IHelpManager*)malloc(sizeof(_di_IHelpManager));
  WebHelpSystem = new TWebHelpSystem();
  // our own reference
  WebHelpSystem->AddRef();
  RegisterViewer(WebHelpSystem, *PHelpManager);
  HelpManagerUnknown = dynamic_cast<IUnknown *>(&**PHelpManager);

  // gross hack
  // Due to major bugs in VCL help system, unregister winhelp at all.
  // To do this we must call RegisterViewer first to get HelpManager.
  // Due to another bug, viewers must be unregistred in order reversed to
  // registration order, so we must unregister WebHelpSystem first and register
  // it again at the end
  WebHelpSystem->InternalShutdown();
  // 40 is offset from IHelpManager to THelpManager
  // 16 is offset from THelpManager to THelpManager::FViewerList
  ViewerList =
    *reinterpret_cast<TObjectList **>(reinterpret_cast<char *>(&**PHelpManager)
    - 40 + 16);
  assert(ViewerList->Count == 1);
  // 8 is offset from THelpViewerNode to THelpViewerNode::FViewer
  int WinHelpViewerID =
    *reinterpret_cast<int *>(reinterpret_cast<char *>(ViewerList->Items[0])
    + 8);
  // 4 is offset from THelpViewerNode to THelpViewerNode::FViewer
  WinHelpViewer =
    *reinterpret_cast<_di_ICustomHelpViewer *>(reinterpret_cast<char *>(ViewerList->Items[0])
    + 4);
  // our reference
  // we cannot release win help viewer completelly here as finalization code of
  // WinHelpViewer expect it to exist
  WinHelpViewer->AddRef();
  (**PHelpManager).Release(WinHelpViewerID);
  // remove forgoten 3 references of manager
  WinHelpViewer->Release();
  WinHelpViewer->Release();
  WinHelpViewer->Release();
  assert(ViewerList->Count == 0);
  // this clears reference to manager in TWinHelpViewer::FHelpManager,
  // preventing call to TWinHelpViewer::InternalShutdown from finalization code
  // of WinHelpViewer
  WinHelpViewer->ShutDown();

  RegisterViewer(WebHelpSystem, *PHelpManager);
  // we've got second reference to the same pointer here, release it
  HelpManagerUnknown->Release();
  assert(ViewerList->Count == 1);

  // now when winhelp is not registered, the tester is not used anyway
  // for any real work, but we use it as a hook to be called after
  // finalization of WinHelpViewer (see its finalization section)
  WinHelpTester = new TWinHelpTester();
}
//---------------------------------------------------------------------------
void __fastcall FinalizeWinHelp()
{
  if (WebHelpSystem != NULL)
  {
    // unregister ourselves to release both references
    WebHelpSystem->InternalShutdown();
    // our own reference
    WebHelpSystem->Release();
  }

  if (PHelpManager != NULL)
  {
    assert(ViewerList->Count == 0);

    // our reference
    HelpManagerUnknown->Release();
    free(PHelpManager);
    PHelpManager = NULL;
    HelpManagerUnknown = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall CleanUpWinHelp()
{
  // WinHelpViewer finalization code should have been called by now already,
  // so we can safely remove the last reference to destroy it
  assert(WinHelpViewer != NULL);
  WinHelpViewer->Release();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWebHelpSystem::GetViewerName()
{
  return "Web";
}
//---------------------------------------------------------------------------
int __fastcall TWebHelpSystem::ViewerID()
{
  return FViewerID;
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::InternalShutdown()
{
  assert(PHelpManager != NULL);
  if (PHelpManager != NULL)
  {
    // override conflict of two Release() methods by getting
    // IHelpManager explicitly using operator *
    (**PHelpManager).Release(FViewerID);
    // registration to help manager increases refcount by 2, but deregistration
    // by one only
    assert(RefCount > 0);
    Release();
  }
}
//---------------------------------------------------------------------------
int __fastcall TWebHelpSystem::UnderstandsKeyword(const AnsiString HelpString)
{
  // pretend that we know everyting
  return 1;
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWebHelpSystem::GetHelpStrings(const AnsiString HelpString)
{
  TStringList * Result = new TStringList();
  Result->Add(GetViewerName() + " : " + HelpString);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TWebHelpSystem::CanShowTableOfContents()
{
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShowTableOfContents()
{
  OpenBrowser(LoadStr(DOCUMENTATION_URL));
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShowHelp(const AnsiString HelpString)
{
  OpenBrowser(FMTLOAD(DOCUMENTATION_KEYWORD_URL, (HelpString)));
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::NotifyID(const int ViewerID)
{
  FViewerID = ViewerID;
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::SoftShutDown()
{
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShutDown()
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TWinHelpTester::~TWinHelpTester()
{
  CleanUpWinHelp();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowALink(const AnsiString ALink,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowTopic(const AnsiString Topic,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowContext(const int /*Context*/,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWinHelpTester::GetHelpStrings(const AnsiString ALink)
{
  assert(false);
  TStringList * Result = new TStringList();
  Result->Add(ViewerName + ": " + ALink);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetHelpPath()
{
  assert(false);
  // never called on windows anyway
  return ExtractFilePath(Application->HelpFile);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetDefaultHelpFile()
{
  assert(false);
  return Application->HelpFile;
}
