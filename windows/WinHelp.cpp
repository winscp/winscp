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
  bool __fastcall CanShowALink(const AnsiString ALink, const AnsiString FileName);
  bool __fastcall CanShowTopic(const AnsiString Topic, const AnsiString FileName);
  bool __fastcall CanShowContext(const int Context, const AnsiString FileName);
  TStringList * __fastcall GetHelpStrings(const AnsiString ALink);
  AnsiString __fastcall GetHelpPath();
  AnsiString __fastcall GetDefaultHelpFile();

  IUNKNOWN
};
//---------------------------------------------------------------------------
TWebHelpSystem * WebHelpSystem;
_di_IHelpManager * HelpManager = NULL;
//---------------------------------------------------------------------------
void __fastcall InitializeWebHelp()
{
  assert(HelpManager == NULL);

  // workaround
  // _di_IHelpManager cannot be instantiated due to either bug in compiler or
  // VCL code
  HelpManager = (_di_IHelpManager*)malloc(sizeof(_di_IHelpManager));
  WebHelpSystem = new TWebHelpSystem();
  RegisterViewer(WebHelpSystem, *HelpManager);

  // gross hack
  // Due to major bugs in VCL help system, unregister winhelp at all.
  // To do this we must call RegisterViewer first to get HelpManager. 
  // Due to another bug, viewers must be unregistred in order reversed to
  // registration order, so we must unregister WebHelpSystem first and register
  // it again at the end
  WebHelpSystem->InternalShutdown();
  (**HelpManager).Release(WebHelpSystem->ViewerID() - 1);
  RegisterViewer(WebHelpSystem, *HelpManager);

  // now when winhelp is not registered, the tested is not user anyway
  WinHelpTester = new TWinHelpTester();
}
//---------------------------------------------------------------------------
void __fastcall FinalizeWebHelp()
{
  if (WebHelpSystem != NULL)
  {
    // another workaround.
    // HelpMananger has bug in destructor, as workaround we must unregister
    // our viewer sooner 
    WebHelpSystem->InternalShutdown();
  }
  
  if (HelpManager != NULL)
  {
    free(HelpManager);
    HelpManager = NULL;
  }

  if (WinHelpTester != NULL)
  {
    delete WinHelpTester;
    WinHelpTester = NULL;
  }
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
  assert(HelpManager != NULL);
  if (HelpManager != NULL)
  {
    // override conflict of two Release() methods by getting 
    // IHelpManager explicitly using operator *
    (**HelpManager).Release(FViewerID);
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
bool __fastcall TWinHelpTester::CanShowALink(const AnsiString ALink, 
  const AnsiString FileName)
{
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowTopic(const AnsiString Topic, 
  const AnsiString FileName)
{
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowContext(const int /*Context*/, 
  const AnsiString FileName)
{
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWinHelpTester::GetHelpStrings(const AnsiString ALink)
{
  TStringList * Result = new TStringList();
  Result->Add(ViewerName + ": " + ALink);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetHelpPath()
{
  // never called on windows anyway
  return ExtractFilePath(Application->HelpFile);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetDefaultHelpFile()
{
  return Application->HelpFile;
}

