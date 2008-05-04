//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Tools.h>
#include <TextsWin.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TWebHelpSystem : public TInterfacedObject, public ICustomHelpViewer
{
public:
  virtual int __fastcall UnderstandsKeyword(const AnsiString HelpString);
  virtual TStringList * __fastcall GetHelpStrings(const AnsiString HelpString);
  virtual void __fastcall NotifyID(const int ViewerID);
  virtual void __fastcall SoftShutDown();
  virtual void __fastcall ShutDown();
  virtual AnsiString __fastcall GetViewerName();
  virtual bool __fastcall CanShowTableOfContents();
  virtual void __fastcall ShowTableOfContents();
  virtual void __fastcall ShowHelp(const AnsiString HelpString);

  IUNKNOWN
};
//---------------------------------------------------------------------------
void __fastcall InitializeWinHelp()
{
  InitializeCustomHelp(new TWebHelpSystem);
}
//---------------------------------------------------------------------------
void __fastcall FinalizeWinHelp()
{
  FinalizeCustomHelp();
}
//---------------------------------------------------------------------------
int __fastcall TWebHelpSystem::UnderstandsKeyword(const AnsiString HelpString)
{
  // pretend that we know everything
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
void __fastcall TWebHelpSystem::NotifyID(const int /*ViewerID*/)
{
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
AnsiString __fastcall TWebHelpSystem::GetViewerName()
{
  return "Web";
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
