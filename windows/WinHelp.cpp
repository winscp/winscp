//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Tools.h>
#include <TextsWin.h>
#include <CoreMain.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TWebHelpSystem : public TInterfacedObject, public ICustomHelpViewer
{
public:
  __fastcall TWebHelpSystem(const AnsiString & Version);
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

private:
  AnsiString FVersion;
};
//---------------------------------------------------------------------------
void __fastcall SearchHelp(const AnsiString & Message)
{
  OpenBrowser(FMTLOAD(DOCUMENTATION_SEARCH_URL,
    (EncodeUrlString(UTF8Encode(Message)), Configuration->ProductVersion)));
}
//---------------------------------------------------------------------------
void __fastcall InitializeWinHelp()
{
  InitializeCustomHelp(new TWebHelpSystem(Configuration->ProductVersion));
}
//---------------------------------------------------------------------------
void __fastcall FinalizeWinHelp()
{
  FinalizeCustomHelp();
}
//---------------------------------------------------------------------------
__fastcall TWebHelpSystem::TWebHelpSystem(const AnsiString & Version) :
  FVersion(Version)
{
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
  OpenBrowser(FMTLOAD(DOCUMENTATION_URL, (FVersion)));
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShowHelp(const AnsiString HelpString)
{
  OpenBrowser(FMTLOAD(DOCUMENTATION_KEYWORD_URL, (HelpString, FVersion)));
}
