//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Tools.h>
#include <TextsWin.h>
#include <CoreMain.h>
#include <HelpIntfs.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TWebHelpSystem : public TInterfacedObject, public ICustomHelpViewer
{
public:
  __fastcall TWebHelpSystem(const UnicodeString & Version, const UnicodeString & Language);
  virtual int __fastcall UnderstandsKeyword(const UnicodeString HelpString);
  virtual TStringList * __fastcall GetHelpStrings(const UnicodeString HelpString);
  virtual void __fastcall NotifyID(const int ViewerID);
  virtual void __fastcall SoftShutDown();
  virtual void __fastcall ShutDown();
  virtual UnicodeString __fastcall GetViewerName();
  virtual bool __fastcall CanShowTableOfContents();
  virtual void __fastcall ShowTableOfContents();
  virtual void __fastcall ShowHelp(const UnicodeString HelpString);

  IUNKNOWN

private:
  UnicodeString FVersion;
  UnicodeString FLanguage;
};
//---------------------------------------------------------------------------
void __fastcall SearchHelp(const UnicodeString & Message)
{
  OpenBrowser(FMTLOAD(DOCUMENTATION_SEARCH_URL2,
    (EncodeUrlString(Message), Configuration->ProductVersion,
     IntToHex(__int64(GUIConfiguration->Locale), 4))));
}
//---------------------------------------------------------------------------
void __fastcall InitializeWinHelp()
{
  InitializeCustomHelp(new TWebHelpSystem(
      Configuration->ProductVersion, IntToHex(__int64(GUIConfiguration->Locale), 4)));
}
//---------------------------------------------------------------------------
void __fastcall FinalizeWinHelp()
{
  FinalizeCustomHelp();
}
//---------------------------------------------------------------------------
__fastcall TWebHelpSystem::TWebHelpSystem(
  const UnicodeString & Version, const UnicodeString & Language) :
  FVersion(Version), FLanguage(Language)
{
}
//---------------------------------------------------------------------------
int __fastcall TWebHelpSystem::UnderstandsKeyword(const UnicodeString HelpString)
{
  // pretend that we know everything
  return 1;
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWebHelpSystem::GetHelpStrings(const UnicodeString HelpString)
{
  TStringList * Result = new TStringList();
  Result->Add(GetViewerName() + L" : " + HelpString);
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
UnicodeString __fastcall TWebHelpSystem::GetViewerName()
{
  return L"Web";
}
//---------------------------------------------------------------------------
bool __fastcall TWebHelpSystem::CanShowTableOfContents()
{
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShowTableOfContents()
{
  OpenBrowser(FMTLOAD(DOCUMENTATION_URL2, (FVersion, FLanguage)));
}
//---------------------------------------------------------------------------
void __fastcall TWebHelpSystem::ShowHelp(const UnicodeString AHelpString)
{
  // see also CampaignUrl
  UnicodeString HelpString = AHelpString;
  const wchar_t FragmentSeparator = L'#';
  UnicodeString HelpPath = CutToChar(HelpString, FragmentSeparator, false);
  UnicodeString HelpUrl = FMTLOAD(DOCUMENTATION_KEYWORD_URL2, (HelpPath, FVersion, FLanguage));
  AddToList(HelpUrl, HelpString, FragmentSeparator);
  OpenBrowser(HelpUrl);
}
