//---------------------------------------------------------------------------
#include <WinPCH.h>
#pragma hdrstop

#include <HelpIntfs.hpp>
//---------------------------------------------------------------------------
class TWebHelpSystem : public TCppInterfacedObject<ICustomHelpViewer>
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

private:
  UnicodeString FVersion;
  UnicodeString FLanguage;
};
//---------------------------------------------------------------------------
void __fastcall SearchHelp(const UnicodeString & Message)
{
  // Message goes last, as it may exceed URL parameters limit (2048) and get truncated.
  // And we need to preserve the other parameters.
  OpenBrowser(FMTLOAD(DOCUMENTATION_SEARCH_URL3,
    (Configuration->ProductVersion, GUIConfiguration->AppliedLocaleHex,
     EncodeUrlString(Message))));
}
//---------------------------------------------------------------------------
void __fastcall InitializeWinHelp()
{
  InitializeCustomHelp(new TWebHelpSystem(
      Configuration->ProductVersion, GUIConfiguration->AppliedLocaleHex));
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
int __fastcall TWebHelpSystem::UnderstandsKeyword(const UnicodeString DebugUsedArg(HelpString))
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
  if (IsHttpOrHttpsUrl(AHelpString))
  {
    OpenBrowser(AHelpString);
  }
  else
  {
    ::ShowHelp(AHelpString);
  }
}
