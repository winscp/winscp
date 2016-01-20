//---------------------------------------------------------------------------
// Part of this code is
// Copyright (C) 2002-2004, Marco Barisione <marco.bari@vene.ws>
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>
#include <tchar.h>
#include <shlobj.h>
#include <tlhelp32.h>
#include <Common.h>
#include <CoreMain.h>
#include <Exceptions.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>
#include <Http.h>
#include <CompThread.hpp>
#include <FileInfo.h>
#include "WinConfiguration.h"
#include "WinInterface.h"
#include "Tools.h"
#include "Setup.h"
#include <StrUtils.hpp>
#include "ProgParams.h"
#include <Consts.hpp>
#include <GUITools.h>
#include <PuttyTools.h>
#include <VCLCommon.h>
#include <WebBrowserEx.hpp>
#include <DateUtils.hpp>
//---------------------------------------------------------------------------
#define KEY _T("SYSTEM\\CurrentControlSet\\Control\\") \
            _T("Session Manager\\Environment")
// when the PATH registry key is over aprox 2048 characters,
// PATH as well as WINDIR variables are actually not set, breaking the system
#define MAX_PATH_LEN 2000

/* Command line options. */
UnicodeString LastPathError;
//---------------------------------------------------------------------------
// Display the error "err_msg".
void err_out(LPCTSTR err_msg)
{
  LastPathError = err_msg;
}
//---------------------------------------------------------------------------
// Display "base_err_msg" followed by the description of the system error
// identified by "sys_err".
void err_out_sys(LPCTSTR base_err_msg, LONG sys_err)
{
  LastPathError = FORMAT(L"%s %s", (base_err_msg, SysErrorMessage(sys_err)));
}
//---------------------------------------------------------------------------
// Works as "strcmp" but the comparison is not case sensitive.
int tcharicmp(LPCTSTR str1, LPCTSTR str2){
    for (; tolower(*str1) == tolower(*str2); ++str1, ++str2)
        if (*str1 == L'\0')
            return 0;
    return tolower(*str1) - tolower(*str2);
}
//---------------------------------------------------------------------------
// Returns un unquoted copy of "str" (or a copy of "str" if the quotes are
// not present). The returned value must be freed with "free".
LPTSTR unquote(LPCTSTR str){
    int last_pos;
    LPTSTR ret;
    size_t new_len;

    last_pos = _tcslen(str) - 1;
    if (last_pos != -1 && str[0] == L'"' && str[last_pos] == L'"'){
        new_len= (_tcslen(str) - 1);
        ret = (LPTSTR)malloc(new_len * sizeof(TCHAR));
        lstrcpyn(ret, &str[1], new_len);
    }
    else
        ret = _tcsdup(str);
    return ret;
}
//---------------------------------------------------------------------------
// Find "what" in the ";" separated string "str" and returns a pointer to
// the first letter of "what" in the string. If "next" is not "NULL" it
// points to the first letter after "what" (excluding the trailing ";").
// If "what" isn't find the functions returns "NULL".
LPTSTR find_reg_str(LPTSTR str, LPCTSTR what, LPTSTR * next){
    LPTSTR tok_buff;
    LPTSTR curr_tok;
    LPTSTR curr_tok_dup;
    BOOL path_eq;
    TCHAR sh_path1[MAX_PATH], sh_path2[MAX_PATH];
    int pos = -1;
    LPTSTR ret;

    tok_buff = _tcsdup(str);
    curr_tok = _tcstok(tok_buff, _T(";"));
    while (pos == -1 && curr_tok){
        curr_tok_dup = unquote(curr_tok);
        path_eq = GetShortPathName(what, sh_path1, LENOF(sh_path1)) &&
                  GetShortPathName(curr_tok_dup, sh_path2,
                                   LENOF(sh_path2)) &&
                  (tcharicmp(sh_path1, sh_path2) == 0);
        if (path_eq || tcharicmp(what, curr_tok_dup) == 0){
            pos = curr_tok - tok_buff;
        }
        free(curr_tok_dup);
        curr_tok = _tcstok(NULL, _T(";"));
        if (pos != -1 && next){
            if (curr_tok)
                *next = str + (curr_tok - tok_buff);
            else
                *next = str + _tcslen(str);
        }
    }
    free(tok_buff);
    if (pos != -1)
        ret = str + pos;
    else
        ret = NULL;
    return ret;
}
//---------------------------------------------------------------------------
void path_reg_propagate()
{
  DWORD send_message_result;
  LONG ret = SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
                           (LPARAM)_T("Environment"), SMTO_ABORTIFHUNG,
                           5000, &send_message_result);
  if (ret != ERROR_SUCCESS && GetLastError() != 0)
  {
    err_out_sys(_T("Cannot propagate the new enviroment to ")
                _T("other processes. The new value will be ")
                _T("avaible after a reboot."), GetLastError());
    SimpleErrorDialog(LastPathError);
    LastPathError = L"";
  }
}
//---------------------------------------------------------------------------
// Add "path" to the registry. Return "TRUE" if the path has been added or
// was already in the registry, "FALSE" otherwise.
BOOL add_path_reg(LPCTSTR path){
    HKEY key;
    LONG ret;
    DWORD data_size;
    LPTSTR reg_str;
    BOOL func_ret = TRUE;

    ret = RegOpenKeyEx(HKEY_LOCAL_MACHINE, KEY, 0,
                       KEY_WRITE | KEY_READ, &key);
    if (ret != ERROR_SUCCESS){
        err_out_sys(_T("Cannot open registry."), ret);
        return FALSE;
    }

    RegQueryValueEx(key, _T("PATH"), NULL, NULL, NULL, &data_size);
    data_size += _tcslen(path) + 3 ; /* ";" and quotes, "data_size" already
                                        includes '\0'. */
    reg_str = (LPTSTR)malloc(data_size * sizeof(TCHAR));
    ret = RegQueryValueEx(key, _T("PATH"), NULL, NULL, (LPBYTE)reg_str,
                          &data_size);
    if (ret != ERROR_SUCCESS){
        err_out_sys(_T("Cannot read \"PATH\" key."), ret);
        func_ret = FALSE;
    }
    else{
        if (!find_reg_str(reg_str, path, NULL)){
            _tcscat(reg_str, _T(";"));
            _tcscat(reg_str, path);
            size_t len = _tcslen(reg_str);
            if (len >= MAX_PATH_LEN)
            {
              err_out(LoadStr(PATH_ENV_TOO_LONG).c_str());
              func_ret = FALSE;
            }
            else
            {
              ret = RegSetValueEx(key, _T("PATH"), 0, REG_EXPAND_SZ,
                                  (LPBYTE)reg_str,
                                  (_tcslen(reg_str) + 1) * sizeof(TCHAR));
              if (ret != ERROR_SUCCESS){
                  err_out_sys(_T("Cannot write \"PATH\" key."), ret);
                  func_ret = FALSE;
              }
              /* Is this needed to make the new key avaible? */
              RegFlushKey(key);
              SetLastError(0);
              path_reg_propagate();
            }
        }
    }

    RegCloseKey(key);
    free(reg_str);
    return func_ret;
}
//---------------------------------------------------------------------------
// Removes "path" from the registry. Return "TRUE" if the path has been
// removed or it wasn't in the registry, "FALSE" otherwise.
BOOL remove_path_reg(LPCTSTR path){
    HKEY key;
    LONG ret;
    DWORD data_size;
    LPTSTR reg_str;
    LPTSTR reg_str2;
    BOOL func_ret = TRUE;
    LPTSTR next;
    LPTSTR del_part;
    int last_pos;

    ret = RegOpenKeyEx(HKEY_LOCAL_MACHINE, KEY, 0,
                       KEY_WRITE | KEY_READ, &key);
    if (ret != ERROR_SUCCESS){
        err_out_sys(_T("Cannot open registry."), ret);
        return FALSE;
    }

    RegQueryValueEx(key, _T("PATH"), NULL, NULL, NULL, &data_size);
    data_size += _tcslen(path) + 3; /* ";" and quotes,"data_size" already
                                        includes '\0'. */
    reg_str = (LPTSTR)malloc(data_size * sizeof(TCHAR));
    ret = RegQueryValueEx(key, _T("PATH"), NULL, NULL,
                          (LPBYTE)reg_str, &data_size);
    if (ret != ERROR_SUCCESS){
        err_out_sys(_T("Cannot read \"PATH\" key."), ret);
        func_ret = FALSE;
    }
    else{
        if ((del_part = find_reg_str(reg_str, path, &next)) != NULL){
            reg_str2 = (LPTSTR)malloc((_tcslen(reg_str) + 1) * sizeof(TCHAR));
            *del_part = '\0';
            _stprintf(reg_str2, _T("%s%s"), reg_str, next);
            last_pos = _tcslen(reg_str2) - 1;
            if (last_pos != -1 && reg_str2[last_pos] == ';')
                reg_str2[last_pos] = '\0';
            ret = RegSetValueEx(key, _T("PATH"), 0, REG_EXPAND_SZ,
                                (LPBYTE)reg_str2,
                                (_tcslen(reg_str2) + 1) * sizeof(TCHAR));
            if (ret != ERROR_SUCCESS){
                err_out_sys(_T("Cannot write \"PATH\" key."), ret);
                func_ret = FALSE;
            }
            free(reg_str2);
            /* Is this needed to make the new key avaible? */
            RegFlushKey(key);
            SetLastError(0);
            path_reg_propagate();
        }
    }

    RegCloseKey(key);
    free(reg_str);
    return func_ret;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const UnicodeString Path)
{
  if (!add_path_reg(Path.c_str()))
  {
    throw ExtException(FMTLOAD(ADD_PATH_ERROR, (Path)), LastPathError);
  }
}
//---------------------------------------------------------------------------
void __fastcall RemoveSearchPath(const UnicodeString Path)
{
  if (!remove_path_reg(Path.c_str()))
  {
    throw ExtException(FMTLOAD(REMOVE_PATH_ERROR, (Path)), LastPathError);
  }
}
//---------------------------------------------------------------------------
static const UnicodeString SoftwareClassesBaseKey = L"Software\\Classes\\";
//---------------------------------------------------------------------------
static void __fastcall DeleteKeyIfEmpty(TRegistry * Registry, const UnicodeString & Key, bool AllowRootValues)
{
  if (Registry->OpenKey(Key, false))
  {
    std::unique_ptr<TStrings> List(new TStringList());

    Registry->GetValueNames(List.get());
    bool CanDelete = true;
    for (int Index = 0; CanDelete && (Index < List->Count); Index++)
    {
      UnicodeString ValueName = List->Strings[Index];
      if (!AllowRootValues)
      {
        CanDelete = false;
      }
      if ((ValueName != L"") &&
          (ValueName != L"URL Protocol") &&
          (ValueName != L"EditFlags") &&
          (ValueName != L"BrowserFlags"))
      {
        CanDelete = false;
      }
    }

    List->Clear();
    Registry->GetKeyNames(List.get());

    Registry->CloseKey();

    if (CanDelete)
    {
      for (int Index = 0; Index < List->Count; Index++)
      {
        DeleteKeyIfEmpty(Registry, IncludeTrailingBackslash(Key) + List->Strings[Index], false);
      }

      // will fail, if not all subkeys got removed
      Registry->DeleteKey(Key);
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall RegisterProtocol(TRegistry * Registry,
  const UnicodeString & Protocol, UnicodeString Description, bool Force)
{

  if (Description.IsEmpty())
  {
    Description = FMTLOAD(PROTOCOL_URL_DESC, (Protocol));
  }

  UnicodeString ProtocolKey = SoftwareClassesBaseKey + Protocol;
  if (Force || !Registry->KeyExists(ProtocolKey))
  {
    if (Registry->OpenKey(SoftwareClassesBaseKey + Protocol, true))
    {
      Registry->WriteString(L"", Description);
      Registry->WriteString(L"URL Protocol", L"");
      Registry->WriteInteger(L"EditFlags", 0x02);
      Registry->WriteInteger(L"BrowserFlags", 0x08);
      if (Registry->OpenKey(L"DefaultIcon", true))
      {
        Registry->WriteString(L"", FORMAT(L"\"%s\",0", (Application->ExeName)));
        Registry->CloseKey();
      }
      else
      {
        Abort();
      }
    }
    else
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterProtocol(TRegistry * Registry,
  const UnicodeString & Protocol)
{
  DeleteKeyIfEmpty(Registry, SoftwareClassesBaseKey + Protocol, true);
}
//---------------------------------------------------------------------------
static TRegistry * __fastcall CreateRegistry(HKEY RootKey)
{
  std::unique_ptr<TRegistry> Registry(new TRegistry());

  Registry->Access = KEY_WRITE | KEY_READ;
  Registry->RootKey = RootKey;

  return Registry.release();
}
//---------------------------------------------------------------------------
static void __fastcall RegisterAsUrlHandler(HKEY RootKey,
  const UnicodeString & Protocol, UnicodeString Description = L"")
{
  std::unique_ptr<TRegistry> Registry(CreateRegistry(RootKey));

  RegisterProtocol(Registry.get(), Protocol, Description, true);

  if (Registry->OpenKey(SoftwareClassesBaseKey + Protocol, false) &&
      Registry->OpenKey(L"shell", true) &&
      Registry->OpenKey(L"open", true) &&
      Registry->OpenKey(L"command", true))
  {
    Registry->WriteString(L"", FORMAT(L"\"%s\" %s \"%%1\"", (Application->ExeName, TProgramParams::FormatSwitch(UNSAFE_SWITCH))));
    Registry->CloseKey();
  }
  else
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
static void __fastcall RegisterAsUrlHandler(const UnicodeString & Protocol, UnicodeString Description = L"")
{

  try
  {
    RegisterAsUrlHandler(HKEY_LOCAL_MACHINE, Protocol, Description);

    // get rid of any HKCU registraction that would overrite the HKLM one
    std::unique_ptr<TRegistry> Registry(CreateRegistry(HKEY_CURRENT_USER));
    if (Registry->KeyExists(SoftwareClassesBaseKey + Protocol))
    {
      Registry->DeleteKey(SoftwareClassesBaseKey + Protocol);
    }
  }
  catch (Exception & E)
  {
    try
    {
      RegisterAsUrlHandler(HKEY_CURRENT_USER, Protocol, Description);
    }
    catch(Exception & E)
    {
      throw ExtException(&E, LoadStr(REGISTER_URL_ERROR2));
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterAsUrlHandler(HKEY RootKey,
  const UnicodeString & Protocol, bool UnregisterProtocol, bool ForceHandlerUnregistration)
{
  std::unique_ptr<TRegistry> Registry(CreateRegistry(RootKey));

  UnicodeString DefaultIconKey = SoftwareClassesBaseKey + Protocol + L"\\DefaultIcon";
  if (Registry->OpenKey(DefaultIconKey, false))
  {
    UnicodeString Value = Registry->ReadString(L"");
    UnicodeString ExeBaseName = ExtractFileBaseName(Application->ExeName);
    if (ForceHandlerUnregistration || ContainsText(Value, ExeBaseName))
    {
      Registry->DeleteValue(L"");
    }
    Registry->CloseKey();

    DeleteKeyIfEmpty(Registry.get(), DefaultIconKey, false);
  }

  UnicodeString ShellKey = SoftwareClassesBaseKey + Protocol + L"\\shell";
  if (Registry->OpenKey(ShellKey + L"\\open\\command", false))
  {
    UnicodeString Value = Registry->ReadString(L"");
    UnicodeString ExeBaseName = ExtractFileBaseName(Application->ExeName);
    if (ForceHandlerUnregistration || ContainsText(Value, ExeBaseName))
    {
      Registry->DeleteValue(L"");
    }

    Registry->CloseKey();

    DeleteKeyIfEmpty(Registry.get(), ShellKey, false);
  }

  if (UnregisterProtocol)
  {
    ::UnregisterProtocol(Registry.get(), Protocol);
  }
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterAsUrlHandler(const UnicodeString & Protocol, bool UnregisterProtocol)
{
  UnregisterAsUrlHandler(HKEY_LOCAL_MACHINE, Protocol, UnregisterProtocol, false);
  UnregisterAsUrlHandler(HKEY_CURRENT_USER, Protocol, UnregisterProtocol, false);
}
//---------------------------------------------------------------------------
static void __fastcall RegisterAsNonBrowserUrlHandler(const UnicodeString & Prefix)
{
  RegisterAsUrlHandler(Prefix + SftpProtocol.UpperCase());
  RegisterAsUrlHandler(Prefix + ScpProtocol.UpperCase());
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterAsUrlHandlers(const UnicodeString & Prefix, bool UnregisterProtocol)
{
  UnregisterAsUrlHandler(Prefix + SftpProtocol, UnregisterProtocol);
  UnregisterAsUrlHandler(Prefix + ScpProtocol, UnregisterProtocol);
}
//---------------------------------------------------------------------------
static const UnicodeString GenericUrlHandler(L"WinSCP.Url");
//---------------------------------------------------------------------------
static void __fastcall RegisterProtocolForDefaultPrograms(HKEY RootKey, const UnicodeString & Protocol)
{
  // Register protocol, if it does not exist yet.
  // Prior to Windows 8, we need to register ourselves as legacy handler to
  // become the default handler. On Windows 8, it's automatic as long as no other
  // application is registered for the protocol (i.e. RegisterProtocol would be enough)
  RegisterAsUrlHandler(RootKey, Protocol);

  // see http://msdn.microsoft.com/en-us/library/windows/desktop/cc144154.aspx#registration
  std::unique_ptr<TRegistry> Registry(CreateRegistry(RootKey));

  // create capabilities record

  // this has to be a separate branch from WinSCP one, as by its presence we
  // enforce registry storage usage, and the capabilities branch may exist
  // even if we are using INI file
  UnicodeString CapabilitiesKey = IncludeTrailingBackslash(GetCompanyRegistryKey()) + L"WinSCPCapabilities";
  if (!Registry->OpenKey(CapabilitiesKey, true))
  {
    Abort();
  }

  UnicodeString Description = LoadStr(REGISTERED_APP_DESC2);
  Registry->WriteString(L"ApplicationDescription", Description);

  if (!Registry->OpenKey(L"UrlAssociations", true))
  {
    Abort();
  }

  Registry->WriteString(Protocol, GenericUrlHandler);
  Registry->CloseKey();

  // register application

  if (!Registry->OpenKey(L"Software\\RegisteredApplications", true))
  {
    Abort();
  }

  Registry->WriteString(AppNameString(), CapabilitiesKey);
  Registry->CloseKey();
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterProtocolForDefaultPrograms(HKEY RootKey,
  const UnicodeString & Protocol, bool ForceHandlerUnregistration)
{
  std::unique_ptr<TRegistry> Registry(CreateRegistry(RootKey));

  // unregister the protocol
  UnregisterAsUrlHandler(RootKey, Protocol, false, ForceHandlerUnregistration);

  // remove capabilities record

  UnicodeString CapabilitiesKey = IncludeTrailingBackslash(GetCompanyRegistryKey()) + L"WinSCPCapabilities";
  UnicodeString UrlAssociationsKey = CapabilitiesKey + L"\\UrlAssociations";
  if (Registry->OpenKey(UrlAssociationsKey, false))
  {
    Registry->DeleteValue(Protocol);
    Registry->CloseKey();

    DeleteKeyIfEmpty(Registry.get(), UrlAssociationsKey, false);
  }

  if (Registry->OpenKey(CapabilitiesKey, false))
  {
    if (!Registry->HasSubKeys())
    {
      Registry->DeleteValue(L"ApplicationDescription");
    }

    Registry->CloseKey();

    DeleteKeyIfEmpty(Registry.get(), CapabilitiesKey, false);
  }

  if (!Registry->KeyExists(CapabilitiesKey))
  {
    // unregister application

    if (Registry->OpenKey(L"Software\\RegisteredApplications", false))
    {
      Registry->DeleteValue(AppNameString());
      Registry->CloseKey();
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall RegisterProtocolsForDefaultPrograms(HKEY RootKey)
{
  // register URL handler, if it does not exist yet
  RegisterAsUrlHandler(RootKey, GenericUrlHandler, L"WinSCP URL");

  RegisterProtocolForDefaultPrograms(RootKey, FtpProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, FtpsProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, FtpesProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, SftpProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, ScpProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, SshProtocol);
  // deliberately not including WebDAV/http,
  // it's unlikely that anyone would like to change http handler
  // to non-browser application
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterProtocolsForDefaultPrograms(HKEY RootKey, bool ForceHandlerUnregistration)
{
  UnregisterProtocolForDefaultPrograms(RootKey, FtpProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, FtpsProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, FtpesProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, SftpProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, ScpProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, SshProtocol, ForceHandlerUnregistration);

  // we should not really need the "force" flag here, but why not
  UnregisterAsUrlHandler(RootKey, GenericUrlHandler, true, true);
}
//---------------------------------------------------------------------------
static void __fastcall RegisterForDefaultPrograms()
{
  try
  {
    RegisterProtocolsForDefaultPrograms(HKEY_LOCAL_MACHINE);
    // make sure we unregister any legacy protocol handler for CU,
    // this is needed for Windows Vista+7
    UnregisterProtocolsForDefaultPrograms(HKEY_CURRENT_USER, true);
  }
  catch (Exception & E)
  {
    try
    {
      RegisterProtocolsForDefaultPrograms(HKEY_CURRENT_USER);
    }
    catch (Exception & E)
    {
      throw ExtException(&E, LoadStr(REGISTER_URL_ERROR2));
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall NotifyChangedAssociations()
{
  SHChangeNotify(SHCNE_ASSOCCHANGED, 0, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall RegisterForDefaultProtocols()
{
  if (IsWinVista())
  {
    RegisterForDefaultPrograms();
  }
  else
  {
    RegisterAsNonBrowserUrlHandler(UnicodeString());
  }

  RegisterAsNonBrowserUrlHandler(WinSCPProtocolPrefix);
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpsProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpesProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + WebDAVProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + WebDAVSProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + SshProtocol.UpperCase());

  NotifyChangedAssociations();
}
//---------------------------------------------------------------------------
void __fastcall UnregisterForProtocols()
{
  UnregisterAsUrlHandlers(UnicodeString(), false);
  UnregisterAsUrlHandlers(WinSCPProtocolPrefix, true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpsProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpesProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + WebDAVProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + WebDAVSProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + SshProtocol.UpperCase(), true);

  UnregisterProtocolsForDefaultPrograms(HKEY_CURRENT_USER, false);
  UnregisterProtocolsForDefaultPrograms(HKEY_LOCAL_MACHINE, false);

  NotifyChangedAssociations();
}
//---------------------------------------------------------------------------
void __fastcall LaunchAdvancedAssociationUI()
{
  DebugAssert(IsWinVista());

  RegisterForDefaultPrograms();
  NotifyChangedAssociations();
  // sleep recommended by http://msdn.microsoft.com/en-us/library/windows/desktop/cc144154.aspx#browser
  Sleep(1000);

  if (IsWin10())
  {
    ExecuteShell(L"control.exe", L"/name Microsoft.DefaultPrograms");
  }
  else
  {
    IApplicationAssociationRegistrationUI * AppAssocRegUI;

    HRESULT Result =
      CoCreateInstance(CLSID_ApplicationAssociationRegistrationUI,
        NULL, CLSCTX_INPROC, __uuidof(IApplicationAssociationRegistrationUI), (void**)&AppAssocRegUI);
    if (SUCCEEDED(Result))
    {
      AppAssocRegUI->LaunchAdvancedAssociationUI(AppNameString().c_str());
      AppAssocRegUI->Release();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TemporaryDirectoryCleanup()
{
  bool Continue = true;
  TStrings * Folders = NULL;
  try
  {
    if (WinConfiguration->ConfirmTemporaryDirectoryCleanup)
    {
      Folders = WinConfiguration->FindTemporaryFolders();
      Continue = (Folders != NULL);

      if (Continue)
      {
        Configuration->Usage->Inc(L"TemporaryDirectoryCleanupConfirmations");

        TQueryButtonAlias Aliases[1];
        Aliases[0].Button = qaRetry;
        Aliases[0].Alias = LoadStr(OPEN_BUTTON);
        TMessageParams Params(mpNeverAskAgainCheck);
        Params.Aliases = Aliases;
        Params.AliasesCount = LENOF(Aliases);

        unsigned int Answer = MoreMessageDialog(
          FMTLOAD(CLEANTEMP_CONFIRM2, (Folders->Count)), Folders,
          qtConfirmation, qaYes | qaNo | qaRetry, HELP_CLEAN_TEMP_CONFIRM, &Params);

        if (Answer == qaNeverAskAgain)
        {
          WinConfiguration->ConfirmTemporaryDirectoryCleanup = false;
          Answer = qaYes;
        }
        else if (Answer == qaRetry)
        {
          for (int Index = 0; Index < Folders->Count; Index++)
          {
            ShellExecute(Application->Handle, NULL,
              Folders->Strings[Index].c_str(), NULL, NULL, SW_SHOWNORMAL);
          }
        }
        Continue = (Answer == qaYes);
      }
    }

    if (Continue)
    {
      try
      {
        WinConfiguration->CleanupTemporaryFolders(Folders);
      }
      catch (Exception &E)
      {
        ShowExtendedException(&E);
      }
    }
  }
  __finally
  {
    delete Folders;
  }
}
//-------------------------------------------- -------------------------------
UnicodeString __fastcall VersionStrFromCompoundVersion(int Version)
{
  int MajorVer = Version / (10000*100*100);
  int MinorVer = (Version % (10000*100*100)) / (10000*100);
  int Release = (Version % (10000*100)) / (10000);
  UnicodeString Result;
  if (Release > 0)
  {
    Result = FORMAT(L"%d.%d.%d", (MajorVer, MinorVer, Release));
  }
  else
  {
    Result = FORMAT(L"%d.%d", (MajorVer, MinorVer));
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall CampaignUrl(UnicodeString URL)
{
  int CurrentCompoundVer = Configuration->CompoundVersion;
  UnicodeString Version = VersionStrFromCompoundVersion(CurrentCompoundVer);
  // Beware that these parameters may get truncated if URL is too long,
  // such as with ERROR_REPORT_URL2
  UnicodeString Params = FORMAT(L"utm_source=winscp&utm_medium=app&utm_campaign=%s", (Version));

  return AppendUrlParams(URL, Params);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetUsageData()
{
  return Configuration->Usage->Serialize();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ProgramUrl(UnicodeString URL)
{
  TVSFixedFileInfo * FileInfo = Configuration->FixedApplicationInfo;
  UnicodeString CurrentVersionStr =
    FORMAT(L"%d.%d.%d.%d",
      (HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
       HIWORD(FileInfo->dwFileVersionLS), LOWORD(FileInfo->dwFileVersionLS)));
  UnicodeString Params =
    FORMAT(L"v=%s&lang=%s&isinstalled=%d",
      (CurrentVersionStr,
      GUIConfiguration->LocaleHex,
      int(IsInstalled())));

  if (Configuration->IsUnofficial)
  {
    Params += L"&unofficial=1";
  }

  return AppendUrlParams(URL, Params);
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall WantBetaUrl(UnicodeString URL, bool Force)
{
  bool Beta;
  if (WinConfiguration->IsBeta)
  {
    Beta = true;
  }
  else
  {
    switch (WinConfiguration->Updates.BetaVersions)
    {
      case asAuto:
        Beta = WinConfiguration->AnyBetaInVersionHistory;
        break;

      case asOn:
        Beta = true;
        break;

      default:
        Beta = false;
        break;
    }
  }
  if (Beta || Force)
  {
    URL = AppendUrlParams(URL, FORMAT(L"beta=%d", (Beta ? 1 : 0)));
  }
  return URL;
}
//---------------------------------------------------------------------------
static THttp * __fastcall CreateHttp(const TUpdatesConfiguration & Updates)
{
  std::unique_ptr<THttp> Http(new THttp());

  UnicodeString ProxyHost;
  int ProxyPort = 0;

  switch (Updates.ConnectionType)
  {
    case ctAuto:
      if (AutodetectProxy(ProxyHost, ProxyPort))
      {
        Configuration->Usage->Inc(L"UpdateProxyAutodetected");
      }
      break;

    case ctProxy:
      ProxyHost = Updates.ProxyHost;
      ProxyPort = Updates.ProxyPort;
      Configuration->Usage->Inc(L"UpdateProxyManual");
      break;
  }

  Http->ProxyHost = ProxyHost;
  Http->ProxyPort = ProxyPort;

  return Http.release();
}
//---------------------------------------------------------------------------
static bool __fastcall DoQueryUpdates(TUpdatesConfiguration & Updates, bool CollectUsage)
{
  bool Complete = false;
  UnicodeString Response;
  THttp * CheckForUpdatesHTTP = CreateHttp(Updates);
  try
  {
    UnicodeString URL = ProgramUrl(LoadStr(UPDATES_URL));
    URL = WantBetaUrl(URL, false);
    URL += L"&dotnet=" + Updates.DotNetVersion;
    URL += L"&console=" + Updates.ConsoleVersion;
    UnicodeString LocaleVersion = WinConfiguration->LocaleVersion();
    if (!LocaleVersion.IsEmpty())
    {
      URL += L"&localever=" + LocaleVersion;
      URL += L"&localecompl=" + LoadStr(TRANSLATION_COMPLETENESS);
    }
    if (!Updates.AuthenticationEmail.IsEmpty())
    {
      RawByteString AuthenticationEmailBuf = RawByteString(UTF8String(Updates.AuthenticationEmail.LowerCase()));
      URL += L"&authentication=" + Sha256(AuthenticationEmailBuf.c_str(), AuthenticationEmailBuf.Length()).LowerCase();
    }

    CheckForUpdatesHTTP->URL = URL;
    // sanity check
    CheckForUpdatesHTTP->ResponseLimit = 102400;
    if (CollectUsage)
    {
      UnicodeString Usage = GetUsageData();

      CheckForUpdatesHTTP->Post(Usage);
    }
    else
    {
      CheckForUpdatesHTTP->Get();
    }
    Response = CheckForUpdatesHTTP->Response;
  }
  __finally
  {
    delete CheckForUpdatesHTTP;
  }

  int CurrentCompoundVer = Configuration->CompoundVersion;

  bool Changed = !Updates.HaveResults;
  Updates.LastCheck = Now();
  Updates.HaveResults = true;
  TUpdatesData PrevResults = Updates.Results;
  Updates.Results.Reset();
  Updates.Results.ForVersion = CurrentCompoundVer;

  while (!Response.IsEmpty())
  {
    UnicodeString Line = CutToChar(Response, L'\n', false);
    UnicodeString Name = CutToChar(Line, L'=', false);
    if (AnsiSameText(Name, "Version"))
    {
      int MajorVer = StrToInt(CutToChar(Line, L'.', false));
      int MinorVer = StrToInt(CutToChar(Line, L'.', false));
      int Release = StrToInt(CutToChar(Line, L'.', false));
      int Build = StrToInt(CutToChar(Line, L'.', false));
      int NewVersion = CalculateCompoundVersion(MajorVer, MinorVer, Release, Build);
      Changed |= (NewVersion != PrevResults.Version);
      if (NewVersion <= CurrentCompoundVer)
      {
        NewVersion = 0;
      }
      Updates.Results.Version = NewVersion;
      Complete = true;
    }
    else if (AnsiSameText(Name, L"Message"))
    {
      Changed |= (PrevResults.Message != Line);
      Updates.Results.Message = Line;
    }
    else if (AnsiSameText(Name, L"Critical"))
    {
      bool NewCritical = (StrToIntDef(Line, 0) != 0);
      Changed |= (PrevResults.Critical != NewCritical);
      Updates.Results.Critical = NewCritical;
    }
    else if (AnsiSameText(Name, L"Release"))
    {
      Changed |= (PrevResults.Release != Line);
      Updates.Results.Release = Line;
    }
    else if (AnsiSameText(Name, L"Disabled"))
    {
      bool NewDisabled = (StrToIntDef(Line, 0) != 0);
      Changed |= (PrevResults.Disabled != NewDisabled);
      Updates.Results.Disabled = NewDisabled;
      Complete = true;
    }
    else if (AnsiSameText(Name, L"Url"))
    {
      Changed |= (PrevResults.Url != Line);
      Updates.Results.Url = Line;
    }
    else if (AnsiSameText(Name, L"UrlButton"))
    {
      Changed |= (PrevResults.UrlButton != Line);
      Updates.Results.UrlButton = Line;
    }
    else if (AnsiSameText(Name, L"NewsUrl"))
    {
      Changed |= (PrevResults.NewsUrl != Line);
      Updates.Results.NewsUrl = Line;
    }
    else if (AnsiSameText(Name, L"NewsSize"))
    {
      TSize NewsSize;
      NewsSize.Width = StrToIntDef(CutToChar(Line, L',', true), 0);
      NewsSize.Height = StrToIntDef(CutToChar(Line, L',', true), 0);
      Changed |= (PrevResults.NewsSize != NewsSize);
      Updates.Results.NewsSize = NewsSize;
    }
    else if (AnsiSameText(Name, L"DownloadUrl"))
    {
      Changed |= (PrevResults.DownloadUrl != Line);
      Updates.Results.DownloadUrl = Line;
    }
    else if (AnsiSameText(Name, L"DownloadSize"))
    {
      Updates.Results.DownloadSize = StrToInt64Def(Line, 0);
    }
    else if (AnsiSameText(Name, L"DownloadSha256"))
    {
      Updates.Results.DownloadSha256 = Line;
    }
    else if (AnsiSameText(Name, L"AuthenticationError"))
    {
      Changed |= (PrevResults.AuthenticationError != Line);
      Updates.Results.AuthenticationError = Line;
    }
    else if (AnsiSameText(Name, L"OpenGettingStarted"))
    {
      Updates.Results.OpenGettingStarted = (StrToIntDef(Line, 0) != 0);
    }
    else if (AnsiSameText(Name, L"DownloadingUrl"))
    {
      Updates.Results.DownloadingUrl = Line;
    }
    else if (AnsiSameText(Name, L"TipsSize"))
    {
      TSize TipsSize;
      TipsSize.Width = StrToIntDef(CutToChar(Line, L',', true), 0);
      TipsSize.Height = StrToIntDef(CutToChar(Line, L',', true), 0);
      Updates.Results.TipsSize = TipsSize;
    }
    else if (AnsiSameText(Name, L"TipsUrl"))
    {
      Updates.Results.TipsUrl = Line;
    }
    else if (AnsiSameText(Name, L"Tips"))
    {
      Updates.Results.Tips = Line;
    }
    else if (AnsiSameText(Name, L"TipsIntervalDays"))
    {
      int TipsIntervalDays = StrToIntDef(Line, Updates.Results.TipsIntervalDays);
      if (TipsIntervalDays < 0)
      {
        TipsIntervalDays = Updates.Results.TipsIntervalDays;
      }
      Updates.Results.TipsIntervalDays = TipsIntervalDays;
    }
    else if (AnsiSameText(Name, L"TipsIntervalRuns"))
    {
      int TipsIntervalRuns = StrToIntDef(Line, Updates.Results.TipsIntervalRuns);
      if (TipsIntervalRuns < 0)
      {
        TipsIntervalRuns = Updates.Results.TipsIntervalRuns;
      }
      Updates.Results.TipsIntervalRuns = TipsIntervalRuns;
    }
  }

  if (Changed)
  {
    Updates.ShownResults = false;
  }

  return Complete;
}
//---------------------------------------------------------------------------
bool __fastcall QueryUpdates(TUpdatesConfiguration & Updates)
{
  return DoQueryUpdates(Updates, false);
}
//---------------------------------------------------------------------------
static void __fastcall DoQueryUpdates(bool CollectUsage)
{
  try
  {
    TUpdatesConfiguration Updates = WinConfiguration->Updates;

    bool Complete = DoQueryUpdates(Updates, CollectUsage);

    WinConfiguration->Updates = Updates;

    if (!Complete)
    {
      EXCEPTION;
    }

    Configuration->Usage->Reset();
    Configuration->Usage->Inc(L"UpdateChecksSucceeded");
  }
  catch(Exception & E)
  {
    Configuration->Usage->Inc(L"UpdateChecksFailed");
    throw ExtException(&E, MainInstructions(LoadStr(CHECK_FOR_UPDATES_ERROR)));
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatUpdatesMessage(UnicodeString Message)
{
  Message = ReplaceStr(Message, "%UPDATE_UNAUTHORIZED%", LoadStr(UPDATE_UNAUTHORIZED));
  Message = ReplaceStr(Message, "%UPDATE_EXPIRED%", LoadStr(UPDATE_EXPIRED));
  Message = ReplaceStr(Message, "%UPDATE_TOO_MANY%", LoadStr(UPDATE_TOO_MANY));
  Message = ReplaceStr(Message, "%UPDATE_MISSING_ADDRESS%", LoadStr(UPDATE_MISSING_ADDRESS));
  Message = ReplaceStr(Message, "%UPDATE_TOO_LOW%", LoadStr(UPDATE_TOO_LOW));
  Message = ReplaceStr(Message, L"|", L"\n");
  return Message;
}
//---------------------------------------------------------------------------
void __fastcall GetUpdatesMessage(UnicodeString & Message, bool & New,
  TQueryType & Type, bool Force)
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  DebugAssert(Updates.HaveResults);
  if (Updates.HaveResults)
  {
    if (Updates.Results.Disabled)
    {
      New = false;
      if (Force)
      {
        Message = LoadStr(UPDATE_DISABLED);
      }
    }
    else
    {
      New = (Updates.Results.Version > 0);
      if (New)
      {
        UnicodeString Version = VersionStrFromCompoundVersion(Updates.Results.Version);
        if (!Updates.Results.Release.IsEmpty())
        {
          Version = FORMAT(L"%s %s", (Version, Updates.Results.Release));
        }
        Message = FMTLOAD(NEW_VERSION4, (Version));
      }
      else
      {
        Message = LoadStr(NO_NEW_VERSION);
      }
    }

    if (!Message.IsEmpty())
    {
      Message = MainInstructions(Message);
    }

    if (!Updates.Results.Message.IsEmpty())
    {
      Message +=
        FMTLOAD(UPDATE_MESSAGE, (FormatUpdatesMessage(Updates.Results.Message)));
    }

    if (!Updates.Results.AuthenticationError.IsEmpty())
    {
      Message +=
        FMTLOAD(UPDATE_MESSAGE, (FormatUpdatesMessage(Updates.Results.AuthenticationError)));
    }
    Type = (Updates.Results.Critical ? qtWarning : qtInformation);
  }
  else
  {
    New = false;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetEnableAutomaticUpdatesUrl()
{
  return AppendUrlParams(LoadStr(DONATE_URL), L"automaticupdates=1");
}
//---------------------------------------------------------------------------
void __fastcall EnableAutomaticUpdates()
{
  OpenBrowser(GetEnableAutomaticUpdatesUrl());
}
//---------------------------------------------------------------------------
static void __fastcall OpenHistory(void * /*Data*/, TObject * /*Sender*/)
{
  Configuration->Usage->Inc(L"UpdateHistoryOpens");
  OpenBrowser(LoadStr(HISTORY_URL));
}
//---------------------------------------------------------------------------
static int __fastcall DownloadSizeToProgress(__int64 Size)
{
  return static_cast<int>(Size / 1024);
}
//---------------------------------------------------------------------------
class TUpdateDownloadThread : public TCompThread
{
public:
  __fastcall TUpdateDownloadThread(TProgressBar * ProgressBar);
  virtual __fastcall ~TUpdateDownloadThread();

  void __fastcall CancelClicked(TObject * Sender);

  bool __fastcall CancelDownload();

  __property bool Done = { read = FDone };

protected:
  virtual void __fastcall Execute();
  void __fastcall UpdateDownloaded();
  void __fastcall HttpDownload(THttp * Sender, __int64 Size, bool & Cancel);
  void __fastcall UpdateProgress();
  void __fastcall ShowException();
  void __fastcall DownloadNotVerified();
  void __fastcall CancelForm();

private:
  TCustomForm * FForm;
  TProgressBar * FProgressBar;
  __int64 FDownloaded;
  std::unique_ptr<Exception> FException;
  std::unique_ptr<THttp> FHttp;
  TUpdatesConfiguration FUpdates;
  bool FDone;
};
//---------------------------------------------------------------------------
__fastcall TUpdateDownloadThread::TUpdateDownloadThread(TProgressBar * ProgressBar) :
  TCompThread(true)
{
  // cache, as the progress bar miht be destroyed already when
  // we need the form at the end of Execute()
  FForm = GetParentForm(ProgressBar);
  FProgressBar = ProgressBar;
  // cache to prevent concurrency
  FUpdates = WinConfiguration->Updates;
  FDone = false;
}
//---------------------------------------------------------------------------
__fastcall TUpdateDownloadThread::~TUpdateDownloadThread()
{
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::Execute()
{
  try
  {
    bool Retried = false;
    bool Retry;

    do
    {
      Retry = false;
      try
      {
        FDownloaded = 0;

        FHttp.reset(CreateHttp(FUpdates));
        FHttp->URL = FUpdates.Results.DownloadUrl;
        FHttp->OnDownload = HttpDownload;
        FHttp->Get();
      }
      catch (EAbort &)
      {
        throw;
      }
      catch (Exception & E)
      {
        // The original URL failed, try to get a fresh one and retry
        if (!Retried)
        {
          try
          {
            // Check if new update data (URL particlarly) is available
            if (QueryUpdates(FUpdates) &&
                !FUpdates.Results.DownloadUrl.IsEmpty())
            {
              Retry = true;
              Retried = true;
            }
          }
          catch (...)
          {
          }
        }

        if (!Retry)
        {
          Configuration->Usage->Inc(L"UpdateFailuresDownload");
          throw ExtException(&E, MainInstructions(LoadStr(UPDATE_DOWNLOAD_ERROR)));
        }
      }
    }
    while (Retry);

    Synchronize(UpdateDownloaded);
  }
  catch (EAbort &)
  {
    // noop
  }
  catch (Exception & E)
  {
    Configuration->Usage->Inc(L"UpdateFailures");
    FException.reset(CloneException(&E));
    Synchronize(ShowException);
  }

  FDone = true;
  Synchronize(CancelForm);
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::CancelForm()
{
  FForm->ModalResult = mrCancel;
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::UpdateDownloaded()
{
  size_t Size = static_cast<size_t>(FHttp->ResponseLength);
  const char * Buffer = FHttp->ResponseRaw.c_str();

  if (FHttp->ResponseLength != FUpdates.Results.DownloadSize)
  {
    DownloadNotVerified();
  }

  UnicodeString Digest = Sha256(Buffer, Size);

  if (!SameText(FUpdates.Results.DownloadSha256, Digest))
  {
    DownloadNotVerified();
  }

  UnicodeString FileName = FUpdates.Results.DownloadUrl;
  int P = FileName.Pos(L"?");
  if (P > 0)
  {
    FileName.SetLength(P - 1);
  }
  P = FileName.LastDelimiter("/");
  if (DebugAlwaysTrue(P > 0))
  {
    FileName.Delete(1, P);
  }
  UnicodeString TemporaryDirectory = WinConfiguration->ExpandedTemporaryDirectory();
  UnicodeString SetupPathBase = IncludeTrailingBackslash(TemporaryDirectory) + FileName;
  UnicodeString SetupPath = SetupPathBase;
  int Index = 0;
  while (FileExists(SetupPath))
  {
    Index++;
    SetupPath =
     ExtractFilePath(SetupPathBase) + ExtractFileNameOnly(SetupPathBase) +
     FORMAT(".%d", (Index)) + ExtractFileExt(SetupPathBase);
  }

  std::unique_ptr<TFileStream> FileStream(new TFileStream(SetupPath, fmCreate));
  FileStream->Write(Buffer, Size);
  FileStream.reset(NULL);

  UnicodeString Params = L"/SILENT /NORESTART /AutomaticUpdate";
  if (FUpdates.Results.OpenGettingStarted)
  {
    Params += L" /OpenGettingStarted";
  }

  if (!ExecuteShell(SetupPath, Params))
  {
    throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (SetupPath)));
  }

  Configuration->Usage->Inc(L"UpdateRuns");
  Application->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::DownloadNotVerified()
{
  throw Exception(MainInstructions(LoadStr(UPDATE_VERIFY_ERROR)));
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::HttpDownload(THttp * /*Sender*/, __int64 Size, bool & Cancel)
{
  FDownloaded = Size;
  Synchronize(UpdateProgress);

  // Do not waste bandwidth, if something goes wrong
  if (FDownloaded > FUpdates.Results.DownloadSize)
  {
    DownloadNotVerified();
  }

  if (Terminated)
  {
    Cancel = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::UpdateProgress()
{
  FProgressBar->Position = DownloadSizeToProgress(FDownloaded);
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::ShowException()
{
  DebugAssert(FException.get() != NULL);
  ShowExtendedException(FException.get());
}
//---------------------------------------------------------------------------
bool __fastcall TUpdateDownloadThread::CancelDownload()
{
  bool Result = !Terminated;
  if (Result)
  {
    Configuration->Usage->Inc(L"UpdateDownloadCancels");
    Terminate();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TUpdateDownloadThread::CancelClicked(TObject * /*Sender*/)
{
  if (CancelDownload())
  {
    WaitFor();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TUpdateDownloadData : public TComponent
{
public:
  TUpdateDownloadThread * Thread;

  __fastcall TUpdateDownloadData() :
    TComponent(NULL)
  {
  }

  virtual __fastcall ~TUpdateDownloadData()
  {
    // will stop the thread
    delete Thread;
  }

  static TUpdateDownloadData * __fastcall Retrieve(TObject * Object)
  {
    TComponent * Component = DebugNotNull(dynamic_cast<TComponent *>(Object));
    TComponent * UpdateDownloadDataComponent = Component->FindComponent(QualifiedClassName());
    return DebugNotNull(dynamic_cast<TUpdateDownloadData *>(UpdateDownloadDataComponent));
  }
};
//---------------------------------------------------------------------------
static void __fastcall DownloadClose(void * /*Data*/, TObject * Sender, TCloseAction & Action)
{
  TUpdateDownloadData * UpdateDownloadData = TUpdateDownloadData::Retrieve(Sender);
  // If the form was closed by CancelForm at the end of the thread, do nothing
  if (!UpdateDownloadData->Thread->Done)
  {
    // Otherwise the form is closing because X was clicked (or maybe Cancel).
    // May this should actually call CancelClicked?
    if (UpdateDownloadData->Thread->CancelDownload())
    {
      Action = caNone;
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall DownloadUpdate(void * /*Data*/, TObject * Sender)
{
  Configuration->Usage->Inc(L"UpdateDownloadStarts");
  TButton * Button = DebugNotNull(dynamic_cast<TButton *>(Sender));
  TForm * Form = DebugNotNull(dynamic_cast<TForm *>(GetParentForm(Button)));
  TPanel * Panel = CreateBlankPanel(Form);

  TProgressBar * ProgressBar = new TProgressBar(Panel);
  ProgressBar->Anchors = TAnchors() << akLeft << akTop << akRight;
  ProgressBar->Top = 0;
  ProgressBar->Left = 0;
  ProgressBar->Width = Panel->Width;
  ProgressBar->Parent = Panel;
  ProgressBar->Max = DownloadSizeToProgress(WinConfiguration->Updates.Results.DownloadSize);
  Panel->Height = ProgressBar->Height;

  InsertPanelToMessageDialog(Form, Panel);

  Button->Enabled = false;

  TButton * CancelButton = dynamic_cast<TButton *>(Form->FindComponent(L"Cancel"));
  CancelButton->Caption = Vcl_Consts_SMsgDlgCancel;

  TUpdateDownloadThread * Thread = new TUpdateDownloadThread(ProgressBar);
  // The form becomes owner of the thread (via TUpdateDownloadData),
  // so the thread is automatically stopped when the form closes, if the "cancel" is pressed.
  // But it gets done only after the form controls are gone, and the thread
  // might try to access them meanwhile. So we stop the thread explicitly.
  CancelButton->OnClick = Thread->CancelClicked;

  TUpdateDownloadData * Data = new TUpdateDownloadData();
  Data->Name = TUpdateDownloadData::QualifiedClassName();
  Data->Thread = Thread;
  Form->InsertComponent(Data);

  UnicodeString DownloadingUrl = WinConfiguration->Updates.Results.DownloadingUrl;
  if (!DownloadingUrl.IsEmpty())
  {
    NavigateMessageDialogToUrl(Form, DownloadingUrl);
  }

  DebugAssert(Form->OnClose == NULL);
  Form->OnClose = MakeMethod<TCloseEvent>(NULL, DownloadClose);

  Thread->Resume();
}
//---------------------------------------------------------------------------
static void __fastcall UpdatesDonateClick(void * /*Data*/, TObject * /*Sender*/)
{
  EnableAutomaticUpdates();
}
//---------------------------------------------------------------------------
static void __fastcall InsertDonateLink(void * /*Data*/, TObject * Sender)
{
  TForm * Dialog = DebugNotNull(dynamic_cast<TForm *>(Sender));
  TPanel * Panel = CreateBlankPanel(Dialog);

  TStaticText * StaticText = new TStaticText(Panel);
  StaticText->Top = 0;
  StaticText->Left = 0;
  StaticText->AutoSize = true;
  StaticText->Caption = LoadStr(UPDATES_DONATE_LINK);
  StaticText->Parent = Panel;
  StaticText->OnClick = MakeMethod<TNotifyEvent>(NULL, UpdatesDonateClick);
  StaticText->TabStop = true;

  LinkLabel(StaticText);

  Panel->Height = StaticText->Height;

  // Currently this is noop (will fail assertion), if MoreMessagesUrl is not set
  // (what should not happen)
  InsertPanelToMessageDialog(Dialog, Panel);
}
//---------------------------------------------------------------------------
bool __fastcall CheckForUpdates(bool CachedResults)
{
  TCustomForm * ActiveForm = Screen->ActiveCustomForm;

  bool Result = false;
  TOperationVisualizer Visualizer;

  try
  {
    if (ActiveForm)
    {
      DebugAssert(ActiveForm->Enabled);
      ActiveForm->Enabled = false;
    }

    TUpdatesConfiguration Updates = WinConfiguration->Updates;
    bool Cached =
      Updates.HaveValidResultsForVersion(Configuration->CompoundVersion) &&
      CachedResults;
    if (!Cached)
    {
      DoQueryUpdates(WinConfiguration->CollectUsage);
      // reread new data
      Updates = WinConfiguration->Updates;
    }

    if (!Updates.ShownResults)
    {
      Updates.ShownResults = true;
      WinConfiguration->Updates = Updates;
    }
    DebugAssert(Updates.HaveResults);

    UnicodeString Message;
    bool New;
    TQueryType Type;
    GetUpdatesMessage(Message, New, Type, true);

    Configuration->Usage->Inc(L"UpdateDisplays");
    if (New)
    {
      Configuration->Usage->Inc(L"UpdateDisplaysNew");
    }

    if (Updates.HaveResults &&
        (double(Updates.Period) > 0) &&
        // do not chow next check time, if we have new version info
        !New)
    {
      Message += L"\n\n" +
        FMTLOAD(UPDATE_NEXT, (FormatDateTime("ddddd", Updates.LastCheck + Updates.Period)));
    }

    int Answers = qaOK |
      // show "what's new" button only when change list URL was not provided in results
      FLAGMASK(New && Updates.Results.NewsUrl.IsEmpty(), qaAll) |
      FLAGMASK(New, qaCancel) |
      FLAGMASK(!Updates.Results.Url.IsEmpty(), qaYes);
    TQueryButtonAlias Aliases[4];
    Aliases[0].Button = qaYes;
    if (Updates.Results.UrlButton.IsEmpty())
    {
      Aliases[0].Alias = LoadStr(UPDATE_URL_BUTTON);
    }
    else
    {
      Aliases[0].Alias = Updates.Results.UrlButton;
    }
    Aliases[1].Button = qaAll;
    Aliases[1].Alias = LoadStr(WHATS_NEW_BUTTON);
    Aliases[1].OnClick = MakeMethod<TNotifyEvent>(NULL, OpenHistory);
    Aliases[2].Button = qaCancel;
    Aliases[2].Alias = Vcl_Consts_SMsgDlgClose;
    // Used only when New == true, see AliasesCount below
    Aliases[3].Button = qaOK;
    Aliases[3].Alias = LoadStr(UPGRADE_BUTTON);
    if (!Updates.Results.DownloadUrl.IsEmpty())
    {
      Aliases[3].OnClick = MakeMethod<TNotifyEvent>(NULL, DownloadUpdate);
      Aliases[3].ElevationRequired = true;
    }

    TMessageParams Params;
    Params.Aliases = Aliases;
    Params.MoreMessagesUrl = Updates.Results.NewsUrl;
    Params.MoreMessagesSize = Updates.Results.NewsSize;
    // alias "ok" button to "upgrade" only if we have new version
    Params.AliasesCount = LENOF(Aliases) - (New ? 0 : 1);

    if (New)
    {
      Params.ImageName = L"Installer";
    }

    std::unique_ptr<TForm> Dialog(
      CreateMoreMessageDialogEx(Message, NULL, Type, Answers, HELP_UPDATES, &Params));

    if (New)
    {
      if (Updates.Results.DownloadUrl.IsEmpty() && IsInstalled())
      {
        DebugAssert(Dialog->OnShow == NULL);
        // InsertDonateLink need to be called only after MessageBrowser is created
        Dialog->OnShow = MakeMethod<TNotifyEvent>(NULL, InsertDonateLink);
      }
    }

    unsigned int Answer = ExecuteMessageDialog(Dialog.get(), Answers, &Params);
    switch (Answer)
    {
      case qaOK:
        if (New)
        {
          Configuration->Usage->Inc(L"UpdateDownloadOpens");
          UnicodeString UpgradeUrl = ProgramUrl(LoadStr(UPGRADE_URL));
          UpgradeUrl = WantBetaUrl(UpgradeUrl, true);
          UpgradeUrl = AppendUrlParams(UpgradeUrl, FORMAT(L"to=%s", (VersionStrFromCompoundVersion(Updates.Results.Version))));
          OpenBrowser(UpgradeUrl);
          Result = true;
        }
        break;

      case qaYes:
        OpenBrowser(Updates.Results.Url);
        break;

      case qaAll:
        DebugFail();
        break;
    }
  }
  __finally
  {
    if (ActiveForm)
    {
      ActiveForm->Enabled = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
class TUpdateThread : public TCompThread
{
public:
  __fastcall TUpdateThread(TThreadMethod OnUpdatesChecked);
protected:
  virtual void __fastcall Execute();
  TThreadMethod FOnUpdatesChecked;
};
//---------------------------------------------------------------------------
TUpdateThread * UpdateThread = NULL;
//---------------------------------------------------------------------------
__fastcall TUpdateThread::TUpdateThread(TThreadMethod OnUpdatesChecked) :
  TCompThread(false),
  FOnUpdatesChecked(OnUpdatesChecked)
{
}
//---------------------------------------------------------------------------
void __fastcall TUpdateThread::Execute()
{
  try
  {
    DoQueryUpdates(WinConfiguration->CollectUsage);
    if (FOnUpdatesChecked != NULL)
    {
      Synchronize(FOnUpdatesChecked);
    }
  }
  catch(...)
  {
    // ignore errors
  }
}
//---------------------------------------------------------------------------
void __fastcall StartUpdateThread(TThreadMethod OnUpdatesChecked)
{
  DebugAssert(UpdateThread == NULL);
  UpdateThread = new TUpdateThread(OnUpdatesChecked);
}
//---------------------------------------------------------------------------
void __fastcall StopUpdateThread()
{
  if (UpdateThread != NULL)
  {
    SAFE_DESTROY(UpdateThread);
  }
}
//---------------------------------------------------------------------------
void __fastcall SetupInitialize()
{
  WinConfiguration->UpdateJumpList();
}
//---------------------------------------------------------------------------
static bool __fastcall AddJumpListCategory(TStrings * Names,
  UnicodeString AdditionalParams, TStringList * Removed,
  ICustomDestinationList * DestinationList, UnicodeString CategoryName,
  int IconIndex)
{
  bool Result = false;
  IObjectCollection * Collection = NULL;
  if (SUCCEEDED(CoCreateInstance(CLSID_EnumerableObjectCollection, NULL,
        CLSCTX_INPROC_SERVER, IID_IObjectCollection, (void**)&Collection)))
  {
    try
    {
      AddToList(AdditionalParams, TProgramParams::FormatSwitch(JUMPLIST_SWITCH), L" ");

      int Count = 0;
      for (int Index = 0; Index < Names->Count; Index++)
      {
        IShellLink * Link =
          CreateDesktopSessionShortCut(
            Names->Strings[Index], L"", AdditionalParams, -1, IconIndex, true);

        wchar_t Desc[2048];
        if (SUCCEEDED(Link->GetDescription(Desc, sizeof(Desc) - 1)))
        {
          if (Removed->IndexOf(Desc) < 0)
          {
            try
            {
              DebugCheck(SUCCEEDED(Collection->AddObject(Link)));
              Count++;
            }
            __finally
            {
              Link->Release();
            }
          }
          else
          {
            Names->Delete(Index);
            Index--;
          }
        }
      }

      if (Count > 0)
      {
        IObjectArray * Array;
        if (SUCCEEDED(Collection->QueryInterface(IID_IObjectArray, (void**)&Array)))
        {
          try
          {
            Result = SUCCEEDED(
              DestinationList->AppendCategory(CategoryName.c_str(), Array));
          }
          __finally
          {
            Array->Release();
          }
        }
      }
    }
    __finally
    {
      Collection->Release();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall UpdateJumpList(TStrings * SessionNames, TStrings * WorkspaceNames)
{
  ICustomDestinationList * DestinationList = NULL;
  IObjectArray * RemovedArray = NULL;
  TStringList * Removed = NULL;
  int OldErrMode = SetErrorMode(SEM_FAILCRITICALERRORS);

  try
  {
    if (SUCCEEDED(CoCreateInstance(CLSID_DestinationList, NULL,
          CLSCTX_INPROC_SERVER, IID_ICustomDestinationList, (void**)&DestinationList)))
    {

      unsigned int MinSlots;
      unsigned int * PMinSlots = &MinSlots;
      void ** PRemovedArray = (void**)&RemovedArray;
      HRESULT Result = DestinationList->BeginList(PMinSlots, IID_IObjectArray, PRemovedArray);
      if (SUCCEEDED(Result) && DebugAlwaysTrue(RemovedArray != NULL))
      {
        Removed = new TStringList();

        unsigned int RemovedCount;
        if (FAILED(RemovedArray->GetCount(&RemovedCount)))
        {
          RemovedCount = 0;
        }

        for (unsigned int Index = 0; Index < RemovedCount; Index++)
        {
          IShellLink * Link;
          wchar_t Desc[2048];
          if (SUCCEEDED(RemovedArray->GetAt(Index, IID_IShellLink, (void**)&Link)) &&
              SUCCEEDED(Link->GetDescription(Desc, sizeof(Desc) - 1)))
          {
            Removed->Add(Desc);
          }
        }

        AddJumpListCategory(
          WorkspaceNames, L"", Removed, DestinationList,
          LoadStr(JUMPLIST_WORKSPACES), WORKSPACE_ICON);

        AddJumpListCategory(
          SessionNames, TProgramParams::FormatSwitch(UPLOAD_IF_ANY_SWITCH), Removed, DestinationList,
          LoadStr(JUMPLIST_RECENT), SITE_ICON);

        if (DestinationList != NULL)
        {
          DestinationList->CommitList();
        }
      }
    }
  }
  __finally
  {
    SetErrorMode(OldErrMode);
    if (RemovedArray != NULL)
    {
      RemovedArray->Release();
    }
    if (DestinationList != NULL)
    {
      DestinationList->Release();
    }
    delete Removed;
  }
}
//---------------------------------------------------------------------------
bool __fastcall AnyOtherInstanceOfSelf()
{

  HANDLE Snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, NULL);

  bool Result = false;

  try
  {
    unsigned int Process = GetCurrentProcessId();
    UnicodeString ExeBaseName = ExtractFileBaseName(Application->ExeName);

    PROCESSENTRY32 ProcessEntry;
    ProcessEntry.dwSize = sizeof(PROCESSENTRY32);

    if (Process32First(Snapshot, &ProcessEntry))
    {
      while (!Result && Process32Next(Snapshot, &ProcessEntry))
      {
        // we should check if the process is running in the same session,
        // but for that we probably need some special privileges
        if ((Process != ProcessEntry.th32ProcessID) &&
            SameText(ExtractFileBaseName(ProcessEntry.szExeFile), ExeBaseName))
        {
          Result = true;
        }
      }
    }
  }
  __finally
  {
    CloseHandle(Snapshot);
  }

  return Result;
}
//---------------------------------------------------------------------------
static bool __fastcall DoIsInstalled(HKEY RootKey)
{
  std::unique_ptr<TRegistry> Registry(new TRegistry(KEY_READ));
  Registry->RootKey = RootKey;
  bool Result =
    Registry->OpenKey(L"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\winscp3_is1", false);
  if (Result)
  {
    UnicodeString InstallPath = ExcludeTrailingBackslash(Registry->ReadString(L"Inno Setup: App Path"));
    UnicodeString ExePath = ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName));
    Result =
      !InstallPath.IsEmpty() &&
      CompareFileName(ExePath, InstallPath);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsInstalled()
{
  return
    DoIsInstalled(HKEY_LOCAL_MACHINE) ||
    DoIsInstalled(HKEY_CURRENT_USER);
}
//---------------------------------------------------------------------------
static TStringList * __fastcall TextToTipList(const UnicodeString & Text)
{
  std::unique_ptr<TStringList> List(new TStringList());
  List->CommaText = Text;
  return List.release();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FirstUnshownTip()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  std::unique_ptr<TStringList> Tips(TextToTipList(Updates.Results.Tips));
  Tips->CaseSensitive = false;
  std::unique_ptr<TStringList> TipsSeen(TextToTipList(WinConfiguration->TipsSeen));
  TipsSeen->CaseSensitive = false;

  int LastTipSeen = -1;
  for (int Index = 0; Index < TipsSeen->Count; Index++)
  {
    int TipIndex = Tips->IndexOf(TipsSeen->Names[Index]);
    if (TipIndex >= 0)
    {
      LastTipSeen = TipIndex;
    }
  }

  UnicodeString Result;
  if (LastTipSeen < Tips->Count - 1)
  {
    Result = Tips->Strings[LastTipSeen + 1];
  }
  return Result;
}
//---------------------------------------------------------------------------
class TTipsData : public TComponent
{
public:
  __fastcall TTipsData() :
    TComponent(NULL)
  {
  }

  int Index;
  TStringList * Tips;

  static TTipsData * __fastcall Retrieve(TObject * Object)
  {
    TComponent * Component = DebugNotNull(dynamic_cast<TComponent *>(Object));
    TComponent * TipsDataComponent = Component->FindComponent(QualifiedClassName());
    return DebugNotNull(dynamic_cast<TTipsData *>(TipsDataComponent));
  }
};
//---------------------------------------------------------------------------
static UnicodeString __fastcall TipsMessage(TTipsData * TipsData)
{
  return FMTLOAD(TIPS_MESSAGE, (TipsData->Index + 1, TipsData->Tips->Count));
}
//---------------------------------------------------------------------------
static void __fastcall UpdateTipsForm(TCustomForm * Form)
{
  TTipsData * TipsData = TTipsData::Retrieve(Form);

  TButton * PrevButton = DebugNotNull(dynamic_cast<TButton *>(Form->FindComponent(L"Yes")));
  PrevButton->Enabled = (TipsData->Index > 0);
  TButton * NextButton = DebugNotNull(dynamic_cast<TButton *>(Form->FindComponent(L"No")));
  NextButton->Enabled = (TipsData->Index < TipsData->Tips->Count - 1);

  TPanel * Panel = DebugNotNull(dynamic_cast<TPanel *>(Form->FindComponent(L"Panel")));
  TLabel * MessageLabel = DebugNotNull(dynamic_cast<TLabel *>(Panel->FindComponent(L"MainMessage")));
  MessageLabel->Caption = TipsMessage(TipsData);
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall TipUrl(TTipsData * TipsData)
{
  UnicodeString Tip = TipsData->Tips->Strings[TipsData->Index];
  UnicodeString TipParams = FORMAT(L"tip=%s", (Tip));
  return AppendUrlParams(WinConfiguration->Updates.Results.TipsUrl, TipParams);
}
//---------------------------------------------------------------------------
static void __fastcall TipSeen(const UnicodeString & Tip)
{
  std::unique_ptr<TStringList> TipsSeen(TextToTipList(WinConfiguration->TipsSeen));
  TipsSeen->Values[Tip] = FormatDateTime(L"yyyy-mm-dd", Now());
  WinConfiguration->TipsSeen = TipsSeen->CommaText;
  WinConfiguration->TipsShown = Now();
  WinConfiguration->RunsSinceLastTip = 0;
  // prevent parallel app instances showing the same tip
  WinConfiguration->Save();
}
//---------------------------------------------------------------------------
static void __fastcall PrevNextTipClick(void * Data, TObject * Sender)
{
  TCustomForm * Form = GetParentForm(dynamic_cast<TControl *>(Sender));
  TTipsData * TipsData = TTipsData::Retrieve(Form);
  TipsData->Index += reinterpret_cast<int>(Data);
  UpdateTipsForm(Form);
  TipSeen(TipsData->Tips->Strings[TipsData->Index]);
  UnicodeString Url = TipUrl(TipsData);
  NavigateMessageDialogToUrl(Form, Url);
}
//---------------------------------------------------------------------------
static void __fastcall ShowTip(bool AutoShow)
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  UnicodeString Tip = FirstUnshownTip();
  std::unique_ptr<TStringList> Tips(TextToTipList(Updates.Results.Tips));
  Tips->CaseSensitive = false;
  int Index;
  if (Tip.IsEmpty())
  {
    Index = Tips->Count - 1;
    Tip = Tips->Strings[Index];
  }
  else
  {
    Index = Tips->IndexOf(Tip);
  }

  std::unique_ptr<TTipsData> TipsData(new TTipsData());
  TipsData->Name = TTipsData::QualifiedClassName();
  TipsData->Index = Index;
  TipsData->Tips = Tips.get();

  UnicodeString Message = MainInstructions(TipsMessage(TipsData.get()));

  TQueryButtonAlias Aliases[3];
  Aliases[0].Button = qaYes;
  Aliases[0].Alias = LoadStr(PREV_BUTTON);
  Aliases[0].OnClick = MakeMethod<TNotifyEvent>(reinterpret_cast<void *>(-1), PrevNextTipClick);
  Aliases[1].Button = qaNo;
  Aliases[1].Alias = LoadStr(NEXT_BUTTON);
  Aliases[1].OnClick = MakeMethod<TNotifyEvent>(reinterpret_cast<void *>(+1), PrevNextTipClick);
  Aliases[2].Button = qaCancel;
  Aliases[2].Alias = LoadStr(CLOSE_BUTTON);

  TMessageParams Params;
  Params.CustomCaption = LoadStr(TIPS_TITLE);
  Params.MoreMessagesSize = Updates.Results.TipsSize;
  Params.MoreMessagesUrl = TipUrl(TipsData.get());
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);
  Params.ImageName = L"Bulb On n p";

  if (AutoShow)
  {
    // Won't be used automatically as we have more than the "OK" button
    Params.NeverAskAgainTitle = LoadStr(NEVER_SHOW_AGAIN);
    Params.NeverAskAgainAnswer = qaCancel;
    Params.Params |= mpNeverAskAgainCheck;
  }

  int Answers = qaYes | qaNo | qaCancel;
  std::unique_ptr<TForm> Dialog(CreateMoreMessageDialogEx(Message, NULL, qtInformation, Answers, HELP_TIPS, &Params));
  Dialog->InsertComponent(TipsData.release());
  UpdateTipsForm(Dialog.get());
  TipSeen(Tip);
  unsigned int Result = ExecuteMessageDialog(Dialog.get(), Answers, &Params);

  if ((Result == qaNeverAskAgain) && DebugAlwaysTrue(AutoShow))
  {
    WinConfiguration->ShowTips = false;
  }

  WinConfiguration->Updates = Updates;
}
//---------------------------------------------------------------------------
void __fastcall AutoShowNewTip()
{
  Configuration->Usage->Inc(L"TipsShownAuto");
  ShowTip(true);
}
//---------------------------------------------------------------------------
void __fastcall ShowTips()
{
  {
    TOperationVisualizer Visualizer;
    DoQueryUpdates(false);
  }

  if (WinConfiguration->Updates.Results.Tips.IsEmpty())
  {
    throw Exception(MainInstructions(LoadStr(TIPS_NONE)));
  }

  Configuration->Usage->Inc(L"TipsShownCommand");
  ShowTip(false);
}
//---------------------------------------------------------------------------
void __fastcall TipsUpdateStaticUsage()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  std::unique_ptr<TStringList> Tips(TextToTipList(Updates.Results.Tips));
  Configuration->Usage->Set(L"TipsCount", Tips->Count);
  std::unique_ptr<TStringList> TipsSeen(TextToTipList(WinConfiguration->TipsSeen));
  Configuration->Usage->Set(L"TipsSeen", TipsSeen->Count);
}
