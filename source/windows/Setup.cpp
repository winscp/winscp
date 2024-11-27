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
#include <OperationWithTimeout.hpp>
#include <Soap.HTTPUtil.hpp>
#include <Web.HTTPApp.hpp>
#include <System.IOUtils.hpp>
#include <WinApi.h>
#include <System.NetEncoding.hpp>
//---------------------------------------------------------------------------
#define KEY _T("SYSTEM\\CurrentControlSet\\Control\\") \
            _T("Session Manager\\Environment")
// when the PATH registry key is over aprox 2048 characters,
// PATH as well as WINDIR variables are actually not set, breaking the system
#define MAX_PATH_LEN 2000

/* Command line options. */
UnicodeString LastPathError;
//---------------------------------------------------------------------------
UnicodeString NetVersionStr;
UnicodeString NetCoreVersionStr;
UnicodeString PowerShellVersionStr;
UnicodeString PowerShellCoreVersionStr;
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
    for (; towlower(*str1) == towlower(*str2); ++str1, ++str2)
        if (*str1 == L'\0')
            return 0;
    return towlower(*str1) - towlower(*str2);
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
  unsigned long SessionId = 0;
  if (!ProcessIdToSessionId(GetCurrentProcessId(), &SessionId) ||
      (SessionId == 0))
  {
    AppLog(L"Program does not seem to be running in user session, not propagating search path changes");
  }
  else
  {
    DWORD send_message_result;
    LONG ret = SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
                             (LPARAM)_T("Environment"), SMTO_ABORTIFHUNG,
                             5000, &send_message_result);
    if (ret != ERROR_SUCCESS && GetLastError() != 0)
    {
      err_out_sys(_T("Cannot propagate the new enviroment to ")
                  _T("other processes. The new value will be ")
                  _T("available after a reboot."), GetLastError());
      SimpleErrorDialog(LastPathError);
      LastPathError = L"";
    }
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
        AppLogFmt(L"Previous search path: %s", (reg_str));
        if (find_reg_str(reg_str, path, NULL))
        {
          AppLog(L"Path is already in search path");
        }
        else
        {
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
              AppLogFmt(L"New search path written: %s", (reg_str));
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
  AppLogFmt(L"Adding '%s' to search path", (Path));
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
static UnicodeString KeyName(HKEY RootKey, const UnicodeString & Key)
{
  return FORMAT(L"%s\\%s", (RootKeyToStr(RootKey, L"Unknown"), Key));
}
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

    UnicodeString AKeyName = KeyName(Registry->RootKey, Key);
    if (CanDelete)
    {
      for (int Index = 0; Index < List->Count; Index++)
      {
        DeleteKeyIfEmpty(Registry, IncludeTrailingBackslash(Key) + List->Strings[Index], false);
      }

      // will fail, if not all subkeys got removed
      Registry->DeleteKey(Key);
      AppLogFmt(L"Deleted key %s", (AKeyName));
    }
    else
    {
      AppLogFmt(L"Cannot delete non-empty key %s", (AKeyName));
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
    UnicodeString Key = SoftwareClassesBaseKey + Protocol;
    if (Registry->OpenKey(Key, true))
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
      AppLogFmt(L"Created %s", (KeyName(Registry->RootKey, Key)));
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

  UnicodeString Key = SoftwareClassesBaseKey + Protocol;
  if (Registry->OpenKey(Key, false) &&
      Registry->OpenKey(L"shell", true) &&
      Registry->OpenKey(L"open", true) &&
      Registry->OpenKey(L"command", true))
  {
    Registry->WriteString(L"", FORMAT(L"\"%s\" %s \"%%1\"", (Application->ExeName, TProgramParams::FormatSwitch(UNSAFE_SWITCH))));
    Registry->CloseKey();
    AppLogFmt(L"Added command to %s", (KeyName(RootKey, Key)));
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

    // get rid of any HKCU registraction that would override the HKLM one
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
  RegisterAsUrlHandler(Prefix + WebDAVProtocol.UpperCase());
  RegisterAsUrlHandler(Prefix + WebDAVSProtocol.UpperCase());
  RegisterAsUrlHandler(Prefix + S3Protocol.UpperCase());
}
//---------------------------------------------------------------------------
static void __fastcall UnregisterAsUrlHandlers(const UnicodeString & Prefix, bool UnregisterProtocol)
{
  UnregisterAsUrlHandler(Prefix + SftpProtocol, UnregisterProtocol);
  UnregisterAsUrlHandler(Prefix + ScpProtocol, UnregisterProtocol);
  UnregisterAsUrlHandler(Prefix + WebDAVProtocol, UnregisterProtocol);
  UnregisterAsUrlHandler(Prefix + WebDAVSProtocol, UnregisterProtocol);
  UnregisterAsUrlHandler(Prefix + S3Protocol, UnregisterProtocol);
}
//---------------------------------------------------------------------------
static const UnicodeString GenericUrlHandler(L"WinSCP.Url");
//---------------------------------------------------------------------------
static void __fastcall RegisterProtocolForDefaultPrograms(HKEY RootKey, const UnicodeString & Protocol)
{
  // Register protocol, if it does not exist yet.
  // Prior to Windows 8, we need to register ourselves as legacy handler to
  // become the default handler. On Windows 8, it's automatic as long as no other
  // application is registered for the protocol (i.e. RegisterProtocol would be enough).
  // Inconsistently with other calls, this does not use UpperCase().
  RegisterAsUrlHandler(RootKey, Protocol);

  // see https://learn.microsoft.com/en-us/windows/win32/shell/default-programs#registering-an-application-for-use-with-default-programs
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

  UnicodeString Description = LoadStr(REGISTERED_APP_DESC5);
  Registry->WriteString(L"ApplicationDescription", Description);

  if (!Registry->OpenKey(L"UrlAssociations", true))
  {
    Abort();
  }

  Registry->WriteString(Protocol, GenericUrlHandler);
  Registry->CloseKey();

  AppLogFmt(L"Added capabilities to %s", (KeyName(RootKey, CapabilitiesKey)));

  // register application

  UnicodeString ApplicationsKey = L"Software\\RegisteredApplications";
  if (!Registry->OpenKey(ApplicationsKey, true))
  {
    Abort();
  }

  UnicodeString AppName = AppNameString();
  Registry->WriteString(AppName, CapabilitiesKey);
  Registry->CloseKey();
  AppLogFmt(L"Registered %s in %s", (AppName, KeyName(RootKey, ApplicationsKey)));
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
  RegisterProtocolForDefaultPrograms(RootKey, WebDAVProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, WebDAVSProtocol);
  RegisterProtocolForDefaultPrograms(RootKey, S3Protocol);
  // deliberately not including http,
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
  UnregisterProtocolForDefaultPrograms(RootKey, WebDAVProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, WebDAVSProtocol, ForceHandlerUnregistration);
  UnregisterProtocolForDefaultPrograms(RootKey, S3Protocol, ForceHandlerUnregistration);

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
      // Maybe we should skip this if HKLM key existed already (we are running non-privileged)
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
  AppLog(L"Registering to handle protocol URL addresses");
  AppLog(L"Registering as default program");
  RegisterForDefaultPrograms();

  AppLog(L"Registering for non-browser protocols with prefix");
  RegisterAsNonBrowserUrlHandler(WinSCPProtocolPrefix);
  AppLog(L"Registering for browser protocols with prefix");
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpsProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + FtpesProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + HttpProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + HttpsProtocol.UpperCase());
  RegisterAsUrlHandler(WinSCPProtocolPrefix + SshProtocol.UpperCase());

  AppLog(L"Notifying about changes");
  NotifyChangedAssociations();
  AppLog(L"Registration done");
}
//---------------------------------------------------------------------------
void __fastcall UnregisterForProtocols()
{
  AppLog(L"Unregistering from handling protocol URL addresses");
  UnregisterAsUrlHandlers(UnicodeString(), false);
  UnregisterAsUrlHandlers(WinSCPProtocolPrefix, true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpsProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + FtpesProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + HttpProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + HttpsProtocol.UpperCase(), true);
  UnregisterAsUrlHandler(WinSCPProtocolPrefix + SshProtocol.UpperCase(), true);

  UnregisterProtocolsForDefaultPrograms(HKEY_CURRENT_USER, false);
  UnregisterProtocolsForDefaultPrograms(HKEY_LOCAL_MACHINE, false);

  AppLog(L"Notifying about changes");
  NotifyChangedAssociations();
  AppLog(L"Unregistration done");
}
//---------------------------------------------------------------------------
void __fastcall LaunchAdvancedAssociationUI()
{
  RegisterForDefaultPrograms();
  NotifyChangedAssociations();
  // sleep recommended by https://learn.microsoft.com/en-us/windows/win32/shell/default-programs#becoming-the-default-browser
  Sleep(1000);

  if (IsWin10())
  {
    // WORKAROUND: On Windows 10, the IApplicationAssociationRegistrationUI::LaunchAdvancedAssociationUI does not work.
    // https://stackoverflow.com/q/32178986/850848
    // This approach (IOpenControlPanel::Open) works on Windows 7 too, but not on Windows Vista.
    IOpenControlPanel * OpenControlPanel;

    HRESULT Result =
      CoCreateInstance(CLSID_OpenControlPanel,
        NULL, CLSCTX_INPROC, __uuidof(IOpenControlPanel), (void**)&OpenControlPanel);
    if (SUCCEEDED(Result))
    {
      // This does not work anymore since April 2018 Update, it now has the same effect as mere "pageDefaultProgram".
      UnicodeString Page = FORMAT(L"pageDefaultProgram\\pageAdvancedSettings?pszAppName=%s", (AppNameString()));
      OpenControlPanel->Open(L"Microsoft.DefaultPrograms", Page.c_str(), NULL);
      OpenControlPanel->Release();
    }
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
  std::unique_ptr<TStrings> Folders(WinConfiguration->FindTemporaryFolders());
  if (Folders.get() != NULL)
  {
    bool Continue = true;
    if (WinConfiguration->ConfirmTemporaryDirectoryCleanup)
    {
      Configuration->Usage->Inc(L"TemporaryDirectoryCleanupConfirmations");

      TQueryButtonAlias Aliases[1];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(OPEN_BUTTON);
      TMessageParams Params(mpNeverAskAgainCheck);
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);

      unsigned int Answer = MoreMessageDialog(
        FMTLOAD(CLEANTEMP_CONFIRM2, (Folders->Count)), Folders.get(),
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

    if (Continue)
    {
      try
      {
        WinConfiguration->CleanupTemporaryFolders(Folders.get());
      }
      catch (Exception &E)
      {
        ShowExtendedException(&E);
      }
    }
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
  UnicodeString Medium = IsUWP() ? L"uwp" : L"app";
  // Beware that these parameters may get truncated if URL is too long,
  // such as with ERROR_REPORT_URL2
  UnicodeString Params = FORMAT(L"utm_source=winscp&utm_medium=%s&utm_campaign=%s", (Medium, Version));

  return AppendUrlParams(URL, Params);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ProgramUrl(UnicodeString URL)
{
  TVSFixedFileInfo * FileInfo = Configuration->FixedApplicationInfo;
  UnicodeString CurrentVersionStr =
    FORMAT(L"%d.%d.%d.%d",
      (HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
       HIWORD(FileInfo->dwFileVersionLS), LOWORD(FileInfo->dwFileVersionLS)));
  int IsInstalledFlag = (IsInstalled() ? 1 : (IsInstalledMsi() ? 2 : 0));
  UnicodeString Params =
    FORMAT(L"v=%s&lang=%s&isinstalled=%d",
      (CurrentVersionStr,
      GUIConfiguration->AppliedLocaleHex,
      IsInstalledFlag));

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

  if (!ProxyHost.IsEmpty())
  {
    AppLogFmt("Using proxy: %s:%d", (ProxyHost, ProxyPort));
  }
  Http->ProxyHost = ProxyHost;
  Http->ProxyPort = ProxyPort;

  return Http.release();
}
//---------------------------------------------------------------------------
THttp * __fastcall CreateHttp()
{
  return CreateHttp(WinConfiguration->Updates);
}
//---------------------------------------------------------------------------
UnicodeString GetUpdatesCertificate()
{
  UnicodeString Result = ReadResource(L"UPDATES_ROOT_CA");
  Result = ReplaceStr(Result, L"-----BEGIN CERTIFICATE-----", EmptyStr);
  Result = ReplaceStr(Result, L"-----END CERTIFICATE-----", EmptyStr);
  Result = ReplaceStr(Result, L"\n", EmptyStr);
  Result = ReplaceStr(Result, L"\r", EmptyStr);
  return Result;
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
    UnicodeString LocaleVersion = WinConfiguration->AppliedLocaleVersion();
    if (!LocaleVersion.IsEmpty())
    {
      URL += L"&localever=" + LocaleVersion;
      URL += L"&localecompl=" + LoadStr(TRANSLATION_COMPLETENESS);
    }
    URL += L"&firstrun=" + EncodeUrlString(WinConfiguration->FirstRun);
    if (!IsUWP())
    {
      // Even if donor email is inherited from normal installation,
      // do not use it as this all is merely to report usage statistics, not to check for updates, in UWP.
      if (!Updates.AuthenticationEmail.IsEmpty())
      {
        RawByteString AuthenticationEmailBuf = RawByteString(UTF8String(Updates.AuthenticationEmail.LowerCase()));
        URL += L"&authentication=" + Sha256(AuthenticationEmailBuf.c_str(), AuthenticationEmailBuf.Length()).LowerCase();
      }
    }
    else
    {
      URL += L"&package=" + EncodeUrlString(GetPackageName());
    }
    if (!Updates.Mode.IsEmpty())
    {
      URL += L"&mode=" + EncodeUrlString(Updates.Mode);
    }

    AppLogFmt(L"Updates check URL: %s", (URL));
    CheckForUpdatesHTTP->URL = URL;
    // sanity check
    CheckForUpdatesHTTP->ResponseLimit = BasicHttpResponseLimit;
    CheckForUpdatesHTTP->Certificate = GetUpdatesCertificate();
    try
    {
      if (CollectUsage)
      {
        UnicodeString Usage = Configuration->Usage->Serialize();

        CheckForUpdatesHTTP->Post(Usage);
      }
      else
      {
        CheckForUpdatesHTTP->Get();
      }
    }
    catch (...)
    {
      if (CheckForUpdatesHTTP->IsCertificateError())
      {
        AppLog(L"Certificate error detected.");
        Configuration->Usage->Inc(L"UpdateCertificateErrors");
      }
      throw;
    }
    Response = CheckForUpdatesHTTP->Response;
    AppLogFmt(L"Updates check response: %s", (Response));
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
    if (SameText(Name, "Version"))
    {
      int NewVersion = StrToCompoundVersion(Line);
      Changed |= (NewVersion != PrevResults.Version);
      if (NewVersion <= CurrentCompoundVer)
      {
        NewVersion = 0;
      }
      Updates.Results.Version = NewVersion;
      Complete = true;
    }
    else if (SameText(Name, L"Message"))
    {
      Changed |= (PrevResults.Message != Line);
      Updates.Results.Message = Line;
    }
    else if (SameText(Name, L"Critical"))
    {
      bool NewCritical = (StrToIntDef(Line, 0) != 0);
      Changed |= (PrevResults.Critical != NewCritical);
      Updates.Results.Critical = NewCritical;
    }
    else if (SameText(Name, L"Release"))
    {
      Changed |= (PrevResults.Release != Line);
      Updates.Results.Release = Line;
    }
    else if (SameText(Name, L"Disabled"))
    {
      bool NewDisabled = (StrToIntDef(Line, 0) != 0);
      Changed |= (PrevResults.Disabled != NewDisabled);
      Updates.Results.Disabled = NewDisabled;
      Complete = true;
    }
    else if (SameText(Name, L"Url"))
    {
      Changed |= (PrevResults.Url != Line);
      Updates.Results.Url = Line;
    }
    else if (SameText(Name, L"UrlButton"))
    {
      Changed |= (PrevResults.UrlButton != Line);
      Updates.Results.UrlButton = Line;
    }
    else if (SameText(Name, L"NewsUrl"))
    {
      Changed |= (PrevResults.NewsUrl != Line);
      Updates.Results.NewsUrl = Line;
    }
    else if (SameText(Name, L"NewsSize"))
    {
      TSize NewsSize;
      NewsSize.Width = StrToIntDef(CutToChar(Line, L',', true), 0);
      NewsSize.Height = StrToIntDef(CutToChar(Line, L',', true), 0);
      Changed |= (PrevResults.NewsSize != NewsSize);
      Updates.Results.NewsSize = NewsSize;
    }
    else if (SameText(Name, L"DownloadUrl"))
    {
      Changed |= (PrevResults.DownloadUrl != Line);
      Updates.Results.DownloadUrl = Line;
    }
    else if (SameText(Name, L"DownloadSize"))
    {
      Updates.Results.DownloadSize = StrToInt64Def(Line, 0);
    }
    else if (SameText(Name, L"DownloadSha256"))
    {
      Updates.Results.DownloadSha256 = Line;
    }
    else if (SameText(Name, L"AuthenticationError"))
    {
      Changed |= (PrevResults.AuthenticationError != Line);
      Updates.Results.AuthenticationError = Line;
    }
    else if (SameText(Name, L"OpenGettingStarted"))
    {
      Updates.Results.OpenGettingStarted = (StrToIntDef(Line, 0) != 0);
    }
    else if (SameText(Name, L"DownloadingUrl"))
    {
      Updates.Results.DownloadingUrl = Line;
    }
    else if (SameText(Name, L"TipsSize"))
    {
      TSize TipsSize;
      TipsSize.Width = StrToIntDef(CutToChar(Line, L',', true), 0);
      TipsSize.Height = StrToIntDef(CutToChar(Line, L',', true), 0);
      Updates.Results.TipsSize = TipsSize;
    }
    else if (SameText(Name, L"TipsUrl"))
    {
      Updates.Results.TipsUrl = Line;
    }
    else if (SameText(Name, L"Tips"))
    {
      Updates.Results.Tips = Line;
    }
    else if (SameText(Name, L"TipsIntervalDays"))
    {
      int TipsIntervalDays = StrToIntDef(Line, Updates.Results.TipsIntervalDays);
      if (TipsIntervalDays < 0)
      {
        TipsIntervalDays = Updates.Results.TipsIntervalDays;
      }
      Updates.Results.TipsIntervalDays = TipsIntervalDays;
    }
    else if (SameText(Name, L"TipsIntervalRuns"))
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
    UnicodeString Message;
    if (DebugAlwaysTrue(ExceptionFullMessage(&E, Message)))
    {
      Configuration->Usage->Set(LastUpdateExceptionCounter, Message);
    }
    throw ExtException(&E, MainInstructions(LoadStr(CHECK_FOR_UPDATES_ERROR)));
  }
}
//---------------------------------------------------------------------------
void FormatUpdatesMessage(
  UnicodeString & UpdatesMessage, const UnicodeString & AMessage, const TUpdatesConfiguration & Updates)
{
  UnicodeString Message = AMessage;
  Message = ReplaceStr(Message, "%UPDATE_UNAUTHORIZED%", LoadStr(UPDATE_UNAUTHORIZED));
  Message = ReplaceStr(Message, "%UPDATE_EXPIRED%", LoadStr(UPDATE_EXPIRED));
  Message = ReplaceStr(Message, "%UPDATE_TOO_MANY%", LoadStr(UPDATE_TOO_MANY));
  UnicodeString Buf = LoadStr(UPDATE_MISSING_ADDRESS2);
  if (!Updates.AuthenticationEmail.IsEmpty())
  {
    Buf += L"\n\n" + FMTLOAD(UPDATE_MISSING_ADDRESS3, (Updates.AuthenticationEmail, L"WinSCP donation receipt"));
  }
  Message = ReplaceStr(Message, "%UPDATE_MISSING_ADDRESS%", Buf);
  Message = ReplaceStr(Message, "%UPDATE_TOO_LOW%", LoadStr(UPDATE_TOO_LOW));
  Message = ReplaceStr(Message, L"|", L"\n");

  if (!Message.IsEmpty())
  {
    UpdatesMessage = TrimRight(UpdatesMessage);
    if (!UpdatesMessage.IsEmpty())
    {
      UpdatesMessage += L"\n \n";
    }
    UpdatesMessage += Message;
  }
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
      if (IsUWP())
      {
        New = false;
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
    }

    if (!Message.IsEmpty())
    {
      Message = MainInstructions(Message);
    }

    if (!Updates.Results.Message.IsEmpty())
    {
      FormatUpdatesMessage(Message, Updates.Results.Message, Updates);
    }

    if (!Updates.Results.AuthenticationError.IsEmpty() && !IsUWP())
    {
      FormatUpdatesMessage(Message, Updates.Results.AuthenticationError, Updates);
    }
    Type = (Updates.Results.Critical ? qtWarning : qtInformation);
  }
  else
  {
    New = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall EnableAutomaticUpdates()
{
  ShowHelp(HELP_AUTOMATIC_UPDATE);
}
//---------------------------------------------------------------------------
static void __fastcall OpenHistory(void * /*Data*/, TObject * /*Sender*/, unsigned int & /*Answer*/)
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
static UnicodeString GetInstallationPath(HKEY RootKey)
{
  std::unique_ptr<TRegistry> Registry(new TRegistry(KEY_READ));
  Registry->RootKey = RootKey;
  UnicodeString Result;
  if (Registry->OpenKey(L"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\winscp3_is1", false))
  {
    Result = ExcludeTrailingBackslash(Registry->ReadString(L"Inno Setup: App Path"));
  }
  return Result;
}
//---------------------------------------------------------------------------
static bool DoIsPathToExe(const UnicodeString & Path)
{
  UnicodeString ExePath = ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName));
  return IsPathToSameFile(ExePath, Path);
}
//---------------------------------------------------------------------------
static bool DoIsInstalled(HKEY RootKey)
{
  UnicodeString InstallPath = GetInstallationPath(RootKey);
  bool Result =
    !InstallPath.IsEmpty() &&
    DoIsPathToExe(InstallPath);
  return Result;
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

  UnicodeString FileName = ExtractFileNameFromUrl(FUpdates.Results.DownloadUrl);
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
  if (ApplicationLog->Logging)
  {
    Params += FORMAT(" /LOG=\"%s\"", (ApplicationLog->Path + L".setup"));
  }
  // This condition is not necessary, it's here to reduce an impact of the change only
  if (!GetInstallationPath(HKEY_LOCAL_MACHINE).IsEmpty() &&
      !GetInstallationPath(HKEY_CURRENT_USER).IsEmpty())
  {
    UnicodeString Mode;
    if (DoIsInstalled(HKEY_LOCAL_MACHINE))
    {
      Mode = L" /ALLUSERS";
    }
    else if (DebugAlwaysTrue(DoIsInstalled(HKEY_CURRENT_USER)))
    {
      Mode = L" /CURRENTUSER";
    }
    AppLogFmt(L"Both administrative and non-administrative installation found, explicitly requesting this installation mode:%s", (Mode));
    Params += Mode;
  }

  ExecuteShellChecked(SetupPath, Params);

  Configuration->Usage->Inc(L"UpdateRuns");
  AppLog(L"Terminating to allow installation...");
  TerminateApplication();
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
static void __fastcall DownloadUpdate(void * /*Data*/, TObject * Sender, unsigned int & /*Answer*/)
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
  const UnicodeString DonatePanelName = L"DonatePanel";
  TForm * Dialog = DebugNotNull(dynamic_cast<TForm *>(Sender));
  // OnShow can happen multiple times, for example when showing dialog on start up (being main window)
  if (FindComponentRecursively(Dialog, DonatePanelName) == NULL)
  {
    UnicodeString DocumentBody = LoadStr(UPDATES_DONATE_HTML);
    DocumentBody = ReplaceStr(DocumentBody, L"%DONATE_URL%", AppendUrlParams(LoadStr(DONATE_URL), L"automaticupdates=1"));
    UnicodeString AboutStoreUrl = LoadStr(ABOUT_STORE_URL);
    DocumentBody = ReplaceStr(DocumentBody, L"%STORE_URL%", AboutStoreUrl);
    UnicodeString StoreButtonUrl = ProgramUrl(LoadStr(STORE_GET_IMG_URL));
    StoreButtonUrl = HTMLEscape(StoreButtonUrl);
    UnicodeString StoreButton =
      FORMAT(L"<img src=\"%s\" style=\"height: 1.8em; vertical-align: -0.4em; padding-top: 0.2em; border: 0;\">", (StoreButtonUrl));
    UnicodeString StoreUrl = FMTLOAD(STORE_URL, (L"update"));
    UnicodeString StoreLink = FORMAT(L"<a href=\"%s\">%s</a>", (StoreUrl, StoreButton));

    UnicodeString PlainBody = TNetEncoding::HTML->Decode(DocumentBody);
    int P1, P2;
    while (((P1 = PlainBody.Pos(L"<")) > 0) && ((P2 = PlainBody.Pos(L">")) > 0) && (P1 < P2))
    {
      PlainBody.Delete(P1, P2 - P1 + 1);
    }
    while ((P1 = PlainBody.Pos(L"  ")) > 0)
    {
      PlainBody.Delete(P1, 1);
    }

    DocumentBody = ReplaceStr(DocumentBody, L"%GET_IMG% ", FORMAT(L"%s&nbsp;", (StoreLink)));
    DocumentBody = FORMAT(L"<p>%s</p>", (DocumentBody));

    TPanel * Panel = CreateBlankPanel(Dialog);
    Panel->Name = DonatePanelName;
    Panel->Caption = UnicodeString(); // override default use of Name

    TWebBrowserEx * DonateBrowser = CreateBrowserViewer(Panel, UnicodeString());
    ReadyBrowserForStreaming(DonateBrowser);
    WaitBrowserToIdle(DonateBrowser);

    int Height = 36;
    int TextWidth = Dialog->Canvas->TextWidth(PlainBody);
    int ContentLimit = (GetMessageDialogContentWidth(Dialog) * 5 / 3);
    if (TextWidth > ContentLimit)
    {
      Height = Height * 3 / 2;
    }
    DonateBrowser->Height = ScaleByTextHeight(Dialog, Height);

    DonateBrowser->Top = 0;
    DonateBrowser->Left = 0;

    Panel->Height = DonateBrowser->Height;

    // Currently this is noop (will fail assertion), if MoreMessagesUrl is not set
    // (what should not happen)
    InsertPanelToMessageDialog(Dialog, Panel);

    UnicodeString Document = GenerateAppHtmlPage(Dialog->Font, Panel, DocumentBody, true);
    LoadBrowserDocument(DonateBrowser, Document);
    HideBrowserScrollbars(DonateBrowser);
  }
}
//---------------------------------------------------------------------------
bool __fastcall CheckForUpdates(bool CachedResults)
{
  bool Result = false;
  TOperationVisualizer Visualizer;

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

  if ((double(Updates.Period) > 0) &&
      // do not show next check time, if we have new version info
      !New)
  {
    Message += L"\n\n" +
      FMTLOAD(UPDATE_NEXT, (FormatDateTime("ddddd", Updates.LastCheck + Updates.Period)));
  }
  else if (New)
  {
    UnicodeString Version = Configuration->GetVersionStrHuman();
    Message += L"\n\n" + FMTLOAD(UPDATE_CURRENT, (Version));
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
  Aliases[1].OnSubmit = MakeMethod<TButtonSubmitEvent>(NULL, OpenHistory);
  Aliases[2].Button = qaCancel;
  Aliases[2].Alias = Vcl_Consts_SMsgDlgClose;
  // Used only when New == true, see AliasesCount below
  Aliases[3].Button = qaOK;
  Aliases[3].Alias = LoadStr(UPGRADE_BUTTON);
  if (!Updates.Results.DownloadUrl.IsEmpty())
  {
    Aliases[3].OnSubmit = MakeMethod<TButtonSubmitEvent>(NULL, DownloadUpdate);
    Aliases[3].ElevationRequired = DoIsInstalled(HKEY_LOCAL_MACHINE);
  }

  TMessageParams Params;
  Params.Aliases = Aliases;
  Params.MoreMessagesUrl = Updates.Results.NewsUrl;
  Params.MoreMessagesSize = Updates.Results.NewsSize;
  // alias "ok" button to "upgrade" only if we have new version
  Params.AliasesCount = LENOF(Aliases) - (New ? 0 : 1);
  Params.CustomCaption = LoadStr(CHECK_FOR_UPDATES_TITLE);

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
  try
  {
    WinConfiguration->UpdateJumpList();
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }
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
        if (SUCCEEDED(Link->GetDescription(Desc, LENOF(Desc) - 1)))
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
  IObjectArray * RemovedArray;
  TStringList * Removed = NULL;
  int OldErrMode = SetErrorMode(SEM_FAILCRITICALERRORS);

  try
  {
    if (SUCCEEDED(CoCreateInstance(CLSID_DestinationList, NULL,
          CLSCTX_INPROC_SERVER, IID_ICustomDestinationList, (void**)&DestinationList)))
    {

      unsigned int MinSlots;
      HRESULT Result = DestinationListBeginList(DestinationList, MinSlots, IID_IObjectArray, reinterpret_cast<void *>(RemovedArray), 50000);
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
              SUCCEEDED(Link->GetDescription(Desc, LENOF(Desc) - 1)))
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
bool IsInstalled()
{
  return
    DoIsInstalled(HKEY_LOCAL_MACHINE) ||
    DoIsInstalled(HKEY_CURRENT_USER);
}
//---------------------------------------------------------------------------
static int GIsInstalledMsi = -1;
bool IsInstalledMsi()
{
  if (GIsInstalledMsi < 0)
  {
    GIsInstalledMsi = 0;
    wchar_t ProductCode[MAX_GUID_CHARS + 1];
    if (MsiEnumRelatedProducts(L"{029F9450-CFEF-4408-A2BB-B69ECE29EB18}", 0, 0, ProductCode) == ERROR_SUCCESS)
    {
      UnicodeString InstallPath;
      InstallPath.SetLength(MAX_PATH);
      unsigned long Size = InstallPath.Length() + 1;
      int ErrorCode = MsiGetProductInfo(ProductCode, INSTALLPROPERTY_INSTALLLOCATION, InstallPath.c_str(), &Size);
      if (ErrorCode == ERROR_MORE_DATA)
      {
        InstallPath.SetLength(Size);
        Size++;
        ErrorCode = MsiGetProductInfo(ProductCode, INSTALLPROPERTY_INSTALLLOCATION, InstallPath.c_str(), &Size);
      }
      if (ErrorCode == ERROR_SUCCESS)
      {
        InstallPath.SetLength(Size);
        if (DoIsPathToExe(InstallPath))
        {
          GIsInstalledMsi = 1;
        }
      }
    }
  }
  return (GIsInstalledMsi > 0);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FirstUnshownTip()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  std::unique_ptr<TStringList> Tips(CommaTextToStringList(Updates.Results.Tips));
  Tips->CaseSensitive = false;
  std::unique_ptr<TStringList> TipsSeen(CommaTextToStringList(WinConfiguration->TipsSeen));
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

  TButton * PrevButton = DebugNotNull(dynamic_cast<TButton *>(Form->FindComponent(YesButtonName)));
  PrevButton->Enabled = (TipsData->Index > 0);
  TButton * NextButton = DebugNotNull(dynamic_cast<TButton *>(Form->FindComponent(L"No")));
  NextButton->Enabled = (TipsData->Index < TipsData->Tips->Count - 1);

  TLabel * MessageLabel = DebugNotNull(dynamic_cast<TLabel *>(FindComponentRecursively(Form, MainMessageLabelName)));
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
  std::unique_ptr<TStringList> TipsSeen(CommaTextToStringList(WinConfiguration->TipsSeen));
  TipsSeen->Values[Tip] = FormatDateTime(L"yyyy-mm-dd", Now());
  WinConfiguration->TipsSeen = TipsSeen->CommaText;
  WinConfiguration->TipsShown = Now();
  WinConfiguration->RunsSinceLastTip = 0;
  // prevent parallel app instances showing the same tip
  WinConfiguration->Save();
}
//---------------------------------------------------------------------------
static void __fastcall PrevNextTipClick(void * Data, TObject * Sender, unsigned int & /*Answer*/)
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
  std::unique_ptr<TStringList> Tips(CommaTextToStringList(Updates.Results.Tips));
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
  Aliases[0].OnSubmit = MakeMethod<TButtonSubmitEvent>(reinterpret_cast<void *>(-1), PrevNextTipClick);
  Aliases[1].Button = qaNo;
  Aliases[1].Alias = LoadStr(NEXT_BUTTON);
  Aliases[1].OnSubmit = MakeMethod<TButtonSubmitEvent>(reinterpret_cast<void *>(+1), PrevNextTipClick);
  Aliases[2].Button = qaCancel;
  Aliases[2].Alias = LoadStr(CLOSE_BUTTON);

  TMessageParams Params;
  Params.CustomCaption = LoadStr(TIPS_TITLE);
  Params.MoreMessagesSize = Updates.Results.TipsSize;
  Params.MoreMessagesUrl = TipUrl(TipsData.get());
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);
  Params.ImageName = L"Bulb On";

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
bool __fastcall AnyTips()
{
  return !WinConfiguration->Updates.Results.Tips.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall ShowTips()
{
  {
    TOperationVisualizer Visualizer;
    DoQueryUpdates(false);
  }

  if (!AnyTips())
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
  std::unique_ptr<TStringList> Tips(CommaTextToStringList(Updates.Results.Tips));
  Configuration->Usage->Set(L"TipsCount", Tips->Count);
  std::unique_ptr<TStringList> TipsSeen(CommaTextToStringList(WinConfiguration->TipsSeen));
  Configuration->Usage->Set(L"TipsSeen", TipsSeen->Count);
}
//---------------------------------------------------------------------------
static void ReadNetVersion(TRegistryStorage * Registry)
{
  UnicodeString VersionStr = Registry->ReadString(L"Version", L"");
  if (CompareVersion(VersionStr, NetVersionStr) > 0)
  {
    NetVersionStr = VersionStr;
  }
}
//---------------------------------------------------------------------------
UnicodeString GetNetVersionStr()
{
  if (NetVersionStr.IsEmpty())
  {
    NetVersionStr = L"0"; // not to retry on failure

    std::unique_ptr<TRegistryStorage> Registry(new TRegistryStorage(L"SOFTWARE\\Microsoft\\NET Framework Setup\\NDP", HKEY_LOCAL_MACHINE));
    if (Registry->OpenRootKey(false))
    {
      std::unique_ptr<TStringList> Keys(new TStringList());
      Registry->GetSubKeyNames(Keys.get());
      for (int Index = 0; Index < Keys->Count; Index++)
      {
        UnicodeString Key = Keys->Strings[Index];
        if (Registry->OpenSubKey(Key, false))
        {
          ReadNetVersion(Registry.get());

          if (Registry->OpenSubKey(L"Full", false))
          {
            ReadNetVersion(Registry.get());
            Registry->CloseSubKey();
          }
          if (Registry->OpenSubKey(L"Client", false))
          {
            ReadNetVersion(Registry.get());
            Registry->CloseSubKey();
          }

          Registry->CloseSubKey();
        }
      }
    }
  }

  return NetVersionStr;
}
//---------------------------------------------------------------------------
UnicodeString GetNetCoreVersionStr()
{
  if (NetCoreVersionStr.IsEmpty())
  {
    NetCoreVersionStr = L"0"; // not to retry on failure

    UnicodeString ProgramsFolder = DefaultStr(GetEnvironmentVariable(L"ProgramW6432"), GetEnvironmentVariable(L"ProgramFiles"));
    if (ProgramsFolder.IsEmpty())
    {
      ::SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
    }
    UnicodeString RuntimeFolder = L"shared\\Microsoft.NETCore.App";
    UnicodeString DotNetPath = CombinePaths(CombinePaths(ProgramsFolder, L"dotnet"), RuntimeFolder);
    if (!DirectoryExistsFix(DotNetPath))
    {
      UnicodeString DotNetExe = L"dotnet.exe";
      if (FindFile(DotNetExe))
      {
        DotNetPath = CombinePaths(ExtractFilePath(DotNetExe), RuntimeFolder);
      }
    }
    if (DirectoryExistsFix(DotNetPath))
    {
      TSearchRecChecked SearchRec;
      DotNetPath = CombinePaths(DotNetPath, L"*.*");
      if (FindFirstUnchecked(ApiPath(DotNetPath), faDirectory, SearchRec) == 0)
      {
        do
        {
          if (SearchRec.IsRealFile())
          {
            UnicodeString Name = SearchRec.Name;
            // 1.0.0-preview2-003131
            UnicodeString VersionStr = CutToChar(Name, L'-', true);
            if (!VersionStr.IsEmpty() && IsDigit(VersionStr[1]) && (VersionStr.Pos(L".") >= 2))
            {
              for (int I = 1; I <= VersionStr.Length(); I++)
              {
                if (!IsDigit(VersionStr[I]) && (VersionStr[I] != L'.'))
                {
                  VersionStr = EmptyStr;
                }
              }

              if (!VersionStr.IsEmpty() && (CompareVersion(VersionStr, NetCoreVersionStr) > 0))
              {
                NetCoreVersionStr = VersionStr;
              }
            }
          }
        }
        while (FindNextUnchecked(SearchRec) == 0);
      }
    }
  }

  return NetCoreVersionStr;
}
//---------------------------------------------------------------------------
UnicodeString GetPowerShellVersionStr()
{
  if (PowerShellVersionStr.IsEmpty())
  {
    PowerShellVersionStr = L"0"; // not to retry on failure

    std::unique_ptr<TRegistryStorage> Registry(new TRegistryStorage(L"SOFTWARE\\Microsoft\\PowerShell", HKEY_LOCAL_MACHINE));
    if (Registry->OpenRootKey(false))
    {
      std::unique_ptr<TStringList> Keys(new TStringList());
      Registry->GetSubKeyNames(Keys.get());
      for (int Index = 0; Index < Keys->Count; Index++)
      {
        UnicodeString Key = Keys->Strings[Index];
        if (Registry->OpenSubKeyPath(Key + L"\\PowerShellEngine", false))
        {
          UnicodeString VersionStr = Registry->ReadString(L"PowerShellVersion", L"");
          if (!VersionStr.IsEmpty() && (CompareVersion(VersionStr, PowerShellVersionStr) > 0))
          {
            PowerShellVersionStr = VersionStr;
          }

          Registry->CloseSubKeyPath();
        }
      }
    }
  }

  return PowerShellVersionStr;
}
//---------------------------------------------------------------------------
UnicodeString GetPowerShellCoreVersionStr()
{
  if (PowerShellCoreVersionStr.IsEmpty())
  {
    PowerShellCoreVersionStr = L"0"; // not to retry on failure

    // TRegistryStorage does not support KEY_WOW64_64KEY
    unsigned int Access = KEY_READ | FLAGMASK(IsWin64(), KEY_WOW64_64KEY);
    std::unique_ptr<TRegistry> Registry(new TRegistry(Access));
    Registry->RootKey = HKEY_LOCAL_MACHINE;
    UnicodeString RootKey(L"SOFTWARE\\Microsoft\\PowerShellCore\\InstalledVersions");
    if (Registry->OpenKeyReadOnly(RootKey))
    {
      std::unique_ptr<TStringList> Keys(new TStringList());
      Registry->GetKeyNames(Keys.get());
      Registry->CloseKey();
      for (int Index = 0; Index < Keys->Count; Index++)
      {
        UnicodeString Key = RootKey + L"\\" + Keys->Strings[Index];
        if (Registry->OpenKeyReadOnly(Key))
        {
          UnicodeString VersionStr = Registry->ReadString(L"SemanticVersion");
          if (!VersionStr.IsEmpty() && (CompareVersion(VersionStr, PowerShellCoreVersionStr) > 0))
          {
            PowerShellCoreVersionStr = VersionStr;
          }
          Registry->CloseKey();
        }
      }
    }
  }

  return PowerShellCoreVersionStr;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static void CollectCLSIDKey(
  TConsole * Console, TStrings * Keys, int PlatformSet, TRegistryStorage * Storage, const UnicodeString & CLSID,
  UnicodeString & CommonCodeBase, const UnicodeString & Platform, UnicodeString & Platforms)
{
  UnicodeString CLSIDKey = FORMAT(L"CLSID\\%s", (CLSID));
  if (Storage->OpenSubKeyPath(CLSIDKey, false))
  {
    int Index = Keys->IndexOf(CLSIDKey);
    if (Index >= 0)
    {
      Keys->Objects[Index] = reinterpret_cast<TObject *>(PlatformSet | reinterpret_cast<int>(Keys->Objects[Index]));
    }
    else
    {
      Keys->AddObject(CLSIDKey, reinterpret_cast<TObject *>(PlatformSet));
    }
    UnicodeString CodeBase;
    if (Storage->OpenSubKey(L"InprocServer32", false))
    {
      CodeBase = Storage->ReadString(L"CodeBase", UnicodeString());
      UnicodeString Assembly = Storage->ReadString(L"Assembly", UnicodeString());
      UnicodeString Version;
      if (!Assembly.IsEmpty())
      {
        UnicodeString VersionPrefix = L"Version=";
        int P = Assembly.UpperCase().Pos(VersionPrefix.UpperCase());
        if (P > 0)
        {
          Assembly.Delete(1, P + VersionPrefix.Length() - 1);
          Version = CutToChar(Assembly, L',', true);
        }
      }
      if (CodeBase.IsEmpty() || Version.IsEmpty())
      {
        Console->PrintLine(FORMAT(L"Warning: Could not find codebase and version for %s.", (CLSID)));
        CodeBase = UnicodeString();
      }
      else
      {
        CodeBase = FORMAT(L"%s (%s)", (CodeBase, Version));
        if (CommonCodeBase.IsEmpty())
        {
          Console->PrintLine(FORMAT(L"Codebase %s, unless stated otherwise", (CodeBase)));
          CommonCodeBase = CodeBase;
        }
        if (SameText(CommonCodeBase, CodeBase))
        {
          CodeBase = L"";
        }
      }
      Storage->CloseSubKey();
    }
    Storage->CloseSubKeyPath();

    UnicodeString Buf = Platform;
    AddToList(Buf, CodeBase, L" - ");
    AddToList(Platforms, Buf, ", ");
  }
}
//---------------------------------------------------------------------------
static UnicodeString PlatformStr(int PlatformSet)
{
  UnicodeString Result;
  if (PlatformSet == 0)
  {
    Result = L"shared";
  }
  else
  {
    if (FLAGSET(PlatformSet, 32))
    {
      Result = L"32-bit";
    }
    if (FLAGSET(PlatformSet, 64))
    {
      AddToList(Result, L"64-bit", L", ");
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static void DoCollectComRegistration(TConsole * Console, TStrings * Keys)
{
  UnicodeString TypeLib = L"{A0B93468-D98A-4845-A234-8076229AD93F}"; // Duplicated in AssemblyInfo.cs
  std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(UnicodeString(), HKEY_CLASSES_ROOT));
  Storage->MungeStringValues = false;
  Storage->AccessMode = smRead;
  std::unique_ptr<TRegistryStorage> Storage64;
  if (IsWin64())
  {
    Storage64.reset(new TRegistryStorage(UnicodeString(), HKEY_CLASSES_ROOT, KEY_WOW64_64KEY));
    Storage64->MungeStringValues = false;
    Storage64->AccessMode = smRead;
  }

  // Classes, TypeLib and Record are shared between 32-bit and 64-bit registry view.
  // 32-bit and 64-bit version of regasm adds CLSID keys to its respective view.
  // Both 32-bit and 64-bit version of regasm seem to add Interface keys to both 32-bit and 64-bit views.
  // On Vista, Interface keys are reflected (so when 32-bit keys is deleted, it's also deleted from 64-bit key,
  // and we show an error, trying to delete the 64-bit key).

  if (Storage->OpenRootKey(false))
  {
    Console->PrintLine(FORMAT(L"Versions of type library %s:", (TypeLib)));
    UnicodeString TypeLibKey = FORMAT(L"TypeLib\\%s", (TypeLib));
    if (Storage->OpenSubKeyPath(TypeLibKey, false))
    {
      Keys->Add(TypeLibKey);
      std::unique_ptr<TStringList> KeyNames(new TStringList());
      Storage->GetSubKeyNames(KeyNames.get());
      if (KeyNames->Count == 0)
      {
        Console->PrintLine(L"Warning: The type library key exists, but no type libraries are present.");
      }
      else
      {
        for (int Index = 0; Index < KeyNames->Count; Index++)
        {
          UnicodeString Version = KeyNames->Strings[Index];
          if (!Storage->OpenSubKeyPath(FORMAT(L"%s\\0", (Version)), false))
          {
            Console->PrintLine(FORMAT(L"Warning: Subkey \"0\" for type library \"%s\" cannot be opened.", (Version)));
          }
          else
          {
            std::unique_ptr<TStringList> Platforms(new TStringList());
            Storage->GetSubKeyNames(Platforms.get());
            if (Platforms->Count == 0)
            {
              Console->PrintLine(FORMAT(L"Warning: Subkey \"0\" for type library \"%s\" exists, but platforms are present.", (Version)));
            }
            else
            {
              for (int Index2 = 0; Index2 < Platforms->Count; Index2++)
              {
                UnicodeString Platform = Platforms->Strings[Index2];
                if (!Storage->OpenSubKey(Platform, false))
                {
                  Console->PrintLine(FORMAT(L"Warning: Platform \"%s\" for type library \"%s\" cannot be opened.", (Platform, Version)));
                }
                else
                {
                  UnicodeString TypeLibraryPath = Storage->ReadString(UnicodeString(), UnicodeString());
                  UnicodeString Exists = FileExists(TypeLibraryPath) ? L"exists" : L"does not exist";
                  Console->PrintLine(FORMAT(L"%s (%s): %s (%s)", (Version, Platform, TypeLibraryPath, Exists)));
                  Storage->CloseSubKey();
                }
              }
            }
            Storage->CloseSubKeyPath();
          }
        }
      }
      Storage->CloseSubKeyPath();
    }
    else
    {
      Console->PrintLine(L"Type library not registered.");
    }
    Console->PrintLine();

    std::unique_ptr<TStringList> KeyNames(new TStringList());
    Storage->GetSubKeyNames(KeyNames.get());
    UnicodeString NamespacePrefix = L"WinSCP.";
    Console->PrintLine(L"Classes:");
    UnicodeString CommonCodeBase;
    int Found = 0;
    for (int Index = 0; Index < KeyNames->Count; Index++)
    {
      UnicodeString KeyName = KeyNames->Strings[Index];
      if (StartsText(NamespacePrefix, KeyName))
      {
        if (Storage->OpenSubKeyPath(FORMAT(L"%s\\%s", (KeyName, L"CLSID")), false))
        {
          UnicodeString Class = KeyName;
          UnicodeString CLSID = Trim(Storage->ReadString(UnicodeString(), UnicodeString()));
          Storage->CloseSubKeyPath();

          if (!CLSID.IsEmpty())
          {
            Keys->Add(KeyName);
            UnicodeString Platforms;
            CollectCLSIDKey(Console, Keys, 32, Storage.get(), CLSID, CommonCodeBase, L"32-bit", Platforms);
            if (Storage64.get() != NULL)
            {
              CollectCLSIDKey(Console, Keys, 64, Storage64.get(), CLSID, CommonCodeBase, L"64-bit", Platforms);
            }

            UnicodeString Line = FORMAT(L"%s - %s", (Class, CLSID));

            if (Platforms.IsEmpty())
            {
              Console->PrintLine(FORMAT(L"Warning: Could not find CLSID %s for class %s.", (CLSID, Class)));
            }
            else
            {
              Line += FORMAT(L" [%s]", (Platforms));
            }

            Console->PrintLine(Line);
            Found++;
          }
        }
      }
    }
    if (Found == 0)
    {
      Console->PrintLine(L"No classes found.");
    }
    Console->PrintLine();

    UnicodeString InterfaceKey = L"Interface";
    if (Storage->OpenSubKey(InterfaceKey, false) &&
        ((Storage64.get() == NULL) || Storage64->OpenSubKey(InterfaceKey, false)))
    {
      Console->PrintLine(L"Interfaces:");
      std::unique_ptr<TStringList> KeyNames(new TStringList());
      Storage->GetSubKeyNames(KeyNames.get());
      KeyNames->Sorted = true;
      for (int Index = 0; Index < KeyNames->Count; Index++)
      {
        KeyNames->Objects[Index] = reinterpret_cast<TObject *>(32);
      }
      if (Storage64.get() != NULL)
      {
        std::unique_ptr<TStringList> KeyNames64(new TStringList());
        Storage64->GetSubKeyNames(KeyNames64.get());
        for (int Index = 0; Index < KeyNames64->Count; Index++)
        {
          UnicodeString Key64 = KeyNames64->Strings[Index];
          int Index32 = KeyNames->IndexOf(Key64);
          if (Index32 >= 0)
          {
            KeyNames->Objects[Index32] = reinterpret_cast<TObject *>(32 | 64);
          }
          else
          {
            KeyNames->AddObject(Key64, reinterpret_cast<TObject *>(64));
          }
        }
      }
      int Found = 0;
      for (int Index = 0; Index < KeyNames->Count; Index++)
      {
        UnicodeString KeyName = KeyNames->Strings[Index];
        // Open sub key first, to check if we are interested in the interface, as an optimization
        int PlatformSet = reinterpret_cast<int>(KeyNames->Objects[Index]);
        THierarchicalStorage * KeyStorage = FLAGSET(PlatformSet, 32) ? Storage.get() : Storage64.get();
        if (KeyStorage->OpenSubKeyPath(FORMAT(L"%s\\TypeLib", (KeyName)), false))
        {
          UnicodeString InterfaceTypeLib = KeyStorage->ReadString(UnicodeString(), UnicodeString());
          UnicodeString Version = KeyStorage->ReadString(L"Version", UnicodeString());
          KeyStorage->CloseSubKeyPath();
          if (SameText(InterfaceTypeLib, TypeLib))
          {
            if (KeyStorage->OpenSubKey(KeyName, false))
            {
              UnicodeString Key = ExcludeTrailingBackslash(KeyStorage->CurrentSubKey);
              UnicodeString Interface = KeyStorage->ReadString(UnicodeString(), UnicodeString());
              KeyStorage->CloseSubKey();
              Keys->AddObject(Key, reinterpret_cast<TObject *>(PlatformSet));
              Console->PrintLine(FORMAT(L"%s - %s (%s) [%s]", (Interface, KeyName, Version, PlatformStr(PlatformSet))));
              Found++;
            }
          }
        }
      }
      if (Found == 0)
      {
        Console->PrintLine(L"No interfaces found.");
      }
      Storage->CloseSubKey();
      Console->PrintLine();
    }

    if (Storage->OpenSubKey(L"Record", false))
    {
      Console->PrintLine(L"Value types:");
      std::unique_ptr<TStringList> KeyNames(new TStringList());
      Storage->GetSubKeyNames(KeyNames.get());
      int Found = 0;
      for (int Index = 0; Index < KeyNames->Count; Index++)
      {
        UnicodeString KeyName = KeyNames->Strings[Index];
        if (Storage->OpenSubKey(KeyName, false))
        {
          std::unique_ptr<TStringList> Versions(new TStringList());
          Storage->GetSubKeyNames(Versions.get());
          UnicodeString VersionsStr;
          std::unique_ptr<TStringList> Classes(CreateSortedStringList());
          for (int Index2 = 0; Index2 < Versions->Count; Index2++)
          {
            UnicodeString Version = Versions->Strings[Index2];
            if (Storage->OpenSubKey(Version, false))
            {
              UnicodeString Class = Storage->ReadString(L"Class", UnicodeString());
              Classes->Add(Class);
              if (StartsStr(NamespacePrefix, Class))
              {
                AddToList(VersionsStr, Version, L", ");
              }
              Storage->CloseSubKey();
            }
          }
          if (!VersionsStr.IsEmpty())
          {
            if (Classes->Count != 1)
            {
              Console->PrintLine(FORMAT(L"Warning: Different class names for the same value type %s: %s", (KeyName, Classes->CommaText)));
            }
            else
            {
              Keys->Add(ExcludeTrailingBackslash(Storage->CurrentSubKey));
              Console->PrintLine(FORMAT(L"%s - %s (%s)", (Classes->Strings[0], KeyName, VersionsStr)));
              Found++;
            }
          }
          Storage->CloseSubKey();
        }
      }
      if (Found == 0)
      {
        Console->PrintLine(L"No value types found.");
      }
      Storage->CloseSubKey();
      Console->PrintLine();
    }
  }
}
//---------------------------------------------------------------------------
bool DoUnregisterChoice(TConsole * Console)
{
  return (Console->Choice(L"U", -1, -1, -1, 0, 0, 0, UnicodeString()) == 1);
}
//---------------------------------------------------------------------------
typedef HRESULT WINAPI (* RegDeleteTreeProc)(HKEY Key, LPCWSTR SubKey);
static RegDeleteTreeProc ARegDeleteTree = NULL;
//---------------------------------------------------------------------------
void DoDeleteKey(TConsole * Console, TRegistry * Registry, const UnicodeString & Key, int Platform, bool & AnyDeleted, bool & AllDeleted)
{
  UnicodeString ParentKey = ExtractFileDir(Key);
  UnicodeString ChildKey = ExtractFileName(Key);
  bool Result = Registry->OpenKey(ParentKey, false);
  if (Result)
  {
    if (DebugAlwaysFalse(ARegDeleteTree == NULL))
    {
      Result = false;
    }
    else
    {
      Result = (ARegDeleteTree(Registry->CurrentKey, ChildKey.c_str()) == 0);
    }
    Registry->CloseKey();
  }

  UnicodeString Status;
  if (Result)
  {
    Status = L"removed";
    AnyDeleted = true;
  }
  else
  {
    AllDeleted = false;
    Status = L"NOT removed";
  }
  Console->PrintLine(FORMAT(L"%s [%s] - %s", (Key, PlatformStr(Platform), Status)));
}
//---------------------------------------------------------------------------
int ComRegistration(TConsole * Console)
{
  int Result = RESULT_SUCCESS;
  try
  {
    if (ARegDeleteTree == NULL)
    {
      HINSTANCE AdvapiLibrary = LoadLibrary(L"advapi32.dll");
      if (DebugAlwaysTrue(AdvapiLibrary != NULL))
      {
        ARegDeleteTree = reinterpret_cast<RegDeleteTreeProc>(GetProcAddress(AdvapiLibrary, "RegDeleteTreeW"));
      }
    }

    if (ARegDeleteTree == NULL)
    {
      throw Exception(FORMAT(L"Not supported on %s", (GetOSInfo())));
    }

    Console->PrintLine(GetEnvironmentInfo());
    Console->PrintLine();

    std::unique_ptr<TStrings> Keys(new TStringList());
    DoCollectComRegistration(Console, Keys.get());

    if (Keys->Count == 0)
    {
      Console->PrintLine(L"No registration found.");
      Console->WaitBeforeExit();
    }
    else
    {
      Console->PrintLine(L"Press (U) to unregister or Esc to exit...");
      if (DoUnregisterChoice(Console))
      {
        Console->PrintLine();
        Console->PrintLine(L"The following registry keys will be removed from HKCR registry hive:");
        for (int Index = 0; Index < Keys->Count; Index++)
        {
          Console->PrintLine(FORMAT(L"%s [%s]", (Keys->Strings[Index], PlatformStr(reinterpret_cast<int>(Keys->Objects[Index])))));
        }
        Console->PrintLine();

        Console->PrintLine(L"You need Administrator privileges to remove the keys.");
        Console->PrintLine(L"Press (U) again to proceed with unregistration or Esc to abort...");
        if (DoUnregisterChoice(Console))
        {
          std::unique_ptr<TRegistry> Registry64(new TRegistry(KEY_READ | KEY_WRITE | KEY_WOW64_64KEY));
          Registry64->RootKey = HKEY_CLASSES_ROOT;
          std::unique_ptr<TRegistry> Registry32(new TRegistry(KEY_READ | KEY_WRITE | KEY_WOW64_32KEY));
          Registry32->RootKey = HKEY_CLASSES_ROOT;

          bool AnyDeleted = false;
          bool AllDeleted = true;
          for (int Index = 0; Index < Keys->Count; Index++)
          {
            UnicodeString Key = Keys->Strings[Index];
            int PlatformSet = reinterpret_cast<int>(Keys->Objects[Index]);
            if (PlatformSet == 0)
            {
              DoDeleteKey(Console, Registry32.get(), Key, 0, AnyDeleted, AllDeleted);
            }
            else
            {
              if (FLAGSET(PlatformSet, 32))
              {
                DoDeleteKey(Console, Registry32.get(), Key, 32, AnyDeleted, AllDeleted);
              }
              if (FLAGSET(PlatformSet, 64))
              {
                DoDeleteKey(Console, Registry64.get(), Key, 64, AnyDeleted, AllDeleted);
              }
            }
          }

          Console->PrintLine();
          if (!AnyDeleted)
          {
            Console->PrintLine(L"No keys were removed. Make sure you have Administrator privileges.");
          }
          else if (!AllDeleted)
          {
            Console->PrintLine(L"Some keys were not removed. Make sure you have Administrator privileges.");
          }
          else
          {
            Console->PrintLine(L"All keys were removed. Unregistration succeeded.");
          }
          Console->WaitBeforeExit();
        }
      }
    }
  }
  catch (Exception & E)
  {
    Result = HandleException(Console, E);
    Console->WaitBeforeExit();
  }
  return Result;
}
