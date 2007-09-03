//---------------------------------------------------------------------------
// Part of this code is
// Copyright (C) 2002-2004, Marco Barisione <marco.bari@vene.ws>
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>
#include <tchar.h>
#include <Common.h>
#include <CoreMain.h>
#include <Exceptions.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <TcpIp.hpp>
#include <CompThread.hpp>
#include "WinConfiguration.h"
#include "WinInterface.h"
#include "Tools.h"
#include "Setup.h"
//---------------------------------------------------------------------------
/* Using quotes or not should make no difference but some programs (for
   instance cygwin and msys) don't like them. */
/* #define USE_QUOTES */
//---------------------------------------------------------------------------
#define APP_NAME "WinSCP"
#define KEY _T("SYSTEM\\CurrentControlSet\\Control\\") \
            _T("Session Manager\\Environment")
#define AUTOEXEC_PATH _T("c:\\autoexec.bat")
#define AUTOEXEC_INTRO "rem ***** The following line was added by " \
                       APP_NAME " *****"
#ifdef USE_QUOTES
#  define AUTOEXEC_CMD "set PATH=%%PATH%%;\"%s\""
#else
#  define AUTOEXEC_CMD "set PATH=%%PATH%%;%s"
#endif

/* Command line options. */
AnsiString LastPathError;
//---------------------------------------------------------------------------
#define verb_out(msg) ((void)0)
#define verb_out_param(msg, param) ((void)0)
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
  LastPathError = FORMAT("%s%s", (base_err_msg, SysErrorMessage(sys_err)));
}
//---------------------------------------------------------------------------
// Works as "strcmp" but the comparison is not case sensitive.
int tcharicmp(LPCTSTR str1, LPCTSTR str2){
    for (; tolower(*str1) == tolower(*str2); ++str1, ++str2)
        if (*str1 == '\0')
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
    if (last_pos != -1 && str[0] == '"' && str[last_pos] == '"'){
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
                           (LPARAM)"Environment", SMTO_ABORTIFHUNG,
                           5000, &send_message_result);
  if (ret != ERROR_SUCCESS && GetLastError() != 0)
  {
    err_out_sys(_T("Cannot propagate the new enviroment to ")
                _T("other processes. The new value will be ")
                _T("avaible after a reboot."), GetLastError());
    SimpleErrorDialog(LastPathError);
    LastPathError = "";
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
#ifdef USE_QUOTES
            _tcscat(reg_str, _T(";\""));
#endif
            _tcscat(reg_str, path);
#ifdef USE_QUOTES
            _tcscat(reg_str, _T("\""));
#endif
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
        else
            verb_out(_T("Value already exists in the registry."));
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
        else
            verb_out(_T("Value does not exist in the registry."));
    }

    RegCloseKey(key);
    free(reg_str);
    return func_ret;
}
//---------------------------------------------------------------------------
/* Can this program run under Win9x if compiled with unicode support? */
#if !defined _UNICODE
//---------------------------------------------------------------------------
// Add "path" to "autoexec.bat". Return "TRUE" if the path has been added or
// was already in the file, "FALSE" otherwise.
BOOL add_path_autoexec(LPCTSTR long_path){
    FILE * file;
    LPTSTR path;
    size_t path_size;
    LPTSTR line;
    LPTSTR out_line;
    size_t line_size;
    size_t sz1, sz2;
    LPTSTR autoexec_intro;
    BOOL found;
    BOOL func_ret = TRUE;

    file = _tfopen(AUTOEXEC_PATH, _T("r+"));
    if (!file){
        err_out(_T("Cannot open \"autoexec.bat\"."));
        return FALSE;
    }

    path_size = _tcslen(long_path) + 1;
    path = (LPTSTR)malloc(path_size * sizeof(TCHAR));
    if (!GetShortPathName(long_path, path, path_size))
        _tcsncpy(path, long_path, path_size);
    sz1 = _tcslen(path) + _tcslen(AUTOEXEC_CMD);
    sz2 = _tcslen(AUTOEXEC_INTRO) + 2 /* '\n' and '\0'. */;
    line_size = sz1 > sz2 ? sz1 : sz2;
    line = (LPTSTR)malloc(line_size * sizeof(TCHAR));
    out_line = (LPTSTR)malloc(line_size * sizeof(TCHAR));
    _stprintf(out_line, AUTOEXEC_CMD, path);
    _tcscat(out_line, _T("\n"));
    autoexec_intro = (LPTSTR)malloc((_tcslen(AUTOEXEC_INTRO) + 2 /* '\0', '\n' */)
                            * sizeof(TCHAR));
    _tcscpy(autoexec_intro, AUTOEXEC_INTRO);
    _tcscat(autoexec_intro, _T("\n"));

    found = FALSE;
    while (!found && _fgetts(line, line_size, file)){
        if (_tcscmp(autoexec_intro, line) == 0){
            _fgetts(line, line_size, file);
            if (_tcscmp(out_line, line) == 0)
                found = TRUE;
        }
    }

    if (!found){
        if (fseek(file, 0, SEEK_END) != 0 ||
            _fputts(_T("\n"), file) == _TEOF ||
            _fputts(autoexec_intro, file) == _TEOF ||
            _fputts(out_line, file) == _TEOF)
                func_ret = FALSE;
    }
    else
        verb_out(_T("Value already exists in \"autoexec.bat\"."));

    fclose(file);
    free(path);
    free(line);
    free(out_line);
    free(autoexec_intro);

    return func_ret;
}
//---------------------------------------------------------------------------
// Removes "path" from "autoexec.bat". Return "TRUE" if the path has been
// removed or it wasn't in the file, "FALSE" otherwise.
BOOL remove_path_autoexec(LPTSTR long_path){
    FILE * file;
    LPTSTR path;
    size_t path_size;
    LPTSTR data;
    long file_size;
    LPTSTR expected_text;
    size_t expected_text_size;
    LPTSTR buff;
    size_t buff_size;
    LPTSTR begin_pos;
    LPTSTR final_part;
    size_t fread_ret;
    BOOL func_ret = TRUE;

    file = _tfopen(AUTOEXEC_PATH, _T("rb"));
    if (!file){
        err_out(_T("Cannot open \"autoexec.bat\" for reading."));
        return FALSE;
    }
    fseek(file, 0, SEEK_END);
    file_size = ftell(file);
    data = (LPTSTR)malloc(file_size + sizeof(TCHAR) /* '\0'. */);
    data[file_size / sizeof(TCHAR)] = '\0';
    fseek(file, 0, SEEK_SET);
    fread_ret = fread(data, file_size, 1, file);
    fclose(file);
    if (fread_ret != 1){
        err_out(_T("Cannot read \"autoexec.bat\"."));
        return FALSE;
    }

    path_size = _tcslen(long_path) + 1;
    path = (LPTSTR)malloc(path_size * sizeof(TCHAR));
    if (!GetShortPathName(long_path, path, path_size))
        _tcsncpy(path, long_path, path_size);
    buff_size = _tcslen(AUTOEXEC_CMD) + _tcslen(path);
    buff = (LPTSTR)malloc(buff_size * sizeof(TCHAR));
    expected_text_size = buff_size + _tcslen(AUTOEXEC_INTRO)
                         + 4 /* 2 * '\r\n' */;
    expected_text = (LPTSTR)malloc(expected_text_size * sizeof(TCHAR));
    _tcscpy(expected_text, AUTOEXEC_INTRO);
    _tcscat(expected_text, _T("\r\n"));
    _stprintf(buff, AUTOEXEC_CMD, path);
    _tcscat(expected_text, buff);
    _tcscat(expected_text, _T("\r\n"));

    begin_pos = _tcsstr(data, expected_text);
    if (begin_pos){
        file = _tfopen(AUTOEXEC_PATH, _T("wb"));
        if (!file){
            err_out(_T("Cannot open \"autoexec.bat\" for writing."));
            func_ret = FALSE;
        }
        else{
            final_part = begin_pos + _tcslen(expected_text);
            if ((fwrite(data, begin_pos - data, 1, file) != 1 &&
                 (begin_pos - data)) || /* "fwrite"fails if the
                                           second argument is 0 */
                (fwrite(final_part, _tcslen(final_part), 1, file) != 1 &&
                 _tcslen(final_part)))
                    func_ret = FALSE;
            fclose(file);
        }
    }
    else
        verb_out(_T("Value does not exist in \"autoexec.bat\"."));

    free(data);
    free(path);
    free(buff);
    free(expected_text);

    return func_ret;
}
//---------------------------------------------------------------------------
#endif /* #if !defined _UNICODE */
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const AnsiString Path)
{
  bool Result;
  if (Win32Platform == VER_PLATFORM_WIN32_NT)
  {
    Result = add_path_reg(Path.c_str());
  }
  else
  {
    Result = add_path_autoexec(Path.c_str());
  }

  if (!Result)
  {
    throw ExtException(FMTLOAD(ADD_PATH_ERROR, (Path)), LastPathError);
  }
}
//---------------------------------------------------------------------------
void __fastcall RemoveSearchPath(const AnsiString Path)
{
  bool Result;
  if (Win32Platform == VER_PLATFORM_WIN32_NT)
  {
    Result = remove_path_reg(Path.c_str());
  }
  else
  {
    Result = remove_path_autoexec(Path.c_str());
  }

  if (!Result)
  {
    throw ExtException(FMTLOAD(REMOVE_PATH_ERROR, (Path)), LastPathError);
  }
}
//---------------------------------------------------------------------------
void __fastcall RegisterAsUrlHandler()
{
  try
  {
    bool Success;
    bool User = true;
    TRegistry * Registry = new TRegistry();
    try
    {
      do
      {
        Success = true;
        User = !User;

        try
        {
          assert(Configuration != NULL);
          AnsiString FileName = Application->ExeName;
          AnsiString BaseKey;

          Registry->Access = KEY_WRITE;
          if (User)
          {
            Registry->RootKey = HKEY_CURRENT_USER;
            BaseKey = "Software\\Classes\\";
          }
          else
          {
            Registry->RootKey = HKEY_CLASSES_ROOT;
            BaseKey = "";
          }

          AnsiString Protocol;
          for (int Index = 0; Index <= 1; Index++)
          {
            Protocol = (Index == 0) ? "SCP" : "SFTP";
            if (Registry->OpenKey(BaseKey + Protocol, true))
            {
              Registry->WriteString("", FMTLOAD(PROTOCOL_URL_DESC, (Protocol)));
              Registry->WriteString("URL Protocol", "");
              Registry->WriteInteger("EditFlags", 0x02);
              Registry->WriteInteger("BrowserFlags", 0x08);
              if (Registry->OpenKey("DefaultIcon", true))
              {
                Registry->WriteString("", FORMAT("\"%s\",0", (FileName)));
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

            if (Registry->OpenKey(BaseKey + Protocol, false) &&
                Registry->OpenKey("shell", true) &&
                Registry->OpenKey("open", true) &&
                Registry->OpenKey("command", true))
            {
              Registry->WriteString("", FORMAT("\"%s\" \"%%1\"", (FileName)));
              Registry->CloseKey();
            }
            else
            {
              Abort();
            }
          }
        }
        catch(...)
        {
          Success = false;
        }
      }
      while (!Success && !User);
    }
    __finally
    {
      delete Registry;
    }
  }
  catch(Exception & E)
  {
    throw ExtException(&E, LoadStr(REGISTER_URL_ERROR));
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
        TQueryButtonAlias Aliases[1];
        Aliases[0].Button = qaRetry;
        Aliases[0].Alias = LoadStr(OPEN_BUTTON);
        TMessageParams Params(mpNeverAskAgainCheck);
        Params.Aliases = Aliases;
        Params.AliasesCount = LENOF(Aliases);

        int Answer = MoreMessageDialog(
          FMTLOAD(CLEAN_TEMP_CONFIRM, (Folders->Count)), Folders,
          qtWarning, qaYes | qaNo | qaRetry, HELP_CLEAN_TEMP_CONFIRM, &Params);

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
      TStrings * F = Folders;
      Folders = NULL;
      try
      {
        WinConfiguration->CleanupTemporaryFolders(F);
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
//---------------------------------------------------------------------------
int __fastcall CalculateCompoundVersion(int MajorVer,
  int MinorVer, int Release, int Build)
{
  int CompoundVer = Build + 1000 * (Release + 100 * (MinorVer +
    100 * MajorVer));
  return CompoundVer;
}
//---------------------------------------------------------------------------
int __fastcall CurrentCompoundVersion()
{
  TVSFixedFileInfo * FileInfo = Configuration->FixedApplicationInfo;
  return CalculateCompoundVersion(
    HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
    HIWORD(FileInfo->dwFileVersionLS), LOWORD(FileInfo->dwFileVersionLS));
}
//---------------------------------------------------------------------------
AnsiString __fastcall VersionStrFromCompoundVersion(int Version)
{
  int MajorVer = Version / (1000*100*100);
  int MinorVer = (Version % (1000*100*100)) / (1000*100);
  int Release = (Version % (1000*100)) / (1000);
  AnsiString Result;
  if (Release > 0)
  {
    Result = FORMAT("%d.%d.%d", (MajorVer, MinorVer, Release));
  }
  else
  {
    Result = FORMAT("%d.%d", (MajorVer, MinorVer));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall QueryUpdates()
{
  bool Complete = false;
  try
  {
    AnsiString Response;

    TVSFixedFileInfo * FileInfo = Configuration->FixedApplicationInfo;
    int CurrentCompoundVer = CurrentCompoundVersion();
    AnsiString CurrentVersionStr =
      FORMAT("%d.%d.%d.%d",
        (HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
         HIWORD(FileInfo->dwFileVersionLS), LOWORD(FileInfo->dwFileVersionLS)));

    TUpdatesConfiguration Updates = WinConfiguration->Updates;
    THttp * CheckForUpdatesHTTP = new THttp(Application);
    try
    {
      AnsiString URL = LoadStr(UPDATES_URL) +
        FORMAT("?v=%s&lang=%s", (CurrentVersionStr,
          IntToHex(__int64(GUIConfiguration->Locale), 4)));
      if (!Updates.ProxyHost.IsEmpty())
      {
        CheckForUpdatesHTTP->Proxy = FORMAT("%s:%d", (Updates.ProxyHost, Updates.ProxyPort));
      }
      CheckForUpdatesHTTP->URL = URL;
      CheckForUpdatesHTTP->Action();
      // sanity check
      if (CheckForUpdatesHTTP->Stream->Size > 102400)
      {
        Abort();
      }
      Response.SetLength(static_cast<int>(CheckForUpdatesHTTP->Stream->Size));
      CheckForUpdatesHTTP->Stream->Read(Response.c_str(), Response.Length());
    }
    __finally
    {
      delete CheckForUpdatesHTTP;
    }

    bool Changed = !Updates.HaveResults;
    Updates.LastCheck = Now();
    Updates.HaveResults = true;
    TUpdatesData PrevResults = Updates.Results;
    Updates.Results.Reset();
    Updates.Results.ForVersion = CurrentCompoundVer;

    while (!Response.IsEmpty())
    {
      AnsiString Line = ::CutToChar(Response, '\n', false);
      AnsiString Name = ::CutToChar(Line, '=', false);
      if (AnsiSameText(Name, "Version"))
      {
        int MajorVer = StrToInt(::CutToChar(Line, '.', false));
        int MinorVer = StrToInt(::CutToChar(Line, '.', false));
        int Release = StrToInt(::CutToChar(Line, '.', false));
        int Build = StrToInt(::CutToChar(Line, '.', false));
        int NewVersion = CalculateCompoundVersion(MajorVer, MinorVer, Release, Build);
        Changed |= (NewVersion != PrevResults.Version);
        if (NewVersion <= CurrentCompoundVer)
        {
          NewVersion = 0;
        }
        Updates.Results.Version = NewVersion;
        Complete = true;
      }
      else if (AnsiSameText(Name, "Message"))
      {
        Changed |= (PrevResults.Message != Line);
        Updates.Results.Message = Line;
      }
      else if (AnsiSameText(Name, "Critical"))
      {
        bool NewCritical = (StrToIntDef(Line, 0) != 0);
        Changed |= (PrevResults.Critical != NewCritical);
        Updates.Results.Critical = NewCritical;
      }
      else if (AnsiSameText(Name, "Release"))
      {
        Changed |= (PrevResults.Release != Line);
        Updates.Results.Release = Line;
      }
      else if (AnsiSameText(Name, "Disabled"))
      {
        bool NewDisabled = (StrToIntDef(Line, 0) != 0);
        Changed |= (PrevResults.Disabled != NewDisabled);
        Updates.Results.Disabled = NewDisabled;
        Complete = true;
      }
      else if (AnsiSameText(Name, "Url"))
      {
        Changed |= (PrevResults.Url != Line);
        Updates.Results.Url = Line;
      }
      else if (AnsiSameText(Name, "UrlButton"))
      {
        Changed |= (PrevResults.UrlButton != Line);
        Updates.Results.UrlButton = Line;
      }
    }

    if (Changed)
    {
      Updates.ShownResults = false;
    }

    WinConfiguration->Updates = Updates;
  }
  catch(Exception & E)
  {
    throw ExtException(&E, LoadStr(CHECK_FOR_UPDATES_ERROR));
  }

  if (!Complete)
  {
    throw Exception(LoadStr(CHECK_FOR_UPDATES_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall GetUpdatesMessage(AnsiString & Message, bool & New,
  TQueryType & Type, bool Force)
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  assert(Updates.HaveResults);
  if (Updates.HaveResults)
  {
    if (Updates.Results.Disabled)
    {
      if (Force)
      {
        Message = LoadStr(UPDATE_DISABLED)+"%s";
      }
    }
    else
    {
      New = (Updates.Results.Version > 0);
      if (New)
      {
        AnsiString Version = VersionStrFromCompoundVersion(Updates.Results.Version);
        if (!Updates.Results.Release.IsEmpty())
        {
          Version = FORMAT("%s %s", (Version, Updates.Results.Release));
        }
        Message = FMTLOAD(NEW_VERSION3, (Version, "%s"));
      }
      else
      {
        Message = LoadStr(NO_NEW_VERSION) + "%s";
      }
    }

    if (!Updates.Results.Message.IsEmpty())
    {
      Message = FORMAT(Message,
        (FMTLOAD(UPDATE_MESSAGE,
          (StringReplace(Updates.Results.Message, "|", "\n", TReplaceFlags() << rfReplaceAll)))));
    }
    else
    {
      Message = FORMAT(Message, (""));
    }
    Type = (Updates.Results.Critical ? qtWarning : qtInformation);
  }
  else
  {
    New = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall CheckForUpdates(bool CachedResults)
{
  TCustomForm * ActiveForm = Screen->ActiveCustomForm;
  Busy(true);
  try
  {
    if (ActiveForm)
    {
      assert(ActiveForm->Enabled);
      ActiveForm->Enabled = false;
    }

    bool Again = false;
    do
    {
      TUpdatesConfiguration Updates = WinConfiguration->Updates;
      bool Cached = !Again && Updates.HaveResults &&
        (double(Updates.Period) > 0) &&
        (Updates.Results.ForVersion == CurrentCompoundVersion()) &&
        CachedResults;
      if (!Cached)
      {
        QueryUpdates();
        // reread enw data
        Updates = WinConfiguration->Updates;
      }
      Again = false;

      if (!Updates.ShownResults)
      {
        Updates.ShownResults = true;
        WinConfiguration->Updates = Updates;
      }
      assert(Updates.HaveResults);

      AnsiString Message;
      bool New;
      TQueryType Type;
      GetUpdatesMessage(Message, New, Type, true);

      // add FLAGMASK(Cached, qaRetry) to enable "check again" button
      // for cached results
      int Answers = qaOK |
        FLAGMASK(New, qaCancel | qaAll) |
        FLAGMASK(!Updates.Results.Url.IsEmpty(), qaYes);
      TQueryButtonAlias Aliases[4];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(CHECK_AGAIN_BUTTON);
      Aliases[1].Button = qaYes;
      if (Updates.Results.UrlButton.IsEmpty())
      {
        Aliases[1].Alias = LoadStr(UPDATE_URL_BUTTON);
      }
      else
      {
        Aliases[1].Alias = Updates.Results.UrlButton;
      }
      Aliases[2].Button = qaAll;
      Aliases[2].Alias = LoadStr(WHATS_NEW_BUTTON);
      Aliases[3].Button = qaOK;
      Aliases[3].Alias = LoadStr(DOWNLOAD_BUTTON);

      TMessageParams Params;
      Params.Aliases = Aliases;
      // alias "ok" button to "download" only if we have new version
      Params.AliasesCount = (New ? 4 : 3);
      int Answer =
        MessageDialog(Message, Type,
          Answers, HELP_UPDATES, &Params);
      switch (Answer)
      {
        case qaOK:
          if (New)
          {
            OpenBrowser(LoadStr(DOWNLOAD_URL));
          }
          break;

        case qaYes:
          OpenBrowser(Updates.Results.Url);
          break;

        case qaAll:
          OpenBrowser(LoadStr(HISTORY_URL));
          break;

        case qaRetry:
          Again = true;
          break;
      }
    }
    while (Again);
  }
  __finally
  {
    if (ActiveForm)
    {
      ActiveForm->Enabled = true;
    }
    Busy(false);
  }
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
    QueryUpdates();
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
  assert(UpdateThread == NULL);
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
