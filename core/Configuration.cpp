//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <shlobj.hpp>
#include <FileInfo.h>

#include "Exceptions.h"
#include "Common.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#include "TextsCore.h"
#include "Interface.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define SET_CONFIG_PROPERTY(PROPERTY) \
  if (PROPERTY != value) { F ## PROPERTY = value; Changed(); }
//---------------------------------------------------------------------------
const char ShellCommandFileNamePattern[] = "!.!";
//---------------------------------------------------------------------------
bool SpecialFolderLocation(int PathID, AnsiString & Path)
{
  LPITEMIDLIST Pidl;
  char Buf[256];
  if (SHGetSpecialFolderLocation(NULL, PathID, &Pidl) == NO_ERROR &&
      SHGetPathFromIDList(Pidl, Buf))
  {
    Path = AnsiString(Buf);
    return true;
  }
  return false;
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::TConfiguration()
{
  FUpdating = 0;
  FStorage = stDetect;
  DontSave = false;
  RandomSeedSave = true;
  FApplicationInfo = NULL;
  FBookmarks[osLocal] = new TStringList();
  FBookmarks[osRemote] = new TStringList();
  FCommandsHistory = new TStringList();
  Default();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Default()
{
  FInterface = ifCommander;

  AnsiString ARandomSeedFile = RandomSeedFile;
  // This works correct only when Default() is called before first
  // change to RandomSeedFile property
  RandomSeedFile = StringReplace(ExtractFilePath(ARandomSeedFile) +
    "winscp" + ExtractFileExt(ARandomSeedFile), "\\\\", "\\",
    TReplaceFlags() << rfReplaceAll);
  FIgnoreCancelBeforeFinish = TDateTime(0, 0, 3, 0);
  FDefaultDirIsHome = true;
  FDDAllowMove = false;
  FDDTransferConfirmation = true;
  FDDTemporaryDirectory = "";
  FDDWarnLackOfTempSpace = true;
  FDDWarnLackOfTempSpaceRatio = 1.1;
  FDeleteToRecycleBin = true;
  FCopyParamDialogExpanded = false;
  FErrorDialogExpanded = false;
  FMaskHistory = "";
  FSelectDirectories = false;
  FSelectMask = "*.*";
  FShowHiddenFiles = true;
  FShowInaccesibleDirectories = true;
  FShowAdvancedLoginOptions = false;
  FConfirmOverwriting = true;
  FConfirmDeleting = true;
  FConfirmClosingSession = true;
  FCopyOnDoubleClick = false;
  FCopyOnDoubleClickConfirmation = false;
  FDimmHiddenFiles = true;
  FCopyParam.Default();
  FAutoStartSession = "";
  FExpertMode = true;

  FEditor.Editor = edInternal;
  FEditor.ExternalEditor = "notepad.exe";
  FEditor.FontName = "Courier New";
  FEditor.FontHeight = -12;
  FEditor.FontStyle = 0;
  FEditor.FontCharset = DEFAULT_CHARSET;
  FEditor.WordWrap = false;
  FEditor.FindText = "";
  FEditor.ReplaceText = "";
  FEditor.FindMatchCase = false;
  FEditor.FindWholeWord = false;

  FLogging = false;
  FLogFileName = "";
  FLogFileAppend = true;
  FLogWindowLines = 100;
  FLogView = lvNone;
  FLogWindowOnStartup = true;
  FLogWindowParams = "-1;-1;500;400";

  FScpExplorer.WindowParams = "-1;-1;600;400;0";
  FScpExplorer.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,1|0;1;2;3;4;5";
  FScpExplorer.CoolBarLayout = "6,0,1,196,6;5,1,0,104,5;4,0,0,117,4;3,0,1,127,3;2,1,0,373,2;1,1,1,281,1;0,1,1,766,0";
  FScpExplorer.StatusBar = true;
  AnsiString PersonalFolder;
  SpecialFolderLocation(CSIDL_PERSONAL, PersonalFolder);
  FScpExplorer.LastLocalTargetDirectory = PersonalFolder;
  FScpExplorer.ViewStyle = 0; /* vsIcon */
  FScpExplorer.ShowFullAddress = true;

  FScpCommander.WindowParams = "-1;-1;600;400;0";
  FScpCommander.LocalPanelWidth = 0.5;
  FScpCommander.StatusBar = true;
  FScpCommander.ToolBar = true;
  FScpCommander.ExplorerStyleSelection = false;
  FScpCommander.CoolBarLayout = "5,1,0,655,6;6,1,0,311,5;4,0,0,204,4;3,1,0,137,3;2,1,0,68,2;1,1,1,127,1;0,1,1,655,0";
  FScpCommander.CurrentPanel = osLocal;
  FScpCommander.RemotePanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0|0;1;2;3;4;5";
  FScpCommander.RemotePanel.StatusBar = true;
  FScpCommander.RemotePanel.CoolBarLayout = "2,1,0,137,2;1,1,0,86,1;0,1,1,91,0";
  FScpCommander.LocalPanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0|0;1;2;3;4;5";
  FScpCommander.LocalPanel.StatusBar = true;
  FScpCommander.LocalPanel.CoolBarLayout = "2,1,0,137,2;1,1,0,86,1;0,1,1,91,0";

  Changed();
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::~TConfiguration()
{
  assert(!FUpdating);
  if (RandomSeedSave) random_save_seed();
  if (FApplicationInfo) FreeFileInfo(FApplicationInfo);

  if (!FTemporarySessionFile.IsEmpty()) DeleteFile(FTemporarySessionFile);
  ClearTemporaryLoginData();

  FreeBookmarks();
  delete FBookmarks[osLocal];
  delete FBookmarks[osRemote];
  delete FCommandsHistory;
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::CreateScpStorage(bool SessionList)
{
  if (SessionList && !FTemporarySessionFile.IsEmpty())
  {
    return new TIniFileStorage(FTemporarySessionFile);
  }
  else if (Storage == stRegistry)
  {
    return new TRegistryStorage(RegistryStorageKey);
  }
  else
  {
    return new TIniFileStorage(IniFileStorageName);
  }
}
//---------------------------------------------------------------------------
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(".>")+1, ELEM.Length() - ELEM.LastDelimiter(".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(ACCESS, CANCREATE, ADDON) \
  THierarchicalStorage * Storage = CreateScpStorage(false); \
  try { \
    Storage->AccessMode = ACCESS; \
    if (Storage->OpenSubKey(ConfigurationSubKey, CANCREATE)) { \
      BLOCK("Interface", CANCREATE, \
        KEY(String,   RandomSeedFile); \
        KEY(Bool,     CopyOnDoubleClick); \
        KEY(Bool,     CopyOnDoubleClickConfirmation); \
        KEY(Bool,     CopyParamDialogExpanded); \
        KEY(Bool,     DDAllowMove); \
        KEY(Bool,     DDTransferConfirmation); \
        KEY(String,   DDTemporaryDirectory); \
        KEY(Bool,     DDWarnLackOfTempSpace); \
        KEY(Float,    DDWarnLackOfTempSpaceRatio); \
        KEY(Bool,     DefaultDirIsHome); \
        KEY(Bool,     DeleteToRecycleBin); \
        KEY(Bool,     DimmHiddenFiles); \
        KEY(Bool,     ErrorDialogExpanded); \
        KEY(DateTime, IgnoreCancelBeforeFinish); \
        KEY(Integer,  Interface); \
        KEY(String,   MaskHistory); \
        KEY(Bool,     SelectDirectories); \
        KEY(String,   SelectMask); \
        KEY(Bool,     ShowHiddenFiles); \
        KEY(Bool,     ShowInaccesibleDirectories); \
        KEY(Bool,     ShowAdvancedLoginOptions); \
        KEY(Bool,     ConfirmOverwriting); \
        KEY(Bool,     ConfirmDeleting); \
        KEY(Bool,     ConfirmClosingSession); \
        KEY(String,   AutoStartSession); \
      ); \
      BLOCK("Interface\\Editor", CANCREATE, \
        KEY(Integer,  Editor.Editor); \
        KEY(String,   Editor.ExternalEditor); \
        KEY(String,   Editor.FontName); \
        KEY(Integer,  Editor.FontHeight); \
        KEY(Integer,  Editor.FontStyle); \
        KEY(Integer,  Editor.FontCharset); \
        KEY(Bool,     Editor.WordWrap); \
        KEY(String,   Editor.FindText); \
        KEY(String,   Editor.ReplaceText); \
        KEY(Bool,     Editor.FindMatchCase); \
        KEY(Bool,     Editor.FindWholeWord); \
      ); \
      BLOCK("Interface\\CopyParam", CANCREATE, \
        KEY(Bool,    CopyParam.AddXToDirectories); \
        KEY(String,  CopyParam.AsciiFileMask.Masks); \
        KEY(Integer, CopyParam.FileNameCase); \
        KEY(Bool,    CopyParam.PreserveReadOnly); \
        KEY(Bool,    CopyParam.PreserveTime); \
        KEY(Bool,    CopyParam.PreserveRights); \
        KEY(String,  CopyParam.Rights.Text); \
        KEY(Integer, CopyParam.TransferMode); \
        KEY(Integer, CopyParam.ResumeSupport); \
        KEY(Int64,   CopyParam.ResumeThreshold); \
      ); \
      BLOCK("Interface\\Explorer", CANCREATE, \
        KEY(String,  ScpExplorer.CoolBarLayout); \
        KEY(String,  ScpExplorer.DirViewParams); \
        KEY(String,  ScpExplorer.LastLocalTargetDirectory); \
        KEY(Bool,    ScpExplorer.StatusBar); \
        KEY(String,  ScpExplorer.WindowParams); \
        KEY(Integer, ScpExplorer.ViewStyle); \
        KEY(Bool,    ScpExplorer.ShowFullAddress); \
      ); \
      BLOCK("Interface\\Commander", CANCREATE, \
        KEY(String,  ScpCommander.CoolBarLayout); \
        KEY(Integer, ScpCommander.CurrentPanel); \
        KEY(Float,   ScpCommander.LocalPanelWidth); \
        KEY(Bool,    ScpCommander.StatusBar); \
        KEY(Bool,    ScpCommander.ToolBar); \
        KEY(String,  ScpCommander.WindowParams); \
        KEY(Bool,    ScpCommander.ExplorerStyleSelection); \
      ); \
      BLOCK("Interface\\Commander\\LocalPanel", CANCREATE, \
        KEY(String, ScpCommander.LocalPanel.CoolBarLayout); \
        KEY(String, ScpCommander.LocalPanel.DirViewParams); \
        KEY(Bool,   ScpCommander.LocalPanel.StatusBar); \
      ); \
      BLOCK("Interface\\Commander\\RemotePanel", CANCREATE, \
        KEY(String, ScpCommander.RemotePanel.CoolBarLayout); \
        KEY(String, ScpCommander.RemotePanel.DirViewParams); \
        KEY(Bool,   ScpCommander.RemotePanel.StatusBar); \
      ); \
      BLOCK("Logging", CANCREATE, \
        KEY(Bool,    Logging); \
        KEY(String,  LogFileName); \
        KEY(Bool,    LogFileAppend); \
        KEY(Integer, LogWindowLines); \
        KEY(Integer, LogView); \
        KEY(Bool,    LogWindowOnStartup); \
        KEY(String,  LogWindowParams); \
      ); \
      ADDON(Storage); \
    }; \
  } __finally { \
    delete Storage; \
  }
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  Storage->RecursiveDeleteSubKey("Bookmarks");
  if (Storage->OpenSubKey("Bookmarks", true))
  {
    for (int Side = 0; Side < 2; Side++)
    {
      if (Storage->OpenSubKey(Side == osLocal ? "Local" : "Remote", true))
      {
        for (int Index = 0; Index < FBookmarks[Side]->Count; Index++)
        {
          if (FBookmarks[Side]->Objects[Index] &&
              ((TStrings*)FBookmarks[Side]->Objects[Index])->Count &&
              Storage->OpenSubKey(FBookmarks[Side]->Strings[Index], true))
          {
            Storage->WriteValues((TStrings*)FBookmarks[Side]->Objects[Index]);
            Storage->CloseSubKey();
          }
        }
        Storage->CloseSubKey();
      }
    }
    Storage->CloseSubKey();
  }
  if (Storage->OpenSubKey("Commands", true))
  {
    Storage->WriteValues(FCommandsHistory);
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Save()
{
  if (DontSave) return;

  if (Storage == stRegistry) CleanupIniFile();

  #define KEY(TYPE, VAR) Storage->Write ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  REGCONFIG(smReadWrite, true, SaveSpecial);
  #undef KEY
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::FreeBookmarks()
{
  for (int Side = 0; Side < 2; Side++)
  {
    for (int Index = 0; Index < FBookmarks[Side]->Count; Index++)
    {
      if (FBookmarks[Side]->Objects[Index])
      {
        delete FBookmarks[Side]->Objects[Index];
      }
    }
    FBookmarks[Side]->Clear();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadSpecial(THierarchicalStorage * Storage)
{
  FreeBookmarks();
  if (Storage->OpenSubKey("Bookmarks", false))
  {
    for (int Side = 0; Side < 2; Side++)
    {
      if (Storage->OpenSubKey(Side == osLocal ? "Local" : "Remote", false))
      {
        TStrings * BookmarkKeys = new TStringList();
        try
        {
          Storage->GetSubKeyNames(BookmarkKeys);
          for (int Index = 0; Index < BookmarkKeys->Count; Index++)
          {
            if (Storage->OpenSubKey(BookmarkKeys->Strings[Index], false))
            {
              TStrings * Bookmarks = new TStringList();
              Storage->ReadValues(Bookmarks);
              FBookmarks[Side]->AddObject(BookmarkKeys->Strings[Index], Bookmarks);
              Storage->CloseSubKey();
            }
          }
        }
        __finally
        {
          delete BookmarkKeys;
        }
        Storage->CloseSubKey();
      }
    }
    Storage->CloseSubKey();
  }
  FCommandsHistory->Clear();
  if (Storage->OpenSubKey("Commands", false))
  {
    Storage->ReadValues(FCommandsHistory);
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Load()
{
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  #pragma warn -eas
  REGCONFIG(smRead, false, LoadSpecial);
  #pragma warn +eas
  #undef KEY
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::ClearTemporaryLoginData()
{
  if (!FTemporaryKeyFile.IsEmpty())
  {
    DeleteFile(FTemporaryKeyFile);
    FTemporaryKeyFile = "";
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Changed()
{
  if (FUpdating == 0)
  {
    if (OnChange)
    {
      OnChange(this);
    }
  }
  else
  {
    FChanged = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::BeginUpdate()
{
  if (FUpdating == 0)
  {
    FChanged = false;
  }
  FUpdating++;
  // Greater value would probably indicate some nesting problem in code
  assert(FUpdating < 6);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::EndUpdate()
{
  assert(FUpdating > 0);
  FUpdating--;
  if ((FUpdating == 0) && FChanged)
  {
    FChanged = false;
    Changed();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupConfiguration()
{
  try
  {
    CleanupRegistry(ConfigurationSubKey);
    if (Storage == stRegistry)
    {
      DontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_CONFIG_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRegistry(AnsiString CleanupSubKey)
{
  TRegistryStorage *Registry = new TRegistryStorage(RegistryStorageKey);
  try
  {
    Registry->RecursiveDeleteSubKey(CleanupSubKey);
  }
  __finally
  {
    delete Registry;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupHostKeys()
{
  try
  {
    CleanupRegistry(SshHostKeysSubKey);
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_HOSTKEYS_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRandomSeedFile()
{
  try
  {
    RandomSeedSave = false;
    if (FileExists(RandomSeedFile))
    {
      if (!DeleteFile(RandomSeedFile)) Abort();
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_SEEDFILE_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupIniFile()
{
  try
  {
    if (FileExists(IniFileStorageName))
    {
      if (!DeleteFile(IniFileStorageName)) Abort();
    }
    if (Storage == stIniFile)
    {
      DontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_INIFILE_ERROR);
  }
}
//---------------------------------------------------------------------------
TVSFixedFileInfo *__fastcall TConfiguration::GetFixedApplicationInfo()
{
  return GetFixedFileInfo(ApplicationInfo);
}
//---------------------------------------------------------------------------
void * __fastcall TConfiguration::GetApplicationInfo()
{
  if (!FApplicationInfo)
  {
    FApplicationInfo = CreateFileInfo(ParamStr(0));
  }
  return FApplicationInfo;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetVersionStr()
{
  try
  {
    return FmtLoadStr(VERSION, ARRAYOFCONST((
      HIWORD(FixedApplicationInfo->dwFileVersionMS),
      LOWORD(FixedApplicationInfo->dwFileVersionMS),
      HIWORD(FixedApplicationInfo->dwFileVersionLS),
      LOWORD(FixedApplicationInfo->dwFileVersionLS))));
  }
  catch (Exception &E)
  {
    throw ExtException(&E, "Can't get application version");
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRegistryStorageKey()
{
  return GetRegistryKey();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetIniFileStorageName(AnsiString value)
{
  if (!value.IsEmpty() && !FileExists(value))
  {
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (value)));
  }
  FIniFileStorageName = value;
  FStorage = stIniFile;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetIniFileStorageName()
{
  if (FIniFileStorageName.IsEmpty())
  {
    return ChangeFileExt(ParamStr(0), ".ini");
  }
  else
  {
    return FIniFileStorageName;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPuttyRegistryStorageKey()
{
  return PUTTY_REG_POS;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPuttySessionsKey()
{
  return PuttyRegistryStorageKey + "\\Sessions";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetStoredSessionsSubKey()
{
  return "Sessions";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetSshHostKeysSubKey()
{
  return "SshHostKeys";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetConfigurationSubKey()
{
  return "Configuration";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRootKeyStr()
{
  return RootKeyToStr(HKEY_CURRENT_USER);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetStorage(TStorage value)
{
  if(FStorage != value)
  {
    FStorage = value;
    Save();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::DumpResourceToFile(
  const AnsiString ResName, const AnsiString FileName)
{
  HRSRC Resource;
  Resource = FindResourceEx(NULL, RT_RCDATA, ResName.c_str(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
  if (Resource)
  {
    unsigned long Size = SizeofResource(NULL, Resource);
    if (!Size)
    {
      throw Exception(FORMAT("Cannot get size of resource %s", (ResName)));
    }

    void * Content = LoadResource(NULL, Resource);
    if (!Content)
    {
      throw Exception(FORMAT("Cannot read resource %s", (ResName)));
    }

    Content = LockResource(Content);
    if (!Content)
    {
      throw Exception(FORMAT("Cannot lock resource %s", (ResName)));
    }

    FILE * f = fopen(FileName.c_str(), "wb");
    if (!f)
    {
      throw Exception(FORMAT("Cannot create file %s", (FileName)));
    }
    if (fwrite(Content, 1, Size, f) != Size)
    {
      throw Exception(FORMAT("Cannot write to file %s", (FileName)));
    }
    fclose(f);
  }

  return (Resource != NULL);
}
//---------------------------------------------------------------------------
TStorage __fastcall TConfiguration::GetStorage()
{
  if (FStorage == stDetect)
  {
    FStorage = FileExists(IniFileStorageName) ? stIniFile : stRegistry;

    if (FindResourceEx(NULL, RT_RCDATA, "WINSCP_SESSION",
      MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL)))
    {
      FTemporarySessionFile = GetTemporaryPath() + "winscp3s.tmp";
      DumpResourceToFile("WINSCP_SESSION", FTemporarySessionFile);
      FEmbeddedSessions = true;
      FTemporaryKeyFile = GetTemporaryPath() + "winscp3k.tmp";
      if (!DumpResourceToFile("WINSCP_KEY", FTemporaryKeyFile))
      {
        FTemporaryKeyFile = "";
      }
    }
  }
  return FStorage;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::RestoreForm(AnsiString Data, TCustomForm * Form)
{
  assert(Form);
  if (!Data.IsEmpty())
  {
    TRect Bounds = Form->BoundsRect;
    Bounds.Left = StrToIntDef(CutToChar(Data, ';', true), Bounds.Left);
    Bounds.Top = StrToIntDef(CutToChar(Data, ';', true), Bounds.Top);
    Bounds.Right = StrToIntDef(CutToChar(Data, ';', true), Bounds.Right);
    Bounds.Bottom = StrToIntDef(CutToChar(Data, ';', true), Bounds.Bottom);
    TWindowState State = (TWindowState)StrToIntDef(CutToChar(Data, ';', true), (int)wsNormal);
    ((TForm*)Form)->WindowState = State;
    if (State == wsNormal)
    {
      if (Bounds.Width() > Screen->Width) Bounds.Right -= (Bounds.Width() - Screen->Width);
      if (Bounds.Height() > Screen->Height) Bounds.Bottom -= (Bounds.Height() - Screen->Height);
      Form->BoundsRect = Bounds;
      #define POS_RANGE(x, prop) (x < 0) || (x > Screen->prop)
      if (POS_RANGE(Bounds.Left, Width - 20) || POS_RANGE(Bounds.Top, Height - 40))
      {
        ((TForm*)Form)->Position = poDefaultPosOnly;
      }
      else
      {
        ((TForm*)Form)->Position = poDesigned;
      }
      #undef POS_RANGE
    }
  }
  else if (((TForm*)Form)->Position == poDesigned)
  {
    ((TForm*)Form)->Position = poDefaultPosOnly;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::StoreForm(TCustomForm * Form)
{
  assert(Form);
  return FORMAT("%d;%d;%d;%d;%d", ((int)Form->BoundsRect.Left, (int)Form->BoundsRect.Top,
    (int)Form->BoundsRect.Right, (int)Form->BoundsRect.Bottom,
    (int)Form->WindowState));
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetRandomSeedFile(AnsiString value)
{
  char *seedpath = seedpath_ptr();
  if (value.Length() > seedpath_size())
  {
    value.SetLength(seedpath_size());
  }
  strcpy(seedpath, StripPathQuotes(value).c_str());
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRandomSeedFile()
{
  return AnsiString(seedpath_ptr());
}
//---------------------------------------------------------------------------
TEOLType __fastcall TConfiguration::GetLocalEOLType()
{
  return eolCRLF;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogging(bool value)
{
  SET_CONFIG_PROPERTY(Logging);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileName(AnsiString value)
{
  SET_CONFIG_PROPERTY(LogFileName);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogToFile(bool value)
{
  if (value != LogToFile)
  {
    LogFileName = value ? DefaultLogFileName : AnsiString("");
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogToFile()
{
  return !LogFileName.IsEmpty();
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileAppend(bool value)
{
  SET_CONFIG_PROPERTY(LogFileAppend);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowLines(int value)
{
  SET_CONFIG_PROPERTY(LogWindowLines);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowComplete(bool value)
{
  if (value != LogWindowComplete)
  {
    LogWindowLines = value ? 0 : 50;
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogWindowComplete()
{
  return (bool)(LogWindowLines == 0);
}
//---------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetDefaultLogFileName()
{
  return GetTemporaryPath() + "winscp.log";
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogView(TLogView value)
{
  SET_CONFIG_PROPERTY(LogView);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowOnStartup(bool value)
{
  SET_CONFIG_PROPERTY(LogWindowOnStartup);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowParams(AnsiString value)
{
  SET_CONFIG_PROPERTY(LogWindowParams);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDDAllowMove(bool value)
{
  SET_CONFIG_PROPERTY(DDAllowMove);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDDTransferConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(DDTransferConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDDTemporaryDirectory(AnsiString value)
{
  SET_CONFIG_PROPERTY(DDTemporaryDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDDWarnLackOfTempSpace(bool value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpace);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDDWarnLackOfTempSpaceRatio(double value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpaceRatio);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCopyParam(TCopyParamType value)
{
  FCopyParam.Assign(value);
  Changed();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetMaskHistory(AnsiString value)
{
  SET_CONFIG_PROPERTY(MaskHistory);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetScpExplorer(TScpExplorerConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpExplorer);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetScpCommander(TScpCommanderConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpCommander);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetEditor(TEditorConfiguration value)
{
  SET_CONFIG_PROPERTY(Editor);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDefaultDirIsHome(bool value)
{
  SET_CONFIG_PROPERTY(DefaultDirIsHome);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetInterface(TInterface value)
{
  SET_CONFIG_PROPERTY(Interface);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDeleteToRecycleBin(bool value)
{
  SET_CONFIG_PROPERTY(DeleteToRecycleBin);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetSelectDirectories(bool value)
{
  SET_CONFIG_PROPERTY(SelectDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetShowHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(ShowHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetShowInaccesibleDirectories(bool value)
{
  SET_CONFIG_PROPERTY(ShowInaccesibleDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetShowAdvancedLoginOptions(bool value)
{
  SET_CONFIG_PROPERTY(ShowAdvancedLoginOptions);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmDeleting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmDeleting);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmOverwriting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmOverwriting);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmClosingSession(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmClosingSession);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCopyOnDoubleClick(bool value)
{
  SET_CONFIG_PROPERTY(CopyOnDoubleClick);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCopyOnDoubleClickConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(CopyOnDoubleClickConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDimmHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(DimmHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetIgnoreCancelBeforeFinish(TDateTime value)
{
  SET_CONFIG_PROPERTY(IgnoreCancelBeforeFinish);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetAutoStartSession(AnsiString value)
{
  SET_CONFIG_PROPERTY(AutoStartSession);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetExpertMode(bool value)
{
  SET_CONFIG_PROPERTY(ExpertMode);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCommandsHistory(TStrings * value)
{
  assert(FCommandsHistory);
  FCommandsHistory->Assign(value);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetTimeFormat()
{
  return "h:nn:ss";
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetBookmarks(TOperationSide Side, AnsiString Key,
  TStrings * value)
{
  assert(FBookmarks[Side]);
  int Index;
  Key = AnsiLowerCase(Key);
  Index = FBookmarks[Side]->IndexOf(Key);
  TStrings * NewBookmarks = new TStringList();
  NewBookmarks->Assign(value);
  if (Index >= 0)
  {
    if (FBookmarks[Side]->Objects[Index])
    {
      delete FBookmarks[Side]->Objects[Index];
    }
    FBookmarks[Side]->Objects[Index] = NewBookmarks;
  }
  else
  {
    FBookmarks[Side]->AddObject(Key, NewBookmarks);
  };
  Changed();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TConfiguration::GetBookmarks(TOperationSide Side, AnsiString Key)
{
  assert(FBookmarks[Side]);
  int Index = FBookmarks[Side]->IndexOf(AnsiLowerCase(Key));
  return Index >= 0 ? (TStrings*)FBookmarks[Side]->Objects[Index] : NULL;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPartialExt() const
{
  return ".filepart";
}
//---------------------------------------------------------------------------
void TConfiguration::ReformatFileNameCommand(AnsiString & Command)
{
  AnsiString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  if (Params.Pos(ShellCommandFileNamePattern) == 0)
  {
    Params = Params + (Params.IsEmpty() ? "" : " ") + ShellCommandFileNamePattern;
  }
  Command = FormatCommand(Program, Params);
}

