//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "WinConfiguration.h"
#include "Common.h"
#include "Bookmarks.h"
#include <stdio.h>
//---------------------------------------------------------------------------
#define CSIDL_PERSONAL                  0x0005        // My Documents
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const char ShellCommandFileNamePattern[] = "!.!";
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::TWinConfiguration(): TGUIConfiguration()
{
  FBookmarks = new TBookmarks();
  FCommandsHistory = new TStringList();
  Default();
}
//---------------------------------------------------------------------------
__fastcall TWinConfiguration::~TWinConfiguration()
{
  if (!FTemporarySessionFile.IsEmpty()) DeleteFile(FTemporarySessionFile);
  ClearTemporaryLoginData();

  delete FBookmarks;
  delete FCommandsHistory;
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Default()
{
  TGUIConfiguration::Default();

  FInterface = ifCommander;

  FDDAllowMove = false;
  FDDTransferConfirmation = true;
  FDDTemporaryDirectory = "";
  FDDWarnLackOfTempSpace = true;
  FDDWarnLackOfTempSpaceRatio = 1.1;
  FDeleteToRecycleBin = true;
  FMaskHistory = "";
  FSelectDirectories = false;
  FSelectMask = "*.*";
  FShowHiddenFiles = true;
  FShowInaccesibleDirectories = true;
  FShowAdvancedLoginOptions = false;
  FConfirmDeleting = true;
  FConfirmClosingSession = true;
  FCopyOnDoubleClick = false;
  FCopyOnDoubleClickConfirmation = false;
  FDimmHiddenFiles = true;
  FAutoStartSession = "";
  FExpertMode = true;
  FUseLocationProfiles = true;

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

  FLogView = lvNone;
  FLogWindowOnStartup = true;
  FLogWindowParams = "-1;-1;500;400";

  FScpExplorer.WindowParams = "-1;-1;600;400;0";
  FScpExplorer.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,1|0;1;2;3;4;5";
  FScpExplorer.CoolBarLayout = "6,0,1,196,6;2,1,0,531,5;5,1,1,103,4;3,0,1,127,3;4,1,0,636,2;1,1,1,636,1;0,1,1,636,0";
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
  FScpCommander.CoolBarLayout = "5,0,0,219,6;1,1,0,319,5;4,0,0,227,4;3,1,0,136,3;6,1,0,121,2;2,1,1,67,1;0,1,1,649,0";
  FScpCommander.CurrentPanel = osLocal;
  FScpCommander.RemotePanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0|0;1;2;3;4;5";
  FScpCommander.RemotePanel.StatusBar = true;
  FScpCommander.RemotePanel.CoolBarLayout = "2,1,0,137,2;1,1,0,86,1;0,1,1,91,0";
  FScpCommander.LocalPanel.DirViewParams = "0;1;0|150,1;70,1;101,1;79,1;62,1;55,0|0;1;2;3;4;5";
  FScpCommander.LocalPanel.StatusBar = true;
  FScpCommander.LocalPanel.CoolBarLayout = "2,1,0,137,2;1,1,0,86,1;0,1,1,91,0";

}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::Load()
{
  TGUIConfiguration::Load();

  TRegistry * Registry = new TRegistry();
  try
  {
    Registry->Access = KEY_READ;
    Registry->RootKey = HKEY_LOCAL_MACHINE;
    FDisableOpenEdit = false;
    if (Registry->OpenKey(RegistryStorageKey, false))
    {
      try
      {
        FDisableOpenEdit = Registry->ReadBool("DisableOpenEdit");
      }
      catch(...)
      {
      }
    }
  }
  __finally
  {
    delete Registry;
  }
}
//---------------------------------------------------------------------------
TStorage __fastcall TWinConfiguration::GetStorage()
{
  if (FStorage == stDetect)
  {
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
  return TGUIConfiguration::GetStorage();
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::ModifyAll()
{
  TGUIConfiguration::ModifyAll();
  FBookmarks->ModifyAll(true);
}
//---------------------------------------------------------------------------
THierarchicalStorage * TWinConfiguration::CreateScpStorage(bool SessionList)
{
  if (SessionList && !FTemporarySessionFile.IsEmpty())
  {
    return new TIniFileStorage(FTemporarySessionFile);
  }
  else
  {
    return TGUIConfiguration::CreateScpStorage(SessionList);
  }
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(".>")+1, ELEM.Length() - ELEM.LastDelimiter(".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(CANCREATE) \
  BLOCK("Interface", CANCREATE, \
    KEY(Bool,     CopyOnDoubleClick); \
    KEY(Bool,     CopyOnDoubleClickConfirmation); \
    KEY(Bool,     DDAllowMove); \
    KEY(Bool,     DDTransferConfirmation); \
    KEY(String,   DDTemporaryDirectory); \
    KEY(Bool,     DDWarnLackOfTempSpace); \
    KEY(Float,    DDWarnLackOfTempSpaceRatio); \
    KEY(Bool,     DeleteToRecycleBin); \
    KEY(Bool,     DimmHiddenFiles); \
    KEY(Integer,  Interface); \
    KEY(String,   MaskHistory); \
    KEY(Bool,     SelectDirectories); \
    KEY(String,   SelectMask); \
    KEY(Bool,     ShowHiddenFiles); \
    KEY(Bool,     ShowInaccesibleDirectories); \
    KEY(Bool,     ShowAdvancedLoginOptions); \
    KEY(Bool,     ConfirmDeleting); \
    KEY(Bool,     ConfirmClosingSession); \
    KEY(String,   AutoStartSession); \
    KEY(Bool,     UseLocationProfiles); \
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
    KEY(Integer, LogView); \
    KEY(Bool,    LogWindowOnStartup); \
    KEY(String,  LogWindowParams); \
  );
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  TGUIConfiguration::SaveSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) Storage->Write ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  REGCONFIG(true);
  #undef KEY

  if (Storage->OpenSubKey("Bookmarks", true))
  {
    FBookmarks->Save(Storage);

    Storage->CloseSubKey();
  }
  if (Storage->OpenSubKey("Commands", true))
  {
    Storage->WriteValues(FCommandsHistory);
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::LoadSpecial(THierarchicalStorage * Storage)
{
  TGUIConfiguration::LoadSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEY

  if (Storage->OpenSubKey("Bookmarks", false))
  {
    FBookmarks->Load(Storage);
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
void __fastcall TWinConfiguration::ClearTemporaryLoginData()
{
  if (!FTemporaryKeyFile.IsEmpty())
  {
    DeleteFile(FTemporaryKeyFile);
    FTemporaryKeyFile = "";
  }
}
//---------------------------------------------------------------------------
bool __fastcall TWinConfiguration::DumpResourceToFile(
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
void __fastcall TWinConfiguration::RestoreForm(AnsiString Data, TCustomForm * Form)
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
AnsiString __fastcall TWinConfiguration::StoreForm(TCustomForm * Form)
{
  assert(Form);
  return FORMAT("%d;%d;%d;%d;%d", ((int)Form->BoundsRect.Left, (int)Form->BoundsRect.Top,
    (int)Form->BoundsRect.Right, (int)Form->BoundsRect.Bottom,
    (int)Form->WindowState));
}
//---------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLogView(TLogView value)
{
  SET_CONFIG_PROPERTY(LogView);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLogWindowOnStartup(bool value)
{
  SET_CONFIG_PROPERTY(LogWindowOnStartup);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetLogWindowParams(AnsiString value)
{
  SET_CONFIG_PROPERTY(LogWindowParams);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDAllowMove(bool value)
{
  SET_CONFIG_PROPERTY(DDAllowMove);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTransferConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(DDTransferConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDTemporaryDirectory(AnsiString value)
{
  SET_CONFIG_PROPERTY(DDTemporaryDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDWarnLackOfTempSpace(bool value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpace);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDDWarnLackOfTempSpaceRatio(double value)
{
  SET_CONFIG_PROPERTY(DDWarnLackOfTempSpaceRatio);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetMaskHistory(AnsiString value)
{
  SET_CONFIG_PROPERTY(MaskHistory);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetScpExplorer(TScpExplorerConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpExplorer);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetScpCommander(TScpCommanderConfiguration value)
{
  SET_CONFIG_PROPERTY(ScpCommander);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetEditor(TEditorConfiguration value)
{
  SET_CONFIG_PROPERTY(Editor);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetInterface(TInterface value)
{
  SET_CONFIG_PROPERTY(Interface);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDeleteToRecycleBin(bool value)
{
  SET_CONFIG_PROPERTY(DeleteToRecycleBin);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetSelectDirectories(bool value)
{
  SET_CONFIG_PROPERTY(SelectDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(ShowHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowInaccesibleDirectories(bool value)
{
  SET_CONFIG_PROPERTY(ShowInaccesibleDirectories);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetShowAdvancedLoginOptions(bool value)
{
  SET_CONFIG_PROPERTY(ShowAdvancedLoginOptions);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmDeleting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmDeleting);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetUseLocationProfiles(bool value)
{
  SET_CONFIG_PROPERTY(UseLocationProfiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetConfirmClosingSession(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmClosingSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCopyOnDoubleClick(bool value)
{
  SET_CONFIG_PROPERTY(CopyOnDoubleClick);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCopyOnDoubleClickConfirmation(bool value)
{
  SET_CONFIG_PROPERTY(CopyOnDoubleClickConfirmation);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetDimmHiddenFiles(bool value)
{
  SET_CONFIG_PROPERTY(DimmHiddenFiles);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetAutoStartSession(AnsiString value)
{
  SET_CONFIG_PROPERTY(AutoStartSession);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetExpertMode(bool value)
{
  SET_CONFIG_PROPERTY(ExpertMode);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetCommandsHistory(TStrings * value)
{
  assert(FCommandsHistory);
  FCommandsHistory->Assign(value);
}
//---------------------------------------------------------------------------
void __fastcall TWinConfiguration::SetBookmarks(AnsiString Key,
  TBookmarkList * value)
{
  FBookmarks->Bookmarks[Key] = value;
  Changed();
}
//---------------------------------------------------------------------------
TBookmarkList * __fastcall TWinConfiguration::GetBookmarks(AnsiString Key)
{
  return FBookmarks->Bookmarks[Key];
}
//---------------------------------------------------------------------------
void TWinConfiguration::ReformatFileNameCommand(AnsiString & Command)
{
  AnsiString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  if (Params.Pos(ShellCommandFileNamePattern) == 0)
  {
    Params = Params + (Params.IsEmpty() ? "" : " ") + ShellCommandFileNamePattern;
  }
  Command = FormatCommand(Program, Params);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinConfiguration::GetDefaultKeyFile()
{
  return FTemporaryKeyFile;
}


