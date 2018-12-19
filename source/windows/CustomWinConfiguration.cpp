//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <TextsCore.h>
#include <SessionData.h>
#include <CoreMain.h>
#include <Interface.h>
#include "CustomWinConfiguration.h"
#include <Exceptions.h>
#include <PasTools.hpp>
#include <Math.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TCustomWinConfiguration * CustomWinConfiguration = NULL;
//---------------------------------------------------------------------------
class THistoryStrings : public TStringList
{
public:
  __fastcall THistoryStrings() : TStringList()
  {
    FModified = false;
  };

  __property bool Modified = { read = FModified, write = FModified };

private:
  bool FModified;
};
//---------------------------------------------------------------------------
__fastcall TCustomWinConfiguration::TCustomWinConfiguration():
  TGUIConfiguration()
{
  FHistory = new TStringList();
  FEmptyHistory = new TStringList();
  FDefaultInterface = ifCommander;
  FCanApplyInterfaceImmediately = true;
}
//---------------------------------------------------------------------------
__fastcall TCustomWinConfiguration::~TCustomWinConfiguration()
{
  ClearHistory();
  delete FHistory;
  delete FEmptyHistory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::ClearHistory()
{
  DebugAssert(FHistory != NULL);

  THistoryStrings * HistoryStrings;
  for (int Index = 0; Index < FHistory->Count; Index++)
  {
    HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[Index]);
    FHistory->Objects[Index] = NULL;
    delete HistoryStrings;
  }
  FHistory->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::DefaultHistory()
{
  ClearHistory();

  std::unique_ptr<THistoryStrings> Strings;

  // Defaults for speed limits.
  Strings.reset(new THistoryStrings());
  // This is language-specifics, what has to be dealt with when changing language.
  // There's ad-hoc workaround in CopySpeedLimits.
  // If we need to solve this for another history, we should introduce
  // a generic solution, like language-specific history ("SpeedLimitEN")
  Strings->Add(LoadStr(SPEED_UNLIMITED));
  unsigned long Speed = 8192;
  while (Speed >= 8)
  {
    Strings->Add(IntToStr(int(Speed)));
    Speed = Speed / 2;
  }
  FHistory->AddObject(L"SpeedLimit", Strings.release());

  Strings.reset(new THistoryStrings());
  Strings->Add(FormatCommand(DefaultPuttyPath, L""));
  Strings->Add(FormatCommand(DefaultPuttyPath, L"-t -m \"%TEMP%\\putty.txt\" !`cmd.exe /c echo cd '!/' ; /bin/bash -login > \"%TEMP%\\putty.txt\"`"));
  Strings->Add(KittyExecutable);
  Strings->Add(FORMAT(L"%s -cmd \"cd '!/'\" !U@!@ -P !# -title \"!N\"", (KittyExecutable)));
  FHistory->AddObject(L"PuttyPath", Strings.release());
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomWinConfiguration::FormatDefaultWindowSize(int Width, int Height)
{
  return FORMAT(L"%d,%d,%s", (Width, Height, SaveDefaultPixelsPerInch()));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomWinConfiguration::FormatDefaultWindowParams(int Width, int Height)
{
  return FORMAT(L"-1;-1;%d;%d;%d;%s", (Width, Height, int(wsNormal), SaveDefaultPixelsPerInch()));
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::Default()
{
  TGUIConfiguration::Default();

  FInterface = FDefaultInterface;
  int WorkAreaWidthScaled = DimensionToDefaultPixelsPerInch(Screen->WorkAreaWidth);
  int WorkAreaHeightScaled = DimensionToDefaultPixelsPerInch(Screen->WorkAreaHeight);
  // Same as commander interface (really used only with /synchronize)
  int ChecklistWidth = Min(WorkAreaWidthScaled - 40, 1090);
  int ChecklistHeight = Min(WorkAreaHeightScaled - 30, 700);
  // 0 means no "custom-pos"
  FSynchronizeChecklist.WindowParams = L"0;" + FormatDefaultWindowParams(ChecklistWidth, ChecklistHeight);
  FSynchronizeChecklist.ListParams = L"1;1|150,1;100,1;80,1;130,1;25,1;100,1;80,1;130,1;@" + SaveDefaultPixelsPerInch() + L"|0;1;2;3;4;5;6;7";
  FFindFile.WindowParams = FormatDefaultWindowSize(646, 481);
  FFindFile.ListParams = L"3;1|125,1;181,1;80,1;122,1;@" + SaveDefaultPixelsPerInch() + L"|0;1;2;3";
  FConsoleWin.WindowSize = FormatDefaultWindowSize(570, 430);
  FLoginDialog.WindowSize = FormatDefaultWindowSize(640, 430);
  FLoginDialog.SiteSearch = isName;
  FConfirmExitOnCompletion = true;
  FSynchronizeSummary = true;
  FSessionColors = L"";
  FFontColors = L"";
  FCopyShortCutHintShown = false;
  FHttpForWebDAV = false;

  DefaultHistory();
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::Saved()
{
  TGUIConfiguration::Saved();

  THistoryStrings * HistoryStrings;
  for (int Index = 0; Index < FHistory->Count; Index++)
  {
    HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[Index]);
    DebugAssert(HistoryStrings != NULL);
    HistoryStrings->Modified = false;
  }
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(L".>")+1, ELEM.Length() - ELEM.LastDelimiter(L".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE, true)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(CANCREATE) \
  BLOCK(L"Interface", CANCREATE, \
    KEY(Integer,  Interface); \
    KEY(Bool,     ConfirmExitOnCompletion); \
    KEY(Bool,     SynchronizeSummary); \
    KEY(String,   SessionColors); \
    KEY(String,   FontColors); \
    KEY(Bool,     CopyShortCutHintShown); \
    KEY(Bool,     HttpForWebDAV); \
  ) \
  BLOCK(L"Interface\\SynchronizeChecklist", CANCREATE, \
    KEY(String,   SynchronizeChecklist.WindowParams); \
    KEY(String,   SynchronizeChecklist.ListParams); \
  ); \
  BLOCK(L"Interface\\FindFile", CANCREATE, \
    KEY(String,   FindFile.WindowParams); \
    KEY(String,   FindFile.ListParams); \
  ); \
  BLOCK(L"Interface\\ConsoleWin", CANCREATE, \
    KEY(String,   ConsoleWin.WindowSize); \
  ); \
  BLOCK(L"Interface\\LoginDialog", CANCREATE, \
    KEY(String,   LoginDialog.WindowSize); \
    KEY(Integer,  LoginDialog.SiteSearch); \
  ); \
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SaveData(
  THierarchicalStorage * Storage, bool All)
{
  TGUIConfiguration::SaveData(Storage, All);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) Storage->Write ## TYPE(LASTELEM(UnicodeString(TEXT(#VAR))), VAR)
  REGCONFIG(true);
  #undef KEY

  if (FHistory->Count > 0)
  {
    if (Storage->OpenSubKey(L"History", true))
    {
      try
      {
        THistoryStrings * HistoryStrings;
        for (int Index = 0; Index < FHistory->Count; Index++)
        {
          HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[Index]);
          DebugAssert(HistoryStrings != NULL);
          if (All || HistoryStrings->Modified)
          {
            if (Storage->OpenSubKey(FHistory->Strings[Index], true))
            {
              try
              {
                Storage->WriteValues(HistoryStrings);
              }
              __finally
              {
                Storage->CloseSubKey();
              }
            }
          }
        }
      }
      __finally
      {
        Storage->CloseSubKey();
      }
    }

    if (Storage->OpenSubKey(L"HistoryParams", true))
    {
      try
      {
        THistoryStrings * HistoryStrings;
        for (int Index = 0; Index < FHistory->Count; Index++)
        {
          HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[Index]);
          DebugAssert(HistoryStrings != NULL);
          if (All || HistoryStrings->Modified)
          {
            bool HasData = false;
            for (int VIndex = 0; !HasData && (VIndex < HistoryStrings->Count); VIndex++)
            {
              HasData = (HistoryStrings->Objects[VIndex] != NULL);
            }

            if (!HasData)
            {
              Storage->RecursiveDeleteSubKey(FHistory->Strings[Index]);
            }
            else if (Storage->OpenSubKey(FHistory->Strings[Index], true))
            {
              try
              {
                Storage->ClearValues();
                for (int VIndex = 0; VIndex < HistoryStrings->Count; VIndex++)
                {
                  void * Data = HistoryStrings->Objects[VIndex];
                  Storage->WriteBinaryData(IntToStr(VIndex), &Data, sizeof(Data));
                }
              }
              __finally
              {
                Storage->CloseSubKey();
              }
            }
          }
        }
      }
      __finally
      {
        Storage->CloseSubKey();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::LoadData(
  THierarchicalStorage * Storage)
{
  TGUIConfiguration::LoadData(Storage);

  FAppliedInterface = FInterface;

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(LASTELEM(UnicodeString(TEXT(#VAR))), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEY

  DefaultHistory();
  if (Storage->OpenSubKey(L"History", false))
  {
    TStrings * Names = NULL;
    try
    {
      Names = new TStringList();
      Storage->GetSubKeyNames(Names);
      for (int Index = 0; Index < Names->Count; Index++)
      {
        if (Storage->OpenSubKey(Names->Strings[Index], false))
        {
          THistoryStrings * HistoryStrings = NULL;
          try
          {
            // remove defaults, if any
            int HIndex = FHistory->IndexOf(Names->Strings[Index]);
            if (HIndex >= 0)
            {
              THistoryStrings * DefaultStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[HIndex]);
              delete DefaultStrings;
              FHistory->Delete(HIndex);
            }

            HistoryStrings = new THistoryStrings();
            Storage->ReadValues(HistoryStrings);
            FHistory->AddObject(Names->Strings[Index], HistoryStrings);
            HistoryStrings = NULL;
          }
          __finally
          {
            Storage->CloseSubKey();
            delete HistoryStrings;
          }
        }
      }
    }
    __finally
    {
      Storage->CloseSubKey();
      delete Names;
    }
  }

  if (Storage->OpenSubKey(L"HistoryParams", false))
  {
    try
    {
      THistoryStrings * HistoryStrings;
      for (int Index = 0; Index < FHistory->Count; Index++)
      {
        HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[Index]);
        if (Storage->OpenSubKey(FHistory->Strings[Index], false))
        {
          try
          {
            for (int VIndex = 0; VIndex < HistoryStrings->Count; VIndex++)
            {
              void * Data;
              if (Storage->ReadBinaryData(IntToStr(VIndex), &Data, sizeof(Data)) ==
                      sizeof(Data))
              {
                HistoryStrings->Objects[VIndex] = reinterpret_cast<TObject *>(Data);
              }
            }
          }
          __finally
          {
            Storage->CloseSubKey();
          }
        }
      }
    }
    __finally
    {
      Storage->CloseSubKey();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  TGUIConfiguration::LoadAdmin(Storage);
  FDefaultInterface = TInterface(Storage->ReadInteger(L"DefaultInterfaceInterface", FDefaultInterface));
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::RecryptPasswords(TStrings * RecryptPasswordErrors)
{
  TOperationVisualizer Visualizer;

  StoredSessions->RecryptPasswords(RecryptPasswordErrors);

  if (OnMasterPasswordRecrypt != NULL)
  {
    try
    {
      OnMasterPasswordRecrypt(NULL);
    }
    catch (Exception & E)
    {
      UnicodeString Message;
      if (ExceptionMessage(&E, Message))
      {
        // we do not expect this really to happen,
        // so we do not bother providing context
        RecryptPasswordErrors->Add(Message);
      }
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::AskForMasterPasswordIfNotSetAndNeededToPersistSessionData(
  TSessionData * SessionData)
{
  if (!DisablePasswordStoring &&
      SessionData->HasAnyPassword() &&
      UseMasterPassword)
  {
    AskForMasterPasswordIfNotSet();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetInterface(TInterface value)
{
  SET_CONFIG_PROPERTY(Interface);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetHistory(const UnicodeString Index,
  TStrings * value)
{
  int I = FHistory->IndexOf(Index);
  bool NonEmpty = (value != NULL) && (value->Count > 0);
  THistoryStrings * HistoryStrings = NULL;
  if (I >= 0)
  {
    HistoryStrings = dynamic_cast<THistoryStrings *>(FHistory->Objects[I]);
    if (HistoryStrings->Equals(value))
    {
      HistoryStrings = NULL;
    }
  }
  else if (NonEmpty)
  {
    HistoryStrings = new THistoryStrings();
    FHistory->AddObject(Index, HistoryStrings);
  }

  if (HistoryStrings != NULL)
  {
    if (NonEmpty)
    {
      HistoryStrings->Assign(value);
      while (HistoryStrings->Count > MaxHistoryCount)
      {
        HistoryStrings->Delete(HistoryStrings->Count - 1);
      }
    }
    else
    {
      HistoryStrings->Clear();
    }
    HistoryStrings->Modified = true;
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCustomWinConfiguration::GetHistory(const UnicodeString Index)
{
  int I = FHistory->IndexOf(Index);
  return I >= 0 ? dynamic_cast<TStrings *>(FHistory->Objects[I]) : FEmptyHistory;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomWinConfiguration::GetValidHistoryKey(UnicodeString Key)
{
  for (int Index = 1; Index <= Key.Length(); Index++)
  {
    if (!IsLetter(Key[Index]) && !IsDigit(Key[Index]))
    {
      Key[Index] = L'_';
    }
  }

  while (!Key.IsEmpty() && (Key[1] == L'_'))
  {
    Key.Delete(1, 1);
  }

  while (!Key.IsEmpty() && (Key[Key.Length()] == L'_'))
  {
    Key.Delete(Key.Length(), 1);
  }

  int P;
  while ((P = Key.Pos(L"__")) > 0)
  {
    Key.Delete(P, 1);
  }

  return Key;
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetSynchronizeChecklist(TSynchronizeChecklistConfiguration value)
{
  SET_CONFIG_PROPERTY(SynchronizeChecklist);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetFindFile(TFindFileConfiguration value)
{
  SET_CONFIG_PROPERTY(FindFile);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetConsoleWin(TConsoleWinConfiguration value)
{
  SET_CONFIG_PROPERTY(ConsoleWin);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetLoginDialog(TLoginDialogConfiguration value)
{
  SET_CONFIG_PROPERTY(LoginDialog);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetConfirmExitOnCompletion(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmExitOnCompletion);
}
//---------------------------------------------------------------------------
void __fastcall TCustomWinConfiguration::SetSynchronizeSummary(bool value)
{
  SET_CONFIG_PROPERTY(SynchronizeSummary);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomWinConfiguration::GetDefaultFixedWidthFontName()
{
  // These are defaults for respective version of Windows Notepad
  UnicodeString Result;
  if (IsWin8())
  {
    Result = L"Consolas";
  }
  else
  {
    Result = L"Lucida Console";
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TCustomWinConfiguration::GetDefaultFixedWidthFontSize()
{
  // These are defaults for respective version of Windows Notepad
  int Result;
  if (IsWin8())
  {
    Result = 11;
  }
  else
  {
    Result = 10;
  }
  return Result;
}
