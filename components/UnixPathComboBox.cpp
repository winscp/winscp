//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "UnixPathComboBox.h"

#ifndef DESIGN_ONLY
#include <RemoteFiles.h>
#endif

#include <Common.h>

#include <CustomDirView.hpp>
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
static inline void ValidCtrCheck(TUnixPathComboBox *)
{
  new TUnixPathComboBox(NULL);
}
//---------------------------------------------------------------------------
namespace Unixpathcombobox
{
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[1] = {__classid(TUnixPathComboBox)};
    RegisterComponents("Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
// TUnixPathComboBox
//---------------------------------------------------------------------------
__fastcall TUnixPathComboBox::TUnixPathComboBox(TComponent* Owner)
        : TCustomPathComboBox(Owner)
{
  UseSystemImageList = True;
  FRootName = DEFAULT_ROOTNAME;
  FPath = '/';
  ResetItemHeight();

  TSHFileInfo SFI;
  SHGetFileInfo("dummy", FILE_ATTRIBUTE_NORMAL | FILE_ATTRIBUTE_DIRECTORY,
    &SFI, sizeof(SFI), SHGFI_SYSICONINDEX | SHGFI_USEFILEATTRIBUTES);
  FCloseImage = SFI.iIcon;
  SHGetFileInfo("dummy", FILE_ATTRIBUTE_NORMAL | FILE_ATTRIBUTE_DIRECTORY,
    &SFI, sizeof(SFI), SHGFI_SYSICONINDEX | SHGFI_USEFILEATTRIBUTES | SHGFI_OPENICON);
  FOpenImage = SFI.iIcon;
}
//---------------------------------------------------------------------------
Integer __fastcall TUnixPathComboBox::GetItemImage(Integer Index)
{
  if (Index < Items->Count-1) return FCloseImage;
    else return FOpenImage;
}
//---------------------------------------------------------------------------
Integer __fastcall TUnixPathComboBox::GetItemIndent(Integer Index)
{
  return (10 * Index);
}
//---------------------------------------------------------------------------
void __fastcall TUnixPathComboBox::SetRootName(AnsiString value)
{
  if (FRootName != value)
  {
    FRootName = value;
    ResetItems();
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TUnixPathComboBox::IsRootNameStored()
{
  return (FRootName != DEFAULT_ROOTNAME);
}
//---------------------------------------------------------------------------
void __fastcall TUnixPathComboBox::ResetItems()
{
#ifndef DESIGN_ONLY
  Items->BeginUpdate();
  try {
    Items->Clear();
    AnsiString APath = UnixExcludeTrailingBackslash(Path);
    while (APath.Length() > 1)
    {
      Integer P = APath.LastDelimiter('/');
      assert(P >= 0);
      Items->Insert(0, APath.SubString(P + 1, APath.Length() - P));
      APath.SetLength(P - 1);
    }
    Items->Insert(0, RootName);
  } __finally {
    ItemIndex = Items->Count - 1;
    Items->EndUpdate();
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixPathComboBox::CreateWnd()
{
  TCustomPathComboBox::CreateWnd();
  ResetItems();
}
//---------------------------------------------------------------------------
void __fastcall TUnixPathComboBox::SetPath(AnsiString Value)
{
#ifndef DESIGN_ONLY
  if (!Value.IsEmpty())
  {
    Value = UnixIncludeTrailingBackslash(Value);
    if (Value != FPath)
    {
      FPath = Value;
      ResetItems();
    }
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixPathComboBox::PathChanged()
{
#ifndef DESIGN_ONLY
  AnsiString APath = UnixExcludeTrailingBackslash(Path);
  for (Integer Index = ItemIndex; Index < Items->Count - 1; Index++)
    APath = UnixExtractFileDir(APath);
  Path = APath;
  TCustomPathComboBox::PathChanged();
  // in case that path was not changed (e.g. inaccessible directory)
  Path = ((TCustomDirView*)DirView)->Path;
#endif
}


