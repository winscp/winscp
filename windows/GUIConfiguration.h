//---------------------------------------------------------------------------
#ifndef GUIConfigurationH
#define GUIConfigurationH
//---------------------------------------------------------------------------
#include "Configuration.h"
//---------------------------------------------------------------------------
class TGUIConfiguration : public TConfiguration
{
private:
  bool FCopyParamDialogExpanded;
  bool FErrorDialogExpanded;

protected:
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);

public:
  __fastcall TGUIConfiguration();
  __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();

  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
};
//---------------------------------------------------------------------------
#define GUIConfiguration ((TGUIConfiguration *) Configuration)
//---------------------------------------------------------------------------
#endif
