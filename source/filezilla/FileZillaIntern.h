//---------------------------------------------------------------------------
#ifndef FileZillaInternH
#define FileZillaInternH
//---------------------------------------------------------------------------
class TFileZillaIntf;
//---------------------------------------------------------------------------
class TFileZillaIntern
{
public:
  TFileZillaIntern(TFileZillaIntf * AOwner);

  bool PostMessage(WPARAM wParam, LPARAM lParam) const;
  CString GetOption(int OptionID) const;
  int GetOptionVal(int OptionID) const;

  inline const TFileZillaIntf * GetOwner() const { return FOwner; }

  int GetDebugLevel() const;
  void SetDebugLevel(int DebugLevel);

protected:
  TFileZillaIntf * FOwner;
  int FDebugLevel;
};
//---------------------------------------------------------------------------
#endif // FileZillaInternH
