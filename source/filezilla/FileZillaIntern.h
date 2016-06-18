//---------------------------------------------------------------------------
#ifndef FileZillaInternH
#define FileZillaInternH
//---------------------------------------------------------------------------
class TFileZillaIntf;
//---------------------------------------------------------------------------
class TFileZillaIntern : public CApiLog
{
public:
  TFileZillaIntern(TFileZillaIntf * AOwner);

  virtual BOOL PostMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) const;

  inline const TFileZillaIntf * GetOwner() const { return FOwner; }

protected:
  TFileZillaIntf * FOwner;
};
//---------------------------------------------------------------------------
#endif // FileZillaInternH
