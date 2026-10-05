//---------------------------------------------------------------------------
#ifndef DriveViewH
#define DriveViewH
//---------------------------------------------------------------------------
#include "DriveViewInt.hpp"
//---------------------------------------------------------------------------
class TDriveView : public TDriveViewInt
{
public:
  __fastcall TDriveView(TComponent * AOwner);

  virtual TDriveStatus * __fastcall GetDriveStatus(UnicodeString Drive);

protected:
  // Using TStringList as it's easy to iterate with int iterator for GetNextDriveStatus.
  // Once all code is moved here, we can replace it easily with std::map.
  std::unique_ptr<TStringList> FDriveStatus;

  virtual bool __fastcall GetNextDriveStatus(int & Iterator, UnicodeString * Drive, TDriveStatus *& Status);
  virtual void __fastcall CreateWnd();
  virtual void __fastcall ReadSubDirs(TTreeNode * Node);
  virtual bool __fastcall DoScanDir(TTreeNode * FromNode);
  virtual void __fastcall AddChildNode(TTreeNode * ParentNode, UnicodeString ParentPath, const TSearchRec & SRec);

private:
  TDriveStatus * GetDriveStatus(int Index);
};
//---------------------------------------------------------------------------
#endif
