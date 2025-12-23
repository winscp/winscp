//---------------------------------------------------------------------------
#ifndef IEDriveInfoH
#define IEDriveInfoH
//---------------------------------------------------------------------------
#include "IEDriveInfoInt.hpp"
#include <map>
//---------------------------------------------------------------------------
class TDriveInfo : public TDriveInfoInt
{
public:
  __fastcall TDriveInfo();

protected:
  virtual TDriveInfoRec * __fastcall AddDrive(UnicodeString Drive);
  virtual TDriveInfoRec * __fastcall GetInternal(UnicodeString Drive);
  virtual TDriveInfoRec * __fastcall Get(UnicodeString Drive);
  virtual void __fastcall DriveRemoving(THandle DeviceHandle);
  virtual void __fastcall UpdateDrivesNotifications();

private:
  std::map<UnicodeString, std::unique_ptr<TDriveInfoRec>> FData;
};
//---------------------------------------------------------------------------
void DriveInfoRequire();
//---------------------------------------------------------------------------
#endif
