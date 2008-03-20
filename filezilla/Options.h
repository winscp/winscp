//---------------------------------------------------------------------------
#ifndef OptionsH
#define OptionsH
//---------------------------------------------------------------------------
#ifndef MPEXT_NO_SPEED_LIM_RULES
#include <SpeedLimit.h>
#endif
//---------------------------------------------------------------------------
class COptions
{
public:
#ifndef MPEXT_NO_SPEED_LIM_RULES
  static CCriticalSection m_Sync; //  Moved to public section - needed for locking list
  static SPEEDLIMITSLIST m_DownloadSpeedLimits;
  static SPEEDLIMITSLIST m_UploadSpeedLimits;
#endif

  static CString GetInstanceOption(CApiLog * Instance, int OptionID);
  static int GetInstanceOptionVal(CApiLog * Instance, int OptionID);
};
//---------------------------------------------------------------------------
#endif
