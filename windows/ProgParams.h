//---------------------------------------------------------------------------
#ifndef ProgParamsH
#define ProgParamsH

#include <Option.h>
//---------------------------------------------------------------------------
class TProgramParams : public TOptions
{
public:
  static TProgramParams * __fastcall Instance();

  TProgramParams();
  ~TProgramParams();

private:
  static TProgramParams * SInstance;
};
//---------------------------------------------------------------------------
#endif
