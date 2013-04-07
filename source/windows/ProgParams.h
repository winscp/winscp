//---------------------------------------------------------------------------
#ifndef ProgParamsH
#define ProgParamsH

#include <Option.h>
//---------------------------------------------------------------------------
class TProgramParams : public TOptions
{
public:
  static TProgramParams * __fastcall Instance();

  __fastcall TProgramParams();
  __fastcall TProgramParams(const UnicodeString & CmdLine);

private:
  void __fastcall Init(const UnicodeString & CmdLine);
};
//---------------------------------------------------------------------------
#endif
