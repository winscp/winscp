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

  static UnicodeString __fastcall FormatSwitch(const UnicodeString & Switch);

private:
  void __fastcall Init(const UnicodeString & CmdLine);
};
//---------------------------------------------------------------------------
#endif
