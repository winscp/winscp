//---------------------------------------------------------------------------
#ifndef UsageH
#define UsageH

#include <map>
#include "HierarchicalStorage.h"
//---------------------------------------------------------------------------
class TConfiguration;
//---------------------------------------------------------------------------
class TUsage
{
public:
  __fastcall TUsage(TConfiguration * Configuration);
  virtual __fastcall ~TUsage();

  void __fastcall Set(const UnicodeString & Key, const UnicodeString & Value);
  void __fastcall Set(const UnicodeString & Key, int Value);
  void __fastcall Set(const UnicodeString & Key, bool Value);
  UnicodeString __fastcall Get(const UnicodeString & Key);
  void __fastcall Inc(const UnicodeString & Key);
  void __fastcall SetMax(const UnicodeString & Key, int Value);

  void __fastcall UpdateCurrentVersion();
  void __fastcall Reset();

  void __fastcall Default();
  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;
  UnicodeString __fastcall Serialize() const;

  __property bool Collect = { read = FCollect, write = SetCollect };

private:
  typedef std::map<UnicodeString, int> TCounters;
  TCriticalSection * FCriticalSection;
  TConfiguration * FConfiguration;
  TCounters FPeriodCounters;
  TCounters FLifetimeCounters;
  TStringList * FValues;
  bool FCollect;

  void __fastcall SetCollect(bool value);
  void __fastcall UpdateLastReport();
  void __fastcall Load(THierarchicalStorage * Storage,
    const UnicodeString & Name, TCounters & Counters);
  void __fastcall Save(THierarchicalStorage * Storage,
    const UnicodeString & Name, const TCounters & Counters) const;
  void __fastcall Inc(const UnicodeString & Key, TCounters & Counters);
  void __fastcall SetMax(const UnicodeString & Key, int Value, TCounters & Counters);
  void __fastcall Serialize(UnicodeString& List,
    const UnicodeString & Name, const TCounters & Counters) const;
};
//---------------------------------------------------------------------------
#endif
