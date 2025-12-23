//---------------------------------------------------------------------------
#ifndef DirViewH
#define DirViewH
//---------------------------------------------------------------------------
#include "DirViewInt.hpp"
#include <set>
#include <queue>
#include <map>
//---------------------------------------------------------------------------
class TDirView : public TDirViewInt
{
public:
  __fastcall TDirView(TComponent * AOwner);

protected:
  virtual void __fastcall IconUpdateClear();
  virtual void __fastcall IconUpdateEnqueue(TListItem * ListItem);
  virtual int __fastcall IconUpdatePeek();
  virtual bool __fastcall IconUpdateDeprioritize(Dirviewint::TFileRec * ItemData, int Index);
  virtual void __fastcall IconUpdateDequeue(int Index);
  virtual void __fastcall PathChanged();
  virtual bool __fastcall TryGetLastPath(UnicodeString Drive, UnicodeString & Path);

private:
  std::set<int> FIconUpdateSet;
  typedef std::queue<TIconUpdateSchedule> TIconUpdateQueue;
  TIconUpdateQueue FIconUpdateQueue;
  TIconUpdateQueue FIconUpdateQueueDeferred;
  std::map<UnicodeString, UnicodeString> FLastPaths;
};
//---------------------------------------------------------------------------
#endif
