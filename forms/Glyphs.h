//---------------------------------------------------------------------------
#ifndef GlyphsH
#define GlyphsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TGlyphsModule : public TDataModule
{
__published:
  TImageList *ExplorerImages;
  TImageList *ExplorerDisabledImages;
  TImageList *SessionImages;
  TImageList *QueueImages;
  TImageList *LogImages;
  TImageList *LogDisabledImages;
  TImageList *ArrowImages;
  
public:
  __fastcall TGlyphsModule(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
