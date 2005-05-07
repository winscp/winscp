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
  TImageList *SessionImages;
  TImageList *QueueImages;
  TImageList *LogImages;
  TImageList *ArrowImages;
  
public:
  __fastcall TGlyphsModule(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGlyphsModule * GlyphsModule;
//---------------------------------------------------------------------------
#endif
