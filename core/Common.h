//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH

#ifndef C_ONLY
//---------------------------------------------------------------------------
#define CATCH(command) \
  try {command;} catch (Exception &E) {ShowExtendedException(&E);}
#define EXCEPTION throw ExtException(NULL, "")
#define SCOPY(dest, source) \
  strncpy(dest, source, sizeof(dest)); \
  dest[sizeof(dest)-1] = '\0'
#define SAFE_DESTROY(OBJ) { TObject * PObj = OBJ; OBJ = NULL; delete PObj; }
#define ASCOPY(dest, source) SCOPY(dest, source.c_str())
#define FORMAT(S, F) Format(S, ARRAYOFCONST(F))
#define FMTLOAD(I, F) FmtLoadStr(I, ARRAYOFCONST(F))
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
//---------------------------------------------------------------------------
extern const char EngShortMonthNames[12][4];
//---------------------------------------------------------------------------
AnsiString ReplaceChar(AnsiString Str, Char A, Char B);
AnsiString DeleteChar(AnsiString Str, Char C);
void PackStr(AnsiString &Str);
AnsiString GetTemporaryPath();
AnsiString MakeValidFileName(AnsiString FileName);
AnsiString RootKeyToStr(HKEY RootKey);
AnsiString BooleanToStr(bool B);
AnsiString BooleanToEngStr(bool B);
AnsiString CutToChar(AnsiString &Str, Char Ch, bool Trim);
AnsiString ExceptionLogString(Exception *E);
bool IsDots(const AnsiString Str);
AnsiString __fastcall SystemTemporaryDirectory();
AnsiString __fastcall StripPathQuotes(const AnsiString Path);
AnsiString __fastcall AddPathQuotes(AnsiString Path);
void __fastcall SplitCommand(AnsiString Command, AnsiString &Program,
  AnsiString & Params, AnsiString & Dir);
AnsiString __fastcall FormatCommand(AnsiString Program, AnsiString Params);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TProcessLocalFileEvent)
  (const AnsiString FileName, const TSearchRec Rec, void * Param);
bool __fastcall FileSearchRec(const AnsiString FileName, TSearchRec & Rec);
void __fastcall ProcessLocalDirectory(AnsiString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param = NULL, int FindAttrs = -1);
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
#include <assert.h>
#ifndef _DEBUG
#undef assert
#define assert(p)   ((void)0)
#endif
#define USEDPARAM(p) ((p) == (p))
//---------------------------------------------------------------------------
#endif
