//---------------------------------------------------------------------------
#ifndef SecurityH
#define SecurityH
//---------------------------------------------------------------------------
#define PWALG_SIMPLE 1
#define PWALG_SIMPLE_MAGIC 0xA3
#define PWALG_SIMPLE_STRING ((AnsiString)"0123456789ABCDEF")
#define PWALG_SIMPLE_MAXLEN 50
#define PWALG_SIMPLE_FLAG 0xFF
AnsiString EncryptPassword(AnsiString Password, AnsiString Key, Integer Algorithm = PWALG_SIMPLE);
AnsiString DecryptPassword(AnsiString Password, AnsiString Key, Integer Algorithm = PWALG_SIMPLE);
//---------------------------------------------------------------------------
#endif
