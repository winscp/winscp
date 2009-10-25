//---------------------------------------------------------------------------
#ifndef PuttyToolsH
#define PuttyToolsH
//---------------------------------------------------------------------------
enum TKeyType { ktUnopenable, ktUnknown, ktSSH1, ktSSH2, ktOpenSSH, ktSSHCom };
TKeyType KeyType(AnsiString FileName);
AnsiString KeyTypeName(TKeyType KeyType);
//---------------------------------------------------------------------------
AnsiString __fastcall DecodeUTF(const AnsiString UTF);
AnsiString __fastcall EncodeUTF(const WideString Source);
//---------------------------------------------------------------------------
__int64 __fastcall ParseSize(AnsiString SizeStr);
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI();
//---------------------------------------------------------------------------
void __fastcall AES256EncodeWithMAC(char * Data, size_t Len, const char * Password,
  size_t PasswordLen, const char * Salt);
//---------------------------------------------------------------------------
#endif
