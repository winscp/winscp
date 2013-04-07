//---------------------------------------------------------------------------
#ifndef PuttyToolsH
#define PuttyToolsH
//---------------------------------------------------------------------------
enum TKeyType { ktUnopenable, ktUnknown, ktSSH1, ktSSH2, ktOpenSSH, ktSSHCom };
TKeyType KeyType(UnicodeString FileName);
UnicodeString KeyTypeName(TKeyType KeyType);
//---------------------------------------------------------------------------
__int64 __fastcall ParseSize(UnicodeString SizeStr);
//---------------------------------------------------------------------------
bool __fastcall HasGSSAPI();
//---------------------------------------------------------------------------
void __fastcall AES256EncodeWithMAC(char * Data, size_t Len, const char * Password,
  size_t PasswordLen, const char * Salt);
//---------------------------------------------------------------------------
#endif
