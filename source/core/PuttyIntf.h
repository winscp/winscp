//---------------------------------------------------------------------------
#ifndef PuttyIntfH
#define PuttyIntfH
//---------------------------------------------------------------------------
void __fastcall PuttyInitialize();
void __fastcall PuttyFinalize();
//---------------------------------------------------------------------------
void __fastcall DontSaveRandomSeed();
//---------------------------------------------------------------------------
#include "PuttyTools.h"
//---------------------------------------------------------------------------
#define MPEXT
extern "C"
{
#include <putty.h>
#include <puttyexp.h>
// To rename ssh1_cipheralg::new member, what is a keyword in C++
#define new _new_
#include <ssh.h>
#undef new
#include <proxy.h>
#include <storage.h>
// Defined in misc.h - Conflicts with std::min/max
#undef min
#undef max
// Defined in marshal.h - Conflicts with xml.xmldom.hpp
#undef get_data
}
//---------------------------------------------------------------------------
UnicodeString GetCipher1Name(const ssh1_cipher * Cipher);
UnicodeString GetCipher2Name(const ssh2_cipher * Cipher);
UnicodeString GetCompressorName(const ssh_compressor * Compressor);
UnicodeString GetDecompressorName(const ssh_decompressor * Decompressor);
//---------------------------------------------------------------------------
class TSecureShell;
struct ScpSeat : public Seat
{
  TSecureShell * SecureShell;

  ScpSeat(TSecureShell * SecureShell);
};
//---------------------------------------------------------------------------
#endif
