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
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wold-style-cast"
extern "C"
{
#include <putty.h>
// To rename ssh1_cipheralg::new member, what is a keyword in C++
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wkeyword-macro"
#define new _new_
#pragma clang diagnostic pop
#include <ssh.h>
#undef new
#include <puttyexp.h>
#include <proxy\proxy.h>
#include <storage.h>
// Defined in misc.h - Conflicts with std::min/max
#undef min
#undef max
// Defined in marshal.h - Conflicts with xml.xmldom.hpp
#undef get_data
}
#pragma clang diagnostic pop
//---------------------------------------------------------------------------
UnicodeString GetCipherName(const ssh_cipher * Cipher);
UnicodeString GetCompressorName(const ssh_compressor * Compressor);
UnicodeString GetDecompressorName(const ssh_decompressor * Decompressor);
void PuttyDefaults(Conf * conf);
int GetCipherGroup(const ssh_cipher * TheCipher);
//---------------------------------------------------------------------------
class TSecureShell;
struct ScpSeat : public Seat
{
  TSecureShell * SecureShell;

  ScpSeat(TSecureShell * SecureShell);
};
//---------------------------------------------------------------------------
extern std::unique_ptr<TCriticalSection> PuttyStorageSection;
extern THierarchicalStorage * PuttyStorage;
//---------------------------------------------------------------------------
#endif
