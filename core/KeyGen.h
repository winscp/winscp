//---------------------------------------------------------------------------
#ifndef KeyGenH
#define KeyGenH
//---------------------------------------------------------------------------
enum TKeyType { ktRSA1, ktRSA2, ktDSA };
typedef unsigned TEntropyBit;
class TKeyGenerationThread;
class TKeyGenerator;
enum TKeyGeneratorState { kgInitializing, kgInitialized, kgGenerating, kgComplete };
enum TKeyGenerationComplete { kgInProgress, kgSuccess, kgFailure };
enum TKeyFormat { kfPutty, kfOpenSSH, kfSSHCom };
typedef void __fastcall (__closure *TKeyGeneratorGenerating)
  (TKeyGenerator * Generator, int Range, int Position, TKeyGenerationComplete Complete);
//---------------------------------------------------------------------------
struct ssh2_userkey;
struct RSAKey;
struct dss_key;
//---------------------------------------------------------------------------
class TKeyGenerator : public TObject
{
friend class TKeyGenerationThread;
private:
  AnsiString FComment;
  int FKeySize;
  TKeyType FKeyType;
  TEntropyBit * FEntropy;
  int FEntropyGot;
  int FEntropyRequired;
  AnsiString FFingerprint;
  TKeyGeneratorState FState;
  int FGenerationRange;
  int FGenerationPosition;
  TKeyGeneratorGenerating FOnGenerating;
  AnsiString FPublicKey;
  TKeyGenerationThread * FThread;
  AnsiString __fastcall GetAuthorizedKeysLine();
  AnsiString __fastcall GetFingerprint();
  bool __fastcall GetIsSSH2();
  struct ssh2_userkey * FSSH2Key;
  int __fastcall GetPercentGenerated();
  AnsiString __fastcall GetPublicKey();
  void __fastcall SetComment(AnsiString value);
  void __fastcall SetKeySize(int value);
  void __fastcall SetKeyType(TKeyType value);
protected:
  // both written by TKeyGenerationThread (from different thread)
  RSAKey * FRSAKey;
  dss_key * FDSSKey;
  // called by TKeyGenerationThread (from main VCL thread)
  void __fastcall ProgressUpdate(int Range, int Position, TKeyGenerationComplete Complete);
  void __fastcall ResetKey();
public:
  __fastcall TKeyGenerator();
  virtual __fastcall ~TKeyGenerator();
  void __fastcall AddEntropy(TEntropyBit Entropy);
  void __fastcall Generate();
  void __fastcall SaveKey(const AnsiString FileName,
    const AnsiString Passphrase, TKeyFormat Format);
  void __fastcall StartGenerationThread();
  __property AnsiString AuthorizedKeysLine = { read = GetAuthorizedKeysLine };
  __property AnsiString Comment = { read = FComment, write = SetComment };
  __property int EntropyGot = { read = FEntropyGot };
  __property int EntropyRequired = { read = FEntropyRequired };
  __property AnsiString Fingerprint = { read = GetFingerprint };
  __property bool IsSSH2 = { read = GetIsSSH2 };
  __property TKeyGeneratorState State = { read = FState };

  __property int KeySize = { read = FKeySize, write = SetKeySize };
  __property TKeyType KeyType = { read = FKeyType, write = SetKeyType };
  __property TKeyGeneratorGenerating OnGenerating = { read = FOnGenerating, write = FOnGenerating };
  __property int PercentGenerated = { read = GetPercentGenerated };
  __property AnsiString PublicKey = { read = GetPublicKey };
};
//---------------------------------------------------------------------------
#endif
