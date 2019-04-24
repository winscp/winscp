//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

// CompThread.pas must be linked to project
#include <CompThread.hpp>
#define THREAD_CLASS TCompThread
#else
#include <Classes.hpp>
#define THREAD_CLASS TThread
#endif

#include <Common.h>
#include "TextsCore.h"
#include <PuttyIntf.h>
#include "KeyGen.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
extern "C" void KeyGenerationProgressUpdate(void * Thread,
    int Action, int Phase, int IProgress);
//---------------------------------------------------------------------------
class TKeyGenerationThread : public THREAD_CLASS
{
public:
  #define PROGRESSRANGE 65535
  #define MAXPHASE 5
  struct
  {
    int NPhases;
    struct
    {
      bool Exponential;
      unsigned StartPoint, Total;
      unsigned Param, Current, N;   /* if exponential */
      unsigned Mult;                    /* if linear */
    } Phases[MAXPHASE];
    unsigned Total, Divisor, Range;
    unsigned Position;
    TKeyGenerationComplete Complete;
  } Progress;

  TKeyGenerator * FGenerator;

  __fastcall TKeyGenerationThread(TKeyGenerator * AGenerator):
    THREAD_CLASS(true)
  {
    FGenerator = AGenerator;
    Resume();
  }

  void __fastcall DistributeProgressUpdate()
  {
    FGenerator->ProgressUpdate(Progress.Range, Progress.Position, Progress.Complete);
  }

  void __fastcall ProgressUpdate(int Action, int Phase, unsigned IProgress)
  {
    int Position;

    if (Action < PROGFN_READY && Progress.NPhases < Phase)
        Progress.NPhases = Phase;

    switch (Action)
    {
      case PROGFN_INITIALISE:
        Progress.NPhases = 0;
        Progress.Complete = kgInProgress;
        break;

      case PROGFN_LIN_PHASE:
        Progress.Phases[Phase-1].Exponential = false;
        Progress.Phases[Phase-1].Mult = Progress.Phases[Phase].Total / IProgress;
        break;

      case PROGFN_EXP_PHASE:
        Progress.Phases[Phase-1].Exponential = true;
        Progress.Phases[Phase-1].Param = 0x10000 + IProgress;
        Progress.Phases[Phase-1].Current = Progress.Phases[Phase-1].Total;
        Progress.Phases[Phase-1].N = 0;
        break;

      case PROGFN_PHASE_EXTENT:
        Progress.Phases[Phase-1].Total = IProgress;
        break;

      case PROGFN_READY:
        {
          unsigned Total = 0;
          int i;
          for (i = 0; i < Progress.NPhases; i++)
          {
            Progress.Phases[i].StartPoint = Total;
            Total += Progress.Phases[i].Total;
          }
          Progress.Total = Total;
          Progress.Divisor = ((Progress.Total + PROGRESSRANGE - 1) / PROGRESSRANGE);
          Progress.Range = Progress.Total / Progress.Divisor;

          Synchronize(DistributeProgressUpdate);
        }
        break;

      case PROGFN_PROGRESS:
        if (Progress.Phases[Phase-1].Exponential)
        {
          while (Progress.Phases[Phase-1].N < IProgress)
          {
            Progress.Phases[Phase-1].N++;
            Progress.Phases[Phase-1].Current *= Progress.Phases[Phase-1].Param;
            Progress.Phases[Phase-1].Current /= 0x10000;
          }
          Position = (Progress.Phases[Phase-1].StartPoint +
            Progress.Phases[Phase-1].Total - Progress.Phases[Phase-1].Current);
        }
        else
        {
          Position = (Progress.Phases[Phase-1].StartPoint +
            IProgress * Progress.Phases[Phase-1].Mult);
        }
        Progress.Position = Position / Progress.Divisor;
        Synchronize(DistributeProgressUpdate);
        break;
    }
  }

  virtual void __fastcall Execute()
  {
    try
    {
      ProgressUpdate(PROGFN_INITIALISE, 0, 0);

      if (FGenerator->KeyType == ktDSA)
      {
        dsa_generate(FGenerator->FDSSKey, FGenerator->KeySize,
          KeyGenerationProgressUpdate, this);
      }
      else
      {
        rsa_generate(FGenerator->FRSAKey, FGenerator->KeySize,
          KeyGenerationProgressUpdate, this);
      }
      Progress.Complete = kgSuccess;
    }
    catch(...)
    {
      Progress.Complete = kgFailure;
    }
    Synchronize(DistributeProgressUpdate);
  }
#pragma warn +lvc
};
//---------------------------------------------------------------------------
void KeyGenerationProgressUpdate(void * Thread,
  int Action, int Phase, int IProgress)
{
  DebugAssert(Thread);
  ((TKeyGenerationThread*)Thread)->ProgressUpdate(Action, Phase, IProgress);
}
//---------------------------------------------------------------------------
__fastcall TKeyGenerator::TKeyGenerator():
  TObject()
{
  FSSH2Key = new ssh2_userkey;
  FRSAKey = new RSAKey;
  FDSSKey = new dss_key;
  FEntropy = NULL;
  FState = kgInitializing;
  FOnGenerating = NULL;
  FGenerationRange = -1;
  KeyType = ktRSA2;
  KeySize = 1024;
}
//---------------------------------------------------------------------------
__fastcall TKeyGenerator::~TKeyGenerator()
{
  DebugAssert(FState != kgGenerating);
  if (FEntropy) delete FEntropy;
  delete FSSH2Key;
  delete FRSAKey;
  delete FDSSKey;
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::SetKeySize(int value)
{
  DebugAssert(FState != kgGenerating);
  FState = kgInitializing;
  FKeySize = value;
  FEntropyRequired = (KeySize / 2) * 2;
  FEntropyGot = 0;
  if (FEntropy) delete FEntropy;
  FEntropy = new TEntropyBit[FEntropyRequired];
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::SetKeyType(TKeyType value)
{
  DebugAssert(FState != kgGenerating);
  FState = kgInitializing;
  FKeyType = value;
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::AddEntropy(TEntropyBit Entropy)
{
  DebugAssert(FState == kgInitializing);
  DebugAssert(FEntropy);
  DebugAssert(FEntropyGot < FEntropyRequired);
  FEntropy[FEntropyGot++] = Entropy;
  if (FEntropyGot == FEntropyRequired)
  {
    FState = kgInitialized;
    random_add_heavynoise(FEntropy, FEntropyRequired * sizeof(TEntropyBit));
    delete FEntropy;
    FEntropy = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::ResetKey()
{
  DebugAssert(FSSH2Key);
  switch (KeyType) {
    case ktDSA:
      FSSH2Key->data = FDSSKey;
      FSSH2Key->alg = &ssh_dss;
      break;

    case ktRSA2:
      FSSH2Key->data = FRSAKey;
      FSSH2Key->alg = &ssh_rsa;
      break;
  }

  FFingerprint = "";
  FPublicKey = "";

  if (Comment.IsEmpty())
  {
    Comment = FORMAT("%s-key-%s",
      ((KeyType == ktDSA ? "dsa" : "rsa"), FormatDateTime("yyyymmdd", Now())));
  }
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::StartGenerationThread()
{
  DebugAssert(FState == kgInitialized);
  FState = kgGenerating;
  new TKeyGenerationThread(this);
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::Generate()
{
  DebugAssert(FState == kgInitialized);
  THREAD_CLASS * Thread;
  FState = kgGenerating;
  Thread = new TKeyGenerationThread(this);
  Thread->WaitFor();
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::ProgressUpdate(int Range, int Position,
  TKeyGenerationComplete Complete)
{
  DebugAssert(FState == kgGenerating);
  if (Complete == kgSuccess)
  {
    FState = kgComplete;
    ResetKey();
  }
  FGenerationRange = Range;
  FGenerationPosition = Position;
  if (FOnGenerating) FOnGenerating(this, Range, Position, Complete);
}
//---------------------------------------------------------------------------
int __fastcall TKeyGenerator::GetPercentGenerated()
{
  switch (FState) {
    case kgComplete: return 100;
    case kgGenerating: return (FGenerationPosition * 100) / FGenerationRange;
    default: return 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::SetComment(AnsiString value)
{
  if (FComment != value)
  {
    FComment = value;
    FPublicKey = "";
    DebugAssert(FSSH2Key);
    FSSH2Key->comment = FComment.c_str();
    FRSAKey->comment = FComment.c_str();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TKeyGenerator::GetFingerprint()
{
  if (FFingerprint.IsEmpty())
  {
    if (IsSSH2)
    {
      DebugAssert(FSSH2Key);
      FFingerprint = FSSH2Key->alg->fingerprint(FSSH2Key->data);
    }
    else
    {
      char Buf[128];
      rsa_fingerprint(Buf, sizeof(Buf), FRSAKey);
      FFingerprint = Buf;
    }
  }
  return FFingerprint;
}
//---------------------------------------------------------------------------
bool __fastcall TKeyGenerator::GetIsSSH2()
{
  return (KeyType == ktDSA || KeyType == ktRSA2);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TKeyGenerator::GetPublicKey()
{
  if (FPublicKey.IsEmpty())
  {
    unsigned char *pub_blob;
    char *buffer, *p;
    int pub_len;
    int i;

    DebugAssert(FSSH2Key);
    pub_blob = FSSH2Key->alg->public_blob(FSSH2Key->data, &pub_len);
    buffer = new char[4 * ((pub_len + 2) / 3) + 1];
    p = buffer;
    i = 0;
    while (i < pub_len)
    {
      int n = (pub_len - i < 3 ? pub_len - i : 3);
      base64_encode_atom(pub_blob + i, n, p);
      i += n;
      p += 4;
    }
    *p = '\0';
    FPublicKey = buffer;
    sfree(pub_blob);
    delete [] buffer;
  }
  return FPublicKey;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TKeyGenerator::GetAuthorizedKeysLine()
{
  DebugAssert(FSSH2Key);
  return FORMAT("%s %s %s", (FSSH2Key->alg->name, PublicKey, Comment));
}
//---------------------------------------------------------------------------
void __fastcall TKeyGenerator::SaveKey(const AnsiString FileName,
  const AnsiString Passphrase, TKeyFormat Format)
{
  DebugAssert(FSSH2Key);
  DebugAssert(FState == kgComplete);
  DebugAssert((Format != kfOpenSSH && Format != kfSSHCom) || IsSSH2);

  int Result;

  if (IsSSH2)
  {
    switch (Format)
    {
      case kfPutty:
        Result = ssh2_save_userkey(FileName.c_str(), FSSH2Key,
          (char*)Passphrase.data());
        break;

      case kfOpenSSH:
        Result = export_ssh2(FileName.c_str(), SSH_KEYTYPE_OPENSSH, FSSH2Key,
          (char*)Passphrase.data());
        break;

      case kfSSHCom:
        Result = export_ssh2(FileName.c_str(), SSH_KEYTYPE_SSHCOM, FSSH2Key,
          (char*)Passphrase.data());
        break;

      default:
        DebugFail();
    }
  }
  else
  {
    DebugAssert(Format == kfPutty);
    Result = saversakey(FileName.c_str(), FRSAKey,
      (char*)Passphrase.data());
  }

  if (Result <= 0)
    throw Exception(FMTLOAD(SAVE_KEY_ERROR, (FileName)));
}
