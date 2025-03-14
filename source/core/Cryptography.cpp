//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "PuttyIntf.h"
#include "Cryptography.h"
#include "FileBuffer.h"
#include "TextsCore.h"
#include "CoreMain.h"
#include "Exceptions.h"
#include <openssl\rand.h>
#include <openssl\err.h>
#include <openssl\ssl.h>
#include <process.h>
#include <Soap.EncdDecd.hpp>
#include <System.StrUtils.hpp>

/*
 ---------------------------------------------------------------------------
 Copyright (c) 2002, Dr Brian Gladman <brg@gladman.me.uk>, Worcester, UK.
 All rights reserved.

 LICENSE TERMS

 The free distribution and use of this software in both source and binary
 form is allowed (with or without changes) provided that:

   1. distributions of this source code include the above copyright
      notice, this list of conditions and the following disclaimer;

   2. distributions in binary form include the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other associated materials;

   3. the copyright holder's name is not used to endorse products
      built using this software without specific written permission.

 ALTERNATIVELY, provided that this notice is retained in full, this product
 may be distributed under the terms of the GNU General Public License (GPL),
 in which case the provisions of the GPL apply INSTEAD OF those given above.

 DISCLAIMER

 This software is provided 'as is' with no explicit or implied warranties
 in respect of its properties, including, but not limited to, correctness
 and/or fitness for purpose.
 -------------------------------------------------------------------------

 This file implements password based file encryption and authentication
 using AES in CTR mode, HMAC-SHA1 authentication and RFC2898 password
 based key derivation.

 This is an implementation of HMAC, the FIPS standard keyed hash function

*/

#include <memory.h>

#define sha1_begin(ctx)           ctx = (&ssh_sha1)->_new_(&ssh_sha1)
#define sha1_hash(buf, len, ctx)  put_data(ctx, buf, len)
#define sha1_end(dig, ctx)        ssh_hash_final(ctx, dig); ctx = NULL

#define IN_BLOCK_LENGTH     64
#define OUT_BLOCK_LENGTH    20
#define HMAC_IN_DATA        0xffffffff

struct hmac_ctx
{   unsigned char   key[IN_BLOCK_LENGTH];
    ssh_hash       *ctx;
    unsigned int    klen;
    hmac_ctx()
    {
        memset(this, 0, sizeof(*this));
    }
    ~hmac_ctx()
    {
        if (ctx != NULL) ssh_hash_free(ctx);
    }
    void CopyFrom(hmac_ctx * Source)
    {
        if (ctx != NULL)
        {
            ssh_hash_free(ctx);
        }
        memmove(this, Source, sizeof(*this));
        if (Source->ctx != NULL)
        {
            ctx = ssh_hash_copy(Source->ctx);
        }
    }
};

/* initialise the HMAC context to zero */
static void hmac_sha1_begin(hmac_ctx cx[1])
{
    memset(cx, 0, sizeof(hmac_ctx));
}

/* input the HMAC key (can be called multiple times)    */
static void hmac_sha1_key(const unsigned char key[], unsigned long key_len, hmac_ctx cx[1])
{
    if(cx->klen + key_len > IN_BLOCK_LENGTH)    /* if the key has to be hashed  */
    {
        if(cx->klen <= IN_BLOCK_LENGTH)         /* if the hash has not yet been */
        {                                       /* started, initialise it and   */
            sha1_begin(cx->ctx);                /* hash stored key characters   */
            sha1_hash(cx->key, cx->klen, cx->ctx);
        }

        sha1_hash(const_cast<unsigned char *>(key), key_len, cx->ctx);       /* hash long key data into hash */
    }
    else                                        /* otherwise store key data     */
        memcpy(cx->key + cx->klen, key, key_len);

    cx->klen += key_len;                        /* update the key length count  */
}

/* input the HMAC data (can be called multiple times) - */
/* note that this call terminates the key input phase   */
static void hmac_sha1_data(const unsigned char data[], unsigned long data_len, hmac_ctx cx[1])
{   unsigned int i;

    if(cx->klen != HMAC_IN_DATA)                /* if not yet in data phase */
    {
        if(cx->klen > IN_BLOCK_LENGTH)          /* if key is being hashed   */
        {                                       /* complete the hash and    */
            sha1_end(cx->key, cx->ctx);         /* store the result as the  */
            cx->klen = OUT_BLOCK_LENGTH;        /* key and set new length   */
        }

        /* pad the key if necessary */
        memset(cx->key + cx->klen, 0, IN_BLOCK_LENGTH - cx->klen);

        /* xor ipad into key value  */
        for(i = 0; i < (IN_BLOCK_LENGTH >> 2); ++i)
            ((unsigned long*)cx->key)[i] ^= 0x36363636;

        /* and start hash operation */
        sha1_begin(cx->ctx);
        sha1_hash(cx->key, IN_BLOCK_LENGTH, cx->ctx);

        /* mark as now in data mode */
        cx->klen = HMAC_IN_DATA;
    }

    /* hash the data (if any)       */
    if(data_len)
        sha1_hash(const_cast<unsigned char *>(data), data_len, cx->ctx);
}

/* compute and output the MAC value */
static void hmac_sha1_end(unsigned char mac[], unsigned long mac_len, hmac_ctx cx[1])
{   unsigned char dig[OUT_BLOCK_LENGTH];
    unsigned int i;

    /* if no data has been entered perform a null data phase        */
    if(cx->klen != HMAC_IN_DATA)
        hmac_sha1_data((const unsigned char*)0, 0, cx);

    sha1_end(dig, cx->ctx);         /* complete the inner hash      */

    /* set outer key value using opad and removing ipad */
    for(i = 0; i < (IN_BLOCK_LENGTH >> 2); ++i)
        ((unsigned long*)cx->key)[i] ^= 0x36363636 ^ 0x5c5c5c5c;

    /* perform the outer hash operation */
    sha1_begin(cx->ctx);
    sha1_hash(cx->key, IN_BLOCK_LENGTH, cx->ctx);
    sha1_hash(dig, OUT_BLOCK_LENGTH, cx->ctx);
    sha1_end(dig, cx->ctx);

    /* output the hash value            */
    for(i = 0; i < mac_len; ++i)
        mac[i] = dig[i];
}

#define BLOCK_SIZE  16

void aes_set_encrypt_key(const unsigned char in_key[], unsigned int klen, void * cx)
{
  call_aes_setup(cx, const_cast<unsigned char *>(in_key), klen);
}

void aes_encrypt_block(const unsigned char in_blk[], unsigned char out_blk[], void * cx)
{
  int Index;
  memmove(out_blk, in_blk, BLOCK_SIZE);
  for (Index = 0; Index < 4; Index++)
  {
    unsigned char t;
    t = out_blk[Index * 4 + 0];
    out_blk[Index * 4 + 0] = out_blk[Index * 4 + 3];
    out_blk[Index * 4 + 3] = t;
    t = out_blk[Index * 4 + 1];
    out_blk[Index * 4 + 1] = out_blk[Index * 4 + 2];
    out_blk[Index * 4 + 2] = t;
  }
  call_aesold_encrypt(cx, reinterpret_cast<unsigned int*>(out_blk));
  for (Index = 0; Index < 4; Index++)
  {
    unsigned char t;
    t = out_blk[Index * 4 + 0];
    out_blk[Index * 4 + 0] = out_blk[Index * 4 + 3];
    out_blk[Index * 4 + 3] = t;
    t = out_blk[Index * 4 + 1];
    out_blk[Index * 4 + 1] = out_blk[Index * 4 + 2];
    out_blk[Index * 4 + 2] = t;
  }
}

typedef struct
{   unsigned char   nonce[BLOCK_SIZE];          /* the CTR nonce          */
    unsigned char   encr_bfr[BLOCK_SIZE];       /* encrypt buffer         */
    AESContext *    encr_ctx;                   /* encryption context     */
    hmac_ctx        auth_ctx;                   /* authentication context */
    unsigned int    encr_pos;                   /* block position (enc)   */
    unsigned int    pwd_len;                    /* password length        */
    unsigned int    mode;                       /* File encryption mode   */
} fcrypt_ctx;

#define MAX_KEY_LENGTH        32
#define KEYING_ITERATIONS   1000
#define PWD_VER_LENGTH         2

/*
    Field lengths (in bytes) versus File Encryption Mode (0 < mode < 4)

    Mode Key Salt  MAC Overhead
       1  16    8   10       18
       2  24   12   10       22
       3  32   16   10       26

   The following macros assume that the mode value is correct.
*/

#define KEY_LENGTH(mode)        (8 * (mode & 3) + 8)
#define SALT_LENGTH(mode)       (4 * (mode & 3) + 4)
#define MAC_LENGTH(mode)        (10)

/* subroutine for data encryption/decryption    */
/* this could be speeded up a lot by aligning   */
/* buffers and using 32 bit operations          */

static void derive_key(const unsigned char pwd[],  /* the PASSWORD     */
               unsigned int pwd_len,        /* and its length   */
               const unsigned char salt[],  /* the SALT and its */
               unsigned int salt_len,       /* length           */
               unsigned int iter,   /* the number of iterations */
               unsigned char key[], /* space for the output key */
               unsigned int key_len)/* and its required length  */
{
    unsigned int    i, j, k, n_blk;
    unsigned char uu[OUT_BLOCK_LENGTH], ux[OUT_BLOCK_LENGTH];
    hmac_ctx c1[1], c2[1], c3[1];

    /* set HMAC context (c1) for password               */
    hmac_sha1_begin(c1);
    hmac_sha1_key(pwd, pwd_len, c1);

    /* set HMAC context (c2) for password and salt      */
    c2->CopyFrom(c1);
    hmac_sha1_data(salt, salt_len, c2);

    /* find the number of SHA blocks in the key         */
    n_blk = 1 + (key_len - 1) / OUT_BLOCK_LENGTH;

    for(i = 0; i < n_blk; ++i) /* for each block in key */
    {
        /* ux[] holds the running xor value             */
        memset(ux, 0, OUT_BLOCK_LENGTH);

        /* set HMAC context (c3) for password and salt  */
        c3->CopyFrom(c2);

        /* enter additional data for 1st block into uu  */
        uu[0] = (unsigned char)((i + 1) >> 24);
        uu[1] = (unsigned char)((i + 1) >> 16);
        uu[2] = (unsigned char)((i + 1) >> 8);
        uu[3] = (unsigned char)(i + 1);

        /* this is the key mixing iteration         */
        for(j = 0, k = 4; j < iter; ++j)
        {
            /* add previous round data to HMAC      */
            hmac_sha1_data(uu, k, c3);

            /* obtain HMAC for uu[]                 */
            hmac_sha1_end(uu, OUT_BLOCK_LENGTH, c3);

            /* xor into the running xor block       */
            for(k = 0; k < OUT_BLOCK_LENGTH; ++k)
                ux[k] ^= uu[k];

            /* set HMAC context (c3) for password   */
            c3->CopyFrom(c1);
        }

        /* compile key blocks into the key output   */
        j = 0; k = i * OUT_BLOCK_LENGTH;
        while(j < OUT_BLOCK_LENGTH && k < key_len)
            key[k++] = ux[j++];
    }
}

static void encr_data(unsigned char data[], unsigned long d_len, fcrypt_ctx cx[1])
{
    unsigned long i = 0, pos = cx->encr_pos;

    while(i < d_len)
    {
        if(pos == BLOCK_SIZE)
        {   unsigned int j = 0;
            /* increment encryption nonce   */
            while(j < 8 && !++cx->nonce[j])
                ++j;
            /* encrypt the nonce to form next xor buffer    */
            aes_encrypt_block(cx->nonce, cx->encr_bfr, cx->encr_ctx);
            pos = 0;
        }

        data[i++] ^= cx->encr_bfr[pos++];
    }

    cx->encr_pos = pos;
}

static void fcrypt_init(
    int mode,                               /* the mode to be used (input)          */
    const unsigned char pwd[],              /* the user specified password (input)  */
    unsigned int pwd_len,                   /* the length of the password (input)   */
    const unsigned char salt[],             /* the salt (input)                     */
    fcrypt_ctx      cx[1])                  /* the file encryption context (output) */
{
    unsigned char kbuf[2 * MAX_KEY_LENGTH + PWD_VER_LENGTH];

    cx->mode = mode;
    cx->pwd_len = pwd_len;

    /* derive the encryption and authetication keys and the password verifier   */
    derive_key(pwd, pwd_len, salt, SALT_LENGTH(mode), KEYING_ITERATIONS,
                        kbuf, 2 * KEY_LENGTH(mode) + PWD_VER_LENGTH);

    /* initialise the encryption nonce and buffer pos   */
    cx->encr_pos = BLOCK_SIZE;
    /* if we need a random component in the encryption  */
    /* nonce, this is where it would have to be set     */
    memset(cx->nonce, 0, BLOCK_SIZE * sizeof(unsigned char));

    /* initialise for encryption using key 1            */
    cx->encr_ctx = aesold_make_context();
    call_aesold_setup(cx->encr_ctx, BLOCK_SIZE, kbuf, KEY_LENGTH(mode));

    /* initialise for authentication using key 2        */
    hmac_sha1_begin(&cx->auth_ctx);
    hmac_sha1_key(kbuf + KEY_LENGTH(mode), KEY_LENGTH(mode), &cx->auth_ctx);
}

/* perform 'in place' encryption and authentication */

static void fcrypt_encrypt(unsigned char data[], unsigned int data_len, fcrypt_ctx cx[1])
{
    encr_data(data, data_len, cx);
    hmac_sha1_data(data, data_len, &cx->auth_ctx);
}

/* perform 'in place' authentication and decryption */

static void fcrypt_decrypt(unsigned char data[], unsigned int data_len, fcrypt_ctx cx[1])
{
    hmac_sha1_data(data, data_len, &cx->auth_ctx);
    encr_data(data, data_len, cx);
}

/* close encryption/decryption and return the MAC value */

static int fcrypt_end(unsigned char mac[], fcrypt_ctx cx[1])
{
    hmac_sha1_end(mac, MAC_LENGTH(cx->mode), &cx->auth_ctx);
    aesold_free_context(cx->encr_ctx);
    return MAC_LENGTH(cx->mode);    /* return MAC length in bytes   */
}
//---------------------------------------------------------------------------
#define PASSWORD_MANAGER_AES_MODE 3
//---------------------------------------------------------------------------
static void AES256Salt(RawByteString & Salt)
{
  Salt.SetLength(SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
  RAND_bytes(reinterpret_cast<unsigned char *>(Salt.c_str()), Salt.Length());
}
//---------------------------------------------------------------------------
RawByteString GenerateEncryptKey()
{
  RawByteString Result;
  Result.SetLength(KEY_LENGTH(PASSWORD_MANAGER_AES_MODE));
  RAND_bytes(reinterpret_cast<unsigned char *>(Result.c_str()), Result.Length());
  return Result;
}
//---------------------------------------------------------------------------
void ValidateEncryptKey(const RawByteString & Key)
{
  int Len = KEY_LENGTH(PASSWORD_MANAGER_AES_MODE);
  if (Key.Length() != Len)
  {
    throw Exception(FMTLOAD(INVALID_ENCRYPT_KEY, (L"AES-256", Len, Len * 2)));
  }
}
//---------------------------------------------------------------------------
void __fastcall AES256EncyptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString & Salt, RawByteString & Output, RawByteString & Mac)
{
  fcrypt_ctx aes;
  if (Salt.IsEmpty())
  {
    AES256Salt(Salt);
  }
  DebugAssert(Salt.Length() == SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
  UTF8String UtfPassword = Password;
  fcrypt_init(PASSWORD_MANAGER_AES_MODE,
    reinterpret_cast<const unsigned char *>(UtfPassword.c_str()), UtfPassword.Length(),
    reinterpret_cast<const unsigned char *>(Salt.c_str()), &aes);
  Output = Input;
  Output.Unique();
  fcrypt_encrypt(reinterpret_cast<unsigned char *>(Output.c_str()), Output.Length(), &aes);
  Mac.SetLength(MAC_LENGTH(PASSWORD_MANAGER_AES_MODE));
  fcrypt_end(reinterpret_cast<unsigned char *>(Mac.c_str()), &aes);
}
//---------------------------------------------------------------------------
void __fastcall AES256EncyptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString & Output)
{
  RawByteString Salt;
  RawByteString Encrypted;
  RawByteString Mac;
  AES256EncyptWithMAC(Input, Password, Salt, Encrypted, Mac);
  Output = Salt + Encrypted + Mac;
}
//---------------------------------------------------------------------------
bool __fastcall AES256DecryptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString Salt, RawByteString & Output, RawByteString Mac)
{
  fcrypt_ctx aes;
  DebugAssert(Salt.Length() == SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
  UTF8String UtfPassword = Password;
  fcrypt_init(PASSWORD_MANAGER_AES_MODE,
    reinterpret_cast<const unsigned char *>(UtfPassword.c_str()), UtfPassword.Length(),
    reinterpret_cast<const unsigned char *>(Salt.c_str()), &aes);
  Output = Input;
  Output.Unique();
  fcrypt_decrypt(reinterpret_cast<unsigned char *>(Output.c_str()), Output.Length(), &aes);
  RawByteString Mac2;
  Mac2.SetLength(MAC_LENGTH(PASSWORD_MANAGER_AES_MODE));
  DebugAssert(Mac.Length() == Mac2.Length());
  fcrypt_end(reinterpret_cast<unsigned char *>(Mac2.c_str()), &aes);
  return (Mac2 == Mac);
}
//---------------------------------------------------------------------------
bool __fastcall AES256DecryptWithMAC(RawByteString Input, UnicodeString Password,
  RawByteString & Output)
{
  bool Result =
    Input.Length() > SALT_LENGTH(PASSWORD_MANAGER_AES_MODE) + MAC_LENGTH(PASSWORD_MANAGER_AES_MODE);
  if (Result)
  {
    RawByteString Salt = Input.SubString(1, SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
    RawByteString Encrypted =
      Input.SubString(SALT_LENGTH(PASSWORD_MANAGER_AES_MODE) + 1,
        Input.Length() - SALT_LENGTH(PASSWORD_MANAGER_AES_MODE) - MAC_LENGTH(PASSWORD_MANAGER_AES_MODE));
    RawByteString Mac =
      Input.SubString(Input.Length() - MAC_LENGTH(PASSWORD_MANAGER_AES_MODE) + 1,
        MAC_LENGTH(PASSWORD_MANAGER_AES_MODE));
    Result = AES256DecryptWithMAC(Encrypted, Password, Salt, Output, Mac);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall AES256CreateVerifier(UnicodeString Input, RawByteString & Verifier)
{
  RawByteString Salt;
  RawByteString Dummy;
  AES256Salt(Dummy);

  RawByteString Encrypted;
  RawByteString Mac;
  AES256EncyptWithMAC(Dummy, Input, Salt, Encrypted, Mac);

  Verifier = Salt + Dummy + Mac;
}
//---------------------------------------------------------------------------
bool __fastcall AES256Verify(UnicodeString Input, RawByteString Verifier)
{
  int SaltLength = SALT_LENGTH(PASSWORD_MANAGER_AES_MODE);
  RawByteString Salt = Verifier.SubString(1, SaltLength);
  RawByteString Dummy = Verifier.SubString(SaltLength + 1, SaltLength);
  RawByteString Mac = Verifier.SubString(SaltLength + SaltLength + 1, MAC_LENGTH(PASSWORD_MANAGER_AES_MODE));

  RawByteString Encrypted;
  RawByteString Mac2;
  AES256EncyptWithMAC(Dummy, Input, Salt, Encrypted, Mac2);

  DebugAssert(Mac2.Length() == Mac.Length());

  return (Mac == Mac2);
}
//---------------------------------------------------------------------------
static unsigned char SScrambleTable[256] =
{
    0, 223, 235, 233, 240, 185,  88, 102,  22, 130,  27,  53,  79, 125,  66, 201,
   90,  71,  51,  60, 134, 104, 172, 244, 139,  84,  91,  12, 123, 155, 237, 151,
  192,   6,  87,  32, 211,  38, 149,  75, 164, 145,  52, 200, 224, 226, 156,  50,
  136, 190, 232,  63, 129, 209, 181, 120,  28,  99, 168,  94, 198,  40, 238, 112,
   55, 217, 124,  62, 227,  30,  36, 242, 208, 138, 174, 231,  26,  54, 214, 148,
   37, 157,  19, 137, 187, 111, 228,  39, 110,  17, 197, 229, 118, 246, 153,  80,
   21, 128,  69, 117, 234,  35,  58,  67,  92,   7, 132, 189,   5, 103,  10,  15,
  252, 195,  70, 147, 241, 202, 107,  49,  20, 251, 133,  76, 204,  73, 203, 135,
  184,  78, 194, 183,   1, 121, 109,  11, 143, 144, 171, 161,  48, 205, 245,  46,
   31,  72, 169, 131, 239, 160,  25, 207, 218, 146,  43, 140, 127, 255,  81,  98,
   42, 115, 173, 142, 114,  13,   2, 219,  57,  56,  24, 126,   3, 230,  47, 215,
    9,  44, 159,  33, 249,  18,  93,  95,  29, 113, 220,  89,  97, 182, 248,  64,
   68,  34,   4,  82,  74, 196, 213, 165, 179, 250, 108, 254,  59,  14, 236, 175,
   85, 199,  83, 106,  77, 178, 167, 225,  45, 247, 163, 158,   8, 221,  61, 191,
  119,  16, 253, 105, 186,  23, 170, 100, 216,  65, 162, 122, 150, 176, 154, 193,
  206, 222, 188, 152, 210, 243,  96,  41,  86, 180, 101, 177, 166, 141, 212, 116
};
//---------------------------------------------------------------------------
static unsigned char * ScrambleTable;
static unsigned char * UnscrambleTable;
//---------------------------------------------------------------------------
RawByteString __fastcall ScramblePassword(UnicodeString Password)
{
  #define SCRAMBLE_LENGTH_EXTENSION 50
  UTF8String UtfPassword = Password;
  int Len = UtfPassword.Length();
  char * Buf = new char[Len + SCRAMBLE_LENGTH_EXTENSION];
  int Padding = (((Len + 3) / 17) * 17 + 17) - 3 - Len;
  for (int Index = 0; Index < Padding; Index++)
  {
    int P = 0;
    while ((P <= 0) || (P > 255) || IsDigit(static_cast<wchar_t>(P)))
    {
      P = (int)((double)rand() / ((double)RAND_MAX / 256.0));
    }
    Buf[Index] = (unsigned char)P;
  }
  Buf[Padding] = (char)('0' + (Len % 10));
  Buf[Padding + 1] = (char)('0' + ((Len / 10) % 10));
  Buf[Padding + 2] = (char)('0' + ((Len / 100) % 10));
  strcpy(Buf + Padding + 3, UtfPassword.c_str());
  char * S = Buf;
  int Last = 31;
  while (*S != '\0')
  {
    Last = (Last + (unsigned char)*S) % 255 + 1;
    *S = ScrambleTable[Last];
    S++;
  }
  RawByteString Result = Buf;
  memset(Buf, 0, Len + SCRAMBLE_LENGTH_EXTENSION);
  delete[] Buf;
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall UnscramblePassword(RawByteString Scrambled, UnicodeString & Password)
{
  Scrambled.Unique();
  char * S = Scrambled.c_str();
  int Last = 31;
  while (*S != '\0')
  {
    int X = (int)UnscrambleTable[(unsigned char)*S] - 1 - (Last % 255);
    if (X <= 0)
    {
      X += 255;
    }
    *S = (char)X;
    Last = (Last + X) % 255 + 1;
    S++;
  }

  S = Scrambled.c_str();
  while ((*S != '\0') && ((*S < '0') || (*S > '9')))
  {
    S++;
  }
  bool Result = false;
  if (strlen(S) >= 3)
  {
    int Len = (S[0] - '0') + 10 * (S[1] - '0') + 100 * (S[2] - '0');
    int Total = (((Len + 3) / 17) * 17 + 17);
    if ((Len >= 0) && (Total == Scrambled.Length()) && (Total - (S - Scrambled.c_str()) - 3 == Len))
    {
      Scrambled.Delete(1, Scrambled.Length() - Len);
      Result = true;
    }
  }
  if (Result)
  {
    Password = UTF8ToString(Scrambled);
  }
  else
  {
    Password = L"";
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString OpensslInitializationErrors;
//---------------------------------------------------------------------------
static bool InitOpenssl()
{
  // RAND_poll already calls OPENSSL_init_crypto with OPENSSL_INIT_LOAD_CONFIG and other flags.
  // OPENSSL_init_ssl does not do much more, so it is not really big overhead.
  // And we need to call OPENSSL_init_ssl, as it we need to use OPENSSL_INIT_LOAD_SSL_STRINGS to match what SSL_CTX_new does
  // OPENSSL_init_ssl passes all flags to OPENSSL_init_crypto (even those it does not understand, like the very  OPENSSL_INIT_LOAD_SSL_STRINGS).
  // And OPENSSL_init_ssl caches the initialization results based on the flags
  ERR_clear_error();
  return OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS, NULL);
}
//---------------------------------------------------------------------------
void __fastcall CryptographyInitialize()
{
  ScrambleTable = SScrambleTable;
  UnscrambleTable = new unsigned char[256];
  for (int Index = 0; Index < 256; Index++)
  {
    UnscrambleTable[SScrambleTable[Index]] = (unsigned char)Index;
  }
  srand((unsigned int)time(NULL) ^ (unsigned int)getpid());

  // The results are partly cached. So when later some OpenSSL function is called, which internally
  // calls OPENSSL_init_crypto, it will fail, without doing full initialization. So afterwards
  // the ERR_get_error might return 0, even if the function itself returns failure.
  // So we have to remember here what went wrong.
  // But there seems to be an issue in OpenSSL that when OPENSSL_init_crypto is called again
  // with OPENSSL_INIT_LOAD_CONFIG, after the loading failed before, _from the same thread_, the
  // initialization succeeds. It seems to be because the in_init_config_local recursion fuest is never cleared.
  // So for example, is the configuration is invalid, the foreground updates check still work,
  // as the foreground thread always initialized OpenSSL here (via RAND_poll), and the later
  // OPENSSL_init_crypto from within updates TLS code succeeds. Similarly scripting TLS connections work.
  // But opening GUI TLS connections (WebDAV, S3...) fail, as they are opened on background thread,
  // and there the OPENSSL_init_crypto is first called from TLS connection.
  // Clean solution would be to fail any TLS connection,
  // if OPENSSL_init_crypto failed when called the first time, but that would be regression.
  // But let's be prepared that this happens if OpenSSL is ever fixed.
  if (!InitOpenssl())
  {
    OpensslInitializationErrors = GetTlsErrorStrs();
    AppLogFmt(L"OpenSSL initialization failed (possibly wrong configuration file) - TLS connections might be failing:\n%s", (OpensslInitializationErrors));
    char * ConfigPathBuf = CONF_get1_default_config_file();
    UnicodeString ConfigPath = UnicodeString(UTF8String(ConfigPathBuf));
    if (!ConfigPath.IsEmpty() && FileExists(ApiPath(ConfigPath)))
    {
      AppLogFmt(L"OpenSSL configuration file: %s", (ConfigPath));
    }
    OPENSSL_free(ConfigPathBuf);
  }
  else
  {
    AppLog(L"OpenSSL initialization succeeded");
  }

  RAND_poll();
}
//---------------------------------------------------------------------------
void __fastcall CryptographyFinalize()
{
  delete[] UnscrambleTable;
  UnscrambleTable = NULL;
  ScrambleTable = NULL;
}
//---------------------------------------------------------------------------
void RequireTls()
{
  if (!InitOpenssl())
  {
    UnicodeString Errors = DefaultStr(GetTlsErrorStrs(), OpensslInitializationErrors);
    throw ExtException(MainInstructions(LoadStr(OPENSSL_INIT_ERROR)), OpensslInitializationErrors);
  }
}
//---------------------------------------------------------------------------
int __fastcall PasswordMaxLength()
{
  return 128;
}
//---------------------------------------------------------------------------
int __fastcall IsValidPassword(UnicodeString Password)
{
  if (Password.IsEmpty() || (Password.Length() > PasswordMaxLength()))
  {
    return -1;
  }
  else
  {
    int A = 0;
    int B = 0;
    int C = 0;
    int D = 0;
    for (int Index = 1; Index <= Password.Length(); Index++)
    {
      if (IsLowerCaseLetter(Password[Index]))
      {
        A = 1;
      }
      else if (IsUpperCaseLetter(Password[Index]))
      {
        B = 1;
      }
      else if (IsDigit(Password[Index]))
      {
        C = 1;
      }
      else
      {
        D = 1;
      }
    }
    return (Password.Length() >= 6) && ((A + B + C + D) >= 2);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TEncryption::TEncryption(const RawByteString & Key)
{
  FKey = Key;
  FOutputtedHeader = false;
  if (!FKey.IsEmpty())
  {
    DebugAssert(FKey.Length() == KEY_LENGTH(PASSWORD_MANAGER_AES_MODE));
    FContext = aes_make_context();
    aes_set_encrypt_key(reinterpret_cast<unsigned char *>(FKey.c_str()), FKey.Length(), FContext);
  }
  else
  {
    FContext = NULL;
  }
}
//---------------------------------------------------------------------------
TEncryption::~TEncryption() EXCEPT
{
  if (FContext != NULL)
  {
    aes_free_context(FContext);
  }
  Shred(FKey);
  if ((FInputHeader.Length() > 0) && (FInputHeader.Length() < GetOverhead()))
  {
    throw Exception(LoadStr(UNKNOWN_FILE_ENCRYPTION));
  }
}
//---------------------------------------------------------------------------
void TEncryption::SetSalt()
{
  aes_iv(FContext, reinterpret_cast<const void *>(FSalt.c_str()));
}
//---------------------------------------------------------------------------
void TEncryption::NeedSalt()
{
  if (FSalt.IsEmpty())
  {
    AES256Salt(FSalt);
    SetSalt();
  }
}
//---------------------------------------------------------------------------
static UnicodeString AesCtrExt(L".aesctr.enc");
static RawByteString AesCtrMagic("aesctr.........."); // 16 bytes fixed [to match AES block size], even for future algos
//---------------------------------------------------------------------------
int TEncryption::RoundToBlock(int Size)
{
  int M = (Size % BLOCK_SIZE);
  if (M != 0)
  {
    Size += (BLOCK_SIZE - M);
  }
  return Size;
}
//---------------------------------------------------------------------------
int TEncryption::RoundToBlockDown(int Size)
{
  return Size - (Size % BLOCK_SIZE);
}
//---------------------------------------------------------------------------
void TEncryption::Aes(char * Buffer, int Size)
{
  DebugAssert(!FSalt.IsEmpty());
  call_aes_sdctr(reinterpret_cast<unsigned char *>(Buffer), Size, FContext);
}
//---------------------------------------------------------------------------
void TEncryption::Aes(TFileBuffer & Buffer, bool Last)
{
  if (!FOverflowBuffer.IsEmpty())
  {
    Buffer.Insert(0, FOverflowBuffer.c_str(), FOverflowBuffer.Length());
    FOverflowBuffer.SetLength(0);
  }

  int Size = 0; // shut up
  if (Last)
  {
    Size = Buffer.Size;
    Buffer.Size = RoundToBlock(Size);
  }
  else
  {
    int RoundedSize = RoundToBlockDown(Buffer.Size);
    if (RoundedSize != Buffer.Size)
    {
      FOverflowBuffer += RawByteString(Buffer.Data + RoundedSize, Buffer.Size - RoundedSize);
      Buffer.Size = RoundedSize;
    }
  }

  Aes(Buffer.Data, Buffer.Size);

  if (Last)
  {
    Buffer.Size = Size;
  }
}
//---------------------------------------------------------------------------
void TEncryption::Encrypt(TFileBuffer & Buffer, bool Last)
{
  NeedSalt();
  Aes(Buffer, Last);
  if (!FOutputtedHeader)
  {
    DebugAssert(AesCtrMagic.Length() == BLOCK_SIZE);
    RawByteString Header = AesCtrMagic + FSalt;
    DebugAssert(Header.Length() == GetOverhead());
    Buffer.Insert(0, Header.c_str(), Header.Length());
    FOutputtedHeader = true;
  }
}
//---------------------------------------------------------------------------
void TEncryption::Decrypt(TFileBuffer & Buffer)
{
  if (FInputHeader.Length() < GetOverhead())
  {
    int HeaderSize = std::min(GetOverhead() - FInputHeader.Length(), Buffer.Size);
    FInputHeader += RawByteString(Buffer.Data, HeaderSize);
    Buffer.Delete(0, HeaderSize);

    if (FInputHeader.Length() >= GetOverhead())
    {
      if (FInputHeader.SubString(1, AesCtrMagic.Length()) != AesCtrMagic)
      {
        throw Exception(LoadStr(UNKNOWN_FILE_ENCRYPTION));
      }

      FSalt = FInputHeader.SubString(AesCtrMagic.Length() + 1, SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
      SetSalt();
    }
  }

  if (Buffer.Size > 0)
  {
    Aes(Buffer, false);
  }
}
//---------------------------------------------------------------------------
bool TEncryption::DecryptEnd(TFileBuffer & Buffer)
{
  bool Result = !FOverflowBuffer.IsEmpty();
  if (Result)
  {
    Aes(Buffer, true);
  }
  return Result;
}
//---------------------------------------------------------------------------
void TEncryption::Aes(RawByteString & Buffer)
{
  int Size = Buffer.Length();
  Buffer.SetLength(RoundToBlock(Buffer.Length()));
  Aes(Buffer.c_str(), Buffer.Length());
  Buffer.SetLength(Size);
}
//---------------------------------------------------------------------------
UnicodeString TEncryption::EncryptFileName(const UnicodeString & FileName)
{
  NeedSalt();
  UTF8String FileNameUtf(FileName);
  RawByteString Buffer(FileNameUtf);
  Aes(Buffer);
  Buffer = FSalt + Buffer;
  UnicodeString Base64 = UnicodeString(EncodeBase64(Buffer.c_str(), Buffer.Length()));
  Base64 = ReplaceChar(Base64, L'/', L'_');
  Base64 = ReplaceStr(Base64, L"\r\n", "");
  while (DebugAlwaysTrue(!Base64.IsEmpty()) && (Base64.SubString(Base64.Length(), 1) == L'='))
  {
    Base64.SetLength(Base64.Length() - 1);
  }
  UnicodeString Result = Base64 + AesCtrExt;
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TEncryption::DecryptFileName(const UnicodeString & FileName)
{
  if (!IsEncryptedFileName(FileName))
  {
    throw Exception(L"Not an encrypted file name");
  }
  UnicodeString Base64 = ReplaceChar(LeftStr(FileName, FileName.Length() - AesCtrExt.Length()), L'_', L'/');
  int Padding = 4 - (Base64.Length() % 4);
  if ((Padding > 0) && (Padding < 4))
  {
    Base64 += UnicodeString::StringOfChar(L'=', Padding);
  }
  RawByteString Buffer = DecodeBase64ToStr(Base64);
  FSalt = Buffer.SubString(1, SALT_LENGTH(PASSWORD_MANAGER_AES_MODE));
  SetSalt();
  Buffer.Delete(1, FSalt.Length());
  Aes(Buffer);
  UnicodeString Result(UTF8ToString(Buffer));
  return Result;
}
//---------------------------------------------------------------------------
bool TEncryption::IsEncryptedFileName(const UnicodeString & FileName)
{
  return EndsStr(AesCtrExt, FileName);
}
//---------------------------------------------------------------------------
int TEncryption::GetOverhead()
{
  return AesCtrMagic.Length() + SALT_LENGTH(PASSWORD_MANAGER_AES_MODE);
}
//---------------------------------------------------------------------------
