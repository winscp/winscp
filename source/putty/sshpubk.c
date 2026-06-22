/*
 * Generic SSH public-key handling operations. In particular,
 * reading of SSH public-key files, and also the generic `sign'
 * operation for SSH-2 (which checks the type of the key and
 * dispatches to the appropriate key-type specific function).
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "putty.h"
#include "mpint.h"
#include "ssh.h"
#include "misc.h"

/*
 * Fairly arbitrary size limit on any public or private key blob.
 * Chosen to match AGENT_MAX_MSGLEN, on the basis that any key too
 * large to transfer over the ssh-agent protocol is probably too large
 * to be useful in general.
 *
 * MAX_KEY_BLOB_LINES is the corresponding limit on the Public-Lines
 * or Private-Lines header field in a key file.
 */
#define MAX_KEY_BLOB_SIZE 262144
#define MAX_KEY_BLOB_LINES (MAX_KEY_BLOB_SIZE / 48)

/*
 * Corresponding limit on the size of a key _file_ itself, based on
 * base64-encoding the key blob and then adding a few Kb for
 * surrounding metadata.
 */
#define MAX_KEY_FILE_SIZE (MAX_KEY_BLOB_SIZE * 4 / 3 + 4096)

static const ptrlen rsa1_signature =
    PTRLEN_DECL_LITERAL("SSH PRIVATE KEY FILE FORMAT 1.1\n\0");

#define BASE64_TOINT(x) ( (x)-'A'<26 ? (x)-'A'+0 :\
                          (x)-'a'<26 ? (x)-'a'+26 :\
                          (x)-'0'<10 ? (x)-'0'+52 :\
                          (x)=='+' ? 62 : \
                          (x)=='/' ? 63 : 0 )

LoadedFile *lf_new(size_t max_size)
{
    LoadedFile *lf = snew_plus(LoadedFile, max_size);
    lf->data = snew_plus_get_aux(lf);
    lf->len = 0;
    lf->max_size = max_size;
    return lf;
}

void lf_free(LoadedFile *lf)
{
    smemclr(lf->data, lf->max_size);
    smemclr(lf, sizeof(LoadedFile));
    sfree(lf);
}

LoadFileStatus lf_load_fp(LoadedFile *lf, FILE *fp)
{
    lf->len = 0;
    while (lf->len < lf->max_size) {
        size_t retd = fread(lf->data + lf->len, 1, lf->max_size - lf->len, fp);
        if (ferror(fp))
            return LF_ERROR;

        if (retd == 0)
            break;

        lf->len += retd;
    }

    { // WINSCP
    LoadFileStatus status = LF_OK;

    if (lf->len == lf->max_size) {
        /* The file might be too long to fit in our fixed-size
         * structure. Try reading one more byte, to check. */
        if (fgetc(fp) != EOF)
            status = LF_TOO_BIG;
    }

    BinarySource_INIT(lf, lf->data, lf->len);

    return status;
    } // WINSCP
}

LoadFileStatus lf_load(LoadedFile *lf, const Filename *filename)
{
    #ifdef WINSCP
    const char * data = in_memory_key_data(filename);
    if (data != NULL)
    {
        LoadFileStatus status = LF_OK;
        int len = strlen(data);
        char buf[3] = { '\0' };
        int i;
        for (i = 0; i < len; i += 2)
        {
            if (lf->len == lf->max_size)
            {
                status = LF_TOO_BIG;
                break;
            }
            buf[0] = data[i];
            buf[1] = data[i + 1];
            lf->data[lf->len] = strtol(buf, NULL, 16);
            lf->len++;
        }

        BinarySource_INIT(lf, lf->data, lf->len);
        return status;
    }
    #endif
    { // WINSCP
    FILE *fp = f_open(filename, "rb", false);
    if (!fp)
        return LF_ERROR;

    { // WINSCP
    LoadFileStatus status = lf_load_fp(lf, fp);
    fclose(fp);
    return status;
    } // WINSCP
    } // WINSCP
}

static inline bool lf_load_keyfile_helper(LoadFileStatus status,
                                          const char **errptr)
{
    const char *error;
    switch (status) {
      case LF_OK:
        return true;
      case LF_TOO_BIG:
        error = "file is too large to be a key file";
        break;
      case LF_ERROR:
        error = strerror(errno);
        break;
      default:
        unreachable("bad status value in lf_load_keyfile_helper");
    }
    if (errptr)
        *errptr = error;
    return false;
}

LoadedFile *lf_load_keyfile(const Filename *filename, const char **errptr)
{
    LoadedFile *lf = lf_new(MAX_KEY_FILE_SIZE);
    if (!lf_load_keyfile_helper(lf_load(lf, filename), errptr)) {
        lf_free(lf);
        return NULL;
    }
    return lf;
}

#ifndef WINSCP
/* This API does not support in-memory keys like lf_load, so make sure it's not in use */
LoadedFile *lf_load_keyfile_fp(FILE *fp, const char **errptr)
{
    LoadedFile *lf = lf_new(MAX_KEY_FILE_SIZE);
    if (!lf_load_keyfile_helper(lf_load_fp(lf, fp), errptr)) {
        lf_free(lf);
        return NULL;
    }
    return lf;
}
#endif

static bool expect_signature(BinarySource *src, ptrlen realsig)
{
    ptrlen thissig = get_data(src, realsig.len);
    return !get_err(src) && ptrlen_eq_ptrlen(realsig, thissig);
}

#ifndef WINSCP

static int rsa1_load_s_internal(BinarySource *src, RSAKey *key, bool pub_only,
                                char **commentptr, const char *passphrase,
                                const char **error)
{
    strbuf *buf = NULL;
    int ciphertype;
    int ret = 0;
    ptrlen comment;

    *error = "not an SSH-1 RSA file";

    if (!expect_signature(src, rsa1_signature))
        goto end;

    *error = "file format error";

    /* One byte giving encryption type, and one reserved uint32. */
    ciphertype = get_byte(src);
    if (ciphertype != 0 && ciphertype != SSH1_CIPHER_3DES)
        goto end;
    if (get_uint32(src) != 0)
        goto end;                 /* reserved field nonzero, panic! */

    /* Now the serious stuff. An ordinary SSH-1 public key. */
    get_rsa_ssh1_pub(src, key, RSA_SSH1_MODULUS_FIRST);

    /* Next, the comment field. */
    comment = get_string(src);
    if (commentptr)
        *commentptr = mkstr(comment);
    if (key)
        key->comment = mkstr(comment);

    if (pub_only) {
        ret = 1;
        goto end;
    }

    if (!key) {
        ret = ciphertype != 0;
        *error = NULL;
        goto end;
    }

    /*
     * Decrypt remainder of buffer.
     */
    if (ciphertype) {
        size_t enclen = get_avail(src);
        if (enclen & 7)
            goto end;

        buf = strbuf_dup_nm(get_data(src, enclen));

        { // WINSCP
        unsigned char keybuf[16];
        hash_simple(&ssh_md5, ptrlen_from_asciz(passphrase), keybuf);
        des3_decrypt_pubkey(keybuf, buf->u, enclen);
        smemclr(keybuf, sizeof(keybuf));        /* burn the evidence */

        BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(buf));
        } // WINSCP
    }

    /*
     * We are now in the secret part of the key. The first four
     * bytes should be of the form a, b, a, b.
     */
    {
        int b0a = get_byte(src);
        int b1a = get_byte(src);
        int b0b = get_byte(src);
        int b1b = get_byte(src);
        if (b0a != b0b || b1a != b1b) {
            *error = "wrong passphrase";
            ret = -1;
            goto end;
        }
    }

    /*
     * After that, we have one further bignum which is our
     * decryption exponent, and then the three auxiliary values
     * (iqmp, q, p).
     */
    get_rsa_ssh1_priv(src, key);
    key->iqmp = get_mp_ssh1(src);
    key->q = get_mp_ssh1(src);
    key->p = get_mp_ssh1(src);

    if (!rsa_verify(key)) {
        *error = "rsa_verify failed";
        freersakey(key);
        ret = 0;
    } else {
        *error = NULL;
        ret = 1;
    }

  end:
    if (buf)
        strbuf_free(buf);
    return ret;
}

int rsa1_load_s(BinarySource *src, RSAKey *key,
                const char *passphrase, const char **errstr)
{
    return rsa1_load_s_internal(src, key, false, NULL, passphrase, errstr);
}

int rsa1_load_f(const Filename *filename, RSAKey *key,
                const char *passphrase, const char **errstr)
{
    LoadedFile *lf = lf_load_keyfile(filename, errstr);
    if (!lf)
        return false;

    { // WINSCP
    int toret = rsa1_load_s(BinarySource_UPCAST(lf), key, passphrase, errstr);
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * See whether an RSA key is encrypted. Return its comment field as
 * well.
 */
bool rsa1_encrypted_s(BinarySource *src, char **comment)
{
    const char *dummy;
    return rsa1_load_s_internal(src, NULL, false, comment, NULL, &dummy) == 1;
}

bool rsa1_encrypted_f(const Filename *filename, char **comment)
{
    LoadedFile *lf = lf_load_keyfile(filename, NULL);
    if (!lf)
        return false; /* couldn't even open the file */

    { // WINSCP
    bool toret = rsa1_encrypted_s(BinarySource_UPCAST(lf), comment);
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * Read the public part of an SSH-1 RSA key from a file (public or
 * private), and generate its public blob in exponent-first order.
 */
int rsa1_loadpub_s(BinarySource *src, BinarySink *bs,
                   char **commentptr, const char **errorstr)
{
    RSAKey key;
    int ret;
    const char *error = NULL;

    /* Default return if we fail. */
    ret = 0;

    { // WINSCP
    bool is_privkey_file = expect_signature(src, rsa1_signature);
    BinarySource_REWIND(src);

    if (is_privkey_file) {
        /*
         * Load just the public half from an SSH-1 private key file.
         */
        memset(&key, 0, sizeof(key));
        if (rsa1_load_s_internal(src, &key, true, commentptr, NULL, &error)) {
            rsa_ssh1_public_blob(bs, &key, RSA_SSH1_EXPONENT_FIRST);
            freersakey(&key);
            ret = 1;
        }
    } else {
        /*
         * Try interpreting the file as an SSH-1 public key.
         */
        char *line, *p, *bitsp, *expp, *modp, *commentp;

        line = mkstr(get_chomped_line(src));
        p = line;

        bitsp = p;
        p += strspn(p, "0123456789");
        if (*p != ' ')
            goto not_public_either;
        *p++ = '\0';

        expp = p;
        p += strspn(p, "0123456789");
        if (*p != ' ')
            goto not_public_either;
        *p++ = '\0';

        modp = p;
        p += strspn(p, "0123456789");
        if (*p) {
            if (*p != ' ')
                goto not_public_either;
            *p++ = '\0';
            commentp = p;
        } else {
            commentp = NULL;
        }

        memset(&key, 0, sizeof(key));
        key.exponent = mp_from_decimal(expp);
        key.modulus = mp_from_decimal(modp);
        if (atoi(bitsp) != mp_get_nbits(key.modulus)) {
            mp_free(key.exponent);
            mp_free(key.modulus);
            sfree(line);
            error = "key bit count does not match in SSH-1 public key file";
            goto end;
        }
        if (commentptr)
            *commentptr = commentp ? dupstr(commentp) : NULL;
        rsa_ssh1_public_blob(bs, &key, RSA_SSH1_EXPONENT_FIRST);
        freersakey(&key);
        sfree(line);
        return 1;

      not_public_either:
        sfree(line);
        error = "not an SSH-1 RSA file";
    }

  end:
    if ((ret != 1) && errorstr)
        *errorstr = error;
    return ret;
    } // WINSCP
}

int rsa1_loadpub_f(const Filename *filename, BinarySink *bs,
                   char **commentptr, const char **errorstr)
{
    LoadedFile *lf = lf_load_keyfile(filename, errorstr);
    if (!lf)
        return 0;

    { // WINSCP
    int toret = rsa1_loadpub_s(BinarySource_UPCAST(lf), bs,
                               commentptr, errorstr);
    lf_free(lf);
    return toret;
    } // WINSCP
}

strbuf *rsa1_save_sb(RSAKey *key, const char *passphrase)
{
    strbuf *buf = strbuf_new_nm();
    int estart;

    /*
     * The public part of the key.
     */
    put_datapl(buf, rsa1_signature);
    put_byte(buf, passphrase ? SSH1_CIPHER_3DES : 0); /* encryption type */
    put_uint32(buf, 0);                              /* reserved */
    rsa_ssh1_public_blob(BinarySink_UPCAST(buf), key,
                         RSA_SSH1_MODULUS_FIRST);
    put_stringz(buf, NULLTOEMPTY(key->comment));

    /*
     * The encrypted portion starts here.
     */
    estart = buf->len;

    /*
     * Two bytes, then the same two bytes repeated.
     */
    {
        uint8_t bytes[2];
        random_read(bytes, 2);
        put_data(buf, bytes, 2);
        put_data(buf, bytes, 2);
    }

    /*
     * Four more bignums: the decryption exponent, then iqmp, then
     * q, then p.
     */
    put_mp_ssh1(buf, key->private_exponent);
    put_mp_ssh1(buf, key->iqmp);
    put_mp_ssh1(buf, key->q);
    put_mp_ssh1(buf, key->p);

    /*
     * Now write zeros until the encrypted portion is a multiple of
     * 8 bytes.
     */
    put_padding(buf, (estart - buf->len) & 7, 0);

    /*
     * Now encrypt the encrypted portion.
     */
    if (passphrase) {
        unsigned char keybuf[16];

        hash_simple(&ssh_md5, ptrlen_from_asciz(passphrase), keybuf);
        des3_encrypt_pubkey(keybuf, buf->u + estart, buf->len - estart);
        smemclr(keybuf, sizeof(keybuf));        /* burn the evidence */
    }

    return buf;
}

/*
 * Save an RSA key file. Return true on success.
 */
bool rsa1_save_f(const Filename *filename, RSAKey *key, const char *passphrase)
{
    FILE *fp = f_open(filename, "wb", true);
    if (!fp)
        return false;

    { // WINSCP
    strbuf *buf = rsa1_save_sb(key, passphrase);
    bool toret = fwrite(buf->s, 1, buf->len, fp) == buf->len;
    if (fclose(fp))
        toret = false;
    strbuf_free(buf);
    return toret;
    } // WINSCP
}

#endif

/* ----------------------------------------------------------------------
 * SSH-2 private key load/store functions.
 *
 * PuTTY's own file format for SSH-2 keys is given in doc/ppk.but, aka
 * the "PPK file format" appendix in the PuTTY manual.
 */

static bool read_header(BinarySource *src, char *header)
{
    int len = 39;
    int c;

    while (1) {
        c = get_byte(src);
        if (c == '\n' || c == '\r' || get_err(src))
            return false;              /* failure */
        if (c == ':') {
            c = get_byte(src);
            if (c != ' ')
                return false;
            *header = '\0';
            return true;               /* success! */
        }
        if (len == 0)
            return false;              /* failure */
        *header++ = c;
        len--;
    }
    return false;                      /* failure */
}

static char *read_body(BinarySource *src)
{
    strbuf *buf = strbuf_new_nm();

    while (1) {
        int c = get_byte(src);
        if (c == '\r' || c == '\n' || get_err(src)) {
            if (!get_err(src)) {
                c = get_byte(src);
                if (c != '\r' && c != '\n' && !get_err(src))
                    src->pos--;
            }
            return strbuf_to_str(buf);
        }
        put_byte(buf, c);
    }
}

static bool read_blob(BinarySource *src, int nlines, BinarySink *bs)
{
    char *line;
    int linelen;
    int i, j, k;

    /* We expect at most 64 base64 characters, ie 48 real bytes, per line. */

    for (i = 0; i < nlines; i++) {
        line = read_body(src);
        if (!line)
            return false;
        linelen = strlen(line);
        if (linelen % 4 != 0 || linelen > 64) {
            sfree(line);
            return false;
        }
        for (j = 0; j < linelen; j += 4) {
            unsigned char decoded[3];
            k = base64_decode_atom(line + j, decoded);
            if (!k) {
                sfree(line);
                return false;
            }
            put_data(bs, decoded, k);
        }
        sfree(line);
    }
    return true;
}

/*
 * Magic error return value for when the passphrase is wrong.
 */
ssh2_userkey ssh2_wrong_passphrase = { NULL, NULL };

const ssh_keyalg *const all_keyalgs[] = {
    &ssh_rsa,
    &ssh_rsa_sha256,
    &ssh_rsa_sha512,
    &ssh_dsa,
    &ssh_ecdsa_nistp256,
    &ssh_ecdsa_nistp384,
    &ssh_ecdsa_nistp521,
    &ssh_ecdsa_ed25519,
    &ssh_ecdsa_ed448,
    &opensshcert_ssh_dsa,
    &opensshcert_ssh_rsa,
    &opensshcert_ssh_rsa_sha256,
    &opensshcert_ssh_rsa_sha512,
    &opensshcert_ssh_ecdsa_ed25519,
    &opensshcert_ssh_ecdsa_nistp256,
    &opensshcert_ssh_ecdsa_nistp384,
    &opensshcert_ssh_ecdsa_nistp521,
};
const size_t n_keyalgs = lenof(all_keyalgs);

const ssh_keyalg *find_pubkey_alg_len(ptrlen name)
{
    size_t i; // WINSCP
    for (i = 0; i < n_keyalgs; i++)
        if (ptrlen_eq_string(name, all_keyalgs[i]->ssh_id))
            return all_keyalgs[i];

    return NULL;
}

const ssh_keyalg *find_pubkey_alg(const char *name)
{
    return find_pubkey_alg_len(ptrlen_from_asciz(name));
}

ptrlen pubkey_blob_to_alg_name(ptrlen blob)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, blob);
    return get_string(src);
}

const ssh_keyalg *pubkey_blob_to_alg(ptrlen blob)
{
    return find_pubkey_alg_len(pubkey_blob_to_alg_name(blob));
}

struct ppk_cipher {
    const char *name;
    size_t blocklen, keylen, ivlen;
};
static const struct ppk_cipher ppk_cipher_none = { "none", 1, 0, 0 };
static const struct ppk_cipher ppk_cipher_aes256_cbc = { "aes256-cbc", 16, 32, 16 };

static void ssh2_ppk_derive_keys(
    unsigned fmt_version, const struct ppk_cipher *ciphertype,
    ptrlen passphrase, strbuf *storage, ptrlen *cipherkey, ptrlen *cipheriv,
    ptrlen *mackey, ptrlen passphrase_salt, ppk_save_parameters *params)
{
    size_t mac_keylen;

    switch (fmt_version) {
      case 3: {
        if (ciphertype->keylen == 0) {
            mac_keylen = 0;
            break;
        }
        { // WINSCP
        ptrlen empty = PTRLEN_LITERAL("");

        mac_keylen = 32;

        { // WINSCP
        uint32_t taglen = ciphertype->keylen + ciphertype->ivlen + mac_keylen;

        if (params->argon2_passes_auto) {
            uint32_t passes;

            argon2_choose_passes(
                params->argon2_flavour, params->argon2_mem,
                params->argon2_milliseconds, &passes,
                params->argon2_parallelism, taglen,
                passphrase, passphrase_salt, empty, empty, storage);

            params->argon2_passes_auto = false;
            params->argon2_passes = passes;
        } else {
            argon2(params->argon2_flavour, params->argon2_mem,
                   params->argon2_passes, params->argon2_parallelism, taglen,
                   passphrase, passphrase_salt, empty, empty, storage);
        }
        } // WINSCP
        } // WINSCP

        break;
      }

      case 2:
      case 1: {
        /* Counter-mode iteration to generate cipher key data. */
        { // WINSCP
        unsigned ctr; // WINSCP
        for (ctr = 0; ctr * 20 < ciphertype->keylen; ctr++) {
            ssh_hash *h = ssh_hash_new(&ssh_sha1);
            put_uint32(h, ctr);
            put_datapl(h, passphrase);
            ssh_hash_final(h, strbuf_append(storage, 20));
        }
        strbuf_shrink_to(storage, ciphertype->keylen);

        /* In this version of the format, the CBC IV was always all 0. */
        put_padding(storage, ciphertype->ivlen, 0);

        /* Completely separate hash for the MAC key. */
        { // WINSCP
        ssh_hash *h = ssh_hash_new(&ssh_sha1);
        mac_keylen = ssh_hash_alg(h)->hlen;
        put_datapl(h, PTRLEN_LITERAL("putty-private-key-file-mac-key"));
        put_datapl(h, passphrase);
        ssh_hash_final(h, strbuf_append(storage, mac_keylen));
        } // WINSCP
        } // WINSCP

        break;
      }

      default:
        unreachable("bad format version in ssh2_ppk_derive_keys");
    }

    { // WINSCP
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(storage));
    *cipherkey = get_data(src, ciphertype->keylen);
    *cipheriv = get_data(src, ciphertype->ivlen);
    *mackey = get_data(src, mac_keylen);
    } // WINSCP
}

static int userkey_parse_line_counter(const char *text)
{
    char *endptr;
    unsigned long ul = strtoul(text, &endptr, 10);
    if (*text && !*endptr && ul < MAX_KEY_BLOB_LINES)
        return ul;
    else
        return -1;
}

static bool str_to_uint32_t(const char *s, uint32_t *out)
{
    char *endptr;
    unsigned long converted = strtoul(s, &endptr, 10);
    if (*s && !*endptr && converted <= ~(uint32_t)0) {
        *out = converted;
        return true;
    } else {
        return false;
    }
}

ssh2_userkey *ppk_load_s(BinarySource *src, const char *passphrase,
                         const char **errorstr)
{
    char header[40], *b, *encryption, *comment, *mac;
    const ssh_keyalg *alg;
    ssh2_userkey *ukey;
    strbuf *public_blob, *private_blob, *cipher_mac_keys_blob;
    strbuf *passphrase_salt = strbuf_new();
    ptrlen cipherkey, cipheriv, mackey;
    const struct ppk_cipher *ciphertype;
    int i;
    bool is_mac;
    unsigned fmt_version;
    const char *error = NULL;
    ppk_save_parameters params;

    ukey = NULL;                        /* return NULL for most errors */
    encryption = comment = mac = NULL;
    public_blob = private_blob = cipher_mac_keys_blob = NULL;

    /* Read the first header line which contains the key type. */
    if (!read_header(src, header)) {
        error = "no header line found in key file";
        goto error;
    }
    if (0 == strcmp(header, "PuTTY-User-Key-File-3")) {
        fmt_version = 3;
    } else if (0 == strcmp(header, "PuTTY-User-Key-File-2")) {
        fmt_version = 2;
    } else if (0 == strcmp(header, "PuTTY-User-Key-File-1")) {
        /* this is an old key file; warn and then continue */
        old_keyfile_warning();
        fmt_version = 1;
    } else if (0 == strncmp(header, "PuTTY-User-Key-File-", 20)) {
        /* this is a key file FROM THE FUTURE; refuse it, but with a
         * more specific error message than the generic one below */
        error = "PuTTY key format too new";
        goto error;
    } else {
        error = "not a PuTTY SSH-2 private key";
        goto error;
    }
    error = "file format error";
    if ((b = read_body(src)) == NULL)
        goto error;
    /* Select key algorithm structure. */
    alg = find_pubkey_alg(b);
    if (!alg) {
        sfree(b);
        goto error;
    }
    sfree(b);

    /* Read the Encryption header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Encryption"))
        goto error;
    if ((encryption = read_body(src)) == NULL)
        goto error;
    if (!strcmp(encryption, "aes256-cbc")) {
        ciphertype = &ppk_cipher_aes256_cbc;
    } else if (!strcmp(encryption, "none")) {
        ciphertype = &ppk_cipher_none;
    } else {
        goto error;
    }

    /* Read the Comment header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Comment"))
        goto error;
    if ((comment = read_body(src)) == NULL)
        goto error;

    memset(&params, 0, sizeof(params)); /* in particular, sets
                                         * passes_auto=false */

    /* Read the Public-Lines header line and the public blob. */
    if (!read_header(src, header) || 0 != strcmp(header, "Public-Lines"))
        goto error;
    if ((b = read_body(src)) == NULL)
        goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    public_blob = strbuf_new();
    if (!read_blob(src, i, BinarySink_UPCAST(public_blob)))
        goto error;

    if (fmt_version >= 3 && ciphertype->keylen != 0) {
        /* Read Argon2 key derivation parameters. */
        if (!read_header(src, header) || 0 != strcmp(header, "Key-Derivation"))
            goto error;
        if ((b = read_body(src)) == NULL)
            goto error;
        if (!strcmp(b, "Argon2d")) {
            params.argon2_flavour = Argon2d;
        } else if (!strcmp(b, "Argon2i")) {
            params.argon2_flavour = Argon2i;
        } else if (!strcmp(b, "Argon2id")) {
            params.argon2_flavour = Argon2id;
        } else {
            sfree(b);
            goto error;
        }
        sfree(b);

        if (!read_header(src, header) || 0 != strcmp(header, "Argon2-Memory"))
            goto error;
        if ((b = read_body(src)) == NULL)
            goto error;
        if (!str_to_uint32_t(b, &params.argon2_mem)) {
            sfree(b);
            goto error;
        }
        sfree(b);

        if (!read_header(src, header) || 0 != strcmp(header, "Argon2-Passes"))
            goto error;
        if ((b = read_body(src)) == NULL)
            goto error;
        if (!str_to_uint32_t(b, &params.argon2_passes)) {
            sfree(b);
            goto error;
        }
        sfree(b);

        if (!read_header(src, header) ||
            0 != strcmp(header, "Argon2-Parallelism"))
            goto error;
        if ((b = read_body(src)) == NULL)
            goto error;
        if (!str_to_uint32_t(b, &params.argon2_parallelism)) {
            sfree(b);
            goto error;
        }
        sfree(b);

        if (!read_header(src, header) || 0 != strcmp(header, "Argon2-Salt"))
            goto error;
        if ((b = read_body(src)) == NULL)
            goto error;
        { // WINSCP
        size_t i; // WINSCP
        for (i = 0; b[i]; i += 2) {
            if (isxdigit((unsigned char)b[i]) && b[i+1] &&
                isxdigit((unsigned char)b[i+1])) {
                char s[3];
                s[0] = b[i];
                s[1] = b[i+1];
                s[2] = '\0';
                put_byte(passphrase_salt, strtoul(s, NULL, 16));
            } else {
                sfree(b);
                goto error;
            }
        }
        } // WINSCP
        sfree(b);
    }

    /* Read the Private-Lines header line and the Private blob. */
    if (!read_header(src, header) || 0 != strcmp(header, "Private-Lines"))
        goto error;
    if ((b = read_body(src)) == NULL)
        goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    private_blob = strbuf_new_nm();
    if (!read_blob(src, i, BinarySink_UPCAST(private_blob)))
        goto error;

    /* Read the Private-MAC or Private-Hash header line. */
    if (!read_header(src, header))
        goto error;
    if (0 == strcmp(header, "Private-MAC")) {
        if ((mac = read_body(src)) == NULL)
            goto error;
        is_mac = true;
    } else if (0 == strcmp(header, "Private-Hash") && fmt_version == 1) {
        if ((mac = read_body(src)) == NULL)
            goto error;
        is_mac = false;
    } else
        goto error;

    cipher_mac_keys_blob = strbuf_new();
    ssh2_ppk_derive_keys(fmt_version, ciphertype,
                         ptrlen_from_asciz(passphrase ? passphrase : ""),
                         cipher_mac_keys_blob, &cipherkey, &cipheriv, &mackey,
                         ptrlen_from_strbuf(passphrase_salt), &params);

    /*
     * Decrypt the private blob.
     */
    if (private_blob->len % ciphertype->blocklen)
        goto error;
    if (ciphertype == &ppk_cipher_aes256_cbc) {
        aes256_decrypt_pubkey(cipherkey.ptr, cipheriv.ptr,
                              private_blob->u, private_blob->len);
    }

    /*
     * Verify the MAC.
     */
    {
        unsigned char binary[32];
        char realmac[sizeof(binary) * 2 + 1];
        strbuf *macdata;
        bool free_macdata;

        const ssh2_macalg *mac_alg =
            fmt_version <= 2 ? &ssh_hmac_sha1 : &ssh_hmac_sha256;

        if (fmt_version == 1) {
            /* MAC (or hash) only covers the private blob. */
            macdata = private_blob;
            free_macdata = false;
        } else {
            macdata = strbuf_new_nm();
            put_stringz(macdata, alg->ssh_id);
            put_stringz(macdata, encryption);
            put_stringz(macdata, comment);
            put_string(macdata, public_blob->s,
                       public_blob->len);
            put_string(macdata, private_blob->s,
                       private_blob->len);
            free_macdata = true;
        }

        if (is_mac) {
            ssh2_mac *mac;

            mac = ssh2_mac_new(mac_alg, NULL);
            ssh2_mac_setkey(mac, mackey);
            ssh2_mac_start(mac);
            put_data(mac, macdata->s, macdata->len);
            ssh2_mac_genresult(mac, binary);
            ssh2_mac_free(mac);
        } else {
            hash_simple(&ssh_sha1, ptrlen_from_strbuf(macdata), binary);
        }

        if (free_macdata)
            strbuf_free(macdata);

        for (i = 0; i < mac_alg->len; i++)
            sprintf(realmac + 2 * i, "%02x", binary[i]);

        if (strcmp(mac, realmac)) {
            /* An incorrect MAC is an unconditional Error if the key is
             * unencrypted. Otherwise, it means Wrong Passphrase. */
            if (ciphertype->keylen != 0) {
                error = "wrong passphrase";
                ukey = SSH2_WRONG_PASSPHRASE;
            } else {
                error = "MAC failed";
                ukey = NULL;
            }
            goto error;
        }
    }

    /*
     * Create and return the key.
     */
    ukey = snew(ssh2_userkey);
    ukey->comment = comment;
    comment = NULL;
    ukey->key = ssh_key_new_priv(
        alg, ptrlen_from_strbuf(public_blob),
        ptrlen_from_strbuf(private_blob));
    if (!ukey->key) {
        sfree(ukey);
        ukey = NULL;
        error = "createkey failed";
        goto error;
    }
    error = NULL;

    /*
     * Error processing.
     */
  error:
    if (comment)
        sfree(comment);
    if (encryption)
        sfree(encryption);
    if (mac)
        sfree(mac);
    if (public_blob)
        strbuf_free(public_blob);
    if (private_blob)
        strbuf_free(private_blob);
    if (cipher_mac_keys_blob)
        strbuf_free(cipher_mac_keys_blob);
    strbuf_free(passphrase_salt);
    if (errorstr)
        *errorstr = error;
    return ukey;
}

ssh2_userkey *ppk_load_f(const Filename *filename, const char *passphrase,
                         const char **errorstr)
{
    LoadedFile *lf = lf_load_keyfile(filename, errorstr);
    ssh2_userkey *toret;
    if (lf) {
        toret = ppk_load_s(BinarySource_UPCAST(lf), passphrase, errorstr);
        lf_free(lf);
    } else {
        toret = NULL;
        *errorstr = "can't open file";
    }
    return toret;
}

static bool rfc4716_loadpub(BinarySource *src, char **algorithm,
                            BinarySink *bs,
                            char **commentptr, const char **errorstr)
{
    const char *error;
    char *line, *colon, *value;
    char *comment = NULL;
    strbuf *pubblob = NULL;
    char base64in[4];
    unsigned char base64out[3];
    int base64bytes;
    int alglen;

    line = mkstr(get_chomped_line(src));
    if (!line || 0 != strcmp(line, "---- BEGIN SSH2 PUBLIC KEY ----")) {
        error = "invalid begin line in SSH-2 public key file";
        goto error;
    }
    sfree(line); line = NULL;

    while (1) {
        line = mkstr(get_chomped_line(src));
        if (!line) {
            error = "truncated SSH-2 public key file";
            goto error;
        }
        colon = strstr(line, ": ");
        if (!colon)
            break;
        *colon = '\0';
        value = colon + 2;

        if (!strcmp(line, "Comment")) {
            char *p, *q;

            /* Remove containing double quotes, if present */
            p = value;
            if (*p == '"' && p[strlen(p)-1] == '"') {
                p[strlen(p)-1] = '\0';
                p++;
            }

            /* Remove \-escaping, not in RFC4716 but seen in the wild
             * in practice. */
            for (q = line; *p; p++) {
                if (*p == '\\' && p[1])
                    p++;
                *q++ = *p;
            }

            *q = '\0';
            sfree(comment);   /* *just* in case of multiple Comment headers */
            comment = dupstr(line);
        } else if (!strcmp(line, "Subject") ||
                   !strncmp(line, "x-", 2)) {
            /* Headers we recognise and ignore. Do nothing. */
        } else {
            error = "unrecognised header in SSH-2 public key file";
            goto error;
        }

        sfree(line); line = NULL;
    }

    /*
     * Now line contains the initial line of base64 data. Loop round
     * while it still does contain base64.
     */
    pubblob = strbuf_new();
    base64bytes = 0;
    while (line && line[0] != '-') {
        char *p;
        for (p = line; *p; p++) {
            base64in[base64bytes++] = *p;
            if (base64bytes == 4) {
                int n = base64_decode_atom(base64in, base64out);
                put_data(pubblob, base64out, n);
                base64bytes = 0;
            }
        }
        sfree(line); line = NULL;
        if (!get_avail(src))
            break;
        line = mkstr(get_chomped_line(src));
    }

    /*
     * Finally, check the END line makes sense.
     */
    if (!line || 0 != strcmp(line, "---- END SSH2 PUBLIC KEY ----")) {
        error = "invalid end line in SSH-2 public key file";
        goto error;
    }
    sfree(line); line = NULL;

    /*
     * OK, we now have a public blob and optionally a comment. We must
     * return the key algorithm string too, so look for that at the
     * start of the public blob.
     */
    if (pubblob->len < 4) {
        error = "not enough data in SSH-2 public key file";
        goto error;
    }
    alglen = toint(GET_32BIT_MSB_FIRST(pubblob->u));
    if (alglen < 0 || alglen > pubblob->len-4) {
        error = "invalid algorithm prefix in SSH-2 public key file";
        goto error;
    }
    if (algorithm)
        *algorithm = dupprintf("%.*s", alglen, pubblob->s+4);
    if (commentptr)
        *commentptr = comment;
    else
        sfree(comment);
    put_datapl(bs, ptrlen_from_strbuf(pubblob));
    strbuf_free(pubblob);
    return true;

  error:
    sfree(line);
    sfree(comment);
    if (pubblob)
        strbuf_free(pubblob);
    if (errorstr)
        *errorstr = error;
    return false;
}

/*WINSCP static*/ bool openssh_loadpub(BinarySource *src, char **algorithm,
                            BinarySink *bs,
                            char **commentptr, const char **errorstr)
{
    const char *error;
    char *line, *base64;
    char *comment = NULL;
    unsigned char *pubblob = NULL;
    int pubbloblen, pubblobsize;
    int alglen;

    line = mkstr(get_chomped_line(src));

    base64 = strchr(line, ' ');
    if (!base64) {
        error = "no key blob in OpenSSH public key file";
        goto error;
    }
    *base64++ = '\0';

    comment = strchr(base64, ' ');
    if (comment) {
        *comment++ = '\0';
        comment = dupstr(comment);
    }

    pubblobsize = strlen(base64) / 4 * 3;
    pubblob = snewn(pubblobsize, unsigned char);
    pubbloblen = 0;

    while (!memchr(base64, '\0', 4)) {
        assert(pubbloblen + 3 <= pubblobsize);
        pubbloblen += base64_decode_atom(base64, pubblob + pubbloblen);
        base64 += 4;
    }
    if (*base64) {
        error = "invalid length for base64 data in OpenSSH public key file";
        goto error;
    }

    /*
     * Sanity check: the first word on the line should be the key
     * algorithm, and should match the encoded string at the start of
     * the public blob.
     */
    alglen = strlen(line);
    if (pubbloblen < alglen + 4 ||
        GET_32BIT_MSB_FIRST(pubblob) != alglen ||
        0 != memcmp(pubblob + 4, line, alglen)) {
        error = "key algorithms do not match in OpenSSH public key file";
        goto error;
    }

    /*
     * Done.
     */
    if (algorithm)
        *algorithm = dupstr(line);
    if (commentptr)
        *commentptr = comment;
    else
        sfree(comment);
    sfree(line);
    put_data(bs, pubblob, pubbloblen);
    sfree(pubblob);
    return true;

  error:
    sfree(line);
    sfree(comment);
    sfree(pubblob);
    if (errorstr)
        *errorstr = error;
    return false;
}

bool ppk_loadpub_s(BinarySource *src, char **algorithm, BinarySink *bs,
                   char **commentptr, const char **errorstr)
{
    char header[40], *b;
    const ssh_keyalg *alg;
    int type, i;
    const char *error = NULL;
    char *comment = NULL;

    /* Initially, check if this is a public-only key file. Sometimes
     * we'll be asked to read a public blob from one of those. */
    type = key_type_s(src);
    if (type == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716) {
        bool ret = rfc4716_loadpub(src, algorithm, bs, commentptr, errorstr);
        return ret;
    } else if (type == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
        bool ret = openssh_loadpub(src, algorithm, bs, commentptr, errorstr);
        return ret;
    } else if (type != SSH_KEYTYPE_SSH2) {
        error = "not a public key or a PuTTY SSH-2 private key";
        goto error;
    }

    /* Read the first header line which contains the key type. */
    if (!read_header(src, header)
        || (0 != strcmp(header, "PuTTY-User-Key-File-3") &&
            0 != strcmp(header, "PuTTY-User-Key-File-2") &&
            0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
        if (0 == strncmp(header, "PuTTY-User-Key-File-", 20))
            error = "PuTTY key format too new";
        else
            error = "not a public key or a PuTTY SSH-2 private key";
        goto error;
    }
    error = "file format error";
    if ((b = read_body(src)) == NULL)
        goto error;
    /* Select key algorithm structure. */
    alg = find_pubkey_alg(b);
    sfree(b);
    if (!alg) {
        goto error;
    }

    /* Read the Encryption header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Encryption"))
        goto error;
    if ((b = read_body(src)) == NULL)
        goto error;
    sfree(b);                          /* we don't care */

    /* Read the Comment header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Comment"))
        goto error;
    if ((comment = read_body(src)) == NULL)
        goto error;

    if (commentptr)
        *commentptr = comment;
    else
        sfree(comment);

    /* Read the Public-Lines header line and the public blob. */
    if (!read_header(src, header) || 0 != strcmp(header, "Public-Lines"))
        goto error;
    if ((b = read_body(src)) == NULL)
        goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    if (!read_blob(src, i, bs))
        goto error;

    if (algorithm)
        *algorithm = dupstr(alg->ssh_id);
    return true;

    /*
     * Error processing.
     */
  error:
    if (errorstr)
        *errorstr = error;
    if (comment && commentptr) {
        sfree(comment);
        *commentptr = NULL;
    }
    return false;
}

bool ppk_loadpub_f(const Filename *filename, char **algorithm, BinarySink *bs,
                   char **commentptr, const char **errorstr)
{
    LoadedFile *lf = lf_load_keyfile(filename, errorstr);
    if (!lf)
        return false;

    { // WINSCP
    bool toret = ppk_loadpub_s(BinarySource_UPCAST(lf), algorithm, bs,
                               commentptr, errorstr);
    lf_free(lf);
    return toret;
    } // WINSCP
}

bool ppk_encrypted_s(BinarySource *src, char **commentptr)
{
    char header[40], *b, *comment;
    bool ret;

    if (commentptr)
        *commentptr = NULL;

    if (!read_header(src, header)
        || (0 != strcmp(header, "PuTTY-User-Key-File-3") &&
            0 != strcmp(header, "PuTTY-User-Key-File-2") &&
            0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
        return false;
    }
    if ((b = read_body(src)) == NULL) {
        return false;
    }
    sfree(b);                          /* we don't care about key type here */
    /* Read the Encryption header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Encryption")) {
        return false;
    }
    if ((b = read_body(src)) == NULL) {
        return false;
    }

    /* Read the Comment header line. */
    if (!read_header(src, header) || 0 != strcmp(header, "Comment")) {
        sfree(b);
        return true;
    }
    if ((comment = read_body(src)) == NULL) {
        sfree(b);
        return true;
    }

    if (commentptr)
        *commentptr = comment;
    else
        sfree(comment);

    if (!strcmp(b, "aes256-cbc"))
        ret = true;
    else
        ret = false;
    sfree(b);
    return ret;
}

bool ppk_encrypted_f(const Filename *filename, char **commentptr)
{
    LoadedFile *lf = lf_load_keyfile(filename, NULL);
    if (!lf) {
        if (commentptr)
            *commentptr = NULL;
        return false;
    }

    { // WINSCP
    bool toret = ppk_encrypted_s(BinarySource_UPCAST(lf), commentptr);
    lf_free(lf);
    return toret;
    } // WINSCP
}

int base64_lines(int datalen)
{
    /* When encoding, we use 64 chars/line, which equals 48 real chars. */
    return (datalen + 47) / 48;
}

const ppk_save_parameters ppk_save_default_parameters = {
    // WINSCP
    /*.fmt_version =*/ 3,

    /*
     * The Argon2 spec recommends the hybrid variant Argon2id, where
     * you don't have a good reason to go with the pure Argon2d or
     * Argon2i.
     */
    /*.argon2_flavour =*/ Argon2id,

    /*
     * Memory requirement for hashing a password: I don't want to set
     * this to some truly huge thing like a gigabyte, because for all
     * I know people might perfectly reasonably be running PuTTY on
     * machines that don't _have_ a gigabyte spare to hash a private
     * key passphrase in the legitimate use cases.
     *
     * I've picked 8 MB as an amount of memory that isn't unreasonable
     * to expect a desktop client machine to have, but is also large
     * compared to the memory requirements of the PPK v2 password hash
     * (which was plain SHA-1), so it still imposes a limit on
     * parallel attacks on someone's key file.
     */
    /*.argon2_mem =*/ 8192,                /* require 8 Mb memory */

    /*
     * Automatically scale the number of Argon2 passes so that the
     * overall time taken is about 1/10 second. (Again, I could crank
     * this up to a larger time and _most_ people might be OK with it,
     * but for the moment, I'm trying to err on the side of not
     * stopping anyone from using the tools at all.)
     */
    /*.argon2_passes_auto =*/ true,
    /*.argon2_milliseconds =*/ 100,

    /*
     * PuTTY's own Argon2 implementation is single-threaded. So we
     * might as well set parallelism to 1, which requires that
     * attackers' implementations must also be effectively
     * single-threaded, and they don't get any benefit from using
     * multiple cores on the same hash attempt. (Of course they can
     * still use multiple cores for _separate_ hash attempts, but at
     * least they don't get a speed advantage over us in computing
     * even one hash.)
     */
    /*.argon2_parallelism =*/ 1,
    NULL, 0, // WINSCP
};

strbuf *ppk_save_sb(ssh2_userkey *key, const char *passphrase,
                    const ppk_save_parameters *params_orig)
{
    strbuf *pub_blob, *priv_blob, *cipher_mac_keys_blob;
    unsigned char *priv_blob_encrypted;
    int priv_encrypted_len;
    int cipherblk;
    int i;
    const char *cipherstr;
    ptrlen cipherkey, cipheriv, mackey;
    const struct ppk_cipher *ciphertype;
    unsigned char priv_mac[32];

    /*
     * Fetch the key component blobs.
     */
    pub_blob = strbuf_new();
    ssh_key_public_blob(key->key, BinarySink_UPCAST(pub_blob));
    priv_blob = strbuf_new_nm();
    ssh_key_private_blob(key->key, BinarySink_UPCAST(priv_blob));

    /*
     * Determine encryption details, and encrypt the private blob.
     */
    if (passphrase) {
        cipherstr = "aes256-cbc";
        cipherblk = 16;
        ciphertype = &ppk_cipher_aes256_cbc;
    } else {
        cipherstr = "none";
        cipherblk = 1;
        ciphertype = &ppk_cipher_none;
    }
    priv_encrypted_len = priv_blob->len + cipherblk - 1;
    priv_encrypted_len -= priv_encrypted_len % cipherblk;
    priv_blob_encrypted = snewn(priv_encrypted_len, unsigned char);
    memset(priv_blob_encrypted, 0, priv_encrypted_len);
    memcpy(priv_blob_encrypted, priv_blob->u, priv_blob->len);
    /* Create padding based on the SHA hash of the unpadded blob. This prevents
     * too easy a known-plaintext attack on the last block. */
    hash_simple(&ssh_sha1, ptrlen_from_strbuf(priv_blob), priv_mac);
    assert(priv_encrypted_len - priv_blob->len < 20);
    memcpy(priv_blob_encrypted + priv_blob->len, priv_mac,
           priv_encrypted_len - priv_blob->len);

    /* Copy the save parameters, so that when derive_keys chooses the
     * number of Argon2 passes, it can write the result back to our
     * copy for us to retrieve. */
    { // WINSCP
    ppk_save_parameters params = *params_orig;

    strbuf *passphrase_salt = strbuf_new();

    if (params.fmt_version == 3) {
        /* Invent a salt for the password hash. */
        if (params.salt)
            put_data(passphrase_salt, params.salt, params.saltlen);
        else
            random_read(strbuf_append(passphrase_salt, 16), 16);
    }

    cipher_mac_keys_blob = strbuf_new();
    ssh2_ppk_derive_keys(params.fmt_version, ciphertype,
                         ptrlen_from_asciz(passphrase ? passphrase : ""),
                         cipher_mac_keys_blob, &cipherkey, &cipheriv, &mackey,
                         ptrlen_from_strbuf(passphrase_salt), &params);

    { // WINSCP
    const ssh2_macalg *macalg = (params.fmt_version == 2 ?
                                 &ssh_hmac_sha1 : &ssh_hmac_sha256);

    /* Now create the MAC. */
    {
        strbuf *macdata;

        macdata = strbuf_new_nm();
        put_stringz(macdata, ssh_key_ssh_id(key->key));
        put_stringz(macdata, cipherstr);
        put_stringz(macdata, key->comment);
        put_string(macdata, pub_blob->s, pub_blob->len);
        put_string(macdata, priv_blob_encrypted, priv_encrypted_len);

        mac_simple(macalg, mackey, ptrlen_from_strbuf(macdata), priv_mac);
        strbuf_free(macdata);
    }

    if (passphrase) {
        assert(cipherkey.len == 32);
        aes256_encrypt_pubkey(cipherkey.ptr, cipheriv.ptr,
                              priv_blob_encrypted, priv_encrypted_len);
    }

    { // WINSCP
    strbuf *out = strbuf_new_nm();
    put_fmt(out, "PuTTY-User-Key-File-%u: %s\n",
            params.fmt_version, ssh_key_ssh_id(key->key));
    put_fmt(out, "Encryption: %s\n", cipherstr);
    put_fmt(out, "Comment: %s\n", key->comment);
    put_fmt(out, "Public-Lines: %d\n", base64_lines(pub_blob->len));
    base64_encode_bs(BinarySink_UPCAST(out), ptrlen_from_strbuf(pub_blob), 64);
    if (params.fmt_version == 3 && ciphertype->keylen != 0) {
        put_fmt(out, "Key-Derivation: %s\n",
                params.argon2_flavour == Argon2d ? "Argon2d" :
                params.argon2_flavour == Argon2i ? "Argon2i" : "Argon2id");
        put_fmt(out, "Argon2-Memory: %"PRIu32"\n", params.argon2_mem);
        assert(!params.argon2_passes_auto);
        put_fmt(out, "Argon2-Passes: %"PRIu32"\n", params.argon2_passes);
        put_fmt(out, "Argon2-Parallelism: %"PRIu32"\n",
                params.argon2_parallelism);
        put_fmt(out, "Argon2-Salt: ");
        { // WINSCP
        size_t i;
        for (i = 0; i < passphrase_salt->len; i++)
            put_fmt(out, "%02x", passphrase_salt->u[i]);
        put_fmt(out, "\n");
        } // WINSCP
    }
    put_fmt(out, "Private-Lines: %d\n", base64_lines(priv_encrypted_len));
    base64_encode_bs(BinarySink_UPCAST(out),
                     make_ptrlen(priv_blob_encrypted, priv_encrypted_len), 64);
    put_fmt(out, "Private-MAC: ");
    for (i = 0; i < macalg->len; i++)
        put_fmt(out, "%02x", priv_mac[i]);
    put_fmt(out, "\n");

    strbuf_free(cipher_mac_keys_blob);
    strbuf_free(passphrase_salt);
    strbuf_free(pub_blob);
    strbuf_free(priv_blob);
    smemclr(priv_blob_encrypted, priv_encrypted_len);
    sfree(priv_blob_encrypted);
    return out;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

bool ppk_save_f(const Filename *filename, ssh2_userkey *key,
                const char *passphrase, const ppk_save_parameters *params)
{
    FILE *fp = f_open(filename, "wb", true);
    if (!fp)
        return false;

    { // WINSCP
    strbuf *buf = ppk_save_sb(key, passphrase, params);
    bool toret = fwrite(buf->s, 1, buf->len, fp) == buf->len;
    if (fclose(fp))
        toret = false;
    strbuf_free(buf);
    return toret;
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Output public keys.
 */
char *ssh1_pubkey_str(RSAKey *key)
{
    char *buffer;
    char *dec1, *dec2;

    dec1 = mp_get_decimal(key->exponent);
    dec2 = mp_get_decimal(key->modulus);
    buffer = dupprintf("%"SIZEu" %s %s%s%s", mp_get_nbits(key->modulus),
                       dec1, dec2, key->comment ? " " : "",
                       key->comment ? key->comment : "");
    sfree(dec1);
    sfree(dec2);
    return buffer;
}

void ssh1_write_pubkey(FILE *fp, RSAKey *key)
{
    char *buffer = ssh1_pubkey_str(key);
    fprintf(fp, "%s\n", buffer);
    sfree(buffer);
}

static char *ssh2_pubkey_openssh_str_internal(const char *comment,
                                              const void *v_pub_blob,
                                              int pub_len)
{
    const unsigned char *ssh2blob = (const unsigned char *)v_pub_blob;
    ptrlen alg;
    char *buffer, *p;
    int i;

    {
        BinarySource src[1];
        BinarySource_BARE_INIT(src, ssh2blob, pub_len);
        alg = get_string(src);
        if (get_err(src)) {
            const char *replacement_str = "INVALID-ALGORITHM";
            alg.ptr = replacement_str;
            alg.len = strlen(replacement_str);
        }
    }

    buffer = snewn(alg.len +
                   4 * ((pub_len+2) / 3) +
                   (comment ? strlen(comment) : 0) + 3, char);
    p = buffer + sprintf(buffer, "%.*s ", PTRLEN_PRINTF(alg));
    i = 0;
    while (i < pub_len) {
        int n = (pub_len - i < 3 ? pub_len - i : 3);
        base64_encode_atom(ssh2blob + i, n, p);
        i += n;
        p += 4;
    }
    if (comment) {
        *p++ = ' ';
        strcpy(p, comment);
    } else
        *p++ = '\0';

    return buffer;
}

char *ssh2_pubkey_openssh_str(ssh2_userkey *key)
{
    strbuf *blob;
    char *ret;

    blob = strbuf_new();
    ssh_key_public_blob(key->key, BinarySink_UPCAST(blob));
    ret = ssh2_pubkey_openssh_str_internal(
        key->comment, blob->s, blob->len);
    strbuf_free(blob);

    return ret;
}

void ssh2_write_pubkey(FILE *fp, const char *comment,
                       const void *v_pub_blob, int pub_len,
                       int keytype)
{
    unsigned char *pub_blob = (unsigned char *)v_pub_blob;

    if (keytype == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716) {
        const char *p;
        int i, column;

        fprintf(fp, "---- BEGIN SSH2 PUBLIC KEY ----\n");

        if (comment) {
            fprintf(fp, "Comment: \"");
            for (p = comment; *p; p++) {
                if (*p == '\\' || *p == '\"')
                    fputc('\\', fp);
                fputc(*p, fp);
            }
            fprintf(fp, "\"\n");
        }

        i = 0;
        column = 0;
        while (i < pub_len) {
            char buf[5];
            int n = (pub_len - i < 3 ? pub_len - i : 3);
            base64_encode_atom(pub_blob + i, n, buf);
            i += n;
            buf[4] = '\0';
            fputs(buf, fp);
            if (++column >= 16) {
                fputc('\n', fp);
                column = 0;
            }
        }
        if (column > 0)
            fputc('\n', fp);

        fprintf(fp, "---- END SSH2 PUBLIC KEY ----\n");
    } else if (keytype == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
        char *buffer = ssh2_pubkey_openssh_str_internal(comment,
                                                        v_pub_blob, pub_len);
        fprintf(fp, "%s\n", buffer);
        sfree(buffer);
    } else {
        unreachable("Bad key type in ssh2_write_pubkey");
    }
}

/* ----------------------------------------------------------------------
 * Utility functions to compute SSH-2 fingerprints in a uniform way.
 */
static void ssh2_fingerprint_blob_md5(ptrlen blob, strbuf *sb)
{
    unsigned char digest[16];

    unsigned i; // WINSCP
    hash_simple(&ssh_md5, blob, digest);
    for (i = 0; i < 16; i++)
        put_fmt(sb, "%02x%s", digest[i], i==15 ? "" : ":");
}

static void ssh2_fingerprint_blob_sha256(ptrlen blob, strbuf *sb)
{
    unsigned char digest[32];
    hash_simple(&ssh_sha256, blob, digest);

    put_datapl(sb, PTRLEN_LITERAL("SHA256:"));

    { // WINSCP
    unsigned i;
    for (i = 0; i < 32; i += 3) {
        char buf[5];
        unsigned len = 32-i;
        if (len > 3)
            len = 3;
        base64_encode_atom(digest + i, len, buf);
        put_data(sb, buf, 4);
    }
    strbuf_chomp(sb, '=');
    } // WINSCP
}

char *ssh2_fingerprint_blob(ptrlen blob, FingerprintType fptype)
{
    strbuf *sb = strbuf_new();
    strbuf *tmp = NULL;

    /*
     * Identify the key algorithm, if possible.
     *
     * If we can't do that, then we have a seriously confused key
     * blob, in which case we return only the hash.
     */
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, blob);
    { // WINSCP
    ptrlen algname = get_string(src);
    if (!get_err(src)) {
        const ssh_keyalg *alg = find_pubkey_alg_len(algname);
        if (alg) {
            int bits = ssh_key_public_bits(alg, blob);
            put_fmt(sb, "%.*s %d ", PTRLEN_PRINTF(algname), bits);

            if (!ssh_fptype_is_cert(fptype) && alg->is_certificate) {
                ssh_key *key = ssh_key_new_pub(alg, blob);
                if (key) {
                    tmp = strbuf_new();
                    ssh_key_public_blob(ssh_key_base_key(key),
                                        BinarySink_UPCAST(tmp));
                    blob = ptrlen_from_strbuf(tmp);
                    ssh_key_free(key);
                }
            }
        } else {
            put_fmt(sb, "%.*s ", PTRLEN_PRINTF(algname));
        }
    }
    } // WINSCP

    switch (ssh_fptype_from_cert(fptype)) {
      case SSH_FPTYPE_MD5:
        ssh2_fingerprint_blob_md5(blob, sb);
        break;
      case SSH_FPTYPE_SHA256:
        ssh2_fingerprint_blob_sha256(blob, sb);
        break;
      default:
        unreachable("ssh_fptype_from_cert ruled out the other values");
    }

    if (tmp)
        strbuf_free(tmp);

    return strbuf_to_str(sb);
}

char *ssh2_double_fingerprint_blob(ptrlen blob, FingerprintType fptype)
{
    if (ssh_fptype_is_cert(fptype))
        fptype = ssh_fptype_from_cert(fptype);

    { // WINSCP
    char *fp = ssh2_fingerprint_blob(blob, fptype);
    char *p = strrchr(fp, ' ');
    char *hash = p ? p + 1 : fp;

    char *fpc = ssh2_fingerprint_blob(blob, ssh_fptype_to_cert(fptype));
    char *pc = strrchr(fpc, ' ');
    char *hashc = pc ? pc + 1 : fpc;

    if (strcmp(hash, hashc)) {
        char *tmp = dupprintf("%s (with certificate: %s)", fp, hashc);
        sfree(fp);
        fp = tmp;
    }

    sfree(fpc);
    return fp;
    } // WINSCP
}

char **ssh2_all_fingerprints_for_blob(ptrlen blob)
{
    char **fps = snewn(SSH_N_FPTYPES, char *);
    unsigned i; // WINSCP
    for (i = 0; i < SSH_N_FPTYPES; i++)
        fps[i] = ssh2_fingerprint_blob(blob, i);
    return fps;
}

char *ssh2_fingerprint(ssh_key *data, FingerprintType fptype)
{
    strbuf *blob = strbuf_new();
    char *ret; //MPEXT
    ssh_key_public_blob(data, BinarySink_UPCAST(blob));
    ret = ssh2_fingerprint_blob(ptrlen_from_strbuf(blob), fptype);
    strbuf_free(blob);
    return ret;
}

char *ssh2_double_fingerprint(ssh_key *data, FingerprintType fptype)
{
    strbuf *blob = strbuf_new();
    ssh_key_public_blob(data, BinarySink_UPCAST(blob));
    { // WINSCP
    char *ret = ssh2_double_fingerprint_blob(ptrlen_from_strbuf(blob), fptype);
    strbuf_free(blob);
    return ret;
    } // WINSCP
}

char **ssh2_all_fingerprints(ssh_key *data)
{
    strbuf *blob = strbuf_new();
    ssh_key_public_blob(data, BinarySink_UPCAST(blob));
    { // WINSCP
    char **ret = ssh2_all_fingerprints_for_blob(ptrlen_from_strbuf(blob));
    strbuf_free(blob);
    return ret;
    } // WINSCP
}

void ssh2_free_all_fingerprints(char **fps)
{
    unsigned i; // WINSCP
    for (i = 0; i < SSH_N_FPTYPES; i++)
        sfree(fps[i]);
    sfree(fps);
}

/* ----------------------------------------------------------------------
 * Determine the type of a private key file.
 */
static int key_type_s_internal(BinarySource *src)
{
    static const ptrlen public_std_sig =
        PTRLEN_DECL_LITERAL("---- BEGIN SSH2 PUBLIC KEY");
    static const ptrlen putty2_sig =
        PTRLEN_DECL_LITERAL("PuTTY-User-Key-File-");
    static const ptrlen sshcom_sig =
        PTRLEN_DECL_LITERAL("---- BEGIN SSH2 ENCRYPTED PRIVAT");
    static const ptrlen openssh_new_sig =
        PTRLEN_DECL_LITERAL("-----BEGIN OPENSSH PRIVATE KEY");
    static const ptrlen openssh_sig =
        PTRLEN_DECL_LITERAL("-----BEGIN ");

    if (BinarySource_REWIND(src), expect_signature(src, rsa1_signature))
        return SSH_KEYTYPE_SSH1;
    if (BinarySource_REWIND(src), expect_signature(src, public_std_sig))
        return SSH_KEYTYPE_SSH2_PUBLIC_RFC4716;
    if (BinarySource_REWIND(src), expect_signature(src, putty2_sig))
        return SSH_KEYTYPE_SSH2;
    if (BinarySource_REWIND(src), expect_signature(src, openssh_new_sig))
        return SSH_KEYTYPE_OPENSSH_NEW;
    if (BinarySource_REWIND(src), expect_signature(src, openssh_sig))
        return SSH_KEYTYPE_OPENSSH_PEM;
    if (BinarySource_REWIND(src), expect_signature(src, sshcom_sig))
        return SSH_KEYTYPE_SSHCOM;

    BinarySource_REWIND(src);
    if (get_chars(src, "0123456789").len > 0 && get_chars(src, " ").len == 1 &&
        get_chars(src, "0123456789").len > 0 && get_chars(src, " ").len == 1 &&
        get_chars(src, "0123456789").len > 0 &&
        get_nonchars(src, " \n").len == 0)
        return SSH_KEYTYPE_SSH1_PUBLIC;

    BinarySource_REWIND(src);
    if (find_pubkey_alg_len(get_nonchars(src, " \n")) > 0 &&
        get_chars(src, " ").len == 1 &&
        get_chars(src, "0123456789ABCDEFGHIJKLMNOPQRSTUV"
                  "WXYZabcdefghijklmnopqrstuvwxyz+/=").len > 0 &&
        get_nonchars(src, " \n").len == 0)
        return SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH;

    return SSH_KEYTYPE_UNKNOWN;        /* unrecognised or EOF */
}

int key_type_s(BinarySource *src)
{
    int toret = key_type_s_internal(src);
    BinarySource_REWIND(src);
    return toret;
}

int key_type(const Filename *filename)
{
    LoadedFile *lf = lf_new(1024);
    if (lf_load(lf, filename) == LF_ERROR) {
        lf_free(lf);
        return SSH_KEYTYPE_UNOPENABLE;
    }

    { // WINSCP
    int toret = key_type_s(BinarySource_UPCAST(lf));
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * Convert the type word to a string, for `wrong type' error
 * messages.
 */
const char *key_type_to_str(int type)
{
    switch (type) {
      case SSH_KEYTYPE_UNOPENABLE:
        return "unable to open file";
      case SSH_KEYTYPE_UNKNOWN:
        return "not a recognised key file format";
      case SSH_KEYTYPE_SSH1_PUBLIC:
        return "SSH-1 public key";
      case SSH_KEYTYPE_SSH2_PUBLIC_RFC4716:
        return "SSH-2 public key (RFC 4716 format)";
      case SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH:
        return "SSH-2 public key (OpenSSH format)";
      case SSH_KEYTYPE_SSH1:
        return "SSH-1 private key";
      case SSH_KEYTYPE_SSH2:
        return "PuTTY SSH-2 private key";
      case SSH_KEYTYPE_OPENSSH_PEM:
        return "OpenSSH SSH-2 private key (old PEM format)";
      case SSH_KEYTYPE_OPENSSH_NEW:
        return "OpenSSH SSH-2 private key (new format)";
      case SSH_KEYTYPE_SSHCOM:
        return "ssh.com SSH-2 private key";

        /*
         * This function is called with a key type derived from
         * looking at an actual key file, so the output-only type
         * OPENSSH_AUTO should never get here, and is much an INTERNAL
         * ERROR as a code we don't even understand.
         */
      case SSH_KEYTYPE_OPENSSH_AUTO:
        unreachable("OPENSSH_AUTO should never reach key_type_to_str");
      default:
        unreachable("bad key type in key_type_to_str");
    }
}
