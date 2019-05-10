/*
 * Generic SSH public-key handling operations. In particular,
 * reading of SSH public-key files, and also the generic `sign'
 * operation for SSH-2 (which checks the type of the key and
 * dispatches to the appropriate key-type specific function).
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "putty.h"
#include "mpint.h"
#include "ssh.h"
#include "misc.h"

#define rsa_signature "SSH PRIVATE KEY FILE FORMAT 1.1\n"

#define BASE64_TOINT(x) ( (x)-'A'<26 ? (x)-'A'+0 :\
                          (x)-'a'<26 ? (x)-'a'+26 :\
                          (x)-'0'<10 ? (x)-'0'+52 :\
                          (x)=='+' ? 62 : \
                          (x)=='/' ? 63 : 0 )

static int key_type_fp(FILE *fp);

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

static int rsa_ssh1_load_main(FILE * fp, RSAKey *key, bool pub_only,
                              char **commentptr, const char *passphrase,
                              const char **error)
{
    strbuf *buf;
    int ciphertype;
    int ret = 0;
    ptrlen comment;
    BinarySource src[1];

    *error = NULL;

    /* Slurp the whole file (minus the header) into a buffer. */
    buf = strbuf_new_nm();
    {
        int ch;
        while ((ch = fgetc(fp)) != EOF)
            put_byte(buf, ch);
    }
    fclose(fp);

    BinarySource_BARE_INIT(src, buf->u, buf->len);

    *error = "file format error";

    /*
     * A zero byte. (The signature includes a terminating NUL, which
     * we haven't gone past yet because we read it using fgets which
     * stopped after the \n.)
     */
    if (get_byte(src) != 0)
	goto end;

    /* One byte giving encryption type, and one reserved uint32. */
    ciphertype = get_byte(src);
    if (ciphertype != 0 && ciphertype != SSH_CIPHER_3DES)
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
        unsigned char keybuf[16];
        size_t enclen = buf->len - src->pos;

        if (enclen & 7)
            goto end;

        hash_simple(&ssh_md5, ptrlen_from_asciz(passphrase), keybuf);
	des3_decrypt_pubkey(keybuf, buf->u + src->pos, enclen);
	smemclr(keybuf, sizeof(keybuf));	/* burn the evidence */
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
    } else
	ret = 1;

  end:
    strbuf_free(buf);
    return ret;
}

int rsa_ssh1_loadkey(const Filename *filename, RSAKey *key,
                     const char *passphrase, const char **errorstr)
{
    FILE *fp;
    char buf[64];
    int ret = 0;
    const char *error = NULL;

    fp = f_open(filename, "rb", false);
    if (!fp) {
	error = "can't open file";
	goto end;
    }

    /*
     * Read the first line of the file and see if it's a v1 private
     * key file.
     */
    if (fgets(buf, sizeof(buf), fp) && !strcmp(buf, rsa_signature)) {
	/*
	 * This routine will take care of calling fclose() for us.
	 */
	ret = rsa_ssh1_load_main(fp, key, false, NULL, passphrase, &error);
	fp = NULL;
	goto end;
    }

    /*
     * Otherwise, we have nothing. Return empty-handed.
     */
    error = "not an SSH-1 RSA file";

  end:
    if (fp)
	fclose(fp);
    if ((ret != 1) && errorstr)
	*errorstr = error;
    return ret;
}

/*
 * See whether an RSA key is encrypted. Return its comment field as
 * well.
 */
bool rsa_ssh1_encrypted(const Filename *filename, char **comment)
{
    FILE *fp;
    char buf[64];

    fp = f_open(filename, "rb", false);
    if (!fp)
	return false;                  /* doesn't even exist */

    /*
     * Read the first line of the file and see if it's a v1 private
     * key file.
     */
    if (fgets(buf, sizeof(buf), fp) && !strcmp(buf, rsa_signature)) {
	const char *dummy;
	/*
	 * This routine will take care of calling fclose() for us.
	 */
	return rsa_ssh1_load_main(fp, NULL, false, comment, NULL, &dummy) == 1;
    }
    fclose(fp);
    return false;                  /* wasn't the right kind of file */
}

/*
 * Read the public part of an SSH-1 RSA key from a file (public or
 * private), and generate its public blob in exponent-first order.
 */
int rsa_ssh1_loadpub(const Filename *filename, BinarySink *bs,
                     char **commentptr, const char **errorstr)
{
    FILE *fp;
    char buf[64];
    RSAKey key;
    int ret;
    const char *error = NULL;

    /* Default return if we fail. */
    ret = 0;

    fp = f_open(filename, "rb", false);
    if (!fp) {
	error = "can't open file";
	goto end;
    }

    /*
     * Read the first line of the file and see if it's a v1 private
     * key file.
     */
    if (fgets(buf, sizeof(buf), fp) && !strcmp(buf, rsa_signature)) {
	memset(&key, 0, sizeof(key));
	if (rsa_ssh1_load_main(fp, &key, true, commentptr, NULL, &error)) {
            rsa_ssh1_public_blob(bs, &key, RSA_SSH1_EXPONENT_FIRST);
	    freersakey(&key);
	    ret = 1;
	}
	fp = NULL; /* rsa_ssh1_load_main unconditionally closes fp */
    } else {
        /*
         * Try interpreting the file as an SSH-1 public key.
         */
        char *line, *p, *bitsp, *expp, *modp, *commentp;

        rewind(fp);
        line = chomp(fgetline(fp));
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
        fclose(fp);
        return 1;

      not_public_either:
        sfree(line);
	error = "not an SSH-1 RSA file";
    }

  end:
    if (fp)
	fclose(fp);
    if ((ret != 1) && errorstr)
	*errorstr = error;
    return ret;
}

/*
 * Save an RSA key file. Return true on success.
 */
bool rsa_ssh1_savekey(const Filename *filename, RSAKey *key,
                      char *passphrase)
{
    strbuf *buf = strbuf_new_nm();
    int estart;
    FILE *fp;

    /*
     * The public part of the key.
     */
    put_data(buf, rsa_signature, sizeof(rsa_signature));
    put_byte(buf, passphrase ? SSH_CIPHER_3DES : 0); /* encryption type */
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

	ssh_hash *h = ssh_hash_new(&ssh_md5);
	put_data(h, passphrase, strlen(passphrase));
	ssh_hash_final(h, keybuf);
	des3_encrypt_pubkey(keybuf, buf->u + estart, buf->len - estart);
	smemclr(keybuf, sizeof(keybuf));	/* burn the evidence */
    }

    /*
     * Done. Write the result to the file.
     */
    fp = f_open(filename, "wb", true);
    bool ret = false;
    if (fp) {
        ret = (fwrite(buf->u, 1, buf->len, fp) == (size_t) (buf->len));
        if (fclose(fp))
            ret = false;
    }
    strbuf_free(buf);
    return ret;
}

/* ----------------------------------------------------------------------
 * SSH-2 private key load/store functions.
 */

/*
 * PuTTY's own format for SSH-2 keys is as follows:
 *
 * The file is text. Lines are terminated by CRLF, although CR-only
 * and LF-only are tolerated on input.
 *
 * The first line says "PuTTY-User-Key-File-2: " plus the name of the
 * algorithm ("ssh-dss", "ssh-rsa" etc).
 *
 * The next line says "Encryption: " plus an encryption type.
 * Currently the only supported encryption types are "aes256-cbc"
 * and "none".
 *
 * The next line says "Comment: " plus the comment string.
 *
 * Next there is a line saying "Public-Lines: " plus a number N.
 * The following N lines contain a base64 encoding of the public
 * part of the key. This is encoded as the standard SSH-2 public key
 * blob (with no initial length): so for RSA, for example, it will
 * read
 *
 *    string "ssh-rsa"
 *    mpint  exponent
 *    mpint  modulus
 *
 * Next, there is a line saying "Private-Lines: " plus a number N,
 * and then N lines containing the (potentially encrypted) private
 * part of the key. For the key type "ssh-rsa", this will be
 * composed of
 *
 *    mpint  private_exponent
 *    mpint  p                  (the larger of the two primes)
 *    mpint  q                  (the smaller prime)
 *    mpint  iqmp               (the inverse of q modulo p)
 *    data   padding            (to reach a multiple of the cipher block size)
 *
 * And for "ssh-dss", it will be composed of
 *
 *    mpint  x                  (the private key parameter)
 *  [ string hash   20-byte hash of mpints p || q || g   only in old format ]
 * 
 * Finally, there is a line saying "Private-MAC: " plus a hex
 * representation of a HMAC-SHA-1 of:
 *
 *    string  name of algorithm ("ssh-dss", "ssh-rsa")
 *    string  encryption type
 *    string  comment
 *    string  public-blob
 *    string  private-plaintext (the plaintext version of the
 *                               private part, including the final
 *                               padding)
 * 
 * The key to the MAC is itself a SHA-1 hash of:
 * 
 *    data    "putty-private-key-file-mac-key"
 *    data    passphrase
 *
 * (An empty passphrase is used for unencrypted keys.)
 *
 * If the key is encrypted, the encryption key is derived from the
 * passphrase by means of a succession of SHA-1 hashes. Each hash
 * is the hash of:
 *
 *    uint32  sequence-number
 *    data    passphrase
 *
 * where the sequence-number increases from zero. As many of these
 * hashes are used as necessary.
 *
 * For backwards compatibility with snapshots between 0.51 and
 * 0.52, we also support the older key file format, which begins
 * with "PuTTY-User-Key-File-1" (version number differs). In this
 * format the Private-MAC: field only covers the private-plaintext
 * field and nothing else (and without the 4-byte string length on
 * the front too). Moreover, the Private-MAC: field can be replaced
 * with a Private-Hash: field which is a plain SHA-1 hash instead of
 * an HMAC (this was generated for unencrypted keys).
 */

static bool read_header(FILE * fp, char *header)
{
    int len = 39;
    int c;

    while (1) {
	c = fgetc(fp);
	if (c == '\n' || c == '\r' || c == EOF)
	    return false;              /* failure */
	if (c == ':') {
	    c = fgetc(fp);
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

static char *read_body(FILE * fp)
{
    strbuf *buf = strbuf_new_nm();

    while (1) {
	int c = fgetc(fp);
	if (c == '\r' || c == '\n' || c == EOF) {
	    if (c != EOF) {
		c = fgetc(fp);
		if (c != '\r' && c != '\n')
		    ungetc(c, fp);
	    }
	    return strbuf_to_str(buf);
	}
	put_byte(buf, c);
    }
}

static bool read_blob(FILE *fp, int nlines, BinarySink *bs)
{
    unsigned char *blob;
    char *line;
    int linelen;
    int i, j, k;

    /* We expect at most 64 base64 characters, ie 48 real bytes, per line. */
    assert(nlines < MAX_KEY_BLOB_LINES);
    blob = snewn(48 * nlines, unsigned char);

    for (i = 0; i < nlines; i++) {
	line = read_body(fp);
	if (!line) {
	    sfree(blob);
	    return false;
	}
	linelen = strlen(line);
	if (linelen % 4 != 0 || linelen > 64) {
	    sfree(blob);
	    sfree(line);
	    return false;
	}
	for (j = 0; j < linelen; j += 4) {
            unsigned char decoded[3];
	    k = base64_decode_atom(line + j, decoded);
	    if (!k) {
		sfree(line);
		sfree(blob);
		return false;
	    }
	    put_data(bs, decoded, k);
	}
	sfree(line);
    }
    sfree(blob);
    return true;
}

/*
 * Magic error return value for when the passphrase is wrong.
 */
ssh2_userkey ssh2_wrong_passphrase = { NULL, NULL };

const ssh_keyalg *find_pubkey_alg_len(ptrlen name)
{
    if (ptrlen_eq_string(name, "ssh-rsa"))
	return &ssh_rsa;
    else if (ptrlen_eq_string(name, "ssh-dss"))
	return &ssh_dss;
    else if (ptrlen_eq_string(name, "ecdsa-sha2-nistp256"))
        return &ssh_ecdsa_nistp256;
    else if (ptrlen_eq_string(name, "ecdsa-sha2-nistp384"))
        return &ssh_ecdsa_nistp384;
    else if (ptrlen_eq_string(name, "ecdsa-sha2-nistp521"))
        return &ssh_ecdsa_nistp521;
    else if (ptrlen_eq_string(name, "ssh-ed25519"))
        return &ssh_ecdsa_ed25519;
    else
	return NULL;
}

const ssh_keyalg *find_pubkey_alg(const char *name)
{
    return find_pubkey_alg_len(ptrlen_from_asciz(name));
}

static void ssh2_ppk_derivekey(ptrlen passphrase, uint8_t *key)
{
    ssh_hash *h;
    h = ssh_hash_new(&ssh_sha1);
    put_uint32(h, 0);
    put_datapl(h, passphrase);
    ssh_hash_final(h, key + 0);
    h = ssh_hash_new(&ssh_sha1);
    put_uint32(h, 1);
    put_datapl(h, passphrase);
    ssh_hash_final(h, key + 20);
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

ssh2_userkey *ssh2_load_userkey(
    const Filename *filename, const char *passphrase, const char **errorstr)
{
    FILE *fp;
    char header[40], *b, *encryption, *comment, *mac;
    const ssh_keyalg *alg;
    ssh2_userkey *ret;
    int cipher, cipherblk;
    strbuf *public_blob, *private_blob;
    int i;
    bool is_mac, old_fmt;
    int passlen = passphrase ? strlen(passphrase) : 0;
    const char *error = NULL;

    ret = NULL;			       /* return NULL for most errors */
    encryption = comment = mac = NULL;
    public_blob = private_blob = NULL;

    fp = f_open(filename, "rb", false);
    if (!fp) {
	error = "can't open file";
	goto error;
    }

    /* Read the first header line which contains the key type. */
    if (!read_header(fp, header)) {
	error = "no header line found in key file";
	goto error;
    }
    if (0 == strcmp(header, "PuTTY-User-Key-File-2")) {
	old_fmt = false;
    } else if (0 == strcmp(header, "PuTTY-User-Key-File-1")) {
	/* this is an old key file; warn and then continue */
	old_keyfile_warning();
	old_fmt = true;
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
    if ((b = read_body(fp)) == NULL)
	goto error;
    /* Select key algorithm structure. */
    alg = find_pubkey_alg(b);
    if (!alg) {
	sfree(b);
	goto error;
    }
    sfree(b);

    /* Read the Encryption header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Encryption"))
	goto error;
    if ((encryption = read_body(fp)) == NULL)
	goto error;
    if (!strcmp(encryption, "aes256-cbc")) {
	cipher = 1;
	cipherblk = 16;
    } else if (!strcmp(encryption, "none")) {
	cipher = 0;
	cipherblk = 1;
    } else {
	goto error;
    }

    /* Read the Comment header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Comment"))
	goto error;
    if ((comment = read_body(fp)) == NULL)
	goto error;

    /* Read the Public-Lines header line and the public blob. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Public-Lines"))
	goto error;
    if ((b = read_body(fp)) == NULL)
	goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    public_blob = strbuf_new();
    if (!read_blob(fp, i, BinarySink_UPCAST(public_blob)))
	goto error;

    /* Read the Private-Lines header line and the Private blob. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Private-Lines"))
	goto error;
    if ((b = read_body(fp)) == NULL)
	goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    private_blob = strbuf_new_nm();
    if (!read_blob(fp, i, BinarySink_UPCAST(private_blob)))
	goto error;

    /* Read the Private-MAC or Private-Hash header line. */
    if (!read_header(fp, header))
	goto error;
    if (0 == strcmp(header, "Private-MAC")) {
	if ((mac = read_body(fp)) == NULL)
	    goto error;
	is_mac = true;
    } else if (0 == strcmp(header, "Private-Hash") && old_fmt) {
	if ((mac = read_body(fp)) == NULL)
	    goto error;
	is_mac = false;
    } else
	goto error;

    fclose(fp);
    fp = NULL;

    /*
     * Decrypt the private blob.
     */
    if (cipher) {
	unsigned char key[40];

	if (!passphrase)
	    goto error;
	if (private_blob->len % cipherblk)
	    goto error;

        ssh2_ppk_derivekey(ptrlen_from_asciz(passphrase), key);
	aes256_decrypt_pubkey(key, private_blob->u, private_blob->len);
    }

    /*
     * Verify the MAC.
     */
    {
	char realmac[41];
	unsigned char binary[20];
	strbuf *macdata;
        bool free_macdata;

	if (old_fmt) {
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
            ssh_hash *hash;
            ssh2_mac *mac;
	    unsigned char mackey[20];
	    char header[] = "putty-private-key-file-mac-key";

            hash = ssh_hash_new(&ssh_sha1);
	    put_data(hash, header, sizeof(header)-1);
	    if (cipher && passphrase)
		put_data(hash, passphrase, passlen);
	    ssh_hash_final(hash, mackey);

	    mac = ssh2_mac_new(&ssh_hmac_sha1, NULL);
            ssh2_mac_setkey(mac, make_ptrlen(mackey, 20));
            ssh2_mac_start(mac);
            put_data(mac, macdata->s, macdata->len);
            ssh2_mac_genresult(mac, binary);
            ssh2_mac_free(mac);

	    smemclr(mackey, sizeof(mackey));
	} else {
	    hash_simple(&ssh_sha1, ptrlen_from_strbuf(macdata), binary);
	}

	if (free_macdata)
	    strbuf_free(macdata);

	for (i = 0; i < 20; i++)
	    sprintf(realmac + 2 * i, "%02x", binary[i]);

	if (strcmp(mac, realmac)) {
	    /* An incorrect MAC is an unconditional Error if the key is
	     * unencrypted. Otherwise, it means Wrong Passphrase. */
	    if (cipher) {
		error = "wrong passphrase";
		ret = SSH2_WRONG_PASSPHRASE;
	    } else {
		error = "MAC failed";
		ret = NULL;
	    }
	    goto error;
	}
    }
    sfree(mac);
    mac = NULL;

    /*
     * Create and return the key.
     */
    ret = snew(ssh2_userkey);
    ret->comment = comment;
    ret->key = ssh_key_new_priv(
        alg, ptrlen_from_strbuf(public_blob),
        ptrlen_from_strbuf(private_blob));
    if (!ret->key) {
	sfree(ret);
	ret = NULL;
	error = "createkey failed";
	goto error;
    }
    strbuf_free(public_blob);
    strbuf_free(private_blob);
    sfree(encryption);
    if (errorstr)
	*errorstr = NULL;
    return ret;

    /*
     * Error processing.
     */
  error:
    if (fp)
	fclose(fp);
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
    if (errorstr)
	*errorstr = error;
    return ret;
}

bool rfc4716_loadpub(FILE *fp, char **algorithm,
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

    line = chomp(fgetline(fp));
    if (!line || 0 != strcmp(line, "---- BEGIN SSH2 PUBLIC KEY ----")) {
        error = "invalid begin line in SSH-2 public key file";
        goto error;
    }
    sfree(line); line = NULL;

    while (1) {
        line = chomp(fgetline(fp));
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
        line = chomp(fgetline(fp));
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

bool openssh_loadpub(FILE *fp, char **algorithm,
                     BinarySink *bs,
                     char **commentptr, const char **errorstr)
{
    const char *error;
    char *line, *base64;
    char *comment = NULL;
    unsigned char *pubblob = NULL;
    int pubbloblen, pubblobsize;
    int alglen;

    line = chomp(fgetline(fp));

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

bool ssh2_userkey_loadpub(const Filename *filename, char **algorithm,
                          BinarySink *bs,
                          char **commentptr, const char **errorstr)
{
    FILE *fp;
    char header[40], *b;
    const ssh_keyalg *alg;
    int type, i;
    const char *error = NULL;
    char *comment = NULL;

    fp = f_open(filename, "rb", false);
    if (!fp) {
	error = "can't open file";
	goto error;
    }

    /* Initially, check if this is a public-only key file. Sometimes
     * we'll be asked to read a public blob from one of those. */
    type = key_type_fp(fp);
    if (type == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716) {
        bool ret = rfc4716_loadpub(fp, algorithm, bs, commentptr, errorstr);
        fclose(fp);
        return ret;
    } else if (type == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
        bool ret = openssh_loadpub(fp, algorithm, bs, commentptr, errorstr);
        fclose(fp);
        return ret;
    } else if (type != SSH_KEYTYPE_SSH2) {
        error = "not a PuTTY SSH-2 private key";
        goto error;
    }

    /* Read the first header line which contains the key type. */
    if (!read_header(fp, header)
	|| (0 != strcmp(header, "PuTTY-User-Key-File-2") &&
	    0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
        if (0 == strncmp(header, "PuTTY-User-Key-File-", 20))
            error = "PuTTY key format too new";
        else
            error = "not a PuTTY SSH-2 private key";
	goto error;
    }
    error = "file format error";
    if ((b = read_body(fp)) == NULL)
	goto error;
    /* Select key algorithm structure. */
    alg = find_pubkey_alg(b);
    sfree(b);
    if (!alg) {
	goto error;
    }

    /* Read the Encryption header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Encryption"))
	goto error;
    if ((b = read_body(fp)) == NULL)
	goto error;
    sfree(b);			       /* we don't care */

    /* Read the Comment header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Comment"))
	goto error;
    if ((comment = read_body(fp)) == NULL)
	goto error;

    if (commentptr)
	*commentptr = comment;
    else
	sfree(comment);

    /* Read the Public-Lines header line and the public blob. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Public-Lines"))
	goto error;
    if ((b = read_body(fp)) == NULL)
	goto error;
    i = userkey_parse_line_counter(b);
    sfree(b);
    if (i < 0)
        goto error;
    if (!read_blob(fp, i, bs))
	goto error;

    fclose(fp);
    if (algorithm)
	*algorithm = dupstr(alg->ssh_id);
    return true;

    /*
     * Error processing.
     */
  error:
    if (fp)
	fclose(fp);
    if (errorstr)
	*errorstr = error;
    if (comment && commentptr) {
        sfree(comment);
        *commentptr = NULL;
    }
    return false;
}

bool ssh2_userkey_encrypted(const Filename *filename, char **commentptr)
{
    FILE *fp;
    char header[40], *b, *comment;
    bool ret;

    if (commentptr)
	*commentptr = NULL;

    fp = f_open(filename, "rb", false);
    if (!fp)
	return false;
    if (!read_header(fp, header)
	|| (0 != strcmp(header, "PuTTY-User-Key-File-2") &&
	    0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
	fclose(fp);
	return false;
    }
    if ((b = read_body(fp)) == NULL) {
	fclose(fp);
	return false;
    }
    sfree(b);			       /* we don't care about key type here */
    /* Read the Encryption header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Encryption")) {
	fclose(fp);
	return false;
    }
    if ((b = read_body(fp)) == NULL) {
	fclose(fp);
	return false;
    }

    /* Read the Comment header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Comment")) {
	fclose(fp);
	sfree(b);
	return true;
    }
    if ((comment = read_body(fp)) == NULL) {
	fclose(fp);
	sfree(b);
	return true;
    }

    if (commentptr)
	*commentptr = comment;
    else
        sfree(comment);

    fclose(fp);
    if (!strcmp(b, "aes256-cbc"))
	ret = true;
    else
	ret = false;
    sfree(b);
    return ret;
}

int base64_lines(int datalen)
{
    /* When encoding, we use 64 chars/line, which equals 48 real chars. */
    return (datalen + 47) / 48;
}

void base64_encode(FILE *fp, const unsigned char *data, int datalen, int cpl)
{
    int linelen = 0;
    char out[4];
    int n, i;

    while (datalen > 0) {
	n = (datalen < 3 ? datalen : 3);
	base64_encode_atom(data, n, out);
	data += n;
	datalen -= n;
	for (i = 0; i < 4; i++) {
	    if (linelen >= cpl) {
		linelen = 0;
		fputc('\n', fp);
	    }
	    fputc(out[i], fp);
	    linelen++;
	}
    }
    fputc('\n', fp);
}

bool ssh2_save_userkey(
    const Filename *filename, ssh2_userkey *key, char *passphrase)
{
    FILE *fp;
    strbuf *pub_blob, *priv_blob;
    unsigned char *priv_blob_encrypted;
    int priv_encrypted_len;
    int cipherblk;
    int i;
    const char *cipherstr;
    unsigned char priv_mac[20];

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
    } else {
	cipherstr = "none";
	cipherblk = 1;
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

    /* Now create the MAC. */
    {
	strbuf *macdata;
	unsigned char mackey[20];
	char header[] = "putty-private-key-file-mac-key";

	macdata = strbuf_new_nm();
	put_stringz(macdata, ssh_key_ssh_id(key->key));
	put_stringz(macdata, cipherstr);
	put_stringz(macdata, key->comment);
	put_string(macdata, pub_blob->s, pub_blob->len);
	put_string(macdata, priv_blob_encrypted, priv_encrypted_len);

        ssh_hash *h = ssh_hash_new(&ssh_sha1);
	put_data(h, header, sizeof(header)-1);
	if (passphrase)
	    put_data(h, passphrase, strlen(passphrase));
	ssh_hash_final(h, mackey);
	mac_simple(&ssh_hmac_sha1, make_ptrlen(mackey, 20),
                   ptrlen_from_strbuf(macdata), priv_mac);
	strbuf_free(macdata);
	smemclr(mackey, sizeof(mackey));
    }

    if (passphrase) {
	unsigned char key[40];

        ssh2_ppk_derivekey(ptrlen_from_asciz(passphrase), key);
	aes256_encrypt_pubkey(key, priv_blob_encrypted, priv_encrypted_len);

	smemclr(key, sizeof(key));
    }

    fp = f_open(filename, "w", true);
    if (!fp) {
        strbuf_free(pub_blob);
        strbuf_free(priv_blob);
        smemclr(priv_blob_encrypted, priv_encrypted_len);
        sfree(priv_blob_encrypted);
        return false;
    }
    fprintf(fp, "PuTTY-User-Key-File-2: %s\n", ssh_key_ssh_id(key->key));
    fprintf(fp, "Encryption: %s\n", cipherstr);
    fprintf(fp, "Comment: %s\n", key->comment);
    fprintf(fp, "Public-Lines: %d\n", base64_lines(pub_blob->len));
    base64_encode(fp, pub_blob->u, pub_blob->len, 64);
    fprintf(fp, "Private-Lines: %d\n", base64_lines(priv_encrypted_len));
    base64_encode(fp, priv_blob_encrypted, priv_encrypted_len, 64);
    fprintf(fp, "Private-MAC: ");
    for (i = 0; i < 20; i++)
	fprintf(fp, "%02x", priv_mac[i]);
    fprintf(fp, "\n");
    fclose(fp);

    strbuf_free(pub_blob);
    strbuf_free(priv_blob);
    smemclr(priv_blob_encrypted, priv_encrypted_len);
    sfree(priv_blob_encrypted);
    return true;
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
    buffer = dupprintf("%zd %s %s%s%s", mp_get_nbits(key->modulus), dec1, dec2,
                       key->comment ? " " : "",
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
char *ssh2_fingerprint_blob(ptrlen blob)
{
    unsigned char digest[16];
    char fingerprint_str[16*3];
    ptrlen algname;
    const ssh_keyalg *alg;
    int i;
    BinarySource src[1];

    /*
     * The fingerprint hash itself is always just the MD5 of the blob.
     */
    hash_simple(&ssh_md5, blob, digest);
    for (i = 0; i < 16; i++)
        sprintf(fingerprint_str + i*3, "%02x%s", digest[i], i==15 ? "" : ":");

    /*
     * Identify the key algorithm, if possible.
     */
    BinarySource_BARE_INIT_PL(src, blob);
    algname = get_string(src);
    if (!get_err(src)) {
        alg = find_pubkey_alg_len(algname);
        if (alg) {
            int bits = ssh_key_public_bits(alg, blob);
            return dupprintf("%.*s %d %s", PTRLEN_PRINTF(algname),
                             bits, fingerprint_str);
        } else {
            return dupprintf("%.*s %s", PTRLEN_PRINTF(algname),
                             fingerprint_str);
        }
    } else {
        /*
         * No algorithm available (which means a seriously confused
         * key blob, but there we go). Return only the hash.
         */
        return dupstr(fingerprint_str);
    }
}

char *ssh2_fingerprint(ssh_key *data)
{
    strbuf *blob = strbuf_new();
    ssh_key_public_blob(data, BinarySink_UPCAST(blob));
    char *ret = ssh2_fingerprint_blob(ptrlen_from_strbuf(blob));
    strbuf_free(blob);
    return ret;
}

/* ----------------------------------------------------------------------
 * Determine the type of a private key file.
 */
static int key_type_fp(FILE *fp)
{
    char buf[1024];
    const char public_std_sig[] = "---- BEGIN SSH2 PUBLIC KEY";
    const char putty2_sig[] = "PuTTY-User-Key-File-";
    const char sshcom_sig[] = "---- BEGIN SSH2 ENCRYPTED PRIVAT";
    const char openssh_new_sig[] = "-----BEGIN OPENSSH PRIVATE KEY";
    const char openssh_sig[] = "-----BEGIN ";
    int i;
    char *p;

    i = fread(buf, 1, sizeof(buf)-1, fp);
    rewind(fp);

    if (i < 0)
	return SSH_KEYTYPE_UNOPENABLE;
    if (i < 32)
	return SSH_KEYTYPE_UNKNOWN;
    assert(i > 0 && i < sizeof(buf));
    buf[i] = '\0';
    if (!memcmp(buf, rsa_signature, sizeof(rsa_signature)-1))
	return SSH_KEYTYPE_SSH1;
    if (!memcmp(buf, public_std_sig, sizeof(public_std_sig)-1))
	return SSH_KEYTYPE_SSH2_PUBLIC_RFC4716;
    if (!memcmp(buf, putty2_sig, sizeof(putty2_sig)-1))
	return SSH_KEYTYPE_SSH2;
    if (!memcmp(buf, openssh_new_sig, sizeof(openssh_new_sig)-1))
	return SSH_KEYTYPE_OPENSSH_NEW;
    if (!memcmp(buf, openssh_sig, sizeof(openssh_sig)-1))
	return SSH_KEYTYPE_OPENSSH_PEM;
    if (!memcmp(buf, sshcom_sig, sizeof(sshcom_sig)-1))
	return SSH_KEYTYPE_SSHCOM;
    if ((p = buf + strspn(buf, "0123456789"), *p == ' ') &&
        (p = p+1 + strspn(p+1, "0123456789"), *p == ' ') &&
        (p = p+1 + strspn(p+1, "0123456789"), *p == ' ' || *p == '\n' || !*p))
	return SSH_KEYTYPE_SSH1_PUBLIC;
    if ((p = buf + strcspn(buf, " "),
         find_pubkey_alg_len(make_ptrlen(buf, p-buf))) &&
        (p = p+1 + strspn(p+1, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghij"
                          "klmnopqrstuvwxyz+/="),
         *p == ' ' || *p == '\n' || !*p))
	return SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH;
    return SSH_KEYTYPE_UNKNOWN;	       /* unrecognised or EOF */
}

int key_type(const Filename *filename)
{
    FILE *fp;
    int ret;

    fp = f_open(filename, "r", false);
    if (!fp)
	return SSH_KEYTYPE_UNOPENABLE;
    ret = key_type_fp(fp);
    fclose(fp);
    return ret;
}

/*
 * Convert the type word to a string, for `wrong type' error
 * messages.
 */
const char *key_type_to_str(int type)
{
    switch (type) {
      case SSH_KEYTYPE_UNOPENABLE: return "unable to open file"; break;
      case SSH_KEYTYPE_UNKNOWN: return "not a recognised key file format"; break;
      case SSH_KEYTYPE_SSH1_PUBLIC: return "SSH-1 public key"; break;
      case SSH_KEYTYPE_SSH2_PUBLIC_RFC4716: return "SSH-2 public key (RFC 4716 format)"; break;
      case SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH: return "SSH-2 public key (OpenSSH format)"; break;
      case SSH_KEYTYPE_SSH1: return "SSH-1 private key"; break;
      case SSH_KEYTYPE_SSH2: return "PuTTY SSH-2 private key"; break;
      case SSH_KEYTYPE_OPENSSH_PEM: return "OpenSSH SSH-2 private key (old PEM format)"; break;
      case SSH_KEYTYPE_OPENSSH_NEW: return "OpenSSH SSH-2 private key (new format)"; break;
      case SSH_KEYTYPE_SSHCOM: return "ssh.com SSH-2 private key"; break;
        /*
         * This function is called with a key type derived from
         * looking at an actual key file, so the output-only type
         * OPENSSH_AUTO should never get here, and is much an INTERNAL
         * ERROR as a code we don't even understand.
         */
      case SSH_KEYTYPE_OPENSSH_AUTO: return "INTERNAL ERROR (OPENSSH_AUTO)"; break;
      default: return "INTERNAL ERROR"; break;
    }
}
