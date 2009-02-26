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
#include "ssh.h"
#include "misc.h"

#define rsa_signature "SSH PRIVATE KEY FILE FORMAT 1.1\n"

#define BASE64_TOINT(x) ( (x)-'A'<26 ? (x)-'A'+0 :\
                          (x)-'a'<26 ? (x)-'a'+26 :\
                          (x)-'0'<10 ? (x)-'0'+52 :\
                          (x)=='+' ? 62 : \
                          (x)=='/' ? 63 : 0 )

static int loadrsakey_main(FILE * fp, struct RSAKey *key, int pub_only,
			   char **commentptr, char *passphrase,
			   const char **error)
{
    unsigned char buf[16384];
    unsigned char keybuf[16];
    int len;
    int i, j, ciphertype;
    int ret = 0;
    struct MD5Context md5c;
    char *comment;

    *error = NULL;

    /* Slurp the whole file (minus the header) into a buffer. */
    len = fread(buf, 1, sizeof(buf), fp);
    fclose(fp);
    if (len < 0 || len == sizeof(buf)) {
	*error = "error reading file";
	goto end;		       /* file too big or not read */
    }

    i = 0;
    *error = "file format error";

    /*
     * A zero byte. (The signature includes a terminating NUL.)
     */
    if (len - i < 1 || buf[i] != 0)
	goto end;
    i++;

    /* One byte giving encryption type, and one reserved uint32. */
    if (len - i < 1)
	goto end;
    ciphertype = buf[i];
    if (ciphertype != 0 && ciphertype != SSH_CIPHER_3DES)
	goto end;
    i++;
    if (len - i < 4)
	goto end;		       /* reserved field not present */
    if (buf[i] != 0 || buf[i + 1] != 0 || buf[i + 2] != 0
	|| buf[i + 3] != 0) goto end;  /* reserved field nonzero, panic! */
    i += 4;

    /* Now the serious stuff. An ordinary SSH-1 public key. */
    i += makekey(buf + i, len, key, NULL, 1);
    if (i < 0)
	goto end;		       /* overran */

    /* Next, the comment field. */
    j = GET_32BIT(buf + i);
    i += 4;
    if (len - i < j)
	goto end;
    comment = snewn(j + 1, char);
    if (comment) {
	memcpy(comment, buf + i, j);
	comment[j] = '\0';
    }
    i += j;
    if (commentptr)
	*commentptr = dupstr(comment);
    if (key)
	key->comment = comment;
    else
	sfree(comment);

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
	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Final(keybuf, &md5c);
	des3_decrypt_pubkey(keybuf, buf + i, (len - i + 7) & ~7);
	memset(keybuf, 0, sizeof(keybuf));	/* burn the evidence */
    }

    /*
     * We are now in the secret part of the key. The first four
     * bytes should be of the form a, b, a, b.
     */
    if (len - i < 4)
	goto end;
    if (buf[i] != buf[i + 2] || buf[i + 1] != buf[i + 3]) {
	*error = "wrong passphrase";
	ret = -1;
	goto end;
    }
    i += 4;

    /*
     * After that, we have one further bignum which is our
     * decryption exponent, and then the three auxiliary values
     * (iqmp, q, p).
     */
    j = makeprivate(buf + i, len - i, key);
    if (j < 0) goto end;
    i += j;
    j = ssh1_read_bignum(buf + i, len - i, &key->iqmp);
    if (j < 0) goto end;
    i += j;
    j = ssh1_read_bignum(buf + i, len - i, &key->q);
    if (j < 0) goto end;
    i += j;
    j = ssh1_read_bignum(buf + i, len - i, &key->p);
    if (j < 0) goto end;
    i += j;

    if (!rsa_verify(key)) {
	*error = "rsa_verify failed";
	freersakey(key);
	ret = 0;
    } else
	ret = 1;

  end:
    memset(buf, 0, sizeof(buf));       /* burn the evidence */
    return ret;
}

int loadrsakey(const Filename *filename, struct RSAKey *key, char *passphrase,
	       const char **errorstr)
{
    FILE *fp;
    char buf[64];
    int ret = 0;
    const char *error = NULL;

    fp = f_open(*filename, "rb", FALSE);
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
	ret = loadrsakey_main(fp, key, FALSE, NULL, passphrase, &error);
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
int rsakey_encrypted(const Filename *filename, char **comment)
{
    FILE *fp;
    char buf[64];

    fp = f_open(*filename, "rb", FALSE);
    if (!fp)
	return 0;		       /* doesn't even exist */

    /*
     * Read the first line of the file and see if it's a v1 private
     * key file.
     */
    if (fgets(buf, sizeof(buf), fp) && !strcmp(buf, rsa_signature)) {
	const char *dummy;
	/*
	 * This routine will take care of calling fclose() for us.
	 */
	return loadrsakey_main(fp, NULL, FALSE, comment, NULL, &dummy);
    }
    fclose(fp);
    return 0;			       /* wasn't the right kind of file */
}

/*
 * Return a malloc'ed chunk of memory containing the public blob of
 * an RSA key, as given in the agent protocol (modulus bits,
 * exponent, modulus).
 */
int rsakey_pubblob(const Filename *filename, void **blob, int *bloblen,
		   char **commentptr, const char **errorstr)
{
    FILE *fp;
    char buf[64];
    struct RSAKey key;
    int ret;
    const char *error = NULL;

    /* Default return if we fail. */
    *blob = NULL;
    *bloblen = 0;
    ret = 0;

    fp = f_open(*filename, "rb", FALSE);
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
	if (loadrsakey_main(fp, &key, TRUE, commentptr, NULL, &error)) {
	    *blob = rsa_public_blob(&key, bloblen);
	    freersakey(&key);
	    ret = 1;
	    fp = NULL;
	}
    } else {
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
 * Save an RSA key file. Return nonzero on success.
 */
int saversakey(const Filename *filename, struct RSAKey *key, char *passphrase)
{
    unsigned char buf[16384];
    unsigned char keybuf[16];
    struct MD5Context md5c;
    unsigned char *p, *estart;
    FILE *fp;

    /*
     * Write the initial signature.
     */
    p = buf;
    memcpy(p, rsa_signature, sizeof(rsa_signature));
    p += sizeof(rsa_signature);

    /*
     * One byte giving encryption type, and one reserved (zero)
     * uint32.
     */
    *p++ = (passphrase ? SSH_CIPHER_3DES : 0);
    PUT_32BIT(p, 0);
    p += 4;

    /*
     * An ordinary SSH-1 public key consists of: a uint32
     * containing the bit count, then two bignums containing the
     * modulus and exponent respectively.
     */
    PUT_32BIT(p, bignum_bitcount(key->modulus));
    p += 4;
    p += ssh1_write_bignum(p, key->modulus);
    p += ssh1_write_bignum(p, key->exponent);

    /*
     * A string containing the comment field.
     */
    if (key->comment) {
	PUT_32BIT(p, strlen(key->comment));
	p += 4;
	memcpy(p, key->comment, strlen(key->comment));
	p += strlen(key->comment);
    } else {
	PUT_32BIT(p, 0);
	p += 4;
    }

    /*
     * The encrypted portion starts here.
     */
    estart = p;

    /*
     * Two bytes, then the same two bytes repeated.
     */
    *p++ = random_byte();
    *p++ = random_byte();
    p[0] = p[-2];
    p[1] = p[-1];
    p += 2;

    /*
     * Four more bignums: the decryption exponent, then iqmp, then
     * q, then p.
     */
    p += ssh1_write_bignum(p, key->private_exponent);
    p += ssh1_write_bignum(p, key->iqmp);
    p += ssh1_write_bignum(p, key->q);
    p += ssh1_write_bignum(p, key->p);

    /*
     * Now write zeros until the encrypted portion is a multiple of
     * 8 bytes.
     */
    while ((p - estart) % 8)
	*p++ = '\0';

    /*
     * Now encrypt the encrypted portion.
     */
    if (passphrase) {
	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Final(keybuf, &md5c);
	des3_encrypt_pubkey(keybuf, estart, p - estart);
	memset(keybuf, 0, sizeof(keybuf));	/* burn the evidence */
    }

    /*
     * Done. Write the result to the file.
     */
    fp = f_open(*filename, "wb", TRUE);
    if (fp) {
	int ret = (fwrite(buf, 1, p - buf, fp) == (size_t) (p - buf));
        if (fclose(fp))
            ret = 0;
	return ret;
    } else
	return 0;
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

static int read_header(FILE * fp, char *header)
{
    int len = 39;
    int c;

    while (len > 0) {
	c = fgetc(fp);
	if (c == '\n' || c == '\r' || c == EOF)
	    return 0;		       /* failure */
	if (c == ':') {
	    c = fgetc(fp);
	    if (c != ' ')
		return 0;
	    *header = '\0';
	    return 1;		       /* success! */
	}
	if (len == 0)
	    return 0;		       /* failure */
	*header++ = c;
	len--;
    }
    return 0;			       /* failure */
}

static char *read_body(FILE * fp)
{
    char *text;
    int len;
    int size;
    int c;

    size = 128;
    text = snewn(size, char);
    len = 0;
    text[len] = '\0';

    while (1) {
	c = fgetc(fp);
	if (c == '\r' || c == '\n' || c == EOF) {
	    if (c != EOF) {
		c = fgetc(fp);
		if (c != '\r' && c != '\n')
		    ungetc(c, fp);
	    }
	    return text;
	}
	if (len + 1 >= size) {
	    size += 128;
	    text = sresize(text, size, char);
	}
	text[len++] = c;
	text[len] = '\0';
    }
}

int base64_decode_atom(char *atom, unsigned char *out)
{
    int vals[4];
    int i, v, len;
    unsigned word;
    char c;

    for (i = 0; i < 4; i++) {
	c = atom[i];
	if (c >= 'A' && c <= 'Z')
	    v = c - 'A';
	else if (c >= 'a' && c <= 'z')
	    v = c - 'a' + 26;
	else if (c >= '0' && c <= '9')
	    v = c - '0' + 52;
	else if (c == '+')
	    v = 62;
	else if (c == '/')
	    v = 63;
	else if (c == '=')
	    v = -1;
	else
	    return 0;		       /* invalid atom */
	vals[i] = v;
    }

    if (vals[0] == -1 || vals[1] == -1)
	return 0;
    if (vals[2] == -1 && vals[3] != -1)
	return 0;

    if (vals[3] != -1)
	len = 3;
    else if (vals[2] != -1)
	len = 2;
    else
	len = 1;

    word = ((vals[0] << 18) |
	    (vals[1] << 12) | ((vals[2] & 0x3F) << 6) | (vals[3] & 0x3F));
    out[0] = (word >> 16) & 0xFF;
    if (len > 1)
	out[1] = (word >> 8) & 0xFF;
    if (len > 2)
	out[2] = word & 0xFF;
    return len;
}

static unsigned char *read_blob(FILE * fp, int nlines, int *bloblen)
{
    unsigned char *blob;
    char *line;
    int linelen, len;
    int i, j, k;

    /* We expect at most 64 base64 characters, ie 48 real bytes, per line. */
    blob = snewn(48 * nlines, unsigned char);
    len = 0;
    for (i = 0; i < nlines; i++) {
	line = read_body(fp);
	if (!line) {
	    sfree(blob);
	    return NULL;
	}
	linelen = strlen(line);
	if (linelen % 4 != 0 || linelen > 64) {
	    sfree(blob);
	    sfree(line);
	    return NULL;
	}
	for (j = 0; j < linelen; j += 4) {
	    k = base64_decode_atom(line + j, blob + len);
	    if (!k) {
		sfree(line);
		sfree(blob);
		return NULL;
	    }
	    len += k;
	}
	sfree(line);
    }
    *bloblen = len;
    return blob;
}

/*
 * Magic error return value for when the passphrase is wrong.
 */
struct ssh2_userkey ssh2_wrong_passphrase = {
    NULL, NULL, NULL
};

const struct ssh_signkey *find_pubkey_alg(const char *name)
{
    if (!strcmp(name, "ssh-rsa"))
	return &ssh_rsa;
    else if (!strcmp(name, "ssh-dss"))
	return &ssh_dss;
    else
	return NULL;
}

struct ssh2_userkey *ssh2_load_userkey(const Filename *filename,
				       char *passphrase, const char **errorstr)
{
    FILE *fp;
    char header[40], *b, *encryption, *comment, *mac;
    const struct ssh_signkey *alg;
    struct ssh2_userkey *ret;
    int cipher, cipherblk;
    unsigned char *public_blob, *private_blob;
    int public_blob_len, private_blob_len;
    int i, is_mac, old_fmt;
    int passlen = passphrase ? strlen(passphrase) : 0;
    const char *error = NULL;

    ret = NULL;			       /* return NULL for most errors */
    encryption = comment = mac = NULL;
    public_blob = private_blob = NULL;

    fp = f_open(*filename, "rb", FALSE);
    if (!fp) {
	error = "can't open file";
	goto error;
    }

    /* Read the first header line which contains the key type. */
    if (!read_header(fp, header))
	goto error;
    if (0 == strcmp(header, "PuTTY-User-Key-File-2")) {
	old_fmt = 0;
    } else if (0 == strcmp(header, "PuTTY-User-Key-File-1")) {
	/* this is an old key file; warn and then continue */
	old_keyfile_warning();
	old_fmt = 1;
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
	sfree(encryption);
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
    i = atoi(b);
    sfree(b);
    if ((public_blob = read_blob(fp, i, &public_blob_len)) == NULL)
	goto error;

    /* Read the Private-Lines header line and the Private blob. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Private-Lines"))
	goto error;
    if ((b = read_body(fp)) == NULL)
	goto error;
    i = atoi(b);
    sfree(b);
    if ((private_blob = read_blob(fp, i, &private_blob_len)) == NULL)
	goto error;

    /* Read the Private-MAC or Private-Hash header line. */
    if (!read_header(fp, header))
	goto error;
    if (0 == strcmp(header, "Private-MAC")) {
	if ((mac = read_body(fp)) == NULL)
	    goto error;
	is_mac = 1;
    } else if (0 == strcmp(header, "Private-Hash") && old_fmt) {
	if ((mac = read_body(fp)) == NULL)
	    goto error;
	is_mac = 0;
    } else
	goto error;

    fclose(fp);
    fp = NULL;

    /*
     * Decrypt the private blob.
     */
    if (cipher) {
	unsigned char key[40];
	SHA_State s;

	if (!passphrase)
	    goto error;
	if (private_blob_len % cipherblk)
	    goto error;

	SHA_Init(&s);
	SHA_Bytes(&s, "\0\0\0\0", 4);
	SHA_Bytes(&s, passphrase, passlen);
	SHA_Final(&s, key + 0);
	SHA_Init(&s);
	SHA_Bytes(&s, "\0\0\0\1", 4);
	SHA_Bytes(&s, passphrase, passlen);
	SHA_Final(&s, key + 20);
	aes256_decrypt_pubkey(key, private_blob, private_blob_len);
    }

    /*
     * Verify the MAC.
     */
    {
	char realmac[41];
	unsigned char binary[20];
	unsigned char *macdata;
	int maclen;
	int free_macdata;

	if (old_fmt) {
	    /* MAC (or hash) only covers the private blob. */
	    macdata = private_blob;
	    maclen = private_blob_len;
	    free_macdata = 0;
	} else {
	    unsigned char *p;
	    int namelen = strlen(alg->name);
	    int enclen = strlen(encryption);
	    int commlen = strlen(comment);
	    maclen = (4 + namelen +
		      4 + enclen +
		      4 + commlen +
		      4 + public_blob_len +
		      4 + private_blob_len);
	    macdata = snewn(maclen, unsigned char);
	    p = macdata;
#define DO_STR(s,len) PUT_32BIT(p,(len));memcpy(p+4,(s),(len));p+=4+(len)
	    DO_STR(alg->name, namelen);
	    DO_STR(encryption, enclen);
	    DO_STR(comment, commlen);
	    DO_STR(public_blob, public_blob_len);
	    DO_STR(private_blob, private_blob_len);

	    free_macdata = 1;
	}

	if (is_mac) {
	    SHA_State s;
	    unsigned char mackey[20];
	    char header[] = "putty-private-key-file-mac-key";

	    SHA_Init(&s);
	    SHA_Bytes(&s, header, sizeof(header)-1);
	    if (cipher && passphrase)
		SHA_Bytes(&s, passphrase, passlen);
	    SHA_Final(&s, mackey);

	    hmac_sha1_simple(mackey, 20, macdata, maclen, binary);

	    memset(mackey, 0, sizeof(mackey));
	    memset(&s, 0, sizeof(s));
	} else {
	    SHA_Simple(macdata, maclen, binary);
	}

	if (free_macdata) {
	    memset(macdata, 0, maclen);
	    sfree(macdata);
	}

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

    /*
     * Create and return the key.
     */
    ret = snew(struct ssh2_userkey);
    ret->alg = alg;
    ret->comment = comment;
    ret->data = alg->createkey(public_blob, public_blob_len,
			       private_blob, private_blob_len);
    if (!ret->data) {
	sfree(ret->comment);
	sfree(ret);
	ret = NULL;
	error = "createkey failed";
	goto error;
    }
    sfree(public_blob);
    sfree(private_blob);
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
	sfree(public_blob);
    if (private_blob)
	sfree(private_blob);
    if (errorstr)
	*errorstr = error;
    return ret;
}

unsigned char *ssh2_userkey_loadpub(const Filename *filename, char **algorithm,
				    int *pub_blob_len, char **commentptr,
				    const char **errorstr)
{
    FILE *fp;
    char header[40], *b;
    const struct ssh_signkey *alg;
    unsigned char *public_blob;
    int public_blob_len;
    int i;
    const char *error = NULL;
    char *comment;

    public_blob = NULL;

    fp = f_open(*filename, "rb", FALSE);
    if (!fp) {
	error = "can't open file";
	goto error;
    }

    /* Read the first header line which contains the key type. */
    if (!read_header(fp, header)
	|| (0 != strcmp(header, "PuTTY-User-Key-File-2") &&
	    0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
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
    i = atoi(b);
    sfree(b);
    if ((public_blob = read_blob(fp, i, &public_blob_len)) == NULL)
	goto error;

    fclose(fp);
    if (pub_blob_len)
	*pub_blob_len = public_blob_len;
    if (algorithm)
	*algorithm = alg->name;
    return public_blob;

    /*
     * Error processing.
     */
  error:
    if (fp)
	fclose(fp);
    if (public_blob)
	sfree(public_blob);
    if (errorstr)
	*errorstr = error;
    return NULL;
}

int ssh2_userkey_encrypted(const Filename *filename, char **commentptr)
{
    FILE *fp;
    char header[40], *b, *comment;
    int ret;

    if (commentptr)
	*commentptr = NULL;

    fp = f_open(*filename, "rb", FALSE);
    if (!fp)
	return 0;
    if (!read_header(fp, header)
	|| (0 != strcmp(header, "PuTTY-User-Key-File-2") &&
	    0 != strcmp(header, "PuTTY-User-Key-File-1"))) {
	fclose(fp);
	return 0;
    }
    if ((b = read_body(fp)) == NULL) {
	fclose(fp);
	return 0;
    }
    sfree(b);			       /* we don't care about key type here */
    /* Read the Encryption header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Encryption")) {
	fclose(fp);
	return 0;
    }
    if ((b = read_body(fp)) == NULL) {
	fclose(fp);
	return 0;
    }

    /* Read the Comment header line. */
    if (!read_header(fp, header) || 0 != strcmp(header, "Comment")) {
	fclose(fp);
	sfree(b);
	return 1;
    }
    if ((comment = read_body(fp)) == NULL) {
	fclose(fp);
	sfree(b);
	return 1;
    }

    if (commentptr)
	*commentptr = comment;

    fclose(fp);
    if (!strcmp(b, "aes256-cbc"))
	ret = 1;
    else
	ret = 0;
    sfree(b);
    return ret;
}

int base64_lines(int datalen)
{
    /* When encoding, we use 64 chars/line, which equals 48 real chars. */
    return (datalen + 47) / 48;
}

void base64_encode(FILE * fp, unsigned char *data, int datalen, int cpl)
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

int ssh2_save_userkey(const Filename *filename, struct ssh2_userkey *key,
		      char *passphrase)
{
    FILE *fp;
    unsigned char *pub_blob, *priv_blob, *priv_blob_encrypted;
    int pub_blob_len, priv_blob_len, priv_encrypted_len;
    int passlen;
    int cipherblk;
    int i;
    char *cipherstr;
    unsigned char priv_mac[20];

    /*
     * Fetch the key component blobs.
     */
    pub_blob = key->alg->public_blob(key->data, &pub_blob_len);
    priv_blob = key->alg->private_blob(key->data, &priv_blob_len);
    if (!pub_blob || !priv_blob) {
	sfree(pub_blob);
	sfree(priv_blob);
	return 0;
    }

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
    priv_encrypted_len = priv_blob_len + cipherblk - 1;
    priv_encrypted_len -= priv_encrypted_len % cipherblk;
    priv_blob_encrypted = snewn(priv_encrypted_len, unsigned char);
    memset(priv_blob_encrypted, 0, priv_encrypted_len);
    memcpy(priv_blob_encrypted, priv_blob, priv_blob_len);
    /* Create padding based on the SHA hash of the unpadded blob. This prevents
     * too easy a known-plaintext attack on the last block. */
    SHA_Simple(priv_blob, priv_blob_len, priv_mac);
    assert(priv_encrypted_len - priv_blob_len < 20);
    memcpy(priv_blob_encrypted + priv_blob_len, priv_mac,
	   priv_encrypted_len - priv_blob_len);

    /* Now create the MAC. */
    {
	unsigned char *macdata;
	int maclen;
	unsigned char *p;
	int namelen = strlen(key->alg->name);
	int enclen = strlen(cipherstr);
	int commlen = strlen(key->comment);
	SHA_State s;
	unsigned char mackey[20];
	char header[] = "putty-private-key-file-mac-key";

	maclen = (4 + namelen +
		  4 + enclen +
		  4 + commlen +
		  4 + pub_blob_len +
		  4 + priv_encrypted_len);
	macdata = snewn(maclen, unsigned char);
	p = macdata;
#define DO_STR(s,len) PUT_32BIT(p,(len));memcpy(p+4,(s),(len));p+=4+(len)
	DO_STR(key->alg->name, namelen);
	DO_STR(cipherstr, enclen);
	DO_STR(key->comment, commlen);
	DO_STR(pub_blob, pub_blob_len);
	DO_STR(priv_blob_encrypted, priv_encrypted_len);

	SHA_Init(&s);
	SHA_Bytes(&s, header, sizeof(header)-1);
	if (passphrase)
	    SHA_Bytes(&s, passphrase, strlen(passphrase));
	SHA_Final(&s, mackey);
	hmac_sha1_simple(mackey, 20, macdata, maclen, priv_mac);
	memset(macdata, 0, maclen);
	sfree(macdata);
	memset(mackey, 0, sizeof(mackey));
	memset(&s, 0, sizeof(s));
    }

    if (passphrase) {
	unsigned char key[40];
	SHA_State s;

	passlen = strlen(passphrase);

	SHA_Init(&s);
	SHA_Bytes(&s, "\0\0\0\0", 4);
	SHA_Bytes(&s, passphrase, passlen);
	SHA_Final(&s, key + 0);
	SHA_Init(&s);
	SHA_Bytes(&s, "\0\0\0\1", 4);
	SHA_Bytes(&s, passphrase, passlen);
	SHA_Final(&s, key + 20);
	aes256_encrypt_pubkey(key, priv_blob_encrypted,
			      priv_encrypted_len);

	memset(key, 0, sizeof(key));
	memset(&s, 0, sizeof(s));
    }

    fp = f_open(*filename, "w", TRUE);
    if (!fp)
	return 0;
    fprintf(fp, "PuTTY-User-Key-File-2: %s\n", key->alg->name);
    fprintf(fp, "Encryption: %s\n", cipherstr);
    fprintf(fp, "Comment: %s\n", key->comment);
    fprintf(fp, "Public-Lines: %d\n", base64_lines(pub_blob_len));
    base64_encode(fp, pub_blob, pub_blob_len, 64);
    fprintf(fp, "Private-Lines: %d\n", base64_lines(priv_encrypted_len));
    base64_encode(fp, priv_blob_encrypted, priv_encrypted_len, 64);
    fprintf(fp, "Private-MAC: ");
    for (i = 0; i < 20; i++)
	fprintf(fp, "%02x", priv_mac[i]);
    fprintf(fp, "\n");
    fclose(fp);

    sfree(pub_blob);
    memset(priv_blob, 0, priv_blob_len);
    sfree(priv_blob);
    sfree(priv_blob_encrypted);
    return 1;
}

/* ----------------------------------------------------------------------
 * A function to determine the type of a private key file. Returns
 * 0 on failure, 1 or 2 on success.
 */
int key_type(const Filename *filename)
{
    FILE *fp;
    char buf[32];
    const char putty2_sig[] = "PuTTY-User-Key-File-";
    const char sshcom_sig[] = "---- BEGIN SSH2 ENCRYPTED PRIVAT";
    const char openssh_sig[] = "-----BEGIN ";
    int i;

    fp = f_open(*filename, "r", FALSE);
    if (!fp)
	return SSH_KEYTYPE_UNOPENABLE;
    i = fread(buf, 1, sizeof(buf), fp);
    fclose(fp);
    if (i < 0)
	return SSH_KEYTYPE_UNOPENABLE;
    if (i < 32)
	return SSH_KEYTYPE_UNKNOWN;
    if (!memcmp(buf, rsa_signature, sizeof(rsa_signature)-1))
	return SSH_KEYTYPE_SSH1;
    if (!memcmp(buf, putty2_sig, sizeof(putty2_sig)-1))
	return SSH_KEYTYPE_SSH2;
    if (!memcmp(buf, openssh_sig, sizeof(openssh_sig)-1))
	return SSH_KEYTYPE_OPENSSH;
    if (!memcmp(buf, sshcom_sig, sizeof(sshcom_sig)-1))
	return SSH_KEYTYPE_SSHCOM;
    return SSH_KEYTYPE_UNKNOWN;	       /* unrecognised or EOF */
}

/*
 * Convert the type word to a string, for `wrong type' error
 * messages.
 */
char *key_type_to_str(int type)
{
    switch (type) {
      case SSH_KEYTYPE_UNOPENABLE: return "unable to open file"; break;
      case SSH_KEYTYPE_UNKNOWN: return "not a private key"; break;
      case SSH_KEYTYPE_SSH1: return "SSH-1 private key"; break;
      case SSH_KEYTYPE_SSH2: return "PuTTY SSH-2 private key"; break;
      case SSH_KEYTYPE_OPENSSH: return "OpenSSH SSH-2 private key"; break;
      case SSH_KEYTYPE_SSHCOM: return "ssh.com SSH-2 private key"; break;
      default: return "INTERNAL ERROR"; break;
    }
}
