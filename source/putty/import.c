/*
 * Code for PuTTY to import and export private key files in other
 * SSH clients' formats.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "putty.h"
#include "ssh.h"
#include "misc.h"

int openssh_encrypted(const Filename *filename);
struct ssh2_userkey *openssh_read(const Filename *filename, char *passphrase,
				  const char **errmsg_p);
int openssh_write(const Filename *filename, struct ssh2_userkey *key,
		  char *passphrase);

int sshcom_encrypted(const Filename *filename, char **comment);
struct ssh2_userkey *sshcom_read(const Filename *filename, char *passphrase,
				 const char **errmsg_p);
int sshcom_write(const Filename *filename, struct ssh2_userkey *key,
		 char *passphrase);

/*
 * Given a key type, determine whether we know how to import it.
 */
int import_possible(int type)
{
    if (type == SSH_KEYTYPE_OPENSSH)
	return 1;
    if (type == SSH_KEYTYPE_SSHCOM)
	return 1;
    return 0;
}

/*
 * Given a key type, determine what native key type
 * (SSH_KEYTYPE_SSH1 or SSH_KEYTYPE_SSH2) it will come out as once
 * we've imported it.
 */
int import_target_type(int type)
{
    /*
     * There are no known foreign SSH-1 key formats.
     */
    return SSH_KEYTYPE_SSH2;
}

/*
 * Determine whether a foreign key is encrypted.
 */
int import_encrypted(const Filename *filename, int type, char **comment)
{
    if (type == SSH_KEYTYPE_OPENSSH) {
	/* OpenSSH doesn't do key comments */
	*comment = dupstr(filename_to_str(filename));
	return openssh_encrypted(filename);
    }
    if (type == SSH_KEYTYPE_SSHCOM) {
	return sshcom_encrypted(filename, comment);
    }
    return 0;
}

/*
 * Import an SSH-1 key.
 */
int import_ssh1(const Filename *filename, int type,
		struct RSAKey *key, char *passphrase, const char **errmsg_p)
{
    return 0;
}

/*
 * Import an SSH-2 key.
 */
struct ssh2_userkey *import_ssh2(const Filename *filename, int type,
				 char *passphrase, const char **errmsg_p)
{
    if (type == SSH_KEYTYPE_OPENSSH)
	return openssh_read(filename, passphrase, errmsg_p);
    if (type == SSH_KEYTYPE_SSHCOM)
	return sshcom_read(filename, passphrase, errmsg_p);
    return NULL;
}

/*
 * Export an SSH-1 key.
 */
int export_ssh1(const Filename *filename, int type, struct RSAKey *key,
		char *passphrase)
{
    return 0;
}

/*
 * Export an SSH-2 key.
 */
int export_ssh2(const Filename *filename, int type,
                struct ssh2_userkey *key, char *passphrase)
{
    if (type == SSH_KEYTYPE_OPENSSH)
	return openssh_write(filename, key, passphrase);
    if (type == SSH_KEYTYPE_SSHCOM)
	return sshcom_write(filename, key, passphrase);
    return 0;
}

/*
 * Strip trailing CRs and LFs at the end of a line of text.
 */
void strip_crlf(char *str)
{
    char *p = str + strlen(str);

    while (p > str && (p[-1] == '\r' || p[-1] == '\n'))
	*--p = '\0';
}

/* ----------------------------------------------------------------------
 * Helper routines. (The base64 ones are defined in sshpubk.c.)
 */

#define isbase64(c) (    ((c) >= 'A' && (c) <= 'Z') || \
                         ((c) >= 'a' && (c) <= 'z') || \
                         ((c) >= '0' && (c) <= '9') || \
                         (c) == '+' || (c) == '/' || (c) == '=' \
                         )

/*
 * Read an ASN.1/BER identifier and length pair.
 * 
 * Flags are a combination of the #defines listed below.
 * 
 * Returns -1 if unsuccessful; otherwise returns the number of
 * bytes used out of the source data.
 */

/* ASN.1 tag classes. */
#define ASN1_CLASS_UNIVERSAL        (0 << 6)
#define ASN1_CLASS_APPLICATION      (1 << 6)
#define ASN1_CLASS_CONTEXT_SPECIFIC (2 << 6)
#define ASN1_CLASS_PRIVATE          (3 << 6)
#define ASN1_CLASS_MASK             (3 << 6)

/* Primitive versus constructed bit. */
#define ASN1_CONSTRUCTED            (1 << 5)

static int ber_read_id_len(void *source, int sourcelen,
			   int *id, int *length, int *flags)
{
    unsigned char *p = (unsigned char *) source;

    if (sourcelen == 0)
	return -1;

    *flags = (*p & 0xE0);
    if ((*p & 0x1F) == 0x1F) {
	*id = 0;
	while (*p & 0x80) {
	    p++, sourcelen--;
	    if (sourcelen == 0)
		return -1;
	    *id = (*id << 7) | (*p & 0x7F);
	}
	p++, sourcelen--;
    } else {
	*id = *p & 0x1F;
	p++, sourcelen--;
    }

    if (sourcelen == 0)
	return -1;

    if (*p & 0x80) {
	int n = *p & 0x7F;
	p++, sourcelen--;
	if (sourcelen < n)
	    return -1;
	*length = 0;
	while (n--)
	    *length = (*length << 8) | (*p++);
	sourcelen -= n;
    } else {
	*length = *p;
	p++, sourcelen--;
    }

    return p - (unsigned char *) source;
}

/*
 * Write an ASN.1/BER identifier and length pair. Returns the
 * number of bytes consumed. Assumes dest contains enough space.
 * Will avoid writing anything if dest is NULL, but still return
 * amount of space required.
 */
static int ber_write_id_len(void *dest, int id, int length, int flags)
{
    unsigned char *d = (unsigned char *)dest;
    int len = 0;

    if (id <= 30) {
	/*
	 * Identifier is one byte.
	 */
	len++;
	if (d) *d++ = id | flags;
    } else {
	int n;
	/*
	 * Identifier is multiple bytes: the first byte is 11111
	 * plus the flags, and subsequent bytes encode the value of
	 * the identifier, 7 bits at a time, with the top bit of
	 * each byte 1 except the last one which is 0.
	 */
	len++;
	if (d) *d++ = 0x1F | flags;
	for (n = 1; (id >> (7*n)) > 0; n++)
	    continue;		       /* count the bytes */
	while (n--) {
	    len++;
	    if (d) *d++ = (n ? 0x80 : 0) | ((id >> (7*n)) & 0x7F);
	}
    }

    if (length < 128) {
	/*
	 * Length is one byte.
	 */
	len++;
	if (d) *d++ = length;
    } else {
	int n;
	/*
	 * Length is multiple bytes. The first is 0x80 plus the
	 * number of subsequent bytes, and the subsequent bytes
	 * encode the actual length.
	 */
	for (n = 1; (length >> (8*n)) > 0; n++)
	    continue;		       /* count the bytes */
	len++;
	if (d) *d++ = 0x80 | n;
	while (n--) {
	    len++;
	    if (d) *d++ = (length >> (8*n)) & 0xFF;
	}
    }

    return len;
}

static int put_string(void *target, void *data, int len)
{
    unsigned char *d = (unsigned char *)target;

    PUT_32BIT(d, len);
    memcpy(d+4, data, len);
    return len+4;
}

static int put_mp(void *target, void *data, int len)
{
    unsigned char *d = (unsigned char *)target;
    unsigned char *i = (unsigned char *)data;

    if (*i & 0x80) {
        PUT_32BIT(d, len+1);
        d[4] = 0;
        memcpy(d+5, data, len);
        return len+5;
    } else {
        PUT_32BIT(d, len);
        memcpy(d+4, data, len);
        return len+4;
    }
}

/* Simple structure to point to an mp-int within a blob. */
struct mpint_pos { void *start; int bytes; };

static int ssh2_read_mpint(void *data, int len, struct mpint_pos *ret)
{
    int bytes;
    unsigned char *d = (unsigned char *) data;

    if (len < 4)
        goto error;
    bytes = toint(GET_32BIT(d));
    if (bytes < 0 || len-4 < bytes)
        goto error;

    ret->start = d + 4;
    ret->bytes = bytes;
    return bytes+4;

    error:
    ret->start = NULL;
    ret->bytes = -1;
    return len;                        /* ensure further calls fail as well */
}

/* ----------------------------------------------------------------------
 * Code to read and write OpenSSH private keys.
 */

enum { OSSH_DSA, OSSH_RSA };
enum { OSSH_ENC_3DES, OSSH_ENC_AES };
struct openssh_key {
    int type;
    int encrypted, encryption;
    char iv[32];
    unsigned char *keyblob;
    int keyblob_len, keyblob_size;
};

static struct openssh_key *load_openssh_key(const Filename *filename,
					    const char **errmsg_p)
{
    struct openssh_key *ret;
    FILE *fp = NULL;
    char *line = NULL;
    char *errmsg, *p;
    int headers_done;
    char base64_bit[4];
    int base64_chars = 0;

    ret = snew(struct openssh_key);
    ret->keyblob = NULL;
    ret->keyblob_len = ret->keyblob_size = 0;
    ret->encrypted = 0;
    memset(ret->iv, 0, sizeof(ret->iv));

    fp = f_open(filename, "r", FALSE);
    if (!fp) {
	errmsg = "unable to open key file";
	goto error;
    }

    if (!(line = fgetline(fp))) {
	errmsg = "unexpected end of file";
	goto error;
    }
    strip_crlf(line);
    if (0 != strncmp(line, "-----BEGIN ", 11) ||
	0 != strcmp(line+strlen(line)-16, "PRIVATE KEY-----")) {
	errmsg = "file does not begin with OpenSSH key header";
	goto error;
    }
    if (!strcmp(line, "-----BEGIN RSA PRIVATE KEY-----"))
	ret->type = OSSH_RSA;
    else if (!strcmp(line, "-----BEGIN DSA PRIVATE KEY-----"))
	ret->type = OSSH_DSA;
    else {
	errmsg = "unrecognised key type";
	goto error;
    }
    smemclr(line, strlen(line));
    sfree(line);
    line = NULL;

    headers_done = 0;
    while (1) {
	if (!(line = fgetline(fp))) {
	    errmsg = "unexpected end of file";
	    goto error;
	}
	strip_crlf(line);
	if (0 == strncmp(line, "-----END ", 9) &&
	    0 == strcmp(line+strlen(line)-16, "PRIVATE KEY-----")) {
            sfree(line);
            line = NULL;
	    break;		       /* done */
        }
	if ((p = strchr(line, ':')) != NULL) {
	    if (headers_done) {
		errmsg = "header found in body of key data";
		goto error;
	    }
	    *p++ = '\0';
	    while (*p && isspace((unsigned char)*p)) p++;
	    if (!strcmp(line, "Proc-Type")) {
		if (p[0] != '4' || p[1] != ',') {
		    errmsg = "Proc-Type is not 4 (only 4 is supported)";
		    goto error;
		}
		p += 2;
		if (!strcmp(p, "ENCRYPTED"))
		    ret->encrypted = 1;
	    } else if (!strcmp(line, "DEK-Info")) {
		int i, j, ivlen;

		if (!strncmp(p, "DES-EDE3-CBC,", 13)) {
		    ret->encryption = OSSH_ENC_3DES;
		    ivlen = 8;
		} else if (!strncmp(p, "AES-128-CBC,", 12)) {
		    ret->encryption = OSSH_ENC_AES;
		    ivlen = 16;
		} else {
		    errmsg = "unsupported cipher";
		    goto error;
		}
		p = strchr(p, ',') + 1;/* always non-NULL, by above checks */
		for (i = 0; i < ivlen; i++) {
		    if (1 != sscanf(p, "%2x", &j)) {
			errmsg = "expected more iv data in DEK-Info";
			goto error;
		    }
		    ret->iv[i] = j;
		    p += 2;
		}
		if (*p) {
		    errmsg = "more iv data than expected in DEK-Info";
		    goto error;
		}
	    }
	} else {
	    headers_done = 1;

	    p = line;
	    while (isbase64(*p)) {
                base64_bit[base64_chars++] = *p;
                if (base64_chars == 4) {
                    unsigned char out[3];
                    int len;

                    base64_chars = 0;

                    len = base64_decode_atom(base64_bit, out);

                    if (len <= 0) {
                        errmsg = "invalid base64 encoding";
                        goto error;
                    }

                    if (ret->keyblob_len + len > ret->keyblob_size) {
                        ret->keyblob_size = ret->keyblob_len + len + 256;
                        ret->keyblob = sresize(ret->keyblob, ret->keyblob_size,
					       unsigned char);
                    }

                    memcpy(ret->keyblob + ret->keyblob_len, out, len);
                    ret->keyblob_len += len;

                    smemclr(out, sizeof(out));
                }

		p++;
	    }
	}
	smemclr(line, strlen(line));
	sfree(line);
	line = NULL;
    }

    fclose(fp);
    fp = NULL;

    if (ret->keyblob_len == 0 || !ret->keyblob) {
	errmsg = "key body not present";
	goto error;
    }

    if (ret->encrypted && ret->keyblob_len % 8 != 0) {
	errmsg = "encrypted key blob is not a multiple of cipher block size";
	goto error;
    }

    smemclr(base64_bit, sizeof(base64_bit));
    if (errmsg_p) *errmsg_p = NULL;
    return ret;

    error:
    if (line) {
	smemclr(line, strlen(line));
	sfree(line);
	line = NULL;
    }
    smemclr(base64_bit, sizeof(base64_bit));
    if (ret) {
	if (ret->keyblob) {
            smemclr(ret->keyblob, ret->keyblob_size);
            sfree(ret->keyblob);
        }
        smemclr(ret, sizeof(*ret));
	sfree(ret);
    }
    if (errmsg_p) *errmsg_p = errmsg;
    if (fp) fclose(fp);
    return NULL;
}

int openssh_encrypted(const Filename *filename)
{
    struct openssh_key *key = load_openssh_key(filename, NULL);
    int ret;

    if (!key)
	return 0;
    ret = key->encrypted;
    smemclr(key->keyblob, key->keyblob_size);
    sfree(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    return ret;
}

struct ssh2_userkey *openssh_read(const Filename *filename, char *passphrase,
				  const char **errmsg_p)
{
    struct openssh_key *key = load_openssh_key(filename, errmsg_p);
    struct ssh2_userkey *retkey;
    unsigned char *p;
    int ret, id, len, flags;
    int i, num_integers;
    struct ssh2_userkey *retval = NULL;
    char *errmsg;
    unsigned char *blob;
    int blobsize = 0, blobptr, privptr;
    char *modptr = NULL;
    int modlen = 0;

    blob = NULL;

    if (!key)
	return NULL;

    if (key->encrypted) {
	/*
	 * Derive encryption key from passphrase and iv/salt:
	 * 
	 *  - let block A equal MD5(passphrase || iv)
	 *  - let block B equal MD5(A || passphrase || iv)
	 *  - block C would be MD5(B || passphrase || iv) and so on
	 *  - encryption key is the first N bytes of A || B
	 *
	 * (Note that only 8 bytes of the iv are used for key
	 * derivation, even when the key is encrypted with AES and
	 * hence there are 16 bytes available.)
	 */
	struct MD5Context md5c;
	unsigned char keybuf[32];

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, (unsigned char *)key->iv, 8);
	MD5Final(keybuf, &md5c);

	MD5Init(&md5c);
	MD5Update(&md5c, keybuf, 16);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, (unsigned char *)key->iv, 8);
	MD5Final(keybuf+16, &md5c);

	/*
	 * Now decrypt the key blob.
	 */
	if (key->encryption == OSSH_ENC_3DES)
	    des3_decrypt_pubkey_ossh(keybuf, (unsigned char *)key->iv,
				     key->keyblob, key->keyblob_len);
	else {
	    void *ctx;
	    assert(key->encryption == OSSH_ENC_AES);
	    ctx = aes_make_context();
	    aes128_key(ctx, keybuf);
	    aes_iv(ctx, (unsigned char *)key->iv);
	    aes_ssh2_decrypt_blk(ctx, key->keyblob, key->keyblob_len);
	    aes_free_context(ctx);
	}

        smemclr(&md5c, sizeof(md5c));
        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * Now we have a decrypted key blob, which contains an ASN.1
     * encoded private key. We must now untangle the ASN.1.
     *
     * We expect the whole key blob to be formatted as a SEQUENCE
     * (0x30 followed by a length code indicating that the rest of
     * the blob is part of the sequence). Within that SEQUENCE we
     * expect to see a bunch of INTEGERs. What those integers mean
     * depends on the key type:
     *
     *  - For RSA, we expect the integers to be 0, n, e, d, p, q,
     *    dmp1, dmq1, iqmp in that order. (The last three are d mod
     *    (p-1), d mod (q-1), inverse of q mod p respectively.)
     *
     *  - For DSA, we expect them to be 0, p, q, g, y, x in that
     *    order.
     */
    
    p = key->keyblob;

    /* Expect the SEQUENCE header. Take its absence as a failure to
     * decrypt, if the key was encrypted. */
    ret = ber_read_id_len(p, key->keyblob_len, &id, &len, &flags);
    p += ret;
    if (ret < 0 || id != 16) {
	errmsg = "ASN.1 decoding failure";
        retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
	goto error;
    }

    /* Expect a load of INTEGERs. */
    if (key->type == OSSH_RSA)
	num_integers = 9;
    else if (key->type == OSSH_DSA)
	num_integers = 6;
    else
	num_integers = 0;	       /* placate compiler warnings */

    /*
     * Space to create key blob in.
     */
    blobsize = 256+key->keyblob_len;
    blob = snewn(blobsize, unsigned char);
    PUT_32BIT(blob, 7);
    if (key->type == OSSH_DSA)
	memcpy(blob+4, "ssh-dss", 7);
    else if (key->type == OSSH_RSA)
	memcpy(blob+4, "ssh-rsa", 7);
    blobptr = 4+7;
    privptr = -1;

    for (i = 0; i < num_integers; i++) {
	ret = ber_read_id_len(p, key->keyblob+key->keyblob_len-p,
			      &id, &len, &flags);
	p += ret;
	if (ret < 0 || id != 2 ||
	    key->keyblob+key->keyblob_len-p < len) {
	    errmsg = "ASN.1 decoding failure";
	    retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
	    goto error;
	}

	if (i == 0) {
	    /*
	     * The first integer should be zero always (I think
	     * this is some sort of version indication).
	     */
	    if (len != 1 || p[0] != 0) {
		errmsg = "version number mismatch";
		goto error;
	    }
	} else if (key->type == OSSH_RSA) {
	    /*
	     * Integers 1 and 2 go into the public blob but in the
	     * opposite order; integers 3, 4, 5 and 8 go into the
	     * private blob. The other two (6 and 7) are ignored.
	     */
	    if (i == 1) {
		/* Save the details for after we deal with number 2. */
		modptr = (char *)p;
		modlen = len;
	    } else if (i != 6 && i != 7) {
		PUT_32BIT(blob+blobptr, len);
		memcpy(blob+blobptr+4, p, len);
		blobptr += 4+len;
		if (i == 2) {
		    PUT_32BIT(blob+blobptr, modlen);
		    memcpy(blob+blobptr+4, modptr, modlen);
		    blobptr += 4+modlen;
		    privptr = blobptr;
		}
	    }
	} else if (key->type == OSSH_DSA) {
	    /*
	     * Integers 1-4 go into the public blob; integer 5 goes
	     * into the private blob.
	     */
	    PUT_32BIT(blob+blobptr, len);
	    memcpy(blob+blobptr+4, p, len);
	    blobptr += 4+len;
	    if (i == 4)
		privptr = blobptr;
	}

	/* Skip past the number. */
	p += len;
    }

    /*
     * Now put together the actual key. Simplest way to do this is
     * to assemble our own key blobs and feed them to the createkey
     * functions; this is a bit faffy but it does mean we get all
     * the sanity checks for free.
     */
    assert(privptr > 0);	       /* should have bombed by now if not */
    retkey = snew(struct ssh2_userkey);
    retkey->alg = (key->type == OSSH_RSA ? &ssh_rsa : &ssh_dss);
    retkey->data = retkey->alg->createkey(blob, privptr,
					  blob+privptr, blobptr-privptr);
    if (!retkey->data) {
	sfree(retkey);
	errmsg = "unable to create key data structure";
	goto error;
    }

    retkey->comment = dupstr("imported-openssh-key");
    errmsg = NULL;                     /* no error */
    retval = retkey;

    error:
    if (blob) {
        smemclr(blob, blobsize);
        sfree(blob);
    }
    smemclr(key->keyblob, key->keyblob_size);
    sfree(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    if (errmsg_p) *errmsg_p = errmsg;
    return retval;
}

int openssh_write(const Filename *filename, struct ssh2_userkey *key,
		  char *passphrase)
{
    unsigned char *pubblob, *privblob, *spareblob;
    int publen, privlen, sparelen = 0;
    unsigned char *outblob;
    int outlen;
    struct mpint_pos numbers[9];
    int nnumbers, pos, len, seqlen, i;
    char *header, *footer;
    char zero[1];
    unsigned char iv[8];
    int ret = 0;
    FILE *fp;

    /*
     * Fetch the key blobs.
     */
    pubblob = key->alg->public_blob(key->data, &publen);
    privblob = key->alg->private_blob(key->data, &privlen);
    spareblob = outblob = NULL;

    /*
     * Find the sequence of integers to be encoded into the OpenSSH
     * key blob, and also decide on the header line.
     */
    if (key->alg == &ssh_rsa) {
        int pos;
        struct mpint_pos n, e, d, p, q, iqmp, dmp1, dmq1;
        Bignum bd, bp, bq, bdmp1, bdmq1;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        pos = 4 + GET_32BIT(pubblob);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &e);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &n);
        pos = 0;
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &d);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &p);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &q);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &iqmp);

        assert(e.start && iqmp.start); /* can't go wrong */

        /* We also need d mod (p-1) and d mod (q-1). */
        bd = bignum_from_bytes(d.start, d.bytes);
        bp = bignum_from_bytes(p.start, p.bytes);
        bq = bignum_from_bytes(q.start, q.bytes);
        decbn(bp);
        decbn(bq);
        bdmp1 = bigmod(bd, bp);
        bdmq1 = bigmod(bd, bq);
        freebn(bd);
        freebn(bp);
        freebn(bq);

        dmp1.bytes = (bignum_bitcount(bdmp1)+8)/8;
        dmq1.bytes = (bignum_bitcount(bdmq1)+8)/8;
        sparelen = dmp1.bytes + dmq1.bytes;
        spareblob = snewn(sparelen, unsigned char);
        dmp1.start = spareblob;
        dmq1.start = spareblob + dmp1.bytes;
        for (i = 0; i < dmp1.bytes; i++)
            spareblob[i] = bignum_byte(bdmp1, dmp1.bytes-1 - i);
        for (i = 0; i < dmq1.bytes; i++)
            spareblob[i+dmp1.bytes] = bignum_byte(bdmq1, dmq1.bytes-1 - i);
        freebn(bdmp1);
        freebn(bdmq1);

        numbers[0].start = zero; numbers[0].bytes = 1; zero[0] = '\0';
        numbers[1] = n;
        numbers[2] = e;
        numbers[3] = d;
        numbers[4] = p;
        numbers[5] = q;
        numbers[6] = dmp1;
        numbers[7] = dmq1;
        numbers[8] = iqmp;

        nnumbers = 9;
        header = "-----BEGIN RSA PRIVATE KEY-----\n";
        footer = "-----END RSA PRIVATE KEY-----\n";
    } else if (key->alg == &ssh_dss) {
        int pos;
        struct mpint_pos p, q, g, y, x;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        pos = 4 + GET_32BIT(pubblob);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &p);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &q);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &g);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &y);
        pos = 0;
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &x);

        assert(y.start && x.start); /* can't go wrong */

        numbers[0].start = zero; numbers[0].bytes = 1; zero[0] = '\0'; 
        numbers[1] = p;
        numbers[2] = q;
        numbers[3] = g;
        numbers[4] = y;
        numbers[5] = x;

        nnumbers = 6;
        header = "-----BEGIN DSA PRIVATE KEY-----\n";
        footer = "-----END DSA PRIVATE KEY-----\n";
    } else {
        assert(0);                     /* zoinks! */
	exit(1); /* XXX: GCC doesn't understand assert() on some systems. */
    }

    /*
     * Now count up the total size of the ASN.1 encoded integers,
     * so as to determine the length of the containing SEQUENCE.
     */
    len = 0;
    for (i = 0; i < nnumbers; i++) {
	len += ber_write_id_len(NULL, 2, numbers[i].bytes, 0);
	len += numbers[i].bytes;
    }
    seqlen = len;
    /* Now add on the SEQUENCE header. */
    len += ber_write_id_len(NULL, 16, seqlen, ASN1_CONSTRUCTED);
    /* Round up to the cipher block size, ensuring we have at least one
     * byte of padding (see below). */
    outlen = len;
    if (passphrase)
	outlen = (outlen+8) &~ 7;

    /*
     * Now we know how big outblob needs to be. Allocate it.
     */
    outblob = snewn(outlen, unsigned char);

    /*
     * And write the data into it.
     */
    pos = 0;
    pos += ber_write_id_len(outblob+pos, 16, seqlen, ASN1_CONSTRUCTED);
    for (i = 0; i < nnumbers; i++) {
	pos += ber_write_id_len(outblob+pos, 2, numbers[i].bytes, 0);
	memcpy(outblob+pos, numbers[i].start, numbers[i].bytes);
	pos += numbers[i].bytes;
    }

    /*
     * Padding on OpenSSH keys is deterministic. The number of
     * padding bytes is always more than zero, and always at most
     * the cipher block length. The value of each padding byte is
     * equal to the number of padding bytes. So a plaintext that's
     * an exact multiple of the block size will be padded with 08
     * 08 08 08 08 08 08 08 (assuming a 64-bit block cipher); a
     * plaintext one byte less than a multiple of the block size
     * will be padded with just 01.
     * 
     * This enables the OpenSSL key decryption function to strip
     * off the padding algorithmically and return the unpadded
     * plaintext to the next layer: it looks at the final byte, and
     * then expects to find that many bytes at the end of the data
     * with the same value. Those are all removed and the rest is
     * returned.
     */
    assert(pos == len);
    while (pos < outlen) {
        outblob[pos++] = outlen - len;
    }

    /*
     * Encrypt the key.
     *
     * For the moment, we still encrypt our OpenSSH keys using
     * old-style 3DES.
     */
    if (passphrase) {
	/*
	 * Invent an iv. Then derive encryption key from passphrase
	 * and iv/salt:
	 * 
	 *  - let block A equal MD5(passphrase || iv)
	 *  - let block B equal MD5(A || passphrase || iv)
	 *  - block C would be MD5(B || passphrase || iv) and so on
	 *  - encryption key is the first N bytes of A || B
	 */
	struct MD5Context md5c;
	unsigned char keybuf[32];

	for (i = 0; i < 8; i++) iv[i] = random_byte();

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, iv, 8);
	MD5Final(keybuf, &md5c);

	MD5Init(&md5c);
	MD5Update(&md5c, keybuf, 16);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, iv, 8);
	MD5Final(keybuf+16, &md5c);

	/*
	 * Now encrypt the key blob.
	 */
	des3_encrypt_pubkey_ossh(keybuf, iv, outblob, outlen);

        smemclr(&md5c, sizeof(md5c));
        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * And save it. We'll use Unix line endings just in case it's
     * subsequently transferred in binary mode.
     */
    fp = f_open(filename, "wb", TRUE);      /* ensure Unix line endings */
    if (!fp)
	goto error;
    fputs(header, fp);
    if (passphrase) {
	fprintf(fp, "Proc-Type: 4,ENCRYPTED\nDEK-Info: DES-EDE3-CBC,");
	for (i = 0; i < 8; i++)
	    fprintf(fp, "%02X", iv[i]);
	fprintf(fp, "\n\n");
    }
    base64_encode(fp, outblob, outlen, 64);
    fputs(footer, fp);
    fclose(fp);
    ret = 1;

    error:
    if (outblob) {
        smemclr(outblob, outlen);
        sfree(outblob);
    }
    if (spareblob) {
        smemclr(spareblob, sparelen);
        sfree(spareblob);
    }
    if (privblob) {
        smemclr(privblob, privlen);
        sfree(privblob);
    }
    if (pubblob) {
        smemclr(pubblob, publen);
        sfree(pubblob);
    }
    return ret;
}

/* ----------------------------------------------------------------------
 * Code to read ssh.com private keys.
 */

/*
 * The format of the base64 blob is largely SSH-2-packet-formatted,
 * except that mpints are a bit different: they're more like the
 * old SSH-1 mpint. You have a 32-bit bit count N, followed by
 * (N+7)/8 bytes of data.
 * 
 * So. The blob contains:
 * 
 *  - uint32 0x3f6ff9eb       (magic number)
 *  - uint32 size             (total blob size)
 *  - string key-type         (see below)
 *  - string cipher-type      (tells you if key is encrypted)
 *  - string encrypted-blob
 * 
 * (The first size field includes the size field itself and the
 * magic number before it. All other size fields are ordinary SSH-2
 * strings, so the size field indicates how much data is to
 * _follow_.)
 * 
 * The encrypted blob, once decrypted, contains a single string
 * which in turn contains the payload. (This allows padding to be
 * added after that string while still making it clear where the
 * real payload ends. Also it probably makes for a reasonable
 * decryption check.)
 * 
 * The payload blob, for an RSA key, contains:
 *  - mpint e
 *  - mpint d
 *  - mpint n  (yes, the public and private stuff is intermixed)
 *  - mpint u  (presumably inverse of p mod q)
 *  - mpint p  (p is the smaller prime)
 *  - mpint q  (q is the larger)
 * 
 * For a DSA key, the payload blob contains:
 *  - uint32 0
 *  - mpint p
 *  - mpint g
 *  - mpint q
 *  - mpint y
 *  - mpint x
 * 
 * Alternatively, if the parameters are `predefined', that
 * (0,p,g,q) sequence can be replaced by a uint32 1 and a string
 * containing some predefined parameter specification. *shudder*,
 * but I doubt we'll encounter this in real life.
 * 
 * The key type strings are ghastly. The RSA key I looked at had a
 * type string of
 * 
 *   `if-modn{sign{rsa-pkcs1-sha1},encrypt{rsa-pkcs1v2-oaep}}'
 * 
 * and the DSA key wasn't much better:
 * 
 *   `dl-modp{sign{dsa-nist-sha1},dh{plain}}'
 * 
 * It isn't clear that these will always be the same. I think it
 * might be wise just to look at the `if-modn{sign{rsa' and
 * `dl-modp{sign{dsa' prefixes.
 * 
 * Finally, the encryption. The cipher-type string appears to be
 * either `none' or `3des-cbc'. Looks as if this is SSH-2-style
 * 3des-cbc (i.e. outer cbc rather than inner). The key is created
 * from the passphrase by means of yet another hashing faff:
 * 
 *  - first 16 bytes are MD5(passphrase)
 *  - next 16 bytes are MD5(passphrase || first 16 bytes)
 *  - if there were more, they'd be MD5(passphrase || first 32),
 *    and so on.
 */

#define SSHCOM_MAGIC_NUMBER 0x3f6ff9eb

struct sshcom_key {
    char comment[256];                 /* allowing any length is overkill */
    unsigned char *keyblob;
    int keyblob_len, keyblob_size;
};

static struct sshcom_key *load_sshcom_key(const Filename *filename,
					  const char **errmsg_p)
{
    struct sshcom_key *ret;
    FILE *fp;
    char *line = NULL;
    int hdrstart, len;
    char *errmsg, *p;
    int headers_done;
    char base64_bit[4];
    int base64_chars = 0;

    ret = snew(struct sshcom_key);
    ret->comment[0] = '\0';
    ret->keyblob = NULL;
    ret->keyblob_len = ret->keyblob_size = 0;

    fp = f_open(filename, "r", FALSE);
    if (!fp) {
	errmsg = "unable to open key file";
	goto error;
    }
    if (!(line = fgetline(fp))) {
	errmsg = "unexpected end of file";
	goto error;
    }
    strip_crlf(line);
    if (0 != strcmp(line, "---- BEGIN SSH2 ENCRYPTED PRIVATE KEY ----")) {
	errmsg = "file does not begin with ssh.com key header";
	goto error;
    }
    smemclr(line, strlen(line));
    sfree(line);
    line = NULL;

    headers_done = 0;
    while (1) {
	if (!(line = fgetline(fp))) {
	    errmsg = "unexpected end of file";
	    goto error;
	}
	strip_crlf(line);
        if (!strcmp(line, "---- END SSH2 ENCRYPTED PRIVATE KEY ----")) {
            sfree(line);
            line = NULL;
            break;                     /* done */
        }
	if ((p = strchr(line, ':')) != NULL) {
	    if (headers_done) {
		errmsg = "header found in body of key data";
		goto error;
	    }
	    *p++ = '\0';
	    while (*p && isspace((unsigned char)*p)) p++;
	    hdrstart = p - line;

            /*
             * Header lines can end in a trailing backslash for
             * continuation.
             */
	    len = hdrstart + strlen(line+hdrstart);
	    assert(!line[len]);
            while (line[len-1] == '\\') {
		char *line2;
		int line2len;

		line2 = fgetline(fp);
		if (!line2) {
                    errmsg = "unexpected end of file";
                    goto error;
                }
		strip_crlf(line2);

		line2len = strlen(line2);
		line = sresize(line, len + line2len + 1, char);
		strcpy(line + len - 1, line2);
		len += line2len - 1;
		assert(!line[len]);

		smemclr(line2, strlen(line2));
		sfree(line2);
		line2 = NULL;
            }
	    p = line + hdrstart;
	    strip_crlf(p);
            if (!strcmp(line, "Comment")) {
                /* Strip quotes in comment if present. */
                if (p[0] == '"' && p[strlen(p)-1] == '"') {
                    p++;
                    p[strlen(p)-1] = '\0';
                }
                strncpy(ret->comment, p, sizeof(ret->comment));
                ret->comment[sizeof(ret->comment)-1] = '\0';
            }
	} else {
	    headers_done = 1;

	    p = line;
	    while (isbase64(*p)) {
                base64_bit[base64_chars++] = *p;
                if (base64_chars == 4) {
                    unsigned char out[3];

                    base64_chars = 0;

                    len = base64_decode_atom(base64_bit, out);

                    if (len <= 0) {
                        errmsg = "invalid base64 encoding";
                        goto error;
                    }

                    if (ret->keyblob_len + len > ret->keyblob_size) {
                        ret->keyblob_size = ret->keyblob_len + len + 256;
                        ret->keyblob = sresize(ret->keyblob, ret->keyblob_size,
					       unsigned char);
                    }

                    memcpy(ret->keyblob + ret->keyblob_len, out, len);
                    ret->keyblob_len += len;
                }

		p++;
	    }
	}
	smemclr(line, strlen(line));
	sfree(line);
	line = NULL;
    }

    if (ret->keyblob_len == 0 || !ret->keyblob) {
	errmsg = "key body not present";
	goto error;
    }

    fclose(fp);
    if (errmsg_p) *errmsg_p = NULL;
    return ret;

    error:
    if (fp)
        fclose(fp);

    if (line) {
	smemclr(line, strlen(line));
	sfree(line);
	line = NULL;
    }
    if (ret) {
	if (ret->keyblob) {
            smemclr(ret->keyblob, ret->keyblob_size);
            sfree(ret->keyblob);
        }
        smemclr(ret, sizeof(*ret));
	sfree(ret);
    }
    if (errmsg_p) *errmsg_p = errmsg;
    return NULL;
}

int sshcom_encrypted(const Filename *filename, char **comment)
{
    struct sshcom_key *key = load_sshcom_key(filename, NULL);
    int pos, len, answer;

    answer = 0;

    *comment = NULL;
    if (!key)
        goto done;

    /*
     * Check magic number.
     */
    if (GET_32BIT(key->keyblob) != 0x3f6ff9eb) {
        goto done;                     /* key is invalid */
    }

    /*
     * Find the cipher-type string.
     */
    pos = 8;
    if (key->keyblob_len < pos+4)
        goto done;                     /* key is far too short */
    len = toint(GET_32BIT(key->keyblob + pos));
    if (len < 0 || len > key->keyblob_len - pos - 4)
        goto done;                     /* key is far too short */
    pos += 4 + len;                    /* skip key type */
    len = toint(GET_32BIT(key->keyblob + pos)); /* find cipher-type length */
    if (len < 0 || len > key->keyblob_len - pos - 4)
        goto done;                     /* cipher type string is incomplete */
    if (len != 4 || 0 != memcmp(key->keyblob + pos + 4, "none", 4))
        answer = 1;

    done:
    if (key) {
        *comment = dupstr(key->comment);
        smemclr(key->keyblob, key->keyblob_size);
        sfree(key->keyblob);
        smemclr(key, sizeof(*key));
        sfree(key);
    } else {
        *comment = dupstr("");
    }
    return answer;
}

static int sshcom_read_mpint(void *data, int len, struct mpint_pos *ret)
{
    unsigned bits, bytes;
    unsigned char *d = (unsigned char *) data;

    if (len < 4)
        goto error;
    bits = GET_32BIT(d);

    bytes = (bits + 7) / 8;
    if (len < 4+bytes)
        goto error;

    ret->start = d + 4;
    ret->bytes = bytes;
    return bytes+4;

    error:
    ret->start = NULL;
    ret->bytes = -1;
    return len;                        /* ensure further calls fail as well */
}

static int sshcom_put_mpint(void *target, void *data, int len)
{
    unsigned char *d = (unsigned char *)target;
    unsigned char *i = (unsigned char *)data;
    int bits = len * 8 - 1;

    while (bits > 0) {
	if (*i & (1 << (bits & 7)))
	    break;
	if (!(bits-- & 7))
	    i++, len--;
    }

    PUT_32BIT(d, bits+1);
    memcpy(d+4, i, len);
    return len+4;
}

struct ssh2_userkey *sshcom_read(const Filename *filename, char *passphrase,
				 const char **errmsg_p)
{
    struct sshcom_key *key = load_sshcom_key(filename, errmsg_p);
    char *errmsg;
    int pos, len;
    const char prefix_rsa[] = "if-modn{sign{rsa";
    const char prefix_dsa[] = "dl-modp{sign{dsa";
    enum { RSA, DSA } type;
    int encrypted;
    char *ciphertext;
    int cipherlen;
    struct ssh2_userkey *ret = NULL, *retkey;
    const struct ssh_signkey *alg;
    unsigned char *blob = NULL;
    int blobsize = 0, publen, privlen;

    if (!key)
        return NULL;

    /*
     * Check magic number.
     */
    if (GET_32BIT(key->keyblob) != SSHCOM_MAGIC_NUMBER) {
        errmsg = "key does not begin with magic number";
        goto error;
    }

    /*
     * Determine the key type.
     */
    pos = 8;
    if (key->keyblob_len < pos+4 ||
        (len = toint(GET_32BIT(key->keyblob + pos))) < 0 ||
        len > key->keyblob_len - pos - 4) {
        errmsg = "key blob does not contain a key type string";
        goto error;
    }
    if (len > sizeof(prefix_rsa) - 1 &&
        !memcmp(key->keyblob+pos+4, prefix_rsa, sizeof(prefix_rsa) - 1)) {
        type = RSA;
    } else if (len > sizeof(prefix_dsa) - 1 &&
        !memcmp(key->keyblob+pos+4, prefix_dsa, sizeof(prefix_dsa) - 1)) {
        type = DSA;
    } else {
        errmsg = "key is of unknown type";
        goto error;
    }
    pos += 4+len;

    /*
     * Determine the cipher type.
     */
    if (key->keyblob_len < pos+4 ||
        (len = toint(GET_32BIT(key->keyblob + pos))) < 0 ||
        len > key->keyblob_len - pos - 4) {
        errmsg = "key blob does not contain a cipher type string";
        goto error;
    }
    if (len == 4 && !memcmp(key->keyblob+pos+4, "none", 4))
        encrypted = 0;
    else if (len == 8 && !memcmp(key->keyblob+pos+4, "3des-cbc", 8))
        encrypted = 1;
    else {
        errmsg = "key encryption is of unknown type";
        goto error;
    }
    pos += 4+len;

    /*
     * Get hold of the encrypted part of the key.
     */
    if (key->keyblob_len < pos+4 ||
        (len = toint(GET_32BIT(key->keyblob + pos))) < 0 ||
        len > key->keyblob_len - pos - 4) {
        errmsg = "key blob does not contain actual key data";
        goto error;
    }
    ciphertext = (char *)key->keyblob + pos + 4;
    cipherlen = len;
    if (cipherlen == 0) {
        errmsg = "length of key data is zero";
        goto error;
    }

    /*
     * Decrypt it if necessary.
     */
    if (encrypted) {
	/*
	 * Derive encryption key from passphrase and iv/salt:
	 * 
	 *  - let block A equal MD5(passphrase)
	 *  - let block B equal MD5(passphrase || A)
	 *  - block C would be MD5(passphrase || A || B) and so on
	 *  - encryption key is the first N bytes of A || B
	 */
	struct MD5Context md5c;
	unsigned char keybuf[32], iv[8];

        if (cipherlen % 8 != 0) {
            errmsg = "encrypted part of key is not a multiple of cipher block"
                " size";
            goto error;
        }

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Final(keybuf, &md5c);

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, keybuf, 16);
	MD5Final(keybuf+16, &md5c);

	/*
	 * Now decrypt the key blob.
	 */
        memset(iv, 0, sizeof(iv));
	des3_decrypt_pubkey_ossh(keybuf, iv, (unsigned char *)ciphertext,
				 cipherlen);

        smemclr(&md5c, sizeof(md5c));
        smemclr(keybuf, sizeof(keybuf));

        /*
         * Hereafter we return WRONG_PASSPHRASE for any parsing
         * error. (But only if we've just tried to decrypt it!
         * Returning WRONG_PASSPHRASE for an unencrypted key is
         * automatic doom.)
         */
        if (encrypted)
            ret = SSH2_WRONG_PASSPHRASE;
    }

    /*
     * Strip away the containing string to get to the real meat.
     */
    len = toint(GET_32BIT(ciphertext));
    if (len < 0 || len > cipherlen-4) {
        errmsg = "containing string was ill-formed";
        goto error;
    }
    ciphertext += 4;
    cipherlen = len;

    /*
     * Now we break down into RSA versus DSA. In either case we'll
     * construct public and private blobs in our own format, and
     * end up feeding them to alg->createkey().
     */
    blobsize = cipherlen + 256;
    blob = snewn(blobsize, unsigned char);
    privlen = 0;
    if (type == RSA) {
        struct mpint_pos n, e, d, u, p, q;
        int pos = 0;
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &e);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &d);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &n);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &u);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &p);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &q);
        if (!q.start) {
            errmsg = "key data did not contain six integers";
            goto error;
        }

        alg = &ssh_rsa;
        pos = 0;
        pos += put_string(blob+pos, "ssh-rsa", 7);
        pos += put_mp(blob+pos, e.start, e.bytes);
        pos += put_mp(blob+pos, n.start, n.bytes);
        publen = pos;
        pos += put_string(blob+pos, d.start, d.bytes);
        pos += put_mp(blob+pos, q.start, q.bytes);
        pos += put_mp(blob+pos, p.start, p.bytes);
        pos += put_mp(blob+pos, u.start, u.bytes);
        privlen = pos - publen;
    } else {
        struct mpint_pos p, q, g, x, y;
        int pos = 4;

        assert(type == DSA); /* the only other option from the if above */

        if (GET_32BIT(ciphertext) != 0) {
            errmsg = "predefined DSA parameters not supported";
            goto error;
        }
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &p);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &g);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &q);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &y);
        pos += sshcom_read_mpint(ciphertext+pos, cipherlen-pos, &x);
        if (!x.start) {
            errmsg = "key data did not contain five integers";
            goto error;
        }

        alg = &ssh_dss;
        pos = 0;
        pos += put_string(blob+pos, "ssh-dss", 7);
        pos += put_mp(blob+pos, p.start, p.bytes);
        pos += put_mp(blob+pos, q.start, q.bytes);
        pos += put_mp(blob+pos, g.start, g.bytes);
        pos += put_mp(blob+pos, y.start, y.bytes);
        publen = pos;
        pos += put_mp(blob+pos, x.start, x.bytes);
        privlen = pos - publen;
    }

    assert(privlen > 0);	       /* should have bombed by now if not */

    retkey = snew(struct ssh2_userkey);
    retkey->alg = alg;
    retkey->data = alg->createkey(blob, publen, blob+publen, privlen);
    if (!retkey->data) {
	sfree(retkey);
	errmsg = "unable to create key data structure";
	goto error;
    }
    retkey->comment = dupstr(key->comment);

    errmsg = NULL; /* no error */
    ret = retkey;

    error:
    if (blob) {
        smemclr(blob, blobsize);
        sfree(blob);
    }
    smemclr(key->keyblob, key->keyblob_size);
    sfree(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    if (errmsg_p) *errmsg_p = errmsg;
    return ret;
}

int sshcom_write(const Filename *filename, struct ssh2_userkey *key,
		 char *passphrase)
{
    unsigned char *pubblob, *privblob;
    int publen, privlen;
    unsigned char *outblob;
    int outlen;
    struct mpint_pos numbers[6];
    int nnumbers, initial_zero, pos, lenpos, i;
    char *type;
    char *ciphertext;
    int cipherlen;
    int ret = 0;
    FILE *fp;

    /*
     * Fetch the key blobs.
     */
    pubblob = key->alg->public_blob(key->data, &publen);
    privblob = key->alg->private_blob(key->data, &privlen);
    outblob = NULL;

    /*
     * Find the sequence of integers to be encoded into the OpenSSH
     * key blob, and also decide on the header line.
     */
    if (key->alg == &ssh_rsa) {
        int pos;
        struct mpint_pos n, e, d, p, q, iqmp;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        pos = 4 + GET_32BIT(pubblob);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &e);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &n);
        pos = 0;
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &d);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &p);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &q);
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &iqmp);

        assert(e.start && iqmp.start); /* can't go wrong */

        numbers[0] = e;
        numbers[1] = d;
        numbers[2] = n;
        numbers[3] = iqmp;
        numbers[4] = q;
        numbers[5] = p;

        nnumbers = 6;
	initial_zero = 0;
	type = "if-modn{sign{rsa-pkcs1-sha1},encrypt{rsa-pkcs1v2-oaep}}";
    } else if (key->alg == &ssh_dss) {
        int pos;
        struct mpint_pos p, q, g, y, x;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        pos = 4 + GET_32BIT(pubblob);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &p);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &q);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &g);
        pos += ssh2_read_mpint(pubblob+pos, publen-pos, &y);
        pos = 0;
        pos += ssh2_read_mpint(privblob+pos, privlen-pos, &x);

        assert(y.start && x.start); /* can't go wrong */

        numbers[0] = p;
        numbers[1] = g;
        numbers[2] = q;
        numbers[3] = y;
        numbers[4] = x;

        nnumbers = 5;
	initial_zero = 1;
	type = "dl-modp{sign{dsa-nist-sha1},dh{plain}}";
    } else {
        assert(0);                     /* zoinks! */
	exit(1); /* XXX: GCC doesn't understand assert() on some systems. */
    }

    /*
     * Total size of key blob will be somewhere under 512 plus
     * combined length of integers. We'll calculate the more
     * precise size as we construct the blob.
     */
    outlen = 512;
    for (i = 0; i < nnumbers; i++)
	outlen += 4 + numbers[i].bytes;
    outblob = snewn(outlen, unsigned char);

    /*
     * Create the unencrypted key blob.
     */
    pos = 0;
    PUT_32BIT(outblob+pos, SSHCOM_MAGIC_NUMBER); pos += 4;
    pos += 4;			       /* length field, fill in later */
    pos += put_string(outblob+pos, type, strlen(type));
    {
	char *ciphertype = passphrase ? "3des-cbc" : "none";
	pos += put_string(outblob+pos, ciphertype, strlen(ciphertype));
    }
    lenpos = pos;		       /* remember this position */
    pos += 4;			       /* encrypted-blob size */
    pos += 4;			       /* encrypted-payload size */
    if (initial_zero) {
	PUT_32BIT(outblob+pos, 0);
	pos += 4;
    }
    for (i = 0; i < nnumbers; i++)
	pos += sshcom_put_mpint(outblob+pos,
				numbers[i].start, numbers[i].bytes);
    /* Now wrap up the encrypted payload. */
    PUT_32BIT(outblob+lenpos+4, pos - (lenpos+8));
    /* Pad encrypted blob to a multiple of cipher block size. */
    if (passphrase) {
	int padding = -(pos - (lenpos+4)) & 7;
	while (padding--)
	    outblob[pos++] = random_byte();
    }
    ciphertext = (char *)outblob+lenpos+4;
    cipherlen = pos - (lenpos+4);
    assert(!passphrase || cipherlen % 8 == 0);
    /* Wrap up the encrypted blob string. */
    PUT_32BIT(outblob+lenpos, cipherlen);
    /* And finally fill in the total length field. */
    PUT_32BIT(outblob+4, pos);

    assert(pos < outlen);

    /*
     * Encrypt the key.
     */
    if (passphrase) {
	/*
	 * Derive encryption key from passphrase and iv/salt:
	 * 
	 *  - let block A equal MD5(passphrase)
	 *  - let block B equal MD5(passphrase || A)
	 *  - block C would be MD5(passphrase || A || B) and so on
	 *  - encryption key is the first N bytes of A || B
	 */
	struct MD5Context md5c;
	unsigned char keybuf[32], iv[8];

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Final(keybuf, &md5c);

	MD5Init(&md5c);
	MD5Update(&md5c, (unsigned char *)passphrase, strlen(passphrase));
	MD5Update(&md5c, keybuf, 16);
	MD5Final(keybuf+16, &md5c);

	/*
	 * Now decrypt the key blob.
	 */
        memset(iv, 0, sizeof(iv));
	des3_encrypt_pubkey_ossh(keybuf, iv, (unsigned char *)ciphertext,
				 cipherlen);

        smemclr(&md5c, sizeof(md5c));
        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * And save it. We'll use Unix line endings just in case it's
     * subsequently transferred in binary mode.
     */
    fp = f_open(filename, "wb", TRUE);      /* ensure Unix line endings */
    if (!fp)
	goto error;
    fputs("---- BEGIN SSH2 ENCRYPTED PRIVATE KEY ----\n", fp);
    fprintf(fp, "Comment: \"");
    /*
     * Comment header is broken with backslash-newline if it goes
     * over 70 chars. Although it's surrounded by quotes, it
     * _doesn't_ escape backslashes or quotes within the string.
     * Don't ask me, I didn't design it.
     */
    {
	int slen = 60;		       /* starts at 60 due to "Comment: " */
	char *c = key->comment;
	while ((int)strlen(c) > slen) {
	    fprintf(fp, "%.*s\\\n", slen, c);
	    c += slen;
	    slen = 70;		       /* allow 70 chars on subsequent lines */
	}
	fprintf(fp, "%s\"\n", c);
    }
    base64_encode(fp, outblob, pos, 70);
    fputs("---- END SSH2 ENCRYPTED PRIVATE KEY ----\n", fp);
    fclose(fp);
    ret = 1;

    error:
    if (outblob) {
        smemclr(outblob, outlen);
        sfree(outblob);
    }
    if (privblob) {
        smemclr(privblob, privlen);
        sfree(privblob);
    }
    if (pubblob) {
        smemclr(pubblob, publen);
        sfree(pubblob);
    }
    return ret;
}
