/*
 * cmdgen.c - command-line form of PuTTYgen
 */

#define PUTTY_DO_GLOBALS

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <assert.h>
#include <time.h>

#include "putty.h"
#include "ssh.h"

#ifdef TEST_CMDGEN
/*
 * This section overrides some definitions below for test purposes.
 * When compiled with -DTEST_CMDGEN:
 * 
 *  - Calls to get_random_data() are replaced with the diagnostic
 *    function below (I #define the name so that I can still link
 *    with the original set of modules without symbol clash), in
 *    order to avoid depleting the test system's /dev/random
 *    unnecessarily.
 * 
 *  - Calls to console_get_line() are replaced with the diagnostic
 *    function below, so that I can run tests in an automated
 *    manner and provide their interactive passphrase inputs.
 * 
 *  - main() is renamed to cmdgen_main(); at the bottom of the file
 *    I define another main() which calls the former repeatedly to
 *    run tests.
 */
#define get_random_data get_random_data_diagnostic
char *get_random_data(int len)
{
    char *buf = snewn(len, char);
    memset(buf, 'x', len);
    return buf;
}
#define console_get_line console_get_line_diagnostic
int nprompts, promptsgot;
const char *prompts[3];
int console_get_line(const char *prompt, char *str, int maxlen, int is_pw)
{
    if (promptsgot < nprompts) {
	assert(strlen(prompts[promptsgot]) < maxlen);
	strcpy(str, prompts[promptsgot++]);
	return TRUE;
    } else {
	promptsgot++;		       /* track number of requests anyway */
	return FALSE;
    }
}
#define main cmdgen_main
#endif

struct progress {
    int phase, current;
};

static void progress_update(void *param, int action, int phase, int iprogress)
{
    struct progress *p = (struct progress *)param;
    if (action != PROGFN_PROGRESS)
	return;
    if (phase > p->phase) {
	if (p->phase >= 0)
	    fputc('\n', stderr);
	p->phase = phase;
	if (iprogress >= 0)
	    p->current = iprogress - 1;
	else
	    p->current = iprogress;
    }
    while (p->current < iprogress) {
	fputc('+', stdout);
	p->current++;
    }
    fflush(stdout);
}

static void no_progress(void *param, int action, int phase, int iprogress)
{
}

void modalfatalbox(char *p, ...)
{
    va_list ap;
    fprintf(stderr, "FATAL ERROR: ");
    va_start(ap, p);
    vfprintf(stderr, p, ap);
    va_end(ap);
    fputc('\n', stderr);
    cleanup_exit(1);
}

/*
 * Stubs to let everything else link sensibly.
 */
void log_eventlog(void *handle, const char *event)
{
}
char *x_get_default(const char *key)
{
    return NULL;
}
void sk_cleanup(void)
{
}

void showversion(void)
{
    char *verstr = dupstr(ver);
    verstr[0] = tolower(verstr[0]);
    printf("PuTTYgen %s\n", verstr);
    sfree(verstr);
}

void usage(void)
{
    fprintf(stderr,
	    "Usage: puttygen ( keyfile | -t type [ -b bits ] )\n"
	    "                [ -C comment ] [ -P ]\n"
	    "                [ -o output-keyfile ] [ -O type | -l | -L"
	    " | -p ]\n");
}

void help(void)
{
    /*
     * Help message is an extended version of the usage message. So
     * start with that, plus a version heading.
     */
    showversion();
    usage();
    fprintf(stderr,
	    "  -t    specify key type when generating (rsa, dsa, rsa1)\n"
	    "  -b    specify number of bits when generating key\n"
	    "  -C    change or specify key comment\n"
	    "  -P    change key passphrase\n"
	    "  -O    specify output type:\n"
	    "           private             output PuTTY private key format\n"
	    "           private-openssh     export OpenSSH private key\n"
	    "           private-sshcom      export ssh.com private key\n"
	    "           public              standard / ssh.com public key\n"
	    "           public-openssh      OpenSSH public key\n"
	    "           fingerprint         output the key fingerprint\n"
	    "  -o    specify output file\n"
	    "  -l    equivalent to `-O fingerprint'\n"
	    "  -L    equivalent to `-O public-openssh'\n"
	    "  -p    equivalent to `-O public'\n"
	    );
}

static int save_ssh2_pubkey(char *filename, char *comment,
			    void *v_pub_blob, int pub_len)
{
    unsigned char *pub_blob = (unsigned char *)v_pub_blob;
    char *p;
    int i, column;
    FILE *fp;

    if (filename) {
	fp = fopen(filename, "wb");
	if (!fp)
	    return 0;
    } else
	fp = stdout;

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
    if (filename)
	fclose(fp);
    return 1;
}

static int move(char *from, char *to)
{
    int ret;

    ret = rename(from, to);
    if (ret) {
	/*
	 * This OS may require us to remove the original file first.
	 */
	remove(to);
	ret = rename(from, to);
    }
    if (ret) {
	perror("puttygen: cannot move new file on to old one");
	return FALSE;
    }
    return TRUE;
}

static char *blobfp(char *alg, int bits, char *blob, int bloblen)
{
    char buffer[128];
    unsigned char digest[16];
    struct MD5Context md5c;
    int i;

    MD5Init(&md5c);
    MD5Update(&md5c, blob, bloblen);
    MD5Final(digest, &md5c);

    sprintf(buffer, "%s ", alg);
    if (bits > 0)
	sprintf(buffer + strlen(buffer), "%d ", bits);
    for (i = 0; i < 16; i++)
	sprintf(buffer + strlen(buffer), "%s%02x", i ? ":" : "",
		digest[i]);

    return dupstr(buffer);
}

int main(int argc, char **argv)
{
    char *infile = NULL;
    Filename infilename;
    enum { NOKEYGEN, RSA1, RSA2, DSA } keytype = NOKEYGEN;    
    char *outfile = NULL, *outfiletmp = NULL;
    Filename outfilename;
    enum { PRIVATE, PUBLIC, PUBLICO, FP, OPENSSH, SSHCOM } outtype = PRIVATE;
    int bits = 1024;
    char *comment = NULL, *origcomment = NULL;
    int change_passphrase = FALSE;
    int errs = FALSE, nogo = FALSE;
    int intype = SSH_KEYTYPE_UNOPENABLE;
    int sshver = 0;
    struct ssh2_userkey *ssh2key = NULL;
    struct RSAKey *ssh1key = NULL;
    char *ssh2blob = NULL, *ssh2alg = NULL;
    const struct ssh_signkey *ssh2algf = NULL;
    int ssh2bloblen;
    char *passphrase = NULL;
    int load_encrypted;
    progfn_t progressfn = is_interactive() ? progress_update : no_progress;

    /* ------------------------------------------------------------------
     * Parse the command line to figure out what we've been asked to do.
     */

    /*
     * If run with no arguments at all, print the usage message and
     * return success.
     */
    if (argc <= 1) {
	usage();
	return 0;
    }

    /*
     * Parse command line arguments.
     */
    while (--argc) {
	char *p = *++argv;
	if (*p == '-') {
	    /*
	     * An option.
	     */
	    while (p && *++p) {
		char c = *p;
		switch (c) {
		  case '-':
		    /*
		     * Long option.
		     */
		    {
			char *opt, *val;
			opt = p++;     /* opt will have _one_ leading - */
			while (*p && *p != '=')
			    p++;	       /* find end of option */
			if (*p == '=') {
			    *p++ = '\0';
			    val = p;
			} else
			    val = NULL;
			if (!strcmp(opt, "-help")) {
			    help();
			    nogo = TRUE;
			} else if (!strcmp(opt, "-version")) {
			    showversion();
			    nogo = TRUE;
			}
			/*
			 * A sample option requiring an argument:
			 * 
			 * else if (!strcmp(opt, "-output")) {
			 *     if (!val)
			 *         errs = TRUE, error(err_optnoarg, opt);
			 *     else
			 *         ofile = val;
			 * }
			 */
			else {
			    errs = TRUE;
			    fprintf(stderr,
				    "puttygen: no such option `--%s'\n", opt);
			}
		    }
		    p = NULL;
		    break;
		  case 'h':
		  case 'V':
		  case 'P':
		  case 'l':
		  case 'L':
		  case 'p':
		  case 'q':
		    /*
		     * Option requiring no parameter.
		     */
		    switch (c) {
		      case 'h':
			help();
			nogo = TRUE;
			break;
		      case 'V':
			showversion();
			nogo = TRUE;
			break;
		      case 'P':
			change_passphrase = TRUE;
			break;
		      case 'l':
			outtype = FP;
			break;
		      case 'L':
			outtype = PUBLICO;
			break;
		      case 'p':
			outtype = PUBLIC;
			break;
		      case 'q':
			progressfn = no_progress;
			break;
		    }
		    break;
		  case 't':
		  case 'b':
		  case 'C':
		  case 'O':
		  case 'o':
		    /*
		     * Option requiring parameter.
		     */
		    p++;
		    if (!*p && argc > 1)
			--argc, p = *++argv;
		    else if (!*p) {
			fprintf(stderr, "puttygen: option `-%c' expects a"
				" parameter\n", c);
			errs = TRUE;
		    }
		    /*
		     * Now c is the option and p is the parameter.
		     */
		    switch (c) {
		      case 't':
			if (!strcmp(p, "rsa") || !strcmp(p, "rsa2"))
			    keytype = RSA2, sshver = 2;
			else if (!strcmp(p, "rsa1"))
			    keytype = RSA1, sshver = 1;
			else if (!strcmp(p, "dsa") || !strcmp(p, "dss"))
			    keytype = DSA, sshver = 2;
			else {
			    fprintf(stderr,
				    "puttygen: unknown key type `%s'\n", p);
			    errs = TRUE;
			}
                        break;
		      case 'b':
			bits = atoi(p);
                        break;
		      case 'C':
			comment = p;
                        break;
		      case 'O':
			if (!strcmp(p, "public"))
			    outtype = PUBLIC;
			else if (!strcmp(p, "public-openssh"))
			    outtype = PUBLICO;
			else if (!strcmp(p, "private"))
			    outtype = PRIVATE;
			else if (!strcmp(p, "fingerprint"))
			    outtype = FP;
			else if (!strcmp(p, "private-openssh"))
			    outtype = OPENSSH, sshver = 2;
			else if (!strcmp(p, "private-sshcom"))
			    outtype = SSHCOM, sshver = 2;
			else {
			    fprintf(stderr,
				    "puttygen: unknown output type `%s'\n", p);
			    errs = TRUE;
			}
                        break;
		      case 'o':
			outfile = p;
                        break;
		    }
		    p = NULL;	       /* prevent continued processing */
		    break;
		  default:
		    /*
		     * Unrecognised option.
		     */
		    errs = TRUE;
		    fprintf(stderr, "puttygen: no such option `-%c'\n", c);
		    break;
		}
	    }
	} else {
	    /*
	     * A non-option argument.
	     */
	    if (!infile)
		infile = p;
	    else {
		errs = TRUE;
		fprintf(stderr, "puttygen: cannot handle more than one"
			" input file\n");
	    }
	}
    }

    if (errs)
	return 1;

    if (nogo)
	return 0;

    /*
     * If run with at least one argument _but_ not the required
     * ones, print the usage message and return failure.
     */
    if (!infile && keytype == NOKEYGEN) {
	usage();
	return 1;
    }

    /* ------------------------------------------------------------------
     * Figure out further details of exactly what we're going to do.
     */

    /*
     * Bomb out if we've been asked to both load and generate a
     * key.
     */
    if (keytype != NOKEYGEN && intype) {
	fprintf(stderr, "puttygen: cannot both load and generate a key\n");
	return 1;
    }

    /*
     * Analyse the type of the input file, in case this affects our
     * course of action.
     */
    if (infile) {
	infilename = filename_from_str(infile);

	intype = key_type(&infilename);

	switch (intype) {
	    /*
	     * It would be nice here to be able to load _public_
	     * key files, in any of a number of forms, and (a)
	     * convert them to other public key types, (b) print
	     * out their fingerprints. Or, I suppose, for real
	     * orthogonality, (c) change their comment!
	     * 
	     * In fact this opens some interesting possibilities.
	     * Suppose ssh2_userkey_loadpub() were able to load
	     * public key files as well as extracting the public
	     * key from private ones. And suppose I did the thing
	     * I've been wanting to do, where specifying a
	     * particular private key file for authentication
	     * causes any _other_ key in the agent to be discarded.
	     * Then, if you had an agent forwarded to the machine
	     * you were running Unix PuTTY or Plink on, and you
	     * needed to specify which of the keys in the agent it
	     * should use, you could do that by supplying a
	     * _public_ key file, thus not needing to trust even
	     * your encrypted private key file to the network. Ooh!
	     */

	  case SSH_KEYTYPE_UNOPENABLE:
	  case SSH_KEYTYPE_UNKNOWN:
	    fprintf(stderr, "puttygen: unable to load file `%s': %s\n",
		    infile, key_type_to_str(intype));
	    return 1;

	  case SSH_KEYTYPE_SSH1:
	    if (sshver == 2) {
		fprintf(stderr, "puttygen: conversion from SSH1 to SSH2 keys"
			" not supported\n");
		return 1;
	    }
	    sshver = 1;
	    break;

	  case SSH_KEYTYPE_SSH2:
	  case SSH_KEYTYPE_OPENSSH:
	  case SSH_KEYTYPE_SSHCOM:
	    if (sshver == 1) {
		fprintf(stderr, "puttygen: conversion from SSH2 to SSH1 keys"
			" not supported\n");
		return 1;
	    }
	    sshver = 2;
	    break;
	}
    }

    /*
     * Determine the default output file, if none is provided.
     * 
     * This will usually be equal to stdout, except that if the
     * input and output file formats are the same then the default
     * output is to overwrite the input.
     * 
     * Also in this code, we bomb out if the input and output file
     * formats are the same and no other action is performed.
     */
    if ((intype == SSH_KEYTYPE_SSH1 && outtype == PRIVATE) ||
	(intype == SSH_KEYTYPE_SSH2 && outtype == PRIVATE) ||
	(intype == SSH_KEYTYPE_OPENSSH && outtype == OPENSSH) ||
	(intype == SSH_KEYTYPE_SSHCOM && outtype == SSHCOM)) {
	if (!outfile) {
	    outfile = infile;
	    outfiletmp = dupcat(outfile, ".tmp", NULL);
	}

	if (!change_passphrase && !comment) {
	    fprintf(stderr, "puttygen: this command would perform no useful"
		    " action\n");
	    return 1;
	}
    } else {
	if (!outfile) {
	    /*
	     * Bomb out rather than automatically choosing to write
	     * a private key file to stdout.
	     */
	    if (outtype==PRIVATE || outtype==OPENSSH || outtype==SSHCOM) {
		fprintf(stderr, "puttygen: need to specify an output file\n");
		return 1;
	    }
	}
    }

    /*
     * Figure out whether we need to load the encrypted part of the
     * key. This will be the case if either (a) we need to write
     * out a private key format, or (b) the entire input key file
     * is encrypted.
     */
    if (outtype == PRIVATE || outtype == OPENSSH || outtype == SSHCOM ||
	intype == SSH_KEYTYPE_OPENSSH || intype == SSH_KEYTYPE_SSHCOM)
	load_encrypted = TRUE;
    else
	load_encrypted = FALSE;

    /* ------------------------------------------------------------------
     * Now we're ready to actually do some stuff.
     */

    /*
     * Either load or generate a key.
     */
    if (keytype != NOKEYGEN) {
	char *entropy;
	char default_comment[80];
	time_t t;
	struct tm *tm;
	struct progress prog;

	prog.phase = -1;
	prog.current = -1;

	time(&t);
	tm = localtime(&t);
	if (keytype == DSA)
	    strftime(default_comment, 30, "dsa-key-%Y%m%d", tm);
	else
	    strftime(default_comment, 30, "rsa-key-%Y%m%d", tm);

	random_init();
	entropy = get_random_data(bits / 8);
	random_add_heavynoise(entropy, bits / 8);
	memset(entropy, 0, bits/8);
	sfree(entropy);

	if (keytype == DSA) {
	    struct dss_key *dsskey = snew(struct dss_key);
	    dsa_generate(dsskey, bits, progressfn, &prog);
	    ssh2key = snew(struct ssh2_userkey);
	    ssh2key->data = dsskey;
	    ssh2key->alg = &ssh_dss;
	    ssh1key = NULL;
	} else {
	    struct RSAKey *rsakey = snew(struct RSAKey);
	    rsa_generate(rsakey, bits, progressfn, &prog);
	    rsakey->comment = NULL;
	    if (keytype == RSA1) {
		ssh1key = rsakey;
	    } else {
		ssh2key = snew(struct ssh2_userkey);
		ssh2key->data = rsakey;
		ssh2key->alg = &ssh_rsa;
	    }
	}
	progressfn(&prog, PROGFN_PROGRESS, INT_MAX, -1);

	if (ssh2key)
	    ssh2key->comment = dupstr(default_comment);
	if (ssh1key)
	    ssh1key->comment = dupstr(default_comment);

    } else {
	const char *error = NULL;
	int encrypted;

	assert(infile != NULL);

	/*
	 * Find out whether the input key is encrypted.
	 */
	if (intype == SSH_KEYTYPE_SSH1)
	    encrypted = rsakey_encrypted(&infilename, &origcomment);
	else if (intype == SSH_KEYTYPE_SSH2)
	    encrypted = ssh2_userkey_encrypted(&infilename, &origcomment);
	else
	    encrypted = import_encrypted(&infilename, intype, &origcomment);

	/*
	 * If so, ask for a passphrase.
	 */
	if (encrypted && load_encrypted) {
	    passphrase = snewn(512, char);
	    if (!console_get_line("Enter passphrase to load key: ",
				  passphrase, 512, TRUE)) {
		perror("puttygen: unable to read passphrase");
		return 1;
	    }
	} else {
	    passphrase = NULL;
	}

	switch (intype) {
	    int ret;

	  case SSH_KEYTYPE_SSH1:
	    ssh1key = snew(struct RSAKey);
	    if (!load_encrypted) {
		void *vblob;
		char *blob;
		int n, l, bloblen;

		ret = rsakey_pubblob(&infilename, &vblob, &bloblen, &error);
		blob = (char *)vblob;

		n = 4;		       /* skip modulus bits */
		
		l = ssh1_read_bignum(blob + n, bloblen - n,
				     &ssh1key->exponent);
		if (l < 0) {
		    error = "SSH1 public key blob was too short";
		} else {
		    n += l;
		    l = ssh1_read_bignum(blob + n, bloblen - n,
					 &ssh1key->modulus);
		    if (l < 0) {
			error = "SSH1 public key blob was too short";
		    } else
			n += l;
		}
		ssh1key->comment = NULL;
		ssh1key->private_exponent = NULL;
	    } else {
		ret = loadrsakey(&infilename, ssh1key, passphrase, &error);
	    }
	    if (ret > 0)
		error = NULL;
	    else if (!error)
		error = "unknown error";
	    break;

	  case SSH_KEYTYPE_SSH2:
	    if (!load_encrypted) {
		ssh2blob = ssh2_userkey_loadpub(&infilename, &ssh2alg,
						&ssh2bloblen, &error);
		ssh2algf = find_pubkey_alg(ssh2alg);
		if (ssh2algf)
		    bits = ssh2algf->pubkey_bits(ssh2blob, ssh2bloblen);
		else
		    bits = -1;
	    } else {
		ssh2key = ssh2_load_userkey(&infilename, passphrase, &error);
	    }
	    if ((ssh2key && ssh2key != SSH2_WRONG_PASSPHRASE) || ssh2blob)
		error = NULL;
	    else if (!error) {
		if (ssh2key == SSH2_WRONG_PASSPHRASE)
		    error = "wrong passphrase";
		else
		    error = "unknown error";
	    }
	    break;

	  case SSH_KEYTYPE_OPENSSH:
	  case SSH_KEYTYPE_SSHCOM:
	    ssh2key = import_ssh2(&infilename, intype, passphrase);
	    if (ssh2key && ssh2key != SSH2_WRONG_PASSPHRASE)
		error = NULL;
	    else if (!error) {
		if (ssh2key == SSH2_WRONG_PASSPHRASE)
		    error = "wrong passphrase";
		else
		    error = "unknown error";
	    }
	    break;

	  default:
	    assert(0);
	}

	if (error) {
	    fprintf(stderr, "puttygen: error loading `%s': %s\n",
		    infile, error);
	    return 1;
	}
    }

    /*
     * Change the comment if asked to.
     */
    if (comment) {
	if (sshver == 1) {
	    assert(ssh1key);
	    sfree(ssh1key->comment);
	    ssh1key->comment = dupstr(comment);
	} else {
	    assert(ssh2key);
	    sfree(ssh2key->comment);
	    ssh2key->comment = dupstr(comment);
	}
    }

    /*
     * Prompt for a new passphrase if we have been asked to, or if
     * we have just generated a key.
     */
    if (change_passphrase || keytype != NOKEYGEN) {
	char *passphrase2;

	if (passphrase) {
	    memset(passphrase, 0, strlen(passphrase));
	    sfree(passphrase);
	}

	passphrase = snewn(512, char);
	passphrase2 = snewn(512, char);
	if (!console_get_line("Enter passphrase to save key: ",
			      passphrase, 512, TRUE) ||
	    !console_get_line("Re-enter passphrase to verify: ",
			      passphrase2, 512, TRUE)) {
	    perror("puttygen: unable to read new passphrase");
	    return 1;
	}
	if (strcmp(passphrase, passphrase2)) {
	    fprintf(stderr, "puttygen: passphrases do not match\n");
	    return 1;
	}
	memset(passphrase2, 0, strlen(passphrase2));
	sfree(passphrase2);
	if (!*passphrase) {
	    sfree(passphrase);
	    passphrase = NULL;
	}
    }

    /*
     * Write output.
     * 
     * (In the case where outfile and outfiletmp are both NULL,
     * there is no semantic reason to initialise outfilename at
     * all; but we have to write _something_ to it or some compiler
     * will probably complain that it might be used uninitialised.)
     */
    if (outfiletmp)
	outfilename = filename_from_str(outfiletmp);
    else
	outfilename = filename_from_str(outfile ? outfile : "");

    switch (outtype) {
	int ret;

      case PRIVATE:
	if (sshver == 1) {
	    assert(ssh1key);
	    ret = saversakey(&outfilename, ssh1key, passphrase);
	    if (!ret) {
		fprintf(stderr, "puttygen: unable to save SSH1 private key\n");
		return 1;
	    }
	} else {
	    assert(ssh2key);
	    ret = ssh2_save_userkey(&outfilename, ssh2key, passphrase);
 	    if (!ret) {
		fprintf(stderr, "puttygen: unable to save SSH2 private key\n");
		return 1;
	    }
	}
	if (outfiletmp) {
	    if (!move(outfiletmp, outfile))
		return 1;	       /* rename failed */
	}
	break;

      case PUBLIC:
      case PUBLICO:
	if (sshver == 1) {
	    FILE *fp;
	    char *dec1, *dec2;

	    assert(ssh1key);

	    if (outfile)
		fp = f_open(outfilename, "w");
	    else
		fp = stdout;
	    dec1 = bignum_decimal(ssh1key->exponent);
	    dec2 = bignum_decimal(ssh1key->modulus);
	    fprintf(fp, "%d %s %s %s\n", bignum_bitcount(ssh1key->modulus),
		    dec1, dec2, ssh1key->comment);
	    sfree(dec1);
	    sfree(dec2);
	    if (outfile)
		fclose(fp);
	} else if (outtype == PUBLIC) {
	    if (!ssh2blob) {
		assert(ssh2key);
		ssh2blob = ssh2key->alg->public_blob(ssh2key->data,
						     &ssh2bloblen);
	    }
	    save_ssh2_pubkey(outfile, ssh2key ? ssh2key->comment : origcomment,
			     ssh2blob, ssh2bloblen);
	} else if (outtype == PUBLICO) {
	    char *buffer, *p;
	    int i;
	    FILE *fp;

	    if (!ssh2blob) {
		assert(ssh2key);
		ssh2blob = ssh2key->alg->public_blob(ssh2key->data,
						     &ssh2bloblen);
	    }
	    if (!ssh2alg) {
		assert(ssh2key);
		ssh2alg = ssh2key->alg->name;
	    }
	    if (ssh2key)
		comment = ssh2key->comment;
	    else
		comment = origcomment;

	    buffer = snewn(strlen(ssh2alg) +
			   4 * ((ssh2bloblen+2) / 3) +
			   strlen(comment) + 3, char);
	    strcpy(buffer, ssh2alg);
	    p = buffer + strlen(buffer);
	    *p++ = ' ';
	    i = 0;
	    while (i < ssh2bloblen) {
		int n = (ssh2bloblen - i < 3 ? ssh2bloblen - i : 3);
		base64_encode_atom(ssh2blob + i, n, p);
		i += n;
		p += 4;
	    }
	    if (*comment) {
		*p++ = ' ';
		strcpy(p, comment);
	    } else
		*p++ = '\0';

	    if (outfile)
		fp = f_open(outfilename, "w");
	    else
		fp = stdout;
	    fprintf(fp, "%s\n", buffer);
	    if (outfile)
		fclose(fp);

	    sfree(buffer);
	}
	break;

      case FP:
	{
	    FILE *fp;
	    char *fingerprint;

	    if (sshver == 1) {
		assert(ssh1key);
		fingerprint = snewn(128, char);
		rsa_fingerprint(fingerprint, 128, ssh1key);
	    } else {
		if (ssh2key) {
		    fingerprint = ssh2key->alg->fingerprint(ssh2key->data);
		} else {
		    assert(ssh2blob);
		    fingerprint = blobfp(ssh2alg, bits, ssh2blob, ssh2bloblen);
		}
	    }

	    if (outfile)
		fp = f_open(outfilename, "w");
	    else
		fp = stdout;
	    fprintf(fp, "%s\n", fingerprint);
	    if (outfile)
		fclose(fp);

	    sfree(fingerprint);
	}
	break;
	
      case OPENSSH:
      case SSHCOM:
	assert(sshver == 2);
	assert(ssh2key);
	ret = export_ssh2(&outfilename, outtype, ssh2key, passphrase);
	if (!ret) {
	    fprintf(stderr, "puttygen: unable to export key\n");
	    return 1;
	}
	if (outfiletmp) {
	    if (!move(outfiletmp, outfile))
		return 1;	       /* rename failed */
	}
	break;
    }

    if (passphrase) {
	memset(passphrase, 0, strlen(passphrase));
	sfree(passphrase);
    }

    if (ssh1key)
	freersakey(ssh1key);
    if (ssh2key) {
	ssh2key->alg->freekey(ssh2key->data);
	sfree(ssh2key);
    }

    return 0;
}

#ifdef TEST_CMDGEN

#undef main

#include <stdarg.h>

int passes, fails;

void setup_passphrases(char *first, ...)
{
    va_list ap;
    char *next;

    nprompts = 0;
    if (first) {
	prompts[nprompts++] = first;
	va_start(ap, first);
	while ((next = va_arg(ap, char *)) != NULL) {
	    assert(nprompts < lenof(prompts));
	    prompts[nprompts++] = next;
	}
	va_end(ap);
    }
}

void test(int retval, ...)
{
    va_list ap;
    int i, argc, ret;
    char **argv;

    argc = 0;
    va_start(ap, retval);
    while (va_arg(ap, char *) != NULL)
	argc++;
    va_end(ap);

    argv = snewn(argc+1, char *);
    va_start(ap, retval);
    for (i = 0; i <= argc; i++)
	argv[i] = va_arg(ap, char *);
    va_end(ap);

    promptsgot = 0;
    ret = cmdgen_main(argc, argv);

    if (ret != retval) {
	printf("FAILED retval (exp %d got %d):", retval, ret);
	for (i = 0; i < argc; i++)
	    printf(" %s", argv[i]);
	printf("\n");
	fails++;
    } else if (promptsgot != nprompts) {
	printf("FAILED nprompts (exp %d got %d):", nprompts, promptsgot);
	for (i = 0; i < argc; i++)
	    printf(" %s", argv[i]);
	printf("\n");
	fails++;
    } else {
	passes++;
    }
}

void filecmp(char *file1, char *file2, char *fmt, ...)
{
    /*
     * Ideally I should do file comparison myself, to maximise the
     * portability of this test suite once this application begins
     * running on non-Unix platforms. For the moment, though,
     * calling Unix diff is perfectly adequate.
     */
    char *buf;
    int ret;

    buf = dupprintf("diff -q '%s' '%s'", file1, file2);
    ret = system(buf);
    sfree(buf);

    if (ret) {
	va_list ap;

	printf("FAILED diff (ret=%d): ", ret);

	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);

	printf("\n");

	fails++;
    } else
	passes++;
}

char *cleanup_fp(char *s)
{
    char *p;

    if (!strncmp(s, "ssh-", 4)) {
	s += strcspn(s, " \n\t");
	s += strspn(s, " \n\t");
    }

    p = s;
    s += strcspn(s, " \n\t");
    s += strspn(s, " \n\t");
    s += strcspn(s, " \n\t");

    return dupprintf("%.*s", s - p, p);
}

char *get_fp(char *filename)
{
    FILE *fp;
    char buf[256], *ret;

    fp = fopen(filename, "r");
    if (!fp)
	return NULL;
    ret = fgets(buf, sizeof(buf), fp);
    fclose(fp);
    if (!ret)
	return NULL;
    return cleanup_fp(buf);
}

void check_fp(char *filename, char *fp, char *fmt, ...)
{
    char *newfp;

    if (!fp)
	return;

    newfp = get_fp(filename);

    if (!strcmp(fp, newfp)) {
	passes++;
    } else {
	va_list ap;

	printf("FAILED check_fp ['%s' != '%s']: ", newfp, fp);

	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);

	printf("\n");

	fails++;
    }

    sfree(newfp);
}

int main(int argc, char **argv)
{
    int i;
    static char *const keytypes[] = { "rsa1", "dsa", "rsa" };

    /*
     * Even when this thing is compiled for automatic test mode,
     * it's helpful to be able to invoke it with command-line
     * options for _manual_ tests.
     */
    if (argc > 1)
	return cmdgen_main(argc, argv);

    passes = fails = 0;

    for (i = 0; i < lenof(keytypes); i++) {
	char filename[128], osfilename[128], scfilename[128];
	char pubfilename[128], tmpfilename1[128], tmpfilename2[128];
	char *fp;

	sprintf(filename, "test-%s.ppk", keytypes[i]);
	sprintf(pubfilename, "test-%s.pub", keytypes[i]);
	sprintf(osfilename, "test-%s.os", keytypes[i]);
	sprintf(scfilename, "test-%s.sc", keytypes[i]);
	sprintf(tmpfilename1, "test-%s.tmp1", keytypes[i]);
	sprintf(tmpfilename2, "test-%s.tmp2", keytypes[i]);

	/*
	 * Create an encrypted key.
	 */
	setup_passphrases("sponge", "sponge", NULL);
	test(0, "puttygen", "-t", keytypes[i], "-o", filename, NULL);

	/*
	 * List the public key in OpenSSH format.
	 */
	setup_passphrases(NULL);
	test(0, "puttygen", "-L", filename, "-o", pubfilename, NULL);
	{
	    char cmdbuf[256];
	    fp = NULL;
	    sprintf(cmdbuf, "ssh-keygen -l -f '%s' > '%s'",
		    pubfilename, tmpfilename1);
	    if (system(cmdbuf) ||
		(fp = get_fp(tmpfilename1)) == NULL) {
		printf("UNABLE to test fingerprint matching against OpenSSH");
	    }
	}

	/*
	 * List the public key in IETF/ssh.com format.
	 */
	setup_passphrases(NULL);
	test(0, "puttygen", "-p", filename, NULL);

	/*
	 * List the fingerprint of the key.
	 */
	setup_passphrases(NULL);
	test(0, "puttygen", "-l", filename, "-o", tmpfilename1, NULL);
	if (!fp) {
	    /*
	     * If we can't test fingerprints against OpenSSH, we
	     * can at the very least test equality of all the
	     * fingerprints we generate of this key throughout
	     * testing.
	     */
	    fp = get_fp(tmpfilename1);
	} else {
	    check_fp(tmpfilename1, fp, "%s initial fp", keytypes[i]);
	}

	/*
	 * Change the comment of the key; this _does_ require a
	 * passphrase owing to the tamperproofing.
	 * 
	 * NOTE: In SSH1, this only requires a passphrase because
	 * of inadequacies of the loading and saving mechanisms. In
	 * _principle_, it should be perfectly possible to modify
	 * the comment on an SSH1 key without requiring a
	 * passphrase; the only reason I can't do it is because my
	 * loading and saving mechanisms don't include a method of
	 * loading all the key data without also trying to decrypt
	 * the private section.
	 * 
	 * I don't consider this to be a problem worth solving,
	 * because (a) to fix it would probably end up bloating
	 * PuTTY proper, and (b) SSH1 is on the way out anyway so
	 * it shouldn't be highly significant. If it seriously
	 * bothers anyone then perhaps I _might_ be persuadable.
	 */
	setup_passphrases("sponge", NULL);
	test(0, "puttygen", "-C", "new-comment", filename, NULL);

	/*
	 * Change the passphrase to nothing.
	 */
	setup_passphrases("sponge", "", "", NULL);
	test(0, "puttygen", "-P", filename, NULL);

	/*
	 * Change the comment of the key again; this time we expect no
	 * passphrase to be required.
	 */
	setup_passphrases(NULL);
	test(0, "puttygen", "-C", "new-comment-2", filename, NULL);

	/*
	 * Export the private key into OpenSSH format; no passphrase
	 * should be required since the key is currently unencrypted.
	 * For RSA1 keys, this should give an error.
	 */
	setup_passphrases(NULL);
	test((i==0), "puttygen", "-O", "private-openssh", "-o", osfilename,
	     filename, NULL);

	if (i) {
	    /*
	     * List the fingerprint of the OpenSSH-formatted key.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-l", osfilename, "-o", tmpfilename1, NULL);
	    check_fp(tmpfilename1, fp, "%s openssh clear fp", keytypes[i]);

	    /*
	     * List the public half of the OpenSSH-formatted key in
	     * OpenSSH format.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-L", osfilename, NULL);

	    /*
	     * List the public half of the OpenSSH-formatted key in
	     * IETF/ssh.com format.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-p", osfilename, NULL);
	}

	/*
	 * Export the private key into ssh.com format; no passphrase
	 * should be required since the key is currently unencrypted.
	 * For RSA1 keys, this should give an error.
	 */
	setup_passphrases(NULL);
	test((i==0), "puttygen", "-O", "private-sshcom", "-o", scfilename,
	     filename, NULL);

	if (i) {
	    /*
	     * List the fingerprint of the ssh.com-formatted key.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-l", scfilename, "-o", tmpfilename1, NULL);
	    check_fp(tmpfilename1, fp, "%s ssh.com clear fp", keytypes[i]);

	    /*
	     * List the public half of the ssh.com-formatted key in
	     * OpenSSH format.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-L", scfilename, NULL);

	    /*
	     * List the public half of the ssh.com-formatted key in
	     * IETF/ssh.com format.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-p", scfilename, NULL);
	}

	if (i) {
	    /*
	     * Convert from OpenSSH into ssh.com.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", osfilename, "-o", tmpfilename1,
		 "-O", "private-sshcom", NULL);

	    /*
	     * Convert from ssh.com back into a PuTTY key,
	     * supplying the same comment as we had before we
	     * started to ensure the comparison works.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", tmpfilename1, "-C", "new-comment-2",
		 "-o", tmpfilename2, NULL);

	    /*
	     * See if the PuTTY key thus generated is the same as
	     * the original.
	     */
	    filecmp(filename, tmpfilename2,
		    "p->o->s->p clear %s", keytypes[i]);

	    /*
	     * Convert from ssh.com to OpenSSH.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", scfilename, "-o", tmpfilename1,
		 "-O", "private-openssh", NULL);

	    /*
	     * Convert from OpenSSH back into a PuTTY key,
	     * supplying the same comment as we had before we
	     * started to ensure the comparison works.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", tmpfilename1, "-C", "new-comment-2",
		 "-o", tmpfilename2, NULL);

	    /*
	     * See if the PuTTY key thus generated is the same as
	     * the original.
	     */
	    filecmp(filename, tmpfilename2,
		    "p->s->o->p clear %s", keytypes[i]);

	    /*
	     * Finally, do a round-trip conversion between PuTTY
	     * and ssh.com without involving OpenSSH, to test that
	     * the key comment is preserved in that case.
	     */
	    setup_passphrases(NULL);
	    test(0, "puttygen", "-O", "private-sshcom", "-o", tmpfilename1,
		 filename, NULL);
	    setup_passphrases(NULL);
	    test(0, "puttygen", tmpfilename1, "-o", tmpfilename2, NULL);
	    filecmp(filename, tmpfilename2,
		    "p->s->p clear %s", keytypes[i]);
	}

	/*
	 * Check that mismatched passphrases cause an error.
	 */
	setup_passphrases("sponge2", "sponge3", NULL);
	test(1, "puttygen", "-P", filename, NULL);

	/*
	 * Put a passphrase back on.
	 */
	setup_passphrases("sponge2", "sponge2", NULL);
	test(0, "puttygen", "-P", filename, NULL);

	/*
	 * Export the private key into OpenSSH format, this time
	 * while encrypted. For RSA1 keys, this should give an
	 * error.
	 */
	if (i == 0)
	    setup_passphrases(NULL);   /* error, hence no passphrase read */
	else
	    setup_passphrases("sponge2", NULL);
	test((i==0), "puttygen", "-O", "private-openssh", "-o", osfilename,
	     filename, NULL);

	if (i) {
	    /*
	     * List the fingerprint of the OpenSSH-formatted key.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-l", osfilename, "-o", tmpfilename1, NULL);
	    check_fp(tmpfilename1, fp, "%s openssh encrypted fp", keytypes[i]);

	    /*
	     * List the public half of the OpenSSH-formatted key in
	     * OpenSSH format.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-L", osfilename, NULL);

	    /*
	     * List the public half of the OpenSSH-formatted key in
	     * IETF/ssh.com format.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-p", osfilename, NULL);
	}

	/*
	 * Export the private key into ssh.com format, this time
	 * while encrypted. For RSA1 keys, this should give an
	 * error.
	 */
	if (i == 0)
	    setup_passphrases(NULL);   /* error, hence no passphrase read */
	else
	    setup_passphrases("sponge2", NULL);
	test((i==0), "puttygen", "-O", "private-sshcom", "-o", scfilename,
	     filename, NULL);

	if (i) {
	    /*
	     * List the fingerprint of the ssh.com-formatted key.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-l", scfilename, "-o", tmpfilename1, NULL);
	    check_fp(tmpfilename1, fp, "%s ssh.com encrypted fp", keytypes[i]);

	    /*
	     * List the public half of the ssh.com-formatted key in
	     * OpenSSH format.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-L", scfilename, NULL);

	    /*
	     * List the public half of the ssh.com-formatted key in
	     * IETF/ssh.com format.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-p", scfilename, NULL);
	}

	if (i) {
	    /*
	     * Convert from OpenSSH into ssh.com.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", osfilename, "-o", tmpfilename1,
		 "-O", "private-sshcom", NULL);

	    /*
	     * Convert from ssh.com back into a PuTTY key,
	     * supplying the same comment as we had before we
	     * started to ensure the comparison works.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", tmpfilename1, "-C", "new-comment-2",
		 "-o", tmpfilename2, NULL);

	    /*
	     * See if the PuTTY key thus generated is the same as
	     * the original.
	     */
	    filecmp(filename, tmpfilename2,
		    "p->o->s->p encrypted %s", keytypes[i]);

	    /*
	     * Convert from ssh.com to OpenSSH.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", scfilename, "-o", tmpfilename1,
		 "-O", "private-openssh", NULL);

	    /*
	     * Convert from OpenSSH back into a PuTTY key,
	     * supplying the same comment as we had before we
	     * started to ensure the comparison works.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", tmpfilename1, "-C", "new-comment-2",
		 "-o", tmpfilename2, NULL);

	    /*
	     * See if the PuTTY key thus generated is the same as
	     * the original.
	     */
	    filecmp(filename, tmpfilename2,
		    "p->s->o->p encrypted %s", keytypes[i]);

	    /*
	     * Finally, do a round-trip conversion between PuTTY
	     * and ssh.com without involving OpenSSH, to test that
	     * the key comment is preserved in that case.
	     */
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", "-O", "private-sshcom", "-o", tmpfilename1,
		 filename, NULL);
	    setup_passphrases("sponge2", NULL);
	    test(0, "puttygen", tmpfilename1, "-o", tmpfilename2, NULL);
	    filecmp(filename, tmpfilename2,
		    "p->s->p encrypted %s", keytypes[i]);
	}

	/*
	 * Load with the wrong passphrase.
	 */
	setup_passphrases("sponge8", NULL);
	test(1, "puttygen", "-C", "spurious-new-comment", filename, NULL);

	/*
	 * Load a totally bogus file.
	 */
	setup_passphrases(NULL);
	test(1, "puttygen", "-C", "spurious-new-comment", pubfilename, NULL);
    }
    printf("%d passes, %d fails\n", passes, fails);
    return 0;
}

#endif
