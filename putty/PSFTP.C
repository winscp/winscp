/*
 * psftp.c: front end for PSFTP.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>

#define PUTTY_DO_GLOBALS
#include "putty.h"
#include "psftp.h"
#include "storage.h"
#include "ssh.h"
#include "sftp.h"
#include "int64.h"

/*
 * Since SFTP is a request-response oriented protocol, it requires
 * no buffer management: when we send data, we stop and wait for an
 * acknowledgement _anyway_, and so we can't possibly overfill our
 * send buffer.
 */

static int psftp_connect(char *userhost, char *user, int portnumber);
static int do_sftp_init(void);

/* ----------------------------------------------------------------------
 * sftp client state.
 */

char *pwd, *homedir;
static Backend *back;
static void *backhandle;
static Config cfg;

/* ----------------------------------------------------------------------
 * Higher-level helper functions used in commands.
 */

/*
 * Attempt to canonify a pathname starting from the pwd. If
 * canonification fails, at least fall back to returning a _valid_
 * pathname (though it may be ugly, eg /home/simon/../foobar).
 */
char *canonify(char *name)
{
    char *fullname, *canonname;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;

    if (name[0] == '/') {
	fullname = dupstr(name);
    } else {
	char *slash;
	if (pwd[strlen(pwd) - 1] == '/')
	    slash = "";
	else
	    slash = "/";
	fullname = dupcat(pwd, slash, name, NULL);
    }

    sftp_register(req = fxp_realpath_send(fullname));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    canonname = fxp_realpath_recv(pktin, rreq);

    if (canonname) {
	sfree(fullname);
	return canonname;
    } else {
	/*
	 * Attempt number 2. Some FXP_REALPATH implementations
	 * (glibc-based ones, in particular) require the _whole_
	 * path to point to something that exists, whereas others
	 * (BSD-based) only require all but the last component to
	 * exist. So if the first call failed, we should strip off
	 * everything from the last slash onwards and try again,
	 * then put the final component back on.
	 * 
	 * Special cases:
	 * 
	 *  - if the last component is "/." or "/..", then we don't
	 *    bother trying this because there's no way it can work.
	 * 
	 *  - if the thing actually ends with a "/", we remove it
	 *    before we start. Except if the string is "/" itself
	 *    (although I can't see why we'd have got here if so,
	 *    because surely "/" would have worked the first
	 *    time?), in which case we don't bother.
	 * 
	 *  - if there's no slash in the string at all, give up in
	 *    confusion (we expect at least one because of the way
	 *    we constructed the string).
	 */

	int i;
	char *returnname;

	i = strlen(fullname);
	if (i > 2 && fullname[i - 1] == '/')
	    fullname[--i] = '\0';      /* strip trailing / unless at pos 0 */
	while (i > 0 && fullname[--i] != '/');

	/*
	 * Give up on special cases.
	 */
	if (fullname[i] != '/' ||      /* no slash at all */
	    !strcmp(fullname + i, "/.") ||	/* ends in /. */
	    !strcmp(fullname + i, "/..") ||	/* ends in /.. */
	    !strcmp(fullname, "/")) {
	    return fullname;
	}

	/*
	 * Now i points at the slash. Deal with the final special
	 * case i==0 (ie the whole path was "/nonexistentfile").
	 */
	fullname[i] = '\0';	       /* separate the string */
	if (i == 0) {
	    sftp_register(req = fxp_realpath_send("/"));
	} else {
	    sftp_register(req = fxp_realpath_send(fullname));
	}
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	canonname = fxp_realpath_recv(pktin, rreq);

	if (!canonname)
	    return fullname;	       /* even that failed; give up */

	/*
	 * We have a canonical name for all but the last path
	 * component. Concatenate the last component and return.
	 */
	returnname = dupcat(canonname,
			    canonname[strlen(canonname) - 1] ==
			    '/' ? "" : "/", fullname + i + 1, NULL);
	sfree(fullname);
	sfree(canonname);
	return returnname;
    }
}

/*
 * Return a pointer to the portion of str that comes after the last
 * slash (or backslash or colon, if `local' is TRUE).
 */
static char *stripslashes(char *str, int local)
{
    char *p;

    if (local) {
        p = strchr(str, ':');
        if (p) str = p+1;
    }

    p = strrchr(str, '/');
    if (p) str = p+1;

    if (local) {
	p = strrchr(str, '\\');
	if (p) str = p+1;
    }

    return str;
}

/* ----------------------------------------------------------------------
 * Actual sftp commands.
 */
struct sftp_command {
    char **words;
    int nwords, wordssize;
    int (*obey) (struct sftp_command *);	/* returns <0 to quit */
};

int sftp_cmd_null(struct sftp_command *cmd)
{
    return 1;			       /* success */
}

int sftp_cmd_unknown(struct sftp_command *cmd)
{
    printf("psftp: unknown command \"%s\"\n", cmd->words[0]);
    return 0;			       /* failure */
}

int sftp_cmd_quit(struct sftp_command *cmd)
{
    return -1;
}

/*
 * List a directory. If no arguments are given, list pwd; otherwise
 * list the directory given in words[1].
 */
static int sftp_ls_compare(const void *av, const void *bv)
{
    const struct fxp_name *const *a = (const struct fxp_name *const *) av;
    const struct fxp_name *const *b = (const struct fxp_name *const *) bv;
    return strcmp((*a)->filename, (*b)->filename);
}
int sftp_cmd_ls(struct sftp_command *cmd)
{
    struct fxp_handle *dirh;
    struct fxp_names *names;
    struct fxp_name **ournames;
    int nnames, namesize;
    char *dir, *cdir;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    int i;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2)
	dir = ".";
    else
	dir = cmd->words[1];

    cdir = canonify(dir);
    if (!cdir) {
	printf("%s: %s\n", dir, fxp_error());
	return 0;
    }

    printf("Listing directory %s\n", cdir);

    sftp_register(req = fxp_opendir_send(cdir));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    dirh = fxp_opendir_recv(pktin, rreq);

    if (dirh == NULL) {
	printf("Unable to open %s: %s\n", dir, fxp_error());
    } else {
	nnames = namesize = 0;
	ournames = NULL;

	while (1) {

	    sftp_register(req = fxp_readdir_send(dirh));
	    rreq = sftp_find_request(pktin = sftp_recv());
	    assert(rreq == req);
	    names = fxp_readdir_recv(pktin, rreq);

	    if (names == NULL) {
		if (fxp_error_type() == SSH_FX_EOF)
		    break;
		printf("Reading directory %s: %s\n", dir, fxp_error());
		break;
	    }
	    if (names->nnames == 0) {
		fxp_free_names(names);
		break;
	    }

	    if (nnames + names->nnames >= namesize) {
		namesize += names->nnames + 128;
		ournames = sresize(ournames, namesize, struct fxp_name *);
	    }

	    for (i = 0; i < names->nnames; i++)
		ournames[nnames++] = fxp_dup_name(&names->names[i]);

	    fxp_free_names(names);
	}
	sftp_register(req = fxp_close_send(dirh));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	fxp_close_recv(pktin, rreq);

	/*
	 * Now we have our filenames. Sort them by actual file
	 * name, and then output the longname parts.
	 */
	qsort(ournames, nnames, sizeof(*ournames), sftp_ls_compare);

	/*
	 * And print them.
	 */
	for (i = 0; i < nnames; i++) {
	    printf("%s\n", ournames[i]->longname);
	    fxp_free_name(ournames[i]);
	}
	sfree(ournames);
    }

    sfree(cdir);

    return 1;
}

/*
 * Change directories. We do this by canonifying the new name, then
 * trying to OPENDIR it. Only if that succeeds do we set the new pwd.
 */
int sftp_cmd_cd(struct sftp_command *cmd)
{
    struct fxp_handle *dirh;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    char *dir;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2)
	dir = dupstr(homedir);
    else
	dir = canonify(cmd->words[1]);

    if (!dir) {
	printf("%s: %s\n", dir, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_opendir_send(dir));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    dirh = fxp_opendir_recv(pktin, rreq);

    if (!dirh) {
	printf("Directory %s: %s\n", dir, fxp_error());
	sfree(dir);
	return 0;
    }

    sftp_register(req = fxp_close_send(dirh));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    fxp_close_recv(pktin, rreq);

    sfree(pwd);
    pwd = dir;
    printf("Remote directory is now %s\n", pwd);

    return 1;
}

/*
 * Print current directory. Easy as pie.
 */
int sftp_cmd_pwd(struct sftp_command *cmd)
{
    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    printf("Remote directory is %s\n", pwd);
    return 1;
}

/*
 * Get a file and save it at the local end. We have two very
 * similar commands here: `get' and `reget', which differ in that
 * `reget' checks for the existence of the destination file and
 * starts from where a previous aborted transfer left off.
 */
int sftp_general_get(struct sftp_command *cmd, int restart)
{
    struct fxp_handle *fh;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    char *fname, *outfname;
    uint64 offset;
    FILE *fp;
    int ret;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("get: expects a filename\n");
	return 0;
    }

    fname = canonify(cmd->words[1]);
    if (!fname) {
	printf("%s: %s\n", cmd->words[1], fxp_error());
	return 0;
    }
    outfname = (cmd->nwords == 2 ?
		stripslashes(cmd->words[1], 0) : cmd->words[2]);

    sftp_register(req = fxp_open_send(fname, SSH_FXF_READ));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    fh = fxp_open_recv(pktin, rreq);

    if (!fh) {
	printf("%s: %s\n", fname, fxp_error());
	sfree(fname);
	return 0;
    }

    if (restart) {
	fp = fopen(outfname, "rb+");
    } else {
	fp = fopen(outfname, "wb");
    }

    if (!fp) {
	printf("local: unable to open %s\n", outfname);

	sftp_register(req = fxp_close_send(fh));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	fxp_close_recv(pktin, rreq);

	sfree(fname);
	return 0;
    }

    if (restart) {
	long posn;
	fseek(fp, 0L, SEEK_END);
	posn = ftell(fp);
	printf("reget: restarting at file position %ld\n", posn);
	offset = uint64_make(0, posn);
    } else {
	offset = uint64_make(0, 0);
    }

    printf("remote:%s => local:%s\n", fname, outfname);

    /*
     * FIXME: we can use FXP_FSTAT here to get the file size, and
     * thus put up a progress bar.
     */
    ret = 1;
    while (1) {
	char buffer[4096];
	int len;
	int wpos, wlen;

	sftp_register(req = fxp_read_send(fh, offset, sizeof(buffer)));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	len = fxp_read_recv(pktin, rreq, buffer, sizeof(buffer));

	if ((len == -1 && fxp_error_type() == SSH_FX_EOF) || len == 0)
	    break;
	if (len == -1) {
	    printf("error while reading: %s\n", fxp_error());
	    ret = 0;
	    break;
	}

	wpos = 0;
	while (wpos < len) {
	    wlen = fwrite(buffer, 1, len - wpos, fp);
	    if (wlen <= 0) {
		printf("error while writing local file\n");
		ret = 0;
		break;
	    }
	    wpos += wlen;
	}
	if (wpos < len) {	       /* we had an error */
	    ret = 0;
	    break;
	}
	offset = uint64_add32(offset, len);
    }

    fclose(fp);

    sftp_register(req = fxp_close_send(fh));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    fxp_close_recv(pktin, rreq);

    sfree(fname);

    return ret;
}
int sftp_cmd_get(struct sftp_command *cmd)
{
    return sftp_general_get(cmd, 0);
}
int sftp_cmd_reget(struct sftp_command *cmd)
{
    return sftp_general_get(cmd, 1);
}

/*
 * Send a file and store it at the remote end. We have two very
 * similar commands here: `put' and `reput', which differ in that
 * `reput' checks for the existence of the destination file and
 * starts from where a previous aborted transfer left off.
 */
int sftp_general_put(struct sftp_command *cmd, int restart)
{
    struct fxp_handle *fh;
    char *fname, *origoutfname, *outfname;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    uint64 offset;
    FILE *fp;
    int ret;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("put: expects a filename\n");
	return 0;
    }

    fname = cmd->words[1];
    origoutfname = (cmd->nwords == 2 ?
		    stripslashes(cmd->words[1], 1) : cmd->words[2]);
    outfname = canonify(origoutfname);
    if (!outfname) {
	printf("%s: %s\n", origoutfname, fxp_error());
	return 0;
    }

    fp = fopen(fname, "rb");
    if (!fp) {
	printf("local: unable to open %s\n", fname);
	sfree(outfname);
	return 0;
    }
    if (restart) {
	sftp_register(req = fxp_open_send(outfname, SSH_FXF_WRITE));
    } else {
	sftp_register(req = fxp_open_send(outfname, SSH_FXF_WRITE |
					  SSH_FXF_CREAT | SSH_FXF_TRUNC));
    }
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    fh = fxp_open_recv(pktin, rreq);

    if (!fh) {
	printf("%s: %s\n", outfname, fxp_error());
	sfree(outfname);
	return 0;
    }

    if (restart) {
	char decbuf[30];
	struct fxp_attrs attrs;
	int ret;

	sftp_register(req = fxp_fstat_send(fh));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	ret = fxp_fstat_recv(pktin, rreq, &attrs);

	if (!ret) {
	    printf("read size of %s: %s\n", outfname, fxp_error());
	    sfree(outfname);
	    return 0;
	}
	if (!(attrs.flags & SSH_FILEXFER_ATTR_SIZE)) {
	    printf("read size of %s: size was not given\n", outfname);
	    sfree(outfname);
	    return 0;
	}
	offset = attrs.size;
	uint64_decimal(offset, decbuf);
	printf("reput: restarting at file position %s\n", decbuf);
	if (uint64_compare(offset, uint64_make(0, LONG_MAX)) > 0) {
	    printf("reput: remote file is larger than we can deal with\n");
	    sfree(outfname);
	    return 0;
	}
	if (fseek(fp, offset.lo, SEEK_SET) != 0)
	    fseek(fp, 0, SEEK_END);    /* *shrug* */
    } else {
	offset = uint64_make(0, 0);
    }

    printf("local:%s => remote:%s\n", fname, outfname);

    /*
     * FIXME: we can use FXP_FSTAT here to get the file size, and
     * thus put up a progress bar.
     */
    ret = 1;
    while (1) {
	char buffer[4096];
	int len, ret;

	len = fread(buffer, 1, sizeof(buffer), fp);
	if (len == -1) {
	    printf("error while reading local file\n");
	    ret = 0;
	    break;
	} else if (len == 0) {
	    break;
	}

	sftp_register(req = fxp_write_send(fh, buffer, offset, len));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	ret = fxp_write_recv(pktin, rreq);

	if (!ret) {
	    printf("error while writing: %s\n", fxp_error());
	    ret = 0;
	    break;
	}
	offset = uint64_add32(offset, len);
    }

    sftp_register(req = fxp_close_send(fh));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    fxp_close_recv(pktin, rreq);

    fclose(fp);
    sfree(outfname);

    return ret;
}
int sftp_cmd_put(struct sftp_command *cmd)
{
    return sftp_general_put(cmd, 0);
}
int sftp_cmd_reput(struct sftp_command *cmd)
{
    return sftp_general_put(cmd, 1);
}

int sftp_cmd_mkdir(struct sftp_command *cmd)
{
    char *dir;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    int result;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("mkdir: expects a directory\n");
	return 0;
    }

    dir = canonify(cmd->words[1]);
    if (!dir) {
	printf("%s: %s\n", dir, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_mkdir_send(dir));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_mkdir_recv(pktin, rreq);

    if (!result) {
	printf("mkdir %s: %s\n", dir, fxp_error());
	sfree(dir);
	return 0;
    }

    sfree(dir);
    return 1;
}

int sftp_cmd_rmdir(struct sftp_command *cmd)
{
    char *dir;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    int result;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("rmdir: expects a directory\n");
	return 0;
    }

    dir = canonify(cmd->words[1]);
    if (!dir) {
	printf("%s: %s\n", dir, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_rmdir_send(dir));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_rmdir_recv(pktin, rreq);

    if (!result) {
	printf("rmdir %s: %s\n", dir, fxp_error());
	sfree(dir);
	return 0;
    }

    sfree(dir);
    return 1;
}

int sftp_cmd_rm(struct sftp_command *cmd)
{
    char *fname;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    int result;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("rm: expects a filename\n");
	return 0;
    }

    fname = canonify(cmd->words[1]);
    if (!fname) {
	printf("%s: %s\n", fname, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_remove_send(fname));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_remove_recv(pktin, rreq);

    if (!result) {
	printf("rm %s: %s\n", fname, fxp_error());
	sfree(fname);
	return 0;
    }

    sfree(fname);
    return 1;
}

int sftp_cmd_mv(struct sftp_command *cmd)
{
    char *srcfname, *dstfname;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;
    int result;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 3) {
	printf("mv: expects two filenames\n");
	return 0;
    }
    srcfname = canonify(cmd->words[1]);
    if (!srcfname) {
	printf("%s: %s\n", srcfname, fxp_error());
	return 0;
    }

    dstfname = canonify(cmd->words[2]);
    if (!dstfname) {
	printf("%s: %s\n", dstfname, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_rename_send(srcfname, dstfname));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_rename_recv(pktin, rreq);

    if (!result) {
	char const *error = fxp_error();
	struct fxp_attrs attrs;

	/*
	 * The move might have failed because dstfname pointed at a
	 * directory. We check this possibility now: if dstfname
	 * _is_ a directory, we re-attempt the move by appending
	 * the basename of srcfname to dstfname.
	 */
	sftp_register(req = fxp_stat_send(dstfname));
	rreq = sftp_find_request(pktin = sftp_recv());
	assert(rreq == req);
	result = fxp_stat_recv(pktin, rreq, &attrs);

	if (result &&
	    (attrs.flags & SSH_FILEXFER_ATTR_PERMISSIONS) &&
	    (attrs.permissions & 0040000)) {
	    char *p;
	    char *newname, *newcanon;
	    printf("(destination %s is a directory)\n", dstfname);
	    p = srcfname + strlen(srcfname);
	    while (p > srcfname && p[-1] != '/') p--;
	    newname = dupcat(dstfname, "/", p, NULL);
	    newcanon = canonify(newname);
	    sfree(newname);
	    if (newcanon) {
		sfree(dstfname);
		dstfname = newcanon;

		sftp_register(req = fxp_rename_send(srcfname, dstfname));
		rreq = sftp_find_request(pktin = sftp_recv());
		assert(rreq == req);
		result = fxp_rename_recv(pktin, rreq);

		error = result ? NULL : fxp_error();
	    }
	}
	if (error) {
	    printf("mv %s %s: %s\n", srcfname, dstfname, error);
	    sfree(srcfname);
	    sfree(dstfname);
	    return 0;
	}
    }
    printf("%s -> %s\n", srcfname, dstfname);

    sfree(srcfname);
    sfree(dstfname);
    return 1;
}

int sftp_cmd_chmod(struct sftp_command *cmd)
{
    char *fname, *mode;
    int result;
    struct fxp_attrs attrs;
    unsigned attrs_clr, attrs_xor, oldperms, newperms;
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;

    if (back == NULL) {
	printf("psftp: not connected to a host; use \"open host.name\"\n");
	return 0;
    }

    if (cmd->nwords < 3) {
	printf("chmod: expects a mode specifier and a filename\n");
	return 0;
    }

    /*
     * Attempt to parse the mode specifier in cmd->words[1]. We
     * don't support the full horror of Unix chmod; instead we
     * support a much simpler syntax in which the user can either
     * specify an octal number, or a comma-separated sequence of
     * [ugoa]*[-+=][rwxst]+. (The initial [ugoa] sequence may
     * _only_ be omitted if the only attribute mentioned is t,
     * since all others require a user/group/other specification.
     * Additionally, the s attribute may not be specified for any
     * [ugoa] specifications other than exactly u or exactly g.
     */
    attrs_clr = attrs_xor = 0;
    mode = cmd->words[1];
    if (mode[0] >= '0' && mode[0] <= '9') {
	if (mode[strspn(mode, "01234567")]) {
	    printf("chmod: numeric file modes should"
		   " contain digits 0-7 only\n");
	    return 0;
	}
	attrs_clr = 07777;
	sscanf(mode, "%o", &attrs_xor);
	attrs_xor &= attrs_clr;
    } else {
	while (*mode) {
	    char *modebegin = mode;
	    unsigned subset, perms;
	    int action;

	    subset = 0;
	    while (*mode && *mode != ',' &&
		   *mode != '+' && *mode != '-' && *mode != '=') {
		switch (*mode) {
		  case 'u': subset |= 04700; break; /* setuid, user perms */
		  case 'g': subset |= 02070; break; /* setgid, group perms */
		  case 'o': subset |= 00007; break; /* just other perms */
		  case 'a': subset |= 06777; break; /* all of the above */
		  default:
		    printf("chmod: file mode '%.*s' contains unrecognised"
			   " user/group/other specifier '%c'\n",
			   (int)strcspn(modebegin, ","), modebegin, *mode);
		    return 0;
		}
		mode++;
	    }
	    if (!*mode || *mode == ',') {
		printf("chmod: file mode '%.*s' is incomplete\n",
		       (int)strcspn(modebegin, ","), modebegin);
		return 0;
	    }
	    action = *mode++;
	    if (!*mode || *mode == ',') {
		printf("chmod: file mode '%.*s' is incomplete\n",
		       (int)strcspn(modebegin, ","), modebegin);
		return 0;
	    }
	    perms = 0;
	    while (*mode && *mode != ',') {
		switch (*mode) {
		  case 'r': perms |= 00444; break;
		  case 'w': perms |= 00222; break;
		  case 'x': perms |= 00111; break;
		  case 't': perms |= 01000; subset |= 01000; break;
		  case 's':
		    if ((subset & 06777) != 04700 &&
			(subset & 06777) != 02070) {
			printf("chmod: file mode '%.*s': set[ug]id bit should"
			       " be used with exactly one of u or g only\n",
			       (int)strcspn(modebegin, ","), modebegin);
			return 0;
		    }
		    perms |= 06000;
		    break;
		  default:
		    printf("chmod: file mode '%.*s' contains unrecognised"
			   " permission specifier '%c'\n",
			   (int)strcspn(modebegin, ","), modebegin, *mode);
		    return 0;
		}
		mode++;
	    }
	    if (!(subset & 06777) && (perms &~ subset)) {
		printf("chmod: file mode '%.*s' contains no user/group/other"
		       " specifier and permissions other than 't' \n",
		       (int)strcspn(modebegin, ","), modebegin);
		return 0;
	    }
	    perms &= subset;
	    switch (action) {
	      case '+':
		attrs_clr |= perms;
		attrs_xor |= perms;
		break;
	      case '-':
		attrs_clr |= perms;
		attrs_xor &= ~perms;
		break;
	      case '=':
		attrs_clr |= subset;
		attrs_xor |= perms;
		break;
	    }
	    if (*mode) mode++;	       /* eat comma */
	}
    }

    fname = canonify(cmd->words[2]);
    if (!fname) {
	printf("%s: %s\n", fname, fxp_error());
	return 0;
    }

    sftp_register(req = fxp_stat_send(fname));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_stat_recv(pktin, rreq, &attrs);

    if (!result || !(attrs.flags & SSH_FILEXFER_ATTR_PERMISSIONS)) {
	printf("get attrs for %s: %s\n", fname,
	       result ? "file permissions not provided" : fxp_error());
	sfree(fname);
	return 0;
    }

    attrs.flags = SSH_FILEXFER_ATTR_PERMISSIONS;   /* perms _only_ */
    oldperms = attrs.permissions & 07777;
    attrs.permissions &= ~attrs_clr;
    attrs.permissions ^= attrs_xor;
    newperms = attrs.permissions & 07777;

    sftp_register(req = fxp_setstat_send(fname, attrs));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    result = fxp_setstat_recv(pktin, rreq);

    if (!result) {
	printf("set attrs for %s: %s\n", fname, fxp_error());
	sfree(fname);
	return 0;
    }

    printf("%s: %04o -> %04o\n", fname, oldperms, newperms);

    sfree(fname);
    return 1;
}

static int sftp_cmd_open(struct sftp_command *cmd)
{
    if (back != NULL) {
	printf("psftp: already connected\n");
	return 0;
    }

    if (cmd->nwords < 2) {
	printf("open: expects a host name\n");
	return 0;
    }

    if (psftp_connect(cmd->words[1], NULL, 0)) {
	back = NULL;		       /* connection is already closed */
	return -1;		       /* this is fatal */
    }
    do_sftp_init();
    return 1;
}

static int sftp_cmd_lcd(struct sftp_command *cmd)
{
    char *currdir, *errmsg;

    if (cmd->nwords < 2) {
	printf("lcd: expects a local directory name\n");
	return 0;
    }

    errmsg = psftp_lcd(cmd->words[1]);
    if (errmsg) {
	printf("lcd: unable to change directory: %s\n", errmsg);
	sfree(errmsg);
	return 0;
    }

    currdir = psftp_getcwd();
    printf("New local directory is %s\n", currdir);
    sfree(currdir);

    return 1;
}

static int sftp_cmd_lpwd(struct sftp_command *cmd)
{
    char *currdir;

    currdir = psftp_getcwd();
    printf("Current local directory is %s\n", currdir);
    sfree(currdir);

    return 1;
}

static int sftp_cmd_pling(struct sftp_command *cmd)
{
    int exitcode;

    exitcode = system(cmd->words[1]);
    return (exitcode == 0);
}

static int sftp_cmd_help(struct sftp_command *cmd);

static struct sftp_cmd_lookup {
    char *name;
    /*
     * For help purposes, there are two kinds of command:
     * 
     *  - primary commands, in which `longhelp' is non-NULL. In
     *    this case `shorthelp' is descriptive text, and `longhelp'
     *    is longer descriptive text intended to be printed after
     *    the command name.
     * 
     *  - alias commands, in which `longhelp' is NULL. In this case
     *    `shorthelp' is the name of a primary command, which
     *    contains the help that should double up for this command.
     */
    int listed;			       /* do we list this in primary help? */
    char *shorthelp;
    char *longhelp;
    int (*obey) (struct sftp_command *);
} sftp_lookup[] = {
    /*
     * List of sftp commands. This is binary-searched so it MUST be
     * in ASCII order.
     */
    {
	"!", TRUE, "run a local command",
	    "<command>\n"
	    /* FIXME: this example is crap for non-Windows. */
	    "  Runs a local command. For example, \"!del myfile\".\n",
	    sftp_cmd_pling
    },
    {
	"bye", TRUE, "finish your SFTP session",
	    "\n"
	    "  Terminates your SFTP session and quits the PSFTP program.\n",
	    sftp_cmd_quit
    },
    {
	"cd", TRUE, "change your remote working directory",
	    " [ <New working directory> ]\n"
	    "  Change the remote working directory for your SFTP session.\n"
	    "  If a new working directory is not supplied, you will be\n"
	    "  returned to your home directory.\n",
	    sftp_cmd_cd
    },
    {
	"chmod", TRUE, "change file permissions and modes",
	    " ( <octal-digits> | <modifiers> ) <filename>\n"
	    "  Change the file permissions on a file or directory.\n"
	    "  <octal-digits> can be any octal Unix permission specifier.\n"
	    "  Alternatively, <modifiers> can include:\n"
	    "    u+r     make file readable by owning user\n"
	    "    u+w     make file writable by owning user\n"
	    "    u+x     make file executable by owning user\n"
	    "    u-r     make file not readable by owning user\n"
	    "    [also u-w, u-x]\n"
	    "    g+r     make file readable by members of owning group\n"
	    "    [also g+w, g+x, g-r, g-w, g-x]\n"
	    "    o+r     make file readable by all other users\n"
	    "    [also o+w, o+x, o-r, o-w, o-x]\n"
	    "    a+r     make file readable by absolutely everybody\n"
	    "    [also a+w, a+x, a-r, a-w, a-x]\n"
	    "    u+s     enable the Unix set-user-ID bit\n"
	    "    u-s     disable the Unix set-user-ID bit\n"
	    "    g+s     enable the Unix set-group-ID bit\n"
	    "    g-s     disable the Unix set-group-ID bit\n"
	    "    +t      enable the Unix \"sticky bit\"\n"
	    "  You can give more than one modifier for the same user (\"g-rwx\"), and\n"
	    "  more than one user for the same modifier (\"ug+w\"). You can\n"
	    "  use commas to separate different modifiers (\"u+rwx,g+s\").\n",
	    sftp_cmd_chmod
    },
    {
	"del", TRUE, "delete a file",
	    " <filename>\n"
	    "  Delete a file.\n",
	    sftp_cmd_rm
    },
    {
	"delete", FALSE, "del", NULL, sftp_cmd_rm
    },
    {
	"dir", TRUE, "list contents of a remote directory",
	    " [ <directory-name> ]\n"
	    "  List the contents of a specified directory on the server.\n"
	    "  If <directory-name> is not given, the current working directory\n"
	    "  will be listed.\n",
	    sftp_cmd_ls
    },
    {
	"exit", TRUE, "bye", NULL, sftp_cmd_quit
    },
    {
	"get", TRUE, "download a file from the server to your local machine",
	    " <filename> [ <local-filename> ]\n"
	    "  Downloads a file on the server and stores it locally under\n"
	    "  the same name, or under a different one if you supply the\n"
	    "  argument <local-filename>.\n",
	    sftp_cmd_get
    },
    {
	"help", TRUE, "give help",
	    " [ <command> [ <command> ... ] ]\n"
	    "  Give general help if no commands are specified.\n"
	    "  If one or more commands are specified, give specific help on\n"
	    "  those particular commands.\n",
	    sftp_cmd_help
    },
    {
	"lcd", TRUE, "change local working directory",
	    " <local-directory-name>\n"
	    "  Change the local working directory of the PSFTP program (the\n"
	    "  default location where the \"get\" command will save files).\n",
	    sftp_cmd_lcd
    },
    {
	"lpwd", TRUE, "print local working directory",
	    "\n"
	    "  Print the local working directory of the PSFTP program (the\n"
	    "  default location where the \"get\" command will save files).\n",
	    sftp_cmd_lpwd
    },
    {
	"ls", TRUE, "dir", NULL,
	    sftp_cmd_ls
    },
    {
	"mkdir", TRUE, "create a directory on the remote server",
	    " <directory-name>\n"
	    "  Creates a directory with the given name on the server.\n",
	    sftp_cmd_mkdir
    },
    {
	"mv", TRUE, "move or rename a file on the remote server",
	    " <source-filename> <destination-filename>\n"
	    "  Moves or renames the file <source-filename> on the server,\n"
	    "  so that it is accessible under the name <destination-filename>.\n",
	    sftp_cmd_mv
    },
    {
	"open", TRUE, "connect to a host",
	    " [<user>@]<hostname>\n"
	    "  Establishes an SFTP connection to a given host. Only usable\n"
	    "  when you did not already specify a host name on the command\n"
	    "  line.\n",
	    sftp_cmd_open
    },
    {
	"put", TRUE, "upload a file from your local machine to the server",
	    " <filename> [ <remote-filename> ]\n"
	    "  Uploads a file to the server and stores it there under\n"
	    "  the same name, or under a different one if you supply the\n"
	    "  argument <remote-filename>.\n",
	    sftp_cmd_put
    },
    {
	"pwd", TRUE, "print your remote working directory",
	    "\n"
	    "  Print the current remote working directory for your SFTP session.\n",
	    sftp_cmd_pwd
    },
    {
	"quit", TRUE, "bye", NULL,
	    sftp_cmd_quit
    },
    {
	"reget", TRUE, "continue downloading a file",
	    " <filename> [ <local-filename> ]\n"
	    "  Works exactly like the \"get\" command, but the local file\n"
	    "  must already exist. The download will begin at the end of the\n"
	    "  file. This is for resuming a download that was interrupted.\n",
	    sftp_cmd_reget
    },
    {
	"ren", TRUE, "mv", NULL,
	    sftp_cmd_mv
    },
    {
	"rename", FALSE, "mv", NULL,
	    sftp_cmd_mv
    },
    {
	"reput", TRUE, "continue uploading a file",
	    " <filename> [ <remote-filename> ]\n"
	    "  Works exactly like the \"put\" command, but the remote file\n"
	    "  must already exist. The upload will begin at the end of the\n"
	    "  file. This is for resuming an upload that was interrupted.\n",
	    sftp_cmd_reput
    },
    {
	"rm", TRUE, "del", NULL,
	    sftp_cmd_rm
    },
    {
	"rmdir", TRUE, "remove a directory on the remote server",
	    " <directory-name>\n"
	    "  Removes the directory with the given name on the server.\n"
	    "  The directory will not be removed unless it is empty.\n",
	    sftp_cmd_rmdir
    }
};

const struct sftp_cmd_lookup *lookup_command(char *name)
{
    int i, j, k, cmp;

    i = -1;
    j = sizeof(sftp_lookup) / sizeof(*sftp_lookup);
    while (j - i > 1) {
	k = (j + i) / 2;
	cmp = strcmp(name, sftp_lookup[k].name);
	if (cmp < 0)
	    j = k;
	else if (cmp > 0)
	    i = k;
	else {
	    return &sftp_lookup[k];
	}
    }
    return NULL;
}

static int sftp_cmd_help(struct sftp_command *cmd)
{
    int i;
    if (cmd->nwords == 1) {
	/*
	 * Give short help on each command.
	 */
	int maxlen;
	maxlen = 0;
	for (i = 0; i < sizeof(sftp_lookup) / sizeof(*sftp_lookup); i++) {
	    int len;
	    if (!sftp_lookup[i].listed)
		continue;
	    len = strlen(sftp_lookup[i].name);
	    if (maxlen < len)
		maxlen = len;
	}
	for (i = 0; i < sizeof(sftp_lookup) / sizeof(*sftp_lookup); i++) {
	    const struct sftp_cmd_lookup *lookup;
	    if (!sftp_lookup[i].listed)
		continue;
	    lookup = &sftp_lookup[i];
	    printf("%-*s", maxlen+2, lookup->name);
	    if (lookup->longhelp == NULL)
		lookup = lookup_command(lookup->shorthelp);
	    printf("%s\n", lookup->shorthelp);
	}
    } else {
	/*
	 * Give long help on specific commands.
	 */
	for (i = 1; i < cmd->nwords; i++) {
	    const struct sftp_cmd_lookup *lookup;
	    lookup = lookup_command(cmd->words[i]);
	    if (!lookup) {
		printf("help: %s: command not found\n", cmd->words[i]);
	    } else {
		printf("%s", lookup->name);
		if (lookup->longhelp == NULL)
		    lookup = lookup_command(lookup->shorthelp);
		printf("%s", lookup->longhelp);
	    }
	}
    }
    return 1;
}

/* ----------------------------------------------------------------------
 * Command line reading and parsing.
 */
struct sftp_command *sftp_getcmd(FILE *fp, int mode, int modeflags)
{
    char *line;
    int linelen, linesize;
    struct sftp_command *cmd;
    char *p, *q, *r;
    int quoting;

    if ((mode == 0) || (modeflags & 1)) {
	printf("psftp> ");
    }
    fflush(stdout);

    cmd = snew(struct sftp_command);
    cmd->words = NULL;
    cmd->nwords = 0;
    cmd->wordssize = 0;

    line = NULL;
    linesize = linelen = 0;
    while (1) {
	int len;
	char *ret;

	linesize += 512;
	line = sresize(line, linesize, char);
	ret = fgets(line + linelen, linesize - linelen, fp);

	if (!ret || (linelen == 0 && line[0] == '\0')) {
	    cmd->obey = sftp_cmd_quit;
	    if ((mode == 0) || (modeflags & 1))
		printf("quit\n");
	    return cmd;		       /* eof */
	}
	len = linelen + strlen(line + linelen);
	linelen += len;
	if (line[linelen - 1] == '\n') {
	    linelen--;
	    line[linelen] = '\0';
	    break;
	}
    }
    if (modeflags & 1) {
	printf("%s\n", line);
    }

    p = line;
    while (*p && (*p == ' ' || *p == '\t'))
	p++;

    if (*p == '!') {
	/*
	 * Special case: the ! command. This is always parsed as
	 * exactly two words: one containing the !, and the second
	 * containing everything else on the line.
	 */
	cmd->nwords = cmd->wordssize = 2;
	cmd->words = sresize(cmd->words, cmd->wordssize, char *);
	cmd->words[0] = "!";
	cmd->words[1] = p+1;
    } else {

	/*
	 * Parse the command line into words. The syntax is:
	 *  - double quotes are removed, but cause spaces within to be
	 *    treated as non-separating.
	 *  - a double-doublequote pair is a literal double quote, inside
	 *    _or_ outside quotes. Like this:
	 *
	 *      firstword "second word" "this has ""quotes"" in" and""this""
	 *
	 * becomes
	 *
	 *      >firstword<
	 *      >second word<
	 *      >this has "quotes" in<
	 *      >and"this"<
	 */
	while (*p) {
	    /* skip whitespace */
	    while (*p && (*p == ' ' || *p == '\t'))
		p++;
	    /* mark start of word */
	    q = r = p;		       /* q sits at start, r writes word */
	    quoting = 0;
	    while (*p) {
		if (!quoting && (*p == ' ' || *p == '\t'))
		    break;		       /* reached end of word */
		else if (*p == '"' && p[1] == '"')
		    p += 2, *r++ = '"';    /* a literal quote */
		else if (*p == '"')
		    p++, quoting = !quoting;
		else
		    *r++ = *p++;
	    }
	    if (*p)
		p++;		       /* skip over the whitespace */
	    *r = '\0';
	    if (cmd->nwords >= cmd->wordssize) {
		cmd->wordssize = cmd->nwords + 16;
		cmd->words = sresize(cmd->words, cmd->wordssize, char *);
	    }
	    cmd->words[cmd->nwords++] = q;
	}
    }

    /*
     * Now parse the first word and assign a function.
     */

    if (cmd->nwords == 0)
	cmd->obey = sftp_cmd_null;
    else {
	const struct sftp_cmd_lookup *lookup;
	lookup = lookup_command(cmd->words[0]);
	if (!lookup)
	    cmd->obey = sftp_cmd_unknown;
	else
	    cmd->obey = lookup->obey;
    }

    return cmd;
}

static int do_sftp_init(void)
{
    struct sftp_packet *pktin;
    struct sftp_request *req, *rreq;

    /*
     * Do protocol initialisation. 
     */
    if (!fxp_init()) {
	fprintf(stderr,
		"Fatal: unable to initialise SFTP: %s\n", fxp_error());
	return 1;		       /* failure */
    }

    /*
     * Find out where our home directory is.
     */
    sftp_register(req = fxp_realpath_send("."));
    rreq = sftp_find_request(pktin = sftp_recv());
    assert(rreq == req);
    homedir = fxp_realpath_recv(pktin, rreq);

    if (!homedir) {
	fprintf(stderr,
		"Warning: failed to resolve home directory: %s\n",
		fxp_error());
	homedir = dupstr(".");
    } else {
	printf("Remote working directory is %s\n", homedir);
    }
    pwd = dupstr(homedir);
    return 0;
}

void do_sftp(int mode, int modeflags, char *batchfile)
{
    FILE *fp;
    int ret;

    /*
     * Batch mode?
     */
    if (mode == 0) {

        /* ------------------------------------------------------------------
         * Now we're ready to do Real Stuff.
         */
        while (1) {
	    struct sftp_command *cmd;
	    cmd = sftp_getcmd(stdin, 0, 0);
	    if (!cmd)
		break;
	    if (cmd->obey(cmd) < 0)
		break;
	}
    } else {
        fp = fopen(batchfile, "r");
        if (!fp) {
	    printf("Fatal: unable to open %s\n", batchfile);
	    return;
        }
        while (1) {
	    struct sftp_command *cmd;
	    cmd = sftp_getcmd(fp, mode, modeflags);
	    if (!cmd)
		break;
	    ret = cmd->obey(cmd);
	    if (ret < 0)
		break;
	    if (ret == 0) {
		if (!(modeflags & 2))
		    break;
	    }
        }
	fclose(fp);

    }
}

/* ----------------------------------------------------------------------
 * Dirty bits: integration with PuTTY.
 */

static int verbose = 0;

/*
 *  Print an error message and perform a fatal exit.
 */
void fatalbox(char *fmt, ...)
{
    char *str, *str2;
    va_list ap;
    va_start(ap, fmt);
    str = dupvprintf(fmt, ap);
    str2 = dupcat("Fatal: ", str, "\n", NULL);
    sfree(str);
    va_end(ap);
    fputs(str2, stderr);
    sfree(str2);

    cleanup_exit(1);
}
void modalfatalbox(char *fmt, ...)
{
    char *str, *str2;
    va_list ap;
    va_start(ap, fmt);
    str = dupvprintf(fmt, ap);
    str2 = dupcat("Fatal: ", str, "\n", NULL);
    sfree(str);
    va_end(ap);
    fputs(str2, stderr);
    sfree(str2);

    cleanup_exit(1);
}
void connection_fatal(void *frontend, char *fmt, ...)
{
    char *str, *str2;
    va_list ap;
    va_start(ap, fmt);
    str = dupvprintf(fmt, ap);
    str2 = dupcat("Fatal: ", str, "\n", NULL);
    sfree(str);
    va_end(ap);
    fputs(str2, stderr);
    sfree(str2);

    cleanup_exit(1);
}

void ldisc_send(void *handle, char *buf, int len, int interactive)
{
    /*
     * This is only here because of the calls to ldisc_send(NULL,
     * 0) in ssh.c. Nothing in PSFTP actually needs to use the
     * ldisc as an ldisc. So if we get called with any real data, I
     * want to know about it.
     */
    assert(len == 0);
}

/*
 * In psftp, all agent requests should be synchronous, so this is a
 * never-called stub.
 */
void agent_schedule_callback(void (*callback)(void *, void *, int),
			     void *callback_ctx, void *data, int len)
{
    assert(!"We shouldn't be here");
}

/*
 * Receive a block of data from the SSH link. Block until all data
 * is available.
 *
 * To do this, we repeatedly call the SSH protocol module, with our
 * own trap in from_backend() to catch the data that comes back. We
 * do this until we have enough data.
 */

static unsigned char *outptr;	       /* where to put the data */
static unsigned outlen;		       /* how much data required */
static unsigned char *pending = NULL;  /* any spare data */
static unsigned pendlen = 0, pendsize = 0;	/* length and phys. size of buffer */
int from_backend(void *frontend, int is_stderr, const char *data, int datalen)
{
    unsigned char *p = (unsigned char *) data;
    unsigned len = (unsigned) datalen;

    assert(len > 0);

    /*
     * stderr data is just spouted to local stderr and otherwise
     * ignored.
     */
    if (is_stderr) {
	fwrite(data, 1, len, stderr);
	return 0;
    }

    /*
     * If this is before the real session begins, just return.
     */
    if (!outptr)
	return 0;

    if (outlen > 0) {
	unsigned used = outlen;
	if (used > len)
	    used = len;
	memcpy(outptr, p, used);
	outptr += used;
	outlen -= used;
	p += used;
	len -= used;
    }

    if (len > 0) {
	if (pendsize < pendlen + len) {
	    pendsize = pendlen + len + 4096;
	    pending = sresize(pending, pendsize, unsigned char);
	}
	memcpy(pending + pendlen, p, len);
	pendlen += len;
    }

    return 0;
}
int sftp_recvdata(char *buf, int len)
{
    outptr = (unsigned char *) buf;
    outlen = len;

    /*
     * See if the pending-input block contains some of what we
     * need.
     */
    if (pendlen > 0) {
	unsigned pendused = pendlen;
	if (pendused > outlen)
	    pendused = outlen;
	memcpy(outptr, pending, pendused);
	memmove(pending, pending + pendused, pendlen - pendused);
	outptr += pendused;
	outlen -= pendused;
	pendlen -= pendused;
	if (pendlen == 0) {
	    pendsize = 0;
	    sfree(pending);
	    pending = NULL;
	}
	if (outlen == 0)
	    return 1;
    }

    while (outlen > 0) {
	if (ssh_sftp_loop_iteration() < 0)
	    return 0;		       /* doom */
    }

    return 1;
}
int sftp_senddata(char *buf, int len)
{
    back->send(backhandle, (unsigned char *) buf, len);
    return 1;
}

/*
 *  Short description of parameters.
 */
static void usage(void)
{
    printf("PuTTY Secure File Transfer (SFTP) client\n");
    printf("%s\n", ver);
    printf("Usage: psftp [options] user@host\n");
    printf("Options:\n");
    printf("  -b file   use specified batchfile\n");
    printf("  -bc       output batchfile commands\n");
    printf("  -be       don't stop batchfile processing if errors\n");
    printf("  -v        show verbose messages\n");
    printf("  -load sessname  Load settings from saved session\n");
    printf("  -l user   connect with specified username\n");
    printf("  -P port   connect to specified port\n");
    printf("  -pw passw login with specified password\n");
    printf("  -1 -2     force use of particular SSH protocol version\n");
    printf("  -C        enable compression\n");
    printf("  -i key    private key file for authentication\n");
    printf("  -batch    disable all interactive prompts\n");
    cleanup_exit(1);
}

/*
 * Connect to a host.
 */
static int psftp_connect(char *userhost, char *user, int portnumber)
{
    char *host, *realhost;
    const char *err;
    void *logctx;

    /* Separate host and username */
    host = userhost;
    host = strrchr(host, '@');
    if (host == NULL) {
	host = userhost;
    } else {
	*host++ = '\0';
	if (user) {
	    printf("psftp: multiple usernames specified; using \"%s\"\n",
		   user);
	} else
	    user = userhost;
    }

    /* Try to load settings for this host */
    do_defaults(host, &cfg);
    if (cfg.host[0] == '\0') {
	/* No settings for this host; use defaults */
	do_defaults(NULL, &cfg);
	strncpy(cfg.host, host, sizeof(cfg.host) - 1);
	cfg.host[sizeof(cfg.host) - 1] = '\0';
    }

    /*
     * Force use of SSH. (If they got the protocol wrong we assume the
     * port is useless too.)
     */
    if (cfg.protocol != PROT_SSH) {
        cfg.protocol = PROT_SSH;
        cfg.port = 22;
    }

    /*
     * Enact command-line overrides.
     */
    cmdline_run_saved(&cfg);

    /*
     * Trim leading whitespace off the hostname if it's there.
     */
    {
	int space = strspn(cfg.host, " \t");
	memmove(cfg.host, cfg.host+space, 1+strlen(cfg.host)-space);
    }

    /* See if host is of the form user@host */
    if (cfg.host[0] != '\0') {
	char *atsign = strchr(cfg.host, '@');
	/* Make sure we're not overflowing the user field */
	if (atsign) {
	    if (atsign - cfg.host < sizeof cfg.username) {
		strncpy(cfg.username, cfg.host, atsign - cfg.host);
		cfg.username[atsign - cfg.host] = '\0';
	    }
	    memmove(cfg.host, atsign + 1, 1 + strlen(atsign + 1));
	}
    }

    /*
     * Trim a colon suffix off the hostname if it's there.
     */
    cfg.host[strcspn(cfg.host, ":")] = '\0';

    /*
     * Remove any remaining whitespace from the hostname.
     */
    {
	int p1 = 0, p2 = 0;
	while (cfg.host[p2] != '\0') {
	    if (cfg.host[p2] != ' ' && cfg.host[p2] != '\t') {
		cfg.host[p1] = cfg.host[p2];
		p1++;
	    }
	    p2++;
	}
	cfg.host[p1] = '\0';
    }

    /* Set username */
    if (user != NULL && user[0] != '\0') {
	strncpy(cfg.username, user, sizeof(cfg.username) - 1);
	cfg.username[sizeof(cfg.username) - 1] = '\0';
    }
    if (!cfg.username[0]) {
	printf("login as: ");
	fflush(stdout);
	if (!fgets(cfg.username, sizeof(cfg.username), stdin)) {
	    fprintf(stderr, "psftp: aborting\n");
	    cleanup_exit(1);
	} else {
	    int len = strlen(cfg.username);
	    if (cfg.username[len - 1] == '\n')
		cfg.username[len - 1] = '\0';
	}
    }

    if (portnumber)
	cfg.port = portnumber;

    /* SFTP uses SSH2 by default always */
    cfg.sshprot = 2;

    /*
     * Disable scary things which shouldn't be enabled for simple
     * things like SCP and SFTP: agent forwarding, port forwarding,
     * X forwarding.
     */
    cfg.x11_forward = 0;
    cfg.agentfwd = 0;
    cfg.portfwd[0] = cfg.portfwd[1] = '\0';

    /* Set up subsystem name. */
    strcpy(cfg.remote_cmd, "sftp");
    cfg.ssh_subsys = TRUE;
    cfg.nopty = TRUE;

    /*
     * Set up fallback option, for SSH1 servers or servers with the
     * sftp subsystem not enabled but the server binary installed
     * in the usual place. We only support fallback on Unix
     * systems, and we use a kludgy piece of shellery which should
     * try to find sftp-server in various places (the obvious
     * systemwide spots /usr/lib and /usr/local/lib, and then the
     * user's PATH) and finally give up.
     * 
     *   test -x /usr/lib/sftp-server && exec /usr/lib/sftp-server
     *   test -x /usr/local/lib/sftp-server && exec /usr/local/lib/sftp-server
     *   exec sftp-server
     * 
     * the idea being that this will attempt to use either of the
     * obvious pathnames and then give up, and when it does give up
     * it will print the preferred pathname in the error messages.
     */
    cfg.remote_cmd_ptr2 =
	"test -x /usr/lib/sftp-server && exec /usr/lib/sftp-server\n"
	"test -x /usr/local/lib/sftp-server && exec /usr/local/lib/sftp-server\n"
	"exec sftp-server";
    cfg.ssh_subsys2 = FALSE;

    back = &ssh_backend;

    err = back->init(NULL, &backhandle, &cfg, cfg.host, cfg.port, &realhost,0);
    if (err != NULL) {
	fprintf(stderr, "ssh_init: %s\n", err);
	return 1;
    }
    logctx = log_init(NULL, &cfg);
    back->provide_logctx(backhandle, logctx);
    console_provide_logctx(logctx);
    while (!back->sendok(backhandle)) {
	if (ssh_sftp_loop_iteration() < 0) {
	    fprintf(stderr, "ssh_init: error during SSH connection setup\n");
	    return 1;
	}
    }
    if (verbose && realhost != NULL)
	printf("Connected to %s\n", realhost);
    return 0;
}

void cmdline_error(char *p, ...)
{
    va_list ap;
    fprintf(stderr, "psftp: ");
    va_start(ap, p);
    vfprintf(stderr, p, ap);
    va_end(ap);
    fprintf(stderr, "\n       try typing \"psftp -h\" for help\n");
    exit(1);
}

/*
 * Main program. Parse arguments etc.
 */
int psftp_main(int argc, char *argv[])
{
    int i;
    int portnumber = 0;
    char *userhost, *user;
    int mode = 0;
    int modeflags = 0;
    char *batchfile = NULL;
    int errors = 0;

    flags = FLAG_STDERR | FLAG_INTERACTIVE
#ifdef FLAG_SYNCAGENT
	| FLAG_SYNCAGENT
#endif
	;
    cmdline_tooltype = TOOLTYPE_FILETRANSFER;
    ssh_get_line = &console_get_line;
    sk_init();

    userhost = user = NULL;

    errors = 0;
    for (i = 1; i < argc; i++) {
	int ret;
	if (argv[i][0] != '-') {
            if (userhost)
                usage();
            else
                userhost = dupstr(argv[i]);
	    continue;
	}
	ret = cmdline_process_param(argv[i], i+1<argc?argv[i+1]:NULL, 1, &cfg);
	if (ret == -2) {
	    cmdline_error("option \"%s\" requires an argument", argv[i]);
	} else if (ret == 2) {
	    i++;	       /* skip next argument */
	} else if (ret == 1) {
	    /* We have our own verbosity in addition to `flags'. */
	    if (flags & FLAG_VERBOSE)
		verbose = 1;
	} else if (strcmp(argv[i], "-h") == 0 ||
		   strcmp(argv[i], "-?") == 0) {
	    usage();
	} else if (strcmp(argv[i], "-batch") == 0) {
	    console_batch_mode = 1;
	} else if (strcmp(argv[i], "-b") == 0 && i + 1 < argc) {
	    mode = 1;
	    batchfile = argv[++i];
	} else if (strcmp(argv[i], "-bc") == 0) {
	    modeflags = modeflags | 1;
	} else if (strcmp(argv[i], "-be") == 0) {
	    modeflags = modeflags | 2;
	} else if (strcmp(argv[i], "--") == 0) {
	    i++;
	    break;
	} else {
	    cmdline_error("unknown option \"%s\"", argv[i]);
	}
    }
    argc -= i;
    argv += i;
    back = NULL;

    /*
     * If a user@host string has already been provided, connect to
     * it now.
     */
    if (userhost) {
	if (psftp_connect(userhost, user, portnumber))
	    return 1;
	if (do_sftp_init())
	    return 1;
    } else {
	printf("psftp: no hostname specified; use \"open host.name\""
	    " to connect\n");
    }

    do_sftp(mode, modeflags, batchfile);

    if (back != NULL && back->socket(backhandle) != NULL) {
	char ch;
	back->special(backhandle, TS_EOF);
	sftp_recvdata(&ch, 1);
    }
    random_save_seed();

    return 0;
}
