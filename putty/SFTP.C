/*
 * sftp.c: SFTP generic client code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "misc.h"
#include "int64.h"
#include "tree234.h"
#include "sftp.h"

#define GET_32BIT(cp) \
    (((unsigned long)(unsigned char)(cp)[0] << 24) | \
    ((unsigned long)(unsigned char)(cp)[1] << 16) | \
    ((unsigned long)(unsigned char)(cp)[2] << 8) | \
    ((unsigned long)(unsigned char)(cp)[3]))

#define PUT_32BIT(cp, value) { \
    (cp)[0] = (unsigned char)((value) >> 24); \
    (cp)[1] = (unsigned char)((value) >> 16); \
    (cp)[2] = (unsigned char)((value) >> 8); \
    (cp)[3] = (unsigned char)(value); }

struct sftp_packet {
    char *data;
    int length, maxlen;
    int savedpos;
    int type;
};

static const char *fxp_error_message;
static int fxp_errtype;

static void fxp_internal_error(char *msg);

/* ----------------------------------------------------------------------
 * SFTP packet construction functions.
 */
static void sftp_pkt_ensure(struct sftp_packet *pkt, int length)
{
    if (pkt->maxlen < length) {
	pkt->maxlen = length + 256;
	pkt->data = sresize(pkt->data, pkt->maxlen, char);
    }
}
static void sftp_pkt_adddata(struct sftp_packet *pkt, void *data, int len)
{
    pkt->length += len;
    sftp_pkt_ensure(pkt, pkt->length);
    memcpy(pkt->data + pkt->length - len, data, len);
}
static void sftp_pkt_addbyte(struct sftp_packet *pkt, unsigned char byte)
{
    sftp_pkt_adddata(pkt, &byte, 1);
}
static struct sftp_packet *sftp_pkt_init(int pkt_type)
{
    struct sftp_packet *pkt;
    pkt = snew(struct sftp_packet);
    pkt->data = NULL;
    pkt->savedpos = -1;
    pkt->length = 0;
    pkt->maxlen = 0;
    sftp_pkt_addbyte(pkt, (unsigned char) pkt_type);
    return pkt;
}
/*
static void sftp_pkt_addbool(struct sftp_packet *pkt, unsigned char value)
{
    sftp_pkt_adddata(pkt, &value, 1);
}
*/
static void sftp_pkt_adduint32(struct sftp_packet *pkt,
			       unsigned long value)
{
    unsigned char x[4];
    PUT_32BIT(x, value);
    sftp_pkt_adddata(pkt, x, 4);
}
static void sftp_pkt_adduint64(struct sftp_packet *pkt, uint64 value)
{
    unsigned char x[8];
    PUT_32BIT(x, value.hi);
    PUT_32BIT(x + 4, value.lo);
    sftp_pkt_adddata(pkt, x, 8);
}
static void sftp_pkt_addstring_start(struct sftp_packet *pkt)
{
    sftp_pkt_adduint32(pkt, 0);
    pkt->savedpos = pkt->length;
}
static void sftp_pkt_addstring_str(struct sftp_packet *pkt, char *data)
{
    sftp_pkt_adddata(pkt, data, strlen(data));
    PUT_32BIT(pkt->data + pkt->savedpos - 4, pkt->length - pkt->savedpos);
}
static void sftp_pkt_addstring_data(struct sftp_packet *pkt,
				    char *data, int len)
{
    sftp_pkt_adddata(pkt, data, len);
    PUT_32BIT(pkt->data + pkt->savedpos - 4, pkt->length - pkt->savedpos);
}
static void sftp_pkt_addstring(struct sftp_packet *pkt, char *data)
{
    sftp_pkt_addstring_start(pkt);
    sftp_pkt_addstring_str(pkt, data);
}
static void sftp_pkt_addattrs(struct sftp_packet *pkt, struct fxp_attrs attrs)
{
    sftp_pkt_adduint32(pkt, attrs.flags);
    if (attrs.flags & SSH_FILEXFER_ATTR_SIZE) {
	sftp_pkt_adduint32(pkt, attrs.size.hi);
	sftp_pkt_adduint32(pkt, attrs.size.lo);
    }
    if (attrs.flags & SSH_FILEXFER_ATTR_UIDGID) {
	sftp_pkt_adduint32(pkt, attrs.uid);
	sftp_pkt_adduint32(pkt, attrs.gid);
    }
    if (attrs.flags & SSH_FILEXFER_ATTR_PERMISSIONS) {
	sftp_pkt_adduint32(pkt, attrs.permissions);
    }
    if (attrs.flags & SSH_FILEXFER_ATTR_ACMODTIME) {
	sftp_pkt_adduint32(pkt, attrs.atime);
	sftp_pkt_adduint32(pkt, attrs.mtime);
    }
    if (attrs.flags & SSH_FILEXFER_ATTR_EXTENDED) {
	/*
	 * We currently don't support sending any extended
	 * attributes.
	 */
    }
}

/* ----------------------------------------------------------------------
 * SFTP packet decode functions.
 */

static unsigned char sftp_pkt_getbyte(struct sftp_packet *pkt)
{
    unsigned char value;
    if (pkt->length - pkt->savedpos < 1)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = (unsigned char) pkt->data[pkt->savedpos];
    pkt->savedpos++;
    return value;
}
static unsigned long sftp_pkt_getuint32(struct sftp_packet *pkt)
{
    unsigned long value;
    if (pkt->length - pkt->savedpos < 4)
	return 0;		       /* arrgh, no way to decline (FIXME?) */
    value = GET_32BIT(pkt->data + pkt->savedpos);
    pkt->savedpos += 4;
    return value;
}
static void sftp_pkt_getstring(struct sftp_packet *pkt,
			       char **p, int *length)
{
    *p = NULL;
    if (pkt->length - pkt->savedpos < 4)
	return;
    *length = GET_32BIT(pkt->data + pkt->savedpos);
    pkt->savedpos += 4;
    if (pkt->length - pkt->savedpos < *length)
	return;
    *p = pkt->data + pkt->savedpos;
    pkt->savedpos += *length;
}
static struct fxp_attrs sftp_pkt_getattrs(struct sftp_packet *pkt)
{
    struct fxp_attrs ret;
    ret.flags = sftp_pkt_getuint32(pkt);
    if (ret.flags & SSH_FILEXFER_ATTR_SIZE) {
	unsigned long hi, lo;
	hi = sftp_pkt_getuint32(pkt);
	lo = sftp_pkt_getuint32(pkt);
	ret.size = uint64_make(hi, lo);
    }
    if (ret.flags & SSH_FILEXFER_ATTR_UIDGID) {
	ret.uid = sftp_pkt_getuint32(pkt);
	ret.gid = sftp_pkt_getuint32(pkt);
    }
    if (ret.flags & SSH_FILEXFER_ATTR_PERMISSIONS) {
	ret.permissions = sftp_pkt_getuint32(pkt);
    }
    if (ret.flags & SSH_FILEXFER_ATTR_ACMODTIME) {
	ret.atime = sftp_pkt_getuint32(pkt);
	ret.mtime = sftp_pkt_getuint32(pkt);
    }
    if (ret.flags & SSH_FILEXFER_ATTR_EXTENDED) {
	int count;
	count = sftp_pkt_getuint32(pkt);
	while (count--) {
	    char *str;
	    int len;
	    /*
	     * We should try to analyse these, if we ever find one
	     * we recognise.
	     */
	    sftp_pkt_getstring(pkt, &str, &len);
	    sftp_pkt_getstring(pkt, &str, &len);
	}
    }
    return ret;
}
static void sftp_pkt_free(struct sftp_packet *pkt)
{
    if (pkt->data)
	sfree(pkt->data);
    sfree(pkt);
}

/* ----------------------------------------------------------------------
 * Send and receive packet functions.
 */
int sftp_send(struct sftp_packet *pkt)
{
    int ret;
    char x[4];
    PUT_32BIT(x, pkt->length);
    ret = (sftp_senddata(x, 4) && sftp_senddata(pkt->data, pkt->length));
    sftp_pkt_free(pkt);
    return ret;
}
struct sftp_packet *sftp_recv(void)
{
    struct sftp_packet *pkt;
    char x[4];

    if (!sftp_recvdata(x, 4))
	return NULL;

    pkt = snew(struct sftp_packet);
    pkt->savedpos = 0;
    pkt->length = pkt->maxlen = GET_32BIT(x);
    pkt->data = snewn(pkt->length, char);

    if (!sftp_recvdata(pkt->data, pkt->length)) {
	sftp_pkt_free(pkt);
	return NULL;
    }

    pkt->type = sftp_pkt_getbyte(pkt);

    return pkt;
}

/* ----------------------------------------------------------------------
 * Request ID allocation and temporary dispatch routines.
 */

#define REQUEST_ID_OFFSET 256

struct sftp_request {
    unsigned id;
    int registered;
};

static int sftp_reqcmp(void *av, void *bv)
{
    struct sftp_request *a = (struct sftp_request *)av;
    struct sftp_request *b = (struct sftp_request *)bv;
    if (a->id < b->id)
	return -1;
    if (a->id > b->id)
	return +1;
    return 0;
}
static int sftp_reqfind(void *av, void *bv)
{
    unsigned *a = (unsigned *) av;
    struct sftp_request *b = (struct sftp_request *)bv;
    if (*a < b->id)
	return -1;
    if (*a > b->id)
	return +1;
    return 0;
}

static tree234 *sftp_requests;

static struct sftp_request *sftp_alloc_request(void)
{
    unsigned low, high, mid;
    int tsize;
    struct sftp_request *r;

    if (sftp_requests == NULL)
	sftp_requests = newtree234(sftp_reqcmp);

    /*
     * First-fit allocation of request IDs: always pick the lowest
     * unused one. To do this, binary-search using the counted
     * B-tree to find the largest ID which is in a contiguous
     * sequence from the beginning. (Precisely everything in that
     * sequence must have ID equal to its tree index plus
     * REQUEST_ID_OFFSET.)
     */
    tsize = count234(sftp_requests);

    low = -1;
    high = tsize;
    while (high - low > 1) {
	mid = (high + low) / 2;
	r = index234(sftp_requests, mid);
	if (r->id == mid + REQUEST_ID_OFFSET)
	    low = mid;		       /* this one is fine */
	else
	    high = mid;		       /* this one is past it */
    }
    /*
     * Now low points to either -1, or the tree index of the
     * largest ID in the initial sequence.
     */
    {
	unsigned i = low + 1 + REQUEST_ID_OFFSET;
	assert(NULL == find234(sftp_requests, &i, sftp_reqfind));
    }

    /*
     * So the request ID we need to create is
     * low + 1 + REQUEST_ID_OFFSET.
     */
    r = snew(struct sftp_request);
    r->id = low + 1 + REQUEST_ID_OFFSET;
    r->registered = 0;
    add234(sftp_requests, r);
    return r;
}

void sftp_register(struct sftp_request *req)
{
    req->registered = 1;
}

struct sftp_request *sftp_find_request(struct sftp_packet *pktin)
{
    unsigned long id;
    struct sftp_request *req;

    if (!pktin) {
	fxp_internal_error("did not receive a valid SFTP packet\n");
	return NULL;
    }

    id = sftp_pkt_getuint32(pktin);
    req = find234(sftp_requests, &id, sftp_reqfind);

    if (!req || !req->registered) {
	fxp_internal_error("request ID mismatch\n");
        sftp_pkt_free(pktin);
	return NULL;
    }

    del234(sftp_requests, req);

    return req;
}

/* ----------------------------------------------------------------------
 * String handling routines.
 */

static char *mkstr(char *s, int len)
{
    char *p = snewn(len + 1, char);
    memcpy(p, s, len);
    p[len] = '\0';
    return p;
}

/* ----------------------------------------------------------------------
 * SFTP primitives.
 */

/*
 * Deal with (and free) an FXP_STATUS packet. Return 1 if
 * SSH_FX_OK, 0 if SSH_FX_EOF, and -1 for anything else (error).
 * Also place the status into fxp_errtype.
 */
static int fxp_got_status(struct sftp_packet *pktin)
{
    static const char *const messages[] = {
	/* SSH_FX_OK. The only time we will display a _message_ for this
	 * is if we were expecting something other than FXP_STATUS on
	 * success, so this is actually an error message! */
	"unexpected OK response",
	"end of file",
	"no such file or directory",
	"permission denied",
	"failure",
	"bad message",
	"no connection",
	"connection lost",
	"operation unsupported",
    };

    if (pktin->type != SSH_FXP_STATUS) {
	fxp_error_message = "expected FXP_STATUS packet";
	fxp_errtype = -1;
    } else {
	fxp_errtype = sftp_pkt_getuint32(pktin);
	if (fxp_errtype < 0 ||
	    fxp_errtype >= sizeof(messages) / sizeof(*messages))
		fxp_error_message = "unknown error code";
	else
	    fxp_error_message = messages[fxp_errtype];
    }

    if (fxp_errtype == SSH_FX_OK)
	return 1;
    else if (fxp_errtype == SSH_FX_EOF)
	return 0;
    else
	return -1;
}

static void fxp_internal_error(char *msg)
{
    fxp_error_message = msg;
    fxp_errtype = -1;
}

const char *fxp_error(void)
{
    return fxp_error_message;
}

int fxp_error_type(void)
{
    return fxp_errtype;
}

/*
 * Perform exchange of init/version packets. Return 0 on failure.
 */
int fxp_init(void)
{
    struct sftp_packet *pktout, *pktin;
    int remotever;

    pktout = sftp_pkt_init(SSH_FXP_INIT);
    sftp_pkt_adduint32(pktout, SFTP_PROTO_VERSION);
    sftp_send(pktout);

    pktin = sftp_recv();
    if (!pktin) {
	fxp_internal_error("could not connect");
	return 0;
    }
    if (pktin->type != SSH_FXP_VERSION) {
	fxp_internal_error("did not receive FXP_VERSION");
        sftp_pkt_free(pktin);
	return 0;
    }
    remotever = sftp_pkt_getuint32(pktin);
    if (remotever > SFTP_PROTO_VERSION) {
	fxp_internal_error
	    ("remote protocol is more advanced than we support");
        sftp_pkt_free(pktin);
	return 0;
    }
    /*
     * In principle, this packet might also contain extension-
     * string pairs. We should work through them and look for any
     * we recognise. In practice we don't currently do so because
     * we know we don't recognise _any_.
     */
    sftp_pkt_free(pktin);

    return 1;
}

/*
 * Canonify a pathname.
 */
struct sftp_request *fxp_realpath_send(char *path)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_REALPATH);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_str(pktout, path);
    sftp_send(pktout);

    return req;
}

char *fxp_realpath_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    sfree(req);

    if (pktin->type == SSH_FXP_NAME) {
	int count;
	char *path;
	int len;

	count = sftp_pkt_getuint32(pktin);
	if (count != 1) {
	    fxp_internal_error("REALPATH returned name count != 1\n");
            sftp_pkt_free(pktin);
	    return NULL;
	}
	sftp_pkt_getstring(pktin, &path, &len);
	if (!path) {
	    fxp_internal_error("REALPATH returned malformed FXP_NAME\n");
            sftp_pkt_free(pktin);
	    return NULL;
	}
	path = mkstr(path, len);
	sftp_pkt_free(pktin);
	return path;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return NULL;
    }
}

/*
 * Open a file.
 */
struct sftp_request *fxp_open_send(char *path, int type)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_OPEN);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, path);
    sftp_pkt_adduint32(pktout, type);
    sftp_pkt_adduint32(pktout, 0);     /* (FIXME) empty ATTRS structure */
    sftp_send(pktout);

    return req;
}

struct fxp_handle *fxp_open_recv(struct sftp_packet *pktin,
				 struct sftp_request *req)
{
    sfree(req);

    if (pktin->type == SSH_FXP_HANDLE) {
	char *hstring;
	struct fxp_handle *handle;
	int len;

	sftp_pkt_getstring(pktin, &hstring, &len);
	if (!hstring) {
	    fxp_internal_error("OPEN returned malformed FXP_HANDLE\n");
            sftp_pkt_free(pktin);
	    return NULL;
	}
	handle = snew(struct fxp_handle);
	handle->hstring = mkstr(hstring, len);
	handle->hlen = len;
	sftp_pkt_free(pktin);
	return handle;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return NULL;
    }
}

/*
 * Open a directory.
 */
struct sftp_request *fxp_opendir_send(char *path)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_OPENDIR);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, path);
    sftp_send(pktout);

    return req;
}

struct fxp_handle *fxp_opendir_recv(struct sftp_packet *pktin,
				    struct sftp_request *req)
{
    sfree(req);
    if (pktin->type == SSH_FXP_HANDLE) {
	char *hstring;
	struct fxp_handle *handle;
	int len;

	sftp_pkt_getstring(pktin, &hstring, &len);
	if (!hstring) {
	    fxp_internal_error("OPENDIR returned malformed FXP_HANDLE\n");
            sftp_pkt_free(pktin);
	    return NULL;
	}
	handle = snew(struct fxp_handle);
	handle->hstring = mkstr(hstring, len);
	handle->hlen = len;
	sftp_pkt_free(pktin);
	return handle;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return NULL;
    }
}

/*
 * Close a file/dir.
 */
struct sftp_request *fxp_close_send(struct fxp_handle *handle)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_CLOSE);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_send(pktout);

    sfree(handle->hstring);
    sfree(handle);

    return req;
}

void fxp_close_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    sfree(req);
    fxp_got_status(pktin);
    sftp_pkt_free(pktin);
}

struct sftp_request *fxp_mkdir_send(char *path)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_MKDIR);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, path);
    sftp_pkt_adduint32(pktout, 0);     /* (FIXME) empty ATTRS structure */
    sftp_send(pktout);

    return req;
}

int fxp_mkdir_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

struct sftp_request *fxp_rmdir_send(char *path)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_RMDIR);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, path);
    sftp_send(pktout);

    return req;
}

int fxp_rmdir_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

struct sftp_request *fxp_remove_send(char *fname)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_REMOVE);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, fname);
    sftp_send(pktout);

    return req;
}

int fxp_remove_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

struct sftp_request *fxp_rename_send(char *srcfname, char *dstfname)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_RENAME);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, srcfname);
    sftp_pkt_addstring(pktout, dstfname);
    sftp_send(pktout);

    return req;
}

int fxp_rename_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

/*
 * Retrieve the attributes of a file. We have fxp_stat which works
 * on filenames, and fxp_fstat which works on open file handles.
 */
struct sftp_request *fxp_stat_send(char *fname)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_STAT);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, fname);
    sftp_send(pktout);

    return req;
}

int fxp_stat_recv(struct sftp_packet *pktin, struct sftp_request *req,
		  struct fxp_attrs *attrs)
{
    sfree(req);
    if (pktin->type == SSH_FXP_ATTRS) {
	*attrs = sftp_pkt_getattrs(pktin);
        sftp_pkt_free(pktin);
	return 1;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return 0;
    }
}

struct sftp_request *fxp_fstat_send(struct fxp_handle *handle)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_FSTAT);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_send(pktout);

    return req;
}

int fxp_fstat_recv(struct sftp_packet *pktin, struct sftp_request *req,
		   struct fxp_attrs *attrs)
{
    sfree(req);
    if (pktin->type == SSH_FXP_ATTRS) {
	*attrs = sftp_pkt_getattrs(pktin);
        sftp_pkt_free(pktin);
	return 1;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return 0;
    }
}

/*
 * Set the attributes of a file.
 */
struct sftp_request *fxp_setstat_send(char *fname, struct fxp_attrs attrs)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_SETSTAT);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring(pktout, fname);
    sftp_pkt_addattrs(pktout, attrs);
    sftp_send(pktout);

    return req;
}

int fxp_setstat_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

struct sftp_request *fxp_fsetstat_send(struct fxp_handle *handle,
				       struct fxp_attrs attrs)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_FSETSTAT);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_pkt_addattrs(pktout, attrs);
    sftp_send(pktout);

    return req;
}

int fxp_fsetstat_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    int id;
    sfree(req);
    id = fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    if (id != 1) {
    	return 0;
    }
    return 1;
}

/*
 * Read from a file. Returns the number of bytes read, or -1 on an
 * error, or possibly 0 if EOF. (I'm not entirely sure whether it
 * will return 0 on EOF, or return -1 and store SSH_FX_EOF in the
 * error indicator. It might even depend on the SFTP server.)
 */
struct sftp_request *fxp_read_send(struct fxp_handle *handle,
				   uint64 offset, int len)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_READ);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_pkt_adduint64(pktout, offset);
    sftp_pkt_adduint32(pktout, len);
    sftp_send(pktout);

    return req;
}

int fxp_read_recv(struct sftp_packet *pktin, struct sftp_request *req,
		  char *buffer, int len)
{
    sfree(req);
    if (pktin->type == SSH_FXP_DATA) {
	char *str;
	int rlen;

	sftp_pkt_getstring(pktin, &str, &rlen);

	if (rlen > len || rlen < 0) {
	    fxp_internal_error("READ returned more bytes than requested");
            sftp_pkt_free(pktin);
	    return -1;
	}

	memcpy(buffer, str, rlen);
        sftp_pkt_free(pktin);
	return rlen;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return -1;
    }
}

/*
 * Read from a directory.
 */
struct sftp_request *fxp_readdir_send(struct fxp_handle *handle)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_READDIR);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_send(pktout);

    return req;
}

struct fxp_names *fxp_readdir_recv(struct sftp_packet *pktin,
				   struct sftp_request *req)
{
    sfree(req);
    if (pktin->type == SSH_FXP_NAME) {
	struct fxp_names *ret;
	int i;
	ret = snew(struct fxp_names);
	ret->nnames = sftp_pkt_getuint32(pktin);
	ret->names = snewn(ret->nnames, struct fxp_name);
	for (i = 0; i < ret->nnames; i++) {
	    char *str;
	    int len;
	    sftp_pkt_getstring(pktin, &str, &len);
	    ret->names[i].filename = mkstr(str, len);
	    sftp_pkt_getstring(pktin, &str, &len);
	    ret->names[i].longname = mkstr(str, len);
	    ret->names[i].attrs = sftp_pkt_getattrs(pktin);
	}
        sftp_pkt_free(pktin);
	return ret;
    } else {
	fxp_got_status(pktin);
        sftp_pkt_free(pktin);
	return NULL;
    }
}

/*
 * Write to a file. Returns 0 on error, 1 on OK.
 */
struct sftp_request *fxp_write_send(struct fxp_handle *handle,
				    char *buffer, uint64 offset, int len)
{
    struct sftp_request *req = sftp_alloc_request();
    struct sftp_packet *pktout;

    pktout = sftp_pkt_init(SSH_FXP_WRITE);
    sftp_pkt_adduint32(pktout, req->id);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, handle->hstring, handle->hlen);
    sftp_pkt_adduint64(pktout, offset);
    sftp_pkt_addstring_start(pktout);
    sftp_pkt_addstring_data(pktout, buffer, len);
    sftp_send(pktout);

    return req;
}

int fxp_write_recv(struct sftp_packet *pktin, struct sftp_request *req)
{
    sfree(req);
    fxp_got_status(pktin);
    sftp_pkt_free(pktin);
    return fxp_errtype == SSH_FX_OK;
}

/*
 * Free up an fxp_names structure.
 */
void fxp_free_names(struct fxp_names *names)
{
    int i;

    for (i = 0; i < names->nnames; i++) {
	sfree(names->names[i].filename);
	sfree(names->names[i].longname);
    }
    sfree(names->names);
    sfree(names);
}

/*
 * Duplicate an fxp_name structure.
 */
struct fxp_name *fxp_dup_name(struct fxp_name *name)
{
    struct fxp_name *ret;
    ret = snew(struct fxp_name);
    ret->filename = dupstr(name->filename);
    ret->longname = dupstr(name->longname);
    ret->attrs = name->attrs;	       /* structure copy */
    return ret;
}

/*
 * Free up an fxp_name structure.
 */
void fxp_free_name(struct fxp_name *name)
{
    sfree(name->filename);
    sfree(name->longname);
    sfree(name);
}
