/*
 * sftp.h: definitions for SFTP and the sftp.c routines.
 */

#include "int64.h"

#define SSH_FXP_INIT                              1	/* 0x1 */
#define SSH_FXP_VERSION                           2	/* 0x2 */
#define SSH_FXP_OPEN                              3	/* 0x3 */
#define SSH_FXP_CLOSE                             4	/* 0x4 */
#define SSH_FXP_READ                              5	/* 0x5 */
#define SSH_FXP_WRITE                             6	/* 0x6 */
#define SSH_FXP_LSTAT                             7	/* 0x7 */
#define SSH_FXP_FSTAT                             8	/* 0x8 */
#define SSH_FXP_SETSTAT                           9	/* 0x9 */
#define SSH_FXP_FSETSTAT                          10	/* 0xa */
#define SSH_FXP_OPENDIR                           11	/* 0xb */
#define SSH_FXP_READDIR                           12	/* 0xc */
#define SSH_FXP_REMOVE                            13	/* 0xd */
#define SSH_FXP_MKDIR                             14	/* 0xe */
#define SSH_FXP_RMDIR                             15	/* 0xf */
#define SSH_FXP_REALPATH                          16	/* 0x10 */
#define SSH_FXP_STAT                              17	/* 0x11 */
#define SSH_FXP_RENAME                            18	/* 0x12 */
#define SSH_FXP_STATUS                            101	/* 0x65 */
#define SSH_FXP_HANDLE                            102	/* 0x66 */
#define SSH_FXP_DATA                              103	/* 0x67 */
#define SSH_FXP_NAME                              104	/* 0x68 */
#define SSH_FXP_ATTRS                             105	/* 0x69 */
#define SSH_FXP_EXTENDED                          200	/* 0xc8 */
#define SSH_FXP_EXTENDED_REPLY                    201	/* 0xc9 */

#define SSH_FX_OK                                 0
#define SSH_FX_EOF                                1
#define SSH_FX_NO_SUCH_FILE                       2
#define SSH_FX_PERMISSION_DENIED                  3
#define SSH_FX_FAILURE                            4
#define SSH_FX_BAD_MESSAGE                        5
#define SSH_FX_NO_CONNECTION                      6
#define SSH_FX_CONNECTION_LOST                    7
#define SSH_FX_OP_UNSUPPORTED                     8

#define SSH_FILEXFER_ATTR_SIZE                    0x00000001
#define SSH_FILEXFER_ATTR_UIDGID                  0x00000002
#define SSH_FILEXFER_ATTR_PERMISSIONS             0x00000004
#define SSH_FILEXFER_ATTR_ACMODTIME               0x00000008
#define SSH_FILEXFER_ATTR_EXTENDED                0x80000000

#define SSH_FXF_READ                              0x00000001
#define SSH_FXF_WRITE                             0x00000002
#define SSH_FXF_APPEND                            0x00000004
#define SSH_FXF_CREAT                             0x00000008
#define SSH_FXF_TRUNC                             0x00000010
#define SSH_FXF_EXCL                              0x00000020

#define SFTP_PROTO_VERSION 3

/*
 * External references. The sftp client module sftp.c expects to be
 * able to get at these functions.
 * 
 * sftp_recvdata must never return less than len. It either blocks
 * until len is available, or it returns failure.
 * 
 * Both functions return 1 on success, 0 on failure.
 */
int sftp_senddata(char *data, int len);
int sftp_recvdata(char *data, int len);

/*
 * Free sftp_requests
 */
void sftp_cleanup_request(void);

struct fxp_attrs {
    unsigned long flags;
    uint64 size;
    unsigned long uid;
    unsigned long gid;
    unsigned long permissions;
    unsigned long atime;
    unsigned long mtime;
};

struct fxp_handle {
    char *hstring;
    int hlen;
};

struct fxp_name {
    char *filename, *longname;
    struct fxp_attrs attrs;
};

struct fxp_names {
    int nnames;
    struct fxp_name *names;
};

struct sftp_request;
struct sftp_packet;

const char *fxp_error(void);
int fxp_error_type(void);

/*
 * Perform exchange of init/version packets. Return 0 on failure.
 */
int fxp_init(void);

/*
 * Canonify a pathname. Concatenate the two given path elements
 * with a separating slash, unless the second is NULL.
 */
struct sftp_request *fxp_realpath_send(char *path);
char *fxp_realpath_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Open a file.
 */
struct sftp_request *fxp_open_send(char *path, int type);
struct fxp_handle *fxp_open_recv(struct sftp_packet *pktin,
				 struct sftp_request *req);

/*
 * Open a directory.
 */
struct sftp_request *fxp_opendir_send(char *path);
struct fxp_handle *fxp_opendir_recv(struct sftp_packet *pktin,
				    struct sftp_request *req);

/*
 * Close a file/dir.
 */
struct sftp_request *fxp_close_send(struct fxp_handle *handle);
void fxp_close_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Make a directory.
 */
struct sftp_request *fxp_mkdir_send(char *path);
int fxp_mkdir_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Remove a directory.
 */
struct sftp_request *fxp_rmdir_send(char *path);
int fxp_rmdir_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Remove a file.
 */
struct sftp_request *fxp_remove_send(char *fname);
int fxp_remove_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Rename a file.
 */
struct sftp_request *fxp_rename_send(char *srcfname, char *dstfname);
int fxp_rename_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Return file attributes.
 */
struct sftp_request *fxp_stat_send(char *fname);
int fxp_stat_recv(struct sftp_packet *pktin, struct sftp_request *req,
		  struct fxp_attrs *attrs);
struct sftp_request *fxp_fstat_send(struct fxp_handle *handle);
int fxp_fstat_recv(struct sftp_packet *pktin, struct sftp_request *req,
		   struct fxp_attrs *attrs);

/*
 * Set file attributes.
 */
struct sftp_request *fxp_setstat_send(char *fname, struct fxp_attrs attrs);
int fxp_setstat_recv(struct sftp_packet *pktin, struct sftp_request *req);
struct sftp_request *fxp_fsetstat_send(struct fxp_handle *handle,
				       struct fxp_attrs attrs);
int fxp_fsetstat_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Read from a file.
 */
struct sftp_request *fxp_read_send(struct fxp_handle *handle,
				   uint64 offset, int len);
int fxp_read_recv(struct sftp_packet *pktin, struct sftp_request *req,
		  char *buffer, int len);

/*
 * Write to a file. Returns 0 on error, 1 on OK.
 */
struct sftp_request *fxp_write_send(struct fxp_handle *handle,
				    char *buffer, uint64 offset, int len);
int fxp_write_recv(struct sftp_packet *pktin, struct sftp_request *req);

/*
 * Read from a directory.
 */
struct sftp_request *fxp_readdir_send(struct fxp_handle *handle);
struct fxp_names *fxp_readdir_recv(struct sftp_packet *pktin,
				   struct sftp_request *req);

/*
 * Free up an fxp_names structure.
 */
void fxp_free_names(struct fxp_names *names);

/*
 * Duplicate and free fxp_name structures.
 */
struct fxp_name *fxp_dup_name(struct fxp_name *name);
void fxp_free_name(struct fxp_name *name);

/*
 * Store user data in an sftp_request structure.
 */
void *fxp_get_userdata(struct sftp_request *req);
void fxp_set_userdata(struct sftp_request *req, void *data);

/*
 * These functions might well be temporary placeholders to be
 * replaced with more useful similar functions later. They form the
 * main dispatch loop for processing incoming SFTP responses.
 */
void sftp_register(struct sftp_request *req);
struct sftp_request *sftp_find_request(struct sftp_packet *pktin);
struct sftp_packet *sftp_recv(void);

/*
 * A wrapper to go round fxp_read_* and fxp_write_*, which manages
 * the queueing of multiple read/write requests.
 */

struct fxp_xfer;

struct fxp_xfer *xfer_download_init(struct fxp_handle *fh, uint64 offset);
void xfer_download_queue(struct fxp_xfer *xfer);
int xfer_download_gotpkt(struct fxp_xfer *xfer, struct sftp_packet *pktin);
int xfer_download_data(struct fxp_xfer *xfer, void **buf, int *len);

struct fxp_xfer *xfer_upload_init(struct fxp_handle *fh, uint64 offset);
int xfer_upload_ready(struct fxp_xfer *xfer);
void xfer_upload_data(struct fxp_xfer *xfer, char *buffer, int len);
int xfer_upload_gotpkt(struct fxp_xfer *xfer, struct sftp_packet *pktin);

int xfer_done(struct fxp_xfer *xfer);
void xfer_set_error(struct fxp_xfer *xfer);
void xfer_cleanup(struct fxp_xfer *xfer);
