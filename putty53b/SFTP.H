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
char *fxp_realpath(char *path);

/*
 * Open a file.
 */
struct fxp_handle *fxp_open(char *path, int type);

/*
 * Open a directory.
 */
struct fxp_handle *fxp_opendir(char *path);

/*
 * Close a file/dir.
 */
void fxp_close(struct fxp_handle *handle);

/*
 * Make a directory.
 */
int fxp_mkdir(char *path);

/*
 * Remove a directory.
 */
int fxp_rmdir(char *path);

/*
 * Remove a file.
 */
int fxp_remove(char *fname);

/*
 * Rename a file.
 */
int fxp_rename(char *srcfname, char *dstfname);

/*
 * Return file attributes.
 */
int fxp_stat(char *fname, struct fxp_attrs *attrs);
int fxp_fstat(struct fxp_handle *handle, struct fxp_attrs *attrs);

/*
 * Set file attributes.
 */
int fxp_setstat(char *fname, struct fxp_attrs attrs);
int fxp_fsetstat(struct fxp_handle *handle, struct fxp_attrs attrs);

/*
 * Read from a file.
 */
int fxp_read(struct fxp_handle *handle, char *buffer, uint64 offset,
	     int len);

/*
 * Write to a file. Returns 0 on error, 1 on OK.
 */
int fxp_write(struct fxp_handle *handle, char *buffer, uint64 offset,
	      int len);

/*
 * Read from a directory.
 */
struct fxp_names *fxp_readdir(struct fxp_handle *handle);

/*
 * Free up an fxp_names structure.
 */
void fxp_free_names(struct fxp_names *names);

/*
 * Duplicate and free fxp_name structures.
 */
struct fxp_name *fxp_dup_name(struct fxp_name *name);
void fxp_free_name(struct fxp_name *name);
