/*
 * psftp.h: interface between psftp.c / scp.c and each
 * platform-specific SFTP module.
 */

#ifndef PUTTY_PSFTP_H
#define PUTTY_PSFTP_H

/*
 * psftp_getcwd returns the local current directory. The returned
 * string must be freed by the caller.
 */
char *psftp_getcwd(void);

/*
 * psftp_lcd changes the local current directory. The return value
 * is NULL on success, or else an error message which must be freed
 * by the caller.
 */
char *psftp_lcd(char *newdir);

/*
 * Retrieve file times on a local file. Must return two unsigned
 * longs in POSIX time_t format.
 */
void get_file_times(char *filename, unsigned long *mtime,
		    unsigned long *atime);

/*
 * One iteration of the PSFTP event loop: wait for network data and
 * process it, once.
 */
int ssh_sftp_loop_iteration(void);

/*
 * The main program in psftp.c. Called from main() in the platform-
 * specific code, after doing any platform-specific initialisation.
 */
int psftp_main(int argc, char *argv[]);

/*
 * These functions are used by PSCP to transmit progress updates
 * and error information to a GUI window managing it. This will
 * probably only ever be supported on Windows, so these functions
 * can safely be stubs on all other platforms.
 */
void gui_update_stats(char *name, unsigned long size,
		      int percentage, unsigned long elapsed,
		      unsigned long done, unsigned long eta,
		      unsigned long ratebs);
void gui_send_errcount(int list, int errs);
void gui_send_char(int is_stderr, int c);
void gui_enable(char *arg);

/*
 * It's likely that a given platform's implementation of file
 * transfer utilities is going to want to do things with them that
 * aren't present in stdio. Hence we supply an alternative
 * abstraction for file access functions.
 * 
 * This abstraction tells you the size and access times when you
 * open an existing file (platforms may choose the meaning of the
 * file times if it's not clear; whatever they choose will be what
 * PSCP sends to the server as mtime and atime), and lets you set
 * the times when saving a new file.
 * 
 * On the other hand, the abstraction is pretty simple: it supports
 * only opening a file and reading it, or creating a file and
 * writing it. (FIXME: to use this in PSFTP it will also need to
 * support seeking to a starting point for restarted transfers.)
 * None of this read-and-write, seeking-back-and-forth stuff.
 */
typedef struct RFile RFile;
typedef struct WFile WFile;
/* Output params size, mtime and atime can all be NULL if desired */
RFile *open_existing_file(char *name, unsigned long *size,
			  unsigned long *mtime, unsigned long *atime);
/* Returns <0 on error, 0 on eof, or number of bytes read, as usual */
int read_from_file(RFile *f, void *buffer, int length);
/* Closes and frees the RFile */
void close_rfile(RFile *f);
WFile *open_new_file(char *name);
/* Returns <0 on error, 0 on eof, or number of bytes written, as usual */
int write_to_file(WFile *f, void *buffer, int length);
void set_file_times(WFile *f, unsigned long mtime, unsigned long atime);
/* Closes and frees the WFile */
void close_wfile(WFile *f);

/*
 * Determine the type of a file: nonexistent, file, directory or
 * weird. `weird' covers anything else - named pipes, Unix sockets,
 * device files, fish, badgers, you name it. Things marked `weird'
 * will be skipped over in recursive file transfers, so the only
 * real reason for not lumping them in with `nonexistent' is that
 * it allows a slightly more sane error message.
 */
enum {
    FILE_TYPE_NONEXISTENT, FILE_TYPE_FILE, FILE_TYPE_DIRECTORY, FILE_TYPE_WEIRD
};
int file_type(char *name);

/*
 * Read all the file names out of a directory.
 */
typedef struct DirHandle DirHandle;
DirHandle *open_directory(char *name);
/* The string returned from this will need freeing if not NULL */
char *read_filename(DirHandle *dir);
void close_directory(DirHandle *dir);

/*
 * Test a filespec to see whether it's a local wildcard or not.
 * Return values:
 * 
 *  - WCTYPE_WILDCARD (this is a wildcard).
 *  - WCTYPE_FILENAME (this is a single file name).
 *  - WCTYPE_NONEXISTENT (whichever it was, nothing of that name exists).
 * 
 * Some platforms may choose not to support local wildcards when
 * they come from the command line; in this case they simply never
 * return WCTYPE_WILDCARD, but still test the file's existence.
 * (However, all platforms will probably want to support wildcards
 * inside the PSFTP CLI.)
 */
enum {
    WCTYPE_NONEXISTENT, WCTYPE_FILENAME, WCTYPE_WILDCARD
};
int test_wildcard(char *name, int cmdline);

/*
 * Actually return matching file names for a local wildcard.
 */
typedef struct WildcardMatcher WildcardMatcher;
WildcardMatcher *begin_wildcard_matching(char *name);
/* The string returned from this will need freeing if not NULL */
char *wildcard_get_filename(WildcardMatcher *dir);
void finish_wildcard_matching(WildcardMatcher *dir);

/*
 * Create a directory. Returns 0 on error, !=0 on success.
 */
int create_directory(char *name);

/*
 * Concatenate a directory name and a file name. The way this is
 * done will depend on the OS.
 */
char *dir_file_cat(char *dir, char *file);

#endif /* PUTTY_PSFTP_H */
