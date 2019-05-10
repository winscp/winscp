/*
 * winstuff.h: Windows-specific inter-module stuff.
 */

#ifndef PUTTY_WINSTUFF_H
#define PUTTY_WINSTUFF_H

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
#include <windows.h>
#include <stdio.h>		       /* for FILENAME_MAX */

/* We use uintptr_t for Win32/Win64 portability, so we should in
 * principle include stdint.h, which defines it according to the C
 * standard. But older versions of Visual Studio - including the one
 * used for official PuTTY builds as of 2015-09-28 - don't provide
 * stdint.h at all, but do (non-standardly) define uintptr_t in
 * stddef.h. So here we try to make sure _some_ standard header is
 * included which defines uintptr_t. */
#include <stddef.h>
#if !defined _MSC_VER || _MSC_VER >= 1600 || defined __clang__
#include <stdint.h>
#endif

#include "defs.h"
#include "marshal.h"

#include "tree234.h"

#include "winhelp.h"

#if defined _M_IX86 || defined _M_AMD64
#define BUILDINFO_PLATFORM "x86 Windows"
#elif defined _M_ARM || defined _M_ARM64
#define BUILDINFO_PLATFORM "Arm Windows"
#else
#define BUILDINFO_PLATFORM "Windows"
#endif

struct Filename {
    char *path;
};
static inline FILE *f_open(const Filename *filename, const char *mode,
                           bool isprivate)
{
    return fopen(filename->path, mode);
}

struct FontSpec {
    char *name;
    bool isbold;
    int height;
    int charset;
};
struct FontSpec *fontspec_new(
    const char *name, bool bold, int height, int charset);

#ifndef CLEARTYPE_QUALITY
#define CLEARTYPE_QUALITY 5
#endif
#define FONT_QUALITY(fq) ( \
    (fq) == FQ_DEFAULT ? DEFAULT_QUALITY : \
    (fq) == FQ_ANTIALIASED ? ANTIALIASED_QUALITY : \
    (fq) == FQ_NONANTIALIASED ? NONANTIALIASED_QUALITY : \
    CLEARTYPE_QUALITY)

#define PLATFORM_IS_UTF16 /* enable UTF-16 processing when exchanging
			   * wchar_t strings with environment */

#define PLATFORM_CLIPBOARDS(X)                      \
    X(CLIP_SYSTEM, "system clipboard")              \
    /* end of list */

/*
 * Where we can, we use GetWindowLongPtr and friends because they're
 * more useful on 64-bit platforms, but they're a relatively recent
 * innovation, missing from VC++ 6 and older MinGW.  Degrade nicely.
 * (NB that on some systems, some of these things are available but
 * not others...)
 */

#ifndef GCLP_HCURSOR
/* GetClassLongPtr and friends */
#undef  GetClassLongPtr
#define GetClassLongPtr GetClassLong
#undef  SetClassLongPtr
#define SetClassLongPtr SetClassLong
#define GCLP_HCURSOR GCL_HCURSOR
/* GetWindowLongPtr and friends */
#undef  GetWindowLongPtr
#define GetWindowLongPtr GetWindowLong
#undef  SetWindowLongPtr
#define SetWindowLongPtr SetWindowLong
#undef  GWLP_USERDATA
#define GWLP_USERDATA GWL_USERDATA
#undef  DWLP_MSGRESULT
#define DWLP_MSGRESULT DWL_MSGRESULT
/* Since we've clobbered the above functions, we should clobber the
 * associated type regardless of whether it's defined. */
#undef LONG_PTR
#define LONG_PTR LONG
#endif

#define BOXFLAGS DLGWINDOWEXTRA
#define BOXRESULT (DLGWINDOWEXTRA + sizeof(LONG_PTR))
#define DF_END 0x0001

#ifndef __WINE__
/* Up-to-date Windows headers warn that the unprefixed versions of
 * these names are deprecated. */
#define stricmp _stricmp
#define strnicmp _strnicmp
#else
/* Compiling with winegcc, _neither_ version of these functions
 * exists. Use the POSIX names. */
#define stricmp strcasecmp
#define strnicmp strncasecmp
#endif

#define BROKEN_PIPE_ERROR_CODE ERROR_BROKEN_PIPE   /* used in sshshare.c */

/*
 * Dynamically linked functions. These come in two flavours:
 *
 *  - GET_WINDOWS_FUNCTION does not expose "name" to the preprocessor,
 *    so will always dynamically link against exactly what is specified
 *    in "name". If you're not sure, use this one.
 *
 *  - GET_WINDOWS_FUNCTION_PP allows "name" to be redirected via
 *    preprocessor definitions like "#define foo bar"; this is principally
 *    intended for the ANSI/Unicode DoSomething/DoSomethingA/DoSomethingW.
 *    If your function has an argument of type "LPTSTR" or similar, this
 *    is the variant to use.
 *    (However, it can't always be used, as it trips over more complicated
 *    macro trickery such as the WspiapiGetAddrInfo wrapper for getaddrinfo.)
 *
 * (DECL_WINDOWS_FUNCTION works with both these variants.)
 */
#define DECL_WINDOWS_FUNCTION(linkage, rettype, name, params)   \
    typedef rettype (WINAPI *t_##name) params;                  \
    linkage t_##name p_##name
#define STR1(x) #x
#define STR(x) STR1(x)
#define GET_WINDOWS_FUNCTION_PP(module, name)                           \
    TYPECHECK((t_##name)NULL == name,                                   \
              (p_##name = module ?                                      \
               (t_##name) GetProcAddress(module, STR(name)) : NULL))
#define GET_WINDOWS_FUNCTION(module, name)                              \
    TYPECHECK((t_##name)NULL == name,                                   \
              (p_##name = module ?                                      \
               (t_##name) GetProcAddress(module, #name) : NULL))
#define GET_WINDOWS_FUNCTION_NO_TYPECHECK(module, name) \
    (p_##name = module ?                                \
     (t_##name) GetProcAddress(module, #name) : NULL)

/*
 * Global variables. Most modules declare these `extern', but
 * window.c will do `#define PUTTY_DO_GLOBALS' before including this
 * module, and so will get them properly defined.
*/
#ifndef GLOBAL
#ifdef PUTTY_DO_GLOBALS
#define GLOBAL
#else
#define GLOBAL extern
#endif
#endif

#define PUTTY_REG_POS "Software\\SimonTatham\\PuTTY"
#define PUTTY_REG_PARENT "Software\\SimonTatham"
#define PUTTY_REG_PARENT_CHILD "PuTTY"
#define PUTTY_REG_GPARENT "Software"
#define PUTTY_REG_GPARENT_CHILD "SimonTatham"

/* Result values for the jumplist registry functions. */
#define JUMPLISTREG_OK 0
#define JUMPLISTREG_ERROR_INVALID_PARAMETER 1
#define JUMPLISTREG_ERROR_KEYOPENCREATE_FAILURE 2
#define JUMPLISTREG_ERROR_VALUEREAD_FAILURE 3
#define JUMPLISTREG_ERROR_VALUEWRITE_FAILURE 4
#define JUMPLISTREG_ERROR_INVALID_VALUE 5

#define PUTTY_CHM_FILE "putty.chm"

#define GETTICKCOUNT GetTickCount
#define CURSORBLINK GetCaretBlinkTime()
#define TICKSPERSEC 1000	       /* GetTickCount returns milliseconds */

#define DEFAULT_CODEPAGE CP_ACP
#define USES_VTLINE_HACK

#ifndef NO_GSSAPI
/*
 * GSS-API stuff
 */
#define GSS_CC CALLBACK
/*
typedef struct Ssh_gss_buf {
    size_t length;
    char *value;
} Ssh_gss_buf;

#define SSH_GSS_EMPTY_BUF (Ssh_gss_buf) {0,NULL}
typedef void *Ssh_gss_name;
*/
#endif

/*
 * Window handles for the windows that can be running during a
 * PuTTY session.
 */
GLOBAL HWND hwnd;	/* the main terminal window */
GLOBAL HWND logbox;

/*
 * The all-important instance handle.
 */
GLOBAL HINSTANCE hinst;

/*
 * Help file stuff in winhelp.c.
 */
void init_help(void);
void shutdown_help(void);
bool has_help(void);
void launch_help(HWND hwnd, const char *topic);
void quit_help(HWND hwnd);
int has_embedded_chm(void);            /* 1 = yes, 0 = no, -1 = N/A */

/*
 * The terminal and logging context are notionally local to the
 * Windows front end, but they must be shared between window.c and
 * windlg.c. Likewise the Seat structure for the Windows GUI, and the
 * Conf for the main session..
 */
GLOBAL Terminal *term;
GLOBAL LogContext *logctx;
GLOBAL Conf *conf;

/*
 * GUI seat methods in windlg.c, so that the vtable definition in
 * window.c can refer to them.
 */
int win_seat_verify_ssh_host_key(
    Seat *seat, const char *host, int port,
    const char *keytype, char *keystr, char *key_fingerprint,
    void (*callback)(void *ctx, int result), void *ctx);
int win_seat_confirm_weak_crypto_primitive(
    Seat *seat, const char *algtype, const char *algname,
    void (*callback)(void *ctx, int result), void *ctx);
int win_seat_confirm_weak_cached_hostkey(
    Seat *seat, const char *algname, const char *betteralgs,
    void (*callback)(void *ctx, int result), void *ctx);

/*
 * The Windows GUI seat object itself, so that its methods can be
 * called outside window.c.
 */
extern Seat *const win_seat;

/*
 * Windows-specific clipboard helper function shared with windlg.c,
 * which takes the data string in the system code page instead of
 * Unicode.
 */
void write_aclip(int clipboard, char *, int, bool);

#define WM_NETEVENT  (WM_APP + 5)

/*
 * On Windows, we send MA_2CLK as the only event marking the second
 * press of a mouse button. Compare unix.h.
 */
#define MULTICLICK_ONLY_EVENT 1

/*
 * On Windows, data written to the clipboard must be NUL-terminated.
 */
#define SELECTION_NUL_TERMINATED 1

/*
 * On Windows, copying to the clipboard terminates lines with CRLF.
 */
#define SEL_NL { 13, 10 }

/*
 * sk_getxdmdata() does not exist under Windows (not that I
 * couldn't write it if I wanted to, but I haven't bothered), so
 * it's a macro which always returns NULL. With any luck this will
 * cause the compiler to notice it can optimise away the
 * implementation of XDM-AUTHORIZATION-1 in x11fwd.c :-)
 */
#define sk_getxdmdata(socket, lenp) (NULL)

/*
 * File-selector filter strings used in the config box. On Windows,
 * these strings are of exactly the type needed to go in
 * `lpstrFilter' in an OPENFILENAME structure.
 */
#define FILTER_KEY_FILES ("PuTTY Private Key Files (*.ppk)\0*.ppk\0" \
			      "All Files (*.*)\0*\0\0\0")
#define FILTER_WAVE_FILES ("Wave Files (*.wav)\0*.WAV\0" \
			       "All Files (*.*)\0*\0\0\0")
#define FILTER_DYNLIB_FILES ("Dynamic Library Files (*.dll)\0*.dll\0" \
				 "All Files (*.*)\0*\0\0\0")

/*
 * Exports from winnet.c.
 */
/* Report an event notification from WSA*Select */
void select_result(WPARAM, LPARAM);
/* Enumerate all currently live OS-level SOCKETs */
SOCKET first_socket(int *);
SOCKET next_socket(int *);
/* Ask winnet.c whether we currently want to try to write to a SOCKET */
bool socket_writable(SOCKET skt);
/* Force a refresh of the SOCKET list by re-calling do_select for each one */
void socket_reselect_all(void);
/* Make a SockAddr which just holds a named pipe address. */
SockAddr *sk_namedpipe_addr(const char *pipename);

/*
 * winnet.c dynamically loads WinSock 2 or WinSock 1 depending on
 * what it can get, which means any WinSock routines used outside
 * that module must be exported from it as function pointers. So
 * here they are.
 */
DECL_WINDOWS_FUNCTION(GLOBAL, int, WSAAsyncSelect,
		      (SOCKET, HWND, u_int, long));
DECL_WINDOWS_FUNCTION(GLOBAL, int, WSAEventSelect,
		      (SOCKET, WSAEVENT, long));
DECL_WINDOWS_FUNCTION(GLOBAL, int, WSAGetLastError, (void));
DECL_WINDOWS_FUNCTION(GLOBAL, int, WSAEnumNetworkEvents,
		      (SOCKET, WSAEVENT, LPWSANETWORKEVENTS));
#ifdef NEED_DECLARATION_OF_SELECT
/* This declaration is protected by an ifdef for the sake of building
 * against winelib, in which you have to include winsock2.h before
 * stdlib.h so that the right fd_set type gets defined. It would be a
 * pain to do that throughout this codebase, so instead I arrange that
 * only a modules actually needing to use (or define, or initialise)
 * this function pointer will see its declaration, and _those_ modules
 * - which will be Windows-specific anyway - can take more care. */
DECL_WINDOWS_FUNCTION(GLOBAL, int, select,
		      (int, fd_set FAR *, fd_set FAR *,
		       fd_set FAR *, const struct timeval FAR *));
#endif

/*
 * Provided by each client of winnet.c, and called by winnet.c to turn
 * on or off WSA*Select for a given socket.
 */
char *do_select(SOCKET skt, bool startup);

/*
 * Network-subsystem-related functions provided in other Windows modules.
 */
Socket *make_handle_socket(HANDLE send_H, HANDLE recv_H, HANDLE stderr_H,
                           Plug *plug, bool overlapped); /* winhsock */
Socket *new_named_pipe_client(const char *pipename, Plug *plug); /* winnpc */
Socket *new_named_pipe_listener(const char *pipename, Plug *plug); /* winnps */

/*
 * Exports from winctrls.c.
 */

struct ctlpos {
    HWND hwnd;
    WPARAM font;
    int dlu4inpix;
    int ypos, width;
    int xoff;
    int boxystart, boxid;
    char *boxtext;
};
void init_common_controls(void);       /* also does some DLL-loading */

/*
 * Exports from winutils.c.
 */
typedef struct filereq_tag filereq; /* cwd for file requester */
bool request_file(filereq *state, OPENFILENAME *of, bool preserve, bool save);
filereq *filereq_new(void);
void filereq_free(filereq *state);
int message_box(LPCTSTR text, LPCTSTR caption, DWORD style, DWORD helpctxid);
char *GetDlgItemText_alloc(HWND hwnd, int id);
void split_into_argv(char *, int *, char ***, char ***);

/*
 * Private structure for prefslist state. Only in the header file
 * so that we can delegate allocation to callers.
 */
struct prefslist {
    int listid, upbid, dnbid;
    int srcitem;
    int dummyitem;
    bool dragging;
};

/*
 * This structure is passed to event handler functions as the `dlg'
 * parameter, and hence is passed back to winctrls access functions.
 */
struct dlgparam {
    HWND hwnd;			       /* the hwnd of the dialog box */
    struct winctrls *controltrees[8];  /* can have several of these */
    int nctrltrees;
    char *wintitle;		       /* title of actual window */
    char *errtitle;		       /* title of error sub-messageboxes */
    void *data;			       /* data to pass in refresh events */
    union control *focused, *lastfocused; /* which ctrl has focus now/before */
    bool shortcuts[128];               /* track which shortcuts in use */
    bool coloursel_wanted;             /* has an event handler asked for
					* a colour selector? */
    struct {
        unsigned char r, g, b;         /* 0-255 */
        bool ok;
    } coloursel_result;
    tree234 *privdata;		       /* stores per-control private data */
    bool ended;                        /* has the dialog been ended? */
    int endresult;                     /* and if so, what was the result? */
    bool fixed_pitch_fonts;            /* are we constrained to fixed fonts? */
};

/*
 * Exports from winctrls.c.
 */
void ctlposinit(struct ctlpos *cp, HWND hwnd,
		int leftborder, int rightborder, int topborder);
HWND doctl(struct ctlpos *cp, RECT r,
	   char *wclass, int wstyle, int exstyle, char *wtext, int wid);
void bartitle(struct ctlpos *cp, char *name, int id);
void beginbox(struct ctlpos *cp, char *name, int idbox);
void endbox(struct ctlpos *cp);
void editboxfw(struct ctlpos *cp, bool password, char *text,
	       int staticid, int editid);
void radioline(struct ctlpos *cp, char *text, int id, int nacross, ...);
void bareradioline(struct ctlpos *cp, int nacross, ...);
void radiobig(struct ctlpos *cp, char *text, int id, ...);
void checkbox(struct ctlpos *cp, char *text, int id);
void statictext(struct ctlpos *cp, char *text, int lines, int id);
void staticbtn(struct ctlpos *cp, char *stext, int sid,
	       char *btext, int bid);
void static2btn(struct ctlpos *cp, char *stext, int sid,
		char *btext1, int bid1, char *btext2, int bid2);
void staticedit(struct ctlpos *cp, char *stext,
		int sid, int eid, int percentedit);
void staticddl(struct ctlpos *cp, char *stext,
	       int sid, int lid, int percentlist);
void combobox(struct ctlpos *cp, char *text, int staticid, int listid);
void staticpassedit(struct ctlpos *cp, char *stext,
		    int sid, int eid, int percentedit);
void bigeditctrl(struct ctlpos *cp, char *stext,
		 int sid, int eid, int lines);
void ersatztab(struct ctlpos *cp, char *stext, int sid, int lid, int s2id);
void editbutton(struct ctlpos *cp, char *stext, int sid,
		int eid, char *btext, int bid);
void sesssaver(struct ctlpos *cp, char *text,
	       int staticid, int editid, int listid, ...);
void envsetter(struct ctlpos *cp, char *stext, int sid,
	       char *e1stext, int e1sid, int e1id,
	       char *e2stext, int e2sid, int e2id,
	       int listid, char *b1text, int b1id, char *b2text, int b2id);
void charclass(struct ctlpos *cp, char *stext, int sid, int listid,
	       char *btext, int bid, int eid, char *s2text, int s2id);
void colouredit(struct ctlpos *cp, char *stext, int sid, int listid,
		char *btext, int bid, ...);
void prefslist(struct prefslist *hdl, struct ctlpos *cp, int lines,
	       char *stext, int sid, int listid, int upbid, int dnbid);
int handle_prefslist(struct prefslist *hdl,
		     int *array, int maxmemb,
		     bool is_dlmsg, HWND hwnd,
		     WPARAM wParam, LPARAM lParam);
void progressbar(struct ctlpos *cp, int id);
void fwdsetter(struct ctlpos *cp, int listid, char *stext, int sid,
	       char *e1stext, int e1sid, int e1id,
	       char *e2stext, int e2sid, int e2id,
	       char *btext, int bid,
	       char *r1text, int r1id, char *r2text, int r2id);

void dlg_auto_set_fixed_pitch_flag(dlgparam *dlg);
bool dlg_get_fixed_pitch_flag(dlgparam *dlg);
void dlg_set_fixed_pitch_flag(dlgparam *dlg, bool flag);

#define MAX_SHORTCUTS_PER_CTRL 16

/*
 * This structure is what's stored for each `union control' in the
 * portable-dialog interface.
 */
struct winctrl {
    union control *ctrl;
    /*
     * The control may have several components at the Windows
     * level, with different dialog IDs. To avoid needing N
     * separate platformsidectrl structures (which could be stored
     * separately in a tree234 so that lookup by ID worked), we
     * impose the constraint that those IDs must be in a contiguous
     * block.
     */
    int base_id;
    int num_ids;
    /*
     * Remember what keyboard shortcuts were used by this control,
     * so that when we remove it again we can take them out of the
     * list in the dlgparam.
     */
    char shortcuts[MAX_SHORTCUTS_PER_CTRL];
    /*
     * Some controls need a piece of allocated memory in which to
     * store temporary data about the control.
     */
    void *data;
};
/*
 * And this structure holds a set of the above, in two separate
 * tree234s so that it can find an item by `union control' or by
 * dialog ID.
 */
struct winctrls {
    tree234 *byctrl, *byid;
};
struct controlset;
struct controlbox;

void winctrl_init(struct winctrls *);
void winctrl_cleanup(struct winctrls *);
void winctrl_add(struct winctrls *, struct winctrl *);
void winctrl_remove(struct winctrls *, struct winctrl *);
struct winctrl *winctrl_findbyctrl(struct winctrls *, union control *);
struct winctrl *winctrl_findbyid(struct winctrls *, int);
struct winctrl *winctrl_findbyindex(struct winctrls *, int);
void winctrl_layout(struct dlgparam *dp, struct winctrls *wc,
		    struct ctlpos *cp, struct controlset *s, int *id);
bool winctrl_handle_command(struct dlgparam *dp, UINT msg,
                            WPARAM wParam, LPARAM lParam);
void winctrl_rem_shortcuts(struct dlgparam *dp, struct winctrl *c);
bool winctrl_context_help(struct dlgparam *dp, HWND hwnd, int id);

void dp_init(struct dlgparam *dp);
void dp_add_tree(struct dlgparam *dp, struct winctrls *tree);
void dp_cleanup(struct dlgparam *dp);

/*
 * Exports from wincfg.c.
 */
void win_setup_config_box(struct controlbox *b, HWND *hwndp, bool has_help,
			  bool midsession, int protocol);

/*
 * Exports from windlg.c.
 */
void defuse_showwindow(void);
bool do_config(void);
bool do_reconfig(HWND, int);
void showeventlog(HWND);
void showabout(HWND);
void force_normal(HWND hwnd);
void modal_about_box(HWND hwnd);
void show_help(HWND hwnd);

/*
 * Exports from winmisc.c.
 */
GLOBAL DWORD osMajorVersion, osMinorVersion, osPlatformId;
void init_winver(void);
void dll_hijacking_protection(void);
HMODULE load_system32_dll(const char *libname);
const char *win_strerror(int error);
void restrict_process_acl(void);
GLOBAL bool restricted_acl;
void escape_registry_key(const char *in, strbuf *out);
void unescape_registry_key(const char *in, strbuf *out);

bool is_console_handle(HANDLE);

/* A few pieces of up-to-date Windows API definition needed for older
 * compilers. */
#ifndef LOAD_LIBRARY_SEARCH_SYSTEM32
#define LOAD_LIBRARY_SEARCH_SYSTEM32 0x00000800
#endif
#ifndef LOAD_LIBRARY_SEARCH_USER_DIRS
#define LOAD_LIBRARY_SEARCH_USER_DIRS 0x00000400
#endif
#ifndef LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR
#define LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR 0x00000100
#endif
#ifndef DLL_DIRECTORY_COOKIE
typedef PVOID DLL_DIRECTORY_COOKIE;
DECLSPEC_IMPORT DLL_DIRECTORY_COOKIE WINAPI AddDllDirectory (PCWSTR NewDirectory);
#endif

/*
 * Exports from sizetip.c.
 */
void UpdateSizeTip(HWND src, int cx, int cy);
void EnableSizeTip(bool bEnable);

/*
 * Exports from unicode.c.
 */
struct unicode_data;
void init_ucs(Conf *, struct unicode_data *);

/*
 * Exports from winhandl.c.
 */
#define HANDLE_FLAG_OVERLAPPED 1
#define HANDLE_FLAG_IGNOREEOF 2
#define HANDLE_FLAG_UNITBUFFER 4
struct handle;
typedef size_t (*handle_inputfn_t)(
    struct handle *h, const void *data, size_t len, int err);
typedef void (*handle_outputfn_t)(
    struct handle *h, size_t new_backlog, int err);
struct handle *handle_input_new(HANDLE handle, handle_inputfn_t gotdata,
				void *privdata, int flags);
struct handle *handle_output_new(HANDLE handle, handle_outputfn_t sentdata,
				 void *privdata, int flags);
size_t handle_write(struct handle *h, const void *data, size_t len);
void handle_write_eof(struct handle *h);
HANDLE *handle_get_events(int *nevents);
void handle_free(struct handle *h);
void handle_got_event(HANDLE event);
void handle_unthrottle(struct handle *h, size_t backlog);
size_t handle_backlog(struct handle *h);
void *handle_get_privdata(struct handle *h);
struct handle *handle_add_foreign_event(HANDLE event,
                                        void (*callback)(void *), void *ctx);
/* Analogue of stdio_sink in marshal.h, for a Windows handle */
struct handle_sink {
    struct handle *h;
    BinarySink_IMPLEMENTATION;
};
void handle_sink_init(handle_sink *sink, struct handle *h);

/*
 * winpgntc.c needs to schedule callbacks for asynchronous agent
 * requests. This has to be done differently in GUI and console, so
 * there's an exported function used for the purpose.
 * 
 * Also, we supply FLAG_SYNCAGENT to force agent requests to be
 * synchronous in pscp and psftp.
 */
void agent_schedule_callback(void (*callback)(void *, void *, int),
			     void *callback_ctx, void *data, int len);
#define FLAG_SYNCAGENT 0x1000

/*
 * Exports from winser.c.
 */
extern const struct BackendVtable serial_backend;

/*
 * Exports from winjump.c.
 */
#define JUMPLIST_SUPPORTED             /* suppress #defines in putty.h */
void add_session_to_jumplist(const char * const sessionname);
void remove_session_from_jumplist(const char * const sessionname);
void clear_jumplist(void);
bool set_explicit_app_user_model_id(void);

/*
 * Exports from winnoise.c.
 */
bool win_read_random(void *buf, unsigned wanted); /* returns true on success */

/*
 * Extra functions in winstore.c over and above the interface in
 * storage.h.
 *
 * These functions manipulate the Registry section which mirrors the
 * current Windows 7 jump list. (Because the real jump list storage is
 * write-only, we need to keep another copy of whatever we put in it,
 * so that we can put in a slightly modified version the next time.)
 */

/* Adds a saved session to the registry jump list mirror. 'item' is a
 * string naming a saved session. */
int add_to_jumplist_registry(const char *item);

/* Removes an item from the registry jump list mirror. */
int remove_from_jumplist_registry(const char *item);

/* Returns the current jump list entries from the registry. Caller
 * must free the returned pointer, which points to a contiguous
 * sequence of NUL-terminated strings in memory, terminated with an
 * empty one. */
char *get_jumplist_registry_entries(void);

/*
 * Windows clipboard-UI wording.
 */
#define CLIPNAME_IMPLICIT "Last selected text"
#define CLIPNAME_EXPLICIT "System clipboard"
#define CLIPNAME_EXPLICIT_OBJECT "system clipboard"
/* These defaults are the ones PuTTY has historically had */
#define CLIPUI_DEFAULT_AUTOCOPY true
#define CLIPUI_DEFAULT_MOUSE CLIPUI_EXPLICIT
#define CLIPUI_DEFAULT_INS CLIPUI_EXPLICIT

/* In winmisc.c */
char *registry_get_string(HKEY root, const char *path, const char *leaf);

#endif
