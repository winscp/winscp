/*
 * windows/platform.h: Windows-specific inter-module stuff.
 */

#ifndef PUTTY_WINDOWS_PLATFORM_H
#define PUTTY_WINDOWS_PLATFORM_H

#include <winsock2.h>
#include <windows.h>
#include <stdio.h>                     /* for FILENAME_MAX */

/* We use uintptr_t for Win32/Win64 portability, so we should in
 * principle include stdint.h, which defines it according to the C
 * standard. But older versions of Visual Studio don't provide
 * stdint.h at all, but do (non-standardly) define uintptr_t in
 * stddef.h. So here we try to make sure _some_ standard header is
 * included which defines uintptr_t. */
#include <stddef.h>
#if !HAVE_NO_STDINT_H
#include <stdint.h>
#endif

#include "defs.h"
#include "marshal.h"

#include "tree234.h"

#ifndef WINSCP
#include "help.h"
#else
typedef const char *HelpCtx;
#define HELPCTX(x) #x
#endif

#if defined _M_IX86 || defined _M_AMD64
#define BUILDINFO_PLATFORM "x86 Windows"
#elif defined _M_ARM || defined _M_ARM64
#define BUILDINFO_PLATFORM "Arm Windows"
#else
#define BUILDINFO_PLATFORM "Windows"
#endif

#if defined __GNUC__ || defined __clang__
#define THREADLOCAL __thread
#elif defined _MSC_VER
#define THREADLOCAL __declspec(thread)
#else
#ifdef WINSCP
#define THREADLOCAL __thread
#else
#error Do not know how to declare thread-local storage with this toolchain
#endif
#endif

/* Randomly-chosen dwData value identifying a WM_COPYDATA message as
 * being a Pageant transaction */
#define AGENT_COPYDATA_ID 0x804e50ba

struct Filename {
    /*
     * A Windows Filename stores a path in three formats:
     *
     *  - wchar_t (in Windows UTF-16 encoding). The best format to use
     *    for actual file API functions, because all legal Windows
     *    file names are representable.
     *
     *  - char, in the system default codepage. A fallback to use if
     *    necessary, e.g. in diagnostics written to somewhere that is
     *    unavoidably encoded _in_ the system codepage.
     *
     *  - char, in UTF-8. An equally general representation to wpath,
     *    but suitable for keeping in char-typed strings.
     */
    wchar_t *wpath;
    char *cpath, *utf8path;
};
void ptrace(const char* msg);
Filename *filename_from_wstr(const wchar_t *str);
const wchar_t *filename_to_wstr(const Filename *fn);
FILE *f_open(const Filename *filename, const char *mode, bool isprivate);

#ifndef SUPERSEDE_FONTSPEC_FOR_TESTING
struct FontSpec {
    char *name;
    bool isbold;
    int height;
    int charset;
};
struct FontSpec *fontspec_new(
    const char *name, bool bold, int height, int charset);
#endif

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

#if !HAVE_STRTOUMAX
/* Work around lack of strtoumax in older MSVC libraries */
static inline uintmax_t strtoumax(const char *nptr, char **endptr, int base)
{ return _strtoui64(nptr, endptr, base); }
#endif

typedef INT_PTR (*ShinyDlgProc)(HWND hwnd, UINT msg, WPARAM wParam,
                                LPARAM lParam, void *ctx);
int ShinyDialogBox(HINSTANCE hinst, LPCTSTR tmpl, const char *winclass,
                   HWND hwndparent, ShinyDlgProc proc, void *ctx);
void ShinyEndDialog(HWND hwnd, int ret);

void centre_window(HWND hwnd);

#ifndef __WINE__
#ifdef MPEXT
/* use them as is in bcb */
#else
/* Up-to-date Windows headers warn that the unprefixed versions of
 * these names are deprecated. */
#define stricmp _stricmp
#define strnicmp _strnicmp
#endif
#else
/* Compiling with winegcc, _neither_ version of these functions
 * exists. Use the POSIX names. */
#define stricmp strcasecmp
#define strnicmp strncasecmp
#endif

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
/* If you DECL_WINDOWS_FUNCTION as extern in a header file, use this to
 * define the function pointer in a source file */
#define DEF_WINDOWS_FUNCTION(name) t_##name p_##name
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
#define TICKSPERSEC 1000               /* GetTickCount returns milliseconds */

#define DEFAULT_CODEPAGE CP_ACP
#define USES_VTLINE_HACK
#define CP_UTF8 65001
#define CP_437 437                     /* used for test suites */
#define CP_ISO8859_1 0x10001           /* used for test suites */

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
 * The all-important instance handle, saved from WinMain in every GUI
 * program and exported for other GUI code to pass back to the Windows
 * API.
 */
extern HINSTANCE hinst;

/*
 * Help file stuff in help.c.
 */
void init_help(void);
void shutdown_help(void);
bool has_help(void);
void launch_help(HWND hwnd, const char *topic);
void quit_help(HWND hwnd);
int has_embedded_chm(void);            /* 1 = yes, 0 = no, -1 = N/A */

#ifndef WINSCP
/*
 * GUI seat methods in dialog.c, so that the vtable definition in
 * window.c can refer to them.
 */
SeatPromptResult win_seat_confirm_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, SeatDialogText *text, HelpCtx helpctx,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
SeatPromptResult win_seat_confirm_weak_crypto_primitive(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
SeatPromptResult win_seat_confirm_weak_cached_hostkey(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
const SeatDialogPromptDescriptions *win_seat_prompt_descriptions(Seat *seat);
#endif

/*
 * Windows-specific clipboard helper function shared with dialog.c,
 * which takes the data string in the system code page instead of
 * Unicode.
 */
void write_aclip(HWND hwnd, int clipboard, char *, int);

#define WM_NETEVENT  (WM_APP + 5)

/*
 * On Windows, we send MA_2CLK as the only event marking the second
 * press of a mouse button. Compare unix/platform.h.
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
 * implementation of XDM-AUTHORIZATION-1 in ssh/x11fwd.c :-)
 */
#define sk_getxdmdata(socket, lenp) (NULL)

/*
 * Exports from network.c.
 */
/* Report an event notification from WSA*Select */
void select_result(WPARAM, LPARAM);
/* Enumerate all currently live OS-level SOCKETs */
SOCKET first_socket(int *);
SOCKET next_socket(int *);
/* Ask network.c whether we currently want to try to write to a SOCKET */
bool socket_writable(SOCKET skt);
/* Force a refresh of the SOCKET list by re-calling do_select for each one */
void socket_reselect_all(void);
/* Make a SockAddr which just holds a named pipe address. */
SockAddr *sk_namedpipe_addr(const char *pipename);
/* Turn a WinSock error code into a string. */
const char *winsock_error_string(int error);
Socket *sk_newlistener_unix(const char *socketpath, Plug *plug);

/*
 * network.c dynamically loads WinSock 2 or WinSock 1 depending on
 * what it can get, which means any WinSock routines used outside
 * that module must be exported from it as function pointers. So
 * here they are.
 */
DECL_WINDOWS_FUNCTION(extern, int, WSAAsyncSelect,
                      (SOCKET, HWND, u_int, LONG));
DECL_WINDOWS_FUNCTION(extern, int, WSAEventSelect,
                      (SOCKET, WSAEVENT, LONG));
DECL_WINDOWS_FUNCTION(extern, int, WSAGetLastError, (void));
DECL_WINDOWS_FUNCTION(extern, int, WSAEnumNetworkEvents,
                      (SOCKET, WSAEVENT, LPWSANETWORKEVENTS));
#ifdef NEED_DECLARATION_OF_SELECT
/* This declaration is protected by an ifdef for the sake of building
 * against winelib, in which you have to include winsock2.h before
 * stdlib.h so that the right fd_set type gets defined. It would be a
 * pain to do that throughout this codebase, so instead I arrange that
 * only a modules actually needing to use (or define, or initialise)
 * this function pointer will see its declaration, and _those_ modules
 * - which will be Windows-specific anyway - can take more care. */
DECL_WINDOWS_FUNCTION(extern, int, select,
                      (int, fd_set FAR *, fd_set FAR *,
                       fd_set FAR *, const struct timeval FAR *));
#endif

/*
 * Implemented differently depending on the client of network.c, and
 * called by network.c to turn on or off WSA*Select for a given socket.
 */
const char *do_select(Plug * plug, SOCKET skt, bool enable); // WINSCP

/*
 * Exports from select-{gui,cli}.c, each of which provides an
 * implementation of do_select.
 */
void winselgui_set_hwnd(HWND hwnd);
void winselgui_clear_hwnd(void);
void winselgui_response(WPARAM wParam, LPARAM lParam);

void winselcli_setup(void);
SOCKET winselcli_unique_socket(void);
extern HANDLE winselcli_event;

/*
 * Network-subsystem-related functions provided in other Windows modules.
 */
Socket *make_handle_socket(HANDLE send_H, HANDLE recv_H, HANDLE stderr_H,
                           SockAddr *addr, int port, Plug *plug,
                           bool overlapped); /* winhsock */
Socket *make_deferred_handle_socket(DeferredSocketOpener *opener,
                                    SockAddr *addr, int port, Plug *plug);
void setup_handle_socket(Socket *s, HANDLE send_H, HANDLE recv_H,
                         HANDLE stderr_H, bool overlapped);
void handle_socket_set_psb_prefix(Socket *s, const char *prefix);
Socket *new_named_pipe_client(const char *pipename, Plug *plug); /* winnpc */
Socket *new_named_pipe_listener(const char *pipename, Plug *plug); /* winnps */

/* A lower-level function in named-pipe-client.c, which does most of
 * the work of new_named_pipe_client (including checking the ownership
 * of what it's connected to), but returns a plain HANDLE instead of
 * wrapping it into a Socket. */
HANDLE connect_to_named_pipe(const char *pipename, char **err);

/*
 * Exports from controls.c.
 */

struct ctlpos {
    HWND hwnd;
    WPARAM font;
    int dlu4inpix;
    int ypos, width;
    int xoff;
    int boxystart, boxid;
    const char *boxtext;
};
void init_common_controls(void);       /* also does some DLL-loading */

/*
 * Exports from utils.
 */
typedef struct filereq_saved_dir filereq_saved_dir;
filereq_saved_dir *filereq_saved_dir_new(void);
void filereq_saved_dir_free(filereq_saved_dir *state);
Filename *request_file(
    HWND hwnd, const char *title, Filename *initial, bool save,
    filereq_saved_dir *dir, bool preserve_cwd, FilereqFilter filter);
struct request_multi_file_return {
    Filename **filenames;
    size_t nfilenames;
};
struct request_multi_file_return *request_multi_file(
    HWND hwnd, const char *title, Filename *initial, bool save,
    filereq_saved_dir *dir, bool preserve_cwd, FilereqFilter filter);
void request_multi_file_free(struct request_multi_file_return *);

void pgp_fingerprints_msgbox(HWND owner);
int message_box(HWND owner, LPCTSTR text, LPCTSTR caption, DWORD style,
                bool utf8, DWORD helpctxid);
void MakeDlgItemBorderless(HWND parent, int id);
char *GetDlgItemText_alloc(HWND hwnd, int id);
wchar_t *GetDlgItemTextW_alloc(HWND hwnd, int id);
/*
 * The split_into_argv functions take a single string 'cmdline' (char
 * or wide) to split up into arguments. They return an argc and argv
 * pair, and also 'argstart', an array of pointers into the original
 * command line, pointing at the place where each output argument
 * begins. (Useful for retrieving the tail of the original command
 * line corresponding to a certain argument onwards, or identifying a
 * section of the original command line to blank out for privacy.)
 *
 * If the command line includes the program name (e.g. if it was
 * returned from GetCommandLine()), set includes_program_name=true. If
 * it doesn't (e.g. it was the arguments string received by WinMain),
 * set that flag to false. This affects the rules for argument
 * splitting, which is done differently in the program name
 * (specifically, \ isn't special, and won't escape ").
 *
 * Mutability: the argv[] words are in fresh dynamically allocated
 * memory, so you can write into them safely. The original cmdline is
 * passed in as a const pointer, and not modified in this function.
 * But the pointers into that string written into argstart have the
 * type of a mutable char *. Similarly to strchr, this is due to the
 * limitation of C that you can't specify argstart as having the same
 * constness as cmdline: the idea is that you either pass a
 * non-mutable cmdline and promise not to write through the argstart
 * pointers, of you pass a mutable one and are free to write through
 * it.
 *
 * Allocation: argv and argstart are dynamically allocated. There's
 * also a dynamically allocated string behind the scenes storing the
 * actual strings. argv[0] guarantees to point at the first character
 * of that. So to free all the memory allocated by this function, you
 * must free argv[0], then argv, and also argstart.
 */
void split_into_argv(const char *cmdline, bool includes_program_name,
                     int *argc, char ***argv, char ***argstart);
void split_into_argv_w(const wchar_t *cmdline, bool includes_program_name,
                       int *argc, wchar_t ***argv, wchar_t ***argstart);

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
    HWND hwnd;                         /* the hwnd of the dialog box */
    struct winctrls *controltrees[8];  /* can have several of these */
    int nctrltrees;
    char *wintitle;                    /* title of actual window */
    char *errtitle;                    /* title of error sub-messageboxes */
    void *data;                        /* data to pass in refresh events */
    dlgcontrol *focused, *lastfocused; /* which ctrl has focus now/before */
    bool shortcuts[128];               /* track which shortcuts in use */
    bool coloursel_wanted;             /* has an event handler asked for
                                        * a colour selector? */
    struct {
        unsigned char r, g, b;         /* 0-255 */
        bool ok;
    } coloursel_result;
    tree234 *privdata;                 /* stores per-control private data */
    bool ended;                        /* has the dialog been ended? */
    int endresult;                     /* and if so, what was the result? */
    bool fixed_pitch_fonts;            /* are we constrained to fixed fonts? */
};

/*
 * Exports from controls.c.
 */
void ctlposinit(struct ctlpos *cp, HWND hwnd,
                int leftborder, int rightborder, int topborder);
HWND doctl(struct ctlpos *cp, RECT r, const char *wclass, int wstyle,
           int exstyle, const char *wtext, int wid);
void bartitle(struct ctlpos *cp, const char *name, int id);
void beginbox(struct ctlpos *cp, const char *name, int idbox);
void endbox(struct ctlpos *cp);
void editboxfw(struct ctlpos *cp, bool password, bool readonly,
               const char *text, int staticid, int editid);
void radioline(struct ctlpos *cp, const char *text, int id, int nacross, ...);
void bareradioline(struct ctlpos *cp, int nacross, ...);
void radiobig(struct ctlpos *cp, const char *text, int id, ...);
void checkbox(struct ctlpos *cp, const char *text, int id);
void button(struct ctlpos *cp, const char *btext, int bid, bool defbtn);
void statictext(struct ctlpos *cp, const char *text, int lines, int id);
void staticbtn(struct ctlpos *cp, const char *stext, int sid,
               const char *btext, int bid);
void static2btn(struct ctlpos *cp, const char *stext, int sid,
                const char *btext1, int bid1, const char *btext2, int bid2);
void staticedit(struct ctlpos *cp, const char *stext,
                int sid, int eid, int percentedit);
void staticddl(struct ctlpos *cp, const char *stext,
               int sid, int lid, int percentlist);
void combobox(struct ctlpos *cp, const char *text, int staticid, int listid);
void staticpassedit(struct ctlpos *cp, const char *stext,
                    int sid, int eid, int percentedit);
void bigeditctrl(struct ctlpos *cp, const char *stext,
                 int sid, int eid, int lines);
void ersatztab(struct ctlpos *cp, const char *stext, int sid, int lid,
               int s2id);
void editbutton(struct ctlpos *cp, const char *stext, int sid,
                int eid, const char *btext, int bid);
void sesssaver(struct ctlpos *cp, const char *text,
               int staticid, int editid, int listid, ...);
void envsetter(struct ctlpos *cp, const char *stext, int sid,
               const char *e1stext, int e1sid, int e1id,
               const char *e2stext, int e2sid, int e2id,
               int listid, const char *b1text, int b1id,
               const char *b2text, int b2id);
void charclass(struct ctlpos *cp, const char *stext, int sid, int listid,
               const char *btext, int bid, int eid, const char *s2text,
               int s2id);
void colouredit(struct ctlpos *cp, const char *stext, int sid, int listid,
                const char *btext, int bid, ...);
void prefslist(struct prefslist *hdl, struct ctlpos *cp, int lines,
               const char *stext, int sid, int listid, int upbid, int dnbid);
int handle_prefslist(struct prefslist *hdl,
                     int *array, int maxmemb,
                     bool is_dlmsg, HWND hwnd,
                     WPARAM wParam, LPARAM lParam);
void progressbar(struct ctlpos *cp, int id);
void fwdsetter(struct ctlpos *cp, int listid, const char *stext, int sid,
               const char *e1stext, int e1sid, int e1id,
               const char *e2stext, int e2sid, int e2id,
               const char *btext, int bid,
               const char *r1text, int r1id, const char *r2text, int r2id);

void dlg_auto_set_fixed_pitch_flag(dlgparam *dlg);
bool dlg_get_fixed_pitch_flag(dlgparam *dlg);
void dlg_set_fixed_pitch_flag(dlgparam *dlg, bool flag);

#define MAX_SHORTCUTS_PER_CTRL 16

/*
 * This structure is what's stored for each `dlgcontrol' in the
 * portable-dialog interface.
 */
struct winctrl {
    dlgcontrol *ctrl;
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
     * For vertical alignment, the id of a particular representative
     * control that has the y-extent of the sensible part of the
     * control.
     */
    int align_id;
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
 * tree234s so that it can find an item by `dlgcontrol' or by
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
struct winctrl *winctrl_findbyctrl(struct winctrls *, dlgcontrol *);
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
 * Exports from config.c.
 */
void win_setup_config_box(struct controlbox *b, HWND *hwndp, bool has_help,
                          bool midsession, int protocol);

/*
 * Exports from dialog.c.
 */
void defuse_showwindow(void);
bool do_config(Conf *);
bool do_reconfig(HWND, Conf *, int);
void showeventlog(HWND);
void showabout(HWND);
void force_normal(HWND hwnd);
void modal_about_box(HWND hwnd);
void show_help(HWND hwnd);
HWND event_log_window(void);

/*
 * Exports from utils.
 */
extern DWORD osMajorVersion, osMinorVersion, osPlatformId;
void init_winver(void);
void dll_hijacking_protection(void);
const char *get_system_dir(void);
HMODULE load_system32_dll(const char *libname);
const char *win_strerror(int error);
bool should_have_security(void);
void restrict_process_acl(void);
bool restricted_acl(void);
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
void init_ucs(Conf *, struct unicode_data *);

/*
 * Exports from handle-io.c.
 */
#define HANDLE_FLAG_OVERLAPPED 1
#define HANDLE_FLAG_IGNOREEOF 2
#define HANDLE_FLAG_UNITBUFFER 4
struct handle;
typedef size_t (*handle_inputfn_t)(
    struct handle *h, const void *data, size_t len, int err);
typedef void (*handle_outputfn_t)(
    struct handle *h, size_t new_backlog, int err, bool close);
struct handle *handle_input_new(struct callback_set * callback_set, HANDLE handle, handle_inputfn_t gotdata, // WINSCP
                                void *privdata, int flags);
struct handle *handle_output_new(struct callback_set * callback_set, HANDLE handle, handle_outputfn_t sentdata, // WINSCP
                                 void *privdata, int flags);
size_t handle_write(struct handle *h, const void *data, size_t len);
void handle_write_eof(struct handle *h);
void handle_free(struct handle *h); // WINSCP
void handle_unthrottle(struct handle *h, size_t backlog);
size_t handle_backlog(struct handle *h);
void *handle_get_privdata(struct handle *h);
/* Analogue of stdio_sink in marshal.h, for a Windows handle */
struct handle_sink {
    struct handle *h;
    BinarySink_IMPLEMENTATION;
};
void handle_sink_init(handle_sink *sink, struct handle *h);

/*
 * Exports from handle-wait.c.
 */
typedef struct HandleWait HandleWait;
typedef bool (*handle_wait_callback_fn_t)(struct callback_set * callback_set, void *); // WINSCP
HandleWait *add_handle_wait(struct callback_set * callback_set, HANDLE h, handle_wait_callback_fn_t callback,
                            void *callback_ctx);
void delete_handle_wait(struct callback_set * callback_set, HandleWait *hw);

typedef struct HandleWaitList {
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    int nhandles;
} HandleWaitList;
HandleWaitList *get_handle_wait_list(struct callback_set * callback_set);
bool handle_wait_activate(struct callback_set * callback_set, HandleWaitList *hwl, int index); // WINSCP
void handle_wait_list_free(HandleWaitList *hwl);

/*
 * Pageant-related pathnames.
 */
char *agent_mutex_name(void);
char *agent_named_pipe_name(void);

/*
 * Exports from serial.c.
 */
extern const struct BackendVtable serial_backend;

/*
 * Exports from jump-list.c.
 */
#define JUMPLIST_SUPPORTED             /* suppress #defines in putty.h */
void add_session_to_jumplist(const char * const sessionname);
void remove_session_from_jumplist(const char * const sessionname);
void clear_jumplist(void);
bool set_explicit_app_user_model_id(void);

/*
 * Exports from noise.c.
 */
bool win_read_random(void *buf, unsigned wanted); /* returns true on success */

/*
 * Extra functions in storage.c over and above the interface in
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

/* In utils */
int reg_override_winscp(void);
HKEY open_regkey_fn(bool create, bool write, HKEY base, const char *path, ...);
HKEY open_regkey_fn_winscp(bool create, bool write, HKEY base, const char *path, ...);
#define open_regkey_ro(base, ...) \
    reg_override_winscp() ? open_regkey_fn_winscp(false, false, base, __VA_ARGS__, (const char *)NULL) : open_regkey_fn(false, false, base, __VA_ARGS__, (const char *)NULL)
#define open_regkey_rw(base, ...) \
    DebugFail();
#define create_regkey(base, ...) \
    reg_override_winscp() ? open_regkey_fn_winscp(true, true, base, __VA_ARGS__, (const char *)NULL) : open_regkey_fn(true, true, base, __VA_ARGS__, (const char *)NULL)
void close_regkey(HKEY key);
void close_regkey_winscp(HKEY key);
void putty_registry_pass(bool enable);
void del_regkey(HKEY key, const char *name);
char *enum_regkey(HKEY key, int index);
bool get_reg_dword(HKEY key, const char *name, DWORD *out);
bool get_reg_dword_winscp(HKEY key, const char *name, DWORD *out);
bool put_reg_dword(HKEY key, const char *name, DWORD value);
bool put_reg_dword_winscp(HKEY key, const char *name, DWORD value);
char *get_reg_sz(HKEY key, const char *name);
char *get_reg_sz_winscp(HKEY key, const char *name);
bool put_reg_sz(HKEY key, const char *name, const char *str);
bool put_reg_sz_winscp(HKEY key, const char *name, const char *str);
strbuf *get_reg_multi_sz(HKEY key, const char *name);
bool put_reg_multi_sz(HKEY key, const char *name, strbuf *str);

char *get_reg_sz_simple(HKEY key, const char *name, const char *leaf);

/* In cliloop.c */
typedef bool (*cliloop_pre_t)(void *vctx, const HANDLE **extra_handles,
                              size_t *n_extra_handles);
typedef bool (*cliloop_post_t)(void *vctx, size_t extra_handle_index);
void cli_main_loop(cliloop_pre_t pre, cliloop_post_t post, void *ctx);
bool cliloop_null_pre(void *vctx, const HANDLE **, size_t *);
bool cliloop_null_post(void *vctx, size_t);

extern const struct BackendVtable conpty_backend;

/* Functions that parametrise window.c between PuTTY and pterm */
void gui_term_process_cmdline(Conf *conf, char *cmdline);
const struct BackendVtable *backend_vt_from_conf(Conf *conf);
const wchar_t *get_app_user_model_id(void);
/* And functions in window.c that those files call back to */
char *handle_restrict_acl_cmdline_prefix(char *cmdline);
bool handle_special_sessionname_cmdline(char *cmdline, Conf *conf);
bool handle_special_filemapping_cmdline(char *cmdline, Conf *conf);

/* network.c: network error reporting helpers taking OS error code */
void plug_closing_system_error(Plug *plug, DWORD error);
void plug_closing_winsock_error(Plug *plug, DWORD error);

SeatPromptResult make_spr_sw_abort_winerror(const char *prefix, DWORD error);

HANDLE lock_interprocess_mutex(const char *mutexname, char **error);
void unlock_interprocess_mutex(HANDLE mutex);

typedef void (*aux_opt_error_fn_t)(const char *, ...);
typedef struct AuxMatchOpt {
    CmdlineArgList *arglist;
    size_t index;
    bool doing_opts;
    aux_opt_error_fn_t error;
} AuxMatchOpt;
AuxMatchOpt aux_match_opt_init(aux_opt_error_fn_t opt_error);
bool aux_match_arg(AuxMatchOpt *amo, CmdlineArg **val);
bool aux_match_opt(AuxMatchOpt *amo, CmdlineArg **val,
                   const char *optname, ...);
bool aux_match_done(AuxMatchOpt *amo);

char *save_screenshot(HWND hwnd, Filename *outfile);
void gui_terminal_ready(HWND hwnd, Seat *seat, Backend *backend);

void setup_gui_timing(void);

/* Windows-specific extra functions in cmdline_arg.c */
CmdlineArgList *cmdline_arg_list_from_GetCommandLineW(void);
const wchar_t *cmdline_arg_remainder_wide(CmdlineArg *);
char *cmdline_arg_remainder_acp(CmdlineArg *);
char *cmdline_arg_remainder_utf8(CmdlineArg *);
CmdlineArg *cmdline_arg_from_utf8(CmdlineArgList *list, const char *string);

#endif /* PUTTY_WINDOWS_PLATFORM_H */
