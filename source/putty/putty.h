#ifndef PUTTY_PUTTY_H
#define PUTTY_PUTTY_H

#include <stddef.h>		       /* for wchar_t */

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

#ifndef DONE_TYPEDEFS
#define DONE_TYPEDEFS
typedef struct conf_tag Conf;
typedef struct backend_tag Backend;
typedef struct terminal_tag Terminal;
#endif

#include "puttyps.h"
#include "network.h"
#include "misc.h"

/*
 * Fingerprints of the PGP master keys that can be used to establish a trust
 * path between an executable and other files.
 */
#define PGP_MASTER_KEY_FP \
    "440D E3B5 B7A1 CA85 B3CC  1718 AB58 5DC6 0467 6F7C"
#define PGP_RSA_MASTER_KEY_FP \
    "8F 15 97 DA 25 30 AB 0D  88 D1 92 54 11 CF 0C 4C"
#define PGP_DSA_MASTER_KEY_FP \
    "313C 3E76 4B74 C2C5 F2AE  83A8 4F5E 6DF5 6A93 B34E"

/* Three attribute types: 
 * The ATTRs (normal attributes) are stored with the characters in
 * the main display arrays
 *
 * The TATTRs (temporary attributes) are generated on the fly, they
 * can overlap with characters but not with normal attributes.
 *
 * The LATTRs (line attributes) are an entirely disjoint space of
 * flags.
 * 
 * The DATTRs (display attributes) are internal to terminal.c (but
 * defined here because their values have to match the others
 * here); they reuse the TATTR_* space but are always masked off
 * before sending to the front end.
 *
 * ATTR_INVALID is an illegal colour combination.
 */

#define TATTR_ACTCURS 	    0x40000000UL      /* active cursor (block) */
#define TATTR_PASCURS 	    0x20000000UL      /* passive cursor (box) */
#define TATTR_RIGHTCURS	    0x10000000UL      /* cursor-on-RHS */
#define TATTR_COMBINING	    0x80000000UL      /* combining characters */

#define DATTR_STARTRUN      0x80000000UL   /* start of redraw run */

#define TDATTR_MASK         0xF0000000UL
#define TATTR_MASK (TDATTR_MASK)
#define DATTR_MASK (TDATTR_MASK)

#define LATTR_NORM   0x00000000UL
#define LATTR_WIDE   0x00000001UL
#define LATTR_TOP    0x00000002UL
#define LATTR_BOT    0x00000003UL
#define LATTR_MODE   0x00000003UL
#define LATTR_WRAPPED 0x00000010UL     /* this line wraps to next */
#define LATTR_WRAPPED2 0x00000020UL    /* with WRAPPED: CJK wide character
					  wrapped to next line, so last
					  single-width cell is empty */

#define ATTR_INVALID 0x03FFFFU

/* Like Linux use the F000 page for direct to font. */
#define CSET_OEMCP   0x0000F000UL      /* OEM Codepage DTF */
#define CSET_ACP     0x0000F100UL      /* Ansi Codepage DTF */

/* These are internal use overlapping with the UTF-16 surrogates */
#define CSET_ASCII   0x0000D800UL      /* normal ASCII charset ESC ( B */
#define CSET_LINEDRW 0x0000D900UL      /* line drawing charset ESC ( 0 */
#define CSET_SCOACS  0x0000DA00UL      /* SCO Alternate charset */
#define CSET_GBCHR   0x0000DB00UL      /* UK variant   charset ESC ( A */
#define CSET_MASK    0xFFFFFF00UL      /* Character set mask */

#define DIRECT_CHAR(c) ((c&0xFFFFFC00)==0xD800)
#define DIRECT_FONT(c) ((c&0xFFFFFE00)==0xF000)

#define UCSERR	     (CSET_LINEDRW|'a')	/* UCS Format error character. */
/*
 * UCSWIDE is a special value used in the terminal data to signify
 * the character cell containing the right-hand half of a CJK wide
 * character. We use 0xDFFF because it's part of the surrogate
 * range and hence won't be used for anything else (it's impossible
 * to input it via UTF-8 because our UTF-8 decoder correctly
 * rejects surrogates).
 */
#define UCSWIDE	     0xDFFF

#define ATTR_NARROW  0x800000U
#define ATTR_WIDE    0x400000U
#define ATTR_BOLD    0x040000U
#define ATTR_UNDER   0x080000U
#define ATTR_REVERSE 0x100000U
#define ATTR_BLINK   0x200000U
#define ATTR_FGMASK  0x0001FFU
#define ATTR_BGMASK  0x03FE00U
#define ATTR_COLOURS 0x03FFFFU
#define ATTR_FGSHIFT 0
#define ATTR_BGSHIFT 9

/*
 * The definitive list of colour numbers stored in terminal
 * attribute words is kept here. It is:
 * 
 *  - 0-7 are ANSI colours (KRGYBMCW).
 *  - 8-15 are the bold versions of those colours.
 *  - 16-255 are the remains of the xterm 256-colour mode (a
 *    216-colour cube with R at most significant and B at least,
 *    followed by a uniform series of grey shades running between
 *    black and white but not including either on grounds of
 *    redundancy).
 *  - 256 is default foreground
 *  - 257 is default bold foreground
 *  - 258 is default background
 *  - 259 is default bold background
 *  - 260 is cursor foreground
 *  - 261 is cursor background
 */

#define ATTR_DEFFG   (256 << ATTR_FGSHIFT)
#define ATTR_DEFBG   (258 << ATTR_BGSHIFT)
#define ATTR_DEFAULT (ATTR_DEFFG | ATTR_DEFBG)

struct sesslist {
    int nsessions;
    char **sessions;
    char *buffer;		       /* so memory can be freed later */
};

struct unicode_data {
    char **uni_tbl;
    int dbcs_screenfont;
    int font_codepage;
    int line_codepage;
    wchar_t unitab_scoacs[256];
    wchar_t unitab_line[256];
    wchar_t unitab_font[256];
    wchar_t unitab_xterm[256];
    wchar_t unitab_oemcp[256];
    unsigned char unitab_ctrl[256];
};

#define LGXF_OVR  1		       /* existing logfile overwrite */
#define LGXF_APN  0		       /* existing logfile append */
#define LGXF_ASK -1		       /* existing logfile ask */
#define LGTYP_NONE  0		       /* logmode: no logging */
#define LGTYP_ASCII 1		       /* logmode: pure ascii */
#define LGTYP_DEBUG 2		       /* logmode: all chars of traffic */
#define LGTYP_PACKETS 3		       /* logmode: SSH data packets */
#define LGTYP_SSHRAW 4		       /* logmode: SSH raw data */

typedef enum {
    /* Actual special commands. Originally Telnet, but some codes have
     * been re-used for similar specials in other protocols. */
    TS_AYT, TS_BRK, TS_SYNCH, TS_EC, TS_EL, TS_GA, TS_NOP, TS_ABORT,
    TS_AO, TS_IP, TS_SUSP, TS_EOR, TS_EOF, TS_LECHO, TS_RECHO, TS_PING,
    TS_EOL,
    /* Special command for SSH. */
    TS_REKEY,
    /* POSIX-style signals. (not Telnet) */
    TS_SIGABRT, TS_SIGALRM, TS_SIGFPE,  TS_SIGHUP,  TS_SIGILL,
    TS_SIGINT,  TS_SIGKILL, TS_SIGPIPE, TS_SIGQUIT, TS_SIGSEGV,
    TS_SIGTERM, TS_SIGUSR1, TS_SIGUSR2,
    /* Pseudo-specials used for constructing the specials menu. */
    TS_SEP,	    /* Separator */
    TS_SUBMENU,	    /* Start a new submenu with specified name */
    TS_EXITMENU	    /* Exit current submenu or end of specials */
} Telnet_Special;

struct telnet_special {
    const char *name;
    int code;
};

typedef enum {
    MBT_NOTHING,
    MBT_LEFT, MBT_MIDDLE, MBT_RIGHT,   /* `raw' button designations */
    MBT_SELECT, MBT_EXTEND, MBT_PASTE, /* `cooked' button designations */
    MBT_WHEEL_UP, MBT_WHEEL_DOWN       /* mouse wheel */
} Mouse_Button;

typedef enum {
    MA_NOTHING, MA_CLICK, MA_2CLK, MA_3CLK, MA_DRAG, MA_RELEASE
} Mouse_Action;

/* Keyboard modifiers -- keys the user is actually holding down */

#define PKM_SHIFT	0x01
#define PKM_CONTROL	0x02
#define PKM_META	0x04
#define PKM_ALT		0x08

/* Keyboard flags that aren't really modifiers */
#define PKF_CAPSLOCK	0x10
#define PKF_NUMLOCK	0x20
#define PKF_REPEAT	0x40

/* Stand-alone keysyms for function keys */

typedef enum {
    PK_NULL,		/* No symbol for this key */
    /* Main keypad keys */
    PK_ESCAPE, PK_TAB, PK_BACKSPACE, PK_RETURN, PK_COMPOSE,
    /* Editing keys */
    PK_HOME, PK_INSERT, PK_DELETE, PK_END, PK_PAGEUP, PK_PAGEDOWN,
    /* Cursor keys */
    PK_UP, PK_DOWN, PK_RIGHT, PK_LEFT, PK_REST,
    /* Numeric keypad */			/* Real one looks like: */
    PK_PF1, PK_PF2, PK_PF3, PK_PF4,		/* PF1 PF2 PF3 PF4 */
    PK_KPCOMMA, PK_KPMINUS, PK_KPDECIMAL,	/*  7   8   9   -  */
    PK_KP0, PK_KP1, PK_KP2, PK_KP3, PK_KP4,	/*  4   5   6   ,  */
    PK_KP5, PK_KP6, PK_KP7, PK_KP8, PK_KP9,	/*  1   2   3  en- */
    PK_KPBIGPLUS, PK_KPENTER,			/*    0     .  ter */
    /* Top row */
    PK_F1,  PK_F2,  PK_F3,  PK_F4,  PK_F5,
    PK_F6,  PK_F7,  PK_F8,  PK_F9,  PK_F10,
    PK_F11, PK_F12, PK_F13, PK_F14, PK_F15,
    PK_F16, PK_F17, PK_F18, PK_F19, PK_F20,
    PK_PAUSE
} Key_Sym;

#define PK_ISEDITING(k)	((k) >= PK_HOME && (k) <= PK_PAGEDOWN)
#define PK_ISCURSOR(k)	((k) >= PK_UP && (k) <= PK_REST)
#define PK_ISKEYPAD(k)	((k) >= PK_PF1 && (k) <= PK_KPENTER)
#define PK_ISFKEY(k)	((k) >= PK_F1 && (k) <= PK_F20)

enum {
    VT_XWINDOWS, VT_OEMANSI, VT_OEMONLY, VT_POORMAN, VT_UNICODE
};

enum {
    /*
     * SSH-2 key exchange algorithms
     */
    KEX_WARN,
    KEX_DHGROUP1,
    KEX_DHGROUP14,
    KEX_DHGEX,
    KEX_RSA,
    KEX_MAX
};

enum {
    /*
     * SSH ciphers (both SSH-1 and SSH-2)
     */
    CIPHER_WARN,		       /* pseudo 'cipher' */
    CIPHER_3DES,
    CIPHER_BLOWFISH,
    CIPHER_AES,			       /* (SSH-2 only) */
    CIPHER_DES,
    CIPHER_ARCFOUR,
    CIPHER_MAX			       /* no. ciphers (inc warn) */
};

enum {
    /*
     * Several different bits of the PuTTY configuration seem to be
     * three-way settings whose values are `always yes', `always
     * no', and `decide by some more complex automated means'. This
     * is true of line discipline options (local echo and line
     * editing), proxy DNS, Close On Exit, and SSH server bug
     * workarounds. Accordingly I supply a single enum here to deal
     * with them all.
     */
    FORCE_ON, FORCE_OFF, AUTO
};

enum {
    /*
     * Proxy types.
     */
    PROXY_NONE, PROXY_SOCKS4, PROXY_SOCKS5,
    PROXY_HTTP, PROXY_TELNET, PROXY_CMD
};

enum {
    /*
     * Line discipline options which the backend might try to control.
     */
    LD_EDIT,			       /* local line editing */
    LD_ECHO			       /* local echo */
};

enum {
    /* Actions on remote window title query */
    TITLE_NONE, TITLE_EMPTY, TITLE_REAL
};

enum {
    /* Protocol back ends. (CONF_protocol) */
    PROT_RAW, PROT_TELNET, PROT_RLOGIN, PROT_SSH,
    /* PROT_SERIAL is supported on a subset of platforms, but it doesn't
     * hurt to define it globally. */
    PROT_SERIAL
};

enum {
    /* Bell settings (CONF_beep) */
    BELL_DISABLED, BELL_DEFAULT, BELL_VISUAL, BELL_WAVEFILE, BELL_PCSPEAKER
};

enum {
    /* Taskbar flashing indication on bell (CONF_beep_ind) */
    B_IND_DISABLED, B_IND_FLASH, B_IND_STEADY
};

enum {
    /* Resize actions (CONF_resize_action) */
    RESIZE_TERM, RESIZE_DISABLED, RESIZE_FONT, RESIZE_EITHER
};

enum {
    /* Function key types (CONF_funky_type) */
    FUNKY_TILDE,
    FUNKY_LINUX,
    FUNKY_XTERM,
    FUNKY_VT400,
    FUNKY_VT100P,
    FUNKY_SCO
};

enum {
    FQ_DEFAULT, FQ_ANTIALIASED, FQ_NONANTIALIASED, FQ_CLEARTYPE
};

enum {
    SER_PAR_NONE, SER_PAR_ODD, SER_PAR_EVEN, SER_PAR_MARK, SER_PAR_SPACE
};

enum {
    SER_FLOW_NONE, SER_FLOW_XONXOFF, SER_FLOW_RTSCTS, SER_FLOW_DSRDTR
};

/*
 * Tables of string <-> enum value mappings used in settings.c.
 * Defined here so that backends can export their GSS library tables
 * to the cross-platform settings code.
 */
struct keyvalwhere {
    /*
     * Two fields which define a string and enum value to be
     * equivalent to each other.
     */
    char *s;
    int v;

    /*
     * The next pair of fields are used by gprefs() in settings.c to
     * arrange that when it reads a list of strings representing a
     * preference list and translates it into the corresponding list
     * of integers, strings not appearing in the list are entered in a
     * configurable position rather than uniformly at the end.
     */

    /*
     * 'vrel' indicates which other value in the list to place this
     * element relative to. It should be a value that has occurred in
     * a 'v' field of some other element of the array, or -1 to
     * indicate that we simply place relative to one or other end of
     * the list.
     *
     * gprefs will try to process the elements in an order which makes
     * this field work (i.e. so that the element referenced has been
     * added before processing this one).
     */
    int vrel;

    /*
     * 'where' indicates whether to place the new value before or
     * after the one referred to by vrel. -1 means before; +1 means
     * after.
     *
     * When vrel is -1, this also implicitly indicates which end of
     * the array to use. So vrel=-1, where=-1 means to place _before_
     * some end of the list (hence, at the last element); vrel=-1,
     * where=+1 means to place _after_ an end (hence, at the first).
     */
    int where;
};

#ifndef NO_GSSAPI
extern const int ngsslibs;
extern const char *const gsslibnames[]; /* for displaying in configuration */
extern const struct keyvalwhere gsslibkeywords[]; /* for settings.c */
#endif

extern const char *const ttymodes[];

enum {
    /*
     * Network address types. Used for specifying choice of IPv4/v6
     * in config; also used in proxy.c to indicate whether a given
     * host name has already been resolved or will be resolved at
     * the proxy end.
     */
    ADDRTYPE_UNSPEC, ADDRTYPE_IPV4, ADDRTYPE_IPV6, ADDRTYPE_NAME
};

struct backend_tag {
    const char *(*init) (void *frontend_handle, void **backend_handle,
			 Conf *conf, char *host, int port, char **realhost,
			 int nodelay, int keepalive);
    void (*free) (void *handle);
    /* back->reconfig() passes in a replacement configuration. */
    void (*reconfig) (void *handle, Conf *conf);
    /* back->send() returns the current amount of buffered data. */
    int (*send) (void *handle, char *buf, int len);
    /* back->sendbuffer() does the same thing but without attempting a send */
    int (*sendbuffer) (void *handle);
    void (*size) (void *handle, int width, int height);
    void (*special) (void *handle, Telnet_Special code);
    const struct telnet_special *(*get_specials) (void *handle);
    int (*connected) (void *handle);
    int (*exitcode) (void *handle);
    /* If back->sendok() returns FALSE, data sent to it from the frontend
     * may be lost. */
    int (*sendok) (void *handle);
    int (*ldisc) (void *handle, int);
    void (*provide_ldisc) (void *handle, void *ldisc);
    void (*provide_logctx) (void *handle, void *logctx);
    /*
     * back->unthrottle() tells the back end that the front end
     * buffer is clearing.
     */
    void (*unthrottle) (void *handle, int);
    int (*cfg_info) (void *handle);
    char *name;
    int protocol;
    int default_port;
};

extern Backend *backends[];

/*
 * Suggested default protocol provided by the backend link module.
 * The application is free to ignore this.
 */
extern const int be_default_protocol;

/*
 * Name of this particular application, for use in the config box
 * and other pieces of text.
 */
extern const char *const appname;

/*
 * Some global flags denoting the type of application.
 * 
 * FLAG_VERBOSE is set when the user requests verbose details.
 * 
 * FLAG_STDERR is set in command-line applications (which have a
 * functioning stderr that it makes sense to write to) and not in
 * GUI applications (which don't).
 * 
 * FLAG_INTERACTIVE is set when a full interactive shell session is
 * being run, _either_ because no remote command has been provided
 * _or_ because the application is GUI and can't run non-
 * interactively.
 * 
 * These flags describe the type of _application_ - they wouldn't
 * vary between individual sessions - and so it's OK to have this
 * variable be GLOBAL.
 * 
 * Note that additional flags may be defined in platform-specific
 * headers. It's probably best if those ones start from 0x1000, to
 * avoid collision.
 */
#define FLAG_VERBOSE     0x0001
#define FLAG_STDERR      0x0002
#define FLAG_INTERACTIVE 0x0004
GLOBAL int flags;

/*
 * Likewise, these two variables are set up when the application
 * initialises, and inform all default-settings accesses after
 * that.
 */
GLOBAL int default_protocol;
GLOBAL int default_port;

/*
 * This is set TRUE by cmdline.c iff a session is loaded with "-load".
 */
GLOBAL int loaded_session;
/*
 * This is set to the name of the loaded session.
 */
GLOBAL char *cmdline_session_name;

struct RSAKey;			       /* be a little careful of scope */

/*
 * Mechanism for getting text strings such as usernames and passwords
 * from the front-end.
 * The fields are mostly modelled after SSH's keyboard-interactive auth.
 * FIXME We should probably mandate a character set/encoding (probably UTF-8).
 *
 * Since many of the pieces of text involved may be chosen by the server,
 * the caller must take care to ensure that the server can't spoof locally-
 * generated prompts such as key passphrase prompts. Some ground rules:
 *  - If the front-end needs to truncate a string, it should lop off the
 *    end.
 *  - The front-end should filter out any dangerous characters and
 *    generally not trust the strings. (But \n is required to behave
 *    vaguely sensibly, at least in `instruction', and ideally in
 *    `prompt[]' too.)
 */
typedef struct {
    char *prompt;
    int echo;
    /*
     * 'result' must be a dynamically allocated array of exactly
     * 'resultsize' chars. The code for actually reading input may
     * realloc it bigger (and adjust resultsize accordingly) if it has
     * to. The caller should free it again when finished with it.
     *
     * If resultsize==0, then result may be NULL. When setting up a
     * prompt_t, it's therefore easiest to initialise them this way,
     * which means all actual allocation is done by the callee. This
     * is what add_prompt does.
     */
    char *result;
    size_t resultsize;
} prompt_t;
typedef struct {
    /*
     * Indicates whether the information entered is to be used locally
     * (for instance a key passphrase prompt), or is destined for the wire.
     * This is a hint only; the front-end is at liberty not to use this
     * information (so the caller should ensure that the supplied text is
     * sufficient).
     */
    int to_server;
    char *name;		/* Short description, perhaps for dialog box title */
    int name_reqd;	/* Display of `name' required or optional? */
    char *instruction;	/* Long description, maybe with embedded newlines */
    int instr_reqd;	/* Display of `instruction' required or optional? */
    size_t n_prompts;   /* May be zero (in which case display the foregoing,
                         * if any, and return success) */
    prompt_t **prompts;
    void *frontend;
    void *data;		/* slot for housekeeping data, managed by
			 * get_userpass_input(); initially NULL */
} prompts_t;
prompts_t *new_prompts(void *frontend);
void add_prompt(prompts_t *p, char *promptstr, int echo);
void prompt_set_result(prompt_t *pr, const char *newstr);
void prompt_ensure_result_size(prompt_t *pr, int len);
/* Burn the evidence. (Assumes _all_ strings want free()ing.) */
void free_prompts(prompts_t *p);

/*
 * Exports from the front end.
 */
void request_resize(void *frontend, int, int);
void do_text(Context, int, int, wchar_t *, int, unsigned long, int);
void do_cursor(Context, int, int, wchar_t *, int, unsigned long, int);
int char_width(Context ctx, int uc);
#ifdef OPTIMISE_SCROLL
void do_scroll(Context, int, int, int);
#endif
void set_title(void *frontend, char *);
void set_icon(void *frontend, char *);
void set_sbar(void *frontend, int, int, int);
Context get_ctx(void *frontend);
void free_ctx(Context);
void palette_set(void *frontend, int, int, int, int);
void palette_reset(void *frontend);
void write_aclip(void *frontend, char *, int, int);
void write_clip(void *frontend, wchar_t *, int *, int, int);
void get_clip(void *frontend, wchar_t **, int *);
void optimised_move(void *frontend, int, int, int);
void set_raw_mouse_mode(void *frontend, int);
void connection_fatal(void *frontend, char *, ...);
void nonfatal(char *, ...);
void fatalbox(char *, ...);
void modalfatalbox(char *, ...);
#ifdef macintosh
#pragma noreturn(fatalbox)
#pragma noreturn(modalfatalbox)
#endif
void do_beep(void *frontend, int);
void begin_session(void *frontend);
void sys_cursor(void *frontend, int x, int y);
void request_paste(void *frontend);
void frontend_keypress(void *frontend);
void ldisc_update(void *frontend, int echo, int edit);
/* It's the backend's responsibility to invoke this at the start of a
 * connection, if necessary; it can also invoke it later if the set of
 * special commands changes. It does not need to invoke it at session
 * shutdown. */
void update_specials_menu(void *frontend);
int from_backend(void *frontend, int is_stderr, const char *data, int len);
int from_backend_untrusted(void *frontend, const char *data, int len);
/* Called when the back end wants to indicate that EOF has arrived on
 * the server-to-client stream. Returns FALSE to indicate that we
 * intend to keep the session open in the other direction, or TRUE to
 * indicate that if they're closing so are we. */
int from_backend_eof(void *frontend);
void notify_remote_exit(void *frontend);
/* Get a sensible value for a tty mode. NULL return = don't set.
 * Otherwise, returned value should be freed by caller. */
char *get_ttymode(void *frontend, const char *mode);
/*
 * >0 = `got all results, carry on'
 * 0  = `user cancelled' (FIXME distinguish "give up entirely" and "next auth"?)
 * <0 = `please call back later with more in/inlen'
 */
int get_userpass_input(prompts_t *p, unsigned char *in, int inlen);
#define OPTIMISE_IS_SCROLL 1

void set_iconic(void *frontend, int iconic);
void move_window(void *frontend, int x, int y);
void set_zorder(void *frontend, int top);
void refresh_window(void *frontend);
void set_zoomed(void *frontend, int zoomed);
int is_iconic(void *frontend);
void get_window_pos(void *frontend, int *x, int *y);
void get_window_pixels(void *frontend, int *x, int *y);
char *get_window_title(void *frontend, int icon);
/* Hint from backend to frontend about time-consuming operations.
 * Initial state is assumed to be BUSY_NOT. */
enum {
    BUSY_NOT,	    /* Not busy, all user interaction OK */
    BUSY_WAITING,   /* Waiting for something; local event loops still running
		       so some local interaction (e.g. menus) OK, but network
		       stuff is suspended */
    BUSY_CPU	    /* Locally busy (e.g. crypto); user interaction suspended */
};
void set_busy_status(void *frontend, int status);

void cleanup_exit(int);

/*
 * Exports from conf.c, and a big enum (via parametric macro) of
 * configuration option keys.
 */
#define CONFIG_OPTIONS(X) \
    /* X(value-type, subkey-type, keyword) */ \
    X(STR, NONE, host) \
    X(INT, NONE, port) \
    X(INT, NONE, protocol) \
    X(INT, NONE, addressfamily) \
    X(INT, NONE, close_on_exit) \
    X(INT, NONE, warn_on_close) \
    X(INT, NONE, ping_interval) /* in seconds */ \
    X(INT, NONE, tcp_nodelay) \
    X(INT, NONE, tcp_keepalives) \
    X(STR, NONE, loghost) /* logical host being contacted, for host key check */ \
    /* Proxy options */ \
    X(STR, NONE, proxy_exclude_list) \
    X(INT, NONE, proxy_dns) \
    X(INT, NONE, even_proxy_localhost) \
    X(INT, NONE, proxy_type) \
    X(STR, NONE, proxy_host) \
    X(INT, NONE, proxy_port) \
    X(STR, NONE, proxy_username) \
    X(STR, NONE, proxy_password) \
    X(STR, NONE, proxy_telnet_command) \
    /* SSH options */ \
    X(STR, NONE, remote_cmd) \
    X(STR, NONE, remote_cmd2) /* fallback if remote_cmd fails; never loaded or saved */ \
    X(INT, NONE, nopty) \
    X(INT, NONE, compression) \
    X(INT, INT, ssh_kexlist) \
    X(INT, NONE, ssh_rekey_time) /* in minutes */ \
    X(STR, NONE, ssh_rekey_data) /* string encoding e.g. "100K", "2M", "1G" */ \
    X(INT, NONE, tryagent) \
    X(INT, NONE, agentfwd) \
    X(INT, NONE, change_username) /* allow username switching in SSH-2 */ \
    X(INT, INT, ssh_cipherlist) \
    X(FILENAME, NONE, keyfile) \
    X(INT, NONE, sshprot) /* use v1 or v2 when both available */ \
    X(INT, NONE, ssh2_des_cbc) /* "des-cbc" unrecommended SSH-2 cipher */ \
    X(INT, NONE, ssh_no_userauth) /* bypass "ssh-userauth" (SSH-2 only) */ \
    X(INT, NONE, ssh_show_banner) /* show USERAUTH_BANNERs (SSH-2 only) */ \
    X(INT, NONE, try_tis_auth) \
    X(INT, NONE, try_ki_auth) \
    X(INT, NONE, try_gssapi_auth) /* attempt gssapi auth */ \
    X(INT, NONE, gssapifwd) /* forward tgt via gss */ \
    X(INT, INT, ssh_gsslist) /* preference order for local GSS libs */ \
    X(FILENAME, NONE, ssh_gss_custom) \
    X(INT, NONE, ssh_subsys) /* run a subsystem rather than a command */ \
    X(INT, NONE, ssh_subsys2) /* fallback to go with remote_cmd_ptr2 */ \
    X(INT, NONE, ssh_no_shell) /* avoid running a shell */ \
    X(STR, NONE, ssh_nc_host) /* host to connect to in `nc' mode */ \
    X(INT, NONE, ssh_nc_port) /* port to connect to in `nc' mode */ \
    /* Telnet options */ \
    X(STR, NONE, termtype) \
    X(STR, NONE, termspeed) \
    X(STR, STR, ttymodes) /* values are "Vvalue" or "A" */ \
    X(STR, STR, environmt) \
    X(STR, NONE, username) \
    X(INT, NONE, username_from_env) \
    X(STR, NONE, localusername) \
    X(INT, NONE, rfc_environ) \
    X(INT, NONE, passive_telnet) \
    /* Serial port options */ \
    X(STR, NONE, serline) \
    X(INT, NONE, serspeed) \
    X(INT, NONE, serdatabits) \
    X(INT, NONE, serstopbits) \
    X(INT, NONE, serparity) \
    X(INT, NONE, serflow) \
    /* Keyboard options */ \
    X(INT, NONE, bksp_is_delete) \
    X(INT, NONE, rxvt_homeend) \
    X(INT, NONE, funky_type) \
    X(INT, NONE, no_applic_c) /* totally disable app cursor keys */ \
    X(INT, NONE, no_applic_k) /* totally disable app keypad */ \
    X(INT, NONE, no_mouse_rep) /* totally disable mouse reporting */ \
    X(INT, NONE, no_remote_resize) /* disable remote resizing */ \
    X(INT, NONE, no_alt_screen) /* disable alternate screen */ \
    X(INT, NONE, no_remote_wintitle) /* disable remote retitling */ \
    X(INT, NONE, no_dbackspace) /* disable destructive backspace */ \
    X(INT, NONE, no_remote_charset) /* disable remote charset config */ \
    X(INT, NONE, remote_qtitle_action) /* remote win title query action */ \
    X(INT, NONE, app_cursor) \
    X(INT, NONE, app_keypad) \
    X(INT, NONE, nethack_keypad) \
    X(INT, NONE, telnet_keyboard) \
    X(INT, NONE, telnet_newline) \
    X(INT, NONE, alt_f4) /* is it special? */ \
    X(INT, NONE, alt_space) /* is it special? */ \
    X(INT, NONE, alt_only) /* is it special? */ \
    X(INT, NONE, localecho) \
    X(INT, NONE, localedit) \
    X(INT, NONE, alwaysontop) \
    X(INT, NONE, fullscreenonaltenter) \
    X(INT, NONE, scroll_on_key) \
    X(INT, NONE, scroll_on_disp) \
    X(INT, NONE, erase_to_scrollback) \
    X(INT, NONE, compose_key) \
    X(INT, NONE, ctrlaltkeys) \
    X(STR, NONE, wintitle) /* initial window title */ \
    /* Terminal options */ \
    X(INT, NONE, savelines) \
    X(INT, NONE, dec_om) \
    X(INT, NONE, wrap_mode) \
    X(INT, NONE, lfhascr) \
    X(INT, NONE, cursor_type) /* 0=block 1=underline 2=vertical */ \
    X(INT, NONE, blink_cur) \
    X(INT, NONE, beep) \
    X(INT, NONE, beep_ind) \
    X(INT, NONE, bellovl) /* bell overload protection active? */ \
    X(INT, NONE, bellovl_n) /* number of bells to cause overload */ \
    X(INT, NONE, bellovl_t) /* time interval for overload (seconds) */ \
    X(INT, NONE, bellovl_s) /* period of silence to re-enable bell (s) */ \
    X(FILENAME, NONE, bell_wavefile) \
    X(INT, NONE, scrollbar) \
    X(INT, NONE, scrollbar_in_fullscreen) \
    X(INT, NONE, resize_action) \
    X(INT, NONE, bce) \
    X(INT, NONE, blinktext) \
    X(INT, NONE, win_name_always) \
    X(INT, NONE, width) \
    X(INT, NONE, height) \
    X(FONT, NONE, font) \
    X(INT, NONE, font_quality) \
    X(FILENAME, NONE, logfilename) \
    X(INT, NONE, logtype) \
    X(INT, NONE, logxfovr) \
    X(INT, NONE, logflush) \
    X(INT, NONE, logomitpass) \
    X(INT, NONE, logomitdata) \
    X(INT, NONE, hide_mouseptr) \
    X(INT, NONE, sunken_edge) \
    X(INT, NONE, window_border) \
    X(STR, NONE, answerback) \
    X(STR, NONE, printer) \
    X(INT, NONE, arabicshaping) \
    X(INT, NONE, bidi) \
    /* Colour options */ \
    X(INT, NONE, ansi_colour) \
    X(INT, NONE, xterm_256_colour) \
    X(INT, NONE, system_colour) \
    X(INT, NONE, try_palette) \
    X(INT, NONE, bold_style) \
    X(INT, INT, colours) \
    /* Selection options */ \
    X(INT, NONE, mouse_is_xterm) \
    X(INT, NONE, rect_select) \
    X(INT, NONE, rawcnp) \
    X(INT, NONE, rtf_paste) \
    X(INT, NONE, mouse_override) \
    X(INT, INT, wordness) \
    /* translations */ \
    X(INT, NONE, vtmode) \
    X(STR, NONE, line_codepage) \
    X(INT, NONE, cjk_ambig_wide) \
    X(INT, NONE, utf8_override) \
    X(INT, NONE, xlat_capslockcyr) \
    /* X11 forwarding */ \
    X(INT, NONE, x11_forward) \
    X(STR, NONE, x11_display) \
    X(INT, NONE, x11_auth) \
    X(FILENAME, NONE, xauthfile) \
    /* port forwarding */ \
    X(INT, NONE, lport_acceptall) /* accept conns from hosts other than localhost */ \
    X(INT, NONE, rport_acceptall) /* same for remote forwarded ports (SSH-2 only) */ \
    /*                                                                \
     * Subkeys for 'portfwd' can have the following forms:            \
     *                                                                \
     *   [LR]localport                                                \
     *   [LR]localaddr:localport                                      \
     *                                                                \
     * Dynamic forwardings are indicated by an 'L' key, and the       \
     * special value "D". For all other forwardings, the value        \
     * should be of the form 'host:port'.                             \
     */ \
    X(STR, STR, portfwd) \
    /* SSH bug compatibility modes */ \
    X(INT, NONE, sshbug_ignore1) \
    X(INT, NONE, sshbug_plainpw1) \
    X(INT, NONE, sshbug_rsa1) \
    X(INT, NONE, sshbug_hmac2) \
    X(INT, NONE, sshbug_derivekey2) \
    X(INT, NONE, sshbug_rsapad2) \
    X(INT, NONE, sshbug_pksessid2) \
    X(INT, NONE, sshbug_rekey2) \
    X(INT, NONE, sshbug_maxpkt2) \
    X(INT, NONE, sshbug_ignore2) \
    X(INT, NONE, sshbug_oldgex2) \
    X(INT, NONE, sshbug_winadj) \
    X(INT, NONE, sshbug_chanreq) \
    /*                                                                \
     * ssh_simple means that we promise never to open any channel     \
     * other than the main one, which means it can safely use a very  \
     * large window in SSH-2.                                         \
     */ \
    X(INT, NONE, ssh_simple) \
    X(INT, NONE, ssh_connection_sharing) \
    X(INT, NONE, ssh_connection_sharing_upstream) \
    X(INT, NONE, ssh_connection_sharing_downstream) \
    /*
     * ssh_manual_hostkeys is conceptually a set rather than a
     * dictionary: the string subkeys are the important thing, and the
     * actual values to which those subkeys map are all "".
     */ \
    X(STR, STR, ssh_manual_hostkeys) \
    /* Options for pterm. Should split out into platform-dependent part. */ \
    X(INT, NONE, stamp_utmp) \
    X(INT, NONE, login_shell) \
    X(INT, NONE, scrollbar_on_left) \
    X(INT, NONE, shadowbold) \
    X(FONT, NONE, boldfont) \
    X(FONT, NONE, widefont) \
    X(FONT, NONE, wideboldfont) \
    X(INT, NONE, shadowboldoffset) \
    X(INT, NONE, crhaslf) \
    X(STR, NONE, winclass) \

/* Now define the actual enum of option keywords using that macro. */
#define CONF_ENUM_DEF(valtype, keytype, keyword) CONF_ ## keyword,
enum config_primary_key { CONFIG_OPTIONS(CONF_ENUM_DEF) N_CONFIG_OPTIONS };
#undef CONF_ENUM_DEF

#define NCFGCOLOURS 22 /* number of colours in CONF_colours above */

/* Functions handling configuration structures. */
Conf *conf_new(void);		       /* create an empty configuration */
void conf_free(Conf *conf);
Conf *conf_copy(Conf *oldconf);
void conf_copy_into(Conf *dest, Conf *src);
/* Mandatory accessor functions: enforce by assertion that keys exist. */
int conf_get_int(Conf *conf, int key);
int conf_get_int_int(Conf *conf, int key, int subkey);
char *conf_get_str(Conf *conf, int key);   /* result still owned by conf */
char *conf_get_str_str(Conf *conf, int key, const char *subkey);
Filename *conf_get_filename(Conf *conf, int key);
FontSpec *conf_get_fontspec(Conf *conf, int key); /* still owned by conf */
/* Optional accessor function: return NULL if key does not exist. */
char *conf_get_str_str_opt(Conf *conf, int key, const char *subkey);
/* Accessor function to step through a string-subkeyed list.
 * Returns the next subkey after the provided one, or the first if NULL.
 * Returns NULL if there are none left.
 * Both the return value and *subkeyout are still owned by conf. */
char *conf_get_str_strs(Conf *conf, int key, char *subkeyin, char **subkeyout);
/* Return the nth string subkey in a list. Owned by conf. NULL if beyond end */
char *conf_get_str_nthstrkey(Conf *conf, int key, int n);
/* Functions to set entries in configuration. Always copy their inputs. */
void conf_set_int(Conf *conf, int key, int value);
void conf_set_int_int(Conf *conf, int key, int subkey, int value);
void conf_set_str(Conf *conf, int key, const char *value);
void conf_set_str_str(Conf *conf, int key,
		      const char *subkey, const char *val);
void conf_del_str_str(Conf *conf, int key, const char *subkey);
void conf_set_filename(Conf *conf, int key, const Filename *val);
void conf_set_fontspec(Conf *conf, int key, const FontSpec *val);
/* Serialisation functions for Duplicate Session */
int conf_serialised_size(Conf *conf);
void conf_serialise(Conf *conf, void *data);
int conf_deserialise(Conf *conf, void *data, int maxsize);/*returns size used*/

/*
 * Functions to copy, free, serialise and deserialise FontSpecs.
 * Provided per-platform, to go with the platform's idea of a
 * FontSpec's contents.
 *
 * fontspec_serialise returns the number of bytes written, and can
 * handle data==NULL without crashing. So you can call it once to find
 * out a size, then again once you've allocated a buffer.
 */
FontSpec *fontspec_copy(const FontSpec *f);
void fontspec_free(FontSpec *f);
int fontspec_serialise(FontSpec *f, void *data);
FontSpec *fontspec_deserialise(void *data, int maxsize, int *used);

/*
 * Exports from noise.c.
 */
void noise_get_heavy(void (*func) (void *, int));
void noise_get_light(void (*func) (void *, int));
void noise_regular(void);
void noise_ultralight(unsigned long data);
void random_save_seed(void);
void random_destroy_seed(void);

/*
 * Exports from settings.c.
 */
Backend *backend_from_name(const char *name);
Backend *backend_from_proto(int proto);
char *get_remote_username(Conf *conf); /* dynamically allocated */
char *save_settings(char *section, Conf *conf);
void save_open_settings(void *sesskey, Conf *conf);
void load_settings(char *section, Conf *conf);
void load_open_settings(void *sesskey, Conf *conf);
void get_sesslist(struct sesslist *, int allocate);
void do_defaults(char *, Conf *);
void registry_cleanup(void);

/*
 * Functions used by settings.c to provide platform-specific
 * default settings.
 * 
 * (The integer one is expected to return `def' if it has no clear
 * opinion of its own. This is because there's no integer value
 * which I can reliably set aside to indicate `nil'. The string
 * function is perfectly all right returning NULL, of course. The
 * Filename and FontSpec functions are _not allowed_ to fail to
 * return, since these defaults _must_ be per-platform.)
 *
 * The 'Filename *' returned by platform_default_filename, and the
 * 'FontSpec *' returned by platform_default_fontspec, have ownership
 * transferred to the caller, and must be freed.
 */
char *platform_default_s(const char *name);
int platform_default_i(const char *name, int def);
Filename *platform_default_filename(const char *name);
FontSpec *platform_default_fontspec(const char *name);

/*
 * Exports from terminal.c.
 */

Terminal *term_init(Conf *, struct unicode_data *, void *);
void term_free(Terminal *);
void term_size(Terminal *, int, int, int);
void term_paint(Terminal *, Context, int, int, int, int, int);
void term_scroll(Terminal *, int, int);
void term_scroll_to_selection(Terminal *, int);
void term_pwron(Terminal *, int);
void term_clrsb(Terminal *);
void term_mouse(Terminal *, Mouse_Button, Mouse_Button, Mouse_Action,
		int,int,int,int,int);
void term_key(Terminal *, Key_Sym, wchar_t *, size_t, unsigned int,
	      unsigned int);
void term_deselect(Terminal *);
void term_update(Terminal *);
void term_invalidate(Terminal *);
void term_blink(Terminal *, int set_cursor);
void term_do_paste(Terminal *);
void term_nopaste(Terminal *);
int term_ldisc(Terminal *, int option);
void term_copyall(Terminal *);
void term_reconfig(Terminal *, Conf *);
void term_seen_key_event(Terminal *); 
int term_data(Terminal *, int is_stderr, const char *data, int len);
int term_data_untrusted(Terminal *, const char *data, int len);
void term_provide_resize_fn(Terminal *term,
			    void (*resize_fn)(void *, int, int),
			    void *resize_ctx);
void term_provide_logctx(Terminal *term, void *logctx);
void term_set_focus(Terminal *term, int has_focus);
char *term_get_ttymode(Terminal *term, const char *mode);
int term_get_userpass_input(Terminal *term, prompts_t *p,
			    unsigned char *in, int inlen);

int format_arrow_key(char *buf, Terminal *term, int xkey, int ctrl);

/*
 * Exports from logging.c.
 */
void *log_init(void *frontend, Conf *conf);
void log_free(void *logctx);
void log_reconfig(void *logctx, Conf *conf);
void logfopen(void *logctx);
void logfclose(void *logctx);
void logtraffic(void *logctx, unsigned char c, int logmode);
void logflush(void *logctx);
void log_eventlog(void *logctx, const char *string);
enum { PKT_INCOMING, PKT_OUTGOING };
enum { PKTLOG_EMIT, PKTLOG_BLANK, PKTLOG_OMIT };
struct logblank_t {
    int offset;
    int len;
    int type;
};
void log_packet(void *logctx, int direction, int type,
		char *texttype, const void *data, int len,
		int n_blanks, const struct logblank_t *blanks,
		const unsigned long *sequence,
                unsigned downstream_id, const char *additional_log_text);

/*
 * Exports from testback.c
 */

extern Backend null_backend;
extern Backend loop_backend;

/*
 * Exports from raw.c.
 */

extern Backend raw_backend;

/*
 * Exports from rlogin.c.
 */

extern Backend rlogin_backend;

/*
 * Exports from telnet.c.
 */

extern Backend telnet_backend;

/*
 * Exports from ssh.c.
 */
extern Backend ssh_backend;

/*
 * Exports from ldisc.c.
 */
void *ldisc_create(Conf *, Terminal *, Backend *, void *, void *);
void ldisc_configure(void *, Conf *);
void ldisc_free(void *);
void ldisc_send(void *handle, char *buf, int len, int interactive);

/*
 * Exports from ldiscucs.c.
 */
void lpage_send(void *, int codepage, char *buf, int len, int interactive);
void luni_send(void *, wchar_t * widebuf, int len, int interactive);

/*
 * Exports from sshrand.c.
 */

void random_add_noise(void *noise, int length);
int random_byte(void);
void random_get_savedata(void **data, int *len);
extern int random_active;
/* The random number subsystem is activated if at least one other entity
 * within the program expresses an interest in it. So each SSH session
 * calls random_ref on startup and random_unref on shutdown. */
void random_ref(void);
void random_unref(void);

/*
 * Exports from pinger.c.
 */
typedef struct pinger_tag *Pinger;
Pinger pinger_new(Conf *conf, Backend *back, void *backhandle);
void pinger_reconfig(Pinger, Conf *oldconf, Conf *newconf);
void pinger_free(Pinger);

/*
 * Exports from misc.c.
 */

#include "misc.h"
int conf_launchable(Conf *conf);
char const *conf_dest(Conf *conf);

/*
 * Exports from sercfg.c.
 */
void ser_setup_config_box(struct controlbox *b, int midsession,
			  int parity_mask, int flow_mask);

/*
 * Exports from version.c.
 */
extern char ver[];

/*
 * Exports from unicode.c.
 */
#ifndef CP_UTF8
#define CP_UTF8 65001
#endif
/* void init_ucs(void); -- this is now in platform-specific headers */
int is_dbcs_leadbyte(int codepage, char byte);
int mb_to_wc(int codepage, int flags, const char *mbstr, int mblen,
	     wchar_t *wcstr, int wclen);
int wc_to_mb(int codepage, int flags, const wchar_t *wcstr, int wclen,
	     char *mbstr, int mblen, char *defchr, int *defused,
	     struct unicode_data *ucsdata);
wchar_t xlat_uskbd2cyrllic(int ch);
int check_compose(int first, int second);
int decode_codepage(char *cp_name);
const char *cp_enumerate (int index);
const char *cp_name(int codepage);
void get_unitab(int codepage, wchar_t * unitab, int ftype);

/*
 * Exports from wcwidth.c
 */
int mk_wcwidth(unsigned int ucs);
int mk_wcswidth(const unsigned int *pwcs, size_t n);
int mk_wcwidth_cjk(unsigned int ucs);
int mk_wcswidth_cjk(const unsigned int *pwcs, size_t n);

/*
 * Exports from mscrypto.c
 */
#ifdef MSCRYPTOAPI
int crypto_startup();
void crypto_wrapup();
#endif

/*
 * Exports from pageantc.c.
 * 
 * agent_query returns 1 for here's-a-response, and 0 for query-in-
 * progress. In the latter case there will be a call to `callback'
 * at some future point, passing callback_ctx as the first
 * parameter and the actual reply data as the second and third.
 * 
 * The response may be a NULL pointer (in either of the synchronous
 * or asynchronous cases), which indicates failure to receive a
 * response.
 */
int agent_query(void *in, int inlen, void **out, int *outlen,
		void (*callback)(void *, void *, int), void *callback_ctx);
int agent_exists(void);

/*
 * Exports from wildcard.c
 */
const char *wc_error(int value);
int wc_match(const char *wildcard, const char *target);
int wc_unescape(char *output, const char *wildcard);

/*
 * Exports from frontend (windlg.c etc)
 */
void logevent(void *frontend, const char *);
void pgp_fingerprints(void);
/*
 * verify_ssh_host_key() can return one of three values:
 * 
 *  - +1 means `key was OK' (either already known or the user just
 *    approved it) `so continue with the connection'
 * 
 *  - 0 means `key was not OK, abandon the connection'
 * 
 *  - -1 means `I've initiated enquiries, please wait to be called
 *    back via the provided function with a result that's either 0
 *    or +1'.
 */
int verify_ssh_host_key(void *frontend, char *host, int port, char *keytype,
                        char *keystr, char *fingerprint,
                        void (*callback)(void *ctx, int result), void *ctx);
/*
 * askalg has the same set of return values as verify_ssh_host_key.
 */
int askalg(void *frontend, const char *algtype, const char *algname,
	   void (*callback)(void *ctx, int result), void *ctx);
/*
 * askappend can return four values:
 * 
 *  - 2 means overwrite the log file
 *  - 1 means append to the log file
 *  - 0 means cancel logging for this session
 *  - -1 means please wait.
 */
int askappend(void *frontend, Filename *filename,
	      void (*callback)(void *ctx, int result), void *ctx);

/*
 * Exports from console frontends (wincons.c, uxcons.c)
 * that aren't equivalents to things in windlg.c et al.
 */
extern int console_batch_mode;
int console_get_userpass_input(prompts_t *p, unsigned char *in, int inlen);
void console_provide_logctx(void *logctx);
int is_interactive(void);

/*
 * Exports from printing.c.
 */
typedef struct printer_enum_tag printer_enum;
typedef struct printer_job_tag printer_job;
printer_enum *printer_start_enum(int *nprinters);
char *printer_get_name(printer_enum *, int);
void printer_finish_enum(printer_enum *);
printer_job *printer_start_job(char *printer);
void printer_job_data(printer_job *, void *, int);
void printer_finish_job(printer_job *);

/*
 * Exports from cmdline.c (and also cmdline_error(), which is
 * defined differently in various places and required _by_
 * cmdline.c).
 */
int cmdline_process_param(char *, char *, int, Conf *);
void cmdline_run_saved(Conf *);
void cmdline_cleanup(void);
int cmdline_get_passwd_input(prompts_t *p, unsigned char *in, int inlen);
#define TOOLTYPE_FILETRANSFER 1
#define TOOLTYPE_NONNETWORK 2
extern int cmdline_tooltype;

void cmdline_error(char *, ...);

/*
 * Exports from config.c.
 */
struct controlbox;
union control;
void conf_radiobutton_handler(union control *ctrl, void *dlg,
			      void *data, int event);
#define CHECKBOX_INVERT (1<<30)
void conf_checkbox_handler(union control *ctrl, void *dlg,
			   void *data, int event);
void conf_editbox_handler(union control *ctrl, void *dlg,
			  void *data, int event);
void conf_filesel_handler(union control *ctrl, void *dlg,
			  void *data, int event);
void conf_fontsel_handler(union control *ctrl, void *dlg,
			  void *data, int event);
void setup_config_box(struct controlbox *b, int midsession,
		      int protocol, int protcfginfo);

/*
 * Exports from minibidi.c.
 */
typedef struct bidi_char {
    unsigned int origwc, wc;
    unsigned short index;
} bidi_char;
int do_bidi(bidi_char *line, int count);
int do_shape(bidi_char *line, bidi_char *to, int count);
int is_rtl(int c);

/*
 * X11 auth mechanisms we know about.
 */
enum {
    X11_NO_AUTH,
    X11_MIT,                           /* MIT-MAGIC-COOKIE-1 */
    X11_XDM,			       /* XDM-AUTHORIZATION-1 */
    X11_NAUTHS
};
extern const char *const x11_authnames[];  /* declared in x11fwd.c */

/*
 * Miscellaneous exports from the platform-specific code.
 *
 * filename_serialise and filename_deserialise have the same semantics
 * as fontspec_serialise and fontspec_deserialise above.
 */
Filename *filename_from_str(const char *string);
const char *filename_to_str(const Filename *fn);
int filename_equal(const Filename *f1, const Filename *f2);
int filename_is_null(const Filename *fn);
Filename *filename_copy(const Filename *fn);
void filename_free(Filename *fn);
int filename_serialise(const Filename *f, void *data);
Filename *filename_deserialise(void *data, int maxsize, int *used);
char *get_username(void);	       /* return value needs freeing */
char *get_random_data(int bytes);      /* used in cmdgen.c */
char filename_char_sanitise(char c);   /* rewrite special pathname chars */

/*
 * Exports and imports from timing.c.
 *
 * schedule_timer() asks the front end to schedule a callback to a
 * timer function in a given number of ticks. The returned value is
 * the time (in ticks since an arbitrary offset) at which the
 * callback can be expected. This value will also be passed as the
 * `now' parameter to the callback function. Hence, you can (for
 * example) schedule an event at a particular time by calling
 * schedule_timer() and storing the return value in your context
 * structure as the time when that event is due. The first time a
 * callback function gives you that value or more as `now', you do
 * the thing.
 * 
 * expire_timer_context() drops all current timers associated with
 * a given value of ctx (for when you're about to free ctx).
 * 
 * run_timers() is called from the front end when it has reason to
 * think some timers have reached their moment, or when it simply
 * needs to know how long to wait next. We pass it the time we
 * think it is. It returns TRUE and places the time when the next
 * timer needs to go off in `next', or alternatively it returns
 * FALSE if there are no timers at all pending.
 * 
 * timer_change_notify() must be supplied by the front end; it
 * notifies the front end that a new timer has been added to the
 * list which is sooner than any existing ones. It provides the
 * time when that timer needs to go off.
 * 
 * *** FRONT END IMPLEMENTORS NOTE:
 * 
 * There's an important subtlety in the front-end implementation of
 * the timer interface. When a front end is given a `next' value,
 * either returned from run_timers() or via timer_change_notify(),
 * it should ensure that it really passes _that value_ as the `now'
 * parameter to its next run_timers call. It should _not_ simply
 * call GETTICKCOUNT() to get the `now' parameter when invoking
 * run_timers().
 * 
 * The reason for this is that an OS's system clock might not agree
 * exactly with the timing mechanisms it supplies to wait for a
 * given interval. I'll illustrate this by the simple example of
 * Unix Plink, which uses timeouts to select() in a way which for
 * these purposes can simply be considered to be a wait() function.
 * Suppose, for the sake of argument, that this wait() function
 * tends to return early by 1%. Then a possible sequence of actions
 * is:
 * 
 *  - run_timers() tells the front end that the next timer firing
 *    is 10000ms from now.
 *  - Front end calls wait(10000ms), but according to
 *    GETTICKCOUNT() it has only waited for 9900ms.
 *  - Front end calls run_timers() again, passing time T-100ms as
 *    `now'.
 *  - run_timers() does nothing, and says the next timer firing is
 *    still 100ms from now.
 *  - Front end calls wait(100ms), which only waits for 99ms.
 *  - Front end calls run_timers() yet again, passing time T-1ms.
 *  - run_timers() says there's still 1ms to wait.
 *  - Front end calls wait(1ms).
 * 
 * If you're _lucky_ at this point, wait(1ms) will actually wait
 * for 1ms and you'll only have woken the program up three times.
 * If you're unlucky, wait(1ms) might do nothing at all due to
 * being below some minimum threshold, and you might find your
 * program spends the whole of the last millisecond tight-looping
 * between wait() and run_timers().
 * 
 * Instead, what you should do is to _save_ the precise `next'
 * value provided by run_timers() or via timer_change_notify(), and
 * use that precise value as the input to the next run_timers()
 * call. So:
 * 
 *  - run_timers() tells the front end that the next timer firing
 *    is at time T, 10000ms from now.
 *  - Front end calls wait(10000ms).
 *  - Front end then immediately calls run_timers() and passes it
 *    time T, without stopping to check GETTICKCOUNT() at all.
 * 
 * This guarantees that the program wakes up only as many times as
 * there are actual timer actions to be taken, and that the timing
 * mechanism will never send it into a tight loop.
 * 
 * (It does also mean that the timer action in the above example
 * will occur 100ms early, but this is not generally critical. And
 * the hypothetical 1% error in wait() will be partially corrected
 * for anyway when, _after_ run_timers() returns, you call
 * GETTICKCOUNT() and compare the result with the returned `next'
 * value to find out how long you have to make your next wait().)
 */
typedef void (*timer_fn_t)(void *ctx, unsigned long now);
unsigned long schedule_timer(int ticks, timer_fn_t fn, void *ctx);
void expire_timer_context(void *ctx);
int run_timers(unsigned long now, unsigned long *next);
void timer_change_notify(unsigned long next);

/*
 * Exports from callback.c.
 *
 * This provides a method of queuing function calls to be run at the
 * earliest convenience from the top-level event loop. Use it if
 * you're deep in a nested chain of calls and want to trigger an
 * action which will probably lead to your function being re-entered
 * recursively if you just call the initiating function the normal
 * way.
 *
 * Most front ends run the queued callbacks by simply calling
 * run_toplevel_callbacks() after handling each event in their
 * top-level event loop. However, if a front end doesn't have control
 * over its own event loop (e.g. because it's using GTK) then it can
 * instead request notifications when a callback is available, so that
 * it knows to ask its delegate event loop to do the same thing. Also,
 * if a front end needs to know whether a callback is pending without
 * actually running it (e.g. so as to put a zero timeout on a select()
 * call) then it can call toplevel_callback_pending(), which will
 * return true if at least one callback is in the queue.
 */
typedef void (*toplevel_callback_fn_t)(void *ctx);
void queue_toplevel_callback(toplevel_callback_fn_t fn, void *ctx);
void run_toplevel_callbacks(void);
int toplevel_callback_pending(void);

typedef void (*toplevel_callback_notify_fn_t)(void *frontend);
void request_callback_notifications(toplevel_callback_notify_fn_t notify,
                                    void *frontend);

/*
 * Define no-op macros for the jump list functions, on platforms that
 * don't support them. (This is a bit of a hack, and it'd be nicer to
 * localise even the calls to those functions into the Windows front
 * end, but it'll do for the moment.)
 */
#ifndef JUMPLIST_SUPPORTED
#define add_session_to_jumplist(x) ((void)0)
#define remove_session_from_jumplist(x) ((void)0)
#endif

/* SURROGATE PAIR */
#define HIGH_SURROGATE_START 0xd800
#define HIGH_SURROGATE_END 0xdbff
#define LOW_SURROGATE_START 0xdc00
#define LOW_SURROGATE_END 0xdfff

/* These macros exist in the Windows API, so the environment may
 * provide them. If not, define them in terms of the above. */
#ifndef IS_HIGH_SURROGATE
#define IS_HIGH_SURROGATE(wch) (((wch) >= HIGH_SURROGATE_START) && \
                                ((wch) <= HIGH_SURROGATE_END))
#define IS_LOW_SURROGATE(wch) (((wch) >= LOW_SURROGATE_START) && \
                               ((wch) <= LOW_SURROGATE_END))
#define IS_SURROGATE_PAIR(hs, ls) (IS_HIGH_SURROGATE(hs) && \
                                   IS_LOW_SURROGATE(ls))
#endif


#define IS_SURROGATE(wch) (((wch) >= HIGH_SURROGATE_START) &&   \
                           ((wch) <= LOW_SURROGATE_END))
#define HIGH_SURROGATE_OF(codept) \
    (HIGH_SURROGATE_START + (((codept) - 0x10000) >> 10))
#define LOW_SURROGATE_OF(codept) \
    (LOW_SURROGATE_START + (((codept) - 0x10000) & 0x3FF))
#define FROM_SURROGATES(wch1, wch2) \
    (0x10000 + (((wch1) & 0x3FF) << 10) + ((wch2) & 0x3FF))

#endif
