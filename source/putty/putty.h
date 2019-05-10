#ifndef PUTTY_PUTTY_H
#define PUTTY_PUTTY_H

#include <stddef.h>		       /* for wchar_t */
#include <limits.h>                    /* for INT_MAX */

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

#include "defs.h"
#include "puttyps.h"
#include "network.h"
#include "misc.h"
#include "marshal.h"

/*
 * We express various time intervals in unsigned long minutes, but may need to
 * clip some values so that the resulting number of ticks does not overflow an
 * integer value.
 */
#define MAX_TICK_MINS	(INT_MAX / (60 * TICKSPERSEC))

/*
 * Fingerprints of the current and previous PGP master keys, to
 * establish a trust path between an executable and other files.
 */
#define PGP_MASTER_KEY_YEAR "2018"
#define PGP_MASTER_KEY_DETAILS "RSA, 4096-bit"
#define PGP_MASTER_KEY_FP                                       \
    "24E1 B1C5 75EA 3C9F F752  A922 76BC 7FE4 EBFD 2D9E"
#define PGP_PREV_MASTER_KEY_YEAR "2015"
#define PGP_PREV_MASTER_KEY_DETAILS "RSA, 4096-bit"
#define PGP_PREV_MASTER_KEY_FP                                  \
    "440D E3B5 B7A1 CA85 B3CC  1718 AB58 5DC6 0467 6F7C"

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

#define ATTR_NARROW  0x0800000U
#define ATTR_WIDE    0x0400000U
#define ATTR_BOLD    0x0040000U
#define ATTR_UNDER   0x0080000U
#define ATTR_REVERSE 0x0100000U
#define ATTR_BLINK   0x0200000U
#define ATTR_FGMASK  0x00001FFU
#define ATTR_BGMASK  0x003FE00U
#define ATTR_COLOURS 0x003FFFFU
#define ATTR_DIM     0x1000000U
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
    const char **sessions;
    char *buffer;		       /* so memory can be freed later */
};

struct unicode_data {
    char **uni_tbl;
    bool dbcs_screenfont;
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

/*
 * Enumeration of 'special commands' that can be sent during a
 * session, separately from the byte stream of ordinary session data.
 */
typedef enum {
    /*
     * Commands that are generally useful in multiple backends.
     */
    SS_BRK,    /* serial-line break */
    SS_EOF,    /* end-of-file on session input */
    SS_NOP,    /* transmit data with no effect */
    SS_PING,   /* try to keep the session alive (probably, but not
                * necessarily, implemented as SS_NOP) */

    /*
     * Commands specific to Telnet.
     */
    SS_AYT,    /* Are You There */
    SS_SYNCH,  /* Synch */
    SS_EC,     /* Erase Character */
    SS_EL,     /* Erase Line */
    SS_GA,     /* Go Ahead */
    SS_ABORT,  /* Abort Process */
    SS_AO,     /* Abort Output */
    SS_IP,     /* Interrupt Process */
    SS_SUSP,   /* Suspend Process */
    SS_EOR,    /* End Of Record */
    SS_EOL,    /* Telnet end-of-line sequence (CRLF, as opposed to CR
                * NUL that escapes a literal CR) */

    /*
     * Commands specific to SSH.
     */
    SS_REKEY,  /* trigger an immediate repeat key exchange */
    SS_XCERT,  /* cross-certify another host key ('arg' indicates which) */

    /*
     * Send a POSIX-style signal. (Useful in SSH and also pterm.)
     *
     * We use the master list in sshsignals.h to define these enum
     * values, which will come out looking like names of the form
     * SS_SIGABRT, SS_SIGINT etc.
     */
    #define SIGNAL_MAIN(name, text) SS_SIG ## name,
    #define SIGNAL_SUB(name) SS_SIG ## name,
    #include "sshsignals.h"
    #undef SIGNAL_MAIN
    #undef SIGNAL_SUB

    /*
     * These aren't really special commands, but they appear in the
     * enumeration because the list returned from
     * backend_get_specials() will use them to specify the structure
     * of the GUI specials menu.
     */
    SS_SEP,	    /* Separator */
    SS_SUBMENU,	    /* Start a new submenu with specified name */
    SS_EXITMENU,    /* Exit current submenu, or end of entire specials list */
} SessionSpecialCode;

/*
 * The structure type returned from backend_get_specials.
 */
struct SessionSpecial {
    const char *name;
    SessionSpecialCode code;
    int arg;
};

/* Needed by both sshchan.h and sshppl.h */
typedef void (*add_special_fn_t)(
    void *ctx, const char *text, SessionSpecialCode code, int arg);

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
    KEX_ECDH,
    KEX_MAX
};

enum {
    /*
     * SSH-2 host key algorithms
     */
    HK_WARN,
    HK_RSA,
    HK_DSA,
    HK_ECDSA,
    HK_ED25519,
    HK_MAX
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
    CIPHER_CHACHA20,
    CIPHER_MAX			       /* no. ciphers (inc warn) */
};

enum TriState {
    /*
     * Several different bits of the PuTTY configuration seem to be
     * three-way settings whose values are `always yes', `always
     * no', and `decide by some more complex automated means'. This
     * is true of line discipline options (local echo and line
     * editing), proxy DNS, proxy terminal logging, Close On Exit, and
     * SSH server bug workarounds. Accordingly I supply a single enum
     * here to deal with them all.
     */
    FORCE_ON, FORCE_OFF, AUTO
};

enum {
    /*
     * Proxy types.
     */
    PROXY_NONE, PROXY_SOCKS4, PROXY_SOCKS5,
    PROXY_HTTP, PROXY_TELNET, PROXY_CMD, PROXY_FUZZ
};

enum {
    /*
     * Line discipline options which the backend might try to control.
     */
    LD_EDIT,			       /* local line editing */
    LD_ECHO,                           /* local echo */
    LD_N_OPTIONS
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
    const char *s;
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
    ADDRTYPE_UNSPEC,
    ADDRTYPE_IPV4,
    ADDRTYPE_IPV6,
    ADDRTYPE_LOCAL,    /* e.g. Unix domain socket, or Windows named pipe */
    ADDRTYPE_NAME      /* SockAddr storing an unresolved host name */
};

struct Backend {
    const BackendVtable *vt;
};
struct BackendVtable {
    const char *(*init) (Seat *seat, Backend **backend_out,
                         LogContext *logctx, Conf *conf,
                         const char *host, int port,
                         char **realhost, bool nodelay, bool keepalive);

    void (*free) (Backend *be);
    /* Pass in a replacement configuration. */
    void (*reconfig) (Backend *be, Conf *conf);
    /* send() returns the current amount of buffered data. */
    size_t (*send) (Backend *be, const char *buf, size_t len);
    /* sendbuffer() does the same thing but without attempting a send */
    size_t (*sendbuffer) (Backend *be);
    void (*size) (Backend *be, int width, int height);
    void (*special) (Backend *be, SessionSpecialCode code, int arg);
    const SessionSpecial *(*get_specials) (Backend *be);
    bool (*connected) (Backend *be);
    int (*exitcode) (Backend *be);
    /* If back->sendok() returns false, the backend doesn't currently
     * want input data, so the frontend should avoid acquiring any if
     * possible (passing back-pressure on to its sender). */
    bool (*sendok) (Backend *be);
    bool (*ldisc_option_state) (Backend *be, int);
    void (*provide_ldisc) (Backend *be, Ldisc *ldisc);
    /* Tells the back end that the front end  buffer is clearing. */
    void (*unthrottle) (Backend *be, size_t bufsize);
    int (*cfg_info) (Backend *be);

    /* Only implemented in the SSH protocol: check whether a
     * connection-sharing upstream exists for a given configuration. */
    bool (*test_for_upstream)(const char *host, int port, Conf *conf);

    const char *name;
    int protocol;
    int default_port;
};

static inline const char *backend_init(
    const BackendVtable *vt, Seat *seat, Backend **out, LogContext *logctx,
    Conf *conf, const char *host, int port, char **rhost, bool nd, bool ka)
{ return vt->init(seat, out, logctx, conf, host, port, rhost, nd, ka); }
static inline void backend_free(Backend *be)
{ be->vt->free(be); }
static inline void backend_reconfig(Backend *be, Conf *conf)
{ be->vt->reconfig(be, conf); }
static inline size_t backend_send(Backend *be, const char *buf, size_t len)
{ return be->vt->send(be, buf, len); }
static inline size_t backend_sendbuffer(Backend *be)
{ return be->vt->sendbuffer(be); }
static inline void backend_size(Backend *be, int width, int height)
{ be->vt->size(be, width, height); }
static inline void backend_special(
    Backend *be, SessionSpecialCode code, int arg)
{ be->vt->special(be, code, arg); }
static inline const SessionSpecial *backend_get_specials(Backend *be)
{ return be->vt->get_specials(be); }
static inline bool backend_connected(Backend *be)
{ return be->vt->connected(be); }
static inline int backend_exitcode(Backend *be)
{ return be->vt->exitcode(be); }
static inline bool backend_sendok(Backend *be)
{ return be->vt->sendok(be); }
static inline bool backend_ldisc_option_state(Backend *be, int state)
{ return be->vt->ldisc_option_state(be, state); }
static inline void backend_provide_ldisc(Backend *be, Ldisc *ldisc)
{ be->vt->provide_ldisc(be, ldisc); }
static inline void backend_unthrottle(Backend *be, size_t bufsize)
{ be->vt->unthrottle(be, bufsize); }
static inline int backend_cfg_info(Backend *be)
{ return be->vt->cfg_info(be); }

extern const struct BackendVtable *const backends[];

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
#define FLAG_INTERACTIVE 0x0002
GLOBAL int flags;

/*
 * Likewise, these two variables are set up when the application
 * initialises, and inform all default-settings accesses after
 * that.
 */
GLOBAL int default_protocol;
GLOBAL int default_port;

/*
 * This is set true by cmdline.c iff a session is loaded with "-load".
 */
GLOBAL bool loaded_session;
/*
 * This is set to the name of the loaded session.
 */
GLOBAL char *cmdline_session_name;

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
    bool echo;
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
    bool to_server;

    /*
     * Indicates whether the prompts originated _at_ the server, so
     * that the front end can display some kind of trust sigil that
     * distinguishes (say) a legit private-key passphrase prompt from
     * a fake one sent by a malicious server.
     */
    bool from_server;

    char *name;		/* Short description, perhaps for dialog box title */
    bool name_reqd;     /* Display of `name' required or optional? */
    char *instruction;	/* Long description, maybe with embedded newlines */
    bool instr_reqd;    /* Display of `instruction' required or optional? */
    size_t n_prompts;   /* May be zero (in which case display the foregoing,
                         * if any, and return success) */
    size_t prompts_size; /* allocated storage capacity for prompts[] */
    prompt_t **prompts;
    void *data;		/* slot for housekeeping data, managed by
			 * seat_get_userpass_input(); initially NULL */
} prompts_t;
prompts_t *new_prompts();
void add_prompt(prompts_t *p, char *promptstr, bool echo);
void prompt_set_result(prompt_t *pr, const char *newstr);
void prompt_ensure_result_size(prompt_t *pr, int len);
/* Burn the evidence. (Assumes _all_ strings want free()ing.) */
void free_prompts(prompts_t *p);

/*
 * Data type definitions for true-colour terminal display.
 * 'optionalrgb' describes a single RGB colour, which overrides the
 * other colour settings if 'enabled' is nonzero, and is ignored
 * otherwise. 'truecolour' contains a pair of those for foreground and
 * background.
 */
typedef struct optionalrgb {
    bool enabled;
    unsigned char r, g, b;
} optionalrgb;
extern const optionalrgb optionalrgb_none;
typedef struct truecolour {
    optionalrgb fg, bg;
} truecolour;
#define optionalrgb_equal(r1,r2) (                              \
        (r1).enabled==(r2).enabled &&                           \
        (r1).r==(r2).r && (r1).g==(r2).g && (r1).b==(r2).b)
#define truecolour_equal(c1,c2) (               \
        optionalrgb_equal((c1).fg, (c2).fg) &&  \
        optionalrgb_equal((c1).bg, (c2).bg))

/*
 * Enumeration of clipboards. We provide some standard ones cross-
 * platform, and then permit each platform to extend this enumeration
 * further by defining PLATFORM_CLIPBOARDS in its own header file.
 *
 * CLIP_NULL is a non-clipboard, writes to which are ignored and reads
 * from which return no data.
 *
 * CLIP_LOCAL refers to a buffer within terminal.c, which
 * unconditionally saves the last data selected in the terminal. In
 * configurations where a system clipboard is not written
 * automatically on selection but instead by an explicit UI action,
 * this is where the code responding to that action can find the data
 * to write to the clipboard in question.
 */
#define CROSS_PLATFORM_CLIPBOARDS(X)                    \
    X(CLIP_NULL, "null clipboard")                      \
    X(CLIP_LOCAL, "last text selected in terminal")     \
    /* end of list */

#define ALL_CLIPBOARDS(X)                       \
    CROSS_PLATFORM_CLIPBOARDS(X)                \
    PLATFORM_CLIPBOARDS(X)                      \
    /* end of list */

#define CLIP_ID(id,name) id,
enum { ALL_CLIPBOARDS(CLIP_ID) N_CLIPBOARDS };
#undef CLIP_ID

/* Hint from backend to frontend about time-consuming operations, used
 * by seat_set_busy_status. Initial state is assumed to be
 * BUSY_NOT. */
typedef enum BusyStatus {
    BUSY_NOT,	    /* Not busy, all user interaction OK */
    BUSY_WAITING,   /* Waiting for something; local event loops still
		       running so some local interaction (e.g. menus)
		       OK, but network stuff is suspended */
    BUSY_CPU	    /* Locally busy (e.g. crypto); user interaction
                     * suspended */
} BusyStatus;

typedef enum SeatInteractionContext {
    SIC_BANNER, SIC_KI_PROMPTS
} SeatInteractionContext;

/*
 * Data type 'Seat', which is an API intended to contain essentially
 * everything that a back end might need to talk to its client for:
 * session output, password prompts, SSH warnings about host keys and
 * weak cryptography, notifications of events like the remote process
 * exiting or the GUI specials menu needing an update.
 */
struct Seat {
    const struct SeatVtable *vt;
};
struct SeatVtable {
    /*
     * Provide output from the remote session. 'is_stderr' indicates
     * that the output should be sent to a separate error message
     * channel, if the seat has one. But combining both channels into
     * one is OK too; that's what terminal-window based seats do.
     *
     * The return value is the current size of the output backlog.
     */
    size_t (*output)(Seat *seat, bool is_stderr, const void *data, size_t len);

    /*
     * Called when the back end wants to indicate that EOF has arrived
     * on the server-to-client stream. Returns false to indicate that
     * we intend to keep the session open in the other direction, or
     * true to indicate that if they're closing so are we.
     */
    bool (*eof)(Seat *seat);

    /*
     * Try to get answers from a set of interactive login prompts. The
     * prompts are provided in 'p'; the bufchain 'input' holds the
     * data currently outstanding in the session's normal standard-
     * input channel. Seats may implement this function by consuming
     * data from 'input' (e.g. password prompts in GUI PuTTY,
     * displayed in the same terminal as the subsequent session), or
     * by doing something entirely different (e.g. directly
     * interacting with standard I/O, or putting up a dialog box).
     *
     * A positive return value means that all prompts have had answers
     * filled in. A zero return means that the user performed a
     * deliberate 'cancel' UI action. A negative return means that no
     * answer can be given yet but please try again later.
     *
     * (FIXME: it would be nice to distinguish two classes of cancel
     * action, so the user could specify 'I want to abandon this
     * entire attempt to start a session' or the milder 'I want to
     * abandon this particular form of authentication and fall back to
     * a different one' - e.g. if you turn out not to be able to
     * remember your private key passphrase then perhaps you'd rather
     * fall back to password auth rather than aborting the whole
     * session.)
     *
     * (Also FIXME: currently, backends' only response to the 'try
     * again later' is to try again when more input data becomes
     * available, because they assume that a seat is returning that
     * value because it's consuming keyboard input. But a seat that
     * handled this function by putting up a dialog box might want to
     * put it up non-modally, and therefore would want to proactively
     * notify the backend to retry once the dialog went away. So if I
     * ever do want to move password prompts into a dialog box, I'll
     * want a backend method for sending that notification.)
     */
    int (*get_userpass_input)(Seat *seat, prompts_t *p, bufchain *input);

    /*
     * Notify the seat that the process running at the other end of
     * the connection has finished.
     */
    void (*notify_remote_exit)(Seat *seat);

    /*
     * Notify the seat that the connection has suffered a fatal error.
     */
    void (*connection_fatal)(Seat *seat, const char *message);

    /*
     * Notify the seat that the list of special commands available
     * from backend_get_specials() has changed, so that it might want
     * to call that function to repopulate its menu.
     *
     * Seats are not expected to call backend_get_specials()
     * proactively; they may start by assuming that the backend
     * provides no special commands at all, so if the backend does
     * provide any, then it should use this notification at startup
     * time. Of course it can also invoke it later if the set of
     * special commands changes.
     *
     * It does not need to invoke it at session shutdown.
     */
    void (*update_specials_menu)(Seat *seat);

    /*
     * Get the seat's preferred value for an SSH terminal mode
     * setting. Returning NULL indicates no preference (i.e. the SSH
     * connection will not attempt to set the mode at all).
     *
     * The returned value is dynamically allocated, and the caller
     * should free it.
     */
    char *(*get_ttymode)(Seat *seat, const char *mode);

    /*
     * Tell the seat whether the backend is currently doing anything
     * CPU-intensive (typically a cryptographic key exchange). See
     * BusyStatus enumeration above.
     */
    void (*set_busy_status)(Seat *seat, BusyStatus status);

    /*
     * Ask the seat whether a given SSH host key should be accepted.
     * This may return immediately after checking saved configuration
     * or command-line options, or it may have to present a prompt to
     * the user and return asynchronously later.
     *
     * Return values:
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
    int (*verify_ssh_host_key)(
        Seat *seat, const char *host, int port,
        const char *keytype, char *keystr, char *key_fingerprint,
        void (*callback)(void *ctx, int result), void *ctx);

    /*
     * Check with the seat whether it's OK to use a cryptographic
     * primitive from below the 'warn below this line' threshold in
     * the input Conf. Return values are the same as
     * verify_ssh_host_key above.
     */
    int (*confirm_weak_crypto_primitive)(
        Seat *seat, const char *algtype, const char *algname,
        void (*callback)(void *ctx, int result), void *ctx);

    /*
     * Variant form of confirm_weak_crypto_primitive, which prints a
     * slightly different message but otherwise has the same
     * semantics.
     *
     * This form is used in the case where we're using a host key
     * below the warning threshold because that's the best one we have
     * cached, but at least one host key algorithm *above* the
     * threshold is available that we don't have cached. 'betteralgs'
     * lists the better algorithm(s).
     */
    int (*confirm_weak_cached_hostkey)(
        Seat *seat, const char *algname, const char *betteralgs,
        void (*callback)(void *ctx, int result), void *ctx);

    /*
     * Indicates whether the seat is expecting to interact with the
     * user in the UTF-8 character set. (Affects e.g. visual erase
     * handling in local line editing.)
     */
    bool (*is_utf8)(Seat *seat);

    /*
     * Notify the seat that the back end, and/or the ldisc between
     * them, have changed their idea of whether they currently want
     * local echo and/or local line editing enabled.
     */
    void (*echoedit_update)(Seat *seat, bool echoing, bool editing);

    /*
     * Return the local X display string relevant to a seat, or NULL
     * if there isn't one or if the concept is meaningless.
     */
    const char *(*get_x_display)(Seat *seat);

    /*
     * Return the X11 id of the X terminal window relevant to a seat,
     * by returning true and filling in the output pointer. Return
     * false if there isn't one or if the concept is meaningless.
     */
    bool (*get_windowid)(Seat *seat, long *id_out);

    /*
     * Return the size of the terminal window in pixels. If the
     * concept is meaningless or the information is unavailable,
     * return false; otherwise fill in the output pointers and return
     * true.
     */
    bool (*get_window_pixel_size)(Seat *seat, int *width, int *height);

    /*
     * Return a StripCtrlChars appropriate for sanitising untrusted
     * terminal data (e.g. SSH banners, prompts) being sent to the
     * user of this seat. May return NULL if no sanitisation is
     * needed.
     */
    StripCtrlChars *(*stripctrl_new)(
        Seat *seat, BinarySink *bs_out, SeatInteractionContext sic);

    /*
     * Set the seat's current idea of where output is coming from.
     * True means that output is being generated by our own code base
     * (and hence, can be trusted if it's asking you for secrets such
     * as your passphrase); false means output is coming from the
     * server.
     *
     * Returns true if the seat has a way to indicate this
     * distinction. Returns false if not, in which case the backend
     * should use a fallback defence against spoofing of PuTTY's local
     * prompts by malicious servers.
     */
    bool (*set_trust_status)(Seat *seat, bool trusted);
};

static inline size_t seat_output(
    Seat *seat, bool err, const void *data, size_t len)
{ return seat->vt->output(seat, err, data, len); }
static inline bool seat_eof(Seat *seat)
{ return seat->vt->eof(seat); }
static inline int seat_get_userpass_input(
    Seat *seat, prompts_t *p, bufchain *input)
{ return seat->vt->get_userpass_input(seat, p, input); }
static inline void seat_notify_remote_exit(Seat *seat)
{ seat->vt->notify_remote_exit(seat); }
static inline void seat_update_specials_menu(Seat *seat)
{ seat->vt->update_specials_menu(seat); }
static inline char *seat_get_ttymode(Seat *seat, const char *mode)
{ return seat->vt->get_ttymode(seat, mode); }
static inline void seat_set_busy_status(Seat *seat, BusyStatus status)
{ seat->vt->set_busy_status(seat, status); }
static inline int seat_verify_ssh_host_key(
    Seat *seat, const char *h, int p, const char *ktyp, char *kstr,
    char *fp, void (*cb)(void *ctx, int result), void *ctx)
{ return seat->vt->verify_ssh_host_key(seat, h, p, ktyp, kstr, fp, cb, ctx); }
static inline int seat_confirm_weak_crypto_primitive(
    Seat *seat, const char *atyp, const char *aname,
    void (*cb)(void *ctx, int result), void *ctx)
{ return seat->vt->confirm_weak_crypto_primitive(seat, atyp, aname, cb, ctx); }
static inline int seat_confirm_weak_cached_hostkey(
    Seat *seat, const char *aname, const char *better,
    void (*cb)(void *ctx, int result), void *ctx)
{ return seat->vt->confirm_weak_cached_hostkey(seat, aname, better, cb, ctx); }
static inline bool seat_is_utf8(Seat *seat)
{ return seat->vt->is_utf8(seat); }
static inline void seat_echoedit_update(Seat *seat, bool ec, bool ed)
{ seat->vt->echoedit_update(seat, ec, ed); }
static inline const char *seat_get_x_display(Seat *seat)
{ return seat->vt->get_x_display(seat); }
static inline bool seat_get_windowid(Seat *seat, long *id_out)
{ return seat->vt->get_windowid(seat, id_out); }
static inline bool seat_get_window_pixel_size(Seat *seat, int *w, int *h)
{ return seat->vt->get_window_pixel_size(seat, w, h); }
static inline StripCtrlChars *seat_stripctrl_new(
    Seat *seat, BinarySink *bs, SeatInteractionContext sic)
{ return seat->vt->stripctrl_new(seat, bs, sic); }
static inline bool seat_set_trust_status(Seat *seat, bool trusted)
{ return  seat->vt->set_trust_status(seat, trusted); }

/* Unlike the seat's actual method, the public entry point
 * seat_connection_fatal is a wrapper function with a printf-like API,
 * defined in misc.c. */
void seat_connection_fatal(Seat *seat, const char *fmt, ...);

/* Handy aliases for seat_output which set is_stderr to a fixed value. */
static inline size_t seat_stdout(Seat *seat, const void *data, size_t len)
{ return seat_output(seat, false, data, len); }
static inline size_t seat_stdout_pl(Seat *seat, ptrlen data)
{ return seat_output(seat, false, data.ptr, data.len); }
static inline size_t seat_stderr(Seat *seat, const void *data, size_t len)
{ return seat_output(seat, true, data, len); }
static inline size_t seat_stderr_pl(Seat *seat, ptrlen data)
{ return seat_output(seat, true, data.ptr, data.len); }

/*
 * Stub methods for seat implementations that want to use the obvious
 * null handling for a given method.
 *
 * These are generally obvious, except for is_utf8, where you might
 * plausibly want to return either fixed answer 'no' or 'yes'.
 */
size_t nullseat_output(
    Seat *seat, bool is_stderr, const void *data, size_t len);
bool nullseat_eof(Seat *seat);
int nullseat_get_userpass_input(Seat *seat, prompts_t *p, bufchain *input);
void nullseat_notify_remote_exit(Seat *seat);
void nullseat_connection_fatal(Seat *seat, const char *message);
void nullseat_update_specials_menu(Seat *seat);
char *nullseat_get_ttymode(Seat *seat, const char *mode);
void nullseat_set_busy_status(Seat *seat, BusyStatus status);
int nullseat_verify_ssh_host_key(
    Seat *seat, const char *host, int port,
    const char *keytype, char *keystr, char *key_fingerprint,
    void (*callback)(void *ctx, int result), void *ctx);
int nullseat_confirm_weak_crypto_primitive(
    Seat *seat, const char *algtype, const char *algname,
    void (*callback)(void *ctx, int result), void *ctx);
int nullseat_confirm_weak_cached_hostkey(
    Seat *seat, const char *algname, const char *betteralgs,
    void (*callback)(void *ctx, int result), void *ctx);
bool nullseat_is_never_utf8(Seat *seat);
bool nullseat_is_always_utf8(Seat *seat);
void nullseat_echoedit_update(Seat *seat, bool echoing, bool editing);
const char *nullseat_get_x_display(Seat *seat);
bool nullseat_get_windowid(Seat *seat, long *id_out);
bool nullseat_get_window_pixel_size(Seat *seat, int *width, int *height);
StripCtrlChars *nullseat_stripctrl_new(
        Seat *seat, BinarySink *bs_out, SeatInteractionContext sic);
bool nullseat_set_trust_status(Seat *seat, bool trusted);
bool nullseat_set_trust_status_vacuously(Seat *seat, bool trusted);

/*
 * Seat functions provided by the platform's console-application
 * support module (wincons.c, uxcons.c).
 */

void console_connection_fatal(Seat *seat, const char *message);
int console_verify_ssh_host_key(
    Seat *seat, const char *host, int port,
    const char *keytype, char *keystr, char *key_fingerprint,
    void (*callback)(void *ctx, int result), void *ctx);
int console_confirm_weak_crypto_primitive(
    Seat *seat, const char *algtype, const char *algname,
    void (*callback)(void *ctx, int result), void *ctx);
int console_confirm_weak_cached_hostkey(
    Seat *seat, const char *algname, const char *betteralgs,
    void (*callback)(void *ctx, int result), void *ctx);
StripCtrlChars *console_stripctrl_new(
        Seat *seat, BinarySink *bs_out, SeatInteractionContext sic);
bool console_set_trust_status(Seat *seat, bool trusted);

/*
 * Other centralised seat functions.
 */
int filexfer_get_userpass_input(Seat *seat, prompts_t *p, bufchain *input);

/*
 * Data type 'TermWin', which is a vtable encapsulating all the
 * functionality that Terminal expects from its containing terminal
 * window.
 */
struct TermWin {
    const struct TermWinVtable *vt;
};
struct TermWinVtable {
    /*
     * All functions listed here between setup_draw_ctx and
     * free_draw_ctx expect to be _called_ between them too, so that
     * the TermWin has a drawing context currently available.
     *
     * (Yes, even char_width, because e.g. the Windows implementation
     * of TermWin handles it by loading the currently configured font
     * into the HDC and doing a GDI query.)
     */
    bool (*setup_draw_ctx)(TermWin *);
    /* Draw text in the window, during a painting operation */
    void (*draw_text)(TermWin *, int x, int y, wchar_t *text, int len,
                      unsigned long attrs, int line_attrs, truecolour tc);
    /* Draw the visible cursor. Expects you to have called do_text
     * first (because it might just draw an underline over a character
     * presumed to exist already), but also expects you to pass in all
     * the details of the character under the cursor (because it might
     * redraw it in different colours). */
    void (*draw_cursor)(TermWin *, int x, int y, wchar_t *text, int len,
                        unsigned long attrs, int line_attrs, truecolour tc);
    /* Draw the sigil indicating that a line of text has come from
     * PuTTY itself rather than the far end (defence against end-of-
     * authentication spoofing) */
    void (*draw_trust_sigil)(TermWin *, int x, int y);
    int (*char_width)(TermWin *, int uc);
    void (*free_draw_ctx)(TermWin *);

    void (*set_cursor_pos)(TermWin *, int x, int y);

    void (*set_raw_mouse_mode)(TermWin *, bool enable);

    void (*set_scrollbar)(TermWin *, int total, int start, int page);

    void (*bell)(TermWin *, int mode);

    void (*clip_write)(TermWin *, int clipboard, wchar_t *text, int *attrs,
                       truecolour *colours, int len, bool must_deselect);
    void (*clip_request_paste)(TermWin *, int clipboard);

    void (*refresh)(TermWin *);

    void (*request_resize)(TermWin *, int w, int h);

    void (*set_title)(TermWin *, const char *title);
    void (*set_icon_title)(TermWin *, const char *icontitle);
    /* set_minimised and set_maximised are assumed to set two
     * independent settings, rather than a single three-way
     * {min,normal,max} switch. The idea is that when you un-minimise
     * the window it remembers whether to go back to normal or
     * maximised. */
    void (*set_minimised)(TermWin *, bool minimised);
    bool (*is_minimised)(TermWin *);
    void (*set_maximised)(TermWin *, bool maximised);
    void (*move)(TermWin *, int x, int y);
    void (*set_zorder)(TermWin *, bool top);

    bool (*palette_get)(TermWin *, int n, int *r, int *g, int *b);
    void (*palette_set)(TermWin *, int n, int r, int g, int b);
    void (*palette_reset)(TermWin *);

    void (*get_pos)(TermWin *, int *x, int *y);
    void (*get_pixels)(TermWin *, int *x, int *y);
    const char *(*get_title)(TermWin *, bool icon);
    bool (*is_utf8)(TermWin *);
};

static inline bool win_setup_draw_ctx(TermWin *win)
{ return win->vt->setup_draw_ctx(win); }
static inline void win_draw_text(
    TermWin *win, int x, int y, wchar_t *text, int len,
    unsigned long attrs, int line_attrs, truecolour tc)
{ win->vt->draw_text(win, x, y, text, len, attrs, line_attrs, tc); }
static inline void win_draw_cursor(
    TermWin *win, int x, int y, wchar_t *text, int len,
    unsigned long attrs, int line_attrs, truecolour tc)
{ win->vt->draw_cursor(win, x, y, text, len, attrs, line_attrs, tc); }
static inline void win_draw_trust_sigil(TermWin *win, int x, int y)
{ win->vt->draw_trust_sigil(win, x, y); }
static inline int win_char_width(TermWin *win, int uc)
{ return win->vt->char_width(win, uc); }
static inline void win_free_draw_ctx(TermWin *win)
{ win->vt->free_draw_ctx(win); }
static inline void win_set_cursor_pos(TermWin *win, int x, int y)
{ win->vt->set_cursor_pos(win, x, y); }
static inline void win_set_raw_mouse_mode(TermWin *win, bool enable)
{ win->vt->set_raw_mouse_mode(win, enable); }
static inline void win_set_scrollbar(TermWin *win, int t, int s, int p)
{ win->vt->set_scrollbar(win, t, s, p); }
static inline void win_bell(TermWin *win, int mode)
{ win->vt->bell(win, mode); }
static inline void win_clip_write(
    TermWin *win, int clipboard, wchar_t *text, int *attrs,
    truecolour *colours, int len, bool deselect)
{ win->vt->clip_write(win, clipboard, text, attrs, colours, len, deselect); }
static inline void win_clip_request_paste(TermWin *win, int clipboard)
{ win->vt->clip_request_paste(win, clipboard); }
static inline void win_refresh(TermWin *win)
{ win->vt->refresh(win); }
static inline void win_request_resize(TermWin *win, int w, int h)
{ win->vt->request_resize(win, w, h); }
static inline void win_set_title(TermWin *win, const char *title)
{ win->vt->set_title(win, title); }
static inline void win_set_icon_title(TermWin *win, const char *icontitle)
{ win->vt->set_icon_title(win, icontitle); }
static inline void win_set_minimised(TermWin *win, bool minimised)
{ win->vt->set_minimised(win, minimised); }
static inline bool win_is_minimised(TermWin *win)
{ return win->vt->is_minimised(win); }
static inline void win_set_maximised(TermWin *win, bool maximised)
{ win->vt->set_maximised(win, maximised); }
static inline void win_move(TermWin *win, int x, int y)
{ win->vt->move(win, x, y); }
static inline void win_set_zorder(TermWin *win, bool top)
{ win->vt->set_zorder(win, top); }
static inline bool win_palette_get(TermWin *win, int n, int *r, int *g, int *b)
{ return win->vt->palette_get(win, n, r, g, b); }
static inline void win_palette_set(TermWin *win, int n, int r, int g, int b)
{ win->vt->palette_set(win, n, r, g, b); }
static inline void win_palette_reset(TermWin *win)
{ win->vt->palette_reset(win); }
static inline void win_get_pos(TermWin *win, int *x, int *y)
{ win->vt->get_pos(win, x, y); }
static inline void win_get_pixels(TermWin *win, int *x, int *y)
{ win->vt->get_pixels(win, x, y); }
static inline const char *win_get_title(TermWin *win, bool icon)
{ return win->vt->get_title(win, icon); }
static inline bool win_is_utf8(TermWin *win)
{ return win->vt->is_utf8(win); }

/*
 * Global functions not specific to a connection instance.
 */
void nonfatal(const char *, ...);
NORETURN void modalfatalbox(const char *, ...);
NORETURN void cleanup_exit(int);

/*
 * Exports from conf.c, and a big enum (via parametric macro) of
 * configuration option keys.
 */
#define CONFIG_OPTIONS(X) \
    /* X(value-type, subkey-type, keyword) */ \
    X(STR, NONE, host) \
    X(INT, NONE, port) \
    X(INT, NONE, protocol) /* PROT_SSH, PROT_TELNET etc */ \
    X(INT, NONE, addressfamily) /* ADDRTYPE_IPV[46] or ADDRTYPE_UNSPEC */ \
    X(INT, NONE, close_on_exit) /* FORCE_ON, FORCE_OFF, AUTO */ \
    X(BOOL, NONE, warn_on_close) \
    X(INT, NONE, ping_interval) /* in seconds */ \
    X(BOOL, NONE, tcp_nodelay) \
    X(BOOL, NONE, tcp_keepalives) \
    X(STR, NONE, loghost) /* logical host being contacted, for host key check */ \
    /* Proxy options */ \
    X(STR, NONE, proxy_exclude_list) \
    X(INT, NONE, proxy_dns) /* FORCE_ON, FORCE_OFF, AUTO */ \
    X(BOOL, NONE, even_proxy_localhost) \
    X(INT, NONE, proxy_type) /* PROXY_NONE, PROXY_SOCKS4, ... */ \
    X(STR, NONE, proxy_host) \
    X(INT, NONE, proxy_port) \
    X(STR, NONE, proxy_username) \
    X(STR, NONE, proxy_password) \
    X(STR, NONE, proxy_telnet_command) \
    X(INT, NONE, proxy_log_to_term) /* FORCE_ON, FORCE_OFF, AUTO */ \
    /* SSH options */ \
    X(STR, NONE, remote_cmd) \
    X(STR, NONE, remote_cmd2) /* fallback if remote_cmd fails; never loaded or saved */ \
    X(BOOL, NONE, nopty) \
    X(BOOL, NONE, compression) \
    X(INT, INT, ssh_kexlist) \
    X(INT, INT, ssh_hklist) \
    X(INT, NONE, ssh_rekey_time) /* in minutes */ \
    X(STR, NONE, ssh_rekey_data) /* string encoding e.g. "100K", "2M", "1G" */ \
    X(BOOL, NONE, tryagent) \
    X(BOOL, NONE, agentfwd) \
    X(BOOL, NONE, change_username) /* allow username switching in SSH-2 */ \
    X(INT, INT, ssh_cipherlist) \
    X(FILENAME, NONE, keyfile) \
    /* \
     * Which SSH protocol to use. \
     * For historical reasons, the current legal values for CONF_sshprot \
     * are: \
     *  0 = SSH-1 only \
     *  3 = SSH-2 only \
     * We used to also support \
     *  1 = SSH-1 with fallback to SSH-2 \
     *  2 = SSH-2 with fallback to SSH-1 \
     * and we continue to use 0/3 in storage formats rather than the more \
     * obvious 1/2 to avoid surprises if someone saves a session and later \
     * downgrades PuTTY. So it's easier to use these numbers internally too. \
     */ \
    X(INT, NONE, sshprot) \
    X(BOOL, NONE, ssh2_des_cbc) /* "des-cbc" unrecommended SSH-2 cipher */ \
    X(BOOL, NONE, ssh_no_userauth) /* bypass "ssh-userauth" (SSH-2 only) */ \
    X(BOOL, NONE, ssh_show_banner) /* show USERAUTH_BANNERs (SSH-2 only) */ \
    X(BOOL, NONE, try_tis_auth) \
    X(BOOL, NONE, try_ki_auth) \
    X(BOOL, NONE, try_gssapi_auth) /* attempt gssapi auth via ssh userauth */ \
    X(BOOL, NONE, try_gssapi_kex) /* attempt gssapi auth via ssh kex */ \
    X(BOOL, NONE, gssapifwd) /* forward tgt via gss */ \
    X(INT, NONE, gssapirekey) /* KEXGSS refresh interval (mins) */ \
    X(INT, INT, ssh_gsslist) /* preference order for local GSS libs */ \
    X(FILENAME, NONE, ssh_gss_custom) \
    X(BOOL, NONE, ssh_subsys) /* run a subsystem rather than a command */ \
    X(BOOL, NONE, ssh_subsys2) /* fallback to go with remote_cmd_ptr2 */ \
    X(BOOL, NONE, ssh_no_shell) /* avoid running a shell */ \
    X(STR, NONE, ssh_nc_host) /* host to connect to in `nc' mode */ \
    X(INT, NONE, ssh_nc_port) /* port to connect to in `nc' mode */ \
    /* Telnet options */ \
    X(STR, NONE, termtype) \
    X(STR, NONE, termspeed) \
    X(STR, STR, ttymodes) /* values are "Vvalue" or "A" */ \
    X(STR, STR, environmt) \
    X(STR, NONE, username) \
    X(BOOL, NONE, username_from_env) \
    X(STR, NONE, localusername) \
    X(BOOL, NONE, rfc_environ) \
    X(BOOL, NONE, passive_telnet) \
    /* Serial port options */ \
    X(STR, NONE, serline) \
    X(INT, NONE, serspeed) \
    X(INT, NONE, serdatabits) \
    X(INT, NONE, serstopbits) \
    X(INT, NONE, serparity) /* SER_PAR_NONE, SER_PAR_ODD, ... */ \
    X(INT, NONE, serflow) /* SER_FLOW_NONE, SER_FLOW_XONXOFF, ... */ \
    /* Keyboard options */ \
    X(BOOL, NONE, bksp_is_delete) \
    X(BOOL, NONE, rxvt_homeend) \
    X(INT, NONE, funky_type) /* FUNKY_XTERM, FUNKY_LINUX, ... */ \
    X(BOOL, NONE, no_applic_c) /* totally disable app cursor keys */ \
    X(BOOL, NONE, no_applic_k) /* totally disable app keypad */ \
    X(BOOL, NONE, no_mouse_rep) /* totally disable mouse reporting */ \
    X(BOOL, NONE, no_remote_resize) /* disable remote resizing */ \
    X(BOOL, NONE, no_alt_screen) /* disable alternate screen */ \
    X(BOOL, NONE, no_remote_wintitle) /* disable remote retitling */ \
    X(BOOL, NONE, no_remote_clearscroll) /* disable ESC[3J */ \
    X(BOOL, NONE, no_dbackspace) /* disable destructive backspace */ \
    X(BOOL, NONE, no_remote_charset) /* disable remote charset config */ \
    X(INT, NONE, remote_qtitle_action) /* remote win title query action
                                       * (TITLE_NONE, TITLE_EMPTY, ...) */ \
    X(BOOL, NONE, app_cursor) \
    X(BOOL, NONE, app_keypad) \
    X(BOOL, NONE, nethack_keypad) \
    X(BOOL, NONE, telnet_keyboard) \
    X(BOOL, NONE, telnet_newline) \
    X(BOOL, NONE, alt_f4) /* is it special? */ \
    X(BOOL, NONE, alt_space) /* is it special? */ \
    X(BOOL, NONE, alt_only) /* is it special? */ \
    X(INT, NONE, localecho) /* FORCE_ON, FORCE_OFF, AUTO */ \
    X(INT, NONE, localedit) /* FORCE_ON, FORCE_OFF, AUTO */ \
    X(BOOL, NONE, alwaysontop) \
    X(BOOL, NONE, fullscreenonaltenter) \
    X(BOOL, NONE, scroll_on_key) \
    X(BOOL, NONE, scroll_on_disp) \
    X(BOOL, NONE, erase_to_scrollback) \
    X(BOOL, NONE, compose_key) \
    X(BOOL, NONE, ctrlaltkeys) \
    X(BOOL, NONE, osx_option_meta) \
    X(BOOL, NONE, osx_command_meta) \
    X(STR, NONE, wintitle) /* initial window title */ \
    /* Terminal options */ \
    X(INT, NONE, savelines) \
    X(BOOL, NONE, dec_om) \
    X(BOOL, NONE, wrap_mode) \
    X(BOOL, NONE, lfhascr) \
    X(INT, NONE, cursor_type) /* 0=block 1=underline 2=vertical */ \
    X(BOOL, NONE, blink_cur) \
    X(INT, NONE, beep) /* BELL_DISABLED, BELL_DEFAULT, ... */ \
    X(INT, NONE, beep_ind) /* B_IND_DISABLED, B_IND_FLASH, ... */ \
    X(BOOL, NONE, bellovl) /* bell overload protection active? */ \
    X(INT, NONE, bellovl_n) /* number of bells to cause overload */ \
    X(INT, NONE, bellovl_t) /* time interval for overload (seconds) */ \
    X(INT, NONE, bellovl_s) /* period of silence to re-enable bell (s) */ \
    X(FILENAME, NONE, bell_wavefile) \
    X(BOOL, NONE, scrollbar) \
    X(BOOL, NONE, scrollbar_in_fullscreen) \
    X(INT, NONE, resize_action) /* RESIZE_TERM, RESIZE_DISABLED, ... */ \
    X(BOOL, NONE, bce) \
    X(BOOL, NONE, blinktext) \
    X(BOOL, NONE, win_name_always) \
    X(INT, NONE, width) \
    X(INT, NONE, height) \
    X(FONT, NONE, font) \
    X(INT, NONE, font_quality) /* FQ_DEFAULT, FQ_ANTIALIASED, ... */ \
    X(FILENAME, NONE, logfilename) \
    X(INT, NONE, logtype) /* LGTYP_NONE, LGTYPE_ASCII, ... */ \
    X(INT, NONE, logxfovr) /* LGXF_OVR, LGXF_APN, LGXF_ASK */ \
    X(BOOL, NONE, logflush) \
    X(BOOL, NONE, logheader) \
    X(BOOL, NONE, logomitpass) \
    X(BOOL, NONE, logomitdata) \
    X(BOOL, NONE, hide_mouseptr) \
    X(BOOL, NONE, sunken_edge) \
    X(INT, NONE, window_border) /* in pixels */ \
    X(STR, NONE, answerback) \
    X(STR, NONE, printer) \
    X(BOOL, NONE, arabicshaping) \
    X(BOOL, NONE, bidi) \
    /* Colour options */ \
    X(BOOL, NONE, ansi_colour) \
    X(BOOL, NONE, xterm_256_colour) \
    X(BOOL, NONE, true_colour) \
    X(BOOL, NONE, system_colour) \
    X(BOOL, NONE, try_palette) \
    X(INT, NONE, bold_style) /* 1=font 2=colour (3=both) */ \
    X(INT, INT, colours) \
    /* Selection options */ \
    X(INT, NONE, mouse_is_xterm) /* 0=compromise 1=xterm 2=Windows */ \
    X(BOOL, NONE, rect_select) \
    X(BOOL, NONE, paste_controls) \
    X(BOOL, NONE, rawcnp) \
    X(BOOL, NONE, utf8linedraw) \
    X(BOOL, NONE, rtf_paste) \
    X(BOOL, NONE, mouse_override) \
    X(INT, INT, wordness) \
    X(BOOL, NONE, mouseautocopy) \
    X(INT, NONE, mousepaste) /* CLIPUI_IMPLICIT, CLIPUI_EXPLICIT, ... */ \
    X(INT, NONE, ctrlshiftins) /* CLIPUI_IMPLICIT, CLIPUI_EXPLICIT, ... */ \
    X(INT, NONE, ctrlshiftcv) /* CLIPUI_IMPLICIT, CLIPUI_EXPLICIT, ... */ \
    X(STR, NONE, mousepaste_custom) \
    X(STR, NONE, ctrlshiftins_custom) \
    X(STR, NONE, ctrlshiftcv_custom) \
    /* translations */ \
    X(INT, NONE, vtmode) /* VT_XWINDOWS, VT_OEMANSI, ... */ \
    X(STR, NONE, line_codepage) \
    X(BOOL, NONE, cjk_ambig_wide) \
    X(BOOL, NONE, utf8_override) \
    X(BOOL, NONE, xlat_capslockcyr) \
    /* X11 forwarding */ \
    X(BOOL, NONE, x11_forward) \
    X(STR, NONE, x11_display) \
    X(INT, NONE, x11_auth) /* X11_NO_AUTH, X11_MIT, X11_XDM */ \
    X(FILENAME, NONE, xauthfile) \
    /* port forwarding */ \
    X(BOOL, NONE, lport_acceptall) /* accept conns from hosts other than localhost */ \
    X(BOOL, NONE, rport_acceptall) /* same for remote forwarded ports (SSH-2 only) */ \
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
    /* SSH bug compatibility modes. All FORCE_ON/FORCE_OFF/AUTO */ \
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
    X(BOOL, NONE, ssh_simple) \
    X(BOOL, NONE, ssh_connection_sharing) \
    X(BOOL, NONE, ssh_connection_sharing_upstream) \
    X(BOOL, NONE, ssh_connection_sharing_downstream) \
    /*
     * ssh_manual_hostkeys is conceptually a set rather than a
     * dictionary: the string subkeys are the important thing, and the
     * actual values to which those subkeys map are all "".
     */ \
    X(STR, STR, ssh_manual_hostkeys) \
    /* Options for pterm. Should split out into platform-dependent part. */ \
    X(BOOL, NONE, stamp_utmp) \
    X(BOOL, NONE, login_shell) \
    X(BOOL, NONE, scrollbar_on_left) \
    X(BOOL, NONE, shadowbold) \
    X(FONT, NONE, boldfont) \
    X(FONT, NONE, widefont) \
    X(FONT, NONE, wideboldfont) \
    X(INT, NONE, shadowboldoffset) /* in pixels */ \
    X(BOOL, NONE, crhaslf) \
    X(STR, NONE, winclass) \
    /* end of list */

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
bool conf_get_bool(Conf *conf, int key);
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
void conf_set_bool(Conf *conf, int key, bool value);
void conf_set_int(Conf *conf, int key, int value);
void conf_set_int_int(Conf *conf, int key, int subkey, int value);
void conf_set_str(Conf *conf, int key, const char *value);
void conf_set_str_str(Conf *conf, int key,
		      const char *subkey, const char *val);
void conf_del_str_str(Conf *conf, int key, const char *subkey);
void conf_set_filename(Conf *conf, int key, const Filename *val);
void conf_set_fontspec(Conf *conf, int key, const FontSpec *val);
/* Serialisation functions for Duplicate Session */
void conf_serialise(BinarySink *bs, Conf *conf);
bool conf_deserialise(Conf *conf, BinarySource *src);/*returns true on success*/

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
void fontspec_serialise(BinarySink *bs, FontSpec *f);
FontSpec *fontspec_deserialise(BinarySource *src);

/*
 * Exports from noise.c.
 */
typedef enum NoiseSourceId {
    NOISE_SOURCE_TIME,
    NOISE_SOURCE_IOID,
    NOISE_SOURCE_IOLEN,
    NOISE_SOURCE_KEY,
    NOISE_SOURCE_MOUSEBUTTON,
    NOISE_SOURCE_MOUSEPOS,
    NOISE_SOURCE_MEMINFO,
    NOISE_SOURCE_STAT,
    NOISE_SOURCE_RUSAGE,
    NOISE_SOURCE_FGWINDOW,
    NOISE_SOURCE_CAPTURE,
    NOISE_SOURCE_CLIPBOARD,
    NOISE_SOURCE_QUEUE,
    NOISE_SOURCE_CURSORPOS,
    NOISE_SOURCE_THREADTIME,
    NOISE_SOURCE_PROCTIME,
    NOISE_SOURCE_PERFCOUNT,
    NOISE_MAX_SOURCES
} NoiseSourceId;
void noise_get_heavy(void (*func) (void *, int));
void noise_get_light(void (*func) (void *, int));
void noise_regular(void);
void noise_ultralight(NoiseSourceId id, unsigned long data);
void random_save_seed(void);
void random_destroy_seed(void);

/*
 * Exports from settings.c.
 *
 * load_settings() and do_defaults() return false if the provided
 * session name didn't actually exist. But they still fill in the
 * provided Conf with _something_.
 */
const struct BackendVtable *backend_vt_from_name(const char *name);
const struct BackendVtable *backend_vt_from_proto(int proto);
char *get_remote_username(Conf *conf); /* dynamically allocated */
char *save_settings(const char *section, Conf *conf);
void save_open_settings(settings_w *sesskey, Conf *conf);
bool load_settings(const char *section, Conf *conf);
void load_open_settings(settings_r *sesskey, Conf *conf);
void get_sesslist(struct sesslist *, bool allocate);
bool do_defaults(const char *, Conf *);
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
bool platform_default_b(const char *name, bool def);
int platform_default_i(const char *name, int def);
Filename *platform_default_filename(const char *name);
FontSpec *platform_default_fontspec(const char *name);

/*
 * Exports from terminal.c.
 */

Terminal *term_init(Conf *, struct unicode_data *, TermWin *);
void term_free(Terminal *);
void term_size(Terminal *, int, int, int);
void term_paint(Terminal *, int, int, int, int, bool);
void term_scroll(Terminal *, int, int);
void term_scroll_to_selection(Terminal *, int);
void term_pwron(Terminal *, bool);
void term_clrsb(Terminal *);
void term_mouse(Terminal *, Mouse_Button, Mouse_Button, Mouse_Action,
		int, int, bool, bool, bool);
void term_key(Terminal *, Key_Sym, wchar_t *, size_t, unsigned int,
	      unsigned int);
void term_lost_clipboard_ownership(Terminal *, int clipboard);
void term_update(Terminal *);
void term_invalidate(Terminal *);
void term_blink(Terminal *, bool set_cursor);
void term_do_paste(Terminal *, const wchar_t *, int);
void term_nopaste(Terminal *);
bool term_ldisc(Terminal *, int option);
void term_copyall(Terminal *, const int *, int);
void term_reconfig(Terminal *, Conf *);
void term_request_copy(Terminal *, const int *clipboards, int n_clipboards);
void term_request_paste(Terminal *, int clipboard);
void term_seen_key_event(Terminal *); 
size_t term_data(Terminal *, bool is_stderr, const void *data, size_t len);
void term_provide_backend(Terminal *term, Backend *backend);
void term_provide_logctx(Terminal *term, LogContext *logctx);
void term_set_focus(Terminal *term, bool has_focus);
char *term_get_ttymode(Terminal *term, const char *mode);
int term_get_userpass_input(Terminal *term, prompts_t *p, bufchain *input);
void term_set_trust_status(Terminal *term, bool trusted);

typedef enum SmallKeypadKey {
    SKK_HOME, SKK_END, SKK_INSERT, SKK_DELETE, SKK_PGUP, SKK_PGDN,
} SmallKeypadKey;
int format_arrow_key(char *buf, Terminal *term, int xkey, bool ctrl);
int format_function_key(char *buf, Terminal *term, int key_number,
                        bool shift, bool ctrl);
int format_small_keypad_key(char *buf, Terminal *term, SmallKeypadKey key);
int format_numeric_keypad_key(char *buf, Terminal *term, char key,
                              bool shift, bool ctrl);

/*
 * Exports from logging.c.
 */
struct LogPolicyVtable {
    /*
     * Pass Event Log entries on from LogContext to the front end,
     * which might write them to standard error or save them for a GUI
     * list box or other things.
     */
    void (*eventlog)(LogPolicy *lp, const char *event);

    /*
     * Ask what to do about the specified output log file already
     * existing. Can return four values:
     *
     *  - 2 means overwrite the log file
     *  - 1 means append to the log file
     *  - 0 means cancel logging for this session
     *  - -1 means please wait, and callback() will be called with one
     *    of those options.
     */
    int (*askappend)(LogPolicy *lp, Filename *filename,
                     void (*callback)(void *ctx, int result), void *ctx);

    /*
     * Emergency logging when the log file itself can't be opened,
     * which typically means we want to shout about it more loudly
     * than a mere Event Log entry.
     *
     * One reasonable option is to send it to the same place that
     * stderr output from the main session goes (so, either a console
     * tool's actual stderr, or a terminal window). In many cases this
     * is unlikely to cause this error message to turn up
     * embarrassingly in a log file of real server output, because the
     * whole point is that we haven't managed to open any such log
     * file :-)
     */
    void (*logging_error)(LogPolicy *lp, const char *event);
};
struct LogPolicy {
    const LogPolicyVtable *vt;
};

static inline void lp_eventlog(LogPolicy *lp, const char *event)
{ lp->vt->eventlog(lp, event); }
static inline int lp_askappend(
    LogPolicy *lp, Filename *filename,
    void (*callback)(void *ctx, int result), void *ctx)
{ return lp->vt->askappend(lp, filename, callback, ctx); }
static inline void lp_logging_error(LogPolicy *lp, const char *event)
{ lp->vt->logging_error(lp, event); }

LogContext *log_init(LogPolicy *lp, Conf *conf);
void log_free(LogContext *logctx);
void log_reconfig(LogContext *logctx, Conf *conf);
void logfopen(LogContext *logctx);
void logfclose(LogContext *logctx);
void logtraffic(LogContext *logctx, unsigned char c, int logmode);
void logflush(LogContext *logctx);
void logevent(LogContext *logctx, const char *event);
void logeventf(LogContext *logctx, const char *fmt, ...);
void logeventvf(LogContext *logctx, const char *fmt, va_list ap);

/*
 * Pass a dynamically allocated string to logevent and immediately
 * free it. Intended for use by wrapper macros which pass the return
 * value of dupprintf straight to this.
 */
void logevent_and_free(LogContext *logctx, char *event);
enum { PKT_INCOMING, PKT_OUTGOING };
enum { PKTLOG_EMIT, PKTLOG_BLANK, PKTLOG_OMIT };
struct logblank_t {
    int offset;
    int len;
    int type;
};
void log_packet(LogContext *logctx, int direction, int type,
		const char *texttype, const void *data, size_t len,
		int n_blanks, const struct logblank_t *blanks,
		const unsigned long *sequence,
                unsigned downstream_id, const char *additional_log_text);

/* This is defined by applications that have an obvious logging
 * destination like standard error or the GUI. */
extern LogPolicy default_logpolicy[1];

/*
 * Exports from testback.c
 */

extern const struct BackendVtable null_backend;
extern const struct BackendVtable loop_backend;

/*
 * Exports from raw.c.
 */

extern const struct BackendVtable raw_backend;

/*
 * Exports from rlogin.c.
 */

extern const struct BackendVtable rlogin_backend;

/*
 * Exports from telnet.c.
 */

extern const struct BackendVtable telnet_backend;

/*
 * Exports from ssh.c.
 */
extern const struct BackendVtable ssh_backend;

/*
 * Exports from ldisc.c.
 */
Ldisc *ldisc_create(Conf *, Terminal *, Backend *, Seat *);
void ldisc_configure(Ldisc *, Conf *);
void ldisc_free(Ldisc *);
void ldisc_send(Ldisc *, const void *buf, int len, bool interactive);
void ldisc_echoedit_update(Ldisc *);

/*
 * Exports from ldiscucs.c.
 */
void lpage_send(Ldisc *, int codepage, const char *buf, int len,
                bool interactive);
void luni_send(Ldisc *, const wchar_t * widebuf, int len, bool interactive);

/*
 * Exports from sshrand.c.
 */

void random_add_noise(NoiseSourceId source, const void *noise, int length);
void random_read(void *buf, size_t size);
void random_get_savedata(void **data, int *len);
extern int random_active;
/* The random number subsystem is activated if at least one other entity
 * within the program expresses an interest in it. So each SSH session
 * calls random_ref on startup and random_unref on shutdown. */
void random_ref(void);
void random_unref(void);
/* random_setup_special is used by PuTTYgen. It makes an extra-big
 * random number generator. */
void random_setup_special();
/* Manually drop a random seed into the random number generator, e.g.
 * just before generating a key. */
void random_reseed(ptrlen seed);
/* Limit on how much entropy is worth putting into the generator (bits). */
size_t random_seed_bits(void);

/*
 * Exports from pinger.c.
 */
typedef struct Pinger Pinger;
Pinger *pinger_new(Conf *conf, Backend *backend);
void pinger_reconfig(Pinger *, Conf *oldconf, Conf *newconf);
void pinger_free(Pinger *);

/*
 * Exports from misc.c.
 */

#include "misc.h"
bool conf_launchable(Conf *conf);
char const *conf_dest(Conf *conf);

/*
 * Exports from sessprep.c.
 */
void prepare_session(Conf *conf);

/*
 * Exports from sercfg.c.
 */
void ser_setup_config_box(struct controlbox *b, bool midsession,
			  int parity_mask, int flow_mask);

/*
 * Exports from version.c.
 */
extern const char ver[];
extern const char commitid[];

/*
 * Exports from unicode.c.
 */
#ifndef CP_UTF8
#define CP_UTF8 65001
#endif
/* void init_ucs(void); -- this is now in platform-specific headers */
bool is_dbcs_leadbyte(int codepage, char byte);
int mb_to_wc(int codepage, int flags, const char *mbstr, int mblen,
	     wchar_t *wcstr, int wclen);
int wc_to_mb(int codepage, int flags, const wchar_t *wcstr, int wclen,
	     char *mbstr, int mblen, const char *defchr,
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
 * Exports from pageantc.c.
 * 
 * agent_query returns NULL for here's-a-response, and non-NULL for
 * query-in- progress. In the latter case there will be a call to
 * `callback' at some future point, passing callback_ctx as the first
 * parameter and the actual reply data as the second and third.
 * 
 * The response may be a NULL pointer (in either of the synchronous
 * or asynchronous cases), which indicates failure to receive a
 * response.
 *
 * When the return from agent_query is not NULL, it identifies the
 * in-progress query in case it needs to be cancelled. If
 * agent_cancel_query is called, then the pending query is destroyed
 * and the callback will not be called. (E.g. if you're going to throw
 * away the thing you were using as callback_ctx.)
 *
 * Passing a null pointer as callback forces agent_query to behave
 * synchronously, i.e. it will block if necessary, and guarantee to
 * return NULL. The wrapper function agent_query_synchronous() makes
 * this easier.
 */
typedef struct agent_pending_query agent_pending_query;
agent_pending_query *agent_query(
    strbuf *in, void **out, int *outlen,
    void (*callback)(void *, void *, int), void *callback_ctx);
void agent_cancel_query(agent_pending_query *);
void agent_query_synchronous(strbuf *in, void **out, int *outlen);
bool agent_exists(void);

/*
 * Exports from wildcard.c
 */
const char *wc_error(int value);
int wc_match_pl(const char *wildcard, ptrlen target);
int wc_match(const char *wildcard, const char *target);
bool wc_unescape(char *output, const char *wildcard);

/*
 * Exports from frontend (windlg.c etc)
 */
void pgp_fingerprints(void);
/*
 * have_ssh_host_key() just returns true if a key of that type is
 * already cached and false otherwise.
 */
bool have_ssh_host_key(const char *host, int port, const char *keytype);

/*
 * Exports from console frontends (wincons.c, uxcons.c)
 * that aren't equivalents to things in windlg.c et al.
 */
extern bool console_batch_mode, console_antispoof_prompt;
int console_get_userpass_input(prompts_t *p);
bool is_interactive(void);
void console_print_error_msg(const char *prefix, const char *msg);
void console_print_error_msg_fmt_v(
    const char *prefix, const char *fmt, va_list ap);
void console_print_error_msg_fmt(const char *prefix, const char *fmt, ...);

/*
 * Exports from printing.c.
 */
typedef struct printer_enum_tag printer_enum;
typedef struct printer_job_tag printer_job;
printer_enum *printer_start_enum(int *nprinters);
char *printer_get_name(printer_enum *, int);
void printer_finish_enum(printer_enum *);
printer_job *printer_start_job(char *printer);
void printer_job_data(printer_job *, const void *, size_t);
void printer_finish_job(printer_job *);

/*
 * Exports from cmdline.c (and also cmdline_error(), which is
 * defined differently in various places and required _by_
 * cmdline.c).
 *
 * Note that cmdline_process_param takes a const option string, but a
 * writable argument string. That's not a mistake - that's so it can
 * zero out password arguments in the hope of not having them show up
 * avoidably in Unix 'ps'.
 */
int cmdline_process_param(const char *, char *, int, Conf *);
void cmdline_run_saved(Conf *);
void cmdline_cleanup(void);
int cmdline_get_passwd_input(prompts_t *p);
bool cmdline_host_ok(Conf *);
#define TOOLTYPE_FILETRANSFER 1
#define TOOLTYPE_NONNETWORK 2
#define TOOLTYPE_HOST_ARG 4
#define TOOLTYPE_HOST_ARG_CAN_BE_SESSION 8
#define TOOLTYPE_HOST_ARG_PROTOCOL_PREFIX 16
#define TOOLTYPE_HOST_ARG_FROM_LAUNCHABLE_LOAD 32
#define TOOLTYPE_PORT_ARG 64
extern int cmdline_tooltype;

void cmdline_error(const char *, ...);

/*
 * Exports from config.c.
 */
struct controlbox;
union control;
void conf_radiobutton_handler(union control *ctrl, dlgparam *dlg,
			      void *data, int event);
#define CHECKBOX_INVERT (1<<30)
void conf_checkbox_handler(union control *ctrl, dlgparam *dlg,
			   void *data, int event);
void conf_editbox_handler(union control *ctrl, dlgparam *dlg,
			  void *data, int event);
void conf_filesel_handler(union control *ctrl, dlgparam *dlg,
			  void *data, int event);
void conf_fontsel_handler(union control *ctrl, dlgparam *dlg,
			  void *data, int event);
/* Much more special-purpose function needed by sercfg.c */
void config_protocolbuttons_handler(union control *, dlgparam *, void *, int);

void setup_config_box(struct controlbox *b, bool midsession,
		      int protocol, int protcfginfo);

/*
 * Exports from minibidi.c.
 */
#define BIDI_CHAR_INDEX_NONE ((unsigned short)-1)
typedef struct bidi_char {
    unsigned int origwc, wc;
    unsigned short index, nchars;
} bidi_char;
int do_bidi(bidi_char *line, int count);
int do_shape(bidi_char *line, bidi_char *to, int count);
bool is_rtl(int c);

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
 * An enum for the copy-paste UI action configuration.
 */
enum {
    CLIPUI_NONE,     /* UI action has no copy/paste effect */
    CLIPUI_IMPLICIT, /* use the default clipboard implicit in mouse actions  */
    CLIPUI_EXPLICIT, /* use the default clipboard for explicit Copy/Paste */
    CLIPUI_CUSTOM,   /* use a named clipboard (on systems that support it) */
};

/*
 * Miscellaneous exports from the platform-specific code.
 *
 * filename_serialise and filename_deserialise have the same semantics
 * as fontspec_serialise and fontspec_deserialise above.
 */
Filename *filename_from_str(const char *string);
const char *filename_to_str(const Filename *fn);
bool filename_equal(const Filename *f1, const Filename *f2);
bool filename_is_null(const Filename *fn);
Filename *filename_copy(const Filename *fn);
void filename_free(Filename *fn);
void filename_serialise(BinarySink *bs, const Filename *f);
Filename *filename_deserialise(BinarySource *src);
char *get_username(void);	       /* return value needs freeing */
char *get_random_data(int bytes, const char *device); /* used in cmdgen.c */
char filename_char_sanitise(char c);   /* rewrite special pathname chars */
bool open_for_write_would_lose_data(const Filename *fn);

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
 * think it is. It returns true and places the time when the next
 * timer needs to go off in `next', or alternatively it returns
 * false if there are no timers at all pending.
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
 * Unix Plink, which uses timeouts to poll() in a way which for
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
bool run_timers(unsigned long now, unsigned long *next);
void timer_change_notify(unsigned long next);
unsigned long timing_last_clock(void);

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
 * actually running it (e.g. so as to put a zero timeout on a poll()
 * call) then it can call toplevel_callback_pending(), which will
 * return true if at least one callback is in the queue.
 *
 * run_toplevel_callbacks() returns true if it ran any actual code.
 * This can be used as a means of speculatively terminating a poll
 * loop, as in PSFTP, for example - if a callback has run then perhaps
 * it might have done whatever the loop's caller was waiting for.
 */
typedef void (*toplevel_callback_fn_t)(void *ctx);
void queue_toplevel_callback(toplevel_callback_fn_t fn, void *ctx);
bool run_toplevel_callbacks(void);
bool toplevel_callback_pending(void);
void delete_callbacks_for_context(void *ctx);

/*
 * Another facility in callback.c deals with 'idempotent' callbacks,
 * defined as those which never need to be scheduled again if they are
 * already scheduled and have not yet run. (An example would be one
 * which, when called, empties a queue of data completely: when data
 * is added to the queue, you must ensure a run of the queue-consuming
 * function has been scheduled, but if one is already pending, you
 * don't need to schedule a second one.)
 */
struct IdempotentCallback {
    toplevel_callback_fn_t fn;
    void *ctx;
    bool queued;
};
void queue_idempotent_callback(struct IdempotentCallback *ic);

typedef void (*toplevel_callback_notify_fn_t)(void *ctx);
void request_callback_notifications(toplevel_callback_notify_fn_t notify,
                                    void *ctx);

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
#ifndef HIGH_SURROGATE_START /* in some toolchains <winnls.h> defines these */
#define HIGH_SURROGATE_START 0xd800
#define HIGH_SURROGATE_END 0xdbff
#define LOW_SURROGATE_START 0xdc00
#define LOW_SURROGATE_END 0xdfff
#endif

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
