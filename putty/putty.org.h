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
typedef struct config_tag Config;
typedef struct backend_tag Backend;
typedef struct terminal_tag Terminal;
#endif

#include "puttyps.h"
#include "network.h"
#include "misc.h"

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
 * ATTR_INVALID is an illegal colour combination.
 */

#define TATTR_ACTCURS 	    0x4UL      /* active cursor (block) */
#define TATTR_PASCURS 	    0x2UL      /* passive cursor (box) */
#define TATTR_RIGHTCURS	    0x1UL      /* cursor-on-RHS */

#define LATTR_NORM   0x00000000UL
#define LATTR_WIDE   0x01000000UL
#define LATTR_TOP    0x02000000UL
#define LATTR_BOT    0x03000000UL
#define LATTR_MODE   0x03000000UL
#define LATTR_WRAPPED 0x10000000UL
#define LATTR_WRAPPED2 0x20000000UL

#define ATTR_INVALID 0x03FF0000UL

/* Like Linux use the F000 page for direct to font. */
#define ATTR_OEMCP   0x0000F000UL      /* OEM Codepage DTF */
#define ATTR_ACP     0x0000F100UL      /* Ansi Codepage DTF */

/* These are internal use overlapping with the UTF-16 surrogates */
#define ATTR_ASCII   0x0000D800UL      /* normal ASCII charset ESC ( B */
#define ATTR_LINEDRW 0x0000D900UL      /* line drawing charset ESC ( 0 */
#define ATTR_SCOACS  0x0000DA00UL      /* SCO Alternate charset */
#define ATTR_GBCHR   0x0000DB00UL      /* UK variant   charset ESC ( A */
#define CSET_MASK    0x0000FF00UL      /* Character set mask; MUST be 0xFF00 */

#define DIRECT_CHAR(c) ((c&0xFC00)==0xD800)
#define DIRECT_FONT(c) ((c&0xFE00)==0xF000)

#define UCSERR	     (ATTR_LINEDRW|'a')	/* UCS Format error character. */
/*
 * UCSWIDE is a special value used in the terminal data to signify
 * the character cell containing the right-hand half of a CJK wide
 * character. We use 0xDFFF because it's part of the surrogate
 * range and hence won't be used for anything else (it's impossible
 * to input it via UTF-8 because our UTF-8 decoder correctly
 * rejects surrogates).
 */
#define UCSWIDE	     0xDFFF

#define ATTR_NARROW  0x80000000UL
#define ATTR_WIDE    0x40000000UL
#define ATTR_BOLD    0x04000000UL
#define ATTR_UNDER   0x08000000UL
#define ATTR_REVERSE 0x10000000UL
#define ATTR_BLINK   0x20000000UL
#define ATTR_FGMASK  0x001F0000UL
#define ATTR_BGMASK  0x03E00000UL
#define ATTR_COLOURS 0x03FF0000UL
#define ATTR_FGSHIFT 16
#define ATTR_BGSHIFT 21

#define ATTR_DEFAULT 0x01280000UL      /* bg 9, fg 8 */
#define ATTR_DEFFG   0x00080000UL
#define ATTR_DEFBG   0x01200000UL
#define ERASE_CHAR   (ATTR_DEFAULT | ATTR_ASCII | ' ')
#define ATTR_MASK    0xFFFFFF00UL
#define CHAR_MASK    0x000000FFUL

#define ATTR_CUR_AND (~(ATTR_BOLD|ATTR_REVERSE|ATTR_BLINK|ATTR_COLOURS))
#define ATTR_CUR_XOR 0x016A0000UL

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

typedef enum {
    TS_AYT, TS_BRK, TS_SYNCH, TS_EC, TS_EL, TS_GA, TS_NOP, TS_ABORT,
    TS_AO, TS_IP, TS_SUSP, TS_EOR, TS_EOF, TS_LECHO, TS_RECHO, TS_PING,
    TS_EOL
} Telnet_Special;

struct telnet_special {
    const char *name;		       /* NULL==end, ""==separator */
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
     * SSH ciphers (both SSH1 and SSH2)
     */
    CIPHER_WARN,		       /* pseudo 'cipher' */
    CIPHER_3DES,
    CIPHER_BLOWFISH,
    CIPHER_AES,			       /* (SSH 2 only) */
    CIPHER_DES,
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
    /* Protocol back ends. (cfg.protocol) */
    PROT_RAW, PROT_TELNET, PROT_RLOGIN, PROT_SSH
};

enum {
    /* Bell settings (cfg.beep) */
    BELL_DISABLED, BELL_DEFAULT, BELL_VISUAL, BELL_WAVEFILE, BELL_PCSPEAKER
};

enum {
    /* Taskbar flashing indication on bell (cfg.beep_ind) */
    B_IND_DISABLED, B_IND_FLASH, B_IND_STEADY
};

enum {
    /* Resize actions (cfg.resize_action) */
    RESIZE_TERM, RESIZE_DISABLED, RESIZE_FONT, RESIZE_EITHER
};

enum {
    /* Function key types (cfg.funky_type) */
    FUNKY_TILDE,
    FUNKY_LINUX,
    FUNKY_XTERM,
    FUNKY_VT400,
    FUNKY_VT100P,
    FUNKY_SCO
};

struct backend_tag {
    const char *(*init) (void *frontend_handle, void **backend_handle,
			 Config *cfg,
			 char *host, int port, char **realhost, int nodelay,
			 int keepalive);
    void (*free) (void *handle);
    /* back->reconfig() passes in a replacement configuration. */
    void (*reconfig) (void *handle, Config *cfg);
    /* back->send() returns the current amount of buffered data. */
    int (*send) (void *handle, char *buf, int len);
    /* back->sendbuffer() does the same thing but without attempting a send */
    int (*sendbuffer) (void *handle);
    void (*size) (void *handle, int width, int height);
    void (*special) (void *handle, Telnet_Special code);
    const struct telnet_special *(*get_specials) (void *handle);
    Socket(*socket) (void *handle);
    int (*exitcode) (void *handle);
    int (*sendok) (void *handle);
    int (*ldisc) (void *handle, int);
    void (*provide_ldisc) (void *handle, void *ldisc);
    void (*provide_logctx) (void *handle, void *logctx);
    /*
     * back->unthrottle() tells the back end that the front end
     * buffer is clearing.
     */
    void (*unthrottle) (void *handle, int);
    int default_port;
};

extern struct backend_list {
    int protocol;
    char *name;
    Backend *backend;
} backends[];

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
 * IMPORTANT POLICY POINT: everything in this structure which wants
 * to be treated like an integer must be an actual, honest-to-
 * goodness `int'. No enum-typed variables. This is because parts
 * of the code will want to pass around `int *' pointers to them
 * and we can't run the risk of porting to some system on which the
 * enum comes out as a different size from int.
 */
struct config_tag {
    /* Basic options */
    char host[512];
    int port;
    int protocol;
    int close_on_exit;
    int warn_on_close;
    int ping_interval;		       /* in seconds */
    int tcp_nodelay;
    int tcp_keepalives;
    /* Proxy options */
    char proxy_exclude_list[512];
    int proxy_dns;
    int even_proxy_localhost;
    int proxy_type;
    char proxy_host[512];
    int proxy_port;
    char proxy_username[128];
    char proxy_password[128];
    char proxy_telnet_command[512];
    /* SSH options */
    char remote_cmd[512];
    char remote_cmd2[512];	       /* fallback if the first fails
					* (used internally for scp) */
    char *remote_cmd_ptr;	       /* might point to a larger command
				        * but never for loading/saving */
    char *remote_cmd_ptr2;	       /* might point to a larger command
				        * but never for loading/saving */
    int nopty;
    int compression;
    int agentfwd;
    int change_username;	       /* allow username switching in SSH2 */
    int ssh_cipherlist[CIPHER_MAX];
    Filename keyfile;
    int sshprot;		       /* use v1 or v2 when both available */
    int ssh2_des_cbc;		       /* "des-cbc" nonstandard SSH2 cipher */
    int try_tis_auth;
    int try_ki_auth;
#ifdef GSSAPI
    int try_gssapi_auth;
    int gssapi_fwd_tgt;
#endif
    int ssh_subsys;		       /* run a subsystem rather than a command */
    int ssh_subsys2;		       /* fallback to go with remote_cmd2 */
    /* Telnet options */
    char termtype[32];
    char termspeed[32];
    char environmt[1024];	       /* VAR\tvalue\0VAR\tvalue\0\0 */
    char username[100];
    char localusername[100];
    int rfc_environ;
    int passive_telnet;
    /* Keyboard options */
    int bksp_is_delete;
    int rxvt_homeend;
    int funky_type;
    int no_applic_c;		       /* totally disable app cursor keys */
    int no_applic_k;		       /* totally disable app keypad */
    int no_mouse_rep;		       /* totally disable mouse reporting */
    int no_remote_resize;	       /* disable remote resizing */
    int no_alt_screen;		       /* disable alternate screen */
    int no_remote_wintitle;	       /* disable remote retitling */
    int no_dbackspace;		       /* disable destructive backspace */
    int no_remote_charset;	       /* disable remote charset config */
    int no_remote_qtitle;	       /* disable remote win title query */
    int app_cursor;
    int app_keypad;
    int nethack_keypad;
    int telnet_keyboard;
    int telnet_newline;
    int alt_f4;			       /* is it special? */
    int alt_space;		       /* is it special? */
    int alt_only;		       /* is it special? */
    int localecho;
    int localedit;
    int alwaysontop;
    int fullscreenonaltenter;
    int scroll_on_key;
    int scroll_on_disp;
    int erase_to_scrollback;
    int compose_key;
    int ctrlaltkeys;
    char wintitle[256];		       /* initial window title */
    /* Terminal options */
    int savelines;
    int dec_om;
    int wrap_mode;
    int lfhascr;
    int cursor_type;		       /* 0=block 1=underline 2=vertical */
    int blink_cur;
    int beep;
    int beep_ind;
    int bellovl;		       /* bell overload protection active? */
    int bellovl_n;		       /* number of bells to cause overload */
    int bellovl_t;		       /* time interval for overload (seconds) */
    int bellovl_s;		       /* period of silence to re-enable bell (s) */
    Filename bell_wavefile;
    int scrollbar;
    int scrollbar_in_fullscreen;
    int resize_action;
    int bce;
    int blinktext;
    int win_name_always;
    int width, height;
    FontSpec font;
    Filename logfilename;
    int logtype;
    int logxfovr;
    int logomitpass;
    int logomitdata;
    int hide_mouseptr;
    int sunken_edge;
    int window_border;
    char answerback[256];
    char printer[128];
    int arabicshaping;
    int bidi;
    /* Colour options */
    int system_colour;
    int try_palette;
    int bold_colour;
    unsigned char colours[22][3];
    /* Selection options */
    int mouse_is_xterm;
    int rect_select;
    int rawcnp;
    int rtf_paste;
    int mouse_override;
    short wordness[256];
    /* translations */
    int vtmode;
    char line_codepage[128];
    int xlat_capslockcyr;
    /* X11 forwarding */
    int x11_forward;
    char x11_display[128];
    int x11_auth;
    /* port forwarding */
    int lport_acceptall; /* accept conns from hosts other than localhost */
    int rport_acceptall; /* same for remote forwarded ports (SSH2 only) */
    /*
     * The port forwarding string contains a number of
     * NUL-terminated substrings, terminated in turn by an empty
     * string (i.e. a second NUL immediately after the previous
     * one). Each string can be of one of the following forms:
     * 
     *   [LR]localport\thost:port
     *   [LR]localaddr:localport\thost:port
     *   Dlocalport
     *   Dlocaladdr:localport
     */
    char portfwd[1024];
    /* SSH bug compatibility modes */
    int sshbug_ignore1, sshbug_plainpw1, sshbug_rsa1,
	sshbug_hmac2, sshbug_derivekey2, sshbug_rsapad2,
	sshbug_dhgex2, sshbug_pksessid2;
    /* Options for pterm. Should split out into platform-dependent part. */
    int stamp_utmp;
    int login_shell;
    int scrollbar_on_left;
    int shadowbold;
    FontSpec boldfont;
    FontSpec widefont;
    FontSpec wideboldfont;
    int shadowboldoffset;
};

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

struct RSAKey;			       /* be a little careful of scope */

/*
 * Exports from window.c.
 */
void request_resize(void *frontend, int, int);
void do_text(Context, int, int, char *, int, unsigned long, int);
void do_cursor(Context, int, int, char *, int, unsigned long, int);
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
void write_clip(void *frontend, wchar_t *, int, int);
void get_clip(void *frontend, wchar_t **, int *);
void optimised_move(void *frontend, int, int, int);
void set_raw_mouse_mode(void *frontend, int);
void connection_fatal(void *frontend, char *, ...);
void fatalbox(char *, ...);
void modalfatalbox(char *, ...);
#ifdef macintosh
#pragma noreturn(fatalbox)
#pragma noreturn(modalfatalbox)
#endif
void beep(void *frontend, int);
void begin_session(void *frontend);
void sys_cursor(void *frontend, int x, int y);
void request_paste(void *frontend);
void frontend_keypress(void *frontend);
void ldisc_update(void *frontend, int echo, int edit);
void update_specials_menu(void *frontend);
int from_backend(void *frontend, int is_stderr, const char *data, int len);
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

void cleanup_exit(int);

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
char *save_settings(char *section, int do_host, Config * cfg);
void save_open_settings(void *sesskey, int do_host, Config *cfg);
void load_settings(char *section, int do_host, Config * cfg);
void load_open_settings(void *sesskey, int do_host, Config *cfg);
void get_sesslist(struct sesslist *, int allocate);
void do_defaults(char *, Config *);
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
 */
char *platform_default_s(const char *name);
int platform_default_i(const char *name, int def);
Filename platform_default_filename(const char *name);
FontSpec platform_default_fontspec(const char *name);

/*
 * Exports from terminal.c.
 */

Terminal *term_init(Config *, struct unicode_data *, void *);
void term_free(Terminal *);
void term_size(Terminal *, int, int, int);
void term_out(Terminal *);
void term_paint(Terminal *, Context, int, int, int, int, int);
void term_scroll(Terminal *, int, int);
void term_pwron(Terminal *);
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
int term_paste_pending(Terminal *);
void term_paste(Terminal *);
void term_nopaste(Terminal *);
int term_ldisc(Terminal *, int option);
void term_copyall(Terminal *);
void term_reconfig(Terminal *, Config *);
void term_seen_key_event(Terminal *); 
int term_data(Terminal *, int is_stderr, const char *data, int len);
void term_provide_resize_fn(Terminal *term,
			    void (*resize_fn)(void *, int, int),
			    void *resize_ctx);
void term_provide_logctx(Terminal *term, void *logctx);

/*
 * Exports from logging.c.
 */
void *log_init(void *frontend, Config *cfg);
void log_free(void *logctx);
void log_reconfig(void *logctx, Config *cfg);
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
		char *texttype, void *data, int len,
		int n_blanks, const struct logblank_t *blanks);

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
 * Exports from ssh.c. (NB the getline variables have to be GLOBAL
 * so that PuTTYtel will still compile - otherwise it would depend
 * on ssh.c.)
 */

#ifdef MPEXT
GLOBAL int (*ssh_get_line) (void *frontend, const char *prompt, char *str, int maxlen,
                            int is_pw);
#else
GLOBAL int (*ssh_get_line) (const char *prompt, char *str, int maxlen,
			    int is_pw);
#endif
GLOBAL int ssh_getline_pw_only;
extern Backend ssh_backend;

/*
 * Exports from ldisc.c.
 */
void *ldisc_create(Config *, Terminal *, Backend *, void *, void *);
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
void random_init(void);
int random_byte(void);
void random_get_savedata(void **data, int *len);
extern int random_active;

/*
 * Exports from misc.c.
 */

#include "misc.h"

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
int mb_to_wc(int codepage, int flags, char *mbstr, int mblen,
	     wchar_t *wcstr, int wclen);
int wc_to_mb(int codepage, int flags, wchar_t *wcstr, int wclen,
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
int wcwidth(wchar_t ucs);
int wcswidth(const wchar_t *pwcs, size_t n);

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
 * Exports from windlg.c
 */
void logevent(void *frontend, const char *);
void verify_ssh_host_key(void *frontend, char *host, int port, char *keytype,
			 char *keystr, char *fingerprint);
void askcipher(void *frontend, char *ciphername, int cs);
int askappend(void *frontend, Filename filename);

/*
 * Exports from console.c (that aren't equivalents to things in
 * windlg.c).
 */
extern int console_batch_mode;
int console_get_line(const char *prompt, char *str, int maxlen, int is_pw);
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
int cmdline_process_param(char *, char *, int, Config *);
void cmdline_run_saved(Config *);
void cmdline_cleanup(void);
extern char *cmdline_password;
#define TOOLTYPE_FILETRANSFER 1
#define TOOLTYPE_NONNETWORK 2
extern int cmdline_tooltype;

void cmdline_error(char *, ...);

/*
 * Exports from config.c.
 */
struct controlbox;
void setup_config_box(struct controlbox *b, struct sesslist *sesslist,
		      int midsession, int protocol);

/*
 * Exports from minibidi.c.
 */
typedef struct bidi_char {
    wchar_t origwc, wc;
    unsigned short index;
} bidi_char;
int do_bidi(bidi_char *line, int count);
int do_shape(bidi_char *line, bidi_char *to, int count);

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
 */
Filename filename_from_str(const char *string);
const char *filename_to_str(const Filename *fn);
int filename_equal(Filename f1, Filename f2);
int filename_is_null(Filename fn);
char *get_username(void);	       /* return value needs freeing */
char *get_random_data(int bytes);      /* used in cmdgen.c */

#endif
