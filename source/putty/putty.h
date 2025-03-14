#ifndef PUTTY_PUTTY_H
#define PUTTY_PUTTY_H

#include <stddef.h>                    /* for wchar_t */
#include <limits.h>                    /* for INT_MAX */

/*
 * Declared before including platform.h, because that will refer to it
 *
 * An enum for different types of file that a GUI file requester might
 * focus on. (Our requesters never _insist_ on a particular file type
 * or extension - there's always an escape hatch to select any file
 * you want - but the default can be configured.)
 */
typedef enum {
    FILTER_ALL_FILES, /* no particular focus */
    FILTER_KEY_FILES, /* .ppk */
    FILTER_DYNLIB_FILES, /* whatever the host platform uses as shared libs */
    FILTER_SOUND_FILES, /* whatever kind of sound file we can use as bell */
} FilereqFilter;

struct callback_set;

#include "defs.h"
#include "platform.h"
#include "network.h"
#include "misc.h"
#include "marshal.h"

/*
 * We express various time intervals in unsigned long minutes, but may need to
 * clip some values so that the resulting number of ticks does not overflow an
 * integer value.
 */
#define MAX_TICK_MINS   (INT_MAX / (60 * TICKSPERSEC))

/*
 * Fingerprints of the current and previous PGP master keys, to
 * establish a trust path between an executable and other files.
 */
#define PGP_MASTER_KEY_YEAR "2023"
#define PGP_MASTER_KEY_DETAILS "RSA, 4096-bit"
#define PGP_MASTER_KEY_FP                                  \
    "28D4 7C46 55E7 65A6 D827  AC66 B15D 9EFC 216B 06A1"
#define PGP_PREV_MASTER_KEY_YEAR "2021"
#define PGP_PREV_MASTER_KEY_DETAILS "RSA, 3072-bit"
#define PGP_PREV_MASTER_KEY_FP                                  \
    "A872 D42F 1660 890F 0E05  223E DD43 55EA AC11 19DE"

/*
 * Definitions of three separate indexing schemes for colour palette
 * entries.
 *
 * Why three? Because history, sorry.
 *
 * Two of the colour indexings are used in escape sequences. The
 * Linux-console style OSC P sequences for setting the palette use an
 * indexing in which the eight standard ANSI SGR colours come first,
 * then their bold versions, and then six extra colours for default
 * fg/bg and the terminal cursor. And the xterm OSC 4 sequences for
 * querying the palette use a related indexing in which the six extra
 * colours are pushed up to indices 256 and onwards, with the previous
 * 16 being the first part of the xterm 256-colour space, and 240
 * additional terminal-accessible colours inserted in the middle.
 *
 * The third indexing is the order that the colours appear in the
 * PuTTY configuration panel, and also the order in which they're
 * described in the saved session files. This order specifies the same
 * set of colours as the OSC P encoding, but in a different order,
 * with the default fg/bg colours (which users are most likely to want
 * to reconfigure) at the start, and the ANSI SGR colours coming
 * later.
 *
 * So all three indices really are needed, because all three appear in
 * protocols or file formats outside the PuTTY binary. (Changing the
 * saved-session encoding would have a backwards-compatibility impact;
 * also, if we ever do, it would be better to replace the numeric
 * indices with descriptive keywords.)
 *
 * Since the OSC 4 encoding contains the full set of colours used in
 * the terminal display, that's the encoding used by front ends to
 * store any actual data associated with their palette entries. So the
 * TermWin palette_set and palette_get_overrides methods use that
 * encoding, and so does the bitwise encoding of attribute words used
 * in terminal redraw operations.
 *
 * The Conf encoding, of course, is used by config.c and settings.c.
 *
 * The aim is that those two sections of the code should never need to
 * come directly into contact, and the only module that should have to
 * deal directly with the mapping between these colour encodings - or
 * to deal _at all_ with the intermediate OSC P encoding - is
 * terminal.c itself.
 */

#define CONF_NCOLOURS 22               /* 16 + 6 special ones */
#define OSCP_NCOLOURS 22               /* same as CONF, but different order */
#define OSC4_NCOLOURS 262              /* 256 + the same 6 special ones */

/* The list macro for the conf colours also gives the textual names
 * used in the GUI configurer */
#define CONF_COLOUR_LIST(X)                     \
    X(fg, "Default Foreground")                 \
    X(fg_bold, "Default Bold Foreground")       \
    X(bg, "Default Background")                 \
    X(bg_bold, "Default Bold Background")       \
    X(cursor_fg, "Cursor Text")                 \
    X(cursor_bg, "Cursor Colour")               \
    X(black, "ANSI Black")                      \
    X(black_bold, "ANSI Black Bold")            \
    X(red, "ANSI Red")                          \
    X(red_bold, "ANSI Red Bold")                \
    X(green, "ANSI Green")                      \
    X(green_bold, "ANSI Green Bold")            \
    X(yellow, "ANSI Yellow")                    \
    X(yellow_bold, "ANSI Yellow Bold")          \
    X(blue, "ANSI Blue")                        \
    X(blue_bold, "ANSI Blue Bold")              \
    X(magenta, "ANSI Magenta")                  \
    X(magenta_bold, "ANSI Magenta Bold")        \
    X(cyan, "ANSI Cyan")                        \
    X(cyan_bold, "ANSI Cyan Bold")              \
    X(white, "ANSI White")                      \
    X(white_bold, "ANSI White Bold")            \
    /* end of list */

#define OSCP_COLOUR_LIST(X)                     \
    X(black)                                    \
    X(red)                                      \
    X(green)                                    \
    X(yellow)                                   \
    X(blue)                                     \
    X(magenta)                                  \
    X(cyan)                                     \
    X(white)                                    \
    X(black_bold)                               \
    X(red_bold)                                 \
    X(green_bold)                               \
    X(yellow_bold)                              \
    X(blue_bold)                                \
    X(magenta_bold)                             \
    X(cyan_bold)                                \
    X(white_bold)                               \
    /*
     * In the OSC 4 indexing, this is where the extra 240 colours go.
     * They consist of:
     *
     *  - 216 colours forming a 6x6x6 cube, with R the most
     *    significant colour and G the least. In other words, these
     *    occupy the space of indices 16 <= i < 232, with each
     *    individual colour found as i = 16 + 36*r + 6*g + b, for all
     *    0 <= r,g,b <= 5.
     *
     *  - The remaining indices, 232 <= i < 256, consist of a uniform
     *    series of grey shades running between black and white (but
     *    not including either, since actual black and white are
     *    already provided in the previous colour cube).
     *
     * After that, we have the remaining 6 special colours:
     */                                         \
    X(fg)                                       \
    X(fg_bold)                                  \
    X(bg)                                       \
    X(bg_bold)                                  \
    X(cursor_fg)                                \
    X(cursor_bg)                                \
    /* end of list */

/* Enumerations of the colour lists. These are available everywhere in
 * the code. The OSC P encoding shouldn't be used outside terminal.c,
 * but the easiest way to define the OSC 4 enum is to have the OSC P
 * one available to compute with. */
enum {
    #define ENUM_DECL(id,name) CONF_COLOUR_##id,
    CONF_COLOUR_LIST(ENUM_DECL)
    #undef ENUM_DECL
};
enum {
    #define ENUM_DECL(id) OSCP_COLOUR_##id,
    OSCP_COLOUR_LIST(ENUM_DECL)
    #undef ENUM_DECL
};
enum {
    #define ENUM_DECL(id) OSC4_COLOUR_##id = \
        OSCP_COLOUR_##id + (OSCP_COLOUR_##id >= 16 ? 240 : 0),
    OSCP_COLOUR_LIST(ENUM_DECL)
    #undef ENUM_DECL
};

/* Mapping tables defined in terminal.c */
extern const int colour_indices_conf_to_oscp[CONF_NCOLOURS];
extern const int colour_indices_conf_to_osc4[CONF_NCOLOURS];
extern const int colour_indices_oscp_to_osc4[OSCP_NCOLOURS];

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

#define TATTR_ACTCURS       0x40000000UL      /* active cursor (block) */
#define TATTR_PASCURS       0x20000000UL      /* passive cursor (box) */
#define TATTR_RIGHTCURS     0x10000000UL      /* cursor-on-RHS */
#define TATTR_COMBINING     0x80000000UL      /* combining characters */

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

/* Use the DC00 page for direct to font. */
#define CSET_OEMCP   0x0000DC00UL      /* OEM Codepage DTF */
#define CSET_ACP     0x0000DD00UL      /* Ansi Codepage DTF */

/* These are internal use overlapping with the UTF-16 surrogates */
#define CSET_ASCII   0x0000D800UL      /* normal ASCII charset ESC ( B */
#define CSET_LINEDRW 0x0000D900UL      /* line drawing charset ESC ( 0 */
#define CSET_SCOACS  0x0000DA00UL      /* SCO Alternate charset */
#define CSET_GBCHR   0x0000DB00UL      /* UK variant   charset ESC ( A */
#define CSET_MASK    0xFFFFFF00UL      /* Character set mask */

#define DIRECT_CHAR(c) ((c&0xFFFFFC00)==0xD800)
#define DIRECT_FONT(c) ((c&0xFFFFFE00)==0xDC00)

#define UCSERR       (CSET_LINEDRW|'a') /* UCS Format error character. */
/*
 * UCSWIDE is a special value used in the terminal data to signify
 * the character cell containing the right-hand half of a CJK wide
 * character. We use 0xDFFF because it's part of the surrogate
 * range and hence won't be used for anything else (it's impossible
 * to input it via UTF-8 because our UTF-8 decoder correctly
 * rejects surrogates).
 */
#define UCSWIDE      0xDFFF

#define ATTR_NARROW  0x0800000U
#define ATTR_WIDE    0x0400000U
#define ATTR_BOLD    0x0040000U
#define ATTR_UNDER   0x0080000U
#define ATTR_REVERSE 0x0100000U
#define ATTR_BLINK   0x0200000U
#define ATTR_FGMASK  0x00001FFU /* stores a colour in OSC 4 indexing */
#define ATTR_BGMASK  0x003FE00U /* stores a colour in OSC 4 indexing */
#define ATTR_COLOURS 0x003FFFFU
#define ATTR_DIM     0x1000000U
#define ATTR_STRIKE  0x2000000U
#define ATTR_FGSHIFT 0
#define ATTR_BGSHIFT 9

#define ATTR_DEFFG   (OSC4_COLOUR_fg << ATTR_FGSHIFT)
#define ATTR_DEFBG   (OSC4_COLOUR_bg << ATTR_BGSHIFT)
#define ATTR_DEFAULT (ATTR_DEFFG | ATTR_DEFBG)

struct sesslist {
    int nsessions;
    const char **sessions;
    char *buffer;                      /* so memory can be freed later */
};

struct unicode_data {
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

#define LGXF_OVR  1                    /* existing logfile overwrite */
#define LGXF_APN  0                    /* existing logfile append */
#define LGXF_ASK -1                    /* existing logfile ask */
#define LGTYP_NONE  0                  /* logmode: no logging */
#define LGTYP_ASCII 1                  /* logmode: pure ascii */
#define LGTYP_DEBUG 2                  /* logmode: all chars of traffic */
#define LGTYP_PACKETS 3                /* logmode: SSH data packets */
#define LGTYP_SSHRAW 4                 /* logmode: SSH raw data */

/* Platform-generic function to set up a struct unicode_data. This is
 * only likely to be useful to test programs; real clients will want
 * to use the more flexible per-platform setup functions. */
void init_ucs_generic(Conf *conf, struct unicode_data *ucsdata);

/*
 * Enumeration of 'special commands' that can be sent during a
 * session, separately from the byte stream of ordinary session data.
 */
typedef enum {
    /* The list of enum constants is defined in a separate header so
     * they can be reused in other contexts */
    #define SPECIAL(x) SS_ ## x,
    #include "specials.h"
    #undef SPECIAL
} SessionSpecialCode;

/*
 * The structure type returned from backend_get_specials.
 */
struct SessionSpecial {
    const char *name;
    SessionSpecialCode code;
    int arg;
};

/* Needed by both ssh/channel.h and ssh/ppl.h */
typedef void (*add_special_fn_t)(
    void *ctx, const char *text, SessionSpecialCode code, int arg);

typedef enum {
    MBT_NOTHING,
    MBT_LEFT, MBT_MIDDLE, MBT_RIGHT,   /* `raw' button designations */
    MBT_SELECT, MBT_EXTEND, MBT_PASTE, /* `cooked' button designations */
    MBT_WHEEL_UP, MBT_WHEEL_DOWN,      /* vertical mouse wheel */
    MBT_WHEEL_LEFT, MBT_WHEEL_RIGHT    /* horizontal mouse wheel */
} Mouse_Button;

typedef enum {
    MA_NOTHING, MA_CLICK, MA_2CLK, MA_3CLK, MA_DRAG, MA_RELEASE, MA_MOVE
} Mouse_Action;

/* Keyboard modifiers -- keys the user is actually holding down */

#define PKM_SHIFT       0x01
#define PKM_CONTROL     0x02
#define PKM_META        0x04
#define PKM_ALT         0x08

/* Keyboard flags that aren't really modifiers */
#define PKF_CAPSLOCK    0x10
#define PKF_NUMLOCK     0x20
#define PKF_REPEAT      0x40

/* Stand-alone keysyms for function keys */

typedef enum {
    PK_NULL,            /* No symbol for this key */
    /* Main keypad keys */
    PK_ESCAPE, PK_TAB, PK_BACKSPACE, PK_RETURN, PK_COMPOSE,
    /* Editing keys */
    PK_HOME, PK_INSERT, PK_DELETE, PK_END, PK_PAGEUP, PK_PAGEDOWN,
    /* Cursor keys */
    PK_UP, PK_DOWN, PK_RIGHT, PK_LEFT, PK_REST,
    /* Numeric keypad */                        /* Real one looks like: */
    PK_PF1, PK_PF2, PK_PF3, PK_PF4,             /* PF1 PF2 PF3 PF4 */
    PK_KPCOMMA, PK_KPMINUS, PK_KPDECIMAL,       /*  7   8   9   -  */
    PK_KP0, PK_KP1, PK_KP2, PK_KP3, PK_KP4,     /*  4   5   6   ,  */
    PK_KP5, PK_KP6, PK_KP7, PK_KP8, PK_KP9,     /*  1   2   3  en- */
    PK_KPBIGPLUS, PK_KPENTER,                   /*    0     .  ter */
    /* Top row */
    PK_F1,  PK_F2,  PK_F3,  PK_F4,  PK_F5,
    PK_F6,  PK_F7,  PK_F8,  PK_F9,  PK_F10,
    PK_F11, PK_F12, PK_F13, PK_F14, PK_F15,
    PK_F16, PK_F17, PK_F18, PK_F19, PK_F20,
    PK_PAUSE
} Key_Sym;

#define PK_ISEDITING(k) ((k) >= PK_HOME && (k) <= PK_PAGEDOWN)
#define PK_ISCURSOR(k)  ((k) >= PK_UP && (k) <= PK_REST)
#define PK_ISKEYPAD(k)  ((k) >= PK_PF1 && (k) <= PK_KPENTER)
#define PK_ISFKEY(k)    ((k) >= PK_F1 && (k) <= PK_F20)

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
    KEX_DHGROUP15,
    KEX_DHGROUP16,
    KEX_DHGROUP17,
    KEX_DHGROUP18,
    KEX_DHGEX,
    KEX_RSA,
    KEX_ECDH,
    KEX_NTRU_HYBRID,
    KEX_MLKEM_25519_HYBRID,
    KEX_MLKEM_NIST_HYBRID,
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
    HK_ED448,
    HK_MAX
};

enum {
    /*
     * SSH ciphers (both SSH-1 and SSH-2)
     */
    CIPHER_WARN,                       /* pseudo 'cipher' */
    CIPHER_3DES,
    CIPHER_BLOWFISH,
    CIPHER_AES,                        /* (SSH-2 only) */
    CIPHER_DES,
    CIPHER_ARCFOUR,
    CIPHER_CHACHA20,
    CIPHER_AESGCM,
    CIPHER_MAX                         /* no. ciphers (inc warn) */
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
    PROXY_HTTP, PROXY_TELNET, PROXY_CMD, PROXY_SSH_TCPIP,
    PROXY_SSH_EXEC, PROXY_SSH_SUBSYSTEM,
    PROXY_FUZZ
};

enum {
    /*
     * Line discipline options which the backend might try to control.
     */
    LD_EDIT,                           /* local line editing */
    LD_ECHO,                           /* local echo */
    LD_N_OPTIONS
};

enum {
    /* Actions on remote window title query */
    TITLE_NONE, TITLE_EMPTY, TITLE_REAL
};

enum {
    /* SUPDUP character set options */
    SUPDUP_CHARSET_ASCII, SUPDUP_CHARSET_ITS, SUPDUP_CHARSET_WAITS
};

enum {
    /* Protocol back ends. (CONF_protocol) */
    PROT_RAW, PROT_TELNET, PROT_RLOGIN, PROT_SSH, PROT_SSHCONN,
    /* PROT_SERIAL is supported on a subset of platforms, but it doesn't
     * hurt to define it globally. */
    PROT_SERIAL,
    /* PROT_SUPDUP is the historical RFC 734 protocol. */
    PROT_SUPDUP,
    PROTOCOL_LIMIT, /* upper bound on number of protocols */
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
    /* Mouse-button assignments */
    MOUSE_COMPROMISE, /* xterm-ish but with paste on RB in case no MB exists */
    MOUSE_XTERM, /* xterm-style: MB pastes, RB extends selection */
    MOUSE_WINDOWS /* Windows-style: RB brings up menu. MB still extends. */
};

enum {
    /* Function key types (CONF_funky_type) */
    FUNKY_TILDE,
    FUNKY_LINUX,
    FUNKY_XTERM,
    FUNKY_VT400,
    FUNKY_VT100P,
    FUNKY_SCO,
    FUNKY_XTERM_216
};

enum {
    /* Shifted arrow key types (CONF_sharrow_type) */
    SHARROW_APPLICATION,  /* Ctrl flips between ESC O A and ESC [ A */
    SHARROW_BITMAP        /* ESC [ 1 ; n A, where n = 1 + bitmap of CAS */
};

enum {
    FQ_DEFAULT, FQ_ANTIALIASED, FQ_NONANTIALIASED, FQ_CLEARTYPE
};

enum {
    CURSOR_BLOCK, CURSOR_UNDERLINE, CURSOR_VERTICAL_LINE
};

enum {
    /* these are really bit flags */
    BOLD_STYLE_FONT = 1,
    BOLD_STYLE_COLOUR = 2,
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

/* Backend flags */
#define BACKEND_RESIZE_FORBIDDEN    0x01   /* Backend does not allow
                                              resizing terminal */
#define BACKEND_NEEDS_TERMINAL      0x02   /* Backend must have terminal */
#define BACKEND_SUPPORTS_NC_HOST    0x04   /* Backend can honour
                                              CONF_ssh_nc_host */
#define BACKEND_NOTIFIES_SESSION_START 0x08 /* Backend will call
                                               seat_notify_session_started */

/* In (no)sshproxy.c */
extern const bool ssh_proxy_supported;

/*
 * This structure type wraps a Seat pointer, in a way that has no
 * purpose except to be a different type.
 *
 * The Seat wrapper functions that present interactive prompts all
 * expect one of these in place of their ordinary Seat pointer. You
 * get one by calling interactor_announce (defined below), which will
 * print a message (if not already done) identifying the Interactor
 * that originated the prompt.
 *
 * This arranges that the C type system itself will check that no call
 * to any of those Seat methods has omitted the mandatory call to
 * interactor_announce beforehand.
 */
struct InteractionReadySeat {
    Seat *seat;
};

/*
 * The Interactor trait is implemented by anything that is capable of
 * presenting interactive prompts or questions to the user during
 * network connection setup. Every Backend that ever needs to do this
 * is an Interactor, but also, while a Backend is making its initial
 * network connection, it may go via network proxy code which is also
 * an Interactor and can ask questions of its own.
 */
struct Interactor {
    const InteractorVtable *vt;

    /* The parent Interactor that we are a proxy for, if any. */
    Interactor *parent;

    /*
     * If we're the top-level Interactor (parent==NULL), then this
     * field records the last Interactor that actually did anything
     * interactive, so that we know when to announce a changeover
     * between levels of proxying.
     *
     * If parent != NULL, this field is not used.
     */
    Interactor *last_to_talk;
};

struct InteractorVtable {
    /*
     * Returns a user-facing description of the nature of the network
     * connection being made. Used in interactive proxy authentication
     * to announce which connection attempt is now in control of the
     * Seat.
     *
     * The idea is not just to be written in natural language, but to
     * connect with the user's idea of _why_ they think some
     * connection is being made. For example, instead of saying 'TCP
     * connection to 123.45.67.89 port 22', you might say 'SSH
     * connection to [logical host name for SSH host key purposes]'.
     *
     * The returned string must be freed by the caller.
     */
    char *(*description)(Interactor *itr);

    /*
     * Returns the LogPolicy associated with this Interactor. (A
     * Backend can derive this from its logging context; a proxy
     * Interactor inherits it from the Interactor for the parent
     * network connection.)
     */
    LogPolicy *(*logpolicy)(Interactor *itr);

    /*
     * Gets and sets the Seat that this Interactor talks to. When a
     * Seat is borrowed and replaced with a TempSeat, this will be the
     * mechanism by which that replacement happens.
     */
    Seat *(*get_seat)(Interactor *itr);
    void (*set_seat)(Interactor *itr, Seat *seat);
};

static inline char *interactor_description(Interactor *itr)
{ return itr->vt->description(itr); }
static inline LogPolicy *interactor_logpolicy(Interactor *itr)
{ return itr->vt->logpolicy(itr); }
static inline Seat *interactor_get_seat(Interactor *itr)
{ return itr->vt->get_seat(itr); }
static inline void interactor_set_seat(Interactor *itr, Seat *seat)
{ itr->vt->set_seat(itr, seat); }

static inline void interactor_set_child(Interactor *parent, Interactor *child)
{ child->parent = parent; }
Seat *interactor_borrow_seat(Interactor *itr);
void interactor_return_seat(Interactor *itr);
InteractionReadySeat interactor_announce(Interactor *itr);

/* Interactors that are Backends will find this helper function useful
 * in constructing their description strings */
char *default_description(const BackendVtable *backvt,
                          const char *host, int port);

/*
 * The Backend trait is the top-level one that governs each of the
 * user-facing main modes that PuTTY can use to talk to some
 * destination: SSH, Telnet, serial port, pty, etc.
 */

struct Backend {
    const BackendVtable *vt;

    /* Many Backends are also Interactors. If this one is, a pointer
     * to its Interactor trait lives here. */
    Interactor *interactor;
};
struct BackendVtable {
    char *(*init) (const BackendVtable *vt, Seat *seat,
                   Backend **backend_out, LogContext *logctx, Conf *conf,
                   const char *host, int port, char **realhost,
                   bool nodelay, bool keepalive);

    void (*free) (Backend *be);
    /* Pass in a replacement configuration. */
    void (*reconfig) (Backend *be, Conf *conf);
    void (*send) (Backend *be, const char *buf, size_t len);
    /* sendbuffer() returns the current amount of buffered data */
    size_t (*sendbuffer) (Backend *be);
    void (*size) (Backend *be, int width, int height);
    void (*special) (Backend *be, SessionSpecialCode code, int arg);
    const SessionSpecial *(*get_specials) (Backend *be);
    bool (*connected) (Backend *be);
    int (*exitcode) (Backend *be);
    /* If back->sendok() returns false, the backend doesn't currently
     * want input data, so the frontend should avoid acquiring any if
     * possible (passing back-pressure on to its sender).
     *
     * Policy rule: no backend shall return true from sendok() while
     * its network connection attempt is still ongoing. This ensures
     * that if making the network connection involves a proxy type
     * which wants to interact with the user via the terminal, the
     * proxy implementation and the backend itself won't fight over
     * who gets the terminal input. */
    bool (*sendok) (Backend *be);
    bool (*ldisc_option_state) (Backend *be, int);
    void (*provide_ldisc) (Backend *be, Ldisc *ldisc);
    /* Tells the back end that the front end  buffer is clearing. */
    void (*unthrottle) (Backend *be, size_t bufsize);
    int (*cfg_info) (Backend *be);

    /* Only implemented in the SSH protocol: check whether a
     * connection-sharing upstream exists for a given configuration. */
    bool (*test_for_upstream)(const char *host, int port, Conf *conf);
    /* Special-purpose function to return additional information to put
     * in a "are you sure you want to close this session" dialog;
     * return NULL if no such info, otherwise caller must free.
     * Only implemented in the SSH protocol, to warn about downstream
     * connections that would be lost if this one were terminated. */
    char *(*close_warn_text)(Backend *be);

    /* 'id' is a machine-readable name for the backend, used in
     * saved-session storage. 'displayname_tc' and 'displayname_lc'
     * are human-readable names, one in title-case for config boxes,
     * and one in lower-case for use in mid-sentence. */
    const char *id, *displayname_tc, *displayname_lc;

    int protocol;
    int default_port;
    unsigned flags;

    /* Only relevant for the serial protocol: bit masks of which
     * parity and flow control settings are supported. */
    unsigned serial_parity_mask, serial_flow_mask;
};

static inline char *backend_init(
    const BackendVtable *vt, Seat *seat, Backend **out, LogContext *logctx,
    Conf *conf, const char *host, int port, char **rhost, bool nd, bool ka)
{ return vt->init(vt, seat, out, logctx, conf, host, port, rhost, nd, ka); }
static inline void backend_free(Backend *be)
{ be->vt->free(be); }
static inline void backend_reconfig(Backend *be, Conf *conf)
{ be->vt->reconfig(be, conf); }
static inline void backend_send(Backend *be, const char *buf, size_t len)
{ be->vt->send(be, buf, len); }
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
 * In programs with a config UI, only the first few members of
 * backends[] will be displayed at the top-level; the others will be
 * relegated to a drop-down.
 */
extern const size_t n_ui_backends;

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
 * Used by callback.c; declared up here so that prompts_t can use it
 */
typedef void (*toplevel_callback_fn_t)(void *ctx);

/* Enum of result types in SeatPromptResult below */
typedef enum SeatPromptResultKind {
    /* Answer not yet available at all; either try again later or wait
     * for a callback (depending on the request's API) */
    SPRK_INCOMPLETE,

    /* We're abandoning the connection because the user interactively
     * told us to. (Hence, no need to present an error message
     * telling the user we're doing that: they already know.) */
    SPRK_USER_ABORT,

    /* We're abandoning the connection for some other reason (e.g. we
     * were unable to present the prompt at all, or a batch-mode
     * configuration told us to give the answer no). This may
     * ultimately have stemmed from some user configuration, but they
     * didn't _tell us right now_ to abandon this connection, so we
     * still need to inform them that we've done so. */
    SPRK_SW_ABORT,

    /* We're proceeding with the connection and have all requested
     * information (if any) */
    SPRK_OK
} SeatPromptResultKind;

/* Small struct to present the results of interactive requests from
 * backend to Seat (see below) */
struct SeatPromptResult {
    SeatPromptResultKind kind;

    /*
     * In the case of SPRK_SW_ABORT, the frontend provides an error
     * message to present to the user. But dynamically allocating it
     * up front would mean having to make sure it got freed at any
     * call site where one of these structs is received (and freed
     * _once_ no matter how many times the struct is copied). So
     * instead we provide a function that will generate the error
     * message into a BinarySink.
     */
    void (*errfn)(SeatPromptResult, BinarySink *);

    /*
     * And some fields the error function can use to construct the
     * message (holding, e.g. an OS error code).
     */
    const char *errdata_lit; /* statically allocated, e.g. a string literal */
    unsigned errdata_u;
};

/* Helper function to construct the simple versions of these
 * structures inline */
static inline SeatPromptResult make_spr_simple(SeatPromptResultKind kind)
{
    SeatPromptResult spr;
    spr.kind = kind;
    spr.errdata_lit = NULL;
    return spr;
}

/* Most common constructor function for SPRK_SW_ABORT errors */
SeatPromptResult make_spr_sw_abort_static(const char *);

/* Convenience macros wrapping those constructors in turn */
#define SPR_INCOMPLETE make_spr_simple(SPRK_INCOMPLETE)
#define SPR_USER_ABORT make_spr_simple(SPRK_USER_ABORT)
#define SPR_SW_ABORT(lit) make_spr_sw_abort_static(lit)
#define SPR_OK make_spr_simple(SPRK_OK)

/* Query function that folds both kinds of abort together */
static inline bool spr_is_abort(SeatPromptResult spr)
{
    return spr.kind == SPRK_USER_ABORT || spr.kind == SPRK_SW_ABORT;
}

/* Function to return a dynamically allocated copy of the error message */
char *spr_get_error_message(SeatPromptResult spr);

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
    strbuf *result;
} prompt_t;
typedef struct prompts_t prompts_t;
struct prompts_t {
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

    char *name;         /* Short description, perhaps for dialog box title */
    bool name_reqd;     /* Display of `name' required or optional? */
    char *instruction;  /* Long description, maybe with embedded newlines */
    bool instr_reqd;    /* Display of `instruction' required or optional? */
    size_t n_prompts;   /* May be zero (in which case display the foregoing,
                         * if any, and return success) */
    size_t prompts_size; /* allocated storage capacity for prompts[] */
    prompt_t **prompts;
    void *data;         /* slot for housekeeping data, managed by
                         * seat_get_userpass_input(); initially NULL */
    SeatPromptResult spr; /* some implementations need to cache one of these */

    /*
     * Set this flag to indicate that the caller has encoded the
     * prompts in UTF-8, and expects the responses to be UTF-8 too.
     *
     * Ideally this flag would be unnecessary because it would always
     * be true, but for legacy reasons, we have to switch over a bit
     * at a time from the old behaviour, and may never manage to get
     * rid of it completely.
     */
    bool utf8;

    /*
     * Callback you can fill in to be notified when all the prompts'
     * responses are available. After you receive this notification, a
     * further call to the get_userpass_input function will return the
     * final state of the prompts system, which is guaranteed not to
     * be negative for 'still ongoing'.
     */
    toplevel_callback_fn_t callback;
    void *callback_ctx;

    /*
     * When this prompts_t is known to an Ldisc, we might need to
     * break the connection if things get freed in an emergency. So
     * this is a pointer to the Ldisc's pointer to us.
     */
    prompts_t **ldisc_ptr_to_us;
};
prompts_t *new_prompts(void);
void add_prompt(prompts_t *p, char *promptstr, bool echo);
void prompt_set_result(prompt_t *pr, const char *newstr);
char *prompt_get_result(prompt_t *pr);
const char *prompt_get_result_ref(prompt_t *pr);
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
    BUSY_NOT,       /* Not busy, all user interaction OK */
    BUSY_WAITING,   /* Waiting for something; local event loops still
                       running so some local interaction (e.g. menus)
                       OK, but network stuff is suspended */
    BUSY_CPU        /* Locally busy (e.g. crypto); user interaction
                     * suspended */
} BusyStatus;

typedef enum SeatInteractionContext {
    SIC_BANNER, SIC_KI_PROMPTS
} SeatInteractionContext;

typedef enum SeatOutputType {
    SEAT_OUTPUT_STDOUT, SEAT_OUTPUT_STDERR
} SeatOutputType;

typedef enum SeatDialogTextType {
    SDT_PARA, SDT_DISPLAY, SDT_SCARY_HEADING,
    SDT_TITLE, SDT_PROMPT, SDT_BATCH_ABORT,
    SDT_MORE_INFO_KEY, SDT_MORE_INFO_VALUE_SHORT, SDT_MORE_INFO_VALUE_BLOB
} SeatDialogTextType;
struct SeatDialogTextItem {
    SeatDialogTextType type;
    char *text;
};
struct SeatDialogText {
    size_t nitems, itemsize;
    SeatDialogTextItem *items;
};
SeatDialogText *seat_dialog_text_new(void);
void seat_dialog_text_free(SeatDialogText *sdt);
PRINTF_LIKE(3, 4) void seat_dialog_text_append(
    SeatDialogText *sdt, SeatDialogTextType type, const char *fmt, ...);

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
     * Provide output from the remote session. 'type' indicates the
     * type of the output (stdout or stderr), which can be used to
     * split the output into separate message channels, if the seat
     * wants to handle them differently. But combining the channels
     * into one is OK too; that's what terminal-window based seats do.
     *
     * The return value is the current size of the output backlog.
     */
    size_t (*output)(Seat *seat, SeatOutputType type,
                     const void *data, size_t len);

    /*
     * Called when the back end wants to indicate that EOF has arrived
     * on the server-to-client stream. Returns false to indicate that
     * we intend to keep the session open in the other direction, or
     * true to indicate that if they're closing so are we.
     */
    bool (*eof)(Seat *seat);

    /*
     * Called by the back end to notify that the output backlog has
     * changed size. A front end in control of the event loop won't
     * necessarily need this (they can just keep checking it via
     * backend_sendbuffer at every opportunity), but one buried in the
     * depths of something else (like an SSH proxy) will need to be
     * proactively notified that the amount of buffered data has
     * become smaller.
     */
    void (*sent)(Seat *seat, size_t new_sendbuffer);

    /*
     * Provide authentication-banner output from the session setup.
     * End-user Seats can treat this as very similar to 'output', but
     * intermediate Seats in complex proxying situations will want to
     * implement this and 'output' differently.
     */
    size_t (*banner)(Seat *seat, const void *data, size_t len);

    /*
     * Try to get answers from a set of interactive login prompts. The
     * prompts are provided in 'p'.
     *
     * (FIXME: it would be nice to distinguish two classes of user-
     * abort action, so the user could specify 'I want to abandon this
     * entire attempt to start a session' or the milder 'I want to
     * abandon this particular form of authentication and fall back to
     * a different one' - e.g. if you turn out not to be able to
     * remember your private key passphrase then perhaps you'd rather
     * fall back to password auth rather than aborting the whole
     * session.)
     */
    SeatPromptResult (*get_userpass_input)(Seat *seat, prompts_t *p);

    /*
     * Notify the seat that the main session channel has been
     * successfully set up.
     *
     * This is only used as part of the SSH proxying system, so it's
     * not necessary to implement it in all backends. A backend must
     * call this if it advertises the BACKEND_NOTIFIES_SESSION_START
     * flag, and otherwise, doesn't have to.
     */
    void (*notify_session_started)(Seat *seat);

    /*
     * Notify the seat that the process running at the other end of
     * the connection has finished.
     */
    void (*notify_remote_exit)(Seat *seat);

    /*
     * Notify the seat that the whole connection has finished.
     * (Distinct from notify_remote_exit, e.g. in the case where you
     * have port forwardings still active when the main foreground
     * session goes away: then you'd get notify_remote_exit when the
     * foreground session dies, but notify_remote_disconnect when the
     * last forwarding vanishes and the network connection actually
     * closes.)
     *
     * This function might be called multiple times by accident; seats
     * should be prepared to cope.
     *
     * More precisely: this function notifies the seat that
     * backend_connected() might now return false where previously it
     * returned true. (Note the 'might': an accidental duplicate call
     * might happen when backend_connected() was already returning
     * false. Or even, in weird situations, when it hadn't stopped
     * returning true yet. The point is, when you get this
     * notification, all it's really telling you is that it's worth
     * _checking_ backend_connected, if you weren't already.)
     */
    void (*notify_remote_disconnect)(Seat *seat);

    /*
     * Notify the seat that the connection has suffered an error,
     * either fatal to the whole connection or not.
     *
     * The latter kind of error is expected to be things along the
     * lines of 'I/O error storing the new host key', which has
     * traditionally been presented via a dialog box or similar.
     */
    void (*connection_fatal)(Seat *seat, const char *message);
    void (*nonfatal)(Seat *seat, const char *message);

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
     * This is called after we've already checked it by any means we
     * can do ourselves, such as checking against host key
     * fingerprints in the Conf or the host key cache on disk: once we
     * call this function, we've already decided there's nothing for
     * it but to prompt the user.
     *
     * 'mismatch' reports the result of checking the host key cache:
     * it is true if the server has presented a host key different
     * from the one we expected, and false if we had no expectation in
     * the first place.
     *
     * This call may prompt the user synchronously and not return
     * until the answer is available, or it may present the prompt and
     * return immediately, giving the answer later via the provided
     * callback.
     *
     * Return values:
     *
     *  - +1 means `user approved the key, so continue with the
     *    connection'
     *
     *  - 0 means `user rejected the key, abandon the connection'
     *
     *  - -1 means `I've initiated enquiries, please wait to be called
     *    back via the provided function with a result that's either 0
     *    or +1'.
     */
    SeatPromptResult (*confirm_ssh_host_key)(
        Seat *seat, const char *host, int port, const char *keytype,
        char *keystr, SeatDialogText *text, HelpCtx helpctx,
        void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
        char **fingerprints, bool is_certificate, int ca_count, bool already_verified); // WINSCP

    /*
     * Check with the seat whether it's OK to use a cryptographic
     * primitive from below the 'warn below this line' threshold in
     * the input Conf. Return values are the same as
     * confirm_ssh_host_key above.
     */
    SeatPromptResult (*confirm_weak_crypto_primitive)(
        Seat *seat, SeatDialogText *text,
        void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
        const char *algtype, const char *algname, int wcr);

    /*
     * Variant form of confirm_weak_crypto_primitive, which prints a
     * slightly different message but otherwise has the same
     * semantics.
     *
     * This form is used in the case where we're using a host key
     * below the warning threshold because that's the best one we have
     * cached, but at least one host key algorithm *above* the
     * threshold is available that we don't have cached.
     */
    SeatPromptResult (*confirm_weak_cached_hostkey)(
        Seat *seat, SeatDialogText *text,
        void (*callback)(void *ctx, SeatPromptResult result), void *ctx);

    /*
     * Some snippets of text describing the UI actions in host key
     * prompts / dialog boxes, to be used in ssh/common.c when it
     * assembles the full text of those prompts.
     */
    const SeatDialogPromptDescriptions *(*prompt_descriptions)(Seat *seat);

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
     */
    void (*set_trust_status)(Seat *seat, bool trusted);

    /*
     * Query whether this Seat can do anything user-visible in
     * response to set_trust_status.
     *
     * Returns true if the seat has a way to indicate this
     * distinction. Returns false if not, in which case the backend
     * should use a fallback defence against spoofing of PuTTY's local
     * prompts by malicious servers.
     */
    bool (*can_set_trust_status)(Seat *seat);

    /*
     * Query whether this Seat's interactive prompt responses and its
     * session input come from the same place.
     *
     * If false, this is used to suppress the final 'Press Return to
     * begin session' anti-spoofing prompt in Plink. For example,
     * Plink itself sets this flag if its standard input is redirected
     * (and therefore not coming from the same place as the console
     * it's sending its prompts to).
     */
    bool (*has_mixed_input_stream)(Seat *seat);

    /*
     * Ask the seat whether it would like verbose messages.
     */
    bool (*verbose)(Seat *seat);

    /*
     * Ask the seat whether it's an interactive program.
     */
    bool (*interactive)(Seat *seat);

    /*
     * Return the seat's current idea of where the output cursor is.
     *
     * Returns true if the seat has a cursor. Returns false if not.
     */
    bool (*get_cursor_position)(Seat *seat, int *x, int *y);
};

static inline size_t seat_output(
    Seat *seat, SeatOutputType type, const void *data, size_t len)
{ return seat->vt->output(seat, type, data, len); }
static inline bool seat_eof(Seat *seat)
{ return seat->vt->eof(seat); }
static inline void seat_sent(Seat *seat, size_t bufsize)
{ seat->vt->sent(seat, bufsize); }
static inline size_t seat_banner(
    InteractionReadySeat iseat, const void *data, size_t len)
{ return iseat.seat->vt->banner(iseat.seat, data, len); }
static inline SeatPromptResult seat_get_userpass_input(
    InteractionReadySeat iseat, prompts_t *p)
{ return iseat.seat->vt->get_userpass_input(iseat.seat, p); }
static inline void seat_notify_session_started(Seat *seat)
{ seat->vt->notify_session_started(seat); }
static inline void seat_notify_remote_exit(Seat *seat)
{ seat->vt->notify_remote_exit(seat); }
static inline void seat_notify_remote_disconnect(Seat *seat)
{ seat->vt->notify_remote_disconnect(seat); }
static inline void seat_update_specials_menu(Seat *seat)
{ seat->vt->update_specials_menu(seat); }
static inline char *seat_get_ttymode(Seat *seat, const char *mode)
{ return seat->vt->get_ttymode(seat, mode); }
static inline void seat_set_busy_status(Seat *seat, BusyStatus status)
{ seat->vt->set_busy_status(seat, status); }
static inline SeatPromptResult seat_confirm_ssh_host_key(
    InteractionReadySeat iseat, const char *h, int p, const char *ktyp,
    char *kstr, SeatDialogText *text, HelpCtx helpctx,
    void (*cb)(void *ctx, SeatPromptResult result), void *ctx,
    char **fingerprints, bool is_certificate, int ca_count, bool already_verified) // WINSCP
{ return iseat.seat->vt->confirm_ssh_host_key(
        iseat.seat, h, p, ktyp, kstr, text, helpctx, cb, ctx,
        fingerprints, is_certificate, ca_count, already_verified); } // WINSCP
static inline SeatPromptResult seat_confirm_weak_crypto_primitive(
    InteractionReadySeat iseat, SeatDialogText *text,
    void (*cb)(void *ctx, SeatPromptResult result), void *ctx,
    const char *algtype, const char *algname, int wcr) // WINSCP 
{ return iseat.seat->vt->confirm_weak_crypto_primitive(
        iseat.seat, text, cb, ctx,
        algtype, algname, wcr); } // WINSCP
static inline SeatPromptResult seat_confirm_weak_cached_hostkey(
    InteractionReadySeat iseat, SeatDialogText *text,
    void (*cb)(void *ctx, SeatPromptResult result), void *ctx)
{ return iseat.seat->vt->confirm_weak_cached_hostkey(
        iseat.seat, text, cb, ctx); }
static inline const SeatDialogPromptDescriptions *seat_prompt_descriptions(
    Seat *seat)
{ return seat->vt->prompt_descriptions(seat); }
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
static inline void seat_set_trust_status(Seat *seat, bool trusted)
{ seat->vt->set_trust_status(seat, trusted); }
static inline bool seat_can_set_trust_status(Seat *seat)
{ return seat->vt->can_set_trust_status(seat); }
static inline bool seat_has_mixed_input_stream(Seat *seat)
{ return seat->vt->has_mixed_input_stream(seat); }
static inline bool seat_verbose(Seat *seat)
{ return seat->vt->verbose(seat); }
static inline bool seat_interactive(Seat *seat)
{ return seat->vt->interactive(seat); }
static inline bool seat_get_cursor_position(Seat *seat, int *x, int *y)
{ return  seat->vt->get_cursor_position(seat, x, y); }

/* Unlike the seat's actual method, the public entry points
 * seat_connection_fatal and seat_nonfatal are wrapper functions with
 * a printf-like API, defined in utils. */
void seat_connection_fatal(Seat *seat, const char *fmt, ...) PRINTF_LIKE(2, 3);
void seat_nonfatal(Seat *seat, const char *fmt, ...) PRINTF_LIKE(2, 3);

/* Handy aliases for seat_output which set is_stderr to a fixed value. */
static inline size_t seat_stdout(Seat *seat, const void *data, size_t len)
{ return seat_output(seat, SEAT_OUTPUT_STDOUT, data, len); }
static inline size_t seat_stdout_pl(Seat *seat, ptrlen data)
{ return seat_output(seat, SEAT_OUTPUT_STDOUT, data.ptr, data.len); }
static inline size_t seat_stderr(Seat *seat, const void *data, size_t len)
{ return seat_output(seat, SEAT_OUTPUT_STDERR, data, len); }
static inline size_t seat_stderr_pl(Seat *seat, ptrlen data)
{ return seat_output(seat, SEAT_OUTPUT_STDERR, data.ptr, data.len); }

/* Alternative API for seat_banner taking a ptrlen */
static inline size_t seat_banner_pl(InteractionReadySeat iseat, ptrlen data)
{ return iseat.seat->vt->banner(iseat.seat, data.ptr, data.len); }

struct SeatDialogPromptDescriptions {
    const char *hk_accept_action;
    const char *hk_connect_once_action;
    const char *hk_cancel_action, *hk_cancel_action_Participle;
    const char *weak_accept_action, *weak_cancel_action;
};

/* In the utils subdir: print a message to the Seat which can't be
 * spoofed by server-supplied auth-time output such as SSH banners */
void seat_antispoof_msg(InteractionReadySeat iseat, const char *msg);

/*
 * Stub methods for seat implementations that want to use the obvious
 * null handling for a given method.
 *
 * These are generally obvious, except for is_utf8, where you might
 * plausibly want to return either fixed answer 'no' or 'yes'.
 */
size_t nullseat_output(
    Seat *seat, SeatOutputType type, const void *data, size_t len);
bool nullseat_eof(Seat *seat);
void nullseat_sent(Seat *seat, size_t bufsize);
size_t nullseat_banner(Seat *seat, const void *data, size_t len);
size_t nullseat_banner_to_stderr(Seat *seat, const void *data, size_t len);
SeatPromptResult nullseat_get_userpass_input(Seat *seat, prompts_t *p);
void nullseat_notify_session_started(Seat *seat);
void nullseat_notify_remote_exit(Seat *seat);
void nullseat_notify_remote_disconnect(Seat *seat);
void nullseat_connection_fatal(Seat *seat, const char *message);
void nullseat_nonfatal(Seat *seat, const char *message);
void nullseat_update_specials_menu(Seat *seat);
char *nullseat_get_ttymode(Seat *seat, const char *mode);
void nullseat_set_busy_status(Seat *seat, BusyStatus status);
SeatPromptResult nullseat_confirm_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, SeatDialogText *text, HelpCtx helpctx,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    char **fingerprints, bool is_certificate, int ca_count, bool already_verified); // WINSCP
SeatPromptResult nullseat_confirm_weak_crypto_primitive(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    const char *algtype, const char *algname, int wcr); // WINSCP
SeatPromptResult nullseat_confirm_weak_cached_hostkey(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
const SeatDialogPromptDescriptions *nullseat_prompt_descriptions(Seat *seat);
bool nullseat_is_never_utf8(Seat *seat);
bool nullseat_is_always_utf8(Seat *seat);
void nullseat_echoedit_update(Seat *seat, bool echoing, bool editing);
const char *nullseat_get_x_display(Seat *seat);
bool nullseat_get_windowid(Seat *seat, long *id_out);
bool nullseat_get_window_pixel_size(Seat *seat, int *width, int *height);
StripCtrlChars *nullseat_stripctrl_new(
    Seat *seat, BinarySink *bs_out, SeatInteractionContext sic);
void nullseat_set_trust_status(Seat *seat, bool trusted);
bool nullseat_can_set_trust_status_yes(Seat *seat);
bool nullseat_can_set_trust_status_no(Seat *seat);
bool nullseat_has_mixed_input_stream_yes(Seat *seat);
bool nullseat_has_mixed_input_stream_no(Seat *seat);
bool nullseat_verbose_no(Seat *seat);
bool nullseat_verbose_yes(Seat *seat);
bool nullseat_interactive_no(Seat *seat);
bool nullseat_interactive_yes(Seat *seat);
bool nullseat_get_cursor_position(Seat *seat, int *x, int *y);

/*
 * Seat functions provided by the platform's console-application
 * support module (console.c in each platform subdirectory).
 */

#ifndef WINSCP
void console_connection_fatal(Seat *seat, const char *message);
void console_nonfatal(Seat *seat, const char *message);
SeatPromptResult console_confirm_ssh_host_key(
    Seat *seat, const char *host, int port, const char *keytype,
    char *keystr, SeatDialogText *text, HelpCtx helpctx,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
SeatPromptResult console_confirm_weak_crypto_primitive(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
SeatPromptResult console_confirm_weak_cached_hostkey(
    Seat *seat, SeatDialogText *text,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
StripCtrlChars *console_stripctrl_new(
    Seat *seat, BinarySink *bs_out, SeatInteractionContext sic);
void console_set_trust_status(Seat *seat, bool trusted);
bool console_can_set_trust_status(Seat *seat);
bool console_has_mixed_input_stream(Seat *seat);
const SeatDialogPromptDescriptions *console_prompt_descriptions(Seat *seat);
#endif

/*
 * Other centralised seat functions.
 */
SeatPromptResult filexfer_get_userpass_input(Seat *seat, prompts_t *p);
bool cmdline_seat_verbose(Seat *seat);

/*
 * TempSeat: a seat implementation that can be given to a backend
 * temporarily while network proxy setup is using the real seat.
 * Buffers output and trust-status changes until the real seat is
 * available again.
 */

/* Called by the proxy code to make a TempSeat. */
Seat *tempseat_new(Seat *real);

/* Query functions to tell if a Seat _is_ temporary, and if so, to
 * return the underlying real Seat. */
bool is_tempseat(Seat *seat);
Seat *tempseat_get_real(Seat *seat);

/* Called by interactor_return_seat once the proxy connection has
 * finished setting up (or failed), to pass on any buffered stuff to
 * the real seat. */
void tempseat_flush(Seat *ts);

/* Frees a TempSeat, without flushing anything it has buffered. (Call
 * this after tempseat_flush, or alternatively, when you were going to
 * abandon the whole connection anyway.) */
void tempseat_free(Seat *ts);

typedef struct rgb {
    uint8_t r, g, b;
} rgb;

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

    /* set_raw_mouse_mode instructs the front end to start sending mouse events
     * in raw mode suitable for translating into mouse-tracking terminal data
     * (e.g. include scroll-wheel events and don't bother to identify double-
     * and triple-clicks). set_raw_mouse_mode_pointer instructs the front end
     * to change the mouse pointer shape to *indicate* raw mouse mode. */
    void (*set_raw_mouse_mode)(TermWin *, bool enable);
    void (*set_raw_mouse_mode_pointer)(TermWin *, bool enable);

    void (*set_scrollbar)(TermWin *, int total, int start, int page);

    void (*bell)(TermWin *, int mode);

    void (*clip_write)(TermWin *, int clipboard, wchar_t *text, int *attrs,
                       truecolour *colours, int len, bool must_deselect);
    void (*clip_request_paste)(TermWin *, int clipboard);

    void (*refresh)(TermWin *);

    /* request_resize asks the front end if the terminal can please be
     * resized to (w,h) in characters. The front end MAY call
     * term_size() in response to tell the terminal its new size
     * (which MAY be the requested size, or some other size if the
     * requested one can't be achieved). The front end MAY also not
     * call term_size() at all. But the front end MUST reply to this
     * request by calling term_resize_request_completed(), after the
     * responding resize event has taken place (if any).
     *
     * The calls to term_size and term_resize_request_completed may be
     * synchronous callbacks from within the call to request_resize(). */
    void (*request_resize)(TermWin *, int w, int h);

    void (*set_title)(TermWin *, const char *title, int codepage);
    void (*set_icon_title)(TermWin *, const char *icontitle, int codepage);

    /* set_minimised and set_maximised are assumed to set two
     * independent settings, rather than a single three-way
     * {min,normal,max} switch. The idea is that when you un-minimise
     * the window it remembers whether to go back to normal or
     * maximised. */
    void (*set_minimised)(TermWin *, bool minimised);
    void (*set_maximised)(TermWin *, bool maximised);
    void (*move)(TermWin *, int x, int y);
    void (*set_zorder)(TermWin *, bool top);

    /* Set the colour palette that the TermWin will use to display
     * text. One call to this function sets 'ncolours' consecutive
     * colours in the OSC 4 sequence, starting at 'start'. */
    void (*palette_set)(TermWin *, unsigned start, unsigned ncolours,
                        const rgb *colours);

    /* Query the front end for any OS-local overrides to the default
     * colours stored in Conf. The front end should set any it cares
     * about by calling term_palette_override.
     *
     * The Terminal object is passed in as a parameter, because this
     * can be called as a callback from term_init(). So the TermWin
     * itself won't yet have been told where to find its Terminal
     * object, because that doesn't happen until term_init
     * returns. */
    void (*palette_get_overrides)(TermWin *, Terminal *);

    /* Notify the front end that the terminal's buffer of unprocessed
     * output has reduced. (Front ends will likely pass this straight
     * on to backend_unthrottle.) */
    void (*unthrottle)(TermWin *, size_t bufsize);
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
static inline void win_set_raw_mouse_mode_pointer(TermWin *win, bool enable)
{ win->vt->set_raw_mouse_mode_pointer(win, enable); }
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
static inline void win_set_title(TermWin *win, const char *title, int codepage)
{ win->vt->set_title(win, title, codepage); }
static inline void win_set_icon_title(TermWin *win, const char *icontitle,
                                      int codepage)
{ win->vt->set_icon_title(win, icontitle, codepage); }
static inline void win_set_minimised(TermWin *win, bool minimised)
{ win->vt->set_minimised(win, minimised); }
static inline void win_set_maximised(TermWin *win, bool maximised)
{ win->vt->set_maximised(win, maximised); }
static inline void win_move(TermWin *win, int x, int y)
{ win->vt->move(win, x, y); }
static inline void win_set_zorder(TermWin *win, bool top)
{ win->vt->set_zorder(win, top); }
static inline void win_palette_set(
    TermWin *win, unsigned start, unsigned ncolours, const rgb *colours)
{ win->vt->palette_set(win, start, ncolours, colours); }
static inline void win_palette_get_overrides(TermWin *win, Terminal *term)
{ win->vt->palette_get_overrides(win, term); }
static inline void win_unthrottle(TermWin *win, size_t size)
{ win->vt->unthrottle(win, size); }

/*
 * Global functions not specific to a connection instance.
 */
NORETURN void nonfatal(const char *, ...) PRINTF_LIKE(1, 2); // WINSCP
NORETURN void modalfatalbox(const char *, ...) PRINTF_LIKE(1, 2);
NORETURN void cleanup_exit(int);

/*
 * Exports from conf.c, and a big enum (via parametric macro) of
 * configuration option keys.
 */

/* The master list of option keywords lives in conf.h */
enum config_primary_key {
    #define CONF_OPTION(keyword, ...) CONF_ ## keyword,
    #include "conf.h"
    #undef CONF_OPTION

    N_CONFIG_OPTIONS
};

/* Types that appear in Conf keys and values. */
enum {
    /*
     * CONF_TYPE_NONE is included in this enum because sometimes you
     * need a placeholder for 'no type found'. (In Rust you'd leave it
     * out, and use Option<ConfType> for those situations.)
     *
     * In particular, it's used as the subkey type for options that
     * don't have subkeys.
     */
    CONF_TYPE_NONE,

    /* Booleans, accessed via conf_get_bool and conf_set_bool */
    CONF_TYPE_BOOL,

    /* Integers, accessed via conf_get_int and conf_set_int */
    CONF_TYPE_INT,

    /*
     * NUL-terminated char strings, accessed via conf_get_str and
     * conf_set_str.
     *
     * Where character encoding is relevant, these are generally
     * expected to be in the host system's default character encoding.
     *
     * (Character encoding might not be relevant at all: for example,
     * if the string is going to be used as a shell command on Unix,
     * then the exec system call will want a char string anyway.)
     */
    CONF_TYPE_STR,

    /* NUL-terminated char strings encoded in UTF-8, accessed via
     * conf_get_utf8 and conf_set_utf8. */
    CONF_TYPE_UTF8,

    /*
     * A type that can be _either_ a char string in system encoding
     * (aka CONF_TYPE_STR), _or_ a char string in UTF-8 (aka
     * CONF_TYPE_UTF8). You can set it to be one or the other via
     * conf_set_str or conf_set_utf8. To read it, you must use
     * conf_get_str_ambi(), which returns a char string and a boolean
     * telling you whether it's UTF-8.
     *
     * These can't be used as _keys_ in Conf, only as values. (If you
     * used them as keys, you'd have to answer the difficult question
     * of whether a UTF-8 and a non-UTF-8 string should be considered
     * equal.)
     */
    CONF_TYPE_STR_AMBI,

    /* PuTTY's OS-specific 'Filename' data type, accessed via
     * conf_get_filename and conf_set_filename */
    CONF_TYPE_FILENAME,

    /* PuTTY's GUI-specific 'FontSpec' data type, accessed via
     * conf_get_fontspec and conf_set_fontspec */
    CONF_TYPE_FONT,
};

struct ConfKeyInfo {
    int subkey_type;
    int value_type;

    union {
        // WINSCP changed order, because only the first member can be initialized
        // and char* can be casted to int, but not to bool
        int ival;
        bool bval;
        const char *sval;
    } default_value;

    bool save_custom : 1;
    bool load_custom : 1;
    bool not_saved : 1;

    const char *save_keyword;
    const ConfSaveEnumType *storage_enum;
};
struct ConfSaveEnumType {
    const ConfSaveEnumValue *values;
    size_t nvalues;
};
struct ConfSaveEnumValue {
    int confval, storageval;
    bool obsolete;
};

extern const ConfKeyInfo conf_key_info[];
bool conf_enum_map_to_storage(const ConfSaveEnumType *etype,
                              int confval, int *storageval_out);
bool conf_enum_map_from_storage(const ConfSaveEnumType *etype,
                                int storageval, int *confval_out);

/* Functions handling configuration structures. */
Conf *conf_new(void);                  /* create an empty configuration */
void conf_free(Conf *conf);
void conf_clear(Conf *conf);    /* likely only useful for test programs */
Conf *conf_copy(Conf *oldconf);
void conf_copy_into(Conf *dest, Conf *src);
/* Mandatory accessor functions: enforce by assertion that keys exist. */
bool conf_get_bool(Conf *conf, int key);
int conf_get_int(Conf *conf, int key);
int conf_get_int_int(Conf *conf, int key, int subkey);
char *conf_get_str(Conf *conf, int key);   /* result still owned by conf */
char *conf_get_utf8(Conf *conf, int key);   /* result still owned by conf */
char *conf_get_str_ambi( /* result still owned by conf; 'utf8' may be NULL */
    Conf *conf, int key, bool *utf8);
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
void conf_set_utf8(Conf *conf, int key, const char *value);
bool conf_try_set_str(Conf *conf, int key, const char *value);
bool conf_try_set_utf8(Conf *conf, int key, const char *value);
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
 * The full fontspec_new is declared in the platform header, because
 * each platform may need it to have a different prototype, due to
 * constructing fonts in different ways. But fontspec_new_default()
 * will at least produce _some_ kind of a FontSpec, for use in
 * situations where one needs to exist (e.g. to put in a Conf) and be
 * freeable but won't actually be used for anything important.
 */
FontSpec *fontspec_new_default(void);
FontSpec *fontspec_copy(const FontSpec *f);
void fontspec_free(FontSpec *f);
void fontspec_serialise(BinarySink *bs, FontSpec *f);
FontSpec *fontspec_deserialise(BinarySource *src);

/*
 * Exports from each platform's noise.c.
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

/*
 * Exports from sshrand.c.
 */
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
void settings_set_default_protocol(int);
void settings_set_default_port(int);

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
void term_resize_request_completed(Terminal *);
void term_paint(Terminal *, int, int, int, int, bool);
void term_scroll(Terminal *, int, int);
void term_scroll_to_selection(Terminal *, int);
void term_pwron(Terminal *, bool);
void term_clrsb(Terminal *);
void term_mouse(Terminal *, Mouse_Button, Mouse_Button, Mouse_Action,
                int, int, bool, bool, bool);
void term_cancel_selection_drag(Terminal *);
void term_key(Terminal *, Key_Sym, wchar_t *, size_t, unsigned int,
              unsigned int);
void term_lost_clipboard_ownership(Terminal *, int clipboard);
void term_update(Terminal *);
void term_invalidate(Terminal *);
void term_blink(Terminal *, bool set_cursor);
void term_do_paste(Terminal *, const wchar_t *, size_t);
void term_nopaste(Terminal *);
void term_copyall(Terminal *, const int *, int);
void term_pre_reconfig(Terminal *, Conf *);
void term_reconfig(Terminal *, Conf *);
void term_request_copy(Terminal *, const int *clipboards, int n_clipboards);
void term_request_paste(Terminal *, int clipboard);
void term_seen_key_event(Terminal *);
size_t term_data(Terminal *, const void *data, size_t len);
void term_provide_backend(Terminal *term, Backend *backend);
void term_provide_logctx(Terminal *term, LogContext *logctx);
void term_set_focus(Terminal *term, bool has_focus);
char *term_get_ttymode(Terminal *term, const char *mode);
SeatPromptResult term_get_userpass_input(Terminal *term, prompts_t *p);
void term_set_trust_status(Terminal *term, bool trusted);
void term_keyinput(Terminal *, int codepage, const void *buf, int len);
void term_keyinputw(Terminal *, const wchar_t *widebuf, int len);
void term_get_cursor_position(Terminal *term, int *x, int *y);
void term_setup_window_titles(Terminal *term, const char *title_hostname);
void term_notify_minimised(Terminal *term, bool minimised);
void term_notify_palette_changed(Terminal *term);
void term_notify_window_pos(Terminal *term, int x, int y);
void term_notify_window_size_pixels(Terminal *term, int x, int y);
void term_palette_override(Terminal *term, unsigned osc4_index, rgb rgb);

typedef enum SmallKeypadKey {
    SKK_HOME, SKK_END, SKK_INSERT, SKK_DELETE, SKK_PGUP, SKK_PGDN,
} SmallKeypadKey;
int format_arrow_key(char *buf, Terminal *term, int xkey,
                     bool shift, bool ctrl, bool alt, bool *consumed_alt);
int format_function_key(char *buf, Terminal *term, int key_number,
                        bool shift, bool ctrl, bool alt, bool *consumed_alt);
int format_small_keypad_key(char *buf, Terminal *term, SmallKeypadKey key,
                            bool shift, bool ctrl, bool alt,
                            bool *consumed_alt);
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

    /*
     * Ask whether extra verbose log messages are required.
     */
    bool (*verbose)(LogPolicy *lp);
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
static inline bool lp_verbose(LogPolicy *lp)
{ return lp->vt->verbose(lp); }

/* Defined in clicons.c, used in several console command-line tools */
extern LogPolicy console_cli_logpolicy[];

int console_askappend(LogPolicy *lp, Filename *filename,
                      void (*callback)(void *ctx, int result), void *ctx);
void console_logging_error(LogPolicy *lp, const char *string);
void console_eventlog(LogPolicy *lp, const char *string);
bool null_lp_verbose_yes(LogPolicy *lp);
bool null_lp_verbose_no(LogPolicy *lp);
bool cmdline_lp_verbose(LogPolicy *lp);

LogContext *log_init(LogPolicy *lp, Conf *conf);
void log_free(LogContext *logctx);
void log_reconfig(LogContext *logctx, Conf *conf);
void logfopen(LogContext *logctx);
void logfclose(LogContext *logctx);
void logtraffic(LogContext *logctx, unsigned char c, int logmode);
void logflush(LogContext *logctx);
LogPolicy *log_get_policy(LogContext *logctx);
void logevent(LogContext *logctx, const char *event);
void logeventf(LogContext *logctx, const char *fmt, ...) PRINTF_LIKE(2, 3);
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
 * Exports from ssh/ssh.c.
 */
extern const struct BackendVtable ssh_backend;
extern const struct BackendVtable sshconn_backend;

/*
 * Exports from supdup.c.
 */
extern const struct BackendVtable supdup_backend;

/*
 * Exports from ldisc.c.
 */
Ldisc *ldisc_create(Conf *, Terminal *, Backend *, Seat *);
void ldisc_configure(Ldisc *, Conf *);
void ldisc_free(Ldisc *);
void ldisc_send(Ldisc *, const void *buf, int len, bool interactive);
void ldisc_echoedit_update(Ldisc *);
void ldisc_provide_userpass_le(Ldisc *, TermLineEditor *);
void ldisc_check_sendok(Ldisc *);

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
/* random_clear is equivalent to calling random_unref as many times as
 * necessary to shut down the global PRNG instance completely. It's
 * not needed in normal applications, but the command-line PuTTYgen
 * test finds it useful to clean up after each invocation of the
 * logical main() no matter whether it needed random numbers or
 * not. */
void random_clear(void);
/* random_setup_custom sets up the process-global random number
 * generator specially, with a hash function of your choice. */
void random_setup_custom(const ssh_hashalg *hash);
/* random_setup_special() is a macro wrapper on that, which makes an
 * extra-big one based on the largest hash function we have. It's
 * defined this way to avoid what would otherwise be an unnecessary
 * module dependency from sshrand.c to a hash function implementation. */
#define random_setup_special() random_setup_custom(&ssh_shake256_114bytes)
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
 * Exports from modules in utils.
 */

#include "misc.h"
bool conf_launchable(Conf *conf);
char const *conf_dest(Conf *conf);

/*
 * Exports from sessprep.c.
 */
void prepare_session(Conf *conf);

/*
 * Exports from version.c and cmake_commit.c.
 */
extern const char ver[];
extern const char commitid[];

/*
 * Exports from unicode.c in platform subdirs.
 */
/* void init_ucs(void); -- this is now in platform-specific headers */
bool is_dbcs_leadbyte(int codepage, char byte);
/* For put_mb_to_wc / put_wc_to_mb, see marshal.h */
wchar_t xlat_uskbd2cyrllic(int ch);
int check_compose(int first, int second);
int decode_codepage(const char *cp_name);
const char *cp_enumerate (int index);
const char *cp_name(int codepage);
void get_unitab(int codepage, wchar_t *unitab, int ftype);

/*
 * Exports from wcwidth.c
 */
int mk_wcwidth(unsigned int ucs);
int mk_wcswidth(const unsigned int *pwcs, size_t n);
int mk_wcwidth_cjk(unsigned int ucs);
int mk_wcswidth_cjk(const unsigned int *pwcs, size_t n);

/*
 * Exports from agent-client.c in platform subdirs.
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
 * return NULL. The wrapper function agent_query_synchronous()
 * (defined in its own module aqsync.c) makes this easier.
 */
typedef struct agent_pending_query agent_pending_query;
agent_pending_query *agent_query(
    strbuf *in, void **out, int *outlen,
    void (*callback)(void *, void *, int), void *callback_ctx, struct callback_set * callback_set); // WINSCP
void agent_cancel_query(agent_pending_query *);
void agent_query_synchronous(strbuf *in, void **out, int *outlen);
bool agent_exists(void);

/* For stream-oriented agent connections, if available. */
Socket *agent_connect(Plug *plug);

/*
 * Exports from wildcard.c
 */
const char *wc_error(int value);
int wc_match_pl(const char *wildcard, ptrlen target);
int wc_match(const char *wildcard, const char *target);
bool wc_unescape(char *output, const char *wildcard);

/*
 * Exports from frontend (dialog.c etc)
 */
void pgp_fingerprints(void);
/*
 * have_ssh_host_key() just returns true if a key of that type is
 * already cached and false otherwise.
 */
bool have_ssh_host_key(Seat *seat, const char *host, int port, const char *keytype);

/*
 * Exports from console frontends (console.c in platform subdirs)
 * that aren't equivalents to things in windlg.c et al.
 */
extern bool console_batch_mode, console_antispoof_prompt;
extern bool console_set_batch_mode(bool);
extern bool console_set_stdio_prompts(bool);
SeatPromptResult console_get_userpass_input(prompts_t *p);
bool is_interactive(void);
void console_print_error_msg(const char *prefix, const char *msg);
void console_print_error_msg_fmt_v(
    const char *prefix, const char *fmt, va_list ap);
void console_print_error_msg_fmt(const char *prefix, const char *fmt, ...)
    PRINTF_LIKE(2, 3);

/*
 * Exports from either console frontends or terminal.c.
 */
extern bool set_legacy_charset_handling(bool);

/*
 * Exports from printing.c in platform subdirs.
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
 */
struct cmdline_get_passwd_input_state { bool tried; };
#define CMDLINE_GET_PASSWD_INPUT_STATE_INIT { .tried = false }
extern const cmdline_get_passwd_input_state cmdline_get_passwd_input_state_new;
int cmdline_process_param(CmdlineArg *, CmdlineArg *, int, Conf *);
void cmdline_run_saved(Conf *);
void cmdline_cleanup(void);
SeatPromptResult cmdline_get_passwd_input(
    prompts_t *p, cmdline_get_passwd_input_state *state, bool restartable);
bool cmdline_host_ok(Conf *);
bool cmdline_verbose(void);
bool cmdline_loaded_session(void);

/*
 * Abstraction provided by each platform to represent a command-line
 * argument. May not be as simple as a default-encoded string: on
 * Windows, command lines can be Unicode representing characters not
 * in the system codepage, so you might need to retrieve the argument
 * in a richer form.
 */
struct CmdlineArgList {
    /* args[0], args[1], ... represent the original arguments in the
     * command line. Then there's a null pointer. Further arguments
     * can be invented to add to the array after that, in which case
     * they'll be freed with the rest of the CmdlineArgList, but
     * aren't logically part of the original command line. */
    CmdlineArg **args;
    size_t nargs, argssize;
};
struct CmdlineArg {
    CmdlineArgList *list;
};
const char *cmdline_arg_to_utf8(CmdlineArg *arg); /* may fail */
const char *cmdline_arg_to_str(CmdlineArg *arg);  /* must not fail */
Filename *cmdline_arg_to_filename(CmdlineArg *arg);  /* caller must free */
void cmdline_arg_wipe(CmdlineArg *arg);
CmdlineArg *cmdline_arg_from_str(CmdlineArgList *list, const char *string);
/* Platforms provide their own constructors for CmdlineArgList */
void cmdline_arg_list_free(CmdlineArgList *list);

/*
 * Here we have a flags word provided by each tool, which describes
 * the capabilities of that tool that cmdline.c needs to know about.
 * It will refuse certain command-line options if a particular tool
 * inherently can't do anything sensible. For example, the file
 * transfer tools (psftp, pscp) can't do a great deal with protocol
 * selections (ever tried running scp over telnet?) or with port
 * forwarding (even if it wasn't a hideously bad idea, they don't have
 * the select/poll infrastructure to make them work).
 */
extern const unsigned cmdline_tooltype;

/* Bit flags for the above */
#define TOOLTYPE_LIST(X)                        \
    X(TOOLTYPE_FILETRANSFER)                    \
    X(TOOLTYPE_NONNETWORK)                      \
    X(TOOLTYPE_HOST_ARG)                        \
    X(TOOLTYPE_HOST_ARG_CAN_BE_SESSION)         \
    X(TOOLTYPE_HOST_ARG_PROTOCOL_PREFIX)        \
    X(TOOLTYPE_HOST_ARG_FROM_LAUNCHABLE_LOAD)   \
    X(TOOLTYPE_PORT_ARG)                        \
    X(TOOLTYPE_NO_VERBOSE_OPTION)               \
    X(TOOLTYPE_GUI)                             \
    /* end of list */
#define BITFLAG_INDEX(val) val ## _bitflag_index,
enum { TOOLTYPE_LIST(BITFLAG_INDEX) };
#define BITFLAG_DEF(val) val = 1U << (val ## _bitflag_index),
enum { TOOLTYPE_LIST(BITFLAG_DEF) };

void cmdline_error(const char *, ...) PRINTF_LIKE(1, 2);

/*
 * Exports from config.c.
 */
struct controlbox;
void conf_radiobutton_handler(dlgcontrol *ctrl, dlgparam *dlg,
                              void *data, int event);
#define CHECKBOX_INVERT (1<<30)
void conf_checkbox_handler(dlgcontrol *ctrl, dlgparam *dlg,
                           void *data, int event);
void conf_editbox_handler(dlgcontrol *ctrl, dlgparam *dlg,
                          void *data, int event);
void conf_filesel_handler(dlgcontrol *ctrl, dlgparam *dlg,
                          void *data, int event);
void conf_fontsel_handler(dlgcontrol *ctrl, dlgparam *dlg,
                          void *data, int event);

// to avoid "[bcc32c Warning] anonymous types declared in an anonymous union are an extension"
#ifndef WINSCP
struct conf_editbox_handler_type {
    /* Structure passed as context2 to conf_editbox_handler */
    enum { EDIT_STR, EDIT_INT, EDIT_FIXEDPOINT } type;
    union {
        /*
         * EDIT_STR means the edit box is connected to a string
         * field in Conf. No further parameters needed.
         */

        /*
         * EDIT_INT means the edit box is connected to an int field in
         * Conf, and the input string is interpreted as decimal. No
         * further parameters needed. (But we could add one here later
         * if for some reason we wanted int fields in hex.)
         */

        /*
         * EDIT_FIXEDPOINT means the edit box is connected to an int
         * field in Conf, but the input string is interpreted as
         * _floating point_, and converted to/from the output int by
         * means of a fixed denominator. That is,
         *
         *   (floating value in edit box) * denominator = value in Conf
         */
        struct {
            double denominator;
        };
    };
};

extern const struct conf_editbox_handler_type conf_editbox_str;
extern const struct conf_editbox_handler_type conf_editbox_int;
#define ED_STR CP(&conf_editbox_str)
#define ED_INT CP(&conf_editbox_int)
#endif

void setup_config_box(struct controlbox *b, bool midsession,
                      int protocol, int protcfginfo);

void setup_ca_config_box(struct controlbox *b);

/* Platforms provide this to be called from config.c */
void show_ca_config_box(dlgparam *dlg);
extern const bool has_ca_config_box; /* false if, e.g., we're PuTTYtel */

/* Visible outside config.c so that platforms can use it to recognise
 * the proxy type control */
void proxy_type_handler(dlgcontrol *ctrl, dlgparam *dlg,
                        void *data, int event);
/* And then they'll set this flag in its generic.context.i */
#define PROXY_UI_FLAG_LOCAL 1 /* has a local proxy */

/*
 * Exports from bidi.c.
 */
#define BIDI_CHAR_INDEX_NONE ((unsigned short)-1)
typedef struct bidi_char {
    unsigned int origwc, wc;
    unsigned short index, nchars;
} bidi_char;
BidiContext *bidi_new_context(void);
void bidi_free_context(BidiContext *ctx);
void do_bidi(BidiContext *ctx, bidi_char *line, size_t count);
int do_shape(bidi_char *line, bidi_char *to, int count);
bool is_rtl(int c);

/*
 * X11 auth mechanisms we know about.
 */
enum {
    X11_NO_AUTH,
    X11_MIT,                           /* MIT-MAGIC-COOKIE-1 */
    X11_XDM,                           /* XDM-AUTHORIZATION-1 */
    X11_NAUTHS
};
extern const char *const x11_authnames[X11_NAUTHS];

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
Filename *filename_from_utf8(const char *ustr); // WINSCP
const char *filename_to_str(const Filename *fn);
const char* in_memory_key_data(const Filename *fn); // WINSCP
bool filename_equal(const Filename *f1, const Filename *f2);
bool filename_is_null(const Filename *fn);
Filename *filename_copy(const Filename *fn);
void filename_free(Filename *fn);
void filename_serialise(BinarySink *bs, const Filename *f);
Filename *filename_deserialise(BinarySource *src);
char *get_username(void);              /* return value needs freeing */
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
#ifdef MPEXT
typedef struct callback callback;
struct IdempotentCallback;
typedef struct PacketQueueNode PacketQueueNode;
typedef struct handle_list_node handle_list_node;
struct handle_list_node {
    handle_list_node *next, *prev;
};
struct callback_set {
    struct callback *cbcurr, *cbhead, *cbtail;
    IdempotentCallback * ic_pktin_free;
    PacketQueueNode * pktin_freeq_head;
    handle_list_node ready_head[1];
    CRITICAL_SECTION ready_critsec[1];
    HANDLE ready_event;
    tree234 *handlewaits_tree_real;
};
#define CALLBACK_SET_ONLY struct callback_set * callback_set_v
#define CALLBACK_SET CALLBACK_SET_ONLY,
#else
#define CALLBACK_SET_ONLY void
#define CALLBACK_SET
#endif
void queue_toplevel_callback(CALLBACK_SET toplevel_callback_fn_t fn, void *ctx);
bool run_toplevel_callbacks(CALLBACK_SET_ONLY);
bool toplevel_callback_pending(CALLBACK_SET_ONLY);
bool is_idempotent_callback_pending(CALLBACK_SET struct IdempotentCallback *ic); // WINSCP
struct callback_set * get_callback_set(Plug * plug);
struct callback_set * get_seat_callback_set(Seat * seat);
void delete_callbacks_for_context(CALLBACK_SET void *ctx);
LogPolicy *log_get_logpolicy(LogContext *ctx); // WINSCP
Seat * get_log_seat(LogContext * lp); // WINSCP
struct callback_set * get_log_callback_set(LogContext * lp); // WINSCP

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
    struct callback_set * set;
};
void queue_idempotent_callback(struct IdempotentCallback *ic);

#ifndef MPEXT
typedef void (*toplevel_callback_notify_fn_t)(void *ctx);
void request_callback_notifications(toplevel_callback_notify_fn_t notify,
                                    void *ctx);
#endif

/*
 * Facility provided by the platform to spawn a parallel subprocess
 * and present its stdio via a Socket.
 *
 * 'prefix' indicates the prefix that should appear on messages passed
 * to plug_log to provide stderr output from the process.
 */
Socket *platform_start_subprocess(const char *cmd, Plug *plug,
                                  const char *prefix);

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

/* REGIONAL INDICATOR SYMBOL LETTER A-Z */
#define IS_REGIONAL_INDICATOR_LETTER(wc) ((unsigned)(wc) - 0x1F1E6U < 26)

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

#ifdef WINSCP
extern CRITICAL_SECTION putty_section;
void putty_initialize();
void putty_finalize();
void pktin_free_queue_callback(void *vctx);
#define WINSCP_PUTTY_SECTION_ENTER EnterCriticalSection(&putty_section);
#define WINSCP_PUTTY_SECTION_LEAVE LeaveCriticalSection(&putty_section);
#else
#define WINSCP_PUTTY_SECTION_ENTER
#define WINSCP_PUTTY_SECTION_LEAVE
#endif

#ifdef MPEXT
// To mark carefully selected messages from PuTTY code as UTF-8.
// Only for messages that are certain not to ever get ansi-encoded component,
// but known to get UTF-8 encoded component (currently private key path only)
#define WINSCP_BOM "\xEF\xBB\xBF"
#endif

#ifdef _DEBUG
#undef assert
void DoAssertC(char * Message, char * Filename, int LineNumber);
#define assert(p) ((p) ? (void)0 : DoAssertC(TEXT(#p), TEXT(__FILE__), __LINE__))
#endif

#endif
