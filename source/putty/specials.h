/*
 * Commands that are generally useful in multiple backends.
 */
SPECIAL(BRK)    /* serial-line break */
SPECIAL(EOF)    /* end-of-file on session input */
SPECIAL(NOP)    /* transmit data with no effect */
SPECIAL(PING)   /* try to keep the session alive (probably, but not
                 * necessarily, implemented as SS_NOP) */

/*
 * Commands specific to Telnet.
 */
SPECIAL(AYT)    /* Are You There */
SPECIAL(SYNCH)  /* Synch */
SPECIAL(EC)     /* Erase Character */
SPECIAL(EL)     /* Erase Line */
SPECIAL(GA)     /* Go Ahead */
SPECIAL(ABORT)  /* Abort Process */
SPECIAL(AO)     /* Abort Output */
SPECIAL(IP)     /* Interrupt Process */
SPECIAL(SUSP)   /* Suspend Process */
SPECIAL(EOR)    /* End Of Record */
SPECIAL(EOL)    /* Telnet end-of-line sequence (CRLF, as opposed to
                 * CR NUL that escapes a literal CR) */

/*
 * Commands specific to SSH.
 */
SPECIAL(REKEY)  /* trigger an immediate repeat key exchange */
SPECIAL(XCERT)  /* cross-certify another host key ('arg' indicates which) */

/*
 * Send a POSIX-style signal. (Useful in SSH and also pterm.)
 *
 * We use the master list in ssh/signal-list.h to define these enum
 * values, which will come out looking like names of the form
 * SS_SIGABRT, SS_SIGINT etc.
 */
#define SIGNAL_MAIN(name, text) SPECIAL(SIG ## name)
#define SIGNAL_SUB(name) SPECIAL(SIG ## name)
#include "ssh/signal-list.h"
#undef SIGNAL_MAIN
#undef SIGNAL_SUB

/*
 * These aren't really special commands, but they appear in the
 * enumeration because the list returned from backend_get_specials()
 * will use them to specify the structure of the GUI specials menu.
 */
SPECIAL(SEP)      /* Separator */
SPECIAL(SUBMENU)  /* Start a new submenu with specified name */
SPECIAL(EXITMENU) /* Exit current submenu, or end of entire specials list */
