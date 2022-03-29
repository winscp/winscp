/*
 * List of SSH terminal modes, indicating whether SSH types them as
 * char or boolean, and if they're boolean, which POSIX flags field of
 * a termios structure they appear in, and what bit mask removes them
 * (e.g. CS7 and CS8 aren't single bits).
 *
 * Sources: RFC 4254, SSH-1 RFC-1.2.31, POSIX 2017, and the Linux
 * termios manpage for flags not specified by POSIX.
 *
 * This is a separate header file rather than my usual style of a
 * parametric list macro, because in this case I need to be able to
 * #ifdef out each mode in case it's not defined on a particular
 * target system.
 *
 * If you want only the locally defined modes, #define
 * TTYMODES_LOCAL_ONLY before including this header.
 */
#if !defined TTYMODES_LOCAL_ONLY || defined VINTR
TTYMODE_CHAR(INTR, 1, VINTR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VQUIT
TTYMODE_CHAR(QUIT, 2, VQUIT)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VERASE
TTYMODE_CHAR(ERASE, 3, VERASE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VKILL
TTYMODE_CHAR(KILL, 4, VKILL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VEOF
TTYMODE_CHAR(EOF, 5, VEOF)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VEOL
TTYMODE_CHAR(EOL, 6, VEOL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VEOL2
TTYMODE_CHAR(EOL2, 7, VEOL2)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VSTART
TTYMODE_CHAR(START, 8, VSTART)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VSTOP
TTYMODE_CHAR(STOP, 9, VSTOP)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VSUSP
TTYMODE_CHAR(SUSP, 10, VSUSP)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VDSUSP
TTYMODE_CHAR(DSUSP, 11, VDSUSP)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VREPRINT
TTYMODE_CHAR(REPRINT, 12, VREPRINT)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VWERASE
TTYMODE_CHAR(WERASE, 13, VWERASE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VLNEXT
TTYMODE_CHAR(LNEXT, 14, VLNEXT)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VFLUSH
TTYMODE_CHAR(FLUSH, 15, VFLUSH)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VSWTCH
TTYMODE_CHAR(SWTCH, 16, VSWTCH)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VSTATUS
TTYMODE_CHAR(STATUS, 17, VSTATUS)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined VDISCARD
TTYMODE_CHAR(DISCARD, 18, VDISCARD)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IGNPAR
TTYMODE_FLAG(IGNPAR, 30, i, IGNPAR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined PARMRK
TTYMODE_FLAG(PARMRK, 31, i, PARMRK)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined INPCK
TTYMODE_FLAG(INPCK, 32, i, INPCK)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ISTRIP
TTYMODE_FLAG(ISTRIP, 33, i, ISTRIP)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined INLCR
TTYMODE_FLAG(INLCR, 34, i, INLCR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IGNCR
TTYMODE_FLAG(IGNCR, 35, i, IGNCR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ICRNL
TTYMODE_FLAG(ICRNL, 36, i, ICRNL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IUCLC
TTYMODE_FLAG(IUCLC, 37, i, IUCLC)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IXON
TTYMODE_FLAG(IXON, 38, i, IXON)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IXANY
TTYMODE_FLAG(IXANY, 39, i, IXANY)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IXOFF
TTYMODE_FLAG(IXOFF, 40, i, IXOFF)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IMAXBEL
TTYMODE_FLAG(IMAXBEL, 41, i, IMAXBEL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IUTF8
TTYMODE_FLAG(IUTF8, 42, i, IUTF8)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ISIG
TTYMODE_FLAG(ISIG, 50, l, ISIG)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ICANON
TTYMODE_FLAG(ICANON, 51, l, ICANON)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined XCASE
TTYMODE_FLAG(XCASE, 52, l, XCASE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHO
TTYMODE_FLAG(ECHO, 53, l, ECHO)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHOE
TTYMODE_FLAG(ECHOE, 54, l, ECHOE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHOK
TTYMODE_FLAG(ECHOK, 55, l, ECHOK)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHONL
TTYMODE_FLAG(ECHONL, 56, l, ECHONL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined NOFLSH
TTYMODE_FLAG(NOFLSH, 57, l, NOFLSH)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined TOSTOP
TTYMODE_FLAG(TOSTOP, 58, l, TOSTOP)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined IEXTEN
TTYMODE_FLAG(IEXTEN, 59, l, IEXTEN)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHOCTL
TTYMODE_FLAG(ECHOCTL, 60, l, ECHOCTL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ECHOKE
TTYMODE_FLAG(ECHOKE, 61, l, ECHOKE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined PENDIN
TTYMODE_FLAG(PENDIN, 62, l, PENDIN)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined OPOST
TTYMODE_FLAG(OPOST, 70, o, OPOST)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined OLCUC
TTYMODE_FLAG(OLCUC, 71, o, OLCUC)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ONLCR
TTYMODE_FLAG(ONLCR, 72, o, ONLCR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined OCRNL
TTYMODE_FLAG(OCRNL, 73, o, OCRNL)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ONOCR
TTYMODE_FLAG(ONOCR, 74, o, ONOCR)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined ONLRET
TTYMODE_FLAG(ONLRET, 75, o, ONLRET)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined CS7
TTYMODE_FLAG(CS7, 90, c, CSIZE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined CS8
TTYMODE_FLAG(CS8, 91, c, CSIZE)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined PARENB
TTYMODE_FLAG(PARENB, 92, c, PARENB)
#endif
#if !defined TTYMODES_LOCAL_ONLY || defined PARODD
TTYMODE_FLAG(PARODD, 93, c, PARODD)
#endif
