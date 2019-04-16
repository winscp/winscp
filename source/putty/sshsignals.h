/*
 * List of signal names known to SSH, indicating whether PuTTY's UI
 * for special session commands likes to put them in the main specials
 * menu or in a submenu (and if the former, what title they have).
 *
 * This is a separate header file rather than my usual style of a
 * parametric list macro, because in this case I need to be able to
 * #ifdef out each mode in case it's not defined on a particular
 * target system.
 *
 * If you want only the locally defined signals, #define
 * SIGNALS_LOCAL_ONLY before including this header.
 */

#if !defined SIGNALS_LOCAL_ONLY || defined SIGINT
SIGNAL_MAIN(INT, "Interrupt")
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGTERM
SIGNAL_MAIN(TERM, "Terminate")
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGKILL
SIGNAL_MAIN(KILL, "Kill")
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGQUIT
SIGNAL_MAIN(QUIT, "Quit")
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGHUP
SIGNAL_MAIN(HUP, "Hangup")
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGABRT
SIGNAL_SUB(ABRT)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGALRM
SIGNAL_SUB(ALRM)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGFPE
SIGNAL_SUB(FPE)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGILL
SIGNAL_SUB(ILL)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGPIPE
SIGNAL_SUB(PIPE)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGSEGV
SIGNAL_SUB(SEGV)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGUSR1
SIGNAL_SUB(USR1)
#endif
#if !defined SIGNALS_LOCAL_ONLY || defined SIGUSR2
SIGNAL_SUB(USR2)
#endif
