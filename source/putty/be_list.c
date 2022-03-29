/*
 * Source file that is rebuilt per application, and provides the list
 * of backends, the default protocol, and the application name.
 *
 * This file expects the build system to provide some per-application
 * definitions on the compiler command line. So you don't just add it
 * directly to the sources list for an application. Instead you call
 * the be_list() function defined in setup.cmake, e.g.
 *
 *    be_list(target-name AppName [SSH] [SERIAL] [OTHERBACKENDS])
 *
 * This translates into the following command-line macro definitions
 * used by the code below:
 *
 *  - APPNAME should be defined to the name of the program, in
 *    user-facing capitalisation (e.g. PuTTY rather than putty).
 *    Unquoted: it's easier to stringify it in the preprocessor than
 *    to persuade cmake to put the right quotes on the command line on
 *    all build platforms.
 *
 *  - The following macros should each be defined to 1 if a given set
 *    of backends should be added to the backends[] list, or 0 if they
 *    should not be:
 *
 *     * SSH: the two SSH backends (SSH proper, and bare-ssh-connection)
 *
 *     * SERIAL: the serial port backend
 *
 *     * OTHERBACKENDS: the non-cryptographic network protocol backends
 *       (Telnet, Rlogin, SUPDUP, Raw)
 */

#include <stdio.h>
#include "putty.h"

const char *const appname = STR(APPNAME);

/*
 * Define the default protocol for the application. This is always a
 * network backend (serial ports come second behind network, in every
 * case). Applications that don't have either (such as pterm) don't
 * need this variable anyway, so just set it to -1.
 */
#if SSH
const int be_default_protocol = PROT_SSH;
#elif OTHERBACKENDS
const int be_default_protocol = PROT_TELNET;
#else
const int be_default_protocol = -1;
#endif

/*
 * List all the configured backends, in the order they should appear
 * in the config box.
 */
const struct BackendVtable *const backends[] = {
    /*
     * Start with the most-preferred network-remote-login protocol.
     * That's SSH if present, otherwise Telnet if present.
     */
#if SSH
    &ssh_backend,
#elif OTHERBACKENDS
    &telnet_backend,         /* Telnet at the top if SSH is absent  */
#endif

    /*
     * Second on the list is the serial-port backend, if available.
     */
#if SERIAL
    &serial_backend,
#endif

    /*
     * After that come the remaining network protocols: Telnet if it
     * hasn't already appeared above, and Rlogin, SUPDUP and Raw.
     */
#if OTHERBACKENDS && SSH
    &telnet_backend,         /* only if SSH displaced it at the top  */
#endif
#if OTHERBACKENDS
    &rlogin_backend,
    &supdup_backend,
    &raw_backend,
#endif

    /*
     * Bare ssh-connection / PSUSAN is a niche protocol and goes well
     * down the list.
     */
#if SSH
    &sshconn_backend,
#endif

    /*
     * Done. Null pointer to mark the end of the list.
     */
    NULL
};

/*
 * Number of backends at the start of the above list that should have
 * radio buttons in the config UI.
 *
 * The rule is: the most-preferred network backend, and Serial, each
 * get a radio button if present.
 *
 * The rest will be relegated to a dropdown list.
 */
const size_t n_ui_backends =
    0
#if SSH || OTHERBACKENDS
    + 1
#endif
#if SERIAL
    + 1
#endif
    ;
