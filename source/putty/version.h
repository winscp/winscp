/*
 * This header file provides the various versioning-related #defines
 * for a particular PuTTY build.
 *
 * When my automated build system does a full build, Buildscr
 * completely overwrites this file with information derived from the
 * circumstances and type of that build. The information _here_ is
 * default stuff used for local development runs of 'make'.
 */

#define TEXTVER "Unidentified build"
#define SSHVER "-Unidentified-Local-Build"
#define BINARY_VERSION 0,0,0,0

#ifndef SOURCE_COMMIT
/*
 * git commit id from which this build was made. This is defined by
 * Buildscr for official builds - both source archives and prebuilt
 * binaries - in the course of overwriting this file as described
 * above. But we put it here under ifdef, so that it can also be
 * passed in on the command line for Unix local development builds,
 * which I treat specially because Unix developers - e.g. me - are
 * quite likely to run 'make install' straight out of their dev
 * directory so as to use the bleeding-edge code for day-to-day
 * running.
 *
 * Windows doesn't really need the same treatment, because the easiest
 * way to install a build properly on Windows is to run the installer,
 * and the easiest way to do that is to run Buildscr, which will
 * populate this field its own way. It's only the Unix automake build
 * where you might go straight from local 'make' to 'make install'
 * without going through Buildscr.
 */
#define SOURCE_COMMIT "unavailable"
#endif
