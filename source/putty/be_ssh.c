/*
 * Linking module for programs that are restricted to only using
 * SSH-type protocols (pscp and psftp). These still have a choice of
 * two actual backends, because they can also speak PROT_SSHCONN.
 */

#include <stdio.h>
#include "putty.h"

const int be_default_protocol = PROT_SSH;

const struct BackendVtable *const backends[] = {
    &ssh_backend,
    &sshconn_backend,
    NULL
};

const size_t n_ui_backends = 0;  /* not used in programs with a config UI */
