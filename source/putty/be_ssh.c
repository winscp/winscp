/*
 * Linking module for programs that are restricted to only using SSH
 * (pscp and psftp). These do not support selection of backend, but
 * must still have a backends[] array mentioning SSH because
 * settings.c will want to consult it during session load.
 */

#include <stdio.h>
#include "putty.h"

const int be_default_protocol = PROT_SSH;

const struct BackendVtable *const backends[] = {
    &ssh_backend,
    NULL
};
