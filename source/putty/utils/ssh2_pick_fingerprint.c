/*
 * Choose an SSH-2 fingerprint type, out of an array of possible ones.
 */

#include "defs.h"
#include "misc.h"
#include "ssh.h"

FingerprintType ssh2_pick_fingerprint(
    char **fingerprints, FingerprintType preferred_type)
{
    /*
     * Keys are either SSH-2, in which case we have all fingerprint
     * types, or SSH-1, in which case we have only MD5. So we return
     * the default type if we can, or MD5 if that's all we have; no
     * need for a fully general preference-list system.
     */
    FingerprintType fptype = fingerprints[preferred_type] ?
        preferred_type : SSH_FPTYPE_MD5;
    assert(fingerprints[fptype]);
    return fptype;
}

FingerprintType ssh2_pick_default_fingerprint(char **fingerprints)
{
    return ssh2_pick_fingerprint(fingerprints, SSH_FPTYPE_DEFAULT);
}
