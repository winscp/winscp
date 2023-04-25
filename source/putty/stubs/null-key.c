#include "misc.h"
#include "ssh.h"

unsigned nullkey_supported_flags(const ssh_keyalg *self)
{
    return 0;
}

const char *nullkey_alternate_ssh_id(const ssh_keyalg *self, unsigned flags)
{
    /* There are no alternate ids */
    return self->ssh_id;
}

ssh_key *nullkey_base_key(ssh_key *key)
{
    /* When a key is not certified, it is its own base */
    return key;
}

bool nullkey_variable_size_no(const ssh_keyalg *self) { return false; }
bool nullkey_variable_size_yes(const ssh_keyalg *self) { return true; }
