/*
 * Decide whether the 'host name' or 'serial line' field of a Conf is
 * important, based on which protocol it has selected.
 */

#include "putty.h"

char const *conf_dest(Conf *conf)
{
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
        return conf_get_str(conf, CONF_serline);
    else
        return conf_get_str(conf, CONF_host);
}

