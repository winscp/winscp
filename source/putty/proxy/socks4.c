/*
 * SOCKS 4 proxy negotiation.
 */

#include "putty.h"
#include "network.h"
#include "proxy.h"
#include "socks.h"
#include "sshcr.h"

typedef struct Socks4ProxyNegotiator {
    int crLine;
    ProxyNegotiator pn;
} Socks4ProxyNegotiator;

static ProxyNegotiator *proxy_socks4_new(const ProxyNegotiatorVT *vt)
{
    Socks4ProxyNegotiator *s = snew(Socks4ProxyNegotiator);
    s->pn.vt = vt;
    s->crLine = 0;
    return &s->pn;
}

static void proxy_socks4_free(ProxyNegotiator *pn)
{
    Socks4ProxyNegotiator *s = container_of(pn, Socks4ProxyNegotiator, pn);
    sfree(s);
}

static void proxy_socks4_process_queue(ProxyNegotiator *pn)
{
    Socks4ProxyNegotiator *s = container_of(pn, Socks4ProxyNegotiator, pn);

    crBegin(s->crLine);

    {
        char hostname[512];
        bool write_hostname = false;

        /*
         * SOCKS 4 request packet:
         *
         *   byte     version
         *   byte     command
         *   uint16   destination port number
         *   uint32   destination IPv4 address (or something in the
         *            SOCKS4A_NAME_FOLLOWS range)
         *   asciz    username
         *   asciz    destination hostname (if we sent SOCKS4A_NAME_FOLLOWS_*)
         */

        put_byte(pn->output, SOCKS4_REQUEST_VERSION);
        put_byte(pn->output, SOCKS_CMD_CONNECT);
        put_uint16(pn->output, pn->ps->remote_port);

        switch (sk_addrtype(pn->ps->remote_addr)) {
          case ADDRTYPE_IPV4: {
            char addr[4];
            sk_addrcopy(pn->ps->remote_addr, addr);
            put_data(pn->output, addr, 4);
            break;
          }
          case ADDRTYPE_NAME:
            put_uint32(pn->output, SOCKS4A_NAME_FOLLOWS_BASE);
            sk_getaddr(pn->ps->remote_addr, hostname, lenof(hostname));
            write_hostname = true;
            break;
          case ADDRTYPE_IPV6:
            pn->error = dupstr("SOCKS version 4 does not support IPv6");
            crStopV;
        }

        put_asciz(pn->output, conf_get_str(pn->ps->conf, CONF_proxy_username));

        if (write_hostname)
            put_asciz(pn->output, hostname);
    }

    crReturnV;

    {
        unsigned char data[8];
        crMaybeWaitUntilV(bufchain_try_fetch_consume(pn->input, data, 8));

        /*
         * SOCKS 4 response packet:
         *
         *   byte     version
         *   byte     status
         *   uint16   port number
         *   uint32   IPv4 address
         *
         * We don't need to worry about the port and destination address.
         */

        if (data[0] != SOCKS4_REPLY_VERSION) {
            pn->error = dupprintf("SOCKS proxy response contained reply "
                                  "version number %d (expected 0)",
                                  (int)data[0]);
            crStopV;
        }

        switch (data[1]) {
          case SOCKS4_RESP_SUCCESS:
            pn->done = true;
            break;

          case SOCKS4_RESP_FAILURE:
            pn->error = dupstr("SOCKS server reported failure to connect");
            break;

          case SOCKS4_RESP_WANT_IDENTD:
            pn->error = dupstr("SOCKS server wanted IDENTD on client");
            break;

          case SOCKS4_RESP_IDENTD_MISMATCH:
            pn->error = dupstr("Username and IDENTD on client don't agree");
            break;

          default:
            pn->error = dupprintf("SOCKS server sent unrecognised error "
                                  "code %d", (int)data[1]);
            break;
        }
        crStopV;
    }

    crFinishV;
}

const struct ProxyNegotiatorVT socks4_proxy_negotiator_vt = {
    .new = proxy_socks4_new,
    .free = proxy_socks4_free,
    .process_queue = proxy_socks4_process_queue,
    .type = "SOCKS 4",
};
