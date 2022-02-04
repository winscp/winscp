/*
 * Constants used in the SOCKS protocols.
 */

/* Command codes common to both versions */
#define SOCKS_CMD_CONNECT 1
#define SOCKS_CMD_BIND 2

/* SOCKS 4 definitions */

#define SOCKS4_REQUEST_VERSION 4
#define SOCKS4_REPLY_VERSION 0

#define SOCKS4_RESP_SUCCESS         90
#define SOCKS4_RESP_FAILURE         91
#define SOCKS4_RESP_WANT_IDENTD     92
#define SOCKS4_RESP_IDENTD_MISMATCH 93

/*
 * Special nonsense IP address range, used as a signal to indicate
 * that an ASCIZ hostname follows the user id field.
 *
 * Strictly speaking, the use of this extension indicates that we're
 * speaking SOCKS 4A rather than vanilla SOCKS 4, although we don't
 * bother to draw the distinction.
 */
#define SOCKS4A_NAME_FOLLOWS_BASE  0x00000001 /* inclusive */
#define SOCKS4A_NAME_FOLLOWS_LIMIT 0x00000100 /* exclusive */

/* SOCKS 5 definitions */

#define SOCKS5_REQUEST_VERSION 5
#define SOCKS5_REPLY_VERSION 5

/* Extra command codes extending the SOCKS_CMD_* list above */
#define SOCKS5_CMD_UDP_ASSOCIATE 3

#define SOCKS5_AUTH_NONE 0
#define SOCKS5_AUTH_GSSAPI 1
#define SOCKS5_AUTH_PASSWORD 2
#define SOCKS5_AUTH_CHAP 3
#define SOCKS5_AUTH_REJECTED 0xFF /* used in reply to indicate 'no
                                   * acceptable method offered' */

#define SOCKS5_AUTH_PASSWORD_VERSION 1

#define SOCKS5_AUTH_CHAP_VERSION 1

#define SOCKS5_AUTH_CHAP_ATTR_STATUS     0x00
#define SOCKS5_AUTH_CHAP_ATTR_INFO       0x01
#define SOCKS5_AUTH_CHAP_ATTR_USERNAME   0x02
#define SOCKS5_AUTH_CHAP_ATTR_CHALLENGE  0x03
#define SOCKS5_AUTH_CHAP_ATTR_RESPONSE   0x04
#define SOCKS5_AUTH_CHAP_ATTR_CHARSET    0x05
#define SOCKS5_AUTH_CHAP_ATTR_IDENTIFIER 0x10
#define SOCKS5_AUTH_CHAP_ATTR_ALGLIST    0x11

#define SOCKS5_AUTH_CHAP_ALG_HMACMD5   0x85

#define SOCKS5_ADDR_IPV4 1
#define SOCKS5_ADDR_IPV6 4
#define SOCKS5_ADDR_HOSTNAME 3

#define SOCKS5_RESP_SUCCESS 0
#define SOCKS5_RESP_FAILURE 1
#define SOCKS5_RESP_CONNECTION_NOT_ALLOWED_BY_RULESET 2
#define SOCKS5_RESP_NETWORK_UNREACHABLE 3
#define SOCKS5_RESP_HOST_UNREACHABLE 4
#define SOCKS5_RESP_CONNECTION_REFUSED 5
#define SOCKS5_RESP_TTL_EXPIRED 6
#define SOCKS5_RESP_COMMAND_NOT_SUPPORTED 7
#define SOCKS5_RESP_ADDRTYPE_NOT_SUPPORTED 8
