/*
 * defs.h: initial definitions for PuTTY.
 *
 * The rule about this header file is that it can't depend on any
 * other header file in this code base. This is where we define
 * things, as much as we can, that other headers will want to refer
 * to, such as opaque structure types and their associated typedefs,
 * or macros that are used by other headers.
 */

#ifndef PUTTY_DEFS_H
#define PUTTY_DEFS_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef WINSCP
// This is used in pageant, pscp, psftp and servers only
#if defined _MSC_VER && _MSC_VER < 1800
/* Work around lack of inttypes.h in older MSVC */
#define PRIx32 "x"
#define PRIu64 "I64u"
#define SCNu64 "I64u"
#else
#include <inttypes.h>
#endif
#endif

typedef struct conf_tag Conf;
typedef struct terminal_tag Terminal;

typedef struct Filename Filename;
typedef struct FontSpec FontSpec;

typedef struct bufchain_tag bufchain;

typedef struct strbuf strbuf;

struct RSAKey;

typedef struct BinarySink BinarySink;
typedef struct BinarySource BinarySource;

typedef struct IdempotentCallback IdempotentCallback;

typedef struct SockAddr SockAddr;

typedef struct Socket Socket;
typedef struct Plug Plug;
typedef struct SocketPeerInfo SocketPeerInfo;

typedef struct Backend Backend;
typedef struct BackendVtable BackendVtable;

typedef struct Ldisc_tag Ldisc;
typedef struct LogContext LogContext;
typedef struct LogPolicy LogPolicy;
typedef struct LogPolicyVtable LogPolicyVtable;

typedef struct Seat Seat;
typedef struct SeatVtable SeatVtable;

typedef struct TermWin TermWin;
typedef struct TermWinVtable TermWinVtable;

typedef struct Ssh Ssh;

typedef struct mp_int mp_int;
typedef struct MontyContext MontyContext;

typedef struct WeierstrassCurve WeierstrassCurve;
typedef struct WeierstrassPoint WeierstrassPoint;
typedef struct MontgomeryCurve MontgomeryCurve;
typedef struct MontgomeryPoint MontgomeryPoint;
typedef struct EdwardsCurve EdwardsCurve;
typedef struct EdwardsPoint EdwardsPoint;

typedef struct SftpServer SftpServer;
typedef struct SftpServerVtable SftpServerVtable;

typedef struct Channel Channel;
typedef struct SshChannel SshChannel;
typedef struct mainchan mainchan;

typedef struct ssh_sharing_state ssh_sharing_state;
typedef struct ssh_sharing_connstate ssh_sharing_connstate;
typedef struct share_channel share_channel;

typedef struct PortFwdManager PortFwdManager;
typedef struct PortFwdRecord PortFwdRecord;
typedef struct ConnectionLayer ConnectionLayer;

typedef struct dlgparam dlgparam;

typedef struct settings_w settings_w;
typedef struct settings_r settings_r;
typedef struct settings_e settings_e;

typedef struct SessionSpecial SessionSpecial;

/*
 * A small structure wrapping up a (pointer, length) pair so that it
 * can be conveniently passed to or from a function.
 */
typedef struct ptrlen {
    const void *ptr;
    size_t len;
} ptrlen;

typedef struct logblank_t logblank_t;

typedef struct BinaryPacketProtocol BinaryPacketProtocol;
typedef struct PacketProtocolLayer PacketProtocolLayer;

/* Do a compile-time type-check of 'to_check' (without evaluating it),
 * as a side effect of returning the value 'to_return'. Note that
 * although this macro double-*expands* to_return, it always
 * *evaluates* exactly one copy of it, so it's side-effect safe. */
#define TYPECHECK(to_check, to_return)                  \
    (sizeof(to_check) ? (to_return) : (to_return))

/* Return a pointer to the object of structure type 'type' whose field
 * with name 'field' is pointed at by 'object'. */
#define container_of(object, type, field)                               \
    TYPECHECK(object == &((type *)0)->field,                            \
              ((type *)(((char *)(object)) - offsetof(type, field))))

#if defined __GNUC__ || defined __clang__
#define NORETURN __attribute__((__noreturn__))
#else
#define NORETURN
#endif

#endif /* PUTTY_DEFS_H */
