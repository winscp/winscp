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

#if defined _MSC_VER && _MSC_VER < 1800
/* Work around lack of inttypes.h in older MSVC */
#define PRIx32 "x"
#define PRIu64 "I64u"
#define PRIdMAX "I64d"
#define PRIXMAX "I64X"
#define SCNu64 "I64u"
#else
#include <inttypes.h>
#endif

typedef struct conf_tag Conf;
typedef struct terminal_tag Terminal;
typedef struct term_utf8_decode term_utf8_decode;

typedef struct Filename Filename;
typedef struct FontSpec FontSpec;

typedef struct bufchain_tag bufchain;

typedef struct strbuf strbuf;

typedef struct RSAKey RSAKey;

typedef struct BinarySink BinarySink;
typedef struct BinarySource BinarySource;
typedef struct stdio_sink stdio_sink;
typedef struct bufchain_sink bufchain_sink;
typedef struct handle_sink handle_sink;

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

typedef struct prng prng;
typedef struct ssh_hashalg ssh_hashalg;
typedef struct ssh_hash ssh_hash;
typedef struct ssh_kex ssh_kex;
typedef struct ssh_kexes ssh_kexes;
typedef struct ssh_keyalg ssh_keyalg;
typedef struct ssh_key ssh_key;
typedef struct ssh_compressor ssh_compressor;
typedef struct ssh_decompressor ssh_decompressor;
typedef struct ssh_compression_alg ssh_compression_alg;
typedef struct ssh2_userkey ssh2_userkey;
typedef struct ssh2_macalg ssh2_macalg;
typedef struct ssh2_mac ssh2_mac;
typedef struct ssh_cipheralg ssh_cipheralg;
typedef struct ssh_cipher ssh_cipher;
typedef struct ssh2_ciphers ssh2_ciphers;
typedef struct dh_ctx dh_ctx;
typedef struct ecdh_key ecdh_key;

typedef struct dlgparam dlgparam;

typedef struct settings_w settings_w;
typedef struct settings_r settings_r;
typedef struct settings_e settings_e;

typedef struct SessionSpecial SessionSpecial;

typedef struct StripCtrlChars StripCtrlChars;

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

/* ----------------------------------------------------------------------
 * Platform-specific definitions.
 *
 * Most of these live in the per-platform header files, of which
 * puttyps.h selects the appropriate one. But some of the sources
 * (particularly standalone test applications) would prefer not to
 * have to include a per-platform header at all, because that makes it
 * more portable to platforms not supported by the code base as a
 * whole (for example, compiling purely computational parts of the
 * code for specialist platforms for test and analysis purposes). So
 * any definition that has to affect even _those_ modules will have to
 * go here, with the key constraint being that this code has to come
 * to _some_ decision even if the compilation platform is not a
 * recognised one at all.
 */

/* Purely computational code uses smemclr(), so we have to make the
 * decision here about whether that's provided by utils.c or by a
 * platform implementation. We define PLATFORM_HAS_SMEMCLR to suppress
 * utils.c's definition. */
#ifdef _WINDOWS
/* Windows provides the API function 'SecureZeroMemory', which we use
 * unless the user has told us not to by defining NO_SECUREZEROMEMORY. */
#ifndef NO_SECUREZEROMEMORY
#define PLATFORM_HAS_SMEMCLR
#endif
#endif

#endif /* PUTTY_DEFS_H */
