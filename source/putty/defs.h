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

#ifdef WINSCP
#define HAVE_AES_NI 1
#define HAVE_WMEMCHR 1
#define HAVE_CMAKE_H 0
#define HAVE_NO_STDINT_H 0
#define HAVE_ARM_DIT 0
#define HAVE_STRTOUMAX 1
#endif

#ifndef WINSCP
#ifdef NDEBUG
/*
 * PuTTY is a security project, so assertions are important - if an
 * assumption is violated, proceeding anyway may have far worse
 * consequences than simple program termination. This check and #error
 * should arrange that we don't ever accidentally compile assertions
 * out.
 */
#error Do not compile this code base with NDEBUG defined!
#endif
#endif

#if HAVE_CMAKE_H
#include "cmake.h"
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>                     /* for __MINGW_PRINTF_FORMAT */
#include <stdbool.h>

#if defined _MSC_VER && _MSC_VER < 1800
/* Work around lack of inttypes.h and strtoumax in older MSVC */
#define PRIx32 "x"
#define PRIu32 "u"
#define PRIu64 "I64u"
#define PRIdMAX "I64d"
#define PRIXMAX "I64X"
#define SCNu64 "I64u"
#define SIZEx "Ix"
#define SIZEu "Iu"
/* Also, define a LEGACY_WINDOWS flag to enable other workarounds */
#define LEGACY_WINDOWS
#else
#include <inttypes.h>
/* Because we still support older MSVC libraries which don't recognise the
 * standard C "z" modifier for size_t-sized integers, we must use an
 * inttypes.h-style macro for those */
#define SIZEx "zx"
#define SIZEu "zu"
#endif

#if !HAVE_WMEMCHR
/* Work around lack of wmemchr in older MSVC */
wchar_t *wmemchr(const wchar_t *s, wchar_t c, size_t n);
#endif

#if defined __GNUC__ || defined __clang__
/*
 * On MinGW, the correct compiler format checking for vsnprintf() etc
 * can depend on compile-time flags; these control whether you get
 * ISO C or Microsoft's non-standard format strings.
 * We sometimes use __attribute__ ((format)) for our own printf-like
 * functions, which are ultimately interpreted by the toolchain-chosen
 * printf, so we need to take that into account to get correct warnings.
 */
#ifdef __MINGW_PRINTF_FORMAT
#define PRINTF_LIKE(fmt_index, ellipsis_index) \
    __attribute__ ((format (__MINGW_PRINTF_FORMAT, fmt_index, ellipsis_index)))
#else
#define PRINTF_LIKE(fmt_index, ellipsis_index) \
    __attribute__ ((format (printf, fmt_index, ellipsis_index)))
#endif
#else /* __GNUC__ */
#define PRINTF_LIKE(fmt_index, ellipsis_index)
#endif /* __GNUC__ */

typedef struct conf_tag Conf;
typedef struct ConfKeyInfo ConfKeyInfo;
typedef struct ConfSaveEnumValue ConfSaveEnumValue;
typedef struct ConfSaveEnumType ConfSaveEnumType;
typedef struct CmdlineArgList CmdlineArgList;
typedef struct CmdlineArg CmdlineArg;

typedef struct terminal_tag Terminal;
typedef struct term_utf8_decode term_utf8_decode;

typedef struct Filename Filename;
typedef struct FontSpec FontSpec;

typedef struct bufchain_tag bufchain;

typedef struct strbuf strbuf;
typedef struct LoadedFile LoadedFile;

typedef struct RSAKey RSAKey;

typedef struct BinarySink BinarySink;
typedef struct BinarySource BinarySource;
typedef struct stdio_sink stdio_sink;
typedef struct bufchain_sink bufchain_sink;
typedef struct buffer_sink buffer_sink;
typedef struct handle_sink handle_sink;

typedef struct IdempotentCallback IdempotentCallback;

typedef struct SockAddr SockAddr;

typedef struct Socket Socket;
typedef struct Plug Plug;
typedef struct SocketEndpointInfo SocketEndpointInfo;
typedef struct DeferredSocketOpener DeferredSocketOpener;
typedef struct DeferredSocketOpenerVtable DeferredSocketOpenerVtable;

typedef struct Backend Backend;
typedef struct BackendVtable BackendVtable;
typedef struct Interactor Interactor;
typedef struct InteractorVtable InteractorVtable;
typedef struct InteractionReadySeat InteractionReadySeat;

typedef struct Ldisc_tag Ldisc;
typedef struct LogContext LogContext;
typedef struct LogPolicy LogPolicy;
typedef struct LogPolicyVtable LogPolicyVtable;

typedef struct TermLineEditor TermLineEditor;
typedef struct TermLineEditorCallbackReceiver TermLineEditorCallbackReceiver;
typedef struct TermLineEditorCallbackReceiverVtable
    TermLineEditorCallbackReceiverVtable;

typedef struct Seat Seat;
typedef struct SeatVtable SeatVtable;
typedef struct SeatDialogText SeatDialogText;
typedef struct SeatDialogTextItem SeatDialogTextItem;
typedef struct SeatDialogPromptDescriptions SeatDialogPromptDescriptions;
typedef struct SeatPromptResult SeatPromptResult;

typedef struct cmdline_get_passwd_input_state cmdline_get_passwd_input_state;

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

typedef struct SshServerConfig SshServerConfig;
typedef struct SftpServer SftpServer;
typedef struct SftpServerVtable SftpServerVtable;

typedef struct Channel Channel;
typedef struct SshChannel SshChannel;
typedef struct mainchan mainchan;

typedef struct CertExprBuilder CertExprBuilder;

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
typedef struct ecdh_keyalg ecdh_keyalg;
typedef struct pq_kemalg pq_kemalg;
typedef struct pq_kem_dk pq_kem_dk;
typedef struct NTRUKeyPair NTRUKeyPair;
typedef struct NTRUEncodeSchedule NTRUEncodeSchedule;
typedef struct RFC6979 RFC6979;
typedef struct RFC6979Result RFC6979Result;
typedef struct ShakeXOF ShakeXOF;

typedef struct dlgparam dlgparam;
typedef struct dlgcontrol dlgcontrol;

typedef struct settings_w settings_w;
typedef struct settings_r settings_r;
typedef struct settings_e settings_e;
typedef struct ca_options ca_options;
typedef struct host_ca host_ca;
typedef struct host_ca_enum host_ca_enum;

typedef struct SessionSpecial SessionSpecial;

typedef struct StripCtrlChars StripCtrlChars;

typedef struct BidiContext BidiContext;

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

struct unicode_data;

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

#undef NORETURN // WINSCP
#if defined __GNUC__ || defined __clang__
#define NORETURN __attribute__((__noreturn__))
#elif defined _MSC_VER
#define NORETURN __declspec(noreturn)
#else
#define NORETURN
#endif

/*
 * Standard macro definitions. STR() behaves like the preprocessor
 * stringification # operator, and CAT() behaves like the token paste
 * ## operator, except that each one macro-expands its argument(s)
 * first, unlike the raw version. E.g.
 *
 *   #__LINE__               ->    "__LINE__"
 *   STR(__LINE__)           ->    "1234"             (or whatever)
 *
 * and similarly,
 *
 *   foo ## __LINE__         ->    foo__LINE__
 *   CAT(foo, __LINE__)      ->    foo1234            (or whatever)
 *
 * The expansion is achieved by having each macro pass its arguments
 * to a secondary inner macro, because parameter lists of a macro call
 * get expanded before the called macro is invoked. So STR(__LINE__)
 * -> STR_INNER(1234) -> #1234 -> "1234", and similarly for CAT.
 */
#define STR_INNER(x) #x
#define STR(x) STR_INNER(x)
#define CAT_INNER(x,y) x ## y
#define CAT(x,y) CAT_INNER(x,y)

/*
 * Structure shared between ssh.h and storage.h, giving strictness
 * options relating to checking of an OpenSSH certificate. It's a bit
 * cheaty to put something so specific in here, but more painful to
 * put it in putty.h.
 */
struct ca_options {
    bool permit_rsa_sha1, permit_rsa_sha256, permit_rsa_sha512;
};

#endif /* PUTTY_DEFS_H */
