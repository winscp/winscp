/*
 * pageant.h: header for pageant.c.
 */

#include <stdarg.h>

/*
 * Upper limit on length of any agent message. Used as a basic sanity
 * check on messages' length fields, and used by the Windows Pageant
 * client IPC to decide how large a file mapping to allocate.
 */
#define AGENT_MAX_MSGLEN  262144

typedef struct PageantClientVtable PageantClientVtable;
typedef struct PageantClient PageantClient;
typedef struct PageantClientInfo PageantClientInfo;
typedef struct PageantClientRequestId PageantClientRequestId;
typedef struct PageantClientDialogId PageantClientDialogId;
struct PageantClient {
    const struct PageantClientVtable *vt;
    PageantClientInfo *info;    /* used by the central Pageant code */

    /* Setting this flag prevents the 'log' vtable entry from ever
     * being called, so that it's safe to make it NULL. This also
     * allows optimisations in the core code (it can avoid entire
     * loops that are only used for logging purposes). So you can also
     * set it dynamically if you find out at run time that you're not
     * doing logging. */
    bool suppress_logging;
};
struct PageantClientVtable {
    void (*log)(PageantClient *pc, PageantClientRequestId *reqid,
                const char *fmt, va_list ap);
    void (*got_response)(PageantClient *pc, PageantClientRequestId *reqid,
                         ptrlen response);
    bool (*ask_passphrase)(PageantClient *pc, PageantClientDialogId *dlgid,
                           const char *key_comment);
};

static inline void pageant_client_log_v(
    PageantClient *pc, PageantClientRequestId *reqid,
    const char *fmt, va_list ap)
{
    if (!pc->suppress_logging)
        pc->vt->log(pc, reqid, fmt, ap);
}
static inline PRINTF_LIKE(3, 4) void pageant_client_log(
    PageantClient *pc, PageantClientRequestId *reqid, const char *fmt, ...)
{
    if (!pc->suppress_logging) {
        va_list ap;
        va_start(ap, fmt);
        pc->vt->log(pc, reqid, fmt, ap);
        va_end(ap);
    }
}
static inline void pageant_client_got_response(
    PageantClient *pc, PageantClientRequestId *reqid, ptrlen response)
{ pc->vt->got_response(pc, reqid, response); }
static inline bool pageant_client_ask_passphrase(
    PageantClient *pc, PageantClientDialogId *dlgid, const char *comment)
{ return pc->vt->ask_passphrase(pc, dlgid, comment); }

/* PageantClientRequestId is used to match up responses to the agent
 * requests they refer to. A client may allocate one of these for each
 * call to pageant_handle_request, (probably as a subfield of some
 * larger struct on the client side) and expect the same pointer to be
 * passed back in pageant_client_got_response. */
struct PageantClientRequestId { int unused_; };

/*
 * Initial setup.
 */
void pageant_init(void);

/*
 * Register and unregister PageantClients. This is necessary so that
 * when a PageantClient goes away, any unfinished asynchronous
 * requests can be cleaned up.
 *
 * pageant_register_client will fill in pc->id. The client itself
 * should not touch that field.
 */
void pageant_register_client(PageantClient *pc);
void pageant_unregister_client(PageantClient *pc);

/*
 * The main agent function that answers messages.
 *
 * Expects a message/length pair as input, minus its initial length
 * field but still with its type code on the front.
 *
 * When a response is ready, the got_response method in the
 * PageantClient vtable will be passed it in the form of a ptrlen,
 * again minus its length field.
 */
void pageant_handle_msg(PageantClient *pc, PageantClientRequestId *reqid,
                        ptrlen msg);

/*
 * Send the core Pageant code a response to a passphrase request.
 */
void pageant_passphrase_request_success(PageantClientDialogId *dlgid,
                                        ptrlen passphrase);
void pageant_passphrase_request_refused(PageantClientDialogId *dlgid);

/*
 * Construct a list of public keys, just as the two LIST_IDENTITIES
 * requests would have returned them.
 */
void pageant_make_keylist1(BinarySink *);
void pageant_make_keylist2(BinarySink *);

/*
 * Accessor functions for Pageant's internal key lists, used by GUI
 * Pageant, to count the keys, to delete a key, or to re-encrypt a
 * decrypted-on-demand key (SSH-2 only).
 */
int pageant_count_ssh1_keys(void);
int pageant_count_ssh2_keys(void);
bool pageant_delete_nth_ssh1_key(int i);
bool pageant_delete_nth_ssh2_key(int i);
bool pageant_reencrypt_nth_ssh2_key(int i);
void pageant_delete_all(void);
void pageant_reencrypt_all(void);

/*
 * This callback must be provided by the Pageant front end code.
 * pageant_handle_msg calls it to indicate that the message it's just
 * handled has changed the list of keys held by the agent. Front ends
 * which expose that key list through dedicated UI may need to refresh
 * that UI's state in this function; other front ends can leave it
 * empty.
 */
void keylist_update(void);

/*
 * Functions to establish a listening socket speaking the SSH agent
 * protocol. Call pageant_listener_new() to set up a state; then
 * create a socket using the returned Plug; then call
 * pageant_listener_got_socket() to give the listening state its own
 * socket pointer. Also, provide a logging function later if you want
 * to.
 */
typedef struct PageantListenerClientVtable PageantListenerClientVtable;
typedef struct PageantListenerClient PageantListenerClient;
struct PageantListenerClient {
    const PageantListenerClientVtable *vt;
    /* suppress_logging flag works similarly to the one in
     * PageantClient, but it is only read when a new connection comes
     * in. So if you do need to change it in mid-run, expect existing
     * agent connections to still use the old value. */
    bool suppress_logging;
};
struct PageantListenerClientVtable {
    void (*log)(PageantListenerClient *, const char *fmt, va_list ap);
    bool (*ask_passphrase)(PageantListenerClient *pc,
                           PageantClientDialogId *dlgid,
                           const char *key_comment);
};

static inline void pageant_listener_client_log_v(
    PageantListenerClient *plc, const char *fmt, va_list ap)
{
    if (!plc->suppress_logging)
        plc->vt->log(plc, fmt, ap);
}
static inline PRINTF_LIKE(2, 3) void pageant_listener_client_log(
    PageantListenerClient *plc, const char *fmt, ...)
{
    if (!plc->suppress_logging) {
        va_list ap;
        va_start(ap, fmt);
        plc->vt->log(plc, fmt, ap);
        va_end(ap);
    }
}
static inline bool pageant_listener_client_ask_passphrase(
    PageantListenerClient *plc, PageantClientDialogId *dlgid,
    const char *comment)
{ return plc->vt->ask_passphrase(plc, dlgid, comment); }

struct pageant_listen_state;
struct pageant_listen_state *pageant_listener_new(
    Plug **plug, PageantListenerClient *plc);
void pageant_listener_got_socket(struct pageant_listen_state *pl, Socket *);
void pageant_listener_free(struct pageant_listen_state *pl);

/*
 * Functions to perform specific key actions, either as a client of an
 * ssh-agent running elsewhere, or directly on the agent state in this
 * process. (On at least one platform we want to do this in an
 * agnostic way between the two situations.)
 *
 * pageant_add_keyfile() is used to load a private key from a file and
 * add it to the agent. Initially, you should call it with passphrase
 * NULL, and it will check if the key is already in the agent, and
 * whether a passphrase is required. Return values are given in the
 * enum below. On return, *retstr will either be NULL, or a
 * dynamically allocated string containing a key comment or an error
 * message.
 *
 * pageant_add_keyfile() also remembers passphrases with which it's
 * successfully decrypted keys (because if you try to add multiple
 * keys in one go, you might very well have used the same passphrase
 * for keys that have the same trust properties). Call
 * pageant_forget_passphrases() to get rid of them all.
 */
enum {
    PAGEANT_ACTION_OK,       /* success; no further action needed */
    PAGEANT_ACTION_FAILURE,  /* failure; *retstr is error message */
    PAGEANT_ACTION_NEED_PP,  /* need passphrase: *retstr is key comment */
    PAGEANT_ACTION_WARNING,  /* success but with a warning message;
                              * *retstr is warning message */
};
int pageant_add_keyfile(Filename *filename, const char *passphrase,
                        char **retstr, bool add_encrypted);
void pageant_forget_passphrases(void);

struct pageant_pubkey {
    /* Everything needed to identify a public key found by
     * pageant_enum_keys and pass it back to the agent or other code
     * later */
    strbuf *blob;
    char *comment;
    int ssh_version;
};
struct pageant_pubkey *pageant_pubkey_copy(struct pageant_pubkey *key);
void pageant_pubkey_free(struct pageant_pubkey *key);

typedef void (*pageant_key_enum_fn_t)(void *ctx, char **fingerprints,
                                      const char *comment, uint32_t ext_flags,
                                      struct pageant_pubkey *key);
int pageant_enum_keys(pageant_key_enum_fn_t callback, void *callback_ctx,
                      char **retstr);
int pageant_delete_key(struct pageant_pubkey *key, char **retstr);
int pageant_delete_all_keys(char **retstr);
int pageant_reencrypt_key(struct pageant_pubkey *key, char **retstr);
int pageant_reencrypt_all_keys(char **retstr);
int pageant_sign(struct pageant_pubkey *key, ptrlen message, strbuf *out,
                 uint32_t flags, char **retstr);

/*
 * Definitions for agent protocol extensions.
 */
#define PUTTYEXT(base) base "@putty.projects.tartarus.org"

#define KNOWN_EXTENSIONS(X)                             \
    X(EXT_QUERY, "query")                               \
    X(EXT_ADD_PPK, PUTTYEXT("add-ppk"))                 \
    X(EXT_REENCRYPT, PUTTYEXT("reencrypt"))             \
    X(EXT_REENCRYPT_ALL, PUTTYEXT("reencrypt-all"))     \
    X(EXT_LIST_EXTENDED, PUTTYEXT("list-extended"))     \
    /* end of list */

#define LIST_EXTENDED_FLAG_HAS_ENCRYPTED_KEY_FILE    1
#define LIST_EXTENDED_FLAG_HAS_NO_CLEARTEXT_KEY      2
