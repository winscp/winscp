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

typedef void (*pageant_logfn_t)(void *logctx, const char *fmt, va_list ap);

/*
 * Initial setup.
 */
void pageant_init(void);

/*
 * The main agent function that answers messages.
 *
 * Expects a message/length pair as input, minus its initial length
 * field but still with its type code on the front.
 *
 * Returns a fully formatted message as output, *with* its initial
 * length field, and sets *outlen to the full size of that message.
 */
void pageant_handle_msg(BinarySink *bs,
                        const void *msg, int msglen,
                        void *logctx, pageant_logfn_t logfn);

/*
 * Construct a failure response. Useful for agent front ends which
 * suffer a problem before they even get to pageant_handle_msg.
 *
 * 'log_reason' is only used if logfn is not NULL.
 */
void pageant_failure_msg(BinarySink *bs,
                         const char *log_reason,
                         void *logctx, pageant_logfn_t logfn);

/*
 * Construct a list of public keys, just as the two LIST_IDENTITIES
 * requests would have returned them.
 */
void pageant_make_keylist1(BinarySink *);
void pageant_make_keylist2(BinarySink *);

/*
 * Accessor functions for Pageant's internal key lists. Fetch the nth
 * key; count the keys; attempt to add a key (returning true on
 * success, in which case the ownership of the key structure has been
 * taken over by pageant.c); attempt to delete a key (returning true
 * on success, in which case the ownership of the key structure is
 * passed back to the client).
 */
RSAKey *pageant_nth_ssh1_key(int i);
ssh2_userkey *pageant_nth_ssh2_key(int i);
int pageant_count_ssh1_keys(void);
int pageant_count_ssh2_keys(void);
bool pageant_add_ssh1_key(RSAKey *rkey);
bool pageant_add_ssh2_key(ssh2_userkey *skey);
bool pageant_delete_ssh1_key(RSAKey *rkey);
bool pageant_delete_ssh2_key(ssh2_userkey *skey);

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
struct pageant_listen_state;
struct pageant_listen_state *pageant_listener_new(Plug **plug);
void pageant_listener_got_socket(struct pageant_listen_state *pl, Socket *);
void pageant_listener_set_logfn(struct pageant_listen_state *pl,
                                void *logctx, pageant_logfn_t logfn);
void pageant_listener_free(struct pageant_listen_state *pl);

/*
 * Functions to perform specific key actions, either as a client of an
 * ssh-agent running elsewhere, or directly on the agent state in this
 * process. (On at least one platform we want to do this in an
 * agnostic way between the two situations.)
 *
 * pageant_get_keylist{1,2} work just like pageant_make_keylist{1,2}
 * above, except that they can also cope if they have to contact an
 * external agent.
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
void *pageant_get_keylist1(int *length);
void *pageant_get_keylist2(int *length);
enum {
    PAGEANT_ACTION_OK,       /* success; no further action needed */
    PAGEANT_ACTION_FAILURE,  /* failure; *retstr is error message */
    PAGEANT_ACTION_NEED_PP   /* need passphrase: *retstr is key comment */
};
int pageant_add_keyfile(Filename *filename, const char *passphrase,
                        char **retstr);
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

typedef void (*pageant_key_enum_fn_t)(void *ctx,
                                      const char *fingerprint,
                                      const char *comment,
                                      struct pageant_pubkey *key);
int pageant_enum_keys(pageant_key_enum_fn_t callback, void *callback_ctx,
                      char **retstr);
int pageant_delete_key(struct pageant_pubkey *key, char **retstr);
int pageant_delete_all_keys(char **retstr);
