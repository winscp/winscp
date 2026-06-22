/*
 * Implement LocalProxyOpener, a centralised system for setting up the
 * command string to be run by platform-specific local-subprocess
 * proxy types.
 *
 * The platform-specific local proxy code is expected to use this
 * system by calling local_proxy_opener() from
 * platform_new_connection(); then using the resulting
 * DeferredSocketOpener to make a deferred version of whatever local
 * socket type is used for talking to subcommands (Unix FdSocket,
 * Windows HandleSocket); then passing the 'Socket *' back to us via
 * local_proxy_opener_set_socket().
 *
 * The LocalProxyOpener object implemented by this code will set
 * itself up as an Interactor if possible, so that it can prompt for
 * the proxy username and/or password if they're referred to in the
 * command string but not given in the config (exactly as the Telnet
 * proxy does). Once it knows the exact command it wants to run -
 * whether that was done immediately or after user interaction - it
 * calls back to platform_setup_local_proxy() with the full command,
 * which is expected to actually start the subprocess and fill in the
 * missing details in the deferred socket, freeing the
 * LocalProxyOpener as a side effect.
 */

#include "tree234.h"
#include "putty.h"
#include "network.h"
#include "sshcr.h"
#include "proxy/proxy.h"

typedef struct LocalProxyOpener {
    int crLine;

    Socket *socket;
    char *formatted_cmd;
    Plug *plug;
    SockAddr *addr;
    int port;
    Conf *conf;

    Interactor *clientitr;
    LogPolicy *clientlp;
    Seat *clientseat;
    prompts_t *prompts;
    int username_prompt_index, password_prompt_index;

    Interactor interactor;
    DeferredSocketOpener opener;
} LocalProxyOpener;

static void local_proxy_opener_free(DeferredSocketOpener *opener)
{
    LocalProxyOpener *lp = container_of(opener, LocalProxyOpener, opener);
    burnstr(lp->formatted_cmd);
    if (lp->prompts)
        free_prompts(lp->prompts);
    sk_addr_free(lp->addr);
    conf_free(lp->conf);
    sfree(lp);
}

static const DeferredSocketOpenerVtable LocalProxyOpener_openervt = {
    /*WINSCP .free =*/ local_proxy_opener_free,
};

static char *local_proxy_opener_description(Interactor *itr)
{
    return dupstr("connection via local command");
}

static LogPolicy *local_proxy_opener_logpolicy(Interactor *itr)
{
    LocalProxyOpener *lp = container_of(itr, LocalProxyOpener, interactor);
    return lp->clientlp;
}

static Seat *local_proxy_opener_get_seat(Interactor *itr)
{
    LocalProxyOpener *lp = container_of(itr, LocalProxyOpener, interactor);
    return lp->clientseat;
}

static void local_proxy_opener_set_seat(Interactor *itr, Seat *seat)
{
    LocalProxyOpener *lp = container_of(itr, LocalProxyOpener, interactor);
    lp->clientseat = seat;
}

static const InteractorVtable LocalProxyOpener_interactorvt = {
    // WINSCP
    /*.description =*/ local_proxy_opener_description,
    /*.logpolicy =*/ local_proxy_opener_logpolicy,
    /*.get_seat =*/ local_proxy_opener_get_seat,
    /*.set_seat =*/ local_proxy_opener_set_seat,
};

static void local_proxy_opener_cleanup_interactor(LocalProxyOpener *lp)
{
    if (lp->clientseat) {
        interactor_return_seat(lp->clientitr);
        lp->clientitr = NULL;
        lp->clientseat = NULL;
    }
}

static void local_proxy_opener_coroutine(void *vctx)
{
    LocalProxyOpener *lp = (LocalProxyOpener *)vctx;

    crBegin(lp->crLine);

    /*
     * Make an initial attempt to figure out the command we want, and
     * see if it tried to include a username or password that we don't
     * have.
     */
    {
        unsigned flags;
        lp->formatted_cmd = format_telnet_command(
            lp->addr, lp->port, lp->conf, &flags);

        if (lp->clientseat && (flags & (TELNET_CMD_MISSING_USERNAME |
                                        TELNET_CMD_MISSING_PASSWORD))) {
            burnstr(lp->formatted_cmd);
            lp->formatted_cmd = NULL;

            /*
             * We're missing at least one of the two parts, and we
             * have an Interactor we can use to prompt for them, so
             * try it.
             */
            lp->prompts = new_prompts();
            lp->prompts->callback = local_proxy_opener_coroutine;
            lp->prompts->callback_ctx = lp;
            lp->prompts->to_server = true;
            lp->prompts->from_server = false;
            lp->prompts->name = dupstr("Local proxy authentication");
            if (flags & TELNET_CMD_MISSING_USERNAME) {
                lp->username_prompt_index = lp->prompts->n_prompts;
                add_prompt(lp->prompts, dupstr("Proxy username: "), true);
            } else {
                lp->username_prompt_index = -1;
            }
            if (flags & TELNET_CMD_MISSING_PASSWORD) {
                lp->password_prompt_index = lp->prompts->n_prompts;
                add_prompt(lp->prompts, dupstr("Proxy password: "), false);
            } else {
                lp->password_prompt_index = -1;
            }

            while (true) {
                SeatPromptResult spr = seat_get_userpass_input(
                    interactor_announce(&lp->interactor), lp->prompts);
                if (spr.kind == SPRK_OK) {
                    break;
                } else if (spr.kind == SPRK_USER_ABORT) {
                    local_proxy_opener_cleanup_interactor(lp);
                    plug_closing_user_abort(lp->plug);
                    /* That will have freed us, so we must just return
                     * without calling any crStop */
                    return;
                } else if (spr.kind == SPRK_SW_ABORT) {
                    local_proxy_opener_cleanup_interactor(lp);
                    { // WINSCP
                    char *err = spr_get_error_message(spr);
                    plug_closing_error(lp->plug, err);
                    sfree(err);
                    return; /* without crStop, as above */
                    } // WINSCP
                }
                crReturnV;
            }

            if (lp->username_prompt_index != -1) {
                conf_set_str(
                    lp->conf, CONF_proxy_username,
                    prompt_get_result_ref(
                        lp->prompts->prompts[lp->username_prompt_index]));
            }

            if (lp->password_prompt_index != -1) {
                conf_set_str(
                    lp->conf, CONF_proxy_password,
                    prompt_get_result_ref(
                        lp->prompts->prompts[lp->password_prompt_index]));
            }

            free_prompts(lp->prompts);
            lp->prompts = NULL;
        }

        /*
         * Now format the command a second time, with the results of
         * those prompts written into lp->conf.
         */
        lp->formatted_cmd = format_telnet_command(
            lp->addr, lp->port, lp->conf, NULL);
    }

    /*
     * Log the command, with some changes. Firstly, we regenerate it
     * with the password masked; secondly, we escape control
     * characters so that the log message is printable.
     */
    conf_set_str(lp->conf, CONF_proxy_password, "*password*");
    {
        char *censored_cmd = format_telnet_command(
            lp->addr, lp->port, lp->conf, NULL);

        strbuf *logmsg = strbuf_new();
        put_datapl(logmsg, PTRLEN_LITERAL("Starting local proxy command: "));
        put_c_string_literal(logmsg, ptrlen_from_asciz(censored_cmd));

        plug_log(lp->plug, lp->socket, PLUGLOG_PROXY_MSG, NULL, 0,
                 logmsg->s, 0);
        strbuf_free(logmsg);
        sfree(censored_cmd);
    }

    /*
     * Now we're ready to actually do the platform-specific socket
     * setup.
     */
    { // WINSCP
    char *cmd = lp->formatted_cmd;
    lp->formatted_cmd = NULL;

    local_proxy_opener_cleanup_interactor(lp);

    { // WINSCP
    char *error_msg = platform_setup_local_proxy(lp->socket, cmd);
    burnstr(cmd);

    if (error_msg) {
        plug_closing_error(lp->plug, error_msg);
        sfree(error_msg);
    } else {
        /* If error_msg was NULL, there was no error in setup,
         * which means that platform_setup_local_proxy will have
         * called back to free us. So return without calling any
         * crStop. */
        return;
    }

    } // WINSCP
    } // WINSCP
    crFinishV;
}

DeferredSocketOpener *local_proxy_opener(
    SockAddr *addr, int port, Plug *plug, Conf *conf, Interactor *itr)
{
    LocalProxyOpener *lp = snew(LocalProxyOpener);
    memset(lp, 0, sizeof(*lp));
    lp->plug = plug;
    lp->opener.vt = &LocalProxyOpener_openervt;
    lp->interactor.vt = &LocalProxyOpener_interactorvt;
    lp->addr = sk_addr_dup(addr);
    lp->port = port;
    lp->conf = conf_copy(conf);

    if (itr) {
        lp->clientitr = itr;
        interactor_set_child(lp->clientitr, &lp->interactor);
        lp->clientlp = interactor_logpolicy(lp->clientitr);
        lp->clientseat = interactor_borrow_seat(lp->clientitr);
    }

    return &lp->opener;
}

void local_proxy_opener_set_socket(DeferredSocketOpener *opener,
                                   Socket *socket)
{
    assert(opener->vt == &LocalProxyOpener_openervt);
    { // WINSCP
    LocalProxyOpener *lp = container_of(opener, LocalProxyOpener, opener);
    lp->socket = socket;
    queue_toplevel_callback(get_callback_set(lp->plug), local_proxy_opener_coroutine, lp);
    } // WINSCP
}
