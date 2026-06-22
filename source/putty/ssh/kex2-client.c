/*
 * Client side of key exchange for the SSH-2 transport protocol (RFC 4253).
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "bpp.h"
#include "ppl.h"
#include "sshcr.h"
#include "storage.h"
#include "transport2.h"
#include "mpint.h"

/*
 * Another copy of the symbol defined in mpunsafe.c. See the comment
 * there.
 */
const int deliberate_symbol_clash = 12345;

void ssh2kex_coroutine(struct ssh2_transport_state *s, bool *aborted)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    PktIn *pktin;
    PktOut *pktout;

    crBegin(s->crStateKex);

    if (s->kex_alg->main_type == KEXTYPE_DH) {
        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher ? s->out.cipher->real_keybits : 0;
            scbits = s->in.cipher ? s->in.cipher->real_keybits : 0;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > s->kex_alg->hash->hlen * 8)
            s->nbits = s->kex_alg->hash->hlen * 8;

        /*
         * If we're doing Diffie-Hellman group exchange, start by
         * requesting a group.
         */
        if (dh_is_gex(s->kex_alg)) {
            ppl_logevent("Doing Diffie-Hellman group exchange");
            s->ppl.bpp->pls->kctx = SSH2_PKTCTX_DHGEX;
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            if (s->pbits < DH_MIN_SIZE)
                s->pbits = DH_MIN_SIZE;
            if (s->pbits > DH_MAX_SIZE)
                s->pbits = DH_MAX_SIZE;
            if ((s->ppl.remote_bugs & BUG_SSH2_OLDGEX)) {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEX_DH_GEX_REQUEST_OLD);
                put_uint32(pktout, s->pbits);
            } else {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEX_DH_GEX_REQUEST);
                put_uint32(pktout, DH_MIN_SIZE);
                put_uint32(pktout, s->pbits);
                put_uint32(pktout, DH_MAX_SIZE);
            }
            pq_push(s->ppl.out_pq, pktout);

            crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_KEX_DH_GEX_GROUP) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                                "expecting Diffie-Hellman group, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                *aborted = true;
                return;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                ssh_proto_error(s->ppl.ssh,
                                "Unable to parse Diffie-Hellman group packet");
                *aborted = true;
                return;
            }
            s->dh_ctx = dh_setup_gex(s->p, s->g);
            s->kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
            s->kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;

            ppl_logevent("Doing Diffie-Hellman key exchange using %d-bit "
                         "modulus and hash %s with a server-supplied group",
                         dh_modulus_bit_size(s->dh_ctx),
                         ssh_hash_alg(s->exhash)->text_name);
        } else {
            s->ppl.bpp->pls->kctx = SSH2_PKTCTX_DHGROUP;
            s->dh_ctx = dh_setup_group(s->kex_alg);
            s->kex_init_value = SSH2_MSG_KEXDH_INIT;
            s->kex_reply_value = SSH2_MSG_KEXDH_REPLY;

            ppl_logevent("Doing Diffie-Hellman key exchange using %d-bit "
                         "modulus and hash %s with standard group \"%s\"",
                         dh_modulus_bit_size(s->dh_ctx),
                         ssh_hash_alg(s->exhash)->text_name,
                         s->kex_alg->groupname);
        }

        /*
         * Now generate and send e for Diffie-Hellman.
         */
        seat_set_busy_status(s->ppl.seat, BUSY_CPU);
        s->e = dh_create_e(s->dh_ctx);
        pktout = ssh_bpp_new_pktout(s->ppl.bpp, s->kex_init_value);
        put_mp_ssh2(pktout, s->e);
        pq_push(s->ppl.out_pq, pktout);

        seat_set_busy_status(s->ppl.seat, BUSY_WAITING);
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != s->kex_reply_value) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting Diffie-Hellman reply, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            *aborted = true;
            return;
        }
        seat_set_busy_status(s->ppl.seat, BUSY_CPU);
        s->hostkeydata = get_string(pktin);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);
        s->f = get_mp_ssh2(pktin);
        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh,
                            "Unable to parse Diffie-Hellman reply packet");
            *aborted = true;
            return;
        }

        {
            const char *err = dh_validate_f(s->dh_ctx, s->f);
            if (err) {
                ssh_proto_error(s->ppl.ssh, "Diffie-Hellman reply failed "
                                "validation: %s", err);
                *aborted = true;
                return;
            }
        }
        { // WINSCP
        mp_int *K = dh_find_K(s->dh_ctx, s->f);
        put_mp_ssh2(s->kex_shared_secret, K);
        mp_free(K);

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        seat_set_busy_status(s->ppl.seat, BUSY_NOT);

        put_stringpl(s->exhash, s->hostkeydata);
        if (dh_is_gex(s->kex_alg)) {
            if (!(s->ppl.remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(s->exhash, DH_MIN_SIZE);
            put_uint32(s->exhash, s->pbits);
            if (!(s->ppl.remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(s->exhash, DH_MAX_SIZE);
            put_mp_ssh2(s->exhash, s->p);
            put_mp_ssh2(s->exhash, s->g);
        }
        put_mp_ssh2(s->exhash, s->e);
        put_mp_ssh2(s->exhash, s->f);

        dh_cleanup(s->dh_ctx);
        s->dh_ctx = NULL;
        mp_free(s->f); s->f = NULL;
        if (dh_is_gex(s->kex_alg)) {
            mp_free(s->g); s->g = NULL;
            mp_free(s->p); s->p = NULL;
        }
        } // WINSCP
    } else if (s->kex_alg->main_type == KEXTYPE_ECDH) {
        char *desc = ecdh_keyalg_description(s->kex_alg);
        ppl_logevent("Doing %s, using hash %s", desc,
                     ssh_hash_alg(s->exhash)->text_name);
        sfree(desc);

        s->ppl.bpp->pls->kctx = s->kex_alg->ecdh_vt->packet_naming_ctx;

        s->ecdh_key = ecdh_key_new(s->kex_alg, false);

        pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEX_ECDH_INIT);
        {
            strbuf *pubpoint = strbuf_new();
            ecdh_key_getpublic(s->ecdh_key, BinarySink_UPCAST(pubpoint));
            put_stringsb(pktout, pubpoint);
        }

        pq_push(s->ppl.out_pq, pktout);

        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEX_ECDH_REPLY) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting ECDH reply, type %d (%s)", pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            *aborted = true;
            return;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(s->exhash, s->hostkeydata);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);

        {
            strbuf *pubpoint = strbuf_new();
            ecdh_key_getpublic(s->ecdh_key, BinarySink_UPCAST(pubpoint));
            put_string(s->exhash, pubpoint->u, pubpoint->len);
            strbuf_free(pubpoint);
        }

        {
            ptrlen keydata = get_string(pktin);
            put_stringpl(s->exhash, keydata);
            { // WINSCP
            bool ok = ecdh_key_getkey(s->ecdh_key, keydata,
                                      BinarySink_UPCAST(s->kex_shared_secret));
            if (!get_err(pktin) && !ok) {
                ssh_proto_error(s->ppl.ssh, "Received invalid elliptic curve "
                                "point in ECDH reply");
                *aborted = true;
                return;
            }
            } // WINSCP
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh, "Unable to parse ECDH reply packet");
            *aborted = true;
            return;
        }

        ecdh_key_free(s->ecdh_key);
        s->ecdh_key = NULL;
#ifndef NO_GSSAPI
    } else if (kex_is_gss(s->kex_alg)) {
        ptrlen data;

        s->ppl.bpp->pls->kctx = SSH2_PKTCTX_GSSKEX;
        s->init_token_sent = false;
        s->complete_rcvd = false;
        s->hkey = NULL;
        s->keystr = NULL;

        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         *
         * This is rote from the KEXTYPE_DH section above.
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher->real_keybits;
            scbits = s->in.cipher->real_keybits;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > s->kex_alg->hash->hlen * 8)
            s->nbits = s->kex_alg->hash->hlen * 8;

        assert(!s->ecdh_key);
        assert(!s->dh_ctx);

        if (s->kex_alg->main_type == KEXTYPE_GSS_ECDH) {
            s->ecdh_key = ecdh_key_new(s->kex_alg, false);

            { // WINSCP
            char *desc = ecdh_keyalg_description(s->kex_alg);
            ppl_logevent("Doing GSSAPI (with Kerberos V5) %s with hash %s",
                         desc, ssh_hash_alg(s->exhash)->text_name);
            sfree(desc);
            } // WINSCP
        } else if (dh_is_gex(s->kex_alg)) {
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            ppl_logevent("Doing GSSAPI (with Kerberos V5) Diffie-Hellman "
                         "group exchange, with minimum %d bits, and hash %s",
                         s->pbits, ssh_hash_alg(s->exhash)->text_name);
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXGSS_GROUPREQ);
            put_uint32(pktout, s->pbits); /* min */
            put_uint32(pktout, s->pbits); /* preferred */
            put_uint32(pktout, s->pbits * 2); /* max */
            pq_push(s->ppl.out_pq, pktout);

            crMaybeWaitUntilV(
                (pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_KEXGSS_GROUP) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                                "expecting Diffie-Hellman group, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                *aborted = true;
                return;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                ssh_proto_error(s->ppl.ssh,
                                "Unable to parse Diffie-Hellman group packet");
                *aborted = true;
                return;
            }
            s->dh_ctx = dh_setup_gex(s->p, s->g);
        } else {
            s->dh_ctx = dh_setup_group(s->kex_alg);
            ppl_logevent("Using GSSAPI (with Kerberos V5) Diffie-Hellman with"
                         " standard group \"%s\" and hash %s",
                         s->kex_alg->groupname,
                         ssh_hash_alg(s->exhash)->text_name);
        }

        /* Now generate e for Diffie-Hellman. */
        seat_set_busy_status(s->ppl.seat, BUSY_CPU);
        if (s->ecdh_key) {
            s->ebuf = strbuf_new_nm();
            ecdh_key_getpublic(s->ecdh_key, BinarySink_UPCAST(s->ebuf));
        } else {
            s->e = dh_create_e(s->dh_ctx);
        }

        if (s->shgss->lib->gsslogmsg)
            ppl_logevent("%s", s->shgss->lib->gsslogmsg);

        /* initial tokens are empty */
        SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);
        SSH_GSS_CLEAR_BUF(&s->gss_sndtok);
        SSH_GSS_CLEAR_BUF(&s->mic);
        s->gss_stat = s->shgss->lib->acquire_cred(
            s->shgss->lib, &s->shgss->ctx, &s->gss_cred_expiry);
        if (s->gss_stat != SSH_GSS_OK) {
            ssh_sw_abort(s->ppl.ssh,
                         "GSSAPI key exchange failed to initialise");
            *aborted = true;
            return;
        }

        /* now enter the loop */
        assert(s->shgss->srv_name);
        do {
            /*
             * When acquire_cred yields no useful expiration, go with the
             * service ticket expiration.
             */
            s->gss_stat = s->shgss->lib->init_sec_context(
                s->shgss->lib, &s->shgss->ctx, s->shgss->srv_name,
                s->gss_delegate, &s->gss_rcvtok, &s->gss_sndtok,
                (s->gss_cred_expiry == GSS_NO_EXPIRATION ?
                 &s->gss_cred_expiry : NULL), NULL);
            SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break; /* MIC is verified after the loop */

            if (s->gss_stat != SSH_GSS_S_COMPLETE &&
                s->gss_stat != SSH_GSS_S_CONTINUE_NEEDED) {
                if (s->shgss->lib->display_status(
                        s->shgss->lib, s->shgss->ctx,
                        &s->gss_buf) == SSH_GSS_OK) {
                    char *err = s->gss_buf.value;
                    ssh_sw_abort(s->ppl.ssh,
                                 "GSSAPI key exchange failed to initialise "
                                 "context: %s", err);
                    sfree(err);
                    *aborted = true;
                    return;
                }
            }
            assert(s->gss_stat == SSH_GSS_S_COMPLETE ||
                   s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED);

            if (!s->init_token_sent) {
                s->init_token_sent = true;
                pktout = ssh_bpp_new_pktout(s->ppl.bpp,
                                            SSH2_MSG_KEXGSS_INIT);
                if (s->gss_sndtok.length == 0) {
                    ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange failed: "
                                 "no initial context token");
                    *aborted = true;
                    return;
                }
                put_string(pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                if (s->ecdh_key) {
                    put_stringpl(pktout, ptrlen_from_strbuf(s->ebuf));
                } else {
                    put_mp_ssh2(pktout, s->e);
                }
                pq_push(s->ppl.out_pq, pktout);
                s->shgss->lib->free_tok(s->shgss->lib, &s->gss_sndtok);
                ppl_logevent("GSSAPI key exchange initialised");
            } else if (s->gss_sndtok.length != 0) {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEXGSS_CONTINUE);
                put_string(pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                pq_push(s->ppl.out_pq, pktout);
                s->shgss->lib->free_tok(s->shgss->lib, &s->gss_sndtok);
            }

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break;

          wait_for_gss_token:
            crMaybeWaitUntilV(
                (pktin = ssh2_transport_pop(s)) != NULL);
            switch (pktin->type) {
              case SSH2_MSG_KEXGSS_CONTINUE:
                data = get_string(pktin);
                s->gss_rcvtok.value = (char *)data.ptr;
                s->gss_rcvtok.length = data.len;
                continue;
              case SSH2_MSG_KEXGSS_COMPLETE:
                s->complete_rcvd = true;
                if (s->ecdh_key) {
                    s->fbuf = strbuf_dup_nm(get_string(pktin));
                } else {
                    s->f = get_mp_ssh2(pktin);
                }
                data = get_string(pktin);
                s->mic.value = (char *)data.ptr;
                s->mic.length = data.len;
                /* If there's a final token we loop to consume it */
                if (get_bool(pktin)) {
                    data = get_string(pktin);
                    s->gss_rcvtok.value = (char *)data.ptr;
                    s->gss_rcvtok.length = data.len;
                    continue;
                }
                break;
              case SSH2_MSG_KEXGSS_HOSTKEY:
                s->hostkeydata = get_string(pktin);
                if (s->hostkey_alg) {
                    s->hkey = ssh_key_new_pub(s->hostkey_alg,
                                              s->hostkeydata);
                    put_stringpl(s->exhash, s->hostkeydata);
                }
                /*
                 * Can't loop as we have no token to pass to
                 * init_sec_context.
                 */
                goto wait_for_gss_token;
              case SSH2_MSG_KEXGSS_ERROR:
                /*
                 * We have no use for the server's major and minor
                 * status.  The minor status is really only
                 * meaningful to the server, and with luck the major
                 * status means something to us (but not really all
                 * that much).  The string is more meaningful, and
                 * hopefully the server sends any error tokens, as
                 * that will produce the most useful information for
                 * us.
                 */
                get_uint32(pktin); /* server's major status */
                get_uint32(pktin); /* server's minor status */
                data = get_string(pktin);
                ppl_logevent("GSSAPI key exchange failed; "
                             "server's message: %.*s", PTRLEN_PRINTF(data));
                /* Language tag, but we have no use for it */
                get_string(pktin);
                /*
                 * Wait for an error token, if there is one, or the
                 * server's disconnect.  The error token, if there
                 * is one, must follow the SSH2_MSG_KEXGSS_ERROR
                 * message, per the RFC.
                 */
                goto wait_for_gss_token;
              default:
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet "
                                "during GSSAPI key exchange, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                *aborted = true;
                return;
            }
        } while (s->gss_rcvtok.length ||
                 s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED ||
                 !s->complete_rcvd);

        if (s->ecdh_key) {
            bool ok = ecdh_key_getkey(s->ecdh_key, ptrlen_from_strbuf(s->fbuf),
                                      BinarySink_UPCAST(s->kex_shared_secret));
            if (!ok) {
                ssh_proto_error(s->ppl.ssh, "Received invalid elliptic curve "
                                "point in GSSAPI ECDH reply");
                *aborted = true;
                return;
            }
        } else {
            const char *err = dh_validate_f(s->dh_ctx, s->f);
            if (err) {
                ssh_proto_error(s->ppl.ssh, "GSSAPI reply failed "
                                "validation: %s", err);
                *aborted = true;
                return;
            }
            { // WINSCP
            mp_int *K = dh_find_K(s->dh_ctx, s->f);
            put_mp_ssh2(s->kex_shared_secret, K);
            mp_free(K);
            } // WINSCP
        }

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        seat_set_busy_status(s->ppl.seat, BUSY_NOT);

        if (!s->hkey)
            put_stringz(s->exhash, "");

        if (s->ecdh_key) {
            put_stringpl(s->exhash, ptrlen_from_strbuf(s->ebuf));
            put_stringpl(s->exhash, ptrlen_from_strbuf(s->fbuf));
        } else {
            if (dh_is_gex(s->kex_alg)) {
                /* min, preferred, max */
                put_uint32(s->exhash, s->pbits);
                put_uint32(s->exhash, s->pbits);
                put_uint32(s->exhash, s->pbits * 2);

                put_mp_ssh2(s->exhash, s->p);
                put_mp_ssh2(s->exhash, s->g);
            }
            put_mp_ssh2(s->exhash, s->e);
            put_mp_ssh2(s->exhash, s->f);
        }

        /*
         * MIC verification is done below, after we compute the hash
         * used as the MIC input.
         */

        if (s->ecdh_key) {
            ecdh_key_free(s->ecdh_key);
            s->ecdh_key = NULL;
            strbuf_free(s->ebuf); s->ebuf = NULL;
            strbuf_free(s->fbuf); s->fbuf = NULL;
        } else {
            dh_cleanup(s->dh_ctx);
            s->dh_ctx = NULL;
            mp_free(s->f); s->f = NULL;
            if (dh_is_gex(s->kex_alg)) {
                mp_free(s->g); s->g = NULL;
                mp_free(s->p); s->p = NULL;
            }
        }
#endif
    } else {
        ptrlen rsakeydata;

        assert(s->kex_alg->main_type == KEXTYPE_RSA);
        ppl_logevent("Doing RSA key exchange with hash %s",
                     ssh_hash_alg(s->exhash)->text_name);
        s->ppl.bpp->pls->kctx = SSH2_PKTCTX_RSAKEX;
        /*
         * RSA key exchange. First expect a KEXRSA_PUBKEY packet
         * from the server.
         */
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_PUBKEY) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting RSA public key, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            *aborted = true;
            return;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(s->exhash, s->hostkeydata);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);

        rsakeydata = get_string(pktin);

        s->rsa_kex_key = ssh_rsakex_newkey(rsakeydata);
        if (!s->rsa_kex_key) {
            ssh_proto_error(s->ppl.ssh,
                            "Unable to parse RSA public key packet");
            *aborted = true;
            return;
        }
        s->rsa_kex_key_needs_freeing = true;

        put_stringpl(s->exhash, rsakeydata);

        /*
         * Next, set up a shared secret K, of precisely KLEN -
         * 2*HLEN - 49 bits, where KLEN is the bit length of the
         * RSA key modulus and HLEN is the bit length of the hash
         * we're using.
         */
        {
            int klen = ssh_rsakex_klen(s->rsa_kex_key);

            const struct ssh_rsa_kex_extra *extra =
                (const struct ssh_rsa_kex_extra *)s->kex_alg->extra;
            if (klen < extra->minklen) {
                ssh_proto_error(s->ppl.ssh, "Server sent %d-bit RSA key, "
                                "less than the minimum size %d for %s "
                                "key exchange", klen, extra->minklen,
                                s->kex_alg->name);
                *aborted = true;
                return;
            }

            { // WINSCP
            int nbits = klen - (2*s->kex_alg->hash->hlen*8 + 49);
            assert(nbits > 0);

            { // WINSCP
            strbuf *buf, *outstr;

            mp_int *tmp = mp_random_bits(nbits - 1);
            mp_int *K = mp_power_2(nbits - 1);
            mp_add_into(K, K, tmp);
            mp_free(tmp);

            /*
             * Encode this as an mpint.
             */
            buf = strbuf_new_nm();
            put_mp_ssh2(buf, K);

            /*
             * Store a copy as the output shared secret from the kex.
             */
            put_mp_ssh2(s->kex_shared_secret, K);
            mp_free(K);

            /*
             * Encrypt it with the given RSA key.
             */
            outstr = ssh_rsakex_encrypt(s->rsa_kex_key, s->kex_alg->hash,
                                        ptrlen_from_strbuf(buf));

            /*
             * And send it off in a return packet.
             */
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXRSA_SECRET);
            put_stringpl(pktout, ptrlen_from_strbuf(outstr));
            pq_push(s->ppl.out_pq, pktout);

            put_stringsb(s->exhash, outstr); /* frees outstr */

            strbuf_free(buf);
            } // WINSCP
            } // WINSCP
        }

        ssh_rsakex_freekey(s->rsa_kex_key);
        s->rsa_kex_key = NULL;
        s->rsa_kex_key_needs_freeing = false;

        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_DONE) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting RSA kex signature, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            *aborted = true;
            return;
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh, "Unable to parse RSA kex signature");
            *aborted = true;
            return;
        }
    }

    ssh2transport_finalise_exhash(s);

#ifndef NO_GSSAPI
    if (kex_is_gss(s->kex_alg)) {
        Ssh_gss_buf gss_buf;
        SSH_GSS_CLEAR_BUF(&s->gss_buf);

        gss_buf.value = s->exchange_hash;
        gss_buf.length = s->kex_alg->hash->hlen;
        s->gss_stat = s->shgss->lib->verify_mic(
            s->shgss->lib, s->shgss->ctx, &gss_buf, &s->mic);
        if (s->gss_stat != SSH_GSS_OK) {
            if (s->shgss->lib->display_status(
                    s->shgss->lib, s->shgss->ctx, &s->gss_buf) == SSH_GSS_OK) {
                char *err = s->gss_buf.value;
                ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange MIC was "
                             "not valid: %s", err);
                sfree(err);
            } else {
                ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange MIC was "
                             "not valid");
            }
            *aborted = true;
            return;
        }

        s->gss_kex_used = true;

        /*-
         * If this the first KEX, save the GSS context for "gssapi-keyex"
         * authentication.
         *
         * https://www.rfc-editor.org/rfc/rfc4462#section-4
         *
         * This method may be used only if the initial key exchange was
         * performed using a GSS-API-based key exchange method defined in
         * accordance with Section 2.  The GSS-API context used with this
         * method is always that established during an initial GSS-API-based
         * key exchange.  Any context established during key exchange for the
         * purpose of rekeying MUST NOT be used with this method.
         */
        if (s->got_session_id) {
            s->shgss->lib->release_cred(s->shgss->lib, &s->shgss->ctx);
        }
        ppl_logevent("GSSAPI Key Exchange complete!");
    }
#endif

    s->dh_ctx = NULL;

    /* In GSS keyex there's no hostkey signature to verify */
    if (!kex_is_gss(s->kex_alg)) {
        if (!s->hkey) {
            ssh_proto_error(s->ppl.ssh, "Server's host key is invalid");
            *aborted = true;
            return;
        }

        if (!ssh_key_verify(
                s->hkey, s->sigdata,
                make_ptrlen(s->exchange_hash, s->kex_alg->hash->hlen))) {
#ifndef FUZZING
            ssh_proto_error(s->ppl.ssh, "Signature from server's host key "
                            "is invalid");
            *aborted = true;
            return;
#endif
        }
    }

    s->keystr = s->hkey ? ssh_key_cache_str(s->hkey) : NULL;
#ifndef NO_GSSAPI
    if (s->gss_kex_used) {
        /*
         * In a GSS-based session, check the host key (if any) against
         * the transient host key cache.
         */
        if (kex_is_gss(s->kex_alg)) {

            /*
             * We've just done a GSS key exchange. If it gave us a
             * host key, store it.
             */
            if (s->hkey) {
                char *fingerprint = ssh2_double_fingerprint(
                    s->hkey, SSH_FPTYPE_DEFAULT);
                ppl_logevent("GSS kex provided fallback host key:");
                ppl_logevent("%s", fingerprint);
                sfree(fingerprint);

                ssh_transient_hostkey_cache_add(s->thc, s->hkey);
            } else if (!ssh_transient_hostkey_cache_non_empty(s->thc)) {
                /*
                 * But if it didn't, then we currently have no
                 * fallback host key to use in subsequent non-GSS
                 * rekeys. So we should immediately trigger a non-GSS
                 * rekey of our own, to set one up, before the session
                 * keys have been used for anything else.
                 *
                 * This is similar to the cross-certification done at
                 * user request in the permanent host key cache, but
                 * here we do it automatically, once, at session
                 * startup, and only add the key to the transient
                 * cache.
                 */
                if (s->hostkey_alg) {
                    s->need_gss_transient_hostkey = true;
                } else {
                    /*
                     * If we negotiated the "null" host key algorithm
                     * in the key exchange, that's an indication that
                     * no host key at all is available from the server
                     * (both because we listed "null" last, and
                     * because RFC 4462 section 5 says that a server
                     * MUST NOT offer "null" as a host key algorithm
                     * unless that is the only algorithm it provides
                     * at all).
                     *
                     * In that case we actually _can't_ perform a
                     * non-GSSAPI key exchange, so it's pointless to
                     * attempt one proactively. This is also likely to
                     * cause trouble later if a rekey is required at a
                     * moment whne GSS credentials are not available,
                     * but someone setting up a server in this
                     * configuration presumably accepts that as a
                     * consequence.
                     */
                    if (!s->warned_about_no_gss_transient_hostkey) {
                        ppl_logevent("No fallback host key available");
                        s->warned_about_no_gss_transient_hostkey = true;
                    }
                }
            }
        } else {
            /*
             * We've just done a fallback key exchange, so make
             * sure the host key it used is in the cache of keys
             * we previously received in GSS kexes.
             *
             * An exception is if this was the non-GSS key exchange we
             * triggered on purpose to populate the transient cache.
             */
            assert(s->hkey);  /* only KEXTYPE_GSS* lets this be null */
            { // WINSCP
            char *fingerprint = ssh2_double_fingerprint(
                s->hkey, SSH_FPTYPE_DEFAULT);

            if (s->need_gss_transient_hostkey) {
                ppl_logevent("Post-GSS rekey provided fallback host key:");
                ppl_logevent("%s", fingerprint);
                ssh_transient_hostkey_cache_add(s->thc, s->hkey);
                s->need_gss_transient_hostkey = false;
            } else if (!ssh_transient_hostkey_cache_verify(s->thc, s->hkey)) {
                ppl_logevent("Non-GSS rekey after initial GSS kex "
                             "used host key:");
                ppl_logevent("%s", fingerprint);
                sfree(fingerprint);
                ssh_sw_abort(s->ppl.ssh, "Server's host key did not match any "
                             "used in previous GSS kex");
                *aborted = true;
                return;
            }

            sfree(fingerprint);
            } // WINSCP
        }
    } else
#endif /* NO_GSSAPI */
        if (!s->got_session_id) {
            /*
             * Make a note of any other host key formats that are available.
             */
            {
                int i, j, nkeys = 0;
                char *list = NULL;
                for (i = 0; i < lenof(ssh2_hostkey_algs); i++) {
                    if (ssh2_hostkey_algs[i].alg == s->hostkey_alg)
                        continue;

                    for (j = 0; j < s->n_uncert_hostkeys; j++)
                        if (s->uncert_hostkeys[j] == i)
                            break;

                    if (j < s->n_uncert_hostkeys) {
                        char *newlist;
                        if (list)
                            newlist = dupprintf(
                                "%s/%s", list,
                                ssh2_hostkey_algs[i].alg->ssh_id);
                        else
                            newlist = dupprintf(
                                "%s", ssh2_hostkey_algs[i].alg->ssh_id);
                        sfree(list);
                        list = newlist;
                        nkeys++;
                    }
                }
                if (list) {
                    ppl_logevent("Server also has %s host key%s, but we "
                                 "don't know %s", list,
                                 nkeys > 1 ? "s" : "",
                                 nkeys > 1 ? "any of them" : "it");
                    sfree(list);
                }
            }

            { // WINSCP
            ssh2_userkey uk; // WINSCP
            uk.key = s->hkey; // WINSCP
            uk.comment = NULL; // WINSCP
            { // WINSCP
            char **fingerprints = ssh2_all_fingerprints(s->hkey);

            FingerprintType fptype_default =
                ssh2_pick_default_fingerprint(fingerprints);
            ppl_logevent("Host key fingerprint is:");
            ppl_logevent("%s", fingerprints[fptype_default]);

            /*
             * Authenticate remote host: verify host key, either by
             * certification or by the local host key cache.
             *
             * (We've already checked the signature of the exchange
             * hash.)
             */
            if (ssh_key_alg(s->hkey)->is_certificate) {
                char *base_fp = ssh2_fingerprint(
                    s->hkey, ssh_fptype_to_cert(fptype_default));
                ppl_logevent("Host key is a certificate. "
                             "Hash including certificate:");
                ppl_logevent("%s", base_fp);
                sfree(base_fp);

                { // WINSCP
                strbuf *id_string = strbuf_new();
                #ifndef WINSCP
                StripCtrlChars *id_string_scc = stripctrl_new(
                    BinarySink_UPCAST(id_string), false, L'\0');
                #endif
                ssh_key_cert_id_string(
                    s->hkey, BinarySink_UPCAST(id_string/*WINSCP _scc*/));
                #ifndef WINSCP
                stripctrl_free(id_string_scc);
                #endif
                ppl_logevent("Certificate ID string is \"%s\"", id_string->s);
                strbuf_free(id_string);

                { // WINSCP
                strbuf *ca_pub = strbuf_new();
                ssh_key_ca_public_blob(s->hkey, BinarySink_UPCAST(ca_pub));
                { // WINSCP
                host_ca hca_search;
                smemclr(&hca_search, sizeof(hca_search));
                hca_search.ca_public_key = ca_pub;
                { // WINSCP
                host_ca *hca_found = find234(s->host_cas, &hca_search, NULL);

                char *ca_fp = ssh2_fingerprint_blob(ptrlen_from_strbuf(ca_pub),
                                                    fptype_default);
                ppl_logevent("Fingerprint of certification authority:");
                ppl_logevent("%s", ca_fp);
                sfree(ca_fp);

                strbuf_free(ca_pub);

                { // WINSCP
                strbuf *error = strbuf_new();
                bool cert_ok = false;

                if (!hca_found) {
                    put_fmt(error, "Certification authority is not trusted");
                } else {
                    ppl_logevent("Certification authority matches '%s'",
                                 hca_found->name);
                    cert_ok = ssh_key_check_cert(
                        s->hkey,
                        true, /* host certificate */
                        ptrlen_from_asciz(s->savedhost),
                        time(NULL),
                        &hca_found->opts,
                        BinarySink_UPCAST(error));
                }
                if (cert_ok) {
                    strbuf_free(error);
                    // WINSCP (does not "verify", only informs about the hostkeys)
                    seat_confirm_ssh_host_key(
                        ppl_get_iseat(&s->ppl), s->savedhost, s->savedport, ssh_key_cache_id(s->hkey), s->keystr, NULL, NULL, NULL, NULL,
                        fingerprints, true, 0, true);
                    ssh2_free_all_fingerprints(fingerprints);
                    ppl_logevent("Accepted certificate");
                    goto host_key_ok;
                } else {
                    ppl_logevent("Rejected host key certificate: %s",
                                 error->s);
                    strbuf_free(error);
                    /* now fall through into normal host key checking */
                }
                } // WINSCP
                } // WINSCP
                } // WINSCP
                } // WINSCP
                } // WINSCP
            }

            {
                { // WINSCP
                char *keydisp = ssh2_pubkey_openssh_str(&uk);

                int ca_count = ssh_key_alg(s->hkey)->is_certificate ?
                    count234(s->host_cas) : 0;

                s->spr = verify_ssh_host_key(
                    ppl_get_iseat(&s->ppl), s->conf, s->savedhost, s->savedport,
                    s->hkey, ssh_key_cache_id(s->hkey), s->keystr, keydisp,
                    fingerprints, ca_count, ssh2_transport_dialog_callback, s);

                ssh2_free_all_fingerprints(fingerprints);
                sfree(keydisp);
                } // WINSCP
#ifdef FUZZING
                s->spr = SPR_OK;
#endif
                crMaybeWaitUntilV(s->spr.kind != SPRK_INCOMPLETE);
                if (spr_is_abort(s->spr)) {
                    *aborted = true;
                    ssh_spr_close(s->ppl.ssh, s->spr, "host key verification");
                    return;
                }

                if (ssh_key_alg(s->hkey)->is_certificate) {
                    /*
                     * Explain what's going on in the Event Log: if we
                     * got here by way of a certified key whose
                     * certificate we didn't like, then we should
                     * explain why we chose to continue with the
                     * connection anyway!
                     */
                    ppl_logevent("Accepting certified host key anyway based "
                                 "on cache");
                }
            }

          host_key_ok:

            /*
             * Save this host key, to check against the one presented in
             * subsequent rekeys.
             */
            strbuf_clear(s->hostkeyblob);
            ssh_key_public_blob(s->hkey, BinarySink_UPCAST(s->hostkeyblob));
            } // WINSCP
            } // WINSCP
        } else if (s->cross_certifying) {
            assert(s->hkey);
            assert(ssh_key_alg(s->hkey) == s->cross_certifying);

            { // WINSCP
            char *fingerprint = ssh2_double_fingerprint(
                s->hkey, SSH_FPTYPE_DEFAULT);
            ppl_logevent("Storing additional host key for this host:");
            ppl_logevent("%s", fingerprint);
            sfree(fingerprint);

            store_host_key(s->ppl.seat, s->savedhost, s->savedport,
                           ssh_key_cache_id(s->hkey), s->keystr);
            /*
             * Don't forget to store the new key as the one we'll be
             * re-checking in future normal rekeys.
             */
            strbuf_clear(s->hostkeyblob);
            ssh_key_public_blob(s->hkey, BinarySink_UPCAST(s->hostkeyblob));
            } // WINSCP
        } else {
            /*
             * In a rekey, we never present an interactive host key
             * verification request to the user. Instead, we simply
             * enforce that the key we're seeing this time is identical to
             * the one we saw before.
             */
            strbuf *thisblob = strbuf_new();
            ssh_key_public_blob(s->hkey, BinarySink_UPCAST(thisblob));
            { // WINSCP
            bool match = ptrlen_eq_ptrlen(ptrlen_from_strbuf(thisblob),
                                          ptrlen_from_strbuf(s->hostkeyblob));
            strbuf_free(thisblob);
            if (!match) {
#ifndef FUZZING
                ssh_sw_abort(s->ppl.ssh,
                             "Host key was different in repeat key exchange");
                *aborted = true;
                return;
#endif
            }
            } // WINSCP
        }

    sfree(s->keystr);
    s->keystr = NULL;
    if (s->hkey) {
        ssh_key_free(s->hkey);
        s->hkey = NULL;
    }

    crFinishV;
}
