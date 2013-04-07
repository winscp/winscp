/*
 * Routines to do cryptographic interaction with proxies in PuTTY.
 * This is in a separate module from proxy.c, so that it can be
 * conveniently removed in PuTTYtel by replacing this module with
 * the stub version nocproxy.c.
 */

#include <assert.h>
#include <ctype.h>
#include <string.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "putty.h"
#include "ssh.h" /* For MD5 support */
#include "network.h"
#include "proxy.h"

static void hmacmd5_chap(const unsigned char *challenge, int challen,
			 const char *passwd, unsigned char *response)
{
    void *hmacmd5_ctx;
    int pwlen;

    hmacmd5_ctx = hmacmd5_make_context();

    pwlen = strlen(passwd);
    if (pwlen>64) {
	unsigned char md5buf[16];
	MD5Simple(passwd, pwlen, md5buf);
	hmacmd5_key(hmacmd5_ctx, md5buf, 16);
    } else {
	hmacmd5_key(hmacmd5_ctx, passwd, pwlen);
    }

    hmacmd5_do_hmac(hmacmd5_ctx, challenge, challen, response);
    hmacmd5_free_context(hmacmd5_ctx);
}

void proxy_socks5_offerencryptedauth(char *command, int *len)
{
    command[*len] = 0x03; /* CHAP */
    (*len)++;
}

int proxy_socks5_handlechap (Proxy_Socket p)
{

    /* CHAP authentication reply format:
     *  version number (1 bytes) = 1
     *  number of commands (1 byte)
     *
     * For each command:
     *  command identifier (1 byte)
     *  data length (1 byte)
     */
    unsigned char data[260];
    unsigned char outbuf[20];

    while(p->chap_num_attributes == 0 ||
	  p->chap_num_attributes_processed < p->chap_num_attributes) {
	if (p->chap_num_attributes == 0 ||
	    p->chap_current_attribute == -1) {
	    /* CHAP normally reads in two bytes, either at the
	     * beginning or for each attribute/value pair.  But if
	     * we're waiting for the value's data, we might not want
	     * to read 2 bytes.
	     */
 
	    if (bufchain_size(&p->pending_input_data) < 2)
		return 1;	       /* not got anything yet */

	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data, 2);
	    bufchain_consume(&p->pending_input_data, 2);
	}

	if (p->chap_num_attributes == 0) {
	    /* If there are no attributes, this is our first msg
	     * with the server, where we negotiate version and 
	     * number of attributes
	     */
	    if (data[0] != 0x01) {
		plug_closing(p->plug, "Proxy error: SOCKS proxy wants"
			     " a different CHAP version",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }
	    if (data[1] == 0x00) {
		plug_closing(p->plug, "Proxy error: SOCKS proxy won't"
			     " negotiate CHAP with us",
			     PROXY_ERROR_GENERAL, 0);
		return 1;
	    }
	    p->chap_num_attributes = data[1];
	} else {
	    if (p->chap_current_attribute == -1) {
		/* We have to read in each attribute/value pair -
		 * those we don't understand can be ignored, but
		 * there are a few we'll need to handle.
		 */
		p->chap_current_attribute = data[0];
		p->chap_current_datalen = data[1];
	    }
	    if (bufchain_size(&p->pending_input_data) <
		p->chap_current_datalen)
		return 1;	       /* not got everything yet */

	    /* get the response */
	    bufchain_fetch(&p->pending_input_data, data,
			   p->chap_current_datalen);

	    bufchain_consume(&p->pending_input_data,
			     p->chap_current_datalen);

	    switch (p->chap_current_attribute) {
	      case 0x00:
		/* Successful authentication */
		if (data[0] == 0x00)
		    p->state = 2;
		else {
		    plug_closing(p->plug, "Proxy error: SOCKS proxy"
				 " refused CHAP authentication",
				 PROXY_ERROR_GENERAL, 0);
		    return 1;
		}
	      break;
	      case 0x03:
		outbuf[0] = 0x01; /* Version */
		outbuf[1] = 0x01; /* One attribute */
		outbuf[2] = 0x04; /* Response */
		outbuf[3] = 0x10; /* Length */
		hmacmd5_chap(data, p->chap_current_datalen,
			     p->cfg.proxy_password, &outbuf[4]);
		sk_write(p->sub_socket, (char *)outbuf, 20);
	      break;
	      case 0x11:
	        /* Chose a protocol */
		if (data[0] != 0x85) {
		    plug_closing(p->plug, "Proxy error: Server chose "
				 "CHAP of other than HMAC-MD5 but we "
				 "didn't offer it!",
				 PROXY_ERROR_GENERAL, 0);
		    return 1;
		}
	      break;
	    }
	    p->chap_current_attribute = -1;
	    p->chap_num_attributes_processed++;
	}
	if (p->state == 8 &&
	    p->chap_num_attributes_processed >= p->chap_num_attributes) {
	    p->chap_num_attributes = 0;
	    p->chap_num_attributes_processed = 0;
	    p->chap_current_datalen = 0;
	}
    }
    return 0;
}

int proxy_socks5_selectchap(Proxy_Socket p)
{
    if (p->cfg.proxy_username[0] || p->cfg.proxy_password[0]) {
	char chapbuf[514];
	int ulen;
	chapbuf[0] = '\x01'; /* Version */
	chapbuf[1] = '\x02'; /* Number of attributes sent */
	chapbuf[2] = '\x11'; /* First attribute - algorithms list */
	chapbuf[3] = '\x01'; /* Only one CHAP algorithm */
	chapbuf[4] = '\x85'; /* ...and it's HMAC-MD5, the core one */
	chapbuf[5] = '\x02'; /* Second attribute - username */

	ulen = strlen(p->cfg.proxy_username);
	if (ulen > 255) ulen = 255; if (ulen < 1) ulen = 1;

	chapbuf[6] = ulen;
	memcpy(chapbuf+7, p->cfg.proxy_username, ulen);

	sk_write(p->sub_socket, chapbuf, ulen + 7);
	p->chap_num_attributes = 0;
	p->chap_num_attributes_processed = 0;
	p->chap_current_attribute = -1;
	p->chap_current_datalen = 0;

	p->state = 8;
    } else 
	plug_closing(p->plug, "Proxy error: Server chose "
		     "CHAP authentication but we didn't offer it!",
		 PROXY_ERROR_GENERAL, 0);
    return 1;
}
