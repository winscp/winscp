/*
 * settings.c: read and write saved sessions.
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "putty.h"
#include "storage.h"

/*
 * Tables of string <-> enum value mappings
 */
struct keyval { char *s; int v; };

static const struct keyval ciphernames[] = {
    { "aes",	    CIPHER_AES },
    { "blowfish",   CIPHER_BLOWFISH },
    { "3des",	    CIPHER_3DES },
    { "WARN",	    CIPHER_WARN },
    { "des",	    CIPHER_DES }
};

static void gpps(void *handle, char *name, char *def, char *val, int len)
{
    if (!read_setting_s(handle, name, val, len)) {
	strncpy(val, def, len);
	val[len - 1] = '\0';
    }
}

static void gppi(void *handle, char *name, int def, int *i)
{
    *i = read_setting_i(handle, name, def);
}

static int key2val(const struct keyval *mapping, int nmaps, char *key)
{
    int i;
    for (i = 0; i < nmaps; i++)
	if (!strcmp(mapping[i].s, key)) return mapping[i].v;
    return -1;
}

static const char *val2key(const struct keyval *mapping, int nmaps, int val)
{
    int i;
    for (i = 0; i < nmaps; i++)
	if (mapping[i].v == val) return mapping[i].s;
    return NULL;
}

/*
 * Helper function to parse a comma-separated list of strings into
 * a preference list array of values. Any missing values are added
 * to the end and duplicates are weeded.
 * XXX: assumes vals in 'mapping' are small +ve integers
 */
static void gprefs(void *sesskey, char *name, char *def,
		   const struct keyval *mapping, int nvals,
		   int *array)
{
    char commalist[80];
    int n;
    unsigned long seen = 0;	       /* bitmap for weeding dups etc */
    gpps(sesskey, name, def, commalist, sizeof(commalist));

    /* Grotty parsing of commalist. */
    n = 0;
    do {
	int v;
	char *key;
	key = strtok(n==0 ? commalist : NULL, ","); /* sorry */
	if (!key) break;
	if (((v = key2val(mapping, nvals, key)) != -1) &&
	    !(seen & 1<<v)) {
	    array[n] = v;
	    n++;
	    seen |= 1<<v;
	}
    } while (n < nvals);
    /* Add any missing values (backward compatibility ect). */
    {
	int i;
	for (i = 0; i < nvals; i++) {
	    if (!(seen & 1<<mapping[i].v)) {
		array[n] = mapping[i].v;
		n++;
	    }
	}
    }
}

/* 
 * Write out a preference list.
 */
static void wprefs(void *sesskey, char *name,
		   const struct keyval *mapping, int nvals,
		   int *array)
{
    char buf[80] = "";	/* XXX assumed big enough */
    int l = sizeof(buf)-1, i;
    buf[l] = '\0';
    for (i = 0; l > 0 && i < nvals; i++) {
	const char *s = val2key(mapping, nvals, array[i]);
	if (s) {
	    int sl = strlen(s);
	    if (i > 0) {
		strncat(buf, ",", l);
		l--;
	    }
	    strncat(buf, s, l);
	    l -= sl;
	}
    }
    write_setting_s(sesskey, name, buf);
}

void save_settings(char *section, int do_host, Config * cfg)
{
    int i;
    char *p;
    void *sesskey;

    sesskey = open_settings_w(section);
    if (!sesskey)
	return;

    write_setting_i(sesskey, "Present", 1);
    if (do_host) {
	write_setting_s(sesskey, "HostName", cfg->host);
	write_setting_s(sesskey, "LogFileName", cfg->logfilename);
	write_setting_i(sesskey, "LogType", cfg->logtype);
	write_setting_i(sesskey, "LogFileClash", cfg->logxfovr);
    }
    p = "raw";
    for (i = 0; backends[i].name != NULL; i++)
	if (backends[i].protocol == cfg->protocol) {
	    p = backends[i].name;
	    break;
	}
    write_setting_s(sesskey, "Protocol", p);
    write_setting_i(sesskey, "PortNumber", cfg->port);
    write_setting_i(sesskey, "CloseOnExit", cfg->close_on_exit);
    write_setting_i(sesskey, "WarnOnClose", !!cfg->warn_on_close);
    write_setting_i(sesskey, "PingInterval", cfg->ping_interval / 60);	/* minutes */
    write_setting_i(sesskey, "PingIntervalSecs", cfg->ping_interval % 60);	/* seconds */
    write_setting_i(sesskey, "TCPNoDelay", cfg->tcp_nodelay);
    write_setting_s(sesskey, "TerminalType", cfg->termtype);
    write_setting_s(sesskey, "TerminalSpeed", cfg->termspeed);

    /* proxy settings */
    write_setting_s(sesskey, "ProxyExcludeList", cfg->proxy_exclude_list);
    write_setting_i(sesskey, "ProxyType", cfg->proxy_type);
    write_setting_s(sesskey, "ProxyHost", cfg->proxy_host);
    write_setting_i(sesskey, "ProxyPort", cfg->proxy_port);
    write_setting_s(sesskey, "ProxyUsername", cfg->proxy_username);
    write_setting_s(sesskey, "ProxyPassword", cfg->proxy_password);
    write_setting_s(sesskey, "ProxyTelnetCommand", cfg->proxy_telnet_command);
    write_setting_i(sesskey, "ProxySOCKSVersion", cfg->proxy_socks_version);

    {
	char buf[2 * sizeof(cfg->environmt)], *p, *q;
	p = buf;
	q = cfg->environmt;
	while (*q) {
	    while (*q) {
		int c = *q++;
		if (c == '=' || c == ',' || c == '\\')
		    *p++ = '\\';
		if (c == '\t')
		    c = '=';
		*p++ = c;
	    }
	    *p++ = ',';
	    q++;
	}
	*p = '\0';
	write_setting_s(sesskey, "Environment", buf);
    }
    write_setting_s(sesskey, "UserName", cfg->username);
    write_setting_s(sesskey, "LocalUserName", cfg->localusername);
    write_setting_i(sesskey, "NoPTY", cfg->nopty);
    write_setting_i(sesskey, "Compression", cfg->compression);
    write_setting_i(sesskey, "AgentFwd", cfg->agentfwd);
    write_setting_i(sesskey, "ChangeUsername", cfg->change_username);
    wprefs(sesskey, "Cipher", ciphernames, CIPHER_MAX,
	   cfg->ssh_cipherlist);
    write_setting_i(sesskey, "AuthTIS", cfg->try_tis_auth);
    write_setting_i(sesskey, "AuthKI", cfg->try_ki_auth);
    write_setting_i(sesskey, "SshProt", cfg->sshprot);
    write_setting_i(sesskey, "SSH2DES", cfg->ssh2_des_cbc);
    write_setting_s(sesskey, "PublicKeyFile", cfg->keyfile);
    write_setting_s(sesskey, "RemoteCommand", cfg->remote_cmd);
    write_setting_i(sesskey, "RFCEnviron", cfg->rfc_environ);
    write_setting_i(sesskey, "PassiveTelnet", cfg->passive_telnet);
    write_setting_i(sesskey, "BackspaceIsDelete", cfg->bksp_is_delete);
    write_setting_i(sesskey, "RXVTHomeEnd", cfg->rxvt_homeend);
    write_setting_i(sesskey, "LinuxFunctionKeys", cfg->funky_type);
    write_setting_i(sesskey, "NoApplicationKeys", cfg->no_applic_k);
    write_setting_i(sesskey, "NoApplicationCursors", cfg->no_applic_c);
    write_setting_i(sesskey, "NoMouseReporting", cfg->no_mouse_rep);
    write_setting_i(sesskey, "NoRemoteResize", cfg->no_remote_resize);
    write_setting_i(sesskey, "NoAltScreen", cfg->no_alt_screen);
    write_setting_i(sesskey, "NoRemoteWinTitle", cfg->no_remote_wintitle);
    write_setting_i(sesskey, "NoDBackspace", cfg->no_dbackspace);
    write_setting_i(sesskey, "NoRemoteCharset", cfg->no_remote_charset);
    write_setting_i(sesskey, "ApplicationCursorKeys", cfg->app_cursor);
    write_setting_i(sesskey, "ApplicationKeypad", cfg->app_keypad);
    write_setting_i(sesskey, "NetHackKeypad", cfg->nethack_keypad);
    write_setting_i(sesskey, "AltF4", cfg->alt_f4);
    write_setting_i(sesskey, "AltSpace", cfg->alt_space);
    write_setting_i(sesskey, "AltOnly", cfg->alt_only);
    write_setting_i(sesskey, "ComposeKey", cfg->compose_key);
    write_setting_i(sesskey, "CtrlAltKeys", cfg->ctrlaltkeys);
    write_setting_i(sesskey, "TelnetKey", cfg->telnet_keyboard);
    write_setting_i(sesskey, "TelnetRet", cfg->telnet_newline);
    write_setting_i(sesskey, "LocalEcho", cfg->localecho);
    write_setting_i(sesskey, "LocalEdit", cfg->localedit);
    write_setting_s(sesskey, "Answerback", cfg->answerback);
    write_setting_i(sesskey, "AlwaysOnTop", cfg->alwaysontop);
    write_setting_i(sesskey, "FullScreenOnAltEnter", cfg->fullscreenonaltenter);
    write_setting_i(sesskey, "HideMousePtr", cfg->hide_mouseptr);
    write_setting_i(sesskey, "SunkenEdge", cfg->sunken_edge);
    write_setting_i(sesskey, "WindowBorder", cfg->window_border);
    write_setting_i(sesskey, "CurType", cfg->cursor_type);
    write_setting_i(sesskey, "BlinkCur", cfg->blink_cur);
    write_setting_i(sesskey, "Beep", cfg->beep);
    write_setting_i(sesskey, "BeepInd", cfg->beep_ind);
    write_setting_s(sesskey, "BellWaveFile", cfg->bell_wavefile);
    write_setting_i(sesskey, "BellOverload", cfg->bellovl);
    write_setting_i(sesskey, "BellOverloadN", cfg->bellovl_n);
    write_setting_i(sesskey, "BellOverloadT", cfg->bellovl_t);
    write_setting_i(sesskey, "BellOverloadS", cfg->bellovl_s);
    write_setting_i(sesskey, "ScrollbackLines", cfg->savelines);
    write_setting_i(sesskey, "DECOriginMode", cfg->dec_om);
    write_setting_i(sesskey, "AutoWrapMode", cfg->wrap_mode);
    write_setting_i(sesskey, "LFImpliesCR", cfg->lfhascr);
    write_setting_i(sesskey, "WinNameAlways", cfg->win_name_always);
    write_setting_s(sesskey, "WinTitle", cfg->wintitle);
    write_setting_i(sesskey, "TermWidth", cfg->width);
    write_setting_i(sesskey, "TermHeight", cfg->height);
    write_setting_s(sesskey, "Font", cfg->font);
    write_setting_i(sesskey, "FontIsBold", cfg->fontisbold);
    write_setting_i(sesskey, "FontCharSet", cfg->fontcharset);
    write_setting_i(sesskey, "FontHeight", cfg->fontheight);
    write_setting_i(sesskey, "FontVTMode", cfg->vtmode);
    write_setting_i(sesskey, "TryPalette", cfg->try_palette);
    write_setting_i(sesskey, "BoldAsColour", cfg->bold_colour);
    for (i = 0; i < 22; i++) {
	char buf[20], buf2[30];
	sprintf(buf, "Colour%d", i);
	sprintf(buf2, "%d,%d,%d", cfg->colours[i][0],
		cfg->colours[i][1], cfg->colours[i][2]);
	write_setting_s(sesskey, buf, buf2);
    }
    write_setting_i(sesskey, "RawCNP", cfg->rawcnp);
    write_setting_i(sesskey, "PasteRTF", cfg->rtf_paste);
    write_setting_i(sesskey, "MouseIsXterm", cfg->mouse_is_xterm);
    write_setting_i(sesskey, "RectSelect", cfg->rect_select);
    write_setting_i(sesskey, "MouseOverride", cfg->mouse_override);
    for (i = 0; i < 256; i += 32) {
	char buf[20], buf2[256];
	int j;
	sprintf(buf, "Wordness%d", i);
	*buf2 = '\0';
	for (j = i; j < i + 32; j++) {
	    sprintf(buf2 + strlen(buf2), "%s%d",
		    (*buf2 ? "," : ""), cfg->wordness[j]);
	}
	write_setting_s(sesskey, buf, buf2);
    }
    write_setting_s(sesskey, "LineCodePage", cfg->line_codepage);
    write_setting_s(sesskey, "Printer", cfg->printer);
    write_setting_i(sesskey, "CapsLockCyr", cfg->xlat_capslockcyr);
    write_setting_i(sesskey, "ScrollBar", cfg->scrollbar);
    write_setting_i(sesskey, "ScrollBarFullScreen", cfg->scrollbar_in_fullscreen);
    write_setting_i(sesskey, "ScrollOnKey", cfg->scroll_on_key);
    write_setting_i(sesskey, "ScrollOnDisp", cfg->scroll_on_disp);
    write_setting_i(sesskey, "LockSize", cfg->resize_action);
    write_setting_i(sesskey, "BCE", cfg->bce);
    write_setting_i(sesskey, "BlinkText", cfg->blinktext);
    write_setting_i(sesskey, "X11Forward", cfg->x11_forward);
    write_setting_s(sesskey, "X11Display", cfg->x11_display);
    write_setting_i(sesskey, "LocalPortAcceptAll", cfg->lport_acceptall);
    write_setting_i(sesskey, "RemotePortAcceptAll", cfg->rport_acceptall);
    {
	char buf[2 * sizeof(cfg->portfwd)], *p, *q;
	p = buf;
	q = cfg->portfwd;
	while (*q) {
	    while (*q) {
		int c = *q++;
		if (c == '=' || c == ',' || c == '\\')
		    *p++ = '\\';
		if (c == '\t')
		    c = '=';
		*p++ = c;
	    }
	    *p++ = ',';
	    q++;
	}
	*p = '\0';
	write_setting_s(sesskey, "PortForwardings", buf);
    }
    write_setting_i(sesskey, "BugIgnore1", cfg->sshbug_ignore1);
    write_setting_i(sesskey, "BugPlainPW1", cfg->sshbug_plainpw1);
    write_setting_i(sesskey, "BugRSA1", cfg->sshbug_rsa1);
    write_setting_i(sesskey, "BugHMAC2", cfg->sshbug_hmac2);
    write_setting_i(sesskey, "BugDeriveKey2", cfg->sshbug_derivekey2);
    write_setting_i(sesskey, "BugRSAPad2", cfg->sshbug_rsapad2);
    write_setting_i(sesskey, "BugDHGEx2", cfg->sshbug_dhgex2);
    close_settings_w(sesskey);
}

void load_settings(char *section, int do_host, Config * cfg)
{
    int i;
    char prot[10];
    void *sesskey;

    sesskey = open_settings_r(section);

    cfg->ssh_subsys = 0;	       /* FIXME: load this properly */
    cfg->remote_cmd_ptr = cfg->remote_cmd;
    cfg->remote_cmd_ptr2 = NULL;

    if (do_host) {
	gpps(sesskey, "HostName", "", cfg->host, sizeof(cfg->host));
    } else {
	cfg->host[0] = '\0';	       /* blank hostname */
    }
    gpps(sesskey, "LogFileName", "putty.log",
	 cfg->logfilename, sizeof(cfg->logfilename));
    gppi(sesskey, "LogType", 0, &cfg->logtype);
    gppi(sesskey, "LogFileClash", LGXF_ASK, &cfg->logxfovr);

    gpps(sesskey, "Protocol", "default", prot, 10);
    cfg->protocol = default_protocol;
    cfg->port = default_port;
    for (i = 0; backends[i].name != NULL; i++)
	if (!strcmp(prot, backends[i].name)) {
	    cfg->protocol = backends[i].protocol;
	    gppi(sesskey, "PortNumber", default_port, &cfg->port);
	    break;
	}

    gppi(sesskey, "CloseOnExit", COE_NORMAL, &cfg->close_on_exit);
    gppi(sesskey, "WarnOnClose", 1, &cfg->warn_on_close);
    {
	/* This is two values for backward compatibility with 0.50/0.51 */
	int pingmin, pingsec;
	gppi(sesskey, "PingInterval", 0, &pingmin);
	gppi(sesskey, "PingIntervalSecs", 0, &pingsec);
	cfg->ping_interval = pingmin * 60 + pingsec;
    }
    gppi(sesskey, "TCPNoDelay", 1, &cfg->tcp_nodelay);
    gpps(sesskey, "TerminalType", "xterm", cfg->termtype,
	 sizeof(cfg->termtype));
    gpps(sesskey, "TerminalSpeed", "38400,38400", cfg->termspeed,
	 sizeof(cfg->termspeed));

    /* proxy settings */
    gpps(sesskey, "ProxyExcludeList", "", cfg->proxy_exclude_list,
	 sizeof(cfg->proxy_exclude_list));
    gppi(sesskey, "ProxyType", PROXY_NONE, &cfg->proxy_type);
    gpps(sesskey, "ProxyHost", "proxy", cfg->proxy_host,
	 sizeof(cfg->proxy_host));
    gppi(sesskey, "ProxyPort", 80, &cfg->proxy_port);
    gpps(sesskey, "ProxyUsername", "", cfg->proxy_username,
	 sizeof(cfg->proxy_username));
    gpps(sesskey, "ProxyPassword", "", cfg->proxy_password,
	 sizeof(cfg->proxy_password));
    gpps(sesskey, "ProxyTelnetCommand", "connect %host %port\\n",
	 cfg->proxy_telnet_command, sizeof(cfg->proxy_telnet_command));
    gppi(sesskey, "ProxySOCKSVersion", 5, &cfg->proxy_socks_version);

    {
	char buf[2 * sizeof(cfg->environmt)], *p, *q;
	gpps(sesskey, "Environment", "", buf, sizeof(buf));
	p = buf;
	q = cfg->environmt;
	while (*p) {
	    while (*p && *p != ',') {
		int c = *p++;
		if (c == '=')
		    c = '\t';
		if (c == '\\')
		    c = *p++;
		*q++ = c;
	    }
	    if (*p == ',')
		p++;
	    *q++ = '\0';
	}
	*q = '\0';
    }
    gpps(sesskey, "UserName", "", cfg->username, sizeof(cfg->username));
    gpps(sesskey, "LocalUserName", "", cfg->localusername,
	 sizeof(cfg->localusername));
    gppi(sesskey, "NoPTY", 0, &cfg->nopty);
    gppi(sesskey, "Compression", 0, &cfg->compression);
    gppi(sesskey, "AgentFwd", 0, &cfg->agentfwd);
    gppi(sesskey, "ChangeUsername", 0, &cfg->change_username);
    gprefs(sesskey, "Cipher", "\0",
	   ciphernames, CIPHER_MAX, cfg->ssh_cipherlist);
    gppi(sesskey, "SshProt", 1, &cfg->sshprot);
    gppi(sesskey, "SSH2DES", 0, &cfg->ssh2_des_cbc);
    gppi(sesskey, "AuthTIS", 0, &cfg->try_tis_auth);
    gppi(sesskey, "AuthKI", 1, &cfg->try_ki_auth);
    gpps(sesskey, "PublicKeyFile", "", cfg->keyfile, sizeof(cfg->keyfile));
    gpps(sesskey, "RemoteCommand", "", cfg->remote_cmd,
	 sizeof(cfg->remote_cmd));
    gppi(sesskey, "RFCEnviron", 0, &cfg->rfc_environ);
    gppi(sesskey, "PassiveTelnet", 0, &cfg->passive_telnet);
    gppi(sesskey, "BackspaceIsDelete", 1, &cfg->bksp_is_delete);
    gppi(sesskey, "RXVTHomeEnd", 0, &cfg->rxvt_homeend);
    gppi(sesskey, "LinuxFunctionKeys", 0, &cfg->funky_type);
    gppi(sesskey, "NoApplicationKeys", 0, &cfg->no_applic_k);
    gppi(sesskey, "NoApplicationCursors", 0, &cfg->no_applic_c);
    gppi(sesskey, "NoMouseReporting", 0, &cfg->no_mouse_rep);
    gppi(sesskey, "NoRemoteResize", 0, &cfg->no_remote_resize);
    gppi(sesskey, "NoAltScreen", 0, &cfg->no_alt_screen);
    gppi(sesskey, "NoRemoteWinTitle", 0, &cfg->no_remote_wintitle);
    gppi(sesskey, "NoDBackspace", 0, &cfg->no_dbackspace);
    gppi(sesskey, "NoRemoteCharset", 0, &cfg->no_remote_charset);
    gppi(sesskey, "ApplicationCursorKeys", 0, &cfg->app_cursor);
    gppi(sesskey, "ApplicationKeypad", 0, &cfg->app_keypad);
    gppi(sesskey, "NetHackKeypad", 0, &cfg->nethack_keypad);
    gppi(sesskey, "AltF4", 1, &cfg->alt_f4);
    gppi(sesskey, "AltSpace", 0, &cfg->alt_space);
    gppi(sesskey, "AltOnly", 0, &cfg->alt_only);
    gppi(sesskey, "ComposeKey", 0, &cfg->compose_key);
    gppi(sesskey, "CtrlAltKeys", 1, &cfg->ctrlaltkeys);
    gppi(sesskey, "TelnetKey", 0, &cfg->telnet_keyboard);
    gppi(sesskey, "TelnetRet", 1, &cfg->telnet_newline);
    gppi(sesskey, "LocalEcho", LD_BACKEND, &cfg->localecho);
    gppi(sesskey, "LocalEdit", LD_BACKEND, &cfg->localedit);
    gpps(sesskey, "Answerback", "PuTTY", cfg->answerback,
	 sizeof(cfg->answerback));
    gppi(sesskey, "AlwaysOnTop", 0, &cfg->alwaysontop);
    gppi(sesskey, "FullScreenOnAltEnter", 0, &cfg->fullscreenonaltenter);
    gppi(sesskey, "HideMousePtr", 0, &cfg->hide_mouseptr);
    gppi(sesskey, "SunkenEdge", 0, &cfg->sunken_edge);
    gppi(sesskey, "WindowBorder", 1, &cfg->window_border);
    gppi(sesskey, "CurType", 0, &cfg->cursor_type);
    gppi(sesskey, "BlinkCur", 0, &cfg->blink_cur);
    /* pedantic compiler tells me I can't use &cfg->beep as an int * :-) */
    gppi(sesskey, "Beep", 1, &i); cfg->beep = i;
    gppi(sesskey, "BeepInd", 0, &i); cfg->beep_ind = i;
    gpps(sesskey, "BellWaveFile", "", cfg->bell_wavefile,
	 sizeof(cfg->bell_wavefile));
    gppi(sesskey, "BellOverload", 1, &cfg->bellovl);
    gppi(sesskey, "BellOverloadN", 5, &cfg->bellovl_n);
    gppi(sesskey, "BellOverloadT", 2000, &cfg->bellovl_t);
    gppi(sesskey, "BellOverloadS", 5000, &cfg->bellovl_s);
    gppi(sesskey, "ScrollbackLines", 200, &cfg->savelines);
    gppi(sesskey, "DECOriginMode", 0, &cfg->dec_om);
    gppi(sesskey, "AutoWrapMode", 1, &cfg->wrap_mode);
    gppi(sesskey, "LFImpliesCR", 0, &cfg->lfhascr);
    gppi(sesskey, "WinNameAlways", 0, &cfg->win_name_always);
    gpps(sesskey, "WinTitle", "", cfg->wintitle, sizeof(cfg->wintitle));
    gppi(sesskey, "TermWidth", 80, &cfg->width);
    gppi(sesskey, "TermHeight", 24, &cfg->height);
    gpps(sesskey, "Font", "Courier New", cfg->font, sizeof(cfg->font));
    gppi(sesskey, "FontIsBold", 0, &cfg->fontisbold);
    gppi(sesskey, "FontCharSet", ANSI_CHARSET, &cfg->fontcharset);
    gppi(sesskey, "FontHeight", 10, &cfg->fontheight);
    if (cfg->fontheight < 0) {
	int oldh, newh;
	HDC hdc = GetDC(NULL);
	int logpix = GetDeviceCaps(hdc, LOGPIXELSY);
	ReleaseDC(NULL, hdc);

	oldh = -cfg->fontheight;
	newh = MulDiv(oldh, 72, logpix) + 1;
	if (MulDiv(newh, logpix, 72) > oldh)
	    newh--;
	cfg->fontheight = newh;
    }
    gppi(sesskey, "FontVTMode", VT_UNICODE, (int *) &cfg->vtmode);
    gppi(sesskey, "TryPalette", 0, &cfg->try_palette);
    gppi(sesskey, "BoldAsColour", 1, &cfg->bold_colour);
    for (i = 0; i < 22; i++) {
	static char *defaults[] = {
	    "187,187,187", "255,255,255", "0,0,0", "85,85,85", "0,0,0",
	    "0,255,0", "0,0,0", "85,85,85", "187,0,0", "255,85,85",
	    "0,187,0", "85,255,85", "187,187,0", "255,255,85", "0,0,187",
	    "85,85,255", "187,0,187", "255,85,255", "0,187,187",
	    "85,255,255", "187,187,187", "255,255,255"
	};
	char buf[20], buf2[30];
	int c0, c1, c2;
	sprintf(buf, "Colour%d", i);
	gpps(sesskey, buf, defaults[i], buf2, sizeof(buf2));
	if (sscanf(buf2, "%d,%d,%d", &c0, &c1, &c2) == 3) {
	    cfg->colours[i][0] = c0;
	    cfg->colours[i][1] = c1;
	    cfg->colours[i][2] = c2;
	}
    }
    gppi(sesskey, "RawCNP", 0, &cfg->rawcnp);
    gppi(sesskey, "PasteRTF", 0, &cfg->rtf_paste);
    gppi(sesskey, "MouseIsXterm", 0, &cfg->mouse_is_xterm);
    gppi(sesskey, "RectSelect", 0, &cfg->rect_select);
    gppi(sesskey, "MouseOverride", 1, &cfg->mouse_override);
    for (i = 0; i < 256; i += 32) {
	static char *defaults[] = {
	    "0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0",
	    "0,1,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1",
	    "1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2",
	    "1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1",
	    "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",
	    "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",
	    "2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2",
	    "2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2"
	};
	char buf[20], buf2[256], *p;
	int j;
	sprintf(buf, "Wordness%d", i);
	gpps(sesskey, buf, defaults[i / 32], buf2, sizeof(buf2));
	p = buf2;
	for (j = i; j < i + 32; j++) {
	    char *q = p;
	    while (*p && *p != ',')
		p++;
	    if (*p == ',')
		*p++ = '\0';
	    cfg->wordness[j] = atoi(q);
	}
    }
    /*
     * The empty default for LineCodePage will be converted later
     * into a plausible default for the locale.
     */
    gpps(sesskey, "LineCodePage", "", cfg->line_codepage,
	 sizeof(cfg->line_codepage));
    gpps(sesskey, "Printer", "", cfg->printer, sizeof(cfg->printer));
    gppi (sesskey, "CapsLockCyr", 0, &cfg->xlat_capslockcyr);
    gppi(sesskey, "ScrollBar", 1, &cfg->scrollbar);
    gppi(sesskey, "ScrollBarFullScreen", 0, &cfg->scrollbar_in_fullscreen);
    gppi(sesskey, "ScrollOnKey", 0, &cfg->scroll_on_key);
    gppi(sesskey, "ScrollOnDisp", 1, &cfg->scroll_on_disp);
    gppi(sesskey, "LockSize", 0, &cfg->resize_action);
    gppi(sesskey, "BCE", 0, &cfg->bce);
    gppi(sesskey, "BlinkText", 0, &cfg->blinktext);
    gppi(sesskey, "X11Forward", 0, &cfg->x11_forward);
    gpps(sesskey, "X11Display", "localhost:0", cfg->x11_display,
	 sizeof(cfg->x11_display));

    gppi(sesskey, "LocalPortAcceptAll", 0, &cfg->lport_acceptall);
    gppi(sesskey, "RemotePortAcceptAll", 0, &cfg->rport_acceptall);
    {
	char buf[2 * sizeof(cfg->portfwd)], *p, *q;
	gpps(sesskey, "PortForwardings", "", buf, sizeof(buf));
	p = buf;
	q = cfg->portfwd;
	while (*p) {
	    while (*p && *p != ',') {
		int c = *p++;
		if (c == '=')
		    c = '\t';
		if (c == '\\')
		    c = *p++;
		*q++ = c;
	    }
	    if (*p == ',')
		p++;
	    *q++ = '\0';
	}
	*q = '\0';
    }
    gppi(sesskey, "BugIgnore1", BUG_AUTO, &cfg->sshbug_ignore1);
    gppi(sesskey, "BugPlainPW1", BUG_AUTO, &cfg->sshbug_plainpw1);
    gppi(sesskey, "BugRSA1", BUG_AUTO, &cfg->sshbug_rsa1);
    {
	int i;
	gppi(sesskey, "BugHMAC2", BUG_AUTO, &cfg->sshbug_hmac2);
	if (cfg->sshbug_hmac2 == BUG_AUTO) {
	    gppi(sesskey, "BuggyMAC", 0, &i);
	    if (i == 1)
		cfg->sshbug_hmac2 = BUG_ON;
	}
    }
    gppi(sesskey, "BugDeriveKey2", BUG_AUTO, &cfg->sshbug_derivekey2);
    gppi(sesskey, "BugRSAPad2", BUG_AUTO, &cfg->sshbug_rsapad2);
    gppi(sesskey, "BugDHGEx2", BUG_AUTO, &cfg->sshbug_dhgex2);

    close_settings_r(sesskey);
}

void do_defaults(char *session, Config * cfg)
{
    if (session)
	load_settings(session, TRUE, cfg);
    else
	load_settings("Default Settings", FALSE, cfg);
}

static int sessioncmp(const void *av, const void *bv)
{
    const char *a = *(const char *const *) av;
    const char *b = *(const char *const *) bv;

    /*
     * Alphabetical order, except that "Default Settings" is a
     * special case and comes first.
     */
    if (!strcmp(a, "Default Settings"))
	return -1;		       /* a comes first */
    if (!strcmp(b, "Default Settings"))
	return +1;		       /* b comes first */
    /*
     * FIXME: perhaps we should ignore the first & in determining
     * sort order.
     */
    return strcmp(a, b);	       /* otherwise, compare normally */
}

void get_sesslist(int allocate)
{
    static char otherbuf[2048];
    static char *buffer;
    int buflen, bufsize, i;
    char *p, *ret;
    void *handle;

    if (allocate) {

	buflen = bufsize = 0;
	buffer = NULL;
	if ((handle = enum_settings_start())) {
	    do {
		ret = enum_settings_next(handle, otherbuf, sizeof(otherbuf));
		if (ret) {
		    int len = strlen(otherbuf) + 1;
		    if (bufsize < buflen + len) {
			bufsize = buflen + len + 2048;
			buffer = srealloc(buffer, bufsize);
		    }
		    strcpy(buffer + buflen, otherbuf);
		    buflen += strlen(buffer + buflen) + 1;
		}
	    } while (ret);
	    enum_settings_finish(handle);
	}
	buffer = srealloc(buffer, buflen + 1);
	buffer[buflen] = '\0';

	/*
	 * Now set up the list of sessions. Note that "Default
	 * Settings" must always be claimed to exist, even if it
	 * doesn't really.
	 */

	p = buffer;
	nsessions = 1;		       /* "Default Settings" counts as one */
	while (*p) {
	    if (strcmp(p, "Default Settings"))
		nsessions++;
	    while (*p)
		p++;
	    p++;
	}

	sessions = smalloc((nsessions + 1) * sizeof(char *));
	sessions[0] = "Default Settings";
	p = buffer;
	i = 1;
	while (*p) {
	    if (strcmp(p, "Default Settings"))
		sessions[i++] = p;
	    while (*p)
		p++;
	    p++;
	}

	qsort(sessions, i, sizeof(char *), sessioncmp);
    } else {
	sfree(buffer);
	sfree(sessions);
    }
}
