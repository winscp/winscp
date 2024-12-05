/*
 * Master list of configuration options living in the Conf data
 * structure.
 *
 * Each CONF_OPTION directive defines a single CONF_foo primary key in
 * Conf, and can be equipped with the following properties:
 *
 *  - VALUE_TYPE: the type of data associated with that key
 *  - SUBKEY_TYPE: if the primary key goes with a subkey (that is, the
 *    primary key identifies some mapping from subkeys to values), the
 *    data type of the subkey
 *  - DEFAULT_INT, DEFAULT_STR, DEFAULT_BOOL: the default value for
 *    the key, if no save data is available. Must match VALUE_TYPE, if
 *    the key has no subkey. Otherwise, no default is permitted, and
 *    the default value of the mapping is assumed to be empty (and if
 *    not, then LOAD_CUSTOM code must override that).
 *  - SAVE_KEYWORD: the keyword used for the option in the Windows
 *    registry or ~/.putty/sessions save files.
 *  - STORAGE_ENUM: for int-typed settings with no subkeys, this
 *    identifies an enumeration in conf-enums.h which maps internal
 *    values of the setting in the Conf to values in the saved data.
 *  - LOAD_CUSTOM, SAVE_CUSTOM: suppress automated loading or saving
 *    (respectively) of this setting, in favour of manual code in
 *    settings.c load_open_settings() or save_open_settings()
 *    respectively.
 *  - NOT_SAVED: indicate that this setting is not part of saved
 *    session data at all.
 */

CONF_OPTION(host, NONE, STR, (int)"", false, false, false, "HostName", NULL)
/* default value depends on the value of CONF_protocol */
CONF_OPTION(port, NONE, INT, 0, false, true, false, "PortNumber", NULL)
/* PROT_SSH, PROT_TELNET etc */
/*
 * Notionally SAVE_KEYWORD("Protocol"), but saving/loading is handled by
 * custom code because the stored value is a string representation
 * of the protocol name.
 */
CONF_OPTION(protocol, NONE, INT, 0, true, true, false, NULL, NULL)
CONF_OPTION(addressfamily, NONE, INT, ADDRTYPE_UNSPEC, false, false, false, "AddressFamily", &conf_enum_addressfamily)
CONF_OPTION(close_on_exit, NONE, INT, AUTO, false, false, false, "CloseOnExit", &conf_enum_off_auto_on)
CONF_OPTION(warn_on_close, NONE, BOOL, (int)true, false, false, false, "WarnOnClose", NULL)
/* in seconds */
/*
 * Saving/loading is handled by custom code because for historical
 * reasons this value corresponds to two save keywords,
 * "PingInterval" (measured in minutes) and "PingIntervalSecs"
 * (measured in seconds), which are added together on loading.
 * Rationale: the value was once measured in minutes, and the
 * seconds field was added later.
 */
CONF_OPTION(ping_interval, NONE, INT, 0, true, true, false, NULL, NULL)
CONF_OPTION(tcp_nodelay, NONE, BOOL, (int)true, false, false, false, "TCPNoDelay", NULL)
CONF_OPTION(tcp_keepalives, NONE, BOOL, (int)false, false, false, false, "TCPKeepalives", NULL)
/* logical host being contacted, for host key check */
CONF_OPTION(loghost, NONE, STR, (int)"", false, false, false, "LogHost", NULL)

/* Proxy options */
CONF_OPTION(proxy_exclude_list, NONE, STR, (int)"", false, false, false, "ProxyExcludeList", NULL)
CONF_OPTION(proxy_dns, NONE, INT, AUTO, false, false, false, "ProxyDNS", &conf_enum_off_auto_on)
CONF_OPTION(even_proxy_localhost, NONE, BOOL, (int)false, false, false, false, "ProxyLocalhost", NULL)
/* PROXY_NONE, PROXY_SOCKS4, ... */
/*
 * Custom load code: there was an earlier keyword "ProxyType"
 * using a different enumeration, in which SOCKS4 and SOCKS5
 * shared a value, and a second keyword "ProxySOCKSVersion"
 * disambiguated.
 */
CONF_OPTION(proxy_type, NONE, INT, 0, false, true, false, "ProxyMethod", &conf_enum_proxy_type)
CONF_OPTION(proxy_host, NONE, STR, (int)"proxy", false, false, false, "ProxyHost", NULL)
CONF_OPTION(proxy_port, NONE, INT, 80, false, false, false, "ProxyPort", NULL)
CONF_OPTION(proxy_username, NONE, STR, (int)"", false, false, false, "ProxyUsername", NULL)
CONF_OPTION(proxy_password, NONE, STR, (int)"", false, false, false, "ProxyPassword", NULL)
CONF_OPTION(proxy_telnet_command, NONE, STR, (int)"connect %host %port\\n", false, false, false, "ProxyTelnetCommand", NULL)
CONF_OPTION(proxy_log_to_term, NONE, INT, FORCE_OFF, false, false, false, "ProxyLogToTerm", &conf_enum_on_off_auto)

/* SSH options */
CONF_OPTION(remote_cmd, NONE, STR_AMBI, (int)"", false, false, false, "RemoteCommand", NULL)
/*
 * Fallback command to try to run if remote_cmd fails. Only set
 * internally by PSCP and PSFTP (so that they can try multiple
 * methods of running an SFTP server at the remote end); never set
 * by user configuration, or loaded or saved.
 */
CONF_OPTION(remote_cmd2, NONE, STR_AMBI, (int)"", false, false, true, NULL, NULL)
CONF_OPTION(nopty, NONE, BOOL, (int)false, false, false, false, "NoPTY", NULL)
CONF_OPTION(compression, NONE, BOOL, (int)false, false, false, false, "Compression", NULL)
/* indices in preference order: 0,...,KEX_MAX-1
 * (lower is more preferred) */
/* KEX_* enum values */
/* necessary for preference lists */
CONF_OPTION(ssh_kexlist, INT, INT, 0, true, true, false, NULL, NULL)
/* indices in preference order: 0,...,HK_MAX-1
 * (lower is more preferred) */
/* HK_* enum values */
/* necessary for preference lists */
CONF_OPTION(ssh_hklist, INT, INT, 0, true, true, false, NULL, NULL)
CONF_OPTION(ssh_prefer_known_hostkeys, NONE, BOOL, (int)true, false, false, false, "PreferKnownHostKeys", NULL)
/* in minutes */
CONF_OPTION(ssh_rekey_time, NONE, INT, 60, false, false, false, "RekeyTime", NULL)
/* string encoding e.g. "100K", "2M", "1G" */
CONF_OPTION(ssh_rekey_data, NONE, STR, (int)"1G", false, false, false, "RekeyBytes", NULL)
CONF_OPTION(tryagent, NONE, BOOL, (int)true, false, false, false, "TryAgent", NULL)
CONF_OPTION(agentfwd, NONE, BOOL, (int)false, false, false, false, "AgentFwd", NULL)
/* allow username switching in SSH-2 */
CONF_OPTION(change_username, NONE, BOOL, (int)false, false, false, false, "ChangeUsername", NULL)
/* indices in preference order: 0,...,CIPHER_MAX-1
 * (lower is more preferred) */
/* CIPHER_* enum values */
/* necessary for preference lists */
CONF_OPTION(ssh_cipherlist, INT, INT, 0, true, true, false, NULL, NULL)
CONF_OPTION(keyfile, NONE, FILENAME, 0, false, false, false, "PublicKeyFile", NULL)
CONF_OPTION(detached_cert, NONE, FILENAME, 0, false, false, false, "DetachedCertificate", NULL)
CONF_OPTION(auth_plugin, NONE, STR, (int)"", false, false, false, "AuthPlugin", NULL)
/*
 * Which SSH protocol to use.
 *
 * For historical reasons, the current legal values for CONF_sshprot
 * are:
 *  0 = SSH-1 only
 *  3 = SSH-2 only
 *
 * We used to also support
 *  1 = SSH-1 with fallback to SSH-2
 *  2 = SSH-2 with fallback to SSH-1
 *
 * and we continue to use 0/3 in storage formats rather than the more
 * obvious 1/2 to avoid surprises if someone saves a session and later
 * downgrades PuTTY. So it's easier to use these numbers internally too.
 */
CONF_OPTION(sshprot, NONE, INT, 3, false, false, false, "SshProt", &conf_enum_ssh_protocol)
/*
 * This means that we promise never to open any channel other
 * than the main one, which means it can safely use a very large
 * window in SSH-2.
 *
 * Only ever set internally by file transfer tools; never set by
 * user configuration, or loaded or saved.
 */
CONF_OPTION(ssh_simple, NONE, BOOL, (int)false, false, false, true, NULL, NULL)
CONF_OPTION(ssh_connection_sharing, NONE, BOOL, (int)false, false, false, false, "ConnectionSharing", NULL)
CONF_OPTION(ssh_connection_sharing_upstream, NONE, BOOL, (int)true, false, false, false, "ConnectionSharingUpstream", NULL)
CONF_OPTION(ssh_connection_sharing_downstream, NONE, BOOL, (int)true, false, false, false, "ConnectionSharingDownstream", NULL)
/*
 * Manually configured host keys to accept regardless of the state
 * of the host key cache.
 *
 * This is conceptually a set rather than a dictionary: every
 * value in this map is the empty string, and the set of subkeys
 * that exist is the important data.
 */
/* necessary for mappings */
CONF_OPTION(ssh_manual_hostkeys, STR, STR, 0, true, true, false, NULL, NULL)
/* "des-cbc" unrecommended SSH-2 cipher */
CONF_OPTION(ssh2_des_cbc, NONE, BOOL, (int)false, false, false, false, "SSH2DES", NULL)
/* bypass "ssh-userauth" (SSH-2 only) */
CONF_OPTION(ssh_no_userauth, NONE, BOOL, (int)false, false, false, false, "SshNoAuth", NULL)
/* disable trivial types of auth */
CONF_OPTION(ssh_no_trivial_userauth, NONE, BOOL, (int)false, false, false, false, "SshNoTrivialAuth", NULL)
/* show USERAUTH_BANNERs (SSH-2 only) */
CONF_OPTION(ssh_show_banner, NONE, BOOL, (int)true, false, false, false, "SshBanner", NULL)
CONF_OPTION(try_tis_auth, NONE, BOOL, (int)false, false, false, false, "AuthTIS", NULL)
CONF_OPTION(try_ki_auth, NONE, BOOL, (int)true, false, false, false, "AuthKI", NULL)
/* attempt gssapi via ssh userauth */
/* under #ifndef NO_GSSAPI */
CONF_OPTION(try_gssapi_auth, NONE, BOOL, 0, true, true, false, NULL, NULL)
/* attempt gssapi via ssh kex */
/* under #ifndef NO_GSSAPI */
CONF_OPTION(try_gssapi_kex, NONE, BOOL, 0, true, true, false, NULL, NULL)
/* forward tgt via gss */
/* under #ifndef NO_GSSAPI */
CONF_OPTION(gssapifwd, NONE, BOOL, 0, true, true, false, NULL, NULL)
/* KEXGSS refresh interval (mins) */
/* under #ifndef NO_GSSAPI */
CONF_OPTION(gssapirekey, NONE, INT, 0, true, true, false, NULL, NULL)
/* indices in preference order: 0,...,ngsslibs
 * (lower is more preferred; ngsslibs is a platform-
 * dependent value) */
/* indices of GSSAPI lib types (platform-dependent) */
/* necessary for preference lists, also this
 * setting is under #ifndef NO_GSSAPI */
CONF_OPTION(ssh_gsslist, INT, INT, 0, true, true, false, NULL, NULL)
/* under #ifndef NO_GSSAPI */
CONF_OPTION(ssh_gss_custom, NONE, FILENAME, 0, true, true, false, NULL, NULL)
/* run a subsystem rather than a command */
/*
 * Only set internally by PSCP and PSFTP; never set by user
 * configuration, or loaded or saved.
 */
CONF_OPTION(ssh_subsys, NONE, BOOL, (int)false, false, false, true, NULL, NULL)
/* fallback to go with remote_cmd2 */
/*
 * Only set internally by PSCP and PSFTP; never set by user
 * configuration, or loaded or saved.
 */
CONF_OPTION(ssh_subsys2, NONE, BOOL, (int)false, false, false, true, NULL, NULL)
/* avoid running a shell */
CONF_OPTION(ssh_no_shell, NONE, BOOL, (int)false, false, false, false, "SshNoShell", NULL)
/* host to connect to in `nc' mode */
/*
 * Only set by the '-nc' command-line option and by the SSH proxy
 * code. There's no GUI config option for this, and therefore it's
 * also never loaded or saved.
 */
CONF_OPTION(ssh_nc_host, NONE, STR, (int)"", false, false, true, NULL, NULL)
/* port to connect to in `nc' mode */
/*
 * Only set by the '-nc' command-line option and by the SSH proxy
 * code. There's no GUI config option for this, and therefore it's
 * also never loaded or saved.
 */
CONF_OPTION(ssh_nc_port, NONE, INT, 0, false, false, true, NULL, NULL)

/* Telnet options */
CONF_OPTION(termtype, NONE, STR, (int)"xterm", false, false, false, "TerminalType", NULL)
CONF_OPTION(termspeed, NONE, STR, (int)"38400,38400", false, false, false, "TerminalSpeed", NULL)
/*
 * The full set of permitted subkeys is listed in
 * ssh/ttymode-list.h, as the first parameter of each TTYMODE_CHAR
 * or TTYMODE_FLAG macro.
 *
 * The permitted value strings are:
 *
 *  - "N" means do not include a record for this mode at all in
 *    the terminal mode data in the "pty-req" channel request.
 *    Corresponds to setting the mode to 'Nothing' in the GUI.
 *  - "A" means use PuTTY's automatic default, matching the
 *    settings for GUI PuTTY's terminal window or Unix Plink's
 *    controlling tty. Corresponds to setting 'Auto' in the GUI.
 *  - "V" followed by further string data means send a custom
 *    value to the SSH server. Values are as documented in the
 *    manual.
 */
/* necessary for mappings */
CONF_OPTION(ttymodes, STR, STR, 0, true, true, false, NULL, NULL)
/* environment variable name */
/* environment variable value */
/* necessary for mappings */
CONF_OPTION(environmt, STR, STR, 0, true, true, false, NULL, NULL)
CONF_OPTION(username, NONE, STR_AMBI, (int)"", false, false, false, "UserName", NULL)
CONF_OPTION(username_from_env, NONE, BOOL, (int)false, false, false, false, "UserNameFromEnvironment", NULL)
CONF_OPTION(localusername, NONE, STR, (int)"", false, false, false, "LocalUserName", NULL)
CONF_OPTION(rfc_environ, NONE, BOOL, (int)false, false, false, false, "RFCEnviron", NULL)
CONF_OPTION(passive_telnet, NONE, BOOL, (int)false, false, false, false, "PassiveTelnet", NULL)

/* Serial port options */
CONF_OPTION(serline, NONE, STR, (int)"", false, false, false, "SerialLine", NULL)
CONF_OPTION(serspeed, NONE, INT, 9600, false, false, false, "SerialSpeed", NULL)
CONF_OPTION(serdatabits, NONE, INT, 8, false, false, false, "SerialDataBits", NULL)
CONF_OPTION(serstopbits, NONE, INT, 2, false, false, false, "SerialStopHalfbits", NULL)
CONF_OPTION(serparity, NONE, INT, SER_PAR_NONE, false, false, false, "SerialParity", &conf_enum_serparity)
CONF_OPTION(serflow, NONE, INT, SER_FLOW_XONXOFF, false, false, false, "SerialFlowControl", &conf_enum_serflow)

/* SUPDUP options */
CONF_OPTION(supdup_location, NONE, STR, (int)"The Internet", false, false, false, "SUPDUPLocation", NULL)
CONF_OPTION(supdup_ascii_set, NONE, INT, SUPDUP_CHARSET_ASCII, false, false, false, "SUPDUPCharset", &conf_enum_supdup_charset)
CONF_OPTION(supdup_more, NONE, BOOL, (int)false, false, false, false, "SUPDUPMoreProcessing", NULL)
CONF_OPTION(supdup_scroll, NONE, BOOL, (int)false, false, false, false, "SUPDUPScrolling", NULL)

/* Keyboard options */
CONF_OPTION(bksp_is_delete, NONE, BOOL, (int)true, false, false, false, "BackspaceIsDelete", NULL)
CONF_OPTION(rxvt_homeend, NONE, BOOL, (int)false, false, false, false, "RXVTHomeEnd", NULL)
CONF_OPTION(funky_type, NONE, INT, FUNKY_TILDE, false, false, false, "LinuxFunctionKeys", &conf_enum_funky_type)
CONF_OPTION(sharrow_type, NONE, INT, SHARROW_APPLICATION, false, false, false, "ShiftedArrowKeys", &conf_enum_sharrow_type)
/* totally disable app cursor keys */
CONF_OPTION(no_applic_c, NONE, BOOL, (int)false, false, false, false, "NoApplicationCursors", NULL)
/* totally disable app keypad */
CONF_OPTION(no_applic_k, NONE, BOOL, (int)false, false, false, false, "NoApplicationKeys", NULL)
/* totally disable mouse reporting */
CONF_OPTION(no_mouse_rep, NONE, BOOL, (int)false, false, false, false, "NoMouseReporting", NULL)
/* disable remote resizing */
CONF_OPTION(no_remote_resize, NONE, BOOL, (int)false, false, false, false, "NoRemoteResize", NULL)
/* disable alternate screen */
CONF_OPTION(no_alt_screen, NONE, BOOL, (int)false, false, false, false, "NoAltScreen", NULL)
/* disable remote retitling */
CONF_OPTION(no_remote_wintitle, NONE, BOOL, (int)false, false, false, false, "NoRemoteWinTitle", NULL)
/* disable ESC[3J */
CONF_OPTION(no_remote_clearscroll, NONE, BOOL, (int)false, false, false, false, "NoRemoteClearScroll", NULL)
/* disable destructive backspace */
CONF_OPTION(no_dbackspace, NONE, BOOL, (int)false, false, false, false, "NoDBackspace", NULL)
/* disable remote charset config */
CONF_OPTION(no_remote_charset, NONE, BOOL, (int)false, false, false, false, "NoRemoteCharset", NULL)
/* handling of remote window title queries */
/* older versions had a boolean "NoRemoteQTitle"
 * before we ended up with three options */
CONF_OPTION(remote_qtitle_action, NONE, INT, 0, false, true, false, "RemoteQTitleAction", &conf_enum_remote_qtitle_action)
CONF_OPTION(app_cursor, NONE, BOOL, (int)false, false, false, false, "ApplicationCursorKeys", NULL)
CONF_OPTION(app_keypad, NONE, BOOL, (int)false, false, false, false, "ApplicationKeypad", NULL)
CONF_OPTION(nethack_keypad, NONE, BOOL, (int)false, false, false, false, "NetHackKeypad", NULL)
CONF_OPTION(telnet_keyboard, NONE, BOOL, (int)false, false, false, false, "TelnetKey", NULL)
CONF_OPTION(telnet_newline, NONE, BOOL, (int)true, false, false, false, "TelnetRet", NULL)
/* is it special? */
CONF_OPTION(alt_f4, NONE, BOOL, (int)true, false, false, false, "AltF4", NULL)
/* is it special? */
CONF_OPTION(alt_space, NONE, BOOL, (int)false, false, false, false, "AltSpace", NULL)
/* is it special? */
CONF_OPTION(alt_only, NONE, BOOL, (int)false, false, false, false, "AltOnly", NULL)
CONF_OPTION(localecho, NONE, INT, AUTO, false, false, false, "LocalEcho", &conf_enum_on_off_auto)
CONF_OPTION(localedit, NONE, INT, AUTO, false, false, false, "LocalEdit", &conf_enum_on_off_auto)
CONF_OPTION(alwaysontop, NONE, BOOL, (int)false, false, false, false, "AlwaysOnTop", NULL)
CONF_OPTION(fullscreenonaltenter, NONE, BOOL, (int)false, false, false, false, "FullScreenOnAltEnter", NULL)
CONF_OPTION(scroll_on_key, NONE, BOOL, (int)false, false, false, false, "ScrollOnKey", NULL)
CONF_OPTION(scroll_on_disp, NONE, BOOL, (int)true, false, false, false, "ScrollOnDisp", NULL)
CONF_OPTION(erase_to_scrollback, NONE, BOOL, (int)true, false, false, false, "EraseToScrollback", NULL)
CONF_OPTION(compose_key, NONE, BOOL, (int)false, false, false, false, "ComposeKey", NULL)
CONF_OPTION(ctrlaltkeys, NONE, BOOL, (int)true, false, false, false, "CtrlAltKeys", NULL)
/* under #ifdef OSX_META_KEY_CONFIG */
CONF_OPTION(osx_option_meta, NONE, BOOL, 0, true, true, false, NULL, NULL)
/* under #ifdef OSX_META_KEY_CONFIG */
CONF_OPTION(osx_command_meta, NONE, BOOL, 0, true, true, false, NULL, NULL)
/* initial window title */
CONF_OPTION(wintitle, NONE, STR, (int)"", false, false, false, "WinTitle", NULL)
/* Terminal options */
CONF_OPTION(savelines, NONE, INT, 2000, false, false, false, "ScrollbackLines", NULL)
CONF_OPTION(dec_om, NONE, BOOL, (int)false, false, false, false, "DECOriginMode", NULL)
CONF_OPTION(wrap_mode, NONE, BOOL, (int)true, false, false, false, "AutoWrapMode", NULL)
CONF_OPTION(lfhascr, NONE, BOOL, (int)false, false, false, false, "LFImpliesCR", NULL)
CONF_OPTION(cursor_type, NONE, INT, 0, false, false, false, "CurType", &conf_enum_cursor_type)
CONF_OPTION(blink_cur, NONE, BOOL, (int)false, false, false, false, "BlinkCur", NULL)
CONF_OPTION(beep, NONE, INT, BELL_DEFAULT, false, false, false, "Beep", &conf_enum_beep)
CONF_OPTION(beep_ind, NONE, INT, B_IND_DISABLED, false, false, false, "BeepInd", &conf_enum_beep_indication)
/* bell overload protection active? */
CONF_OPTION(bellovl, NONE, BOOL, (int)true, false, false, false, "BellOverload", NULL)
/* number of bells to cause overload */
CONF_OPTION(bellovl_n, NONE, INT, 5, false, false, false, "BellOverloadN", NULL)
/* time interval for overload (ticks) */
/*
 * Loading and saving is done in custom code because the format is
 * platform-dependent for historical reasons: on Unix, the stored
 * value is multiplied by 1000. (And since TICKSPERSEC=1000 on
 * that platform, it means the stored value is interpreted in
 * microseconds.)
 */
CONF_OPTION(bellovl_t, NONE, INT, 0, true, true, false, NULL, NULL)
/* period of silence to re-enable bell (s) */
/*
 * Loading and saving is done in custom code because the format is
 * platform-dependent for historical reasons: on Unix, the stored
 * value is multiplied by 1000. (And since TICKSPERSEC=1000 on
 * that platform, it means the stored value is interpreted in
 * microseconds.)
 */
CONF_OPTION(bellovl_s, NONE, INT, 0, true, true, false, NULL, NULL)
CONF_OPTION(bell_wavefile, NONE, FILENAME, 0, false, false, false, "BellWaveFile", NULL)
CONF_OPTION(scrollbar, NONE, BOOL, (int)true, false, false, false, "ScrollBar", NULL)
CONF_OPTION(scrollbar_in_fullscreen, NONE, BOOL, (int)false, false, false, false, "ScrollBarFullScreen", NULL)
CONF_OPTION(resize_action, NONE, INT, RESIZE_TERM, false, false, false, "LockSize", &conf_enum_resize_effect)
CONF_OPTION(bce, NONE, BOOL, (int)true, false, false, false, "BCE", NULL)
CONF_OPTION(blinktext, NONE, BOOL, (int)false, false, false, false, "BlinkText", NULL)
CONF_OPTION(win_name_always, NONE, BOOL, (int)true, false, false, false, "WinNameAlways", NULL)
CONF_OPTION(width, NONE, INT, 80, false, false, false, "TermWidth", NULL)
CONF_OPTION(height, NONE, INT, 24, false, false, false, "TermHeight", NULL)
CONF_OPTION(font, NONE, FONT, 0, false, false, false, "Font", NULL)
CONF_OPTION(font_quality, NONE, INT, FQ_DEFAULT, false, false, false, "FontQuality", &conf_enum_font_quality)
CONF_OPTION(logfilename, NONE, FILENAME, 0, false, false, false, "LogFileName", NULL)
CONF_OPTION(logtype, NONE, INT, LGTYP_NONE, false, false, false, "LogType", &conf_enum_log_type)
CONF_OPTION(logxfovr, NONE, INT, LGXF_ASK, false, false, false, "LogFileClash", &conf_enum_log_to_existing_file)
CONF_OPTION(logflush, NONE, BOOL, (int)true, false, false, false, "LogFlush", NULL)
CONF_OPTION(logheader, NONE, BOOL, (int)true, false, false, false, "LogHeader", NULL)
CONF_OPTION(logomitpass, NONE, BOOL, (int)true, false, false, false, "SSHLogOmitPasswords", NULL)
CONF_OPTION(logomitdata, NONE, BOOL, (int)false, false, false, false, "SSHLogOmitData", NULL)
CONF_OPTION(hide_mouseptr, NONE, BOOL, (int)false, false, false, false, "HideMousePtr", NULL)
CONF_OPTION(sunken_edge, NONE, BOOL, (int)false, false, false, false, "SunkenEdge", NULL)
/* in pixels */
CONF_OPTION(window_border, NONE, INT, 1, false, false, false, "WindowBorder", NULL)
CONF_OPTION(answerback, NONE, STR, (int)"PuTTY", false, false, false, "Answerback", NULL)
CONF_OPTION(printer, NONE, STR, (int)"", false, false, false, "Printer", NULL)
CONF_OPTION(no_arabicshaping, NONE, BOOL, (int)false, false, false, false, "DisableArabicShaping", NULL)
CONF_OPTION(no_bidi, NONE, BOOL, (int)false, false, false, false, "DisableBidi", NULL)
CONF_OPTION(no_bracketed_paste, NONE, BOOL, (int)false, false, false, false, "DisableBracketedPaste", NULL)

/* Colour options */
CONF_OPTION(ansi_colour, NONE, BOOL, (int)true, false, false, false, "ANSIColour", NULL)
CONF_OPTION(xterm_256_colour, NONE, BOOL, (int)true, false, false, false, "Xterm256Colour", NULL)
CONF_OPTION(true_colour, NONE, BOOL, (int)true, false, false, false, "TrueColour", NULL)
CONF_OPTION(system_colour, NONE, BOOL, (int)false, false, false, false, "UseSystemColours", NULL)
CONF_OPTION(try_palette, NONE, BOOL, (int)false, false, false, false, "TryPalette", NULL)
CONF_OPTION(bold_style, NONE, INT, 2, false, false, false, "BoldAsColour", &conf_enum_bold_style)
/*
 * Subkeys in this setting are indexed based on the CONF_COLOUR_*
 * enum values in putty.h. But each subkey identifies just one
 * component of the RGB value. Subkey 3*a+b identifies colour #a,
 * channel #b, where channels 0,1,2 mean R,G,B respectively.
 *
 * Values are 8-bit integers.
 */
/* necessary for mappings */
CONF_OPTION(colours, INT, INT, 0, true, true, false, NULL, NULL)

/* Selection options */
CONF_OPTION(mouse_is_xterm, NONE, INT, 0, false, false, false, "MouseIsXterm", &conf_enum_mouse_buttons)
CONF_OPTION(rect_select, NONE, BOOL, (int)false, false, false, false, "RectSelect", NULL)
CONF_OPTION(paste_controls, NONE, BOOL, (int)false, false, false, false, "PasteControls", NULL)
CONF_OPTION(rawcnp, NONE, BOOL, (int)false, false, false, false, "RawCNP", NULL)
CONF_OPTION(utf8linedraw, NONE, BOOL, (int)false, false, false, false, "UTF8linedraw", NULL)
CONF_OPTION(rtf_paste, NONE, BOOL, (int)false, false, false, false, "PasteRTF", NULL)
CONF_OPTION(mouse_override, NONE, BOOL, (int)true, false, false, false, "MouseOverride", NULL)
/* ASCII character codes (literally, just 00-7F) */
/* arbitrary equivalence-class value for that char */
/* necessary for mappings */
CONF_OPTION(wordness, INT, INT, 0, true, true, false, NULL, NULL)
/*
 * What clipboard (if any) to copy text to as soon as it's
 * selected with the mouse.
 */
/* platform-dependent bool-valued
 * macro */
CONF_OPTION(mouseautocopy, NONE, BOOL, (int)CLIPUI_DEFAULT_AUTOCOPY, false, false, false, "MouseAutocopy", NULL)
/* clipboard used by one-mouse-click paste actions */
/*
 * SAVE_KEYWORD("MousePaste"), but loading and saving is done by
 * custom code, because the saved value is a string, and also sets
 * CONF_mousepaste_custom
 */
CONF_OPTION(mousepaste, NONE, INT, 0, true, true, false, NULL, NULL)
/* clipboard used by Ctrl+Ins and Shift+Ins */
/*
 * SAVE_KEYWORD("CtrlShiftIns"), but loading and saving is done by
 * custom code, because the saved value is a string, and also sets
 * CONF_ctrlshiftins_custom
 */
CONF_OPTION(ctrlshiftins, NONE, INT, 0, true, true, false, NULL, NULL)
/* clipboard used by Ctrl+Shift+C and Ctrl+Shift+V */
/*
 * SAVE_KEYWORD("CtrlShiftCV"), but loading and saving is done by
 * custom code, because the saved value is a string, and also sets
 * CONF_ctrlshiftcv_custom
 */
CONF_OPTION(ctrlshiftcv, NONE, INT, 0, true, true, false, NULL, NULL)
/* Custom clipboard name if CONF_mousepaste is set to CLIPUI_CUSTOM */
/*
 * Loading and saving is handled by custom code in conjunction
 * with CONF_mousepaste
 */
CONF_OPTION(mousepaste_custom, NONE, STR, 0, true, true, false, NULL, NULL)
/* Custom clipboard name if CONF_ctrlshiftins is set to CLIPUI_CUSTOM */
/*
 * Loading and saving is handled by custom code in conjunction
 * with CONF_ctrlshiftins
 */
CONF_OPTION(ctrlshiftins_custom, NONE, STR, 0, true, true, false, NULL, NULL)
/* Custom clipboard name if CONF_ctrlshiftcv is set to CLIPUI_CUSTOM */
/*
 * Loading and saving is handled by custom code in conjunction
 * with CONF_ctrlshiftcv
 */
CONF_OPTION(ctrlshiftcv_custom, NONE, STR, 0, true, true, false, NULL, NULL)

/* Character-set translation */
CONF_OPTION(vtmode, NONE, INT, VT_UNICODE, false, false, false, "FontVTMode", &conf_enum_line_drawing)
CONF_OPTION(line_codepage, NONE, STR, (int)"", false, false, false, "LineCodePage", NULL)
CONF_OPTION(cjk_ambig_wide, NONE, BOOL, (int)false, false, false, false, "CJKAmbigWide", NULL)
CONF_OPTION(utf8_override, NONE, BOOL, (int)true, false, false, false, "UTF8Override", NULL)
CONF_OPTION(xlat_capslockcyr, NONE, BOOL, (int)false, false, false, false, "CapsLockCyr", NULL)

/* X11 forwarding */
CONF_OPTION(x11_forward, NONE, BOOL, (int)false, false, false, false, "X11Forward", NULL)
CONF_OPTION(x11_display, NONE, STR, (int)"", false, false, false, "X11Display", NULL)
CONF_OPTION(x11_auth, NONE, INT, X11_MIT, false, false, false, "X11AuthType", &conf_enum_x11_auth)
CONF_OPTION(xauthfile, NONE, FILENAME, 0, false, false, false, "X11AuthFile", NULL)

/* Port forwarding */
/* accept conns from hosts other than localhost */
CONF_OPTION(lport_acceptall, NONE, BOOL, (int)false, false, false, false, "LocalPortAcceptAll", NULL)
/* same for remote forwarded ports */
CONF_OPTION(rport_acceptall, NONE, BOOL, (int)false, false, false, false, "RemotePortAcceptAll", NULL)
/*
 * Subkeys for 'portfwd' can have the following forms:
 *
 *   [LR]localport
 *   [LR]localaddr:localport
 *
 * Dynamic forwardings are indicated by an 'L' key, and the
 * special value "D". For all other forwardings, the value should
 * be of the form 'host:port'.
 */
/* necessary for mappings */
CONF_OPTION(portfwd, STR, STR, 0, true, true, false, NULL, NULL)

/* SSH bug compatibility modes. All FORCE_ON/FORCE_OFF/AUTO */
CONF_OPTION(sshbug_ignore1, NONE, INT, AUTO, false, false, false, "BugIgnore1", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_plainpw1, NONE, INT, AUTO, false, false, false, "BugPlainPW1", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_rsa1, NONE, INT, AUTO, false, false, false, "BugRSA1", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_ignore2, NONE, INT, AUTO, false, false, false, "BugIgnore2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_derivekey2, NONE, INT, AUTO, false, false, false, "BugDeriveKey2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_rsapad2, NONE, INT, AUTO, false, false, false, "BugRSAPad2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_pksessid2, NONE, INT, AUTO, false, false, false, "BugPKSessID2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_rekey2, NONE, INT, AUTO, false, false, false, "BugRekey2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_maxpkt2, NONE, INT, AUTO, false, false, false, "BugMaxPkt2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_oldgex2, NONE, INT, AUTO, false, false, false, "BugOldGex2", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_winadj, NONE, INT, AUTO, false, false, false, "BugWinadj", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_chanreq, NONE, INT, AUTO, false, false, false, "BugChanReq", &conf_enum_auto_off_on)
CONF_OPTION(sshbug_dropstart, NONE, INT, FORCE_OFF, false, false, false, "BugDropStart", &conf_enum_off1_on2)
CONF_OPTION(sshbug_filter_kexinit, NONE, INT, FORCE_OFF, false, false, false, "BugFilterKexinit", &conf_enum_off1_on2)
CONF_OPTION(sshbug_rsa_sha2_cert_userauth, NONE, INT, AUTO, false, false, false, "BugRSASHA2CertUserauth", &conf_enum_auto_off_on)
/* there was an earlier keyword called "BuggyMAC" */
CONF_OPTION(sshbug_hmac2, NONE, INT, AUTO, false, true, false, "BugHMAC2", &conf_enum_auto_off_on)

/* Options for Unix. Should split out into platform-dependent part. */
/* used by Unix pterm */
CONF_OPTION(stamp_utmp, NONE, BOOL, (int)true, false, false, false, "StampUtmp", NULL)
/* used by Unix pterm */
CONF_OPTION(login_shell, NONE, BOOL, (int)true, false, false, false, "LoginShell", NULL)
CONF_OPTION(scrollbar_on_left, NONE, BOOL, (int)false, false, false, false, "ScrollbarOnLeft", NULL)
CONF_OPTION(shadowbold, NONE, BOOL, (int)false, false, false, false, "ShadowBold", NULL)
CONF_OPTION(boldfont, NONE, FONT, 0, false, false, false, "BoldFont", NULL)
CONF_OPTION(widefont, NONE, FONT, 0, false, false, false, "WideFont", NULL)
CONF_OPTION(wideboldfont, NONE, FONT, 0, false, false, false, "WideBoldFont", NULL)
/* in pixels */
CONF_OPTION(shadowboldoffset, NONE, INT, 1, false, false, false, "ShadowBoldOffset", NULL)
CONF_OPTION(crhaslf, NONE, BOOL, (int)false, false, false, false, "CRImpliesLF", NULL)
CONF_OPTION(winclass, NONE, STR, (int)"", false, false, false, "WindowClass", NULL)

/* WINSCP BEGIN */
CONF_OPTION(connect_timeout, NONE, INT, 0, false, false, true, NULL, NULL)
CONF_OPTION(sndbuf, NONE, INT, 0, false, false, true, NULL, NULL)
CONF_OPTION(srcaddr, NONE, STR, (int)"", false, false, true, NULL, NULL)
CONF_OPTION(force_remote_cmd2, NONE, BOOL, (int)false, false, false, true, NULL, NULL)
CONF_OPTION(change_password, NONE, BOOL, (int)false, false, false, true, NULL, NULL)
/* WINSCP END */
