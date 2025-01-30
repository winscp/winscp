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

CONF_OPTION(host,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("HostName"),
)
CONF_OPTION(port,
    VALUE_TYPE(INT),
    SAVE_KEYWORD("PortNumber"),
    LOAD_CUSTOM, /* default value depends on the value of CONF_protocol */
)
CONF_OPTION(protocol,
    VALUE_TYPE(INT), /* PROT_SSH, PROT_TELNET etc */
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Notionally SAVE_KEYWORD("Protocol"), but saving/loading is handled by
     * custom code because the stored value is a string representation
     * of the protocol name.
     */
)
CONF_OPTION(addressfamily,
    VALUE_TYPE(INT),
    DEFAULT_INT(ADDRTYPE_UNSPEC),
    SAVE_KEYWORD("AddressFamily"),
    STORAGE_ENUM(addressfamily),
)
CONF_OPTION(close_on_exit,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("CloseOnExit"),
    STORAGE_ENUM(off_auto_on),
)
CONF_OPTION(warn_on_close,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("WarnOnClose"),
)
CONF_OPTION(ping_interval,
    VALUE_TYPE(INT), /* in seconds */
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Saving/loading is handled by custom code because for historical
     * reasons this value corresponds to two save keywords,
     * "PingInterval" (measured in minutes) and "PingIntervalSecs"
     * (measured in seconds), which are added together on loading.
     * Rationale: the value was once measured in minutes, and the
     * seconds field was added later.
     */
)
CONF_OPTION(tcp_nodelay,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("TCPNoDelay"),
)
CONF_OPTION(tcp_keepalives,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("TCPKeepalives"),
)
CONF_OPTION(loghost, /* logical host being contacted, for host key check */
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("LogHost"),
)

/* Proxy options */
CONF_OPTION(proxy_exclude_list,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("ProxyExcludeList"),
)
CONF_OPTION(proxy_dns,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("ProxyDNS"),
    STORAGE_ENUM(off_auto_on),
)
CONF_OPTION(even_proxy_localhost,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ProxyLocalhost"),
)
CONF_OPTION(proxy_type,
    VALUE_TYPE(INT), /* PROXY_NONE, PROXY_SOCKS4, ... */
    STORAGE_ENUM(proxy_type),
    SAVE_KEYWORD("ProxyMethod"),
    LOAD_CUSTOM,
    /*
     * Custom load code: there was an earlier keyword "ProxyType"
     * using a different enumeration, in which SOCKS4 and SOCKS5
     * shared a value, and a second keyword "ProxySOCKSVersion"
     * disambiguated.
     */
)
CONF_OPTION(proxy_host,
    VALUE_TYPE(STR),
    DEFAULT_STR("proxy"),
    SAVE_KEYWORD("ProxyHost"),
)
CONF_OPTION(proxy_port,
    VALUE_TYPE(INT),
    DEFAULT_INT(80),
    SAVE_KEYWORD("ProxyPort"),
)
CONF_OPTION(proxy_username,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("ProxyUsername"),
)
CONF_OPTION(proxy_password,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("ProxyPassword"),
)
CONF_OPTION(proxy_telnet_command,
    VALUE_TYPE(STR),
    DEFAULT_STR("connect %host %port\\n"),
    SAVE_KEYWORD("ProxyTelnetCommand"),
)
CONF_OPTION(proxy_log_to_term,
    VALUE_TYPE(INT),
    DEFAULT_INT(FORCE_OFF),
    SAVE_KEYWORD("ProxyLogToTerm"),
    STORAGE_ENUM(on_off_auto),
)

/* SSH options */
CONF_OPTION(remote_cmd,
    VALUE_TYPE(STR_AMBI),
    DEFAULT_STR(""),
    SAVE_KEYWORD("RemoteCommand"),
)
CONF_OPTION(remote_cmd2,
    /*
     * Fallback command to try to run if remote_cmd fails. Only set
     * internally by PSCP and PSFTP (so that they can try multiple
     * methods of running an SFTP server at the remote end); never set
     * by user configuration, or loaded or saved.
     */
    VALUE_TYPE(STR_AMBI),
    DEFAULT_STR(""),
    NOT_SAVED,
)
CONF_OPTION(nopty,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoPTY"),
)
CONF_OPTION(compression,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("Compression"),
)
CONF_OPTION(ssh_kexlist,
    SUBKEY_TYPE(INT), /* indices in preference order: 0,...,KEX_MAX-1
                       * (lower is more preferred) */
    VALUE_TYPE(INT),  /* KEX_* enum values */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for preference lists */
)
CONF_OPTION(ssh_hklist,
    SUBKEY_TYPE(INT), /* indices in preference order: 0,...,HK_MAX-1
                       * (lower is more preferred) */
    VALUE_TYPE(INT),  /* HK_* enum values */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for preference lists */
)
CONF_OPTION(ssh_prefer_known_hostkeys,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("PreferKnownHostKeys"),
)
CONF_OPTION(ssh_rekey_time,
    VALUE_TYPE(INT), /* in minutes */
    DEFAULT_INT(60),
    SAVE_KEYWORD("RekeyTime"),
)
CONF_OPTION(ssh_rekey_data,
    VALUE_TYPE(STR), /* string encoding e.g. "100K", "2M", "1G" */
    DEFAULT_STR("1G"),
    SAVE_KEYWORD("RekeyBytes"),
)
CONF_OPTION(tryagent,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("TryAgent"),
)
CONF_OPTION(agentfwd,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("AgentFwd"),
)
CONF_OPTION(change_username, /* allow username switching in SSH-2 */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ChangeUsername"),
)
CONF_OPTION(ssh_cipherlist,
    SUBKEY_TYPE(INT), /* indices in preference order: 0,...,CIPHER_MAX-1
                       * (lower is more preferred) */
    VALUE_TYPE(INT),  /* CIPHER_* enum values */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for preference lists */
)
CONF_OPTION(keyfile,
    VALUE_TYPE(FILENAME),
    SAVE_KEYWORD("PublicKeyFile"),
)
CONF_OPTION(detached_cert,
    VALUE_TYPE(FILENAME),
    SAVE_KEYWORD("DetachedCertificate"),
)
CONF_OPTION(auth_plugin,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("AuthPlugin"),
)
CONF_OPTION(sshprot,
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
    VALUE_TYPE(INT),
    DEFAULT_INT(3),
    SAVE_KEYWORD("SshProt"),
    STORAGE_ENUM(ssh_protocol),
)
CONF_OPTION(ssh_simple,
    /*
     * This means that we promise never to open any channel other
     * than the main one, which means it can safely use a very large
     * window in SSH-2.
     *
     * Only ever set internally by file transfer tools; never set by
     * user configuration, or loaded or saved.
     */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    NOT_SAVED,
)
CONF_OPTION(ssh_connection_sharing,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ConnectionSharing"),
)
CONF_OPTION(ssh_connection_sharing_upstream,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("ConnectionSharingUpstream"),
)
CONF_OPTION(ssh_connection_sharing_downstream,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("ConnectionSharingDownstream"),
)
CONF_OPTION(ssh_manual_hostkeys,
    /*
     * Manually configured host keys to accept regardless of the state
     * of the host key cache.
     *
     * This is conceptually a set rather than a dictionary: every
     * value in this map is the empty string, and the set of subkeys
     * that exist is the important data.
     */
    SUBKEY_TYPE(STR),
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)
CONF_OPTION(ssh2_des_cbc, /* "des-cbc" unrecommended SSH-2 cipher */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SSH2DES"),
)
CONF_OPTION(ssh_no_userauth, /* bypass "ssh-userauth" (SSH-2 only) */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SshNoAuth"),
)
CONF_OPTION(ssh_no_trivial_userauth, /* disable trivial types of auth */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SshNoTrivialAuth"),
)
CONF_OPTION(ssh_show_banner, /* show USERAUTH_BANNERs (SSH-2 only) */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("SshBanner"),
)
CONF_OPTION(try_tis_auth,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("AuthTIS"),
)
CONF_OPTION(try_ki_auth,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("AuthKI"),
)
CONF_OPTION(try_gssapi_auth, /* attempt gssapi via ssh userauth */
    VALUE_TYPE(BOOL),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifndef NO_GSSAPI */
)
CONF_OPTION(try_gssapi_kex, /* attempt gssapi via ssh kex */
    VALUE_TYPE(BOOL),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifndef NO_GSSAPI */
)
CONF_OPTION(gssapifwd, /* forward tgt via gss */
    VALUE_TYPE(BOOL),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifndef NO_GSSAPI */
)
CONF_OPTION(gssapirekey, /* KEXGSS refresh interval (mins) */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifndef NO_GSSAPI */
)
CONF_OPTION(ssh_gsslist,
    SUBKEY_TYPE(INT), /* indices in preference order: 0,...,ngsslibs
                       * (lower is more preferred; ngsslibs is a platform-
                       * dependent value) */
    VALUE_TYPE(INT),  /* indices of GSSAPI lib types (platform-dependent) */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for preference lists, also this
                               * setting is under #ifndef NO_GSSAPI */
)
CONF_OPTION(ssh_gss_custom,
    VALUE_TYPE(FILENAME),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifndef NO_GSSAPI */
)
CONF_OPTION(ssh_subsys, /* run a subsystem rather than a command */
    /*
     * Only set internally by PSCP and PSFTP; never set by user
     * configuration, or loaded or saved.
     */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    NOT_SAVED,
)
CONF_OPTION(ssh_subsys2, /* fallback to go with remote_cmd2 */
    /*
     * Only set internally by PSCP and PSFTP; never set by user
     * configuration, or loaded or saved.
     */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    NOT_SAVED,
)
CONF_OPTION(ssh_no_shell, /* avoid running a shell */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SshNoShell"),
)
CONF_OPTION(ssh_nc_host, /* host to connect to in `nc' mode */
    /*
     * Only set by the '-nc' command-line option and by the SSH proxy
     * code. There's no GUI config option for this, and therefore it's
     * also never loaded or saved.
     */
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    NOT_SAVED,
)
CONF_OPTION(ssh_nc_port, /* port to connect to in `nc' mode */
    /*
     * Only set by the '-nc' command-line option and by the SSH proxy
     * code. There's no GUI config option for this, and therefore it's
     * also never loaded or saved.
     */
    VALUE_TYPE(INT),
    DEFAULT_INT(0),
    NOT_SAVED,
)

/* Telnet options */
CONF_OPTION(termtype,
    VALUE_TYPE(STR),
    DEFAULT_STR("xterm"),
    SAVE_KEYWORD("TerminalType"),
)
CONF_OPTION(termspeed,
    VALUE_TYPE(STR),
    DEFAULT_STR("38400,38400"),
    SAVE_KEYWORD("TerminalSpeed"),
)
CONF_OPTION(ttymodes,
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
    SUBKEY_TYPE(STR),
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)
CONF_OPTION(environmt,
    SUBKEY_TYPE(STR), /* environment variable name */
    VALUE_TYPE(STR),  /* environment variable value */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)
CONF_OPTION(username,
    VALUE_TYPE(STR_AMBI),
    DEFAULT_STR(""),
    SAVE_KEYWORD("UserName"),
)
CONF_OPTION(username_from_env,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("UserNameFromEnvironment"),
)
CONF_OPTION(localusername,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("LocalUserName"),
)
CONF_OPTION(rfc_environ,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("RFCEnviron"),
)
CONF_OPTION(passive_telnet,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("PassiveTelnet"),
)

/* Serial port options */
CONF_OPTION(serline,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("SerialLine"),
)
CONF_OPTION(serspeed,
    VALUE_TYPE(INT),
    DEFAULT_INT(9600),
    SAVE_KEYWORD("SerialSpeed"),
)
CONF_OPTION(serdatabits,
    VALUE_TYPE(INT),
    DEFAULT_INT(8),
    SAVE_KEYWORD("SerialDataBits"),
)
CONF_OPTION(serstopbits,
    VALUE_TYPE(INT),
    DEFAULT_INT(2),
    SAVE_KEYWORD("SerialStopHalfbits"),
)
CONF_OPTION(serparity,
    VALUE_TYPE(INT),
    DEFAULT_INT(SER_PAR_NONE),
    SAVE_KEYWORD("SerialParity"),
    STORAGE_ENUM(serparity),
)
CONF_OPTION(serflow,
    VALUE_TYPE(INT),
    DEFAULT_INT(SER_FLOW_XONXOFF),
    SAVE_KEYWORD("SerialFlowControl"),
    STORAGE_ENUM(serflow),
)

/* SUPDUP options */
CONF_OPTION(supdup_location,
    VALUE_TYPE(STR),
    DEFAULT_STR("The Internet"),
    SAVE_KEYWORD("SUPDUPLocation"),
)
CONF_OPTION(supdup_ascii_set,
    VALUE_TYPE(INT),
    DEFAULT_INT(SUPDUP_CHARSET_ASCII),
    SAVE_KEYWORD("SUPDUPCharset"),
    STORAGE_ENUM(supdup_charset),
)
CONF_OPTION(supdup_more,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SUPDUPMoreProcessing"),
)
CONF_OPTION(supdup_scroll,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SUPDUPScrolling"),
)

/* Keyboard options */
CONF_OPTION(bksp_is_delete,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("BackspaceIsDelete"),
)
CONF_OPTION(rxvt_homeend,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("RXVTHomeEnd"),
)
CONF_OPTION(funky_type,
    VALUE_TYPE(INT),
    DEFAULT_INT(FUNKY_TILDE),
    SAVE_KEYWORD("LinuxFunctionKeys"),
    STORAGE_ENUM(funky_type),
)
CONF_OPTION(sharrow_type,
    VALUE_TYPE(INT),
    DEFAULT_INT(SHARROW_APPLICATION),
    SAVE_KEYWORD("ShiftedArrowKeys"),
    STORAGE_ENUM(sharrow_type),
)
CONF_OPTION(no_applic_c, /* totally disable app cursor keys */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoApplicationCursors"),
)
CONF_OPTION(no_applic_k, /* totally disable app keypad */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoApplicationKeys"),
)
CONF_OPTION(no_mouse_rep, /* totally disable mouse reporting */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoMouseReporting"),
)
CONF_OPTION(no_remote_resize, /* disable remote resizing */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoRemoteResize"),
)
CONF_OPTION(no_alt_screen, /* disable alternate screen */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoAltScreen"),
)
CONF_OPTION(no_remote_wintitle, /* disable remote retitling */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoRemoteWinTitle"),
)
CONF_OPTION(no_remote_clearscroll, /* disable ESC[3J */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoRemoteClearScroll"),
)
CONF_OPTION(no_dbackspace, /* disable destructive backspace */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoDBackspace"),
)
CONF_OPTION(no_remote_charset, /* disable remote charset config */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NoRemoteCharset"),
)
CONF_OPTION(remote_qtitle_action, /* handling of remote window title queries */
    VALUE_TYPE(INT),
    STORAGE_ENUM(remote_qtitle_action),
    SAVE_KEYWORD("RemoteQTitleAction"),
    LOAD_CUSTOM, /* older versions had a boolean "NoRemoteQTitle"
                  * before we ended up with three options */
)
CONF_OPTION(app_cursor,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ApplicationCursorKeys"),
)
CONF_OPTION(app_keypad,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ApplicationKeypad"),
)
CONF_OPTION(nethack_keypad,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("NetHackKeypad"),
)
CONF_OPTION(telnet_keyboard,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("TelnetKey"),
)
CONF_OPTION(telnet_newline,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("TelnetRet"),
)
CONF_OPTION(alt_f4, /* is it special? */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("AltF4"),
)
CONF_OPTION(alt_space, /* is it special? */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("AltSpace"),
)
CONF_OPTION(alt_only, /* is it special? */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("AltOnly"),
)
CONF_OPTION(localecho,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("LocalEcho"),
    STORAGE_ENUM(on_off_auto),
)
CONF_OPTION(localedit,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("LocalEdit"),
    STORAGE_ENUM(on_off_auto),
)
CONF_OPTION(alwaysontop,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("AlwaysOnTop"),
)
CONF_OPTION(fullscreenonaltenter,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("FullScreenOnAltEnter"),
)
CONF_OPTION(scroll_on_key,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ScrollOnKey"),
)
CONF_OPTION(scroll_on_disp,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("ScrollOnDisp"),
)
CONF_OPTION(erase_to_scrollback,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("EraseToScrollback"),
)
CONF_OPTION(compose_key,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ComposeKey"),
)
CONF_OPTION(ctrlaltkeys,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("CtrlAltKeys"),
)
CONF_OPTION(osx_option_meta,
    VALUE_TYPE(BOOL),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifdef OSX_META_KEY_CONFIG */
)
CONF_OPTION(osx_command_meta,
    VALUE_TYPE(BOOL),
    LOAD_CUSTOM, SAVE_CUSTOM, /* under #ifdef OSX_META_KEY_CONFIG */
)
CONF_OPTION(wintitle, /* initial window title */
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("WinTitle"),
)
/* Terminal options */
CONF_OPTION(savelines,
    VALUE_TYPE(INT),
    DEFAULT_INT(2000),
    SAVE_KEYWORD("ScrollbackLines"),
)
CONF_OPTION(dec_om,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("DECOriginMode"),
)
CONF_OPTION(wrap_mode,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("AutoWrapMode"),
)
CONF_OPTION(lfhascr,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("LFImpliesCR"),
)
CONF_OPTION(cursor_type,
    VALUE_TYPE(INT),
    DEFAULT_INT(0),
    SAVE_KEYWORD("CurType"),
    STORAGE_ENUM(cursor_type),
)
CONF_OPTION(blink_cur,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("BlinkCur"),
)
CONF_OPTION(beep,
    VALUE_TYPE(INT),
    DEFAULT_INT(BELL_DEFAULT),
    SAVE_KEYWORD("Beep"),
    STORAGE_ENUM(beep),
)
CONF_OPTION(beep_ind,
    VALUE_TYPE(INT),
    DEFAULT_INT(B_IND_DISABLED),
    SAVE_KEYWORD("BeepInd"),
    STORAGE_ENUM(beep_indication),
)
CONF_OPTION(bellovl, /* bell overload protection active? */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("BellOverload"),
)
CONF_OPTION(bellovl_n, /* number of bells to cause overload */
    VALUE_TYPE(INT),
    DEFAULT_INT(5),
    SAVE_KEYWORD("BellOverloadN"),
)
CONF_OPTION(bellovl_t, /* time interval for overload (ticks) */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Loading and saving is done in custom code because the format is
     * platform-dependent for historical reasons: on Unix, the stored
     * value is multiplied by 1000. (And since TICKSPERSEC=1000 on
     * that platform, it means the stored value is interpreted in
     * microseconds.)
     */
)
CONF_OPTION(bellovl_s, /* period of silence to re-enable bell (s) */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Loading and saving is done in custom code because the format is
     * platform-dependent for historical reasons: on Unix, the stored
     * value is multiplied by 1000. (And since TICKSPERSEC=1000 on
     * that platform, it means the stored value is interpreted in
     * microseconds.)
     */
)
CONF_OPTION(bell_wavefile,
    VALUE_TYPE(FILENAME),
    SAVE_KEYWORD("BellWaveFile"),
)
CONF_OPTION(scrollbar,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("ScrollBar"),
)
CONF_OPTION(scrollbar_in_fullscreen,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ScrollBarFullScreen"),
)
CONF_OPTION(resize_action,
    VALUE_TYPE(INT),
    DEFAULT_INT(RESIZE_TERM),
    SAVE_KEYWORD("LockSize"),
    STORAGE_ENUM(resize_effect),
)
CONF_OPTION(bce,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("BCE"),
)
CONF_OPTION(blinktext,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("BlinkText"),
)
CONF_OPTION(win_name_always,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("WinNameAlways"),
)
CONF_OPTION(width,
    VALUE_TYPE(INT),
    DEFAULT_INT(80),
    SAVE_KEYWORD("TermWidth"),
)
CONF_OPTION(height,
    VALUE_TYPE(INT),
    DEFAULT_INT(24),
    SAVE_KEYWORD("TermHeight"),
)
CONF_OPTION(font,
    VALUE_TYPE(FONT),
    SAVE_KEYWORD("Font"),
)
CONF_OPTION(font_quality,
    VALUE_TYPE(INT),
    DEFAULT_INT(FQ_DEFAULT),
    SAVE_KEYWORD("FontQuality"),
    STORAGE_ENUM(font_quality),
)
CONF_OPTION(logfilename,
    VALUE_TYPE(FILENAME),
    SAVE_KEYWORD("LogFileName"),
)
CONF_OPTION(logtype,
    VALUE_TYPE(INT),
    DEFAULT_INT(LGTYP_NONE),
    SAVE_KEYWORD("LogType"),
    STORAGE_ENUM(log_type),
)
CONF_OPTION(logxfovr,
    VALUE_TYPE(INT),
    DEFAULT_INT(LGXF_ASK),
    SAVE_KEYWORD("LogFileClash"),
    STORAGE_ENUM(log_to_existing_file),
)
CONF_OPTION(logflush,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("LogFlush"),
)
CONF_OPTION(logheader,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("LogHeader"),
)
CONF_OPTION(logomitpass,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("SSHLogOmitPasswords"),
)
CONF_OPTION(logomitdata,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SSHLogOmitData"),
)
CONF_OPTION(hide_mouseptr,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("HideMousePtr"),
)
CONF_OPTION(sunken_edge,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("SunkenEdge"),
)
CONF_OPTION(window_border,
    VALUE_TYPE(INT), /* in pixels */
    DEFAULT_INT(1),
    SAVE_KEYWORD("WindowBorder"),
)
CONF_OPTION(answerback,
    VALUE_TYPE(STR),
    DEFAULT_STR("PuTTY"),
    SAVE_KEYWORD("Answerback"),
)
CONF_OPTION(printer,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("Printer"),
)
CONF_OPTION(no_arabicshaping,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("DisableArabicShaping"),
)
CONF_OPTION(no_bidi,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("DisableBidi"),
)
CONF_OPTION(no_bracketed_paste,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("DisableBracketedPaste"),
)

/* Colour options */
CONF_OPTION(ansi_colour,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("ANSIColour"),
)
CONF_OPTION(xterm_256_colour,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("Xterm256Colour"),
)
CONF_OPTION(true_colour,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("TrueColour"),
)
CONF_OPTION(system_colour,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("UseSystemColours"),
)
CONF_OPTION(try_palette,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("TryPalette"),
)
CONF_OPTION(bold_style,
    VALUE_TYPE(INT),
    DEFAULT_INT(2),
    SAVE_KEYWORD("BoldAsColour"),
    STORAGE_ENUM(bold_style),
)
CONF_OPTION(colours,
    /*
     * Subkeys in this setting are indexed based on the CONF_COLOUR_*
     * enum values in putty.h. But each subkey identifies just one
     * component of the RGB value. Subkey 3*a+b identifies colour #a,
     * channel #b, where channels 0,1,2 mean R,G,B respectively.
     *
     * Values are 8-bit integers.
     */
    SUBKEY_TYPE(INT),
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)

/* Selection options */
CONF_OPTION(mouse_is_xterm,
    VALUE_TYPE(INT),
    DEFAULT_INT(0),
    SAVE_KEYWORD("MouseIsXterm"),
    STORAGE_ENUM(mouse_buttons),
)
CONF_OPTION(rect_select,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("RectSelect"),
)
CONF_OPTION(paste_controls,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("PasteControls"),
)
CONF_OPTION(rawcnp,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("RawCNP"),
)
CONF_OPTION(utf8linedraw,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("UTF8linedraw"),
)
CONF_OPTION(rtf_paste,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("PasteRTF"),
)
CONF_OPTION(mouse_override,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("MouseOverride"),
)
CONF_OPTION(wordness,
    SUBKEY_TYPE(INT), /* ASCII character codes (literally, just 00-7F) */
    VALUE_TYPE(INT),  /* arbitrary equivalence-class value for that char */
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)
CONF_OPTION(mouseautocopy,
    /*
     * What clipboard (if any) to copy text to as soon as it's
     * selected with the mouse.
     */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(CLIPUI_DEFAULT_AUTOCOPY), /* platform-dependent bool-valued
                                            * macro */
    SAVE_KEYWORD("MouseAutocopy"),
)
CONF_OPTION(mousepaste, /* clipboard used by one-mouse-click paste actions */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * SAVE_KEYWORD("MousePaste"), but loading and saving is done by
     * custom code, because the saved value is a string, and also sets
     * CONF_mousepaste_custom
     */
)
CONF_OPTION(ctrlshiftins, /* clipboard used by Ctrl+Ins and Shift+Ins */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * SAVE_KEYWORD("CtrlShiftIns"), but loading and saving is done by
     * custom code, because the saved value is a string, and also sets
     * CONF_ctrlshiftins_custom
     */
)
CONF_OPTION(ctrlshiftcv, /* clipboard used by Ctrl+Shift+C and Ctrl+Shift+V */
    VALUE_TYPE(INT),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * SAVE_KEYWORD("CtrlShiftCV"), but loading and saving is done by
     * custom code, because the saved value is a string, and also sets
     * CONF_ctrlshiftcv_custom
     */
)
CONF_OPTION(mousepaste_custom,
    /* Custom clipboard name if CONF_mousepaste is set to CLIPUI_CUSTOM */
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Loading and saving is handled by custom code in conjunction
     * with CONF_mousepaste
     */
)
CONF_OPTION(ctrlshiftins_custom,
    /* Custom clipboard name if CONF_ctrlshiftins is set to CLIPUI_CUSTOM */
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Loading and saving is handled by custom code in conjunction
     * with CONF_ctrlshiftins
     */
)
CONF_OPTION(ctrlshiftcv_custom,
    /* Custom clipboard name if CONF_ctrlshiftcv is set to CLIPUI_CUSTOM */
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM,
    /*
     * Loading and saving is handled by custom code in conjunction
     * with CONF_ctrlshiftcv
     */
)

/* Character-set translation */
CONF_OPTION(vtmode,
    VALUE_TYPE(INT),
    DEFAULT_INT(VT_UNICODE),
    SAVE_KEYWORD("FontVTMode"),
    STORAGE_ENUM(line_drawing),
)
CONF_OPTION(line_codepage,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("LineCodePage"),
)
CONF_OPTION(cjk_ambig_wide,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("CJKAmbigWide"),
)
CONF_OPTION(utf8_override,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("UTF8Override"),
)
CONF_OPTION(xlat_capslockcyr,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("CapsLockCyr"),
)

/* X11 forwarding */
CONF_OPTION(x11_forward,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("X11Forward"),
)
CONF_OPTION(x11_display,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("X11Display"),
)
CONF_OPTION(x11_auth,
    VALUE_TYPE(INT),
    DEFAULT_INT(X11_MIT),
    SAVE_KEYWORD("X11AuthType"),
    STORAGE_ENUM(x11_auth),
)
CONF_OPTION(xauthfile,
    VALUE_TYPE(FILENAME),
    SAVE_KEYWORD("X11AuthFile"),
)

/* Port forwarding */
CONF_OPTION(lport_acceptall, /* accept conns from hosts other than localhost */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("LocalPortAcceptAll"),
)
CONF_OPTION(rport_acceptall, /* same for remote forwarded ports */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("RemotePortAcceptAll"),
)
CONF_OPTION(portfwd,
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
    SUBKEY_TYPE(STR),
    VALUE_TYPE(STR),
    LOAD_CUSTOM, SAVE_CUSTOM, /* necessary for mappings */
)

/* SSH bug compatibility modes. All FORCE_ON/FORCE_OFF/AUTO */
CONF_OPTION(sshbug_ignore1,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugIgnore1"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_plainpw1,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugPlainPW1"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_rsa1,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugRSA1"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_ignore2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugIgnore2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_derivekey2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugDeriveKey2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_rsapad2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugRSAPad2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_pksessid2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugPKSessID2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_rekey2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugRekey2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_maxpkt2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugMaxPkt2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_oldgex2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugOldGex2"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_winadj,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugWinadj"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_chanreq,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugChanReq"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_dropstart,
    VALUE_TYPE(INT),
    DEFAULT_INT(FORCE_OFF),
    SAVE_KEYWORD("BugDropStart"),
    STORAGE_ENUM(off1_on2),
)
CONF_OPTION(sshbug_filter_kexinit,
    VALUE_TYPE(INT),
    DEFAULT_INT(FORCE_OFF),
    SAVE_KEYWORD("BugFilterKexinit"),
    STORAGE_ENUM(off1_on2),
)
CONF_OPTION(sshbug_rsa_sha2_cert_userauth,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugRSASHA2CertUserauth"),
    STORAGE_ENUM(auto_off_on),
)
CONF_OPTION(sshbug_hmac2,
    VALUE_TYPE(INT),
    DEFAULT_INT(AUTO),
    SAVE_KEYWORD("BugHMAC2"),
    STORAGE_ENUM(auto_off_on),
    LOAD_CUSTOM, /* there was an earlier keyword called "BuggyMAC" */
)

/* Options for Unix. Should split out into platform-dependent part. */
CONF_OPTION(stamp_utmp, /* used by Unix pterm */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("StampUtmp"),
)
CONF_OPTION(login_shell, /* used by Unix pterm */
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(true),
    SAVE_KEYWORD("LoginShell"),
)
CONF_OPTION(scrollbar_on_left,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ScrollbarOnLeft"),
)
CONF_OPTION(shadowbold,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("ShadowBold"),
)
CONF_OPTION(boldfont,
    VALUE_TYPE(FONT),
    SAVE_KEYWORD("BoldFont"),
)
CONF_OPTION(widefont,
    VALUE_TYPE(FONT),
    SAVE_KEYWORD("WideFont"),
)
CONF_OPTION(wideboldfont,
    VALUE_TYPE(FONT),
    SAVE_KEYWORD("WideBoldFont"),
)
CONF_OPTION(shadowboldoffset,
    VALUE_TYPE(INT), /* in pixels */
    DEFAULT_INT(1),
    SAVE_KEYWORD("ShadowBoldOffset"),
)
CONF_OPTION(crhaslf,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    SAVE_KEYWORD("CRImpliesLF"),
)
CONF_OPTION(winclass,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    SAVE_KEYWORD("WindowClass"),
)

/* WINSCP BEGIN */
CONF_OPTION(connect_timeout,
    VALUE_TYPE(INT),
    NOT_SAVED,
)
CONF_OPTION(sndbuf,
    VALUE_TYPE(INT),
    NOT_SAVED,
)
CONF_OPTION(srcaddr,
    VALUE_TYPE(STR),
    DEFAULT_STR(""),
    NOT_SAVED,
)
CONF_OPTION(force_remote_cmd2,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    NOT_SAVED,
)
CONF_OPTION(change_password,
    VALUE_TYPE(BOOL),
    DEFAULT_BOOL(false),
    NOT_SAVED,
)
/* WINSCP END */
