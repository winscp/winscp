/*
 * Define various enumerations used in the storage format of PuTTY's
 * settings.
 *
 * Each CONF_ENUM here defines an identifier that config options can
 * cite using their STORAGE_ENUM property. Within each enum, the
 * VALUE(x,y) records indicate that if the internal value of the
 * integer Conf property in question is x, then it's stored in the
 * saved settings as y.
 *
 * This allows the internal enumerations to be rearranged without
 * breaking storage compatibility, and also allows for the fact that
 * (for embarrassing historical reasons) the tri-state FORCE_OFF /
 * FORCE_ON / AUTO enumeration has several different storage
 * representations depending on the Conf option.
 *
 * VALUE_OBSOLETE(x,y) records only work one way: they indicate how a
 * legacy value in saved session data should be interpreted, but also
 * say that that value should no longer be generated in newly saved
 * data. That is, they indicate that y maps to x, but not that x maps
 * to y.
 */

CONF_ENUM(addressfamily,
    VALUE(ADDRTYPE_UNSPEC, 0),
    VALUE(ADDRTYPE_IPV4, 1),
    VALUE(ADDRTYPE_IPV6, 2),
)

CONF_ENUM(on_off_auto,
    VALUE(FORCE_ON, 0),
    VALUE(FORCE_OFF, 1),
    VALUE(AUTO, 2),
)

CONF_ENUM(off_auto_on,
    VALUE(FORCE_OFF, 0),
    VALUE(AUTO, 1),
    VALUE(FORCE_ON, 2),
)

CONF_ENUM(auto_off_on,
    VALUE(AUTO, 0),
    VALUE(FORCE_OFF, 1),
    VALUE(FORCE_ON, 2),
)

CONF_ENUM(off1_on2,
    VALUE(FORCE_OFF, 1),
    VALUE(FORCE_ON, 2),
)

CONF_ENUM(ssh_protocol,
    VALUE(0, 0),
    VALUE(3, 3),
    /* Save value 1 used to mean 'prefer SSH-1 but allow 2'; 2 vice versa.
     * We no longer support SSH-1 vs SSH-2 agnosticism, so we translate each
     * to the preferred version only. */
    VALUE_OBSOLETE(0, 1),
    VALUE_OBSOLETE(3, 2),
)

CONF_ENUM(serparity,
    VALUE(SER_PAR_NONE, 0),
    VALUE(SER_PAR_ODD, 1),
    VALUE(SER_PAR_EVEN, 2),
    VALUE(SER_PAR_MARK, 3),
    VALUE(SER_PAR_SPACE, 4),
)

CONF_ENUM(serflow,
    VALUE(SER_FLOW_NONE, 0),
    VALUE(SER_FLOW_XONXOFF, 1),
    VALUE(SER_FLOW_RTSCTS, 2),
    VALUE(SER_FLOW_DSRDTR, 3),
)

CONF_ENUM(supdup_charset,
    VALUE(SUPDUP_CHARSET_ASCII, 0),
    VALUE(SUPDUP_CHARSET_ITS, 1),
    VALUE(SUPDUP_CHARSET_WAITS, 2),
)

CONF_ENUM(proxy_type,
    VALUE(PROXY_NONE, 0),
    VALUE(PROXY_SOCKS4, 1),
    VALUE(PROXY_SOCKS5, 2),
    VALUE(PROXY_HTTP, 3),
    VALUE(PROXY_TELNET, 4),
    VALUE(PROXY_CMD, 5),
    VALUE(PROXY_SSH_TCPIP, 6),
    VALUE(PROXY_SSH_EXEC, 7),
    VALUE(PROXY_SSH_SUBSYSTEM, 8),
)

CONF_ENUM(old_proxy_type,
    VALUE(PROXY_NONE, 0),
    VALUE(PROXY_HTTP, 1),
    VALUE(PROXY_SOCKS5, 2), /* really, both SOCKS 4 and 5 */
    VALUE(PROXY_TELNET, 3),
    VALUE(PROXY_CMD, 4),
)

CONF_ENUM(funky_type,
    VALUE(FUNKY_TILDE, 0),
    VALUE(FUNKY_LINUX, 1),
    VALUE(FUNKY_XTERM, 2),
    VALUE(FUNKY_VT400, 3),
    VALUE(FUNKY_VT100P, 4),
    VALUE(FUNKY_SCO, 5),
    VALUE(FUNKY_XTERM_216, 6),
)

CONF_ENUM(sharrow_type,
    VALUE(SHARROW_APPLICATION, 0),
    VALUE(SHARROW_BITMAP, 1),
)

CONF_ENUM(remote_qtitle_action,
    VALUE(TITLE_NONE, 0),
    VALUE(TITLE_EMPTY, 1),
    VALUE(TITLE_REAL, 2),
)

CONF_ENUM(cursor_type,
    VALUE(CURSOR_BLOCK, 0),
    VALUE(CURSOR_UNDERLINE, 1),
    VALUE(CURSOR_VERTICAL_LINE, 2),
)

CONF_ENUM(beep,
    VALUE(BELL_DISABLED, 0),
    VALUE(BELL_DEFAULT, 1),
    VALUE(BELL_VISUAL, 2),
    VALUE(BELL_WAVEFILE, 3),
    VALUE(BELL_PCSPEAKER, 4),
)

CONF_ENUM(beep_indication,
    VALUE(B_IND_DISABLED, 0),
    VALUE(B_IND_FLASH, 1),
    VALUE(B_IND_STEADY, 2),
)

CONF_ENUM(resize_effect,
    VALUE(RESIZE_TERM, 0),
    VALUE(RESIZE_DISABLED, 1),
    VALUE(RESIZE_FONT, 2),
    VALUE(RESIZE_EITHER, 3),
)

CONF_ENUM(font_quality,
    VALUE(FQ_DEFAULT, 0),
    VALUE(FQ_ANTIALIASED, 1),
    VALUE(FQ_NONANTIALIASED, 2),
    VALUE(FQ_CLEARTYPE, 3),
)

CONF_ENUM(log_type,
    VALUE(LGTYP_NONE, 0),
    VALUE(LGTYP_ASCII, 1),
    VALUE(LGTYP_DEBUG, 2),
    VALUE(LGTYP_PACKETS, 3),
    VALUE(LGTYP_SSHRAW, 4),
)

CONF_ENUM(log_to_existing_file,
    VALUE(LGXF_OVR, 1),
    VALUE(LGXF_APN, 0),
    VALUE(LGXF_ASK, -1),
)

CONF_ENUM(bold_style,
    VALUE(BOLD_STYLE_FONT, 0),
    VALUE(BOLD_STYLE_COLOUR, 1),
    VALUE(BOLD_STYLE_FONT | BOLD_STYLE_COLOUR, 2),
)

CONF_ENUM(mouse_buttons,
    VALUE(MOUSE_COMPROMISE, 0),
    VALUE(MOUSE_XTERM, 1),
    VALUE(MOUSE_WINDOWS, 2),
)

CONF_ENUM(line_drawing,
    VALUE(VT_XWINDOWS, 0),
    VALUE(VT_OEMANSI, 1),
    VALUE(VT_OEMONLY, 2),
    VALUE(VT_POORMAN, 3),
    VALUE(VT_UNICODE, 4),
)

CONF_ENUM(x11_auth,
    VALUE(X11_NO_AUTH, 0),
    VALUE(X11_MIT, 1),
    VALUE(X11_XDM, 2),
)
