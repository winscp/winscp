#include <windows.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "putty.h"

/*
 * Some command-line parameters need to be saved up until after
 * we've loaded the saved session which will form the basis of our
 * eventual running configuration. For this we use the macro
 * SAVEABLE, which notices if the `need_save' parameter is set and
 * saves the parameter and value on a list.
 * 
 * We also assign priorities to saved parameters, just to slightly
 * ameliorate silly ordering problems. For example, if you specify
 * a saved session to load, it will be loaded _before_ all your
 * local modifications such as -L are evaluated; and if you specify
 * a protocol and a port, the protocol is set up first so that the
 * port can override its choice of port number.
 */

#define NPRIORITIES 3

struct cmdline_saved_param {
    char *p, *value;
};
struct cmdline_saved_param_set {
    struct cmdline_saved_param *params;
    int nsaved, savesize;
};

/*
 * C guarantees this structure will be initialised to all zero at
 * program start, which is exactly what we want.
 */
static struct cmdline_saved_param_set saves[NPRIORITIES];

static void cmdline_save_param(char *p, char *value, int pri)
{
    if (saves[pri].nsaved >= saves[pri].savesize) {
	saves[pri].savesize = saves[pri].nsaved + 32;
	saves[pri].params =
	    srealloc(saves[pri].params,
		     saves[pri].savesize*sizeof(*saves[pri].params));
    }
    saves[pri].params[saves[pri].nsaved].p = p;
    saves[pri].params[saves[pri].nsaved].value = value;
    saves[pri].nsaved++;
}

#define SAVEABLE(pri) do { \
    if (need_save) { cmdline_save_param(p, value, pri); return ret; } \
} while (0)

char *cmdline_password = NULL;

static int cmdline_get_line(const char *prompt, char *str,
                            int maxlen, int is_pw)
{
    static int tried_once = 0;

    assert(is_pw && cmdline_password);

    if (tried_once) {
	return 0;
    } else {
	strncpy(str, cmdline_password, maxlen);
	str[maxlen - 1] = '\0';
	tried_once = 1;
	return 1;
    }
}

/*
 * Here we have a flags word which describes the capabilities of
 * the particular tool on whose behalf we're running. We will
 * refuse certain command-line options if a particular tool
 * inherently can't do anything sensible. For example, the file
 * transfer tools (psftp, pscp) can't do a great deal with protocol
 * selections (ever tried running scp over telnet?) or with port
 * forwarding (even if it wasn't a hideously bad idea, they don't
 * have the select() infrastructure to make them work).
 */
int cmdline_tooltype = 0;

static int cmdline_check_unavailable(int flag, char *p)
{
    if (cmdline_tooltype & flag) {
	cmdline_error("option \"%s\" not available in this tool", p);
	return 1;
    }
    return 0;
}

#define UNAVAILABLE_IN(flag) do { \
    if (cmdline_check_unavailable(flag, p)) return ret; \
} while (0)

/*
 * Process a standard command-line parameter. `p' is the parameter
 * in question; `value' is the subsequent element of argv, which
 * may or may not be required as an operand to the parameter.
 * Return value is 2 if both arguments were used; 1 if only p was
 * used; 0 if the parameter wasn't one we recognised; -2 if it
 * should have been 2 but value was NULL.
 */

#define RETURN(x) do { \
    if ((x) == 2 && !value) return -2; \
    ret = x; \
} while (0)

int cmdline_process_param(char *p, char *value, int need_save)
{
    int ret = 0;

    if (!strcmp(p, "-load")) {
	RETURN(2);
	SAVEABLE(0);		       /* very high priority */
	do_defaults(value, &cfg);
	return 2;
    }
    if (!strcmp(p, "-ssh")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	default_protocol = cfg.protocol = PROT_SSH;
	default_port = cfg.port = 22;
	return 1;
    }
    if (!strcmp(p, "-telnet")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	default_protocol = cfg.protocol = PROT_TELNET;
	default_port = cfg.port = 23;
	return 1;
    }
    if (!strcmp(p, "-rlogin")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	default_protocol = cfg.protocol = PROT_RLOGIN;
	default_port = cfg.port = 513;
	return 1;
    }
    if (!strcmp(p, "-raw")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	default_protocol = cfg.protocol = PROT_RAW;
    }
    if (!strcmp(p, "-v")) {
	RETURN(1);
	flags |= FLAG_VERBOSE;
    }
    if (!strcmp(p, "-l")) {
	RETURN(2);
	SAVEABLE(1);
	strncpy(cfg.username, value, sizeof(cfg.username));
	cfg.username[sizeof(cfg.username) - 1] = '\0';
    }
    if ((!strcmp(p, "-L") || !strcmp(p, "-R"))) {
	char *fwd, *ptr, *q;
	int i=0;
	RETURN(2);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	fwd = value;
	ptr = cfg.portfwd;
	/* if multiple forwards, find end of list */
	if (ptr[0]=='R' || ptr[0]=='L') {
	    for (i = 0; i < sizeof(cfg.portfwd) - 2; i++)
		if (ptr[i]=='\000' && ptr[i+1]=='\000')
		    break;
	    ptr = ptr + i + 1;  /* point to next forward slot */
	}
	ptr[0] = p[1];  /* insert a 'L' or 'R' at the start */
	if (strlen(fwd) > sizeof(cfg.portfwd) - i - 2) {
	    cmdline_error("out of space for port forwardings");
	    return ret;
	}
	strncpy(ptr+1, fwd, sizeof(cfg.portfwd) - i);
	q = strchr(ptr, ':');
	if (q) *q = '\t';      /* replace first : with \t */
	cfg.portfwd[sizeof(cfg.portfwd) - 1] = '\0';
	cfg.portfwd[sizeof(cfg.portfwd) - 2] = '\0';
	ptr[strlen(ptr)+1] = '\000';    /* append two '\000' */
    }
    if (!strcmp(p, "-m")) {
	char *filename, *command;
	int cmdlen, cmdsize;
	FILE *fp;
	int c, d;

	RETURN(2);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);

	filename = value;

	cmdlen = cmdsize = 0;
	command = NULL;
	fp = fopen(filename, "r");
	if (!fp) {
	    cmdline_error("unable to open command "
			  "file \"%s\"", filename);
	    return ret;
	}
	do {
	    c = fgetc(fp);
	    d = c;
	    if (c == EOF)
		d = 0;
	    if (cmdlen >= cmdsize) {
		cmdsize = cmdlen + 512;
		command = srealloc(command, cmdsize);
	    }
	    command[cmdlen++] = d;
	} while (c != EOF);
	cfg.remote_cmd_ptr = command;
	cfg.remote_cmd_ptr2 = NULL;
	cfg.nopty = TRUE;      /* command => no terminal */
    }
    if (!strcmp(p, "-P")) {
	RETURN(2);
	SAVEABLE(2);		       /* lower priority than -ssh,-telnet */
	cfg.port = atoi(value);
    }
    if (!strcmp(p, "-pw")) {
	RETURN(2);
	cmdline_password = value;
	ssh_get_line = cmdline_get_line;
	ssh_getline_pw_only = TRUE;
    }

    if (!strcmp(p, "-A")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.agentfwd = 1;
    }
    if (!strcmp(p, "-a")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.agentfwd = 0;
    }

    if (!strcmp(p, "-X")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.x11_forward = 1;
    }
    if (!strcmp(p, "-x")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.x11_forward = 0;
    }

    if (!strcmp(p, "-t")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.nopty = 0;
    }
    if (!strcmp(p, "-T")) {
	RETURN(1);
	UNAVAILABLE_IN(TOOLTYPE_FILETRANSFER);
	SAVEABLE(1);
	cfg.nopty = 1;
    }

    if (!strcmp(p, "-C")) {
	RETURN(1);
	SAVEABLE(1);
	cfg.compression = 1;
    }

    if (!strcmp(p, "-1")) {
	RETURN(1);
	SAVEABLE(1);
	cfg.sshprot = 0;	       /* ssh protocol 1 only */
    }
    if (!strcmp(p, "-2")) {
	RETURN(1);
	SAVEABLE(1);
	cfg.sshprot = 3;	       /* ssh protocol 2 only */
    }

    if (!strcmp(p, "-i")) {
	RETURN(2);
	SAVEABLE(1);
	strncpy(cfg.keyfile, value, sizeof(cfg.keyfile));
	cfg.keyfile[sizeof(cfg.keyfile)-1] = '\0';
    }

    return ret;			       /* unrecognised */
}

void cmdline_run_saved(void)
{
    int pri, i;
    for (pri = 0; pri < NPRIORITIES; pri++)
	for (i = 0; i < saves[pri].nsaved; i++)
	    cmdline_process_param(saves[pri].params[i].p,
				  saves[pri].params[i].value, 0);
}
