#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <time.h>
#include <assert.h>

#include "putty.h"

/* log session to file stuff ... */
struct LogContext {
    FILE *lgfp;
    Filename currlogfilename;
    void *frontend;
    Config cfg;
};

static void xlatlognam(Filename *d, Filename s, char *hostname, struct tm *tm);

/*
 * Log session traffic.
 */
void logtraffic(void *handle, unsigned char c, int logmode)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    if (ctx->cfg.logtype > 0) {
	if (ctx->cfg.logtype == logmode) {
	    /* deferred open file from pgm start? */
	    if (!ctx->lgfp)
		logfopen(ctx);
	    if (ctx->lgfp)
		fputc(c, ctx->lgfp);
	}
    }
}

/*
 * Log an Event Log entry. Used in SSH packet logging mode; this is
 * also as convenient a place as any to put the output of Event Log
 * entries to stderr when a command-line tool is in verbose mode.
 * (In particular, this is a better place to put it than in the
 * front ends, because it only has to be done once for all
 * platforms. Platforms which don't have a meaningful stderr can
 * just avoid defining FLAG_STDERR.
 */
void log_eventlog(void *handle, const char *event)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    if ((flags & FLAG_STDERR) && (flags & FLAG_VERBOSE)) {
	fprintf(stderr, "%s\n", event);
	fflush(stderr);
    }
    if (ctx->cfg.logtype != LGTYP_PACKETS)
	return;
    if (!ctx->lgfp)
	logfopen(ctx);
    if (ctx->lgfp)
	fprintf(ctx->lgfp, "Event Log: %s\r\n", event);
}

/*
 * Log an SSH packet.
 */
void log_packet(void *handle, int direction, int type,
		char *texttype, void *data, int len)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    int i, j;
    char dumpdata[80], smalldata[5];

    if (ctx->cfg.logtype != LGTYP_PACKETS)
	return;
    if (!ctx->lgfp)
	logfopen(ctx);
    if (ctx->lgfp) {
	fprintf(ctx->lgfp, "%s packet type %d / 0x%02x (%s)\r\n",
		direction == PKT_INCOMING ? "Incoming" : "Outgoing",
		type, type, texttype);
	for (i = 0; i < len; i += 16) {
	    sprintf(dumpdata, "  %08x%*s\r\n", i, 1+3*16+2+16, "");
	    for (j = 0; j < 16 && i+j < len; j++) {
		int c = ((unsigned char *)data)[i+j];
		sprintf(smalldata, "%02x", c);
		dumpdata[10+2+3*j] = smalldata[0];
		dumpdata[10+2+3*j+1] = smalldata[1];
		dumpdata[10+1+3*16+2+j] = (isprint(c) ? c : '.');
	    }
	    strcpy(dumpdata + 10+1+3*16+2+j, "\r\n");
	    fputs(dumpdata, ctx->lgfp);
	}
	fflush(ctx->lgfp);
    }
}

/* open log file append/overwrite mode */
void logfopen(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    char buf[256];
    time_t t;
    struct tm tm;
    char writemod[4];

    /* Prevent repeat calls */
    if (ctx->lgfp)
	return;

    if (!ctx->cfg.logtype)
	return;
    sprintf(writemod, "wb");	       /* default to rewrite */

    time(&t);
    tm = *localtime(&t);

    /* substitute special codes in file name */
    xlatlognam(&ctx->currlogfilename, ctx->cfg.logfilename,ctx->cfg.host, &tm);

    ctx->lgfp = f_open(ctx->currlogfilename, "r");  /* file already present? */
    if (ctx->lgfp) {
	int i;
	fclose(ctx->lgfp);
	if (ctx->cfg.logxfovr != LGXF_ASK) {
	    i = ((ctx->cfg.logxfovr == LGXF_OVR) ? 2 : 1);
	} else
	    i = askappend(ctx->frontend, ctx->currlogfilename);
	if (i == 1)
	    writemod[0] = 'a';	       /* set append mode */
	else if (i == 0) {	       /* cancelled */
	    ctx->lgfp = NULL;
	    ctx->cfg.logtype = 0;	       /* disable logging */
	    return;
	}
    }

    ctx->lgfp = f_open(ctx->currlogfilename, writemod);
    if (ctx->lgfp) {			       /* enter into event log */
	/* --- write header line into log file */
	fputs("=~=~=~=~=~=~=~=~=~=~=~= PuTTY log ", ctx->lgfp);
	strftime(buf, 24, "%Y.%m.%d %H:%M:%S", &tm);
	fputs(buf, ctx->lgfp);
	fputs(" =~=~=~=~=~=~=~=~=~=~=~=\r\n", ctx->lgfp);

	sprintf(buf, "%s session log (%s mode) to file: ",
		(writemod[0] == 'a') ? "Appending" : "Writing new",
		(ctx->cfg.logtype == LGTYP_ASCII ? "ASCII" :
		 ctx->cfg.logtype == LGTYP_DEBUG ? "raw" :
		 ctx->cfg.logtype == LGTYP_PACKETS ? "SSH packets" : "<ukwn>"));
	/* Make sure we do not exceed the output buffer size */
	strncat(buf, filename_to_str(&ctx->currlogfilename), 128);
	buf[strlen(buf)] = '\0';
	logevent(ctx->frontend, buf);
    }
}

void logfclose(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    if (ctx->lgfp) {
	fclose(ctx->lgfp);
	ctx->lgfp = NULL;
    }
}

void *log_init(void *frontend, Config *cfg)
{
    struct LogContext *ctx = snew(struct LogContext);
    ctx->lgfp = NULL;
    ctx->frontend = frontend;
    ctx->cfg = *cfg;		       /* STRUCTURE COPY */
    return ctx;
}

void log_free(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;

    logfclose(ctx);
    sfree(ctx);
}

void log_reconfig(void *handle, Config *cfg)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    int reset_logging;

    if (!filename_equal(ctx->cfg.logfilename, cfg->logfilename) ||
	ctx->cfg.logtype != cfg->logtype)
	reset_logging = TRUE;
    else
	reset_logging = FALSE;

    if (reset_logging)
	logfclose(ctx);

    ctx->cfg = *cfg;		       /* STRUCTURE COPY */

    if (reset_logging)
	logfopen(ctx);
}

/*
 * translate format codes into time/date strings
 * and insert them into log file name
 *
 * "&Y":YYYY   "&m":MM   "&d":DD   "&T":hhmm   "&h":<hostname>   "&&":&
 */
static void xlatlognam(Filename *dest, Filename src,
		       char *hostname, struct tm *tm) {
    char buf[10], *bufp;
    int size;
    char buffer[FILENAME_MAX];
    int len = sizeof(buffer)-1;
    char *d;
    const char *s;

    d = buffer;
    s = filename_to_str(&src);

    while (*s) {
	/* Let (bufp, len) be the string to append. */
	bufp = buf;		       /* don't usually override this */
	if (*s == '&') {
	    char c;
	    s++;
	    size = 0;
	    if (*s) switch (c = *s++, tolower(c)) {
	      case 'y':
		size = strftime(buf, sizeof(buf), "%Y", tm);
		break;
	      case 'm':
		size = strftime(buf, sizeof(buf), "%m", tm);
		break;
	      case 'd':
		size = strftime(buf, sizeof(buf), "%d", tm);
		break;
	      case 't':
		size = strftime(buf, sizeof(buf), "%H%M%S", tm);
		break;
	      case 'h':
		bufp = hostname;
		size = strlen(bufp);
		break;
	      default:
		buf[0] = '&';
		size = 1;
		if (c != '&')
		    buf[size++] = c;
	    }
	} else {
	    buf[0] = *s++;
	    size = 1;
	}
	if (size > len)
	    size = len;
	memcpy(d, bufp, size);
	d += size;
	len -= size;
    }
    *d = '\0';

    *dest = filename_from_str(buffer);
}
