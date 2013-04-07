/*
 * Session logging.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <time.h>
#include <assert.h>

#include "putty.h"

/* log session to file stuff ... */
struct LogContext {
    FILE *lgfp;
    enum { L_CLOSED, L_OPENING, L_OPEN, L_ERROR } state;
    bufchain queue;
    Filename currlogfilename;
    void *frontend;
    Config cfg;
};

static void xlatlognam(Filename *d, Filename s, char *hostname, struct tm *tm);

/*
 * Internal wrapper function which must be called for _all_ output
 * to the log file. It takes care of opening the log file if it
 * isn't open, buffering data if it's in the process of being
 * opened asynchronously, etc.
 */
static void logwrite(struct LogContext *ctx, void *data, int len)
{
    /*
     * In state L_CLOSED, we call logfopen, which will set the state
     * to one of L_OPENING, L_OPEN or L_ERROR. Hence we process all of
     * those three _after_ processing L_CLOSED.
     */
    if (ctx->state == L_CLOSED)
	logfopen(ctx);

    if (ctx->state == L_OPENING) {
	bufchain_add(&ctx->queue, data, len);
    } else if (ctx->state == L_OPEN) {
	assert(ctx->lgfp);
	if (fwrite(data, 1, len, ctx->lgfp) < (size_t)len) {
	    logfclose(ctx);
	    ctx->state = L_ERROR;
	    /* Log state is L_ERROR so this won't cause a loop */
	    logevent(ctx->frontend,
		     "Disabled writing session log due to error while writing");
	}
    }				       /* else L_ERROR, so ignore the write */
}

/*
 * Convenience wrapper on logwrite() which printf-formats the
 * string.
 */
static void logprintf(struct LogContext *ctx, const char *fmt, ...)
{
    va_list ap;
    char *data;

    va_start(ap, fmt);
    data = dupvprintf(fmt, ap);
    va_end(ap);

    logwrite(ctx, data, strlen(data));
    sfree(data);
}

/*
 * Flush any open log file.
 */
void logflush(void *handle) {
    struct LogContext *ctx = (struct LogContext *)handle;
    if (ctx->cfg.logtype > 0)
	if (ctx->state == L_OPEN)
	    fflush(ctx->lgfp);
}

static void logfopen_callback(void *handle, int mode)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    char buf[256], *event;
    struct tm tm;
    const char *fmode;

    if (mode == 0) {
	ctx->state = L_ERROR;	       /* disable logging */
    } else {
	fmode = (mode == 1 ? "ab" : "wb");
	ctx->lgfp = f_open(ctx->currlogfilename, fmode, FALSE);
	if (ctx->lgfp)
	    ctx->state = L_OPEN;
	else
	    ctx->state = L_ERROR;
    }

    if (ctx->state == L_OPEN) {
	/* Write header line into log file. */
	tm = ltime();
	strftime(buf, 24, "%Y.%m.%d %H:%M:%S", &tm);
	logprintf(ctx, "=~=~=~=~=~=~=~=~=~=~=~= PuTTY log %s"
		  " =~=~=~=~=~=~=~=~=~=~=~=\r\n", buf);
    }

    event = dupprintf("%s session log (%s mode) to file: %s",
		      ctx->state == L_ERROR ?
		      (mode == 0 ? "Disabled writing" : "Error writing") :
		      (mode == 1 ? "Appending" : "Writing new"),
		      (ctx->cfg.logtype == LGTYP_ASCII ? "ASCII" :
		       ctx->cfg.logtype == LGTYP_DEBUG ? "raw" :
		       ctx->cfg.logtype == LGTYP_PACKETS ? "SSH packets" :
		       ctx->cfg.logtype == LGTYP_SSHRAW ? "SSH raw data" :
		       "unknown"),
		      filename_to_str(&ctx->currlogfilename));
    logevent(ctx->frontend, event);
    sfree(event);

    /*
     * Having either succeeded or failed in opening the log file,
     * we should write any queued data out.
     */
    assert(ctx->state != L_OPENING);   /* make _sure_ it won't be requeued */
    while (bufchain_size(&ctx->queue)) {
	void *data;
	int len;
	bufchain_prefix(&ctx->queue, &data, &len);
	logwrite(ctx, data, len);
	bufchain_consume(&ctx->queue, len);
    }
}

/*
 * Open the log file. Takes care of detecting an already-existing
 * file and asking the user whether they want to append, overwrite
 * or cancel logging.
 */
void logfopen(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    struct tm tm;
    int mode;

    /* Prevent repeat calls */
    if (ctx->state != L_CLOSED)
	return;

    if (!ctx->cfg.logtype)
	return;

    tm = ltime();

    /* substitute special codes in file name */
    xlatlognam(&ctx->currlogfilename, ctx->cfg.logfilename,ctx->cfg.host, &tm);

    ctx->lgfp = f_open(ctx->currlogfilename, "r", FALSE);  /* file already present? */
    if (ctx->lgfp) {
	fclose(ctx->lgfp);
	if (ctx->cfg.logxfovr != LGXF_ASK) {
	    mode = ((ctx->cfg.logxfovr == LGXF_OVR) ? 2 : 1);
	} else
	    mode = askappend(ctx->frontend, ctx->currlogfilename,
			     logfopen_callback, ctx);
    } else
	mode = 2;		       /* create == overwrite */

    if (mode < 0)
	ctx->state = L_OPENING;
    else
	logfopen_callback(ctx, mode);  /* open the file */
}

void logfclose(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    if (ctx->lgfp) {
	fclose(ctx->lgfp);
	ctx->lgfp = NULL;
    }
    ctx->state = L_CLOSED;
}

/*
 * Log session traffic.
 */
void logtraffic(void *handle, unsigned char c, int logmode)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    if (ctx->cfg.logtype > 0) {
	if (ctx->cfg.logtype == logmode)
	    logwrite(ctx, &c, 1);
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
    /* If we don't have a context yet (eg winnet.c init) then skip entirely */
    if (!ctx)
	return;
    if (ctx->cfg.logtype != LGTYP_PACKETS &&
	ctx->cfg.logtype != LGTYP_SSHRAW)
	return;
    logprintf(ctx, "Event Log: %s\r\n", event);
    logflush(ctx);
}

/*
 * Log an SSH packet.
 * If n_blanks != 0, blank or omit some parts.
 * Set of blanking areas must be in increasing order.
 */
void log_packet(void *handle, int direction, int type,
		char *texttype, const void *data, int len,
		int n_blanks, const struct logblank_t *blanks,
		const unsigned long *seq)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    char dumpdata[80], smalldata[5];
    int p = 0, b = 0, omitted = 0;
    int output_pos = 0; /* NZ if pending output in dumpdata */

    if (!(ctx->cfg.logtype == LGTYP_SSHRAW ||
          (ctx->cfg.logtype == LGTYP_PACKETS && texttype)))
	return;

    /* Packet header. */
    if (texttype) {
	if (seq) {
	    logprintf(ctx, "%s packet #0x%lx, type %d / 0x%02x (%s)\r\n",
		      direction == PKT_INCOMING ? "Incoming" : "Outgoing",
		      *seq, type, type, texttype);
	} else {
	    logprintf(ctx, "%s packet type %d / 0x%02x (%s)\r\n",
		      direction == PKT_INCOMING ? "Incoming" : "Outgoing",
		      type, type, texttype);
	}
    } else {
        logprintf(ctx, "%s raw data\r\n",
                  direction == PKT_INCOMING ? "Incoming" : "Outgoing");
    }

    /*
     * Output a hex/ASCII dump of the packet body, blanking/omitting
     * parts as specified.
     */
    while (p < len) {
	int blktype;

	/* Move to a current entry in the blanking array. */
	while ((b < n_blanks) &&
	       (p >= blanks[b].offset + blanks[b].len))
	    b++;
	/* Work out what type of blanking to apply to
	 * this byte. */
	blktype = PKTLOG_EMIT; /* default */
	if ((b < n_blanks) &&
	    (p >= blanks[b].offset) &&
	    (p < blanks[b].offset + blanks[b].len))
	    blktype = blanks[b].type;

	/* If we're about to stop omitting, it's time to say how
	 * much we omitted. */
	if ((blktype != PKTLOG_OMIT) && omitted) {
	    logprintf(ctx, "  (%d byte%s omitted)\r\n",
		      omitted, (omitted==1?"":"s"));
	    omitted = 0;
	}

	/* (Re-)initialise dumpdata as necessary
	 * (start of row, or if we've just stopped omitting) */
	if (!output_pos && !omitted)
	    sprintf(dumpdata, "  %08x%*s\r\n", p-(p%16), 1+3*16+2+16, "");

	/* Deal with the current byte. */
	if (blktype == PKTLOG_OMIT) {
	    omitted++;
	} else {
	    int c;
	    if (blktype == PKTLOG_BLANK) {
		c = 'X';
		sprintf(smalldata, "XX");
	    } else {  /* PKTLOG_EMIT */
		c = ((unsigned char *)data)[p];
		sprintf(smalldata, "%02x", c);
	    }
	    dumpdata[10+2+3*(p%16)] = smalldata[0];
	    dumpdata[10+2+3*(p%16)+1] = smalldata[1];
	    dumpdata[10+1+3*16+2+(p%16)] = (isprint(c) ? c : '.');
	    output_pos = (p%16) + 1;
	}

	p++;

	/* Flush row if necessary */
	if (((p % 16) == 0) || (p == len) || omitted) {
	    if (output_pos) {
		strcpy(dumpdata + 10+1+3*16+2+output_pos, "\r\n");
		logwrite(ctx, dumpdata, strlen(dumpdata));
		output_pos = 0;
	    }
	}

    }

    /* Tidy up */
    if (omitted)
	logprintf(ctx, "  (%d byte%s omitted)\r\n",
		  omitted, (omitted==1?"":"s"));
    logflush(ctx);
}

void *log_init(void *frontend, Config *cfg)
{
    struct LogContext *ctx = snew(struct LogContext);
    ctx->lgfp = NULL;
    ctx->state = L_CLOSED;
    ctx->frontend = frontend;
    ctx->cfg = *cfg;		       /* STRUCTURE COPY */
    bufchain_init(&ctx->queue);
    return ctx;
}

void log_free(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;

    logfclose(ctx);
    bufchain_clear(&ctx->queue);
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
 * "&Y":YYYY   "&m":MM   "&d":DD   "&T":hhmmss   "&h":<hostname>   "&&":&
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
	    if (*s) switch (c = *s++, tolower((unsigned char)c)) {
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
