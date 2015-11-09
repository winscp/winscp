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
    Filename *currlogfilename;
    void *frontend;
    Conf *conf;
    int logtype;		       /* cached out of conf */
};

static Filename *xlatlognam(Filename *s, char *hostname, int port,
                            struct tm *tm);

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
    if (ctx->logtype > 0)
	if (ctx->state == L_OPEN)
	    fflush(ctx->lgfp);
}

static void logfopen_callback(void *handle, int mode)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    char buf[256], *event;
    struct tm tm;
    const char *fmode;
    int shout = FALSE;

    if (mode == 0) {
	ctx->state = L_ERROR;	       /* disable logging */
    } else {
	fmode = (mode == 1 ? "ab" : "wb");
	ctx->lgfp = f_open(ctx->currlogfilename, fmode, FALSE);
	if (ctx->lgfp) {
	    ctx->state = L_OPEN;
        } else {
	    ctx->state = L_ERROR;
            shout = TRUE;
        }
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
		      (ctx->logtype == LGTYP_ASCII ? "ASCII" :
		       ctx->logtype == LGTYP_DEBUG ? "raw" :
		       ctx->logtype == LGTYP_PACKETS ? "SSH packets" :
		       ctx->logtype == LGTYP_SSHRAW ? "SSH raw data" :
		       "unknown"),
		      filename_to_str(ctx->currlogfilename));
    logevent(ctx->frontend, event);
    if (shout) {
        /*
         * If we failed to open the log file due to filesystem error
         * (as opposed to user action such as clicking Cancel in the
         * askappend box), we should log it more prominently. We do
         * this by sending it to the same place that stderr output
         * from the main session goes (so, either a console tool's
         * actual stderr, or a terminal window).
         *
         * Of course this is one case in which that policy won't cause
         * it to turn up embarrassingly in a log file of real server
         * output, because the whole point is that we haven't managed
         * to open any such log file :-)
         */
        from_backend(ctx->frontend, 1, event, strlen(event));
        from_backend(ctx->frontend, 1, "\r\n", 2);
    }
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
    FILE *fp;
    int mode;

    /* Prevent repeat calls */
    if (ctx->state != L_CLOSED)
	return;

    if (!ctx->logtype)
	return;

    tm = ltime();

    /* substitute special codes in file name */
    if (ctx->currlogfilename)
        filename_free(ctx->currlogfilename);
    ctx->currlogfilename = 
        xlatlognam(conf_get_filename(ctx->conf, CONF_logfilename),
                   conf_get_str(ctx->conf, CONF_host),
                   conf_get_int(ctx->conf, CONF_port), &tm);

    fp = f_open(ctx->currlogfilename, "r", FALSE);  /* file already present? */
    if (fp) {
	int logxfovr = conf_get_int(ctx->conf, CONF_logxfovr);
	fclose(fp);
	if (logxfovr != LGXF_ASK) {
	    mode = ((logxfovr == LGXF_OVR) ? 2 : 1);
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
    if (ctx->logtype > 0) {
	if (ctx->logtype == logmode)
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
    if (ctx->logtype != LGTYP_PACKETS &&
	ctx->logtype != LGTYP_SSHRAW)
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
		const unsigned long *seq,
                unsigned downstream_id, const char *additional_log_text)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    char dumpdata[80], smalldata[5];
    int p = 0, b = 0, omitted = 0;
    int output_pos = 0; /* NZ if pending output in dumpdata */

    if (!(ctx->logtype == LGTYP_SSHRAW ||
          (ctx->logtype == LGTYP_PACKETS && texttype)))
	return;

    /* Packet header. */
    if (texttype) {
        logprintf(ctx, "%s packet ",
                  direction == PKT_INCOMING ? "Incoming" : "Outgoing");

	if (seq)
	    logprintf(ctx, "#0x%lx, ", *seq);

        logprintf(ctx, "type %d / 0x%02x (%s)", type, type, texttype);

        if (downstream_id) {
	    logprintf(ctx, " on behalf of downstream #%u", downstream_id);
            if (additional_log_text)
                logprintf(ctx, " (%s)", additional_log_text);
        }

        logprintf(ctx, "\r\n");
    } else {
        /*
         * Raw data is logged with a timestamp, so that it's possible
         * to determine whether a mysterious delay occurred at the
         * client or server end. (Timestamping the raw data avoids
         * cluttering the normal case of only logging decrypted SSH
         * messages, and also adds conceptual rigour in the case where
         * an SSH message arrives in several pieces.)
         */
        char buf[256];
        struct tm tm;
	tm = ltime();
	strftime(buf, 24, "%Y-%m-%d %H:%M:%S", &tm);
        logprintf(ctx, "%s raw data at %s\r\n",
                  direction == PKT_INCOMING ? "Incoming" : "Outgoing",
                  buf);
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

void *log_init(void *frontend, Conf *conf)
{
    struct LogContext *ctx = snew(struct LogContext);
    ctx->lgfp = NULL;
    ctx->state = L_CLOSED;
    ctx->frontend = frontend;
    ctx->conf = conf_copy(conf);
    ctx->logtype = conf_get_int(ctx->conf, CONF_logtype);
    ctx->currlogfilename = NULL;
    bufchain_init(&ctx->queue);
    return ctx;
}

void log_free(void *handle)
{
    struct LogContext *ctx = (struct LogContext *)handle;

    logfclose(ctx);
    bufchain_clear(&ctx->queue);
    if (ctx->currlogfilename)
        filename_free(ctx->currlogfilename);
    conf_free(ctx->conf);
    sfree(ctx);
}

void log_reconfig(void *handle, Conf *conf)
{
    struct LogContext *ctx = (struct LogContext *)handle;
    int reset_logging;

    if (!filename_equal(conf_get_filename(ctx->conf, CONF_logfilename),
			conf_get_filename(conf, CONF_logfilename)) ||
	conf_get_int(ctx->conf, CONF_logtype) !=
	conf_get_int(conf, CONF_logtype))
	reset_logging = TRUE;
    else
	reset_logging = FALSE;

    if (reset_logging)
	logfclose(ctx);

    conf_free(ctx->conf);
    ctx->conf = conf_copy(conf);

    ctx->logtype = conf_get_int(ctx->conf, CONF_logtype);

    if (reset_logging)
	logfopen(ctx);
}

/*
 * translate format codes into time/date strings
 * and insert them into log file name
 *
 * "&Y":YYYY   "&m":MM   "&d":DD   "&T":hhmmss   "&h":<hostname>   "&&":&
 */
static Filename *xlatlognam(Filename *src, char *hostname, int port,
                            struct tm *tm)
{
    char buf[32], *bufp;
    int size;
    char *buffer;
    int buflen, bufsize;
    const char *s;
    Filename *ret;

    bufsize = FILENAME_MAX;
    buffer = snewn(bufsize, char);
    buflen = 0;
    s = filename_to_str(src);

    while (*s) {
        int sanitise = FALSE;
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
	      case 'p':
                size = sprintf(buf, "%d", port);
		break;
	      default:
		buf[0] = '&';
		size = 1;
		if (c != '&')
		    buf[size++] = c;
	    }
            /* Never allow path separators - or any other illegal
             * filename character - to come out of any of these
             * auto-format directives. E.g. 'hostname' can contain
             * colons, if it's an IPv6 address, and colons aren't
             * legal in filenames on Windows. */
            sanitise = TRUE;
	} else {
	    buf[0] = *s++;
	    size = 1;
	}
        if (bufsize <= buflen + size) {
            bufsize = (buflen + size) * 5 / 4 + 512;
            buffer = sresize(buffer, bufsize, char);
        }
        while (size-- > 0) {
            char c = *bufp++;
            if (sanitise)
                c = filename_char_sanitise(c);
            buffer[buflen++] = c;
        }
    }
    buffer[buflen] = '\0';

    ret = filename_from_str(buffer);
    sfree(buffer);
    return ret;
}
