#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <time.h>
#include <assert.h>

#include "putty.h"

/* log session to file stuff ... */
static FILE *lgfp = NULL;
static char timdatbuf[20];
static char currlogfilename[FILENAME_MAX];

static void xlatlognam(char *d, char *s, char *hostname, struct tm *tm);

/*
 * Log session traffic.
 */
void logtraffic(unsigned char c, int logmode)
{
    if (cfg.logtype > 0) {
	if (cfg.logtype == logmode) {
	    /* deferred open file from pgm start? */
	    if (!lgfp)
		logfopen();
	    if (lgfp)
		fputc(c, lgfp);
	}
    }
}

/*
 * Log an Event Log entry (used in SSH packet logging mode).
 */
void log_eventlog(char *event)
{
    if (cfg.logtype != LGTYP_PACKETS)
	return;
    if (!lgfp)
	logfopen();
    if (lgfp)
	fprintf(lgfp, "Event Log: %s\n", event);
}

/*
 * Log an SSH packet.
 */
void log_packet(int direction, int type, char *texttype, void *data, int len)
{
    int i, j;
    char dumpdata[80], smalldata[5];

    if (cfg.logtype != LGTYP_PACKETS)
	return;
    if (!lgfp)
	logfopen();
    if (lgfp) {
	fprintf(lgfp, "%s packet type %d / 0x%02x (%s)\n",
		direction == PKT_INCOMING ? "Incoming" : "Outgoing",
		type, type, texttype);
	for (i = 0; i < len; i += 16) {
	    sprintf(dumpdata, "  %08x%*s\n", i, 1+3*16+2+16, "");
	    for (j = 0; j < 16 && i+j < len; j++) {
		int c = ((unsigned char *)data)[i+j];
		sprintf(smalldata, "%02x", c);
		dumpdata[10+2+3*j] = smalldata[0];
		dumpdata[10+2+3*j+1] = smalldata[1];
		dumpdata[10+1+3*16+2+j] = (isprint(c) ? c : '.');
	    }
	    strcpy(dumpdata + 10+1+3*16+2+j, "\n");
	    fputs(dumpdata, lgfp);
	}
	fflush(lgfp);
    }
}

/* open log file append/overwrite mode */
void logfopen(void)
{
    char buf[256];
    time_t t;
    struct tm tm;
    char writemod[4];

    /* Prevent repeat calls */
    if (lgfp)
	return;

    if (!cfg.logtype)
	return;
    sprintf(writemod, "wb");	       /* default to rewrite */

    time(&t);
    tm = *localtime(&t);

    /* substitute special codes in file name */
    xlatlognam(currlogfilename,cfg.logfilename,cfg.host, &tm);

    lgfp = fopen(currlogfilename, "r");	/* file already present? */
    if (lgfp) {
	int i;
	fclose(lgfp);
	i = askappend(currlogfilename);
	if (i == 1)
	    writemod[0] = 'a';	       /* set append mode */
	else if (i == 0) {	       /* cancelled */
	    lgfp = NULL;
	    cfg.logtype = 0;	       /* disable logging */
	    return;
	}
    }

    lgfp = fopen(currlogfilename, writemod);
    if (lgfp) {			       /* enter into event log */
	/* --- write header line into log file */
	fputs("=~=~=~=~=~=~=~=~=~=~=~= PuTTY log ", lgfp);
	strftime(buf, 24, "%Y.%m.%d %H:%M:%S", &tm);
	fputs(buf, lgfp);
	fputs(" =~=~=~=~=~=~=~=~=~=~=~=\r\n", lgfp);

	sprintf(buf, "%s session log (%s mode) to file: ",
		(writemod[0] == 'a') ? "Appending" : "Writing new",
		(cfg.logtype == LGTYP_ASCII ? "ASCII" :
		 cfg.logtype == LGTYP_DEBUG ? "raw" :
		 cfg.logtype == LGTYP_PACKETS ? "SSH packets" : "<ukwn>"));
	/* Make sure we do not exceed the output buffer size */
	strncat(buf, currlogfilename, 128);
	buf[strlen(buf)] = '\0';
	logevent(buf);
    }
}

void logfclose(void)
{
    if (lgfp) {
	fclose(lgfp);
	lgfp = NULL;
    }
}

/*
 * translate format codes into time/date strings
 * and insert them into log file name
 *
 * "&Y":YYYY   "&m":MM   "&d":DD   "&T":hhmm   "&h":<hostname>   "&&":&
 */
static void xlatlognam(char *d, char *s, char *hostname, struct tm *tm) {
    char buf[10], *bufp;
    int size;
    char *ds = d; /* save start pos. */
    int len = FILENAME_MAX-1;

    while (*s) {
	/* Let (bufp, len) be the string to append. */
	bufp = buf;		       /* don't usually override this */
	if (*s == '&') {
	    char c;
	    s++;
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
}
