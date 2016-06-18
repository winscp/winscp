/*
 * Platform-independent routines shared between all PuTTY programs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <ctype.h>
#include <assert.h>
#include "putty.h"

/*
 * Parse a string block size specification. This is approximately a
 * subset of the block size specs supported by GNU fileutils:
 *  "nk" = n kilobytes
 *  "nM" = n megabytes
 *  "nG" = n gigabytes
 * All numbers are decimal, and suffixes refer to powers of two.
 * Case-insensitive.
 */
unsigned long parse_blocksize(const char *bs)
{
    char *suf;
    unsigned long r = strtoul(bs, &suf, 10);
    if (*suf != '\0') {
	while (*suf && isspace((unsigned char)*suf)) suf++;
	switch (*suf) {
	  case 'k': case 'K':
	    r *= 1024ul;
	    break;
	  case 'm': case 'M':
	    r *= 1024ul * 1024ul;
	    break;
	  case 'g': case 'G':
	    r *= 1024ul * 1024ul * 1024ul;
	    break;
	  case '\0':
	  default:
	    break;
	}
    }
    return r;
}

/*
 * Parse a ^C style character specification.
 * Returns NULL in `next' if we didn't recognise it as a control character,
 * in which case `c' should be ignored.
 * The precise current parsing is an oddity inherited from the terminal
 * answerback-string parsing code. All sequences start with ^; all except
 * ^<123> are two characters. The ones that are worth keeping are probably:
 *   ^?		    127
 *   ^@A-Z[\]^_	    0-31
 *   a-z	    1-26
 *   <num>	    specified by number (decimal, 0octal, 0xHEX)
 *   ~		    ^ escape
 */
char ctrlparse(char *s, char **next)
{
    char c = 0;
    if (*s != '^') {
	*next = NULL;
    } else {
	s++;
	if (*s == '\0') {
	    *next = NULL;
	} else if (*s == '<') {
	    s++;
	    c = (char)strtol(s, next, 0);
	    if ((*next == s) || (**next != '>')) {
		c = 0;
		*next = NULL;
	    } else
		(*next)++;
	} else if (*s >= 'a' && *s <= 'z') {
	    c = (*s - ('a' - 1));
	    *next = s+1;
	} else if ((*s >= '@' && *s <= '_') || *s == '?' || (*s & 0x80)) {
	    c = ('@' ^ *s);
	    *next = s+1;
	} else if (*s == '~') {
	    c = '^';
	    *next = s+1;
	}
    }
    return c;
}

prompts_t *new_prompts(void *frontend)
{
    prompts_t *p = snew(prompts_t);
    p->prompts = NULL;
    p->n_prompts = 0;
    p->frontend = frontend;
    p->data = NULL;
    p->to_server = TRUE; /* to be on the safe side */
    p->name = p->instruction = NULL;
    p->name_reqd = p->instr_reqd = FALSE;
    return p;
}
void add_prompt(prompts_t *p, char *promptstr, int echo)
{
    prompt_t *pr = snew(prompt_t);
    pr->prompt = promptstr;
    pr->echo = echo;
    pr->result = NULL;
    pr->resultsize = 0;
    p->n_prompts++;
    p->prompts = sresize(p->prompts, p->n_prompts, prompt_t *);
    p->prompts[p->n_prompts-1] = pr;
}
void prompt_ensure_result_size(prompt_t *pr, int newlen)
{
    if ((int)pr->resultsize < newlen) {
        char *newbuf;
        newlen = newlen * 5 / 4 + 512; /* avoid too many small allocs */

        /*
         * We don't use sresize / realloc here, because we will be
         * storing sensitive stuff like passwords in here, and we want
         * to make sure that the data doesn't get copied around in
         * memory without the old copy being destroyed.
         */
        newbuf = snewn(newlen, char);
        memcpy(newbuf, pr->result, pr->resultsize);
        smemclr(pr->result, pr->resultsize);
        sfree(pr->result);
        pr->result = newbuf;
        pr->resultsize = newlen;
    }
}
void prompt_set_result(prompt_t *pr, const char *newstr)
{
    prompt_ensure_result_size(pr, strlen(newstr) + 1);
    strcpy(pr->result, newstr);
}
void free_prompts(prompts_t *p)
{
    size_t i;
    for (i=0; i < p->n_prompts; i++) {
	prompt_t *pr = p->prompts[i];
	smemclr(pr->result, pr->resultsize); /* burn the evidence */
	sfree(pr->result);
	sfree(pr->prompt);
	sfree(pr);
    }
    sfree(p->prompts);
    sfree(p->name);
    sfree(p->instruction);
    sfree(p);
}

/* ----------------------------------------------------------------------
 * String handling routines.
 */

char *dupstr(const char *s)
{
    char *p = NULL;
    if (s) {
        int len = strlen(s);
        p = snewn(len + 1, char);
        strcpy(p, s);
    }
    return p;
}

/* Allocate the concatenation of N strings. Terminate arg list with NULL. */
char *dupcat(const char *s1, ...)
{
    int len;
    char *p, *q, *sn;
    va_list ap;

    len = strlen(s1);
    va_start(ap, s1);
    while (1) {
	sn = va_arg(ap, char *);
	if (!sn)
	    break;
	len += strlen(sn);
    }
    va_end(ap);

    p = snewn(len + 1, char);
    strcpy(p, s1);
    q = p + strlen(p);

    va_start(ap, s1);
    while (1) {
	sn = va_arg(ap, char *);
	if (!sn)
	    break;
	strcpy(q, sn);
	q += strlen(q);
    }
    va_end(ap);

    return p;
}

void burnstr(char *string)             /* sfree(str), only clear it first */
{
    if (string) {
        smemclr(string, strlen(string));
        sfree(string);
    }
}

int toint(unsigned u)
{
    /*
     * Convert an unsigned to an int, without running into the
     * undefined behaviour which happens by the strict C standard if
     * the value overflows. You'd hope that sensible compilers would
     * do the sensible thing in response to a cast, but actually I
     * don't trust modern compilers not to do silly things like
     * assuming that _obviously_ you wouldn't have caused an overflow
     * and so they can elide an 'if (i < 0)' test immediately after
     * the cast.
     *
     * Sensible compilers ought of course to optimise this entire
     * function into 'just return the input value'!
     */
    if (u <= (unsigned)INT_MAX)
        return (int)u;
    else if (u >= (unsigned)INT_MIN)   /* wrap in cast _to_ unsigned is OK */
        return INT_MIN + (int)(u - (unsigned)INT_MIN);
    else
        return INT_MIN; /* fallback; should never occur on binary machines */
}

/*
 * Do an sprintf(), but into a custom-allocated buffer.
 * 
 * Currently I'm doing this via vsnprintf. This has worked so far,
 * but it's not good, because vsnprintf is not available on all
 * platforms. There's an ifdef to use `_vsnprintf', which seems
 * to be the local name for it on Windows. Other platforms may
 * lack it completely, in which case it'll be time to rewrite
 * this function in a totally different way.
 * 
 * The only `properly' portable solution I can think of is to
 * implement my own format string scanner, which figures out an
 * upper bound for the length of each formatting directive,
 * allocates the buffer as it goes along, and calls sprintf() to
 * actually process each directive. If I ever need to actually do
 * this, some caveats:
 * 
 *  - It's very hard to find a reliable upper bound for
 *    floating-point values. %f, in particular, when supplied with
 *    a number near to the upper or lower limit of representable
 *    numbers, could easily take several hundred characters. It's
 *    probably feasible to predict this statically using the
 *    constants in <float.h>, or even to predict it dynamically by
 *    looking at the exponent of the specific float provided, but
 *    it won't be fun.
 * 
 *  - Don't forget to _check_, after calling sprintf, that it's
 *    used at most the amount of space we had available.
 * 
 *  - Fault any formatting directive we don't fully understand. The
 *    aim here is to _guarantee_ that we never overflow the buffer,
 *    because this is a security-critical function. If we see a
 *    directive we don't know about, we should panic and die rather
 *    than run any risk.
 */
char *dupprintf(const char *fmt, ...)
{
    char *ret;
    va_list ap;
    va_start(ap, fmt);
    ret = dupvprintf(fmt, ap);
    va_end(ap);
    return ret;
}
char *dupvprintf(const char *fmt, va_list ap)
{
    char *buf;
    int len, size;

    buf = snewn(512, char);
    size = 512;

    while (1) {
#ifdef _WINDOWS
#define vsnprintf _vsnprintf
#endif
#ifdef va_copy
	/* Use the `va_copy' macro mandated by C99, if present.
	 * XXX some environments may have this as __va_copy() */
	va_list aq;
	va_copy(aq, ap);
	len = vsnprintf(buf, size, fmt, aq);
	va_end(aq);
#else
	/* Ugh. No va_copy macro, so do something nasty.
	 * Technically, you can't reuse a va_list like this: it is left
	 * unspecified whether advancing a va_list pointer modifies its
	 * value or something it points to, so on some platforms calling
	 * vsnprintf twice on the same va_list might fail hideously
	 * (indeed, it has been observed to).
	 * XXX the autoconf manual suggests that using memcpy() will give
	 *     "maximum portability". */
	len = vsnprintf(buf, size, fmt, ap);
#endif
	if (len >= 0 && len < size) {
	    /* This is the C99-specified criterion for snprintf to have
	     * been completely successful. */
	    return buf;
	} else if (len > 0) {
	    /* This is the C99 error condition: the returned length is
	     * the required buffer size not counting the NUL. */
	    size = len + 1;
	} else {
	    /* This is the pre-C99 glibc error condition: <0 means the
	     * buffer wasn't big enough, so we enlarge it a bit and hope. */
	    size += 512;
	}
	buf = sresize(buf, size, char);
    }
}

/*
 * Read an entire line of text from a file. Return a buffer
 * malloced to be as big as necessary (caller must free).
 */
char *fgetline(FILE *fp)
{
    char *ret = snewn(512, char);
    int size = 512, len = 0;
    while (fgets(ret + len, size - len, fp)) {
	len += strlen(ret + len);
	if (ret[len-1] == '\n')
	    break;		       /* got a newline, we're done */
	size = len + 512;
	ret = sresize(ret, size, char);
    }
    if (len == 0) {		       /* first fgets returned NULL */
	sfree(ret);
	return NULL;
    }
    ret[len] = '\0';
    return ret;
}

/* ----------------------------------------------------------------------
 * Base64 encoding routine. This is required in public-key writing
 * but also in HTTP proxy handling, so it's centralised here.
 */

void base64_encode_atom(unsigned char *data, int n, char *out)
{
    static const char base64_chars[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    unsigned word;

    word = data[0] << 16;
    if (n > 1)
	word |= data[1] << 8;
    if (n > 2)
	word |= data[2];
    out[0] = base64_chars[(word >> 18) & 0x3F];
    out[1] = base64_chars[(word >> 12) & 0x3F];
    if (n > 1)
	out[2] = base64_chars[(word >> 6) & 0x3F];
    else
	out[2] = '=';
    if (n > 2)
	out[3] = base64_chars[word & 0x3F];
    else
	out[3] = '=';
}

/* ----------------------------------------------------------------------
 * Generic routines to deal with send buffers: a linked list of
 * smallish blocks, with the operations
 * 
 *  - add an arbitrary amount of data to the end of the list
 *  - remove the first N bytes from the list
 *  - return a (pointer,length) pair giving some initial data in
 *    the list, suitable for passing to a send or write system
 *    call
 *  - retrieve a larger amount of initial data from the list
 *  - return the current size of the buffer chain in bytes
 */

#define BUFFER_MIN_GRANULE  512

struct bufchain_granule {
    struct bufchain_granule *next;
    char *bufpos, *bufend, *bufmax;
};

void bufchain_init(bufchain *ch)
{
    ch->head = ch->tail = NULL;
    ch->buffersize = 0;
}

void bufchain_clear(bufchain *ch)
{
    struct bufchain_granule *b;
    while (ch->head) {
	b = ch->head;
	ch->head = ch->head->next;
	sfree(b);
    }
    ch->tail = NULL;
    ch->buffersize = 0;
}

int bufchain_size(bufchain *ch)
{
    return ch->buffersize;
}

void bufchain_add(bufchain *ch, const void *data, int len)
{
    const char *buf = (const char *)data;

    if (len == 0) return;

    ch->buffersize += len;

    while (len > 0) {
	if (ch->tail && ch->tail->bufend < ch->tail->bufmax) {
	    int copylen = min(len, ch->tail->bufmax - ch->tail->bufend);
	    memcpy(ch->tail->bufend, buf, copylen);
	    buf += copylen;
	    len -= copylen;
	    ch->tail->bufend += copylen;
	}
	if (len > 0) {
	    int grainlen =
		max(sizeof(struct bufchain_granule) + len, BUFFER_MIN_GRANULE);
	    struct bufchain_granule *newbuf;
	    newbuf = smalloc(grainlen);
	    newbuf->bufpos = newbuf->bufend =
		(char *)newbuf + sizeof(struct bufchain_granule);
	    newbuf->bufmax = (char *)newbuf + grainlen;
	    newbuf->next = NULL;
	    if (ch->tail)
		ch->tail->next = newbuf;
	    else
		ch->head = newbuf;
	    ch->tail = newbuf;
	}
    }
}

void bufchain_consume(bufchain *ch, int len)
{
    struct bufchain_granule *tmp;

    assert(ch->buffersize >= len);
    while (len > 0) {
	int remlen = len;
	assert(ch->head != NULL);
	if (remlen >= ch->head->bufend - ch->head->bufpos) {
	    remlen = ch->head->bufend - ch->head->bufpos;
	    tmp = ch->head;
	    ch->head = tmp->next;
	    if (!ch->head)
		ch->tail = NULL;
	    sfree(tmp);
	} else
	    ch->head->bufpos += remlen;
	ch->buffersize -= remlen;
	len -= remlen;
    }
}

void bufchain_prefix(bufchain *ch, void **data, int *len)
{
    *len = ch->head->bufend - ch->head->bufpos;
    *data = ch->head->bufpos;
}

void bufchain_fetch(bufchain *ch, void *data, int len)
{
    struct bufchain_granule *tmp;
    char *data_c = (char *)data;

    tmp = ch->head;

    assert(ch->buffersize >= len);
    while (len > 0) {
	int remlen = len;

	assert(tmp != NULL);
	if (remlen >= tmp->bufend - tmp->bufpos)
	    remlen = tmp->bufend - tmp->bufpos;
	memcpy(data_c, tmp->bufpos, remlen);

	tmp = tmp->next;
	len -= remlen;
	data_c += remlen;
    }
}

/* ----------------------------------------------------------------------
 * My own versions of malloc, realloc and free. Because I want
 * malloc and realloc to bomb out and exit the program if they run
 * out of memory, realloc to reliably call malloc if passed a NULL
 * pointer, and free to reliably do nothing if passed a NULL
 * pointer. We can also put trace printouts in, if we need to; and
 * we can also replace the allocator with an ElectricFence-like
 * one.
 */

#ifdef MINEFIELD
void *minefield_c_malloc(size_t size);
void minefield_c_free(void *p);
void *minefield_c_realloc(void *p, size_t size);
#endif

#ifdef MALLOC_LOG
static FILE *fp = NULL;

static char *mlog_file = NULL;
static int mlog_line = 0;

void mlog(char *file, int line)
{
    mlog_file = file;
    mlog_line = line;
    if (!fp) {
	fp = fopen("putty_mem.log", "w");
	setvbuf(fp, NULL, _IONBF, BUFSIZ);
    }
    if (fp)
	fprintf(fp, "%s:%d: ", file, line);
}
#endif

void *safemalloc(size_t n, size_t size)
{
    void *p;

    if (n > INT_MAX / size) {
	p = NULL;
    } else {
	size *= n;
	if (size == 0) size = 1;
#ifdef MINEFIELD
	p = minefield_c_malloc(size);
#else
	p = malloc(size);
#endif
    }

    if (!p) {
	char str[200];
#ifdef MALLOC_LOG
	sprintf(str, "Out of memory! (%s:%d, size=%d)",
		mlog_file, mlog_line, size);
	fprintf(fp, "*** %s\n", str);
	fclose(fp);
#else
	strcpy(str, "Out of memory!");
#endif
	modalfatalbox(str);
    }
#ifdef MALLOC_LOG
    if (fp)
	fprintf(fp, "malloc(%d) returns %p\n", size, p);
#endif
    return p;
}

void *saferealloc(void *ptr, size_t n, size_t size)
{
    void *p;

    if (n > INT_MAX / size) {
	p = NULL;
    } else {
	size *= n;
	if (!ptr) {
#ifdef MINEFIELD
	    p = minefield_c_malloc(size);
#else
	    p = malloc(size);
#endif
	} else {
#ifdef MINEFIELD
	    p = minefield_c_realloc(ptr, size);
#else
	    p = realloc(ptr, size);
#endif
	}
    }

    if (!p) {
	char str[200];
#ifdef MALLOC_LOG
	sprintf(str, "Out of memory! (%s:%d, size=%d)",
		mlog_file, mlog_line, size);
	fprintf(fp, "*** %s\n", str);
	fclose(fp);
#else
	strcpy(str, "Out of memory!");
#endif
	modalfatalbox(str);
    }
#ifdef MALLOC_LOG
    if (fp)
	fprintf(fp, "realloc(%p,%d) returns %p\n", ptr, size, p);
#endif
    return p;
}

void safefree(void *ptr)
{
    if (ptr) {
#ifdef MALLOC_LOG
	if (fp)
	    fprintf(fp, "free(%p)\n", ptr);
#endif
#ifdef MINEFIELD
	minefield_c_free(ptr);
#else
	free(ptr);
#endif
    }
#ifdef MALLOC_LOG
    else if (fp)
	fprintf(fp, "freeing null pointer - no action taken\n");
#endif
}

/* ----------------------------------------------------------------------
 * Debugging routines.
 */

#ifdef DEBUG
extern void dputs(char *);             /* defined in per-platform *misc.c */

void debug_printf(char *fmt, ...)
{
    char *buf;
    va_list ap;

    va_start(ap, fmt);
    buf = dupvprintf(fmt, ap);
    dputs(buf);
    sfree(buf);
    va_end(ap);
}


void debug_memdump(void *buf, int len, int L)
{
    int i;
    unsigned char *p = buf;
    char foo[17];
    if (L) {
	int delta;
	debug_printf("\t%d (0x%x) bytes:\n", len, len);
	delta = 15 & (unsigned long int) p;
	p -= delta;
	len += delta;
    }
    for (; 0 < len; p += 16, len -= 16) {
	dputs("  ");
	if (L)
	    debug_printf("%p: ", p);
	strcpy(foo, "................");	/* sixteen dots */
	for (i = 0; i < 16 && i < len; ++i) {
	    if (&p[i] < (unsigned char *) buf) {
		dputs("   ");	       /* 3 spaces */
		foo[i] = ' ';
	    } else {
		debug_printf("%c%02.2x",
			&p[i] != (unsigned char *) buf
			&& i % 4 ? '.' : ' ', p[i]
		    );
		if (p[i] >= ' ' && p[i] <= '~')
		    foo[i] = (char) p[i];
	    }
	}
	foo[i] = '\0';
	debug_printf("%*s%s\n", (16 - i) * 3 + 2, "", foo);
    }
}

#endif				/* def DEBUG */

/*
 * Determine whether or not a Conf represents a session which can
 * sensibly be launched right now.
 */
int conf_launchable(Conf *conf)
{
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
	return conf_get_str(conf, CONF_serline)[0] != 0;
    else
	return conf_get_str(conf, CONF_host)[0] != 0;
}

char const *conf_dest(Conf *conf)
{
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
	return conf_get_str(conf, CONF_serline);
    else
	return conf_get_str(conf, CONF_host);
}

#ifndef PLATFORM_HAS_SMEMCLR
/*
 * Securely wipe memory.
 *
 * The actual wiping is no different from what memset would do: the
 * point of 'securely' is to try to be sure over-clever compilers
 * won't optimise away memsets on variables that are about to be freed
 * or go out of scope. See
 * https://buildsecurityin.us-cert.gov/bsi-rules/home/g1/771-BSI.html
 *
 * Some platforms (e.g. Windows) may provide their own version of this
 * function.
 */
void smemclr(void *b, size_t n) {
    volatile char *vp;

    if (b && n > 0) {
        /*
         * Zero out the memory.
         */
        memset(b, 0, n);

        /*
         * Perform a volatile access to the object, forcing the
         * compiler to admit that the previous memset was important.
         *
         * This while loop should in practice run for zero iterations
         * (since we know we just zeroed the object out), but in
         * theory (as far as the compiler knows) it might range over
         * the whole object. (If we had just written, say, '*vp =
         * *vp;', a compiler could in principle have 'helpfully'
         * optimised the memset into only zeroing out the first byte.
         * This should be robust.)
         */
        vp = b;
        while (*vp) vp++;
    }
}
#endif
