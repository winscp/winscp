/*
 * Platform-independent utility routines used throughout this code base.
 *
 * This file is linked into stand-alone test utilities which only want
 * to include the things they really need, so functions in here should
 * avoid depending on any functions outside it. Utility routines that
 * are more tightly integrated into the main code should live in
 * misc.c.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <ctype.h>
#include <assert.h>

#include "defs.h"
#include "misc.h"

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

/*
 * Find a character in a string, unless it's a colon contained within
 * square brackets. Used for untangling strings of the form
 * 'host:port', where host can be an IPv6 literal.
 *
 * We provide several variants of this function, with semantics like
 * various standard string.h functions.
 */
static const char *host_strchr_internal(const char *s, const char *set,
                                        bool first)
{
    int brackets = 0;
    const char *ret = NULL;

    while (1) {
        if (!*s)
            return ret;

        if (*s == '[')
            brackets++;
        else if (*s == ']' && brackets > 0)
            brackets--;
        else if (brackets && *s == ':')
            /* never match */ ;
        else if (strchr(set, *s)) {
            ret = s;
            if (first)
                return ret;
        }

        s++;
    }
}
size_t host_strcspn(const char *s, const char *set)
{
    const char *answer = host_strchr_internal(s, set, true);
    if (answer)
        return answer - s;
    else
        return strlen(s);
}
char *host_strchr(const char *s, int c)
{
    char set[2];
    set[0] = c;
    set[1] = '\0';
    return (char *) host_strchr_internal(s, set, true);
}
char *host_strrchr(const char *s, int c)
{
    char set[2];
    set[0] = c;
    set[1] = '\0';
    return (char *) host_strchr_internal(s, set, false);
}

#ifdef TEST_HOST_STRFOO
int main(void)
{
    int passes = 0, fails = 0;

#define TEST1(func, string, arg2, suffix, result) do                    \
    {                                                                   \
        const char *str = string;                                       \
        unsigned ret = func(string, arg2) suffix;                       \
        if (ret == result) {                                            \
            passes++;                                                   \
        } else {                                                        \
            printf("fail: %s(%s,%s)%s = %u, expected %u\n",             \
                   #func, #string, #arg2, #suffix, ret,                 \
                   (unsigned)result);                                   \
            fails++;                                                    \
        }                                                               \
} while (0)

    TEST1(host_strchr, "[1:2:3]:4:5", ':', -str, 7);
    TEST1(host_strrchr, "[1:2:3]:4:5", ':', -str, 9);
    TEST1(host_strcspn, "[1:2:3]:4:5", "/:",, 7);
    TEST1(host_strchr, "[1:2:3]", ':', == NULL, 1);
    TEST1(host_strrchr, "[1:2:3]", ':', == NULL, 1);
    TEST1(host_strcspn, "[1:2:3]", "/:",, 7);
    TEST1(host_strcspn, "[1:2/3]", "/:",, 4);
    TEST1(host_strcspn, "[1:2:3]/", "/:",, 7);

    printf("passed %d failed %d total %d\n", passes, fails, passes+fails);
    return fails != 0 ? 1 : 0;
}
/* Stubs to stop the rest of this module causing compile failures. */
void modalfatalbox(const char *fmt, ...) {}
int conf_get_int(Conf *conf, int primary) { return 0; }
char *conf_get_str(Conf *conf, int primary) { return NULL; }
#endif /* TEST_HOST_STRFOO */

/*
 * Trim square brackets off the outside of an IPv6 address literal.
 * Leave all other strings unchanged. Returns a fresh dynamically
 * allocated string.
 */
char *host_strduptrim(const char *s)
{
    if (s[0] == '[') {
        const char *p = s+1;
        int colons = 0;
        while (*p && *p != ']') {
            if (isxdigit((unsigned char)*p))
                /* OK */;
            else if (*p == ':')
                colons++;
            else
                break;
            p++;
        }
        if (*p == '%') {
            /*
             * This delimiter character introduces an RFC 4007 scope
             * id suffix (e.g. suffixing the address literal with
             * %eth1 or %2 or some such). There's no syntax
             * specification for the scope id, so just accept anything
             * except the closing ].
             */
            p += strcspn(p, "]");
        }
        if (*p == ']' && !p[1] && colons > 1) {
            /*
             * This looks like an IPv6 address literal (hex digits and
             * at least two colons, plus optional scope id, contained
             * in square brackets). Trim off the brackets.
             */
            return dupprintf("%.*s", (int)(p - (s+1)), s+1);
        }
    }

    /*
     * Any other shape of string is simply duplicated.
     */
    return dupstr(s);
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

int string_length_for_printf(size_t s)
{
    /* Truncate absurdly long strings (should one show up) to fit
     * within a positive 'int', which is what the "%.*s" format will
     * expect. */
    if (s > INT_MAX)
        return INT_MAX;
    return s;
}

/* Work around lack of va_copy in old MSC */
#if defined _MSC_VER && !defined va_copy
#define va_copy(a, b) TYPECHECK(                        \
        (va_list *)0 == &(a) && (va_list *)0 == &(b),   \
        memcpy(&a, &b, sizeof(va_list)))
#endif

/* Also lack of vsnprintf before VS2015 */
#if defined _WINDOWS && !defined __WINE__ && _MSC_VER < 1900
#define vsnprintf _vsnprintf
#endif

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
static char *dupvprintf_inner(char *buf, size_t oldlen, size_t *sizeptr,
                              const char *fmt, va_list ap)
{
    size_t size = *sizeptr;
    sgrowarrayn_nm(buf, size, oldlen, 512);

    while (1) {
	va_list aq;
	va_copy(aq, ap);
	int len = vsnprintf(buf + oldlen, size - oldlen, fmt, aq);
	va_end(aq);

	if (len >= 0 && len < size) {
	    /* This is the C99-specified criterion for snprintf to have
	     * been completely successful. */
            *sizeptr = size;
	    return buf;
	} else if (len > 0) {
	    /* This is the C99 error condition: the returned length is
	     * the required buffer size not counting the NUL. */
	    sgrowarrayn_nm(buf, size, oldlen + 1, len);
	} else {
	    /* This is the pre-C99 glibc error condition: <0 means the
	     * buffer wasn't big enough, so we enlarge it a bit and hope. */
	    sgrowarray_nm(buf, size, size);
	}
    }
}

char *dupvprintf(const char *fmt, va_list ap)
{
    size_t size = 0;
    return dupvprintf_inner(NULL, 0, &size, fmt, ap);
}
char *dupprintf(const char *fmt, ...)
{
    char *ret;
    va_list ap;
    va_start(ap, fmt);
    ret = dupvprintf(fmt, ap);
    va_end(ap);
    return ret;
}

struct strbuf_impl {
    size_t size;
    struct strbuf visible;
    bool nm;          /* true if we insist on non-moving buffer resizes */
};

#define STRBUF_SET_UPTR(buf)                                    \
    ((buf)->visible.u = (unsigned char *)(buf)->visible.s)
#define STRBUF_SET_PTR(buf, ptr)                                \
    ((buf)->visible.s = (ptr), STRBUF_SET_UPTR(buf))

void *strbuf_append(strbuf *buf_o, size_t len)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    char *toret;
    sgrowarray_general(
        buf->visible.s, buf->size, buf->visible.len + 1, len, buf->nm);
    STRBUF_SET_UPTR(buf);
    toret = buf->visible.s + buf->visible.len;
    buf->visible.len += len;
    buf->visible.s[buf->visible.len] = '\0';
    return toret;
}

static void strbuf_BinarySink_write(
    BinarySink *bs, const void *data, size_t len)
{
    strbuf *buf_o = BinarySink_DOWNCAST(bs, strbuf);
    memcpy(strbuf_append(buf_o, len), data, len);
}

static strbuf *strbuf_new_general(bool nm)
{
    struct strbuf_impl *buf = snew(struct strbuf_impl);
    BinarySink_INIT(&buf->visible, strbuf_BinarySink_write);
    buf->visible.len = 0;
    buf->size = 512;
    buf->nm = nm;
    STRBUF_SET_PTR(buf, snewn(buf->size, char));
    *buf->visible.s = '\0';
    return &buf->visible;
}
strbuf *strbuf_new(void) { return strbuf_new_general(false); }
strbuf *strbuf_new_nm(void) { return strbuf_new_general(true); }
void strbuf_free(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    if (buf->visible.s) {
        smemclr(buf->visible.s, buf->size);
        sfree(buf->visible.s);
    }
    sfree(buf);
}
char *strbuf_to_str(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    char *ret = buf->visible.s;
    sfree(buf);
    return ret;
}
void strbuf_catfv(strbuf *buf_o, const char *fmt, va_list ap)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    STRBUF_SET_PTR(buf, dupvprintf_inner(buf->visible.s, buf->visible.len,
                                         &buf->size, fmt, ap));
    buf->visible.len += strlen(buf->visible.s + buf->visible.len);
}
void strbuf_catf(strbuf *buf_o, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strbuf_catfv(buf_o, fmt, ap);
    va_end(ap);
}

strbuf *strbuf_new_for_agent_query(void)
{
    strbuf *buf = strbuf_new();
    strbuf_append(buf, 4);
    return buf;
}
void strbuf_finalise_agent_query(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    assert(buf->visible.len >= 5);
    PUT_32BIT_MSB_FIRST(buf->visible.u, buf->visible.len - 4);
}

/*
 * Read an entire line of text from a file. Return a buffer
 * malloced to be as big as necessary (caller must free).
 */
char *fgetline(FILE *fp)
{
    char *ret = snewn(512, char);
    size_t size = 512, len = 0;
    while (fgets(ret + len, size - len, fp)) {
	len += strlen(ret + len);
	if (len > 0 && ret[len-1] == '\n')
	    break;		       /* got a newline, we're done */
        sgrowarrayn_nm(ret, size, len, 512);
    }
    if (len == 0) {		       /* first fgets returned NULL */
	sfree(ret);
	return NULL;
    }
    ret[len] = '\0';
    return ret;
}

/*
 * Perl-style 'chomp', for a line we just read with fgetline. Unlike
 * Perl chomp, however, we're deliberately forgiving of strange
 * line-ending conventions. Also we forgive NULL on input, so you can
 * just write 'line = chomp(fgetline(fp));' and not bother checking
 * for NULL until afterwards.
 */
char *chomp(char *str)
{
    if (str) {
        int len = strlen(str);
        while (len > 0 && (str[len-1] == '\r' || str[len-1] == '\n'))
            len--;
        str[len] = '\0';
    }
    return str;
}

/* ----------------------------------------------------------------------
 * Core base64 encoding and decoding routines.
 */

void base64_encode_atom(const unsigned char *data, int n, char *out)
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

int base64_decode_atom(const char *atom, unsigned char *out)
{
    int vals[4];
    int i, v, len;
    unsigned word;
    char c;

    for (i = 0; i < 4; i++) {
	c = atom[i];
	if (c >= 'A' && c <= 'Z')
	    v = c - 'A';
	else if (c >= 'a' && c <= 'z')
	    v = c - 'a' + 26;
	else if (c >= '0' && c <= '9')
	    v = c - '0' + 52;
	else if (c == '+')
	    v = 62;
	else if (c == '/')
	    v = 63;
	else if (c == '=')
	    v = -1;
	else
	    return 0;		       /* invalid atom */
	vals[i] = v;
    }

    if (vals[0] == -1 || vals[1] == -1)
	return 0;
    if (vals[2] == -1 && vals[3] != -1)
	return 0;

    if (vals[3] != -1)
	len = 3;
    else if (vals[2] != -1)
	len = 2;
    else
	len = 1;

    word = ((vals[0] << 18) |
	    (vals[1] << 12) | ((vals[2] & 0x3F) << 6) | (vals[3] & 0x3F));
    out[0] = (word >> 16) & 0xFF;
    if (len > 1)
	out[1] = (word >> 8) & 0xFF;
    if (len > 2)
	out[2] = word & 0xFF;
    return len;
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

static void uninitialised_queue_idempotent_callback(IdempotentCallback *ic)
{
    unreachable("bufchain callback used while uninitialised");
}

void bufchain_init(bufchain *ch)
{
    ch->head = ch->tail = NULL;
    ch->buffersize = 0;
    ch->ic = NULL;
    ch->queue_idempotent_callback = uninitialised_queue_idempotent_callback;
}

void bufchain_clear(bufchain *ch)
{
    struct bufchain_granule *b;
    while (ch->head) {
	b = ch->head;
	ch->head = ch->head->next;
        smemclr(b, sizeof(*b));
	sfree(b);
    }
    ch->tail = NULL;
    ch->buffersize = 0;
}

size_t bufchain_size(bufchain *ch)
{
    return ch->buffersize;
}

void bufchain_set_callback_inner(
    bufchain *ch, IdempotentCallback *ic,
    void (*queue_idempotent_callback)(IdempotentCallback *ic))
{
    ch->queue_idempotent_callback = queue_idempotent_callback;
    ch->ic = ic;
}

void bufchain_add(bufchain *ch, const void *data, size_t len)
{
    const char *buf = (const char *)data;

    if (len == 0) return;

    ch->buffersize += len;

    while (len > 0) {
	if (ch->tail && ch->tail->bufend < ch->tail->bufmax) {
	    size_t copylen = min(len, ch->tail->bufmax - ch->tail->bufend);
	    memcpy(ch->tail->bufend, buf, copylen);
	    buf += copylen;
	    len -= copylen;
	    ch->tail->bufend += copylen;
	}
	if (len > 0) {
	    size_t grainlen =
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

    if (ch->ic)
        ch->queue_idempotent_callback(ch->ic);
}

void bufchain_consume(bufchain *ch, size_t len)
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
            smemclr(tmp, sizeof(*tmp));
	    sfree(tmp);
	} else
	    ch->head->bufpos += remlen;
	ch->buffersize -= remlen;
	len -= remlen;
    }
}

ptrlen bufchain_prefix(bufchain *ch)
{
    return make_ptrlen(ch->head->bufpos, ch->head->bufend - ch->head->bufpos);
}

void bufchain_fetch(bufchain *ch, void *data, size_t len)
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

void bufchain_fetch_consume(bufchain *ch, void *data, size_t len)
{
    bufchain_fetch(ch, data, len);
    bufchain_consume(ch, len);
}

bool bufchain_try_fetch_consume(bufchain *ch, void *data, size_t len)
{
    if (ch->buffersize >= len) {
        bufchain_fetch_consume(ch, data, len);
        return true;
    } else {
        return false;
    }
}

size_t bufchain_fetch_consume_up_to(bufchain *ch, void *data, size_t len)
{
    if (len > ch->buffersize)
        len = ch->buffersize;
    if (len)
        bufchain_fetch_consume(ch, data, len);
    return len;
}

/* ----------------------------------------------------------------------
 * Debugging routines.
 */

#ifdef DEBUG
extern void dputs(const char *); /* defined in per-platform *misc.c */

void debug_printf(const char *fmt, ...)
{
    char *buf;
    va_list ap;

    va_start(ap, fmt);
    buf = dupvprintf(fmt, ap);
    dputs(buf);
    sfree(buf);
    va_end(ap);
}

void debug_memdump(const void *buf, int len, bool L)
{
    int i;
    const unsigned char *p = buf;
    char foo[17];
    if (L) {
	int delta;
	debug_printf("\t%d (0x%x) bytes:\n", len, len);
	delta = 15 & (uintptr_t)p;
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

bool smemeq(const void *av, const void *bv, size_t len)
{
    const unsigned char *a = (const unsigned char *)av;
    const unsigned char *b = (const unsigned char *)bv;
    unsigned val = 0;

    while (len-- > 0) {
        val |= *a++ ^ *b++;
    }
    /* Now val is 0 iff we want to return 1, and in the range
     * 0x01..0xFF iff we want to return 0. So subtracting from 0x100
     * will clear bit 8 iff we want to return 0, and leave it set iff
     * we want to return 1, so then we can just shift down. */
    return (0x100 - val) >> 8;
}

int nullstrcmp(const char *a, const char *b)
{
    if (a == NULL && b == NULL)
        return 0;
    if (a == NULL)
        return -1;
    if (b == NULL)
        return +1;
    return strcmp(a, b);
}

bool ptrlen_eq_string(ptrlen pl, const char *str)
{
    size_t len = strlen(str);
    return (pl.len == len && !memcmp(pl.ptr, str, len));
}

bool ptrlen_eq_ptrlen(ptrlen pl1, ptrlen pl2)
{
    return (pl1.len == pl2.len && !memcmp(pl1.ptr, pl2.ptr, pl1.len));
}

int ptrlen_strcmp(ptrlen pl1, ptrlen pl2)
{
    size_t minlen = pl1.len < pl2.len ? pl1.len : pl2.len;
    if (minlen) {  /* tolerate plX.ptr==NULL as long as plX.len==0 */
        int cmp = memcmp(pl1.ptr, pl2.ptr, minlen);
        if (cmp)
            return cmp;
    }
    return pl1.len < pl2.len ? -1 : pl1.len > pl2.len ? +1 : 0;
}

bool ptrlen_startswith(ptrlen whole, ptrlen prefix, ptrlen *tail)
{
    if (whole.len >= prefix.len &&
        !memcmp(whole.ptr, prefix.ptr, prefix.len)) {
        if (tail) {
            tail->ptr = (const char *)whole.ptr + prefix.len;
            tail->len = whole.len - prefix.len;
        }
        return true;
    }
    return false;
}

bool ptrlen_endswith(ptrlen whole, ptrlen suffix, ptrlen *tail)
{
    if (whole.len >= suffix.len &&
        !memcmp((char *)whole.ptr + (whole.len - suffix.len),
                suffix.ptr, suffix.len)) {
        if (tail) {
            tail->ptr = whole.ptr;
            tail->len = whole.len - suffix.len;
        }
        return true;
    }
    return false;
}

char *mkstr(ptrlen pl)
{
    char *p = snewn(pl.len + 1, char);
    memcpy(p, pl.ptr, pl.len);
    p[pl.len] = '\0';
    return p;
}

bool strstartswith(const char *s, const char *t)
{
    return !memcmp(s, t, strlen(t));
}

bool strendswith(const char *s, const char *t)
{
    size_t slen = strlen(s), tlen = strlen(t);
    return slen >= tlen && !strcmp(s + (slen - tlen), t);
}

size_t encode_utf8(void *output, unsigned long ch)
{
    unsigned char *start = (unsigned char *)output, *p = start;

    if (ch < 0x80) {
        *p++ = ch;
    } else if (ch < 0x800) {
        *p++ = 0xC0 | (ch >> 6);
        *p++ = 0x80 | (ch & 0x3F);
    } else if (ch < 0x10000) {
        *p++ = 0xE0 | (ch >> 12);
        *p++ = 0x80 | ((ch >> 6) & 0x3F);
        *p++ = 0x80 | (ch & 0x3F);
    } else {
        *p++ = 0xF0 | (ch >> 18);
        *p++ = 0x80 | ((ch >> 12) & 0x3F);
        *p++ = 0x80 | ((ch >> 6) & 0x3F);
        *p++ = 0x80 | (ch & 0x3F);
    }
    return p - start;
}
