#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "putty.h"

/* ----------------------------------------------------------------------
 * String handling routines.
 */

char *dupstr(char *s)
{
    int len = strlen(s);
    char *p = smalloc(len + 1);
    strcpy(p, s);
    return p;
}

/* Allocate the concatenation of N strings. Terminate arg list with NULL. */
char *dupcat(char *s1, ...)
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

    p = smalloc(len + 1);
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

#define BUFFER_GRANULE  512

struct bufchain_granule {
    struct bufchain_granule *next;
    int buflen, bufpos;
    char buf[BUFFER_GRANULE];
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

void bufchain_add(bufchain *ch, void *data, int len)
{
    char *buf = (char *)data;

    ch->buffersize += len;

    if (ch->tail && ch->tail->buflen < BUFFER_GRANULE) {
	int copylen = min(len, BUFFER_GRANULE - ch->tail->buflen);
	memcpy(ch->tail->buf + ch->tail->buflen, buf, copylen);
	buf += copylen;
	len -= copylen;
	ch->tail->buflen += copylen;
    }
    while (len > 0) {
	int grainlen = min(len, BUFFER_GRANULE);
	struct bufchain_granule *newbuf;
	newbuf = smalloc(sizeof(struct bufchain_granule));
	newbuf->bufpos = 0;
	newbuf->buflen = grainlen;
	memcpy(newbuf->buf, buf, grainlen);
	buf += grainlen;
	len -= grainlen;
	if (ch->tail)
	    ch->tail->next = newbuf;
	else
	    ch->head = ch->tail = newbuf;
	newbuf->next = NULL;
	ch->tail = newbuf;
    }
}

void bufchain_consume(bufchain *ch, int len)
{
    struct bufchain_granule *tmp;

    assert(ch->buffersize >= len);
    while (len > 0) {
	int remlen = len;
	assert(ch->head != NULL);
	if (remlen >= ch->head->buflen - ch->head->bufpos) {
	    remlen = ch->head->buflen - ch->head->bufpos;
	    tmp = ch->head;
	    ch->head = tmp->next;
	    sfree(tmp);
	    if (!ch->head)
		ch->tail = NULL;
	} else
	    ch->head->bufpos += remlen;
	ch->buffersize -= remlen;
	len -= remlen;
    }
}

void bufchain_prefix(bufchain *ch, void **data, int *len)
{
    *len = ch->head->buflen - ch->head->bufpos;
    *data = ch->head->buf + ch->head->bufpos;
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
	if (remlen >= tmp->buflen - tmp->bufpos)
	    remlen = tmp->buflen - tmp->bufpos;
	memcpy(data_c, tmp->buf + tmp->bufpos, remlen);

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
/*
 * Minefield - a Windows equivalent for Electric Fence
 */

#define PAGESIZE 4096

/*
 * Design:
 * 
 * We start by reserving as much virtual address space as Windows
 * will sensibly (or not sensibly) let us have. We flag it all as
 * invalid memory.
 * 
 * Any allocation attempt is satisfied by committing one or more
 * pages, with an uncommitted page on either side. The returned
 * memory region is jammed up against the _end_ of the pages.
 * 
 * Freeing anything causes instantaneous decommitment of the pages
 * involved, so stale pointers are caught as soon as possible.
 */

static int minefield_initialised = 0;
static void *minefield_region = NULL;
static long minefield_size = 0;
static long minefield_npages = 0;
static long minefield_curpos = 0;
static unsigned short *minefield_admin = NULL;
static void *minefield_pages = NULL;

static void minefield_admin_hide(int hide)
{
    int access = hide ? PAGE_NOACCESS : PAGE_READWRITE;
    VirtualProtect(minefield_admin, minefield_npages * 2, access, NULL);
}

static void minefield_init(void)
{
    int size;
    int admin_size;
    int i;

    for (size = 0x40000000; size > 0; size = ((size >> 3) * 7) & ~0xFFF) {
	minefield_region = VirtualAlloc(NULL, size,
					MEM_RESERVE, PAGE_NOACCESS);
	if (minefield_region)
	    break;
    }
    minefield_size = size;

    /*
     * Firstly, allocate a section of that to be the admin block.
     * We'll need a two-byte field for each page.
     */
    minefield_admin = minefield_region;
    minefield_npages = minefield_size / PAGESIZE;
    admin_size = (minefield_npages * 2 + PAGESIZE - 1) & ~(PAGESIZE - 1);
    minefield_npages = (minefield_size - admin_size) / PAGESIZE;
    minefield_pages = (char *) minefield_region + admin_size;

    /*
     * Commit the admin region.
     */
    VirtualAlloc(minefield_admin, minefield_npages * 2,
		 MEM_COMMIT, PAGE_READWRITE);

    /*
     * Mark all pages as unused (0xFFFF).
     */
    for (i = 0; i < minefield_npages; i++)
	minefield_admin[i] = 0xFFFF;

    /*
     * Hide the admin region.
     */
    minefield_admin_hide(1);

    minefield_initialised = 1;
}

static void minefield_bomb(void)
{
    div(1, *(int *) minefield_pages);
}

static void *minefield_alloc(int size)
{
    int npages;
    int pos, lim, region_end, region_start;
    int start;
    int i;

    npages = (size + PAGESIZE - 1) / PAGESIZE;

    minefield_admin_hide(0);

    /*
     * Search from current position until we find a contiguous
     * bunch of npages+2 unused pages.
     */
    pos = minefield_curpos;
    lim = minefield_npages;
    while (1) {
	/* Skip over used pages. */
	while (pos < lim && minefield_admin[pos] != 0xFFFF)
	    pos++;
	/* Count unused pages. */
	start = pos;
	while (pos < lim && pos - start < npages + 2 &&
	       minefield_admin[pos] == 0xFFFF)
	    pos++;
	if (pos - start == npages + 2)
	    break;
	/* If we've reached the limit, reset the limit or stop. */
	if (pos >= lim) {
	    if (lim == minefield_npages) {
		/* go round and start again at zero */
		lim = minefield_curpos;
		pos = 0;
	    } else {
		minefield_admin_hide(1);
		return NULL;
	    }
	}
    }

    minefield_curpos = pos - 1;

    /*
     * We have npages+2 unused pages starting at start. We leave
     * the first and last of these alone and use the rest.
     */
    region_end = (start + npages + 1) * PAGESIZE;
    region_start = region_end - size;
    /* FIXME: could align here if we wanted */

    /*
     * Update the admin region.
     */
    for (i = start + 2; i < start + npages + 1; i++)
	minefield_admin[i] = 0xFFFE;   /* used but no region starts here */
    minefield_admin[start + 1] = region_start % PAGESIZE;

    minefield_admin_hide(1);

    VirtualAlloc((char *) minefield_pages + region_start, size,
		 MEM_COMMIT, PAGE_READWRITE);
    return (char *) minefield_pages + region_start;
}

static void minefield_free(void *ptr)
{
    int region_start, i, j;

    minefield_admin_hide(0);

    region_start = (char *) ptr - (char *) minefield_pages;
    i = region_start / PAGESIZE;
    if (i < 0 || i >= minefield_npages ||
	minefield_admin[i] != region_start % PAGESIZE)
	minefield_bomb();
    for (j = i; j < minefield_npages && minefield_admin[j] != 0xFFFF; j++) {
	minefield_admin[j] = 0xFFFF;
    }

    VirtualFree(ptr, j * PAGESIZE - region_start, MEM_DECOMMIT);

    minefield_admin_hide(1);
}

static int minefield_get_size(void *ptr)
{
    int region_start, i, j;

    minefield_admin_hide(0);

    region_start = (char *) ptr - (char *) minefield_pages;
    i = region_start / PAGESIZE;
    if (i < 0 || i >= minefield_npages ||
	minefield_admin[i] != region_start % PAGESIZE)
	minefield_bomb();
    for (j = i; j < minefield_npages && minefield_admin[j] != 0xFFFF; j++);

    minefield_admin_hide(1);

    return j * PAGESIZE - region_start;
}

static void *minefield_c_malloc(size_t size)
{
    if (!minefield_initialised)
	minefield_init();
    return minefield_alloc(size);
}

static void minefield_c_free(void *p)
{
    if (!minefield_initialised)
	minefield_init();
    minefield_free(p);
}

/*
 * realloc _always_ moves the chunk, for rapid detection of code
 * that assumes it won't.
 */
static void *minefield_c_realloc(void *p, size_t size)
{
    size_t oldsize;
    void *q;
    if (!minefield_initialised)
	minefield_init();
    q = minefield_alloc(size);
    oldsize = minefield_get_size(p);
    memcpy(q, p, (oldsize < size ? oldsize : size));
    minefield_free(p);
    return q;
}

#endif				/* MINEFIELD */

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

void *safemalloc(size_t size)
{
    void *p;
#ifdef MINEFIELD
    p = minefield_c_malloc(size);
#else
    p = malloc(size);
#endif
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
	MessageBox(NULL, str, "PuTTY Fatal Error",
		   MB_SYSTEMMODAL | MB_ICONERROR | MB_OK);
	cleanup_exit(1);
    }
#ifdef MALLOC_LOG
    if (fp)
	fprintf(fp, "malloc(%d) returns %p\n", size, p);
#endif
    return p;
}

void *saferealloc(void *ptr, size_t size)
{
    void *p;
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
	MessageBox(NULL, str, "PuTTY Fatal Error",
		   MB_SYSTEMMODAL | MB_ICONERROR | MB_OK);
	cleanup_exit(1);
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
static FILE *debug_fp = NULL;
static int debug_got_console = 0;

static void dputs(char *buf)
{
    DWORD dw;

    if (!debug_got_console) {
	AllocConsole();
	debug_got_console = 1;
    }
    if (!debug_fp) {
	debug_fp = fopen("debug.log", "w");
    }

    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), buf, strlen(buf), &dw,
	      NULL);
    fputs(buf, debug_fp);
    fflush(debug_fp);
}


void dprintf(char *fmt, ...)
{
    char buf[2048];
    va_list ap;

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    dputs(buf);
    va_end(ap);
}


void debug_memdump(void *buf, int len, int L)
{
    int i;
    unsigned char *p = buf;
    char foo[17];
    if (L) {
	int delta;
	dprintf("\t%d (0x%x) bytes:\n", len, len);
	delta = 15 & (int) p;
	p -= delta;
	len += delta;
    }
    for (; 0 < len; p += 16, len -= 16) {
	dputs("  ");
	if (L)
	    dprintf("%p: ", p);
	strcpy(foo, "................");	/* sixteen dots */
	for (i = 0; i < 16 && i < len; ++i) {
	    if (&p[i] < (unsigned char *) buf) {
		dputs("   ");	       /* 3 spaces */
		foo[i] = ' ';
	    } else {
		dprintf("%c%02.2x",
			&p[i] != (unsigned char *) buf
			&& i % 4 ? '.' : ' ', p[i]
		    );
		if (p[i] >= ' ' && p[i] <= '~')
		    foo[i] = (char) p[i];
	    }
	}
	foo[i] = '\0';
	dprintf("%*s%s\n", (16 - i) * 3 + 2, "", foo);
    }
}

#endif				/* def DEBUG */
