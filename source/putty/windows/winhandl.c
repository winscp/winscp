/*
 * winhandl.c: Module to give Windows front ends the general
 * ability to deal with consoles, pipes, serial ports, or any other
 * type of data stream accessed through a Windows API HANDLE rather
 * than a WinSock SOCKET.
 *
 * We do this by spawning a subthread to continuously try to read
 * from the handle. Every time a read successfully returns some
 * data, the subthread sets an event object which is picked up by
 * the main thread, and the main thread then sets an event in
 * return to instruct the subthread to resume reading.
 * 
 * Output works precisely the other way round, in a second
 * subthread. The output subthread should not be attempting to
 * write all the time, because it hasn't always got data _to_
 * write; so the output thread waits for an event object notifying
 * it to _attempt_ a write, and then it sets an event in return
 * when one completes.
 * 
 * (It's terribly annoying having to spawn a subthread for each
 * direction of each handle. Technically it isn't necessary for
 * serial ports, since we could use overlapped I/O within the main
 * thread and wait directly on the event objects in the OVERLAPPED
 * structures. However, we can't use this trick for some types of
 * file handle at all - for some reason Windows restricts use of
 * OVERLAPPED to files which were opened with the overlapped flag -
 * and so we must use threads for those. This being the case, it's
 * simplest just to use threads for everything rather than trying
 * to keep track of multiple completely separate mechanisms.)
 */

#include <assert.h>

#include "putty.h"

/* ----------------------------------------------------------------------
 * Generic definitions.
 */

/*
 * Maximum amount of backlog we will allow to build up on an input
 * handle before we stop reading from it.
 */
#define MAX_BACKLOG 32768

struct handle_generic {
    /*
     * Initial fields common to both handle_input and handle_output
     * structures.
     * 
     * The three HANDLEs are set up at initialisation time and are
     * thereafter read-only to both main thread and subthread.
     * `moribund' is only used by the main thread; `done' is
     * written by the main thread before signalling to the
     * subthread. `defunct' and `busy' are used only by the main
     * thread.
     */
    HANDLE h;			       /* the handle itself */
    HANDLE ev_to_main;		       /* event used to signal main thread */
    HANDLE ev_from_main;	       /* event used to signal back to us */
    bool moribund;                     /* are we going to kill this soon? */
    bool done;                         /* request subthread to terminate */
    bool defunct;                      /* has the subthread already gone? */
    bool busy;                         /* operation currently in progress? */
    void *privdata;		       /* for client to remember who they are */
};

typedef enum { HT_INPUT, HT_OUTPUT, HT_FOREIGN } HandleType;

/* ----------------------------------------------------------------------
 * Input threads.
 */

/*
 * Data required by an input thread.
 */
struct handle_input {
    /*
     * Copy of the handle_generic structure.
     */
    HANDLE h;			       /* the handle itself */
    HANDLE ev_to_main;		       /* event used to signal main thread */
    HANDLE ev_from_main;	       /* event used to signal back to us */
    bool moribund;                     /* are we going to kill this soon? */
    bool done;                         /* request subthread to terminate */
    bool defunct;                      /* has the subthread already gone? */
    bool busy;                         /* operation currently in progress? */
    void *privdata;		       /* for client to remember who they are */

    /*
     * Data set at initialisation and then read-only.
     */
    int flags;

    /*
     * Data set by the input thread before signalling ev_to_main,
     * and read by the main thread after receiving that signal.
     */
    char buffer[4096];		       /* the data read from the handle */
    DWORD len;			       /* how much data that was */
    int readerr;		       /* lets us know about read errors */

    /*
     * Callback function called by this module when data arrives on
     * an input handle.
     */
    handle_inputfn_t gotdata;
};

/*
 * The actual thread procedure for an input thread.
 */
static DWORD WINAPI handle_input_threadfunc(void *param)
{
    struct handle_input *ctx = (struct handle_input *) param;
    OVERLAPPED ovl, *povl;
    HANDLE oev;
    bool readret, finished;
    int readlen;

    if (ctx->flags & HANDLE_FLAG_OVERLAPPED) {
	povl = &ovl;
	oev = CreateEvent(NULL, true, false, NULL);
    } else {
	povl = NULL;
    }

    if (ctx->flags & HANDLE_FLAG_UNITBUFFER)
	readlen = 1;
    else
	readlen = sizeof(ctx->buffer);

    while (1) {
	if (povl) {
	    memset(povl, 0, sizeof(OVERLAPPED));
	    povl->hEvent = oev;
	}
	readret = ReadFile(ctx->h, ctx->buffer,readlen, &ctx->len, povl);
	if (!readret)
	    ctx->readerr = GetLastError();
	else
	    ctx->readerr = 0;
	if (povl && !readret && ctx->readerr == ERROR_IO_PENDING) {
	    WaitForSingleObject(povl->hEvent, INFINITE);
	    readret = GetOverlappedResult(ctx->h, povl, &ctx->len, false);
	    if (!readret)
		ctx->readerr = GetLastError();
	    else
		ctx->readerr = 0;
	}

	if (!readret) {
	    /*
	     * Windows apparently sends ERROR_BROKEN_PIPE when a
	     * pipe we're reading from is closed normally from the
	     * writing end. This is ludicrous; if that situation
	     * isn't a natural EOF, _nothing_ is. So if we get that
	     * particular error, we pretend it's EOF.
	     */
	    if (ctx->readerr == ERROR_BROKEN_PIPE)
		ctx->readerr = 0;
	    ctx->len = 0;
	}

	if (readret && ctx->len == 0 &&
	    (ctx->flags & HANDLE_FLAG_IGNOREEOF))
	    continue;

        /*
         * If we just set ctx->len to 0, that means the read operation
         * has returned end-of-file. Telling that to the main thread
         * will cause it to set its 'defunct' flag and dispose of the
         * handle structure at the next opportunity, in which case we
         * mustn't touch ctx at all after the SetEvent. (Hence we do
         * even _this_ check before the SetEvent.)
         */
        finished = (ctx->len == 0);

	SetEvent(ctx->ev_to_main);

	if (finished)
	    break;

	WaitForSingleObject(ctx->ev_from_main, INFINITE);
	if (ctx->done) {
            /*
             * The main thread has asked us to shut down. Send back an
             * event indicating that we've done so. Hereafter we must
             * not touch ctx at all, because the main thread might
             * have freed it.
             */
            SetEvent(ctx->ev_to_main);
            break;
        }
    }

    if (povl)
	CloseHandle(oev);

    return 0;
}

/*
 * This is called after a succcessful read, or from the
 * `unthrottle' function. It decides whether or not to begin a new
 * read operation.
 */
static void handle_throttle(struct handle_input *ctx, int backlog)
{
    if (ctx->defunct)
	return;

    /*
     * If there's a read operation already in progress, do nothing:
     * when that completes, we'll come back here and be in a
     * position to make a better decision.
     */
    if (ctx->busy)
	return;

    /*
     * Otherwise, we must decide whether to start a new read based
     * on the size of the backlog.
     */
    if (backlog < MAX_BACKLOG) {
	SetEvent(ctx->ev_from_main);
	ctx->busy = true;
    }
}

/* ----------------------------------------------------------------------
 * Output threads.
 */

/*
 * Data required by an output thread.
 */
struct handle_output {
    /*
     * Copy of the handle_generic structure.
     */
    HANDLE h;			       /* the handle itself */
    HANDLE ev_to_main;		       /* event used to signal main thread */
    HANDLE ev_from_main;	       /* event used to signal back to us */
    bool moribund;                     /* are we going to kill this soon? */
    bool done;                         /* request subthread to terminate */
    bool defunct;                      /* has the subthread already gone? */
    bool busy;                         /* operation currently in progress? */
    void *privdata;		       /* for client to remember who they are */

    /*
     * Data set at initialisation and then read-only.
     */
    int flags;

    /*
     * Data set by the main thread before signalling ev_from_main,
     * and read by the input thread after receiving that signal.
     */
    const char *buffer;                /* the data to write */
    DWORD len;			       /* how much data there is */

    /*
     * Data set by the input thread before signalling ev_to_main,
     * and read by the main thread after receiving that signal.
     */
    DWORD lenwritten;		       /* how much data we actually wrote */
    int writeerr;		       /* return value from WriteFile */

    /*
     * Data only ever read or written by the main thread.
     */
    bufchain queued_data;	       /* data still waiting to be written */
    enum { EOF_NO, EOF_PENDING, EOF_SENT } outgoingeof;

    /*
     * Callback function called when the backlog in the bufchain
     * drops.
     */
    handle_outputfn_t sentdata;
};

static DWORD WINAPI handle_output_threadfunc(void *param)
{
    struct handle_output *ctx = (struct handle_output *) param;
    OVERLAPPED ovl, *povl;
    HANDLE oev;
    bool writeret;

    if (ctx->flags & HANDLE_FLAG_OVERLAPPED) {
	povl = &ovl;
	oev = CreateEvent(NULL, true, false, NULL);
    } else {
	povl = NULL;
    }

    while (1) {
	WaitForSingleObject(ctx->ev_from_main, INFINITE);
	if (ctx->done) {
            /*
             * The main thread has asked us to shut down. Send back an
             * event indicating that we've done so. Hereafter we must
             * not touch ctx at all, because the main thread might
             * have freed it.
             */
	    SetEvent(ctx->ev_to_main);
	    break;
	}
	if (povl) {
	    memset(povl, 0, sizeof(OVERLAPPED));
	    povl->hEvent = oev;
	}

	writeret = WriteFile(ctx->h, ctx->buffer, ctx->len,
			     &ctx->lenwritten, povl);
	if (!writeret)
	    ctx->writeerr = GetLastError();
	else
	    ctx->writeerr = 0;
	if (povl && !writeret && GetLastError() == ERROR_IO_PENDING) {
	    writeret = GetOverlappedResult(ctx->h, povl,
					   &ctx->lenwritten, true);
	    if (!writeret)
		ctx->writeerr = GetLastError();
	    else
		ctx->writeerr = 0;
	}

	SetEvent(ctx->ev_to_main);
	if (!writeret) {
            /*
             * The write operation has suffered an error. Telling that
             * to the main thread will cause it to set its 'defunct'
             * flag and dispose of the handle structure at the next
             * opportunity, so we must not touch ctx at all after
             * this.
             */
	    break;
        }
    }

    if (povl)
	CloseHandle(oev);

    return 0;
}

static void handle_try_output(struct handle_output *ctx)
{
    if (!ctx->busy && bufchain_size(&ctx->queued_data)) {
	ptrlen data = bufchain_prefix(&ctx->queued_data);
	ctx->buffer = data.ptr;
	ctx->len = min(data.len, ~(DWORD)0);
	SetEvent(ctx->ev_from_main);
	ctx->busy = true;
    } else if (!ctx->busy && bufchain_size(&ctx->queued_data) == 0 &&
               ctx->outgoingeof == EOF_PENDING) {
        CloseHandle(ctx->h);
        ctx->h = INVALID_HANDLE_VALUE;
        ctx->outgoingeof = EOF_SENT;
    }
}

/* ----------------------------------------------------------------------
 * 'Foreign events'. These are handle structures which just contain a
 * single event object passed to us by another module such as
 * winnps.c, so that they can make use of our handle_get_events /
 * handle_got_event mechanism for communicating with application main
 * loops.
 */
struct handle_foreign {
    /*
     * Copy of the handle_generic structure.
     */
    HANDLE h;			       /* the handle itself */
    HANDLE ev_to_main;		       /* event used to signal main thread */
    HANDLE ev_from_main;	       /* event used to signal back to us */
    bool moribund;                     /* are we going to kill this soon? */
    bool done;                         /* request subthread to terminate */
    bool defunct;                      /* has the subthread already gone? */
    bool busy;                         /* operation currently in progress? */
    void *privdata;		       /* for client to remember who they are */

    /*
     * Our own data, just consisting of knowledge of who to call back.
     */
    void (*callback)(void *);
    void *ctx;
};

/* ----------------------------------------------------------------------
 * Unified code handling both input and output threads.
 */

struct handle {
    HandleType type;
    union {
	struct handle_generic g;
	struct handle_input i;
	struct handle_output o;
	struct handle_foreign f;
    } u;
};

static tree234 *handles_by_evtomain;

static int handle_cmp_evtomain(void *av, void *bv)
{
    struct handle *a = (struct handle *)av;
    struct handle *b = (struct handle *)bv;

    if ((uintptr_t)a->u.g.ev_to_main < (uintptr_t)b->u.g.ev_to_main)
	return -1;
    else if ((uintptr_t)a->u.g.ev_to_main > (uintptr_t)b->u.g.ev_to_main)
	return +1;
    else
	return 0;
}

static int handle_find_evtomain(void *av, void *bv)
{
    HANDLE *a = (HANDLE *)av;
    struct handle *b = (struct handle *)bv;

    if ((uintptr_t)*a < (uintptr_t)b->u.g.ev_to_main)
	return -1;
    else if ((uintptr_t)*a > (uintptr_t)b->u.g.ev_to_main)
	return +1;
    else
	return 0;
}

struct handle *handle_input_new(HANDLE handle, handle_inputfn_t gotdata,
				void *privdata, int flags)
{
    struct handle *h = snew(struct handle);
    DWORD in_threadid; /* required for Win9x */

    h->type = HT_INPUT;
    h->u.i.h = handle;
    h->u.i.ev_to_main = CreateEvent(NULL, false, false, NULL);
    h->u.i.ev_from_main = CreateEvent(NULL, false, false, NULL);
    h->u.i.gotdata = gotdata;
    h->u.i.defunct = false;
    h->u.i.moribund = false;
    h->u.i.done = false;
    h->u.i.privdata = privdata;
    h->u.i.flags = flags;

    if (!handles_by_evtomain)
	handles_by_evtomain = newtree234(handle_cmp_evtomain);
    add234(handles_by_evtomain, h);

    CreateThread(NULL, 0, handle_input_threadfunc,
		 &h->u.i, 0, &in_threadid);
    h->u.i.busy = true;

    return h;
}

struct handle *handle_output_new(HANDLE handle, handle_outputfn_t sentdata,
				 void *privdata, int flags)
{
    struct handle *h = snew(struct handle);
    DWORD out_threadid; /* required for Win9x */

    h->type = HT_OUTPUT;
    h->u.o.h = handle;
    h->u.o.ev_to_main = CreateEvent(NULL, false, false, NULL);
    h->u.o.ev_from_main = CreateEvent(NULL, false, false, NULL);
    h->u.o.busy = false;
    h->u.o.defunct = false;
    h->u.o.moribund = false;
    h->u.o.done = false;
    h->u.o.privdata = privdata;
    bufchain_init(&h->u.o.queued_data);
    h->u.o.outgoingeof = EOF_NO;
    h->u.o.sentdata = sentdata;
    h->u.o.flags = flags;

    if (!handles_by_evtomain)
	handles_by_evtomain = newtree234(handle_cmp_evtomain);
    add234(handles_by_evtomain, h);

    CreateThread(NULL, 0, handle_output_threadfunc,
		 &h->u.o, 0, &out_threadid);

    return h;
}

struct handle *handle_add_foreign_event(HANDLE event,
                                        void (*callback)(void *), void *ctx)
{
    struct handle *h = snew(struct handle);

    h->type = HT_FOREIGN;
    h->u.f.h = INVALID_HANDLE_VALUE;
    h->u.f.ev_to_main = event;
    h->u.f.ev_from_main = INVALID_HANDLE_VALUE;
    h->u.f.defunct = true;  /* we have no thread in the first place */
    h->u.f.moribund = false;
    h->u.f.done = false;
    h->u.f.privdata = NULL;
    h->u.f.callback = callback;
    h->u.f.ctx = ctx;
    h->u.f.busy = true;

    if (!handles_by_evtomain)
	handles_by_evtomain = newtree234(handle_cmp_evtomain);
    add234(handles_by_evtomain, h);

    return h;
}

size_t handle_write(struct handle *h, const void *data, size_t len)
{
    assert(h->type == HT_OUTPUT);
    assert(h->u.o.outgoingeof == EOF_NO);
    bufchain_add(&h->u.o.queued_data, data, len);
    handle_try_output(&h->u.o);
    return bufchain_size(&h->u.o.queued_data);
}

void handle_write_eof(struct handle *h)
{
    /*
     * This function is called when we want to proactively send an
     * end-of-file notification on the handle. We can only do this by
     * actually closing the handle - so never call this on a
     * bidirectional handle if we're still interested in its incoming
     * direction!
     */
    assert(h->type == HT_OUTPUT);
    if (h->u.o.outgoingeof == EOF_NO) {
        h->u.o.outgoingeof = EOF_PENDING;
        handle_try_output(&h->u.o);
    }
}

HANDLE *handle_get_events(int *nevents)
{
    HANDLE *ret;
    struct handle *h;
    int i;
    size_t n, size;

    /*
     * Go through our tree counting the handle objects currently
     * engaged in useful activity.
     */
    ret = NULL;
    n = size = 0;
    if (handles_by_evtomain) {
	for (i = 0; (h = index234(handles_by_evtomain, i)) != NULL; i++) {
	    if (h->u.g.busy) {
                sgrowarray(ret, size, n);
		ret[n++] = h->u.g.ev_to_main;
	    }
	}
    }

    *nevents = n;
    return ret;
}

static void handle_destroy(struct handle *h)
{
    if (h->type == HT_OUTPUT)
	bufchain_clear(&h->u.o.queued_data);
    CloseHandle(h->u.g.ev_from_main);
    CloseHandle(h->u.g.ev_to_main);
    del234(handles_by_evtomain, h);
    sfree(h);
}

void handle_free(struct handle *h)
{
    assert(h && !h->u.g.moribund);
    if (h->u.g.busy && h->type != HT_FOREIGN) {
        /*
         * If the handle is currently busy, we cannot immediately free
         * it, because its subthread is in the middle of something.
         * (Exception: foreign handles don't have a subthread.)
         *
         * Instead we must wait until it's finished its current
         * operation, because otherwise the subthread will write to
         * invalid memory after we free its context from under it. So
         * we set the moribund flag, which will be noticed next time
         * an operation completes.
	 */
	h->u.g.moribund = true;
    } else if (h->u.g.defunct) {
	/*
	 * There isn't even a subthread; we can go straight to
	 * handle_destroy.
	 */
	handle_destroy(h);
    } else {
	/*
	 * The subthread is alive but not busy, so we now signal it
	 * to die. Set the moribund flag to indicate that it will
	 * want destroying after that.
	 */
	h->u.g.moribund = true;
	h->u.g.done = true;
	h->u.g.busy = true;
	SetEvent(h->u.g.ev_from_main);
    }
}

void handle_got_event(HANDLE event)
{
    struct handle *h;

    assert(handles_by_evtomain);
    h = find234(handles_by_evtomain, &event, handle_find_evtomain);
    if (!h) {
	/*
	 * This isn't an error condition. If two or more event
	 * objects were signalled during the same select operation,
	 * and processing of the first caused the second handle to
	 * be closed, then it will sometimes happen that we receive
	 * an event notification here for a handle which is already
	 * deceased. In that situation we simply do nothing.
	 */
	return;
    }

    if (h->u.g.moribund) {
	/*
	 * A moribund handle is one which we have either already
	 * signalled to die, or are waiting until its current I/O op
	 * completes to do so. Either way, it's treated as already
	 * dead from the external user's point of view, so we ignore
	 * the actual I/O result. We just signal the thread to die if
	 * we haven't yet done so, or destroy the handle if not.
	 */
	if (h->u.g.done) {
	    handle_destroy(h);
	} else {
	    h->u.g.done = true;
	    h->u.g.busy = true;
	    SetEvent(h->u.g.ev_from_main);
	}
	return;
    }

    switch (h->type) {
	int backlog;

      case HT_INPUT:
	h->u.i.busy = false;

	/*
	 * A signal on an input handle means data has arrived.
	 */
	if (h->u.i.len == 0) {
	    /*
	     * EOF, or (nearly equivalently) read error.
	     */
	    h->u.i.defunct = true;
	    h->u.i.gotdata(h, NULL, 0, h->u.i.readerr);
	} else {
	    backlog = h->u.i.gotdata(h, h->u.i.buffer, h->u.i.len, 0);
	    handle_throttle(&h->u.i, backlog);
	}
        break;

      case HT_OUTPUT:
	h->u.o.busy = false;

	/*
	 * A signal on an output handle means we have completed a
	 * write. Call the callback to indicate that the output
	 * buffer size has decreased, or to indicate an error.
	 */
	if (h->u.o.writeerr) {
	    /*
	     * Write error. Send a negative value to the callback,
	     * and mark the thread as defunct (because the output
	     * thread is terminating by now).
	     */
	    h->u.o.defunct = true;
	    h->u.o.sentdata(h, 0, h->u.o.writeerr);
	} else {
	    bufchain_consume(&h->u.o.queued_data, h->u.o.lenwritten);
            noise_ultralight(NOISE_SOURCE_IOLEN, h->u.o.lenwritten);
	    h->u.o.sentdata(h, bufchain_size(&h->u.o.queued_data), 0);
	    handle_try_output(&h->u.o);
	}
        break;

      case HT_FOREIGN:
        /* Just call the callback. */
        h->u.f.callback(h->u.f.ctx);
        break;
    }
}

void handle_unthrottle(struct handle *h, size_t backlog)
{
    assert(h->type == HT_INPUT);
    handle_throttle(&h->u.i, backlog);
}

size_t handle_backlog(struct handle *h)
{
    assert(h->type == HT_OUTPUT);
    return bufchain_size(&h->u.o.queued_data);
}

void *handle_get_privdata(struct handle *h)
{
    return h->u.g.privdata;
}

static void handle_sink_write(BinarySink *bs, const void *data, size_t len)
{
    handle_sink *sink = BinarySink_DOWNCAST(bs, handle_sink);
    handle_write(sink->h, data, len);
}

void handle_sink_init(handle_sink *sink, struct handle *h)
{
    sink->h = h;
    BinarySink_INIT(sink, handle_sink_write);
}
