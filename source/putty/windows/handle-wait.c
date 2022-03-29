/*
 * handle-wait.c: Manage a collection of HANDLEs to wait for (in a
 * WaitFor{Single,Multiple}Objects sense), each with a callback to be
 * called when it's activated. Tracks the list, and provides an API to
 * event loops that let them get a list of things to wait for and a
 * way to call back to here when one of them does something.
 */

/*
 * TODO: currently this system can't cope with more than
 * MAXIMUM_WAIT_OBJECTS (= 64) handles at a time. It enforces that by
 * assertion, so we'll at least find out if that assumption is ever
 * violated.
 *
 * It should be OK for the moment. As of 2021-05-24, the only uses of
 * this system are by the ConPTY backend (just once, to watch for its
 * subprocess terminating); by Pageant (for the event that the
 * WM_COPYDATA subthread uses to signal the main thread); and by
 * named-pipe-server.c (once per named-pipe server, of which there is
 * one in Pageant and one in connection-sharing upstreams). So the
 * total number of handles has a pretty small upper bound.
 *
 * But sooner or later, I'm sure we'll find a reason why we really
 * need to watch a squillion handles at once. When that happens, I
 * can't see any alternative to setting up some kind of tree of
 * subthreads in this module, each one condensing 64 of our handles
 * into one, by doing its own WaitForMultipleObjects and setting an
 * event object to indicate that one of them did something. It'll be
 * horribly ugly.
 */

#include "putty.h"

struct HandleWait {
    HANDLE handle;
    handle_wait_callback_fn_t callback;
    void *callback_ctx;

    int index;                    /* sort key for tree234 */
};

struct HandleWaitListInner {
    HandleWait *hws[MAXIMUM_WAIT_OBJECTS];
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];

    struct HandleWaitList hwl;
};

static int handlewait_cmp(void *av, void *bv)
{
    HandleWait *a = (HandleWait *)av, *b = (HandleWait *)bv;
    if (a->index < b->index)
        return -1;
    if (a->index > b->index)
        return +1;
    return 0;
}

static tree234 *handlewaits_tree_real;

static inline tree234 *ensure_handlewaits_tree_exists(void)
{
    if (!handlewaits_tree_real)
        handlewaits_tree_real = newtree234(handlewait_cmp);
    return handlewaits_tree_real;
}

static int allocate_index(void)
{
    tree234 *t = ensure_handlewaits_tree_exists();
    search234_state st[1];

    search234_start(st, t);
    while (st->element) {
        HandleWait *hw = (HandleWait *)st->element;
        if (st->index < hw->index) {
            /* There are unused index slots to the left of this element */
            search234_step(st, -1);
        } else {
            assert(st->index == hw->index);
            search234_step(st, +1);
        }
    }

    return st->index;
}

HandleWait *add_handle_wait(HANDLE h, handle_wait_callback_fn_t callback,
                            void *callback_ctx)
{
    HandleWait *hw = snew(HandleWait);
    hw->handle = h;
    hw->callback = callback;
    hw->callback_ctx = callback_ctx;

    tree234 *t = ensure_handlewaits_tree_exists();
    hw->index = allocate_index();
    HandleWait *added = add234(t, hw);
    assert(added == hw);

    return hw;
}

void delete_handle_wait(HandleWait *hw)
{
    tree234 *t = ensure_handlewaits_tree_exists();
    HandleWait *deleted = del234(t, hw);
    assert(deleted == hw);
    sfree(hw);
}

HandleWaitList *get_handle_wait_list(void)
{
    tree234 *t = ensure_handlewaits_tree_exists();
    struct HandleWaitListInner *hwli = snew(struct HandleWaitListInner);
    size_t n = 0;
    HandleWait *hw;
    for (int i = 0; (hw = index234(t, i)) != NULL; i++) {
        assert(n < MAXIMUM_WAIT_OBJECTS);
        hwli->hws[n] = hw;
        hwli->hwl.handles[n] = hw->handle;
        n++;
    }
    hwli->hwl.nhandles = n;
    return &hwli->hwl;
}

void handle_wait_activate(HandleWaitList *hwl, int index)
{
    struct HandleWaitListInner *hwli =
        container_of(hwl, struct HandleWaitListInner, hwl);
    assert(0 <= index);
    assert(index < hwli->hwl.nhandles);
    HandleWait *hw = hwli->hws[index];
    hw->callback(hw->callback_ctx);
}

void handle_wait_list_free(HandleWaitList *hwl)
{
    struct HandleWaitListInner *hwli =
        container_of(hwl, struct HandleWaitListInner, hwl);
    sfree(hwli);
}
