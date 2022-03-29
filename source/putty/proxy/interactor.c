/*
 * Centralised functions for the Interactor trait.
 */

#include "putty.h"

Seat *interactor_borrow_seat(Interactor *itr)
{
    Seat *clientseat = interactor_get_seat(itr);
    if (!clientseat)
        return NULL;

    /* If the client has already had its Seat borrowed, then look
     * through the existing TempSeat to find the underlying one. */
    if (is_tempseat(clientseat))
        return tempseat_get_real(clientseat);

    /* Otherwise, make a new TempSeat and give that to the client. */
    Seat *tempseat = tempseat_new(clientseat);
    interactor_set_seat(itr, tempseat);
    return clientseat;
}

static Interactor *interactor_toplevel(Interactor *itr, unsigned *level_out)
{
    /*
     * Find the Interactor at the top of the chain, so that all the
     * Interactors in a stack can share that one's last-to-talk field.
     * Also, count how far we had to go to get to it, to put in the
     * message.
     */
    Interactor *itr_top = itr;
    unsigned level = 0;
    while (itr_top->parent) {
        itr_top = itr_top->parent;
        level++;
    }

    if (level_out)
        *level_out = level;
    return itr_top;
}

void interactor_return_seat(Interactor *itr)
{
    Seat *tempseat = interactor_get_seat(itr);
    if (!is_tempseat(tempseat))
        return;                        /* no-op */

    /*
     * We're about to hand this seat back to the parent Interactor to
     * do its own thing with. It will typically expect to start in the
     * same state as if the seat had never been borrowed, i.e. in the
     * starting trust state.
     *
     * However, this may be overridden by the tempseat_flush call.
     */
    Seat *realseat = tempseat_get_real(tempseat);
    seat_set_trust_status(realseat, true);

    tempseat_flush(tempseat);
    interactor_set_seat(itr, realseat);
    tempseat_free(tempseat);

    /*
     * If we have a parent Interactor, and anyone has ever called
     * interactor_announce, then all Interactors from now on will
     * announce themselves even if they have nothing to say.
     */
    Interactor *itr_top = interactor_toplevel(itr, NULL);
    if (itr_top->last_to_talk)
        interactor_announce(itr);
}

InteractionReadySeat interactor_announce(Interactor *itr)
{
    Seat *seat = interactor_get_seat(itr);
    assert(!is_tempseat(seat) &&
           "Shouldn't call announce when someone else is using our seat");

    InteractionReadySeat iseat;
    iseat.seat = seat;

    unsigned level;
    Interactor *itr_top = interactor_toplevel(itr, &level);

    /*
     * Generally, we should announce ourself if the previous
     * Interactor that said anything was not us. That includes if
     * there was no previous Interactor to talk (i.e. if we're the
     * first to say anything) - *except* that the primary Interactor
     * doesn't need to announce itself, if no proxy has intervened
     * before it.
     */
    bool need_announcement = (itr_top->last_to_talk != itr);
    if (!itr->parent && !itr_top->last_to_talk)
        need_announcement = false;

    if (need_announcement) {
        const char *prefix = "";
        if (itr_top->last_to_talk != NULL)
            seat_antispoof_msg(iseat, ""); /* leave a separating blank line */

        char *desc = interactor_description(itr);
        char *adjective = (level == 0 ? dupstr("primary") :
                           level == 1 ? dupstr("proxy") :
                           dupprintf("proxy^%u", level));
        char *msg = dupprintf("%sMaking %s %s", prefix, adjective, desc);
        sfree(adjective);
        sfree(desc);

        seat_antispoof_msg(iseat, msg);
        sfree(msg);

        itr_top->last_to_talk = itr;
    }

    return iseat;
}
