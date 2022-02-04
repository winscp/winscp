/*
 * Securely wipe memory.
 *
 * The actual wiping is no different from what memset would do: the
 * point of 'securely' is to try to be sure over-clever compilers
 * won't optimise away memsets on variables that are about to be freed
 * or go out of scope. See
 * https://buildsecurityin.us-cert.gov/bsi-rules/home/g1/771-BSI.html
 */

#include "defs.h"
#include "misc.h"

/*
 * Trivial function that is given a pointer to some memory and ignores
 * it.
 */
static void no_op(void *ptr, size_t size) {}

/*
 * Function pointer that is given a pointer to some memory, and from
 * the compiler's point of view, _might_ read it, or otherwise depend
 * on its contents.
 *
 * In fact, this function pointer always points to no_op() above. But
 * because the pointer itself is volatile-qualified, the compiler
 * isn't allowed to optimise based on the assumption that that will
 * always be the case. So it has to call through the function pointer
 * anyway, on the basis that it _might_ have magically changed at run
 * time into a pointer to some completely arbitrary function. And
 * therefore it must also avoid optimising away any observable effect
 * beforehand that a completely arbitrary function might depend on -
 * such as the zeroing of our memory region.
 */
static void (*const volatile maybe_read)(void *ptr, size_t size) = no_op;

void smemclr(void *b, size_t n)
{
    if (b && n > 0) {
        /*
         * Zero out the memory.
         */
        memset(b, 0, n);

        /*
         * Call the above function pointer, which (for all the
         * compiler knows) might check that we've really zeroed the
         * memory.
         */
        maybe_read(b, n);
    }
}
