/*
 * Coroutine mechanics used in PuTTY's SSH code.
 */

#ifndef PUTTY_SSHCR_H
#define PUTTY_SSHCR_H

/*
 * If these macros look impenetrable to you, you might find it helpful
 * to read
 *
 *   https://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
 *
 * which explains the theory behind these macros.
 *
 * In particular, if you are getting `case expression not constant'
 * errors when building with MS Visual Studio, this is because MS's
 * Edit and Continue debugging feature causes their compiler to
 * violate ANSI C. To disable Edit and Continue debugging:
 *
 *  - click Settings
 *  - select the C/C++ tab and the General category
 *  - under `Debug info:', select anything _other_ than `Program
 *    Database for Edit and Continue'.
 */

#define crBegin(v)      do { int *crLine = &v; switch(v) { case 0:
#define crBeginState    crBegin(s->crLine)
#define crStateP(t, v)                          \
    struct t *s;                                \
    if (!(v)) { s = (v) = snew(struct t); s->crLine = 0; }      \
    s = (v);
#define crState(t)      crStateP(t, ssh->t)
#define crFinish(z)     } *crLine = 0; return (z); } while (0)
#define crFinishV       } *crLine = 0; return; } while (0)
#define crFinishFreed(z) } return (z); } while (0)
#define crFinishFreedV   } return; } while (0)
#define crFinishFree(z) } sfree(s); return (z); } while (0)
#define crFinishFreeV   } sfree(s); return; } while (0)
#define crReturn(z)     \
        do {\
            *crLine = __LINE__; return (z); case __LINE__:;\
        } while (0)
#define crReturnV       \
        do {\
            *crLine=__LINE__; return; case __LINE__:;\
        } while (0)
#define crStop(z)       do{ *crLine = 0; return (z); }while(0)
#define crStopV         do{ *crLine = 0; return; }while(0)

/*
 * The crMaybeWaitUntil macros could have been more easily written in
 * terms of the simple crReturn above, by writing things like
 *
 *       while (!condition) { crReturn(whatever); }
 *
 * (or do-while in the case of crWaitUntil). But it's better to do it
 * directly by writing _once_ to crLine before first testing the
 * condition, because this way it's robust against the condition check
 * potentially freeing the entire coroutine state structure as a side
 * effect (as long as it also evaluates false if it does that),
 * because we don't write into crLine between the condition evaluating
 * to false and the 'return' statement.
 */
#define crMaybeWaitUntil(c)                     \
    do {                                        \
        *crLine = __LINE__;                     \
        case __LINE__: if (!(c)) return 0;      \
    } while (0)
#define crMaybeWaitUntilV(c)                    \
    do {                                        \
        *crLine = __LINE__;                     \
        case __LINE__: if (!(c)) return;        \
    } while (0)
#define crWaitUntil(c)                          \
    do {                                        \
        *crLine = __LINE__; return;             \
        case __LINE__: if (!(c)) return 0;      \
    } while (0)
#define crWaitUntilV(c)                         \
    do {                                        \
        *crLine = __LINE__; return;             \
        case __LINE__: if (!(c)) return;        \
    } while (0)

#endif /* PUTTY_SSHCR_H */
