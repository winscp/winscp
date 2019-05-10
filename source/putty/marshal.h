#ifndef PUTTY_MARSHAL_H
#define PUTTY_MARSHAL_H

#include "defs.h"

#include <stdio.h>

/*
 * A sort of 'abstract base class' or 'interface' or 'trait' which is
 * the common feature of all types that want to accept data formatted
 * using the SSH binary conventions of uint32, string, mpint etc.
 */
struct BinarySink {
    void (*write)(BinarySink *sink, const void *data, size_t len);
    BinarySink *binarysink_;
};

/*
 * To define a structure type as a valid target for binary formatted
 * data, put 'BinarySink_IMPLEMENTATION' in its declaration, and when
 * an instance is set up, use 'BinarySink_INIT' to initialise the
 * 'base class' state, providing a function pointer to be the
 * implementation of the write() call above.
 */
#define BinarySink_IMPLEMENTATION BinarySink binarysink_[1]
#define BinarySink_INIT(obj, writefn) \
    ((obj)->binarysink_->write = (writefn), \
     (obj)->binarysink_->binarysink_ = (obj)->binarysink_)

/*
 * To define a larger structure type as a valid BinarySink in such a
 * way that it will delegate the write method to some other object,
 * put 'BinarySink_DELEGATE_IMPLEMENTATION' in its declaration, and
 * when an instance is set up, use 'BinarySink_DELEGATE_INIT' to point
 * at the object it wants to delegate to.
 *
 * In such a delegated structure, you might sometimes want to have the
 * delegation stop being valid (e.g. it might be delegating to an
 * object that only sometimes exists). You can null out the delegate
 * pointer using BinarySink_DELEGATE_CLEAR.
 */
#define BinarySink_DELEGATE_IMPLEMENTATION BinarySink *binarysink_
#define BinarySink_DELEGATE_INIT(obj, othersink) \
    ((obj)->binarysink_ = BinarySink_UPCAST(othersink))
#define BinarySink_DELEGATE_CLEAR(obj) ((obj)->binarysink_ = NULL)

/*
 * The implementing type's write function will want to downcast its
 * 'BinarySink *' parameter back to the more specific type. Also,
 * sometimes you'll want to upcast a pointer to a particular
 * implementing type into an abstract 'BinarySink *' to pass to
 * generic subroutines not defined in this file. These macros do that
 * job.
 *
 * Importantly, BinarySink_UPCAST can also be applied to a BinarySink
 * * itself (and leaves it unchanged). That's achieved by a small
 * piece of C trickery: implementing structures and the BinarySink
 * structure itself both contain a field called binarysink_, but in
 * implementing objects it's a BinarySink[1] whereas in the abstract
 * type it's a 'BinarySink *' pointing back to the same structure,
 * meaning that you can say 'foo->binarysink_' in either case and get
 * a pointer type by different methods.
 */
#define BinarySink_DOWNCAST(object, type)                               \
    TYPECHECK((object) == ((type *)0)->binarysink_,                     \
              ((type *)(((char *)(object)) - offsetof(type, binarysink_))))
#define BinarySink_UPCAST(object)                                       \
    TYPECHECK((object)->binarysink_ == (BinarySink *)0,                 \
              (object)->binarysink_)

/*
 * If you structure-copy an object that's implementing BinarySink,
 * then that tricky self-pointer in its trait subobject will point to
 * the wrong place. You could call BinarySink_INIT again, but this
 * macro is terser and does all that's needed to fix up the copied
 * object.
 */
#define BinarySink_COPIED(obj) \
    ((obj)->binarysink_->binarysink_ = (obj)->binarysink_)

/*
 * The put_* macros are the main client to this system. Any structure
 * which implements the BinarySink 'trait' is valid for use as the
 * first parameter of any of these put_* macros.
 */

/* Basic big-endian integer types. */
#define put_byte(bs, val) \
    BinarySink_put_byte(BinarySink_UPCAST(bs), val)
#define put_uint16(bs, val) \
    BinarySink_put_uint16(BinarySink_UPCAST(bs), val)
#define put_uint32(bs, val) \
    BinarySink_put_uint32(BinarySink_UPCAST(bs), val)
#define put_uint64(bs, val) \
    BinarySink_put_uint64(BinarySink_UPCAST(bs), val)

/* SSH booleans, encoded as a single byte storing either 0 or 1. */
#define put_bool(bs, val) \
    BinarySink_put_bool(BinarySink_UPCAST(bs), val)

/* SSH strings, with a leading uint32 length field. 'stringz' is a
 * convenience function that takes an ordinary C zero-terminated
 * string as input. 'stringsb' takes a strbuf * as input, and
 * finalises it as a side effect (handy for multi-level marshalling in
 * which you use these same functions to format an inner blob of data
 * that then gets wrapped into a string container in an outer one). */
#define put_string(bs, val, len) \
    BinarySink_put_string(BinarySink_UPCAST(bs),val,len)
#define put_stringpl(bs, ptrlen) \
    BinarySink_put_stringpl(BinarySink_UPCAST(bs),ptrlen)
#define put_stringz(bs, val) \
    BinarySink_put_stringz(BinarySink_UPCAST(bs), val)
#define put_stringsb(bs, val) \
    BinarySink_put_stringsb(BinarySink_UPCAST(bs), val)

/* Other string outputs: 'asciz' emits the string data directly into
 * the output including the terminating \0, and 'pstring' emits the
 * string in Pascal style with a leading _one_-byte length field.
 * pstring can fail if the string is too long. */
#define put_asciz(bs, val) \
    BinarySink_put_asciz(BinarySink_UPCAST(bs), val)
#define put_pstring(bs, val) \
    BinarySink_put_pstring(BinarySink_UPCAST(bs), val)

/* Multiprecision integers, in both the SSH-1 and SSH-2 formats. */
#define put_mp_ssh1(bs, val) \
    BinarySink_put_mp_ssh1(BinarySink_UPCAST(bs), val)
#define put_mp_ssh2(bs, val) \
    BinarySink_put_mp_ssh2(BinarySink_UPCAST(bs), val)

/* Padding with a specified byte. */
#define put_padding(bs, len, padbyte) \
    BinarySink_put_padding(BinarySink_UPCAST(bs), len, padbyte)

/* Fallback: just emit raw data bytes, using a syntax that matches the
 * rest of these macros. */
#define put_data(bs, val, len) \
    BinarySink_put_data(BinarySink_UPCAST(bs), val, len)
#define put_datapl(bs, pl) \
    BinarySink_put_datapl(BinarySink_UPCAST(bs), pl)

/*
 * The underlying real C functions that implement most of those
 * macros. Generally you won't want to call these directly, because
 * they have such cumbersome names; you call the wrapper macros above
 * instead.
 *
 * A few functions whose wrapper macros are defined above are actually
 * declared in other headers, so as to guarantee that the
 * declaration(s) of their other parameter type(s) are in scope.
 */
void BinarySink_put_data(BinarySink *, const void *data, size_t len);
void BinarySink_put_datapl(BinarySink *, ptrlen);
void BinarySink_put_padding(BinarySink *, size_t len, unsigned char padbyte);
void BinarySink_put_byte(BinarySink *, unsigned char);
void BinarySink_put_bool(BinarySink *, bool);
void BinarySink_put_uint16(BinarySink *, unsigned long);
void BinarySink_put_uint32(BinarySink *, unsigned long);
void BinarySink_put_uint64(BinarySink *, uint64_t);
void BinarySink_put_string(BinarySink *, const void *data, size_t len);
void BinarySink_put_stringpl(BinarySink *, ptrlen);
void BinarySink_put_stringz(BinarySink *, const char *str);
struct strbuf;
void BinarySink_put_stringsb(BinarySink *, struct strbuf *);
void BinarySink_put_asciz(BinarySink *, const char *str);
bool BinarySink_put_pstring(BinarySink *, const char *str);
void BinarySink_put_mp_ssh1(BinarySink *bs, mp_int *x);
void BinarySink_put_mp_ssh2(BinarySink *bs, mp_int *x);

/* ---------------------------------------------------------------------- */

/*
 * A complementary trait structure for _un_-marshalling.
 *
 * This structure contains client-visible data fields rather than
 * methods, because that seemed more useful than leaving it totally
 * opaque. But it's still got the self-pointer system that will allow
 * the set of get_* macros to target one of these itself or any other
 * type that 'derives' from it. So, for example, an SSH packet
 * structure can act as a BinarySource while also having additional
 * fields like the packet type.
 */
typedef enum BinarySourceError {
    BSE_NO_ERROR,
    BSE_OUT_OF_DATA,
    BSE_INVALID
} BinarySourceError;
struct BinarySource {
    /*
     * (data, len) is the data block being decoded. pos is the current
     * position within the block.
     */
    const void *data;
    size_t pos, len;

    /*
     * 'err' indicates whether a decoding error has happened at any
     * point. Once this has been set to something other than
     * BSE_NO_ERROR, it shouldn't be changed by any unmarshalling
     * function. So you can safely do a long sequence of get_foo()
     * operations and then test err just once at the end, rather than
     * having to conditionalise every single get.
     *
     * The unmarshalling functions should always return some value,
     * even if a decoding error occurs. Generally on error they'll
     * return zero (if numeric) or the empty string (if string-based),
     * or some other appropriate default value for more complicated
     * types.
     *
     * If the usual return value is dynamically allocated (e.g. a
     * bignum, or a normal C 'char *' string), then the error value is
     * also dynamic in the same way. So you have to free exactly the
     * same set of things whether or not there was a decoding error,
     * which simplifies exit paths - for example, you could call a big
     * pile of get_foo functions, then put the actual handling of the
     * results under 'if (!get_err(src))', and then free everything
     * outside that if.
     */
    BinarySourceError err;

    /*
     * Self-pointer for the implicit derivation trick, same as
     * BinarySink above.
     */
    BinarySource *binarysource_;
};

/*
 * Implementation macros, similar to BinarySink.
 */
#define BinarySource_IMPLEMENTATION BinarySource binarysource_[1]
static inline void BinarySource_INIT__(BinarySource *src, ptrlen data)
{
    src->data = data.ptr;
    src->len = data.len;
    src->pos = 0;
    src->err = BSE_NO_ERROR;
    src->binarysource_ = src;
}
#define BinarySource_BARE_INIT_PL(obj, pl)                      \
    TYPECHECK(&(obj)->binarysource_ == (BinarySource **)0,      \
              BinarySource_INIT__(obj, pl))
#define BinarySource_BARE_INIT(obj, data_, len_)                \
    BinarySource_BARE_INIT_PL(obj, make_ptrlen(data_, len_))
#define BinarySource_INIT_PL(obj, pl)                                   \
    TYPECHECK(&(obj)->binarysource_ == (BinarySource (*)[1])0,          \
              BinarySource_INIT__(BinarySource_UPCAST(obj), pl))
#define BinarySource_INIT(obj, data_, len_)             \
    BinarySource_INIT_PL(obj, make_ptrlen(data_, len_))
#define BinarySource_DOWNCAST(object, type)                             \
    TYPECHECK((object) == ((type *)0)->binarysource_,                     \
              ((type *)(((char *)(object)) - offsetof(type, binarysource_))))
#define BinarySource_UPCAST(object)                                       \
    TYPECHECK((object)->binarysource_ == (BinarySource *)0,                 \
              (object)->binarysource_)
#define BinarySource_COPIED(obj) \
    ((obj)->binarysource_->binarysource_ = (obj)->binarysource_)

#define get_data(src, len) \
    BinarySource_get_data(BinarySource_UPCAST(src), len)
#define get_byte(src) \
    BinarySource_get_byte(BinarySource_UPCAST(src))
#define get_bool(src) \
    BinarySource_get_bool(BinarySource_UPCAST(src))
#define get_uint16(src) \
    BinarySource_get_uint16(BinarySource_UPCAST(src))
#define get_uint32(src) \
    BinarySource_get_uint32(BinarySource_UPCAST(src))
#define get_uint64(src) \
    BinarySource_get_uint64(BinarySource_UPCAST(src))
#define get_string(src) \
    BinarySource_get_string(BinarySource_UPCAST(src))
#define get_asciz(src) \
    BinarySource_get_asciz(BinarySource_UPCAST(src))
#define get_pstring(src) \
    BinarySource_get_pstring(BinarySource_UPCAST(src))
#define get_mp_ssh1(src) \
    BinarySource_get_mp_ssh1(BinarySource_UPCAST(src))
#define get_mp_ssh2(src) \
    BinarySource_get_mp_ssh2(BinarySource_UPCAST(src))
#define get_rsa_ssh1_pub(src, rsa, order) \
    BinarySource_get_rsa_ssh1_pub(BinarySource_UPCAST(src), rsa, order)
#define get_rsa_ssh1_priv(src, rsa) \
    BinarySource_get_rsa_ssh1_priv(BinarySource_UPCAST(src), rsa)

#define get_err(src) (BinarySource_UPCAST(src)->err)
#define get_avail(src) (BinarySource_UPCAST(src)->len - \
                       BinarySource_UPCAST(src)->pos)
#define get_ptr(src)                                                    \
    ((const void *)(                                                    \
        (const unsigned char *)(BinarySource_UPCAST(src)->data) +       \
        BinarySource_UPCAST(src)->pos))

ptrlen BinarySource_get_data(BinarySource *, size_t);
unsigned char BinarySource_get_byte(BinarySource *);
bool BinarySource_get_bool(BinarySource *);
unsigned BinarySource_get_uint16(BinarySource *);
unsigned long BinarySource_get_uint32(BinarySource *);
uint64_t BinarySource_get_uint64(BinarySource *);
ptrlen BinarySource_get_string(BinarySource *);
const char *BinarySource_get_asciz(BinarySource *);
ptrlen BinarySource_get_pstring(BinarySource *);
mp_int *BinarySource_get_mp_ssh1(BinarySource *src);
mp_int *BinarySource_get_mp_ssh2(BinarySource *src);

/*
 * A couple of useful standard BinarySink implementations, which live
 * as sensibly here as anywhere else: one that makes a BinarySink
 * whose effect is to write to a stdio stream, and one whose effect is
 * to append to a bufchain.
 */
struct stdio_sink {
    FILE *fp;
    BinarySink_IMPLEMENTATION;
};
struct bufchain_sink {
    bufchain *ch;
    BinarySink_IMPLEMENTATION;
};
void stdio_sink_init(stdio_sink *sink, FILE *fp);
void bufchain_sink_init(bufchain_sink *sink, bufchain *ch);

#endif /* PUTTY_MARSHAL_H */
