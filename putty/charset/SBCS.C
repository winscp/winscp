/*
 * sbcs.c - routines to handle single-byte character sets.
 */

#include "charset.h"
#include "internal.h"

/*
 * The charset_spec for any single-byte character set should
 * provide read_sbcs() as its read function, and its `data' field
 * should be a wchar_t string constant containing the 256 entries
 * of the translation table.
 */

void read_sbcs(charset_spec const *charset, long int input_chr,
	       charset_state *state,
	       void (*emit)(void *ctx, long int output), void *emitctx)
{
    const struct sbcs_data *sd = charset->data;

    UNUSEDARG(state);

    emit(emitctx, sd->sbcs2ucs[input_chr]);
}

void write_sbcs(charset_spec const *charset, long int input_chr,
		charset_state *state,
		void (*emit)(void *ctx, long int output), void *emitctx)
{
    const struct sbcs_data *sd = charset->data;
    int i, j, k, c;

    UNUSEDARG(state);

    /*
     * Binary-search in the ucs2sbcs table.
     */
    i = -1;
    j = sd->nvalid;
    while (i+1 < j) {
	k = (i+j)/2;
	c = sd->ucs2sbcs[k];
	if (input_chr < sd->sbcs2ucs[c])
	    j = k;
	else if (input_chr > sd->sbcs2ucs[c])
	    i = k;
	else {
	    emit(emitctx, c);
	    return;
	}
    }
    emit(emitctx, ERROR);
}
