/*
 * toucs.c - convert charsets to Unicode.
 */

#include "charset.h"
#include "internal.h"

struct unicode_emit_param {
    wchar_t *output;
    int outlen;
    const wchar_t *errstr;
    int errlen;
    int stopped;
};

static void unicode_emit(void *ctx, long int output)
{
    struct unicode_emit_param *param = (struct unicode_emit_param *)ctx;
    wchar_t outval;
    wchar_t const *p;
    int outlen;

    if (output == ERROR) {
	if (param->errstr) {
	    p = param->errstr;
	    outlen = param->errlen;
	} else {
	    outval = 0xFFFD;	       /* U+FFFD REPLACEMENT CHARACTER */
	    p = &outval;
	    outlen = 1;
	}
    } else {
	outval = output;
	p = &outval;
	outlen = 1;
    }

    if (param->outlen >= outlen) {
	while (outlen > 0) {
	    *param->output++ = *p++;
	    param->outlen--;
	    outlen--;
	}
    } else {
	param->stopped = 1;
    }
}

int charset_to_unicode(char **input, int *inlen, wchar_t *output, int outlen,
		       int charset, charset_state *state,
		       const wchar_t *errstr, int errlen)
{
    charset_spec const *spec = charset_find_spec(charset);
    charset_state localstate;
    struct unicode_emit_param param;

    param.output = output;
    param.outlen = outlen;
    param.errstr = errstr;
    param.errlen = errlen;
    param.stopped = 0;

    if (!state) {
	localstate.s0 = 0;
    } else {
	localstate = *state;	       /* structure copy */
    }

    while (*inlen > 0) {
	int lenbefore = param.output - output;
	spec->read(spec, (unsigned char)**input, &localstate,
		   unicode_emit, &param);
	if (param.stopped) {
	    /*
	     * The emit function has _tried_ to output some
	     * characters, but ran up against the end of the
	     * buffer. Leave immediately, and return what happened
	     * _before_ attempting to process this character.
	     */
	    return lenbefore;
	}
	if (state)
	    *state = localstate;   /* structure copy */
	(*input)++;
	(*inlen)--;
    }

    return param.output - output;
}
