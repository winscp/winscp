/*
 * fromucs.c - convert Unicode to other character sets.
 */

#include "charset.h"
#include "internal.h"

struct charset_emit_param {
    char *output;
    int outlen;
    const char *errstr;
    int errlen;
    int stopped;
};

static void charset_emit(void *ctx, long int output)
{
    struct charset_emit_param *param = (struct charset_emit_param *)ctx;
    char outval;
    char const *p;
    int outlen;

    if (output == ERROR) {
	p = param->errstr;
	outlen = param->errlen;
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

int charset_from_unicode(wchar_t **input, int *inlen, char *output, int outlen,
			 int charset, charset_state *state,
			 const char *errstr, int errlen)
{
    charset_spec const *spec = charset_find_spec(charset);
    charset_state localstate;
    struct charset_emit_param param;

    param.output = output;
    param.outlen = outlen;
    param.stopped = 0;

    /*
     * charset_emit will expect a valid errstr.
     */
    if (!errstr) {
	/* *shrug* this is good enough, and consistent across all SBCS... */
	param.errstr = ".";
	param.errlen = 1;
    }
    param.errstr = errstr;
    param.errlen = errlen;

    if (!state) {
	localstate.s0 = 0;
    } else {
	localstate = *state;	       /* structure copy */
    }
    state = &localstate;

    while (*inlen > 0) {
	int lenbefore = param.output - output;
	spec->write(spec, **input, &localstate, charset_emit, &param);
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
	    *state = localstate;       /* structure copy */
	(*input)++;
	(*inlen)--;
    }
    return param.output - output;
}
