/*
 * Wildcard matching engine for use with SFTP-based file transfer
 * programs (PSFTP, new-look PSCP): since SFTP has no notion of
 * getting the remote side to do globbing (and rightly so) we have
 * to do it locally, by retrieving all the filenames in a directory
 * and checking each against the wildcard pattern.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "putty.h"

/*
 * Definition of wildcard syntax:
 * 
 *  - * matches any sequence of characters, including zero.
 *  - ? matches exactly one character which can be anything.
 *  - [abc] matches exactly one character which is a, b or c.
 *  - [a-f] matches anything from a through f.
 *  - [^a-f] matches anything _except_ a through f.
 *  - [-_] matches - or _; [^-_] matches anything else. (The - is
 *    non-special if it occurs immediately after the opening
 *    bracket or ^.)
 *  - [a^] matches an a or a ^. (The ^ is non-special if it does
 *    _not_ occur immediately after the opening bracket.)
 *  - \*, \?, \[, \], \\ match the single characters *, ?, [, ], \.
 *  - All other characters are non-special and match themselves.
 */

/*
 * Some notes on differences from POSIX globs (IEEE Std 1003.1, 2003 ed.):
 *  - backslashes act as escapes even within [] bracket expressions
 *  - does not support [!...] for non-matching list (POSIX are weird);
 *    NB POSIX allows [^...] as well via "A bracket expression starting
 *    with an unquoted circumflex character produces unspecified
 *    results". If we wanted to allow [!...] we might want to define
 *    [^!] as having its literal meaning (match '^' or '!').
 *  - none of the scary [[:class:]] stuff, etc
 */

/*
 * The wildcard matching technique we use is very simple and
 * potentially O(N^2) in running time, but I don't anticipate it
 * being that bad in reality (particularly since N will be the size
 * of a filename, which isn't all that much). Perhaps one day, once
 * PuTTY has grown a regexp matcher for some other reason, I might
 * come back and reimplement wildcards by translating them into
 * regexps or directly into NFAs; but for the moment, in the
 * absence of any other need for the NFA->DFA translation engine,
 * anything more than the simplest possible wildcard matcher is
 * vast code-size overkill.
 * 
 * Essentially, these wildcards are much simpler than regexps in
 * that they consist of a sequence of rigid fragments (? and [...]
 * can never match more or less than one character) separated by
 * asterisks. It is therefore extremely simple to look at a rigid
 * fragment and determine whether or not it begins at a particular
 * point in the test string; so we can search along the string
 * until we find each fragment, then search for the next. As long
 * as we find each fragment in the _first_ place it occurs, there
 * will never be a danger of having to backpedal and try to find it
 * again somewhere else.
 */

enum {
    WC_TRAILINGBACKSLASH = 1,
    WC_UNCLOSEDCLASS,
    WC_INVALIDRANGE
};

/*
 * Error reporting is done by returning various negative values
 * from the wildcard routines. Passing any such value to wc_error
 * will give a human-readable message.
 */
const char *wc_error(int value)
{
    value = abs(value);
    switch (value) {
      case WC_TRAILINGBACKSLASH:
	return "'\' occurred at end of string (expected another character)";
      case WC_UNCLOSEDCLASS:
	return "expected ']' to close character class";
      case WC_INVALIDRANGE:
	return "character range was not terminated (']' just after '-')";
    }
    return "INTERNAL ERROR: unrecognised wildcard error number";
}

/*
 * This is the routine that tests a target string to see if an
 * initial substring of it matches a fragment. If successful, it
 * returns 1, and advances both `fragment' and `target' past the
 * fragment and matching substring respectively. If unsuccessful it
 * returns zero. If the wildcard fragment suffers a syntax error,
 * it returns <0 and the precise value indexes into wc_error.
 */
static int wc_match_fragment(const char **fragment, const char **target,
                             const char *target_end)
{
    const char *f, *t;

    f = *fragment;
    t = *target;
    /*
     * The fragment terminates at either the end of the string, or
     * the first (unescaped) *.
     */
    while (*f && *f != '*' && t < target_end) {
	/*
	 * Extract one character from t, and one character's worth
	 * of pattern from f, and step along both. Return 0 if they
	 * fail to match.
	 */
	if (*f == '\\') {
	    /*
	     * Backslash, which means f[1] is to be treated as a
	     * literal character no matter what it is. It may not
	     * be the end of the string.
	     */
	    if (!f[1])
		return -WC_TRAILINGBACKSLASH;   /* error */
	    if (f[1] != *t)
		return 0;	       /* failed to match */
	    f += 2;
	} else if (*f == '?') {
	    /*
	     * Question mark matches anything.
	     */
	    f++;
	} else if (*f == '[') {
	    bool invert = false;
	    bool matched = false;
	    /*
	     * Open bracket introduces a character class.
	     */
	    f++;
	    if (*f == '^') {
		invert = true;
		f++;
	    }
	    while (*f != ']') {
		if (*f == '\\')
		    f++;	       /* backslashes still work */
		if (!*f)
		    return -WC_UNCLOSEDCLASS;   /* error again */
		if (f[1] == '-') {
		    int lower, upper, ourchr;
		    lower = (unsigned char) *f++;
		    f++;	       /* eat the minus */
		    if (*f == ']')
			return -WC_INVALIDRANGE;   /* different error! */
		    if (*f == '\\')
			f++;	       /* backslashes _still_ work */
		    if (!*f)
			return -WC_UNCLOSEDCLASS;   /* error again */
		    upper = (unsigned char) *f++;
		    ourchr = (unsigned char) *t;
		    if (lower > upper) {
			int t = lower; lower = upper; upper = t;
		    }
		    if (ourchr >= lower && ourchr <= upper)
			matched = true;
		} else {
		    matched |= (*t == *f++);
		}
	    }
	    if (invert == matched)
		return 0;	       /* failed to match character class */
	    f++;		       /* eat the ] */
	} else {
	    /*
	     * Non-special character matches itself.
	     */
	    if (*f != *t)
		return 0;
	    f++;
	}
	/*
	 * Now we've done that, increment t past the character we
	 * matched.
	 */
	t++;
    }
    if (!*f || *f == '*') {
	/*
	 * We have reached the end of f without finding a mismatch;
	 * so we're done. Update the caller pointers and return 1.
	 */
	*fragment = f;
	*target = t;
	return 1;
    }
    /*
     * Otherwise, we must have reached the end of t before we
     * reached the end of f; so we've failed. Return 0. 
     */
    return 0;
}

/*
 * This is the real wildcard matching routine. It returns 1 for a
 * successful match, 0 for an unsuccessful match, and <0 for a
 * syntax error in the wildcard.
 */
static int wc_match_inner(
    const char *wildcard, const char *target, size_t target_len)
{
    const char *target_end = target + target_len;
    int ret;

    /*
     * Every time we see a '*' _followed_ by a fragment, we just
     * search along the string for a location at which the fragment
     * matches. The only special case is when we see a fragment
     * right at the start, in which case we just call the matching
     * routine once and give up if it fails.
     */
    if (*wildcard != '*') {
	ret = wc_match_fragment(&wildcard, &target, target_end);
	if (ret <= 0)
	    return ret;		       /* pass back failure or error alike */
    }

    while (*wildcard) {
	assert(*wildcard == '*');
	while (*wildcard == '*')
	    wildcard++;

	/*
	 * It's possible we've just hit the end of the wildcard
	 * after seeing a *, in which case there's no need to
	 * bother searching any more because we've won.
	 */
	if (!*wildcard)
	    return 1;

	/*
	 * Now `wildcard' points at the next fragment. So we
	 * attempt to match it against `target', and if that fails
	 * we increment `target' and try again, and so on. When we
	 * find we're about to try matching against the empty
	 * string, we give up and return 0.
	 */
	ret = 0;
	while (*target) {
	    const char *save_w = wildcard, *save_t = target;

	    ret = wc_match_fragment(&wildcard, &target, target_end);

	    if (ret < 0)
		return ret;	       /* syntax error */

	    if (ret > 0 && !*wildcard && target != target_end) {
		/*
		 * Final special case - literally.
		 * 
		 * This situation arises when we are matching a
		 * _terminal_ fragment of the wildcard (that is,
		 * there is nothing after it, e.g. "*a"), and it
		 * has matched _too early_. For example, matching
		 * "*a" against "parka" will match the "a" fragment
		 * against the _first_ a, and then (if it weren't
		 * for this special case) matching would fail
		 * because we're at the end of the wildcard but not
		 * at the end of the target string.
		 * 
		 * In this case what we must do is measure the
		 * length of the fragment in the target (which is
		 * why we saved `target'), jump straight to that
		 * distance from the end of the string using
		 * strlen, and match the same fragment again there
		 * (which is why we saved `wildcard'). Then we
		 * return whatever that operation returns.
		 */
		target = target_end - (target - save_t);
		wildcard = save_w;
		return wc_match_fragment(&wildcard, &target, target_end);
	    }

	    if (ret > 0)
		break;
	    target++;
	}
	if (ret > 0)
	    continue;
	return 0;
    }

    /*
     * If we reach here, it must be because we successfully matched
     * a fragment and then found ourselves right at the end of the
     * wildcard. Hence, we return 1 if and only if we are also
     * right at the end of the target.
     */
    return target == target_end;
}

int wc_match(const char *wildcard, const char *target)
{
    return wc_match_inner(wildcard, target, strlen(target));
}

int wc_match_pl(const char *wildcard, ptrlen target)
{
    return wc_match_inner(wildcard, target.ptr, target.len);
}

/*
 * Another utility routine that translates a non-wildcard string
 * into its raw equivalent by removing any escaping backslashes.
 * Expects a target string buffer of anything up to the length of
 * the original wildcard. You can also pass NULL as the output
 * buffer if you're only interested in the return value.
 * 
 * Returns true on success, or false if a wildcard character was
 * encountered. In the latter case the output string MAY not be
 * zero-terminated and you should not use it for anything!
 */
bool wc_unescape(char *output, const char *wildcard)
{
    while (*wildcard) {
	if (*wildcard == '\\') {
	    wildcard++;
	    /* We are lenient about trailing backslashes in non-wildcards. */
	    if (*wildcard) {
		if (output)
		    *output++ = *wildcard;
		wildcard++;
	    }
	} else if (*wildcard == '*' || *wildcard == '?' ||
		   *wildcard == '[' || *wildcard == ']') {
	    return false;              /* it's a wildcard! */
	} else {
	    if (output)
		*output++ = *wildcard;
	    wildcard++;
	}
    }
    if (output)
        *output = '\0';
    return true;                       /* it's clean */
}

#ifdef TESTMODE

struct test {
    const char *wildcard;
    const char *target;
    int expected_result;
};

const struct test fragment_tests[] = {
    /*
     * We exhaustively unit-test the fragment matching routine
     * itself, which should save us the need to test all its
     * intricacies during the full wildcard tests.
     */
    {"abc", "abc", 1},
    {"abc", "abd", 0},
    {"abc", "abcd", 1},
    {"abcd", "abc", 0},
    {"ab[cd]", "abc", 1},
    {"ab[cd]", "abd", 1},
    {"ab[cd]", "abe", 0},
    {"ab[^cd]", "abc", 0},
    {"ab[^cd]", "abd", 0},
    {"ab[^cd]", "abe", 1},
    {"ab\\", "abc", -WC_TRAILINGBACKSLASH},
    {"ab\\*", "ab*", 1},
    {"ab\\?", "ab*", 0},
    {"ab?", "abc", 1},
    {"ab?", "ab", 0},
    {"ab[", "abc", -WC_UNCLOSEDCLASS},
    {"ab[c-", "abb", -WC_UNCLOSEDCLASS},
    {"ab[c-]", "abb", -WC_INVALIDRANGE},
    {"ab[c-e]", "abb", 0},
    {"ab[c-e]", "abc", 1},
    {"ab[c-e]", "abd", 1},
    {"ab[c-e]", "abe", 1},
    {"ab[c-e]", "abf", 0},
    {"ab[e-c]", "abb", 0},
    {"ab[e-c]", "abc", 1},
    {"ab[e-c]", "abd", 1},
    {"ab[e-c]", "abe", 1},
    {"ab[e-c]", "abf", 0},
    {"ab[^c-e]", "abb", 1},
    {"ab[^c-e]", "abc", 0},
    {"ab[^c-e]", "abd", 0},
    {"ab[^c-e]", "abe", 0},
    {"ab[^c-e]", "abf", 1},
    {"ab[^e-c]", "abb", 1},
    {"ab[^e-c]", "abc", 0},
    {"ab[^e-c]", "abd", 0},
    {"ab[^e-c]", "abe", 0},
    {"ab[^e-c]", "abf", 1},
    {"ab[a^]", "aba", 1},
    {"ab[a^]", "ab^", 1},
    {"ab[a^]", "abb", 0},
    {"ab[^a^]", "aba", 0},
    {"ab[^a^]", "ab^", 0},
    {"ab[^a^]", "abb", 1},
    {"ab[-c]", "ab-", 1},
    {"ab[-c]", "abc", 1},
    {"ab[-c]", "abd", 0},
    {"ab[^-c]", "ab-", 0},
    {"ab[^-c]", "abc", 0},
    {"ab[^-c]", "abd", 1},
    {"ab[\\[-\\]]", "abZ", 0},
    {"ab[\\[-\\]]", "ab[", 1},
    {"ab[\\[-\\]]", "ab\\", 1},
    {"ab[\\[-\\]]", "ab]", 1},
    {"ab[\\[-\\]]", "ab^", 0},
    {"ab[^\\[-\\]]", "abZ", 1},
    {"ab[^\\[-\\]]", "ab[", 0},
    {"ab[^\\[-\\]]", "ab\\", 0},
    {"ab[^\\[-\\]]", "ab]", 0},
    {"ab[^\\[-\\]]", "ab^", 1},
    {"ab[a-fA-F]", "aba", 1},
    {"ab[a-fA-F]", "abF", 1},
    {"ab[a-fA-F]", "abZ", 0},
};

const struct test full_tests[] = {
    {"a", "argh", 0},
    {"a", "ba", 0},
    {"a", "a", 1},
    {"a*", "aardvark", 1},
    {"a*", "badger", 0},
    {"*a", "park", 0},
    {"*a", "pArka", 1},
    {"*a", "parka", 1},
    {"*a*", "park", 1},
    {"*a*", "perk", 0},
    {"?b*r?", "abracadabra", 1},
    {"?b*r?", "abracadabr", 0},
    {"?b*r?", "abracadabzr", 0},
};

int main(void)
{
    int i;
    int fails, passes;

    fails = passes = 0;

    for (i = 0; i < sizeof(fragment_tests)/sizeof(*fragment_tests); i++) {
	const char *f, *t;
	int eret, aret;
	f = fragment_tests[i].wildcard;
	t = fragment_tests[i].target;
	eret = fragment_tests[i].expected_result;
	aret = wc_match_fragment(&f, &t, t + strlen(t));
	if (aret != eret) {
	    printf("failed test: /%s/ against /%s/ returned %d not %d\n",
		   fragment_tests[i].wildcard, fragment_tests[i].target,
		   aret, eret);
	    fails++;
	} else
	    passes++;
    }

    for (i = 0; i < sizeof(full_tests)/sizeof(*full_tests); i++) {
	const char *f, *t;
	int eret, aret;
	f = full_tests[i].wildcard;
	t = full_tests[i].target;
	eret = full_tests[i].expected_result;
	aret = wc_match(f, t);
	if (aret != eret) {
	    printf("failed test: /%s/ against /%s/ returned %d not %d\n",
		   full_tests[i].wildcard, full_tests[i].target,
		   aret, eret);
	    fails++;
	} else
	    passes++;
    }

    printf("passed %d, failed %d\n", passes, fails);

    return 0;
}

#endif
