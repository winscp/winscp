/*
 * winutils.c: miscellaneous Windows utilities for GUI apps
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "misc.h"

#ifdef TESTMODE
/* Definitions to allow this module to be compiled standalone for testing. */
#define smalloc malloc
#define srealloc realloc
#define sfree free
#endif

/*
 * Split a complete command line into argc/argv, attempting to do
 * it exactly the same way Windows itself would do it (so that
 * console utilities, which receive argc and argv from Windows,
 * will have their command lines processed in the same way as GUI
 * utilities which get a whole command line and must break it
 * themselves).
 * 
 * Does not modify the input command line.
 * 
 * The final parameter (argstart) is used to return a second array
 * of char * pointers, the same length as argv, each one pointing
 * at the start of the corresponding element of argv in the
 * original command line. So if you get half way through processing
 * your command line in argc/argv form and then decide you want to
 * treat the rest as a raw string, you can. If you don't want to,
 * `argstart' can be safely left NULL.
 */
void split_into_argv(char *cmdline, int *argc, char ***argv,
		     char ***argstart)
{
    char *p;
    char *outputline, *q;
    char **outputargv, **outputargstart;
    int outputargc;

    /*
     * At first glance the rules appeared to be:
     *
     *  - Single quotes are not special characters.
     *
     *  - Double quotes are removed, but within them spaces cease
     *    to be special.
     *
     *  - Backslashes are _only_ special when a sequence of them
     *    appear just before a double quote. In this situation,
     *    they are treated like C backslashes: so \" just gives a
     *    literal quote, \\" gives a literal backslash and then
     *    opens or closes a double-quoted segment, \\\" gives a
     *    literal backslash and then a literal quote, \\\\" gives
     *    two literal backslashes and then opens/closes a
     *    double-quoted segment, and so forth. Note that this
     *    behaviour is identical inside and outside double quotes.
     *
     *  - Two successive double quotes become one literal double
     *    quote, but only _inside_ a double-quoted segment.
     *    Outside, they just form an empty double-quoted segment
     *    (which may cause an empty argument word).
     *
     *  - That only leaves the interesting question of what happens
     *    when one or more backslashes precedes two or more double
     *    quotes, starting inside a double-quoted string. And the
     *    answer to that appears somewhat bizarre. Here I tabulate
     *    number of backslashes (across the top) against number of
     *    quotes (down the left), and indicate how many backslashes
     *    are output, how many quotes are output, and whether a
     *    quoted segment is open at the end of the sequence:
     * 
     *                      backslashes
     * 
     *               0         1      2      3      4
     * 
     *         0   0,0,y  |  1,0,y  2,0,y  3,0,y  4,0,y
     *            --------+-----------------------------
     *         1   0,0,n  |  0,1,y  1,0,n  1,1,y  2,0,n
     *    q    2   0,1,n  |  0,1,n  1,1,n  1,1,n  2,1,n
     *    u    3   0,1,y  |  0,2,n  1,1,y  1,2,n  2,1,y
     *    o    4   0,1,n  |  0,2,y  1,1,n  1,2,y  2,1,n
     *    t    5   0,2,n  |  0,2,n  1,2,n  1,2,n  2,2,n
     *    e    6   0,2,y  |  0,3,n  1,2,y  1,3,n  2,2,y
     *    s    7   0,2,n  |  0,3,y  1,2,n  1,3,y  2,2,n
     *         8   0,3,n  |  0,3,n  1,3,n  1,3,n  2,3,n
     *         9   0,3,y  |  0,4,n  1,3,y  1,4,n  2,3,y
     *        10   0,3,n  |  0,4,y  1,3,n  1,4,y  2,3,n
     *        11   0,4,n  |  0,4,n  1,4,n  1,4,n  2,4,n
     * 
     * 
     *      [Test fragment was of the form "a\\\"""b c" d.]
     * 
     * There is very weird mod-3 behaviour going on here in the
     * number of quotes, and it even applies when there aren't any
     * backslashes! How ghastly.
     * 
     * With a bit of thought, this extremely odd diagram suddenly
     * coalesced itself into a coherent, if still ghastly, model of
     * how things work:
     * 
     *  - As before, backslashes are only special when one or more
     *    of them appear contiguously before at least one double
     *    quote. In this situation the backslashes do exactly what
     *    you'd expect: each one quotes the next thing in front of
     *    it, so you end up with n/2 literal backslashes (if n is
     *    even) or (n-1)/2 literal backslashes and a literal quote
     *    (if n is odd). In the latter case the double quote
     *    character right after the backslashes is used up.
     * 
     *  - After that, any remaining double quotes are processed. A
     *    string of contiguous unescaped double quotes has a mod-3
     *    behaviour:
     * 
     *     * inside a quoted segment, a quote ends the segment.
     *     * _immediately_ after ending a quoted segment, a quote
     *       simply produces a literal quote.
     *     * otherwise, outside a quoted segment, a quote begins a
     *       quoted segment.
     * 
     *    So, for example, if we started inside a quoted segment
     *    then two contiguous quotes would close the segment and
     *    produce a literal quote; three would close the segment,
     *    produce a literal quote, and open a new segment. If we
     *    started outside a quoted segment, then two contiguous
     *    quotes would open and then close a segment, producing no
     *    output (but potentially creating a zero-length argument);
     *    but three quotes would open and close a segment and then
     *    produce a literal quote.
     */

    /*
     * First deal with the simplest of all special cases: if there
     * aren't any arguments, return 0,NULL,NULL.
     */
    while (*cmdline && isspace(*cmdline)) cmdline++;
    if (!*cmdline) {
	if (argc) *argc = 0;
	if (argv) *argv = NULL;
	if (argstart) *argstart = NULL;
	return;
    }

    /*
     * This will guaranteeably be big enough; we can realloc it
     * down later.
     */
    outputline = snewn(1+strlen(cmdline), char);
    outputargv = snewn(strlen(cmdline)+1 / 2, char *);
    outputargstart = snewn(strlen(cmdline)+1 / 2, char *);

    p = cmdline; q = outputline; outputargc = 0;

    while (*p) {
	int quote;

	/* Skip whitespace searching for start of argument. */
	while (*p && isspace(*p)) p++;
	if (!*p) break;

	/* We have an argument; start it. */
	outputargv[outputargc] = q;
	outputargstart[outputargc] = p;
	outputargc++;
	quote = 0;

	/* Copy data into the argument until it's finished. */
	while (*p) {
	    if (!quote && isspace(*p))
		break;		       /* argument is finished */

	    if (*p == '"' || *p == '\\') {
		/*
		 * We have a sequence of zero or more backslashes
		 * followed by a sequence of zero or more quotes.
		 * Count up how many of each, and then deal with
		 * them as appropriate.
		 */
		int i, slashes = 0, quotes = 0;
		while (*p == '\\') slashes++, p++;
		while (*p == '"') quotes++, p++;

		if (!quotes) {
		    /*
		     * Special case: if there are no quotes,
		     * slashes are not special at all, so just copy
		     * n slashes to the output string.
		     */
		    while (slashes--) *q++ = '\\';
		} else {
		    /* Slashes annihilate in pairs. */
		    while (slashes >= 2) slashes -= 2, *q++ = '\\';

		    /* One remaining slash takes out the first quote. */
		    if (slashes) quotes--, *q++ = '"';

		    if (quotes > 0) {
			/* Outside a quote segment, a quote starts one. */
			if (!quote) quotes--, quote = 1;

			/* Now we produce (n+1)/3 literal quotes... */
			for (i = 3; i <= quotes+1; i += 3) *q++ = '"';

			/* ... and end in a quote segment iff 3 divides n. */
			quote = (quotes % 3 == 0);
		    }
		}
	    } else {
		*q++ = *p++;
	    }
	}

	/* At the end of an argument, just append a trailing NUL. */
	*q++ = '\0';
    }

    outputargv = sresize(outputargv, outputargc, char *);
    outputargstart = sresize(outputargstart, outputargc, char *);

    if (argc) *argc = outputargc;
    if (argv) *argv = outputargv; else sfree(outputargv);
    if (argstart) *argstart = outputargstart; else sfree(outputargstart);
}

#ifdef TESTMODE

const struct argv_test {
    const char *cmdline;
    const char *argv[10];
} argv_tests[] = {
    /*
     * We generate this set of tests by invoking ourself with
     * `-generate'.
     */
    {"ab c\" d", {"ab", "c d", NULL}},
    {"a\"b c\" d", {"ab c", "d", NULL}},
    {"a\"\"b c\" d", {"ab", "c d", NULL}},
    {"a\"\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"a\"\"\"\"b c\" d", {"a\"b c", "d", NULL}},
    {"a\"\"\"\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"a\"\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"a\"\"\"\"\"\"\"b c\" d", {"a\"\"b c", "d", NULL}},
    {"a\"\"\"\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"a\\b c\" d", {"a\\b", "c d", NULL}},
    {"a\\\"b c\" d", {"a\"b", "c d", NULL}},
    {"a\\\"\"b c\" d", {"a\"b c", "d", NULL}},
    {"a\\\"\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"a\\\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"a\\\"\"\"\"\"b c\" d", {"a\"\"b c", "d", NULL}},
    {"a\\\"\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"a\\\"\"\"\"\"\"\"b c\" d", {"a\"\"\"b", "c d", NULL}},
    {"a\\\"\"\"\"\"\"\"\"b c\" d", {"a\"\"\"b c", "d", NULL}},
    {"a\\\\b c\" d", {"a\\\\b", "c d", NULL}},
    {"a\\\\\"b c\" d", {"a\\b c", "d", NULL}},
    {"a\\\\\"\"b c\" d", {"a\\b", "c d", NULL}},
    {"a\\\\\"\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"a\\\\\"\"\"\"b c\" d", {"a\\\"b c", "d", NULL}},
    {"a\\\\\"\"\"\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"a\\\\\"\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"a\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\"\"b c", "d", NULL}},
    {"a\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"a\\\\\\b c\" d", {"a\\\\\\b", "c d", NULL}},
    {"a\\\\\\\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"a\\\\\\\"\"b c\" d", {"a\\\"b c", "d", NULL}},
    {"a\\\\\\\"\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"a\\\\\\\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"a\\\\\\\"\"\"\"\"b c\" d", {"a\\\"\"b c", "d", NULL}},
    {"a\\\\\\\"\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"a\\\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b", "c d", NULL}},
    {"a\\\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b c", "d", NULL}},
    {"a\\\\\\\\b c\" d", {"a\\\\\\\\b", "c d", NULL}},
    {"a\\\\\\\\\"b c\" d", {"a\\\\b c", "d", NULL}},
    {"a\\\\\\\\\"\"b c\" d", {"a\\\\b", "c d", NULL}},
    {"a\\\\\\\\\"\"\"b c\" d", {"a\\\\\"b", "c d", NULL}},
    {"a\\\\\\\\\"\"\"\"b c\" d", {"a\\\\\"b c", "d", NULL}},
    {"a\\\\\\\\\"\"\"\"\"b c\" d", {"a\\\\\"b", "c d", NULL}},
    {"a\\\\\\\\\"\"\"\"\"\"b c\" d", {"a\\\\\"\"b", "c d", NULL}},
    {"a\\\\\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\\\"\"b c", "d", NULL}},
    {"a\\\\\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\\\"\"b", "c d", NULL}},
    {"\"ab c\" d", {"ab c", "d", NULL}},
    {"\"a\"b c\" d", {"ab", "c d", NULL}},
    {"\"a\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"\"a\"\"\"b c\" d", {"a\"b c", "d", NULL}},
    {"\"a\"\"\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"\"a\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"\"a\"\"\"\"\"\"b c\" d", {"a\"\"b c", "d", NULL}},
    {"\"a\"\"\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"\"a\"\"\"\"\"\"\"\"b c\" d", {"a\"\"\"b", "c d", NULL}},
    {"\"a\\b c\" d", {"a\\b c", "d", NULL}},
    {"\"a\\\"b c\" d", {"a\"b c", "d", NULL}},
    {"\"a\\\"\"b c\" d", {"a\"b", "c d", NULL}},
    {"\"a\\\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"\"a\\\"\"\"\"b c\" d", {"a\"\"b c", "d", NULL}},
    {"\"a\\\"\"\"\"\"b c\" d", {"a\"\"b", "c d", NULL}},
    {"\"a\\\"\"\"\"\"\"b c\" d", {"a\"\"\"b", "c d", NULL}},
    {"\"a\\\"\"\"\"\"\"\"b c\" d", {"a\"\"\"b c", "d", NULL}},
    {"\"a\\\"\"\"\"\"\"\"\"b c\" d", {"a\"\"\"b", "c d", NULL}},
    {"\"a\\\\b c\" d", {"a\\\\b c", "d", NULL}},
    {"\"a\\\\\"b c\" d", {"a\\b", "c d", NULL}},
    {"\"a\\\\\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"\"a\\\\\"\"\"b c\" d", {"a\\\"b c", "d", NULL}},
    {"\"a\\\\\"\"\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"\"a\\\\\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"\"a\\\\\"\"\"\"\"\"b c\" d", {"a\\\"\"b c", "d", NULL}},
    {"\"a\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"\"a\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b", "c d", NULL}},
    {"\"a\\\\\\b c\" d", {"a\\\\\\b c", "d", NULL}},
    {"\"a\\\\\\\"b c\" d", {"a\\\"b c", "d", NULL}},
    {"\"a\\\\\\\"\"b c\" d", {"a\\\"b", "c d", NULL}},
    {"\"a\\\\\\\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"\"a\\\\\\\"\"\"\"b c\" d", {"a\\\"\"b c", "d", NULL}},
    {"\"a\\\\\\\"\"\"\"\"b c\" d", {"a\\\"\"b", "c d", NULL}},
    {"\"a\\\\\\\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b", "c d", NULL}},
    {"\"a\\\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b c", "d", NULL}},
    {"\"a\\\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\"\"\"b", "c d", NULL}},
    {"\"a\\\\\\\\b c\" d", {"a\\\\\\\\b c", "d", NULL}},
    {"\"a\\\\\\\\\"b c\" d", {"a\\\\b", "c d", NULL}},
    {"\"a\\\\\\\\\"\"b c\" d", {"a\\\\\"b", "c d", NULL}},
    {"\"a\\\\\\\\\"\"\"b c\" d", {"a\\\\\"b c", "d", NULL}},
    {"\"a\\\\\\\\\"\"\"\"b c\" d", {"a\\\\\"b", "c d", NULL}},
    {"\"a\\\\\\\\\"\"\"\"\"b c\" d", {"a\\\\\"\"b", "c d", NULL}},
    {"\"a\\\\\\\\\"\"\"\"\"\"b c\" d", {"a\\\\\"\"b c", "d", NULL}},
    {"\"a\\\\\\\\\"\"\"\"\"\"\"b c\" d", {"a\\\\\"\"b", "c d", NULL}},
    {"\"a\\\\\\\\\"\"\"\"\"\"\"\"b c\" d", {"a\\\\\"\"\"b", "c d", NULL}},
};

int main(int argc, char **argv)
{
    int i, j;

    if (argc > 1) {
	/*
	 * Generation of tests.
	 * 
	 * Given `-splat <args>', we print out a C-style
	 * representation of each argument (in the form "a", "b",
	 * NULL), backslash-escaping each backslash and double
	 * quote.
	 * 
	 * Given `-split <string>', we first doctor `string' by
	 * turning forward slashes into backslashes, single quotes
	 * into double quotes and underscores into spaces; and then
	 * we feed the resulting string to ourself with `-splat'.
	 * 
	 * Given `-generate', we concoct a variety of fun test
	 * cases, encode them in quote-safe form (mapping \, " and
	 * space to /, ' and _ respectively) and feed each one to
	 * `-split'.
	 */
	if (!strcmp(argv[1], "-splat")) {
	    int i;
	    char *p;
	    for (i = 2; i < argc; i++) {
		putchar('"');
		for (p = argv[i]; *p; p++) {
		    if (*p == '\\' || *p == '"')
			putchar('\\');
		    putchar(*p);
		}
		printf("\", ");
	    }
	    printf("NULL");
	    return 0;
	}

	if (!strcmp(argv[1], "-split") && argc > 2) {
	    char *str = malloc(20 + strlen(argv[0]) + strlen(argv[2]));
	    char *p, *q;

	    q = str + sprintf(str, "%s -splat ", argv[0]);
	    printf("    {\"");
	    for (p = argv[2]; *p; p++, q++) {
		switch (*p) {
		  case '/':  printf("\\\\"); *q = '\\'; break;
		  case '\'': printf("\\\""); *q = '"';  break;
		  case '_':  printf(" ");    *q = ' ';  break;
		  default:   putchar(*p);    *q = *p;   break;
		}
	    }
	    *p = '\0';
	    printf("\", {");
	    fflush(stdout);

	    system(str);

	    printf("}},\n");

	    return 0;
	}

	if (!strcmp(argv[1], "-generate")) {
	    char *teststr, *p;
	    int i, initialquote, backslashes, quotes;

	    teststr = malloc(200 + strlen(argv[0]));

	    for (initialquote = 0; initialquote <= 1; initialquote++) {
		for (backslashes = 0; backslashes < 5; backslashes++) {
		    for (quotes = 0; quotes < 9; quotes++) {
			p = teststr + sprintf(teststr, "%s -split ", argv[0]);
			if (initialquote) *p++ = '\'';
			*p++ = 'a';
			for (i = 0; i < backslashes; i++) *p++ = '/';
			for (i = 0; i < quotes; i++) *p++ = '\'';
			*p++ = 'b';
			*p++ = '_';
			*p++ = 'c';
			*p++ = '\'';
			*p++ = '_';
			*p++ = 'd';
			*p = '\0';

			system(teststr);
		    }
		}
	    }
	    return 0;
	}

	fprintf(stderr, "unrecognised option: \"%s\"\n", argv[1]);
	return 1;
    }

    /*
     * If we get here, we were invoked with no arguments, so just
     * run the tests.
     */

    for (i = 0; i < lenof(argv_tests); i++) {
	int ac;
	char **av;

	split_into_argv(argv_tests[i].cmdline, &ac, &av);

	for (j = 0; j < ac && argv_tests[i].argv[j]; j++) {
	    if (strcmp(av[j], argv_tests[i].argv[j])) {
		printf("failed test %d (|%s|) arg %d: |%s| should be |%s|\n",
		       i, argv_tests[i].cmdline,
		       j, av[j], argv_tests[i].argv[j]);
	    }
#ifdef VERBOSE
	    else {
		printf("test %d (|%s|) arg %d: |%s| == |%s|\n",
		       i, argv_tests[i].cmdline,
		       j, av[j], argv_tests[i].argv[j]);
	    }
#endif
	}
	if (j < ac)
	    printf("failed test %d (|%s|): %d args returned, should be %d\n",
		   i, argv_tests[i].cmdline, ac, j);
	if (argv_tests[i].argv[j])
	    printf("failed test %d (|%s|): %d args returned, should be more\n",
		   i, argv_tests[i].cmdline, ac);
    }

    return 0;
}

#endif
