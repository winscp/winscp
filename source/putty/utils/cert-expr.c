/*
 * Parser for the boolean expression language used to configure what
 * host names an OpenSSH certificate will be trusted to sign for.
 */

/*

Language specification
======================

Outer lexical layer: the input expression is broken up into tokens,
with any whitespace between them discarded and ignored. The following
tokens are special:

  ( ) && || !

and the remaining token type is an 'atom', which is any non-empty
sequence of characters from the following set:

  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  abcdefghijklmnopqrstuvwxyz
  0123456789
  .-_*?[]/:

Inner lexical layer: once the boundaries of an 'atom' token have been
determined by the outer lex layer, each atom is further classified
into one of the following subtypes:

 - If it contains no ':' or '/', it's taken to be a wildcard matching
   hostnames, e.g. "*.example.com".

 - If it begins with 'port:' followed by digits, it's taken to be a
   single port number specification, e.g. "port:22".

 - If it begins with 'port:' followed by two digit sequences separated
   by '-', it's taken to be a port number range, e.g. "port:0-1023".

 - Any other atom is reserved for future expansion. (See Rationale.)

Syntax layer: all of those types of atom are interpreted as predicates
applied to the (hostname, port) data configured for the SSH connection
for which the certificate is being validated.

Wildcards are handled using the syntax in wildcard.c. The dot-
separated structure of hostnames is thus not special; the '*' in
"*.example.com" will match any number of subdomains under example.com.

More complex boolean expressions can be made by combining those
predicates using the boolean operators and parentheses, in the obvious
way: && and || are infix operators representing logical AND and OR, !
is a prefix operator representing logical NOT, and parentheses
indicate grouping.

Each of && and || can associate freely with itself (that is, you can
write "a && b && c" without having to parenthesise one or the other
subexpression). But they are forbidden to associate with _each other_.
That is, if you write "a && b || c" or "a || b && c", it's a syntax
error, and you must add parentheses to indicate which operator was
intended to have the higher priority.

Rationale
=========

Atoms: restrictions
-------------------

The characters permitted in the 'atom' token don't include \, even
though it's a special character defined by wildcard.c. That's because
in this restricted context wildcards will never need it: no hostname
contains a literal \, and neither does any hostname contain a literal
instance of any of the wildcard characters that wildcard.c allows you
to use \ to escape.

Atoms: future extension
-----------------------

The specification of the 'atom' token is intended to leave space for
more than one kind of future extension.

Most obviously, additional special predicates similar to "port:", with
different disambiguating prefixes. I don't know what things of that
kind we might need, but space is left for them just in case.

Also, the unused '/' in the permitted-characters spec is intended to
leave open the possibility of allowing certificate acceptance to be
based on IP address, because the usual CIDR syntax for specifying IP
ranges (e.g. "192.168.1.0/24" or "2345:6789:abcd:ef01::/128") would be
lexed as a single atom under these rules.

For the moment, certificate acceptance rules based on IP address are
not supported, because it's not clear what the semantics ought to be.
There are two problems with using IP addresses for this purpose:

 1. Sometimes they come from the DNS, which means you can't trust
    them. The whole idea of SSH is to end-to-end authenticate the host
    key against only the input given _by the user_ to the client. Any
    additional data provided by the network, such as the result of a
    DNS lookup, is suspect.

    On the other hand, sometimes the IP address *is* part of the user
    input, because the user can provide an IP address rather than a
    hostname as the intended connection destination. So there are two
    kinds of IP address, and they should very likely be treated
    differently.

 2. Sometimes the server's IP address is not even *known* by the
    client, if you're connecting via a proxy and leaving DNS lookups
    to the proxy.

So, what should a boolean expression do if it's asked to accept or
reject based on an IP address, and the IP address is unknown or
untrustworthy? I'm not sure, and therefore, in the initial version of
this expression system, I haven't implemented them at all.

But the syntax is still available for a future extension to use, if we
come up with good answers to these questions.

(One possibility would be to evaluate the whole expression in Kleene
three-valued logic, so that every subexpression has the possible
answers TRUE, FALSE and UNKNOWN. If a definite IP address is not
available, IP address predicates evaluate to UNKNOWN. Then, once the
expression as a whole is evaluated, fail closed, by interpreting
UNKNOWN as 'reject'. The effect would be that a positive _or_ negative
constraint on the IP address would cause rejection if the IP address
is not reliably known, because once the predicate itself has returned
UNKNOWN, negating it still gives UNKNOWN. The only way you could still
accept a certificate in that situation would be if the overall
structure of the expression meant that the test of the IP address
couldn't affect the result anyway, e.g. if it was ANDed with another
subexpression that definitely evaluated to FALSE, or ORed with one
that evaluated to TRUE. This system seems conceptually elegant to me,
but the argument against it is that it's complicated and
counterintuitive, which is not a property you want in something a user
is writing for security purposes!)

Operator precedence
-------------------

Why did I choose to make && and || refuse to associate with each
other, instead of applying the usual C precedence rule that && beats
||? Because I think the C precedence rule is essentially arbitrary, in
the sense that when people are writing boolean expressions in practice
based on predicates from the rest of their program, it's about equally
common to want to nest an && within an || and vice versa. So the
default precedence rule only gives the user what they actually wanted
about 50% of the time, and leads to absent-minded errors about as
often as it conveniently allows you to omit a pair of parens.

With my mathematician hat on, it's not so arbitrary. I agree that if
you're *going* to give || and && a relative priority then it makes
more sense to make && the higher-priority one, because if you're
thinking algebraically, && is more multiplicative and || is more
additive. But the pure-maths contexts in which that's convenient have
nothing to do with general boolean expressions in if statements.

This boolean syntax is still close enough to that of C and its
derivatives to allow easy enough expression interchange (not counting
the fact that atoms would need rewriting). Any boolean expression
structure accepted by this syntax is also legal C and means the same
thing; any expression structure accepted by C is either legal and
equivalent in this syntax, or will fail with an error. In no case is
anything accepted but mapped to a different meaning.

 */

#include "putty.h"

typedef enum Token {
    TOK_LPAR, TOK_RPAR,
    TOK_AND, TOK_OR, TOK_NOT,
    TOK_ATOM,
    TOK_END, TOK_ERROR
} Token;

static inline bool is_space(char c)
{
    return (c == ' ' || c == '\n' || c == '\r' || c == '\t' ||
            c == '\f' || c == '\v');
}

static inline bool is_operator_char(char c)
{
    return (c == '(' || c == ')' || c == '&' || c == '|' || c == '!');
}

static inline bool is_atom_char(char c)
{
    return (('A' <= c && c <= 'Z') ||
            ('a' <= c && c <= 'z') ||
            ('0' <= c && c <= '9') ||
            c == '.' || c == '-' || c == '_' || c == '*' || c == '?' ||
            c == '[' || c == ']' || c == '/' || c == ':');
}

static Token lex(ptrlen *text, ptrlen *token, char **err)
{
    const char *p = text->ptr, *e = p + text->len;
    Token type = TOK_ERROR;

    /* Skip whitespace */
    while (p < e && is_space(*p))
        p++;

    { // WINSCP
    const char *start = p;

    if (!(p < e)) {
        type = TOK_END;
        goto out;
    }

    if (is_operator_char(*p)) {
        /* Match boolean-expression tokens */
        static const struct operator {
            ptrlen text;
            Token type;
        } operators[] = {
            {PTRLEN_DECL_LITERAL("("), TOK_LPAR},
            {PTRLEN_DECL_LITERAL(")"), TOK_RPAR},
            {PTRLEN_DECL_LITERAL("&&"), TOK_AND},
            {PTRLEN_DECL_LITERAL("||"), TOK_OR},
            {PTRLEN_DECL_LITERAL("!"), TOK_NOT},
        };

        { // WINSCP
        size_t i;
        for (i = 0; i < lenof(operators); i++) {
            const struct operator *op = &operators[i];
            if (e - p >= op->text.len &&
                ptrlen_eq_ptrlen(op->text, make_ptrlen(p, op->text.len))) {
                p += op->text.len;
                type = op->type;
                goto out;
            }
        }
        } // WINSCP

        /*
         * Report an error if one of the operator characters is used
         * in a way that doesn't match something in that table (e.g. a
         * single &).
         */
        p++;
        type = TOK_ERROR;
        *err = dupstr("unrecognised boolean operator");
        goto out;
    } else if (is_atom_char(*p)) {
        /*
         * Match an 'atom' token, which is any non-empty sequence of
         * characters from the combined set that allows hostname
         * wildcards, IP address ranges and special predicates like
         * port numbers.
         */
        do {
            p++;
        } while (p < e && is_atom_char(*p));

        type = TOK_ATOM;
        goto out;
    } else {
        /*
         * Otherwise, report an error.
         */
        p++;
        type = TOK_ERROR;
        *err = dupstr("unexpected character in expression");
        goto out;
    }

  out:
    *token = make_ptrlen(start, p - start);
    text->ptr = p;
    text->len = e - p;
    return type;
    } // WINSCP
}

typedef enum Operator {
    OP_AND, OP_OR, OP_NOT,
    OP_HOSTNAME_WC, OP_PORT_RANGE
} Operator;

typedef struct ExprNode ExprNode;
struct ExprNode {
    Operator op;
    ptrlen text;
    union {
        struct {
            /* OP_AND, OP_OR */
            ExprNode **subexprs;
            size_t nsubexprs;
        };
        struct {
            /* OP_NOT */
            ExprNode *subexpr;
        };
        struct {
            /* OP_HOSTNAME_WC */
            char *wc;
        };
        struct {
            /* OP_PORT_RANGE */
            unsigned lo, hi;           /* both inclusive */
        };
    };
};

static ExprNode *exprnode_new(Operator op, ptrlen text)
{
    ExprNode *en = snew(ExprNode);
    memset(en, 0, sizeof(*en));
    en->op = op;
    en->text = text;
    return en;
}

static void exprnode_free(ExprNode *en)
{
    switch (en->op) {
      case OP_AND:
      case OP_OR:
        { // WINSCP
        size_t i;
        for (i = 0; i < en->nsubexprs; i++)
            exprnode_free(en->subexprs[i]);
        } // WINSCP
        sfree(en->subexprs);
        break;
      case OP_NOT:
        exprnode_free(en->subexpr);
        break;
      case OP_HOSTNAME_WC:
        sfree(en->wc);
        break;
      case OP_PORT_RANGE:
        break;
      default:
        unreachable("unhandled node type in exprnode_free");
    }

    sfree(en);
}

static unsigned ptrlen_to_port_number(ptrlen input)
{
    unsigned val = 0;
    const char *p, *end; // WINSCP
    for (p = input.ptr, end = p + input.len; p < end; p++) {
        assert('0' <= *p && *p <= '9'); /* expect parser to have checked */
        val = 10 * val + (*p - '0');
        if (val >= 65536)
            val = 65536; /* normalise 'too large' to avoid integer overflow */
    }
    return val;
}

typedef struct ParserState ParserState;
struct ParserState {
    ptrlen currtext;
    Token tok;
    ptrlen toktext;
    char *err;
    ptrlen errloc;
};

static void error(ParserState *ps, char *errtext, ptrlen errloc)
{
    if (!ps->err) {
        ps->err = errtext;
        ps->errloc = errloc;
    } else {
        sfree(errtext);
    }
}

static void advance(ParserState *ps)
{
    char *err = NULL;
    ps->tok = lex(&ps->currtext, &ps->toktext, &err);
    if (ps->tok == TOK_ERROR)
        error(ps, err, ps->toktext);
}

static ExprNode *parse_atom(ParserState *ps);
static ExprNode *parse_expr(ParserState *ps);

static bool atom_is_hostname_wc(ptrlen toktext)
{
    return !ptrlen_contains(toktext, ":/");
}

static ExprNode *parse_atom(ParserState *ps)
{
    if (ps->tok == TOK_LPAR) {
        ptrlen openpar = ps->toktext;
        advance(ps);                   /* eat the ( */

        { // WINSCP
        ExprNode *subexpr = parse_expr(ps);
        if (!subexpr)
            return NULL;

        if (ps->tok != TOK_RPAR) {
            error(ps, dupstr("expected ')' after parenthesised subexpression"),
                  subexpr->text);
            exprnode_free(subexpr);
            return NULL;
        }

        { // WINSCP
        ptrlen closepar = ps->toktext;
        advance(ps);                   /* eat the ) */

        /* We can reuse the existing AST node, but we need to extend
         * its bounds within the input expression to include the
         * parentheses */
        subexpr->text = make_ptrlen_startend(
            openpar.ptr, ptrlen_end(closepar));
        return subexpr;
        } // WINSCP
        } // WINSCP
    }

    if (ps->tok == TOK_NOT) {
        ptrlen notloc = ps->toktext;
        advance(ps);                   /* eat the ! */

        { // WINSCP
        ExprNode *subexpr = parse_atom(ps);
        if (!subexpr)
            return NULL;

        { // WINSCP
        ExprNode *en = exprnode_new(
            OP_NOT, make_ptrlen_startend(
                notloc.ptr, ptrlen_end(subexpr->text)));
        en->subexpr = subexpr;
        return en;
        } // WINSCP
        } // WINSCP
    }

    if (ps->tok == TOK_ATOM) {
        if (atom_is_hostname_wc(ps->toktext)) {
            /* Hostname wildcard. */
            ExprNode *en = exprnode_new(OP_HOSTNAME_WC, ps->toktext);
            en->wc = mkstr(ps->toktext);
            advance(ps);
            return en;
        }

        { // WINSCP
        ptrlen tail;
        if (ptrlen_startswith(ps->toktext, PTRLEN_LITERAL("port:"), &tail)) {
            /* Port number (single or range). */
            unsigned lo, hi;
            char *minus;
            static const char DIGITS[] = "0123456789\0";
            bool parse_ok = false;

            if (tail.len > 0 && ptrlen_contains_only(tail, DIGITS)) {
                lo = ptrlen_to_port_number(tail);
                if (lo >= 65536) {
                    error(ps, dupstr("port number too large"), tail);
                    return NULL;
                }
                hi = lo;
                parse_ok = true;
            } else if ((minus = memchr(tail.ptr, '-', tail.len)) != NULL) {
                ptrlen pl_lo = make_ptrlen_startend(tail.ptr, minus);
                ptrlen pl_hi = make_ptrlen_startend(minus+1, ptrlen_end(tail));
                if (pl_lo.len > 0 && ptrlen_contains_only(pl_lo, DIGITS) &&
                    pl_hi.len > 0 && ptrlen_contains_only(pl_hi, DIGITS)) {

                    lo = ptrlen_to_port_number(pl_lo);
                    if (lo >= 65536) {
                        error(ps, dupstr("port number too large"), pl_lo);
                        return NULL;
                    }

                    hi = ptrlen_to_port_number(pl_hi);
                    if (hi >= 65536) {
                        error(ps, dupstr("port number too large"), pl_hi);
                        return NULL;
                    }

                    if (hi < lo) {
                        error(ps, dupstr("port number range is backwards"),
                              make_ptrlen_startend(pl_lo.ptr,
                                                   ptrlen_end(pl_hi)));
                        return NULL;
                    }

                    parse_ok = true;
                }
            }

            if (!parse_ok) {
                error(ps, dupstr("unable to parse port number specification"),
                      ps->toktext);
                return NULL;
            }


            { // WINSCP
            ExprNode *en = exprnode_new(OP_PORT_RANGE, ps->toktext);
            en->lo = lo;
            en->hi = hi;
            advance(ps);
            return en;
            } // WINSCP
        }
        } // WINSCP
    }

    error(ps, dupstr("expected a predicate or a parenthesised subexpression"),
          ps->toktext);
    return NULL;
}

static ExprNode *parse_expr(ParserState *ps)
{
    ExprNode *subexpr = parse_atom(ps);
    if (!subexpr)
        return NULL;

    if (ps->tok != TOK_AND && ps->tok != TOK_OR)
        return subexpr;

    { // WINSCP
    Token operator = ps->tok;
    ExprNode *en = exprnode_new(ps->tok == TOK_AND ? OP_AND : OP_OR,
                                subexpr->text);
    size_t subexprs_size = 0;

    sgrowarray(en->subexprs, subexprs_size, en->nsubexprs);
    en->subexprs[en->nsubexprs++] = subexpr;

    while (true) {
        advance(ps);                   /* eat the operator */

        subexpr = parse_atom(ps);
        if (!subexpr) {
            exprnode_free(en);
            return NULL;
        }
        sgrowarray(en->subexprs, subexprs_size, en->nsubexprs);
        en->subexprs[en->nsubexprs++] = subexpr;
        en->text = make_ptrlen_startend(
            en->text.ptr, ptrlen_end(subexpr->text));

        if (ps->tok != TOK_AND && ps->tok != TOK_OR)
            return en;

        if (ps->tok != operator) {
            error(ps, dupstr("expected parentheses to disambiguate && and || "
                             "on either side of expression"), subexpr->text);
            exprnode_free(en);
            return NULL;
        }
    }
    } // WINSCP
}

static ExprNode *parse(ptrlen expr, char **error_msg, ptrlen *error_loc)
{
    ParserState ps[1];
    ps->currtext = expr;
    ps->err = NULL;
    advance(ps);

    { // WINSCP
    ExprNode *en = parse_expr(ps);
    if (en && ps->tok != TOK_END) {
        error(ps, dupstr("unexpected text at end of expression"),
              make_ptrlen_startend(ps->toktext.ptr, ptrlen_end(expr)));
        exprnode_free(en);
        en = NULL;
    }

    if (!en) {
        if (error_msg)
            *error_msg = ps->err;
        else
            sfree(ps->err);
        if (error_loc)
            *error_loc = ps->errloc;
        return NULL;
    }

    return en;
    } // WINSCP
}

static bool eval(ExprNode *en, const char *hostname, unsigned port)
{
    switch (en->op) {
      case OP_AND:
        { // WINSCP
        size_t i;
        for (i = 0; i < en->nsubexprs; i++)
            if (!eval(en->subexprs[i], hostname, port))
                return false;
        return true;
        } // WINSCP

      case OP_OR:
        { // WINSCP
        size_t i;
        for (i = 0; i < en->nsubexprs; i++)
            if (eval(en->subexprs[i], hostname, port))
                return true;
        return false;
        } // WINSCP

      case OP_NOT:
        return !eval(en->subexpr, hostname, port);

      case OP_HOSTNAME_WC:
        return wc_match(en->wc, hostname);

      case OP_PORT_RANGE:
        return en->lo <= port && port <= en->hi;

      default:
        unreachable("unhandled node type in eval");
    }
}

bool cert_expr_match_str(const char *expression,
                         const char *hostname, unsigned port)
{
    ExprNode *en = parse(ptrlen_from_asciz(expression), NULL, NULL);
    if (!en)
        return false;

    { // WINSCP
    bool matched = eval(en, hostname, port);
    exprnode_free(en);
    return matched;
    } // WINSCP
}

bool cert_expr_valid(const char *expression,
                     char **error_msg, ptrlen *error_loc)
{
    ExprNode *en = parse(ptrlen_from_asciz(expression), error_msg, error_loc);
    if (en) {
        exprnode_free(en);
        return true;
    } else {
        return false;
    }
}

struct CertExprBuilder {
    char **wcs;
    size_t nwcs, wcsize;
};

CertExprBuilder *cert_expr_builder_new(void)
{
    CertExprBuilder *eb = snew(CertExprBuilder);
    eb->wcs = NULL;
    eb->nwcs = eb->wcsize = 0;
    return eb;
}

void cert_expr_builder_free(CertExprBuilder *eb)
{
    size_t i; // WINSCP
    for (i = 0; i < eb->nwcs; i++)
        sfree(eb->wcs[i]);
    sfree(eb->wcs);
    sfree(eb);
}

void cert_expr_builder_add(CertExprBuilder *eb, const char *wildcard)
{
    /* Check this wildcard is lexically valid as an atom */
    ptrlen orig = ptrlen_from_asciz(wildcard), pl = orig;
    ptrlen toktext;
    char *err;
    Token tok = lex(&pl, &toktext, &err);
    if (!(tok == TOK_ATOM &&
          toktext.ptr == orig.ptr &&
          toktext.len == orig.len &&
          atom_is_hostname_wc(toktext))) {
        if (tok == TOK_ERROR)
            sfree(err);
        return;
    }

    sgrowarray(eb->wcs, eb->wcsize, eb->nwcs);
    eb->wcs[eb->nwcs++] = mkstr(orig);
}

char *cert_expr_expression(CertExprBuilder *eb)
{
    strbuf *sb = strbuf_new();
    size_t i; // WINSCP
    for (i = 0; i < eb->nwcs; i++) {
        if (i)
            put_dataz(sb, " || ");
        put_dataz(sb, eb->wcs[i]);
    }
    return strbuf_to_str(sb);
}

#ifdef TEST

void out_of_memory(void) { fprintf(stderr, "out of memory\n"); abort(); }

static void exprnode_dump(BinarySink *bs, ExprNode *en, const char *origtext)
{
    put_fmt(bs, "(%zu:%zu ",
            (size_t)((const char *)en->text.ptr - origtext),
            (size_t)((const char *)ptrlen_end(en->text) - origtext));
    switch (en->op) {
      case OP_AND:
      case OP_OR:
        put_dataz(bs, en->op == OP_AND ? "and" : "or");
        for (size_t i = 0; i < en->nsubexprs; i++) {
            put_byte(bs, ' ');
            exprnode_dump(bs, en->subexprs[i], origtext);
        }
        break;
      case OP_NOT:
        put_dataz(bs, "not ");
        exprnode_dump(bs, en->subexpr, origtext);
        break;
      case OP_HOSTNAME_WC:
        put_dataz(bs, "host-wc '");
        put_dataz(bs, en->wc);
        put_byte(bs, '\'');
        break;
      case OP_PORT_RANGE:
        put_fmt(bs, "port-range %u %u", en->lo, en->hi);
        break;
      default:
        unreachable("unhandled node type in exprnode_dump");
    }
    put_byte(bs, ')');
}

static const struct ParseTest {
    const char *file;
    int line;
    const char *expr, *output;
} parsetests[] = {
#define T(expr_, output_) { \
        .file=__FILE__, .line=__LINE__, .expr=expr_, .output=output_}

    T("*.example.com", "(0:13 host-wc '*.example.com')"),
    T("port:0", "(0:6 port-range 0 0)"),
    T("port:22", "(0:7 port-range 22 22)"),
    T("port:22-22", "(0:10 port-range 22 22)"),
    T("port:65535", "(0:10 port-range 65535 65535)"),
    T("port:0-1023", "(0:11 port-range 0 1023)"),

    T("&", "ERR:0:1:unrecognised boolean operator"),
    T("|", "ERR:0:1:unrecognised boolean operator"),
    T(";", "ERR:0:1:unexpected character in expression"),
    T("port:", "ERR:0:5:unable to parse port number specification"),
    T("port:abc", "ERR:0:8:unable to parse port number specification"),
    T("port:65536", "ERR:5:10:port number too large"),
    T("port:65536-65537", "ERR:5:10:port number too large"),
    T("port:0-65536", "ERR:7:12:port number too large"),
    T("port:23-22", "ERR:5:10:port number range is backwards"),

    T("a", "(0:1 host-wc 'a')"),
    T("(a)", "(0:3 host-wc 'a')"),
    T("((a))", "(0:5 host-wc 'a')"),
    T(" (\n(\ra\t)\f)\v", "(1:10 host-wc 'a')"),
    T("a&&b", "(0:4 and (0:1 host-wc 'a') (3:4 host-wc 'b'))"),
    T("a||b", "(0:4 or (0:1 host-wc 'a') (3:4 host-wc 'b'))"),
    T("a&&b&&c", "(0:7 and (0:1 host-wc 'a') (3:4 host-wc 'b') (6:7 host-wc 'c'))"),
    T("a||b||c", "(0:7 or (0:1 host-wc 'a') (3:4 host-wc 'b') (6:7 host-wc 'c'))"),
    T("a&&(b||c)", "(0:9 and (0:1 host-wc 'a') (3:9 or (4:5 host-wc 'b') (7:8 host-wc 'c')))"),
    T("a||(b&&c)", "(0:9 or (0:1 host-wc 'a') (3:9 and (4:5 host-wc 'b') (7:8 host-wc 'c')))"),
    T("(a&&b)||c", "(0:9 or (0:6 and (1:2 host-wc 'a') (4:5 host-wc 'b')) (8:9 host-wc 'c'))"),
    T("(a||b)&&c", "(0:9 and (0:6 or (1:2 host-wc 'a') (4:5 host-wc 'b')) (8:9 host-wc 'c'))"),
    T("!a&&b", "(0:5 and (0:2 not (1:2 host-wc 'a')) (4:5 host-wc 'b'))"),
    T("a&&!b&&c", "(0:8 and (0:1 host-wc 'a') (3:5 not (4:5 host-wc 'b')) (7:8 host-wc 'c'))"),
    T("!a||b", "(0:5 or (0:2 not (1:2 host-wc 'a')) (4:5 host-wc 'b'))"),
    T("a||!b||c", "(0:8 or (0:1 host-wc 'a') (3:5 not (4:5 host-wc 'b')) (7:8 host-wc 'c'))"),

    T("", "ERR:0:0:expected a predicate or a parenthesised subexpression"),
    T("a &&", "ERR:4:4:expected a predicate or a parenthesised subexpression"),
    T("a ||", "ERR:4:4:expected a predicate or a parenthesised subexpression"),
    T("a b c d", "ERR:2:7:unexpected text at end of expression"),
    T("(", "ERR:1:1:expected a predicate or a parenthesised subexpression"),
    T("(a", "ERR:1:2:expected ')' after parenthesised subexpression"),
    T("(a b", "ERR:1:2:expected ')' after parenthesised subexpression"),
    T("a&&b&&c||d||e", "ERR:6:7:expected parentheses to disambiguate && and || on either side of expression"),
    T("a||b||c&&d&&e", "ERR:6:7:expected parentheses to disambiguate && and || on either side of expression"),
    T("!", "ERR:1:1:expected a predicate or a parenthesised subexpression"),

    T("!a", "(0:2 not (1:2 host-wc 'a'))"),

#undef T
};

static const struct EvalTest {
    const char *file;
    int line;
    const char *expr;
    const char *host;
    unsigned port;
    bool output;
} evaltests[] = {
#define T(expr_, host_, port_, output_) { \
        .file=__FILE__, .line=__LINE__, \
        .expr=expr_, .host=host_, .port=port_, .output=output_}

    T("*.example.com", "hostname.example.com", 22, true),
    T("*.example.com", "hostname.example.org", 22, false),
    T("*.example.com", "hostname.dept.example.com", 22, true),
    T("*.example.com && port:22", "hostname.example.com", 21, false),
    T("*.example.com && port:22", "hostname.example.com", 22, true),
    T("*.example.com && port:22", "hostname.example.com", 23, false),
    T("*.example.com && port:22-24", "hostname.example.com", 21, false),
    T("*.example.com && port:22-24", "hostname.example.com", 22, true),
    T("*.example.com && port:22-24", "hostname.example.com", 23, true),
    T("*.example.com && port:22-24", "hostname.example.com", 24, true),
    T("*.example.com && port:22-24", "hostname.example.com", 25, false),

    T("*a* && *b* && *c*", "", 22, false),
    T("*a* && *b* && *c*", "a", 22, false),
    T("*a* && *b* && *c*", "b", 22, false),
    T("*a* && *b* && *c*", "c", 22, false),
    T("*a* && *b* && *c*", "ab", 22, false),
    T("*a* && *b* && *c*", "ac", 22, false),
    T("*a* && *b* && *c*", "bc", 22, false),
    T("*a* && *b* && *c*", "abc", 22, true),

    T("*a* || *b* || *c*", "", 22, false),
    T("*a* || *b* || *c*", "a", 22, true),
    T("*a* || *b* || *c*", "b", 22, true),
    T("*a* || *b* || *c*", "c", 22, true),
    T("*a* || *b* || *c*", "ab", 22, true),
    T("*a* || *b* || *c*", "ac", 22, true),
    T("*a* || *b* || *c*", "bc", 22, true),
    T("*a* || *b* || *c*", "abc", 22, true),

    T("*a* && !*b* && *c*", "", 22, false),
    T("*a* && !*b* && *c*", "a", 22, false),
    T("*a* && !*b* && *c*", "b", 22, false),
    T("*a* && !*b* && *c*", "c", 22, false),
    T("*a* && !*b* && *c*", "ab", 22, false),
    T("*a* && !*b* && *c*", "ac", 22, true),
    T("*a* && !*b* && *c*", "bc", 22, false),
    T("*a* && !*b* && *c*", "abc", 22, false),

    T("*a* || !*b* || *c*", "", 22, true),
    T("*a* || !*b* || *c*", "a", 22, true),
    T("*a* || !*b* || *c*", "b", 22, false),
    T("*a* || !*b* || *c*", "c", 22, true),
    T("*a* || !*b* || *c*", "ab", 22, true),
    T("*a* || !*b* || *c*", "ac", 22, true),
    T("*a* || !*b* || *c*", "bc", 22, true),
    T("*a* || !*b* || *c*", "abc", 22, true),

#undef T
};

int main(int argc, char **argv)
{
    if (argc > 1) {
        /*
         * Parse an expression from the command line.
         */

        ptrlen expr = ptrlen_from_asciz(argv[1]);
        char *error_msg;
        ptrlen error_loc;
        ExprNode *en = parse(expr, &error_msg, &error_loc);
        if (!en) {
            fprintf(stderr, "ERR:%zu:%zu:%s\n",
                    (size_t)((const char *)error_loc.ptr - argv[1]),
                    (size_t)((const char *)ptrlen_end(error_loc) - argv[1]),
                    error_msg);
            fprintf(stderr, "%.*s\n", PTRLEN_PRINTF(expr));
            for (const char *p = expr.ptr, *e = error_loc.ptr; p<e; p++)
                fputc(' ', stderr);
            for (size_t i = 0; i < error_loc.len || i < 1; i++)
                fputc('^', stderr);
            fputc('\n', stderr);
            sfree(error_msg);
            return 1;
        }

        if (argc > 2) {
            /*
             * Test-evaluate against a host/port pair given on the
             * command line.
             */
            const char *host = argv[2];
            unsigned port = (argc > 3 ? strtoul(argv[3], NULL, 0) : 22);
            bool result = eval(en, host, port);
            printf("%s\n", result ? "accept" : "reject");
        } else {
            /*
             * Just dump the result of parsing the expression.
             */
            stdio_sink ss[1];
            stdio_sink_init(ss, stdout);
            exprnode_dump(BinarySink_UPCAST(ss), en, expr.ptr);
            put_byte(ss, '\n');
        }

        exprnode_free(en);

        return 0;
    } else {
        /*
         * Run our automated tests.
         */
        size_t pass = 0, fail = 0;

        for (size_t i = 0; i < lenof(parsetests); i++) {
            const struct ParseTest *test = &parsetests[i];

            ptrlen expr = ptrlen_from_asciz(test->expr);
            char *error_msg;
            ptrlen error_loc;
            ExprNode *en = parse(expr, &error_msg, &error_loc);

            strbuf *output = strbuf_new();
            if (!en) {
                put_fmt(output, "ERR:%zu:%zu:%s",
                        (size_t)((const char *)error_loc.ptr - test->expr),
                        (size_t)((const char *)ptrlen_end(error_loc) -
                                 test->expr),
                        error_msg);
                sfree(error_msg);
            } else {
                exprnode_dump(BinarySink_UPCAST(output), en, expr.ptr);
                exprnode_free(en);
            }

            if (ptrlen_eq_ptrlen(ptrlen_from_strbuf(output),
                                 ptrlen_from_asciz(test->output))) {
                pass++;
            } else {
                fprintf(stderr, "FAIL: parsetests[%zu] @ %s:%d:\n"
                        "  expression: %s\n"
                        "  expected:   %s\n"
                        "  actual:     %s\n",
                        i, test->file, test->line, test->expr,
                        test->output, output->s);
                fail++;
            }

            strbuf_free(output);
        }

        for (size_t i = 0; i < lenof(evaltests); i++) {
            const struct EvalTest *test = &evaltests[i];

            ptrlen expr = ptrlen_from_asciz(test->expr);
            char *error_msg;
            ptrlen error_loc;
            ExprNode *en = parse(expr, &error_msg, &error_loc);

            if (!en) {
                fprintf(stderr, "FAIL: evaltests[%zu] @ %s:%d:\n"
                        "  expression:  %s\n"
                        "  parse error: %zu:%zu:%s\n",
                        i, test->file, test->line, test->expr,
                        (size_t)((const char *)error_loc.ptr - test->expr),
                        (size_t)((const char *)ptrlen_end(error_loc) -
                                 test->expr),
                        error_msg);
                sfree(error_msg);
            } else {
                bool output = eval(en, test->host, test->port);
                if (output == test->output) {
                    pass++;
                } else {
                    fprintf(stderr, "FAIL: evaltests[%zu] @ %s:%d:\n"
                            "  expression: %s\n"
                            "  host:       %s\n"
                            "  port:       %u\n"
                            "  expected:   %s\n"
                            "  actual:     %s\n",
                            i, test->file, test->line, test->expr,
                            test->host, test->port,
                            test->output ? "accept" : "reject",
                            output ? "accept" : "reject");
                    fail++;
                }
                exprnode_free(en);
            }
        }

        fprintf(stderr, "pass %zu fail %zu total %zu\n",
                pass, fail, pass+fail);
        return fail != 0;
    }
}

#endif // TEST
