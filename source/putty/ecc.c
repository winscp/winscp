#include <assert.h>

#include "ssh.h"
#include "mpint.h"
#include "ecc.h"

/* ----------------------------------------------------------------------
 * Weierstrass curves.
 */

struct WeierstrassPoint {
    /*
     * Internally, we represent a point using 'Jacobian coordinates',
     * which are three values X,Y,Z whose relation to the affine
     * coordinates x,y is that x = X/Z^2 and y = Y/Z^3.
     *
     * This allows us to do most of our calculations without having to
     * take an inverse mod p: every time the obvious affine formulae
     * would need you to divide by something, you instead multiply it
     * into the 'denominator' coordinate Z. You only have to actually
     * take the inverse of Z when you need to get the affine
     * coordinates back out, which means you do it once after your
     * entire computation instead of at every intermediate step.
     *
     * The point at infinity is represented by setting all three
     * coordinates to zero.
     *
     * These values are also stored in the Montgomery-multiplication
     * transformed representation.
     */
    mp_int *X, *Y, *Z;

    WeierstrassCurve *wc;
};

struct WeierstrassCurve {
    /* Prime modulus of the finite field. */
    mp_int *p;

    /* Persistent Montgomery context for doing arithmetic mod p. */
    MontyContext *mc;

    /* Modsqrt context for point decompression. NULL if this curve was
     * constructed without providing nonsquare_mod_p. */
    ModsqrtContext *sc;

    /* Parameters of the curve, in Montgomery-multiplication
     * transformed form. */
    mp_int *a, *b;
};

WeierstrassCurve *ecc_weierstrass_curve(
    mp_int *p, mp_int *a, mp_int *b, mp_int *nonsquare_mod_p)
{
    WeierstrassCurve *wc = snew(WeierstrassCurve);
    wc->p = mp_copy(p);
    wc->mc = monty_new(p);
    wc->a = monty_import(wc->mc, a);
    wc->b = monty_import(wc->mc, b);

    if (nonsquare_mod_p)
        wc->sc = modsqrt_new(p, nonsquare_mod_p);
    else
        wc->sc = NULL;

    return wc;
}

void ecc_weierstrass_curve_free(WeierstrassCurve *wc)
{
    mp_free(wc->p);
    mp_free(wc->a);
    mp_free(wc->b);
    monty_free(wc->mc);
    if (wc->sc)
        modsqrt_free(wc->sc);
    sfree(wc);
}

static WeierstrassPoint *ecc_weierstrass_point_new_empty(WeierstrassCurve *wc)
{
    WeierstrassPoint *wp = snew(WeierstrassPoint);
    wp->wc = wc;
    wp->X = wp->Y = wp->Z = NULL;
    return wp;
}

static WeierstrassPoint *ecc_weierstrass_point_new_imported(
    WeierstrassCurve *wc, mp_int *monty_x, mp_int *monty_y)
{
    WeierstrassPoint *wp = ecc_weierstrass_point_new_empty(wc);
    wp->X = monty_x;
    wp->Y = monty_y;
    wp->Z = mp_copy(monty_identity(wc->mc));
    return wp;
}

WeierstrassPoint *ecc_weierstrass_point_new(
    WeierstrassCurve *wc, mp_int *x, mp_int *y)
{
    return ecc_weierstrass_point_new_imported(
        wc, monty_import(wc->mc, x), monty_import(wc->mc, y));
}

WeierstrassPoint *ecc_weierstrass_point_new_identity(WeierstrassCurve *wc)
{
    WeierstrassPoint *wp = ecc_weierstrass_point_new_empty(wc);
    size_t bits = mp_max_bits(wc->p);
    wp->X = mp_new(bits);
    wp->Y = mp_new(bits);
    wp->Z = mp_new(bits);
    return wp;
}

void ecc_weierstrass_point_copy_into(
    WeierstrassPoint *dest, WeierstrassPoint *src)
{
    mp_copy_into(dest->X, src->X);
    mp_copy_into(dest->Y, src->Y);
    mp_copy_into(dest->Z, src->Z);
}

WeierstrassPoint *ecc_weierstrass_point_copy(WeierstrassPoint *orig)
{
    WeierstrassPoint *wp = ecc_weierstrass_point_new_empty(orig->wc);
    wp->X = mp_copy(orig->X);
    wp->Y = mp_copy(orig->Y);
    wp->Z = mp_copy(orig->Z);
    return wp;
}

void ecc_weierstrass_point_free(WeierstrassPoint *wp)
{
    mp_free(wp->X);
    mp_free(wp->Y);
    mp_free(wp->Z);
    smemclr(wp, sizeof(*wp));
    sfree(wp);
}

WeierstrassPoint *ecc_weierstrass_point_new_from_x(
    WeierstrassCurve *wc, mp_int *xorig, unsigned desired_y_parity)
{
    assert(wc->sc);

    /*
     * The curve equation is y^2 = x^3 + ax + b, which is already
     * conveniently in a form where we can compute the RHS and take
     * the square root of it to get y.
     */
    unsigned success;

    mp_int *x = monty_import(wc->mc, xorig);

    /*
     * Compute the RHS of the curve equation. We don't need to take
     * account of z here, because we're constructing the point from
     * scratch. So it really is just x^3 + ax + b.
     */
    mp_int *x2 = monty_mul(wc->mc, x, x);
    mp_int *x2_plus_a = monty_add(wc->mc, x2, wc->a);
    mp_int *x3_plus_ax = monty_mul(wc->mc, x2_plus_a, x);
    mp_int *rhs = monty_add(wc->mc, x3_plus_ax, wc->b);
    mp_free(x2);
    mp_free(x2_plus_a);
    mp_free(x3_plus_ax);

    mp_int *y = monty_modsqrt(wc->sc, rhs, &success);
    mp_free(rhs);

    if (!success) {
        /* Failure! x^3+ax+b worked out to be a number that has no
         * square root mod p. In this situation there's no point in
         * trying to be time-constant, since the protocol sequence is
         * going to diverge anyway when we complain to whoever gave us
         * this bogus value. */
        mp_free(x);
        mp_free(y);
        return NULL;
    }

    /*
     * Choose whichever of y and p-y has the specified parity (of its
     * lowest positive residue mod p).
     */
    mp_int *tmp = monty_export(wc->mc, y);
    unsigned flip = (mp_get_bit(tmp, 0) ^ desired_y_parity) & 1;
    mp_sub_into(tmp, wc->p, y);
    mp_select_into(y, y, tmp, flip);
    mp_free(tmp);

    return ecc_weierstrass_point_new_imported(wc, x, y);
}

static void ecc_weierstrass_cond_overwrite(
    WeierstrassPoint *dest, WeierstrassPoint *src, unsigned overwrite)
{
    mp_select_into(dest->X, dest->X, src->X, overwrite);
    mp_select_into(dest->Y, dest->Y, src->Y, overwrite);
    mp_select_into(dest->Z, dest->Z, src->Z, overwrite);
}

static void ecc_weierstrass_cond_swap(
    WeierstrassPoint *P, WeierstrassPoint *Q, unsigned swap)
{
    mp_cond_swap(P->X, Q->X, swap);
    mp_cond_swap(P->Y, Q->Y, swap);
    mp_cond_swap(P->Z, Q->Z, swap);
}

/*
 * Shared code between all three of the basic arithmetic functions:
 * once we've determined the slope of the line that we're intersecting
 * the curve with, this takes care of finding the coordinates of the
 * third intersection point (given the two input x-coordinates and one
 * of the y-coords) and negating it to generate the output.
 */
static inline void ecc_weierstrass_epilogue(
    mp_int *Px, mp_int *Qx, mp_int *Py, mp_int *common_Z,
    mp_int *lambda_n, mp_int *lambda_d, WeierstrassPoint *out)
{
    WeierstrassCurve *wc = out->wc;

    /* Powers of the numerator and denominator of the slope lambda */
    mp_int *lambda_n2 = monty_mul(wc->mc, lambda_n, lambda_n);
    mp_int *lambda_d2 = monty_mul(wc->mc, lambda_d, lambda_d);
    mp_int *lambda_d3 = monty_mul(wc->mc, lambda_d, lambda_d2);

    /* Make the output x-coordinate */
    mp_int *xsum = monty_add(wc->mc, Px, Qx);
    mp_int *lambda_d2_xsum = monty_mul(wc->mc, lambda_d2, xsum);
    out->X = monty_sub(wc->mc, lambda_n2, lambda_d2_xsum);

    /* Make the output y-coordinate */
    mp_int *lambda_d2_Px = monty_mul(wc->mc, lambda_d2, Px);
    mp_int *xdiff = monty_sub(wc->mc, lambda_d2_Px, out->X);
    mp_int *lambda_n_xdiff = monty_mul(wc->mc, lambda_n, xdiff);
    mp_int *lambda_d3_Py = monty_mul(wc->mc, lambda_d3, Py);
    out->Y = monty_sub(wc->mc, lambda_n_xdiff, lambda_d3_Py);

    /* Make the output z-coordinate */
    out->Z = monty_mul(wc->mc, common_Z, lambda_d);

    mp_free(lambda_n2);
    mp_free(lambda_d2);
    mp_free(lambda_d3);
    mp_free(xsum);
    mp_free(xdiff);
    mp_free(lambda_d2_xsum);
    mp_free(lambda_n_xdiff);
    mp_free(lambda_d2_Px);
    mp_free(lambda_d3_Py);
}

/*
 * Shared code between add and add_general: put the two input points
 * over a common denominator, and determine the slope lambda of the
 * line through both of them. If the points have the same
 * x-coordinate, then the slope will be returned with a zero
 * denominator.
 */
static inline void ecc_weierstrass_add_prologue(
    WeierstrassPoint *P, WeierstrassPoint *Q,
    mp_int **Px, mp_int **Py, mp_int **Qx, mp_int **denom,
    mp_int **lambda_n, mp_int **lambda_d)
{
    WeierstrassCurve *wc = P->wc;

    /* Powers of the points' denominators */
    mp_int *Pz2 = monty_mul(wc->mc, P->Z, P->Z);
    mp_int *Pz3 = monty_mul(wc->mc, Pz2, P->Z);
    mp_int *Qz2 = monty_mul(wc->mc, Q->Z, Q->Z);
    mp_int *Qz3 = monty_mul(wc->mc, Qz2, Q->Z);

    /* Points' x,y coordinates scaled by the other one's denominator
     * (raised to the appropriate power) */
    *Px = monty_mul(wc->mc, P->X, Qz2);
    *Py = monty_mul(wc->mc, P->Y, Qz3);
    *Qx = monty_mul(wc->mc, Q->X, Pz2);
    mp_int *Qy = monty_mul(wc->mc, Q->Y, Pz3);

    /* Common denominator */
    *denom = monty_mul(wc->mc, P->Z, Q->Z);

    /* Slope of the line through the two points, if P != Q */
    *lambda_n = monty_sub(wc->mc, Qy, *Py);
    *lambda_d = monty_sub(wc->mc, *Qx, *Px);

    mp_free(Pz2);
    mp_free(Pz3);
    mp_free(Qz2);
    mp_free(Qz3);
    mp_free(Qy);
}

WeierstrassPoint *ecc_weierstrass_add(WeierstrassPoint *P, WeierstrassPoint *Q)
{
    WeierstrassCurve *wc = P->wc;
    assert(Q->wc == wc);

    WeierstrassPoint *S = ecc_weierstrass_point_new_empty(wc);

    mp_int *Px, *Py, *Qx, *denom, *lambda_n, *lambda_d;
    ecc_weierstrass_add_prologue(
        P, Q, &Px, &Py, &Qx, &denom, &lambda_n, &lambda_d);

    /* Never expect to have received two mutually inverse inputs, or
     * two identical ones (which would make this a doubling). In other
     * words, the two input x-coordinates (after putting over a common
     * denominator) should never have been equal. */
    assert(!mp_eq_integer(lambda_n, 0));

    /* Now go to the common epilogue code. */
    ecc_weierstrass_epilogue(Px, Qx, Py, denom, lambda_n, lambda_d, S);

    mp_free(Px);
    mp_free(Py);
    mp_free(Qx);
    mp_free(denom);
    mp_free(lambda_n);
    mp_free(lambda_d);

    return S;
}

/*
 * Code to determine the slope of the line you need to intersect with
 * the curve in the case where you're adding a point to itself. In
 * this situation you can't just say "the line through both input
 * points" because that's under-determined; instead, you have to take
 * the _tangent_ to the curve at the given point, by differentiating
 * the curve equation y^2=x^3+ax+b to get 2y dy/dx = 3x^2+a.
 */
static inline void ecc_weierstrass_tangent_slope(
    WeierstrassPoint *P, mp_int **lambda_n, mp_int **lambda_d)
{
    WeierstrassCurve *wc = P->wc;

    mp_int *X2 = monty_mul(wc->mc, P->X, P->X);
    mp_int *twoX2 = monty_add(wc->mc, X2, X2);
    mp_int *threeX2 = monty_add(wc->mc, twoX2, X2);
    mp_int *Z2 = monty_mul(wc->mc, P->Z, P->Z);
    mp_int *Z4 = monty_mul(wc->mc, Z2, Z2);
    mp_int *aZ4 = monty_mul(wc->mc, wc->a, Z4);

    *lambda_n = monty_add(wc->mc, threeX2, aZ4);
    *lambda_d = monty_add(wc->mc, P->Y, P->Y);

    mp_free(X2);
    mp_free(twoX2);
    mp_free(threeX2);
    mp_free(Z2);
    mp_free(Z4);
    mp_free(aZ4);
}

WeierstrassPoint *ecc_weierstrass_double(WeierstrassPoint *P)
{
    WeierstrassCurve *wc = P->wc;
    WeierstrassPoint *D = ecc_weierstrass_point_new_empty(wc);

    mp_int *lambda_n, *lambda_d;
    ecc_weierstrass_tangent_slope(P, &lambda_n, &lambda_d);
    ecc_weierstrass_epilogue(P->X, P->X, P->Y, P->Z, lambda_n, lambda_d, D);
    mp_free(lambda_n);
    mp_free(lambda_d);

    return D;
}

static inline void ecc_weierstrass_select_into(
    WeierstrassPoint *dest, WeierstrassPoint *P, WeierstrassPoint *Q,
    unsigned choose_Q)
{
    mp_select_into(dest->X, P->X, Q->X, choose_Q);
    mp_select_into(dest->Y, P->Y, Q->Y, choose_Q);
    mp_select_into(dest->Z, P->Z, Q->Z, choose_Q);
}

WeierstrassPoint *ecc_weierstrass_add_general(
    WeierstrassPoint *P, WeierstrassPoint *Q)
{
    WeierstrassCurve *wc = P->wc;
    assert(Q->wc == wc);

    WeierstrassPoint *S = ecc_weierstrass_point_new_empty(wc);

    /* Parameters for the epilogue, and slope of the line if P != Q */
    mp_int *Px, *Py, *Qx, *denom, *lambda_n, *lambda_d;
    ecc_weierstrass_add_prologue(
        P, Q, &Px, &Py, &Qx, &denom, &lambda_n, &lambda_d);

    /* Slope if P == Q */
    mp_int *lambda_n_tangent, *lambda_d_tangent;
    ecc_weierstrass_tangent_slope(P, &lambda_n_tangent, &lambda_d_tangent);

    /* Select between those slopes depending on whether P == Q */
    unsigned same_x_coord = mp_eq_integer(lambda_d, 0);
    unsigned same_y_coord = mp_eq_integer(lambda_n, 0);
    unsigned equality = same_x_coord & same_y_coord;
    mp_select_into(lambda_n, lambda_n, lambda_n_tangent, equality);
    mp_select_into(lambda_d, lambda_d, lambda_d_tangent, equality);

    /* Now go to the common code between addition and doubling */
    ecc_weierstrass_epilogue(Px, Qx, Py, denom, lambda_n, lambda_d, S);

    /* Check for the input identity cases, and overwrite the output if
     * necessary. */
    ecc_weierstrass_select_into(S, S, Q, mp_eq_integer(P->Z, 0));
    ecc_weierstrass_select_into(S, S, P, mp_eq_integer(Q->Z, 0));

    /*
     * In the case where P == -Q and so the output is the identity,
     * we'll have calculated lambda_d = 0 and so the output will have
     * z==0 already. Detect that and use it to normalise the other two
     * coordinates to zero.
     */
    unsigned output_id = mp_eq_integer(S->Z, 0);
    mp_cond_clear(S->X, output_id);
    mp_cond_clear(S->Y, output_id);

    mp_free(Px);
    mp_free(Py);
    mp_free(Qx);
    mp_free(denom);
    mp_free(lambda_n);
    mp_free(lambda_d);
    mp_free(lambda_n_tangent);
    mp_free(lambda_d_tangent);

    return S;
}

WeierstrassPoint *ecc_weierstrass_multiply(WeierstrassPoint *B, mp_int *n)
{
    WeierstrassPoint *two_B = ecc_weierstrass_double(B);
    WeierstrassPoint *k_B = ecc_weierstrass_point_copy(B);
    WeierstrassPoint *kplus1_B = ecc_weierstrass_point_copy(two_B);

    /*
     * This multiply routine more or less follows the shape of the
     * 'Montgomery ladder' technique that you have to use under the
     * extra constraint on addition in Montgomery curves, because it
     * was fresh in my mind and easier to just do it the same way. See
     * the comment in ecc_montgomery_multiply.
     */

    unsigned not_started_yet = 1;
    for (size_t bitindex = mp_max_bits(n); bitindex-- > 0 ;) {
        unsigned nbit = mp_get_bit(n, bitindex);

        WeierstrassPoint *sum = ecc_weierstrass_add(k_B, kplus1_B);
        ecc_weierstrass_cond_swap(k_B, kplus1_B, nbit);
        WeierstrassPoint *other = ecc_weierstrass_double(k_B);
        ecc_weierstrass_point_free(k_B);
        ecc_weierstrass_point_free(kplus1_B);
        k_B = other;
        kplus1_B = sum;
        ecc_weierstrass_cond_swap(k_B, kplus1_B, nbit);

        ecc_weierstrass_cond_overwrite(k_B, B, not_started_yet);
        ecc_weierstrass_cond_overwrite(kplus1_B, two_B, not_started_yet);
        not_started_yet &= ~nbit;
    }

    ecc_weierstrass_point_free(two_B);
    ecc_weierstrass_point_free(kplus1_B);
    return k_B;
}

unsigned ecc_weierstrass_is_identity(WeierstrassPoint *wp)
{
    return mp_eq_integer(wp->Z, 0);
}

/*
 * Normalise a point by scaling its Jacobian coordinates so that Z=1.
 * This doesn't change what point is represented by the triple, but it
 * means the affine x,y can now be easily recovered from X and Y.
 */
static void ecc_weierstrass_normalise(WeierstrassPoint *wp)
{
    WeierstrassCurve *wc = wp->wc;
    mp_int *zinv = monty_invert(wc->mc, wp->Z);
    mp_int *zinv2 = monty_mul(wc->mc, zinv, zinv);
    mp_int *zinv3 = monty_mul(wc->mc, zinv2, zinv);
    monty_mul_into(wc->mc, wp->X, wp->X, zinv2);
    monty_mul_into(wc->mc, wp->Y, wp->Y, zinv3);
    mp_free(zinv);
    mp_free(zinv2);
    mp_free(zinv3);
    mp_copy_into(wp->Z, monty_identity(wc->mc));
}

void ecc_weierstrass_get_affine(
    WeierstrassPoint *wp, mp_int **x, mp_int **y)
{
    WeierstrassCurve *wc = wp->wc;

    ecc_weierstrass_normalise(wp);

    if (x)
        *x = monty_export(wc->mc, wp->X);
    if (y)
        *y = monty_export(wc->mc, wp->Y);
}

unsigned ecc_weierstrass_point_valid(WeierstrassPoint *P)
{
    WeierstrassCurve *wc = P->wc;

    /*
     * The projective version of the curve equation is
     * Y^2 = X^3 + a X Z^4 + b Z^6
     */
    mp_int *lhs = monty_mul(P->wc->mc, P->Y, P->Y);
    mp_int *x2 = monty_mul(wc->mc, P->X, P->X);
    mp_int *x3 = monty_mul(wc->mc, x2, P->X);
    mp_int *z2 = monty_mul(wc->mc, P->Z, P->Z);
    mp_int *z4 = monty_mul(wc->mc, z2, z2);
    mp_int *az4 = monty_mul(wc->mc, wc->a, z4);
    mp_int *axz4 = monty_mul(wc->mc, az4, P->X);
    mp_int *x3_plus_axz4 = monty_add(wc->mc, x3, axz4);
    mp_int *z6 = monty_mul(wc->mc, z2, z4);
    mp_int *bz6 = monty_mul(wc->mc, wc->b, z6);
    mp_int *rhs = monty_add(wc->mc, x3_plus_axz4, bz6);
    
    unsigned valid = mp_cmp_eq(lhs, rhs);

    mp_free(lhs);
    mp_free(x2);
    mp_free(x3);
    mp_free(z2);
    mp_free(z4);
    mp_free(az4);
    mp_free(axz4);
    mp_free(x3_plus_axz4);
    mp_free(z6);
    mp_free(bz6);
    mp_free(rhs);

    return valid;
}

/* ----------------------------------------------------------------------
 * Montgomery curves.
 */

struct MontgomeryPoint {
    /* XZ coordinates. These represent the affine x coordinate by the
     * relationship x = X/Z. */
    mp_int *X, *Z;

    MontgomeryCurve *mc;
};

struct MontgomeryCurve {
    /* Prime modulus of the finite field. */
    mp_int *p;

    /* Montgomery context for arithmetic mod p. */
    MontyContext *mc;

    /* Parameters of the curve, in Montgomery-multiplication
     * transformed form. */
    mp_int *a, *b;

    /* (a+2)/4, also in Montgomery-multiplication form. */
    mp_int *aplus2over4;
};

MontgomeryCurve *ecc_montgomery_curve(
    mp_int *p, mp_int *a, mp_int *b)
{
    MontgomeryCurve *mc = snew(MontgomeryCurve);
    mc->p = mp_copy(p);
    mc->mc = monty_new(p);
    mc->a = monty_import(mc->mc, a);
    mc->b = monty_import(mc->mc, b);

    mp_int *four = mp_from_integer(4);
    mp_int *fourinverse = mp_invert(four, mc->p);
    mp_int *aplus2 = mp_copy(a);
    mp_add_integer_into(aplus2, aplus2, 2);
    mp_int *aplus2over4 = mp_modmul(aplus2, fourinverse, mc->p);
    mc->aplus2over4 = monty_import(mc->mc, aplus2over4);
    mp_free(four);
    mp_free(fourinverse);
    mp_free(aplus2);
    mp_free(aplus2over4);

    return mc;
}

void ecc_montgomery_curve_free(MontgomeryCurve *mc)
{
    mp_free(mc->p);
    mp_free(mc->a);
    mp_free(mc->b);
    mp_free(mc->aplus2over4);
    monty_free(mc->mc);
    sfree(mc);
}

static MontgomeryPoint *ecc_montgomery_point_new_empty(MontgomeryCurve *mc)
{
    MontgomeryPoint *mp = snew(MontgomeryPoint);
    mp->mc = mc;
    mp->X = mp->Z = NULL;
    return mp;
}

MontgomeryPoint *ecc_montgomery_point_new(MontgomeryCurve *mc, mp_int *x)
{
    MontgomeryPoint *mp = ecc_montgomery_point_new_empty(mc);
    mp->X = monty_import(mc->mc, x);
    mp->Z = mp_copy(monty_identity(mc->mc));
    return mp;
}

void ecc_montgomery_point_copy_into(
    MontgomeryPoint *dest, MontgomeryPoint *src)
{
    mp_copy_into(dest->X, src->X);
    mp_copy_into(dest->Z, src->Z);
}

MontgomeryPoint *ecc_montgomery_point_copy(MontgomeryPoint *orig)
{
    MontgomeryPoint *mp = ecc_montgomery_point_new_empty(orig->mc);
    mp->X = mp_copy(orig->X);
    mp->Z = mp_copy(orig->Z);
    return mp;
}

void ecc_montgomery_point_free(MontgomeryPoint *mp)
{
    mp_free(mp->X);
    mp_free(mp->Z);
    smemclr(mp, sizeof(*mp));
    sfree(mp);
}

static void ecc_montgomery_cond_overwrite(
    MontgomeryPoint *dest, MontgomeryPoint *src, unsigned overwrite)
{
    mp_select_into(dest->X, dest->X, src->X, overwrite);
    mp_select_into(dest->Z, dest->Z, src->Z, overwrite);
}

static void ecc_montgomery_cond_swap(
    MontgomeryPoint *P, MontgomeryPoint *Q, unsigned swap)
{
    mp_cond_swap(P->X, Q->X, swap);
    mp_cond_swap(P->Z, Q->Z, swap);
}

MontgomeryPoint *ecc_montgomery_diff_add(
    MontgomeryPoint *P, MontgomeryPoint *Q, MontgomeryPoint *PminusQ)
{
    MontgomeryCurve *mc = P->mc;
    assert(Q->mc == mc);
    assert(PminusQ->mc == mc);

    /*
     * Differential addition is achieved using the following formula
     * that relates the affine x-coordinates of P, Q, P+Q and P-Q:
     *
     * x(P+Q) x(P-Q) (x(Q)-x(P))^2 = (x(P)x(Q) - 1)^2
     *
     * As with the Weierstrass coordinates, the code below transforms
     * that affine relation into a projective one to avoid having to
     * do a division during the main arithmetic.
     */

    MontgomeryPoint *S = ecc_montgomery_point_new_empty(mc);

    mp_int *Px_m_Pz = monty_sub(mc->mc, P->X, P->Z);
    mp_int *Px_p_Pz = monty_add(mc->mc, P->X, P->Z);
    mp_int *Qx_m_Qz = monty_sub(mc->mc, Q->X, Q->Z);
    mp_int *Qx_p_Qz = monty_add(mc->mc, Q->X, Q->Z);
    mp_int *PmQp = monty_mul(mc->mc, Px_m_Pz, Qx_p_Qz);
    mp_int *PpQm = monty_mul(mc->mc, Px_p_Pz, Qx_m_Qz);
    mp_int *Xpre = monty_add(mc->mc, PmQp, PpQm);
    mp_int *Zpre = monty_sub(mc->mc, PmQp, PpQm);
    mp_int *Xpre2 = monty_mul(mc->mc, Xpre, Xpre);
    mp_int *Zpre2 = monty_mul(mc->mc, Zpre, Zpre);
    S->X = monty_mul(mc->mc, Xpre2, PminusQ->Z);
    S->Z = monty_mul(mc->mc, Zpre2, PminusQ->X);

    mp_free(Px_m_Pz);
    mp_free(Px_p_Pz);
    mp_free(Qx_m_Qz);
    mp_free(Qx_p_Qz);
    mp_free(PmQp);
    mp_free(PpQm);
    mp_free(Xpre);
    mp_free(Zpre);
    mp_free(Xpre2);
    mp_free(Zpre2);

    return S;
}

MontgomeryPoint *ecc_montgomery_double(MontgomeryPoint *P)
{
    MontgomeryCurve *mc = P->mc;
    MontgomeryPoint *D = ecc_montgomery_point_new_empty(mc);

    /*
     * To double a point in affine coordinates, in principle you can
     * use the same technique as for Weierstrass: differentiate the
     * curve equation to get the tangent line at the input point, use
     * that to get an expression for y which you substitute back into
     * the curve equation, and subtract the known two roots (in this
     * case both the same) from the x^2 coefficient of the resulting
     * cubic.
     *
     * In this case, we don't have an input y-coordinate, so you have
     * to do a bit of extra transformation to find a formula that can
     * work without it. The tangent formula is (3x^2 + 2ax + 1)/(2y),
     * and when that appears in the final formula it will be squared -
     * so we can substitute the y^2 in the denominator for the RHS of
     * the curve equation. Put together, that gives
     *
     *   x_out = (x+1)^2 (x-1)^2 / 4(x^3+ax^2+x)
     *
     * and, as usual, the code below transforms that into projective
     * form to avoid the division.
     */

    mp_int *Px_m_Pz = monty_sub(mc->mc, P->X, P->Z);
    mp_int *Px_p_Pz = monty_add(mc->mc, P->X, P->Z);
    mp_int *Px_m_Pz_2 = monty_mul(mc->mc, Px_m_Pz, Px_m_Pz);
    mp_int *Px_p_Pz_2 = monty_mul(mc->mc, Px_p_Pz, Px_p_Pz);
    D->X = monty_mul(mc->mc, Px_m_Pz_2, Px_p_Pz_2);
    mp_int *XZ = monty_mul(mc->mc, P->X, P->Z);
    mp_int *twoXZ = monty_add(mc->mc, XZ, XZ);
    mp_int *fourXZ = monty_add(mc->mc, twoXZ, twoXZ);
    mp_int *fourXZ_scaled = monty_mul(mc->mc, fourXZ, mc->aplus2over4);
    mp_int *Zpre = monty_add(mc->mc, Px_m_Pz_2, fourXZ_scaled);
    D->Z = monty_mul(mc->mc, fourXZ, Zpre);

    mp_free(Px_m_Pz);
    mp_free(Px_p_Pz);
    mp_free(Px_m_Pz_2);
    mp_free(Px_p_Pz_2);
    mp_free(XZ);
    mp_free(twoXZ);
    mp_free(fourXZ);
    mp_free(fourXZ_scaled);
    mp_free(Zpre);

    return D;
}

static void ecc_montgomery_normalise(MontgomeryPoint *mp)
{
    MontgomeryCurve *mc = mp->mc;
    mp_int *zinv = monty_invert(mc->mc, mp->Z);
    monty_mul_into(mc->mc, mp->X, mp->X, zinv);
    mp_free(zinv);
    mp_copy_into(mp->Z, monty_identity(mc->mc));
}

MontgomeryPoint *ecc_montgomery_multiply(MontgomeryPoint *B, mp_int *n)
{
    /*
     * 'Montgomery ladder' technique, to compute an arbitrary integer
     * multiple of B under the constraint that you can only add two
     * unequal points if you also know their difference.
     *
     * The setup is that you maintain two curve points one of which is
     * always the other one plus B. Call them kB and (k+1)B, where k
     * is some integer that evolves as we go along. We begin by
     * doubling the input B, to initialise those points to B and 2B,
     * so that k=1.
     *
     * At each stage, we add kB and (k+1)B together - which we can do
     * under the differential-addition constraint because we know
     * their difference is always just B - to give us (2k+1)B. Then we
     * double one of kB or (k+1)B, and depending on which one we
     * choose, we end up with (2k)B or (2k+2)B. Either way, that
     * differs by B from the other value we've just computed. So in
     * each iteration, we do one diff-add and one doubling, plus a
     * couple of conditional swaps to choose which value we double and
     * which way round we put the output points, and the effect is to
     * replace k with either 2k or 2k+1, which we choose based on the
     * appropriate bit of the desired exponent.
     *
     * This routine doesn't assume we know the exact location of the
     * topmost set bit of the exponent. So to maintain constant time
     * it does an iteration for every _potential_ bit, starting from
     * the top downwards; after each iteration in which we haven't
     * seen a set exponent bit yet, we just overwrite the two points
     * with B and 2B again,
     */

    MontgomeryPoint *two_B = ecc_montgomery_double(B);
    MontgomeryPoint *k_B = ecc_montgomery_point_copy(B);
    MontgomeryPoint *kplus1_B = ecc_montgomery_point_copy(two_B);

    unsigned not_started_yet = 1;
    for (size_t bitindex = mp_max_bits(n); bitindex-- > 0 ;) {
        unsigned nbit = mp_get_bit(n, bitindex);

        MontgomeryPoint *sum = ecc_montgomery_diff_add(k_B, kplus1_B, B);
        ecc_montgomery_cond_swap(k_B, kplus1_B, nbit);
        MontgomeryPoint *other = ecc_montgomery_double(k_B);
        ecc_montgomery_point_free(k_B);
        ecc_montgomery_point_free(kplus1_B);
        k_B = other;
        kplus1_B = sum;
        ecc_montgomery_cond_swap(k_B, kplus1_B, nbit);

        ecc_montgomery_cond_overwrite(k_B, B, not_started_yet);
        ecc_montgomery_cond_overwrite(kplus1_B, two_B, not_started_yet);
        not_started_yet &= ~nbit;
    }

    ecc_montgomery_point_free(two_B);
    ecc_montgomery_point_free(kplus1_B);
    return k_B;
}

void ecc_montgomery_get_affine(MontgomeryPoint *mp, mp_int **x)
{
    MontgomeryCurve *mc = mp->mc;

    ecc_montgomery_normalise(mp);

    if (x)
        *x = monty_export(mc->mc, mp->X);
}

/* ----------------------------------------------------------------------
 * Twisted Edwards curves.
 */

struct EdwardsPoint {
    /*
     * We represent an Edwards curve point in 'extended coordinates'.
     * There's more than one coordinate system going by that name,
     * unfortunately. These ones have the semantics that X,Y,Z are
     * ordinary projective coordinates (so x=X/Z and y=Y/Z), but also,
     * we store the extra value T = xyZ = XY/Z.
     */
    mp_int *X, *Y, *Z, *T;

    EdwardsCurve *ec;
};

struct EdwardsCurve {
    /* Prime modulus of the finite field. */
    mp_int *p;

    /* Montgomery context for arithmetic mod p. */
    MontyContext *mc;

    /* Modsqrt context for point decompression. */
    ModsqrtContext *sc;

    /* Parameters of the curve, in Montgomery-multiplication
     * transformed form. */
    mp_int *d, *a;
};

EdwardsCurve *ecc_edwards_curve(mp_int *p, mp_int *d, mp_int *a,
                                mp_int *nonsquare_mod_p)
{
    EdwardsCurve *ec = snew(EdwardsCurve);
    ec->p = mp_copy(p);
    ec->mc = monty_new(p);
    ec->d = monty_import(ec->mc, d);
    ec->a = monty_import(ec->mc, a);

    if (nonsquare_mod_p)
        ec->sc = modsqrt_new(p, nonsquare_mod_p);
    else
        ec->sc = NULL;

    return ec;
}

void ecc_edwards_curve_free(EdwardsCurve *ec)
{
    mp_free(ec->p);
    mp_free(ec->d);
    mp_free(ec->a);
    monty_free(ec->mc);
    if (ec->sc)
        modsqrt_free(ec->sc);
    sfree(ec);
}

static EdwardsPoint *ecc_edwards_point_new_empty(EdwardsCurve *ec)
{
    EdwardsPoint *ep = snew(EdwardsPoint);
    ep->ec = ec;
    ep->X = ep->Y = ep->Z = ep->T = NULL;
    return ep;
}

static EdwardsPoint *ecc_edwards_point_new_imported(
    EdwardsCurve *ec, mp_int *monty_x, mp_int *monty_y)
{
    EdwardsPoint *ep = ecc_edwards_point_new_empty(ec);
    ep->X = monty_x;
    ep->Y = monty_y;
    ep->T = monty_mul(ec->mc, ep->X, ep->Y);
    ep->Z = mp_copy(monty_identity(ec->mc));
    return ep;
}

EdwardsPoint *ecc_edwards_point_new(
    EdwardsCurve *ec, mp_int *x, mp_int *y)
{
    return ecc_edwards_point_new_imported(
        ec, monty_import(ec->mc, x), monty_import(ec->mc, y));
}

void ecc_edwards_point_copy_into(EdwardsPoint *dest, EdwardsPoint *src)
{
    mp_copy_into(dest->X, src->X);
    mp_copy_into(dest->Y, src->Y);
    mp_copy_into(dest->Z, src->Z);
    mp_copy_into(dest->T, src->T);
}

EdwardsPoint *ecc_edwards_point_copy(EdwardsPoint *orig)
{
    EdwardsPoint *ep = ecc_edwards_point_new_empty(orig->ec);
    ep->X = mp_copy(orig->X);
    ep->Y = mp_copy(orig->Y);
    ep->Z = mp_copy(orig->Z);
    ep->T = mp_copy(orig->T);
    return ep;
}

void ecc_edwards_point_free(EdwardsPoint *ep)
{
    mp_free(ep->X);
    mp_free(ep->Y);
    mp_free(ep->Z);
    mp_free(ep->T);
    smemclr(ep, sizeof(*ep));
    sfree(ep);
}

EdwardsPoint *ecc_edwards_point_new_from_y(
    EdwardsCurve *ec, mp_int *yorig, unsigned desired_x_parity)
{
    assert(ec->sc);

    /*
     * The curve equation is ax^2 + y^2 = 1 + dx^2y^2, which
     * rearranges to x^2(dy^2-a) = y^2-1. So we compute
     * (y^2-1)/(dy^2-a) and take its square root.
     */
    unsigned success;

    mp_int *y = monty_import(ec->mc, yorig);
    mp_int *y2 = monty_mul(ec->mc, y, y);
    mp_int *dy2 = monty_mul(ec->mc, ec->d, y2);
    mp_int *dy2ma = monty_sub(ec->mc, dy2, ec->a);
    mp_int *y2m1 = monty_sub(ec->mc, y2, monty_identity(ec->mc));
    mp_int *recip_denominator = monty_invert(ec->mc, dy2ma);
    mp_int *radicand = monty_mul(ec->mc, y2m1, recip_denominator);
    mp_int *x = monty_modsqrt(ec->sc, radicand, &success);
    mp_free(y2);
    mp_free(dy2);
    mp_free(dy2ma);
    mp_free(y2m1);
    mp_free(recip_denominator);
    mp_free(radicand);

    if (!success) {
        /* Failure! x^2 worked out to be a number that has no square
         * root mod p. In this situation there's no point in trying to
         * be time-constant, since the protocol sequence is going to
         * diverge anyway when we complain to whoever gave us this
         * bogus value. */
        mp_free(x);
        mp_free(y);
        return NULL;
    }

    /*
     * Choose whichever of x and p-x has the specified parity (of its
     * lowest positive residue mod p).
     */
    mp_int *tmp = monty_export(ec->mc, x);
    unsigned flip = (mp_get_bit(tmp, 0) ^ desired_x_parity) & 1;
    mp_sub_into(tmp, ec->p, x);
    mp_select_into(x, x, tmp, flip);
    mp_free(tmp);

    return ecc_edwards_point_new_imported(ec, x, y);
}

static void ecc_edwards_cond_overwrite(
    EdwardsPoint *dest, EdwardsPoint *src, unsigned overwrite)
{
    mp_select_into(dest->X, dest->X, src->X, overwrite);
    mp_select_into(dest->Y, dest->Y, src->Y, overwrite);
    mp_select_into(dest->Z, dest->Z, src->Z, overwrite);
    mp_select_into(dest->T, dest->T, src->T, overwrite);
}

static void ecc_edwards_cond_swap(
    EdwardsPoint *P, EdwardsPoint *Q, unsigned swap)
{
    mp_cond_swap(P->X, Q->X, swap);
    mp_cond_swap(P->Y, Q->Y, swap);
    mp_cond_swap(P->Z, Q->Z, swap);
    mp_cond_swap(P->T, Q->T, swap);
}

EdwardsPoint *ecc_edwards_add(EdwardsPoint *P, EdwardsPoint *Q)
{
    EdwardsCurve *ec = P->ec;
    assert(Q->ec == ec);

    EdwardsPoint *S = ecc_edwards_point_new_empty(ec);

    /*
     * The affine rule for Edwards addition of (x1,y1) and (x2,y2) is
     *
     *   x_out = (x1 y2 +   y1 x2) / (1 + d x1 x2 y1 y2)
     *   y_out = (y1 y2 - a x1 x2) / (1 - d x1 x2 y1 y2)
     *
     * The formulae below are listed as 'add-2008-hwcd' in
     * https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html
     *
     * and if you undo the careful optimisation to find out what
     * they're actually computing, it comes out to
     *
     *   X_out = (X1 Y2 +   Y1 X2) (Z1 Z2 - d T1 T2)
     *   Y_out = (Y1 Y2 - a X1 X2) (Z1 Z2 + d T1 T2)
     *   Z_out = (Z1 Z2 - d T1 T2) (Z1 Z2 + d T1 T2)
     *   T_out = (X1 Y2 +   Y1 X2) (Y1 Y2 - a X1 X2)
     */
    mp_int *PxQx = monty_mul(ec->mc, P->X, Q->X);
    mp_int *PyQy = monty_mul(ec->mc, P->Y, Q->Y);
    mp_int *PtQt = monty_mul(ec->mc, P->T, Q->T);
    mp_int *PzQz = monty_mul(ec->mc, P->Z, Q->Z);
    mp_int *Psum = monty_add(ec->mc, P->X, P->Y);
    mp_int *Qsum = monty_add(ec->mc, Q->X, Q->Y);
    mp_int *aPxQx = monty_mul(ec->mc, ec->a, PxQx);
    mp_int *dPtQt = monty_mul(ec->mc, ec->d, PtQt);
    mp_int *sumprod = monty_mul(ec->mc, Psum, Qsum);
    mp_int *xx_p_yy = monty_add(ec->mc, PxQx, PyQy);
    mp_int *E = monty_sub(ec->mc, sumprod, xx_p_yy);
    mp_int *F = monty_sub(ec->mc, PzQz, dPtQt);
    mp_int *G = monty_add(ec->mc, PzQz, dPtQt);
    mp_int *H = monty_sub(ec->mc, PyQy, aPxQx);
    S->X = monty_mul(ec->mc, E, F);
    S->Z = monty_mul(ec->mc, F, G);
    S->Y = monty_mul(ec->mc, G, H);
    S->T = monty_mul(ec->mc, H, E);

    mp_free(PxQx);
    mp_free(PyQy);
    mp_free(PtQt);
    mp_free(PzQz);
    mp_free(Psum);
    mp_free(Qsum);
    mp_free(aPxQx);
    mp_free(dPtQt);
    mp_free(sumprod);
    mp_free(xx_p_yy);
    mp_free(E);
    mp_free(F);
    mp_free(G);
    mp_free(H);

    return S;
}

static void ecc_edwards_normalise(EdwardsPoint *ep)
{
    EdwardsCurve *ec = ep->ec;
    mp_int *zinv = monty_invert(ec->mc, ep->Z);
    monty_mul_into(ec->mc, ep->X, ep->X, zinv);
    monty_mul_into(ec->mc, ep->Y, ep->Y, zinv);
    mp_free(zinv);
    mp_copy_into(ep->Z, monty_identity(ec->mc));
    monty_mul_into(ec->mc, ep->T, ep->X, ep->Y);
}

EdwardsPoint *ecc_edwards_multiply(EdwardsPoint *B, mp_int *n)
{
    EdwardsPoint *two_B = ecc_edwards_add(B, B);
    EdwardsPoint *k_B = ecc_edwards_point_copy(B);
    EdwardsPoint *kplus1_B = ecc_edwards_point_copy(two_B);

    /*
     * Another copy of the same exponentiation routine following the
     * pattern of the Montgomery ladder, because it works as well as
     * any other technique and this way I didn't have to debug two of
     * them.
     */

    unsigned not_started_yet = 1;
    for (size_t bitindex = mp_max_bits(n); bitindex-- > 0 ;) {
        unsigned nbit = mp_get_bit(n, bitindex);

        EdwardsPoint *sum = ecc_edwards_add(k_B, kplus1_B);
        ecc_edwards_cond_swap(k_B, kplus1_B, nbit);
        EdwardsPoint *other = ecc_edwards_add(k_B, k_B);
        ecc_edwards_point_free(k_B);
        ecc_edwards_point_free(kplus1_B);
        k_B = other;
        kplus1_B = sum;
        ecc_edwards_cond_swap(k_B, kplus1_B, nbit);

        ecc_edwards_cond_overwrite(k_B, B, not_started_yet);
        ecc_edwards_cond_overwrite(kplus1_B, two_B, not_started_yet);
        not_started_yet &= ~nbit;
    }

    ecc_edwards_point_free(two_B);
    ecc_edwards_point_free(kplus1_B);
    return k_B;
}

/*
 * Helper routine to determine whether two values each given as a pair
 * of projective coordinates represent the same affine value.
 */
static inline unsigned projective_eq(
    MontyContext *mc, mp_int *An, mp_int *Ad,
    mp_int *Bn, mp_int *Bd)
{
    mp_int *AnBd = monty_mul(mc, An, Bd);
    mp_int *BnAd = monty_mul(mc, Bn, Ad);
    unsigned toret = mp_cmp_eq(AnBd, BnAd);
    mp_free(AnBd);
    mp_free(BnAd);
    return toret;
}

unsigned ecc_edwards_eq(EdwardsPoint *P, EdwardsPoint *Q)
{
    EdwardsCurve *ec = P->ec;
    assert(Q->ec == ec);

    return (projective_eq(ec->mc, P->X, P->Z, Q->X, Q->Z) &
            projective_eq(ec->mc, P->Y, P->Z, Q->Y, Q->Z));
}

void ecc_edwards_get_affine(EdwardsPoint *ep, mp_int **x, mp_int **y)
{
    EdwardsCurve *ec = ep->ec;

    ecc_edwards_normalise(ep);

    if (x)
        *x = monty_export(ec->mc, ep->X);
    if (y)
        *y = monty_export(ec->mc, ep->Y);
}
