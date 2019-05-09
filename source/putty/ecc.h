#ifndef PUTTY_ECC_H
#define PUTTY_ECC_H

/*
 * Arithmetic functions for the various kinds of elliptic curves used
 * by PuTTY's public-key cryptography.
 *
 * All of these elliptic curves are over the finite field whose order
 * is a large prime p. (Elliptic curves over a field of order 2^n are
 * also known, but PuTTY currently has no need of them.)
 */

/* ----------------------------------------------------------------------
 * Weierstrass curves (or rather, 'short form' Weierstrass curves).
 *
 * A curve in this form is defined by two parameters a,b, and the
 * non-identity points on the curve are represented by (x,y) (the
 * 'affine coordinates') such that y^2 = x^3 + ax + b.
 *
 * The identity element of the curve's group is an additional 'point
 * at infinity', which is considered to be the third point on the
 * intersection of the curve with any vertical line. Hence, the
 * inverse of the point (x,y) is (x,-y).
 */

/*
 * Create and destroy Weierstrass curve data structures. The mandatory
 * parameters to the constructor are the prime modulus p, and the
 * curve parameters a,b.
 *
 * 'nonsquare_mod_p' is an optional extra parameter, only needed by
 * ecc_edwards_point_new_from_y which has to take a modular square
 * root. You can pass it as NULL if you don't need that function.
 */
WeierstrassCurve *ecc_weierstrass_curve(
    mp_int *p, mp_int *a, mp_int *b, mp_int *nonsquare_mod_p);
void ecc_weierstrass_curve_free(WeierstrassCurve *);

/*
 * Create points on a Weierstrass curve, given the curve.
 *
 * point_new_identity returns the special identity point.
 * point_new(x,y) returns the non-identity point with the given affine
 * coordinates.
 *
 * point_new_from_x constructs a non-identity point given only the
 * x-coordinate, by using the curve equation to work out what y has to
 * be. Of course the equation only tells you y^2, so it only
 * determines y up to sign; the parameter desired_y_parity controls
 * which of the two values of y you get, by saying whether you'd like
 * its minimal non-negative residue mod p to be even or odd. (Of
 * course, since p itself is odd, exactly one of y and p-y is odd.)
 * This function has to take a modular square root, so it will only
 * work if you passed in a non-square mod p when constructing the
 * curve.
 */
WeierstrassPoint *ecc_weierstrass_point_new_identity(WeierstrassCurve *curve);
WeierstrassPoint *ecc_weierstrass_point_new(
    WeierstrassCurve *curve, mp_int *x, mp_int *y);
WeierstrassPoint *ecc_weierstrass_point_new_from_x(
    WeierstrassCurve *curve, mp_int *x, unsigned desired_y_parity);

/* Memory management: copy and free points. */
void ecc_weierstrass_point_copy_into(
    WeierstrassPoint *dest, WeierstrassPoint *src);
WeierstrassPoint *ecc_weierstrass_point_copy(WeierstrassPoint *wc);
void ecc_weierstrass_point_free(WeierstrassPoint *point);

/* Check whether a point is actually on the curve. */
unsigned ecc_weierstrass_point_valid(WeierstrassPoint *);

/*
 * Add two points and return their sum. This function is fully
 * general: it should do the right thing if the two inputs are the
 * same, or if either (or both) of the input points is the identity,
 * or if the two input points are inverses so the output is the
 * identity. However, it pays for that generality by being slower than
 * the special-purpose functions below..
 */
WeierstrassPoint *ecc_weierstrass_add_general(
    WeierstrassPoint *, WeierstrassPoint *);

/*
 * Fast but less general arithmetic functions: add two points on the
 * condition that they are not equal and neither is the identity, and
 * add a point to itself.
 */
WeierstrassPoint *ecc_weierstrass_add(WeierstrassPoint *, WeierstrassPoint *);
WeierstrassPoint *ecc_weierstrass_double(WeierstrassPoint *);

/*
 * Compute an integer multiple of a point. Not guaranteed to work
 * unless the integer argument is less than the order of the point in
 * the group (because it won't cope if an identity element shows up in
 * any intermediate product).
 */
WeierstrassPoint *ecc_weierstrass_multiply(WeierstrassPoint *, mp_int *);

/*
 * Query functions to get the value of a point back out. is_identity
 * tells you whether the point is the identity; if it isn't, then
 * get_affine will retrieve one or both of its affine coordinates.
 * (You can pass NULL as either output pointer, if you don't need that
 * coordinate as output.)
 */
unsigned ecc_weierstrass_is_identity(WeierstrassPoint *wp);
void ecc_weierstrass_get_affine(WeierstrassPoint *wp, mp_int **x, mp_int **y);

/* ----------------------------------------------------------------------
 * Montgomery curves.
 *
 * A curve in this form is defined by two parameters a,b, and the
 * curve equation is by^2 = x^3 + ax^2 + x.
 *
 * As with Weierstrass curves, there's an additional point at infinity
 * that is the identity element, and the inverse of (x,y) is (x,-y).
 *
 * However, we don't actually work with full (x,y) pairs. We just
 * store the x-coordinate (so what we're really representing is not a
 * specific point on the curve but a two-point set {P,-P}). This means
 * you can't quite do point addition, because if you're given {P,-P}
 * and {Q,-Q} as input, you can work out a pair of x-coordinates that
 * are those of P-Q and P+Q, but you don't know which is which.
 *
 * Instead, the basic operation is 'differential addition', in which
 * you are given three parameters P, Q and P-Q and you return P+Q. (As
 * well as disambiguating which of the possible answers you want, that
 * extra input also enables a fast formulae for computing it. This
 * fast formula is more or less why Montgomery curves are useful in
 * the first place.)
 *
 * Doubling a point is still possible to do unambiguously, so you can
 * still compute an integer multiple of P if you start by making 2P
 * and then doing a series of differential additions.
 */

/*
 * Create and destroy Montgomery curve data structures.
 */
MontgomeryCurve *ecc_montgomery_curve(mp_int *p, mp_int *a, mp_int *b);
void ecc_montgomery_curve_free(MontgomeryCurve *);

/*
 * Create, copy and free points on the curve. We don't need to
 * explicitly represent the identity for this application.
 */
MontgomeryPoint *ecc_montgomery_point_new(MontgomeryCurve *mc, mp_int *x);
void ecc_montgomery_point_copy_into(
    MontgomeryPoint *dest, MontgomeryPoint *src);
MontgomeryPoint *ecc_montgomery_point_copy(MontgomeryPoint *orig);
void ecc_montgomery_point_free(MontgomeryPoint *mp);

/*
 * Basic arithmetic routines: differential addition and point-
 * doubling. Each of these assumes that no special cases come up - no
 * input or output point should be the identity, and in diff_add, P
 * and Q shouldn't be the same.
 */
MontgomeryPoint *ecc_montgomery_diff_add(
    MontgomeryPoint *P, MontgomeryPoint *Q, MontgomeryPoint *PminusQ);
MontgomeryPoint *ecc_montgomery_double(MontgomeryPoint *P);

/*
 * Compute an integer multiple of a point.
 */
MontgomeryPoint *ecc_montgomery_multiply(MontgomeryPoint *, mp_int *);

/*
 * Return the affine x-coordinate of a point.
 */
void ecc_montgomery_get_affine(MontgomeryPoint *mp, mp_int **x);

/* ----------------------------------------------------------------------
 * Twisted Edwards curves.
 *
 * A curve in this form is defined by two parameters d,a, and the
 * curve equation is a x^2 + y^2 = 1 + d x^2 y^2.
 *
 * Apparently if you ask a proper algebraic geometer they'll tell you
 * that this is technically not an actual elliptic curve. Certainly it
 * doesn't work quite the same way as the other kinds: in this form,
 * there is no need for a point at infinity, because the identity
 * element is represented by the affine coordinates (0,1). And you
 * invert a point by negating its x rather than y coordinate: the
 * inverse of (x,y) is (-x,y).
 *
 * The usefulness of this representation is that the addition formula
 * is 'strongly unified', meaning that the same formula works for any
 * input and output points, without needing special cases for the
 * identity or for doubling.
 */

/*
 * Create and destroy Edwards curve data structures.
 *
 * Similarly to ecc_weierstrass_curve, you don't have to provide
 * nonsquare_mod_p if you don't need ecc_edwards_point_new_from_y.
 */
EdwardsCurve *ecc_edwards_curve(
    mp_int *p, mp_int *d, mp_int *a, mp_int *nonsquare_mod_p);
void ecc_edwards_curve_free(EdwardsCurve *);

/*
 * Create points.
 *
 * There's no need to have a separate function to create the identity
 * point, because you can just pass x=0 and y=1 to the usual function.
 *
 * Similarly to the Weierstrass curve, ecc_edwards_point_new_from_y
 * creates a point given only its y-coordinate and the desired parity
 * of its x-coordinate, and you can only call it if you provided the
 * optional nonsquare_mod_p argument when creating the curve.
 */
EdwardsPoint *ecc_edwards_point_new(
    EdwardsCurve *curve, mp_int *x, mp_int *y);
EdwardsPoint *ecc_edwards_point_new_from_y(
    EdwardsCurve *curve, mp_int *y, unsigned desired_x_parity);

/* Copy and free points. */
void ecc_edwards_point_copy_into(EdwardsPoint *dest, EdwardsPoint *src);
EdwardsPoint *ecc_edwards_point_copy(EdwardsPoint *ec);
void ecc_edwards_point_free(EdwardsPoint *point);

/*
 * Arithmetic: add two points, and calculate an integer multiple of a
 * point.
 */
EdwardsPoint *ecc_edwards_add(EdwardsPoint *, EdwardsPoint *);
EdwardsPoint *ecc_edwards_multiply(EdwardsPoint *, mp_int *);

/*
 * Query functions: compare two points for equality, and return the
 * affine coordinates of a point.
 */
unsigned ecc_edwards_eq(EdwardsPoint *, EdwardsPoint *);
void ecc_edwards_get_affine(EdwardsPoint *wp, mp_int **x, mp_int **y);

#endif /* PUTTY_ECC_H */
