#include "ssh.h"

const struct ssh_kex ssh_diffiehellman = {
    "diffie-hellman-group1-sha1"
};

const struct ssh_kex ssh_diffiehellman_gex = {
    "diffie-hellman-group-exchange-sha1"
};

/*
 * The prime p used in the key exchange. 
 */
static unsigned char P[] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC9, 0x0F, 0xDA, 0xA2,
    0x21, 0x68, 0xC2, 0x34, 0xC4, 0xC6, 0x62, 0x8B, 0x80, 0xDC, 0x1C, 0xD1,
    0x29, 0x02, 0x4E, 0x08, 0x8A, 0x67, 0xCC, 0x74, 0x02, 0x0B, 0xBE, 0xA6,
    0x3B, 0x13, 0x9B, 0x22, 0x51, 0x4A, 0x08, 0x79, 0x8E, 0x34, 0x04, 0xDD,
    0xEF, 0x95, 0x19, 0xB3, 0xCD, 0x3A, 0x43, 0x1B, 0x30, 0x2B, 0x0A, 0x6D,
    0xF2, 0x5F, 0x14, 0x37, 0x4F, 0xE1, 0x35, 0x6D, 0x6D, 0x51, 0xC2, 0x45,
    0xE4, 0x85, 0xB5, 0x76, 0x62, 0x5E, 0x7E, 0xC6, 0xF4, 0x4C, 0x42, 0xE9,
    0xA6, 0x37, 0xED, 0x6B, 0x0B, 0xFF, 0x5C, 0xB6, 0xF4, 0x06, 0xB7, 0xED,
    0xEE, 0x38, 0x6B, 0xFB, 0x5A, 0x89, 0x9F, 0xA5, 0xAE, 0x9F, 0x24, 0x11,
    0x7C, 0x4B, 0x1F, 0xE6, 0x49, 0x28, 0x66, 0x51, 0xEC, 0xE6, 0x53, 0x81,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

/*
 * The generator g = 2.
 */
static unsigned char G[] = { 2 };

/*
 * Variables.
 */
static Bignum x, e, p, q, qmask, g;

/*
 * Common DH initialisation.
 */
static void dh_init(void)
{
    q = bignum_rshift(p, 1);
    qmask = bignum_bitmask(q);
}

/*
 * Initialise DH for the standard group1.
 */
void dh_setup_group1(void)
{
    p = bignum_from_bytes(P, sizeof(P));
    g = bignum_from_bytes(G, sizeof(G));
    dh_init();
}

/*
 * Initialise DH for an alternative group.
 */
void dh_setup_group(Bignum pval, Bignum gval)
{
    p = copybn(pval);
    g = copybn(gval);
    dh_init();
}

/*
 * Clean up.
 */
void dh_cleanup(void)
{
    freebn(p);
    freebn(g);
    freebn(q);
    freebn(qmask);
}

/*
 * DH stage 1: invent a number x between 1 and q, and compute e =
 * g^x mod p. Return e.
 * 
 * If `nbits' is greater than zero, it is used as an upper limit
 * for the number of bits in x. This is safe provided that (a) you
 * use twice as many bits in x as the number of bits you expect to
 * use in your session key, and (b) the DH group is a safe prime
 * (which SSH demands that it must be).
 * 
 * P. C. van Oorschot, M. J. Wiener
 * "On Diffie-Hellman Key Agreement with Short Exponents".
 * Advances in Cryptology: Proceedings of Eurocrypt '96
 * Springer-Verlag, May 1996.
 */
Bignum dh_create_e(int nbits)
{
    int i;

    int nbytes;
    unsigned char *buf;

    nbytes = ssh1_bignum_length(qmask);
    buf = smalloc(nbytes);

    do {
	/*
	 * Create a potential x, by ANDing a string of random bytes
	 * with qmask.
	 */
	if (x)
	    freebn(x);
	if (nbits == 0 || nbits > bignum_bitcount(qmask)) {
	    ssh1_write_bignum(buf, qmask);
	    for (i = 2; i < nbytes; i++)
		buf[i] &= random_byte();
	    ssh1_read_bignum(buf, &x);
	} else {
	    int b, nb;
	    x = bn_power_2(nbits);
	    b = nb = 0;
	    for (i = 0; i < nbits; i++) {
		if (nb == 0) {
		    nb = 8;
		    b = random_byte();
		}
		bignum_set_bit(x, i, b & 1);
		b >>= 1;
		nb--;
	    }
	}
    } while (bignum_cmp(x, One) <= 0 || bignum_cmp(x, q) >= 0);

    /*
     * Done. Now compute e = g^x mod p.
     */
    e = modpow(g, x, p);

    return e;
}

/*
 * DH stage 2: given a number f, compute K = f^x mod p.
 */
Bignum dh_find_K(Bignum f)
{
    Bignum ret;
    ret = modpow(f, x, p);
    return ret;
}
