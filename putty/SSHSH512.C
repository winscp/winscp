/*
 * SHA-512 algorithm as described at
 * 
 *   http://csrc.nist.gov/cryptval/shs.html
 */

#include "ssh.h"

#define BLKSIZE 128

/*
 * Arithmetic implementations. Note that AND, XOR and NOT can
 * overlap destination with one source, but the others can't.
 */
#define add(r,x,y) ( r.lo = y.lo + x.lo, \
		     r.hi = y.hi + x.hi + ((uint32)r.lo < (uint32)y.lo) )
#define rorB(r,x,y) ( r.lo = ((uint32)x.hi >> ((y)-32)) | ((uint32)x.lo << (64-(y))), \
		      r.hi = ((uint32)x.lo >> ((y)-32)) | ((uint32)x.hi << (64-(y))) )
#define rorL(r,x,y) ( r.lo = ((uint32)x.lo >> (y)) | ((uint32)x.hi << (32-(y))), \
		      r.hi = ((uint32)x.hi >> (y)) | ((uint32)x.lo << (32-(y))) )
#define shrB(r,x,y) ( r.lo = (uint32)x.hi >> ((y)-32), r.hi = 0 )
#define shrL(r,x,y) ( r.lo = ((uint32)x.lo >> (y)) | ((uint32)x.hi << (32-(y))), \
		      r.hi = (uint32)x.hi >> (y) )
#define and(r,x,y) ( r.lo = x.lo & y.lo, r.hi = x.hi & y.hi )
#define xor(r,x,y) ( r.lo = x.lo ^ y.lo, r.hi = x.hi ^ y.hi )
#define not(r,x) ( r.lo = ~x.lo, r.hi = ~x.hi )
#define INIT(h,l) { h, l }
#define BUILD(r,h,l) ( r.hi = h, r.lo = l )
#define EXTRACT(h,l,r) ( h = r.hi, l = r.lo )

/* ----------------------------------------------------------------------
 * Core SHA512 algorithm: processes 16-doubleword blocks into a
 * message digest.
 */

#define Ch(r,t,x,y,z) ( not(t,x), and(r,t,z), and(t,x,y), xor(r,r,t) )
#define Maj(r,t,x,y,z) ( and(r,x,y), and(t,x,z), xor(r,r,t), \
			 and(t,y,z), xor(r,r,t) )
#define bigsigma0(r,t,x) ( rorL(r,x,28), rorB(t,x,34), xor(r,r,t), \
			   rorB(t,x,39), xor(r,r,t) )
#define bigsigma1(r,t,x) ( rorL(r,x,14), rorL(t,x,18), xor(r,r,t), \
			   rorB(t,x,41), xor(r,r,t) )
#define smallsigma0(r,t,x) ( rorL(r,x,1), rorL(t,x,8), xor(r,r,t), \
			     shrL(t,x,7), xor(r,r,t) )
#define smallsigma1(r,t,x) ( rorL(r,x,19), rorB(t,x,61), xor(r,r,t), \
			     shrL(t,x,6), xor(r,r,t) )

static void SHA512_Core_Init(SHA512_State *s) {
    static const uint64 iv[] = {
	INIT(0x6a09e667, 0xf3bcc908),
	INIT(0xbb67ae85, 0x84caa73b),
	INIT(0x3c6ef372, 0xfe94f82b),
	INIT(0xa54ff53a, 0x5f1d36f1),
	INIT(0x510e527f, 0xade682d1),
	INIT(0x9b05688c, 0x2b3e6c1f),
	INIT(0x1f83d9ab, 0xfb41bd6b),
	INIT(0x5be0cd19, 0x137e2179),
    };
    int i;
    for (i = 0; i < 8; i++)
	s->h[i] = iv[i];
}

static void SHA512_Block(SHA512_State *s, uint64 *block) {
    uint64 w[80];
    uint64 a,b,c,d,e,f,g,h;
    static const uint64 k[] = {
	INIT(0x428a2f98, 0xd728ae22), INIT(0x71374491, 0x23ef65cd),
	INIT(0xb5c0fbcf, 0xec4d3b2f), INIT(0xe9b5dba5, 0x8189dbbc),
	INIT(0x3956c25b, 0xf348b538), INIT(0x59f111f1, 0xb605d019),
	INIT(0x923f82a4, 0xaf194f9b), INIT(0xab1c5ed5, 0xda6d8118),
	INIT(0xd807aa98, 0xa3030242), INIT(0x12835b01, 0x45706fbe),
	INIT(0x243185be, 0x4ee4b28c), INIT(0x550c7dc3, 0xd5ffb4e2),
	INIT(0x72be5d74, 0xf27b896f), INIT(0x80deb1fe, 0x3b1696b1),
	INIT(0x9bdc06a7, 0x25c71235), INIT(0xc19bf174, 0xcf692694),
	INIT(0xe49b69c1, 0x9ef14ad2), INIT(0xefbe4786, 0x384f25e3),
	INIT(0x0fc19dc6, 0x8b8cd5b5), INIT(0x240ca1cc, 0x77ac9c65),
	INIT(0x2de92c6f, 0x592b0275), INIT(0x4a7484aa, 0x6ea6e483),
	INIT(0x5cb0a9dc, 0xbd41fbd4), INIT(0x76f988da, 0x831153b5),
	INIT(0x983e5152, 0xee66dfab), INIT(0xa831c66d, 0x2db43210),
	INIT(0xb00327c8, 0x98fb213f), INIT(0xbf597fc7, 0xbeef0ee4),
	INIT(0xc6e00bf3, 0x3da88fc2), INIT(0xd5a79147, 0x930aa725),
	INIT(0x06ca6351, 0xe003826f), INIT(0x14292967, 0x0a0e6e70),
	INIT(0x27b70a85, 0x46d22ffc), INIT(0x2e1b2138, 0x5c26c926),
	INIT(0x4d2c6dfc, 0x5ac42aed), INIT(0x53380d13, 0x9d95b3df),
	INIT(0x650a7354, 0x8baf63de), INIT(0x766a0abb, 0x3c77b2a8),
	INIT(0x81c2c92e, 0x47edaee6), INIT(0x92722c85, 0x1482353b),
	INIT(0xa2bfe8a1, 0x4cf10364), INIT(0xa81a664b, 0xbc423001),
	INIT(0xc24b8b70, 0xd0f89791), INIT(0xc76c51a3, 0x0654be30),
	INIT(0xd192e819, 0xd6ef5218), INIT(0xd6990624, 0x5565a910),
	INIT(0xf40e3585, 0x5771202a), INIT(0x106aa070, 0x32bbd1b8),
	INIT(0x19a4c116, 0xb8d2d0c8), INIT(0x1e376c08, 0x5141ab53),
	INIT(0x2748774c, 0xdf8eeb99), INIT(0x34b0bcb5, 0xe19b48a8),
	INIT(0x391c0cb3, 0xc5c95a63), INIT(0x4ed8aa4a, 0xe3418acb),
	INIT(0x5b9cca4f, 0x7763e373), INIT(0x682e6ff3, 0xd6b2b8a3),
	INIT(0x748f82ee, 0x5defb2fc), INIT(0x78a5636f, 0x43172f60),
	INIT(0x84c87814, 0xa1f0ab72), INIT(0x8cc70208, 0x1a6439ec),
	INIT(0x90befffa, 0x23631e28), INIT(0xa4506ceb, 0xde82bde9),
	INIT(0xbef9a3f7, 0xb2c67915), INIT(0xc67178f2, 0xe372532b),
	INIT(0xca273ece, 0xea26619c), INIT(0xd186b8c7, 0x21c0c207),
	INIT(0xeada7dd6, 0xcde0eb1e), INIT(0xf57d4f7f, 0xee6ed178),
	INIT(0x06f067aa, 0x72176fba), INIT(0x0a637dc5, 0xa2c898a6),
	INIT(0x113f9804, 0xbef90dae), INIT(0x1b710b35, 0x131c471b),
	INIT(0x28db77f5, 0x23047d84), INIT(0x32caab7b, 0x40c72493),
	INIT(0x3c9ebe0a, 0x15c9bebc), INIT(0x431d67c4, 0x9c100d4c),
	INIT(0x4cc5d4be, 0xcb3e42b6), INIT(0x597f299c, 0xfc657e2a),
	INIT(0x5fcb6fab, 0x3ad6faec), INIT(0x6c44198c, 0x4a475817),
    };

    int t;

    for (t = 0; t < 16; t++)
        w[t] = block[t];

    for (t = 16; t < 80; t++) {
	uint64 p, q, r, tmp;
	smallsigma1(p, tmp, w[t-2]);
	smallsigma0(q, tmp, w[t-15]);
	add(r, p, q);
	add(p, r, w[t-7]);
	add(w[t], p, w[t-16]);
    }

    a = s->h[0]; b = s->h[1]; c = s->h[2]; d = s->h[3];
    e = s->h[4]; f = s->h[5]; g = s->h[6]; h = s->h[7];

    for (t = 0; t < 80; t+=8) {
        uint64 tmp, p, q, r;

#define ROUND(j,a,b,c,d,e,f,g,h) \
	bigsigma1(p, tmp, e); \
	Ch(q, tmp, e, f, g); \
	add(r, p, q); \
	add(p, r, k[j]) ; \
	add(q, p, w[j]); \
	add(r, q, h); \
	bigsigma0(p, tmp, a); \
	Maj(tmp, q, a, b, c); \
	add(q, tmp, p); \
	add(p, r, d); \
	d = p; \
	add(h, q, r);

	ROUND(t+0, a,b,c,d,e,f,g,h);
	ROUND(t+1, h,a,b,c,d,e,f,g);
	ROUND(t+2, g,h,a,b,c,d,e,f);
	ROUND(t+3, f,g,h,a,b,c,d,e);
	ROUND(t+4, e,f,g,h,a,b,c,d);
	ROUND(t+5, d,e,f,g,h,a,b,c);
	ROUND(t+6, c,d,e,f,g,h,a,b);
	ROUND(t+7, b,c,d,e,f,g,h,a);
    }

    {
	uint64 tmp;
#define UPDATE(state, local) ( tmp = state, add(state, tmp, local) )
	UPDATE(s->h[0], a); UPDATE(s->h[1], b);
	UPDATE(s->h[2], c); UPDATE(s->h[3], d);
	UPDATE(s->h[4], e); UPDATE(s->h[5], f);
	UPDATE(s->h[6], g); UPDATE(s->h[7], h);
    }
}

/* ----------------------------------------------------------------------
 * Outer SHA512 algorithm: take an arbitrary length byte string,
 * convert it into 16-doubleword blocks with the prescribed padding
 * at the end, and pass those blocks to the core SHA512 algorithm.
 */

void SHA512_Init(SHA512_State *s) {
    int i;
    SHA512_Core_Init(s);
    s->blkused = 0;
    for (i = 0; i < 4; i++)
	s->len[i] = 0;
}

void SHA512_Bytes(SHA512_State *s, const void *p, int len) {
    unsigned char *q = (unsigned char *)p;
    uint64 wordblock[16];
    uint32 lenw = len;
    int i;

    /*
     * Update the length field.
     */
    for (i = 0; i < 4; i++) {
	s->len[i] += lenw;
	lenw = (s->len[i] < lenw);
    }

    if (s->blkused && s->blkused+len < BLKSIZE) {
        /*
         * Trivial case: just add to the block.
         */
        memcpy(s->block + s->blkused, q, len);
        s->blkused += len;
    } else {
        /*
         * We must complete and process at least one block.
         */
        while (s->blkused + len >= BLKSIZE) {
            memcpy(s->block + s->blkused, q, BLKSIZE - s->blkused);
            q += BLKSIZE - s->blkused;
            len -= BLKSIZE - s->blkused;
            /* Now process the block. Gather bytes big-endian into words */
            for (i = 0; i < 16; i++) {
		uint32 h, l;
                h = ( ((uint32)s->block[i*8+0]) << 24 ) |
                    ( ((uint32)s->block[i*8+1]) << 16 ) |
                    ( ((uint32)s->block[i*8+2]) <<  8 ) |
                    ( ((uint32)s->block[i*8+3]) <<  0 );
                l = ( ((uint32)s->block[i*8+4]) << 24 ) |
                    ( ((uint32)s->block[i*8+5]) << 16 ) |
                    ( ((uint32)s->block[i*8+6]) <<  8 ) |
                    ( ((uint32)s->block[i*8+7]) <<  0 );
		BUILD(wordblock[i], h, l);
            }
            SHA512_Block(s, wordblock);
            s->blkused = 0;
        }
        memcpy(s->block, q, len);
        s->blkused = len;
    }
}

void SHA512_Final(SHA512_State *s, unsigned char *digest) {
    int i;
    int pad;
    unsigned char c[BLKSIZE];
    uint32 len[4];

    if (s->blkused >= BLKSIZE-16)
        pad = (BLKSIZE-16) + BLKSIZE - s->blkused;
    else
        pad = (BLKSIZE-16) - s->blkused;

    for (i = 4; i-- ;) {
	uint32 lenhi = s->len[i];
	uint32 lenlo = i > 0 ? s->len[i-1] : 0;
	len[i] = (lenhi << 3) | (lenlo >> (32-3));
    }

    memset(c, 0, pad);
    c[0] = 0x80;
    SHA512_Bytes(s, &c, pad);

    for (i = 0; i < 4; i++) {
	c[i*4+0] = (len[3-i] >> 24) & 0xFF;
	c[i*4+1] = (len[3-i] >> 16) & 0xFF;
	c[i*4+2] = (len[3-i] >>  8) & 0xFF;
	c[i*4+3] = (len[3-i] >>  0) & 0xFF;
    }

    SHA512_Bytes(s, &c, 16);

    for (i = 0; i < 8; i++) {
	uint32 h, l;
	EXTRACT(h, l, s->h[i]);
	digest[i*8+0] = (h >> 24) & 0xFF;
	digest[i*8+1] = (h >> 16) & 0xFF;
	digest[i*8+2] = (h >>  8) & 0xFF;
	digest[i*8+3] = (h >>  0) & 0xFF;
	digest[i*8+4] = (l >> 24) & 0xFF;
	digest[i*8+5] = (l >> 16) & 0xFF;
	digest[i*8+6] = (l >>  8) & 0xFF;
	digest[i*8+7] = (l >>  0) & 0xFF;
    }
}

void SHA512_Simple(const void *p, int len, unsigned char *output) {
    SHA512_State s;

    SHA512_Init(&s);
    SHA512_Bytes(&s, p, len);
    SHA512_Final(&s, output);
}

#ifdef TEST

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(void) {
    unsigned char digest[64];
    int i, j, errors;

    struct {
	const char *teststring;
	unsigned char digest512[64];
    } tests[] = {
	{ "abc", {
	    0xdd, 0xaf, 0x35, 0xa1, 0x93, 0x61, 0x7a, 0xba,
            0xcc, 0x41, 0x73, 0x49, 0xae, 0x20, 0x41, 0x31,
            0x12, 0xe6, 0xfa, 0x4e, 0x89, 0xa9, 0x7e, 0xa2,
            0x0a, 0x9e, 0xee, 0xe6, 0x4b, 0x55, 0xd3, 0x9a,
            0x21, 0x92, 0x99, 0x2a, 0x27, 0x4f, 0xc1, 0xa8,
            0x36, 0xba, 0x3c, 0x23, 0xa3, 0xfe, 0xeb, 0xbd,
            0x45, 0x4d, 0x44, 0x23, 0x64, 0x3c, 0xe8, 0x0e,
            0x2a, 0x9a, 0xc9, 0x4f, 0xa5, 0x4c, 0xa4, 0x9f,
	} },
	{ "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"
	"hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", {
	    0x8e, 0x95, 0x9b, 0x75, 0xda, 0xe3, 0x13, 0xda,
            0x8c, 0xf4, 0xf7, 0x28, 0x14, 0xfc, 0x14, 0x3f,
            0x8f, 0x77, 0x79, 0xc6, 0xeb, 0x9f, 0x7f, 0xa1,
            0x72, 0x99, 0xae, 0xad, 0xb6, 0x88, 0x90, 0x18,
            0x50, 0x1d, 0x28, 0x9e, 0x49, 0x00, 0xf7, 0xe4,
            0x33, 0x1b, 0x99, 0xde, 0xc4, 0xb5, 0x43, 0x3a,
            0xc7, 0xd3, 0x29, 0xee, 0xb6, 0xdd, 0x26, 0x54,
            0x5e, 0x96, 0xe5, 0x5b, 0x87, 0x4b, 0xe9, 0x09,
	} },
	{ NULL, {
	    0xe7, 0x18, 0x48, 0x3d, 0x0c, 0xe7, 0x69, 0x64,
	    0x4e, 0x2e, 0x42, 0xc7, 0xbc, 0x15, 0xb4, 0x63,
	    0x8e, 0x1f, 0x98, 0xb1, 0x3b, 0x20, 0x44, 0x28,
	    0x56, 0x32, 0xa8, 0x03, 0xaf, 0xa9, 0x73, 0xeb,
	    0xde, 0x0f, 0xf2, 0x44, 0x87, 0x7e, 0xa6, 0x0a,
	    0x4c, 0xb0, 0x43, 0x2c, 0xe5, 0x77, 0xc3, 0x1b,
	    0xeb, 0x00, 0x9c, 0x5c, 0x2c, 0x49, 0xaa, 0x2e,
	    0x4e, 0xad, 0xb2, 0x17, 0xad, 0x8c, 0xc0, 0x9b, 
	} },
    };

    errors = 0;

    for (i = 0; i < sizeof(tests) / sizeof(*tests); i++) {
	if (tests[i].teststring) {
	    SHA512_Simple(tests[i].teststring,
			  strlen(tests[i].teststring), digest);
	} else {
	    SHA512_State s;
	    int n;
	    SHA512_Init(&s);
	    for (n = 0; n < 1000000 / 40; n++)
		SHA512_Bytes(&s, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
			     40);
	    SHA512_Final(&s, digest);
	}
	for (j = 0; j < 64; j++) {
	    if (digest[j] != tests[i].digest512[j]) {
		fprintf(stderr,
			"\"%s\" digest512 byte %d should be 0x%02x, is 0x%02x\n",
			tests[i].teststring, j, tests[i].digest512[j],
			digest[j]);
		errors++;
	    }
	}

    }

    printf("%d errors\n", errors);

    return 0;
}

#endif
