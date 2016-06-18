/*
 * CRC32 implementation.
 *
 * The basic concept of a CRC is that you treat your bit-string
 * abcdefg... as a ludicrously long polynomial M=a+bx+cx^2+dx^3+...
 * over Z[2]. You then take a modulus polynomial P, and compute the
 * remainder of M on division by P. Thus, an erroneous message N
 * will only have the same CRC if the difference E = M-N is an
 * exact multiple of P. (Note that as we are working over Z[2], M-N
 * = N-M = M+N; but that's not very important.)
 *
 * What makes the CRC good is choosing P to have good properties:
 *
 *  - If its first and last terms are both nonzero then it cannot
 *    be a factor of any single term x^i. Therefore if M and N
 *    differ by exactly one bit their CRCs will guaranteeably
 *    be distinct.
 *
 *  - If it has a prime (irreducible) factor with three terms then
 *    it cannot divide a polynomial of the form x^i(1+x^j).
 *    Therefore if M and N differ by exactly _two_ bits they will
 *    have different CRCs.
 *
 *  - If it has a factor (x+1) then it cannot divide a polynomial
 *    with an odd number of terms. Therefore if M and N differ by
 *    _any odd_ number of bits they will have different CRCs.
 *
 *  - If the error term E is of the form x^i*B(x) where B(x) has
 *    order less than P (i.e. a short _burst_ of errors) then P
 *    cannot divide E (since no polynomial can divide a shorter
 *    one), so any such error burst will be spotted.
 *
 * The CRC32 standard polynomial is
 *   x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x^1+x^0
 *
 * In fact, we don't compute M mod P; we compute M*x^32 mod P.
 *
 * The concrete implementation of the CRC is this: we maintain at
 * all times a 32-bit word which is the current remainder of the
 * polynomial mod P. Whenever we receive an extra bit, we multiply
 * the existing remainder by x, add (XOR) the x^32 term thus
 * generated to the new x^32 term caused by the incoming bit, and
 * remove the resulting combined x^32 term if present by replacing
 * it with (P-x^32).
 *
 * Bit 0 of the word is the x^31 term and bit 31 is the x^0 term.
 * Thus, multiplying by x means shifting right. So the actual
 * algorithm goes like this:
 *
 *   x32term = (crcword & 1) ^ newbit;
 *   crcword = (crcword >> 1) ^ (x32term * 0xEDB88320);
 *
 * In practice, we pre-compute what will happen to crcword on any
 * given sequence of eight incoming bits, and store that in a table
 * which we then use at run-time to do the job:
 * 
 *   outgoingplusnew = (crcword & 0xFF) ^ newbyte;
 *   crcword = (crcword >> 8) ^ table[outgoingplusnew];
 *
 * where table[outgoingplusnew] is computed by setting crcword=0
 * and then iterating the first code fragment eight times (taking
 * the incoming byte low bit first).
 *
 * Note that all shifts are rightward and thus no assumption is
 * made about exact word length! (Although word length must be at
 * _least_ 32 bits, but ANSI C guarantees this for `unsigned long'
 * anyway.)
 */

#include <stdlib.h>

#include "ssh.h"

/* ----------------------------------------------------------------------
 * Multi-function module. Can be compiled three ways.
 *
 *  - Compile with no special #defines. Will generate a table
 *    that's already initialised at compile time, and one function
 *    crc32_compute(buf,len) that uses it. Normal usage.
 *
 *  - Compile with INITFUNC defined. Will generate an uninitialised
 *    array as the table, and as well as crc32_compute(buf,len) it
 *    will also generate void crc32_init(void) which sets up the
 *    table at run time. Useful if binary size is important.
 *
 *  - Compile with GENPROGRAM defined. Will create a standalone
 *    program that does the initialisation and outputs the table as
 *    C code.
 */

#define POLY (0xEDB88320L)

#ifdef GENPROGRAM
#define INITFUNC		       /* the gen program needs the init func :-) */
#endif

#ifdef INITFUNC

/*
 * This variant of the code generates the table at run-time from an
 * init function.
 */
static unsigned long crc32_table[256];

void crc32_init(void)
{
    unsigned long crcword;
    int i;

    for (i = 0; i < 256; i++) {
	unsigned long newbyte, x32term;
	int j;
	crcword = 0;
	newbyte = i;
	for (j = 0; j < 8; j++) {
	    x32term = (crcword ^ newbyte) & 1;
	    crcword = (crcword >> 1) ^ (x32term * POLY);
	    newbyte >>= 1;
	}
	crc32_table[i] = crcword;
    }
}

#else

/*
 * This variant of the code has the data already prepared.
 */
static const unsigned long crc32_table[256] = {
    0x00000000L, 0x77073096L, 0xEE0E612CL, 0x990951BAL,
    0x076DC419L, 0x706AF48FL, 0xE963A535L, 0x9E6495A3L,
    0x0EDB8832L, 0x79DCB8A4L, 0xE0D5E91EL, 0x97D2D988L,
    0x09B64C2BL, 0x7EB17CBDL, 0xE7B82D07L, 0x90BF1D91L,
    0x1DB71064L, 0x6AB020F2L, 0xF3B97148L, 0x84BE41DEL,
    0x1ADAD47DL, 0x6DDDE4EBL, 0xF4D4B551L, 0x83D385C7L,
    0x136C9856L, 0x646BA8C0L, 0xFD62F97AL, 0x8A65C9ECL,
    0x14015C4FL, 0x63066CD9L, 0xFA0F3D63L, 0x8D080DF5L,
    0x3B6E20C8L, 0x4C69105EL, 0xD56041E4L, 0xA2677172L,
    0x3C03E4D1L, 0x4B04D447L, 0xD20D85FDL, 0xA50AB56BL,
    0x35B5A8FAL, 0x42B2986CL, 0xDBBBC9D6L, 0xACBCF940L,
    0x32D86CE3L, 0x45DF5C75L, 0xDCD60DCFL, 0xABD13D59L,
    0x26D930ACL, 0x51DE003AL, 0xC8D75180L, 0xBFD06116L,
    0x21B4F4B5L, 0x56B3C423L, 0xCFBA9599L, 0xB8BDA50FL,
    0x2802B89EL, 0x5F058808L, 0xC60CD9B2L, 0xB10BE924L,
    0x2F6F7C87L, 0x58684C11L, 0xC1611DABL, 0xB6662D3DL,
    0x76DC4190L, 0x01DB7106L, 0x98D220BCL, 0xEFD5102AL,
    0x71B18589L, 0x06B6B51FL, 0x9FBFE4A5L, 0xE8B8D433L,
    0x7807C9A2L, 0x0F00F934L, 0x9609A88EL, 0xE10E9818L,
    0x7F6A0DBBL, 0x086D3D2DL, 0x91646C97L, 0xE6635C01L,
    0x6B6B51F4L, 0x1C6C6162L, 0x856530D8L, 0xF262004EL,
    0x6C0695EDL, 0x1B01A57BL, 0x8208F4C1L, 0xF50FC457L,
    0x65B0D9C6L, 0x12B7E950L, 0x8BBEB8EAL, 0xFCB9887CL,
    0x62DD1DDFL, 0x15DA2D49L, 0x8CD37CF3L, 0xFBD44C65L,
    0x4DB26158L, 0x3AB551CEL, 0xA3BC0074L, 0xD4BB30E2L,
    0x4ADFA541L, 0x3DD895D7L, 0xA4D1C46DL, 0xD3D6F4FBL,
    0x4369E96AL, 0x346ED9FCL, 0xAD678846L, 0xDA60B8D0L,
    0x44042D73L, 0x33031DE5L, 0xAA0A4C5FL, 0xDD0D7CC9L,
    0x5005713CL, 0x270241AAL, 0xBE0B1010L, 0xC90C2086L,
    0x5768B525L, 0x206F85B3L, 0xB966D409L, 0xCE61E49FL,
    0x5EDEF90EL, 0x29D9C998L, 0xB0D09822L, 0xC7D7A8B4L,
    0x59B33D17L, 0x2EB40D81L, 0xB7BD5C3BL, 0xC0BA6CADL,
    0xEDB88320L, 0x9ABFB3B6L, 0x03B6E20CL, 0x74B1D29AL,
    0xEAD54739L, 0x9DD277AFL, 0x04DB2615L, 0x73DC1683L,
    0xE3630B12L, 0x94643B84L, 0x0D6D6A3EL, 0x7A6A5AA8L,
    0xE40ECF0BL, 0x9309FF9DL, 0x0A00AE27L, 0x7D079EB1L,
    0xF00F9344L, 0x8708A3D2L, 0x1E01F268L, 0x6906C2FEL,
    0xF762575DL, 0x806567CBL, 0x196C3671L, 0x6E6B06E7L,
    0xFED41B76L, 0x89D32BE0L, 0x10DA7A5AL, 0x67DD4ACCL,
    0xF9B9DF6FL, 0x8EBEEFF9L, 0x17B7BE43L, 0x60B08ED5L,
    0xD6D6A3E8L, 0xA1D1937EL, 0x38D8C2C4L, 0x4FDFF252L,
    0xD1BB67F1L, 0xA6BC5767L, 0x3FB506DDL, 0x48B2364BL,
    0xD80D2BDAL, 0xAF0A1B4CL, 0x36034AF6L, 0x41047A60L,
    0xDF60EFC3L, 0xA867DF55L, 0x316E8EEFL, 0x4669BE79L,
    0xCB61B38CL, 0xBC66831AL, 0x256FD2A0L, 0x5268E236L,
    0xCC0C7795L, 0xBB0B4703L, 0x220216B9L, 0x5505262FL,
    0xC5BA3BBEL, 0xB2BD0B28L, 0x2BB45A92L, 0x5CB36A04L,
    0xC2D7FFA7L, 0xB5D0CF31L, 0x2CD99E8BL, 0x5BDEAE1DL,
    0x9B64C2B0L, 0xEC63F226L, 0x756AA39CL, 0x026D930AL,
    0x9C0906A9L, 0xEB0E363FL, 0x72076785L, 0x05005713L,
    0x95BF4A82L, 0xE2B87A14L, 0x7BB12BAEL, 0x0CB61B38L,
    0x92D28E9BL, 0xE5D5BE0DL, 0x7CDCEFB7L, 0x0BDBDF21L,
    0x86D3D2D4L, 0xF1D4E242L, 0x68DDB3F8L, 0x1FDA836EL,
    0x81BE16CDL, 0xF6B9265BL, 0x6FB077E1L, 0x18B74777L,
    0x88085AE6L, 0xFF0F6A70L, 0x66063BCAL, 0x11010B5CL,
    0x8F659EFFL, 0xF862AE69L, 0x616BFFD3L, 0x166CCF45L,
    0xA00AE278L, 0xD70DD2EEL, 0x4E048354L, 0x3903B3C2L,
    0xA7672661L, 0xD06016F7L, 0x4969474DL, 0x3E6E77DBL,
    0xAED16A4AL, 0xD9D65ADCL, 0x40DF0B66L, 0x37D83BF0L,
    0xA9BCAE53L, 0xDEBB9EC5L, 0x47B2CF7FL, 0x30B5FFE9L,
    0xBDBDF21CL, 0xCABAC28AL, 0x53B39330L, 0x24B4A3A6L,
    0xBAD03605L, 0xCDD70693L, 0x54DE5729L, 0x23D967BFL,
    0xB3667A2EL, 0xC4614AB8L, 0x5D681B02L, 0x2A6F2B94L,
    0xB40BBE37L, 0xC30C8EA1L, 0x5A05DF1BL, 0x2D02EF8DL
};

#endif

#ifdef GENPROGRAM
int main(void)
{
    unsigned long crcword;
    int i;

    crc32_init();
    for (i = 0; i < 256; i++) {
	printf("%s0x%08XL%s",
	       (i % 4 == 0 ? "    " : " "),
	       crc32_table[i],
	       (i % 4 == 3 ? (i == 255 ? "\n" : ",\n") : ","));
    }

    return 0;
}
#endif

unsigned long crc32_update(unsigned long crcword, const void *buf, size_t len)
{
    const unsigned char *p = (const unsigned char *) buf;
    while (len--) {
	unsigned long newbyte = *p++;
	newbyte ^= crcword & 0xFFL;
	crcword = (crcword >> 8) ^ crc32_table[newbyte];
    }
    return crcword;
}

unsigned long crc32_compute(const void *buf, size_t len)
{
    return crc32_update(0L, buf, len);
}
