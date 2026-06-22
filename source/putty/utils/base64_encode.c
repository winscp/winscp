#include "misc.h"

void base64_encode_bs(BinarySink *bs, ptrlen input, int cpl)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, input);
    int linelen = 0;

    while (get_avail(src)) {
        size_t n = get_avail(src) < 3 ? get_avail(src) : 3;
        ptrlen binatom = get_data(src, n);

        char b64atom[4];
        base64_encode_atom(binatom.ptr, binatom.len, b64atom);
        for (size_t i = 0; i < 4; i++) {
            if (cpl > 0 && linelen >= cpl) {
                linelen = 0;
                put_byte(bs, '\n');
            }
            put_byte(bs, b64atom[i]);
            linelen++;
        }
    }
    if (cpl > 0)
        put_byte(bs, '\n');
}

void base64_encode_fp(FILE *fp, ptrlen input, int cpl)
{
    stdio_sink ss;
    stdio_sink_init(&ss, fp);
    base64_encode_bs(BinarySink_UPCAST(&ss), input, cpl);
}

strbuf *base64_encode_sb(ptrlen input, int cpl)
{
    strbuf *sb = strbuf_new_nm();
    base64_encode_bs(BinarySink_UPCAST(sb), input, cpl);
    return sb;
}
