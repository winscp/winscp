#include "misc.h"

void base64_decode_bs(BinarySink *bs, ptrlen input)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, input);

    while (get_avail(src)) {
        char b64atom[4];
        unsigned char binatom[3];

        { // WINSCP
        size_t i;
        for (i = 0; i < 4 ;) {
            char c = get_byte(src);
            if (get_err(src))
                c = '=';
            if (c == '\n' || c == '\r')
                continue;
            b64atom[i++] = c;
        }
        } // WINSCP

        put_data(bs, binatom, base64_decode_atom(b64atom, binatom));
    }
}

void base64_decode_fp(FILE *fp, ptrlen input)
{
    stdio_sink ss;
    stdio_sink_init(&ss, fp);
    base64_decode_bs(BinarySink_UPCAST(&ss), input);
}

strbuf *base64_decode_sb(ptrlen input)
{
    strbuf *sb = strbuf_new_nm();
    base64_decode_bs(BinarySink_UPCAST(sb), input);
    return sb;
}
