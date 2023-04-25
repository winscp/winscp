/*
 * Write data to a file or BinarySink in the form of a C string
 * literal, with any non-printable-ASCII character escaped
 * appropriately.
 */

#include "defs.h"
#include "misc.h"

void BinarySink_put_c_string_literal(BinarySink *bs, ptrlen str)
{
    for (const char *p = str.ptr; p < (const char *)str.ptr + str.len; p++) {
        char c = *p;

        if (c == '\n')
            put_datalit(bs, "\\n");
        else if (c == '\r')
            put_datalit(bs, "\\r");
        else if (c == '\t')
            put_datalit(bs, "\\t");
        else if (c == '\b')
            put_datalit(bs, "\\b");
        else if (c == '\\')
            put_datalit(bs, "\\\\");
        else if (c == '"')
            put_datalit(bs, "\\\"");
        else if (c >= 32 && c <= 126)
            put_byte(bs, c);
        else
            put_fmt(bs, "\\%03o", (unsigned)c & 0xFFU);
    }
}

void write_c_string_literal(FILE *fp, ptrlen str)
{
    stdio_sink s;
    stdio_sink_init(&s, fp);
    put_c_string_literal(&s, str);
}
