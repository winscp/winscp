/*
 * Implementation of FontSpec for Windows.
 */

#include "putty.h"

FontSpec *fontspec_new(const char *name, bool bold, int height, int charset)
{
    FontSpec *f = snew(FontSpec);
    f->name = dupstr(name);
    f->isbold = bold;
    f->height = height;
    f->charset = charset;
    return f;
}

FontSpec *fontspec_new_default(void)
{
    return fontspec_new("", false, 0, 0);
}

FontSpec *fontspec_copy(const FontSpec *f)
{
    return fontspec_new(f->name, f->isbold, f->height, f->charset);
}

void fontspec_free(FontSpec *f)
{
    sfree(f->name);
    sfree(f);
}

void fontspec_serialise(BinarySink *bs, FontSpec *f)
{
    put_asciz(bs, f->name);
    put_uint32(bs, f->isbold);
    put_uint32(bs, f->height);
    put_uint32(bs, f->charset);
}

FontSpec *fontspec_deserialise(BinarySource *src)
{
    const char *name = get_asciz(src);
    unsigned isbold = get_uint32(src);
    unsigned height = get_uint32(src);
    unsigned charset = get_uint32(src);
    return fontspec_new(name, isbold, height, charset);
}
