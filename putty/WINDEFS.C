/*
 * windefs.c: default settings that are specific to Windows.
 */

#include "putty.h"

#include <commctrl.h>

FontSpec platform_default_fontspec(const char *name)
{
    FontSpec ret;
    if (!strcmp(name, "Font")) {
	strcpy(ret.name, "Courier New");
	ret.isbold = 0;
	ret.charset = ANSI_CHARSET;
	ret.height = 10;
    } else {
	ret.name[0] = '\0';
    }
    return ret;
}

Filename platform_default_filename(const char *name)
{
    Filename ret;
    if (!strcmp(name, "LogFileName"))
	strcpy(ret.path, "putty.log");
    else
	*ret.path = '\0';
    return ret;
}

char *platform_default_s(const char *name)
{
    return NULL;
}

int platform_default_i(const char *name, int def)
{
    return def;
}
