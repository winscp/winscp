/*
 * Printing interface for PuTTY.
 */

#include "putty.h"
#include <winspool.h>

struct printer_enum_tag {
    int nprinters;
    DWORD enum_level;
    union {
	LPPRINTER_INFO_4 i4;
	LPPRINTER_INFO_5 i5;
    } info;
};

struct printer_job_tag {
    HANDLE hprinter;
};

static char *printer_add_enum(int param, DWORD level, char *buffer,
                              int offset, int *nprinters_ptr)
{
    DWORD needed, nprinters;

    buffer = sresize(buffer, offset+512, char);

    /*
     * Exploratory call to EnumPrinters to determine how much space
     * we'll need for the output. Discard the return value since it
     * will almost certainly be a failure due to lack of space.
     */
    EnumPrinters(param, NULL, level, buffer+offset, 512,
		 &needed, &nprinters);

    if (needed < 512)
        needed = 512;

    buffer = sresize(buffer, offset+needed, char);

    if (EnumPrinters(param, NULL, level, buffer+offset,
                     needed, &needed, &nprinters) == 0)
        return NULL;

    *nprinters_ptr += nprinters;

    return buffer;
}

printer_enum *printer_start_enum(int *nprinters_ptr)
{
    printer_enum *ret = snew(printer_enum);
    char *buffer = NULL, *retval;

    *nprinters_ptr = 0;		       /* default return value */
    buffer = snewn(512, char);

    /*
     * Determine what enumeration level to use.
     * When enumerating printers, we need to use PRINTER_INFO_4 on
     * NT-class systems to avoid Windows looking too hard for them and
     * slowing things down; and we need to avoid PRINTER_INFO_5 as
     * we've seen network printers not show up.
     * On 9x-class systems, PRINTER_INFO_4 isn't available and
     * PRINTER_INFO_5 is recommended.
     * Bletch.
     */
    if (osVersion.dwPlatformId != VER_PLATFORM_WIN32_NT) {
	ret->enum_level = 5;
    } else {
	ret->enum_level = 4;
    }

    retval = printer_add_enum(PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS,
			      ret->enum_level, buffer, 0, nprinters_ptr);
    if (!retval)
        goto error;
    else
        buffer = retval;

    switch (ret->enum_level) {
      case 4:
	ret->info.i4 = (LPPRINTER_INFO_4)buffer;
	break;
      case 5:
	ret->info.i5 = (LPPRINTER_INFO_5)buffer;
	break;
    }
    ret->nprinters = *nprinters_ptr;
    
    return ret;

    error:
    sfree(buffer);
    sfree(ret);
    *nprinters_ptr = 0;
    return NULL;
}

char *printer_get_name(printer_enum *pe, int i)
{
    if (!pe)
	return NULL;
    if (i < 0 || i >= pe->nprinters)
	return NULL;
    switch (pe->enum_level) {
      case 4:
	return pe->info.i4[i].pPrinterName;
      case 5:
	return pe->info.i5[i].pPrinterName;
      default:
	return NULL;
    }
}

void printer_finish_enum(printer_enum *pe)
{
    if (!pe)
	return;
    switch (pe->enum_level) {
      case 4:
	sfree(pe->info.i4);
	break;
      case 5:
	sfree(pe->info.i5);
	break;
    }
    sfree(pe);
}

printer_job *printer_start_job(char *printer)
{
    printer_job *ret = snew(printer_job);
    DOC_INFO_1 docinfo;
    int jobstarted = 0, pagestarted = 0;

    ret->hprinter = NULL;
    if (!OpenPrinter(printer, &ret->hprinter, NULL))
	goto error;

    docinfo.pDocName = "PuTTY remote printer output";
    docinfo.pOutputFile = NULL;
    docinfo.pDatatype = "RAW";

    if (!StartDocPrinter(ret->hprinter, 1, (LPSTR)&docinfo))
	goto error;
    jobstarted = 1;

    if (!StartPagePrinter(ret->hprinter))
	goto error;
    pagestarted = 1;

    return ret;

    error:
    if (pagestarted)
	EndPagePrinter(ret->hprinter);
    if (jobstarted)
	EndDocPrinter(ret->hprinter);
    if (ret->hprinter)
	ClosePrinter(ret->hprinter);
    sfree(ret);
    return NULL;
}

void printer_job_data(printer_job *pj, void *data, int len)
{
    DWORD written;

    if (!pj)
	return;

    WritePrinter(pj->hprinter, data, len, &written);
}

void printer_finish_job(printer_job *pj)
{
    if (!pj)
	return;

    EndPagePrinter(pj->hprinter);
    EndDocPrinter(pj->hprinter);
    ClosePrinter(pj->hprinter);
    sfree(pj);
}
