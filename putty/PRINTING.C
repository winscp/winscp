/*
 * Printing interface for PuTTY.
 */

#include <windows.h>
#include "putty.h"

/*
 * Boggle. Flipping between the two branches of this #if appears to
 * make all the difference as to whether network printers show up
 * under PRINTER_ENUM_CONNECTIONS on NT 4. I don't pretend to
 * understand this...
 */
#if 0
#define ENUM_LEVEL 5
#define ENUM_PTR LPPRINTER_INFO_5
#define ENUM_TYPE PRINTER_INFO_5
#define ENUM_MEMBER pPrinterName
#else
#define ENUM_LEVEL 1
#define ENUM_PTR LPPRINTER_INFO_1
#define ENUM_TYPE PRINTER_INFO_1
#define ENUM_MEMBER pName
#endif

struct printer_enum_tag {
    int nprinters;
    ENUM_PTR info;
};

struct printer_job_tag {
    HANDLE hprinter;
};

static char *printer_add_enum(int param, char *buffer,
                              int offset, int *nprinters_ptr)
{
    DWORD needed, nprinters;

    buffer = sresize(buffer, offset+512, char);

    /*
     * Exploratory call to EnumPrinters to determine how much space
     * we'll need for the output. Discard the return value since it
     * will almost certainly be a failure due to lack of space.
     */
    EnumPrinters(param, NULL, ENUM_LEVEL, buffer+offset, 512,
		 &needed, &nprinters);

    if (needed < 512)
        needed = 512;

    buffer = sresize(buffer, offset+needed, char);

    if (EnumPrinters(param, NULL, ENUM_LEVEL, buffer+offset,
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

    retval = printer_add_enum(PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS,
			      buffer, 0, nprinters_ptr);
    if (!retval)
        goto error;
    else
        buffer = retval;

    ret->info = (ENUM_PTR)buffer;
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
    return pe->info[i].ENUM_MEMBER;
}

void printer_finish_enum(printer_enum *pe)
{
    if (!pe)
	return;
    sfree(pe->info);
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
