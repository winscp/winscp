Using MFC 4.2 with Borland C++Builder 4.0
-----------------------------------------

Here are the steps to build the MFC libraries with BCB.  We will assume
that your copy of Borland C++Builder is located in c:\bcb.

1) Build the Libraries
   -------------------
   Note: the makefile's DEBUG=1 switch enables Diagnostics in the libraries
   and DBGINFO=1 enables Turbo Debugger information.

1.1) Static Non-Debug library
     ------------------------

To build the static, non-debug library (NAFXCW.LIB)

    make -fborland.mak  DEBUG=0 LIBDIR=c:\bcb\lib

1.2) Static Debug library
     --------------------

To build the static, debug library (NAFXCWD.LIB)

    make  -fborland.mak LIBDIR=c:\bcb\lib

1.3) Non-Debug DLLs and Libraries
     ----------------------------

To build the monolithic MFC DLL (BFC42.DLL, BFC42.LIB, and BFC42S.LIB)

    make -fbfcdll.mak DEBUG=0 LIBDIR=c:\bcb\lib

NOTE: BFC42.DLL contains everything you'll need for MFC in non-debug
mode.  When building MFC with DEBUG=1, you'll end up with 3 DLLs (see
1.4)


1.4) Debug DLLs and Libraries
     ------------------------

To build the debug MFC DLLs:

    BFC42D.DLL,  BFC42D.LIB
    BFCO42D.DLL, BFCO42D.LIB
    BFCN42D.DLL, BFCN42D.LIB
    BFCD42D.DLL, BFCD42D.LIB

    make -fbfcdll.mak LIBDIR=c:\bcb\lib
    make -fbfcole.mak LIBDIR=c:\bcb\lib
    make -fbfcnet.mak LIBDIR=c:\bcb\lib
    make -fbfcdb.mak  LIBDIR=c:\bcb\lib

2) Install Libraries and DLLs
   --------------------------

The libraries that were created should already be in the Borland
directory C:\BCB\LIB. Copy the new DLLs (located in this directory) to
the Windows System directory, typically C:\WINDOWS\SYSTEM32.


3) Try it out
   ----------

The MFC examples all contain Borland C++Builder project files (.BPRs) that
have the proper defines and switches set for the type of MFC application
being built.

In case you want to add these into an existing MAKEFILE or otherwise compile
at the command line, here is a list of the defines that should be added with
-D in a compiler configuration file:

  MFC 4.2, Static,  No OLE:
    _X86_;_WINDOWS;_MSC_VER=1100;_AFX_PORTABLE;_AFX_NOFORCE_LIBS;_MT;
    _CRT_PORTABLE;_AFX_NO_DEBUG_CRT;

  MFC 4.2, Static,  OLE:
    INC_OLE2;_X86_;_WINDOWS;_MSC_VER=1100;_AFX_PORTABLE;_AFX_NOFORCE_LIBS;
    _MT;_CRT_PORTABLE;_AFX_NO_DEBUG_CRT;

  MFC 4.2, Dynamic, No OLE:
    _RTLDLL;_AFXDLL;_DLL;_X86_;_WINDOWS;_MSC_VER=1100;_AFX_PORTABLE;
    _AFX_NOFORCE_LIBS;_MT;_CRT_PORTABLE;_AFX_NO_DEBUG_CRT;

  MFC 4.2, Dynamic, OLE:
    INC_OLE2;_RTLDLL;_AFXDLL;_DLL;_X86_;_WINDOWS;_MSC_VER=1100;
    _AFX_PORTABLE;_AFX_NOFORCE_LIBS;_MT;_CRT_PORTABLE;_AFX_NO_DEBUG_CRT;

  The libraries needed to link with are as follows:

  MFC 4.2, Static,  No Diagnostics, No OLE:
    nafxcw.lib odbc32.lib import32.lib cw32mt.lib

  MFC 4.2, Static,  No Diagnostics, OLE:
    nafxcw.lib ole2w32.lib odbc32.lib import32.lib cw32mt.lib

  MFC 4.2, Static,  Diagnostics,    No OLE:
    nafxcwd.lib odbc32.lib import32.lib cw32mt.lib

  MFC 4.2, Static,  Diagnostics,    OLE:
    nafxcwd.lib ole2w32.lib odbc32.lib import32.lib cw32mt.lib

  MFC 4.2, Dynamic, No Diagnostics, No OLE:
    bfcs42.lib bfc42.lib odbc32.lib import32.lib cw32mti.lib

  MFC 4.2, Dynamic, No Diagnostics, OLE:
    bfcs42.lib bfc42.lib ole2w32.lib odbc32.lib import32.lib cw32mti.lib

  MFC 4.2, Dynamic, Diagnostics,    No OLE:
    bfcs42d.lib bfc42d.lib bfco42d.lib bfcn42d.lib odbc32.lib
    import32.lib cw32mti.lib

  MFC 4.2, Dynamic, Diagnostics,    OLE:
    bfcs42d.lib bfc42d.lib bfco42d.lib bfcn42d.lib ole2w32.lib
    odbc32.lib import32.lib cw32mti.lib

  NOTE: When building from the command line remember to use the -VF switch

