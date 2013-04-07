# BORLAND.MAK: Borland makefile for MFC variants
#
# Usage: MAKE -F BORLAND.MAK CLEAN        (removes all intermediary files)
#    or: MAKE -F BORLAND.MAK options      (builds library variant (see below))
# Note that an MAKE CLEAN should be performed before building a new variant.
#
# 'Options' are one of each of:
#   "DLL"              (defaults to 0)
#           If this item is 0, then a normal library is generated.
#                       DLL=1 is obsolete and not supported by this release.
#           If this item is 2, objects suitable for the shared DLL version
#           of MFC are created.  Note: DLL=2 is to be used only from
#           MFCDLL.MAK, MFCOLE.MAK, or MFCDB.MAK
#
#   "DEBUG"             (defaults to 1)
#           If this item is 1, diagnostic support is compiled into
#           the library.  If this item is 0, then diagnostic support
#           is disabled. Diagnostic support does not include debug info
#           information.
#
#   "OBJ=.\obj"         (defaults to '$$(MODEL)$(BASE)$(DEBUG)')
#           This optional specification specifies where temporary OBJ files
#           are stored during the build process.  The directory is created or
#           removed as necessary.
#
#   "OPT="              (no default value)
#           This allows additional compiler options to be added to the build.
#           If more than one switch is desired, put double-quotes around the
#           whole OPT= argument, e.g., "OPT=/J /W3".
#
#   "NO_PCH=1"
#           Set this item to override the default use of precompiled headers.
#
#   "BROWSE=1"          (defaults to 0)
#           Set this option to build browse info
#
#   "MT=0"              (defaults to 1)
#           To build a non-multithreaded library instead of the default
#           (which enables multitheading and uses the multithread
#           C-runtimes) you can use MT=0.
#
#=============================================================================
# Borland Additional flags
#=============================================================================
#
#   "NO_CLEAN_PCH"      (defaults to off)
#           To prevent deleting an existing PCH (.csm) file
#
#   "DBGINFO"           (defaults to 0)
#           If this item is 1, Turbo Debugger debug info is compiled into
#           the library.  If it's 0, then no debugger info is added.
#
#   "LIBDIR=..\..\lib"
#           Directory for libraries that are created
#
#   "MFCINCL=..\..\include\mfc"
#           MFC Include directory base
#
#   "BCINCL=..\..\include"
#           BC++ Include directory
#
#############################################################################
# Define defaults if not defined

# Default to DEBUG mode
!ifndef DEBUG
DEBUG=0
!endif

# Default to No Debug Info mode
!ifndef DBGINFO
DBGINFO=0
!endif

# Check for MS syntax
!ifdef CODEVIEW
DBGINFO=$(CODEVIEW)
!endif

# Default to NOT DLL
!ifndef DLL
DLL=0
!endif

# Default to no BROWSE info
!ifndef BROWSE
BROWSE=0
!endif

# Default to no precompiled headers
#!ifndef NO_PCH
#NO_PCH=1
#!endif

# Default to _MBCS build
MBCS=1

# Default to multithreading support
!ifndef MT
MT=1
!endif

# Default to not enable Borland CodeGuard
!ifndef CG
CG=0
!endif

# Lib directory
!ifndef LIBDIR
LIBDIR=..\..\LIB
!endif

# Lib Path
!ifndef LPATH
LPATH=..\..\LIB;..\..\LIB\PSDK
!endif

# MFC Include directory base
!ifndef MFCINCL
MFCINCL=..\..\include\mfc
!endif

# BC Include directory
!ifndef BCINCL
BCINCL=..\..\include;..\..\include\atl
!endif

# Disable Warnings
!ifndef NO_WARNINGS
NO_WARNINGS=0
!endif


# Clean up PCH file
!ifndef NO_CLEAN_PCH
NO_CLEAN_PCH=0
!endif

BASE=W
MODEL=N
!if "$(MBCS)" != "0"
TARGDEFS=$(TARGDEFS) /D_MBCS
!endif
PLATFORM=INTEL

LFLAGS=$(LFLAGS) /j$(LPATH) /L$(LPATH)
!if "$(NO_WARNINGS)" == "1"
LFLAGS=$(LFLAGS) /w-
!endif

#
# DEBUG OPTIONS
#
!if "$(DEBUG)" != "0"

DEBUGSUF=D
DEBDEFS=/D_DEBUG
DEBOPTS=

!endif

#
# NON-DEBUG OPTIONS
#
!if "$(DEBUG)" == "0"

DEBUGSUF=
DEBDEFS=
DEBOPTS=/O1

!endif

!if "$(DBGINFO)" == "1"
DEBOPTS=/Od /v /vi
!endif

!if "$(CG)" == "1"
DEBOPTS=$(DEBOPTS) -vG
!endif

#
# PLATFORM options
#
CC=bcc32
LIB32=tlib
!ifndef LINK32
LINK32=ilink32 /Gn
!endif
CL_MODEL=/D_X86_
GENDEFS=perl -w $(MAKEDIR)\gendefs.pl

# TYPE = Library Type Designator
#       c = normal C library
#       d = DLL library
TYPE=c

!if "$(DEXT)" == ""
DEXT=
!endif

#
# Object File Directory
#
!if "$(OBJ)" == ""
D=$$$(MODEL)$(BASE)$(DEBUGSUF)$(DEXT)    # subdirectory specific to variant
!else
D=$(OBJ)                                 # User specified directory
!endif

#
# _AFXDLL DLL Variant
#
!if "$(DLL)" == "2"
!if "$(TARG)" == ""
!error DLL=2 is used only from BFCDLL.MAK, BFCOLE.MAK, or BFCDB.MAK
!endif
GOAL=$(TARG)
TYPE=e
!if "$(OBJ)" == ""
D=DLL$(DEBUGSUF).$(BASE)
!if "$(UNICODE)" == "1"
D=$(MODEL)$(D)
!endif
D=$$$(D)
!endif
TARGDEFS=$(TARGDEFS) /D_WINDLL /D_AFXDLL
!else
GOAL=$(MODEL)afx$(TYPE)$(BASE)$(DEBUGSUF)
!endif

#
# Threaded-ness
#
!if "$(MT)" != "0"
TARGOPTS=$(TARGOPTS) /WM /D_MT=1
!else
!error This makefile only builds multi-threaded libraries, not single threaded.
!endif

#
# Import libraries for the DLL builds
#
IMPORT_LIBS=import32.lib inet.lib odbc32.lib

#
# COMPILER OPTIONS
#

DEFS=$(DEFS)  -D_declspec=__declspec /D_WCHAR_T_DEFINED /D__MSC /D_ANONYMOUS_STRUCT -D_MSC_VER=1200 -D_WINDOWS
!if "$(DLL)" == "2"
DEFS=$(DEFS) /D_RTLDLL /D_DLL
!endif
CL_OPT= -a8 -g0 -j2 -jb -VF4 $(INCROPTS) $(DEBOPTS) $(CVOPTS) $(TARGOPTS)

#
# Internal debugging switch for the compiler (-=xx)
#
!ifdef COMPILER_DIAG
CL_OPT= $(CL_OPT) -=o
!endif

!if "$(BROWSE)" != "0"
CL_OPT=-R $(CL_OPT)
!endif

CL_OPT=-n$(D) $(CL_OPT)

!if "$(NO_WARNINGS)" == "1"
CL_OPT=$(CL_OPT) -w-
!endif

!if "$(DEVBUILD)" != "0"
CL_OPT=$(CL_OPT) /D_AFX_DEVBUILD
!endif

DEFS=$(DEFS) $(DEBDEFS) $(TARGDEFS)

#############################################################################
# Library Components

OBJECT=objcore.obj except.obj \
	validadd.obj dumpcont.obj dumpflt.obj \
	arccore.obj arcobj.obj arcex.obj arcstrm.obj

# non-shared diagnostics
OBJDIAG=dumpinit.obj dumpout.obj \
	afxasert.obj afxmem.obj afxabort.obj

FILES=filecore.obj filetxt.obj filemem.obj fileshrd.obj \
	filex.obj filest.obj

COLL1=array_b.obj array_d.obj array_p.obj array_o.obj \
	array_s.obj array_u.obj array_w.obj \
	list_o.obj list_p.obj list_s.obj

COLL2=map_pp.obj map_pw.obj map_so.obj \
	map_sp.obj map_ss.obj map_wo.obj map_wp.obj plex.obj

MISC=\
	strcore.obj strex.obj timecore.obj \
	afxdbcs.obj afxstate.obj afxtls.obj fixalloc.obj \
        mtcore.obj mtex.obj

WINDOWS=\
	wincore.obj winfrm.obj winfrm2.obj winfrmx.obj \
	winmdi.obj tooltip.obj winmini.obj winhand.obj \
	winmain.obj barcore.obj bartool.obj bardlg.obj \
	barstat.obj bardock.obj dockcont.obj dockstat.obj \
	dcprev.obj dcmeta.obj trckrect.obj barcool.obj

DIALOG=\
	winctrl1.obj winctrl2.obj winctrl3.obj winctrl4.obj \
	winbtn.obj dlgcore.obj dlgdata.obj dlgfloat.obj \
	dlgprop.obj dlgcomm.obj dlgfile.obj dlgprnt.obj \
	dlgclr.obj dlgfnt.obj dlgfr.obj ccdata.obj dlgtempl.obj \
        winctrl6.obj winctrl7.obj

WINMISC=\
	wingdi.obj wingdix.obj winstr.obj winmenu.obj \
	auxdata.obj afxcrit.obj afxtrace.obj winutil.obj winocc.obj

DOCVIEW=\
	cmdtarg.obj doccore.obj doctempl.obj \
	docsingl.obj docmulti.obj docmgr.obj \
	viewcore.obj viewprnt.obj winsplit.obj viewscrl.obj \
	viewform.obj viewedit.obj viewprev.obj viewcmn.obj \
	docmapi.obj

INTERNET=inet.obj filefind.obj
!if "$(UNICODE)" == "1"
INTERNET=$(INTERNET) isapimix.obj
!endif

APPLICATION=\
	thrdcore.obj appcore.obj appinit.obj appterm.obj \
	appui.obj appui1.obj appui2.obj appui3.obj appgray.obj \
	appdlg.obj app3d.obj appprnt.obj apphelp.obj apphelpx.obj \
	filelist.obj

!if "$(DLL)" != "2"
APPLICATION=$(APPLICATION) app3ds.obj \
	nolib.obj appmodul.obj dllmodul.obj oleexp.obj dumpstak.obj
!endif

# ODBC components:
DB=\
	dbcore.obj dbrfx.obj dbview.obj dbflt.obj \
	dblong.obj dbvar.obj

#
# DAO is not supported under Borland C++
#
#DB= $(DB) daocore.obj daodfx.obj daoview.obj viewoled.obj
#

SOCKETS=sockcore.obj

OLEREQ=olelock.obj

OLE=\
	oleinit.obj olecli1.obj olecli2.obj \
	olecli3.obj olecnvrt.obj oledobj1.obj oledobj2.obj \
	oledisp1.obj oledisp2.obj oledlgs1.obj oledlgs2.obj \
	oledlgs3.obj oledata.obj olevar.obj olevar1.obj \
	oledoc1.obj oledoc2.obj oledrop1.obj oledrop2.obj \
	olemsgf.obj oleenum.obj olefact.obj oleipfrm.obj \
	olelink.obj olemisc.obj olestrm.obj olesvr1.obj \
	olesvr2.obj olereg.obj oletsvr.obj oleui1.obj \
	oleui2.obj oleunk.obj oleverb.obj olecall.obj \
	viewrich.obj oledll.obj oletyplb.obj \
	olemon.obj winctrl5.obj viewhtml.obj \
	occmgr.obj occevent.obj occcont.obj occsite.obj \
	occlock.obj occddx.obj occddxf.obj occdlg.obj \
	oledocvw.obj oledocob.obj oledoctg.obj oledocip.obj \
	oledoccl.obj oleasmon.obj olebar.obj

OLECTL=\
	ctlcache.obj ctlcore.obj ctlconn.obj \
	ctldata.obj ctlevent.obj ctlmodul.obj \
	ctlframe.obj ctlfont.obj ctlinplc.obj \
	ctllic.obj oleconn.obj ctlobj.obj ctlpict.obj \
	ctlpropx.obj ctlppg.obj ctlprop.obj \
	ctlpset.obj ctlpstg.obj ctlpstm.obj \
	ctlrefl.obj ctlreg.obj ctltrack.obj \
	ctlview.obj olepset.obj ctlpbag.obj \
	ctlquick.obj ctlnownd.obj \
	ppgcolor.obj ppgfont.obj ppgpict.obj ppgstock.obj

!if "$(DEBUG)" == "1"
OLECTL=$(OLECTL) ctlinl.obj
!endif

# Borland enhancement:
!if "$(DEBUG)" == "1" && "$(MONOLITHIC)" == "1"
OLE=$(OLE) dllole.obj
!endif

OLEDLL=$(OLE) $(OLECTL) $(OLEASM)

!if "$(DEBUG)" == "1"
INLINES = afxinl1.obj afxinl2.obj afxinl3.obj
!else
INLINES =
!endif

CPP_OBJS=$(OBJECT) $(OBJDIAG) $(INLINES) $(FILES) $(COLL1) $(COLL2) $(MISC) \
	$(WINDOWS) $(DIALOG) $(WINMISC) $(DOCVIEW) $(APPLICATION) \
	$(SOCKETS) $(OLEREQ) $(OLE) $(DAO) $(DB) $(INTERNET) $(OLECTL)


OBJS=$(CPP_OBJS) $(OLEASM)


#############################################################################
# Standard tools

#############################################################################
# Set CPPFLAGS for use with .cpp.obj and .c.obj rules
# Define rule for use with OBJ directory
# C++ uses a PCH file

CPPFLAGS=$(CPPFLAGS) $(CL_MODEL) $(CL_OPT) $(PDBOPTS) $(DEFS) $(OPT)
BORRSP=bor.rsp

!ifndef NO_PCH
!ifndef PCH_FILE
# put the PCH file in the OBJ directory for easy cleanup later
PCH_FILE=$(D)\stdafx
!if "$(BROWSE)" != "0"
PCH_FILE=$(PCH_FILE)b
!endif
PCH_FILE=$(PCH_FILE).csm
!endif
!ifndef PCH_CPP
PCH_CPP=objcore
!endif
PCH_CMD=-Hc -H=$(PCH_FILE)
!else
PCH_CMD=-H-
PCH_FILE=
!endif
CPPFLAGS=$(CPPFLAGS) $(PCH_CMD)

!ifdef JOHNS_CPP32
PREPROCFLAGS=-Sd -Sk -Sr -P-
!else
PREPROCFLAGS=-Sd -Sk -Ss -P
!endif

.SUFFIXES: .cpp

.path.obj = $(D)


.cpp.i:
	cpp32 @$(BORRSP) $(PREPROCFLAGS) $<

.cpp.obj:
!if $d(FORCE_CPP32)
	cpp32 @$(BORRSP) $(PREPROCFLAGS) $<
!else
	$(CC) @$(BORRSP) /c { $< }
!endif

!ifndef NO_PCH
PCH_TARGETS=$(PCH_FILE) $(D)\$(PCH_CPP).obj
!endif

#############################################################################
# Goals to build

GOALS=log.mode create.dir create.rsp
!ifndef NO_PCH
GOALS=$(GOALS) $(PCH_TARGETS)
!endif
GOALS=$(GOALS) $(LIBDIR)\$(GOAL).lib

goal: $(GOALS)

log.mode:
  @-echo BORLAND.MAK: 
  @-echo              GOAL=$(GOAL), DLL=$(DLL),  DEBUG=$(DEBUG), 
  @-echo              DBGINFO=$(DBGINFO), RCDEFINES=$(RCDEFINES), 
  @-echo              CL_OPT=$(CL_OPT), DEFS=$(DEFS),
  @-echo              CPPFLAGS=$(CPPFLAGS)	 
  @-echo              D=$(D)
  @-echo              STATICLINK_OBJS=$(STATICLINK_OBJS)

create.rsp:
!ifndef NO_PCH
!if "$(NO_CLEAN_PCH)" == "0"
    @-if exist $(PCH_FILE) echo Erasing PCH file: $(PCH_FILE) for a fresh build
    @-if exist $(PCH_FILE) erase $(PCH_FILE) > nul 
!endif
!endif
    @-if exist $(BORRSP) erase $(BORRSP) > nul
    copy &&|
$(CPPFLAGS) -I$(BCINCL);$(MFCINCL)
| $(BORRSP)

create.dir:
	@-if not exist $(D) mkdir $(D)
	@-if not exist $(LIBDIR) mkdir $(LIBDIR)

clean:
	@-if exist *.map erase *.map
	@-if exist *.dll erase *.dll
	@-if exist *.cfg erase *.cfg
	@-if exist *.rsp erase *.rsp
	@-if exist *.tds erase *.tds
!if $d(INTEGRATION_BUILD)
	@-if exist $(PLATFORM)\*.def erase $(PLATFORM)\*.def
!endif
	@-if exist $$DLLD.W  rmdir $$DLLD.W /s
	@-if exist $$DLL.W rmdir $$DLL.W /s
	@-if exist $$NW rmdir $$NW /s
	@-if exist $$NWD rmdir $$NWD /s


#############################################################################
# Precompiled header file

!ifndef NO_PCH

!if "$(DEBUG)" == "1"
HDRS =$(MFCINCL)\*.h
!else
HDRS =$(MFCINCL)\*.h $(MFCINCL)\*.inl
!endif

!if "$(DLL)" != "2"
$(PCH_TARGETS): $(PCH_CPP).cpp
	$(CC) @&&!
$(CPPFLAGS) -I$(BCINCL);$(MFCINCL) /c $(PCH_CPP).cpp
!

!endif # DLL != 2
!endif # NO_PCH


#############################################################################
# Build the library from the up-to-date objs

!if "$(DLL)" != "2"
# Build final library
$(LIBDIR)\$(GOAL).lib: $(OBJS)
	@-if exist $@ erase $@
	@$(LIB32) $@ /P2048 @&&!
+-$(**: = &^
+-)
!

!endif #DLL!=2

#############################################################################
