# Makefile : Builds a Microsoft Foundation Class library variant.
#
# This is a part of the Microsoft Foundation Classes C++ library.
# Copyright (C) 1992-1998 Microsoft Corporation
# All rights reserved.
#
# This source code is only intended as a supplement to the
# Microsoft Foundation Classes Reference and related
# electronic documentation provided with the library.
# See these sources for detailed information regarding the
# Microsoft Foundation Classes product.
#
# Usage: NMAKE CLEAN        (removes all intermediary files)
#    or: NMAKE options      (builds one library variant (see below))
# Note that an NMAKE CLEAN should be performed before building a new variant.
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
#           If this item is 1, debugging support is compiled into
#           the library.  If this item is 0, then debugging support
#           is disabled.  Debug support does not include CodeView information.
#
#   "CODEVIEW"          (defaults to 1 for DEBUG=1, 0 for DEBUG=0)
#           If this item is 1 CodeView information is compiled into
#           the library.  You must use the /DEBUG:FULL and /DEBUGTYPE:cv link
#           options when linking your executable. A value of 0 indicates that
#           no CodeView information is to be generated.
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
#   "NO_PDB=1"
#           Set this item to override the default use of PDB files.
#
#   "BROWSE=1"          (defaults to 0)
#           Set this option to build the browse database for the MFC
#           library.  By setting BROWSE=1, both the .SBRs and the .BSC
#           files will be built along with the .OBJ and .LIB files that
#           are part of the normal build process.
#
#   "BROWSEONLY=1"      (defaults to 0)
#           Set this option to build the browse files without re-building
#           the MFC library itself.  Note: This option is used internally
#           when BROWSE=1 is selected.
#
#   "PLATFORM=INTEL"    (defaults depends on host)
#           This option chooses the appropriate tools and sources for the
#           different platforms supporting the Win32 API. Currently INTEL,
#           MIPS, ALPHA, PPC are supported.
#
#   "INCREMENTAL=1"     (defaults to 0)
#           This option enables incremental/minimal compilation and
#           incremental linking.
#
# Advanced Options:
#
#   "MBCS=0"            (defaults to 1)
#           To build an SBCS library instead of the default (MBCS)
#           you can use MBCS=0.  This creates a slightly smaller
#           library, but the code will not work in far-east markets.
#           This option has no effect when UNICODE=1.
#
#   "MT=0"              (defaults to 1)
#           To build a non-multithreaded library instead of the default
#           (which enables multitheading and uses the multithread
#           C-runtimes) you can use MT=0.
#
#############################################################################
# Define defaults if not defined

# Default PLATFORM depending on host environment
!ifndef PLATFORM
!ifndef PROCESSOR_ARCHITECTURE
PROCESSOR_ARCHITECTURE=x86
!endif
!if "$(PROCESSOR_ARCHITECTURE)" == "x86"
PLATFORM=INTEL
!endif
!if "$(PROCESSOR_ARCHITECTURE)" == "MIPS"
PLATFORM=MIPS
!endif
!if "$(PROCESSOR_ARCHITECTURE)" == "ALPHA"
PLATFORM=ALPHA
!endif
!if "$(PROCESSOR_ARCHITECTURE)" == "PPC"
PLATFORM=PPC
!endif
!endif

# Default to DEBUG mode
!ifndef DEBUG
DEBUG=1
!endif

# Default to NOT DLL
!ifndef DLL
DLL=0
!endif

# Default Codeview Info
!ifndef CODEVIEW
!if "$(DEBUG)" == "1"
CODEVIEW=1
!else
CODEVIEW=0
!endif
!endif

# BROWSEONLY is default 0 and implies BROWSE=1 if BROWSEONLY=1
!ifndef BROWSEONLY
BROWSEONLY=0
!endif

!if "$(BROWSEONLY)" != "0"
!undef BROWSE
BROWSE=1
!endif

# Default to no BROWSE info
!ifndef BROWSE
BROWSE=0
!endif

# Default to no INCREMENTAL build
!ifndef DEVBUILD
DEVBUILD=0
!endif
!if "$(DEBUG)" != "0"
!ifndef INCREMENTAL
INCREMENTAL=$(DEVBUILD)
!endif
!endif
!ifndef INCREMENTAL
INCREMENTAL=0
!endif

# Default to _MBCS build
!ifndef MBCS
MBCS=1
!endif

# Default to multithreading support
!ifndef MT
MT=1
!endif

#############################################################################
# normalize cases of parameters, or error check

!if "$(CPU)" == "MIPS"
!if "$(PLATFORM)" != "MIPS"
!error Must set PLATFORM=MIPS for MIPS builds
!endif
!endif

!if "$(CPU)" == "ALPHA"
!if "$(PLATFORM)" != "ALPHA"
!error Must set PLATFORM=ALPHA for ALPHA builds
!endif
!endif

BASE=W

#############################################################################
# Parse options

#
# DEBUG OPTIONS
#
!if "$(DEBUG)" != "0"

DEBUGSUF=D
DEBDEFS=/D_DEBUG
DEBOPTS=/Od

!endif

#
# NON-DEBUG OPTIONS
#
!if "$(DEBUG)" == "0"

DEBUGSUF=
DEBDEFS=

!if "$(PLATFORM)" == "INTEL"
DEBOPTS=/O1 /GyF
!endif
!if "$(PLATFORM)" == "MIPS"
DEBOPTS=/O1 /GyF
!endif
!if "$(PLATFORM)" == "ALPHA"
DEBOPTS=/O1 /GyF
!endif
!if "$(PLATFORM)" == "PPC"
DEBOPTS=/O1 /GyF
!endif

!endif

#
# PLATFORM options
#

!if "$(PLATFORM)" == "INTEL"
CL_MODEL=/D_X86_
!endif

!if "$(PLATFORM)" == "MIPS"
CL_MODEL=/D_MIPS_
!endif

!if "$(PLATFORM)" == "ALPHA"
CL_MODEL=/D_ALPHA_
!endif

!if "$(PLATFORM)" == "PPC"
CL_MODEL=/D_PPC_
!endif

!if "$(CL_MODEL)" == ""
!error PLATFORM must be one of INTEL, MIPS, ALPHA, or PPC.
!endif

# TYPE = Library Type Designator
#       c = normal C library
#       d = DLL library
TYPE=c
DEXT=

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
# _AFXDLL library
TYPE=e
!if "$(OBJ)" == ""
D=DLL$(DEBUGSUF).$(BASE)
!if "$(UNICODE)" == "1"
D=$(MODEL)$D
!endif
D=$$$D
!endif
TARGOPTS=$(TARGOPTS) /MD /D_DLL /GF
!if "$(MT)" != "0"
TARGOPTS=$(TARGOPTS) /D_MT
!endif
TARGDEFS=$(TARGDEFS) /D_WINDLL /D_AFXDLL
!else
# not _AFXDLL library
!if "$(MD)" == "1"
TARGOPTS=$(TARGOPTS) /MD
!else
!if "$(MT)" != "0"
TARGOPTS=$(TARGOPTS) /MT
!endif
!endif
!endif

!if "$(UNICODE)" == "1"
MODEL=U
TARGDEFS=$(TARGDEFS) /D_UNICODE
!else
MODEL=N
!if "$(MBCS)" != "0"
TARGDEFS=$(TARGDEFS) /D_MBCS
!endif
!endif

!if "$(DLL)" == "2" && "$(BROWSEONLY)" != "1"
!if "$(TARG)" == ""
!error DLL=2 is used only from MFCDLL.MAK, MFCOLE.MAK, or MFCDB.MAK
!endif
GOAL=$(TARG)
!else
GOAL=$(MODEL)afx$(TYPE)$(BASE)$(DEBUGSUF)
!endif

#
# CODEVIEW options
#
!if "$(CODEVIEW)" == "1"
!if "$(NO_PDB)" == "1"
CVOPTS=/Z7
!if "$(PROFLIB)" != ""
!error Can't build for profiling without PDB files.
!endif
!else
CVOPTS=/Zi
!if "$(PROFLIB)" != ""
CVOPTS=$(CVOPTS) /Gh
!endif
!if "$(DLL)" == "2"
PDBOPTS=/Fd$(GOAL).pdb
!else
PDBOPTS=/Fd..\lib\$(GOAL).pdb
!endif
!endif
!endif

#
# INCREMENTAL options
#
!if "$(INCREMENTAL)" == "1"
INCROPTS=/Gi /Gm
!else
INCROPTS=/Gi- /Gm-
!endif

#
# COMPILER OPTIONS
#
!if "$(PLATFORM)" == "INTEL"
CL_OPT=/W4 /WX /Zl /GX /GR $(INCROPTS) $(DEBOPTS) $(CVOPTS) $(TARGOPTS)
!endif

!if "$(PLATFORM)" == "MIPS"
CL_OPT=/W4 /WX /Zl /GX /GR $(INCROPTS) $(DEBOPTS) $(CVOPTS) $(TARGOPTS)
!endif

!if "$(PLATFORM)" == "ALPHA"
CL_OPT=/W4 /WX /Zl /GX /GR $(INCROPTS) $(DEBOPTS) $(CVOPTS) $(TARGOPTS)
!endif

!if "$(PLATFORM)" == "PPC"
CL_OPT=/W4 /WX /Zl /GX /GR $(INCROPTS) $(DEBOPTS) $(CVOPTS) $(TARGOPTS)
!endif

!if "$(DEVBUILD)" != "0"
CL_OPT=$(CL_OPT) /D_AFX_DEVBUILD
!endif

!if "$(BROWSE)" != "0"
CL_OPT=/FR$D\ $(CL_OPT)
!endif

!if "$(BROWSEONLY)" != "0"
CL_OPT=/Zs $(CL_OPT)
!else
CL_OPT=/Fo$D\ $(CL_OPT)
!endif

DEFS=$(DEFS) $(DEBDEFS) $(TARGDEFS)

#############################################################################
# Library Components

OBJECT=$D\objcore.obj $D\except.obj \
	$D\validadd.obj $D\dumpcont.obj $D\dumpflt.obj \
	$D\arccore.obj $D\arcobj.obj $D\arcex.obj \
		$D\arcstrm.obj

# non-shared diagnostics
OBJDIAG=$D\dumpinit.obj $D\dumpout.obj \
	$D\afxasert.obj $D\afxmem.obj $D\afxabort.obj

FILES=$D\filecore.obj $D\filetxt.obj $D\filemem.obj $D\fileshrd.obj \
	$D\filex.obj $D\filest.obj

COLL1=$D\array_b.obj $D\array_d.obj $D\array_p.obj $D\array_o.obj \
	$D\array_s.obj $D\array_u.obj $D\array_w.obj \
	$D\list_o.obj $D\list_p.obj $D\list_s.obj

COLL2=$D\map_pp.obj $D\map_pw.obj $D\map_so.obj \
	$D\map_sp.obj $D\map_ss.obj $D\map_wo.obj $D\map_wp.obj $D\plex.obj

MISC=\
	$D\strcore.obj $D\strex.obj $D\timecore.obj \
	$D\afxdbcs.obj $D\afxstate.obj $D\afxtls.obj $D\fixalloc.obj \
	$D\mtcore.obj $D\mtex.obj

WINDOWS=\
	$D\wincore.obj $D\winfrm.obj $D\winfrm2.obj $D\winfrmx.obj \
	$D\winmdi.obj $D\tooltip.obj $D\winmini.obj $D\winhand.obj \
	$D\winmain.obj $D\barcore.obj $D\bartool.obj $D\bardlg.obj \
	$D\barstat.obj $D\bardock.obj $D\dockcont.obj $D\dockstat.obj \
		$D\dcprev.obj $D\dcmeta.obj $D\trckrect.obj $D\barcool.obj

DIALOG=\
	$D\winctrl1.obj $D\winctrl2.obj $D\winctrl3.obj $D\winctrl4.obj \
	$D\winbtn.obj $D\dlgcore.obj $D\dlgdata.obj $D\dlgfloat.obj \
	$D\dlgprop.obj $D\dlgcomm.obj $D\dlgfile.obj $D\dlgprnt.obj \
	$D\dlgclr.obj $D\dlgfnt.obj $D\dlgfr.obj $D\ccdata.obj \
	$D\dlgtempl.obj $D\winctrl6.obj $D\winctrl7.obj

WINMISC=\
	$D\wingdi.obj $D\wingdix.obj $D\winstr.obj $D\winmenu.obj \
	$D\auxdata.obj $D\afxcrit.obj $D\afxtrace.obj $D\winutil.obj \
	$D\winocc.obj

DOCVIEW=\
	$D\cmdtarg.obj $D\doccore.obj $D\doctempl.obj \
	$D\docsingl.obj $D\docmulti.obj $D\docmgr.obj \
	$D\viewcore.obj $D\viewprnt.obj $D\winsplit.obj $D\viewscrl.obj \
	$D\viewform.obj $D\viewedit.obj $D\viewprev.obj $D\viewcmn.obj \
	$D\docmapi.obj

INTERNET=$D\inet.obj $D\filefind.obj
!if "$(UNICODE)" != "1"
INTERNET=$(INTERNET) $D\isapimix.obj
!endif

APPLICATION=\
	$D\thrdcore.obj $D\appcore.obj $D\appinit.obj $D\appterm.obj \
	$D\appui.obj $D\appui1.obj $D\appui2.obj $D\appui3.obj $D\appgray.obj \
	$D\appdlg.obj $D\app3d.obj $D\appprnt.obj $D\apphelp.obj $D\apphelpx.obj \
	$D\filelist.obj

!if "$(DLL)" != "2"
APPLICATION=$(APPLICATION) $D\app3ds.obj \
	$D\nolib.obj $D\appmodul.obj $D\dllmodul.obj $D\oleexp.obj $D\dumpstak.obj
!endif

DB=\
	$D\dbcore.obj $D\dbrfx.obj $D\dbview.obj $D\dbflt.obj \
	$D\dblong.obj $D\dbvar.obj \
	$(DB) $D\daocore.obj $D\daodfx.obj $D\daoview.obj $D\viewoled.obj

SOCKETS=$D\sockcore.obj

OLEREQ=$D\olelock.obj

OLE=\
	$D\oleinit.obj $D\olecli1.obj $D\olecli2.obj \
	$D\olecli3.obj $D\olecnvrt.obj $D\oledobj1.obj $D\oledobj2.obj \
	$D\oledisp1.obj $D\oledisp2.obj $D\oledlgs1.obj $D\oledlgs2.obj \
	$D\oledlgs3.obj $D\oledata.obj $D\olevar.obj $D\olevar1.obj \
	$D\oledoc1.obj $D\oledoc2.obj $D\oledrop1.obj $D\oledrop2.obj \
	$D\olemsgf.obj $D\oleenum.obj $D\olefact.obj $D\oleipfrm.obj \
	$D\olelink.obj $D\olemisc.obj $D\olestrm.obj $D\olesvr1.obj \
	$D\olesvr2.obj $D\olereg.obj $D\oletsvr.obj $D\oleui1.obj \
	$D\oleui2.obj $D\oleunk.obj $D\oleverb.obj $D\olecall.obj \
	$D\viewrich.obj $D\oledll.obj $D\oletyplb.obj \
	$D\olemon.obj $D\winctrl5.obj $D\viewhtml.obj \
	$D\occmgr.obj $D\occevent.obj $D\occcont.obj $D\occsite.obj \
	$D\occlock.obj $D\occddx.obj $D\occddxf.obj $D\occdlg.obj \
	$D\oledocvw.obj $D\oledocob.obj $D\oledoctg.obj $D\oledocip.obj \
	$D\oledoccl.obj $D\oleasmon.obj $D\olebar.obj

OLECTL=\
	$D\ctlcache.obj $D\ctlcore.obj $D\ctlconn.obj \
	$D\ctldata.obj $D\ctlevent.obj $D\ctlmodul.obj \
	$D\ctlframe.obj $D\ctlfont.obj $D\ctlinplc.obj \
	$D\ctllic.obj $D\oleconn.obj $D\ctlobj.obj $D\ctlpict.obj \
	$D\ctlpropx.obj $D\ctlppg.obj $D\ctlprop.obj \
	$D\ctlpset.obj $D\ctlpstg.obj $D\ctlpstm.obj \
	$D\ctlrefl.obj $D\ctlreg.obj $D\ctltrack.obj \
	$D\ctlview.obj $D\olepset.obj $D\ctlpbag.obj \
	$D\ctlquick.obj $D\ctlnownd.obj \
	$D\ppgcolor.obj $D\ppgfont.obj $D\ppgpict.obj $D\ppgstock.obj

!if "$(DEBUG)" == "1"
OLECTL=$(OLECTL) $D\ctlinl.obj
!endif

!if "$(PLATFORM)" == "ALPHA"
!if "$(DEBUG)" == "1"
OLEASM=.\alpha\olecalld.obj
!else
OLEASM=.\alpha\olecalln.obj
!endif
!endif

!if "$(PLATFORM)" == "PPC"
!if "$(DEBUG)" == "1"
OLEASM=.\ppc\olecalld.obj
!else
OLEASM=.\ppc\olecalln.obj
!endif
!endif

OLEDLL=$(OLE) $(OLECTL) $(OLEASM)

!if "$(DEBUG)" == "1"
INLINES = $D\afxinl1.obj $D\afxinl2.obj $D\afxinl3.obj
!else
INLINES =
!endif

CPP_OBJS=$(OBJECT) $(OBJDIAG) $(INLINES) $(FILES) $(COLL1) $(COLL2) $(MISC) \
	$(WINDOWS) $(DIALOG) $(WINMISC) $(DOCVIEW) $(APPLICATION) \
	$(SOCKETS) $(OLEREQ) $(OLE) $(DAO) $(DB) $(INTERNET) $(OLECTL)

!if "$(BROWSEONLY)" == "1" && "$(DLL)" == "2"
TARGDEFS=$(TARGDEFS) /D_AFX_CORE_IMPL /D_AFX_OLE_IMPL /D_AFX_DB_IMPL /D_AFX_NET_IMPL
!endif

OBJS=$(CPP_OBJS) $(OLEASM)


#############################################################################
# Standard tools

#############################################################################
# Set CPPFLAGS for use with .cpp.obj and .c.obj rules
# Define rule for use with OBJ directory
# C++ uses a PCH file

CPPFLAGS=$(CPPFLAGS) $(CL_MODEL) $(CL_OPT) $(PDBOPTS) $(DEFS) $(OPT)

!ifndef NO_PCH
!ifndef PCH_FILE
PCH_FILE=$D\stdafx
!if "$(BROWSE)" != "0"
PCH_FILE=$(PCH_FILE)b
!endif
PCH_FILE=$(PCH_FILE).pch
!endif
!ifndef PCH_CPP
PCH_CPP=objcore
!endif

CPPFLAGS=$(CPPFLAGS) /Yustdafx.h /Fp$(PCH_FILE)
!else
PCH_FILE=
!endif

.SUFFIXES:: .cpp

!if "$(BROWSE)" != "0"
.cpp{$D}.obj:
	cl @<<
$(CPPFLAGS) /c $<
<<
	copy /b $*.sbr+pchmark.bin $*.sbr >NUL
!else
.cpp{$D}.obj::
	cl @<<
$(CPPFLAGS) /c $<
<<
!endif

.cpp{$D}.sbr:
	cl @<<
$(CPPFLAGS) /c $<
<<
	copy /b $*.sbr+pchmark.bin $*.sbr >NUL

#############################################################################
# Goals to build

GOALS=create.dir
!if "$(BROWSEONLY)" == "0"
GOALS=$(GOALS) ..\lib\$(GOAL).lib
!endif
!if "$(BROWSE)" != "0"
GOALS=$(GOALS) $(GOAL).bsc
!endif

goal: $(GOALS)

create.dir:
	@-if not exist $D\*.* mkdir $D

clean:
	-if exist $D\*.obj erase $D\*.obj
	-if exist $D\*.pch erase $D\*.pch
	-if exist $D\*.res erase $D\*.res
	-if exist $D\*.rsc erase $D\*.rsc
	-if exist $D\*.map erase $D\*.map
	-if not exist $D\*.* rmdir $D
	-if exist ..\lib\$(GOAL).pdb del ..\lib\$(GOAL).pdb
	-if exist ..\lib\$(GOAL).idb del ..\lib\$(GOAL).idb
	-if exist ..\lib\$(GOAL).rep del ..\lib\$(GOAL).rep
	-if exist $(GOAL).pdb del $(GOAL).pdb
	-if exist $(GOAL).idb del $(GOAL).idb
	-if exist $(GOAL).rep del $(GOAL).rep

#############################################################################
# Precompiled header file

!ifndef NO_PCH

!if "$(DEBUG)" == "1"
HDRS =..\include\*.h
!else
HDRS =..\include\*.h ..\include\*.inl
!endif

PCH_TARGETS=$(PCH_FILE) $D\$(PCH_CPP).obj
!if "$(BROWSEONLY)" != "0"
PCH_TARGETS=$(PCH_TARGETS) $D\$(PCH_CPP).sbr
!endif

$(PCH_TARGETS):: $(PCH_CPP).cpp $(HDRS)
	cl @<<
/Ycstdafx.h /Fp$(PCH_FILE) $(CL_MODEL) $(CL_OPT) $(PDBOPTS) $(DEFS) $(OPT) /c $(PCH_CPP).cpp
<<
!if "$(BROWSE)" != "0"
	copy /b $D\$(PCH_CPP).sbr+pchmark.bin $D\$(PCH_CPP).sbr>NUL
!endif

!if "$(BROWSEONLY)" == "1"
$D\$(PCH_CPP).sbr:: $(PCH_CPP).cpp $(PCH_FILE)
!endif

!endif # NO_PCH

#############################################################################
## PLATFORM=ALPHA specific target(s)

!if "$(PLATFORM)" == "ALPHA"

!if "$(DEBUG)" == "1"
ASMOPT=$(ASMOPT) /D_DEBUG /g2
!else
ASMOPT=$(ASMOPT)
!endif

$(OLEASM) : alpha\olecall_.s
	asaxp $(ASMOPT) -o $@ alpha\olecall_.s

!endif

#############################################################################
## PLATFORM=PPC specific target(s)

!if "$(PLATFORM)" == "PPC"

!if "$(DEBUG)" == "1"
ASMOPT=$(ASMOPT)
!else
ASMOPT=$(ASMOPT)
!endif

$(OLEASM) : ppc\olecall_.s
	pas $(ASMOPT) -o $@ ppc\olecall_.s

!endif

#############################################################################
# Build the library from the up-to-date objs

SBRS=$(CPP_OBJS:.obj=.sbr)

!if "$(BROWSEONLY)" != "0"

# Build final browse database
$(GOAL).bsc: $(PCH_TARGETS) $(SBRS)
	bscmake /n /Iu /El /o$@ @<<
$(SBRS)
<<

!else #BROWSEONLY

!if "$(DLL)" != "2"
# Build final library
..\lib\$(GOAL).lib: $(PCH_TARGETS) $(OBJS)
	@-if exist $@ erase $@
	@lib /out:$@ @<<
$(OBJS)
<<

# Recurse to build browse database
$(GOAL).bsc: $(PCH_TARGETS) $(SBRS)
	$(MAKE) /f makefile. @<<
BROWSEONLY=1 PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) CODEVIEW=$(CODEVIEW) \
DLL=$(DLL) NO_PCH=$(NO_PCH) OBJ=$(OBJ) OPT=$(OPT)
<<
!endif #DLL!=2

!endif #!BROWSEONLY

#############################################################################
