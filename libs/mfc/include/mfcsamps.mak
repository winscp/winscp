# This is a part of the Microsoft Foundation Classes C++ library.
# Copyright (C) 1992-1995 Microsoft Corporation
# All rights reserved.
#
# This source code is only intended as a supplement to the
# Microsoft Foundation Classes Reference and related
# electronic documentation provided with the library.
# See these sources for detailed information regarding the
# Microsoft Foundation Classes product.

# Common include for building MFC Sample programs
#
#  typical usage
#       PROJ=foo
#       OBJS=foo.obj bar.obj ...
#       !INCLUDE ..\..\SAMPLE_.MAK
#
#  ROOT specifies the location of the msdev\samples\mfc directory,
#  relative to the project directory. Because the MFC tutorial samples
#  have an intermediate STEP<n> subdirectory, they use
#       ROOT=..\..\..
#  instead of the default
#       ROOT=..\..
#
# Options to NMAKE:
#     "DEBUG=0"     use retail (default debug)
#     "CODEVIEW=1"  include codeview info (even for retail builds)
#         "AFXDLL=1"    to use shared DLL version of MFC
#         "USRDLL=1"    to build a DLL that uses static MFC
#############################################################################
#  Borland specific options:
#
#  "USRSTARTUP=xxx" used to link with a specific startup code .OBJ
#                   (by default it uses the correct one for the specified
#                    target).
#
#  "DBGINFO=1"      is used to include source debugging information.
#
#  "NO_RESOURCE=1"  is set to not attempt to bind in resources.
#
#  "NO_PCH=1"       is set to prevent the use of pre-compiled headers
#                   (default is to use them)
#
#  "NO_CLEAN_PCH"   To prevent deleting an existing PCH (.csm) file
#
#  LINKFLAGS and CFLAGS can be set to contain additional or overriding option
#  for the linker or compiler
#
#

!ifndef PROJ
!ERROR You forgot to define 'PROJ' symbol!!
!endif

ROOT=.

!ifndef OBJS
!ERROR You forgot to define 'OBJS' symbol!!
!endif

!ifndef DEBUG
DEBUG=0
!endif

!ifndef DBGINFO
DBGINFO=0
!endif

!ifdef CODEVIEW
DBGINFO=$(CODEVIEW)
!endif

!ifndef AFXDLL
AFXDLL=0
!endif

!ifndef UNICODE
UNICODE=0
!endif

!ifndef USRDLL
USRDLL=0
!endif

!ifdef USRSTARTUP
STARTUP=$(USRSTARTUP)
!endif

!ifndef NO_RESOURCE
NO_RESOURCE=0
!endif

!ifndef NO_PCH
NO_PCH=0
!endif

!ifndef NO_CLEAN_PCH
NO_CLEAN_PCH=0
!endif

!if "$(SIMPLE_APP)" == "1"
  NO_RESOURCE=1
!endif

!ifndef USES_OLE
USES_OLE=0
!endif

!ifndef USES_DB
USES_DB=0
!endif

!ifndef CONSOLE
CONSOLE=0
!endif

#
# Set BASE=W, M, or P depending on platform
#
BASE=W

!if "$(UNICODE)" == "0"
!if "$(AFXDLL)" == "0"
!if "$(USRDLL)" != "1"
STDAFX=stdafx
!else
STDAFX=stdusr
!endif
!else
STDAFX=stddll
!endif
!endif

!if "$(UNICODE)" == "1"
!if "$(AFXDLL)" == "0"
!if "$(USRDLL)" != "1"
STDAFX=uniafx
!else
STDAFX=uniusr
!endif
!else
STDAFX=unidll
!endif
!endif

!if "$(DEBUG)" == "1"
STDAFX=$(STDAFX)d
!endif

!if "$(DEBUG)" == "1"
DEBUG_SUFFIX=d
!endif

!if "$(DEBUG)" != "0"
DEBUGFLAGS=/Od
MFCDEFS=$(MFCDEFS) /D_DEBUG


!endif

!if "$(DEBUG)" == "0"
DEBUGFLAGS=/O1 
!endif # DEBUG == 0

!if "$(DBGINFO)" == "1"
DEBUGFLAGS=$(DEBUGFLAGS) -v -vi -r-
LINKDEBUG=/v
!endif

!if "$(UNICODE)" == "1"
DLL_SUFFIX=u
!endif

MFCDEFS = $(MFCDEFS) /D_NO_VCL

!if "$(AFXDLL)" == "1"
MFCDEFS=$(MFCDEFS) /D_RTLDLL;_AFXDLL;_X86_;_WINDOWS;_MSC_VER=1200;_AFX_PORTABLE;_AFX_NOFORCE_LIBS;_CRT_PORTABLE;_AFX_NO_DEBUG_CRT;
!else
MFCDEFS=$(MFCDEFS) /D_X86_;_WINDOWS;_MSC_VER=1100;_AFX_PORTABLE;_AFX_NOFORCE_LIBS;_CRT_PORTABLE;_AFX_NO_DEBUG_CRT;
!endif

!if "$(USRDLL)" == "1"
MFCDEFS=$(MFCDEFS) /D_USRDLL /D_WINDLL
!if "$(CONSOLE)" == "1"
MFCFLAGS=$(MFCFLAGS) -WD
!else
MFCFLAGS=$(MFCFLAGS) -WD
!endif
!else
!if "$(CONSOLE)" == "1"
MFCFLAGS=$(MFCFLAGS) -WC
!else
MFCFLAGS=$(MFCFLAGS) -W
!endif
!endif

!if "$(UNICODE)" == "1"
MFCDEFS=$(MFCDEFS) /D_UNICODE
!endif

!if !$d(PREPROCESS_ONLY)
CPP=bcc32
!else
CPP=cpp32 -Sd -Sk
!endif

BORINCL=$(MAKEDIR)\..\include;$(MAKEDIR)\..\include\mfc;$(MAKEDIR)\..\include\atl;$(BORINCL)
CFLAGS=-X -c -g0 -j1 -w-hid -w-par -w-aus -w-inl -VF -Vd -tWM -I$(BORINCL) $(DEBUGFLAGS) $(MFCFLAGS) $(MFCDEFS) $(CFLAGS)
CPPMAIN_FLAGS=$(CFLAGS)

!if "$(NO_PCH)" == "1"
CPPFLAGS=$(CPPMAIN_FLAGS)
!else
PCHDIR=.
PCH_FILE=$(PCHDIR)\$(STDAFX).csm
CPPFLAGS=$(CPPMAIN_FLAGS) -H -Hc -H=$(PCH_FILE)
!endif

BORLIB=$(MAKEDIR)\..\lib;$(BORLIB)
LINKFLAGS= /Gn /n /m /s $(LINKDEBUG) /L$(BORLIB);$(OTHERLIBPATH) $(LINKFLAGS)
LINKCMD=ilink32
!if "$(USES_OLE)" == "1"
  OTHERLIBS=ole2w32.lib $(OTHERLIBS)
!endif

!if "$(USES_DB)" == "1"
  OTHERLIBS=odbc32.lib $(OTHERLIBS)
!endif

!if "$(AFXDLL)" == "1"
!if "$(DEBUG)" == "1"
#LIBRARIES=BFC42D.lib BFCS42D.lib bfcd42d.lib bfco42d.lib bfcn42d.lib $(OTHERLIBS) cw32mti.lib import32.lib
LIBRARIES=BFC42D.lib BFCS42D.lib $(OTHERLIBS) cw32mti.lib import32.lib
!else
LIBRARIES=BFC42.lib BFCS42.lib $(OTHERLIBS) cw32mti.lib import32.lib stlpmti.lib
!endif
!else
LIBRARIES=nafxcw$(DEBUG_SUFFIX).lib $(OTHERLIBS) cw32mt.lib import32.lib stlpmt.lib
!endif
RC=brc32

# link flags - must be specified after $(LINKCMD)
#
# conflags : creating a character based console application
# guiflags : creating a GUI based "Windows" application

#CONFLAGS=/subsystem:console
#GUIFLAGS=/subsystem:windows
CONFLAGS=/ap
GUIFLAGS=/aa

BORRSP=bor.rsp

#.SUFFIXES:
.SUFFIXES: .c .cpp .rc

.cpp.obj:
	$(CPP) @$(BORRSP) { $< }

.c.obj:
	$(CPP) @$(BORRSP) { $< }

.rc.res:
	$(RC) -I$(BORINCL) -r @&&~
$(MFCDEFS) /fo $@ $<
~
#############################################################################

LINK_OBJS=$(OBJS)

!if "$(CONSOLE)" == "1"
LINKFLAGS=$(LINKFLAGS) $(CONFLAGS)
!ifndef USRSTARTUP
STARTUP=$(BORLIB)\c0x32.obj
!endif
!else
LINKFLAGS=$(LINKFLAGS) $(GUIFLAGS)
!ifndef USRSTARTUP
STARTUP=$(BORLIB)\c0w32.obj
!endif
!endif

!if "$(USRDLL)" == "1"
!ifndef USRSTARTUP
STARTUP=$(BORLIB)\c0d32.obj
!endif
TARGET=$(PROJ).DLL
!else
TARGET=$(PROJ).EXE
!endif

MainRule: create.rsp build.pch $(TARGET)

create.rsp:
!if "$(NO_PCH)" != "1"
!if "$(NO_CLEAN_PCH)" == "0"
!ifdef PCH_FILE
     @-if exist $(PCH_FILE) echo Erasing PCH file: $(PCH_FILE) for a fresh build
     @-if exist $(PCH_FILE) erase $(PCH_FILE) > nul
!endif
!endif
!endif
     @-if exist $(BORRSP) erase $(BORRSP) > nul
     copy &&|
$(CPPFLAGS)
| $(BORRSP)

build.pch:
!if "$(NO_PCH)" != "1"
    @-echo Checking / building precompiled headers:
    @-if exist stdafx.cpp if not exist $(PCH_FILE) $(CPP) @$(BORRSP) stdafx.cpp
!else
    @-echo NOT using Precompiled Headers.
!endif

!if "$(SIMPLE_APP)" == "1"
$(TARGET): $(LINK_OBJS)
	$(LINKCMD) /Tpe @&&~
$(LINKFLAGS) +
$(STARTUP) $(LINK_OBJS)
$(TARGET)
$(PROJ).map
$(EXTRA_LIBS) $(LIBRARIES)

~
!else

!if "$(USRDLL)" == "0"
!if "$(NO_RESOURCE)" == "1"
$(TARGET): $(LINK_OBJS)
!else
$(TARGET): $(LINK_OBJS) $(PROJ).res
!endif
	$(LINKCMD) /Tpe @&&~
$(LINKFLAGS) +
$(STARTUP) $(LINK_OBJS)
$(TARGET)
$(PROJ).map
$(EXTRA_LIBS) $(LIBRARIES)

!if "$(NO_RESOURCE)" != "1"
$(PROJ).res
!endif
~
!else
!if "$(NO_RESOURCE)" == "1"
$(TARGET): $(LINK_OBJS)
!else
$(TARGET): $(LINK_OBJS) $(PROJ).res
!endif
	$(LINKCMD) /Tpd @&&~
$(LINKFLAGS) +
$(STARTUP) $(LINK_OBJS)
$(TARGET)
$(PROJ).map
$(EXTRA_LIBS) $(LIBRARIES)
$(PROJ).def
!if "$(NO_RESOURCE)" != "1"
$(PROJ).res
!endif
~

!endif # userdll
!endif # simple_app

#  This rule is for testing.
#  Useage:
#  MAKE {put any make options here} printdebuginfo
#
printdebuginfo:
        @-echo List of macros:
        @-type &&~
  USRDLL    = $(USRDLL)
  AFXDLL    = $(AFXDLL)
  CONSOLE   = $(CONSOLE)
  PROJ      = $(PROJ)
  STARTUP   = $(STARTUP)
  LIBRARIES = $(LIBRARIES)
  LIBDIR    = $(BORLIB)
  INCLDIR   = $(BORINCL)
  LINKFLAGS = $(LINKFLAGS)
  CFLAGS    = $(CFLAGS)
~


clean::
	@if exist $(PROJ).exe erase $(PROJ).exe
	@if exist $(PROJ).dll erase $(PROJ).dll
	@if exist *.?0? erase *.?0?
	@if exist *.csm erase *.csm
	@if exist *.map erase *.map
	@if exist *.obj erase *.obj
	@if exist *.exp erase *.exp
	@if exist *.map erase *.map
	@if exist *.lib erase *.lib
	@if exist *.res erase *.res
	@if exist *.tds erase *.tds
	@if exist *.il? erase *.il?
	@if exist $(BORRSP) erase $(BORRSP)

#############################################################################
