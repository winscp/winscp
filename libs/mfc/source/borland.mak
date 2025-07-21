!include ../../Makefile.inc

# BORLAND.MAK: Borland makefile for MFC variants
#
# Usage: MAKE -F BORLAND.MAK CLEAN        (removes all intermediary files)
#    or: MAKE -F BORLAND.MAK options      (builds library variant (see below))
# Note that an MAKE CLEAN should be performed before building a new variant.
#
# 'Options' are one of each of:
#   "DEBUG"             (defaults to 1)
#           If this item is 1, diagnostic support is compiled into
#           the library.  If this item is 0, then diagnostic support
#           is disabled. Diagnostic support does not include debug info
#           information.
#
#############################################################################
# Define defaults if not defined
!ifndef DEBUG
DEBUG=0
!endif

# Lib Path
LPATH=..\..\lib;..\..\lib\psdk

# MFC Include directory base
MFCINCL=..\include

# BC Include directory
BCINCL=..\..\include;..\..\include\atl

BASE=W
MODEL=U
TARGDEFS=_UNICODE;UNICODE;WINSCP
TARGDEFS=$(TARGDEFS);_AFX_NO_OLEDB_SUPPORT;_AFX_NO_OCC_SUPPORT;_AFX_NO_OCX_SUPPORT;_AFX_NO_DB_SUPPORT
TARGDEFS=$(TARGDEFS);_AFX_NO_AFXCMN_SUPPORT;_AFX_NO_RICHEDIT_SUPPORT;_AFX_NO_INET_SUPPORT;_AFX_NO_DHTML_SUPPORT
TARGDEFS=$(TARGDEFS);_AFX_NO_OLE_SUPPORT;_AFX_NO_SOCKET_SUPPORT;_AFX_NO_SYNC_SUPPORT
TARGDEFS=$(TARGDEFS);_MBCS;_MT=1;_X86_

LFLAGS=$(LFLAGS) /j$(LPATH) /L$(LPATH)
LFLAGS=$(LFLAGS) /w-

#
# DEBUG OPTIONS
#
!if "$(DEBUG)" != "0"

DEBUGSUF=D
DEBDEFS=;_DEBUG

!else

#
# NON-DEBUG OPTIONS
#

DEBUGSUF=
DEBDEFS=

!endif

#
# Object File Directory
#
D=$$$(MODEL)$(BASE)$(DEBUGSUF)    # subdirectory specific to variant

GOAL=$(MODEL)afxc$(BASE)$(DEBUGSUF)

#
# COMPILER OPTIONS
#
# -VF4 Support MFC 4.0
CL_OPT=-VF4 -n$(D) -w-

DEFS=_declspec=__declspec;_WCHAR_T_DEFINED;__MSC;_ANONYMOUS_STRUCT;_MSC_VER=1200;_WINDOWS
DEFS=$(DEFS)$(DEBDEFS);$(TARGDEFS)

#############################################################################
# Library Components

OBJECT = objcore.obj except.obj validadd.obj
	
FILES = filecore.obj filex.obj filest.obj

MISC = strcore.obj strex.obj timecore.obj fixalloc.obj plex.obj

WINMISC = winstr.obj apphelp.obj 

!if "$(DEBUG)" == "1"
INLINES = afxinl1.obj afxinl2.obj afxinl3.obj
!else
INLINES =
!endif

CPP_OBJS=$(OBJECT) $(INLINES) $(FILES)  $(MISC) $(WINMISC)

OBJS=$(CPP_OBJS)


#############################################################################
# Standard tools

#############################################################################
# Set CPPFLAGS for use with .cpp.obj and .c.obj rules
# Define rule for use with OBJ directory

CPPFLAGS=$(CFLAG_COMMON) $(CL_OPT) -D$(DEFS:;= -D)

INCL=$(BCINCL);$(MFCINCL)

BORFLAGS=$(CPPFLAGS) -I$(INCL:;= -I)

.SUFFIXES: .cpp

.path.obj = $(D)

.cpp.obj:
	$(CC) $(BORFLAGS) { $< }

#############################################################################
# Goals to build

LIB=$(LIB_PATH)\$(GOAL).lib
GOALS=log.mode create.dir $(LIB)

goal: $(GOALS)

log.mode:
  @-echo BORLAND.MAK: 
  @-echo              GOAL=$(GOAL), DEBUG=$(DEBUG), 
  @-echo              CL_OPT=$(CL_OPT)
  @-echo              DEFS=$(DEFS),
  @-echo              CPPFLAGS=$(CPPFLAGS)	 
  @-echo              D=$(D)

create.dir:
	@-if not exist $(D) mkdir $(D)
	@-if not exist $(LIB_PATH) mkdir $(LIB_PATH)

clean:
	@-if exist *.map erase *.map
	@-if exist *.dll erase *.dll
	@-if exist *.cfg erase *.cfg
	@-if exist *.rsp erase *.rsp
	@-if exist *.tds erase *.tds
	@-if exist $$DLLD.W  rmdir $$DLLD.W /s
	@-if exist $$DLL.W rmdir $$DLL.W /s
	@-if exist $$NW rmdir $$NW /s
	@-if exist $$NWD rmdir $$NWD /s


#############################################################################
# Precompiled header file

!if "$(DEBUG)" == "1"
HDRS =$(MFCINCL)\*.h
!else
HDRS =$(MFCINCL)\*.h $(MFCINCL)\*.inl
!endif

#############################################################################
# Build the library from the up-to-date objs

# Build final library
$(LIB): $(OBJS)
	$(MKLIB) $@ $(**)

#############################################################################
