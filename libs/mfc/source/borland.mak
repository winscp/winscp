!include ../../Makefile.inc

#############################################################################
# Lib Path
LPATH=..\..\lib;..\..\lib\psdk

# MFC Include directory base
MFCINCL=..\include

# BC Include directory
BCINCL=..\..\include;..\..\include\atl

BASE=W
MODEL=U
TARGDEFS=_UNICODE;UNICODE;WINSCP
TARGDEFS=$(TARGDEFS);_MBCS;_MT=1;_X86_

LFLAGS=$(LFLAGS) /j$(LPATH) /L$(LPATH)
LFLAGS=$(LFLAGS) /w-

#
# Object File Directory
#
D=$$$(MODEL)$(BASE)    # subdirectory specific to variant

GOAL=$(MODEL)afxc$(BASE)

#
# COMPILER OPTIONS
#
# -VF4 Support MFC 4.0
CL_OPT=-VF4 -w-

DEFS=_declspec=__declspec;_WCHAR_T_DEFINED;__MSC;_ANONYMOUS_STRUCT;_MSC_VER=1200;_WINDOWS
DEFS=$(DEFS)$(DEBDEFS);$(TARGDEFS)

#############################################################################
# Library Components

OBJECT = except.obj
	
FILES = filecore.obj filex.obj filest.obj

MISC = strcore.obj strex.obj timecore.obj

WINMISC = winstr.obj

INLINES =

CPP_OBJS=$(OBJECT) $(FILES)  $(MISC) $(WINMISC)

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
	$(CC) $(BORFLAGS) -o $@ $<

#############################################################################
# Goals to build

LIB=$(LIB_PATH)\$(GOAL).lib
GOALS=log.mode create.dir $(LIB)

goal: $(GOALS)

log.mode:
  @-echo BORLAND.MAK: 
  @-echo              GOAL=$(GOAL), 
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
# Build the library from the up-to-date objs

# Build final library
$(LIB): $(OBJS)
	$(MKLIB) $@ $(**)

#############################################################################
