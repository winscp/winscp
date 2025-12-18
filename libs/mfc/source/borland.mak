!include ../../Makefile.inc

#############################################################################
INCL=..\include

BASE=W
MODEL=U
DEFS=_UNICODE;UNICODE;WINSCP;_WINDOWS

#
# Object File Directory
#
D=$$$(MODEL)$(BASE)    # subdirectory specific to variant

GOAL=$(MODEL)afxc$(BASE)

#
# COMPILER OPTIONS
#
WARNINGS=-Wno-extra-tokens -Wno-extern-initializer -Wno-varargs
CL_OPT=$(WARNINGS)

#############################################################################
# Library Components

FILES = filecore.obj filex.obj filest.obj

MISC = strcore.obj strex.obj timecore.obj

CPP_OBJS=$(FILES) $(MISC)

OBJS=$(CPP_OBJS)


#############################################################################
# Standard tools

#############################################################################
# Set CPPFLAGS for use with .cpp.obj and .c.obj rules
# Define rule for use with OBJ directory

CPPFLAGS=$(CFLAG_CPP) $(CL_OPT) -D$(DEFS:;= -D)

BORFLAGS=$(CPPFLAGS) -I$(INCL:;= -I) -isystem "$(BDS)/include/windows/rtl"

.SUFFIXES: .cpp

.path.obj = $(D)

.cpp.obj:
	$(CC) $(BORFLAGS) -output-dir $(@D) {$< }

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
