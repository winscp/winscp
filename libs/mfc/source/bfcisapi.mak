PLATFORM=INTEL
PROCESSOR_ARCHITECTURE=x86

# Default to DEBUG mode
!ifndef DEBUG
DEBUG=1
!endif

# Default to NOT DLL
!ifndef DLL
DLL=0
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

# Default to no _MBCS build
MBCS=0

# Default to multithreading support
!ifndef MT
MT=1
!endif

!if "$(MT)" != "1"
!error This library must be built with threadding enabled.
!endif

# Lib directory
!ifndef LIBDIR
LIBDIR=..\LIB\BORLAND
!endif

# TYPE = Library Type Designator
#       N = normal C library
#	D = for use with MFC USRDLL library
#       E = for use with MFC Extension DLL library

!if "$(DLL)" == "0"
TYPE=N
!if "$(DEBUG)" == "1"
TARGOPTS=$(TARGOPTS) -v
!endif
!endif

!if "$(DLL)" == "1"
!error The _USRDLL configuration is not supported by this library.
!endif

!if "$(DLL)" == "2"
!if "$(DEBUG)" == "1"
TARGOPTS=$(TARGOPTS) -v
!endif
TARGDEFS=$(TARGDEFS) /D_WINDLL /D_AFXDLL /D_DLL /D_AFX_CORE_IMPL
TYPE=E
!endif

#############################################################################
# Parse options

#
# DEBUG OPTIONS
#
!if "$(DEBUG)" != "0"

DEBUGSUF=D
DEBDEFS=/D_DEBUG
DEBOPTS=-Od

!endif

#
# NON-DEBUG OPTIONS
#
!if "$(DEBUG)" == "0"

DEBUGSUF=
DEBDEFS=

DEBOPTS=-O1
!endif

#
# PLATFORM options
#
CPP=bcc32
LIB32=tlib
!ifndef LINK32
LINK32=tlink32
!endif

CL_MODEL=/D_X86_
MODEL=IS

#
# Object File Directory
#
!if "$(OBJ)" == ""
D=$$$(TYPE)$(MODEL)$(DEBUGSUF)    # subdirectory specific to variant
!else
D=$(OBJ)                                 # User specified directory
!endif

GOAL=$(TYPE)AFX$(MODEL)$(DEBUGSUF)

#
# COMPILER OPTIONS
#
!if "$(PLATFORM)" == "INTEL"
CL_OPT=-w- -X -VF4 -n$(D) $(DEBOPTS) $(TARGOPTS)
!endif


!if "$(BROWSE)" != "0"
CL_OPT=-R $(CL_OPT)
!endif

DEFS=$(DEFS) $(DEBDEFS) $(TARGDEFS)

#############################################################################
# Library Components

OBJS=isapi.obj inetcall.obj

#############################################################################
# Standard tools

#############################################################################
# Set CPPFLAGS for use with .cpp.obj and .c.obj rules
# Define rule for use with OBJ directory
# C++ uses a PCH file

CPPFLAGS=$(CPPFLAGS) $(CL_MODEL) $(CL_OPT) $(DEFS) $(OPT)

PCH_FILE=

.SUFFIXES: .cpp
.path.obj = $(D)

.cpp.obj:
        $(CC) @&&~
$(CPPFLAGS) -c  $<
~

#############################################################################
# Goals to build

GOALS=create.dir
GOALS=$(GOALS) $(LIBDIR)\$(GOAL).lib

goal: $(GOALS)

create.dir:
	@-if not exist $(D)\*.* mkdir $(D)
	@-if not exist $(LIBDIR) mkdir $(LIBDIR)

clean:
	-if exist $(D)\*.obj erase $(D)\*.obj
	-if exist $(D)\*.pch erase $(D)\*.pch
	-if exist $(D)\*.res erase $(D)\*.res
	-if exist $(D)\*.rsc erase $(D)\*.rsc
	-if exist $(D)\*.map erase $(D)\*.map
	-if exist $(D)\*.* rmdir $(D)
	-if exist ..\lib\$(GOAL).pdb del ..\lib\$(GOAL).pdb

#############################################################################
# Build the library from the up-to-date objs

# Build final library

!if "$(DLL)" != "2"
# Build final library
$(LIBDIR)\$(GOAL).lib: $(OBJS)
	@-if exist $@ erase $@
	@tlib $@ /P2048 @&&!
+$(**: = &^
+)
!
!else
!error No DLL Version of ISAPI Libraries supported
!endif #DLL!=2


#############################################################################
