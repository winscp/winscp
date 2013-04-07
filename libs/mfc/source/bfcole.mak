# BFCOLE.MAK - Borland makefile for BFCO42*.DLL
#
# BFCO42[D].DLL is a DLL
#  which exports all the MFC/OLE classes
#

# Default to No Debug Info mode
!ifndef DBGINFO
DBGINFO=0
!endif

!ifndef LIBNAME
LIBNAME=BFCO42
!endif

!if "$(DEBUG)" != "0"
CRTDLL=cw32mti.lib
!else
CRTDLL=cw32mti.lib
!endif

!if "$(CG)" == "1"
CGLIB=cg32.lib
!else
CGLIB=
!endif

TARGET=w
DLL=2
TARG=$(LIBNAME)
TARGDEFS=/D_AFX_OLE_IMPL
MFCDLL=BFC42
LFLAGS= /n /m /s $(LFLAGS)
RCFLAGS=-I$(BCINCL);$(MFCINCL)

!if "$(DBGINFO)" != "0"
LFLAGS=$(LFLAGS) /v
!endif

!if "$(DEBUG)" != "0"
# Debug DLL build
RCDEFINES=/D_DEBUG
PCH_FILE=$(D)\stdafxo.csm
PCH_CPP=dllole
TARG=$(TARG)D
MFCDLL=$(MFCDLL)D

!ELSE
# Release DLL build
RCDEFINES=
LFLAGS=$(LFLAGS)
PCH_FILE=$(D)\stdafxo.csm
PCH_CPP=dllole
!ENDIF

DEFFILE=$(PLATFORM)\$(TARG).DEF

MFCDLL=$(LIBDIR)\$(MFCDLL).lib

!ifdef RELEASE # Release VERSION info
RCDEFINES=$(RCDEFINES) /DRELEASE
LFLAGS=$(LFLAGS) /release
!endif

LFLAGS=$(LFLAGS) /Tpd

bfcole_goal: dll_goal

#############################################################################
# import most rules and library files from normal makefile

!include borland.mak

dll_goal: create.dir create.rsp $(TARG).dll $(LIBDIR)\$(TARG).lib


#############################################################################
# Define linker libs

LIBS=$(CGLIB) $(MFCDLL) $(CRTDLL) ole2w32.lib $(IMPORT_LIBS)

#############################################################################
# Build target

$(D)\$(TARG).res: mfcole.rc build_.h
	brcc32 $(RCFLAGS) $(RCDEFINES) /fo $(D)\$(TARG).res mfcole.rc

DLL_OBJS=$(OLEDLL)

$(DEFFILE): $(DLL_OBJS)
	$(GENDEFS) $(TARG) $(D)\ > $(DEFFILE)

$(TARG).dll:: $(DEFFILE) $(D)\$(TARG).res $(MRC_RESOURCES)

#$(TARG).dll:: $(PCH_CPP).obj $(DLL_OBJS)
$(TARG).dll:: $(PCH_TARGETS).obj $(DLL_OBJS)
	$(LINK32) @&&!
$(LFLAGS) +
C0D32.OBJ +
$(**: = +^
)
$(TARG).DLL
$(D)\$(TARG).MAP
$(LIBS)
$(DEFFILE)
$(D)\$(TARG).res
!

$(LIBDIR)\$(TARG).lib: $(TARG).dll
    implib  $(LIBDIR)\$(TARG).lib $(TARG).DLL

#############################################################################
