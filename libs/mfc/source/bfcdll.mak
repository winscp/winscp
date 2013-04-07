# BFCDLL.MAK - Borland makefile for BFC42*.DLL
#
# BFC42[D].DLL is a DLL
#  which exports all the MFC classes
#

# Default to No Debug Info mode
!ifndef DBGINFO
DBGINFO=0
!endif

!ifndef LIBNAME
LIBNAME=BFC42
!endif

!if "$(DEBUG)" != "0"
CRTDLL=CW32MTI.lib
!else
CRTDLL=CW32MTI.lib
!endif

!if "$(CG)" == "1"
CGLIB=cg32.lib
!else
CGLIB=
!endif

TARGET=w
DLL=2
TARGDEFS=/D_AFX_CORE_IMPL
LFLAGS= /n /m /s  $(LFLAGS)
RCFLAGS=-I$(BCINCL);$(MFCINCL)

!if "$(DBGINFO)" != "0"
LFLAGS=$(LFLAGS) /v
!endif

!if "$(DEBUG)" != "0"
# Debug DLL build
TARGTYPE=$(TARGTYPE)D
RCDEFINES=$(RCDEFINES) /D_DEBUG
!ifndef MONOLITHIC
MONOLITHIC=0
!endif
!ELSE
# Release DLL build
RCDEFINES=$(RCDEFINES)
LFLAGS=$(LFLAGS)
!ifndef MONOLITHIC
MONOLITHIC=1
!endif
!ENDIF

!if "$(MONOLITHIC)" == "1"
TARGDEFS=$(TARGDEFS) -D_AFX_OLE_IMPL -D_AFX_DB_IMPL -D_AFX_NET_IMPL
RCDEFINES=$(RCDEFINES) /D_AFX_MONOLITHIC
!endif

CFNAME=$(LIBNAME)$(TARGTYPE)
TARG=$(LIBNAME)$(PF)$(TARGTYPE)
TARG_STATIC=BFCS42$(PF)$(TARGTYPE)

DEFFILE=$(PLATFORM)\$(TARG).DEF


!ifdef RELEASE # Release VERSION info
RCDEFINES=$(RCDEFINES) /DRELEASE
!endif

LFLAGS=$(LFLAGS) /Tpd

bfcdll_goal: dll_goal

#############################################################################
# import most rules and library files from normal makefile

!include borland.mak

dll_goal: create.dir create.rsp \
	$(TARG).dll $(LIBDIR)\$(TARG).lib $(LIBDIR)\$(TARG_STATIC).lib

#############################################################################
# more flags and switches and libs

LFLAGS=$(LFLAGS)
LIBS=$(CGLIB) $(CRTDLL) $(IMPORT_LIBS) ole2w32.lib

#############################################################################

STATICLINK_OBJS= stdafx.obj nolib.obj appmodul.obj dllmodul.obj oleexp.obj

PCH_STATIC_CMD=-Hc -H\"stdafx.h\" -H=$(D)\stdafxs.csm

CPPFLAGS_STATIC=$(CL_MODEL) $(CL_OPT) $(DEFS) $(OPT) $(EH) $(PCH_STATIC_CMD)

stdafx.obj:
	$(CC)  /c $(CPPFLAGS_STATIC) -I$(BCINCL);$(MFCINCL) { $(STATICLINK_OBJS:.obj=.cpp) }

nolib.obj:    stdafx.obj
	@

appmodul.obj: stdafx.obj
	@

dllmodul.obj: stdafx.obj
	@

oleexp.obj:   stdafx.obj
	@


#############################################################################
# Build target

$(D)\$(TARG).res: mfcdll.rc build_.h
	brcc32 /r $(RCFLAGS) $(RCDEFINES) /fo $(D)\$(TARG).res mfcdll.rc

DLL_OBJS=$(OBJECT) $(OBJDIAG) $(INLINES) $(FILES) $(COLL1) $(COLL2) $(MISC) \
	$(WINDOWS) $(DIALOG) $(WINMISC) $(DOCVIEW) $(APPLICATION) $(OLEREQ) \
        $(INTERNET)

!if "$(MONOLITHIC)" == "1"
DLL_OBJS=$(DLL_OBJS) $(SOCKETS) $(OLEDLL) $(DAO) $(DB)
!else
#
# BORLAND fix: need to link with occcont.obj since a needed destructor is in there
 DLL_OBJS= $(DLL_OBJS) occstub.obj
#
#
!endif

DLL_OBJS=$(DLL_OBJS) dllinit.obj bormem.obj

DLL_RESOURCES=$(D)\$(TARG).res

$(DEFFILE): $(DLL_OBJS)
!if $d(INTEGRATION_BUILD)
	$(GENDEFS) $(TARG) $(D)\ > $(DEFFILE)
!else
	@echo End-user build, so DEF files are already created
!endif

$(TARG).dll:: $(DEFFILE) $(DLL_RESOURCES)

$(TARG).dll:: $(DLL_OBJS)
	$(LINK32) @&&!
$(LFLAGS) +
C0D32.OBJ +
$(**: = +^
)
$(TARG).DLL
$(D)\$(TARG).MAP
$(LIBS)
$(DEFFILE)
$(DLL_RESOURCES)
!

$(LIBDIR)\$(TARG).lib: $(TARG).dll
    implib  $(LIBDIR)\$(TARG).lib $(TARG).DLL


$(LIBDIR)\$(TARG_STATIC).lib: $(STATICLINK_OBJS)
	$(LIB32) $@ /P4096 @&&!
+-$(**: = &^
+-)
!

#############################################################################
