!include ../../Makefile.inc
# ---------------------------------------------------------------------------
PROJECT = $(LIB_PATH)\libexpats_mtd.lib
OBJD = Release\obj\libexpat_static
# ---------------------------------------------------------------------------
OBJFILES = \
    $(OBJD)\xmlparse.obj \
    $(OBJD)\xmlrole.obj \
    $(OBJD)\xmltok.obj \
    $(OBJD)\loadlibrary.obj
# ---------------------------------------------------------------------------
DEFINES = _WINDOWS;WIN32;NDEBUG;_LIB;XML_STATIC;WINSCP;_NO_VCL;_ASSERTE;NO_STRICT;_RTLDLL
INCLUDEPATH = ..;..\lib
WARNINGS = -w-rch -w-par -w-8027 -w-8026 -w-ccc -w-8012
CFLAG1 = $(CFLAG_COMMON)
LFLAGS = 
# ---------------------------------------------------------------------------
.autodepend
# ---------------------------------------------------------------------------
DEFINESARGS = -D$(DEFINES:;= -D)
# ---------------------------------------------------------------------------
$(PROJECT): $(OBJFILES)
    $(MKLIB) $@ $(**)
# ---------------------------------------------------------------------------
.path.c = ..\lib
.c.obj:
    $(CC) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH:;= -I) $(DEFINESARGS) -n$(@D) {$< }
# ---------------------------------------------------------------------------
