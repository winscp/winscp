!include ../Makefile.inc

# Set your compiler options
DEFINES = WIN32;WIN32_LEAN_AND_MEAN;WINSCP;NE_LFS
WARNINGS = -Wno-deprecated-declarations
# Compile like C++
CFLAG = $(CFLAG_CPP) $(CFLAG_OPENSSL) -D$(DEFINES:;= -D) $(WARNINGS)
# The output directory for all the temporary muck
TMP_D=tmp

.autodepend
.path.c   = src

# ---------------------------------------------------------------------------

# OBJ_D  - temp object file directory
OBJ_D=$(TMP_D)
INCL=inc;inc/mingw;../openssl/include;../expat/lib;../neon/src

O_LIBS3=$(LIB_PATH)\libs3.lib

INC=-I$(INCL:;= -I)
LIB_CFLAGS=$(CFLAG) $(INC)

#############################################
LIBS3_OBJS=\
    $(OBJ_D)\bucket.obj \
    $(OBJ_D)\bucket_metadata.obj \
    $(OBJ_D)\error_parser.obj \
    $(OBJ_D)\general.obj \
    $(OBJ_D)\mingw_functions.obj \
    $(OBJ_D)\multipart.obj \
    $(OBJ_D)\object.obj \
    $(OBJ_D)\request.obj \
    $(OBJ_D)\request_context.obj \
    $(OBJ_D)\response_headers_handler.obj \
    $(OBJ_D)\service.obj \
    $(OBJ_D)\service_access_logging.obj \
    $(OBJ_D)\simplexml.obj \
    $(OBJ_D)\util.obj

.c.obj:
    $(CC) $(LIB_CFLAGS) -output-dir $(@D) {$< }

$(O_LIBS3): $(LIBS3_OBJS)
    if exist $(O_LIBS3) del $(O_LIBS3)
    $(MKLIB) /P64 $@ $(**)

###################################################################
all: banner $(TMP_D) lib

banner:
    @echo Building libs3

$(TMP_D):
    if not exist "$(TMP_D)" ( mkdir "$(TMP_D)" )

lib: $(O_LIBS3)
