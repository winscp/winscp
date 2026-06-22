!include ../Makefile.inc

# Set your compiler options
DEFINES = WIN32_LEAN_AND_MEAN;WINSCP;NE_LFS
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
    $(OBJ_D)\bucket.$(O) \
    $(OBJ_D)\bucket_metadata.$(O) \
    $(OBJ_D)\error_parser.$(O) \
    $(OBJ_D)\general.$(O) \
    $(OBJ_D)\mingw_functions.$(O) \
    $(OBJ_D)\multipart.$(O) \
    $(OBJ_D)\object.$(O) \
    $(OBJ_D)\request.$(O) \
    $(OBJ_D)\request_context.$(O) \
    $(OBJ_D)\response_headers_handler.$(O) \
    $(OBJ_D)\service.$(O) \
    $(OBJ_D)\service_access_logging.$(O) \
    $(OBJ_D)\simplexml.$(O) \
    $(OBJ_D)\util.$(O)

.c.$(O):
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
