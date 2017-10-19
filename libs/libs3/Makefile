# Set your compiler options
CFLAG=-DWIN32 -DWIN32_LEAN_AND_MEAN -DWINSCP -DNE_LFS -q-c -tWC -tWM -P
CFLAG = $(CFLAG) -w-aus -w-par

# The output directory for everything interesting
OUT_D=..\lib
# The output directory for all the temporary muck
TMP_D=tmp

CC=bcc32
MKLIB=tlib /P64 /C

.autodepend
.PATH.c   = src

# ---------------------------------------------------------------------------

# OBJ_D  - temp object file directory
OBJ_D=$(TMP_D)
INCL=inc;inc/mingw;../openssl;../expat/lib;../neon/src

O_LIBS3=$(OUT_D)\libs3.lib

INC=-I$(INCL)
LIB_CFLAGS=$(INC) $(CFLAG) $(LIB_CFLAG)
LIBS_DEP=$(O_LIBS3)

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
    $(CC) -n$(OBJ_D) $(LIB_CFLAGS) -c {$< }

$(O_LIBS3): $(LIBS3_OBJS)
    if exist $(O_LIBS3) del $(O_LIBS3)
    $(MKLIB) $(O_LIBS3) @&&!
+$(**: = &^
+)
!

###################################################################
all: banner $(TMP_D) $(OUT_D) lib

banner:
    @echo Building libs3
    @echo $(LIBS3_OBJS)

$(TMP_D):
    if not exist "$(TMP_D)" ( mkdir "$(TMP_D)" )

$(OUT_D):
    if not exist "$(OUT_D)" ( mkdir "$(OUT_D)" )

lib: $(LIBS_DEP)
