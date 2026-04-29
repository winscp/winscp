BDS_BIN = $(BDS)\bin
BDS_INCLUDE = $(BDS)/include

MKLIB_ARG = /a /C
CC = "$(BDS_BIN)\bcc32c"
CC_ARG = -triple i686-pc-windows-omf -ffunction-sections
MKLIB = "$(BDS_BIN)\tlib" $(MKLIB_ARG)
OBJ_EXT = obj

CFLAG_BASE = -O2

# Debug
# OpenSSL needs -O2, otherwise it won't link
# CFLAG_BASE = -O0 -D_DEBUG -debug-info-kind=standalone
# For nasm, add -g

CFLAG_BASE = \
    -cc1 -nobuiltininc -nostdsysteminc $(CC_ARG) -emit-obj \
    $(CFLAG_BASE) -tM -fborland-extensions -munwind-tables -fno-common --jobs=8 \
    -isystem "$(BDS_INCLUDE)/windows/crtl" -isystem "$(BDS_INCLUDE)/windows/sdk" \
    -isystem "$(BDS_INCLUDE)/dinkumware64"

CFLAG_COMMON = \
    $(CFLAG_BASE) -std=c99 -x c -fno-threadsafe-statics

CFLAG_CPP = \
    $(CFLAG_BASE) -x c++ -std=c++17 -fcxx-exceptions -fno-use-cxa-atexit

CFLAG_OPENSSL=-DOPENSSL_API_COMPAT=11000 -DOPENSSL_NO_DEPRECATED
