BDS_BIN = $(BDS)\bin
BDS_INCLUDE = $(BDS)/include

MKLIB_ARG = /a /C
!ifdef WIN64
CC = "$(BDS_BIN)\bcc64"
# -fexceptions forces exception support in C code (to allow passing exception from C++ callbacks via C layer)
# -ffunction-sections absence is needed for the same
# -fseh - needed for MFC, but maybe also for passing exteption through C code
CC_ARG = -triple x86_64-pc-win32-elf -masm-verbose -mstack-alignment=16 -tW -fexceptions -fseh
CFLAG_OPENSSL=-D_WIN64
MKLIB = "$(BDS_BIN)\tlib64" $(MKLIB_ARG)
O = o
!else
CC = "$(BDS_BIN)\bcc32c"
CC_ARG = -triple i686-pc-windows-omf -ffunction-sections
MKLIB = "$(BDS_BIN)\tlib" $(MKLIB_ARG)
O = obj
!endif

CFLAG_BASE = -O2

# Debug
# OpenSSL needs -O2, otherwise it won't link (with 64-bit it seems to be ok though)
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

CFLAG_OPENSSL=$(CFLAG_OPENSSL) -DOPENSSL_API_COMPAT=11000 -DOPENSSL_NO_DEPRECATED
