CC = "$(BDS)\bin\bcc32c"
MKLIB = "$(BDS)\bin\tlib" /a /C

CFLAG_BASE = -O2

# Debug
# OpenSSL needs -O2, otherwise it won't link
# CFLAG_BASE = -O0 -D_DEBUG -debug-info-kind=standalone
# For nasm, add -g

CFLAG_BASE = \
    -cc1 -nobuiltininc -nostdsysteminc -triple i686-pc-windows-omf -emit-obj -mrelocation-model static \
    -masm-verbose -mstack-alignment=16 $(CFLAG_BASE) -tM -tW -fborland-extensions --jobs=8 \
    -isystem "$(BDS)/include/windows/crtl" -isystem "$(BDS)/include/windows/sdk" \
    -isystem "$(BDS)/include/dinkumware64"

CFLAG_COMMON = \
    $(CFLAG_BASE) -std=c99 -x c -fno-threadsafe-statics

CFLAG_CPP = \
    $(CFLAG_BASE) -x c++ -std=c++17 -fcxx-exceptions -ffunction-sections -fno-use-cxa-atexit

CFLAG_OPENSSL=-DOPENSSL_API_COMPAT=11000 -DOPENSSL_NO_DEPRECATED
