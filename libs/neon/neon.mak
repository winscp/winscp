#**** neon Win32 -*- Makefile -*- ********************************************
#
# Define DEBUG_BUILD to create a debug version of the library.

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

!IF "$(BUILD_DLL)" == "yes"
USE_DLL = 1
!endif

########
# Debug vs. Release build
!IF "$(DEBUG_BUILD)" == ""
INTDIR = Release
CFLAGS = /MD /W3 /EHsc /Zi /O2 /D "NDEBUG"
TARGET = .\libneon.lib
!ELSE
INTDIR = Debug
CFLAGS = /MDd /W3 /Gm /EHsc /Zi /Od /D "_DEBUG"
TARGET = .\libneonD.lib
!ENDIF

NE_DEP_LIBS =

!IFDEF USE_DLL
CFLAGS = $(CFLAGS) /D NE_DLL
NE_DEP_LIBS = $(NE_DEP_LIBS) ws2_32.lib
!ENDIF

# Silence deprecation warnings on later Visual Studio versions, which
# actually can be ignored
CFLAGS = $(CFLAGS) /D _CRT_SECURE_NO_WARNINGS /D _CRT_NONSTDC_NO_WARNINGS /D _WINSOCK_DEPRECATED_NO_WARNINGS

########
# Whether to build SSPI
!IF "$(SSPI_BUILD)" != ""
CFLAGS = $(CFLAGS) /D HAVE_SSPI
!ENDIF

########
# Support for Expat or libxml2 integration
#
# If USE_EXPAT or USE_LIBXML are set, then assume compiling against a
# pre-built binary Expat or libxml2.  Note that if both are set, USE_EXPAT is
# assumed and USE_LIBXML is ignored.  If neither of USE_EXPAT or USE_LIBXML are
# set, disable WebDAV support.

!IF DEFINED (USE_EXPAT) && DEFINED (USE_LIBXML)
!MESSAGE Expat is preferred over LibXML2
!ENDIF

!IFDEF USE_EXPAT
BUILD_XML_SUPPORT = 1
NE_XML_FLAGS = /D HAVE_EXPAT /D NE_HAVE_DAV
EXPAT_LIBS = libexpat.lib
NE_DEP_LIBS = $(NE_DEP_LIBS) $(EXPAT_LIBS)
!ENDIF

!IF DEFINED(USE_LIBXML) && !DEFINED(USE_EXPAT)
BUILD_XML_SUPPORT = 1
NE_XML_FLAGS = /D HAVE_LIBXML /D NE_HAVE_DAV
LIBXML_LIBS = libxml2.lib
NE_DEP_LIBS = $(NE_DEP_LIBS) $(LIBXML_LIBS)
!ENDIF

########
# Support for OpenSSL integration
!IF "$(OPENSSL_SRC)" == ""
!IF DEFINED (USE_OPENSSL11) || DEFINED (USE_OPENSSL)
OPENSSL_FLAGS = /D NE_HAVE_SSL /D HAVE_OPENSSL
!IFDEF USE_OPENSSL11
OPENSSL_FLAGS = $(OPENSSL_FLAGS) /D HAVE_OPENSSL11
!ENDIF
BUILD_OPENSSL = 1
!ELSE
OPENSSL_FLAGS =
!ENDIF
!ELSE
OPENSSL_FLAGS = /I "$(OPENSSL_SRC)\inc32" /D NE_HAVE_SSL /D HAVE_OPENSSL
BUILD_OPENSSL = 1
!ENDIF

########
# Support for zlib integration
!IF "$(DEBUG_BUILD)" == ""
ZLIB_STATICLIB = zlib.lib
ZLIB_SHAREDLIB = zlib1.dll
!IF "$(ZLIB_SRC)" == ""
!IF "$(ZLIB_IMPLIB)" == ""
ZLIB_IMPLIB    = zlib1.lib
!ENDIF
!ELSE
ZLIB_IMPLIB    = zdll.lib
!ENDIF
ZLIB_LDFLAGS   = /nologo /release
!ELSE
ZLIB_STATICLIB = zlib_d.lib
ZLIB_SHAREDLIB = zlib1_d.dll
!IF "$(ZLIB_SRC)" == ""
!IF "$(ZLIB_IMPLIB)" == ""
ZLIB_IMPLIB    = zlib1d.lib
!ENDIF
!ELSE
ZLIB_IMPLIB    = zdll_d.lib
!ENDIF
ZLIB_LDFLAGS   = /nologo /debug
!ENDIF

!IF "$(ZLIB_SRC)" == ""
ZLIB_CLEAN =
!IF "$(USE_ZLIB)" == ""
ZLIB_FLAGS =
ZLIB_LIBS =
!ELSE
ZLIB_FLAGS = /D NE_HAVE_ZLIB
!IFNDEF ZLIB_LIBS
!IF "$(ZLIB_DLL)" == ""
ZLIB_LIBS = $(ZLIB_STATICLIB)
!ELSE
ZLIB_LIBS = $(ZLIB_IMPLIB)
!ENDIF
!ENDIF
!ENDIF
!ELSE
ZLIB_CLEAN = ZLIB_CLEAN
ZLIB_FLAGS = /I "$(ZLIB_SRC)" /D NE_HAVE_ZLIB
!IF "$(ZLIB_DLL)" == ""
ZLIB_LIBS = "$(ZLIB_SRC)\$(ZLIB_STATICLIB)"
!ELSE
ZLIB_FLAGS = $(ZLIB_FLAGS) /D ZLIB_DLL
ZLIB_LIBS = "$(ZLIB_SRC)\$(ZLIB_IMPLIB)"
!ENDIF
!ENDIF

########
# Support for IPv6
!IF "$(ENABLE_IPV6)" == "yes"
IPV6_FLAGS = /D USE_GETADDRINFO
!ENDIF


# Exclude stuff we don't need from the Win32 headers
WIN32_DEFS = /D WIN32_LEAN_AND_MEAN /D NOUSER /D NOGDI /D NONLS /D NOCRYPT

CPP=cl.exe
CPP_PROJ = /c /nologo $(CFLAGS) $(WIN32_DEFS) $(NE_XML_FLAGS) $(OPENSSL_FLAGS) $(ZLIB_FLAGS) $(IPV6_FLAGS) /D "HAVE_CONFIG_H" /D BUILDING_NEON /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"
LINK=link.exe
LIB32=$(LINK) -lib
LIB32_FLAGS=/nologo /out:$@
LINK_DLL_FLAGS=$(LIB32_FLAGS) /DLL /DEBUG /def:src\neon.def

LIB32_OBJS= \
	"$(INTDIR)\ne_alloc.obj" \
	"$(INTDIR)\ne_auth.obj" \
	"$(INTDIR)\ne_basic.obj" \
	"$(INTDIR)\ne_compress.obj" \
	"$(INTDIR)\ne_dates.obj" \
	"$(INTDIR)\ne_i18n.obj" \
	"$(INTDIR)\ne_md5.obj" \
	"$(INTDIR)\ne_pkcs11.obj" \
	"$(INTDIR)\ne_redirect.obj" \
	"$(INTDIR)\ne_request.obj" \
	"$(INTDIR)\ne_session.obj" \
	"$(INTDIR)\ne_socket.obj" \
	"$(INTDIR)\ne_socks.obj" \
	"$(INTDIR)\ne_sspi.obj" \
	"$(INTDIR)\ne_string.obj" \
	"$(INTDIR)\ne_uri.obj" \
	"$(INTDIR)\ne_utils.obj"

!IF "$(BUILD_XML_SUPPORT)" != ""
LIB32_OBJS= \
	$(LIB32_OBJS) \
	"$(INTDIR)\ne_207.obj" \
	"$(INTDIR)\ne_xml.obj" \
	"$(INTDIR)\ne_xmlreq.obj" \
	"$(INTDIR)\ne_oldacl.obj" \
	"$(INTDIR)\ne_acl3744.obj" \
	"$(INTDIR)\ne_props.obj" \
	"$(INTDIR)\ne_locks.obj" 
!ENDIF

!IFDEF BUILD_OPENSSL
LIB32_OBJS = $(LIB32_OBJS) "$(INTDIR)\ne_openssl.obj"
!ENDIF
!IF "$(OPENSSL_SRC)" != ""
!IFDEF OPENSSL_STATIC
LIB32_OBJS = $(LIB32_OBJS) "$(OPENSSL_SRC)\out32\libeay32.lib" \
			   "$(OPENSSL_SRC)\out32\ssleay32.lib"
!ELSE
LIB32_OBJS = $(LIB32_OBJS) "$(OPENSSL_SRC)\out32dll\libeay32.lib" \
			   "$(OPENSSL_SRC)\out32dll\ssleay32.lib"
!ENDIF
!ELSE
!IF DEFINED (USE_OPENSSL11) || DEFINED (USE_OPENSSL)
!IFDEF USE_OPENSSL11
SSL_LIBS = libssl.lib libcrypto.lib
!ELSE
SSL_LIBS = ssleay32.lib libeay32.lib
!ENDIF
NE_DEP_LIBS = $(NE_DEP_LIBS) $(SSL_LIBS)
!ELSE
# Provide ABI-compatibility stubs for SSL interface
LIB32_OBJS = $(LIB32_OBJS) "$(INTDIR)\ne_stubssl.obj"
!ENDIF
!ENDIF
!IF "$(ZLIB_SRC)" != ""
LIB32_OBJS = $(LIB32_OBJS) $(ZLIB_LIBS)
!ENDIF
!IF "$(USE_ZLIB)" != ""
NE_DEP_LIBS = $(NE_DEP_LIBS) $(ZLIB_LIBS)
!ENDIF

ALL: ".\src\config.h" "$(TARGET)"

CLEAN: $(ZLIB_CLEAN)
	-@erase "$(INTDIR)\ne_207.obj"
	-@erase "$(INTDIR)\ne_alloc.obj"
	-@erase "$(INTDIR)\ne_oldacl.obj"
	-@erase "$(INTDIR)\ne_acl3744.obj"
	-@erase "$(INTDIR)\ne_auth.obj"
	-@erase "$(INTDIR)\ne_basic.obj"
	-@erase "$(INTDIR)\ne_compress.obj"
	-@erase "$(INTDIR)\ne_dates.obj"
	-@erase "$(INTDIR)\ne_i18n.obj"
	-@erase "$(INTDIR)\ne_locks.obj"
	-@erase "$(INTDIR)\ne_md5.obj"
	-@erase "$(INTDIR)\ne_props.obj"
	-@erase "$(INTDIR)\ne_redirect.obj"
	-@erase "$(INTDIR)\ne_request.obj"
	-@erase "$(INTDIR)\ne_session.obj"
	-@erase "$(INTDIR)\ne_openssl.obj"
	-@erase "$(INTDIR)\ne_stubssl.obj"
	-@erase "$(INTDIR)\ne_pkcs11.obj"
	-@erase "$(INTDIR)\ne_socket.obj"
	-@erase "$(INTDIR)\ne_socks.obj"
	-@erase "$(INTDIR)\ne_sspi.obj"
	-@erase "$(INTDIR)\ne_string.obj"
	-@erase "$(INTDIR)\ne_uri.obj"
	-@erase "$(INTDIR)\ne_utils.obj"
	-@erase "$(INTDIR)\ne_xml.obj"
	-@erase "$(INTDIR)\ne_xmlreq.obj"
	-@erase "$(TARGET:.lib=.dll)"
	-@erase "$(TARGET:.lib=.pdb)"
	-@erase "$(TARGET:.lib=.exp)"
	-@erase "$(TARGET:.lib=.ilk)"
	-@erase "$(TARGET)"
	-@erase ".\src\config.h"

!IFDEF USE_DLL
"$(TARGET)": "$(TARGET:.lib=.dll)"

"$(TARGET:.lib=.dll)": $(LIB32_OBJS)
	-@if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	$(LINK) @<<
$(LINK_DLL_FLAGS) $(LIB32_OBJS)	$(NE_DEP_LIBS)
<<

!ELSE
"$(TARGET)": $(DEF_FILE) $(LIB32_OBJS)
	-@if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	$(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS) $(NE_DEP_LIBS)
<<
!ENDIF

{src}.c{$(INTDIR)}.obj::
	-@if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	$(CPP) @<<
  $(CPP_PROJ) $<
<<

".\src\config.h": config.hw
	-@if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	<<tempfile.bat
  @echo off
  copy .\config.hw .\src\config.h > nul
  echo Created config.h from config.hw
<<

"$(INTDIR)\ne_207.obj":      .\src\ne_207.c
"$(INTDIR)\ne_alloc.obj":    .\src\ne_alloc.c
"$(INTDIR)\ne_acl3744.obj":  .\src\ne_acl3744.c
"$(INTDIR)\ne_oldacl.obj":   .\src\ne_oldacl.c
"$(INTDIR)\ne_auth.obj":     .\src\ne_auth.c
"$(INTDIR)\ne_basic.obj":    .\src\ne_basic.c
"$(INTDIR)\ne_compress.obj": .\src\ne_compress.c
"$(INTDIR)\ne_dates.obj":    .\src\ne_dates.c
"$(INTDIR)\ne_i18n.obj":     .\src\ne_i18n.c
"$(INTDIR)\ne_locks.obj":    .\src\ne_locks.c
"$(INTDIR)\ne_md5.obj":      .\src\ne_md5.c
"$(INTDIR)\ne_props.obj":    .\src\ne_props.c
"$(INTDIR)\ne_redirect.obj": .\src\ne_redirect.c
"$(INTDIR)\ne_request.obj":  .\src\ne_request.c
"$(INTDIR)\ne_session.obj":  .\src\ne_session.c
"$(INTDIR)\ne_openssl.obj":  .\src\ne_openssl.c
"$(INTDIR)\ne_stubssl.obj":  .\src\ne_stubssl.c
"$(INTDIR)\ne_pkcs11.obj":   .\src\ne_pkcs11.c
"$(INTDIR)\ne_socket.obj":   .\src\ne_socket.c
"$(INTDIR)\ne_socks.obj":    .\src\ne_socks.c
"$(INTDIR)\ne_sspi.obj":     .\src\ne_sspi.c
"$(INTDIR)\ne_string.obj":   .\src\ne_string.c
"$(INTDIR)\ne_uri.obj":      .\src\ne_uri.c
"$(INTDIR)\ne_utils.obj":    .\src\ne_utils.c
"$(INTDIR)\ne_xml.obj":      .\src\ne_xml.c
"$(INTDIR)\ne_xmlreq.obj":   .\src\ne_xmlreq.c

"$(ZLIB_SRC)\$(ZLIB_STATICLIB)":
	<<tempfile.bat
  @echo off
  cd /d "$(ZLIB_SRC)"
  $(MAKE) /nologo /f win32\Makefile.msc CFLAGS="/nologo $(CFLAGS)" LDFLAGS="$(ZLIB_LDFLAGS)" STATICLIB=$(ZLIB_STATICLIB) $(ZLIB_STATICLIB)
<<

"$(ZLIB_SRC)\$(ZLIB_IMPLIB)":
	<<tempfile.bat
  @echo off
  cd /d "$(ZLIB_SRC)"
  $(MAKE) /nologo /f win32\Makefile.msc CFLAGS="/nologo $(CFLAGS)" LDFLAGS="$(ZLIB_LDFLAGS)" SHAREDLIB=$(ZLIB_SHAREDLIB) IMPLIB=$(ZLIB_IMPLIB) $(ZLIB_SHAREDLIB) $(ZLIB_IMPLIB)
<<

ZLIB_CLEAN:
	<<tempfile.bat
  @echo off
  cd /d "$(ZLIB_SRC)"
  $(MAKE) /nologo /f win32\Makefile.msc STATICLIB=$(ZLIB_STATICLIB) SHAREDLIB=$(ZLIB_SHAREDLIB) IMPLIB=$(ZLIB_IMPLIB) clean
<<
