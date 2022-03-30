#                          __  __            _
#                       ___\ \/ /_ __   __ _| |_
#                      / _ \\  /| '_ \ / _` | __|
#                     |  __//  \| |_) | (_| | |_
#                      \___/_/\_\ .__/ \__,_|\__|
#                               |_| XML parser
#
# Copyright (c) 2010      Patrick Spendrin <ps_ml@gmx.de>
# Copyright (c) 2012      Karl Waclawek <karl@waclawek.net>
# Copyright (c) 2016-2022 Sebastian Pipping <sebastian@pipping.org>
# Copyright (c) 2016      Sergei Nikulov <sergey.nikulov@gmail.com>
# Copyright (c) 2016      Björn Lindahl <bjorn.lindahl@foi.se>
# Copyright (c) 2016      Tobias Taschner <github@tc84.de>
# Copyright (c) 2016      Ben Boeckel <ben.boeckel@kitware.com>
# Copyright (c) 2017      Rhodri James <rhodri@wildebeest.org.uk>
# Copyright (c) 2017      Rolf Eike Beer <eike@sf-mail.de>
# Copyright (c) 2017      Stephen Groat <stephen@groat.us>
# Copyright (c) 2017      Franek Korta <fkorta@gmail.com>
# Copyright (c) 2018      pedro-vicente <pedro.vicente@space-research.org>
# Copyright (c) 2018      Frank Rast <frank.rast@gefeg.com>
# Copyright (c) 2018      userwithuid <userwithuid@gmail.com>
# Copyright (c) 2018      Yury Gribov <tetra2005@gmail.com>
# Copyright (c) 2019      Kishore Kunche <kishore.kunche@intel.com>
# Copyright (c) 2019      xantares <xantares09@hotmail.com>
# Copyright (c) 2019      Mohammed Khajapasha <mohammed.khajapasha@intel.com>
# Copyright (c) 2019      David Loffredo <loffredo@steptools.com>
# Copyright (c) 2019      Bhargava Shastry <bhargava.shastry@ethereum.org>
# Copyright (c) 2020      Maciej Sroczyński <macieksroczynski@poczta.fm>
# Copyright (c) 2020      Gulliver <gulliver@traumkristalle.net>
# Copyright (c) 2020      Thomas Beutlich <tc@tbeu.de>
# Copyright (c) 2021      Alex Richardson <Alexander.Richardson@cl.cam.ac.uk>
# Copyright (c) 2022      Johnny Jazeix <jazeix@gmail.com>
# Unlike most of Expat,
# this file is copyrighted under the BSD-license for buildsystem files of KDE.

cmake_minimum_required(VERSION 3.1.3)

# This allows controlling documented build time switches
# when Expat is pulled in using the add_subdirectory function, e.g.
#
#   set(EXPAT_BUILD_DOCS OFF)
#   set(EXPAT_BUILD_TOOLS OFF)
#   add_subdirectory(${expat_SOURCE_DIR}/expat ${expat_BINARY_DIR})
#
# would disable compilation of the xmlwf CLI and its man page.
# Without activating behaviour NEW for policy CMP0077 here,
# a user with -Wdev enabled would see warning
#
#   Policy CMP0077 is not set: option() honors normal variables.  Run "cmake
#   --help-policy CMP0077" for policy details.  Use the cmake_policy command to
#   set the policy and suppress this warning.
#
#   For compatibility with older versions of CMake, option is clearing the
#   normal variable 'EXPAT_BUILD_DOCS'.
#
# and effectively not be able to adjust option EXPAT_BUILD_DOCS.
#
# For more details please see:
# - https://cmake.org/cmake/help/latest/policy/CMP0077.html
# - https://github.com/libexpat/libexpat/pull/419
#
if(POLICY CMP0077)
    cmake_policy(SET CMP0077 NEW)
endif()

project(expat
    VERSION
        2.4.8
    LANGUAGES
        C
)

set(PACKAGE_BUGREPORT "expat-bugs@libexpat.org")
set(PACKAGE_NAME "expat")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
set(PACKAGE_TARNAME "${PACKAGE_NAME}")

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

#
# Configuration defaults
#
if(WINCE)
    set(_EXPAT_BUILD_TOOLS_DEFAULT OFF)
else()
    set(_EXPAT_BUILD_TOOLS_DEFAULT ON)
endif()
if(MSVC OR NOT _EXPAT_BUILD_TOOLS_DEFAULT)
    set(_EXPAT_BUILD_DOCS_DEFAULT OFF)
else()
    find_program(DOCBOOK_TO_MAN NAMES docbook2x-man db2x_docbook2man docbook2man docbook-to-man)
    if(DOCBOOK_TO_MAN)
        set(_EXPAT_BUILD_DOCS_DEFAULT ON)
    else()
        set(_EXPAT_BUILD_DOCS_DEFAULT OFF)
    endif()
endif()
if(MSVC)
    set(_EXPAT_BUILD_PKGCONFIG_DEFAULT OFF)
else()
    set(_EXPAT_BUILD_PKGCONFIG_DEFAULT ON)
endif()
if(DEFINED BUILD_SHARED_LIBS)
    set(_EXPAT_SHARED_LIBS_DEFAULT ${BUILD_SHARED_LIBS})
else()
    set(_EXPAT_SHARED_LIBS_DEFAULT ON)
endif()
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE NoConfig)  # so that accessing CMAKE_*_POSTFIX will be waterproof
endif()
string(TOUPPER "${CMAKE_BUILD_TYPE}" _EXPAT_BUILD_TYPE_UPPER)

#
# Configuration
#
option(EXPAT_BUILD_TOOLS "build the xmlwf tool for expat library" ${_EXPAT_BUILD_TOOLS_DEFAULT})
option(EXPAT_BUILD_EXAMPLES "build the examples for expat library" ON)
option(EXPAT_BUILD_TESTS "build the tests for expat library" ON)
option(EXPAT_SHARED_LIBS "build a shared expat library" ${_EXPAT_SHARED_LIBS_DEFAULT})
option(EXPAT_BUILD_DOCS "build man page for xmlwf" ${_EXPAT_BUILD_DOCS_DEFAULT})
option(EXPAT_BUILD_FUZZERS "build fuzzers for the expat library" OFF)
option(EXPAT_BUILD_PKGCONFIG "build pkg-config file" ${_EXPAT_BUILD_PKGCONFIG_DEFAULT})
option(EXPAT_OSSFUZZ_BUILD "build fuzzers via ossfuzz for the expat library" OFF)
if(UNIX OR _EXPAT_HELP)
    option(EXPAT_WITH_LIBBSD "utilize libbsd (for arc4random_buf)" OFF)
endif()
option(EXPAT_ENABLE_INSTALL "install expat files in cmake install target" ON)
set(EXPAT_CONTEXT_BYTES 1024 CACHE STRING "Define to specify how much context to retain around the current parse point")
mark_as_advanced(EXPAT_CONTEXT_BYTES)
option(EXPAT_DTD "Define to make parameter entity parsing functionality available" ON)
mark_as_advanced(EXPAT_DTD)
option(EXPAT_NS "Define to make XML Namespaces functionality available" ON)
mark_as_advanced(EXPAT_NS)
option(EXPAT_WARNINGS_AS_ERRORS "Treat all compiler warnings as errors" OFF)
if(UNIX OR _EXPAT_HELP)
    option(EXPAT_DEV_URANDOM "Define to include code reading entropy from `/dev/urandom'." ON)
    set(EXPAT_WITH_GETRANDOM "AUTO" CACHE STRING
            "Make use of getrandom function (ON|OFF|AUTO) [default=AUTO]")
    set(EXPAT_WITH_SYS_GETRANDOM "AUTO" CACHE STRING
            "Make use of syscall SYS_getrandom (ON|OFF|AUTO) [default=AUTO]")
    mark_as_advanced(EXPAT_DEV_URANDOM)
endif()
set(EXPAT_CHAR_TYPE "char" CACHE STRING "Character type to use (char|ushort|wchar_t) [default=char]")
option(EXPAT_ATTR_INFO "Define to allow retrieving the byte offsets for attribute names and values" OFF)
mark_as_advanced(EXPAT_ATTR_INFO)
option(EXPAT_LARGE_SIZE "Make XML_GetCurrent* functions return <(unsigned) long long> rather than <(unsigned) long>" OFF)
mark_as_advanced(EXPAT_LARGE_SIZE)
option(EXPAT_MIN_SIZE "Get a smaller (but slower) parser (in particular avoid multiple copies of the tokenizer)" OFF)
mark_as_advanced(EXPAT_MIN_SIZE)
if(MSVC OR _EXPAT_HELP)
    set(EXPAT_MSVC_STATIC_CRT OFF CACHE BOOL "Use /MT flag (static CRT) when compiling in MSVC")
endif()
if(NOT _EXPAT_HELP)
    set(_EXPAT_M32 OFF CACHE BOOL "(Unofficial!) Produce 32bit code with -m32")
endif()

if(EXPAT_BUILD_TESTS)
    # We have to call enable_language() before modifying any CMAKE_CXX_* variables
    enable_language(CXX)
endif()

#
# Environment checks
#
if(EXPAT_WITH_LIBBSD)
    find_library(LIB_BSD NAMES bsd)
    if(NOT LIB_BSD)
        message(SEND_ERROR "EXPAT_WITH_LIBBSD option is enabled, but libbsd was not found")
    else()
        set(HAVE_LIBBSD TRUE)
    endif()
endif()

if(MSVC)
    # For the three types of MSVC version values, please see:
    # - https://cmake.org/cmake/help/latest/variable/MSVC_VERSION.html
    # - https://sourceforge.net/p/predef/wiki/Compilers/
    # - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio#History
    set(_EXPAT_MSVC_REQUIRED_INT 1800)  # i.e. 12.0/2013/1800; see PR #426
    set(_EXPAT_MSVC_SUPPORTED_INT 1910)
    set(_EXPAT_MSVC_SUPPORTED_DISPLAY "Visual Studio 15.0/2017/${_EXPAT_MSVC_SUPPORTED_INT}")

    if(MSVC_VERSION VERSION_LESS ${_EXPAT_MSVC_SUPPORTED_INT})
        if(MSVC_VERSION VERSION_LESS ${_EXPAT_MSVC_REQUIRED_INT})
            message(SEND_ERROR "MSVC_VERSION ${MSVC_VERSION} is TOO OLD to compile Expat without errors.")
            message(SEND_ERROR "Please use officially supported ${_EXPAT_MSVC_SUPPORTED_DISPLAY} or later.  Thank you!")
        else()
            message(WARNING "MSVC_VERSION ${MSVC_VERSION} is NOT OFFICIALLY SUPPORTED by Expat.")
            message(WARNING "Please use ${_EXPAT_MSVC_SUPPORTED_DISPLAY} or later.  Thank you!")
        endif()
    endif()
endif()

macro(_expat_copy_bool_int source_ref dest_ref)
    if(${source_ref})
        set(${dest_ref} 1)
    else()
        set(${dest_ref} 0)
    endif()
endmacro()

if(EXPAT_LARGE_SIZE)
    add_definitions(-DXML_LARGE_SIZE)
endif()

if(EXPAT_MIN_SIZE)
    add_definitions(-DXML_MIN_SIZE)
endif()

if(EXPAT_CHAR_TYPE STREQUAL "char")
    set(_EXPAT_UNICODE OFF)
    set(_EXPAT_UNICODE_WCHAR_T OFF)
elseif(EXPAT_CHAR_TYPE STREQUAL "ushort")
    set(_EXPAT_UNICODE ON)
    set(_EXPAT_UNICODE_WCHAR_T OFF)
    if(EXPAT_BUILD_EXAMPLES)
        message(SEND_ERROR "Examples can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_EXAMPLES=OFF.")
    endif()
    if(EXPAT_BUILD_TESTS)
        message(SEND_ERROR "The testsuite can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_TESTS=OFF.")
    endif()
    if(EXPAT_BUILD_TOOLS)
        message(SEND_ERROR "The xmlwf tool can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_TOOLS=OFF.")
    endif()
elseif(EXPAT_CHAR_TYPE STREQUAL "wchar_t")
    set(_EXPAT_UNICODE ON)
    set(_EXPAT_UNICODE_WCHAR_T ON)
    if(NOT WIN32)
        string(FIND "${CMAKE_C_FLAGS}" "-fshort-wchar" _expat_short_wchar_found)
        if(${_expat_short_wchar_found} EQUAL "-1")
            message(SEND_ERROR "Configuration -DEXPAT_CHAR_TYPE=wchar_t requires -DCMAKE_{C,CXX}_FLAGS=-fshort-wchar (which was not found) and libc compiled with -fshort-wchar, too.")
        endif()
        if (EXPAT_BUILD_TOOLS)
            message(SEND_ERROR "The xmlwf tool can not be built with option -DEXPAT_CHAR_TYPE=wchar_t outside of Windows. Please pass -DEXPAT_CHAR_TYPE=char or -DEXPAT_BUILD_TOOLS=OFF.")
        endif()
    endif()
else()
    message(SEND_ERROR "Option -DEXPAT_CHAR_TYPE=(char|ushort|wchar_t) cannot be \"${EXPAT_CHAR_TYPE}\".")
endif()

if(_EXPAT_UNICODE)
    add_definitions(-DXML_UNICODE)              # for unsigned short
    if(_EXPAT_UNICODE_WCHAR_T)
        add_definitions(-DXML_UNICODE_WCHAR_T)  # for wchar_t
    endif()
endif()

include(${CMAKE_CURRENT_LIST_DIR}/ConfigureChecks.cmake)

macro(evaluate_detection_results use_ref have_ref thing_lower thing_title)
    if(${use_ref} AND NOT (${use_ref} STREQUAL "AUTO") AND NOT ${have_ref})
        message(SEND_ERROR
                "Use of ${thing_lower} was enforced by ${use_ref}=ON but it could not be found.")
    elseif(NOT ${use_ref} AND ${have_ref})
        message("${thing_title} was found but it will not be used due to ${use_ref}=OFF.")
        set(${have_ref} 0)
    endif()
endmacro()

if(NOT WIN32)
    evaluate_detection_results(EXPAT_WITH_GETRANDOM HAVE_GETRANDOM "function getrandom" "Function getrandom")
    evaluate_detection_results(EXPAT_WITH_SYS_GETRANDOM HAVE_SYSCALL_GETRANDOM "syscall SYS_getrandom" "Syscall SYS_getrandom")
endif()

_expat_copy_bool_int(EXPAT_ATTR_INFO        XML_ATTR_INFO)
_expat_copy_bool_int(EXPAT_DTD              XML_DTD)
_expat_copy_bool_int(EXPAT_LARGE_SIZE       XML_LARGE_SIZE)
_expat_copy_bool_int(EXPAT_MIN_SIZE         XML_MIN_SIZE)
_expat_copy_bool_int(EXPAT_NS               XML_NS)
if(NOT WIN32)
    _expat_copy_bool_int(EXPAT_DEV_URANDOM  XML_DEV_URANDOM)
endif()
set(XML_CONTEXT_BYTES ${EXPAT_CONTEXT_BYTES})

macro(expat_install)
    if(EXPAT_ENABLE_INSTALL)
        install(${ARGN})
    endif()
endmacro()

configure_file(expat_config.h.cmake "${CMAKE_CURRENT_BINARY_DIR}/expat_config.h")
expat_install(FILES "${CMAKE_CURRENT_BINARY_DIR}/expat_config.h" DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})


set(EXTRA_COMPILE_FLAGS)
if(FLAG_NO_STRICT_ALIASING)
    set(EXTRA_COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} -fno-strict-aliasing")
endif()
if(FLAG_VISIBILITY)
  add_definitions(-DXML_ENABLE_VISIBILITY=1)
  set(EXTRA_COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} -fvisibility=hidden")
endif()
if(MINGW)
    # Without __USE_MINGW_ANSI_STDIO the compiler produces a false positive
    set(EXTRA_COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} -Wno-pedantic-ms-format")
endif()
if (EXPAT_WARNINGS_AS_ERRORS)
    if(MSVC)
        add_definitions(/WX)
    else()
        set(EXTRA_COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} -Werror")
    endif()
endif()
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${EXTRA_COMPILE_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${EXTRA_COMPILE_FLAGS}")

if (MSVC)
    if (EXPAT_MSVC_STATIC_CRT)
        message("-- Using static CRT ${EXPAT_MSVC_STATIC_CRT}")
        foreach(flag_var
                CMAKE_CXX_FLAGS_${_EXPAT_BUILD_TYPE_UPPER}
                CMAKE_CXX_FLAGS_DEBUG
                CMAKE_CXX_FLAGS_RELEASE
                CMAKE_CXX_FLAGS_MINSIZEREL
                CMAKE_CXX_FLAGS_RELWITHDEBINFO
                CMAKE_C_FLAGS_${_EXPAT_BUILD_TYPE_UPPER}
                CMAKE_C_FLAGS_DEBUG
                CMAKE_C_FLAGS_RELEASE
                CMAKE_C_FLAGS_MINSIZEREL
                CMAKE_C_FLAGS_RELWITHDEBINFO
                )
            string(REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
        endforeach()
    endif()
endif()

if(_EXPAT_M32 AND NOT MSVC)
    foreach(flag_var
            CMAKE_CXX_FLAGS_${_EXPAT_BUILD_TYPE_UPPER}
            CMAKE_CXX_FLAGS_DEBUG
            CMAKE_CXX_FLAGS_RELEASE
            CMAKE_CXX_FLAGS_MINSIZEREL
            CMAKE_CXX_FLAGS_RELWITHDEBINFO
            CMAKE_C_FLAGS_${_EXPAT_BUILD_TYPE_UPPER}
            CMAKE_C_FLAGS_DEBUG
            CMAKE_C_FLAGS_RELEASE
            CMAKE_C_FLAGS_MINSIZEREL
            CMAKE_C_FLAGS_RELWITHDEBINFO
            )
        set(${flag_var} "${${flag_var}} -m32")
    endforeach()
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/lib)
if(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -wd4996)
endif()

#
# Library filename postfix
#
if(_EXPAT_UNICODE)
    set(_POSTFIX_WIDE "w")
endif()

if(MSVC AND NOT EXPAT_SHARED_LIBS)
    if(EXPAT_MSVC_STATIC_CRT)
        set(_POSTFIX_CRT "MT")
    else()
        set(_POSTFIX_CRT "MD")
    endif()
endif()

foreach(postfix_var
        CMAKE_${_EXPAT_BUILD_TYPE_UPPER}_POSTFIX
        CMAKE_DEBUG_POSTFIX
        CMAKE_RELEASE_POSTFIX
        CMAKE_MINSIZEREL_POSTFIX
        CMAKE_RELWITHDEBINFO_POSTFIX
        )
    if(WIN32 AND postfix_var STREQUAL "CMAKE_DEBUG_POSTFIX")
        set(_POSTFIX_DEBUG "d")
    else()
        set(_POSTFIX_DEBUG "")  # needs a reset because of being looped
    endif()

    set(${postfix_var} "${_POSTFIX_WIDE}${_POSTFIX_DEBUG}${_POSTFIX_CRT}" CACHE STRING "Library filename postfix, e.g. libexpat<postfix=[w][d][MD|MT]>.lib")
endforeach()

#
# C library
#
set(_EXPAT_C_SOURCES
    lib/xmlparse.c
    lib/xmlrole.c
    lib/xmltok.c
# NOTE: ISO C forbids an empty translation unit
#   lib/xmltok_impl.c
#   lib/xmltok_ns.c
)

if(EXPAT_SHARED_LIBS)
    set(_SHARED SHARED)
    if(MSVC)
        set(_EXPAT_EXTRA_SOURCES ${_EXPAT_EXTRA_SOURCES} lib/libexpat.def)
    endif()
    if(WIN32)
        # Add DLL version
        string(REPLACE "." "," _EXPAT_DLL_VERSION ${PROJECT_VERSION}.0)
        set(_EXPAT_EXTRA_SOURCES ${_EXPAT_EXTRA_SOURCES} win32/version.rc)
    endif()
else()
    set(_SHARED STATIC)
endif()

add_library(expat ${_SHARED} ${_EXPAT_C_SOURCES} ${_EXPAT_EXTRA_SOURCES})
if(_EXPAT_LIBM_FOUND)
    target_link_libraries(expat m)
endif()
if(EXPAT_WITH_LIBBSD)
    target_link_libraries(expat ${LIB_BSD})
endif()

set(LIBCURRENT 9)   # sync
set(LIBREVISION 8)  # with
set(LIBAGE 8)       # configure.ac!
math(EXPR LIBCURRENT_MINUS_AGE "${LIBCURRENT} - ${LIBAGE}")

if(NOT WIN32)
    set_property(TARGET expat PROPERTY VERSION ${LIBCURRENT_MINUS_AGE}.${LIBAGE}.${LIBREVISION})
    set_property(TARGET expat PROPERTY SOVERSION ${LIBCURRENT_MINUS_AGE})
    set_property(TARGET expat PROPERTY NO_SONAME ${NO_SONAME})

    if(APPLE)
        if(NOT CMAKE_VERSION VERSION_GREATER_EQUAL 3.17)
            message(FATAL_ERROR "Expat requires CMake >=3.17 on platform \"APPLE\".")
        endif()

        # NOTE: This intends to talk CMake into compatiblity with GNU Libtool
        math(EXPR _EXPAT_MACHO_COMPATIBILITY_VERSION "${LIBCURRENT} + 1")
        set(_EXPAT_MACHO_CURRENT_VERSION "${_EXPAT_MACHO_COMPATIBILITY_VERSION}.${LIBREVISION}")
        set_property(TARGET expat PROPERTY MACHO_COMPATIBILITY_VERSION ${_EXPAT_MACHO_COMPATIBILITY_VERSION})
        set_property(TARGET expat PROPERTY MACHO_CURRENT_VERSION ${_EXPAT_MACHO_CURRENT_VERSION})
    endif()
endif()
if(WIN32 AND NOT MINGW)
    # NOTE: This avoids a name collision with Expat.dll of Perl's XML::Parser::Expat
    #       on Windows by resorting to filename libexpat.dll since Expat 1.95.3.
    #       Everything but MSVC is already adding prefix "lib", automatically.
    # NOTE: "set_property(TARGET expat PROPERTY PREFIX lib)" would only affect *.dll
    #       files but not *.lib files, so we have to rely on property OUTPUT_NAME, instead.
    #       Property CMAKE_*_POSTFIX still applies.
    set(_EXPAT_OUTPUT_NAME libexpat)
    set_property(TARGET expat PROPERTY OUTPUT_NAME ${_EXPAT_OUTPUT_NAME})
else()
    set(_EXPAT_OUTPUT_NAME expat)
endif()

target_include_directories(expat
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/lib>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

if(WIN32)
    if(EXPAT_SHARED_LIBS)
        target_compile_definitions(expat PRIVATE VER_FILEVERSION=${_EXPAT_DLL_VERSION})
    else()
        target_compile_definitions(expat PUBLIC -DXML_STATIC)
    endif()
endif()

expat_install(TARGETS expat EXPORT expat
                      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
                      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
                      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

expat_install(FILES lib/expat.h lib/expat_external.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

#
# pkg-config file
#
if(EXPAT_BUILD_PKGCONFIG)
    if(CMAKE_INSTALL_LIBDIR MATCHES "^/")
        set(_expat_pkgconfig_libdir "${CMAKE_INSTALL_LIBDIR}")
    else()
        set(_expat_pkgconfig_libdir "\${exec_prefix}/${CMAKE_INSTALL_LIBDIR}")
    endif()

    if(CMAKE_INSTALL_INCLUDEDIR MATCHES "^/")
        set(_expat_pkgconfig_includedir "${CMAKE_INSTALL_INCLUDEDIR}")
    else()
        set(_expat_pkgconfig_includedir "\${prefix}/${CMAKE_INSTALL_INCLUDEDIR}")
    endif()

    set_target_properties(expat PROPERTIES
        pkgconfig_prefix "${CMAKE_INSTALL_PREFIX}"
        pkgconfig_exec_prefix "\${prefix}"
        pkgconfig_libdir "${_expat_pkgconfig_libdir}"
        pkgconfig_includedir "${_expat_pkgconfig_includedir}"
        pkgconfig_version "${PACKAGE_VERSION}")

    foreach(_build_type ${CMAKE_BUILD_TYPE} Debug Release RelWithDebInfo MinSizeRel)
        string(TOLOWER "${_build_type}" _build_type_lower)
        string(TOUPPER "${_build_type}" _build_type_upper)
        set_property(TARGET expat PROPERTY "pkgconfig_${_build_type_lower}_name" "expat${CMAKE_${_build_type_upper}_POSTFIX}")
        set_property(TARGET expat PROPERTY "pkgconfig_${_build_type_lower}_output_name" "${_EXPAT_OUTPUT_NAME}${CMAKE_${_build_type_upper}_POSTFIX}")
        if(_EXPAT_LIBM_FOUND)
            set_property(TARGET expat PROPERTY "pkgconfig_libm" "-lm")
        else()
            set_property(TARGET expat PROPERTY "pkgconfig_libm" "")
        endif()
    endforeach()

    file(GENERATE
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/$<CONFIG>/expat.pc
        INPUT ${PROJECT_SOURCE_DIR}/expat.pc.cmake)

    expat_install(FILES ${CMAKE_CURRENT_BINARY_DIR}/$<CONFIG>/expat.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif()

#
# C command line tool xmlwf
#
if(EXPAT_BUILD_TOOLS)
    set(xmlwf_SRCS
        xmlwf/xmlwf.c
        xmlwf/xmlfile.c
        xmlwf/codepage.c
        xmlwf/readfilemap.c
    )

    add_executable(xmlwf ${xmlwf_SRCS})
    set_property(TARGET xmlwf PROPERTY RUNTIME_OUTPUT_DIRECTORY xmlwf)
    target_link_libraries(xmlwf expat)
    if(_EXPAT_LIBM_FOUND)
        target_link_libraries(xmlwf m)
    endif()
    expat_install(TARGETS xmlwf DESTINATION ${CMAKE_INSTALL_BINDIR})

    if(MINGW AND _EXPAT_UNICODE_WCHAR_T)
        # https://gcc.gnu.org/onlinedocs/gcc/x86-Windows-Options.html
        set_target_properties(xmlwf PROPERTIES LINK_FLAGS -municode)
    endif()

    if(EXPAT_BUILD_DOCS)
        file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/doc)
        add_custom_target(
            xmlwf-manpage
            COMMAND
                "${DOCBOOK_TO_MAN}" "${PROJECT_SOURCE_DIR}/doc/xmlwf.xml" && mv "XMLWF.1" "${PROJECT_BINARY_DIR}/doc/xmlwf.1"
            BYPRODUCTS
                doc/xmlwf.1)
        add_dependencies(expat xmlwf-manpage)
        expat_install(FILES "${PROJECT_BINARY_DIR}/doc/xmlwf.1" DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
    elseif(EXISTS ${PROJECT_SOURCE_DIR}/doc/xmlwf.1)
        expat_install(FILES "${PROJECT_SOURCE_DIR}/doc/xmlwf.1" DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
    endif()
endif()

#
# C code examples
#
if(EXPAT_BUILD_EXAMPLES)
    add_executable(elements examples/elements.c)
    set_property(TARGET elements PROPERTY RUNTIME_OUTPUT_DIRECTORY examples)
    target_link_libraries(elements expat)

    add_executable(outline examples/outline.c)
    set_property(TARGET outline PROPERTY RUNTIME_OUTPUT_DIRECTORY examples)
    target_link_libraries(outline expat)
endif()

#
# C/C++ test runners
#
if(EXPAT_BUILD_TESTS)
    ## these are unittests that can be run on any platform
    enable_testing()

    set(test_SRCS
        tests/chardata.c
        tests/memcheck.c
        tests/minicheck.c
        tests/structdata.c
        ${_EXPAT_C_SOURCES}
    )

    if(NOT MSVC)
        if(MINGW)
            set(host whatever-mingw32)  # for nothing but run.sh
        endif()
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/run.sh.in run.sh @ONLY)
    endif()

    function(expat_add_test _name _file)
        if(MSVC)
            add_test(NAME ${_name} COMMAND ${_file})
        else()
            add_test(NAME ${_name} COMMAND bash run.sh ${_file})
        endif()
    endfunction()

    add_executable(runtests tests/runtests.c ${test_SRCS})
    set_property(TARGET runtests PROPERTY RUNTIME_OUTPUT_DIRECTORY tests)
    expat_add_test(runtests $<TARGET_FILE:runtests>)

    add_executable(runtestspp tests/runtestspp.cpp ${test_SRCS})
    set_property(TARGET runtestspp PROPERTY RUNTIME_OUTPUT_DIRECTORY tests)
    expat_add_test(runtestspp $<TARGET_FILE:runtestspp>)

    if(_EXPAT_LIBM_FOUND)
        target_link_libraries(runtests m)
        target_link_libraries(runtestspp m)
    endif()

    if(EXPAT_WITH_LIBBSD)
        target_link_libraries(runtests ${LIB_BSD})
        target_link_libraries(runtestspp ${LIB_BSD})
    endif()
endif()

if(EXPAT_BUILD_FUZZERS)
    if(NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
        message(SEND_ERROR
            "Building fuzz targets without Clang (but ${CMAKE_C_COMPILER_ID}) "
            "is not supported. Please set "
            "-DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++.")
    endif()

    string(FIND "${CMAKE_C_FLAGS}" "-fsanitize" sanitizer_present)
    if(${sanitizer_present} EQUAL "-1")
        message(WARNING
            "There was no sanitizer present when building the fuzz targets. "
            "This is likely in error - consider adding "
            "-DCMAKE_C_FLAGS='-fsanitize=<sanitizer>' and "
            "-DCMAKE_CXX_FLAGS='-fsanitize=<sanitizer>' and "
            "-DCMAKE_EXE_LINKER_FLAGS='-fsanitize=<sanitizer>' and "
            "-DCMAKE_MODULE_LINKER_FLAGS='-fsanitize=<sanitizer>' and "
            "-DCMAKE_SHARED_LINKER_FLAGS='-fsanitize=<sanitizer>' to your cmake "
            "execution.")
    endif()

    if(EXPAT_OSSFUZZ_BUILD AND NOT DEFINED ENV{LIB_FUZZING_ENGINE})
        message(SEND_ERROR
            "OSS-Fuzz builds require the environment variable "
            "LIB_FUZZING_ENGINE to be set. If you are seeing this "
            "warning, it points to a deeper problem in the ossfuzz "
            "build setup.")
    endif()

    set(encoding_types UTF-16 UTF-8 ISO-8859-1 US-ASCII UTF-16BE UTF-16LE)
    set(fuzz_targets xml_parse_fuzzer xml_parsebuffer_fuzzer)

    add_library(fuzzpat STATIC ${_EXPAT_C_SOURCES})
    if(NOT EXPAT_OSSFUZZ_BUILD)
        target_compile_options(fuzzpat PRIVATE -fsanitize=fuzzer-no-link)
    endif()

    foreach(fuzz_target ${fuzz_targets})
        foreach(encoding_type ${encoding_types})
            set(target_name ${fuzz_target}_${encoding_type})
            add_executable(${target_name} fuzz/${fuzz_target}.c)
            target_link_libraries(${target_name} fuzzpat)
            target_compile_definitions(${target_name}
                PRIVATE ENCODING_FOR_FUZZING=${encoding_type})
            if(NOT EXPAT_OSSFUZZ_BUILD)
                target_compile_options(${target_name} PRIVATE -fsanitize=fuzzer-no-link)
            endif()
            # NOTE: Avoiding target_link_options here only because it needs CMake >=3.13
            if(EXPAT_OSSFUZZ_BUILD)
                set_target_properties(${target_name} PROPERTIES LINK_FLAGS $ENV{LIB_FUZZING_ENGINE})
                set_target_properties(${target_name} PROPERTIES LINKER_LANGUAGE "CXX")
            else()
                set_target_properties(${target_name} PROPERTIES LINK_FLAGS -fsanitize=fuzzer)
            endif()
            set_property(
                TARGET ${target_name} PROPERTY RUNTIME_OUTPUT_DIRECTORY fuzz)
        endforeach()
    endforeach()
else()
    if(EXPAT_OSSFUZZ_BUILD)
        message(SEND_ERROR
                "Attempting to perform an ossfuzz build without turning on the fuzzer build. "
                "This is likely in error - consider adding "
                "-DEXPAT_BUILD_FUZZERS=ON to your cmake execution.")
    endif()
endif()

#
# Custom target "run-xmltest"
#
if(EXPAT_BUILD_TOOLS AND NOT MSVC)
    add_custom_target(
        xmlts-zip-downloaded
        COMMAND
            sh -c 'test -f xmlts.zip || wget --output-document=xmlts.zip https://www.w3.org/XML/Test/xmlts20080827.zip'
        BYPRODUCTS
            tests/xmlts.zip
        WORKING_DIRECTORY
            tests/)

    add_custom_target(
        xmlts-zip-extracted
        COMMAND
            sh -c 'test -d xmlconf || unzip -q xmlts.zip'
        BYPRODUCTS
            tests/xmlconf
        WORKING_DIRECTORY
            tests/)
    add_dependencies(xmlts-zip-extracted xmlts-zip-downloaded)

    add_custom_target(
        xmltest-sh-been-run
        COMMAND
            sh -c '${CMAKE_CURRENT_SOURCE_DIR}/tests/xmltest.sh "bash ${CMAKE_CURRENT_BINARY_DIR}/run.sh $<TARGET_FILE:xmlwf>" 2>&1 | tee tests/xmltest.log'
        BYPRODUCTS
            tests/xmltest.log)
    add_dependencies(xmltest-sh-been-run xmlts-zip-extracted xmlwf)

    add_custom_target(
        xmltest-log-fixed
        COMMAND
            ${CMAKE_CURRENT_SOURCE_DIR}/fix-xmltest-log.sh tests/xmltest.log
        DEPENDS
            tests/xmltest.log)
    add_dependencies(xmltest-log-fixed xmltest-sh-been-run)

    add_custom_target(
        xmltest-log-verified
        COMMAND
            diff -u ${CMAKE_CURRENT_SOURCE_DIR}/tests/xmltest.log.expected tests/xmltest.log)
    add_dependencies(xmltest-log-verified xmltest-log-fixed)

    add_custom_target(run-xmltest)
    add_dependencies(run-xmltest xmltest-log-verified)
endif()

#
# Documentation
#
configure_file(Changes changelog COPYONLY)
expat_install(
    FILES
        AUTHORS
        ${CMAKE_CURRENT_BINARY_DIR}/changelog
    DESTINATION
        ${CMAKE_INSTALL_DOCDIR})

#
# CMake files for find_package(expat [..] CONFIG [..])
#
configure_package_config_file(
        cmake/expat-config.cmake.in
        cmake/expat-config.cmake
    INSTALL_DESTINATION
        ${CMAKE_INSTALL_LIBDIR}/cmake/expat-${PROJECT_VERSION}/
)
write_basic_package_version_file(
    cmake/expat-config-version.cmake
    COMPATIBILITY SameMajorVersion  # i.e. semver
)
export(
    TARGETS
        expat
    FILE
        cmake/expat-targets.cmake  # not going to be installed
)
expat_install(
    FILES
        ${CMAKE_CURRENT_BINARY_DIR}/cmake/expat-config.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/cmake/expat-config-version.cmake
    DESTINATION
        ${CMAKE_INSTALL_LIBDIR}/cmake/expat-${PROJECT_VERSION}/
)
expat_install(
    EXPORT
        expat
    DESTINATION
        ${CMAKE_INSTALL_LIBDIR}/cmake/expat-${PROJECT_VERSION}/
    NAMESPACE
        expat::
)

#
# CPack
#

# This effectively disables target "package_source".
# That is done due to CPack's unfortunate choice of an exclusion list
# rather than inclusion list.  An exclusion list does not protect against
# unwanted files ending up in the resulting archive in a way that's
# safe to run from an Expat developer's machine.
set(CPACK_SOURCE_GENERATOR '')

if(WIN32)
    set(CPACK_GENERATOR ZIP)
else()
    set(CPACK_GENERATOR TGZ)
endif()

include(CPack)

#
# Summary
#
if(EXPAT_CHAR_TYPE STREQUAL "char")
    set(_EXPAT_CHAR_TYPE_SUMMARY "char (UTF-8)")
elseif(EXPAT_CHAR_TYPE STREQUAL "ushort")
    set(_EXPAT_CHAR_TYPE_SUMMARY "ushort (unsigned short, UTF-16)")
elseif(EXPAT_CHAR_TYPE STREQUAL "wchar_t")
    if(WIN32)
        set(_EXPAT_CHAR_TYPE_SUMMARY "wchar_t (UTF-16)")
    else()
        set(_EXPAT_CHAR_TYPE_SUMMARY "wchar_t (UTF-32)  // not implemented")
    endif()
else()
    set(_EXPAT_CHAR_TYPE_SUMMARY "ERROR")
endif()
# NOTE: We're not accessing global property GENERATOR_IS_MULTI_CONFIG
#       because that would require CMake >=3.9
if(CMAKE_CONFIGURATION_TYPES)
    set(_EXPAT_GENERATOR_IS_MULTI_CONFIG TRUE)
else()
    set(_EXPAT_GENERATOR_IS_MULTI_CONFIG FALSE)
endif()

message(STATUS "===========================================================================")
message(STATUS "")
message(STATUS "Configuration")
message(STATUS "  Generator .................. ${CMAKE_GENERATOR}")
if(_EXPAT_GENERATOR_IS_MULTI_CONFIG)
    message(STATUS "  Build types ................ ${CMAKE_CONFIGURATION_TYPES}")
else()
    message(STATUS "  Build type ................. ${CMAKE_BUILD_TYPE}")
endif()
message(STATUS "  Prefix ..................... ${CMAKE_INSTALL_PREFIX}")
message(STATUS "  Shared libraries ........... ${EXPAT_SHARED_LIBS}")
if(MSVC)
    message(STATUS "  Static CRT ................. ${EXPAT_MSVC_STATIC_CRT}")
endif()
message(STATUS "  Character type ............. ${_EXPAT_CHAR_TYPE_SUMMARY}")
if(NOT _EXPAT_GENERATOR_IS_MULTI_CONFIG)
    message(STATUS "  Library name postfix ....... ${CMAKE_${_EXPAT_BUILD_TYPE_UPPER}_POSTFIX}")
endif()
message(STATUS "")
message(STATUS "  Build documentation ........ ${EXPAT_BUILD_DOCS}")
message(STATUS "  Build examples ............. ${EXPAT_BUILD_EXAMPLES}")
message(STATUS "  Build fuzzers .............. ${EXPAT_BUILD_FUZZERS}")
message(STATUS "  Build tests ................ ${EXPAT_BUILD_TESTS}")
message(STATUS "  Build tools (xmlwf) ........ ${EXPAT_BUILD_TOOLS}")
message(STATUS "  Build pkg-config file ...... ${EXPAT_BUILD_PKGCONFIG}")
message(STATUS "  Install files .............. ${EXPAT_ENABLE_INSTALL}")
message(STATUS "")
message(STATUS "  Features")
message(STATUS "    // Advanced options, changes not advised")
message(STATUS "    Attributes info .......... ${EXPAT_ATTR_INFO}")
message(STATUS "    Context bytes ............ ${EXPAT_CONTEXT_BYTES}")
message(STATUS "    DTD support .............. ${EXPAT_DTD}")
message(STATUS "    Large size ............... ${EXPAT_LARGE_SIZE}")
message(STATUS "    Minimum size ............. ${EXPAT_MIN_SIZE}")
message(STATUS "    Namespace support ........ ${EXPAT_NS}")
message(STATUS "")
message(STATUS "  Entropy sources")
if(WIN32)
    message(STATUS "    rand_s ................... ON")
else()
    message(STATUS "    getrandom ................ ${HAVE_GETRANDOM}")
    message(STATUS "    syscall SYS_getrandom .... ${HAVE_SYSCALL_GETRANDOM}")
    message(STATUS "    libbsd ................... ${EXPAT_WITH_LIBBSD}")
    message(STATUS "    /dev/random .............. ${EXPAT_DEV_URANDOM}")
endif()
message(STATUS "")
if(CMAKE_GENERATOR STREQUAL "Unix Makefiles")
    message(STATUS "Continue with")
    message(STATUS "  make")
    if(EXPAT_BUILD_TESTS)
        message(STATUS "  make test")
    endif()
    if(EXPAT_ENABLE_INSTALL)
        message(STATUS "  sudo make install")
    endif()
    message(STATUS "")
endif()
message(STATUS "===========================================================================")
