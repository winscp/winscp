/*                                                      -*- c -*-
   Win32 config.h
   Copyright (C) 1999-2000, Peter Boos <pedib@colorfullife.com>
   Copyright (C) 2002-2006, Joe Orton <joe@manyfish.co.uk>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA

*/
#if defined(_WIN32) && !defined(WIN32)
#define WIN32
#endif

#ifdef WIN32

#define NEON_VERSION "0.34.2"
#define NE_VERSION_MAJOR (0)
#define NE_VERSION_MINOR (34)

#define HAVE_ERRNO_H
#define HAVE_LIMITS_H
#define HAVE_STDLIB_H
#define HAVE_STRING_H

#define HAVE_MEMCPY
#define HAVE_SETSOCKOPT

#define HAVE_SSPI

#define NE_HAVE_TS_SSL 1

#define HAVE_INET_NTOP 1
#define HAVE_INET_PTON 1
#define HAVE_IF_INDEXTONAME 1
#define HAVE_IF_NAMETOINDEX 1
#define USE_GETADDRINFO
#define USE_GAI_ADDRCONFIG
#define NE_HAVE_IPV6 // doesn't really have any practival effect

/* Define to enable debugging */
#define NE_DEBUGGING 1

#ifdef _WIN64
#define NE_FMT_SIZE_T "I64u"
#define NE_FMT_SSIZE_T "I64d"
#else
#define NE_FMT_SIZE_T "u"
#define NE_FMT_SSIZE_T "d"
#endif
#define NE_FMT_OFF_T "ld"
#define NE_FMT_OFF64_T "I64d"
#define NE_FMT_NE_OFF_T NE_FMT_OFF_T

#ifndef NE_FMT_XML_SIZE
#define NE_FMT_XML_SIZE "d"
#endif

#define SIZEOF_INT 4
#define SIZEOF_LONG 4

/* Win32 uses a underscore, so we use a macro to eliminate that. */
/* VS2015 has this already defined */
#if (_MSC_VER < 1900)
#define snprintf			_snprintf
#endif
/* VS2008 has this already defined */
#if (_MSC_VER < 1500)
#define vsnprintf			_vsnprintf
#endif

#if defined(_MSC_VER) && _MSC_VER >= 1400
#define strcasecmp			_strcmpi
#define strncasecmp			_strnicmp
#else
#define strcasecmp			strcmpi
#define strncasecmp			strnicmp
#endif
#if defined(_MSC_VER) && _MSC_VER >= 1300
#define HAVE_STRTOLL
/* VS2013 has this already defined */
#if _MSC_VER < 1800
#define strtoll				_strtoi64
#endif
#endif
#ifndef __BORLANDC__
#ifdef _WIN64
#define ssize_t				__int64
#else
#define ssize_t				int
#endif
#endif
/* VS2015 has this already defined */
#if defined (_MSC_VER) && (_MSC_VER < 1900)
#define inline                          __inline
#endif
#if defined(NE_LFS)
#ifdef __BORLANDC__
#define lseek64				_lseeki64
#define fstat64				_fstati64
#define stat64				stati64
#else
#define lseek64				_lseeki64
#define fstat64				_fstat64
#define stat64				__stat64
#endif
#else
#define off_t                           _off_t
#endif

#ifndef USE_GETADDRINFO
#define in_addr_t                       unsigned int
#endif

// WINSCP
#define HAVE_CRYPTO_SET_IDPTR_CALLBACK
#define NE_FMT_TIME_T "ld"

typedef int socklen_t;

#include <io.h>
#define read _read

#endif
