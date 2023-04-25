/* 
   Standard definitions for neon headers
   Copyright (C) 2003-2021, Joe Orton <joe@manyfish.co.uk>

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

#undef NE_BEGIN_DECLS
#undef NE_END_DECLS
#ifdef __cplusplus
# define NE_BEGIN_DECLS extern "C" {
# define NE_END_DECLS }
#else
# define NE_BEGIN_DECLS /* empty */
# define NE_END_DECLS /* empty */
#endif

#ifndef NE_DEFS_H
#define NE_DEFS_H

#include <sys/types.h>

#ifdef NE_LFS
# ifdef _MSC_VER
typedef __int64 off64_t;
# endif
# ifdef __BORLANDC__
typedef __int64 off64_t;
# endif
typedef off64_t ne_off_t;
#else
typedef off_t ne_off_t;
#endif

/* define ssize_t for Win32 */
#if defined(WIN32) && !defined(ssize_t)
#ifdef _WIN64
#define ssize_t __int64
#else
#define ssize_t int
#endif
#endif

#ifdef __NETWARE__
#include <time.h> /* for time_t */
#endif

#ifdef __GNUC__
#if __GNUC__ >= 3
#ifndef NE_PRIVATE
#define NE_PRIVATE __attribute__((visibility ("hidden")))
#endif
#define ne_attribute_malloc __attribute__((malloc))
#define ne_attribute_alloc_size(x) __attribute__((alloc_size(x)))
#else
#define ne_attribute_malloc
#define ne_attribute_alloc_size(x)
#endif
#if __GNUC__ > 3
#define ne_attribute_sentinel __attribute__((sentinel))
#else
#define ne_attribute_sentinel 
#endif
#define ne_attribute(x) __attribute__(x)
#else
#define ne_attribute(x)
#define ne_attribute_malloc
#define ne_attribute_alloc_size(x)
#define ne_attribute_sentinel
#endif

#ifndef NE_PRIVATE
#define NE_PRIVATE
#endif

#ifndef NE_BUFSIZ
#define NE_BUFSIZ 65536
#endif

#ifndef NE_VAR
# if defined(_MSC_VER) && defined(NE_DLL)
#  ifdef BUILDING_NEON
#   define NE_VAR extern __declspec(dllexport)
#  else
#   define NE_VAR extern __declspec(dllimport)
#  endif
# else
#  define NE_VAR extern
# endif
#endif

#endif /* NE_DEFS_H */
