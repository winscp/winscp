
[![Travis CI Build Status](https://travis-ci.org/notroj/neon.svg?branch=0.31.x)](https://travis-ci.org/notroj/neon)

# neon

_neon_ is an HTTP and WebDAV client library, with a C language API.

Mailing list: neon@lists.manyfish.co.uk || Web site: https://notroj.github.io/neon/

The neon API and ABI are stable and maintain backwards compatibility
from 0.27.x through 0.31.x.

Features:

 - High-level interface to HTTP and WebDAV methods.
 - Low-level interface to HTTP request handling, to allow implementing
   new methods easily.
 - Persistent connection support (HTTP/1.1 and HTTP/1.0 aware)
 - Basic and digest authentication (RFC2617) (including auth-int, md5-sess)
 - Proxy support (including basic/digest authentication)
 - SSL/TLS support using OpenSSL (including client certificate support)
 - Generic WebDAV 207 XML response handling mechanism
 - XML parsing using expat or libxml (1.x or 2.x) parser
 - Easy generation of error messages from 207 error responses
 - Basic HTTP/1.1 methods: GET, PUT, HEAD, OPTIONS, conditional PUT
 - WebDAV resource manipulation: MOVE, COPY, DELETE, MKCOL.
 - WebDAV metadata support: set and remove properties (PROPPATCH), query
   any set of properties (PROPFIND).
 - WebDAV locking support
 - Autoconf macros supplied for easily embedding neon directly inside 
   an application source tree.

Provides lower-level interfaces to directly implement new HTTP
methods, and higher-level interfaces so that you don't have to worry
about the lower-level stuff.

The neon library source code is licensed under the GNU Library GPL;
see src/COPYING.LIB for full details.  The manual and test suite are
licensed under the terms of the GNU GPL; see test/COPYING for terms.
The autoconf macros in the "macros" directory are under a less
restrictive license, see each file for details.

~~~
neon is Copyright (C) 1999-2008 Joe Orton
Portions are:
Copyright (C) Aleix Conchillo Flaque
Copyright (C) Arfrever Frehtes Taifersar Arahesis
Copyright (C) Arun Garg
Copyright (C) Daniel Stenberg
Copyright (C) Free Software Foundation, Inc.
Copyright (C) Henrik Holst
Copyright (C) Jiang Lei
Copyright (C) Kai Sommerfeld
Copyright (C) Karl Ove Hufthammer.
Copyright (C) Michael Sobolev
Copyright (C) Nobuyuki Tsuchimura
Copyright (C) Sylvain Glaize
Copyright (C) Thomas Schultz
Copyright (C) Vladimir Berezniker
Copyright (C) Yves Martin
~~~
