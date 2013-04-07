@echo off
if not exist lib mkdir lib

rem ==== OpenSSL ====

if exist lib\libeay32.lib (
echo OpenSSL already built
goto SKIP_OPENSSL
)

echo Building OpenSSL ...
cd openssl
make
cd ..

if not exist lib\libeay32.lib (
echo OpenSSL build failed
exit
)

:SKIP_OPENSSL

rem ==== zlib ====

if exist lib\zlib.lib (
echo zlib already built
goto SKIP_ZLIB
)

echo Building zlib ...
cd zlib
make -f Makefile.bcb all
cd ..

if not exist lib\zlib.lib (
echo zlib build failed
exit
)

:SKIP_ZLIB

rem ==== Expat ====

if exist lib\libexpats_mtd.lib (
echo Expat already built
goto SKIP_EXPAT
)

echo Building Expat ...
cd expat\bcb5
call setup.bat
make -f makefile.mak expat_static
cd ..\..

if not exist expat\bcb5\release\libexpats_mtd.lib (
echo Expat build failed
exit
)

copy expat\bcb5\release\libexpats_mtd.lib lib

:SKIP_EXPAT

rem ==== APR ====

if exist lib\apr.lib (
echo APR already built
goto SKIP_APR
)

echo Building APR ...
cd apr
make -f Makefile.bcb all
cd ..

if not exist lib\apr.lib (
echo APR build failed
exit
)

:SKIP_APR

rem ==== neon ====

if exist lib\neon.lib (
echo neon already built
goto SKIP_NEON
)

echo Building neon ...
cd neon
make -f Makefile.bcb all
cd ..

if not exist lib\neon.lib (
echo neon build failed
exit
)

:SKIP_NEON

echo All done
