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
