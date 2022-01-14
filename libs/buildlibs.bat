@echo off
set BUILDTOOLS_PATH=%1
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
make -f makefile.mak
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

rem ==== PuTTY VS ====

if exist lib\PuTTYVS.lib (
echo PuTTYVS already built
goto SKIP_PUTTYVS
)

echo Building PuTTYVS ...
cd puttyvs
call build.bat %BUILDTOOLS_PATH%
cd ..

if not exist lib\PuTTYVS.lib (
echo PuTTYVS build failed
exit
)

:SKIP_PUTTYVS

rem ==== libs3 ====

if exist lib\libs3.lib (
echo libs3 already built
goto SKIP_LIBS3
)

echo Building libs3 ...
cd libs3
make all
cd ..

if not exist lib\libs3.lib (
echo libs3 build failed
exit
)

:SKIP_LIBS3

rem ==== MFC ====

if exist lib\UafxcW.lib (
echo MFC already built
goto SKIP_MFC
)

echo Building MFC ...
cd mfc\source
make -fborland.mak NO_WARNINGS=1
cd ..\..

if not exist lib\UafxcW.lib (
echo MFC build failed
exit
)

:SKIP_MFC

echo All done
