@echo off
if "%BUILDTOOLS%" == "" echo BUILDTOOLS not set & exit /B 1

set LIB_PATH=%INTERM_PATH%\Win32

if not exist %LIB_PATH% mkdir %LIB_PATH%

rem ==== OpenSSL ====

if exist %LIB_PATH%\libeay32.lib (
echo OpenSSL already built
goto SKIP_OPENSSL
)

echo Building OpenSSL ...
cd openssl
make
cd ..

if not exist %LIB_PATH%\libeay32.lib (
echo OpenSSL build failed
exit /B 1
)

:SKIP_OPENSSL

rem ==== Expat ====

if exist %LIB_PATH%\libexpats_mtd.lib (
echo Expat already built
goto SKIP_EXPAT
)

echo Building Expat ...
cd expat\bcb5
make -f makefile.mak
cd ..\..

if not exist expat\bcb5\release\libexpats_mtd.lib (
echo Expat build failed
exit /B 1
)

copy expat\bcb5\release\libexpats_mtd.lib %LIB_PATH%

:SKIP_EXPAT

rem ==== neon ====

if exist %LIB_PATH%\neon.lib (
echo neon already built
goto SKIP_NEON
)

echo Building neon ...
cd neon
make -f Makefile.bcb all
cd ..

if not exist %LIB_PATH%\neon.lib (
echo neon build failed
exit /B 1
)

:SKIP_NEON

rem ==== PuTTY VS ====

if exist %LIB_PATH%\PuTTYVS.lib (
echo PuTTYVS already built
goto SKIP_PUTTYVS
)

echo Building PuTTYVS ...
cd puttyvs
call build.bat
cd ..

if not exist %LIB_PATH%\PuTTYVS.lib (
echo PuTTYVS build failed
exit /B 1
)

:SKIP_PUTTYVS

rem ==== libs3 ====

if exist %LIB_PATH%\libs3.lib (
echo libs3 already built
goto SKIP_LIBS3
)

echo Building libs3 ...
cd libs3
make all
cd ..

if not exist %LIB_PATH%\libs3.lib (
echo libs3 build failed
exit /B 1
)

:SKIP_LIBS3

rem ==== MFC ====

if exist %LIB_PATH%\UafxcW.lib (
echo MFC already built
goto SKIP_MFC
)

echo Building MFC ...
cd mfc\source
make -fborland.mak NO_WARNINGS=1
cd ..\..

if not exist %LIB_PATH%\UafxcW.lib (
echo MFC build failed
exit /B 1
)

:SKIP_MFC

echo All done
