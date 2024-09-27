@echo off

setlocal

set ANY_CLEANED=0

rem ==== OpenSSL ====

if exist openssl\tmp (
    echo Cleaning up OpenSSL ...
    rmdir /s /q openssl\tmp
    set ANY_CLEANED=1
)

rem ==== Expat ====

if exist expat\bcb5\release (
    echo Cleaning up Expat ...
    rmdir /s /q expat\bcb5\release
    set ANY_CLEANED=1
)

rem ==== neon ====

if exist neon\tmp (
    echo Cleaning up neon ...
    rmdir /s /q neon\tmp
    set ANY_CLEANED=1
)

rem ==== PuTTYVS ====

if exist puttyvs\PuTTYVS (
    echo Cleaning up PuTTYVS ...
    rmdir /s /q puttyvs\PuTTYVS
    set ANY_CLEANED=1
)

rem ==== libs3 ====

if exist libs3\tmp (
    echo Cleaning up libs3 ...
    rmdir /s /q libs3\tmp
    set ANY_CLEANED=1
)

rem ==== MFC ====

set CLEAN_MFC=0
if exist mfc\source\$UW set CLEAN_MFC=1
if exist mfc\source\bor.rsp set CLEAN_MFC=1
if %CLEAN_MFC% == 1 (
    echo Cleaning up MFC ...
    if exist mfc\source\$UW rmdir /s /q mfc\source\$UW
    if exist mfc\source\bor.rsp del mfc\source\bor.rsp
    set ANY_CLEANED=1
)

if %ANY_CLEANED% == 1 (
    echo All done
) else (
    echo No clean needed
)
