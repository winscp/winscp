@echo off
rem See INSTALL.W32
call perl Configure BC-32
call ms\do_nasm
call ms\x86asm
call %~dp0\cleanup.bat

copy %~dp0\..\..\..\libs\openssl\Makefile

echo. >> crypto\opensslconf.h
echo #ifndef _timeb >> crypto\opensslconf.h
echo #define _timeb timeb >> crypto\opensslconf.h
echo #endif >> crypto\opensslconf.h
echo. >> crypto\opensslconf.h
echo #ifndef _ftime >> crypto\opensslconf.h
echo #define _ftime ftime >> crypto\opensslconf.h
echo #endif >> crypto\opensslconf.h
