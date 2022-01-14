@echo off

rem ==== OpenSSL ====

echo Cleaning up OpenSSL ...
rmdir /s /q openssl\tmp

rem ==== Expat ====

echo Cleaning up Expat ...
rmdir /s /q expat\bcb5\release

rem ==== neon ====

echo Cleaning up neon ...
rmdir /s /q neon\tmp

rem ==== PuTTYVS ====

echo Cleaning up PuTTYVS ...
rmdir /s /q puttyvs\Release

rem ==== libs3 ====

echo Cleaning up libs3 ...
rmdir /s /q libs3\tmp

rem ==== MFC ====

echo Cleaning up MFC ...
rmdir /s /q mfc\source\$UW
del mfc\source\bor.rsp

echo All done
