@echo off
set BUILDTOOLS_PATH=%1
if "%BUILDTOOLS_PATH%" == "" set BUILDTOOLS_PATH=..\..\buildtools
if "%PROCESSOR_ARCHITECTURE%"=="x86" set PROGRAMFILES32=%ProgramFiles%
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" set PROGRAMFILES32=%ProgramFiles(x86)%

"%PROGRAMFILES32%\MSBuild\14.0\Bin\MSBuild.exe" PuTTYVS.vcxproj /p:Configuration=Release
%BUILDTOOLS_PATH%\tools\objconv.exe -fomf32 Release\PuTTYVS.lib ..\lib\PuTTYVS.lib
