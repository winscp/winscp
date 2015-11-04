@echo off
set BUILDTOOLS_PATH=%1
if "%BUILDTOOLS_PATH%" == "" set BUILDTOOLS_PATH=..\..\buildtools
%WINDIR%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe PuTTYVS.vcxproj /p:Configuration=Release
%BUILDTOOLS_PATH%\tools\objconv.exe -fomf32 Release\PuTTYVS.lib ..\lib\PuTTYVS.lib
