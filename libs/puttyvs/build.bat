@echo off
set BUILDTOOLS_PATH=%1
if "%BUILDTOOLS_PATH%" == "" set BUILDTOOLS_PATH=..\..\buildtools
if "%PROCESSOR_ARCHITECTURE%"=="x86" set PROGRAMFILES32=%ProgramFiles%
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" set PROGRAMFILES32=%ProgramFiles(x86)%

set COFF_LIB=Release\PuTTYVS.lib
set OMF_LIB=..\lib\PuTTYVS.lib
if exist %COFF_LIB% del %COFF_LIB%
if exist %OMF_LIB% del %OMF_LIB%

"%PROGRAMFILES32%\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin\MSBuild.exe" PuTTYVS.vcxproj /p:Configuration=Release
if exist %COFF_LIB% (
  %BUILDTOOLS_PATH%\tools\objconv.exe -v2 -fomf32 %COFF_LIB% %OMF_LIB%
)
