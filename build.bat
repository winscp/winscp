@echo off
rem See 'readme' file
if "%PROCESSOR_ARCHITECTURE%"=="x86" set PROGRAMFILES32=%ProgramFiles%
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" set PROGRAMFILES32=%ProgramFiles(x86)%
set BDS=%PROGRAMFILES32%\Embarcadero\Studio\14.0
set MSBUILD=%PROGRAMFILES32%\MSBuild\14.0\Bin\MSBuild.exe

set WITH_DOTNET=1
if "%BUILD_TARGET%"=="" set BUILD_TARGET=Build
if "%BUILD_CONFIG%"=="" set BUILD_CONFIG=Release

cd libs
call buildlibs.bat

cd ..\source
set BDS_BUILD_PROPERTIES=RELEASE_TYPE=%RELEASE_TYPE%;CONFIG=%BUILD_CONFIG%;INTERM_PATH=.;FINAL_PATH=.
"%MSBUILD%" WinSCP.groupproj /t:%BUILD_TARGET% /p:%BDS_BUILD_PROPERTIES%
"%MSBUILD%" DragExt.cbproj /t:%BUILD_TARGET% /p:%BDS_BUILD_PROPERTIES%;Platform=Win64

if "%WITH_DOTNET%"=="0" goto SKIP_DOTNET
cd ..\dotnet
"%MSBUILD%" WinSCPnet.csproj /t:Build /p:Configuration=%BUILD_CONFIG%;Platform=AnyCPU;INTERM_PATH=.;FINAL_PATH=.
:SKIP_DOTNET
