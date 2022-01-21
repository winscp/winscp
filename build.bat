@echo off
rem See 'readme' file
if "%PROCESSOR_ARCHITECTURE%"=="x86" set PROGRAMFILES32=%ProgramFiles%
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" set PROGRAMFILES32=%ProgramFiles(x86)%
set BDS=%PROGRAMFILES32%\Embarcadero\Studio\14.0
set MSBUILD=%PROGRAMFILES32%\Microsoft Visual Studio\2019\BuildTools\MSBuild\Current\Bin\MSBuild.exe

set WITH_DOTNET=1
if "%BUILD_TARGET%"=="" set BUILD_TARGET=Build
if "%BUILD_CONFIG%"=="" set BUILD_CONFIG=Release

set BUILDTOOLS=%~dp0\buildtools

cd libs
call buildlibs.bat

cd ..\source
set BDS_BUILD_PROPERTIES=RELEASE_TYPE=%RELEASE_TYPE%;CONFIG=%BUILD_CONFIG%;INTERM_PATH=.;FINAL_PATH=.
"%MSBUILD%" WinSCP.groupproj /t:%BUILD_TARGET% /p:%BDS_BUILD_PROPERTIES%
"%MSBUILD%" DragExt.cbproj /t:%BUILD_TARGET% /p:%BDS_BUILD_PROPERTIES%;Platform=Win64

if "%WITH_DOTNET%"=="0" goto SKIP_DOTNET
cd ..\dotnet
set DOTNET_BUILD_PROPERTIES=INTERM_PATH=.;FINAL_PATH=.
dotnet restore WinSCPnet.csproj -p:%DOTNET_BUILD_PROPERTIES%
mkdir obj
move win32\Debug\WinSCPnet.csproj.nuget.g.targets obj\
rmdir /s /q win32
dotnet build WinSCPnet.csproj -c %BUILD_CONFIG% -p:%DOTNET_BUILD_PROPERTIES%
:SKIP_DOTNET
