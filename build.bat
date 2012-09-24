@echo off
rem See 'readme' file
set BDS=c:\program files\Embarcadero\RAD Studio\9.0
set WITH_DRAGEXT64=0
rem set DRAGEXT64CL=<path to x64 cl.exe>
rem set DRAGEXT64INCL=<path to x64 includes>
rem set DRAGEXT64LIB=<path to x64 libraries>
if "%BUILD_TARGET%"=="" set BUILD_TARGET=Build
if "%BUILD_CONFIG%"=="" set BUILD_CONFIG=Release
C:\Windows\Microsoft.NET\Framework\v3.5\MSBuild.exe WinSCP.groupproj /t:clean,%BUILD_TARGET% /p:RELEASE_TYPE=%RELEASE_TYPE%;CONFIG=%BUILD_CONFIG%;INTERM_PATH=.;FINAL_PATH=.;LIB_PATH=lib
