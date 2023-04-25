@echo off
set COFF_LIB=Release\PuTTYVS.lib
set OMF_LIB=%LIB_PATH%\PuTTYVS.lib
if exist %COFF_LIB% del %COFF_LIB%
if exist %OMF_LIB% del %OMF_LIB%

"%MSBUILD%" PuTTYVS.vcxproj /p:Configuration=Release
if exist %COFF_LIB% (
  %BUILDTOOLS%\tools\objconv.exe -v2 -fomf32 %COFF_LIB% %OMF_LIB%
)
