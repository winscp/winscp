@echo off

set KOMPILATOR=brcc32

echo ----------

set TEXTSET=TextsCore
goto kompilace
:TextsCore_hotovo

set TEXTSET=TextsWin
goto kompilace
:TextsWin_hotovo

echo TEST:
echo.

echo #include "TextsCore.rc" > TextsAll.rc
echo #include "TextsWin.rc" >> TextsAll.rc
%KOMPILATOR% TextsAll.rc -foTextsAll.res

del TextsCore.rc
del TextsWin.rc
del TextsAll.rc
del TextsAll.res

echo ----------

exit

:kompilace

echo SADA: %TEXTSET%
echo.

echo #ifndef %TEXTSET%H >  %TEXTSET%.new
echo #define %TEXTSET%H >> %TEXTSET%.new
echo. >> %TEXTSET%.new

grep "#define" %TEXTSET%2.rc >> %TEXTSET%.new

echo. >> %TEXTSET%.new

grep "#define" %TEXTSET%1.rc >> %TEXTSET%.new

echo. >> %TEXTSET%.new
echo #endif // %TEXTSET% >> %TEXTSET%.new

fc %TEXTSET%.new %TEXTSET%.h
if errorlevel 1 goto zmena
goto bezezmeny

:zmena
copy %TEXTSET%.new %TEXTSET%.h > nul

:bezezmeny

del %TEXTSET%.new

echo #include "%TEXTSET%1.rc" > %TEXTSET%.rc
echo #include "%TEXTSET%2.rc" >> %TEXTSET%.rc

%KOMPILATOR% %TEXTSET%.rc -fo%TEXTSET%.res

echo ----------

goto %TEXTSET%_hotovo
