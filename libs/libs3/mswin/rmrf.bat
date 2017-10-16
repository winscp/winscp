@echo off

if exist "%1". (
   rmdir /S /Q "%1"
)

if exist "%1". (
   del /Q "%1"
)
