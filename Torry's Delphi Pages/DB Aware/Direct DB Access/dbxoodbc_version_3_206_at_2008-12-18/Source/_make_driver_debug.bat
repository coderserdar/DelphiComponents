@echo off
set path=.\make_tools;%path%

set compiler=%1
if "%compiler%" == "" set compiler=10

rem compiler:
rem  6 - Delphi 6
rem  7 - Delphi 7
rem  9 - Delphi 2005 Win32
rem 10 - Delphi 2006 Win32
rem 11 - Delphi 2007 Win32
rem 12 - Delphi 2009 Win32

rem Clean
  call .\make_tools\make_prj.bat /c
  del common.bat >nul 2>nul
  set ERROR_STATE=0
  del /Q dbxoodbc.dll >nul 2>nul
  md _Debug 2>nul >nul
  del /Q .\_Debug\*.* >nul 2>nul
  set JDBG_LINK=0

rem set debug options
  copy debug.opt common.bat >nul

rem build
  call .\make_tools\make_prj.bat %compiler% dbxoodbc.pas
  call .\make_tools\make_prj.bat %compiler% dbxoodbc.dpr

rem Clean
  del /Q common.bat >nul

rem check result
  if "%ERROR_STATE%"=="1" goto L_ERROR

@if "%JDBG_LINK%"=="1" @if exist "dbxoodbc.map" @call .\make_tools\MakeJclDbg.exe -E dbxoodbc.map

rem copy result to directory "_Debug"

  md .\_Debug >nul 2>nul
  copy dbxoodbc.dll .\_Debug\ >nul
  copy *.dcu .\_Debug\ >nul
  if exist dbxoodbc.map copy dbxoodbc.map .\_Debug\ >nul
  if exist dbxoodbc.jdbg copy dbxoodbc.jdbg .\_Debug\ >nul
  rem copy *.res .\_Debug\ >nul
  rem copy *.inc .\_Debug\ >nul
  rem clean
  call .\make_tools\make_prj.bat /c
  del /Q dbxoodbc.dll >nul

rem

:L_ERROR
  rem clean dcu
  rem make_prj.bat /c
  goto L_EXIT

:L_EXIT
