@echo off

set compiler=%1
if "%compiler%" == "" set compiler=10

rem compiler:
rem  6 - Delphi 6
rem  7 - Delphi 7
rem  9 - Delphi 2005 Win32
rem 10 - Delphi 2006 Win32
rem 11 - Delphi 2007 Win32
rem 12 - Delphi 2009 Win32

call _make_driver_debug.bat %compiler%
  if "%ERROR_STATE%"=="1" exit
call _make_driver_release.bat %compiler%
