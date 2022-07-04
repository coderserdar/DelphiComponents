@set compiler=%1

@if "%compiler%"=="/r" set compiler=
@if "%compiler%"=="/d" set compiler=
@if "%compiler%"=="/R" set compiler=
@if "%compiler%"=="/D" set compiler=

@if "%compiler%" == "" set compiler=10

@rem compiler:
@rem 6  - Delphi  6
@rem 7  - Delphi  7
@rem 9  - Delphi  9 Win32
@rem 9  - Delphi 2005 Win32
@rem 10 - Delphi 2006 Win32
@rem 11 - Delphi 2007 Win32

@set f=%2
@if "%f%"=="" set f=%1
@if "%f%"=="/d" copy debug.opt common.bat >nul
@if "%f%"=="/r" copy release.opt common.bat >nul
@if "%f%"=="/D" copy debug.opt common.bat >nul
@if "%f%"=="/R" copy release.opt common.bat >nul

@rem set release options
  @if not exist common.bat copy release.opt common.bat >nul

@call .\make_tools\make_prj.bat %compiler% dbxoodbc.pas
@call .\make_tools\make_prj.bat %compiler% dbxoodbc.dpr
