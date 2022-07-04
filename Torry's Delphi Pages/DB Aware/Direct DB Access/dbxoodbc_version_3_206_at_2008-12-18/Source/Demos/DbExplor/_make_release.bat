@set path=.\..\..\make_tools;%path%

@set Destination=release

@rem set debug options
  @copy %Destination%.opt common.bat >nul

@rem set compiler:
  @rem 6  - Delphi 6
  @rem 7  - Delphi 7
  @rem 9  - Delphi 2005 Win32
  @rem 10 - Delphi 2006 Win32
  @rem 11 - Delphi 2007 Win32

@set compiler=%1
  @if "%compiler%" == "" set compiler=7

@set app=OdbcExplor
@rem build

  @echo #
  @echo # Build OdbcExplor.dpr: ...
  @echo #
  @call ..\..\make_tools\make_prj.bat %compiler% %app%.dpr
  @echo #
  @echo # Done.
  @echo #

  @set link_map1=0
  @if "%link_map1%" NEQ "1" goto L_SKIP_LINK_MAP1
  @echo #
  @echo # Link map file to EXE: ...
  @echo #
  @call MakeJclDbg.exe -E OdbcExplor.map
  @del OdbcExplor.map >nul
  @echo #
  @echo # Done.
  @echo #
:L_SKIP_LINK_MAP1

  @set make_jdbg=0
  @if "%make_jdbg%" NEQ "1" goto L_SKIP_JDBG
  @echo #
  @echo # Convert map file to compresset JDBG format: ...
  @echo #
  @call MakeJclDbg.exe -J OdbcExplor.map
  @echo #
  @echo # Done.
  @echo #
:L_SKIP_JDBG

  @set compress=1
  @if "%compress%" NEQ "1" goto L_SKIP_COMPRESS
  @echo #
  @echo # Compress EXE: ...
  @echo #
  @call compres_by_upx.cmd OdbcExplor
  @rem call compres_by_pe.cmd OdbcExplor 7
  @echo #
  @echo # Done.
  @echo #
:L_SKIP_COMPRESS

  @set link_map2=1
  @if "%link_map2%" NEQ "1" goto L_SKIP_LINK_MAP2
  @echo #
  @echo # Link map file to EXE: ...
  @echo #
  @call MakeJclDbg.exe -E OdbcExplor.map
  @del OdbcExplor.map >nul
  @echo #
  @echo # Done.
  @echo #
:L_SKIP_LINK_MAP2

  @del%app%_%Destination%.exe >nul 2>nul
  @del%app%_%Destination%.map >nul 2>nul
  @del%app%_%Destination%.jdbg >nul 2>nul

  @copy %app%.exe %app%_%Destination%.exe >nul
  @copy %app%.map %app%_%Destination%.map >nul 2>nul
  @copy %app%.jdbg %app%_%Destination%.jdbg >nul 2>nul

  @del /Q %app%.exe >nul
  @del /Q %app%.map >nul 2>nul
  @del /Q %app%.jdbg >nul 2>nul
