@set compiler=%1
@if "%compiler%" == "" set compiler=7

@rem compiler:
@rem 6 - Delphi 6
@rem 7 - Delphi 7
@rem 9 - Delphi 9 Win32
@rem 10 - Delphi 10 Win32

@call "..\make_tools\make_prj.bat" %compiler% MakeJclDbg.dpr
