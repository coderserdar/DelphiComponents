@call env-init.cmd
@set path=.\..\..\..\..\make_tools;%path%
@call .\..\..\..\..\make_tools\make_prj.bat 12 %app%.dpr
@del %app%_d2009.exe 1>nul 2>nul
@ren %app%.exe %app%_d2009.exe
