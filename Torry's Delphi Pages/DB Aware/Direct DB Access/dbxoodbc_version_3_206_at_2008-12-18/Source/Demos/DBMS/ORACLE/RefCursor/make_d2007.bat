@call env-init.cmd
@set path=.\..\..\..\..\make_tools;%path%
@call .\..\..\..\..\make_tools\make_prj.bat 11 %app%.dpr
@del %app%_d2007.exe 1>nul 2>nul
@ren %app%.exe %app%_d2007.exe
