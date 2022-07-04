@call env-init.cmd
@set path=.\..\..\..\..\make_tools;%path%
@call .\..\..\..\..\make_tools\make_prj.bat 6 %app%.dpr
@del %app%_d-6.exe 1>nul 2>nul
@ren %app%.exe %app%_d-6.exe
