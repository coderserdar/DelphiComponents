@set path=.\..\..\..\..\make_tools;%path%
@call .\..\..\..\..\make_tools\make_prj.bat 12 MSSQLMultipleCursors.dpr
@del MSSQLMultipleCursors_d2009.exe 1>nul 2>nul
@ren MSSQLMultipleCursors.exe MSSQLMultipleCursors_d2009.exe
