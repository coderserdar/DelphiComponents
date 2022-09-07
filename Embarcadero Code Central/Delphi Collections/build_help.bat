@ECHO OFF
REM Copyright 2003 by Matthew Greet.  See help\Licence.rtf
SET DELPHIBASE="C:\PROGRAM FILES\BORLAND\DELPHI7"
SET HELPCOMPILER=%DELPHIBASE%\"HELP\TOOLS\HCW.EXE"

REM Compiles help
cd help
%HELPCOMPILER% /C /E "Delphi Collections"
cd ..