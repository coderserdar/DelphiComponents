@ECHO OFF
REM Copyright 2003 by Matthew Greet.  See test\Licence.rtf
SET DELPHIBASE=C:\PROGRAM FILES\BORLAND\DELPHI5
SET LOCATION="%DELPHIBASE%\Projects\Bpl"
SET COMPILER="%DELPHIBASE%\BIN\DCC32.EXE"
SET HELPCOMPILER="%DELPHIBASE%\HELP\TOOLS\HCW.EXE"

REM Compiles package
%COMPILER% /LE%LOCATION% /LN%LOCATION% Collections_1_0_D5.dpk

REM Compiles test program
cd test
%COMPILER% /U%LOCATION% Test_D5.dpr
cd ..

REM Compiles help
cd help
%HELPCOMPILER% /C /E "Delphi Collections"
cd ..