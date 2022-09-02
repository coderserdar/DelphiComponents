echo off

set timestr=5:55:00
set datestr=08/19/98

del *.lzh

cd CCSKEYS
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\CCSKEYS *.*
cd..

cd CHKFB
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\CHKFB *.*
cd..

cd EXTEND
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\EXTEND *.*
cd..

cd FBROWSE
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\FBROWSE *.*
cd..

cd JULSTULS
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\JULSTULS *.*
cd..

cd NUMKEYSZ
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\NUMKEYSZ *.*
cd..

cd OOFILER
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\OOFILER *.*
cd..

cd TVBRWDLG
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\TVBRWDLG *.*
cd..

cd TWOBRDOS
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\TWOBRDOS *.*
cd..

cd TWOBRWIN
j:\util\fileset *.* -d %datestr% -t %timestr%
lha a ..\TWOBRWIN *.*
cd..

j:\util\fileset *.lzh -d %datestr% -t %timestr%
j:\util\fileset read.me! -d %datestr% -t %timestr%

:exit
set timestr=
set datestr=

