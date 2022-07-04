@echo off
rem ########################################################
rem # you must first update the path to the brcc32 command #
rem ########################################################

if Exist Sfx.exe goto suite
echo you must first compile the Sfx project
goto fin

:suite
del SfxCode.exe
ren sfx.exe SfxCode.exe
c:\Progra~1\Borland\Delphi~1\bin\brcc32 SfxCode.rc
move /y SfxCode.res ..\Components\Archiver 
del SfxCode.exe

:fin
