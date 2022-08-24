@echo off
F:\DeveloperTools\lazarus\fpc\2.2.4\bin\i386-win32\windres.exe --include F:/DEVELO~2/lazarus/fpc/222A9D~1.4/bin/I386-W~1/ -O res -o "E:\PrecisionWare\Precision Language Suite for FPC\examples\embeddededitdemo\__ppu\EmbeddedEditDemo.res" E:/PRECIS~1/PR2F9B~1/examples/EMBEDD~1/EMBEDD~1.RC --preprocessor=F:\DeveloperTools\lazarus\fpc\2.2.4\bin\i386-win32\cpp.exe
if errorlevel 1 goto linkend
SET THEFILE=EmbeddedEditDemo.exe
echo Linking %THEFILE%
F:\DeveloperTools\lazarus\fpc\2.2.4\bin\i386-win32\ld.exe -b pe-i386 -m i386pe  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o EmbeddedEditDemo.exe link.res
if errorlevel 1 goto linkend
F:\DeveloperTools\lazarus\fpc\2.2.4\bin\i386-win32\postw32.exe --subsystem gui --input EmbeddedEditDemo.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
