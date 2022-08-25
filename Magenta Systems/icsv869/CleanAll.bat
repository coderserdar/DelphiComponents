@echo off
if exist source\OverbyteIcsWSocket.pas goto clean
echo.
echo.
echo Not an ICS repository
goto end
:clean
echo Hit return to clean temporary and all D2006+ project files, of CTRL+C to abort...
pause
if exist *.bak del *.bak

if exist Install\*.local del Install\*.local
if exist Install\*.~* del Install\*.~*

REM Clear lib directories 
if exist Lib\release\Win32\D7\*.hpp del Lib\release\Win32\D7\*.hpp
if exist Lib\release\Win32\D7\*.dcu del Lib\release\Win32\D7\*.dcu
if exist Lib\release\Win32\D7\*.obj del Lib\release\Win32\D7\*.obj
if exist Lib\debug\Win32\D7\*.hpp del Lib\debug\Win32\D7\*.hpp
if exist Lib\debug\Win32\D7\*.dcu del Lib\debug\Win32\D7\*.dcu
if exist Lib\debug\Win32\D7\*.obj del Lib\debug\Win32\D7\*.obj

if exist Lib\release\Win32\D2006\*.hpp del Lib\release\Win32\D2006\*.hpp
if exist Lib\release\Win32\D2006\*.dcu del Lib\release\Win32\D2006\*.dcu
if exist Lib\release\Win32\D2006\*.obj del Lib\release\Win32\D2006\*.obj
if exist Lib\debug\Win32\D2006\*.hpp del Lib\debug\Win32\D2006\*.hpp
if exist Lib\debug\Win32\D2006\*.dcu del Lib\debug\Win32\D2006\*.dcu
if exist Lib\debug\Win32\D2006\*.obj del Lib\debug\Win32\D2006\*.obj
if exist Lib\debug\Win32\D2006\*.#00 del Lib\debug\Win32\D2006\*.#00
if exist Lib\debug\Win32\D2006\*.pch del Lib\debug\Win32\D2006\*.pch
if exist Lib\release\Win32\D2006\*.#00 del Lib\release\Win32\D2006\*.#00
if exist Lib\release\Win32\D2006\*.pch del Lib\release\Win32\D2006\*.pch

if exist Lib\release\Win32\D2007\*.hpp del Lib\release\Win32\D2007\*.hpp
if exist Lib\release\Win32\D2007\*.dcu del Lib\release\Win32\D2007\*.dcu
if exist Lib\release\Win32\D2007\*.obj del Lib\release\Win32\D2007\*.obj
if exist Lib\release\Win32\D2007\*.dfm del Lib\release\Win32\D2007\*.dfm
if exist Lib\debug\Win32\D2007\*.hpp del Lib\debug\Win32\D2007\*.hpp
if exist Lib\debug\Win32\D2007\*.dcu del Lib\debug\Win32\D2007\*.dcu
if exist Lib\debug\Win32\D2007\*.obj del Lib\debug\Win32\D2007\*.obj
if exist Lib\debug\Win32\D2007\*.dfm del Lib\debug\Win32\D2007\*.dfm
if exist Lib\debug\Win32\D2007\*.#00 del Lib\debug\Win32\D2007\*.#00
if exist Lib\debug\Win32\D2007\*.pch del Lib\debug\Win32\D2007\*.pch
if exist Lib\release\Win32\D2007\*.#00 del Lib\release\Win32\D2007\*.#00
if exist Lib\release\Win32\D2007\*.pch del Lib\release\Win32\D2007\*.pch

if exist Lib\release\Win32\D2009\*.hpp del Lib\release\Win32\D2009\*.hpp
if exist Lib\release\Win32\D2009\*.dcu del Lib\release\Win32\D2009\*.dcu
if exist Lib\release\Win32\D2009\*.obj del Lib\release\Win32\D2009\*.obj
if exist Lib\release\Win32\D2009\*.dfm del Lib\release\Win32\D2009\*.dfm
if exist Lib\debug\Win32\D2009\*.hpp del Lib\debug\Win32\D2009\*.hpp
if exist Lib\debug\Win32\D2009\*.dcu del Lib\debug\Win32\D2009\*.dcu
if exist Lib\debug\Win32\D2009\*.obj del Lib\debug\Win32\D2009\*.obj
if exist Lib\debug\Win32\D2009\*.dfm del Lib\debug\Win32\D2009\*.dfm
if exist Lib\debug\Win32\D2009\*.#00 del Lib\debug\Win32\D2009\*.#00
if exist Lib\debug\Win32\D2009\*.pch del Lib\debug\Win32\D2009\*.pch
if exist Lib\release\Win32\D2009\*.#00 del Lib\release\Win32\D2009\*.#00
if exist Lib\release\Win32\D2009\*.pch del Lib\release\Win32\D2009\*.pch

if exist Lib\release\Win32\D2010\*.hpp del Lib\release\Win32\D2010\*.hpp
if exist Lib\release\Win32\D2010\*.dcu del Lib\release\Win32\D2010\*.dcu
if exist Lib\release\Win32\D2010\*.obj del Lib\release\Win32\D2010\*.obj
if exist Lib\release\Win32\D2010\*.dfm del Lib\release\Win32\D2010\*.dfm
if exist Lib\debug\Win32\D2010\*.hpp del Lib\debug\Win32\D2010\*.hpp
if exist Lib\debug\Win32\D2010\*.dcu del Lib\debug\Win32\D2010\*.dcu
if exist Lib\debug\Win32\D2010\*.obj del Lib\debug\Win32\D2010\*.obj
if exist Lib\debug\Win32\D2010\*.dfm del Lib\debug\Win32\D2010\*.dfm
if exist Lib\debug\Win32\D2010\*.#00 del Lib\debug\Win32\D2010\*.#00
if exist Lib\debug\Win32\D2010\*.pch del Lib\debug\Win32\D2010\*.pch
if exist Lib\release\Win32\D2010\*.#00 del Lib\release\Win32\D2010\*.#00
if exist Lib\release\Win32\D2010\*.pch del Lib\release\Win32\D2010\*.pch

if exist Lib\release\Win32\XE\*.hpp del Lib\release\Win32\XE\*.hpp
if exist Lib\release\Win32\XE\*.dcu del Lib\release\Win32\XE\*.dcu
if exist Lib\release\Win32\XE\*.obj del Lib\release\Win32\XE\*.obj
if exist Lib\release\Win32\XE\*.dfm del Lib\release\Win32\XE\*.dfm
if exist Lib\debug\Win32\XE\*.hpp del Lib\debug\Win32\XE\*.hpp
if exist Lib\debug\Win32\XE\*.dcu del Lib\debug\Win32\XE\*.dcu
if exist Lib\debug\Win32\XE\*.obj del Lib\debug\Win32\XE\*.obj
if exist Lib\debug\Win32\XE\*.dfm del Lib\debug\Win32\XE\*.dfm
if exist Lib\debug\Win32\XE\*.#00 del Lib\debug\Win32\XE\*.#00
if exist Lib\debug\Win32\XE\*.pch del Lib\debug\Win32\XE\*.pch
if exist Lib\release\Win32\XE\*.#00 del Lib\release\Win32\XE\*.#00
if exist Lib\release\Win32\XE\*.pch del Lib\release\Win32\XE\*.pch

if exist Lib\release\Win32\XE2\*.hpp del Lib\release\Win32\XE2\*.hpp
if exist Lib\release\Win32\XE2\*.dcu del Lib\release\Win32\XE2\*.dcu
if exist Lib\release\Win32\XE2\*.obj del Lib\release\Win32\XE2\*.obj
if exist Lib\release\Win32\XE2\*.dfm del Lib\release\Win32\XE2\*.dfm
if exist Lib\debug\Win32\XE2\*.hpp del Lib\debug\Win32\XE2\*.hpp
if exist Lib\debug\Win32\XE2\*.dcu del Lib\debug\Win32\XE2\*.dcu
if exist Lib\debug\Win32\XE2\*.obj del Lib\debug\Win32\XE2\*.obj
if exist Lib\debug\Win32\XE2\*.dfm del Lib\debug\Win32\XE2\*.dfm
if exist Lib\debug\Win32\XE2\*.#00 del Lib\debug\Win32\XE2\*.#00
if exist Lib\debug\Win32\XE2\*.pch del Lib\debug\Win32\XE2\*.pch
if exist Lib\release\Win32\XE2\*.#00 del Lib\release\Win32\XE2\*.#00
if exist Lib\release\Win32\XE2\*.pch del Lib\release\Win32\XE2\*.pch

if exist Lib\release\Win64\XE2\*.hpp del Lib\release\Win64\XE2\*.hpp
if exist Lib\release\Win64\XE2\*.dcu del Lib\release\Win64\XE2\*.dcu
if exist Lib\release\Win64\XE2\*.obj del Lib\release\Win64\XE2\*.obj
if exist Lib\release\Win64\XE2\*.dfm del Lib\release\Win64\XE2\*.dfm
if exist Lib\debug\Win64\XE2\*.hpp del Lib\debug\Win64\XE2\*.hpp
if exist Lib\debug\Win64\XE2\*.dcu del Lib\debug\Win64\XE2\*.dcu
if exist Lib\debug\Win64\XE2\*.obj del Lib\debug\Win64\XE2\*.obj
if exist Lib\debug\Win64\XE2\*.dfm del Lib\debug\Win64\XE2\*.dfm
if exist Lib\debug\Win64\XE2\*.#00 del Lib\debug\Win64\XE2\*.#00
if exist Lib\debug\Win64\XE2\*.pch del Lib\debug\Win64\XE2\*.pch
if exist Lib\release\Win64\XE2\*.#00 del Lib\release\Win64\XE2\*.#00
if exist Lib\release\Win64\XE2\*.pch del Lib\release\Win64\XE2\*.pch

if exist Lib\release\osx32\XE2\*.hpp del Lib\release\osx32\XE2\*.hpp
if exist Lib\release\osx32\XE2\*.dcu del Lib\release\osx32\XE2\*.dcu
if exist Lib\release\osx32\XE2\*.obj del Lib\release\osx32\XE2\*.obj
if exist Lib\release\osx32\XE2\*.dfm del Lib\release\osx32\XE2\*.fmx
if exist Lib\debug\osx32\XE2\*.hpp del Lib\debug\osx32\XE2\*.hpp
if exist Lib\debug\osx32\XE2\*.dcu del Lib\debug\osx32\XE2\*.dcu
if exist Lib\debug\osx32\XE2\*.obj del Lib\debug\osx32\XE2\*.obj
if exist Lib\debug\osx32\XE2\*.dfm del Lib\debug\osx32\XE2\*.fmx
if exist Lib\debug\osx32\XE2\*.#00 del Lib\debug\osx32\XE2\*.#00
if exist Lib\debug\osx32\XE2\*.pch del Lib\debug\osx32\XE2\*.pch
if exist Lib\release\osx32\XE2\*.#00 del Lib\release\osx32\XE2\*.#00
if exist Lib\release\osx32\XE2\*.pch del Lib\release\osx32\XE2\*.pch

REM Clear Package directory
if exist packages\OverbyteIcs*.~*  del packages\OverbyteIcs*.~*
if exist packages\OverbyteIcs*.bak del packages\OverbyteIcs*.bak
if exist packages\*.dproj.local del packages\*.dproj.local
REM if exist packages\*.dproj.2007 del packages\*.dproj.2007
if exist packages\*.cbproj.local del packages\*.cbproj.local
if exist packages\*.bdsproj.local del packages\*.bdsproj.local
if exist packages\*.identcache del packages\*.identcache
REM if exist packages\__history\*.* del packages\__history\*.*

call CleanSamples.bat

:end
pause
