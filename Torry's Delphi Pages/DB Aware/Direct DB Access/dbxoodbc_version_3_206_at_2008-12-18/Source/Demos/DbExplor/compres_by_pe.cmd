@rem "PE COMPACT" executable packer. Home: http://www.bitsum.com

@set peCompactDir=C:\PROGRAMS\TOOLS\pe.Compact

@set rate=%2
@if "%rate%"=="" set rate=7

@del lzmacodec_temp.dat 2>nul
@del lzmacodec_temp.lzma 2>nul
@if exist lzmacodec_temp.dat goto L_LOCK1
@if exist lzmacodec_temp.lzma goto L_LOCK2

@set sCap=
@if "%OS%"=="Windows_NT" set sCap="fix"

@set path=%peCompactDir%;%path%

@set p=/NoBackup /CompressionLevel:%rate%

@set p=%p% /StripUnusedRsrc:No
@set p=%p% /Quiet
@set p=%p% /CodecHost=pec2codec_lzma.dll
@set p=%p% /Emp=Yes
@set p=%p% /KeepOverlay

@rem @set p=%p% /CompressExports=Auto
@rem @set p=%p% /EnableMemoryProtection=Yes
@rem @set p=%p% /SkipSharedSections=Yes
@rem @set p=%p% /StripDebug=Yes
@rem @set p=%p% /CodeIntegrityCheck=Yes

@set p=%p% /Priority:high

@call PEC2.exe %1.exe %p%

@del lzmacodec_temp.dat 2>nul
@del lzmacodec_temp.lzma 2>nul

goto L_END

:L_LOCK1
@set em=lzmacodec_temp.dat
goto L_LOCK
:L_LOCK2
@set em=lzmacodec_temp.lzma
:L_LOCK
@echo ERROR: File "%em%" is locked. Check process "lzma.exe" into memory. Kill it process is it needed ...

:L_END
