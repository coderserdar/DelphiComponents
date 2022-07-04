@rem echo off

@rem tested for Windows NT only

@set make_prj_ver=2008.08.07

@rem make_prj Version 2008.08.07
@rem ======================
@rem Описание
@rem ======================
@rem Компиляция :Delphi/Delphi.Net/C++Builder" pas модулей/проектов.
@rem Для работы нужен get_regval.exe. Брать get_regval.dpr из CVS ...
@rem
@rem Использование:
@rem  make_prj.bat  %%1 %%2
@rem
@rem %%1  - Версия Delphi/C++Builder-а или /C (обязательно).
@rem    Доступны:
@rem        3        - Delphi 3
@rem        4        - Delphi 4
@rem        5        - Delphi 5
@rem        6        - Delphi 6
@rem        7        - Delphi 7
@rem        8        - Delphi 8 for .Net
@rem        BDS      - Delphi 8 for .Net
@rem        9        - Delphi 2005 for Win32
@rem        9N       - Delphi 2005 for .Net
@rem        10       - Delphi 2006 for Win32
@rem        10N      - Delphi 2006 for .Net
@rem        11       - Delphi 2007 for Win32
@rem        12       - Delphi 2009 for Win32
@rem        B3       - C++ Builder 3.5
@rem        B4       - C++ Builder 4
@rem        B5       - C++ Builder 5
@rem        B6       - C++ Builder 6
@rem        D.Net1   - Delphi for .Net Preview
@rem
@rem        /C       - Clear - чистка текущей директории от: (dcu, obj, hpp, ...)
@rem
@rem        /CA      - /C + remove  "cfg, dof, dsk" Files
@rem
@rem %%2  - Имя проекта или ? для установки переменных окружения.
@rem
@rem [%3] - /c - искать общий "%%SystemRoot%%\dx_common.cmd" описывающий общие настройки.
@rem
@rem Примеры использования:
@rem   make_prj.bat 3 Project1.dpr
@rem   make_prj.bat 6 Unit1.pas
@rem   make_prj.bat B3 Unit1.pas
@rem   make_prj.bat B6 Unit1.pas
@rem   make_prj.bat D.Net1 EldoS.Delphi.ElPack.ElUnicodeStrings.pas
@rem   make_prj.bat D5 ?   -  только установка переменных окружения
@rem
@rem Copyright (c) 1998-2007
@rem by "Vadim V.Lopushansky" <pult@ukr.net>

@rem params parsing:

@if "%1"=="" goto L_HELP

@if "%1" == "/C"  goto L_CLEAN
@if "%1" == "/c"  goto L_CLEAN
@if "%1" == "/CA" goto L_CLEAN
@if "%1" == "/ca" goto

@goto L_CLEAR_SKIP

:L_CLEAN

@del dcc32.log 2>nul

@del *.tds 2>nul
@del *.bpi 2>nul
@del *.hpp 2>nul
@del *.drc 2>nul

@del *.map 2>nul
@del *.jdbg 2>nul
@del *.elf 2>nul
@del *.dcp 2>nul
@del *.dcu 2>nul
@del *.lib 2>nul
@del *.obj 2>nul

@del *.dcuil 2>nul
@del *.dcpil 2>nul
@del *.pdb 2>nul
@del *.rsp 2>nul
@del *.rsm 2>nul
@del *.~* 2>nul

@del *.bpl 2>nul
@del *.dpl 2>nul
@rem del *.dll 2>nul

@if "%1" NEQ "/CA" goto L_CLEAR_ALL_SKIP
@if "%1" NEQ "/ca" goto L_CLEAR_ALL_SKIP

@del *.cfg 2>nul
@del *.dof 2>nul
@del *.dsk 2>nul

:L_CLEAR_ALL_SKIP

@goto L_EXIT

:L_CLEAR_SKIP

@set DVER=%1
@rem if exist "%1"
@if "%2" NEQ "" set Project=%2
@if "%Project%"=="" goto L_ERROR_PARAM
@set DBASE=%DVER%

@rem Calculate Project specified options file name (Extract File Name):

@set file_name=
@for /F "delims=" %%i in ("%Project%") do @set file_name=%%~ni

@rem Clear user Envinronments
@set dccilOpt=
@set bccilOpt=

@set UserLib=.
@set UserLibI=.
@set UserLibO=.
@set UserLibR=.
@set UserPack=
@set UserCOpt=.
@set DEBUG_BATCH=0
@set CleanDcc32Log=0
@set UsePack=0
@set DEBUG=0
@set EurekaLog=0
@set MAPFILE=1
@set TRACE_STACK_SOURCE=0
@set JDBG=0

@rem set platform variables:

@set COMMAND=
@set DELPHI_ROOTDIR=
@set KEY=HKCU
@set Platform=
@set DelphiName=
@set REGPATH=
@set DXVER=?

@if "%DVER%"=="D3" set DVER=3
@if "%DVER%"=="D4" set DVER=4
@if "%DVER%"=="D5" set DVER=5
@if "%DVER%"=="D6" set DVER=6
@if "%DVER%"=="D7" set DVER=7
@if "%DVER%"=="D8" set DVER=8
@if "%DVER%"=="D9" set DVER=9
@if "%DVER%"=="D10" set DVER=10
@if "%DVER%"=="D11" set DVER=11
@if "%DVER%"=="D12" set DVER=12

@if "%DVER%"=="D.Net1" goto L_SET_DNET1
@if "%DVER%"=="D.NET1" goto L_SET_DNET1
@if "%DVER%"=="d.net1" goto L_SET_DNET1
@if "%DVER%"=="8" set DVER=BDS
@if "%DVER%"=="bds" set DVER=BDS
@if "%DVER%"=="BDS" goto L_SET_BDS
@if "%DVER%"=="9N" goto L_SET_BDS
@if "%DVER%"=="9n" goto L_SET_BDS
@if "%DVER%"=="10N" goto L_SET_BDS
@if "%DVER%"=="10n" goto L_SET_BDS

@if "%DVER%"=="B3" goto L_SET_BUILDER
@if "%DVER%"=="b3" goto L_SET_BUILDER
@if "%DVER%"=="B4" goto L_SET_BUILDER
@if "%DVER%"=="b4" goto L_SET_BUILDER
@if "%DVER%"=="B5" goto L_SET_BUILDER
@if "%DVER%"=="b5" goto L_SET_BUILDER
@if "%DVER%"=="B6" goto L_SET_BUILDER
@if "%DVER%"=="b6" goto L_SET_BUILDER
@if "%DVER%"=="9W" @set DVER="9"
@if "%DVER%"=="9w" @set DVER="9"
@if "%DVER%"=="10W" @set DVER="10"
@if "%DVER%"=="10w" @set DVER="10"
@if "%DVER%"=="11W" @set DVER="11"
@if "%DVER%"=="12W" @set DVER="12"
@if "%DVER%"=="11w" @set DVER="11"
@if "%DVER%"=="12w" @set DVER="12"

@if "%DVER%"=="3" set DXVER=03
@if "%DVER%"=="4" set DXVER=04
@if "%DVER%"=="5" set DXVER=05
@if "%DVER%"=="6" set DXVER=06
@if "%DVER%"=="7" set DXVER=07
@if "%DVER%"=="8" set DXVER=08
@if "%DVER%"=="9" set DXVER=09
@if "%DVER%"=="10" set DXVER=10
@if "%DVER%"=="11" set DXVER=11
@if "%DVER%"=="12" set DXVER=12

@set Platform=DX
@set REGPATH=\Software\Borland\Delphi
@set DelphiName=Delphi
@if "%DVER%"=="3" set KEY=HKLM
@if "%DVER%"=="4" set KEY=HKLM
@if "%DVER%"=="5" set KEY=HKLM
@if "%DVER%"=="9" set REGPATH=\Software\Borland\BDS
@if "%DVER%"=="9" set DelphiName=BDS
@if "%DVER%"=="9" set DVER=3
@if "%DVER%"=="10" set REGPATH=\Software\Borland\BDS
@if "%DVER%"=="10" set DelphiName=BDS
@if "%DVER%"=="10" set DVER=4
@if "%DVER%"=="11" set REGPATH=\Software\Borland\BDS
@if "%DVER%"=="11" set DelphiName=BDS
@if "%DVER%"=="11" set DVER=5
@if "%DVER%"=="12" set REGPATH=\Software\CodeGear\BDS
@if "%DVER%"=="12" set DelphiName=BDS
@if "%DVER%"=="12" set DVER=6
@goto L_SET_DONE

:L_SET_DNET1
@set Platform=D.Net1
@set DVER=1
@set KEY=HKLM
@set REGPATH=\Software\Borland\Delphi for .NET Preview
@set DelphiName=Delphi.Net
@goto L_SET_DONE

:L_SET_BDS
@set Platform=DELPHI_NET
@if "%DVER%"=="12N" set DVER=6
@if "%DVER%"=="12n" set DVER=6
@if "%DVER%"=="11N" set DVER=5
@if "%DVER%"=="11n" set DVER=5
@if "%DVER%"=="10N" set DVER=4
@if "%DVER%"=="10n" set DVER=4
@if "%DVER%"=="9N" set DVER=3
@if "%DVER%"=="9n" set DVER=3
@set KEY=HKCU
@set REGPATH=\Software\Borland\BDS
@set DelphiName=Delphi.Net
@goto L_SET_DONE

:L_SET_BUILDER
@set Platform=CB
@set REGPATH=\SOFTWARE\Borland\C++Builder
@if "%DVER%"=="B3" set DVER=1
@if "%DVER%"=="B4" set DVER=4
@if "%DVER%"=="B5" set DVER=5
@if "%DVER%"=="B6" set DVER=6
@set KEY=HKLM
@if "%DVER%"=="6" set KEY=HKCU
@set DelphiName=C++Builder

:L_SET_DONE
@rem read compiler root directory from registry:

@rem for Windows NT only
@for /F "eol=; tokens=1,2,3,4* delims=" %%i in ('get_regval.exe %KEY% %REGPATH%\%DVER%.0 RootDir /p /s') do @set DELPHI_ROOTDIR=%%i
@if "%DELPHI_ROOTDIR%"=="" goto L_ERROR_DX_ROOTDIR

@rem for Windows 9X and NT
@rem get_regval.exe %KEY% "%REGPATH%\%DVER%.0" RootDir /p /s /set >getregval.bat
@rem set REGVAL=
@rem call getregval.bat
@rem set DELPHI_ROOTDIR=%REGVAL%
@rem del getregval.bat 2>nul
@rem if "%DELPHI_ROOTDIR%"=="" goto L_ERROR_DX_ROOTDIR

@rem define system envinronment path:

@if "%Platform%"=="DELPHI_NET" goto L_PATH_DELPHI_NET
@if "%Platform%"=="D.Net1" goto L_PATH_DNET1
@set path=%DELPHI_ROOTDIR%\Bin\system32;%DELPHI_ROOTDIR%\Bin;%DELPHI_ROOTDIR%\Projects\Bpl;%path%
@goto L_PATH_DONE
:L_PATH_DNET1
:L_PATH_DELPHI_NET
@rem todo: .Net SDK Assemblies
@rem todo: BDS Shared Assemblies
@set path=%DELPHI_ROOTDIR%\Bin;%DELPHI_ROOTDIR%\Projects\Assemblies;%path%
:L_PATH_DONE

@rem Set User Envinronments

@rem if "%3"=="/c" if exist "%SystemRoot%\dx_common.cmd" if not exist "common.cmd" call call "%SystemRoot%\dx_common.cmd"
@if "%3"=="/c" if exist "%SystemRoot%\dx_common.cmd" call call "%SystemRoot%\dx_common.cmd"
@if exist "common.bat" call "common.bat"
@if exist "common.cmd" call "common.cmd"
@if exist "%file_name%.bat" call "%file_name%.bat"

@if "%UserLib%"=="" set UserLib=.
@if "%UserLibI%"=="" set UserLibI=.
@if "%UserLibO%"=="" set UserLibO=.
@if "%UserLibR%"=="" set UserLibR=.
@if "%UserCOpt%"=="." set UserCOpt=

@rem define Library path:

@if "%Platform%"=="D.Net1" goto L_SRC_DNET
@if "%Platform%"=="DELPHI_NET" goto L_SRC_DELPHI_NET
@set DLib=%UserLib%
@if "%DEBUG%" == "1" set DLib=%UserLib%;%DELPHI_ROOTDIR%\Lib\Debug
@if "%DEBUG%" NEQ "1" if "%TRACE_STACK_SOURCE%"=="1" set DLib=%UserLib%;%DELPHI_ROOTDIR%\Lib\Debug

@set DLib=%DLib%;%DELPHI_ROOTDIR%\Lib;%DELPHI_ROOTDIR%\Imports;%DELPHI_ROOTDIR%\Projects\Bpl;%DELPHI_ROOTDIR%\Source\Toolsapi;%DELPHI_ROOTDIR%\Bin\system32
@set DLib=%DLib%;%DELPHI_ROOTDIR%\Lib\Indy10
@goto L_SRC_DONE

:L_SRC_DELPHI_NET
@set %UserLibI%=%UserLibI%;%DELPHI_ROOTDIR%\Lib
@set %UserLibO%=%UserLibO%;%DELPHI_ROOTDIR%\Lib
@set %UserLibR%=%UserLibR%;%DELPHI_ROOTDIR%\Lib
@set DLib=%UserLib%
@if "%DEBUG%" == "1" set DLib=%UserLib%;%DELPHI_ROOTDIR%\Lib\Debug
@if "%DEBUG%" NEQ "1" if "%TRACE_STACK_SOURCE%"=="1" set DLib=%UserLib%;%DELPHI_ROOTDIR%\Lib\Debug
@set DLib=%DLib%;%DELPHI_ROOTDIR%\Lib
@goto L_SRC_DONE
:L_SRC_DNET
@rem delphi .Net Preview:
@set %UserLibI%=%UserLibI%;%DELPHI_ROOTDIR%\units
@set %UserLibO%=%UserLibO%;%DELPHI_ROOTDIR%\units
@set %UserLibR%=%UserLibR%;%DELPHI_ROOTDIR%\units
@set DLib=%UserLib%;%DELPHI_ROOTDIR%\units
:L_SRC_DONE

@rem Build:

@if "%Platform%"=="DELPHI_NET" goto L_BUILD_DNET
@if "%Platform%"=="D.Net1" goto L_BUILD_DNET
@if "%Platform%"=="CB" goto L_BUILD_BUILDER

@if "%Project%" NEQ "?" @echo %Project% - start make_prj, ver "%make_prj_ver%"
@set DCC_OPT=-$J+,R-,I-,Q-,Y-,B-,A+,W-,U-,T-,H+,X+,P+,V+,G+

@set dccName=dcc32.exe
@if "%EurekaLog%" == "1" set dccName=ecc32.exe
@if "%EurekaLog%" == "1" set TRACE_STACK_SOURCE=1
@if "%DEBUG%" == "1" set DCC_OPT=%DCC_OPT% -DDEBUG;_DEBUG_
@if "%EurekaLog%" == "1" set DEBUG=E
@if "%MAPFILE%" == "1" if "%EurekaLog%" == "1" set MAPFILE=0

@if "%DEBUG%" == "0" set DCC_OPT=%DCC_OPT% -$D-
@rem ,$C-,O+
@if "%DEBUG%" NEQ "0" if "%DEBUG%" NEQ "E" set DCC_OPT=%DCC_OPT% -$D+,L+,C-,O-
@if "%EurekaLog%" == "1" set DCC_OPT=%DCC_OPT% -$D+,L+,O- -DEurekaLog
@if "%MAPFILE%" == "1" set DCC_OPT=%DCC_OPT% -GD
@if "%UserCOpt%" NEQ "" set DCC_OPT=%DCC_OPT% %UserCOpt%

@rem set DCC_OPT=-Q -M -B %DCC_OPT%
@set DCC_OPT=-M -B %DCC_OPT%
@rem System Unit Recompile:
@rem set DCC_OPT=-v %DCC_OPT%

@if "%UserLib%" NEQ "" set UserLib=%UserLib%;

@if "%UserPack%" == "" goto L_USE_PACK_DX
@rem todo: ???: not supported -LU
@set DCC_OPT=%DCC_OPT% -LU%UserPack%
:L_USE_PACK_DX

@if "%Project%"=="?" goto L_EXIT
@set COMMAND=%dccName% %Project% %DCC_OPT% -U"%DLib%" -I"%DLib%" -R"%DLib%"
@call date/t>>dcc32.log
@call time/t>>dcc32.log
@echo %COMMAND%>>dcc32.log
@echo.
@echo %COMMAND%
@echo.
@"%DELPHI_ROOTDIR%\bin\%dccName%" %Project% %DCC_OPT% -U"%DLib%" -I"%DLib%" -R"%DLib%"
@echo %Project% - finish
@if "%ERRORLEVEL%" NEQ "0" goto L_ERROR
@if "%JDBG%"=="1" @call MakeJclDbg.exe -J %Project%

@goto L_BUILD_DONE

:L_BUILD_DNET

@set dccilOpt=-m -nsBorland.Delphi.System -nsBorland.Delphi -nsBorland.Vcl -luSystem.Drawing -luSystem.Data -luSystem.Windows.Forms %dccilOpt%

@if "%Project%"=="?" goto L_EXIT
@if "%UserLibO%"=="." @set UserLibO=
@if "%UserLibO%" NEQ "" @set UserLibO=-o%UserLibO%
@set COMMAND=dccil %dccilOpt% %UserCOpt% %Project% -U"%DLib%" -I"%UserLibI%" -R"%UserLibR%" %UserLibO%
@echo ------------------------------------------------------------------------------------------
@echo COMMAND=%COMMAND%
@echo ------------------------------------------------------------------------------------------
@dccil %dccilOpt% %UserCOpt% %Project% -U"%DLib%" -I"%UserLibI%" -R"%UserLibR%" -o%UserLibO%
@if "%ERRORLEVEL%" NEQ "0" goto L_ERROR

@goto L_BUILD_DONE

:L_BUILD_BUILDER
@if "%DVER%"=="1" set DVER=3
@set BDFIX=
@if "%DVER%"=="3" set BDFIX=;VER110
@if "%DVER%"=="4" set BDFIX=;VER125
@if "%DVER%"=="5" set BDFIX=;VER130;BCB
@if "%DVER%"=="6" set BDFIX=;VER140;BCB

@set DCC_OPT=-$J+,R-,I-,Q-,Y-,B-,A+,W-,U-,T-,H+,X+,P+,V+

@if "%DEBUG%" == "0" set DCC_OPT=%DCC_OPT%,D-,$C-,O+
@if "%DEBUG%" NEQ "0" set DCC_OPT=%DCC_OPT%,D+,L+,C-,O-
@if "%MAPFILE%" == "1" set DCC_OPT=%DCC_OPT% -GD
@if "%UserCOpt%" NEQ "" set DCC_OPT=%DCC_OPT% %UserCOpt%

@rem set DCC_OPT=-JPHN -M %DCC_OPT%
@set DCC_OPT=-JPHN %DCC_OPT%
@rem System Unit Recompile
@rem set DCC_OPT=-v %DCC_OPT%

@if "%UserLib%" NEQ "" set UserLib=%UserLib%;

@set UnitDir=%DELPHI_ROOTDIR%\lib\release;%DELPHI_ROOTDIR%\lib\obj;%DELPHI_ROOTDIR%\bin\lib;%DELPHI_ROOTDIR%\release
@set IncludeDir=%DELPHI_ROOTDIR%\include;%DELPHI_ROOTDIR%\include\vcl
@set ResourceDir=%IncludeDir%;%UnitDir%
@set UnitDir=%UserLib%%UnitDir%
@set IncludeDir=%UserLib%%UnitDir%

@if "%Project%"=="?" goto L_EXIT
@set COMMAND=dcc32.exe" -D_RTLDLL;USEPACKAGES%BDFIX% -U%UnitDir%  -I%IncludeDir% -R%ResourceDir% %Project% %DCC_OPT%
@echo ------------------------------------------------------------------------------------------
@echo "%DELPHI_ROOTDIR%\bin\dcc32.exe"  -D_RTLDLL;USEPACKAGES%BDFIX% -U%UnitDir%  -I%IncludeDir% -R%ResourceDir% %Project% %DCC_OPT% %bccOpt%
@echo ------------------------------------------------------------------------------------------
@"%DELPHI_ROOTDIR%\bin\dcc32.exe" -D_RTLDLL;USEPACKAGES%BDFIX% -U%UnitDir%  -I%IncludeDir% -R%ResourceDir% -M %Project% %DCC_OPT%
@if "%ERRORLEVEL%" NEQ "0" goto L_ERROR

:L_BUILD_DONE
@if "%CleanDcc32Log%"=="1" del dcc32.log 2>nul

@echo Done.

@goto L_EXIT

:L_ERROR_DX_ROOTDIR
@set KEYSTR=HKEY_CURRENT_USER
@if "%KEY%"=="HKLM" set KEYSTR=HKEY_LOCAL_MACHIME
@echo ERROR:
@echo  Не смог найти %DelphiName% %DVER%
@echo  Подробнее:
@echo    Cannot find Delphi %DVER%.0 RootDir in registry path:
@echo       '%KEYSTR%%REGPATH%\%DVER%.0\RootDir'
@echo press any key to exit
@pause >nul
@goto L_EXIT

:L_HELP
@rem cls
@goto L_HELP_INFO
:L_ERROR_PARAM
@echo ERROR: (неверные параметры)
:L_HELP_INFO
@echo  make_prj Version %make_prj_ver%
@echo   "make_prj.bat" - утилита сборки/компиляции проекта/модуля Delphi/Delphi.Net/C++Builder
@echo ------------------------------------------------------------------------------------------
@echo Использование:
@echo   make_prj.bat  "Delphi/C++Builder Major Version"  "Project_Name|?" ["options"]
@echo Где:
@echo   [1] "Delphi/C++Builder Major Version" - Версия Delphi/C++Builder-а (обязательно).
@echo   --
@echo    Доступны:
@echo        3        - Delphi 3
@echo        4        - Delphi 4
@echo        5        - Delphi 5
@echo        6        - Delphi 6
@echo        7        - Delphi 7
@echo        8        - Delphi 8 for .Net
@echo        BDS      - Delphi 8 for .Net
@echo        9        - Delphi 2005 for Win32
@echo        9N       - Delphi 2005 for .Net
@echo        B3       - C++ Builder 3.5
@echo        B4       - C++ Builder 4
@echo        B5       - C++ Builder 5
@echo        B6       - C++ Builder 6
@echo        D.Net1   - Delphi for .Net Preview
@echo        /C       - Clear - чистка текущей директории от: (dcu, obj, hpp, ...)
@echo        /CA      - /C + remove  "cfg, dof, dsk" Files


@echo   --
@echo   [2] "Project_Name|?"  -  Имя проекта/модуля или ? для установки переменных окружения.

@echo   --
@echo   [3] "Options":
@echo          /c  - искать общий "%%SystemRoot%%\dx_common.cmd" описывающий общие настройки.

@echo Примеры:
@echo   make_prj.bat 3 Project1.dpr
@echo   make_prj.bat 6 Unit1.pas
@echo   make_prj.bat D.Net1 EldoS.Delphi.ElPack.ElUnicodeStrings.pas
@echo   make_prj.bat B6 ElBiProgr.pas
@echo   make_prj.bat /C
@echo   make_prj.bat D5 ?  - только установка переменных окружения
@echo   make_prj.bat 6 project1.dpr /c - все настройки указанны в dx_common.cmd
@echo PS:
@echo   Для работы нужен "get_regval.exe". Брать "get_regval.dpr" из CVS ...
@echo ------------------------------------------------------------------------------------------
@goto L_EXIT

:L_ERROR

@if "%DEBUG_BATCH%"=="1" goto L_ERROR_LOG

@echo ------------------------------------------------------------------------------------------
@echo PATH=%path%
@echo ------------------------------------------------------------------------------------------
@echo Error (ERRORLEVEL="%ERRORLEVEL%").
@if "%UserLib%" NEQ "" if "%UserLib%" NEQ "." echo UserLib=%UserLib%
@if "%UserPack%" NEQ "" echo UserPack=%UserPack%
@echo ------------------------------------------------------------------------------------------
@echo command:
@echo %COMMAND%
@echo ------------------------------------------------------------------------------------------

:L_ERROR_LOG

@rem Clean Log File
@if "%CleanDcc32Log%"=="1" del dcc32.log 2>nul

@echo ------------------------------------------------------------------------------------------>>dcc32.log
@set>>dcc32.log
@echo . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . >>dcc32.log
@echo Error (ERRORLEVEL="%ERRORLEVEL%").>>dcc32.log
@echo . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . >>dcc32.log
@echo PATH=%path%>>dcc32.log
@echo . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . >>dcc32.log
@echo Error (ERRORLEVEL="%ERRORLEVEL%").>>dcc32.log
@echo UserLib=%UserLib%>>dcc32.log
@echo UserPack=%UserPack%>>dcc32.log
@echo . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . >>dcc32.log
@echo command:>>dcc32.log
@echo %COMMAND%>>dcc32.log
@echo . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . >>dcc32.log

@if exist "error_handler.bat" call error_handler.bat
@if "%SKIP_ERROR%"=="1" goto L_EXIT

:L_ERROR_SKIPCMD
@echo !!! ERROR !!! Press any key to exit . . .
@pause >nul
@exit

:L_EXIT
