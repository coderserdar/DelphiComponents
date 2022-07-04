@echo off
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *
REM *                                                   *
REM * ICS - Internet Component Suite                    *
REM *                                                   *
REM * C++ Builder 1 automated construction V1.00        *
REM * (c) 1997-2000 by Francois PIETTE                  *
REM * http://www.rtfm.be/fpiette/indexuk.htm            *
REM * francois.piette@swing.be  francois.piette@rtfm.be *
REM *                                                   *
REM * You must change PATH, BCB_PATH, ICS_DRIVE and     *
REM * ICS_PATH below to fit your system.                *
REM *                                                   *
REM * Remember to install all components in BCB1 !      *
REM * Remember to use BCB1 to open all forms and        *
REM * ignore Font.CharSet and OldCreateOrder properties *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

SET BCB_PATH=C:\PROGRA~1\BORLAND\BCB1
SET ICS_DRIVE=D:
SET ICS_PATH=%ICS_DRIVE%\FPIETTE

PATH=C:\WINDOWS;C:\WINDOWS\COMMAND;%BCB_PATH%\BIN

REM Build components (Delphi code will produce OBJ and HPP files)
%ICS_DRIVE%
cd %ICS_PATH%\delphi\vc32
make -B -f%ICS_PATH%\bcb1\delbcb1.mak
if errorlevel 1 goto error

REM Build all projects
cd %ICS_PATH%\cpp\internet\bcb1
make -B -f%ICS_PATH%\bcb1\IcsBcb1.mak
if errorlevel 1 goto error
goto done

:error
@echo Compile error
goto done

:done
@cd %ICS_PATH%\bcb1

:end
