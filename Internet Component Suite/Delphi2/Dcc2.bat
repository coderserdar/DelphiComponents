@echo off
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *
REM *                                                   *
REM * ICS - Internet Component Suite                    *
REM *                                                   *
REM * Delphi 2 automated construction V1.00             *
REM * (c) 1997-2000 by Francois PIETTE                  *
REM * http://www.rtfm.be/fpiette/indexuk.htm            *
REM * francois.piette@swing.be  francois.piette@rtfm.be *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

if exist %1.res goto compile
echo 1 ICON DISCARDABLE "ICS.ICO" >%1.rc
brcc32 %1.rc
del %1.rc

:compile
dcc32 -H -W -B %2 %3 %4 %5 %6 %7 %8 %1.dpr
