@echo off
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *
REM *                                                   *
REM * ICS - Internet Component Suite                    *
REM *                                                   *
REM * Delphi 1 automated construction V1.00             *
REM * (c) 1997-2000 by Francois PIETTE                  *
REM * http://www.rtfm.be/fpiette/indexuk.htm            *
REM * francois.piette@swing.be  francois.piette@rtfm.be *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

if not exist %1.res echo >%1.res
dcc -T%ICS_PATH%\DELPHI1 -T%DELPHI_PATH%\BIN -B %1.dpr
