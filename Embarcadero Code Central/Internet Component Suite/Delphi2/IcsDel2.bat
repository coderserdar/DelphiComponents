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
REM * You must change PATH, DELPHI_PATH and ICS_PATH    *
REM * below to fit your system.                         *
REM *                                                   *
REM * Remember to install all components in Delphi 2 !  *
REM * Remember to use Delphi 2 to open all forms and    *
REM * ignore Font.CharSet and OldCreateOrder properties *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

SET DELPHI_PATH=C:\PROGRA~1\BORLAND\DELPHI~1.0
SET ICS_PATH=D:\FPIETTE

PATH=C:\WINDOWS;C:\WINDOWS\COMMAND;%DELPHI_PATH%\BIN

echo -M                             >%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -E%ICS_PATH%\DELPHI2          >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 

d:
cd %ICS_PATH%\delphi\internet
call ..\..\DELPHI2\dcc2 clidemo -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 client5 -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 client7 -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 concli1 -CC 
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 concli2 -CC 
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 consrv1 -CC -DNOFORMS
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 dlltst1 -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 dnslook -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 dynCli -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 finger -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 ftpthrd -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 ftpserv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 ftptst -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpasp -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpasy -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpchk -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpdmo -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpget -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httppg -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httpthrd -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 httptst -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 icsdll1
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mailrcv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mailrob -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mailsnd -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 md5test -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mimedemo -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mimetst -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 mtsrv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 newsrdr -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 nslookup -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 pingtst -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 pop3mime -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 recv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 sender -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 server5 -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 sockstst -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 srvdemo -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 srvtcp -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 tcpsrv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 tnclient -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 tndemo -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 tnsrv -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 twschat -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 udplstn -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 udpsend -CG
if errorlevel 1 goto error
call ..\..\DELPHI2\dcc2 webserv -CG
if errorlevel 1 goto error
goto done

:error
@echo Compile error
goto done

:done
@cd %ICS_PATH%\DELPHI2

:end
