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
REM * Remember to install all components in Delphi 3 !  *
REM * Remember to use Delphi 3 to open all forms and    *
REM * ignore Font.CharSet and OldCreateOrder properties *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

SET DELPHI_PATH=C:\PROGRA~1\BORLAND\DELPHI~1
SET ICS_PATH=D:\FPIETTE

PATH=C:\WINDOWS;C:\WINDOWS\COMMAND;%DELPHI_PATH%\BIN

echo -M                             >%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%DELPHI_PATH%\LIB           >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -E%ICS_PATH%\DELPHI3          >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -I%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -R%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%ICS_PATH%\DELPHI\VC32      >>%ICS_PATH%\delphi\internet\dcc32.cfg 
echo -U%ICS_PATH%\DELPHI\INTERNET  >>%ICS_PATH%\delphi\internet\dcc32.cfg 

d:
cd %ICS_PATH%\delphi\internet
call ..\..\delphi3\dcc3 clidemo -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 client5 -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 client7 -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 concli1 -CC 
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 concli2 -CC 
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 consrv1 -CC -DNOFORMS
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 dlltst1 -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 dnslook -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 dynCli -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 finger -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 ftpthrd -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 ftpserv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 ftptst -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpasp -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpasy -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpchk -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpdmo -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpget -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httppg -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httpthrd -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 httptst -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 icsdll1
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mailrcv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mailrob -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mailsnd -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 md5test -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mimedemo -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mimetst -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 mtsrv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 newsrdr -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 nslookup -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 pingtst -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 pop3mime -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 recv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 sender -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 server5 -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 sockstst -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 srvdemo -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 srvtcp -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 tcpsrv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 tnclient -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 tndemo -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 tnsrv -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 twschat -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 udplstn -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 udpsend -CG
if errorlevel 1 goto error
call ..\..\delphi3\dcc3 webserv -CG
if errorlevel 1 goto error
goto done

:error
@echo Compile error
goto done

:done
@cd %ICS_PATH%\DELPHI3

:end
