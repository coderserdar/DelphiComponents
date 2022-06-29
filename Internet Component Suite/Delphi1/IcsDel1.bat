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
REM * Remember to install all components in Delphi 1 !  *
REM * Remember to use Delphi 1 to open all forms and    *
REM * ignore Font.CharSet and OldCreateOrder properties *
REM *                                                   *
REM * * * * * * * * * * * * * * * * * * * * * * * * * * *

SET DELPHI_PATH=C:\PROGRA~1\BORLAND\DELPHI1
SET ICS_PATH=D:\FPIETTE

PATH=C:\WINDOWS;C:\WINDOWS\COMMAND;%DELPHI_PATH%\BIN

echo /cw                            >dcc.cfg 
echo /m                            >>dcc.cfg 
echo /r%DELPHI_PATH%\LIB           >>dcc.cfg 
echo /u%DELPHI_PATH%\LIB           >>dcc.cfg 
echo /i%DELPHI_PATH%\LIB           >>dcc.cfg 
echo /E%ICS_PATH%\DELPHI1          >>dcc.cfg 
echo /O%ICS_PATH%\DELPHI1          >>dcc.cfg 
echo /I%ICS_PATH%\DELPHI\VC        >>dcc.cfg 
echo /I%ICS_PATH%\DELPHI\VC32      >>dcc.cfg 
echo /I%ICS_PATH%\DELPHI\INTERNET  >>dcc.cfg 
echo /R%ICS_PATH%\DELPHI\VC        >>dcc.cfg 
echo /R%ICS_PATH%\DELPHI\VC32      >>dcc.cfg 
echo /R%ICS_PATH%\DELPHI\INTERNET  >>dcc.cfg 
echo /U%ICS_PATH%\DELPHI\VC        >>dcc.cfg 
echo /U%ICS_PATH%\DELPHI\VC32      >>dcc.cfg 
echo /U%ICS_PATH%\DELPHI\INTERNET  >>dcc.cfg 

d:
cd %ICS_PATH%\delphi\internet
call ..\..\delphi1\dcc1 clidemo
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 client5
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 client7
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 dnslook
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 dynCli
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 finger
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 ftpserv
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 ftptst
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httpasp
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httpasy
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httpchk
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httpdmo
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httpget
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httppg
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 httptst
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 mailrcv
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 mailrob
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 mailsnd
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 md5test
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 mimedemo
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 mimetst
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 newsrdr
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 nslookup
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 pop3mime
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 recv
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 sender
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 server5
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 sockstst
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 srvdemo
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 srvtcp
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 tcpsrv
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 tnclient
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 tndemo
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 tnsrv
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 twschat
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 udplstn
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 udpsend
if errorlevel 1 goto error
call ..\..\delphi1\dcc1 webserv
if errorlevel 1 goto error
goto done

:error
@echo Compile error
goto done

:done
@cd %ICS_PATH%\delphi1

:end
