@echo off
if exist source\OverbyteIcsWSocket.pas goto clean
echo.
echo.
echo Not an ICS repository
goto end
:clean

REM Clear samples
if exist samples\delphi\FtpDemos\OverbyteIcs*.rsm del samples\delphi\FtpDemos\OverbyteIcs*.rsm
if exist samples\delphi\FtpDemos\OverbyteIcs*_Icon.ico del samples\delphi\FtpDemos\OverbyteIcs*_Icon.ico
if exist samples\delphi\FtpDemos\OverbyteIcs*.exe del samples\delphi\FtpDemos\OverbyteIcs*.exe
if exist samples\delphi\FtpDemos\OverbyteIcs*.dll del samples\delphi\FtpDemos\OverbyteIcs*.dll
if exist samples\delphi\FtpDemos\OverbyteIcs*.ddp del samples\delphi\FtpDemos\OverbyteIcs*.ddp
if exist samples\delphi\FtpDemos\Dcu\*.dcu del samples\delphi\FtpDemos\Dcu\*.dcu
if exist samples\delphi\FtpDemos\OverbyteIcs*.dcu del samples\delphi\FtpDemos\OverbyteIcs*.dcu
if exist samples\delphi\FtpDemos\OverbyteIcs*.dproj del samples\delphi\FtpDemos\OverbyteIcs*.dproj
if exist samples\delphi\FtpDemos\OverbyteIcs*.dproj.2007 del samples\delphi\FtpDemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\FtpDemos\OverbyteIcs*.bdsproj del samples\delphi\FtpDemos\OverbyteIcs*.bdsproj
if exist samples\delphi\FtpDemos\OverbyteIcs*.bdsgroup del samples\delphi\FtpDemos\OverbyteIcs*.bdsgroup
if exist samples\delphi\FtpDemos\OverbyteIcs*.groupproj del samples\delphi\FtpDemos\OverbyteIcs*.groupproj
if exist samples\delphi\FtpDemos\dcu\OverbyteIcs*.dcu del samples\delphi\FtpDemos\dcu\OverbyteIcs*.dcu
if exist samples\delphi\FtpDemos\OverbyteIcs*.cfg del samples\delphi\FtpDemos\OverbyteIcs*.cfg
if exist samples\delphi\FtpDemos\OverbyteIcs*.~*  del samples\delphi\FtpDemos\OverbyteIcs*.~*
if exist samples\delphi\FtpDemos\OverbyteIcs*.bak del samples\delphi\FtpDemos\OverbyteIcs*.bak
if exist samples\delphi\FtpDemos\OverbyteIcs*.dproj.local del samples\delphi\FtpDemos\OverbyteIcs*.dproj.local
if exist samples\delphi\FtpDemos\OverbyteIcs*.groupproj.local del samples\delphi\FtpDemos\OverbyteIcs*.groupproj.local
if exist samples\delphi\FtpDemos\OverbyteIcs*.bdsproj.local del samples\delphi\FtpDemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\FtpDemos\OverbyteIcs*.bdsgroup.local del samples\delphi\FtpDemos\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\FtpDemos\OverbyteIcs*.identcache del samples\delphi\FtpDemos\OverbyteIcs*.identcache
if exist samples\delphi\FtpDemos\FtpDemos.groupproj del samples\delphi\FtpDemos\FtpDemos.groupproj
if exist samples\delphi\FtpDemos\FtpDemos.bdsgroup del samples\delphi\FtpDemos\FtpDemos.bdsgroup
if exist samples\delphi\FtpDemos\FtpDemos.groupproj.local del samples\delphi\FtpDemos\FtpDemos.groupproj.local
if exist samples\delphi\FtpDemos\FtpDemos.bdsgroup.local del samples\delphi\FtpDemos\FtpDemos.bdsgroup.local

if exist samples\delphi\MailNewsDemos\OverbyteIcs*.rsm del samples\delphi\MailNewsDemos\OverbyteIcs*.rsm
if exist samples\delphi\MailNewsDemos\OverbyteIcs*_Icon.ico del samples\delphi\MailNewsDemos\OverbyteIcs*_Icon.ico
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.exe del samples\delphi\MailNewsDemos\OverbyteIcs*.exe
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.dll del samples\delphi\MailNewsDemos\OverbyteIcs*.dll
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.ddp del samples\delphi\MailNewsDemos\OverbyteIcs*.ddp
if exist samples\delphi\MailNewsDemos\Dcu\*.dcu del samples\delphi\MailNewsDemos\Dcu\*.dcu
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.dcu del samples\delphi\MailNewsDemos\OverbyteIcs*.dcu
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.dproj del samples\delphi\MailNewsDemos\OverbyteIcs*.dproj
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.dproj.2007 del samples\delphi\MailNewsDemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.bdsproj del samples\delphi\MailNewsDemos\OverbyteIcs*.bdsproj
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.bdsgroup del samples\delphi\MailNewsDemos\OverbyteIcs*.bdsgroup
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.groupproj del samples\delphi\MailNewsDemos\OverbyteIcs*.groupproj
if exist samples\delphi\MailNewsDemos\dcu\OverbyteIcs*.dcu del samples\delphi\MailNewsDemos\dcu\OverbyteIcs*.dcu
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.cfg del samples\delphi\MailNewsDemos\OverbyteIcs*.cfg
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.~*  del samples\delphi\MailNewsDemos\OverbyteIcs*.~*
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.bak del samples\delphi\MailNewsDemos\OverbyteIcs*.bak
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.dproj.local del samples\delphi\MailNewsDemos\OverbyteIcs*.dproj.local
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.groupproj.local del samples\delphi\MailNewsDemos\OverbyteIcs*.groupproj.local
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.bdsproj.local del samples\delphi\MailNewsDemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.bdsgroup.local del samples\delphi\MailNewsDemos\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\MailNewsDemos\OverbyteIcs*.identcache del samples\delphi\MailNewsDemos\OverbyteIcs*.identcache
if exist samples\delphi\MailNewsDemos\MailNewsDemos.groupproj del samples\delphi\MailNewsDemos\MailNewsDemos.groupproj
if exist samples\delphi\MailNewsDemos\MailNewsDemos.bdsgroup del samples\delphi\MailNewsDemos\MailNewsDemos.bdsgroup
if exist samples\delphi\MailNewsDemos\MailNewsDemos.groupproj.local del samples\delphi\MailNewsDemos\MailNewsDemos.groupproj.local
if exist samples\delphi\MailNewsDemos\MailNewsDemos.bdsgroup.local del samples\delphi\MailNewsDemos\MailNewsDemos.bdsgroup.local


if exist samples\delphi\OtherDemos\OverbyteIcs*.rsm del samples\delphi\OtherDemos\OverbyteIcs*.rsm
if exist samples\delphi\OtherDemos\OverbyteIcs*_Icon.ico del samples\delphi\OtherDemos\OverbyteIcs*_Icon.ico
if exist samples\delphi\OtherDemos\OverbyteIcs*.exe del samples\delphi\OtherDemos\OverbyteIcs*.exe
if exist samples\delphi\OtherDemos\OverbyteIcs*.dll del samples\delphi\OtherDemos\OverbyteIcs*.dll
if exist samples\delphi\OtherDemos\OverbyteIcs*.ddp del samples\delphi\OtherDemos\OverbyteIcs*.ddp
if exist samples\delphi\OtherDemos\Dcu\*.dcu del samples\delphi\OtherDemos\Dcu\*.dcu
if exist samples\delphi\OtherDemos\OverbyteIcs*.dcu del samples\delphi\OtherDemos\OverbyteIcs*.dcu
if exist samples\delphi\OtherDemos\OverbyteIcs*.dproj del samples\delphi\OtherDemos\OverbyteIcs*.dproj
if exist samples\delphi\OtherDemos\OverbyteIcs*.dproj.2007 del samples\delphi\OtherDemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\OtherDemos\OverbyteIcs*.bdsproj del samples\delphi\OtherDemos\OverbyteIcs*.bdsproj
if exist samples\delphi\OtherDemos\OverbyteIcs*.bdsgroup del samples\delphi\OtherDemos\OverbyteIcs*.bdsgroup
if exist samples\delphi\OtherDemos\OverbyteIcs*.groupproj del samples\delphi\OtherDemos\OverbyteIcs*.groupproj
if exist samples\delphi\OtherDemos\dcu\OverbyteIcs*.dcu del samples\delphi\OtherDemos\dcu\OverbyteIcs*.dcu
if exist samples\delphi\OtherDemos\OverbyteIcs*.cfg del samples\delphi\OtherDemos\OverbyteIcs*.cfg
if exist samples\delphi\OtherDemos\OverbyteIcs*.~*  del samples\delphi\OtherDemos\OverbyteIcs*.~*
if exist samples\delphi\OtherDemos\OverbyteIcs*.bak del samples\delphi\OtherDemos\OverbyteIcs*.bak
if exist samples\delphi\OtherDemos\OverbyteIcs*.dproj.local del samples\delphi\OtherDemos\OverbyteIcs*.dproj.local
if exist samples\delphi\OtherDemos\OverbyteIcs*.groupproj.local del samples\delphi\OtherDemos\OverbyteIcs*.groupproj.local
if exist samples\delphi\OtherDemos\OverbyteIcs*.bdsproj.local del samples\delphi\OtherDemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\OtherDemos\OverbyteIcs*.bdsgroup.local del samples\delphi\OtherDemos\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\OtherDemos\OverbyteIcs*.identcache del samples\delphi\OtherDemos\OverbyteIcs*.identcache
if exist samples\delphi\OtherDemos\OtherDemos.groupproj del samples\delphi\OtherDemos\OtherDemos.groupproj
if exist samples\delphi\OtherDemos\OtherDemos.bdsgroup del samples\delphi\OtherDemos\OtherDemos.bdsgroup
if exist samples\delphi\OtherDemos\OtherDemos.groupproj.local del samples\delphi\OtherDemos\OtherDemos.groupproj.local
if exist samples\delphi\OtherDemos\OtherDemos.bdsgroup.local del samples\delphi\OtherDemos\OtherDemos.bdsgroup.local

if exist samples\delphi\SocketDemos\OverbyteIcs*.rsm del samples\delphi\SocketDemos\OverbyteIcs*.rsm
if exist samples\delphi\SocketDemos\OverbyteIcs*_Icon.ico del samples\delphi\SocketDemos\OverbyteIcs*_Icon.ico
if exist samples\delphi\SocketDemos\OverbyteIcs*.exe del samples\delphi\SocketDemos\OverbyteIcs*.exe
if exist samples\delphi\SocketDemos\OverbyteIcs*.dll del samples\delphi\SocketDemos\OverbyteIcs*.dll
if exist samples\delphi\SocketDemos\OverbyteIcs*.ddp del samples\delphi\SocketDemos\OverbyteIcs*.ddp
if exist samples\delphi\SocketDemos\Dcu\*.dcu del samples\delphi\SocketDemos\Dcu\*.dcu
if exist samples\delphi\SocketDemos\OverbyteIcs*.dcu del samples\delphi\SocketDemos\OverbyteIcs*.dcu
if exist samples\delphi\SocketDemos\OverbyteIcs*.dproj del samples\delphi\SocketDemos\OverbyteIcs*.dproj
if exist samples\delphi\SocketDemos\OverbyteIcs*.dproj.2007 del samples\delphi\SocketDemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\SocketDemos\OverbyteIcs*.bdsproj del samples\delphi\SocketDemos\OverbyteIcs*.bdsproj
if exist samples\delphi\SocketDemos\OverbyteIcs*.bdsgroup del samples\delphi\SocketDemos\OverbyteIcs*.bdsgroup
if exist samples\delphi\SocketDemos\OverbyteIcs*.groupproj del samples\delphi\SocketDemos\OverbyteIcs*.groupproj
if exist samples\delphi\SocketDemos\dcu\OverbyteIcs*.dcu del samples\delphi\SocketDemos\dcu\OverbyteIcs*.dcu
if exist samples\delphi\SocketDemos\OverbyteIcs*.cfg del samples\delphi\SocketDemos\OverbyteIcs*.cfg
if exist samples\delphi\SocketDemos\OverbyteIcs*.~*  del samples\delphi\SocketDemos\OverbyteIcs*.~*
if exist samples\delphi\SocketDemos\OverbyteIcs*.bak del samples\delphi\SocketDemos\OverbyteIcs*.bak
if exist samples\delphi\SocketDemos\OverbyteIcs*.dproj.local del samples\delphi\SocketDemos\OverbyteIcs*.dproj.local
if exist samples\delphi\SocketDemos\OverbyteIcs*.groupproj.local del samples\delphi\SocketDemos\OverbyteIcs*.groupproj.local
if exist samples\delphi\SocketDemos\OverbyteIcs*.bdsproj.local del samples\delphi\SocketDemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\SocketDemos\OverbyteIcs*.bdsgroup.local del samples\delphi\SocketDemos\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\SocketDemos\OverbyteIcs*.identcache del samples\delphi\SocketDemos\OverbyteIcs*.identcache
if exist samples\delphi\SocketDemos\SocketDemos.groupproj del samples\delphi\SocketDemos\SocketDemos.groupproj
if exist samples\delphi\SocketDemos\SocketDemos.bdsgroup del samples\delphi\SocketDemos\SocketDemos.bdsgroup
if exist samples\delphi\SocketDemos\SocketDemos.groupproj.local del samples\delphi\SocketDemos\SocketDemos.groupproj.local
if exist samples\delphi\SocketDemos\SocketDemos.bdsgroup.local del samples\delphi\SocketDemos\SocketDemos.bdsgroup.local

if exist samples\delphi\WebDemos\OverbyteIcs*.rsm del samples\delphi\WebDemos\OverbyteIcs*.rsm
if exist samples\delphi\WebDemos\OverbyteIcs*_Icon.ico del samples\delphi\WebDemos\OverbyteIcs*_Icon.ico
if exist samples\delphi\WebDemos\OverbyteIcs*.exe del samples\delphi\WebDemos\OverbyteIcs*.exe
if exist samples\delphi\WebDemos\OverbyteIcs*.dll del samples\delphi\WebDemos\OverbyteIcs*.dll
if exist samples\delphi\WebDemos\OverbyteIcs*.ddp del samples\delphi\WebDemos\OverbyteIcs*.ddp
if exist samples\delphi\WebDemos\Dcu\*.dcu del samples\delphi\WebDemos\Dcu\*.dcu
if exist samples\delphi\WebDemos\OverbyteIcs*.dcu del samples\delphi\WebDemos\OverbyteIcs*.dcu
if exist samples\delphi\WebDemos\OverbyteIcs*.dproj del samples\delphi\WebDemos\OverbyteIcs*.dproj
if exist samples\delphi\WebDemos\OverbyteIcs*.dproj.2007 del samples\delphi\WebDemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\WebDemos\OverbyteIcs*.bdsproj del samples\delphi\WebDemos\OverbyteIcs*.bdsproj
if exist samples\delphi\WebDemos\OverbyteIcs*.bdsgroup del samples\delphi\WebDemos\OverbyteIcs*.bdsgroup
if exist samples\delphi\WebDemos\OverbyteIcs*.groupproj del samples\delphi\WebDemos\OverbyteIcs*.groupproj
if exist samples\delphi\WebDemos\dcu\OverbyteIcs*.dcu del samples\delphi\WebDemos\dcu\OverbyteIcs*.dcu
if exist samples\delphi\WebDemos\OverbyteIcs*.cfg del samples\delphi\WebDemos\OverbyteIcs*.cfg
if exist samples\delphi\WebDemos\OverbyteIcs*.~*  del samples\delphi\WebDemos\OverbyteIcs*.~*
if exist samples\delphi\WebDemos\OverbyteIcs*.bak del samples\delphi\WebDemos\OverbyteIcs*.bak
if exist samples\delphi\WebDemos\OverbyteIcs*.dproj.local del samples\delphi\WebDemos\OverbyteIcs*.dproj.local
if exist samples\delphi\WebDemos\OverbyteIcs*.groupproj.local del samples\delphi\WebDemos\OverbyteIcs*.groupproj.local
if exist samples\delphi\WebDemos\OverbyteIcs*.bdsproj.local del samples\delphi\WebDemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\WebDemos\OverbyteIcs*.bdsgroup.local del samples\delphi\WebDemos\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\WebDemos\OverbyteIcs*.identcache del samples\delphi\WebDemos\OverbyteIcs*.identcache
if exist samples\delphi\WebDemos\WebDemos.groupproj del samples\delphi\WebDemos\WebDemos.groupproj
if exist samples\delphi\WebDemos\WebDemos.bdsgroup del samples\delphi\WebDemos\WebDemos.bdsgroup
if exist samples\delphi\WebDemos\WebDemos.groupproj.local del samples\delphi\WebDemos\WebDemos.groupproj.local
if exist samples\delphi\WebDemos\WebDemos.bdsgroup.local del samples\delphi\WebDemos\WebDemos.bdsgroup.local

if exist samples\delphi\sslinternet\OverbyteIcs*.rsm del samples\delphi\sslinternet\OverbyteIcs*.rsm
if exist samples\delphi\sslinternet\OverbyteIcs*_Icon.ico del samples\delphi\sslinternet\OverbyteIcs*_Icon.ico
if exist samples\delphi\sslinternet\OverbyteIcs*.exe del samples\delphi\sslinternet\OverbyteIcs*.exe
if exist samples\delphi\sslinternet\OverbyteIcs*.ddp del samples\delphi\sslinternet\OverbyteIcs*.ddp
if exist samples\delphi\sslinternet\Dcu\*.dcu del samples\delphi\sslinternet\Dcu\*.dcu
if exist samples\delphi\sslinternet\OverbyteIcs*.dcu del samples\delphi\sslinternet\OverbyteIcs*.dcu
if exist samples\delphi\sslinternet\OverbyteIcs*.dproj del samples\delphi\sslinternet\OverbyteIcs*.dproj
if exist samples\delphi\sslinternet\OverbyteIcs*.dproj.2007 del samples\delphi\sslinternet\OverbyteIcs*.dproj.2007
if exist samples\delphi\sslinternet\OverbyteIcs*.bdsproj del samples\delphi\sslinternet\OverbyteIcs*.bdsproj
if exist samples\delphi\sslinternet\OverbyteIcs*.bdsgroup del samples\delphi\sslinternet\OverbyteIcs*.bdsgroup
if exist samples\delphi\sslinternet\OverbyteIcs*.groupproj del samples\delphi\sslinternet\OverbyteIcs*.groupproj
if exist samples\delphi\sslinternet\OverbyteIcs*.cfg del samples\delphi\sslinternet\OverbyteIcs*.cfg
if exist samples\delphi\sslinternet\OverbyteIcs*.~*  del samples\delphi\sslinternet\OverbyteIcs*.~*
if exist samples\delphi\sslinternet\OverbyteIcs*.bak del samples\delphi\sslinternet\OverbyteIcs*.bak
if exist samples\delphi\sslinternet\OverbyteIcs*.dproj.local del samples\delphi\sslinternet\OverbyteIcs*.dproj.local
if exist samples\delphi\sslinternet\OverbyteIcs*.groupproj.local del samples\delphi\sslinternet\OverbyteIcs*.groupproj.local
if exist samples\delphi\sslinternet\OverbyteIcs*.bdsproj.local del samples\delphi\sslinternet\OverbyteIcs*.bdsproj.local
if exist samples\delphi\sslinternet\OverbyteIcs*.bdsgroup.local del samples\delphi\sslinternet\OverbyteIcs*.bdsgroup.local
if exist samples\delphi\sslinternet\OverbyteIcs*.identcache del samples\delphi\sslinternet\OverbyteIcs*.identcache

if exist samples\delphi\miscdemos\OverbyteIcs*.rsm del samples\delphi\miscdemos\OverbyteIcs*.rsm
if exist samples\delphi\miscdemos\OverbyteIcs*_Icon.icon del samples\delphi\miscdemos\OverbyteIcs*_Icon.icon
if exist samples\delphi\miscdemos\OverbyteIcs*.exe del samples\delphi\miscdemos\OverbyteIcs*.exe
if exist samples\delphi\miscdemos\OverbyteIcs*.ddp del samples\delphi\miscdemos\OverbyteIcs*.ddp
if exist samples\delphi\miscdemos\Dcu\*.dcu del samples\delphi\miscdemos\Dcu\*.dcu
if exist samples\delphi\miscdemos\OverbyteIcs*.dcu del samples\delphi\miscdemos\OverbyteIcs*.dcu
if exist samples\delphi\miscdemos\OverbyteIcs*.dproj del samples\delphi\miscdemos\OverbyteIcs*.dproj
if exist samples\delphi\miscdemos\OverbyteIcs*.dproj.2007 del samples\delphi\miscdemos\OverbyteIcs*.dproj.2007
if exist samples\delphi\miscdemos\OverbyteIcs*.bdsproj del samples\delphi\miscdemos\OverbyteIcs*.bdsproj
if exist samples\delphi\miscdemos\MiscDemos.bdsgroup del samples\delphi\miscdemos\MiscDemos.bdsgroup
if exist samples\delphi\miscdemos\MiscDemos.groupproj del samples\delphi\miscdemos\MiscDemos.groupproj
if exist samples\delphi\miscdemos\OverbyteIcs*.cfg del samples\delphi\miscdemos\OverbyteIcs*.cfg
if exist samples\delphi\miscdemos\OverbyteIcs*.~*  del samples\delphi\miscdemos\OverbyteIcs*.~*
if exist samples\delphi\miscdemos\OverbyteIcs*.bak del samples\delphi\miscdemos\OverbyteIcs*.bak
if exist samples\delphi\miscdemos\OverbyteIcs*.dproj.local del samples\delphi\miscdemos\OverbyteIcs*.dproj.local
if exist samples\delphi\miscdemos\MiscDemos.groupproj.local del samples\delphi\miscdemos\MiscDemos.groupproj.local
if exist samples\delphi\miscdemos\OverbyteIcs*.bdsproj.local del samples\delphi\miscdemos\OverbyteIcs*.bdsproj.local
if exist samples\delphi\miscdemos\MiscDemos.bdsgroup.local del samples\delphi\miscdemos\MiscDemos.bdsgroup.local
if exist samples\delphi\miscdemos\OverbyteIcs*.identcache del samples\delphi\miscdemos\OverbyteIcs*.identcache



if exist samples\delphi\platformdemos\*.identcache del samples\delphi\platformdemos\*.identcache
if exist samples\delphi\platformdemos\*.dproj.local del samples\delphi\platformdemos\*.dproj.local
if exist samples\delphi\platformdemos\win32\debug\*.* del samples\delphi\platformdemos\win32\debug\*.*
if exist samples\delphi\platformdemos\win32\release\*.* del samples\delphi\platformdemos\win32\release\*.*
if exist samples\delphi\platformdemos\win64\debug\*.* del samples\delphi\platformdemos\win64\debug\*.*
if exist samples\delphi\platformdemos\win64\release\*.* del samples\delphi\platformdemos\win64\release\*.*
if exist samples\delphi\platformdemos\osx32\debug\*.* del samples\delphi\platformdemos\osx32\debug\*.*
if exist samples\delphi\platformdemos\osx32\release\*.* del samples\delphi\platformdemos\osx32\release\*.*


if exist samples\cpp\internet\OverbyteIcs*.dcu del samples\cpp\internet\OverbyteIcs*.dcu
if exist samples\cpp\internet\OverbyteIcs*.~*  del samples\cpp\internet\OverbyteIcs*.~*
if exist samples\cpp\internet\OverbyteIcs*.ba  del samples\cpp\internet\OverbyteIcs*.bak
if exist samples\cpp\internet\OverbyteIcs*.obj del samples\cpp\internet\OverbyteIcs*.obj

if exist samples\cpp\internet\bcb6\OverbyteIcs*.dcu del samples\cpp\internet\bcb6\OverbyteIcs*.dcu
if exist samples\cpp\internet\bcb6\OverbyteIcs*.~*  del samples\cpp\internet\bcb6\OverbyteIcs*.~*
if exist samples\cpp\internet\bcb6\OverbyteIcs*.bak del samples\cpp\internet\bcb6\OverbyteIcs*.bak
if exist samples\cpp\internet\bcb6\OverbyteIcs*.obj del samples\cpp\internet\bcb6\OverbyteIcs*.obj

echo Done !
:end
pause