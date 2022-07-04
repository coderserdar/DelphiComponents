@echo off
if exist delphi\vc32\OverbyteIcsWSocket.pas goto clean
echo.
echo.
echo Not an ICS repository
goto end
:clean
echo Hit return to clean temporary files, of CTRL+C to abort...
pause
if exist delphi\vc32\OverbyteIcs*.dcu del delphi\vc32\OverbyteIcs*.dcu
if exist delphi\vc32\OverbyteIcs*.~*  del delphi\vc32\OverbyteIcs*.~*
if exist delphi\vc32\OverbyteIcs*.bak del delphi\vc32\OverbyteIcs*.bak
if exist delphi\vc32\OverbyteIcs*.obj del delphi\vc32\OverbyteIcs*.obj
if exist delphi\vc32\OverbyteIcs*.dproj.local del delphi\vc32\OverbyteIcs*.dproj.local
if exist delphi\vc32\OverbyteIcs*.bdsproj.local del delphi\vc32\OverbyteIcs*.bdsproj.local
if exist delphi\vc32\OverbyteIcs*.identcache del delphi\vc32\OverbyteIcs*.identcache

if exist delphi\internet\OverbyteIcs*.dcu del delphi\internet\OverbyteIcs*.dcu
if exist delphi\internet\OverbyteIcs*.~*  del delphi\internet\OverbyteIcs*.~*
if exist delphi\internet\OverbyteIcs*.bak del delphi\internet\OverbyteIcs*.bak
if exist delphi\internet\OverbyteIcs*.obj del delphi\internet\OverbyteIcs*.obj
if exist delphi\internet\OverbyteIcs*.dproj.local del delphi\internet\OverbyteIcs*.dproj.local
if exist delphi\internet\OverbyteIcs*.groupproj.local del delphi\internet\OverbyteIcs*.groupproj.local
if exist delphi\internet\OverbyteIcs*.bdsproj.local del delphi\internet\OverbyteIcs*.bdsproj.local
if exist delphi\internet\OverbyteIcs*.bdsgroup.local del delphi\internet\OverbyteIcs*.bdsgroup.local
if exist delphi\internet\OverbyteIcs*.identcache del delphi\internet\OverbyteIcs*.identcache

if exist delphi\sslinternet\OverbyteIcs*.dcu del delphi\sslinternet\OverbyteIcs*.dcu
if exist delphi\sslinternet\OverbyteIcs*.~*  del delphi\sslinternet\OverbyteIcs*.~*
if exist delphi\sslinternet\OverbyteIcs*.bak del delphi\sslinternet\OverbyteIcs*.bak
if exist delphi\sslinternet\OverbyteIcs*.obj del delphi\sslinternet\OverbyteIcs*.obj
if exist delphi\sslinternet\OverbyteIcs*.dproj.local del delphi\sslinternet\OverbyteIcs*.dproj.local
if exist delphi\sslinternet\OverbyteIcs*.groupproj.local del delphi\sslinternet\OverbyteIcs*.groupproj.local
if exist delphi\sslinternet\OverbyteIcs*.bdsproj.local del delphi\sslinternet\OverbyteIcs*.bdsproj.local
if exist delphi\sslinternet\OverbyteIcs*.bdsgroup.local del delphi\sslinternet\OverbyteIcs*.bdsgroup.local
if exist delphi\sslinternet\OverbyteIcs*.identcache del delphi\sslinternet\OverbyteIcs*.identcache

if exist delphi\miscdemos\OverbyteIcs*.dcu del delphi\miscdemos\OverbyteIcs*.dcu
if exist delphi\miscdemos\OverbyteIcs*.~*  del delphi\miscdemos\OverbyteIcs*.~*
if exist delphi\miscdemos\OverbyteIcs*.bak del delphi\miscdemos\OverbyteIcs*.bak
if exist delphi\miscdemos\OverbyteIcs*.obj del delphi\miscdemos\OverbyteIcs*.obj
if exist delphi\miscdemos\OverbyteIcs*.dproj.local del delphi\miscdemos\OverbyteIcs*.dproj.local
if exist delphi\miscdemos\OverbyteIcs*.groupproj.local del delphi\miscdemos\OverbyteIcs*.groupproj.local
if exist delphi\miscdemos\OverbyteIcs*.bdsproj.local del delphi\miscdemos\OverbyteIcs*.bdsproj.local
if exist delphi\miscdemos\OverbyteIcs*.bdsgroup.local del delphi\miscdemos\OverbyteIcs*.bdsgroup.local
if exist delphi\miscdemos\OverbyteIcs*.identcache del delphi\miscdemos\OverbyteIcs*.identcache

if exist cpp\internet\OverbyteIcs*.dcu del cpp\internet\OverbyteIcs*.dcu
if exist cpp\internet\OverbyteIcs*.~*  del cpp\internet\OverbyteIcs*.~*
if exist cpp\internet\OverbyteIcs*.ba  del cpp\internet\OverbyteIcs*.bak
if exist cpp\internet\OverbyteIcs*.obj del cpp\internet\OverbyteIcs*.obj

if exist cpp\internet\bcb6\OverbyteIcs*.dcu del cpp\internet\bcb6\OverbyteIcs*.dcu
if exist cpp\internet\bcb6\OverbyteIcs*.~*  del cpp\internet\bcb6\OverbyteIcs*.~*
if exist cpp\internet\bcb6\OverbyteIcs*.bak del cpp\internet\bcb6\OverbyteIcs*.bak
if exist cpp\internet\bcb6\OverbyteIcs*.obj del cpp\internet\bcb6\OverbyteIcs*.obj

echo Done !
:end
pause
