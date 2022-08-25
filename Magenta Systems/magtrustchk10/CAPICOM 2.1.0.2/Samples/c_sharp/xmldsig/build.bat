@ECHO OFF
ECHO The sample requires at least one certificate, you can make one using MakeCert.Exe
REM makecert -sk MyContainer -ss MY -n "CN=Joe Smith"
ECHO.
ECHO To use CAPICOM from .NET you need to create the RCW, you can do so using tlbimp.exe
REM tlbimp capicom.dll /out:Interop.CAPICOM.dll
ECHO.
ECHO To verify messages you need the helper library CertGetKey.dll, before you build this makesure your enviroment includes the appropriate INCLUDE and LIB environment variables.
REM cl /LD CertGetKey.c advapi32.lib crypt32.lib
ECHO.
csc /debug+ /res:.\resources\traffic_light.gif,traffic_light.gif /res:.\resources\Traffic.ico,Traffic.ico /r:System.dll,System.Web.dll,System.Windows.Forms.dll,System.Xml.dll,System.Drawing.dll,System.Security.dll,Interop.CAPICOM.dll Messages.cs SignVerify.cs AboutBox.cs MainWindow.cs XMLDSIG.cs

