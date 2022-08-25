CAPICOM C++ sample
-----------------------
Date:  January 15, 2001

Thi demonstration program run on Windows 9x with IE 5.0 or later, Windows NT 4.0 with
SP4 or later, Windows 2000 and Whistler.  It uses CAPICOM.dll an ActiveX component, that 
must be registered. To run these scripts under Windows 9x, download a current verison of
the windows scripting host from the web.

This sample program requires one or more test certificates. 

You can obtain a test certificate from a Certificate Authority such as Verisign.

Alternatively, you can create a self-signed cert using the SDK tool, makecert.exe

Download the signing tools from:
  http://msdn.microsoft.com/downloads/default.asp?URL=/code/sample.asp?url=/msdn-files/027/000/219/msdncompositedoc.xml

Once makecert.exe has be downloaded, run the filling command at the command prompt. Substitute 
your name for xxx, your organization for yyy and your company name for zzz

  makecert -r -n "cn=xxx, ou=yyy, corp = zzz" -ss my

The certificate is placed in the current user's MY store. Import the certificate created 
into your root store so that it is trusted.



This directory contains the following files:
readme.txt    --  this file

store.cpp     --  the C++ source code for this program.

To run this program, load it into Visual C++, build it, and run. The program displays certificates
from the current user MY store. Code in the program can be altered to open other certificate stores 
and display their contents.


