ICS - Internet Component Suite - V7 - RAD Studio 2009
=====================================================
(Aka FPIETTE's Components)

Disclaimer: ICS-V7 has been adapted to Delphi 2009 using pre-release
            software. Please check website for updated version.


Revised: Aug 4, 2008
http://www.overbyte.be

Table of content:
-----------------

- Legal issues
- Donate
- Register
- Contributions
- Installation
- Version Control repository
- Sample applications
- About SSL
- Support
- Release notes
- Midware
- Known problems
- Special thanks


Legal issues: 
-------------
              Copyright (C) 1997-2008 by François PIETTE 
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

              5. As this code make use of OpenSSL, your rights are restricted 
                 by OpenSSL license as soon as you use any SSL feature.
                 See http://www.openssl.org for details.



Donate
------

ICS is freeware. You can use it without paying anything except the registration
postcard (see "register" below). But of course donations are welcome. You can
send cash (Euro currency or US Dollars) in an envelop to my street address or
buy a gift certificate at Amazon in the UK. I will then use it to buy books.
Here is the direct URL at Amazon UK (nearest to my home, please don't use another):
http://www.amazon.co.uk/exec/obidos/gc-email-order1/ref=g_gc_email/202-6198323-6681414
For more generous amount, contact me by email.


Register
--------

ICS is freeware. If you use the components, you must register by sending a 
picture postcard showing the area you live in and some beautiful stamps for 
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me. Write on the postcard that it is your ICS registration.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, street address, EMail and website.


Contributions:
--------------

ICS has been designed by François PIETTE but many other peoples are working on the
components and sample programs. The history of changes in each source file list
all developers having contributed (Wehn no name is given, the change is by F. Piette).
I can't list all contributors here but I want to specially thanks two specially active
contributors:
    - Arno Garrels <arno.garrels@gmx.de>
    - Angus Robertson <angus@magsys.co.uk>


Installation:
-------------

ICS-V7 has been designed for Delphi 2009 and up, and BCB 2009 and up.

The zip file has subdirectories in it. You must use the WinZip "Use folder 
names" option to restore this directory tree or you will have problems 
because the files would not be in their proper subdirectories.

This is the subdirectory layout:

.\                            Info directory
.\delphi\internet             Delphi.W32 sample applications (all Win32 Delphi versions)
.\delphi\sslinternet          Delphi.W32 SSL-enabled sample applications (all Win32 Delphi versions)
.\delphi\internet\WebServData Directory for WebServ demo data files
.\cpp\internet                C++Builder sample applications
.\cpp\internet\cb2006         C++Builder 2006 projects
.\cpp\internet\cb2007         C++Builder 2007 projects
.\cpp\internet\cb2009         C++Builder 2009 projects
.\delphi\vc32                 Delphi (7 and up) and C++Builder (2006 and up) components
.\Install                     Component packages project groups for all versions


UPGRADING and REINSTALLING
Uninstall an existing ICS package (Menu | Component | Install Packages, select
the component package and click Remove).  
Rename the old ICS directory and unzip to a new or empty directory, remove the
old path from the library path and add the new VC32 directory to the library 
path under Tools | Options |...

All DELPHI and C++ BUILDER VERSIONS/WIN32
Always upgrade your compiler with the latest update available from Borland.
Always update your system with http://windowsupdate.microsoft.com

SSL or not SSL?
By default the SSL code is compiled into the run-time package and additional SSL- 
enabled components are installed. In order to not compile the SSL code into the
run-time package and to not install the SSL-Enabled components you need to remove
the conditional define USE_SSL from both the run-time and design-time package.  
However if you do not build your applications with run-time packages it is 
recommended to build the packages with default settings. The SSL code will the
be compiled into your applications depending on whether the conditional define 
USE_SSL is set in the project options or not.
Actual use of SSL in your applications also requires LIBEAY32.DLL and SSLEAY32.DLL
being available somewhere in the path, more details in IcsSslHowTo.txt. 

INSTALLATION USING THE INSTALL PROJECT GROUPS
For each Delphi and C++ Builder version one project group is provided in directory
.\Install:

Delphi 7         :  D7Install.bpg
Delphi 2006      :  D2006Install.bdsgroup
Delphi 2007      :  D2007Install.groupproj
Delphi 2009      :  D2009Install.groupproj
C++ Builder 2006 :  CB2006Install.bdsgroup
C++ Builder 2007 :  CB2007Install.groupproj
C++ Builder 2009 :  CB2009Install.groupproj

1 - Do a File/Open Project, navigate to the Install directory, select the correct
file and open it. The project manager view should now display two package 
projects, one run-time and one design-time package. The run-time package name
contains the "Run" suffix. The design-time package name contains the "Design"
suffix.
2 - Select and Build the run-time package (do not install).
3 - Select and Install the design-time package.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All". 

ALTERNATE INSTALLATION USING THE PACKAGE PROJECT FILES:
For each Delphi and C++ Builder version two package project files exist in the
VC32 directory. One run-time and one design-time package project file. 
The run-time file name contains the "Run" suffix. The design-time file name
contains the "Design" suffix.

PACKAGE PROJECT FILE NAMES:
Delphi 7         :  OverbyteIcsD7Run.dpk, OverbyteIcsD7Design.dpk
Delphi 2006      :  OverbyteIcsD2006Run.bdsproj, OverbyteIcsD2006Design.bdsproj
Delphi 2007      :  OverbyteIcsD2007Run.dproj, OverbyteIcsD2007Design.dproj
Delphi 2009      :  OverbyteIcsD2009Run.dproj, OverbyteIcsD2009Design.dproj
C++ Builder 2006 :  OverbyteIcsCB2006Run.bdsproj, OverbyteIcsCB2006Design.bdsproj
C++ Builder 2007 :  OverbyteIcsCB2007Run.cbproj, OverbyteIcsCB2007Design.cbproj 
C++ Builder 2009 :  OverbyteIcsCB2009Run.cbproj, OverbyteIcsCB2009Design.cbproj

1 - Open and Build the run-time package project (do not install!).
2 - Open and Install the design-time package project.
(Do a File/Open Project, browse to the VC32 directory. Select the correct file
and open it. Then in the project manager view, right-click on the package, 
then click on either the Build or Install button.)

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".

[ToDo!! The following description of installation is no longer correct] 

DELPHI 2009/WIN32:
Directory VC32 contains OverbyteIcsDel120Package.dproj which is a package source for
all components. Using Delphi, do a file/open project (Ctrl-F11, browse to the 
VC32 directory. Select OverbyteIcsDel120Package.dproj and open it. Then in the project
manager view, right-click on OverbyteIcsDel120Package, then click on Install button.
After a few seconds, you should have a dialog box telling you the package 
OverbyteIcsDel120Package.bpl has been installed with a bunch of new components registered.
Then do a "Save All" (Shift-Ctrl-S) and a "Close All".
Having installed the package, verify that the VC32 directory has been added to
the Win32 Library Path (Tools / Options / Delphi Options / Library - Win32 / 
Library Path). If not, add it manually. It is not mandatory to add vc32 to the global
Delphi path, but it will be much easier for you because otherwise you'll have to
add it to each project.

Once the package is installed, you may open the sample projects. There is a
project group called OverByteIcsDel120Sam.groupproj which has all sample programs. Open it
with file/open project (Ctrl-F11), browse to the Internet directory, select 
and open OverByteIcsDel120Sam.groupproj. You will get some dialog box telling you that
resource files are missing (they have not been included in the zip file to 
save space) and are recreated by Delphi. It is OK. Any other error message 
is a problem you should fix. After all resource files have been recreated,
you should see in the project manager a group of projects called OverByteIcsDel120Sam.
In this group, you'll find all sample programs.

To compile all samples at once, do Project / Build all projects. This will 
take some time to compile all sample programs. Delphi may run out of
memory if you don't have enough RAM installed in your computer. 
If this happend, just build the projects one by one.


DELPHI 2007/WIN32:
Same as Delphi 2009 except package and projectgroup has "110" in their names instead
of "120".


DELPHI 2006/WIN32:
Directory VC32 contains OverbyteIcsDel100.bdsproj which is a package source for
all components. Using Delphi, do a file/open project (Ctrl-F11, browse to the 
VC32 directory. Select OverbyteIcsDel100.bdsproj and open it. Then in the project
manager view, right-click on OverbyteIcsDel100.bpl, then click on Install button.
After a few seconds, you should have a dialog box telling you the package 
IcsDel100.bpl has been installed with a bunch of new components registered.
Then do a "Save All" (Shift-Ctrl-S) and a "Close All".
Having installed the package, verify that the VC32 directory has been added to
the Win32 Library Path (Tools / Options / Delphi Options / Library - Win32 / 
Library Path). If not, add it manually.

Once the package is installed, you may open the sample projects. There is a
project group called OverbyteDel100Sam.bdsgroup which has all sample programs. Open it
with file/open project (Ctrl-F11), browse to the Internet directory, select 
and open OverbyteDel100Sam.bdsgroup. You will get some dialog box telling you that
resource files are missing (they have not been included in the zip file to 
save space) and are recreated by Delphi. It is OK. Any other error message 
is a problem you should fix. After all resource files have been recreated,
you should see in the project manager a group of projects called OverbyteDel100Sam.
In this group, you'll find all sample programs.

To compile all samples at once, do Project / Build all projects. This will 
take some time to compile all sample programs. Delphi may run out of
memory if you don't have large RAM installed. If this happend, just build
the project one by one.

DELPHI 2005/WIN32: 
Follow Delphi 2006/Win32 installion. Replace "90" by "100" in all project group
or packages.

DELPHI 7: Directory VC32 contains OverbyteIcsDel70.dpk which is a package source for
all components. Using Delphi, do a file/open, select *.dpk and browse to
the VC32 directory. Select OverbyteIcsDel70.dpk and open it. Then click on the 
Install button. You should see the FPiette tab on the component gallery.
Add VC32 directory path to your library path (Tools menu / Environment Options
/ Library / Library Path. Add VC32 path at the end of the existing path).

Once the package is installed, you may open the sample projects. There is a
project group called OverbyteDel70Sam.bpg which has all sample programs. Open it
with file/open, browse to the Internet directory, select and open OverbyteDel70Sam.bpg.
Then Project/Build all projects. You'll get all sample programs compiled.
It is likely that for each project, Delphi complains about a missing .res
file. This is not a problem, Delphi will recreate it as needed. They have not
been included to save space in the zip file.

Note 1: Delphi may run out of memory if you ask to compile all projects at 
once. If you have not enough RAM, then compile each project individually.

Note 2: Delphi has warnings which triggers a lot of messages for 100% OK
code. You can turn those warnings off in the project/ options / Compiler messages
and deselecting: "Deprecated symbol", "Platform symbol", "usafe type", "unsafe code", 
"usafe typecast". Those are intended for .NET and Linux portability. You can
safely ignore them if you run windows. For you facility, I included a utility 
SetProjectOptions (source code, you must compile it) in the internet directory.
This utility will update project options to disable the warnings.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

CBUILDER 2006:
Follow the installation procedure described for Delphi 2006. Just change
the project group and package name: replace "del" by "bcb" in their names.
You can't have Delphi 2006 and CBuilder 2006 packages installed at the 
same time in the IDE. So when switching from one to the other, be sure to
remove the one you don't need.
If you need both BCB and Delphi personnalities ate the same time, then
use Delphi 2006 package (OverbyteIcsDel100.bpl) and change his options to make it
a dual mode Delphi/CPP package. See Borland documentation.

BCB6: First you need to install all components. There is a package in
Delphi\VC32 directory, called OverbyteIcsBcb60.bpk. You do File/Open project and
browse to OverbyteIcsBcb60.bpk. Select and open it, then Project/Build OverbyteIcsBcb60.
Once the package is compiled, you can install it into your component
palette: Component/Install Packages, click the Add button and select 
Delphi\VC32\OverbyteIcsBcb60.bpl (you just generated this file by compiling the 
package). Click OK button. You must now see FPiette Tab on the component 
gallery.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB6. There is also a project group
OverbyteBcb60Sam.bpg which contains all sample projects.
It is likely that for each project, Bcb complains about a missing .res
file. This is not a problem, Bcb will recreate it as needed. They have not
been included to save space in the zip file.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB1.


NOTES: 
- You may have an error message, using Delphi or BCB, complaining about 
Font.Charset, OldCreateOrder and other properties. Those are new properties 
in newer Delphi or BCB versions. 
You can safely ignore those errors because those properties are not 
used by the components nor sample programs. You may encounter this 
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error 
at design time, you'll have it at runtime !

- If you have Delphi or BCB complaining about a file not found, add VC32
directory to your library path.

- If you are using BCB you may encounter an error at link time 
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in BCB. To solve it, you can edit project option file (right click in 
project manager) and remove any reference to the missing libraries.

- Don't forget that the C++Builder components are located in .\delphi\vc32 
which is object pascal source code (not a problem for C++Builder, just 
indicate that the *.pas files are displayed when installing). C++Builder 
will create the *.hpp files. There are some on-line help files in the VC32
directory.

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> OverbyteIcsDnsQuery         DNS lookup component - useful for getting MX records
> OverbyteIcsEmulVT.pas       ANSI terminal emulation in a control
> OverbyteIcsFingCli.pas      FINGER client protocol - Find informations about user
> OverbyteIcsFtpCli.pas       FTP client protocol - file transfert
> OverbyteIcsFtpSrv.pas       FTP server protocol - file transfert
> OverbyteIcsHttpProt.pas     HTTP client protocol - used by the web
> OverbyteIcsHttpSrv.pas      HTTP server protocol - used to build webservers
> OverbyteIcsIcsLogger.pas    A component to log informations
> OverbyteIcsMimeDec.pas      MIME component - decode file attach, use with POP3
> OverbyteIcsNntpCli.pas      NNTP client protocol - send and receive newsgroups messages
> OverbyteIcsPing.pas         ICMP echo protocol - ping a host
> OverbyteIcsPop3Prot.pas     POP3 client protocol - get mail from mail server
> OverbyteIcsSmtpProt.pas     SMTP client protocol - send mail to server
> OverbyteIcsTnCnx.pas        TELNET client protocol - terminal emulation protocol
> OverbyteIcsTnEmulVT.pas     TELNET and ANSI terminal emulation combined
> OverbyteIcsTnScript.pas     TELNET client protocol - with automation
> OverbyteIcsWait.pas         A kind of progress bar - now obsolete, forget it for new cod
> OverbyteIcsWSocket.pas      Winsock component - TCP, UDP, DNS,...
> OverbyteIcsWSocketE.pas     Register procedure andproperty editor for TWSocket
> OverbyteIcsWSocketS.pas     Winsock component for building servers
> OverbyteIcsWSocketTS.pas    Winsock component for building multithreaded servers

As a rule, the components are the files which have a Register procedure.

- The following list support and utilities units:
> OverbyteIcsIcmp.pas         ICMP protocol support, used by the PING component
> OverbyteIcsIcsCRC.pas       32 bit CRC computation
> OverbyteIcsIcsDES.pas       Implementation of the Data Encryption Standard (DES)
> OverbyteIcsIcsMD4.pas       Implementation of the MD4 Message-Digest Algorithm 
> OverbyteIcsIcsMD5.pas       Implementation of the MD5 Message-Digest Algorithm
> OverbyteIcsIcsSHA1.pas      Implementation of US Secure Hash Algorithm 1 (SHA1)
> OverbyteIcsIcsURL.pas       Support routines for URL handling
> OverbyteIcsMimeUtil.pas     Support routines for MIME standard

Version Control repository:
---------------------------
svn://svn.overbyte.be/ics or http://svn.overbyte.be:8443/svn/ics
(Usercode = ics, password = ics).


Sample applications:
--------------------

Note: All sample file names begins with prefix "OverbyteIcs". Some samples listed
below maybe missing from distribution because the are from version 5 and not yet
ported to V7. If you need those samples, download V5.

BASFTP          Basic FTP client program
CLIDEMO         Example of client for SRVDEMO
CLIENT5         Basic client GUI applications
CONCLI1         Basic client/server console applications
CONCLI2         Basic client/server console applications with thread
CONFTP          Basic console mode FTP client
CONHTTP         Basic console mode HTTP client
CONPING         Basic console mode demo of ping component
CONPOP3         Basic console mode demo for POP3 (mail receive)
CONSMTP         Basic console mode demo for SMTP (mail send)
CONSRV1         Basic server application in console mode
DLLTST1         Test program using ICSDLL
DNSLOOK         Example of name resolution, see also NSLOOKUP sample
DYNCLI          Basic client creatin TWSocket dynamically
DYNCLI          Demo of dynamically created TWSocket components
FINGER          Example of TFingerCli component
FTPASY          Example of asychronous FTP client
FTPSERV         General purpose FTP server
FTPTHRD         Demo of multithreaded FTP client, see also FTPASY
FTPTST          Basic graphical FTP client
HTTPASP         Example of THttpCli component with cookie (POST to an ASP page)
HTTPASY         Example of THttpCli component with multiple async requests (GET)
HTTPCHK         Example of THttpCli to check for valid URL
HTTPDMO         Yet another HTTP client demo
HTTPGET         Example of THttpCli component (GET into a file)
HTTPPG          Example of THttpCli component (POST)
HTTPPOST	Example of THttpCli component (POST), work with WebServ sample
HTTPTHRD        Example of THttpCli component (multi-threaded GET)
HTTPTST	        Example of THttpCli component (GET), show many features
ICSDLL1         Example of TWSocket component within a DLL, use with TcpSrv demo
ICSDLL2         Example of HTTP client component within a DLL
ICSISAPI        Example of FTP client component within an ISAPI extension
MAILHTML        Example of HTML formatted EMail sending, including embedded images
MAILRCV         Internet EMail access using POP3 protocol
MAILSND         Example of EMail sending, including file attach
MAILSNDASYNC    Example of simultaneous EMail sending
MD5FILE         Example of MD5 unit: computer MD5 checksum for files
MD5TEST         Test program for MD5 unit
MIMEDEMO        Example of EMail decoding (attached files are extracted)
MIMETST         Example of EMail sending with attached files
MTSRV           Basic server, multi-threaded, see THRDSRV for better code
NEWSHTML        Example of NNTP component to send HTML messages
NEWSRDR         Example of TNntpCli component (Send/receive newsgroups)
NSLOOKUP        Demo for the DNS query component
PINGTST         Demo for the ping component
POP3MIME        Example of MIME decoding, for example of EMails received with POP3 component
RECV            Simple file receive (server), use with SENDER demo (client)
SENDER          Simple file send (client), use with RECV demo (server)
SERVER5         Basic server GUI applications
SHATEST         Test program for SHA unit
SOCKSTST        How to use TWSocket with SOCKS protocol (firewall traversing)
SRVDEMO         Example of server using a TTable
SRVTCP          Basic server without client forms, event-driven
SVCTCP          Same as SRVTCP but as an NT/2K/XP service
TCPSRV          Basic server without client forms, event-driven
THRDSRV         Basic multithreaded TCP server
TNCLIENT        Telnet client using a TnEmulVT
TNDEMO          Telnet client using a TMemo
TNSRV           Basic TCP server with client forms, event-driven
TWSCHAT         Chat program (both client and server in a single program)
UDPLSTN         UDP listen demo
UDPSEND         UDP send demo
WEBSERV         HTTP server demo. Show static and dynamic pages. Use template for HTML.

Note 1: Many samples are similar. When searching for something, always look at the date
        the demos where created. The most recent is always the best code !
Note 2: Not all samples have been rewritten in C++ for BCB. And those rewritten are
        frequently much simpler. So BCB user: have a look at the Delphi sample too !
Note 3: Follow "UserMade" link on ICS website to find more sample programs written by
        ICS users.
Note 4: See folder SslInternet for SSL-enabled versions of the samples.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I
loaded the form, I was using another Delphi or BCB version which has new properties. 
You can safely ignore messages related to those new properties. They are not used 
in the samples. (The properties are CharSet, OldCreateOrder and others). 
You can also encounter error about duplicate resources. You can ignore them 
safely. If you have those errors, open each form in the IDE, ignore the error 
then recompile. If you don't open the form in the IDE, you'll get the errors 
at runtime and your program will abort.

When installing a new version, always delete old dcu, obj, dcpil and always 
recompile everything ! 
Close everything before recompiling the library or packages.
When installing a new version, be sure to unzip it in the same directory
tree as the old one or you'll mess both versions.

It is possible to use several Delphi or BCB versions at the same time, but 
before switching from one to the other, you MUST delete all DCU, OBJ, ILS, 
ILF, TDS, ILC and ILD files.For BCB, you sometimes need to delete HPP files
as well. They will be recreated when you reinstall ICS components.

About SSL:
----------
TSslWSocket and TSslWSocketServer component are derived from the standard 
TWSocket and TWSocketServer component. The SSL code is compiled into the
component only if you define USE_SSL symbol to your packages and projects.
Just add USE_SSL to the defines in the project or package options and
recompile everything.

The components make use of LIBEAY32.DLL and SSLEAY32.DLL to handle SSL
protocol stuff. The DLLs are dynamically loaded at runtime. It means that
the DLLs will only be required at runtime when you first make use of a SSL
function. Your applications will run on systems without OpenSSL DLLs as long
as you don't call any SSL function.

This version requires OpenSsl version 0.98e! If you need to support 
older OpenSsl versions as well (not recommended) define symbol 
BEFORE_OSSL_098E in OverbyteIcsSslDefs.inc and rebuild all.

Most ICS components have their SSL enabled counter part. They work exactly
the same way as the regular component except when SSL specific stuff is needed,
for example certificates. To support SSL stuff, the SSL-enabled version use
some new properties, events and methods. Many sample programs have their
SSL-enabled counter part in a separate sources located in SslInternet folder.

SSL certificates:
To make use of SSL, you frequently need certificates. I provide some demo
certificates I built using command line OpenSSL tool. PEM certificates can 
be opened by a text editor, LF as well as CRLF are allowed as line breaks.

CACERT.PEM :   A demo certificate for "Example CA" 
01CERT.PEM :   A demo certificate which is signed by CACERT.PEM
01KEY.PEM :    A demo private key for 01CERT.PEM
               Passphrase is "password".
CLIENT.PEM :   A demo certificate and private key.
               Passphrase is "password".
SERVER.PEM :   A demo certificate and private key.
               Passphrase is "password".
ROOT.PEM :     A demo CA certificate.
               Passphrase is "password".               
TRUSTEDCABUNDLE.PEM :
               A demo CA file in PEM format containing multiple
               wellknown root CA certificates to be spezified in
               property CA Path of the demo applications. Read
               the comments included in this file.
6F6359FC.0 :   Located in sub directory SslInternet\TrustedCaStore,
               it's the file CACERT.PEM stored with a hashed file
               name. Directory TrustedCaStore can be spezified in
               property CA Path of the demo applications.                                   

For details about certificate, see the excellent book:
  "Network security with OpenSSL", O'Reilly, ISBN 10: 0-596-00270-X

You will find more informations in IcsSslHowTo.txt file.


Support:
--------
There is a mailing list to discuss F. Piette's components and applications.
To subscribe surf to http://lists.elists.org/mailman/listinfo/twsocket.
Do not use an aliased EMail address, use your real EMail address, the one 
you'll use to post messages. After asking for subscription, you'll receive a
confirmation email you must reply to it or you will _not_ be added to the
subscriber's list (this is to check for email path and also make sure 
someone doesn't subscribe you without your consent).

Once you have been registered with the mailing list processor, you can 
send messages to twsocket@elists.org. Every subscriber will receive a copy of 
your message. I will respond, but anybody is welcome to respond to each 
other's messages. So every body can share his expertise. There are many other
useful mailing lists at http://www.elists.org !

Before asking a question, browse the message archive you can download from
the support page on the website (click the "support" button from main page)
and from the mailing list website http://lists.elists.org/mailman/listinfo/twsocket.
Google is also archiving the list with some delay.

If you found a bug, please make a short program that reproduces the problem 
attach it to a message addressed to me. If I can reproduce the problem, I 
can find a fix ! Do not send exe file but just source code and instructions.
Always use the latest version (beta if any) before reporting any bug.

You are also encouraged to use the support mailing list to ask for 
enhancements. You are welcome to post your own code.

The support mailing list has a heavy traffic: 20 to 40 messages each day. If
it is too much for you, you can select "digest" mode in which mailing list
processor will mail you only one big message per day. To select digest mode
goto http://lists.elists.org/mailman/listinfo/twsocket.
 
You can also subscribe to another mailing list called twsocket-announce which 
will receive only very few messages when major bug fixes or updates are done. 
The subscription process is the same as for the other mailing list. 
See above procedure.


Release notes
-------------

There is no global release notes. Each component and sample has his own history.
You can find those histories in the comment in the beginning of each source file.
There are also a bunch of useful comments in the source code. You should at least
browse the source for the components you are interested in.


MidWare
-------
If you wants to build client/server applications using TCP/IP protocol, you 
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site http://www.overbyte.be.


Special thanks
--------------

to Bob Dolan (bobd@overware.com) who corrected my bad English.
(Ok, I introduced new errors since I updated this file... For those
who care, I normally speak French.)


francois.piette@overbyte.be
francois.piette@swing.be
http://www.overbyte.be
