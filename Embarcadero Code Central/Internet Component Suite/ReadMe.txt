ICS - Internet Component Suite
==============================
(Aka FPIETTE's Components)


Revised: May 05, 2001
http://www.overbyte.be
http://www.rtfm.be/fpiette/indexuk.htm
http://users.swing.be/francois.piette/indexuk.htm

Legal issues: 
-------------
              Copyright (C) 1997-2001 by François PIETTE 
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>
              <francois.piette@swing.be>
              <francois.piette@rtfm.be>
              <francois.piette@pophost.eunet.be> 

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


Register
--------

ICS is freeware. If you use the components, you must register by sending a 
picture postcard showing the area you lives in and some beautiful stamps for 
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, address and EMail.


Installation:
-------------

The zip file has subdirectories in it. You must use the pkunzip -d option
to restore this directory tree or you will have problems because the files 
will not be in their proper subdirectories.

This is the subdirectory layout:

.\                           Info directory
.\delphi\internet            Delphi sample applications (all Delphi versions)
.\cpp\internet               C++Builder sample applications
.\cpp\internet\bcb1          C++Builder version 1 projects
.\cpp\internet\bcb3          C++Builder version 3 projects
.\cpp\internet\bcb4          C++Builder version 4 projects
.\cpp\internet\bcb5          C++Builder version 5 projects
.\delphi\vc                  Delphi 1.x files (winsock.pas 16 bits and *.dcr)
.\delphi\vc32                Delphi (1/2/3/4/5/6) and C++Builder (1/3/4/5) components
.\Delphi1                    Automated build for Delphi 1. Not for beginners.
.\Delphi2                    Automated build for Delphi 2. Not for beginners.
.\Delphi3                    Automated build for Delphi 3. Not for beginners.
.\bcb1                       Automated build for Bcb 1. Not for beginners.

DELPHI 6: Directory VC32 contains IcsDel60.dpk which is a package source for
all components. Using Delphi, do a file/open, select *.dpk and browse to
the VC32 directory. Select IcsDel60.dpk and open it. Then click on the 
Install button. You should see the FPiette tab on the component gallery.
Add VC32 directory path to your library path (Tools menu / Environment Options
/ Library / Library Path. Add VC32 path at the and of the existing path).

Once the package is installed, you may open the sample projects. There is a
project group called Del60Sam.bpg which has all sample programs. Open it
with file/open, browse to the Internet directory, select and open Del60Sam.bpg.
Then Project/Build all projects. You'll get all sample programs compiled.
It is likely that for each project, Delphi complains about a missing .res
file. This is not a problem, Delphi will recreate it as needed.

Note: Package is in the full ICS, in ICS.ZIP file. If you downloaded a
partial ICS such as FTPCLI.ZIP, then you have to add each component by hand 
in the package of your choice. If you don't know how to do that, you'd better
download the full ICS.

DELPHI 5: Same installation as Delphi 6. Files are IcsDel50.dpk and Del50Sam.bpg.

DELPHI 4: Same installation as Delphi 5. Files are IcsDel40.dpk and Del40Sam.bpg.

DELPHI 3: Same installation as Delphi 5. Files are IcsDel30.dpk. Del30Sam.bpg 
doesn't exists because project group was introduced with Delphi 4. Open each
project individually (*.dpr).

DELPHI 2: Directory VC32 contains all components. You must add all those
components into your library. But warning: not all files are components.
Only those with a Register procedure are components. Other are just support
files and must not be installed as components. To install a component file,
do Component/Install, then click on the Add button, then click on the Browse
button and browse to the VC32 directory. Select a component file such as
wsocket.pas and click on OK. Do it again for all component files and
finally click OK button on the Install Component dialog window. Your 
library will be rebuilt. You should see the tab FPiette added to your
component gallery.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.


DELPHI 1: The install procedure is the same as for Delphi 2. But you must
add the directory VC into the library path before VC32 directory which
will be added automatically when you add the first component source.
VC directory in the library path is needed because Delphi 1 doesn't have 
built in winsock support. ICS contains winsock.pas in VC directory to 
add winsock support. VC directory also contains 16 bits component resource 
files (*.dcr) needed by Delphi 1. VC32 contains the 32 bits resource files.
If you use only Delphi 1, you can safely copy all files from VC to VC32
directory, but not the contrary.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

BCB5: follow BCB4 instruction below. Package is IcsBcb50.bpk and samples
project group is Bcb50Sam.bpg. Directory is ...\internet\bcb5.

BCB4: First you need to install all components. There is a package in
Delphi\VC32 directory, called IcsBcb40.bpk. You do File/Open project and
browse to IcsBcb40.bpk. Select and open it, then Project/Build IcsBcb40.
Once the package is compiled, you can install it into your component
palette: Component/Install Packages, click the Add button and select 
Delphi\VC32\IcsBcb40.bpl (you just generated this file by compiling the 
package). Click OK button. You must now see FPiette Tab on the component 
gallery.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB4. There is also a project group
Bcb40Sam.bpg which contains all sample projects.
It is likely that for each project, Bcb complains about a missing .res
file. This is not a problem, Bcb will recreate it as needed.

Note: Packages are in the full ICS, in ICS.ZIP file. If you downloaded a
partial ICS, then you have to add each component by hand in the package
of your choice.

BCB 3: First you need to install all components. There is a package in
Delphi\VC32 directory, called IcsBcb30.bpk. You do File/Open project and
browse to IcsBcb30.bpk. Select and open it, then Project/Build IcsBcb30.
Once the package is compiled, you can install it into your component
palette: Packages/Install Packages, click the Add button and select 
Delphi\VC32\IcsBcb30.bpl (you just generated this file by compiling the 
package). Click OK button. You must now see FPiette Tab on the component 
gallery.
 
Directory is CPP/INTERNET/BCB3.

BCB1: First you need to install all components that are located in
Delphi/VC32. Components are all Object Pascal sources. You must select
*.pas when in the open dialog box (Menu/Component/Install/Add/Browse).
Components are files with a register procedure. Do not install files
with no register procedure (if you try, you'll get an error message).

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB1.


NOTES: 
- If you use Delphi 1, 2, 3, 4 or BCB 1, 3 or 5, you may have an error 
message complaining about Font.Charset, OldCreateOrder and other
properties. Those are two new properties in newer Delphi versions. 
You can safely ignore those errors because those properties are not 
used by the components nor sample programs. You may encounter this 
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error 
at design time, you'll have it at runtime !

- If you have Delphi or BCB complaining about a file not found, add VC32
directory to your library path.

- If you are using BCB3, 4 or 5, you may encounter an error at link time 
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in BCB. To solve it, close BCB and open the package or project file with 
notepad or any text editor and remove any reference to the missing libraries. 
Then start BCB again, reopen the project and rebuilt it. With BCB5 you can
edit project option file (right click in project manager).

- Don't forget that the C++Builder components are located in .\delphi\vc32 
which is object pascal source code (not a problem for C++Builder, just 
indicate that the *.pas files are displayed when installing). C++Builder 
will create the *.hpp files. There are some on-line help files in the VC32
directory.

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection.  Note that 
you may not have all of these present:

> WSocket.pas      (Winsock component - TCP, UDP, DNS,...)
> WSocketS.pas     (Winsock component for building servers)
> HttpProt.pas     (HTTP client protocol - used by the web)
> HttpSrv.pas      (HTTP server protocol - used to build webservers)
> FtpCli.pas       (FTP client protocol - file transfert)
> FtpSrv.pas       (FTP server protocol - file transfert)
> Ping.pas         (ICMP echo protocol - ping a host)
> Pop3Prot.pas     (POP3 client protocol - get mail from mail server)
> MimeDec.pas      (MIME component - decode file attach, use with POP3)
> SmtpProt.pas     (SMTP client protocol - send mail to server)
> NntpCli.pas      (NNTP client protocol - send and receive newsgroups messages)
> TnCnx.pas        (TELNET client protocol - terminal emulation protocol)
> TnScript.pas     (TELNET client protocol - with automation)
> EmulVT.pas       (ANSI terminal emulation in a control)
> TnEmulVT.pas     (TELNET and ANSI terminal emulation combined)
> FingCli.pas      (FINGER client protocol - Find informations about user)
> Wait.pas         (A kind of progress bar - now obsolete, forget it for new code)
> DnsQuery         (DNS lookup component - useful for getting MX records)

As a rule, the components are the files which have a Register procedure.


Sample applications:
--------------------

TWSCHAT         Chat program (both client and server in a single program)
SRV5/CLI5       Basic client/server GUI applications
DYNCLI          Basic client creatin TWSocket dynamically
CONCLI          Basic client/server console applications
CONSRV          Basic server application in console mode
TCPSRV          Basic server without client forms, event-driven
SRVTCP          Basic server without client forms, event-driven
SVCTCP          Same as SRVTCP but as an NT/W2K service
TNSRV           Basic server with client forms, event-driven
MTSRV           Basic server, multi-threaded
FTPCLI          Graphical FTP client
FTPSERV         General purpose FTP server
TNCLIENT        Telnet client using a TnEmulVT
TNDEMO          Telnet client using a TMemo
UDPDEMO         UDP send/receive
MIMEDEMO        Example of EMail decoding (file attach)
MIMETST         Example of EMail sending with attached files
MAILSND         Example of EMail sending, including file attach
MAILRCV         Internet EMail access using POP3 protocol
HTTPTST	        Example of THttpCli component (GET)
HTTPGET         Example of THttpCli component (GET into a file)
HTTPPG          Example of THttpCli component (POST)
HTTPASP         Example of THttpCli component with cookie (POST)
HTTPASY         Example of THttpCli component with multiple requests (GET)
HTTPTHRD        Example of THttpCli component (multi-threaded GET)
FINGER          Example of TFingerCli component
NEWSRDR         Example of TNntpCli component (Send/receive newsgroups)
SRVDEMO         Example of server using a TTable
CLIDEMO         Example of client for SRVDEMO
WEBSERV         A basic webserver
ICSDLL          Example of ICS component within a DLL
ICSISAPI        Example of ICS component within an ISAPI

Note: Follow "usermade" link on ICS website to find more sample programs.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I
loaded the form, I was using another Delphi version which has new properties. 
You can safely ignore those properties. They are not used in the samples.
(The properties are CharSet, OldCreateOrder and others). You can also encounter
error about duplicate resources. You can ignore them safely. If you have
those errors, open each form in the IDE, ignore the error then recompile. 
If you don't open the form in the IDE, you'll get the errors at runtime 
and your program will abort.

If you use C++Builder 1, you may encounter problems because Borland has
two winsock include files: one .h and one .hpp. TWSocket needs the 
winsock.hpp file and it is automatically included. However, sometimes
the winsock.h file is included indirectly and you will get numerous error 
messages from the compiler. Just add a #define _WINSOCKAPI_ before 
your existing #include directives. This will prevent winsock.h from being
included. This is no a bug, it is a feature :-) Another annoying feature
is the SetPortA syndrome. Add a #define _WINSPOOL_ before any #include
directive to work around. If you have some file not found error at
compile time, be sure to verify the project options to change any directory
following your own configuration (include files and library path).
Do NOT use those #include if you use C++Builder 3: Borland has corrected
their mistake.

When installing a new version, always recompile everything ! 
Close everything before recompiling the library or packages.
When installing a new version, be sure to unzip it in the same directory
tree as the old one.

The provided delphi\vc\winsock.pas is for use with Delphi 1 only.
Use Borland's file with Delphi 2, 3, 4, 5 and C++Builder.

C++Builder 1/3/4/5 projects are not compatibles. BCB can provide conversions.
I made separate projects in CPP\INTERNET\BCB1, CPP\INTERNET\BCB3,
CPP\INTERNET\BCB4 and CPP\INTERNET\BCB5 directories. If you install this 
ICS version above a very old one, be sure to delete every *.mak and 
corresponding *.cpp files form the internet directory (not all *.cpp files,
only those which has a corresponding *.mak file !).


Support:
--------
There is a mailing list to discuss F. Piette's components and applications.
To subscribe surf to http://elists.org/mailman/listinfo/twsocket.
Do not use an aliased EMail address, use your real EMail address, the one 
you'll use to post messages.

Once you have been registered with the mailing list processor, you can 
send messages to twsocket@elists.org. Every subscriber will receive a copy of 
your message. I will respond, but anybody is welcome to respond to each 
other's messages. So every body can share his expertize. There are many other
useful mailing lists at http://www.elists.org !

Before asking a question, browse the message digest you can download from
the support page on the website (click the "support" button from main page).

If you found a bug, please make a short program which reproduce the problem 
attach it to a message addressed to me. If I can reproduce the problem, I 
can find a fix ! Do not send exe file but just source code and instructions.

You are also encouraged to use the support mailing list to ask for 
enhancements. You are welcome to post your own code.

The support mailing list has an heavy traffic: 20 to 40 messages each day. If
it is too much for you, you can subscribe to another mailing list called
twsocket-announce which will receive only very few messages when major bug
fixes or updates are done. The subscription process is the same as for the 
other mailing list. See above procedure.


MidWare
-------
If you wants to build client/server applications using TCP/IP protocol, you 
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site.


Known problems:
--------------

Old Delphi and C++Builder have limitation on path length. If you installed 
many components in many differents subdirectories, you'll get strange errors. 
Just make the path shorter and use less different directories. You cannot have
a path greater than 255 characters (this problem is solved starting from 
Delphi 3.02 or later).

Using Delphi 3 Pro, there could a problem after inserting all the component
into your user package. Delphi 3 gives an access violation error in DCC.DLL.
This does not occur if you use Delphi C/S (That's what I do). Ben Ark
(BenArk@email.msn.com) has found the following procedure to overcome 
this [Delphi] bug:

---- copy of a message posted in twsocket@rtfm.be mailing list ----

Just thought I'd correct that previous message with some more detailed
steps...

1)	Remove the EmulVT and TnEmulVT units from your User Package.
2)	Create a new project/application.
3)	Put all of these new files in a directory seperate from your components.
4)	MOVE, do not copy, your Emulvt.pas source file into this new directory.
5)	You need to add Emulvt.pas to the new project.
6)	Build the project.
7)	This should generate an Emultvt.dcu in the directory where the project
        is.
8)	Copy ONLY the DCU file into your component directory
9)	Add EmulVT.dcu to your User Package.
10)	Make sure the EmulVT.pas file IS NOT on your search path.
11)	Now you can safely readd TnEmulVT.pas to your User Package and build
        it.

If you have any problems with these steps, let me know.

-Ben

---- End of message ----

Thank you Ben !

Eric Daniels <sparky@netgsi.com> also found the same problem with Delphi 3
standard. Here what's he tell me in a message:

---- copy of a message posted in twsocket@rtfm.be mailing list ----

This error does happen under Delphi 3 Standard which I use...  I traced it
down and found out to trigger this error all one needs to do is goto:
package options / compiler  / debugging section  and look for local symbols
if this is not checked I will get this error.  After playing around with my
compiler options I unchecked local symbols and rebuilt the package and
Access Violation in dcc.dll read of address occurred...  If anyone has this
happen while using the ics components that is how I managed to fix it.  Also
I got error about unable to find *.dcr files if you edit your package source
and specify the full path to each dcr file you will find all goes well.

---- End of message ----


Special thanks
--------------

to Bob Dolan (bobd@overware.com) who corrected my bad english.
(Ok, I introduced new errors since I updated this file... For those
who cares, I normally speak french.)


francois.piette@overbyte.be
francois.piette@rtfm.be
francois.piette@swing.be
http://www.overbyte.be
http://www.rtfm.be/fpiette/indexuk.htm
http://users.swing.be/francois.piette/indexuk.htm
