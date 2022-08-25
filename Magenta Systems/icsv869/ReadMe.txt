ICS - Internet Component Suite - V8 - Delphi 7 to RAD Studio 11.0
=================================================================
(Aka FPIETTE's Components)


Revised: May 26, 2022
Release: V8.69
http://www.overbyte.be/
http://wiki.overbyte.be/
http://www.overbyte.eu/
http://wiki.overbyte.eu/
https://svn.overbyte.be/svn/ics/


Table of content:
-----------------

- Legal issues
- Donate
- Register
- Contributions
- Latest Versions
- Version Control repository
- Installation
- Available VCL Components
- Sample applications
- About SSL
- Support
- Release notes
- Midware
- Known problems
- Special thanks


Legal issues:
-------------
              Copyright (C) 1997-2022 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany

              ICS is freeware.

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
Don't forget to mention your name, street address, EMail and web site.


Contributions:
--------------

ICS has been designed by François PIETTE but many other peoples are working on the
components and sample programs. The history of changes in each source file list
all developers having contributed (When no name is given, the change is by F. Piette).
I can't list all contributors here but I want to specially thanks two specially active
contributors:
    - Arno Garrels
    - Angus Robertson <angus@magsys.co.uk>


Support:
--------

A new web support forum was created for ICS in February 2019:

https://en.delphipraxis.net/forum/37-ics-internet-component-suite/

Once registered, it is possible to follow a forum with email messages for new
posts, or a daily summary like the old mailing list.

The old twsocket mailing list ceased accepting new messages in late 2018, but
20 years of archived messages are still available at:

http://lists.elists.org/pipermail/twsocket/


Latest versions:
---------------

The latest versions of ICS can be downloaded from the ICS Wiki web site:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download

ICS V5, V6 and V7 are archive releases no longer updated or supported.

ICS V8 is the current development release which is held in a public Version Control
repository that is zipped each night for easy download.  The download page above
also includes the OpenSSL binaries needed to support SSL. ICS V8 supports Delphi
64-bit and Mac OS-X projects.  Note that C++ Builder versions supported are up to
XE3, 10.2 Tokyo and later.  There are currently no C++ packages for XE4 to 10.1.
but older or newer ones will often work.

The latest version is V8.67  which will be reported by the CopyRight constant in
OverbyteIcsWSocket.pas and the integer WSocketVersion as 867.

ICS V9 is in early development and is planned to support Android and Linux Server. There
are no current plans for ICS for iOS.


Version Control repository:
---------------------------

svn://svn.overbyte.be/ics/trunk or https://svn.overbyte.be/svn/ics/trunk
(Usercode = ics, password = ics)


Platforms and Targets
---------------------

The latest version of ICS supports Windows 32-bit and 64-bit targets using VCL and FMX
components, and Apple MacOS 32-bit and 64-bit platforms with FMX (Delphi permitting).

Testing of ICS is primarily with Windows 11 and Server 2019 and 2022, also Windows 10 and
Server 2012, note that Windows XP is not supported and SSL will not work. Windows 7, 8 and
Server 2008 R2 are still be supported but testing is minimal.


Installation:
-------------

ICS V8 has been designed for Embarcadero Delphi 2009 and up, and C++ Builder
2009 and up, but is fully compatible with Borland Delphi 7 and CodeGear 2006 and
2007. Embarcadero RAD Studio includes Delphi and C++ Builder.

https://www.embarcadero.com/

With Delphi XE2 and later, VCL 64-bit Windows targets are supported for Delphi only.
Currently FireMonkey is partly supported for Delphi only (there are still a few
non-ported components). ICS for Mac OSX is currently experimental.

The zip file has sub-directories in it. You must use the WinZip "Use folder names"
option to restore this directory tree or you will have problems because the files
would not be in their proper subdirectories.

Please note most of these directories are differently named to ICS V7 and earlier,
to ease support of multiple versions of Delphi and platforms, and to ease location
of similar sample projects.  Please don't install V8 over an existing V7
installation, it will be a mess of old and new.

This is the new V8 sub-directory layout:

.\                                    Info directory
.\Install                             Component packages project groups for all versions
.\Packages        (was Delphi\Vc32)   Delphi (7 and up) and C++Builder (2006 and up) packages projects
.\Source          (was Delphi\Vc32)   ICS Delphi source code built into packages
.\Source\Include  (was Delphi\Vc32)   .inc files (including OverbyteIcsDefs.inc)
.\Source\Extras   (was Delphi\Vc32)   Extra source code not built into packages
.\Source\zobj125   (was Delphi\Vc32)   ZLIB C OBJ include files

.\Lib                                 Unit output directories for all package builds, subdirectories
    |                                 for 2007+ will be created on building the packages
  \$(Config)                          Release / Debug
      |
    \$(Platform)                      Win32 / Win64 / OSX32 / OSX64
        |
      \<delphi_version>               D7..XE8, 10 Seattle includes .dcu and .dfm files for Delphi
                                      and .obj and .hpp files for C++ Builder

.\Samples                             Delphi Win32/Win64 common source for all demos
.\Samples\delphi\BroswerDemo          Delphi Win32/Win64 Web Browser sample application (all Delphi versions)
.\Samples\delphi\BroswerDemo\Resources Resource file, web pages and movie linked into browser demo
.\Samples\delphi\FtpDemos             Delphi Win32/Win64 FTP sample applications (all Delphi versions)
.\Samples\delphi\MailNewsDemos        Delphi Win32/Win64 SMTP, POP3, NNTP sample applications (all Delphi versions)
.\Samples\delphi\MiscDemos            Delphi Win32/Win64 Miscellaneous applications (all Delphi versions)
.\Samples\delphi\OtherDemos           Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications (all Delphi versions)
.\Samples\delphi\PlatformDemos        Delphi FireMonkey and cross-platform samples (Delphi XE2+)
.\Samples\delphi\SocketDemos          Delphi Win32/Win64 Socket sample applications (all Delphi versions)
.\Samples\delphi\sslinternet          Delphi Win32/Win64 SSL-enabled sample applications (all Delphi versions)
.\Samples\delphi\WebDemos             Delphi Win32/Win64 HTTP sample applications (all Delphi versions)
.\Samples\delphi\WebDemos\WebAppServerData  Directory for WebAppServ demo data files
.\Samples\delphi\WebDemos\WebServData Directory for WebServ demo data files
.\Samples\cpp\internet                C++Builder sample applications
.\Samples\cpp\internet\cb2006         C++Builder 2006 projects
.\Samples\cpp\internet\cb2007         C++Builder 2007 projects
.\Samples\cpp\internet\cb2009         C++Builder 2009 projects
.\Samples\cpp\internet\cb2010         C++Builder 2010 projects
.\Samples\cpp\internet\cbXE           C++Builder XE projects
.\Samples\cpp\internet\cbXE2          C++Builder XE2 projects


UPGRADING and REINSTALLING

Uninstall an existing ICS package (Menu | Component | Install Packages, select
the component package and click Remove).

Rename the old ICS directory and unzip to a new or empty directory, remove the
old path from the library path and add either the new .\Source directory to the library
path under Tools | Options |... or the appropriate .\Lib subdirectory according to
version, ie .\Lib\Debug\Win32\D2007 for Delphi 2007.

The latter has the advantage that the ICS source code won't be recompiled whenever
your project is build. Also under Tools | Options |... add the new .\Source directory
to the Browsing path.


All DELPHI and C++ BUILDER VERSIONS/WIN32

Always upgrade your compiler with the latest update available from Embarcadero.
Always update your system with http://windowsupdate.microsoft.com


SSL or not SSL?

By default the SSL code is compiled into the run-time package and additional SSL-
enabled components are installed. In order to not compile the SSL code into the
run-time package and to not install the SSL-Enabled components you need to remove
the conditional define USE_SSL from both the run-time and design-time package.

However if you do not build your applications with run-time packages it is
recommended to build the packages with default settings. The SSL code will the
be compiled into your applications depending on whether the conditional define
USE_SSL is set in the project options or not (this requires having the .\Source
directory in either in the library path or in projects Search path).

Note the use of USE_SSL is historical from when SSL was a chargeable extra and
future versions of ICS may always build with SSL to reduce the heavy support
burden of building with and without SSL.  Most modern applications need SSL.
All new ICS components in the last few years have been SSL only.

Actual use of SSL in your applications also requires the OpenSSL files
libcrypto-3.dll (or libcrypto-3-x64.dll) and libssl-3.dll (or libssl-3-x64).dll
being available somewhere in the path.  The ICS distribution includes the latest Win32
OpenSSL 3.0 files in the .\OpenSSL-Win32 directory and the six main DLLs duplicated
in .\Samples\delphi\sslinternet for Win32 and Win64 samples. Samples also includes
the old OpenSSL 1.1.1 DLL files.

Note OpenSSL 1.1.1 and later only support Windows Vista and later, and Windows Server
2008 and later, not Windows XP.

Other OpenSSL files, including older and Win64, may be downloaded from:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download

Note that OpenSSL support for 1.0.2 and 1.1.0 ceased in 2019 with no more security
fixes, and ICS V8.66 and later no longer support them.

ICS will try and load OpenSSL 3.0 first, then 1.1.1 if not found, unless the global
variable GSSLEAY_DLL_IgnoreNew is set true before OpenSSL is loaded. Likewise
GSSLEAY_DLL_IgnoreOld may be set true to ignore 1.1.1 and fail unless 3.0 is available.
Setting both GSSLEAY_DLL_IgnoreNew and GSSLEAY_DLL_IgnoreOld to true is ignored.

ICS also support YuOpenSSL which provides OpenSSL in a pre-built DCU statically
linked into applications, rather than using external OpenSSL DLLs. This make
application distribution more reliable since it can no fail by users deleting the
DLLs or copying incompatible versions into the directory.  YuOpenSSL is a commercial
product from https://www.yunqa.de/ and is supplied as separate compiled DCUs for
Delphi 5 to 10.4. DEFINE YuOpenSSL in Include\OverbyteIcsDefs.inc determines whether
the DCU is linked or the external DLLs.  Note only one version of OpenSSL can be
linked with YuOpenSSL, whereas different DLLs can be supported.  Apart from setting
the define and adding a path to YuOpenSSL.dcu, no other application code changes
are needed unless you check or report the DLL version directly.


INSTALLATION USING THE INSTALL PROJECT GROUPS

For each Delphi and C++ Builder version one project group is provided in directory
.\Install:

Delphi 7         :  D7Install.bpg
Delphi 2006      :  D2006Install.bdsgroup
Delphi 2007      :  D2007Install.groupproj
Delphi 2009      :  D2009Install.groupproj
Delphi 2010      :  D2010Install.groupproj
Delphi XE        :  DXeInstall.groupproj
Delphi XE2       :  DXe2Install.groupproj // VCL only, no FireMonkey components
Delphi XE2       :  DXe2InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE3       :  DXe3Install.groupproj // VCL only, no FireMonkey components
Delphi XE3       :  DXe3InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE4       :  DXe4Install.groupproj // VCL only, no FireMonkey components
Delphi XE4       :  DXe4InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE5       :  DXe5Install.groupproj // VCL only, no FireMonkey components
Delphi XE5       :  DXe5InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE6       :  DXe6Install.groupproj // VCL only, no FireMonkey components
Delphi XE6       :  DXe6InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE7       :  DXe7Install.groupproj // VCL only, no FireMonkey components
Delphi XE7       :  DXe7InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE8       :  DXe8Install.groupproj // VCL only, no FireMonkey components
Delphi XE8       :  DXe8InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10 Seattle  : D10SInstall.groupproj // VCL only, no FireMonkey components
Delphi 10 Seattle  : D10SInstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.1 Berlin : D101Install.groupproj // VCL only, no FireMonkey components
Delphi 10.1 Berlin : D101InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.2 Tokyo  : D102Install.groupproj // VCL only, no FireMonkey components
Delphi 10.2 Tokyo  : D102InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.3 Rio    : D103Install.groupproj // VCL only, no FireMonkey components
Delphi 10.3 Rio    : D103InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.4 Sydney : D104Install.groupproj // VCL only, no FireMonkey components
Delphi 10.4 Sydney : D104InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 11,0      :  D110Install.groupproj // VCL only, no FireMonkey components
Delphi 11.0      :  D110InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 2006 :  CB2006Install.bdsgroup
C++ Builder 2007 :  CB2007Install.groupproj
C++ Builder 2009 :  CB2009Install.groupproj
C++ Builder 2010 :  CB2010Install.groupproj
C++ Builder XE   :  CBXeInstall.groupproj
C++ Builder XE2  :  CBXe2Install.groupproj // VCL only no FireMonkey components
C++ Builder XE2  :  CBXe2InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder XE3  :  CBXe3InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 10.2 Tokyo  : CB102InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 10.3 Rio    : CB103InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 10.4 Sydney : CB104InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 11.0 :  CB110InstallVclFmx.groupproj // Both VCL and FireMonkey components

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

4 - One package is installed, called 'Overbyte ICS Design-Time Package for Delphi xxx'.

5 - Various directories under .\Samples\delphi\ include samples that illustrate use
of all the ICS components, see later.

6 - Alternatively, recent Delphi versions have a Build Groups pane in the Project
Manager, opened by clicking a a button.  This allows building multiple packages with
multiple configurations and platforms with a single click. After building, the
32-bit design package still need to be installed manually.

7 - Note that some SSL projects may need these (or similar) files adding:

"C:\Program Files (x86)\CodeGear\RAD Studio\6.0\lib\psdk\crypt32.lib"
"C:\Program Files (x86)\CodeGear\RAD Studio\6.0\lib\psdk\cryptui.lib"

Also define SECURITY_WIN32 in the project which should allow OverbyteIcsSspi.hpp
to build okay.



FIREMONKEY CROSS PLATFORM PACKAGES:

1 - For XE2 and later, DXe?Install (where ? is the version) installs VCL components
only, while DXe?InstallVclFmx also installs FireMonkey cross platform components
(three run time packages). In order to use this feature first uninstall the old
design-time package.

2 = Build all three run-time packages for all available platforms (32-bit
and 64-bit Windows and Mac OS X) in the order they are listed in project manager.

3 - Next build and install the three design-time packages in the order they are
listed in project manager.

4 - Three packages are installed, called:
    'Overbyte ICS Common Design-Time Package for Delphi xxx'
    'Overbyte ICS FMX Design-Time Package for Delphi xxx'
    'Overbyte ICS VCL Design-Time Package for Delphi xxx'

Note that the new packaging is still beta/alpha, both package names and included
units might change in a future beta drop. The old VCL packages are still there
however they do no longer support FireMonkey and of course only one set of
packages can be installed in the IDE at the same time, if you want both VCL
and FMX install DXe2InstallVclFmx.groupproj only. Currently the XE2 package
cache is buggy and should be disabled by adding the -nocache parameter.

5 - Alternatively, recent Delphi versions have a Build Groups pane in the Project
Manager, opened by clicking a a button.  This allows building multiple packages with
multiple configurations and platforms with a single click.  After building, the
32-bit design packages still need to be installed one by one.

6 - The .\Samples\delphi\PlatformDemos\ folder contains FireMonkey sample
projects that may all be built with FireMonkey for Mac OS X (and Windows).


ALTERNATE INSTALLATION USING THE PACKAGE PROJECT FILES:

For each Delphi and C++ Builder version two package project files exist in the
.\Packages directory. One run-time and one design-time package project file.
The run-time file name contains the "Run" suffix. The design-time file name
contains the "Design" suffix.

PACKAGE PROJECT FILE NAMES - VCL:
Delphi 7         :  OverbyteIcsD7Run.dpk, OverbyteIcsD7Design.dpk
Delphi 2006      :  OverbyteIcsD2006Run.bdsproj, OverbyteIcsD2006Design.bdsproj
Delphi 2007      :  OverbyteIcsD2007Run.dproj, OverbyteIcsD2007Design.dproj
Delphi 2009      :  OverbyteIcsD2009Run.dproj, OverbyteIcsD2009Design.dproj
Delphi 2010      :  OverbyteIcsD2010Run.dproj, OverbyteIcsD2010Design.dproj
Delphi XE        :  OverbyteIcsDXeRun.dproj, OverbyteIcsDXeDesign.dproj
Delphi XE2       :  OverbyteIcsDXe2Run.dproj, OverbyteIcsDXe2Design.dproj
Delphi XE3       :  OverbyteIcsDXe3Run.dproj, OverbyteIcsDXe3Design.dproj
Delphi XE4       :  OverbyteIcsDXe4Run.dproj, OverbyteIcsDXe4Design.dproj
Delphi XE5       :  OverbyteIcsDXe5Run.dproj, OverbyteIcsDXe5Design.dproj
Delphi XE6       :  OverbyteIcsDXe6Run.dproj, OverbyteIcsDXe6Design.dproj
Delphi XE7       :  OverbyteIcsDXe7Run.dproj, OverbyteIcsDXe7Design.dproj
Delphi XE8       :  OverbyteIcsDXe8Run.dproj, OverbyteIcsDXe8Design.dproj
Delphi 10 Seattle  : OverbyteIcsD10SRun.dproj, OverbyteIcsD10SDesign.dproj
Delphi 10.1 Berlin : OverbyteIcsD101Run.dproj, OverbyteIcsD101Design.dproj
Delphi 10.2 Tokyo  : OverbyteIcsD102Run.dproj, OverbyteIcsD102Design.dproj
Delphi 10.3 Rio    : OverbyteIcsD103Run.dproj, OverbyteIcsD103Design.dproj
Delphi 10.4 Sydney : OverbyteIcsD104Run.dproj, OverbyteIcsD104Design.dproj
Delphi 11.0        : OverbyteIcsD110Run.dproj, OverbyteIcsD110Design.dproj
C++ Builder 2006 :  OverbyteIcsCB2006Run.bdsproj, OverbyteIcsCB2006Design.bdsproj
C++ Builder 2007 :  OverbyteIcsCB2007Run.cbproj, OverbyteIcsCB2007Design.cbproj
C++ Builder 2009 :  OverbyteIcsCB2009Run.cbproj, OverbyteIcsCB2009Design.cbproj
C++ Builder 2010 :  OverbyteIcsCB2010Run.cbproj, OverbyteIcsCB2010Design.cbproj
C++ Builder XE   :  OverbyteIcsCBXeRun.cbproj, OverbyteIcsCBXeDesign.cbproj
C++ Builder XE2  :  OverbyteIcsCBXe2Run.cbproj, OverbyteIcsCBXe2Design.cbproj
C++ Builder XE3  :  OverbyteIcsCBXe3Run.cbproj, OverbyteIcsCBXe3Design.cbproj

PACKAGE PROJECT FILE NAMES - FireMonkey and VCL:
Delphi XE2 FMX/VCL      :  IcsCommonDXe2Run.dproj, IcsCommonDXe2Design.dproj
Delphi XE2 VCL          :  IcsVclDXe2Run.dproj, IcsVclDXe2Design.dproj
Delphi XE2 FMX          :  IcsFmxDXe2Run.dproj, IcsFmxDXe2Design.dproj
Delphi XE3 FMX/VCL      :  IcsCommonDXe3Run.dproj, IcsCommonDXe3Design.dproj
Delphi XE3 VCL          :  IcsVclDXe3Run.dproj, IcsVclDXe3Design.dproj
Delphi XE3 FMX          :  IcsFmxDXe3Run.dproj, IcsFmxDXe3Design.dproj
Delphi XE4 FMX/VCL      :  IcsCommonDXe4Run.dproj, IcsCommonDXe4Design.dproj
Delphi XE4 VCL          :  IcsVclDXe4Run.dproj, IcsVclDXe4Design.dproj
Delphi XE4 FMX          :  IcsFmxDXe4Run.dproj, IcsFmxDXe4Design.dproj
Delphi XE5 FMX/VCL      :  IcsCommonDXe5Run.dproj, IcsCommonDXe5Design.dproj
Delphi XE5 VCL          :  IcsVclDXe5Run.dproj, IcsVclDXe5Design.dproj
Delphi XE5 FMX          :  IcsFmxDXe5Run.dproj, IcsFmxDXe5Design.dproj
Delphi XE6 FMX/VCL      :  IcsCommonDXe6Run.dproj, IcsCommonDXe6Design.dproj
Delphi XE6 VCL          :  IcsVclDXe6Run.dproj, IcsVclDXe6Design.dproj
Delphi XE6 FMX          :  IcsFmxDXe6Run.dproj, IcsFmxDXe6Design.dproj
Delphi XE7 FMX/VCL      :  IcsCommonDXe7Run.dproj, IcsCommonDXe7Design.dproj
Delphi XE7 VCL          :  IcsVclDXe7Run.dproj, IcsVclDXe7Design.dproj
Delphi XE7 FMX          :  IcsFmxDXe7Run.dproj, IcsFmxDXe7Design.dproj
Delphi XE8 FMX/VCL      :  IcsCommonDXe8Run.dproj, IcsCommonDXe8Design.dproj
Delphi XE8 VCL          :  IcsVclDXe8Run.dproj, IcsVclDXe8Design.dproj
Delphi XE8 FMX          :  IcsFmxDXe8Run.dproj, IcsFmxDXe8Design.dproj
Delphi 10 Seattle FMX/VCL: IcsCommonD10SRun.dproj, IcsCommonD10SDesign.dproj
Delphi 10 Seattle VCL   :  IcsVclD10SRun.dproj, IcsVclD10SDesign.dproj
Delphi 10 Seattle FMX   :  IcsFmxD10SRun.dproj, IcsFmxD10SDesign.dproj
Delphi 10.1 Berlin FMX/VCL: IcsCommonD101Run.dproj, IcsCommonD101Design.dproj
Delphi 10.1 Berlin VCL  :  IcsVclD101Run.dproj, IcsVclD101Design.dproj
Delphi 10.1 Berlin FMX  :  IcsFmxD101Run.dproj, IcsFmxD101Design.dproj
Delphi 10.2 Tokyo FMX/VCL: IcsCommonD102Run.dproj, IcsCommonD102Design.dproj
Delphi 10.2 Tokyo VCL   :  IcsVclD102Run.dproj, IcsVclD102Design.dproj
Delphi 10.2 Tokyo FMX   :  IcsFmxD102Run.dproj, IcsFmxD102Design.dproj
Delphi 10.3 Rio FMX/VCL :  IcsCommonD103Run.dproj, IcsCommonD103Design.dproj
Delphi 10.3 Rio VCL     :  IcsVclD103Run.dproj, IcsVclD103Design.dproj
Delphi 10.3 Rio FMX     :  IcsFmxD103Run.dproj, IcsFmxD103Design.dproj
Delphi 10.4 Sydney FMX/VCL :  IcsCommonD104Run.dproj, IcsCommonD104Design.dproj
Delphi 10.4 Sydney VCL  :  IcsVclD104Run.dproj, IcsVclD104Design.dproj
Delphi 10.4 Sydney FMX  :  IcsFmxD104Run.dproj, IcsFmxD104Design.dproj
Delphi 11.0 FMX/VCL     :  IcsCommonD110Run.dproj, IcsCommonD110Design.dproj
Delphi 11.0 VCL         :  IcsVclD110Run.dproj, IcsVclD110Design.dproj
Delphi 11.0 FMX         :  IcsFmxD110Run.dproj, IcsFmxD110Design.dproj
C++ Builder XE2 FMX/VCL :  IcsCommonCBXe2Run.cbproj, IcsCommonDXe2Design.cbproj
C++ Builder XE2 VCL     :  IcsVclCBXe2Run.cbproj, IcsVclCBXe2Design.cbproj
C++ Builder XE2 FMX     :  IcsFmxCBXe2Run.cbproj, IcsFmxCBXe2Design.cbproj
C++ Builder XE3 FMX/VCL :  IcsCommonCBXe3Run.cbproj, IcsCommonDXe3Design.cbproj
C++ Builder XE3 VCL     :  IcsVclCBXe3Run.cbproj, IcsVclCBXe3Design.cbproj
C++ Builder XE3 FMX     :  IcsFmxCBXe3Run.cbproj, IcsFmxCBXe3Design.cbproj
C++ Builder 10.2 Tokyo VCL : IcsVclCB102Run.cbproj, IcsVclCB102Design.cbproj
C++ Builder 10.2 Tokyo FMX : IcsFmxCB102Run.cbproj, IcsFmxCB102Design.cbproj
C++ Builder 10.3 Rio VCL: IcsVclCB103Run.cbproj, IcsVclCB103Design.cbproj
C++ Builder 10.3 Rio FMX: IcsFmxCB103Run.cbproj, IcsFmxCB103Design.cbproj
C++ Builder 10.4 Sydney VCL: IcsVclCB104Run.cbproj, IcsVclCB104Design.cbproj
C++ Builder 11.0 FMX    : IcsFmxCB110Run.cbproj, IcsFmxCB110Design.cbproj
C++ Builder 11.0 VCL    : IcsVclCB110Run.cbproj, IcsVclCB110Design.cbproj


1 - Open and Build the run-time package project (do not install!).

2 - Open and Install the design-time package project.
(Do a File/Open Project, browse to the .\Packages directory. Select the correct file
and open it. Then in the project manager view, right-click on the package,
then click on either the Build or Install button.)

3 - For Delphi XE2 and later, a 64-bit run-time package can be built by changing
the package target platform to 64-bit Windows. This has the same name as the
32-bit package, so a different package output directory needs to be specified in
Tools / Options / Delphi Options for 64-bit Windows.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".


DELPHI 2006/WIN32, 2007/WIN32, 2009/WIN32, 2010/WIN32, XE/WIN32:

Having installed the package, verify that the appropriate Win32 Library Path
(Tools / Options / Delphi Options / Library - Win32 / Library Path) has been added,
.\Lib subdirectory according to version, ie .\Lib\Debug\Win32\D2007 for Delphi 2007.
If not, add it manually. It is not mandatory to add .\Lib to the global Delphi path,
but it will be much easier for you because otherwise you'll have to add it to each
project.


DELPHI XE2/WIN32, XE3/WIN32, XE4/WIN32, XE5/WIN32, XE6/WIN32, XE7/WIN32, XE8/WIN32,
10 Seattle/WIN32, 10.1 Berlin/WIN32, 10.2 Tokyo/WIN32, 10.3 Rio/WIN32, 10.4 Sydney/WIN32,
11.0/WIN32, XE2/WIN64, XE3/WIN64, XE4/WIN64,XE5/WIN64, XE6/WIN64, XE7/WIN64, XE8/WIN64,
10 Seattle/WIN64, 10.1 Berlin/WIN64, 10.2 Tokyo/WIN64, 10.3 Rio/WIN64, 10.4 Sydney/WIN64,
11.0/WIN64:

Similar to above, but now an extra level, so Tools / Options / Language / Delphi/
Library / select platform as Windows 32-bit or 64-bit, or MacOs 32-bit or 64-bit.
Note the Library path is specified separately for 32-bit and 64-bit Platforms.
Beware Delphi seems to default to 64-bit platform, and needs to be changed to
32-bit plaform before setting the Library path, .\Lib subdirectory according to
version, ie .\Lib\Debug\Win64\D103 for Delphi 10.3 Rio 64-bit,

DELPHI 7: Add VC32 directory path to your library path (Tools menu / Environment
Options / Library / Library Path. Add .\Lib\Debug\Win32\D7 path at the end of the
existing path).


SAMPLE DELPHI PROJECTS

Once the package is installed, you may open the sample projects. There are about 95
samples are split into several directories according to protocols, with a project
group that can be opened in all versions of Delphi.

.\Samples\delphi\AllDemosProject.bpg
.\Samples\delphi\BroswerDemo
.\Samples\delphi\FtpDemos\FtpDemos.bpg
.\Samples\delphi\MailNewsDemos\MailNewsDemos.bpg
.\Samples\delphi\MiscDemos\MiscDemos.bpg
.\Samples\delphi\OtherDemos\OtherDemos.bpg
.\Samples\delphi\PlatformDemos\XSamples.groupproj
.\Samples\delphi\SocketDemos\SocketDemos.bpg
.\Samples\delphi\sslinternet\SslDemos.bpg
.\Samples\delphi\WebDemos\WebDemos.bpg

Full details of the individual sample projects are shown later in this document.

AllDemosProject.bpg contains all 94 samples except BrowserDemo (which needs a third
party component installed).  But building all the samples at the same requires a
lot of memory and was not possible until Delphi 10 Seattle and later which have improved
memory management.  Earlier versions of Delphi will compile each other project group.

You might get some dialog box telling you that resource files are missing (they may not
have been included in the zip file to save space) and are recreated by Delphi. It is OK.
Any other error message is a problem you should fix. After all resource files have
been recreated, you should see in the project manager a group of projects.

To compile all samples in the group at once, do Project / Build all projects. This may
take a few minutes.

Note Delphi has warnings which triggers a lot of messages for 100% OK code. You
can turn those warnings off in the project/ options / Compiler messages
and deselecting: "Deprecated symbol", "Platform symbol", "unsafe type", "unsafe code",
"unsafe typecast". Those are intended for .NET and Linux portability. You can
safely ignore them if you run windows. For you facility, I included a utility
SetProjectOptions (source code, you must compile it) in the internet directory.
This utility will update project options to disable the warnings.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.


C++ BUILDER 2006, 2007, 2009, 2010, XE, XE2, XE3, 10.2 Tokyo, 10.3 Rio,
10.4 Sydney, 11.0:

Follow the installation procedure described for Delphi 2006. Just change
the project group and package name: use CB2006, CBXe, etc, see above.
You can't have Delphi 2006 and CBuilder 2006 packages installed at the
same time in the IDE. So when switching from one to the other, be sure to
remove the one you don't need.

The Embarcadero installation adds this to the system path:

  C:\Users\Public\Documents\Embarcadero\Studio\20.0\Bpl

This however does not allow Win32 packages to be installed, to do that you
need to add this path to the system path (win10=Start, Edit the system
environment variables, Environment variables, System variables, Path,
Edit, New:)

C:\Users\Public\Documents\Embarcadero\Studio\20.0\Bpl\Win32

New projects that need to use ICS:

include path:
- add {THE DIR YOU EXTRACTED ICS To}\source\include\103\win32

library path
- add C:\Users\Public\Documents\Embarcadero\Studio\20.0\BPL\Win32

Building the FireMonkey CBXE2InstallVclFmx C++ packages for OSX may trigger an
ILINK32 error, this is a bug in C++ Builder reported as QC #103668 the Win32
packages should build without errors.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in SAMPLES\CPP\INTERNET\CB2006\ (or CB2006, CBXE, etc)
with a project group in each directory, OverbyteIcsCB2006Sam.bdsgroup,
OverbyteIcsCBXe2Sam.groupproj, etc. It is likely that for each project,
C++ Builder complains about a missing .res file. This is not a problem,
C++ Builder will recreate it as needed. They have not been included to save
space in the zip file.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

NOTES:
- You may have an error message, using Delphi or C++ Builder complaining about
Font.Charset, OldCreateOrder and other properties. Those are new properties
in newer Delphi or C++ Builder versions, newer than the version you use.
You can safely ignore those errors because those properties are not
used by the components nor sample programs. You may encounter this
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error
at design time, you'll have it at runtime !

- If you have Delphi or C++ Builder complaining about a file not found, add
.\source directory to your library path.

- If you are using C++ Builder you may encounter an error at link time
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in C++ Builder. To solve it, you can edit project option file (right click in
project manager) and remove any reference to the missing libraries.

- Don't forget that the C++Builder components are located in .\delphi\vc32
which is object pascal source code (not a problem for C++Builder, just
indicate that the *.pas files are displayed when installing). C++Builder
will create the *.hpp files. There are some on-line help files in the VC32
directory.


Available VCL Components
------------------------

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> OverbyteIcsCharsetComboBox.pas Provides easy MIME charset selection
> OverbyteIcsDnsQuery          DNS lookup component - useful for getting MX records
> OverbyteIcsDprUpdFix.pas     IDE plugin for Delphi 2009 and 2010 to update old projects
> OverbyteIcsEmulVT.pas        ANSI terminal emulation in a control
> OverbyteIcsFileCopy.pas      Indexing, copying and deleting of multiple file directories
> OverbyteIcsFileCopyW.pas     Same as OverbyteIcsFileCopy but Unicode for Delphi 2007.
> OverbyteIcsFingCli.pas       FINGER client protocol - Find information about user
> OverbyteIcsFtpCli.pas        FTP client protocol - file transfer
> OverbyteIcsFtpCliW.pas       Same as OverbyteIcsFtpCli but Unicode for Delphi 2007.
> OverbyteIcsFtpMulti.pas      FTP client that indexes, uploads or downloads multiple files
> OverbyteIcsFtpMultiW.pas     Same as OverbyteIcsFtpMulti but Unicode for Delphi 2007.
> OverbyteIcsFtpSrv.pas        FTP server protocol - file transfer
> OverbyteIcsFtpSrvT.pas       FTP server protocol - helpers
> OverbyteIcsFtpSrvW.pas       Same as OverbyteIcsFtpSrvW but Unicode for Delphi 2007.
> OverbyteIcsHttpAppServer.pas HTTP server protocol - used to build advanced web servers
> OverbyteIcsHttpMulti.pas     HTTP client that downloads multiple files from a list or by parsing web links
> OverbyteIcsHttpMultiW.pas    Same as OverbyteIcsHttpMulti but Unicode for Delphi 2007.
> OverbyteIcsHttpProt.pas      HTTP client protocol - used by the web
> OverbyteIcsHttpSrv.pas       HTTP server protocol - used to build web servers
> OverbyteIcsIpStreamLog.pas   IP stream logging, using TCP Client or Server, UDP Client or Server, sends simple text
> OverbyteIcsLogger.pas        A component to log information
> OverbyteIcsMailQueue.pas     SMTP Mail Queue with extended retries, multiple SMTP servers or MX look up
> OverbyteIcsMimeDec.pas       MIME component - decode file attach, use with POP3
> OverbyteIcsMultiProgressBar.pas A segmented progress bar
> OverbyteIcsMultipartFtpDownloader.pas   FTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsMultipartHttpDownloader.pas  HTTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsNntpCli.pas       NNTP client protocol - send and receive newsgroups messages
> OverbyteIcsPing.pas          ICMP echo protocol - ping a host
> OverbyteIcsPop3Prot.pas      POP3 client protocol - get mail from mail server
> OverbyteIcsProxy.pas         Proxy server protocol - HTTP forward and reverse proxy, and others
> OverbyteIcsReg.pas           Register design components
> OverbyteIcsSmtpProt.pas      SMTP client protocol - send mail to server
> OverbyteIcsSmtpSrv.pas       SMTP server protocol - receive mail from client
> OverbyteIcsSnmpCli.pas       SNMP client protocol - network management
> OverbyteIcsSnmpMsgs.pas      SNMP client protocol - message helper
> OverbyteIcsSntp.pas          Time server and client supporting SNTP time protocol
> OverbyteIcsSslHttpOAuth.pas  HTTPS OAuth2, and cloud components for email and Twitter
> OverbyteIcsSslHttpRest.pas   HTTPS REST functions, descends from THttpCli, Send SMS and DNS over HTTPS
> OverbyteIcsSysLogClient.pas  Syslog Client Protocol - receive syslog messages
> OverbyteIcsSysLogDefs.pas    Syslog Protocol - helpers
> OverbyteIcsSysLogServer.pas  Syslog Server Protocol - send syslog messages
> OverbyteIcsTnCnx.pas         TELNET client protocol - terminal emulation protocol
> OverbyteIcsTnEmulVT.pas      TELNET and ANSI terminal emulation combined
> OverbyteIcsTnOptFrm.pas      TELNET Client configuration form
> OverbyteIcsTnScript.pas      TELNET client protocol - with automation
> OverbyteIcsWebSockets.pas    WebSockets Server protocol
> OverbyteIcsWSocket.pas       Winsock component - TCP, UDP, DNS,...
> OverbyteIcsWSocketE.pas      Register procedure and property editor for TWSocket
> OverbyteIcsWSocketS.pas      Winsock component for building servers
> OverbyteIcsWSocketTS.pas     Winsock component for building multithreaded servers
> OverbyteIcsWhoisCli.pas      Whois protocol client

- The following list support and utilities units:
> OverbyteIcsAsn1Utils.pas     ASN1 utilities (for TSnmpClient component)
> OverbyteIcsAvlTrees.pas      Implements a fast cache-like data storage
> OverbyteIcsBlacklist.pas     Blacklisting of malicious IP addresses, logging functions
> OverbyteIcsCRC.pas           32 bit CRC computation
> OverbyteIcsCharsetUtils.pas  MIME-charset functions
> OverbyteIcsCookies.pas       Client Cookie Handling
> OverbyteIcsCsc.pas           character set routines
> OverbyteIcsDES.pas           Implementation of the Data Encryption Standard (DES)
> OverbyteIcsDigestAuth.pas    HTTP Digest Access Authentication
> OverbyteIcsFormDataDecoder.pas Decode a MIME data block as generated by a HTML form
> OverbyteIcsFtpSrvWT.pas      Same as OverbyteIcsFtpSrvWT but Unicode for Delphi 2007.
> OverbyteIcsHtmlPars.pas      HTML web page parser
> OverbyteIcsHttpCCodZLib.pas  Supports GZIP coding for HttpContCod
> OverbyteIcsHttpContCod.pas   HTTP Content Coding support, uses extra units
> OverbyteIcsIcmp.pas          ICMP protocol support, used by the PING component
> OverbyteIcsIconv.pas         Headers for iconv library (LGPL)
> OverbyteIcsLIBEAY.pas        Delphi encapsulation for libeay32.dll and libcrypto-1_1.dll (OpenSSL)
> OverbyteIcsMD4.pas           Implementation of the MD4 Message-Digest Algorithm
> OverbyteIcsMD5.pas           Implementation of the MD5 Message-Digest Algorithm
> OverbyteIcsMLang.pas         A few header translations from MS mlang.h
> OverbyteIcsMimeUtil.pas      Support routines for MIME standard
> OverbyteIcsNtlmMsgs.pas      Client NTLM authentification messages used within HTTP protocol
> OverbyteIcsNtlmSsp.pas       Server NTLM authentification of user credentials using Windows SSPI
> OverbyteIcsOneTimePw.pas     One Time Password support functions, used by FTP
> OverbyteIcsSHA1.pas          Implementation of US Secure Hash Algorithm 1 (SHA1)
> OverbyteIcsSSLEAY.pas        Delphi encapsulation for ssleay32.dll and libssl-1_1.dll (OpenSSL)
> OverbyteIcsSocketUtils.pas   Cross platform socket utilities for ICS
> OverbyteIcsSslJose.pas       JOSE - Json Object Signing and Encryption
> OverbyteIcsSslSessionCache.pas  A very fast external SSL-session-cache component
> OverbyteIcsSslX509Certs.pas  Automatically download SSL X509 certificates from Let's Encrypt and CertCentre AG
> OverbyteIcsSslX509Utils.pas  SSL key and X509 certification creation
> OverbyteIcsSspi.pas          A few header translations from MS sspi.h and security.h
> OverbyteIcsStreams.pas       Fast streams for ICS
> OverbyteIcsThreadTimer.pas   A custom timer class using custom timer messages from one or more threads
> OverbyteIcsTicks64.pas       GetTickCount64 support for all versions of Windows
> OverbyteIcsTimeList.pas      List of items with expiry times, used for WebSessions
> OverbyteIcsTypes.pas         Common types, mainly for backward compiler compatibility
> OverbyteIcsURL.pas           Support routines for URL handling
> OverbyteIcsUtils.pas         Vast number of common utilities, many supporting Unicode for D7/2007
> OverbyteIcsWSockBuf.pas      FIFO buffers for TWSocket
> OverbyteIcsWebSession.pas    Web session support for THttpAppSrv and MidWare
> OverbyteIcsWinnls.pas        A few header translations for Unicode Normalization in winnls.h
> OverbyteIcsWinsock.pas       Some Winsock initialisations
> OverbyteIcsWMI.pas           WMI support functions, setting IP addresses, controlling DNS server.
> OverbyteIcsWndControl.pas    A class that encapsulates a windows message queue and a message map
> OverbyteIcsZLibDll.pas       Zlib support, interface to external zlib.dll functions
> OverbyteIcsZLibObj.pas       Zlib support, interface to zlib linked C OBJ functions
> OverbyteIcsZlibHigh.pas      Zlib support, high level interface for compression and decompression
> WbemScripting_TLB.pas        WMI API headers.


FireMonkey Cross Platform Support:
----------------------------------

For Delphi and C++ Builder XE2 and later, FireMonkey Desktop applications are an alternate
to VCL Forms applications, supporting cross platforms of Windows 32-bit and 64-bit and Mac
OS X (and perhaps other platforms in future).  FireMonkey uses different visual components
to VCL, while some non-visual components can be used for both VCL and FMX projects, while
other components need special versions, such as ICS.

Earlier betas of V8 used the conditional define "FMX" which is *no longer required in
project options. Instead in your existing ICS FireMonkey app. add either "Ics.Fmx" to
the unit scope names in project options or apply the following changes in the uses clause,
rename:

OverbyteIcsBlacklist               -> Ics.Fmx.OverbyteIcsBlacklist.pas
OverbyteIcsCharsetComboBox         -> Ics.Fmx.OverbyteIcsCharsetComboBox.pas
OverbyteIcsDnsQuery                -> Ics.Fmx.OverbyteIcsDnsQuery.pas
OverbyteIcsFileCopy                -> Ics.Fmx.OverbyteIcsFileCopy.pas
OverbyteIcsFingCli                 -> Ics.Fmx.OverbyteIcsFingCli.pas
OverbyteIcsFtpCli                  -> Ics.Fmx.OverbyteIcsFtpCli
OverbyteIcsFtpMulti                -> Ics.Fmx.OverbyteIcsFtpMulti.pas
OverbyteIcsFtpSrv                  -> Ics.Fmx.OverbyteIcsFtpSrv
OverbyteIcsHttpAppServer           -> Ics.Fmx.OverbyteIcsHttpAppServer.pas
OverbyteIcsHttpMulti               -> Ics.Fmx.OverbyteIcsHttpMulti.pas
OverbyteIcsHttpProt                -> Ics.Fmx.OverbyteIcsHttpProt
OverbyteIcsHttpSrv                 -> Ics.Fmx.OverbyteIcsHttpSrv.pas
OverbyteIcsIcmp                    -> Ics.Fmx.OverbyteIcsIcmp.pas
OverbyteIcsIpStreamLog             -> Ics.Fmx.OverbyteIcsIpStreamLog.pas
OverbyteIcsMailQueue               -> Ics.Fmx.OverbyteIcsMailQueue.pas
OverbyteIcsMsSslUtils              -> Ics.Fmx.OverbyteIcsMsSslUtils.pas
OverbyteIcsMultipartFtpDownloader  -> Ics.Fmx.OverbyteIcsMultipartFtpDownloader.pas
OverbyteIcsMultipartHttpDownloader -> Ics.Fmx.OverbyteIcsMultipartHttpDownloader.pas
OverbyteIcsNntpCli                 -> Ics.Fmx.OverbyteIcsNntpCli.pas
OverbyteIcsPing                    -> Ics.Fmx.OverbyteIcsPing.pas
OverbyteIcsPop3Prot                -> Ics.Fmx.OverbyteIcsPop3Prot.pas
OverbyteIcsProxy                   -> Ics.Fmx.OverbyteIcsProxy.pas
OverbyteIcsSmtpProt                -> Ics.Fmx.OverbyteIcsSmtpProt.pas
OverbyteIcsSntp                    -> Ics.Fmx.OverbyteIcsSntp.pas
OverbyteIcsSocketUtils             -> Ics.Fmx.OverbyteIcsSocketUtils.pas
OverbyteIcsSslHttpRest             -> Ics.Fmx.OverbyteIcsSslHttpRest.pas
OverbyteIcsSslJose                 -> Ics.Fmx.OverbyteIcsSslJose.pas
OverbyteIcsSslSessionCache         -> Ics.Fmx.OverbyteIcsSslSessionCache.pas
OverbyteIcsSslX509Certs            -> Ics.Fmx.OverbyteIcsSslX509Certs.pas
OverbyteIcsSslX509Utils            -> Ics.Fmx.OverbyteIcsSslX509Utils.pas
OverbyteIcsThreadTimer             -> Ics.Fmx.OverbyteIcsThreadTimer.pas
OverbyteIcsWSocket                 -> Ics.Fmx.OverbyteIcsWSocket
OverbyteIcsWSocketS                -> Ics.Fmx.OverbyteIcsWSocketS
OverbyteIcsWhoisCli                -> Ics.Fmx.OverbyteIcsWhoisCli.pas
OverbyteIcsWndControl              -> Ics.Fmx.OverbyteIcsWndControl

{ Demo units }
OverbyteIcsWebAppServerCounter     -> Ics.Fmx.OverbyteIcsWebAppServerCounter
OverbyteIcsWebAppServerMailer      -> Ics.Fmx.OverbyteIcsWebAppServerMailer

The list above is also the list of units that now have different names in the FireMonkey
framework however most of them share the same source file.

Dropping a ICS component on the form will add the correct unit name for each framework
automatically (don't forget to disable the package cache as described above).

Unit OverbyteIcsLibrary.pas has been *deprecated* and ICS IPv8 doesn't use it anymore.
If you used it in your own code read the comment in OverbyteIcsLibrary.pas, search
for "deprecated".


Sample applications:
--------------------

With V8, the 96 sample applications are now grouped into directories according to
general functionality, to make it easier to compare related samples.

Many samples are similar. When searching for something, always look at the date
the demos where created. The most recent is always the best code!  In the lists
below, ACTIVE!! indicates applications that are actively maintained to test and
support new functionality in the ICS components.  These may not be simplest
samples, but are usually the first to try when learning about a component.


Delphi Win32/Win64 FTP sample applications
------------------------------------------
.\Samples\delphi\FtpDemos\FtpDemos.bpg - Project group
> OverbyteIcsBasFtp.dpr         Basic FTP client program
> OverbyteIcsConFtp.dpr         Basic console mode FTP client
> OverbyteIcsFtpAsy.dpr         Example of asynchronous FTP client
> OverbyteIcsFtpMulti.dpr       Demo to do several FTP downloads in parallel to get a list of files
> OverbyteIcsFtpMultipartDownload.dpr Demo to FTP download a single large file in several parts in parallel
> OverbyteIcsFtpServ.dpr        General purpose FTP server, uses TSocketServer - ACTIVE!!
> OverbyteIcsFtpThrd.dpr        Demo of multithreaded FTP client, see also FTPASY
> OverbyteIcsFtpTst.dpr         Basic graphical FTP client - ACTIVE!!
Note better samples under sslinternet with SSL enabled.

Delphi Win32/Win64 SMTP, POP3, NNTP sample applications
-------------------------------------------------------
.\Samples\delphi\MailNewsDemos\MailNewsDemos.bpg - Project group
> OverbyteIcsBasNntp.dpr        Basic NNTP client program
> OverbyteIcsConPop3.dpr        Basic console mode demo for POP3 (mail receive)
> OverbyteIcsConSmtp.dpr        Basic console mode demo for SMTP (mail send)
> OverbyteIcsMailHtml.dpr       Example of HTML formatted EMail sending, including embedded images - ACTIVE!!
> OverbyteIcsMailRcv.dpr        Internet EMail access using POP3 protocol - ACTIVE!!
> OverbyteIcsMailSnd.dpr        Example of EMail sending using SMTP, including file attach - ACTIVE!!
> OverbyteIcsMailSndAsync.dpr   Example of parallel EMail sending with multiple connections
> OverbyteIcsMimeDemo.dpr       Example of EMail decoding (attached files are extracted) - ACTIVE!!
> OverbyteIcsNewsReader.dpr     Example of TNntpCli component (Send/receive newsgroups) - ACTIVE!!
> OverbyteIcsSmtpServer.dpr     Internet EMail server using SMTP protocol - ACTIVE!!
Note better samples under sslinternet with SSL enabled.

Delphi Win32/Win64 Miscellaneous applications
---------------------------------------------
.\Samples\delphi\MiscDemos\MiscDemos.bpg - Project group
> OverbyteIcsBufStrmTst.dpr     Test of buffered stream classes
> OverbyteIcsCacheTest.dpr      Test of TCacheTree class used in TSslAvlSessionCache
> OverbyteIcsMD4Test.dpr        Test program for MD4 unit
> OverbyteIcsMD5File.dpr        Example of MD5 unit: computer MD5 checksum for files
> OverbyteIcsMD5Test.dpr        Test program for MD5 unit
> OverbyteIcsOneTimePassword.dpr One Time Password test routines for OverByteIcsOneTimePw unit
> OverbyteIcsSHA1Test.dpr       Test program for SHA unit
> OverbyteIcsThreadTimerDemo.dpr Demo for TIcsThreadTimer
> OverbyteIcsTicks64Demo.dpr    GetTickCount64 test routines for OverbyteIcsTicks64 unit
> OverbyteIcsTimerDemo.dpr      Very simple demo for TIcsTimer
> OverByteIcsWndControlTest.dpr Test program for windows and threads

Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications
--------------------------------------------------------------
.\Samples\delphi\OtherDemos\OtherDemos.bpg - Project group
.\Samples\delphi\OtherDemos\OtherDemos64.bpg - Project group with Win64 versions of several samples.
> OverbyteIcsBatchDnsLookup.dpr Batch async DNS lookup using DnsLookup (IPv6 and IPv4)
> OverbyteIcsConPing.dpr        Basic console mode demo for ping component
> OverbyteIcsDll1.dpr           Demo showing how to use a TWSocket component in a DLL
> OverbyteIcsDll2.dpr           Demo showing how to use a THttpCli component in a DLL
> OverbyteIcsDllTst.dpr         Test program calling ICSDLL1 and ICSDLL2
> OverbyteIcsDnsLook.dpr        Example of name resolution (IPv6 and IPv4)
> OverbyteIcsDnsResolver.dpr    Batch async DNS lookup event driven using DnsQuery
> OverbyteIcsFinger.dpr         Example of TFingerCli component
> OverbyteIcsNsLookup.dpr       Demo for the DnsQuery component - ACTIVE!!
> OverbyteIcsPingTst.dpr        Demo for the ping component, includes trace route - ACTIVE!!
> OverbyteIcsSnmpCliTst.dpr     Demo for SNMP (simple network management protocol) component
> OverbyteIcsSysLogClientDemo.dpr Demo for SysLog client component
> OverbyteIcsSysLogServerDemo.dpr Demo for SysLog server component
> OverbyteIcsTimeTst.dpr        Test SNTP time protocol as client or server - ACTIVE!!
> OverbyteIcsWhoisCliTst.dpr    Test Whois protocol, looks up servers automatically - ACTIVE!!
> OverbyteIcsWmiTst.dpr         Test WMI functions, general purpose, IP addressses and DNS Server - ACTIVE!!

Delphi FireMonkey cross-platform samples (Delphi XE2 and later)
---------------------------------------------------------------
All these samples may be built for Mac OS X (and Windows).
.\Samples\delphi\PlatformDemos\XSamples.groupproj
> IcsCliDemo.dproj              Example of client for SRVDEMO, IPV4 only
> IcsTcpSrvIPv6.dproj           Basic server without client forms, event-driven, IPv4/IPV6
> IcsConSmtp.dproj              Basic console mode demo for SMTP (mail send)
> IcsMailSnd.dproj              Example of EMail sending using SMTP, including file attach
> IcsMailRcv.dproj              Internet EMail access using POP3 protocol
> IcsHttpsTst.dproj             Example of THttpCli component (GET), show many features
> IcsWebServ.dproj              Demo of HTTP server, uses TSocketServer
> IcsWebAppServ.dproj           Advanced HTTP server demo, uses WebServ, adds sessions
> IcsFtpTst.dproj               Basic graphical FTP client
> IcsFtpServ.dproj              General purpose FTP server, uses TSocketServer
> IcsUdpLstn.dproj              UDP listen demo
> IcsUdpSend.dproj              UDP send demo
> IcsBatchDnsLookup.dproj       Batch async DNS lookup using DnsLookup (IPv6 and IPv4)
> IcsDll1.dproj                 Demo showing how to use a TWSocket component in a DLL
> IcsDll2.dproj                 Demo showing how to use a THttpCli component in a DLL
> IcsDllTst.dproj               Test program calling ICSDLL1 and ICSDLL2
> IcsThreadTimerDemo.dproj      Very simple demo for TIcsTimer

 Delphi Win32/Win64 Socket sample applications
 ---------------------------------------------
 .\Samples\delphi\SocketDemos\SocketDemos.bpg - Project group
> OverbyteIcsBinCliDemo.dpr       Client program to receive binary and delimited text data. Works with OverbyteIcsTcpSrv demo.
> OverbyteIcsCliDemo.dpr          Example of client for SRVDEMO, IPV4 only  - ACTIVE!!
> OverbyteIcsClient5.dpr          Basic client GUI applications
> OverbyteIcsClient7.dpr          Simple client application demonstrating TWSocket
> OverbyteIcsConCli1.dpr          Basic client/server console applications
> OverbyteIcsConCli2.dpr          Basic client/server console applications with thread
> OverbyteIcsConSrv1.dpr          Basic server application in console mode
> OverbyteIcsConUdpLstn.dpr       Console application to listen for UDP messages
> OverbyteIcsDynCli.dpr           Demo of dynamically created TWSocket components
> OverbyteIcsMtSrv.dpr            Basic server, multi-threaded, see THRDSRV for better code
> OverbyteIcsRecv.dpr             Simple file receive (server), use with SENDER demo (client)
> OverbyteIcsSender.dpr           Simple file send (client), use with RECV demo (server)
> OverbyteIcsServer5.dpr          Basic server GUI applications
> OverbyteIcsSocksTst.dpr         How to use TWSocket with SOCKS and HTTP Tunnel Proxy protocol - ACTIVE!!
> OverbyteIcsSrvDemo.dpr          Example of server using a TTable - ACTIVE!!
> OverbyteIcsSrvTcp.dpr           Basic server without client forms, event-driven
> OverbyteIcsSvcTcp.dpr           Same as SRVTCP but as an NT/2K/XP service
> OverbyteIcsTWSChat.dpr          Chat program (both client and server in a single program)
> OverbyteIcsTcpSrv.dpr           Basic server without client forms, event-driven, IPv4 only - ACTIVE!!
> OverbyteIcsTcpSrvIPv6.dpr       Basic server without client forms, event-driven, IPv4/IPV6 - ACTIVE!!
> OverbyteIcsTelnetClient.dpr     Telnet client using a TnEmulVT
> OverbyteIcsThrdSrv.dpr          Basic multithreaded TCP server, banner sent in main thread
> OverbyteIcsThrdSrvV2.dpr        Basic multithreaded TCP server, banner sent in worker thread
> OverbyteIcsThrdSrvV3.dpr        Basic TCP server showing how to use TWSocketThrdServer
> OverbyteIcsTnDemo.dpr           Telnet client using a TMemo
> OverbyteIcsTnSrv.dpr            Basic TCP server with client forms, event-driven
> OverbyteIcsUdpLstn.dpr          UDP listen demo
> OverbyteIcsUdpSend.dpr          UDP send demo
Note better samples under sslinternet with SSL enabled.

Delphi Win32/Win64 SSL-enabled sample applications
--------------------------------------------------
.\Samples\delphi\sslinternet\SslDemos.bpg - Project group
.\Samples\delphi\sslinternet\SslDemos64.bpg - Project group with Win64 versions all these samples.
> OverbyteIcsHttpRestTst.dpr      ICS HTTPS REST and OAuth, Send SMS and DNS over HTTPS functions demo - ACTIVE!!.
> OverbyteIcsHttpsTst.dpr         Example of TSslHttpCli component (GET) - ACTIVE!!
> OverbyteIcsIpStmLogTst.dpr      Test IP stream logging, sending streams as client or server using SSL - ACTIVE!!
> OverbyteIcsJoseTst.dpr          ICS SSL Json Object Signing (Jose) Demos - ACTIVE!!
> OverbyteIcsMailQuTst.dpr        Simple mailing list tool using Mail Queue using SSL - ACTIVE!!
> OverbyteIcsMsVerify.dpr         Verify and show an OpenSSL certificate or certificate chain using
                                     class TMsCertChainEngine which uses MS crypto API - ACTIVE!!
> OverbyteIcsPemTool.dpr          ICS Pem Certificate Tool - Create and import certificates in many formats  - ACTIVE!!
> OverbyteIcsProxySslServer.dpr   ICS Proxy server  - ACTIVE!!
> OverbyteIcsSimpleSslCli.dpr     Example of simple SSL client using TSslWSocket - ACTIVE!!
> OverbyteIcsSimpleSslServer.dpr  Example of SSL server using TSslWSocket - ACTIVE!!
> OverbyteIcsSslFtpServ.dpr       General purpose FTP SSL server, uses TSocketServer - ACTIVE!!
> OverbyteIcsSslFtpTst.dpr        Basic graphical FTP SSL client - ACTIVE!!
> OverbyteIcsSslMailRcv.dpr       Internet EMail access using POP3 protocol and SSL - ACTIVE!!
> OverbyteIcsSslMailSnd.dpr       Example of EMail sending using SMTP and SSL - ACTIVE!!
> OverbyteIcsSslMultiFtpServ.dpr  Advanced multi host FTP server demo  - ACTIVE!!
> OverbyteIcsSslMultiWebServ.dpr  Advanced multi host web server demo  - ACTIVE!!
> OverbyteIcsSslNewsRdr.dpr       Example of TSslNntpCli component (Send/receive newsgroups) - ACTIVE!!
> OverbyteIcsSslSmtpServer.dpr    Internet EMail server using SMTP protocol and SSL - ACTIVE!!
> OverbyteIcsSslSniSrv.dpr        Test of Server Name Indication (SNI) in server mode - ACTIVE!!
> OverbyteIcsSslWebAppServer.dpr  Advanced HTTPS server demo, uses WebServ, adds sessions - ACTIVE!!
> OverbyteIcsSslWebServ.dpr       Demo of HTTPS server, uses TSocketServer - ACTIVE!!
> OverbyteIcsWebSocketSrv.dpr     Demo of WebSockets server, used with websocketclient.html - ACTIVE!!
> websocketclient.html            Web page for WebSockets demo
> OverbyteIcsX509CertsTst.dpr     Automatically download SSL X509 certificates from Let's Encrypt and CertCentre AG - ACTIVE!!
> OverbyteIcsXferTst.dpr          File transfer testing, file copying, FTP up and download, HTTP download, with SSL - ACTIVE!!

Note following sample is not in the project group since it only builds with Delphi 2007.
> OverbyteIcsXferTstW.dpr         Same as OverbyteIcsXferTst but Unicode for Delphi 2007 - ACTIVE!!

Delphi Win32/Win64 Special SSL-enabled sample applications
--------------------------------------------------
.\Samples\delphi\sslinternet\SslSpecials.groupproj - Project group
.\Samples\delphi\BroswerDemo.FrameBrowserIcs.dpr           Web Browser using HtmlViewer component - ACTIVE!!
Note this sample requires the HtmlViewer component installed
.\Samples\delphi\sslinternet\OverbyteIcsDDWebService.dpr     Advanced Windows Service multi host web server demom, maybe run as a GUI   - ACTIVE!!
Note this sample requires DDService framework to be installed

Delphi Win32/Win64 HTTP sample applications (the SSL versions are preferred!)
-------------------------------------------
.\Samples\delphi\WebDemos\WebDemos.bpg - Project group
> OverbyteIcsConHttp.dpr          Basic console mode HTTP client
> OverbyteIcsHttpAsp.dpr          Example of THttpCli component with cookie (POST to an ASP page)
> OverbyteIcsHttpAsy.dpr          Example of THttpCli component with multiple async requests (GET)
> OverbyteIcsHttpChk.dpr          Example of THttpCli to check for valid URL using HEAD request
> OverbyteIcsHttpDmo.dpr          Simple HTTP client demo with proxy
> OverbyteIcsHttpGet.dpr          Example of THttpCli component (GET into a file)
> OverbyteIcsHttpMultipartDownload.dpr  Demo application for TMultipartHttpDownloader to download files using simultaneous connections
> OverbyteIcsHttpPg.dpr           Example of THttpCli component (POST to CGI script)
> OverbyteIcsHttpPost.dpr         Example of THttpCli component (POST), work with WebServ sample - ACTIVE!!
> OverbyteIcsHttpThrd.dpr         Example of THttpCli component (multi-threaded GET)
> OverbyteIcsHttpTst.dpr          Example of THttpCli component (GET), show many features - ACTIVE!!
> OverbyteIcsIsapi.dll            Example of FTP client component within an HTTP server ISAPI extension
> OverbyteIcsWebAppServer.dpr     Advanced HTTP server demo, uses WebServ, adds sessions - ACTIVE!!
> OverbyteIcsWebServ.dpr          Demo of HTTP server, uses TSocketServer - ACTIVE!!
Note better samples under sslinternet with SSL enabled.


Sample Notes
------------
Note 1: Not all samples have been rewritten in C++ for C++ Builder. And those rewritten are
        frequently much simpler. So C++ Builder user: have a look at the Delphi sample too !
Note 2: Follow "UserMade" link on ICS web site to find more sample programs written by
        ICS users, although these are mostly for older versions of ICS.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I loaded the form,
I was using another Delphi or C++ Builder version which has new properties.
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


Getting Started with ICS
------------------------

ICS has a large number of sample application whose primary purpose is to test
all the components and to learn about using those components and how to use
them in your own applications.  There are often Multiple samples for a single
protocol with different purposes, so this section should help get you started
choosing the components and samples for your internet project.

ICS often offers low and high level versions of components, the former allow
your application to send the various commands used by the protocol but you
need to send those commands in the correct order often dependent upon the
result from earlier commands, so you need to understand the protocol, but
have control over the commands.  The high level components are quicker and
easier to implement because they hide most of the protocol and offer complex
methods instead such as download a file, they often include extra
functionality.

Historically, most ICS components are available on non-SSL and SSL versions,
these notes assume you are using SSL/TLS components which are often essential
today.  Note most low level component need SSL/TLS adding using an SslContext
and need SSL certificate chain checking added to applications, while the
higher level components mostly already include the SslContext and chain
checking and hide much of the SSL/TLS complexity making them faster to
implement and easier to maintain as SSL changes.  Note that any ICS
applications using SSL/TLS need to redistribute two OpenSSL DLLs.


World Wide Web, HTTP Client
---------------------------

There are three types of HTTP component, with many extra components used to
extend their capabilities.

TSslHttpCli in unit OverbyteIcsHttpProt is the low level HTTP protocol client
that is tested using OverbyteIcsHttpsTst.dpr. It has buttons for GET and HEAD
commands and allows numerous SSL parameters to be specified. POST requests are
tested with OverbyteIcsHttpPost.dpr and OverbyteIcsHttpPg.dpr. Other units
containing components assisting HTTP include OverbyteIcsHttpCCodZLib,
OverbyteIcsHttpContCod, OverbyteIcsCookies, OverbyteIcsMimeUtils,
OverbyteIcsFormDataDecoder, OverbyteIcsCharsetUtils, OverbyteIcsMsSslUtils,
MIME with OverbyteIcsMimeDemo.dpr, SSL certificate chains with
OverbyteIcsMsVerify.dpr. Note TSslHttpCli requires an SslContext for SSL
configuration.  Note HTTP clients do not need SSL/TLS certificates, but
generally should check the certificate chains received from HTTPS servers
to ensure they are talking to the correct servers.

TSslHttpRest in unit OverbyteIcsSslHttpRest is the high level HTTP protocol
client that has additional methods and properties for making GET, POST, PUT
and HEAD REST (REpresentional State Transfer) client requests, but can
still do everything TSslHttpCli does.  It includes a TRestParams class to
build and encode GET/PUT/POST parameter strings. It also includes SSL
configuration and certificate validation with a root bundle, SSL session
caching, content compression, content code page decoding, persistent
cookies, Json handling, logging and client SSL certificate support.  There
is also TRestOAuth for OAuth1/2 authentication and some REST examples
TDnsQueryHttps, TIcsSms, TIcsTwitter and TIcsRestEmail.  All tested using
OverbyteIcsHttpRestTst.dpr.

TIcsHttpMulti in unit OverbyteIcsHttpMulti is another high level HTTP client
that allows downloading of multiple files from an HTTP server using full URLs,
or listed by parsing links from a web page, using a single function call. It
also includes SSL configuration and certificate validation with a root bundle.
Tested using OverbyteIcsXferTst.dpr.

ICS has a visual web browser sample FrameBrowserIcs.dpr which needs
the HtmlViewer component to be installed, which will view simple web pages
that don't need Javascript, it logs both HTTP and HTML protocol and can be
very useful for debugging.

There are two SSL samples OverbyteIcsHttpsTst.dpr and OverbyteIcsHttpRestTst.dpr
that illustrate HTTP GET and POST requests, authentication including OAuth2,
file uploading and downloading, cookies, certificate chain verification,
content encoding and decoding.

There are some older non-SSL demos for console and DLL and threads, see
OverbyteIcsConHttp.dpr, OverbyteIcsHttpAsp.dpr and OverbyteIcsHttpThrd.dpr.
Another sample OverbyteIcsJoseTst.dpr can be used to test Json Object
Signing (Jose) functions often used for REST requests, URL encoding and
decoding and display of Json and XML data.


World Wide Web, HTTP Server
---------------------------

There are five different HTTP web servers, which are based on
TSslWSocketServer.

TSslHttpServer in unit OverbyteIcsHttpSrv is the main web server, tested
with OverbyteIcsSslWebServ.dpr, while TSslHttpAppSrv in unit
OverbyteIcsHttpAppServer adds session support and page handlers for creating
dynamic page web applications.  These servers only listen on one IP address
and port, but you use multiple components for multiple listeners sharing the
same events.  Note TSslHttpServer and OverbyteIcsHttpAppServer usually
require an SslContext for SSL configuration. The samples are full web
servers with a lot of SSL configuration options for an SSL/TLS certificate,
note HTTPS servers require an SSL certificate and will not start without one.
Both samples include a number of dynamic web pages to illustrate basic
web server facilities, including a contact form that sends email.

There is a third more advanced HTTP sample OverbyteIcsSslMultiWebServ.dpr
which configures TSslHttpAppSrv differently using collections of
IcsHosts properties. This allows the web server to listen on multiple
IP addresses and ports at the same time, and to support multiple hosts
on the same IP address serving different page content (as do most
web servers).  IcsHosts allow different SSL/TLS certificates to be
specified for each host using built-in SslContexts, will automatically
create self signed SSL/TLS certificates so the server can start, and will
them order free SSL/TLS certificates from Let's Encrypt (provided running
on the public internet), and re-order them every three months before they
expire.

OverbyteIcsSslMultiWebServ.dpr is different to most ICS samples
in having a minimal GUI and being entirely configured using an INI file,
it is really designed to be built as a Windows service application to
run unattended in background.  It includes a separate web log for each
host, and will send emails when it starts and stops.  The sample is based
on a commercial web server.

OverbyteIcsDDWebService.dpr is very similar to OverbyteIcsSslMultiWebServ.dpr,
but designed as a Windows service, although it will also run as a GUI for
debugging.  It requires DDService service framework to be installed. It also
includes a REST server with simple lookup responses from a SQL database, which
requires DISQLite3 to be installed.

The fifth web server is TSimpleWebSrv in unit OverbyteIcsSslHttpRest which
is a lightweight server with minimal functionality designed for embedding
in applications needing OAuth2 or SSL/TLS certificate ordering that require
access to web server to check a host exists.  It has a single event that
presents a request and returns a response. It supports SSL with IcsHosts.



File Transfer Protocol, FTP Client
----------------------------------

There are two types of FTP components for file transfers.

TSslFtpClient in unit OverbyteIcsFtpCli is the low level FTP client that is
tested with OverbyteIcsSslFtpTst.dpr.  It has about 50 buttons the test the
various FTP commands in various ways, and allows numerous SSL parameters to
be specified. Note TSslFtpClient requires an SslContext for SSL configuration.
Other FTP samples include OverbyteIcsBasFtp.dpr, OverbyteIcsConFtp.dpr,
OverbyteIcsFtpAsy.dpr and OverbyteIcsFtpMulti.dpr.

TIcsFtpMulti in unit OverbyteIcsFtpMulti is a high level FTP client that indexes,
uploads or downloads single or multiple files automatically, without needing
to understand most FTP commands.  One function indexes files and directories
on an FTP server building a list compatible with the TIcsFileCopy component
that indexes Windows directories, allowing local and remote directories to
be compared and files FTP uploaded or downloaded so they match.  It also
includes SSL configuration and certificate validation with a root bundle,
SSL session caching and logging.


File Transfer Protocol, FTP Server
----------------------------------

The FTP server is based on TSslWSocketServer.

TSslFtpServer in unit OverbyteIcsFtpSrv is the FTP server, tested using
OverbyteIcsSslFtpServ.drp. The FTP server only listens on one IP address
and port, but you use multiple components for multiple listeners sharing the
same events.  Note TSslFtpServer usually requires an SslContext for SSL
configuration. The sample is a full FTP server for file uploads and
downloads, with a lot of SSL configuration options for the SSL/TLS
certificate and will not start without one.

There is a more advanced FTP server sample OverbyteIcsSslMultiFtpServ.dpr
which configures TSslFtpServer differently using collections of
IcsHosts properties. This allows the FTP server to listen on multiple
IP addresses and ports at the same time, and to support multiple hosts
on the same IP address.  IcsHosts allow different SSL/TLS certificates to
be specified for each host using built-in SslContexts, will automatically
create self signed SSL/TLS certificates so the server can start, and will
them order free SSL/TLS certificates from Let's Encrypt (provided running
on the public internet), and re-order them every three months before they
expire.

OverbyteIcsSslMultiFtpServ is different to most ICS samples
in having a minimal GUI and being entirely configured using an INI file,
it is really designed to be built as a Windows service application to run
unattended in background. The sample is based on a commercial FTP server.


Sending Email, SMTP Client
--------------------------

There are three types of components for sending email using the SMTP protocol
or HTTP REST protocol.

TSslSmtpCli in unit OverbyteIcsSmtpProt is the low level SMTP client that
is tested with OverbyteIcsSslMailSnd1.dpr.  It has about 16 buttons to
test various SMTP commands and allow an email to be sent with attachments.
Note TSslSmtpCli requires an SslContext for SSL configuration. Other test
samples include OverbyteIcsConSmtp.dpr, OverbyteIcsMailHtml.dpr and
OverbyteIcsMailSndAsync.dpr.

TIcsMailQueue in unit OverbyteIcsMailQueue is the high level SMTP client,
tested by OverbyteIcsMailQuTst.dpr.  It supports extended retries over many
hours or days, and supports multiple SMTP relay servers or looks up MX
servers, while alleviating the need for the application to handle retries.
It spools emails as EML files, and can send them as well.  It includes SSL
configuration and certificate validation with a root bundle and logging.

TIcsRestEmail in unit OverbyteIcsSslHttpRest is alternative means of
sending email using HTTP REST requests to Google and Microsoft, instead
of using SMTP. Tested using OverbyteIcsHttpRestTst.dpr.  This component
also adds XOAuth2 authentication to the other SMTP components.


Receiving Email, POP3 Client
----------------------------

There are two types of components for receiving email using the POP3 protocol
or HTTP REST protocol.

TSslPop3Cli in unit OverbyteIcsPop3Prot is the low level POP3 client that
is tested with OverbyteIcsSslMailSnd1.dpr.  It has about 22 buttons to
test various POP3 commands and allow emails to be retrieved from a mailbox.
The unit OverbyteIcsMimeDec contains functions for decoding MIME encoded
emails, tested with OverbyteIcsMimeDemo.dpr. AnOther test sample is
OverbyteIcsConPop3.dpr.  Note TSslPop3Cli requires an SslContext for SSL
configuration.

TIcsRestEmail in unit OverbyteIcsSslHttpRest is alternative means of
receiving email using HTTP REST requests to Google and Microsoft, instead
of using POP3. Tested using OverbyteIcsHttpRestTst.dpr.  This component
also adds XOAuth2 authentication to the POP3 component.


Forwarding Email, SMTP Server
-----------------------------

TSslSmtpServer in unit OverbyteIcsSmtpSrv is an SMTP server that accepts
emails from a client, making some checks and adding headers, which is
tested by OverbyteIcsSslSmtpServ.drp which writes emails to an EML spool
file.  Note neither component or sample support POP3 access, nor do they
do anything with the EML file.  The TIcsMailQueue component could be
used to forward EML files.  Note TSslSmtpServer requires an SslContext
for SSL configuration and SSL/TLS certificate, it does not yet support
IcsHosts.


Simple TCP Socket Client
------------------------

TSslWSocket in unit OverbyteIcsWSocket is the root of most other ICS
components opening a socket to either connect to a remote server, or to
listen for connections from a remote server.  The component always opens
a socket by IP address, but will look-up that IP address from a host
name if required, or provide a reverse look-up of host or domain name
from an IP address. TSslWSocket sends or receives a stream of 8-bit
binary characters, but does have methods to send and receive lines by
checking or sending a CRLF line ending, which is the Telnet protocol,
used for the headers all most other high level protocols like HTTP,
FTP, SMTP, etc.  TSslWSocket can use TCP or UDP transmission, most
protocols use TCP, except DNS and SNMP. TSslWSocket can be tested using
OverbyteIcsSimpleSslCli.dpr, OverbyteIcsCliDemo.dpr, OverbyteIcsClient5.dpr,
OverbyteIcsClient7.dpr, OverbyteIcsUdpLstn.dpr, OverbyteIcsUdpSend.dpr
and many others.  Note TSslWSocket requires an SslContext for SSL
configuration.

TIcsIpStrmLog in unit OverbyteIcsIpStreamLog is a higher level version
of TSslWSocket, originally designed for IP stream logging with minimal
events and extra coding, including an SslContext and full SSL/TLS
certificate chain checking, with better line handling, multiple
connection attempts and retries on failure or loss of connection.
TIcsIpStrmLog can be configured a client or server, TCP or UDP, and
is tested by OverbyteIcsIpStmLogTst.dpr which can run as client and
server at the same time, sending data to itself.


Simple TCP Socket Server
------------------------

TSslWSocketServer in unit OverbyteIcsWSocketS is the main socket server
accepting a few thousand remote clients using multiple IP addresses and
ports, and separately allowing data to be sent and received from those
remote clients, all in a single thread.  Applications need to derive
a client from TSslWSocketClient into which the required functionality
is added.  TSslWSocketServer supports using collections of IcsHosts
properties. This allows the server to listen on multiple IP addresses
and ports at the same time with different SSL/TLS certificates for each
host using built-in SslContexts, will automatically create self signed
SSL/TLS certificates so the server can start, and will them order free
SSL/TLS certificates from Let's Encrypt (provided running on the public
internet), and re-order them every three months before they expire.

TSslWSocketServer is mostly tested using the ICS HTTP and FTP servers,
but there are other samples, OverbyteIcsSslSmtpServ, OverbyteIcsTcpSrv.dpr,
OverbyteIcsTcpSrvIPv6.dpr, etc.

TIcsIpStrmLog mentioned just above uses TSslWSocketServer for simpler
server applications with a small number of remote clients.

There is also a threaded version TSslWSocketThrdServer in unit
OverbyteIcsWSocketTS where each client is created with a separate thread
to avoid blocking on high load servers.  Beware this server does not yet
support IcsHosts and multiple IP addresses, nor is there a web server
using it.  It is tested using OverbyteIcsThrdSrvV3.dpr.


Forward or Reverse Proxy Server
-------------------------------

TIcsProxy and TIcsHttpProxy in unit OverbyteIcsProxy are designed for
forward or reverse socket proxying and are tested by
OverbyteIcsProxySslServer.dpr.  Despite the component names, these
components support SSL using IcsHosts with all the usual functions.
TIcsProxy is protocol agnostic and may be used to proxy any TCP protocol,
the sample includes SMTP, POP3, NNTP and telnet. TIcsHttpProxy is a full
forward and reverse HTTP/HTTPS proxy with header and body parsing and
processing host names and URLs to match the source and destination.
Note the sample has a minimal GUI and is configuring using an INI file.


Websockets Server
-----------------

TWebSocketSocket in unit OverbyteIcsWebSockets is a basic websockets server
based on TWSocketServer and websockets implementation ported from phpws
project, tested by OverbyteIcsWebSocketSrv.


Telnet Client
--------------

TTnCnx in unit OverbyteIcsTnCnx implements the TCP/IP telnet protocol
including some options negotiations, tested by OverbyteIcsTnDemo.dpr.
There are samples that support telnet server, ANSI terminal emulation,
and chat components.


Network News Reader, NNTP Client
--------------------------------

TNntpCli in unit OverbyteIcsNntpCli is a NNTP client, tested by
OverbyteIcsNewsReader.dpr with 28 buttons for the various commands,
SSL is not supported yet.


Create, Order or Review SSL/TLS Certificates
--------------------------------------------

ICS contains many functions for processing SSL/TLS X509 certificates and
private keys. TX509Base in unit OverbyteIcsWSocket may contain and server
or client certificate, private key and one of more intermediate
certificates, and has properties to display most of the certificate
elements, all tested by OverbyteIcsPemtool.dpr.  TX509List contains
multiple certificates, typically a root store.  The sample allow root
stores and single certificates to be viewed, roots extracted from the
Windows Certificate Store, and for SSL/TLS certificates, private keys
and certificate requests to be created and saved in multiple file formats
and bundles using TSslCertTools in unit OverbyteIcsSslX509Utils.

TSslX509Certs in unit OverbyteIcsSslX509Certs, tested by
OverbyteIcsX509CertsTst.dpr automatically downloads SSL/TLS X509
certificates from various issuers, including free certificates from Let's
Encrypt, and  commercial certificates from CertCentre AG. Supports ACME
V2 protocol, and REST protocols for specific vendors.  Domain and DNS
validated certificates should generally  be issued without intervention,
other commercial certificates may take days to be approved. This unit may
be added to ICS server applications using IcsHosts while the sample may
separately used to order certificates, including DNS validated wildcard
certificates from Let'S Encrypt.  All orders are kept in a database to
allow automatic or manual re-ordering before expiry.


Lookup Domain Names, DNS
------------------------

Simple DNS host look-ups are done using the DnsLookup method in TSslWSocket
and also ReverseDnsLookup, both fire an event with potentially multiple
results, tested by OverbyteIcsDnsLook.dpr and OverbyteIcsBatchDnsLookup.dpr.
These functions use Windows APIs that use the DNS servers configured for
the PC.

TDnsQuery in unit OverbyteIcsDnsQuery allows more complex DNS requests to
be made to any DNS server to get any DNS records, tested using
OverbyteIcsNsLookup.dpr and OverByteIcsDnsResolver.dpr.

Unit OverbyteIcsWmi contains a number of functions for accessing a Windows
DNS Server (Windows Server 2012 and later) to list DNS zones and zone
records, and to add zone records, tested by OverbyteIcsWmiTst.dpr.  The
functions are also used by OverbyteIcsX509CertsTst.dpr to add DNS records
for the ACME DNS challenge.


Network Diagnostic Tools
------------------------

TPing in unit OverbyteIcsPing is used to ping any host to see if it's
available on the internet, note some hosts may deliberately not reply,
tested by OverbyteIcsPingTst.dpr which includes trace route.

TSnmpCli in unit OverbyteIcsSnmpCli does SNMP (simple network management
protocol), tested by OverbyteIcsSnmpCliTst.

TSysLogClient in unit OverbyteIcsSysLogClient send syslog packets, tested
by OverbyteIcsSysLogClientDemo.dpr.

TSysLogServer in unit OverbyteIcsSysLogServer receives syslog packets,
tested by OverbyteIcsSysLogServerDemo.dpr.

TIcsWhoisCli in unit OverbyteIcsWhoisCli makes Whois requests to get
details for the registrations of domain names and IP address ranges,
tested by OverbyteIcsWhoisCliTst.dpr.  The component has a large list
of Whois servers for various countries around the world.

TIcsTimeClient and TIcsTimeServer in unit OverbyteIcsSntp support SNTP
for getting and setting the correct time over the internet, tested
using OverbyteIcsTimeTst.dpr.



About SSL:
----------

TSslWSocket and TSslWSocketServer component are derived from the standard
TWSocket and TWSocketServer component. The SSL code is compiled into the
component only if you define USE_SSL symbol to your packages and projects.
Just add USE_SSL to the defines in the project or package options and
recompile everything.

The components make use of libcrypto-1_1.dll (or libcrypto-1_1-x64.dll) and
libssl-1_1.dll (or libssl-1_1-x64).dll to handle SSL protocol stuff. The DLLs
are dynamically loaded at runtime. It means that the DLLs will only be required
at runtime when you first make use of a SSL function. Your applications will
run on systems without OpenSSL DLLs as long as you don't call any SSL function.
The ICS distribution includes the latest OpenSSL files or they may be downloaded
from:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download

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
               A demo CA file in PEM format containing about 52
               well known root CA certificates to be specified in
               property CA Path of the demo applications. Read
               the comments included in this file.
ROOTCABUNDLE.PEM :
               A demo CA file in PEM format containing about 280
               well known root CA certificates to be specified in
               property CA Path of the demo applications. Read
               the comments included in this file.
6F6359FC.0 :   Located in sub directory SslInternet\TrustedCaStore,
               it's the file CACERT.PEM stored with a hashed file
               name. Directory TrustedCaStore can be specified in
               property CA Path of the demo applications.

For details about certificate, see the excellent book:
  "Network security with OpenSSL", O'Reilly, ISBN 10: 0-596-00270-X

The SSL demo project OverbyteIcsPemTool may be used to create self
signed PEM certificates, certificate requests for commercial use, to
convert existing certificates in the Windows Certificate Store
to PEM format understood by OpenSSL and to examine PEM certificates.

You will find more information in IcsSslHowTo.txt file.



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


francois.piette@overbyte.be
francois.piette@swing.be
http://www.overbyte.be/
http://wiki.overbyte.eu/


