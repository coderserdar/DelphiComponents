LMD-Tools Special Edition
***********************
© by LMD Innovative (http://www.lmd.de)


Free Component Suite with 95+ components for Delphi/CBuilder 6 and higher 
Release: 2011


Welcome to LMD-Tools SE-Edition! This package represents a free extract of almost 100 components of our successful LMD-Tools component suite. Against previous SE releases this version uses the same core like the full releases without any limitations. Even the same runtime packages are now included.

No restrictions exist - all components run outside the IDE as well and can be used royality free in your applications. 

Limitations against commercial versions are among others:
* Major components like the ButtonBar, SplitterPanel, Extended Dialog, Datasensitive or TLMDCalendar related controls are missing.
* Several enhanced editors are missing.
* No bonus packages are included

For complete feature list of latest LMD-Tools 8 features check our web site at http://www.lmd.de. 

SPECIAL NOTE 
***************
LMD-Tools SE 7.1 and LMD-Tools SE 2011 provide a different set of components. If you rely on certain controls in your existing projects, please check first Comparison Sheet our web site, whether these are still included in LMD-Tools 2011 SE:
http://www.lmd.de/mfs/products/lmdtoolsse.php
If not, it is recommended to stick with LMD-Tools 7 or to upgrade to higher leveled LMD-Tools releases.
***************

The names of LMD-Tools SE ZIP archives follow this pattern:
setupse11XX.zip

The XX represents the compiler version: 
* D6 indicates version for Delphi 6, B6 for C++ Builder 6.
* D7 indicates version for Delphi 7.
* D9 indicates Delphi 2005 Win32, D9Net Delphi 2005 VCL.NET version.
* D10 indicates Borland Developer Studio 2006 Win32 (Delphi 2006), B10 indicates BDS 2006 Win32 (Delphi AND C++ Builder), D10Net BDS 2006 Delphi.NET version.
* D11 indicates CodeGear RAD Studio 2007 (Win32, Delphi only), B11 indicates CRS 2007 (Win32, Delphi AND C++ Builder), D11Net CRS 2007 (Delphi.NET)
* D12 indicates Delphi 2009 (Win32, Delphi only) and B12 indicates C++ Builder 2009 release.
* D14 indicates Delphi 2010 (Win32, Delphi only) and B14 indicates C++ Builder 2010 release.
* D15 indicates Delphi XE (Win32, Delphi only) and B15 indicates C++ Builder XE release.

Installation
============
Use setup.exe to start the installation program and follow the instructions. The installer tries to install package and help files automatically. A full featured installer and uninstalling instructions are provided. Please read _README.HTM after installation. If installation fails for any reason, please check LMDTroubleShooting.htm or LMDInstall[..].htm in \info folder of installation directory. 
Uninstaller is provided. Additional information for manual installation are available in LMDInstall[..].htm document. 


NOTES
=====
1.) Make sure that your IDE has latest Service Packs installed. 

2.) If you used any previous or other release before: Please uninstall this version COMPLETELY to avoid problems caused by mixing up two different versions.

3.) The LMD 2011 releases can not be used at the same time with other LMD 200X release packages e.g. LMD-Tools 7.X (SE). Make sure you update either to latest version or use our free LMDPackUtil (http://www.lmd.de/download) utility to switch between different package settings. 

4.) Please note that Trial will NOT work in any Delphi/C++ Builder Trialversion! Reason is the different DCU signature of Delphi/C++ Builder releases against full versions.


How to remove an old LMD-Tools version or package completely:
=============================================================
To remove LMD-Tools from your system completely follow the directions below.

Delphi 7
- Close all files and projects and and select "Components|Install Packages...".
- Select all LMD-Tools packages from the Packages list and click the "Remove" button. 
- After that remove all LMD-Tools paths from the global library search path (Library Page/Environment Options in the Tools Menu).
- Start OpenHelp tool and remove any LMD-Tools helpfiles (Menu Help|Customize..., select Tab "Link" then).

Removing LMD-Tools files completely from your hard disk
After uninstalling the components from the CBuilder IDE/Delphi you may remove the component files physically. 
-Open the Add/Remove Programs icon from the Control Panel and double click the "LMD ElPack 8.0" entry.
- All the DCUs/object and backup files have to be removed manually from the LMD-Tools' uninstallation directory. As a rule the installation directory won't be deleted during the unstallation because such files will be generated while you work on the Tools but - for safety's sake - won't be deleted automatically by the uninstaller. 
- Remove all dcllmd*.* and lmd*.* files from the \projects\bpl (and  \projects\lib in CBuilder) folders below your CBuilder/Delphi installation folder. If you had chosen to install runtime files into system directory, remove all lmd*.bpl packages from the System directory of your Windows installation. To be sure search harddisk for these files and remove them all. Do not remove license files or registration utility from the system directory.
Be sure that you did not compile LMD-Tools files into a custom package!
- Possible registry keys can be found under:
HKEY_CURRENT_USER\Software\LMD Innovative\LMD-Tools 2011 and
HKEY_LOCAL_MACHINE\SOFTWARE\LMD Innovative\LMD-Tools 2011
Remove them if you don't want to use LMD 2011 products any longer.


Redistribution Notes
====================
This file may be redistributed freely as long as the included Exe-installer remains unchanged.

If you encounter problems feel free to contact us:

email: 			support@lmd.de
fax:			+49-69-25428779
newsgroups:		news.lmdtools.com


LMD Innovative, Germany

