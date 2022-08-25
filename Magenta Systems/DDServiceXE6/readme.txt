DD Service Application Framework
================================

http://delphi.duodata.de


Description
-----------

DDService is an enhanced Windows NT service application 
framework for Delphi and C++ Builder based on the original
VCL service framework. In addition to it it also encapsulates
new Windows NT service APIs introduced since Windows 2000.
DDService is freeware with source and currently supports
Delphi 5, 7 and 2006-XE5 as well as C++ Builder 2006-XE5.

Features:
 
 - Win 2000 FailureOptions and FailureActions, 
   Service Description.
   HandlerEx extended service controls:
   ParamChange, NetBindChange. 
   Optional device events as well as power events.
   Assigning one of these events creates a hidden
   window in the context of the service thread.
   Creation of the service window can be enforced by
   setting option eoForceServiceThreadWindow.
 - Win XP SessionChange service control
 - Win Vista PreShutdown service control and
   NonCrashFailures flag, Service SID Info, Required
   Privileges and StartType AutoDelayed.
 - Optional console control handler
 - Property ServiceName.
 - Includes fixes of QC #68050 and QC #37706.  
  

Installation
------------

If any previous version of DDService is already
installed remove it from Components | Install Packages..

In RAD Studio or BDS installing the Delphi or C++ Builder
packages installs DDService into both Delphi and C++ Builder
personality.

1 - Unzip DDService.zip to a new or empty directory of your
  choice. Make sure to unzip the directory structure included
  in the zip file.

2 - If not yet done run CopyAndPatchSvcMgr.bat in order to
  create unit \Source\DDSvcMgr.pas which will copy the original
  VCL source file SvcMgr.pas and patch the copy.

3 - Open group project file InstallDDServicexxxxx located
  in one of the sub directories of directory \Packages.
  Each IDE supported has its own sub directory, i.e if
  you want to install into Delphi or RAD Studio XE5
  open \Packages\Delphi XE5\InstallDDServiceDelphiXE5.groupproj.

  The IDE might ask for permission to create missing res-files,
  simply confirm. In newer IDE versions additional project
  files might be created, if asked save all.

4 - Build the (32-bit) runtime package (do not install).

5 - In Delphi XE2+ switch target platform of the runtime package
  to "Windows (64-bit)" and build for 64-bit as well
  (do not install).

6 - Build and then install the designtime package.

7 - Set search paths in Tools | Options:

  Delphi XE5
  - Add directory \Lib\XE5 to the 32-bit Windows Library
    search path i.e. "C:\DDService\Lib\XE5".
  - Add directory \Lib\XE5\Win64 to the 64-bit Windows
    Library search path i.e. "C:\DDService\Lib\XE5\Win64".
  - Add directory \Source to the browsing path for
    both 32 and 64-bit, i.e "C:\DDService\Source".

  Delphi XE and older
  - Add directory \Lib\xx to the Library search path
    where "xx" has to be replaced by the IDE version,
    i.e. "C:\DDService\Lib\XE" or "C:\DDService\Lib\2010".
  - Add directory \Source to the browsing path
    i.e "C:\DDService\Source".

  C++ Builder XE5 and older
  - Add directory \Lib\xx to the 32-bit Windows Include
    path where "xx" has to be replaced by the IDE version,
    i.e. "C:\DDService\Lib\XE2" or "C:\DDService\Lib\2010".
  - Add directory \Source to the browsing path
    i.e "C:\DDService\Source".
  
After installation a new item "DDService" is available in the
repository and on the Tool Palette containing wizards to create
a new DDService application as well as a new DDService. In
directory \Demos you'll find some simple demo applications.


Revision History
----------------

- 06 March 07 Removed calls to Classes.(De)AllocateHwnd
  since they are not thread-safe. Made WndProc virtual.
- 08 Sept 07 New event OnRunException.
- 17 Oct, 08 New property ServiceName. Fixed a (CodeGear)
  bug in TDDService.ReportStatus.
  As a result it is now possible to delay Vista system
  shutdown in event OnPreshutdown. Also random
  ERangeErrors in function ReportStatus are now gone.
  Delphi 2009 compatibility added.
- V1.4 03 Nov 08 ( version number introduced ;-)
  Value of property ImagePath is now enclosed in double
  quotes if it includes spaces in order to avoid 
  error 193:0xc1 on service start, more details:
  http://support.microsoft.com/default.aspx?scid=kb;en-us;Q812486.  
  Added const WM_USER_DDSERVICE which should be used as
  a base to create custom message IDs for custom messages
  to be sent to the service window. New demos added.
- V1.5 13 Nov 08 load extended service API dynamically
- V1.6 Aug 09 Windows 7 service API headers and
  packages for Delphi 2010 added.
- V1.7 Aug 2010 packages for Delphi XE added.
- V1.8 Aug 2011 Added support for Delphi XE2 and
  C++ Builder 2006 - XE2.
- V1.8a Aug 2012 Added support for Delphi and C++ Builder XE3.
- V1.8b Apr 2013 Added support for Delphi and C++ Builder XE4.
- V1.8c Sep 2013 Added support for Delphi and C++ Builder XE5.

Notes 
-----

In this version messages WM_POWERBROADCAST as well as 
WM_DEVICECHANGE are trapped in the window procedure of the
service window directly instead of registering/handling
SERVICE_CONTROL_POWEREVENT and SERVICE_CONTROL_DEVICEEVENT.
I think this is a solution since both control codes expect
a message result/return which otherwise would require
synchronize the control codes by SendMessage them from
HandlerEx to the service thread window.

Please send your comments, improvements, or fixes to:
Arno Garrels <arno.garrels@gmx.de>

Enjoy
 
  