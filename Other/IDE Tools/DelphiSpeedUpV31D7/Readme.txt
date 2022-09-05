== Delphi SpeedUp ===

DelphiSpeedUp improves the general Delphi IDE speed by optimizing I/O
operations and replacing slow RTL functions with faster algorithms and
and functions with optimized CPU register usage.

Code from the FastCode project (http://fastcode.sourceforge.net) is used
to optimize RTL functions for the installed CPU.


=== How to install ===

Simply start the InstallDelphiSpeedUpX.exe where X is your Delphi/BCB version.

This will copy the DelphiSpeedUpX.dll and DelphiSpeedUpLoaderX.bpl to
$(APPDATA)\DelphiSpeedUp and it registers the BPL as an Known IDE Package
in the registry. This allows DelphiSpeedUp to be loaded very early.


=== How to uninstall ===

Start the InstallDelphiSpeedUpX.exe (where X is your Delphi version number) and
press the <Uninstall> button.


=== Delphi/BCB versions ===

5 = Delphi/BCB 5
6 = Delphi/BCB 6
7 = Delphi 7
9 = Delphi 2005
10 = Delphi/C++Builder/BDS 2006
105 = Delphi/C++Builder/RAD Studio 2007



Author:

Andreas Hauslanden (Andreas.Hausladen@gmx.de)
http://andy.jgknet.de/dspeedup
