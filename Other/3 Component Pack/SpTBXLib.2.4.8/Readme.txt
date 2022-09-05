[SpTBXLib]

SpTBXLib is an add on package for TB2K components, it adds the following features:
- Skins
- Unicode support for items captions and hints properties.
- Custom painting events.
- Custom item size.
- Anchored items.
- Right aligned items.
- Accel char handling.
- Button, Label, Checkbox, RadioButton, with unicode support.
- TabControl with toolbar items support.
- DockablePanel with unicode toolbar items support.
- Panel and GroupBox with transparency and unicode support.
- Titlebar and Form Popup components with unicode support.

For more info go to:
http://www.silverpointdevelopment.com

[License]

Use and/or distribution of the files requires compliance with the
SpTBXLib License, found in SpTBXLib-LICENSE.txt or at:

  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, at your option, the files may be used and/or distributed under
the terms of the Mozilla Public License Version 1.1, found in MPL-LICENSE.txt or at:

  http://www.mozilla.org/MPL


[Installation]

Requirements:
- Jordan Russell's Toolbar 2000 
  http://www.jrsoftware.org
- Troy Wolbrink's TNT Unicode Controls (not needed for Delphi/C++Builder 2009 or newer)
  http://www.tntware.com/delphicontrols/unicode/
  Mirror: http://www.silverpointdevelopment.com/sptbxlib/TntUnicodeControls.zip

To install SpTBXLib manually:
- If you have a previous version of SpTBXLib installed in the IDE remove it from Component->Install Packages, select SpTBXLib from the list and press the Remove button. 
- Add the SpTBXLib 'Source' directory to Tools->Environment Options->Library->Library Path.
- Open the SpTBXLibDsgn_*.dpk design package corresponding to the IDE version, press Compile and then press Install, close the package window (don't save the changes).

To install SpTBXLib with Silverpoint MultiInstaller (http://www.silverpointdevelopment.com/multiinstaller/index.htm):
- Create a new folder for the installation.
- Download all the component zips to a folder: SpTBXLib + TNT + TB2K
- Download the MultiInstaller and the Setup.Ini, extract them to the folder:

The installation folder will end up with this files: 
C:\MyInstall
       |-  SpTBXLib.zip
       |-  TntUnicodeControls.zip
       |-  tb2k-2.2.2.zip
       |-  MultiInstaller.exe
       |-  Setup.ini 

You are ready to install the component packages, just run the MultiInstaller, select the destination folder, and all the components will be unziped, patched, compiled and installed on the Delphi IDE.

For more info go to:
http://www.silverpointdevelopment.com/sptbxlib/support/index.htm