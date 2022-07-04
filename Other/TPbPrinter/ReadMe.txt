Author: Poul Bak
Copyright © 2000 - 2005 : Bak-O-Soft (Poul Bak). All rights reserved.
http://bak-o-soft.dk/
Mailto:info@bak-o-soft.dk

Component Version: 6.20.00.00

TPBPrinterSetupDialog is a TPrinterSetupDialog component with capability of getting/setting printersetup-values (orientation, papersize etc) at designtime and runtime. Settings can be saved so users don't have to setup the printer every time they run your program.
Standard PrinterSetupDialog is used as propertyeditor.
At runtime you can switch between 4 setuptypes: stDefault (like standard PrinterSetupDialog), stInitial (settings set at designtime), stSaved (user defined settings saved) and stUser (settings set when the dialog has executed). AutoSave and ForceInitialSetupValues.

Version 6.00 supports long printernames for Windows 2000/XP.

Version 3.00 has a small unit (DelphiPrinterSetup) that, when installed, automatically saves and reloads the printersetup in Delphi (for instance to print code in low quality).

Context-sensitive help is included.

How to use: First set the InitialSetupOptions (the values you want to change from default).
Then either: Click the ellipse-button by InitialSetupValues (or double-click the component on the form) to launch a PrinterSetupDialog, change the values there and press 'Ok'.
Or: Double-click +InitialSetupValues to expand the sub-properties. Set the values by picking from the lists or enter your own values.
That's enough - use the other properties if you like.

Note: When you have selected the PBPrinterSetupDialog component on the form, the printersettings will follow the settings of the component until you deselect the component again (or closes your project). Pay attention if you print from Delphi.

Legal stuff:
------------
PBPrinterSetupDialog is Freeware. Use it any way, you like. There is no timelimit or 'nag-screens'. Applications developed using this component are yours alone, Bak-O-Soft have no rights to it.

PBPrinterSetupDialog is provided 'as-is' and Bak-O-Soft is under no circumstances responsible for any damage, what soever, that it might cause to anyone or anything.

Installation:
-------------
NOTE: If you are upgrading, you must delete the old cfg-files prior to installing the new version !

1. Unzip all files to a folder of your choice.
Note for Delphi 3/4: All forms are saved as text format. To convert them into Delphi 3/4 binary format: Open the folder where you have unzipped the files. Doubleclick 'Convert_Forms.bat'. Now the forms are in Delphi 3/4 format. When you later open the forms, Delphi might tell you that 'Property does not exist'. Just ignore.

2. Start Delphi (if you haven't done so).
3. If you have unzipped to a folder that is not in the Delphi Search path, add it: Select 'Tools', 'Environment options...', 'Library' tab, add the full path to the folder to 'Library path'.
4.  Delphi 3, 4 & 5: Make a new package: Press 'File', 'New', 'Package', 'Ok'. Press 'Add' and browse to 'PBPrinterSetupDialog.pas'. Press 'Ok'. Repeat for the other units.
Delphi 6+: Select 'Files', 'Open', 'Delphi Package Source' as filetype and browse to the folder. Select PBPrinterSetupPackRun.dpk' and 'open'.
5. Click 'Compile'.
6. Delphi 6+: Select 'Files', 'Open', 'Delphi Package Source' as filetype and browse to the folder. Select PBPrinterSetupPack.dpk' and 'open'.
7. Click 'Compile'.
8. Click 'Install'.

Now you have a new Palette-page named 'PB' with the new component.

Install context-sensitive help:
-------------------------------
1. Move or copy 'PBPrinterSetupDialog.hlp' and PBPrinterSetupDialog.cnt' to ..\Delphix\Help folder
(x is your Delphi version).
2. Doubleclick 'Delphix.cnt' and 'Microsoft Help Workshop' starts. Click 'Index Files', 'add'. Type 'PBPrinterSetupDialog help' as Help title and 'PBPrinterSetupDialog.hlp' as Help filename. Click 'Ok'.
3. Include 'PBPrinterSetupDialog' on the main page (Insert above).
4. Exit 'Microsoft Help Workshop' and save the changes.

Now you have full context-sensitive help for the PBPrinterSetupDialog component.
--------------------------------
I have included a small demo: Project1.exe to show the basics. Load, compile and run.

If you find any bugs, have any great ideas or just a comment or question, feel free to e-mail.
