PBEditPack.
-----------
Version 8.50.00.00

Copyright © 1999 - 2003 BakSoft-Denmark, Poul Bak.

http://home11.inet.tele.dk/BakSoft/
Mailto: baksoft.denmark@tiscali.dk
NOTE: Be sure to include my name in the mail-body to get pass my filters.

-----------------------------------
PBEditPack is a collection of 7 FREEWARE Edit-components for Delphi: PBBinHexEdit, PBEdit, PBEditEx, PBMaskEdit, PBNumEdit, PBSuperSpin and PBSpinEdit.

Supports Default-button-Click and Cancel-button-Click.

Full source and Context-sensitive help is included.

The components have been tested with Delphi 6.2 and 4.0, but should work in Delphi 3 - 7.

All components have Alignment and 'mouse-selectall'.
'Mouse-selectall' means that when you set focus using the mouse (and AutoSelect is True), all the text is selected. Next click deselects. That is more or less a 'Windows-standard'. When you set focus using <tab> it is standard, so it is more logical that the mouse works the same way.

All components have DisabledColor property - changes the backgroundcolor when disabled.

PBEdit, PBMaskEdit and PBSpinEdit are standard components with Alignment and 'mouse-selectall'.

PBEditEx is PBEdit component that can show an image in the Edit-box.

PBBinHexEdit is a special Edit component for binary and hexadecimal values. NumberFormat sets the display- and editformat (Number, Binary or HexaDecimal). Set and access values by propertys: AsInteger, AsHex and AsBin. (Tip: You can make it invisible, if you you just need the conversions).

PBNumEdit is a special Edit component for numeric values - supporting WYSIWYG editing, floating and fixed decimalpoint. Supports WM_SETTINGCHANGE. NumberFormat sets the display- and editformat (Standard, Thousands, Scientific and Engineering). You can set max- and minValue. 

PBSuperSpin is PBNumEdit component with spin-buttons (have all PBNumEdit's functions). Increment by decimal values (not just integers). Wrap can set value to MinValue when MaxValue is exceeded. Accelerated spin. RoundValues will round values, that users enter.

Legal stuff:
------------
PBEditPack is Freeware. Use it any way, you like. There are no timelimit or 'nag-screens'. Applications developed using these components are yours alone, Baksoft-Denmark have no rights to it.

PBEditPack is provided 'as-is' and Baksoft-Denmark is under no circumstances responsible for any damage, what soever, that it might cause to anyone or anything.

Installation:
-------------
1. Unzip all files to a folder of your choice.
Note for Delphi 3/4: All forms are saved as text format. To convert them into Delphi 3/4 binary format: Open the folder where you have unzipped the files. Doubleclick 'Convert_Forms.bat'. Now the forms are in Delphi 3/4 format. When you later open the forms, Delphi might tell you that 'Property does not exist'. Just ignore (none of the new properties are used).

2. Start Delphi (if you haven't done so).
3. If you have unzipped to a folder that is not in the Delphi Search path, add it: Select 'Tools', 'Environment options...', 'Library' tab, add the full path to the folder to 'Library path'.
4.  Delphi 3, 4 & 5: Select 'Files', 'Open', 'Delphi Package Source' as filetype and browse to the folder. Select 'PBEditPack345.dpk' and 'open'.
Delphi 6: Select 'Files', 'Open', 'Delphi Package Source' as filetype and browse to the folder. Select 'PBEditPack.dpk' and 'open'.
5. Click 'Install'.

Now you have a new Palette-page named 'PB' with the new components.

Install context-sensitive help:
-------------------------------
1. Move or copy 'PBEditPack.hlp' and 'PBEditPack.cnt' to ..\Delphi\Help folder.
2. Doubleclick 'Delphi6.cnt' and 'Microsoft Help Workshop' starts.
   Click 'Index Files', 'add'. Type 'PBEditPack' as Help title and 'PBEditPack.hlp' as Help filename. Click 'Ok'.
3. Include 'PBEditPack.cnt' in the main page (Insert above).
4. Exit 'Microsoft Help Workshop' and save the changes.

Now you have full context-sensitive help for the PBEditPack components.
--------------------------------
I have included a small demo: PBEditPackDemo to show the basics. Load, compile and run.

