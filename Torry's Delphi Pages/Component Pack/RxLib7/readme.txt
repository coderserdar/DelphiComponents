  RX Library 2.75 conversion to Delphi 7, v1.0 (based on conversion by SGB Software)
  from Project JEDI (http://jvcl.sourceforge.net)
  =================================================================

TABLE OF CONTENTS
-----------------
  Overview
  Conversion to JVCL
  Compatibility with your older code
  History
  Installation
  Help files
  Copyright Notes


Overview
--------
This is an official conversion of the Rx Library of components for Borland Delphi 6.  You are hearby given permition to distribute the contents of this zip file as long as all copyright notices are kept in there current state.

Rx Library is a set of components and classes for Borland Delphi and C Builder.  Originally developed by Fedor Koshevnikov, Igor Pavluk and Serge Korolev, it has become a widely used and respected addition to the Borland VCL.

Conversion to JVCL
--------------------------

In the "Convert" subdirectory you'll find "JVCL Convert", a global search/replace tool that will help you to convert you RxLib-based projects to JVCL. As you might know already, JVCL incorporates now the RxLib components, but in order to avoid any name conflicts, all of them are renamed to JVCL prefix standard, i.e. "Jv"

After you install JVCL 2.0, and convert your projects, they should work with JVCL. 

Before you start with the conversion, please make sure that your .DFM files are in Text format (open them in Delphi, and save as text).

You can find more information about JVCL on our Website: http://jvcl.sourceforge.net


Compatibility with your older code
-----------------------------------
  - StrUtils has been renamed to rxStrUtils to avoid a naming conflict with borlands new StrUtils module.
  - RxGrids.TInplaceEditStyle type has been converted to the Delphi 6 TEditStyle type.
  - DualList has been renamed to RxDualList 


History
-------
08/01/2002
  First Official release of RX Library 2.75 for Delphi 7

20/02/2002
  First Official release of RX Library 2.75 for Delphi 6.


Installation
------------

IF YOU HAVE DELPHI 6 PROFESSIONAL OR PERSON EDITION YOU MUST DEACTIVATE THE CONDITIONAL DEFINE {$DEFINE DCS} IN THE RX.INC FILE BEFORE ATTEMPTING TO COMPILE THIS LIBRARY.  FAILURE TO DO THIS COULD CAUSE THE LIBRARY TO INSTALL INCORRECTLY.

On the File Menu select the Open command (Ctrl+ O) and browse for the Delphi Package RXCTL7.DPK that you extracted from this archive. In the Package window click on the compile button to
compile the package, RXCTL7.DPK. 

After compiling repeat these steps for the remaining RX Library run-time packages RXDB6.DPK, RXBDE7.DPK. 

Put the compiled BPL files into a directory that is accessible through the search PATH. After compiling RX run-time packages you must install RX design-time packages into the IDE.

On the File Menu select the Open command (Ctrl+ O) and browse for the Delphi Package DCLRX7.DPK that you extracted from this archive. In the Package window click on the compile button to
compile the package and then click on the Install button to register the package in the IDE.

Repeat the above step for the remaining RX Library Design Time packages, DCLRXDB7.DPK and DCLRXBD7.DPK.

NOTE: do not save the package sources in the Delphi IDE.

Help files
----------

Help files for the Rx Library are currently provided in 2 languages , English and Russian.

English help file is copyright of Check frasersoft (http://www.frasersoft.clara.net/rxhelp) 2001

Copyright Notes
---------------
  RX Library is a copyright of SGB Software,Fedor Kozhevnikov, Igor Pavluk and Serge Korolev.  All rights reserved.  For Detailed licensing and distribution instructions please navigate to www.sgbsoftware.com or consult your help file.

---END OF FILE---




last updated - 08/01/2002