                    Pro VCL Extensions Library
                    ~~~~~~~~~~~~~~~~~~~~~~~~~~

                    Version 1.86 (14-Mar-2005)
                    ~~~~~~~~~~~~~~~~~~~~~~~~~~
           Copyright (c) 1996-2005 by Dmitry G. Barabash
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



CONTENTS
~~~~~~~~
  1. General
  2. Copyrights
  3. Licence agreement
  4. Redistribution
  5. Support, feedback and bugs
  6. Installing the Pro VCL Extensions Library
  7. Supported languages
  8. Revision History
  9. Contacting




1. GENERAL
~~~~~~~~~~

Pro VCL Extensions Library is a freeware component library for
Borland Delphi 1, 2, 3, 4, 5, 6, 7 & 9 (2005) and Borland C++Builder 
1, 3, 4, 5 & 6. Pro VCL Extensions Library includes 28 native VCL
components, component and property editors, many useful procedures
and functions and complete source code for all components and all
component and property editors. Unfortunately there is *no* help
available.




2. COPYRIGHTS
~~~~~~~~~~~~~

This product is Copyright (c) 1996-2005 by Dmitry G. Barabash.
Author retains full copyright in all the files included in
this package. If you make any modifications to the source,
please send a copy to dmitry@barabash.com.




3. LICENCE AGREEMENT
~~~~~~~~~~~~~~~~~~~~

This library may be used freely and distributed in commercial or private
environments, provided this notice is not modified or removed. This
library may be included in compilations of other software, as long as
there is no charge for this library or part of this library specifically
(e.g. as a Delphi component or as part of a Delphi component collection).

This library is provided "AS-IS". No warranties of any kind, expressed
or implied, are made as to it or any medium it may be on. Any remedy for
indirect, consequential, punitive or incidental damages arising from it,
including such from negligence, strict liability, or breach of warranty
or contract, even after notice of the possibility of such damages will
not be provided.




4. REDISTRIBUTION
~~~~~~~~~~~~~~~~~

You may freely redistribute this package, on the following conditions:

- the whole package is redistributed;
- no alteration is made to the package, or to any of the files in it;
- no charge (other than charges for disk duplication and handling) be
  made for any redistribution.




5. SUPPORT, FEEDBACK AND BUGS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I do not "officially" provide support for this software. However,
if you have any questions about it, please feel free to write to me at:

    dmitry@barabash.com

I will try to answer your questions promptly. If you have any suggestions
for improvement, please let me know also, as I will be more than happy to
hear your opinions!

If you find a bug in this library, please let me know by sending a complete
description of the bug and the circumstances under which it appears to:

    dmitry@barabash.com

I will correct all reported bugs as soon as possible.




6. INSTALLING THE PRO VCL EXTENSIONS LIBRARY
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pro VCL Extensions Library is distributed in the ProLibSetup.exe
installation program. To install the library, simply run
the ProLibSetup.exe program and follow the instructions.

Once the installation program is completed you will need to
perform a few steps to install the base components into the
appropriate version of Delphi or C++Builder.

Delphi 2005 Enterprise and Professional:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib90.BPL, ProLibDB90.BPL and ProDsgn90.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib90.DPK.
4. In "Project Manager" window click right mouse button on the loaded project
   "ProLib90.BPL" to open context menu. Choose "Build" to compile runtime
   package ProLib90.DPK.
5. Repeat steps 3-4 for runtime package ProLibDB90.DPK.
6. Put compiled files ProLib90.BPL and ProLibDB90.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn90.DPK.
8. In "Project Manager" window click right mouse button on the loaded project
   "ProDsgn90.BPL" to open context menu. Choose "Install" to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

Delphi 2005 Personal:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib90.BPL and ProDsgn90P.BPL from your
   hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib90.DPK.
4. In "Project Manager" window click right mouse button on the loaded project
   "ProLib90.BPL" to open context menu. Choose "Build" to compile runtime
   package ProLib90.DPK.
5. Put compiled file ProLib90.BPL into directory that is accessible
   through the search PATH (i.e. DOS "PATH" environment variable;
   for example, in the Windows\System32 directory).
6. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn90P.DPK.
8. In "Project Manager" window click right mouse button on the loaded project
   "ProDsgn90P.BPL" to open context menu. Choose "Install" to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

Delphi 7 Enterprise and Professional:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib70.BPL, ProLibDB70.BPL and ProDsgn70.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib70.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib70.DPK.
5. Repeat steps 3-4 for runtime package ProLibDB70.DPK.
6. Put compiled files ProLib70.BPL and ProLibDB70.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn70.DPK.
8. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

Delphi 7 Personal:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib70.BPL and ProDsgn70P.BPL from your
   hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib70.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib70.DPK.
5. Put compiled file ProLib70.BPL into directory that is accessible
   through the search PATH (i.e. DOS "PATH" environment variable;
   for example, in the Windows\System32 directory).
6. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn70P.DPK.
7. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

Delphi 6 Enterprise and Professional:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib60.BPL, ProLibDB60.BPL and ProDsgn60.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib60.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib60.DPK.
5. Repeat steps 3-4 for runtime package ProLibDB60.DPK.
6. Put compiled files ProLib60.BPL and ProLibDB60.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn60.DPK.
8. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

Delphi 6 Personal:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib60.BPL and ProDsgn60P.BPL from your
   hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib60.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib60.DPK.
5. Put compiled file ProLib60.BPL into directory that is accessible
   through the search PATH (i.e. DOS "PATH" environment variable;
   for example, in the Windows\System32 directory).
6. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn60P.DPK.
7. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

C++Builder 6 Enterprise and Professional:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the C++Builder IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib60.BPL, ProLibDB60.BPL and ProDsgn60.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the C++Builder IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(BCB)\Lib;$(BCB)\Bin;$(BCB)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library runtime package ProLib60.BPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib60.BPK.
5. Repeat steps 3-4 for runtime package ProLibDB60.BPK.
6. Put compiled files ProLib60.BPL and ProLibDB60.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library designtime package ProDsgn60.BPK.
8. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the C++Builder IDE.

C++Builder 6 Personal:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the C++Builder IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib60.BPL and ProDsgn60P.BPL from your
   hard disk.
2. Select "Tools\Environment Options..." menu item of the C++Builder IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(BCB)\Lib;$(BCB)\Bin;$(BCB)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library runtime package ProLib60.BPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib60.BPK.
5. Put compiled file ProLib60.BPL into directory that is accessible
   through the search PATH (i.e. DOS "PATH" environment variable;
   for example, in the Windows\System32 directory).
6. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library designtime package ProDsgn60P.BPK.
7. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the C++Builder IDE.

Delphi 5:

1. Unlike previous versions of Delphi there is no compiled version of
   DsgnIntf unit in the Delphi's Lib directory. So, you have to compile it
   by yourself. To do it you have to use the command-line Delphi compiler
   DCC32.EXE which can be found in Delphi's Bin directory. Go to
   Delphi's Source\Toolsapi directory and run the following command:

       ..\..\Bin\dcc32.exe DsgnIntf.pas

   Move obtained DsgnIntf.dcu file to the Delphi's Lib directory.
2. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib50.BPL, ProLibDB50.BPL and ProDsgn50.BPL
   from your hard disk.
3. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

4. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib50.DPK.
5. In "Package..." window click "Compile" button to compile runtime
   package ProLib50.DPK.
6. Repeat steps 3-4 for runtime package ProLibDB50.DPK.
7. Put compiled files ProLib50.BPL and ProLibDB50.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
8. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn50.DPK.
9. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

C++Builder 5:

1. Unlike previous versions of C++Builder there is no compiled version of
   DsgnIntf unit in the C++Builder's Lib directory. So, you have to compile
   it by yourself. To do it you have to use the command-line Delphi compiler
   DCC32.EXE which can be found in C++Builder's Bin directory. Go to
   C++Builder's Source\Toolsapi directory and run the following command:

       ..\..\Bin\dcc32.exe DsgnIntf.pas

   Move obtained DsgnIntf.dcu file to the C++Builder's Lib directory.
2. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib50.BPL, ProLibDB50.BPL and ProDsgn50.BPL
   from your hard disk.
3. Be sure that linker option "Use dynamic RTL" is unchecked.
4. Select "Tools\Environment Options..." menu item of the C++Builder IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(BCB)\Lib;$(BCB)\Bin;$(BCB)\Imports;C:\ProLib\Source

5. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library runtime package ProLib50.BPK.
6. In "Package..." window click "Compile" button to compile runtime
   package ProLib50.BPK.
7. Repeat steps 3-4 for runtime package ProLibDB50.BPK.
8. Put compiled files ProLib50.BPL and ProLibDB50.BPL (that are now in
   C++Builder's Projects\Bpl directory) into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
9. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library designtime package ProDsgn50.BPK.
10. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the C++Builder IDE.

Delphi 4:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib40.BPL, ProLibDB40.BPL and ProDsgn40.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib40.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib40.DPK.
5. Repeat steps 3-4 for runtime package ProLibDB40.DPK.
6. Put compiled files ProLib40.BPL and ProLibDB40.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn40.DPK.
8. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

C++Builder 4:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from C++Builder IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib45.BPL, ProLibDB45.BPL and ProDsgn45.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the C++Builder IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(BCB)\LIB;$(BCB)\LIB\OBJ;C:\ProLib\Source

3. Be sure that linker option "Use dynamic RTL" is unchecked.
4. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library runtime package ProLib45.BPK.
5. In "Package..." window click "Compile" button to compile runtime
   package ProLib45.BPK.
6. Repeat steps 3-4 for runtime package ProLibDB45.BPK.
7. Put compiled files ProLib45.BPL and ProLibDB45.BPL (that are now in
   C++Builder's Projects\Bpl directory) into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
8. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library designtime package ProDsgn45.BPK.
9. Then use "Project\Make..." or "Project\Build..." menu item to compile
   designtime package ProDsgn45.BPK.
10. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the C++Builder IDE.

Delphi 3:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the Delphi IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib30.DPL, ProLibDB30.BPL and ProDsgn30.DPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the Delphi IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       C:\Delphi3\Lib;C:\Delphi3\Bin;C:\Delphi3\Imports;C:\ProLib\Source

3. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library runtime package ProLib30.DPK.
4. In "Package..." window click "Compile" button to compile runtime
   package ProLib30.DPK.
5. Repeat steps 3-4 for runtime package ProLibDB30.DPK.
6. Put compiled files ProLib30.DPL and ProLibDB30.DPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
7. Use "File\Open..." menu item of the Delphi IDE to open
   Pro VCL Extensions Library designtime package ProDsgn30.DPK.
8. In "Package..." window click "Install" button to register
   Pro VCL Extensions Library components on the "Pro" page.

NOTE: Do not save package sources in the Delphi IDE.

C++Builder 3:

1. Uninstall previous installed version of Pro VCL Extensions Library
   from the C++Builder IDE. Remove previously compiled Pro VCL Extensions
   Library packages (if any) ProLib35.BPL, ProLibDB35.BPL and ProDsgn35.BPL
   from your hard disk.
2. Select "Tools\Environment Options..." menu item of the the C++Builder IDE.
   Go to the "Library" tab and add the full path of your Pro VCL Extensions
   Library Source directory to the "Library Path" field if you have
   not already done so. The "Library Path" field should then look
   similar to this:

       $(BCB)\LIB;$(BCB)\LIB\OBJ;C:\ProLib\Source

3. Be sure that linker option "Use dynamic RTL" is unchecked.
4. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library runtime package ProLib35.BPK.
5. Then use "Project\Make..." or "Project\Build..." menu item to compile
   runtime package ProLib35.BPK.
6. Repeat steps 3-4 for runtime package ProLibDB35.BPK.
7. Put compiled files ProLib35.BPL and ProLibDB35.BPL into directory that
   is accessible through the search PATH (i.e. DOS "PATH" environment
   variable; for example, in the Windows\System32 directory).
8. Use "File\Open..." menu item of the C++Builder IDE to open
   Pro VCL Extensions Library designtime package ProDsgn35.BPK.
9. Then use "Project\Make..." or "Project\Build..." menu item to compile
   designtime package ProDsgn35.BPK.
10. Use "Component\Install packages..." menu item to open "Packages"
   dialog box. Then click "Add..." button to locate ProDsgn35.BPL file
   to install and click Ok to close the "Packages" dialog.

NOTE: Do not save package sources in the C++Builder IDE.

Delphi 2 and C++Builder 1:

1. Choose "Component\Install..." menu item of the Delphi or C++Builder IDE.
2. Click the Add button to display the "Add Module" dialog box and
   click the Browse button to locate the directory where you placed
   files of Pro VCL Extensions Library (e.g. C:\ProLib\Source).
   Select module ProReg.PAS to install and click Ok to close the
   "Add Module" file selection dialog.
3. Click Ok to close the "Install Components" dialog.
4. Your component palette will now be recompiled.
5. Pro VCL Extensions Library are installed in the "Pro" tab on the
   component palette.

Delphi 1:

1. Choose "Options\Install Components..." menu item of the Delphi
   or C++Builder IDE.
2. Click the Add button to display the "Add Module" dialog box and
   click the Browse button to locate the directory where you placed
   files of Pro VCL Extensions Library (e.g. C:\ProLib\Source).
   Select module ProReg.PAS to install and click Ok to close the
   "Add Module" file selection dialog.
3. Click Ok to close the "Install Components" dialog.
4. Your component palette will now be recompiled.
5. Pro VCL Extensions Library are installed in the "Pro" tab
   on the component palette.




7. SUPPORTED LANGUAGES
~~~~~~~~~~~~~~~~~~~~~~

This release provides following resources:
  - English;
  - Russian;
  - Ukranian;
  - French (translation by Thierry Boulestin <thierry.boulestin@wanadoo.fr>);
  - Portuguese (translation by Aldo Caetano <aldo@vipclinic.com>,
    http://members.xoom.com/intelectus);
  - Spanish (translation by Iv€n Diniz Collazo <ivan.diniz@correo.nu>);
  - German (translation by Wolfgang Matsche <dasawm@cbn.net.id>);
  - Danish (translation by Claus Svalekjaer <claus_svalekjaer@hotmail.com>,
    http://svalekjaer.frip.dk);
  - Italian (translation by Mauro Batini <m.batini@sistinf.it>);
  - Swedish (translation by Bengt Nilsson <bengan.nilsson@telia.com>);
  - Dutch (translation by Karel Scholten <karel@hesp.demon.nl>);
  - Polish (translation by BofAG <_barrel@friko5.onet.pl>);
  - Hungarian (translation by Gabor Szucs <szucs_sw@mail.inext.hu>);
  - Turkish (translation by Cumhur Semin <freeman35@pmail.net>;
  - Slovak (translation by Roland Turcan <rolo@sedas.sk>);
  - Korean (translation by Van Cheju <vancheju@hotmail.com>),
  - Traditional Chinese (translation by James Jeng <james@leberger.com.tw>,
    http://www.unicotec.com).




8. REVISION HISTORY
~~~~~~~~~~~~~~~~~~~

Version 1.86 (14-Mar-2005)
    o Added compatibility with Borland Delphi 2005.

Version 1.85 (24-Mar-2003)
    o Added support of Traditional Chinese language. Chinese translation by
      James Jeng <james@leberger.com.tw>, http://www.unicotec.com.

Version 1.84 (14-Oct-2002)
    o Added compatibility with Borland Delphi 7.

Version 1.83 (26-Jun-2002)
    o Added compatibility with Borland C++Builder 6.

Version 1.82 (10-Dec-2001)
    o Add special package ProDsgn60P.dpk for using with Delphi 6
      Personal Edition. This package doesn't contain DB-aware components.
    o Added new TProComboBox component. It's a descendant of
      standard TComboBox component which has ItemIndex property published.
    o Added DefaultFontName routine (ProVCL unit).
    o Bugs fixed:
      - Fixed annoying bug in TIntegerList.Add and TFloatList.Add methods;
      - Fixed syntax error in ProLibDB30.dpk.

Version 1.81 (16-Jul-2001)
    o Added compatibility with Borland Delphi 6.
    o Added ProDBUtl unit (This unit doesn't work in Delphi 1.0).
    o Added C++Builder version of Demo project (tested under C++Builder 5).
      Translation to C++ by Victor Villarrubia <willarrubia@airtel.net>,
      http://www.wbuilder.f2s.es.
    o Added support of Korean language. Korean translation by
      Van Cheju <vancheju@hotmail.com>.
    o Design-time packages ProDclXX.XXX were renamed to ProDsgnXX.XXX.
    o Fixed an application icon bug caused by ProDlg unit.

Version 1.80 (25-Mar-2001)
    o Added TProLabelEdit and TProDBLabelEdit components.
    o Added FontInstalled and FontTahomaInstalled routines (ProVCL unit).
    o MakeInt routine was renamed to MakeSmallInt (ProUtils unit).
    o TProFileStream class (ProUtils unit):
      - Fixed annoying bug in ReadLn method.

Version 1.74 (29-Nov-2000)
    o Fixed problem with showing of TProPasswordDialog and
      TProLoginDialog components in different screen resolutions.

Version 1.73 (27-Nov-2000)
    o New design of forms for TProPasswordDialog and TProLoginDialog
      components.

Version 1.72 (21-Aug-2000)
    o Corrected section "Installing the Pro VCL Extensions Library" in
      this file for Delphi 5 and C++Builder 5.
    o Changed the demo program to illustrate how to use TProConfirmDialog.

Version 1.71 (1-May-2000)
    o Added compatibility with Borland C++Builder 5.

Version 1.70 (3-Apr-2000)
    o All TProXXXDialog components:
      - Added SoundType property. This property allows to play sound
        before showing dialog. Added by Cumhur Semin <freeman35@pmail.net>.
    o Added OpenURL routine (ProVCL unit).

Version 1.62 (8-Jan-2000)
    o Added support of Slovak language. Slovak translation by
      Roland Turcan <rolo@sedas.sk>.

Version 1.61 (8-Nov-1999)
    o Added support of Hungarian language. Hungarian translation by
      Gabor Szucs <szucs_sw@mail.inext.hu>.
    o Added support of Turkish language. Turkish translation by
      Cumhur Semin <freeman35@pmail.net>.

Version 1.60 (4-Oct-1999)
    o Added support of Polish language. Polish translation by
      BofAG <_barrel@friko5.onet.pl>.
    o Added new TProStatusBar component. Unlike standard TStatusBar
      this component can accept other controls.
    o Optimized code of ExchangeBytes, ExchangeWords and ExchangeLongInts
      routines. (Optimized by DSnake).

Version 1.51 (19-July-1999)
    o Added support of Dutch language. Dutch translation by
      Karel Scholten <karel@hesp.demon.nl>.
    o TProCalcDialog component:
      - Fixed old bug which appeared when the result of
        an operation had more than 15 numbers.
        Fixed by Alberto Perez <alberto19@bitmailer.net>;
      - Added emulation of pressing buttons when keyboard is used
        (idea by BofAG).
    o Added new properties TProUrlLabel.HoveredColor and
      TProUrlLabel.CanHover. These properties make TProUrlLabel work
      similar as links in the IE4+ - it can use hover color and
      comes underlined when user moves cursor over the control.
      Added by BofAG <_barrel@friko5.onet.pl>,
      http://friko5.onet.pl/by/_barrel/.

Version 1.50 (4-June-1999)
    o Removed TProToolBar component.
    o Changed the demo program.

Version 1.42 (1-June-1999)
    o Added support of Swedish language. Swedish translation by
      Bengt Nilsson <bengan.nilsson@telia.com>.

Version 1.41 (26-Apr-1999)
    o Bugs fixed.

Version 1.40 (12-Apr-1999)
    o Added compatibility with Borland C++Builder 4.
    o TProPasswordDialog.Password, TProLoginDialog.UserName and
      TProLoginDialog.Password properties are not "readonly" any more.
      Now they can be set to any value before calling Execute method
      to initialize corresponding edit fields in these dialogs.
    o Bugs fixed:
      - TProColorGrid component now is correctly created in runtime;
      - Picture in TProConfirmDialog, TProWarningDialog, TProInfoDialog and
        TProStopDialog components now has a transparent background under
        Delphi 3 and higher and C++Builder 3 and higher.
    o Added support of Italian language. Italian translation by
      Mauro Batini <m.batini@sistinf.it>.

Version 1.31 (10-Mar-1999)
    o Added support of Danish language. Danish translation by
      Claus Svalekjaer <cgsdata@post.tele.dk>,
      http://skyscraper.fortunecity.com/hamilton/840/index.html.

Version 1.30 (10-Jan-1999)
    o New component:
      - TProMouseRegion.
    o New procedures:
      - ShowInteger;
      - ShowFloat.
    o Added new highlight effect and new label styles in TProCustomLabel,
      TProLabel, TProClockLabel, TProUrlLabel and TProDBText components.
    o Run-time package ProLibXX.dpk was separated into two run-time
      packages: ProLibXX.dpk (with all non-data-aware components) and
      ProLibDBXX.dpk (with data-aware components).
    o Since this release the installation program is a 32-bit application.

Version 1.24 (25-Nov-1998)
    o Added support of German language. German translation by
      Wolfgang Matsche <dasawm@cbn.net.id>.
    o Bugs fixed:
      - TProButton now correctly works when Kind = pbkCloseEsp;
      - TProCalcDialog now correctly works when Language = dlSpanish.

Version 1.23 (10-Nov-1998)
    o Added TProCalcDialog.Value property.
    o Added support of Spanish language. Spanish translation by
      IvAn Diniz Collazo <ivan.diniz@correo.nu>.

Version 1.22 (20-Oct-1998)
    o Added support of Portuguese language. Portuguese translation by
      Aldo Caetano <aldo@vipclinic.com>, http://members.xoom.com/intelectus.

Version 1.21 (22-Sep-1998)
    o Bugs fixed.

Version 1.20 (20-Sep-1998)
    o New components:
      - TProUrlLabel;
      - TProUrlImage;
      - TProTiledImage.
    o Added "Test..." item to standard TXXXDialog component editors.
    o Added support of French language. French translation by
      Thierry Boulestin <thierry.boulestin@wanadoo.fr>.
    o Run-time packages ProCmpXX.dpk were renamed to ProLibXX.dpk.
    o Added an installation program.

Version 1.10 (28-Aug-1998)
    o Design-time package ProComps.dpk was separated into run-time
      packages ProCmpXX.dpk and design-time packages ProDsgnXX.dpk.
    o Added "Test..." item to TProXXXDialog component editors.
    o Bugs fixed:
      - TProButton.Kind property now can be set to pbkNextXXX;
      - ProCalc.dfm, ProAbout.dfm, ProTblEd.dfm and demo project now is
        compatibile with Delphi 1.0.
    o In this file corrected the manual of the installation.

Version 1.01 (18-Aug-1998)
    o Bugs fixed.

Version 1.0 (11-Aug-1998)
    o First release.




9. CONTACTING
~~~~~~~~~~~~~

If you have any questions, remarks, suggestions, bugs, requested features
please feel free to contact me. I would appreciate you if you send me
your comments about this software. My e-mail:

    dmitry@barabash.com

Be sure to check my web site

    http://barabash.com

for updates.



Yours sincerely,
Dmitry G. Barabash
