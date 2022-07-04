
               Firesoft - ExportSuite
               ======================

Index
-----

  Copyrigth
  License
  Register
  History
  Installation
  Files List



Copyrigth
---------

  Copyright (C) 1997-2006 
  Federico Firenze,
  Buenos Aires, Argentina
  webmaster@delphi.com.ar


License
-------

  This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


Register
--------

  The register of this library, is totaly free. If your register your copy
you should recive information about changes and new version of the software
at you mailbox. To register the libray send a e-mail to webmaster@delphi.com.ar 
with the subject: "Register: ExpSuite".


History
-------

      1999 - First version of the library.

23/05/2003 - Added the component TDataToWK1 to write Lotus 123 files
           - Optimization of Biff write library.

17/06/2003 - Added the componente TDataToHTML.
           - Added in the property editor the context menu "Test to File..."

25/02/2006 - Free the code with GNU public license.


Installation
------------

  Use "File\Open..." menu item of Delphi IDE to open the runtime
package RunExpSuitD?.dpk. In "Package..." window click "Compile" button to
compile package. Put compiled BPL files into directory that is accessible 
through the search PATH.

  After compiling run-time packages you must install design-time packages 
into the IDE.

  Use "File\Open..." menu item to open the design time package 
DesExpSuitD?.dpk. In "Package..." window click "Compile" button to compile 
the package and then click "Install" button to register the Library 
components on the component palette. 


Files List
----------

  RunTime Units
  -------------
    DataExport.pas     Abstract classess to export DataSets 
    DataToAscii.pas    Classes and functions to export DataSets in ASCII mode.
    DataToDBF.pas      Classes and functions to export DataSets into DBase files.
    DataToHTML.pas     Classes and functions to export DataSets into HTML files. 
    BiffFile.pas       Generic class to write BIFF5 files (Old Excel version)  
    DataToXLS.pas      Classes and functions to export DataSets into Excel files. 
    WKFile.pas         Generic class to write WK1 files.
    DataToWK1.pas      Classes and functions to export DataSets into Lotus123 files. 
    ExpDlg.pas         INCLOMPLETE, standard Dialog to export all posible type´s.

  Desing Time Units
  -----------------
    RegExpSuite.pas    Property Editor´s and register procedures
    unClassLister.pas  Dialog to find a component by class, for example TDBGrid when
                       in design time the user right click over the any TDataExport 
                       and choose the menu "Retrive TDBColumns..."
_