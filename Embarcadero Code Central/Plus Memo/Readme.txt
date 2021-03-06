    Welcome to the PlusMemo Package
                by
      Electro-Concept Mauricie

Installation Instructions
-------------------------

To install this component set for design time access in Delphi/CBuilder, add
file PMemo6Packx.bpl to your package list (x is the Delphi version you're using,
see next section).

To do that, follow normal package installation procedure, repeated here for
your convenience:
- click menu Components/Install Packages...
- click button Add...
- browse to PMemo6Packx.bpl, click Open (x is your Delphi/CBuilder version)
- click OK

You should have a new palette page, called "PlusMemo" with TPlusMemo
and accessory components on it.

You may need to add the directory where PlusMemo is located to your
library path.  If Delphi complains that it does not find PlusMemo.dcu
at compile time, it means it is not on the library path.
In that case, go to Tools/Environment Options..., select tab "Library"
and enter the PlusMemo directory to your library path.

Package naming convention
-------------------------   

This is the corresondamce between package name and Delphi/CBuilder version:
PMemo6Pack4.bpl    Delphi 4
PMemo6Pack5.bpl    Delphi 5
PMemo6Pack6.bpl    Delphi 6
PMemo6Pack7.bpl    Delphi 7
PMemo6Pack9.bpl    Delphi 2005

PMemo6Packc4.bpl   CBuilder 4
PMemo6Packc5.bpl   CBuilder 5
PMemo6Packc6.bpl   CBuilder 6


Data aware version Installation Instructions
--------------------------------------------

Design time packages from Electro-Concept Mauricie do not include the
data aware version of PlusMemo, TDBPlusMemo, because not all Delphi
editions have database support.

To add TDBPlusMemo to your component palette, add files Plusdb.pas and
Plusdb.dcr to a design time package of your choice.  These files are included
in zip archives you downloaded.  We recommend not using the PlusMemo package
itself, otherwise you will have to reinstall it every time you update your
PlusMemo version.  It is best put it to a new package or another one where
you use to put third party components.


Help Installation Instructions
------------------------------

If you want to have access to PlusMemo's help from within Delphi/CBuilder,
follow normal help installation procedure, which is given here for your
convenience:

- click menu Help/Customize...  This brings you in the OpenHelp editor.
- select tab "Contents"
- click Edit/Add Files...
- browse to PlusMemo.cnt, click Open
- select tab "Index"
- click Edit/Add Files...
- browse to PlusMemo.hlp, click Open
- select tab "Link"
- click Edit/Add Files...
- browse to PlusMemo.hlp, click Open

- click File/Save project
- close the OpenHelp editor.

Under Delphi6/7 and CBuilder6, also copy file PlusMemo.ALS to your Delphi help directory (typically "c:\Program Files\Borland\Delphix\Help").
