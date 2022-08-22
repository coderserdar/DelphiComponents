TB97Editor originally by Jan Erik Christensen (1999-06-02)

TB97Editor revision 2 update by Jordan Russell (2000-05-27):
It now comes with a unit TB97TlbrEdReg.pas, which replaces the modified
TB97Reg.pas unit in the original version.

Description:

Adds a design-time dialog which automates adding standard buttons such as
Open, Save, and Cut to a toolbar, including glyphs and hints. To access this
dialog, right-click the toolbar and select "Add Buttons".

Installation instructions:

Place the TB97TlbrEd*.* files in your Toolbar97 directory. Then open the
Toolbar97 package file, add TB97TlbrEd.pas and TB97TlbrEdReg.pas to it, and
recompile it. Or on Delphi 2 & C++Builder 1, add TB97TlbrEdReg.pas to the
component library via Component | Install.
