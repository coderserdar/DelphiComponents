Toolbar 2000 AddOn   -    ReadMe
(C) 2007 By Andreas Kniest

Version 1.20

This Package for Delphi 7 is Freeware und may be used in any of your projects (Freeware/Shareware/Commercial), as long as you show my Name in the AboutBox of your project (or any other place where you show your Credits). 

If you like this components, I would be happy over any Donation. Developing them taked time and my family had missed me. ;-) Just contact me via eMail.


What is it:
===========

This Package is an AddOn to the Package "Toolbar2000" by Jordan Russell (www.jrsoftware.org).  It contains three different components and 2 CustomItems für Toolbar2000.

1. TTB2KToolbarList + AutoVisibility List Item (TTBAVListItem)
   - The TTB2KToolbarList is a Helper for the AutoVisibility List Item. The AutoVisibility List Item (used in a Toolbar-Menu for example) creates a List of all closeable Toolbars with the ability to show/hide them. No more forgeting to create an Visibility-Item for your new Toolbar.

2. TTB2KCustomizeDialog + CustomizeDialog Item (TTBCustomizeDialogItem)
   - The CustomizeDialog gives your users the ability to change the Look-N-Feel of your program. You can define, what they are able to change (Toolbars, Menus, Docks, Backgrounds). All with a handy little Dialog like in many other programs (Toolbar -> Customize...).
   - You can select the language (English or German)
   - The CustomizeDialogItem is for use as a menu- or toolbar-Button and starts the Customize-Dialog.
3. TTB2KAutoStateSave
   - This is very cool. Once dropped on the form with your Toolbar2000 and activated (don't forget to set the parameters), it stores and restores the most settings of the Toolbar2000 on creating/destroying the Window. Positions, customizing via the CustomizeDialog, floating, visibility, almost everything.
   - Stores in Inifiles or Registry
   - Procedure to Reset the original settings (must be called via a Button for example). Needs to close and reopen the Application!

Installation:
=============

This Package is for Delphi 7, maybe it also works under newer Versions of Delphi. I can't test this, but you get the sourcecode with this Zip, so you can try.

1. Extract the Zip and copy the files to a Folder you like.
2. In Delphi add this Path to the Library-Path (so Delphi can find the source)
3. Open the TB2KAddOn_Inst.dpk Package and choose compile and then install

You should now have 3 new components in the Toolbar2000-Tab and 2 new Items in the Toolbar2000-Designer.

Historie:
=========

Version 1.20 - Fixed a Bug in TB2KAddOnAutoState.pas: Crashed on closing a Form, if the Name of the Form does not start with tform...

Version 1.10 - First Release on Torry.net

Version 1.00 - Never released


Contact me:
===========
If you have any comments, ideas or problems you can contact me, I try to help you.

mailto: andreas.kniest@arcor.de


Andreas
