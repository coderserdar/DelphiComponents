
This package (DCLTailorWareIDE) is made by Lutz Kutscher
It is tested with Delphi 7
Do with it, whatever you like - use it, change it, extend it, pass it on, delete it...
I won't take responsibility for anything that might happen with it whether good or bad.

Its main purpose is to give a simple way to register your own form class(es) in DXE2, so you can add published properties, editable in the object inspector. TSRegFrmWiz.pas is the file that can help you with that. 
There is a description of how to do it in the beginning of that unit (and a copy of it at the end of this file).

The package contains a unit TSForms.pas, that declares three form classes which inherit from each other, and that are added to the Delphi Object Repository.

Furthermore there is a string/string list Editor dialog (TSStrPropDlg.pas and TSPropEdit.pas).
There is a color editor, whicht enables you to add and use your own color codes (TSColors.pas and TSPropEdit.pas).

!!!!!!!!!!!!!!!
!! ATTENTION !! 
!!!!!!!!!!!!!!!
	There is an issue though with the TColor property editor. Once it is installed into the IDE, recompiling the Package is only possible if you deactivate the option "show compiler progress" in the IDE options. Otherwise you'll get an access violation when you try to compile the package.
When you use the color names defined in TSColors, you have to add TSColors to the uses clause of the form's unit.
If you don't want to use the TSColors and property editor: 
	1) remove TSColors from the package 
	2a) edit TSPropEdit.pas. Remove (or comment) the class TTSColorProperty and it's implementation.
or	2b) rename TSPropEdit.pas to whatever you like and rename "TSPropEdit.pas.nocolor" to TSPropEdit.pas
	3) remove (or comment) the "RegisterPropertyEditor(TypeInfo(TColor)"[...] lines from the Register procedure.

I hope everything is understandable and of good use.
Feel free to contact me (kutscher>at<tailorsoft>dot<de) if you have questions or comments.


Installation:
Open the .dpk with Delphi 7.
Open the project's context menu in the project manager and select "install".
If you don't want to use all property editors in the package simply edit "TSRegisterIDE.pas" and comment or delete those "RegisterPropertyEditor" lines, you don't want.


Lutz Kutscher



Description from the beginning of "TSRegFrmWiz.pas":

{	The purpose of this Unit is to register selfmade form classes (descendants of TForm)
	that introduce new published properties.
	The registered form classes appear in the "Form Classes" tab of the "New Items" dialog.

	Before you register a form to the IDE, you should make sure, that it is bug free.
	Otherwise it could repeatedly crash the IDE.
	To register a Form to the repository you have to add your form's unit and this
	unit (TSRegFrmWiz.pas) to a design time package.
	In the "Register" procedure of your package you add:

	RegisterNewFormClass(<FormClass>, '<FormUnitName>', '<IconResourceName>', '<Comment>', '<Author>');
	whereas the Parameters are:
		<FormClass>: The class of your new form (e.g. TMyNewForm)
		'<FormUnitName>': The name of the unit, in which your new form is saved.
											If your form uses other units you can also add them here (comma separated).
											This string is simply added to the interface uses section.
		<IconResourceName>: You can add an icon file to your package resource
											D7: Open  TSRegFrmWiz.res with the Borland Image Editor
												Add an Icon and name it.
											Add the resource name here to display your own icon in the repository.
											If you don't supply an IconResourceName, the wizard tries to locate
											and icon resouce of the class name.
											
		<Comment>: If you supply a comment,it is shown in the detail view of the repository.
		<Author>: if you enter your name here, it is shown in the detail view of the repository.

		Enjoy,
		Lutz Kutscher
}

Description from the beginning of "TSColors.pas":
{
	How to add your own colors to the IDE:

		1) Add the color constant to the "const" section below
		2) Add the color value and name to the TSColorMap array
		3) Count the number of Items in TSColorMap and set TSColorCount accordingly.

	Remember to add a reference to this unit to every form unit that uses the
	newly defined color codes.
}
