{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit uksydConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	ToolIntf, TypInfo, {$IFDEF DELPHI4}Classes{$ELSE}Menus{$ENDIF}, uksyShortCuts,
	uksyUtils, uksyConsts;
	{##NI##}{ do not remove! force uksyUtils to be registered in unit running list }{##NI##}

{
--------------------------------------------------------------------------------
------------------------------- Generic constants ------------------------------
--------------------------------------------------------------------------------
}

const
	DEFUALT_HEXA_PROPERTY_DIGITS  = $8;
	DEFAULT_HEXA_PROPERTY_PATTERN = '$%.*x';

{$IFDEF DELPHI3}
	DELPHI_PROPEDT_REGKEY = 'Software\Borland\Delphi\3.0\Property Editors';
{$ENDIF}

	METHOD_COMMENT_PATTERN = '{'#13#10'%s'#13#10'}'#13#10;
	METHOD_START_POINT = 'begin';

	REPOSITORY_PAGES_SECTION = 'Repository Pages';
	CODE_INSIGHT_TEMPLATE_PATTERN = #13#10'[%s | %s]'#13#10'%s'#13#10;

	DEFAULT_MODULECREATOR_USESLIS =
		'  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,' + CH_CRLF +
		'  uksyUtils;';

{
--------------------------------------------------------------------------------
------------------------------ Shortcut constants ------------------------------
--------------------------------------------------------------------------------
}

	DEFAULT_DELPHI_SHORTCUTS: array[0..82] of TShortCut =
	(
		SC_NULL,
		SC_CTRL_A,
		SC_CTRL_B,
		SC_CTRL_C,
		SC_CTRL_D,
		SC_CTRL_E,
		SC_CTRL_F,
		SC_CTRL_G,
		SC_CTRL_H,
		SC_CTRL_I,
		SC_CTRL_J,
		SC_CTRL_K,
		SC_CTRL_L,
		SC_CTRL_M,
		SC_CTRL_N,
		SC_CTRL_O,
		SC_CTRL_P,
		SC_CTRL_Q,
		SC_CTRL_R,
		SC_CTRL_S,
		SC_CTRL_T,
		SC_CTRL_U,
		SC_CTRL_V,
		SC_CTRL_W,
		SC_CTRL_X,
		SC_CTRL_Y,
		SC_CTRL_Z,
		SC_F1,
		SC_F2,
		SC_F3,
		SC_F4,
		SC_F5,
		SC_F6,
		SC_F7,
		SC_F8,
		SC_F9,
		SC_F10,
		SC_F11,
		SC_F12,
		SC_CTRL_F1,
		SC_CTRL_F2,
		SC_CTRL_F3,
		SC_CTRL_F4,
		SC_CTRL_F5,
		SC_CTRL_F6,
		SC_CTRL_F7,
		SC_CTRL_F8,
		SC_CTRL_F9,
		SC_CTRL_F10,
		SC_CTRL_F11,
		SC_CTRL_F12,
		SC_SHIFT_F1,
		SC_SHIFT_F2,
		SC_SHIFT_F3,
		SC_SHIFT_F4,
		SC_SHIFT_F5,
		SC_SHIFT_F6,
		SC_SHIFT_F7,
		SC_SHIFT_F8,
		SC_SHIFT_F9,
		SC_SHIFT_F10,
		SC_SHIFT_F11,
		SC_SHIFT_F12,
		SC_CTRL_SHIFT_F1,
		SC_CTRL_SHIFT_F2,
		SC_CTRL_SHIFT_F3,
		SC_CTRL_SHIFT_F4,
		SC_CTRL_SHIFT_F5,
		SC_CTRL_SHIFT_F6,
		SC_CTRL_SHIFT_F7,
		SC_CTRL_SHIFT_F8,
		SC_CTRL_SHIFT_F9,
		SC_CTRL_SHIFT_F10,
		SC_CTRL_SHIFT_F11,
		SC_CTRL_SHIFT_F12,
		SC_INSERT,
		SC_SHIFT_INSERT,
		SC_CTRL_INSERT,
		SC_DELETE,
		SC_SHIFT_DELETE,
		SC_CTRL_DELETE,
		SC_ALT_BACKSPACE,
		SC_SHIFT_BACKSPACE
	);

	DEFAULT_KNOWHOW_SHORTCUTS: array[0..1] of TShortCut =
	(
		SC_ESCAPE,
		SC_CTRL_RETURN
	);

{
--------------------------------------------------------------------------------
--------------------------- Open Tools API constants ---------------------------
--------------------------------------------------------------------------------
}

	DEFAULT_MODULE_CREATION_FLAGS = [cmAddToProject, cmShowSource, cmShowForm,
		cmMarkModified, cmUnNamed];
	DEFAULT_PROJECT_CREATION_FLAGS = [cpCustom];

	VALID_VFS_PROP_TYPEKINDS = [
		tkInteger, tkChar, tkEnumeration, tkClass, tkSet, tkWChar, { Ordinal }
		tkLString, tkWString, tkString,														 { String  }
		tkFloat];                                                  { Float   }

{##NI##}
{
--------------------------------------------------------------------------------
--------------------------- Form Designers constants ---------------------------
--------------------------------------------------------------------------------
}

{

	CUSTOM MODULES ARCHITECTURE
	--------------------------


	FOR ALL COMPONENT CUSTOMMODULES, THE STRUCTURE OF THE DESIGNER.FORM IS:

	ClassName -> 'TDataModuleForm'
	Name 			-> 'DataModuleForm'
		( either for TDataModule descendant or not - is TComponent descendent )

	Components
	----------

	Components[0].ClassName -> 'TComponentContainer' (TScrollBox descendent...)
	Components[0].Name 			-> ''

	Components[1].ClassName -> CustomModule.Root.ClassName
	Components[1].Name 			-> Default name assigned by Designer.UniqueName

PS:

	If you put any component/control into the FormDesigner, it will be Owned by the
	CustomModule.Root component not by the Designer.Form

	------------------------------------------------------------------------------

	FOR ALL WINCONTROL CUSTOMMODULES (EXCEPT TCUSTOMFORM CUSTOMMODULES),
	THE STRUCTURE OF THE DESIGNER.FORM IS:

	ClassName -> 'TWinControlForm'
	Name 			-> 'WinControlForm'
		( for any TWinControl Descendent. eg. TQuickReport, TPanel, etc... )

	Components
	----------

	Components[0].ClassName -> 'TShape'
	Components[0].Name 			-> 'WidthHandle'

	Components[1].ClassName -> 'TShape'
	Components[1].Name 			-> 'SizeHandle'

	Components[2].ClassName -> 'TShape'
	Components[2].Name 			-> 'HeightHandle'

	Components[1].ClassName -> CustomModule.Root.ClassName
	Components[1].Name 			-> Default name assigned by Designer.UniqueName

PS:

	If you put any component/control into the FormDesigner, it will be Owned by the
	CustomModule.Root component not by the Designer.Form

	------------------------------------------------------------------------------

	FOR ALL TCUSTOMFORM CUSTOMMODULES, THE STRUCTURE OF THE DESIGNER.FORM IS:

	ClassName -> CustomModule.Root.ClassName (eg. TForm1)
	Name 			-> Default name assigned by Designer.UniqueName (Form1)
		( for any TCustomForm Descendent - especial/default situation... )

	Components
	----------

	There are no components initialy.

PS:

	If you put any component/control into the FormDesigner, it will be Owned by the
	CustomModule.Root component, IN THAT CASE IS THE DESIGNER.FORM

}

	COMPONENT_DESINGER_CLASSNAME  = 'TDataModuleForm';
	WINCONTROL_DESIGNER_CLASSNAME = 'TWinControlForm';
{##NI##}

{
--------------------------------------------------------------------------------
-------------------------------- External Use ----------------------------------
--------------------------------------------------------------------------------
}

resourcestring

{---------------------------------- uksydUtils ---------------------------------}

	sDelphiDRO = 'DELPHI32.DRO';
	sDelphiDCI = 'DELPHI32.DCI';

	sDefDlgFilter = 'All Files(*.*)|*.*';
	sDefDlgTitle  = 'Open File';

	sSelRegComp = 'Registered Components';
	sCompClass = 'Component Class';

	sSelUnit = 'Select Unit';
	sSelUnitName = 'Unit Name';

  sErrInvFormName = 'Cannot retrieve the ModuleInterface for the form named "%s"';
	sErrInvModuleName = 'Cannot retrieve the ModuleInterface for the module named "%s"';
	sErrInvFormIntf = 'Could not retrieve the form interface for the selected module';
	sErrInvCompIntf = 'Could not create component "%s" of class "%s"';

	sErrEdtIntfInvWriter    = 'Cannot create an undoalbe editor writer';
	sErrEdtIntfInvViewNum   = 'Invalid editor view number ("%d")';
	sErrEdtIntfInvBlockType = 'Invalid editor block type ("%s")';
	sErrEdtIntfInvFileName  = 'Invalid module interface file name ("%s")';
	sErrEdtIntfFormSelected	= 'Invalid editor interface: an editor code window must be selected first';
  sErrEdtIntfEditorSelected = 'Invalid form interface interface: a form designer window must be selected first';

	sErrPropEdtInvPropCount = 'Invalid property count "%d" to instantiate a property editor';

	sErrMenuIntfNotFound     = 'IDE menu item "%s" with caption "%s" not found';
	sErrMainMenuIntfNotFound = 'Cannot get the IDE main menu';
	sErrMenuIntfNullParent   = 'No parent menu was found for the IDE menu item "%s"';

	sErrModIntfSave = 'An error occoured while trying to save "%s"';

	sErrInvProjInfoParams = 'Invalid GetProjectInfo parameters: at least one of the lists must be assigned';
	sErrInvProjInfoEnum = 'Unexpected error while enumerating the project units. Cannot retrieve project information';

	{$IFNDEF DELPHI3}
	sErrInvCodeInsightAdd = 'Cannot add a code insight template for Delphi 1/2';
	{$ENDIF}

{--------------------------------- uksydClasses --------------------------------}

	sKnowHow_IDEConfig = 'IDEConfig';
	sIDEEnableComment  = 'EnableComment';

	sErrInvStreamWrite = 'Invalid stream operation: attempt to write to readonly stream';
	sErrInvStreamRead = 'Invalid stream operation: attempt to read from writeonly stream';
	sErrInvStreamSeekOrigin = 'Invalid stream seek origin %d';

	sErrMethodPropInvMehotName = 'Invalid method name "%s" for automatic comment';
	sErrMethodPropInvStartPoint = 'Invalid method start point for methodname "%s"';

	sMethodPropDefaultComment = 'Default Knowhow® Method Comment...';

{##NI##}
	sMethodPropShareWareComment = 'SHAREWARE VERSION - Knowhow® Library Method Comment - SHAREWARE VERSION';
	sExpertsShareWare = '{ SHAREWARE VERSION - Knowhow® Library Project/Module Expert - SHAREWARE VERSION }';
{##NI##}

	sErrAddInExptInvMenuItem = 'Cannot install an add-in expert previously installed.';

  sMethodExptName 				   = 'KnowHow Method Expert';
	sMethodExptTargetMenuPoint = 'ToolsOptionsItem';
	sMethodExptMenuText        = 'Comment &Methods';
	sMethodExptMenuName        = 'ToolsCommentMethodsItem';
	sMethodExptMenuHint        = 'Enabled the automatic method comment';

	sCommentName = 'KnowHow Comment Wizard';
	sCommentIDString = 'KnowHow.CommentExpert';
	sCommentMenuText = 'Insert Co&mment';
	sCommentMenuName = 'EditInsertCommentItem';
	sCommentTargetMenuPoint = 'EditDeleteItem';
	sCommentSucessiful = 'Block sucessiful commented...';
	sCannotComment = 'Cannot comment this block. Block already commented';
  
  sKnowHowName = 'KnowHow';
	sKnowHowPage = 'KnowHow';
	sAuthors = 'Demian Lessa and Leonardo Freitas';

	sErrVFSReg = 'Could not register the virtual file system "%s"';
	sErrVFSUnReg = 'Virtual file system list was cleared with at least one failure at virtual file system index "%d"';
	sErrVFSStreamMode = 'The virtual file system "%s" does not support file mode "%.8x"';
	sErrVFSRegModule = 'Could not register the module "%s" for virtual file system "%s"';
	sErrInvVFSMultiSelect = 'Cannot show code editor window for multi select property';
	sErrVFSModAddInRegError = 'An unexpected error occoured while trying to install the Add-In notifier for KnowHow virtual file systems';
	sErrVFSModAddInUnRegError = 'An unexpected error occoured while trying to uninstall the Add-In notifier for KnowHow virtual file systems';
	sErrVFSModAddInUnRegModError = 'An unexpected error occoured while trying to uninstall the module "%s" for the Add-In notifier of the KnowHow virtual file systems';
	sErrVFSModuleUnReg = 'Cannot unregister the module "%s" for virtual file system named "%s"';
	sErrVFSRegModIntfPropInfo = 'sErrVFSRegModIntfPropInfo => %s %s %s';
	sErrVFSSearchModGetVFS = 'Could not get the file system for the module interface "%s"';

	sErrInvDateTimePart = 'Invalid %s value: value must be between %d and %d';

	sPropNameMeasurements = 'Measurements';
	sPropNameOwner = 'Owner';

	sErrInvBufferViewCount = 'Could not position the buffer in a multiple view environment. Only one Code editor view should be opened';
	sErrCouldNotOpenFileType = 'Could not open the file "%s": either the file does not exists, or is a project file, or could not be opened in the IDE. Only units, packages and include files are allowed';

implementation

end.
