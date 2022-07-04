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

unit uksydReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	SysUtils, Classes, Graphics, Forms, Menus, Controls, StdCtrls, DsgnIntf,
	ExptIntf, {$IFDEF DEBUG} ToolIntf, ExtCtrls, Dialogs, {$IFDEF DSGN_SHAREWARE_TESTING}
	uksydUtils, {$ENDIF} {$ENDIF} uksyShortCuts, uksyUtils, uksyClasses,
	uksydConsts, uksydMComments, uksydClasses;

{$IFDEF DEBUG}
type

	TKIHelpMC = class( TKIModuleCreator )
	protected
		function GetUsesList: string; override;
		
	end;

	TKIPanelMC = class( TKIHelpMC )
	protected
		function GetCustomModuleClassName: string; override;

	end;

	TKIFDlgMC = class( TKIHelpMC )
	protected
		function GetCustomModuleClassName: string; override;

	end;

	TKITestExpert = class( TKIAddInExpert )
	public
		procedure Initialize; override;
		procedure MenuClick( Sender: TIMenuItemIntf ); override;

	end;

function TKIHelpMC.GetUsesList: string;
begin
	Result := inherited GetUsesList;
	Insert( ', ExtCtrls', Result, Pos( ';', Result ) );
end;

function TKIPanelMC.GetCustomModuleClassName: string;
begin
	Result := TPanel.ClassNAme;
end;

function TKIFDlgMC.GetCustomModuleClassName: string;
begin
	Result := TFindDialog.ClassName;
end;

procedure TKITestExpert.Initialize;
begin
  inherited Initialize;
	Name := 'miTeste';
	InstallExpert( iaAfter, 'ToolsToolsItem', 'Teste', 'miTeste', '', 0, 0, 0,
		[mfEnabled, mfVisible] );
end;

procedure TKITestExpert.MenuClick( Sender: TIMenuItemIntf );
begin
	UniqueDebugLogSnapshot( 'c:\mc.dbg' );
	Inform( 'Sucessiful Logged!' );
end;

{$IFDEF DSGN_SHAREWARE_TESTING}

type
	TKHexaProperty2 = class( TKHexaProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure Edit; override;

	end;

function TKHexaProperty2.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

procedure TKHexaProperty2.Edit;	
begin
	TestSystemDsgnShareWareVersion;
end;

{$ENDIF}

{$ENDIF}

procedure Register;
begin

{ Experts }
	RegisterLibraryExpert( TKIMethodExpert.Create );

{ property Editors }
	RegisterPropertyEditor( TypeInfo( TColor ), nil, '', TKColorProperty );
	RegisterPropertyEditor( TypeInfo( TColor ), TPersistent, 'Color', TKColorProperty );
	RegisterPropertyEditor( TypeInfo( TColor ), TControl, 'Color', TKColorProperty );
	RegisterPropertyEditor( TypeInfo( TKThreadPriority ), nil, '', TKThreadPriorityProperty );

	RegisterPropertyEditor( TKGradient.ClassInfo, nil, '', TKCustomGradientProperty );
	RegisterPropertyEditor( TypeInfo( TKGradientStyle ), nil, '', TKGradientStyleProperty );

	RegisterPropertyEditor( TypeInfo( TShortCut ), TPersistent, '', TKShortCutProperty );
{

	RegisterPropertyEditor( TypeInfo( TDateTime ), TPersistent, '', TKDateTimeProperty );
	RegisterPropertyEditor( TypeInfo( Double ), TPersistent, '', TKFloatProperty );
	RegisterPropertyEditor( TypeInfo( Single ), TPersistent, '', TKFloatProperty );
	RegisterPropertyEditor( TypeInfo( Extended ), TPersistent, '', TKFloatProperty );

	RegisterPropertyEditor( TypeInfo( Integer ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( Cardinal ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( Word ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( Byte ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( SmallInt ), TPersistent, '', TKIntegerProperty );
	RegisterPropertyEditor( TypeInfo( ShortInt ), TPersistent, '', TKIntegerProperty );
}

{$IFDEF DEBUG}

{$IFDEF DSGN_SHAREWARE_TESTING}
	RegisterPropertyEditor( TypeInfo( LongInt ), TComponent, 'Tag', TKHexaProperty2 );
{$ELSE}
	RegisterPropertyEditor( TypeInfo( LongInt ), TComponent, 'Tag', TKHexaProperty );
{$ENDIF}

{$ENDIF}

	RegisterPropertyEditor( TypeInfo( Single ), TKCustomMeasures, '', TKMMFloatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TPersistent, 'FileName', TKFileNameProperty );
	RegisterPropertyEditor( TypeInfo( TFileName ), TPersistent, 'FileName', TKFileNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TPersistent, 'Directory', TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( TFileName ), TPersistent, 'Directory', TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TPersistent, 'InitialDir', TKDirectoryProperty );

	RegisterPropertyEditor( TypeInfo( TCaption ), TControl, 'Caption', TKLongCaptionProperty );
	RegisterPropertyEditor( TypeInfo( string ), TControl, 'Text', TKLongCaptionProperty );
	RegisterPropertyEditor( TypeInfo( string ), TControl, 'Hint', TKLongStringProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomCollectionItem, 'Name',
		TComponentNameProperty );

{$IFDEF DEBUG}
	RegisterPropertyEditor( TypeInfo( string ), TOpenDialog, 'DefaultExt', TKLongStringProperty );
	RegisterCustomModule( TPanel, TKCustomModule );
	RegisterCustomModule( TFindDialog, TKCustomModule );
	RegisterLibraryExpert( TKIFormExpert.Create( TKIPanelMC ) );
	RegisterLibraryExpert( TKIFormExpert.Create( TKIFDlgMC ) );
	RegisterLibraryExpert( TKITestExpert.Create );
{$ENDIF}

{ ShortCut Constants }
	RegisterShortCuts( DEFAULT_DELPHI_SHORTCUTS );
	RegisterShortCuts( DEFAULT_KNOWHOW_SHORTCUTS );

{ Method Comments }

{--------------------------- CustomMeasures Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKGetMeasuresEvent ), TKCustomMeasures, 'OnGetMeasures',
		sComGetMeasures );
	RegisterDefaultMethod( TypeInfo( TKSetMeasuresEvent ), TKCustomMeasures, 'OnSetMeasures',
		sComSetMeasures );

{ There are no events defined for System package! }

	if IsSystem_Shareware then
	begin
		RegisterMethodShareWare( [
			TypeInfo( TNotifyEvent ), TypeInfo( THelpEvent ), 						 { from Classes  }

			TypeInfo( TProgressEvent ),                                    { from Graphics }

			TypeInfo( TKeyEvent ), TypeInfo( TKeyPressEvent ),             { from Controls }
			TypeInfo( TMouseEvent ), TypeInfo( TMouseMoveEvent ),
			TypeInfo( TDragOverEvent ), TypeInfo( TDragDropEvent ),
			TypeInfo( TStartDragEvent ), TypeInfo( TEndDragEvent ),

			TypeInfo( TCloseEvent ), TypeInfo( TCloseQueryEvent ),         { from Forms    }

			TypeInfo( TDrawItemEvent ), TypeInfo( TMeasureItemEvent ),     { from StdCtrls }
			TypeInfo( TScrollEvent )] );
	end;

end;

end.
