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

unit ukwdClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, DsgnIntf, ToolIntf, Registry, uksydUtils, uksydClasses, ukwClasses;

type

	EKWDClasses = class( EKDWinAPI );

{
--------------------------------------------------------------------------------
----------------------------- TKPerformanceObjects -----------------------------
--------------------------------------------------------------------------------
}

{ TKW95PerformanceStrReadOnly }

	TKW95PerformanceStrReadOnly = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKW95PerformanceBoolReadOnly }

	TKW95PerformanceBoolReadOnly = class( TBoolProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKW95PerformanceProperty }

	TKW95PerformanceProperty = class( TKStringsProperty )
	private
		function GetOwner: TKW95PerformanceItem;
		function GetRegistry: TRegistry;

	protected
		procedure GetValueList( List: TStrings ); override;

		function GetBaseKeyValue: string; virtual; abstract;

	public
		property Owner: TKW95PerformanceItem
						 read GetOwner;
		property Registry: TRegistry
						 read GetRegistry;

	end;

{ TKObjectNameProperty }

	TKObjectNameProperty = class( TKW95PerformanceProperty )
	{$IFDEF DELPHI4}
	protected
	{$ELSE}
	private
	{$ENDIF}
		function GetBaseKeyValue: string; override;

	end;

{ TKCounterNameProperty }

	TKCounterNameProperty = class( TKW95PerformanceProperty )
	{$IFDEF DELPHI4}
	protected
	{$ELSE}
	private
	{$ENDIF}
		function GetBaseKeyValue: string; override;

	end;


{##NI##}


{
---------------------------------------------------------------------------------
--------------------------- Property Editor Support -----------------------------
---------------------------------------------------------------------------------
}

{$IFNDEF EXCLUDED_CLASSES}

{$IFDEF DELPHI3}

{ TKActivePageProperty }

	TKActivePageProperty = class( TKCompNameProperty )
	protected
		function GetFilterClass: TComponentClass; override;
		function ProcessValue( GetComp_i: TPersistent; BaseComp: TComponent;
			const Value: string ): Boolean; override;

	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{$ENDIF}

{$ENDIF}

{##NI##}

{ TKMachineNameProperty }

	TKMachineNameProperty = class( TKDirectoryProperty )
	protected
		function GetRoot: string; override;

	end;

{ TKDomainProperty }

	TKDomainProperty = class( TKStringsProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function IsReadOnly: Boolean; override;

	public
		function GetValue: string; override;

	end;	

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{##NI##}

{$IFNDEF EXCLUDED_CLASSES}

{$IFDEF DELPHI3}

{ TKPageControlEditor }

	TKPageControlEditor = class( TDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{$ENDIF}

{$ENDIF}

{##NI##}

{ TKWinAPIDialogEditor }

	TKWinAPIDialogEditor = class( TKDefaultEditor )
	public
		procedure ExecuteVerb(Index : Integer); override;
		function GetVerb(Index : Integer): string; override;
		function GetVerbCount : Integer; override;

	end;

{ TKPageSetupDialogEditor }

	TKPageSetupDialogEditor = class( TKWinAPIDialogEditor )
	public
		procedure Edit; override;

	end;

{ TKBrowseFolderDialogEditor }

	TKBrowseFolderDialogEditor = class( TKWinAPIDialogEditor )
	public
		procedure Edit; override;

	end;

{ TKNetworkDialogEditor }

	TKNetworkDialogEditor = class( TKWinAPIDialogEditor )
	public
		procedure Edit; override;

	end;

{ TKShellComboBoxEditor }

	TKShellComboBoxEditor = class( TDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKShellListBoxEditor }

	TKShellListBoxEditor = class( TDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

implementation

uses
	Windows, TypInfo, ExptIntf, uksyUtils, ukwConsts, ukwResStr, ukwUtils, ukwCtrls, 
	ukwdConsts;

{
---------------------------------------------------------------------------------
--------------------------- Property Editor Support -----------------------------
---------------------------------------------------------------------------------
}

{---------------------------- TKPerformanceObjects -----------------------------}

type

	TKPerformanceObjectsHack = class( TKPerformanceObjects );

{ TKW95PerformanceStrReadOnly }

function TKW95PerformanceStrReadOnly.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paReadOnly];
end;

{ TKW95PerformanceBoolReadOnly }

function TKW95PerformanceBoolReadOnly.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paReadOnly];
end;

{ TKW95PerformanceProperty }

procedure TKW95PerformanceProperty.GetValueList( List: TStrings );
begin
	if ( not CheckTrimStr( GetBaseKeyValue ) ) then
		Exit;
	Registry.RootKey := HKEY_LOCAL_MACHINE;
	if ( not Registry.OpenKey( GetBaseKeyValue, false ) ) then
		RaiseExceptionFmt( EKW95Performance, sErrW95InvBaseKey, [GetBaseKeyValue] );
	try
		Registry.GetKeyNames( List );
	finally
		Registry.CloseKey;
	end;
end;

function TKW95PerformanceProperty.GetOwner: TKW95PerformanceItem;
begin
	Result := ( GetComponent( 0 ) as TKW95PerformanceItem );
end;

function TKW95PerformanceProperty.GetRegistry: TRegistry;
begin
	Result := TKPerformanceObjectsHack( Owner.Owner.Component ).Registry;
end;

{ TKObjectNameProperty }

function TKObjectNameProperty.GetBaseKeyValue: string;
begin
	Result := sW95PerfObjKey;
end;

{ TKCounterNameProperty }

function TKCounterNameProperty.GetBaseKeyValue: string;
begin
	if CheckTrimStr( Owner.ObjectName ) then
		Result := sW95PerfObjKey + '\' + Owner.ObjectName
	else
		Result := '';
end;

{$IFNDEF EXCLUDED_CLASSES}

{$IFDEF DELPHI3}

{ TKActivePageProperty }

function TKActivePageProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList];
end;

function TKActivePageProperty.GetFilterClass: TComponentClass;
begin
	Result := TKTabSheet;
end;

function TKActivePageProperty.ProcessValue( GetComp_i: TPersistent;
	BaseComp: TComponent; const Value: string ): Boolean;
begin
	Result := ( ( BaseComp as TKTabSheet ).PageControl = GetComponent( 0 ) );
end;

{$ENDIF}

{$ENDIF}

{ TKMachineNameProperty }

function TKMachineNameProperty.GetRoot: string;
begin
	Result := inherited GetRoot; { Result := 'Ambiente de Rede'; }
end;

{ TKDomainProperty }

type
  TKMailSlotHack = class( TKMailSlot );

function TKDomainProperty.IsReadOnly: Boolean;
begin
	Result := inherited IsReadOnly; { can be readonly... }
end;

procedure TKDomainProperty.GetValueList( List: TStrings );
begin
	uksyUtils.GetDomainNames( List );
	List.Add( MAILSLOT_DEFAULT_DOMAIN_BROADCAST );
	TKMailSlotHack( ( GetComponent( 0 ) as TKMailSlot ) ).Domains.Assign( List );
end;

function TKDomainProperty.GetValue: string;
begin
	if CheckStrEqual( GetStrValue, MAILSLOT_DEFAULT_DOMAIN_BROADCAST ) then
		Result := sDMSDefaultDomain
	else
		Result := inherited GetValue;
end;

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{$IFNDEF EXCLUDED_CLASSES}

{$IFDEF DELPHI3}

{ TKPageControlEditor }

procedure TKPageControlEditor.ExecuteVerb( Index: Integer );
var
	Page: TKTabSheet;
	Designer: TKFormDesigner;
	PageControl: TKPageControl;
begin
	if CheckObjectClass( Component, TKTabSheet ) then
		PageControl := TKTabSheet( Component ).PageControl
	else
		PageControl := TKPageControl( Component );
	if CheckObject( PageControl ) then
	begin
		Designer := Self.Designer;
		if ( Index = 0 ) then
		begin
			Page := TKTabSheet.Create( Designer.Form );
			try
				Page.Name := Designer.UniqueName( TKTabSheet.ClassName );
				Page.Parent := PageControl;
				Page.PageControl := PageControl;
			except
				Page.Free;
				raise;
			end;
			PageControl.ActivePage := Page;
			Designer.SelectComponent( Page );
			Designer.Modified;
		end
		else
		begin
			Page := PageControl.FindNextPage( PageControl.ActivePage, ( Index = 1 ), False );
			if CheckObject( Page ) and ( Page <> PageControl.ActivePage ) then
			begin
				PageControl.ActivePage := Page;
				if CheckObjectClass( Component, TKTabSheet ) then
				  Designer.SelectComponent( Page );
				Designer.Modified;
			end;
		end;
	end;
end;

function TKPageControlEditor.GetVerb( Index: Integer ): string;
begin
	Result := PAGE_CONTROL_VERB[Index];
end;

function TKPageControlEditor.GetVerbCount: Integer;
begin
	Result := PAGE_CONTROL_VERBCOUNT;
end;

{$ENDIF}

{$ENDIF}

{ TKWinAPIDialogEditor }

procedure TKWinAPIDialogEditor.ExecuteVerb(Index: Integer);
begin
	Edit;
end;

function TKWinAPIDialogEditor.GetVerb( Index: Integer ): string;
begin
	Result := sWAPIDlgEditorTest;
end;

function TKWinAPIDialogEditor.GetVerbCount: Integer;
begin
	Result := WINAPI_DLGEDITOR_VERBCOUNT;
end;

{ TKPageSetupDialogEditor }

procedure TKPageSetupDialogEditor.Edit;
begin
	if ( CheckObjectClass( Component, TKPageSetupDialog ) and
			 TKPageSetupDialog( Component ).Execute ) then
		Designer.Modified;
end;

{ TKBrowseFolderDialogEditor }

procedure TKBrowseFolderDialogEditor.Edit;
begin
	if ( CheckObjectClass( Component, TKBrowseFolderDialog ) and
			 TKBrowseFolderDialog( Component ).Execute ) then
			Designer.Modified;
end;

{ TKNetworkDialogEditor }

procedure TKNetworkDialogEditor.Edit;
begin
	if ( CheckObjectClass( Component, TKNetworkDialog ) and
			 TKNetworkDialog( Component ).Execute ) then
		Designer.Modified;
end;

{ TKShellComboBoxEditor }

procedure TKShellComboBoxEditor.ExecuteVerb( Index: Integer );
var
	s: string;
begin
	case Index of
		0: if ShellBrowseFolder( sShellCaption, '', s ) then
				( Component as TKShellComboBox ).AddPath( s );
		1: ( Component as TKShellComboBox ).Clear( True );
	end;
	Designer.Modified;
end;

function TKShellComboBoxEditor.GetVerb( Index: Integer ): string;
begin
	Result := '';
	case Index of
		0: Result := sShellAddPath;
		1: Result := sShellClear;
	end;
end;

function TKShellComboBoxEditor.GetVerbCount: Integer;
begin
  Result := SHELL_CONTROLS_VERB_COUNT;
end;

{ TKShellListBoxEditor }

procedure TKShellListBoxEditor.ExecuteVerb( Index: Integer );
var
	s: string;
begin
	case Index of
		0: if ShellBrowseFolder( sShellCaption, '', s ) then
				( Component as TKShellListBox ).AddPath( s );
		1: ( Component as TKShellListBox ).Clear( True );
	end;
	Designer.Modified;
end;

function TKShellListBoxEditor.GetVerb( Index: Integer ): string;
begin
	Result := '';
	case Index of
		0: Result := sShellAddPath;
		1: Result := sShellClear;
	end;
end;

function TKShellListBoxEditor.GetVerbCount: Integer;
begin
	Result := SHELL_CONTROLS_VERB_COUNT;
end;

end.
