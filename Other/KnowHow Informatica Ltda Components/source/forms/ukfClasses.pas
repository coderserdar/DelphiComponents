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

unit ukfClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, Forms, Menus, TypInfo, uksyUtils;

type

	EKForm = class( EKForms );
	TTrackStyle = ( tsMinTrack, tsMaxTrack, tsNone );
	TLoadOptions = ( loCreate, loActivate, loShow );
	TSaveOptions = ( soDestroy, soDeactivate, soHide );

	TKMainMenu = class( TMainMenu );

{
	TKFormTrack

	This class is responsible for controling width and height of
	a TKForm object. Set Ignore to false to bypass the values of
	Width and Height.

}

	TKFormTrack = class( TPersistent )
	private
		FWidth: Integer;
		FHeight: Integer;
		FIgnore: Boolean;

		FForm: TCustomForm;
		FTrackStyle: TTrackStyle;

		procedure SetWidth( Value: Integer );
		procedure SetHeight( Value: Integer );
		procedure SetIgnore( Value: Boolean );

	public
		constructor Create( AStyle: TTrackStyle );
		procedure SetOwner( AOwner: TCustomForm );

	published
		property Ignore: Boolean
						 read FIgnore write SetIgnore;
		property Height: Integer
						 read FHeight write SetHeight default 300;
		property Width: Integer
						 read FWidth write SetWidth default 440;

	end;

{

	TKPersistenceItem

	This class describes a persistent property. It will properly
	save and retrieve values of certain kinds of properties ( char,
	integer, enumeration, and string ).

	To specify a property to be streamed, define the component,
	the property, the default value and the registry subkey.

	The component will automatically save those values to the
	registry under the proper key, and use this same key to
	retrieve those values later.

	Define the registry key and the moment to save and load the
	properties' values through the TKPersistence object.

}

	TKPersistenceItem = class( TCollectionItem )
	private
		FDefaultValue: String;
		FPropertyName: String;
		FComponentName: String;
		FRegistrySubKey: String;

		function PropRef: PPropInfo;
		function CompRef: TComponent;
		function GetComponentName: String;
		function GetDefaultValue: String;
		function GetPropertyName: String;
		procedure SetComponentName( Value: String );
		procedure SetDefaultValue( Value: String );
		procedure SetPropertyName( Value: String );

	public
		procedure Assign( pt: TPersistent ); override;
		function GetDisplayName: String; override;
		function GetOwner: TPersistent; override;

	published
		property ComponentName: String
						 read GetComponentName write SetComponentName;
		property DefaultValue: String
						 read GetDefaultValue write SetDefaultValue;
		property PropertyName: String
						 read GetPropertyName write SetPropertyName;
		property RegistrySubKey: String
						 read FRegistrySubKey write FRegistrySubKey;

	end;

{

	TKPersistence

	This class describes a collection of persistent properties.

	Define the main registry key under which the object should
	load/save property values through the RegistryKey property.

	Define loading and saving moments through the SaveOption and
	LoadOption properties.

}

	TKPersistence = class( TCollection )
	private
		FForm: TCustomForm;
		FRegistryKey: String;
		FSaveOption: TSaveOptions;
		FLoadOption: TLoadOptions;

		function GetItem( Index: Integer ): TKPersistenceItem;
		procedure SetItem( Index: Integer; Value: TKPersistenceItem );

	public
		constructor Create( AForm: TCustomForm );

		function Add: TkPersistenceItem;
		function GetOwner: TPersistent; override;

		property Items[Index: Integer]: TkPersistenceItem
						 read GetItem write SetItem;

	published
		property RegistryKey: String
						 read FRegistryKey write FRegistryKey;
		property LoadOption: TLoadOptions
						 read FLoadOption write FLoadOption default loCreate;
		property SaveOption: TSaveOptions
						 read FSaveOption write FSaveOption default soDestroy;

	end;

var
	FormOffSet: Integer;


implementation

uses
	Windows, SysUtils, ukfConsts, ukfResStr;

{************************************************************************}
{ TKFormTrack }
{************************************************************************}

constructor TKFormTrack.Create( AStyle: TTrackStyle );
begin
	inherited Create;
  FForm := nil;
	FWidth := 440;
	FHeight := 300;
	FTrackStyle := AStyle;
	FIgnore := ( AStyle <> tsMinTrack );
end;

procedure TKFormTrack.SetOwner( AOwner: TCustomForm );
begin
	ForceObject( AOwner );
	FForm := AOwner;
end;

procedure TKFormTrack.SetHeight( Value: Integer );
begin
	if FIgnore then
	begin
		FHeight := Value;
		Exit;
	end;
	if ( FTrackStyle = tsMinTrack ) then
	begin
		if ( FForm = nil ) or ( Value <= FForm.Height ) then
			FHeight := Value;
	end
	else if ( FTrackStyle = tsMaxTrack ) then
	begin
		if ( FForm = nil ) or ( Value >= FForm.Height ) then
			FHeight := Value;
	end;
end;

procedure TKFormTrack.SetIgnore( Value: Boolean );
begin
	if ( Value <> FIgnore ) then
	begin
		FIgnore := Value;
		if ( not FIgnore ) and ( FForm <> nil ) then
		begin
			SetWidth( FForm.Width );
			SetHeight( FForm.Height );
		end;
	end;
end;

procedure TKFormTrack.SetWidth( Value: Integer );
begin
	if FIgnore then
	begin
		FHeight := Value;
		Exit;
	end;
	if ( FTrackStyle = tsMinTrack ) then
	begin
		if ( FForm = nil ) or ( Value <= FForm.Width ) then
			FWidth := Value;
	end
	else if ( FTrackStyle = tsMaxTrack ) then
	begin
		if ( FForm = nil ) or ( Value >= FForm.Width ) then
			FWidth := Value;
	end;
end;

{************************************************************************}
{ TKPersistenceItem }
{************************************************************************}

function TKPersistenceItem.GetComponentName: String;
begin
	Result := FComponentName;
end;

function TKPersistenceItem.GetDefaultValue: String;
begin
	Result := FDefaultValue;
end;

function TKPersistenceItem.GetPropertyName: String;
begin
	Result := FPropertyName;
end;

procedure TKPersistenceItem.SetComponentName( Value: String );
begin
	if ( csLoading in TComponent( GetOwner ).ComponentState ) or
		 ( not ( csDesigning in TComponent( GetOwner ).ComponentState ) ) then
	begin
		FComponentName := Value;
		Exit;
	end;
	if ( CompareText( FComponentName, Value ) = 0 ) then Exit;
	if ( CompareText( TComponent( GetOwner ).Name, Value ) = 0 ) or
		 ( TComponent( GetOwner ).FindComponent( Value ) <> nil ) then
	begin
		FPropertyName := '';
		FDefaultValue := '';
		FComponentName := Value;
	end
	else
	begin
		FPropertyName := '';
		FDefaultValue := '';
		FComponentName := '';
	end;
end;

{ SetPropertyName para zerar o valor de DefaultValue quando é alterado }

{$HINTS OFF}
procedure TKPersistenceItem.SetDefaultValue( Value: String );
var
	pd: PTypeData;
	iValue: Integer;
begin
	if ( csLoading in TComponent( GetOwner ).ComponentState ) or
		 ( not ( csDesigning in TComponent( GetOwner ).ComponentState ) ) then
	begin
		FDefaultValue := Value;
		Exit;
	end;
	if ( PropRef <> nil ) then
		with PropRef^ do
			case PropType^.Kind of
				tkChar:
					if ( Length( Value ) >= 1 ) then
						FDefaultValue := Value[1]
					else
						FDefaultValue := '*';
				tkInteger:
					try
						iValue := StrToInt( Value );
						FDefaultValue := IntToStr( iValue );
					except
						FDefaultValue := '0';
					end;
				tkEnumeration:
					try
						iValue := StrToInt( Value );
						pd := GetTypeData( PropType^ );
						if ( iValue <= pd^.MaxValue ) and
							 ( iValue >= pd^.MinValue ) then
							FDefaultValue := GetEnumName( PropType^, iValue )
						else
							FDefaultValue := GetEnumName( PropType^, 0 );
					except
						try
							iValue := GetEnumValue( PropType^, Value );
							FDefaultValue := GetEnumName( PropType^, iValue );
						except
							FDefaultValue := GetEnumName( PropType^, 0 );
						end;
					end;
			else
				FDefaultValue := Value;
			end;
end;
{$HINTS ON}

function TKPersistenceItem.CompRef: TComponent;
begin
	Result := TComponent( GetOwner ).FindComponent( FComponentName );
	if ( Result = nil ) then
		if ( CompareText( TComponent( GetOwner ).Name, FComponentName ) = 0 ) then
			Result := TComponent( GetOwner );
end;

function TKPersistenceItem.PropRef: PPropInfo;
begin
	Result := nil;
	if ( CompRef <> nil ) then
		Result := GetPropInfo( CompRef.ClassInfo, FPropertyName );
end;

procedure TKPersistenceItem.SetPropertyName( Value: String );
begin
	if ( csLoading in TComponent( GetOwner ).ComponentState ) or
		 ( not ( csDesigning in TComponent( GetOwner ).ComponentState ) ) then
	begin
		FPropertyName := Value;
		Exit;
	end;
	if ( CompareText( FPropertyName, Value ) = 0 ) then Exit;
	if ( FComponentName = '' ) or ( CompRef = nil ) then
	begin
		FPropertyName := '';
		FDefaultValue := '';
		Exit;
	end;
	FPropertyName := Value;
	if ( PropRef <> nil ) then
		SetDefaultValue( FDefaultValue )
	else
	begin
		FPropertyName := '';
		FDefaultValue := '';
	end;
end;

procedure TKPersistenceItem.Assign( pt: TPersistent );
begin
	if ( pt is TKPersistenceItem ) then
	begin
		FDefaultValue := TKPersistenceItem( pt ).DefaultValue;
		FPropertyName := TKPersistenceItem( pt ).PropertyName;
		FComponentName := TKPersistenceItem( pt ).ComponentName;
		FRegistrySubKey := TKPersistenceItem( pt ).RegistrySubKey;
	end
	else
		inherited Assign( pt );
end;

function TKPersistenceItem.GetDisplayName: String;
begin
	if ( ComponentName = '' ) then
		Result := sFPNoCompSelected
	else if ( PropertyName = '' ) then
		Result := sFPNoPropSelected
	else
		Result := Format ( '%s.%s', [ComponentName, PropertyName] );
end;

function TKPersistenceItem.GetOwner: TPersistent;
begin
	Result := ( Collection as TKPersistence ).FForm;
end;

{************************************************************************}
{ TKPersistence }
{************************************************************************}

constructor TKPersistence.Create( AForm: TCustomForm );
begin
	ForceObject( AForm );
	inherited Create( TKPersistenceItem );
	FForm := AForm;
	FRegistryKey := sFPersisteceRegKey;
	FLoadOption := loCreate;
	FSaveOption := soDestroy;
end;

function TKPersistence.GetItem( Index: Integer ): TKPersistenceItem;
begin
	Result := TKPersistenceItem( inherited GetItem( Index ) );
end;

procedure TKPersistence.SetItem( Index: Integer; Value: TKPersistenceItem );
begin
	inherited SetItem( Index, Value );
end;

function TKPersistence.Add: TkPersistenceItem;
begin
	Result := TkPersistenceItem( inherited Add );
end;

function TKPersistence.GetOwner: TPersistent;
begin
	Result := FForm;
end;

initialization
	FormOffSet := GetSystemMetrics( SM_CYCAPTION ) +
			      			2 * GetSystemMetrics( SM_CYFRAME );

end.
