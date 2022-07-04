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

unit uksydInternal;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	SysUtils, Classes, DsgnIntf, TypInfo, uksydUtils;

{
--------------------------------------------------------------------------------
---------------------------- TPropertyEditor Hack ------------------------------
--------------------------------------------------------------------------------
}

type
  
	TKPropertyEditor = class( TObject ) {##NI##}
	private
		FEditor: TPropertyEditor;
		FForceFree: Boolean;

	protected
		function GetDesigner: TKFormDesigner;
		function GetPrivateDirectory: string;
		function GetPropCount: Integer;
		function GetPropList: PInstPropList;

	public
		constructor Create( AClass: TPropertyEditorClass;
			ADesigner: TKFormDesigner; APropCount: Integer ); virtual;
		constructor CreateFrom( AEditor: TPropertyEditor ); virtual;

		function RegisteredFor( ComponentClass: TClass; ComponentInstance: TPersistent;
			const PropertyName: string ): Boolean; virtual;

		destructor Destroy; override;
		procedure Activate; virtual;
		function AllEqual: Boolean; virtual;
		procedure Edit; virtual;
		function GetAttributes: TPropertyAttributes; virtual;
		function GetComponent( Index: Integer ): TPersistent;
		function GetEditLimit: Integer; virtual;
		function GetName: string; virtual;
		procedure GetProperties( Proc: TGetPropEditProc ); virtual;
		function GetPropType: PTypeInfo;
		function GetValue: string; virtual;
		procedure GetValues( Proc: TGetStrProc ); virtual;
		procedure Initialize; virtual;
		procedure Revert;
		procedure SetPropEntry( Index: Integer; AInstance: TPersistent;
			APropInfo: PPropInfo );
		procedure SetValue( const Value: string ); virtual;
		function ValueAvailable: Boolean;

{ old protected methods }
		function GetPropInfo: PPropInfo;
		function GetFloatValue: Extended;
		function GetFloatValueAt( Index: Integer ): Extended;
		function GetMethodValue: TMethod;
		function GetMethodValueAt( Index: Integer ): TMethod;
		function GetOrdValue: Longint;
		function GetOrdValueAt( Index: Integer ): Longint;
		function GetStrValue: string;
		function GetStrValueAt( Index: Integer ): string;
		function GetVarValue: Variant;
		function GetVarValueAt( Index: Integer ): Variant;
		procedure Modified;
		procedure SetFloatValue( Value: Extended );
		procedure SetMethodValue( const Value: TMethod );
		procedure SetOrdValue( Value: Longint );
		procedure SetStrValue( const Value: string );
		procedure SetVarValue( const Value: Variant );
{ were protected }

		property Designer: TKFormDesigner
						 read GetDesigner;
		property Editor: TPropertyEditor
						 read FEditor;
		property ForceFree: Boolean
						 read FForceFree write FForceFree;
		property PropCount: Integer
						 read GetPropCount;
		property PropInfo: PPropInfo
						 read GetPropInfo;
		property PrivateDirectory: String
						 read GetPrivateDirectory;
		property PropList: PInstPropList
						 read GetPropList;
		property Value: string
						 read GetValue write SetValue;
    {##NI##}
	end;

	TKPropertyEditorClass = class of TKPropertyEditor;

	function CreatePropEditor( AClass: TPropertyEditorClass;
		ADesigner: TKFormDesigner; APropCount: Integer ): TPropertyEditor;

implementation

uses
	Consts, uksydConsts, uksyUtils;

{
--------------------------------------------------------------------------------
---------------------------- TPropertyEditor Hack ------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

{
--------------------------------------------------------------------------------
------------------------------- TKPropEditorHack -------------------------------
--------------------------------------------------------------------------------
}

{$HINTS OFF}
type

	TKPropEditorHack = class( TObject )
	private
		FDesigner: TKFormDesigner;
		FPropList: PInstPropList;
		FPropCount: Integer;

		constructor Create( ADesigner: TKFormDesigner; APropCount: Integer );
		function GetPrivateDirectory: string;
		procedure SetPropEntry( Index: Integer; AInstance: TPersistent;
			APropInfo: PPropInfo );

	protected
		function GetPropInfo: PPropInfo;
		function GetFloatValue: Extended;
		function GetFloatValueAt( Index: Integer ): Extended;
		function GetMethodValue: TMethod;
		function GetMethodValueAt( Index: Integer ): TMethod;
		function GetOrdValue: Longint;
		function GetOrdValueAt( Index: Integer ): Longint;
		function GetStrValue: string;
		function GetStrValueAt( Index: Integer ): string;
		function GetVarValue: Variant;
		function GetVarValueAt( Index: Integer ): Variant;
		procedure Modified;
		procedure SetFloatValue( Value: Extended );
		procedure SetMethodValue( const Value: TMethod );
		procedure SetOrdValue( Value: Longint );
		procedure SetStrValue( const Value: string );
		procedure SetVarValue( const Value: Variant );

	public
		destructor Destroy; override;
		procedure Activate; virtual;
		function AllEqual: Boolean; virtual;
		procedure Edit; virtual;
		function GetAttributes: TPropertyAttributes; virtual;
		function GetComponent( Index: Integer ): TPersistent;
		function GetEditLimit: Integer; virtual;
		function GetName: string; virtual;
		procedure GetProperties( Proc: TGetPropEditProc ); virtual;
		function GetPropType: PTypeInfo;
		function GetValue: string; virtual;
		procedure GetValues( Proc: TGetStrProc ); virtual;
		procedure Initialize; virtual;
		procedure Revert;
		procedure SetValue( const Value: string ); virtual;
		function ValueAvailable: Boolean;

		property Designer: TKFormDesigner
						 read FDesigner;
		property PrivateDirectory: string
						 read GetPrivateDirectory;
		property PropCount: Integer
						 read FPropCount;
		property Value: string
						 read GetValue write SetValue;

	end;

	TKPropEditorHackClass = class of TKPropEditorHack;

constructor TKPropEditorHack.Create( ADesigner: TKFormDesigner;
	APropCount: Integer );
begin
	inherited Create;
	FDesigner := ADesigner;
	GetMem( FPropList, APropCount * SizeOf( TInstProp ) );
	FPropCount := APropCount;
end;

destructor TKPropEditorHack.Destroy;
begin
	if CheckPointer( FPropList ) then
		FreeMem( FPropList, FPropCount * SizeOf( TInstProp ) );
	inherited Destroy;
end;

procedure TKPropEditorHack.Activate;
begin
end;

function TKPropEditorHack.AllEqual: Boolean;
begin
	Result := FPropCount = 1;
end;

procedure TKPropEditorHack.Edit;
type
	TGetStrFunc = function( const Value: string ): Integer of object;
var
	I: Integer;
	Values: TStringList;
	AddValue: TGetStrFunc;
begin
	Values := TStringList.Create;
	try
		Values.Sorted := ( paSortList in GetAttributes );
		AddValue := Values.Add;
		GetValues( TGetStrProc( AddValue ) );
		if CheckStrings( Values ) then
		begin
			I := Values.IndexOf( Value ) + 1;
			if I = Values.Count then
				I := 0;
			Value := Values[I];
		end;
	finally
		Values.Free;
	end;
end;

function TKPropEditorHack.GetAttributes: TPropertyAttributes;
begin
	Result := [paMultiSelect, paRevertable];
end;

function TKPropEditorHack.GetComponent( Index: Integer ): TPersistent;
begin
	Result := FPropList^[Index].Instance;
end;

function TKPropEditorHack.GetFloatValue: Extended;
begin
	Result := GetFloatValueAt( 0 );
end;

function TKPropEditorHack.GetFloatValueAt( Index: Integer ): Extended;
begin
	with FPropList^[Index] do
		Result := GetFloatProp( Instance, PropInfo );
end;

function TKPropEditorHack.GetMethodValue: TMethod;
begin
	Result := GetMethodValueAt( 0 );
end;

function TKPropEditorHack.GetMethodValueAt( Index: Integer ): TMethod;
begin
	with FPropList^[Index] do
		Result := GetMethodProp( Instance, PropInfo );
end;

function TKPropEditorHack.GetEditLimit: Integer;
begin
	Result := 255;
end;

function TKPropEditorHack.GetName: string;
begin
	Result := FPropList^[0].PropInfo^.Name;
end;

function TKPropEditorHack.GetOrdValue: Longint;
begin
	Result := GetOrdValueAt( 0 );
end;

function TKPropEditorHack.GetOrdValueAt( Index: Integer ): Longint;
begin
	with FPropList^[Index] do
		Result := GetOrdProp( Instance, PropInfo );
end;

function TKPropEditorHack.GetPrivateDirectory: string;
begin
	Result := Designer.GetPrivateDirectory;
end;

procedure TKPropEditorHack.GetProperties( Proc: TGetPropEditProc );
begin
end;

function TKPropEditorHack.GetPropInfo: PPropInfo;
begin
	Result := FPropList^[0].PropInfo;
end;

function TKPropEditorHack.GetPropType: PTypeInfo;
begin
	Result := FPropList^[0].PropInfo^.PropType^;
end;

function TKPropEditorHack.GetStrValue: string;
begin
	Result := GetStrValueAt( 0 );
end;

function TKPropEditorHack.GetStrValueAt( Index: Integer ): string;
begin
	with FPropList^[Index] do
	  Result := GetStrProp( Instance, PropInfo );
end;

function TKPropEditorHack.GetVarValue: Variant;
begin
  Result := GetVarValueAt( 0 );
end;

function TKPropEditorHack.GetVarValueAt( Index: Integer ): Variant;
begin
	with FPropList^[Index] do
	  Result := GetVariantProp( Instance, PropInfo );
end;

function TKPropEditorHack.GetValue: string;
begin
	Result := srUnknown;
end;

procedure TKPropEditorHack.GetValues( Proc: TGetStrProc );
begin
end;

procedure TKPropEditorHack.Initialize;
begin
end;

procedure TKPropEditorHack.Modified;
begin
	Designer.Modified;
end;

procedure TKPropEditorHack.SetFloatValue( Value: Extended );
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
			SetFloatProp( Instance, PropInfo, Value );
	Modified;
end;

procedure TKPropEditorHack.SetMethodValue( const Value: TMethod );
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
			SetMethodProp( Instance, PropInfo, Value );
	Modified;
end;

procedure TKPropEditorHack.SetOrdValue( Value: Longint );
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
		  SetOrdProp( Instance, PropInfo, Value );
	Modified;
end;

procedure TKPropEditorHack.SetPropEntry( Index: Integer;
	AInstance: TPersistent; APropInfo: PPropInfo );
begin
	with FPropList^[Index] do
	begin
		Instance := AInstance;
		PropInfo := APropInfo;
	end;
end;

procedure TKPropEditorHack.SetStrValue( const Value: string );
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
		  SetStrProp( Instance, PropInfo, Value );
	Modified;
end;

procedure TKPropEditorHack.SetVarValue( const Value: Variant );
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
		  SetVariantProp( Instance, PropInfo, Value );
	Modified;
end;

procedure TKPropEditorHack.Revert;
var
	I: Integer;
begin
	for I := 0 to FPropCount - 1 do
		with FPropList^[I] do
			Designer.Revert( Instance, PropInfo );
end;

procedure TKPropEditorHack.SetValue( const Value: string );
begin
end;

function TKPropEditorHack.ValueAvailable: Boolean;
var
	I: Integer;
	S: string;
begin
	Result := True;
	for I := 0 to FPropCount - 1 do
	begin
		if CheckObjectClass( FPropList^[I].Instance, TComponent ) and
			( csCheckPropAvail in TComponent( FPropList^[I].Instance ).ComponentStyle ) then
		begin
			try
				S := GetValue;
				AllEqual;
			except
				Result := False;
			end;
			Exit;
		end;
	end;
end;
{$HINTS ON}

{
--------------------------------------------------------------------------------
------------------------------ TKPropertyEditor --------------------------------
--------------------------------------------------------------------------------
}

constructor TKPropertyEditor.Create( AClass: TPropertyEditorClass;
	ADesigner: TKFormDesigner; APropCount: Integer );
begin
	Force( [AClass, ADesigner] );
	inherited Create;
	FForceFree := false;
	FEditor := TPropertyEditor( TKPropEditorHackClass( AClass ).Create(
		ADesigner, APropCount ) );
end;

constructor TKPropertyEditor.CreateFrom( AEditor: TPropertyEditor );
begin
	ForceObject( AEditor );
	inherited Create;
	FForceFree := false;
	FEditor := AEditor;
end;

destructor TKPropertyEditor.Destroy;
begin
	if FForceFree then
		FEditor.Free;
	inherited Destroy;
end;

function TKPropertyEditor.RegisteredFor( ComponentClass: TClass;
	ComponentInstance: TPersistent; const PropertyName: string ): Boolean;
var
	pp: PPropInfo;
begin
	pp := TypInfo.GetPropInfo( PTypeInfo( ComponentClass.ClassInfo ), PropertyName );
	Result := ( pp <> nil );
	if Result then
		{for i := 0 to PropCount-1 do}
			SetPropEntry( 0, ComponentInstance, pp );
end;

function TKPropertyEditor.GetDesigner: TKFormDesigner;
begin
	Result := FEditor.Designer;
end;

function TKPropertyEditor.GetPrivateDirectory: string;
begin
	Result := FEditor.PrivateDirectory;
end;

function TKPropertyEditor.GetPropCount: Integer;
begin
	Result := TKPropEditorHack( FEditor ).FPropCount;
end;

function TKPropertyEditor.GetPropList: PInstPropList;
begin
	Result := TKPropEditorHack( FEditor ).FPropList;
end;

function TKPropertyEditor.GetPropInfo: PPropInfo;
begin
	Result := TKPropEditorHack( FEditor ).GetPropInfo;
end;

function TKPropertyEditor.GetFloatValue: Extended;
begin
	Result := TKPropEditorHack( FEditor ).GetFloatValue;
end;

function TKPropertyEditor.GetFloatValueAt( Index: Integer ): Extended;
begin
	Result := TKPropEditorHack( FEditor ).GetFloatValueAt( Index );
end;

function TKPropertyEditor.GetMethodValue: TMethod;
begin
	Result := TKPropEditorHack( FEditor ).GetMethodValue;
end;

function TKPropertyEditor.GetMethodValueAt( Index: Integer ): TMethod;
begin
	Result := TKPropEditorHack( FEditor ).GetMethodValueAt( Index );
end;

function TKPropertyEditor.GetOrdValue: Longint;
begin
	Result := TKPropEditorHack( FEditor ).GetOrdValue;
end;

function TKPropertyEditor.GetOrdValueAt( Index: Integer ): Longint;
begin
	Result := TKPropEditorHack( FEditor ).GetOrdValueAt( Index );
end;

function TKPropertyEditor.GetStrValue: string;
begin
	Result := TKPropEditorHack( FEditor ).GetStrValue;
end;

function TKPropertyEditor.GetStrValueAt( Index: Integer ): string;
begin
	Result := TKPropEditorHack( FEditor ).GetStrValueAt( Index );
end;

function TKPropertyEditor.GetVarValue: Variant;
begin
	Result := TKPropEditorHack( FEditor ).GetVarValue;
end;

function TKPropertyEditor.GetVarValueAt( Index: Integer ): Variant;
begin
	Result := TKPropEditorHack( FEditor ).GetVarValueAt( Index );
end;

procedure TKPropertyEditor.Modified;
begin
	TKPropEditorHack( FEditor ).Modified;
end;

procedure TKPropertyEditor.SetFloatValue( Value: Extended );
begin
	TKPropEditorHack( FEditor ).SetFloatValue( Value );
end;

procedure TKPropertyEditor.SetMethodValue( const Value: TMethod );
begin
	TKPropEditorHack( FEditor ).SetMethodValue( Value );
end;

procedure TKPropertyEditor.SetOrdValue( Value: Longint );
begin
	TKPropEditorHack( FEditor ).SetOrdValue( Value );
end;

procedure TKPropertyEditor.SetStrValue( const Value: string );
begin
	TKPropEditorHack( FEditor ).SetStrValue( Value );
end;

procedure TKPropertyEditor.SetVarValue( const Value: Variant );
begin
	TKPropEditorHack( FEditor ).SetVarValue( Value );
end;

procedure TKPropertyEditor.Activate;
begin
	TKPropEditorHack( FEditor ).Activate;
end;

function TKPropertyEditor.AllEqual: Boolean;
begin
	Result := TKPropEditorHack( FEditor ).AllEqual;
end;

procedure TKPropertyEditor.Edit;
begin
	TKPropEditorHack( FEditor ).Edit;
end;

function TKPropertyEditor.GetAttributes: TPropertyAttributes;
begin
	Result := TKPropEditorHack( FEditor ).GetAttributes;
end;

function TKPropertyEditor.GetComponent( Index: Integer ): TPersistent;
begin
	Result := TKPropEditorHack( FEditor ).GetComponent( Index );
end;

function TKPropertyEditor.GetEditLimit: Integer;
begin
	Result := TKPropEditorHack( FEditor ).GetEditLimit;
end;

function TKPropertyEditor.GetName: string;
begin
	Result := TKPropEditorHack( FEditor ).GetName;
end;

procedure TKPropertyEditor.GetProperties( Proc: TGetPropEditProc );
begin
	TKPropEditorHack( FEditor ).GetProperties( Proc );
end;

function TKPropertyEditor.GetPropType: PTypeInfo;
begin
	Result := TKPropEditorHack( FEditor ).GetPropType;
end;

function TKPropertyEditor.GetValue: string;
begin
	Result := TKPropEditorHack( FEditor ).GetValue;
end;

procedure TKPropertyEditor.GetValues( Proc: TGetStrProc );
begin
	TKPropEditorHack( FEditor ).GetValues( Proc );
end;

procedure TKPropertyEditor.Initialize;
begin
	TKPropEditorHack( FEditor ).Initialize;
end;

procedure TKPropertyEditor.Revert;
begin
	TKPropEditorHack( FEditor ).Revert;
end;

procedure TKPropertyEditor.SetPropEntry( Index: Integer; AInstance: TPersistent;
	APropInfo: PPropInfo );
begin
	TKPropEditorHack( FEditor ).SetPropEntry( Index, AInstance, APropInfo );
end;

procedure TKPropertyEditor.SetValue( const Value: string );
begin
	TKPropEditorHack( FEditor ).SetValue( Value );
end;

function TKPropertyEditor.ValueAvailable: Boolean;
begin
	Result := TKPropEditorHack( FEditor ).ValueAvailable;
end;

function CreatePropEditor( AClass: TPropertyEditorClass;
	ADesigner: TKFormDesigner; APropCount: Integer ): TPropertyEditor;
var
	kpe: TKPropertyEditor;
begin
	if ( APropCount <= 0 ) then
	  RaiseExceptionFmt( EKPropertyEditor, sErrPropEdtInvPropCount, [APropCount] );
	kpe := TKPropertyEditor.Create( AClass, ADesigner, APropCount );
	try
		Result := kpe.Editor;
	finally
		kpe.Free;
	end;
end;

end.
