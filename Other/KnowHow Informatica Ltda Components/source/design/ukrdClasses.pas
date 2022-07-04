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

unit ukrdClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	DsgnIntf, EditIntf, Classes, Graphics, DBTables, uksydUtils, 
	uksydClasses;

type

	EKRDClasses = class( EKDKernel );

{
---------------------------------------------------------------------------------
--------------------------- Property Editor Support -----------------------------
---------------------------------------------------------------------------------
}

{ TKControlNameProperty }

	TKControlNameProperty = class( TKCompNameProperty )
	protected
		function GetFilterClass: TComponentClass; override;

	end;

{ TKStringsArrayProperty }

	TKStringsArrayProperty = class( TKDialogClassProperty )
	public
		procedure Edit; override;

	end;

{ TKLanguageProperty }

	TKLanguageProperty = class( TIntegerProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure SetValue( const Value: String ); override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKDBStringProperty }

	TKDBStringProperty = class( TKStringsProperty )
	private
		{$HINTS OFF}
		function GetOwner: TPersistent;
		property Owner;
		{$HINTS ON}

	protected
		function GetDataSet: TDBDataSet; virtual; abstract;
		function IsReadOnly: Boolean; override;

	public
		property DataSet: TDBDataSet
						 read GetDataSet;

	end;

{ TKSessionNameProperty }

	TKSessionNameProperty = class( TKDBStringProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function GetDataSet: TDBDataSet; override;

	end;

{ TKDatabaseNameProperty }

	TKDatabaseNameProperty = class( TKDBStringProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function GetDataSet: TDBDataSet; override;

	end;

{ TKTableNameProperty }

	TKTableNameProperty = class( TKDBStringProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function GetDataSet: TDBDataSet; override;

	end;

{ TKPersistentCharSetProperty }

	TKPersistentCharSetProperty = class( TKDialogClassProperty )
	public
		procedure Edit; override;

	end;

{ TKDBNavEventFormNameProperty }

	TKDBNavEventFormNameProperty = class( TKStringsProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function IsReadOnly: Boolean; override;

	end;

{ TKSpeedFloatProperty }

	TKSpeedFloatProperty = class( TKFloatProperty )
	protected
		function GetFloatFormat: string; override;

	end;

{ TKSpeedIntegerProperty }

	TKSpeedIntegerProperty = class( TKIntegerProperty )
	protected
		function GetIntegerFormat: string; override;

	end;

{ TKSpeedHexaProperty Editor }

	TKSpeedHexaProperty = class( TKHexaProperty )
	protected
		function GetHexaDigits: Integer; override;

	end;

{ TKCustomFormatProperty Editor }

	TKCustomFormatProperty = class( TKStringsProperty )
	{$IFDEF DELPHI4}
	protected
	{$ELSE}
	private
	{$ENDIF}
		procedure GetValueList( List: TStrings ); override;

	protected
		procedure GetFormatList( List: TStrings ); virtual; abstract;
		function IsReadOnly: Boolean; override;

	end;

{ TKFloatFormatProperty Editor }

	TKFloatFormatProperty = class( TKCustomFormatProperty )
	protected
		procedure GetFormatList( List: TStrings ); override;

	end;

{ TKIntegerFormatProperty Editor }

	TKIntegerFormatProperty = class( TKCustomFormatProperty )
	protected
		procedure GetFormatList( List: TStrings ); override;

	end;

{ TKDateTimeFormatProperty Editor }

	TKDateTimeFormatProperty = class( TKCustomFormatProperty )
	protected
		procedure GetFormatList( List: TStrings ); override;

	end;

{ TKDateFormatProperty Editor }

	TKDateFormatProperty = class( TKCustomFormatProperty )
	protected
		procedure GetFormatList( List: TStrings ); override;

	end;

{ TKTimeFormatProperty Editor }

	TKTimeFormatProperty = class( TKCustomFormatProperty )
	protected
		procedure GetFormatList( List: TStrings ); override;

	end;

{ TKSpeedDateTimeProperty Editor }

	TKSpeedDateTimeProperty = class( TKDateProperty )
	protected
		function GetDateTimeFormat: string; override;

	end;

{ TKSpeedDateProperty Editor }

	TKSpeedDateProperty = class( TKDateProperty )
	protected
		function GetDateTimeFormat: string; override;

	end;

{ TKSpeedTimeProperty Editor }

	TKSpeedTimeProperty = class( TKTimeProperty )
	protected
		function GetDateTimeFormat: string; override;

	end;

{ TKBitsProperty Editor }

	TKBitsProperty = class( TKDialogClassProperty )
	protected
		function GetIsSorted: Boolean; dynamic;
		function GetBackGroundColor: TColor; dynamic;
		procedure GetItemList( sl: TStrings ); virtual; abstract;

	public
		procedure Edit; override;

	end;

implementation

uses
	Windows, SysUtils, Controls, Forms, TypInfo, ExptIntf, uksyConsts, uksyUtils,
	ukrResStr, ukrDBUtils, ukrLanguage, ukrClasses, ukrUtils, ukrCtrls, ukrdConsts,
	ukrdfStrArray, ukrdfCharSet;

{
---------------------------------------------------------------------------------
--------------------------- Property Editor Support -----------------------------
---------------------------------------------------------------------------------
}

{-------------------------- TKControlNameProperty ------------------------------}

function TKControlNameProperty.GetFilterClass: TComponentClass;

begin
	Result := TControl;
end;

{------------------------- TKStringsArrayProperty ------------------------------}

procedure TKStringsArrayProperty.Edit;
var
	csa: TKCustomStringsArray;
begin
	csa := TKCustomStringsArray( GetOrdValue );
	ForceObject( csa );
	with csa do
		if EditKCustomStringsArray( csa, RowCount, HighColCount, ColsAsRows,
			GetHeader, GetGridOptions, EditorState ) then
			Modified;
end;

{--------------------------- TKLanguageProperty --------------------------------}

procedure TKLanguageProperty.GetValues( Proc: TGetStrProc );
begin
	GetLanguageValues( Proc );
end;

procedure TKLanguageProperty.SetValue( const Value: String );
begin
	SetOrdValue( Integer( StringToLanguage( Value ) ) );
end;

function TKLanguageProperty.GetValue: string;
begin
	Result := LanguageToString( TLanguage( GetOrdValue ) );
end;

function TKLanguageProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paValueList];
end;

{--------------------------- TKDBStringProperty --------------------------------}

function TKDBStringProperty.GetOwner: TPersistent;
begin
	Result := GetDataSet;
end;

function TKDBStringProperty.IsReadOnly: Boolean;
begin
	Result := False;
end;

{------------------------- TKSessionNameProperty -------------------------------}

function TKSessionNameProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TTable );
end;

procedure TKSessionNameProperty.GetValueList( List: TStrings );
begin
	ForceObject( List );
	Sessions.GetSessionNames( List );
end;

{------------------------ TKDatabaseNameProperty -------------------------------}

function TKDataBaseNameProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TDBDataSet );
end;

procedure TKDatabaseNameProperty.GetValueList( List: TStrings );
begin
	ForceObject( List );
	DataSet.DBSession.GetDatabaseNames( List );
end;

{-------------------------- TKTableNameProperty --------------------------------}

function TKTableNameProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TTable );
end;

procedure TKTableNameProperty.GetValueList( List: TStrings );
begin
	ForceObject( List );
	with ( DataSet as TTable ) do
		DBSession.GetTableNames( DatabaseName, TABLE_TYPE_MASKS[TableType],
			( TableType = ttDefault ), False, List );
end;

{--------------------- TKPersistentCharSetProperty -----------------------------}

procedure TKPersistentCharSetProperty.Edit;
var
	pcs: TKPersistentCharSet;
	s: string;
begin
	pcs := TKPersistentCharSet( GetOrdValue );
	ForceObject( pcs );
	if CheckObjectClass( GetComponent( 0 ), TComponent ) then
		s := ( GetComponent( 0 ) as TComponent ).Name
	else if CheckObjectClass( GetComponent( 0 ), TPersistent ) then
		s := ( GetComponent( 0 ) as TPersistent ).GetNamePath;
	if EditValidCharSet( s, pcs ) then
		Modified;
end;


{------------------------ TKDBNavEventFormNameProperty -------------------------}

function TKDBNavEventFormNameProperty.IsReadOnly: Boolean;
begin
  Result := True;
end;

procedure TKDBNavEventFormNameProperty.GetValueList( List: TStrings );
begin
	GetProjectInfo( nil, nil, List );
	TrimStrings( List );
end;

{---------------------------- TKSpeedFloatProperty -----------------------------}

type

	TKFormattedSpeedControlHack = class( TKFormattedSpeedControl );

function TKSpeedFloatProperty.GetFloatFormat: string;
begin
	Result := GetFirstString( [TKFormattedSpeedControlHack( ( GetComponent( 0 ) as
		TKFormattedSpeedControl ) ).DisplayFormat, ( inherited GetFloatFormat )] );
end;

{---------------------------- TKSpeedIntegerProperty -----------------------------}

type

	TKCustomSpeedIntegerHack = class( TKCustomSpeedInteger );

function TKSpeedIntegerProperty.GetIntegerFormat: string;
begin
	Result := GetFirstString( [TKCustomSpeedIntegerHack( ( GetComponent( 0 ) as
		TKCustomSpeedInteger ) ).DisplayFormat, ( inherited GetIntegerFormat )] );
end;

{---------------------------- TKSpeedHexaProperty Editor ------------------------}

type

  TKCustomSpeedHexaHack = class( TKCustomSpeedHexa );

function TKSpeedHexaProperty.GetHexaDigits: Integer;
begin
	Result := TKCustomSpeedHexaHack( ( GetComponent( 0 ) as TKCustomSpeedHexa ) ).Digits;
end;

{-------------------------- TKCustomFormatProperty Editor -----------------------}

procedure TKCustomFormatProperty.GetValueList( List: TStrings );
begin
	GetFormatList( List );
end;

function TKCustomFormatProperty.IsReadOnly: Boolean;
begin
	Result := False;
end;

{-------------------------- TKFloatFormatProperty Editor -----------------------}

procedure TKFloatFormatProperty.GetFormatList( List: TStrings );
var
	i: Byte;
begin
	List.Text := FLOAT_FORMATS;
	for i := 0 to MAX_CUR_FORMATS - 1 do
	begin
		List.Add( Format( CURRENCY_FORMAT, [Format( CURRENCY_FORMATS[i], [CurrencyString,
			DEFAULT_FLOAT_FORMAT] ), Format( CURRENCY_NEG_FORMATS[i], [CurrencyString,
			DEFAULT_FLOAT_FORMAT] )] ) );
		List.Add( Format( CURRENCY_FORMAT, [Format( CURRENCY_FORMATS[i], [CurrencyString,
			EXTENDED_FLOAT_FORMAT] ), Format( CURRENCY_NEG_FORMATS[i], [CurrencyString,
			EXTENDED_FLOAT_FORMAT] )] ) );
	end;
end;

{-------------------------- TKIntegerFormatProperty Editor ---------------------}

procedure TKIntegerFormatProperty.GetFormatList( List: TStrings );
begin
	List.Text := INTEGER_FORMATS;
end;

{------------------------ TKDateTimeFormatProperty Editor ----------------------}

procedure TKDateTimeFormatProperty.GetFormatList( List: TStrings );
begin
	List.Text := DATETIME_FORMATS + CH_CRLF + DATE_FORMATS + CH_CRLF + TIME_FORMATS;
end;

{-------------------------- TKDateFormatProperty Editor ------------------------}

procedure TKDateFormatProperty.GetFormatList( List: TStrings );
begin
	List.Text := DATE_FORMATS;
end;

{-------------------------- TKTimeFormatProperty Editor ------------------------}

procedure TKTimeFormatProperty.GetFormatList( List: TStrings );
begin
	List.Text := TIME_FORMATS;
end;

{---------------------- TKSpeedDateTimeProperty Editor -------------------------}

function TKSpeedDateTimeProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [TKFormattedSpeedControlHack( ( GetComponent( 0 ) as
		TKFormattedSpeedControl ) ).DisplayFormat, ( inherited GetDateTimeFormat )] );
end;

{------------------------ TKSpeedDateProperty Editor ---------------------------}

function TKSpeedDateProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [TKFormattedSpeedControlHack( ( GetComponent( 0 ) as
		TKFormattedSpeedControl ) ).DisplayFormat, ( inherited GetDateTimeFormat )] );
end;

{------------------------ TKSpeedTimeProperty Editor ---------------------------}

function TKSpeedTimeProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [TKFormattedSpeedControlHack( ( GetComponent( 0 ) as
		TKFormattedSpeedControl ) ).DisplayFormat, ( inherited GetDateTimeFormat )] );
end;

{--------------------------- TKBitsProperty Editor -----------------------------}

function TKBitsProperty.GetBackGroundColor: TColor;
begin
	Result := clBtnFace;
end;

function TKBitsProperty.GetIsSorted: Boolean;
begin
	Result := False;
end;

procedure TKBitsProperty.Edit;
var
	sl: TStrings;
	bw: TKBitsWrapper;
begin
	bw := TKBitsWrapper( GetOrdValue );
	sl := TStringList.Create;
	try
		GetItemList( sl );
		ForceStrings( sl );
	{ Do not call SetOrdProp for bw, see TKLinkedFieldsProperty.Edit. }
		if InputCheckListDialog( GetComponent( 0 ).GetNamePath + '.' + GetName,
			GetBackGroundColor, GetIsSorted, sl, bw.Bits ) then
			Modified;
	finally
	  sl.Free;
	end;
end;

end.
