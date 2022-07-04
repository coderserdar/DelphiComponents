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

unit ukbcDBCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  Messages, Classes, Controls, DB, DBTables, DBCtrls, ukbcCtrls;

type

{ TKCustomDBBarcode }

	TKCustomDBBarcode = class( TKCustomBarcode )
	private
		FDataLink: TFieldDataLink;
		FAddOnDataLink: TFieldDataLink;

		function GetAddOnField: TField;
		function GetBarcodeField: TField;
		function GetAddOnText: String;
		function GetBarcodeText: String;
		procedure DataChange( Sender: TObject );
		procedure AddOnDataChange( Sender: TObject );
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GETDATALINK;

		function GetBarcodeFieldName: String;
		function GetAddOnFieldName: String;
		function GetDataSource: TDataSource;
		procedure SetBarcodeFieldName( const Value: String );
		procedure SetDataSource( Value: TDataSource );
		procedure SetAddOnFieldName( const Value: String );

	protected
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property FldAddOn: TField
						 read GetAddOnField;
		property FldBarcode: TField
						 read GetBarcodeField;

		property AddOnField: string
						 read GetAddOnFieldName write SetAddOnFieldName;
		property BarcodeField: string
						 read GetBarcodeFieldName write SetBarcodeFieldName;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

	end;

{ TKDBBarcode }

	TKDBBarcode = class( TKCustomDBBarcode )
	published
		property AddOnField;
		property AddOnStyle;
		property AddOnString;
		property Align;
		property Alignment;
		property Autosize;
		property BarcodeField;
		property BarColor;
		property BarCrackColor;
		property BarDigit;
		property BarKind;
		property BarString;
		property BarWidth;
    property DataSource;
		property Font;
		property PaddingStyle;
		property ShowCaption;
		property Stretched;
		property ThickBarRatio;
		property Transparent;
		property UseCheckDigit;

		property ShowHint;
		property Visible;

		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;

	end;
	
implementation

uses
  ukbcUtils;

{
-------------------------------------------------------------------------
------------------------ TKCustomDBBarcode Class ------------------------
-------------------------------------------------------------------------
}

constructor TKCustomDBBarcode.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDataLink := TFieldDataLink.Create;
	FAddOnDataLink := TFieldDataLink.Create;
	FDataLink.OnDataChange := DataChange;
	FAddOnDataLink.OnDataChange := AddOnDataChange;
end;

destructor TKCustomDBBarcode.Destroy;
begin
	FDataLink.Free;
  FDataLink := nil;
	FAddOnDataLink.Free;
  FAddOnDataLink := nil;
	inherited Destroy;
end;

procedure TKCustomDBBarcode.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and
		 ( ( FDataLink <> nil ) or ( FAddOnDataLink <> nil ) ) and
		 ( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKCustomDBBarcode.GetBarcodeFieldName: String;
begin
	Result := FDataLink.FieldName;
end;

function TKCustomDBBarcode.GetBarcodeField: TField;
begin
	Result := FDataLink.Field;
end;

function TKCustomDBBarcode.GetBarcodeText: String;
begin
	if ( FDataLink.Field <> nil ) then
		Result := FDataLink.Field.DisplayText
	else
		Result := '0';
end;

function TKCustomDBBarcode.GetAddOnFieldName: String;
begin
	Result := FAddOnDataLink.FieldName;
end;

function TKCustomDBBarcode.GetAddOnField: TField;
begin
	Result := FAddOnDataLink.Field;
end;

function TKCustomDBBarcode.GetAddOnText: String;
begin
	if ( FAddOnDataLink.Field <> nil ) then
		Result := FAddOnDataLink.Field.DisplayText
	else
		Result := '0';
end;

procedure TKCustomDBBarcode.AddOnDataChange( Sender: TObject );
begin
	AddOnCaption := GetAddOnText;
end;

procedure TKCustomDBBarcode.DataChange( Sender: TObject );
begin
	BarCaption := GetBarcodeText;
end;

procedure TKCustomDBBarcode.CMGetDataLink( var Message: TMessage );
begin
  Message.Result := Integer( FDataLink );
end;

function TKCustomDBBarcode.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKCustomDBBarcode.SetAddOnFieldName( const Value: String );
begin
	FAddOnDataLink.FieldName := Value;
end;

procedure TKCustomDBBarcode.SetBarcodeFieldName( const Value: String );
begin
	FDataLink.FieldName := Value;
end;

procedure TKCustomDBBarcode.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	FAddOnDataLink.DataSource := Value;
	if ( Value <> nil ) then
		Value.FreeNotification( Self );
end;

end.
