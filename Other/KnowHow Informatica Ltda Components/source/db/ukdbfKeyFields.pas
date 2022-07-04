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

unit ukdbfKeyFields;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, checklst, Buttons, DB, ExtCtrls, ukrDBUtils, ukdbTables;

type

	TfrmKeyFields = class( TForm )
		pnBtns: TPanel;
		btOk: TBitBtn;
		btCancel: TBitBtn;
		btHelp: TBitBtn;
		pncbList: TPanel;
		cbListFields: TCheckListBox;
		procedure btHelpClick( Sender: TObject );

	private
		FFldTypes: TFieldTypes;

		procedure AdjustForm;
		function GetKeyFld: string;
		procedure SetFields( DataSet: TDataSet );
		procedure SetKeyFld( const Value: string );

	public
		property KeyFields: string
						 read GetKeyFld write SetKeyFld;
		property ValidFieldTypes: TFieldTypes
						 read FFldTypes write FFldTypes;

	end;

{##NI##}

function EditKeyFields( AQuery: TKQuery; const ACaption: string; var KeyFlds: string;
	ValidFieldTypes: TFieldTypes ): Boolean;

implementation

uses
	uksyConsts, uksyUtils, ukdbResStr, ukdbUtils;

{$R *.DFM}

type

	EKDBDFKeyFields = class( EKDB );

{---------------------------- Public Editor Function ---------------------------}

function EditKeyFields( AQuery: TKQuery; const ACaption: string; var KeyFlds: string;
	ValidFieldTypes: TFieldTypes ): Boolean;
var
	frmKeyFields: TfrmKeyFields;
begin
	AQuery.FieldDefs.Update;
	try
		if ( AQuery.FieldDefs.Count = 0 ) then
			RaiseException( EKDBDFKeyFields, sErrDSInvFldCount );
		if ( not CheckStrings( AQuery.SQL ) ) then
			RaiseException( EKDBDFKeyFields, sErrDSInvSQLStatement );
		if ( ValidFieldTypes = [] ) then
			ValidFieldTypes := VALID_KEYFIELD_TYPES
		else if ( ValidFieldTypes - VALID_KEYFIELD_TYPES <> [] ) then
			RaiseException( EKDBDFKeyFields, sErrDSInvFldTypes );
		ForceFields( AQuery, KeyFlds, ValidFieldTypes );
	except
{ to allow a next time editing with some copy/paste operation }
		on EKDBDFKeyFields do
		begin
			AQuery.KeyFields := '';
			raise;
		end;
		on EKRDBUtils do
		begin
  		AQuery.KeyFields := '';
			raise;
		end;
	end;
	frmKeyFields := TfrmKeyFields.Create( nil );
	try
		frmKeyFields.Caption := Format( frmKeyFields.Caption, [ACaption] );
		frmKeyFields.ValidFieldTypes := ValidFieldTypes;
		frmKeyFields.SetFields( AQuery );
		frmKeyFields.KeyFields := KeyFlds;
		Result := ( frmKeyFields.ShowModal = mrOk );
		if Result then
			KeyFlds := frmKeyFields.KeyFields;
	finally
		frmKeyFields.Free;
	end;
end;

{-------------------------- Form Editor Implementation -------------------------}

procedure TfrmKeyFields.AdjustForm;
var
	i,
	iAux,
	iWMax,
	iHMin: Integer;
begin
	iWMax := 0;
	for i := 0 to cbListFields.Items.Count - 1 do
	begin
		iAux := Canvas.TextWidth( cbListFields.Items[i] );
		if ( iAux > iWMax ) then
			iWMax := iAux;
	end;
	Inc( iWMax, 5 );
	if ( iWMax > ClientWidth ) then
		ClientWidth := iWMax;
	iHMin := ( ( cbListFields.Items.Count * cbListFields.ItemHeight ) + pnBtns.Height + 11 );
	if ( iHMin < ClientHeight ) then
		ClientHeight := iHMin + 3;
end;

procedure TfrmKeyFields.SetFields( DataSet: TDataSet );
var
	i: Integer;
begin
	for i := 0 to DataSet.FieldDefs.Count - 1 do
		if ( DataSet.FieldDefs.Items[i].DataType in ValidFieldTypes ) then
			cbListFields.Items.Add( DataSet.FieldDefs.Items[i].Name );
	AdjustForm;
end;

function TfrmKeyFields.GetKeyFld: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to cbListFields.Items.Count - 1 do
		if cbListFields.Checked[i] then
			Result := Result + cbListFields.Items[i] + CH_LIST_TOKEN;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ), 1 );
end;

procedure TfrmKeyFields.SetKeyFld( const Value: string );
var
	i: Integer;
	sl: TStrings;
begin
	if ( not CheckStr( Value ) ) then
		Exit;
	sl := TStringList.Create;
	try
		ExtractStrings( Value, CH_LIST_TOKEN, sl );
		for i := 0 to sl.Count - 1 do
{ The IndexOf must always return a valid value! }
			cbListFields.Checked[cbListFields.Items.IndexOf( sl[i] )] := True;
	finally
		sl.Free;
	end;
end;

procedure TfrmKeyFields.btHelpClick( Sender: TObject );
begin
  NotYetImplemented;
end;

end.
