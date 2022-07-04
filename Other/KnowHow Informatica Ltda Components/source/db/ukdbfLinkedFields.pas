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

unit ukdbfLinkedFields;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Db, DBTables, StdCtrls, ExtCtrls, Buttons, ukrDBUtils, ukdbTables, ukrClasses;

type

	TfrmLinkedFields = class( TForm )
		tbDest: TTable;
		pnBtns: TPanel;
    pnLinks: TPanel;
    pnFields: TPanel;
    grpItem: TGroupBox;
    pnSourceFields: TPanel;
		lbItemFields: TListBox;
		grpSource: TGroupBox;
    Panel1: TPanel;
		lbSourceFields: TListBox;
    btAddLink: TBitBtn;
    btOk: TBitBtn;
		btCancel: TBitBtn;
    btHelp: TBitBtn;
    pnJoinedFlds: TPanel;
    grpJoined: TGroupBox;
    pnGrpJoined: TPanel;
		lbJoinedFields: TListBox;
		pnJoinedBtns: TPanel;
    btDelete: TBitBtn;
    btClear: TBitBtn;
    btUp: TBitBtn;
    btDown: TBitBtn;

		procedure btAddLinkClick( Sender: TObject );
		procedure btDeleteClick( Sender: TObject );
		procedure btClearClick( Sender: TObject );
		procedure AlllbClick( Sender: TObject );
		procedure FormShow( Sender: TObject );
		procedure lbJoinedFieldsDragDrop( Sender, Source: TObject; X, Y: Integer );
		procedure lbJoinedFieldsDragOver( Sender, Source: TObject; X, Y: Integer;
			State: TDragState; var Accept: Boolean );
		procedure btMovementClick( Sender: TObject );
    procedure btHelpClick( Sender: TObject );
		procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

	private
		FFldTypes: TFieldTypes;
		FKTDBI: TKTableDBIntegrityItem;
		FLinkedFields: TKStrings;
		FPRKF: Boolean;

		procedure AdjustLists;
		procedure RebuildLists;
		procedure SetJoinButtons;
		procedure SetListBox( DataSet: TTable; ListBox: TListBox; IsPrimKeyFlds: Boolean );
		procedure SetDBIntegrityItem( Value: TKTableDBIntegrityItem );
		procedure SetLinkedFields( const Value: TKStrings );
		function GetLinkedFields: TKStrings;

	public
		property DBIntegrityItem: TKTableDBIntegrityItem
						 read FKTDBI write SetDBIntegrityItem;
		property ValidFieldTypes: TFieldTypes
						 read FFldTypes write FFldTypes;
		property LinkedFields: TKStrings
						 read GetLinkedFields write SetLinkedFields;
		property IsPrimKeyFlds: Boolean
						 read FPRKF write FPRKF;

	end;

{##NI##}

function EditLinkedFields( ADBIntegrityItem: TKTableDBIntegrityItem; ALinkedFlds: TKStrings;
	AValidFieldTypes: TFieldTypes; IsPrimKeyFields: Boolean ): Boolean;

implementation

uses
	uksyConsts, uksyResStr, uksyUtils, ukdbResStr, ukdbUtils;

{$R *.DFM}

const
	JOINED_PATTERN = ( '%s->%s' );

	TAG_LBITEMFLD   = 100;
	TAG_LBSOURCEFLD = 200;
	TAG_LBJOINEDFLD = 300;
	TAG_BTUP        = 500;
	TAG_BTDOWN      = 600;

type

	EKDBDFLinkedFields = class( EKDB );

	TDataSetHack = class( TDBDataSet );

{---------------------------- Public Editor Function ---------------------------}

function EditLinkedFields( ADBIntegrityItem: TKTableDBIntegrityItem; ALinkedFlds: TKStrings;
	AValidFieldTypes: TFieldTypes; IsPrimKeyFields: Boolean ): Boolean;
const
	PATTERN: array[Boolean] of string[16] = ( '', '(KeyFields Only)' );
var
	frmLinkedFields: TfrmLinkedFields;
begin
	ForceObject( ALinkedFlds );
	with ADBIntegrityItem.Owner.DataSet do
		FieldDefs.Update;
	if ( AValidFieldTypes = [] ) then
		AValidFieldTypes := VALID_LINKEDFIELD_TYPES
	else if ( AValidFieldTypes - VALID_LINKEDFIELD_TYPES <> [] ) then
		RaiseException( EKDBDFLinkedFields, sErrDSInvFldTypes );
	try
		if IsPrimKeyFields then
			ForcePrimaryKeyFieldsEx( ADBIntegrityItem.Owner.DataSet, ALinkedFlds, AValidFieldTypes,
				stValues )
		else
			ForceFieldsEx( ADBIntegrityItem.Owner.DataSet, ALinkedFlds, AValidFieldTypes, stValues );
	except
		on E: EKKernel do
{ do not resource }
			if CheckStrContains( 'Invalid field name', E.Message ) then
			begin
				ALinkedFlds.Clear;
				ShowDialogFmt( sError, sErrDSInvDsgnFldName, nil, dsOk, boSoftBug, [E.Message] );
			end;
	end;
	frmLinkedFields := TfrmLinkedFields.Create( nil );
	try
		with frmLinkedFields do
		begin
			FLinkedFields := TKStrings.Create;
			try
				Caption := Format( Caption, [ADBIntegrityItem.Owner.DataSet.Name,
						ADBIntegrityItem.Name, PATTERN[IsPrimKeyFields]] );
				grpSource.Caption := Format( grpSource.Caption, [ADBIntegrityItem.Owner.DataSet.TableName] );
				grpItem.Caption := Format( grpItem.Caption, [ADBIntegrityItem.TableName] );
{ Must be called in this order }
				ValidFieldTypes := AValidFieldTypes;
				IsPrimKeyFlds := IsPrimKeyFields;
				SetListBox( ADBIntegrityItem.Owner.DataSet, lbSourceFields, IsPrimKeyFields );
				LinkedFields := ALinkedFlds;
				DBIntegrityItem := ADBIntegrityItem;
				AdjustLists;
				Result := ( ShowModal = mrOk );
				if Result then
					ALinkedFlds.Assign( LinkedFields );
			finally
				FLinkedFields.Free;
			end;
		end;
	finally
		frmLinkedFields.Free;
	end;
end;

{-------------------------- Form Editor Implementation -------------------------}

procedure TfrmLinkedFields.SetListBox( DataSet: TTable; ListBox: TListBox;
	IsPrimKeyFlds: Boolean );
var
	i: Integer;
	idef: TIndexDef;
begin
	ListBox.Items.Clear;
	for i := 0 to DataSet.FieldDefs.Count - 1 do
		if ( DataSet.FieldDefs.Items[i].DataType in ValidFieldTypes ) then
			if ( not IsPrimKeyFlds ) then
				ListBox.Items.Add( DataSet.FieldDefs.Items[i].Name )
			else
			begin
				idef := DataSet.IndexDefs.GetIndexForFields( DataSet.FieldDefs.Items[i].Name, False );
				if ( CheckObject( idef ) and ( ( idef.Options * [ixPrimary] ) <> [] ) ) then
					ListBox.Items.Add( DataSet.FieldDefs.Items[i].Name );
			end;
	if ( not CheckStrings( ListBox.Items ) ) then
  	RaiseExceptionFmt( EKDBDFLinkedFields, sErrDBIInvFldCount, [DataSet.Name] );
end;

procedure TfrmLinkedFields.SetDBIntegrityItem( Value: TKTableDBIntegrityItem );
begin
	FKTDBI := Value;
	try
		tbDest.Close;
		tbDest.DatabaseName := FKTDBI.DataBaseName;
		tbDest.TableName := FKTDBI.TableName;
		tbDest.FieldDefs.Update;
		SetListBox( tbDest, lbItemFields, IsPrimKeyFlds );
	except
		FKTDBI := nil;
		raise;
	end;
end;

procedure TfrmLinkedFields.SetLinkedFields( const Value: TKStrings );
var
	i: Integer;
begin
	lbJoinedFields.Items.Clear;
	FLinkedFields.Clear;
	if CheckObject( Value ) then
	begin
		FLinkedFields.Assign( Value );
		with Value do
			for i := 0 to Count - 1 do
				lbJoinedFields.Items.Add( Format( JOINED_PATTERN, [Names[i], ValuesByIndex[i]] ) );
	end;
end;

function TfrmLinkedFields.GetLinkedFields: TKStrings;
var
	i: Integer;
begin
	FLinkedFields.Clear;
	for i := 0 to lbJoinedFields.Items.Count-1 do
		FLinkedFields.Add( StringReplace( lbJoinedFields.Items[i], '->', CH_EQUAL_TOKEN, krfAll ) );
	Result := FLinkedFields;	
end;

procedure TfrmLinkedFields.AdjustLists;
var
	i,
	j,
	k: Integer;
begin
	if ( not ( CheckStrings( lbJoinedFields.Items ) and CheckStrings( FLinkedFields ) ) ) then
		Exit;
	for i := 0 to FLinkedFields.Count - 1 do
	begin
		j := lbSourceFields.Items.IndexOf( FLinkedFields.ValuesByIndex[i] );
		k := lbItemFields.Items.IndexOf( FLinkedFields.Names[i] );
		if ( j <> -1 ) then
			lbSourcefields.Items.Delete( j );
		if ( k <> -1 ) then
			lbItemFields.Items.Delete( k );
		if ( j = -1 ) or ( k = -1 ) then
			RaiseExceptionFmt( EKDBDFLinkedFields, sErrDBIInvLinkedFlds, [DBIntegrityItem.Name] );
	end;
end;

procedure TfrmLinkedFields.RebuildLists;
begin
	SetListBox( DBIntegrityItem.Owner.DataSet, lbSourceFields, IsPrimKeyFlds );
	SetListBox( tbDest, lbItemFields, IsPrimKeyFlds );
	AdjustLists;
end;

procedure TfrmLinkedFields.SetJoinButtons;
begin
	btClear.Enabled := ( lbJoinedFields.Items.Count > 0 );
	btDelete.Enabled := ( lbJoinedFields.ItemIndex <> -1 );
	btUp.Enabled := ( lbJoinedFields.ItemIndex > 0 );
	btDown.Enabled := ( lbJoinedFields.ItemIndex < ( lbJoinedFields.Items.Count-1 ) );
end;

{ Form Events }

procedure TfrmLinkedFields.FormShow( Sender: TObject );
begin
	btClear.Enabled := ( lbJoinedFields.Items.Count > 0 );
end;

procedure TfrmLinkedFields.btAddLinkClick( Sender: TObject );
begin
	lbJoinedFields.Items.Add( Format( JOINED_PATTERN,
		[lbItemFields.Items[lbItemFields.ItemIndex],
		 lbSourceFields.Items[lbSourceFields.ItemIndex]] ) );
	lbItemFields.Items.Delete( lbItemFields.ItemIndex );
	lbSourceFields.Items.Delete( lbSourceFields.ItemIndex );
	SetJoinButtons;
	if CheckStrings( lbItemFields.Items ) then
		lbItemFields.SetFocus
	else if CheckStrings( lbSourceFields.Items ) then
		lbSourceFields.SetFocus
	else
		btOk.SetFocus;
end;

procedure TfrmLinkedFields.btDeleteClick( Sender: TObject );
begin
	lbJoinedFields.Items.Delete( lbJoinedFields.ItemIndex );
	SetJoinButtons;
	RebuildLists;
end;

procedure TfrmLinkedFields.btClearClick( Sender: TObject );
begin
	lbJoinedFields.Items.Clear;
	btClear.Enabled := False;
	btDelete.Enabled := False;
	btUp.Enabled := False;
	btDown.Enabled := False;
	RebuildLists;
end;

procedure TfrmLinkedFields.AlllbClick( Sender: TObject );
begin
	with ( Sender as TListBox ) do
	begin
		if ( Tag = TAG_LBJOINEDFLD ) then
			SetJoinButtons;
		if ( ItemIndex <> -1 ) then
			case Tag of
				TAG_LBITEMFLD:
					btAddLink.Enabled := ( lbSourceFields.ItemIndex <> -1 );
				TAG_LBSOURCEFLD:
					btAddLink.Enabled := ( lbItemFields.ItemIndex <> -1 );
			end;
	end;
end;

procedure TfrmLinkedFields.lbJoinedFieldsDragOver( Sender, Source: TObject;
	X, Y: Integer; State: TDragState; var Accept: Boolean );
begin
	Accept := ( Sender = lbJoinedFields ) and ( Source = lbJoinedFields ) and
		( ( Sender as TListBox ).ItemAtPos( Point( X, Y ), True ) <> -1 );
end;

procedure TfrmLinkedFields.lbJoinedFieldsDragDrop( Sender, Source: TObject;
	X, Y: Integer );
var
	iPos1,
	iPos2: Integer;
begin
	with ( Sender as TListBox ) do
	begin
		iPos1 := ItemAtPos( Point( X, Y ), True );
		iPos2 := ItemIndex;
		if ( iPos1 > 0 ) and ( iPos2 > 0 ) and ( iPos1 <> iPos2 ) then
		begin
			Items.Exchange( iPos1, iPos2 );
			ItemIndex := iPos1;
		end
	end;
end;

procedure TfrmLinkedFields.btMovementClick( Sender: TObject );
const
	FILTER: array[Boolean] of Integer = ( 1, -1 );
begin
	with lbJoinedFields, ( Sender as TBitBtn ) do
	begin
		Enabled := ( ( Tag = TAG_BTUP ) and ( ItemIndex > 0 ) ) or
			         ( ( Tag = TAG_BTDOWN ) and ( ItemIndex < ( Items.Count - 1 ) ) );
		if Enabled then
		begin
			Items.Exchange( ItemIndex, ( ItemIndex + FILTER[( Tag = TAG_BTUP )] ) );
			ItemIndex := ItemIndex + FILTER[( Tag = TAG_BTUP )];
			case Tag of
				TAG_BTUP:
				begin
					btDown.Enabled := ( ItemIndex < ( Items.Count - 1 ) );
					Enabled := ( ItemIndex <> 0 );
				end;
				TAG_BTDOWN:
				begin
					btUp.Enabled := ( ItemIndex > 0 );
					Enabled := ( ItemIndex <> ( Items.Count - 1 ) );
				end;
			end;
		end;
	end;
end;

procedure TfrmLinkedFields.btHelpClick( Sender: TObject );
begin
	NotYetImplemented;
end;

procedure TfrmLinkedFields.FormKeyDown( Sender: TObject; var Key: Word;
  Shift: TShiftState );
begin
	case Key of
		VK_INSERT:
			if ( lbItemFields.ItemIndex <> -1 ) and ( lbSourceFields.ItemIndex <> -1 ) and
				( btAddLink.Enabled ) then
				btAddLink.Click;
		VK_DELETE:
			if ( lbJoinedFields.ItemIndex <> -1 ) and ( btDelete.Enabled ) then
			  btDelete.Click;
	end;
end;

end.
