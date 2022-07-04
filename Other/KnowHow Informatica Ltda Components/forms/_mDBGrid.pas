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

unit _mDBGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	_mDB, Menus, DBCtrls, Buttons, ukdcCtrls, ExtCtrls, Grids, DBGrids,
  ComCtrls, Db, DBTables, ukdbTables;

type
  TfmDBGrid = class(TfmDB)
		pnIndex: TPanel;
		lvIndex: TListView;
		Splitter1: TSplitter;
		ilIndice: TImageList;
		srcGrid: TDataSource;
		pnGrid: TPanel;
		dgCadastro: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure qyDataAfterCommit(DataSet: TDataSet);
    procedure BeforeEditModes(DataSet: TDataSet);
    procedure srcGridStateChange(Sender: TObject);
    procedure lvIndexChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
    procedure lvIndexKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvIndexClick(Sender: TObject);
    procedure dgCadastroKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

	protected
		procedure UpdateControls; virtual;
		procedure SynchronizeDataset; virtual; abstract;

	end;

implementation

{$R *.DFM}

uses
	uksyShortcuts, ukrDBUtils;

procedure TfmDBGrid.UpdateControls;
const
	lvCOLOR: array[Boolean] of TColor = ( clSilver, clWhite );
begin
	lvIndex.Color := lvCOLOR[lvIndex.Enabled];
end;

procedure TfmDBGrid.FormCreate(Sender: TObject);
begin
	inherited;
	lvIndex.Items[0].Selected := true;
end;

procedure TfmDBGrid.BeforeEditModes(DataSet: TDataSet);
begin
	lvIndex.Enabled := false;
	UpdateControls;
end;

procedure TfmDBGrid.srcGridStateChange(Sender: TObject);
begin
	if ( not CheckDataSource( DataSource ) ) then
		Exit;
	if ( not ( DataSource.State in dsEditModes ) ) then
	begin
		lvIndex.Enabled := true;
		UpdateControls;
	end;
end;

procedure TfmDBGrid.qyDataAfterCommit(DataSet: TDataSet);
begin
	UpdateRowCount;
end;

procedure TfmDBGrid.lvIndexChanging(Sender: TObject; Item: TListItem;
	Change: TItemChange; var AllowChange: Boolean);
begin
	AllowChange := ( Change = ctText ) or
    ( CheckDatasource( DataSource ) and ( DataSource.State = dsBrowse ) );
end;

procedure TfmDBGrid.lvIndexKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	SynchronizeDataset;
end;

procedure TfmDBGrid.lvIndexClick(Sender: TObject);
begin
	SynchronizeDataset;
end;

procedure TfmDBGrid.dgCadastroKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if lvIndex.Enabled and ( ShortCut( Key, Shift ) = SC_CTRL_I ) then
    lvIndex.SetFocus;
end;

end.
