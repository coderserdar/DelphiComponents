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

unit _mDBSplit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  _mDB, Menus, DBCtrls, Buttons, ukdcCtrls, ExtCtrls, Grids, DBGrids, Db,
  DBTables, ukdbTables;

type
	TfmDBSplit = class( TfmDB )
		dgIndex: TDBGrid;
    sptIndex: TSplitter;
		miShowNavigation: TMenuItem;
    srcData: TDataSource;
    qyData: TKQuery;
		procedure miCheckOptionClick(Sender: TObject);
    procedure srcIndexStateChange(Sender: TObject);
    procedure BeforeEditModes(DataSet: TDataSet);
    procedure qyDataAfterCommit(DataSet: TDataSet);
    procedure dgIndexEnter(Sender: TObject);
    procedure dgIndexExit(Sender: TObject);
    procedure sptIndexMoved(Sender: TObject);
		
	private
		function GetShowIndex: Boolean;
		
	protected
		procedure UpdateControls; virtual;

	public
		property ShowIndex: Boolean
						 read GetShowIndex;

	end;

implementation

{$R *.DFM}

uses
	ukrDBUtils;

function TfmDBSplit.GetShowIndex: Boolean;
begin
	Result := ( miShowNavigation.Visible and miShowNavigation.Enabled and
		miShowNavigation.Checked );
end;

procedure TfmDBSplit.UpdateControls;
const
	dgCOLOR: array[Boolean] of TColor = ( clSilver, clWhite );
begin
	dgIndex.Color := dgCOLOR[dgIndex.Enabled];
end;

procedure TfmDBSplit.miCheckOptionClick(Sender: TObject);
begin
	inherited;
	sptIndex.Visible := ShowIndex;
	dgIndex.Visible := ShowIndex;
end;

procedure TfmDBSplit.srcIndexStateChange(Sender: TObject);
begin
	if ( not CheckDataSource( DataSource ) ) then
		Exit;
	if ( not ( DataSource.State in dsEditModes ) ) then
		dgIndex.Enabled := true;
	UpdateControls;
end;

procedure TfmDBSplit.BeforeEditModes(DataSet: TDataSet);
begin
	dgIndex.Enabled := false;
	UpdateControls;
end;

procedure TfmDBSplit.qyDataAfterCommit(DataSet: TDataSet);
begin
	UpdateRowCount;
	UpdateControls;
end;

procedure TfmDBSplit.dgIndexEnter(Sender: TObject);
begin
	TDBGrid( Sender ).Columns[0].Title.Font.Color := clRed;
end;

procedure TfmDBSplit.dgIndexExit(Sender: TObject);
begin
	TDBGrid( Sender ).Columns[0].Title.Font.Color := clWindowText;
end;

procedure TfmDBSplit.sptIndexMoved(Sender: TObject);
begin
  inherited;
	dgIndex.Columns[0].Width := ( TSplitter( Sender ).Left - 28 );
end;

end.
