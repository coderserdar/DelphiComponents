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

unit _mDBSplitLookupFilter;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	_mDBSplit, Db, DBTables, ukdbTables, Menus, Grids, DBGrids, DBCtrls,
  Buttons, ukdcCtrls, ExtCtrls;

type
  TfmDBSplitLookupFilter = class(TfmDBSplit)
    dsLookup: TDataSource;
    quLookup: TQuery;
    dcxFilter: TDBLookupComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cxFilterChange(Sender: TObject);
    procedure dcxFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure quLookupAfterScroll(DataSet: TDataSet);
    procedure srcIndexStateChange(Sender: TObject);

	protected
		procedure UpdateControls; override;

	public
		procedure FilterChange; virtual;
		
	end;

implementation

{$R *.DFM}

procedure TfmDBSplitLookupFilter.FormCreate(Sender: TObject);
begin
	inherited;
	quLookup.Open;
	dcxFilter.KeyValue := 0;
end;

procedure TfmDBSplitLookupFilter.UpdateControls;
begin
	inherited UpdateControls;
	dcxFilter.Enabled := dgIndex.Enabled;
end;

procedure TfmDBSplitLookupFilter.FilterChange;
begin
{ foo }
end;

procedure TfmDBSplitLookupFilter.cxFilterChange(Sender: TObject);
begin
	inherited;
	FilterChange;
end;

procedure TfmDBSplitLookupFilter.dcxFilterKeyUp(Sender: TObject;
	var Key: Word; Shift: TShiftState);
begin
	inherited;
	FilterChange;
end;

procedure TfmDBSplitLookupFilter.quLookupAfterScroll(DataSet: TDataSet);
begin
  inherited;
	FilterChange;
end;

procedure TfmDBSplitLookupFilter.srcIndexStateChange(Sender: TObject);
begin
  inherited;
	UpdateControls;
end;

end.
