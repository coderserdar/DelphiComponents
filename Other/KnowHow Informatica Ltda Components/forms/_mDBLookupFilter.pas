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

unit _mDBLookupFilter;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  _mDB, Menus, DBCtrls, Buttons, ukdcCtrls, ExtCtrls, Db, DBTables;

type
  TfmDBLookupFilter = class(TfmDB)
    dcxFilter: TDBLookupComboBox;
    dsLookup: TDataSource;
    quLookup: TQuery;
		procedure FormCreate(Sender: TObject);
		procedure cxFilterChange(Sender: TObject);
		procedure dcxFilterKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure quLookupAfterScroll(DataSet: TDataSet);

	private
		FFiltering: Boolean;

	public
		procedure FilterChange; virtual;

	end;

implementation

{$R *.DFM}

procedure TfmDBLookupFilter.FormCreate(Sender: TObject);
begin
	inherited;
	quLookup.Open;
  FFiltering := false;
	dcxFilter.KeyValue := 0;
end;

procedure TfmDBLookupFilter.FilterChange;
begin
{ foo }
end;

procedure TfmDBLookupFilter.cxFilterChange(Sender: TObject);
begin
	if FFiltering then
		Exit;
	FFiltering := true;
	try
		FilterChange;
	finally
    FFiltering := false;
	end;
end;

procedure TfmDBLookupFilter.dcxFilterKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	inherited;
	if FFiltering then
		Exit;
	FFiltering := true;
	try
		FilterChange;
	finally
    FFiltering := false;
	end;
end;

procedure TfmDBLookupFilter.quLookupAfterScroll(DataSet: TDataSet);
begin
	inherited;
	if FFiltering then
		Exit;
	FFiltering := true;
	try
		FilterChange;
	finally
		FFiltering := false;
	end;
end;

end.
