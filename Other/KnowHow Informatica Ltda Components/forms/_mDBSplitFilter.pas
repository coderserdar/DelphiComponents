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

unit _mDBSplitFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  _mDBSplit, StdCtrls, Db, DBTables, Menus, Grids, DBGrids, DBCtrls,
  Buttons, ukdcCtrls, ExtCtrls, ukdbTables;

type
  TfmDBSplitFilter = class(TfmDBSplit)
    cxFilter: TComboBox;
    procedure FormCreate(Sender: TObject);

	protected
		procedure UpdateControls; override;

	end;

implementation

{$R *.DFM}

procedure TfmDBSplitFilter.FormCreate(Sender: TObject);
begin
	inherited;
	cxFilter.ItemIndex := 0;
end;

procedure TfmDBSplitFilter.UpdateControls;
begin
	inherited;
	cxFilter.Enabled := dgIndex.Enabled;
end;

end.
