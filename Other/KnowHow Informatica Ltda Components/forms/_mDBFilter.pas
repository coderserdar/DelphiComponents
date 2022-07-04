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

unit _mDBFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  _mDB, Menus, DBCtrls, Buttons, ukdcCtrls, ExtCtrls, StdCtrls;

type
  TfmDBFilter = class(TfmDB)
    cxFilter: TComboBox;
    procedure FormCreate(Sender: TObject);

	end;

implementation

{$R *.DFM}

procedure TfmDBFilter.FormCreate(Sender: TObject);
begin
	inherited;
	cxFilter.ItemIndex := 0;
end;

end.
