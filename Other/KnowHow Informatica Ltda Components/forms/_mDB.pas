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

unit _mDB;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	Dialogs, ukfCtrls, ukfClasses, Menus, DBCtrls, Buttons, ukdcCtrls,
  ExtCtrls;

type
	TfmDB = class( TKChild )
    pnStatus: TPanel;
    pnRight: TPanel;
    sbSearch: TKDBSpeedButton;
    sbPrint: TKDBSpeedButton;
		dnNavigator: TDBNavigator;
    pmRowCount: TPopupMenu;
    miRowCount: TMenuItem;
    pnRowCount: TPanel;
		miPSep: TMenuItem;
		miAtualizar: TMenuItem;
		pnLeft: TPanel;
		procedure miCheckOptionClick(Sender: TObject);

	private
		FSQLText: string;

		procedure SetSQLText( Value: string );
		function GetShowRowCount: Boolean;

	protected
		procedure CloseQueries;
		procedure UpdateRowCount;
		procedure DoDataSourceChanged; override;

		property ShowRowCount: Boolean
						 read GetShowRowCount;
		property SQLText: string
						 read FSQLText write SetSQLText;

	end;

implementation

{$R *.DFM}

uses
	DBTables, uksyUtils, ukrDBUtils;

procedure TfmDB.CloseQueries;
var
  i: Integer;
begin
	for i := 0 to ComponentCount - 1 do
		if ( Components[i] is TQuery ) then
			with TQuery( Components[i] ) do
				Close;
end;

procedure TfmDB.SetSQLText( Value: string );
begin
	if ( not CheckStrEqual( FSQLText, Value ) ) then
	begin
		FSQLText := Value;
		UpdateRowCount;
	end;
end;

procedure TfmDB.UpdateRowCount;

	function MyFormat( Value: Integer ): String;
	begin
		if ( Value < 1000 ) then
			Result := IntToStr( Value )
		else
		Result := IntToStr( Value div 1000 ) + '.' +
							Format( '%3.3d', [( Value mod 1000)] );
	end;

var
	s: String;
begin
	if ( not ShowRowCount ) or
		 ( not CheckTrimStr( SQLText ) ) or
		 ( not CheckDataSource( DataSource ) ) or
		 ( not DataSource.DataSet.Active ) then
	begin
		pnRowCount.Caption := s;
		Exit;
	end;
	with TQuery.Create( nil ) do
		try
			DatabaseName := TDBDataSet( Self.DataSource.Dataset ).DatabaseName;
			SQL.Text := SQLText;
			try
				Open;
				s := Fields[0].AsString;
				Close;
			except
				s := '';
			end;
		finally
			Free;
		end;
	if ( s <> '' ) then
		pnRowCount.Caption := MyFormat( StrToInt(s) ) + ' registros'
	else
		pnRowCount.Caption := s;
end;

procedure TfmDB.DoDataSourceChanged;
begin
	inherited DoDataSourceChanged;
	dnNavigator.DataSource := DataSource;
	sbSearch.DataSource := DataSource;
	sbPrint.DataSource := DataSource;
end;

function TfmDB.GetShowRowCount: Boolean;
begin
	Result := ( miRowCount.Visible and miRowCount.Enabled and miRowCount.Checked );
end;

procedure TfmDB.miCheckOptionClick(Sender: TObject);
begin
	( Sender as TMenuItem ).Checked := not ( Sender as TMenuItem ).Checked;
	pnRowCount.Visible := ShowRowCount;
end;

end.
