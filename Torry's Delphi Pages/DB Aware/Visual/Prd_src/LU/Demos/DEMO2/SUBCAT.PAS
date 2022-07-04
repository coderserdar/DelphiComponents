unit Subcat;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Grids, DBGrids, DB, DBTables, ExtCtrls, Mask, DBCtrls,
  Dblup2;

type
  TSubCatDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    DataSourceSubCat: TDataSource;
    DBEditSubCatName: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    DBLookupComboPlus1: TDBLookupComboPlus;
    TableSubCat2: TTable;
    DataSourceSubCat2: TDataSource;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SubCatDlg: TSubCatDlg;

implementation
Uses
  Demomain;

{$R *.DFM}

procedure TSubCatDlg.FormCreate(Sender: TObject);
begin
  DataSourceSubCat.DataSet := DemoMain.Form1.DataSourceSubCat.DataSet;
end;

end.
