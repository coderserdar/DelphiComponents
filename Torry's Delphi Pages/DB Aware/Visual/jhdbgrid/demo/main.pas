unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, JanHDBGrid, DB, ADODB, StdCtrls, Spin;

type
  TfmMain = class(TForm)
    JanHDBGrid: TJanHDBGrid;
    DataSource: TDataSource;
    ADOConnection: TADOConnection;
    btClose: TButton;
    ADOTable: TADOTable;
    lbMove: TLabel;
    spinWheel: TSpinEdit;
    lbRows: TLabel;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JanHDBGridMouseWheel(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
    ADOConnection.ConnectionString:='Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;Data Source=database.mdb;Mode=Share Deny None;Extended Properties="";Persist Security Info=False';
    ADOConnection.Connected:=true;

    ADOTable.Active:=true;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
    ADOTable.Active:=false;

    ADOConnection.Connected:=false;
end;

procedure TfmMain.btCloseClick(Sender: TObject);
begin
    fmMain.Close;
end;

procedure TfmMain.JanHDBGridMouseWheel(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    JanHDBGrid.MoveByRows:=spinWheel.Value;
end;

end.
