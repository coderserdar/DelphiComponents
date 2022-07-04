unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ExtCtrls, DBCtrls, Grids, DBGrids, vgDBExpr, StdCtrls,
  vgStndrt;

type
  TMainForm = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    dsCust: TDataSource;
    DBGrid2: TDBGrid;
    dsOrders: TDataSource;
    dbB: TDBExpression;
    dbA: TDBExpression;
    lbA: TLabel;
    lbB: TLabel;
    Label1: TLabel;
    edA: TEdit;
    edB: TEdit;
    Label2: TLabel;
    cmApply: TButton;
    afIniFile: TAppIniFile;
    PropStorage1: TPropStorage;
    procedure dbAChange(Sender: TObject);
    procedure dsCustDataChange(Sender: TObject; Field: TField);
    procedure cmApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure CalcAgg;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses vgUtils, bDM;

{$R *.DFM}

procedure TMainForm.dbAChange(Sender: TObject);
begin
  lbA.Caption := 'Simple expression: ' + VarToStr(dbA.Value);
end;

procedure TMainForm.CalcAgg;
begin
  lbB.Caption := 'Aggregate: ' + VarToStr(dbB.Value);
end;

procedure TMainForm.dsCustDataChange(Sender: TObject; Field: TField);
begin
  if (Field = nil) and (dsCust.State = dsBrowse) then CalcAgg;
end;

procedure TMainForm.cmApplyClick(Sender: TObject);
begin
  dbA.Expression := edA.Text;  // dbA evaluates automatically
  dbB.Expression := edB.Text;
  CalcAgg;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cmApplyClick(nil);
end;

end.
