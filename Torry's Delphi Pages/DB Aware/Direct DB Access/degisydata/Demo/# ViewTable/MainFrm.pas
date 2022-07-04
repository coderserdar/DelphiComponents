unit MainFrm;
{$I DEGISYDATA.INC}
interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
{$IFDEF VCL6}
  Variants,
{$ENDIF}
  DsDdaTable,
  DB,
  DsClrTable,
  DsDatabase,
  Buttons,
  DsPdxTable,
  DsDbfTable,
  Grids,
  DBGrids;

type
  TMainForm = class(TForm)
    DDA_DB: TDsDatabase;
    TBL_CLR: TDsClrTable;
    TBL_DDA: TDsDdaTable;
    GRID: TDBGrid;
    TBL_DBF: TDsDbfTable;
    TBL_PDX: TDsPdxTable;
    Label1: TLabel;
    CB_ACTIVETBL: TComboBox;
    CB_FILTER: TComboBox;
    DS: TDataSource;
    Label2: TLabel;
    procedure CB_ACTIVETBLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CB_FILTERClick(Sender: TObject);
    procedure CB_FILTERKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.CB_ACTIVETBLChange(Sender: TObject);
begin
  case( CB_ACTIVETBL.ItemIndex )of
  0: DS.DataSet := TBL_CLR;
  1: DS.DataSet := TBL_DBF;
  2: DS.DataSet := TBL_DDA;
  3: DS.DataSet := TBL_PDX;
  end;
  CB_FILTER.Text := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TBL_CLR.Active := True;
  TBL_DBF.Active := True;
  TBL_DDA.Active := True;
  TBL_PDX.Active := True;
  CB_ACTIVETBL.ItemIndex := 0;
  CB_ACTIVETBLChange(CB_ACTIVETBL);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TBL_CLR.Active := False;
  TBL_DBF.Active := False;
  TBL_DDA.Active := False;
  TBL_PDX.Active := False;
end;

procedure TMainForm.CB_FILTERClick(Sender: TObject);
begin
  DS.DataSet.Filter := CB_FILTER.Text;
end;

procedure TMainForm.CB_FILTERKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case( Key )of
  VK_RETURN: CB_FILTERClick(CB_FILTER);
  end;
end;

end.
