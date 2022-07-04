unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids, Spin;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    CTable1: TCTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Find: TButton;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    procedure FindClick(Sender: TObject);
    procedure CTable1FetchProgress(Sender: TObject; Progress,
      MaxProgress: Integer; var Status: TEventStatus);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.FindClick(Sender: TObject);
begin
  if not CTable1.FindKey([SpinEdit1.Value])
     then ShowMessage('Record not found');
end;

procedure TMainForm.CTable1FetchProgress(Sender: TObject; Progress,
  MaxProgress: Integer; var Status: TEventStatus);
begin
  Label1.Caption := IntTOStr(CTable1.RecordCount);
end;

end.
