unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    CTable1: TCTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Find: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    procedure FindClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  if not CTable1.Locate('LastName', Edit1.Text, [loPartialKey], loFromBeginningForward)
     then ShowMessage('Record not found');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if not CTable1.Locate('LastName', Edit1.Text, [loPartialKey], loFromCurrentForward)
     then ShowMessage('Record not found');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if not CTable1.Locate('LastName', Edit1.Text, [loPartialKey], loFromEndBackward)
     then ShowMessage('Record not found');
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if not CTable1.Locate('LastName', Edit1.Text, [loPartialKey], loFromCurrentBackward)
     then ShowMessage('Record not found');
end;

end.
