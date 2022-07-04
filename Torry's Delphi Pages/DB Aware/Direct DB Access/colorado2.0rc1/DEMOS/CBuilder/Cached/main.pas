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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
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

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CTable1.ApplyUpdates;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  CTable1.CancelUpdates;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if Button3.Caption = 'Show changes' then
     begin
       CTable1.Filtered := TRUE;
       Button3.Caption := 'Show all';
     end else
           begin
             CTable1.Filtered := FALSE;
             Button3.Caption := 'Show changes';
           end;
end;

end.
