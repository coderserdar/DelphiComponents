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
    CPersistedRecordset1: TCPersistedRecordset;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  if SaveDialog1.Execute then
     begin
       DeleteFile(SaveDialog1.FileName);
       CTable1.SaveRecordset(SaveDialog1.FileName);
     end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
     begin
       CPersistedRecordset1.Close;
       CPersistedRecordset1.FileName := OpenDialog1.FileName;
       CPersistedRecordset1.Open;
     end;
end;

end.
