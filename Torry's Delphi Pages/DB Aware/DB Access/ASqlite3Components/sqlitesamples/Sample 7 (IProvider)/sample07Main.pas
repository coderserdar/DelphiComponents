// This is a simple sample of using a iprovider interface. Since
// IPRovider manages his own resultset, don't forget to set
// unidirectional property of tquery to true. Otherwize you will
// have two full resultsets in memory!
// aducom software
// (c) 2007
unit Sample07Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ExtCtrls, DBCtrls, Grids, DBGrids, ASGSQLite3, StdCtrls,
  DBClient, Provider;

type
  TForm1 = class(TForm)
    ASQLite3DB1: TASQLite3DB;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    ASQLite3Query1: TASQLite3Query;
    Button1: TButton;
    DataSetProvider: TDataSetProvider;
    ClientDataSet: TClientDataSet;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClientDataSetAfterPost(DataSet: TDataSet);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    bOk : boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 ASQLite3Query1.Open;
 ClientDataSet.Open;
 bOk := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 ClientDataSet.ApplyUpdates(0);
 bOk := true;
end;

procedure TForm1.ClientDataSetAfterPost(DataSet: TDataSet);
begin
 bOk := false;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := true;
 if not bOK then
   if MessageDlg('Updates are not saved into the databaase. Exit now?',
       mtConfirmation, [mbYes, mbNo], 0) = mrNo then CanClose := false;
end;

end.
