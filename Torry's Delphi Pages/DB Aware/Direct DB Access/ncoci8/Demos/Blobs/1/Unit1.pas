// --------------------------------------------------------
// To run this demo you must at first run script: BLOBS.SQL
// --------------------------------------------------------
// Take a look on OCIUpdateSQL1.
// For editing dataset with blobs, you must have setup:
// TOCIQuery:
//    UpdateSQL = <my update sql>
// TOCIUpdateSQL:
//    LockMode = lmPessimistic
//    LockPoint = lpImmediate
//    TableName = '<my table>'

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, StdCtrls, ExtCtrls, DBCtrls, Grids,
  DBGrids, NCOciUpdateSQL, NCSQLMon;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OCIUpdateSQL1: TOCIUpdateSQL;
    OCITransactionManager1: TOCITransactionManager;
    NCSQLMonitorClient1: TNCSQLMonitorClient;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OCITransactionManager1StateChanged(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    FDS: TDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
    FDS.Edit;
    if OpenDialog1.Execute then
        (FDS.FieldByName('F2') as TBlobField).LoadFromFile(OpenDialog1.FileName);
    FDS.Post;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    FDS.Edit;
    if OpenDialog2.Execute then
        (FDS.FieldByName('F3') as TMemoField).LoadFromFile(OpenDialog2.FileName);
    FDS.Post;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    FDS.Close;
    if FDS is TOCIDataSet then
        TOCIQuery(FDS).Unprepare;
    FDS.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    FDS := OCIQuery1;
    FDS.Open;
end;

procedure TForm1.OCITransactionManager1StateChanged(Sender: TObject);
begin
    if OCITransactionManager1.InTransaction then
        Label1.Caption := 'TX active'
    else
        Label1.Caption := '';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
    OCITransactionManager1.StartTransaction;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
    OCITransactionManager1.Commit;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
    OCITransactionManager1.Rollback;
end;

end.
