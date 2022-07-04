(*
Edit the SQLClientDataSet's FileName property to point at the biolife client
data set supplied with Kylix
*)
unit Xpress;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  Provider, SqlExpr, compress, CompCLX, QGrids, QDBGrids, DB, DBClient,
  DBLocal, DBLocalS, QExtCtrls, QDBCtrls, QStdCtrls;

type
  TForm1 = class(TForm)
    SQLClientDataSet1: TSQLClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    CDBImage1: TCDBImage;
    Compress1: TCompress;
    DBNavigator1: TDBNavigator;
    CDBMemo1: TCDBMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLClientDataSet1.FileName := 'biolife.cds';
  SQLClientDataSet1.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  SaveFileName : String;
begin
  with SaveDialog1 do
  begin
    Title := 'Save Data Set';
    DefaultExt := 'cds';
    Filter := 'Client Data Sets (*.cds)|*.CDS';
    if (Execute) then
    begin
      SaveFileName := FileName;
      SQLClientDataSet1.SaveToFile(SaveFileName, dfBinary);
    end;
  end;
end;

procedure TForm1.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
begin
  if (Button = nbInsert) then
  begin
    with OpenDialog1 do
    begin
      Title := 'Select Image';
      Filter := 'Image Files|*.JPG;*.BMP';
      if (Execute) then
      begin
        SQLClientDataSet1.FieldByName('Graphic').AsVariant := FileName;
      end;
    end;
  end;
end;

end.
