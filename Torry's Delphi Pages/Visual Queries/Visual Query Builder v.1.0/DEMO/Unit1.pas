unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, VQBuilder, DBEBDE, StdCtrls, Menus, ExtCtrls,
  Grids, DBGrids;

type
  TForm1 = class(TForm)
    SQLDialog1: TSQLDialog;
    DBEngineBDE1: TDBEngineBDE;
    Database1: TDatabase;
    Query1: TQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    Builder1: TMenuItem;
    Execute1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Execute1Click(Sender: TObject);
    procedure SQLDialog1LoadModel(Sender: TObject; var Content: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
   SQLDialog1.Execute;
end;

procedure TForm1.Execute1Click(Sender: TObject);
begin
   if SQLDialog1.Execute then
   begin
      Memo1.Lines.Text:= SQLDialog1.ResultSQL;
      try
        Query1.Close;
        Query1.SQL.Text:= Memo1.Lines.Text;
        Query1.Open;
      except
        Query1.Close;
      end;
   end;
end;

procedure TForm1.SQLDialog1LoadModel(Sender: TObject; var Content: String);
var
  Path: String;
  List: TStringList;
begin
   Path:= ExtractFilePath(Application.ExeName) + 'BDETest.sql';
   List:= TstringList.Create;
   List.LoadFromFile(Path);
   Content:= List.Text;
   List.Free;
end;

end.
