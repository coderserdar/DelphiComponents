unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_workbook;

type
  TForm2 = class(TForm)
    API_workbook1: TAPI_workbook;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button4: TButton;
    Button5: TButton;
    Button3: TButton;
    Button6: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  api_workbook1.Pages:= 1;
  api_workbook1.Columns:= strtoint(edit2.text);
  api_workbook1.rows:= strtoint(edit1.text);
  showmessage('Created.');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  api_workbook1.ImportCSVText(memo1.text,'|');
  edit1.Text:= inttostr(api_workbook1.Columns);
  edit2.Text:= inttostr(api_workbook1.rows);
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  csvtext: string;
begin
  api_workbook1.ExportCSVText(csvtext, '|');
  memo1.lines.Text:= csvtext;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  memo1.lines.SaveToFile( extractfiledir(application.exename)+'\test.csv' );
  showmessage('Saved.');
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  if fileexists( extractfiledir(application.exename)+'\test.csv' ) then
  begin
    memo1.clear;
    memo1.lines.LoadFromFile( extractfiledir(application.exename)+'\test.csv' );
    showmessage('Loaded.');
  end else
    showmessage('File does not exist.');
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  if api_workbook1.SortByColumn(0,true) then
    showmessage('Sorted.');
end;

end.
