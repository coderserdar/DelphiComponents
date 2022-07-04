unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DB, ASGSQLite3, Grids, DBGrids;

type
  TForm4 = class(TForm)
    BitBtn1: TBitBtn;
    DB: TASQLite3DB;
    Q: TASQLite3Query;
    Memo1: TMemo;
    Button1: TButton;
    QQ: TASQLite3Query;
    DSQQ: TDataSource;
    DBGrid1: TDBGrid;
    Memo2: TMemo;
    Button2: TButton;
    Button3: TButton;
    Memo3: TMemo;
    Button4: TButton;
    Label1: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.BitBtn1Click(Sender: TObject);
begin
 DB.Open;
 if not DB.TableExists('recipe') then begin
   Q.SQL.Text := 'create virtual table recipe using fts3(name varchar(80), ingredients varchar(255))';
   Q.ExecSQL;
 end;
 Q.SQL.Text := 'insert into recipe (name, ingredients) values ("broccoli stew", "broccoli peppers cheese tomatoes")';
 Q.ExecSQL;
 Q.SQL.Text := 'insert into recipe (name, ingredients) values ("pumpkin stew", "pumpkin onions garlic celery")';
 Q.ExecSQL;
 Q.SQL.Text := 'insert into recipe (name, ingredients) values ("broccoli pie", "broccoli cheese onions flour")';
 Q.ExecSQL;
 Q.SQL.Text := 'insert into recipe (name, ingredients) values ("pumpkin pie", "pumpkin sugar flour butter")';
 Q.ExecSQL;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  QQ.SQL.text := Memo1.text;
  QQ.Open;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  QQ.SQL.text := Memo2.text;
  QQ.Open;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  QQ.SQL.text := 'select * from recipe_content';
  QQ.Open;
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  QQ.SQL.text := Memo3.text;
  QQ.Open;
end;

end.
