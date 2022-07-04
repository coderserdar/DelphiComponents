unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, ComCtrls, DBTreeView, StdCtrls, ImgList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    DBTreeView1: TDBTreeView;
    ADOConnection1: TADOConnection;
    ImageList2: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure DBTreeView1DblClick(Sender: TObject);
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
  DBTreeView1.Open;
end;

procedure TForm1.DBTreeView1DblClick(Sender: TObject);
var
  Sel: integer;
  Folder: Boolean;
begin
  Sel := DBTreeView1.Selected.StateIndex;
  Folder := DBTreeView1.CheckOnFolder(DBTreeView1.Selected);

  if Folder = True then
    ShowMessage('Папка № '+IntToStr(Sel))
  else
    ShowMessage('Объект № '+IntToStr(Sel));
end;

end.
