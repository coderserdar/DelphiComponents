unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SmWinList, StdCtrls, Grids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    SmWinList1: TSmWinList;
    StringGrid1: TStringGrid;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0,0] := 'Windows Name';
  StringGrid1.Cells[1,0] := 'HWND';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  WinType: TTypeOfWindows;
begin
  case ComboBox1.ItemIndex of
    0: WinType := wnAll;
    1: WinType := wnVisible;
  end;
  SmWinList1.WinType := WinType;
  SmWinList1.Refresh;
  for i:=0 to SmWinList1.Count-1 do
    begin
    StringGrid1.Cells[0,i+1] := SmWinList1[i].WinName;
    StringGrid1.Cells[1,i+1] := IntToStr(SmWinList1[i].Hdl);
    end;

  StringGrid1.RowCount := SmWinList1.Count+1;
  Edit1.Text := IntToStr(SmWinList1.Count);
  Edit2.Text := SmWinList1.ActiveWinName;
  Edit3.Text := IntToStr(SmWinList1.ActiveWinHdl);
end;



end.
