unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_ledgrid, StdCtrls, API_trackbar, API_base;

type
  TForm1 = class(TForm)
    API_ledgrid1: TAPI_ledgrid;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    API_trackbar1: TAPI_trackbar;
    API_trackbar2: TAPI_trackbar;
    procedure API_trackbar1Change(sender: TObject; value: Extended);
    procedure FormCreate(Sender: TObject);
    procedure API_ledgrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
var
  col, row: integer;
begin
  col:=Random(api_ledgrid1.XCount);
  row:=random(api_ledgrid1.YCount);
  if api_ledgrid1.iscolor(col,row,clblack) then api_ledgrid1.setcolor(col,row,clred,$00000040) else
  if api_ledgrid1.iscolor(col,row,clred) then api_ledgrid1.setcolor(col,row,clgreen,$00004000) else
  if api_ledgrid1.iscolor(col,row,clgreen) then api_ledgrid1.setcolor(col,row,clblue,$00400000) else
  if api_ledgrid1.IsColor(col,row,clblue) then api_ledgrid1.setcolor(col,row,clyellow,$00004080) else
    api_ledgrid1.setcolor(col,row,clblack,clgray);
end;

procedure TForm1.API_ledgrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  label1.Caption:= 'x='+inttostr(api_ledgrid1.GetMousePosX(x))+
    ', y='+inttostr(api_ledgrid1.GetMousePosY(y));
end;

procedure TForm1.API_trackbar1Change(sender: TObject; value: Extended);
begin
  formcreate(self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  api_ledgrid1.XCount:= round(api_trackbar1.value);
  api_ledgrid1.YCount:= round(api_trackbar2.value);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  api_ledgrid1.Invalidate;
end;

end.
