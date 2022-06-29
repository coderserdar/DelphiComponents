unit Demo4Frm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}  
  Menus;

type
  TfrmDemo4 = class(TForm)
    rv: TRichView;
    rvs: TRVStyle;
    pm: TPopupMenu;
    Watch1: TMenuItem;
    Phone1: TMenuItem;
    Keyboard1: TMenuItem;
    NoSmokeSign1: TMenuItem;
    Socket1: TMenuItem;
    Pot1: TMenuItem;
    Books1: TMenuItem;
    Bridge1: TMenuItem;
    ilGoods: TImageList;
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure rvJump(Sender: TObject; id: Integer);
    procedure pmItemClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    Goods: array[0..9] of Integer;
    GoodsCount: Integer;
  public
    { Public declarations }
    procedure BuildDoc;
  end;

implementation

const Descriptions: array [0..7] of String =
(
   'Exact time twice a day!',
   'Unique 9-button model',
   'Additional keys available on request',
   'Must have for any airline!',
   'Second hand',
   'Universal thing',
   '2 kg',
   'Brooklyn Bridge, available only today, 10% discount for first 10 buyers!'
);


{$R *.DFM}

{ TfrmDemo4 }

procedure TfrmDemo4.BuildDoc;
var i, VPos: Integer;
begin
  VPos := rv.VScrollPos;
  rv.Clear;
  rv.AddNL('Welcome to our shop!',1,1);
  rv.AddNL('Today we have:',2,1);
  for i := 0 to GoodsCount-1 do begin
    rv.AddBulletEx('', Goods[i], ilGoods, 0);
    rv.Add(pm.Items[Goods[i]].Caption,3);
    rv.Add(' ('+Descriptions[Goods[i]]+') ',0);
    rv.AddHotspotEx('', 2,3, il, -1);
    rv.AddBreak;
  end;
  if GoodsCount<>10 then
    rv.AddHotspotEx('', 0,1, il, 0);  
  rv.Format;
  rv.VScrollPos := VPos;
end;

procedure TfrmDemo4.FormCreate(Sender: TObject);
begin
  {$IFDEF RICHVIEWDEF5}
  pm.AutoHotkeys := maManual;
  {$ENDIF}
  BuildDoc;
end;

procedure TfrmDemo4.rvJump(Sender: TObject; id: Integer);
var p: TPoint;
    i: Integer;
begin
  if id=GoodsCount then begin
    // adding
    GetCursorPos(p);
    pm.Popup(p.x, p.y);
    end
  else begin
    // deleting
    for i := id to GoodsCount-2 do
      Goods[i] := Goods[i+1];
    dec(GoodsCount);
    BuildDoc;
  end;
end;

procedure TfrmDemo4.pmItemClick(Sender: TObject);
begin
  Goods[GoodsCount] := TMenuItem(Sender).MenuIndex;
  inc(GoodsCount);
  BuildDoc;
  rv.VScrollPos := rv.VScrollMax;  
end;

procedure TfrmDemo4.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;

end.
