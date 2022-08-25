unit OverbyteIcsPrngTst1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    IcsRandomIntButton32: TButton;
    RtlRandButton: TButton;
    procedure IcsRandomIntButton32Click(Sender: TObject);
    procedure RtlRandButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  OverbyteIcsUtils;

procedure TForm1.IcsRandomIntButton32Click(Sender: TObject);
var
  Bmp: TBitmap;
  x, y : Integer;
begin
  Bmp := TBitmap.Create;
  Bmp.Width  := Image1.Width;
  Bmp.Height := Image1.Height;
  for x := 0 to Bmp.Width - 1 do
   for y := 0 to Bmp.Height -1 do
      Bmp.Canvas.Pixels[x, y] := IcsRandomInt($1000000);
  Image1.Picture.Bitmap := Bmp;
  Bmp.Free;
end;

procedure TForm1.RtlRandButtonClick(Sender: TObject);
var
  Bmp: TBitmap;
  x, y : Integer;
begin
  Bmp := TBitmap.Create;
  Bmp.Width  := Image2.Width;
  Bmp.Height := Image2.Height;
  for x := 0 to Bmp.Width - 1 do
    for y := 0 to Bmp.Height -1 do
      Bmp.Canvas.Pixels[x, y] := Random($1000000);
  Image2.Picture.Bitmap := Bmp;
  Bmp.Free;
end;



end.
