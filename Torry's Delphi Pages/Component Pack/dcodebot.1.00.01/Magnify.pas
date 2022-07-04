unit Magnify;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TMagnifier = class(TForm)
    Panel: TPanel;
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FScreen: TCanvas;
    FScale: Integer;
    FX: Integer;
    FY: Integer;
    procedure SetScale(Value: Integer);
  public
    procedure DrawScreen(X, Y: Integer);
    property Scale: Integer read FScale write SetScale;
  end;

implementation

{$R *.DFM}

procedure TMagnifier.FormCreate(Sender: TObject);
begin
  FScreen := TCanvas.Create;
  FScreen.Handle := GetDC(0);
  FScale := 200;
end;

procedure TMagnifier.FormDestroy(Sender: TObject);
begin
  FScreen.Free;
end;

procedure TMagnifier.DrawScreen(X, Y: Integer);
begin
  FX := X;
  FY := Y;
  PaintBoxPaint(nil);
end;

procedure TMagnifier.PaintBoxPaint(Sender: TObject);
var
  Rect: TRect;
  Source: TRect;
begin
  Rect := PaintBox.ClientRect;
  Source := Rect;
  Source.Right := Trunc(Source.Right * 100 / Scale);
  Source.Bottom := Trunc(Source.Bottom * 100 / Scale);
  with Source do
    OffsetRect(Source, FX - (Right - Left) div 2, FY - (Bottom - Top) div 2);
  PaintBox.Canvas.CopyRect(Rect, FScreen, Source);
end;

procedure TMagnifier.SetScale(Value: Integer);
begin
  FScale := Value;
  PaintBoxPaint(nil);
end;

end.
