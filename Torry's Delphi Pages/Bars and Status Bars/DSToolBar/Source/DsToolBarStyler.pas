unit DsToolBarStyler;
//------------------------------------------------------------------------------
interface
uses
  Windows, Graphics;
//------------------------------------------------------------------------------
type
//------------------------------------------------------------------------------
  TDSToolBarStyler = class
  private
    fCorner: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DrawEdge(aCanvas: TCanvas; aRect: TRect; aRaised: Boolean);
    procedure DrawGradientV(Canvas: TCanvas; FromColor, ToColor: TColor; aRect: TRect);
    procedure DrawGradientH(Canvas: TCanvas; FromColor, ToColor: TColor; aRect: TRect);
    property Corner: Integer read fCorner write fCorner;
  end;
//------------------------------------------------------------------------------
implementation

uses Types;
//------------------------------------------------------------------------------
constructor TDSToolBarStyler.Create;
begin
  inherited Create;
  Corner := 1;
end;
//------------------------------------------------------------------------------
destructor TDSToolBarStyler.Destroy;
begin
  inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TDSToolBarStyler.DrawEdge(aCanvas: TCanvas; aRect: TRect; aRaised: Boolean);
var
  vLeftTop, vBottomRight: Integer;
begin
  if aRaised then
  begin
    vLeftTop := clWhite;
    vBottomRight := clBtnShadow;
  end
  else
  begin
    vLeftTop := clBtnShadow;
    vBottomRight := clWhite;
  end;
  aRect.Right := aRect.Right-1;
  aRect.Bottom := aRect.Bottom-1;

  aCanvas.Pen.Color := vLeftTop;
  aCanvas.MoveTo(aRect.Left+Corner, aRect.Bottom);
  aCanvas.LineTo(aRect.Left, aRect.Bottom -Corner);
  aCanvas.LineTo(aRect.Left, aRect.Top+Corner);
  aCanvas.LineTo(aRect.Left+Corner, aRect.Top);
  aCanvas.LineTo(aRect.Right-Corner, aRect.Top);

  aCanvas.Pen.Color := vBottomRight;
  aCanvas.LineTo(aRect.Right, aRect.Top+Corner);
  aCanvas.LineTo(aRect.Right, aRect.Bottom-Corner);
  aCanvas.LineTo(aRect.Right-Corner, aRect.Bottom);
  aCanvas.LineTo(aRect.Left+Corner, aRect.Bottom);
end;//DrawMyEdge
//------------------------------------------------------------------------------
procedure TDSToolBarStyler.DrawGradientH(Canvas: TCanvas; FromColor, ToColor: TColor; aRect: TRect);
var
  StartR, StartG, StartB: Double;
  RStepR, RStepG, RStepB: Double;
  i: Integer;
  vWidth: Integer;
begin
  FromColor := ColorToRGB(FromColor);
  ToColor   := ColorToRGB(ToColor);

  StartR := (FromColor and $000000FF);
  StartG := (FromColor and $0000FF00) shr 8;
  StartB := (FromColor and $00FF0000) shr 16;

  vWidth := aRect.Right - aRect.Left;
  RStepR := ((ToColor and $000000FF) - StartR) / vWidth;
  RStepG := (((ToColor and $0000FF00) shr 8) - StartG) / vWidth;
  RStepB := (((ToColor and $00FF0000) shr 16) - StartB) / vWidth;

  Canvas.Brush.Style := bsClear;
  for i := aRect.Left to aRect.Right do
  begin
    Canvas.Pen.Color := (Round(StartR) or (Round(StartG) shl 8) or (Round(StartB) shl 16));
    Canvas.MoveTo(i, aRect.Top);
    Canvas.LineTo(i, aRect.Bottom);
    StartR := StartR + RStepR;
    StartG := StartG + RStepG;
    StartB := StartB + RStepB;
  end;
end;//DrawGradientH
//------------------------------------------------------------------------------
procedure TDSToolBarStyler.DrawGradientV(Canvas: TCanvas; FromColor, ToColor: TColor; aRect: TRect);
var
  StartR, StartG, StartB: Double;
  RStepR, RStepG, RStepB: Double;
  i: Integer;
  vHeight: Integer;
begin
  FromColor := ColorToRGB(FromColor);
  ToColor   := ColorToRGB(ToColor);

  StartR := (FromColor and $000000FF);
  StartG := (FromColor and $0000FF00) shr 8;
  StartB := (FromColor and $00FF0000) shr 16;

  vHeight := aRect.Bottom - aRect.Top;
  RStepR := ((ToColor and $000000FF) - StartR) / vHeight;
  RStepG := (((ToColor and $0000FF00) shr 8) - StartG) / vHeight;
  RStepB := (((ToColor and $00FF0000) shr 16) - StartB) / vHeight;

  Canvas.Brush.Style := bsClear;
  for i := aRect.Top to aRect.Bottom do
  begin
    Canvas.Pen.Color := (Round(StartR) or (Round(StartG) shl 8) or (Round(StartB) shl 16));
    Canvas.MoveTo(aRect.Left, i);
    Canvas.LineTo(aRect.Right, i);
    StartR := StartR + RStepR;
    StartG := StartG + RStepG;
    StartB := StartB + RStepB;
  end;
end;
//------------------------------------------------------------------------------

end.

