unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    RichView2: TRichView;
    procedure RVStyle1DrawStyleText(Sender: TRVStyle; const s: String;
      Canvas: TCanvas; StyleNo, SpaceBefore, Left, Top, Width,
      Height: Integer; DrawState: TRVTextDrawStates;
      var DoDefault: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure RVStyle1StyleHoverSensitive(Sender: TRVStyle;
      StyleNo: Integer; var Sensitive: Boolean);
    procedure RVStyle1DrawTextBack(Sender: TRVStyle; Canvas: TCanvas;
      StyleNo, Left, Top, Width, Height: Integer;
      DrawState: TRVTextDrawStates; var DoDefault: Boolean);
    procedure RVStyle1DrawCheckpoint(Sender: TRVStyle; Canvas: TCanvas; X,
      Y, ItemNo, XShift: Integer; RaiseEvent: Boolean; Control: TControl;
      var DoDefault: Boolean);
    procedure RVStyle1DrawPageBreak(Sender: TRVStyle; Canvas: TCanvas; Y,
      XShift: Integer; Control: TControl; var DoDefault: Boolean);
    procedure RVStyle1DrawParaBack(Sender: TRVStyle; Canvas: TCanvas;
      ParaNo: Integer; ARect: TRect; var DoDefault: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{============================== DRAWING ROUTINES ==============================}

{$R-} // turning off range checking (required for DrawTrRect)
{------------------------------------------------------------------------------}
{ Drawing left to right arrow                                                  }
{------------------------------------------------------------------------------}
procedure DrawArrow(Canvas: TCanvas; Left, Top, Width, Height: Integer);
var midx,midy: Integer;
    one, two : Integer;
begin
   midx := Left+Width div 2;
   midy := Top+Height div 2;
   if Width>0 then begin
     one := 1;
     two := -2;
   end
   else begin
     one := -1;
     two := 2;
   end;
   Canvas.PolyLine([Point(Left+one,Top+3),
                    Point(midx,Top+3),
                    Point(midx,Top+1),
                    Point(Left+Width+two, midy),
                    Point(midx, Top+Height-2),
                    Point(midx, Top+Height-4),
                    Point(Left+one,Top+Height-4),
                    Point(Left+one,Top+3)
                    ]);
end;
{------------------------------------------------------------------------------}
{ Drawing an icon for page breaks                                              }
{------------------------------------------------------------------------------}
procedure DrawPageIcon(Canvas: TCanvas; Left, Top, Width, Height: Integer);
var LeftS, RightS: Integer;
begin
  LeftS := Left+2;
  RightS := Left+Width-4;
  Canvas.PolyLine([Point(RightS-3,Top),
                   Point(LeftS,Top),
                   Point(LeftS, Top+Height),
                   Point(RightS,Top+Height),
                   Point(RightS,Top+3),
                   Point(RightS-3,Top),
                   Point(RightS-3,Top+3),
                   Point(RightS,Top+3)
                   ]);
   Canvas.Pen.Style := psDot;
   Canvas.Pen.Color := clRed;
   Canvas.MoveTo(Left, Top+Height div 2);
   Canvas.LineTo(Left+Width, Top+Height div 2);
end;
{------------------------------------------------------------------------------}
{ Drawing a colored rectangle with specified degree of opacity (0..255)        }
{ (quite slow...)                                                              }
{------------------------------------------------------------------------------}
procedure DrawTrRect(Canvas: TCanvas; const ARect: TRect;
                     Color: TColor;
                     Opacity: Integer);
type
  RGBARR = array [0..0] of TRGBQUAD;
  PRGBARR = ^RGBARR;
var prgb: PRGBARR;
    rgb : TRGBQUAD;
   i,j: Integer;
   tr : Integer;
   Clr: LongInt;
   bmp: TBitmap;
begin
  Clr := ColorToRGB(Color);
  rgb.rgbRed      := Clr and $000000FF;
  rgb.rgbGreen    := (Clr and $0000FF00) shr 8;
  rgb.rgbBlue     := (Clr and $00FFFFFF) shr 16;
  rgb.rgbReserved := 0;

  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Width := ARect.Right-ARect.Left;
  bmp.Height := ARect.Bottom-ARect.Top;
  bmp.Canvas.CopyRect(Rect(0,0,bmp.Width,bmp.Height), Canvas, ARect);

  tr := 255 - Opacity;
  for i := 0 to bmp.Height-1 do begin
    prgb := PRGBARR(bmp.ScanLine[i]);
    for j := 0 to bmp.Width-1 do
      with prgb[j] do begin
        rgbBlue  := (rgbBlue*tr  + rgb.rgbBlue*Opacity) div 255;
        rgbGreen := (rgbGreen*tr + rgb.rgbGreen*Opacity)div 255;
        rgbRed   := (rgbRed*tr   + rgb.rgbRed*Opacity) div 255;
      end;
  end;
  Canvas.Draw(ARect.Left, ARect.Top, bmp);
  bmp.Free;
end;
{------------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  RichView1.AddNL('Example',1,1);
  RichView1.AddNL('This is an example of the new feature - ',0,0);
  RichView1.AddNL('custom drawn text',3,-1);
  RichView1.AddNL('.',0,-1);
  RichView1.AddNL(' Hot link 1 ',4,1);
  RichView1.AddNL(' Hot link 2 ',5,1);
  RichView1.AddBreakEx(1, rvbsLine, clBtnShadow);
  RichView1.AddCheckpoint;
  RichView1.AddNL('Another example - a custom drawing of checkpoints.',0,0);
  RichView1.AddCheckpoint;
  RichView1.AddNL('For example, you can draw a little nice arrow instead of default dotted line.',0,0);
  RichView1.AddBreakEx(1, rvbsLine, clBtnShadow);
  RichView1.AddNL('One more example - a custom displaying of page break',0,0);
  RichView1.PageBreaksBeforeItems[RichView1.ItemCount-1] := True;
  RichView1.Format;

  RichView2.AddNL('Cool Effect - ',2,2);
  RichView2.SetAddParagraphMode(False);
  RichView2.AddNL('Transparent paragraph background.',2,2);
  RichView2.AddNL('example of custom painting of paragraph background',0,2);
  RichView2.SetAddParagraphMode(True);
  RichView2.AddNL('This is the example of using of OnDrawParaBack event.',0,0);
  RichView2.Format;
end;
{------------------------------------------------------------------------------}
{ If RichView should repaint if mouse is over text of specified style?         }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1StyleHoverSensitive(Sender: TRVStyle;
  StyleNo: Integer; var Sensitive: Boolean);
begin
  if StyleNo in [4,5] then
    Sensitive := True; // (default for other styles)
end;
{------------------------------------------------------------------------------}
{ Drawing a background of text                                                 }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1DrawTextBack(Sender: TRVStyle; Canvas: TCanvas;
  StyleNo, Left, Top, Width, Height: Integer; DrawState: TRVTextDrawStates;
  var DoDefault: Boolean);
var r: TRect;
begin
  case StyleNo of
    5:
      begin
        // drawing a sunken edge for the 5th style
        r := Bounds(Left,Top, Width, Height);
        if rvtsHover in DrawState then
          DrawEdge(Canvas.Handle, r, BDR_SUNKENOUTER or BF_ADJUST, BF_RECT)
      end;
  end;
end;
{------------------------------------------------------------------------------}
{ Drawing a text                                                               }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1DrawStyleText(Sender: TRVStyle; const s: String;
  Canvas: TCanvas; StyleNo, SpaceBefore, Left, Top, Width, Height: Integer;
  DrawState: TRVTextDrawStates; var DoDefault: Boolean);
begin
  if rvtsSelected in DrawState then
    exit; // default drawing for selected text
  inc(Left, SpaceBefore);
  case StyleNo of
    0:
      begin
        // sunken effect
        Canvas.Font.Color := clBtnHighlight;
        Canvas.TextOut(Left+1,Top+1, s);
        Canvas.Font.Color := clBtnShadow;
        Canvas.TextOut(Left,Top, s);
        DoDefault := False;
      end;
    3:
      begin
        // raised effect
        Canvas.Font.Color := clBtnHighlight;
        Canvas.TextOut(Left-1,Top-1, s);
        Canvas.Font.Color := clBtnShadow;
        Canvas.TextOut(Left+1,Top+1, s);
        DoDefault := False;
      end;
    4:
      begin
        if rvtsHover in DrawState then begin
          // hot glow effect
          Canvas.Font.Color := Sender.TextStyles[StyleNo].HoverColor;
          Canvas.TextOut(Left+1,Top+1, s);
          Canvas.TextOut(Left-1,Top-1, s);
          Canvas.Font.Color := Sender.TextStyles[StyleNo].Color;
          Canvas.TextOut(Left,Top, s);
          DoDefault := False;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
{ Drawing checkpoint as arrow icon                                             }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1DrawCheckpoint(Sender: TRVStyle; Canvas: TCanvas;
  X, Y, ItemNo, XShift: Integer; RaiseEvent: Boolean; Control: TControl;
  var DoDefault: Boolean);
begin
  if RaiseEvent then
    Canvas.Pen.Color := Sender.CheckpointEvColor
  else
    Canvas.Pen.Color := Sender.CheckpointColor;
  DrawArrow(Canvas, -XShift+2, Y-5, RichView1.LeftMargin-4, 10);
  DoDefault := False;
end;
{------------------------------------------------------------------------------}
{ Drawing page break as icon                                                   }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1DrawPageBreak(Sender: TRVStyle; Canvas: TCanvas;
  Y, XShift: Integer; Control: TControl; var DoDefault: Boolean);
begin
  Canvas.Pen.Color := Sender.PageBreakColor;
  DrawPageIcon(Canvas, -XShift+2, Y-8, 16, 16);
  DoDefault := False;
end;
{------------------------------------------------------------------------------}
{ Drawing a background of paragraphs                                           }
{------------------------------------------------------------------------------}
procedure TForm1.RVStyle1DrawParaBack(Sender: TRVStyle; Canvas: TCanvas;
  ParaNo: Integer; ARect: TRect; var DoDefault: Boolean);
begin
  if ParaNo=2 then begin
    // semi-transparent background for second paragraph style
    DrawTrRect(Canvas, ARect, Sender.ParaStyles[ParaNo].Background.Color, 150);
    DoDefault := False;
  end;
end;

end.
