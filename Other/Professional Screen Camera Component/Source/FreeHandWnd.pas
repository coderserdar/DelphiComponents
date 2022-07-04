{
Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.)
Author E-Mail: mrh.info2007@gmail.com
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
................................................................................
}

unit FreeHandWnd;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls;

{$WARNINGS OFF}
{$RANGECHECKS OFF}

type

  TData = array[0..0] of TPoint;

  TFreeHandWindow = class(TForm)
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
              Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    SelectRegion1,
    RegionOK : Boolean;
    X1,
    Y1,
    X2,
    Y2       : Integer;
    fBMP     : TBitmap;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    { Public declarations }
    PRegion  : TRect;
  end;

var
  FreeHandWindow : TFreeHandWindow;

implementation

{$R *.dfm}

procedure TFreeHandWindow.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
  Msg.Result := 1;
end;

procedure TFreeHandWindow.FormMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
var
  MousePos: TMouse;
begin
  Hint := '';
  if (ssCtrl in Shift) and (Button = mbLeft)
     and RegionOK and (PRegion.Right <> 0)
     and (PRegion.Bottom <> 0) then begin
    // Ok, Ditinct top, left, width and height region
    ModalResult := mrOk;
    end
  else begin
    if (Button = mbLeft) then begin
      X1             := 0;
      Y1             := 0;
      X2             := 0;
      Y2             := 0;
      PRegion.Left   := 0;
      PRegion.Top    := 0;
      PRegion.Bottom := 0;
      PRegion.Right  := 0;

      SelectRegion1  := True;  // Start selecting region
      RegionOK       := False; // Region is not selected

      if not Shape1.Visible then
        Shape1.Visible := True;
      if not Shape2.Visible then
        Shape2.Visible := True;
      if Shape3.Visible then
        Shape3.Visible := False;
      if Shape4.Visible then
        Shape4.Visible := False;

      Shape1.Height := FreeHandWindow.Height;
      Shape1.Width  := 2;
      Shape2.Height := 2;
      Shape2.Width  := FreeHandWindow.Width;

      Shape1.Left   := MousePos.CursorPos.X;
      Shape1.Top    := 0;

      Shape2.Left   := 0;
      Shape2.Top    := MousePos.CursorPos.Y;

      X1            := MousePos.CursorPos.X;
      Y1            := MousePos.CursorPos.Y;
      end;
    end;
end;

procedure TFreeHandWindow.FormMouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: Integer);
var
  MousePos: TMouse;
begin
  if SelectRegion1 then begin
    if not Shape1.Visible then
      Shape1.Visible := True;
    if not Shape2.Visible then
      Shape2.Visible := True;
    if not Shape3.Visible then
      Shape3.Visible := True;
    if not Shape4.Visible then
      Shape4.Visible := True;

    Shape3.Height := FreeHandWindow.Height;
    Shape3.Width  := 2;
    Shape4.Height := 2;
    Shape4.Width  := FreeHandWindow.Width;

    Shape3.Left   := MousePos.CursorPos.X;
    Shape3.Top    := 0;

    Shape4.Left   := 0;
    Shape4.Top    := MousePos.CursorPos.Y;

    X2            := MousePos.CursorPos.X;
    Y2            := MousePos.CursorPos.Y;

    RegionOK      := True;  // Region is selected
    end
  else begin
    if not RegionOK then begin
      if not Shape1.Visible then
        Shape1.Visible := True;
      if not Shape2.Visible then
        Shape2.Visible := True;
      if Shape3.Visible then
        Shape3.Visible := False;
      if Shape4.Visible then
        Shape4.Visible := False;

      Shape1.Height := FreeHandWindow.Height;
      Shape1.Width  := 2;
      Shape2.Height := 2;
      Shape2.Width  := FreeHandWindow.Width;

      Shape1.Left   := MousePos.CursorPos.X;
      Shape1.Top    := 0;

      Shape2.Left   := 0;
      Shape2.Top    := MousePos.CursorPos.Y;
      end;
    end;

    // Calculate width area selected region
    if X1 < X2 then begin
      PRegion.Left   := X1;
      PRegion.Right  := X2 - X1;
      end
    else begin
      PRegion.Left   := X2;
      PRegion.Right  := X1 - X2;
      end;

    // Calculate Height area selected region
    if Y1 < Y2 then begin
      PRegion.Top    := Y1;
      PRegion.Bottom := Y2 - Y1;
      end
    else begin
      PRegion.Top    := Y2;
      PRegion.Bottom := Y1 - Y2;
      end;

    // Show hint
    if (PRegion.Bottom <> 0) and
       (PRegion.Right <> 0) then begin
      Hint := 'X: ' + IntToStr(MousePos.CursorPos.X) +
              ' Y: ' + IntToStr(MousePos.CursorPos.Y);
      Hint := Hint + #10#13 + 'Selected Area : ' +
              IntToStr(PRegion.Right) +
              ' x ' + IntToStr(PRegion.Bottom);
      end
    else
      Hint := 'X: ' + IntToStr(MousePos.CursorPos.X) +
              ' Y: ' + IntToStr(MousePos.CursorPos.Y);
    Hint   := Hint                                       + #10#13 +
              '----------------------------------------' + #10#13 +
              'For Cancel Press (Esc Key)'               + #10#13 +
              'For Select Press (Ctrl+Left Click)';

  Application.ActivateHint(MousePos.CursorPos);
end;

procedure TFreeHandWindow.FormMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
begin
  // Calculate width area selected region
  if X1 < X2 then begin
    PRegion.Left   := X1;
    PRegion.Right  := X2 - X1;
    end
  else begin
    PRegion.Left   := X2;
    PRegion.Right  := X1 - X2;
    end;

  // Calculate Height area selected region
  if Y1 < Y2 then begin
    PRegion.Top    := Y1;
    PRegion.Bottom := Y2 - Y1;
    end
  else begin
    PRegion.Top    := Y2;
    PRegion.Bottom := Y1 - Y2;
    end;

  if ((Shape1.Left = Shape3.Left) or (Shape2.Top = Shape4.Top)) or
     ((not Shape3.Visible) and (not Shape4.Visible)) then begin
    SelectRegion1 := False; // Restart select region
    RegionOK      := False; // Region is not selected
    end
  else begin
    SelectRegion1 := False; // Restart select region
    RegionOK      := True;  // Region is selected
    end;
end;

procedure TFreeHandWindow.FormPaint(Sender: TObject);
begin
  Canvas.CopyRect(Canvas.ClipRect, fBmp.Canvas, Canvas.ClipRect);
end;

procedure TFreeHandWindow.FormDestroy(Sender: TObject);
begin
  fBMP.Free;
  Application.CancelHint;
end;

procedure TFreeHandWindow.FormShow(Sender: TObject);
var
  aDC : HDC;
begin
  Left             := 0;
  Top              := 0;
  Width            := Screen.DesktopWidth;
  Height           := Screen.DesktopHeight;

  aDC := GetDC(0);
  try
    BitBlt(fBMP.Canvas.Handle,
           0,
           0,
           Screen.DesktopWidth,
           Screen.DesktopHeight,
           aDC,
           0,
           0,
           SRCCOPY);
  finally;
    ReleaseDC(0, aDC);
    end;

//  fBMP.Canvas.Brush.Style := bsClear;
//  fBMP.Canvas.Font.Color  := clRed;
//  fBMP.Canvas.Font.Name   := 'Arial Black';
//  fBMP.Canvas.Font.Size   := 15;
//  fBMP.Canvas.TextOut(50, 5, 'For Cancel Press (Esc Key)');
//  fBMP.Canvas.TextOut(390, 5, 'For Select Press (Ctrl + Left Click)');

  RegionOK         := False;

  Shape1.Pen.Color := clRed;
  Shape2.Pen.Color := clRed;
  Shape3.Pen.Color := clRed;
  Shape4.Pen.Color := clRed;

  Shape1.Visible   := False;
  Shape2.Visible   := False;
  Shape3.Visible   := False;
  Shape4.Visible   := False;
end;

procedure TFreeHandWindow.FormCreate(Sender: TObject);
var
  pColors : Integer;
begin
  pColors                := GetDeviceCaps(0, BITSPIXEL);
  fBMP                   := TBitmap.Create;
  case pColors of
    1 : fBMP.PixelFormat := pf1bit;
    4 : fBMP.PixelFormat := pf4bit;
    8 : fBMP.PixelFormat := pf8bit;
    15: fBMP.PixelFormat := pf15bit;
    16: fBMP.PixelFormat := pf16bit;
    24: fBMP.PixelFormat := pf24bit;
    32: fBMP.PixelFormat := pf32bit;
    end;
  fBMP.TransparentMode   := tmAuto;
  fBMP.Transparent       := True;
  fBMP.Width             := Screen.DesktopWidth;
  fBMP.Height            := Screen.DesktopHeight;
end;

end.
