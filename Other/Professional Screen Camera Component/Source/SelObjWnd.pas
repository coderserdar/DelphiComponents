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

unit SelObjWnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

{$WARNINGS OFF}
{$RANGECHECKS OFF}

const
  PW = 1;
  SL = 20;

type
  TSelObjWindow = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    cRect     : TRect;
    OldRegion : HRGN;
    procedure SetUpRegion(X, Y, Width, Height: Integer;
                          ClearLine: Boolean; Text: string);
    procedure PaintBorder(ColorVal: COLORREF; Text: string);
  public
    { Public declarations }
    PRegion   : TRect;
  end;

var
  SelObjWindow: TSelObjWindow;

implementation

{$R *.dfm}

// Set the Window Region for transparancy outside the mask region
procedure TSelObjWindow.SetUpRegion(X, Y, Width, Height: Integer;
                                    ClearLine: Boolean; Text: string);
var
	WndRgn, RgnTemp0, RgnTemp1, RgnTemp2, RgnTemp3: HRgn;
  PenWidth, SideLen, TextWidth, TextHeight: Integer;
begin
  // Create transparent window around selected object
  
  PenWidth     := PW;
  SideLen      := SL;
  TextWidth    := Canvas.TextWidth(Text);
  TextHeight   := Canvas.TextHeight(Text);;

  Self.Left    := X - (PenWidth * 2);
  Self.Top     := Y - ((PenWidth * 2) + TextHeight);
  Self.Width   := Width + (PenWidth * 3);
  Self.Height  := Height + (PenWidth * 3) + TextHeight;

	cRect.Left   := 0;
	cRect.Top    := TextHeight;
	cRect.Right  := Self.Width;
	cRect.Bottom := Self.Height;

  WndRgn       := CreateRectRgn(0,
                                0,
                                Self.Width,
                                Self.Height);

  RgnTemp0     := CreateRectRgn(TextWidth,
                                0,
                                Self.Width,
                                TextHeight);

	RgnTemp1     := CreateRectRgn(PenWidth,
                                PenWidth + TextHeight,
                                Width + (PenWidth * 2),
                                Height + (PenWidth * 2) + TextHeight);

  CombineRgn(WndRgn,
             WndRgn,
             RgnTemp0,
             RGN_DIFF);

	CombineRgn(WndRgn,
             WndRgn,
             RgnTemp1,
             RGN_DIFF);

  if ClearLine then begin
  	RgnTemp2 := CreateRectRgn(0,
                              SideLen + TextHeight,
                              Self.Width,
                              Self.Height - SideLen);
  	RgnTemp3 := CreateRectRgn(SideLen,
                              TextHeight,
                              Self.Width - SideLen,
                              Self.Height);
	  CombineRgn(WndRgn,
               WndRgn,
               RgnTemp2,
               RGN_DIFF);

  	CombineRgn(WndRgn,
               WndRgn,
               RgnTemp3,
               RGN_DIFF);
    end;

	SetWindowRgn(Handle,
               WndRgn,
               True);

  DeleteObject(RgnTemp0);
  DeleteObject(RgnTemp1);

  if ClearLine then begin
    DeleteObject(RgnTemp2);
    DeleteObject(RgnTemp3);
    end;

	if (OldRegion <> 0) then
    DeleteObject(OldRegion);
  OldRegion := WndRgn;
end;

procedure TSelObjWindow.PaintBorder(ColorVal: COLORREF; Text: string);
begin
	if ((cRect.Right > cRect.Left) and
     (cRect.Bottom > cRect.Top)) then begin
    Canvas.Font.Color  := clWhite;
    Canvas.Brush.Color := clRed;
    Canvas.TextOut(0, 0, PChar(Text));

    Canvas.Pen.Color   := clRed;
    Canvas.Brush.Color := clRed;
    Canvas.Rectangle(cRect.Left,
                     cRect.Top,
                     cRect.Right,
                     cRect.Bottom);
    end;
end;

procedure TSelObjWindow.FormCreate(Sender: TObject);
begin
  OldRegion      := 0;
  Timer1.Enabled := True;
end;

procedure TSelObjWindow.FormDestroy(Sender: TObject);
begin
  if OldRegion <> 0 then
    DeleteObject(OldRegion);
  Timer1.Enabled := False;
end;

procedure TSelObjWindow.Timer1Timer(Sender: TObject);
var
  Rect         : TRect;
  ParentHandle : HWnd;
  MousePos     : TMouse;
begin
  // Get object handle under mouse pointer
  ParentHandle   := WindowFromPoint(MousePos.CursorPos);
  // Get region from current object
  GetWindowRect(ParentHandle, Rect);
  PRegion.Left   := Rect.Left;
  PRegion.Top    := Rect.Top;
  PRegion.Right  := Rect.Right - Rect.Left;
  PRegion.Bottom := Rect.Bottom - Rect.Top;

  // Draw line around current object
  SetUpRegion(PRegion.Left,
              PRegion.Top,
              PRegion.Right,
              PRegion.Bottom,
              False,
              'Cancel: Esc -- Select: Ctrl+Enter');

  PaintBorder(clRed,
              'Cancel: Esc -- Select: Ctrl+Enter');

  SetWindowPos(Handle,
               HWND_TOPMOST,
               0,
               0,
               0,
               0,
               SWP_NOREPOSITION or
               SWP_NOMOVE or
               SWP_NOSIZE or
               SWP_NOACTIVATE);
end;

end.
