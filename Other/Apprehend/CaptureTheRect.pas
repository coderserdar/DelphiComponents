//------------------------------------------------------------------------------
//  Apprehend Version     : 5.1
//  Copyright © 1986-2011 : Adirondack Software & Graphics
//  Created               : 1-09-1992
//  Last Modification     : 08-08-2011
//  Compiler              : Delphi 2010
//  Description           : CaptureTheRect Unit
// This file is copyright (C) W W Miller, 1986-2011.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit CaptureTheRect;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Math, PixelFormatFix;

type
  TCaptureRectForm = class ( TForm )
    Timer1: TTimer;
    procedure FormCreate ( Sender: TObject );
    procedure FormMouseDown ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer );
    procedure FormMouseMove ( Sender: TObject; Shift: TShiftState; X,
      Y: Integer );
    procedure FormMouseUp ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer );
    procedure FormPaint ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure Timer1Timer ( Sender: TObject );
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    //  Mouser, 5/18/05 -
    //  these are helper functions that are called from ASGCapture.pas
    //  used to help multimon capturing.  See ASGCapture.pas for more info
    function ScreenToBitmapX ( SX: Integer ): Integer;
    function ScreenToBitmapY ( SY: Integer ): Integer;
  private
      { Private declarations }
    X1, Y1, X2, Y2: Integer;
    procedure RemoveTheRect;
    procedure DrawTheRect;
    procedure WMEraseBkGnd ( var Msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
    //function RectIncludesPoint ( const R: TRect; const Pt: TPoint ): Boolean;
  public
      { Public declarations }
    fRect: TRect;
    fBmp: TBitmap;
    RectBitmap: TBitmap;
    M: Integer; // Which monitor are we on?
  end;

var
  CaptureRectForm: TCaptureRectForm;
  Counter: Byte;
  CounterStart: Byte;
  Looper: LongInt;
  ForceMonitorNum : Integer;

implementation

uses ASGCapture, promptdialog;

{$R *.DFM}

{ Animated Rubbanding }
procedure MovingDots ( X, Y: Integer; TheCanvas: TCanvas ); stdcall;
begin
  {$R-}
  Inc ( Looper );
  Counter := Counter shl 1; // Shift the bit left one
  if Counter = 0 then Counter := 1; // If it shifts off left, reset it
  if ( Counter and 224 ) > 0 then // Are any of the left 3 bits set?
    TheCanvas.Pixels [ X, Y ] := clBlack // Erase the pixel
  else
    TheCanvas.Pixels [ X, Y ] := clWhite; // Draw the pixel
  {$R+}
end;

{ Animated Rubbanding }
function NormalizeRect ( R: TRect ): TRect;
begin
  // This routine normalizes a rectangle. It makes sure that the Left,Top
  // coords are always above and to the left of the Bottom,Right coords.
  with R do
    if Left > Right then
      if Top > Bottom then
        Result := Rect ( Right, Bottom, Left, Top )
      else
        Result := Rect ( Right, Top, Left, Bottom )
    else
      if Top > Bottom then
        Result := Rect ( Left, Bottom, Right, Top )
      else
        Result := Rect ( Left, Top, Right, Bottom );
end;

{ Animated Rubbanding }
procedure TCaptureRectForm.RemoveTheRect;
var
  R: TRect;
begin
  R := NormalizeRect ( Rect ( X1, Y1, X2, Y2 ) ); // Rectangle might be flipped
  InflateRect ( R, 1, 1 ); // Make the rectangle 1 pixel larger
  InvalidateRect ( Handle, @R, True ); // Mark the area as invalid
  InflateRect ( R, -2, -2 ); // Now shrink the rectangle 2 pixels
  ValidateRect ( Handle, @R ); // And validate this new rectangle.
  // This leaves a 2 pixel band all the way around
  // the rectangle that will be erased & redrawn
  UpdateWindow ( Handle );
end;

{ Animated Rubbanding }
procedure TCaptureRectForm.DrawTheRect;
begin
  // Determines starting pixel color of Rect
  Counter := CounterStart;
  // Use LineDDA to draw each of the 4 edges of the rectangle
  LineDDA ( X1, Y1, X2, Y1, @MovingDots, LongInt ( Canvas ) );
  LineDDA ( X2, Y1, X2, Y2, @MovingDots, LongInt ( Canvas ) );
  LineDDA ( X2, Y2, X1, Y2, @MovingDots, LongInt ( Canvas ) );
  LineDDA ( X1, Y2, X1, Y1, @MovingDots, LongInt ( Canvas ) );
end;

procedure TCaptureRectForm.FormCreate ( Sender: TObject );
var
  ScreenDC: HDC;
  lpPal: PLogPalette;
  P : TPoint;   // Where is the mouse now?
begin
  //Code added to determine which monitor is being captured, in multi-monitor system.
  //Use whichever monitor the cursor is currently positioned in.
  GetCursorPos ( P );                                   // Where am I?
  if (ForceMonitorNum >=0) and (ForceMonitorNum <= Screen.MonitorCount-1)
    then M := ForceMonitorNum  {Override}
    else M := Screen.MonitorFromPoint ( P, mdNearest ).MonitorNum; // Here I am!  Monitors[M]
  // Setup to capture image
  fBMP := TBitmap.Create;
  DoPixelFormatFix(fBMP);
  //MessageDlg(Format('Debugging!  Monitor: [%d], X:[%d], Y:[%d]',[M,P.X, P.Y]), mtInformation, [mbOK], 0);
  RectBitmap := TBitmap.Create;
  DoPixelFormatFix(RectBitmap);
  fBMP.Width := Screen.Monitors[M].Width;   // Size to active monitor, which may differ from the primary.
  fBMP.Height := Screen.Monitors[M].Height;
  ScreenDC := GetDC ( 0 );
   // do we have a palette device? - Thanks to Joe C. Hecht
  if ( GetDeviceCaps ( ScreenDC, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
  begin
     // allocate memory for a logical palette
    GetMem ( lpPal, Sizeof ( TLOGPALETTE ) + ( 255 * Sizeof ( TPALETTEENTRY ) ) );
     // zero it out to be neat
    FillChar ( lpPal^, Sizeof ( TLOGPALETTE ) + ( 255 * Sizeof ( TPALETTEENTRY ) ), #0 );
     // fill in the palette version
    lpPal^.palVersion := $300;
     // grab the system palette entries
    lpPal^.palNumEntries :=
      GetSystemPaletteEntries ( ScreenDC, 0, 256, lpPal^.palPalEntry );
    if ( lpPal^.PalNumEntries <> 0 ) then
     // create the palette
      fBMP.Palette := CreatePalette ( lpPal^ );
    FreeMem ( lpPal, Sizeof ( TLOGPALETTE ) + ( 255 * Sizeof ( TPALETTEENTRY ) ) );
  end;
  try
    // Copy the screen of monitors[M] onto the bitmap.
    // Screen.Monitors[M].Left,Top will be 0,0 for a single-monitor system.
    // But for dual monitors, you would typically have 1024,0 on second monitor,
    // depending on resolution and placement, of course.
    BitBlt (fBMP.Canvas.handle, 0, 0, Screen.Monitors[M].Width, Screen.Monitors[M].Height, ScreenDC, Screen.Monitors[M].Left, Screen.Monitors[M].Top, GetBitBlt_RopMode() );
  finally
    ReleaseDC ( 0, ScreenDC );
    // Mouse also must be bounded by the current monitor.
    SetBounds ( 0, 0, Screen.Monitors[M].Width, Screen.Monitors[M].Height );
    // Setup Animated Rubberband
    X1 := 0;
    Y1 := 0;
    X2 := 0;
    Y2 := 0;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clWhite;
    CounterStart := 128;
    Timer1.Interval := 100;
    Timer1.Enabled := True;
    Looper := 0;
  end;
end;

// Upon showing the form, move it to the upper-left of the active monitor.
procedure TCaptureRectForm.FormShow(Sender: TObject);
begin
  Self.Left := Screen.Monitors[M].Left;
  Self.Top  := Screen.Monitors[M].Top
end;

procedure TCaptureRectForm.FormMouseDown ( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer );
begin
  if ShowHint then
  begin
    Hint := IntToStr ( X ) + ' x ' + IntToStr ( Y ) + ' pixels';
    Application.ActivateHint ( Mouse.CursorPos );
  end;
  RemoveTheRect; // Erase any existing rectangle
  X1 := X; Y1 := Y; X2 := X; Y2 := Y;
  SetRect ( fRect, X, Y, X, Y ); // Set initial rectangle position
end;

procedure TCaptureRectForm.FormMouseMove ( Sender: TObject; Shift: TShiftState; X,
  Y: Integer );
var
  HintWidth: Integer;
  HintHeight: Integer;
begin
  if ssLeft in Shift then
  begin
    RemoveTheRect; // Erase any existing rectangle
    X2 := X; Y2 := Y; // Save the new corner where the mouse is
    DrawTheRect; // Draw the Rect now... don't wait for the timer!
    fRect.Right := X; // Set the position of the rectangle to capture
    fRect.Bottom := Y;
    if ShowHint then
    begin
      Hint := IntToStr ( Abs ( fRect.Right - fRect.Left ) ) + ' x ' + IntToStr ( Abs ( fRect.Bottom - fRect.Top ) ) + ' pixels';
      // Get the width and height of the hint window
      HintWidth := ( CaptureRectForm.Canvas.TextWidth ( Hint ) );
      HintHeight := ( CaptureRectForm.Canvas.TextHeight ( Hint ) );
      // Adjust hint position
      if ( Abs ( X ) > ( Screen.Width - ( HintWidth + 8 ) ) ) then
      begin
        Application.HideHint;
        Update;
      end
      else if ( Abs ( Y ) > ( Screen.Height - ( HintHeight * 2 ) ) ) then
      begin
        Application.HideHint;
        Update;
      end
      else if ( ( ( fRect.Right > fRect.Left ) and ( fRect.Bottom > fRect.Top ) ) ) then
      begin
        Application.ActivateHint ( Point ( Mouse.CursorPos.X, Mouse.CursorPos.Y + HintHeight + 2 ) );
        Update;
      end
      else
      begin
        Application.ActivateHint ( Point ( Max ( fRect.Right, fRect.Left ), Max ( fRect.Bottom, fRect.Top ) + HintHeight + 2 ) );
        Update;
      end;
    end;
  end;
end;

procedure TCaptureRectForm.FormMouseUp ( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer );
var
  ScreenDC: HDC;
  Bitmap: TBitmap;
begin
  if Button = mbLeft then
  begin
    Bitmap := TBitmap.Create;
    DoPixelFormatFix(Bitmap);
    // Set fRect
    fRect.Left := min ( X1, X2 );
    fRect.Top := min ( Y1, Y2 );
    fRect.Right := max ( X1, X2 );
    fRect.Bottom := max ( Y1, Y2 );
    // Exit if improper rectangle drawn
    if ( fRect.Right > fRect.Left ) and ( fRect.Bottom > fRect.Top ) then
    begin
      Bitmap.Width := fRect.Right - fRect.Left;
      Bitmap.Height := fRect.Bottom - fRect.Top;
      RemoveTheRect;
      ScreenDC := GetDC ( 0 );
      try
        // Again, Monitors[M] comes into play on multi-monitor systems.
        // Instead of a 0,0 origin, it may be 1024,0 or 0,768, etc..
        // For a single monitor, Screen.Monitors[M].Left is 0, same for ".Top".
        BitBlt ( Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, ScreenDC, Screen.Monitors[M].Left + fRect.Left, Screen.Monitors[M].Top + fRect.Top, GetBitBlt_RopMode() );
        RectBitmap.Assign ( Bitmap );
        fBmp.Assign ( Bitmap );
      finally
        ReleaseDC ( 0, ScreenDC );
        Bitmap.Free;
      end;
    end; // if
    ModalResult := mrOK;
  end;
end;

procedure TCaptureRectForm.FormPaint ( Sender: TObject );
begin
  Canvas.Draw ( 0, 0, fBMP );
end;

procedure TCaptureRectForm.FormDestroy ( Sender: TObject );
begin
  fBMP.Free;
  RectBitmap.Free;
end;

procedure TCaptureRectForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ScreenDC: HDC;
  Bitmap: TBitmap;
  iPoint: TPoint;
begin
  case Key of
    VK_DOWN:
      begin
        RemoveTheRect; // Erase any existing rectangle
        Y1 := Y1 + 1;
        X2 := X1 + fRect.Right;
        Y2 := Y1 + fRect.Bottom;
        DrawTheRect; // Draw the Rect now... don't wait for the timer!
        iPoint.X := Mouse.CursorPos.X;
        iPoint.Y := Mouse.CursorPos.Y + 1;
        SetCursorPos( iPoint.X, iPoint.Y );
      end;
    VK_UP:
      begin
        RemoveTheRect; // Erase any existing rectangle
        Y1 := Y1 - 1;
        X2 := X1 + fRect.Right;
        Y2 := Y1 + fRect.Bottom;
        DrawTheRect; // Draw the Rect now... don't wait for the timer!
        iPoint.X := Mouse.CursorPos.X;
        iPoint.Y := Mouse.CursorPos.Y - 1;
        SetCursorPos( iPoint.X, iPoint.Y );
      end;
    VK_LEFT:
      begin
        RemoveTheRect; // Erase any existing rectangle
        X1 := X1 - 1;
        X2 := X1 + fRect.Right;
        DrawTheRect; // Draw the Rect now... don't wait for the timer!
        iPoint.X := Mouse.CursorPos.X - 1;
        iPoint.Y := Mouse.CursorPos.Y;
        SetCursorPos( iPoint.X, iPoint.Y );
      end;
    VK_RIGHT:
      begin
        RemoveTheRect; // Erase any existing rectangle
        X1 := X1 + 1;
        X2 := X1 + fRect.Right;
        DrawTheRect; // Draw the Rect now... don't wait for the timer!
        iPoint.X := Mouse.CursorPos.X + 1;
        iPoint.Y := Mouse.CursorPos.Y;
        SetCursorPos( iPoint.X, iPoint.Y );
      end;
    VK_RETURN:
      begin
        Bitmap := TBitmap.Create;
        // Set fRect
        fRect.Left := X1;
        fRect.Top := Y1;
        fRect.Right := X2;
        fRect.Bottom := Y2;
        // Exit if improper rectangle drawn
        if ( fRect.Right > fRect.Left ) and ( fRect.Bottom > fRect.Top ) then
        begin
          Bitmap.Width := fRect.Right - fRect.Left;
          Bitmap.Height := fRect.Bottom - fRect.Top;
          Bitmap.PixelFormat := pfDevice;
          RemoveTheRect;
          ScreenDC := GetDC( 0 );
          try
            BitBlt( Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, ScreenDC, fRect.Left, fRect.Top,
              GetBitBlt_RopMode( ) );
            RectBitmap.Assign( Bitmap );
          finally
            ReleaseDC( 0, ScreenDC );
            Bitmap.Free;
          end;
        end; // if
        CaptureRectForm.ModalResult := mrOK;
      end;
    VK_F8:
      if Assigned( frmPosition ) then
        frmPosition.Free;
  end;
end;

procedure TCaptureRectForm.WMEraseBkGnd ( var Msg: TWMEraseBkGnd );
begin
  Msg.Result := 1;
end;

(*
function TCaptureRectForm.RectIncludesPoint ( const R: TRect; const Pt: TPoint ): Boolean;
begin
  Result := ( Pt.X > R.Left ) and ( Pt.X < R.Right ) and
    ( Pt.Y > R.Top ) and ( Pt.Y < R.Bottom );
end; *)

{ Animated Rubbanding }
procedure TCaptureRectForm.Timer1Timer ( Sender: TObject );
begin
  CounterStart := CounterStart shr 2; // Shl 1 will move rect slower
  if CounterStart = 0 then CounterStart := 128; // If bit is lost, reset it
  DrawTheRect; // Draw the rectangle
end;

//  Mouser, 5/18/05 -
//  these are helper functions that are called from ASGCapture.pas
//  used to help multimon capturing.  See ASGCapture.pas for more info

function TCaptureRectForm.ScreenToBitmapX ( SX: Integer ): Integer;
begin
  Result := SX-(Screen.Monitors[M].Left + fRect.Left);
end;

function TCaptureRectForm.ScreenToBitmapY ( SY: Integer ): Integer;
begin
  Result := SY-(Screen.Monitors[M].Top + fRect.Top);
end;

end.

