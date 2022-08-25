// ------------------------------------------------------------------------------
// Apprehend Version     : 5.1
// Copyright © 1986-2011 : Adirondack Software & Graphics
// Created               : 1-09-1992
// Last Modification     : 08-08-2011
// Compiler              : Delphi 2010
// Description           : CaptureFreehand Unit
// This file is copyright (C) W W Miller, 1986-2011.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit CaptureFreehand;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, PixelFormatFix;

type

  TData = array [ 0 .. 0 ] of TPoint;

  TCaptureFreehandForm = class( TForm )
    procedure FormCreate( Sender: TObject );
    procedure FormMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure FormMouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure FormMouseUp( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure FormPaint( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
  private
    { Private declarations }
  public
    { Public declarations }
    fBMP: TBitmap;
    fCapBMP: TBitmap;
    fOrigBmp: TBitmap;
    fDragging: boolean;
    fRect: TRect;
    fXMin, fYMin, fXMax, fYMax: integer;
    M: Integer; // Which monitor are we on?
    procedure WMEraseBkGnd( var Msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
  end;

var
  CaptureFreehandForm: TCaptureFreehandForm;
  i: Longint;
  Memdat: ^TData;
  ForceMonitorNum: Integer;

implementation

uses math, promptdialog;
{$R *.dfm}
{$R CaptureFreehand.Res}

procedure TCaptureFreehandForm.WMEraseBkGnd( var Msg: TWMEraseBkGnd );
begin
  Msg.Result := 1;
end;

procedure TCaptureFreehandForm.FormCreate( Sender: TObject );
var
  aDC: HDC;
  P: TPoint; // Where is the mouse now?
const
  crDraw = -19;
begin
  GetCursorPos( P ); // Where am I?
  if ( ForceMonitorNum >= 0 ) and ( ForceMonitorNum <= Screen.MonitorCount - 1 ) then
    M := ForceMonitorNum { Override }
  else
    M := Screen.MonitorFromPoint( P, mdNearest ).MonitorNum; // Here I am!  Monitors[M]

  Screen.Cursors[ crDraw ] := LoadCursor( hInstance, pChar( 'CURSOR_2' ) );
  Cursor := crDraw;
  fBMP := TBitmap.Create;
  DoPixelFormatFix( fBMP );
  fCapBMP := TBitmap.Create;
  DoPixelFormatFix( fCapBMP );
  fBMP.TransparentMode := tmAuto;
  fBMP.Transparent := True;
  fBMP.Width := Screen.Width;
  fBMP.Height := Screen.Height;
  aDC := GetDC( 0 );
  try
    BitBlt( fBMP.Canvas.handle, 0, 0, Screen.Width, Screen.Height, aDC, 0, 0, GetBitBlt_RopMode( ) );
  finally ;
    ReleaseDC( 0, aDC );
  end;
  SetBounds( 0, 0, Screen.Width, Screen.Height );
  fDragging := false;
  i := 0;
  fOrigBmp := TBitmap.Create;
  DoPixelFormatFix( fOrigBmp );
  fOrigBmp.Assign( fBMP );
  fXMin := 1000000;
  fYMin := 1000000;
  fXMax := 0;
  fYMax := 0;
  Memdat := AllocMem( ( i + 1 ) * Sizeof( TPoint ) );
end;

procedure TCaptureFreehandForm.FormMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  fDragging := true;
  SetRect( fRect, X, Y, X, Y ); // Set initial rectangle position
  if ShowHint then
  begin
    Hint := IntToStr( X ) + ' x ' + IntToStr( Y ) + ' pixels';
    Application.ActivateHint( Mouse.CursorPos );
  end;
end;

procedure TCaptureFreehandForm.FormMouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
begin
  if fdragging then
  begin
    with fBmp do
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Pen.Mode := pmNot;
      if i > 0 then
        Canvas.LineTo( X, Y )
      else
        Canvas.MoveTo( X, Y );
    end;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Mode := pmNot;
    if i > 0 then
      Canvas.LineTo( X, Y )
    else
      Canvas.MoveTo( X, Y );
    fXMin := Min( x, fXMin );
    fYMin := Min( y, fYMin );
    fXMax := Max( x, fXMax );
    fYMax := Max( y, fYMax );
    ReallocMem( memdat, ( i + 1 ) * Sizeof( TPoint ) );
    Memdat^[ i ].x := x;
    Memdat^[ i ].y := y;
    Inc( i );
    if ShowHint then
    begin
      Hint := IntToStr( fYMax - fYMin ) + ' x ' + IntToStr( fXMax - fXMin ) + ' pixels';
      Application.ActivateHint( Mouse.CursorPos );
      Update;
    end;
    fRect.Right := X; // Set the position of the rectangle for hint
    fRect.Bottom := Y;
    Application.ProcessMessages;
  end
end;

procedure TCaptureFreehandForm.FormMouseUp( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  ScreenDC: HDC;
  hrg: thandle;
  x1: longint;
  dxmax, dymax, dxmin, dymin: longint;
begin
  if Button = mbLeft then
  begin
    if i > 0 then
    begin
      fBMP.Assign( fOrigBmp );
      dxmax := 0;
      dymax := 0;
      dxmin := 99999;
      dymin := 99999;
      for x1 := 0 to i - 1 do
      begin
        if memdat^[ x1 ].x > dxmax then
          dxmax := memdat^[ x1 ].x;
        if memdat^[ x1 ].y > dymax then
          dymax := memdat^[ x1 ].y;
        if memdat^[ x1 ].x < dxmin then
          dxmin := memdat^[ x1 ].x;
        if memdat^[ x1 ].y < dymin then
          dymin := memdat^[ x1 ].y;
      end;
      for x1 := 0 to i - 1 do
      begin
        memdat^[ x1 ].x := memdat^[ x1 ].x - dxmin;
        memdat^[ x1 ].y := memdat^[ x1 ].y - dymin;
      end;
      hrg := CreatePolygonRgn( memdat^, i - 1, winding { alternate } );
      try
        FCapBMP.Width := dxmax - dxmin;
        FCapBMP.Height := dymax - dymin;
        SelectObject( FCapBMP.Canvas.Handle, hrg );
        ScreenDC := GetDC( 0 );
        try
          BitBlt( fCapBMP.Canvas.handle, 0, 0, dxmax - dxmin, dymax - dymin, fBMP.Canvas.Handle, dxmin, dymin, GetBitBlt_RopMode( ) );
        finally ;
          ReleaseDC( 0, ScreenDC );
        end;
      finally ;
        DeleteObject( hrg );
      end;
      Dispose( Memdat );
    end;
    fDragging := false;
    ModalResult := mrOK;
  end;
end;

procedure TCaptureFreehandForm.FormPaint( Sender: TObject );
begin
  Canvas.CopyRect( Canvas.ClipRect, fBmp.Canvas, Canvas.ClipRect );
end;

procedure TCaptureFreehandForm.FormDestroy( Sender: TObject );
begin
  fOrigBmp.free;
  FCapBMP.Free;
  fBMP.Free;
  CaptureFreehandForm := nil;
  Screen.Cursor := crDefault;
end;

procedure TCaptureFreehandForm.FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  if Key = vk_f8 then
  begin
    if Assigned( frmPosition ) then
      frmPosition.Free;
  end;
end;

end.


