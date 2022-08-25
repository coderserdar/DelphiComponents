//------------------------------------------------------------------------------
//  Apprehend Version  : 5.1
//  Copyright © 1986-2011 : Adirondack Software & Graphics
//  Created            : 01-09-1992
//  Last Modification  : 08-08-2011
//  Compiler           : Delphi 2010
//  Description        : CaptureTheDesktop Unit
// This file is copyright (C) W W Miller, 1986-2011.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

// Chris Thornton (thornsoft), 6/6/05
// Added MonitorNumber, to force capture from specific monitor,
// overriding the current cursor position method.
// If set to -1 (default), capture will be from montior where the mouse pointer
// resides. Otherwise, if set to 1,2,3, etc., capture will be from that
// specific monitor. This allows easy implementation of "capture screen 0",
// "capture screen 1", etc..  If monitor[MonitorNum] doesn't exist,
// it will default to the monitor where the cursor resides.

unit CaptureTheDesktop;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PixelFormatFix;

type
  TCaptureDesktopForm = class ( TForm )
    procedure FormCreate ( Sender: TObject );
    procedure FormActivate ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure FormMouseUp ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer );
  private
    { Private declarations }
    procedure WMEraseBkGnd ( var Msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
  public
    { Public declarations }
    fBmp: TBitmap;
    M: Integer; // Which monitor are we on?
  end;

var
  CaptureDesktopForm: TCaptureDesktopForm;
  ForceMonitorNum: Integer;

implementation

uses PromptDialog;

{$R *.DFM}

// Create the form
procedure TCaptureDesktopForm.FormCreate ( Sender: TObject );
var
  ScreenDC: HDC;
  lpPal: PLogPalette;
  P: TPoint; // Where is the mouse now?
begin
  GetCursorPos ( P ); // Where am I?
  if ( ForceMonitorNum >= 0 ) and ( ForceMonitorNum <= Screen.MonitorCount - 1 ) then
    M := ForceMonitorNum {Override}
  else
    M := Screen.MonitorFromPoint ( P, mdNearest ).MonitorNum; // Here I am!  Monitors[M]
  //MessageDlg(Format('Debugging!  Monitor: [%d], X:[%d], Y:[%d]',[M,P.X, P.Y]), mtInformation, [mbOK], 0);
  fBMP := TBitmap.Create;
  DoPixelFormatFix(fBMP);
  fBMP.Width := Screen.Monitors [ M ].Width; // Size to active monitor, which may differ from the primary.
  fBMP.Height := Screen.Monitors [ M ].Height;
  ScreenDC := GetDC ( 0 );
    // do we have a palette device? - Thanks to Joe C. Hecht
  if ( GetDeviceCaps ( ScreenDC, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
  begin
     // allocate memory for a logical palette
    GetMem ( lpPal, sizeof ( TLOGPALETTE ) + ( 255 * sizeof ( TPALETTEENTRY ) ) );
     // zero it out to be neat
    FillChar ( lpPal^, sizeof ( TLOGPALETTE ) + ( 255 * sizeof ( TPALETTEENTRY ) ), #0 );
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
  Sleep ( 500 );
  BitBlt ( fBMP.Canvas.handle, 0, 0, Screen.Monitors [ M ].Width, Screen.Monitors [ M ].Height, ScreenDC, Screen.Monitors [ M ].Left, Screen.Monitors [ M ].Top, GetBitBlt_RopMode() );
  ReleaseDC ( 0, ScreenDC );
  // Mouser 07/02/05 - set position and size even before activate (in case we dont want to activate?)
  Self.Left := Screen.Monitors [ M ].Left;
  Self.Top := Screen.Monitors [ M ].Top;
  Self.Width := Screen.Monitors [ M ].Width;
  Self.Height := Screen.Monitors [ M ].Height;
end;

// FormActivate
procedure TCaptureDesktopForm.FormActivate ( Sender: TObject );
begin
  Self.Left := Screen.Monitors [ M ].Left;
  Self.Top := Screen.Monitors [ M ].Top;
  Self.Width := Screen.Monitors [ M ].Width;
  Self.Height := Screen.Monitors [ M ].Height;
  inherited;
end;

// Form Destroy
procedure TCaptureDesktopForm.FormDestroy ( Sender: TObject );
begin
  fBMP.Free;
  Screen.Cursor := crDefault;
end;

// Process FormMouseUp event
procedure TCaptureDesktopForm.FormMouseUp ( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer );
begin
  frmPosition.Free;
  Sleep ( 500 );
  ModalResult := mrOK;
end;

// WMEraseBkGnd
procedure TCaptureDesktopForm.WMEraseBkGnd ( var Msg: TWMEraseBkGnd );
begin
  Msg.Result := 1;
end;


end.

