// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-03-2012
// Description             : ASGCapture Unit
// Function                : Component to capture bitmaps of the screen.
// Compiler                : Delphi 2010
// Latest version          : http://www.frontiernet.net/~w2m/index.html
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.                                                                       *
// Licence to use, terms and conditions:
// The code in the TASGCapture component is released as freeware
// provided you agree to the following terms & conditions:
// 1. the copyright notice, terms and conditions are
// left unchanged
// 2. modifications to the code by other authors must be
// clearly documented and accompanied by the modifier's name.
// 3. the TASGCapture component may be freely compiled into binary
// format and no acknowledgement is required. However, a
// discrete acknowledgement would be appreciated (eg. in a
// program's 'About Box' and/or help file).
//
// Acknowledgements: The key multimonitor portions in this component is developed by:
// Chris Thornton - www.thornsoft.com
// "Mouser" - www.donationcoder.com
//
// History: 1992-1999 - Developed screen capture routines
// July 4, 2000.       William Miller, first BETA version
// July 13, 2000.      William Miller, 2nd BETA version
// Changed CaptureRect.Pas to paint the rubberband
// on the form instead of a TImage to eliminate screen flicker.
// July 15, 2000.      William Miller, 3nd BETA version
// Added animated rubberbanding and version property
// September 21, 2000. William Miller. Compiled Release 1.0
// October 20, 2001.   William Miller. Delphi 6 Build Release 1.5
// March 16, 2002.     William Miller. Added ShowCursor Property
// May 20, 2002.       Added capture cursor option to capturedesktop
// June 23, 2002.      Added capture polygon function
// September 28, 2003. Modified demos, added Delphi 7 package
// June 24, 2004.      Added ShowHint property and CaptureREct Hint
// April 13, 2005.     Multi-Monitor support (Object, Rectangle, Desktop,
// WholeDesktop) (Chris Thornton - www.thornsoft.com)
// April 18, 2005.     Bitmap exposed (Chris Thornton)
// May 26, 2005.       Added CaptureIcon function (32x32 area)
// June 02, 2005.      Added Specific Size Selection, Various Multimonitor
// changes by ( Mouser )
// June 06, 2005.      Various Multimonitor changes by Chris Thornton
// Bugfix, CaptureRect appeared on the wrong monitor
// BugFix, runtime error in the marching ants routine
// Monitor: New property that affects the screen and
// rectangle capturing. Instead of relying on the cursor to
// determine the screen to capture from, you can now
// override by setting a MonitorNum=0,1,2,3,etc. property.
// Default of -1 causes it to use monitor where the mouse is
// located
// Added CaptureSpecificSizeSelection method
// "Mouser", 5/12/05
// Capturing large bitmaps very frequently caused crashing of my applications
// with insufficient memory type errors.
// After some time I tracked down this newsgroup post which suggests that
// the cure is to set the bitmap object to 24bits per pixel prior to filling it.
// See -> http://groups-beta.google.com/group/borland.public.delphi.graphics/browse_thread/thread/2575992b6bd62e66/203f09c94f0b1396?q=bitmap+delphi+%22not+enough+storage+is+available+to+process+this+command%22&rnum=1&hl=en#203f09c94f0b1396
// June 19, 2005.      Final 4.02 Release fizes...
// Multimonitor polygon fixes by ( Mouser )
// June 26, 2005.      SpeedCapture function developed
// Fix for pixelformat problems
// October 26, 2005    Added property ShowInfoDialog
// Nov 6. 2005.        Added specific region capture
// January 9, 2010     Added Delphi 2007-2010 packages
// Other Added Features
// functions for capturing specific HWND or TControl
// auto determine active foreground window location in bitmap
// fix for memory faults (see above)
// capturing cursors during multimon capture
// fixed cursor capture from active window
// Added Delphi 2010 demos
// May 14, 2010        Added property ShowInfoDialog from 4.2 and added infodialog to
//                     most capture methods
// April 03, 2012     Version 6.0
//                    Added public var CaptureCount
//                    Added ResetCaptureCount
//                    Added CaptureSmallIcon method (16x16)
//                    Added CaptureLargeIcon method (48x48)
//                    Added keyboard control ( for precise selection positioning )
//                    to CaptureSmallIcon (16x16), CaptureIcon (32x32),
//                    CaptureLargeIcon(48x48), and
//                    CaptureSpecificSizeSelection methods
//                    Added OnBeforeCaptureEvent
//                    Added OnAfterCaptureEvent
//                    Removed OnCaptureEvent - Move any code in the OnCaptureEvent to the
//                    OnAfterCaptureEvent
//                    Small Bug Fixes
//                    Updated demos
// April 09, 2012     Version 6.1
//                    Bug Fix for Delphi 7, Delphi 2007 -
//                    ASGCapture.pas- made D7 compatable - unfortunately SetParentComponent is protected in D7
//                    Changed all references to SetParentComponent ( xxx ); to frmPosition.Parent := xxx;
//                    Bug fixes TImage Demo
//                    To provide for a canvas all images loaded into TImage are converted to bitmap.  Saving did not convert the bitmap to the native graphic format before saving.  All TImage.Picture.Bitmaps are converted and saved as the specified graphic format.
//                    The image fit code was missing for captures and open image
//                    Dimensions now displayed with locale default thousand separator
//                    Bug fixes PageControlTImageDemo - minor bug fixes
//                    Added Apprehend TImagePainter Demo
//                    Added Delphi XE and XE2 packages
// ------------------------------------------------------------------------------

unit ASGCapture;

// {$D-}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, Clipbrd, PixelFormatFix;

const
  ASG_COMPONENT_VERSION = '6.0';

var
  GlobalSleepingTime: integer = 20;
  MinGlobalSleepingTime: integer = 20;
  MaxGlobalSleepingTime: integer = 100;
  SelectionColor1: TColor;
  SelectionColor2: TColor;

type
  PTRect = ^TRect;

  TASGScreenCapture = class ( TComponent )
  private
    fAutomatic: boolean; // send image to clipboard
    fBitmap: TBitmap; // captured bitmap
    fDelay: integer; // time needed for screen refresh
    // delay for screen refresh - may be set by enduser in Delay property
    fMinimized: boolean; // minimise the form before capture
    fShowHint: boolean; // show hint in CaptureRect
    fShowCursor: boolean; // show the cursor in capturedesktop
    fOnBeforeCapture: TNotifyEvent; // fires before capture
    fOnAfterCapture: TNotifyEvent; // fires after capture
    fMonitorNum: integer; // force this screen in multi-monitor system
    fWidth: integer;
    fHeight: integer;
    fShowInfoDialog: boolean; // show or hide info dialog
    fVersion: string;
    fCaptureCount: integer;
    procedure SetDelay ( const Value: integer );
    procedure SetAutomatic ( const Value: boolean );
    procedure SetMinimized ( const Value: boolean );
    procedure SetCursor ( const Value: boolean );
    procedure SetShowHint ( const Value: boolean );
    function GetVersion: string;
    procedure SetVersion ( const Val: string );
    procedure SetShowInfoDialog ( const Value: boolean );
    procedure CopyToClipboard;
    // procedure SetCaptureCount(const Value: integer);
    // Mouser, 5/15/05 - these provide support for auto-detecting active window
    // so that caller can know where in the bitmap image the active window was.  this is used in my screenshot
    // capture app to highlight the current window in screenshots.
  private
    fObjectLeft: integer; // x pos of active object (during desktop capture)
    fObjectTop: integer; // y pos of active object (during desktop capture)
    fObjectWidth: integer; // width pos of active object (during desktop capture)
    fObjectHeight: integer;
    // height pos of active object (during desktop capture)
    fObjectRLeft: integer; // x pos of active object (during desktop capture)
    fObjectRTop: integer; // y pos of active object (during desktop capture)
    fObjectRWidth: integer;
    // width pos of active object (during desktop capture)
    fObjectRHeight: integer;
    // height pos of active object (during desktop capture)
  public
    CaptureCount: integer;
    procedure ResetCaptureCount;
    property Bitmap: TBitmap read fBitmap;
    // Moved to "public", to expose to parent form.
    constructor Create ( aOwner: TComponent ); override;
    destructor Destroy; override;
    // Capture the desktop
    function CaptureDesktop: TBitmap;
    // Speed capture the desktop
    function SpeedCaptureDesktop: TBitmap;
    // Capture a bitmap image of the active window
    function CaptureWholeDesktop: TBitmap;
    // Capture a bitmap image of the whole desktop
    function CaptureActiveWindow: TBitmap;
    // Capture a bitmap of a selected object (Window, button, toolbar)
    function CaptureObject: TBitmap;
    // Capture a bitmap of a selected area
    function CaptureSelection: TBitmap;
    // Capture a bitmap of freehand selected area
    function CapturePolygon: TBitmap;
    // Capture a bitmap of 16x16 selected area
    function CaptureSmallIcon: TBitmap;
    // Capture a bitmap of 32x32 selected area
    function CaptureIcon: TBitmap;
    // Capture a bitmap of 48x48 selected area
    function CaptureLargeIcon: TBitmap;
    // Capture a bitmap of WidthxHeight selected area
    function CaptureSpecificSizeSelection: TBitmap;
    function CaptureSpecificSize ( CaptureWidth: integer; CaptureHeight: integer ): TBitmap;
    function CaptureSpecificRegion ( CaptureX: integer; CaptureY: integer; CaptureWidth: integer;
      CaptureHeight: integer ): TBitmap;
    // ATTN: Mouser (www.donationcoder.com), 5/10/05 - you can manually capture an object or TControl
    // very useful for capturing image of a specific form for example without user interaction, so you can print it, etc.
    function CaptureObjectByHWND ( Handles: integer; var scrapetext: string; onlygrabclient: Bool = False ): TBitmap;
    function CaptureObjectByTControlp ( const tcontrolp: TControl; var scrapetext: string ): TBitmap;
    function CaptureObjectByHWND_AutoScroll ( Handles: integer; scrollmode: integer;
      Exceptionstrings: string = '' ): TBitmap;
    function CaptureObjectByTControlp_AutoScroll ( const tcontrolp: TControl; scrollmode: integer;
      Exceptionstrings: string = '' ): TBitmap;
    function CaptureSpecificRegionPure ( CaptureX: integer; CaptureY: integer; CaptureWidth: integer;
      CaptureHeight: integer ): TBitmap;
    // ATTN: Mouser new helpers
    function MarkDc ( ScreenDc: hDC; Left: integer; Top: integer; CapW: integer; CapH: integer; Canvas: HWND ): integer;
    function LocateMarkerOffsetVertical ( abitmap: TBitmap; CapW: integer; CapH: integer; Left: integer;
      Top: integer ): integer;
    function LocateMarkerOffsetHorizontal ( abitmap: TBitmap; CapW: integer; CapH: integer; Left: integer;
      Top: integer ): integer;
    function IsBlankBitmap ( abitmap: TBitmap; CapW: integer; CapH: integer ): boolean;
    //
    procedure ScrollWindow_FullLeft ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure ScrollWindow_FullTop ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure ScrollWindow_OneDown ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure ScrollWindow_OneRight ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure ScrollWindow_OneUp ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure ScrollWindow_OneLeft ( Handles: integer; DrawDC: hDC; scrollmode: integer; dorepaint: boolean );
    procedure InducePartialRepaint ( Handles: integer; DrawDC: hDC );
    //
    procedure ForceWinRectRedraw ( Handles: integer; CRect: TRect; DrawDC: hDC );
    function LetUserPicksWindowHandle ( ): integer;
    function CalcWindowExceptions ( windowclassname: string; Exceptionstrings: string; var x1o: integer;
      var y1o: integer; var x2o: integer; var y2o: integer; var scrollmode: integer;
      var stoponnochange: boolean; var extras: string ): integer;
    procedure ParseDelimited ( const sl: TStrings; const Value: string; const delimiter: string );
    //
    procedure SimulateKeyDown ( MyKey: cardinal );
    procedure SimulateKeyUp ( MyKey: cardinal );
    procedure SimulateMouseEvent ( MyKey: cardinal );
    //
    function JR_GetSystemMetrics_YHScroll ( ): integer;
    function JR_GetSystemMetrics_XVScroll ( ): integer;
  published
    // If Auto is true then copy captured bitmap to clipboard
    property Auto: boolean read fAutomatic write fAutomatic default True;
    // Setting for Screen Refresh Time
    property Delay: integer read fDelay write fDelay default 500;
    // If Minimize is true then the mainform of the application is
    // minimized and restored during screen capture
    property Minimize: boolean read fMinimized write fMinimized default True;
    // show or hide cursor during screen capture
    property ShowCursor: boolean read fShowCursor write fShowCursor default True;
    // If ShowHint is true then a hint is shown in DrawRect
    property ShowHint: boolean read fShowHint write fShowHint default True;
    // Event that is executed before any capture
    property OnBeforeCapture: TNotifyEvent read fOnBeforeCapture write fOnBeforeCapture;
    // Event that is executed after any capture
    property OnAfterCapture: TNotifyEvent read fOnAfterCapture write fOnAfterCapture;
    // Show component version
    // property OnCapturewas removed on April 4, 2012 version 6.0
    property Version: string read GetVersion write SetVersion;
    // Force any particular screen with multi-monitor setup here.
    property MonitorNum: integer read fMonitorNum write fMonitorNum default -1;
    // ATTN: Mouser, 5/15/05 - these provide support for auto-detecting active window
    // so that caller can know where in the bitmap image the active window was.  this is used in my screenshot
    // capture app to highlight the current window in screenshots.
    property ShowInfoDialog: boolean read fShowInfoDialog write fShowInfoDialog default False;
    // Displays a small window at the top right side of the screen to display user interaction information
    // with apprehend
    property ObjectLeft: integer read fObjectLeft write fObjectLeft default 0;
    property ObjectTop: integer read fObjectTop write fObjectTop default 0;
    property ObjectWidth: integer read fObjectWidth write fObjectWidth default 0;
    property ObjectHeight: integer read fObjectHeight write fObjectHeight default 0;
    //
    property ObjectRLeft: integer read fObjectRLeft write fObjectRLeft default 0;
    property ObjectRTop: integer read fObjectRTop write fObjectRTop default 0;
    property ObjectRWidth: integer read fObjectRWidth write fObjectRWidth default 0;
    property ObjectRHeight: integer read fObjectRHeight write fObjectRHeight default 0;
  end;

procedure Register;

implementation

uses
  CaptureTheDesktop, { for CaptureTheDesktop Form - nneded for cursor and onkey event }
  CaptureTheObject, { for CaptureTheObject Form  - needed for cursor }
  CaptureObjectHighlight, { for CaptureObjectHighlight Form  - needed for cursor }
  CaptureTheRect, { for CaptureTheRect Form  - needed for animated rubberbanding }
  CaptureFreeHand, { for CaptureFreehand form - needed for prompt dialog, cursor and selection }
  CaptureSmallIcon, { for CaptureIcon (16x16) Form  - needed for animated rubberbanding }
  CaptureIcon, { for CaptureIcon (32x32) Form  - needed for animated rubberbanding }
  CaptureLargeIcon, { for CaptureLargeIcon (48x48) Form  - needed for animated rubberbanding }
  CaptureSpecificRect, { for SpecificRect Form  - needed for animated rubberbanding }
  SelectionDimensions, { for Specific Rect - to select width and height }
  PromptDialog; { for prompt dialog }

// Create the component

constructor TASGScreenCapture.Create ( aOwner: TComponent );
begin
  inherited Create ( aOwner );
  fAutomatic := True;
  fMinimized := True;
  fShowCursor := False;
  fShowHint := True;
  fDelay := 500;
  fBitmap := TBitmap.Create;
  fWidth := 32;
  fHeight := 32;
  fMonitorNum := -1;
  fVersion := ASG_COMPONENT_VERSION;
  fShowInfoDialog := False;
  fCaptureCount := 0;
  DoPixelFormatFix ( fBitmap );
  SetDelay ( fDelay );
  SetAutomatic ( fAutomatic );
  SetMinimized ( fMinimized );
  SetCursor ( fShowCursor );
  SetShowHint ( fShowHint );
  SetShowInfoDialog ( fShowInfoDialog );
end;

destructor TASGScreenCapture.Destroy;
// Destroy the component
begin
  fBitmap.Free;
  inherited Destroy;
end;

procedure TASGScreenCapture.SetDelay ( const Value: integer );
// Set delay for screen refresh
begin
  fDelay := Value;
end;

procedure TASGScreenCapture.SetMinimized ( const Value: boolean );
// Set minimized property
begin
  if Value <> fMinimized then
    fMinimized := Value;
end;

procedure TASGScreenCapture.SetAutomatic ( const Value: boolean );
// Set automatic property
begin
  fAutomatic := Value;
end;

// procedure TASGScreenCapture.SetCaptureCount(const Value: integer);
// Set CaptureCount property
// begin
// fCaptureCount := Value;
// end;

procedure TASGScreenCapture.SetCursor ( const Value: boolean );
// Get showcursor value
begin
  if Value <> fShowCursor then
    fShowCursor := Value;
end;

procedure TASGScreenCapture.SetShowHint ( const Value: boolean );
// Set showhint property
begin
  fShowHint := Value;
end;

procedure TASGScreenCapture.SetShowInfoDialog ( const Value: boolean );
// Set ShowInfoDialog property
begin
  if Value <> fShowInfoDialog then
    fShowInfoDialog := Value;
end;

function TASGScreenCapture.GetVersion: string;
begin
  Result := fVersion;
end;

procedure TASGScreenCapture.SetVersion ( const Val: string );
// Set version property
begin
  // Empty write method, just needed to get it to show up in Object Inspector
  // fVersion := Val;
end;

procedure TASGScreenCapture.CopyToClipboard;
// Copies bitmap to clipboard
begin
  Clipboard.Assign ( fBitmap );
end;

function GetForm ( pComponent: TComponent ): pointer;
var
  lOwner: TComponent;
begin
  lOwner := pComponent;
  while not ( lOwner is tForm ) do
    lOwner := lOwner.Owner;
  Result := lOwner;
end;

function TASGScreenCapture.CaptureDesktop: TBitmap;
// Capture Image of Screen or windows Desktop
// Multi-Monitor by Chris Thornton 04/14/2005
// Note: Multi-Monitor aware, but only captures CURRENT monitor only.
// Use CaptureWholeDesktop for wide-format, multi-monitor capture.
var
  Handles: HWND;
  ScreenDc: hDC;
  Rect: TRect;
  lpPal: PLogPalette;
  CursorInfo: TCursorInfo;
  hCursor: HICON;
  rCursor: TIconInfo;
  iCursorLeft: integer;
  iCursorTop: integer;
  iMouseThread: cardinal;
  iCurrentThread: cardinal;
  p: TPoint;
  // Mouser, 5/15/05 - support for identifying active foreground window
  ActHandles: HWND;
  ActRect: TRect;
  // Mouser - for deciding what monitor we are on
  M: integer;
  dml: integer;
  dmt: integer;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Result := nil;
  CaptureTheDesktop.ForceMonitorNum := fMonitorNum;
  // Thornsoft - capture from specific monitor
  // ATTN: Mouser (www.donationcoder.com), 5/15/05 - support for identifying active foreground window
  ActHandles := GetForegroundWindow ( );
  ScreenDc := GetWindowDC ( ActHandles );
  try
    GetWindowRect ( ActHandles, ActRect );
    fObjectLeft := ActRect.Left;
    fObjectTop := ActRect.Top;
    fObjectWidth := ActRect.Right - ActRect.Left;
    fObjectHeight := ActRect.Bottom - ActRect.Top;
  finally
    ReleaseDC ( ActHandles, ScreenDc );
  end;
  if fShowCursor then
  begin
    // ATTN: mouser - moved this inside OrigMethod Check
    CaptureDesktopForm := TCaptureDesktopForm.Create ( Self );
    try
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( CaptureDesktopForm );
        frmPosition.Label1.Caption :=
          'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button to capture an image of the the desktop and the cursor.';
        frmPosition.Show;
        frmPosition.Invalidate;
      end;
      if fShowInfoDialog then
      begin
        frmPosition.Left := Screen.Monitors [ CaptureDesktopForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureDesktopForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
      // If property set (larger than -1), CaptureDesktopForm.M will now be fMonitorNum
      if CaptureDesktopForm.ShowModal = mrOk then
      begin
        // Give screen time to refresh by delay
        Sleep ( fDelay );
        Handles := GetDesktopWindow ( );
        ScreenDc := GetDC ( Handles );
        GetWindowRect ( Handles, Rect );
        // ATTN: Mouser (www.donationcoder.com), 5/18/05 - needs correction for multimon case (note this code is never called by me since i have this block tested on FALSE)
        fObjectLeft := ActRect.Left - CaptureDesktopForm.Left;
        fObjectTop := ActRect.Top - CaptureDesktopForm.Top;
        // Do we have a palette device? - Thanks to Joe C. Hecht
        if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
        begin
          // Allocate memory for a logical palette
          GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
          // Zero it out to be neat
          FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
          // Fill in the palette version
          lpPal^.palVersion := $300;
          // Grab the system palette entries
          lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
          if ( lpPal^.palNumEntries <> 0 ) then
            // Create the palette
            fBitmap.Palette := CreatePalette ( lpPal^ );
          FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        end;
        fBitmap.Width := Screen.Monitors [ CaptureDesktopForm.M ].Width;
        fBitmap.Height := Screen.Monitors [ CaptureDesktopForm.M ].Height;
        BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, CaptureDesktopForm.fBmp.Canvas.Handle, 0,
          0, GetBitBlt_RopMode ( ) );
        try
          // Get cursor information
          GetCursorPos ( p );
          CursorInfo.cbSize := SizeOf ( CursorInfo );
          GetCursorInfo ( CursorInfo );
          iMouseThread := GetWindowThreadProcessId ( WindowFromPoint ( p ), nil );
          iCurrentThread := GetCurrentThreadId ( );
          if iCurrentThread = iMouseThread then
            hCursor := GetCursor ( )
          else
          begin
            AttachThreadInput ( iCurrentThread, iMouseThread, True );
            hCursor := GetCursor ( );
            AttachThreadInput ( iCurrentThread, iMouseThread, False );
          end;
          // determine absolute position of cursor image
          GetIconInfo ( hCursor, rCursor );
          //
          iCursorLeft := p.X - integer ( rCursor.xHotspot );
          iCursorTop := p.Y - integer ( rCursor.yHotspot );
          DrawIconEx ( fBitmap.Canvas.Handle, iCursorLeft - Screen.Monitors [ CaptureDesktopForm.M ].Left,
            iCursorTop - Screen.Monitors [ CaptureDesktopForm.M ].Top, CursorInfo.hCursor, 0, 0, 0, 0, DI_NORMAL );
        finally
          ReleaseDC ( Handles, ScreenDc );
        end;
        Result := TBitmap.Create;
        // DoPixelFormatFix( Result );
        fBitmap.PixelFormat := pf24bit;
        Result.Assign ( fBitmap );
        if fAutomatic then
          CopyToClipboard;
        if Assigned ( fOnAfterCapture ) then
          fOnAfterCapture ( Self );
        Inc ( fCaptureCount );
        CaptureCount := fCaptureCount;
      end;
    finally
      CaptureDesktopForm.Free;
    end;
  end
  else
  begin
    // If property set (larger than -1), CaptureDesktopForm.M will now be fMonitorNum
    Handles := GetDesktopWindow ( );
    ScreenDc := GetDC ( Handles );
    // Mouser, 5/12/05 -  get cursor before showing form in case we want to overlay it
    GetCursorPos ( p );
    CursorInfo.cbSize := SizeOf ( CursorInfo );
    GetCursorInfo ( CursorInfo );
    iMouseThread := GetWindowThreadProcessId ( WindowFromPoint ( p ), nil );
    iCurrentThread := GetCurrentThreadId ( );
    if iCurrentThread = iMouseThread then
      hCursor := GetCursor ( )
    else
    begin
      AttachThreadInput ( iCurrentThread, iMouseThread, True );
      hCursor := GetCursor ( );
      AttachThreadInput ( iCurrentThread, iMouseThread, False );
    end;
    // determine absolute position of cursor image
    GetIconInfo ( hCursor, rCursor );
    // Mouser - decide what monitor to capture from
    if ( fMonitorNum >= 0 ) and ( fMonitorNum <= Screen.MonitorCount - 1 ) then
      M := fMonitorNum { Override }
    else
      M := Screen.MonitorFromPoint ( p, mdNearest ).MonitorNum;
    // decide left, top, width, height
    dml := Screen.Monitors [ M ].Left;
    dmt := Screen.Monitors [ M ].Top;
    // Mouser, 5/18/05 -  correct for multimon capture
    fObjectLeft := ActRect.Left - dml;
    fObjectTop := ActRect.Top - dmt;
    p.X := p.X - dml;
    p.Y := p.Y - dmt;
    try
      GetWindowRect ( Handles, Rect );
      // Do we have a palette device? - Thanks to Joe C. Hecht
      if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
      begin
        // Allocate memory for a logical palette
        GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        // Zero it out to be neat
        FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
        // Fill in the palette version
        lpPal^.palVersion := $300;
        // Grab the system palette entries
        lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
        if ( lpPal^.palNumEntries <> 0 ) then
          // Create the palette
          fBitmap.Palette := CreatePalette ( lpPal^ );
        FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      end;
      fBitmap.Width := Screen.Monitors [ M ].Width;
      fBitmap.Height := Screen.Monitors [ M ].Height;
      // Need to take multiple monitors into consideration here too
      // Mouser - old way uses capturedesktopform, new way grabs from screen
      // BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, CaptureDesktopForm.fBmp.Canvas.Handle, 0, 0, GetBitBlt_RopMode() );
      BitBlt ( fBitmap.Canvas.Handle, 0, 0, Screen.Monitors [ M ].Width, Screen.Monitors [ M ].Height, ScreenDc,
        Screen.Monitors [ M ].Left, Screen.Monitors [ M ].Top, GetBitBlt_RopMode ( ) );
      // Mouser, 5/18/05 - ok here is our improved mouse cursor capture code
      if fShowCursor then
      begin
        // To overlay cursor on screen dump
        iCursorLeft := p.X - integer ( rCursor.xHotspot );
        iCursorTop := p.Y - integer ( rCursor.yHotspot );
        if ( p.X > 0 ) and ( p.Y > 0 ) and ( p.X < fBitmap.Width - 16 ) and ( p.Y < fBitmap.Height - 16 ) then
        begin
          DrawIconEx ( fBitmap.Canvas.Handle, iCursorLeft, iCursorTop, CursorInfo.hCursor, 0, 0, 0, 0, DI_NORMAL );
        end
      end;
    finally
      ReleaseDC ( Handles, ScreenDc );
    end;
    Result := TBitmap.Create;
    DoPixelFormatFix ( Result );
    Result.Assign ( fBitmap );
    if fAutomatic then
      CopyToClipboard;
    if Assigned ( fOnAfterCapture ) then
      fOnAfterCapture ( Self );
    Inc ( fCaptureCount );
    CaptureCount := fCaptureCount;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.SpeedCaptureDesktop: TBitmap;
var
  Handles: HWND;
  ScreenDc: hDC;
  Rect: TRect;
  lpPal: PLogPalette;
  CursorInfo: TCursorInfo;
  hCursor: HICON;
  rCursor: TIconInfo;
  iCursorLeft: integer;
  iCursorTop: integer;
  iMouseThread: cardinal;
  iCurrentThread: cardinal;
  p: TPoint;
  ActHandles: HWND;
  ActRect: TRect;
  M: integer;
  dml: integer;
  dmt: integer;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  CaptureTheDesktop.ForceMonitorNum := fMonitorNum;
  // Thornsoft - capture from specific monitor
  // Mouser, 5/15/05 - support for identifying active foreground window
  ActHandles := GetForegroundWindow ( );
  ScreenDc := GetWindowDC ( ActHandles );
  try
    GetWindowRect ( ActHandles, ActRect );
    fObjectLeft := ActRect.Left;
    fObjectTop := ActRect.Top;
    fObjectWidth := ActRect.Right - ActRect.Left;
    fObjectHeight := ActRect.Bottom - ActRect.Top;
  finally
    ReleaseDC ( ActHandles, ScreenDc );
  end;
  Handles := GetDesktopWindow ( );
  ScreenDc := GetDC ( Handles );
  try
    // Mouser, 5/12/05 -  get cursor before showing form in case we want to overlay it
    GetCursorPos ( p );
    CursorInfo.cbSize := SizeOf ( CursorInfo );
    GetCursorInfo ( CursorInfo );
    iMouseThread := GetWindowThreadProcessId ( WindowFromPoint ( p ), nil );
    iCurrentThread := GetCurrentThreadId ( );
    if iCurrentThread = iMouseThread then
      hCursor := GetCursor ( )
    else
    begin
      AttachThreadInput ( iCurrentThread, iMouseThread, True );
      hCursor := GetCursor ( );
      AttachThreadInput ( iCurrentThread, iMouseThread, False );
    end;
    // determine absolute position of cursor image
    GetIconInfo ( hCursor, rCursor );
    // Mouser - decide what monitor to capture from
    if ( fMonitorNum >= 0 ) and ( fMonitorNum <= Screen.MonitorCount - 1 ) then
      M := fMonitorNum { Override }
    else
      M := Screen.MonitorFromPoint ( p, mdNearest ).MonitorNum;
    // Here I am!  Monitors[M]
    // decide left, top, width, height
    dml := Screen.Monitors [ M ].Left;
    dmt := Screen.Monitors [ M ].Top;
    // Mouser, 5/18/05 -  correct for multimon capture
    fObjectLeft := ActRect.Left - dml;
    fObjectTop := ActRect.Top - dmt;
    p.X := p.X - dml;
    p.Y := p.Y - dmt;
    GetWindowRect ( Handles, Rect );
    // Do we have a palette device? - Thanks to Joe C. Hecht
    if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
    begin
      // Allocate memory for a logical palette
      GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      // Zero it out to be neat
      FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
      // Fill in the palette version
      lpPal^.palVersion := $300;
      // Grab the system palette entries
      lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
      if ( lpPal^.palNumEntries <> 0 ) then
        // Create the palette
        fBitmap.Palette := CreatePalette ( lpPal^ );
      FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
    end;
    fBitmap.Width := Screen.Monitors [ M ].Width;
    fBitmap.Height := Screen.Monitors [ M ].Height;
    // Need to take multiple monitors into consideration here too
    // Mouser - old way uses capturedesktopform, new way grabs from screen
    BitBlt ( fBitmap.Canvas.Handle, 0, 0, Screen.Monitors [ M ].Width, Screen.Monitors [ M ].Height, ScreenDc,
      Screen.Monitors [ M ].Left, Screen.Monitors [ M ].Top, GetBitBlt_RopMode ( ) );
    // Mouser, 5/18/05 - ok here is our improved mouse cursor capture code
    if fShowCursor then
    begin
      // determine absolute position of cursor image
      iCursorLeft := p.X - integer ( rCursor.xHotspot );
      iCursorTop := p.Y - integer ( rCursor.yHotspot );
      // To overlay cursor on screen dump
      if ( p.X > 0 ) and ( p.Y > 0 ) and ( p.X < fBitmap.Width - 16 ) and ( p.Y < fBitmap.Height - 16 ) then
      begin
        DrawIconEx ( fBitmap.Canvas.Handle, iCursorLeft, iCursorTop, CursorInfo.hCursor, 0, 0, 0, 0, DI_NORMAL );
      end
    end;
  finally
    ReleaseDC ( Handles, ScreenDc );
  end;
  Result := TBitmap.Create;
  DoPixelFormatFix ( Result );
  Result.Assign ( fBitmap );
  if fAutomatic then
    CopyToClipboard;
  if Assigned ( fOnAfterCapture ) then
    fOnAfterCapture ( Self );
  Inc ( fCaptureCount );
  CaptureCount := fCaptureCount;
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureWholeDesktop: TBitmap;
// CaptureWholeDesktop
var
  Handles: HWND;
  ScreenDc: hDC;
  lpPal: PLogPalette;
  // ATTN: Mouser, 5/15/05 - support for identifying active foreground window, and grabbing cursor
  p: TPoint;
  w: integer;
  H: integer;
  ActHandles: HWND;
  ActRect: TRect;
  CursorInfo: TCursorInfo;
  hCursor: HICON;
  rCursor: TIconInfo;
  iCursorLeft: integer;
  iCursorTop: integer;
  iMouseThread: cardinal;
  iCurrentThread: cardinal;
begin
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  // Mouser, 5/15/05 - support for identifying active foreground window
  ActHandles := GetForegroundWindow ( );
  ScreenDc := GetWindowDC ( ActHandles );
  try
    GetWindowRect ( ActHandles, ActRect );
    fObjectLeft := ActRect.Left - 0;
    fObjectTop := ActRect.Top - 0;
    fObjectWidth := ActRect.Right - ActRect.Left;
    fObjectHeight := ActRect.Bottom - ActRect.Top;
  finally
    ReleaseDC ( ActHandles, ScreenDc );
  end;
  // Mouser, 5/18/05 - needs correction for multimon case
  fObjectLeft := ActRect.Left - Screen.DesktopLeft;
  fObjectTop := ActRect.Top - Screen.DesktopTop;
  // Mouser, 5/12/05 -  get cursor before showing form in case we want to overlay it
  GetCursorPos ( p );
  CursorInfo.cbSize := SizeOf ( CursorInfo );
  GetCursorInfo ( CursorInfo );
  iMouseThread := GetWindowThreadProcessId ( WindowFromPoint ( p ), nil );
  iCurrentThread := GetCurrentThreadId ( );
  if iCurrentThread = iMouseThread then
    hCursor := GetCursor ( )
  else
  begin
    AttachThreadInput ( iCurrentThread, iMouseThread, True );
    hCursor := GetCursor ( );
    AttachThreadInput ( iCurrentThread, iMouseThread, False );
  end;
  // determine absolute position of cursor image
  GetIconInfo ( hCursor, rCursor );
  begin
    Handles := GetDesktopWindow ( );
    ScreenDc := GetDC ( Handles );
    try
      // Do we have a palette device? - Thanks to Joe C. Hecht
      if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
      begin
        // Allocate memory for a logical palette
        GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        // Zero it out to be neat
        FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
        // Fill in the palette version
        lpPal^.palVersion := $300;
        // Grab the system palette entries
        lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
        if ( lpPal^.palNumEntries <> 0 ) then
          // Create the palette
          fBitmap.Palette := CreatePalette ( lpPal^ );
        FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      end;
      // Simply draw the whole desktop from 0,0 onto the big, wide bitmap.
      // Mouser, 5/18/05 - needs correction for multimon case  (?)
      w := Screen.DesktopWidth;
      H := Screen.DesktopHeight;
      fBitmap.Width := w;
      fBitmap.Height := H;
      BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, ScreenDc, Screen.DesktopLeft,
        Screen.DesktopTop, GetBitBlt_RopMode ( ) );
      // Mouser, 5/18/05 - ok here is our improved mouse cursor capture code
      if fShowCursor then
      begin
        // To overlay cursor on screen dump
        p.X := p.X - Screen.DesktopLeft;
        p.Y := p.Y - Screen.DesktopTop;
        // determine absolute position of cursor image
        iCursorLeft := p.X - integer ( rCursor.xHotspot );
        iCursorTop := p.Y - integer ( rCursor.yHotspot );
        if ( p.X > 0 ) and ( p.Y > 0 ) and ( p.X < fBitmap.Width - 16 ) and ( p.Y < fBitmap.Height - 16 ) then
        begin
          DrawIconEx ( fBitmap.Canvas.Handle, iCursorLeft, iCursorTop, CursorInfo.hCursor, 0, 0, 0, 0, DI_NORMAL );
        end
      end;
    finally
      ReleaseDC ( Handles, ScreenDc );
    end;
    Result := TBitmap.Create;
    DoPixelFormatFix ( Result );
    Result.Assign ( fBitmap );
    if fAutomatic then
      CopyToClipboard;
    if Assigned ( fOnAfterCapture ) then
      fOnAfterCapture ( Self );
    Inc ( fCaptureCount );
    CaptureCount := fCaptureCount;
  end;
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureActiveWindow: TBitmap;
// Capture Active Window. If window not active then it captures desktop
var
  Handles: HWND;
  Rect: TRect;
  ScreenDc: hDC;
  lpPal: PLogPalette;
  CursorInfo: TCursorInfo;
  hCursor: HICON;
  rCursor: TIconInfo;
  iCursorLeft: integer;
  iCursorTop: integer;
  iMouseThread: cardinal;
  iCurrentThread: cardinal;
  p: TPoint;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Handles := GetForegroundWindow ( );
  ScreenDc := GetWindowDC ( Handles );
  try
    // Do we have a palette device? - Thanks to Joe C. Hecht
    if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
    begin
      // Allocate memory for a logical palette
      GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      // Zero it out to be neat
      FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
      // Fill in the palette version
      lpPal^.palVersion := $300;
      // Grab the system palette entries
      lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
      if ( lpPal^.palNumEntries <> 0 ) then
        // Create the palette
        fBitmap.Palette := CreatePalette ( lpPal^ );
      FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
    end;
    GetWindowRect ( Handles, Rect );
    fBitmap.Width := Rect.Right - Rect.Left;
    fBitmap.Height := Rect.Bottom - Rect.Top;
    BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, ScreenDc, 0, 0, SRCCOPY {GetBitBlt_RopMode( )} );
    if fShowCursor then
    begin
      // Get cursor information
      GetCursorPos ( p );
      CursorInfo.cbSize := SizeOf ( CursorInfo );
      GetCursorInfo ( CursorInfo );
      CursorInfo.cbSize := SizeOf ( CursorInfo );
      GetCursorInfo ( CursorInfo );
      iMouseThread := GetWindowThreadProcessId ( WindowFromPoint ( p ), nil );
      iCurrentThread := GetCurrentThreadId ( );
      if iCurrentThread = iMouseThread then
        hCursor := GetCursor ( )
      else
      begin
        AttachThreadInput ( iCurrentThread, iMouseThread, True );
        hCursor := GetCursor ( );
        AttachThreadInput ( iCurrentThread, iMouseThread, False );
      end;
      // determine absolute position of cursor image
      GetIconInfo ( hCursor, rCursor );
      // Mouser, 5/18/05 -  correct for window pos
      p.X := p.X - Rect.Left;
      p.Y := p.Y - Rect.Top;
      iCursorLeft := p.X - integer ( rCursor.xHotspot );
      iCursorTop := p.Y - integer ( rCursor.yHotspot );
      // To overlay cursor on screen dump
      DrawIconEx ( fBitmap.Canvas.Handle, iCursorLeft, iCursorTop, CursorInfo.hCursor, 32, 32, 0, 0, DI_NORMAL );
    end;
  finally
    ReleaseDC ( Handles, ScreenDc );
  end;
  Result := TBitmap.Create;
  DoPixelFormatFix ( Result );
  Result.Assign ( fBitmap );
  if fAutomatic then
    CopyToClipboard;
  if Assigned ( fOnAfterCapture ) then
    fOnAfterCapture ( Self );
  Inc ( fCaptureCount );
  CaptureCount := fCaptureCount;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureObject: TBitmap;
// Capture Selected Object
var
  Rect: TRect;
  P1: TPoint;
  Handles: HWND;
  ScreenDc: hDC;
  lpPal: PLogPalette;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  // ATTN: 06/18/05 - Force monitor (copied from capture rect procedure
  CaptureTheObject.ForceMonitorNum := fMonitorNum; // Thornsoft - capture from specific monitor
  CaptureObjectForm := TCaptureObjectForm.Create ( Self );
  try
    if fShowHint then
      CaptureObjectForm.ShowHint := True
    else
      CaptureObjectForm.ShowHint := False;
    // Show info dialog
    if fShowInfoDialog then
    begin
      frmPosition := TfrmPosition.Create ( Self );
      // make D7 compatable - unfortunately SetParentComponent is protected in D7
      //frmPosition.SetParentComponent ( CaptureObjectForm );
      frmPosition.Parent := CaptureObjectForm;
      frmPosition.FormStyle := fsStayOnTop;
      frmPosition.PopupMode := pmExplicit;
      frmPosition.PopupParent := CaptureObjectForm;
      frmPosition.Label1.Caption :=
        'Position the mouse cursor then click the left mouse button to capture an image of the selected object.';
      frmPosition.Invalidate;
      frmPosition.Show;
      frmPosition.Invalidate;
      frmPosition.Left := Screen.Monitors [ CaptureObjectForm.M ].Width - frmPosition.Width - 4;
      frmPosition.Top := Screen.Monitors [ CaptureObjectForm.M ].Top + 4;
      frmPosition.Invalidate;
    end;
    if CaptureObjectForm.ShowModal = mrOk then
    begin
      // Get cursor position
      GetCursorPos ( P1 );
      Handles := WindowFromPoint ( P1 );
      // Get mainform out of the way
      GetWindowRect ( Handles, Rect );
      with fBitmap, Rect do
      begin
        Width := ( Right - Left );
        Height := ( Bottom - Top );
        ScreenDc := GetDC ( 0 );
        try
          // Do we have a palette device? - Thanks to Joe C. Hecht
          if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
          begin
            // Allocate memory for a logical palette
            GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
            // Zero it out to be neat
            FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
            // Fill in the palette version
            lpPal^.palVersion := $300;
            // Grab the system palette entries
            lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
            if ( lpPal^.palNumEntries <> 0 ) then
              // Create the palette
              fBitmap.Palette := CreatePalette ( lpPal^ );
            FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
          end;
        finally;
          ReleaseDC ( 0, ScreenDc );
        end;
      end;
      // Need to take multiple monitors into consideration here too
      BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, CaptureObjectForm.fBmp.Canvas.Handle,
        Rect.Left - Screen.Monitors [ CaptureObjectForm.M ].Left,
        Rect.Top - Screen.Monitors [ CaptureObjectForm.M ].Top, GetBitBlt_RopMode ( ) );
      // save rectangle for a repeat capture?
      fObjectRLeft := Rect.Left;
      fObjectRTop := Rect.Top;
      fObjectRWidth := Rect.Right - Rect.Left;
      fObjectRHeight := Rect.Bottom - Rect.Top;
      // Copy bitmap to function result
      Result := TBitmap.Create;
      DoPixelFormatFix ( Result );
      Result.Assign ( fBitmap );
    end;
  finally
    CaptureObjectForm.Free;
  end;
  if fAutomatic then
    CopyToClipboard;
  if Assigned ( fOnAfterCapture ) then
    fOnAfterCapture ( Self );
  Inc ( fCaptureCount );
  CaptureCount := fCaptureCount;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureSelection: TBitmap;
// Capture Selection
// Mouser, 5/15/05 - support for identifying active foreground window
var
  ActHandles: HWND;
  ActRect: TRect;
  ScreenDc: hDC;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  // Mouser, 5/15/05 - support for identifying active foreground window
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // support for identifying active foreground window
    ActHandles := GetForegroundWindow ( ); // added 08/02/2011
    ScreenDc := GetWindowDC ( ActHandles );
    try
      GetWindowRect ( ActHandles, ActRect );
      fObjectLeft := ActRect.Left - 0;
      fObjectTop := ActRect.Top - 0;
      fObjectWidth := ActRect.Right - ActRect.Left;
      fObjectHeight := ActRect.Bottom - ActRect.Top;
    finally
      ReleaseDC ( ActHandles, ScreenDc );
    end;
    // Create and show form to capture Rect
    CaptureTheRect.ForceMonitorNum := fMonitorNum;
    // Thornsoft - capture from specific monitor
    CaptureRectForm := TCaptureRectForm.Create ( Self );
    try
      if fShowHint then
        CaptureRectForm.ShowHint := True
      else
        CaptureRectForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        // make D7 compatable
        //frmPosition.SetParentComponent ( CaptureRectForm );
        frmPosition.Parent := CaptureRectForm;
        frmPosition.FormStyle := fsStayOnTop;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureRectForm;
        frmPosition.Label1.Caption :=
          'Click the left mouse button and hold the left mouse button down and position the selection with the mouse by dragging a rectangle, then release the left mouse button to capture an image of the selection.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureRectForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureRectForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureRectForm.ShowModal = mrOk then
    begin
      // save rectangle for a repeat capture?
      fObjectRLeft := CaptureRectForm.fRect.Left;
      fObjectRTop := CaptureRectForm.fRect.Top;
      fObjectRWidth := CaptureRectForm.fRect.Right - CaptureRectForm.fRect.Left;
      fObjectRHeight := CaptureRectForm.fRect.Bottom - CaptureRectForm.fRect.Top;
      Result := TBitmap.Create;
      // Mouser, 5/18/05 - needs correction for multimon case
      fObjectLeft := CaptureRectForm.ScreenToBitmapX ( ActRect.Left );
      fObjectTop := CaptureRectForm.ScreenToBitmapY ( ActRect.Top );
      DoPixelFormatFix ( Result );
      fBitmap.Assign ( CaptureRectForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureRectForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CapturePolygon: TBitmap;
// CapturePolygon Selection
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    CaptureFreehandForm := TCaptureFreehandForm.Create ( Self );
    try
      if fShowHint then
        CaptureFreehandForm.ShowHint := True
      else
        CaptureFreehandForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        //frmPosition.SetParentComponent ( CaptureFreehandForm );
        frmPosition.Parent := CaptureFreehandForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureFreehandForm;
        frmPosition.Label1.Caption :=
          'Position the mouse to select a polygon area to capture then release the left mouse button.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureFreehandForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureFreehandForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureFreehandForm.ShowModal = mrOk then
    begin
      // Show selection
      fBitmap.Assign ( CaptureFreehandForm.fCapBMP );
      // Copy bitmap to function result
      Result := TBitmap.Create;
      DoPixelFormatFix ( Result );
      Result.Assign ( fBitmap );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureFreehandForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureSmallIcon: TBitmap;
// Capture 16x16 Selection
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // Create and show form to capture Rect
    CaptureSmallIconForm := TCaptureSmallIconForm.Create ( Self );
    try
      if fShowHint then
        CaptureSmallIconForm.ShowHint := True
      else
        CaptureSmallIconForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        //frmPosition.SetParentComponent ( CaptureSmallIconForm );
        frmPosition.Parent := CaptureSmallIconForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureSmallIconForm;
        frmPosition.Label1.Caption :=
          'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button or press the ENTER Key to capture an image of the the selection.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureSmallIconForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureSmallIconForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureSmallIconForm.ShowModal = mrOk then
    begin
      Result := TBitmap.Create;
      fBitmap.Assign ( CaptureSmallIconForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureSmallIconForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureIcon: TBitmap;
// Capture 32x32 Selection
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // Create and show form to capture Rect
    CaptureIconForm := TCaptureIconForm.Create ( Self );
    try
      if fShowHint then
        CaptureIconForm.ShowHint := True
      else
        CaptureIconForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        //frmPosition.SetParentComponent ( CaptureIconForm );
        frmPosition.Parent := CaptureIconForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureIconForm;
        frmPosition.Label1.Caption :=
          'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button or press the ENTER Key to capture an image of the the selection.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureIconForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureIconForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureIconForm.ShowModal = mrOk then
    begin
      Result := TBitmap.Create;
      fBitmap.Assign ( CaptureIconForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureIconForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureLargeIcon: TBitmap;
// Capture 48x48 Selection
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // Create and show form to capture Rect
    CaptureLargeIconForm := TCaptureLargeIconForm.Create ( Self );
    try
      if fShowHint then
        CaptureLargeIconForm.ShowHint := True
      else
        CaptureLargeIconForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        //frmPosition.SetParentComponent ( CaptureLargeIconForm );
        frmPosition.Parent := CaptureLargeIconForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureLargeIconForm;
        frmPosition.Label1.Caption :=
          'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button or press the ENTER Key to capture an image of the the selection.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureLargeIconForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureLargeIconForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureLargeIconForm.ShowModal = mrOk then
    begin
      Result := TBitmap.Create;
      fBitmap.Assign ( CaptureLargeIconForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureLargeIconForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureSpecificSizeSelection: TBitmap;
// Capture SpecificSize Selection
var
  CaptureWidth: integer;
  CaptureHeight: integer;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // Create and show form to capture Rect
    CaptureSpecificRect.ForceMonitorNum := fMonitorNum;
    CaptureSpecificRectForm := TCaptureSpecificRectForm.Create ( Self );
    try
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        // make D7 compatable
        //frmPosition.SetParentComponent ( CaptureSpecificRectForm );
        frmPosition.Parent := CaptureSpecificRectForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureSpecificRectForm;
        frmPosition.Caption := 'Dimensions';
        frmPosition.Label1.Caption := 'Select capture dimensions.';
        frmPosition.Show;
        frmPosition.Invalidate;
      end;
      SelectionDimensionForm := TSelectionDimensionForm.Create ( Self );
      try
        // make D7 compatable
        // SelectionDimensionForm.SetParentComponent( CaptureSpecificRectForm );
        SelectionDimensionForm.Position := poDesktopCenter;
        SelectionDimensionForm.PopupMode := pmExplicit;
        SelectionDimensionForm.PopupParent := CaptureSpecificRectForm;
        if fShowInfoDialog then
        begin
          frmPosition.Left := Screen.Monitors [ CaptureSpecificRectForm.M ].Width - frmPosition.Width - 4;
          frmPosition.Top := Screen.Monitors [ CaptureSpecificRectForm.M ].Top + 4;
          frmPosition.Invalidate;
        end;
        if SelectionDimensionForm.ShowModal = mrOk then
        begin
          CaptureHeight := SelectionDimensionForm.UpDown1.Position;
          CaptureWidth := SelectionDimensionForm.UpDown2.Position;
        end
        else
        begin
          frmPosition.Free;
          Application.MainForm.Show;
          if fMinimized then
            Application.MainForm.WindowState := wsNormal;
          exit;
        end;
        if fShowInfoDialog then
        begin
          frmPosition.Label1.Caption :=
            'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button or press the ENTER Key to capture an image of the the selection.';
          frmPosition.Invalidate;
        end;
        CaptureSpecificRectForm.CaptureHeight := CaptureHeight;
        CaptureSpecificRectForm.CaptureWidth := CaptureWidth;
        fWidth := CaptureWidth;
        fHeight := CaptureHeight;
        if fShowHint then
          CaptureSpecificRectForm.ShowHint := True
        else
          CaptureSpecificRectForm.ShowHint := False;
      finally
        Screen.Cursor := crDefault;
      end;
      if CaptureSpecificRectForm.ShowModal = mrOk then
      begin
        if Assigned ( frmPosition ) then
          frmPosition.Free;
        // save rectangle for a repeat capture
        fObjectRLeft := CaptureSpecificRectForm.fRect.Left;
        fObjectRTop := CaptureSpecificRectForm.fRect.Top;
        fObjectRWidth := CaptureSpecificRectForm.fRect.Right - fObjectRLeft;
        fObjectRHeight := CaptureSpecificRectForm.fRect.Bottom - fObjectRTop;
        Result := TBitmap.Create;
        fBitmap.Assign ( CaptureSpecificRectForm.RectBitmap );
        Result.Assign ( fBitmap );
        DoPixelFormatFix ( Result );
        if fAutomatic then
          CopyToClipboard;
        if Assigned ( fOnAfterCapture ) then
          fOnAfterCapture ( Self );
        Inc ( fCaptureCount );
        CaptureCount := fCaptureCount;
      end
      else
      begin
        fBitmap := nil;
        // Restore mainform to original state
        if fMinimized then
          Application.MainForm.Show;
        CaptureLargeIconForm.Free;
        exit;
      end;
    finally
      SelectionDimensionForm.Free;
    end;
  finally
    CaptureSpecificRectForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureSpecificSize ( CaptureWidth: integer; CaptureHeight: integer ): TBitmap;
// Capture SpecificSize Selection
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  Screen.Cursor := crHourglass; // prevent selection until ready
  try
    // Force monitor (copied from capture rect procedure
    CaptureSpecificRect.ForceMonitorNum := fMonitorNum; // Thornsoft - capture from specific monitor
    // Create and show form to capture Rect
    CaptureSpecificRectForm := TCaptureSpecificRectForm.Create ( Application );
    try
      CaptureSpecificRectForm.CaptureHeight := CaptureHeight;
      CaptureSpecificRectForm.CaptureWidth := CaptureWidth;
      fWidth := CaptureWidth;
      fHeight := CaptureHeight;
      if fShowHint then
        CaptureSpecificRectForm.ShowHint := True
      else
        CaptureSpecificRectForm.ShowHint := False;
      // Show info dialog
      if fShowInfoDialog then
      begin
        frmPosition := TfrmPosition.Create ( Self );
        //frmPosition.SetParentComponent ( CaptureSpecificRectForm );
        frmPosition.Parent := CaptureSpecificRectForm;
        frmPosition.PopupMode := pmExplicit;
        frmPosition.PopupParent := CaptureSpecificRectForm;
        frmPosition.Label1.Caption :=
          'Position the mouse cursor (with the mouse or keyboard arrows) then click the left mouse button to capture an image of the the selection.';
        frmPosition.Invalidate;
        frmPosition.Show;
        frmPosition.Left := Screen.Monitors [ CaptureSpecificRectForm.M ].Width - frmPosition.Width - 4;
        frmPosition.Top := Screen.Monitors [ CaptureSpecificRectForm.M ].Top + 4;
        frmPosition.Invalidate;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if CaptureSpecificRectForm.ShowModal = mrOk then
    begin
      Result := TBitmap.Create;
      fBitmap.Assign ( CaptureSpecificRectForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureSpecificRectForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureObjectByHWND ( Handles: integer; var scrapetext: string;
  onlygrabclient: Bool = False ): TBitmap;
// Mouser, 5/12/05 - capture specific window by Handle
// Capture Specific handle Object
var
  Rect: TRect;
  ScreenDc: hDC;
  lpPal: PLogPalette;
  dummylp: longint;
  scrapebuf: array [ 0..16000 ] of char;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  Sleep ( fDelay );
  // verify object exists
  if ( Handles <> 0 ) then
  begin
    dummylp := GetWindowLong ( Handles, GWL_HINSTANCE );
    if ( dummylp = 0 ) then
      Handles := 0;
  end;
  // ok we got one to capture
  if ( Handles = 0 ) then
  begin
    Result := TBitmap.Create;
    DoPixelFormatFix ( Result );
    exit;
  end;
  // scrape the text
  scrapetext := '';
  FillChar ( scrapebuf, 16000, 0 );
  SendMessage ( Handles, WM_GETTEXT, 15999, integer ( @scrapebuf [ 0 ] ) );
  scrapetext := string ( scrapebuf );
  if ( onlygrabclient ) then
    GetClientRect ( Handles, Rect )
  else
    GetWindowRect ( Handles, Rect );
  with fBitmap, Rect do
  begin
    Width := Right - Left;
    Height := Bottom - Top;
    ScreenDc := GetDC ( 0 );
    try
      // do we have a palette device? - Thanks to Joe C. Hecht
      if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
      begin
        // allocate memory for a logical palette
        GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        // zero it out to be neat
        FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
        // fill in the palette version
        lpPal^.palVersion := $300;
        // grab the system palette entries
        lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
        if ( lpPal^.palNumEntries <> 0 ) then
          // create the palette
          fBitmap.Palette := CreatePalette ( lpPal^ );
        FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      end;
      // save rectangle for a repeat capture
      fObjectRLeft := Left;
      fObjectRTop := Top;
      fObjectRWidth := Width;
      fObjectRHeight := Height;
      BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, ScreenDc, Rect.Left, Rect.Top,
        GetBitBlt_RopMode ( ) );
      // copy bitmap to function result
      Result := TBitmap.Create;
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    finally
      ReleaseDC ( 0, ScreenDc );
    end;
    // Restore mainform to original state
    if fMinimized then
      Application.MainForm.Show;
  end;
end;

function TASGScreenCapture.CaptureObjectByTControlp ( const tcontrolp: TControl; var scrapetext: string ): TBitmap;
// Mouser, 5/12/05 - Capture Specific Object by tcontrol
var
  Handles: HWND;
  tp: TPoint;
begin
  tp.X := 0;
  tp.Y := 0;
  tp := tcontrolp.ClientToScreen ( tp );
  Handles := WindowFromPoint ( tp );
  Result := CaptureObjectByHWND ( Handles, scrapetext );
end;

function TASGScreenCapture.CaptureSpecificRegion ( CaptureX: integer; CaptureY: integer; CaptureWidth: integer;
  CaptureHeight: integer ): TBitmap;
// Capture SpecificSize Selection
var
  Shift: TShiftState;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // Give screen time to refresh by delay
  Sleep ( fDelay );
  // Create and show form to capture Rect
  // ATTN: 06/18/05 - Force monitor (copied from capture rect procedure
  CaptureSpecificRect.ForceMonitorNum := fMonitorNum;
  // Thornsoft - capture from specific monitor
  CaptureSpecificRectForm := TCaptureSpecificRectForm.Create ( Application );
  try
    // use previous
    // CaptureSpecificRectForm.Rect.Left:=CaptureX;
    // CaptureSpecificRectForm.Rect.Top:=CaptureY;
    // CaptureSpecificRectForm.Rect.Height := CaptureHeight;
    // CaptureSpecificRectForm.Rect.Width := CaptureWidth;
    CaptureSpecificRectForm.CaptureHeight := CaptureHeight;
    CaptureSpecificRectForm.CaptureWidth := CaptureWidth;
    fWidth := CaptureWidth;
    fHeight := CaptureHeight;
    if fShowHint then
      CaptureSpecificRectForm.ShowHint := True
    else
      CaptureSpecificRectForm.ShowHint := False;
    CaptureSpecificRectForm.SetRectCoords ( CaptureX, CaptureY, CaptureWidth, CaptureHeight );
    CaptureSpecificRectForm.FormMouseUp ( nil, mbLeft, Shift, 0, 0 );
    // if CaptureSpecificRectForm.ShowModal = mrOK then
    // CaptureSpecificRectForm.Show;
    // CaptureSpecificRectForm.SetRectCoords(CaptureX,CaptureY,CaptureWidth,CaptureHeight);
    if ( True ) then
    begin
      Result := TBitmap.Create;
      fBitmap.Assign ( CaptureSpecificRectForm.RectBitmap );
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    end;
  finally
    CaptureSpecificRectForm.Free;
  end;
  // Restore mainform to original state
  if fMinimized then
    Application.MainForm.Show;
end;

function TASGScreenCapture.CaptureSpecificRegionPure ( CaptureX: integer; CaptureY: integer; CaptureWidth: integer;
  CaptureHeight: integer ): TBitmap;
// Capture SpecificSize Selection
var
  Rect, ActRect: TRect;
  ScreenDc: hDC;
  lpPal: PLogPalette;
  ActHandles: HWND;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  Sleep ( fDelay );
  // fill rect with capture info
  Rect.Left := CaptureX;
  Rect.Top := CaptureY;
  Rect.Bottom := Rect.Top + CaptureHeight;
  Rect.Right := Rect.Left + CaptureWidth;
  // support for identifying active foreground window
  ActHandles := GetForegroundWindow ( );
  ScreenDc := GetWindowDC ( ActHandles );
  try
    GetWindowRect ( ActHandles, ActRect );
    fObjectLeft := ActRect.Left - 0;
    fObjectTop := ActRect.Top - 0;
    fObjectWidth := ActRect.Right - ActRect.Left;
    fObjectHeight := ActRect.Bottom - ActRect.Top;
  finally
    ReleaseDC ( ActHandles, ScreenDc );
  end;
  // offset into selection
  fObjectLeft := ActRect.Left - Rect.Left;
  fObjectTop := ActRect.Top - Rect.Top;
  with fBitmap, Rect do
  begin
    Width := Right - Left;
    Height := Bottom - Top;
    ScreenDc := GetDC ( 0 );
    try
      // do we have a palette device - Thanks to Joe C. Hecht
      if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
      begin
        // allocate memory for a logical palette
        GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        // zero it out to be neat
        FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
        // fill in the palette version
        lpPal^.palVersion := $300;
        // grab the system palette entries
        lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
        if ( lpPal^.palNumEntries <> 0 ) then
          // create the palette
          fBitmap.Palette := CreatePalette ( lpPal^ );
        FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      end;
      // save rectangle for a repeat capture
      fObjectRLeft := Left;
      fObjectRTop := Top;
      fObjectRWidth := Width;
      fObjectRHeight := Height;
      BitBlt ( fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, ScreenDc, Rect.Left, Rect.Top,
        GetBitBlt_RopMode ( ) );
      // copy bitmap to function result
      Result := TBitmap.Create;
      Result.Assign ( fBitmap );
      DoPixelFormatFix ( Result );
      if fAutomatic then
        CopyToClipboard;
      if Assigned ( fOnAfterCapture ) then
        fOnAfterCapture ( Self );
      Inc ( fCaptureCount );
      CaptureCount := fCaptureCount;
    finally
      ReleaseDC ( 0, ScreenDc );
    end;
    // Restore mainform to original state
    if fMinimized then
      Application.MainForm.Show;
  end;
end;

function TASGScreenCapture.CaptureObjectByHWND_AutoScroll ( Handles: integer; scrollmode: integer;
  Exceptionstrings: string ): TBitmap;
// CaptureObjectByHWND_AutoScroll
// based on some code on delphi usenet post
// scrollmode: 0 = use pageup messages, 1 = use lineup messages, 2 = use keyboard simulation
var
  CRect, WRect, Rect: TRect;
  ScreenDc, DC: hDC;
  lpPal: PLogPalette;
  MemDC, hdcc: hDC;
  MemBitmap, OldBitmap: HBITMAP;
  LastX, LastY: integer;
  OldX, OldY: integer;
  X, Y: integer;
  CX, CY: integer;
  ExtraWidth, ExtraHeight: integer;
  ScrollInfo: TScrollInfo;
  ViewHeight, ViewWidth: integer;
  FinalHeight, FinalWidth: integer;
  CapW, CapH, CapOffsetX, CapOffsetY: integer;
  //
  P1: TPoint;
  iretv: LongBool;
  loopmode: integer;
  bretv: Bool;
  //
  priorbitmap: TBitmap;
  //
  vert_offsets: array [ 0..2000 ] of integer;
  horz_offsets: array [ 0..2000 ] of integer;
  vert_offset: integer;
  horz_offset: integer;
  vert_lastgoodoffsetindex: integer;
  horz_lastgoodoffsetindex: integer;
  vert_curoffsetindex: integer;
  horz_curoffsetindex: integer;
  //
  std_scrollbarHeight: integer;
  std_scrollbarWidth: integer;
  //
  windowclassname: string;
  wclassnamebuf: array [ 0..255 ] of char;
  //
  x1o, y1o, x2o, y2o: integer;
  customindex: integer;
  extras: string;
  cursorposold, cursorposnew: TPoint;
  maxgoodoffsets, maxtotaloffsets: integer;
  biggestsize: integer;
  //
  validscrollpos: boolean;
  lastscrollpos: integer;
  //
  stoponnochange: boolean;
  dummyt: integer;
  dummylp: longint;
  //
  noscroll_hplus: integer;
  noscroll_vplus: integer;
  //
  mode_try_attempt, max_scrollmodes: integer;
  //
  leftm, topm: integer;
begin
  if Assigned ( fOnBeforeCapture ) then
    fOnBeforeCapture ( Self );
  Result := nil;
  // Get mainform out of the way
  if fMinimized then
    Application.MainForm.Hide;
  // test
  if ( fDelay < 1000 ) then
    fDelay := 1000;
  Sleep ( fDelay );
  // let use pick object iff Handles is null
  if ( Handles = 0 ) then
  begin
    Handles := LetUserPicksWindowHandle ( )
  end;
  //
  // ATTN: 01/12/06 - TEST - we do this later differently in getwindowrect
  // verify object exists
  // if (Handles<>0) then begin
  // dummylp:=GetWindowLong(Handles,GWL_HINSTANCE);
  // if (dummylp=0) then Handles:=0;
  // end;
  // ok we got one to capture?
  if ( Handles = 0 ) then
  begin
    Result := TBitmap.Create;
    DoPixelFormatFix ( Result );
    exit;
  end;
  // get region on window to capture
  iretv := GetClientRect ( Handles, CRect );
  iretv := GetWindowRect ( Handles, WRect );
  if ( iretv = False ) then
  begin
    // we have to exit if we cant find window!
    Result := TBitmap.Create;
    DoPixelFormatFix ( Result );
    exit;
  end;
  // remove cursor temporarily - it can mess up scrolls
  cursorposold := Mouse.CursorPos;
  cursorposnew.X := Screen.DesktopLeft + Screen.DesktopWidth + 10;
  cursorposnew.Y := Screen.DesktopTop + Screen.DesktopHeight + 10;
  Mouse.CursorPos := cursorposnew;
  // ATTN: TEST!!!
  // force it foreground
  SetForegroundWindow ( Handles );
  // another try
  SetFocus ( Handles );
  // moved above
  // // get region on window to capture
  // iretv := GetClientRect(Handles, CRect);
  // iretv := GetWindowRect ( Handles, WRect );
  // if (iretv=FALSE) then begin
  // // we have to exit if we cant find window!
  // Result := TBitmap.Create;
  // DoPixelFormatFix ( Result );
  // Exit;
  // end;
  // ATTN: unsure of this - maybe only makes sense when borders are on
  // CRect.Right:=CRect.Right+1;
  CRect.Bottom := CRect.Bottom - 1;
  // now adjust for scrollbars
  ExtraWidth := ( WRect.Right - WRect.Left ) - ( CRect.Right - CRect.Left );
  ExtraHeight := ( WRect.Bottom - WRect.Top ) - ( CRect.Bottom - CRect.Top );
  // now adjust for capture size
  // we need these values to avoid capturing scrollbars in internet explorer
  std_scrollbarHeight := JR_GetSystemMetrics_YHScroll ( );
  std_scrollbarWidth := JR_GetSystemMetrics_XVScroll ( );
  // noscrolls are a kluge to handle case where there is no scroll bar; so when there is no scrolling we add this to dimensions
  noscroll_hplus := 0;
  noscroll_vplus := 0;
  // ATTN: test 07/02/06
  // scrollmode:=2;
  // SPECIAL HANDLING BASED ON WINDOW CLASS NAME
  // get windows class name
  GetClassName ( Handles, wclassnamebuf, 254 );
  windowclassname := string ( wclassnamebuf );
  // set defaults then get exceptions
  stoponnochange := False;
  x1o := 0;
  y1o := 0;
  x2o := 0;
  y2o := 0;
  customindex := CalcWindowExceptions ( windowclassname, Exceptionstrings, x1o, y1o, x2o, y2o, scrollmode,
    stoponnochange, extras );
  // modify capture based on rules or guess
  if ( customindex = -1 ) then
  begin
    // no exception found in list, use our best guess
    if ( windowclassname = 'Internet Explorer_Server' ) then
    begin
      // ie scrollbar fix - otherwise scrollbar is included in the capture
      // ie strangeness
      // TEST
      ExtraWidth := ExtraWidth + 2;
      ExtraHeight := ExtraHeight + 1;
      Rect := WRect;
      Rect.Left := Rect.Left + 0;
      Rect.Top := Rect.Top + 0;
      Rect.Right := ( Rect.Right - ( std_scrollbarWidth ) ) - ExtraWidth;
      Rect.Bottom := ( Rect.Bottom - std_scrollbarHeight ) - ExtraHeight;
      // noscroll_hplus:=std_scrollbarWidth;
      noscroll_vplus := std_scrollbarHeight;
    end
    else if ( windowclassname = 'MozillaWindowClass' ) then
    begin
      // ie scrollbar fix - otherwise scrollbar is included in the capture
      // we need to use simulated keypress mode for mozilla
      scrollmode := 2;
      Rect := WRect;
      // Rect.Left := Rect.Left+0;
      // Rect.Top := Rect.Top+0;
      // Rect.Right := (Rect.Right - (std_scrollbarWidth))+0;
      // Rect.Bottom := (Rect.Bottom - std_scrollbarHeight)+0;
    end
    else
    begin
      // good for normal (opera, etc)
      Rect := WRect;
      Rect.Left := Rect.Left;
      Rect.Top := Rect.Top;
      Rect.Right := ( Rect.Right - ( ExtraWidth ) ) + 0;
      Rect.Bottom := ( Rect.Bottom - ( ExtraHeight ) ) + 0;
    end;
  end
  else
  begin
    Rect := WRect;
    Rect.Left := Rect.Left + x1o;
    Rect.Top := Rect.Top + y1o;
    Rect.Right := ( Rect.Right - x2o );
    Rect.Bottom := ( Rect.Bottom - y2o );
  end;
  // our selected area
  ViewWidth := Rect.Right - Rect.Left;
  ViewHeight := Rect.Bottom - Rect.Top;
  // save rectangle for a repeat capture?
  fObjectRLeft := Rect.Left;
  fObjectRTop := Rect.Top;
  fObjectRWidth := Rect.Right - Rect.Left;
  fObjectRHeight := Rect.Bottom - Rect.Top;
  // init temp bitmap helpers
  priorbitmap := TBitmap.Create;
  DoPixelFormatFix ( priorbitmap );
  priorbitmap.Width := ViewWidth;
  priorbitmap.Height := ViewHeight;
  DoPixelFormatFix ( priorbitmap );
  // new attempt to inset past left top border when checking
  leftm := 0;
  topm := 0;
  with fBitmap, Rect do
  begin
    // new memory dc+bitmap for temporary rendering during autoscroll
    DC := GetDC ( 0 );
    MemBitmap := CreateCompatibleBitmap ( DC, ViewWidth, ViewHeight );
    ReleaseDC ( 0, DC );
    MemDC := CreateCompatibleDC ( 0 );
    OldBitmap := SelectObject ( MemDC, MemBitmap );
    //
    ScreenDc := GetDC ( 0 );
    // determine last scroll position
    with ScrollInfo do
    begin
      // init the scroll info structure
      ZeroMemory ( @ScrollInfo, SizeOf ( ScrollInfo ) );
      cbSize := SizeOf ( TScrollInfo );
      // fMask := SIF_ALL;
      fMask := SIF_PAGE or SIF_POS or SIF_RANGE or SIF_TRACKPOS;
    end;
    GetScrollInfo ( Handles, SB_HORZ, ScrollInfo );
    LastX := ScrollInfo.nPos;
    GetScrollInfo ( Handles, SB_VERT, ScrollInfo );
    LastY := ScrollInfo.nPos;
    try
      // do we have a palette device? - Thanks to Joe C. Hecht
      if ( GetDeviceCaps ( ScreenDc, RASTERCAPS ) and RC_PALETTE = RC_PALETTE ) then
      begin
        // allocate memory for a logical palette
        GetMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
        // zero it out to be neat
        FillChar ( lpPal^, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ), #0 );
        // fill in the palette version
        lpPal^.palVersion := $300;
        // grab the system palette entries
        lpPal^.palNumEntries := GetSystemPaletteEntries ( ScreenDc, 0, 256, lpPal^.palPalEntry );
        if ( lpPal^.palNumEntries <> 0 ) then
          // create the palette
          fBitmap.Palette := CreatePalette ( lpPal^ );
        FreeMem ( lpPal, SizeOf ( TLOGPALETTE ) + ( 255 * SizeOf ( TPALETTEENTRY ) ) );
      end;
      // ATTN: test 1/10/06
      // maxgoodoffsets:=5;
      // maxtotaloffsets:=10;
      // stoponnochange:=true;
      GlobalSleepingTime := MinGlobalSleepingTime;
      // scrollmode:=2;
      // ---------------------------------------------------------------------------
      // new attempt to figure out smartly iff and how big the scrollbars on the bottom and right are
      // by invalidating client area and seeing where it really ends
      // this is also a good sanity check for if we are going to be able to use this approach
      //
      // force draw
      // ForceWinRectRedraw(Handles,CRect,ScreenDC);
      // ok mark the entire area (CLEARS IT)
      // MarkDc(ScreenDc,Rect.Left, Rect.Top, ViewWidth, ViewHeight,priorbitmap.Canvas.Handle);
      // test
      // InducePartialRepaint(Handles,ScreenDC);
      // Sleep(4000);
      //
      // now grab screen
      // BitBlt ( priorbitmap.Canvas.Handle, 0, 0, ViewWidth, ViewHeight, ScreenDC, Rect.Left, Rect.Top, GetBitBlt_RopMode() );
      // now look for extent
      // vert_offset:=LocateMarkerOffsetVertical(priorbitmap,ViewWidth, ViewHeight, leftm,topm);
      // horz_offset:=LocateMarkerOffsetHorizontal(priorbitmap,ViewWidth, ViewHeight,leftm,topm);
      //
      // ok now the interesting part, we modify view width and height based on where we hit real border
      // ViewWidth:=horz_offset;
      // ViewHeight:=vert_offset;
      // ---------------------------------------------------------------------------
      if ( ViewWidth > leftm ) and ( ViewHeight > topm ) then
      begin
        // ---------------------------------------------------------------------------
        // PHASE 1 we figure out extents
        // figure out extent
        mode_try_attempt := 0;
        max_scrollmodes := 4;
        repeat
          // increment mode_try_attempt
          if ( mode_try_attempt > 0 ) then
          begin
            // ok try to switch scroll modes
            scrollmode := scrollmode + 1;
            if ( scrollmode >= max_scrollmodes ) then
            begin
              // wrap around scroll mode
              scrollmode := 0;
            end;
          end;
          mode_try_attempt := mode_try_attempt + 1;
          // set some max heuristics for scroll captures
          if ( scrollmode = 0 ) then
          begin
            maxgoodoffsets := 50;
            maxtotaloffsets := 200;
          end
          else
          begin
            maxgoodoffsets := 200;
            maxtotaloffsets := 500;
          end;
          if ( scrollmode = 3 ) then
          begin
            GlobalSleepingTime := MaxGlobalSleepingTime;
          end
          else
          begin
            GlobalSleepingTime := MinGlobalSleepingTime;
          end;
          // init
          vert_offsets [ 0 ] := 0;
          horz_offsets [ 0 ] := 0;
          // first vertical extent
          vert_curoffsetindex := 1;
          vert_lastgoodoffsetindex := 0;
          ScrollWindow_FullLeft ( Handles, ScreenDc, scrollmode, True );
          ScrollWindow_FullTop ( Handles, ScreenDc, scrollmode, True );
          // see if we can get scroll position information
          validscrollpos := False;
          GetScrollInfo ( Handles, SB_VERT, ScrollInfo );
          lastscrollpos := ScrollInfo.nPos;
          repeat
            // force draw
            ForceWinRectRedraw ( Handles, CRect, ScreenDc );
            // ok mark the screen
            MarkDc ( ScreenDc, Rect.Left, Rect.Top, ViewWidth, ViewHeight, priorbitmap.Canvas.Handle );
            // now scroll down (page or line, as long as we are consistent
            ScrollWindow_OneDown ( Handles, ScreenDc, scrollmode, True );
            // if we are checking for stop on no visual change
            if ( vert_curoffsetindex > 1 ) and ( stoponnochange = True ) then
            begin
              // grab screen as xor
              BitBlt ( priorbitmap.Canvas.Handle, 0, 0, ViewWidth, ViewHeight, ScreenDc, Rect.Left, Rect.Top,
                SRCINVERT );
              // stop when there is no change
              if ( IsBlankBitmap ( priorbitmap, ViewWidth, ViewHeight ) ) then
              begin
                vert_lastgoodoffsetindex := vert_curoffsetindex - 1;
                Break;
              end;
            end;
            // now grab screen
            BitBlt ( priorbitmap.Canvas.Handle, 0, 0, ViewWidth, ViewHeight, ScreenDc, Rect.Left, Rect.Top,
              GetBitBlt_RopMode ( ) );
            // and locate vertical offset where marker ends
            vert_offset := LocateMarkerOffsetVertical ( priorbitmap, ViewWidth, ViewHeight, leftm, topm );
            if ( vert_offset <= topm ) then
            begin
              // no marker found - it's entire window or error?
              // dont worry for now but if we get too many then we consider it an error
            end
            else if ( vert_offset = ViewHeight ) then
            begin
              // ok no more to scroll we are done we hit end
              Break;
            end
            else
            begin
              // a valid one - remember when last good one we had was
              vert_lastgoodoffsetindex := vert_curoffsetindex;
            end;
            // can we get scroll pos? - backup method for telling when we are at the end
            GetScrollInfo ( Handles, SB_VERT, ScrollInfo );
            if ( lastscrollpos <> ScrollInfo.nPos ) then
            begin
              // ok scroll position has changed, so we can read it
              validscrollpos := True;
              lastscrollpos := ScrollInfo.nPos;
            end
            else if ( validscrollpos = True ) then
            begin
              // ok we are stuck and we got valid numbers, so stop - this could happen if we get a window that never holds marker
              vert_lastgoodoffsetindex := vert_curoffsetindex - 1;
              Break;
            end;
            // store it
            vert_offsets [ vert_curoffsetindex ] := vert_offset;
            // repeat
            if ( vert_curoffsetindex - vert_lastgoodoffsetindex > maxgoodoffsets ) then
            begin
              // too many full windows?
              dummyt := 1;
              Break;
            end;
            // iterate
            vert_curoffsetindex := vert_curoffsetindex + 1;
          until ( vert_curoffsetindex >= maxtotaloffsets );
          // ok at this point, we know how many VALID scrolls we have (vert_lastgoodoffsetindex)
          // and we have an array telling us the vertical offset of marker each time
          // next the horizontal extent
          horz_curoffsetindex := 1;
          horz_lastgoodoffsetindex := 0;
          ScrollWindow_FullLeft ( Handles, ScreenDc, scrollmode, True );
          ScrollWindow_FullTop ( Handles, ScreenDc, scrollmode, True );
          // see if we can get scroll position information
          validscrollpos := False;
          GetScrollInfo ( Handles, SB_HORZ, ScrollInfo );
          lastscrollpos := ScrollInfo.nPos;
          repeat
            // force draw
            ForceWinRectRedraw ( Handles, CRect, ScreenDc );
            // ok mark the screen
            MarkDc ( ScreenDc, Rect.Left, Rect.Top, ViewWidth, ViewHeight, priorbitmap.Canvas.Handle );
            // now scroll down (page or line, as long as we are consistent
            ScrollWindow_OneRight ( Handles, ScreenDc, scrollmode, True );
            // if we are checking for stop on no visual change
            if ( horz_curoffsetindex > 1 ) and ( stoponnochange = True ) then
            begin
              // grab screen as xor
              BitBlt ( priorbitmap.Canvas.Handle, 0, 0, ViewWidth, ViewHeight, ScreenDc, Rect.Left, Rect.Top,
                SRCINVERT );
              // stop when there is no change
              if ( IsBlankBitmap ( priorbitmap, ViewWidth, ViewHeight ) ) then
              begin
                horz_lastgoodoffsetindex := horz_curoffsetindex - 1;
                Break;
              end;
            end;
            // now grab screen
            BitBlt ( priorbitmap.Canvas.Handle, 0, 0, ViewWidth, ViewHeight, ScreenDc, Rect.Left, Rect.Top,
              GetBitBlt_RopMode ( ) );
            // and locate vertical offset where marker ends
            horz_offset := LocateMarkerOffsetHorizontal ( priorbitmap, ViewWidth, ViewHeight, leftm, topm );
            if ( horz_offset <= leftm ) then
            begin
              // no marker found - it's entire window or error?
              // dont worry for now but if we get too many then we consider it an error
            end
            else if ( horz_offset = ViewWidth ) then
            begin
              // ok no more to scroll we are done we hit end
              Break;
            end
            else
            begin
              // a valid one - remember when last good one we had was
              horz_lastgoodoffsetindex := horz_curoffsetindex;
            end;
            // can we get scroll pos? - backup method for telling when we are at the end
            GetScrollInfo ( Handles, SB_HORZ, ScrollInfo );
            if ( lastscrollpos <> ScrollInfo.nPos ) then
            begin
              // ok scroll position has changed, so we can read it
              validscrollpos := True;
              lastscrollpos := ScrollInfo.nPos;
            end
            else if ( validscrollpos = True ) then
            begin
              // ok we are stuck and we got valid numbers, so stop - this could happen if we get a window that never holds marker
              horz_lastgoodoffsetindex := horz_curoffsetindex - 1;
              Break;
            end;
            // store it
            horz_offsets [ horz_curoffsetindex ] := horz_offset;
            // repeat
            if ( horz_curoffsetindex - horz_lastgoodoffsetindex > maxgoodoffsets ) then
            begin
              // too many full windows?
              Break;
            end;
            // iterate
            horz_curoffsetindex := horz_curoffsetindex + 1;
          until ( horz_curoffsetindex >= maxtotaloffsets );
          // ok at this point, we know how many VALID scrolls we have (horz_lastgoodoffsetindex)
          // and we have an array telling us the horizontal offset of marker each time
          // ok NOW NEW thing we do is, IFF we couldnt find any scrolling area at all, assume the scroll method didnt work and try the other
        until ( ( horz_lastgoodoffsetindex > 0 ) or ( vert_lastgoodoffsetindex > 0 ) or
          ( mode_try_attempt >= max_scrollmodes ) );
        // ---------------------------------------------------------------------------
        // go back to min sleep
        GlobalSleepingTime := MinGlobalSleepingTime;
        // PHASE 2 - calculate final size
        vert_curoffsetindex := 0;
        // horz_curoffsetindex := 0;
        CY := 0;
        repeat
          // scroll to left border
          horz_curoffsetindex := 0;
          CX := 0;
          repeat
            CapOffsetX := horz_offsets [ horz_curoffsetindex ];
            CapOffsetY := vert_offsets [ vert_curoffsetindex ];
            CapW := ViewWidth - CapOffsetX;
            CapH := ViewHeight - CapOffsetY;
            // and advance
            CX := CX + CapW;
            horz_curoffsetindex := horz_curoffsetindex + 1;
            // until end
          until ( horz_curoffsetindex > horz_lastgoodoffsetindex );
          // advance target loci
          CY := CY + CapH;
          // and advance
          vert_curoffsetindex := vert_curoffsetindex + 1;
          // until end
        until ( vert_curoffsetindex > vert_lastgoodoffsetindex );
        // too big?
        biggestsize := 1920 * 1200;
        // 30 giant pages is our heuristic for how big we will allow it to get
        biggestsize := biggestsize * 30;
        if ( CX * CY > biggestsize ) then
        begin
          // just cap 1 screens worth
          CX := ViewWidth;
          CY := ViewHeight;
          vert_lastgoodoffsetindex := 0;
          horz_lastgoodoffsetindex := 0;
          horz_offsets [ 0 ] := 0;
          vert_offsets [ 0 ] := 0;
        end;
        // noscrolls are a kluge to handle case where there is no scroll bar; so when there is no scrolling we add this to dimensions
        if ( vert_lastgoodoffsetindex <= 0 ) then
        begin
          CX := CX + noscroll_hplus;
        end;
        if ( horz_lastgoodoffsetindex <= 0 ) then
        begin
          CY := CY + noscroll_vplus;
        end;
        // set size
        FinalWidth := CX;
        FinalHeight := CY;
        fBitmap.Width := FinalWidth;
        fBitmap.Height := FinalHeight;
        // PHASE 3 - we capture
        ScrollWindow_FullTop ( Handles, ScreenDc, scrollmode, True );
        CY := 0;
        repeat
          // scroll to left border
          horz_curoffsetindex := 0;
          CX := 0;
          ScrollWindow_FullLeft ( Handles, ScreenDc, scrollmode, True );
          repeat
            ForceWinRectRedraw ( Handles, CRect, ScreenDc );
            CapOffsetX := horz_offsets [ horz_curoffsetindex ];
            CapOffsetY := vert_offsets [ vert_curoffsetindex ];
            CapW := ViewWidth - CapOffsetX;
            CapH := ViewHeight - CapOffsetY;
            // noscrolls are a kluge to handle case where there is no scroll bar; so when there is no scrolling we add this to dimensions
            if ( vert_lastgoodoffsetindex = 0 ) and ( horz_curoffsetindex = horz_lastgoodoffsetindex ) then
            begin
              CapW := CapW + noscroll_hplus;
            end;
            if ( horz_lastgoodoffsetindex = 0 ) and ( vert_curoffsetindex = vert_lastgoodoffsetindex ) then
            begin
              CapH := CapH + noscroll_vplus;
            end;
            // copy to helper
            BitBlt ( priorbitmap.Canvas.Handle, 0, 0, CapW, CapH, ScreenDc, Rect.Left + CapOffsetX,
              Rect.Top + CapOffsetY, GetBitBlt_RopMode ( ) );
            // now into our target bitmap
            BitBlt ( fBitmap.Canvas.Handle, CX, CY, CapW, CapH, priorbitmap.Canvas.Handle, 0, 0, GetBitBlt_RopMode ( ) );
            // now advance target loci
            CX := CX + CapW;
            // and scroll
            ScrollWindow_OneRight ( Handles, ScreenDc, scrollmode, True );
            // and advance
            horz_curoffsetindex := horz_curoffsetindex + 1;
            // until end
          until ( horz_curoffsetindex > horz_lastgoodoffsetindex );
          // scroll vert
          ScrollWindow_OneDown ( Handles, ScreenDc, scrollmode, True );
          // advance target loci
          CY := CY + CapH;
          // and advance
          vert_curoffsetindex := vert_curoffsetindex + 1;
          // until end
        until ( vert_curoffsetindex > vert_lastgoodoffsetindex );
        // finally scroll to the previous position before we started
        ScrollWindow_FullLeft ( Handles, ScreenDc, scrollmode, True );
        ScrollWindow_FullTop ( Handles, ScreenDc, scrollmode, True );
        SendMessage ( Handles, WM_HSCROLL, MakeWParam ( SB_THUMBPOSITION, LastX ), 0 );
        SendMessage ( Handles, WM_VSCROLL, MakeWParam ( SB_THUMBPOSITION, LastY ), 0 );
        // ---------------------------------------------------------------------------
        // copy bitmap to function result
        Result := TBitmap.Create;
        DoPixelFormatFix ( Result );
        Result.Width := FinalWidth;
        Result.Height := FinalHeight;
        Result.Assign ( fBitmap );
        // to clipboard and event
        if fAutomatic then
          CopyToClipboard;
        if Assigned ( fOnAfterCapture ) then
          fOnAfterCapture ( Self );
        // end of if good capture size test
      end;
    finally
      // release main dc
      ReleaseDC ( 0, ScreenDc );
      // release new autoscroll stuff
      SelectObject ( MemDC, OldBitmap );
      DeleteDC ( MemDC );
      DeleteObject ( MemBitmap );
      // clear temp bitmaps
      priorbitmap.Free ( );
    end;
    Inc ( fCaptureCount );
    CaptureCount := fCaptureCount;
    // Restore mainform to original state
    if fMinimized then
      Application.MainForm.Show;
    // replace mouse
    Mouse.CursorPos := cursorposold;
  end;
end;

function TASGScreenCapture.CaptureObjectByTControlp_AutoScroll ( const tcontrolp: TControl; scrollmode: integer;
  Exceptionstrings: string = '' ): TBitmap;
// Mouser, 1/5/06  - Capture Specific Object by tcontrol - mouser
var
  Handles: HWND;
  tp: TPoint;
begin
  tp.X := 0;
  tp.Y := 0;
  tp := tcontrolp.ClientToScreen ( tp );
  Handles := WindowFromPoint ( tp );
  Result := CaptureObjectByHWND_AutoScroll ( Handles, scrollmode, Exceptionstrings );
end;

function TASGScreenCapture.CalcWindowExceptions ( windowclassname: string; Exceptionstrings: string; var x1o: integer;
  var y1o: integer; var x2o: integer; var y2o: integer; var scrollmode: integer; var stoponnochange: boolean;
  var extras: string ): integer;
// check the string for any stored exceptions
var
  std_scrollbarHeight: integer;
  std_scrollbarWidth: integer;
  ts: TStringList;
  strcomps: TStringList;
  i, i2: integer;
  astr, asubstr: string;
begin
  // default answer not found is -1
  Result := -1;
  // Calculate scrollbar dimensions
  // ATTN: unfinished
  std_scrollbarHeight := GetSystemMetrics ( SM_CYHSCROLL ); // 16;
  std_scrollbarWidth := GetSystemMetrics ( SM_CXVSCROLL ); // 16;
  // allocate
  ts := TStringList.Create;
  strcomps := TStringList.Create;
  // set stringlist for parsing
  ts.Text := Exceptionstrings;
  // search -try to find windows class name in our list
  for i := 0 to ( ts.Count - 1 ) do
  begin
    // grab string entry
    astr := ts.Strings [ i ];
    // parse it to components
    ParseDelimited ( strcomps, astr, ';' );
    if ( strcomps.Count >= 5 ) then
    begin
      if ( strcomps.Strings [ 0 ] = windowclassname ) then
      begin
        // ok we got a match for this window class
        // set offsets
        if ( strcomps.Strings [ 1 ] = 's' ) then
          x1o := std_scrollbarWidth
        else
          x1o := StrToInt ( strcomps.Strings [ 1 ] );
        if ( strcomps.Strings [ 2 ] = 's' ) then
          y1o := std_scrollbarHeight
        else
          y1o := StrToInt ( strcomps.Strings [ 2 ] );
        if ( strcomps.Strings [ 3 ] = 's' ) then
          x2o := std_scrollbarWidth
        else
          x2o := StrToInt ( strcomps.Strings [ 3 ] );
        if ( strcomps.Strings [ 4 ] = 's' ) then
          y2o := std_scrollbarHeight
        else
          y2o := StrToInt ( strcomps.Strings [ 4 ] );
        // extras
        if ( strcomps.Count > 5 ) then
        begin
          // fill extras
          extras := strcomps.Strings [ 5 ];
          for i2 := 6 to ( strcomps.Count - 1 ) do
          begin
            asubstr := strcomps.Strings [ i2 ];
            extras := extras + ';' + asubstr;
          end;
          // some we can parse
          if ( Pos ( 'bylines', extras ) > 0 ) then
            scrollmode := 1;
          if ( Pos ( 'sendkeys', extras ) > 0 ) then
            scrollmode := 2;
          if ( Pos ( 'blankstop', extras ) > 0 ) then
            stoponnochange := True;
          if ( Pos ( 'debug', extras ) > 0 ) then
            GlobalSleepingTime := 1000;
        end;
        // set result index
        Result := i;
        Break;
      end;
    end;
  end;
  // deallocate
  ts.Free;
  strcomps.Free;
end;

procedure TASGScreenCapture.ParseDelimited ( const sl: TStrings; const Value: string; const delimiter: string );
// from http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm - Zarko Gajic
var
  dx: integer;
  ns: string;
  txt: string;
  delta: integer;
begin
  delta := Length ( delimiter );
  txt := Value + delimiter;
  sl.BeginUpdate;
  sl.Clear;
  try
    while Length ( txt ) > 0 do
    begin
      dx := Pos ( delimiter, txt );
      ns := Copy ( txt, 0, dx - 1 );
      sl.Add ( ns );
      txt := Copy ( txt, dx + delta, Maxint );
    end;
  finally
    sl.EndUpdate;
  end;
end;

procedure TASGScreenCapture.ResetCaptureCount;
begin
  fCaptureCount := 0;
  CaptureCount := fCaptureCount;
end;

function TASGScreenCapture.MarkDc ( ScreenDc: hDC; Left: integer; Top: integer; CapW: integer; CapH: integer;
  Canvas: HWND ): integer;
begin
  // for now we just set it all black, later we can be more sophisticated
  BitBlt ( ScreenDc, Left, Top, CapW, CapH, Canvas, 0, 0, BLACKNESS );
  Result := 1;
end;

function TASGScreenCapture.LocateMarkerOffsetVertical ( abitmap: TBitmap; CapW: integer; CapH: integer; Left: integer;
  Top: integer ): integer;
var
  X, Y: integer;
  C: TColor;
  aCanvas: TCanvas;
  Right, Bottom: integer;
begin
  // ok we want to walk through abitmap from top down and find where mark ends
  aCanvas := abitmap.Canvas;
  Right := CapW;
  Bottom := CapH;
  if ( CapW > 110 ) then
  begin
    Left := 50;
    Right := CapW - 50;
  end
  else if ( CapW > 50 ) then
  begin
    Left := 20;
    Right := CapW - 20;
  end;
  for Y := Top to Bottom - 1 do
  begin
    for X := Left to Right - 1 do
    begin
      // get pixel color
      C := aCanvas.Pixels [ X, Y ];
      if ( C <> clBlack ) then
      begin
        Result := Y;
        exit;
      end;
    end;
  end;
  // not found
  Result := CapH;
end;

function TASGScreenCapture.LocateMarkerOffsetHorizontal ( abitmap: TBitmap; CapW: integer; CapH: integer; Left: integer;
  Top: integer ): integer;
var
  X, Y: integer;
  C: TColor;
  aCanvas: TCanvas;
  Right, Bottom: integer;
begin
  // ok we want to walk through abitmap from left to right and find where mark ends
  aCanvas := abitmap.Canvas;
  Right := CapW;
  Bottom := CapH;
  if ( CapH > 110 ) then
  begin
    Top := 50;
    Bottom := CapH - 50;
  end
  else if ( CapH > 50 ) then
  begin
    Top := 20;
    Bottom := CapH - 20;
  end;
  for X := Left to Right - 1 do
  begin
    for Y := Top to Bottom - 1 do
    begin
      // get pixel color
      C := aCanvas.Pixels [ X, Y ];
      if ( C <> clBlack ) then
      begin
        Result := X;
        exit;
      end;
    end;
  end;
  // not found
  Result := CapW;
end;

function TASGScreenCapture.IsBlankBitmap ( abitmap: TBitmap; CapW: integer; CapH: integer ): boolean;
var
  X, Y: integer;
  C: TColor;
  aCanvas: TCanvas;
  Left, Right, Top, Bottom: integer;
begin
  // ok we want to walk through abitmap from left to right and find where its not blank
  aCanvas := abitmap.Canvas;
  Left := 0;
  Right := CapW;
  Top := 0;
  Bottom := CapH;
  for X := Left to Right - 1 do
  begin
    for Y := Top to Bottom - 1 do
    begin
      // get pixel color
      C := aCanvas.Pixels [ X, Y ];
      if ( C <> 0 ) then
      begin
        Result := False;
        exit;
      end;
    end;
  end;
  Result := True;
  exit;
end;

procedure TASGScreenCapture.ScrollWindow_FullLeft ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
var
  i: integer;
begin
  // some dont listen to SB_LEFT? (opera)
  for i := 1 to 100 do
    ScrollWindow_OneLeft ( Handles, DrawDC, scrollmode, False );
  SendMessage ( Handles, WM_HSCROLL, SB_LEFT, 0 );
end;

procedure TASGScreenCapture.ScrollWindow_FullTop ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
var
  i: integer;
begin
  // some dont listen to SB_TOP? (opera)
  for i := 1 to 100 do
    ScrollWindow_OneUp ( Handles, DrawDC, scrollmode, False );
  SendMessage ( Handles, WM_VSCROLL, SB_TOP, 0 );
  //
  if ( scrollmode = 2 ) then
  begin
    SendMessage ( Handles, WM_KeyDown, VK_Home, 0 );
  end
  else if ( scrollmode = 3 ) then
  begin
    SimulateKeyDown ( VK_Home );
    SimulateKeyUp ( VK_Home );
  end;
end;

procedure TASGScreenCapture.ScrollWindow_OneDown ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
begin
  if ( scrollmode = 0 ) then
    SendMessage ( Handles, WM_VSCROLL, SB_PAGEDOWN, 0 )
  else if ( scrollmode = 1 ) then
    SendMessage ( Handles, WM_VSCROLL, SB_LINEDOWN, 0 )
  else if ( scrollmode = 2 ) then
  begin
    SendMessage ( Handles, WM_KeyDown, VK_NEXT, 0 );
    SendMessage ( Handles, WM_KeyUP, VK_NEXT, 0 );
    // SimulateKeyDown(VK_NEXT);
    // SimulateKeyUp(VK_NEXT);
  end
  else
  begin
    SimulateMouseEvent ( VK_NEXT );
    // SimulateKeyDown(VK_NEXT);
    // SimulateKeyUp(VK_NEXT);
  end;
  // try to induce app to do a PARTIAL repaint
  if ( dorepaint = True ) then
    InducePartialRepaint ( Handles, DrawDC );
end;

procedure TASGScreenCapture.ScrollWindow_OneRight ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
begin
  if ( scrollmode = 0 ) then
    SendMessage ( Handles, WM_HSCROLL, SB_PAGERIGHT, 0 )
  else if ( scrollmode = 1 ) then
    SendMessage ( Handles, WM_HSCROLL, SB_LINERIGHT, 0 )
  else if ( scrollmode = 2 ) then
  begin
    SendMessage ( Handles, WM_KeyDown, VK_Right, 0 );
    SendMessage ( Handles, WM_KeyUP, VK_Right, 0 );
    // SimulateKeyDown(VK_Right);
    // SimulateKeyUp(VK_Right);
  end
  else
  begin
    // SimulateKeyDown(VK_Right);
    // SimulateKeyUp(VK_Right);
    exit;
  end;
  // try to induce app to do a PARTIAL repaint
  if ( dorepaint = True ) then
    InducePartialRepaint ( Handles, DrawDC );
end;

procedure TASGScreenCapture.ScrollWindow_OneUp ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
begin
  if ( scrollmode = 0 ) then
    SendMessage ( Handles, WM_VSCROLL, SB_PAGEUP, 0 )
  else if ( scrollmode = 1 ) then
    SendMessage ( Handles, WM_VSCROLL, SB_LINEUP, 0 )
  else if ( scrollmode = 2 ) then
  begin
    SendMessage ( Handles, WM_KeyDown, VK_PRIOR, 0 );
    SendMessage ( Handles, WM_KeyUP, VK_PRIOR, 0 );
    // SimulateKeyDown(VK_PRIOR);
    // SimulateKeyUp(VK_PRIOR);
  end
  else
  begin
    SimulateMouseEvent ( VK_PRIOR );
  end;
  // try to induce app to do a PARTIAL repaint
  if ( dorepaint = True ) then
    InducePartialRepaint ( Handles, DrawDC );
end;

procedure TASGScreenCapture.ScrollWindow_OneLeft ( Handles: integer; DrawDC: hDC; scrollmode: integer;
  dorepaint: boolean );
begin
  if ( scrollmode = 0 ) then
    SendMessage ( Handles, WM_HSCROLL, SB_PAGELEFT, 0 )
  else if ( scrollmode = 1 ) then
    SendMessage ( Handles, WM_HSCROLL, SB_LINELEFT, 0 )
  else if ( scrollmode = 2 ) then
  begin
    SendMessage ( Handles, WM_KeyDown, VK_Left, 0 );
    SendMessage ( Handles, WM_KeyUP, VK_Left, 0 );
    // SimulateKeyDown(VK_Left);
    // SimulateKeyUp(VK_Left);
  end
  else
  begin
    // SimulateKeyDown(VK_Left);
    // SimulateKeyUp(VK_Left);
    exit;
  end;
  // try to induce app to do a PARTIAL repaint
  if ( dorepaint = True ) then
    InducePartialRepaint ( Handles, DrawDC );
end;

procedure TASGScreenCapture.InducePartialRepaint ( Handles: integer; DrawDC: hDC );
begin
  // try to induce app to repaint
  // Sleep(1000);
  // Application.ProcessMessages();
  // SendMessage(Handles, WM_PAINT, DrawDC, 0);
  UpdateWindow ( Handles );
  // ATTN: test
  // RedrawWindow(Handles,0,0,RDW_UPDATENOW);
  Application.ProcessMessages ( );
  Sleep ( GlobalSleepingTime );
  // Sleep(20);
end;

procedure TASGScreenCapture.ForceWinRectRedraw ( Handles: integer; CRect: TRect; DrawDC: hDC );
begin
  // CRect.Left:=CRect.Left-2;
  // CRect.Right:=CRect.Right+2;
  // CRect.Top:=CRect.Top-2;
  // CRect.Bottom:=CRect.Bottom+2;
  // RedrawWindow(Handles,@CRect,0,RDW_INVALIDATE OR RDW_ERASE OR RDW_UPDATENOW OR RDW_ALLCHILDREN);
  // Application.ProcessMessages();
  // Sleep(1000);
  RedrawWindow ( Handles, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or RDW_UPDATENOW or RDW_ALLCHILDREN );
  Application.ProcessMessages ( );
  Sleep ( GlobalSleepingTime );
end;

function TASGScreenCapture.LetUserPicksWindowHandle ( ): integer;
var
  Handles: integer;
  P1: TPoint;
begin
  // let user interactively pick which windows object to capture and return the handle to it, or 0 if aborted
  Handles := 0;
  // ATTN: 06/18/05 - Force monitor (copied from capture rect procedure
  CaptureObjectHighlight.ForceMonitorNum := fMonitorNum;
  // Thornsoft - capture from specific monitor
  CaptureObjectHighlightForm := TCaptureObjectHighlightForm.Create ( Application );
  CaptureObjectForm := TCaptureObjectForm.Create ( Application );
  try
    if CaptureObjectForm.ShowModal = mrOk then
    begin
      // Get cursor position
      GetCursorPos ( P1 );
      Handles := WindowFromPoint ( P1 );
    end;
  finally
    CaptureObjectForm.Free;
    CaptureObjectHighlightForm.Free;
  end;
  // return result
  Result := Handles;
end;

procedure TASGScreenCapture.SimulateKeyDown ( MyKey: cardinal );
var
  MyInput: tagINPUT;
begin
  MyInput.Itype := INPUT_KEYBOARD;
  MyInput.ki.wVk := MyKey;
  MyInput.ki.wScan := MapVirtualKey ( MyKey, 0 );
  if ( MyKey = VK_UP ) or ( MyKey = VK_DOWN ) or ( MyKey = VK_Left ) or ( MyKey = VK_Right ) or ( MyKey = VK_Home ) or
    ( MyKey = VK_END ) or ( MyKey = VK_PRIOR ) or ( MyKey = VK_NEXT ) or ( MyKey = VK_INSERT ) or
    ( MyKey = VK_DELETE ) then
    MyInput.ki.dwFlags := KEYEVENTF_EXTENDEDKEY
  else
    MyInput.ki.dwFlags := 0;
  MyInput.ki.time := 0;
  MyInput.ki.dwExtraInfo := 0;
  SendInput ( 1, MyInput, SizeOf ( MyInput ) );
end;

procedure TASGScreenCapture.SimulateKeyUp ( MyKey: cardinal );
var
  MyInput: tagINPUT;
begin
  MyInput.Itype := INPUT_KEYBOARD;
  MyInput.ki.wVk := MyKey;
  MyInput.ki.wScan := MapVirtualKey ( MyKey, 0 );
  if ( MyKey = VK_UP ) or ( MyKey = VK_DOWN ) or ( MyKey = VK_Left ) or ( MyKey = VK_Right ) or ( MyKey = VK_Home ) or
    ( MyKey = VK_END ) or ( MyKey = VK_PRIOR ) or ( MyKey = VK_NEXT ) or ( MyKey = VK_INSERT ) or
    ( MyKey = VK_DELETE ) then
    MyInput.ki.dwFlags := KEYEVENTF_KEYUP or KEYEVENTF_EXTENDEDKEY
  else
    MyInput.ki.dwFlags := KEYEVENTF_KEYUP;
  MyInput.ki.time := 0;
  MyInput.ki.dwExtraInfo := 0;
  SendInput ( 1, MyInput, SizeOf ( MyInput ) );
end;

procedure TASGScreenCapture.SimulateMouseEvent ( MyKey: cardinal );
var
  MyInput: tagINPUT;
begin
  MyInput.Itype := INPUT_MOUSE;
  MyInput.mi.dwFlags := MOUSEEVENTF_WHEEL;
  if ( MyKey = VK_NEXT ) then
    MyInput.mi.mouseData := DWord ( -120 );
  if ( MyKey = VK_PRIOR ) then
    MyInput.mi.mouseData := DWord ( 120 );
  MyInput.mi.time := 0;
  MyInput.mi.dwExtraInfo := 0;
  SendInput ( 1, MyInput, SizeOf ( MyInput ) );
end;

function TASGScreenCapture.JR_GetSystemMetrics_YHScroll ( ): integer;
begin
  Result := GetSystemMetrics ( SM_CYHSCROLL );
end;

function TASGScreenCapture.JR_GetSystemMetrics_XVScroll ( ): integer;
begin
  Result := GetSystemMetrics ( SM_CXVSCROLL );
end;

procedure Register;
begin
  RegisterComponents ( 'ASG', [ TASGScreenCapture ] );
end;

end.

