// ------------------------------------------------------------------------------
// DPF.iOS.GLKView Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.GLKView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,
  System.TypInfo,

  DPF.iOS.BaseControl,
  DPF.iOS.NSTimer,
{$IFDEF IOS}
  iOSapi.GLKit,
  iOSapi.OpenGLES,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  iOSapi.QuartzCore,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls;

type

  TDPFGLKViewOnDrawRect = procedure( Sender: TObject; Rect: DPFNSRect ) of object;
  TDPFGLKView           = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFBaseGLKView = interface( GLKView )
    ['{6FE239D4-CC58-485C-9BBC-409AD6AFA29A}']
    procedure drawRect( rect: CGRect ); cdecl; overload;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
  end;

  TDPFBaseGLKView = class( TOCLocal )
  private
    FDPFGLKView: TDPFGLKView;
  protected
  public
    constructor Create( ADPFGLKView: TDPFGLKView );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure drawRect( rect: CGRect ); overload; cdecl;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
  end;

{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFGLKView = class( TDPFiOSBaseControl )
  private
    FBackgroundColor: TAlphaColor;
    FBackgroundImage: string;
    FOnClick        : TDPFOnClicked;
    FOnDrawRect     : TDPFGLKViewOnDrawRect;
    FEnableRender   : Boolean;

    FDPFNSTimer    : TDPFNSTimer;
    FRenderTimer   : Longword;
    FTouchesBegan  : TDPFTouchesBegan;
    FOnDoubleClick : TDPFOnDoubleClicked;
    FTouchesEnded  : TDPFTouchesEnded;
    FTouchesMoved  : TDPFTouchesMoved;
    FDoubleTapDelay: integer;
    FTapDelay      : integer;

    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetBackgroundImage( const Value: string );
    procedure SetEnableRender( const Value: Boolean );
    procedure SetRenderTimer( const Value: Longword );
{$IFDEF IOS}
    procedure RenderGL( Sender: TObject );
{$ENDIF}
  protected
{$IFDEF IOS}
    FGLKView      : GLKView;
    FEAGLContext  : EAGLContext;
    DPFBaseGLKView: TDPFBaseGLKView;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF IOS}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Lightsteelblue;
    property BackgroundImage: string read FBackgroundImage write SetBackgroundImage;

    property OnClick      : TDPFOnClicked read FOnClick write FOnClick;
    property OnDoubleClick: TDPFOnDoubleClicked read FOnDoubleClick write FOnDoubleClick;

    property OnTouchesEnded: TDPFTouchesEnded read FTouchesEnded write FTouchesEnded;
    property OnTouchesBegan: TDPFTouchesBegan read FTouchesBegan write FTouchesBegan;
    property OnTouchesMoved: TDPFTouchesMoved read FTouchesMoved write FTouchesMoved;

    property OnDrawRect: TDPFGLKViewOnDrawRect read FOnDrawRect write FOnDrawRect;

    property RenderEnabled: Boolean read FEnableRender write SetEnableRender default false;
    property RenderTimer  : Longword read FRenderTimer write SetRenderTimer default 25;

    property TapDelay      : integer read FTapDelay write FTapDelay default 1;
    property DoubleTapDelay: integer read FDoubleTapDelay write FDoubleTapDelay default 2;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFGLKView }
constructor TDPFGLKView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'GLKView';
  FBackgroundColor := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FEnableRender    := false;
  FRenderTimer     := 25;
  FTapDelay        := 1;
  FDoubleTapDelay  := 2;

{$IFDEF IOS}
  // FGLKViewDelegate := TGLKViewDelegate.Create( self );
  FEAGLContext := TEAGLContext.Wrap( TEAGLContext.Alloc.initWithAPI( kEAGLRenderingAPIOpenGLES2 ) );
  if TOSVersion.Major >= 5 then
  begin
    DPFBaseGLKView := TDPFBaseGLKView.Create( self );
    FGLKView       := TGLKView.Wrap( DPFBaseGLKView.Super.init );
  end
  else
    FGLKView := TGLKView.Create;
  // FGLKView.setDelegate( FGLKViewDelegate.GetObjectID );
  FUIControl := FGLKView;

  FDPFNSTimer          := TDPFNSTimer.Create( nil );
  FDPFNSTimer.Enabled  := false;
  FDPFNSTimer.OnTimer  := RenderGL;
  FDPFNSTimer.Interval := FRenderTimer;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFGLKView.Destroy;
begin
{$IFDEF IOS}
  FEAGLContext.release;
  FDPFNSTimer.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFGLKView.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  FGLKView.setHidden( not Visible );

  if FBackgroundImage <> '' then
    SetBackgroundImage( FBackgroundImage )
  else
    SetBackgroundColor( FBackgroundColor );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGLKView.Resize;
begin
{$IFDEF IOS}
  if FGLKView <> nil then
  begin
    FGLKView.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
    // SetBackgroundImage( FBackgroundImage );
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFGLKView.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFGLKView.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGLKView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FGLKView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FGLKView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FGLKView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFGLKView.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Image2: UIImage;
  Image1: UIImage;
{$IFNDEF IOSDEVICE}
  transform: CGAffineTransform;
  context  : CGContextRef;
{$ENDIF}
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if Assigned( FGLKView ) then
  begin
    if FBackgroundImage <> '' then
    begin
      Image1 := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FBackgroundImage ) ) );
      UIGraphicsBeginImageContext( FGLKView.frame.size );
{$IFNDEF IOSDEVICE}
      // ----------------------------------------------------------
      // Mirror on Simulator
      context   := UIGraphicsGetCurrentContext( );
      transform := CGAffineTransformMakeTranslation( 0.0, Height );
      transform := CGAffineTransformScale( transform, 1.0, -1.0 );
      CGContextConcatCTM( context, transform );
{$ENDIF}
      Image1.drawInRect( FGLKView.bounds );
      Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
      UIGraphicsEndImageContext( );

      FGLKView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image2 ) ) );
    end
    else
      SetBackgroundColor( FBackgroundColor );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFGLKView.SetRenderTimer( const Value: Longword );
begin
  FRenderTimer := Value;
  if Assigned( FDPFNSTimer ) then
  begin
    FDPFNSTimer.Interval := FRenderTimer;
    if FDPFNSTimer.Enabled then
    begin
      FDPFNSTimer.Enabled := false;
      FDPFNSTimer.Enabled := true;
    end;

  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFGLKView.SetEnableRender( const Value: Boolean );
begin
  FEnableRender := Value;
  if Assigned( FDPFNSTimer ) then
  begin
    FDPFNSTimer.Interval := FRenderTimer;
    FDPFNSTimer.Enabled  := value;
  end;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFGLKView.RenderGL( Sender: TObject );
begin
  if Assigned( FGLKView ) then
    FGLKView.display;
end;

// ------------------------------------------------------------------------------
{ TDPFBaseGLKView }
constructor TDPFBaseGLKView.Create( ADPFGLKView: TDPFGLKView );
var
  V: Pointer;
begin
  inherited create;
  FDPFGLKView := ADPFGLKView;
  V           := GLKView( Super ).initWithFrame( CGRectMake( 0, 0, 0, 0 ), FDPFGLKView.FEAGLContext );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFBaseGLKView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFBaseGLKView );
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.drawRect( rect: CGRect );
begin
  GLKView( Super ).drawRect( rect );
  if assigned( FDPFGLKView.FOnDrawRect ) then
    FDPFGLKView.FOnDrawRect( FDPFGLKView, DPFNSRect( rect ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesBegan( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFGLKView.FGLKView );
  PrevP := Touch.previousLocationInView( FDPFGLKView.FGLKView );
  if Assigned( FDPFGLKView.FTouchesBegan ) then
    FDPFGLKView.FTouchesBegan( FDPFGLKView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesMoved( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFGLKView.FGLKView );
  PrevP := Touch.previousLocationInView( FDPFGLKView.FGLKView );
  if Assigned( FDPFGLKView.FTouchesMoved ) then
    FDPFGLKView.FTouchesMoved( FDPFGLKView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesEnded( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFGLKView.FGLKView );
  PrevP := Touch.previousLocationInView( FDPFGLKView.FGLKView );
  if Assigned( FDPFGLKView.FTouchesEnded ) then
    FDPFGLKView.FTouchesEnded( FDPFGLKView, Touch.tapCount, DPFNSPoint( P ), DPFNSPoint( PrevP ) );

  if Touch.tapCount = 1 then
    NSObject( self.Super ).performSelector( sel_getUid( 'singleTap:' ), nil, FDPFGLKView.TapDelay / 1000 )
  else if Touch.tapCount > 1 then
  begin
    iOSapi.{$IFDEF DELPHIXE7}Foundation{$ELSE}CocoaTypes{$ENDIF}.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget( Self.GetObjectID );
    NSObject( self.Super ).performSelector( sel_getUid( 'doubleTap:' ), nil, FDPFGLKView.DoubleTapDelay / 1000 );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.singleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFGLKView.FOnClick ) then
    FDPFGLKView.FOnClick( FDPFGLKView );
end;

// ------------------------------------------------------------------------------
procedure TDPFBaseGLKView.doubleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFGLKView.FOnDoubleClick ) then
    FDPFGLKView.FOnDoubleClick( FDPFGLKView );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
