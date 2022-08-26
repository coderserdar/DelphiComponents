// ------------------------------------------------------------------------------
// DPF.iOS.UIView Component
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
unit DPF.iOS.UIView;

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
{$IFDEF IOS}
  iOSapi.GLKit,
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
  DPF.iOS.Classes,
{$ENDIF}
  FMX.Forms,
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFUIView = class;
  // TDPFUIViewClass = class of TDPFUIView;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------

  (*
    IGestureRecognizerDelegate = interface( IObjectiveC )
    ['{E24404F9-DBAD-43E6-8371-9D13EFB25BA2}']
    function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch ): Boolean; cdecl; overload;
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
    end;

    TDPFViewGestureRecognizerDelegate = class( TOCLocal, IGestureRecognizerDelegate )
    private
    FDPFUIView: TDPFUIView;
    public
    constructor Create( ADPFUIView: TDPFUIView );

    function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch ): Boolean; overload; cdecl;
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
    end;
  *)

  DPFView = interface( UIView )
    ['{C45D4CB7-7127-49F2-84FA-7E0F480F99AE}']
    procedure drawRect( rect: CGRect ); cdecl;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;

    procedure remoteControlReceivedWithEvent( event: DPF.iOS.Classes.UIEvent ); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
  end;

  TDPFView = class( TOCLocal )
  private
  protected
    FDPFUIView: TDPFUIView;
  public
    constructor Create( ADPFUIView: TDPFUIView );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure drawRect( rect: CGRect ); cdecl;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;

    procedure remoteControlReceivedWithEvent( event: UIEvent ); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    destructor Destroy; override;
  end;
{$ENDIF}

  TDPFViewOnDrawRect          = procedure( Sender: TObject; Rect: DPFNSRect ) of object;
  TDPFTouchesBegan            = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesMoved            = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesEnded            = procedure( Sender: TObject; const TapCount: Integer; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFOnremoteControlReceived = procedure( Sender: TObject; const EventType: NativeUInt; const EventSubType: NativeUInt ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIView = class( TDPFiOSBaseControl )
  private
    FEmbFrame               : TFrame;
    FBackgroundColor        : TAlphaColor;
    FForm                   : TForm;
    FBackgroundImage        : string;
    FOnClick                : TDPFOnClicked;
    FDelaysTouchesBegan     : Boolean;
    FDelaysTouchesEnded     : Boolean;
    FOnFormChanging         : TDPFFormChanging;
    FOnFormChanged          : TDPFFormChanged;
    FFrame                  : TDPFFrame;
    FOnFrameChanged         : TDPFFrameChanged;
    FOnFrameChanging        : TDPFFrameChanging;
    FCheckReceiveTouch      : Boolean;
    FOnDrawRect             : TDPFViewOnDrawRect;
    FTouchesEnded           : TDPFTouchesEnded;
    FOnDoubleClick          : TDPFOnDoubleClicked;
    FTouchesBegan           : TDPFTouchesBegan;
    FTouchesMoved           : TDPFTouchesMoved;
    FOnRemoteControlReceived: TDPFOnRemoteControlReceived;
    FCanBecomeFirstResponder: Boolean;
    FDoubleTapDelay         : integer;
    FTapDelay               : integer;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetBackgroundImage( const Value: string );
    procedure SetForm( const Value: TForm );
    procedure SetFrame( const Value: TDPFFrame );
    // procedure SetOnClick( const Value: TDPFOnClicked );

  protected
{$IFDEF IOS}
    FUIView    : UIView;
    FDPFView   : TDPFView;
    FScreenRect: NSRect;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF IOS}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF IOS}
    property GetUIView: UIView read FUIView;
    procedure Loaded; override;
    procedure BecomeFirstResponder;
    procedure ResignFirstResponder;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    // procedure clearAllSubviews( isRelease: Boolean = false );
    function GetFrame: TFrame;
    procedure Rotate( Degree: Single );

  published
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Lightsteelblue;
    property BackgroundImage: string read FBackgroundImage write SetBackgroundImage;

    property Form : TForm read FForm write SetForm;
    property Frame: TDPFFrame read FFrame write SetFrame;

    property OnFormChanging: TDPFFormChanging read FOnFormChanging write FOnFormChanging;
    property OnFormChanged : TDPFFormChanged read FOnFormChanged write FOnFormChanged;

    property OnFrameChanging: TDPFFrameChanging read FOnFrameChanging write FOnFrameChanging;
    property OnFrameChanged : TDPFFrameChanged read FOnFrameChanged write FOnFrameChanged;

    property DelaysTouchesBegan: Boolean read FDelaysTouchesBegan write FDelaysTouchesBegan default true;
    property DelaysTouchesEnded: Boolean read FDelaysTouchesEnded write FDelaysTouchesEnded default false;

    property CheckReceiveTouch      : Boolean read FCheckReceiveTouch write FCheckReceiveTouch default true;
    property CanBecomeFirstResponder: Boolean read FCanBecomeFirstResponder write FCanBecomeFirstResponder default false;

    property OnClick      : TDPFOnClicked read FOnClick write FOnClick { SetOnClick };
    property OnDoubleClick: TDPFOnDoubleClicked read FOnDoubleClick write FOnDoubleClick;

    property OnTouchesEnded: TDPFTouchesEnded read FTouchesEnded write FTouchesEnded;
    property OnTouchesBegan: TDPFTouchesBegan read FTouchesBegan write FTouchesBegan;
    property OnTouchesMoved: TDPFTouchesMoved read FTouchesMoved write FTouchesMoved;

    property OnRemoteControlReceived: TDPFOnRemoteControlReceived read FOnRemoteControlReceived write FOnRemoteControlReceived;

    property OnDrawRect    : TDPFViewOnDrawRect read FOnDrawRect write FOnDrawRect;
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
    property OnPanGestureRecognizer;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFUIView }
constructor TDPFUIView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'View Control';
  FBackgroundColor := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FForm            := nil;

  FDelaysTouchesBegan := true;
  DelaysTouchesEnded  := false;

  FCheckReceiveTouch       := true;
  FCanBecomeFirstResponder := false;

  FTapDelay       := 1;
  FDoubleTapDelay := 2;

{$IFDEF IOS}
  if TOSVersion.Major >= 5 then
  begin
    FDPFView := TDPFView.Create( self );
    FUIView  := TUIView.Wrap( FDPFView.Super.init );
  end
  else
    FUIView  := TUIView.Create;
  FUIControl := FUIView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIView.Destroy;
begin
{$IFDEF IOS}
  FDPFView.DisposeOf;
  // FUIControl := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFUIView.GetFrame: TFrame;
begin
  result := FEmbFrame;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUIView.Loaded;
begin
  FScreenRect := TUIScreen.Wrap( TUIScreen.OCClass.mainScreen ).bounds;
  Resize; // Very Important for Embbeded Forms, Frames

  FUIView.setHidden( not Visible );

  if FBackgroundImage <> '' then
    SetBackgroundImage( FBackgroundImage )
  else
    SetBackgroundColor( FBackgroundColor );

  // SetOnClick( FOnClick );

  Resize;
  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  if isRootControl then
    resize;
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIView.Resize;
{$IFDEF IOS}
{$IFDEF DELPHIXE7}
//var
//  LOrientation: Cardinal;
//  ASize       : TSizeF;
//  tmpHeight   : Single;
//  sbh         : Single;
{$ENDIF}
{$ENDIF}
begin
//SZ: do we still need this? It appears that UpdateUIControlPosition works much better
(*{$IFDEF IOS}
  FScreenRect := TUIScreen.Wrap( TUIScreen.OCClass.mainScreen ).bounds;
  if isRootControl and ( Align in [TAlignLayout.Client, TAlignLayout.Left, TAlignLayout.Right, TAlignLayout.Contents, TAlignLayout.MostLeft, TAlignLayout.MostRight, TAlignLayout.FitLeft, TAlignLayout.Fit, TAlignLayout.FitRight] ) then
  begin
    if TOSVersion.Major > 6 then
    begin
      LOrientation := GetSharedApplication.statusBarOrientation;
      if ( LOrientation = UIDeviceOrientationLandscapeLeft ) or ( LOrientation = UIDeviceOrientationLandscapeRight ) then
      begin
        sbh           := GetSharedApplication.statusBarFrame.size.width;
        tmpHeight     := FScreenRect.size.width - sbh;
        MinClipHeight := FScreenRect.size.width - sbh;
      end
      else
      begin
        sbh           := GetSharedApplication.statusBarFrame.size.height;
        tmpHeight     := FScreenRect.size.height - sbh;
        MinClipHeight := FScreenRect.size.height - sbh;
      end;

      ASize.Width  := Size.Width;
      ASize.Height := tmpHeight;
      Size.SetSizeWithoutNotification( ASize );
      DPFNSLog('uiview.resize', CGRectMake( Position.X, Position.Y, ASize.Width * Scale.X, ASize.Height * Scale.Y ));
      if FUIView <> nil then
        FUIView.setFrame( CGRectMake( Position.X, Position.Y, ASize.Width * Scale.X, ASize.Height * Scale.Y ) );
      exit;
    end;
  end;
{$ENDIF}   *)
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIView.Paint;
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
procedure TDPFUIView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIView.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIView.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.SetBackgroundImage( const Value: string );
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
  if Assigned( FUIView ) then
  begin
    if FBackgroundImage <> '' then
    begin
      // Image2 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );

      Image1 := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FBackgroundImage ) ) );
      UIGraphicsBeginImageContext( FUIView.frame.size );
{$IFNDEF IOSDEVICE}
      // ----------------------------------------------------------
      // Mirror on Simulator
      context   := UIGraphicsGetCurrentContext( );
      transform := CGAffineTransformMakeTranslation( 0.0, Height );
      transform := CGAffineTransformScale( transform, 1.0, -1.0 );
      CGContextConcatCTM( context, transform );
{$ENDIF}
      Image1.drawInRect( FUIView.bounds );
      Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
      UIGraphicsEndImageContext( );

      FUIView.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image2 ) ) );
    end
    else
      SetBackgroundColor( FBackgroundColor );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.SetForm( const Value: TForm );
{$IFDEF IOS}
var
  C        : TDPFiOSBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF IOS}
  if ( FUIView <> nil ) and ( ( FForm <> Value ) or ( Assigned( Value ) and not Assigned( Value.TagObject ) ) ) then
  begin
    CanChange := True;
    if Assigned( FOnFormChanging ) then
      FOnFormChanging( Self, FForm, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FForm ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews( );
      FForm.TagObject := nil;
    end;

    if Assigned( Value ) then
    begin

      if Value.TagObject <> nil then
        MoveChildsToOriginParent( Value.TagObject as TDPFiOSBaseControl );

      while Value.ChildrenCount <> 0 do
        if Value.Children[0] is TDPFiOSBaseControl then
        begin
          C        := Value.Children[0] as TDPFiOSBaseControl;
          C.Parent := Self;
          C.Loaded;
          Value.TagObject := Self;
        end;
    end;
    if Assigned( FOnFormChanged ) then
      FOnFormChanged( Self, FForm, Value );
  end;
{$ENDIF}
  FForm := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.SetFrame( const Value: TDPFFrame );
{$IFDEF IOS}
var
  C        : TDPFiOSBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FEmbFrame ) and ( FEmbFrame is Value ) then
    exit;

  if ( FUIView <> nil ) then
  begin
    CanChange := True;
    if Assigned( FOnFrameChanging ) then
      FOnFrameChanging( Self, FFrame, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FEmbFrame ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews(  );
      FEmbFrame.TagObject := nil;
      FEmbFrame.DisposeOf;
      FEmbFrame := nil;
    end;
    if Assigned( Value ) then
    begin
      FEmbFrame := Value.Create( nil );

      while FEmbFrame.ChildrenCount <> 0 do
        if FEmbFrame.Children[0] is TDPFiOSBaseControl then
        begin
          C        := FEmbFrame.Children[0] as TDPFiOSBaseControl;
          C.Parent := Self;
          C.Loaded;
        end;
    end;
    if Assigned( FOnFrameChanged ) then
      FOnFrameChanged( Self, Frame, Value );
  end;
{$ENDIF}
  FFrame := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.Rotate( Degree: Single );
{$IFDEF IOS}
var
  swingTransform: CGAffineTransform;
{$ENDIF}
begin
{$IFDEF IOS}
  swingTransform := CGAffineTransformIdentity;
  swingTransform := CGAffineTransformRotate( swingTransform, DegToRad( Degree ) );

  TUIView.OCClass.beginAnimations( PNSStr( ' swing ' ), ( FUIView as ILocalObject ).GetObjectID );
  TUIView.OCClass.setAnimationDuration( 0.25 );
  FUIView.setTransform( swingTransform );
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIView.BecomeFirstResponder;
begin
  if Assigned( FUIView ) then
    FUIView.becomeFirstResponder
end;

// ------------------------------------------------------------------------------
procedure TDPFUIView.ResignFirstResponder;
begin
  if Assigned( FUIView ) then
    FUIView.resignFirstResponder
end;

// ------------------------------------------------------------------------------
{ TDPFView }

constructor TDPFView.Create( ADPFUIView: TDPFUIView );
var
  V: Pointer;
begin
  inherited Create;
  FDPFUIView := ADPFUIView;
  V          := UIView( Super ).initWithFrame( CGRectMake( 0, 0, 0, 0 ) );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

destructor TDPFView.Destroy;
begin
  {$IFDEF IOS}
  UIView(Super).release; //SZ added (fix memory leak)
  {$ENDIF}
  inherited Destroy;
end;

// ------------------------------------------------------------------------------
function TDPFView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFView );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.drawRect( rect: CGRect );
begin
  UIView( Super ).drawRect( rect );
  if assigned( FDPFUIView.FOnDrawRect ) then
    FDPFUIView.FOnDrawRect( FDPFUIView, DPFNSRect( rect ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesBegan( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFUIView.FUIView );
  PrevP := Touch.previousLocationInView( FDPFUIView.FUIView );
  if Assigned( FDPFUIView.FTouchesBegan ) then
    FDPFUIView.FTouchesBegan( FDPFUIView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesMoved( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFUIView.FUIView );
  PrevP := Touch.previousLocationInView( FDPFUIView.FUIView );
  if Assigned( FDPFUIView.FTouchesMoved ) then
    FDPFUIView.FTouchesMoved( FDPFUIView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIView( Super ).touchesEnded( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFUIView.FUIView );
  PrevP := Touch.previousLocationInView( FDPFUIView.FUIView );
  if Assigned( FDPFUIView.FTouchesEnded ) then
    FDPFUIView.FTouchesEnded( FDPFUIView, Touch.tapCount, DPFNSPoint( P ), DPFNSPoint( PrevP ) );

  if Touch.tapCount = 1 then
    NSObject( self.Super ).performSelector( sel_getUid( 'singleTap:' ), nil, FDPFUIView.TapDelay / 1000 )
  else if Touch.tapCount > 1 then
  begin
    iOSapi.{$IFDEF DELPHIXE7}Foundation{$ELSE}CocoaTypes{$ENDIF}.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget( Self.GetObjectID );
    NSObject( self.Super ).performSelector( sel_getUid( 'doubleTap:' ), nil, FDPFUIView.DoubleTapDelay / 1000 );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFView.singleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFUIView.FOnClick ) then
    FDPFUIView.FOnClick( FDPFUIView );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.doubleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFUIView.FOnDoubleClick ) then
    FDPFUIView.FOnDoubleClick( FDPFUIView );
end;

// ------------------------------------------------------------------------------
procedure TDPFView.remoteControlReceivedWithEvent( event: UIEvent ); cdecl;
begin
  if Assigned( FDPFUIView.FOnRemoteControlReceived ) then
    FDPFUIView.FOnRemoteControlReceived( FDPFUIView, event.&Type, event.subtype );
end;

// ------------------------------------------------------------------------------
function TDPFView.canBecomeFirstResponder: Boolean; cdecl;
begin
  result := FDPFUIView.CanBecomeFirstResponder or Assigned( FDPFUIView.FOnRemoteControlReceived );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
