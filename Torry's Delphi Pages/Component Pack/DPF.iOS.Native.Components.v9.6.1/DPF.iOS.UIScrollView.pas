// ------------------------------------------------------------------------------
// DPF.iOS.UIScrollView Component
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
unit DPF.iOS.UIScrollView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.TypInfo,
  System.Math,

  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
  DPF.iOS.Classes,
{$ENDIF}
  FMX.Types,
  FMX.Controls;

type
{$M+}
  TDPFUIScrollView = class;
{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  UIScrollViewDelegate = interface( IObjectiveC )
    ['{93DAB693-B2C6-4C15-8607-5805B9B5F406}']
    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;

    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
    function viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;
  end;

  TUIScrollViewDelegate = class( TOCLocal, UIScrollViewDelegate )
  private
    FDPFUIScrollView: TDPFUIScrollView;
  public
    constructor Create( AParent: TDPFUIScrollView );

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;

    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
    function viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  TDPFScrollType = ( stHorizontal, stVertical, stBoth );

  TScrollPageChanged          = procedure( Sender: TObject; PageNO: Integer ) of object;
  TScrollWillBeginDragging    = procedure( Sender: TObject ) of object;
  TScrollDidEndDecelerating   = procedure( Sender: TObject ) of object;
  TScrollDidScroll            = procedure( Sender: TObject; ContentOffset: DPFNSPoint ) of object;
  TViewForZoomingInScrollView = procedure( Sender: TObject; var ViewForZoomObject: TDPFiOSBaseControl ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIScrollView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIScrollView        : UIScrollView;
    FUIScrollViewDelegate: TUIScrollViewDelegate;
{$ENDIF}
    FBackgroundColor: TAlphaColor;

    FShowsHorizontalScrollIndicator: Boolean;
    FShowsVerticalScrollIndicator  : Boolean;
    FAlwaysBounceHorizontal        : Boolean;
    FAlwaysBounceVertical          : Boolean;
    FPagingEnabled                 : Boolean;
    FScrollType                    : TDPFScrollType;
    FBounces                       : Boolean;
    FScrollEnabled                 : Boolean;
    FOnPageChanged                 : TScrollPageChanged;

    // Begin Add by Tyson
{$IFDEF IOS}
    FNeedOffset     : Boolean;
    FKeyboardShowing: Boolean;
    FRestorePoint   : TPointF;
{$ENDIF}
    FLastAlign                   : TAlignLayout;
    FOnWillBeginDragging         : TScrollWillBeginDragging;
    FOnDidEndDecelerating        : TScrollDidEndDecelerating;
    FOnDidScroll                 : TScrollDidScroll;
    FBackgroundImage             : string;
    FBouncesZoom                 : Boolean;
    FMaximumZoomScale            : Single;
    FMinimumZoomScale            : Single;
    FOnViewForZoomingInScrollView: TViewForZoomingInScrollView;
    // End Add by Tyson
{$IFDEF IOS}
    FLastPos: TRectF; // Added by Fenistil
{$ENDIF}
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetScrollEnabled( const Value: Boolean );
    procedure SetShowsVerticalScrollIndicator( const Value: Boolean );
    procedure SetShowsHorizontalScrollIndicator( const Value: Boolean );
    procedure SetAlwaysBounceHorizontal( const Value: Boolean );
    procedure SetAlwaysBounceVertical( const Value: Boolean );
    procedure SetPagingEnabled( const Value: Boolean );
    procedure SetBounces( const Value: Boolean );
    function GetPageNo: Integer;
    procedure SetBackgroundImage( const Value: string );
    procedure SetBouncesZoom( const Value: Boolean );
    procedure SetMaximumZoomScale( const Value: Single );
    procedure SetMinimumZoomScale( const Value: Single );

  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
    function GetUIScrollView: UIScrollView;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure AddControl( obj: TDPFiOSBaseControl; AutoReCalc: Boolean = true );
    procedure RemoveControl( obj: TDPFiOSBaseControl );
    procedure DeleteAllControls;
    procedure ScrollToTop( PreserveX: Boolean = false; Animated: Boolean = true );
    procedure ScrollTo( x, y: single; Animated: Boolean = true );
    procedure ScrollToPageNo( PageNo: Integer; Animate: Boolean = true );
    procedure SetScrollPageContentSize( Width, Height: Single );
    procedure ReCalc;
    procedure ReHAlignObjects;
    procedure ScrollToControl( Control: TDPFiOSBaseControl );
    // Begin Add by Tyson
    procedure ScrollToFocused( Control: TControl; KBBounds: TRectF; Offset: Integer );
    procedure RestoreFromFocused( KBBounds: TRectF );
    procedure Resizeframe( KBBounds: TRectF; KBVisible: Boolean );
    // End Add by Tyson
    // Added by Fensitil
    procedure KeyboardShown( KBBounds: TRectF; const FocusedControl: TControl = nil; const Offset: integer = 0 );
    procedure KeyboardHidden;
  published
    property BackgroundImage               : string read FBackgroundImage write SetBackgroundImage;
    property BackgroundColor               : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.White;
    property ScrollEnabled                 : Boolean read FScrollEnabled write SetScrollEnabled default True;
    property Bounces                       : Boolean read FBounces write SetBounces default True;
    property ShowsHorizontalScrollIndicator: Boolean read FShowsHorizontalScrollIndicator write SetShowsHorizontalScrollIndicator default True;
    property ShowsVerticalScrollIndicator  : Boolean read FShowsVerticalScrollIndicator write SetShowsVerticalScrollIndicator default True;
    property AlwaysBounceHorizontal        : Boolean read FAlwaysBounceHorizontal write SetAlwaysBounceHorizontal default True;
    property AlwaysBounceVertical          : Boolean read FAlwaysBounceVertical write SetAlwaysBounceVertical default True;
    property PagingEnabled                 : Boolean read FPagingEnabled write SetPagingEnabled default True;

    property BouncesZoom     : Boolean read FBouncesZoom write SetBouncesZoom default True;
    property MaximumZoomScale: Single read FMaximumZoomScale write SetMaximumZoomScale;
    property MinimumZoomScale: Single read FMinimumZoomScale write SetMinimumZoomScale;

    property ScrollType                  : TDPFScrollType read FScrollType write FScrollType default stBoth;
    property PageNo                      : Integer read GetPageNo;
    property OnPageChanged               : TScrollPageChanged read FOnPageChanged write FOnPageChanged;
    property OnWillBeginDragging         : TScrollWillBeginDragging read FOnWillBeginDragging write FOnWillBeginDragging;
    property OnDidEndDecelerating        : TScrollDidEndDecelerating read FOnDidEndDecelerating write FOnDidEndDecelerating;
    property OnDidScroll                 : TScrollDidScroll read FOnDidScroll write FOnDidScroll;
    property OnViewForZoomingInScrollView: TViewForZoomingInScrollView read FOnViewForZoomingInScrollView write FOnViewForZoomingInScrollView;

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
{ TDPFUIScrollView }
procedure TDPFUIScrollView.AddControl( obj: TDPFiOSBaseControl; AutoReCalc: Boolean = true );
begin
  self.AddObject( obj );
{$IFDEF IOS}
  obj.Loaded;
{$ENDIF}
  if AutoReCalc then
    ReCalc;
end;

// Begin Add by Tyson
// ------------------------------------------------------------------------------

procedure TDPFUIScrollView.ResizeFrame( KBBounds: TRectF; KBVisible: Boolean );
begin
  if KBVisible then
  begin
    FLastAlign := Align;
    Align      := TAlignLayout.None;
  end
  else
    Align := FLastAlign;

{$IFDEF IOS}
  // Do animation for scrollview frame change.
  TUIView.OCClass.beginAnimations( nil, nil );

  try
    TUIView.OCClass.setAnimationDuration( 0.25 );
    TUIView.OCClass.setAnimationBeginsFromCurrentState( True );
    Height := Height - KBBounds.Height;
    Resize;
  finally
    TUIView.OCClass.commitAnimations;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.RestoreFromFocused( KBBounds: TRectF );
{$IFDEF IOS}
var
  ContentInsets: UIEdgeInsets;
{$ENDIF}
begin
{$IFDEF IOS}
  ContentInsets.top    := 0;
  ContentInsets.bottom := 0;
  ContentInsets.left   := 0;
  ContentInsets.right  := 0;

  if FUIScrollView <> nil then
  begin
    FUIScrollView.setContentInset( ContentInsets );
    FUIScrollView.setScrollIndicatorInsets( ContentInsets );
  end;

  // ScrollTo(FRestorePoint.X, FRestorePoint.Y);
  FNeedOffset := False;

  FKeyboardShowing := False;
  ResizeFrame( KBBounds, FKeyboardshowing );

  SetBounces( FBounces );
  SetAlwaysBounceHorizontal( FAlwaysBounceHorizontal );
  SetAlwaysBounceVertical( FAlwaysBounceHorizontal );
  SetPagingEnabled( FPagingEnabled );
  SetScrollEnabled( FScrollEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ScrollToFocused( Control: TControl; KBBounds: TRectF; Offset: Integer );
{$IFDEF IOS}
var
  LFocusRect   : TRectF;
  ContentInsets: UIEdgeInsets;
{$ENDIF}
begin
{$IFDEF IOS}
  if not FKeyboardShowing then
    FLastAlign := Align;

  FKeyboardShowing := True;

  FRestorepoint.X := Position.X;
  FRestorepoint.Y := Position.Y;

  ContentInsets.top    := 0;
  ContentInsets.bottom := 0;
  ContentInsets.left   := 0;
  ContentInsets.right  := 0;

  if FUIScrollView <> nil then
  begin
    FUIScrollView.setContentInset( ContentInsets );
    FUIScrollView.setscrollIndicatorInsets( ContentInsets );
  end;

  if Assigned( Control ) then
  begin

    LFocusRect := Control.AbsoluteRect;

    if FUIScrollView <> nil then
    begin
      FUIScrollView.setBounces( False ); // Disable Bounces
      FUIScrollView.SetAlwaysBounceHorizontal( False );
      FUIScrollView.SetAlwaysBounceVertical( False );
      FUIScrollView.setScrollEnabled( true );
    end;

    if ( LFocusRect.IntersectsWith( TRectF.Create( KBBounds ) ) ) and ( LFocusRect.Bottom > KBBounds.TopLeft.Y ) then
    begin
      FNeedOffset := True;
      ScrollTo( 0, LFocusRect.Bottom - ( KBBounds.TopLeft.Y - Offset ) );
      ResizeFrame( KBBounds, FKeyboardShowing );
    end
    else
      ResizeFrame( KBBounds, FKeyboardShowing );

  end;
{$ENDIF}
end;
// End Add by Tyson

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ScrollTo( x, y: single; Animated: Boolean = true );
begin
{$IFDEF IOS}
  FUIScrollView.setContentOffset( CGPointMake( x, y ), Animated );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ScrollToControl( Control: TDPFiOSBaseControl );
begin
{$IFDEF IOS}
  FUIScrollView.scrollRectToVisible( UIView( Control.UIControl ).frame, true );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ScrollToPageNo( PageNo: Integer; Animate: Boolean = true );
{$IFDEF IOS}
var
  frame: CGRect;
{$ENDIF}
begin
{$IFDEF IOS}
  frame          := FUIScrollView.frame;
  frame.origin.x := frame.size.width * PageNo;
  frame.origin.y := 0;
  FUIScrollView.scrollRectToVisible( frame, Animate );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ScrollToTop( PreserveX, Animated: Boolean );
begin
{$IFDEF IOS}
  if PreserveX then
    FUIScrollView.setContentOffset( CGPointMake( FUIScrollView.contentOffset.x, 0 ), Animated )
  else
    FUIScrollView.setContentOffset( CGPointMake( 0, 0 ), Animated );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFUIScrollView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'ScrollView Control';
  FBackgroundColor := TAlphaColors.White;
  FScrollEnabled   := True;

  FMaximumZoomScale               := 1.0;
  FMinimumZoomScale               := 1.0;
  FBouncesZoom                    := True;
  FBounces                        := True;
  FShowsHorizontalScrollIndicator := True;
  FShowsVerticalScrollIndicator   := True;
  FAlwaysBounceHorizontal         := True;
  FAlwaysBounceVertical           := True;
  FPagingEnabled                  := True;
  FScrollType                     := stBoth;

{$IFDEF IOS}
  FUIScrollViewDelegate := TUIScrollViewDelegate.Create( self );

  FUIScrollView := TUIScrollView.Create;
  FUIControl    := FUIScrollView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIScrollView.Destroy;
begin
  while ChildrenCount > 0 do
    Children[0].DisposeOf;

{$IFDEF IOS}
  FUIScrollViewDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFUIScrollView.GetPageNo: Integer;
begin
  result := 0;
{$IFDEF IOS}
  if not Assigned( FUIScrollView ) or ( FUIScrollView.frame.size.width = 0 ) then
    exit;
  Result := Round( FUIScrollView.contentOffset.x / FUIScrollView.frame.size.width );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.KeyboardHidden;
begin
{$IFDEF IOS}
  // Based on Tyson's resize methods.
  if not FKeyboardShowing then
    Exit;
  FKeyboardShowing := false;
  // Resize the ScrollView back to its original
  TUIView.OCClass.beginAnimations( nil, nil );
  try
    TUIView.OCClass.setAnimationDuration( 0.25 );
    TUIView.OCClass.setAnimationBeginsFromCurrentState( true );
    Position.X := FLastPos.Left;
    Position.Y := FLastPos.Top;
    Width      := FLastPos.Width;
    Height     := FLastPos.Height;
    Align      := FLastAlign;
    Resize;
  finally
    TUIView.OCClass.commitAnimations;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.KeyboardShown( KBBounds: TRectF; const FocusedControl: TControl = nil; const Offset: integer = 0 );

  function RealAbsoluteRect( Control: TControl ): TRectF;
  var
    P: TControl;

  begin
    Result.Left := Control.Position.X;
    Result.Top  := Control.Position.Y;
    if Control.Parent is TControl then
      P := Control.Parent as TControl
    else
      P := nil;
    while P <> nil do
    begin
      Result.Left := Result.Left + P.Position.X;
      Result.Top  := Result.Top + P.Position.Y;
      if P.Parent is TControl then
        P := P.Parent as TControl
      else
        P := nil;
    end;
    Result.Right  := Result.Left + Control.Width;
    Result.Bottom := Result.Top + Control.Height;
  end;

begin
{$IFDEF IOS}
  // Based on Tyson's resize methods.
  if FKeyboardShowing then
    Exit;
  FKeyboardShowing := true;
  // Save the align, positions and sizes
  FLastAlign      := Align;
  FLastPos.Left   := Position.X;
  FLastPos.Top    := Position.Y;
  FLastPos.Width  := Width;
  FLastPos.Height := Height;
  Align           := TAlignLayout.None;
  // Disable bounces
  if FUIScrollView <> nil then
  begin
    FUIScrollView.setBounces( false );
    FUIScrollView.SetAlwaysBounceHorizontal( false );
    FUIScrollView.SetAlwaysBounceVertical( false );
  end;
  // Do we need to resize the ScrollView? (Is the keyboard cover it?)
  if ( RealAbsoluteRect( Self ).Bottom > KBBounds.Top ) then
  begin
    // Do we need to scroll it, due to the Focusedcontrol?
    if FocusedControl <> nil then
    begin
      if RealAbsoluteRect( FocusedControl ).IntersectsWith( KBBounds ) then
        ScrollTo( 0, RealAbsoluteRect( FocusedControl ).Bottom - KBBounds.Top + Offset );
    end;
    // Resize the ScrollView
    TUIView.OCClass.beginAnimations( nil, nil );
    try
      TUIView.OCClass.setAnimationDuration( 0.25 );
      TUIView.OCClass.setAnimationBeginsFromCurrentState( true );
      Height := KBBounds.Top - RealAbsoluteRect( Self ).Top;
      Resize;
    finally
      TUIView.OCClass.commitAnimations;
    end;
  end;
  // Enable bounces
  SetBounces( FBounces );
  SetAlwaysBounceHorizontal( FAlwaysBounceHorizontal );
  SetAlwaysBounceVertical( FAlwaysBounceHorizontal );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.DeleteAllControls;
var
  cobj: TFmxObject;
begin
{$IFDEF IOS}
  while FUIScrollView.subviews.count > 0 do
  begin
    TUIView.Wrap( FUIScrollView.subviews.lastObject ).removeFromSuperview;
  end;
{$ENDIF}
  while ChildrenCount > 0 do
  begin
    cobj := Children.Items[0];
    cobj.DisposeOf;
  end;
  ReCalc;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUIScrollView.Loaded;
begin
  Resize;
  FUIScrollView.setIndicatorStyle( UIScrollViewIndicatorStyleBlack );

  SetBounces( FBounces );
  SetBackgroundColor( FBackgroundColor );
  SetScrollEnabled( FScrollEnabled );

  SetShowsHorizontalScrollIndicator( FShowsHorizontalScrollIndicator );
  SetShowsVerticalScrollIndicator( FShowsVerticalScrollIndicator );

  SetAlwaysBounceHorizontal( FAlwaysBounceHorizontal );
  SetAlwaysBounceVertical( FAlwaysBounceVertical );

  SetBouncesZoom( FBouncesZoom );
  SetMinimumZoomScale( FMinimumZoomScale );
  SetMaximumZoomScale( FMaximumZoomScale );

  FUIScrollView.setDelegate( ILocalObject( FUIScrollViewDelegate ).GetObjectID );

  ReCalc;

  // FUIScrollView.setDirectionalLockEnabled( true );
  // FUIScrollView.setAutoresizingMask( UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight );
  // FUIScrollView.setAutoresizesSubviews( false );

  SetPagingEnabled( FPagingEnabled );
  AddSubView( self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFUIScrollView.GetUIScrollView: UIScrollView;
begin
  result := FUIScrollView;
end;
// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ReHAlignObjects;
var
  I   : Integer;
  X, Y: Single;
  C   : TDPFiOSBaseControl;
begin
  X     := 0;
  Y     := 0;
  for i := 0 to ChildrenCount - 1 do
  begin
    C := ( Children[i] as TDPFiOSBaseControl );
    C.SetBounds( X, Y, Width, Height );
    X := X + Width + 10;
  end;
  SetScrollPageContentSize( Width * ChildrenCount, Height );
  SetPagingEnabled( FPagingEnabled );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.ReCalc;
{$IFDEF IOS}
var
  MaxW, MaxH: Single;
  i         : Integer;
  C         : TDPFiOSBaseControl;
{$ENDIF}
begin
{$IFDEF IOS}
  MaxW  := Width;
  MaxH  := Height;
  for i := 0 to ChildrenCount - 1 do
  begin
    C    := ( Children[i] as TDPFiOSBaseControl );
    MaxW := Max( MaxW, C.Position.X + C.Width + C.Margins.Left + C.Margins.Right );
    MaxH := Max( MaxH, C.Position.Y + C.Height + C.Margins.Top + C.Margins.Bottom );
  end;

  FUIScrollView.setContentSize( CGSizeMake( MaxW, MaxH ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.RemoveControl( obj: TDPFiOSBaseControl );
var
  idx : Integer;
  cobj: TFmxObject;
begin
  idx := Children.IndexOf( obj );
  if idx > -1 then
  begin
    cobj := Children.Items[idx];
    Children.Items[idx].DisposeOf;
    cobj.DisposeOf;
  end;
  ReCalc;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.Paint;
begin
  InternalPaint( '', TAlphaColors.White, TDPFTextAlignment.taCenter, FBackgroundColor );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetAlwaysBounceHorizontal( const Value: Boolean );
begin
  FAlwaysBounceHorizontal := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetAlwaysBounceHorizontal( FAlwaysBounceHorizontal );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetAlwaysBounceVertical( const Value: Boolean );
begin
  FAlwaysBounceVertical := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetAlwaysBounceVertical( FAlwaysBounceVertical );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIScrollView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIScrollView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

procedure TDPFUIScrollView.SetBackgroundImage( const Value: string );
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
  if Assigned( FUIScrollView ) then
  begin
    if FBackgroundImage <> '' then
    begin
      Image2 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );

      // ----------------------------------------------------------
      // Mirror on Simulator

      Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );
      UIGraphicsBeginImageContext( FUIScrollView.frame.size );
{$IFNDEF IOSDEVICE}
      context   := UIGraphicsGetCurrentContext( );
      transform := CGAffineTransformMakeTranslation( 0.0, Height );
      transform := CGAffineTransformScale( transform, 1.0, -1.0 );
      CGContextConcatCTM( context, transform );
{$ENDIF}
      Image1.drawInRect( FUIScrollView.frame );
      Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
      UIGraphicsEndImageContext( );

      FUIScrollView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image2 ) ) );
    end
    else
      SetBackgroundColor( FBackgroundColor );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetBounces( const Value: Boolean );
begin
  FBounces := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.setBounces( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetBouncesZoom( const Value: Boolean );
begin
  FBouncesZoom := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.setBouncesZoom( FBouncesZoom );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetMaximumZoomScale( const Value: Single );
begin
  FMaximumZoomScale := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.setMaximumZoomScale( FMaximumZoomScale );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetMinimumZoomScale( const Value: Single );
begin
  FMinimumZoomScale := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.setMinimumZoomScale( FMinimumZoomScale );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetShowsVerticalScrollIndicator( const Value: Boolean );
begin
  FShowsVerticalScrollIndicator := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetShowsVerticalScrollIndicator( FShowsVerticalScrollIndicator );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetPagingEnabled( const Value: Boolean );
begin
  FPagingEnabled := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetPagingEnabled( PagingEnabled );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetScrollEnabled( const Value: Boolean );
begin
  FScrollEnabled := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetScrollEnabled( FScrollEnabled );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetScrollPageContentSize( Width, Height: Single );
{$IFDEF IOS}
var
  NS: NSSize;
{$ENDIF}
begin
{$IFDEF IOS}
  NS.width  := Width;
  NS.height := Height;
  FUIScrollView.setContentSize( NS );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIScrollView.SetShowsHorizontalScrollIndicator( const Value: Boolean );
begin
  FShowsHorizontalScrollIndicator := Value;
{$IFDEF IOS}
  if FUIScrollView <> nil then
  begin
    FUIScrollView.SetShowsHorizontalScrollIndicator( ShowsHorizontalScrollIndicator );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TUIScrollViewDelegate }
{$IFDEF IOS}

constructor TUIScrollViewDelegate.Create( AParent: TDPFUIScrollView );
begin
  inherited Create;
  FDPFUIScrollView := AParent;
end;

// ------------------------------------------------------------------------------
procedure TUIScrollViewDelegate.scrollViewDidScroll( scrollView: UIScrollView );
var
  offset: CGPoint;
begin
  if Assigned( FDPFUIScrollView.OnDidScroll ) then
    FDPFUIScrollView.OnDidScroll( FDPFUIScrollView, DPFNSPoint( scrollView.contentOffset ) );
  offset := scrollView.contentOffset;
  if FDPFUIScrollView.ScrollType = stVertical then
  begin
    if scrollView.contentOffset.X <> 0 then
    begin
      offset.X := 0;
      scrollView.setContentOffset( offset );
    end
  end
  else if FDPFUIScrollView.ScrollType = stHorizontal then
  begin
    if scrollView.contentOffset.Y <> 0 then
    begin
      offset.Y := 0;
      scrollView.setContentOffset( offset );
    end
  end;
end;

// ------------------------------------------------------------------------------

procedure TUIScrollViewDelegate.scrollViewWillBeginDragging( scrollView: UIScrollView );
begin
  if Assigned( FDPFUIScrollView.FOnWillBeginDragging ) then
    FDPFUIScrollView.FOnWillBeginDragging( FDPFUIScrollView );
end;

// ------------------------------------------------------------------------------
procedure TUIScrollViewDelegate.scrollViewDidEndDecelerating( scrollView: UIScrollView );
begin
  if Assigned( FDPFUIScrollView.FOnDidEndDecelerating ) then
    FDPFUIScrollView.FOnDidEndDecelerating( FDPFUIScrollView );

  if Assigned( FDPFUIScrollView.FOnPageChanged ) and FDPFUIScrollView.PagingEnabled then
    FDPFUIScrollView.FOnPageChanged( FDPFUIScrollView, FDPFUIScrollView.PageNo );
end;

// ------------------------------------------------------------------------------
function TUIScrollViewDelegate.viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;
var
  obj: TDPFiOSBaseControl;
begin
  result := nil;
  if assigned( FDPFUIScrollView.FOnViewForZoomingInScrollView ) then
  begin
    FDPFUIScrollView.FOnViewForZoomingInScrollView( FDPFUIScrollView, obj );
    result := obj.ThisView;
  end;

end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
