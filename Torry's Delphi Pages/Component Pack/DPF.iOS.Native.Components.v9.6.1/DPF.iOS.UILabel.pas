// ------------------------------------------------------------------------------
// DPF.iOS.UILabel Component
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
unit DPF.iOS.UILabel;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  iOSapi.CocoaTypes,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
  DPF.iOS.Classes,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFLabel = class;

{$IFDEF IOS}

  DPFUILabel = interface( UIlabel )
    ['{4C32C9BB-2550-4815-ACF2-96333B96D0A7}']
    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure drawTextInRect( rect: CGRect ); cdecl;
  end;

  TDPFUILabel = class( TOCLocal )
  private
  protected
    FDPFLabel: TDPFLabel;
  public
    constructor Create( ADPFLabel: TDPFLabel );
    function GetObjectiveCClass: PTypeInfo; override;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure drawTextInRect( rect: CGRect ); cdecl;

  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFLabel = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFUILabel: TDPFUILabel;
    FUILabel   : UILabel;
{$ENDIF}
    FText                     : string;
    FTextColor                : TAlphaColor;
    FLineBreak                : TDPFLineBreak;
    FBackgroundColor          : TAlphaColor;
    FNumberOfLines            : Integer;
    FTextAlignment            : TDPFTextAlignment;
    FAdjustsFontSizeToFitWidth: Boolean;
    FBackgroundImage          : string;
    FOnClick                  : TDPFOnClicked;
    FDelaysTouchesBegan       : Boolean;
    FDelaysTouchesEnded       : Boolean;
    FAutoSizeToFit            : Boolean;
    FOnTouchesBegan           : TDPFTouchesBegan;
    FOnDoubleClick            : TDPFOnDoubleClicked;
    FOnTouchesEnded           : TDPFTouchesEnded;
    FOnTouchesMoved           : TDPFTouchesMoved;
    FEnabled                  : Boolean;
    FCapitalizedString        : Boolean;
    FDoubleTapDelay           : integer;
    FTapDelay                 : integer;
    FBaseLineAdjustment       : TDPFBaseLineAdjustment;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetBackgroundImage( const Value: string );
    procedure SetLineBreak( const Value: TDPFLineBreak );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetEnabled( const Value: Boolean );
    procedure SetBaseLineAdjustment( const Value: TDPFBaseLineAdjustment );

  protected
    procedure Resize; override;
    procedure Move; override;
    procedure FontOnChanged( Sender: TObject );
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; override;
    function sizeToFit( isSetHeight: Boolean = false ): DPFNSize;
  published
    property LineBreak                : TDPFLineBreak read FLineBreak write SetLineBreak default lbCharacterWrap;
    property Text                     : string read FText write SetText;
    property TextColor                : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property BackgroundColor          : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property BackgroundImage          : string read FBackgroundImage write SetBackgroundImage;
    property NumberOfLines            : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment            : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taLeft;
    property BaseLineAdjustment       : TDPFBaseLineAdjustment read FBaseLineAdjustment write SetBaseLineAdjustment;
    property AdjustsFontSizeToFitWidth: Boolean read FAdjustsFontSizeToFitWidth write FAdjustsFontSizeToFitWidth default False;
    property AutoSizeToFit            : Boolean read FAutoSizeToFit write FAutoSizeToFit default False;
    property CapitalizedString        : Boolean read FCapitalizedString write FCapitalizedString default False;

    property DelaysTouchesBegan: Boolean read FDelaysTouchesBegan write FDelaysTouchesBegan default true;
    property DelaysTouchesEnded: Boolean read FDelaysTouchesEnded write FDelaysTouchesEnded default false;

    property OnTouchesBegan: TDPFTouchesBegan read FOnTouchesBegan write FOnTouchesBegan;
    property OnTouchesEnded: TDPFTouchesEnded read FOnTouchesEnded write FOnTouchesEnded;
    property OnTouchesMoved: TDPFTouchesMoved read FOnTouchesMoved write FOnTouchesMoved;

    property OnClick      : TDPFOnClicked read FOnClick write FOnClick;
    property OnDoubleClick: TDPFOnDoubleClicked read FOnDoubleClick write FOnDoubleClick;

    property Enabled: Boolean read FEnabled write SetEnabled default True;

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
    property Padding;
  end;

  // ------------------------------------------------------------------------------
function GetLabelTextHeight( Text: string; FontName: TDPFIOSFontList; FontSize: integer; Width: Single ): Single;

// ------------------------------------------------------------------------------
implementation

function GetLabelTextHeight( Text: string; FontName: TDPFIOSFontList; FontSize: integer; Width: Single ): Single;
var
  DPFLabel : TDPFLabel;
  cDPFNSize: DPFNSize;
begin
  DPFLabel               := TDPFLabel.Create( nil );
  DPFLabel.Font.FontName := FontName;
  DPFLabel.Font.FontSize := FontSize;
  DPFLabel.LineBreak     := lbWordWrap;
  DPFLabel.NumberOfLines := 0;
  DPFLabel.Text          := Text;
  DPFLabel.Width         := Width;
  DPFLabel.Loaded;
  cDPFNSize := DPFLabel.sizeToFit;
  Result    := Max( cDPFNSize.Height + 25, 50 ) + 25;
  Result    := Round( Result - 25 );
end;

// ------------------------------------------------------------------------------
{ TDPFLabel }
// ------------------------------------------------------------------------------
constructor TDPFLabel.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption             := 'Label';
  FLineBreak                 := lbCharacterWrap;
  FTextColor                 := TAlphaColors.Black;
  FBackgroundColor           := TAlphaColors.Null;
  FNumberOfLines             := 1;
  FTextAlignment             := taLeft;
  FAdjustsFontSizeToFitWidth := False;
  FDelaysTouchesBegan        := true;
  DelaysTouchesEnded         := false;
  Font.OnChanged             := FontOnChanged;
  ClipsToBounds              := false;
  FAutoSizeToFit             := False;
  FEnabled                   := true;
  FCapitalizedString         := False;
  FTapDelay                  := 1;
  FDoubleTapDelay            := 2;

{$IFDEF IOS}
  // FUILabel   := TUILabel.Create;
  FDPFUILabel := TDPFUILabel.Create( self );
  // FUILabel    := UILabel( FDPFUILabel.Super );
  FUILabel   := TUILabel.Wrap( FDPFUILabel.Super.Init );
  FUIControl := FUILabel;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFLabel.Destroy;
begin
{$IFDEF IOS}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF IOS}
  if FUILabel <> nil then
  begin
    FUILabel.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
function TDPFLabel.sizeToFit( isSetHeight: Boolean = false ): DPFNSize;
{$IFDEF IOS}
var
  f: CGRect;
{$ENDIF}
begin
{$IFDEF IOS}
  FUILabel.sizeToFit;
  FUILabel.setNeedsDisplay;
  if isSetHeight then
  begin
    f             := FUILabel.frame;
    f.size.height := Height;
    FUILabel.setFrame( f );
  end;
  result.width  := FUILabel.frame.size.width;
  result.height := FUILabel.frame.size.height;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFLabel.Loaded;
begin
  Resize;
  SetText( FText );
  SetLineBreak( FLineBreak );

  SetTextColor( FTextColor );

  setEnabled( FEnabled );
  FUILabel.setHidden( not Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );
  FUILabel.setAdjustsFontSizeToFitWidth( FAdjustsFontSizeToFitWidth );
  FUILabel.setFont( Font._UIFont );

  SetBackgroundColor( FBackgroundColor );
  SetBackgroundImage( FBackgroundImage );
  FUILabel.setBaselineAdjustment( Ord( FBaselineAdjustment ) );

  AddSubView( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;

  // ----------------------------
  // Important: Must be after AddSubView
  if FAutoSizeToFit then
    sizeToFit;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFLabel.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUILabel <> nil then
  begin
    FUILabel.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFLabel.Paint;
var
  Caption    : string;
  CaptionRect: TRectF;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.Solid;
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, 0, 0, AllCorners, Alpha, TCornerType.InnerRound );
  end;
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    Caption           := name;
    Canvas.Fill.Color := TAlphaColors.Gray;
  end
  else
  begin
    Caption           := Text;
    Canvas.Fill.Color := TextColor;
  end;
  Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  CaptionRect        := ClipRect;
  PaintCaption( Self, Caption, CaptionRect, LineBreak, NumberOfLines, CTextAlign[FTextAlignment] );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) and ( FBackgroundImage = '' ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUILabel.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUILabel.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Image2: UIImage;
  Image1: UIImage;
  NData : NSData;
{$IFNDEF IOSDEVICE}
  transform: CGAffineTransform;
  context  : CGContextRef;
{$ENDIF}
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
  begin
    if FBackgroundImage <> '' then
    begin
      // Image2 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );
      Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );

      if not Assigned( Image1 ) or Assigned( Image1.CGImage ) or Assigned( Image1.CIImage ) then
      begin
        NData  := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( FBackgroundImage ) ) ) ) );
        Image1 := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NData ) );
      end;

      UIGraphicsBeginImageContext( FUILabel.frame.size );
{$IFNDEF IOSDEVICE}
      // ----------------------------------------------------------
      // Mirror on Simulator
      context   := UIGraphicsGetCurrentContext( );
      transform := CGAffineTransformMakeTranslation( 0.0, Height );
      transform := CGAffineTransformScale( transform, 1.0, -1.0 );
      CGContextConcatCTM( context, transform );
{$ENDIF}
      Image1.drawInRect( FUILabel.bounds );
      Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
      UIGraphicsEndImageContext( );

      FUILabel.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image2 ) ) );
    end
    else
      SetBackgroundColor( FBackgroundColor );
  end;
{$ENDIF}
end;

procedure TDPFLabel.SetBaseLineAdjustment( const Value: TDPFBaseLineAdjustment );
begin
  if FBaseLineAdjustment <> Value then
  begin
    FBaseLineAdjustment := Value;
{$IFDEF IOS}
    if Assigned( FUILabel ) then
      FUILabel.setBaselineAdjustment( Ord( Value ) );
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
    FUILabel.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetLineBreak( const Value: TDPFLineBreak );
begin
  FLineBreak := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
    FUILabel.setLineBreakMode( Integer( FLineBreak ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
  begin
    FUILabel.setNumberOfLines( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
  begin
    if FCapitalizedString then
      FUILabel.setText( NSStr( Value ).capitalizedString )
    else
      FUILabel.setText( NSStr( Value ) );
  end;
  if FAutoSizeToFit then
  begin
    sizeToFit;
  end;

{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF IOS}
  if Assigned( FUILabel ) then
  begin
    FUILabel.setTextAlignment( Integer( FTextAlignment ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFLabel.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF IOS}
  if FUILabel <> nil then
  begin
    if FTextColor <> TAlphaColors.Null then
      FUILabel.setTextColor( TColorToUIColor( FTextColor ) )
    else
      FUILabel.setTextColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFUILabel }

constructor TDPFUILabel.Create( ADPFLabel: TDPFLabel );
var
  V: Pointer;
begin
  inherited Create;
  FDPFLabel := ADPFLabel;

  V := UILabel( Super ).initWithFrame( CGRectMake( 0, 0, 100, 100 ) );
  // V := Super.init;
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFUILabel.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFUILabel );
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UILabel( Super ).touchesBegan( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  if Assigned( FDPFLabel.FOnTouchesBegan ) then
  begin
    Touch := TUITouch.Wrap( touches.anyObject );
    P     := Touch.locationInView( FDPFLabel.FUILabel );
    PrevP := Touch.previousLocationInView( FDPFLabel.FUILabel );
    FDPFLabel.FOnTouchesBegan( FDPFLabel, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UILabel( Super ).touchesMoved( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFLabel.FUILabel );
  PrevP := Touch.previousLocationInView( FDPFLabel.FUILabel );
  if Assigned( FDPFLabel.FOnTouchesMoved ) then
    FDPFLabel.FOnTouchesMoved( FDPFLabel, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UILabel( Super ).touchesEnded( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFLabel.FUILabel );
  PrevP := Touch.previousLocationInView( FDPFLabel.FUILabel );
  if Assigned( FDPFLabel.FOnTouchesEnded ) then
    FDPFLabel.FOnTouchesEnded( FDPFLabel, Touch.tapCount, DPFNSPoint( P ), DPFNSPoint( PrevP ) );

  if Touch.tapCount = 1 then
    NSObject( self.Super ).performSelector( sel_getUid( 'singleTap:' ), nil, FDPFLabel.TapDelay / 1000 )
  else if Touch.tapCount > 1 then
  begin
    iOSapi.{$IFDEF DELPHIXE7}Foundation{$ELSE}CocoaTypes{$ENDIF}.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget( Self.GetObjectID );
    NSObject( self.Super ).performSelector( sel_getUid( 'doubleTap:' ), nil, FDPFLabel.DoubleTapDelay / 1000 );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.singleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFLabel.FOnClick ) then
    FDPFLabel.FOnClick( FDPFLabel );
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.doubleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFLabel.FOnDoubleClick ) then
    FDPFLabel.FOnDoubleClick( FDPFLabel );
end;

// ------------------------------------------------------------------------------
procedure TDPFUILabel.drawTextInRect( rect: CGRect ); cdecl;
begin
  UILabel( super ).drawTextInRect( CGRectMake( rect.origin.x + FDPFLabel.Padding.Left, rect.origin.y + FDPFLabel.Padding.Top, rect.size.width - FDPFLabel.Padding.Right - FDPFLabel.Padding.Left, rect.size.height - FDPFLabel.Padding.Bottom - FDPFLabel.Padding.Top ) );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
