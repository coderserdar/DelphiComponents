// ------------------------------------------------------------------------------
// DPF.iOS.UIButton Component
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
unit DPF.iOS.UIButton;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFButton = class;

  // in iOS 7 : UIButtonTypeRoundedRect = UIButtonTypeSystem
  TDPFButtonType = ( btCustom, btRoundedRect, btDetailDisclosure, btInfoLight, btInfoDark, btContactAdd );

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFButtonDelegate = interface( NSObject )
    ['{C816C5FA-059F-4DA5-A410-6608DEC19C53}']

    procedure TouchDown; cdecl;
    procedure TouchUp; cdecl;
    procedure TouchDouble; cdecl;
    procedure TouchCancel; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFButtonDelegate = class( TOCLocal )
  private
    FDPFButton   : TDPFButton;
    isTouchCancel: Boolean;
  public
    constructor Create( Owner: TDPFButton );
    function GetObjectiveCClass: PTypeInfo; override;

    procedure TouchDown; cdecl;
    procedure TouchUp; cdecl;
    procedure TouchDouble; cdecl;
    procedure TouchCancel; cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFButton = class( TDPFiOSBaseControl )
  private
    FButtonType                 : TDPFButtonType;
    FText                       : string;
    FBackgroundImageNormal      : string;
    FBackgroundImageHighlight   : string;
    FBackgroundImageDisable     : string;
    FTextColorNormal            : TAlphaColor;
    FOnMouseDown                : TNotifyEvent;
    FOnClick                    : TNotifyEvent;
    FBackgroundColorNormal      : TAlphaColor;
    FTextAlpha                  : Single;
    FBackgroundColorHighlight   : TAlphaColor;
    FBackgroundColorSelected    : TAlphaColor;
    FBackgroundColorDisabled    : TAlphaColor;
    FTextColorHighlight         : TAlphaColor;
    FSizeToFit                  : Boolean;
    FShowsTouchWhenHighlighted  : Boolean;
    FSelected                   : Boolean;
    FHighlighted                : Boolean;
    FAdjustsImageWhenHighlighted: Boolean;
    FForgroundImageHighlight    : string;
    FForgroundImageDisable      : string;
    FForgroundImageNormal       : string;
    FTextLineBreak              : TDPFLineBreak;
    FContentHorizontalAlignment : TDPFContentHorizontalAlignment;
    FContentVerticalAlignment   : TDPFContentVerticalAlignment;
    FTintColor                  : TAlphaColor;
    FBackgroundImageSelected    : string;
    FForgroundImageSelected     : string;
    FEnabled                    : Boolean;
    FTextShadowColorNormal      : TAlphaColor;
    FTextShadowColorHighlight   : TAlphaColor;
    FOnDoubleClick              : TNotifyEvent;

    procedure SetText( const Value: string );
    procedure SetTextColorNormal( const Value: TAlphaColor );
    procedure SetTextAlpha( const Value: Single );

    procedure SetBackgroundColorNormal( const Value: TAlphaColor );
    procedure SetBackgroundColorHighlight( const Value: TAlphaColor );
    procedure SetBackgroundColorSelected( const Value: TAlphaColor );
    procedure SetBackgroundColorDisabled( const Value: TAlphaColor );

    procedure SetBackgroundImageNormal( const Value: string );
    procedure SetBackgroundImageHighlight( const Value: string );
    procedure SetBackgroundImageDisable( const Value: string );
    procedure SetBackgroundImageSelected( const Value: string );

    procedure SetHighlighted( const Value: Boolean );
    procedure SetSelected( const Value: Boolean );
    procedure SetShowsTouchWhenHighlighted( const Value: Boolean );
    procedure SetAdjustsImageWhenHighlighted( const Value: Boolean );
    procedure SetForgroundImageDisable( const Value: string );
    procedure SetForgroundImageHighlight( const Value: string );
    procedure SetForgroundImageNormal( const Value: string );
    procedure SetButtonType( const Value: TDPFButtonType );

    procedure OnFontChanged( Sender: TObject );
    procedure SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
    procedure SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
    procedure SetTintColor( const Value: TAlphaColor );
    procedure SetForgroundImageSelected( const Value: string );
    procedure SetEnabled( const Value: Boolean );
    procedure SetTextColorHighlight( const Value: TAlphaColor );
    procedure SetTextShadowColorHighlight( const Value: TAlphaColor );
    procedure SetTextShadowColorNormal( const Value: TAlphaColor );
  protected
{$IFDEF IOS}
    FUIButton      : UIButton;
    FButtonDelegate: TDPFButtonDelegate;
{$ENDIF}
{$IFNDEF IOS}
    procedure Click; override;
{$ENDIF}
    procedure SetBkImage( ImageName: string; ImageState: Integer );
    procedure SetFgImage( ImageName: string; ImageState: Integer );
    procedure SetBkColor( Color: TAlphaColor );
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;

    destructor Destroy; override;
    procedure SetHighlightedWithDelay( const Value: Boolean; const Delay: double = 0 );
    procedure SetSelectedWithDelay( const Value: Boolean; const Delay: double = 0 );
  published
    property ButtonType: TDPFButtonType read FButtonType write SetButtonType default btRoundedRect;
    property Text      : string read FText write SetText;
    property TintColor : TAlphaColor read FTintColor write SetTintColor default TAlphaColors.null;

    property TextColorNormal   : TAlphaColor read FTextColorNormal write SetTextColorNormal default TAlphaColors.Black;
    property TextColorHighlight: TAlphaColor read FTextColorHighlight write SetTextColorHighlight default TAlphaColors.White;

    // property TextShadowColorNormal   : TAlphaColor read FTextShadowColorNormal write SetTextShadowColorNormal default TAlphaColors.Null;
    // property TextShadowColorHighlight: TAlphaColor read FTextShadowColorHighlight write SetTextShadowColorHighlight default TAlphaColors.Null;

    property TextLineBreak: TDPFLineBreak read FTextLineBreak write FTextLineBreak default lbWordWrap;
    property TextAlpha    : Single read FTextAlpha write SetTextAlpha;

    property ContentHorizontalAlignment: TDPFContentHorizontalAlignment read FContentHorizontalAlignment write SetContentHorizontalAlignment default TDPFContentHorizontalAlignment.cchaCenter;
    property ContentVerticalAlignment  : TDPFContentVerticalAlignment read FContentVerticalAlignment write SetContentVerticalAlignment default TDPFContentVerticalAlignment.ccvaCenter;

    property BackgroundColorNormal   : TAlphaColor read FBackgroundColorNormal write SetBackgroundColorNormal default TAlphaColors.Null;
    property BackgroundColorHighlight: TAlphaColor read FBackgroundColorHighlight write SetBackgroundColorHighlight default TAlphaColors.Null;
    property BackgroundColorSelected : TAlphaColor read FBackgroundColorSelected write SetBackgroundColorSelected default TAlphaColors.Null;
    property BackgroundColorDisabled : TAlphaColor read FBackgroundColorDisabled write SetBackgroundColorDisabled default TAlphaColors.Null;

    property BackgroundImageNormal   : string read FBackgroundImageNormal write SetBackgroundImageNormal;
    property BackgroundImageHighlight: string read FBackgroundImageHighlight write SetBackgroundImageHighlight;
    property BackgroundImageSelected : string read FBackgroundImageSelected write SetBackgroundImageSelected;
    property BackgroundImageDisable  : string read FBackgroundImageDisable write SetBackgroundImageDisable;

    property ForgroundImageNormal   : string read FForgroundImageNormal write SetForgroundImageNormal;
    property ForgroundImageHighlight: string read FForgroundImageHighlight write SetForgroundImageHighlight;
    property ForgroundImageSelected : string read FForgroundImageSelected write SetForgroundImageSelected;
    property ForgroundImageDisable  : string read FForgroundImageDisable write SetForgroundImageDisable;

    property SizeToFit                  : Boolean read FSizeToFit write FSizeToFit default false;
    property Highlighted                : Boolean read FHighlighted write SetHighlighted default false;
    property Selected                   : Boolean read FSelected write SetSelected default false;
    property ShowsTouchWhenHighlighted  : Boolean read FShowsTouchWhenHighlighted write SetShowsTouchWhenHighlighted default true;
    property AdjustsImageWhenHighlighted: Boolean read FAdjustsImageWhenHighlighted write SetAdjustsImageWhenHighlighted default true;

    property Enabled: Boolean read FEnabled write SetEnabled default true;

    property OnMouseDown  : TNotifyEvent read FOnMouseDown write FOnMouseDown;
    property OnClick      : TNotifyEvent read FOnClick write FOnClick;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFButtonDelegate }
constructor TDPFButtonDelegate.Create( Owner: TDPFButton );
begin
  inherited Create;
  FDPFButton := Owner;
end;

// ------------------------------------------------------------------------------
function TDPFButtonDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFButtonDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFButtonDelegate.TouchDown;
begin
  isTouchCancel := false;
  if ( FDPFButton.ButtonType = btCustom ) then
  begin
    if FDPFButton.FBackgroundColorHighlight <> TAlphaColors.Null then
    begin
      FDPFButton.FUIButton.setBackgroundColor( TColorToUIColor( FDPFButton.FBackgroundColorHighlight ) );
      FDPFButton.FUIButton.setTitleColor( TColorToUIColor( FDPFButton.FTextColorHighlight ), 0 );
    end
    else
    begin
      FDPFButton.FUIButton.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
    end;
  end;

  if Assigned( FDPFButton.FOnMouseDown ) then
    FDPFButton.FOnMouseDown( FDPFButton );
end;

// ------------------------------------------------------------------------------
procedure TDPFButtonDelegate.TouchUp;
begin
  if ( FDPFButton.ButtonType = btCustom ) then
  begin
    if FDPFButton.FBackgroundColorNormal <> TAlphaColors.Null then
    begin
      FDPFButton.FUIButton.setBackgroundColor( TColorToUIColor( FDPFButton.FBackgroundColorNormal ) );
      FDPFButton.FUIButton.setTitleColor( TColorToUIColor( FDPFButton.FTextColorNormal ), 0 );
    end
    else
      FDPFButton.FUIButton.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;

  if not isTouchCancel and Assigned( FDPFButton.FOnClick ) then
    FDPFButton.FOnClick( FDPFButton );
end;

// ------------------------------------------------------------------------------
procedure TDPFButtonDelegate.TouchCancel;
begin
  isTouchCancel := true;
  TouchUp;
end;

// ------------------------------------------------------------------------------
procedure TDPFButtonDelegate.TouchDouble; cdecl;
begin
  if Assigned( FDPFButton.FOnDoubleClick ) then
    FDPFButton.FOnDoubleClick( FDPFButton );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFButton }
{$IFNDEF IOS}
procedure TDPFButton.Click; // SZ
begin
  inherited;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;
{$ENDIF}

constructor TDPFButton.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption            := 'Button';
  FEnabled                  := true;
  FTextAlpha                := 1;
  FTextLineBreak            := lbCharacterWrap;
  FButtonType               := btRoundedRect;
  FTextColorNormal          := TAlphaColors.Black;
  FTextColorHighlight       := TAlphaColors.White;
  FTextShadowColorNormal    := TAlphaColors.Null;
  FTextShadowColorHighlight := TAlphaColors.Null;

  FContentHorizontalAlignment := TDPFContentHorizontalAlignment.cchaCenter;
  FContentVerticalAlignment   := TDPFContentVerticalAlignment.ccvaCenter;

  FBackgroundColorNormal       := TAlphaColors.Null;
  FBackgroundColorHighlight    := TAlphaColors.Null;
  FBackgroundColorSelected     := TAlphaColors.Null;
  FAdjustsImageWhenHighlighted := true;

  ClipsToBounds              := false;
  FSizeToFit                 := false;
  FHighlighted               := false;
  FSelected                  := false;
  FShowsTouchWhenHighlighted := true;

  Font.OnChanged := OnFontChanged;
{$IFDEF IOS}
  FUIButton := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFButton.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FButtonDelegate ) then
  begin
    FUIButton.removeTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchDown' ),                         // action
      UIControlEventTouchDown );                         // event

    FUIButton.removeTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchUp' ),                           // action
      UIControlEventTouchUpInside + UIControlEventTouchUpOutside ); // event

    FUIButton.removeTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchCancel' ),                       // action
      UIControlEventTouchCancel );                       // event

    FUIButton.removeTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchDouble' ),                       // action
      UIControlEventTouchDownRepeat );                   // event

    FButtonDelegate.DisposeOf;
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFButton.Loaded;
begin
  if not Assigned( FUIButton ) then
  begin
    FUIButton := TUIButton.Wrap( TUIButton.OCClass.buttonWithType( NativeUInt( FButtonType ) ) );
    FUIButton.retain; // <--- Important For Release Button
    FUIButton.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
    FUIControl := FUIButton;

    FButtonDelegate := TDPFButtonDelegate.Create( Self );

    FUIButton.addTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchDown' ),                      // action
      UIControlEventTouchDown );                      // event

    FUIButton.addTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchUp' ),                        // action
      UIControlEventTouchUpInside + UIControlEventTouchUpOutside ); // event

    FUIButton.addTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchCancel' ),                    // action
      UIControlEventTouchCancel );                    // event

    FUIButton.addTarget( FButtonDelegate.GetObjectID, // target
      Sel_getUid( 'TouchDouble' ),                    // action
      UIControlEventTouchDownRepeat );                // event

  end;

  SetEnabled( FEnabled );
  SetText( FText );

  FUIButton.setLineBreakMode( Integer( FTextLineBreak ) );

  if FTextColorNormal <> TAlphaColors.Null then
    FUIButton.setTitleColor( TColorToUIColor( FTextColorNormal ), 0 );

  SetBackgroundImageNormal( FBackgroundImageNormal );
  SetBackgroundImageHighlight( FBackgroundImageHighlight );
  SetBackgroundImageDisable( FBackgroundImageDisable );

  SetForgroundImageNormal( FForgroundImageNormal );
  SetForgroundImageHighlight( FForgroundImageHighlight );
  SetForgroundImageDisable( FForgroundImageDisable );

  SetTextColorHighlight( FTextColorHighlight );
  SetTextColorNormal( FTextColorNormal );

  SetTextShadowColorNormal( FTextShadowColorNormal );
  SetTextShadowColorHighlight( FTextShadowColorHighlight );

  if FUIButton.isEnabled then
    SetBkColor( FBackgroundColorNormal )
  else
    SetBkColor( FBackgroundColorDisabled );

  SetHighlighted( FHighlighted );
  SetSelected( FSelected );
  SetShowsTouchWhenHighlighted( FShowsTouchWhenHighlighted );
  SetAdjustsImageWhenHighlighted( FAdjustsImageWhenHighlighted );
  SetTintColor( FTintColor );

  FUIButton.setFont( Font._UIFont );

  SetContentHorizontalAlignment( FContentHorizontalAlignment );
  SetContentVerticalAlignment( FContentVerticalAlignment );

  if FSizeToFit then
    FUIButton.sizeToFit;

  AddSubView( Self, ParentControl );

  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFButton.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if Assigned( FUIButton ) then FUIButton.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ELSE} // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFButton.Paint;
var
  Caption    : string;
  CaptionRect: TRectF;
  tw, iw     : single;

begin
  // Added by Fenistil

  Canvas.BeginScene;
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
    Caption := name
  else
    Caption := Text;
  case ButtonType of
    btCustom:
      begin
        if BackgroundColorNormal <> TAlphaColors.Null then
        begin
          Canvas.Fill.Kind  := TBrushKind.Solid;
          Canvas.Fill.Color := BackgroundColorNormal;
          Canvas.FillRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
        end;
        Canvas.Font.Size   := 15;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Fill.Color  := TextColorNormal;
        Canvas.Font.Family := 'Helvetica';
        CaptionRect        := ClipRect;
        PaintCaption( Self, Caption, CaptionRect, FTextLineBreak, 1, ContentAlignToText[FContentHorizontalAlignment] );
      end;
    btRoundedRect:
      begin
        BitmapAsBorder( Self, iOS_GUI_Bitmaps.Button.RoundedRect, 10, TAlphaColors.White );
        Canvas.Font.Size   := Font.FontSize;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Fill.Color  := TAlphaColors.Black;
        Canvas.Font.Family := 'Helvetica';
        CaptionRect        := ClipRect;
        PaintCaption( Self, Caption, CaptionRect, FTextLineBreak, 1, ContentAlignToText[FContentHorizontalAlignment] );
      end;
    btDetailDisclosure, btInfoLight, btInfoDark, btContactAdd:
      begin
        Canvas.Font.Size   := Font.FontSize;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Fill.Color  := TAlphaColors.Black;
        Canvas.Font.Family := 'Helvetica';
        tw                 := Canvas.TextWidth( Text );
        iw                 := 0; // To prevent compiler warning
        case ButtonType of
          btDetailDisclosure:
            iw := iOS_GUI_Bitmaps.Button.DetailDisclosure.Width;
          btInfoLight:
            iw := iOS_GUI_Bitmaps.Button.InfoLight.Width;
          btInfoDark:
            iw := iOS_GUI_Bitmaps.Button.InfoDark.Width;
          btContactAdd:
            iw := iOS_GUI_Bitmaps.Button.ContactAdd.Width;
        end;
        CaptionRect := ClipRect;
        if iw + tw < ClipRect.Width then
        begin
          CaptionRect.Left  := ClipRect.CenterPoint.X - ( iw + tw ) / 2;
          CaptionRect.Right := ClipRect.CenterPoint.X + ( iw + tw ) / 2;
        end;
        // Icon
        case ButtonType of
          btDetailDisclosure:
            BitmapToPosition( Self, iOS_GUI_Bitmaps.Button.DetailDisclosure, CaptionRect.Left, CaptionRect.CenterPoint.Y - iOS_GUI_Bitmaps.Button.DetailDisclosure.Height / 2 );
          btInfoLight:
            BitmapToPosition( Self, iOS_GUI_Bitmaps.Button.InfoLight, CaptionRect.Left, CaptionRect.CenterPoint.Y - iOS_GUI_Bitmaps.Button.InfoLight.Height / 2 );
          btInfoDark:
            BitmapToPosition( Self, iOS_GUI_Bitmaps.Button.InfoDark, CaptionRect.Left, CaptionRect.CenterPoint.Y - iOS_GUI_Bitmaps.Button.InfoDark.Height / 2 );
          btContactAdd:
            BitmapToPosition( Self, iOS_GUI_Bitmaps.Button.ContactAdd, CaptionRect.Left, CaptionRect.CenterPoint.Y - iOS_GUI_Bitmaps.Button.ContactAdd.Height / 2 );
        end;
        // Caption
        Canvas.Font.Size   := Font.FontSize;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Fill.Color  := TAlphaColors.Black;
        Canvas.Font.Family := 'Helvetica';
        Caption            := Text;
        CaptionRect.Left   := CaptionRect.Left + iw;
        PaintCaption( Self, Caption, CaptionRect, FTextLineBreak, 1, ContentAlignToText[FContentHorizontalAlignment] );
      end;
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTextAlpha( const Value: Single );
begin
  FTextAlpha := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setAlpha( FTextAlpha );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetAdjustsImageWhenHighlighted( const Value: Boolean );
begin
  FAdjustsImageWhenHighlighted := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
    FUIButton.setAdjustsImageWhenHighlighted( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundColorDisabled( const Value: TAlphaColor );
begin
  FBackgroundColorDisabled := Value;
  SetBkColor( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundColorHighlight( const Value: TAlphaColor );
begin
  FBackgroundColorHighlight := Value;
  SetBkColor( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundColorSelected( const Value: TAlphaColor );
begin
  FBackgroundColorSelected := Value;
  SetBkColor( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBkColor( Color: TAlphaColor );
begin
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if Color <> TAlphaColors.Null then
      FUIButton.setBackgroundColor( TColorToUIColor( Color ) )
    else
      FUIButton.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBkImage( ImageName: string; ImageState: Integer );
{$IFDEF IOS}
var
  Image: UIImage;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if ImageName <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( ImageName ) ) );
      FUIButton.setBackgroundImage( Image, ImageState );
    end
    else if Assigned( FUIButton.imageView ) and Assigned( FUIButton.imageView.image ) then
      FUIButton.setBackgroundImage( nil, ImageState );
  end

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetButtonType( const Value: TDPFButtonType );
begin
  FButtonType := Value;
{$IFNDEF IOS}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
begin
  FContentHorizontalAlignment := Value;
{$IFDEF IOS}
  if FUIButton <> nil then
  begin
    FUIButton.setContentHorizontalAlignment( Integer( Value ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
begin
  FContentVerticalAlignment := Value;
{$IFDEF IOS}
  if FUIButton <> nil then
  begin
    FUIButton.setContentVerticalAlignment( Integer( Value ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setEnabled( FEnabled );
    if not Value then
      SetBkColor( FBackgroundColorDisabled )
    else
    begin
      if FHighlighted then
        SetBkColor( FBackgroundColorHighlight )
      else if FSelected then
        SetBkColor( FBackgroundColorSelected )
      else
        SetBkColor( FBackgroundColorNormal );
    end;

  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetFgImage( ImageName: string; ImageState: Integer );
{$IFDEF IOS}
var
  Image: UIImage;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if ImageName <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( ImageName ) ) );
      FUIButton.setImage( Image, ImageState );
      if Assigned( FUIButton.imageView ) and ( ContentMode = vcmScaleToFill ) then
      begin
        FUIButton.imageView.setFrame( CGRectMake( 0, 0, FUIButton.frame.size.width, FUIButton.frame.size.height ) );
        FUIButton.imageView.setContentMode( Integer( ContentMode ) );
      end;
    end
    else if ( FUIButton.imageView <> nil ) and ( FUIButton.imageView.image <> nil ) then
      FUIButton.setImage( nil, ImageState );
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetForgroundImageDisable( const Value: string );
begin
  FForgroundImageDisable := Value;
{$IFDEF IOS}
  SetFgImage( Value, UIControlStateDisabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetForgroundImageHighlight( const Value: string );
begin
  FForgroundImageHighlight := Value;
{$IFDEF IOS}
  SetFgImage( Value, UIControlStateHighlighted );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetForgroundImageNormal( const Value: string );
begin
  FForgroundImageNormal := Value;
{$IFDEF IOS}
  SetFgImage( Value, UIControlStateNormal );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetForgroundImageSelected( const Value: string );
begin
  FForgroundImageSelected := Value;
{$IFDEF IOS}
  SetFgImage( Value, UIControlStateSelected );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetHighlightedWithDelay( const Value: Boolean; const Delay: double = 0 );
begin
  FHighlighted := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.performSelector( sel_getUid( 'setHighlighted:' ), ( TNSNumber.Wrap( TNSNumber.OCClass.numberWithBool( value ) ) as ILocalObject ).GetObjectID, Delay );
    if Value then
    begin
      SetBkImage( FBackgroundImageHighlight, UIControlStateNormal );
      SetFgImage( FForgroundImageHighlight, UIControlStateNormal );
    end
    else
    begin
      SetBkImage( FBackgroundImageHighlight, UIControlStateNormal );
      SetFgImage( FForgroundImageHighlight, UIControlStateNormal );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetHighlighted( const Value: Boolean );
begin
  FHighlighted := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setHighlighted( Value );
    SetEnabled( FEnabled );
    if Value then
    begin
      SetBkImage( FBackgroundImageHighlight, UIControlStateNormal );
      SetFgImage( FForgroundImageHighlight, UIControlStateNormal );
    end
    else
    begin
      SetBkImage( FBackgroundImageHighlight, UIControlStateNormal );
      SetFgImage( FForgroundImageHighlight, UIControlStateNormal );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetSelectedWithDelay( const Value: Boolean; const Delay: double = 0 );
begin
  FHighlighted := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.performSelector( sel_getUid( 'setSelected:' ), ( TNSNumber.Wrap( TNSNumber.OCClass.numberWithBool( value ) ) as ILocalObject ).GetObjectID, Delay );
    if Value then
    begin
      SetBkImage( FBackgroundImageSelected, UIControlStateNormal );
      SetFgImage( FForgroundImageSelected, UIControlStateNormal );
    end
    else
    begin
      SetBkImage( FBackgroundImageNormal, UIControlStateNormal );
      SetFgImage( FForgroundImageNormal, UIControlStateNormal );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetSelected( const Value: Boolean );
begin
  FSelected := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setSelected( Value );
    SetEnabled( FEnabled );
    if Value then
    begin
      SetBkImage( FBackgroundImageSelected, UIControlStateNormal );
      SetFgImage( FForgroundImageSelected, UIControlStateNormal );
    end
    else
    begin
      SetBkImage( FBackgroundImageNormal, UIControlStateNormal );
      SetFgImage( FForgroundImageNormal, UIControlStateNormal );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTextShadowColorHighlight( const Value: TAlphaColor );
begin
  FTextShadowColorHighlight := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if Value = TAlphaColors.Null then
      FUIButton.setTitleShadowColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ), UIControlStateHighlighted )
    else
      FUIButton.setTitleShadowColor( TColorToUIColor( Value ), UIControlStateHighlighted )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetShowsTouchWhenHighlighted( const Value: Boolean );
begin
  FShowsTouchWhenHighlighted := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
    FUIButton.setShowsTouchWhenHighlighted( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundColorNormal( const Value: TAlphaColor );
begin
  FBackgroundColorNormal := Value;
  SetBkColor( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundImageDisable( const Value: string );
begin
  FBackgroundImageDisable := Value;
{$IFDEF IOS}
  SetBkImage( Value, UIControlStateDisabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundImageHighlight( const Value: string );
begin
  FBackgroundImageHighlight := Value;
{$IFDEF IOS}
  SetBkImage( Value, UIControlStateHighlighted or UIControlStateSelected );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundImageNormal( const Value: string );
begin
  FBackgroundImageNormal := Value;
{$IFDEF IOS}
  SetBkImage( Value, UIControlStateNormal );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetBackgroundImageSelected( const Value: string );
begin
  FBackgroundImageSelected := Value;
{$IFDEF IOS}
  SetBkImage( Value, UIControlStateSelected );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetText( const Value: string );
begin
  FText          := Value;
  ControlCaption := FText;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setTitle( NSStr( FText ), 0 );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTextColorHighlight( const Value: TAlphaColor );
begin
  FTextColorHighlight := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setTitleColor( TColorToUIColor( Value ), UIControlStateHighlighted );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTextColorNormal( const Value: TAlphaColor );
begin
  FTextColorNormal := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    FUIButton.setTitleColor( TColorToUIColor( FTextColorNormal ), 0 );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTextShadowColorNormal( const Value: TAlphaColor );
begin
  FTextShadowColorNormal := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if Value = TAlphaColors.Null then
      FUIButton.setTitleShadowColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ), UIControlStateNormal )
    else
      FUIButton.setTitleShadowColor( TColorToUIColor( Value ), UIControlStateNormal )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.SetTintColor( const Value: TAlphaColor );
begin
  FTintColor := Value;
{$IFDEF IOS}
  if Assigned( FUIButton ) then
  begin
    if Integer( FButtonType ) > 1 then
      exit;

    if Value = TAlphaColors.Null then
      FUIButton.setTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUIButton.setTintColor( TColorToUIColor( Value ) )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFButton.OnFontChanged( Sender: TObject );
{$IFDEF IOS}
{$ENDIF}
begin
  inherited;
{$IFDEF IOS}
  if FUIButton <> nil then
  begin
    FUIButton.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
