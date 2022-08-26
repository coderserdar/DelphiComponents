// ------------------------------------------------------------------------------
// DPF.Android.BaseControl Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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
unit DPF.Android.BaseControl;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.TypInfo,
  System.Math,
{$IFDEF DELPHIXE6}
  System.Math.Vectors,
{$ENDIF}
{$IFDEF ANDROID}
  DPF.Android.Widget,
  Androidapi.JNI.Widget,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
  Androidapi.JNI.Webkit,

  Androidapi.JNI.JavaTypes,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.Log,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Consts,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs;

const
  DPFAbout   = 'D.P.F Delphi Android Native Components (by Babak Yaghoobi - b_yaghobi@yahoo.com / bayaghoobi@gmail.com)';
  DPFVersion = '2.8.6';

  GRAVITY_NO_GRAVITY                       = $00000000;
  GRAVITY_AXIS_X_SHIFT                     = $00000000;
  GRAVITY_AXIS_SPECIFIED                   = $00000001;
  GRAVITY_AXIS_PULL_BEFORE                 = $00000002;
  GRAVITY_AXIS_PULL_AFTER                  = $00000004;
  GRAVITY_AXIS_Y_SHIFT                     = $00000004;
  GRAVITY_RIGHT                            = $00000005;
  GRAVITY_END                              = $00800005;
  GRAVITY_FILL                             = $00000077;
  GRAVITY_START                            = $00800003;
  GRAVITY_TOP                              = $00000030;
  GRAVITY_LEFT                             = $00000003;
  GRAVITY_BOTTOM                           = $00000050;
  GRAVITY_CENTER                           = $00000011;
  GRAVITY_CENTER_VERTICAL                  = $00000010;
  GRAVITY_CENTER_HORIZONTAL                = $00000001;
  GRAVITY_FILL_VERTICAL                    = $00000070;
  GRAVITY_AXIS_CLIP                        = $00000008;
  GRAVITY_CLIP_HORIZONTAL                  = $00000008;
  GRAVITY_CLIP_VERTICAL                    = $00000080;
  GRAVITY_DISPLAY_CLIP_HORIZONTAL          = $01000000;
  GRAVITY_DISPLAY_CLIP_VERTICAL            = $10000000;
  GRAVITY_FILL_HORIZONTAL                  = $00000007;
  GRAVITY_HORIZONTAL_GRAVITY_MASK          = $00000007;
  GRAVITY_VERTICAL_GRAVITY_MASK            = $00000070;
  GRAVITY_RELATIVE_HORIZONTAL_GRAVITY_MASK = $00800007;
  GRAVITY_RELATIVE_LAYOUT_DIRECTION        = $00800000;

type
  TDPFANDBaseControl = class;

{$IFDEF ANDROID}

  TDPFJViewOnClickListener = class( TJavaLocal, JView_OnClickListener )
  private
    FDPFANDBaseControl: TDPFANDBaseControl;
  public
    constructor create( ADPFANDBaseControl: TDPFANDBaseControl );
    procedure onClick( P1: JView ); cdecl;
  end;

  TDPFJViewOnKeyListener = class( TJavaLocal, JView_OnKeyListener )
  private
    FDPFANDBaseControl: TDPFANDBaseControl;
  public
    constructor create( ADPFANDBaseControl: TDPFANDBaseControl );
    function onKey( v: JView; keyCode: Integer; event: JKeyEvent ): Boolean; cdecl;
  end;

{$ENDIF}

  DPFNSPoint = record
    x: Single;
    y: Single;
  end;

  DPFNSize = record
    width: Single;
    height: Single;
  end;

  DPFNSRect = record
    origin: DPFNSPoint;
    size: DPFNSize;
  end;

  TDPFTextAlignment = ( taAuto = 0, taLeft = 1, taCenter = 2, taRight = 3, taBottom = 4, taCenter_Horizontal = 5, taCenter_Vertical = 6, taClip_Horizontal = 7, taClip_Vertical = 8, taFill = 9, taFill_Horizontal = 10, taFill_Vertical = 11, taTop = 12 );
  TDPFLineBreak     = ( lbWordWrap, lbCharacterWrap, lbClip, lbHeadTruncation, lbTailTruncation, lbMiddleTruncation );
  TDPFLayoutType    = ( lpWRAP_CONTENT = -2, lpMATCH_PARENT = -1, lpNone = 0 );

  TLayoutParamsType = ( ptFrameLayout, ptJLinearLayout, ptAbsoluteLayout, ptRelativeLayout );

  TDPFFormChanging = procedure( Sender: TObject; OldForm: TForm; NewForm: TForm; var CanChange: Boolean ) of object;
  TDPFFormChanged  = procedure( Sender: TObject; OldForm: TForm; NewForm: TForm ) of object;

  TDPFFrame = class of TFrame;

  TDPFFrameChanging = procedure( Sender: TObject; OldFrame: TDPFFrame; NewFrame: TDPFFrame; var CanChange: Boolean ) of object;
  TDPFFrameChanged  = procedure( Sender: TObject; OldFrame: TDPFFrame; NewFrame: TDPFFrame ) of object;

  TDPFOnKey           = procedure( Sender: TObject; KeyCode: Integer; var isDispatchKeyEvent: Boolean ) of object;
  TDPFOnDoubleClicked = procedure( Sender: TObject ) of object;

  TDPFDrawType = ( dtStandard, dtCustom );
  TDPFOnClick  = procedure( Sender: TObject ) of object;

  TDPFGradientOrientation = ( BL_TR, BOTTOM_TOP, BR_TL, LEFT_RIGHT, RIGHT_LEFT, TL_BR, TOP_BOTTOM, TR_BL );

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFANDBaseControl = class( TControl )
  private
    FDestroying      : Boolean;
    FBorderColor     : TAlphaColor;
    FTagBoolean      : Boolean;
    FEnabled         : Boolean;
    FTagDateTime     : TDateTime;
    FOldPositionEvent: TNotifyEvent;
    // FFont: TDPFFont;
{$IFDEF ANDROID}
    FDPFJViewOnClickListener     : TDPFJViewOnClickListener;
    FDPFJViewOnKeyListener       : TDPFJViewOnKeyListener;
    FScreenScale                 : Single;
    FScreenDensityDPI            : Single;
    FBounds                      : TRect;
    FRealBounds                  : TRect;
    FJControl                    : JView;
    FParentJControl              : JView;
    FJNativeLayout               : JNativeLayout;
    FJRelativeLayout_LayoutParams: JRelativeLayout_LayoutParams;
{$ENDIF}
    FBorderCornerRadius  : Single;
    FLockComponent       : Boolean;
    FVisible             : Boolean;
    FAlpha               : Single;
    FisLoaded            : Boolean;
    FOriginParent        : TDPFANDBaseControl;
    FClickable           : Boolean;
    FBorderWidth         : Integer;
    FOnClick             : TDPFOnClick;
    FBackgroundImage     : string;
    FBackgroundColor1    : TAlphaColor;
    FBackgroundColor2    : TAlphaColor;
    FBackgroundColor3    : TAlphaColor;
    FFocusable           : Boolean;
    FFocusableInTouchMode: Boolean;
    FGradientOrientation : TDPFGradientOrientation;
    FDrawType            : TDPFDrawType;
    FClipChildren        : Boolean;
    FOnKey               : TDPFOnKey;
    FJID                 : Integer;
    FLayoutWidth         : TDPFLayoutType;
    FLayoutHeight        : TDPFLayoutType;

    procedure PositionOnChange( Sender: TObject );
    procedure ScaleOnChange( Sender: TObject );
    procedure SetAlpha( const Value: Single );

    procedure SetBorderColor( const Value: TAlphaColor );
    procedure SetBorderWidth( const Value: Integer );

    procedure SetLockComponent( const Value: Boolean );
    procedure SetClickable( const Value: Boolean );
    procedure SetOnClick( const Value: TDPFOnClick );
    procedure SetBackgroundColor1( const Value: TAlphaColor );
    procedure SetBackgroundColor2( const Value: TAlphaColor );
    procedure SetBackgroundColor3( const Value: TAlphaColor );
    procedure SetBackgroundImage( const Value: string );
    procedure SetFocusable( const Value: Boolean );
    procedure SetFocusableInTouchMode( const Value: Boolean );
    procedure DrawBackground;
    procedure SetBorderCornerRadius( const Value: Single );
    procedure SetGradientOrientation( const Value: TDPFGradientOrientation );
    procedure SetOnKey( const Value: TDPFOnKey );
    procedure SetJID( const Value: Integer );
{$IFDEF ANDROID}
    procedure SetJControl( const value: JView ); overload;
{$ENDIF}
  protected
{$IFDEF ANDROID}
    procedure UpdateContentFromControl;
    procedure CalcRealBorder;
    function GetScreenScale: Single;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
    procedure InternalPaint( Text: string; TextColor: TAlphaColor; TextAlign: TDPFTextAlignment; BackgroundColor: TAlphaColor );
{$IFNDEF ANDROID}
    procedure Paint; override;
    procedure Invalidate;
{$ENDIF}
  public
{$IFDEF ANDROID}
{$ENDIF}
    AddThisToSubView: Boolean;
    AddSubViewToThis: Boolean;
    IsAutoResize    : Boolean;
    LayoutParamsType: TLayoutParamsType;
    ControlCaption  : string;
    procedure Loaded; override;
{$IFDEF DELPHIXE7}
    procedure ParentChanged; override;
{$ELSE}
    procedure ChangeParent; override;
{$ENDIF}
{$IFDEF ANDROID}
    property JControl: JView read FJControl write SetJControl;
    property JRelativeLayout_LayoutParams: JRelativeLayout_LayoutParams read FJRelativeLayout_LayoutParams write FJRelativeLayout_LayoutParams;

    procedure AddSubView( ThisControl: TDPFANDBaseControl; ToParentControl: TControl; ViewAdded: JView = nil );
    procedure clearAllSubviews( Control: JObject = nil; isRelease: Boolean = false );
    function FindViewById( id: Integer ): JView;
{$ENDIF}
    procedure BringToFront; override;
    procedure SetFocus;
    procedure DoRealign; override;
    procedure SetEnabled( const Value: Boolean ); override;
    procedure SetVisible( const Value: Boolean ); override;

    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; virtual;

    property OriginParent: TDPFANDBaseControl read FOriginParent;
    property isLoaded: Boolean read FisLoaded;
    property TagDateTime: TDateTime read FTagDateTime write FTagDateTime;
{$IFDEF ANDROID}
    property ScreenScale: Single read FScreenScale;
{$ENDIF}
  published
    property Alpha               : Single read FAlpha write SetAlpha;
    property Visible             : Boolean read FVisible write SetVisible default true;
    property Enabled             : Boolean read FEnabled write SetEnabled default true;
    property Clickable           : Boolean read FClickable write SetClickable default true;
    property Focusable           : Boolean read FFocusable write SetFocusable default true;
    property FocusableInTouchMode: Boolean read FFocusableInTouchMode write SetFocusableInTouchMode default true;
    property BackgroundColor1    : TAlphaColor read FBackgroundColor1 write SetBackgroundColor1 default TAlphaColors.Null;
    property BackgroundColor2    : TAlphaColor read FBackgroundColor2 write SetBackgroundColor2 default TAlphaColors.Null;
    property BackgroundColor3    : TAlphaColor read FBackgroundColor3 write SetBackgroundColor3 default TAlphaColors.Null;
    property BackgroundImage     : string read FBackgroundImage write SetBackgroundImage;

    property LockComponent: Boolean read FLockComponent write SetLockComponent default false;

    property JID: Integer read FJID write SetJID default 0;

    property LayoutWidth : TDPFLayoutType read FLayoutWidth write FLayoutWidth default lpNone;
    property LayoutHeight: TDPFLayoutType read FLayoutHeight write FLayoutHeight default lpNone;

    property BorderWidth       : Integer read FBorderWidth write SetBorderWidth default 0;
    property BorderColor       : TAlphaColor read FBorderColor write SetBorderColor default TAlphaColors.Null;
    property BorderCornerRadius: Single read FBorderCornerRadius write SetBorderCornerRadius;

    property GradientOrientation: TDPFGradientOrientation read FGradientOrientation write SetGradientOrientation default TOP_BOTTOM;

    property TagBoolean: Boolean read FTagBoolean write FTagBoolean default true;
    property OnClick   : TDPFOnClick read FOnClick write SetOnClick;
    property OnKey     : TDPFOnKey read FOnKey write SetOnKey;
    property DrawType  : TDPFDrawType read FDrawType write FDrawType default dtStandard;

    property Scale;
    property Anchors;
    property Margins;
    property Align;
    property Position;
    property Width;
    property Height;
  end;

procedure MoveChildsToOriginParent( Control: TDPFANDBaseControl; removeFromSuperViw: Boolean = true );

function MakeDPFNSPoint( x: Single; y: Single ): DPFNSPoint;

const
  CTextAlign: array [TDPFTextAlignment] of TTextAlign = ( TTextAlign.taLeading, TTextAlign.taLeading, TTextAlign.taCenter, TTextAlign.taTrailing, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter, TTextAlign.taCenter );
  TDPFTextAlignmentToGravity: array [TDPFTextAlignment] of integer = ( 0, 3, 17, 5, 80, 1, 16, 8, 128, 119, 7, 112, 48 );
  TDPFAlignToGravity: array [TAlignLayout] of integer = ( $00, $30, $03, $05, $50, $30, $50, $03, $05, $77, $77, $11, $10, $01, $07, $70, $00, $77, $00800003, $00800005 );

procedure DPFNSLog( LogMsg: string );

implementation

procedure DPFNSLog( LogMsg: string );
{$IFDEF ANDROID}
var
  M: TMarshaller;
{$ENDIF}
begin
{$IFDEF ANDROID}
  LOGI( M.AsAnsi( GetApplicationTitle + LogMsg ).ToPointer );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function MakeDPFNSPoint( x: Single; y: Single ): DPFNSPoint;
begin
  result.x := x;
  result.y := y;
end;

// ------------------------------------------------------------------------------
procedure MoveChildsToOriginParent( Control: TDPFANDBaseControl; removeFromSuperViw: Boolean = true );
var
  I: Integer;
  C: TDPFANDBaseControl;
begin
  I := 0;
  while I < Control.ChildrenCount do
  begin
    if Control.Children[I] is TDPFANDBaseControl then
    begin
      C := Control.Children[I] as TDPFANDBaseControl;
      if C.Parent <> C.OriginParent then
      begin
        C.Parent := C.OriginParent;
        C.Loaded;
        i := 0;
        Continue;
      end;
    end;
    inc( i );
  end;
end;

{$IFDEF ANDROID}
// ------------------------------------------------------------------------------
{ TJButtonClickListener }

constructor TDPFJViewOnClickListener.create( ADPFANDBaseControl: TDPFANDBaseControl );
begin
  inherited create;
  FDPFANDBaseControl := ADPFANDBaseControl;
end;

// ------------------------------------------------------------------------------
procedure TDPFJViewOnClickListener.onClick( P1: JView ); cdecl;
begin
  if Assigned( FDPFANDBaseControl.FOnClick ) then
    FDPFANDBaseControl.FOnClick( FDPFANDBaseControl );
end;

// ------------------------------------------------------------------------------
constructor TDPFJViewOnKeyListener.create( ADPFANDBaseControl: TDPFANDBaseControl );
begin
  inherited create;
  FDPFANDBaseControl := ADPFANDBaseControl;
end;

// ------------------------------------------------------------------------------
function TDPFJViewOnKeyListener.onKey( v: JView; keyCode: Integer; event: JKeyEvent ): Boolean; cdecl;
var
  isDispatchKeyEvent: Boolean;
begin
  isDispatchKeyEvent := true;
  if Assigned( FDPFANDBaseControl.FOnKey ) then
    FDPFANDBaseControl.FOnKey( FDPFANDBaseControl, keyCode, isDispatchKeyEvent );
  if isDispatchKeyEvent then
    SharedActivity.dispatchKeyEvent( event );

  // If result = true then Keyboard Delete key not work !!!!!
  result := false;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.AddSubView( ThisControl: TDPFANDBaseControl; ToParentControl: TControl; ViewAdded: JView = nil );
var
  C: JObject;
  // Flag: Boolean;
  V: JView;
  // J   : JView;
  // LP  : JViewGroup_LayoutParams;
  // bType, bFlag, bFormat: Integer;
begin
  if not AddThisToSubView then
    exit;

  if ( ThisControl = nil ) or ( ThisControl.FJControl = nil ) then
    exit;

  if ViewAdded = nil then
    V := ThisControl.FJControl
  else
    V := ViewAdded;

  if ( ToParentControl <> nil ) and ( ToParentControl is TDPFANDBaseControl ) then
  begin
    if not( ToParentControl as TDPFANDBaseControl ).AddSubViewToThis then
      Exit;

    if not Assigned( ( ToParentControl as TDPFANDBaseControl ).FJControl ) and not( ToParentControl as TDPFANDBaseControl ).isLoaded then
      ( ToParentControl as TDPFANDBaseControl ).Loaded;

    C := ( ToParentControl as TDPFANDBaseControl ).FJControl;

    if not Assigned( C ) then
    begin
      ShowMessage( 'Android parent control (FJControl) not created: ' + ToParentControl.Name );
      exit;
    end;

    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FParentJControl := ( ToParentControl as TDPFANDBaseControl ).FJControl;
        JViewGroup( FParentJControl ).addView( FJControl );
        Resize;
      end );
  end
  else
  begin
    if Assigned( Owner ) then
      if Owner.InheritsFrom( TFrame ) then
        exit;

    if Assigned( Parent ) then
      if Parent.InheritsFrom( TFrame ) then
        exit;

    C := ( ThisControl as TDPFANDBaseControl ).FJControl;
    if Assigned( Owner ) and Assigned( TCommonCustomForm( Owner ).Handle ) then
    begin

      CallInUIThreadAndWaitFinishing(
        procedure
        begin
          FJNativeLayout := TJNativeLayout.JavaClass.init( SharedActivity, MainActivity.getTextEditorProxy.getWindowToken );
          FJNativeLayout.SetPosition( 100, 100 );
          FJNativeLayout.SetSize( 300, 300 );
          FJNativeLayout.SetControl( FJControl );
          FJNativeLayout.SetFocus( true );

          // bType := FJNativeLayout.&type;    // default 1002
          // 0bFlag := FJNativeLayout.flags;    // default 393736
          // bFormat := FJNativeLayout.format; // default -3


          // FJNativeLayout.&type := 1000;
          // FJNativeLayout.flags := 0 { 268435456 };
          // FJNativeLayout.format := 67108864;

          Resize;
        end );

      DPFNSLog( 'TDPFANDBaseControl.AddSubView(Handle): ' + name );
    end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.BringToFront;
begin
  inherited BringToFront;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if Assigned( FJControl ) then
          FJControl.bringToFront;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF DELPHIXE7}

procedure TDPFANDBaseControl.ParentChanged;
{$ELSE}

procedure TDPFANDBaseControl.ChangeParent;
{$ENDIF}
var
  Flag: Boolean;
  S   : string;
begin
{$IFDEF ANDROID}
  DPFNSLog( 'TDPFANDBaseControl.ChangeParent : ' + name );
{$ENDIF}
  inherited;
  if FDestroying then
    exit;

  Flag := False;
  if Assigned( Owner ) then
  begin
    Flag := Owner.InheritsFrom( TFrame );
    S    := Owner.Name;
  end;

  if Flag then
    exit;

  if Assigned( Parent ) then
  begin
    Flag := Parent.InheritsFrom( TFrame );
    S    := Parent.Name;
  end;

  if Flag then
    exit;

{$IFDEF ANDROID}
  FParentJControl := nil;
  if Assigned( Parent ) and ( Parent is TDPFANDBaseControl ) then
    FParentJControl := ( Parent as TDPFANDBaseControl ).FJControl
  else if Assigned( Owner ) and ( Owner is TDPFANDBaseControl ) then
    FParentJControl := ( Owner as TDPFANDBaseControl ).FJControl;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFANDBaseControl.Create( AOwner: TComponent );
begin
{$IFDEF DEBUG}
  DPFNSLog( 'TDPFANDBaseControl.Create: ' + ClassName );
{$ENDIF}
  inherited Create( AOwner );
  FLayoutWidth          := lpNone;
  FLayoutHeight         := lpNone;
  FJID                  := 0;
  IsAutoResize          := false;
  ClipChildren          := true;
  FBackgroundColor1     := TAlphaColors.Null;
  FBackgroundColor2     := TAlphaColors.Null;
  FBackgroundColor3     := TAlphaColors.Null;
  FisLoaded             := False;
  FOriginParent         := TDPFANDBaseControl( AOwner );
  AddThisToSubView      := true;
  AddSubViewToThis      := true;
  ControlCaption        := 'BaseControl';
  FAlpha                := 1;
  FOldPositionEvent     := Position.OnChange;
  Position.OnChange     := PositionOnChange;
  Scale.OnChange        := ScaleOnChange;
  FVisible              := true;
  FEnabled              := true;
  FClickable            := true;
  FFocusable            := true;
  FFocusableInTouchMode := true;
  // FFont                   := TDPFFont.Create;
  FVisible             := true;
  FTagBoolean          := true;
  FBorderWidth         := 0;
  FBorderCornerRadius  := 0;
  FBorderColor         := TAlphaColors.Null;
  FGradientOrientation := TOP_BOTTOM;
  FDrawType            := dtStandard;
  FClipChildren        := true;

  FDestroying := false;
{$IFDEF ANDROID}
  FScreenScale := 0.0;
  GetScreenScale;
  CalcRealBorder;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFANDBaseControl.Destroy;
begin
  FDestroying := true;
{$IFDEF ANDROID}
  DPFNSLog( 'TDPFANDBaseControl.Destroy: ' + ClassName + ' Name: ' + name + ' Unit: ' + Self.UnitName );

  if not Assigned( FDPFJViewOnKeyListener ) then
    FDPFJViewOnKeyListener := nil;

  if Assigned( FJControl ) then
  begin
    FJControl.setOnKeyListener( nil );
    FJControl := nil;
  end;

  if assigned( FJNativeLayout ) then
    FJNativeLayout := nil;

{$ENDIF}
  inherited;
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
function TDPFANDBaseControl.findViewById( id: Integer ): JView;
begin
  result := JControl.findViewById( id );
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.clearAllSubviews( Control: JObject = nil; isRelease: Boolean = false );
begin
  DPFNSLog( 'TDPFANDBaseControl.clearAllSubviews' + name );
  if Control = nil then
    Control := FJControl;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.InternalPaint( Text: string; TextColor: TAlphaColor; TextAlign: TDPFTextAlignment; BackgroundColor: TAlphaColor );
{$IFNDEF ANDROID}
var
  R: TRectF;
begin
  Canvas.BeginScene;
  // Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  // Canvas.Font.Size   := Font.FontSize;
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Kind  := TBrushKind.bkSolid;
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, 0.5, 0.5, AllCorners, Alpha, TCornerType.ctInnerRound );
  end;
  // Canvas.DrawRect( ClipRect, 0, 0, [], 1 );
  // ---
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  if ( Text = '' ) and ( csDesigning in ComponentState ) and ( ChildrenCount = 0 ) then
  begin
    Canvas.Fill.Color := TAlphaColors.gray;
    Text              := name;
  end
  else
  begin
    Canvas.Fill.Color := TextColor;
  end;

  Canvas.Stroke.Color     := $B2005ACC;
  Canvas.Stroke.Cap       := TStrokeCap.scRound;
  Canvas.Stroke.Join      := TStrokeJoin.sjRound;
  Canvas.Stroke.Dash      := TStrokeDash.sdSolid;
  Canvas.Stroke.Thickness := 1;
  R                       := LocalRect;
  InflateRect( R, -0.5, -0.5 );
  Canvas.DrawRect( R, 0.5, 0.5, AllCorners, Alpha, TCornerType.ctInnerRound );
  // ---

  Canvas.FillText( ClipRect, Text, true, Alpha, [], CTextAlign[TextAlign], TTextAlign.taCenter );
  Canvas.EndScene;
end;

{$ELSE}

begin
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.Loaded;
begin
{$IFDEF ANDROID}
  DPFNSLog( 'TDPFANDBaseControl.Loaded: ' + ClassName + ' Name: ' + name + ' Unit: ' + Self.UnitName );
{$ENDIF}
  FisLoaded := true;
  inherited;
{$IFDEF ANDROID}
  SetBorderWidth( FBorderWidth );
  SetBorderColor( FBorderColor );

  SetAlpha( FAlpha );
  SetVisible( FVisible );
  SetClickable( FClickable );
  SetFocusable( FFocusable );
  SetBackgroundImage( FBackgroundImage );
  SetFocusableInTouchMode( FFocusableInTouchMode );
  DrawBackground;

  if not Assigned( FDPFJViewOnKeyListener ) then
    FDPFJViewOnKeyListener := TDPFJViewOnKeyListener.create( self );
  FJControl.setOnKeyListener( FDPFJViewOnKeyListener );

{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFANDBaseControl.Paint;
begin
  inherited Paint;
  InternalPaint( ControlCaption, TAlphaColors.Black, TDPFTextAlignment.taCenter, TAlphaColors.White );
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.Invalidate;
begin
  InvalidateRect( RectF( 0, 0, width, height ) );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.PositionOnChange( Sender: TObject );
begin
  if Assigned( FOldPositionEvent ) then
    FOldPositionEvent( Sender );
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.ScaleOnChange( Sender: TObject );
begin
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure
      begin
        if FJControl <> nil then
        begin
          FJControl.setScaleX( Scale.Point.X );
          FJControl.setScaleY( Scale.Point.Y );
        end;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.RefreshNeeded;
begin
  // Must Be Empty !!!
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.Resize;
var
  x: single;
  n: string;
begin
{$IFDEF ANDROID}
  DPFNSLog( 'TDPFANDBaseControl.Resize: ' + ClassName + ' Name: ' + name );
{$ENDIF}
  inherited Resize;
{$IFDEF ANDROID}
  if IsAutoResize then
    exit;
  if Assigned( FJNativeLayout ) then
  begin
    CallInUIThread(
      procedure
      begin
        UpdateContentFromControl;
        n := name;
        x := Position.y * FScreenScale;
        x := Top;
        x := FBounds.Top;
        FJControl.setX( Left - FBounds.Left / FScreenScale );
        FJControl.setY( Top - FBounds.Top / FScreenScale);
        FJNativeLayout.SetPosition( FBounds.Left, FBounds.Top );
        FJNativeLayout.SetSize( FBounds.Right, FBounds.Bottom );
      end );
  end
  else if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if not assigned( FJRelativeLayout_LayoutParams ) then
        begin
          FJRelativeLayout_LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init( Integer( FLayoutWidth ), Integer( FLayoutHeight ) );
          // FJControl.setLayoutParams( FJRelativeLayout_LayoutParams );
        end;
        FJControl.setX( Position.x * FScreenScale );
        FJControl.setY( Position.y * FScreenScale );

        n := name;
        x := Position.y * FScreenScale;

        // FJRelativeLayout_LayoutParams.leftMargin := round( Position.x * FScreenScale );
        // FJRelativeLayout_LayoutParams.topMargin := round( Position.y * FScreenScale );

        if LayoutWidth <> lpNone then
          FJRelativeLayout_LayoutParams.width := Integer( LayoutWidth )
        else
          FJRelativeLayout_LayoutParams.width := round( Width * FScreenScale );

        if LayoutHeight <> lpNone then
          FJRelativeLayout_LayoutParams.height := integer( LayoutHeight )
        else
          FJRelativeLayout_LayoutParams.height := round( Height * FScreenScale );

        FJControl.setLayoutParams( FJRelativeLayout_LayoutParams );
      end );
  end;

{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetAlpha( const Value: Single );
begin
  if ( FAlpha > 1 ) or ( FAlpha < 0 ) then
    exit;
  FAlpha  := Value;
  Opacity := FAlpha;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure
      begin
        if FJControl <> nil then
          FJControl.setAlpha( FAlpha );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBorderCornerRadius( const Value: Single );
begin
  FBorderCornerRadius := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.DoRealign;
begin
  inherited;
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetJControl( const Value: JView );
begin
  FJControl := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.CalcRealBorder;
var
  NativeWin  : JWindow;
  DecorView  : JView;
  ContentRect: JRect;
begin
  NativeWin := SharedActivity.getWindow;
  if Assigned( NativeWin ) then
  begin
    ContentRect := TJRect.Create;
    DecorView   := NativeWin.getDecorView;
    DecorView.getWindowVisibleDisplayFrame( ContentRect );
    FRealBounds := Rect( ContentRect.left, Round( ContentRect.top / FScreenScale ), ContentRect.right, ContentRect.bottom );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.UpdateContentFromControl;
var
  Pos: TPointF;
begin
  if Assigned( FJNativeLayout ) then
  begin
    if Parent is TCommonCustomForm then
      Pos := {FRealBounds.TopLeft + }Position.Point
    else
      Pos   := {$IFDEF DELPHIXE7}(Parent as IControl){$ELSE}Parent.AsIControl{$ENDIF}.LocalToScreen( Position.Point );

    FBounds := Rect( Round( Pos.X * FScreenScale ), Round( Pos.Y * FScreenScale ), Round( Width * FScreenScale ), Round( Height * FScreenScale ) );
{$IFDEF DELPHIXE6}
//    FBounds.Top  := 0;
//    FBounds.Left := 0;
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------------------
function TDPFANDBaseControl.GetScreenScale: Single;
const
  // Default values taken from Android SDK reference:
  // http://developer.Android.com/reference/Android/util/DisplayMetrics.html#density
  DefaultDensityScale = 1;
  DefaultDensityDPI   = 160;
var
  Metrics: JDisplayMetrics;
begin
  if SameValue( FScreenScale, 0, TEpsilon.Scale ) then
  begin
    Metrics := GetJDisplayMetrics;
    if Assigned( Metrics ) then
    begin
      FScreenScale      := Metrics.density;    // API level 1
      FScreenDensityDPI := Metrics.densityDpi; // API level 4
    end
    else
    begin
      FScreenScale      := DefaultDensityScale;
      FScreenDensityDPI := DefaultDensityDPI;
    end;

  end;

  Result := FScreenScale;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.DrawBackground;
{$IFDEF ANDROID}
{$ENDIF}
begin
  if FDrawType = dtStandard then
    exit;

{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin

    CallInUIThread(
      procedure
      var
        GColor: TJavaArray<Integer>;
        GO: JGradientDrawable_Orientation;
        N: Byte;
        Idx: Integer;
        FBackgroundDrawable: JGradientDrawable;
      begin

        N := 0;
        if BackgroundColor1 <> TAlphaColors.Null then
          Inc( N );
        if BackgroundColor2 <> TAlphaColors.Null then
          Inc( N );
        if BackgroundColor3 <> TAlphaColors.Null then
          Inc( N );
        if ( FBorderWidth > 0 ) or ( FBorderCornerRadius > 0 ) or ( N > 1 ) then
        begin
          FJControl.setBackgroundDrawable( nil );
          FBackgroundDrawable := TJGradientDrawable.JavaClass.init;

          Idx := 0;
          if N > 1 then
          begin
            GColor := TJavaArray<Integer>.Create( N );
            if BackgroundColor1 <> TAlphaColors.Null then
            begin
              GColor[Idx] := BackgroundColor1;
              Inc( Idx );
            end;
            if BackgroundColor2 <> TAlphaColors.Null then
            begin
              GColor[Idx] := BackgroundColor2;
              Inc( Idx );
            end;
            if BackgroundColor3 <> TAlphaColors.Null then
            begin
              GColor[Idx] := BackgroundColor3;
            end;
            FBackgroundDrawable.setColors( GColor );
          end;
          case FGradientOrientation of
            BL_TR:
              GO := TJGradientDrawable_Orientation.JavaClass.BL_TR;
            BOTTOM_TOP:
              GO := TJGradientDrawable_Orientation.JavaClass.BOTTOM_TOP;
            BR_TL:
              GO := TJGradientDrawable_Orientation.JavaClass.BR_TL;
            LEFT_RIGHT:
              GO := TJGradientDrawable_Orientation.JavaClass.LEFT_RIGHT;
            RIGHT_LEFT:
              GO := TJGradientDrawable_Orientation.JavaClass.RIGHT_LEFT;
            TL_BR:
              GO := TJGradientDrawable_Orientation.JavaClass.TL_BR;
            TOP_BOTTOM:
              GO := TJGradientDrawable_Orientation.JavaClass.TOP_BOTTOM;
            TR_BL:
              GO := TJGradientDrawable_Orientation.JavaClass.TR_BL;
          end;
          FBackgroundDrawable.setCornerRadius( FBorderCornerRadius );
          FBackgroundDrawable.setStroke( FBorderWidth, FBorderColor );
          FBackgroundDrawable.setOrientation( GO );
        end;
        if Assigned( FBackgroundDrawable ) then
          FJControl.setBackgroundDrawable( FBackgroundDrawable )
        else
          FJControl.setBackgroundColor( FBackgroundColor1 );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBackgroundColor2( const Value: TAlphaColor );
begin
  FBackgroundColor2 := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBackgroundColor3( const Value: TAlphaColor );
begin
  FBackgroundColor3 := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBackgroundColor1( const Value: TAlphaColor );
begin
  FBackgroundColor1 := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBackgroundImage( const Value: string );
begin
  FBackgroundImage := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    if FBackgroundImage <> '' then
    begin
      FJControl.setBackground( TJColorDrawable.JavaClass.createFromPath( StringToJString( Value ) ) );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBorderColor( const Value: TAlphaColor );
begin
  FBorderColor := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetBorderWidth( const Value: Integer );
begin
  FBorderWidth := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if Assigned( FJControl ) then
          FJControl.setEnabled( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetFocus;
begin
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if Assigned( FJControl ) then
          FJControl.requestFocus;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetFocusable( const Value: Boolean );
begin
  FFocusable := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if Assigned( FJControl ) then
          FJControl.setFocusable( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetFocusableInTouchMode( const Value: Boolean );
begin
  FFocusableInTouchMode := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if Assigned( FJControl ) then
          FJControl.setFocusableInTouchMode( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetGradientOrientation( const Value: TDPFGradientOrientation );
begin
  FGradientOrientation := Value;
  DrawBackground;
end;

// ------------------------------------------------------------------------------
// ID number larger than 0x00FFFFFF is reserved for static views defined in the /res xml files.
procedure TDPFANDBaseControl.SetJID( const Value: Integer );
begin
  FJID := Value;
  if FJID > $00FFFFFF then
    FJID := 0;
{$IFDEF ANDROID}
  if Assigned( JControl ) then
    JControl.setId( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetLockComponent( const Value: Boolean );
begin
  FLockComponent := Value;
  Self.Locked    := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetOnClick( const Value: TDPFOnClick );
begin
  FOnClick := Value;
{$IFDEF ANDROID}
  if Assigned( Value ) then
  begin
    if not Assigned( FDPFJViewOnClickListener ) then
      FDPFJViewOnClickListener := TDPFJViewOnClickListener.create( self );
    FJControl.setOnClickListener( FDPFJViewOnClickListener );
  end
  else
  begin
    FJControl.setOnClickListener( nil );
    FDPFJViewOnClickListener := nil;
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetOnKey( const Value: TDPFOnKey );
begin
  FOnKey := Value;
{$IFDEF ANDROID}
  if Assigned( Value ) then
  begin
    if not Assigned( FDPFJViewOnKeyListener ) then
      FDPFJViewOnKeyListener := TDPFJViewOnKeyListener.create( self );
    FJControl.setOnKeyListener( FDPFJViewOnKeyListener );
  end
  else
  begin
    FJControl.setOnKeyListener( nil );
    FDPFJViewOnKeyListener := nil;
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.SetClickable( const Value: Boolean );
begin
  FClickable := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    CallInUIThread(
      procedure
      begin
        if Assigned( FJControl ) then
          FJControl.setClickable( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
//
// 0 is for VISIBLE
// 4 is for INVISIBLE
// 8 is for GONE
//
procedure TDPFANDBaseControl.SetVisible( const Value: Boolean );
begin
  FVisible := Value;
{$IFDEF ANDROID}
  if Assigned( FJControl ) then
  begin
    if Assigned( FJControl ) then
    begin
      CallInUIThread(
        procedure( )
        begin
          if value then
            FJControl.setVisibility( TJView.JavaClass.VISIBLE )
          else
            FJControl.setVisibility( TJView.JavaClass.INVISIBLE );
        end );
    end;
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFANDBaseControl.Move;
begin
  inherited;
  Resize;
end;

// ------------------------------------------------------------------------------
initialization

RegisterClass( TDPFANDBaseControl );
{$IFDEF ANDROID}
{$ENDIF}

// ------------------------------------------------------------------------------
end.
