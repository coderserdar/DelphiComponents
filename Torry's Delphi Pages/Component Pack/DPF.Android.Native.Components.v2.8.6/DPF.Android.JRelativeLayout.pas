// ------------------------------------------------------------------------------
// DPF.Android.JRelativeLayout Component
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
unit DPF.Android.JRelativeLayout;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

  System.TypInfo,
  DPF.Android.BaseControl,
  DPF.Android.Widget,
{$IFDEF ANDROID}
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJRelativeLayout = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJRelativeLayout = class( TDPFANDBaseControl )
  private
    FEmbFrame       : TFrame;
    FForm           : TForm;
    FOnFormChanging : TDPFFormChanging;
    FOnFormChanged  : TDPFFormChanged;
    FFrame          : TDPFFrame;
    FOnFrameChanged : TDPFFrameChanged;
    FOnFrameChanging: TDPFFrameChanging;
    procedure SetForm( const Value: TForm );
    procedure SetFrame( const Value: TDPFFrame );

  protected
{$IFDEF ANDROID}
    FJRelativeLayout: JRelativeLayout;
    // FJRelativeLayout_LayoutParams: JRelativeLayout_LayoutParams;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJRelativeLayout: JRelativeLayout read FJRelativeLayout;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    // procedure clearAllSubviews( isRelease: Boolean = false );
    function GetFrame: TFrame;

  published
    property Form : TForm read FForm write SetForm;
    property Frame: TDPFFrame read FFrame write SetFrame;

    property OnFormChanging: TDPFFormChanging read FOnFormChanging write FOnFormChanging;
    property OnFormChanged : TDPFFormChanged read FOnFormChanged write FOnFormChanged;

    property OnFrameChanging: TDPFFrameChanging read FOnFrameChanging write FOnFrameChanging;
    property OnFrameChanged : TDPFFrameChanged read FOnFrameChanged write FOnFrameChanged;

    property Clickable;
    property Focusable;
    property FocusableInTouchMode;
    property BackgroundColor1;
    property BackgroundColor2;
    property BackgroundColor3;
    property BackgroundImage;
    property BorderWidth;
    property BorderColor;
    property BorderCornerRadius;
    property GradientOrientation;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
    property OnClick;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJRelativeLayout }
constructor TDPFJRelativeLayout.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'RelativeLayout';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FForm            := nil;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      JRelativeLayout_LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init( Integer( LayoutWidth ), Integer( LayoutHeight ) );
      FJRelativeLayout := TJRelativeLayout.JavaClass.init( SharedActivity );
      // FJRelativeLayout_LayoutParams.setLayoutDirection();
      FJRelativeLayout.setLayoutParams( JRelativeLayout_LayoutParams );
      JControl := FJRelativeLayout;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJRelativeLayout.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJRelativeLayout.GetFrame: TFrame;
begin
  result := FEmbFrame;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJRelativeLayout.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJRelativeLayout.Resize;
begin
{$IFDEF ANDROID}
  if IsAutoResize then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRelativeLayout.setX( Position.x * ScreenScale );
        FJRelativeLayout.setY( Position.y * ScreenScale );

        //JRelativeLayout_LayoutParams.leftMargin := round( Position.x * ScreenScale );
        //JRelativeLayout_LayoutParams.topMargin := round( Position.y * ScreenScale );

        if LayoutWidth <> lpNone then
          JRelativeLayout_LayoutParams.width := Integer( LayoutWidth )
        else
          JRelativeLayout_LayoutParams.width := round( Width * ScreenScale );

        if LayoutHeight <> lpNone then
          JRelativeLayout_LayoutParams.height := Integer( LayoutHeight )
        else
          JRelativeLayout_LayoutParams.height := round( Height * ScreenScale );

      end );
  end
  else
    inherited;
{$ELSE}
  inherited;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRelativeLayout.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJRelativeLayout.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJRelativeLayout.SetForm( const Value: TForm );
{$IFDEF ANDROID}
var
  C        : TDPFANDBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if ( FJRelativeLayout <> nil ) and ( ( FForm <> Value ) or ( Assigned( Value ) and not Assigned( Value.TagObject ) ) ) then
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
        MoveChildsToOriginParent( Value.TagObject as TDPFANDBaseControl );

      while Value.ChildrenCount <> 0 do
        if Value.Children[0] is TDPFANDBaseControl then
        begin
          C        := Value.Children[0] as TDPFANDBaseControl;
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
procedure TDPFJRelativeLayout.SetFrame( const Value: TDPFFrame );
{$IFDEF ANDROID}
var
  C        : TDPFANDBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if Assigned( FEmbFrame ) and ( FEmbFrame is Value ) then
    exit;

  if ( FJRelativeLayout <> nil ) then
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
        if FEmbFrame.Children[0] is TDPFANDBaseControl then
        begin
          C        := FEmbFrame.Children[0] as TDPFANDBaseControl;
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

end.
