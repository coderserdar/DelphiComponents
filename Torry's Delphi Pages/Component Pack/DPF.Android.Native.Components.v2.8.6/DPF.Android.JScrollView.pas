// ------------------------------------------------------------------------------
// DPF.Android.JScrollView Component
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
unit DPF.Android.JScrollView;

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

  TDPFJScrollView = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJScrollView = class( TDPFANDBaseControl )
  private

  protected
{$IFDEF ANDROID}
    FJScrollView             : JScrollView;
    FJScrollViewContentLayout: JLinearLayout;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJScrollView: JScrollView read FJScrollView;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure AddControl( Control: TDPFANDBaseControl );
    procedure RemoveAllControls;
  published

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
constructor TDPFJScrollView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'ScrollView';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    var
      lp: JLinearLayout_LayoutParams;
    begin
      FJScrollView := TJScrollView.JavaClass.init( SharedActivity );
      FJScrollViewContentLayout := TJLinearLayout.JavaClass.init( SharedActivity );
      lp := TJLinearLayout_LayoutParams.JavaClass.init( -1, -1 );

      FJScrollViewContentLayout.setLayoutParams( lp );
      FJScrollViewContentLayout.setOrientation( TJLinearLayout.JavaClass.VERTICAL );
      FJScrollView.addView( FJScrollViewContentLayout );
    end );
  JControl := FJScrollView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJScrollView.Destroy;
begin
{$IFDEF ANDROID}
  FJScrollViewContentLayout := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJScrollView.Loaded;
var
  i : Integer;
  lp: JLinearLayout_LayoutParams;
  b : JButton;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;

end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFJScrollView }
procedure TDPFJScrollView.AddControl( Control: TDPFANDBaseControl );
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    var
      lp: JLinearLayout;
    begin
      //lp := TJLinearLayout.JavaClass.init( SharedActivity );
      //lp.addView(Control.ThisView);
      FJScrollViewContentLayout.addView( JView(Control.JControl) );
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJScrollView.RemoveAllControls;
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJScrollViewContentLayout.removeAllViews;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJScrollView.Resize;
begin
{$IFDEF ANDROID}
  // if FJScrollViewGroup <> nil then FJScrollViewGroup.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJScrollView.Move;
begin
  inherited;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJScrollView.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}
{$IFDEF ANDROID}
{$ENDIF}
// ------------------------------------------------------------------------------

end.
