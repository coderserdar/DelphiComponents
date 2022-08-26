// ------------------------------------------------------------------------------
// DPF.Android.JRadioGroup Component
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
unit DPF.Android.JRadioGroup;

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
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJRadioGroup = class;

{$IFDEF ANDROID}

  TJRadioGroupOnCheckedChangeListener = class( TJavaLocal, JRadioGroup_OnCheckedChangeListener )
  private
    FDPFJRadioGroup: TDPFJRadioGroup;
  public
    constructor create( ADPFJRadioGroup: TDPFJRadioGroup );

    procedure onCheckedChanged( group: JRadioGroup; checkedId: integer ); cdecl;
  end;
{$ENDIF}

  TDPFRadioGroupOnCheckedChanged = procedure( sender: TObject; CheckedID: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJRadioGroup = class( TDPFANDBaseControl )
  private
    FOnCheckedChanged: TDPFRadioGroupOnCheckedChanged;
    FRadioButtons    : TStrings;
    FButtonTextColor : TAlphaColor;
    FButtonTextSize  : Single;
    procedure SetRadioButtons( const Value: TStrings );
    procedure RadioButtonsChanged( Sender: TObject );

  protected
{$IFDEF ANDROID}
    FJRadioGroup                       : JRadioGroup;
    FJRadioGroupOnCheckedChangeListener: TJRadioGroupOnCheckedChangeListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJRadioGroup: JRadioGroup read FJRadioGroup;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure CheckRadioButton( id: Integer );
    procedure ClearCheck;

  published
    property ButtonTextColor: TAlphaColor read FButtonTextColor write FButtonTextColor default TAlphaColors.Black;
    property ButtonTextSize : Single read FButtonTextSize write FButtonTextSize;

    property RadioButtons    : TStrings read FRadioButtons write SetRadioButtons;
    property OnCheckedChanged: TDPFRadioGroupOnCheckedChanged read FOnCheckedChanged write FOnCheckedChanged;

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
{ TDPFJRadioGroup }
procedure TDPFJRadioGroup.CheckRadioButton( id: Integer );
begin
{$IFDEF ANDROID}
  if Assigned( FJRadioGroup ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioGroup.check( id );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioGroup.ClearCheck;
begin
{$IFDEF ANDROID}
  if Assigned( FJRadioGroup ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioGroup.clearCheck;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFJRadioGroup.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption                        := 'Radio Group';
  BackgroundColor1                      := TAlphaColors.Lightsteelblue;
  FRadioButtons                         := TStringList.Create;
  TStringList( FRadioButtons ).OnChange := RadioButtonsChanged;

  FButtonTextColor := TAlphaColors.Black;
  FButtonTextSize  := 21;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJRadioGroup := TJRadioGroup.JavaClass.init( SharedActivity );
      FJRadioGroupOnCheckedChangeListener := TJRadioGroupOnCheckedChangeListener.create( self );
      FJRadioGroup.setOnCheckedChangeListener( FJRadioGroupOnCheckedChangeListener );
    end );
  JControl := FJRadioGroup;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJRadioGroup.Destroy;
begin
{$IFDEF ANDROID}
  FJRadioGroupOnCheckedChangeListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJRadioGroup.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames
  RadioButtonsChanged( FRadioButtons );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJRadioGroup.RadioButtonsChanged( Sender: TObject );
begin
{$IFDEF ANDROID}
  if Assigned( FJRadioGroup ) then
  begin
    CallInUIThread(
      procedure( )
      var
        I: Integer;
        J: JRadioButton;
      begin
        if FJRadioGroup.getChildCount > 1 then
          FJRadioGroup.removeAllViews;
        for I := 0 to FRadioButtons.Count - 1 do
        begin
          J := TJRadioButton.JavaClass.init( SharedActivity );
          J.setText( StrToJCharSequence( FRadioButtons[I] ) );
          J.setTextColor( FButtonTextColor );
          J.setTextSize( ButtonTextSize );
          J.setGravity( TDPFTextAlignmentToGravity[taCenter_Vertical] );
          J.setId( i );
          FJRadioGroup.addView( J );
        end;

      end );
  end;
{$ELSE}
  ControlCaption := FRadioButtons.Text;
  Invalidate;
{$ENDIF}
end;

procedure TDPFJRadioGroup.Resize;
begin
{$IFDEF ANDROID}
  // if FJRadioGroupGroup <> nil then FJRadioGroupGroup.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioGroup.SetRadioButtons( const Value: TStrings );
begin
  FRadioButtons.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioGroup.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJRadioGroup.Paint;
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

// ------------------------------------------------------------------------------
constructor TJRadioGroupOnCheckedChangeListener.create( ADPFJRadioGroup: TDPFJRadioGroup );
begin
  inherited create;
  FDPFJRadioGroup := ADPFJRadioGroup;
end;

// ------------------------------------------------------------------------------
procedure TJRadioGroupOnCheckedChangeListener.onCheckedChanged( group: JRadioGroup; checkedId: integer ); cdecl;
begin
  if assigned( FDPFJRadioGroup.FOnCheckedChanged ) then
    FDPFJRadioGroup.FOnCheckedChanged( FDPFJRadioGroup, checkedId );
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
