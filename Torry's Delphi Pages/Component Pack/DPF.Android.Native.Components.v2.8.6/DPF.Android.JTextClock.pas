// ------------------------------------------------------------------------------
// DPF.Android.JTextClock Component
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
unit DPF.Android.JTextClock;

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
  Androidapi.JNI.Dalvik,
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

  TDPFJTextClock = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJTextClock = class( TDPFANDBaseControl )
  private
    FTextColor    : TAlphaColor;
    FTextAlignment: TDPFTextAlignment;
    FTextSize     : Single;
    FFormat12Hour : string;
    FFormat24Hour : string;
    FTime24Hour   : boolean;
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextSize( const Value: Single );
    procedure SetTime24Hour( const Value: boolean );

  protected
{$IFDEF ANDROID}
    FJTextClock: JTextClock;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJTextClock: JTextClock read FJTextClock;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property TextColor    : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize     : Single read FTextSize write SetTextSize;
    property TextAlignment: TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property Format12Hour : string read FFormat12Hour write FFormat12Hour;
    property Format24Hour : string read FFormat24Hour write FFormat24Hour;
    property Time24Hour   : boolean read FTime24Hour write SetTime24Hour default true;

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
{ TDPFJTextClock }
constructor TDPFJTextClock.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'TextClock';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;

  FTextColor     := TAlphaColors.Black;
  FTextAlignment := taAuto;
  FTextSize      := 18.0;
  FFormat12Hour  := 'h:m:s';
  FFormat24Hour  := 'hh:mm:ss';
  FTime24Hour    := true;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJTextClock := TJTextClock.JavaClass.init( SharedActivity );
    end );
  JControl := FJTextClock;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJTextClock.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJTextClock.Loaded;
var
  classLoader: JDexClassLoader;
  myClass    : Jlang_Class;
begin
  // classLoader := TJDexClassLoader.JavaClass.init( StringToJString( '/classes/DPFClasses.dex' ), StringToJString( '/tmp' ), nil, FJTextClock.getClass.getClassLoader );
  // myClass     := classLoader.loadClass( StringToJString( 'HelloAndroid' ) );

  SetTextAlignment( FTextAlignment );
  SetTextColor( FTextColor );
  SetTextSize( TextSize );
  SetTime24Hour( FTime24Hour );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJTextClock ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextClock.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJTextClock <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextClock.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJTextClock <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextClock.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.SetTime24Hour( const Value: boolean );
begin
  FTime24Hour := Value;
{$IFDEF ANDROID}
  if Assigned( FJTextClock ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        if value then
        begin
          FJTextClock.setFormat12Hour( nil );
          FJTextClock.setFormat24Hour( StrToJCharSequence( FFormat24Hour ) );
        end
        else
        begin
          FJTextClock.setFormat24Hour( nil );
          FJTextClock.setFormat12Hour( StrToJCharSequence( FFormat12Hour ) );
        end;
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJTextClock.Paint;
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

end.
