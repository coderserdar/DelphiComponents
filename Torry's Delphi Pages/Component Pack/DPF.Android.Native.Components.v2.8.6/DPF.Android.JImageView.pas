// ------------------------------------------------------------------------------
// DPF.Android.JImageView Component
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
unit DPF.Android.JImageView;

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
  DPF.Android.IO,
  DPF.Android.Common,
{$IFDEF ANDROID}
  DPF.Android.Net,
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

  TDPFJImageView = class;

{$IFDEF ANDROID}
{$ENDIF}
  TDPFImageViewScaleType = ( stCENTER, stCENTER_CROP, stCENTER_INSIDE, stFIT_CENTER, stFIT_END, stFIT_START, stFIT_XY, stMATRIX );

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJImageView = class( TDPFANDBaseControl )
  private
    FFileName : string;
    FScaleType: TDPFImageViewScaleType;
    FURL      : string;
    procedure SetFileName( const Value: string );
    procedure SetScaleType( const Value: TDPFImageViewScaleType );
    procedure setURL( const value: string );

  protected
{$IFDEF ANDROID}
    FJImageView: JImageView;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJImageView: JImageView read FJImageView;
    procedure Loaded; override;
    procedure LoadFromStream( Stream: TStream );
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property FileName : string read FFileName write SetFileName;
    property ScaleType: TDPFImageViewScaleType read FScaleType write SetScaleType default TDPFImageViewScaleType.stFIT_CENTER;
    property URL      : string read FURL write SetURL;

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
{ TDPFJImageView }
constructor TDPFJImageView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'ImageView';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  FScaleType       := TDPFImageViewScaleType.stFIT_CENTER;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJImageView := TJImageView.JavaClass.init( SharedActivity );
    end );
  JControl := FJImageView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJImageView.Destroy;
begin
{$IFDEF ANDROID}
  { if Assigned( FTapGestureRecognizer ) then
    FJImageViewGroup.removeGestureRecognizer( FTapGestureRecognizer );

    if Assigned( FDPFGestureRecognizerDelegate ) then
    FDPFGestureRecognizerDelegate.DisposeOf; }
{$ENDIF}
  inherited;
end;


// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJImageView.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  SetScaleType( FScaleType );
  if FFileName <> '' then
    SetFileName( FFileName )
  else if FURL <> '' then
    setURL( FURL );

  addSubview( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJImageView.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJImageView.setURL( const value: string );
{$IFDEF ANDROID}
var
  myFileUrl: JURL;
  conn     : JURLConnection;
  inp      : JInputStream;
{$ENDIF}
begin
  FURL := value;
{$IFDEF ANDROID}
  if assigned( FJImageView ) then
  begin
    myFileUrl := TJURL.JavaClass.init( StringToJString( value ) );
    conn      := JHttpURLConnection( myFileUrl.openConnection( ) );
    conn.setDoInput( true );
    conn.connect( );

    inp := conn.getInputStream;
    FJImageView.setImageBitmap( TJBitmapFactory.JavaClass.decodeStream( inp ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}
procedure TDPFJImageView.LoadFromStream( Stream: TStream );
var
  bmImg    : JBitmap;
  FullName : string;
  bMapArray: TJavaArray<byte>;
  (* inp      : JFileInputStream;
    buf      : JBufferedInputStream;
    bMapArray: TJavaArray<byte>;
    bMap     : JBitmap; *)

begin
  if assigned( FJImageView ) then
  begin
    bMapArray       := TJavaArray<byte>.Create( Stream.Size );
    Stream.Position := 0;
    Stream.ReadBuffer( bMapArray.Data^, Stream.Size );
    bmImg := TJBitmapFactory.JavaClass.decodeByteArray( bMapArray, 0, Stream.Size );
    if assigned( bmImg ) then
      FJImageView.setImageBitmap( bmImg );
  end;

end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJImageView.SetFileName( const Value: string );
{$IFDEF ANDROID}
var
  bmImg   : JBitmap;
  FullName: string;
  (* inp      : JFileInputStream;
    buf      : JBufferedInputStream;
    bMapArray: TJavaArray<byte>;
    bMap     : JBitmap; *)

{$ENDIF}
begin
  FFileName := Value;
{$IFDEF ANDROID}
  if assigned( FJImageView ) then
  begin
    FullName := Value;
    if not FileExists( Value ) then
      FullName := GetAppFolder + Value;
    if FileExists( FullName ) then
    begin
      bmImg := TJBitmapFactory.JavaClass.decodeFile( StringToJString( FullName ) );
      if assigned( bmImg ) then
        FJImageView.setImageBitmap( bmImg );
    end;

    (*
      if FileExists( FullName ) then
      begin
      inp := TJFileInputStream.JavaClass.init( StringToJString( FullName ) );
      buf := TJBufferedInputStream.JavaClass.init( inp );
      if buf.available > 0 then
      begin
      bMapArray := TJavaArray<byte>.Create( buf.available );
      buf.read( bMapArray );
      bMap := TJBitmapFactory.JavaClass.decodeByteArray( bMapArray, 0, bMapArray.Length );
      FJImageView.setImageBitmap( bMap );
      end;

      if inp <> nil then
      inp.close;

      if buf <> nil then
      buf.close( );
      end;
    *)
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJImageView.SetScaleType( const Value: TDPFImageViewScaleType );
{$IFDEF ANDROID}
var
  st: JImageView_ScaleType;
{$ENDIF}
begin
  FScaleType := Value;
{$IFDEF ANDROID}
  if assigned( FJImageView ) then
  begin
    st := nil;
    case Value of
      stCENTER:
        st := TJImageView_ScaleType.JavaClass.CENTER;
      stCENTER_CROP:
        st := TJImageView_ScaleType.JavaClass.CENTER_CROP;
      stCENTER_INSIDE:
        st := TJImageView_ScaleType.JavaClass.CENTER_INSIDE;
      stFIT_CENTER:
        st := TJImageView_ScaleType.JavaClass.FIT_CENTER;
      stFIT_END:
        st := TJImageView_ScaleType.JavaClass.FIT_END;
      stFIT_START:
        st := TJImageView_ScaleType.JavaClass.FIT_START;
      stFIT_XY:
        st := TJImageView_ScaleType.JavaClass.FIT_XY;
      stMATRIX:
        st := TJImageView_ScaleType.JavaClass.MATRIX;
    end;
    if assigned( st ) then
      FJImageView.setScaleType( st );
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJImageView.Move;
begin
  inherited;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJImageView.Paint;
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
