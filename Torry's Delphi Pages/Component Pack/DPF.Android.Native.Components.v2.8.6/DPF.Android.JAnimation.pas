// ------------------------------------------------------------------------------
// DPF.Android.JAnimation Component
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
unit DPF.Android.JAnimation;

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
{$IFDEF ANDROID}
  DPF.Android.Widget,
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

  TDPFJAnimation = class;

{$IFDEF ANDROID}

  TDPFOnAnimationListener = class( TJavaLocal, JDPFOnAnimationListener )
  private
    [weak]
    FDPFJAnimation: TDPFJAnimation;
  public
    constructor create( ADPFJAnimation: TDPFJAnimation );

    procedure onAnimationStart( animation: JAnimation ); cdecl;
    procedure onAnimationRepeat( animation: JAnimation ); cdecl;
    procedure onAnimationEnd( animation: JAnimation ); cdecl;
  end;

{$ENDIF}

  TAnimationOnAnimationStart  = procedure( sender: TObject ) of object;
  TAnimationOnAnimationRepeat = procedure( sender: TObject ) of object;
  TAnimationOnAnimationEnd    = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJAnimation = class( TComponent )
  private
    FOnAnimationEnd   : TAnimationOnAnimationEnd;
    FOnAnimationRepeat: TAnimationOnAnimationRepeat;
    FOnAnimationStart : TAnimationOnAnimationStart;
  protected
{$IFDEF ANDROID}
    FJDPFAnimation         : JDPFAnimation;
    FDPFOnAnimationListener: TDPFOnAnimationListener;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJAnimation: JDPFAnimation read FJDPFAnimation;
    procedure StartTranslateAnimation( Control: TDPFANDBaseControl; fromXDelta: single; toXDelta: single; fromYDelta: single; toYDelta: single; fromAlpha: Single; toAlpha: Single; durationMillis: LongWord );
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnAnimationStart : TAnimationOnAnimationStart read FOnAnimationStart write FOnAnimationStart;
    property OnAnimationRepeat: TAnimationOnAnimationRepeat read FOnAnimationRepeat write FOnAnimationRepeat;
    property OnAnimationEnd   : TAnimationOnAnimationEnd read FOnAnimationEnd write FOnAnimationEnd;

  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJAnimation }
constructor TDPFJAnimation.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF ANDROID}
  FDPFOnAnimationListener := TDPFOnAnimationListener.create( self );
  FJDPFAnimation          := TJDPFAnimation.JavaClass.init( );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJAnimation.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

{$IFDEF ANDROID}

procedure TDPFJAnimation.StartTranslateAnimation( Control: TDPFANDBaseControl; fromXDelta: single; toXDelta: single; fromYDelta: single; toYDelta: single; fromAlpha: Single; toAlpha: Single; durationMillis: LongWord );
begin
  CallInUIThread(
    procedure( )
    begin
      if assigned( FJDPFAnimation ) then
      begin
        FJDPFAnimation.startTranslateAnimation( SharedActivity, Control.JControl, Control.ScreenScale, fromXDelta, toXDelta, fromYDelta, toYDelta, fromAlpha, toAlpha, durationMillis, FDPFOnAnimationListener );
      end
    end );
end;

// ------------------------------------------------------------------------------
constructor TDPFOnAnimationListener.create( ADPFJAnimation: TDPFJAnimation );
begin
  inherited create;
  FDPFJAnimation := ADPFJAnimation;
end;

// ------------------------------------------------------------------------------
procedure TDPFOnAnimationListener.onAnimationStart( animation: JAnimation ); cdecl;
begin
  if Assigned( FDPFJAnimation.FOnAnimationStart ) then
    FDPFJAnimation.FOnAnimationStart( FDPFJAnimation );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnAnimationListener.onAnimationRepeat( animation: JAnimation ); cdecl;
begin
  if Assigned( FDPFJAnimation.FOnAnimationRepeat ) then
    FDPFJAnimation.FOnAnimationRepeat( FDPFJAnimation );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnAnimationListener.onAnimationEnd( animation: JAnimation ); cdecl;
begin
  if Assigned( FDPFJAnimation.FOnAnimationEnd ) then
    FDPFJAnimation.FOnAnimationEnd( FDPFJAnimation );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
