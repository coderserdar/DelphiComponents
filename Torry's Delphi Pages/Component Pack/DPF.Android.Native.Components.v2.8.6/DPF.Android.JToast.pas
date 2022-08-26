// ------------------------------------------------------------------------------
// DPF.Android.JToast Component
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
unit DPF.Android.JToast;

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

  TDPFJToast = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJToast = class( TComponent )
  private
  protected
{$IFDEF ANDROID}
    FJToast: JToast;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJToast: JToast read FJToast;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Show( Msg: string; duration: integer = 3000; Alignment: TDPFTextAlignment = TDPFTextAlignment.taCenter );
  published
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJToast }
constructor TDPFJToast.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF ANDROID}
  FJToast := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJToast.Destroy;
begin
{$IFDEF ANDROID}
  { if Assigned( FTapGestureRecognizer ) then
    FJToastGroup.removeGestureRecognizer( FTapGestureRecognizer );

    if Assigned( FDPFGestureRecognizerDelegate ) then
    FDPFGestureRecognizerDelegate.DisposeOf; }
{$ENDIF}
  inherited;
end;

procedure TDPFJToast.Show( Msg: string; duration: integer = 3000; Alignment: TDPFTextAlignment = TDPFTextAlignment.taCenter );
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      if assigned( FJToast ) then
      begin
        FJToast.setText( StrToJCharSequence( MSg ) );
        FJToast.setDuration( duration );
      end
      else
        FJToast := TJToast.JavaClass.makeText( SharedActivity, StrToJCharSequence( MSg ), duration );
      FJToast.setGravity( TDPFTextAlignmentToGravity[Alignment], 0, 0 );
      FJToast.show;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------

end.
