// ------------------------------------------------------------------------------
// DPF.Android.ApplicationManager Component
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
unit DPF.Android.ApplicationManager;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  FMX.VirtualKeyboard,
  DPF.Android.BaseControl,
{$IFDEF ANDROID}
  Androidapi.NativeActivity,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.AppGlue,
  Androidapi.Input,
  FMX.Helpers.Android,
  FMX.Platform.Android,
  DPF.Android.Common,
{$ENDIF}
  FMX.Platform;

type

  // ------------------------------------------------------------------------------
  // TDPFAppOnOpenURL = procedure( Sender: TObject; Url: string; sourceApplication: string ) of object;
  TDPFAppOnKey = procedure( Sender: TObject; Action: Integer; KeyCode: Word ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFAndroidApplicationManager = class( TComponent )
  private
    FOnMemoryWarning       : TNotifyEvent;
    FOnBecameActive        : TNotifyEvent;
    FOnEnteredBackground   : TNotifyEvent;
    FOnTimeChange          : TNotifyEvent;
    FOnWillBecomeForeground: TNotifyEvent;
    FOnWillBecomeInactive  : TNotifyEvent;
    FOnWillTerminate       : TNotifyEvent;
    FAppOnKey              : TDPFAppOnKey;
    // FOnOpenURL             : TDPFAppOnOpenURL;
    procedure HandleAndroidInputEvent( Action: Integer; KeyCode: Word );
{$IFDEF ANDROID}
{$ENDIF}
  protected
    function AppEvent( AAppEvent: TApplicationEvent; AContext: TObject ): Boolean;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property AppOnKey: TDPFAppOnKey read FAppOnKey write FAppOnKey;
    // property OnMemoryWarning       : TNotifyEvent read FOnMemoryWarning write FOnMemoryWarning;
    // property OnBecameActive        : TNotifyEvent read FOnBecameActive write FOnBecameActive;
    // property OnEnteredBackground   : TNotifyEvent read FOnEnteredBackground write FOnEnteredBackground;
    // property OnTimeChange          : TNotifyEvent read FOnTimeChange write FOnTimeChange;
    // property OnWillBecomeForeground: TNotifyEvent read FOnWillBecomeForeground write FOnWillBecomeForeground;
    // property OnWillBecomeInactive  : TNotifyEvent read FOnWillBecomeInactive write FOnWillBecomeInactive;
    // property OnOnWillTerminate     : TNotifyEvent read FOnWillTerminate write FOnWillTerminate;
    // property OnOpenURL             : TDPFAppOnOpenURL read FOnOpenURL write FOnOpenURL;
  end;

  // ------------------------------------------------------------------------------

implementation

var
  FDPFAndroidApplicationManager: TDPFAndroidApplicationManager = nil;
{$IFDEF ANDROID}
  OldOnInputEvent: function( App: PAndroid_app; Event: PAInputEvent ): Int32; cdecl;

function DPFHandleAndroidInputEvent( var App: TAndroid_app; Event: PAInputEvent ): Int32;
var
  EventType          : Int64;
  KeyCode, vkKeyCode : Word;
  Action, MetaState  : Integer;
  KeyChar            : Char;
  KeyEvent           : JKeyEvent;
  KeyEventChars      : string;
  EventTime, DownTime: Int64;
begin
  EventType := AInputEvent_getType( Event );
  if EventType = AINPUT_EVENT_TYPE_KEY then
  begin // Keyboard input
    Action    := AKeyEvent_getAction( Event );
    KeyCode   := AKeyEvent_getKeyCode( Event );
    MetaState := AKeyEvent_getMetaState( Event );
    EventTime := AKeyEvent_getEventTime( Event ) div 1000000;
    DownTime  := AKeyEvent_getDownTime( Event ) div 1000000;
  end;
  FDPFAndroidApplicationManager.HandleAndroidInputEvent( Action, KeyCode );
  Result := OldOnInputEvent( @App, Event );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFAndroidApplicationManager }
// ------------------------------------------------------------------------------
constructor TDPFAndroidApplicationManager.Create( AOwner: TComponent );
{$IFDEF ANDROID}
var
  AndroidApp: PAndroid_app;
{$ENDIF}
begin
  inherited Create( AOwner );
{$IFDEF ANDROID}
  AndroidApp                    := GetAndroidApp;
  OldOnInputEvent               := AndroidApp^.onInputEvent;
  AndroidApp^.onInputEvent      := @DPFHandleAndroidInputEvent;
  FDPFAndroidApplicationManager := self;
{$ENDIF}
  // PANativeActivity( System.DelphiActivity )^. := OnContentRectChanged;
  {
    if TPlatformServices.Current.SupportsPlatformService( IFMXApplicationEventService, IInterface( AppEventSvc ) ) then
    AppEventSvc.SetApplicationEventHandler( AppEvent );
  }
end;

// ------------------------------------------------------------------------------
destructor TDPFAndroidApplicationManager.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFAndroidApplicationManager.AppEvent( AAppEvent: TApplicationEvent; AContext: TObject ): Boolean;
begin
  result := true;
{$IFDEF ANDROID}
  DPFNSLog( 'TDPFAndroidApplicationManager.AppEvent: ' + IntToStr( Integer( AAppEvent ) ) );
{$ENDIF}
  case AAppEvent of

    // ---------------------------------------------------------
    TApplicationEvent.aeLowMemory:
      begin
        if Assigned( FOnMemoryWarning ) then
          FOnMemoryWarning( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeBecameActive:
      begin
        if Assigned( FOnBecameActive ) then
          FOnBecameActive( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeEnteredBackground:
      begin
        if Assigned( FOnEnteredBackground ) then
          FOnEnteredBackground( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeTimeChange:
      begin
        if Assigned( FOnTimeChange ) then
          FOnTimeChange( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeWillBecomeForeground:
      begin
        if Assigned( FOnWillBecomeForeground ) then
          FOnWillBecomeForeground( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeWillBecomeInactive:
      begin
        if Assigned( FOnWillBecomeInactive ) then
          FOnWillBecomeInactive( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeWillTerminate:
      begin
        if Assigned( FOnWillTerminate ) then
          FOnWillTerminate( Self );
      end;

    // ---------------------------------------------------------
    TApplicationEvent.aeOpenURL:
      begin
        // if Assigned( FOnOpenURL ) then
        // begin
{$IFDEF ANDROID}
        // FOnOpenURL( Self, TiOSOpenApplicationContext( AContext ).URL, TiOSOpenApplicationContext( AContext ).SourceApp );
{$ENDIF}
        // end;
      end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidApplicationManager.HandleAndroidInputEvent( Action: Integer; KeyCode: Word );
begin
  if Assigned( FAppOnKey ) then
    FAppOnKey( Self, Action, KeyCode );
end;

// ------------------------------------------------------------------------------
end.
