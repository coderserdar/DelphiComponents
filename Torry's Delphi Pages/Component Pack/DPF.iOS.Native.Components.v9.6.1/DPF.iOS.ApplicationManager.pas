// ------------------------------------------------------------------------------
// DPF.iOS.ApplicationManager Component
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
// Usefull guid link:
// http://tech.glowing.com/cn/method-swizzling-aop/
// ------------------------------------------------------------------------------
unit DPF.iOS.ApplicationManager;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Messaging,

  System.TypInfo,
  Generics.Collections,
  FMX.VirtualKeyboard,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Platform,
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFApplicationManager = class;

  // ------------------------------------------------------------------------------
  TDPFAppOnOpenURL = procedure( Sender: TObject; Url: string; sourceApplication: string ) of object;

  TOriginalViewDidAppear = procedure( id: Pointer; SEL: pointer; Anim: Boolean )cdecl;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFApplicationManager = class( TComponent )
  private
    FOnFinishedLaunching   : TNotifyEvent;
    FOnMemoryWarning       : TNotifyEvent;
    FOnBecameActive        : TNotifyEvent;
    FOnEnteredBackground   : TNotifyEvent;
    FOnTimeChange          : TNotifyEvent;
    FOnWillBecomeForeground: TNotifyEvent;
    FOnWillBecomeInactive  : TNotifyEvent;
    FOnWillTerminate       : TNotifyEvent;
    FOnOpenURL             : TDPFAppOnOpenURL;
    FOnInterfaceRotated    : TNotifyEvent;
    procedure MessageListener( const Sender: TObject; const M: TMessage );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnFinishedLaunching   : TNotifyEvent read FOnFinishedLaunching write FOnFinishedLaunching;
    property OnMemoryWarning       : TNotifyEvent read FOnMemoryWarning write FOnMemoryWarning;
    property OnBecameActive        : TNotifyEvent read FOnBecameActive write FOnBecameActive;
    property OnEnteredBackground   : TNotifyEvent read FOnEnteredBackground write FOnEnteredBackground;
    property OnTimeChange          : TNotifyEvent read FOnTimeChange write FOnTimeChange;
    property OnWillBecomeForeground: TNotifyEvent read FOnWillBecomeForeground write FOnWillBecomeForeground;
    property OnWillBecomeInactive  : TNotifyEvent read FOnWillBecomeInactive write FOnWillBecomeInactive;
    property OnWillTerminate       : TNotifyEvent read FOnWillTerminate write FOnWillTerminate;
    property OnOpenURL             : TDPFAppOnOpenURL read FOnOpenURL write FOnOpenURL;
    property OnInterfaceRotated    : TNotifyEvent read FOnInterfaceRotated write FOnInterfaceRotated;
  end;

  TViewDidAppearEvent = procedure( Sender: TObject; Animated: Boolean ) of object;

  TDPFViewControllerEvents = class( TComponent )
  private
    FOnViewDidAppear: TViewDidAppearEvent;
    procedure SetOnViewDidAppear( const Value: TViewDidAppearEvent );
  protected
    procedure DoViewDidAppear( View: Pointer; Animated: Boolean );
  public
    constructor Create( AOwner: TComponent ); override;
    property OnViewDidAppear: TViewDidAppearEvent read FOnViewDidAppear write SetOnViewDidAppear;
  end;

  // ------------------------------------------------------------------------------
//procedure class_replaceMethod( Cls: Pointer; Method1, Method2: Pointer; TypeEnc: Pointer ); cdecl; external libobjc name _PU + 'class_replaceMethod';

implementation

type
  TDPFViewControllerMultiCaster = class( TComponent )
  private
    FIsHookActive: Boolean;
    FVCEvents    : TObjectList<TDPFViewControllerEvents>;
    procedure Hook;
    procedure DoViewDidAppear( View: Pointer; Animated: Boolean );
  public
    procedure AddViewControllerEvent( Event: TDPFViewControllerEvents );
    procedure RemoveViewControllerEvent( Event: TDPFViewControllerEvents );
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  end;

var
  ViewControllerMultiCaster: TDPFViewControllerMultiCaster = nil;
  POriginalViewDidAppear   : ^TOriginalViewDidAppear;

  // ------------------------------------------------------------------------------
  { TDPFApplicationManager }
  // ------------------------------------------------------------------------------
constructor TDPFApplicationManager.Create( AOwner: TComponent );
// var
// AppEventSvc: IFMXApplicationEventService;
begin
  inherited Create( AOwner );

  TMessageManager.DefaultManager.SubscribeToMessage( TOrientationChangedMessage, MessageListener );
  TMessageManager.DefaultManager.SubscribeToMessage( TApplicationEventMessage, MessageListener );

  // SZ: use new XE7 mechanism
  // if TPlatformServices.Current.SupportsPlatformService( IFMXApplicationEventService, IInterface( AppEventSvc ) ) then
  // AppEventSvc.SetApplicationEventHandler( AppEvent );
end;

// ------------------------------------------------------------------------------
destructor TDPFApplicationManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe( TOrientationChangedMessage, MessageListener );
  TMessageManager.DefaultManager.Unsubscribe( TApplicationEventMessage, MessageListener );
  inherited;
end;

procedure TDPFApplicationManager.MessageListener( const Sender: TObject; const M: TMessage );
begin
  if M is TOrientationChangedMessage then
  begin
    if Assigned( FOnInterfaceRotated ) then
      FOnInterfaceRotated( Self );
  end
  else if M is TApplicationEventMessage then
  begin
{$IFDEF IOS}
    DPFNSLog( 'TDPFApplicationManager.AppEvent: ' + IntToStr( Ord( TApplicationEventMessage( M ).Value.Event ) ) );
{$ENDIF}
    case TApplicationEventMessage( M ).Value.Event of

      // ---------------------------------------------------------
      TApplicationEvent.FinishedLaunching:
        begin
          if Assigned( FOnMemoryWarning ) then
            FOnFinishedLaunching( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.LowMemory:
        begin
          if Assigned( FOnMemoryWarning ) then
            FOnMemoryWarning( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.BecameActive:
        begin
          if Assigned( FOnBecameActive ) then
            FOnBecameActive( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.EnteredBackground:
        begin
          if Assigned( FOnEnteredBackground ) then
            FOnEnteredBackground( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.TimeChange:
        begin
          if Assigned( FOnTimeChange ) then
            FOnTimeChange( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.WillBecomeForeground:
        begin
          if Assigned( FOnWillBecomeForeground ) then
            FOnWillBecomeForeground( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.WillBecomeInactive:
        begin
          if Assigned( FOnWillBecomeInactive ) then
            FOnWillBecomeInactive( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.WillTerminate:
        begin
          if Assigned( FOnWillTerminate ) then
            FOnWillTerminate( Self );
        end;

      // ---------------------------------------------------------
      TApplicationEvent.OpenURL:
        begin
          if Assigned( FOnOpenURL ) then
          begin
{$IFDEF IOS}
            FOnOpenURL( Self, TiOSOpenApplicationContext( TApplicationEventMessage( M ).Value.Context ).URL, TiOSOpenApplicationContext( TApplicationEventMessage( M ).Value.Context ).SourceApp );
{$ENDIF}
          end;
        end;
    end;
  end;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure vceViewDidAppear( self: Pointer; _cmd: SEL; animated: Boolean ); cdecl;
begin
  if not Assigned( ViewControllerMultiCaster ) then
    ViewControllerMultiCaster := TDPFViewControllerMultiCaster.Create( Application );

  ViewControllerMultiCaster.DoViewDidAppear( Self, animated );

  // objc_msgSend( Self, sel_getUid( 'viewDidAppearOriginal:' ) );
  TOriginalViewDidAppear( POriginalViewDidAppear )( self, _cmd, animated );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFViewControllerEvents }
constructor TDPFViewControllerEvents.Create( AOwner: TComponent );
begin
  inherited;
  if not( csDesigning in ComponentState ) then
  begin
    if not Assigned( ViewControllerMultiCaster ) then
      ViewControllerMultiCaster := TDPFViewControllerMultiCaster.Create( Application );

    ViewControllerMultiCaster.AddViewControllerEvent( Self );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFViewControllerEvents.DoViewDidAppear( View: Pointer; Animated: Boolean );
begin
  if Assigned( FOnViewDidAppear ) then
    FOnViewDidAppear( Self, Animated );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewControllerEvents.SetOnViewDidAppear( const Value: TViewDidAppearEvent );
begin
  FOnViewDidAppear := Value;
end;

// ------------------------------------------------------------------------------
{ TDPFViewControllerMultiCaster }
procedure TDPFViewControllerMultiCaster.AddViewControllerEvent( Event: TDPFViewControllerEvents );
begin
  if FVCEvents.IndexOf( Event ) = -1 then
    FVCEvents.Add( Event );
end;

// ------------------------------------------------------------------------------
constructor TDPFViewControllerMultiCaster.Create( AOwner: TComponent );
begin
  inherited;
  FVCEvents     := TObjectList<TDPFViewControllerEvents>.Create;
  FIsHookActive := false;
  Hook;
end;

// ------------------------------------------------------------------------------
destructor TDPFViewControllerMultiCaster.Destroy;
begin
  FVCEvents.Free;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFViewControllerMultiCaster.DoViewDidAppear( View: Pointer; Animated: Boolean );
var
  I: Integer;
begin
  for I := FVCEvents.Count - 1 downto 0 do
    FVCEvents[I].DoViewDidAppear( View, Animated );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewControllerMultiCaster.Hook;
{$IFDEF IOS}
var
  ViewController: UIViewController;
  ViewClass     : Pointer;
  M1, M2        : Pointer;
  didAddMethod  : Integer;
  en            : MarshaledAString;
{$ENDIF}
begin
{$IFDEF IOS}
  if not FIsHookActive then
  begin
    FIsHookActive  := True;
    ViewController := TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).keyWindow.rootViewController;

    ViewClass := object_getclass( ( ViewController as ILocalObject ).GetObjectID );
    class_addMethod( ViewClass, sel_getUid( 'viewDidAppearOriginal:' ), @vceViewDidAppear, 'v@:@B' );

    M1 := class_getInstanceMethod( ViewClass, sel_getUid( 'viewDidAppear:' ) );
    M2 := class_getInstanceMethod( ViewClass, sel_getUid( 'viewDidAppearOriginal:' ) );

    POriginalViewDidAppear := method_getImplementation( M1 );

    en           := method_getTypeEncoding( M2 );
    didAddMethod := class_addMethod( ViewClass, M1, M2, en );

    if didAddMethod = 1 then
      // class_replaceMethod( ViewClass, M1, M2, method_getTypeEncoding( M2 ) )
      method_setImplementation( M1, @vceViewDidAppear )
    else
      method_exchangeImplementations( M1, M2 );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFViewControllerMultiCaster.RemoveViewControllerEvent( Event: TDPFViewControllerEvents );
begin
  FVCEvents.Remove( Event );
end;

// ------------------------------------------------------------------------------
end.
