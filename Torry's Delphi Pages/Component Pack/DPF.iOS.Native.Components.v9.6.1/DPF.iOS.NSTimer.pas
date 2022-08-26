// ------------------------------------------------------------------------------
// DPF.iOS.NSTimer Component
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
// ------------------------------------------------------------------------------
unit DPF.iOS.NSTimer;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  System.Math,
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
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFNSTimer = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFTimerDelegate = interface( IObjectiveC )
    ['{2B76D2FE-03CA-4D87-BB45-D4B475331915}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  TDPFTimerDelegate = class( TOCLocal, IDPFTimerDelegate )
  private
    FDPFNSTimer: TDPFNSTimer;
  public
    constructor Create( ADPFNSTimer: TDPFNSTimer );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFNSTimer = class( TComponent )
  private
{$IFDEF IOS}
    FNSTimer         : NSTimer;
    FDPFTimerDelegate: TDPFTimerDelegate;
{$ENDIF}
    FOnTimer : TNotifyEvent;
    FEnabled : Boolean;
    FInterval: Longword;
    procedure SetEnabled( const Value: Boolean );
    procedure SetInterval( const Value: Longword );

  protected
    isClicked: Boolean;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property Enabled : Boolean read FEnabled write SetEnabled default false;
    property Interval: Longword read FInterval write SetInterval default 1000;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFNSTimer }
// ------------------------------------------------------------------------------
constructor TDPFNSTimer.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FEnabled  := false;
  FInterval := 1000;
{$IFDEF IOS}
  FNSTimer := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFNSTimer.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;
  if assigned( FDPFTimerDelegate ) then
    FDPFTimerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFNSTimer.SetEnabled( const Value: Boolean );
begin
{$IFDEF IOS}
  if FEnabled = Value then
    exit;
  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;
  if value then
  begin
    if not assigned( FDPFTimerDelegate ) then
      FDPFTimerDelegate := TDPFTimerDelegate.Create( Self );
    FNSTimer            := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( FInterval / 1000, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );
  end;
{$ENDIF}
  FEnabled := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFNSTimer.SetInterval( const Value: Longword );
begin
  FInterval := Value;
  SetEnabled( FEnabled );
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFTimerDelegate }
constructor TDPFTimerDelegate.Create( ADPFNSTimer: TDPFNSTimer );
begin
  inherited Create;
  FDPFNSTimer := ADPFNSTimer;
end;

// ------------------------------------------------------------------------------
procedure TDPFTimerDelegate.ondidTimer( timer: NSTimer );
begin
  if Assigned( FDPFNSTimer.FOnTimer ) then
    FDPFNSTimer.FOnTimer( FDPFNSTimer );
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
