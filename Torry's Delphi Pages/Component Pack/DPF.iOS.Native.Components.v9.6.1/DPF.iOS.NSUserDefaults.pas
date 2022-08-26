// ------------------------------------------------------------------------------
// DPF.iOS.NSUserDefaults Component
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
unit DPF.iOS.NSUserDefaults;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.IniFiles,
  System.TypInfo,
  System.Math,
  System.DateUtils,

  DPF.iOS.BaseControl,
  DPF.iOS.UIAlertView,
  DPF.iOS.StoreKit,
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
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls;

type
  TDPFUserDefaults = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  IDPFNSNotificationCenterHandler = interface( NSObject )
    ['{70CD840A-7F21-4CB6-99F4-EA45EAE2A48C}']

    procedure defaultsChanged( notification: NSNotification ); cdecl;

  end;

  // ------------------------------------------------------------------------------
  TDPFNSNotificationCenterHandler = class( TOCLocal )
  private

  private
    FDPFUserDefaults: TDPFUserDefaults;

  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor create( ADPFUserDefaults: TDPFUserDefaults );

    procedure defaultsChanged( notification: NSNotification ); cdecl;
  end;

{$ENDIF}

  TDPFUserDefaultsChanged = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUserDefaults = class( TComponent )
  private
    FOnUserDefaultsChanged: TDPFUserDefaultsChanged;
{$IFDEF IOS}
    FDPFNSNotificationCenterHandler: TDPFNSNotificationCenterHandler;
{$ENDIF}
  protected

  public
{$IFDEF IOS}
    procedure Loaded; override;

    function GetBooleanValue( key: string ): boolean;
    function GetDoubleValue( key: string ): double;
    function GetInt64Value( key: string ): nativeuint;
    function GetSingleValue( key: string ): single;
    function GetStringValue( key: string; const DefaultValue: string = '' ): string;
    function GetStringArrayValue( key: string ): TArray<string>;

    procedure SetBooleanValue( key: string; value: boolean );
    procedure SetDoubleValue( key: string; value: double );
    procedure SetInt64Value( key: string; value: nativeuint );
    procedure SetSingleValue( key: string; value: single );
    procedure SetStringValue( key: string; value: string );
    procedure SetStringArrayValue( key: string; values: TArray<string> );

    function synchronize: Boolean;

{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnUserDefaultsChanged: TDPFUserDefaultsChanged read FOnUserDefaultsChanged write FOnUserDefaultsChanged;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFUserDefaults }
// ------------------------------------------------------------------------------
constructor TDPFUserDefaults.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF IOS}
  FDPFNSNotificationCenterHandler := TDPFNSNotificationCenterHandler.create( self );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFNSNotificationCenterHandler.GetObjectID, sel_getUid( 'defaultsChanged:' ), ( NSStr( 'NSUserDefaultsDidChangeNotification' ) as ILocalObject ).GetObjectID, nil );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUserDefaults.Destroy;
begin
{$IFDEF IOS}
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFNSNotificationCenterHandler.GetObjectID );
  FDPFNSNotificationCenterHandler.DisposeOf;
{$ENDIF}
  inherited;
end;
// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUserDefaults.Loaded;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

function TDPFUserDefaults.GetBooleanValue( key: string ): boolean;
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  result    := UDefaults.boolForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.GetDoubleValue( key: string ): double;
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  result    := UDefaults.doubleForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.GetInt64Value( key: string ): nativeuint;
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  result    := UDefaults.integerForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.GetSingleValue( key: string ): single;
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  result    := UDefaults.floatForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.GetStringValue( key: string; const DefaultValue: string = '' ): string;
var
  UDefaults: NSUserDefaults;
  nStr     : NSString;
begin
  if key = '' then
    exit;
  result    := DefaultValue;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  nStr      := UDefaults.stringForKey( NSStr( key ) );
  if Assigned( nStr ) then
    result := UTF8ToString( nStr.UTF8String );
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetBooleanValue( key: string; value: boolean );
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.setBool( value, NSStr( Key ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetDoubleValue( key: string; value: double );
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.setDouble( value, NSStr( Key ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetInt64Value( key: string; value: nativeuint );
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.setInteger( value, NSStr( Key ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetSingleValue( key: string; value: single );
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;

  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.setFloat( value, NSStr( Key ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetStringArrayValue( key: string; values: TArray<string> );
var
  UDefaults: NSUserDefaults;
  vls      : NSMutableArray;
  i        : Integer;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );

  vls   := TNSMutableArray.Create;
  for I := 0 to high( values ) do
    vls.addObject( ( NSSTR( values[i] ) as ILocalObject ).GetObjectID );

  UDefaults.setObject( ( vls as ILocalObject ).GetObjectID, NSStr( Key ) );
  vls.release;
end;

// ------------------------------------------------------------------------------
procedure TDPFUserDefaults.SetStringValue( key, value: string );
var
  UDefaults: NSUserDefaults;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.setObject( ( NSStr( value ) as ILocalObject ).GetObjectID, NSStr( Key ) );
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.Synchronize: Boolean;
var
  UDefaults: NSUserDefaults;
begin
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );
  UDefaults.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFUserDefaults.GetStringArrayValue( key: string ): TArray<string>;
var
  UDefaults: NSUserDefaults;
  ar       : NSArray;
  i        : integer;
begin
  if key = '' then
    exit;
  UDefaults := TNSUserDefaults.Wrap( TNSUserDefaults.OCClass.standardUserDefaults );

  ar := UDefaults.stringArrayForKey( NSStr( key ) );
  SetLength( Result, ar.count );
  if ar.count = 0 then
    exit;
  for I       := 0 to ar.count - 1 do
    Result[i] := UTF8ToString( TNSString.Wrap( ar.objectAtIndex( i ) ).UTF8String );

end;

// ------------------------------------------------------------------------------
{ TDPFNSNotificationCenterHandler }
constructor TDPFNSNotificationCenterHandler.create( ADPFUserDefaults: TDPFUserDefaults );
begin
  inherited create;
  FDPFUserDefaults := ADPFUserDefaults;
end;

// ------------------------------------------------------------------------------
function TDPFNSNotificationCenterHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IDPFNSNotificationCenterHandler );
end;

// ------------------------------------------------------------------------------
procedure TDPFNSNotificationCenterHandler.defaultsChanged( notification: NSNotification );
begin
  if Assigned( FDPFUserDefaults.FOnUserDefaultsChanged ) then
    FDPFUserDefaults.FOnUserDefaultsChanged( FDPFUserDefaults );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
