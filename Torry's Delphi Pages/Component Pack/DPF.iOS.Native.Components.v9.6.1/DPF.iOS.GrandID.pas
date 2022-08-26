// ------------------------------------------------------------------------------
// DPF.iOS.GrandID Component
// 931288
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
unit DPF.iOS.GrandID;

interface

uses
  Classes,
  SysUtils,
  zlib,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
  DPF.iOS.BaseControl,
  DPF.iOS.Common
{$IFDEF IOS}
    , TypInfo,
  Macapi.ObjCRuntime,
  MacApi.ObjectiveC,
  Macapi.CoreFoundation,
  iOSApi.CocoaTypes,

  iOSApi.Foundation,
  iOSApi.UIKit,
  iOSApi.QuartzCore,
  iOSApi.CoreMedia,
  iOSApi.CoreVideo,
  iOSApi.AVFoundation
{$ENDIF}
    ;

const
{$IFDEF UNDERSCOREIMPORTNAME}
  _PU = '_';
{$ELSE}
  _PU = '';
{$ENDIF}
{$EXTERNALSYM _PU}
  libExternalAccessory = '/System/Library/Frameworks/ExternalAccessory.framework/ExternalAccessory';
  libxml2              = '/usr/lib/libxml2.dylib';

type
  TGrandID = class;

{$IFDEF IOS}
  GrandIDSoapBindingResponse  = interface;
  GrandIDSoapBindingOperation = interface;
  GrandIDSoapBinding          = interface;

  // ----------------------------------------------------------------------------

  GrandIDSoapBindingResponseDelegateClass = interface( NSObjectClass )
    ['{0CA8A6FC-E2D2-41A8-B4C4-8082C82DCE5C}']
  end;

  GrandIDSoapBindingResponseDelegate = interface( IObjectiveC )
    ['{9D246AB6-95E0-4F48-9DF1-1DE2C8264BB1}']

    procedure operation( operation: GrandIDSoapBindingOperation; completedWithResponse: GrandIDSoapBindingResponse ); cdecl;
  end;

  TGrandIDSoapBindingResponseDelegate = class( TOCLocal, GrandIDSoapBindingResponseDelegate )
  private
    FDPFGrandID: TGrandID;
  public
    constructor Create( ADPFGrandID: TGrandID );
    // function GetObjectiveCClass: PTypeInfo; override;

    procedure operation( operation: GrandIDSoapBindingOperation; completedWithResponse: GrandIDSoapBindingResponse ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  GrandIDSoapBindingResponseClass = interface( NSObjectClass )
    ['{E53684A8-D25D-402F-8D92-AD4B08068CEC}']

  end;

  GrandIDSoapBindingResponse = interface( NSObject )
    ['{A6978194-5071-4207-8DDB-643CBB7170A7}']
    function headers: NSArray; cdecl;
    procedure setHeaders( headers: NSArray ); cdecl;

    function bodyParts: NSArray; cdecl;
    procedure setBodyParts( bodyParts: NSArray ); cdecl;

    function error: NSError; cdecl;
    procedure setError( error: NSError ); cdecl;
  end;

  TGrandIDSoapBindingResponse = class( TOCGenericImport<GrandIDSoapBindingResponseClass, GrandIDSoapBindingResponse> )
  end;

  // ----------------------------------------------------------------------------

  GrandIDSoapBindingOperationClass = interface( NSOperationClass )
    ['{3FD7755F-D0C8-4089-A242-79CCD3CE4F74}']
  end;

  GrandIDSoapBindingOperation = interface( NSOperation )
    ['{B1955DD8-739C-451A-A076-C28331D6E6F6}']

    function binding: GrandIDSoapBinding; cdecl;
    procedure setBinding( binding: GrandIDSoapBinding ); cdecl;

    function response: GrandIDSoapBindingResponse; cdecl;

    function delegate: GrandIDSoapBindingResponseDelegate; cdecl;
    procedure setDelegate( delegate: GrandIDSoapBindingResponseDelegate ); cdecl;

    function responseData: NSMutableData; cdecl;
    procedure setResponseData( responseData: NSMutableData ); cdecl;

    function urlConnection: NSURLConnection; cdecl;
    procedure setUrlConnection( urlConnection: NSURLConnection ); cdecl;

    function initWithBinding( aBinding: GrandIDSoapBinding; delegate: pointer { GrandIDSoapBindingResponseDelegate } ): Pointer; cdecl;

  end;

  TGrandIDSoapBindingOperation = class( TOCGenericImport<GrandIDSoapBindingOperationClass, GrandIDSoapBindingOperation> )
  end;

  // ----------------------------------------------------------------------------
  GrandIDSoapBindingClass = interface( NSObjectClass )
    ['{135A5911-13F6-4899-9314-03CEC6BA929A}']

  end;

  GrandIDSoapBinding = interface( NSObject )
    ['{51B7C49B-A644-4D01-8FBE-D98F5031313E}']

    function initWithAddress( anAddress: NSString ): Pointer; cdecl;
    procedure sendHTTPCallUsingBody( body: NSString; soapAction: NSString; forOperation: GrandIDSoapBindingOperation ); cdecl;
    procedure addCookie( toAdd: NSHTTPCookie ); cdecl;

    function FederatedDirectLoginUsingApiKey( aApiKey: NSString; authenticateServiceKey: NSString; serviceProviderId: NSString; loginMethod: NSString; userCatalogId: NSString; CertificateLogin: NSString; Username: NSString; Password: NSString; twoFactorLoginType: NSString; twoFactorLoginValue: NSString; ipAddressOfEndUser: NSString )
      : GrandIDSoapBindingResponse; cdecl;
    function FederatedLoginUsingApiKey( aApiKey: NSString; authenticateServiceKey: NSString; loginMethodId: NSString; callbackUrl: NSString; onErrorReturnToThisUrl: NSString; userCatalogId: NSString; useIdpConnectorId: NSString; excludeTwoFactor: NSString; ipAddressOfEndUser: NSString ): GrandIDSoapBindingResponse; cdecl;

    procedure FederatedDirectLoginAsyncUsingApiKey( aApiKey: NSString; authenticateServiceKey: NSString; serviceProviderId: NSString; loginMethod: NSString; userCatalogId: NSString; CertificateLogin: NSString; Username: NSString; Password: NSString; twoFactorLoginType: NSString; twoFactorLoginValue: NSString; ipAddressOfEndUser: NSString;
      delegate: GrandIDSoapBindingResponseDelegate ); cdecl;
    procedure FederatedLoginAsyncUsingApiKey( aApiKey: NSString; authenticateServiceKey: NSString; loginMethodId: NSString; callbackUrl: NSString; onErrorReturnToThisUrl: NSString; userCatalogId: NSString; useIdpConnectorId: NSString; excludeTwoFactor: NSString; ipAddressOfEndUser: NSString; delegate: Pointer ); cdecl;

    function GetSessionUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString ): GrandIDSoapBindingResponse; cdecl;
    procedure GetSessionAsyncUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString; responseDelegate: GrandIDSoapBindingResponseDelegate ); cdecl;
    function LogoutUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString ): GrandIDSoapBindingResponse; cdecl;
    procedure LogoutAsyncUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString; responseDelegate: GrandIDSoapBindingResponseDelegate ); cdecl;
    function GetSessionGmailUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString ): GrandIDSoapBindingResponse; cdecl;
    procedure GetSessionGmailAsyncUsingSessionid( aSessionid: NSString; apiKey: NSString; authenticate_service_key: NSString; responseDelegate: GrandIDSoapBindingResponseDelegate ); cdecl;

  end;

  TGrandIDSoapBinding = class( TOCLocal )
  private
    FDPFGrandID: TGrandID;
  public
    constructor Create( ADPFGrandID: TGrandID );
    function GetObjectiveCClass: PTypeInfo; override;

  end;

  { TGrandIDSoapBinding = class( TOCGenericImport<GrandIDSoapBindingClass, GrandIDSoapBinding> )
    end; }

  // ----------------------------------------------------------------------------
  MobileAccessLibClass = interface( NSObjectClass )
    ['{9272069D-1AD2-4D43-8431-BCAF8E01931B}']
    function GrandIDSoapBinding: GrandIDSoapBinding { pointer }; cdecl;
    function getXMLProperty( _xmlData: NSString; &type: NSString ): NSString; cdecl;
  end;

  MobileAccessLib = interface( NSObject )
    ['{97FADE16-65FA-4E39-BC10-08F90263C66A}']
  end;

  TMobileAccessLib = class( TOCGenericImport<MobileAccessLibClass, MobileAccessLib> )
  end;

  // ----------------------------------------------------------------------------
  TBXML = interface( NSObject )
    ['{EFE93E4C-7DDC-42BD-B838-7F037EAEB0F2}']
  end;

  // ----------------------------------------------------------------------------
  GrandIDSoapBinding_envelopeClass = interface( NSObjectClass )
    ['{583F7E30-4941-4445-9720-769E17E1833B}']
  end;

  // ----------------------------------------------------------------------------
  GrandIDSoapBinding_envelope = interface( NSObject )
    ['{583F7E30-4941-4445-9720-769E17E1833B}']
    function sharedInstance: pointer; cdecl;
    function serializedFormUsingHeaderElements( headerElements: NSDictionary; bodyElements: NSDictionary ): NSString; cdecl;
  end;

  // ----------------------------------------------------------------------------
  PKICard = interface( NSObject )
    ['{B3E7E4C4-2634-4F9D-8227-624BF0F075B8}']
  end;

  PKICardClass = interface( NSObjectClass )
    ['{7FB68B5F-16A9-4591-8F42-E78086F7D5D8}']
  end;

  TPKICard = class( TOCGenericImport<PKICardClass, PKICard> )
  end;

  // ----------------------------------------------------------------------------
  PBAccessory = interface( NSObject )
    ['{A6F338C0-C725-4902-B82D-2E252705F174}']
  end;

  PBAccessoryClass = interface( NSObjectClass )
    ['{D7946529-588B-42BA-B604-FAB2519BD620}']
  end;

  TPBAccessory = class( TOCGenericImport<PBAccessoryClass, PBAccessory> )
  end;

  // ----------------------------------------------------------------------------
  PKICardEngine = interface( NSObject )
    ['{6444FCF2-F308-416E-906E-18B200B7F1A7}']
  end;

  PKICardEngineClass = interface( NSObjectClass )
    ['{5A6E9EA4-5779-4972-B4E8-118EE9282195}']
  end;

  TPKICardEngine = class( TOCGenericImport<PKICardEngineClass, PKICardEngine> )
  end;

  // ----------------------------------------------------------------------------
  EAAccessoryManager = interface( NSObject )
    ['{1355624A-8F02-40A9-8F98-6D81127AEAED}']
    // procedure showBluetoothAccessoryPickerWithNameFilter(predicate: NSPredicate; completion: Pointer); cdecl;
    procedure registerForLocalNotifications; cdecl;
    procedure unregisterForLocalNotifications; cdecl;
    function connectedAccessories: NSArray; cdecl;
  end;

  EAAccessoryManagerClass = interface( NSObjectClass )
    ['{43CD8175-3CC4-4D17-9C00-F74A8226169B}']
    function sharedAccessoryManager: Pointer; cdecl;
  end;

  TEAAccessoryManager = class( TOCGenericImport<EAAccessoryManagerClass, EAAccessoryManager> )
  end;

  // ----------------------------------------------------------------------------
  PKIEngineControllerClass = interface( UIViewControllerClass )
    ['{6CCF82E1-F2B7-4009-B3A1-DC4BE50E75D5}']
  end;

  PKIEngineController = interface( UIViewController )
    ['{C2E0C58D-08D7-42C2-AD6D-7A29EE543F0D}']
    procedure startPKIEngine; cdecl;

    // procedure threadPKIEngine; cdecl;

    // card/reader action functions
    // after calling startPKIEngine function, you can get status of card/reader using
    function isCardLeaderPresent: Boolean; cdecl;
    function isCardPresent: Boolean; cdecl;

    // Events
    // procedure cardIsInserted; cdecl;
    // procedure cardIsRemoved; cdecl;
    // procedure cardReaderIsInserted; cdecl;
    // procedure cardReaderIsRemoved; cdecl;

  end;

  TPKIEngineController = class( TOCGenericImport<PKIEngineControllerClass, PKIEngineController> )
  end;

  DPFPKIEngineController = interface( PKIEngineController )
    ['{21935680-BA6A-40EA-98D4-0989D978DAA3}']

    function isCardLeaderPresent: Boolean; cdecl;
    function isCardPresent: Boolean; cdecl;

    // Events
    procedure cardIsInserted; cdecl;
    procedure cardIsRemoved; cdecl;
    procedure cardReaderIsInserted; cdecl;
    procedure cardReaderIsRemoved; cdecl;

    // procedure threadPKIEngine; cdecl;

    // procedure operation( operation: GrandIDSoapBindingOperation; completedWithResponse: GrandIDSoapBindingResponse ); cdecl;
  end;

  TDPFPKIEngineController = class( TOCLocal )
  private
  protected
    FDPFGrandID: TGrandID;
  public
    constructor Create( ADPFGrandID: TGrandID );
    function GetObjectiveCClass: PTypeInfo; override;

    function isCardLeaderPresent: Boolean; cdecl;
    function isCardPresent: Boolean; cdecl;

    procedure cardIsInserted; cdecl;
    procedure cardIsRemoved; cdecl;
    procedure cardReaderIsInserted; cdecl;
    procedure cardReaderIsRemoved; cdecl;

    // procedure threadPKIEngine; cdecl;
  end;

{$ENDIF}

  TCardIsInserted       = procedure( sender: TObject ) of object;
  TCardIsRemoved        = procedure( sender: TObject ) of object;
  TCardReaderIsInserted = procedure( sender: TObject ) of object;
  TCardReaderIsRemoved  = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TGrandID = class( TDPFiOSBaseControl )
  private
    FOnCardReaderIsInserted: TCardReaderIsInserted;
    FOnCardIsInserted      : TCardIsInserted;
    FOnCardIsRemoved       : TCardIsRemoved;
    FOnCardReaderIsRemoved : TCardReaderIsRemoved;

{$IFDEF IOS}
    FGrandIDSoapBindingResponseDelegate: TGrandIDSoapBindingResponseDelegate;
    FDPFPKIEngineController            : TDPFPKIEngineController;
    FPKIEngineController               : PKIEngineController;

    createdPKISession, sithsLogResponse: NSString;
{$ENDIF}
  public
    procedure Loaded;

    procedure startPKIEngine;
    function isCardLeaderPresent: Boolean;
    function isCardPresent: Boolean;

    procedure Login;
    procedure FederatedLogin( const _apiKey: string; const authenticateServiceKey: string );

    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published

    property OnCardIsInserted      : TCardIsInserted read FOnCardIsInserted write FOnCardIsInserted;
    property OnCardIsRemoved       : TCardIsRemoved read FOnCardIsRemoved write FOnCardIsRemoved;
    property OnCardReaderIsInserted: TCardReaderIsInserted read FOnCardReaderIsInserted write FOnCardReaderIsInserted;
    property OnCardReaderIsRemoved : TCardReaderIsRemoved read FOnCardReaderIsRemoved write FOnCardReaderIsRemoved;

  end;

{$IFDEF IOS}

  // function EAAccessoryDidDisconnectNotification: NSString;
function EAAccessoryDidDisconnectNotification: NSString; cdecl; external libExternalAccessory name _PU + 'EAAccessoryDidDisconnectNotification';

function xmlAddChild( parent: Pointer; cur: Pointer ): Pointer; cdecl; external libxml2 name _PU + 'xmlAddChild';

{$O-}
// function libMobileAccess00: TBXML; cdecl; external 'Lib\libMobileAccess.a' name 'OBJC_CLASS_$_TBXML';
// function libMobileAccess01: GrandIDSoapBinding_envelope; cdecl; external 'Lib\libMobileAccess.a' name 'OBJC_CLASS_$_GrandIDSoapBinding_envelope';
// function libMobileAccess02: GrandIDSoapBindingOperation; cdecl; external 'Lib\libMobileAccess.a' name 'OBJC_CLASS_$_GrandIDSoapBindingOperation';
function libMobileAccess03: MobileAccessLib; cdecl; external 'Lib\libMobileAccess.a' name 'OBJC_CLASS_$_MobileAccessLib';

// function libPKICard00: EAAccessoryManager; cdecl; external 'Lib\libPKICard.a' name 'OBJC_CLASS_$_EAAccessoryManager';
// function libPKICard01: PKICard; cdecl; external 'Lib\libPKICard.a' name 'OBJC_CLASS_$_PKICard';
// function libPKICard02: PKICardEngine; cdecl; external 'Lib\libPKICard.a' name 'OBJC_CLASS_$_PKICardEngine';
function libPKICard03: PKIEngineController; cdecl; external 'Lib\libPKICard.a' name 'OBJC_CLASS_$_PKIEngineController';
// function libPKICard03: PBAccessory; cdecl; external 'Lib\libPKICard.a' name 'OBJC_CLASS_$_PBAccessory';

function CFURLCreateStringByAddingPercentEscapes( allocator: CFAllocatorRef; originalString: CFStringRef; charactersToLeaveUnescaped: CFStringRef; legalURLCharactersToBeEscaped: CFStringRef; encoding: CFStringEncoding ): CFStringRef; cdecl; external libFoundation name _PU + 'CFURLCreateStringByAddingPercentEscapes';
// function CFBridgingRelease(X: CFTypeRef): pointer; cdecl; external libFoundation name 'CFBridgingRelease';

{$O+}
// function zbar_symbol_set_first_symbol(symbols : zbar_symbol_set_t): zbar_symbol_t; cdecl; external 'ZBarSDK\libPKICard.a' name 'zbar_symbol_set_first_symbol';
{$ENDIF}
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents( 'D.P.F iOS GrandID', [TGrandID] );
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ function EAAccessoryDidDisconnectNotification: NSString;
  begin
  result := CocoaNSStringConst( libExternalAccessory, 'EAAccessoryDidDisconnectNotification' );
  end; }

// ------------------------------------------------------------------------------
function GetSharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TGrandID }
constructor TGrandID.Create( AOwner: TComponent );
begin
  inherited;
{$IFDEF IOS}
  FDPFPKIEngineController := TDPFPKIEngineController.Create( Self );
  FPKIEngineController    := TPKIEngineController.Wrap( FDPFPKIEngineController.Super.init );
  FUIControl              := FPKIEngineController;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TGrandID.Destroy;
begin
{$IFDEF IOS}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TGrandID.Loaded;
begin
{$IFDEF IOS}
  DPFNSLog( 'Loaded' );
  addSubview( Self, ParentControl, FPKIEngineController.view );

  // ----------------------------
  // Important
  if isRootControl then
    resize;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TGrandID.FederatedLogin( const _apiKey: string; const authenticateServiceKey: string );
var
  grandBinding: GrandIDSoapBinding;
  gr          : GrandIDSoapBindingResponse;
begin
  if not assigned( FGrandIDSoapBindingResponseDelegate ) then
    FGrandIDSoapBindingResponseDelegate := TGrandIDSoapBindingResponseDelegate.Create( self );

  grandBinding := TMobileAccessLib.OCClass.GrandIDSoapBinding;
  grandBinding.FederatedLoginAsyncUsingApiKey( NSStr( _apiKey ), NSStr( authenticateServiceKey ), NSStr( 'tabletpki' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), FGrandIDSoapBindingResponseDelegate.GetObjectID { Sel_getUid( 'operation:' ) } );
  // gr := grandBinding.FederatedLoginUsingApiKey( NSStr( _apiKey ), NSStr( authenticateServiceKey ), NSStr( 'tabletpki' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ), NSStr( '0' ) );

  // [grandBinding FederatedDirectLoginAsyncUsingApiKey:_apiKey authenticateServiceKey:_authenticateServiceKey serviceProviderId:@"0" loginMethod:@"0" userCatalogId:@"0" CertificateLogin:@"0" Username:_username Password:_password twoFactorLoginType:@"0" twoFactorLoginValue:@"0" ipAddressOfEndUser:@"0" delegate:self];
end;

// ------------------------------------------------------------------------------
procedure TGrandID.Login;
var
  grandBinding: GrandIDSoapBinding;
begin
{$IFDEF IOS}
  // grandBinding := TGrandIDSoapBinding.Wrap( TMobileAccessLib.OCClass.GrandIDSoapBinding );
  FederatedLogin( '6c6e4f14-713e-4837-a60b-996e53655dd7', 'e035a6f1-c1b9-4f6f-8ebe-64f8e9da04c1' );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TGrandID.startPKIEngine;
begin
{$IFDEF IOS}
  DPFNSLog( 'startPKIEngine' );
  FPKIEngineController.startPKIEngine;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TGrandID.isCardLeaderPresent: Boolean;
begin
  result := true;
{$IFDEF IOS}
  result := FPKIEngineController.isCardLeaderPresent;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TGrandID.isCardPresent: Boolean;
begin
{$IFDEF IOS}
  result := FPKIEngineController.isCardPresent;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

constructor TDPFPKIEngineController.Create( ADPFGrandID: TGrandID );
var
  V: Pointer;
begin
  inherited Create;
  FDPFGrandID := ADPFGrandID;
  V           := PKIEngineController( Super ).initWithNibName( nil, nil );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFPKIEngineController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFPKIEngineController );
end;

// ------------------------------------------------------------------------------
procedure TDPFPKIEngineController.cardIsInserted;
begin
  // PKIEngineController( Super ).cardIsInserted;
  if assigned( FDPFGrandID.FOnCardIsInserted ) then
    FDPFGrandID.FOnCardIsInserted( FDPFGrandID );
end;

// ------------------------------------------------------------------------------
procedure TDPFPKIEngineController.cardIsRemoved;
begin
  if assigned( FDPFGrandID.FOnCardIsRemoved ) then
    FDPFGrandID.FOnCardIsRemoved( FDPFGrandID );
end;

// ------------------------------------------------------------------------------
procedure TDPFPKIEngineController.cardReaderIsInserted;
begin
  if assigned( FDPFGrandID.FOnCardReaderIsInserted ) then
    FDPFGrandID.FOnCardReaderIsInserted( FDPFGrandID );
end;

// ------------------------------------------------------------------------------
procedure TDPFPKIEngineController.cardReaderIsRemoved;
begin
  if assigned( FDPFGrandID.OnCardReaderIsRemoved ) then
    FDPFGrandID.OnCardReaderIsRemoved( FDPFGrandID );
end;

// ------------------------------------------------------------------------------
function TDPFPKIEngineController.isCardLeaderPresent: Boolean; cdecl;
begin
  result := DPFPKIEngineController( Super ).isCardLeaderPresent
end;

// ------------------------------------------------------------------------------
function TDPFPKIEngineController.isCardPresent: Boolean; cdecl;
begin
  result := DPFPKIEngineController( Super ).isCardPresent;
end;

// ------------------------------------------------------------------------------
{ TGrandIDSoapBindingResponseDelegate }
constructor TGrandIDSoapBindingResponseDelegate.Create( ADPFGrandID: TGrandID );
begin
  inherited create;
  FDPFGrandID := ADPFGrandID;
end;

// ------------------------------------------------------------------------------
{ function TGrandIDSoapBindingResponseDelegate.GetObjectiveCClass: PTypeInfo;
  begin
  Result := TypeInfo( GrandIDSoapBindingResponseDelegate );
  end; }

// ------------------------------------------------------------------------------
procedure TGrandIDSoapBindingResponseDelegate.operation( operation: GrandIDSoapBindingOperation; completedWithResponse: GrandIDSoapBindingResponse );
var
  theXml       : NSString;
  range        : NSRange;
  params       : NSString;
  paramString  : NSString;
  sessionStatus: NSString;
begin
  // theXml :=   [[NSString alloc] initWithBytes:[operation.responseData mutableBytes] length:[operation.responseData length] encoding:NSUTF8StringEncoding];
  theXml := TNSString.Wrap( TNSString.Alloc.initWithBytes( operation.responseData.mutableBytes, operation.responseData.length, NSUTF8StringEncoding ) );
  theXml := theXml.stringByReplacingOccurrencesOfString( NSStr( '&lt;' ), NSStr( '<' ) );
  theXml := theXml.stringByReplacingOccurrencesOfString( NSStr( '&gt;' ), NSStr( '>' ) );

  DPFNSLog( theXml.UTF8String );

  range := theXml.rangeOfString( NSStr( 'ns1:FederatedLoginResponse' ) );

  if range.location <> NSNotFound then
  begin
    FDPFGrandID.createdPKISession := TMobileAccessLib.OCClass.getXMLProperty( theXml, NSStr( 'sessionid' ) );

    // if assigned(FDPFGrandID.createdPKISession) Then loginBtn.enabled = YES;

    params      := TNSString.Wrap( TNSString.OCClass.stringWithFormat( NSStr( 'wsdldemo://tab=siths&sessionid=' + FDPFGrandID.createdPKISession.UTF8String ) ) );
    paramString := TNSString.Wrap( TNSString.OCClass.stringWithFormat( NSStr( 'netid:///?autostarttoken=' + FDPFGrandID.createdPKISession.UTF8String + '&redirect=' + params.UTF8String ) ) );
    DPFNSLog( paramString.UTF8String );

    if TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).openURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( paramString ) ) ) then
    begin

    end

  end
  else
  begin
    sessionStatus := TMobileAccessLib.OCClass.getXMLProperty( theXml, NSStr( 'sessionstatus' ) );

    if sessionStatus.isEqualToString( NSStr( '100' ) ) then
    begin
      FDPFGrandID.sithsLogResponse := theXml;
      FDPFGrandID.FPKIEngineController.performSegueWithIdentifier( NSStr( 'SITHSLOG' ), ( FDPFGrandID.FPKIEngineController as ILocalObject ).GetObjectID );
    end
  end;

end;

constructor TGrandIDSoapBinding.Create( ADPFGrandID: TGrandID );
var
  V: Pointer;
begin
  inherited Create;
  FDPFGrandID := ADPFGrandID;
end;

// ------------------------------------------------------------------------------
function TGrandIDSoapBinding.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( GrandIDSoapBinding );
end;

(*
  procedure TGrandIDSoapBinding.operation( operation: GrandIDSoapBindingOperation; completedWithResponse: GrandIDSoapBindingResponse );
  begin
  { }
  end;
*)

{$ENDIF}

// ------------------------------------------------------------------------------
end.
