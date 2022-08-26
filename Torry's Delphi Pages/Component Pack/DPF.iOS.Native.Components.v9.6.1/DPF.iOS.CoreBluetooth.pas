// ------------------------------------------------------------------------------
// DPF.iOS.CoreBluetooth Wrapped Classes & Interfaces
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
unit DPF.iOS.CoreBluetooth;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
{$IFDEF IOS}
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.CoreLocation,
{$ENDIF}
  DPF.iOS.Common,
  DPF.iOS.Classes,
  DPF.iOS.Dispatch,
  FMX.Dialogs;

const
  libCoreBluetooth = '/System/Library/Frameworks/CoreBluetooth.framework/CoreBluetooth';

  // ----------------------------------------------------
  // Characteristic Value Permissions
  CBCharacteristicPropertyBroadcast                  = $01;
  CBCharacteristicPropertyRead                       = $02;
  CBCharacteristicPropertyWriteWithoutResponse       = $04;
  CBCharacteristicPropertyWrite                      = $08;
  CBCharacteristicPropertyNotify                     = $10;
  CBCharacteristicPropertyIndicate                   = $20;
  CBCharacteristicPropertyAuthenticatedSignedWrites  = $40;
  CBCharacteristicPropertyExtendedProperties         = $80;
  CBCharacteristicPropertyNotifyEncryptionRequired   = $100;
  CBCharacteristicPropertyIndicateEncryptionRequired = $200;

  // ----------------------------------------------------
  // Characteristic Value Permissions
  CBAttributePermissionsReadable                = $01;
  CBAttributePermissionsWriteable               = $02;
  CBAttributePermissionsReadEncryptionRequired  = $04;
  CBAttributePermissionsWriteEncryptionRequired = $08;

{$IFDEF IOS}

type
  id                                   = pointer;
  CBCentralManagerState                = ( CBCentralManagerStateUnknown = 0, CBCentralManagerStateResetting = 1, CBCentralManagerStateUnsupported = 2, CBCentralManagerStateUnauthorized = 3, CBCentralManagerStatePoweredOff = 4, CBCentralManagerStatePoweredOn = 5 );
  CBCharacteristicWriteType            = ( CBCharacteristicWriteWithResponse = 0, CBCharacteristicWriteWithoutResponse = 1 );
  CBPeripheralState                    = ( CBPeripheralStateDisconnected = 0, CBPeripheralStateConnecting = 1, CBPeripheralStateConnected = 2 );
  CBPeripheralManagerState             = ( CBPeripheralManagerStateUnknown = 0, CBPeripheralManagerStateResetting = 1, CBPeripheralManagerStateUnsupported = 2, CBPeripheralManagerStateUnauthorized = 3, CBPeripheralManagerStatePoweredOff = 4, CBPeripheralManagerStatePoweredOn = 5 );
  CBPeripheralAuthorizationStatus      = ( CBPeripheralManagerAuthorizationStatusNotDetermined = 0, CBPeripheralManagerAuthorizationStatusRestricted = 1, CBPeripheralManagerAuthorizationStatusDenied = 2, CBPeripheralManagerAuthorizationStatusAuthorized = 3 );
  CBPeripheralManagerConnectionLatency = ( CBPeripheralManagerConnectionLatencyLow = 0, CBPeripheralManagerConnectionLatencyMedium = 1, CBPeripheralManagerConnectionLatencyHigh = 2 );

  CBCharacteristicProperties = NSUInteger;
  CBAttributePermissions     = NSUInteger;

  // ----------------------------------------------------------------------------
  CBCentralManager            = interface;
  CBPeripheral                = interface;
  CBService                   = interface;
  CBCharacteristic            = interface;
  CBPeripheralManagerDelegate = interface;

  // ----------------------------------------------------------------------------
  CFUUIDRef = ^__CFUUID;

  __CFUUID = record
  end;

  // ----------------------------------------------------------------------------
  NSError_1 = interface( NSError )
  end;

  NSError_2 = interface( NSError )
  end;

  NSError_3 = interface( NSError )
  end;

  NSError_4 = interface( NSError )
  end;

  NSError_5 = interface( NSError )
  end;

  NSArray_1 = interface( NSArray )
  end;

  NSArray_2 = interface( NSArray )
  end;
  // ----------------------------------------------------------------------------
  // CoreBluetooth
  // ----------------------------------------------------------------------------

  CBUUIDClass = interface( NSObjectClass )
    ['{8304194B-506C-43C3-90F7-EE2873CA7A62}']
    function UUIDWithString( theString: NSString ): pointer; cdecl;
    function UUIDWithData( theData: NSData ): pointer; cdecl;
    function UUIDWithCFUUID( theUUID: CFUUIDRef ): pointer; cdecl;
    function UUIDWithNSUUID( theUUID: NSUUID ): pointer; cdecl;
  end;

  CBUUID = interface( NSObject )
    ['{2B6B4E10-7697-44FB-ABB0-D64B3A043DE8}']
    function data: NSData; cdecl;
  end;

  TCBUUID = class( TOCGenericImport<CBUUIDClass, CBUUID> )
  end;

  // ---------------------------------------------------------------------------
  CBMutableServiceClass = interface( NSObjectClass )
    ['{96C8D610-3F42-43D1-8CA3-2C13B0AADDD1}']
  end;

  CBMutableService = interface( NSObject )
    ['{04A6B4F0-9DF7-4C33-B1D6-D80849FB5E6C}']

    function initWithType( UUID: CBUUID; isPrimary: boolean ): id; cdecl;
    function UUID: CBUUID; cdecl;
    function isPrimary: boolean; cdecl;
    function characteristics: NSArray; cdecl;
    function includedServices: NSArray; cdecl;
  end;

  TCBMutableService = class( TOCGenericImport<CBMutableServiceClass, CBMutableService> )
  end;

  // ---------------------------------------------------------------------------
  CBMutableDescriptorClass = interface( NSObjectClass )
    ['{4515D6BC-B64A-4C19-98C3-179232AB1A7F}']
  end;

  CBMutableDescriptor = interface( NSObject )
    ['{EDC5CE1A-AF2A-4901-AAC7-27D8A206E81D}']

    function initWithType( UUID: CBUUID; value: id ): id; cdecl;
  end;

  TCBMutableDescriptor = class( TOCGenericImport<CBMutableDescriptorClass, CBMutableDescriptor> )
  end;

  // ---------------------------------------------------------------------------
  CBCentralClass = interface( NSObjectClass )
    ['{39B44003-FE90-47DD-A123-4AB3303C4312}']
  end;

  CBCentral = interface( NSObject )
    ['{43C9DBBA-FF20-43F4-BB9E-ACE1647E11AE}']

    function maximumUpdateValueLength: NSUInteger; cdecl; // Available in iOS 7.0 and later
    function identifier: NSUUID; cdecl;                   // Available in iOS 7.0 and later.
    function UUID: CFUUIDRef; cdecl;                      // Deprecated in iOS 7.0. Use the identifier property instead.
  end;

  TCBCentral = class( TOCGenericImport<CBCentralClass, CBCentral> )
  end;

  // ---------------------------------------------------------------------------
  CBMutableCharacteristicClass = interface( NSObjectClass )
    ['{4042677A-8152-48C6-B243-8E8F0A050415}']
  end;

  CBMutableCharacteristic = interface( NSObject )
    ['{BE48FECE-8086-4A0A-85F4-B32EBC3EB19B}']

    function initWithType( UUID: CBUUID; properties: CBCharacteristicProperties; value: NSData; permissions: CBAttributePermissions ): id; cdecl;
    function UUID: CBUUID; cdecl;
    function value: NSData; cdecl;
    function descriptors: NSArray; cdecl;
    function properties: CBCharacteristicProperties; cdecl;
    function permissions: CBAttributePermissions; cdecl;
    function subscribedCentrals: NSArray; cdecl;
  end;

  TCBMutableCharacteristic = class( TOCGenericImport<CBMutableCharacteristicClass, CBMutableCharacteristic> )
  end;

  // ---------------------------------------------------------------------------
  CBPeripheralManagerClass = interface( NSObjectClass )
    ['{B5465B53-375A-4D92-ABF7-E2FFC4E88B47}']

    function authorizationStatus: CBPeripheralAuthorizationStatus; cdecl;
  end;

  CBPeripheralManager = interface( NSObject )
    ['{8B6E150C-C667-4D7D-851F-41472CCD8E37}']

    function initWithDelegate( delegate: id; queue: dispatch_queue_t ): id; cdecl; overload;
    function initWithDelegate( delegate: id; queue: dispatch_queue_t; options: NSDictionary ): id; cdecl; overload;
    function delegate: id; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
    function state: CBPeripheralManagerState; cdecl;
    procedure addService( service: CBMutableService ); cdecl;
    procedure removeService( service: CBMutableService ); cdecl;
    procedure removeAllServices; cdecl;
    procedure startAdvertising( advertisementData: NSDictionary ); cdecl;
    procedure stopAdvertising; cdecl;
    function isAdvertising: boolean; cdecl;
    function updateValue( value: NSData; forCharacteristic: CBMutableCharacteristic; onSubscribedCentrals: NSArray ): Boolean; cdecl;
    procedure setDesiredConnectionLatency( latency: CBPeripheralManagerConnectionLatency; forCentral: CBCentral ); cdecl;
  end;

  TCBPeripheralManager = class( TOCGenericImport<CBPeripheralManagerClass, CBPeripheralManager> )
  end;

  // ---------------------------------------------------------------------------
  CBDescriptorClass = interface( NSObjectClass )
    ['{B38590D5-36BC-4D23-BE4F-D0AF39F4F612}']
  end;

  CBDescriptor = interface( NSObject )
    ['{E33F6ABF-ED99-4565-BF3F-09A21DDE2966}']

    function UUID: CBUUID; cdecl;
    function characteristic: CBCharacteristic; cdecl;
    function value: pointer; cdecl;
  end;

  TCBDescriptor = class( TOCGenericImport<CBDescriptorClass, CBDescriptor> )
  end;

  // ---------------------------------------------------------------------------
  CBCharacteristicClass = interface( NSObjectClass )
    ['{084B4168-F481-4076-9D3C-ECA694AAAAEF}']
  end;

  CBCharacteristic = interface( NSObject )
    ['{C67EE607-7F52-402C-95D1-9F27643F570D}']

    function UUID: CBUUID; cdecl;
    function service: CBService; cdecl;

    function value: NSData; cdecl;
    function descriptors: NSArray; cdecl;
    function properties: CBCharacteristicProperties; cdecl;
    function isNotifying: Boolean; cdecl;
    function isBroadcasted: Boolean; cdecl;
  end;

  TCBCharacteristic = class( TOCGenericImport<CBCharacteristicClass, CBCharacteristic> )
  end;

  // ---------------------------------------------------------------------------
  CBServiceClass = interface( NSObjectClass )
    ['{1BBF6433-0B06-4E98-99ED-F2C4323CC594}']
  end;

  CBService = interface( NSObject )
    ['{2F48BB61-E388-4ABF-85B4-62A4DC7F1E2B}']

    function UUID: CBUUID; cdecl;
    function peripheral: CBPeripheral; cdecl;
    function isPrimary: Boolean; cdecl;
    function characteristics: NSArray; cdecl;
    function includedServices: NSArray; cdecl;
  end;

  TCBService = class( TOCGenericImport<CBServiceClass, CBService> )
  end;

  // ---------------------------------------------------------------------------
  CBATTRequestClass = interface( NSObjectClass )
    ['{89DD7D04-2F01-4C9F-8427-D2873AE7E422}']
  end;

  CBATTRequest = interface( NSObject )
    ['{33F9C1A2-B7DA-4FB4-95CD-33ACE35D3129}']

    function central: CBCentral; cdecl;
    function characteristic: CBCharacteristic; cdecl;
    function offset: NSUInteger; cdecl;
    function value: NSData; cdecl;
    procedure setValue( value: NSData ); cdecl;
  end;

  TCBATTRequest = class( TOCGenericImport<CBATTRequestClass, CBATTRequest> )
  end;

  // ---------------------------------------------------------------------------
  CBCharacteristic1 = interface( CBCharacteristic )
  end;

  CBPeripheralManagerDelegate = interface( NSObject )
    ['{F8CBBDDE-7FBC-4317-935C-3102300C1C5B}']

    procedure peripheralManagerDidUpdateState( peripheral: CBPeripheralManager ); cdecl;
    procedure peripheralManager( peripheral: CBPeripheralManager; willRestoreState: NSDictionary ); cdecl; overload;
    procedure peripheralManager( peripheral: CBPeripheralManager; didAddService: CBService; error: NSError ); cdecl; overload;
    procedure peripheralManagerDidStartAdvertising( peripheral: CBPeripheralManager; error: NSError ); cdecl;
    procedure peripheralManager( peripheral: CBPeripheralManager; central: CBCentral; didSubscribeToCharacteristic: CBCharacteristic ); cdecl; overload;
    procedure peripheralManager( peripheral: CBPeripheralManager; central: CBCentral; didUnsubscribeFromCharacteristic: CBCharacteristic1 ); cdecl; overload;
    procedure peripheralManagerIsReadyToUpdateSubscribers( peripheral: CBPeripheralManager ); cdecl;
    procedure peripheralManager( peripheral: CBPeripheralManager; didReceiveReadRequest: CBATTRequest ); cdecl; overload;
    procedure peripheralManager( peripheral: CBPeripheralManager; didReceiveWriteRequests: NSArray ); cdecl; overload;
  end;

  // ---------------------------------------------------------------------------
  CBPeripheralDelegate = interface( NSObject )
    ['{A9449ABF-F7C9-4859-9B19-3CB650C04507}']

    procedure peripheral( peripheral: CBPeripheral; didDiscoverServices: NSError ); cdecl; overload;
    procedure peripheral( peripheral: CBPeripheral; didDiscoverIncludedServicesForService: CBService; error: NSError ); cdecl; overload;

    procedure peripheral( peripheral: CBPeripheral; didDiscoverCharacteristicsForService: CBService; error: NSError_1 ); cdecl; overload;
    procedure peripheral( peripheral: CBPeripheral; didDiscoverDescriptorsForCharacteristic: CBCharacteristic; error: NSError ); cdecl; overload;

    procedure peripheral( peripheral: CBPeripheral; didUpdateValueForCharacteristic: CBCharacteristic; error: NSError_2 ); cdecl; overload;
    procedure peripheral( peripheral: CBPeripheral; didUpdateValueForDescriptor: CBDescriptor; error: NSError ); cdecl; overload;

    procedure peripheral( peripheral: CBPeripheral; didWriteValueForCharacteristic: CBCharacteristic; error: NSError_3 ); cdecl; overload;
    procedure peripheral( peripheral: CBPeripheral; didWriteValueForDescriptor: CBDescriptor; error: NSError_4 ); cdecl; overload;

    procedure peripheral( peripheral: CBPeripheral; didUpdateNotificationStateForCharacteristic: CBCharacteristic; error: NSError_1 ); cdecl; overload;

    procedure peripheralDidUpdateRSSI( peripheral: CBPeripheral; error: NSError ); cdecl;

    procedure peripheralDidUpdateName( peripheral: CBPeripheral ); cdecl;
    procedure peripheral( peripheral: CBPeripheral; didModifyServices: NSArray ); cdecl; overload;
  end;

  // ---------------------------------------------------------------------------
  CBPeripheralClass = interface( NSObjectClass )
    ['{968C709E-119E-4A4B-B972-A542E98EA988}']
  end;

  CBPeripheral = interface( NSObject )
    ['{EC712E68-ABDC-49C0-82B6-12950DDF0A25}']
    function identifier: NSUUID; cdecl;
    function name: NSString; cdecl;
    function delegate: CBPeripheralDelegate; cdecl;
    procedure setDelegate( delegate: CBPeripheralDelegate ); cdecl;

    procedure discoverServices( serviceUUIDs: NSArray ); cdecl;
    procedure discoverIncludedServices( includedServiceUUIDs: NSArray; forService: CBService ); cdecl;
    function services: NSArray; cdecl;

    procedure discoverCharacteristics( characteristicUUIDs: NSArray; forService: CBService ); cdecl;
    procedure discoverDescriptorsForCharacteristic( characteristic: CBCharacteristic ); cdecl;

    procedure readValueForCharacteristic( characteristic: CBCharacteristic ); cdecl;
    procedure readValueForDescriptor( descriptor: CBDescriptor ); cdecl;

    procedure writeValue( data: NSData; forCharacteristic: CBCharacteristic; &type: CBCharacteristicWriteType ); cdecl;

    procedure setNotifyValue( enabled: Boolean; forCharacteristic: CBCharacteristic ); cdecl;

    function state: CBPeripheralState; cdecl;
    procedure readRSSI; cdecl;
    function RSSI: NSNumber; cdecl;
  end;

  TCBPeripheral = class( TOCGenericImport<CBPeripheralClass, CBPeripheral> )
  end;

  // ---------------------------------------------------------------------------
  (* CBCentralManagerDelegateClass = interface( NSObjectClass )
    ['{72F24C2B-8833-454F-9AC0-C9BF2DD949C2}']
    end; *)

  CBCentralManagerDelegate = interface( NSObject { IObjectiveC } )
    ['{3E43EAC6-9D04-44FD-BCBC-0180BCBCCC44}']
    procedure centralManager( centralManager: CBCentralManager; didConnectPeripheral: CBPeripheral ); cdecl; overload;
    procedure centralManager( centralManager: CBCentralManager; didDisconnectPeripheral: CBPeripheral; error: NSError ); cdecl; overload;
    procedure centralManager( centralManager: CBCentralManager; didFailToConnectPeripheral: CBPeripheral; error: NSError_1 ); cdecl; overload;

    procedure centralManager( centralManager: CBCentralManager; didDiscoverPeripheral: CBPeripheral; advertisementData: NSDictionary; RSSI: NSNumber ); cdecl; overload;
    procedure centralManager( centralManager: CBCentralManager; didRetrieveConnectedPeripherals: NSArray ); cdecl; overload;
    procedure centralManager( centralManager: CBCentralManager; didRetrievePeripherals: NSArray_1 ); cdecl; overload;

    procedure centralManagerDidUpdateState( centralManager: CBCentralManager ); cdecl;
    procedure centralManager( centralManager: CBCentralManager; willRestoreState: NSDictionary ); cdecl; overload;
  end;

  (* TCBCentralManagerDelegate = class( TOCGenericImport<CBCentralManagerDelegateClass, CBCentralManagerDelegate> )
    end; *)

  // ---------------------------------------------------------------------------
  CBCentralManagerClass = interface( NSObjectClass )
    ['{EA1DC27B-FEE6-42EA-9852-CF9B2A45A088}']
  end;

  CBCentralManager = interface( NSObject )
    ['{89AEAC22-F02E-4F9F-9F10-1D47ABAFC0A7}']
    function initWithDelegate( delegate: Pointer; queue: dispatch_queue_t ): Pointer; cdecl; overload;
    function initWithDelegate( delegate: Pointer; queue: dispatch_queue_t; options: NSDictionary ): Pointer; cdecl; overload;
    procedure connectPeripheral( peripheral: CBPeripheral; options: NSDictionary ); cdecl;
    procedure cancelPeripheralConnection( peripheral: CBPeripheral ); cdecl;
    function retrieveConnectedPeripheralsWithServices( serviceUUIDs: NSArray ): NSArray; cdecl;
    function retrievePeripheralsWithIdentifiers( identifiers: NSArray ): NSArray; cdecl;
    procedure scanForPeripheralsWithServices( serviceUUIDs: NSArray; options: NSDictionary ); cdecl;
    procedure stopScan; cdecl;
    function state: CBCentralManagerState; cdecl;
    function delegate: CBCentralManagerDelegate; cdecl;
    procedure setDelegate( delegate: CBCentralManagerDelegate ); cdecl;
  end;

  TCBCentralManager = class( TOCGenericImport<CBCentralManagerClass, CBCentralManager> )
  end;

  // -------------------------------------------------------------------
  // Central Manager State Restoration Options
function CBCentralManagerRestoredStatePeripheralsKey: NSString;
function CBCentralManagerRestoredStateScanServicesKey: NSString;
function CBCentralManagerRestoredStateScanOptionsKey: NSString;

// -------------------------------------------------------------------
// Central Manager State Restoration Options
function CBAdvertisementDataLocalNameKey: NSString;
function CBAdvertisementDataManufacturerDataKey: NSString;
function CBAdvertisementDataServiceDataKey: NSString;
function CBAdvertisementDataServiceUUIDsKey: NSString;
function CBAdvertisementDataOverflowServiceUUIDsKey: NSString;
function CBAdvertisementDataTxPowerLevelKey: NSString;
function CBAdvertisementDataIsConnectable: NSString;
function CBAdvertisementDataSolicitedServiceUUIDsKey: NSString;

// -------------------------------------------------------------------
// Characteristic Descriptors
function CBUUIDCharacteristicExtendedPropertiesString: NSString;
function CBUUIDCharacteristicUserDescriptionString: NSString;
function CBUUIDClientCharacteristicConfigurationString: NSString;
function CBUUIDServerCharacteristicConfigurationString: NSString;
function CBUUIDCharacteristicFormatString: NSString;
function CBUUIDCharacteristicAggregateFormatString: NSString;

// -------------------------------------------------------------------
// Peripheral Scanning Options
function CBCentralManagerScanOptionAllowDuplicatesKey: NSString;
function CBCentralManagerScanOptionSolicitedServiceUUIDsKey: NSString;

{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iCoreBluetoothModule: THandle;
{$ENDIF}
{$IFDEF IOS}

  // ----------------------------------------------------------------------------
function CBCentralManagerRestoredStatePeripheralsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBCentralManagerRestoredStatePeripheralsKey' );
end;

// ----------------------------------------------------------------------------
function CBCentralManagerRestoredStateScanServicesKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBCentralManagerRestoredStateScanServicesKey' );
end;

// ----------------------------------------------------------------------------
function CBCentralManagerRestoredStateScanOptionsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataLocalNameKey' );
end;

// ----------------------------------------------------------------------------
// Central Manager State Restoration Options
function CBAdvertisementDataLocalNameKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataLocalNameKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataManufacturerDataKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataManufacturerDataKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataServiceDataKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataServiceDataKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataServiceUUIDsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataServiceUUIDsKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataOverflowServiceUUIDsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataOverflowServiceUUIDsKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataTxPowerLevelKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataTxPowerLevelKey' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataIsConnectable: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataIsConnectable' );
end;

// ----------------------------------------------------------------------------
function CBAdvertisementDataSolicitedServiceUUIDsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBAdvertisementDataSolicitedServiceUUIDsKey' );
end;

// -------------------------------------------------------------------
// Characteristic Descriptors
function CBUUIDCharacteristicExtendedPropertiesString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDCharacteristicExtendedPropertiesString' );
end;

// -------------------------------------------------------------------
function CBUUIDCharacteristicUserDescriptionString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDCharacteristicUserDescriptionString' );
end;

// -------------------------------------------------------------------
function CBUUIDClientCharacteristicConfigurationString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDClientCharacteristicConfigurationString' );
end;

// -------------------------------------------------------------------
function CBUUIDServerCharacteristicConfigurationString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDServerCharacteristicConfigurationString' );
end;

// -------------------------------------------------------------------
function CBUUIDCharacteristicFormatString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDCharacteristicFormatString' );
end;

// -------------------------------------------------------------------
function CBUUIDCharacteristicAggregateFormatString: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBUUIDCharacteristicAggregateFormatString' );
end;

// -------------------------------------------------------------------
// Peripheral Scanning Options
// -------------------------------------------------------------------
function CBCentralManagerScanOptionAllowDuplicatesKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBCentralManagerScanOptionAllowDuplicatesKey' );
end;

// -------------------------------------------------------------------
function CBCentralManagerScanOptionSolicitedServiceUUIDsKey: NSString;
begin
  result := CocoaNSStringConst( libCoreBluetooth, 'CBCentralManagerScanOptionSolicitedServiceUUIDsKey' );
end;

// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
{$IF defined(CPUARM)}
procedure libCoreBluetoothLoader; cdecl; external libCoreBluetooth;
{$ELSE}

initialization

iCoreBluetoothModule := dlopen( MarshaledAString( libCoreBluetooth ), RTLD_LAZY );

finalization

dlclose( iCoreBluetoothModule );
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
