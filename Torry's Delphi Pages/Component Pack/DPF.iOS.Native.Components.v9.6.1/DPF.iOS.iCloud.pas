// ------------------------------------------------------------------------------
// DPF.iOS.iCloud Component
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
unit DPF.iOS.iCloud;

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
  DPF.iOS.Dispatch,
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
  TDPFiCloud = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  IDPFiCloudHandler = interface( NSObject )
    ['{0FC8C156-9F12-4078-BF3E-4FD81CAC5F4A}']

    procedure ubiquitousKeyValueStoreDidChange( notification: NSNotification ); cdecl;

  end;

  // ------------------------------------------------------------------------------
  TDPFiCloudHandler = class( TOCLocal )
  private

  private
    FDPFiCloud: TDPFiCloud;

  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor create( ADPFiCloud: TDPFiCloud );

    procedure ubiquitousKeyValueStoreDidChange( notification: NSNotification ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  NSFileManagerClass = interface( NSObjectClass )
    ['{5B3CDB25-4214-4EE3-9801-EB252FF3E144}']
    function defaultManager: Pointer; cdecl;
  end;

  NSFileManager = interface( NSObject )
    ['{F8C9DA56-9DBC-4A28-BB5D-D03DB3F187C3}']
    (*
      function URLForDirectory( directory: NSSearchPathDirectory; inDomain: NSSearchPathDomainMask; appropriateForURL: NSURL; create: Boolean; error: PPointer ): NSURL; cdecl;
      function URLForPublishingUbiquitousItemAtURL( url: NSURL; expirationDate: NSDate; error: PPointer ): NSURL; cdecl;
      function URLsForDirectory( directory: NSSearchPathDirectory; inDomains: NSSearchPathDomainMask ): NSArray; cdecl;
      function attributesOfFileSystemForPath( path: NSString; error: PPointer ): NSDictionary; cdecl;
      function attributesOfItemAtPath( path: NSString; error: PPointer ): NSDictionary; cdecl;
      function changeCurrentDirectoryPath( path: NSString ): Boolean; cdecl;
      function changeFileAttributes( attributes: NSDictionary; atPath: NSString ): Boolean; cdecl;
      function componentsToDisplayForPath( path: NSString ): NSArray; cdecl;
      function contentsAtPath( path: NSString ): NSData; cdecl;
      function contentsEqualAtPath( path1: NSString; andPath: NSString ): Boolean; cdecl;
      function contentsOfDirectoryAtPath( path: NSString; error: PPointer ): NSArray; cdecl;
      function contentsOfDirectoryAtURL( url: NSURL; includingPropertiesForKeys: NSArray; options: NSDirectoryEnumerationOptions; error: PPointer ): NSArray; cdecl;
      function copyItemAtPath( srcPath: NSString; toPath: NSString; error: PPointer ): Boolean; cdecl;
      function copyItemAtURL( srcURL: NSURL; toURL: NSURL; error: PPointer ): Boolean; cdecl;
      function createDirectoryAtPath( path: NSString; attributes: NSDictionary ): Boolean; cdecl; overload;
      function createDirectoryAtPath( path: NSString; withIntermediateDirectories: Boolean; attributes: NSDictionary; error: PPointer ): Boolean; cdecl; overload;
      function createDirectoryAtURL( url: NSURL; withIntermediateDirectories: Boolean; attributes: NSDictionary; error: PPointer ): Boolean; cdecl;
      function createFileAtPath( path: NSString; contents: NSData; attributes: NSDictionary ): Boolean; cdecl;
      function createSymbolicLinkAtPath( path: NSString; pathContent: NSString ): Boolean; cdecl; overload;
      function createSymbolicLinkAtPath( path: NSString; withDestinationPath: NSString; error: PPointer ): Boolean; cdecl; overload;
      function createSymbolicLinkAtURL( url: NSURL; withDestinationURL: NSURL; error: PPointer ): Boolean; cdecl;
      function currentDirectoryPath: NSString; cdecl;
      function delegate: Pointer; cdecl;
      function destinationOfSymbolicLinkAtPath( path: NSString; error: PPointer ): NSString; cdecl;
      function directoryContentsAtPath( path: NSString ): NSArray; cdecl;
      function displayNameAtPath( path: NSString ): NSString; cdecl;
      function enumeratorAtPath( path: NSString ): NSDirectoryEnumerator; cdecl;
      function evictUbiquitousItemAtURL( url: NSURL; error: PPointer ): Boolean; cdecl;
      function fileAttributesAtPath( path: NSString; traverseLink: Boolean ): NSDictionary; cdecl;
      function fileExistsAtPath( path: NSString ): Boolean; cdecl; overload;
      function fileExistsAtPath( path: NSString; isDirectory: PBoolean ): Boolean; cdecl; overload;
      function fileSystemAttributesAtPath( path: NSString ): NSDictionary; cdecl;
      function fileSystemRepresentationWithPath( path: NSString ): MarshaledAString; cdecl;
      function isDeletableFileAtPath( path: NSString ): Boolean; cdecl;
      function isExecutableFileAtPath( path: NSString ): Boolean; cdecl;
      function isReadableFileAtPath( path: NSString ): Boolean; cdecl;
      function isUbiquitousItemAtURL( url: NSURL ): Boolean; cdecl;
      function isWritableFileAtPath( path: NSString ): Boolean; cdecl;
      function linkItemAtPath( srcPath: NSString; toPath: NSString; error: PPointer ): Boolean; cdecl;
      function linkItemAtURL( srcURL: NSURL; toURL: NSURL; error: PPointer ): Boolean; cdecl;
      function mountedVolumeURLsIncludingResourceValuesForKeys( propertyKeys: NSArray; options: NSVolumeEnumerationOptions ): NSArray; cdecl;
      function moveItemAtPath( srcPath: NSString; toPath: NSString; error: PPointer ): Boolean; cdecl;
      function moveItemAtURL( srcURL: NSURL; toURL: NSURL; error: PPointer ): Boolean; cdecl;
      function pathContentOfSymbolicLinkAtPath( path: NSString ): NSString; cdecl;
      function removeItemAtPath( path: NSString; error: PPointer ): Boolean; cdecl;
      function removeItemAtURL( URL: NSURL; error: PPointer ): Boolean; cdecl;
      function replaceItemAtURL( originalItemURL: NSURL; withItemAtURL: NSURL; backupItemName: NSString; options: NSFileManagerItemReplacementOptions; resultingItemURL: NSURL; error: PPointer ): Boolean; cdecl;
      function setAttributes( attributes: NSDictionary; ofItemAtPath: NSString; error: PPointer ): Boolean; cdecl;
      procedure setDelegate( delegate: Pointer ); cdecl;
      function setUbiquitous( flag: Boolean; itemAtURL: NSURL; destinationURL: NSURL; error: PPointer ): Boolean; cdecl;
      function startDownloadingUbiquitousItemAtURL( url: NSURL; error: PPointer ): Boolean; cdecl;
      function stringWithFileSystemRepresentation( str: MarshaledAString; length: NSUInteger ): NSString; cdecl;
      function subpathsAtPath( path: NSString ): NSArray; cdecl;
      function subpathsOfDirectoryAtPath( path: NSString; error: PPointer ): NSArray; cdecl;
    *)

    function URLForUbiquityContainerIdentifier( containerIdentifier: NSString ): NSURL; cdecl;
    function ubiquityIdentityToken: pointer; cdecl; // iOS 6.0 and later
  end;

  TNSFileManager = class( TOCGenericImport<NSFileManagerClass, NSFileManager> )
  end;

{$ENDIF}

  TDPFiCloudOnKeyChanged = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFiCloud = class( TComponent )
  private
    FOnKeyChanged: TDPFiCloudOnKeyChanged;

{$IFDEF IOS}
    FDPFiCloudHandler: TDPFiCloudHandler;
{$ENDIF}
  protected

  public
{$IFDEF IOS}
    procedure Loaded; override;
    function iCloudAvailable: boolean;

    function SetValue( const key: string; const value: string ): Boolean; overload;
    function SetValue( const key: string; const value: Boolean ): Boolean; overload;
    function SetValue( const key: string; const value: Double ): Boolean; overload;
    function SetValue( const key: string; const value: Int64 ): Boolean; overload;

    function GetValue( const key: string; DefValue: string ): string; overload;
    function GetValue( const key: string; DefValue: boolean ): Boolean; overload;
    function GetValue( const key: string; DefValue: Double ): Double; overload;
    function GetValue( const key: string; DefValue: Int64 ): Int64; overload;

    procedure RemoveKey( const key: string );
    procedure Synchronize;

{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnKeyChanged: TDPFiCloudOnKeyChanged read FOnKeyChanged write FOnKeyChanged;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFiCloud }
// ------------------------------------------------------------------------------
constructor TDPFiCloud.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF IOS}
  FDPFiCloudHandler := TDPFiCloudHandler.create( self );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFiCloudHandler.GetObjectID, sel_getUid( 'ubiquitousKeyValueStoreDidChange:' ), ( NSStr( 'NSUbiquitousKeyValueStoreDidChangeExternallyNotification' ) as ILocalObject ).GetObjectID, nil );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFiCloud.Destroy;
begin
{$IFDEF IOS}
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFiCloudHandler.GetObjectID );
  FDPFiCloudHandler.DisposeOf;
{$ENDIF}
  inherited;
end;
// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFiCloud.Loaded;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.iCloudAvailable: boolean;
begin
  result := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).ubiquityIdentityToken <> nil;
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloud.Synchronize;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.GetValue( const key: string; DefValue: string ): string;
var
  keyStore: NSUbiquitousKeyValueStore;
  ns      : NSString;
begin
  result   := DefValue;
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  ns       := keyStore.stringForKey( NSStr( key ) );
  if assigned( ns ) then
    result := UTF8ToString( ns.UTF8String );
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloud.RemoveKey( const key: string );
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.removeObjectForKey( NSStr( Key ) );
  keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.GetValue( const key: string; DefValue: Boolean ): Boolean;
var
  keyStore: NSUbiquitousKeyValueStore;
  ns      : NSString;
begin
  result   := DefValue;
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  result   := keyStore.boolForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.GetValue( const key: string; DefValue: Int64 ): Int64;
var
  keyStore: NSUbiquitousKeyValueStore;
  ns      : NSString;
begin
  result   := DefValue;
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  result   := keyStore.longLongForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.GetValue( const key: string; DefValue: Double ): Double;
var
  keyStore: NSUbiquitousKeyValueStore;
  ns      : NSString;
begin
  result   := DefValue;
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  result   := keyStore.doubleForKey( NSStr( key ) );
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.SetValue( const key: string; const value: string ): Boolean;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.setString( NSStr( value ), NSStr( key ) );
  result := keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.SetValue( const key: string; const value: Boolean ): Boolean;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.setBool( value, NSStr( key ) );
  result := keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.SetValue( const key: string; const value: Double ): Boolean;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.setDouble( value, NSStr( key ) );
  result := keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
function TDPFiCloud.SetValue( const key: string; const value: Int64 ): Boolean;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.setLongLong( value, NSStr( key ) );
  result := keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
{ TDPFiCloudHandler }
constructor TDPFiCloudHandler.create( ADPFiCloud: TDPFiCloud );
begin
  inherited create;
  FDPFiCloud := ADPFiCloud;
end;

// ------------------------------------------------------------------------------
function TDPFiCloudHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IDPFiCloudHandler );
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloudHandler.ubiquitousKeyValueStoreDidChange( notification: NSNotification );
begin
  if Assigned( FDPFiCloud.FOnKeyChanged ) then
    FDPFiCloud.FOnKeyChanged( FDPFiCloud );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
