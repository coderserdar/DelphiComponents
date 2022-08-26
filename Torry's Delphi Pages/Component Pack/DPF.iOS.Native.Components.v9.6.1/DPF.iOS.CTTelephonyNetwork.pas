// ------------------------------------------------------------------------------
// DPF.iOS.CTTelephonyNetwork Wrapped Classes & Interfaces
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
unit DPF.iOS.CTTelephonyNetwork;

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
  iOSapi.CoreTelephony,
  Macapi.CoreFoundation,
{$ENDIF}
  DPF.iOS.Common,
  DPF.iOS.Classes,
  FMX.Dialogs;

{$IFDEF IOS}

type
  id = pointer;

  TCarrierInfo = record
    allowsVOIP: Boolean;
    carrierName: string;
    isoCountryCode: string;
    mobileCountryCode: string;
    mobileNetworkCode: string;
    MyPhoneNumber: string;
    IMSI: string;
    SignalStrength: Integer;
  end;

function CTGetSignalStrength( ): Integer; cdecl; external libCoreTelephony name _PU + 'CTGetSignalStrength';
function CTSettingCopyMyPhoneNumber( ): NSString; cdecl; external libCoreTelephony name _PU + 'CTSettingCopyMyPhoneNumber';

// function CTSIMSupportCopyMobileSubscriberIdentity( ): NSString; cdecl; external libCoreTelephony name _PU + 'CTSIMSupportCopyMobileSubscriberIdentity';
function GetCarrierInfo: TCarrierInfo;

{$ENDIF}

// ----------------------------------------------------------------------------
implementation

uses Posix.Dlfcn;

{$IFDEF IOS}

function GetCarrierInfo: TCarrierInfo;
// const libCoreTelephony = '/System/Library/PrivateFrameworks/CoreTelephony.framework/CoreTelephony';
var
  networkInfo   : CTTelephonyNetworkInfo;
  carrier       : CTCarrier;
  pn            : NSString;
  iCoreTelephony: THandle;
  _kC           : function( p: pointer ): NSString; cdecl;
  PP            : Pointer;
begin
  networkInfo := TCTTelephonyNetworkInfo.Wrap( TCTTelephonyNetworkInfo.Alloc.init );
  carrier     := networkInfo.subscriberCellularProvider;

  if not assigned( carrier ) then
    exit;
  Result.allowsVOIP        := carrier.allowsVOIP;
  Result.carrierName       := UTF8ToString( carrier.carrierName.UTF8String );
  Result.isoCountryCode    := UTF8ToString( carrier.isoCountryCode.UTF8String );
  Result.mobileCountryCode := UTF8ToString( carrier.mobileCountryCode.UTF8String );
  Result.mobileNetworkCode := UTF8ToString( carrier.mobileNetworkCode.UTF8String );
  pn                       := CTSettingCopyMyPhoneNumber;
  if assigned( pn ) then
    Result.MyPhoneNumber := UTF8ToString( pn.UTF8String );
  Result.SignalStrength  := CTGetSignalStrength;

  iCoreTelephony := dlopen( MarshaledAString( libCoreTelephony ), RTLD_LAZY );
  _kC            := dlsym( iCoreTelephony, MarshaledAString( 'CTSIMSupportCopyMobileSubscriberIdentity' ) );
  PP             := Pointer( _kC( nil ) );
  dlclose( iCoreTelephony );
  if assigned( pn ) then
    Result.IMSI := UTF8ToString( pn.UTF8String );

end;
{$ENDIF}

end.
