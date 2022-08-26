// ------------------------------------------------------------------------------
// DPF.iOS.ASIdentifierManager Wrapped Classes & Interfaces
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
unit DPF.iOS.ASIdentifierManager;

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
  FMX.Dialogs;

const
  libASIdentifierManager = '/System/Library/Frameworks/ASIdentifierManager.framework/ASIdentifierManager';

{$IFDEF IOS}

type
  id = pointer;

  // ----------------------------------------------------------------------------
  // ASIdentifierManager
  // ----------------------------------------------------------------------------
  ASIdentifierManagerClass = interface( NSObjectClass )
    ['{FF12D505-65AE-4BD4-88E6-B43656211895}']

    function sharedManager: id; cdecl;
  end;

  ASIdentifierManager = interface( NSObject )
    ['{B8183CFD-4834-4D65-970B-7F1814435176}']

    function advertisingIdentifier: NSUUID; cdecl;
    function isAdvertisingTrackingEnabled: boolean; cdecl;
  end;

  TASIdentifierManager = class( TOCGenericImport<ASIdentifierManagerClass, ASIdentifierManager> )
  end;

{$ENDIF}

  // ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
// ----------------------------------------------------------------------------
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iASIdentifierManagerModule: THandle;
{$ENDIF}
  // ----------------------------------------------------------------------------
{$IF defined(CPUARM)}
procedure libASIdentifierManagerLoader; cdecl; external libASIdentifierManager;
{$ELSE}

initialization

iASIdentifierManagerModule := dlopen( MarshaledAString( libASIdentifierManager ), RTLD_LAZY );

finalization

dlclose( iASIdentifierManagerModule );
{$ENDIF}
{$ENDIF}

end.
