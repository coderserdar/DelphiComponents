// ------------------------------------------------------------------------------
// DPF.iOS.SCNetworks Tools & Classes
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

unit DPF.iOS.SCNetworks;

interface

{$I DPF.iOS.Defs.inc}
{$IFDEF IOS}

uses
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  Posix.SysSocket,
  DPF.iOS.Dispatch,
  iOSapi.Foundation;

// ------------------------------------------------------------------------------
function IsHostReachable( const Host: string ): Boolean;
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iSystemConfigurationModule: THandle;
{$ENDIF}

const
  libSystemConfiguration = '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';

  kSCNetworkReachabilityFlagsTransientConnection  = 1 shl 0; // 1;
  kSCNetworkReachabilityFlagsReachable            = 1 shl 1; // 2;
  kSCNetworkReachabilityFlagsConnectionRequired   = 1 shl 2; // 4;
  kSCNetworkReachabilityFlagsConnectionOnTraffic  = 1 shl 3; // 8;
  kSCNetworkReachabilityFlagsInterventionRequired = 1 shl 4; // $10;
  kSCNetworkReachabilityFlagsConnectionOnDemand   = 1 shl 5; // $20;
  kSCNetworkReachabilityFlagsIsLocalAddress       = 1 shl 16; // $10000;
  kSCNetworkReachabilityFlagsIsDirect             = 1 shl 17; // $20000;
  kSCNetworkReachabilityFlagsIsWWAN               = 1 shl 18; // $40000;

  // ------------------------------------------------------------------------------
type
  SCNetworkReachabilityFlags = UInt32;

  SCNetworkReachabilityRef = ^__SCNetworkReachability;

  __SCNetworkReachability = record
  end;

  SCNetworkReachabilityContext = record
    version: CFIndex;
    info: Pointer;
    retain: function( info: Pointer ): Pointer;
    release: procedure( info: Pointer );
    copyDescription: function( info: Pointer ): CFStringRef;
  end;

  SCNetworkReachabilityContextPtr = ^SCNetworkReachabilityContext;

  SCNetworkReachabilityCallback = procedure( target: SCNetworkReachabilityRef; flags: SCNetworkReachabilityFlags; info: Pointer );

  // ------------------------------------------------------------------------------

function SCNetworkReachabilityCreateWithAddress( allocator: CFAllocatorRef; address: psockaddr ): SCNetworkReachabilityRef; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddress';
function SCNetworkReachabilityCreateWithAddressPair( allocator: CFAllocatorRef; localAddress: psockaddr; remoteAddress: psockaddr ): SCNetworkReachabilityRef; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddressPair';
function SCNetworkReachabilityCreateWithName( allocator: CFAllocatorRef; nodename: PChar ): SCNetworkReachabilityRef; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithName';
function SCNetworkReachabilityGetTypeID: CFTypeID; external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetTypeID';
function SCNetworkReachabilityGetFlags( target: SCNetworkReachabilityRef; var flags: SCNetworkReachabilityFlags ): Boolean; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetFlags';
function SCNetworkReachabilitySetCallback( target: SCNetworkReachabilityRef; callout: SCNetworkReachabilityCallBack; var context: SCNetworkReachabilityContext ): Boolean; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetCallback';
function SCNetworkReachabilityScheduleWithRunLoop( target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityScheduleWithRunLoop';
function SCNetworkReachabilityUnscheduleFromRunLoop( target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): Boolean; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilityUnscheduleFromRunLoop';
function SCNetworkReachabilitySetDispatchQueue( target: SCNetworkReachabilityRef; queue: dispatch_queue_t ): Boolean; cdecl; external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetDispatchQueue';

// ------------------------------------------------------------------------------
function IsHostReachable( const Host: string ): Boolean;
var
  Reachability: SCNetworkReachabilityRef;
  Flags       : SCNetworkReachabilityFlags;
begin
  Result       := False;
  Reachability := SCNetworkReachabilityCreateWithName( kCFAllocatorDefault, PWideChar( Host ) );
  try
    if ( Reachability <> nil ) and SCNetworkReachabilityGetFlags( Reachability, Flags ) then
      Result := ( ( Flags and kSCNetworkReachabilityFlagsReachable ) <> 0 ) and ( ( Flags and kSCNetworkReachabilityFlagsConnectionRequired ) = 0 );
  finally
    CFRelease( Reachability );
  end;
end;
{$ENDIF}
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure LibSystemConfigurationLoader; cdecl; external libSystemConfiguration;
{$ELSE}

initialization

iSystemConfigurationModule := dlopen( MarshaledAString( libSystemConfiguration ), RTLD_LAZY );

finalization

dlclose( iSystemConfigurationModule );
{$ENDIF}
{$ENDIF}

// ------------------------------------------------------------------------------
end.
