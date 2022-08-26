// ------------------------------------------------------------------------------
// DPF.iOS.CacheManager Library
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
unit DPF.iOS.CacheManager;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,

  DPF.iOS.BaseControl,
  DPF.iOS.NSOperationQueue,
{$IFDEF IOS}
  Macapi.ObjCRuntime,
  DPF.iOS.Common,

  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
{$ENDIF}
  FMX.Forms,
  FMX.Dialogs;

{$IFDEF IOS}
procedure DisableCache;

procedure InitCache( memoryCapacity: NativeUInt; diskCapacity: NativeUInt; diskPath: string );
procedure RemoveAllCachedResponses;

procedure SetDiskCapacity( DiskCapacity: NativeUInt );
procedure SetMemoryCapacity( MemoryCapacity: NativeUInt );

function GetCurrentMemoryUsage: NativeUInt;
function GetCurrentDiskUsage: NativeUInt;

function GetChacheDiskCapacity: NativeUInt;
function GetChacheMemoryCapacity: NativeUInt;

function GetCachedData( HTTPURL: string ): NSData;
{$ENDIF}

implementation

{$IFDEF IOS}

//var CacheDic: TDictionary<string, NSData>;

  // ----------------------------------------------------------------------------
procedure DisableCache;
var
  sharedCache: NSURLCache;
begin
  sharedCache := TNSURLCache.Wrap( TNSURLCache.Alloc.initWithMemoryCapacity( 0, 0, nil ) );
  TNSURLCache.OCClass.setSharedURLCache( sharedCache );
  sharedCache.release;
end;

// ----------------------------------------------------------------------------
procedure RemoveAllCachedResponses;
var
  R: NSURLCache;
begin
  R := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  R.removeAllCachedResponses;
end;

// ----------------------------------------------------------------------------
procedure SetDiskCapacity( DiskCapacity: NativeUInt );
var
  R: NSURLCache;
begin
  R := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  R.setDiskCapacity( DiskCapacity );
end;

// ----------------------------------------------------------------------------
procedure SetMemoryCapacity( MemoryCapacity: NativeUInt );
var
  R: NSURLCache;
begin
  R := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  R.setMemoryCapacity( MemoryCapacity );
end;

// ----------------------------------------------------------------------------
function GetCurrentDiskUsage: NativeUInt;
var
  R: NSURLCache;
begin
  R      := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  result := R.currentDiskUsage;
end;

// ----------------------------------------------------------------------------
function GetCurrentMemoryUsage: NativeUInt;
var
  R: NSURLCache;
begin
  R      := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  result := R.currentMemoryUsage;
end;

// ----------------------------------------------------------------------------
function GetChacheDiskCapacity: NativeUInt;
var
  R: NSURLCache;
begin
  R      := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  result := R.diskCapacity;
end;

// ----------------------------------------------------------------------------
function GetChacheMemoryCapacity: NativeUInt;
var
  R: NSURLCache;
begin
  R      := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  result := R.memoryCapacity;
end;

// ----------------------------------------------------------------------------
// Deafult : memoryCapacity : 500K & diskCapacity: 10M
procedure InitCache( memoryCapacity: NativeUInt; diskCapacity: NativeUInt; diskPath: string );
var
  sharedCache: NSURLCache;
  dp         : NSString;
begin
  dp := nil;
  if diskPath <> '' then
    dp        := DPF.iOS.Common.NSSTR( diskPath );
  sharedCache := TNSURLCache.Wrap( TNSURLCache.Alloc.initWithMemoryCapacity( MemoryCapacity, diskCapacity, dp ) );
  TNSURLCache.OCClass.setSharedURLCache( sharedCache );
  sharedCache.release;
end;

// ----------------------------------------------------------------------------
function GetCachedData( HTTPURL: string ): NSData;
var
  FURL                : NSURL;
  FNSCachedURLResponse: NSCachedURLResponse;
  FNSURLCache         : NSURLCache;
  FURLRequest         : NSMutableURLRequest;
//  NSD                 : NSData;
begin
{  if CacheDic.ContainsKey( HTTPURL ) then
  begin
    CacheDic.TryGetValue( HTTPURL, Result );
    if Assigned( Result ) then
      exit;
  end;}

  FURL                 := TNSURL.Wrap( TNSURL.OCClass.URLWithString( DPF.iOS.Common.NSStr( HTTPURL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) );
  FURLRequest          := TNSMutableURLRequest.Wrap( TNSMutableURLRequest.OCClass.requestWithURL( FURL, {NSURLRequestReturnCacheDataDontLoad}NSURLRequestUseProtocolCachePolicy, 5 ) );
  FNSURLCache          := TNSURLCache.Wrap( TNSURLCache.OCClass.sharedURLCache );
  FNSCachedURLResponse := FNSURLCache.cachedResponseForRequest( FURLRequest );
  Result               := nil;
  if ( FNSCachedURLResponse <> nil ) and ( FNSCachedURLResponse.data.length > 0 ) then
    Result := FNSCachedURLResponse.data;

  {NSD := TNSData.Create;
  NSD := TNSData.Wrap( NSD.initWithData(Result) );
  CacheDic.AddOrSetValue( HTTPURL, NSD );}
end;

{$ENDIF}

// ----------------------------------------------------------------------------
initialization

//CacheDic := TDictionary<string, NSData>.Create;

finalization

//CacheDic.Free;

end.
