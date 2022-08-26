// ------------------------------------------------------------------------------
// DPF.iOS.Dispatch (Interface to Apple's Grand Central Dispatch API for multithreading)
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

unit DPF.iOS.Dispatch;

interface

{$I DPF.iOS.Defs.inc}
{$IFDEF IOS}

uses
  System.SysUtils,
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  iOSapi.Foundation,
  Macapi.Dispatch;

// ------------------------------------------------------------------------------
const
{$IFDEF IOS8}
  libdispatch = '/usr/lib/libSystem.dylib';
{$ELSE}
  libdispatch = '/usr/lib/system/libdispatch.dylib';
{$ENDIF}

  // SZ: iOS 8 needs libSystem.dylib. Deploying to the iOS 7.1 simulator just fine
  // I don't know how we might be able to check if we link against the iOS 7 or iOS 8 framework
  // so let's try using libSystem.dylib also when linking against iOS 7.1

  DISPATCH_QUEUE_SERIAL = 0;
{$IF NOT DECLARED(_PU)}
{$IFDEF UNDERSCOREIMPORTNAME}
  _PU = '_';
{$ELSE}
  _PU = '';
{$ENDIF}
{$EXTERNALSYM _PU}
{$ENDIF}

type

  // https://developer.apple.com/library/ios/documentation/Performance/Reference/GCD_libdispatch_Ref/Reference/reference.html

  dispatch_object_t = IntPtr;
  dispatch_queue_t  = dispatch_object_t;

  dispatch_block_t    = reference to procedure;
  dispatch_function_t = procedure( context: Pointer ); cdecl;

procedure dispatch_async( queue: dispatch_queue_t; block: dispatch_block_t );
procedure dispatch_sync( queue: dispatch_queue_t; block: dispatch_block_t );
function dispatch_get_main_queue: dispatch_queue_t;

function dispatch_queue_create( QueueLabel: MarshaledAString; attr: dispatch_queue_t ): dispatch_queue_t; cdecl; external libdispatch name _PU + 'dispatch_queue_create';
procedure dispatch_release( obj: dispatch_object_t ); cdecl; external libdispatch name _PU + 'dispatch_release';
// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}
procedure dispatch_async_f( queue: dispatch_queue_t; context: Pointer; work: dispatch_function_t ); cdecl; external libdispatch name _PU + 'dispatch_async_f';
procedure dispatch_sync_f( queue: dispatch_queue_t; context: Pointer; work: dispatch_function_t ); cdecl; external libdispatch name _PU + 'dispatch_sync_f';

// ------------------------------------------------------------------------------
function dispatch_get_main_queue: dispatch_queue_t;
var
  dispatchModule: HMODULE;
begin
  Result         := 0;
  dispatchModule := LoadLibrary( PWideChar( libdispatch ) );
  if dispatchModule <> 0 then
  begin
    Result := dispatch_queue_t( GetProcAddress( dispatchModule, PWideChar( '_dispatch_main_q' ) ) );
    FreeLibrary( dispatchModule );
  end;
end;

// ------------------------------------------------------------------------------
procedure DispatchCallback( context: Pointer ); cdecl;
var
  CallbackProc: dispatch_block_t absolute context;
begin
  try
    CallbackProc;
  finally
    IInterface( context )._Release;
  end;
end;

// ------------------------------------------------------------------------------
procedure dispatch_async( queue: dispatch_queue_t; block: dispatch_block_t );
var
  context: pointer absolute block;
begin
  IInterface( context )._AddRef;
  dispatch_async_f( queue, context, DispatchCallback );
end;

// ------------------------------------------------------------------------------
procedure dispatch_sync( queue: dispatch_queue_t; block: dispatch_block_t );
var
  context: pointer absolute block;
begin
  IInterface( context )._AddRef;
  dispatch_sync_f( queue, context, DispatchCallback );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
