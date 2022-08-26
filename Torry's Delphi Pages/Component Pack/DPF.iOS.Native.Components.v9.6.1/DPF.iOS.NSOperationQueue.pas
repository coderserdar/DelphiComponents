// ------------------------------------------------------------------------------
// DPF.iOS.NSOperationQueue Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: NSOperationQueue://www.dpfaragir.com
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
// NSOperationQueue://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.NSOperationQueue;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjCRuntime,
  DPF.iOS.Common,

  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
{$ENDIF}
  FMX.Dialogs;

type
{$M+}
  TDPFNSOperationQueue = class;
{$IFDEF IOS}
  TDPFOperationQueueBlock = procedure of object;

  // ----------------------------------------------------------------------------
  NSBlockOperationClass = interface( NSOperationClass )
    ['{9522E5E5-270E-4489-89C9-D33E466120E4}']

    function blockOperationWithBlock( block: TDPFOperationQueueBlock ): pointer; cdecl;
  end;

  NSBlockOperation = interface( NSOperation )
    ['{13045965-4EB5-4C83-81BD-830CC36EE65F}']

    procedure addExecutionBlock( block: TDPFOperationQueueBlock ); cdecl;
    function executionBlocks: NSArray; cdecl;
  end;

  TNSBlockOperation = class( TOCGenericImport<NSBlockOperationClass, NSBlockOperation> )
  end;

  // ----------------------------------------------------------------------------
  NSOperationQueueClass = interface( NSObjectClass )
    ['{642482A4-2A52-460C-BDBD-860F6020392F}']
    function currentQueue: Pointer; cdecl;
    function mainQueue: Pointer; cdecl;
  end;

  NSOperationQueue = interface( NSObject )
    ['{D5ABE7A8-CD7C-40C7-A4AB-F2F28968D11D}']

    procedure addOperationWithBlock( Bloc: TDPFOperationQueueBlock ); cdecl;
    procedure addOperation( op: NSOperation ); cdecl;
    procedure addOperations( ops: NSArray; waitUntilFinished: Boolean ); cdecl;
    procedure cancelAllOperations; cdecl;
    function isSuspended: Boolean; cdecl;
    function maxConcurrentOperationCount: NSInteger; cdecl;
    function name: NSString; cdecl;
    function operationCount: NSUInteger; cdecl;
    function operations: NSArray; cdecl;
    procedure setMaxConcurrentOperationCount( cnt: NSInteger ); cdecl;
    procedure setName( n: NSString ); cdecl;
    procedure setSuspended( suspended: Boolean ); cdecl;
    procedure waitUntilAllOperationsAreFinished; cdecl;
  end;

  TNSOperationQueue = class( TOCGenericImport<NSOperationQueueClass, NSOperationQueue> )
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFNSOperationQueue = class( TComponent )
  private
    FMaxConcurrentOperationCount: Integer;
    procedure SetMaxConcurrentOperationCount( const Value: Integer );
  protected
  public
{$IFDEF IOS}
    FNSOperationQueue: NSOperationQueue;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
{$IFDEF IOS}
    procedure AddOperation( op: NSOperation );
    procedure AddOperationBlock( Block: TDPFOperationQueueBlock );
    class procedure AddOperationBlockToMainQueue( Block: TDPFOperationQueueBlock );
    procedure CancelAllOperations;
    procedure WaitUntilAllOperationsAreFinished;
    procedure SetSuspended( Suspended: Boolean );
    function GetOperationCount: Int64;
{$ENDIF}
  published
    property MaxConcurrentOperationCount: Integer read FMaxConcurrentOperationCount write SetMaxConcurrentOperationCount default 10;
  end;
  // ----------------------------------------------------------------------------

implementation

// ----------------------------------------------------------------------------

constructor TDPFNSOperationQueue.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FMaxConcurrentOperationCount := 10;
{$IFDEF IOS}
  FNSOperationQueue := TNSOperationQueue.Wrap( TNSOperationQueue.Alloc.init );
  FNSOperationQueue.cancelAllOperations;
  FNSOperationQueue.setMaxConcurrentOperationCount( FMaxConcurrentOperationCount );
  FNSOperationQueue.setSuspended( false );
{$ENDIF}
end;

// ----------------------------------------------------------------------------
destructor TDPFNSOperationQueue.Destroy;
begin
{$IFDEF IOS}
  FNSOperationQueue.cancelAllOperations;
  FNSOperationQueue := nil;
{$ENDIF}
  inherited;
end;

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.SetMaxConcurrentOperationCount( const Value: Integer );
begin
  if Value < 1 then
    Exit;

  FMaxConcurrentOperationCount := Value;
{$IFDEF IOS}
  if Assigned( FNSOperationQueue ) then
    FNSOperationQueue.setMaxConcurrentOperationCount( Value );
{$ENDIF}
end;

{$IFDEF IOS}

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.SetSuspended( Suspended: Boolean );
begin
  if Assigned( FNSOperationQueue ) then
    FNSOperationQueue.setSuspended( Suspended );
end;

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.CancelAllOperations;
begin
  if Assigned( FNSOperationQueue ) then
  begin
    FNSOperationQueue.setSuspended( true );
    FNSOperationQueue.cancelAllOperations;
    FNSOperationQueue.setSuspended( false );
  end;
end;

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.WaitUntilAllOperationsAreFinished;
begin
  if Assigned( FNSOperationQueue ) then
    FNSOperationQueue.waitUntilAllOperationsAreFinished;
end;

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.AddOperation( op: NSOperation );
begin
  if Assigned( FNSOperationQueue ) then
    FNSOperationQueue.addOperation( op );
end;

// ----------------------------------------------------------------------------
function TDPFNSOperationQueue.GetOperationCount: Int64;
begin
  result := 0;
  if Assigned( FNSOperationQueue ) then
    result := FNSOperationQueue.operationCount;
end;

// ----------------------------------------------------------------------------
procedure TDPFNSOperationQueue.AddOperationBlock( Block: TDPFOperationQueueBlock );
begin
  // if FNSOperationQueue.respondsToSelector( sel_getUid( 'addOperationWithBlock:' ) ) then
  if Assigned( FNSOperationQueue ) then
    FNSOperationQueue.addOperationWithBlock( Block );
end;

// ----------------------------------------------------------------------------
class procedure TDPFNSOperationQueue.AddOperationBlockToMainQueue( Block: TDPFOperationQueueBlock );
begin
  TNSOperationQueue.Wrap( TNSOperationQueue.OCClass.mainQueue ).addOperationWithBlock( Block );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
end.
