// ------------------------------------------------------------------------------
// DPF.iOS.LocalAuthentication Wrapped Classes & Interfaces
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
unit DPF.iOS.LocalAuthentication;

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
  DPF.iOS.BaseControl;

const
  libLocalAuthentication = '/System/Library/Frameworks/LocalAuthentication.framework/LocalAuthentication';

type
  id = pointer;

  ACAccountCredentialRenewResult = ( ACAccountCredentialRenewResultRenewed = 0, ACAccountCredentialRenewResultRejected = 1, ACAccountCredentialRenewResultFailed = 2 );
  LAError                        = ( LAErrorAuthenticationFailed, LAErrorUserCancel, LAErrorUserFallback, LAErrorSystemCancel, LAErrorPasscodeNotSet, LAErrorTouchIDNotAvailable, LAErrorTouchIDNotEnrolled );
  TLocalAuthenticationResult     = procedure( Sender: TObject; Result: Boolean; ErrorText: string ) of object;
{$IFDEF IOS}
  TLocalAuthenticationCompletionReply = procedure( result: pointer; error: Pointer ) of object;

  LAPolicy = ( LADONOTUSE, LAPolicyDeviceOwnerAuthenticationWithBiometrics, LAPolicyDeviceOwnerAuthentication );

  // ----------------------------------------------------------------------------
  // LAContextClass
  // ----------------------------------------------------------------------------
  LAContextClass = interface( NSObjectClass )
    ['{144DA075-6F12-4785-BCBC-B6107EB7916F}']
  end;

  LAContext = interface( NSObject )
    ['{BCBAF0A6-0593-430D-BB2C-6B19DB5AAB0A}']

    function canEvaluatePolicy( policy: LAPolicy; error: NSError ): Boolean; cdecl;
    procedure evaluatePolicy( policy: LAPolicy; localizedReason: NSString; reply: TLocalAuthenticationCompletionReply ); cdecl;
  end;

  TLAContext = class( TOCGenericImport<LAContextClass, LAContext> )
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or PidiOSDevice or PidiOSSimulator or pidiOSDevice64 )]
  TDPFLocalAuthentication = class( TComponent )
  private
    FResultProc: TLocalAuthenticationResult;
{$IFDEF IOS}
    procedure RecievedReply( result: pointer; error: Pointer );
{$ENDIF}
  public
    procedure StartAuthentication( ReasonString: string );
    // procedure Stop;
  published
    property OnAuthenticationComplete: TLocalAuthenticationResult read FResultProc write FResultProc;
  end;

{$IFDEF IOS}

function LAErrorDomain: NSString; cdecl;
{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
// ----------------------------------------------------------------------------
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iLocalAuthenticationModule: THandle;
{$ENDIF}

procedure TDPFLocalAuthentication.RecievedReply( result: pointer; error: Pointer );
var
  LocalError  : NSError;
  ErrorMessage: string;
begin
  if ( Assigned( FResultProc ) ) then
  begin
    if ( Assigned( error ) ) then
    begin
      LocalError   := TNSError.Wrap( error );
      ErrorMessage := UTF8ToString( LocalError.localizedDescription.UTF8String );
    end;

    if ( not Boolean( result ) ) or ( not( ErrorMessage = '' ) ) then
      FResultProc( self, False, ErrorMessage )
    else
      FResultProc( self, True, '' );
  end;
end;

// ----------------------------------------------------------------------------
function LAErrorDomain: NSString; cdecl;
begin
  result := CocoaNSStringConst( libLocalAuthentication, 'LAErrorDomain' );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
procedure TDPFLocalAuthentication.StartAuthentication( ReasonString: string );
{$IFDEF IOS}
var
  LocalizedReasonString: NSString;
  LocalError           : NSError;
  LocalLAContext       : LAContext;
{$ENDIF}
begin
{$IFDEF IOS}
  LocalizedReasonString := NSStr( ReasonString );
  LocalLAContext        := TLAContext.Wrap( TLAContext.Alloc.init );
  if Assigned( LocalLAContext ) then
  begin
    if ( LocalLAContext.canEvaluatePolicy( LAPolicyDeviceOwnerAuthenticationWithBiometrics, LocalError ) ) then
    begin
      LocalLAContext.evaluatePolicy( LAPolicyDeviceOwnerAuthenticationWithBiometrics, LocalizedReasonString, RecievedReply );
    end
    else
    begin
      if ( Assigned( FResultProc ) ) then
        FResultProc( Self, False, UTF8ToString( LocalError.localizedDescription.UTF8String ) );
    end;
  end
  else
  begin
    if ( Assigned( FResultProc ) ) then
      FResultProc( Self, False, 'Error initialising authentication service' );
  end;
  if Assigned( LocalLAContext ) then
  begin
    LocalLAContext.release;
    LocalLAContext := nil;
  end;
{$ENDIF}
end;

// ----------------------------------------------------------------------------
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure libLocalAuthenticationLoader; cdecl; external libLocalAuthentication;
{$ELSE}

initialization

iLocalAuthenticationModule := dlopen( MarshaledAString( libLocalAuthentication ), RTLD_LAZY );

finalization

dlclose( iLocalAuthenticationModule );
{$ENDIF}
{$ENDIF}

end.
