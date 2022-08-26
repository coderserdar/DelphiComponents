// ------------------------------------------------------------------------------
// DPF.iOS.Accounts Wrapped Classes & Interfaces
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
unit DPF.iOS.Accounts;

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
  FMX.Dialogs;

const
  libAccounts = '/System/Library/Frameworks/Accounts.framework/Accounts';

{$IFDEF IOS}

type
  id = pointer;

  ACAccountCredentialRenewResult = ( ACAccountCredentialRenewResultRenewed = 0, ACAccountCredentialRenewResultRejected = 1, ACAccountCredentialRenewResultFailed = 2 );

  TACAccountStoreSaveCompletionHandler          = procedure( success: boolean; error: NSError ) of object;
  TACAccountStoreRequestAccessCompletionHandler = procedure( granted: boolean; error: NSError ) of object;
  TACAccountStoreCredentialRenewalHandler       = procedure( renewResult: ACAccountCredentialRenewResult; error: NSError ) of object;
  TACAccountStoreRemoveCompletionHandler        = procedure( success: boolean; error: NSError ) of object;

  // ----------------------------------------------------------------------------
  // ACAccountCredential
  // ----------------------------------------------------------------------------
  ACAccountCredentialClass = interface( NSObjectClass )
    ['{BEBCAAE5-17A9-4AC8-A96F-3577EEBBF3DB}']
  end;

  ACAccountCredential = interface( NSObject )
    ['{DCF94958-AC06-44FF-B2C3-A6F6BF2FBC29}']

    function initWithOAuthToken( token: NSString; tokenSecret: NSString ): id; cdecl;
    function initWithOAuth2Token( token: NSString; refreshToken: NSString; expiryDate: NSDate ): id; cdecl;
    function oauthToken: NSString; cdecl;
  end;

  TACAccountCredential = class( TOCGenericImport<ACAccountCredentialClass, ACAccountCredential> )
  end;

  // ----------------------------------------------------------------------------
  // ACAccountType
  // ----------------------------------------------------------------------------
  ACAccountTypeClass = interface( NSObjectClass )
    ['{43E11D80-616D-4CF5-9D92-7587B531C72E}']
  end;

  ACAccountType = interface( NSObject )
    ['{5267102E-8E94-4E7D-B4F9-B319494A9774}']

    function accessGranted: boolean; cdecl;
    function accountTypeDescription: NSString; cdecl;
    function identifier: NSString; cdecl;
  end;

  TACAccountType = class( TOCGenericImport<ACAccountTypeClass, ACAccountType> )
  end;

  // ----------------------------------------------------------------------------
  // Account
  // ----------------------------------------------------------------------------
  ACAccountClass = interface( NSObjectClass )
    ['{C607F956-84C7-4CD2-A61F-4DC075A452CD}']
  end;

  ACAccount = interface( NSObject )
    ['{248C314F-52B7-42B5-929E-AB03761208F4}']

    function initWithAccountType( &type: ACAccountType ): id; cdecl;
    function accountDescription: NSString; cdecl;
    function accountType: ACAccountType; cdecl;
    function credential: ACAccountCredential; cdecl;
    function identifier: NSString; cdecl;
    function username: NSString; cdecl;
  end;

  TACAccount = class( TOCGenericImport<ACAccountClass, ACAccount> )
  end;

  // ----------------------------------------------------------------------------
  // ACAccountStore
  // ----------------------------------------------------------------------------
  ACAccountStoreClass = interface( NSObjectClass )
    ['{B6A2179A-94A7-4BAD-A06B-40F0355F1224}']
  end;

  ACAccountStore = interface( NSObject )
    ['{13603B9A-6078-4410-94AA-2053E0C7B214}']

    function accounts: NSArray; cdecl;
    function accountWithIdentifier( identifier: NSString ): ACAccount; cdecl;
    function accountsWithAccountType( accountType: ACAccountType ): NSArray; cdecl;

    function accountTypeWithAccountTypeIdentifier( typeIdentifier: NSString ): ACAccountType; cdecl;
    procedure saveAccount( account: ACAccount; completionHandler: TACAccountStoreSaveCompletionHandler ); cdecl;
    procedure requestAccessToAccountsWithType( accountType: ACAccountType; options: NSDictionary; completion: TACAccountStoreRequestAccessCompletionHandler ); cdecl;
    procedure renewCredentialsForAccount( account: ACAccount; completionHandler: TACAccountStoreCredentialRenewalHandler ); cdecl;
    procedure removeAccount( account: ACAccount; completionHandler: TACAccountStoreRemoveCompletionHandler ); cdecl;
  end;

  TACAccountStore = class( TOCGenericImport<ACAccountStoreClass, ACAccountStore> )
  end;

  // ----------------------------------------------------------------------------
  // Account Type Identifiers
  // Identifiers for supported account types.

function ACAccountTypeIdentifierFacebook: NSString; cdecl;
function ACAccountTypeIdentifierSinaWeibo: NSString; cdecl;
function ACAccountTypeIdentifierTwitter: NSString; cdecl;
function ACAccountTypeIdentifierTencentWeibo: NSString; cdecl;
{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
// ----------------------------------------------------------------------------
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iAccountsModule: THandle;
{$ENDIF}

  // ----------------------------------------------------------------------------
function ACAccountTypeIdentifierFacebook: NSString; cdecl;
begin
  result := CocoaNSStringConst( libAccounts, 'ACAccountTypeIdentifierFacebook' );
end;

// ----------------------------------------------------------------------------
function ACAccountTypeIdentifierSinaWeibo: NSString; cdecl;
begin
  result := CocoaNSStringConst( libAccounts, 'ACAccountTypeIdentifierSinaWeibo' );
end;

// ----------------------------------------------------------------------------
function ACAccountTypeIdentifierTwitter: NSString; cdecl;
begin
  result := CocoaNSStringConst( libAccounts, 'ACAccountTypeIdentifierTwitter' );
end;

// ----------------------------------------------------------------------------
function ACAccountTypeIdentifierTencentWeibo: NSString; cdecl;
begin
  result := CocoaNSStringConst( libAccounts, 'ACAccountTypeIdentifierTencentWeibo' );
end;

// ----------------------------------------------------------------------------
{$IF defined(CPUARM)}
procedure libAccountsLoader; cdecl; external libAccounts;
{$ELSE}

initialization

iAccountsModule := dlopen( MarshaledAString( libAccounts ), RTLD_LAZY );

finalization

dlclose( iAccountsModule );
{$ENDIF}
{$ENDIF}

end.
