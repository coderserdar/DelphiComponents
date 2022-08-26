// ------------------------------------------------------------------------------
// DPF.iOS.MFMailComposeViewController Wrapped Classes & Interfaces
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
unit DPF.iOS.MFMailComposeViewController;

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
  DPF.iOS.Common;


// ===== External functions =====

const
  libMessageUI = '/System/Library/Frameworks/MessageUI.framework/MessageUI';

  MFMailComposeResultCancelled = 0;
  MFMailComposeResultSaved     = 1;
  MFMailComposeResultSent      = 2;
  MFMailComposeResultFailed    = 3;

{$IFDEF IOS}

type
{$M+}
  MFMailComposeResult                 = NSInteger;
  MFMailComposeViewControllerDelegate = interface;

  // ----------------------------------------------------------------------------
  // MFMailComposeViewController
  // ----------------------------------------------------------------------------
  MFMailComposeViewControllerClass = interface( UINavigationControllerClass )
    ['{F9F0901D-FA19-4E11-A3D5-CC77384B5A30}']
    function canSendMail: Boolean; cdecl;
  end;

  MFMailComposeViewController = interface( UINavigationController )
    ['{1E3E2D5A-C07B-4FC9-B27F-3A676FE374C5}']
    function mailComposeDelegate: MFMailComposeViewControllerDelegate; cdecl;
    procedure setMailComposeDelegate( mailComposeDelegate: MFMailComposeViewControllerDelegate ); cdecl;
    procedure setSubject( subject: NSString ); cdecl;
    procedure setToRecipients( toRecipients: NSArray ); cdecl;
    procedure setCcRecipients( ccRecipients: NSArray ); cdecl;
    procedure setBccRecipients( bccRecipients: NSArray ); cdecl;
    procedure setMessageBody( body: NSString; isHTML: Boolean ); cdecl;
    procedure addAttachmentData( attachment: NSData; mimeType: NSString; fileName: NSString ); cdecl;
  end;

  TMFMailComposeViewController = class( TOCGenericImport<MFMailComposeViewControllerClass, MFMailComposeViewController> )
  end;

  // ----------------------------------------------------------------------------
  // MFMailComposeViewControllerDelegate
  // ----------------------------------------------------------------------------
  MFMailComposeViewControllerDelegate = interface( IObjectiveC )
    ['{697CC853-5BCE-4979-B5B2-E9AFC9A03950}']
    procedure mailComposeController( controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError ); cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iMessageUIModule: THandle;
{$ENDIF}
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure LibMessageUIFakeLoader; cdecl; external libMessageUI;
{$ELSE}

initialization

iMessageUIModule := dlopen( MarshaledAString( libMessageUI ), RTLD_LAZY );

finalization

dlclose( iMessageUIModule );
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
