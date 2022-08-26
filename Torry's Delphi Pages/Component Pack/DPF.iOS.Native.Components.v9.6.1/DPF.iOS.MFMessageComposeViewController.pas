// ------------------------------------------------------------------------------
// DPF.iOS.MFMessageComposeViewController Wrapped Classes & Interfaces
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
unit DPF.iOS.MFMessageComposeViewController;

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


// ===== External functions =====

const
  libMessageUI = '/System/Library/Frameworks/MessageUI.framework/MessageUI';

  // MFMessageComposeResult
  MessageComposeResultCancelled = 0;
  MessageComposeResultSent      = 1;
  MessageComposeResultFailed    = 2;

{$IFDEF IOS}

type
{$M+}
  MFMessageComposeResult                 = NSInteger;
  MFMessageComposeViewControllerDelegate = interface;

  // ----------------------------------------------------------------------------
  // MFMessageComposeViewController
  // ----------------------------------------------------------------------------
  MFMessageComposeViewControllerClass = interface( UINavigationControllerClass )
    ['{DD763AD9-D1C7-4D4D-BF15-0554B0BAA68D}']
    function canSendText: Boolean; cdecl;
  end;

  MFMessageComposeViewController = interface( UINavigationController )
    ['{E622AD32-FF44-4165-8666-7ABBDB20FA08}']

    function messageComposeDelegate: MFMessageComposeViewControllerDelegate; cdecl;
    procedure setMessageComposeDelegate( messageComposeDelegate: MFMessageComposeViewControllerDelegate ); cdecl;

    function body: NSString; cdecl;
    procedure setBody( body: NSString ); cdecl;

    function recipients: NSArray; cdecl;
    procedure setRecipients( recipients: NSArray ); cdecl;
  end;

  TMFMessageComposeViewController = class( TOCGenericImport<MFMessageComposeViewControllerClass, MFMessageComposeViewController> )
  end;

  // ----------------------------------------------------------------------------
  // MFMessageComposeViewControllerDelegate
  // ----------------------------------------------------------------------------
  MFMessageComposeViewControllerDelegate = interface( IObjectiveC )
    ['{E22AB8B9-7DCB-4BE9-AAFF-5186CC6C131F}']
    procedure messageComposeViewController( controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult ); cdecl;
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
