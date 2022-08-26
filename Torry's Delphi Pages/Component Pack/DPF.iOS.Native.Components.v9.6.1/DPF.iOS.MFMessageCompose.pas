// ------------------------------------------------------------------------------
// DPF.iOS.MFMessageCompose Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// EMail #2: b_yaghobi@yahoo.com
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
unit DPF.iOS.MFMessageCompose;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
  DPF.iOS.MFMessageComposeViewController,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFMessageCompose = class;

  TDPFOnSendStatus = procedure( Sender: TObject; Result: Integer ) of object;
{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  TMFMessageComposeViewControllerDelegate = class( TOCLocal, MFMessageComposeViewControllerDelegate )
  private
    FDPFMessageCompose: TDPFMessageCompose;
  public
    constructor Create( DPFMessageCompose: TDPFMessageCompose );
    procedure messageComposeViewController( controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFMessageCompose = class( TComponent )
  private
    FOnSendStatus: TDPFOnSendStatus;
{$IFDEF IOS}
    FMainWindow                            : UIWindow;
    FMessageCompose                        : MFMessageComposeViewController;
    FMFMessageComposeViewControllerDelegate: TMFMessageComposeViewControllerDelegate;
    RecipientsArr                          : NSMutableArray;
{$ENDIF}
  protected
    procedure ViewControllerCompletion;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function MessageCompose( Body: string; Recipients: array of string ): Boolean;
  published
    property OnSendStatus: TDPFOnSendStatus read FOnSendStatus write FOnSendStatus;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFMessageCompose }
// ------------------------------------------------------------------------------
constructor TDPFMessageCompose.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  FMessageCompose                         := nil;
  RecipientsArr                           := TNSMutableArray.Create;
  FMFMessageComposeViewControllerDelegate := TMFMessageComposeViewControllerDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFMessageCompose.Destroy;
begin
{$IFDEF IOS}
  RecipientsArr.release;
  FMFMessageComposeViewControllerDelegate.DisposeOf;
  if Assigned( FMessageCompose ) then
    FMessageCompose.release;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFMessageCompose.ViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------
function TDPFMessageCompose.MessageCompose( Body: string; Recipients: array of string ): Boolean;
{$IFNDEF IOS}
begin
  result := false;
end;
{$ELSE}

var
  I: Integer;
begin
  RecipientsArr.removeAllObjects;

  result := TMFMessageComposeViewController.OCClass.canSendText;
  if not Result then
  begin
    Exit;
  end;

  if not Assigned( FMessageCompose ) then
  begin
    FMessageCompose := TMFMessageComposeViewController.Create;
    FMessageCompose.setMessageComposeDelegate( FMFMessageComposeViewControllerDelegate );
  end;
  FMessageCompose.setBody( NSSTR( Body ) );

  // Recipients
  if Length( Recipients ) > 0 then
  begin
    RecipientsArr := TNSMutableArray.Create;
    for I         := 0 to high( Recipients ) do
      RecipientsArr.addObject( ( NSSTR( Recipients[I] ) as ILocalObject ).GetObjectID );
    FMessageCompose.setRecipients( RecipientsArr );
  end;

  FMainWindow := GetSharedApplication.keyWindow;
  if Assigned( FMainWindow ) and Assigned( FMainWindow.rootViewController ) then
    FMainWindow.rootViewController.presentViewController( FMessageCompose, True, ViewControllerCompletion );

end;
// ------------------------------------------------------------------------------
{ TMFMessageComposeViewControllerDelegate }

constructor TMFMessageComposeViewControllerDelegate.Create( DPFMessageCompose: TDPFMessageCompose );
begin
  inherited Create;
  FDPFMessageCompose := DPFMessageCompose;
end;

// ------------------------------------------------------------------------------
procedure TMFMessageComposeViewControllerDelegate.messageComposeViewController( controller: MFMessageComposeViewController; didFinishWithResult: MFMessageComposeResult );
begin

  case didFinishWithResult of
    MessageComposeResultCancelled:
      ;
    MessageComposeResultSent:
      ;
    MessageComposeResultFailed:
      ;
  end;
  FDPFMessageCompose.FMainWindow.rootViewController.dismissModalViewControllerAnimated( True );
  if Assigned( FDPFMessageCompose.FOnSendStatus ) then
    FDPFMessageCompose.FOnSendStatus( FDPFMessageCompose, didFinishWithResult );

  FDPFMessageCompose.FMessageCompose.release;
  FDPFMessageCompose.FMessageCompose := nil;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
