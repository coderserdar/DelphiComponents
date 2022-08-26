// ------------------------------------------------------------------------------
// DPF.iOS.MFMailCompose Component
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
unit DPF.iOS.MFMailCompose;

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
  DPF.iOS.UIImageView,
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
  DPF.iOS.MFMailComposeViewController,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFMailCompose = class;

  TDPFOnSendStatus = procedure( Sender: TObject; Result: Integer ) of object;
{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  TMFMailComposeViewControllerDelegate = class( TOCLocal, MFMailComposeViewControllerDelegate )
  private
    FDPFMailCompose: TDPFMailCompose;
  public
    constructor Create( DPFMailCompose: TDPFMailCompose );
    procedure mailComposeController( controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFMailCompose = class( TComponent )
  private
    FOnSendStatus: TDPFOnSendStatus;
{$IFDEF IOS}
    FMailCompose                        : MFMailComposeViewController;
    FMFMailComposeViewControllerDelegate: TMFMailComposeViewControllerDelegate;
{$ENDIF}
  protected
    procedure PresentViewControllerCompleted;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function MailCompose( Subject: string; MessageBody: string; MessageBodyisHtml: Boolean; Recipients: array of string; CCRecipients: array of string; BCCRecipients: array of string; AttachedFiles: array of string; ImageView: TDPFImageView = nil; ImageViewName: string = 'DPF-Photo.png' ): Boolean;
    function CanSendMail: Boolean;
  published
    property OnSendStatus: TDPFOnSendStatus read FOnSendStatus write FOnSendStatus;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFMailCompose }
constructor TDPFMailCompose.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
end;

// ------------------------------------------------------------------------------
destructor TDPFMailCompose.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FMFMailComposeViewControllerDelegate ) then
    FMFMailComposeViewControllerDelegate.DisposeOf;

  if Assigned( FMailCompose ) then
    FMailCompose.release;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFMailCompose.PresentViewControllerCompleted;
begin
  { }
end;

// ------------------------------------------------------------------------------
function TDPFMailCompose.CanSendMail: Boolean;
begin
{$IFDEF IOS}
  try
    Result := TMFMailComposeViewController.OCClass.canSendMail;
  except
    Result := false;
    exit;
  end;
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFMailCompose.MailCompose( Subject: string; MessageBody: string; MessageBodyisHtml: Boolean; Recipients: array of string; CCRecipients: array of string; BCCRecipients: array of string; AttachedFiles: array of string; ImageView: TDPFImageView = nil; ImageViewName: string = 'DPF-Photo.png' ): Boolean;
{$IFNDEF IOS}
begin
  result := false;
end;

{$ELSE}

var
  I               : Integer;
  AttFile         : NSData;
  imageData       : NSData;
  RecipientsArr   : NSMutableArray;
  CCRecipientsArr : NSMutableArray;
  BCCRecipientsArr: NSMutableArray;
  FMainWindow     : UIWindow;
begin
  try
    Result := TMFMailComposeViewController.OCClass.canSendMail;
  except
    Result := false;
    exit;
  end;
  if not Result then
    exit;

  RecipientsArr    := TNSMutableArray.Create;
  CCRecipientsArr  := TNSMutableArray.Create;
  BCCRecipientsArr := TNSMutableArray.Create;

  FMailCompose := TMFMailComposeViewController.Wrap( TMFMailComposeViewController.Alloc.init );
  if not assigned( FMailCompose ) then
    exit;

  if not assigned( FMFMailComposeViewControllerDelegate ) then
  begin
    FMFMailComposeViewControllerDelegate := TMFMailComposeViewControllerDelegate.Create( Self );
    FMailCompose.setMailComposeDelegate( FMFMailComposeViewControllerDelegate );
  end;

  FMailCompose.setSubject( NSStr( Subject ) );
  FMailCompose.setMessageBody( NSStr( MessageBody ), MessageBodyisHtml );

  // Recipients
  if Length( Recipients ) > 0 then
  begin
    RecipientsArr := TNSMutableArray.Create;
    for I         := 0 to high( Recipients ) do
      RecipientsArr.addObject( ( NSStr( Recipients[I] ) as ILocalObject ).GetObjectID );
    FMailCompose.setToRecipients( RecipientsArr );
  end;

  // CC Recipients
  if Length( CCRecipients ) > 0 then
  begin
    CCRecipientsArr := TNSMutableArray.Create;
    for I           := 0 to high( CCRecipients ) do
      CCRecipientsArr.addObject( ( NSStr( CCRecipients[I] ) as ILocalObject ).GetObjectID );
    FMailCompose.setCcRecipients( RecipientsArr );
  end;

  // BCC Recipients
  if Length( BCCRecipients ) > 0 then
  begin
    for I := 0 to high( BCCRecipients ) do
      BCCRecipientsArr.addObject( ( NSStr( BCCRecipients[I] ) as ILocalObject ).GetObjectID );
    FMailCompose.setBccRecipients( BCCRecipientsArr );
  end;

  // Attached Files

  if Assigned( ImageView ) then
  begin
    imageData := TNSData.Wrap( UIImagePNGRepresentation( ( ImageView.GetUIImage as ILocalObject ).GetObjectID ) );
    FMailCompose.addAttachmentData( imageData, NSStr( 'image/png' ), NSStr( ImageViewName ) );
  end;

  if Length( AttachedFiles ) > 0 then
  begin
    for I := 0 to high( AttachedFiles ) do
      if FileExists( AttachedFiles[I] ) then
      begin
        AttFile := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfFile( NSStr( AttachedFiles[I] ) ) );
        FMailCompose.addAttachmentData( AttFile, NSStr( GetFileMIMEType( AttachedFiles[I] ) ), NSStr( ExtractFileName( AttachedFiles[I] ) ) );
      end;
  end;

  FMainWindow := GetSharedApplication.keyWindow;
  if Assigned( FMainWindow ) and Assigned( FMainWindow.rootViewController ) then
    FMainWindow.rootViewController.presentViewController( FMailCompose, True, PresentViewControllerCompleted );

  RecipientsArr.release;
  CCRecipientsArr.release;
  BCCRecipientsArr.release;
end;
// ------------------------------------------------------------------------------
{ TMFMailComposeViewControllerDelegate }

constructor TMFMailComposeViewControllerDelegate.Create( DPFMailCompose: TDPFMailCompose );
begin
  inherited Create;
  FDPFMailCompose := DPFMailCompose;
end;

// ------------------------------------------------------------------------------
procedure TMFMailComposeViewControllerDelegate.mailComposeController( controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError );
begin

  case didFinishWithResult of
    MFMailComposeResultCancelled:
      ;
    MFMailComposeResultSaved:
      ;
    MFMailComposeResultSent:
      ;
    MFMailComposeResultFailed:
      ;
  end;
  FDPFMailCompose.FMailCompose.dismissViewControllerAnimated( True, FDPFMailCompose.PresentViewControllerCompleted );
  if Assigned( FDPFMailCompose.FOnSendStatus ) then
    FDPFMailCompose.FOnSendStatus( FDPFMailCompose, didFinishWithResult );

  FDPFMailCompose.FMailCompose.release;
  FDPFMailCompose.FMailCompose := nil;
  FDPFMailCompose.FMFMailComposeViewControllerDelegate.DisposeOf;
  FDPFMailCompose.FMFMailComposeViewControllerDelegate := nil;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
