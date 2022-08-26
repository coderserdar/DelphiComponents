// ------------------------------------------------------------------------------
// DPF.iOS.UIActivityViewController Component
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
unit DPF.iOS.UIActivityViewController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
{$IFDEF IOS6}
  Macapi.ObjectiveC,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  System.TypInfo;

type

  TDPFActivityViewController = class;

  TDPFActivityExcludedType = ( atMail, atPrint, atMessage, atPostToWeibo, atPostToTwitter, atPostToFacebook, atAssignToContact, atSaveToCameraRoll, atCopyToPasteboard );

{$IFDEF IOS6}

  // iOS 6.0 and later.
  UIActivityViewControllerClass = interface( UIViewControllerClass )
    ['{A498B704-02AA-459E-AFCC-98845E143FEF}']
  end;

  // iOS 6.0 and later.
  UIActivityViewController = interface( UIViewController )
    ['{7B3B16DE-00E3-4806-A852-82C99994D013}']
    function initWithActivityItems( activityItems: NSArray; applicationActivities: NSArray ): Pointer; cdecl; // iOS 6.0 and later.

    function completionHandler: UIActivityViewControllerCompletionHandler; cdecl; // iOS 6.0 and later.
    procedure setCompletionHandler( completionHandler: UIActivityViewControllerCompletionHandler ); cdecl; // iOS 6.0 and later.

    function excludedActivityTypes: NSArray; cdecl; // iOS 6.0 and later.
    procedure setExcludedActivityTypes( excludedActivityTypes: NSArray ); cdecl; // iOS 6.0 and later.
  end;

  TUIActivityViewController = class( TOCGenericImport<UIActivityViewControllerClass, UIActivityViewController> )
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFActivityViewController = class( TComponent )
  private
{$IFDEF IOS6}
    FDPFActivityViewController: UIActivityViewController;
    FDPFActivityItems         : NSMutableArray;
    FDPFPopover               : UIPopoverController;
{$ENDIF}
  protected
    procedure ViewControllerCompletion;
{$IFDEF IOS6}
{$ENDIF}
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ShowActivity( AControl: TDPFiOSBaseControl; const AText: string; const ABitmap: TBitmap; ExcludedTypes: array of TDPFActivityExcludedType );
  published
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFActivityViewController }
// ------------------------------------------------------------------------------
constructor TDPFActivityViewController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF IOS6}
  FDPFActivityItems := TNSMutableArray.Create;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFActivityViewController.Destroy;
begin
{$IFDEF IOS6}
  if Assigned( FDPFActivityViewController ) then
    FDPFActivityViewController.release;

  if Assigned( FDPFPopover ) then
    FDPFPopover.release;
  FDPFActivityItems.release;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityViewController.ShowActivity( AControl: TDPFiOSBaseControl; const AText: string; const ABitmap: TBitmap; ExcludedTypes: array of TDPFActivityExcludedType );
const
  TDPFExtTypes: array [TDPFActivityExcludedType] of string = ( 'UIActivityTypeMail', 'UIActivityTypePrint', 'UIActivityTypeMessage', 'UIActivityTypePostToWeibo', 'UIActivityTypePostToTwitter', 'UIActivityTypePostToFacebook', 'UIActivityTypeAssignToContact', 'UIActivityTypeSaveToCameraRoll', 'UIActivityTypeCopyToPasteboard' );
{$IFDEF IOS6}
var
  I      : Integer;
  frame  : CGRect;
  Win    : UIWindow;
  OCImage: UIImage;
  NSArr  : NSMutableArray;
{$ENDIF}
begin
{$IFDEF IOS6}
  if Assigned( FDPFActivityViewController ) then
  begin
    FDPFActivityViewController.release;
    FDPFActivityViewController := nil;
  end;

  FDPFActivityItems.removeAllObjects;

  if not AText.IsEmpty then
    FDPFActivityItems.addObject( ( NSSTR( AText ) as ILocalObject ).GetObjectID );

  if Assigned( ABitmap ) and not ABitmap.IsEmpty then
  begin
    OCImage := BitmapToUIImage( ABitmap );
    FDPFActivityItems.addObject( ( OCImage as ILocalObject ).GetObjectID );
  end;

  if FDPFActivityItems.count > 0 then
  begin
    FDPFActivityViewController := TUIActivityViewController.Wrap( TUIActivityViewController.Alloc.initWithActivityItems( FDPFActivityItems, nil ) );
  end
  else
    exit;

  NSArr := TNSMutableArray.Create;
  for I := 0 to high( ExcludedTypes ) do
  begin
    NSArr.addObject( ( CocoaNSStringConst( libKit, TDPFExtTypes[ExcludedTypes[i]] ) as ILocalObject ).GetObjectID );
  end;
  if Length( ExcludedTypes ) > 0 then
    FDPFActivityViewController.setExcludedActivityTypes( NSArr );

  Win := GetSharedApplication.keyWindow;

  if not IsIPad then
  begin
    Win.rootViewController.presentViewController( FDPFActivityViewController, true, ViewControllerCompletion );
  end
  else
  begin
    if Assigned( FDPFPopover ) then
    begin
      FDPFPopover.release;
      FDPFPopover := nil;
    end;
    FDPFPopover := TUIPopoverController.Wrap( TUIPopoverController.Alloc.initWithContentViewController( FDPFActivityViewController ) );
    //FDPFPopover.retain;

    if Assigned( AControl ) then
      frame := UIView( AControl.UIControl ).bounds
    else
      frame := CGRectMake( 0, 0, 0, 0 );

    FDPFPopover.presentPopoverFromRect( frame, Win.rootViewController.View, UIPopoverArrowDirectionAny, True );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityViewController.ViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------

end.
