// ------------------------------------------------------------------------------
// DPF.iOS.UIPopoverController Component
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
unit DPF.iOS.UIPopoverController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIViewController,
{$IFDEF IOS}
  iOSapi.CocoaTypes,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  iOSapi.UIKit,
  IOSapi.Foundation,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Types;

type

  TDPFPopoverArrowDirection = ( padUp = 1, padDown = 2, padLeft = 4, padRight = 8, padAny = 15, padUnknown = 16 );

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFPopover = class( TDPFUIViewController )
  private
{$IFDEF IOS}
    FUIPopover: UIPopoverController;
{$ENDIF}
  protected
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure ShowPopup( ShowOver: TDPFiOSBaseControl; ArrowDirection: TDPFPopoverArrowDirection ); overload;
{$IFDEF IOS}
    procedure ShowPopup( BarButtonItem: UIBarButtonItem; ArrowDirection: TDPFPopoverArrowDirection ); overload;
{$ENDIF}
    procedure HidePopup;
  published
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFPopover }
// ------------------------------------------------------------------------------
constructor TDPFPopover.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  AddThisToSubView := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFPopover.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FUIPopover ) then
  begin
    FUIPopover.release;
    FUIPopover := nil;
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFPopover.Loaded;
begin

  // ----------------------------
  // Important
  inherited;
end;
// ------------------------------------------------------------------------------

procedure TDPFPopover.ShowPopup( BarButtonItem: UIBarButtonItem; ArrowDirection: TDPFPopoverArrowDirection );
begin
  if IsIPad then
  begin
    if Assigned( FUIPopover ) then
    begin
      FUIPopover.dismissPopoverAnimated( false ); // Graham added this line
      FUIPopover.release;
      FUIPopover := nil;
    end;

    FUIPopover := TUIPopoverController.Wrap( TUIPopoverController.Alloc.initWithContentViewController( FUIViewController ) );

    FUIPopover.setPopoverContentSize( FUIViewController.view.bounds.size );
    if Assigned( FUIPopover ) then
      FUIPopover.presentPopoverFromBarButtonItem( BarButtonItem, Integer( ArrowDirection ), True );

  end
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFPopover.ShowPopup( ShowOver: TDPFiOSBaseControl; ArrowDirection: TDPFPopoverArrowDirection );
{$IFDEF IOS}
var
  frame: CGRect;
  Win  : UIWindow;
{$ENDIF}
begin
{$IFDEF IOS}
  if IsIPad then
  begin
    if Assigned( FUIPopover ) then
    begin
      FUIPopover.dismissPopoverAnimated( false ); // added this line
      FUIPopover.release;
      FUIPopover := nil;
    end;

    FUIPopover := TUIPopoverController.Wrap( TUIPopoverController.Alloc.initWithContentViewController( FUIViewController ) );

    Win   := GetSharedApplication.keyWindow;
    frame := FUIViewController.view.bounds;
    if Assigned( ShowOver ) then
      frame := UIView( ShowOver.UIControl ).frame
    else
      frame := FUIViewController.view.bounds;

    FUIPopover.setPopoverContentSize( FUIViewController.view.bounds.size );
    if Assigned( FUIPopover ) then
      FUIPopover.presentPopoverFromRect( frame, Win.rootViewController.View, Integer( ArrowDirection ), True );
  end
  else
  begin
    self.Visible     := true;
    AddThisToSubView := true;
    Loaded;
    self.FUIViewController.view.setCenter( UIView( ShowOver.UIControl ).center );
    frame          := self.FUIViewController.view.frame;
    frame.origin.y := UIView( ShowOver.UIControl ).frame.origin.y + UIView( ShowOver.UIControl ).frame.origin.y + UIView( ShowOver.UIControl ).frame.size.height;
    self.FUIViewController.view.setFrame( frame );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFPopover.HidePopup;
begin
{$IFDEF IOS}
  if IsIPad then
  begin
    if Assigned( FUIPopover ) then
      FUIPopover.dismissPopoverAnimated( true );
  end
  else
    self.Visible := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
