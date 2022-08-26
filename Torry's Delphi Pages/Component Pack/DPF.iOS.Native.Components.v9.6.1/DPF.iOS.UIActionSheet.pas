// ------------------------------------------------------------------------------
// DPF.iOS.UIActionSheet Component
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
unit DPF.iOS.UIActionSheet;

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
  DPF.iOS.UIFont,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
{$ENDIF}
  DPF.iOS.Common,
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

{$IFDEF IOS}
  TDPFUIActionSheet = class;

  // ------------------------------------------------------------------------------

  UIActionSheetDelegate = interface( IObjectiveC )
    ['{D034E69A-7423-412A-9207-4C72F03FC504}']
    procedure actionSheet( actionSheet: UIActionSheet; clickedButtonAtIndex: NSInteger ); cdecl;
    [MethodName( 'actionSheet:didDismissWithButtonIndex:' )]
    procedure actionSheetDidDismissWithButtonIndex( actionSheet: UIActionSheet; didDismissWithButtonIndex: NSInteger ); cdecl;
    procedure actionSheetCancel( actionSheet: UIActionSheet ); cdecl;
    procedure didPresentActionSheet( actionSheet: UIActionSheet ); cdecl;
    procedure willPresentActionSheet( actionSheet: UIActionSheet ); cdecl;
  end;

  TDPFActionSheetDelegate = class( TOCLocal, UIActionSheetDelegate )
  private
    FDPFUIActionSheet: TDPFUIActionSheet;
    FModalResult     : TModalResult;
    FModal           : Boolean;
  public
    constructor Create( ADPFUIActionSheet: TDPFUIActionSheet );
    procedure actionSheet( actionSheet: UIActionSheet; clickedButtonAtIndex: NSInteger ); cdecl;
    [MethodName( 'actionSheet:didDismissWithButtonIndex:' )]
    procedure actionSheetDidDismissWithButtonIndex( actionSheet: UIActionSheet; didDismissWithButtonIndex: NSInteger ); cdecl;
    procedure actionSheetCancel( actionSheet: UIActionSheet ); cdecl;
    procedure didPresentActionSheet( actionSheet: UIActionSheet ); cdecl;
    procedure willPresentActionSheet( actionSheet: UIActionSheet ); cdecl;
    property ModalResult: TModalResult read FModalResult;
    property Modal: Boolean read FModal;
  end;

  // ------------------------------------------------------------------------------
  IActionSheetGestureRecognizerDelegate = interface( IObjectiveC )
    ['{37FFBFC1-9BDB-4457-A98C-DC44C633DB2B}']
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

  TDPFActionSheetGestureRecognizerDelegate = class( TOCLocal, IActionSheetGestureRecognizerDelegate )
  private
    FDPFUIActionSheet: TDPFUIActionSheet;
  public
    constructor Create( ADPFUIActionSheet: TDPFUIActionSheet );
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  // TDPFActionSheetButtonItem

  TDPFActionSheetButtonKind = ( bkOther, bkDestructive, bkCancel, bkCustom );

  TDPFActionSheetButtonItem = class( TCollectionItem )
  private
    FOwner                   : TCollection;
    FTitle                   : string;
    FBackgroundImageNormal   : string;
    FBackgroundImageHighlight: string;
    FBackgroundImageDisable  : string;
    FBackgroundColor         : TAlphaColor;
    FBorderColor             : TAlphaColor;
    FCornerRadius            : Single;
    FBorderWidth             : Integer;
    FImageHighlight          : string;
    FImageDisable            : string;
    FImageNormal             : string;
    FFont                    : TDPFFont;
    FTitleColorNormal        : TAlphaColor;
    FTitleColorDisable       : TAlphaColor;
    FTitleColorHighlight     : TAlphaColor;
    FButtonKind              : TDPFActionSheetButtonKind;
    FModalResult             : TModalResult;
    procedure SetFont( const Value: TDPFFont );
    procedure SetModalResult( const Value: TModalResult );
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
  published
    property Font               : TDPFFont read FFont write SetFont;
    property Title              : string read FTitle write FTitle;
    property TitleColorNormal   : TAlphaColor read FTitleColorNormal write FTitleColorNormal default TAlphaColors.Black;
    property TitleColorHighlight: TAlphaColor read FTitleColorHighlight write FTitleColorHighlight default TAlphaColors.White;
    property TitleColorDisable  : TAlphaColor read FTitleColorDisable write FTitleColorDisable default TAlphaColors.Gray;
    property ButtonKind         : TDPFActionSheetButtonKind read FButtonKind write FButtonKind default TDPFActionSheetButtonKind.bkOther;

    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.Null;

    property BackgroundImageNormal   : string read FBackgroundImageNormal write FBackgroundImageNormal;
    property BackgroundImageHighlight: string read FBackgroundImageHighlight write FBackgroundImageHighlight;
    property BackgroundImageDisable  : string read FBackgroundImageDisable write FBackgroundImageDisable;

    property ImageNormal   : string read FImageNormal write FImageNormal;
    property ImageHighlight: string read FImageHighlight write FImageHighlight;
    property ImageDisable  : string read FImageDisable write FImageDisable;

    property BorderWidth : Integer read FBorderWidth write FBorderWidth default 0;
    property BorderColor : TAlphaColor read FBorderColor write FBorderColor default TAlphaColors.Null;
    property CornerRadius: Single read FCornerRadius write FCornerRadius;

    property ModalResult: TModalResult read FModalResult write SetModalResult;
  end;

  // ----------------------------------------------------------------------------
  TDPFActionSheetButtonsCollection = class( TCollection )
  private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFActionSheetButtonItem;
    procedure SetItem( Index: Integer; Value: TDPFActionSheetButtonItem );

  public
    constructor Create( AOwner: TComponent );
    destructor Destroy; override;

    function Add: TDPFActionSheetButtonItem;
    function Insert( Index: Integer ): TDPFActionSheetButtonItem;

    property Items[index: Integer]: TDPFActionSheetButtonItem read GetItem write SetItem; default;
  end;

  // ----------------------------------------------------------------------------
  TDPFActionSheetOnClick = procedure( Sender: TObject; ButtonIndex: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIActionSheet = class( TComponent )
  private
    FOnClick             : TDPFActionSheetOnClick;
    FButtons             : TDPFActionSheetButtonsCollection;
    FTitle               : string;
    FTitleColor          : TAlphaColor;
    FTitleBackGroundColor: TAlphaColor;
    FOnDismiss           : TDPFActionSheetOnClick;
    FPopoverControl      : TControl;
    // FTitleFont           : TDPFFont;

{$IFDEF IOS}
    FMessageView                            : UIActionSheet;
    FUITapGestureRecognizer                 : UITapGestureRecognizer;
    FDPFActionSheetDelegate                 : TDPFActionSheetDelegate;
    FDPFActionSheetGestureRecognizerDelegate: TDPFActionSheetGestureRecognizerDelegate;
    procedure SetBkImage( FUIButton: UIButton; ImageName: string; ImageState: Integer );
    procedure SetFgImage( FUIButton: UIButton; ImageName: string; ImageState: Integer );
    procedure SetBkColor( FUIButton: UIButton; Color: TAlphaColor );
    procedure SetTitleColor( FUIButton: UIButton; Color: TAlphaColor; ColorState: Integer );

    procedure SetLabelTextColor( FUILabel: UILabel; Color: TAlphaColor );
    procedure SetLabelBackColor( FUIlabel: UILabel; Color: TAlphaColor );
{$ENDIF}
    procedure SetButtons( const Value: TDPFActionSheetButtonsCollection );
    // procedure SetTitleFont( const Value: TDPFFont );
  protected
    function GetButtonItem( Title: string ): TDPFActionSheetButtonItem;
    procedure DoDismiss( ButtonIndex: Integer );
    procedure DoClick( ButtonIndex: Integer );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure ShowMessage; overload;
    procedure ShowMessage( AMessageTitle: string; const AButtons: array of string ); overload;
    function Execute: TModalResult; overload;
    function Execute( APopoverControl: TControl ): TModalResult; overload; // Added by Graham
  published
    property OnClick             : TDPFActionSheetOnClick read FOnClick write FOnClick;
    property OnDismiss           : TDPFActionSheetOnClick read FOnDismiss write FOnDismiss;
    property Title               : string read FTitle write FTitle;
    property TitleColor          : TAlphaColor read FTitleColor write FTitleColor default TAlphaColors.White;
    property TitleBackGroundColor: TAlphaColor read FTitleBackGroundColor write FTitleBackGroundColor default TAlphaColors.Null;
    // property TitleFont           : TDPFFont read FTitleFont write SetTitleFont;
    property Buttons: TDPFActionSheetButtonsCollection read FButtons write SetButtons;
    // property PopupComponent: TComponent read FPopupComponent write FPopupComponent;
  end;

  // ------------------------------------------------------------------------------

implementation

uses
  Math;

// ------------------------------------------------------------------------------
{ TDPFUIActionSheet }
// ------------------------------------------------------------------------------
constructor TDPFUIActionSheet.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  FDPFActionSheetDelegate                  := TDPFActionSheetDelegate.Create( Self );
  FDPFActionSheetGestureRecognizerDelegate := TDPFActionSheetGestureRecognizerDelegate.Create( Self );
  FUITapGestureRecognizer                  := TUITapGestureRecognizer.Wrap( TUITapGestureRecognizer.Alloc.initWithTarget( FDPFActionSheetGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'handleSingleTap:' ) ) );
{$ENDIF}
  FButtons := TDPFActionSheetButtonsCollection.Create( Self );
  // FTitleFont := TDPFFont.create;
end;

// ------------------------------------------------------------------------------
destructor TDPFUIActionSheet.Destroy;
begin
{$IFDEF IOS}
  DisposeOfAndNil( FDPFActionSheetDelegate );
  DisposeOfAndNil( FDPFActionSheetGestureRecognizerDelegate );
  FUITapGestureRecognizer.release;
  FUITapGestureRecognizer := nil;
{$ENDIF}
  DisposeOfAndNil( FButtons );
  // FTitleFont.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.DoClick( ButtonIndex: Integer );
begin
  if Assigned( FOnClick ) then
    FOnClick( Self, ButtonIndex );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.DoDismiss( ButtonIndex: Integer );
begin
  if Assigned( FOnDismiss ) then
    FOnDismiss( Self, ButtonIndex );

{$IFDEF IOS}
  if FMessageView <> nil then
  begin
    objc_msgSend( ( FMessageView as ILocalObject ).GetObjectID, sel_getUid( 'autorelease' ) ); // add it to the autorelease pool, so it will be freed when it is safe
    FMessageView := nil;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUIActionSheet.Execute: TModalResult;
begin
  Result := Execute( nil );
end;

// ------------------------------------------------------------------------------
// Modified for iPad by Graham
function TDPFUIActionSheet.Execute( APopoverControl: TControl ): TModalResult;
{$IFDEF IOS}
var
  RunLoop           : NSRunLoop;
  DistantFuture     : NSDate;
  DefaultRunLoopMode: NSString;
{$ENDIF}
begin
  FPopoverControl := APopoverControl;
{$IFDEF IOS}
  FDPFActionSheetDelegate.FModal := True;
  ShowMessage;

  RunLoop            := TNSRunLoop.Wrap( TNSRunLoop.OCClass.currentRunLoop );
  DistantFuture      := TNSDate.Wrap( TNSDate.OCClass.distantFuture );
  DefaultRunLoopMode := NSDefaultRunLoopMode;
  while FDPFActionSheetDelegate.Modal and RunLoop.runMode( DefaultRunLoopMode, DistantFuture ) do;

  Result := FDPFActionSheetDelegate.ModalResult;
{$ELSE}
  Result := mrNone;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUIActionSheet.GetButtonItem( Title: string ): TDPFActionSheetButtonItem;
var
  I: Integer;
begin
  Result := nil;
  for I  := 0 to Buttons.Count - 1 do
    if AnsiSameText( Buttons[I].Title, Title ) then
    begin
      Result := Buttons[I];
      break;
    end;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUIActionSheet.SetBkColor( FUIButton: UIButton; Color: TAlphaColor );
begin
  if Assigned( FUIButton ) then
  begin
    if Color <> TAlphaColors.Null then
      FUIButton.setBackgroundColor( TColorToUIColor( Color ) )
    else
      FUIButton.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetTitleColor( FUIButton: UIButton; Color: TAlphaColor; ColorState: Integer );
begin
  if Assigned( FUIButton ) then
  begin
    if Color <> TAlphaColors.Null then
      FUIButton.setTitleColor( TColorToUIColor( Color ), ColorState )
    else
      FUIButton.setTitleColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ), ColorState );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetLabelBackColor( FUIlabel: UILabel; Color: TAlphaColor );
begin
  if Assigned( FUIlabel ) then
  begin
    if Color <> TAlphaColors.Null then
      FUIlabel.setBackgroundColor( TColorToUIColor( Color ) )
    else
      FUIlabel.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetLabelTextColor( FUILabel: UILabel; Color: TAlphaColor );
begin
  if Assigned( FUILabel ) then
  begin
    if Color <> TAlphaColors.Null then
      FUILabel.setTextColor( TColorToUIColor( Color ) )
    else
      FUILabel.setTextColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetBkImage( FUIButton: UIButton; ImageName: string; ImageState: Integer );
var
  Image: UIImage;
begin
  if Assigned( FUIButton ) then
  begin
    if ImageName <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( ImageName ) ) );
      FUIButton.setBackgroundImage( Image, ImageState );
    end
    else if Assigned( FUIButton.imageView ) then
      FUIButton.setBackgroundImage( nil, ImageState );
  end

end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetFgImage( FUIButton: UIButton; ImageName: string; ImageState: Integer );
var
  Image: UIImage;
begin
  if Assigned( FUIButton ) then
  begin
    if ImageName <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( ImageName ) ) );
      FUIButton.setImage( Image, ImageState );
      // Image.release;
      Image := nil;
    end
    else if ( FUIButton.imageView <> nil ) then
      FUIButton.setImage( nil, ImageState );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.SetButtons( const Value: TDPFActionSheetButtonsCollection );
begin
  FButtons.Assign( Value );
end;

// ------------------------------------------------------------------------------
(* procedure TDPFUIActionSheet.SetTitleFont( const Value: TDPFFont );
  begin
  FTitleFont.Assign( Value );
  end; *)

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.ShowMessage( AMessageTitle: string; const AButtons: array of string );
var
  I: Integer;
begin
  Buttons.Clear;
  for I               := 0 to high( AButtons ) do
    Buttons.Add.Title := AButtons[i];
  Title               := AMessageTitle;
  ShowMessage;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIActionSheet.ShowMessage;
{$IFDEF IOS}
var
  I, idx: Integer;
  p     : Pointer;
  LView : UIView;
  ARect : NSRect;
{$ENDIF}
begin
{$IFDEF IOS}
  // this function is not re-entry safe - have to kill the previous message
  if FMessageView <> nil then
  begin
    FMessageView.dismissWithClickedButtonIndex( FMessageView.cancelButtonIndex, False );
    FMessageView := nil;
  end;

  FMessageView := TUIActionSheet.Wrap( TUIActionSheet.Alloc.initWithTitle( NSStr( '' ), nil, nil, nil, nil ) );
  FMessageView.setDelegate( FDPFActionSheetDelegate.GetObjectID );
  FMessageView.setActionSheetStyle( UIActionSheetStyleDefault );

  for I := 0 to Buttons.Count - 1 do
  begin
    idx := FMessageView.addButtonWithTitle( NSStr( Buttons[I].Title ) );
    if Buttons[I].ButtonKind = bkCancel then
      FMessageView.setCancelButtonIndex( idx )
    else if Buttons[I].ButtonKind = bkDestructive then
      FMessageView.setDestructiveButtonIndex( idx );

  end;

  if Buttons.Count = 0 then
  begin
    FUITapGestureRecognizer.setNumberOfTapsRequired( 1 );
    FUITapGestureRecognizer.setNumberOfTouchesRequired( 1 );
    FUITapGestureRecognizer.setDelaysTouchesBegan( true );
    FUITapGestureRecognizer.setDelaysTouchesEnded( false );
    FMessageView.addGestureRecognizer( FUITapGestureRecognizer );
  end;

  FMessageView.setTitle( NSSTR( Title ) );

  LView := nil;

  if Assigned( GetSharedApplication.keyWindow.rootViewController ) then
    LView := GetSharedApplication.keyWindow.rootViewController.view;

  if LView = nil then
  begin
    p := GetSharedApplication.keyWindow.subviews.objectAtIndex( 0 );
    if Assigned( p ) then
      LView := TUIView.Wrap( p );
  end;
  if ( FPopoverControl <> nil ) and ( IsIPad ) then
  begin

    ARect.origin.x    := FPopoverControl.AbsoluteRect.Left;
    ARect.origin.y    := FPopoverControl.AbsoluteRect.Top + GetSharedApplication.statusBarFrame.size.height;
    ARect.size.width  := FPopoverControl.Width;
    ARect.size.height := FPopoverControl.Height;
    FMessageView.showFromRect( ARect, LView, True );
  end
  else
    FMessageView.showInView( LView );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFActionSheetDelegate }
{$IFDEF IOS}

procedure TDPFActionSheetDelegate.actionSheet( actionSheet: UIActionSheet; clickedButtonAtIndex: NSInteger );
begin
  FDPFUIActionSheet.DoClick( clickedButtonAtIndex );
end;

procedure TDPFActionSheetDelegate.actionSheetDidDismissWithButtonIndex( actionSheet: UIActionSheet; didDismissWithButtonIndex: NSInteger );
begin
  if InRange( didDismissWithButtonIndex, 0, FDPFUIActionSheet.Buttons.Count - 1 ) then
  begin
    FModalResult := FDPFUIActionSheet.Buttons[didDismissWithButtonIndex].ModalResult;
    if FModalResult = 0 then
      FModalResult := didDismissWithButtonIndex + 100;
  end;

  FDPFUIActionSheet.DoDismiss( didDismissWithButtonIndex );
  FModal := False;
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetDelegate.actionSheetCancel( actionSheet: UIActionSheet );
begin
  FModalResult := mrCancel;
  FModal       := False;
end;

// ------------------------------------------------------------------------------
constructor TDPFActionSheetDelegate.Create( ADPFUIActionSheet: TDPFUIActionSheet );
begin
  inherited create;
  FDPFUIActionSheet := ADPFUIActionSheet;
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetDelegate.didPresentActionSheet( actionSheet: UIActionSheet );
begin
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetDelegate.willPresentActionSheet( actionSheet: UIActionSheet );
var
  I         : Integer;
  b         : UIButton;
  l         : UILabel;
  O         : NSObject;
  ButtonItem: TDPFActionSheetButtonItem;
begin
  FModalResult := mrNone;

  for I := 0 to FDPFUIActionSheet.FMessageView.subviews.count - 1 do
  begin
    O := TNSObject.Wrap( FDPFUIActionSheet.FMessageView.subviews.objectAtIndex( i ) );
    if O.isKindOfClass( objc_getClass( 'UIButton' ) ) then
    begin
      b          := TUIButton.Wrap( FDPFUIActionSheet.FMessageView.subviews.objectAtIndex( i ) );
      ButtonItem := nil;
      if assigned( b ) and assigned( b.titleLabel ) and assigned( b.titleLabel.text ) then
        ButtonItem := FDPFUIActionSheet.GetButtonItem( UTF8ToString( b.titleLabel.text.UTF8String ) );

      if Assigned( ButtonItem ) and ( ButtonItem.ButtonKind = TDPFActionSheetButtonKind.bkCustom ) then
      begin
        b.setFont( ButtonItem.Font._UIFont );

        FDPFUIActionSheet.SetTitleColor( b, ButtonItem.TitleColorNormal, UIControlStateNormal );
        FDPFUIActionSheet.SetTitleColor( b, ButtonItem.TitleColorHighlight, UIControlStateHighlighted );
        FDPFUIActionSheet.SetTitleColor( b, ButtonItem.TitleColorDisable, UIControlStateDisabled );

        FDPFUIActionSheet.SetBkColor( b, ButtonItem.BackgroundColor );

        FDPFUIActionSheet.SetBkImage( b, ButtonItem.BackgroundImageNormal, UIControlStateNormal );
        FDPFUIActionSheet.SetBkImage( b, ButtonItem.BackgroundImageHighlight, UIControlStateHighlighted );
        FDPFUIActionSheet.SetBkImage( b, ButtonItem.BackgroundImageDisable, UIControlStateDisabled );

        FDPFUIActionSheet.SetFgImage( b, ButtonItem.ImageNormal, UIControlStateNormal );
        FDPFUIActionSheet.SetFgImage( b, ButtonItem.ImageHighlight, UIControlStateHighlighted );
        FDPFUIActionSheet.SetFgImage( b, ButtonItem.ImageDisable, UIControlStateDisabled );

        b.layer.setCornerRadius( ButtonItem.CornerRadius );
        b.layer.setBorderColor( TColorToUIColor( ButtonItem.BorderColor ).CGColor );
        b.layer.setBorderWidth( ButtonItem.BorderWidth );
      end;
      b.setShowsTouchWhenHighlighted( True );
    end
    else if O.isKindOfClass( objc_getClass( 'UILabel' ) ) then
    begin
      l := TUILabel.Wrap( FDPFUIActionSheet.FMessageView.subviews.objectAtIndex( i ) );
      // l.setFont( FDPFUIActionSheet.TitleFont._UIFont );

      l.setNumberOfLines( 0 );

      FDPFUIActionSheet.SetLabelTextColor( l, FDPFUIActionSheet.TitleColor );
      FDPFUIActionSheet.SetLabelBackColor( l, FDPFUIActionSheet.TitleBackGroundColor );
    end;

  end;
end;
// ------------------------------------------------------------------------------
{ TDPFActionSheetGestureRecognizerDelegate }

constructor TDPFActionSheetGestureRecognizerDelegate.Create( ADPFUIActionSheet: TDPFUIActionSheet );
begin
  inherited create;
  FDPFUIActionSheet := ADPFUIActionSheet;
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetGestureRecognizerDelegate.handleSingleTap( gr: UITapGestureRecognizer );
begin
  FDPFUIActionSheet.FMessageView.removeGestureRecognizer( gr );
  FDPFUIActionSheet.FMessageView.dismissWithClickedButtonIndex( 0, true );
  FDPFUIActionSheet.FMessageView.release;
  FDPFUIActionSheet.FMessageView := nil;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFActionSheetButtonItem }

constructor TDPFActionSheetButtonItem.Create( AOwner: TCollection );
begin
  inherited Create( AOwner );
  FOwner              := AOwner;
  FBorderWidth        := 0;
  FBorderColor        := TAlphaColors.Null;
  FCornerRadius       := 0;
  FFont               := TDPFFont.create;
  TitleColorNormal    := TAlphaColors.Black;
  TitleColorHighlight := TAlphaColors.White;
  TitleColorDisable   := TAlphaColors.Gray;
  FButtonKind         := TDPFActionSheetButtonKind.bkOther;
end;

// ------------------------------------------------------------------------------
destructor TDPFActionSheetButtonItem.Destroy;
begin
  FFont.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFActionSheetButtonItem.GetDisplayName: string;
begin
  Result := Format( 'Button %d', [index] );
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetButtonItem.SetFont( const Value: TDPFFont );
begin
  FFont.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetButtonItem.SetModalResult( const Value: TModalResult );
begin
  FModalResult := Value;
end;

// ------------------------------------------------------------------------------
{ TDPFActionSheetButtonsCollection }

function TDPFActionSheetButtonsCollection.Add: TDPFActionSheetButtonItem;
begin
  Result := inherited Add as TDPFActionSheetButtonItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFActionSheetButtonsCollection.Create( AOwner: TComponent );
begin
  inherited Create( TDPFActionSheetButtonItem );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TDPFActionSheetButtonsCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFActionSheetButtonsCollection.GetItem( Index: Integer ): TDPFActionSheetButtonItem;
begin
  Result := inherited Items[index] as TDPFActionSheetButtonItem;
end;

// ------------------------------------------------------------------------------
function TDPFActionSheetButtonsCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------------------------------------------------------------------
function TDPFActionSheetButtonsCollection.Insert( Index: Integer ): TDPFActionSheetButtonItem;
begin
  Result := inherited insert( index ) as TDPFActionSheetButtonItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFActionSheetButtonsCollection.SetItem( Index: Integer; Value: TDPFActionSheetButtonItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
end.

