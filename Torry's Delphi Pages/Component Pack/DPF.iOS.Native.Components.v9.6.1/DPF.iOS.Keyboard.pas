// ------------------------------------------------------------------------------
// DPF.iOS.Keyboard Component
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
unit DPF.iOS.Keyboard;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections,
  System.UITypes, System.Types, FMX.Controls,

{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  iOSapi.CoreGraphics,
  FMX.VirtualKeyboard.iOS,
  DPF.iOS.Common,
{$ENDIF}
  DPF.iOS.BaseControl,
  FMX.VirtualKeyboard,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

const
  TOOLBAR_HEIGHT = 44;

type

  TDPFKeyboard = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFKeyboardEvents = interface( NSObject )
    ['{76ECC493-3AC2-44D7-8E58-492CB406442B}']

    procedure KeyboardWillShow( notification: Pointer ); cdecl;
    procedure KeyboardWillHide( notification: Pointer ); cdecl;
    procedure keyboardWillChange( notification: Pointer ); cdecl;

    procedure HideVirtualKeyboard; cdecl;
    procedure CustomButtonAction( sender: Pointer ); cdecl;

  end;

  // ------------------------------------------------------------------------------
  TDPFKeyboardEventHandler = class( TOCLocal )
  private

  private
    FDPFKeyboard: TDPFKeyboard;

    { Keyborad Notifications }
    procedure MakeKeyboardRect( Notification: Pointer );
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public

    constructor create( ADPFKeyboard: TDPFKeyboard );

    procedure KeyboardWillShow( notification: Pointer ); cdecl;
    procedure KeyboardWillHide( notification: Pointer ); cdecl;
    procedure keyboardWillChange( notification: Pointer ); cdecl;
    procedure HideVirtualKeyboard; cdecl;

    procedure CustomButtonAction( sender: Pointer ); cdecl;
  end;

{$ENDIF}

  TDPFKeyBoardWillShow   = procedure( Sender: TObject; const Bounds: TRectF ) of object;
  TDPFKeyBoardWillHide   = procedure( Sender: TObject; const Bounds: TRectF ) of object;
  TDPFKeyBoardWillChange = procedure( Sender: TObject ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFKeyboard = class( TComponent )
  private
    FToolbarVisible      : Boolean;
    FButtons             : TList<TVirtualKeyboardToolButton>;
    FUpdatingButtons     : Boolean;
    FToolbarEnabled      : Boolean;
    FHideButtonVisible   : Boolean;
    FHideButtonCaption   : string;
    FOnKeyBoardWillShow  : TDPFKeyBoardWillShow;
    FOnKeyBoardWillHide  : TDPFKeyBoardWillHide;
    FOnKeyBoardWillChange: TDPFKeyBoardWillChange;
{$IFDEF IOS}
    FKeyboardHandler    : TDPFKeyboardEventHandler;
    FToolBar            : UIToolBar;
    FFlexibleSepararator: UIBarButtonItem;
    FHideButton         : UIBarButtonItem;
    FToolbarFrame       : NSRect;
    FKeyboardFrame      : NSRect;
    FKeyboardHide       : Boolean;
    AnimationDuration   : Double;
    AnimationCurve      : NSUInteger;

    procedure SetToolbarFrame( const UseAnimation: Boolean );

    procedure RefreshToolbarButtons;
    procedure CreateToolbar;

{$ENDIF}
    procedure SetHideButtonCaption( const Value: string );
    procedure SetHideButtonVisible( const Value: Boolean );
    procedure SetToolbarVisible( const Value: Boolean );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFDEF IOS}
    function ShowVirtualKeyboard( const ADPFControl: TDPFiOSBaseControl): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetToolbarEnabled( const Value: Boolean );
    function AddButton( const Title: string; ExecuteEvent: TNotifyEvent ): TVirtualKeyboardToolButton;

    procedure DeleteButton( const Index: Integer );
    procedure ClearButtons;

    property KeyboardFrame: NSRect read FKeyboardFrame;
{$ENDIF}
    function GetButtonByIndex( const Index: Integer ): TVirtualKeyboardToolButton;
    function ButtonsCount: Integer;

    function IsToolbarEnabled: Boolean;
  published
    property HideButtonVisible: Boolean read FHideButtonVisible write SetHideButtonVisible default true;
    property HideButtonCaption: string read FHideButtonCaption write SetHideButtonCaption;

    property ToolbarVisible: Boolean read FToolbarVisible write SetToolbarVisible default true;

    property OnKeyBoardWillShow  : TDPFKeyBoardWillShow read FOnKeyBoardWillShow write FOnKeyBoardWillShow;
    property OnKeyBoardWillHide  : TDPFKeyBoardWillHide read FOnKeyBoardWillHide write FOnKeyBoardWillHide;
    property OnKeyBoardWillChange: TDPFKeyBoardWillChange read FOnKeyBoardWillChange write FOnKeyBoardWillChange;

  end;

  // ------------------------------------------------------------------------------
  TDPFVKToolbarButton = class( TVirtualKeyboardToolButton )
  protected
    FDPFKeyboard: TDPFKeyboard;
    procedure DoChanged; override;
  end;

implementation

{$IFDEF IOS}

function GetActiveView( View: UIView ): UIView;
var
  i: Integer;
  v: UIView;
begin
  result := nil;
  for i  := 0 to view.subviews.count - 1 do
  begin
    v := TUIView.Wrap( view.subviews.objectAtIndex( i ) );
    if v.isFirstResponder then
    begin
      result := TUIView.Wrap( view.subviews.objectAtIndex( i ) );
      break;
    end
    else
      result := GetActiveView( v );
  end;

end;

// ------------------------------------------------------------------------------
constructor TDPFKeyboardEventHandler.create( ADPFKeyboard: TDPFKeyboard );
begin
  inherited create;
  FDPFKeyboard := ADPFKeyboard;
end;

function TDPFKeyboardEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IDPFKeyboardEvents );
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.CustomButtonAction( sender: Pointer );
var
  Index: Integer;
begin
  index := TUIBarButtonItem.Wrap( sender ).tag - 1;
  if index >= 0 then
    TDPFVKToolbarButton( FDPFKeyboard.FButtons[index] ).DoExecute;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.MakeKeyboardRect( Notification: Pointer );
var
  StatusBar  : integer;
begin
  FDPFKeyboard.AnimationDuration := 0.25;
  if Notification <> nil then
  begin
    FDPFKeyboard.AnimationDuration := TNSNumber.Wrap( TNSNotification.Wrap( Notification ).userInfo.valueForKey( NSSTR( 'UIKeyboardAnimationDurationUserInfoKey' ) ) ).doubleValue;
    FDPFKeyboard.AnimationCurve    := TNSNumber.Wrap( TNSNotification.Wrap( Notification ).userInfo.valueForKey( NSSTR( 'UIKeyboardAnimationCurveUserInfoKey' ) ) ).intValue;
    FDPFKeyboard.FKeyboardFrame    := iOSapi.UIKit.TNSValue.Wrap( TNSNotification.Wrap( Notification ).userInfo.valueForKey( NSSTR( 'UIKeyboardFrameEndUserInfoKey' ) ) ).CGRectValue;
    FDPFKeyboard.FKeyboardFrame    := GetSharedApplication.keyWindow.rootViewController.view.convertRect( FDPFKeyboard.FKeyboardFrame, nil );
  end;

  StatusBar := 0;
  if TOSVersion.Major < 0 then
    if not GetSharedApplication.isStatusBarHidden then
      StatusBar := 20;

  FDPFKeyboard.FToolbarFrame             := FDPFKeyboard.FKeyboardFrame;
  FDPFKeyboard.FToolbarFrame.size.height := TOOLBAR_HEIGHT;
  FDPFKeyboard.FToolbarFrame.origin.y    := FDPFKeyboard.FToolbarFrame.origin.y - TOOLBAR_HEIGHT - StatusBar;

  if FDPFKeyboard.FKeyboardHide then
    FDPFKeyboard.FToolbarFrame.origin.y := FDPFKeyboard.FToolbarFrame.origin.y * 5;

end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.HideVirtualKeyboard;
begin
  FDPFKeyboard.HideVirtualKeyboard;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.KeyboardWillHide( notification: Pointer );
var
  Bounds: TRectF;
  NR    : NSRect;
begin
  FDPFKeyboard.FKeyboardHide := true;
  MakeKeyboardRect( notification );
  FDPFKeyboard.SetToolbarFrame( true );

  if FDPFKeyboard.FToolbarVisible then
    NR := CGRectUnion( FDPFKeyboard.FKeyboardFrame, FDPFKeyboard.FToolbarFrame )
  else
    NR := FDPFKeyboard.FKeyboardFrame;

  Bounds := TRectF.Create( PointF( NR.origin.x, NR.origin.y ), NR.size.width, NR.size.height );

  if Assigned( FDPFKeyboard.OnKeyBoardWillHide ) then
    FDPFKeyboard.OnKeyBoardWillHide( FDPFKeyboard, Bounds );
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.KeyboardWillShow( notification: Pointer );
var
  Bounds: TRectF;
  NR    : NSRect;

begin
  FDPFKeyboard.FKeyboardHide := false;
  MakeKeyboardRect( notification );
  FDPFKeyboard.CreateToolbar;

  if FDPFKeyboard.FToolbarVisible then
    NR := CGRectUnion( FDPFKeyboard.FKeyboardFrame, FDPFKeyboard.FToolbarFrame )
  else
    NR := FDPFKeyboard.FKeyboardFrame;

  Bounds := TRectF.Create( PointF( NR.origin.x, NR.origin.y ), NR.size.width, NR.size.height );

  if Assigned( FDPFKeyboard.OnKeyBoardWillShow ) then
    FDPFKeyboard.OnKeyBoardWillShow( FDPFKeyboard, Bounds );

end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboardEventHandler.keyboardWillChange( notification: Pointer );
begin
  MakeKeyboardRect( notification );
  FDPFKeyboard.CreateToolbar;

  if Assigned( FDPFKeyboard.OnKeyBoardWillChange ) then
    FDPFKeyboard.OnKeyBoardWillChange( FDPFKeyboard );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFKeyboard }
function TDPFKeyboard.ButtonsCount: Integer;
begin
  Result := FButtons.Count;
end;

// ------------------------------------------------------------------------------
constructor TDPFKeyboard.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FHideButtonCaption := 'Done';
  FHideButtonVisible := true;
  FToolbarVisible    := true;
  FToolbarEnabled    := true;

  FUpdatingButtons := False;
  FButtons         := TList<TVirtualKeyboardToolButton>.Create;

{$IFDEF IOS}
  UnregisterVirtualKeyboardServices;

  AnimationDuration := 0.25;
  FKeyboardHandler  := TDPFKeyboardEventHandler.Create( self );
  // Subscribing to events
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FKeyboardHandler.GetObjectID, sel_getUid( 'KeyboardWillShow:' ), ( NSSTR( 'UIKeyboardWillShowNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FKeyboardHandler.GetObjectID, sel_getUid( 'KeyboardWillHide:' ), ( NSSTR( 'UIKeyboardWillHideNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FKeyboardHandler.GetObjectID, sel_getUid( 'keyboardWillChange:' ), ( NSSTR( 'UIKeyboardWillChangeFrameNotification' ) as ILocalObject ).GetObjectID, nil );

  // FToolbarEnabled := ( TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).userInterfaceIdiom = UIUserInterfaceIdiomPhone );
  // FHideButtonVisible := ( TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).userInterfaceIdiom = UIUserInterfaceIdiomPhone );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFKeyboard.Destroy;
begin

  FUpdatingButtons := True;
  FreeAndNil( FButtons );

{$IFDEF IOS}
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FKeyboardHandler.GetObjectID );
  FreeAndNil( FKeyboardHandler );

  if Assigned( FToolBar ) then
  begin
    if Assigned( FToolBar.items ) then
      FToolBar.items.release;
    FToolBar.release;
    FToolBar := nil;
  end;
  if Assigned( FFlexibleSepararator ) then
  begin
    FFlexibleSepararator.release;
    FFlexibleSepararator := nil;
  end;
  if Assigned( FHideButton ) then
  begin
    FHideButton.release;
    FHideButton := nil;
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFKeyboard.ClearButtons;
begin
  if FButtons.Count > 0 then
  begin
    FUpdatingButtons := True;
    FButtons.Clear;
    FUpdatingButtons := False;
    RefreshToolbarButtons;
  end;
end;

// ------------------------------------------------------------------------------
function TDPFKeyboard.AddButton( const Title: string; ExecuteEvent: TNotifyEvent ): TVirtualKeyboardToolButton;
begin
  FUpdatingButtons                           := True;
  Result                                     := TDPFVKToolbarButton.Create;
  TDPFVKToolbarButton( result ).FDPFKeyboard := self;
  Result.Title                               := Title;
  Result.OnExecute                           := ExecuteEvent;
  FUpdatingButtons                           := False;
  FButtons.Add( Result );
  RefreshToolbarButtons;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.CreateToolbar;
var
  SharedApplication: UIApplication;
  KeyWindow        : UIWindow;
begin
  SharedApplication := TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication );
  KeyWindow         := SharedApplication.keyWindow;
  if Assigned( KeyWindow ) and Assigned( KeyWindow.rootViewController ) then
  begin
    if not Assigned( FToolBar ) and FToolbarEnabled then
    begin
      FToolBar := TUIToolbar.Create;
      FToolBar.setBarStyle( UIBarStyleBlackOpaque );
      FToolBar.setAlpha( 0.8 );
      SetToolbarFrame( false );
      RefreshToolbarButtons;
      KeyWindow.rootViewController.view.addSubview( FToolbar );
    end
    else
    begin
      SetToolbarFrame( true );
      KeyWindow.rootViewController.view.bringSubviewToFront( FToolbar );
    end
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.RefreshToolbarButtons;
var
  Buttons: NSMutableArray;
  I      : Integer;
  B      : UIBarButtonItem;
begin
  if not FUpdatingButtons and Assigned( FToolBar ) then
  begin
    if Assigned( FToolBar.items ) then
    begin
      FToolBar.items.release;
      FFlexibleSepararator := nil;
      FHideButton          := nil;
    end;

    Buttons := TNSMutableArray.Create;

    // Custom buttons
    for I := 0 to FButtons.Count - 1 do
    begin
      B := TUIBarButtonItem.Create;
      B.setTitle( NSSTR( FButtons[I].Title ) );
      B.setStyle( UIBarButtonItemStyleBordered );
      B.setTag( I + 1 );
      B.setTarget( FKeyboardHandler.GetObjectID );
      B.setAction( sel_getUid( 'CustomButtonAction:' ) );
      B.setEnabled( False );
      Buttons.addObject( ILocalObject( B ).GetObjectID );
    end;

    if FHideButtonVisible then
    begin
      // Separator
      if not Assigned( FFlexibleSepararator ) then
      begin
        FFlexibleSepararator := TUIBarButtonItem.Create;
        FFlexibleSepararator.initWithBarButtonSystemItem( UIBarButtonSystemItemFlexibleSpace, nil, nil );
      end;
      Buttons.addObject( ILocalObject( FFlexibleSepararator ).GetObjectID );

      // Hide button
      if not Assigned( FHideButton ) then
      begin
        FHideButton := TUIBarButtonItem.Create;
        FHideButton.setTitle( NSSTR( FHideButtonCaption ) );
        FHideButton.setStyle( UIBarButtonItemStyleBordered );
        if TOSVersion.Major > 6 then
          FHideButton.setTintColor( TColorToUIColor( TAlphaColors.White ) );

        FHideButton.setTarget( FKeyboardHandler.GetObjectID );
        FHideButton.setAction( sel_getUid( 'HideVirtualKeyboard' ) );
      end;
      Buttons.addObject( ILocalObject( FHideButton ).GetObjectID );
    end;

    FToolBar.setItems( Buttons );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.SetToolbarFrame( const UseAnimation: Boolean );
begin
  if Assigned( FToolBar ) and ToolbarVisible then
    if UseAnimation then
    begin
      TUIView.OCClass.beginAnimations( nil, nil );
      try
        TUIView.OCClass.setAnimationCurve( AnimationCurve );
        TUIView.OCClass.setAnimationDuration( AnimationDuration );
        TUIView.OCClass.setAnimationBeginsFromCurrentState( True );
        FToolBar.setFrame( FToolbarFrame );
      finally
        TUIView.OCClass.commitAnimations;
      end;
    end
    else
      FToolBar.setFrame( FToolbarFrame );
end;

// ------------------------------------------------------------------------------
function TDPFKeyboard.ShowVirtualKeyboard( const ADPFControl: TDPFiOSBaseControl): Boolean;
begin
  ADPFControl.SetFocus;
  GetSharedApplication.keyWindow.rootViewController.view.becomeFirstResponder;
  Result := True;
end;

// ------------------------------------------------------------------------------
function TDPFKeyboard.HideVirtualKeyboard: Boolean;
begin
  GetSharedApplication.keyWindow.rootViewController.view.endEditing( true );
  Result := True;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.DeleteButton( const Index: Integer );
begin
  if ( index >= 0 ) and ( index < FButtons.Count ) then
  begin
    FButtons.Delete( index );
    RefreshToolbarButtons;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.SetToolbarEnabled( const Value: Boolean );
begin
  if FToolbarEnabled <> Value then
  begin
    if not Value then
      ToolbarVisible := False;
    FToolbarEnabled  := Value;
  end;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFKeyboard.GetButtonByIndex( const Index: Integer ): TVirtualKeyboardToolButton;
begin
  if ( index >= 0 ) and ( index < FButtons.Count ) then
    Result := FButtons[index]
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
function TDPFKeyboard.IsToolbarEnabled: Boolean;
begin
  Result := FToolbarEnabled;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.SetToolbarVisible( const Value: Boolean );
begin
  if FToolbarVisible <> Value then
  begin
    FToolbarVisible := Value;
{$IFDEF IOS}
    if Assigned( FToolBar ) and FToolbarEnabled then
      SetToolbarFrame( True );
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.SetHideButtonCaption( const Value: string );
begin
  FHideButtonCaption := Value;
{$IFDEF IOS}
  RefreshToolbarButtons;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFKeyboard.SetHideButtonVisible( const Value: Boolean );
begin
  FHideButtonVisible := Value;
{$IFDEF IOS}
  RefreshToolbarButtons;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFVKToolbarButton }

procedure TDPFVKToolbarButton.DoChanged;
begin
  inherited;
{$IFDEF IOS}
  FDPFKeyboard.RefreshToolbarButtons;
{$ENDIF}
end;

end.
