// ------------------------------------------------------------------------------
// DPF.iOS.UINavigationController Component
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
unit DPF.iOS.UINavigationController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,
  System.Generics.Collections,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  iOSapi.QuartzCore,
  FMX.Platform.iOS,
  DPF.iOS.Common,
  DPF.iOS.Classes,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  System.Math,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  FMX.Forms,
  FMX.Dialogs;

type
{$M+}
  TDPFNavigationController     = class;
  TDPFNavigationControllerPage = class;

  // ----------------------------------------------------------------------------
  TDPFNavigationNavBar = class( TDPFiOSBaseControl )
{$IFNDEF IOS}
  private
  public
    procedure Paint; override;
  published
    property Visible default true;
  end
{$ENDIF}

    ;

  TDPFNavigationToolBar = class( TDPFiOSBaseControl )
{$IFNDEF IOS}
  private
  public
    procedure Paint; override;
  published
    property Visible default false;
  end
{$ENDIF}

    ;

{$IFDEF IOS}
  TDPFNavigationBarButtonDelegate = class;
{$ENDIF}
  TDPFBarButtonType = ( bkLeft, bkRight, bkBack );
  TDPFBarType       = ( btNavBar, btToolBar );
{$IFDEF IOS}
  TDPFModalTransitionStyle = ( mtsCoverVertical = 0, mtsFlipHorizontal = 1, mtsCrossDissolve = 2, mtsPartialCurl = 3 );

  // ----------------------------------------------------------------------------
  UINavigationBarClass = interface( UIViewClass )
    ['{20F13F2F-1EED-4BB6-8F2F-66FCEE46021B}']
  end;

  UINavigationBar = interface( UIView )
    ['{9B854421-8894-46FB-9FCA-1D4738CB34AB}']
    function backItem: UINavigationItem; cdecl;
    function backgroundImageForBarMetrics( barMetrics: UIBarMetrics ): UIImage; cdecl;
    function barStyle: UIBarStyle; cdecl;
    function delegate: Pointer; cdecl;
    function isTranslucent: Boolean; cdecl;
    function items: NSArray; cdecl;
    function popNavigationItemAnimated( animated: Boolean ): UINavigationItem; cdecl;
    procedure pushNavigationItem( item: UINavigationItem; animated: Boolean ); cdecl;
    procedure setBackgroundImage( backgroundImage: UIImage; forBarMetrics: UIBarMetrics ); cdecl;
    procedure setBarStyle( barStyle: UIBarStyle ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setItems( items: NSArray ); cdecl; overload;
    procedure setItems( items: NSArray; animated: Boolean ); cdecl; overload;
    procedure setTintColor( tintColor: UIColor ); cdecl;
    procedure setTitleTextAttributes( titleTextAttributes: NSDictionary ); cdecl;
    procedure setTitleVerticalPositionAdjustment( adjustment: Single; forBarMetrics: UIBarMetrics ); cdecl;
    procedure setTranslucent( translucent: Boolean ); cdecl;
    function tintColor: UIColor; cdecl;
    function titleTextAttributes: NSDictionary; cdecl;
    function titleVerticalPositionAdjustmentForBarMetrics( barMetrics: UIBarMetrics ): Single; cdecl;
    function topItem: UINavigationItem; cdecl;

    function barTintColor: UIColor; cdecl; // Available in iOS 7.0 and later.
    procedure setBarTintColor( tintColor: UIColor ); cdecl; // Available in iOS 7.0 and later.
  end;

  TUINavigationBar = class( TOCGenericImport<UINavigationBarClass, UINavigationBar> )
  end;

  // ----------------------------------------------------------------------------
  UINavigationControllerClass = interface( UIViewControllerClass )
    ['{CF07C65B-0A72-470A-8A1D-795A69F2D01C}']
  end;

  UINavigationController = interface( UIViewController )
    ['{C33583FE-F338-44B3-8511-450CDC24024E}']
    function delegate: Pointer; cdecl;
    function initWithRootViewController( rootViewController: UIViewController ): Pointer; cdecl;
    function isNavigationBarHidden: Boolean; cdecl;
    function isToolbarHidden: Boolean; cdecl;
    function navigationBar: UINavigationBar; cdecl;
    function popToRootViewControllerAnimated( animated: Boolean ): NSArray; cdecl;
    function popToViewController( viewController: UIViewController; animated: Boolean ): NSArray; cdecl;
    function popViewControllerAnimated( animated: Boolean ): UIViewController; cdecl;
    procedure pushViewController( viewController: UIViewController; animated: Boolean ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setNavigationBarHidden( navigationBarHidden: Boolean ); cdecl; overload;
    procedure setNavigationBarHidden( hidden: Boolean; animated: Boolean ); cdecl; overload;
    procedure setToolbarHidden( toolbarHidden: Boolean ); cdecl; overload;
    procedure setToolbarHidden( hidden: Boolean; animated: Boolean ); cdecl; overload;
    procedure setViewControllers( viewControllers: NSArray ); cdecl; overload;
    procedure setViewControllers( viewControllers: NSArray; animated: Boolean ); cdecl; overload;
    function toolbar: UIToolbar; cdecl;
    function topViewController: UIViewController; cdecl;
    function viewControllers: NSArray; cdecl;
    function visibleViewController: UIViewController; cdecl;
  end;

  TUINavigationController = class( TOCGenericImport<UINavigationControllerClass, UINavigationController> )
  end;

  // ----------------------------------------------------------------------------
  UINavigationControllerDelegate = interface( IObjectiveC )
    ['{8696798C-7A40-4429-88F0-8FFD27501775}']
    procedure navigationController( navigationController: UINavigationController; didShowViewController: UIViewController; animated: Boolean ); cdecl; overload;
    // procedure navigationController( navigationController: UINavigationController; willShowViewController: UIViewController; animated: Boolean ); cdecl; overload;
  end;

  TUINavigationControllerDelegate = class( TOCLocal, UINavigationControllerDelegate )
  private
    FDPFNavigationController: TDPFNavigationController;
  public
    constructor Create( ADPFNavigationController: TDPFNavigationController );
    procedure navigationController( navigationController: UINavigationController; didShowViewController: UIViewController; animated: Boolean ); overload; cdecl;
    // procedure navigationController( navigationController: UINavigationController; willShowViewController: UIViewController; animated: Boolean ); cdecl; overload;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  // TDPFCustomBarButtonItem
  TDPFCustomBarButtonItem = class( TCollectionItem )
  private
{$IFDEF IOS}
    FButtonDelegate: TDPFNavigationBarButtonDelegate;
    FDPFBarItem    : UIBarButtonItem;
{$ENDIF}
    FDPFNavigationControllerPage: TDPFNavigationControllerPage;
    FButtonItemKind             : TDPFBarButtonKind;
    FCustomView                 : TDPFiOSBaseControl;
    FImage                      : string;
    FButtonColor                : TAlphaColor;
    FButtonItemStyle            : TDPFBarButtonItemStyle;
    FButtonTitle                : string;
    FOnClick                    : TNotifyEvent;
    FEnabled                    : Boolean;
    FWidth                      : single;
    FTagNative                  : NativeInt;
    procedure SetButtonColor( const Value: TAlphaColor );
    procedure SetButtonTitle( const Value: string );
    procedure SetEnabled( const Value: Boolean );
    procedure SetWidth( const Value: single );
    procedure SetTagNative( const Value: NativeInt );
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
    property Page: TDPFNavigationControllerPage read FDPFNavigationControllerPage;
  published
    property ButtonTitle    : string read FButtonTitle write SetButtonTitle;
    property Image          : string read FImage write FImage;
    property CustomView     : TDPFiOSBaseControl read FCustomView write FCustomView;
    property ButtonItemStyle: TDPFBarButtonItemStyle read FButtonItemStyle write FButtonItemStyle default bbisPlain;
    property ButtonItemKind : TDPFBarButtonKind read FButtonItemKind write FButtonItemKind default bkSystem;
    property ButtonColor    : TAlphaColor read FButtonColor write SetButtonColor default TAlphaColors.Gray;
    property OnClick        : TNotifyEvent read FOnClick write FOnClick;
    property Enabled        : boolean read FEnabled write SetEnabled;
    property Width          : single read FWidth write SetWidth;
    property TagNative      : NativeInt read FTagNative write SetTagNative;

  end;

  TDPFNavigationBarButtonItem = class( TDPFCustomBarButtonItem )
  private
    FButtonType      : TDPFBarButtonType;
    FButtonSystemItem: TDPFBarButtonSystemItem;
    procedure SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
  protected
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
    procedure AddToNavigation;
{$IFDEF IOS}
    function GetUIBarButtonItem: UIBarButtonItem;
{$ENDIF}
  published
    property ButtonType      : TDPFBarButtonType read FButtonType write FButtonType default TDPFBarButtonType.bkLeft;
    property ButtonSystemItem: TDPFBarButtonSystemItem read FButtonSystemItem write SetButtonSystemItem default bbsiDone;

    property ButtonTitle;
    property Image;
    property CustomView;
    property ButtonItemStyle;
    property ButtonItemKind;
    property ButtonColor;
    property OnClick;
    property Enabled;

  end;

  TDPFNavigationBarButtonCollection = class( TCollection )
  private
    FDPFNavigationControllerPage: TDPFNavigationControllerPage;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFNavigationBarButtonItem;
    procedure SetItem( Index: Integer; Value: TDPFNavigationBarButtonItem );
    procedure Update( Item: TCollectionItem ); override;

  public
    constructor Create( ADPFNavigationControllerPage: TComponent );
    destructor Destroy; override;

    function Add: TDPFNavigationBarButtonItem;
    function Insert( Index: Integer ): TDPFNavigationBarButtonItem;

    property Items[index: Integer]: TDPFNavigationBarButtonItem read GetItem write SetItem; default;
  end;

  // ----------------------------------------------------------------------------
  TDPFToolBarButtonItem = class( TDPFCustomBarButtonItem )
  private
    FButtonSystemItem: TDPFBarButtonSystemItem;
    procedure SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
  protected
  public
    constructor Create( AOwner: TCollection ); override;
  published
    property ButtonSystemItem: TDPFBarButtonSystemItem read FButtonSystemItem write SetButtonSystemItem default bbsiDone;

    property ButtonTitle;
    property Image;
    property CustomView;
    property ButtonItemStyle;
    property ButtonItemKind;
    property ButtonColor;
    property OnClick;
    property Enabled;
  end;

  // ----------------------------------------------------------------------------
  TDPFToolBarButtonCollection = class( TCollection )
  private
    FDPFNavigationControllerPage: TDPFNavigationControllerPage;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFToolBarButtonItem;
    procedure SetItem( Index: Integer; Value: TDPFToolBarButtonItem );
    procedure Update( Item: TCollectionItem ); override;

  public
    constructor Create( ADPFNavigationControllerPage: TComponent );
    destructor Destroy; override;

    function Add: TDPFToolBarButtonItem;
    function Insert( Index: Integer ): TDPFToolBarButtonItem;

    property Items[index: Integer]: TDPFToolBarButtonItem read GetItem write SetItem; default;
  end;

  // ----------------------------------------------------------------------------
  // TDPFNavigationControllerPage
  TDPFNavigationControllerPage = class( TDPFUIViewController )
  private
    FDPFNavigationController: TDPFNavigationController;
    FPageViewTitle          : string;
    FPagePrompt             : string;
    FPageControl            : TDPFNavigationController;
    FBarButtons             : TDPFNavigationBarButtonCollection;
    FPageIndex              : Integer;
    FToolBarButtons         : TDPFToolBarButtonCollection;
    FHidesBackButton        : boolean;

    procedure SetPageIndex( const Value: Integer );
    function GetPageIndex: Integer;
    procedure SetPageControl( const APageControl: TDPFNavigationController );

    procedure SetBarButtons( const Value: TDPFNavigationBarButtonCollection );
    procedure SetToolBarButtons( const Value: TDPFToolBarButtonCollection );
    procedure SetPageViewTitle( const Value: string );
    procedure SetPagePrompt( const Value: string );
    procedure SetHidesBackButton( const Value: boolean );
  protected
  public
    procedure Loaded; override;
{$IFNDEF IOS}
    procedure Paint; override;
{$ELSE}
    function GetUIViewController: UIViewController;
{$ENDIF}
    procedure Resize; override;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    property PageControl: TDPFNavigationController read FPageControl write SetPageControl;
  published
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored true;

    property BarButtons    : TDPFNavigationBarButtonCollection read FBarButtons write SetBarButtons;
    property ToolBarButtons: TDPFToolBarButtonCollection read FToolBarButtons write SetToolBarButtons;

    property PageViewTitle  : string read FPageViewTitle write SetPageViewTitle;
    property PagePrompt     : string read FPagePrompt write SetPagePrompt;
    property HidesBackButton: boolean read FHidesBackButton write SetHidesBackButton default false;

  end;

  TBeforeLoading = procedure of object;
  // ------------------------------------------------------------------------------
  TOnShowViewController = procedure( Sender: TObject; PreviousPage: TDPFNavigationControllerPage ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFNavigationController = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUINavigationController        : UINavigationController;
    FUINavigationControllerDelegate: TUINavigationControllerDelegate;
{$ENDIF}
    FNavBar    : TDPFNavigationNavBar;
    FToolBar   : TDPFNavigationToolBar;
    FPages     : TList<TDPFNavigationControllerPage>;
    FActivePage: TDPFNavigationControllerPage;
    FBarStyle  : TDPFBarStyle;
    FBarColor  : TAlphaColor;
    // FBarVisible          : Boolean;
    // FToolbarVisible      : Boolean;
    FOnShowViewController: TOnShowViewController;
    FBarTranslucent      : Boolean;
    FBarBackgroundImage  : string;
    FBarBackgroundColor  : TAlphaColor;
    FBeforeLoading       : TBeforeLoading;
    procedure SetBarStyle( const Value: TDPFBarStyle );
    procedure SetBarColor( const Value: TAlphaColor );
    procedure SetBarVisible( const Value: Boolean );
    procedure SetBarTranslucent( const Value: Boolean );
    function GetPage( index: Integer ): TDPFNavigationControllerPage;
    function GetPageCount: Integer;
    procedure SetActivePage( const Page: TDPFNavigationControllerPage );
    procedure SetToolbarVisible( const Value: Boolean );
    function GetBarVisible: Boolean;
    function GetToolbarVisible: Boolean;
    procedure SetBarBackgroundImage( const Value: string );
    procedure SetBarBackgroundColor( const Value: TAlphaColor );

  protected
{$IFDEF IOS}
    function CreateNavBarButtonItem( NavBarItem: TDPFNavigationBarButtonItem ): UIBarButtonItem;
    function CreateToolBarButtonItem( ToolbarItem: TDPFToolBarButtonItem ): UIBarButtonItem;
{$ENDIF}
    procedure PresentViewControllerCompleted;
    procedure UpdateUIControlPosition; override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure PushViewController( View: TDPFNavigationControllerPage; Animated: Boolean );
    procedure PopViewController( View: TDPFNavigationControllerPage; Animated: Boolean );
    procedure PopRootViewController( Animated: Boolean );
{$IFDEF IOS}
    procedure PresentViewController( View: TDPFUIViewController; FModalTransitionStyle: TDPFModalTransitionStyle; Animated: Boolean );
    procedure DismissViewController( Animated: Boolean );
    procedure Loaded; override;
    function GetNavigationController: UINavigationController;
{$ELSE}
    procedure Paint; override;
    procedure Change;
{$ENDIF}
    procedure RemovePage( Page: TDPFNavigationControllerPage );
    procedure InsertPage( Page: TDPFNavigationControllerPage );

    property Pages[index: Integer]: TDPFNavigationControllerPage read GetPage;
    property PageCount: Integer read GetPageCount;
  published
    property ActivePage          : TDPFNavigationControllerPage read FActivePage write SetActivePage stored false;
    property BarStyle            : TDPFBarStyle read FBarStyle write SetBarStyle default TDPFBarStyle.bsDefault;
    property BarColor            : TAlphaColor read FBarColor write SetBarColor default TAlphaColors.Black;
    property BarBackgroundColor  : TAlphaColor read FBarBackgroundColor write SetBarBackgroundColor default TAlphaColors.White;
    property BarVisible          : Boolean read GetBarVisible write SetBarVisible stored true default true;
    property BarTranslucent      : Boolean read FBarTranslucent write SetBarTranslucent default false;
    property OnShowViewController: TOnShowViewController read FOnShowViewController write FOnShowViewController;
    property ToolbarVisible      : Boolean read GetToolbarVisible write SetToolbarVisible stored true default true;
    property BarBackgroundImage  : string read FBarBackgroundImage write SetBarBackgroundImage;
    property OnBeforeLoading     : TBeforeLoading read FBeforeLoading write FBeforeLoading;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Enabled;
    property Visible;
  end;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFButtonDelegate = interface( IObjectiveC )
    ['{5BA89EA4-9F26-4E69-8835-F8E6EF68FA53}']
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFNavigationBarButtonDelegate = class( TOCLocal, IDPFButtonDelegate )
  private
    FDPFNavigationBarButtonItem: TDPFCustomBarButtonItem;
  public
    constructor Create( ADPFNavigationBarButtonItem: TDPFCustomBarButtonItem );
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFNavigationController }
// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFNavigationController.Change;
begin
  { }
end;
{$ENDIF}

// ------------------------------------------------------------------------------
constructor TDPFNavigationController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'NavigationController';

  FPages := TList<TDPFNavigationControllerPage>.Create;

  FBeforeLoading := nil;

  FNavBar                  := TDPFNavigationNavBar.Create( Self );
  FNavBar.Parent           := Self;
  FNavBar.Align            := TAlignLayout.Top;
  FNavBar.Height           := 44;
  FNavBar.Locked           := true;
  FNavBar.Stored           := false;
  FNavBar.ControlCaption   := 'Nav Bar';
  FNavBar.AddThisToSubView := false;
  FNavBar.BringToFront;
  Self.AddObject( FNavBar );

  FToolBar                  := TDPFNavigationToolBar.Create( Self );
  FToolBar.Parent           := Self;
  FToolBar.Align            := TAlignLayout.Bottom;
  FToolBar.Height           := 44;
  FToolBar.Locked           := true;
  FToolBar.Stored           := false;
  FToolBar.ControlCaption   := 'Tool Bar';
  FToolBar.AddThisToSubView := false;
  FToolBar.BringToFront;
  Self.AddObject( FToolBar );

  FBarStyle           := TDPFBarStyle.bsDefault;
  Align               := TAlignLayout.Client;
  FBarColor           := TAlphaColors.Black;
  FBarBackgroundColor := TAlphaColors.White;
  FBarTranslucent     := False;
  // FBarVisible     := true;
  // FToolbarVisible := false;
{$IFDEF IOS}
  FUINavigationControllerDelegate := TUINavigationControllerDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFNavigationController.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
  begin
    FPages[I].FPageControl := nil;
  end;
  FPages.DisposeOf;
  FNavBar.DisposeOf;
{$IFDEF IOS}
  FUINavigationControllerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

function TDPFNavigationControllerPage.GetUIViewController: UIViewController;
begin
  result := FUIViewController;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationController.CreateToolBarButtonItem( ToolbarItem: TDPFToolBarButtonItem ): UIBarButtonItem;
var
  Image  : UIImage;
  CButton: UIButton;
  NS     : NSMutableArray;
  NI     : TDPFNavigationControllerPage;
begin
  NI := ToolbarItem.FDPFNavigationControllerPage;
  // Titled Button
  if ToolbarItem.ButtonItemKind = bkTitle then
  begin
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithTitle( NSStr( ToolbarItem.ButtonTitle ), Integer( ToolbarItem.ButtonItemStyle ), ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) );
  end
  // System Button
  else if ToolbarItem.ButtonItemKind = bkSystem then
  begin
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithBarButtonSystemItem( Integer( ToolbarItem.ButtonSystemItem ), ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) )
  end
  // Imaged Button
  else if ToolbarItem.ButtonItemKind = bkImage then
  begin
    Image   := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( ToolbarItem.Image ) ) );
    CButton := TUIButton.Wrap( TUIButton.OCClass.buttonWithType( UIButtonTypeCustom ) );
    CButton.setBounds( CGRectMake( 0, 0, Image.size.Width, Image.size.Height ) );
    CButton.setImage( Image, UIControlStateNormal );
    CButton.addTarget( ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ), UIControlEventTouchUpInside );
    Result  := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( CButton ) );
    Image   := nil;
    CButton := nil;
  end
  // CustomView
  else if ToolbarItem.ButtonItemKind = bkCustomView then
  begin
    if ToolbarItem.CustomView <> nil then
    begin
      if ToolbarItem.CustomView.UIControl = nil then
        ToolbarItem.CustomView.Loaded;

      // ToolbarItem.Width := ToolbarItem.CustomView.Width;
      // ToolbarItem.Height := ToolbarItem.CustomView.Height;

      UIView( ToolbarItem.CustomView.UIControl ).setFrame( CGRectMake( 0, 0, ToolbarItem.CustomView.Width, ToolbarItem.CustomView.Height ) );
      Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( UIView( ToolbarItem.CustomView.UIControl ) ) );
    end;
  end
  else
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.init );

  ToolbarItem.FDPFBarItem := Result;

  Result.setEnabled( ToolbarItem.Enabled );

  if ToolbarItem.Width <> 0 then
    Result.setWidth( ToolbarItem.Width );

  Result.setStyle( LongInt( ToolbarItem.ButtonItemStyle ) );

  Result.setTitle( NSStr( ToolbarItem.ButtonTitle ) );
  ToolbarItem.SetButtonColor( ToolbarItem.ButtonColor );

  NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.toolbarItems ) );
  NS.retain;
  NS.addObject( ( Result as ILocalObject ).GetObjectID );
  NI.FUIViewController.setToolbarItems( NS );
  NS.release;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationController.CreateNavBarButtonItem( NavBarItem: TDPFNavigationBarButtonItem ): UIBarButtonItem;
var
  Image  : UIImage;
  CButton: UIButton;
  NS     : NSMutableArray;
  NI     : TDPFNavigationControllerPage;
begin
  if NavBarItem.ButtonType = bkBack then
    exit;
  NI := NavBarItem.FDPFNavigationControllerPage;
  // Titled Button
  if NavBarItem.ButtonItemKind = bkTitle then
  begin
    { if TOSVersion.Major > 6 then
      Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithTitle( NSStr( NavBarItem.ButtonTitle ), Integer( NavBarItem.ButtonItemStyle ), nil, nil ) )
      else }
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithTitle( NSStr( NavBarItem.ButtonTitle ), Integer( NavBarItem.ButtonItemStyle ), NavBarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) )
  end
  // System Button
  else if NavBarItem.ButtonItemKind = bkSystem then
  begin
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithBarButtonSystemItem( Integer( NavBarItem.ButtonSystemItem ), NavBarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) )
  end
  // Imaged Button
  else if NavBarItem.ButtonItemKind = bkImage then
  begin
    Image   := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( NavBarItem.Image ) ) );
    CButton := TUIButton.Wrap( TUIButton.OCClass.buttonWithType( UIButtonTypeCustom ) );
    CButton.setBounds( CGRectMake( 0, 0, Image.size.Width, Image.size.Height ) );
    CButton.setImage( Image, UIControlStateNormal );
    CButton.addTarget( NavBarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ), UIControlEventTouchUpInside );
    Result  := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( CButton ) );
    Image   := nil;
    CButton := nil;
  end
  // CustomView
  else if NavBarItem.ButtonItemKind = bkCustomView then
  begin
    if NavBarItem.CustomView <> nil then
    begin
      if NavBarItem.CustomView.UIControl = nil then
        NavBarItem.CustomView.Loaded;

      // ToolbarItem.Width := ToolbarItem.CustomView.Width;
      // ToolbarItem.Height := ToolbarItem.CustomView.Height;

      UIView( NavBarItem.CustomView.UIControl ).setFrame( CGRectMake( 0, 0, NavBarItem.CustomView.Width, NavBarItem.CustomView.Height ) );
      Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( UIView( NavBarItem.CustomView.UIControl ) ) );
    end;
  end
  else
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.init );

  NavBarItem.FDPFBarItem := Result;
  {
    if NavBarItem.Width <> 0 then
    Result.setWidth( NavBarItem.Width );
    Result.setEnabled( NavBarItem.Enabled );

    Result.setStyle( Integer( NavBarItem.ButtonItemStyle ) );
    NavBarItem.SetButtonColor( NavBarItem.ButtonColor );
  }
  Result.setTintColor( TColorToUIColor( NavBarItem.ButtonColor ) );

  Result.setTitle( NSStr( NavBarItem.ButtonTitle ) );

  if ( NavBarItem.ButtonType = bkLeft ) then
  begin
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.leftBarButtonItems ) );
    NS.retain;
    NS.addObject( ( Result as ILocalObject ).GetObjectID );
    NI.FUIViewController.navigationItem.setLeftBarButtonItems( NS );
    NS.release;
  end
  else if NavBarItem.ButtonType = bkRight then
  begin
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.rightBarButtonItems ) );
    NS.retain;
    NS.addObject( ( Result as ILocalObject ).GetObjectID );
    NI.FUIViewController.navigationItem.setRightBarButtonItems( NS );
    NS.release;
  end
  else
  begin
    NI.FUIViewController.navigationItem.setBackBarButtonItem( Result );
    NI.FUIViewController.navigationItem.setHidesBackButton( false );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.Loaded;
var
  BIndex, I: Integer;
  BI       : TDPFNavigationControllerPage;
  RootView : TDPFNavigationControllerPage;
begin
  if assigned( FBeforeLoading ) then
    FBeforeLoading;

  RootView := nil;
  if FPages.Count > 0 then
    RootView := FPages.Items[0]
  else
  begin
    ShowMessage( 'No any page !!!  ' );
    exit;
  end;

  if not Assigned( FUINavigationController ) then
  begin
    FUINavigationController := TUINavigationController.Wrap( TUINavigationController.Alloc.initWithRootViewController( RootView.FUIViewController ) );
    FUIControl              := FUINavigationController;

    for I := 0 to FPages.Count - 1 do
    begin
      BI := FPages.Items[I];
      BI.FUIViewController.setTitle( NSStr( BI.FPageViewTitle ) );

      BI.PagePrompt := BI.PagePrompt;
      {
        if BI.PagePrompt <> '' then
        BI.FUIViewController.navigationItem.setPrompt( NSStr( BI.PagePrompt ) )
        else
        BI.FUIViewController.navigationItem.setPrompt( nil ); }
      BI.FUIViewController.navigationItem.setPrompt( NSStr( BI.PagePrompt ) );

      BI.FUIViewController.navigationItem.setHidesBackButton( BI.FHidesBackButton, true );

      for BIndex := 0 to BI.BarButtons.Count - 1 do
        CreateNavBarButtonItem( BI.BarButtons.Items[BIndex] );

      for BIndex := 0 to BI.ToolBarButtons.Count - 1 do
        CreateToolBarButtonItem( BI.ToolBarButtons.Items[BIndex] );
    end;
  end;

  SetBarStyle( FBarStyle );
  SetBarColor( FBarColor );
  SetBarVisible( GetBarVisible );
  SetToolbarVisible( GetToolbarVisible );
  SetBarBackgroundImage( FBarBackgroundImage );
  SetBarBackgroundColor( FBarBackgroundColor );

  FUINavigationController.navigationBar.setTranslucent( FBarTranslucent );
  FUINavigationController.setWantsFullScreenLayout( True );
  FUINavigationController.view.setFrame( CGRectMake( 0, 0, width, height ) );

  FUINavigationController.setDelegate( FUINavigationControllerDelegate.GetObjectID );
  addSubview( Self, ParentControl, FUINavigationController.View );

  UpdateUIControlPosition;
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.RemovePage( Page: TDPFNavigationControllerPage );
begin
  Page.FPageControl := nil;
  FPages.Remove( Page );
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.UpdateUIControlPosition;
{$IFDEF IOS}
var
  I    : Integer;
  frame: CGRect;
  P    : TDPFNavigationControllerPage;
  v    : UIView;
{$ENDIF}
begin
  inherited;
{$IFDEF IOS}
  if Assigned( FUINavigationController ) then
  begin
    if not IsIPad { and ( TOSVersion.Major < 7 ) } then
    begin
      frame           := FUINavigationController.navigationBar.frame;
      FNavBar.Height  := 44; // + 30;
      FToolBar.Height := 44;
      if TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).orientation in [UIDeviceOrientationLandscapeLeft, UIDeviceOrientationLandscapeRight] then
      begin
        FNavBar.Height  := 32 + 22;
        FToolBar.Height := 32;
      end;

      // FUINavigationController.navigationBar.setFrame( frame );
      for I := 0 to FPages.Count - 1 do
      begin
        P       := FPages.Items[I];
        P.Align := TAlignLayout.None;
        P.SetBounds( p.Margins.Left, p.Margins.top, Width - p.Margins.Right, Height - frame.size.height - p.Margins.Bottom );
        // P.PagePrompt := P.PagePrompt;
        P.Resize;
      end;
    end
    else
    begin
      FNavBar.Height  := 44; // + 30;
      FToolBar.Height := 44;
      if TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).orientation in [UIDeviceOrientationLandscapeLeft, UIDeviceOrientationLandscapeRight] then
      begin
        // frame.size.height := 32 + 22;
        FNavBar.Height  := 32 + 22;
        FToolBar.Height := 32;
      end;
    end;
    frame.size.height := FNavBar.Height;
    // Babak FUINavigationController.navigationBar.setFrame( frame );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFNavigationController.Paint;
begin
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, TAlphaColors.White );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.PushViewController( View: TDPFNavigationControllerPage; Animated: Boolean );
begin
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    if not FPages.Contains( View ) then
      ShowMessage( 'This View Controller not in Navigator page list: ' + '[' + View.Name + ']' )
    else
    begin
      FUINavigationController.pushViewController( View.FUIViewController, Animated );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.PopViewController( View: TDPFNavigationControllerPage; Animated: Boolean );
begin
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    if View = nil then
      FUINavigationController.popViewControllerAnimated( Animated )
    else if FPages.Contains( View ) then
      FUINavigationController.popToViewController( View.FUIViewController, Animated )
    else
      ShowMessage( 'This View Controller not in Navigator page list: ' + '[' + View.Name + ']' );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.PopRootViewController( Animated: Boolean );
begin
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    FUINavigationController.popToRootViewControllerAnimated( Animated );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.PresentViewControllerCompleted;
begin

end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.DismissViewController( Animated: Boolean );
begin
  if FUINavigationController <> nil then
  begin
    FUINavigationController.dismissViewControllerAnimated( true, PresentViewControllerCompleted )
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFNavigationController.GetBarVisible: Boolean;
begin
  result := FNavBar.Visible;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationController.GetPage( index: Integer ): TDPFNavigationControllerPage;
begin
  Result := nil;
  if ( index > 0 ) or ( index < FPages.Count ) then
    Result := FPages[index];
end;

// ------------------------------------------------------------------------------
function TDPFNavigationController.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationController.GetToolbarVisible: Boolean;
begin
  result := FToolBar.Visible;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

function TDPFNavigationController.GetNavigationController: UINavigationController;
begin
  result := FUINavigationController;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.InsertPage( Page: TDPFNavigationControllerPage );
begin
  if Page.FPageIndex < FPages.Count then
    FPages.Insert( Page.FPageIndex, Page )
  else
    FPages.Add( Page );

  Page.FPageControl := Self;

  if FNavBar.Parent <> Self then
    FNavBar.Parent := Self;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.PresentViewController( View: TDPFUIViewController; FModalTransitionStyle: TDPFModalTransitionStyle; Animated: Boolean );
begin
  if FUINavigationController <> nil then
  begin
    FUINavigationController.setModalInPopover( true );

    // view.FUIViewController.setModalPresentationStyle( 1 );
    view.FUIViewController.setModalTransitionStyle( Integer( FModalTransitionStyle ) );

    FUINavigationController.presentViewController( view.FUIViewController, Animated, PresentViewControllerCompleted )
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetActivePage( const Page: TDPFNavigationControllerPage );
begin
  if Page = nil then
  begin
    if FPages.Count > 0 then
      FActivePage := FPages[0];
    Exit;
  end;
  if Page = FActivePage then
    Exit;

  if Page.PageControl <> Self then
    Exit;
  Page.BringToFront;

  if Assigned( Application ) and Assigned( Application.MainForm ) then
    TCustomForm( Application.MainForm ).ActiveControl := Page;

  Page.PagePrompt := Page.PagePrompt;
  FActivePage     := Page;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarBackgroundColor( const Value: TAlphaColor );
begin
  FBarBackgroundColor := Value;
{$IFDEF IOS}
  if assigned( FUINavigationController ) and assigned( FUINavigationController.navigationBar ) then
  begin
    if TOSVersion.Major > 6 then
    begin
      if Value = TAlphaColors.Null then
      begin
        if FUINavigationController.navigationBar.barTintColor <> nil then
          FUINavigationController.navigationBar.setBarTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
      end
      else
        FUINavigationController.navigationBar.setBarTintColor( TColorToUIColor( value ) );
    end
    else
    begin
      if Value = TAlphaColors.Null then
        FUINavigationController.navigationBar.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
      else
        FUINavigationController.navigationBar.setBackgroundColor( TColorToUIColor( value ) );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Image2: UIImage;
  Image1: UIImage;
{$IFNDEF IOSDEVICE}
  transform: CGAffineTransform;
  context  : CGContextRef;
{$ENDIF}
{$ENDIF}
begin
  FBarBackgroundImage := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    if FBarBackgroundImage = '' then
    begin
      FUINavigationController.navigationBar.setBackgroundImage( nil, 0 );
    end
    else
    begin

      Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBarBackgroundImage ) ) );
      Image2 := nil;
      if Assigned( Image1 ) and ( Assigned( Image1.CGImage ) or Assigned( Image1.CIImage ) ) and ( Image1.size.width > 0.99 ) and ( Image1.size.height > 0.99 ) then
      begin
        UIGraphicsBeginImageContext( FUINavigationController.navigationBar.frame.size );
{$IFNDEF IOSDEVICE}
        // ----------------------------------------------------------
        // Mirror on Simulator
        context   := UIGraphicsGetCurrentContext( );
        transform := CGAffineTransformMakeTranslation( 0.0, Height );
        transform := CGAffineTransformScale( transform, 1.0, -1.0 );
        CGContextConcatCTM( context, transform );
{$ENDIF}
        Image1.drawInRect( FUINavigationController.navigationBar.bounds );
        Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
        UIGraphicsEndImageContext( );
      end;
      FUINavigationController.navigationBar.setBackgroundImage( Image2, 0 );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarColor( const Value: TAlphaColor );
begin
  FBarColor := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    if FBarColor <> TAlphaColors.Null then
      FUINavigationController.navigationBar.setTintColor( TColorToUIColor( FBarColor ) )
    else
    begin
      if TOSVersion.Major > 6 then
        FUINavigationController.navigationBar.setTintColor( TUIColor.Wrap( TUIColor.OCClass.blackColor ) )
      else
        FUINavigationController.navigationBar.setTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarStyle( const Value: TDPFBarStyle );
begin
  FBarStyle := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
    FUINavigationController.navigationBar.setBarStyle( Integer( Value ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarTranslucent( const Value: Boolean );
begin
  FBarTranslucent := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
  begin
    FUINavigationController.navigationBar.setTranslucent( Value )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetBarVisible( const Value: Boolean );
begin
  FNavBar.Visible := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
    FUINavigationController.navigationBar.setHidden( not Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationController.SetToolbarVisible( const Value: Boolean );
begin
  FToolBar.Visible := Value;
{$IFDEF IOS}
  if FUINavigationController <> nil then
    FUINavigationController.setToolbarHidden( not Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFNavigationControllerPage }

constructor TDPFNavigationControllerPage.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  Align            := TAlignLayout.Client;
  FBarButtons      := TDPFNavigationBarButtonCollection.Create( Self );
  FToolBarButtons  := TDPFToolBarButtonCollection.Create( Self );
  FHidesBackButton := false;
  SetSubComponent( true );
  if AOwner is TDPFNavigationController then
  begin
    PageControl := AOwner as TDPFNavigationController;
  end;

  AddThisToSubView := False;

end;

// ------------------------------------------------------------------------------
destructor TDPFNavigationControllerPage.Destroy;
{$IFDEF IOS}
{$ENDIF}
begin
  SetPageControl( nil );
  FBarButtons.DisposeOf;
  FToolBarButtons.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.Resize;
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) and assigned( FDPFNavigationController ) then
    begin
      Margins.Top    := 44; // 88;
      Margins.Bottom := 44; // 88;
      Height         := FDPFNavigationController.Height - 44;
      C.setFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, ( ( Height - FDPFNavigationController.FToolBar.Height ) * Scale.Y ) ) );
    end;
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationControllerPage.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf( Self )
  else
    Result := -1;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.Loaded;
begin
  inherited;
  if ( name <> '' ) and ( ParentControl is TDPFNavigationController ) then
    PageControl := TDPFNavigationController( ParentControl );
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationBarButtonItem.AddToNavigation;
{$IFDEF IOS}
var
  NavigationController: TDPFNavigationController;
  BB                  : UIBarButtonItem;
  NS                  : NSMutableArray;
  NI                  : TDPFNavigationControllerPage;
{$ENDIF}
begin
{$IFDEF IOS}
  NI                   := Self.FDPFNavigationControllerPage;
  NavigationController := Self.FDPFNavigationControllerPage.FDPFNavigationController;

  if ButtonType = bkLeft then
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.leftBarButtonItems ) )
  else if ButtonType = bkRight then
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.rightBarButtonItems ) );
  NS.retain;

  BB := NavigationController.CreateNavBarButtonItem( Self );
  NS.addObject( ( BB as ILocalObject ).GetObjectID );

  if ButtonType = bkLeft then
    NI.FUIViewController.navigationItem.setLeftBarButtonItems( NS )
  else if ButtonType = bkRight then
    NI.FUIViewController.navigationItem.setRightBarButtonItems( NS )
  else
    NI.FUIViewController.navigationItem.setBackBarButtonItem( BB );
  NS.release;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCustomBarButtonItem.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
    FDPFBarItem.setEnabled( FEnabled );
{$ENDIF}
end;

procedure TDPFCustomBarButtonItem.SetTagNative( const Value: NativeInt );
begin
  FTagNative := Value;
{$IFDEF IOS}
  if Assigned( FDPFBarItem ) then
    FDPFBarItem.setTag( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCustomBarButtonItem.SetWidth( const Value: single );
begin
  FWidth := Value;
{$IFDEF IOS}
  if Assigned( FDPFBarItem ) then
    FDPFBarItem.setWidth( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetPageControl( const APageControl: TDPFNavigationController );
begin
  if FPageControl <> APageControl then
  begin
    FDPFNavigationController := APageControl as TDPFNavigationController;
    if FPageControl <> nil then
      FPageControl.RemovePage( Self );
    Parent := APageControl;
    if APageControl <> nil then
      APageControl.InsertPage( Self );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetPageIndex( const Value: Integer );
var
  I, MaxPageIndex: Integer;
begin

  if FPageControl <> nil then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.Create( 'TDPFNavigationControllerPage.SetPageIndex: ' + IntToStr( Value ) );

    FPageControl.FPages.Move( PageIndex, Value );

    for I := 0 to FPageControl.FPages.Count - 1 do
    begin
      FPageControl.FPages[I].index := I;
    end;
    FPageControl.FPages[value].BringToFront;
  end;
  FPageIndex := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetPagePrompt( const Value: string );
var
  S: string;
begin
  FPagePrompt := Value;
  if Assigned( FDPFNavigationController ) then
  begin
    S := '';
    // FDPFNavigationController.FNavBar.Height := 88;
    if value <> '' then
    begin
      S := #10#13;
      // FDPFNavigationController.FNavBar.Height := 170; // 74;
    end;

    FDPFNavigationController.FNavBar.ControlCaption := value + S + 'Nav Bar';
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetPageViewTitle( const Value: string );
begin
  FPageViewTitle := Value;
{$IFDEF IOS}
  if FUIViewController <> nil then
  begin
    FUIViewController.setTitle( NSStr( Value ) );
    // FUIViewController. navigationItem.SetTitle( NSStr( value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetToolBarButtons( const Value: TDPFToolBarButtonCollection );
begin
  FToolBarButtons.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetBarButtons( const Value: TDPFNavigationBarButtonCollection );
begin
  FBarButtons.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerPage.SetHidesBackButton( const Value: boolean );
begin
  FHidesBackButton := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCustomBarButtonItem.SetButtonColor( const Value: TAlphaColor );
begin
  FButtonColor := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
  begin
    if Value <> TAlphaColors.Null then
      FDPFBarItem.setTintColor( TColorToUIColor( Value ) )
    else
      FDPFBarItem.setTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFNavigationBarButtonItem.Create( AOwner: TCollection );
begin
  inherited;
  FButtonType       := TDPFBarButtonType.bkLeft;
  FButtonSystemItem := bbsiDone;
end;

// ------------------------------------------------------------------------------
destructor TDPFNavigationBarButtonItem.Destroy;
{$IFDEF IOS}
var
  NS: NSMutableArray;
  NI: TDPFNavigationControllerPage;
{$ENDIF}
begin
{$IFDEF IOS}
  NS := nil;
  NI := Self.FDPFNavigationControllerPage;
  if ButtonType = bkLeft then
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.leftBarButtonItems ) )
  else if ButtonType = bkRight then
    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.rightBarButtonItems ) );
  if assigned( NS ) then
    NS.retain;

  if ButtonType = bkLeft then
    NI.FUIViewController.navigationItem.setLeftBarButtonItems( NS )
  else if ButtonType = bkRight then
    NI.FUIViewController.navigationItem.setRightBarButtonItems( NS )
  else
    NI.FUIViewController.navigationItem.setBackBarButtonItem( nil );

  if assigned( NS ) then
    NS.release;
{$ENDIF}
  inherited;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFNavigationBarButtonItem.GetUIBarButtonItem: UIBarButtonItem;
begin
  Result := FDPFBarItem;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFNavigationBarButtonItem.SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
{$IFDEF IOS}
var
  NavigationController: TDPFNavigationController;
  BB                  : UIBarButtonItem;
  NS                  : NSMutableArray;
  Idx                 : Integer;
  NI                  : TDPFNavigationControllerPage;
{$ENDIF}
begin
  if FButtonSystemItem = Value then
    exit;

  FButtonSystemItem := Value;
{$IFDEF IOS}
  NI := Self.FDPFNavigationControllerPage;
  if FDPFBarItem <> nil then
  begin
    NS := nil;
    if ButtonType = bkLeft then
      NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.leftBarButtonItems ) )
    else if ButtonType = bkRight then
      NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.navigationItem.rightBarButtonItems ) );
    if assigned( NS ) then
      NS.retain;

    Idx := NS.indexOfObject( ( FDPFBarItem as ILocalObject ).GetObjectID );
    BB  := NavigationController.CreateNavBarButtonItem( Self );
    NS.replaceObjectAtIndex( Idx, ( BB as ILocalObject ).GetObjectID );

    if ButtonType = bkLeft then
      NI.FUIViewController.navigationItem.setLeftBarButtonItems( NS )
    else if ButtonType = bkRight then
      NI.FUIViewController.navigationItem.setRightBarButtonItems( NS )
    else
      NI.FUIViewController.navigationItem.setBackBarButtonItem( BB );
    if assigned( NS ) then
      NS.release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCustomBarButtonItem.SetButtonTitle( const Value: string );
begin
  FButtonTitle := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
  begin
    if FButtonTitle = '' then
      FDPFBarItem.setTitle( nil )
    else
      FDPFBarItem.setTitle( NSStr( FButtonTitle ) );
  end
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFNavigationBarButtonDelegate }
{$IFDEF IOS}

procedure TDPFNavigationBarButtonDelegate.clickedButton( Sender: UIBarButtonItem );
begin
  if Assigned( FDPFNavigationBarButtonItem.OnClick ) then
    FDPFNavigationBarButtonItem.FOnClick( FDPFNavigationBarButtonItem )
  else if ( FDPFNavigationBarButtonItem is TDPFNavigationBarButtonItem ) and ( ( FDPFNavigationBarButtonItem as TDPFNavigationBarButtonItem ).ButtonType = bkBack ) then
    FDPFNavigationBarButtonItem.FDPFNavigationControllerPage.FDPFNavigationController.PopViewController( FDPFNavigationBarButtonItem.FDPFNavigationControllerPage, true );
end;

// ------------------------------------------------------------------------------
constructor TDPFNavigationBarButtonDelegate.Create( ADPFNavigationBarButtonItem: TDPFCustomBarButtonItem );
begin
  inherited Create;
  FDPFNavigationBarButtonItem := ADPFNavigationBarButtonItem;
end;

// ------------------------------------------------------------------------------
{ TUINavigationControllerDelegate }

constructor TUINavigationControllerDelegate.Create( ADPFNavigationController: TDPFNavigationController );
begin
  inherited Create;
  FDPFNavigationController := ADPFNavigationController;
end;

// ------------------------------------------------------------------------------
procedure TUINavigationControllerDelegate.navigationController( navigationController: UINavigationController; didShowViewController: UIViewController; animated: Boolean );
var
  idx     : integer;
  PrevPage: TDPFNavigationControllerPage;
begin
  for idx := 0 to FDPFNavigationController.FPages.Count - 1 do
    if ( FDPFNavigationController.FPages[Idx].FUIViewController as ILocalObject ).GetObjectID = ( didShowViewController as ILocalObject ).GetObjectID then
      break;
  if idx < FDPFNavigationController.FPages.Count then
  begin
    PrevPage := FDPFNavigationController.FActivePage;
    if idx <> -1 then
      FDPFNavigationController.FActivePage := FDPFNavigationController.Pages[idx];
  end;
  if Assigned( FDPFNavigationController.FOnShowViewController ) then
    FDPFNavigationController.FOnShowViewController( FDPFNavigationController, PrevPage );
end;

{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFCustomBarButtonItem }

constructor TDPFCustomBarButtonItem.Create( AOwner: TCollection );
begin
  inherited;
  FDPFNavigationControllerPage := TDPFNavigationBarButtonCollection( AOwner ).FDPFNavigationControllerPage;
  FButtonTitle                 := '';
  FEnabled                     := True;
  FButtonItemStyle             := bbisPlain;
  FButtonItemKind              := bkSystem;
  FButtonColor                 := TAlphaColors.Gray;
{$IFDEF IOS}
  FDPFBarItem     := nil;
  FButtonDelegate := TDPFNavigationBarButtonDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFCustomBarButtonItem.Destroy;
begin
{$IFDEF IOS }
  if Assigned( FDPFBarItem ) then
  begin
    FDPFBarItem.setAction( nil );
    FDPFBarItem.setTarget( nil );
    FDPFBarItem.release;
  end;

  FButtonDelegate.DisposeOf;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFCustomBarButtonItem.GetDisplayName: string;
begin
  Result := Format( 'Button%d', [index] );
end;

// ------------------------------------------------------------------------------
{ TDPFNavigationBarButtonCollection }
function TDPFNavigationBarButtonCollection.Add: TDPFNavigationBarButtonItem;
begin
  Result := inherited Add as TDPFNavigationBarButtonItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFNavigationBarButtonCollection.Create( ADPFNavigationControllerPage: TComponent );
begin
  inherited Create( TDPFNavigationBarButtonItem );
  FDPFNavigationControllerPage := TDPFNavigationControllerPage( ADPFNavigationControllerPage );
end;

// ------------------------------------------------------------------------------
destructor TDPFNavigationBarButtonCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationBarButtonCollection.GetItem( Index: Integer ): TDPFNavigationBarButtonItem;
begin
  Result := inherited Items[index] as TDPFNavigationBarButtonItem;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationBarButtonCollection.GetOwner: TPersistent;
begin
  Result := FDPFNavigationControllerPage;
end;

// ------------------------------------------------------------------------------
function TDPFNavigationBarButtonCollection.Insert( Index: Integer ): TDPFNavigationBarButtonItem;
begin
  Result := inherited insert( index ) as TDPFNavigationBarButtonItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationBarButtonCollection.SetItem( Index: Integer; Value: TDPFNavigationBarButtonItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationBarButtonCollection.Update( Item: TCollectionItem );
begin
  inherited;

end;

// ------------------------------------------------------------------------------
{ TDPFToolBarButtonItem }

constructor TDPFToolBarButtonItem.Create( AOwner: TCollection );
begin
  inherited create( AOwner );
  FWidth := 0;
end;

// ------------------------------------------------------------------------------
procedure TDPFToolBarButtonItem.SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
{$IFDEF IOS}
var
  NavigationController: TDPFNavigationController;
  BB                  : UIBarButtonItem;
  NS                  : NSMutableArray;
  Idx                 : Integer;
  NI                  : TDPFNavigationControllerPage;
{$ENDIF}
begin
  FButtonSystemItem := Value;
{$IFDEF IOS}
  NI := Self.FDPFNavigationControllerPage;
  if FDPFBarItem <> nil then
  begin

    NS := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( NI.FUIViewController.toolbarItems ) );
    NS.retain;

    Idx := NS.indexOfObject( ( FDPFBarItem as ILocalObject ).GetObjectID );
    BB  := NavigationController.CreateToolBarButtonItem( Self );
    NS.replaceObjectAtIndex( Idx, ( BB as ILocalObject ).GetObjectID );

    NI.FUIViewController.setToolbarItems( NS );
    NS.release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFToolBarButtonCollection }
function TDPFToolBarButtonCollection.Add: TDPFToolBarButtonItem;
begin
  Result := inherited Add as TDPFToolBarButtonItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFToolBarButtonCollection.Create( ADPFNavigationControllerPage: TComponent );
begin
  inherited Create( TDPFToolBarButtonItem );
  FDPFNavigationControllerPage := TDPFNavigationControllerPage( ADPFNavigationControllerPage );
end;

// ------------------------------------------------------------------------------
destructor TDPFToolBarButtonCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFToolBarButtonCollection.GetItem( index: Integer ): TDPFToolBarButtonItem;
begin
  Result := inherited Items[index] as TDPFToolBarButtonItem;
end;

// ------------------------------------------------------------------------------
function TDPFToolBarButtonCollection.GetOwner: TPersistent;
begin
  Result := FDPFNavigationControllerPage;
end;

// ------------------------------------------------------------------------------
function TDPFToolBarButtonCollection.Insert( index: Integer ): TDPFToolBarButtonItem;
begin
  Result := inherited insert( index ) as TDPFToolBarButtonItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFToolBarButtonCollection.SetItem( index: Integer; Value: TDPFToolBarButtonItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFToolBarButtonCollection.Update( Item: TCollectionItem );
begin
  inherited;

end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFNavigationToolBar }
procedure TDPFNavigationToolBar.Paint;
var
  Items   : TArrayOfUniversalToolBarItem;
  i       : integer;
  BarItems: TDPFToolBarButtonCollection;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  if ( Parent as TDPFNavigationController ).ActivePage = nil then
  begin
    if ( Parent as TDPFNavigationController ).PageCount > 0 then
      BarItems := ( Parent as TDPFNavigationController ).Pages[0].ToolBarButtons
    else
      BarItems := nil;
  end
  else
    BarItems := ( Parent as TDPFNavigationController ).ActivePage.ToolBarButtons;
  if BarItems = nil then
    SetLength( Items, 0 )
  else
  begin
    SetLength( Items, BarItems.Count );
    for i := 0 to high( Items ) do
    begin
      Items[i].ButtonItemKind   := BarItems[i].ButtonItemKind;
      Items[i].ButtonItemStyle  := BarItems[i].ButtonItemStyle;
      Items[i].ButtonSystemItem := BarItems[i].ButtonSystemItem;
      Items[i].Title            := BarItems[i].ButtonTitle;
      Items[i].Width            := BarItems[i].Width;
    end;
  end;
  PaintToolBar( Self, Items );
  Canvas.EndScene;
end;
{$ENDIF}
{ TDPFNavigationNavBar }

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFNavigationNavBar.Paint;
var
  Page              : TDPFNavigationControllerPage;
  q, y, i, j, NoS   : integer;
  w, x, fs, SumWidth: single;
  R                 : TRectF;
  s                 : string;
  Items             : TArrayOfUniversalToolBarItem;
  ItemsHelper       : array of record IsLeft, CanPainted: boolean;
end;
LeftButtons, RightButtons: array of integer;
BackButton, PageTitle: record Needed: boolean;
Width, NeedMoreSpace: single;
Text:
string;
end;

// ------------------------------------------------------------------------------
procedure PaintButton( Item: TUniversalToolBarItem; pos: single );
var
  Bmp: TBitmap;

begin
  // Space
  if ( Item.ButtonItemKind = bkSystem ) and ( Item.ButtonSystemItem in [bbsiFlexibleSpace, bbsiFixedSpace] ) then
    Exit;

  // Left
  R := RectF( x + 5, y + 1, x + 10, y + 31 );
  BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonLeft, R );
  // Center
  R.Left  := R.Right;
  R.Right := R.Left + GetWidthOfToolBarButton( Item, false ) - 10;
  BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonBG, R );
  // Right
  R.Left  := R.Right;
  R.Right := R.Left + 5;
  BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonRight, R );

  // System
  if Item.ButtonItemKind = bkSystem then
  begin
    Bmp := nil;
    case Item.ButtonSystemItem of
      bbsiAdd:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add;
      bbsiCancel:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel;
      bbsiDone:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done;
      bbsiEdit:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit;
      bbsiSave:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save;
      bbsiCompose:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose;
      bbsiReply:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply;
      bbsiAction:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action;
      bbsiOrganize:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize;
      bbsiBookmarks:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks;
      bbsiSearch:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search;
      bbsiRefresh:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh;
      bbsiStop:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop;
      bbsiCamera:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera;
      bbsiTrash:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash;
      bbsiPlay:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play;
      bbsiPause:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause;
      bbsiRewind:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind;
      bbsiFastForward:
        Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward;
    end;
    if Bmp <> nil then
      BitmapToPosition( Self, Bmp, x + 10, y + 1 );
  end;

  // Title
  if Item.ButtonItemKind = bkTitle then
  begin
    Canvas.Font.Style  := [TFontStyle.fsBold];
    Canvas.Font.Family := 'Helvetica';
    Canvas.Font.Size   := 12;
    // Shadow
    Canvas.Fill.Color := TAlphaColors.Gray;
    R                 := RectF( x + 5, y, x + 5 + GetWidthOfToolBarButton( Item, false ), y + 30 );
    PaintCaption( Self, Item.Title, R, lbMiddleTruncation, 1, TTextAlign.Center );
    // Text
    Canvas.Fill.Color := TAlphaColors.White;
    R                 := RectF( x + 5, y + 1, x + 5 + GetWidthOfToolBarButton( Item, false ), y + 31 );
    PaintCaption( Self, Item.Title, R, lbMiddleTruncation, 1, TTextAlign.Center );
  end;

end;

begin
  Canvas.BeginScene;
  { Uncomment the following 2 lines if you want to turn off designtime rendering: }
  // Canvas.EndScene;
  // Exit;

  if ( Parent as TDPFNavigationController ).ActivePage = nil then
  begin
    if ( Parent as TDPFNavigationController ).PageCount > 0 then
      Page := ( Parent as TDPFNavigationController ).Pages[0]
    else
      Page := nil;
  end
  else
    Page := ( Parent as TDPFNavigationController ).ActivePage;

  // Paint BG
  if Height = 44 then
  begin
    BitmapAsBackground( Self, iOS_GUI_Bitmaps.NavigationController.SmallBG );
    y := 7; // Button's top
  end
  else
  begin
    BitmapAsBackground( Self, iOS_GUI_Bitmaps.NavigationController.LargeBG );
    y := 37; // Button's top
  end;

  // No page - Exit
  if Page = nil then
  begin
    Canvas.EndScene;
    Exit;
  end;

  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := 'Helvetica';

  // Page Prompt
  if Page.PagePrompt <> '' then
  begin
    Canvas.Font.Size  := 15;
    Canvas.Fill.Color := $FFA3A3A3;
    R                 := RectF( 0, 0, Width, 37 );
    PaintCaption( Self, Page.PagePrompt, R, lbTailTruncation, 1, TTextAlign.Center );
  end;

  // Items
  NoS := 0; // Number of FlexisbleSpaces
  SetLength( Items, 0 );
  SetLength( ItemsHelper, 0 );
  SetLength( LeftButtons, 0 );
  SetLength( RightButtons, 0 );
  Canvas.Font.Size := 12;
  for q            := 0 to Page.BarButtons.Count - 1 do
    if Page.BarButtons[q].ButtonType <> bkBack then
    begin
      SetLength( Items, Length( Items ) + 1 );
      SetLength( ItemsHelper, Length( ItemsHelper ) + 1 );
      i                         := high( Items );
      Items[i].ButtonItemKind   := Page.BarButtons[q].ButtonItemKind;
      Items[i].ButtonItemStyle  := DPF.iOS.BaseControl.TDPFBarButtonItemStyle.bbisBordered; // Always bordered
      Items[i].ButtonSystemItem := Page.BarButtons[q].ButtonSystemItem;
      Items[i].Title            := Page.BarButtons[q].ButtonTitle;
      if Items[i].ButtonItemKind = bkTitle then
        Items[i].Width := Max( Canvas.TextWidth( Items[i].Title ) + 10, 48 )
      else
        Items[i].Width := Page.BarButtons[q].Width;
      if ( Items[i].ButtonItemKind = bkSystem ) and ( Items[i].ButtonSystemItem = bbsiFlexibleSpace ) then
        inc( NoS );
      ItemsHelper[i].IsLeft     := Page.BarButtons[q].ButtonType = bkLeft;
      ItemsHelper[i].CanPainted := false;
      if ItemsHelper[i].IsLeft then
      begin
        SetLength( LeftButtons, Length( LeftButtons ) + 1 );
        LeftButtons[high( LeftButtons )] := i;
      end
      else
      begin
        SetLength( RightButtons, Length( RightButtons ) + 1 );
        RightButtons[high( RightButtons )] := i;
      end;
    end;

  // Is there a Back button ?
  BackButton.Needed        := Page.PageIndex > 0;
  BackButton.NeedMoreSpace := 0;
  if BackButton.Needed then
  begin
    for q := 0 to Page.BarButtons.Count - 1 do
      if Page.BarButtons[q].ButtonType = bkLeft then // If there is any bkLeft button then no Back Button is used
      begin
        BackButton.Needed := false;
        Break;
      end;
  end;

  // Has page a title ?
  PageTitle.Needed        := Page.PageViewTitle <> '';
  PageTitle.NeedMoreSpace := 0;

  // Calculate how much space remains for the buttons
  SumWidth := Width;
  if BackButton.Needed then
  begin
    BackButton.NeedMoreSpace := 0;
    // Get prev. page's title
    s               := ( Parent as TDPFNavigationController ).Pages[Page.PageIndex - 1].PageViewTitle;
    BackButton.Text := s;
    if s = '' then
      s := 'Back'
    else if Canvas.TextWidth( BackButton.Text ) > Canvas.TextWidth( copy( s, 1, 4 ) + '...' ) then
    begin
      s                        := copy( s, 1, 4 ) + '...';
      BackButton.NeedMoreSpace := Min( 160 - iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Width - iOS_GUI_Bitmaps.NavigationController.ButtonRight.Width, Canvas.TextWidth( BackButton.Text ) ) - Canvas.TextWidth( s );
    end;
    Canvas.Font.Size := 12;
    BackButton.Width := Canvas.TextWidth( s );
    SumWidth         := SumWidth - BackButton.Width - iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Width - iOS_GUI_Bitmaps.NavigationController.ButtonRight.Width - 10 { Margin };
  end;
  if PageTitle.Needed then
  begin
    PageTitle.NeedMoreSpace := 0;
    s                       := Page.PageViewTitle;
    PageTitle.Text          := s;
    if Canvas.TextWidth( BackButton.Text ) > Canvas.TextWidth( copy( s, 1, 5 ) + '...' ) then
    begin
      s                       := copy( s, 1, 5 ) + '...';
      PageTitle.NeedMoreSpace := Canvas.TextWidth( BackButton.Text ) - Canvas.TextWidth( s );
    end;
    Canvas.Font.Size := 15;
    PageTitle.Width  := Canvas.TextWidth( s );
    SumWidth         := SumWidth - PageTitle.Width - 10 { Margin };
  end;

  // Get how many button can be painted
  i := 0;
  j := 0;
  repeat
    // Check for the next left button
    if i <= high( LeftButtons ) then
    begin
      w := GetWidthOfToolBarButton( Items[LeftButtons[i]], true );
      if SumWidth > w then
      begin
        SumWidth                               := SumWidth - w;
        ItemsHelper[LeftButtons[i]].CanPainted := true;
        inc( i );
      end
      else
        i := -1;
    end;
    // Check for the next right button
    if j <= high( RightButtons ) then
    begin
      w := GetWidthOfToolBarButton( Items[RightButtons[j]], true );
      if SumWidth > w then
      begin
        SumWidth                                := SumWidth - w;
        ItemsHelper[RightButtons[j]].CanPainted := true;
        inc( j );
      end
      else
        j := -1;
    end;
  until ( ( i > high( LeftButtons ) ) or ( i = -1 ) ) and ( ( j > high( RightButtons ) ) or ( j = -1 ) );

  // Some space is remained, add it to the BackButton and/or PageTitle
  if SumWidth > 0 then
  begin
    while SumWidth > 0 do
    begin
      if BackButton.Needed and ( BackButton.NeedMoreSpace > 0 ) then
      begin
        BackButton.Width         := BackButton.Width + 1;
        BackButton.NeedMoreSpace := BackButton.NeedMoreSpace - 1;
        SumWidth                 := SumWidth - 1;
      end;
      if PageTitle.Needed and ( PageTitle.NeedMoreSpace > 0 ) then
      begin
        PageTitle.Width         := PageTitle.Width + 1;
        PageTitle.NeedMoreSpace := PageTitle.NeedMoreSpace - 1;
        SumWidth                := SumWidth - 1;
      end;
      // No more space needed
      if not BackButton.Needed and not PageTitle.Needed then
        break;
      if ( BackButton.NeedMoreSpace <= 0 ) and ( PageTitle.NeedMoreSpace <= 0 ) then
        break;
    end;
  end;

  // If any space remains, calculate FlexibleSpaces from it
  if ( SumWidth > 0 ) and ( NoS > 0 ) then
    fs := SumWidth / NoS
  else
    fs := 0;

  // Set FlexibleSpaces to FixedSpace with fs as Width
  for i := 0 to high( Items ) do
    if ( Items[i].ButtonItemKind = bkSystem ) and ( Items[i].ButtonSystemItem = bbsiFlexibleSpace ) then
    begin
      Items[i].ButtonSystemItem := bbsiFixedSpace;
      Items[i].Width            := fs;
      ItemsHelper[i].CanPainted := true;
    end;

  // Paint Buttons + Title
  x := 0;

  // BackButton
  if BackButton.Needed then
  begin
    x := x + 5; // Left margin
    // Left
    R.Left   := x;
    R.Top    := y;
    R.Right  := x + iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Width;
    R.Bottom := y + iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Height;
    BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack, R );
    // Center
    R.Left  := R.Right;
    R.Right := R.Left + BackButton.Width;
    BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonBG, R );
    // Right
    R.Left  := R.Right;
    R.Right := R.Left + iOS_GUI_Bitmaps.NavigationController.ButtonLeft.Width;
    BitmapToRect( Self, iOS_GUI_Bitmaps.NavigationController.ButtonRight, R );
    Canvas.Font.Size := 12;
    // Shadow
    R.Left            := x + iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Width;
    R.Right           := R.Left + BackButton.Width;
    Canvas.Fill.Color := TAlphaColors.Gray;
    PaintCaption( Self, BackButton.Text, R, lbTailTruncation, 1, TTextAlign.Center );
    // Text
    R.Top             := R.Top + 1;
    R.Bottom          := R.Bottom + 1;
    Canvas.Fill.Color := TAlphaColors.White;
    PaintCaption( Self, BackButton.Text, R, lbTailTruncation, 1, TTextAlign.Center );
    // Left button's X
    x := R.Right;
  end
  else
    x := 0;

  // Left Buttons
  for i := 0 to high( LeftButtons ) do
  begin
    if ItemsHelper[LeftButtons[i]].CanPainted then
    begin
      PaintButton( Items[LeftButtons[i]], x );
      x := x + GetWidthOfToolBarButton( Items[LeftButtons[i]], true );
    end;
  end;
  w := x; // Save for Title

  // Right Buttons
  x     := Width;
  for i := 0 to high( RightButtons ) do
  begin
    if ItemsHelper[RightButtons[i]].CanPainted then
    begin
      x := x - GetWidthOfToolBarButton( Items[RightButtons[i]], true );
      PaintButton( Items[RightButtons[i]], x );
    end;
  end;

  // Title
  if PageTitle.Needed then
  begin
    Canvas.Font.Size  := 18;
    Canvas.Fill.Color := $FFFFFFFF;
    R                 := RectF( w, y, x, y + 30 );
    PaintCaption( Self, PageTitle.Text, R, lbTailTruncation, 1, TTextAlign.Center );
  end;

  Canvas.EndScene;
end;
{$ENDIF}

end.
