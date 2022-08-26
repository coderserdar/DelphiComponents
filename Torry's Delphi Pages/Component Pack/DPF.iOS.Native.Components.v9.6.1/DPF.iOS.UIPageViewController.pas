// ------------------------------------------------------------------------------
// DPF.iOS.UIPageViewController Component
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
unit DPF.iOS.UIPageViewController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  DPF.iOS.BaseControl,
  DPF.iOS.UIViewController,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  iOSapi.CocoaTypes,
  DPF.iOS.Common,
{$ENDIF}
  System.TypInfo;

type

  TDPFUIPageViewController = class;

  TDPFPageViewControllerSpineLocation = ( pvcslNone = 0, pvcslMin = 1, pvcslMid = 2, pvcslMax = 3 );

  TDPFPageViewControllerNavigationDirection = ( pvcndForward = 0, pvcndReverse = 1 );

  TDPFPageViewControllerOption = ( pvcoSpineLocationKey, pvcoInterPageSpacingKey );

  TDPFPageViewControllerTransitionStylePageCurl = ( tsPageCurl = 0, tsScroll = 1 );

  TDPFPageViewControllerNavigationOrientation = ( vnoHorizontal = 0, vnoVertical = 1 );

{$IFDEF IOS}
  TPageViewControllerCompletion = procedure( finished: boolean ) of object;

  UIPageViewController = interface( UIViewController )
    ['{BD8F4370-A8C4-4B5D-9001-CCAB87C3E446}']
    function dataSource: Pointer; cdecl;
    function delegate: Pointer; cdecl;
    function gestureRecognizers: NSArray; cdecl;
    function initWithTransitionStyle( style: UIPageViewControllerTransitionStyle; navigationOrientation: UIPageViewControllerNavigationOrientation; options: NSDictionary ): Pointer; cdecl;
    function isDoubleSided: Boolean; cdecl;
    function navigationOrientation: UIPageViewControllerNavigationOrientation; cdecl;
    procedure setDataSource( dataSource: Pointer ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setDoubleSided( doubleSided: Boolean ); cdecl;
    function spineLocation: UIPageViewControllerSpineLocation; cdecl;
    function transitionStyle: UIPageViewControllerTransitionStyle; cdecl;
    function viewControllers: NSArray; cdecl;

    procedure setViewControllers( viewControllers: NSArray; direction: UIPageViewControllerNavigationDirection; animated: Boolean; completion: Pointer ); cdecl;
  end;

  TUIPageViewController = class( TOCGenericImport<UIPageViewControllerClass, UIPageViewController> )
  end;

  UIViewController1 = interface( UIViewController )
  end;

  // ----------------------------------------------------------------------------
  UIPageViewControllerDataSource = interface( IObjectiveC )
    ['{8CEBFF8B-4810-46E4-A0F4-BBD80D349096}']
    function pageViewController( pageViewController: UIPageViewController; viewControllerAfterViewController: UIViewController1 ): UIViewController; cdecl; overload;
    function pageViewController( pageViewController: UIPageViewController; viewControllerBeforeViewController: UIViewController ): UIViewController; cdecl; overload;
{$IFDEF IOS6}
    // function presentationCountForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl; // iOS 6.0 and later
    // function presentationIndexForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl; // iOS 6.0 and later
{$ENDIF}
  end;

  TDPFPageViewControllerDataSource = class( TOCLocal, UIPageViewControllerDataSource )
  private
    FDPFUIPageViewController: TDPFUIPageViewController;
  public
    constructor Create( ADPFUIPageViewController: TDPFUIPageViewController );

    function pageViewController( pageViewController: UIPageViewController; viewControllerAfterViewController: UIViewController1 ): UIViewController; overload; cdecl;
    function pageViewController( pageViewController: UIPageViewController; viewControllerBeforeViewController: UIViewController ): UIViewController; overload; cdecl;
{$IFDEF IOS6}
    // function presentationCountForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl; // iOS 6.0 and later
    // function presentationIndexForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl; // iOS 6.0 and later
{$ENDIF}
  end;

  UIPageViewControllerDelegate = interface( IObjectiveC )
    ['{B4F9E8DC-B488-4977-8253-BC43CE2ECE01}']
    // procedure pageViewController( pageViewController: UIPageViewController; didFinishAnimating: Boolean; previousViewControllers: NSArray; transitionCompleted: Boolean ); cdecl; overload;

    // function pageViewController( pageViewController: UIPageViewController; spineLocationForInterfaceOrientation: UIInterfaceOrientation ): UIPageViewControllerSpineLocation; cdecl; overload; // Raise Error !!!
  end;

  TDPFPageViewControllerDelegate = class( TOCLocal, UIPageViewControllerDelegate )
  private
    FDPFUIPageViewController: TDPFUIPageViewController;
  public
    constructor create( ADPFUIPageViewController: TDPFUIPageViewController );

    // procedure pageViewController( pageViewController: UIPageViewController; didFinishAnimating: Boolean; previousViewControllers: NSArray; transitionCompleted: Boolean ); overload; cdecl;
    // function pageViewController( pageViewController: UIPageViewController; spineLocationForInterfaceOrientation: UIInterfaceOrientation ): UIPageViewControllerSpineLocation; overload; cdecl; // Raise Error !!!
  end;

{$ENDIF}

  // ----------------------------------------------------------------------------
  // PageViewController Items
  TDPFPageViewControllerItem = class( TCollectionItem )
  private
    FOwner: TCollection;
    FTitle: string;
    FImage: string;
    FPage : TDPFUIViewController;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
  published
    property Title: string read FTitle write FTitle;
    property Image: string read FImage write FImage;
    property Page : TDPFUIViewController read FPage write FPage;

  end;

  // ----------------------------------------------------------------------------
  TDPFPageViewControllerCollection = class( TCollection )
  private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFPageViewControllerItem;
    procedure SetItem( Index: Integer; Value: TDPFPageViewControllerItem );

  public
    constructor create( AOwner: TComponent );
    destructor Destroy; override;

    function Add: TDPFPageViewControllerItem;
    function Insert( Index: Integer ): TDPFPageViewControllerItem;

    property Items[index: Integer]: TDPFPageViewControllerItem read GetItem write SetItem;
  end;

  // ----------------------------------------------------------------------------
  TDPFOnPageChanged = procedure( Sender: TObject; PrevPageIndex, NewPageIndex: Integer ) of object;

  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIPageViewController = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIPageViewController                        : UIPageViewController;
    FDPFPageViewControllerDataSource             : TDPFPageViewControllerDataSource;
    FDPFPageViewControllerDelegate               : TDPFPageViewControllerDelegate;
    UIPageViewControllerOptionSpineLocationKey   : NSString;
    UIPageViewControllerOptionInterPageSpacingKey: NSString;
    FNativePagesArray                            : NSMutableArray;
{$ENDIF}
    FSpineLocation        : TDPFPageViewControllerSpineLocation;
    FPages                : TDPFPageViewControllerCollection;
    FNavigationDirection  : TDPFPageViewControllerNavigationDirection;
    FActivePageIndex      : Integer;
    FActivePage           : TDPFUIViewController;
    FOption               : TDPFPageViewControllerOption;
    FOnPageChanged        : TDPFOnPageChanged;
    FTransitionStyle      : TDPFPageViewControllerTransitionStylePageCurl;
    FNavigationOrientation: TDPFPageViewControllerNavigationOrientation;
    procedure SetPages( const Value: TDPFPageViewControllerCollection );
    procedure SetActivePageIndex( const Value: Integer );

  protected
    procedure DirectionFinished( finished: boolean );
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure EnableScroll( isEnable: Boolean );
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex default -1;
    property ActivePage: TDPFUIViewController read FActivePage;
  published
    property NavigationDirection  : TDPFPageViewControllerNavigationDirection read FNavigationDirection write FNavigationDirection default TDPFPageViewControllerNavigationDirection.pvcndForward;
    property SpineLocation        : TDPFPageViewControllerSpineLocation read FSpineLocation write FSpineLocation default TDPFPageViewControllerSpineLocation.pvcslMax;
    property Option               : TDPFPageViewControllerOption read FOption write FOption default TDPFPageViewControllerOption.pvcoSpineLocationKey;
    property TransitionStyle      : TDPFPageViewControllerTransitionStylePageCurl read FTransitionStyle write FTransitionStyle default TDPFPageViewControllerTransitionStylePageCurl.tsPageCurl;
    property NavigationOrientation: TDPFPageViewControllerNavigationOrientation read FNavigationOrientation write FNavigationOrientation default TDPFPageViewControllerNavigationOrientation.vnoHorizontal;
    property Pages                : TDPFPageViewControllerCollection read FPages write SetPages;
    property OnPageChanged        : TDPFOnPageChanged read FOnPageChanged write FOnPageChanged;

    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

constructor TDPFUIPageViewController.create( AOwner: TComponent );
begin
  inherited create( AOwner );
  ControlCaption         := 'Page View Controller';
  FSpineLocation         := TDPFPageViewControllerSpineLocation.pvcslNone;
  FTransitionStyle       := TDPFPageViewControllerTransitionStylePageCurl.tsPageCurl;
  FNavigationOrientation := TDPFPageViewControllerNavigationOrientation.vnoHorizontal;
  AddSubViewToThis       := False;
  FActivePageIndex       := -1;
  FActivePage            := nil;
  FOption                := TDPFPageViewControllerOption.pvcoSpineLocationKey;
  FSpineLocation         := TDPFPageViewControllerSpineLocation.pvcslMax;

  FPages := TDPFPageViewControllerCollection.create( Self );

{$IFDEF IOS}
  FNativePagesArray                             := TNSMutableArray.Wrap( TNSMutableArray.Alloc.init );
  FDPFPageViewControllerDataSource              := TDPFPageViewControllerDataSource.create( Self );
  FDPFPageViewControllerDelegate                := TDPFPageViewControllerDelegate.create( Self );
  UIPageViewControllerOptionInterPageSpacingKey := CocoaNSStringConst( libUIKit, 'UIPageViewControllerOptionInterPageSpacingKey' );
  UIPageViewControllerOptionSpineLocationKey    := CocoaNSStringConst( libUIKit, 'UIPageViewControllerOptionSpineLocationKey' );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIPageViewController.Destroy;
begin
  FPages.DisposeOf;

{$IFDEF IOS}
  FDPFPageViewControllerDataSource.DisposeOf;
  FDPFPageViewControllerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageViewController.SetActivePageIndex( const Value: Integer );
{$IFDEF IOS}
var
  VC: NSArray;
{$ENDIF}
begin
  if ( Value < 0 ) or ( value > Pages.Count - 1 ) then
    exit;

{$IFDEF IOS}
  if Pages.Count > 0 then
  begin
    { if value > FActivePageIndex then
      Dir := Integer( UIPageViewControllerNavigationDirectionForward )
      else
      Dir       := Integer( UIPageViewControllerNavigationDirectionReverse ); }
    FActivePage := Pages.Items[value].Page;
    VC          := TNSArray.Wrap( TNSArray.OCClass.arrayWithObject( ( FActivePage.FUIViewController as ILocalObject ).GetObjectID ) );
    FUIPageViewController.setViewControllers( VC, Integer( FNavigationDirection ), true, nil );
    if Assigned( FOnPageChanged ) then
      FOnPageChanged( Self, FActivePageIndex, Value );
  end;
  FActivePageIndex := Value;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageViewController.SetPages( const Value: TDPFPageViewControllerCollection );
begin
  FPages.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageViewController.DirectionFinished( finished: boolean );
begin

end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageViewController.EnableScroll( isEnable: Boolean );
{$IFDEF IOS}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF IOS}
  for I := 0 to FUIPageViewController.gestureRecognizers.count - 1 do
    TUIGestureRecognizer.Wrap( FUIPageViewController.gestureRecognizers.objectAtIndex( i ) ).setEnabled( isEnable );

  for I := 0 to FUIPageViewController.view.subviews.count - 1 do
    TUIScrollView.Wrap( FUIPageViewController.view.subviews.objectAtIndex( i ) ).setScrollEnabled( isEnable );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUIPageViewController.Loaded;
var
  I        : Integer;
  PVOptions: NSDictionary;
  NSOption : NSString;
  V        : UIViewController;
begin

  if FUIPageViewController = nil then
  begin
    if FOption = TDPFPageViewControllerOption.pvcoSpineLocationKey then
      NSOption := UIPageViewControllerOptionSpineLocationKey
    else
      NSOption := UIPageViewControllerOptionInterPageSpacingKey;

    PVOptions             := TNSDictionary.Wrap( TNSDictionary.OCClass.dictionaryWithObject( TNSNumber.OCClass.numberWithInteger( Integer( FSpineLocation ) ), ( NSOption as ILocalObject ).GetObjectID ) );
    FUIPageViewController := TUIPageViewController.Wrap( TUIPageViewController.Alloc.initWithTransitionStyle( Integer( FTransitionStyle ), Integer( NavigationOrientation ), PVOptions ) );
    FUIControl            := FUIPageViewController;
  end;

  for I := 0 to Pages.Count - 1 do
  begin
    Pages.Items[i].Page.AddThisToSubView := False;
    v                                    := Pages.Items[i].Page.FUIViewController;
    FNativePagesArray.addObject( ( v as ILocalObject ).GetObjectID );
  end;

  FActivePage := nil;
  if FActivePageIndex = -1 then
    FActivePageIndex := 0;
  SetActivePageIndex( FActivePageIndex );

  FUIPageViewController.setDataSource( FDPFPageViewControllerDataSource.GetObjectID );
  FUIPageViewController.setDelegate( FDPFPageViewControllerDelegate.GetObjectID );
  FUIPageViewController.view.setFrame( CGRectMake( 0, 0, width * Scale.X, height * Scale.Y ) );

  ( TDPFiOSBaseControl( ParentControl ).UIControl as UIViewController ).addChildViewController( FUIPageViewController );
  ( TDPFiOSBaseControl( ParentControl ).UIControl as UIViewController ).view.addSubview( FUIPageViewController.View );
  FUIPageViewController.didMoveToParentViewController( ( TDPFiOSBaseControl( ParentControl ).UIControl as UIViewController ) );

  // AddSubView( Self, ParentControl, FUIPageViewController.view );
end;
// ------------------------------------------------------------------------------
{ TDPFPageViewControllerDataSource }

constructor TDPFPageViewControllerDataSource.Create( ADPFUIPageViewController: TDPFUIPageViewController );
begin
  inherited create;
  FDPFUIPageViewController := ADPFUIPageViewController;
end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerDataSource.pageViewController( pageViewController: UIPageViewController; viewControllerAfterViewController: UIViewController1 ): UIViewController;
var
  idx: NSUInteger;
begin
  result := nil;
  if FDPFUIPageViewController.FActivePageIndex < FDPFUIPageViewController.Pages.Count - 1 then
  begin
    idx := FDPFUIPageViewController.FNativePagesArray.indexOfObject( ( viewControllerAfterViewController as ILocalObject ).GetObjectID );
    if Assigned( FDPFUIPageViewController.FOnPageChanged ) then
      FDPFUIPageViewController.FOnPageChanged( FDPFUIPageViewController, FDPFUIPageViewController.FActivePageIndex, FDPFUIPageViewController.FActivePageIndex + 1 );

    if idx >= FDPFUIPageViewController.Pages.Count - 1 then
      exit;

    FDPFUIPageViewController.FActivePageIndex := idx;
    FDPFUIPageViewController.FActivePage      := FDPFUIPageViewController.Pages.Items[FDPFUIPageViewController.FActivePageIndex + 1].Page;
    result                                    := FDPFUIPageViewController.FActivePage.FUIViewController;
  end;

end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerDataSource.pageViewController( pageViewController: UIPageViewController; viewControllerBeforeViewController: UIViewController ): UIViewController;
var
  idx: NSUInteger;
begin
  result := nil;
  if FDPFUIPageViewController.FActivePageIndex > 0 then
  begin
    idx := FDPFUIPageViewController.FNativePagesArray.indexOfObject( ( viewControllerBeforeViewController as ILocalObject ).GetObjectID );
    if Assigned( FDPFUIPageViewController.FOnPageChanged ) then
      FDPFUIPageViewController.FOnPageChanged( FDPFUIPageViewController, FDPFUIPageViewController.FActivePageIndex, FDPFUIPageViewController.FActivePageIndex - 1 );

    if idx <= 0 then
      exit;

    FDPFUIPageViewController.FActivePageIndex := idx;
    FDPFUIPageViewController.FActivePage      := FDPFUIPageViewController.Pages.Items[FDPFUIPageViewController.FActivePageIndex - 1].Page;
    result                                    := FDPFUIPageViewController.FActivePage.FUIViewController;
  end;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS6}
(*
  // iOS 6.0 and later
  function TDPFPageViewControllerDataSource.presentationCountForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl;
  begin
  Result := FDPFUIPageViewController.Pages.Count;
  end;

  // ------------------------------------------------------------------------------
  // iOS 6.0 and later
  function TDPFPageViewControllerDataSource.presentationIndexForPageViewController( pageViewController: UIPageViewController ): NSInteger; cdecl;
  begin
  Result := 0;
  end;
*)
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFPageViewControllerDelegate }
constructor TDPFPageViewControllerDelegate.create( ADPFUIPageViewController: TDPFUIPageViewController );
begin
  inherited create;
  FDPFUIPageViewController := ADPFUIPageViewController;
end;

// ------------------------------------------------------------------------------
(* procedure TDPFPageViewControllerDelegate.pageViewController( pageViewController: UIPageViewController; didFinishAnimating: Boolean; previousViewControllers: NSArray; transitionCompleted: Boolean );
  begin

  end; *)

// ------------------------------------------------------------------------------
(* // Raise Error !!!
  function TDPFPageViewControllerDelegate.pageViewController( pageViewController: UIPageViewController; spineLocationForInterfaceOrientation: UIInterfaceOrientation ): UIPageViewControllerSpineLocation;
  begin
  Result := Integer( FDPFUIPageViewController.FSpineLocation );
  end;
*)
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFPageViewControllerItem }
constructor TDPFPageViewControllerItem.create( AOwner: TCollection );
begin
  inherited create( AOwner );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TDPFPageViewControllerItem.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerItem.GetDisplayName: string;
begin
  Result := Format( 'ViewControl %d', [index] );
end;

// ------------------------------------------------------------------------------
{ TDPFPageViewControllerCollection }
function TDPFPageViewControllerCollection.Add: TDPFPageViewControllerItem;
begin
  Result := inherited Add as TDPFPageViewControllerItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFPageViewControllerCollection.create( AOwner: TComponent );
begin
  inherited create( TDPFPageViewControllerItem );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TDPFPageViewControllerCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerCollection.GetItem( Index: Integer ): TDPFPageViewControllerItem;
begin
  Result := inherited Items[index] as TDPFPageViewControllerItem;
end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------------------------------------------------------------------
function TDPFPageViewControllerCollection.Insert( Index: Integer ): TDPFPageViewControllerItem;
begin
  Result := nil;
end;

// ------------------------------------------------------------------------------
procedure TDPFPageViewControllerCollection.SetItem( Index: Integer; Value: TDPFPageViewControllerItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
end.
