// ------------------------------------------------------------------------------
// DPF.iOS.UITabBarController Component
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
unit DPF.iOS.UITabBarController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

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
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  FMX.Menus,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFTabBarController = class;

{$IFDEF IOS}
  { NSArray1 = interface( NSArray )
    end;

    UIViewController1 = interface( UIViewController )
    end; }

  UITabBarClass = interface( UIViewClass )
    ['{39033095-9074-44DE-815C-E3E577443DB9}']
  end;

  UITabBar = interface( UIView )
    ['{22F48163-A921-49D4-9DB0-E7F60B2DEAF9}']
    function backgroundImage: UIImage; cdecl;
    procedure beginCustomizingItems( items: NSArray ); cdecl;
    function delegate: Pointer; cdecl;
    function endCustomizingAnimated( animated: Boolean ): Boolean; cdecl;
    function isCustomizing: Boolean; cdecl;
    function items: NSArray; cdecl;
    function selectedImageTintColor: UIColor; cdecl;
    function selectedItem: UITabBarItem; cdecl;
    function selectionIndicatorImage: UIImage; cdecl;
    procedure setBackgroundImage( backgroundImage: UIImage ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setItems( items: NSArray ); cdecl; overload;
    procedure setItems( items: NSArray; animated: Boolean ); cdecl; overload;
    procedure setSelectedImageTintColor( selectedImageTintColor: UIColor ); cdecl;
    procedure setSelectedItem( selectedItem: UITabBarItem ); cdecl;
    procedure setSelectionIndicatorImage( selectionIndicatorImage: UIImage ); cdecl;
    procedure setTintColor( tintColor: UIColor ); cdecl;
    function tintColor: UIColor; cdecl;
    procedure setBarTintColor( barTintColor: UIColor ); cdecl;
  end;

  TUITabBar = class( TOCGenericImport<UITabBarClass, UITabBar> )
  end;

  // -------------------------------------------------------------------
  UITabBarControllerClass = interface( UIViewControllerClass )
    ['{E87301BB-A23D-428C-B81E-09C5179BE826}']
  end;

  UITabBarController = interface( UIViewController )
    ['{A64CA5CC-3508-4B51-9678-6E1E5A62353B}']
    function customizableViewControllers: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function moreNavigationController: UINavigationController; cdecl;
    function selectedIndex: NSUInteger; cdecl;
    function selectedViewController: UIViewController; cdecl;
    procedure setCustomizableViewControllers( customizableViewControllers: NSArray ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setSelectedIndex( selectedIndex: NSUInteger ); cdecl;
    procedure setSelectedViewController( selectedViewController: UIViewController ); cdecl;
    procedure setViewControllers( viewControllers: NSArray ); cdecl; overload;
    procedure setViewControllers( viewControllers: NSArray; animated: Boolean ); cdecl; overload;
    function tabBar: UITabBar; cdecl;
    function viewControllers: NSArray; cdecl;
  end;

  TUITabBarController = class( TOCGenericImport<UITabBarControllerClass, UITabBarController> )
  end;

  // -------------------------------------------------------------------
  UITabBarControllerDelegate = interface( IObjectiveC )
    ['{F9D9EED8-9581-429B-ADEA-1C637959FE72}']
    // function tabBarController( tabBarController: UITabBarController; shouldSelectViewController: UIViewController ): Boolean; cdecl; overload;
    procedure tabBarController( tabBarController: UITabBarController; didSelectViewController: UIViewController ); cdecl; overload;
  end;

  TDPFTabBarControllerDelegate = class( TOCLocal, UITabBarControllerDelegate )
  private
    FDPFTabBarController: TDPFTabBarController;
  public
    constructor Create( AParent: TDPFTabBarController );

    // function tabBarController( tabBarController: UITabBarController; shouldSelectViewController: UIViewController ): Boolean; overload; cdecl;
    procedure tabBarController( tabBarController: UITabBarController; didSelectViewController: UIViewController ); overload; cdecl;

  end;
  // ----------------------------------------------------------------------------
  (*
    UITabBarDelegate = interface( IObjectiveC )
    ['{DEAF3E7F-9C2E-4E6F-BC49-E10FD800B5B9}']
    procedure tabBar( tabBar: UITabBar; didSelectItem: UITabBarItem ); cdecl;
    end;

    TDPFTabBarDelegate = class( TOCLocal, UITabBarDelegate )
    private
    FDPFTabBarController: TDPFTabBarController;
    public
    constructor Create( AParent: TDPFTabBarController );
    procedure tabBar( tabBar: UITabBar; didSelectItem: UITabBarItem ); cdecl;
    end;
  *)

{$ENDIF}

  TDPFTabBarSystemItem = ( tbsiNone, tbsiMore, tbsiFavorites, tbsiFeatured, tbsiTopRated, tbsiRecents, tbsiContacts, tbsiHistory, tbsiBookmarks, tbsiSearch, tbsiDownloads, tbsiMostRecent, tbsiMostViewed );

  TDPFOnSelectPage = procedure( Sender: TObject; SelectedIndex: Integer ) of object;

  // ----------------------------------------------------------------------------
  TDPFTabBar = class( TDPFiOSBaseControl )
  private
{$IFNDEF IOS}
    TabPositions: array of TRectF;
    TabWidth    : single;
    Tabs        : integer;
    LastIsMore  : boolean;
    Popup       : TPopupMenu;
    procedure GetTabPositions;
    procedure Clicked( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
    procedure PopupClicked( Sender: TObject );
{$ENDIF}
  public
{$IFNDEF IOS}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single ); override;
{$ENDIF}
  end;

  // ----------------------------------------------------------------------------
  TDPFTabBarProperty = class( TPersistent )
  private
    FDPFTabBarController: TDPFTabBarController;
    FBackgroundImage    : string;
    FAlpha              : Single;

    FVisible               : Boolean;
    FUserInteractionEnabled: Boolean;
    FTintColor             : TAlphaColor;
    FHeight                : Integer;
    FBarTintColor          : TAlphaColor;
    procedure SetAlpha( const Value: Single );
    procedure SetVisible( const Value: Boolean );
    procedure SetUserInteractionEnabled( const Value: Boolean );
    procedure SetTintColor( const Value: TAlphaColor );
    procedure setBarTintColor( const Value: TAlphaColor );
{$IFDEF IOS}
    procedure RealignTabBar( Visible: Boolean );
{$ENDIF}
  public
    constructor Create( ADPFTabBarController: TDPFTabBarController );
    destructor Destroy; override;

  published
    property Alpha                 : Single read FAlpha write SetAlpha;
    property BackgroundImage       : string read FBackgroundImage write FBackgroundImage;
    property TintColor             : TAlphaColor read FTintColor write SetTintColor default TAlphaColors.Black;
    property BarTintColor          : TAlphaColor read FBarTintColor write setBarTintColor default TAlphaColors.Null;
    property Visible               : Boolean read FVisible write SetVisible default true;
    property UserInteractionEnabled: Boolean read FUserInteractionEnabled write SetUserInteractionEnabled default true;
  end;

  // ----------------------------------------------------------------------------
  // TDPFTabBarItem
  TDPFTabBarItem = class( TDPFUIViewController )
  private
{$IFDEF IOS}
    FUITabBarItem: UITabBarItem;
{$ENDIF}
    FItemTitle          : string;
    FItemImage          : string;
    FTabBarSystemItem   : TDPFTabBarSystemItem;
    FBadgeValue         : string;
    FPageControl        : TDPFTabBarController;
    FAnimationTransition: TDPFViewAnimationTransition;
    FAnimationDuration  : Double;
    FPageIndexInitValue : Integer;

    procedure SetPageControl( const APageControl: TDPFTabBarController );
    function GetPageIndex: Integer;
    procedure SetPageIndex( const Value: Integer );
    procedure SetBadgeValue( const Value: string );
    procedure SetItemTitle( const Value: string );
    procedure SetTabBarSystemItem( const Value: TDPFTabBarSystemItem ); // Added by Fenistil

  protected
    procedure ReadState( Reader: TReader ); override;
  public
    procedure Loaded; override;
{$IFNDEF IOS}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    property PageControl: TDPFTabBarController read FPageControl write SetPageControl;
  published
    property PageIndex       : Integer read GetPageIndex write SetPageIndex stored false;
    property ItemTitle       : string read FItemTitle write SetItemTitle;
    property ItemImage       : string read FItemImage write FItemImage;
    property BadgeValue      : string read FBadgeValue write SetBadgeValue;
    property TabBarSystemItem: TDPFTabBarSystemItem read FTabBarSystemItem write SetTabBarSystemItem default tbsiNone;

    property AnimationTransition: TDPFViewAnimationTransition read FAnimationTransition write FAnimationTransition default vatNone;
    property AnimationDuration  : Double read FAnimationDuration write FAnimationDuration;

    property Visible stored False;
    property Top stored false;
    property Height stored false;
    property Left stored false;
    property Width stored false;
  end;

  // ------------------------------------------------------------------------------
  TDPFOnChangingPage = procedure( Sender: TObject; FromIndedx: Integer; ToIndex: Integer; var CanChange: Boolean ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFTabBarController = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUITabBarController: UITabBarController;
    // FDPFTabBarDelegate          : TDPFTabBarDelegate;
    FDPFTabBarControllerDelegate: TDPFTabBarControllerDelegate;

{$ENDIF}
    FBar            : TDPFTabBar;
    FPages          : TList<TDPFTabBarItem>;
    FActivePage     : TDPFTabBarItem;
    FOnSelectPage   : TDPFOnSelectPage;
    FTabBarPanel    : TDPFTabBarProperty;
    FOnChangingPage : TDPFOnChangingPage;
    FActivePageIndex: Integer;
    FInSetActivePage: Boolean;
    procedure ChangeActivePage( APage: TDPFTabBarItem );
    function GetPage( index: Integer ): TDPFTabBarItem;
    function GetPageCount: Integer;
    procedure SetActivePage( const Page: TDPFTabBarItem );

    procedure SetActivePageIndex( const Value: Integer );

    procedure SetTabBarPanel( const Value: TDPFTabBarProperty );
    function GetActivePage: TDPFTabBarItem;
    procedure UpdateActiveTab;
  protected

{$IFNDEF IOS}
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single ); override;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;
  public
    // {$IFDEF IOS}
    procedure Loaded; override;
    // {$ELSE}
    // {$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFNDEF IOS}
    procedure Change;
{$ENDIF}
    procedure RemovePage( Page: TDPFTabBarItem );
    procedure InsertPage( Page: TDPFTabBarItem );

    property Pages[index: Integer]: TDPFTabBarItem read GetPage;
    property PageCount: Integer read GetPageCount;
  published
    property ActivePage     : TDPFTabBarItem read GetActivePage write SetActivePage stored false;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex;
    property TabBarPanel    : TDPFTabBarProperty read FTabBarPanel write SetTabBarPanel;
    property OnSelectPage   : TDPFOnSelectPage read FOnSelectPage write FOnSelectPage;
    property OnChangingPage : TDPFOnChangingPage read FOnChangingPage write FOnChangingPage;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFTabBarItem }

constructor TDPFTabBarItem.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FPageIndexInitValue  := -1;
  ControlCaption       := name;
  FBadgeValue          := '';
  FTabBarSystemItem    := tbsiNone;
  Align                := TAlignLayout.Client;
  FAnimationTransition := vatNone;
  FAnimationDuration   := 1;
{$IFDEF IOS}
  Visible := True;
{$ELSE}
  Visible := False;
{$ENDIF}
  SetSubComponent( true );
  if AOwner is TDPFTabBarController then
    PageControl    := TDPFTabBarController( AOwner );
  AddThisToSubView := False;
{$IFDEF IOS}
  // FUIControl := TUIViewController.Wrap( TUIViewController.Alloc.Init );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFTabBarItem.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFTabBarItem.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf( Self )
  else
    Result := -1;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.Loaded;
begin
  inherited;
  // if ( name <> '' ) and ( ParentControl is TDPFTabBarController ) then
  // PageControl := TDPFTabBarController( ParentControl );
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

procedure TDPFTabBarItem.ReadState( Reader: TReader );
begin
  inherited;
  if Reader.Parent is TDPFTabBarController then
    PageControl := TDPFTabBarController( Reader.Parent );
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.SetBadgeValue( const Value: string );
begin
  FBadgeValue := Value;
{$IFDEF IOS}
  if Assigned( FUITabBarItem ) then
  begin
    if Value = '' then
      FUITabBarItem.setBadgeValue( nil )
    else
      FUITabBarItem.setBadgeValue( NSStr( Value ) );
  end
{$ELSE} // Added by Fenistil
  ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.SetPageControl( const APageControl: TDPFTabBarController );
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then
    begin
{$IFNDEF IOS} // Added by Fenistil
      ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
      FPageControl.RemovePage( Self );
    end;
    Parent := APageControl;
    if APageControl <> nil then
    begin
{$IFNDEF IOS} // Added by Fenistil
      ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
      APageControl.InsertPage( Self );
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.SetPageIndex( const Value: Integer );
var
  I, MaxPageIndex: Integer;
begin
  if FPageControl = nil then
    FPageIndexInitValue := Value;

  if ( FPageControl <> nil ) { and ( csDesigning in ComponentState ) } then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.Create( 'TDPFTabBarItem.SetPageIndex: ' + IntToStr( Value ) );

    FPageControl.FPages.Move( PageIndex, Value );

    for I := 0 to FPageControl.FPages.Count - 1 do
    begin
      TDPFTabBarItem( FPageControl.FPages[I] ).index := I;
    end;

  end;
{$IFNDEF IOS}  // Added by Fenistil
  ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
end;

procedure TDPFTabBarItem.SetTabBarSystemItem( const Value: TDPFTabBarSystemItem );
begin
  FTabBarSystemItem := Value;
{$IFNDEF IOS}
  ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarItem.SetItemTitle( const Value: string );
begin
  FItemTitle := Value;
{$IFDEF IOS}
  if Assigned( FUITabBarItem ) then
  begin
    if FItemTitle = '' then
      FUITabBarItem.setTitle( nil )
    else
      FUITabBarItem.setTitle( NSStr( FItemTitle ) );
  end;
{$ELSE}  // Added by Fenistil
  ( Parent as TDPFTabBarController ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFTabBarController }
// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFTabBarController.Change;
var
  Form: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    Form := TCustomForm( Application.MainForm );
    if ( Form <> nil ) and ( Form.Designer <> nil ) then
    begin
      Form.Designer.Modified;
      ShowMessage( 'Changed!' );
    end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.ChangeActivePage( APage: TDPFTabBarItem );
begin
  if FActivePage <> APage then
  begin
    if APage <> nil then
    begin
      APage.BringToFront;
{$IFNDEF IOS}
      APage.Visible := True;
{$ENDIF}
    end;
{$IFNDEF IOS}
    if FActivePage <> nil then
      FActivePage.Visible := False;
{$ENDIF}
    FActivePage := APage;
  end;
end;

constructor TDPFTabBarController.Create( AOwner: TComponent );
{$IFDEF IOS}
// var GU: TGUID;
{$ENDIF}
begin
  inherited Create( AOwner );
{$IFNDEF IOS}
  FDesignInteractive := True;
{$ENDIF}
  ControlCaption := 'TabBarController';

  FPages              := TList<TDPFTabBarItem>.Create;
  FTabBarPanel        := TDPFTabBarProperty.Create( Self );
  FBar                := TDPFTabBar.Create( Self );
  FBar.Parent         := Self;
  FBar.Height         := 49; // Added by Fenistil
  FBar.Align          := TAlignLayout.Bottom;
  FBar.Locked         := true;
  FBar.Stored         := false;
  FBar.ControlCaption := 'Tab Bar';
  FBar.BringToFront;
  Self.AddObject( FBar );

  FActivePageIndex := -1;

{$IFDEF IOS}
  // FDPFTabBarDelegate           := TDPFTabBarDelegate.Create( Self );
  FDPFTabBarControllerDelegate := TDPFTabBarControllerDelegate.Create( Self );
  FUITabBarController          := TUITabBarController.Create;
  FUIControl                   := FUITabBarController.view;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFTabBarController.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
  begin
    FPages[I].FPageControl := nil;
  end;
  FPages.DisposeOf;
  FTabBarPanel.DisposeOf;
  FBar.DisposeOf;

{$IFDEF IOS}
  if Assigned( FDPFTabBarControllerDelegate ) then
    FDPFTabBarControllerDelegate.DisposeOf;
  // FDPFTabBarDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFTabBarController.GetActivePage: TDPFTabBarItem;
begin
  Result := nil;
  if ( FActivePageIndex > -1 ) and ( FActivePageIndex < FPages.Count ) then
    Result := FPages[ActivePageIndex];
end;

procedure TDPFTabBarController.GetChildren( Proc: TGetChildProc; Root: TComponent );
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc( TComponent( FPages[I] ) );
end;

// ------------------------------------------------------------------------------
function TDPFTabBarController.GetPage( index: Integer ): TDPFTabBarItem;
begin
  Result := nil;
  if ( index > 0 ) or ( index < FPages.Count ) then
    Result := FPages[index];
end;

// ------------------------------------------------------------------------------
function TDPFTabBarController.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.InsertPage( Page: TDPFTabBarItem );
begin
  if ( csLoading in ComponentState ) then
    // {$IFDEF DELPHIXE5}
    // FPages.Insert( 0, Page )
    // {$ELSE}
    FPages.Add( Page )
    // {$ENDIF}
  else
    FPages.Add( Page );

  Page.FPageControl := Self;

  if FBar.Parent <> Self then
    FBar.Parent := Self;

  // Page.UpdateTabShowing;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFTabBarController.Loaded;
var
  FTabForms  : NSMutableArray;
  FTabBarItem: TDPFTabBarItem;
  TBI        : UITabBarItem;
  VC         : UIViewController;
  I          : Integer;
  Image1     : UIImage;
begin
  I := 0;
  while I < PageCount do
  begin
    FTabBarItem := Pages[I];
    if ( FTabBarItem.PageIndex <> FTabBarItem.FPageIndexInitValue ) and ( FTabBarItem.FPageIndexInitValue <> -1 ) then
    begin
      FTabBarItem.SetPageIndex( FTabBarItem.FPageIndexInitValue );
      I := 0;
      Continue;
    end;
    Inc( I );
  end;

  FUITabBarController.view.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
  TabBarPanel.SetVisible( TabBarPanel.Visible );
  TabBarPanel.RealignTabBar( TabBarPanel.Visible );

  if ActivePageIndex < 0 then
    ActivePageIndex := 0;

  FTabForms := TNSMutableArray.Wrap( TNSMutableArray.Alloc.Init );

  for I := 0 to PageCount - 1 do
  begin
    FTabBarItem := Pages[I];
    if FTabBarItem.TabBarSystemItem <> tbsiNone then
      TBI := TUITabBarItem.Wrap( TUITabBarItem.Alloc.InitWithTabBarSystemItem( LongInt( FTabBarItem.TabBarSystemItem ) - 1, 0 ) )
    else
      TBI := TUITabBarItem.Wrap( TUITabBarItem.Alloc.InitWithTitle( NSStr( FTabBarItem.ItemTitle ), nil, 0 ) );
    TBI.SetTitle( NSStr( FTabBarItem.ItemTitle ) );
    FTabBarItem.FUITabBarItem := TBI;

    if FTabBarItem.ItemImage <> '' then
    begin
      Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FTabBarItem.ItemImage ) ) );
      TBI.setImage( Image1 );
      Image1 := nil;
    end;

    FTabBarItem.SetBadgeValue( FTabBarItem.BadgeValue );
    FTabBarItem.SetItemTitle( FTabBarItem.ItemTitle );

    VC := UIViewController( FTabBarItem.FUIControl );
    // VC.retain ;

    VC.SetTabBarItem( TBI );

    FTabForms.AddObject( ( VC as ILocalObject ).GetObjectID );
  end;

  FTabBarPanel.setBarTintColor( FTabBarPanel.FBarTintColor );

  if FTabBarPanel.BackgroundImage <> '' then
  begin
    Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FTabBarPanel.BackgroundImage ) ) );
    FUITabBarController.tabBar.setBackgroundImage( Image1 );
    Image1 := nil;
  end;

  FUITabBarController.SetWantsFullScreenLayout( true );
  FUITabBarController.SetViewControllers( FTabForms, true );

  FTabForms.release;

  FUITabBarController.setDelegate( ( FDPFTabBarControllerDelegate as ILocalObject ).GetObjectID );

  TabBarPanel.SetUserInteractionEnabled( TabBarPanel.FUserInteractionEnabled );
  TabBarPanel.SetTintColor( TabBarPanel.FTintColor );
  TabBarPanel.SetAlpha( TabBarPanel.FAlpha );

  // FUITabBarController.tabBar.setBackgroundColor( TColorToUIColor( TAlphaColors.Red ) );
  // FUITabBarController.tabBar.setSelectedImageTintColor(TColorToUIColor( TAlphaColors.Red ) );

  SetActivePageIndex( FActivePageIndex );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
  UpdateActiveTab;
end;
{$ELSE}

procedure TDPFTabBarController.Loaded;
begin
  inherited;
  UpdateActiveTab;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.RemovePage( Page: TDPFTabBarItem );
begin
  Page.FPageControl := nil;
  FPages.Remove( Page );
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUITabBarController <> nil then
  begin
    FUITabBarController.view.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
    TabBarPanel.RealignTabBar( TabBarPanel.FVisible );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.SetActivePage( const Page: TDPFTabBarItem );
begin
  FInSetActivePage := True;
  try
    ChangeActivePage( Page );
    if Page = nil then
    begin
      if ActivePageIndex <> -1 then
        ActivePageIndex := -1;
      Exit;
    end
    else
      FActivePageIndex := Page.PageIndex;
    // if Page = FActivePage then
    // Exit;
    //
    // if Page.FPageControl <> Self then
    // Exit;
    // Page.BringToFront;
    //
    if Assigned( Application ) and Assigned( Application.MainForm ) then
      TCustomForm( Application.MainForm ).ActiveControl := Page;

    // ActivePageIndex := Page.PageIndex;
    // FActivePage     := Page;

{$IFDEF IOS}
    if not TabBarPanel.Visible then
    begin
      SetAnimationTransition( ActivePage.AnimationTransition );
      if Assigned( FOnSelectPage ) then
        OnSelectPage( Self, FActivePageIndex );
    end;
    FUITabBarController.setSelectedIndex( FActivePageIndex );
{$ELSE}
    // Added by Fenistil
    Invalidate;
    // FActivePage.BringToFront;
{$ENDIF}
  finally
    FInSetActivepage := False;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.SetActivePageIndex( const Value: Integer );
begin
  if FActivePageIndex = Value then
    Exit;

  FActivePageIndex := Value;

  if FInSetActivePage or not InRange( Value, 0, FPages.Count - 1 ) then
    Exit;

  SetActivePage( FPages[Value] );

  (* exit;
    if FActivePageIndex = Value then
    exit;

    FActivePageIndex := Value;
    if ( Value > -1 ) and ( Value < PageCount ) then
    begin
    ActivePage := FPages[Value];
    {$IFDEF IOS}
    if not TabBarPanel.Visible then
    begin
    SetAnimationTransition( ActivePage.AnimationTransition );
    if Assigned( FOnSelectPage ) then
    OnSelectPage( Self, Value );
    end;
    FUITabBarController.setSelectedIndex( Value );
    {$ELSE}
    // Added by Fenistil
    Invalidate;
    FActivePage.BringToFront;
    {$ENDIF}
    end
    else
    ActivePage := nil; *)
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.SetTabBarPanel( const Value: TDPFTabBarProperty );
begin
  FTabBarPanel.Assign( Value );
end;

procedure TDPFTabBarController.UpdateActiveTab;
begin
  if InRange( ActivePageIndex, 0, FPages.Count - 1 ) then
    SetActivePage( FPages[ActivePageIndex] )
  else
    SetActivePage( nil );
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFTabBarController.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  if Y >= FBar.Top then
    FBar.Clicked( Button, Shift, X, Y - FBar.Top );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTabBarController.Move;
begin
  Resize;
end;

{$IFDEF IOS}
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFTabBarProperty }
constructor TDPFTabBarProperty.Create( ADPFTabBarController: TDPFTabBarController );
begin
  inherited Create;
  FDPFTabBarController    := ADPFTabBarController;
  FTintColor              := TAlphaColors.Black;
  FBarTintColor           := TAlphaColors.White;
  FVisible                := true;
  FUserInteractionEnabled := true;
  FAlpha                  := 1;
  FHeight                 := 49;
end;

// ------------------------------------------------------------------------------
destructor TDPFTabBarProperty.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetAlpha( const Value: Single );
begin
  FAlpha := Value;
{$IFDEF IOS}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FUITabBarController <> nil ) then
    FDPFTabBarController.FUITabBarController.tabBar.SetAlpha( FAlpha );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.setBarTintColor( const Value: TAlphaColor );
begin
  FBarTintColor := Value;
{$IFDEF IOS}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FUITabBarController <> nil ) then
    FDPFTabBarController.FUITabBarController.tabBar.setBarTintColor( TColorToUIColor( FBarTintColor ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetTintColor( const Value: TAlphaColor );
begin
  FTintColor := Value;
{$IFDEF IOS}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FUITabBarController <> nil ) then
    FDPFTabBarController.FUITabBarController.tabBar.setTintColor( TColorToUIColor( FTintColor ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetUserInteractionEnabled( const Value: Boolean );
begin
  FUserInteractionEnabled := Value;
{$IFDEF IOS}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FUITabBarController <> nil ) then
    FDPFTabBarController.FUITabBarController.tabBar.setUserInteractionEnabled( FUserInteractionEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFTabBarProperty.RealignTabBar( Visible: Boolean );
var
  vFrame, tFrame: CGRect;
  isVisible     : Boolean;
begin
  vFrame    := FDPFTabBarController.FUITabBarController.view.Frame;
  tFrame    := FDPFTabBarController.FUITabBarController.tabBar.Frame;
  isVisible := vFrame.size.Height <= FDPFTabBarController.Height;
  if isVisible <> Visible then
  begin
    if isVisible then
      FDPFTabBarController.FUITabBarController.view.setFrame( CGRectMake( vFrame.origin.X, vFrame.origin.Y, vFrame.size.Width * FDPFTabBarController.Scale.X, ( vFrame.size.Height + tFrame.size.Height ) * FDPFTabBarController.Scale.Y ) )
    else
      FDPFTabBarController.FUITabBarController.view.setFrame( CGRectMake( vFrame.origin.X, vFrame.origin.Y, vFrame.size.Width * FDPFTabBarController.Scale.X, ( vFrame.size.Height - tFrame.size.Height ) * FDPFTabBarController.Scale.Y ) );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetVisible( const Value: Boolean );
begin
  if FVisible = Value then
    Exit;

  FDPFTabBarController.FBar.Visible := Value;

{$IFDEF IOS}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FUITabBarController <> nil ) then
  begin
    FDPFTabBarController.FUITabBarController.tabBar.setHidden( not Value );
    RealignTabBar( Value );
  end;
  if Value then
    FDPFTabBarController.FBar.Height := FDPFTabBarController.FUITabBarController.tabBar.Frame.size.Height
  else
    FDPFTabBarController.FBar.Height := 0;
{$ELSE}
  if Value then
    FDPFTabBarController.FBar.Height := 49
  else
    FDPFTabBarController.FBar.Height := 0;
{$ENDIF}
  FVisible := Value;
  FDPFTabBarController.Realign;
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFTabBarControllerDelegate }

constructor TDPFTabBarControllerDelegate.Create( AParent: TDPFTabBarController );
begin
  inherited Create;
  FDPFTabBarController := AParent;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarControllerDelegate.TabBarController( tabBarController: UITabBarController; didSelectViewController: UIViewController );
var
  Idx: Integer;
begin
  Idx := didSelectViewController.tabBarController.SelectedIndex;

  FDPFTabBarController.FActivePage      := FDPFTabBarController.Pages[Idx];
  FDPFTabBarController.FActivePageIndex := Idx;

  if Assigned( FDPFTabBarController.FOnSelectPage ) then
    FDPFTabBarController.OnSelectPage( FDPFTabBarController, Idx );
end;

// ------------------------------------------------------------------------------
(* function TDPFTabBarControllerDelegate.tabBarController( tabBarController: UITabBarController; shouldSelectViewController: UIViewController ): Boolean;
  var
  currentVC         : UIViewController;
  fromIndex, ToIndex: Integer;
  TB                : TDPFTabBarItem;
  begin
  Result    := true;
  fromIndex := tabBarController.SelectedIndex;
  ToIndex   := tabBarController.viewControllers.indexOfObject( ( shouldSelectViewController as ILocalObject ).GetObjectID );
  currentVC := tabBarController.selectedViewController;

  if fromIndex = ToIndex then
  begin
  Result := false;
  Exit;
  end;

  TB := FDPFTabBarController.Pages[tabBarController.SelectedIndex];

  if Assigned( FDPFTabBarController.FOnChangingPage ) then
  FDPFTabBarController.FOnChangingPage( FDPFTabBarController, tabBarController.SelectedIndex, ToIndex, Result );

  if Result and ( TB.AnimationTransition <> vatNone ) then
  begin
  Result := true;
  TUIView.OCClass.beginAnimations( nil, nil );
  TUIView.OCClass.setAnimationDuration( TB.FAnimationDuration );
  TUIView.OCClass.SetAnimationTransition( Cardinal( TB.AnimationTransition ), tabBarController.view, true );
  currentVC.viewWillAppear( true );
  shouldSelectViewController.viewWillDisappear( true );
  shouldSelectViewController.viewDidDisappear( true );
  currentVC.viewDidAppear( true );
  TUIView.OCClass.commitAnimations;
  end;

  if Assigned( FDPFTabBarController.FOnSelectPage ) then
  FDPFTabBarController.OnSelectPage( FDPFTabBarController, ToIndex );

  end;
*)
// ------------------------------------------------------------------------------

{$ENDIF}
{ TDPFTabBar }

{$IFNDEF IOS}

constructor TDPFTabBar.Create( AOwner: TComponent );
begin
  inherited;
  Popup := TPopupMenu.Create( nil );
end;

destructor TDPFTabBar.Destroy;
begin
  Popup.Clear;
  Popup.Free;
  inherited;
end;

procedure TDPFTabBar.GetTabPositions;
var
  i         : integer;
  x         : single;
  TabControl: TDPFTabBarController;
  MaxTabs   : integer;

begin
  TabControl := Parent as TDPFTabBarController;
  MaxTabs    := Round( TabControl.Width ) div 64;
  if TabControl.PageCount > MaxTabs then
  begin
    Tabs       := MaxTabs;
    LastIsMore := true;
  end
  else
  begin
    Tabs       := TabControl.PageCount;
    LastIsMore := false;
  end;
  SetLength( TabPositions, Tabs );
  if Tabs = 0 then
  begin
    TabWidth := Self.Width;
    Exit;
  end;
  TabWidth := Width / Tabs;
  x        := 0;
  for i    := 0 to Tabs - 1 do
  begin
    TabPositions[i].Left   := x;
    TabPositions[i].Top    := 0;
    TabPositions[i].Bottom := Height;
    TabPositions[i].Right  := x + TabWidth;
    x                      := x + TabWidth;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  if csDesigning in ComponentState then
    inherited
  else
    Clicked( Button, Shift, X, Y );
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBar.Clicked( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
var
  i, c      : integer;
  mi        : TMenuItem;
  TabControl: TDPFTabBarController;

begin
  GetTabPositions;
  c     := -1;
  for i := 0 to Tabs - 1 do
  begin
    if ( X >= TabPositions[i].Left ) and ( X <= TabPositions[i].Right ) then
    begin
      c := i;
      break;
    end;
  end;
  if c = -1 then
    Exit;
  if ( c = Tabs - 1 ) and LastIsMore then
  begin
    Popup.Clear;
    TabControl := Parent as TDPFTabBarController;
    for i      := c to TabControl.PageCount - 1 do
    begin
      mi := TMenuItem.Create( nil );
      if TabControl.Pages[i].ItemTitle <> '' then
        mi.Text := TabControl.Pages[i].ItemTitle
      else
        mi.Text  := TabControl.Pages[i].Name;
      mi.Tag     := i;
      mi.OnClick := PopupClicked;
      Popup.AddObject( mi );
    end;
    Popup.Popup( Screen.MousePos.X, Screen.MousePos.Y );
  end
  else
    ( Parent as TDPFTabBarController ).SetActivePageIndex( c );
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBar.Paint;
var
  TabControl: TDPFTabBarController;
  Caption, s: string;
  Bmp       : TBitmap;
  SystemItem: TDPFTabBarSystemItem;
  i         : integer;
  x, w      : single;
  R         : TRectF;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  TabControl := Parent as TDPFTabBarController;
  // Background
  BitmapAsBackground( Self, iOS_GUI_Bitmaps.TabBar.BG );
  // Tabs
  GetTabPositions;
  if Tabs = 0 then
  begin
    Canvas.EndScene;
    Exit;
  end;
  for i := 0 to Tabs - 1 do
  begin
    Bmp     := nil;
    Caption := '';
    if ( i = Tabs - 1 ) and LastIsMore then
      SystemItem := tbsiMore
    else
      SystemItem := TabControl.Pages[i].TabBarSystemItem;
    case SystemItem of
      tbsiNone:
        Caption := TabControl.Pages[i].ItemTitle;
      tbsiBookmarks:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Bookmark_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Bookmark_Off;
          Caption := 'Bookmarks';
        end;
      tbsiContacts:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Contacts_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Contacts_Off;
          Caption := 'Contacts';
        end;
      tbsiDownloads:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Downloads_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Downloads_Off;
          Caption := 'Downloads';
        end;
      tbsiFavorites:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Favorites_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Favorites_Off;
          Caption := 'Favorites';
        end;
      tbsiFeatured:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Featured_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Featured_Off;
          Caption := 'Featured';
        end;
      tbsiHistory:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.History_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.History_Off;
          Caption := 'History';
        end;
      tbsiMore:
        begin
          Bmp     := iOS_GUI_Bitmaps.TabBar.More_Off;
          Caption := 'More';
        end;
      tbsiMostRecent:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.MostRecent_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.MostRecent_Off;
          Caption := 'Most Recent';
        end;
      tbsiMostViewed:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.MostViewed_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.MostViewed_Off;
          Caption := 'Most Viewed';
        end;
      tbsiRecents:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.History_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.History_Off;
          Caption := 'Recents';
        end;
      tbsiSearch:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Search_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Search_Off;
          Caption := 'Search';
        end;
      tbsiTopRated:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Favorites_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Favorites_Off;
          Caption := 'Top Rated';
        end;
    end;
    if Bmp <> nil then
      BitmapToPosition( Self, Bmp, TabPositions[i].Left + TabWidth / 2 - Bmp.Width / 2, 3 );
    Canvas.Font.Size  := 9;
    Canvas.Font.Style := [TFontStyle.fsBold];
    if SystemItem = tbsiMore then
      Canvas.Fill.Color := $FF999999
    else
    begin
      if TabControl.ActivePageIndex = i then
        Canvas.Fill.Color := TAlphaColors.White
      else
        Canvas.Fill.Color := $FF999999;
    end;
    Canvas.Font.Family := 'Helvetica';
    PaintCaption( Self, Caption, RectF( TabPositions[i].Left, 35, TabPositions[i].Right, Height ), lbMiddleTruncation, 1, TTextAlign.Center );

    // Badge
    if SystemItem <> tbsiMore then
    begin
      s := TabControl.Pages[i].BadgeValue;
      if s <> '' then
      begin
        Canvas.Font.Size   := 9;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        Canvas.Fill.Color  := $FFFFFFFF;
        if Length( s ) = 1 then
          w := 22
        else
          w := Canvas.TextWidth( s ) + 16;
        x   := Round( TabPositions[i].Right - 14 - ( w / 2 ) );
        BitmapToPosition( Self, iOS_GUI_Bitmaps.TabBar.BadgeLeft, x, 2 );
        R.Left   := x + iOS_GUI_Bitmaps.TabBar.BadgeLeft.Width;
        R.Top    := 2;
        R.Bottom := 2 + iOS_GUI_Bitmaps.TabBar.BadgeBG.Height;
        R.Right  := x + w - iOS_GUI_Bitmaps.TabBar.BadgeRight.Width;
        BitmapToRect( Self, iOS_GUI_Bitmaps.TabBar.BadgeBG, R );
        BitmapToPosition( Self, iOS_GUI_Bitmaps.TabBar.BadgeRight, x + w - iOS_GUI_Bitmaps.TabBar.BadgeRight.Width, 2 );
        R.Left  := x;
        R.Right := x + w;
        R.Top   := 7;
        Canvas.FillText( R, s, false, 1, [], TTextAlign.Center, TTextAlign.Leading );
      end;
    end;
  end;
  Canvas.EndScene;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBar.PopupClicked( Sender: TObject );
begin
  ( Parent as TDPFTabBarController ).SetActivePageIndex( ( Sender as TMenuItem ).Tag );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
