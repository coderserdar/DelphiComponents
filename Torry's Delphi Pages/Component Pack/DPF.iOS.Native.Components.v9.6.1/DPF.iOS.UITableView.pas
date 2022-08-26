// ------------------------------------------------------------------------------
// DPF.iOS.UITableView Component
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
unit DPF.iOS.UITableView;

interface

{$DEFINE DEBUG}
{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.TypInfo,
  System.DateUtils,
  System.StrUtils,
  System.Types,
  System.Generics.Collections,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UIImageView,
  DPF.iOS.UIFont,
  DPF.iOS.Common,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  Macapi.Helpers,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  iOSapi.QuartzCore,
  FMX.Platform.iOS,
  DPF.iOS.Classes,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  System.Math,
  FMX.Types,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Dialogs,
  FMX.Forms;

// {$M+}
const
  TAG_BASE                  = 10000;
  ROTATE_90_ClockwiseAngle  = -( 90 * PI / 180.0 );
  ROTATE_90_ClockwiseAngleN = +( 90 * PI / 180.0 );

const
  LEFT_MENU_WIDTH = 285;

type

  TDPFUITableView = class;

  TScrollPoint = DPFNSPoint;

  TDPFTableViewStyle         = ( tvsPlain, tvsGrouped );
  TDPFGestureRecognizerState = TDPFPanGestureRecognizeState;

  TDPFTableViewIndicatorStyle = ( tviDefault, tviBlack, tviWhite );

  TDPFTableViewSeparatorStyle = ( tssNone, tssSingleLine, tssSingleLineEtched );

  TDPFTableViewSelectionStyle = ( tcsNone, tcsBlue, tcsGray );

  TDPFTableViewSearchKind = ( skStartsWith, skContains );

  DPFTableViewScrollPosition = ( spNone = 0, spMiddle = 2, spTop = 1, spBottom = 3 );

  TDPFTableViewItemOnDeSelect = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; var CellAccessory: TTableViewCellAccessory ) of object;
  TDPFTableViewItemOnSelect   = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; var CellAccessory: TTableViewCellAccessory ) of object;
  TDPFTableViewItemOnInsert   = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt ) of object;
  TDPFTableViewItemOnDelete   = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt ) of object;
  TDPFTableViewItemOnEdit     = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt ) of object;

  TDPFTableViewOnGetNumberOfSections      = procedure( Sender: TObject; inSerachMode: Boolean; var NumberOfSections: LongInt ) of object;
  TDPFTableViewOnGetNumberOfRowsInSection = procedure( Sender: TObject; inSerachMode: Boolean; SectionNo: LongInt; var numberOfRowsInSection: LongInt ) of object;

  TDPFTableViewOnNeedCellIdentifier = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; var CellIdentifier: string ) of object;

  TDPFTableViewItemOnAccessoryButtonTapped = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt ) of object;

  TDPFTableViewItemOnDrawCell       = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; TableItem: TTableItem; var Objects: TArray<TDPFiOSBaseControl>; var Handled: Boolean ) of object;
  TDPFTableViewItemOnApplyFrameData = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; TableItem: TTableItem; var Frame: TFrame; var Handled: Boolean ) of object;
  TDPFTableViewItemOnGetRowHeight   = procedure( Sender: TObject; Section: LongInt; RowNo: LongInt; var RowHeight: Single ) of object;

  TDPFTableViewSectionCount    = procedure( Sender: TObject; var SectionCount: LongInt ) of object;
  TDPFTableViewSectionRowCount = procedure( Sender: TObject; Section: LongInt; var RowCount: LongInt ) of object;

  TDPFTableViewScrollBegin = procedure( Sender: TObject ) of object;
  TDPFTableViewScrollEnd   = procedure( Sender: TObject; StartScroll, EndScroll: TScrollPoint ) of object;

  TDPFTableViewScrollBeginDragging = procedure( Sender: TObject ) of object;
  TDPFTableViewScrollEndDragging   = procedure( Sender: TObject; StartScroll, EndScroll: TScrollPoint ) of object;

  TDPFTableViewOnCanMoveRow          = procedure( Sender: TObject; SectionNo: LongInt; RowNo: LongInt; var CanMove: Boolean ) of object;
  TDPFTableViewOnCanMoveFromRowToRow = procedure( Sender: TObject; FromSectionNo: LongInt; FromRowNo: LongInt; ToSectionNo: LongInt; ToRowNo: LongInt; var CanMove: Boolean ) of object;
  TDPFTableViewOnAfterMoveRow        = procedure( Sender: TObject; FromSectionNo: LongInt; FromRowNo: LongInt; ToSectionNo: LongInt; ToRowNo: LongInt ) of object;
  TDPFTableScrollPageChanged         = procedure( Sender: TObject; PageNO: LongInt ) of object;
  TDPFTableViewOnCanDeleteRow        = procedure( Sender: TObject; SectionNo: LongInt; RowNo: LongInt; var CanDelete: Boolean ) of object;

  // ----------------------------------------------------------------------------
  TDPFTableViewEditing = class( TPersistent )
  private
    FDPFUITableView: TDPFUITableView;
    FCellEdit      : Boolean;
    FEditAll       : Boolean;
    FMoving        : Boolean;
    procedure SetEditAll( const Value: Boolean );
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    destructor Destroy; override;
  published
    property CellEdit: Boolean read FCellEdit write FCellEdit default false;
    property EditAll : Boolean read FEditAll write SetEditAll default false;
    property Moving  : Boolean read FMoving write FMoving default false;
  end;

  TDPFTableRefreshControl = class( TPersistent )
  private
    FDPFUITableView : TDPFUITableView;
    FVisible        : Boolean;
    FColor          : TAlphaColor;
    FBackgroundColor: TAlphaColor;
    FText           : string;
    procedure SetText( const Value: string );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetColor( const Value: TAlphaColor );
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    destructor Destroy; override;
  published
    property Visible        : Boolean read FVisible write FVisible default false;
    property Color          : TAlphaColor read FColor write SetColor default TAlphaColors.Tomato;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property Text           : string read FText write SetText;
  end;

  TDPFTableViewScrolling = class( TPersistent )
  private
    FAlwaysBounceHorizontal        : Boolean;
    FEnabled                       : Boolean;
    FDirectionalLockEnabled        : Boolean;
    FShowsVerticalScrollIndicator  : Boolean;
    FShowsHorizontalScrollIndicator: Boolean;
    FAlwaysBounceVertical          : Boolean;
    FBounces                       : Boolean;
    FScrollsToTop                  : Boolean;
    FHorizontal                    : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Horizontal                    : Boolean read FHorizontal write FHorizontal default false;
    property Enabled                       : Boolean read FEnabled write FEnabled default True;
    property DirectionalLockEnabled        : Boolean read FDirectionalLockEnabled write FDirectionalLockEnabled default False;
    property Bounces                       : Boolean read FBounces write FBounces default True;
    property AlwaysBounceVertical          : Boolean read FAlwaysBounceVertical write FAlwaysBounceVertical default False;
    property AlwaysBounceHorizontal        : Boolean read FAlwaysBounceHorizontal write FAlwaysBounceHorizontal default False;
    property ShowsHorizontalScrollIndicator: Boolean read FShowsHorizontalScrollIndicator write FShowsHorizontalScrollIndicator default True;
    property ShowsVerticalScrollIndicator  : Boolean read FShowsVerticalScrollIndicator write FShowsVerticalScrollIndicator default True;
    property ScrollsToTop                  : Boolean read FScrollsToTop write FScrollsToTop default True;
  end;

  TDPFTableViewSearchBar = class( TPersistent )
  private
    FSearchInItemText: Boolean;
    FSearchInItemDesc: Boolean;
    FSearchKind      : TDPFTableViewSearchKind;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SearchInItemText: Boolean read FSearchInItemText write FSearchInItemText default true;
    property SearchInItemDesc: Boolean read FSearchInItemDesc write FSearchInItemDesc default true;
    property SearchKind      : TDPFTableViewSearchKind read FSearchKind write FSearchKind default TDPFTableViewSearchKind.skStartsWith;
  end;

  TDPFTableViewOptions = class( TPersistent )
  private
{$IFDEF IOS}
    selectedBackgroundView: UIView;
{$ENDIF}
    FDPFUITableView         : TDPFUITableView;
    FEdition                : TDPFTableViewEditing;
    FAllowsMultipleSelection: Boolean;
    // FHeaderTitle                 : string;
    FSeparatorColor              : TAlphaColor;
    FRowHeight                   : CGFloat;
    FAllowsSelection             : Boolean;
    FScrolling                   : TDPFTableViewScrolling;
    FAllowsSelectionDuringEditing: Boolean;
    FPaging                      : Boolean;
    FSectionFooterHeight         : LongInt;
    FSectionHeaderHeight         : LongInt;
    FSeparatorStyle              : TDPFTableViewSeparatorStyle;
    FViewStyle                   : TDPFTableViewStyle;
    FIndicatorStyle              : TDPFTableViewIndicatorStyle;
    FBackgroundColor             : TAlphaColor;

    FBackgroundImage: string;
    FSelectionStyle : TDPFTableViewSelectionStyle;
    FSearchBar      : TDPFTableViewSearchBar;
    FRefreshControl : TDPFTableRefreshControl;

    FTextFont                            : TDPFFont;
    FDescriptionFont                     : TDPFFont;
    FAllowsMultipleSelectionDuringEditing: Boolean;
    FSelectedColor                       : TAlphaColor;

    procedure SetEdition( const Value: TDPFTableViewEditing );
    // procedure SetHeaderTitle( const Value: string );
    procedure SetRowHeight( const Value: CGFloat);
    procedure SetScrolling( const Value: TDPFTableViewScrolling );
    procedure SetSeparatorColor( const Value: TAlphaColor );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetSearchBar( const Value: TDPFTableViewSearchBar );
    procedure SetRefreshControl( const Value: TDPFTableRefreshControl );
    procedure SetDescriptionFont( const Value: TDPFFont );
    procedure SetTextFont( const Value: TDPFFont );
    procedure SetSelectedColor( const Value: TAlphaColor );
    procedure SetBackgroundImage( const Value: string );

  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    destructor Destroy; override;
  published
    property AllowsMultipleSelection             : Boolean read FAllowsMultipleSelection write FAllowsMultipleSelection default False;
    property AllowsMultipleSelectionDuringEditing: Boolean read FAllowsMultipleSelectionDuringEditing write FAllowsMultipleSelectionDuringEditing default False;
    property AllowsSelection                     : Boolean read FAllowsSelection write FAllowsSelection default True;
    property AllowsSelectionDuringEditing        : Boolean read FAllowsSelectionDuringEditing write FAllowsSelectionDuringEditing default False;
    property BackgroundColor                     : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.White;
    property SelectedColor                       : TAlphaColor read FSelectedColor write SetSelectedColor default TAlphaColors.Null;
    property SeparatorColor                      : TAlphaColor read FSeparatorColor write SetSeparatorColor default TAlphaColors.Black;

    property Edition       : TDPFTableViewEditing read FEdition write SetEdition;
    property RefreshControl: TDPFTableRefreshControl read FRefreshControl write SetRefreshControl;
    property Scrolling     : TDPFTableViewScrolling read FScrolling write SetScrolling;
    property SearchBar     : TDPFTableViewSearchBar read FSearchBar write SetSearchBar;

    property TextFont       : TDPFFont read FTextFont write SetTextFont;
    property DescriptionFont: TDPFFont read FDescriptionFont write SetDescriptionFont;

    property ViewStyle          : TDPFTableViewStyle read FViewStyle write FViewStyle default tvsPlain;
    property SeparatorStyle     : TDPFTableViewSeparatorStyle read FSeparatorStyle write FSeparatorStyle default tssSingleLine;
    property SelectionStyle     : TDPFTableViewSelectionStyle read FSelectionStyle write FSelectionStyle default tcsBlue;
    property IndicatorStyle     : TDPFTableViewIndicatorStyle read FIndicatorStyle write FIndicatorStyle default tviDefault;
    property RowHeight          : CGFloat read FRowHeight write SetRowHeight;
    property SectionFooterHeight: LongInt read FSectionFooterHeight write FSectionFooterHeight default 30;
    property SectionHeaderHeight: LongInt read FSectionHeaderHeight write FSectionHeaderHeight default 30;

    property Paging         : Boolean read FPaging write FPaging default False;
    property BackgroundImage: string read FBackgroundImage write SetBackgroundImage;
  end;

{$IFDEF IOS}

  // ----------------------------------------------------------------------------
  DPFTableViewSwipGestureRecognizerDelegate = interface( IObjectiveC )
    ['{4B01F181-3F36-4427-AA11-D29F456FA661}']
    procedure swipeGestureRecognizer( gestureRecognizer: UISwipeGestureRecognizer ); cdecl;
  end;

  TDPFTableViewSwipGestureRecognizerDelegate = class( TOCLocal, DPFTableViewSwipGestureRecognizerDelegate )
  private
    FTableView: UITableView;
  public
    constructor Create( ATableView: IOSapi.Uikit.UITableView );
    procedure swipeGestureRecognizer( gestureRecognizer: UISwipeGestureRecognizer ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TableSectionGestureRecognizer = interface( IObjectiveC )
    ['{BBE3DCF5-1492-4DDC-A413-C87E78273457}']
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

  TDPFTableSectionGestureRecognizerDelegate = class( TOCLocal, TableSectionGestureRecognizer )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TableCellGestureRecognizer = interface( IObjectiveC )
    ['{05BCCBE8-B30D-4527-98CA-36AC5B228DF1}']
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

  TDPFTableCellGestureRecognizerDelegate = class( TOCLocal, TableCellGestureRecognizer )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    procedure handleSingleTap( gr: UITapGestureRecognizer ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TableCellLongPressGestureRecognizer = interface( IObjectiveC )
    ['{39113EB5-531F-4F35-AD86-CBA99F05C54A}']
    procedure handleLongPress( lpgr: UILongPressGestureRecognizer ); cdecl;
  end;

  TDPFTableCellLongPressGestureRecognizerDelegate = class( TOCLocal, TableCellLongPressGestureRecognizer )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    procedure handleLongPress( lpgr: UILongPressGestureRecognizer ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  DPFTableViewTimerDelegate = interface( IObjectiveC )
    ['{03225A46-B4BA-4D84-BADD-01DA47935BB3}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFTableViewTimerDelegate = class( TOCLocal, DPFTableViewTimerDelegate )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  NSIndexPath01 = interface( NSIndexPath )
  end;

  NSIndexPath02 = interface( NSIndexPath )
  end;

  NSIndexPath03 = interface( NSIndexPath )
  end;

  NSIndexPath04 = interface( NSIndexPath )
  end;

  TNSIndexPath04 = class( TOCGenericImport<NSIndexPathClass, NSIndexPath04> );

  NSIndexPath05 = interface( NSIndexPath )
  end;

  NSIndexPath06 = interface( NSIndexPath )
  end;

  NSIndexPath07 = interface( NSIndexPath )
  end;

  TNSIndexPath07 = class( TOCGenericImport<NSIndexPathClass, NSIndexPath07> );

  NSIndexPath08 = interface( NSIndexPath )
  end;

  NSIndexPath09 = interface( NSIndexPath )
  end;

  NSIndexPath10 = interface( NSIndexPath )
  end;

  NSIndexPath11 = interface( NSIndexPath )
  end;

  NSIndexPath12 = interface( NSIndexPath )
  end;

  NSIndexPath13 = interface( NSIndexPath )
  end;

  NSIndexPath14 = interface( NSIndexPath )
  end;

  TNSIndexPath14 = class( TOCGenericImport<NSIndexPathClass, NSIndexPath14> );

  UITableView = interface( UIScrollView )
    ['{C1A9608D-1D18-41E9-9B51-E5DFF12C6651}']
    function allowsMultipleSelection: Boolean; cdecl;
    function allowsMultipleSelectionDuringEditing: Boolean; cdecl;
    function allowsSelection: Boolean; cdecl;
    function allowsSelectionDuringEditing: Boolean; cdecl;
    function backgroundView: UIView; cdecl;
    procedure beginUpdates; cdecl;
    function cellForRowAtIndexPath( indexPath: NSIndexPath ): UITableViewCell; cdecl;
    function dataSource: Pointer; cdecl;
    function delegate: Pointer; cdecl;
    procedure deleteRowsAtIndexPaths( indexPaths: NSArray; withRowAnimation: UITableViewRowAnimation ); cdecl;
    procedure deleteSections( sections: NSIndexSet; withRowAnimation: UITableViewRowAnimation ); cdecl;
    function dequeueReusableCellWithIdentifier( identifier: NSString ): Pointer; cdecl;
    procedure deselectRowAtIndexPath( indexPath: NSIndexPath; animated: Boolean ); cdecl;
    procedure endUpdates; cdecl;
    function indexPathForCell( cell: UITableViewCell ): NSIndexPath; cdecl;
    function indexPathForRowAtPoint( point: CGPoint ): NSIndexPath; cdecl;
    function indexPathForSelectedRow: NSIndexPath; cdecl;
    function indexPathsForRowsInRect( rect: CGRect ): NSArray; cdecl;
    function indexPathsForSelectedRows: NSArray; cdecl;
    function indexPathsForVisibleRows: NSArray; cdecl;
    function initWithFrame( frame: CGRect; style: UITableViewStyle ): Pointer; cdecl;
    procedure insertRowsAtIndexPaths( indexPaths: NSArray; withRowAnimation: UITableViewRowAnimation ); cdecl;
    procedure insertSections( sections: NSIndexSet; withRowAnimation: UITableViewRowAnimation ); cdecl;
    function isEditing: Boolean; cdecl;
    procedure moveRowAtIndexPath( indexPath: NSIndexPath; toIndexPath: NSIndexPath ); cdecl;
    procedure moveSection( section: NSInteger; toSection: NSInteger ); cdecl;
    function numberOfRowsInSection( section: NSInteger ): NSInteger; cdecl;
    function numberOfSections: NSInteger; cdecl;
    function rectForFooterInSection( section: NSInteger ): CGRect; cdecl;
    function rectForHeaderInSection( section: NSInteger ): CGRect; cdecl;
    function rectForRowAtIndexPath( indexPath: NSIndexPath ): CGRect; cdecl;
    function rectForSection( section: NSInteger ): CGRect; cdecl;
    procedure registerNib( nib: UINib; forCellReuseIdentifier: NSString ); cdecl;
    procedure reloadData; cdecl;
    procedure reloadRowsAtIndexPaths( indexPaths: NSArray; withRowAnimation: UITableViewRowAnimation ); cdecl;
    procedure reloadSectionIndexTitles; cdecl;
    procedure reloadSections( sections: NSIndexSet; withRowAnimation: UITableViewRowAnimation ); cdecl;
    function rowHeight: CGFloat; cdecl;
    procedure scrollToNearestSelectedRowAtScrollPosition( scrollPosition: UITableViewScrollPosition; animated: Boolean ); cdecl;
    procedure scrollToRowAtIndexPath( indexPath: NSIndexPath; atScrollPosition: UITableViewScrollPosition; animated: Boolean ); cdecl;
    function sectionFooterHeight: CGFloat; cdecl;
    function sectionHeaderHeight: CGFloat; cdecl;
    function sectionIndexMinimumDisplayRowCount: NSInteger; cdecl;
    procedure selectRowAtIndexPath( indexPath: NSIndexPath; animated: Boolean; scrollPosition: UITableViewScrollPosition ); cdecl;
    function separatorColor: UIColor; cdecl;
    function separatorStyle: UITableViewCellSeparatorStyle; cdecl;
    procedure setAllowsMultipleSelection( allowsMultipleSelection: Boolean ); cdecl;
    procedure setAllowsMultipleSelectionDuringEditing( allowsMultipleSelectionDuringEditing: Boolean ); cdecl;
    procedure setAllowsSelection( allowsSelection: Boolean ); cdecl;
    procedure setAllowsSelectionDuringEditing( allowsSelectionDuringEditing: Boolean ); cdecl;
    procedure setBackgroundView( backgroundView: UIView ); cdecl;
    procedure setDataSource( dataSource: Pointer ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setEditing( editing: Boolean ); cdecl; overload;
    procedure setEditing( editing: Boolean; animated: Boolean ); cdecl; overload;
    procedure setRowHeight( rowHeight: CGFloat ); cdecl;
    procedure setSectionFooterHeight( sectionFooterHeight: CGFloat ); cdecl;
    procedure setSectionHeaderHeight( sectionHeaderHeight: CGFloat ); cdecl;
    procedure setSectionIndexMinimumDisplayRowCount( sectionIndexMinimumDisplayRowCount: NSInteger ); cdecl;
    procedure setSeparatorColor( separatorColor: UIColor ); cdecl;
    procedure setSeparatorStyle( separatorStyle: UITableViewCellSeparatorStyle ); cdecl;
    procedure setTableFooterView( tableFooterView: UIView ); cdecl;
    procedure setTableHeaderView( tableHeaderView: UIView ); cdecl;
    function style: UITableViewStyle; cdecl;
    function tableFooterView: UIView; cdecl;
    function tableHeaderView: UIView; cdecl;
    function visibleCells: NSArray; cdecl;

    function sectionIndexBackgroundColor: UIColor; cdecl; // Added by me / iOS 7.0 and later
    procedure setSectionIndexBackgroundColor( sectionIndexBackgroundColor: UIColor ); cdecl; // Added by me / iOS 7.0 and later

    function sectionIndexColor: UIColor; cdecl; // Added by me / iOS 6.0 and later
    procedure setSectionIndexColor( sectionIndexColor: UIColor ); cdecl; // Added by me / iOS 6.0 and later

    function sectionIndexTrackingBackgroundColor: UIColor; cdecl; // Added by me / iOS 6.0 and later
    procedure setSectionIndexTrackingBackgroundColor( sectionIndexTrackingBackgroundColor: UIColor ); cdecl; // Added by me / iOS 6.0 and later

  end;

  TUITableView = class( TOCGenericImport<UITableViewClass, UITableView> )
  end;

  UITableViewControllerClass = interface( UIViewControllerClass )
    ['{55774770-57B8-4CB7-A26A-A084B0BEF62A}']
  end;

  UITableViewController = interface( UIViewController )
    ['{14693F12-2155-4EC8-A663-71E1542C840F}']
    function clearsSelectionOnViewWillAppear: Boolean; cdecl;
    function initWithStyle( style: UITableViewStyle ): Pointer; cdecl;
    procedure setClearsSelectionOnViewWillAppear( clearsSelectionOnViewWillAppear: Boolean ); cdecl;
    procedure setTableView( tableView: UITableView ); cdecl;
    function tableView: UITableView; cdecl;

    function refreshControl: UIRefreshControl; cdecl;
    procedure setRefreshControl( refreshControl: UIRefreshControl ); cdecl;
  end;

  TUITableViewController = class( TOCGenericImport<UITableViewControllerClass, UITableViewController> )
  end;

  // ----------------------------------------------------------------------------
  DPFUITableViewDelegate = interface( IObjectiveC )
    ['{37485F85-0101-4EB2-9C15-6BDD4F2D9E24}']
    procedure tableView( tableView: UITableView; accessoryButtonTappedForRowWithIndexPath: NSIndexPath03 ); cdecl; overload;

    // function tableView( tableView: UITableView; accessoryTypeForRowWithIndexPath: NSIndexPath05 ): UITableViewCellAccessoryType; cdecl; overload;

    // function tableView( tableView: UITableView; canPerformAction: SEL; forRowAtIndexPath: NSIndexPath01; withSender: Pointer ): Boolean; cdecl; overload;
    // procedure tableView( tableView: UITableView; performAction: SEL; forRowAtIndexPath: NSIndexPath; withSender: Pointer ); cdecl; overload;
    // function tableView( tableView: UITableView; indentationLevelForRowAtIndexPath: NSIndexPath08 ): NSInteger; cdecl; overload;
    // function tableView( tableView: UITableView; shouldIndentWhileEditingRowAtIndexPath: NSIndexPath09 ): Boolean; cdecl; overload;
    // function tableView( tableView: UITableView; shouldShowMenuForRowAtIndexPath: NSIndexPath10 ): Boolean; cdecl; overload;
    // function tableView( tableView: UITableView; titleForDeleteConfirmationButtonForRowAtIndexPath: NSIndexPath11 ): NSString; cdecl; overload;

    function tableView( tableView: UITableView; targetIndexPathForMoveFromRowAtIndexPath: NSIndexPath01; toProposedIndexPath: NSIndexPath01 ): NSIndexPath; cdecl; overload;

    procedure tableView( tableView: UITableView; didDeselectRowAtIndexPath: NSIndexPath04 ); cdecl; overload;
    procedure tableView( tableView: UITableView; didEndEditingRowAtIndexPath: NSIndexPath06 ); cdecl; overload;

    function tableView( tableView: UITableView; editingStyleForRowAtIndexPath: NSIndexPath07 ): UITableViewCellEditingStyle; cdecl; overload;

    procedure tableView( tableView: UITableView; willDisplayCell: UITableViewCell; forRowAtIndexPath: NSIndexPath ); cdecl; overload;

    procedure tableView( tableView: UITableView; didSelectRowAtIndexPath: NSIndexPath ); cdecl; overload;
    function tableView( tableView: UITableView; willSelectRowAtIndexPath: NSIndexPath01 ): NSIndexPath; cdecl; overload;

    function tableView( tableView: UITableView; viewForHeaderInSection: NSInteger ): UIView; cdecl; overload;
    function tableView( tableView: UITableView; heightForHeaderInSection: NSInteger1 ): CGFloat; cdecl; overload;

    procedure tableView( tableView: UITableView; willBeginEditingRowAtIndexPath: NSIndexPath12 ); cdecl; overload;
    function tableView( tableView: UITableView; willDeselectRowAtIndexPath: NSIndexPath13 ): NSIndexPath; cdecl; overload;

    function tableView( tableView: UITableView; viewForFooterInSection: NSInteger2 ): UIView; cdecl; overload;
    function tableView( tableView: UITableView; heightForFooterInSection: NSInteger3 ): CGFloat; cdecl; overload;

    function tableView( tableView: UITableView; heightForRowAtIndexPath: NSIndexPath02 ): CGFloat; cdecl; overload;

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: CGPoint ); cdecl;

    procedure scrollViewWillBeginDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;

    procedure scrollViewDidEndScrollingAnimation( scrollView: UIScrollView ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TTableViewDelegate = class( TOCLocal, DPFUITableViewDelegate )
  private
    FDPFUITableView: TDPFUITableView;
    DeleteButton   : UIButton;
    OriginCellFrame: CGRect;
  public
    constructor Create( AParent: TDPFUITableView );

    procedure tableView( tableView: UITableView; accessoryButtonTappedForRowWithIndexPath: NSIndexPath03 ); overload; cdecl;

    // function tableView( tableView: UITableView; accessoryTypeForRowWithIndexPath: NSIndexPath05 ): UITableViewCellAccessoryType; overload; cdecl;

    // function tableView( tableView: UITableView; canPerformAction: SEL; forRowAtIndexPath: NSIndexPath01; withSender: Pointer ): Boolean; overload; cdecl;
    // procedure tableView( tableView: UITableView; performAction: SEL; forRowAtIndexPath: NSIndexPath; withSender: Pointer ); overload; cdecl;
    // function tableView( tableView: UITableView; indentationLevelForRowAtIndexPath: NSIndexPath08 ): NSInteger; overload; cdecl;
    // function tableView( tableView: UITableView; shouldIndentWhileEditingRowAtIndexPath: NSIndexPath09 ): Boolean; overload; cdecl;
    // function tableView( tableView: UITableView; shouldShowMenuForRowAtIndexPath: NSIndexPath10 ): Boolean; overload; cdecl;
    // function tableView( tableView: UITableView; titleForDeleteConfirmationButtonForRowAtIndexPath: NSIndexPath11 ): NSString; overload; cdecl;

    function tableView( tableView: UITableView; targetIndexPathForMoveFromRowAtIndexPath: NSIndexPath01; toProposedIndexPath: NSIndexPath01 ): NSIndexPath; overload; cdecl;

    procedure tableView( tableView: UITableView; didDeselectRowAtIndexPath: NSIndexPath04 ); overload; cdecl;
    procedure tableView( tableView: UITableView; didEndEditingRowAtIndexPath: NSIndexPath06 ); overload; cdecl;

    function tableView( tableView: UITableView; editingStyleForRowAtIndexPath: NSIndexPath07 ): UITableViewCellEditingStyle; overload; cdecl;

    procedure tableView( tableView: UITableView; didSelectRowAtIndexPath: NSIndexPath ); overload; cdecl;
    function tableView( tableView: UITableView; willSelectRowAtIndexPath: NSIndexPath01 ): NSIndexPath; overload; cdecl;

    function tableView( tableView: UITableView; viewForHeaderInSection: NSInteger ): UIView; overload; cdecl;
    function tableView( tableView: UITableView; heightForHeaderInSection: NSInteger1 ): CGFloat; overload; cdecl;

    function tableView( tableView: UITableView; viewForFooterInSection: NSInteger2 ): UIView; overload; cdecl;
    function tableView( tableView: UITableView; heightForFooterInSection: NSInteger3 ): CGFloat; overload; cdecl;

    procedure tableView( tableView: UITableView; willBeginEditingRowAtIndexPath: NSIndexPath12 ); overload; cdecl;
    function tableView( tableView: UITableView; willDeselectRowAtIndexPath: NSIndexPath13 ): NSIndexPath; overload; cdecl;

    function tableView( tableView: UITableView; heightForRowAtIndexPath: NSIndexPath02 ): CGFloat; overload; cdecl;

    procedure tableView( tableView: UITableView; willDisplayCell: UITableViewCell; forRowAtIndexPath: NSIndexPath ); overload; cdecl;
    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: CGPoint ); cdecl;

    procedure scrollViewWillBeginDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndScrollingAnimation( scrollView: UIScrollView ); cdecl;

    function SetHeaderFooterSettings( Item: TDPFTableItemHeaderFooterSetting ): UIView;
  end;

  // ----------------------------------------------------------------------------

  DPFUITableViewDataSourceDelegate = interface( IObjectiveC )
    ['{57C8B901-5A33-4278-AE99-175BFFBD7EF1}']
    function numberOfSectionsInTableView( tableView: UITableView ): NSInteger; cdecl;
    function sectionIndexTitlesForTableView( tableView: UITableView ): NSArray; cdecl;

    function tableView( tableView: UITableView; cellForRowAtIndexPath: NSIndexPath ): UITableViewCell; cdecl; overload;
    function tableView( tableView: UITableView; numberOfRowsInSection: NSInteger ): NSInteger; cdecl; overload;
    procedure tableView( tableView: UITableView; commitEditingStyle: UITableViewCellEditingStyle; forRowAtIndexPath: NSIndexPath ); cdecl; overload;
    function tableView( tableView: UITableView; titleForHeaderInSection: NSInteger1 ): IOSapi.Foundation.NSString; cdecl; overload;
    function tableView( tableView: UITableView; titleForFooterInSection: NSInteger2 ): IOSapi.Foundation.NSString; cdecl; overload;
    function tableView( tableView: UITableView; canEditRowAtIndexPath: NSIndexPath01 ): Boolean; cdecl; overload;
    function tableView( tableView: UITableView; canMoveRowAtIndexPath: NSIndexPath02 ): Boolean; cdecl; overload;
    procedure tableView( tableView: UITableView; moveRowAtIndexPath: NSIndexPath; toIndexPath: NSIndexPath ); cdecl; overload;
    function tableView( tableView: UITableView; sectionForSectionIndexTitle: NSString; atIndex: NSInteger ): NSInteger; cdecl; overload;
  end;

  TTableViewDataSourceDelegate = class( TOCLocal, DPFUITableViewDataSourceDelegate )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( AParent: TDPFUITableView );
    destructor Destroy; override;

    function numberOfSectionsInTableView( tableView: UITableView ): NSInteger; cdecl;
    function tableView( tableView: UITableView; cellForRowAtIndexPath: NSIndexPath ): UITableViewCell; overload; cdecl;
    function tableView( tableView: UITableView; numberOfRowsInSection: NSInteger ): NSInteger; overload; cdecl;
    procedure tableView( tableView: UITableView; commitEditingStyle: UITableViewCellEditingStyle; forRowAtIndexPath: NSIndexPath ); overload; cdecl;
    function tableView( tableView: UITableView; titleForHeaderInSection: NSInteger1 ): IOSapi.Foundation.NSString; overload; cdecl;
    function tableView( tableView: UITableView; titleForFooterInSection: NSInteger2 ): IOSapi.Foundation.NSString; overload; cdecl;
    function tableView( tableView: UITableView; canEditRowAtIndexPath: NSIndexPath01 ): Boolean; overload; cdecl;
    function tableView( tableView: UITableView; canMoveRowAtIndexPath: NSIndexPath02 ): Boolean; overload; cdecl;
    procedure tableView( tableView: UITableView; moveRowAtIndexPath: NSIndexPath; toIndexPath: NSIndexPath ); overload; cdecl;
    function tableView( tableView: UITableView; sectionForSectionIndexTitle: NSString; atIndex: NSInteger ): NSInteger; overload; cdecl;
    function sectionIndexTitlesForTableView( tableView: UITableView ): NSArray; cdecl;
  end;

{$IFDEF IOS6}

  DPFRefreshControlDelegate = interface( NSObject )
    ['{F218590B-6B26-452D-9857-20EE0ED1D92A}']
    procedure OnRefreshNeeded; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFRefreshControlDelegate = class( TOCLocal )
  private
    FDPFUITableView: TDPFUITableView;
  public
    constructor Create( ADPFUITableView: TDPFUITableView );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure OnRefreshNeeded; cdecl;
  end;

{$ENDIF IOS6}
{$ENDIF}

  // ----------------------------------------------------------------------------
  TAutoScrollDir = ( asdDown, asdUp );

  TTableSearchRec = record
    Section, Row: LongInt;
  end;

  TTableSelectedRows = record
    Section, Row: LongInt;
  end;

  TTapPosition = record
    CellX: Single;
    CellY: Single;
    AbsoluteX: Single;
    AbsoluteY: Single;
    ScreenX: Single;
    ScreenY: Single;
  end;

  TDPFTableViewScrollPosition = ( tvspNone = 0, tvspTop = 1, tvspMiddle = 2, tvspBottom = 3 );

  TOnSectionClick  = procedure( Sender: TObject; SectionNo: LongInt ) of object;
  TOnCellClick     = procedure( Sender: TObject; SectionNo: LongInt; RowNo: LongInt; TapPosition: TTapPosition; State: TDPFGestureRecognizerState; var CancelsTouchesInView: Boolean ) of object;
  TOnCellLongClick = procedure( Sender: TObject; SectionNo: LongInt; RowNo: LongInt; TapPosition: TTapPosition; State: TDPFGestureRecognizerState; var CancelsTouchesInView: Boolean ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUITableView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFTableSectionGestureRecognizerDelegate      : TDPFTableSectionGestureRecognizerDelegate;
    FDPFTableCellGestureRecognizerDelegate         : TDPFTableCellGestureRecognizerDelegate;
    FDPFTableCellLongPressGestureRecognizerDelegate: TDPFTableCellLongPressGestureRecognizerDelegate;
    FNSTimer                                       : NSTimer;
    // FDelayNSTimer    : NSTimer;
    FDPFTimerDelegate: TDPFTableViewTimerDelegate;

    FUITableViewController      : UITableViewController { Controller };
    FTableViewDataSourceDelegate: TTableViewDataSourceDelegate;
    FTableViewDelegate          : TTableViewDelegate;
    FSearchMap                  : array of TTableSearchRec;
    FStartDelay                 : TDateTime;

    // FDPFTableViewSwipGestureRecognizerDelegate: TDPFTableViewSwipGestureRecognizerDelegate;
    FScreenRect: CGRect;
{$IFDEF IOS6}
    FDPFRefreshControlDelegate: TDPFRefreshControlDelegate;
    FDPFRefreshControl        : UIRefreshControl;
{$ENDIF}
{$ENDIF}
    FVisible                         : Boolean;
    FOnItemSelect                    : TDPFTableViewItemOnSelect;
    FOptions                         : TDPFTableViewOptions;
    FSections                        : TSectionCollection;
    FOnItemDelete                    : TDPFTableViewItemOnDelete;
    FOnItemEdit                      : TDPFTableViewItemOnEdit;
    FOnItemInsert                    : TDPFTableViewItemOnInsert;
    FOnDrawCell                      : TDPFTableViewItemOnDrawCell;
    FOnGetRowHeight                  : TDPFTableViewItemOnGetRowHeight;
    FAutoScroll                      : Boolean;
    FisDragging                      : Boolean;
    FAutoScrollDir                   : TAutoScrollDir;
    FOnRefreshNeeded                 : TNotifyEvent;
    FOnApplyFrameData                : TDPFTableViewItemOnApplyFrameData;
    FIndexList                       : TStrings;
    FOnScrollEnd                     : TDPFTableViewScrollEnd;
    FOnScrollBegin                   : TDPFTableViewScrollBegin;
    FOnAccessoryButtonTapped         : TDPFTableViewItemOnAccessoryButtonTapped;
    FSearchText                      : string;
    FSearchTextExists                : Boolean;
    FOnCanMoveRow                    : TDPFTableViewOnCanMoveRow;
    FOnCanMoveFromRowToRow           : TDPFTableViewOnCanMoveFromRowToRow;
    FOnAfterMoveRow                  : TDPFTableViewOnAfterMoveRow;
    FOnItemDeSelect                  : TDPFTableViewItemOnDeSelect;
    FOnNeedCellIdentifier            : TDPFTableViewOnNeedCellIdentifier;
    FOnGetNumberOfRowsInSection      : TDPFTableViewOnGetNumberOfRowsInSection;
    FOnGetNumberOfSections           : TDPFTableViewOnGetNumberOfSections;
    FVirtualMode                     : Boolean;
    FOnPageChanged                   : TDPFTableScrollPageChanged;
    FAutoScrollLoop                  : Boolean;
    FTableStartLoading               : Boolean;
    FSearchCategory                  : string;
    FIndexListBackgroundColor        : TAlphaColor;
    FIndexListTextColor              : TAlphaColor;
    FIndexListTrackingBackgroundColor: TAlphaColor;
    FOnSectionClick                  : TOnSectionClick;
    FOnCellClick                     : TOnCellClick;
    FOnCellLongClick                 : TOnCellLongClick;
    FIsScrolling                     : Boolean;
    FOnScrollEndDragging             : TDPFTableViewScrollEndDragging;
    FOnScrollBeginDragging           : TDPFTableViewScrollBeginDragging;
    FOnCanDeleteRow                  : TDPFTableViewOnCanDeleteRow;

    procedure SetOptions( const Value: TDPFTableViewOptions );
    procedure SetSections( const Value: TSectionCollection );
    procedure SetAutoScroll( const Value: Boolean );
    procedure SetIndexList( const Value: TStrings );
    function GetRow( ASection: LongInt; ARow: LongInt ): TTableItem;
    procedure SetSearchText( const Value: string );
    function GetPageNo: LongInt;
    procedure SetSearchCategory( const Value: string );
    procedure SetIndexListBackgroundColor( const Value: TAlphaColor );
    procedure SetIndexListTextColor( const Value: TAlphaColor );
    procedure SetIndexListTrackingBackgroundColor( const Value: TAlphaColor );
    function GetSearchItemCount: LongInt;

  protected
    CurOffSet          : TScrollPoint;
    CellDicCustomViews : TDictionary<Pointer, TArray<TDPFiOSBaseControl>>;
    CellDicCustomFrames: TDictionary<Pointer, TFrame>;
//    procedure Resize; override;
//    procedure Move; override;
    procedure ClearBuffer;
    procedure DoAbsoluteChanged; override;
  public
    property VirtualMode                          : Boolean read FVirtualMode;
    property Row[ASection: LongInt; ARow: LongInt]: TTableItem read GetRow;
{$IFDEF IOS}
    function GetRowCustomViews( const SectionNo: NativeUInt; const RowNo: NativeUInt ): TArray<TDPFiOSBaseControl>;
    function GetRowCustomFrame( const SectionNo: NativeUInt; const RowNo: NativeUInt ): TFrame;
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    procedure RefreshNeeded; override;
    procedure ClearAll( reloadData: Boolean = false; ClearSelections: Boolean = false );
    procedure StopScroll;

    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
{$IFDEF IOS}
    procedure UpdateCellLabel( CellLabel: UILabel; Item: TDPFTableItemText; UpdateText: Boolean; Font: TDPFFont );
    procedure SetCellBackgroundColor( Cell: UITableViewCell; BackgroundColor: TAlphaColor );
    function GetSelectedRows: TArray<TTableSelectedRows>;
    procedure DeleteRow( const Section: Int64; const Row: Int64 );
{$ENDIF}
    procedure ScrollToPos( x, y: Single );
    procedure ScrollToRow( Section, Row: LongInt; ScrollPosition: DPFTableViewScrollPosition = DPFTableViewScrollPosition.spMiddle; Animate: Boolean = true );
    procedure BeginRefreshing;
    procedure EndRefreshing;
    procedure ClearSelectedRows( const FireEvent: Boolean = False );
    procedure SelectRow( const Section: LongInt; const Row: LongInt; const FireEvent: Boolean = true; ScrollPosition: TDPFTableViewScrollPosition = TDPFTableViewScrollPosition.tvspNone );
    procedure DeSelectRow( const Section: LongInt; const Row: LongInt; const FireEvent: Boolean = true );
    function BOF: Boolean;
    function EOF: Boolean;

    function RowIsEditing( SectionNo: LongInt; RowNo: LongInt ): Boolean;
    property isDragging: Boolean read FisDragging default False;
    property isScrolling: Boolean read FIsScrolling default False;
    property SearchItemCount: LongInt read GetSearchItemCount;
    property SearchText: string read FSearchText write SetSearchText;
    property SearchCategory: string read FSearchCategory write SetSearchCategory;
    property PageNo: LongInt read GetPageNo;
    property TableStartLoading: Boolean read FTableStartLoading;

  published
    property AutoScroll    : Boolean read FAutoScroll write SetAutoScroll default False;
    property AutoScrollLoop: Boolean read FAutoScrollLoop write FAutoScrollLoop default true;
    property AutoScrollDir : TAutoScrollDir read FAutoScrollDir write FAutoScrollDir default asdDown;

    property OnItemDeSelect: TDPFTableViewItemOnDeSelect read FOnItemDeSelect write FOnItemDeSelect;
    property OnItemSelect  : TDPFTableViewItemOnSelect read FOnItemSelect write FOnItemSelect;
    property OnItemInsert  : TDPFTableViewItemOnInsert read FOnItemInsert write FOnItemInsert;
    property OnItemEdit    : TDPFTableViewItemOnEdit read FOnItemEdit write FOnItemEdit;
    property OnItemDelete  : TDPFTableViewItemOnDelete read FOnItemDelete write FOnItemDelete;

    property OnSectionClick : TOnSectionClick read FOnSectionClick write FOnSectionClick;
    property OnCellClick    : TOnCellClick read FOnCellClick write FOnCellClick;
    property OnCellLongClick: TOnCellLongClick read FOnCellLongClick write FOnCellLongClick;

    property OnGetNumberOfSections     : TDPFTableViewOnGetNumberOfSections read FOnGetNumberOfSections write FOnGetNumberOfSections;
    property OnGetNumberOfRowsInSection: TDPFTableViewOnGetNumberOfRowsInSection read FOnGetNumberOfRowsInSection write FOnGetNumberOfRowsInSection;

    property OnNeedCellIdentifier: TDPFTableViewOnNeedCellIdentifier read FOnNeedCellIdentifier write FOnNeedCellIdentifier;

    property OnAccessoryButtonTapped: TDPFTableViewItemOnAccessoryButtonTapped read FOnAccessoryButtonTapped write FOnAccessoryButtonTapped;

    property OnDrawCell      : TDPFTableViewItemOnDrawCell read FOnDrawCell write FOnDrawCell;
    property OnApplyFrameData: TDPFTableViewItemOnApplyFramedata read FOnApplyFrameData write FOnApplyFrameData;
    property OnGetRowHeight  : TDPFTableViewItemOnGetRowHeight read FOnGetRowHeight write FOnGetRowHeight;

    property Options : TDPFTableViewOptions read FOptions write SetOptions;
    property Sections: TSectionCollection read FSections write SetSections;

    property IndexList                       : TStrings read FIndexList write SetIndexList;
    property IndexListBackgroundColor        : TAlphaColor read FIndexListBackgroundColor write SetIndexListBackgroundColor default TAlphaColors.Null;
    property IndexListTrackingBackgroundColor: TAlphaColor read FIndexListTrackingBackgroundColor write SetIndexListTrackingBackgroundColor default TAlphaColors.Null;
    property IndexListTextColor              : TAlphaColor read FIndexListTextColor write SetIndexListTextColor default TAlphaColors.Black;

    property OnRefreshNeeded: TNotifyEvent read FOnRefreshNeeded write FOnRefreshNeeded;

    property OnScrollBegin: TDPFTableViewScrollBegin read FOnScrollBegin write FOnScrollBegin;
    property OnScrollEnd  : TDPFTableViewScrollEnd read FOnScrollEnd write FOnScrollEnd;

    property OnScrollBeginDragging: TDPFTableViewScrollBeginDragging read FOnScrollBeginDragging write FOnScrollBeginDragging;
    property OnScrollEndDragging  : TDPFTableViewScrollEndDragging read FOnScrollEndDragging write FOnScrollEndDragging;

    property OnPageChanged: TDPFTableScrollPageChanged read FOnPageChanged write FOnPageChanged;

    property OnCanDeleteRow       : TDPFTableViewOnCanDeleteRow read FOnCanDeleteRow write FOnCanDeleteRow;
    property OnCanMoveRow         : TDPFTableViewOnCanMoveRow read FOnCanMoveRow write FOnCanMoveRow;
    property OnCanMoveFromRowToRow: TDPFTableViewOnCanMoveFromRowToRow read FOnCanMoveFromRowToRow write FOnCanMoveFromRowToRow;
    property OnAfterMoveRow       : TDPFTableViewOnAfterMoveRow read FOnAfterMoveRow write FOnAfterMoveRow;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Visible;
    property Align;
    property Position;
    property Width;
    property Height;
  end;

implementation

{$IFDEF IOS}

procedure getSecRowIndex( NSI: NSIndexPath; var SecNo: LongWord; var RowNo: LongWord );
var
  Idxes: array of NSUInteger;
begin
  SetLength( Idxes, NSI.length );
  try
    NSI.getIndexes( NSUInteger( @Idxes[0] ) );
    SecNo := Idxes[0];
    RowNo := Idxes[1];
  finally
    SetLength( Idxes, 0 );
    Idxes := nil;
  end;
end;
{$ENDIF}
{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUITableView.Paint;
var
  PaintSections                    : TSectionCollection;
  i, j                             : LongInt;
  ItemRect                         : TRectF;
  y, HeaderPos, FooterPos, h, h2, m: single;
  NeedFree                         : boolean;

  procedure PaintHeaderFooter( PosY: single; Settings: TDPFTableItemHeaderFooterSetting );
  begin
    // ItemRect
    if Options.ViewStyle = tvsPlain then
      ItemRect := RectF( 0, PosY, Self.Width, PosY + iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Height )
    else
      ItemRect := RectF( 0, PosY, Self.Width, PosY + 30 );
    // BG
    if Settings.Kind = kStandard then
    begin
      if Options.ViewStyle = tvsPlain then
        BitmapToRect( Self, iOS_GUI_Bitmaps.TableView.Plain_HeaderBG, ItemRect );
    end
    else
    begin
      Canvas.Fill.Color := Settings.BackgroundColor;
      Canvas.FillRect( ItemRect, 0, 0, AllCorners, Settings.Alpha, TCornerType.Round );
    end;
    // Caption
    if Options.ViewStyle = tvsPlain then
    begin
      Canvas.Font.Size := Settings.Font.FontSize;
      ItemRect.Left    := 13;
      // Shadow
      ItemRect.Top      := ItemRect.Top + 1;
      ItemRect.Bottom   := ItemRect.Bottom + 1;
      Canvas.Fill.Color := $FF697076;
      PaintCaption( Self, Settings.Text, ItemRect, lbTailTruncation, 1, FMTextAlign( Settings.TextAlign ) );
      // Caption
      ItemRect.Top    := ItemRect.Top - 1;
      ItemRect.Bottom := ItemRect.Bottom - 1;
      if Settings.Kind = kStandard then
        Canvas.Fill.Color := TAlphaColors.White
      else
        Canvas.Fill.Color := Settings.TextColor;
      PaintCaption( Self, Settings.Text, ItemRect, lbTailTruncation, 1, FMTextAlign( Settings.TextAlign ) );
    end
    else
    begin
      ItemRect.Left    := 9;
      ItemRect.Right   := Width - 9;
      Canvas.Font.Size := Settings.Font.FontSize;
      if Settings.Kind = kStandard then
        Canvas.Fill.Color := $FF708090
      else
        Canvas.Fill.Color := Settings.TextColor;
      PaintCaption( Self, Settings.Text, ItemRect, lbTailTruncation, 1, FMTextAlign( Settings.TextAlign ) );
    end;
  end;

  procedure PaintItem;
  var
    R: TRectF;
    o: single;

    procedure PaintDescription;
    var
      al: TDPFTextAlignment;

    begin
      if PaintSections[i].TableItems[j].Style = tvcsValue1 then
        al := taRight
      else
        al              := PaintSections[i].TableItems[j].ItemDescription.Alignment;
      Canvas.Fill.Color := $FF808080;
      Canvas.Font.Size  := 12;
      Canvas.Font.Style := [];
      PaintCaption( Self, PaintSections[i].TableItems[j].ItemDescription.Text, R, lbTailTruncation, PaintSections[i].TableItems[j].ItemText.NumberOfLines, FMTextAlign( al ) );
      Canvas.Font.Size  := Options.TextFont.FontSize;
      Canvas.Font.Style := [TFontStyle.fsBold];
      Canvas.Fill.Color := PaintSections[i].TableItems[j].ItemText.Color;
    end;

  begin
    Canvas.Font.Size   := Options.TextFont.FontSize;
    Canvas.Font.Family := 'Verdana';
    R                  := ItemRect;
    if Options.Edition.EditAll and Options.Edition.CellEdit then
    begin
      BitmapToPosition( Self, iOS_GUI_Bitmaps.TableView.Item_Delete, R.Left + 6, R.CenterPoint.Y - iOS_GUI_Bitmaps.TableView.Item_Delete.Height / 2 + 1 );
      R.Left := R.Left + 42;
    end
    else
      R.Left := R.Left + 11;
    if Options.Edition.EditAll and Options.Edition.Moving then
    begin
      R.Right := R.Right - 35;
      BitmapToPosition( Self, iOS_GUI_Bitmaps.TableView.Item_Grabber, R.Right + 8, R.CenterPoint.Y - iOS_GUI_Bitmaps.TableView.Item_Grabber.Height / 2 - 2 );
    end
    else
    begin
      case PaintSections[i].TableItems[j].AccessoryType of
        tvcaDisclosureIndicator:
          begin
            R.Right := R.Right - 30;
            BitmapToPosition( Self, iOS_GUI_Bitmaps.TableView.Item_NextSelection, R.Right + 10, R.CenterPoint.Y - iOS_GUI_Bitmaps.TableView.Item_NextSelection.Height / 2 );
          end;
        tvcaDetailDisclosureButton:
          begin
            R.Right := R.Right - 41;
            BitmapToPosition( Self, iOS_GUI_Bitmaps.TableView.Item_NextButton, R.Right + 5, R.CenterPoint.Y - iOS_GUI_Bitmaps.TableView.Item_NextButton.Height / 2 );
          end;
        tvcaCheckmark:
          begin
            R.Right := R.Right - 34;
            BitmapToPosition( Self, iOS_GUI_Bitmaps.TableView.Item_Check, R.Right + 10, R.CenterPoint.Y - iOS_GUI_Bitmaps.TableView.Item_Check.Height / 2 );
          end;
      end;
    end;
    Canvas.Fill.Color := PaintSections[i].TableItems[j].ItemText.Color;
    case PaintSections[i].TableItems[j].Style of
      tvcsDefault:
        begin
          PaintCaption( Self, PaintSections[i].TableItems[j].ItemText.Text, R, lbTailTruncation, PaintSections[i].TableItems[j].ItemText.NumberOfLines, FMTextAlign( PaintSections[i].TableItems[j].ItemText.Alignment ) );
        end;
      tvcsValue1:
        begin
          if PaintSections[i].TableItems[j].ItemDescription.Text <> '' then
          begin
            o      := R.Left;
            R.Left := Round( R.Left + R.Width / 2 );
            PaintDescription;
            R.Left  := o;
            R.Right := Round( R.Left + R.Width / 2 );
          end;
          PaintCaption( Self, PaintSections[i].TableItems[j].ItemText.Text, R, lbTailTruncation, PaintSections[i].TableItems[j].ItemText.NumberOfLines, FMTextAlign( PaintSections[i].TableItems[j].ItemText.Alignment ) );
        end;
      tvcsValue2:
        begin
          if PaintSections[i].TableItems[j].ItemDescription.Text <> '' then
          begin
            o      := R.Left;
            R.Left := Round( R.Left + R.Width / 3 );
            PaintDescription;
            R.Left  := o;
            R.Right := Round( R.Left + R.Width / 3 );
          end;
          PaintCaption( Self, PaintSections[i].TableItems[j].ItemText.Text, R, lbTailTruncation, PaintSections[i].TableItems[j].ItemText.NumberOfLines, FMTextAlign( PaintSections[i].TableItems[j].ItemText.Alignment ) );
        end;
      tvcsSubtitle:
        begin
          if PaintSections[i].TableItems[j].ItemDescription.Text <> '' then
          begin
            R.Top := R.Top + 25;
            PaintDescription;
            R.Top    := R.Top - 25;
            R.Bottom := R.Top + 25;
          end;
          PaintCaption( Self, PaintSections[i].TableItems[j].ItemText.Text, R, lbTailTruncation, PaintSections[i].TableItems[j].ItemText.NumberOfLines, FMTextAlign( PaintSections[i].TableItems[j].ItemText.Alignment ) );
        end;
    end;
    // Separator
    if j < PaintSections[i].TableItems.Count - 1 then
      if ( Options.SeparatorStyle = tssSingleLine ) or ( ( Options.ViewStyle = tvsGrouped ) and ( Options.SeparatorStyle = tssSingleLineEtched ) ) then
      begin
        Canvas.Stroke.Color := Self.Options.SeparatorColor;
        Canvas.DrawLine( PointF( ItemRect.Left, ItemRect.Bottom ), PointF( ItemRect.Right, ItemRect.Bottom ), 1 );
      end;
  end;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // If there is no section added yet, make an example section,
  // with 3 items to show how the TableView will look in runtime
  if Sections.Count = 0 then
  begin
    NeedFree      := true;
    PaintSections := TSectionCollection.Create( Self );
    PaintSections.Clear;
    with PaintSections.Add do
    begin
      Header.Text := 'Table Header 1';
      Footer.Text := 'Table Footer 1';
      for i       := 1 to 3 do
      begin
        with TableItems.Add do
        begin
          ItemText.Text        := 'Table Item ' + IntToStr( i );
          ItemDescription.Text := 'Description ' + IntToStr( i );
        end;
      end;
    end;
  end
  else
  begin
    NeedFree      := false;
    PaintSections := Sections;
  end;

  // Table BG
  Canvas.Fill.Kind := TBrushKind.Solid;
  if Options.BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := Options.BackgroundColor;
    Canvas.FillRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
  end;
  Canvas.Font.Style := [TFontStyle.fsBold];
  y                 := 0;
  for i             := 0 to PaintSections.Count - 1 do
  begin
    HeaderPos := y; // This Sections's top
    // Get the Items' height
    m     := 0;
    for j := 0 to PaintSections[i].TableItems.Count - 1 do
      if PaintSections[i].TableItems[j].Height = -1 then
        m := m + 44
      else
        m := m + PaintSections[i].TableItems[j].Height;
    // Draw the Items border in one step
    if Options.ViewStyle = tvsGrouped then
    begin
      if m > 0 then
      begin
        ItemRect.Left   := 9;
        ItemRect.Right  := Width - 9;
        ItemRect.Top    := y + 30;
        ItemRect.Bottom := ItemRect.Top + m;
        FooterPos       := ItemRect.Bottom;
        BitmapAsBorderToRect( Self, iOS_GUI_Bitmaps.TableView.Grouped_ItemBorder, 7, ItemRect, TAlphaColors.White );
      end
      else
        FooterPos := y + 30;
    end
    else
      FooterPos := Min( y + iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Height + m, Height - iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Height );

    // Items
    if ( PaintSections[i].Header.Text <> '' ) or ( Options.ViewStyle = tvsGrouped ) then
      y   := y + 30; // Header's height
    for j := 0 to PaintSections[i].TableItems.Count - 1 do
    begin
      if PaintSections[i].TableItems[j].Height = -1 then
        h := 44
      else
        h := PaintSections[i].TableItems[j].Height;
      if Options.ViewStyle = tvsPlain then
        ItemRect := RectF( 0, y, Self.Width, y + h )
      else
        ItemRect := RectF( 10, y, Self.Width - 10, y + h );
      PaintItem;
      y := ItemRect.Bottom + 1;
      if y > Self.Height then
        Break;
    end;

    // Header
    if PaintSections[i].Header.Text <> '' then
      PaintHeaderFooter( HeaderPos, PaintSections[i].Header );

    // Footer
    if PaintSections[i].Footer.Text <> '' then
      PaintHeaderFooter( FooterPos, PaintSections[i].Footer );

    if ( PaintSections[i].Footer.Text <> '' ) or ( Options.ViewStyle = tvsGrouped ) then
      y := y + 30;

    if y > Self.Height then
      Break;
  end;

  if IndexList.Count > 0 then
  begin
    Canvas.Fill.Color := $FF6A737D;
    Canvas.Font.Size  := 10;
    Canvas.Font.Style := [TFontStyle.fsBold];
    m                 := 0;
    for i             := 0 to IndexList.Count - 1 do
    begin
      h := Canvas.TextWidth( IndexList[i] );
      if h > m then
        m := h;
    end;
    if Options.ViewStyle = tvsPlain then
      ItemRect.Right := Width
    else
      ItemRect.Right := Width - 9;
    ItemRect.Left    := ItemRect.Right - m;
    ItemRect.Top     := 0;
    ItemRect.Bottom  := Height;
    if PaintSections[0].Header.Text <> '' then
      if Options.ViewStyle = tvsPlain then
        ItemRect.Top := ItemRect.Top + iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Height
      else
        ItemRect.Top := ItemRect.Top + 30;
    if PaintSections[0].Header.Text <> '' then
      if Options.ViewStyle = tvsPlain then
        ItemRect.Bottom := ItemRect.Bottom - iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Height
      else
        ItemRect.Bottom := ItemRect.Bottom - 30;
    // Space between elements
    h2    := Canvas.TextHeight( 'Ag' );
    h     := Max( ItemRect.Height - IndexList.Count * h2, 0 ) / ( IndexList.Count + 1 );
    y     := ItemRect.Top + h;
    for i := 0 to IndexList.Count - 1 do
    begin
      ItemRect.Top    := y;
      ItemRect.Bottom := ItemRect.Top + h2;
      PaintCaption( Self, IndexList[i], ItemRect, lbClip, 1, TTextAlign.Center );
      y := y + h2 + h;
    end;
  end;

  // Free objects
  if NeedFree then
    PaintSections.Free;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUITableView.ClearBuffer;
var
  I, J    : LongInt;
  Arr     : TArray<TArray<TDPFiOSBaseControl>>;
  ArrFrame: TArray<TFrame>;
begin
  Arr := CellDicCustomViews.Values.ToArray;
  try
    for I := 0 to high( Arr ) do
    begin
      for J := 0 to high( Arr[I] ) do
        Arr[I][J].DisposeOf;
      SetLength( Arr[I], 0 );
    end;
    CellDicCustomViews.Clear;

    // ---------------------------------------------------------------------------
    ArrFrame := CellDicCustomFrames.Values.ToArray;
    for I    := 0 to high( ArrFrame ) do
    begin
      ArrFrame[I].DisposeOf;
    end;
    CellDicCustomFrames.Clear;
  finally
    SetLength( Arr, 0 );
  end;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFUITableView.GetRowCustomFrame( const SectionNo: NativeUInt; const RowNo: NativeUInt ): TFrame;
var
  IndexPath: NSIndexPath;
  Cell     : UITableViewCell;
begin
  IndexPath := TNSIndexPath.Wrap( TNSIndexPath.OCClass.indexPathForRow( RowNo, SectionNo ) );
  Cell      := FUITableViewController.tableView.cellForRowAtIndexPath( IndexPath );
  if Assigned( Cell ) then
  begin
    if CellDicCustomFrames.ContainsKey( ( Cell as ILocalObject ).GetObjectID ) then
      CellDicCustomFrames.TryGetValue( ( Cell as ILocalObject ).GetObjectID, Result );
  end;
  IndexPath := nil;
  Cell      := nil;
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.GetRowCustomViews( const SectionNo: NativeUInt; const RowNo: NativeUInt ): TArray<TDPFiOSBaseControl>;
var
  IndexPath: NSIndexPath;
  Cell     : UITableViewCell;
begin
  IndexPath := TNSIndexPath.Wrap( TNSIndexPath.OCClass.indexPathForRow( RowNo, SectionNo ) );
  Cell      := FUITableViewController.tableView.cellForRowAtIndexPath( IndexPath );
  if Assigned( Cell ) then
  begin
    if CellDicCustomViews.ContainsKey( ( Cell as ILocalObject ).GetObjectID ) then
      CellDicCustomViews.TryGetValue( ( Cell as ILocalObject ).GetObjectID, Result );
  end;
  IndexPath := nil;
  Cell      := nil;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUITableView.StopScroll;
{$IFDEF IOS}
var
  paths: NSArray;
{$ENDIF}
begin
{$IFDEF IOS}
  if assigned( FUITableViewController ) then
    if FUITableViewController.tableView.isDecelerating then
    begin
      paths := FUITableViewController.tableView.indexPathsForVisibleRows;
      FUITableViewController.tableView.scrollToRowAtIndexPath( TNSIndexPath.Wrap( paths.objectAtIndex( 0 ) ), UITableViewScrollPositionTop, False );
    end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.ClearAll( reloadData: Boolean = false; ClearSelections: Boolean = false );
begin
  FTableStartLoading := false;
  if ClearSelections then
    ClearSelectedRows;
  while Sections.Count > 0 do
  begin
    while Sections[0].TableItems.Count > 0 do
    begin
      Sections[0].TableItems.Delete( 0 );
    end;
    Sections.Delete( 0 );
  end;

  if reloadData then
    RefreshNeeded;
  FIsScrolling := false;
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.ClearSelectedRows( const FireEvent: Boolean = False );
{$IFDEF IOS}
var
  r: TArray<TTableSelectedRows>;
  I: LongInt;
{$ENDIF}
begin
{$IFDEF IOS}
  r     := GetSelectedRows;
  for I := 0 to length( r ) - 1 do
    DeSelectRow( r[I].Section, r[I].Row, FireEvent );
  SetLength( r, 0 );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFUITableView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FSearchTextExists  := false;
  FTableStartLoading := false;
  FisDragging        := False;
  FAutoScrollDir     := asdDown;
  FAutoScrollLoop    := true;
  ControlCaption     := 'TableView Control';
  FVisible           := True;
  FAutoScroll        := False;
  FIndexList         := TStringList.Create;
  FVirtualMode       := False;
  FIsScrolling       := False;

  FIndexListBackgroundColor         := TAlphaColors.Null;
  FIndexListTrackingBackgroundColor := TAlphaColors.Null;
  IndexListTextColor                := TAlphaColors.Black;

  FOptions  := TDPFTableViewOptions.Create( Self );
  FSections := TSectionCollection.Create( Self );

{$IFDEF IOS}
  CellDicCustomViews  := TDictionary < Pointer, TArray < TDPFiOSBaseControl >>.Create;
  CellDicCustomFrames := TDictionary<Pointer, TFrame>.Create;

  FTableViewDataSourceDelegate                    := TTableViewDataSourceDelegate.Create( Self );
  FTableViewDelegate                              := TTableViewDelegate.Create( Self );
  FUITableViewController                          := nil;
  FNSTimer                                        := nil;
  FDPFTableSectionGestureRecognizerDelegate       := nil;
  FDPFTableCellGestureRecognizerDelegate          := nil;
  FDPFTableCellLongPressGestureRecognizerDelegate := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUITableView.Destroy;
begin
{$IFDEF IOS}
  FUITableViewController.tableView.setDataSource( nil );
  FUITableViewController.tableView.setDelegate( nil );
  ClearAll( );
  ClearBuffer;
  CellDicCustomViews.DisposeOf;
  CellDicCustomFrames.DisposeOf;

  FStartDelay := IncSecond( Now, 100 );

  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;

  if assigned( FDPFTimerDelegate ) then
    FDPFTimerDelegate.DisposeOf;

  FTableViewDelegate.DisposeOf;
  FTableViewDataSourceDelegate.DisposeOf;
  FDPFTableSectionGestureRecognizerDelegate.DisposeOf;
  FDPFTableCellGestureRecognizerDelegate.DisposeOf;
  FDPFTableCellLongPressGestureRecognizerDelegate.DisposeOf;
{$ENDIF}
  DisposeOfAndNil(FOptions);
  DisposeOfAndNil(FSections);
  DisposeOfAndNil(FIndexList);
  inherited Destroy;
end;

procedure TDPFUITableView.DoAbsoluteChanged;
begin
{$IFDEF IOS}
  if FUITableViewController <> nil then
  begin
    if Options.Scrolling.Horizontal then
    begin
      FUITableViewController.tableView.setFrame( CGRectMake( Position.X, Position.Y, Height * Scale.Y, Width * Scale.X ) );
    end
    else
      FUITableViewController.tableView.setFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUITableView.Loaded;
begin
  FScreenRect := TUIScreen.Wrap( TUIScreen.OCClass.mainScreen ).bounds;
  if FUITableViewController = nil then
  begin
    FUITableViewController := TUITableViewController.Wrap( TUITableViewController.alloc.initWithStyle( UITableViewStyle( Options.ViewStyle ) ) );
    FUIControl             := FUITableViewController;

{$IFDEF IOS6}
    if Options.FRefreshControl.Visible and not Assigned( FDPFRefreshControl ) then
    begin
      FDPFRefreshControlDelegate := TDPFRefreshControlDelegate.Create( Self );
      FDPFRefreshControl         := TUIRefreshControl.Wrap( TUIRefreshControl.Alloc.init );

      Options.FRefreshControl.SetColor( Options.FRefreshControl.Color );
      Options.FRefreshControl.SetBackgroundColor( Options.FRefreshControl.FBackgroundColor );
      Options.RefreshControl.SetText( Options.FRefreshControl.Text );

      FDPFRefreshControl.AddTarget( FDPFRefreshControlDelegate.GetObjectID, // target
        Sel_getUid( 'OnRefreshNeeded' ), // action
        UIControlEventValueChanged ); // event

      FUITableViewController.view.addSubview( FDPFRefreshControl );
      FUITableViewController.setRefreshControl( FDPFRefreshControl );
    end;
{$ENDIF}
  end;

  Resize;

  { if Align = TAlignLayout.alClient then
    FUITableViewController.tableView.SetFrame( UIView( FParentUIControl ).frame )
    else
    FUITableViewController.tableView.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) ); }

  FUITableViewController.tableView.SetRowHeight( Options.FRowHeight );

  Options.SetSeparatorColor( Options.FSeparatorColor );

  Options.SetBackgroundColor( Options.BackgroundColor );

  FUITableViewController.tableView.setHidden( not FVisible );

  FUITableViewController.setWantsFullScreenLayout( true );

  SetIndexListBackgroundColor( FIndexListBackgroundColor );
  SetIndexListTextColor( FIndexListTextColor );
  SetIndexListTrackingBackgroundColor( FIndexListTrackingBackgroundColor );

  FUITableViewController.tableView.setBounces( true );
  FUITableViewController.tableView.setScrollsToTop( Options.Scrolling.FScrollsToTop );
  FUITableViewController.tableView.setScrollEnabled( Options.Scrolling.FEnabled );
  FUITableViewController.tableView.setShowsHorizontalScrollIndicator( Options.Scrolling.FShowsHorizontalScrollIndicator );
  FUITableViewController.tableView.setShowsVerticalScrollIndicator( Options.Scrolling.FShowsVerticalScrollIndicator );
  FUITableViewController.tableView.setBounces( Options.Scrolling.Bounces );
  FUITableViewController.tableView.setAlwaysBounceHorizontal( Options.Scrolling.FAlwaysBounceHorizontal );
  FUITableViewController.tableView.setAlwaysBounceVertical( Options.Scrolling.FAlwaysBounceVertical );
  FUITableViewController.tableView.setDirectionalLockEnabled( Options.Scrolling.DirectionalLockEnabled );

  FUITableViewController.tableView.setAllowsMultipleSelection( Options.AllowsMultipleSelection );
  FUITableViewController.tableView.setAllowsMultipleSelectionDuringEditing( Options.AllowsMultipleSelectionDuringEditing );
  FUITableViewController.tableView.setAllowsSelection( Options.AllowsSelection );
  FUITableViewController.tableView.setAllowsSelectionDuringEditing( Options.AllowsSelectionDuringEditing );

  FUITableViewController.tableView.setSectionFooterHeight( Options.SectionFooterHeight );
  FUITableViewController.tableView.setSectionHeaderHeight( Options.SectionHeaderHeight );

  FUITableViewController.tableView.setPagingEnabled( Options.FPaging );
  FUITableViewController.tableView.setSeparatorStyle( LongInt( Options.SeparatorStyle ) );
  FUITableViewController.tableView.setIndicatorStyle( LongInt( Options.IndicatorStyle ) );

  FUITableViewController.tableView.setEditing( FOptions.FEdition.EditAll, true );
  Options.SetBackgroundImage( Options.FBackgroundImage );

  // FUITableViewController.tableView.setAutoresizingMask( UIViewAutoresizingFlexibleHeight + UIViewAutoresizingFlexibleWidth );
  {
    Horizantal Scrolling
    frame := FUITableViewController.tableView.frame;
    FUITableViewController.tableView.setTransform(  CGAffineTransformRotate(CGAffineTransformIdentity, (90 * PI / 180.0)) );
    FUITableViewController.tableView.setFrame( frame );
  }

  // FUITableViewController.setClearsSelectionOnViewWillAppear(false);
  if Options.Scrolling.Horizontal then
    FUITableViewController.tableView.setTransform( CGAffineTransformRotate( CGAffineTransformIdentity, ROTATE_90_ClockwiseAngle ) );

  FUITableViewController.tableView.setDataSource( ILocalObject( FTableViewDataSourceDelegate ).GetObjectID );
  FUITableViewController.tableView.setDelegate( ILocalObject( FTableViewDelegate ).GetObjectID );

  SetAutoScroll( FAutoScroll );
  AddSubView( Self, ParentControl, FUITableViewController.view );
  Resize;
  // ----------------------------
  // Important

  {
    FDPFTableViewSwipGestureRecognizerDelegate := TDPFTableViewSwipGestureRecognizerDelegate.Create( IOSapi.Uikit.UITableView( FUITableViewController.tableView ) );
    recognizer                                 := TUISwipeGestureRecognizer.Wrap( TUISwipeGestureRecognizer.Alloc.initWithTarget( FDPFTableViewSwipGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'swipeGestureRecognizer:' ) ) );
    recognizer.setDirection( UISwipeGestureRecognizerDirectionLeft );
    FUITableViewController.tableView.addGestureRecognizer( recognizer );
    recognizer.release;
  }

  (*
    //Add a right swipe gesture recognizer
    recognizer = [[UISwipeGestureRecognizer alloc] initWithTarget:self
    action:@selector(handleSwipeRight:)];
    recognizer.delegate = self;
    [recognizer setDirection:(UISwipeGestureRecognizerDirectionRight)];
    [self.tableView addGestureRecognizer:recognizer];
    [recognizer release];
  *)

  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetCellBackgroundColor( Cell: UITableViewCell; BackgroundColor: TAlphaColor );
var
  RR: UIView;
begin

  if BackgroundColor = TAlphaColors.Null then
  begin
    Cell.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
    if FOptions.FViewStyle = TDPFTableViewStyle.tvsGrouped then
    begin
      RR := TUIView.Wrap( TUIView.alloc.initWithFrame( Cell.frame ) );
      RR.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );

      if Options.SeparatorStyle <> tssNone then
      begin
        RR.layer.setBorderColor( TColorToUIColor( Options.SeparatorColor ).CGColor );
        RR.layer.setBorderWidth( 0.6 );
        RR.layer.setCornerRadius( 8 );

      end;
      Cell.setBackgroundView( RR );
      RR.release;
    end;
  end
  else
    Cell.setBackgroundColor( TColorToUIColor( BackgroundColor ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.UpdateCellLabel( CellLabel: UILabel; Item: TDPFTableItemText; UpdateText: Boolean; Font: TDPFFont );
begin
  if CellLabel = nil then
    exit;

  if UpdateText then
    CellLabel.setText( StrToNSStr( Item.Text ) );

  if Item.BackgroundColor = TAlphaColors.Null then
    CellLabel.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    CellLabel.SetBackgroundColor( TColorToUIColor( Item.BackgroundColor ) );
  CellLabel.setHighlightedTextColor( TColorToUIColor( Item.HighlightedColor ) );

  if Item.Color <> TAlphaColors.Null then
    CellLabel.setTextColor( TColorToUIColor( Item.Color ) );

  CellLabel.setNumberOfLines( Item.NumberOfLines );
  CellLabel.setTextAlignment( LongInt( Item.Alignment ) );
  CellLabel.setFont( Font._UIFont );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUITableView.RefreshNeeded;
begin
  inherited;
{$IFDEF IOS}
  FTableStartLoading := false;

  if Assigned( FUITableViewController ) and Assigned( FUITableViewController.tableView ) then
    FUITableViewController.tableView.reloadData;
{$ENDIF}
end;

//SZ: now done in DoAbsoluteChanged
// ------------------------------------------------------------------------------
//procedure TDPFUITableView.Resize;
//begin
//{$IFDEF IOS}
//  if FUITableViewController <> nil then
//  begin
//    if Options.Scrolling.Horizontal then
//    begin
//      FUITableViewController.tableView.setFrame( CGRectMake( Position.X, Position.Y, Height * Scale.Y, Width * Scale.X ) );
//    end
//    else
//      FUITableViewController.tableView.setFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
//  end;
//{$ENDIF}
//  inherited;
//end;

// ------------------------------------------------------------------------------
//procedure TDPFUITableView.Move;
//begin
//  Resize;
//end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetSearchCategory( const Value: string );
begin
  FSearchCategory   := Value;
  FSearchTextExists := Value + FSearchText <> '';
{$IFDEF IOS}
  FUITableViewController.tableView.reloadData;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetSearchText( const Value: string );
begin
  FSearchText       := Value;
  FSearchTextExists := Value + FSearchCategory <> '';
{$IFDEF IOS}
  FUITableViewController.tableView.reloadData;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetSections( const Value: TSectionCollection );
begin
  FSections.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.EndRefreshing;
begin
{$IFDEF IOS6}
  if Assigned( FDPFRefreshControl ) then
    FDPFRefreshControl.endRefreshing;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.BOF: Boolean;
{$IFDEF IOS}
var
  cnt         : LongInt;
  NSI         : NSIndexPath;
  cellRect    : CGRect;
  SecNo, RowNo: LongWord;
{$ENDIF}
begin
  result := false;
{$IFDEF IOS}
  cnt := FUITableViewController.tableView.indexPathsForVisibleRows.count;
  if cnt > 0 then
  begin
    // Idxes[0] = Section No
    // Idxes[1] = Row Index No
    NSI := TNSIndexPath.Wrap( FUITableViewController.tableView.indexPathsForVisibleRows.objectAtIndex( cnt - 1 ) );
    getSecRowIndex( NSI, SecNo, RowNo );
    if FSearchTextExists then
    begin
      SecNo := FSearchMap[RowNo].Section;
      RowNo := FSearchMap[RowNo].Row;
    end;

    cellRect := FUITableViewController.tableView.rectForRowAtIndexPath( NSI );

    if CGRectContainsRect( CGRectOffset( FUITableViewController.tableView.frame, FUITableViewController.tableView.contentOffset.x, FUITableViewController.tableView.contentOffset.y ), cellRect ) > 0 then
      if ( SecNo = 0 ) and ( RowNo = 0 ) then
        result := true;

  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.EOF: Boolean;
{$IFDEF IOS}
var
  cnt            : LongInt;
  SECount, ICount: LongWord;
  NSI            : NSIndexPath;
  cellRect       : CGRect;
  SecNo, RowNo   : LongWord;
{$ENDIF}
begin
  result := false;
{$IFDEF IOS}
  cnt := FUITableViewController.tableView.indexPathsForVisibleRows.count;
  if cnt > 0 then
  begin
    // Idxes[0] = Section No
    // Idxes[1] = Row Index No
    NSI := TNSIndexPath.Wrap( FUITableViewController.tableView.indexPathsForVisibleRows.objectAtIndex( cnt - 1 ) );
    getSecRowIndex( NSI, SecNo, RowNo );

    SECount := Sections.Count;
    ICount  := Sections[SECount - 1].TableItems.Count;

    if FSearchTextExists then
    begin
      SecNo   := FSearchMap[RowNo].Section;
      RowNo   := FSearchMap[RowNo].Row;
      SECount := 1;
      ICount  := Length( FSearchMap );
    end;

    cellRect := FUITableViewController.tableView.rectForRowAtIndexPath( NSI );

    if CGRectContainsRect( CGRectOffset( FUITableViewController.tableView.frame, FUITableViewController.tableView.contentOffset.x, FUITableViewController.tableView.contentOffset.y ), cellRect ) > 0 then
      if ( SecNo = SECount - 1 ) and ( RowNo = ICount - 1 ) then
        result := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.GetPageNo: LongInt;
begin
  result := 0;
{$IFDEF IOS}
  if not Assigned( FUITableViewController ) or ( FUITableViewController.tableView.frame.size.width = 0 ) then
    exit;
  if Options.Scrolling.Horizontal then
    Result := round( FUITableViewController.tableView.contentOffset.y / FUITableViewController.tableView.frame.size.width )
  else
    Result := round( FUITableViewController.tableView.contentOffset.y / FUITableViewController.tableView.frame.size.height );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.GetRow( ASection, ARow: LongInt ): TTableItem;
begin
  Result := nil;
  if ( ASection > -1 ) and ( ARow > -1 ) { and ( ASection < Sections.Count ) and ( ARow < Sections[ASection].TableItems.Count ) } then
    Result := Sections[ASection].TableItems[ARow];
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.GetSearchItemCount: LongInt;
begin
{$IFDEF IOS}
  result := Length( FSearchMap );
{$ELSE}
  result := 0;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUITableView.DeleteRow( const Section: Int64; const Row: Int64 );
var
  editingStyleForRowAtIndexPath: NSIndexPath07;
begin
  editingStyleForRowAtIndexPath := TNSIndexPath07.Wrap( TNSIndexPath.OCClass.indexPathForRow( Row, Section ) );
  FTableViewDataSourceDelegate.tableView( FUITableViewController.tableView, UITableViewCellEditingStyleDelete, editingStyleForRowAtIndexPath );
  FUITableViewController.tableView.reloadData;
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.GetSelectedRows: TArray<TTableSelectedRows>;
var
  Idxes : array of NSUInteger;
  I, Cnt: NSUInteger;
begin
  // Idxes[0] = Section No
  // Idxes[1] = Row Index No
  SetLength( result, 0 );
  if not Assigned( FUITableViewController ) or not Assigned( FUITableViewController.tableView ) or not Assigned( FUITableViewController.tableView.indexPathForSelectedRow ) then
    exit;

  Cnt := FUITableViewController.tableView.indexPathForSelectedRow.length;
  if Cnt = 0 then
    exit;

  SetLength( Idxes, Cnt );
  try
    SetLength( Result, Cnt div 2 );
    FUITableViewController.tableView.indexPathForSelectedRow.getIndexes( NSUInteger( @Idxes[0] ) );
    I := 0;
    while I < cnt do
    begin
      Result[i].Section := Idxes[i + 0];
      Result[i].Row     := Idxes[i + 1];
      Inc( I, 2 );
    end;

  finally
    SetLength( Idxes, 0 );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUITableView.BeginRefreshing;
begin
{$IFDEF IOS6}
  if Assigned( FDPFRefreshControl ) then
    FDPFRefreshControl.beginRefreshing;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.ScrollToPos( x, y: Single );
begin
{$IFDEF IOS}
  FUITableViewController.tableView.setContentOffset( CGPointMake( x, y ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.ScrollToRow( Section, Row: LongInt; ScrollPosition: DPFTableViewScrollPosition = DPFTableViewScrollPosition.spMiddle; Animate: Boolean = true );
{$IFDEF IOS}
var
  NSIndex: NSIndexPath;
{$ENDIF}
begin
{$IFDEF IOS}
  NSIndex := TNSIndexPath.Wrap( TNSIndexPath.OCClass.indexPathForRow( Row, Section ) );
  FUITableViewController.tableView.scrollToRowAtIndexPath( NSIndex, NativeUInt( ScrollPosition ), Animate );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUITableView.RowIsEditing( SectionNo: LongInt; RowNo: LongInt ): Boolean;
{$IFDEF IOS}
var
  NSIndex: NSIndexPath;
  C      : UITableViewCell;
{$ENDIF}
begin
  result := false;
{$IFDEF IOS}
  NSIndex := TNSIndexPath.Wrap( TNSIndexPath.OCClass.indexPathForRow( RowNo, SectionNo ) );
  if Assigned( NSIndex ) then
  begin
    C := FUITableViewController.tableView.cellForRowAtIndexPath( NSIndex );
    if Assigned( C ) then
      result := C.isEditing;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SelectRow( const Section: LongInt; const Row: LongInt; const FireEvent: Boolean = true; ScrollPosition: TDPFTableViewScrollPosition = TDPFTableViewScrollPosition.tvspNone );
{$IFDEF IOS}
var
  indexPath: NSIndexPath;
{$ENDIF}
begin
{$IFDEF IOS}
  if ( Section > -1 ) and ( Row > -1 ) and ( Section < Sections.Count ) and ( Row < Sections[Section].TableItems.Count ) then
  begin
    indexPath := TNSIndexPath14.Wrap( TNSIndexPath.OCClass.indexPathForRow( row, Section ) );
    FUITableViewController.tableView.reloadRowsAtIndexPaths( FUITableViewController.tableView.indexPathsForVisibleRows, UITableViewRowAnimationNone );
    FUITableViewController.tableView.selectRowAtIndexPath( indexPath, true, nativeUInt( ScrollPosition ) );
    if FireEvent then
      FTableViewDelegate.tableView( FUITableViewController.tableView, indexPath );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.DeSelectRow( const Section: LongInt; const Row: LongInt; const FireEvent: Boolean = true );
{$IFDEF IOS}
var
  indexPath: NSIndexPath04;
{$ENDIF}
begin
{$IFDEF IOS}
  if ( Section > -1 ) and ( Row > -1 ) and ( Section < Sections.Count ) and ( Row < Sections[Section].TableItems.Count ) then
  begin
    indexPath := TNSIndexPath04.Wrap( TNSIndexPath.OCClass.indexPathForRow( row, Section ) );
    FUITableViewController.tableView.reloadRowsAtIndexPaths( FUITableViewController.tableView.indexPathsForVisibleRows, UITableViewRowAnimationNone );
    FUITableViewController.tableView.deselectRowAtIndexPath( indexPath, true );
    if FireEvent then
      FTableViewDelegate.tableView( FUITableViewController.tableView, indexPath );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetAutoScroll( const Value: Boolean );
begin
  FAutoScroll := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    if Value then
    begin
      if FNSTimer = nil then
      begin
        if not assigned( FDPFTimerDelegate ) then
          FDPFTimerDelegate := TDPFTableViewTimerDelegate.Create( Self );
        FNSTimer            := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( 0.1, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );
      end;
    end
    else if FNSTimer <> nil then
    begin
      FNSTimer.invalidate;
      FNSTimer := nil;
      // FDPFTimerDelegate.DisposeOf;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetIndexList( const Value: TStrings );
begin
  FIndexList.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetIndexListBackgroundColor( const Value: TAlphaColor );
begin
  FIndexListBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FUITableViewController ) and ( TOSVersion.Major >= 7.0 ) then
  begin
    if Value = TAlphaColors.Null then
      FUITableViewController.tableView.setSectionIndexBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUITableViewController.tableView.setSectionIndexBackgroundColor( TColorToUIColor( value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetIndexListTextColor( const Value: TAlphaColor );
begin
  FIndexListTextColor := Value;
{$IFDEF IOS}
  if Assigned( FUITableViewController ) and ( TOSVersion.Major >= 6.0 ) then
  begin
    if Value = TAlphaColors.Null then
      FUITableViewController.tableView.setSectionIndexColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUITableViewController.tableView.setSectionIndexColor( TColorToUIColor( value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetIndexListTrackingBackgroundColor( const Value: TAlphaColor );
begin
  FIndexListTrackingBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FUITableViewController ) and ( TOSVersion.Major >= 6.0 ) then
  begin
    if Value = TAlphaColors.Null then
      FUITableViewController.tableView.setSectionIndexTrackingBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUITableViewController.tableView.setSectionIndexTrackingBackgroundColor( TColorToUIColor( value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUITableView.SetOptions( const Value: TDPFTableViewOptions );
begin
  FOptions.Assign( Value );
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TTableViewDataSourceDelegate }
constructor TTableViewDataSourceDelegate.Create( AParent: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := AParent;
end;

// ------------------------------------------------------------------------------
destructor TTableViewDataSourceDelegate.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.numberOfSectionsInTableView( tableView: UITableView ): NSInteger;
var Rs: LongInt;
begin
{$IFDEF DEBUG}
  DPFNSLog( '1' );
{$ENDIF}
  Result := -1;
  if Assigned( FDPFUITableView.OnGetNumberOfSections ) then
  begin
    FDPFUITableView.FVirtualMode := true;
    FDPFUITableView.OnGetNumberOfSections( FDPFUITableView, FDPFUITableView.FSearchTextExists, Rs );
    Result := Rs ;
  end
  else
  begin
    if FDPFUITableView.FSearchTextExists then
      result := 1
    else
      result := FDPFUITableView.FSections.Count;
  end;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; numberOfRowsInSection: NSInteger ): NSInteger;
var
  I, J, Rs: LongInt;
  Flag: Boolean;
begin
{$IFDEF DEBUG}
  DPFNSLog( '2' );
{$ENDIF}
  result := 0;
  if Assigned( FDPFUITableView.OnGetNumberOfRowsInSection ) then
  begin
    FDPFUITableView.FVirtualMode := true;
    Rs := 0;
    FDPFUITableView.OnGetNumberOfRowsInSection( FDPFUITableView, FDPFUITableView.FSearchTextExists, numberOfRowsInSection, Rs );
    Result := Rs;
  end
  else
  begin
    if FDPFUITableView.FSearchTextExists then
    begin
      SetLength( FDPFUITableView.FSearchMap, 0 );
      for I   := 0 to FDPFUITableView.FSections.Count - 1 do
        for J := 0 to FDPFUITableView.FSections[I].TableItems.Count - 1 do
        begin
          Flag := true;
          if FDPFUITableView.FSearchCategory <> '' then
            Flag := sameText( FDPFUITableView.FSections[I].TableItems[J].Category, FDPFUITableView.FSearchCategory );

          if FDPFUITableView.FSearchText <> '' then
          begin
            if FDPFUITableView.Options.SearchBar.SearchKind = TDPFTableViewSearchKind.skStartsWith then
              Flag := Flag and ( FDPFUITableView.Options.SearchBar.SearchInItemText and FDPFUITableView.FSections[I].TableItems[J].ItemText.Text.StartsWith( FDPFUITableView.FSearchText, true ) or ( FDPFUITableView.Options.SearchBar.SearchInItemDesc and FDPFUITableView.FSections[I].TableItems[J].ItemDescription.Text.StartsWith( FDPFUITableView.FSearchText,
                true ) ) )
            else
              Flag := Flag and ( FDPFUITableView.Options.SearchBar.SearchInItemText and System.StrUtils.ContainsText( FDPFUITableView.FSections[I].TableItems[J].ItemText.Text, FDPFUITableView.FSearchText ) or ( FDPFUITableView.Options.SearchBar.SearchInItemDesc and System.StrUtils.ContainsText( FDPFUITableView.FSections[I].TableItems[J].ItemDescription.Text,
                FDPFUITableView.FSearchText ) ) );
          end;

          if Flag then
          begin
            result := result + 1;
            SetLength( FDPFUITableView.FSearchMap, result );
            FDPFUITableView.FSearchMap[result - 1].Section := I;
            FDPFUITableView.FSearchMap[result - 1].Row     := J;
          end;
        end;
    end
    else
      result := FDPFUITableView.FSections[numberOfRowsInSection].TableItems.Count
  end;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; sectionForSectionIndexTitle: NSString; atIndex: NSInteger ): NSInteger;
var
  I, Idx   : LongInt;
  indexPath: NSIndexPath;
begin
{$IFDEF DEBUG}
  DPFNSLog( '3' );
{$ENDIF}
  Idx   := -1;
  for I := 0 to FDPFUITableView.Sections.Count - 1 do
  begin
    if FDPFUITableView.Sections[I].Header.Text.StartsWith( FDPFUITableView.IndexList[atIndex] ) then
    begin
      Idx := I;
      Break;
    end;
  end;
  if Idx <> -1 then
  begin
    indexPath := TNSIndexPath.Wrap( TNSIndexPath.OCClass.indexPathForRow( 0, Idx ) );
    FDPFUITableView.FUITableViewController.tableView.scrollToRowAtIndexPath( indexPath, UITableViewScrollPositionTop, true );
  end;
  result := -1;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.sectionIndexTitlesForTableView( tableView: UITableView ): NSArray;
var
  Ar: NSMutableArray;
  I : LongInt;
begin
{$IFDEF DEBUG}
  DPFNSLog( '4' );
{$ENDIF}
  Result := nil;

  if ( FDPFUITableView.FSearchTextExists ) or ( FDPFUITableView.IndexList.Count = 0 ) then
    exit;

  Ar    := TNSMutableArray.Create;
  for I := 0 to FDPFUITableView.IndexList.Count - 1 do
  begin
    Ar.addObject( ( StrToNSStr( FDPFUITableView.IndexList[I] ) as ILocalObject ).GetObjectID );
  end;
  result := Ar;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; canEditRowAtIndexPath: NSIndexPath01 ): Boolean; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '5' );
{$ENDIF}
  result := FDPFUITableView.Options.Edition.CellEdit and not FDPFUITableView.FSearchTextExists;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; canMoveRowAtIndexPath: NSIndexPath02 ): Boolean; cdecl;
var
  SecNo, RowNo: LongWord;
begin
{$IFDEF DEBUG}
  DPFNSLog( '6' );
{$ENDIF}
  result := FDPFUITableView.Options.Edition.Moving and not FDPFUITableView.FSearchTextExists and ( canMoveRowAtIndexPath.length > 0 );
  if not result then
    exit;

  getSecRowIndex( canMoveRowAtIndexPath, SecNo, RowNo );

  if result and Assigned( FDPFUITableView.FOnCanMoveRow ) then
    FDPFUITableView.FOnCanMoveRow( FDPFUITableView, SecNo, RowNo, Result );
end;

// ------------------------------------------------------------------------------
procedure TTableViewDataSourceDelegate.tableView( tableView: UITableView; moveRowAtIndexPath: NSIndexPath; toIndexPath: NSIndexPath ); cdecl;
var
  IdxsFrom, IdxsTo: array of NSUInteger;
  TI              : TTableItem;
begin
{$IFDEF DEBUG}
  DPFNSLog( '7' );
{$ENDIF}
  // Idxes[0] = Section No
  // Idxes[1] = Row Index No

  if ( moveRowAtIndexPath.length = 0 ) or ( toIndexPath.length = 0 ) then
    exit;

  SetLength( IdxsFrom, moveRowAtIndexPath.length );
  try
    moveRowAtIndexPath.getIndexes( NSUInteger( @IdxsFrom[0] ) );

    SetLength( IdxsTo, toIndexPath.length );
    try
      toIndexPath.getIndexes( NSUInteger( @IdxsTo[0] ) );

      if IdxsFrom[0] = IdxsTo[0] then
        FDPFUITableView.Sections[IdxsFrom[0]].TableItems[IdxsFrom[1]].Index := IdxsTo[1]
      else
      begin
        TI := FDPFUITableView.Sections[IdxsTo[0]].TableItems.Add;
        TI.Assign( FDPFUITableView.Sections[IdxsFrom[0]].TableItems[IdxsFrom[1]] );
        TI.Index := IdxsTo[1];
        FDPFUITableView.Sections[IdxsFrom[0]].TableItems.Delete( IdxsFrom[1] );
      end;

      if Assigned( FDPFUITableView.FOnAfterMoveRow ) then
        FDPFUITableView.FOnAfterMoveRow( FDPFUITableView, IdxsFrom[0], IdxsFrom[1], IdxsTo[0], IdxsTo[1] );

    finally
      SetLength( IdxsFrom, 0 );
    end;
  finally
    SetLength( IdxsTo, 0 );
  end;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; titleForFooterInSection: NSInteger2 ): IOSapi.Foundation.NSString; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '8' );
{$ENDIF}
  result := nil;
  if FDPFUITableView.FSearchTextExists then
    exit;
  if FDPFUITableView.Sections[titleForFooterInSection.val].Footer.Text <> '' then
    result := StrToNSStr( FDPFUITableView.Sections[titleForFooterInSection.val].Footer.Text );
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; titleForHeaderInSection: NSInteger1 ): IOSapi.Foundation.NSString; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '9' );
{$ENDIF}
  result := nil;
  if FDPFUITableView.FSearchTextExists then
    exit;
  if FDPFUITableView.Sections[titleForHeaderInSection.val].Header.Text <> '' then
    result := StrToNSStr( FDPFUITableView.Sections[titleForHeaderInSection.val].Header.Text );
end;

// ------------------------------------------------------------------------------
procedure TTableViewDataSourceDelegate.tableView( tableView: UITableView; commitEditingStyle: UITableViewCellEditingStyle; forRowAtIndexPath: NSIndexPath ); cdecl;
var
  Arr         : NSArray;
  SecNo, RowNo: LongWord;
  ACanDelete  : Boolean;
begin
{$IFDEF DEBUG}
  DPFNSLog( '10' );
{$ENDIF}
  if forRowAtIndexPath.length = 0 then
    exit;

  getSecRowIndex( forRowAtIndexPath, SecNo, RowNo );
  if commitEditingStyle = UITableViewCellEditingStyleDelete then
  begin

    ACanDelete := True;
    if Assigned( FDPFUITableView.OnCanDeleteRow ) then
    begin
      FDPFUITableView.OnCanDeleteRow( FDPFUITableView, SecNo, RowNo, ACanDelete );
      if not ACanDelete then
        Exit;
    end;

    if Assigned( FDPFUITableView.FOnItemDelete ) then
      FDPFUITableView.FOnItemDelete( FDPFUITableView, SecNo, RowNo );

    FDPFUITableView.Sections[SecNo].TableItems.Delete( RowNo );

    Arr := TNSArray.Wrap( TNSArray.OCClass.arrayWithObject( ( forRowAtIndexPath as ILocalObject ).GetObjectID ) );

    tableView.beginUpdates;
    tableView.deleteRowsAtIndexPaths( Arr, UITableViewRowAnimationAutomatic );
    tableView.endUpdates;
  end
  else if commitEditingStyle = UITableViewCellEditingStyleInsert then
  begin
    if Assigned( FDPFUITableView.FOnItemInsert ) then
      FDPFUITableView.FOnItemInsert( FDPFUITableView, SecNo, RowNo );
  end;
end;

// ------------------------------------------------------------------------------
function TTableViewDataSourceDelegate.tableView( tableView: UITableView; cellForRowAtIndexPath: NSIndexPath ): UITableViewCell;
const
  CellStyleName: array [TTableViewCellStyle] of string = ( 'tvcsDefault', 'tvcsValue1', 'tvcsValue2', 'tvcsSubtitle' );
var
  I                             : LongInt;
  Res                           : Pointer;
  TableItem                     : TTableItem;
  Image                         : UIImage;
  ReUsable                      : Boolean;
  SecNo, RowNo                  : Longword;
  CellIdentifier                : string;
  CustomViews                   : TArray<TDPFiOSBaseControl>;
  CustomFrame                   : TFrame;
  C                             : UIView;
  CustomCellHandled             : Boolean;
  FTapGestureRecognizer         : UITapGestureRecognizer;
  FTapLongPressGestureRecognizer: UILongPressGestureRecognizer;
  GestureCnt                    : Byte;
begin
{$IFDEF DEBUG}
  DPFNSLog( '11' );
{$ENDIF}
  result := nil;
  try
    if cellForRowAtIndexPath.length = 0 then
      exit;

    getSecRowIndex( cellForRowAtIndexPath, SecNo, RowNo );

    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;
    TableItem := FDPFUITableView.Row[SecNo, RowNo];

    if Assigned( FDPFUITableView.OnNeedCellIdentifier ) then
      FDPFUITableView.OnNeedCellIdentifier( FDPFUITableView, SecNo, RowNo, CellIdentifier );

    CellIdentifier := 'DPF.iOS.Tablview.Cell.' + CellStyleName[TableItem.Style] + '.' + CellIdentifier;
    Res            := tableView.dequeueReusableCellWithIdentifier( StrToNSStr( CellIdentifier ) );
    if Res = nil then
    begin
      result   := TUITableViewCell.Wrap( TUITableViewCell.alloc.initWithStyle( LongInt( TableItem.Style ), StrToNSStr( CellIdentifier ) ) );
      ReUsable := False;

    end
    else
    begin
      result   := TUITableViewCell.Wrap( Res );
      ReUsable := True;
    end;

    if FDPFUITableView.Options.Scrolling.Horizontal then
      result.setTransform( CGAffineTransformRotate( CGAffineTransformIdentity, ROTATE_90_ClockwiseAngleN ) );

    CustomCellHandled := false;
    // -----------------------------------------------------------------------
    // Check Frame Data
    if Assigned( FDPFUITableView.FOnApplyFrameData ) then
    begin
      result.setTag( 1 );
      if FDPFUITableView.CellDicCustomFrames.ContainsKey( ( result as ILocalObject ).GetObjectID ) then
        FDPFUITableView.CellDicCustomFrames.TryGetValue( ( result as ILocalObject ).GetObjectID, CustomFrame );

      if ReUsable and Assigned( CustomFrame ) then
        for I := 0 to CustomFrame.ComponentCount - 1 do
          ( CustomFrame.Components[i] as TDPFiOSBaseControl ).UIControl := result.contentView.viewWithTag( TAG_BASE + i );

      CustomCellHandled := true;
      FDPFUITableView.FOnApplyFrameData( FDPFUITableView, SecNo, RowNo, TableItem, CustomFrame, CustomCellHandled );
      FDPFUITableView.CellDicCustomFrames.AddOrSetValue( ( result as ILocalObject ).GetObjectID, CustomFrame );
      if not ReUsable then
        for I := 0 to CustomFrame.ComponentCount - 1 do
        begin
          UIView( ( CustomFrame.Components[i] as TDPFiOSBaseControl ).UIControl ).setTag( TAG_BASE + i );
          if not( CustomFrame.Components[i] as TDPFiOSBaseControl ).isLoaded then
            ( CustomFrame.Components[i] as TDPFiOSBaseControl ).Loaded;
          ( CustomFrame.Components[i] as TDPFiOSBaseControl ).AutoRelease := True;
          if not ReUsable then
            result.contentView.addSubview( UIView( ( CustomFrame.Components[i] as TDPFiOSBaseControl ).UIControl ) );
          ( CustomFrame.Components[i] as TDPFiOSBaseControl ).DoResize;
        end;

    end
    // -----------------------------------------------------------------------
    // Check Draw Cell Event
    else if Assigned( FDPFUITableView.FOnDrawCell ) then
    begin
      result.setTag( 1 );
      if FDPFUITableView.CellDicCustomViews.ContainsKey( ( result as ILocalObject ).GetObjectID ) then
        FDPFUITableView.CellDicCustomViews.TryGetValue( ( result as ILocalObject ).GetObjectID, CustomViews );

      if ReUsable then
        for I                      := 0 to high( CustomViews ) do
          CustomViews[i].UIControl := result.contentView.viewWithTag( TAG_BASE + i );

      CustomCellHandled := true;
      FDPFUITableView.FOnDrawCell( FDPFUITableView, SecNo, RowNo, TableItem, CustomViews, CustomCellHandled );
      FDPFUITableView.CellDicCustomViews.AddOrSetValue( ( result as ILocalObject ).GetObjectID, CustomViews );

      if not ReUsable then
        if Length( CustomViews ) > 0 then
        begin
          for I := 0 to high( CustomViews ) do
          begin
            C := FDPFUITableView.GetControlView( CustomViews[i].UIControl );
            if C <> nil then
              C.setTag( TAG_BASE + i );
            if not CustomViews[i].isLoaded then
              CustomViews[i].Loaded;
            TDPFiOSBaseControl( CustomViews[i] ).AutoRelease := True;
            if not ReUsable then
              result.contentView.addSubview( C );
          end;
        end;

    end;
    // -----------------------------------------------------------------------
    // Init Standard Table Cell
    // else
    if not CustomCellHandled then

    begin
      // --------------------------------------------------------------------------
      // Right To Left
      // Result.setTransform( CGAffineTransformMakeRotation( 180 * 0.0174532925 ) );
      // Result.textLabel.setTransform( CGAffineTransformMakeRotation( 180 * 0.0174532925 ) );
      // Result.detailTextLabel.setTransform( CGAffineTransformMakeRotation( -PI ) );

      // Result.setAutoresizingMask( UIViewAutoresizingFlexibleLeftMargin Or UIViewAutoresizingFlexibleRightMargin );
      // --------------------------------------------------------------------------
      result.setTag( 0 );
      if ( TableItem.ImageName <> '' ) and ( TableItem.Style <> tvcsValue2 ) then
      begin
        if TableItem.isURL then
          Image := TUIImage.Wrap( TUIImage.OCClass.imageWithData( TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( StrToNSStr( TableItem.ImageName ) ) ) ) ) ) )
        else
          Image := TUIImage.Wrap( TUIImage.OCClass.imageNamed( StrToNSStr( TableItem.ImageName ) ) );

        if Assigned( Image ) and ( Image.size.width > 0.99 ) and ( Image.size.height > 0.99 ) then
        begin
          result.imageView.setImage( Image );
          result.imageView.setAutoresizingMask( UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight );
          result.imageView.setcontentMode( UIViewContentModeScaleToFill );
          Image := nil;
        end;
      end
      else if not TableItem.BitMap.IsEmpty then
      begin
        Image := BitmapToUIImage( TableItem.BitMap );
        result.imageView.setImage( Image );
        Image.release;
        Image := nil;
      end;

      // ----------------------------------------------------------------------------
      // Main Label
      FDPFUITableView.UpdateCellLabel( result.textLabel, TableItem.ItemText, true, FDPFUITableView.Options.TextFont );

      // ----------------------------------------------------------------------------
      // Detail Label
      if Assigned( result.detailTextLabel ) then
      begin
        FDPFUITableView.UpdateCellLabel( result.detailTextLabel, TableItem.ItemDescription, true, FDPFUITableView.Options.DescriptionFont );
      end;

      // result.setAutoresizesSubviews( True );
    end;

    FDPFUITableView.SetCellBackgroundColor( result, TableItem.BackgroundColor );
    result.setSelectionStyle( LongInt( FDPFUITableView.Options.FSelectionStyle ) );

    result.setAccessoryType( LongInt( TableItem.AccessoryType ) );
    result.setEditingAccessoryType( LongInt( TableItem.EditingAccessoryType ) );
    result.setHidesAccessoryWhenEditing( TableItem.HidesAccessoryWhenEditing );
    result.setClipsToBounds( true );
    result.contentView.setClipsToBounds( true );

    result.setSelectedBackgroundView( FDPFUITableView.Options.selectedBackgroundView );

    GestureCnt := 0;
    // Cell Tap GestureRecognizers
    if Assigned( FDPFUITableView.FOnCellClick ) and ( not assigned( result.gestureRecognizers ) or ( result.gestureRecognizers.count = GestureCnt ) ) then
    begin
      if FDPFUITableView.FDPFTableCellGestureRecognizerDelegate = nil then
        FDPFUITableView.FDPFTableCellGestureRecognizerDelegate := TDPFTableCellGestureRecognizerDelegate.Create( FDPFUITableView );
      FTapGestureRecognizer := TUITapGestureRecognizer.Wrap( TUITapGestureRecognizer.Alloc.initWithTarget( FDPFUITableView.FDPFTableCellGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'handleSingleTap:' ) ) );
      result.addGestureRecognizer( FTapGestureRecognizer );
      FTapGestureRecognizer.release;
      GestureCnt := 1;
    end;

    // Cell LongPress GestureRecognizers
    if Assigned( FDPFUITableView.FOnCellLongClick ) and ( not assigned( result.gestureRecognizers ) or ( result.gestureRecognizers.count = GestureCnt ) ) then
    begin
      if FDPFUITableView.FDPFTableCellLongPressGestureRecognizerDelegate = nil then
        FDPFUITableView.FDPFTableCellLongPressGestureRecognizerDelegate := TDPFTableCellLongPressGestureRecognizerDelegate.Create( FDPFUITableView );
      FTapLongPressGestureRecognizer := TUILongPressGestureRecognizer.Wrap( TUILongPressGestureRecognizer.Alloc.initWithTarget( FDPFUITableView.FDPFTableCellLongPressGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'handleLongPress:' ) ) );
      FTapLongPressGestureRecognizer.setMinimumPressDuration( 1.0 );
      result.addGestureRecognizer( FTapLongPressGestureRecognizer );
      FTapLongPressGestureRecognizer.release;
    end;

  except
    on E: Exception do
      DPFNSLog( 'Error 11: ' + E.Message );
  end;
end;

// ------------------------------------------------------------------------------
{ TTableViewDelegate }
constructor TTableViewDelegate.Create( AParent: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := AParent;
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; willDisplayCell: UITableViewCell; forRowAtIndexPath: NSIndexPath ); cdecl;
var
  TableItem   : TTableItem;
  SecNo, RowNo: LongWord;
begin
{$IFDEF DEBUG}
  DPFNSLog( '12' );
{$ENDIF}
  FDPFUITableView.FTableStartLoading := true;
  if forRowAtIndexPath.length = 0 then
    exit;

  try
    getSecRowIndex( forRowAtIndexPath, SecNo, RowNo );

    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;
    TableItem := FDPFUITableView.FSections[SecNo].TableItems[RowNo];

    FDPFUITableView.UpdateCellLabel( willDisplayCell.textLabel, TableItem.ItemText, willDisplayCell.tag = 0, FDPFUITableView.Options.TextFont );

    if TableItem.Style = tvcsSubtitle then
      FDPFUITableView.UpdateCellLabel( willDisplayCell.detailTextLabel, TableItem.ItemDescription, true, FDPFUITableView.Options.DescriptionFont );
  except
    on e: exception do
      DPFNSLog( 'tableView.willDisplayCell ' + FDPFUITableView.Name + 'SecNo: ' + IntToSTr( SecNo ) + ' RoNo: ' + IntToStr( RowNo ) + ' Error: ' + e.Message );
  end;

end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.scrollViewDidEndScrollingAnimation( scrollView: UIScrollView );
begin
{$IFDEF DEBUG}
  DPFNSLog( '13-0' );
{$ENDIF}
  FDPFUITableView.FIsScrolling := false;
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.scrollViewWillBeginDecelerating( scrollView: UIScrollView );
begin
{$IFDEF DEBUG}
  DPFNSLog( '13' );
{$ENDIF}
  FDPFUITableView.FIsScrolling := true;
  FDPFUITableView.CurOffSet    := DPFNSPoint( scrollView.contentOffset );
  if Assigned( FDPFUITableView.OnScrollBegin ) then
    FDPFUITableView.OnScrollBegin( FDPFUITableView );
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.scrollViewDidEndDecelerating( scrollView: UIScrollView );
begin
{$IFDEF DEBUG}
  DPFNSLog( '13-1' );
{$ENDIF}
  FDPFUITableView.FIsScrolling := false;
  if Assigned( FDPFUITableView.OnScrollEnd ) then
    FDPFUITableView.OnScrollEnd( FDPFUITableView, FDPFUITableView.CurOffSet, DPFNSPoint( scrollView.contentOffset ) );
  if Assigned( FDPFUITableView.FOnPageChanged ) and FDPFUITableView.Options.Paging then
    FDPFUITableView.FOnPageChanged( FDPFUITableView, FDPFUITableView.PageNo );
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.scrollViewWillBeginDragging( scrollView: UIScrollView );
begin
{$IFDEF DEBUG}
  DPFNSLog( '14' );
{$ENDIF}
  FDPFUITableView.FisDragging := True;
  FDPFUITableView.CurOffSet   := DPFNSPoint( scrollView.contentOffset );
  if Assigned( FDPFUITableView.OnScrollBeginDragging ) then
    FDPFUITableView.OnScrollBeginDragging( FDPFUITableView );

  if FDPFUITableView.FAutoScroll then
    FDPFUITableView.FStartDelay := IncSecond( Now, 10 );
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: CGPoint );
begin
{$IFDEF DEBUG}
  DPFNSLog( '15' );
{$ENDIF}
  FDPFUITableView.FIsScrolling := false;
  FDPFUITableView.FisDragging  := False;
  FDPFUITableView.FStartDelay  := IncSecond( Now, 10 );

  if Assigned( FDPFUITableView.OnScrollEndDragging ) then
    FDPFUITableView.OnScrollEndDragging( FDPFUITableView, FDPFUITableView.CurOffSet, DPFNSPoint( scrollView.contentOffset ) );
  FDPFUITableView.CurOffSet := DPFNSPoint( scrollView.contentOffset );
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.SetHeaderFooterSettings( Item: TDPFTableItemHeaderFooterSetting ): UIView;
var
  H         : Single;
  titleLabel: UILabel;
  Image     : UIImage;
begin
{$IFDEF DEBUG}
  DPFNSLog( '16' );
{$ENDIF}
  result := nil;
  if Item.Kind = kStandard then
    Exit;

  H := Item.Height;
  if H = -1 then
    H := FDPFUITableView.Options.SectionHeaderHeight;

  result     := TUIView.Wrap( TUIView.alloc.initWithFrame( CGRectMake( Item.Margins.Left, Item.Margins.Top, FDPFUITableView.Width - Item.Margins.Right, H - Item.Margins.Bottom ) ) );
  titleLabel := TUILabel.Wrap( TUILabel.Alloc.initWithFrame( CGRectMake( Item.Padding.Left, Item.Padding.Top, FDPFUITableView.Width - Item.Padding.Right, H - Item.Padding.Bottom ) ) );
  titleLabel.setText( StrToNSStr( Item.Text ) );
  titleLabel.setTextAlignment( LongInt( Item.TextAlign ) );
  titleLabel.setFont( Item.Font._UIFont );
  titleLabel.setTextColor( TColorToUIColor( Item.TextColor ) );
  titleLabel.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );

  if Item.BackgroundColor = TAlphaColors.Null then
    result.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    result.setBackgroundColor( TColorToUIColor( Item.BackgroundColor ) );

  if Item.BackgroundImage <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( StrToNSStr( Item.BackgroundImage ) ) );
    result.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image ) ) );
    // Image.release;
    Image := nil;
  end;

  result.setAlpha( Item.Alpha );
  titleLabel.setAlpha( 1 );
  result.addSubview( titleLabel );
  titleLabel.release;
  titleLabel := nil;
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; willDeselectRowAtIndexPath: NSIndexPath13 ): NSIndexPath; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '17' );
{$ENDIF}
  result := willDeselectRowAtIndexPath;
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; willBeginEditingRowAtIndexPath: NSIndexPath12 ); cdecl;
var
  SecNo, RowNo: LongWord;
  Cell        : UITableViewCell;
  // CellAccessory        : TTableViewCellAccessory;
  // CATransform3DIdentity: CATransform3D;
begin
{$IFDEF DEBUG}
  DPFNSLog( '18' );
{$ENDIF}
  exit;
  if ( willBeginEditingRowAtIndexPath.length > 0 ) then
  begin
    getSecRowIndex( willBeginEditingRowAtIndexPath, SecNo, RowNo );
    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;

    Cell := tableView.cellForRowAtIndexPath( willBeginEditingRowAtIndexPath );
    if Assigned( Cell ) then
    begin
      OriginCellFrame := Cell.frame;
      if DeleteButton = nil then
        DeleteButton := TUIButton.Wrap( TUIButton.OCClass.buttonWithType( 0 ) );
      DeleteButton.setTitle( StrToNSStr( 'Delete' ), 0 );
      DeleteButton.setTitleColor( TColorToUIColor( TAlphaColors.White ), 0 );
      DeleteButton.setBackgroundColor( TColorToUIColor( TAlphaColors.Red ) );
      DeleteButton.setFrame( CGRectMake( OriginCellFrame.origin.x + OriginCellFrame.size.width, OriginCellFrame.origin.y, 100, OriginCellFrame.size.height ) );
      FDPFUITableView.FUITableViewController.tableView.addSubview( DeleteButton );

      TUIView.OCClass.beginAnimations( nil, nil );
      TUIView.OCClass.setAnimationDuration( 0.8 );
      Cell.contentView.setAlpha( 0.5 );
      Cell.setFrame( CGRectMake( -100, OriginCellFrame.origin.y, OriginCellFrame.size.width, OriginCellFrame.size.width ) );
      DeleteButton.setFrame( CGRectMake( OriginCellFrame.origin.x + OriginCellFrame.size.width - 100, OriginCellFrame.origin.y, 100, OriginCellFrame.size.height ) );
      TUIView.OCClass.SetAnimationTransition( 100, Cell.contentView, true );
      TUIView.OCClass.commitAnimations;

    end;
  end;

end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; didEndEditingRowAtIndexPath: NSIndexPath06 ); cdecl;
var
  SecNo, RowNo: LongWord;
  Cell        : UITableViewCell;
begin
{$IFDEF DEBUG}
  DPFNSLog( '36' );
{$ENDIF}
  exit;
  if ( didEndEditingRowAtIndexPath.length > 0 ) then
  begin
    getSecRowIndex( didEndEditingRowAtIndexPath, SecNo, RowNo );
    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;

    Cell := tableView.cellForRowAtIndexPath( didEndEditingRowAtIndexPath );
    if Assigned( Cell ) then
    begin
      TUIView.OCClass.beginAnimations( nil, nil );
      TUIView.OCClass.setAnimationDuration( 0.8 );
      Cell.contentView.setAlpha( 1.0 );
      Cell.setFrame( OriginCellFrame );
      DeleteButton.setFrame( CGRectMake( OriginCellFrame.origin.x + OriginCellFrame.size.width, OriginCellFrame.origin.y, 100, OriginCellFrame.size.height ) );
      TUIView.OCClass.commitAnimations;
    end;
  end;

end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; heightForRowAtIndexPath: NSIndexPath02 ): CGFloat; cdecl;
var
  TableItem   : TTableItem;
  RowHeight   : Single;
  SecNo, RowNo: LongWord;
begin
{$IFDEF DEBUG}
  DPFNSLog( '19' );
{$ENDIF}
  // if FDPFUITableView.FVirtualMode then exit;

  result := -1;
  if heightForRowAtIndexPath.length = 0 then
    exit;

  getSecRowIndex( heightForRowAtIndexPath, SecNo, RowNo );
  if FDPFUITableView.FSearchTextExists then
  begin
    SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
    RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
  end;

  RowHeight := FDPFUITableView.Options.RowHeight;
  if Assigned( FDPFUITableView.FOnGetRowHeight ) then
  begin
    FDPFUITableView.FOnGetRowHeight( FDPFUITableView, SecNo, RowNo, RowHeight );
    Result := RowHeight;
  end
  else
  begin
    TableItem := FDPFUITableView.FSections[SecNo].TableItems[RowNo];
    if TableItem.Height > 0 then
      result := TableItem.Height
    else
      result := FDPFUITableView.Options.RowHeight;
  end;
  // ---------------------------------------------------------------

end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; heightForHeaderInSection: NSInteger1 ): CGFloat; cdecl;
var
  H: LongInt;
begin
{$IFDEF DEBUG}
  DPFNSLog( '20' );
{$ENDIF}
  Result := 0;

  if ( heightForHeaderInSection.val < 0 ) or ( FDPFUITableView.Sections[heightForHeaderInSection.val].Header.Text = '' ) then
    exit;

  H := FDPFUITableView.Sections[heightForHeaderInSection.val].Header.Height;
  if H = -1 then
    H    := FDPFUITableView.Options.SectionHeaderHeight;
  result := H;
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; viewForHeaderInSection: NSInteger ): UIView; cdecl;
var
  FTapGestureRecognizer: UITapGestureRecognizer;
begin
{$IFDEF DEBUG}
  DPFNSLog( '21' );
{$ENDIF}
  result := nil;
  if viewForHeaderInSection < 0 then
    exit;

  result := SetHeaderFooterSettings( FDPFUITableView.Sections[viewForHeaderInSection].Header );
  if result <> nil then
  begin
    if FDPFUITableView.FDPFTableSectionGestureRecognizerDelegate = nil then
      FDPFUITableView.FDPFTableSectionGestureRecognizerDelegate := TDPFTableSectionGestureRecognizerDelegate.Create( FDPFUITableView );
    result.setTag( viewForHeaderInSection );
    FTapGestureRecognizer := TUITapGestureRecognizer.Wrap( TUITapGestureRecognizer.Alloc.initWithTarget( FDPFUITableView.FDPFTableSectionGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'handleSingleTap:' ) ) );
    result.addGestureRecognizer( FTapGestureRecognizer );
    FTapGestureRecognizer.release;
  end;
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; heightForFooterInSection: NSInteger3 ): CGFloat; cdecl;
var
  H: LongInt;
begin
{$IFDEF DEBUG}
  DPFNSLog( '22' );
{$ENDIF}
  result := 0;
  if ( heightForFooterInSection.val < 0 ) or ( FDPFUITableView.Sections[heightForFooterInSection.val].Footer.Text = '' ) then
    exit;

  H := FDPFUITableView.Sections[heightForFooterInSection.val].Footer.Height;
  if H = -1 then
    H    := FDPFUITableView.Options.SectionFooterHeight;
  result := H;
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; viewForFooterInSection: NSInteger2 ): UIView; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '23' );
{$ENDIF}
  result := nil;
  if viewForFooterInSection.val < 0 then
    exit;
  result := SetHeaderFooterSettings( FDPFUITableView.Sections[viewForFooterInSection.val].Footer );
end;

// ------------------------------------------------------------------------------
(* Apple Sayed :
  Using legacy cell layout due to delegate implementation of tableView:accessoryTypeForRowWithIndexPath: in <TTableViewDelegate: 0x1d020520>.
  Please remove your implementation of this method and set the cell properties accessoryType and/or editingAccessoryType to move to the new cell layout behavior.
  This method will no longer be called in a future release.
*)
(*
  function TTableViewDelegate.tableView( tableView: UITableView; accessoryTypeForRowWithIndexPath: NSIndexPath05 ): UITableViewCellAccessoryType; cdecl;
  var
  Idxes       : array of NSUInteger;
  SecNo, RowNo: LongInt;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '23-1' );
  {$ENDIF}
  SetLength( Idxes, accessoryTypeForRowWithIndexPath.length );
  try
  // Idxes[0] = Section No
  // Idxes[1] = Row Index No
  accessoryTypeForRowWithIndexPath.getIndexes( Cardinal( @Idxes[0] ) );

  SecNo := Idxes[0];
  RowNo := Idxes[1];
  if FDPFUITableView.FSearchTextExists then
  begin
  SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
  RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
  end;

  result := LongInt( FDPFUITableView.Row[SecNo, RowNo].AccessoryType );
  finally
  SetLength( Idxs, 0 );
  end;
  end;
*)

// ------------------------------------------------------------------------------
(* function TTableViewDelegate.tableView( tableView: UITableView; canPerformAction: SEL; forRowAtIndexPath: NSIndexPath01; withSender: Pointer ): Boolean; cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '25' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
(* procedure TTableViewDelegate.tableView( tableView: UITableView; performAction: SEL; forRowAtIndexPath: NSIndexPath; withSender: Pointer ); cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '26' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; accessoryButtonTappedForRowWithIndexPath: NSIndexPath03 ); cdecl;
var
  SecNo, RowNo: LongWord;
begin
{$IFDEF DEBUG}
  DPFNSLog( '27' );
{$ENDIF}
  if Assigned( FDPFUITableView.FOnAccessoryButtonTapped ) and ( accessoryButtonTappedForRowWithIndexPath.length > 0 ) then
  begin
    getSecRowIndex( accessoryButtonTappedForRowWithIndexPath, SecNo, RowNo );
    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;
    FDPFUITableView.FOnAccessoryButtonTapped( FDPFUITableView, SecNo, RowNo );
  end;

end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; willSelectRowAtIndexPath: NSIndexPath01 ): NSIndexPath; cdecl;
begin
{$IFDEF DEBUG}
  DPFNSLog( '28' );
{$ENDIF}
  result := willSelectRowAtIndexPath;
end;

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; editingStyleForRowAtIndexPath: NSIndexPath07 ): UITableViewCellEditingStyle; cdecl;
var
  SecNo, RowNo: LongWord;
begin
  result := LongInt( tvesNone );
  if editingStyleForRowAtIndexPath.length = 0 then
    exit;

  getSecRowIndex( editingStyleForRowAtIndexPath, SecNo, RowNo );
  if FDPFUITableView.FSearchTextExists then
  begin
    SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
    RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
  end;
  result := LongInt( FDPFUITableView.Row[SecNo, RowNo].EditingStyle );

{$IFDEF DEBUG}
  DPFNSLog( '29 - result : ' + IntToStr( result ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
(* function TTableViewDelegate.tableView( tableView: UITableView; titleForDeleteConfirmationButtonForRowAtIndexPath: NSIndexPath11 ): NSString; cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '30' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
function TTableViewDelegate.tableView( tableView: UITableView; targetIndexPathForMoveFromRowAtIndexPath: NSIndexPath01; toProposedIndexPath: NSIndexPath01 ): NSIndexPath; cdecl;
var
  IdxsFrom, IdxsTo: array of NSUInteger;
  CanMove         : Boolean;
begin
{$IFDEF DEBUG}
  DPFNSLog( '31' );
{$ENDIF}
  CanMove := True;
  if ( targetIndexPathForMoveFromRowAtIndexPath.length = 0 ) or ( toProposedIndexPath.length = 0 ) then
    exit;

  if Assigned( FDPFUITableView.FOnCanMoveFromRowToRow ) then
  begin
    // Idxes[0] = Section No
    // Idxes[1] = Row Index No
    SetLength( IdxsFrom, targetIndexPathForMoveFromRowAtIndexPath.length );
    try
      targetIndexPathForMoveFromRowAtIndexPath.getIndexes( NSUInteger( @IdxsFrom[0] ) );

      SetLength( IdxsTo, toProposedIndexPath.length );
      try
        toProposedIndexPath.getIndexes( NSUInteger( @IdxsTo[0] ) );

        FDPFUITableView.FOnCanMoveFromRowToRow( FDPFUITableView, IdxsFrom[0], IdxsFrom[1], IdxsTo[0], IdxsTo[1], CanMove );

      finally
        SetLength( IdxsFrom, 0 );
      end;
    finally
      SetLength( IdxsTo, 0 );
    end;

    // OnCanMoveFromRowToRow
    result := targetIndexPathForMoveFromRowAtIndexPath;
  end;
  if CanMove then
    result := toProposedIndexPath
  else
    result := targetIndexPathForMoveFromRowAtIndexPath;
end;

// ------------------------------------------------------------------------------
(* function TTableViewDelegate.tableView( tableView: UITableView; shouldShowMenuForRowAtIndexPath: NSIndexPath10 ): Boolean; cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '32' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
(* function TTableViewDelegate.tableView( tableView: UITableView; shouldIndentWhileEditingRowAtIndexPath: NSIndexPath09 ): Boolean; cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '33' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
(* function TTableViewDelegate.tableView( tableView: UITableView; indentationLevelForRowAtIndexPath: NSIndexPath08 ): NSInteger; cdecl;
  begin
  {$IFDEF DEBUG}
  DPFNSLog( '34' );
  {$ENDIF}
  end; *)

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; didSelectRowAtIndexPath: NSIndexPath ); cdecl;
var
  SecNo, RowNo : LongWord;
  Cell         : UITableViewCell;
  CellAccessory: TTableViewCellAccessory;
begin
{$IFDEF DEBUG}
  DPFNSLog( '35' );
{$ENDIF}
  if Assigned( FDPFUITableView.FOnItemSelect ) and ( didSelectRowAtIndexPath.length > 0 ) then
  begin
    getSecRowIndex( didSelectRowAtIndexPath, SecNo, RowNo );
    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;

    Cell := tableView.cellForRowAtIndexPath( didSelectRowAtIndexPath );
    if Assigned( Cell ) then
    begin
      CellAccessory := FDPFUITableView.Row[SecNo, RowNo].AccessoryType;
      FDPFUITableView.FOnItemSelect( Self, SecNo, RowNo, CellAccessory );
      Cell.setAccessoryType( LongInt( CellAccessory ) );
      Cell.setEditingAccessoryType( LongInt( FDPFUITableView.Row[SecNo, RowNo].EditingAccessoryType ) );
      FDPFUITableView.Row[SecNo, RowNo].AccessoryType := CellAccessory;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TTableViewDelegate.tableView( tableView: UITableView; didDeselectRowAtIndexPath: NSIndexPath04 ); cdecl;
var
  SecNo, RowNo : LongWord;
  CellAccessory: TTableViewCellAccessory;
  LCell        : UITableViewCell;
begin
{$IFDEF DEBUG}
  DPFNSLog( '37' );
{$ENDIF}
  if Assigned( FDPFUITableView.FOnItemDeSelect ) and ( didDeselectRowAtIndexPath.length > 0 ) then
  begin
    getSecRowIndex( didDeselectRowAtIndexPath, SecNo, RowNo );
    if FDPFUITableView.FSearchTextExists then
    begin
      SecNo := FDPFUITableView.FSearchMap[RowNo].Section;
      RowNo := FDPFUITableView.FSearchMap[RowNo].Row;
    end;

    CellAccessory := FDPFUITableView.Row[SecNo, RowNo].AccessoryType;
    FDPFUITableView.FOnItemDeSelect( Self, SecNo, RowNo, CellAccessory );
    LCell := tableView.cellForRowAtIndexPath( didDeselectRowAtIndexPath );
    if Assigned( LCell ) then // SZ: have to check for nil: https://sourceforge.net/p/dpfdelphiios/discussion/general/thread/6176e473/?limit=25#e454
      LCell.setAccessoryType( LongInt( CellAccessory ) );
    FDPFUITableView.Row[SecNo, RowNo].AccessoryType := CellAccessory;
  end;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFTableViewOptions }
constructor TDPFTableViewOptions.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
{$IFDEF IOS}
  FDPFUITableView := ADPFUITableView;
{$ENDIF}
  FRefreshControl  := TDPFTableRefreshControl.Create( ADPFUITableView );
  FScrolling       := TDPFTableViewScrolling.Create;
  FEdition         := TDPFTableViewEditing.Create( FDPFUITableView );
  FSearchBar       := TDPFTableViewSearchBar.Create;
  FBackgroundColor := TAlphaColors.White;
  FSeparatorColor  := TAlphaColors.Black;
  FSelectedColor   := TAlphaColors.Null;

  FIndicatorStyle := tviDefault;
  FSeparatorStyle := tssSingleLine;
  FSelectionStyle := tcsBlue;

  FAllowsMultipleSelection      := False;
  FAllowsSelectionDuringEditing := False;
  FAllowsSelection              := True;
  FAllowsSelectionDuringEditing := False;
  FViewStyle                    := tvsPlain;

  // FHeaderTitle := '';
  FRowHeight := 44;

  FSectionFooterHeight := 30;
  FSectionHeaderHeight := 30;

  FPaging := False;

  FTextFont          := TDPFFont.Create;
  FTextFont.FontName := TDPFIOSFontList.ios_Helvetica_Bold;
  FTextFont.FontSize := 18;

  FDescriptionFont          := TDPFFont.Create;
  FDescriptionFont.FontName := TDPFIOSFontList.ios_Helvetica;
  FDescriptionFont.FontSize := 14;

{$IFDEF IOS}
  selectedBackgroundView := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFTableViewOptions.Destroy;
begin
  DisposeOfAndNil(FScrolling);
  DisposeOfAndNil(FEdition);
  DisposeOfAndNil(FSearchBar);
  DisposeOfAndNil(FRefreshControl);

  DisposeOfAndNil(FTextFont);
  DisposeOfAndNil(FDescriptionFont);

{$IFDEF IOS}
  if Assigned( selectedBackgroundView ) then
    selectedBackgroundView.release;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;

{$IFDEF IOS}
  if FDPFUITableView.FUITableViewController <> nil then
  begin
    if FBackgroundColor = TAlphaColors.Null then
    begin
      FDPFUITableView.FUITableViewController.tableView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
      FDPFUITableView.FUITableViewController.tableView.setBackgroundView( nil );
      FDPFUITableView.FUITableViewController.tableView.setOpaque( false );
    end
    else
      FDPFUITableView.FUITableViewController.tableView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );
  end;
{$ELSE}
  if Assigned( FDPFUITableView ) then
    FDPFUITableView.Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Img: UIImageView;
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if FDPFUITableView.FUITableViewController <> nil then
  begin
    if BackgroundImage <> '' then
    begin
      Img := TUIImageView.Wrap( TUIImageView.alloc.initWithImage( TUIImage.Wrap( TUIImage.OCClass.ImageNamed( StrToNSStr( BackgroundImage ) ) ) ) );
      FDPFUITableView.FUITableViewController.tableView.setBackgroundView( Img );
      Img.release;
      Img := nil;
    end
    else
      FDPFUITableView.FUITableViewController.tableView.setBackgroundView( nil );
  end;
{$ENDIF IOS}
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetDescriptionFont( const Value: TDPFFont );
begin
  FDescriptionFont.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetTextFont( const Value: TDPFFont );
begin
  FTextFont.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetEdition( const Value: TDPFTableViewEditing );
begin
  FEdition.Assign( Value );
end;

// ------------------------------------------------------------------------------
(* procedure TDPFTableViewOptions.SetHeaderTitle( const Value: string );
  begin
  FHeaderTitle := Value;
  end; *)

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetRefreshControl( const Value: TDPFTableRefreshControl );
begin
  FRefreshControl.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetRowHeight( const Value: CGFloat );
begin
  FRowHeight := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetScrolling( const Value: TDPFTableViewScrolling );
begin
  FScrolling.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetSearchBar( const Value: TDPFTableViewSearchBar );
begin
  FSearchBar.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetSelectedColor( const Value: TAlphaColor );
begin
{$IFDEF IOS}
  if Assigned( FDPFUITableView.FUITableViewController ) then
  begin
    if not Assigned( selectedBackgroundView ) then
      selectedBackgroundView := TUIView.Wrap( TUIView.Alloc.initWithFrame( CGRectMake( 0, 0, 600, 600 ) ) );
    selectedBackgroundView.setBackgroundColor( TColorToUIColor( value ) );
    FDPFUITableView.FUITableViewController.tableView.reloadRowsAtIndexPaths( FDPFUITableView.FUITableViewController.tableView.indexPathsForVisibleRows, UITableViewRowAnimationNone );
  end;
{$ENDIF}
  FSelectedColor := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewOptions.SetSeparatorColor( const Value: TAlphaColor );
begin
  FSeparatorColor := Value;
{$IFDEF IOS}
  if FDPFUITableView.FUITableViewController <> nil then
  begin
    if FSeparatorColor = TAlphaColors.Null then
      FDPFUITableView.FUITableViewController.tableView.setSeparatorColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FDPFUITableView.FUITableViewController.tableView.setSeparatorColor( TColorToUIColor( FSeparatorColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFTableViewEditing }
constructor TDPFTableViewEditing.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited create;
  FDPFUITableView := ADPFUITableView;
  FCellEdit       := false;
  FEditAll        := False;
  FMoving         := false;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableViewEditing.Destroy;
begin
  FDPFUITableView := nil;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewEditing.SetEditAll( const Value: Boolean );
begin
  FEditAll := Value;
{$IFDEF IOS}
  if FDPFUITableView.FUITableViewController <> nil then
  begin
    FDPFUITableView.FUITableViewController.tableView.setEditing( value, true );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFTableViewScrolling }
constructor TDPFTableViewScrolling.Create;
begin
  inherited create;
  FEnabled                        := True;
  FDirectionalLockEnabled         := False;
  FBounces                        := True;
  FAlwaysBounceVertical           := False;
  FAlwaysBounceHorizontal         := False;
  FShowsHorizontalScrollIndicator := True;
  FShowsVerticalScrollIndicator   := True;
  FScrollsToTop                   := True;
  FHorizontal                     := false;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableViewScrolling.Destroy;
begin

  inherited;
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFTableViewTimerDelegate }

constructor TDPFTableViewTimerDelegate.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := ADPFUITableView;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewTimerDelegate.ondidTimer( timer: NSTimer );
var
  scrollPoint    : CGPoint;
  NSI            : NSIndexPath;
  cnt            : Longword;
  SECount, ICount: Longword;
  // TC             : UITableViewCell;
  cellRect    : CGRect;
  SecNo, RowNo: LongWord;
begin
  if not assigned( FDPFUITableView ) or not FDPFUITableView.FAutoScroll or FDPFUITableView.FisDragging or ( FDPFUITableView.Sections.Count = 0 ) or ( FDPFUITableView.FSearchTextExists ) then
    exit;

  if Now < FDPFUITableView.FStartDelay then
    exit;

  cnt := FDPFUITableView.FUITableViewController.tableView.indexPathsForVisibleRows.count;
  if cnt > 0 then
  begin
    // Idxes[0] = Section No
    // Idxes[1] = Row Index No
    if ( FDPFUITableView.FAutoScrollDir = asdDown ) then
      NSI := TNSIndexPath.Wrap( FDPFUITableView.FUITableViewController.tableView.indexPathsForVisibleRows.objectAtIndex( cnt - 1 ) )
    else
      NSI := TNSIndexPath.Wrap( FDPFUITableView.FUITableViewController.tableView.indexPathsForVisibleRows.objectAtIndex( 0 ) );

    getSecRowIndex( NSI, SecNo, RowNo );

    SECount  := FDPFUITableView.Sections.Count;
    ICount   := FDPFUITableView.Sections[SECount - 1].TableItems.Count;
    cellRect := FDPFUITableView.FUITableViewController.tableView.rectForRowAtIndexPath( NSI );

    if CGRectContainsRect( CGRectOffset( FDPFUITableView.FUITableViewController.tableView.frame, FDPFUITableView.FUITableViewController.tableView.contentOffset.x, FDPFUITableView.FUITableViewController.tableView.contentOffset.y ), cellRect ) > 0 then
    begin
      if not FDPFUITableView.FAutoScrollLoop and ( ( ( SecNo = SECount - 1 ) and ( RowNo = ICount - 1 ) ) or ( ( SecNo = 0 ) and ( RowNo = 0 ) ) ) then
        exit;

      if ( FDPFUITableView.FAutoScrollDir = asdDown ) and ( SecNo = SECount - 1 ) and ( RowNo = ICount - 1 ) then
      begin
        FDPFUITableView.FAutoScrollDir := asdUp
      end
      else if ( FDPFUITableView.FAutoScrollDir = asdUp ) and ( SecNo = 0 ) and ( RowNo = 0 ) then
      begin
        FDPFUITableView.FAutoScrollDir := asdDown
      end
    end;

  end;

  scrollPoint := FDPFUITableView.FUITableViewController.tableView.contentOffset;
  if FDPFUITableView.FAutoScrollDir = asdUp then
    scrollPoint.y := scrollPoint.y - 1
  else
    scrollPoint.y := scrollPoint.y + 1;

  FDPFUITableView.FUITableViewController.tableView.setContentOffset( scrollPoint, false );
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFTableViewSearchBar }

constructor TDPFTableViewSearchBar.Create;
begin
  inherited;
  FSearchInItemText := true;
  FSearchInItemDesc := true;
  FSearchKind       := TDPFTableViewSearchKind.skStartsWith;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableViewSearchBar.Destroy;
begin

  inherited;
end;

{$IFDEF IOS6}

// ------------------------------------------------------------------------------
{ TDPFRefreshControlDelegate }
constructor TDPFRefreshControlDelegate.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := ADPFUITableView;
end;

// ------------------------------------------------------------------------------
function TDPFRefreshControlDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFRefreshControlDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFRefreshControlDelegate.OnRefreshNeeded;
begin
  if Assigned( FDPFUITableView.OnRefreshNeeded ) then
    FDPFUITableView.OnRefreshNeeded( FDPFUITableView );
end;

// ------------------------------------------------------------------------------
{ TDPFTableSectionGestureRecognizerDelegate }
constructor TDPFTableSectionGestureRecognizerDelegate.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := ADPFUITableView;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableSectionGestureRecognizerDelegate.handleSingleTap( gr: UITapGestureRecognizer );
begin
  if Assigned( FDPFUITableView.FOnSectionClick ) then
    FDPFUITableView.FOnSectionClick( FDPFUITableView, gr.view.tag );
end;

// ------------------------------------------------------------------------------
{ TDPFTableCellGestureRecognizerDelegate }
constructor TDPFTableCellGestureRecognizerDelegate.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := ADPFUITableView;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableCellGestureRecognizerDelegate.handleSingleTap( gr: UITapGestureRecognizer );
var
  p          : CGPoint;
  TapPosition: TTapPosition;
  indexPath  : NSIndexPath;
  cancel     : Boolean;
begin
  cancel := false;
  if Assigned( FDPFUITableView.FOnCellClick ) then
  begin
    p                     := gr.locationInView( FDPFUITableView.FUITableViewController.tableView );
    TapPosition.AbsoluteX := p.x;
    TapPosition.AbsoluteY := p.y;
    indexPath             := FDPFUITableView.FUITableViewController.tableView.indexPathForRowAtPoint( p );
    p                     := gr.locationInView( gr.view );
    TapPosition.CellX     := p.x;
    TapPosition.CellY     := p.y;
    TapPosition.ScreenX   := FDPFUITableView.FScreenRect.size.width;
    TapPosition.ScreenY   := FDPFUITableView.FScreenRect.size.height;
    FDPFUITableView.FOnCellClick( FDPFUITableView, indexPath.section, indexPath.row, TapPosition, TDPFGestureRecognizerState( gr.state ), cancel );
  end;
  gr.setCancelsTouchesInView( cancel );
end;

// ------------------------------------------------------------------------------
{ TDPFTableCellLongPressGestureRecognizerDelegate }
constructor TDPFTableCellLongPressGestureRecognizerDelegate.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView := ADPFUITableView;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableCellLongPressGestureRecognizerDelegate.handleLongPress( lpgr: UILongPressGestureRecognizer );
var
  p          : CGPoint;
  TapPosition: TTapPosition;
  indexPath  : NSIndexPath;
  cancel     : Boolean;
begin
  cancel := false;
  if Assigned( FDPFUITableView.FOnCellLongClick ) then
  begin
    p                     := lpgr.locationInView( FDPFUITableView.FUITableViewController.tableView );
    TapPosition.AbsoluteX := p.x;
    TapPosition.AbsoluteY := p.y;
    indexPath             := FDPFUITableView.FUITableViewController.tableView.indexPathForRowAtPoint( p );
    p                     := lpgr.locationInView( lpgr.view );
    TapPosition.CellX     := p.x;
    TapPosition.CellY     := p.y;
    TapPosition.ScreenX   := FDPFUITableView.FScreenRect.size.width;
    TapPosition.ScreenY   := FDPFUITableView.FScreenRect.size.height;
    FDPFUITableView.FOnCellLongClick( FDPFUITableView, indexPath.section, indexPath.row, TapPosition, TDPFGestureRecognizerState( lpgr.state ), cancel );
  end;
  lpgr.setCancelsTouchesInView( cancel );
end;

// ------------------------------------------------------------------------------
constructor TDPFTableViewSwipGestureRecognizerDelegate.Create( ATableView: IOSapi.Uikit.UITableView );
begin
  inherited Create;
  FTableView := ATableView;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableViewSwipGestureRecognizerDelegate.swipeGestureRecognizer( gestureRecognizer: UISwipeGestureRecognizer ); cdecl;
var
  location : CGPoint;
  indexPath: iOSapi.Foundation.NSIndexPath;
  cell     : UITableViewCell;
begin

  // Get location of the swipe
  location := gestureRecognizer.locationInView( FTableView );

  // Get the corresponding index path within the table view
  indexPath := FTableView.indexPathForRowAtPoint( location );

  // Check if index path is valid
  if assigned( indexPath ) then
  begin
    // Get the cell out of the table view
    cell := FTableView.cellForRowAtIndexPath( indexPath );
    // DPFNSLog( 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' );
    // Update the cell or model
    cell.setAccessoryType( UITableViewCellAccessoryCheckmark );
  end;
end;

// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFTableRefreshControl }
constructor TDPFTableRefreshControl.Create( ADPFUITableView: TDPFUITableView );
begin
  inherited Create;
  FDPFUITableView  := ADPFUITableView;
  FVisible         := false;
  FColor           := TAlphaColors.Tomato;
  FBackgroundColor := TAlphaColors.Null;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableRefreshControl.Destroy;
begin
  FDPFUITableView := nil;
  inherited;
end;

procedure TDPFTableRefreshControl.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS6}
  if Assigned( FDPFUITableView.FDPFRefreshControl ) then
  begin
    if value = TAlphaColors.Null then
      FDPFUITableView.FDPFRefreshControl.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FDPFUITableView.FDPFRefreshControl.setBackgroundColor( TColorToUIColor( Value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTableRefreshControl.SetColor( const Value: TAlphaColor );
begin
  FColor := Value;
{$IFDEF IOS6}
  if Assigned( FDPFUITableView.FDPFRefreshControl ) then
    FDPFUITableView.FDPFRefreshControl.setTintColor( TColorToUIColor( Value ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTableRefreshControl.SetText( const Value: string );
{$IFDEF IOS6}
var
  NA: NSAttributedString;
{$ENDIF}
begin
  FText := Value;
{$IFDEF IOS6}
  if Assigned( FDPFUITableView.FDPFRefreshControl ) then
  begin
    NA := TNSAttributedString.Wrap( TNSAttributedString.Alloc.initWithString( StrToNSStr( Value ) ) );
    FDPFUITableView.FDPFRefreshControl.setAttributedTitle( NA );
    NA.release;
  end;
{$ENDIF}
end;

initialization

// ------------------------------------------------------------------------------
end.
