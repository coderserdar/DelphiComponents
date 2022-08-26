// ------------------------------------------------------------------------------
// DPF.iOS.UICollectionView Component
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
unit DPF.iOS.UICollectionView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.MaskUtils,
  System.Generics.Collections,

{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  iOSapi.QuartzCore,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ELSE}
  DPF.iOS.DesignTime,
{$ENDIF}
  DPF.iOS.BaseControl,
  DPF.iOS.UITextField,
  DPF.iOS.UIPickerView,
  DPF.iOS.UIScrollView,
  DPF.iOS.Classes,
  DPF.iOS.Dispatch,

  FMX.Graphics,
  FMX.Types,
  DPF.iOS.UIFont,
  System.TypInfo;

const
  TAG_BASE        = 10000;
  TAG_CELL_REUSED = 999999;

type
  TDPFUICollectionView = class;

  // ----------------------------------------------------------------------------
  UICollectionViewScrollPosition  = ( UICollectionViewScrollPositionNone = 0, UICollectionViewScrollPositionTop = 1, UICollectionViewScrollPositionCenteredVertically = 2, UICollectionViewScrollPositionBottom = 3, UICollectionViewScrollPositionLeft = 4, UICollectionViewScrollPositionCenteredHorizontally = 5, UICollectionViewScrollPositionRight = 6 );
  UICollectionViewScrollDirection = ( UICollectionViewScrollDirectionVertical, UICollectionViewScrollDirectionHorizontal );

  // ----------------------------------------------------------------------------
  TCollectionViewItem = record
    SectionNo: NativeInt;
    ItemNo: NativeInt;
  end;

  TCollectionViewItems = array of TCollectionViewItem;

{$IFDEF IOS}
  UICollectionView = interface;

  // ----------------------------------------------------------------------------
  NSIndexPath1 = interface( NSIndexPath )
  end;

  NSIndexPath2 = interface( NSIndexPath )
  end;

  NSIndexPath3 = interface( NSIndexPath )
  end;

  NSIndexPath4 = interface( NSIndexPath )
  end;

  NSIndexPath5 = interface( NSIndexPath )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewLayout
  // ----------------------------------------------------------------------------
  UICollectionViewLayoutClass = interface( NSObjectClass )
    ['{96E4E713-239B-47CA-9206-5057A18CEB39}']

  end;

  UICollectionViewLayout = interface( NSObject )
    ['{CCB8FDE6-68D1-49E6-A51A-F0321E8A1D34}']

  end;

  TUICollectionViewLayout = class( TOCGenericImport<UICollectionViewLayoutClass, UICollectionViewLayout> )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewFlowLayout
  // ----------------------------------------------------------------------------
  UICollectionViewFlowLayoutClass = interface( UICollectionViewLayoutClass )
    ['{C17DA9DF-02D7-4DFA-AD9E-42E9D31632C1}']

  end;

  UICollectionViewFlowLayout = interface( UICollectionViewLayout )
    ['{04248BE7-81B0-44DD-96F8-E7C6B2EDAD77}']

    function scrollDirection: UICollectionViewScrollDirection; cdecl;
    procedure setScrollDirection( scrollDirection: UICollectionViewScrollDirection ); cdecl;
  end;

  TUICollectionViewFlowLayout = class( TOCGenericImport<UICollectionViewFlowLayoutClass, UICollectionViewFlowLayout> )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewLayoutAttributes
  // ----------------------------------------------------------------------------
  UICollectionViewLayoutAttributesClass = interface( NSObjectClass )
    ['{95768C05-CD4D-43FA-AEE8-7F65BFB8E20C}']

  end;

  UICollectionViewLayoutAttributes = interface( NSObject )
    ['{33267989-426B-4937-A188-FB7EF9FB38D8}']

  end;

  TUICollectionViewLayoutAttributes = class( TOCGenericImport<UICollectionViewLayoutAttributesClass, UICollectionViewLayoutAttributes> )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewCell
  // ----------------------------------------------------------------------------
  UICollectionReusableViewClass = interface( UIViewClass )
    ['{64245C4E-5F82-4EBA-83BE-4C5AB9A45E30}']

  end;

  UICollectionReusableView = interface( UIView )
    ['{91BD307E-6F62-42CF-8293-9A23D8AC2A6A}']

    function reuseIdentifier: NSString; cdecl;
    procedure prepareForReuse; cdecl;
    procedure applyLayoutAttributes( layoutAttributes: UICollectionViewLayoutAttributes ); cdecl;
    procedure willTransitionFromLayout( oldLayout: UICollectionViewLayout; toLayout: UICollectionViewLayout ); cdecl;
  end;

  TUICollectionReusableView = class( TOCGenericImport<UICollectionReusableViewClass, UICollectionReusableView> )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewCell
  // ----------------------------------------------------------------------------
  UICollectionViewCellClass = interface( UICollectionReusableViewClass )
    ['{6A75CCDB-A041-4C2E-87B6-5C202C7DC2E3}']
  end;

  UICollectionViewCell = interface( UICollectionReusableView )
    ['{B02447C6-C8C2-4E8D-9104-FE54852FEE4C}']

    function contentView: UIView; cdecl;
    function backgroundView: UIView; cdecl;
    function selectedBackgroundView: UIView; cdecl;
    function isSelected: Boolean; cdecl;
    procedure setSelected( selected: Boolean ); cdecl;
    function isHighlighted: Boolean; cdecl;
  end;

  TUICollectionViewCell = class( TOCGenericImport<UICollectionViewCellClass, UICollectionViewCell> )
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewDataSource
  // ----------------------------------------------------------------------------
  UICollectionViewDataSource = interface( IObjectiveC )
    ['{318A604A-ED39-4EEC-B2C9-36CAC8A8A2E7}']

    // Getting Item and Section Metrics
    function collectionView( collectionView: UICollectionView; numberOfItemsInSection: NSInteger ): NSInteger; overload; cdecl;
    function numberOfSectionsInCollectionView( collectionView: UICollectionView ): NSInteger; overload; cdecl;

    // Getting Views for Items
    function collectionView( collectionView: UICollectionView; cellForItemAtIndexPath: NSIndexPath ): UICollectionViewCell; overload; cdecl;
    // function collectionView( collectionView: UICollectionView; viewForSupplementaryElementOfKind: NSString; atIndexPath: NSIndexPath ): UICollectionReusableView; overload; cdecl;
  end;

  TUICollectionViewDataSource = class( TOCLocal, UICollectionViewDataSource )
  private
    FDPFUICollectionView: TDPFUICollectionView;
  public
    constructor Create( ADPFUICollectionView: TDPFUICollectionView );

    // Getting Item and Section Metrics
    function collectionView( collectionView: UICollectionView; numberOfItemsInSection: NSInteger ): NSInteger; overload; cdecl;
    function numberOfSectionsInCollectionView( collectionView: UICollectionView ): NSInteger; overload; cdecl;

    // Getting Views for Items
    function collectionView( collectionView: UICollectionView; cellForItemAtIndexPath: NSIndexPath ): UICollectionViewCell; overload; cdecl;
    // function collectionView( collectionView: UICollectionView; viewForSupplementaryElementOfKind: NSString; atIndexPath: NSIndexPath ): UICollectionReusableView; overload; cdecl;
  end;

  // ----------------------------------------------------------------------------
  // UICollectionViewDelegate
  // ----------------------------------------------------------------------------
  UICollectionViewDelegate = interface( IObjectiveC { UIScrollViewDelegate } )
    ['{CE586BB8-530B-44C0-9AE0-68CC06078139}']

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;

    procedure collectionView( collectionView: UICollectionView; didSelectItemAtIndexPath: NSIndexPath ); overload; cdecl;
    procedure collectionView( collectionView: UICollectionView; didDeselectItemAtIndexPath: NSIndexPath1 ); overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldSelectItemAtIndexPath: NSIndexPath2 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldDeselectItemAtIndexPath: NSIndexPath3 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldHighlightItemAtIndexPath: NSIndexPath4 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldShowMenuForItemAtIndexPath: NSIndexPath5 ): Boolean; overload; cdecl;
  end;

  (*
    TUICollectionViewDelegate = class( TOCLocal, UICollectionViewDelegate )
    private
    FDPFUICollectionView: TDPFUICollectionView;
    public
    constructor Create( ADPFUICollectionView: TDPFUICollectionView );

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;

    procedure collectionView( collectionView: UICollectionView; didSelectItemAtIndexPath: NSIndexPath ); overload; cdecl;
    procedure collectionView( collectionView: UICollectionView; didDeselectItemAtIndexPath: NSIndexPath1 ); overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldSelectItemAtIndexPath: NSIndexPath2 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldDeselectItemAtIndexPath: NSIndexPath3 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldHighlightItemAtIndexPath: NSIndexPath4 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldShowMenuForItemAtIndexPath: NSIndexPath5 ): Boolean; overload; cdecl;
    end;
  *)

  // ----------------------------------------------------------------------------
  // UICollectionViewDelegateFlowLayout
  // ----------------------------------------------------------------------------
  UICollectionViewDelegateFlowLayout = interface( UICollectionViewDelegate )
    ['{E395C26B-1863-4BA6-A83D-E48DB7171B90}']

    function collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; sizeForItemAtIndexPath: NSIndexPath ): CGSize; overload; cdecl;

  end;

  TUICollectionViewDelegateFlowLayout = class( TOCLocal, UICollectionViewDelegateFlowLayout )
  private
    FDPFUICollectionView: TDPFUICollectionView;
  public
    constructor Create( ADPFUICollectionView: TDPFUICollectionView );

    function collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; sizeForItemAtIndexPath: NSIndexPath ): CGSize; overload; cdecl;

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;

    procedure collectionView( collectionView: UICollectionView; didSelectItemAtIndexPath: NSIndexPath ); overload; cdecl;
    procedure collectionView( collectionView: UICollectionView; didDeselectItemAtIndexPath: NSIndexPath1 ); overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldSelectItemAtIndexPath: NSIndexPath2 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldDeselectItemAtIndexPath: NSIndexPath3 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldHighlightItemAtIndexPath: NSIndexPath4 ): Boolean; overload; cdecl;
    function collectionView( collectionView: UICollectionView; shouldShowMenuForItemAtIndexPath: NSIndexPath5 ): Boolean; overload; cdecl;
  end;

  // ----------------------------------------------------------------------------
  TCollectionViewLayoutCompletion = procedure of object;

  // ----------------------------------------------------------------------------
  // UICollectionView
  // ----------------------------------------------------------------------------
  UICollectionViewClass = interface( UIScrollViewClass )
    ['{FAC1D908-B71F-4B3F-8141-04EA202AE4F5}']
  end;

  UICollectionView = interface( UIScrollView )
    ['{C7FFB5D8-B0E7-4199-A91E-1343CC79855F}']

    // Initializing a Collection View
    function initWithFrame( frame: CGRect; collectionViewLayout: UICollectionViewLayout ): Pointer; cdecl;

    // Creating Collection View Cells
    procedure registerClass( cellClass: Pointer; forCellWithReuseIdentifier: NSString ); overload; cdecl;
    procedure registerClass( viewClass: Pointer; forSupplementaryViewOfKind: NSString; withReuseIdentifier: NSString )overload; cdecl;
    procedure registerNib( nib: UINib; forCellWithReuseIdentifier: NSString ); cdecl;

    // Configuring the Collection View
    function delegate: Pointer; cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;

    function dataSource: Pointer; cdecl;
    procedure setDataSource( dataSource: Pointer ); cdecl;

    function backgroundView: UIView; cdecl;
    procedure setNackgroundView( backgroundView: UIView ); cdecl;

    function allowsMultipleSelection: Boolean cdecl;
    procedure setAllowsMultipleSelection( allowsMultipleSelection: Boolean )cdecl;
    function allowsSelection: Boolean cdecl;
    procedure setAllowsSelection( allowsSelection: Boolean )cdecl;

    procedure finishInteractiveTransition cdecl;
    procedure cancelInteractiveTransition cdecl;
    procedure insertItemsAtIndexPaths( indexPaths: NSArray )cdecl;
    procedure deleteItemsAtIndexPaths( indexPaths: NSArray )cdecl;
    procedure moveItemAtIndexPath( indexPath: NSIndexPath; toIndexPath: NSIndexPath )cdecl;
    procedure moveSection( section: NSInteger; toSection: NSInteger )cdecl;
    procedure deleteSections( sections: NSIndexSet )cdecl;
    function indexPathsForSelectedItems: NSArray cdecl;
    function indexPathsForVisibleItems: NSArray cdecl;
    procedure selectItemAtIndexPath( indexPath: NSIndexPath; animated: Boolean; scrollPosition: Integer )cdecl;
    procedure deselectItemAtIndexPath( indexPath: NSIndexPath; animated: Boolean )cdecl;
    function layoutAttributesForItemAtIndexPath( indexPath: NSIndexPath ): UICollectionViewLayoutAttributes cdecl;
    function layoutAttributesForSupplementaryElementOfKind( kind: NSString; atIndexPath: NSIndexPath ): UICollectionViewLayoutAttributes cdecl;

    // Creating Collection View Cells
    function dequeueReusableCellWithReuseIdentifier( identifier: NSString; forIndexPath: NSIndexPath ): Pointer; cdecl;
    function dequeueReusableSupplementaryViewOfKind( elementKind: NSString; withReuseIdentifier: NSString; forIndexPath: NSIndexPath ): Pointer; cdecl;

    // Reloading Content
    procedure reloadData; cdecl;
    procedure reloadItemsAtIndexPaths( indexPaths: NSArray ); cdecl;
    procedure reloadSections( sections: NSIndexSet ); cdecl;

    // Getting the State of the Collection View
    function numberOfSections: NSInteger; cdecl;
    function numberOfItemsInSection: NSInteger; cdecl;
    function visibleCells: NSArray; cdecl;

    // Scrolling an Item Into View
    procedure scrollToItemAtIndexPath( indexPath: NSIndexPath; atScrollPosition: UICollectionViewScrollPosition; animated: Boolean ); cdecl;

    procedure setCollectionViewLayout( layout: UICollectionViewLayout; animated: Boolean )cdecl;
    // procedure setCollectionViewLayout(layout: UICollectionViewLayout;animated: Boolean;completion: TCollectionViewLayoutCompletion) cdecl;

    function cellForItemAtIndexPath( indexPath: NSIndexPath ): UICollectionViewCell; cdecl;

  end;

  ILocalUICollectionView = interface( UICollectionView )
    ['{D688C4B2-9407-4258-B72B-9581452A34A4}']
  end;

  TILocalUICollectionView = class( TOCLocal )
  private
  protected
    FDPFCollectionView: TDPFUICollectionView;
  public
    constructor Create( ADPFUICollectionView: TDPFUICollectionView; ACollectionViewLayout: UICollectionViewLayout );
    function GetObjectiveCClass: PTypeInfo; override;
  end;

  // TUICollectionView = class( TOCGenericImport<UICollectionViewClass, UICollectionView> )end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  TDPFUICollectionViewOnDrawCell                 = procedure( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; FrameSize: DPFNSize; isSelected: Boolean; var Objects: TArray<TDPFiOSBaseControl>; var Handled: Boolean ) of object;
  TDPFUICollectionViewOnSelectCell               = procedure( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<TDPFiOSBaseControl> ) of object;
  TDPFUICollectionViewOnDeSelectCell             = procedure( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<TDPFiOSBaseControl> ) of object;
  TDPFUICollectionViewOnGetNumberOfSections      = procedure( Sender: TObject; var NumberOfSections: Integer ) of object;
  TDPFUICollectionViewOnGetNumberOfItemInSection = procedure( Sender: TObject; SectionNo: NativeInt; var numberOfRowsInSection: Integer ) of object;
  TDPFUICollectionViewOnItemSize                 = procedure( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Width: Single; var Height: Single ) of object;

  // ----------------------------------------------------------------------------
  // Class TDPFUICollectionView
  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUICollectionView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    MainQueue: dispatch_queue_t;

    // selectedItemIndexPath: NSIndexPath;

    FLocalUICollectionView: TILocalUICollectionView;
    FDPFCollectionView    : UICollectionView;

    FDPFUICollectionViewFlowLayout: UICollectionViewFlowLayout;
    FUICollectionViewDataSource   : TUICollectionViewDataSource;
    // FUICollectionViewDelegate          : TUICollectionViewDelegate;
    FUICollectionViewDelegateFlowLayout: TUICollectionViewDelegateFlowLayout;
{$ENDIF}
    FOnDrawCell                : TDPFUICollectionViewOnDrawCell;
    FOnGetNumberOfItemInSection: TDPFUICollectionViewOnGetNumberOfItemInSection;
    FOnGetNumberOfSections     : TDPFUICollectionViewOnGetNumberOfSections;
    FOnItemSize                : TDPFUICollectionViewOnItemSize;
    FAllowsSelection           : Boolean;
    FAllowsMultipleSelection   : Boolean;
    FOnSelectCell              : TDPFUICollectionViewOnSelectCell;
    FOnDeSelectCell            : TDPFUICollectionViewOnDeSelectCell;
    FScrollDirection           : UICollectionViewScrollDirection;
    FBackgroundColor           : TAlphaColor;

    procedure ClearBuffer;
    procedure SetAllowsSelection( const Value: Boolean );
    procedure SetAllowsMultipleSelection( const Value: Boolean );
    procedure SetScrollDirection( const Value: UICollectionViewScrollDirection );
    procedure SetBackgroundColor( const Value: TAlphaColor );
  protected
    CellDicCustomViews: TDictionary<Pointer, TArray<TDPFiOSBaseControl>>;

    FCollectionViewSelectedItems: TCollectionViewItems;
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
    procedure SelectItem( SecNo, ItemNo: NativeInt; Animated: Boolean; scrollPosition: Integer );
    function GetRowCustomViews( const SectionNo: Longword; const ItemNo: NativeInt ): TArray<TDPFiOSBaseControl>; overload;
    function GetRowCustomViews( IndexPath: NSIndexPath ): TArray<TDPFiOSBaseControl>; overload;
    function isSelected( const SectionNo: Longword; const ItemNo: NativeInt ): Boolean;
    procedure ReloadData;
    procedure ReloadRow( const Section, Row: Integer );
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property AllowsSelection        : Boolean read FAllowsSelection write SetAllowsSelection default true;
    property AllowsMultipleSelection: Boolean read FAllowsMultipleSelection write SetAllowsMultipleSelection default false;
    property ScrollDirection        : UICollectionViewScrollDirection read FScrollDirection write SetScrollDirection default UICollectionViewScrollDirection.UICollectionViewScrollDirectionVertical;
    property BackgroundColor        : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;

    property OnDrawCell                : TDPFUICollectionViewOnDrawCell read FOnDrawCell write FOnDrawCell;
    property OnSelectCell              : TDPFUICollectionViewOnSelectCell read FOnSelectCell write FOnSelectCell;
    property OnDeSelectCell            : TDPFUICollectionViewOnDeSelectCell read FOnDeSelectCell write FOnDeSelectCell;
    property OnGetNumberOfSections     : TDPFUICollectionViewOnGetNumberOfSections read FOnGetNumberOfSections write FOnGetNumberOfSections;
    property OnGetNumberOfItemInSection: TDPFUICollectionViewOnGetNumberOfItemInSection read FOnGetNumberOfItemInSection write FOnGetNumberOfItemInSection;
    property OnItemSize                : TDPFUICollectionViewOnItemSize read FOnItemSize write FOnItemSize;

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

procedure DeleteX( var X: TCollectionViewItems; const SectionNo: NativeInt; const ItemNo: NativeInt );
var
  Index, I: NativeInt;
begin
  index := -1;

  for I := 0 to high( X ) do
    if ( X[I].SectionNo = SectionNo ) and ( X[I].ItemNo = ItemNo ) then
    begin
      index := I;
      break;
    end;

  if index > high( X ) then
    Exit;
  if index < low( X ) then
    Exit;
  if index = high( X ) then
  begin
    SetLength( X, Length( X ) - 1 );
    Exit;
  end;
  Finalize( X[index] );
  System.Move( X[index + 1], X[index], ( Length( X ) - index - 1 ) * SizeOf( string ) + 1 );
  SetLength( X, Length( X ) - 1 );

end;

// ------------------------------------------------------------------------------
procedure getSecRowIndex( NSI: NSIndexPath; var SecNo: NativeInt; var ItemNo: NativeInt );
var
  Idxes: array of NSUInteger;
begin
  SetLength( Idxes, NSI.length );
  try
    NSI.getIndexes( NativeInt( @Idxes[0] ) );
    SecNo  := Idxes[0];
    ItemNo := Idxes[1];
  finally
    SetLength( Idxes, 0 );
    Idxes := nil;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFUICollectionView }
// ------------------------------------------------------------------------------
constructor TDPFUICollectionView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption           := 'CollectionView';
  FAllowsSelection         := true;
  FAllowsMultipleSelection := false;
  FScrollDirection         := UICollectionViewScrollDirection.UICollectionViewScrollDirectionVertical;
  FBackgroundColor         := TAlphaColors.Null;

  CellDicCustomViews := TDictionary < Pointer, TArray < TDPFiOSBaseControl >>.Create;

{$IFDEF IOS}
  MainQueue := dispatch_get_main_queue;
  // FUICollectionViewDelegate           := TUICollectionViewDelegate.Create( self );
  FUICollectionViewDelegateFlowLayout := TUICollectionViewDelegateFlowLayout.Create( self );
  FUICollectionViewDataSource         := TUICollectionViewDataSource.Create( self );

  FDPFUICollectionViewFlowLayout := TUICollectionViewFlowLayout.Wrap( TUICollectionViewFlowLayout.Alloc.init );

  FLocalUICollectionView := TILocalUICollectionView.Create( Self, FDPFUICollectionViewFlowLayout );
  FDPFCollectionView     := UICollectionView( FLocalUICollectionView.Super );

  FUIControl := FDPFCollectionView;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUICollectionView.Destroy;
begin
  ClearBuffer;
{$IFDEF IOS}
  CellDicCustomViews.DisposeOf;
  // FUICollectionViewDelegate.DisposeOf;
  FUICollectionViewDelegateFlowLayout.DisposeOf;
  FUICollectionViewDataSource.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUICollectionView.Loaded;
begin

  FDPFCollectionView.setDelegate( FUICollectionViewDelegateFlowLayout { FUICollectionViewDelegate }.GetObjectID );
  FDPFCollectionView.setDataSource( FUICollectionViewDataSource.GetObjectID );

  FDPFCollectionView.registerClass( objc_getClass( 'UICollectionViewCell' ), NSStr( 'DPFCellIdentifier' ) );
  SetBackgroundColor( FBackgroundColor );

  SetAllowsMultipleSelection( FAllowsMultipleSelection );
  SetAllowsSelection( FAllowsSelection );
  SetScrollDirection( FScrollDirection );

  AddSubView( Self, ParentControl );

  FDPFCollectionView.reloadData;
  // ----------------------------
  // Important
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.SelectItem( SecNo, ItemNo: NativeInt; Animated: Boolean; scrollPosition: Integer );
var
  idx: NSIndexPath;
begin
  idx := TNSIndexPath.Wrap( DPF.iOS.Classes.TNSIndexPath.OCClass.indexPathForItem( ItemNo, SecNo ) );
  FUICollectionViewDelegateFlowLayout.collectionView( FDPFCollectionView, idx );
  FDPFCollectionView.selectItemAtIndexPath( idx, animated, scrollPosition );
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.ReloadData;
begin
  FDPFCollectionView.reloadData;
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.ReloadRow( const Section, Row: Integer );
var
  Arr      : NSArray;
  IndexPath: NSIndexPath;
begin
  dispatch_sync( MainQueue,
    procedure
    begin
      if ( Section > -1 ) and ( Row > -1 ) then
      begin
        IndexPath := TNSIndexPath.Wrap( DPF.iOS.Classes.TNSIndexPath.OCClass.indexPathForItem( Row, Section ) );
        Arr := TNSArray.Wrap( TNSArray.OCClass.arrayWithObject( ( indexPath as ILocalObject ).GetObjectID ) );
        FDPFCollectionView.reloadItemsAtIndexPaths( Arr );
      end;
    end );
end;

// ------------------------------------------------------------------------------
function TDPFUICollectionView.isSelected( const SectionNo: Longword; const ItemNo: NativeInt ): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I  := 0 to high( FCollectionViewSelectedItems ) do
    if ( FCollectionViewSelectedItems[I].SectionNo = SectionNo ) and ( FCollectionViewSelectedItems[I].ItemNo = ItemNo ) then
    begin
      Result := True;
      break;
    end;
end;

// ------------------------------------------------------------------------------
function TDPFUICollectionView.GetRowCustomViews( const SectionNo: Longword; const ItemNo: NativeInt ): TArray<TDPFiOSBaseControl>;
var
  IndexPath: NSIndexPath;
  Cell     : UICollectionViewCell;
begin
  IndexPath := TNSIndexPath.Wrap( DPF.iOS.Classes.TNSIndexPath.OCClass.indexPathForItem( ItemNo, SectionNo ) );
  Cell      := FDPFCollectionView.cellForItemAtIndexPath( IndexPath );
  if Assigned( Cell ) then
  begin
    if CellDicCustomViews.ContainsKey( ( Cell as ILocalObject ).GetObjectID ) then
      CellDicCustomViews.TryGetValue( ( Cell as ILocalObject ).GetObjectID, Result );
  end;
  IndexPath := nil;
  Cell      := nil;
end;

// ------------------------------------------------------------------------------
function TDPFUICollectionView.GetRowCustomViews( IndexPath: NSIndexPath ): TArray<TDPFiOSBaseControl>;
var
  Cell: UICollectionViewCell;
begin
  Cell := FDPFCollectionView.cellForItemAtIndexPath( IndexPath );
  if Assigned( Cell ) then
  begin
    if CellDicCustomViews.ContainsKey( ( Cell as ILocalObject ).GetObjectID ) then
      CellDicCustomViews.TryGetValue( ( Cell as ILocalObject ).GetObjectID, Result );
  end
  else
    Cell := nil;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.SetScrollDirection( const Value: UICollectionViewScrollDirection );
begin
  FScrollDirection := Value;
{$IFDEF IOS}
  if assigned( FDPFUICollectionViewFlowLayout ) then
    FDPFUICollectionViewFlowLayout.setScrollDirection( value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.SetAllowsMultipleSelection( const Value: Boolean );
begin
  FAllowsMultipleSelection := Value;
{$IFDEF IOS}
  if assigned( FDPFCollectionView ) then
    FDPFCollectionView.setAllowsMultipleSelection( Value );

  setLength( FCollectionViewSelectedItems, 0 );
  ReloadData;

{$ENDIF}
end;

procedure TDPFUICollectionView.SetAllowsSelection( const Value: Boolean );
begin
  FAllowsSelection := Value;
{$IFDEF IOS}
  if assigned( FDPFCollectionView ) then
    FDPFCollectionView.setAllowsSelection( Value );

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FDPFCollectionView ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FDPFCollectionView.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FDPFCollectionView.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFUICollectionView.ClearBuffer;
var
  I, J: Integer;
  Arr : TArray<TArray<TDPFiOSBaseControl>>;
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
  finally
    SetLength( Arr, 0 );
  end;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFUICollectionView.Paint;
var
  CaptionRect: TRectF;
  rs: Integer;
begin
  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.Solid;
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, 0, 0, AllCorners, Alpha, TCornerType.InnerRound );
  end;

  Canvas.Fill.Color  := TAlphaColors.Gray;
  Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  CaptionRect        := ClipRect;
  PaintCaption( Self, name, CaptionRect, lbWordWrap, 1, CTextAlign[TDPFTextAlignment.taCenter] );
  Canvas.EndScene;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFView }

constructor TILocalUICollectionView.Create( ADPFUICollectionView: TDPFUICollectionView; ACollectionViewLayout: UICollectionViewLayout );
var
  V: Pointer;
begin
  inherited Create;
  FDPFCollectionView := ADPFUICollectionView;
  V                  := UICollectionView( Super ).initWithFrame( CGRectMake( 0, 0, 0, 0 ), ACollectionViewLayout );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TILocalUICollectionView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( ILocalUICollectionView );
end;

// ------------------------------------------------------------------------------
// TUICollectionViewDelegateFlowLayout
constructor TUICollectionViewDelegateFlowLayout.Create( ADPFUICollectionView: TDPFUICollectionView );
begin
  inherited Create;
  FDPFUICollectionView := ADPFUICollectionView;
end;

// ------------------------------------------------------------------------------
procedure TUICollectionViewDelegateFlowLayout.scrollViewDidScroll( scrollView: UIScrollView );
begin
end;

// ------------------------------------------------------------------------------
procedure TUICollectionViewDelegateFlowLayout.scrollViewWillBeginDragging( scrollView: UIScrollView );
begin
end;

// ------------------------------------------------------------------------------
procedure TUICollectionViewDelegateFlowLayout.scrollViewDidEndDecelerating( scrollView: UIScrollView );
begin
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; sizeForItemAtIndexPath: NSIndexPath ): CGSize; cdecl;
var
  SecNo, ItemNo: NativeInt;
  Width, Height: Single;
begin
  Width  := 100;
  Height := 100;
  if Assigned( FDPFUICollectionView.FOnItemSize ) then
  begin
    getSecRowIndex( sizeForItemAtIndexPath, SecNo, ItemNo );
    FDPFUICollectionView.FOnItemSize( FDPFUICollectionView, SecNo, ItemNo, Width, Height );
  end;
  result := CGSizeMake( Width, Height );
end;

// ------------------------------------------------------------------------------
procedure TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; didSelectItemAtIndexPath: NSIndexPath ); cdecl;
var
  SecNo, ItemNo: NativeInt;
  // Cell         : UICollectionViewCell;
  CV        : TArray<TDPFiOSBaseControl>;
  indexPaths: NSMutableArray;
  // I            : Integer;
  Founded: Boolean;
begin
  if assigned( FDPFUICollectionView.FOnSelectCell ) then
  begin
    indexPaths := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithObject( ( didSelectItemAtIndexPath as ILocalObject ).GetObjectID ) );

    getSecRowIndex( didSelectItemAtIndexPath, SecNo, ItemNo );
    CV := FDPFUICollectionView.GetRowCustomViews( didSelectItemAtIndexPath );
    FDPFUICollectionView.FOnSelectCell( FDPFUICollectionView, SecNo, ItemNo, CV );

    if FDPFUICollectionView.FAllowsMultipleSelection then
    begin
      Founded := FDPFUICollectionView.isSelected( SecNo, ItemNo );
      if not Founded then
        SetLength( FDPFUICollectionView.FCollectionViewSelectedItems, Length( FDPFUICollectionView.FCollectionViewSelectedItems ) + 1 )
      else
        DeleteX( FDPFUICollectionView.FCollectionViewSelectedItems, SecNo, ItemNo );
    end
    else
    begin
      if Length( FDPFUICollectionView.FCollectionViewSelectedItems ) = 0 then
        SetLength( FDPFUICollectionView.FCollectionViewSelectedItems, 1 );

      Founded := false;
    end;
    if not Founded then
    begin
      FDPFUICollectionView.FCollectionViewSelectedItems[high( FDPFUICollectionView.FCollectionViewSelectedItems )].SectionNo := SecNo;
      FDPFUICollectionView.FCollectionViewSelectedItems[high( FDPFUICollectionView.FCollectionViewSelectedItems )].ItemNo := ItemNo;
    end;

    // collectionView.reloadItemsAtIndexPaths( indexPaths );
    // FDPFUICollectionView.ReloadData;
  end;
end;

// ------------------------------------------------------------------------------
procedure TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; didDeselectItemAtIndexPath: NSIndexPath1 ); cdecl;
var
  SecNo, ItemNo: NativeInt;
  // Cell         : UICollectionViewCell ;
  CV: TArray<TDPFiOSBaseControl>;
begin
  getSecRowIndex( didDeselectItemAtIndexPath, SecNo, ItemNo );
  DeleteX( FDPFUICollectionView.FCollectionViewSelectedItems, SecNo, ItemNo );
  if assigned( FDPFUICollectionView.FOnDeSelectCell ) then
  begin
    CV := FDPFUICollectionView.GetRowCustomViews( { SecNo, ItemNo } didDeselectItemAtIndexPath );
    FDPFUICollectionView.FOnDeSelectCell( FDPFUICollectionView, SecNo, ItemNo, CV );

    // collectionView.reloadItemsAtIndexPaths( indexPaths );
    // FDPFUICollectionView.ReloadData;
  end;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; shouldSelectItemAtIndexPath: NSIndexPath2 ): Boolean; cdecl;
begin
  result := FDPFUICollectionView.FAllowsSelection;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; shouldDeselectItemAtIndexPath: NSIndexPath3 ): Boolean; cdecl;
begin
  result := FDPFUICollectionView.FAllowsSelection;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; shouldHighlightItemAtIndexPath: NSIndexPath4 ): Boolean; cdecl;
begin
  result := FDPFUICollectionView.FAllowsSelection;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; shouldShowMenuForItemAtIndexPath: NSIndexPath5 ): Boolean; cdecl;
begin
  result := FDPFUICollectionView.FAllowsSelection;
end;

// ------------------------------------------------------------------------------
(* function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; minimumInteritemSpacingForSectionAtIndex: NSUInteger ): CGFloat cdecl;
  begin
  result := -1;
  end;

  // ------------------------------------------------------------------------------
  function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; minimumLineSpacingForSectionAtIndex: NSInteger ): CGFloat cdecl;
  begin
  result := -1;
  end; *)

// ------------------------------------------------------------------------------
{ function TUICollectionViewDelegateFlowLayout.collectionView( collectionView: UICollectionView; layout: UICollectionViewLayout; insetForSectionAtIndex: NSInteger ): UIEdgeInsets; cdecl;
  begin
  result.left   := 2;
  result.right  := 2;
  result.top    := 2;
  result.bottom := 2;
  end; }

// ------------------------------------------------------------------------------
// TUICollectionViewDataSource
constructor TUICollectionViewDataSource.Create( ADPFUICollectionView: TDPFUICollectionView );
begin
  inherited Create;
  FDPFUICollectionView := ADPFUICollectionView;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDataSource.collectionView( collectionView: UICollectionView; cellForItemAtIndexPath: NSIndexPath ): UICollectionViewCell; cdecl;
var
  p                : Pointer;
  CustomViews      : TArray<TDPFiOSBaseControl>;
  ReUsable         : Boolean;
  xx, I            : Integer;
  CustomCellHandled: Boolean;
  SecNo, ItemNo    : NativeInt;
  C                : UIView;
  Size             : DPFNSize;
  isSelected       : Boolean;
  // idx              : NSIndexPath;
  tag: NSInteger;
  v  : UIView;

begin
  p      := collectionView.dequeueReusableCellWithReuseIdentifier( NSStr( 'DPFCellIdentifier' ), cellForItemAtIndexPath );
  result := TUICollectionViewCell.Wrap( p );

  Size.width  := result.frame.size.width;
  Size.height := result.frame.size.height;

  getSecRowIndex( cellForItemAtIndexPath, SecNo, ItemNo );

  // cell.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.greenColor ) );

  CustomViews := nil;
  // -----------------------------------------------------------------------
  // Check Draw Cell Event
  if Assigned( FDPFUICollectionView.FOnDrawCell ) then
  begin
    tag      := result.tag;
    ReUsable := tag = TAG_CELL_REUSED;

    result.setTag( TAG_CELL_REUSED );
    if FDPFUICollectionView.CellDicCustomViews.ContainsKey( ( result as ILocalObject ).GetObjectID ) then
    begin
      // ReUsable := true;
      FDPFUICollectionView.CellDicCustomViews.TryGetValue( ( result as ILocalObject ).GetObjectID, CustomViews );
    end;
    if ReUsable then
      for i := 0 to high( CustomViews ) do
      begin
        v := result.contentView.viewWithTag( TAG_BASE + i );
        if v = nil then
          CustomViews[i].UIControl := nil;

        CustomViews[i].UIControl := v;
      end;

    for i := 0 to result.contentView.subviews.count - 1 do
    begin
      v := TUIView.Wrap( result.contentView.subviews.objectAtIndex( i ) );
      // tag := v.tag;
    end;

    CustomCellHandled := true;
    isSelected        := FDPFUICollectionView.isSelected( SecNo, ItemNo );

    if isSelected then
    begin
      result.setSelected( true );
      collectionView.selectItemAtIndexPath( cellForItemAtIndexPath, false, 0 )
    end;

    FDPFUICollectionView.FOnDrawCell( FDPFUICollectionView, SecNo, ItemNo, Size, isSelected, CustomViews, CustomCellHandled );
    FDPFUICollectionView.CellDicCustomViews.AddOrSetValue( ( result as ILocalObject ).GetObjectID, CustomViews );

    // result.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.greenColor ) );

    if not ReUsable and ( Length( CustomViews ) > 0 ) then
    begin
      for i := 0 to high( CustomViews ) do
      begin
        if not CustomViews[i].isLoaded then
          CustomViews[i].Loaded;
        C := FDPFUICollectionView.GetControlView( CustomViews[i].UIControl );
        if C <> nil then
          C.setTag( TAG_BASE + i );
        TDPFiOSBaseControl( CustomViews[i] ).AutoRelease := True;
        if not ReUsable then
          result.contentView.addSubview( C );

        for xx := 0 to result.contentView.subviews.count - 1 do
        begin
          v := TUIView.Wrap( result.contentView.subviews.objectAtIndex( i ) );
          // tag := v.tag;
        end;

      end;
    end;

  end;
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDataSource.collectionView( collectionView: UICollectionView; numberOfItemsInSection: NSInteger ): NSInteger; cdecl;
var  rs: Integer;
begin
  result := 0;
  if Assigned( FDPFUICollectionView.OnGetNumberOfItemInSection ) then
  begin
    FDPFUICollectionView.OnGetNumberOfItemInSection( FDPFUICollectionView, numberOfItemsInSection, rs );
    Result := rs ;
  end
end;

// ------------------------------------------------------------------------------
function TUICollectionViewDataSource.numberOfSectionsInCollectionView( collectionView: UICollectionView ): NSInteger; cdecl;
Var rs:Integer ;
begin
  result := 0;
  if Assigned( FDPFUICollectionView.OnGetNumberOfSections ) then
  begin
    FDPFUICollectionView.OnGetNumberOfSections( FDPFUICollectionView, rs );
    Result:= rs;
  end
end;

// ------------------------------------------------------------------------------
{ function TUICollectionViewDataSource.collectionView( collectionView: UICollectionView; viewForSupplementaryElementOfKind: NSString; atIndexPath: NSIndexPath ): UICollectionReusableView; cdecl;
  begin

  end; }


// ------------------------------------------------------------------------------
(*
  constructor TUICollectionViewDelegate.Create( ADPFUICollectionView: TDPFUICollectionView );
  begin
  inherited Create;
  FDPFUICollectionView := ADPFUICollectionView;
  end;

  // ------------------------------------------------------------------------------
  procedure TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; didSelectItemAtIndexPath: NSIndexPath ); cdecl;
  var
  SecNo, ItemNo: LongWord;
  Cell        : UICollectionViewCell;
  CV          : TArray<TDPFiOSBaseControl>;
  begin
  if assigned( FDPFUICollectionView.FOnSelectCell ) then
  begin
  getSecRowIndex( didSelectItemAtIndexPath, SecNo, ItemNo );
  CV := FDPFUICollectionView.GetRowCustomViews( SecNo, ItemNo );
  FDPFUICollectionView.FOnSelectCell( FDPFUICollectionView, SecNo, ItemNo, CV );
  end;
  end;

  // ------------------------------------------------------------------------------
  procedure TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; didDeselectItemAtIndexPath: NSIndexPath1 ); cdecl;
  var
  SecNo, ItemNo: LongWord;
  Cell        : UICollectionViewCell;
  CV          : TArray<TDPFiOSBaseControl>;
  begin
  if assigned( FDPFUICollectionView.FOnDeSelectCell ) then
  begin
  getSecRowIndex( didDeselectItemAtIndexPath, SecNo, ItemNo );
  CV := FDPFUICollectionView.GetRowCustomViews( SecNo, ItemNo );
  FDPFUICollectionView.FOnDeSelectCell( FDPFUICollectionView, SecNo, ItemNo, CV );
  end;
  end;

  // ------------------------------------------------------------------------------
  function TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; shouldSelectItemAtIndexPath: NSIndexPath2 ): Boolean; cdecl;
  begin
  result := FDPFUICollectionView.FAllowsSelection;
  end;

  // ------------------------------------------------------------------------------
  function TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; shouldDeselectItemAtIndexPath: NSIndexPath3 ): Boolean; cdecl;
  begin
  result := FDPFUICollectionView.FAllowsSelection;
  end;

  // ------------------------------------------------------------------------------
  function TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; shouldHighlightItemAtIndexPath: NSIndexPath4 ): Boolean; cdecl;
  begin
  result := true;
  end;

  // ------------------------------------------------------------------------------
  function TUICollectionViewDelegate.collectionView( collectionView: UICollectionView; shouldShowMenuForItemAtIndexPath: NSIndexPath5 ): Boolean; cdecl;
  begin
  result := true;
  end;

  // ------------------------------------------------------------------------------
  procedure TUICollectionViewDelegate.scrollViewDidScroll( scrollView: UIScrollView );
  begin
  end;

  // ------------------------------------------------------------------------------

  procedure TUICollectionViewDelegate.scrollViewWillBeginDragging( scrollView: UIScrollView );
  begin
  end;

  // ------------------------------------------------------------------------------
  procedure TUICollectionViewDelegate.scrollViewDidEndDecelerating( scrollView: UIScrollView );
  begin
  end;
*)

{$ENDIF}

// ------------------------------------------------------------------------------
end.
