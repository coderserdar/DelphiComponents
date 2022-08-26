// ------------------------------------------------------------------------------
// DPF.iOS.UISearchBar Component
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
unit DPF.iOS.UISearchBar;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont,
  DPF.iOS.UIViewController,
  DPF.iOS.UIButton,
  DPF.iOS.UITableView,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
  FMX.Types;

type

  TDPFSearchBar = class;

{$IFDEF IOS}

  UISearchBarClass = interface( UIViewClass )
    ['{E1BF33AF-6961-436C-BD4B-0E510228697D}']
  end;

  UISearchBar = interface( UIView )
    ['{62830DEE-15CB-4560-858F-30E62B885A90}']
    function autocapitalizationType: UITextAutocapitalizationType; cdecl;
    function autocorrectionType: UITextAutocorrectionType; cdecl;
    function backgroundImage: UIImage; cdecl;
    function barStyle: UIBarStyle; cdecl;
    function delegate: Pointer; cdecl;
    function imageForSearchBarIcon( icon: UISearchBarIcon; state: UIControlState ): UIImage; cdecl;
    function isSearchResultsButtonSelected: Boolean; cdecl;
    function isTranslucent: Boolean; cdecl;
    function keyboardType: UIKeyboardType; cdecl;
    function placeholder: NSString; cdecl;
    function positionAdjustmentForSearchBarIcon( icon: UISearchBarIcon ): UIOffset; cdecl;
    function prompt: NSString; cdecl;
    function scopeBarBackgroundImage: UIImage; cdecl;
    function scopeBarButtonBackgroundImageForState( state: UIControlState ): UIImage; cdecl;
    function scopeBarButtonDividerImageForLeftSegmentState( leftState: UIControlState; rightSegmentState: UIControlState ): UIImage; cdecl;
    function scopeBarButtonTitleTextAttributesForState( state: UIControlState ): NSDictionary; cdecl;
    function scopeButtonTitles: NSArray; cdecl;
    function searchFieldBackgroundImageForState( state: UIControlState ): UIImage; cdecl;
    function searchFieldBackgroundPositionAdjustment: UIOffset; cdecl;
    function searchTextPositionAdjustment: UIOffset; cdecl;
    function selectedScopeButtonIndex: NSInteger; cdecl;
    procedure setAutocapitalizationType( autocapitalizationType: UITextAutocapitalizationType ); cdecl;
    procedure setAutocorrectionType( autocorrectionType: UITextAutocorrectionType ); cdecl;
    procedure setBackgroundImage( backgroundImage: UIImage ); cdecl; overload;
    procedure setBarStyle( barStyle: UIBarStyle ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setImage( iconImage: UIImage; forSearchBarIcon: UISearchBarIcon; state: UIControlState ); cdecl;
    procedure setKeyboardType( keyboardType: UIKeyboardType ); cdecl;
    procedure setPlaceholder( placeholder: NSString ); cdecl;
    procedure setPositionAdjustment( adjustment: UIOffset; forSearchBarIcon: UISearchBarIcon ); cdecl;
    procedure setPrompt( prompt: NSString ); cdecl;
    procedure setScopeBarBackgroundImage( scopeBarBackgroundImage: UIImage ); cdecl;
    procedure setScopeBarButtonBackgroundImage( backgroundImage: UIImage; forState: UIControlState ); cdecl;
    procedure setScopeBarButtonDividerImage( dividerImage: UIImage; forLeftSegmentState: UIControlState; rightSegmentState: UIControlState ); cdecl;
    procedure setScopeBarButtonTitleTextAttributes( attributes: NSDictionary; forState: UIControlState ); cdecl;
    procedure setScopeButtonTitles( scopeButtonTitles: NSArray ); cdecl;
    procedure setSearchFieldBackgroundImage( backgroundImage: UIImage; forState: UIControlState ); cdecl;
    procedure setSearchFieldBackgroundPositionAdjustment( searchFieldBackgroundPositionAdjustment: UIOffset ); cdecl;
    procedure setSearchResultsButtonSelected( searchResultsButtonSelected: Boolean ); cdecl;
    procedure setSearchTextPositionAdjustment( searchTextPositionAdjustment: UIOffset ); cdecl;
    procedure setSelectedScopeButtonIndex( selectedScopeButtonIndex: NSInteger ); cdecl;
    procedure setShowsBookmarkButton( showsBookmarkButton: Boolean ); cdecl;
    procedure setShowsCancelButton( showsCancelButton: Boolean ); cdecl; overload;
    procedure setShowsCancelButton( showsCancelButton: Boolean; animated: Boolean ); cdecl; overload;
    procedure setShowsScopeBar( showsScopeBar: Boolean ); cdecl;
    procedure setShowsSearchResultsButton( showsSearchResultsButton: Boolean ); cdecl;
    procedure setSpellCheckingType( spellCheckingType: Integer ); cdecl;
    procedure setText( text: NSString ); cdecl;
    procedure setTintColor( tintColor: UIColor ); cdecl;
    procedure setTranslucent( translucent: Boolean ); cdecl;
    function showsBookmarkButton: Boolean; cdecl;
    function showsCancelButton: Boolean; cdecl;
    function showsScopeBar: Boolean; cdecl;
    function showsSearchResultsButton: Boolean; cdecl;
    function spellCheckingType: Integer; cdecl;
    function text: NSString; cdecl;
    function tintColor: UIColor; cdecl;

    function searchBarStyle: Integer; cdecl; // Available in iOS 7.0 and later.
    procedure setSearchBarStyle( searchBarStyle: Integer ); cdecl; // Available in iOS 7.0 and later.

    function barTintColor: UIColor; cdecl;
    procedure setBarTintColor( tintColor: UIColor ); cdecl;
  end;

  TUISearchBar = class( TOCGenericImport<UISearchBarClass, UISearchBar> )
  end;

  UISearchBarDelegate = interface( NSObject )
    ['{F8A0F5E0-9E89-49E9-8133-16B7A5E443E6}']
    procedure searchBar( searchBar: UISearchBar; selectedScopeButtonIndexDidChange: NSInteger ); cdecl; overload;
    function searchBar( searchBar: UISearchBar; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; cdecl; overload;
    procedure searchBar( searchBar: UISearchBar; textDidChange: NSString ); cdecl; overload;

    procedure searchBarBookmarkButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarCancelButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarResultsListButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarSearchButtonClicked( searchBar: UISearchBar ); cdecl;

    function searchBarShouldBeginEditing( searchBar: UISearchBar ): Boolean; cdecl;
    function searchBarShouldEndEditing( searchBar: UISearchBar ): Boolean; cdecl;
    procedure searchBarTextDidBeginEditing( searchBar: UISearchBar ); cdecl;
    procedure searchBarTextDidEndEditing( searchBar: UISearchBar ); cdecl;
  end;

  TDPFSearchBarDelegate = class( TOCLocal )
  private
    FDPFSearchBar: TDPFSearchBar;
  public
    constructor Create( ADPFSearchBar: TDPFSearchBar );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure searchBar( searchBar: UISearchBar; selectedScopeButtonIndexDidChange: NSInteger ); overload; cdecl;
    function searchBar( searchBar: UISearchBar; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; overload; cdecl;
    procedure searchBar( searchBar: UISearchBar; textDidChange: NSString ); overload; cdecl;

    procedure searchBarBookmarkButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarCancelButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarResultsListButtonClicked( searchBar: UISearchBar ); cdecl;
    procedure searchBarSearchButtonClicked( searchBar: UISearchBar ); cdecl;

    function searchBarShouldBeginEditing( searchBar: UISearchBar ): Boolean; cdecl;
    function searchBarShouldEndEditing( searchBar: UISearchBar ): Boolean; cdecl;
    procedure searchBarTextDidBeginEditing( searchBar: UISearchBar ); cdecl;
    procedure searchBarTextDidEndEditing( searchBar: UISearchBar ); cdecl;
  end;
{$ENDIF}

  TDPFSearchBarScopebarClicked = procedure( Sender: TObject; SelectedIndex: Integer ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSearchBar = class( TDPFiOSBaseControl )
  private
    FShowCancelButton           : Boolean;
    FTextPlaceholder            : string;
    FBarStyle                   : TDPFBarStyle;
    FBackgroundImage            : string;
    FAutocorrectionType         : TDPFTextAutocorrectionType;
    FAutocapitalizationType     : TDPFTextAutocapitalizationType;
    FKeyboardType               : TDPFKeyboardType;
    FTextPrompt                 : string;
    FSearchResultsButtonSelected: Boolean;
    FShowsSearchResultsButton   : Boolean;
    FShowsBookmarkButton        : Boolean;
    FShowsScopeBar              : Boolean;
    FSpellCheckingType          : TDPFTextSpellCheckingType;
    FTintColor                  : TAlphaColor;
    FText                       : string;
    FTranslucent                : Boolean;
    FOnSearchButtonClicked      : TNotifyEvent;
    FOnCancelButtonClicked      : TNotifyEvent;
    FOnResultsListButtonClicked : TNotifyEvent;
    FOnBookmarkButtonClicked    : TNotifyEvent;
    FTableView                  : TDPFUITableView;
    FOnEndEditing               : TNotifyEvent;
    FOnBeginEditing             : TNotifyEvent;
    FSearchWithTyping           : Boolean;
    FOnChanged                  : TNotifyEvent;
    FAutoSizeToFit              : Boolean;
    FScopeButtonTitles          : TStrings;
    FScopebarClicked            : TDPFSearchBarScopebarClicked;
    FSelectedScopeButtonIndex   : Integer;
    FBackgroundColor            : TAlphaColor;
    FMaxLength                  : Byte;
{$IFDEF IOS}
    FDPFSearchBar        : UISearchBar;
    FDPFSearchBarDelegate: TDPFSearchBarDelegate;
{$ENDIF}
    procedure SetTextPlaceholder( const Value: string );
    procedure SetBarStyle( const Value: TDPFBarStyle );
    procedure SetBackgroundImage( const Value: string );
    procedure setKeyboardType( const Value: TDPFKeyboardType );
    procedure SetTextPrompt( const Value: string );
    function GetText: string;
    procedure setText( const Value: string );
    procedure setTintColor( const Value: TAlphaColor );
    procedure SetAutoSizeToFit( const Value: Boolean );
    procedure SetScopeButtonTitles( const Value: TStrings );
    procedure SetSelectedScopeButtonIndex( const Value: Integer );
    procedure SetShowsScopeBar( const Value: Boolean );
    procedure SetBackgroundColor( const Value: TAlphaColor );
  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; override;
    procedure sizeToFit;
    procedure UpdateScopeButtons;
  published
    property SearchResultsButtonSelected: Boolean read FSearchResultsButtonSelected write FSearchResultsButtonSelected default false;
    property ShowsSearchResultsButton   : Boolean read FShowsSearchResultsButton write FShowsSearchResultsButton default false;
    property ShowsBookmarkButton        : Boolean read FShowsBookmarkButton write FShowsBookmarkButton default false;
    property ShowsScopeBar              : Boolean read FShowsScopeBar write SetShowsScopeBar default false;
    property ShowCancelButton           : Boolean read FShowCancelButton write FShowCancelButton default true;
    property TextPlaceholder            : string read FTextPlaceholder write SetTextPlaceholder;
    property TextPrompt                 : string read FTextPrompt write SetTextPrompt;
    property BarStyle                   : TDPFBarStyle read FBarStyle write SetBarStyle default TDPFBarStyle.bsDefault;
    property BackgroundImage            : string read FBackgroundImage write SetBackgroundImage;
    property BackgroundColor            : TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property AutocorrectionType         : TDPFTextAutocorrectionType read FAutocorrectionType write FAutocorrectionType default TDPFTextAutocorrectionType.tatDefault;
    property AutocapitalizationType     : TDPFTextAutocapitalizationType read FAutocapitalizationType write FAutocapitalizationType default TDPFTextAutocapitalizationType.tapNone;
    property KeyboardType               : TDPFKeyboardType read FKeyboardType write setKeyboardType default ktDefault;
    property SpellCheckingType          : TDPFTextSpellCheckingType read FSpellCheckingType write FSpellCheckingType default TDPFTextSpellCheckingType.tsctDefault;
    property Text                       : string read GetText write setText;
    property TintColor                  : TAlphaColor read FTintColor write setTintColor default TAlphaColors.null;
    property Translucent                : Boolean read FTranslucent write FTranslucent default false;
    property SearchWithTyping           : Boolean read FSearchWithTyping write FSearchWithTyping default true;
    property AutoSizeToFit              : Boolean read FAutoSizeToFit write SetAutoSizeToFit default true;
    property ScopeButtonTitles          : TStrings read FScopeButtonTitles write SetScopeButtonTitles;
    property SelectedScopeButtonIndex   : Integer read FSelectedScopeButtonIndex write SetSelectedScopeButtonIndex default 0;
    property TableView                  : TDPFUITableView read FTableView write FTableView;
    property MaxLength                  : Byte read FMaxLength write FMaxLength default 20;

    property OnBookmarkButtonClicked   : TNotifyEvent read FOnBookmarkButtonClicked write FOnBookmarkButtonClicked;
    property OnCancelButtonClicked     : TNotifyEvent read FOnCancelButtonClicked write FOnCancelButtonClicked;
    property OnResultsListButtonClicked: TNotifyEvent read FOnResultsListButtonClicked write FOnResultsListButtonClicked;
    property OnSearchButtonClicked     : TNotifyEvent read FOnSearchButtonClicked write FOnSearchButtonClicked;
    property OnBeginEditing            : TNotifyEvent read FOnBeginEditing write FOnBeginEditing;
    property OnEndEditing              : TNotifyEvent read FOnEndEditing write FOnEndEditing;
    property OnChanged                 : TNotifyEvent read FOnChanged write FOnChanged;
    property OnScopebarClicked         : TDPFSearchBarScopebarClicked read FScopebarClicked write FScopebarClicked;

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
{ TDPFSearchBar }
// ------------------------------------------------------------------------------
constructor TDPFSearchBar.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FMaxLength                   := 20;
  ControlCaption               := 'SearchDisplayController';
  FShowCancelButton            := true;
  FAutoSizeToFit               := true;
  FBarStyle                    := TDPFBarStyle.bsDefault;
  FAutocorrectionType          := TDPFTextAutocorrectionType.tatDefault;
  FAutocapitalizationType      := TDPFTextAutocapitalizationType.tapNone;
  FKeyboardType                := ktDefault;
  FSearchResultsButtonSelected := false;
  FShowsSearchResultsButton    := false;
  FShowsBookmarkButton         := false;
  FShowsScopeBar               := false;
  FSpellCheckingType           := TDPFTextSpellCheckingType.tsctDefault;
  FTintColor                   := TAlphaColors.Null;
  FBackgroundColor             := TAlphaColors.Null;
  FTranslucent                 := false;
  FSearchWithTyping            := true;
  FScopeButtonTitles           := TStringList.Create;
  FSelectedScopeButtonIndex    := 0;
{$IFDEF IOS}
  FDPFSearchBar         := TUISearchBar.Wrap( TUISearchBar.Alloc.init );
  FUIControl            := FDPFSearchBar;
  FDPFSearchBarDelegate := TDPFSearchBarDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFSearchBar.Destroy;
begin
{$IFDEF IOS}
  FDPFSearchBarDelegate.DisposeOf;
  FScopeButtonTitles.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFSearchBar.GetText: string;
begin
{$IFDEF IOS}
  if Assigned( FDPFSearchBar ) then
    FText := UTF8ToString( FDPFSearchBar.text.UTF8String );
{$ENDIF}
  Result := FText;
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.sizeToFit;
begin
{$IFDEF IOS}
  if Assigned( FDPFSearchBar ) then
    FDPFSearchBar.sizeToFit;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.UpdateScopeButtons;
{$IFDEF IOS}
var
  I  : Integer;
  NMA: NSMutableArray;
{$ENDIF}
begin
{$IFDEF IOS}
  FDPFSearchBar.setScopeButtonTitles( nil );
  if FScopeButtonTitles.Count = 0 then
    exit;

  NMA   := TNSMutableArray.Create;
  for I := 0 to FScopeButtonTitles.Count - 1 do
  begin
    NMA.addObject( ( NSStr( FScopeButtonTitles[I] ) as ILocalObject ).GetObjectID );
  end;
  FDPFSearchBar.setScopeButtonTitles( NMA );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFSearchBar.Loaded;
begin
  FDPFSearchBar.setShowsCancelButton( False );
  FDPFSearchBar.setAutocorrectionType( Integer( FAutocorrectionType ) );
  FDPFSearchBar.setAutocapitalizationType( Integer( FAutocapitalizationType ) );
  FDPFSearchBar.setSearchResultsButtonSelected( FSearchResultsButtonSelected );
  FDPFSearchBar.setShowsSearchResultsButton( FShowsSearchResultsButton );
  FDPFSearchBar.setShowsBookmarkButton( FShowsBookmarkButton );
  FDPFSearchBar.setShowsScopeBar( FShowsScopeBar );
  FDPFSearchBar.setSpellCheckingType( Integer( FSpellCheckingType ) );
  FDPFSearchBar.setTranslucent( FTranslucent );
  FDPFSearchBar.setHidden( not Visible );

  UpdateScopeButtons;
  SetSelectedScopeButtonIndex( FSelectedScopeButtonIndex );
  if ( FSelectedScopeButtonIndex <> 0 ) and Assigned( OnScopebarClicked ) then
    OnScopebarClicked( Self, FSelectedScopeButtonIndex );

  SetTintColor( FTintColor );
  SetText( FText );
  SetTextPrompt( FTextPrompt );
  SetKeyboardType( FKeyboardType );
  SetBackgroundImage( FBackgroundImage );
  SetBackgroundColor( FBackgroundColor );
  SetBarStyle( FBarStyle );
  SetTextPlaceholder( FTextPlaceholder );

  FDPFSearchBar.setDelegate( FDPFSearchBarDelegate.GetObjectID );

  SetAutoSizeToFit( FAutoSizeToFit );

  Height := FDPFSearchBar.frame.size.height;

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.Resize;
begin
  inherited;
{$IFDEF IOS}
  if ShowsScopeBar then
    Height := 88
  else
    Height := 44;
  if FDPFSearchBar <> nil then
  begin
    FDPFSearchBar.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  end;
{$ELSE}
  if ShowsScopeBar then
    Height := 88
  else
    Height := 44;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetAutoSizeToFit( const Value: Boolean );
begin
  FAutoSizeToFit := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    if Value then
      sizeToFit;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if ( FDPFSearchBar <> nil ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
    begin
      if TOSVersion.Major > 6 then
        FDPFSearchBar.setBarTintColor( TColorToUIColor( Value ) );
      FDPFSearchBar.setBackgroundColor( TColorToUIColor( Value ) )
    end
    else
    begin
      { for i := 0 to FDPFSearchBar.subviews.count - 1 do
        begin
        if TUIView.Wrap( FDPFSearchBar.subviews.objectAtIndex( i ) ).isKindOfClass( objc_getClass( 'UISearchBarBackground' ) ) then
        TUIView.Wrap( FDPFSearchBar.subviews.objectAtIndex( i ) ).setAlpha( 0 );
        end; }
      // FDPFSearchBar.setBackgroundImage( TUIImage.Wrap( TUIImage.Alloc.init ) );
      if TOSVersion.Major > 6 then
        FDPFSearchBar.setBarTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
      FDPFSearchBar.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Img: UIImage;
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    if FBackgroundImage <> '' then
    begin
      Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FBackgroundImage ) ) );
      FDPFSearchBar.setBackgroundImage( Img );
      Img := nil;
    end
    else
      FDPFSearchBar.setBackgroundImage( nil );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetBarStyle( const Value: TDPFBarStyle );
begin
  FBarStyle := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    FDPFSearchBar.setBarStyle( Integer( Value ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.setKeyboardType( const Value: TDPFKeyboardType );
begin
  FKeyboardType := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    FDPFSearchBar.setKeyboardType( Integer( FKeyboardType ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetScopeButtonTitles( const Value: TStrings );
begin
  FScopeButtonTitles.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetSelectedScopeButtonIndex( const Value: Integer );
begin
  FSelectedScopeButtonIndex := Value;
{$IFDEF IOS}
  if Assigned( FDPFSearchBar ) and ( FScopeButtonTitles.Count > 0 ) and ( Value < FScopeButtonTitles.Count ) then
    FDPFSearchBar.setSelectedScopeButtonIndex( Value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetShowsScopeBar( const Value: Boolean );
begin
  FShowsScopeBar := Value;
{$IFDEF IOS}
  Resize;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.setText( const Value: string );
begin
  FText := Value;
{$IFDEF IOS}
  if Assigned( FDPFSearchBar ) then
    FDPFSearchBar.setText( NSStr( value ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetTintColor( const Value: TAlphaColor );
begin
  FTintColor := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    if FTintColor <> TAlphaColors.Null then
      FDPFSearchBar.setTintColor( TColorToUIColor( FTintColor ) )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetTextPlaceholder( const Value: string );
begin
  FTextPlaceholder := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    FDPFSearchBar.setPlaceholder( NSStr( FTextPlaceholder ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBar.SetTextPrompt( const Value: string );
begin
  FTextPrompt := Value;
{$IFDEF IOS}
  if FDPFSearchBar <> nil then
  begin
    FDPFSearchBar.setPrompt( NSStr( FTextPrompt ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFSearchBar.Paint;
var
  R, Seg, CaptionRect: TRectF;
  SegmentWidth, x    : single;
  i                  : integer;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // SearchBar
  R        := ClipRect;
  R.Bottom := iOS_GUI_Bitmaps.SearchBar.BG.Height;
  BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.BG, R );
  BitmapToPosition( Self, iOS_GUI_Bitmaps.SearchBar.Field_Left, 5, 7 );
  R.Left   := 5 + iOS_GUI_Bitmaps.SearchBar.Field_Left.Width;
  R.Top    := 7;
  R.Right  := Width - 5 - iOS_GUI_Bitmaps.SearchBar.Field_Right.Width;
  R.Bottom := R.Top + iOS_GUI_Bitmaps.SearchBar.Field_BG.Height;
  BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Field_BG, R );
  BitmapToPosition( Self, iOS_GUI_Bitmaps.SearchBar.Field_Right, R.Right, 7 );
  BitmapToPosition( Self, iOS_GUI_Bitmaps.SearchBar.SearchIcon, 15, 14 );
  R.Left   := 35;
  R.Top    := 9;
  R.Bottom := R.Top + iOS_GUI_Bitmaps.SearchBar.Field_BG.Height;
  R.Right  := Width - 5 - iOS_GUI_Bitmaps.SearchBar.Field_Right.Width;
  if ShowsSearchResultsButton then
  begin
    BitmapToPosition( Self, iOS_GUI_Bitmaps.SearchBar.Results, Width - 30, 12 );
    R.Right := Width - 42;
  end
  else if ShowsBookmarkButton then
  begin
    BitmapToPosition( Self, iOS_GUI_Bitmaps.SearchBar.Bookmarks, Width - 45, 8 );
    R.Right := Width - 57;
  end;
  Canvas.Font.Size   := 15;
  Canvas.Font.Style  := [];
  Canvas.Font.Family := 'Arial';
  Canvas.Fill.Color  := $FFB3B3B3;
  PaintCaption( Self, TextPlaceholder, R, lbTailTruncation, 1, TTextAlign.Leading );
  // ScopeBar
  if ShowsScopeBar then
  begin
    R     := ClipRect;
    R.Top := 44;
    BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_BG, R );
    if ScopeButtonTitles.Count > 0 then
    begin
      SegmentWidth := ( Width - 10 - ( ScopeButtonTitles.Count - 1 ) ) / ScopeButtonTitles.Count;
      x            := 5;
      R.Top        := 51;
      R.Bottom     := R.Top + iOS_GUI_Bitmaps.SearchBar.Scope_BG_Down.Height;
      Seg.Top      := R.Top;
      Seg.Bottom   := R.Bottom;
      for i        := 0 to ScopeButtonTitles.Count - 1 do
      begin
        Seg.Left        := Round( x );
        Seg.Right       := Round( x + SegmentWidth );
        CaptionRect     := Seg;
        CaptionRect.Top := CaptionRect.Top + 4;
        // Left border - if needed
        if i = 0 then
        begin
          R.Left  := Seg.Left;
          R.Right := Seg.Left + iOS_GUI_Bitmaps.SearchBar.Scope_Left_Down.Width;
          if SelectedScopeButtonIndex = i then
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Left_Down, R )
          else
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Left_Up, R );
          Seg.Left := Seg.Left + iOS_GUI_Bitmaps.SearchBar.Scope_Left_Down.Width;
        end;
        // Right border - if needed
        if i = ScopeButtonTitles.Count - 1 then
        begin
          R.Left  := Seg.Right - iOS_GUI_Bitmaps.SearchBar.Scope_Right_Down.Width;
          R.Right := Seg.Right;
          if SelectedScopeButtonIndex = i then
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Right_Down, R )
          else
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Right_Up, R );
          Seg.Right := Seg.Right - iOS_GUI_Bitmaps.SearchBar.Scope_Right_Down.Width;
        end;
        // ButtonBG
        R.Left  := Seg.Left;
        R.Right := Seg.Right;
        if SelectedScopeButtonIndex = i then
          BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_BG_Down, R )
        else
          BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_BG_Up, R );
        x := Seg.Right;
        // Separator
        if i < ScopeButtonTitles.Count - 1 then
        begin
          R.Left  := Seg.Right;
          R.Right := Seg.Right + 1;
          if ( SelectedScopeButtonIndex = i ) or ( SelectedScopeButtonIndex = i + 1 ) then
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Separator_Down, R )
          else
            BitmapToRect( Self, iOS_GUI_Bitmaps.SearchBar.Scope_Separator_Down, R );
          x := x + 1;
        end;
        Canvas.Font.Size   := 13;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        if i <> SelectedScopeButtonIndex then
        begin
          CaptionRect.Top    := CaptionRect.Top + 1;
          CaptionRect.Bottom := CaptionRect.Bottom + 1;
          Canvas.Fill.Color  := $FFD8DEE2;
          PaintCaption( Self, ScopeButtonTitles[i], CaptionRect, lbMiddleTruncation, 1, TTextAlign.Center );
          CaptionRect.Top    := CaptionRect.Top - 1;
          CaptionRect.Bottom := CaptionRect.Bottom - 1;
          Canvas.Fill.Color  := $FF3F5C84;
          PaintCaption( Self, ScopeButtonTitles[i], CaptionRect, lbMiddleTruncation, 1, TTextAlign.Center );
        end
        else
        begin
          CaptionRect.Top    := CaptionRect.Top - 1;
          CaptionRect.Bottom := CaptionRect.Bottom - 1;
          Canvas.Fill.Color  := $FF525D59;
          PaintCaption( Self, ScopeButtonTitles[i], CaptionRect, lbMiddleTruncation, 1, TTextAlign.Center );
          CaptionRect.Top    := CaptionRect.Top + 1;
          CaptionRect.Bottom := CaptionRect.Bottom + 1;
          Canvas.Fill.Color  := $FFFFFFFF;
          PaintCaption( Self, ScopeButtonTitles[i], CaptionRect, lbMiddleTruncation, 1, TTextAlign.Center );
        end;
      end;
    end;
  end;
  Canvas.EndScene;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFSearchBarDelegate }
{$IFDEF IOS}

constructor TDPFSearchBarDelegate.Create( ADPFSearchBar: TDPFSearchBar );
begin
  inherited create;
  FDPFSearchBar := ADPFSearchBar;
end;

// ------------------------------------------------------------------------------
function TDPFSearchBarDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( UISearchBarDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBar( searchBar: UISearchBar; selectedScopeButtonIndexDidChange: NSInteger ); cdecl;
begin
  if Assigned( FDPFSearchBar.FScopebarClicked ) then
    FDPFSearchBar.FScopebarClicked( FDPFSearchBar, selectedScopeButtonIndexDidChange );
end;

// ------------------------------------------------------------------------------
function TDPFSearchBarDelegate.searchBar( searchBar: UISearchBar; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; cdecl;
begin
  result := ( SearchBar.text.length + replacementText.length - shouldChangeTextInRange.length ) <= FDPFSearchBar.FMaxLength;
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBar( searchBar: UISearchBar; textDidChange: NSString ); cdecl;
begin
  if Assigned( FDPFSearchBar.FTableView ) and FDPFSearchBar.SearchWithTyping then
    FDPFSearchBar.FTableView.SearchText := UTF8ToString( searchBar.text.UTF8String );

  if Assigned( FDPFSearchBar.FOnChanged ) then
    FDPFSearchBar.FOnChanged( FDPFSearchBar );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarBookmarkButtonClicked( searchBar: UISearchBar ); cdecl;
begin
  if Assigned( FDPFSearchBar.OnBookmarkButtonClicked ) then
    FDPFSearchBar.OnBookmarkButtonClicked( FDPFSearchBar );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarCancelButtonClicked( searchBar: UISearchBar ); cdecl;
begin
  if Assigned( FDPFSearchBar.OnCancelButtonClicked ) then
    FDPFSearchBar.OnCancelButtonClicked( FDPFSearchBar );
  searchBar.resignFirstResponder;
  searchBar.setText( NSStr( '' ) );
  if Assigned( FDPFSearchBar.FTableView ) then
    FDPFSearchBar.FTableView.SearchText := '';
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarResultsListButtonClicked( searchBar: UISearchBar ); cdecl;
begin
  if Assigned( FDPFSearchBar.OnResultsListButtonClicked ) then
    FDPFSearchBar.OnResultsListButtonClicked( FDPFSearchBar );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarSearchButtonClicked( searchBar: UISearchBar ); cdecl;
begin
  if Assigned( FDPFSearchBar.OnSearchButtonClicked ) then
    FDPFSearchBar.OnSearchButtonClicked( FDPFSearchBar );
  if Assigned( FDPFSearchBar.FTableView ) and not FDPFSearchBar.SearchWithTyping then
    FDPFSearchBar.FTableView.SearchText := UTF8ToString( searchBar.text.UTF8String );
  searchBar.resignFirstResponder;
end;

// ------------------------------------------------------------------------------
function TDPFSearchBarDelegate.searchBarShouldBeginEditing( searchBar: UISearchBar ): Boolean; cdecl;
begin
  result := true;
end;

// ------------------------------------------------------------------------------
function TDPFSearchBarDelegate.searchBarShouldEndEditing( searchBar: UISearchBar ): Boolean; cdecl;
begin
  result := true;
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarTextDidBeginEditing( searchBar: UISearchBar ); cdecl;
begin
  if FDPFSearchBar.ShowCancelButton then
    searchBar.setShowsCancelButton( true );
  if Assigned( FDPFSearchBar.FOnBeginEditing ) then
    FDPFSearchBar.FOnBeginEditing( FDPFSearchBar );
end;

// ------------------------------------------------------------------------------
procedure TDPFSearchBarDelegate.searchBarTextDidEndEditing( searchBar: UISearchBar ); cdecl;
begin
  searchBar.setShowsCancelButton( false );
  if Assigned( FDPFSearchBar.FOnEndEditing ) then
    FDPFSearchBar.FOnEndEditing( FDPFSearchBar );
end;

// ------------------------------------------------------------------------------
{$ENDIF}
{ TDPFSearchBarDelegate }

end.
