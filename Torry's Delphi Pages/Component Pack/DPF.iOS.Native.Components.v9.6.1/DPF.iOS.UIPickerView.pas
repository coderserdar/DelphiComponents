// ------------------------------------------------------------------------------
// DPF.iOS.UIPickerView Component
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
unit DPF.iOS.UIPickerView;

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

  TDPFPickerView = class;

  TDPFPickerKind = ( pvStandard162px, pvStandard180px, pvStandard216px, pvStretch );

{$IFDEF IOS}

  TDPFPickerDataSource = class( TOCLocal, UIPickerViewDataSource )
  private
    FDPFPickerView: TDPFPickerView;
  public
    constructor Create( ADPFPickerView: TDPFPickerView );
    destructor Destroy; override;

    function numberOfComponentsInPickerView( pickerView: UIPickerView ): NSInteger; cdecl;
    function pickerView( pickerView: UIPickerView; numberOfRowsInComponent: NSInteger ): NSInteger; cdecl;
  end;

  // ------------------------------------------------------------------------------
  UIPickerViewDelegate = interface( IObjectiveC )
    ['{EAD85976-0A8C-4FC0-84BF-6CB8C9C85D82}']
    procedure pickerView( pickerView: UIPickerView; didSelectRow: NSInteger; inComponent: NSInteger ); cdecl; overload;
    function pickerView( pickerView: UIPickerView; titleForRow: NSInteger; forComponent: NSInteger1 ): NSString; cdecl; overload;

    function pickerView( pickerView: UIPickerView; viewForRow: NSInteger; forComponent: NSInteger; reusingView: UIView ): UIView; cdecl; overload;
    // function pickerView(pickerView: UIPickerView; widthForComponent: NSInteger): Single; cdecl; overload;
    // function pickerView(pickerView: UIPickerView; rowHeightForComponent: NSInteger): Single; cdecl; overload;
  end;

  TDPFPickerDelegate = class( TOCLocal, UIPickerViewDelegate )
  private
    FDPFPickerView: TDPFPickerView;
  public
    constructor Create( ADPFPickerView: TDPFPickerView );
    destructor Destroy; override;

    procedure pickerView( pickerView: UIPickerView; didSelectRow: NSInteger; inComponent: NSInteger ); overload; cdecl;
    function pickerView( pickerView: UIPickerView; titleForRow: NSInteger; forComponent: NSInteger1 ): NSString; overload; cdecl;
    function pickerView( pickerView: UIPickerView; viewForRow: NSInteger; forComponent: NSInteger; reusingView: UIView ): UIView; overload; cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  // PickerView Sections
  TDPFPickerViewItem = class( TCollectionItem )
  private
    FOwner          : TCollection;
    FItems          : TStrings;
    FSelectedRow    : integer;
    FWidth          : Integer;
    FHeight         : Integer;
    FFont           : TDPFFont;
    FBackgroundColor: TAlphaColor;
    FTextColor      : TAlphaColor;
    FTextAlignment  : TDPFTextAlignment;
    procedure SetItems( const Value: TStrings );
    procedure SetSelectedRow( const Value: Integer );
    function GetSelectedRow: Integer;
    procedure SetFont( const Value: TDPFFont );
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
  published

    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.Null;
    property TextColor      : TAlphaColor read FTextColor write FTextColor default TAlphaColors.Black;
    property TextAlignment  : TDPFTextAlignment read FTextAlignment write FTextAlignment default TDPFTextAlignment.taCenter;
    property Font           : TDPFFont read FFont write SetFont;
    property Width          : Integer read FWidth write FWidth default 176;
    property Height         : Integer read FHeight write FHeight default 40;
    property Items          : TStrings read FItems write SetItems;
    property SelectedRow    : Integer read GetSelectedRow write SetSelectedRow stored False default -1;

  end;

  // ----------------------------------------------------------------------------
  TDPFPickerViewCollection = class( TCollection )
  private
    FDPFPickerView: TDPFPickerView;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFPickerViewItem;
    procedure SetItem( Index: Integer; Value: TDPFPickerViewItem );

  public
    constructor Create( ADPFPickerView: TDPFPickerView );
    destructor Destroy; override;
    procedure Update( Item: TCollectionItem ); override;

    function Add: TDPFPickerViewItem;
    function Insert( Index: Integer ): TDPFPickerViewItem;

    property Items[index: Integer]: TDPFPickerViewItem read GetItem write SetItem; default;
  end;

  // ------------------------------------------------------------------------------
  TDPFPickerOnChanged = procedure( Sender: TObject; Component: Integer; Row: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFPickerView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFPickerView      : UIPickerView;
    FDPFPickerDelegate  : TDPFPickerDelegate;
    FDPFPickerDataSource: TDPFPickerDataSource;
{$ENDIF}
    FBackgroundColor        : TAlphaColor;
    FDPFPickerViewCollection: TDPFPickerViewCollection;
    FShowsSelectionIndicator: Boolean;
    FOnChanged              : TDPFPickerOnChanged;
    FPickerKind             : TDPFPickerKind;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetDPFPickerViewCollection( const Value: TDPFPickerViewCollection );
    procedure SetShowsSelectionIndicator( const Value: Boolean );
    procedure SetPickerKind( const Value: TDPFPickerKind );

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
  published
    property ShowsSelectionIndicator: Boolean read FShowsSelectionIndicator write SetShowsSelectionIndicator default True;
    property BackgroundColor        : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property PickerComponents       : TDPFPickerViewCollection read FDPFPickerViewCollection write SetDPFPickerViewCollection;
    property PickerKind             : TDPFPickerKind read FPickerKind write SetPickerKind default TDPFPickerKind.pvStandard162px;

    property OnChanged: TDPFPickerOnChanged read FOnChanged write FOnChanged;

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
{ TDPFPickerView }
// ------------------------------------------------------------------------------
constructor TDPFPickerView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption           := 'PickerView';
  FBackgroundColor         := TAlphaColors.Null;
  FShowsSelectionIndicator := True;
  FPickerKind              := TDPFPickerKind.pvStandard162px;
  SetPickerKind( FPickerKind );

  FDPFPickerViewCollection := TDPFPickerViewCollection.Create( Self );

{$IFDEF IOS}
  FDPFPickerDelegate   := TDPFPickerDelegate.Create( Self );
  FDPFPickerDataSource := TDPFPickerDataSource.Create( Self );
  FDPFPickerView       := TUIPickerView.Create;
  FUIControl           := FDPFPickerView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFPickerView.Destroy;
begin
  FDPFPickerViewCollection.DisposeOf;
{$IFDEF IOS}
  FDPFPickerDelegate.DisposeOf;
  FDPFPickerDataSource.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFPickerView.Loaded;
var
  i: integer;
begin
  SetPickerKind( FPickerKind );
  SetBackgroundColor( FBackgroundColor );
  SetShowsSelectionIndicator( FShowsSelectionIndicator );

  FDPFPickerView.setDataSource( FDPFPickerDataSource.GetObjectID );
  FDPFPickerView.setDelegate( FDPFPickerDelegate.GetObjectID );

  for i := 0 to FDPFPickerViewCollection.Count - 1 do
  begin
    FDPFPickerViewCollection.Items[i].SelectedRow := FDPFPickerViewCollection.Items[i].FSelectedRow;
  end;

  Resize;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFPickerView.RefreshNeeded;
begin
{$IFDEF IOS}
  if FDPFPickerView <> nil then
    FDPFPickerView.reloadAllComponents;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerView.Resize;
begin
  inherited;
  if ( Height <> 162 ) and ( Height <> 180 ) and ( Height <> 216 ) then
    PickerKind := pvStretch;
{$IFDEF IOS}
  if FDPFPickerView <> nil then
    FDPFPickerView.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerView.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFPickerView.Paint;
var
  Bars    : array of TRectF;
  Rows    : array of single;
  w, x, th: single;
  q, i    : integer;

  procedure DrawControl( Bitmaps: TPickerView_Bitmaps );
  var
    i, j, start: integer;

  begin
    BitmapToRect( Self, Bitmaps.FrameLeft, RectF( 0, 0, Bitmaps.FrameLeft.Width, Height ) );
    BitmapToRect( Self, Bitmaps.WheelBG, RectF( Bitmaps.FrameLeft.Width, 0, Width - Bitmaps.FrameRight.Width, Height ) );
    BitmapToRect( Self, Bitmaps.FrameRight, RectF( Width - Bitmaps.FrameRight.Width, 0, Width, Height ) );
    if ShowsSelectionIndicator then
    begin
      BitmapToPosition( Self, Bitmaps.SelectorLeft, Bitmaps.FrameLeft.Width - Bitmaps.SelectorLeft.Width, Height / 2 - Bitmaps.SelectorLeft.Height / 2 );
      BitmapToRect( Self, Bitmaps.SelectorBG, RectF( Bitmaps.FrameLeft.Width, Height / 2 - Bitmaps.SelectorLeft.Height / 2, Width - Bitmaps.FrameRight.Width, Height / 2 + Bitmaps.SelectorLeft.Height / 2 ) );
      BitmapToPosition( Self, Bitmaps.SelectorRight, Width - Bitmaps.FrameRight.Width, Height / 2 - Bitmaps.SelectorRight.Height / 2 );
    end;
    for i := 0 to high( Bars ) - 1 do
    begin
      with PickerComponents[i] do
      begin
        if Items.Count > 0 then
        begin
          if SelectedRow = -1 then
            start := -2
          else
            start := SelectedRow - 2;
          for j   := 0 to 4 do
          begin
            if j = 2 then
              Canvas.Fill.Color := $99000000
            else
              Canvas.Fill.Color := $FF000000;
            if ( start >= 0 ) and ( start < Items.Count ) then
              PaintCaption( Self, Items[start], RectF( Bars[i].Left, Rows[j], Bars[i].Right, Rows[j] + th ), lbTailTruncation, 1, TTextAlign.Leading );
            inc( start );
          end;
        end;
      end;
    end;
    BitmapToRect( Self, Bitmaps.FrameMiddle, RectF( Bitmaps.FrameLeft.Width, 0, Width - Bitmaps.FrameRight.Width, Height ) );
    for i := 0 to high( Bars ) - 1 do
    begin
      BitmapToRect( Self, Bitmaps.Separator, RectF( Bars[i].Right, 0, Bars[i].Right + Bitmaps.Separator.Width, Height ) );
      if ShowsSelectionIndicator then
        BitmapToPosition( Self, Bitmaps.SelectorSeparator, Bars[i].Right, Height / 2 - Bitmaps.SelectorSeparator.Height / 2 );
    end;
  end;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  SetLength( Bars, PickerComponents.Count );
  if PickerComponents.Count > 0 then
  begin
    x     := Width;
    x     := x - iOS_GUI_Bitmaps.PickerView.Size162.FrameLeft.Width;
    x     := x - iOS_GUI_Bitmaps.PickerView.Size162.FrameRight.Width;
    x     := x - iOS_GUI_Bitmaps.PickerView.Size162.Separator.Width * ( PickerComponents.Count - 1 );
    w     := x / PickerComponents.Count;
    x     := iOS_GUI_Bitmaps.PickerView.Size162.FrameLeft.Width;
    for q := 0 to high( Bars ) do
    begin
      Bars[q].Left   := Round( x );
      Bars[q].Top    := 0;
      Bars[q].Right  := Round( x + w );
      Bars[q].Bottom := Height;
      x              := x + w + iOS_GUI_Bitmaps.PickerView.Size162.Separator.Width;
    end;
  end;
  Canvas.Font.Size   := 17;
  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := 'Helvetica';
  Canvas.Fill.Color  := $FF000000;
  SetLength( Rows, 5 );
  x     := 0;
  th    := Canvas.TextHeight( 'A' );
  w     := ( Height - ( 5 * th ) ) / 4;
  for i := 0 to 4 do
  begin
    Rows[i] := x;
    x       := x + th + w;
  end;
  case PickerKind of
    pvStandard162px:
      DrawControl( iOS_GUI_Bitmaps.PickerView.Size162 );
    pvStandard180px:
      DrawControl( iOS_GUI_Bitmaps.PickerView.Size180 );
    pvStandard216px, pvStretch:
      DrawControl( iOS_GUI_Bitmaps.PickerView.Size216 );
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFPickerView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FDPFPickerView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FDPFPickerView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FDPFPickerView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerView.SetDPFPickerViewCollection( const Value: TDPFPickerViewCollection );
begin
  FDPFPickerViewCollection.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerView.SetPickerKind( const Value: TDPFPickerKind );
begin
  FPickerKind := Value;
  case FPickerKind of
    pvStandard162px:
      Height := 162;
    pvStandard180px:
      Height := 180;
    pvStandard216px:
      Height := 216;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerView.SetShowsSelectionIndicator( const Value: Boolean );
begin
  FShowsSelectionIndicator := Value;
{$IFDEF IOS}
  if FDPFPickerView <> nil then
  begin
    FDPFPickerView.SetShowsSelectionIndicator( FShowsSelectionIndicator );
  end;
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFPickerDataSource }
constructor TDPFPickerDataSource.Create( ADPFPickerView: TDPFPickerView );
begin
  inherited Create;
  FDPFPickerView := ADPFPickerView;
end;

// ------------------------------------------------------------------------------
destructor TDPFPickerDataSource.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFPickerDataSource.numberOfComponentsInPickerView( pickerView: UIPickerView ): NSInteger;
begin
  Result := FDPFPickerView.PickerComponents.Count;
end;

// ------------------------------------------------------------------------------
function TDPFPickerDataSource.pickerView( pickerView: UIPickerView; numberOfRowsInComponent: NSInteger ): NSInteger;
begin
  Result := FDPFPickerView.PickerComponents.Items[numberOfRowsInComponent].FItems.Count;
end;

// ------------------------------------------------------------------------------
{ TDPFPickerDelegate }

constructor TDPFPickerDelegate.Create( ADPFPickerView: TDPFPickerView );
begin
  inherited Create;
  FDPFPickerView := ADPFPickerView;
end;

// ------------------------------------------------------------------------------
destructor TDPFPickerDelegate.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerDelegate.pickerView( pickerView: UIPickerView; didSelectRow: NSInteger; inComponent: NSInteger ); cdecl;
begin
  FDPFPickerView.FDPFPickerViewCollection.Items[inComponent].FSelectedRow := didSelectRow;
  if Assigned( FDPFPickerView.FOnChanged ) then
    FDPFPickerView.FOnChanged( FDPFPickerView, inComponent, didSelectRow );
end;

// ------------------------------------------------------------------------------
function TDPFPickerDelegate.pickerView( pickerView: UIPickerView; titleForRow: NSInteger; forComponent: NSInteger1 ): NSString; cdecl;
begin
  Result := NSStr( FDPFPickerView.PickerComponents.Items[Integer( forComponent )].FItems[titleForRow] );
end;

// ------------------------------------------------------------------------------
function TDPFPickerDelegate.pickerView( pickerView: UIPickerView; viewForRow: NSInteger; forComponent: NSInteger; reusingView: UIView ): UIView; cdecl;
var
  pickerLabel     : UILabel;
  frame           : CGRect;
  txt             : string;
  //cnt             : Integer;
  FBackgroundColor: TAlphaColor;
  FTextColor      : TAlphaColor;
begin
  frame       := CGRectMake( 0.0, 0.0, FDPFPickerView.FDPFPickerViewCollection.Items[forComponent].Width, FDPFPickerView.FDPFPickerViewCollection.Items[forComponent].Height );
  pickerLabel := TUILabel.Wrap( TUILabel.Alloc.initWithFrame( frame ) );

  FBackgroundColor := FDPFPickerView.PickerComponents.Items[Integer( forComponent )].FBackgroundColor;
  if FBackgroundColor = TAlphaColors.Null then
    pickerLabel.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    pickerLabel.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );

  FTextColor := FDPFPickerView.PickerComponents.Items[Integer( forComponent )].FTextColor;
  if FTextColor = TAlphaColors.Null then
    pickerLabel.setTextColor( TUIColor.Wrap( TUIColor.OCClass.blackColor ) )
  else
    pickerLabel.setTextColor( TColorToUIColor( FTextColor ) );

  pickerLabel.setTextAlignment( Integer( FDPFPickerView.PickerComponents.Items[Integer( forComponent )].FTextAlignment ) );
  pickerLabel.setFont( FDPFPickerView.PickerComponents.Items[Integer( forComponent )].Font._UIFont );

  txt := FDPFPickerView.PickerComponents.Items[Integer( forComponent )].FItems[viewForRow];
  pickerLabel.setText( NSStr( txt ) );

  Result := pickerLabel;
end;

{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFPickerViewItem }

constructor TDPFPickerViewItem.Create( AOwner: TCollection );
begin
  inherited Create( AOwner );
  FOwner           := AOwner;
  FItems           := TStringList.Create;
  FWidth           := 176;
  FHeight          := 40;
  FFont            := TDPFFont.Create;
  FBackgroundColor := TAlphaColors.Null;
  FTextColor       := TAlphaColors.Black;
  FTextAlignment   := TDPFTextAlignment.taCenter;
end;

// ------------------------------------------------------------------------------
destructor TDPFPickerViewItem.Destroy;
begin
  FFont.Free;
  FItems.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFPickerViewItem.GetDisplayName: string;
begin
  Result := Format( 'Component %d', [index] );
end;

// ------------------------------------------------------------------------------
function TDPFPickerViewItem.GetSelectedRow: Integer;
{$IFDEF IOS}
var
  F: TDPFPickerView;
{$ENDIF}
begin
{$IFDEF IOS}
  F := TDPFPickerView( TDPFPickerViewCollection( FOwner ).FDPFPickerView );
  if ( FSelectedRow = -1 ) and ( F <> nil ) and ( F.FDPFPickerView <> nil ) then
  begin
    FSelectedRow := F.FDPFPickerView.selectedRowInComponent( index );
  end;
{$ENDIF}
  Result := FSelectedRow;
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerViewItem.SetSelectedRow( const Value: Integer );
{$IFDEF IOS}
var
  F: TDPFPickerView;
{$ENDIF}
begin
  if ( Value < 0 ) or ( Value > Items.Count ) then
    exit;
  FSelectedRow := Value;
{$IFDEF IOS}
  F := TDPFPickerView( TDPFPickerViewCollection( FOwner ).FDPFPickerView );
  if ( F <> nil ) and ( F.FDPFPickerView <> nil ) then
  begin
    F.FDPFPickerView.selectRow( Value, index, True );
    if Assigned( F.OnChanged ) then
      F.OnChanged( F, index, Value );
  end;
{$ELSE}
  ( ( FOwner as TDPFPickerViewCollection ).Owner as TDPFPickerView ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerViewItem.SetFont( const Value: TDPFFont );
begin
  FFont.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerViewItem.SetItems( const Value: TStrings );
begin
  FItems.Assign( Value );
{$IFNDEF IOS}
  ( ( FOwner as TDPFPickerViewCollection ).Owner as TDPFPickerView ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFPickerViewCollection }

function TDPFPickerViewCollection.Add: TDPFPickerViewItem;
begin
  Result := inherited Add as TDPFPickerViewItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFPickerViewCollection.Create( ADPFPickerView: TDPFPickerView );
begin
  inherited Create( TDPFPickerViewItem );
  FDPFPickerView := ADPFPickerView;
end;

// ------------------------------------------------------------------------------
destructor TDPFPickerViewCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFPickerViewCollection.GetItem( Index: Integer ): TDPFPickerViewItem;
begin
  Result := inherited Items[index] as TDPFPickerViewItem;
end;

// ------------------------------------------------------------------------------
function TDPFPickerViewCollection.GetOwner: TPersistent;
begin
  Result := FDPFPickerView;
end;

// ------------------------------------------------------------------------------
function TDPFPickerViewCollection.Insert( Index: Integer ): TDPFPickerViewItem;
begin
  Result := inherited insert( index ) as TDPFPickerViewItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerViewCollection.SetItem( Index: Integer; Value: TDPFPickerViewItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFPickerViewCollection.Update( Item: TCollectionItem );
begin
  inherited update( item );
  { }
end;

// ------------------------------------------------------------------------------
end.
