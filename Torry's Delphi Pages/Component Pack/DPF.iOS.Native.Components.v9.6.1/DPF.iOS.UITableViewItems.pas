// ------------------------------------------------------------------------------
// DPF.iOS.UITableViewItems Base Classes
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
unit DPF.iOS.UITableViewItems;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Types,
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont;

type

  TTableItemCollection = class;
  TSectionCollection   = class;
  TSectionItem         = class;

  // ----------------------------------------------------------------------------
  TTableViewCellStyle        = ( tvcsDefault, tvcsValue1, tvcsValue2, tvcsSubtitle );
  TTableViewCellEditingStyle = ( tvesNone, tvesDelete, tvesInsert, tvesCheckBox );
  TTableViewCellAccessory    = ( tvcaNone, tvcaDisclosureIndicator, tvcaDetailDisclosureButton, tvcaCheckmark );
  TDPFTableHeaderFooterKind  = ( kStandard, kCustom );

  // ----------------------------------------------------------------------------
  TDPFTableItemHeaderFooterSetting = class( TPersistent )
  private
    FTextColor      : TAlphaColor;
    FFont           : TDPFFont;
    FMargins        : TBounds;
    FKind           : TDPFTableHeaderFooterKind;
    FAlpha          : Single;
    FTextAlign      : TDPFTextAlignment;
    FText           : string;
    FBackgroundColor: TAlphaColor;
    FHeight         : Integer;
    FBackgroundImage: string;
    FPadding        : TBounds;
    procedure SetMargins( const Value: TBounds );
    procedure SetPadding( const Value: TBounds );
    procedure SetFont(const Value: TDPFFont);
  public
    constructor Create( AOwner: TSectionItem );
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Kind           : TDPFTableHeaderFooterKind read FKind write FKind default kStandard;
    property Text           : string read FText write FText;
    property TextAlign      : TDPFTextAlignment read FTextAlign write FTextAlign default TDPFTextAlignment.taLeft;
    property TextColor      : TAlphaColor read FTextColor write FTextColor default TAlphaColors.White;
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.Gray;
    property BackgroundImage: string read FBackgroundImage write FBackgroundImage;
    property Font           : TDPFFont read FFont write SetFont;
    property Height         : Integer read FHeight write FHeight default ( -1 );
    property Margins        : TBounds read FMargins write SetMargins;
    property Padding        : TBounds read FPadding write SetPadding;
    property Alpha          : Single read FAlpha write FAlpha;
  end;

  // ----------------------------------------------------------------------------
  TTableItem = class;

  TDPFTableItemText = class( TPersistent )
  private
    FParent          : TTableItem;
    FHighlightedColor: TAlphaColor;
    FColor           : TAlphaColor;
    FNumberOfLines   : Integer;
    FAlignment       : TDPFTextAlignment;
    FBackgroundColor : TAlphaColor;
    FText            : string;
    FFont            : TDPFFont;
    procedure SetText( const Value: string );
  public
    constructor Create( AParent : TTableItem);
    destructor Destroy; override;
    procedure Assign( Source: TPersistent ); override;
  published
    property Text            : string read FText write SetText;
    property Color           : TAlphaColor read FColor write FColor default TAlphaColors.Black;
    property BackgroundColor : TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.null;
    property HighlightedColor: TAlphaColor read FHighlightedColor write FHighlightedColor default TAlphaColors.White;
    property Alignment       : TDPFTextAlignment read FAlignment write FAlignment default taLeft;
    property NumberOfLines   : Integer read FNumberOfLines write FNumberOfLines default 1;
    // property Font            : TDPFFont read FFont write SetFont;
  end;

  // ----------------------------------------------------------------------------
  // TableView Items
  TTableItem = class( TCollectionItem )
  private
    FParent                   : TTableItemCollection;
    FStyle                    : TTableViewCellStyle;
    FHeight                   : Integer;
    FEditingAccessoryType     : TTableViewCellAccessory;
    FAccessoryType            : TTableViewCellAccessory;
    FImageName                : string;
    FEditingStyle             : TTableViewCellEditingStyle;
    FModified                 : Boolean;
    FItemText                 : TDPFTableItemText;
    FItemDescription          : TDPFTableItemText;
    FBackgroundColor          : TAlphaColor;
    FisURL                    : Boolean;
    FTagStr                   : string;
    FTag                      : Integer;
    FTagFloat                 : Double;
    FTagDateTime              : TDateTime;
    FTagInt64                 : Int64;
    FBitMap                   : TBitMap;
    FHidesAccessoryWhenEditing: Boolean;
    FCategory                 : string;
    FTagBoolean: Boolean;
    FTagStr2: string;
    procedure SetModified( const Value: Boolean );
    procedure SetItemText( const Value: TDPFTableItemText );
    procedure SetItemDescription( const Value: TDPFTableItemText );
    procedure SetBitMap( const Value: TBitMap );

  protected
    function GetDisplayName: string; override;
  public
    // CustomViews: TArray<TDPFiOSBaseControl>;
    constructor Create( ItemCollection: TCollection ); override;
    destructor Destroy; override;
    procedure Assign( Source: TPersistent ); override;

    property TagFloat: Double read FTagFloat write FTagFloat;
    property TagDateTime: TDateTime read FTagDateTime write FTagDateTime;
    property TagStr: string read FTagStr write FTagStr;
    property TagStr2: string read FTagStr2 write FTagStr2;
    property TagInt64: Int64 read FTagInt64 write FTagInt64;
    property Tag: Integer read FTag write FTag;
    property TagBoolean: Boolean read FTagBoolean write FTagBoolean;
  published
    property Modified                 : Boolean read FModified write SetModified default False;
    property Style                    : TTableViewCellStyle read FStyle write FStyle default tvcsDefault;
    property AccessoryType            : TTableViewCellAccessory read FAccessoryType write FAccessoryType default tvcaNone;
    property HidesAccessoryWhenEditing: Boolean read FHidesAccessoryWhenEditing write FHidesAccessoryWhenEditing default true;
    property EditingAccessoryType     : TTableViewCellAccessory read FEditingAccessoryType write FEditingAccessoryType default tvcaNone;
    property EditingStyle             : TTableViewCellEditingStyle read FEditingStyle write FEditingStyle default tvesDelete;
    property Height                   : Integer read FHeight write FHeight default ( -1 );
    property BitMap                   : TBitMap read FBitMap write SetBitMap;
    property ImageName                : string read FImageName write FImageName;
    property isURL                    : Boolean read FisURL write FisURL default false;
    property Category                 : string read FCategory write FCategory;
    property ItemText                 : TDPFTableItemText read FItemText write SetItemText;
    property ItemDescription          : TDPFTableItemText read FItemDescription write SetItemDescription;
    property BackgroundColor          : TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.White;

  end;

  // ----------------------------------------------------------------------------
  TTableItemCollection = class( TCollection )
  private
    FParent: TSectionItem;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TTableItem;
    procedure SetItem( Index: Integer; Value: TTableItem );

  public
    constructor Create( AOwner: TSectionItem );
    destructor Destroy; override;

    function Add: TTableItem;
    function Insert( Index: Integer ): TTableItem;

    property Items[index: Integer]: TTableItem read GetItem write SetItem; default;
  end;

  // ----------------------------------------------------------------------------
  // TableView Sections
  TSectionItem = class( TCollectionItem )
  private
    FParent    : TSectionCollection;
    FTableItems: TTableItemCollection;
    FHeader    : TDPFTableItemHeaderFooterSetting;
    FFooter    : TDPFTableItemHeaderFooterSetting;

    procedure SetTableItems( const Value: TTableItemCollection );
    procedure SetFooter( const Value: TDPFTableItemHeaderFooterSetting );
    procedure SetHeader( const Value: TDPFTableItemHeaderFooterSetting );
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Header    : TDPFTableItemHeaderFooterSetting read FHeader write SetHeader;
    property Footer    : TDPFTableItemHeaderFooterSetting read FFooter write SetFooter;
    property TableItems: TTableItemCollection read FTableItems write SetTableItems;
  end;

  // ----------------------------------------------------------------------------
  TSectionCollection = class( TCollection )
  private
    FParent: TDPFiOSBaseControl;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TSectionItem;
    procedure SetItem( Index: Integer; Value: TSectionItem );

    procedure Notify( Item: TCollectionItem; Action: TCollectionNotification ); override;
  public
    constructor Create( AOwner: TDPFiOSBaseControl );
    destructor Destroy; override;

    function Add: TSectionItem;
    function Insert( Index: Integer ): TSectionItem;

    property Items[index: Integer]: TSectionItem read GetItem write SetItem; default;
  end;

implementation

uses DPF.iOS.UITableView, DPF.iOS.Common;

// ------------------------------------------------------------------------------
{ TSectionItem }
procedure TSectionItem.Assign(Source: TPersistent);
  procedure AssignFromSectionItem(Src: TSectionItem);
  begin
    FHeader.Assign(Src.FHeader);
    FFooter.Assign(Src.FFooter);
    FTableItems.Assign(Src.FTableItems);
  end;
begin
  if Source is TSectionItem then
    AssignFromSectionItem(TSectionItem(Source))
  else
    inherited;
end;

constructor TSectionItem.Create( AOwner: TCollection );
begin
  FTableItems := TTableItemCollection.Create( Self );
  FHeader     := TDPFTableItemHeaderFooterSetting.Create( Self );
  FFooter     := TDPFTableItemHeaderFooterSetting.Create( Self );
  FParent := AOwner as TSectionCollection;

  inherited Create( AOwner );
end;

// ------------------------------------------------------------------------------
destructor TSectionItem.Destroy;
begin
  FParent := nil;
  DisposeOfAndNil(FTableItems);
  DisposeOfAndNil(FHeader);
  DisposeOfAndNil(FFooter);
  inherited;
end;

// ------------------------------------------------------------------------------
function TSectionItem.GetDisplayName: string;
begin
  Result := Format( 'Section %d', [index] );
end;

// ------------------------------------------------------------------------------
procedure TSectionItem.SetFooter( const Value: TDPFTableItemHeaderFooterSetting );
begin
  FFooter.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TSectionItem.SetHeader( const Value: TDPFTableItemHeaderFooterSetting );
begin
  FHeader.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TSectionItem.SetTableItems( const Value: TTableItemCollection );
begin
  FTableItems.Assign( Value );
end;

// ------------------------------------------------------------------------------
{ TSectionCollection }
function TSectionCollection.Add: TSectionItem;
begin
  Result := inherited Add as TSectionItem;
end;

// ------------------------------------------------------------------------------
constructor TSectionCollection.Create( AOwner: TDPFiOSBaseControl );
begin
  inherited Create( TSectionItem );
  FParent := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TSectionCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TSectionCollection.GetItem( Index: Integer ): TSectionItem;
var
  I, Cnt: INteger;
begin
  if TDPFUITableView( FParent ).VirtualMode then
    if index > Count - 1 then
    begin
      Cnt   := Count;
      for I := Cnt to index do
        Add;
    end;

  Result := inherited Items[index] as TSectionItem;
end;

// ------------------------------------------------------------------------------
function TSectionCollection.GetOwner: TPersistent;
begin
  Result := FParent;
end;

// ------------------------------------------------------------------------------
function TSectionCollection.Insert( Index: Integer ): TSectionItem;
begin
  Result := inherited insert( index ) as TSectionItem;
end;

// ------------------------------------------------------------------------------
procedure TSectionCollection.Notify( Item: TCollectionItem; Action: TCollectionNotification );
begin
  inherited;
//  if ( Action = cnAdded ) and not Assigned( TSectionItem( Item ).FTableItems ) then //SZ: no, must be done in constructor - otherwise the items will be overwritten
//    TSectionItem( Item ).FTableItems := TTableItemCollection.Create( TSectionItem( Item ) );
end;

// ------------------------------------------------------------------------------
procedure TSectionCollection.SetItem( Index: Integer; Value: TSectionItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
{ TTableItem }
constructor TTableItem.Create( ItemCollection: TCollection );
begin
  inherited Create( ItemCollection );
  FParent := ItemCollection as TTableItemCollection;

  FCategory                  := '';
  FHidesAccessoryWhenEditing := true;
  FItemText                  := TDPFTableItemText.Create( Self );
  FItemDescription           := TDPFTableItemText.Create( Self );
  FBitMap                    := TBitmap.Create( 0, 0 );
  isURL                      := False;
  FItemDescription.Color     := TAlphaColors.Gray;
  FBackgroundColor           := TAlphaColors.White;
  FModified                  := False;
  FStyle                     := tvcsDefault;
  FAccessoryType             := tvcaNone;
  FEditingAccessoryType      := tvcaNone;
  FEditingStyle              := tvesDelete;
  FHeight                    := -1;
end;

// ------------------------------------------------------------------------------
destructor TTableItem.Destroy;
begin
  DisposeOfAndNil(FItemText);
  DisposeOfAndNil(FItemDescription);
  DisposeOfAndNil(FBitMap);
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TTableItem.Assign( Source: TPersistent );
  procedure AssignFromTableItem(Src: TTableItem);
  begin
    FModified             := false;
    FStyle                := Src.FStyle;
    FAccessoryType        := Src.FAccessoryType;
    FEditingAccessoryType := Src.FEditingAccessoryType;
    FEditingStyle         := Src.FEditingStyle;
    FHeight               := Src.FHeight;
    FImageName            := Src.FImageName;
    FisURL                := Src.FisURL;
    FBackgroundColor      := Src.FBackgroundColor;

    FBitMap.Assign(Src.FBitMap );
    FItemText.Assign(Src.FItemText );
    FItemDescription.Assign(Src.FItemDescription );
  end;
begin
  if Source is TTableItem then
    AssignFromTableItem(TTableItem(Source))
  else
    inherited;
end;

// ------------------------------------------------------------------------------
function TTableItem.GetDisplayName: string;
begin
  Result := Format( 'Section Item %d', [index] );
end;

// ------------------------------------------------------------------------------
procedure TTableItem.SetModified( const Value: Boolean );
begin
  if not( csLoading in FParent.FParent.FParent.FParent.ComponentState ) and not( csDesigning in FParent.FParent.FParent.FParent.ComponentState ) then
    FModified := Value;
end;

// ------------------------------------------------------------------------------
procedure TTableItem.SetBitMap( const Value: TBitMap );
begin
  FBitMap.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TTableItem.SetItemDescription( const Value: TDPFTableItemText );
begin
  FItemDescription.Assign( Value );
end;

procedure TTableItem.SetItemText( const Value: TDPFTableItemText );
begin
  FItemText.Assign( Value );
end;

// ------------------------------------------------------------------------------
{ TTableItemCollection }
function TTableItemCollection.Add: TTableItem;
begin
  Result := inherited Add as TTableItem;
end;

// ------------------------------------------------------------------------------
constructor TTableItemCollection.Create( AOwner: TSectionItem );
begin
  inherited Create( TTableItem );
  FParent := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TTableItemCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TTableItemCollection.GetItem( Index: Integer ): TTableItem;
var
  I, Cnt: INteger;
begin
  if (FParent.FParent.FParent as TDPFUITableView).VirtualMode then
    if index > Count - 1 then
    begin
      Cnt   := Count;
      for I := Cnt to index do
        Add;
    end;

  Result := inherited Items[index] as TTableItem;
end;

// ------------------------------------------------------------------------------
function TTableItemCollection.GetOwner: TPersistent;
begin
  Result := FParent;
end;

// ------------------------------------------------------------------------------
function TTableItemCollection.Insert( Index: Integer ): TTableItem;
begin
  Result := inherited insert( index ) as TTableItem;
end;

// ------------------------------------------------------------------------------
procedure TTableItemCollection.SetItem( Index: Integer; Value: TTableItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
{ TDPFTableItemText }

constructor TDPFTableItemText.Create( AParent: TTableItem);
begin
  inherited Create;
  FParent := AParent;
  // FFont             := TDPFFont.Create;
  // FFont.FontSize    := 16;
  FColor            := TAlphaColors.Black;
  FHighlightedColor := TAlphaColors.White;
  FBackgroundColor  := TAlphaColors.null;
  FNumberOfLines    := 1;
  FAlignment        := taLeft;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableItemText.Destroy;
begin
  if Assigned( FFont ) then
    FFont.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableItemText.Assign( Source: TPersistent );
  procedure AssignFromDPFTableItemText(Src: TDPFTableItemText);
  begin
    FText             := Src.FText;
    FColor            := Src.FColor;
    FBackgroundColor  := Src.FBackgroundColor;
    FHighlightedColor := Src.FHighlightedColor;
    FAlignment        := Src.FAlignment;
    FNumberOfLines    := Src.FNumberOfLines;
  end;
begin
  if Source is TDPFTableItemText then
    AssignFromDPFTableItemText(TDPFTableItemText(Source))
  else
    inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableItemText.SetText( const Value: string );
begin
  FText := Value;
//{$IFDEF IOS} //SZ: why $IFDEF IOS?
  (FParent as TTableItem).FModified := True;
//{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFTableItemHeaderFooterSetting }

procedure TDPFTableItemHeaderFooterSetting.Assign(Source: TPersistent);
  procedure AssignFromTableItemHeaderFooterSetting(Src: TDPFTableItemHeaderFooterSetting);
  begin
    Kind := Src.Kind;
    Text := Src.Text;
    TextAlign := Src.TextAlign;
    TextColor := Src.TextColor;
    BackgroundColor := Src.BackgroundColor;
    BackgroundImage := Src.BackgroundImage;
    Font := Src.Font;
    Height := Src.Height;
    Margins := Src.Margins;
    Padding := Src.Padding;
    Alpha := Src.Alpha;
  end;
begin
  if Source is TDPFTableItemHeaderFooterSetting then
    AssignFromTableItemHeaderFooterSetting(TDPFTableItemHeaderFooterSetting(Source))
  else
    inherited;
end;

constructor TDPFTableItemHeaderFooterSetting.Create( AOwner: TSectionItem );
begin
  inherited Create;
  FHeight          := -1;
  FBackgroundColor := TAlphaColors.Gray;
  FTextColor       := TAlphaColors.White;
  FFont            := TDPFFont.create;
  FFont.FontName   := ios_Helvetica_Bold;
  FFont.FontSize   := 18;
  FTextAlign       := TDPFTextAlignment.taLeft;
  FMargins         := TBounds.Create( RectF( 0, 0, 0, 0 ) );
  FPadding         := TBounds.Create( RectF( 10, 0, 0, 0 ) );
  FKind            := kStandard;
  FAlpha           := 0.9;
end;

// ------------------------------------------------------------------------------
destructor TDPFTableItemHeaderFooterSetting.Destroy;
begin
  FMargins.DisposeOf;
  FPadding.DisposeOf;
  FFont.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTableItemHeaderFooterSetting.SetFont(const Value: TDPFFont);
begin
  FFont.Assign(Value);
end;

procedure TDPFTableItemHeaderFooterSetting.SetMargins( const Value: TBounds );
begin
  FMargins.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTableItemHeaderFooterSetting.SetPadding( const Value: TBounds );
begin
  FPadding.Assign( Value );
end;

// ------------------------------------------------------------------------------
end.
