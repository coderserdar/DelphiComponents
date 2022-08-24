{------------------------------------------------------------------------------
  DecoCompareGrid.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoCompareGrid component implements an easy to use comparison
              table based on the list of criterias and the list of alternatives.
              TDecoCompareGrid is lightweight and easy to setup, so you don't
              have to use some universal (and resource wasteful) grid components
              for displaying this kind of charts/tables.

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2013  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.3 (2013-11-18)
  - added: Support for Delphi XE3/XE4/XE5
  - improved: Minor improvements and fixes

  Version 1.0 (2012-03-11)
  - The first release
}

{ This unit contains TCustomDecoCompareGrid and TDecoCompareGrid components declaration and implementation. }
unit DecoCompareGrid;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, SysUtils,
  DecoCommon, DecoChart, ImgList
  {$IF CompilerVersion >= 24}, System.UITypes{$IFEND}
  ;

type
  { Visual style of criteria value. Visual style affects also other handling with the given value, such as retrieving its text form. }
  TDecoCriteriumStyle = (cgsText, cgsNumber, cgsCheck, cgsProgressBar, cgsRating, cgsCustom);
  { Defines layout of item image inside the item cell. }
  TComparedItemImageLayout = (iilTop, iilBottom, iilLeft, iilRight);

  { Holds the value of an item for criterium. Index of value in TCustomDecoCompareGrid.Items.Values collection have to correspond to the index
    of criterium in TCustomDecoCompareGrid.Criterias collection. }
  TDecoCompareGridValue = class(TDecoChartItem)
  private
    FValue:string;
    procedure SetValue(const aValue:string);
    function GetAsInteger:Integer;
    procedure SetAsInteger(aValue:integer);
    function GetAsExtended:Extended;
    procedure SetAsExtended(aValue:Extended);
    function GetAsBoolean:Boolean;
    procedure SetAsBoolean(aValue:Boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetAsVariant(aValue:Variant);
    property AsString:string read FValue write SetValue;
    property AsInteger:Integer read GetAsInteger write SetAsInteger;
    property AsExtended:Extended read GetAsExtended write SetAsExtended;
    property AsBoolean:Boolean read GetAsBoolean write SetAsBoolean;
  published
    { See @inherited }
    property Hint;
    { See @inherited }
    property Visible;
    { Current value is stored internally as a string. You can also use other public properties or functions (like AsExtended, AsInteger, etc.)
      to access the value. }
    property Value:string read FValue write SetValue;
  end;

  { Defines one criterium for comparison table. }
  TDecoCompareGridCriterium = class(TDecoChartItem)
  private
    FCategory:string;
    FStyle:TDecoCriteriumStyle;
    FTextFormat: string;
    FMinValue: Extended;
    FMaxValue: Extended;
    // calculated rectangle of category label (if the criterium is first in its category)
    FCatRect:TRect;
    procedure SetCategory(const Value: string);
    procedure SetStyle(const Value: TDecoCriteriumStyle);
    procedure SetTextFormat(const Value:string);
    procedure SetMinValue(NewValue: Extended); virtual;
    procedure SetMaxValue(NewValue: Extended); virtual;
  public
    constructor Create(Collection: TCollection); override;
    { Assigns the properties to another criterium. }
    procedure Assign(Source: TPersistent); override;
    // Indicates that the criterium is listed as first in its category (make sense only if TCustomDecoCompareGrid.ShowCategories is True)
    function IsFirstInCategory:Boolean;
    // Calculated drawing rectangle of category (if the criterium is first in its category)
    property CategoryRect:TRect read FCatRect;
  published
    { See @inherited }
    property Caption;
    { See @inherited }
    property Hint;
    { Criteria category. If TCustomDecoCompareGrid.ShowCategories is True, the criterias will be grouped visually under the given category.
      Criteria sorting is not performed automatically, so you must either implement your own OnCompareItems event (and call Sort method manually),
      or simply fill the collection with criterias pre-sorted by category. }
    property Category:string read FCategory write SetCategory;
    { See TDecoCriteriumStyle. }
    property Style:TDecoCriteriumStyle read FStyle write SetStyle default cgsText;
    { This property defines a format string for displaying the values.
      Any formatting options for standard Delphi Format function, that are dedicated to the given criterium style, are allowed.
      So, for example the '%.2n' format string results to displaying the value as '100,000.00' (if style is cgsNumber). }
    property TextFormat: string read FTextFormat write SetTextFormat;
    { A minimum value of the range for the progress bar criterium style. }
    property MinValue: Extended read FMinValue write SetMinValue;
    { A maximum value of the range for the progress bar criterium style. }
    property MaxValue: Extended read FMaxValue write SetMaxValue;
  end;

  { Defines the item to compare (ie. product, application, commodity, car, etc.).
   Each item has Values property, that holds item criteria values (see TDecoCompareGridValue). }
  TDecoCompareGridItem = class(TDecoChartItem)
  private
    FImageIndex: {$IF CompilerVersion >= 24} System.UITypes.{$IFEND}TImageIndex;
    // Criteria values. The index of value item must correspond to criteria index in the grid
    FValues:TDecoChartItems;
    function IsValuesStored: Boolean;
    procedure SetValues(const Value: TDecoChartItems);
  protected
    procedure SetImageIndex(Value: {$IF CompilerVersion >= 24} System.UITypes.{$IFEND}TImageIndex);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { See @inherited }
    property Caption;
    { See @inherited }
    property Hint;
    { See @inherited }
    property Visible;
    { Image index of compared item. Images can be used to present product screenshots, boxes, icons, etc.
      See also TCustomDecoCompareGrid.Images. }
    property ImageIndex: {$IF CompilerVersion >= 24} System.UITypes.{$IFEND}TImageIndex read FImageIndex write SetImageIndex default -1;
    // Criteria values. The index of value item must correspond to criteria index in the grid
    property Values: TDecoChartItems read FValues write SetValues stored IsValuesStored;
  end;

  // Type definition of an OnDrawCell event handler
  TDecoCompareGridDrawCell = procedure(Sender:TObject; TargetCanvas:TCanvas; Item:TDecoCompareGridItem;
      Criterium: TDecoCompareGridCriterium; Value:TDecoCompareGridValue; CellRect:TRect; IsSpecial:Boolean; var Text:string) of object;

  { A base class for all DecoCompareGrid visual component implementations. See also TDecoCompareGridPlus. }
  TCustomDecoCompareGrid = class(TCustomDecoChart)
  private
    // List of criterias
    FCriterias:TSortedDecoChartItems;
    // List of compared items (each item has a Values property, that holds item criteria values)
    FItems:TSortedDecoChartItems;
    FImagesChangeLink: TChangeLink;
    FCheckImagesChangeLink: TChangeLink;
    FRatingImagesChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FCheckImages: TCustomImageList;
    FRatingImages: TCustomImageList;
    FItemTextLayout: TVerticalAlignment;
    FItemImageLayout: TComparedItemImageLayout;
    FCriteriaAlignment: TAlignment;
    FCategoryAlignment: TAlignment;
    FGridLineStyle: TPenStyle;
    FColWidth: Integer;
    FRowHeight: Integer;
    FCategoryRowHeight: Integer;
    FCategoryIndent: Integer;
    FFixedColWidth: Integer;
    FFixedRowHeight: Integer;
    FFixedColor: TColor;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FGridLineColor: TColor;
    FGridLineWidth: Integer;
    FCategoryColor: TColor;
    FShowVertGrid: Boolean;
    FShowHorzGrid: Boolean;
    FHeadGridLine: Boolean;
    FStripeCols: Boolean;
    FStripeRows: Boolean;
    FFrameRoundCorners: Integer;
    FHoverStyle: TFontStyles;
    FItemFont:TFont;
    FCriteriaFont:TFont;

    FProgBarBackColor: TColor;
    FProgBarFrameColor: TColor;
    FProgBarFrameProgress: Boolean;
    FProgBarGradient: TPGGradientStyle;
    FProgBarLevel1Color: TColor;
    FProgBarLevel1TextColor: TColor;
    FProgBarLevel2Color: TColor;
    FProgBarLevel2Percent: Integer;
    FProgBarLevel2TextColor: TColor;
    FProgBarLevel3Color: TColor;
    FProgBarLevel3Percent: Integer;
    FProgBarLevel3TextColor: TColor;
    FProgBarRoundCorners: Integer;
    FProgBarRounded: Boolean;
    FProgBarHeight: Integer;
    FProgBarShowValueInBar: TShowValueInBar;
    FProgBarShowOverMaxIndicator: Boolean;

    FShowGuideHeader:Boolean;
    FShowGuideFooter:Boolean;
    FShowCategories:Boolean;

    // Calculated frame rectangle (comparison table dimensions). See also GridRect property.
    FFrameRect:TRect;
    // Calculated guide header rectangle.
    FGuideHeader:TRect;
    // Calculated guide footer rectangle.
    FGuideFooter:TRect;
    // Calculated rectangle of the chart caption (top left fixed cell)
    FCaptionRect:TRect;
    FHoverCell: TDecoChartItem;
    FSelectedCell: TDecoChartItem;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FOnDrawCell: TDecoCompareGridDrawCell;
    FOnHoverChange: TDecoChartItemEvent;
    FOnCellClick: TDecoChartItemEvent;
    FOnSelectCell: TDecoChartItemEvent;
    procedure CMParentFontChanged(var Message: {$IF CompilerVersion >= 20.0} TCMParentFontChanged {$ELSE} TMessage {$IFEND}); message CM_PARENTFONTCHANGED;
    function IsCriteriasStored: Boolean;
    procedure SetCriterias(const Value: TSortedDecoChartItems);
    procedure ImageListChange(Sender: TObject);
    function IsItemsStored: Boolean;
    procedure SetItems(const Value: TSortedDecoChartItems);
    procedure SetImages(Value: TCustomImageList);
    procedure SetCheckImages(Value: TCustomImageList);
    procedure SetRatingImages(Value: TCustomImageList);
    function IsOnCompareStored: Boolean;
    function GetOnCompareItems:TDecoChartCompareEvent;
    procedure SetOnCompareItems(Value:TDecoChartCompareEvent);
    procedure SetColorVal(Index: Integer; Value: TColor);
    procedure SetIntVal(Index: Integer; Value: Integer);
    procedure SetBoolVal(Index: Integer; Value: Boolean);
    procedure SetHoverCell(const Value:TDecoChartItem);
    procedure SetSelectedCell(const Value:TDecoChartItem);
    procedure SetItemFont(const Value: TFont);
    procedure SetCriteriaFont(const Value: TFont);
    procedure SetOnDrawCell(Value: TDecoCompareGridDrawCell);
    procedure SetItemTextLayout(Value: TVerticalAlignment);
    procedure SetItemImageLayout(Value: TComparedItemImageLayout);
    procedure SetGridLineStyle(Value: TPenStyle);
    procedure SetHoverStyle(Value: TFontStyles);
    procedure SetCriteriaAlignment(Value: TAlignment);
    procedure SetCategoryAlignment(Value: TAlignment);
    procedure SetProgBarGradient(Value: TPGGradientStyle);
    procedure SetProgBarShowValueInBar(Value: TShowValueInBar);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Returns True if the comparison table is empty (ie. there is no item or there is no criterium defined).
    function IsEmpty:Boolean; override;
    // Returns True if any grid cell is selected (this can be an item cell, criteria cell or value cell).
    function IsAnyItemSelected:Boolean; override;
    { Returns True if the given item is selected (here the item means compared item, criterium or value).
      Implemented mainly to support common processing in ancestor (TCustomDecoChart). }
    function IsItemSelected(const Item:TCollectionItem):Boolean; override;
    // Returns the rectangle where to draw focus (ie. selected cell rectangle)
    function GetFocusRect:TRect; override;
    // Selects first available value cell in the comparison table (ie. cell with 0,0 coordinates).
    procedure SelectFirstItem; override;
    // This method canels current selection (if any)
    procedure CancelSelection; override;
    // Indicates if any item (criterium, compared item, or value cell) is hovered
    function IsAnyItemHovered:Boolean; override;
    { Returns True if the given item (criterium, compared item, or value cell) is hovered.
      Implemented mainly to support common processing in ancestor (TCustomDecoChart). }
    function IsItemHovered(const Item:TCollectionItem):Boolean; override;
    // Cancels hover flag (HoverItem, HoverCursor)
    procedure CancelHover; override;
    { Returns Hint and ItemRect for hovered item (criterium, compared item, or value cell).
      Implemented mainly to support common processing in ancestor (TCustomDecoChart). }
    procedure GetHoverHintInfo(var aHint:string; var aRect:TRect); override;
    // Method for setting the current HoverItem based on passed X and Y coordinates.
    procedure UpdateHoverInfo(X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Handling of the general keyboard shortcuts available for this component. }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    // Main drawing method - paints the whole comparison table (criterias, items, values, frame, grid, etc.)
    procedure DrawChart(TargetCanvas:TCanvas); override;
    // Calculated rectangle of the chart caption (top left fixed cell)
    property CaptionRect:TRect read FCaptionRect;
    property GuideHeader:TRect read FGuideHeader;
    property GuideFooter:TRect read FGuideFooter;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    // Returns the cell at X,Y (client rectangle coordinates). Cell means criterium, compared item, or value cell.
    function GetCellAt(X,Y:Integer):TDecoChartItem;
    // Returns the value cell at Col and Row grid coordinates.
    function GetCell(Col,Row:Integer):TDecoChartItem; overload;
    // Returns the value cell for given Item and Criterium.
    function GetCell(const Item:TDecoCompareGridItem; const Criterium:TDecoCompareGridCriterium):TDecoChartItem; overload;
    // Fills the Item, Criterium and Value arguments with an appropriate data for the cell at X and Y client coordinates.
    function GetHitTestInfoAt(X,Y:Integer;var Item:TDecoCompareGridItem;
        var Criterium:TDecoCompareGridCriterium; var Value:TDecoCompareGridValue):Boolean; overload;
    // Fills the Item, Criterium and Value arguments with an appropriate data for the given cell. Cell can be criterium, item or value cell.
    function GetHitTestInfoAt(Cell:TDecoChartItem; var Item:TDecoCompareGridItem;
        var Criterium:TDecoCompareGridCriterium; var Value:TDecoCompareGridValue):Boolean; overload;
    // Returns the compared item, that is associated with passed cell (usually a value cell).
    function GetCellItem(Cell:TDecoChartItem):TDecoCompareGridItem;
    { Returns a formatted Value, based on the given criteria TextFormat property. }
    function GetValueFormatted(const aCriterium:TDecoCompareGridCriterium; aValue:TDecoCompareGridValue):string;
    { Sorts Items collection. See also OnCompareItems event. }
    procedure Sort; override;
    // Sets the drawing offset of the chart. By setting the offset, you can control displaying of huge comparison charts.
    procedure SetOffsetXY(x,y:Integer);
    // Makes the given item (criterium cell, item cell, value cell) visible in current client rectangle area.
    procedure MakeVisible(Item:TDecoChartItem);
    { Adds a new criterium to the chart. }
    function AddCriterium(const Caption:string; const Category:string = ''; const Hint:string = '';
        Style:TDecoCriteriumStyle = cgsText; const TextFormat:string = ''; MinValue:Extended = 0; MaxValue:Extended = 100):TDecoCompareGridCriterium;
    { Adds a new item to the chart. }
    function AddItem(const Caption:string; ImageIndex:Integer = -1; const Hint:string = ''):TDecoCompareGridItem;
    { Adds a new criteria value for the given item. Value is added at the end of item's values collection.
      This collection must corresponds to the criterias collection of the chart (ie. Values[0] belongs to Criterias[0], Values[1] belongs to Criterias[1], etc.). }
    function AddValue(const Item:TDecoCompareGridItem; const Value:string):TDecoCompareGridValue; overload;
    { Adds new criteria values for the given item. Values are added at the end of item's values collection.
      This collection must corresponds to the criterias collection of the chart. Function returns last added value object. }
    function AddValue(const Item:TDecoCompareGridItem; const Values:array of Variant):TDecoCompareGridValue; overload;
    { Sets the value of given criterium for the given item. }
    procedure SetValue(const Item:TDecoCompareGridItem; Criterium:TDecoCompareGridCriterium; const Value:string); overload;
    { Sets the value of given criterium for the given item. }
    procedure SetValue(const Item:TDecoCompareGridItem; Criterium:TDecoCompareGridCriterium; const Value:Variant); overload;
    { Sets values for the given item. Values are assigned to the criterias from the first to last. So you can pass less or more values,
      but they are always set starting from the first criterium. }
    procedure SetValue(const Item:TDecoCompareGridItem; const Values:array of Variant); overload;
    // Blocks "rebuilding" and repainting of the component until the corresponding EndUpdate is called.
    procedure BeginUpdate; override;
    // Re-enables repainting of the chart previously blocked by the BeginUpdate call.
    procedure EndUpdate; override;
    { This method calculates the drawing rectangles (and other aspects) of each displayed element and stores the values to the temporary variables (a cache).
      The repainting of the component itself is then faster. This method is called when the values are modified, on resizing the component, etc. }
    procedure Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil); override;
    // Identifies the item (criteria, compared item, value cell) under the mouse cursor.
    property HoverCell: TDecoChartItem read FHoverCell write SetHoverCell;
    // Identifies the selected item (criteria, compared item, value cell), in other words - an item that has a focus.
    property SelectedCell: TDecoChartItem read FSelectedCell write SetSelectedCell;
    { See @inherited }
    property Spacing:Integer read FSpacing;
    // Current chart rectangle (comparison table dimensions). The rectangle could be shifted by OffsetX and OffsetY properties.
    property GridRect:TRect read FFrameRect;
    // Sets the X drawing offset of the chart. By setting the offset, you can control displaying of huge comparison charts. See also SetOffsetXY method.
    property OffsetX:Integer Index 13 read FOffsetX write SetIntVal default 0;
    // Sets the Y drawing offset of the chart. By setting the offset, you can control displaying of huge comparison charts. See also SetOffsetXY method.
    property OffsetY:Integer Index 14 read FOffsetY write SetIntVal default 0;
  published
    { The list of criteria by which to compare individual items. }
    property Criterias: TSortedDecoChartItems read FCriterias write SetCriterias stored IsCriteriasStored;
    { Collection of items to compare. }
    property Items: TSortedDecoChartItems read FItems write SetItems stored IsItemsStored;
    { ImageList of pictures or icons that belongs to individual compared items. See TDecoCompareGridItem.ImageIndex. }
    property Images: TCustomImageList read FImages write SetImages;
    { ImageList that represents "checked" / "unchecked" states of criteria values with TDecoCompareGridCriterium.Style = cgsCheck.
      ImageIndex 0 corresponds to "unchecked" and ImageIndex 1 to "checked" state. }
    property CheckImages: TCustomImageList read FCheckImages write SetCheckImages;
    { ImageList that represents rating stars of criteria values with TDecoCompareGridCriterium.Style = cgsRating.
      ImageIndex 0 corresponds to "full" star, 1 to "half" star and 2 to "empty" star. You can ommit "half" star image,
      so the image list can contain only two images (for full and empty stars). }
    property RatingImages: TCustomImageList read FRatingImages write SetRatingImages;
    { Width of the grid columns, except the first (fixed) column. If it is lower than or equal to 0, the width is calculated automatically,
      to fit all the columns inside the current client rectangle of the component. }
    property ColWidth:Integer Index 4 read FColWidth write SetIntVal default 0;
    { Height of the grid rows, except the first (fixed) row. If it is lower than or equal to 0, the height is calculated automatically,
      based on the Font or CriteriaFont properties. }
    property RowHeight:Integer Index 5 read FRowHeight write SetIntVal default 0;
    { Height of the category rows, ie. rows that represents a group of criterias. See also ShowCategories and TDecoCompareGridCriteria.Category. }
    property CategoryRowHeight:Integer Index 11 read FCategoryRowHeight write SetIntVal default 0;
    { Indentation of criteria labels (in the first grid column) when the criterias are grouped into categories.
      See also ShowCategories and TDecoCompareGridCriteria.Category. }
    property CategoryIndent:Integer Index 12 read FCategoryIndent write SetIntVal default 16;
    { Width of the first grid column (fixed column). If it is lower than or equal to 0, the width is calculated automatically,
      based on the CriteriaFont property. }
    property FixedColWidth:Integer Index 2 read FFixedColWidth write SetIntVal default 0;
    { Height of the first grid row (fixed row). If it is lower than or equal to 0, the height is calculated automatically,
      based on the ItemFont, Images.Height and ItemImageLayout properties. }
    property FixedRowHeight:Integer Index 3 read FFixedRowHeight write SetIntVal default 0;
    { Background color of the fixed grid areas (first row, first column). }
    property FixedColor: TColor Index 2 read FFixedColor write SetColorVal default clNone;
    // Frame color of the chart. You can set the value to clNone, to remove the frame. See also FrameWidth.
    property FrameColor: TColor index 0 read FFrameColor write SetColorVal default clBtnShadow;
    // A width of the chart frame. See also FrameColor.
    property FrameWidth: Integer Index 0 read FFrameWidth write SetIntVal default 1;
    { Setup this property to non-zero, if you want to draw a rounded frame around the chart. }
    property FrameRoundCorners: Integer Index 6 read FFrameRoundCorners write SetIntVal default 0;
    // Grid line color. You can set the value to clNone, to hide the lines.
    property GridLineColor: TColor index 1 read FGridLineColor write SetColorVal default clBtnShadow;
    // A width of grid lines. See also GridLineColor.
    property GridLineWidth: Integer index 1 read FGridLineWidth write SetIntVal default 1;
    // A style of grid lines. See also GridLineColor, GridLineWidth, ShowHorzGrid and ShowVertGrid properties.
    property GridLineStyle: TPenStyle read FGridLineStyle write SetGridLineStyle default psSolid;
    // Shows or hides the vertical grid lines.
    property ShowVertGrid: Boolean Index 0 read FShowVertGrid write SetBoolVal default True;
    // Shows or hides the horizontal grid lines.
    property ShowHorzGrid: Boolean Index 1 read FShowHorzGrid write SetBoolVal default True;
    { If this property is True, then the first grid line (horizontal and/or vertical) will be painted with frame properties
      instead of grid line properties. }
    property HeadGridLine: Boolean Index 2 read FHeadGridLine write SetBoolVal default False;
    { If this property is True, then columns will be painted as striped. }
    property StripeColumns: Boolean Index 9 read FStripeCols write SetBoolVal default False;
    { If this property is True, then rows will be painted as striped. }
    property StripeRows: Boolean Index 10 read FStripeRows write SetBoolVal default False;
    { A font style for hovered cells. }
    property HoverStyle: TFontStyles read FHoverStyle write SetHoverStyle default [fsUnderline];
    { Text alignment of criteria labels. }
    property CriteriaAlignment: TAlignment read FCriteriaAlignment write SetCriteriaAlignment default taLeftJustify;
    { Text alignment of category labels. Note: CaptionFont is used to draw category labels. }
    property CategoryAlignment: TAlignment read FCategoryAlignment write SetCategoryAlignment default taLeftJustify;
    // Font for item labels
    property ItemFont: TFont read FItemFont write SetItemFont;
    // Font for criteria labels Note: CaptionFont is used to draw category labels.
    property CriteriaFont: TFont read FCriteriaFont write SetCriteriaFont;
    { Text layout of item labels. The item cell consists of the label and the image (if defined). The cell is painted based
      on the layout of the label and the layout of the image. See also ItemImageLayout. }
    property ItemTextLayout: TVerticalAlignment read FItemTextLayout write SetItemTextLayout default taVerticalCenter;
    { Layout of the item image. The item cell consists of the label and the image (if defined). The cell is painted based
      on the layout of the label and the layout of the image. See also ItemImageLayout. }
    property ItemImageLayout: TComparedItemImageLayout read FItemImageLayout write SetItemImageLayout default iilBottom;
    // By this property you can set the gradient style of criteria values with cgsProgressBar style.
    property ProgBarGradient: TPGGradientStyle read FProgBarGradient write SetProgBarGradient default pggGlass;
    { This property defines a kind of text displayed inside progress bars (criteria values with cgsProgressBar style).
      It can be a current value, percentage or no text. }
    property ProgBarShowValueInBar: TShowValueInBar read FProgBarShowValueInBar write SetProgBarShowValueInBar default ibvPercent;
    // Background color of progress bars (criteria values with cgsProgressBar style). You can set the value to clNone, to make the progress bars transparent.
    property ProgBarBackColor: TColor Index 3 read FProgBarBackColor write SetColorVal default clBtnHighlight;
    // Frame color for progress bars (criteria values with cgsProgressBar style). You can set the value to clNone, to remove the frame. See also ProgBarFrameProgress.
    property ProgBarFrameColor: TColor Index 4 read FProgBarFrameColor write SetColorVal default clBtnFace;
    // If True, then progress bars (criteria values with cgsProgressBar style) with percentage greater than 0, will be painted with a frame of a slightly darker color. See also ProgBarFrameColor.
    property ProgBarFrameProgress: Boolean Index 3 read FProgBarFrameProgress write SetBoolVal default False;
    // Color of the progress bar, when the Value is lower than the ProgBarLevel2Percent limit.
    property ProgBarLevel1Color: TColor Index 5 read FProgBarLevel1Color write SetColorVal default $3437D9;
    // Color of the text inside the progress bar, when the Value is lower than the ProgBarLevel2Percent limit. See also ProgBarShowValueInBar property.
    property ProgBarLevel1TextColor: TColor Index 6 read FProgBarLevel1TextColor write SetColorVal default clBlack;
    // Color of the progress bar, when the Value is in the range from ProgBarLevel2Percent to ProgBarLevel3Percent.
    property ProgBarLevel2Color: TColor Index 7 read FProgBarLevel2Color write SetColorVal default $00BFFF;
    // A limit (in percent) for displaying the progress bar in colors defined by ProgBarLevel1Color and ProgBarLevel1TextColor properties.
    property ProgBarLevel2Percent: Integer Index 7 read FProgBarLevel2Percent write SetIntVal default 30;
    // Color of the text inside the progress bar, when the Value is in the range from ProgBarLevel2Percent to ProgBarLevel3Percent. See also ProgBarShowValueInBar property.
    property ProgBarLevel2TextColor: TColor Index 8 read FProgBarLevel2TextColor write SetColorVal default clBlack;
    // Color of the progress bar, when the Value is greater than ProgBarLevel3Percent limit.
    property ProgBarLevel3Color: TColor Index 9 read FProgBarLevel3Color write SetColorVal default $24CA8B;
    // A limit (in percent) for displaying the progress bar in colors defined by ProgBarLevel2Color and ProgBarLevel2TextColor properties.
    property ProgBarLevel3Percent: Integer Index 8 read FProgBarLevel3Percent write SetIntVal default 70;
    // Color of the text inside the progress bar, when the Value is greater than Level3Percent limit. See also ProgBarShowValueInBar property.
    property ProgBarLevel3TextColor: TColor Index 10 read FProgBarLevel3TextColor write SetColorVal default clBlack;
    { Setup this property to non-zero, if you want to use your own rounded corners factor.
      If you set the Rounded property to True and the RoundCorners property is 0, an internal rounded corners factor will be used, based on the RowHeight property.
      See also Rounded property. }
    property ProgBarRoundCorners: Integer Index 9 read FProgBarRoundCorners write SetIntVal default 0;
    // If False, the bars will be displayed as standard rectangles. If True, the bars will be displayed with rounded corners. See also RoundCorners property.
    property ProgBarRounded: Boolean Index 4 read FProgBarRounded write SetBoolVal default True;
    { A height of the progress bars (criteria values with cgsProgressBar style). In a standard grid layout (see BarPosition property) this means also the base height of the grid row.
      But be aware that the resulting height of the grid's row is calculated automatically, based on the RowHeight and BarPosition properties and on the metrics
      of currently selected font. }
    property ProgBarHeight: Integer Index 10 read FProgBarHeight write SetIntVal default 16;
    { If True, a thin dividing line will be displayed in all progress bars (criteria values with cgsProgressBar style), that have the Value greater than MaxValue, or lower than MinValue.
      This line indicates the percentage of exceeding the maximum (or minimum) value. }
    property ProgBarShowOverMaxIndicator: Boolean Index 5 read FProgBarShowOverMaxIndicator write SetBoolVal default False;
    // If True, a thin guiding header with item numbers will be drawn at the top of the chart.
    property ShowGuideHeader: Boolean Index 6 read FShowGuideHeader write SetBoolVal default False;
    // If True, a thin guiding footer with item numbers will be drawn at the top of the chart.
    property ShowGuideFooter: Boolean Index 7 read FShowGuideFooter write SetBoolVal default False;
    // Background color of category rows. Category row is displayed at the top of the grouped criterias.
    property CategoryColor: TColor Index 11 read FCategoryColor write SetColorVal default clNone;
    // Shows/hides the category rows (if any category is defined)
    property ShowCategories: Boolean Index 8 read FShowCategories write SetBoolVal default True;
    { Event handler, that allows you to perform your own sorting mechanism for compared items. See also Sort, Sorted. }
    property OnCompareItems: TDecoChartCompareEvent read GetOnCompareItems write SetOnCompareItems stored IsOnCompareStored;
    { This event handler has two purposes. First, you can use it to assign a custom texts for individual grid cells, mainly
      for cgsCustom styles of criterias. Second, you can perform your own drawing to the passed TargetCanvas for each grid cell.<br><br>
      Warning: For GDI+ descendants, you have to use a public GDIPCanvas variable to draw onto, but you can use the TargetCanvas.Handle to draw the texts.
               See the implementation of TDecoCompareGridPlus.DrawChart method as an inspiration. }
    property OnDrawCell: TDecoCompareGridDrawCell read FOnDrawCell write SetOnDrawCell;
    { This event allows you to process your own actions when an item (criterium, compared item, value cell) under the mouse is changed.
      Item parameter can have a nil value, when no item is hovered. }
    property OnHoverChange: TDecoChartItemEvent read FOnHoverChange write FOnHoverChange;
    { This event allows you to process your own actions when an item (criterium, compared item, value cell) is clicked.
      Item parameter has always a value different from nil. If the control is clicked and no item is hovered,
      only an OnClick event is fired. }
    property OnCellClick: TDecoChartItemEvent read FOnCellClick write FOnCellClick;
    { This event allows you to process your own actions when the SelectedCell property value is changed.
      Item parameter can have a nil value, when no item is selected. }
    property OnSelectCell: TDecoChartItemEvent read FOnSelectCell write FOnSelectCell;
  end;

  { TDecoCompareGrid component implements an easy to use comparison table based on the list of criterias and the list of alternatives.
    TDecoCompareGrid is lightweight and easy to setup, so you don't have to use some universal (and resource wasteful) grid components
    for displaying this kind of charts/tables. }
  TDecoCompareGrid = class(TCustomDecoCompareGrid)
  end;

implementation

uses
  DecoGDI, Variants {$IF CompilerVersion >= 24}, System.Types {$IFEND};

//////////////////////////////////////////// TDecoCompareGridValue ////////////////////////////////////////////
constructor TDecoCompareGridValue.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValue := '';
end;

procedure TDecoCompareGridValue.Assign(Source: TPersistent);
begin
  if Source is TDecoCompareGridValue then
    FValue := TDecoCompareGridValue(Source).FValue;
  inherited Assign(Source);
end;

procedure TDecoCompareGridValue.SetAsVariant(aValue:Variant);
begin
  case VarType(aValue) of
    varInt64,varLongWord,varWord,varByte,varShortInt,varSmallint,varInteger {$IF CompilerVersion >= 20.0}, varUInt64 {$IFEND}:
      AsInteger := aValue;
    varDate,varSingle,varDouble,varCurrency:
      AsExtended := aValue;
    varOleStr,varString {$IF CompilerVersion >= 20.0}, varUString {$IFEND}:
      Value := aValue;
    varBoolean:
      AsBoolean := aValue;
  else
    Value := '';
  end;
end;

procedure TDecoCompareGridValue.SetValue(const aValue:string);
begin
  if FValue <> aValue then
  begin
    FValue := aValue;
    Changed(False);
  end;
end;

function TDecoCompareGridValue.GetAsInteger:Integer;
begin
  if not TryStrToInt(FValue, Result) then
    Result := 0;
end;

procedure TDecoCompareGridValue.SetAsInteger(aValue:integer);
var
  s:string;
begin
  s:=IntToStr(aValue);
  if s<>FValue then
  begin
    FValue := s;
    Changed(False)
  end;
end;

function TDecoCompareGridValue.GetAsExtended:Extended;
begin
  if not TryStrToFloat(FValue, Result) then
    Result := 0;
end;

procedure TDecoCompareGridValue.SetAsExtended(aValue:Extended);
var
  s:string;
begin
  s:=FloatToStr(aValue);
  if s<>FValue then
  begin
    FValue := s;
    Changed(False)
  end;
end;

function TDecoCompareGridValue.GetAsBoolean:Boolean;
begin
  if not TryStrToBool(FValue, Result) then
    Result := False;
end;

procedure TDecoCompareGridValue.SetAsBoolean(aValue:Boolean);
var
  s:string;
begin
  s:=BoolToStr(aValue,True);
  if s<>FValue then
  begin
    FValue := s;
    Changed(False)
  end;
end;

//////////////////////////////////////////// TDecoCompareGridCriterium ////////////////////////////////////////////
constructor TDecoCompareGridCriterium.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStyle := cgsText;
  FCategory := '';
  FTextFormat := '';
  FMinValue := 0;
  FMaxValue := 100;
  SetRectEmpty(FCatRect);
end;

procedure TDecoCompareGridCriterium.Assign(Source: TPersistent);
begin
  if Source is TDecoCompareGridCriterium then
  begin
    FCategory:=TDecoCompareGridCriterium(Source).FCategory;
    FStyle:=TDecoCompareGridCriterium(Source).FStyle;
    FTextFormat:=TDecoCompareGridCriterium(Source).FTextFormat;
    FMinValue:=TDecoCompareGridCriterium(Source).FMinValue;
    FMaxValue:=TDecoCompareGridCriterium(Source).FMaxValue;
  end;
  inherited Assign(Source);
end;

function TDecoCompareGridCriterium.IsFirstInCategory:Boolean;
begin
  Result:=FCatRect.Bottom-FCatRect.Top>0;
end;

procedure TDecoCompareGridCriterium.SetCategory(const Value: string);
begin
  if FCategory <> Value then
  begin
    FCategory := Value;
    Changed(True);
  end;
end;

procedure TDecoCompareGridCriterium.SetStyle(const Value: TDecoCriteriumStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TDecoCompareGridCriterium.SetTextFormat(const Value:string);
begin
  if FTextFormat <> Value then
  begin
    FTextFormat := Value;
    Changed(False);
  end;
end;

procedure TDecoCompareGridCriterium.SetMinValue(NewValue: Extended);
begin
  if NewValue<>FMinValue then
  begin
    FMinValue:=NewValue;
    if FMinValue>=FMaxValue then
      FMaxValue:=FMinValue+1;
    Changed(False);
  end;
end;

procedure TDecoCompareGridCriterium.SetMaxValue(NewValue: Extended);
begin
  if NewValue<>FMaxValue then
  begin
    FMaxValue:=NewValue;
    if FMaxValue<=FMinValue then
      FMinValue:=FMaxValue-1;
    Changed(False);
  end;
end;

//////////////////////////////////////////// TDecoCompareGridItem ////////////////////////////////////////////
constructor TDecoCompareGridItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FValues:=TDecoChartItems.Create(Collection.Owner, TDecoCompareGridValue);
end;

destructor TDecoCompareGridItem.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TDecoCompareGridItem.SetImageIndex(Value: {$IF CompilerVersion >= 24} System.UITypes.{$IFEND}TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

function TDecoCompareGridItem.IsValuesStored: Boolean;
begin
  Result := FValues.Count > 0;
end;

procedure TDecoCompareGridItem.SetValues(const Value: TDecoChartItems);
begin
  FValues.Assign(Value);
end;

procedure TDecoCompareGridItem.Assign(Source: TPersistent);
begin
  if Source is TDecoCompareGridItem then
  begin
    FValues.Assign(TDecoCompareGridItem(Source).FValues);
  end;
  inherited Assign(Source);
end;

//////////////////////////////////////////// TCustomDecoCompareGrid ////////////////////////////////////////////
constructor TCustomDecoCompareGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCriterias:=TSortedDecoChartItems.Create(Self, TDecoCompareGridCriterium);
  FCriterias.AutoSort := False;
  FItems:=TSortedDecoChartItems.Create(Self, TDecoCompareGridItem);
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;
  FCheckImagesChangeLink := TChangeLink.Create;
  FCheckImagesChangeLink.OnChange := ImageListChange;
  FRatingImagesChangeLink := TChangeLink.Create;
  FRatingImagesChangeLink.OnChange := ImageListChange;
  FItemTextLayout := taVerticalCenter;
  FItemImageLayout := iilBottom;
  FGridLineStyle := psSolid;
  FItemFont := TFont.Create;
  FItemFont.Assign(Font);
  FItemFont.OnChange := CaptionFontChange;
  FCriteriaFont := TFont.Create;
  FCriteriaFont.Assign(Font);
  FCriteriaFont.OnChange := CaptionFontChange;
  FHoverStyle := [fsUnderline];
  FCriteriaAlignment := taLeftJustify;
  FCategoryAlignment := taLeftJustify;

  FProgBarBackColor:=clBtnHighlight;
  FProgBarFrameColor:=clBtnFace;
  FProgBarFrameProgress:=False;
  FProgBarGradient:=pggGlass;
  FProgBarLevel1Color := $3437D9;
  FProgBarLevel2Color := $00BFFF;
  FProgBarLevel3Color := $24CA8B;
  FProgBarLevel1TextColor := clBlack;
  FProgBarLevel2TextColor := clBlack;
  FProgBarLevel3TextColor := clBlack;
  FProgBarLevel2Percent := 30;
  FProgBarLevel3Percent := 70;
  FProgBarRoundCorners:=0;
  FProgBarRounded:=True;
  FProgBarHeight:=16;
  FProgBarShowValueInBar := ibvPercent;
  FProgBarShowOverMaxIndicator := False;

  FShowGuideHeader:=False;
  FShowGuideFooter:=False;
  FColWidth := 0;
  FRowHeight := 0;
  FCategoryRowHeight := 0;
  FCategoryIndent := 16;
  FFixedColWidth := 0;
  FFixedRowHeight := 0;
  FHoverCell:=nil;
  FSelectedCell:=nil;
  FFixedColor := clNone;
  FFrameColor := clBtnShadow;
  FFrameWidth := 1;
  FGridLineColor := clBtnShadow;
  FGridLineWidth := 1;
  FShowVertGrid:=True;
  FShowHorzGrid:=True;
  FCategoryColor:=clNone;
  FShowCategories:=True;
  FrameRoundCorners := 0;
  FHeadGridLine := False;
  FStripeCols := False;
  FStripeRows := False;
end;

destructor TCustomDecoCompareGrid.Destroy;
begin
  FItems.Free;
  FCriterias.Free;
  FItemFont.Free;
  FCriteriaFont.Free;
  FImagesChangeLink.Free;
  FCheckImagesChangeLink.Free;
  FRatingImagesChangeLink.Free;
  inherited;
end;

procedure TCustomDecoCompareGrid.CMParentFontChanged(var Message: {$IF CompilerVersion >= 20.0} TCMParentFontChanged {$ELSE} TMessage {$IFEND});
begin
  inherited;
  if ParentFont then
  begin
    FItemFont.Assign(Font);
    FCriteriaFont.Assign(Font);
  end;
end;

procedure TCustomDecoCompareGrid.SetItemFont(const Value: TFont);
begin
  FItemFont.Assign(Value);
end;

procedure TCustomDecoCompareGrid.SetCriteriaFont(const Value: TFont);
begin
  FCriteriaFont.Assign(Value);
end;

function TCustomDecoCompareGrid.IsEmpty:Boolean;
begin
  Result := (FItems.Count=0) and (FCriterias.Count=0);
end;

function TCustomDecoCompareGrid.IsAnyItemSelected:Boolean;
begin
  Result := FSelectedCell<>nil;
end;

function TCustomDecoCompareGrid.IsCriteriasStored: Boolean;
begin
  Result := FCriterias.Count > 0;
end;

function TCustomDecoCompareGrid.IsItemSelected(const Item:TCollectionItem):Boolean;
begin
  Result := FSelectedCell=Item;
end;

function TCustomDecoCompareGrid.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TCustomDecoCompareGrid.GetFocusRect:TRect;
begin
  if FSelectedCell<>nil then
  begin
    Result:=SelectedCell.ItemRect;
    Dec(Result.Bottom);
    Dec(Result.Right);
    if SelectedCell is TDecoCompareGridCriterium then
      Inc(Result.Left)
    else
    if SelectedCell is TDecoCompareGridItem then
      Inc(Result.Top)
  end
  else
    SetRectEmpty(Result);
end;

procedure TCustomDecoCompareGrid.SelectFirstItem;
begin
  SelectedCell:=GetCell(0,0);
end;

function TCustomDecoCompareGrid.IsOnCompareStored: Boolean;
begin
  Result := Assigned(FItems.OnCompareItems);
end;

function TCustomDecoCompareGrid.GetOnCompareItems:TDecoChartCompareEvent;
begin
  Result := FItems.OnCompareItems;
end;

procedure TCustomDecoCompareGrid.SetCriterias(const Value: TSortedDecoChartItems);
begin
  FCriterias.Assign(Value);
end;

procedure TCustomDecoCompareGrid.ImageListChange(Sender: TObject);
begin
  if (Sender = Images) or (Sender = CheckImages) or (Sender = RatingImages) then
  begin
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImagesChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImagesChangeLink);
      FImages.FreeNotification(Self);
    end;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetCheckImages(Value: TCustomImageList);
begin
  if FCheckImages <> Value then
  begin
    if FCheckImages <> nil then
      FCheckImages.UnRegisterChanges(FImagesChangeLink);
    FCheckImages := Value;
    if FCheckImages <> nil then
    begin
      FCheckImages.RegisterChanges(FImagesChangeLink);
      FCheckImages.FreeNotification(Self);
    end;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetRatingImages(Value: TCustomImageList);
begin
  if FRatingImages <> Value then
  begin
    if FRatingImages <> nil then
      FRatingImages.UnRegisterChanges(FImagesChangeLink);
    FRatingImages := Value;
    if FRatingImages <> nil then
    begin
      FRatingImages.RegisterChanges(FImagesChangeLink);
      FRatingImages.FreeNotification(Self);
    end;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil
    else
    if AComponent = CheckImages then
      CheckImages := nil
    else
    if AComponent = RatingImages then
      RatingImages := nil;
  end;
end;

procedure TCustomDecoCompareGrid.SetColorVal(Index: Integer; Value: TColor);
begin
  case Index of
    0:if Value<>FFrameColor then begin FFrameColor:=Value; Invalidate; end;
    1:if Value<>FGridLineColor then begin FGridLineColor:=Value; Invalidate; end;
    2:if Value<>FFixedColor then begin FFixedColor:=Value; Invalidate; end;
    3:if Value<>FProgBarBackColor then begin FProgBarBackColor := Value; Invalidate; end;
    4:if Value<>FProgBarFrameColor then begin FProgBarFrameColor := Value; Invalidate; end;
    5:if Value<>FProgBarLevel1Color then begin FProgBarLevel1Color := Value; Invalidate; end;
    6:if Value<>FProgBarLevel1TextColor then begin FProgBarLevel1TextColor := Value; Invalidate; end;
    7:if Value<>FProgBarLevel2Color then begin FProgBarLevel2Color := Value; Invalidate; end;
    8:if Value<>FProgBarLevel2TextColor then begin FProgBarLevel2TextColor := Value; Invalidate; end;
    9:if Value<>FProgBarLevel3Color then begin FProgBarLevel3Color := Value; Invalidate; end;
   10:if Value<>FProgBarLevel3TextColor then begin FProgBarLevel3TextColor := Value; Invalidate; end;
   11:if Value<>FCategoryColor then begin FCategoryColor:=Value; Invalidate; end;
  end;
end;

procedure TCustomDecoCompareGrid.SetIntVal(Index: Integer; Value: Integer);
begin
  case Index of
    0:if Value<>FFrameWidth then begin FFrameWidth:=Value; Invalidate; end;
    1:if Value<>FGridLineWidth then begin FGridLineWidth:=Value; Invalidate; end;
    2:if Value<>FFixedColWidth then begin FFixedColWidth:=Value; FRebuildNeeded:=True; Invalidate; end;
    3:if Value<>FFixedRowHeight then begin FFixedRowHeight:=Value; FRebuildNeeded:=True; Invalidate; end;
    4:if Value<>FColWidth then begin FColWidth:=Value; FRebuildNeeded:=True; Invalidate; end;
    5:if Value<>FRowHeight then begin FRowHeight:=Value; FRebuildNeeded:=True; Invalidate; end;
    6:if Value<>FFrameRoundCorners then begin FFrameRoundCorners:=Value; Invalidate; end;
    7:if Value<>FProgBarLevel2Percent then begin FProgBarLevel2Percent := Value; Invalidate; end;
    8:if Value<>FProgBarLevel3Percent then begin FProgBarLevel3Percent := Value; Invalidate; end;
    9:if Value<>FProgBarRoundCorners then begin FProgBarRoundCorners := Value; Invalidate; end;
   10:if Value<>FProgBarHeight then begin FProgBarHeight := Value; Invalidate; end;
   11:if Value<>FCategoryRowHeight then begin FCategoryRowHeight:=Value; FRebuildNeeded:=True; Invalidate; end;
   12:if Value<>FCategoryIndent then begin FCategoryIndent:=Value; FRebuildNeeded:=True; Invalidate; end;
   13:if Value<>FOffsetX then begin FOffsetX:=Value; FRebuildNeeded:=True; Invalidate; end;
   14:if Value<>FOffsetY then begin FOffsetY:=Value; FRebuildNeeded:=True; Invalidate; end;
  end;
end;

procedure TCustomDecoCompareGrid.SetBoolVal(Index: Integer; Value: Boolean);
begin
  case Index of
    0:if Value<>FShowVertGrid then begin FShowVertGrid:=Value; Invalidate; end;
    1:if Value<>FShowHorzGrid then begin FShowHorzGrid:=Value; Invalidate; end;
    2:if Value<>FHeadGridLine then begin FHeadGridLine:=Value; Invalidate; end;
    3:if Value<>FProgBarFrameProgress then begin FProgBarFrameProgress := Value; Invalidate; end;
    4:if Value<>FProgBarRounded then begin FProgBarRounded := Value; Invalidate; end;
    5:if Value<>FProgBarShowOverMaxIndicator then begin FProgBarShowOverMaxIndicator := Value; Invalidate; end;
    6:if Value<>FShowGuideHeader then begin FShowGuideHeader := Value; FRebuildNeeded:=True; Invalidate; end;
    7:if Value<>FShowGuideFooter then begin FShowGuideFooter := Value; FRebuildNeeded:=True; Invalidate; end;
    8:if Value<>FShowCategories then begin FShowCategories := Value; FRebuildNeeded:=True; Invalidate; end;
    9:if Value<>FStripeCols then begin FStripeCols:=Value; Invalidate; end;
   10:if Value<>FStripeRows then begin FStripeRows:=Value; Invalidate; end;
  end;
end;

procedure TCustomDecoCompareGrid.SetItems(const Value: TSortedDecoChartItems);
begin
  FItems.Assign(Value);
end;

procedure TCustomDecoCompareGrid.SetItemTextLayout(Value: TVerticalAlignment);
begin
  if FItemTextLayout<>Value then
  begin
    FItemTextLayout:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetItemImageLayout(Value: TComparedItemImageLayout);
begin
  if FItemImageLayout<>Value then
  begin
    FItemImageLayout:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetGridLineStyle(Value: TPenStyle);
begin
  if Value<>FGridLineStyle then
  begin
    FGridLineStyle:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetHoverStyle(Value: TFontStyles);
begin
  if Value<>FHoverStyle then
    FHoverStyle:=Value;
end;

procedure TCustomDecoCompareGrid.SetCriteriaAlignment(Value: TAlignment);
begin
  if Value<>FCriteriaAlignment then
  begin
    FCriteriaAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetCategoryAlignment(Value: TAlignment);
begin
  if Value<>FCategoryAlignment then
  begin
    FCategoryAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetProgBarGradient(Value: TPGGradientStyle);
begin
  if Value<>FProgBarGradient then
  begin
    FProgBarGradient:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetProgBarShowValueInBar(Value: TShowValueInBar);
begin
  if Value<>FProgBarShowValueInBar then
  begin
    FProgBarShowValueInBar:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetOnCompareItems(Value: TDecoChartCompareEvent);
begin
  FItems.OnCompareItems := Value;
  if Sorted then
    Sort;
end;

procedure TCustomDecoCompareGrid.SetOnDrawCell(Value: TDecoCompareGridDrawCell);
begin
  FOnDrawCell:=Value;
  Invalidate;
end;

procedure TCustomDecoCompareGrid.CancelSelection;
begin
  SelectedCell:=nil;
end;

function TCustomDecoCompareGrid.IsAnyItemHovered:Boolean;
begin
  Result := FHoverCell <> nil;
end;

function TCustomDecoCompareGrid.IsItemHovered(const Item:TCollectionItem):Boolean;
begin
  Result := FHoverCell = Item;
end;

procedure TCustomDecoCompareGrid.CancelHover;
begin
  HoverCell := nil;
end;

procedure TCustomDecoCompareGrid.GetHoverHintInfo(var aHint:string; var aRect:TRect);
begin
  if FHoverCell <> nil then
  begin
    aHint := FHoverCell.Hint;
    aRect := FHoverCell.ItemRect;
  end;
end;

procedure TCustomDecoCompareGrid.UpdateHoverInfo(X, Y: Integer);
begin
  if (not Assigned(FHoverCell)) or (not (PtInRect(FHoverCell.ItemRect,Point(x,y)))) then
    HoverCell := GetCellAt(X,Y);
end;

function TCustomDecoCompareGrid.GetCellAt(X,Y:Integer):TDecoChartItem;
var
  p:TPoint;
  R,R2:TRect;
  i,j,z:Integer;
  A:TDecoCompareGridItem;
begin
  Result:=nil;
  p.x:=x; p.y:=y;
  for i:=0 To Items.Count-1 do
    if PtInRect(TDecoChartItem(Items.Items[i]).ItemRect,p) and TDecoChartItem(Items.Items[i]).Visible then
    begin
      Result:=TDecoChartItem(Items.Items[i]);
      break;
    end;
  if Result=nil then
  begin
    for i:=0 To Criterias.Count-1 do
      if PtInRect(TDecoChartItem(Criterias.Items[i]).ItemRect,p) then
      begin
        Result:=TDecoChartItem(Criterias.Items[i]);
        break;
      end;
    if Result=nil then
    begin
      for i:=0 To Items.Count-1 do
      begin
        A:=TDecoCompareGridItem(Items.Items[i]);
        if not A.Visible then
          continue;
        R:=A.ItemRect;
        z:=A.Values.Count;
        if z>FCriterias.Count then
          z:=FCriterias.Count;
        for j:=0 To z-1 do
        begin
          R2:=TDecoCompareGridCriterium(FCriterias.Items[j]).ItemRect;
          R2.Right:=R.Right; R2.Left:=R.Left;
          if PtInRect(R2,p) then
          begin
            Result:=TDecoChartItem(A.Values.Items[j]);
            break;
          end;
        end;
        if Result<>nil then
          break;
      end;
    end;
  end;
end;

function TCustomDecoCompareGrid.GetCell(Col,Row:Integer):TDecoChartItem;
var
  A:TDecoCompareGridItem;
  i,x:Integer;
begin
  Result:=nil;
  if (Col>=0) and (Col<Items.Count) and (Row>=0) and (Row<Criterias.Count) then
  begin
    A:=nil;
    x:=0;
    for i:=0 To Items.Count-1 do
      if TDecoCompareGridItem(Items.Items[i]).Visible then
      begin
        if x=Col then
        begin
          A:=TDecoCompareGridItem(Items.Items[i]);
          break;
        end;
        Inc(x);
      end;
    if (A<>nil) and (Row<A.Values.Count) then
      Result:=TDecoChartItem(A.Values.Items[Row]);
  end;
end;

function TCustomDecoCompareGrid.GetCell(const Item:TDecoCompareGridItem; const Criterium:TDecoCompareGridCriterium):TDecoChartItem;
var
  i:Integer;
begin
  Result:=nil;
  if Assigned(Item) and Assigned(Criterium) then
  begin
    i:=Criterium.Index;
    if i<Item.Values.Count then
      Result:=TDecoChartItem(Item.Values.Items[i]);
  end;
end;

function TCustomDecoCompareGrid.GetHitTestInfoAt(X,Y:Integer;var Item:TDecoCompareGridItem;
    var Criterium:TDecoCompareGridCriterium; var Value:TDecoCompareGridValue):Boolean;
var
  p:TPoint;
  R,R2:TRect;
  i,j,z:Integer;
  A:TDecoCompareGridItem;
begin
  Result := False;
  Item := nil;
  Criterium := nil;
  Value := nil;
  p.x:=x; p.y:=y;
  for i:=0 To Items.Count-1 do
    if PtInRect(TDecoChartItem(Items.Items[i]).ItemRect,p) and TDecoChartItem(Items.Items[i]).Visible then
    begin
      Result:=True;
      Item:=TDecoCompareGridItem(Items.Items[i]);
      break;
    end;
  if not Result then
  begin
    for i:=0 To Criterias.Count-1 do
      if PtInRect(TDecoChartItem(Criterias.Items[i]).ItemRect,p) then
      begin
        Result:=True;
        Criterium:=TDecoCompareGridCriterium(Criterias.Items[i]);
        break;
      end;
    if not Result then
    begin
      for i:=0 To Items.Count-1 do
      begin
        A:=TDecoCompareGridItem(Items.Items[i]);
        if not A.Visible then
          continue;
        R:=A.ItemRect;
        z:=A.Values.Count;
        if z>FCriterias.Count then
          z:=FCriterias.Count;
        for j:=0 To z-1 do
        begin
          R2:=TDecoCompareGridCriterium(FCriterias.Items[j]).ItemRect;
          R2.Right:=R.Right; R2.Left:=R.Left;
          if PtInRect(R2,p) then
          begin
            Result:=True;
            Item:=A;
            Criterium:=TDecoCompareGridCriterium(Criterias.Items[j]);
            Value:=TDecoCompareGridValue(A.Values.Items[j]);
            break;
          end;
        end;
        if Result then
          break;
      end;
    end;
  end;
end;

function TCustomDecoCompareGrid.GetHitTestInfoAt(Cell:TDecoChartItem; var Item:TDecoCompareGridItem;
    var Criterium:TDecoCompareGridCriterium; var Value:TDecoCompareGridValue):Boolean;
var
  i,j,z:Integer;
  A:TDecoCompareGridItem;
begin
  Result := False;
  Item := nil;
  Criterium := nil;
  Value := nil;
  if Cell<>nil then
  begin
    if Cell is TDecoCompareGridItem then
      for i:=0 To Items.Count-1 do
        if Cell=Items.Items[i] then
        begin
          Result:=True;
          Item:=TDecoCompareGridItem(Items.Items[i]);
          break;
        end;
    if not Result then
    begin
      if Cell is TDecoCompareGridCriterium then
        for i:=0 To Criterias.Count-1 do
          if Cell=Criterias.Items[i] then
          begin
            Result:=True;
            Criterium:=TDecoCompareGridCriterium(Criterias.Items[i]);
            break;
          end;
      if (not Result) and (Cell is TDecoCompareGridValue) then
        for i:=0 To Items.Count-1 do
        begin
          A:=TDecoCompareGridItem(Items.Items[i]);
          z:=A.Values.Count;
          if z>FCriterias.Count then
            z:=FCriterias.Count;
          for j:=0 To z-1 do
            if Cell=A.Values.Items[j] then
            begin
              Result:=True;
              Item:=A;
              Criterium:=TDecoCompareGridCriterium(Criterias.Items[j]);
              Value:=TDecoCompareGridValue(A.Values.Items[j]);
              break;
            end;
          if Result then
            break;
        end;
    end;
  end;
end;

function TCustomDecoCompareGrid.GetCellItem(Cell:TDecoChartItem):TDecoCompareGridItem;
var
  C:TDecoCompareGridCriterium;
  V:TDecoCompareGridValue;
begin
  GetHitTestInfoAt(Cell,Result,C,V);
end;

procedure TCustomDecoCompareGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  UpdateHoverInfo(X,Y);
  SelectedCell:=FHoverCell;
end;

procedure TCustomDecoCompareGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateHoverInfo(X,Y);
end;

procedure TCustomDecoCompareGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (button=mbLeft) and Assigned(FOnCellClick) and Assigned(FHoverCell) then
    FOnCellClick(Self,FHoverCell);
end;

procedure TCustomDecoCompareGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  DI:TDecoChartItem;
  Item:TDecoCompareGridItem;
  Criterium:TDecoCompareGridCriterium;
  Value:TDecoCompareGridValue;
begin
  inherited KeyDown(Key,Shift);
  if Shift=[ssCtrl] then
    case Key of
      VK_HOME: begin SelectedCell:=GetCell(0,0); SetOffsetXY(0,0); end;
      VK_END:
        begin
          if Criterias.Count=0 then SelectedCell:=Items.GetPriorItem(nil,True)
          else if Items.Count=0 then SelectedCell:=Criterias.GetPriorItem(nil,True)
          else
          begin
            DI:=Items.GetPriorItem(nil,True);
            if (DI<>nil) and (TDecoCompareGridItem(DI).Values.Count>=Criterias.Count) then
              SelectedCell:=TDecoChartItem(TDecoCompareGridItem(DI).Values.Items[Criterias.Count-1]);
          end;
          MakeVisible(SelectedCell);
        end;
    end
  else
  if Shift=[] then
    case Key of
      VK_ESCAPE:
        if Assigned(FSelectedCell) then SelectedCell:=nil;
      VK_END:
        begin
          DI:=FItems.GetPriorItem(nil,True);
          if DI<>nil then
          begin
            GetHitTestInfoAt(FSelectedCell,Item,Criterium,Value);
            if Criterium=nil then SelectedCell:=DI
            else SelectedCell:=GetCell(TDecoCompareGridItem(DI),Criterium);
          end;
          MakeVisible(SelectedCell);
        end;
      VK_UP,VK_DOWN,VK_LEFT,VK_RIGHT,VK_NEXT,VK_PRIOR,VK_HOME:
        if FSelectedCell=nil then
        begin
          SelectedCell:=GetCell(0,0);
          MakeVisible(SelectedCell);
        end
        else
        begin
          GetHitTestInfoAt(FSelectedCell,Item,Criterium,Value);
          case Key of
            VK_UP:
              if Value<>nil then
              begin
                if Value<>Item.Values.Items[0] then
                  SelectedCell:=Item.Values.GetPriorItem(Value)
                else
                  SelectedCell:=Item;
              end
              else if (Criterium<>nil) and (Criterium<>Criterias.Items[0]) then
                SelectedCell:=Criterias.GetPriorItem(Criterium);
            VK_DOWN:
              if Value<>nil then
              begin
                if (Item.Values.Count>=Criterias.Count) and (Value<>Item.Values.Items[Criterias.Count-1]) then
                  SelectedCell:=Item.Values.GetNextItem(Value)
              end
              else if (Item<>nil) and (Item.Values.Count>0) then
                SelectedCell:=Item.Values.GetNextItem(nil)
              else if (Criterium<>nil) and (Criterium<>Criterias.Items[Criterias.Count-1]) then
                SelectedCell:=Criterias.GetNextItem(Criterium);
            VK_LEFT:
              begin
                if Item<>nil then
                  DI:=Items.GetPriorItem(Item,True)
                else
                  DI:=nil;
                if DI<>nil then
                begin
                  if Value=nil then
                    SelectedCell:=DI
                  else
                    SelectedCell:=GetCell(TDecoCompareGridItem(DI),Criterium)
                end
                else
                if Criterium<>nil then
                  SelectedCell:=Criterium;
              end;
            VK_RIGHT:
              begin
                DI:=Items.GetNextItem(Item,True);
                if DI<>nil then
                begin
                  if Criterium=nil then
                    SelectedCell:=DI
                  else
                    SelectedCell:=GetCell(TDecoCompareGridItem(DI),Criterium)
                end;
              end;
            VK_PRIOR:
              if Value<>nil then
              begin
                if Value<>Item.Values.Items[0] then
                  SelectedCell:=TDecoChartItem(Item.Values.Items[0])
                else
                  SelectedCell:=Item;
              end
              else if (Criterium<>nil) and (Criterium<>Criterias.Items[0]) then
                SelectedCell:=TDecoChartItem(Criterias.Items[0]);
            VK_NEXT:
              if Value<>nil then
              begin
                if (Item.Values.Count>=Criterias.Count) and (Value<>Item.Values.Items[Criterias.Count-1]) then
                  SelectedCell:=TDecoChartItem(Item.Values.Items[Criterias.Count-1])
              end
              else if (Item<>nil) and (Item.Values.Count>=Criterias.Count) then
                SelectedCell:=TDecoChartItem(Item.Values.Items[Criterias.Count-1])
              else if (Criterium<>nil) and (Criterium<>Criterias.Items[Criterias.Count-1]) then
                SelectedCell:=TDecoChartItem(Criterias.Items[Criterias.Count-1]);
            VK_HOME:
              begin
                DI:=Items.GetNextItem(nil,True);
                if DI<>nil then
                begin
                  if Value=nil then
                    SelectedCell:=DI
                  else
                    SelectedCell:=GetCell(TDecoCompareGridItem(DI),Criterium)
                end;
              end;
          end;
          MakeVisible(SelectedCell);
        end;
    end;
end;

procedure TCustomDecoCompareGrid.SetHoverCell(const Value:TDecoChartItem);
begin
  if Value<>FHoverCell then
  begin
    FHoverCell:=Value;
    if Assigned(FOnHoverChange) then
      FOnHoverChange(Self,FHoverCell);
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.SetSelectedCell(const Value:TDecoChartItem);
begin
  if Value<>FSelectedCell then
  begin
    FSelectedCell:=Value;
    if Assigned(FOnSelectCell) then
      FOnSelectCell(Self,FSelectedCell);
    Invalidate;
  end;
end;

function TCustomDecoCompareGrid.GetValueFormatted(const aCriterium:TDecoCompareGridCriterium; aValue:TDecoCompareGridValue):string;
var
  e:Extended;
  i:Integer;
begin
  if Length(aCriterium.TextFormat)=0 then
    Result:=aValue.AsString
  else
  try
    case aCriterium.Style of
      cgsCheck:
        begin
          i:=Pos('|',aCriterium.TextFormat);
          if i>0 then
          begin
            if aValue.AsBoolean then
              Result:=Copy(aCriterium.TextFormat,i+1,maxint)
            else
              Result:=Copy(aCriterium.TextFormat,1,i-1)
          end
          else
            Result:=Format(aCriterium.TextFormat,[aValue.AsBoolean]);
        end;
      cgsRating:Result:=Format(aCriterium.TextFormat,[aValue.AsExtended]);
      cgsProgressBar:Result:=Format(aCriterium.TextFormat,[aValue.AsInteger]);
      cgsNumber:
        if TryStrToFloat(aValue.FValue, e) then
          Result:=Format(aCriterium.TextFormat,[e])
        else
          Result:=aValue.AsString;
      cgsCustom:Result:=aValue.AsString;
    else
      Result:=Format(aCriterium.TextFormat,[aValue.AsString]);
    end;
  except
    Result:=aValue.AsString;
  end;
end;

procedure TCustomDecoCompareGrid.Sort;
begin
  FItems.BeginUpdate;
  try
    FItems.Sort;
  finally
    FItems.EndUpdate;
  end;
end;

procedure TCustomDecoCompareGrid.SetOffsetXY(x,y:Integer);
begin
  if (x<>FOffsetX) or (y<>FOffsetY) then
  begin
    FOffsetX:=x;
    FOffsetY:=y;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoCompareGrid.MakeVisible(Item:TDecoChartItem);
var
  R:TRect;
  dx,dy:Integer;
begin
  if Item = nil then
    Exit;
  dx:=0;
  dy:=0;
  R:=ClientRect;
  if Item.ItemRect.Left<R.Left then
    dx:=Item.ItemRect.Left-R.Left
  else
  if Item.ItemRect.Right>R.Right then
    dx:=Item.ItemRect.Right-R.Right;
  if Item.ItemRect.Top<R.Top then
    dy:=Item.ItemRect.Top-R.Top
  else
  if Item.ItemRect.Bottom>R.Bottom then
    dy:=Item.ItemRect.Bottom-R.Bottom;
  SetOffsetXY(FOffsetX+dx,FOffsetY+dy);
end;

function TCustomDecoCompareGrid.AddCriterium(const Caption:string; const Category:string = ''; const Hint:string = '';
    Style:TDecoCriteriumStyle = cgsText; const TextFormat:string = ''; MinValue:Extended = 0; MaxValue:Extended = 100):TDecoCompareGridCriterium;
begin
  Result := Criterias.Add as TDecoCompareGridCriterium;
  Result.Caption := Caption;
  Result.Category := Category;
  Result.Hint := Hint;
  Result.Style := Style;
  Result.MinValue := MinValue;
  Result.MaxValue := MaxValue;
  Result.TextFormat := TextFormat;
end;

function TCustomDecoCompareGrid.AddItem(const Caption:string; ImageIndex:Integer = -1; const Hint:string = ''):TDecoCompareGridItem;
begin
  Result:=Items.Add as TDecoCompareGridItem;
  Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Hint;
end;

function TCustomDecoCompareGrid.AddValue(const Item:TDecoCompareGridItem; const Value:string):TDecoCompareGridValue;
begin
  Result:=TDecoCompareGridValue(Item.Values.Add);
  Result.Value := Value;
end;

function TCustomDecoCompareGrid.AddValue(const Item:TDecoCompareGridItem; const Values:array of Variant):TDecoCompareGridValue;
var
  i:Integer;
begin
  Result := nil;
  Item.Values.BeginUpdate;
  try
    for i:=0 To Length(Values)-1 do
    begin
      Result:=TDecoCompareGridValue(Item.Values.Add);
      Result.SetAsVariant(Values[i]);
    end;
  finally
    Item.Values.EndUpdate;
  end;
end;

procedure TCustomDecoCompareGrid.SetValue(const Item:TDecoCompareGridItem; Criterium:TDecoCompareGridCriterium; const Value:string);
var
  c,i,j:Integer;
begin
  c:=Criterium.Index;
  j:=Item.Values.Count;
  if j-1<c then
    Item.Values.BeginUpdate;
  try
    if j-1<c then
      for i:=j To c do
        Item.Values.Add;
    TDecoCompareGridValue(Item.Values.Items[c]).Value := Value;
  finally
    if j-1<c then
      Item.Values.EndUpdate;
  end;
end;

procedure TCustomDecoCompareGrid.SetValue(const Item:TDecoCompareGridItem; Criterium:TDecoCompareGridCriterium; const Value:Variant);
var
  c,i,j:Integer;
begin
  c:=Criterium.Index;
  j:=Item.Values.Count;
  if j-1<c then
    Item.Values.BeginUpdate;
  try
    if j-1<c then
      for i:=j To c do
        Item.Values.Add;
    TDecoCompareGridValue(Item.Values.Items[c]).SetAsVariant(Value);
  finally
    if j-1<c then
      Item.Values.EndUpdate;
  end;
end;

procedure TCustomDecoCompareGrid.SetValue(const Item:TDecoCompareGridItem; const Values:array of Variant);
var
  c,i,j:Integer;
begin
  Item.Values.BeginUpdate;
  try
    c:=Length(Values);
    j:=Item.Values.Count;
    if j<c then
      for i:=j To c-1 do
        Item.Values.Add;
    for i:=0 To Length(Values)-1 do
      TDecoCompareGridValue(Item.Values.Items[i]).SetAsVariant(Values[i]);
  finally
    Item.Values.EndUpdate;
  end;
end;

procedure TCustomDecoCompareGrid.BeginUpdate;
begin
  if UpdateCount=0 then
  begin
    Items.BeginUpdate;
    Criterias.BeginUpdate;
  end;
  inherited;
end;

procedure TCustomDecoCompareGrid.EndUpdate;
begin
  if UpdateCount<=1 then
  begin
    Criterias.EndUpdate;
    Items.EndUpdate;
  end;
  inherited;
end;

procedure TCustomDecoCompareGrid.Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil);
var
  i,j,z:Integer;
  C:TDecoCompareGridCriterium;
  A:TDecoCompareGridItem;
  R,R2,iRect,cRect:TRect;
  x,xm,y:Integer;
  TM:TTextMetric;
  cHeight:Integer;  // category row height
  iHeight:Integer;  // row height
  iWidth:Integer;   // column width
  fColW:Integer;    // fixed column width
  fRowH:Integer;    // fixed row height
  lastCat:string;
begin
  if not FRebuildNeeded then
    Exit;
  inherited Rebuild(Dest, TargetCanvas);

  if TargetCanvas=nil then
    TargetCanvas:=Canvas;
  {$IF CompilerVersion >= 20.0}
  Inc(Dest.Left,Padding.Left); Inc(Dest.Top,Padding.Top); Dec(Dest.Right,Padding.Right); Dec(Dest.Bottom,Padding.Bottom);
  {$ELSE}
  InflateRect(Dest,-4,-4);
  {$IFEND}

  if (FOffsetX<>0) or (FOffsetY<>0) then
    offsetRect(Dest,-FOffsetX,-FOffsetY);

  FFrameRect:=Dest;
  FGuideHeader:=Dest;
  FGuideFooter:=Dest;
  FCaptionRect:=Dest;

  TargetCanvas.Font := Font;
  GetTextMetrics(TargetCanvas.Handle,TM);
  if FShowGuideHeader then FGuideHeader.Bottom:=FGuideHeader.Top + TM.tmHeight+TM.tmDescent+1 else FGuideHeader.Bottom:=FGuideHeader.Top;
  if FShowGuideFooter then FGuideFooter.Bottom:=FGuideFooter.Top + TM.tmHeight+TM.tmDescent+1 else FGuideFooter.Bottom:=FGuideFooter.Top;

  if Abs(FCriteriaFont.Height)>abs(Font.Height) then
  begin
    TargetCanvas.Font := FCriteriaFont;
    GetTextMetrics(TargetCanvas.Handle,TM);
    cHeight := Abs(CaptionFont.Height)-Abs(FCriteriaFont.Height);
  end
  else
    cHeight := Abs(CaptionFont.Height)-Abs(Font.Height);
  case TextMargin of
    -1:FSpacing:=TM.tmDescent+1;
     0:FSpacing:=1;
  else
       FSpacing:=TextMargin;
  end;

  iHeight:=TM.tmHeight+2*FSpacing + 1;
  if Assigned(FCheckImages) and (iHeight<FCheckImages.Height) then
    iHeight := FCheckImages.Height;
  if Assigned(FRatingImages) and (iHeight<FRatingImages.Height) then
    iHeight := FRatingImages.Height;

  y:=FGuideHeader.Bottom;
  FCaptionRect.Top:=y;

  if FFixedRowHeight<=0 then
  begin
    TargetCanvas.Font := FItemFont;
    GetTextMetrics(TargetCanvas.Handle,TM);
    fRowH := (TM.tmHeight+2*FSpacing) * 2 - FSpacing;
    if Assigned(FImages) then
    begin
      case FItemImageLayout of
        iilLeft,iilRight:
          if FImages.Height+2*FSpacing>fRowH then
            fRowH:=FImages.Height+2*FSpacing;
      else
        Inc(fRowH,FImages.Height+2*FSpacing);
      end;
    end
  end
  else
    fRowH := FFixedRowHeight;
  if FRowHeight>0 then
    iHeight:=FRowHeight;
  if FCategoryRowHeight>0 then
    cHeight:=FCategoryRowHeight
  else
    cHeight:=iHeight+cHeight;

  SetRect(iRect,Dest.Left,y,Dest.Right,y+fRowH);
  Inc(y,fRowH);
  FCaptionRect.Bottom:=y;

  fColW:=FFixedColWidth;
  if FFixedColWidth<=0 then
  begin
    TargetCanvas.Font := FCriteriaFont;
    for i:=0 To FCriterias.Count-1 do
    begin
      C:=FCriterias.Items[i] as TDecoCompareGridCriterium;
      j:=TargetCanvas.TextWidth(C.Caption);
      if FShowCategories and (Length(C.Category)>0) then
        Inc(j,FCategoryIndent);
      if j>fColW then
        fColW:=j;
    end;
    Inc(fColW, 4*FSpacing+1);
  end;

  if FItems.Count>0 then
    FCaptionRect.Right:=FCaptionRect.Left+fColW;

  lastCat:='';
  SetRect(cRect,Dest.Left,y,Dest.Right,Dest.Bottom);
  for i:=0 To FCriterias.Count-1 do
  begin
    C:=FCriterias.Items[i] as TDecoCompareGridCriterium;
    if FShowCategories then
    begin
      if (C.Category<>lastCat) then
      begin
        SetRect(C.FCatRect, cRect.Left, y, cRect.Left + fColW, y + cHeight);
        Inc(y,cHeight);
        lastCat:=C.Category;
      end
      else
        SetRectEmpty(C.FCatRect);
    end;
    SetRect(R, cRect.Left, y, cRect.Left + fColW, y + iHeight);
    C.ItemRect:=R;
    Inc(y,iHeight);
  end;
  FFrameRect.Bottom:=y;

  xm:=0;
  iRect.Left:=iRect.Left+fColW;
  if FItems.Count>0 then
  begin
    if FColWidth>0 then
      iWidth := FColWidth
    else
    begin
      for i:=0 To FItems.Count-1 do
        if TDecoCompareGridItem(FItems.Items[i]).Visible then Inc(xm);
      if xm>0 then
      begin
        iWidth := (iRect.Right - iRect.Left) div xm;
        xm:=(iRect.Right - iRect.Left) - iWidth*xm;
      end
      else
        iWidth := (iRect.Right - iRect.Left)
    end;
  end
  else
    iWidth := (iRect.Right - iRect.Left);
  x:=iRect.Left;
  for i:=0 To FItems.Count-1 do
  begin
    A:=TDecoCompareGridItem(FItems.Items[i]);
    if not A.Visible then
      continue;
    SetRect(R, x, iRect.Top, x + iWidth, iRect.Bottom);
    Inc(x, iWidth);
    if i=0 then
    begin
      Inc(R.Right,xm);
      Inc(x,xm);
    end;
    A.ItemRect:=R;
    z:=A.Values.Count;
    if z>FCriterias.Count then
      z:=FCriterias.Count;
    for j:=0 To z-1 do
    begin
      R2:=TDecoCompareGridCriterium(FCriterias.Items[j]).ItemRect;
      R2.Right:=R.Right; R2.Left:=R.Left;
      TDecoCompareGridValue(A.Values.Items[j]).ItemRect:=R2;
    end;
  end;

  if (FColWidth>0) and (FItems.Count>0) then
  begin
    FFrameRect.Right:=R.Right;
    FGuideHeader.Right:=R.Right;
    FGuideFooter.Right:=R.Right;
  end;

  if FShowGuideFooter then
  begin
    i:=FGuideFooter.Bottom-FGuideFooter.Top;
    Inc(FFrameRect.Bottom,i);
    FGuideFooter.Bottom:=FFrameRect.Bottom;
    FGuideFooter.Top:=FFrameRect.Bottom-i-1;
  end;
  if FShowGuideHeader then
    Inc(FGuideHeader.Bottom);

end;

procedure TCustomDecoCompareGrid.DrawChart(TargetCanvas:TCanvas);
var
  i,j,x,z,zi:Integer; // rows, cols
  ve,zs:Extended;
  imH,imW,rX,rM:Integer;
  R,R2,RR:TRect;
  Flags,cFlags:Longint;
  C:TDecoCompareGridCriterium;
  A:TDecoCompareGridItem;
  V:TDecoCompareGridValue;
  tmp:string;
  drawImg:Boolean;
  hdrLn:Boolean;
  iiL:TComparedItemImageLayout;
  stripe:Integer;
begin
  // draw fixed areas, caption, guides
  R:=FCaptionRect;
  if (FFixedColor<>clNone) and (not FStripeCols) and (not FStripeRows) then
  begin
    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Brush.Color := FFixedColor;
    R2:=R;
    R2.Right:=FFrameRect.Right;
    TargetCanvas.FillRect(R2);

    R2.Right:=R.Right;
    if FShowGuideFooter then
      R2.Bottom:=FGuideFooter.Top
    else
      R2.Bottom:=FFrameRect.Bottom;
    R2.Top:=R.Bottom;
    TargetCanvas.FillRect(R2);

    if FShowGuideHeader then
      TargetCanvas.FillRect(FGuideHeader);
    if FShowGuideFooter then
      TargetCanvas.FillRect(FGuideFooter);
  end;

  tmp := Caption;
  TargetCanvas.Font := CaptionFont;
  if not Enabled then
    TargetCanvas.Font.Color:=clGrayText;
  TargetCanvas.Brush.Style := bsClear;
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, TargetCanvas, nil, nil, nil, R, False, tmp);
  if ShowCaption and (Length(tmp)>0) then
  begin
    InflateRect(R,-2*FSpacing,0);
    Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_TOP or DT_CENTER);
    R2:=R;
    i:=FDrawTextFunc(TargetCanvas.Handle, tmp, R2, Flags or DT_CALCRECT, TargetCanvas.Font.Color, GlowSize);
    if R.Top+i+FSpacing>R.Bottom then
    begin
      Inc(R.Top,FSpacing);
      FDrawTextFunc(TargetCanvas.Handle, tmp, R, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize)
    end
    else
    begin
      Inc(R.Top,(R.Bottom-R.Top-i) div 2);
      FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
    end;
    InflateRect(R,2*FSpacing,0);
  end;
  // draw guide header
  if FShowGuideHeader then
  begin
    R2:=FGuideHeader; R2.Left:=R.Left; R2.Right:=R.Right;
    TargetCanvas.Font := Font;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;
    TargetCanvas.Brush.Style := bsClear;
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, TargetCanvas, nil, nil, nil, R2, True, tmp);
  end;
  // draw guide footer
  if FShowGuideFooter then
  begin
    R2:=FGuideFooter; R2.Left:=R.Left; R2.Right:=R.Right;
    TargetCanvas.Font := Font;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;
    TargetCanvas.Brush.Style := bsClear;
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, TargetCanvas, nil, nil, nil, R2, True, tmp);
  end;

  // draw items
  if Assigned(FImages) then
  begin
    imH := FImages.Height+2*FSpacing;
    imW := FImages.Width+4*FSpacing;
  end
  else
  begin
    imH := 0;
    imW := 0;
  end;
  iiL:=FItemImageLayout;
  if UseRightToLeftAlignment then
    case iiL of
      iilRight:iiL:=iilLeft;
      iilLeft:iiL:=iilRight;
    end;
  Flags := DT_WORDBREAK or DT_TOP or DT_END_ELLIPSIS;
  case iiL of
    iilLeft:Flags:=Flags or DT_LEFT;
    iilRight:Flags:=Flags or DT_RIGHT;
  else
    Flags:=Flags or DT_CENTER;
  end;
  Flags := DrawTextBiDiModeFlags(Flags);
  hdrLn := (FHeadGridLine and (FFrameWidth>0) and (FFrameColor<>clNone));

  stripe := 0;
  for j:=0 To FItems.Count-1 do
  begin
    A:=TDecoCompareGridItem(FItems.Items[j]);
    if not A.Visible then
      continue;
    R:=A.ItemRect;

    if FStripeCols and (FFixedColor<>clNone) and (stripe mod 2 = 0) then
    begin
      R2:=R;
      if FShowGuideHeader then R2.Top:=FGuideHeader.Top;
      if FShowGuideFooter then R2.Bottom:=FGuideFooter.Bottom else R2.Bottom:=FFrameRect.Bottom;
      TargetCanvas.Brush.Style := bsSolid;
      TargetCanvas.Brush.Color := FFixedColor;
      TargetCanvas.FillRect(R2);
    end;
    Inc(stripe);

    // draw guide header
    if FShowGuideHeader then
    begin
      R2:=FGuideHeader; R2.Left:=R.Left; R2.Right:=R.Right;
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      tmp:=IntToStr(j+1);
      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, TargetCanvas, A, nil, nil, R2, True, tmp);
      if Length(tmp)>0 then
        FDrawTextFunc(TargetCanvas.Handle, tmp, R2, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize);
    end;
    // draw guide footer
    if FShowGuideFooter then
    begin
      R2:=FGuideFooter; R2.Left:=R.Left; R2.Right:=R.Right;
      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      tmp:=IntToStr(j+1);
      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, TargetCanvas, A, nil, nil, R2, True, tmp);
      if Length(tmp)>0 then
      begin
        Dec(R2.Bottom);
        FDrawTextFunc(TargetCanvas.Handle, tmp, R2, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_END_ELLIPSIS), TargetCanvas.Font.Color, GlowSize);
      end;
    end;

    TargetCanvas.Font := FItemFont;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;
    TargetCanvas.Brush.Style := bsClear;
    if A=FHoverCell then
      TargetCanvas.Font.Style:=TargetCanvas.Font.Style+FHoverStyle;

    tmp:=A.Caption;
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, TargetCanvas, A, nil, nil, R, False, tmp);

    InflateRect(R,-2*FSpacing,0);
    if Assigned(FImages) and (R.Bottom-R.Top>=imH) and (R.Right - R.Left>imW) then
    begin
      if A.ImageIndex>=0 then
        case iiL of
          iilBottom:
            begin
              FImages.Draw(TargetCanvas, R.Left + (R.Right - R.Left - FImages.Width) div 2, R.Bottom - imH + FSpacing, A.ImageIndex);
              Dec(R.Bottom,imH);
            end;
          iilTop:
            begin
              FImages.Draw(TargetCanvas, R.Left + (R.Right - R.Left - FImages.Width) div 2, R.Top + FSpacing, A.ImageIndex);
              Inc(R.Top,imH);
            end;
          iilLeft:
            begin
              FImages.Draw(TargetCanvas, R.Left + 2*FSpacing, R.Top + (R.Bottom - R.Top - FImages.Height) div 2, A.ImageIndex);
              Inc(R.Left,imW+2*FSpacing);
            end;
          iilRight:
            begin
              FImages.Draw(TargetCanvas, R.Right - 2*FSpacing - FImages.Width, R.Top + (R.Bottom - R.Top - FImages.Height) div 2, A.ImageIndex);
              Dec(R.Right,imW+2*FSpacing);
            end;
        end;
    end;

    if Length(tmp)>0 then
    begin
      R2:=R;
      i:=FDrawTextFunc(TargetCanvas.Handle, A.Caption, R2, Flags or DT_CALCRECT, TargetCanvas.Font.Color, GlowSize);

      if (i>R.Bottom-R.Top-FSpacing) or (R2.Left<R.Left) or (R2.Right>R.Right) then
      begin
        R2:=R;
        i:=FDrawTextFunc(TargetCanvas.Handle, A.Caption, R2, DT_SINGLELINE or Flags or DT_CALCRECT and (not DT_WORDBREAK), TargetCanvas.Font.Color, GlowSize);
        if i<=R.Bottom-R.Top-FSpacing then
        begin
          case FItemTextLayout of
            taAlignTop:if iiL<>iilTop then Inc(R.Top,FSpacing);
            taAlignBottom:begin R.Top:=R.Bottom-i; if (iiL<>iilBottom) or (not Assigned(FImages)) then Dec(R.Top,FSpacing); end;
          else
            Inc(R.Top,(R.Bottom-R.Top-i) div 2);
          end;
          FDrawTextFunc(TargetCanvas.Handle, tmp, R, DT_SINGLELINE or Flags and (not DT_WORDBREAK), TargetCanvas.Font.Color, GlowSize)
        end;
      end
      else
      begin
        case FItemTextLayout of
          taAlignTop:if iiL<>iilTop then Inc(R.Top,FSpacing);
          taAlignBottom:begin R.Top:=R.Bottom-i; if (iiL<>iilBottom) or (not Assigned(Images)) then Dec(R.Top,FSpacing); end;
        else
          Inc(R.Top,(R.Bottom-R.Top-i) div 2);
        end;

        FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
      end;
    end;

  end;

  // draw values
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_CENTER or DT_END_ELLIPSIS or DT_VCENTER);
  for j:=0 To FItems.Count-1 do
  begin
    A:=TDecoCompareGridItem(FItems.Items[j]);
    if not A.Visible then
      continue;
    stripe:=0;
    x:=A.Values.Count;
    if x>FCriterias.Count then
      x:=FCriterias.Count;
    for i:=0 To x-1 do
    begin
      V:=TDecoCompareGridValue(A.Values.Items[i]);
      C:=TDecoCompareGridCriterium(FCriterias.Items[i]);
      R2:=V.ItemRect;

      if FStripeRows and (FFixedColor<>clNone) and (stripe mod 2 = 0) then
      begin
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.Brush.Color := FFixedColor;
        TargetCanvas.FillRect(R2);
      end;
      Inc(stripe);

      if not V.Visible then
        continue;

      TargetCanvas.Font := Font;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      if V=FHoverCell then
        TargetCanvas.Font.Style:=TargetCanvas.Font.Style+FHoverStyle;

      tmp:=GetValueFormatted(C,V);
      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, TargetCanvas, A, C, V, R2, False, tmp);
      case C.Style of
        cgsCheck:
            if Assigned(FCheckImages) then
            begin
              FCheckImages.Draw(TargetCanvas, R2.Left + (R2.Right - R2.Left - FCheckImages.Width) div 2,
                R2.Top + (R2.Bottom - R2.Top - FCheckImages.Height) div 2, Integer(V.AsBoolean));
              tmp := '';
            end;
        cgsRating:
            begin
              RR:=R2;
              drawImg:=Assigned(FRatingImages) and (FRatingImages.Count>1);
              if drawImg then
              begin
                imW:=FRatingImages.Width;
                cFlags:=FRatingImages.Count;
                rM:=-1;
                offsetRect(RR,0,-1);
              end
              else
              begin
                TargetCanvas.Brush.Style:=bsSolid;
                TargetCanvas.Pen.Style:=psSolid;
                TargetCanvas.Pen.Width:=1;
                imW:=TargetCanvas.Font.Size;
                imW:=imW+(1-imW mod 2);
                if imW>RR.Bottom-RR.Top then imW:=RR.Bottom-RR.Top
                else if imW<4 then imW:=4;
                cFlags:=3;
                rM:=(imW div 8)+1;
              end;
              imH:=imW;

              RR.Left := RR.Left + (RR.Right-RR.Left - 5*(imW+rM)) div 2;
              RR.Right:=RR.Left+imW;
              RR.Top := RR.Top + (RR.Bottom-RR.Top - imH) div 2;
              offsetRect(RR,rM div 2,0);

              if (RR.Left-2*FSpacing>=R2.Left) and (RR.Right+2*FSpacing<=R2.Right) then
              begin
                ve:=V.AsExtended;
                zi:=0;
                z:=1;
                if cFlags=3 then
                begin
                  zs:=round((ve/((C.MaxValue-C.MinValue)/5)+0.2)*10)/10;
                  while z<=5 do
                  begin
                    if zi=0 then
                    begin
                      if zs<z then
                      begin
                        zi:=1;
                        if zs<=z-0.6 then
                          zi:=2;
                      end;
                    end;
                    if drawImg then
                      FRatingImages.Draw(TargetCanvas, RR.Left, RR.Top, zi)
                    else
                    begin
                      if zi=0 then
                      begin
                        TargetCanvas.Pen.Color:=FProgBarLevel2Color;
                        TargetCanvas.Brush.Color:=BrightColor(FProgBarLevel2Color,10);
                        TargetCanvas.Ellipse(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
                      end
                      else
                      if zi=cFlags-1 then
                      begin
                        TargetCanvas.Pen.Color:=FProgBarFrameColor;
                        TargetCanvas.Brush.Color:=FProgBarBackColor;
                        TargetCanvas.Ellipse(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
                      end
                      else
                      begin
                        TargetCanvas.Pen.Color:=FProgBarLevel2Color;
                        TargetCanvas.Brush.Color:=FProgBarBackColor;
                        TargetCanvas.Ellipse(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH);
                        rX:=RR.Left+imW div 2;
                        TargetCanvas.Polyline([point(rX,RR.Top+1), point(rx,RR.Top+imH-1)]);
                        TargetCanvas.Brush.Color:=BrightColor(FProgBarLevel2Color,10);
                        TargetCanvas.FloodFill(rx-2,RR.Top+4,FProgBarLevel2Color,fsBorder);
                        TargetCanvas.Pen.Color:=FProgBarFrameColor;
                        TargetCanvas.Arc(RR.Left,RR.Top,RR.Left+imW,RR.Top+imH,rX+1,RR.Top+imH,rX-1,RR.Top);
                      end;
                    end;
                    offsetRect(RR,imW+rM,0);
                    if zi=1 then
                      zi:=2;
                    Inc(z);
                  end;
                end
                else
                begin
                  zs:=ve/((C.MaxValue-C.MinValue)/5)+0.5;
                  while z<=5 do
                  begin
                    if (zi=0) and (zs<z) then zi:=1;
                    FRatingImages.Draw(TargetCanvas, RR.Left, RR.Top, zi);
                    offsetRect(RR,imW+rM,0);
                    Inc(z);
                  end;
                end;
                tmp := '';
              end;
            end;
        cgsProgressBar:
          begin
            RR:=R2;
            InflateRect(RR,-4*FSpacing,0);
            if RR.Left+FSpacing*12<RR.Right then
            begin
              if (FProgBarHeight>0) and (FProgBarHeight<=R2.Bottom-R2.Top - 2*FSpacing) then
              begin
                RR.Top:=RR.Top+(RR.Bottom-RR.Top-FProgBarHeight) div 2;
                RR.Bottom:=RR.Top+FProgBarHeight;
              end
              else
              begin
                InflateRect(RR,0,-FSpacing);
                Dec(RR.Bottom);
              end;
              TargetCanvas.Font.Style:=[];
              DrawProgressBar(TargetCanvas, RR, V.AsExtended, C.MinValue, C.MaxValue, FProgBarGradient, Color, FProgBarBackColor, FProgBarFrameColor,
                1, FProgBarRounded, FProgBarRoundCorners, FProgBarFrameProgress, FProgBarShowOverMaxIndicator, FProgBarLevel1Color, FProgBarLevel1TextColor,
                FProgBarLevel2Color, FProgBarLevel2Percent, FProgBarLevel2TextColor, FProgBarLevel3Color, FProgBarLevel3Percent, FProgBarLevel3TextColor,
                FProgBarShowValueInBar, Enabled, _EMFExporting);
              tmp:='';
              TargetCanvas.Font := Font;
              if not Enabled then
                TargetCanvas.Font.Color:=clGrayText;
            end;
          end;
      else
          begin
            // cgsCustom, cgsText - handled bellow
          end;
      end;
      if Length(tmp)>0 then
      begin
        InflateRect(R2,-2*FSpacing,0);
        FDrawTextFunc(TargetCanvas.Handle, tmp, R2, Flags, TargetCanvas.Font.Color, GlowSize);
      end;

    end;
  end;

  // draw criteria
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[FCriteriaAlignment] or DT_END_ELLIPSIS or DT_VCENTER);
  cFlags := DrawTextBiDiModeFlags(DT_SINGLELINE or DrawTextAlignments[FCategoryAlignment] or DT_END_ELLIPSIS);
  for i:=0 To FCriterias.Count-1 do
  begin
    C:=TDecoCompareGridCriterium(FCriterias.Items[i]);

    if FShowCategories and C.IsFirstInCategory then
    begin
      R2:=C.FCatRect;
      R2.Right:=FFrameRect.Right;
      TargetCanvas.Brush.Style := bsSolid;
      if FCategoryColor=clNone then
        TargetCanvas.Brush.Color := Color
      else
        TargetCanvas.Brush.Color := FCategoryColor;
      TargetCanvas.FillRect(R2);

      TargetCanvas.Font := CaptionFont;
      if not Enabled then
        TargetCanvas.Font.Color:=clGrayText;
      TargetCanvas.Brush.Style := bsClear;
      tmp:=C.Category;
      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, TargetCanvas, nil, C, nil, R2, True, tmp);
      InflateRect(R2,-2*FSpacing,0);
      if Length(tmp)>0 then
      begin
        if C.FCatRect.Bottom-C.FCatRect.Top>C.ItemRect.Bottom-C.ItemRect.Top+FSpacing then
        begin
          Dec(R2.Bottom,FSpacing);
          FDrawTextFunc(TargetCanvas.Handle, tmp, R2, cFlags or DT_BOTTOM, TargetCanvas.Font.Color, GlowSize)
        end
        else
          FDrawTextFunc(TargetCanvas.Handle, tmp, R2, cFlags or DT_VCENTER, TargetCanvas.Font.Color, GlowSize);
      end;

      if hdrLn or (FShowVertGrid and ((FGridLineWidth>0) and (FGridLineColor<>clNone))) then
      begin
        j:=((FFrameWidth+1) div 2);
        if hdrLn then
        begin
          z:=(FFrameWidth) div 2;
          SetRect(R2,C.FCatRect.Left+j,C.FCatRect.Top+z-1+(FFrameWidth mod 2),FFrameRect.Right-j-1,C.FCatRect.Bottom-z-1);
        end
        else
        if FShowHorzGrid then
        begin
          z:=(FGridLineWidth) div 2;
          SetRect(R2,C.FCatRect.Left+j,C.FCatRect.Top+z-1+(FGridLineWidth mod 2),FFrameRect.Right-j-1,C.FCatRect.Bottom-z-1)
        end;
        if hdrLn or (not FShowHorzGrid) then
        begin
          R2:=C.FCatRect;
          R2.Right:=FFrameRect.Right;
          if hdrLn then
          begin
            TargetCanvas.Pen.Style:= psSolid;
            TargetCanvas.Pen.Width:= FFrameWidth;
            TargetCanvas.Pen.Color := FFrameColor;
          end
          else
          begin
            TargetCanvas.Pen.Style:= FGridLineStyle;
            TargetCanvas.Pen.Width:= FGridLineWidth;
            TargetCanvas.Pen.Color := FGridLineColor;
          end;
          TargetCanvas.Polyline([point(R2.Left,R2.Top-1), point(R2.Right,R2.Top-1)]);
          TargetCanvas.Polyline([point(R2.Left,R2.Bottom-1), point(R2.Right,R2.Bottom-1)]);
          SetRect(R2,C.FCatRect.Left+j,C.FCatRect.Top,FFrameRect.Right-j-1,C.FCatRect.Bottom);
        end;
        ExcludeClipRect(TargetCanvas.Handle,R2.Left,R2.Top,R2.Right,R2.Bottom);
      end;
    end;

    R:=C.ItemRect;
    if FStripeRows and (FFixedColor<>clNone) and (i mod 2 = 0) then
    begin
      R2:=R;
      TargetCanvas.Brush.Style := bsSolid;
      TargetCanvas.Brush.Color := FFixedColor;
      TargetCanvas.FillRect(R2);
    end;

    TargetCanvas.Font := FCriteriaFont;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;
    TargetCanvas.Brush.Style := bsClear;
    if C=FHoverCell then
      TargetCanvas.Font.Style:=TargetCanvas.Font.Style+FHoverStyle;
    tmp:=C.Caption;
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, TargetCanvas, nil, C, nil, R, False, tmp);
    InflateRect(R,-2*FSpacing,0);
    if Length(tmp)>0 then
    begin
      if FShowCategories and (Length(C.Category)>0) and (FCategoryAlignment=taLeftJustify) then
        Inc(R.Left,FCategoryIndent);
      FDrawTextFunc(TargetCanvas.Handle, tmp, R, Flags, TargetCanvas.Font.Color, GlowSize);
    end
  end;

  // draw grid lines
  if ((FGridLineWidth>0) and (FGridLineColor<>clNone)) and (FShowVertGrid or FShowHorzGrid) then
  begin
    TargetCanvas.Pen.Style:= FGridLineStyle;
    TargetCanvas.Pen.Width:= FGridLineWidth;
    TargetCanvas.Pen.Color := FGridLineColor;
    if FShowVertGrid and (FItems.Count>0) then
      for j:=0 To FItems.Count-1 do
      begin
        A:=TDecoCompareGridItem(FItems.Items[j]);
        TargetCanvas.Polyline([point(A.ItemRect.Left-1,FFrameRect.Top),point(A.ItemRect.Left-1,FFrameRect.Bottom - (FGridLineWidth div 2))]);
      end;
    if FShowHorzGrid and (FCriterias.Count>0) then
      for i:=0 To FCriterias.Count-1 do
      begin
        C:=TDecoCompareGridCriterium(FCriterias.Items[i]);
        if FShowCategories and C.IsFirstInCategory and (not hdrLn) then
          TargetCanvas.Polyline([point(C.FCatRect.Left,C.FCatRect.Top-1), point(FFrameRect.Right - (FGridLineWidth div 2),C.FCatRect.Top-1)]);
        TargetCanvas.Polyline([point(C.ItemRect.Left,C.ItemRect.Top-1), point(FFrameRect.Right - (FGridLineWidth div 2),C.ItemRect.Top-1)]);
      end;
  end;

  // draw frame around
  if (FFrameWidth>0) and (FFrameColor<>clNone) then
  begin
    TargetCanvas.Pen.Style:= psSolid;
    TargetCanvas.Brush.Style := bsClear;
    TargetCanvas.Pen.Width:= FFrameWidth;
    TargetCanvas.Pen.Color := FFrameColor;

    if FShowGuideHeader then
      TargetCanvas.Polyline([point(FGuideHeader.Left,FGuideHeader.Bottom), point(FGuideHeader.Right-1,FGuideHeader.Bottom)]);

    if FShowGuideFooter then
      TargetCanvas.Polyline([point(FGuideFooter.Left,FGuideFooter.Top), point(FGuideFooter.Right-1,FGuideFooter.Top)]);

    if FHeadGridLine then
    begin
      if FItems.Count>0 then
        TargetCanvas.Polyline([point(FCaptionRect.Right-1,FFrameRect.Top),point(FCaptionRect.Right-1,FFrameRect.Bottom-1)]);
      if FCriterias.Count>0 then
        TargetCanvas.Polyline([point(FFrameRect.Left,FCaptionRect.Bottom-1), point(FFrameRect.Right-1,FCaptionRect.Bottom-1)]);
    end;

    if FFrameRoundCorners>0 then
      {$IF CompilerVersion >= 20.0}
      TargetCanvas.RoundRect(FFrameRect, FFrameRoundCorners, FFrameRoundCorners)
      {$ELSE}
      TargetCanvas.RoundRect(FFrameRect.Left, FFrameRect.Top, FFrameRect.Right, FFrameRect.Bottom, FFrameRoundCorners, FFrameRoundCorners)
      {$IFEND}
    else
      TargetCanvas.Rectangle(FFrameRect);
  end;

end;

end.
