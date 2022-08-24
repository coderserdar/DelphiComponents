{------------------------------------------------------------------------------
  DecoProgressGrid.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoProgressGrid component implements a specialized grid
              of progress bars and their additional (supporting) data.
              Such a grid is a perfect for presenting a various kinds
              of budgets, KPI values, and other data that need to be displayed
              in a percentage form. Of course, it can also be used as a simple
              progress bar, if needed.
              TDecoProgressGrid is lightweight and easy to setup, so you don't
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

  Version 1.0 (2011-07-17)
  - The first release
}

{ This unit contains TCustomDecoProgressGrid and TDecoProgressGrid components declaration and implementation. }
unit DecoProgressGrid;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, Forms, SysUtils,
  DecoCommon;

type
  TCustomDecoProgressGrid = class;
  { A kind of column within a grid. pgcCustom kind allows you to display your own texts or values in a grid (via OnDrawCell event). }
  TPGColumnKind = (pgcCaption, pgcProgressBar, pgcValue, pgcMinValue, pgcMaxValue, pgcDifference, pgcPercent, pgcCustom);
  { A position of progress bar in a grid row.<br>pgbInline displays the progress bar as if it was a normal grid cell.<br>
    pgbBottom and pgbTop force the progress bar to be displayed at the bottom or top of the whole grid row. }
  TPGBarPosition = (pgbInline, pgbBottom, pgbTop);

  { Represents a column in a grid. You can control the data, that will be displayed in a column, by setting up the Kind property. }
  TDecoProgressGridColumn = class(TCollectionItem)
  private
    FKind: TPGColumnKind;
    FCaption: string;
    FWidth: Integer;
    FVisible: Boolean;
    FAlignment: TAlignment;
    FColRect: TRect;
  protected
    function GetDisplayName: string; override;
    procedure SetKind(Value: TPGColumnKind); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual;
    procedure SetAlignment(Value: TAlignment); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    // A calculated rectangle for drawing the column
    property ColRect:TRect read FColRect write FColRect;
  published
    { A kind of column within a grid. You can choose from all the supporting data kinds, that affects the definition of a progress bar,
      such as minimal value, maximal value, current value, percentage and the difference. Of course you can setup the column to display
      the progress bar itself, and last but not least, you can display your own texts or values by using the pgcCustom kind together with an OnDrawCell event.
      A maximum of one column of pgcProgressBar kind can be defined within the columns collection. }
    property Kind: TPGColumnKind read FKind write SetKind default pgcValue;
    { A caption of column, a label in the header. A special value of '%%' is available for progress bar columns (Kind=pgcProgressBar),
      that will display a 0%..100% label for this column. It is valid only when BarPosition property is set to pgbInline. }
    property Caption: string read FCaption write SetCaption;
    { Controls the visibility of a column }
    property Visible: Boolean read FVisible write SetVisible default True;
    { A width of the column. When the BarPosition property of the owner component is set to pgbBottom or pgbTop, the column width for progress bar column is ignored,
      and the width for first visible column is taken as a minimal width. When the BarPosition is set to pgbInline, the width of progress bar column is taken as minimal width. }
    property Width: Integer read FWidth write SetWidth default 64;
    { A text alignment for all the cells within the column, including the header }
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
  end;

  { A columns collection. Use it for defining the columns of a grid. See also TDecoProgressGridColumn. }
  TDecoProgressGridColumns = class(TCollection)
  private
    FOwner: TPersistent;
    FDecoProgressGrid: TCustomDecoProgressGrid;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    function GetItem(Index: Integer): TDecoProgressGridColumn; virtual;
    procedure SetItem(Index: Integer; Value: TDecoProgressGridColumn); virtual;
  public
    // A collection constructor. It is called automatically by the owner component.
    constructor Create(AOwner: TPersistent);
    // Adds a new column
    function Add: TDecoProgressGridColumn;
    // Inserts a new column at specified position
    function Insert(Index:Integer): TDecoProgressGridColumn;
    // Deletes a column specified by the index
    procedure Delete(Index: Integer);
    // Moves a column to another position in the collection. Moved column is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    // Supporting method, that returns True, if a column of kind pgcProgressBar exists (no matter if visible or not)
    function BarColumnExists:Boolean;
    // Reference to the TCustomDecoProgressGrid component that owns the columns collection
    property DecoProgressGrid: TCustomDecoProgressGrid read FDecoProgressGrid;
    // Access an individual columns through this property
    property Items[Index: Integer]: TDecoProgressGridColumn read GetItem write SetItem; default;
  end;

  { Represents a row within a grid. More precisely - it holds the data for a row in a grid.
    The progress bar drawing and all the displayed values (both static and auto-calculated) are based on the properties of this class.
    The essential properties of TDecoProgressGridItem are almost identical to a classic progress bar,
    so you can find the MinValue, MaxValue and Value properties here. But in this component you can freely go outside the minimum
    and maximum values (see the ShowOverMaxIndicator property). The supporting properties, such as Caption,
    are useful for displaying an additional info (in other columns) within the grid, if needed. }
  TDecoProgressGridItem = class(TCollectionItem)
  private
    FCaption: string;
    FHint: string;
    FTag: Longint;
    FData: Pointer;
    FValue: Extended;
    FMinValue: Extended;
    FMaxValue: Extended;
    FVisible: Boolean;
    // Calculated bounds of an item
    FItemRect: TRect;
  protected
    function GetDisplayName: string; override;
    procedure SetCaption(const Value: string); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetValue(NewValue: Extended); virtual;
    procedure SetMinValue(NewValue: Extended); virtual;
    procedure SetMaxValue(NewValue: Extended); virtual;
    procedure SetVisible(Value: Boolean); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    // Any custom data reference
    property Data: Pointer read FData write FData;
    // Current bounds of an item (a row in the grid)
    property ItemRect: TRect read FItemRect;
  published
    // Caption of the item
    property Caption: string read FCaption write SetCaption;
    // Hint for the item
    property Hint: string read FHint write SetHint;
    { A minimum value of the range for the progress bar, but not for the Value of this item.
      In other words, the MinValue property indicates a 0% treshold. See also MaxValue, Value, ShowOverMaxIndicator. }
    property MinValue: Extended read FMinValue write SetMinValue;
    { A maximum value of the range for the progress bar, but not for the Value of this item.
      In other words, the MaxValue property indicates a 100% treshold. See also MinValue, Value, ShowOverMaxIndicator. }
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    { Current value of the item. If Value is in the range of MinValue and MaxValue, the progress bar will be displayed as usual.
      If Value is greater than MaxValue, or lower than MinValue, you can use the ShowOverMaxIndicator property to display a dividing line
      in the progress bar, that indicates the percentage of exceeding the maximum (or minimum) value. }
    property Value: Extended read FValue write SetValue;
    // Show or hide the item (a row) by using this property
    property Visible: Boolean read FVisible write SetVisible default True;
    // Any custom tag value (integer type)
    property Tag: Longint read FTag write FTag default 0;
  end;

  // Event handler for custom sorting. If no handler is assigned, internal sorting will be used (by the caption in an ascending order).
  TDecoPGCompareEvent = function(Sender:TObject; Item1, Item2:TDecoProgressGridItem):integer of object;

  { Collection of grid rows (progress bars). See the description of TDecoProgressGridItem for more info. }
  TDecoProgressGridItems = class(TCollection)
  private
    FOwner: TPersistent;
    FDecoProgressGrid: TCustomDecoProgressGrid;
    // Direct reference to the private FItems field of ancestor (TCollection). Used for fast sorting of items (see UseSpeedSort property).
    FItemsRef: TList;
    FSorting: Boolean;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    {$IF CompilerVersion >= 15}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Deleting(Item: TCollectionItem);
    {$IFEND}
    function GetItem(Index: Integer): TDecoProgressGridItem; virtual;
    procedure SetItem(Index: Integer; Value: TDecoProgressGridItem); virtual;
    procedure QuickSort(L, R: Integer);
  public
    constructor Create(AOwner: TPersistent);
    // Creates and adds a new item to the collection
    function Add: TDecoProgressGridItem;
    { Allows you to add a pre-filled new item to the collection }
    function AddItem(const Caption:string; Value:Extended=0; MinValue:Extended=0; MaxValue:Extended=100;
        const Hint:string=''; Tag:Longint=0; Data:Pointer=nil): TDecoProgressGridItem;
    // Creates and inserts a new item to the specified position in the collection
    function Insert(Index:Integer): TDecoProgressGridItem;
    // Deletes an item specified by the index
    procedure Delete(Index: Integer);
    // Moves an item to another position in the collection. Moved item is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    // Sorts the items collection alpabetically (or with the help of custom event handler - OnCompareItems)
    procedure Sort; virtual;
    // Reference to the owner component
    property DecoProgressGrid: TCustomDecoProgressGrid read FDecoProgressGrid;
    // Through this property you can access the individual items (sections)
    property Items[Index: Integer]: TDecoProgressGridItem read GetItem write SetItem; default;
  end;

  // Common event handler type, used for various item events (OnSelectItem, OnHoverChange, OnItemClick, etc.)
  TDecoProgressGridItemEvent = procedure(Sender:TObject; Item:TDecoProgressGridItem) of object;
  // Type definition of an OnDrawCell event handler
  TDecoProgressGridDrawCell = procedure(Sender:TObject; TargetCanvas:TCanvas; Item:TDecoProgressGridItem;
      ColumnIndex: Integer; CellRect:TRect; var Text:string) of object;

  // Hit test kind
  TDecoPGHitTestInfoKind = (pgiNoWhere,pgiCaption,pgiColumnsHeader,pgiItem,pgiBar,pgiValueInBar);
  // Hit test info record, a result of GetHitTestInfoAt method
  TDecoPGHitTestInfo = record
    // Info kind
    Kind:TDecoPGHitTestInfoKind;
    // Item reference, when Kind is pgiItem, pgiBar or pgiValueInBar
    Item:TDecoProgressGridItem;
    // Index of column, when Kind is pgiColumnsHeader, pgiItem, pgiBar or pgiValueInBar
    ColumnIndex:Integer;
    // An appropriate value of progress bar at hitting position, when Kind is pgiBar or pgiValueInBar
    BarValue:Extended;
  end;

  { A base class for all DecoProgressGrid visual component implementations. See also TDecoProgressGrid, TDecoProgressGridPlus. }
  TCustomDecoProgressGrid = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FBackColor: TColor;
    FBarPosition: TPGBarPosition;
    FBorderStyle: TBorderStyle;
    FCaptionFont: TFont;
    FColumns: TDecoProgressGridColumns;
    FFrameColor: TColor;
    FFrameProgress: Boolean;
    FFrameWidth: Integer;
    FGlowSize: Integer;
    FGradient: TPGGradientStyle;
    FGridLineColor: TColor;
    FGridLineStyle: TPenStyle;
    FHoverCursor: TCursor;
    FHoverItem: TDecoProgressGridItem;
    FItems: TDecoProgressGridItems;
    FLevel1Color: TColor;
    FLevel1TextColor: TColor;
    FLevel2Color: TColor;
    FLevel2Percent: Integer;
    FLevel2TextColor: TColor;
    FLevel3Color: TColor;
    FLevel3Percent: Integer;
    FLevel3TextColor: TColor;
    FOnAfterPaint: TDecoPaintEvent;
    FOnBeforePaint: TDecoPaintEvent;
    FOnCompareItems: TDecoPGCompareEvent;
    FOnDrawCell: TDecoProgressGridDrawCell;
    FOnHoverChange: TDecoProgressGridItemEvent;
    FOnItemClick: TDecoProgressGridItemEvent;
    FOnSelectItem: TDecoProgressGridItemEvent;
    FParentBackgroundSet: Boolean;
    FRoundCorners: Integer;
    FRounded: Boolean;
    FRowHeight: Integer;
    FSelectedItem: TDecoProgressGridItem;
    FShowCaption: Boolean;
    FShowColumnsHeader: Boolean;
    FShowFirstGridLine: Boolean;
    FShowFocusRect: Boolean;
    FShowHorzGrid: Boolean;
    FShowLastGridLine: Boolean;
    FShowOverMaxIndicator: Boolean;
    FShowValueInBar: TShowValueInBar;
    FShowVertGrid: Boolean;
    FSorted: Boolean;
    FTextMargin: Integer;
    FValueTextFormat: string;
    {$IF CompilerVersion >= 20}
    procedure DoPaddingChange(Sender: TObject);
    {$IFEND}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: {$IF CompilerVersion >= 20} TCMParentFontChanged {$ELSE} TMessage {$IFEND}); message CM_PARENTFONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure CaptionFontChange(Sender: TObject);
    function IsColumnsStored: Boolean;
    function IsItemsStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBackColor(Value: TColor);
    procedure SetBarPosition(Value: TPGBarPosition);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetColumns(const Value: TDecoProgressGridColumns);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameProgress(Value: Boolean);
    procedure SetFrameWidth(Value: Integer);
    procedure SetGlowSize(Value: Integer);
    procedure SetGradient(Value: TPGGradientStyle);
    procedure SetGridLineColor(Value: TColor);
    procedure SetGridLineStyle(Value: TPenStyle);
    procedure SetHoverItem(const Value:TDecoProgressGridItem);
    procedure SetItems(const Value: TDecoProgressGridItems);
    procedure SetLevel1Color(Value: TColor);
    procedure SetLevel1TextColor(Value: TColor);
    procedure SetLevel2Color(Value: TColor);
    procedure SetLevel2Percent(Value: Integer);
    procedure SetLevel2TextColor(Value: TColor);
    procedure SetLevel3Color(Value: TColor);
    procedure SetLevel3Percent(Value: Integer);
    procedure SetLevel3TextColor(Value: TColor);
    procedure SetOnAfterPaint(Value: TDecoPaintEvent);
    procedure SetOnBeforePaint(Value: TDecoPaintEvent);
    procedure SetOnCompareItems(Value: TDecoPGCompareEvent);
    procedure SetOnDrawCell(Value: TDecoProgressGridDrawCell);
    procedure SetRoundCorners(Value: Integer);
    procedure SetRounded(Value: Boolean);
    procedure SetRowHeight(Value: Integer);
    procedure SetSelectedItem(const Value:TDecoProgressGridItem);
    procedure SetShowCaption(Value: Boolean);
    procedure SetShowColumnsHeader(Value: Boolean);
    procedure SetShowFirstGridLine(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetShowHorzGrid(Value: Boolean);
    procedure SetShowLastGridLine(Value: Boolean);
    procedure SetShowOverMaxIndicator(Value: Boolean);
    procedure SetShowValueInBar(Value: TShowValueInBar);
    procedure SetShowVertGrid(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetTextMargin(Value: Integer);
    procedure SetValueTextFormat(const Value: string);
  protected
    // Temporary variable, that indicates a drawing onto the vector based canvas (ie. TMetafileCanvas). It is used in PrintTo method.
    _EMFExporting:Boolean;
    // A temporary variable that indicates if the component has a focus
    FActive:Boolean;
    // Calculated rectangle that covers all currently displayed items (this means the data rows except the title (Caption) and the column headers row)
    FBarsRect: TRect;
    // Calculated rectangle for component title (Caption)
    FCaptionRect: TRect;
    // Calculated rectangle for column headers (captions of columns)
    FColumnsHeaderRect: TRect;
    // Reference to the helper function for drawing the texts
    FDrawTextFunc:TDrawTextFunc;
    // See IsCaptionHovered property
    FIsCaptionHovered: Boolean;
    // See IsColumnsHeaderHovered property
    FIsColumnsHeaderHovered: Boolean;
    // Temporary variable that indicates the need of recalculating the drawing rectangles (FBarsRect, FCaptionRect, item rectangles, etc.)
    FRebuildNeeded: Boolean;
    { Helper variable - a margin between texts and graphics elements. It is based on a TextMargin property,
      or (if TextMargin is set to -1) the currently selected font metrics. }
    FSpacing: Integer;
    // Returns the calculated bounds of the component - used when AutoSize property is True.
    function _GetControlSize:TSize; virtual;
    // Returns the rounding factor of progress bar corners, when Rounded property is True. The result is based on RowHeight and RoundCorners properties.
    function _GetRoundedCornersFactor:Integer;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    // A base method for drawing the component's title (Caption). You can override it in an inherited components.
    procedure DrawCaption(TargetCanvas:TCanvas); virtual;
    // A base method for drawing the columns header. You can override it in inherited components.
    procedure DrawColumnsHeader(TargetCanvas:TCanvas); virtual;
    // A base method for drawing the grid lines (see also GridLineColor, GridLineStyle, ShowHorzGrid, ShowVertGrid, ShowFirstGridLine and ShowLastGridLine properties)
    procedure DrawGrid(TargetCanvas:TCanvas); virtual;
    // A base method for drawing the data rows (progress bars and text cells)
    procedure DrawItems(TargetCanvas:TCanvas); virtual;
    { Handling of keyboard shortcuts available for this component. }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // Base method for drawing the component, that calls other methods to draw the bars, texts, etc.
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetParentBackground(Value: Boolean); override;
    // Method for setting the current HoverItem based on passed X and Y coordinates. This method sets also FIsCaptionHovered and FIsColumnsHeaderHovered variables.
    procedure UpdateHoverInfo(X, Y: Integer);
    // Caption alignment. See also Caption and ShowCaption properties.
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    // Background color of progress bars. You can set the value to clNone, to make the progress bars transparent.
    property BackColor: TColor read FBackColor write SetBackColor default clBtnHighlight;
    { A position of progress bar in a grid row.<br>pgbInline displays the progress bar as if it was a normal grid cell.<br>
      pgbBottom and pgbTop force the progress bar to be displayed at the bottom or top of the whole grid row. }
    property BarPosition: TPGBarPosition read FBarPosition write SetBarPosition default pgbInline;
    // Border style of the component
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    // Font for the chart title (Caption)
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    // Background color of the chart. See also ParentBackground.
    property Color default clBtnFace;
    // Frame color for the progress bars. You can set the value to clNone, to remove the frame. See also FrameWidth, FrameProgress.
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnFace;
    // If True, the progress bars with percentage greater than 0, will be painted with a frame of a slightly darker color. See also FrameColor, FrameWidth.
    property FrameProgress: Boolean read FFrameProgress write SetFrameProgress default False;
    // A width of the progress bars frame. See also FrameColor, FrameProgress.
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
    // By this property you can set the gradient style of progress bars.
    property Gradient: TPGGradientStyle read FGradient write SetGradient default pggVertical;
    // A color of the grid lines. See also GridLineStyle, ShowHorzGrid, ShowVertGrid, ShowFirstGridLine and ShowLastGridLine properties.
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clBtnFace;
    // A style of the grid lines. See also GridLineColor, ShowHorzGrid, ShowVertGrid, ShowFirstGridLine and ShowLastGridLine properties.
    property GridLineStyle: TPenStyle read FGridLineStyle write SetGridLineStyle default psDot;
    // The cursor for hovered items (rows)
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor default crDefault;
    // A color of the progress bar, when the Value is lower than the Level2Percent limit.
    property Level1Color: TColor read FLevel1Color write SetLevel1Color default $24CA8B;
    // A color of the text inside the progress bar, when the Value is lower than the Level2Percent limit. See also ShowValueInBar property.
    property Level1TextColor: TColor read FLevel1TextColor write SetLevel1TextColor default clBlack;
    // A color of the progress bar, when the Value is in the range from Level2Percent to Level3Percent.
    property Level2Color: TColor read FLevel2Color write SetLevel2Color default $00BFFF;
    // A limit (in percent) for displaying the progress bar in colors defined by Level1Color and Level1TextColor properties.
    property Level2Percent: Integer read FLevel2Percent write SetLevel2Percent default 80;
    // A color of the text inside the progress bar, when the Value is in the range from Level2Percent to Level3Percent. See also ShowValueInBar property.
    property Level2TextColor: TColor read FLevel2TextColor write SetLevel2TextColor default clBlack;
    // A color of the progress bar, when the Value is greater than Level3Percent limit.
    property Level3Color: TColor read FLevel3Color write SetLevel3Color default $3437D9;
    // A limit (in percent) for displaying the progress bar in colors defined by Level2Color and Level2TextColor properties.
    property Level3Percent: Integer read FLevel3Percent write SetLevel3Percent default 105;
    // A color of the text inside the progress bar, when the Value is greater than Level3Percent limit. See also ShowValueInBar property.
    property Level3TextColor: TColor read FLevel3TextColor write SetLevel3TextColor default clWhite;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint: TDecoPaintEvent read FOnAfterPaint write SetOnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint: TDecoPaintEvent read FOnBeforePaint write SetOnBeforePaint;
    { Event handler, that allows you to perform a custom sorting of grid items. }
    property OnCompareItems: TDecoPGCompareEvent read FOnCompareItems write SetOnCompareItems;
    { This event handler has two purposes. First, you can use it to assign a custom texts for individual cells, mainly for pgcCustom kinds of columns.
      Secondly, you can perform your own drawing to the passed TargetCanvas for each cell.<br><br>
      Warning: For GDI+ descendants, you have to use a public GDIPCanvas variable to draw onto, but you can use the TargetCanvas.Handle to draw the texts.
               See the implementation of TDecoProgressGridPlus.DrawItems method as an inspiration. }
    property OnDrawCell: TDecoProgressGridDrawCell read FOnDrawCell write SetOnDrawCell;
    { This event allows you to process your own actions when an item (row) under the mouse is changed.
      Item parameter can have a nil value, when no item is hovered. }
    property OnHoverChange: TDecoProgressGridItemEvent read FOnHoverChange write FOnHoverChange;
    { This event allows you to process your own actions when an item (row) is clicked.
      Item parameter has always a value different from nil. If the control is clicked and no item is hovered,
      only an OnClick event is fired. }
    property OnItemClick: TDecoProgressGridItemEvent read FOnItemClick write FOnItemClick;
    { This event allows you to process your own actions when the SelectedItem property value is changed.
      Item parameter can have a nil value, when no item is selected. }
    property OnSelectItem: TDecoProgressGridItemEvent read FOnSelectItem write FOnSelectItem;
    { Set this property to True to draw the component's background as transparent.
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoProgressGridPlus) it is. }
    property ParentBackground stored FParentBackgroundSet default False;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor default False;
    { Setup this property to non-zero, if you want to use your own rounded corners factor.
      If you set the Rounded property to True and the RoundCorners property is 0, an internal rounded corners factor will be used, based on the RowHeight property.
      See also Rounded property. }
    property RoundCorners: Integer read FRoundCorners write SetRoundCorners default 0;
    // If False, the bars will be displayed as standard rectangles. If True, the bars will be displayed with rounded corners. See also RoundCorners property.
    property Rounded: Boolean read FRounded write SetRounded default True;
    { A height of the progress bars. In a standard grid layout (see BarPosition property) this means also the base height of the grid row.
      But be aware that the resulting height of the grid's row is calculated automatically, based on the RowHeight and BarPosition properties and on the metrics
      of currently selected font. }
    property RowHeight: Integer read FRowHeight write SetRowHeight default 20;
    // If True, the title of the chart (Caption) will be displayed. See also Alignment and Caption properties.
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    // If True, a row with column headers (column captions) will be displayed. See also Columns.
    property ShowColumnsHeader: Boolean read FShowColumnsHeader write SetShowColumnsHeader default True;
    // If False, the gradient line will not be painted for the first grid row and for the first grid column. See also GridLineColor, GridLineStyle, ShowHorzGrid, ShowVertGrid and ShowLastGridLine properties.
    property ShowFirstGridLine: Boolean read FShowFirstGridLine write SetShowFirstGridLine default True;
    // If True, a focus rectangle will be drawn around the currently focused item (row).
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    // Shows or hides the horizontal grid lines. See also GridLineColor, GridLineStyle, ShowVertGrid, ShowFirstGridLine and ShowLastGridLine properties.
    property ShowHorzGrid: Boolean read FShowHorzGrid write SetShowHorzGrid default True;
    // If False, gradient line will not be painted for the last grid row and for the last grid column. See also GridLineColor, GridLineStyle, ShowHorzGrid, ShowVertGrid and ShowFirstGridLine properties.
    property ShowLastGridLine: Boolean read FShowLastGridLine write SetShowLastGridLine default True;
    { If True, a thin dividing line will be displayed in all progress bars, that have the Value greater than MaxValue, or lower than MinValue.
      This line indicates the percentage of exceeding the maximum (or minimum) value. }
    property ShowOverMaxIndicator: Boolean read FShowOverMaxIndicator write SetShowOverMaxIndicator default True;
    { This property defines a kind of text displayed inside the progress bar. It can be a current value, percentage or no text. }
    property ShowValueInBar: TShowValueInBar read FShowValueInBar write SetShowValueInBar default ibvValue;
    // Shows or hides the vertical grid lines. See also GridLineColor, GridLineStyle, ShowHorzGrid, ShowFirstGridLine and ShowLastGridLine properties.
    property ShowVertGrid: Boolean read FShowVertGrid write SetShowVertGrid default False;
    // If True, the items will be automatically sorted. See also Sort method and OnCompareItems event).
    property Sorted: Boolean read FSorted write SetSorted nodefault;
    // Margin for texts in cells. When the value is -1, the margin is calculated automatically based on the selected font.
    property TextMargin: Integer read FTextMargin write SetTextMargin default 2;
    { This property defines a format string for displaying the values (Value, MinValue, MaxValue, Difference).
      Any formatting options for standard Delphi Format function, that are dedicated to floating-point numbers, are allowed.
      So, for example the '%.2n' format string results to displaying the value as '100,000.00'. }
    property ValueTextFormat: string read FValueTextFormat write SetValueTextFormat;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    { This method calculates the drawing rectangles (and other aspects) of each displayed element and stores the values to the temporary variables (a cache).
      The repainting of the component itself is then faster.
      This method is called when the values are modified, on resizing the component, etc. }
    procedure Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil); virtual;
    { Returns a formatted Value, based on the ValueTextFormat property. }
    function GetValueFormatted(Value:Extended):string; virtual;
    // Returns the current item (row) at X, Y position
    function GetItemAt(X,Y:Integer):TDecoProgressGridItem; virtual;
    { A hit-test info method, that provides you with a complex information about the element on the X,Y position.
      Refer to the TDecoPGHitTestInfo for description of the returned fields, and look at the main demo project for DecoProgressGrid,
      to see the method in action (it is used in OnMouseDown, OnMouseMove, OnMouseUp events to support interactive changing of values via mouse). }
    function GetHitTestInfoAt(X,Y:Integer):TDecoPGHitTestInfo; virtual;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destination rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas).
      Refer to the main DecoProgressGrid demo project, to see how to use this method to save/export the grid into a graphic formats. }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); virtual;
    // Sorts the items. Use this method for one time sorting when the Sorted property is False. See also OnCompareItems event.
    procedure Sort;
    { A glow size attribut for drawing texts (title, captions, values, etc.).
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoProgressGridPlus) it is. }
    property GlowSize: Integer read FGlowSize write SetGlowSize default 0;
    // Temporary property, that indicates if the component has a focus
    property Active: Boolean read FActive;
    { A columns collection. Use it for defining the columns of a grid. See also TDecoProgressGridColumn. }
    property Columns: TDecoProgressGridColumns read FColumns write SetColumns stored IsColumnsStored;
    { Collection of grid rows (progress bars). See the description of TDecoProgressGridItem for more info. }
    property Items: TDecoProgressGridItems read FItems write SetItems stored IsItemsStored;
    // Identifies the item (a row) under the mouse cursor.
    property HoverItem: TDecoProgressGridItem read FHoverItem write SetHoverItem;
    { Identifies currently selected item (a row). Item can be selected by clicking the mouse, or by using the keyboard. }
    property SelectedItem: TDecoProgressGridItem read FSelectedItem write SetSelectedItem;
    // Read this property to check if the mouse is currently positioned above the component's title (Caption)
    property IsCaptionHovered: Boolean read FIsCaptionHovered;
    // Read this property to check if the mouse is currently positioned above the column headers row
    property IsColumnsHeaderHovered: Boolean read FIsColumnsHeaderHovered;
  end;

  { TDecoProgressGrid component implements a specialized grid of progress bars and their additional (supporting) data.
    Such a grid is a perfect for presenting a various kinds of budgets, KPI values, and other data that need to be displayed
    in a percentage form. Of course, it can also be used as a simple progress bar, if needed.
    TDecoProgressGrid is lightweight and easy to setup, so you don't have to use some universal (and resource wasteful) grid components
    for displaying this kind of charts or tables. }
  TDecoProgressGrid = class(TCustomDecoProgressGrid)
  published
    // Setups the align in the parent control
    property Align;
     { See @inherited }
    property Alignment;
    // Component anchors
    property Anchors;
    { Controls auto-sizing of the component. }
    property AutoSize;
    { See @inherited }
    property BackColor;
    { See @inherited }
    property BarPosition;
    property BevelEdges;
    property BevelKind;
    // Bidirectional text drawing mode
    property BiDiMode;
    // Border style of the componet
    property BorderStyle;
    { A grid title. See also ShowCaption and CaptionFont. }
    property Caption;
    { See @inherited }
    property CaptionFont;
    { See @inherited }
    property Color;
    { See @inherited }
    property Columns;
    { Component size constraints }
    property Constraints;
    property Ctl3D;
    { Controls double-buffering of the component. TDecoProgressGrid component is always painted as double-buffered, but this property is important
      for other descendants (such as TDecoProgressGridPlus), that use a composited rendering. }
    property DoubleBuffered;
    // Defines the drag cursor
    property DragCursor;
    // Defines the drag kind
    property DragKind;
    // Defines the drag mode
    property DragMode;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (values, captions, etc.). See also CaptionFont.
    property Font;
    { See @inherited }
    property FrameColor;
    { See @inherited }
    property FrameProgress;
    { See @inherited }
    property FrameWidth;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property Gradient;
    { See @inherited }
    property GridLineColor;
    { See @inherited }
    property GridLineStyle;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property Items;
    { See @inherited }
    property Level1Color;
    { See @inherited }
    property Level1TextColor;
    { See @inherited }
    property Level2Color;
    { See @inherited }
    property Level2Percent;
    { See @inherited }
    property Level2TextColor;
    { See @inherited }
    property Level3Color;
    { See @inherited }
    property Level3Percent;
    { See @inherited }
    property Level3TextColor;
    { See @inherited }
    property OnAfterPaint;
    {$IF CompilerVersion >= 20}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFEND}
    { See @inherited }
    property OnBeforePaint;
    { OnCanResize event }
    property OnCanResize;
    { OnClick event }
    property OnClick;
    { See @inherited }
    property OnCompareItems;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    { See @inherited }
    property OnHoverChange;
    { See @inherited }
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IF CompilerVersion >= 18}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$IFEND}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    { See @inherited }
    property OnSelectItem;
    property OnStartDrag;
    {$IF CompilerVersion >= 20}
    // A padding for the component
    property Padding;
    {$IFEND}
    property ParentBiDiMode;
    { See @inherited }
    property ParentColor;
    property ParentCtl3D;
    {$IF CompilerVersion >= 20}
    { ParentDoubleBuffered property. See also DoubleBuffered property. }
    property ParentDoubleBuffered;
    {$IFEND}
    // Takes the font of its parent control
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    { See @inherited }
    property RoundCorners;
    { See @inherited }
    property Rounded;
    { See @inherited }
    property RowHeight;
    { See @inherited }
    property ShowCaption;
    { See @inherited }
    property ShowColumnsHeader;
    { See @inherited }
    property ShowFirstGridLine;
    { See @inherited }
    property ShowFocusRect;
    { If True, the hints will be shown when the mouse is above the control }
    property ShowHint;
    { See @inherited }
    property ShowHorzGrid;
    { See @inherited }
    property ShowLastGridLine;
    { See @inherited }
    property ShowOverMaxIndicator;
    { See @inherited }
    property ShowValueInBar;
    { See @inherited }
    property ShowVertGrid;
    { See @inherited }
    property Sorted nodefault;
    property TabOrder;
    property TabStop;
    { See @inherited }
    property TextMargin;
    { See @inherited }
    property ValueTextFormat;
    { Controls visibility of the control }
    property Visible;
  end;

implementation

uses
  DecoGDI {$IF CompilerVersion >= 24}, System.Types {$IFEND};

//////////////////////////////////////////// TDecoProgressGridColumn ////////////////////////////////////////////
constructor TDecoProgressGridColumn.Create(Collection: TCollection);
begin
  inherited;
  FKind := pgcValue;
  FCaption := '';
  FVisible := True;
  FWidth := 64;
  FAlignment := taLeftJustify;
  SetRectEmpty(FColRect);
end;

function TDecoProgressGridColumn.GetDisplayName: string;
begin
  Result:=FCaption;
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TDecoProgressGridColumn.Assign(Source: TPersistent);
begin
  if Source is TDecoProgressGridColumn then
  begin
    FCaption := TDecoProgressGridColumn(Source).FCaption;
    FVisible := TDecoProgressGridColumn(Source).FVisible;
    FWidth := TDecoProgressGridColumn(Source).FWidth;
    FAlignment := TDecoProgressGridColumn(Source).FAlignment;
    if (TDecoProgressGridColumn(Source).FKind=pgcProgressBar) and TDecoProgressGridColumns(Collection).BarColumnExists then
      FKind:=pgcValue
    else
      FKind := TDecoProgressGridColumn(Source).FKind;
    Changed(False);
  end
  else
    inherited;
end;

procedure TDecoProgressGridColumn.SetCaption(const Value: string);
begin
  if Value<>FCaption then
  begin
    FCaption:=Value;
    Changed(False);
  end;
end;

procedure TDecoProgressGridColumn.SetWidth(Value: integer);
begin
  if Value<>FWidth then
  begin
    FWidth:=Value;
    Changed(True);
  end;
end;

procedure TDecoProgressGridColumn.SetAlignment(Value: TAlignment);
begin
  if Value<>FAlignment then
  begin
    FAlignment:=Value;
    Changed(False);
  end;
end;

procedure TDecoProgressGridColumn.SetKind(Value: TPGColumnKind);
begin
  if Value<>FKind then
    if (Value<>pgcProgressBar) or (not TDecoProgressGridColumns(Collection).BarColumnExists) then
    begin
      FKind:=Value;
      Changed(True);
    end;
end;

procedure TDecoProgressGridColumn.SetVisible(Value: Boolean);
begin
  if (Value<>FVisible) then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

//////////////////////////////////////////////// TDecoProgressGridColumns ///////////////////////////////////////////////
constructor TDecoProgressGridColumns.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FDecoProgressGrid:=TCustomDecoProgressGrid(FOwner);
  inherited Create(TDecoProgressGridColumn);
end;

function TDecoProgressGridColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDecoProgressGridColumns.Update(Item: TCollectionItem);
begin
  if Item=nil then
    FDecoProgressGrid.FRebuildNeeded:=True;
  FDecoProgressGrid.Invalidate;
end;

function TDecoProgressGridColumns.GetItem(Index: Integer): TDecoProgressGridColumn;
begin
  Result := inherited GetItem(Index) as TDecoProgressGridColumn;
end;

procedure TDecoProgressGridColumns.SetItem(Index: Integer; Value: TDecoProgressGridColumn);
begin
  inherited SetItem(Index, Value);
end;

function TDecoProgressGridColumns.Add: TDecoProgressGridColumn;
begin
  Result := inherited Add as TDecoProgressGridColumn;
end;

function TDecoProgressGridColumns.Insert(Index:Integer): TDecoProgressGridColumn;
begin
  Result := inherited Insert(Index) as TDecoProgressGridColumn;
end;

procedure TDecoProgressGridColumns.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TDecoProgressGridColumns.Move(CurrIndex, NewIndex: Integer);
begin
  Items[CurrIndex].Index := NewIndex;
end;

function TDecoProgressGridColumns.BarColumnExists:boolean;
var
  i:Integer;
begin
  Result:=False;
  for i:=0 To Count-1 do
    if Items[i].Kind=pgcProgressBar then
    begin
      Result:=True;
      break;
    end;
end;

//////////////////////////////////////////// TDecoProgressGridItem ////////////////////////////////////////////
constructor TDecoProgressGridItem.Create(Collection: TCollection);
begin
  inherited;
  FTag := 0;
  FData := nil;
  FValue := 0;
  FMinValue := 0;
  FMaxValue := 100;
  FCaption := '';
  FVisible := True;
  SetRectEmpty(FItemRect);
end;

function TDecoProgressGridItem.GetDisplayName: string;
begin
  Result:=FCaption;
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TDecoProgressGridItem.Assign(Source: TPersistent);
begin
  if Source is TDecoProgressGridItem then
  begin
    FTag := TDecoProgressGridItem(Source).FTag;
    FData := TDecoProgressGridItem(Source).FData;
    FCaption := TDecoProgressGridItem(Source).FCaption;
    FVisible := TDecoProgressGridItem(Source).FVisible;
    FValue := TDecoProgressGridItem(Source).FValue;
    FMinValue := TDecoProgressGridItem(Source).FMinValue;
    FMaxValue := TDecoProgressGridItem(Source).FMaxValue;
    Changed(False);
  end
  else
    inherited;
end;

procedure TDecoProgressGridItem.SetCaption(const Value: string);
begin
  if Value<>FCaption then
  begin
    FCaption:=Value;
    Changed(False);
  end;
end;

procedure TDecoProgressGridItem.SetHint(const Value: string);
begin
  if Value<>FHint then
  begin
    FHint:=Value;
    Changed(False);
  end;
end;

procedure TDecoProgressGridItem.SetValue(NewValue: Extended);
begin
  if NewValue<>FValue then
  begin
    FValue:=NewValue;
    Changed(False);
  end;
end;

procedure TDecoProgressGridItem.SetMinValue(NewValue: Extended);
begin
  if NewValue<>FMinValue then
  begin
    FMinValue:=NewValue;
    if FMinValue>=FMaxValue then
      FMaxValue:=FMinValue+1;
    Changed(False);
  end;
end;

procedure TDecoProgressGridItem.SetMaxValue(NewValue: Extended);
begin
  if NewValue<>FMaxValue then
  begin
    FMaxValue:=NewValue;
    if FMaxValue<=FMinValue then
      FMinValue:=FMaxValue-1;
    Changed(False);
  end;
end;

procedure TDecoProgressGridItem.SetVisible(Value: Boolean);
begin
  if (Value<>FVisible) then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

//////////////////////////////////////////////// TDecoProgressGridItems ///////////////////////////////////////////////
constructor TDecoProgressGridItems.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TDecoProgressGridItem);
  FDecoProgressGrid:=TCustomDecoProgressGrid(FOwner);
  FSorting := False;
  if DecoUseSpeedSort then
    FItemsRef := GetCollectionList(Self)
  else
    FItemsRef := nil;
end;

function TDecoProgressGridItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDecoProgressGridItems.Update(Item: TCollectionItem);
begin
  if not FSorting then
  begin
    if Item=nil then
    begin
      FDecoProgressGrid.FRebuildNeeded:=True;
      if FDecoProgressGrid.FSorted then
        Sort;
    end;
    FDecoProgressGrid.Invalidate;
  end;
end;

{$IF CompilerVersion >= 15}
procedure TDecoProgressGridItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnDeleting:
      begin
        if Item=FDecoProgressGrid.FSelectedItem then
          FDecoProgressGrid.SelectedItem:=nil;
        if Item=FDecoProgressGrid.FHoverItem then
          FDecoProgressGrid.HoverItem:=nil;
      end;
  end;
  inherited;
end;
{$ELSE}
procedure TDecoProgressGridItems.Deleting(Item: TCollectionItem);
begin
  if Item=FDecoProgressGrid.FSelectedItem then
    FDecoProgressGrid.SelectedItem:=nil;
  if Item=FDecoProgressGrid.FHoverItem then
    FDecoProgressGrid.HoverItem:=nil;
  inherited;
end;
{$IFEND}

function TDecoProgressGridItems.GetItem(Index: Integer): TDecoProgressGridItem;
begin
  Result := inherited GetItem(Index) as TDecoProgressGridItem;
end;

procedure TDecoProgressGridItems.SetItem(Index: Integer; Value: TDecoProgressGridItem);
begin
  inherited SetItem(Index, Value);
end;

function TDecoProgressGridItems.Add: TDecoProgressGridItem;
begin
  Result := inherited Add as TDecoProgressGridItem;
end;

function TDecoProgressGridItems.AddItem(const Caption:string; Value:Extended=0; MinValue:Extended=0; MaxValue:Extended=100;
    const Hint:string=''; Tag:Longint=0; Data:Pointer=nil): TDecoProgressGridItem;
begin
  Result := inherited Add as TDecoProgressGridItem;
  Result.FCaption := Caption;
  Result.FMinValue := MinValue;
  Result.FMaxValue := MaxValue;
  Result.FHint := Hint;
  Result.FTag := Tag;
  Result.FData := Data;
  Result.Value := Value;
end;

function TDecoProgressGridItems.Insert(Index:Integer): TDecoProgressGridItem;
begin
  if FDecoProgressGrid.FSorted then
    Result := Add
  else
    Result := inherited Insert(Index) as TDecoProgressGridItem;
end;

procedure TDecoProgressGridItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TDecoProgressGridItems.Move(CurrIndex, NewIndex: Integer);
begin
  if not FDecoProgressGrid.FSorted then
  begin
    if Assigned(FItemsRef) then
      FItemsRef.Move(CurrIndex,NewIndex)
    else
      Items[CurrIndex].Index := NewIndex;
  end
end;

procedure TDecoProgressGridItems.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      if Assigned(FDecoProgressGrid.FOnCompareItems) then
      begin
        while FDecoProgressGrid.FOnCompareItems(FDecoProgressGrid,Items[i],Items[p]) < 0 do Inc(I);
        while FDecoProgressGrid.FOnCompareItems(FDecoProgressGrid,Items[j],Items[p]) > 0 do Dec(J);
      end
      else
      begin
        while AnsiCompareText(Items[i].FCaption,Items[p].FCaption) < 0 do Inc(I);
        while AnsiCompareText(Items[j].FCaption,Items[p].FCaption) > 0 do Dec(J);
      end;
      if I <= J then
      begin
        if I <> J then
        begin
          if Assigned(FItemsRef) then
            FItemsRef.Exchange(I,J)
          else
          begin
            Items[J].Index := I;
            Items[I+1].Index := J;
          end;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TDecoProgressGridItems.Sort;
begin
  FSorting := True;
  try
    if Count>0 then
      QuickSort(0, Count - 1)
  finally
    FSorting := False;
  end;
end;

{ TCustomDecoProgressGrid }

constructor TCustomDecoProgressGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable, csSetCaption {$IF CompilerVersion >= 20}, csPannable{$IFEND}];
  FDrawTextFunc := DoDrawNormalText;
  FColumns := TDecoProgressGridColumns.Create(Self);
  FItems := TDecoProgressGridItems.Create(Self);
  FCaptionFont := TFont.Create;
  FActive:=False;
  FBorderStyle := bsNone;
  FGlowSize := 0;
  FBackColor := clBtnHighlight;
  FFrameColor := clBtnFace;
  FFrameWidth := 1;
  FFrameProgress := False;
  FLevel1Color := $24CA8B;
  FLevel2Color := $00BFFF;
  FLevel3Color := $3437D9;
  FLevel1TextColor := clBlack;
  FLevel2TextColor := clBlack;
  FLevel3TextColor := clWhite;
  FLevel2Percent := 80;
  FLevel3Percent := 105;
  FRowHeight := 20;
  FRounded := True;
  FRoundCorners := 0;
  FTextMargin := 2;
  FGradient := pggVertical;
  FAlignment := taCenter;
  FShowCaption := True;
  FShowColumnsHeader := True;
  FShowFocusRect := True;
  FValueTextFormat := '%.0f';
  FHoverItem:=nil;
  FSelectedItem:=nil;
  FHoverCursor := crDefault;
  FBarPosition := pgbInline;
  FShowHorzGrid := True;
  FShowVertGrid := False;
  FShowFirstGridLine := True;
  FShowLastGridLine := True;
  FGridLineStyle := psDot;
  FGridLineColor := clBtnFace;
  FShowValueInBar := ibvValue;
  FShowOverMaxIndicator := True;
  FSorted := False;
  _EMFExporting:=False;

  FIsCaptionHovered:=False;
  FIsColumnsHeaderHovered:=False;
  FRebuildNeeded := True;

  FCaptionFont.Assign(Font);
  FCaptionFont.OnChange := CaptionFontChange;

  Width := 128;
  Height := 128;
  {$IF CompilerVersion >= 20}
  Padding.OnChange:=DoPaddingChange;
  {$IFEND}
  Color := clWhite;
  ParentBackground := False;
end;

destructor TCustomDecoProgressGrid.Destroy;
begin
  FItems.Free;
  FColumns.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TCustomDecoProgressGrid.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style  and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCustomDecoProgressGrid.Loaded;
begin
  inherited;
end;

procedure TCustomDecoProgressGrid.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FActive:=False;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FActive:=True;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message do
  begin
    if (CursorWnd = Handle) and (not (csDesigning in ComponentState)) and (Screen.Cursor = crDefault) and (FHoverItem<>nil) then
    begin
      Message.Result := 1;
      Windows.SetCursor(Screen.Cursors[FHoverCursor])
    end
    else
      inherited;
  end;
end;

procedure TCustomDecoProgressGrid.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  if (Message.CharCode in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_HOME, VK_END, VK_NEXT, VK_PRIOR]) then
    Message.Result := 1
  else
  if (Message.CharCode = VK_TAB) and FActive then
  begin
    if (FSelectedItem=nil) and (Items.Count>0) then
      SelectedItem:=Items[0]
    else
      Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TCustomDecoProgressGrid.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.CMFontChanged(var Message: TMessage);
begin
  FRebuildNeeded:=True;
  inherited;
end;

procedure TCustomDecoProgressGrid.CMHintShow(var Msg: TMessage);
begin
  with TCMHintShow(Msg) do
  begin
    if not ShowHint then
      Result := 1
    else
    begin
      if FHoverItem=nil then
        HintInfo.HintStr:=Hint
      else
      begin
        HintInfo.HintStr := FHoverItem.FHint;
        HintInfo.CursorRect:=FHoverItem.ItemRect;
      end;
      if Length(HintInfo.HintStr)>0 then
        Result := 0
      else
        Result := 1
    end;
  end
end;

procedure TCustomDecoProgressGrid.CMParentFontChanged(var Message: {$IF CompilerVersion >= 20} TCMParentFontChanged {$ELSE} TMessage {$IFEND});
begin
  inherited;
  if ParentFont then
    FCaptionFont.Assign(Font);
end;

procedure TCustomDecoProgressGrid.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FHoverItem) then
    HoverItem := nil;
  inherited;
end;

procedure TCustomDecoProgressGrid.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{$IF CompilerVersion >= 20}
procedure TCustomDecoProgressGrid.DoPaddingChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Realign;
  Invalidate;
end;
{$IFEND}

procedure TCustomDecoProgressGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if Shift=[ssCtrl] then
    case Key of
      VK_HOME: if Items.Count>0 then SelectedItem:=Items[0];
      VK_END: if Items.Count>0 then SelectedItem:=Items[Items.Count-1];
    end
  else
  if Shift=[] then
    case Key of
      VK_DOWN,VK_RIGHT,VK_NEXT:
        if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[0]; end
        else
        if FSelectedItem.Index<FSelectedItem.Collection.Count-1 then
          SelectedItem:=TDecoProgressGridItem(FSelectedItem.Collection.Items[FSelectedItem.Index+1]);
      VK_UP,VK_LEFT,VK_PRIOR:
        if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[Items.Count-1]; end
        else
        if FSelectedItem.Index>0 then
          SelectedItem:=TDecoProgressGridItem(FSelectedItem.Collection.Items[FSelectedItem.Index-1]);
      VK_HOME:
        if Items.Count>0 then
          SelectedItem:=Items[0];
      VK_END:
        if Items.Count>0 then
          SelectedItem:=Items[Items.Count-1];
      VK_ESCAPE:
        if Assigned(FSelectedItem) then
          SelectedItem:=nil;
    end;
end;

procedure TCustomDecoProgressGrid.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  FRebuildNeeded:=True;
  Invalidate;
  inherited;
end;

procedure TCustomDecoProgressGrid.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomDecoProgressGrid.UpdateHoverInfo(X, Y: Integer);
begin
  if PtInRect(FCaptionRect,point(x,y)) then
  begin
    if not FIsCaptionHovered then FIsCaptionHovered:=True;
    if FIsColumnsHeaderHovered then FIsColumnsHeaderHovered:=False;
    HoverItem := nil;
  end
  else
  begin
    if FIsCaptionHovered then FIsCaptionHovered:=False;
    if PtInRect(FColumnsHeaderRect,point(x,y)) then
    begin
      if not FIsColumnsHeaderHovered then FIsColumnsHeaderHovered:=True;
      HoverItem := nil;
    end
    else
    begin
      if FIsColumnsHeaderHovered then FIsColumnsHeaderHovered:=False;
      if (not Assigned(FHoverItem)) or (not (PtInRect(FHoverItem.FItemRect,Point(x,y)))) then
        HoverItem := GetItemAt(X,Y);
    end;
  end;
end;

function TCustomDecoProgressGrid._GetControlSize:TSize;
begin
  Result.cx:={$IF CompilerVersion >= 20} Padding.Left+Padding.Right {$ELSE} 8 {$IFEND} + (FBarsRect.Right-FBarsRect.Left);
  Result.cy:={$IF CompilerVersion >= 20} Padding.Top+Padding.Bottom {$ELSE} 8 {$IFEND} + (FCaptionRect.Bottom-FCaptionRect.Top)+
    (FColumnsHeaderRect.Bottom-FColumnsHeaderRect.Top)+(FBarsRect.Bottom-FBarsRect.Top);
end;

function TCustomDecoProgressGrid._GetRoundedCornersFactor:Integer;
begin
  if FRoundCorners<=0 then
    Result:=FRowHeight - FRowHeight div 5
  else
    Result:=FRoundCorners;
end;

function TCustomDecoProgressGrid.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  s:TSize;
begin
  if Align in [alNone,alTop,alBottom,alLeft,alRight] then
  begin
    s:=_GetControlSize;
    case Align of
      alLeft,alRight:
        begin
          Result:=NewWidth<>s.cx;
          NewWidth:=s.cx;
        end;
      alTop,alBottom:
        begin
          Result:=NewHeight<>s.cy;
          NewHeight:=s.cy;
        end;
    else
      begin
        Result:=(NewWidth<>s.cx) or (NewHeight<>s.cy);
        NewWidth:=s.cx;
        NewHeight:=s.cy;
      end;
    end;
  end
  else
    Result:=False;
end;

procedure TCustomDecoProgressGrid.SetAutoSize(Value: Boolean);
begin
  inherited;
  if Value then
  begin
    FRebuildNeeded:=True;
    Rebuild(ClientRect);
    AdjustSize;
  end;
end;

procedure TCustomDecoProgressGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not FActive) and HandleAllocated and Visible and (not (ssDouble in Shift)) then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
  UpdateHoverInfo(X,Y);
  SelectedItem:=FHoverItem;
end;

procedure TCustomDecoProgressGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateHoverInfo(X,Y);
end;

procedure TCustomDecoProgressGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (button=mbLeft) and Assigned(FOnItemClick) and Assigned(FHoverItem) then
    FOnItemClick(Self,FHoverItem);
end;

procedure TCustomDecoProgressGrid.SetHoverItem(const Value:TDecoProgressGridItem);
begin
  if Value<>FHoverItem then
  begin
    FHoverItem:=Value;
    if Assigned(FOnHoverChange) then
      FOnHoverChange(Self,FHoverItem);
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetSelectedItem(const Value:TDecoProgressGridItem);
begin
  if Value<>FSelectedItem then
  begin
    FSelectedItem:=Value;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self,FSelectedItem);
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetAlignment(Value: TAlignment);
begin
  if Value<>FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomDecoProgressGrid.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

procedure TCustomDecoProgressGrid.CaptionFontChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Invalidate;
end;

function TCustomDecoProgressGrid.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TCustomDecoProgressGrid.SetItems(const Value: TDecoProgressGridItems);
begin
  FItems.Assign(Value);
end;

function TCustomDecoProgressGrid.IsColumnsStored: Boolean;
begin
  Result := FColumns.Count > 0;
end;

procedure TCustomDecoProgressGrid.SetColumns(const Value: TDecoProgressGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCustomDecoProgressGrid.SetGlowSize(Value: Integer);
begin
  if Value <> FGlowSize then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TCustomDecoProgressGrid.SetBackColor(Value: TColor);
begin
  if Value<>FBackColor then
  begin
    FBackColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetFrameColor(Value: TColor);
begin
  if Value<>FFrameColor then
  begin
    FFrameColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetFrameProgress(Value: Boolean);
begin
  if Value<>FFrameProgress then
  begin
    FFrameProgress:=Value;
    Invalidate;
  end;
end;


procedure TCustomDecoProgressGrid.SetFrameWidth(Value: Integer);
begin
  if Value<>FFrameWidth then
  begin
    FFrameWidth:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel1Color(Value: TColor);
begin
  if Value<>FLevel1Color then
  begin
    FLevel1Color:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel2Color(Value: TColor);
begin
  if Value<>FLevel2Color then
  begin
    FLevel2Color:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel3Color(Value: TColor);
begin
  if Value<>FLevel3Color then
  begin
    FLevel3Color:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel1TextColor(Value: TColor);
begin
  if Value<>FLevel1TextColor then
  begin
    FLevel1TextColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel2TextColor(Value: TColor);
begin
  if Value<>FLevel2TextColor then
  begin
    FLevel2TextColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel3TextColor(Value: TColor);
begin
  if Value<>FLevel3TextColor then
  begin
    FLevel3TextColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel2Percent(Value: Integer);
begin
  if Value<>FLevel2Percent then
  begin
    FLevel2Percent:=Value;
    if FLevel2Percent<0 then
      FLevel2Percent:=0;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetLevel3Percent(Value: Integer);
begin
  if Value<>FLevel3Percent then
  begin
    FLevel3Percent:=Value;
    if FLevel3Percent<0 then
      FLevel3Percent:=0;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetRowHeight(Value: Integer);
begin
  if Value<>FRowHeight then
  begin
    FRowHeight:=Value;
    if FRowHeight<1 then
      FRowHeight:=1;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetRounded(Value: Boolean);
begin
  if Value<>FRounded then
  begin
    FRounded:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetRoundCorners(Value: Integer);
begin
  if Value<>FRoundCorners then
  begin
    FRoundCorners:=Value;
    if FRoundCorners<0 then
      FRoundCorners:=0;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetSorted(Value: Boolean);
begin
  if Value<>FSorted then
  begin
    FSorted := Value;
    if FSorted then
      Sort;
  end;
end;

procedure TCustomDecoProgressGrid.SetTextMargin(Value: Integer);
begin
  if Value<>FTextMargin then
  begin
    FTextMargin:=Value;
    if FTextMargin<-1 then
      FTextMargin:=-1;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetGradient(Value: TPGGradientStyle);
begin
  if Value<>FGradient then
  begin
    FGradient:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowCaption(Value: Boolean);
begin
  if Value<>FShowCaption then
  begin
    FShowCaption:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowColumnsHeader(Value: Boolean);
begin
  if Value<>FShowColumnsHeader then
  begin
    FShowColumnsHeader:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowFocusRect(Value: Boolean);
begin
  if Value<>FShowFocusRect then
  begin
    FShowFocusRect:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowOverMaxIndicator(Value: Boolean);
begin
  if Value<>FShowOverMaxIndicator then
  begin
    FShowOverMaxIndicator:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetBarPosition(Value: TPGBarPosition);
begin
  if Value<>FBarPosition then
  begin
    FBarPosition:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowHorzGrid(Value: Boolean);
begin
  if Value<>FShowHorzGrid then
  begin
    FShowHorzGrid:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowVertGrid(Value: Boolean);
begin
  if Value<>FShowVertGrid then
  begin
    FShowVertGrid:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowFirstGridLine(Value: Boolean);
begin
  if Value<>FShowFirstGridLine then
  begin
    FShowFirstGridLine:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowLastGridLine(Value: Boolean);
begin
  if Value<>FShowLastGridLine then
  begin
    FShowLastGridLine:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetGridLineStyle(Value: TPenStyle);
begin
  if Value<>FGridLineStyle then
  begin
    FGridLineStyle:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetGridLineColor(Value: TColor);
begin
  if Value<>FGridLineColor then
  begin
    FGridLineColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoProgressGrid.SetShowValueInBar(Value: TShowValueInBar);
begin
  if Value<>FShowValueInBar then
  begin
    FShowValueInBar:=Value;
    Invalidate;
  end;
end;

function TCustomDecoProgressGrid.GetValueFormatted(Value:Extended):string;
begin
  if Length(FValueTextFormat)=0 then
    Result:=FloatToStr(Value)
  else
    Result:=Format(FValueTextFormat,[Value]);
end;

procedure TCustomDecoProgressGrid.SetValueTextFormat(const Value: string);
var
  o:string;
begin
  if Value<>FValueTextFormat then
  begin
    o:=FValueTextFormat;
    FValueTextFormat:=Value;
    try
      GetValueFormatted(0.0);
      Invalidate;
    except
      FValueTextFormat:=o;
    end;
  end;
end;

procedure TCustomDecoProgressGrid.SetOnBeforePaint(Value: TDecoPaintEvent);
begin
  FOnBeforePaint:=Value;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.SetOnCompareItems(Value: TDecoPGCompareEvent);
begin
  FOnCompareItems := Value;
  if FSorted then
    Sort;
end;

procedure TCustomDecoProgressGrid.SetOnAfterPaint(Value: TDecoPaintEvent);
begin
  FOnAfterPaint:=Value;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.SetOnDrawCell(Value: TDecoProgressGridDrawCell);
begin
  FOnDrawCell:=Value;
  Invalidate;
end;

procedure TCustomDecoProgressGrid.Sort;
begin
  FItems.BeginUpdate;
  try
    FItems.Sort;
  finally
    FItems.EndUpdate;
  end;
end;

function TCustomDecoProgressGrid.GetItemAt(X,Y:Integer):TDecoProgressGridItem;
var
  p:TPoint;
  i:Integer;
begin
  Result:=nil;
  p.x:=x; p.y:=y;
  for i:=0 To Items.Count-1 do
    if PtInRect(Items[i].FItemRect,p) then
    begin
      Result:=Items[i];
      break;
    end;
end;

function TCustomDecoProgressGrid.GetHitTestInfoAt(X,Y:Integer):TDecoPGHitTestInfo;
var
  p:TPoint;
  i,c:Integer;
  cRect:TRect;
  vx:Integer;
begin
  Result.Kind:=pgiNoWhere;
  Result.Item:=nil;
  Result.ColumnIndex:=-1;
  Result.BarValue:=0;
  if y<=FBarsRect.Bottom then
  begin
    p.x:=x; p.y:=y;
    if PtInRect(FCaptionRect,p) then
    begin
      Result.Kind:=pgiCaption;
    end
    else
    if PtInRect(FColumnsHeaderRect,p) then
    begin
      Result.Kind:=pgiColumnsHeader;
      for c:=0 To Columns.Count-1 do
        if (x>=Columns[c].FColRect.Left) and (x<=Columns[c].FColRect.Right) then
        begin
          Result.ColumnIndex:=c;
          break;
        end;
    end
    else
    begin
      for i:=0 To Items.Count-1 do
        if PtInRect(Items[i].FItemRect,p) then
        begin
          Result.Item:=Items[i];
          Result.Kind:=pgiItem;
          for c:=0 To Columns.Count-1 do
            if (x>=Columns[c].FColRect.Left) and (x<=Columns[c].FColRect.Right) then
            begin
              Result.ColumnIndex:=c;
              if Columns[c].Kind=pgcProgressBar then
              begin
                Result.Kind:=pgiBar;
                cRect:=Columns[c].FColRect;
                InflateRect(cRect,-FSpacing,0);
                vx:=cRect.Left+Round((cRect.Right-cRect.Left)*((Result.Item.Value-Result.Item.MinValue)/(Result.Item.MaxValue-Result.Item.MinValue)));
                if vx>cRect.Right then
                  vx:=cRect.Right;
                if (x>=vx-2) and (x<=vx+2) then
                  Result.Kind:=pgiValueInBar;
                Result.BarValue:=(x-cRect.Left)/(cRect.Right-cRect.Left)*(Result.Item.MaxValue-Result.Item.MinValue);
                if Result.BarValue<Result.Item.MinValue then
                  Result.BarValue:=Result.Item.MinValue
                else
                if Result.BarValue>Result.Item.MaxValue then
                  Result.BarValue:=Result.Item.MaxValue
              end;
              break;
            end;
          break;
        end;
    end;
  end;
end;

procedure TCustomDecoProgressGrid.Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil);
var
  TM:TTextMetric;
  oldSize,newSize:TSize;
  x,y,i:Integer;
  iHeight:Integer;
  pgWidth,pgMinWidth:Integer;
  pgbSize:Integer;
  Col:TDecoProgressGridColumn;
begin
  if not FRebuildNeeded then
    Exit;
  FRebuildNeeded:=False;

  if (TargetCanvas=nil) and AutoSize and (Align in [alNone,alTop,alBottom,alLeft,alRight]) then
    oldSize:=_GetControlSize
  else
  begin
    oldSize.cx:=-1; oldSize.cy:=-1;
  end;
  if TargetCanvas=nil then
    TargetCanvas:=Canvas;

  SetRectEmpty(FCaptionRect);
  SetRectEmpty(FColumnsHeaderRect);
  FBarsRect := Dest;
  {$IF CompilerVersion >= 20}
  Inc(FBarsRect.Left,Padding.Left);
  Inc(FBarsRect.Top,Padding.Top);
  Dec(FBarsRect.Right,Padding.Right);
  Dec(FBarsRect.Bottom,Padding.Bottom);
  {$ELSE}
  Inc(FBarsRect.Left,4);
  Inc(FBarsRect.Top,4);
  Dec(FBarsRect.Right,4);
  Dec(FBarsRect.Bottom,4);
  {$IFEND}

  // main caption rectangle
  if FShowCaption then
  begin
    TargetCanvas.Font := FCaptionFont;
    GetTextMetrics(TargetCanvas.Handle,TM);
    FCaptionRect:=FBarsRect;
    FCaptionRect.Bottom:=FBarsRect.Top+TM.tmHeight+TM.tmDescent*2;
    FBarsRect.Top:=FCaptionRect.Bottom;
  end;

  TargetCanvas.Font := Font;
  GetTextMetrics(TargetCanvas.Handle,TM);
  if FTextMargin=-1 then
    FSpacing:=TM.tmDescent
  else
    FSpacing:=FTextMargin;

  // column headers
  if FShowColumnsHeader then
  begin
    FColumnsHeaderRect:=FBarsRect;
    FColumnsHeaderRect.Bottom:=FBarsRect.Top+TM.tmHeight+FSpacing;
    FBarsRect.Top:=FColumnsHeaderRect.Bottom;
  end;

  pgWidth:=0; pgMinWidth:=0;
  case FBarPosition of
    pgbInline:
      begin
        // column rectangles for classic grid
        for i:=0 To Columns.Count-1 do
          if Columns[i].Visible then
          begin
            if Columns[i].FKind=pgcProgressBar then
              pgMinWidth:=Columns[i].Width
            else
              Inc(pgWidth,Columns[i].Width);
          end;
        if AutoSize and (Align in [alNone,alLeft,alRight]) then
          pgWidth:=pgMinWidth
        else
        begin
          pgWidth:=(FBarsRect.Right-FBarsRect.Left)-pgWidth; // width of progressbar column
          if pgWidth<pgMinWidth then
            pgWidth:=pgMinWidth;
        end;
        if FRowHeight>TM.tmHeight then
          iHeight:=FRowHeight+2*FSpacing+FFrameWidth
        else
          iHeight:=TM.tmHeight+2*FSpacing+FFrameWidth;
        x:=FBarsRect.Left;
        for i:=0 To Columns.Count-1 do
        begin
          Col:=Columns[i];
          if Col.Visible then
          begin
            if Col.Kind=pgcProgressBar then
            begin
               SetRect(Col.FColRect,x,iHeight-FSpacing-(FFrameWidth div 2)-FRowHeight,x+pgWidth,iHeight-FSpacing-(FFrameWidth div 2));
                if FRowHeight<=TM.tmHeight then
                  offsetRect(Col.FColRect,0,-(TM.tmHeight-FRowHeight) div 2);
            end
            else
            begin
              if pgMinWidth=0 then // autosize first visible column if no progressbar column is defined
              begin
                SetRect(Col.FColRect,x,0,x+pgWidth+Col.Width,iHeight);
                pgMinWidth:=-1;
              end
              else
                SetRect(Col.FColRect,x,0,x+Col.Width,iHeight);
            end;
            x:=Col.FColRect.Right;
          end;
        end;
      end;
  else
      begin
        // column rectangles for top/bottom placement of progressbar
        pgbSize:=0;
        if (not AutoSize) or (not (Align in [alNone,alLeft,alRight])) then
        begin
          for i:=0 To Columns.Count-1 do
            if Columns[i].Visible then
            begin
              if Columns[i].FKind=pgcProgressBar then
                pgbSize:=FRowHeight
              else
                Inc(pgWidth,Columns[i].Width);
            end;
          pgWidth:=(FBarsRect.Right-FBarsRect.Left)-pgWidth; // additional width for the first non progressbar column
          if pgWidth<0 then
          begin
            pgMinWidth:=-pgWidth;
            pgWidth:=0;
          end
        end
        else
          for i:=0 To Columns.Count-1 do
            if Columns[i].Visible and (Columns[i].FKind=pgcProgressBar) then
            begin
              pgbSize:=FRowHeight;
              break;
            end;

        iHeight:=pgbSize+TM.tmHeight+3*FSpacing+FFrameWidth;
        if pgbSize=0 then
          Dec(iHeight,FSpacing);
        x:=FBarsRect.Left;
        for i:=0 To Columns.Count-1 do
        begin
          Col:=Columns[i];
          if Col.Visible then
          begin
            if Col.Kind=pgcProgressBar then
            begin
              if FBarPosition=pgbBottom then
                SetRect(Col.FColRect,FBarsRect.Left,iHeight-FSpacing-pgbSize,FBarsRect.Right+pgMinWidth,iHeight-FSpacing)
              else
                SetRect(Col.FColRect,FBarsRect.Left,FSpacing,FBarsRect.Right+pgMinWidth,FSpacing+pgbSize);
            end
            else
            begin
              if FBarPosition=pgbBottom then
                SetRect(Col.FColRect,x,FSpacing,x+Col.Width+pgWidth,iHeight-FSpacing-pgbSize)
              else
                SetRect(Col.FColRect,x,FSpacing+pgbSize,x+Col.Width+pgWidth,iHeight-FSpacing);
              if pgWidth>0 then
                pgWidth:=0;
              x:=Col.FColRect.Right;
            end;
          end;
        end;
      end;
  end;

  FBarsRect.Right:=x;
  FCaptionRect.Right:=x;
  FColumnsHeaderRect.Right:=x;

  // item rectangles
  y:=FBarsRect.Top;
  for i:=0 To Items.Count-1 do
    if Items[i].Visible then
    begin
      SetRect(Items[i].FItemRect,FBarsRect.Left,y,FBarsRect.Right,y+iHeight);
      Inc(y,iHeight);
    end;

  FBarsRect.Bottom:=y;

  if (oldsize.cx>=0) and AutoSize and (Align in [alNone,alLeft,alRight,alTop,alBottom]) then
  begin
    newSize:=_GetControlSize;
    if (oldSize.cx<>newSize.cx) or (oldSize.cy<>newSize.cy) then
      AdjustSize;
  end;
end;

procedure TCustomDecoProgressGrid.DrawCaption(TargetCanvas:TCanvas);
var
  Flags: Longint;
begin
  TargetCanvas.Font:=FCaptionFont;
  if not Enabled then
    TargetCanvas.Font.Color:=GrayScaleColor(FCaptionFont.Color);
  TargetCanvas.Brush.Style:=bsClear;
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS or DrawTextAlignments[FAlignment]);
  FDrawTextFunc(TargetCanvas.Handle, Caption, FCaptionRect, Flags, TargetCanvas.Font.Color, GlowSize);
end;

procedure TCustomDecoProgressGrid.DrawColumnsHeader(TargetCanvas:TCanvas);
var
  Flags: Longint;
  i:Integer;
  Col:TDecoProgressGridColumn;
  tmpRect:TRect;
begin
  TargetCanvas.Font:=Font;
  if not Enabled then
    TargetCanvas.Font.Color:=GrayScaleColor(FCaptionFont.Color);
  TargetCanvas.Brush.Style:=bsClear;
  for i:=0 To Columns.Count-1 do
  begin
    Col:=Columns[i];
    if Col.Visible and ((Col.Kind<>pgcProgressBar) or (FBarPosition=pgbInline)) then
    begin
      SetRect(tmpRect,Col.FColRect.Left+2*FSpacing,FColumnsHeaderRect.Top,Col.FColRect.Right-2*FSpacing,FColumnsHeaderRect.Bottom);
      if (Col.Kind=pgcProgressBar) and (Col.Caption='%%') then
      begin
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS or DT_LEFT);
        FDrawTextFunc(TargetCanvas.Handle, '0%', tmpRect, Flags, TargetCanvas.Font.Color, GlowSize);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS or DT_RIGHT);
        FDrawTextFunc(TargetCanvas.Handle, '100%', tmpRect, Flags, TargetCanvas.Font.Color, GlowSize);
      end
      else
      begin
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS or DrawTextAlignments[Col.Alignment]);
        FDrawTextFunc(TargetCanvas.Handle, Col.Caption, tmpRect, Flags, TargetCanvas.Font.Color, GlowSize);
      end;
    end;
  end;
end;

procedure TCustomDecoProgressGrid.DrawGrid(TargetCanvas:TCanvas);
var
  i,j:Integer;
  Col:TDecoProgressGridColumn;
  Item:TDecoProgressGridItem;
begin
  if FShowHorzGrid or (FShowVertGrid and (FBarPosition=pgbInline)) then
  begin
    if Enabled then
      TargetCanvas.Pen.Color:=FGridLineColor
    else
      TargetCanvas.Pen.Color:=GrayScaleColor(FGridLineColor);
    TargetCanvas.Pen.Width:=1;
    TargetCanvas.Pen.Style:=FGridLineStyle;
    if FShowHorzGrid then
    begin
      Item:=nil;
      j:=0;
      for i:=0 To Items.Count-1 do
        if Items[i].Visible then
        begin
          Item:=Items[i];
          Inc(j);
          if (j=1) and (not FShowFirstGridLine) then
            Continue;
          TargetCanvas.Polyline([Point(Item.FItemRect.Left,Item.FItemRect.Top), Point(Item.FItemRect.Right,Item.FItemRect.Top)]);
        end;
      if FShowLastGridLine and Assigned(Item) then
        TargetCanvas.Polyline([Point(Item.FItemRect.Left,Item.FItemRect.Bottom), Point(Item.FItemRect.Right,Item.FItemRect.Bottom)]);
    end;
    if FShowVertGrid and (FBarPosition=pgbInline) then
    begin
      Col:=nil;
      j:=0;
      for i:=0 To Columns.Count-1 do
        if Columns[i].Visible then
        begin
          Col:=Columns[i];
          Inc(j);
          if (j=1) and (not FShowFirstGridLine) then
            Continue;
          TargetCanvas.Polyline([Point(Col.FColRect.Left,FBarsRect.Top), Point(Col.FColRect.Left,FBarsRect.Bottom)]);
        end;
      if FShowLastGridLine and Assigned(Col) then
        TargetCanvas.Polyline([Point(Col.FColRect.Right-1,FBarsRect.Top), Point(Col.FColRect.Right-1,FBarsRect.Bottom)]);
    end;
  end;
end;

procedure TCustomDecoProgressGrid.DrawItems(TargetCanvas:TCanvas);
var
  c,i,j:Integer;
  Item:TDecoProgressGridItem;
  RC:HRGN;
  tmpClr,invClr,bkgClr,frmClr:TColor;
  rcs:Integer;
  cRect,cellRect,vRect,bRect:TRect;
  Col:TDecoProgressGridColumn;
  Flags: Longint;
  percent:Extended;
  tmpStr:string;
  eb:Boolean;
begin
  // draw items
  TargetCanvas.Font:=Font;
  rcs:=_GetRoundedCornersFactor;
  eb:=Enabled;
  if eb then
  begin
    bkgClr:=FBackColor;
    frmClr:=FFrameColor;
  end
  else
  begin
    bkgClr:=GrayScaleColor(FBackColor);
    frmClr:=GrayScaleColor(FFrameColor);
  end;
  for i:=0 To Items.Count-1 do
  begin
    Item:=Items[i];
    if Item.Visible then
    begin
      percent:=(Item.Value-Item.MinValue)/(Item.MaxValue-Item.MinValue);
      // setup color by level
      if percent*100>=FLevel3Percent then
      begin
        tmpClr:=FLevel3Color;
        invClr:=FLevel3TextColor;
      end
      else
      if percent*100>=FLevel2Percent then
      begin
        tmpClr:=FLevel2Color;
        invClr:=FLevel2TextColor;
      end
      else
      begin
        tmpClr:=FLevel1Color;
        invClr:=FLevel1TextColor;
      end;
      if not eb then
      begin
        tmpClr:=GrayScaleColor(tmpClr);
        invClr:=GrayScaleColor(invClr);
      end;
      for c:=0 To Columns.Count-1 do
      begin
        Col:=Columns[c];
        if Col.Visible then
        begin
          cellRect:=Col.FColRect;
          if cellRect.Left+FSpacing*8>=cellRect.Right then
            continue;
          OffsetRect(cellRect,0,Item.FItemRect.Top);
          cRect:=cellRect;
          InflateRect(cRect,-FSpacing,0);
          if Col.Kind=pgcProgressBar then
          begin
            if FRounded then
            begin
              RC := CreateRoundRectRgn(cRect.Left,cRect.Top,cRect.Right+1,cRect.Bottom+1,rcs,rcs);
              SelectClipRgn(TargetCanvas.Handle,RC);
            end
            else
              RC:=0;

            TargetCanvas.Brush.Style:=bsSolid;
            TargetCanvas.Pen.Style:=psSolid;
            TargetCanvas.Pen.Width:=1;
            vRect:=cRect;
            if percent<0 then
              vRect.Right:=vRect.Left
            else
              vRect.Right:=vRect.Left+Round((vRect.Right-vRect.Left)*percent);
            if vRect.Right>=cRect.Right then
            begin
              if vRect.Right>cRect.Right then
                vRect.Right:=cRect.Right;
            end
            else
            begin
              bRect:=cRect;
              bRect.Left:=vRect.Right;
              // background rect
              case FGradient of
                pggVertical:FillGradient(TargetCanvas,bRect,BrightColor(bkgClr,15),DarkColor(bkgClr,8));
                pggMirror:
                  begin
                    j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,15),DarkColor(bkgClr,9));
                    FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),DarkColor(bkgClr,8),BrightColor(bkgClr,15));
                  end;
                pggLightMirror:
                  begin
                    j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,30),bkgClr);
                    FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),bkgClr,BrightColor(bkgClr,30));
                  end;
                pggGlass:
                  begin
                    j:=bRect.Top+(bRect.Bottom-bRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(bRect.Left,bRect.Top,bRect.Right,j),BrightColor(bkgClr,20),BrightColor(bkgClr,5));
                    FillGradient(TargetCanvas,Rect(bRect.Left,j,bRect.Right,bRect.Bottom),bkgClr,BrightColor(bkgClr,10));
                  end;
              else
                begin
                  TargetCanvas.Brush.Color:=bkgClr;
                  TargetCanvas.Pen.Color:=bkgClr;
                  TargetCanvas.Rectangle(bRect);
                end;
              end;
            end;
            // value rect
            if vRect.Right>vRect.Left then
            begin
              case FGradient of
                pggVertical:FillGradient(TargetCanvas,vRect,BrightColor(tmpClr,15),DarkColor(tmpClr,8));
                pggMirror:
                  begin
                    j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,15),DarkColor(tmpClr,9));
                    FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),DarkColor(tmpClr,8),BrightColor(tmpClr,15));
                  end;
                pggLightMirror:
                  begin
                    j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,30),tmpClr);
                    FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),tmpClr,BrightColor(tmpClr,30));
                  end;
                pggGlass:
                  begin
                    j:=vRect.Top+(vRect.Bottom-vRect.Top) div 2;
                    FillGradient(TargetCanvas,Rect(vRect.Left,vRect.Top,vRect.Right,j),BrightColor(tmpClr,20),BrightColor(tmpClr,5));
                    FillGradient(TargetCanvas,Rect(vRect.Left,j,vRect.Right,vRect.Bottom),tmpClr,BrightColor(tmpClr,10));
                  end;
              else
                begin
                  TargetCanvas.Brush.Color:=tmpClr;
                  TargetCanvas.Pen.Color:=tmpClr;
                  TargetCanvas.Rectangle(vRect);
                end;
              end;
            end;

            if ShowOverMaxIndicator and ((percent<0) or (percent>1)) then
            begin
              if percent>1 then
                j:=cRect.Right-Round((cRect.Right-cRect.Left)*(percent-1))
              else
                j:=cRect.Left+Round((cRect.Right-cRect.Left)*(-percent));
              if (j>cRect.Left) and (j<cRect.Right) then
              begin
                case FGradient of
                  pggNone:
                    begin
                      TargetCanvas.Pen.Color:=DarkColor(tmpClr,10);
                      TargetCanvas.MoveTo(j-1,cRect.Top); TargetCanvas.LineTo(j-1,cRect.Bottom);
                      TargetCanvas.Pen.Color:=BrightColor(tmpClr,10);
                      TargetCanvas.MoveTo(j,cRect.Top); TargetCanvas.LineTo(j,cRect.Bottom);
                    end;
                else
                    begin
                      FillGradient(TargetCanvas,Rect(j-1,cRect.Top,j,cRect.Bottom),BrightColor(tmpClr,8),DarkColor(tmpClr,15));
                      FillGradient(TargetCanvas,Rect(j,cRect.Top,j+1,cRect.Bottom),BrightColor(tmpClr,38),BrightColor(tmpClr,15));
                    end;
                end;
              end;
            end;

            if FRounded then
              SelectClipRgn(TargetCanvas.Handle, HRGN(nil));

            // draw frame
            if _EMFExporting or ((FFrameColor<>clNone) and (FFrameWidth>0)) then
            begin
              TargetCanvas.Brush.Style:=bsClear;
              TargetCanvas.Pen.Style:=psSolid;
              if _EMFExporting then
              begin
                TargetCanvas.Pen.Color:=Color;
                TargetCanvas.Pen.Width:=3;
                if FRounded then
                  TargetCanvas.RoundRect(cRect.Left-1,cRect.Top-1,cRect.Right+1,cRect.Bottom+1,rcs,rcs)
                else
                  TargetCanvas.Rectangle(cRect.Left-1,cRect.Top-1,cRect.Right+1,cRect.Bottom+1);
              end;
              if (FFrameColor<>clNone) and (FFrameWidth>0) then
              begin
                TargetCanvas.Pen.Color:=frmClr;
                TargetCanvas.Pen.Width:=FFrameWidth;
                if FRounded then
                  {$IF CompilerVersion >= 20}
                  TargetCanvas.RoundRect(cRect,rcs,rcs)
                  {$ELSE}
                  TargetCanvas.RoundRect(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom,rcs,rcs)
                  {$IFEND}
                else
                  TargetCanvas.Rectangle(cRect);
              end;
            end;

            if FFrameProgress and (vRect.Right>vRect.Left) then
            begin
              TargetCanvas.Brush.Style:=bsClear;
              TargetCanvas.Pen.Color:=DarkColor(tmpClr,15);
              TargetCanvas.Pen.Width:=FFrameWidth;
              if FRounded then
              begin
                ExcludeClipRect(TargetCanvas.Handle,vRect.Right+(FFrameWidth div 2),vRect.Top-FFrameWidth,cRect.Right+(FFrameWidth div 2),vRect.Bottom+FFrameWidth);
                {$IF CompilerVersion >= 20}
                TargetCanvas.RoundRect(cRect,rcs,rcs);
                {$ELSE}
                TargetCanvas.RoundRect(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom,rcs,rcs);
                {$IFEND}
                if (vRect.Right>cRect.Left) and (vRect.Right<cRect.Right) then
                begin
                  SelectClipRgn(TargetCanvas.Handle,RC);
                  TargetCanvas.Polyline([Point(vRect.Right,vRect.Top),Point(vRect.Right,vRect.Bottom-(FFrameWidth div 2)-Byte(_EMFExporting))]);
                end;
                SelectClipRgn(TargetCanvas.Handle, HRGN(nil));
              end
              else
                TargetCanvas.Rectangle(vRect);
            end;

            if RC<>0 then
              DeleteObject(RC);

            if FShowValueInBar<>ibvNone then
            begin
              TargetCanvas.Brush.Style:=bsClear;
              TargetCanvas.Font:=Font;
              TargetCanvas.Font.Color:=invClr;
              inflateRect(cRect,-4*FSpacing,0); Dec(cRect.Bottom);
              Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DrawTextAlignments[Col.Alignment]);
              case FShowValueInBar of
                ibvValue:tmpStr:=GetValueFormatted(Item.Value);
                ibvPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
              end;
              if Assigned(FOnDrawCell) then
                FOnDrawCell(Self, TargetCanvas, Item, c, cellRect, tmpStr);
              if Length(tmpStr)>0 then
                FDrawTextFunc(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color, GlowSize);
            end;
          end
          else
          begin
            InflateRect(cRect,-FSpacing,0);
            TargetCanvas.Brush.Style:=bsClear;
            TargetCanvas.Font:=Font;
            if not eb then
              TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
            Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DrawTextAlignments[Col.Alignment]);
            case Col.Kind of
              pgcCaption:tmpStr:=Item.Caption;
              pgcDifference:tmpStr:=GetValueFormatted(Item.MaxValue-Item.Value);
              pgcMinValue:tmpStr:=GetValueFormatted(Item.MinValue);
              pgcMaxValue:tmpStr:=GetValueFormatted(Item.MaxValue);
              pgcValue:tmpStr:=GetValueFormatted(Item.Value);
              pgcPercent:tmpStr:=IntToStr(Round(percent*100))+'%';
            else
              tmpStr:='';
            end;
            if Assigned(FOnDrawCell) then
              FOnDrawCell(Self,TargetCanvas,Item, c, cellRect, tmpStr);
            if Length(tmpStr)>0 then
              FDrawTextFunc(TargetCanvas.Handle, tmpStr, cRect, Flags, TargetCanvas.Font.Color, GlowSize);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomDecoProgressGrid.Paint;
var
  aRect,cRect: TRect;
  Buffer : TBitmap;
  tmpCanvas : TCanvas;
begin
  aRect := ClientRect;
  if FRebuildNeeded then
    Rebuild(aRect);
  cRect := Canvas.ClipRect;
  Buffer := TBitmap.Create;
  try
    Buffer.Width := aRect.Right-aRect.Left;
    Buffer.Height := aRect.Bottom-aRect.Top;
    tmpCanvas := Buffer.Canvas;
    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);

    tmpCanvas.Brush.Color := Color;
    tmpCanvas.FillRect(cRect);

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,tmpCanvas);

    if FShowCaption then
      DrawCaption(tmpCanvas);

    if FShowColumnsHeader then
      DrawColumnsHeader(tmpCanvas);

    DrawGrid(tmpCanvas);

    DrawItems(tmpCanvas);

    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,tmpCanvas);

    Canvas.CopyRect(cRect, tmpCanvas, cRect);

  finally
    Buffer.Free;
  end;

  if FActive and FShowFocusRect and Assigned(FSelectedItem) then
  begin
    aRect:=FSelectedItem.FItemRect;
    Inc(aRect.Bottom);
    windows.DrawFocusRect(Canvas.Handle,aRect);
  end;
end;

procedure TCustomDecoProgressGrid.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
begin
  _EMFExporting:=AsEMF;
  try
    FRebuildNeeded := True;
    Rebuild(Dest,TargetCanvas);

    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Brush.Color := Color;
    TargetCanvas.FillRect(Dest);

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,TargetCanvas);

    if FShowCaption then
      DrawCaption(TargetCanvas);

    if FShowColumnsHeader then
      DrawColumnsHeader(TargetCanvas);

    DrawGrid(TargetCanvas);

    DrawItems(TargetCanvas);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,TargetCanvas);

  finally
    _EMFExporting := False;
    FRebuildNeeded := True;
    Rebuild(ClientRect);
  end;
end;

end.
