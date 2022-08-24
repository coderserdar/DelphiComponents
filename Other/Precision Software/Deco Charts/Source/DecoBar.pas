{------------------------------------------------------------------------------
  DecoBar.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TDecoBar component implements an interactive decomposition chart,
              that allows you to present different types of financial, statistical
              or general numeric data in a clear form of expandable bars. Sections
              in each bar can be expanded and collapsed using the mouse or keyboard,
              so the end-users can easily navigate through the hierarchical structure
              of data defined in the chart. Of course, TDecoBar can be used to
              display a simple (one-level) stacked bar chart.

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

{ This unit contains TCustomDecoBar and TDecoBar components declaration and implementation. }
unit DecoBar;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, Forms, SysUtils,
  DecoCommon;

type
  // Style of the tree lines
  TDecoBarTreeLinesStyle = (tlsNone, tlsRectangular, tlsRoundedRect, tlsCurved, tlsStrightLines);

  TCustomDecoBar = class;
  TDecoBarItems = class;

  { Represents a section in bar. Each section can contain sub-items as a }
  TDecoBarItem = class(TCollectionItem)
  private
    FCaption: string;
    FColor: TColor;
    FData: Pointer;
    FExpanded: Boolean;
    FItems:TDecoBarItems;
    FTag: Longint;
    FValue: Extended;
    FItemRect: TRect;
    FItemLegRect: TRect;
  protected
    function GetDisplayName: string; override;
    procedure SetCaption(Value: string); virtual;
    procedure SetColor(Value: TColor); virtual;
    procedure SetExpanded(Value: Boolean); virtual;
    function IsItemsStored: Boolean;
    procedure SetItems(const Value: TDecoBarItems);
    procedure SetValue(NewValue: Extended); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Any custom data reference
    property Data: Pointer read FData write FData;
    // A calculated rectangle for drawing the item. Valid only when item is visible (ie. it is a root item or the parent is expanded).
    property ItemRect: TRect read FItemRect;
    // A calculated rectangle for drawing the item in a legend. Valid only when item is visible at the last expanded level.
    property ItemLegRect: TRect read FItemLegRect;
  published
    // Background color of the item
    property Color: TColor read FColor write SetColor default clWhite;
    // Caption of the item (displayed in legend and when the item is hovered or selected)
    property Caption: string read FCaption write SetCaption;
    // Indicates the expanded state of the item
    property Expanded: Boolean read FExpanded write SetExpanded default False;
    // Sub-items collection
    property Items: TDecoBarItems read FItems write SetItems stored IsItemsStored;
    // Any custom tag value (integer type)
    property Tag: Longint read FTag write FTag default 0;
    // Value of the item (the bar section)
    property Value: Extended read FValue write SetValue;
  end;

  { Represents a bar of sections. Each section is defined as an item of this collection,
    and can contain its own sub-items, that will be displayed as a subordinate bar (when the item is expanded). }
  TDecoBarItems = class(TCollection)
  private
    FOwner: TPersistent;
    FDecoBar: TCustomDecoBar;
    FBarsRect: TRect;
    FExpandedItem: TDecoBarItem;
    FTotalValue: Extended;
    FUpdateTotalsNeeded:Boolean;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    {$IF CompilerVersion >= 15}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Deleting(Item: TCollectionItem);
    {$IFEND}
    function GetItem(Index: Integer): TDecoBarItem; virtual;
    procedure SetItem(Index: Integer; Value: TDecoBarItem); virtual;
    procedure SetExpandedItem(Item: TDecoBarItem);
  public
    // A collection constructor. It is called automatically by the owner component.
    constructor Create(AOwner: TPersistent);
    // Creates and adds a new item to the collection
    function Add: TDecoBarItem;
    // Creates and inserts a new item to the specified position in the collection
    function Insert(Index:Integer): TDecoBarItem;
    // Deletes an item specified by the index
    procedure Delete(Index: Integer);
    // Moves an item to another position in the collection. Moved item is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    { Allows you to add a pre-filled new item }
    function AddItem(const Caption:string; Value:Extended=0; Color:TColor = clWhite; Tag:Longint=0; Data:Pointer=nil): TDecoBarItem;
    // Collapses an expanded item, if any
    procedure Collapse;
    // Recalculates a total value of this bar and the value of a parent item (section)
    procedure UpdateTotalValue; virtual;
    // Calculated drawing rectangle for this bar
    property BarsRect:TRect read FBarsRect;
    // Reference to TCustomDecoBar component that owns a top level collection
    property DecoBar: TCustomDecoBar read FDecoBar;
    // Holds a currently expanded item (section)
    property ExpandedItem: TDecoBarItem read FExpandedItem write SetExpandedItem;
    // Through this property you can access the individual items (sections)
    property Items[Index: Integer]: TDecoBarItem read GetItem write SetItem; default;
    // Sum of the values of all the items in the collection. Valid only when AutoCalcValues property of TCustomDecoBar component is True, or after calling the UpdateTotalValue method.
    property TotalValue: Extended read FTotalValue;
  end;

  // Common event handler type, used for various item events (OnExpanded, OnCollapsed, OnHoverChange, etc.)
  TDecoBarItemEvent = procedure(Sender:TObject; Item:TDecoBarItem) of object;
  // Confirmation event handler type, used for various item events (OnExpanding, etc.)
  TDecoBarItemAllowEvent = procedure(Sender:TObject; Item:TDecoBarItem; var Allow:boolean) of object;

  { A base class for all DecoBar visual component implementations. See also TDecoBar, TDecoBarPlus. }
  TCustomDecoBar = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FAllowExpandingNodes: Boolean;
    FAutoCalcValues: Boolean;
    FAutoColors: Boolean;
    FBarSize: Integer;
    FBorderStyle: TBorderStyle;
    FCaptionFont: TFont;
    FCaptionFormat: string;
    FCutExpandedBar: Boolean;
    FDarkenCollapsedBars: Boolean;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FGlowSize: Integer;
    FGradient: Boolean;
    FHoverCursor: TCursor;
    FHoverItem: TDecoBarItem;
    FItems: TDecoBarItems;
    FItemTextFormat: string;
    FOnAfterPaint: TDecoPaintEvent;
    FOnBeforePaint: TDecoPaintEvent;
    FOnCollapsed: TDecoBarItemEvent;
    FOnExpanded: TDecoBarItemEvent;
    FOnExpanding: TDecoBarItemAllowEvent;
    FOnHoverChange: TDecoBarItemEvent;
    FOnItemClick: TDecoBarItemEvent;
    FOnSelectItem: TDecoBarItemEvent;
    FParentBackgroundSet: Boolean;
    FRounded: Boolean;
    FRoundCorners:Integer;
    FSelectedItem: TDecoBarItem;
    FStripSize: Integer;
    FShowCaption: Boolean;
    FShowLegend: Boolean;
    FTreeLinesColor: TColor;
    FTreeLinesStyle: TDecoBarTreeLinesStyle;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentFontChanged(var Message: {$IF CompilerVersion >= 20} TCMParentFontChanged {$ELSE} TMessage {$IFEND}); message CM_PARENTFONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure CaptionFontChange(Sender: TObject);
    {$IF CompilerVersion >= 20}
    procedure DoPaddingChange(Sender: TObject);
    {$IFEND}
    function GetTotalValue:Extended;
    function IsItemsStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoCalcValues(Value: Boolean);
    procedure SetAutoColors(Value: Boolean);
    procedure SetBarSize(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionFormat(const Value: string);
    procedure SetCutExpandedBar(Value: Boolean);
    procedure SetDarkenCollapsedBars(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);
    procedure SetGlowSize(Value: Integer);
    procedure SetGradient(Value: Boolean);
    procedure SetHoverItem(const Value:TDecoBarItem);
    procedure SetItems(const Value: TDecoBarItems);
    procedure SetItemTextFormat(const Value: string);
    procedure SetOnAfterPaint(Value: TDecoPaintEvent);
    procedure SetOnBeforePaint(Value: TDecoPaintEvent);
    procedure SetRoundCorners(Value: Integer);
    procedure SetRounded(Value: Boolean);
    procedure SetSelectedItem(const Value:TDecoBarItem);
    procedure SetShowCaption(Value: Boolean);
    procedure SetShowLegend(Value: Boolean);
    procedure SetStripSize(Value: Integer);
    procedure SetTreeLinesColor(Value: TColor);
    procedure SetTreeLinesStyle(Value: TDecoBarTreeLinesStyle);
  protected
    // Temporary variable, that indicates a drawing onto the vector based canvas (ie. TMetafileCanvas). It is used in PrintTo method.
    _EMFExporting:Boolean;
    // A temporary variable that indicates if the component has a focus
    FActive:Boolean;
    // Calculated rectangle that covers all currently displayed bars (top bar and expanded bars)
    FBarsRect: TRect;
    // Calculated rectangle for component title (Caption)
    FCaptionRect: TRect;
    // Reference to the helper function for drawing the texts
    FDrawTextFunc:TDrawTextFunc;
    FExpandedItems: TDecoBarItems;
    // See IsCaptionHovered property
    FIsCaptionHovered: Boolean;
    // See IsLegendHovered property
    FIsLegendHovered: Boolean;
    // Calculated rectangle for legend
    FLegendRect: TRect;
    // Temporary variable that indicates the need of recalculating the drawing rectangles (FBarsRect, FCaptionRect, expanded items rectangles, etc.)
    FRebuildNeeded: Boolean;
    { Temporary variable used as a flag for drawing a focus rectangle. TCustomDecoBar shows the focus only when user manipulates the component via keyboard.
      When the user manipulates the component via mouse, no focus rectangle is drawn. }
    FShowingFocusRect: Boolean;
    // Calculated text spacing - a margin between texts and graphics elements. It is based on a currently selected font metrics.
    FSpacing: Integer;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    // A base method for drawing the component title (Caption). You can override it in inherited components.
    procedure DrawCaption(TargetCanvas:TCanvas); virtual;
    // A base method for drawing the bars. You can override it in inherited components.
    procedure DrawItems(TargetCanvas:TCanvas; L:TDecoBarItems); virtual;
    // A base method for drawing the items texts (captions) and tree lines. You can override it in inherited components.
    procedure DrawItemText(TargetCanvas:TCanvas; Item:TDecoBarItem); virtual;
    // A base method for drawing the legend (items in a deepest expanded level are displayed in the legend). You can override it in inherited components.
    procedure DrawLegend(TargetCanvas:TCanvas; L:TDecoBarItems); virtual;
    // Helper method for getting an appropriate color for the first item (section) in a passed items collection (an expanded bar). See also AutoColors property.
    function GetAutoColorIndex(L:TDecoBarItems):Integer;
    // Returns the calculated bounds of the component - used when AutoSize property is True and the component is aligned to top or to bottom.
    function _GetControlSize:Integer; virtual;
    // Returns the rounding factor of bar corners, when Rounded property is True. The result is based on RowHeight and RoundCorners properties.
    function _GetRoundedCornersFactor:Integer;
    { Handling of keyboard shortcuts available for this component.<br>Space - toggles expanding<br>ESC - collapses selected bar and selects the parent item<br>
      LEFT - selects the previous item<br>RIGHT - selects the next item in the bar<br>UP - selects parent item<br>DOWN - expands the selected item<br>
      HOME - selects the first item in the bar<br>END - selects the last item in the bar<br>PAGE UP - moves the focus to the parent bar<br>
      PAGE DOWN - moves the focus to the child bar (if expanded)<br>CTRL+HOME - selects the very first item in the component<br>CTRL+END - selects the last item in the last expanded bar }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // Base method for drawing the component, that calls other methods to draw the bars, texts, etc.
    procedure Paint; override;
    procedure SetParentBackground(Value: Boolean); override;
    // Method for setting the current HoverItem based on passed X and Y coordinates. This method sets also FIsCaptionHovered and FIsLegendHovered variables.
    procedure UpdateHoverInfo(X, Y: Integer);
    // Caption alignment. See also Caption and ShowCaption properties.
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    { If True, all parent values are automatically recalculated when the value of an item is changed.
      Warning! This does not recalculates the children values. To recalculate all items, use RecalcAll method instead.
      If items are defined in design-time in combination with AutoCalcValues, RecalcAll method is called once, when the component is loaded. }
    property AutoCalcValues: Boolean read FAutoCalcValues write SetAutoCalcValues default False;
    { If True, the color of each item (section) is selected automatically based on the AutoColorPalette array.
      Every expanded bar starts with the same color that is selected for its parent item (section).
      When the end of the color palette is reached, colors are selected from the beggining of the palette again. }
    property AutoColors: Boolean read FAutoColors write SetAutoColors default True;
    { If True, allows you to expand the nodes (deepest level items). Then you can use the DecoBar in combination with another component (ie. grid)
      to display a list of node's details while preserving consistent look and feel. }
    property AllowExpandingNodes: Boolean read FAllowExpandingNodes write FAllowExpandingNodes default False;
    // Size (height) of each bar
    property BarSize: Integer read FBarSize write SetBarSize default 20;
    // Border style of the component
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    // Font for the caption (title)
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    { You can setup this property to display the caption together with a TotalValue as a title of the chart.
      Any formatting options for standard Delphi Format function are allowed. The %s option is replaced with Caption, the rest of the format string
      is passed to the Format function, that takes the TotalValue as a parameter.
      So, for example the '%s: %.2n' format string results to displaying the title as 'Title of the chart: 100,000.00'. }
    property CaptionFormat: string read FCaptionFormat write SetCaptionFormat;
    // A background color of the component
    property Color default clBtnFace;
    // If True, the expanded item (section) will be drawn as cropped. This enhances the visual appearance of an expanded bars for users.
    property CutExpandedBar: Boolean read FCutExpandedBar write SetCutExpandedBar default True;
    // If True, then all the items (sections), except the expanded one, will be displayed in darker colors. This enhances the visual appearance of an expanded bars for users.
    property DarkenCollapsedBars: Boolean read FDarkenCollapsedBars write SetDarkenCollapsedBars default True;
    // Frame color of the bar. If value is clNone then no frame will be displayed.
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnFace;
    // Width of the bar frame. If value is 0, then no frame will be displayed.
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
    // If True, the items (sections) will be drawn in a gradient style, from slightly lighter to the slightly darker color.
    property Gradient: Boolean read FGradient write SetGradient default True;
    // The cursor for hovered items (sections)
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor default crDefault;
    { This property defines a format string for displaying the caption of items.
      Any formatting options for standard Delphi Format function are allowed. The %s option is replaced with item's Caption, the rest of the format string
      is passed to the Format function, that takes the item's Value property as a parameter.
      So, for example the '%s: %.2n' format string results to displaying the item caption as 'My item: 100,000.00'. }
    property ItemTextFormat: string read FItemTextFormat write SetItemTextFormat;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint: TDecoPaintEvent read FOnAfterPaint write SetOnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint: TDecoPaintEvent read FOnBeforePaint write SetOnBeforePaint;
    // Event handler that you can use after the item (section) is collapsed
    property OnCollapsed: TDecoBarItemEvent read FOnCollapsed write FOnCollapsed;
    // Event handler that you can use after the item (section) is expanded
    property OnExpanded: TDecoBarItemEvent read FOnExpanded write FOnExpanded;
    // Confirmation event handler that you can use just before the item is expanded. If you do not want to expand the item, setup the Allow parameter to False.
    property OnExpanding: TDecoBarItemAllowEvent read FOnExpanding write FOnExpanding;
    { This event allows you to process your own actions when an item under the mouse is changed.
      Item parameter can have a nil value, when no item is hovered. You can read the IsLegendHovered property to check if the item is hovered in a legend part of the component. }
    property OnHoverChange: TDecoBarItemEvent read FOnHoverChange write FOnHoverChange;
    { This event allows you to process your own actions when an item is clicked.
      Item parameter has always a value different from nil. If the control is clicked and no item is hovered,
      only an OnClick event is fired. }
    property OnItemClick: TDecoBarItemEvent read FOnItemClick write FOnItemClick;
    { This event allows you to process your own actions when the SelectedItem property value is changed.
      Item parameter can have a nil value, when no item is selected. }
    property OnSelectItem: TDecoBarItemEvent read FOnSelectItem write FOnSelectItem;
    { Set this property to True to draw the component's background as transparent.
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoBarPlus) it is. }
    property ParentBackground stored FParentBackgroundSet default False;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor default False;
    { Setup this property to non-zero, if you want to use your own rounded corners factor.
      If you set the Rounded property to True and the RoundCorners property is 0, an internal rounded corners factor will be used, based on the BarSize property.
      See also Rounded property. }
    property RoundCorners: Integer read FRoundCorners write SetRoundCorners default 0;
    // If False, the bars will be displayed as standard rectangles. If True, the bars will be displayed with rounded corners. See also RoundCorners property.
    property Rounded: Boolean read FRounded write SetRounded default True;
    // If True, the title of the bar (Caption) will be displayed. See also Alignment, Caption, CaptionFormat properties.
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    // If True, the legend will be displayed for the last expanded level of items. See also AllowExpandingNodes property.
    property ShowLegend: Boolean read FShowLegend write SetShowLegend default True;
    // This property defines a spacing between a strips (dividing lines). If StripSize is 0, then no strips will be drawn.
    property StripSize: Integer read FStripSize write SetStripSize default 20;
    // This property defines a color of tree lines
    property TreeLinesColor: TColor read FTreeLinesColor write SetTreeLinesColor default clBtnShadow;
    // This property defines a style of the tree lines
    property TreeLinesStyle: TDecoBarTreeLinesStyle read FTreeLinesStyle write SetTreeLinesStyle default tlsRectangular;
  public
    // You can freely define this array to customize the colors for automatic coloring of the bar items (sections). See also AutoColors property.
    AutoColorPalette:array of TColor;
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    { Returns a formatted caption of the component, based on the CaptionFormat property. }
    function GetCaptionFormatted:string; virtual;
    // Returns the current item (section) at X, Y position
    function GetItemAt(X,Y:Integer):TDecoBarItem; virtual;
    { Returns a formatted caption of the item (section), based on the ItemTextFormat property. }
    function GetItemTextFormatted(Item:TDecoBarItem):string; virtual;
    { Collapses all the expanded items (sections) }
    procedure FullCollapse;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destination rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas).
      Refer to the main DecoBar demo project, to see how to use this method to save/export the grid into a graphic formats. }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); virtual;
    { This method calculates the drawing rectangles (and other aspects) of each displayed item and stores the values to the temporary variables (a cache).
      The repainting of the component itself is then faster.
      This method is called when the items are modified, on resizing the component, etc. }
    procedure Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil); virtual;
    // Recalculates all values (Item.Value and Items.TotalValue properties) starting from the items in the deepest levels
    procedure RecalcAll;
    // Temporary property, that indicates if the component has a focus
    property Active: Boolean read FActive;
    // Reference to the currently expanded items collection in a deepest level. If no items are expanded, ExpandedItems refers to the Items property.
    property ExpandedItems:TDecoBarItems read FExpandedItems;
    { A glow size attribut for drawing the texts (title, captions, legend, etc.).
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoBarPlus) it is. }
    property GlowSize: Integer read FGlowSize write SetGlowSize default 0;
    // Identifies the item under the mouse cursor. You can also read the IsLegendHovered property to check if the item is hovered in a legend part of the component.
    property HoverItem: TDecoBarItem read FHoverItem write SetHoverItem;
    // Read this property to check if the mouse is currently positioned above the component's title (Caption)
    property IsCaptionHovered: Boolean read FIsCaptionHovered;
    // Read this property to check if the mouse is currently positioned above the component's legend. See also HoverItem property
    property IsLegendHovered: Boolean read FIsLegendHovered;
    { Items collection represents a top level bar of sections. Each section is defined as an item of this collection,
      and can contain its own sub-items, that will be displayed as a subordinate bar (when the item is expanded). }
    property Items: TDecoBarItems read FItems write SetItems stored IsItemsStored;
    { Identifies currently selected item. Item can be selected by clicking the mouse, or by navigating through the items structure via various keyboard shortcuts.
      Refer to the KeyDown method to see all the possible hot-keys. }
    property SelectedItem: TDecoBarItem read FSelectedItem write SetSelectedItem;
    // Sum of the values of all items. It is valid only when AutoCalcValues property of the component is True, or after calling a RecalcAll method.
    property TotalValue: Extended read GetTotalValue;
  end;

  { Interactive decomposition chart that allows you to present different types of financial, statistical or general numeric data in a clear form
    of expandable bars. Sections in each bar can be expanded and collapsed using the mouse or keyboard, so the end-users can easily navigate through
    the hierarchical structure of data defined in the chart. Of course, TDecoBar can be used to display a simple (one-level) stacked bar chart. }
  TDecoBar = class(TCustomDecoBar)
  published
    // Setups the align in the parent control
    property Align;
    { See @inherited }
    property Alignment;
    { See @inherited }
    property AllowExpandingNodes;
    // Component anchors
    property Anchors;
    { See @inherited }
    property AutoCalcValues;
    { See @inherited }
    property AutoColors;
    { Controls auto-sizing of the component. TDecoBar allows auto-sizing only when Align property is set to alTop or alBottom. }
    property AutoSize;
    { See @inherited }
    property BarSize;
    property BevelEdges;
    property BevelKind;
    // Bidirectional text drawing mode
    property BiDiMode;
    // Border style of the componet
    property BorderStyle;
    { A chart title. See also CaptionFormat, CaptionFont and ShowCaption. }
    property Caption;
    { Font of the chart title }
    property CaptionFont;
    { See @inherited }
    property CaptionFormat;
    { See @inherited }
    property Color;
    { Component size constraints }
    property Constraints;
    property Ctl3D;
    { See @inherited }
    property CutExpandedBar;
    { See @inherited }
    property DarkenCollapsedBars;
    { Controls double-buffering of the component. TDecoBar component is always painted as double-buffered, but this property is important
      for other descendants (such as TDecoBarPlus), that use a composited rendering. }
    property DoubleBuffered;
    // Defines the drag cursor
    property DragCursor;
    // Defines the drag kind
    property DragKind;
    // Defines the drag mode
    property DragMode;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (item captions, legend, etc.). See also CaptionFont.
    property Font;
    { See @inherited }
    property FrameColor;
    { See @inherited }
    property FrameWidth;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property Gradient;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property Items;
    { See @inherited }
    property ItemTextFormat;
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
    // OnClick event
    property OnClick;
    { See @inherited }
    property OnCollapsed;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    { See @inherited }
    property OnExpanded;
    { See @inherited }
    property OnExpanding;
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
    property ShowCaption;
    { If True, the hints will be shown when the mouse is above the control }
    property ShowHint;
    { See @inherited }
    property ShowLegend;
    { See @inherited }
    property StripSize;
    property TabOrder;
    { TabStop property. TDecoBar shows the focus rectangle only when user enters the component via keyboard.
      When the user clicks the component with mouse, no focus rectangle is displayed. }
    property TabStop;
    { See @inherited }
    property TreeLinesColor;
    { See @inherited }
    property TreeLinesStyle;
    { Controls visibility of the control }
    property Visible;
  end;

implementation

uses
  DecoGDI {$IF CompilerVersion >= 24}, System.Types, System.UITypes {$IFEND};

//////////////////////////////////////////// TDecoBarItem ////////////////////////////////////////////
constructor TDecoBarItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clWhite;
  FTag := 0;
  FData := nil;
  FValue := 0;
  FCaption := '';
  FExpanded := False;
  SetRectEmpty(FItemRect);
  SetRectEmpty(FItemLegRect);
  FItems := TDecoBarItems.Create(Self);
end;

destructor TDecoBarItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TDecoBarItem.GetDisplayName: string;
begin
  Result:=FCaption;
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TDecoBarItem.Assign(Source: TPersistent);
begin
  if Source is TDecoBarItem then
  begin
    FColor := TDecoBarItem(Source).FColor;
    FTag := TDecoBarItem(Source).FTag;
    FData := TDecoBarItem(Source).FData;
    FCaption := TDecoBarItem(Source).FCaption;
    if FValue <> TDecoBarItem(Source).FValue then
      TDecoBarItems(Collection).FUpdateTotalsNeeded:=True;
    FValue := TDecoBarItem(Source).FValue;
    FExpanded := False;
    FItems.Assign(TDecoBarItem(Source).FItems);
    Changed(True);
  end
  else
    inherited;
end;

procedure TDecoBarItem.SetCaption(Value: string);
begin
  if Value<>FCaption then
  begin
    FCaption:=Value;
    Changed(False);
  end;
end;

procedure TDecoBarItem.SetColor(Value: TColor);
begin
  if Value<>FColor then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TDecoBarItem.SetExpanded(Value: Boolean);
begin
  if (Value<>FExpanded) then
  begin
    if Value then
      TDecoBarItems(Collection).ExpandedItem:=Self
    else
      TDecoBarItems(Collection).ExpandedItem:=nil;
  end;
end;

function TDecoBarItem.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TDecoBarItem.SetItems(const Value: TDecoBarItems);
begin
  FItems.Assign(Value);
end;

procedure TDecoBarItem.SetValue(NewValue: Extended);
begin
  if NewValue<>FValue then
  begin
    FValue:=NewValue;
    if FValue<0 then
      FValue:=0;
    TDecoBarItems(Collection).FUpdateTotalsNeeded:=True;
    Changed(True);
  end;
end;

//////////////////////////////////////////// TDecoBarItems ///////////////////////////////////////////
constructor TDecoBarItems.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  if FOwner is TCustomDecoBar then
    FDecoBar:=TCustomDecoBar(FOwner)
  else
  if FOwner is TDecoBarItem then
    FDecoBar:=TDecoBarItems(TDecoBarItem(FOwner).Collection).FDecoBar;
  inherited Create(TDecoBarItem);
  FTotalValue := 0;
  FUpdateTotalsNeeded:=False;
  FExpandedItem := nil;
end;

function TDecoBarItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDecoBarItems.Update(Item: TCollectionItem);
begin
  if FUpdateTotalsNeeded and DecoBar.AutoCalcValues then
    UpdateTotalValue;
  if FOwner is TCustomDecoBar then
  begin
    if Item=nil then
      TCustomDecoBar(FOwner).FRebuildNeeded:=True;
    TCustomDecoBar(FOwner).Invalidate;
  end
  else
  if FOwner is TDecoBarItem then
    TDecoBarItem(FOwner).Changed(Item=nil);
end;

{$IF CompilerVersion >= 15}
procedure TDecoBarItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnDeleting:
      begin
        if Item=FDecoBar.FSelectedItem then
          FDecoBar.SelectedItem:=nil;
        if Item=FDecoBar.FHoverItem then
          FDecoBar.HoverItem:=nil;
      end;
  end;
  inherited;
end;
{$ELSE}
procedure TDecoBarItems.Deleting(Item: TCollectionItem);
begin
  if Item=FDecoBar.FSelectedItem then
    FDecoBar.SelectedItem:=nil;
  if Item=FDecoBar.FHoverItem then
    FDecoBar.HoverItem:=nil;
  inherited;
end;
{$IFEND}

function TDecoBarItems.GetItem(Index: Integer): TDecoBarItem;
begin
  Result := inherited GetItem(Index) as TDecoBarItem;
end;

procedure TDecoBarItems.SetItem(Index: Integer; Value: TDecoBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TDecoBarItems.SetExpandedItem(Item:TDecoBarItem);
var
  i:Integer;
  a:Boolean;
begin
  if Assigned(FDecoBar.FOnExpanding) and Assigned(Item) then
  begin
    a:=True;
    FDecoBar.FOnExpanding(FDecoBar,Item,a);
    if not a then
      Exit;
  end;
  BeginUpdate;
  try
    if (not FDecoBar.FAllowExpandingNodes) and Assigned(Item) and (Item.Items.Count=0) then
      Item:=nil;
    for i:=0 To Count-1 do
      if (Items[i]<>Item) and Items[i].Expanded then
      begin
        Items[i].Items.ExpandedItem:=nil;
        Items[i].FExpanded:=False;
        if Assigned(FDecoBar.FOnCollapsed) then
          FDecoBar.FOnCollapsed(FDecoBar,Items[i]);
      end;
    FExpandedItem:=Item;
    if Assigned(Item) then
    begin
      Item.FExpanded:=True;
      if Assigned(FDecoBar.FOnExpanded) then
        FDecoBar.FOnExpanded(FDecoBar,Item);
    end;
  finally
    EndUpdate;
  end;
end;

function TDecoBarItems.Add: TDecoBarItem;
begin
  Result := inherited Add as TDecoBarItem;
end;

function TDecoBarItems.Insert(Index:Integer): TDecoBarItem;
begin
  Result := inherited Insert(Index) as TDecoBarItem;
end;

procedure TDecoBarItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TDecoBarItems.Move(CurrIndex, NewIndex: Integer);
begin
  Items[CurrIndex].Index := NewIndex;
end;

function TDecoBarItems.AddItem(const Caption:string; Value:Extended=0; Color:TColor = clWhite; Tag:Longint=0; Data:Pointer=nil): TDecoBarItem;
begin
  Result := inherited Add as TDecoBarItem;
  Result.FColor := Color;
  Result.FCaption := Caption;
  Result.FTag := Tag;
  Result.FData := Data;
  Result.Value := Value;
end;

procedure TDecoBarItems.Collapse;
begin
  ExpandedItem:=nil;
end;

procedure TDecoBarItems.UpdateTotalValue;
var
  i:Integer;
begin
  FUpdateTotalsNeeded:=False;
  FTotalValue:=0;
  for i:=0 To Count-1 do
    FTotalValue:=FTotalValue+Items[i].FValue;
  if Owner is TDecoBarItem then
    TDecoBarItem(Owner).Value:=FTotalValue;
end;

//////////////////////////////////////////// TCustomDecoBar //////////////////////////////////////////
constructor TCustomDecoBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable, csSetCaption {$IF CompilerVersion >= 20}, csPannable {$IFEND}];
  FDrawTextFunc := DoDrawNormalText;
  FItems := TDecoBarItems.Create(Self);
  FCaptionFont := TFont.Create;
  FActive:=False;
  FBorderStyle := bsNone;
  FGlowSize := 0;
  FFrameColor := clBtnFace;
  FFrameWidth := 1;
  FBarSize := 20;
  FStripSize := 20;
  FRounded := True;
  FRoundCorners := 0;
  FGradient := True;
  FAlignment := taCenter;
  FShowCaption := True;
  FShowLegend := True;
  FCaptionFormat := '%s';
  FItemTextFormat := '%s: %.0f';
  FTreeLinesColor := clBtnShadow;
  FTreeLinesStyle := tlsRectangular;
  FExpandedItems:=Items;
  FHoverItem:=nil;
  FSelectedItem:=nil;
  FHoverCursor := crDefault;
  FCutExpandedBar := True;
  FDarkenCollapsedBars := True;
  FAutoCalcValues := False;
  FAutoColors := True;
  FAllowExpandingNodes:=False;
  _EMFExporting:=False;

  SetLength(AutoColorPalette,6);
  AutoColorPalette[0]:=$EA7954;
  AutoColorPalette[1]:=$00ADFF;
  AutoColorPalette[2]:=$D66A9A;
  AutoColorPalette[3]:=$24CA8B;
  AutoColorPalette[4]:=$6E53E8;
  AutoColorPalette[5]:=$00EFFF;
  FIsCaptionHovered:=False;
  FIsLegendHovered:=False;
  FShowingFocusRect := False;
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

destructor TCustomDecoBar.Destroy;
begin
  FItems.Free;
  FCaptionFont.Free;
  SetLength(AutoColorPalette,0);
  inherited;
end;

procedure TCustomDecoBar.CreateParams(var Params: TCreateParams);
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

procedure TCustomDecoBar.Loaded;
begin
  inherited;
  if FAutoCalcValues then
    RecalcAll;
end;

procedure TCustomDecoBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomDecoBar.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FActive:=False;
  Invalidate;
end;

procedure TCustomDecoBar.WMSetCursor(var Message: TWMSetCursor);
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

procedure TCustomDecoBar.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FActive:=True;
  Invalidate;
end;

procedure TCustomDecoBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  FRebuildNeeded:=True;
  Invalidate;
  inherited;
end;

procedure TCustomDecoBar.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoBar.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TCustomDecoBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoBar.CMFontChanged(var Message: TMessage);
begin
  FRebuildNeeded:=True;
  inherited;
end;

procedure TCustomDecoBar.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FHoverItem) then
    HoverItem := nil;
  inherited;
end;

procedure TCustomDecoBar.CMParentFontChanged(var Message: {$IF CompilerVersion >= 20} TCMParentFontChanged {$ELSE} TMessage {$IFEND});
begin
  inherited;
  if ParentFont then
    FCaptionFont.Assign(Font);
end;

procedure TCustomDecoBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoBar.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  if (Message.CharCode in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_HOME, VK_END, VK_NEXT, VK_PRIOR]) then
    Message.Result := 1
  else
  if (Message.CharCode = VK_TAB) and FActive then
  begin
    FShowingFocusRect:=True;
    if (FSelectedItem=nil) and (Items.Count>0) then
      SelectedItem:=Items[0]
    else
      Invalidate;
  end;
end;

{$IF CompilerVersion >= 20}
procedure TCustomDecoBar.DoPaddingChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Realign;
  Invalidate;
end;
{$IFEND}

procedure TCustomDecoBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  i:Integer;
begin
  inherited KeyDown(Key,Shift);
  if Shift=[ssCtrl] then
    case Key of
      VK_HOME:
        if Items.Count>0 then
        begin
          FShowingFocusRect:=True;
          SelectedItem:=Items[0];
        end;
      VK_END:
        if FExpandedItems.Count>0 then
        begin
          FShowingFocusRect:=True;
          SelectedItem:=FExpandedItems.Items[FExpandedItems.Count-1];
        end
        else
        if FExpandedItems.Owner is TDecoBarItem then
        begin
          FShowingFocusRect:=True;
          SelectedItem:=TDecoBarItem(FExpandedItems.Owner);
        end;
    end
  else
  if Shift=[] then
    case Key of
      VK_SPACE,VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_HOME,VK_END,VK_NEXT,VK_PRIOR:
        begin
          if (not FShowingFocusRect) and Assigned(FSelectedItem) then
          begin
            FShowingFocusRect:=True;
            Invalidate;
          end
          else
          begin
            FShowingFocusRect:=True;
            case Key of
              VK_SPACE:
                if Assigned(FSelectedItem) then
                  FSelectedItem.Expanded:=not FSelectedItem.Expanded;
              VK_RIGHT:
                if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[0]; end
                else
                if FSelectedItem.Index<FSelectedItem.Collection.Count-1 then
                  SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Items[FSelectedItem.Index+1]);
              VK_LEFT:
                if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[Items.Count-1]; end
                else
                if FSelectedItem.Index>0 then
                  SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Items[FSelectedItem.Index-1]);
              VK_UP:
                if FSelectedItem=nil then
                begin
                  if FExpandedItems.Count>0 then
                    SelectedItem:=FExpandedItems.Items[FExpandedItems.Count-1]
                  else
                  if FExpandedItems.Owner is TDecoBarItem then
                    SelectedItem:=TDecoBarItem(FExpandedItems.Owner);
                end
                else
                if FSelectedItem.Collection.Owner is TDecoBarItem then
                  SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Owner);
              VK_DOWN:
                if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[0]; end
                else
                if FSelectedItem.Expanded then
                begin
                  if FSelectedItem.Items.ExpandedItem<>nil then
                    SelectedItem:=FSelectedItem.Items.ExpandedItem
                  else
                  if FSelectedItem.Items.Count>0 then
                    SelectedItem:=FSelectedItem.Items[0]
                end
                else
                if (FSelectedItem.Items.Count>0) or FAllowExpandingNodes then
                  FSelectedItem.Expanded:=True
                else
                if (TDecoBarItems(FSelectedItem.Collection).ExpandedItem<>nil) and (TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.Count>0) then
                  if TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.ExpandedItem<>nil then
                    SelectedItem:=TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.ExpandedItem
                  else
                  if TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.Count>0 then
                    SelectedItem:=TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items[0];
              VK_HOME:
                if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[0]; end
                else
                  SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Items[0]);
              VK_END:
                if FSelectedItem=nil then
                begin
                  if FExpandedItems.Count>0 then
                    SelectedItem:=FExpandedItems.Items[FExpandedItems.Count-1]
                  else
                  if FExpandedItems.Owner is TDecoBarItem then
                    SelectedItem:=TDecoBarItem(FExpandedItems.Owner);
                end
                else
                  SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Items[FSelectedItem.Collection.Count-1]);
              VK_PRIOR:
                if FSelectedItem=nil then
                begin
                  if FExpandedItems.Count>0 then
                    SelectedItem:=FExpandedItems.Items[FExpandedItems.Count-1]
                  else
                  if FExpandedItems.Owner is TDecoBarItem then
                    SelectedItem:=TDecoBarItem(FExpandedItems.Owner);
                end
                else
                if FSelectedItem.Collection.Owner is TDecoBarItem then
                begin
                  i:=FSelectedItem.Index;
                  if i<TDecoBarItem(FSelectedItem.Collection.Owner).Collection.Count then
                    SelectedItem:=TDecoBarItem(TDecoBarItem(FSelectedItem.Collection.Owner).Collection.Items[i])
                  else
                    SelectedItem:=TDecoBarItem(TDecoBarItem(FSelectedItem.Collection.Owner).Collection.Items[
                      TDecoBarItem(FSelectedItem.Collection.Owner).Collection.Count-1]);
                end;
              VK_NEXT:
                if FSelectedItem=nil then begin if Items.Count>0 then SelectedItem:=Items[0]; end
                else
                if TDecoBarItems(FSelectedItem.Collection).ExpandedItem<>nil then
                begin
                  i:=FSelectedItem.Index;
                  if i<TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.Count then
                    SelectedItem:=TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items[i]
                  else
                  if TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.Count>0 then
                    SelectedItem:=TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items[TDecoBarItems(FSelectedItem.Collection).ExpandedItem.Items.Count-1];
                end;
            end;
          end;
        end;
      VK_ESCAPE:
        if Assigned(FSelectedItem) then
        begin
          if FSelectedItem.Expanded and FAllowExpandingNodes and (FSelectedItem.Items.Count=0) then
            FSelectedItem.Expanded:=False
          else
          if FSelectedItem.Collection.Owner is TDecoBarItem then
          begin
            SelectedItem:=TDecoBarItem(FSelectedItem.Collection.Owner);
            FSelectedItem.Expanded:=False;
          end
          else
          begin
            FShowingFocusRect:=False;
            if FSelectedItem.Expanded then
              FSelectedItem.Expanded:=False;
            SelectedItem:=nil;
          end;
        end;
    end;
end;

procedure TCustomDecoBar.UpdateHoverInfo(X, Y: Integer);
begin
  if PtInRect(FCaptionRect,point(x,y)) then
  begin
    if not FIsCaptionHovered then FIsCaptionHovered:=True;
    HoverItem := nil;
  end
  else
  begin
    if FIsCaptionHovered then FIsCaptionHovered:=False;
    if (not Assigned(FHoverItem)) or
      (not (PtInRect(FHoverItem.FItemRect,Point(x,y)) or PtInRect(FHoverItem.FItemLegRect,Point(x,y)))) then
    begin
      HoverItem := GetItemAt(X,Y);
      if Assigned(FHoverItem) then
        FIsLegendHovered:=PtInRect(FHoverItem.FItemLegRect,Point(x,y));
    end
  end;
end;

function TCustomDecoBar._GetControlSize:Integer;
begin
  Result:={$IF CompilerVersion >= 20} Padding.Top+Padding.Bottom {$ELSE} 8 {$IFEND}+(FCaptionRect.Bottom-FCaptionRect.Top)+
    (FBarsRect.Bottom-FBarsRect.Top)+(FLegendRect.Bottom-FLegendRect.Top);
end;

function TCustomDecoBar._GetRoundedCornersFactor:Integer;
begin
  if FRoundCorners<=0 then
    Result:=FBarSize - FBarSize div 5
  else
    Result:=FRoundCorners;
end;

function TCustomDecoBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if Align in [alTop,alBottom] then
  begin
    Result:=True;
    NewHeight:=_GetControlSize;
  end
  else
    Result:=False;
end;

procedure TCustomDecoBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not FActive) and HandleAllocated and Visible and (not (ssDouble in Shift)) then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
  FShowingFocusRect:=False;
  UpdateHoverInfo(X,Y);
  SelectedItem:=FHoverItem;
  if (button=mbLeft) and (Shift=[ssLeft]) and Assigned(FHoverItem) and (not FIsLegendHovered) then
    FHoverItem.Expanded:=not FHoverItem.Expanded;
end;

procedure TCustomDecoBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateHoverInfo(X,Y);
end;

procedure TCustomDecoBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (button=mbLeft) and (Shift*[ssCtrl,ssShift,ssAlt]=[]) and Assigned(FHoverItem) and (FIsLegendHovered) then
  begin
    FHoverItem.Expanded:=not FHoverItem.Expanded;
    Rebuild(ClientRect);
    FHoverItem:=nil;
    UpdateHoverInfo(X,Y);
  end;
  if (button=mbLeft) and Assigned(FOnItemClick) and Assigned(FHoverItem) then
    FOnItemClick(Self,FHoverItem);
end;

procedure TCustomDecoBar.SetHoverItem(const Value:TDecoBarItem);
begin
  if Value<>FHoverItem then
  begin
    FHoverItem:=Value;
    if FHoverItem=nil then FIsLegendHovered:=False;
    if Assigned(FOnHoverChange) then
      FOnHoverChange(Self,FHoverItem);
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetSelectedItem(const Value:TDecoBarItem);
begin
  if Value<>FSelectedItem then
  begin
    FSelectedItem:=Value;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self,FSelectedItem);
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetAlignment(Value: TAlignment);
begin
  if Value<>FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomDecoBar.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

procedure TCustomDecoBar.CaptionFontChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Invalidate;
end;

function TCustomDecoBar.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TCustomDecoBar.SetItems(const Value: TDecoBarItems);
begin
  FItems.Assign(Value);
end;

procedure TCustomDecoBar.SetGlowSize(Value: Integer);
begin
  if Value <> FGlowSize then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TCustomDecoBar.SetFrameColor(Value: TColor);
begin
  if Value<>FFrameColor then
  begin
    FFrameColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetFrameWidth(Value: Integer);
begin
  if Value<>FFrameWidth then
  begin
    FFrameWidth:=Value;
    if FShowLegend then
      FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetBarSize(Value: Integer);
begin
  if Value<>FBarSize then
  begin
    FBarSize:=Value;
    if FBarSize<1 then
      FBarSize:=1;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetStripSize(Value: Integer);
begin
  if Value<>FStripSize then
  begin
    FStripSize:=Value;
    if FStripSize in [1,2] then
      FStripSize:=3;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetRounded(Value: Boolean);
begin
  if Value<>FRounded then
  begin
    FRounded:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetRoundCorners(Value: Integer);
begin
  if Value<>FRoundCorners then
  begin
    FRoundCorners:=Value;
    if FRoundCorners<0 then
      FRoundCorners:=0;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetGradient(Value: Boolean);
begin
  if Value<>FGradient then
  begin
    FGradient:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetShowCaption(Value: Boolean);
begin
  if Value<>FShowCaption then
  begin
    FShowCaption:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetShowLegend(Value: Boolean);
begin
  if Value<>FShowLegend then
  begin
    FShowLegend:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetCutExpandedBar(Value: Boolean);
begin
  if Value<>FCutExpandedBar then
  begin
    FCutExpandedBar:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetDarkenCollapsedBars(Value: Boolean);
begin
  if Value<>FDarkenCollapsedBars then
  begin
    FDarkenCollapsedBars:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetTreeLinesColor(Value: TColor);
begin
  if Value<>FTreeLinesColor then
  begin
    FTreeLinesColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetTreeLinesStyle(Value: TDecoBarTreeLinesStyle);
begin
  if Value<>FTreeLinesStyle then
  begin
    FTreeLinesStyle:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoBar.SetAutoCalcValues(Value: Boolean);
begin
  if Value<>FAutoCalcValues then
    FAutoCalcValues:=Value;
end;

procedure TCustomDecoBar.SetAutoColors(Value: Boolean);
begin
  if Value<>FAutoColors then
  begin
    FAutoColors:=Value;
    Invalidate;
  end;
end;

function TCustomDecoBar.GetCaptionFormatted:string;
begin
  if Length(FCaptionFormat)=0 then
    Result:=Caption
  else
    Result:=Format(stringReplace(FCaptionFormat,'%s',Caption,[]),[TotalValue])
end;

procedure TCustomDecoBar.SetCaptionFormat(const Value: string);
var
  o:string;
begin
  if Value<>FCaptionFormat then
  begin
    o:=FCaptionFormat;
    FCaptionFormat:=Value;
    try
      GetCaptionFormatted;
      Invalidate;
    except
      FCaptionFormat:=o;
    end;
  end;
end;

function TCustomDecoBar.GetItemTextFormatted(Item:TDecoBarItem):string;
begin
  if Item=nil then
  begin
    if Length(FItemTextFormat)=0 then
      Result:=''
    else
      Result:=Format(stringReplace(FItemTextFormat,'%s','',[]),[1.5]) // for new ItemTextFormat testing
  end
  else
  if Length(FItemTextFormat)=0 then
    Result:=Item.Caption
  else
    Result:=Format(stringReplace(FItemTextFormat,'%s',Item.Caption,[]),[Item.Value]);
end;

procedure TCustomDecoBar.SetItemTextFormat(const Value: string);
var
  o:string;
begin
  if Value<>FItemTextFormat then
  begin
    o:=FItemTextFormat;
    FItemTextFormat:=Value;
    try
      GetItemTextFormatted(nil);
      Invalidate;
    except
      FItemTextFormat:=o;
    end;
  end;
end;

procedure TCustomDecoBar.SetOnBeforePaint(Value: TDecoPaintEvent);
begin
  FOnBeforePaint:=Value;
  Invalidate;
end;

procedure TCustomDecoBar.SetOnAfterPaint(Value: TDecoPaintEvent);
begin
  FOnAfterPaint:=Value;
  Invalidate;
end;

function TCustomDecoBar.GetItemAt(X,Y:Integer):TDecoBarItem;
var
  p:TPoint;
  i:Integer;
  L:TDecoBarItems;
begin
  Result:=nil;
  p.x:=x; p.y:=y;
  if PtInRect(FLegendRect,p) then
  begin
    for i:=0 To FExpandedItems.Count-1 do
      if PtInRect(FExpandedItems.Items[i].FItemLegRect,p) then
      begin
        Result:=FExpandedItems.Items[i];
        break;
      end;
  end
  else
  begin
    L:=FExpandedItems;
    repeat
      for i:=0 To L.Count-1 do
        if PtInRect(L.Items[i].FItemRect,p) then
        begin
          Result:=L.Items[i];
          break;
        end;
      if (Result=nil) and (L.Owner is TDecoBarItem) then
        L:=TDecoBarItems(TDecoBarItem(L.Owner).Collection)
      else
        L:=nil;
    until L=nil;
  end;
end;

function TCustomDecoBar.GetTotalValue:Extended;
begin
  Result:=Items.TotalValue;
end;

procedure TCustomDecoBar.FullCollapse;
begin
  Items.Collapse;
end;

procedure TCustomDecoBar.RecalcAll;
var
  tmp:Boolean;

  procedure _SetRecalcNeeded(L:TDecoBarItems);
  var
    i:Integer;
  begin
    if L.Count>0 then
    begin
      L.BeginUpdate;
      try
        L.FUpdateTotalsNeeded:=True;
        for i:=0 To L.Count-1 do
          _SetRecalcNeeded(L.Items[i].Items);
      finally
        L.EndUpdate;
      end;
    end;
  end;

begin
  if Items.Count=0 then
    Items.UpdateTotalValue
  else
  begin
    tmp:=FAutoCalcValues;
    FAutoCalcValues:=True;
    try
      _SetRecalcNeeded(Items);
    finally
      FAutoCalcValues:=tmp;
    end;
  end;
end;

function TCustomDecoBar.GetAutoColorIndex(L:TDecoBarItems):Integer;
var
  P:TPersistent;
begin
  Result:=-1;
  if FAutoColors and (Length(AutoColorPalette)>0) then
  begin
    Result:=0;
    P:=L.Owner;
    while P is TDecoBarItem do
    begin
      Inc(Result,TDecoBarItem(P).Index);
      while Result>=Length(AutoColorPalette) do
        Result:=Result-Length(AutoColorPalette);
      P:=TDecoBarItem(P).Collection.Owner;
    end;
  end;
end;

procedure TCustomDecoBar.Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil);
var
  TM:TTextMetric;
  j,ly,lyf:Integer;
  oldSize:Integer;

  function RebuildLevel(L:TDecoBarItems; y:Integer):integer;
  var
    x,i,c,offset:Integer;
    totValue:Extended;
    totWidth:Integer;
    Item:TDecoBarItem;
  begin
    Result:=y;
    L.FBarsRect:=FBarsRect;
    L.FBarsRect.Top:=y;
    L.FBarsRect.Bottom:=y+FBarSize;
    if L.Count>0 then
    begin
      c:=L.Count;
      totValue:=0;
      for i:=0 To c-1 do
        totValue:=totValue+L[i].Value;
      totWidth:=FBarsRect.Right-FBarsRect.Left;
      x:=FBarsRect.Left;
      for i:=0 To c-1 do
      begin
        Item:=L[i];
        if i=c-1 then
          offset:=FBarsRect.Right-x
        else
        if totValue=0 then
          offset:=totWidth div c
        else
          offset:=trunc(Item.Value/totValue*totWidth);
        if offset<1 then
        begin
          offset:=1;
          Dec(x);
        end;
        SetRect(Item.FItemRect,x,y,x+offset,y+FBarSize);
        Inc(x,offset);
        if Item.Expanded then
        begin
          FExpandedItems:=Item.Items;
          Result:=RebuildLevel(Item.Items,y+FBarSize+TM.tmHeight+FSpacing*2);
        end
        else
        begin
          Item.Items.FBarsRect:=FBarsRect;
          Item.Items.FBarsRect.Top:=y+FBarSize+TM.tmHeight+FSpacing*2;
          Item.Items.FBarsRect.Bottom:=Item.Items.FBarsRect.Top+FBarSize;
        end;
      end;
    end;
  end;

begin
  if not FRebuildNeeded then
    Exit;
  FRebuildNeeded:=False;

  if (TargetCanvas=nil) and AutoSize and (Align in [alTop,alBottom]) then
    oldSize:=_GetControlSize
  else
    oldSize:=-1;
  if TargetCanvas=nil then
    TargetCanvas:=Canvas;

  SetRectEmpty(FCaptionRect);
  SetRectEmpty(FLegendRect);
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
  FSpacing:=TM.tmDescent*2;

  FExpandedItems:=Items;
  FBarsRect.Bottom:=RebuildLevel(Items,FBarsRect.Top)+FBarSize+TM.tmHeight+FSpacing*2;

  if FExpandedItems.Count=0 then
    Dec(FBarsRect.Bottom,FBarSize+TM.tmHeight+FSpacing*2);

  if FShowLegend and (FExpandedItems.Count>0) then
  begin
    FLegendRect:=FBarsRect;
    ly:=FBarsRect.Bottom + (FSpacing div 4);
    lyf:=TM.tmHeight + (FSpacing div 4);
    FLegendRect.Top:=ly;
    for j:=0 To FExpandedItems.Count-1 do
    begin
      SetRect(FExpandedItems.Items[j].FItemLegRect,FBarsRect.Left,ly,FBarsRect.Right,ly+lyf);
      Inc(ly,lyf);
    end;
    FLegendRect.Bottom:=ly+FFrameWidth;
  end;

  if (oldsize>=0) and AutoSize and (Align in [alTop,alBottom]) and (oldSize<>_GetControlSize) then
    AdjustSize;
end;

procedure TCustomDecoBar.DrawItemText(TargetCanvas:TCanvas; Item:TDecoBarItem);
var
  txtRect,tmpRect:TRect;
  tmpStr:string;
  Flags:Longint;
  rcs,scs:Integer;
  pl,pr:array[0..3] of TPoint;
begin
  if Assigned(Item) then
  begin
    txtRect:=Item.FItemRect;
    txtRect.Top:=txtRect.Bottom+FSpacing;
    txtRect.Bottom:=Item.Items.FBarsRect.Top-FSpacing div 2;
    TargetCanvas.Brush.Style:=bsClear;
    if Enabled then
      TargetCanvas.Font.Color:=Font.Color
    else
      TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
    TargetCanvas.Font.Style:=Font.Style;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_CENTER;
    tmpStr:=GetItemTextFormatted(Item);
    tmpRect:=txtRect;
    DoDrawNormalText(TargetCanvas.Handle, tmpStr, tmpRect, Flags or DT_NOCLIP or DT_CALCRECT); // Do not use FDrawTextFunc here, due to a correct measuring
    txtRect.Right:=tmpRect.Right; txtRect.Left:=tmpRect.Left;
    offsetRect(txtRect,-((txtRect.Right-txtRect.Left)-(Item.FItemRect.Right-Item.FItemRect.Left)) div 2,0);
    if txtRect.Right>FBarsRect.Right-FSpacing then offsetRect(txtRect,-(txtRect.Right-(FBarsRect.Right-FSpacing)),0);
    if txtRect.Left<FBarsRect.Left+FSpacing then
    begin
      offsetRect(txtRect,(FBarsRect.Left+FSpacing)-txtRect.Left,0);
      if txtRect.Right>FBarsRect.Right-FSpacing then
        txtRect.Right:=FBarsRect.Right-FSpacing;
    end;

    if Item.Expanded and (FTreeLinesColor<>clNone) and (FTreeLinesStyle<>tlsNone) then
    begin
      if Enabled then
        TargetCanvas.Pen.Color:=FTreeLinesColor
      else
        TargetCanvas.Pen.Color:=GrayScaleColor(FTreeLinesColor);
      if FFrameWidth=0 then
        TargetCanvas.Pen.Width:=1
      else
        TargetCanvas.Pen.Width:=FFrameWidth;
      pl[0].x:=Item.FItemRect.Left; if pl[0].x>Item.Items.FBarsRect.Left then Dec(pl[0].x);
      pl[0].y:=Item.FItemRect.Bottom+2; if FCutExpandedBar and (pl[0].x<=Item.Items.FBarsRect.Left) then Dec(pl[0].y,FBarSize div 3);

      pl[1].x:=pl[0].x; pl[1].y:=Item.FItemRect.Bottom+FSpacing-1;
      pl[2].x:=Item.Items.FBarsRect.Left; pl[2].y:=pl[1].y;
      pl[3].x:=pl[2].x; pl[3].y:=Item.Items.FBarsRect.Top-2;

      pr[0].x:=Item.FItemRect.Right; if pr[0].x>Item.Items.FBarsRect.Right-1 then pr[0].x:=Item.Items.FBarsRect.Right-1;
      pr[0].y:=Item.FItemRect.Bottom+2; if FCutExpandedBar and (pr[0].x>=Item.Items.FBarsRect.Right-1) then Dec(pr[0].y,FBarSize div 3);

      pr[1].x:=pr[0].x; pr[1].y:=pl[1].y;
      pr[2].x:=Item.Items.FBarsRect.Right-1; pr[2].y:=pl[2].y;
      pr[3].x:=pr[2].x; pr[3].y:=pl[3].y;

      case FTreeLinesStyle of
        tlsRectangular:
          begin
            TargetCanvas.Polyline(pl);
            TargetCanvas.Polyline(pr);
          end;
        tlsRoundedRect:
          begin
            if pl[0].x<FBarsRect.Left+1 then
              TargetCanvas.Polyline(pl)
            else
            begin
              rcs:=_GetRoundedCornersFactor;
              scs:=FSpacing;
              if pl[1].x-scs-rcs<pl[2].x then
              begin
                scs:=(pl[1].x-pl[2].x) div 4;
                rcs:=scs*3;
              end;
              TargetCanvas.PolyBezier([pl[0],pl[1],pl[1],point(pl[1].x-scs,pl[1].y)]);
              TargetCanvas.Polyline([point(pl[1].x-scs,pl[1].y),point(pl[2].x+rcs,pl[2].y)]);
              TargetCanvas.PolyBezier([point(pl[2].x+rcs,pl[2].y),pl[2],pl[2],point(pl[2].x,pl[2].y+rcs)]);
              TargetCanvas.Polyline([point(pl[2].x,pl[2].y+rcs),pl[3]]);
            end;
            if pr[0].x>FBarsRect.Right-4 then
              TargetCanvas.Polyline(pr)
            else
            begin
              rcs:=_GetRoundedCornersFactor;
              scs:=FSpacing;
              if pr[1].x+scs+rcs>pr[2].x then
              begin
                scs:=(pr[2].x-pr[1].x) div 4;
                rcs:=scs*3;
              end;
              TargetCanvas.PolyBezier([pr[0],pr[1],pr[1],point(pr[1].x+scs,pr[1].y)]);
              TargetCanvas.Polyline([point(pr[1].x+scs,pr[1].y),point(pr[2].x-rcs,pr[2].y)]);
              TargetCanvas.PolyBezier([point(pr[2].x-rcs,pr[2].y),pr[2],pr[2],point(pr[2].x,pr[2].y+rcs)]);
              TargetCanvas.Polyline([point(pr[2].x,pr[2].y+rcs),pr[3]]);
            end;
          end;
        tlsCurved:
          begin
            if txtRect.Left-FSpacing<Item.FItemRect.Left then pl[1].x:=txtRect.Left-FSpacing;
            if txtRect.Right+FSpacing>Item.FItemRect.Right then
            begin
              pr[1].x:=txtRect.Right+FSpacing; if pr[1].x>FBarsRect.Right-1 then pr[1].x:=FBarsRect.Right-1;
            end;
            pl[2].y:=Item.FItemRect.Bottom; pr[2].y:=pl[2].y;
            TargetCanvas.PolyBezier(pl);
            TargetCanvas.PolyBezier(pr);
          end;
        tlsStrightLines:
          begin
            if pl[0].x>=FBarsRect.Left+1 then
            begin
              pl[2].x:=txtRect.Left-1;
              if pl[2].x>pl[1].x then pl[2].x:=pl[1].x;
            end;
            if pr[0].x<=FBarsRect.Right-4 then
            begin
              pr[2].x:=txtRect.Right+1;
              if pr[2].x<pr[1].x then pr[2].x:=pr[1].x;
            end;
            TargetCanvas.Polyline(pl);
            TargetCanvas.Polyline(pr);
          end;
      end;
    end;

    FDrawTextFunc(TargetCanvas.Handle, tmpStr, txtRect, Flags or DT_END_ELLIPSIS, TargetCanvas.Font.Color, GlowSize);
  end;
end;

procedure TCustomDecoBar.DrawCaption(TargetCanvas:TCanvas);
var
  Flags: Longint;
begin
  TargetCanvas.Font:=FCaptionFont;
  if not Enabled then
    TargetCanvas.Font.Color:=GrayScaleColor(FCaptionFont.Color);
  TargetCanvas.Brush.Style:=bsClear;
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS or DrawTextAlignments[FAlignment]);
  FDrawTextFunc(TargetCanvas.Handle, GetCaptionFormatted, FCaptionRect, Flags, TargetCanvas.Font.Color, GlowSize);
end;

procedure TCustomDecoBar.DrawItems(TargetCanvas:TCanvas; L:TDecoBarItems);
var
  i,j:Integer;
  Item,expItem:TDecoBarItem;
  RC:HRGN;
  tmpClr:TColor;
  rcs:Integer;
  clIdx:Integer;
begin
  if L.Count>0 then
  begin
    expItem:=L.ExpandedItem;
    rcs:=_GetRoundedCornersFactor;
    if FRounded then
    begin
      RC := CreateRoundRectRgn(L.FBarsRect.Left,L.FBarsRect.Top,L.FBarsRect.Right+1,L.FBarsRect.Bottom+1,rcs,rcs);
      SelectClipRgn(TargetCanvas.Handle,RC);
    end
    else
      RC:=0;
    if FCutExpandedBar and Assigned(expItem) then
      ExcludeClipRect(TargetCanvas.Handle,expItem.FItemRect.Left,expItem.FItemRect.Bottom-FBarSize div 3,expItem.FItemRect.Right,expItem.FItemRect.Bottom);
    j:=L.FBarsRect.Left+FStripSize;
    clIdx:=GetAutoColorIndex(L);

    for i:=0 To L.Count-1 do
    begin
      Item:=L.Items[i];
      if clIdx>=0 then
      begin
        if clIdx>=Length(AutoColorPalette) then
          clIdx:=0;
        if Item.Expanded or (expItem=nil) or (not FDarkenCollapsedBars) then
          tmpClr:=AutoColorPalette[clIdx]
        else
          tmpClr:=ModColorHLS(AutoColorPalette[clIdx],0,-30,-10);
        Inc(clIdx);
      end
      else
      if Item.Expanded or (expItem=nil) or (not FDarkenCollapsedBars) then
        tmpClr:=Item.Color
      else
        tmpClr:=ModColorHLS(Item.Color,0,-30,-10);
      if not Enabled then
        tmpClr:=GrayScaleColor(tmpClr);
      if FGradient then
      begin
        FillGradient(TargetCanvas,Item.FItemRect,BrightColor(tmpClr,15),DarkColor(tmpClr,8));
        if FStripSize>0 then
        begin
          while j<=Item.FItemRect.Right do
          begin
            FillGradient(TargetCanvas,Rect(j-1,Item.FItemRect.Top,j,Item.FItemRect.Bottom),tmpClr,DarkColor(tmpClr,23));
            FillGradient(TargetCanvas,Rect(j,Item.FItemRect.Top,j+1,Item.FItemRect.Bottom),BrightColor(tmpClr,20),DarkColor(tmpClr,3));
            Inc(j,FStripSize);
          end;
        end;
      end
      else
      begin
        TargetCanvas.Brush.Style:=bsSolid;
        TargetCanvas.Brush.Color:=tmpClr;
        TargetCanvas.Pen.Color:=tmpClr;
        TargetCanvas.Pen.Style:=psSolid;
        TargetCanvas.Pen.Width:=1;
        TargetCanvas.Rectangle(Item.FItemRect);
        if FStripSize>0 then
        begin
          while j<=Item.FItemRect.Right do
          begin
            TargetCanvas.Pen.Color:=DarkColor(tmpClr,15);
            TargetCanvas.MoveTo(j-1,Item.FItemRect.Top); TargetCanvas.LineTo(j-1,Item.FItemRect.Bottom);
            TargetCanvas.Pen.Color:=BrightColor(tmpClr,5);
            TargetCanvas.MoveTo(j,Item.FItemRect.Top); TargetCanvas.LineTo(j,Item.FItemRect.Bottom);
            Inc(j,FStripSize);
          end;
        end;
      end;
    end;
    if FRounded or (FCutExpandedBar and Assigned(expItem)) then
    begin
      SelectClipRgn(TargetCanvas.Handle, HRGN(nil));
      if RC<>0 then
        DeleteObject(RC);
    end;

    // draw frame
    if _EMFExporting or ((FFrameColor<>clNone) and (FFrameWidth>0)) then
    begin
      if FCutExpandedBar and Assigned(expItem) then
        ExcludeClipRect(TargetCanvas.Handle,expItem.FItemRect.Left,expItem.FItemRect.Bottom-FBarSize div 3,expItem.FItemRect.Right,expItem.FItemRect.Bottom+FFrameWidth);
      TargetCanvas.Brush.Style:=bsClear;
      TargetCanvas.Pen.Style:=psSolid;
      if _EMFExporting then
      begin
        TargetCanvas.Pen.Color:=Color;
        TargetCanvas.Pen.Width:=3;
        if FRounded then
          TargetCanvas.RoundRect(L.FBarsRect.Left-1,L.FBarsRect.Top-1,L.FBarsRect.Right+1,L.FBarsRect.Bottom+1,rcs,rcs)
        else
          TargetCanvas.Rectangle(L.FBarsRect.Left-1,L.FBarsRect.Top-1,L.FBarsRect.Right+1,L.FBarsRect.Bottom+1);
      end;
      if (FFrameColor<>clNone) and (FFrameWidth>0) then
      begin
        if Enabled then
          TargetCanvas.Pen.Color:=FFrameColor
        else
          TargetCanvas.Pen.Color:=GrayScaleColor(FFrameColor);
        TargetCanvas.Pen.Width:=FFrameWidth;
        if FRounded then
          TargetCanvas.RoundRect(L.FBarsRect.Left,L.FBarsRect.Top,L.FBarsRect.Right,L.FBarsRect.Bottom,rcs,rcs)
        else
          TargetCanvas.Rectangle(L.FBarsRect.Left,L.FBarsRect.Top,L.FBarsRect.Right,L.FBarsRect.Bottom);
      end;
      if FCutExpandedBar and Assigned(expItem) then
        SelectClipRgn(TargetCanvas.Handle, HRGN(nil));
    end;

    if expItem<>nil then
    begin
      // draw expanded item text and lines
      DrawItemText(TargetCanvas,expItem);
      // go into the next level
      DrawItems(TargetCanvas,expItem.Items);
    end;
  end;
end;

procedure TCustomDecoBar.DrawLegend(TargetCanvas:TCanvas; L:TDecoBarItems);
var
  i,ls:Integer;
  Item:TDecoBarItem;
  tmpClr:TColor;
  mRect,txtRect:TRect;
  Flags: Longint;
  rcs,clIdx:Integer;
begin
  TargetCanvas.Font := Font;
  if not Enabled then
    TargetCanvas.Font.Color:=GrayScaleColor(Font.Color);
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT);
  rcs := _GetRoundedCornersFactor;
  clIdx:=GetAutoColorIndex(L);

  for i:=0 To L.Count-1 do
  begin
    Item:=L.Items[i];
    txtRect:=Item.FItemLegRect;
    mRect:=Item.FItemLegRect;
    ls:=txtRect.Bottom-txtRect.Top;
    if Flags and DT_RIGHT = DT_RIGHT then
    begin
      Dec(txtRect.Right,ls + FSpacing div 2);
      mRect.Left:=mRect.Right-ls;
      inflateRect(mRect,-ls div 6,-ls div 6);
    end
    else
    begin
      Inc(txtRect.Left,ls + FSpacing div 2);
      mRect.Right:=mRect.Left+ls;
      inflateRect(mRect,-ls div 6,-ls div 6);
    end;

    if clIdx>=0 then
    begin
      if clIdx>=Length(AutoColorPalette) then
        clIdx:=0;
      tmpClr:=AutoColorPalette[clIdx];
      Inc(clIdx);
    end
    else
      tmpClr:=Item.Color;
    if not Enabled then
      tmpClr:=GrayScaleColor(tmpClr);
    TargetCanvas.Brush.Color:=tmpClr;
    tmpClr:=DarkColor(tmpClr,23);
    TargetCanvas.Pen.Color:=tmpClr;
    TargetCanvas.Pen.Style:=psSolid;
    TargetCanvas.Pen.Width:=FFrameWidth;
    TargetCanvas.Brush.Style:=bsSolid;
    if FRounded then
      {$IF CompilerVersion >= 20}
      TargetCanvas.RoundRect(mRect,rcs div 2,rcs div 2)
      {$ELSE}
      TargetCanvas.RoundRect(mRect.Left,mRect.Top,mRect.Right,mRect.Bottom,rcs div 2,rcs div 2)
      {$IFEND}
    else
      TargetCanvas.Rectangle(mRect);

    TargetCanvas.Brush.Style:=bsClear;
    TargetCanvas.Font.Color:=tmpClr;
    if (Item=FHoverItem) and (FIsLegendHovered) then
    begin
      TargetCanvas.Font.Style:=Font.Style+[fsUnderline];
      FDrawTextFunc(TargetCanvas.Handle, GetItemTextFormatted(Item), txtRect, Flags, tmpClr, GlowSize);
      TargetCanvas.Font.Style:=Font.Style;
    end
    else
      FDrawTextFunc(TargetCanvas.Handle, GetItemTextFormatted(Item), txtRect, Flags, tmpClr, GlowSize);
  end;
end;

procedure TCustomDecoBar.Paint;
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

    tmpCanvas.Font := Font;
    DrawItems(tmpCanvas,Items);

    if FShowLegend then
      DrawLegend(tmpCanvas,FExpandedItems);

    // draw hover or selected item text at bottom of the last expanded bar
    if Assigned(FHoverItem) and (not FIsLegendHovered) and (not FHoverItem.Expanded) and (FHoverItem.Collection=FExpandedItems) then
      DrawItemText(tmpCanvas,FHoverItem)
    else
    if FShowingFocusRect and FActive and Assigned(FSelectedItem) and (not FSelectedItem.Expanded) and (FSelectedItem.Collection=FExpandedItems) then
      DrawItemText(tmpCanvas,FSelectedItem);

    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,tmpCanvas);

    Canvas.CopyRect(cRect, tmpCanvas, cRect);

  finally
    Buffer.Free;
  end;

  if FActive and FShowingFocusRect and Assigned(FSelectedItem) then
  begin
    aRect:=FSelectedItem.FItemRect;
    inflateRect(aRect,0,2);
    windows.DrawFocusRect(Canvas.Handle,aRect);
  end;
end;

procedure TCustomDecoBar.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
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

    TargetCanvas.Font := Font;
    DrawItems(TargetCanvas,Items);

    if FShowLegend then
      DrawLegend(TargetCanvas,FExpandedItems);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,TargetCanvas);

  finally
    _EMFExporting := False;
    FRebuildNeeded := True;
    Rebuild(ClientRect);
  end;
end;

end.
