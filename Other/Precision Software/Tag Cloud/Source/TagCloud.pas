{------------------------------------------------------------------------------
  TagCloud.pas

  VCL implementation of the tag cloud element (TTagCloud, TCustomTagCloudStyler, ...)

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Base unit of "TagCloud for VCL" package, that implements
              general types, classes and functions.

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. The complete source code remains property of the author
  and may not be distributed, published, given or sold in any form as such.
  No parts of the source code can be included in any other component
  or application without written authorization of the author.

  Copyright (c) 2008-2014  Precision software & consulting
  All rights reserved

  DoDrawNormalText, DoDrawNormalTextEx and FillGlassRect methods
  are based on the original Embarcadero Delphi source codes.
------------------------------------------------------------------------------}

{ Change log:

  Version 2.1 (2014-09-29)
  - added: Delphi XE6/XE7 support
  - fixed: A bug in TCustomTagCloud.Rebuild method (line 4082), that caused some memory leaks  

  Version 2.0 (2013-11-16)
  - added: Delphi XE4/XE5 support
  - added: OnTagDblClick event
  - added: HoverColSpace - a new property of TCustomTagCloud, that allows you to handle the space between items
           as a part of the hovered item, so you can draw additional icon or anything else inside custom drawing
           event handlers and react on clicks when the mouse cursor is inside this area.
  - added: New sample project, named DeleteTagsGUI, that demonstrates the HoverColSpace usage

  Version 1.9.5 (2013-01-01)
  - added: Delphi XE3 support
  - and other minor improvements

  Version 1.9 (13.7.2012)
  - added: VerticalAlignment property
  - added: AutoScaleFont property, that scales the items (their display font size) to fit as much available space as possible
  - improved: Missing default values have been assigned to properties

  Version 1.8 (15.3.2012)
  - added: RowCount and VisibleRowCount properties
  - added: GetItemRowIndex function

  Version 1.7 (17.1.2012)
  - added: Selected property for TTagCloudItem - by this way you can handle multi-select behavior of tag cloud (see StylingDemo)
  - fixed: Bug in component installer (Setup Wizard) - Delphi 5 IDE was not recognized on developer's computer
  - changed: Visible property of TTagCloudItem is now declared as published

  Version 1.6 (26.9.2011)
  - added: Compatibility with Delphi XE2
  - added: Automatic installer (Setup Wizard)
  - added: GetItemPageIndex function
  - fixed: Corrected conditional defines for using GetTextExtentPoint32 function

  Version 1.5 (16.3.2011)
  - added: Custom scale definition (you can setup your own value ranges along with their font sizes and colors)
  - added: Frames and backgrounds support for tag items, including colors, styles, margins, etc.
  - added: TTagCloudPrmStyler as a new component for managing the styles of your tag clouds (see also TCustomTagCloudStyler)
  - added: OnBeforePaint and OnAfterPaint events, to support custom painting onto the whole TagCloud component surface
  - added: OnTagPositioning event, that allows you to customize the position of tag items (ie. insert breaks, spaces, etc.)
  - added: FixedColCount property, that forces an automatic calculation of FixedColWidth, so the tag cloud items are arranged into the specified number of columns
  - added: Direction property, that allows you to compose the tags horizontally (by default) or vertically (as an Index, rather than Cloud). It is applicable only for fixed-width tag clouds.
  - added: SelectedItem property, that identifies one of the items as selected (see also SelectedColor and SelectedFrame)
  - added: Colors.AddColor method, to allow more comfortable adding of color levels at run-time
  - added: ShrinkDiacritic property, that can produce even more shrunken tag clouds, when you are not using captions with diacritical characters
  - added: GetItemMetrics method, that can be used to retrieve the calculated rectangles, text height, and other fields of an item used for drawing
  - added: New demonstration projects, that presents the newly implemented components and properties in action.
  - improved: Aligning the tags on the baseline
  - improved: Redesign of TTagCloud component declaration, to support more comfortable inheritance (TTagCloud is inherited from TCustomTagCloud now)
  - improved: MinValue and MaxValue properties are available as public now
  - improved: Better centering the hovered tag items inside their drawing rectangles
  - improved: The speed of rebuild process for a large amount of items
  - improved: More precise calculation of auto-shrinked rows
  - improved: Now all temporary fields of TTagCloudItem are always calculated (even if OnCustomDrawItem is not assigned), so also the GetItemAt method
              and detection of hovered item are now more precise
  - improved: DisplayName for design-time editing of color level items
  - improved: Autosizing of the component (when large items together with small items are located on the last row)
  - improved: Displaying a maximum count of available tags even if the component has the smallest possible size
  - improved: Suppressed calling of Rebuild methods during the load process of the component
  - fixed: A validation flag for items is now handled correctly, so the previously invisibile items are rebuilded after switching the relevant
           properties (filter, logscale, etc.) on and off.
  - fixed: Sorting with diacritics
  - and other minor improvements and fixes

  Version 1.1 (3.11.2010)
  - added: Compatibility with Delphi XE
  - added: Multi-page and AutoSize support (see the PageCount, PageIndex, DisplayCount, FirstDisplayIndex, LastDisplayIndex and AutoSize properties)
  - added: Automatic sorting (see the Sorted property)
  - added: Custom sorting (see the OnCompareItems event)
  - added: Fast binary searching for items (see the Find(Caption) method)
  - added: Searching the items by Data pointer (see the Find(Data) method)
  - added: Quick access methods, such as IncreaseValue(Caption, Increment, ...), AddItem(Caption, Value, ...)
  - added: Automatic and/or user defined filtering (see the Filter and VisibleCount properties and an OnFilter event handler)
  - added: A variable row heights (see the AutoShrinkRows property)
  - added: Fixed column width support (see the FixedColWidth property)
  - added: OnCustomDrawItem event handler, that supports both standard and user defined drawing of items
  - improved: Cache mechanism (tag cloud rebuilding) has been significantly improved, so the component can be used for working with large amounts of data
  - improved: Sorting speed for large number of items (it is implemented as a non-standard workaround, so if you want to use, you have to enable it
              by setting the UseSpeedSort public property to True at run-time)
  - fixed: Truncated item labels, that were located near by the edges of the component (when HoverEnlarge was True and ColSpacing was too small)
  - added: An advanced demonstration project, that shows the newly implemented properties and methods in action
           (also includes an example of "Creating the tag cloud items from text", "Custom drawing example" and "Opacity simulation by color levels definition")

  Version 1.0 (20.8.2010)
  - The first release of TTagCloud component
}

{ Base unit of "TagCloud for VCL" package, that implements general types, classes and functions (TCustomTagCloud, TCustomTagCloudStyler, TTagCloud, etc). }
unit TagCloud;

interface

uses
  Windows, Classes, Controls, Graphics, Messages;

type
  TCustomTagCloud = class;
  TTagCloudItems = class;

  {$IF CompilerVersion<18}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$IFEND}

  { Tag cloud items can be placed in horizontal or vertical direction. Vertical direction is allowed only for fixed-width tag clouds
    (where FixedColWidth or FixedColCount is greater than 0). }
  TTagCloudDirection = (tcdHorizontal, tcdVertical);
  { When handling OnTagPositioning event, you can specify one of the predefined break types, to create a space between items (tibSpacer),
    to create a page break (tibPageBreak), or to place the item on the beginning of next row or column (tibColRowBreak).
    Breaking the row is designer for horizontally placed tag items, breaking the column is designed for vertically placed items (see TTagCloudDirection). }
  TTagItemBreakKind = (tibNone,tibSpacer,tibPageBreak,tibColRowBreak);
  { A record for holding the font metrics information about each available item size. }
  TTagFontMetrics = record
    Height,
    Ascent,
    Descent,
    InternalLeading:Longint;
  end;

  { A pointer to TTagCloudRecord. }
  PTagCloudRecord = ^TTagCloudRecord;
  { Data record for low-level operations with the tag cloud items.
    It is used for loading items from external resources (CSV files, TStrings object),
    for sorting, and another similar handling. }
  TTagCloudRecord = record
    // Tag caption. This string is displayed to the user as a tag.
    Caption:string;
    // Tag hint. A help hint that is displayed when hovering the mouse over a tag.
    Hint:string;
    { Tag value. If you want to use a floating point values, you have to multiply the
      values before you store them, and divide the value after you read it from a tag. }
    Value:Int64;
    { Tag of the tag. An additional integer value for any use. }
    Tag:Longint;
    { Custom data pointer. It can reference to any defined pointer or object, according to your requirements. }
    Data:Pointer;
  end;

  { An array of TTagCloudRecord. Used for low-level operations with the tag cloud items. }
  TTagCloudRecords = array of PTagCloudRecord;

  { Record for retrieving the item's metrics}
  TTagCloudItemMetrics = record
    { Drawing rectangle of the item. If AutoShrinkRows is False, this rectangle is equal to the current row height of the tag cloud (ie. maximum text height).
      If AutoShrinkRows is True, this rectangle covers the item's text without the diacritic marks. In other words, this rectangle is used for drawing the item's caption
      by the DrawText API function. }
    DrawRect: TRect;
    // The same as DrawRect, but for hovered (enlarged) items
    DrawRectHovered: TRect;
    // A real rectangle covered with an item caption, excluding the diacritic marks and underline overlaps
    TextRect: TRect;
    // A real rectangle covered with the hovered item caption, excluding the diacritic marks and underline overlaps
    TextRectHovered: TRect;
    // A real rectangle covered with the whole item caption, including the diacritic marks and underline overlaps
    FrameRect: TRect;
    // A real rectangle covered with the whole caption of hovered item, including diacritic marks and underline overlaps
    FrameRectHovered: TRect;
    // Current font size of an item
    FontSize: Integer;
    // Indicates the page index of an item in a multi-page tag cloud. For auto-sized tag clouds, it returns -1.
    PageIndex: Integer;
  end;

  { Tag item representation in a tag cloud. }
  TTagCloudItem = class(TCollectionItem)
  private
    FTCCollection:TTagCloudItems;
    FCaption: string;
    FHint: string;
    FValue: Int64;
    FTag: Longint;
    FData: Pointer;
    FVisible: Boolean;
    FSelected: Boolean;

    { Following temporary fields are used for rebuilding the tag cloud, and then used for drawing the item }

    // Indicates that the temporary fields of this Item need to be validated
    FNeedsValidate: Boolean;
    // Calculated tag item size (font size)
    FSize: Integer;
    // Indicates the width of an item text (cx) and an absolute value of the item's font height (cy)
    FTextSize: TSize;
    // Indicates the width and height of an enlarged hovered item text
    FEnSize: TSize;
    // Font color
    FColor: TColor;
    // Background color
    FBackColor: TColor;
    // Frame color
    FFrameColor: TColor;
    // Drawing rectangle of the item (used for DrawText API method)
    FRect: TRect;
    // Drawing rectangle of hovered (enlarged) item (used for DrawText API method)
    FRectEn: TRect;
    // Drawing rectangle of the item, excluding the base line compensation factor
    FRectTxt: TRect;
    // Drawing rectangle of hovered (enlarged) item, excluding the base line compensation factor
    FRectTxtEn: TRect;
    // A real rectangle covered with the whole caption, including diacritic marks and underline overlaps, excluding the base line compensation factor
    FRectFrame: TRect;
    // A real rectangle covered with the whole caption of hovered item, including diacritic marks and underline overlaps, excluding the base line compensation factor
    FRectFrameEn: TRect;
    // Indicates the page index of an item in a multi-page tag cloud
    FPageIndex: Integer;
    // Indicates the row index of an item within the whole tag cloud
    FRowIndex: Integer;
  protected
    // Display name for the design-time collection editor
    function GetDisplayName: string; override;
    // Sets the item caption
    procedure SetCaption(const Value: string); virtual;
    // Sets the item hint
    procedure SetHint(const Value: string); virtual;
    // Sets the item's selected state
    procedure SetSelected(Value: Boolean); virtual;
    // Sets the item value
    procedure SetValue(Value: Int64); virtual;
    // Sets the item's visibility
    procedure SetVisible(Value: Boolean); virtual;
  public
    // Tag item constructor. You should use TTagCloudItems.Add method (called from TCustomTagCloud.Items) to create a new item.
    constructor Create(Collection: TCollection); override;
    {$IF CompilerVersion<15}
    // Destructor of tag item
    destructor Destroy; override;
    {$IFEND}
    // Tag item assignment procedure.
    procedure Assign(Source: TPersistent); override;
    { Special tag item assignment from PTagCloudRecord structure. You can request the refreshing of component by passing
      True to the RequestChange parameter.}
    procedure AssignRecord(Source: PTagCloudRecord; RequestChange:Boolean=False);
    // Assigning the item properties (fields) to PTagCloudRecord structure.
    procedure AssignToRecord(Dest: PTagCloudRecord);
    { Custom data for the tag item. It can reference to any defined pointer or object, according to your requirements. }
    property Data: Pointer read FData write FData;
  published
    // Tag caption. This string is displayed to the user as a tag.
    property Caption: string read FCaption write SetCaption;
    // Tag hint. A help hint that is displayed when hovering the mouse over a tag.
    property Hint: string read FHint write SetHint;
    { Tag of the tag. An additional integer value for any use. }
    property Tag: Longint read FTag write FTag default 0;
    { Tag value. If you want to use a floating point values, you have to multiply the
      values before you store them, and divide the value after you read it from a tag. }
    property Value: Int64 read FValue write SetValue default 0;
    { Indicates visibility of an item, when the TagCloud is filtered. It can be also set manually. }
    property Visible: Boolean read FVisible write SetVisible default True;
    { Gets or sets the item's selected state. Supports multi-select in TagCloud, works independently on TCustomTagCloud.SelectedItem. }
    property Selected: Boolean read FSelected write SetSelected default False;
  end;

  { Collection of the tag cloud items. It is accessible through TCustomTagCloud.Items property. }
  TTagCloudItems = class(TCollection)
  private
    FTagCloud: TCustomTagCloud;
    // Direct reference to the private FItems field of ancestor (TCollection). Used for fast sorting of TCustomTagCloud.Items collection (see UseSpeedSort property).
    FItemsRef: TList;
    procedure QuickSort(L, R: Integer);
    procedure CustomQuickSort(L, R: Integer);
    function InternalFind(const aCaption: string; var Index: integer):Boolean;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    {$IF CompilerVersion>=15}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Deleting(Item: TCollectionItem);
    {$IFEND}
    function GetItem(Index: Integer): TTagCloudItem; virtual;
    procedure SetItem(Index: Integer; Value: TTagCloudItem); virtual;
  public
    // The collection constructor. It is called automatically by the owner (TCustomTagCloud)
    constructor Create(TagCloud: TCustomTagCloud);
    // Creates and adds a new tag item
    function Add: TTagCloudItem;
    { Allows you to add a whole new tag item. If items are sorted, this new item is correctly positioned.
      This method updates a duplicit item (by caption) instead of adding new. }
    function AddItem(const aCaption: string; aValue: Int64 = 0; const aHint: string = ''; aTag: LongInt = 0; aData: Pointer = nil): TTagCloudItem;
    // Creates and inserts a new tag item to the specified position in the collection
    function Insert(Index:Integer): TTagCloudItem;
    // Deletes a tag item specified by the index
    procedure Delete(Index: Integer);
    // Moves a tag item to another position in the collection. Moved item is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    { Increases value of a tag item by the Increment. You can decrease the tag item value by passing a negative increment.
      If the item is not found by its caption, a new item is created with the Value set to Increment (AddIfNotExists argument must be True). }
    function IncreaseValue(const aCaption: string; Increment:Int64=1; AddIfNotExists: Boolean = True): TTagCloudItem;
    // Sorts the tag items collection alpabetically
    procedure Sort;
    // Finds the tag item by its caption
    function Find(const aCaption: string):TTagCloudItem; overload;
    // Finds the tag item by its data pointer
    function Find(aData: pointer):TTagCloudItem; overload;
    // Through this property you can access the individual tag items
    property Items[Index: Integer]: TTagCloudItem read GetItem write SetItem; default;
    // Reference to TCustomTagCloud component that owns this collection
    property TagCloud: TCustomTagCloud read FTagCloud;
    // UpdateCount property
    property UpdateCount;
  end;

  { Parametrized notify event handler. It is used internally for handling the changes of the frame style properties in TCustomTagCloud
    and its descendants. See also TTagItemFrame. }
  TTagCloudPrmEvent = procedure(Sender:TObject; Param: Integer) of object;

  // A persistent object for holding the frame style properties of tag items
  TTagItemFrame = class(TPersistent)
  private
    FBackColor: TColor;
    FFrameSize: Integer;
    FFrameColor: TColor;
    FFrameMargin: Integer;
    FFrameStyle: TPenStyle;
    FRoundedSize: Integer;
    FOnChanged: TTagCloudPrmEvent;
    procedure SetBackColor(Value: TColor);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameMargin(Value: Integer);
    procedure SetFrameStyle(Value: TPenStyle);
    procedure SetFrameSize(Value: Integer);
    procedure SetRoundedSize(Value: Integer);
  public
    // The class constructor. It is called automatically by the owner (TCustomTagCloud, TTagCustomScaleItem, etc.)
    constructor Create;
    // Assignment procedure for TTagItemFrame class
    procedure Assign(Source: TPersistent); override;
    // Event handler for notifying an owner component about the changes
    property OnChange: TTagCloudPrmEvent read FOnChanged write FOnChanged;
  published
    // Defines the background color for tag items
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
    // Defines the frame color for tag items
    property FrameColor: TColor read FFrameColor write SetFrameColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Defines the distance of the frame from the text
    property FrameMargin: Integer read FFrameMargin write SetFrameMargin default 0;
    // Defines the frame size for tag items. If value is greater than 0, the frame will be drawn.
    property FrameSize: Integer read FFrameSize write SetFrameSize default 0;
    // Defines the frame style (pen style) for tag items
    property FrameStyle: TPenStyle read FFrameStyle write SetFrameStyle default psSolid;
    // Rounded frame will be drawn, if the value is greater than 0
    property RoundedSize: Integer read FRoundedSize write SetRoundedSize default 0;
  end;

  { Tag color level item. You can do the scaling of tag cloud items by the font size, or by the colors.
    Tag color items in TTagCloudColors collection define the tag cloud color levels. See also TTagCloudCustomScale }
  TTagColorItem = class(TCollectionItem)
  private
    FColor: TColor;
    FBackColor: TColor;
    FFrameColor: TColor;
  protected
    // Display name for the design-time collection editor
    function GetDisplayName: string; override;
    // Sets the color
    procedure SetColor(Value: TColor); virtual;
    // Sets the background color
    procedure SetBackColor(Value: TColor); virtual;
    // Sets the frame color
    procedure SetFrameColor(Value: TColor); virtual;
  public
    // Tag color level constructor. You should use TTagCloudColors.Add method (called from TCustomTagCloud.Colors) to create a new color level.
    constructor Create(Collection: TCollection); override;
    // Tag color level assignment procedure.
    procedure Assign(Source: TPersistent); override;
  published
    // Color of the tag cloud color level item.
    property Color: TColor read FColor write SetColor;
    // Background color of the tag cloud color level item.
    property BackColor: TColor read FBackColor write SetBackColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Frame color of the tag cloud color level item.
    property FrameColor: TColor read FFrameColor write SetFrameColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
  end;

  { Collection of the tag cloud color levels. It is accessible through TCustomTagCloud.Colors property. }
  TTagCloudColors = class(TCollection)
  private
    FTagCloud: TCustomTagCloud;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    {$IF CompilerVersion>=15}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$IFEND}
    function GetItem(Index: Integer): TTagColorItem; virtual;
    procedure SetItem(Index: Integer; Value: TTagColorItem); virtual;
  public
    // The collection constructor. It is called automatically by the owner (TCustomTagCloud)
    constructor Create(TagCloud: TCustomTagCloud);
    // Creates and adds a new color level
    function Add: TTagColorItem;
    // Creates and adds a new color level with passed color value
    function AddColor(Clr:TColor; BackColor: TColor = clNone; FrameColor: TColor = {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND}): TTagColorItem;
    // Creates and inserts a new color level to the specified position in the collection
    function Insert(Index:Integer): TTagColorItem;
    // Deletes a color level specified by the index
    procedure Delete(Index: Integer);
    // Moves a color level to another position in the collection. Moved item is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    // Through this property you can access the individual color levels
    property Items[Index: Integer]: TTagColorItem read GetItem write SetItem; default;
    // Reference to TCustomTagCloud component that owns this collection
    property TagCloud: TCustomTagCloud read FTagCloud;
  end;

  { Tag cloud custom scale definition item. See TTagCloudCustomScale or TCustomTagCloud.CustomScale for more information. }
  TTagCustomScaleItem = class(TCollectionItem)
  private
    FValueFrom: Int64;
    FValueTo: Int64;
    FFontSize: Integer;
    FColor: TColor;
    FBackColor: TColor;
    FFrameColor: TColor;
  protected
    // Display name for the design-time collection editor
    function GetDisplayName: string; override;
    // Sets the color value for tag items in the range
    procedure SetColor(Value: TColor); virtual;
    // Sets the font size for tag items in the range
    procedure SetFontSize(Value: Integer); virtual;
    // Sets the starting value of the range
    procedure SetValueFrom(Value: Int64); virtual;
    // Sets the ending value of the range (excluding this one)
    procedure SetValueTo(Value: Int64); virtual;
    // Sets the background color value for tag items in the range
    procedure SetBackColor(Value: TColor); virtual;
    // Sets the frame color value for tag items in the range
    procedure SetFrameColor(Value: TColor); virtual;
  public
    // Custom scale range constructor. You should use TTagCloudCustomScale.Add method (called from TCustomTagCloud.CustomScale) to create a new custom scale range.
    constructor Create(Collection: TCollection); override;
    // Custom scale assignment procedure
    procedure Assign(Source: TPersistent); override;
  published
    // Background color for the range of tag items
    property BackColor: TColor read FBackColor write SetBackColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Color for the range of tag items
    property Color: TColor read FColor write SetColor;
    // Font size for the range of tag items
    property FontSize: Integer read FFontSize write SetFontSize;
    // Frame color for the range of tag items
    property FrameColor: TColor read FFrameColor write SetFrameColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Starting value of the range
    property ValueFrom: Int64 read FValueFrom write SetValueFrom default 0;
    // Ending value of the range (excluding this one)
    property ValueTo: Int64 read FValueTo write SetValueTo default 0;
  end;

  // Temporary enumerator for TTagCloudCustomScale collection sorting state
  TCollectionSortState = (cssNone, cssNeedsSort, cssSorting);

  { Tag cloud custom scale definition. You can do the scaling of tag cloud items by the font size, or by the colors.
    Normaly, you can use Font.Size, MaxFontSize and LogScale property to differentiate the size of tag cloud items.
    If you want, you can also use the Colors collection and LogScale property to differentiate the colors of tag cloud items.
    With the help of CustomScale property you can override the default linear (or logarithmic) scaling by defining your own ranges of values
    with associated font sizes and colors. You can even mix the standard and custom scaling, for example you can define only one custom range of values,
    that will be displayed differently among the other tag items. }
  TTagCloudCustomScale = class(TCollection)
  private
    FTagCloud: TCustomTagCloud;
    FSortState: TCollectionSortState;
    procedure QuickSort(L, R: Integer);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    procedure Sort; virtual;
    {$IF CompilerVersion>=15}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$IFEND}
    function GetItem(Index: Integer): TTagCustomScaleItem; virtual;
    procedure SetItem(Index: Integer; Value: TTagCustomScaleItem); virtual;
  public
    // The collection constructor. It is called automatically by the owner (TCustomTagCloud)
    constructor Create(TagCloud: TCustomTagCloud);
    // Creates and adds a new range of values for custom scale definition
    function Add: TTagCustomScaleItem;
    // Creates and adds a new range with predefined properties
    function AddScale(aValueFrom, aValueTo:Int64; aFontSize:Integer; aColor:TColor;
        BackColor: TColor = clNone; FrameColor: TColor = {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND}): TTagCustomScaleItem;
    // Deletes a range specified by the index
    procedure Delete(Index: Integer);
    // Finds a range to which the Value belongs (if any)
    function Find(Value: Int64; var fItem:TTagCustomScaleItem):Boolean;
    // Through this property you can access the individual custom scale definition items (ranges of values)
    property Items[Index: Integer]: TTagCustomScaleItem read GetItem write SetItem; default;
    // Reference to TCustomTagCloud component that owns this collection
    property TagCloud: TCustomTagCloud read FTagCloud;
  end;

  { A special class that defines the options for loading tag cloud items from an external resources. }
  TTagCloudLoadingOptions = class(TPersistent)
  private
    FValuesGreaterThan:Int64;
    FAlphaSort:Boolean;
    FLowerCase:Boolean;
    FMaxItemsCount:Integer;
    FSeparator:Char;
    FSkipFirstRow:Boolean;
    FColCaption:Integer;
    FColValue:Integer;
    FColHint:Integer;
    FColTag:Integer;
    FColData:Integer;
  public
    // The class constructor. It is called automatically by the owner (TCustomTagCloud)
    constructor Create;
    // Assignment procedure for TTagCloudLoadingOptions class
    procedure Assign(Source: TPersistent); override;
  published
    // When loading, include only these tag items, that have the value greater than this property.
    property ValuesGreaterThan:Int64 read FValuesGreaterThan write FValuesGreaterThan default 0;
    { When this property is True, the tag items will be sorted alpabetically after loading from an external resource.
      Using this option increases the performance, because sorting the loaded records is faster then sorting the resulting tag item objects. }
    property AlphaSort:Boolean read FAlphaSort write FAlphaSort default True;
    // When this property is True, the loaded tag captions will be converted to the lower case.
    property LowerCase:Boolean read FLowerCase write FLowerCase default False;
    { Specifies the maximum tag items, that will be included into TCustomTagCloud component, when loading from an external resource.
      If this property is zero, all items will be loaded. If this property is greater then zero, all the items are loaded first,
      then they are sorted by its values, and then only the specified maximum count of items is included into the component's items collection.
      This process also takes into account the ValuesGreaterThan property. }
    property MaxItemsCount:Integer read FMaxItemsCount write FMaxItemsCount default 64;
    // Defines the columns (fields) separator for loading values from a CSV based external resource.
    property Separator:Char read FSeparator write FSeparator default ';';
    // The first row of an external resource will be ommitted when loading tag items
    property SkipFirstRow:Boolean read FSkipFirstRow write FSkipFirstRow default False;
    // Defines the index of a column in an external resource (TStrings object, CSV file, etc.), that identifies the tag item caption.
    property ColCaption:Integer read FColCaption write FColCaption default 0;
    // Defines the index of a column in an external resource, that identifies the tag item value.
    property ColValue:Integer read FColValue write FColValue default 1;
    // Defines the index of a column in an external resource, that identifies the tag item hint.
    property ColHint:Integer read FColHint write FColHint default -1;
    // Defines the index of a column in an external resource, that identifies the tag item additional integer value (called "tag").
    property ColTag:Integer read FColTag write FColTag default -1;
    { Defines the index of a column in an external resource, that identifies the tag item data pointer (stored as an integer, or hexadecimal string).
      This only makes sense if it is necessary to transfer items between two instances of TCustomTagCloud in one application context via TStrings or CSV file.
      Normally it is better to use a tranfer via TTagCloudRecords array, or via the TTagCloudItems collections. }
    property ColData:Integer read FColData write FColData default -1;
  end;

  // Common TCustomTagCloud event handler, used for the tag cloud items OnClick or OnHover events.
  TTagCloudEvent = procedure(Sender:TObject; Item:TTagCloudItem) of object;
  // Event handler for custom assignment of the tag cloud item color
  TTagColorEvent = procedure(Sender:TObject; Item:TTagCloudItem; var Color:TColor) of object;
  // Event handler for processing the tag items when Filter is set. If no handler is assigned, internal event for filtering by caption is used.
  TTagFilterEvent = procedure(Sender:TObject; Item:TTagCloudItem; var Allowed:boolean) of object;
  // Event handler for custom assignment of the tag cloud item hint
  TTagHintEvent = procedure(Sender:TObject; Item:TTagCloudItem; var HintText:string) of object;
  // Event handler for custom sorting. If no handler is assigned, internal event for sorting by caption is used.
  TTagCompareEvent = function(Sender:TObject; Item1, Item2:TTagCloudItem):integer of object;
  { Event handler for customizing the position of tag items. It is called everytime before the item's position is rebuilded (calculated),
    so you can adjust the X or Y values or setup one of the predefined break kinds. Keep in mind, that changing the position of an item
    affects the position of all subsequent items. CWidth and CHeight parameters represent the width and height of the item caption in pixels. }
  TTagPositioningEvent = procedure(Sender: TObject; Item: TTagCloudItem; CWidth, CHeight: Integer; const PageRect: TRect; var x, y: Integer; var breakKind: TTagItemBreakKind) of object;

  {$IF CompilerVersion>=20}
  // Tag items drawing procedure reference.
  TTagFNDrawText = procedure(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal) of object;
  {$ELSE}
  TTagFNDrawText = procedure(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal) of object;
  {$IFEND}
  { Event handler that allows you to perform a custom drawing of the tag items. All the default text attributes are already pre-filled in a TargetCanvas properties and in the TextFlags parameter.
    DefaultDraw parameter is False by default, but if you change it to True, the tag will be painted as usual. It is suitable for painting a frame, icon or other background
    and then let the component to draw the text of the item. Transparent property must be True if you want to use both the DefaultDrawing and a painting of your own background.
    Also you can modify the ItemRect dimensions (if needed) to process the default drawing in a different area. }
  TTagCustomDraw = procedure(Sender:TObject; TargetCanvas:TCanvas; Item:TTagCloudItem; TextRect: TRect; var ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw:Boolean) of object;
  { An advanced version of TTagCustomDraw event, that allows you to use (or modify) also the frame rectangle of the customized item.
    It is provided mainly to support a full customization by the tag cloud stylers.
    If OnAdvancedCustomDrawItem event is assigned for the tag cloud component, OnCustomDrawItem event is ignored. }
  TTagAdvancedCustomDraw = procedure(Sender:TObject; TargetCanvas:TCanvas; Item:TTagCloudItem; TextRect: TRect; var FrameRect: TRect; var ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw:Boolean) of object;
  { Event handler that allows you to perform a custom drawing on the tag cloud component surface. It is a prototype for OnBeforePaint and OnAfterPaint events of TCustomTagCloud component.
    Rect is a clipping rectangle of TargetCanvas. FullRepaint parameter indicates that the Rect is equal to the component's ClientRect - thus the whole component rectangle should be painted.
    If FullRepaint is False, the painting is performed only for one tag item or the portion of the tag cloud component.
    Remember, that if you assign the OnBeforePaint event, you should fill the component background properly, with aero glass paint or other parent control states in mind. }
  TTagCloudCustomPaint = procedure(Sender:TObject; TargetCanvas:TCanvas; Rect: TRect; FullRepaint: Boolean) of object;
  // Various state indicators for TCustomTagCloud background processes
  TTagCloudState = (tcsRebuilding, tcsFiltering, tcsSorting, tcsNeedsValidate, tcsNeedsSort, tcsLoadingData, tcsNeedsTextMetrics);
  // Indicates the current state of TCustomTagCloud background processes
  TTagCloudStates = set of TTagCloudState;

  { A base class for all tag cloud stylers. See also TTagCloudStyler, TTagCloudPrmStyler, and other inherited stylers. }
  TCustomTagCloudStyler = class(TComponent)
  private
    FClients: TList;
    FIgnoreDimensions: Boolean;
    FOnStyleApplied: TNotifyEvent;
  protected
    // Method for styler defined painting after all items are drawn
    procedure DoAfterPaint(Sender:TObject; TargetCanvas:TCanvas; Rect: TRect; FullRepaint: Boolean); virtual;
    // Method for styler defined painting of tag cloud background. Keep in mind, that this method is mutually exclusive with user defined OnBeforePaint event.
    procedure DoBeforePaint(Sender:TObject; TargetCanvas:TCanvas; Rect: TRect; FullRepaint: Boolean); virtual;
    { Method for styler defined drawing of individual items. All the default text attributes are already pre-filled in a TargetCanvas properties
      and in the TextFlags parameter. DefaultDraw parameter is False by default, but if you change it to True, the tag will be painted as usual.
      You can modify the ItemRect dimensions (if needed) to process the default drawing in a different area. }
    procedure DoDrawItem(Sender:TObject; TargetCanvas:TCanvas; Item:TTagCloudItem; TextRect: TRect;
        var FrameRect: TRect; var ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw:Boolean); virtual;
    // If return value is True, instructs the TCustomTagCloud component, that the styler uses DoAfterPaint method
    function GetHasAfterPaint:Boolean; virtual;
    // If return value is True, instructs the TCustomTagCloud component, that the styler uses DoBeforePaint method
    function GetHasBeforePaint:Boolean; virtual;
    // If return value is True, instructs the TCustomTagCloud component, that the styler uses DoDrawItem method
    function GetHasDrawItem:Boolean; virtual;
    // This method can be used to apply predefined visual style properties, that your descendant styler implements, to the desired TCustomTagCloud component
    procedure Apply(Sender: TCustomTagCloud); virtual;
    // Instructs the TCustomTagCloud component, that the styler uses DoAfterPaint method
    property HasAfterPaint:Boolean read GetHasAfterPaint;
    // Instructs the TCustomTagCloud component, that the styler uses DoBeforePaint method
    property HasBeforePaint:Boolean read GetHasBeforePaint;
    // Instructs the TCustomTagCloud component, that the styler uses DoDrawItem method
    property HasDrawItem:Boolean read GetHasDrawItem;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    { Call ApplyStyle method to apply predefined visual style properties to the desired TCustomTagCloud component,
      or to all attached tag cloud components (if parameter value is nil). This method calls Apply method for each attached tag cloud. }
    procedure ApplyStyle(Sender: TCustomTagCloud = nil); virtual;
    // Implement this method in inherited stylers to pre-fill the styler with the visual properties of selected TCustomTagCloud component.
    procedure LoadFromTagCloud(Sender: TCustomTagCloud); virtual;
  published
    { If True, the dimensional properties are ignored during applying the style to tag cloud components.
      Dimensional properties are: Font.Size, MaxFontSize, Padding, ColSpacing, RowSpacing, LogScale, Alignment, Direction, etc. }
    property IgnoreDimensions: Boolean read FIgnoreDimensions write FIgnoreDimensions default False;
    { Event handler that is triggered everytime the style is applied to the tag cloud component (Sender). }
    property OnStyleApplied: TNotifyEvent read FOnStyleApplied write FOnStyleApplied;
    // A common Tag property
    property Tag;
  end;

  { A base class for all tag cloud visual component implementations. }
  TCustomTagCloud = class(TGraphicControl)
  private
    aDesignTextInfo:string;
    FAlignment: TAlignment;
    FAutoShrinkRows: Boolean;
    {$IF CompilerVersion<15}
    FAutoSize: Boolean;
    {$IFEND}
    FColors: TTagCloudColors;
    FColSpacing: Integer;
    FCustomScale: TTagCloudCustomScale;
    FDisplayCount: Integer;
    FFilter: string;
    FFirstDisplayIndex: Integer;
    FLastDisplayIndex: Integer;
    FFixedColCount: Integer;
    FFixedColWidth: Integer;
    FFixedColFullFrame: Boolean;
    FGlowSize: Integer;
    FState: TTagCloudStates;
    FHoverItem: TTagCloudItem;
    FHoverColor: TColor;
    FHoverCursor: TCursor;
    FHoverStyle: TFontStyles;
    FHoverEnlarge: Boolean;
    FHoverColSpace: Boolean;
    FItems: TTagCloudItems;
    FOnItemsChanged: TNotifyEvent;
    FLoadingOptions: TTagCloudLoadingOptions;
    FLogScale: Boolean;
    FMaxFontSize: Integer;
    {$IF CompilerVersion>=18}
    FPadding: TPadding;
    {$IFEND}
    FPageCount: Integer;
    FPageIndex: Integer;
    FRowCount: Integer;
    FVisibleRowCount: Integer;
    FRowSpacing: Integer;
    FSorted: Boolean;
    FTransparentSet: Boolean;
    FVisibleCount: Integer;
    FOnAfterPaint: TTagCloudCustomPaint;
    FOnBeforePaint: TTagCloudCustomPaint;
    FOnCompareItems: TTagCompareEvent;
    FOnCustomDrawItem: TTagCustomDraw;
    FOnAdvCustomDrawItem: TTagAdvancedCustomDraw;
    FOnFilter: TTagFilterEvent;
    FOnHoverChange: TTagCloudEvent;
    FOnPageCountChanged: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnTagClick: TTagCloudEvent;
    FOnTagDblClick: TTagCloudEvent;
    FOnTagColor: TTagColorEvent;
    FOnTagHint: TTagHintEvent;
    FOnTagPositioning: TTagPositioningEvent;
    FUseSpeedSort: Boolean;
    FSelectedItem: TTagCloudItem;
    FSelectedColor: TColor;
    FShrinkDiacritic: Boolean;
    FStyler: TCustomTagCloudStyler;
    FItemFrame: TTagItemFrame;
    FHoverFrame: TTagItemFrame;
    FSelectedFrame: TTagItemFrame;
    FDirection: TTagCloudDirection;
    FVerticalAlignment: TVerticalAlignment;
    FAutoScaleFont: Boolean;

    FCmpCursor: TCursor;
    FMinValue: Int64;
    FMaxValue: Int64;
    FMinMaxDiff: Int64;
    FLogDiff: Extended;
    FTagFontsMetrics:array of TTagFontMetrics;
    FAutoScaleFontCoef:Extended;
    {$IF CompilerVersion>=20}
    procedure DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
    procedure DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
    {$ELSE}
    procedure DoDrawNormalText(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal);
      {$IF CompilerVersion>=15}
    procedure DoDrawThemeTextEx(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal);
      {$IFEND}
    {$IFEND}
    {$IF CompilerVersion>=18}
    procedure DoPaddingChange(Sender: TObject);
    {$IFEND}
    procedure CheckRebuild(Sender:TObject; Param: Integer);
    function GetHoveredSize(FontSize:Integer):Integer;
    function GetTransparent: Boolean;
    function IsColorsStored: Boolean;
    function IsCustomScaleStored: Boolean;
    function IsItemsStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoShrinkRows(Value: Boolean);
    procedure SetColors(const Value: TTagCloudColors);
    procedure SetColSpacing(Value: Integer);
    procedure SetCustomScale(const Value: TTagCloudCustomScale);

    procedure SetFilter(const Value: String);
    procedure SetFixedColCount(Value: Integer);
    procedure SetFixedColWidth(const Value: Integer);
    procedure SetFixedColFullFrame(Value: Boolean);
    procedure SetGlowSize(const Value: Integer);
    procedure SetItems(const Value: TTagCloudItems);
    procedure SetHoverEnlarge(Value: Boolean);
    procedure SetHoverColSpace(Value: Boolean);
    procedure SetHoverStyle(const Value: TFontStyles);
    procedure SetHoverItem(const Value:TTagCloudItem);
    procedure SetLoadingOptions(Value: TTagCloudLoadingOptions);
    procedure SetLogScale(Value: Boolean);
    procedure SetMaxFontSize(Value: Integer);
    {$IF CompilerVersion>=18}
    procedure SetPadding(const Value: TPadding);
    {$IFEND}
    procedure SetOnAfterPaint(Value: TTagCloudCustomPaint);
    procedure SetOnBeforePaint(Value: TTagCloudCustomPaint);
    procedure SetOnCompareItems(Value: TTagCompareEvent);
    procedure SetOnTagPositioning(Value: TTagPositioningEvent);
    procedure SetOnCustomDrawItem(Value: TTagCustomDraw);
    procedure SetOnAdvCustomDrawItem(Value: TTagAdvancedCustomDraw);
    procedure SetPageIndex(Value: Integer);
    procedure SetRowSpacing(Value: Integer);
    procedure SetSorted(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetUseSpeedSort(Value: Boolean);
    procedure SetSelectedItem(const Value:TTagCloudItem);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetShrinkDiacritic(Value: Boolean);
    procedure SetStyler(Value: TCustomTagCloudStyler);
    procedure SetItemFrame(Value: TTagItemFrame);
    procedure SetHoverFrame(Value: TTagItemFrame);
    procedure SetSelectedFrame(Value: TTagItemFrame);
    procedure SetDirection(Value: TTagCloudDirection);
    procedure SetVerticalAlignment(Value: TVerticalAlignment);
    procedure SetAutoScaleFont(Value: Boolean);

    // Helper method to assign temporary colors (used for drawing) and other visual aspects to the item
    procedure _RebuildItemColors(Item:TTagCloudItem; CustomScaleItem:TTagCustomScaleItem = nil);
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    FDrawTextProc: TTagFNDrawText;
    procedure CheckInvalidateRect(Rect: TRect);
    // Handles the font changes of a component
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    // Handles the displaying of the hint
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    { This method returns the drawing rectangle for tag cloud items. In descendant classes, you can modify the result,
      so you can use the space for your guidelines or other informations. }
    function GetItemsRect:TRect; virtual;
    // Main drawing procedure. Draws all the tag items in a current clipping rectangle
    procedure DoDrawItems(aCanvas:TCanvas; Rect: TRect); virtual;
    // Called immediately after the component is created and loaded from VCL stream
    procedure Loaded; override;
    // Handling notifications from linked components (ie. stylers, popupmenus, etc.)
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    // Main painting procedure. Paints the background of the component and calls the DoDrawItems method
    procedure Paint; override;
    { This function is a core of TCustomTagCloud component. It calculates all the parameters to draw the tag cloud items
      when the items collection or other component property is changed.
      The calculated drawing parameters are stored in a temporary fields of each tag item, so the drawing itself is then faster.
      The function returns
        -1 if the control has no parent assigned (ie. no calculation has been processed)
         0 on success,
         1 if no item can be shown (ie. the control is not large enough to display at least one row of tag items).
      So you can perform some actions to change the component size or give the user some info depending on the returning value.
      Starting from version 1.1, the better way to check if all the items can fit to the control's area, is to use the OnPageCountChanged event. }
    function Rebuild(DoRedraw:Boolean=True):Integer;
    { Rebuilds only the colors and other visual aspects of the item (or all items, if nil is passed), that do not cover the tag size.
      Implemented to support better rebuilding performance. }
    procedure RebuildColors(anItem:TTagCloudItem = nil);
    { Performs the OnFilter event for all the tag items, when Filter is set. If OnFilter event is not assigned, an internal items's caption comparision
      with the Filter property string is performed. }
    procedure DoFilter;
    // Handles resizing of the component
    procedure Resize; override;
    // Handles mouse click events, so the item clicks can be processed.
    procedure Click; override;
    // Handles mouse double-click events, so the item double-clicks can be processed.
    procedure DblClick; override;
    // Handles mouse movements, so the item hover event can be processed.
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    {$IF CompilerVersion>=15}
    // AutoSize property assignment
    procedure SetAutoSize(Value: Boolean); override;
    {$ELSE}
    // AutoSize property assignment
    procedure SetAutoSize(Value: Boolean);
    {$IFEND}
    // Internal event handler, that will trigger everytime the Items.Update method is called. It is implemented mainly for descendant classes, to handle the changes easily.
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    // Setting this property to non empty string invokes filtering mechanism. When no OnFilter handler is assigned, then internal filtering by caption is processed.
    property Filter: string read FFilter write SetFilter;
    { A special class that defines the options for loading tag cloud items from an external resources
      (TStrings object, CSV based text file, etc. }
    property LoadingOptions: TTagCloudLoadingOptions read FLoadingOptions write SetLoadingOptions;
    { Event handler that allows you to perform a custom drawing on the tag cloud component surface, after all the items are drawn. }
    property OnAfterPaint: TTagCloudCustomPaint read FOnAfterPaint write SetOnAfterPaint;
    { Event handler that allows you to perform a custom drawing on the tag cloud component surface, before all the items are drawn.
      Remember, that if you assign the OnBeforePaint event, you should fill the component background properly, with aero glass paint or other parent control states in mind. }
    property OnBeforePaint: TTagCloudCustomPaint read FOnBeforePaint write SetOnBeforePaint;
    { Event handler, that allows you to perform a custom sorting of the tag items. }
    property OnCompareItems: TTagCompareEvent read FOnCompareItems write SetOnCompareItems;
    { Event handler that allows you to perform a custom drawing of the tag items. All the default text attributes are already pre-filled in a TargetCanvas properties and in the TextFlags parameter.
      DefaultDraw parameter is False by default, but if you change it to True, the tag will be painted as usual. It is suitable for painting a frame, icon or other background
      and then let the component to draw the text of the item. Transparent property must be True if you want to use both the DefaultDrawing and a painting of your own background.
      Also you can modify the TextRect dimensions (if needed) to process the default drawing in a different area. }
    property OnCustomDrawItem: TTagCustomDraw read FOnCustomDrawItem write SetOnCustomDrawItem;
    { An advanced version of OnCustomDrawItem event, that allows you to use (or modify) also the frame rectangle of the customized item.
      It is provided mainly to support a full customization by the tag cloud stylers.
      If OnAdvancedCustomDrawItem event is assigned, OnCustomDrawItem event is ignored. }
    property OnAdvancedCustomDrawItem: TTagAdvancedCustomDraw read FOnAdvCustomDrawItem write SetOnAdvCustomDrawItem;
    { Event handler for processing the tag items when Filtered is True. If no handler is assigned, internal event for filtering by caption is used. }
    property OnFilter: TTagFilterEvent read FOnFilter write FOnFilter;
    { This event allows you to process your own actions when a tag cloud item under the mouse is changed.
      Item parameter can have a nil value, when no tag item is hovered. }
    property OnHoverChange: TTagCloudEvent read FOnHoverChange write FOnHoverChange;
    { This event informs you, that the number of possible pages for viewing the tag items has been changed.
      The number of pages depends on the control dimensions, a number of tag items and the AutoSize property. }
    property OnPageCountChanged: TNotifyEvent read FOnPageCountChanged write FOnPageCountChanged;
    { This event is triggered whenever the tag cloud has been rebuilded and/or when the PageIndex has been changed.
      The FirstDisplayIndex and LastDisplayIndex properties hold the indices of the currently displayed items range. }
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    { This event allows you to process your own actions when a tag cloud item is clicked.
      Item parameter has always a value different from nil. If the control is clicked and no item is hovered,
      only an OnClick event is fired. Please, remember, that if you assign both OnTagClick and OnTagDblClick, you should
      implement some timer to distinguish between them (see AutoScaleFont demo project for that). }
    property OnTagClick: TTagCloudEvent read FOnTagClick write FOnTagClick;
    { This event allows you to process your own actions when a tag cloud item is double-clicked.
      Item parameter has always a value different from nil. If the control is double-clicked and no item is hovered,
      only an OnDblClick event is fired. Please, remember, that if you assign both OnTagClick and OnTagDblClick, you should
      implement some timer to distinguish between them (see AutoScaleFont demo project for that). }
    property OnTagDblClick: TTagCloudEvent read FOnTagDblClick write FOnTagDblClick;
    { By processing this event, you can assign a custom color to any item. In some cases, it is suitable to distinguish
      the tag items by a different criteria than the classic tag cloud value scaling by the color levels. }
    property OnTagColor: TTagColorEvent read FOnTagColor write FOnTagColor;
    { This event allows you to display your custom hint for a tag item. The Hint property value of an item is not changed,
      but your hint is displayed to the user. }
    property OnTagHint: TTagHintEvent read FOnTagHint write FOnTagHint;
    { Event handler for customizing the position of tag items. It is called everytime the item's position is rebuilded (calculated),
      so you can adjust the X or Y values or setup one of the predefined break kinds. }
    property OnTagPositioning: TTagPositioningEvent read FOnTagPositioning write SetOnTagPositioning;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    // Sets the Selected property of all items to False and SelectedItem to nil
    procedure ClearSelection;
    // Returns the current tag cloud item at X, Y position.
    function GetItemAt(X, Y:Integer):TTagCloudItem;
    { Returns True, if frames or backgrounds are used for drawing the tag cloud items. Knowing this is very important,
      because when frames are used, the position and size of each item is calculated differently, apart from the standard (text only) tag cloud.
      The properties that affect the use of frames, are ItemFrame, HoverFrame, SelectedFrame
      and SelectedFrameSize. }
    function FramesInUse:Boolean;
    { Loads the tag cloud items from an array. You can use this method for operations such as transfering items
      between two TCustomTagCloud components, or for fast manually filling the items. This method does not take into
      account LoadingOptions parameters. }
    procedure LoadFromArray(var Source:TTagCloudRecords);
    { Loads the tag cloud items from a CSV based file. See the LoadFromStrings for more description. }
    procedure LoadFromFile(const FileName:string);
    { Loads the tag cloud items from a CSV based text data stored in TStream. See the LoadFromStrings for more description. }
    procedure LoadFromStream(const Stream:TStream);
    { Loads the tag items from TStrings object. Each TStrings item can be divided into columns (caption, value, hint, etc. )
      by the separator character (defined in LoadingOptions property).
      First, all the items are loaded into an internal array (TTagCloudRecords), then they are processed according to the
      parameters defined in LoadingOptions property, and finally the items are stored into the component's items collection. }
    procedure LoadFromStrings(Source:TStrings);
    // Saves the tag cloud items into an array. TTagCloudRecord structure is used for each array element.
    procedure SaveToArray(var Dest:TTagCloudRecords);
    // Saves the tag cloud items into a CSV based text file. See the SaveToStrings for more description.
    procedure SaveToFile(const FileName:string);
    // Saves the tag cloud items into a CSV based text data stream. See the SaveToStrings for more description.
    procedure SaveToStream(const Stream:TStream);
    { Saves the tag cloud items into TStrings object. Each TStrings item represents one tag cloud item, where the Caption,
      Value, Hint, Tag and Data properties are stored according to the parameters defined in the LoadingOptions property.
      Note: LoadingOptions are not handled here exactly as for loading the items. You can specify the columns separator and
      the presence of desired columns (ColCaption, ColValue, etc.) by assigning them some values greater or equal to zero. }
    procedure SaveToStrings(Source:TStrings);
    // Sorts the tag items alphabetically. Use this method for one time sorting when the Sorted property is False.
    procedure Sort;
    // Access to the painting canvas
    property Canvas;
    // Returns the number of currently displayed items (on currently displayed page)
    property DisplayCount: Integer read FDisplayCount;
    // Indicates the first displayed tag item index
    property FirstDisplayIndex: Integer read FFirstDisplayIndex;
    // Indicates the last displayed tag item index
    property LastDisplayIndex: Integer read FLastDisplayIndex;
    // Identifies the current tag cloud item under the mouse cursor
    property HoverItem:TTagCloudItem read FHoverItem write SetHoverItem;
    { Returns the maximum value from the current list of items. This property is not available during rebuilding the items
     (ie. State property contains tcsRebuilding) or when the items are in an updating state and some values has been changed (ie. Items.UpdateCount>0). }
    property MaxValue: Int64 read FMaxValue;
    { Returns the minimal value from the current list of items. This property is not available during rebuilding the items
     (ie. State property contains tcsRebuilding) or when the items are in an updating state and some values has been changed (ie. Items.UpdateCount>0). }
    property MinValue: Int64 read FMinValue;
    // Identifies the selected tag cloud item (selection is not handled automatically - you have to assign the item at runtime)
    property SelectedItem:TTagCloudItem read FSelectedItem write SetSelectedItem;
    // Indicates the number of pages for multi-page tag cloud
    property PageCount: Integer read FPageCount;
    // Indicates the number of rows in the whole tag cloud (including currently not displayed rows)
    property RowCount: Integer read FRowCount;
    // Indicates the number of currently displayed rows in the tag cloud client rectangle
    property VisibleRowCount: Integer read FVisibleRowCount;
    // Indicates the current state of TCustomTagCloud background processes
    property State: TTagCloudStates read FState;
    // Allows you to use a non-standard workaround for fast sorting of large amounts of TCustomTagCloud items. It must be set to True at run-time, if you want to use it.
    property UseSpeedSort: Boolean read FUseSpeedSort write SetUseSpeedSort default False;
    // Returns the number of visible tag items (ie items, where the Visible property is True)
    property VisibleCount: Integer read FVisibleCount;
    { This method can be used to retrieve the metrics of desired tag item. It includes all the calculated rectangles, font height, current page index of an item, and so on.
      Returned values may vary depending on the tag cloud visual options, such as ShrinkDiacritics, ItemFrame, HoverFrame, FixedColFullFrame, etc. }
    procedure GetItemMetrics(Item:TTagCloudItem; var M:TTagCloudItemMetrics);
    { Returns item page index in a multi-page tag cloud}
    function GetItemPageIndex(Item:TTagCloudItem):Integer;
    { Returns item row index within the tag cloud }
    function GetItemRowIndex(Item:TTagCloudItem):Integer;

    function ValidateItemHoverColSpaceRect(Item:TTagCloudItem; var FrameRect:TRect):Boolean;
    // Alignment of the tag cloud items.
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    // Automatically calculates the height of each row. This creates a more efficient tag cloud with variable row heights.
    property AutoShrinkRows: Boolean read FAutoShrinkRows write SetAutoShrinkRows default False;
    {$IF CompilerVersion>=15}
    // Autosize of the component (in vertical direction). For horizontal direction autosizing you have to setup the FixedColWidth or FixedColCount property.
    property AutoSize default False;
    {$ELSE}
    // Autosize of the component (in vertical direction). For horizontal direction autosizing you have to setup the FixedColWidth or FixedColCount property.
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$IFEND}
    // Background color of the component, when it is not transparent
    property Color nodefault;
    { Defines the color levels for tag cloud items. If you do not define any color level,
      only the Font.Size and MaxFontSize properties will be used for distinguish the tags in the cloud,
      and the Font.Color will be used for drawing. }
    property Colors: TTagCloudColors read FColors write SetColors stored IsColorsStored;
    // Defines the horizontal (column) spacing between the tag cloud items.
    property ColSpacing: Integer read FColSpacing write SetColSpacing default 8;
    { Tag cloud custom scale definition. With the help of CustomScale property you can override the default linear (or logarithmic) scaling
      by defining your own ranges of values with associated font sizes and colors. You can mix the standard and custom scaling, so for example
      you can define only one custom range of values, that will be displayed differently among the other tag items. See also TTagCloudCustomScale class. }
    property CustomScale: TTagCloudCustomScale read FCustomScale write SetCustomScale stored IsCustomScaleStored;
    { If the FixedColCount property value is grater than 0, the FixedColWidth property will be calculated automatically according to the tag cloud control width
      and all the tags will have the same width and they will be arranged in the defined columns. }
    property FixedColCount: Integer read FFixedColCount write SetFixedColCount default 0;
    // Fixed width for each tag item. If this property is greater than 0, all the tags will have the same width.
    property FixedColWidth: Integer read FFixedColWidth write SetFixedColWidth default 0;
    { A width of the frame for fixed width tags will be equal to the FixedColWidth value. If this property is False, the frame for fixed width tags will be
      drawn around the text only. }
    property FixedColFullFrame: Boolean read FFixedColFullFrame write SetFixedColFullFrame default False;
    { Defines the font properties of the tag cloud items.
      The Font.Size defines the minimum font size for scaling the tag cloud items (if there is no CustomScale range with smaller FontSize defined). }
    property Font;
    // Glow size used when painting in transparent mode. For Windows Vista and later, with Aero Glass effect enabled.
    property GlowSize: Integer read FGlowSize write SetGlowSize default 0;
    // Defines the color for hovered tag cloud item (item under the mouse).
    property HoverColor: TColor read FHoverColor write FHoverColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Defines the mouse cursor shape for hovered tag cloud item (item under the mouse).
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor default crHandPoint;
    // Defines the font style for hovered tag cloud item (item under the mouse).
    property HoverStyle: TFontStyles read FHoverStyle write SetHoverStyle nodefault;
    // If this property is True, the hovered item (item under the mouse) will be enlarged.
    property HoverEnlarge: Boolean read FHoverEnlarge write SetHoverEnlarge default False;
    { This allows you to handle the space between items as a part of the hovered item, so you can draw additional icon or anything else
      in custom drawing events and react on clicks when the mouse cursor is inside this area }
    property HoverColSpace: Boolean read FHoverColSpace write SetHoverColSpace default False;
    // Defines the frame style for tag items
    property ItemFrame: TTagItemFrame read FItemFrame write SetItemFrame;
    // Defines the frame style for hovered tag items
    property HoverFrame: TTagItemFrame read FHoverFrame write SetHoverFrame;
    // Defines the frame style for selected tag items
    property SelectedFrame: TTagItemFrame read FSelectedFrame write SetSelectedFrame;
    // Direction for composing the tag items horizontally or vertically. It is applicable only for fixed-width tag clouds.
    property Direction: TTagCloudDirection read FDirection write SetDirection default tcdHorizontal;

    // Access to the tag cloud items collection. You can add, insert, delete and modify the items via this property.
    property Items: TTagCloudItems read FItems write SetItems stored IsItemsStored;
    { Defines the logarithmic scaling of tag cloud items. TCustomTagCloud component can scale the items by the linear
      or logarithmic function. A logarithmic representation makes sense for larger ranges of values, since the linear
      function can be successfully used for a small ranges. This property affects both font and color levels scaling.
      See also CustomScale property. }
    property LogScale: Boolean read FLogScale write SetLogScale default False;
    { This property defines a maximum font size for scaling the tag cloud items. If the value is the same as a Font.Size,
      only the color levels (or CustomScale ranges) will be used for distinguish the tags in the cloud. }
    property MaxFontSize: Integer read FMaxFontSize write SetMaxFontSize nodefault;
    {$IF CompilerVersion>=18}
    // Padding defines the spacing between the component frame and the tag items.
    property Padding: TPadding read FPadding write SetPadding;
    {$IFEND}
    // Takes the same background color as its parent control (when not using the transparent mode)
    property ParentColor;
    // Takes the font of its parent control
    property ParentFont default True;
    // Defines the color for selected tag cloud item
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    { If True, ignores diacritical marks during calculation of tag item height. If you are using only lower-case captions,
      or if you do not use captions with diacritical marks, then set this property to True, to let the resulting tag cloud be more shrunken.
      On the other hand, if your tag cloud contains upper-case characters with diacritical marks, and you are using the item frames or backgrounds,
      set this property to False, to draw the items correctly (without overlaps. }
    property ShrinkDiacritic: Boolean read FShrinkDiacritic write SetShrinkDiacritic default True;
    // Reference to TCustomTagCloudStyler descendant that defines the tag cloud style properties
    property Styler: TCustomTagCloudStyler read FStyler write SetStyler;
    // Defines the current visible page index for multi-page tag cloud
    property PageIndex: Integer read FPageIndex write SetPageIndex default 0;
    { Defines the vertical (row) spacing between the tag cloud items. The row height is calculated as a sum of the height
      of the text in the MaximumFontSize and the RowSpacing value. You can assign a negative values to this property,
      so the rows will be less apart. }
    property RowSpacing: Integer read FRowSpacing write SetRowSpacing default 4;
    // If True, the TagCloud items will be automatically sorted by Caption
    property Sorted: Boolean read FSorted write SetSorted nodefault;
    // The control will be drawn in a transparent mode.
    property Transparent: Boolean read GetTransparent write SetTransparent stored FTransparentSet;
    // Vertical alignment for tag cloud items displayed on one page. This property is ignored when AutoSize property is True.
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
    { If this property is set to True, the tag cloud tries to scale the items (their display font size) to fit as much available space as possible.
      This property is ignored when AutoSize property is True, or when the fixed-width style is applied (FixedColCount or FixedColWidth are greater than 0). }
    property AutoScaleFont: Boolean read FAutoScaleFont write SetAutoScaleFont default False;
  end;

  { Tag cloud visual component implementation. }
  TTagCloud = class(TCustomTagCloud)
  published
    // Align in the parent control.
    property Align;
    { See @inherited }
    property Alignment default taCenter;
    // Component anchors
    property Anchors;
    { See @inherited }
    property AutoScaleFont default False;
    { See @inherited }
    property AutoShrinkRows default False;
    { See @inherited }
    property AutoSize default False;
    // Bidirectional text drawing mode
    property BiDiMode;
    { See @inherited }
    property Color nodefault;
    { See @inherited }
    property Colors;
    { See @inherited }
    property ColSpacing;
    { See @inherited }
    property CustomScale;
    // Defines the component size constraints.
    property Constraints;
    { See @inherited }
    property Direction;
    // Defines the drag cursor.
    property DragCursor;
    // Defines the drag kind for component
    property DragKind;
    // Defines the drag mode for component
    property DragMode;
    // Enabling or disabling the component. If the component is disabled, the items are painted with clGrayText color.
    property Enabled;
    { See @inherited }
    property Filter;
    { See @inherited }
    property FixedColCount;
    { See @inherited }
    property FixedColFullFrame;
    { See @inherited }
    property FixedColWidth;
    { See @inherited }
    property Font;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property HoverColor;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property HoverStyle;
    { See @inherited }
    property HoverEnlarge;
    { See @inherited }
    property HoverColSpace;
    { See @inherited }
    property ItemFrame;
    { See @inherited }
    property HoverFrame;
    { See @inherited }
    property SelectedFrame;
    { See @inherited }
    property Items;
    { See @inherited }
    property LoadingOptions;
    { See @inherited }
    property LogScale;
    { See @inherited }
    property MaxFontSize;
    {$IF CompilerVersion>=18}
    // Component margins
    property Margins;
    { See @inherited }
    property Padding;
    // OnMouseActivate event
    property OnMouseActivate;
    // OnMouseEnter event
    property OnMouseEnter;
    // OnMouseLeave event
    property OnMouseLeave;
    {$IFEND}
    { See @inherited }
    property SelectedColor;
    { See @inherited }
    property Styler;
    { See @inherited }
    property PageIndex;
    // Takes the same bidirectional text drawing mode as its parent control
    property ParentBiDiMode;
    { See @inherited }
    property ParentColor;
    { See @inherited }
    property ParentFont default True;
    // Takes the ShowHint property value from its parent control
    property ParentShowHint;
    // Popup menu for the component
    property PopupMenu;
    { See @inherited }
    property RowSpacing;
    // If True, the help hint will be shown for hovered tag items.
    property ShowHint;
    { See @inherited }
    property ShrinkDiacritic default True;
    { See @inherited }
    property Sorted nodefault;
    { See @inherited }
    property Transparent;
    { See @inherited }
    property VerticalAlignment default taAlignTop;
    // Visibility of the control
    property Visible;
    // OnClick event
    property OnClick;
    // OnContextPopup event
    property OnContextPopup;
    // OnDblClick event
    property OnDblClick;
    // OnDragDrop event
    property OnDragDrop;
    // OnDragOver event
    property OnDragOver;
    // OnEndDock event
    property OnEndDock;
    // OnEndDrag event
    property OnEndDrag;
    // OnMouseDown event
    property OnMouseDown;
    // OnMouseMove event
    property OnMouseMove;
    // OnMouseUp event
    property OnMouseUp;
    // OnStartDock event
    property OnStartDock;
    // OnStartDrag event
    property OnStartDrag;
    { See @inherited }
    property OnAfterPaint;
    { See @inherited }
    property OnBeforePaint;
    { See @inherited }
    property OnCompareItems;
    { See @inherited }
    property OnCustomDrawItem;
    { See @inherited }
    property OnAdvancedCustomDrawItem;
    { See @inherited }
    property OnFilter;
    { See @inherited }
    property OnHoverChange;
    { See @inherited }
    property OnPageCountChanged;
    // OnCanResize event
    property OnCanResize;
    // Resize event
    property OnResize;
    { See @inherited }
    property OnShow;
    { See @inherited }
    property OnTagClick;
    { See @inherited }
    property OnTagDblClick;
    { See @inherited }
    property OnTagColor;
    { See @inherited }
    property OnTagPositioning;
    { See @inherited }
    property OnTagHint;
  end;

{ This function returns the tag cloud item value parsed from a string. It is suitable to use it when loading the values from
  a text file or other string representation. }
function StrToTagCloudValue(const S:string;Def:Int64=0):Int64;

implementation

uses
  {$IF CompilerVersion<20}
  Forms,
  {$IFEND}
  {$IF CompilerVersion>=15}
  Themes, UxTheme,
  {$IFEND}
  {$IF CompilerVersion >= 24} System.Types, System.UITypes, {$IFEND}
  SysUtils;

////////////////////////////////////////// Misc Routines ////////////////////////////////////////////
{$IF CompilerVersion < 23.0 }
function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}

function StrToTagCloudValue(const S:string;Def:Int64=0):Int64;
var
  vcode:Integer;
begin
  Val(S,Result,vcode);
  if vcode>0 then
  begin
    Val(Copy(S,1,vcode-1),Result,vcode);
    if vcode>0 then
      Result:=Def;
  end;
end;

function SameRect(var R1,R2:TRect):Boolean;
begin
  Result:=(R1.Left=R2.Left) and (R1.Top=R2.Top) and (R1.Right=R2.Right) and (R1.Bottom=R2.Bottom);
end;

//////////////////////////////////////////// TTagCloudItem ///////////////////////////////////////////
constructor TTagCloudItem.Create(Collection: TCollection);
begin
  inherited;
  FTCCollection:=TTagCloudItems(Collection);
  if ( csDesigning in FTCCollection.FTagCloud.ComponentState ) and
     not ( csLoading in FTCCollection.FTagCloud.ComponentState ) then
    FCaption := 'Item' + IntToStr( Index + 1 );
  FHint := '';
  FValue := 0;
  FTag := 0;
  FData := nil;
  FVisible := True;
  FSelected := False;
  FPageIndex := -1;
  FRowIndex := -1;

  FColor := FTCCollection.FTagCloud.Font.Color;
  FSize := FTCCollection.FTagCloud.Font.Size;
  FBackColor := FTCCollection.FTagCloud.FItemFrame.BackColor;
  FFrameColor := FTCCollection.FTagCloud.FItemFrame.FFrameColor;

  FNeedsValidate := True;
end;

{$IF CompilerVersion<15}
destructor TTagCloudItem.Destroy;
begin
  FTCCollection.Deleting(Self);
  inherited;
end;
{$IFEND}

function TTagCloudItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TTagCloudItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    if FTCCollection.FTagCloud.FSorted and (not (tcsNeedsSort in FTCCollection.FTagCloud.FState))
      and (CompareStr(FCaption, Value)<>0) then
      Include(FTCCollection.FTagCloud.FState, tcsNeedsSort);
    FCaption := Value;
    FNeedsValidate := True;
    Changed(True);
  end;
end;

procedure TTagCloudItem.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed(False);
  end;
end;

procedure TTagCloudItem.SetSelected(Value: Boolean);
begin
  if Value<>FSelected then
  begin
    FSelected := Value;
    if FTCCollection.UpdateCount<=0 then
      FTCCollection.FTagCloud.CheckRebuild(Self,2); { TODO : Needs better handling than full repaint }
  end;
end;

procedure TTagCloudItem.SetValue(Value: Int64);
begin
  if Value<>FValue then
  begin
    if (FValue<>Value) and FTCCollection.FTagCloud.FSorted and
      Assigned(FTCCollection.FTagCloud.FOnCompareItems) then
      Include(FTCCollection.FTagCloud.FState, tcsNeedsSort);
    FValue := Value;
    FNeedsValidate := True;
    Changed(True);
  end;
end;

procedure TTagCloudItem.SetVisible(Value: Boolean);
begin
  if Value<>FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TTagCloudItem.Assign(Source: TPersistent);
begin
  if Source is TTagCloudItem then
  begin
    if FTCCollection.FTagCloud.FSorted and (not (tcsNeedsSort in FTCCollection.FTagCloud.FState)) and
      (((FValue<>TTagCloudItem(Source).Value) and Assigned(FTCCollection.FTagCloud.FOnCompareItems))
        or (CompareStr(FCaption, TTagCloudItem(Source).Caption)<>0)) then
      Include(FTCCollection.FTagCloud.FState, tcsNeedsSort);
    FCaption := TTagCloudItem(Source).Caption;
    FHint := TTagCloudItem(Source).Hint;
    FData := TTagCloudItem(Source).Data;
    FTag := TTagCloudItem(Source).Tag;
    FValue := TTagCloudItem(Source).Value;
    FVisible := TTagCloudItem(Source).FVisible;
    FNeedsValidate := True;
    Changed(True);
  end
  else
    inherited;
end;

procedure TTagCloudItem.AssignRecord(Source: PTagCloudRecord; RequestChange:Boolean=False);
begin
  if RequestChange and FTCCollection.FTagCloud.FSorted and (not (tcsNeedsSort in FTCCollection.FTagCloud.FState)) and
    (((FValue<>Source^.Value) and Assigned(FTCCollection.FTagCloud.FOnCompareItems))
      or (CompareStr(FCaption, Source^.Caption)<>0)) then
    Include(FTCCollection.FTagCloud.FState, tcsNeedsSort);
  FCaption := Source^.Caption;
  FHint := Source^.Hint;
  FData := Source^.Data;
  FTag := Source^.Tag;
  FValue := Source^.Value;
  FNeedsValidate := True;
  if RequestChange then
    Changed(True);
end;

procedure TTagCloudItem.AssignToRecord(Dest: PTagCloudRecord);
begin
  Dest^.Caption := FCaption;
  Dest^.Hint := FHint;
  Dest^.Data := FData;
  Dest^.Tag := FTag;
  Dest^.Value := FValue;
end;

//////////////////////////////////////////// TTagCloudItems ///////////////////////////////////////
function GetCollectionList(C: TCollection): TList;
var
  M: TMethod;
  L: TList absolute M;
  P: Pointer;
begin
  try
    P := @C.ItemClass;
    inc({$IFDEF WIN64}Int64{$ELSE}Integer{$ENDIF}(P), SizeOf(TCollectionItemClass));
    M := TMethod(P^);
    Result := L;
    if not Result.ClassNameIs('TList') then
      Result := nil;
  except
    Result := nil;
  end;
end;

constructor TTagCloudItems.Create(TagCloud: TCustomTagCloud);
begin
  inherited Create(TTagCloudItem);
  FTagCloud := TagCloud;
  FItemsRef := nil;
end;

function TTagCloudItems.GetOwner: TPersistent;
begin
  Result := FTagCloud;
end;

procedure TTagCloudItems.Update(Item: TCollectionItem);
begin
  if (Item=nil) and (not (tcsSorting in FTagCloud.FState)) then
  begin
    if FTagCloud.FSorted and (tcsNeedsSort in FTagCloud.FState) then
      Sort;
    if Assigned(FTagCloud.OnItemsChanged) then
      FTagCloud.OnItemsChanged(FTagCloud);
    FTagCloud.Rebuild;
  end;
end;

{$IF CompilerVersion>=15}
procedure TTagCloudItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnDeleting:
      if Item=FTagCloud.FSelectedItem then
        FTagCloud.SelectedItem:=nil;
  end;
  inherited;
end;
{$ELSE}
procedure TTagCloudItems.Deleting(Item: TCollectionItem);
begin
  if Item=FTagCloud.FSelectedItem then
    FTagCloud.SelectedItem:=nil;
  inherited;
end;
{$IFEND}

function TTagCloudItems.GetItem(Index: Integer): TTagCloudItem;
begin
  Result := inherited GetItem(Index) as TTagCloudItem;
end;

procedure TTagCloudItems.SetItem(Index: Integer; Value: TTagCloudItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TTagCloudItems.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AnsiCompareText(Items[i].FCaption,Items[p].FCaption) < 0 do Inc(I);
      while AnsiCompareText(Items[j].FCaption,Items[p].FCaption) > 0 do Dec(J);
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

procedure TTagCloudItems.CustomQuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while FTagCloud.FOnCompareItems(FTagCloud,Items[i],Items[p]) < 0 do Inc(I);
      while FTagCloud.FOnCompareItems(FTagCloud,Items[j],Items[p]) > 0 do Dec(J);
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
    if L < J then CustomQuickSort(L, J);
    L := I;
  until I >= R;
end;

function TTagCloudItems.InternalFind(const aCaption: string; var Index: integer):Boolean;
var
  i:Integer;
  H, L, C: Integer;
begin
  Result:=False;
  if FTagCloud.FSorted and (not (tcsNeedsSort in FTagCloud.FState)) and (not Assigned(FTagCloud.FOnCompareItems)) then
  begin
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := AnsiCompareText(Items[I].Caption,aCaption);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
    Index := L;
  end
  else
  begin
    Index:=Count;
    for i:=0 To Count-1 do
      if AnsiCompareText(aCaption,Items[i].Caption)=0 then
      begin
        Result:=True;
        Index:=i;
        break;
      end;
  end
end;

function TTagCloudItems.Add: TTagCloudItem;
begin
  Result := inherited Add as TTagCloudItem;
end;

function TTagCloudItems.AddItem(const aCaption: string; aValue: Int64 = 0; const aHint: string = ''; aTag: LongInt = 0; aData: Pointer = nil): TTagCloudItem;
var
  i:Integer;
begin
  if Count=0 then
    Result := inherited Add as TTagCloudItem
  else
  if InternalFind(aCaption,i) then
  begin
    Result:=Items[i];
    Result.FNeedsValidate := True;
  end
  else
    Result := inherited Insert(i) as TTagCloudItem;

  Result.FCaption := aCaption;
  Result.FHint := aHint;
  Result.FTag := aTag;
  Result.FData := aData;
  Result.Value := aValue;
end;

function TTagCloudItems.Insert(Index:Integer): TTagCloudItem;
begin
  if FTagCloud.FSorted then
    Result := Add
  else
    Result := inherited Insert(Index) as TTagCloudItem;
end;

procedure TTagCloudItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TTagCloudItems.Move(CurrIndex, NewIndex: Integer);
begin
  if not FTagCloud.FSorted then
  begin
    if Assigned(FItemsRef) then
      FItemsRef.Move(CurrIndex,NewIndex)
    else
      Items[CurrIndex].Index := NewIndex;
  end
end;

function TTagCloudItems.IncreaseValue(const aCaption: string; Increment:Int64=1; AddIfNotExists: Boolean = True): TTagCloudItem;
var
  i:Integer;
begin
  Result:=nil;
  if Count=0 then
  begin
    if AddIfNotExists then
    begin
      Result := inherited Add as TTagCloudItem;
      Result.FCaption := aCaption;
      Result.Value:=Increment;
    end;
  end
  else
  begin
    if InternalFind(aCaption,i) then
    begin
      Result:=Items[i];
      Result.Value:=Result.Value + Increment;
    end
    else
    if AddIfNotExists then
    begin
      Result := inherited Insert(i) as TTagCloudItem;
      Result.FCaption := aCaption;
      Result.Value:=Result.Value + Increment;
    end
  end;
end;

procedure TTagCloudItems.Sort;
begin
  Include(FTagCloud.FState,tcsSorting);
  try
    Exclude(FTagCloud.FState, tcsNeedsSort);
    if Count>0 then
    begin
      if Assigned(FTagCloud.FOnCompareItems) then
        CustomQuickSort(0, Count - 1)
      else
        QuickSort(0, Count - 1);
    end;
  finally
    Exclude(FTagCloud.FState,tcsSorting);
  end;
end;

function TTagCloudItems.Find(const aCaption:string):TTagCloudItem;
var
  i:Integer;
begin
  if InternalFind(aCaption,i) then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TTagCloudItems.Find(aData: Pointer):TTagCloudItem;
var
  i:Integer;
begin
  Result:=nil;
  if aData<>nil then
    for i:=0 To Count-1 do
      if aData=Items[i].FData then
      begin
        Result:=Items[i];
        break;
      end;
end;

//////////////////////////////////////////// TTagItemFrame ////////////////////////////////////////////
constructor TTagItemFrame.Create;
begin
  inherited Create;
  FBackColor := clNone;
  FFrameSize := 0;
  FFrameMargin := 0;
  {$IF CompilerVersion>=15}
  FFrameColor := clDefault;
  {$ELSE}
  FFrameColor := clNone;
  {$IFEND}
  FFrameStyle := psSolid;
  FRoundedSize := 0;
  FOnChanged := nil;
end;

procedure TTagItemFrame.Assign(Source: TPersistent);
var
  FSource:TTagItemFrame;
begin
  if Source is TTagItemFrame then
  begin
    FSource := TTagItemFrame(Source);
    FBackColor := FSource.FBackColor;
    FFrameSize := FSource.FFrameSize;
    FFrameMargin := FSource.FFrameMargin;
    FFrameColor := FSource.FFrameColor;
    FFrameStyle := FSource.FFrameStyle;
    FRoundedSize := FSource.FRoundedSize;
  end
  else
    inherited Assign(Source);
end;

procedure TTagItemFrame.SetBackColor(Value: TColor);
begin
  if Value<>FBackColor then
  begin
    FBackColor:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,0) // rebuild all
  end;
end;

procedure TTagItemFrame.SetFrameColor(Value: TColor);
begin
  if Value<>FFrameColor then
  begin
    FFrameColor:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,1) // rebuild colors only
  end;
end;

procedure TTagItemFrame.SetFrameStyle(Value: TPenStyle);
begin
  if Value<>FFrameStyle then
  begin
    FFrameStyle:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,2) // invalidate only
  end;
end;

procedure TTagItemFrame.SetFrameSize(Value: Integer);
begin
  if Value<>FFrameSize then
  begin
    if Value<0 then
      Value:=0;
    FFrameSize:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,0) // rebuild all
  end;
end;

procedure TTagItemFrame.SetFrameMargin(Value: Integer);
begin
  if Value<>FFrameMargin then
  begin
    if Value<0 then
      Value:=0;
    FFrameMargin:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,3) // rebuild all without validation
  end;
end;

procedure TTagItemFrame.SetRoundedSize(Value: Integer);
begin
  if Value<>FRoundedSize then
  begin
    if Value<0 then
      Value:=0;
    FRoundedSize:=Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self,2) // invalidate only
  end;
end;

//////////////////////////////////////////// TTagColorItem ////////////////////////////////////////////
constructor TTagColorItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := TTagCloudColors(Collection).FTagCloud.Font.Color;
  {$IF CompilerVersion>=15}
  FBackColor := clDefault;
  FFrameColor := clDefault;
  {$ELSE}
  FBackColor := clNone;
  FFrameColor := clNone;
  {$IFEND}
end;

function TTagColorItem.GetDisplayName: string;
begin
  Result:='';
  if (FColor<>clNone) {$IF CompilerVersion>=15} and (FColor<>clDefault) {$IFEND} then
    Result:=ColorToString(FColor);
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TTagColorItem.SetColor(Value: TColor);
begin
  if Value<>FColor then
  begin
    FColor := Value;
    Changed(True);
  end;
end;

procedure TTagColorItem.SetBackColor(Value: TColor);
begin
  if Value<>FBackColor then
  begin
    FBackColor := Value;
    Changed(True);
  end;
end;

procedure TTagColorItem.SetFrameColor(Value: TColor);
begin
  if Value<>FFrameColor then
  begin
    FFrameColor := Value;
    Changed(True);
  end;
end;

procedure TTagColorItem.Assign(Source: TPersistent);
begin
  if Source is TTagColorItem then
  begin
    FColor := TTagColorItem(Source).Color;
    FBackColor := TTagColorItem(Source).FBackColor;
    FFrameColor := TTagColorItem(Source).FFrameColor;
    Changed(True);
  end
  else
    inherited;
end;

//////////////////////////////////////////// TTagCloudColors ///////////////////////////////////////////
constructor TTagCloudColors.Create(TagCloud: TCustomTagCloud);
begin
  inherited Create(TTagColorItem);
  FTagCloud := TagCloud;
end;

function TTagCloudColors.GetOwner: TPersistent;
begin
  Result := FTagCloud;
end;

procedure TTagCloudColors.Update(Item: TCollectionItem);
begin
  if Item=nil then
    FTagCloud.RebuildColors;
end;

{$IF CompilerVersion>=15}
procedure TTagCloudColors.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited;
end;
{$IFEND}

function TTagCloudColors.GetItem(Index: Integer): TTagColorItem;
begin
  Result := inherited GetItem(Index) as TTagColorItem;
end;

procedure TTagCloudColors.SetItem(Index: Integer; Value: TTagColorItem);
begin
  inherited SetItem(Index, Value);
end;

function TTagCloudColors.Add: TTagColorItem;
begin
  Result := inherited Add as TTagColorItem;
end;

function TTagCloudColors.AddColor(Clr:TColor; BackColor: TColor = clNone; FrameColor: TColor = {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND}): TTagColorItem;
begin
  Result := inherited Add as TTagColorItem;
  Result.FColor:=Clr;
  Result.FBackColor:=BackColor;
  Result.FrameColor:=FrameColor;
end;

function TTagCloudColors.Insert(Index:Integer): TTagColorItem;
begin
  Result := inherited Insert(Index) as TTagColorItem;
end;

procedure TTagCloudColors.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TTagCloudColors.Move(CurrIndex, NewIndex: Integer);
begin
  Items[CurrIndex].Index := NewIndex;
end;

//////////////////////////////////////////// TTagCustomScaleItem ////////////////////////////////////////
constructor TTagCustomScaleItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := TTagCloudCustomScale(Collection).FTagCloud.Font.Color;
  FFontSize := TTagCloudCustomScale(Collection).FTagCloud.Font.Size;
  FValueFrom := 0;
  FValueTo := 0;
  {$IF CompilerVersion>=15}
  FBackColor := clDefault;
  FFrameColor := clDefault;
  {$ELSE}
  FBackColor := clNone;
  FFrameColor := clNone;
  {$IFEND}
end;

procedure TTagCustomScaleItem.Assign(Source: TPersistent);
var
  SI:TTagCustomScaleItem;
begin
  if Source is TTagCustomScaleItem then
  begin
    SI:=TTagCustomScaleItem(Source);
    FColor := SI.Color;
    FFontSize := SI.FontSize;
    FValueFrom := SI.ValueFrom;
    FValueTo := SI.ValueTo;
    FBackColor := SI.FBackColor;
    FFrameColor := SI.FFrameColor;
    Changed(True);
  end
  else
    inherited;
end;

function TTagCustomScaleItem.GetDisplayName: string;
begin
  Result:='';
  if FFontSize>0 then
    Result:=Result+',  Font: '+IntToStr(FFontSize)+' pt';
  if (FColor<>clNone) {$IF CompilerVersion>=15} and (FColor<>clDefault) {$IFEND} then
    Result:=Result+',  Color: '+ColorToString(FColor);
  Result := 'Range: '+IntToStr(FValueFrom)+' to '+IntToStr(FValueTo)+Result;
end;

procedure TTagCustomScaleItem.SetColor(Value: TColor);
begin
  if Value<>FColor then
  begin
    FColor := Value;
    Changed(True);
  end;
end;

procedure TTagCustomScaleItem.SetBackColor(Value: TColor);
begin
  if Value<>FBackColor then
  begin
    FBackColor := Value;
    Changed(True);
  end;
end;

procedure TTagCustomScaleItem.SetFrameColor(Value: TColor);
begin
  if Value<>FFrameColor then
  begin
    FFrameColor := Value;
    Changed(True);
  end;
end;

procedure TTagCustomScaleItem.SetFontSize(Value: Integer);
begin
  if Value<>FFontSize then
  begin
    if Value<0 then
      Value := 0;
    FFontSize := Value;
    Changed(True);
  end;
end;

procedure TTagCustomScaleItem.SetValueFrom(Value: Int64);
begin
  if Value<>FValueFrom then
  begin
    FValueFrom := Value;
    if FValueTo<FValueFrom then
      FValueTo:=FValueFrom;
    TTagCloudCustomScale(Collection).FSortState:=cssNeedsSort;
    Changed(True);
  end;
end;

procedure TTagCustomScaleItem.SetValueTo(Value: Int64);
begin
  if Value<>FValueTo then
  begin
    FValueTo := Value;
    if FValueFrom>FValueTo then
      FValueFrom:=FValueTo;
    TTagCloudCustomScale(Collection).FSortState:=cssNeedsSort;
    Changed(True);
  end;
end;

//////////////////////////////////////////// TTagCloudCustomScale ///////////////////////////////////////////
constructor TTagCloudCustomScale.Create(TagCloud: TCustomTagCloud);
begin
  inherited Create(TTagCustomScaleItem);
  FTagCloud := TagCloud;
  FSortState := cssNone;
end;

function TTagCloudCustomScale.GetOwner: TPersistent;
begin
  Result := FTagCloud;
end;

procedure TTagCloudCustomScale.Update(Item: TCollectionItem);
begin
  if (Item=nil) and (FSortState<>cssSorting) then
  begin
    if FSortState=cssNeedsSort then
      Sort;
    Include(FTagCloud.FState,tcsNeedsValidate);
    FTagCloud.Rebuild;
  end;
end;

procedure TTagCloudCustomScale.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Items[i].FValueFrom<Items[p].FValueFrom do Inc(I);
      while Items[j].FValueFrom>Items[p].FValueFrom do Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Items[J].Index := I;
          Items[I+1].Index := J;
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

procedure TTagCloudCustomScale.Sort;
var
  i:Integer;
begin
  FSortState:=cssSorting;
  try
    if Count>0 then
    begin
      QuickSort(0, Count - 1);
      for i:=0 To Count-2 do
        if Items[i].FValueTo>Items[i+1].FValueFrom then
          Items[i].FValueTo:=Items[i+1].FValueFrom;
    end;
  finally
    FSortState:=cssNone;
  end;
end;

{$IF CompilerVersion>=15}
procedure TTagCloudCustomScale.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited;
end;
{$IFEND}

function TTagCloudCustomScale.GetItem(Index: Integer): TTagCustomScaleItem;
begin
  Result := inherited GetItem(Index) as TTagCustomScaleItem;
end;

procedure TTagCloudCustomScale.SetItem(Index: Integer; Value: TTagCustomScaleItem);
begin
  inherited SetItem(Index, Value);
end;

function TTagCloudCustomScale.Add: TTagCustomScaleItem;
begin
  Result := inherited Add as TTagCustomScaleItem;
end;

function TTagCloudCustomScale.AddScale(aValueFrom, aValueTo:Int64; aFontSize:Integer; aColor:TColor;
    BackColor: TColor = clNone; FrameColor: TColor = {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND}): TTagCustomScaleItem;
begin
  Result := inherited Add as TTagCustomScaleItem;
  Result.FFontSize:=aFontSize;
  Result.FColor:=aColor;
  Result.FBackColor:=BackColor;
  Result.FFrameColor:=FrameColor;
  Result.FValueTo:=aValueTo;
  Result.ValueFrom:=aValueFrom;
end;

procedure TTagCloudCustomScale.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TTagCloudCustomScale.Find(Value: Int64; var fItem:TTagCustomScaleItem):boolean;
var
  i:Integer;
  H, L: Integer;
begin
  Result:=False;
  if FSortState=cssNone then
  begin
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      if Items[I].FValueTo<=Value then
        L := I + 1
      else
      begin
        H := I - 1;
        if Items[I].FValueFrom<=Value then
        begin
          L := I;
          Result := True;
          fItem := Items[L];
        end;
      end;
    end;
  end
  else
  begin
    for i:=0 To Count-1 do
      if (Value>=Items[I].FValueFrom) and (Value<Items[I].FValueTo) then
      begin
        Result := True;
        fItem := Items[I];
        break;
      end;
  end
end;

//////////////////////////////////////////// TTagCloudLoadingOptions ////////////////////////////////////
constructor TTagCloudLoadingOptions.Create;
begin
  inherited Create;
  FValuesGreaterThan:=0;
  FAlphaSort:=True;
  FLowerCase:=False;
  FMaxItemsCount:=64;
  FSeparator:=';';
  FSkipFirstRow:=False;
  FColCaption:=0;
  FColValue:=1;
  FColHint:=-1;
  FColTag:=-1;
  FColData:=-1;
end;

procedure TTagCloudLoadingOptions.Assign(Source: TPersistent);
var
  FSource:TTagCloudLoadingOptions;
begin
  if Source is TTagCloudLoadingOptions then
  begin
    FSource:=TTagCloudLoadingOptions(Source);
    FValuesGreaterThan:=FSource.ValuesGreaterThan;
    FAlphaSort:=FSource.AlphaSort;
    FLowerCase:=FSource.LowerCase;
    FMaxItemsCount:=FSource.MaxItemsCount;
    FSeparator:=FSource.Separator;
    FSkipFirstRow:=FSource.SkipFirstRow;
    FColCaption:=FSource.ColCaption;
    FColValue:=FSource.ColValue;
    FColHint:=FSource.ColHint;
    FColTag:=FSource.ColTag;
    FColData:=FSource.ColData;
  end
  else
    inherited Assign(Source);
end;

//////////////////////////////////////////// TCustomTagCloudStyler ///////////////////////////////////////////
constructor TCustomTagCloudStyler.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients:=TList.Create;
  FIgnoreDimensions:=False;
end;

destructor TCustomTagCloudStyler.Destroy;
begin
  FClients.Free;
  inherited;
end;

procedure TCustomTagCloudStyler.DoAfterPaint(Sender:TObject; TargetCanvas:TCanvas; Rect: TRect; FullRepaint: Boolean);
begin
  // virtual method for descendants - do nothing
end;

procedure TCustomTagCloudStyler.DoBeforePaint(Sender:TObject; TargetCanvas:TCanvas; Rect: TRect; FullRepaint: Boolean);
begin
  // virtual method for descendants - do nothing
end;

procedure TCustomTagCloudStyler.DoDrawItem(Sender:TObject; TargetCanvas:TCanvas; Item:TTagCloudItem; TextRect: TRect;
    var FrameRect: TRect; var ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw:Boolean);
begin
  // virtual method for descendants - do nothing
end;

function TCustomTagCloudStyler.GetHasAfterPaint:Boolean;
begin
  // virtual method for descendants
  Result:=False;
end;

function TCustomTagCloudStyler.GetHasBeforePaint:Boolean;
begin
  // virtual method for descendants
  Result:=False;
end;

function TCustomTagCloudStyler.GetHasDrawItem:Boolean;
begin
  // virtual method for descendants
  Result:=False;
end;

procedure TCustomTagCloudStyler.Apply(Sender: TCustomTagCloud);
begin
  // virtual method for descandants
  if Assigned(FOnStyleApplied) then
    FOnStyleApplied(Sender);
end;

procedure TCustomTagCloudStyler.ApplyStyle(Sender: TCustomTagCloud = nil);
var
  i:Integer;
begin
  if Sender<>nil then
  begin
    Sender.Items.BeginUpdate;
    try
      Apply(Sender);
    finally
      Sender.Items.EndUpdate;
    end;
  end
  else
  for i:=0 To FClients.Count-1 do
    ApplyStyle(FClients[i]);
end;

procedure TCustomTagCloudStyler.LoadFromTagCloud(Sender: TCustomTagCloud);
begin
  // virtual method for descandants
end;

//////////////////////////////////////////// TCustomTagCloud /////////////////////////////////////////////////
constructor TCustomTagCloud.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  aDesignTextInfo := 'Double-click or use the context menu "Edit items" option';
  ControlStyle := ControlStyle + [csReplicatable];
  FItems := TTagCloudItems.Create(Self);
  FLoadingOptions := TTagCloudLoadingOptions.Create;
  FItemFrame := TTagItemFrame.Create;
  FHoverFrame := TTagItemFrame.Create;
  FSelectedFrame := TTagItemFrame.Create;
  FItemFrame.FOnChanged := CheckRebuild;
  FHoverFrame.FOnChanged := CheckRebuild;
  FSelectedFrame.FOnChanged := CheckRebuild;
  FColors := TTagCloudColors.Create(Self);
  FCustomScale := TTagCloudCustomScale.Create(Self);
  {$IF CompilerVersion>=18}
  FPadding := TPadding.Create(Self);
  FPadding.OnChange := DoPaddingChange;
  {$IFEND}
  FAlignment := taCenter;
  FVerticalAlignment := taAlignTop;
  FHoverItem := nil;
  FSelectedItem := nil;
  {$IF CompilerVersion>=15}
  FHoverColor := clDefault;
  FSelectedColor := clDefault;
  {$ELSE}
  FHoverColor := clNone;
  FSelectedColor := clNone;
  {$IFEND}
  FHoverCursor := crHandPoint;
  FCmpCursor := Cursor;
  FHoverStyle := Font.Style+[fsUnderline];
  FHoverEnlarge := False;
  FHoverColSpace := False;
  FMaxFontSize := Font.Size*2+1;
  FColSpacing := 8;
  FPageCount := 1;
  FPageIndex := 0;
  FRowCount := 0;
  FVisibleRowCount := 0;
  FFirstDisplayIndex := 0;
  FLastDisplayIndex := -1;
  FRowSpacing := 4;
  FSorted := False;
  FState := [tcsNeedsValidate,tcsNeedsTextMetrics];
  FMinValue := MaxInt;
  FMaxValue := -MaxInt;
  FMinMaxDiff := 1;
  FLogScale := False;
  FLogDiff := FMinMaxDiff/10;
  FVisibleCount := 0;
  FDisplayCount := 0;
  FShrinkDiacritic := True;
  FFixedColFullFrame := False;
  FDirection := tcdHorizontal;
  SetLength(FTagFontsMetrics,FMaxFontSize+1);
  FAutoScaleFont := False;
  FAutoScaleFontCoef := 1;

  Width := 128;
  Height := 255;
  FDrawTextProc := DoDrawNormalText;
  {$IF CompilerVersion>=15}
  if {$IF CompilerVersion >= 23} StyleServices.Enabled {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
  begin
    ControlStyle := ControlStyle - [csOpaque];
    if (Win32MajorVersion >= 6) then
      FDrawTextProc := DoDrawThemeTextEx
  end
  else
  {$IFEND}
    ControlStyle := ControlStyle + [csOpaque];
end;

destructor TCustomTagCloud.Destroy;
begin
  Styler:=nil;
  FItems.Free;
  FColors.Free;
  FCustomScale.Free;
  FLoadingOptions.Free;
  FItemFrame.Free;
  FHoverFrame.Free;
  FSelectedFrame.Free;
  {$IF CompilerVersion>=18}
  FPadding.Free;
  {$IFEND}
  SetLength(FTagFontsMetrics,0);
  inherited;
end;

{$IF CompilerVersion>=18}
procedure TCustomTagCloud.DoPaddingChange(Sender: TObject);
begin
  Rebuild;
end;
{$IFEND}

procedure TCustomTagCloud.Resize;
begin
  Rebuild(False);
  inherited;
end;

function TCustomTagCloud.IsColorsStored: Boolean;
begin
  Result := FColors.Count > 0;
end;

function TCustomTagCloud.IsCustomScaleStored: Boolean;
begin
  Result := FCustomScale.Count > 0;
end;

function TCustomTagCloud.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TCustomTagCloud.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TCustomTagCloud.SetColors(const Value: TTagCloudColors);
begin
  FColors.Assign(Value);
  Rebuild;
end;

procedure TCustomTagCloud.SetCustomScale(const Value: TTagCloudCustomScale);
begin
  FCustomScale.Assign(Value);
  Rebuild;
end;

procedure TCustomTagCloud.SetFilter(const Value: string);
begin
  if Value <> FFilter then
  begin
    FFilter := Value;
    DoFilter;
  end;
end;

procedure TCustomTagCloud.SetFixedColCount(Value: Integer);
begin
  if Value <> FFixedColCount then
  begin
    if Value<0 then
      Value:=0;
    FFixedColCount := Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetFixedColWidth(const Value: Integer);
begin
  if Value <> FFixedColWidth then
  begin
    if FFixedColCount=0 then
    begin
      FFixedColWidth := Value;
      Include(FState,tcsNeedsValidate);
      Rebuild;
    end;
  end;
end;

procedure TCustomTagCloud.SetFixedColFullFrame(Value: Boolean);
begin
  if Value <> FFixedColFullFrame then
  begin
    FFixedColFullFrame := Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetGlowSize(const Value: Integer);
begin
  if Value <> FGlowSize then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

procedure TCustomTagCloud.SetItems(const Value: TTagCloudItems);
begin
  FItems.Assign(Value);
  Include(FState,tcsNeedsValidate);
  Rebuild;
end;

procedure TCustomTagCloud.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetVerticalAlignment(Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetAutoScaleFont(Value: Boolean);
begin
  if FAutoScaleFont <> Value then
  begin
    FAutoScaleFont := Value;
    FAutoScaleFontCoef := 1;
    Include(FState,tcsNeedsValidate);
    Include(FState,tcsNeedsTextMetrics);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetColSpacing(Value: Integer);
begin
  if FColSpacing <> Value then
  begin
    FColSpacing := Value;
    if FColSpacing<0 then
      FColSpacing:=0;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetDirection(Value: TTagCloudDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetLoadingOptions(Value: TTagCloudLoadingOptions);
begin
  FLoadingOptions.Assign(Value);
end;

procedure TCustomTagCloud.SetItemFrame(Value: TTagItemFrame);
var
  uf:Boolean;
begin
  uf:=FramesInUse;
  FItemFrame.Assign(Value);
  if uf<>FramesInUse then
  begin
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end
  else
    RebuildColors;
end;

procedure TCustomTagCloud.SetHoverFrame(Value: TTagItemFrame);
var
  uf:Boolean;
begin
  uf:=FramesInUse;
  FHoverFrame.Assign(Value);
  if uf<>FramesInUse then
  begin
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end
  else
    Invalidate;
end;

procedure TCustomTagCloud.SetSelectedFrame(Value: TTagItemFrame);
var
  uf:Boolean;
begin
  uf:=FramesInUse;
  FSelectedFrame.Assign(Value);
  if FSelectedFrame.FFrameMargin<>FItemFrame.FFrameMargin then
    FSelectedFrame.FFrameMargin:=FItemFrame.FFrameMargin;
  if uf<>FramesInUse then
  begin
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end
  else
    Invalidate;
end;

procedure TCustomTagCloud.SetLogScale(Value: Boolean);
begin
  if FLogScale <> Value then
  begin
    FLogScale := Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetRowSpacing(Value: Integer);
begin
  if FRowSpacing <> Value then
  begin
    FRowSpacing := Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetShrinkDiacritic(Value: boolean);
begin
  if FShrinkDiacritic <> Value then
  begin
    FShrinkDiacritic := Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetSorted(Value: Boolean);
begin
  if Value<>FSorted then
  begin
    FSorted := Value;
    if FSorted then
      Sort;
  end;
end;

procedure TCustomTagCloud.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
  FTransparentSet := True;
end;

procedure TCustomTagCloud.SetUseSpeedSort(Value: Boolean);
begin
  if FUseSpeedSort <> Value then
  begin
    FUseSpeedSort := Value;
    if FUseSpeedSort then
      FItems.FItemsRef := GetCollectionList(FItems)
    else
      FItems.FItemsRef := nil;
  end;
end;

{$IF CompilerVersion>=20}
procedure FillGlassRect(Canvas: TCanvas; Rect: TRect);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
  try
    FillRect(MemDC, Rect, Canvas.Brush.Handle);
    BufferedPaintMakeOpaque(PaintBuffer, Rect);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;
{$IFEND}

{$IF CompilerVersion>=20}
procedure TCustomTagCloud.DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
  Windows.DrawTextW(DC, Text, Length(Text), TextRect, TextFlags);
end;

procedure TCustomTagCloud.DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
var
  Options: TDTTOpts;
begin
  FillChar(Options, SizeOf(Options), 0);
  Options.dwSize := SizeOf(Options);
  Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE;
  if TextFlags and DT_CALCRECT = DT_CALCRECT then
    Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
  Options.crText := ColorToRGB(Canvas.Font.Color);
  Options.iGlowSize := FGlowSize;
  with StyleServices.GetElementDetails(teEditTextNormal) do
    DrawThemeTextEx(StyleServices.Theme[teEdit], DC, Part, State,
      Text, Length(Text), TextFlags, TextRect, Options);
end;

{$ELSE}
procedure TCustomTagCloud.DoDrawNormalText(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal);
begin
  Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;
{$IF CompilerVersion>=15}
procedure TCustomTagCloud.DoDrawThemeTextEx(DC: HDC; const Text: String; var TextRect: TRect; TextFlags: Cardinal);
begin
  Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;
{$IFEND}
{$IFEND}

function TCustomTagCloud.GetItemsRect:TRect;
begin
  Result:=ClientRect;
  {$IF CompilerVersion>=18}
  Inc(Result.Left,FPadding.Left);
  Inc(Result.Top,FPadding.Top);
  Dec(Result.Right,FPadding.Right);
  Dec(Result.Bottom,FPadding.Bottom);
  {$ELSE}
  InflateRect(Result,-4,-4);
  {$IFEND}

  if FHoverColSpace and (FColSpacing>0) then
    case FAlignment of
      taLeftJustify: Dec(Result.Right,FColSpacing);
      taRightJustify: Inc(Result.Left,FColSpacing);
    else
        if BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign] then
          Inc(Result.Left,FColSpacing)
        else
          Dec(Result.Right,FColSpacing);
    end;
end;

function TCustomTagCloud.GetHoveredSize(FontSize:Integer):Integer;
begin
  Result:=FontSize+round(Sqrt(FontSize)/2);
end;

procedure TCustomTagCloud.DoDrawItems(aCanvas:TCanvas; Rect: TRect);
var
  Flags: Cardinal;
  tmpRect: TRect;
  i: Integer;
  asDisabled,
  useCustomDraw,
  useAdvCustomDraw,
  useStyler: Boolean;
  cItem,hItem,sItem:TTagCloudItem;

  procedure DrawItem(Item:TTagCloudItem);
  var
    bc,fc:TColor;
    fw,rc:Integer;
    fs:TPenStyle;
    IFlags: Cardinal;
    IR, ITR, IRF:TRect;
    dd, bcd:Boolean;
  begin
    IFlags:=Flags;
    aCanvas.Font.Size:=Item.FSize;
    aCanvas.Font.Style:=Font.Style;
    IR:=Item.FRect;
    ITR:=Item.FRectTxt;
    IRF:=Item.FRectFrame;
    if asDisabled then
    begin
      bc:=clNone;
      fc:=clNone;
      fw:=0;
      fs:=psClear;
      rc:=0;
      aCanvas.Font.Color:=clGrayText;
    end
    else
    begin
      aCanvas.Font.Color:=Item.FColor;
      bc:=Item.FBackColor;
      fc:=Item.FFrameColor;
      fw:=FItemFrame.FFrameSize;
      rc:=FItemFrame.FRoundedSize;
      fs:=FItemFrame.FFrameStyle;
      if Item=FHoverItem then
      begin
        if (FHoverColor<>clNone) {$IF CompilerVersion>=15} and (FHoverColor<>clDefault) {$IFEND} then
          aCanvas.Font.Color:=FHoverColor;
        aCanvas.Font.Style:=FHoverStyle;
        if FHoverEnlarge then
          aCanvas.Font.Size:=GetHoveredSize(Item.FSize);
        if FHoverEnlarge or (fsBold in FHoverStyle) or (FHoverFrame.FFrameMargin>FItemFrame.FFrameMargin) then
        begin
          IR:=Item.FRectEn;
          ITR:=Item.FRectTxtEn;
          IRF:=Item.FRectFrameEn;
        end;
        if (FHoverFrame.FBackColor<>clNone) {$IF CompilerVersion>=15} and (FHoverFrame.FBackColor<>clDefault) {$IFEND} then
          bc:=FHoverFrame.FBackColor;
        fw:=FHoverFrame.FFrameSize;
        if fw>0 then
        begin
          if (FHoverFrame.FFrameColor<>clNone) {$IF CompilerVersion>=15} and (FHoverFrame.FFrameColor<>clDefault) {$IFEND} then
            fc:=FHoverFrame.FFrameColor;
          fs:=FHoverFrame.FFrameStyle;
        end;
        rc:=FHoverFrame.FRoundedSize;
      end
      else
      if (Item=FSelectedItem) or Item.Selected then
      begin
        if (FSelectedColor<>clNone) {$IF CompilerVersion>=15} and (FSelectedColor<>clDefault) {$IFEND} then
          aCanvas.Font.Color:=FSelectedColor;
        if (FSelectedFrame.FBackColor<>clNone) {$IF CompilerVersion>=15} and (FSelectedFrame.FBackColor<>clDefault) {$IFEND} then
          bc:=FSelectedFrame.FBackColor;
        fw:=FSelectedFrame.FFrameSize;
        if fw>0 then
        begin
          if (FSelectedFrame.FFrameColor<>clNone) {$IF CompilerVersion>=15} and (FSelectedFrame.FFrameColor<>clDefault) {$IFEND} then
            fc:=FSelectedFrame.FFrameColor;
          fs:=FSelectedFrame.FFrameStyle;
        end;
        rc:=FSelectedFrame.FRoundedSize;
      end;
    end;

    bcd:=(bc<>clNone) {$IF CompilerVersion>=15} and (bc<>clDefault) {$IFEND};
    if bcd then
    begin
      aCanvas.Brush.Color:=bc;
      aCanvas.Brush.Style:=bsSolid;
    end
    else
      aCanvas.Brush.Style:=bsClear;
    if fw>0 then
    begin
      aCanvas.Pen.Width:=fw;
      if (fc=clNone) {$IF CompilerVersion>=15} or (fc=clDefault) {$IFEND} then
        aCanvas.Pen.Color:=aCanvas.Font.Color
      else
        aCanvas.Pen.Color:=fc;
      aCanvas.Pen.Style:=fs;
    end
    else
      aCanvas.Pen.Style:=psClear;

    dd:=not useCustomDraw;
    if useAdvCustomDraw then
      FOnAdvCustomDrawItem(Self, aCanvas, Item, ITR, IRF, IR, IFlags, dd)
    else
    if useCustomDraw then
      FOnCustomDrawItem(Self, aCanvas, Item, ITR, IR, IFlags, dd);
    if dd then
    begin
      if useStyler then
      begin
        dd:=False;
        FStyler.DoDrawItem(Self, aCanvas, Item, ITR, IRF, IR, IFlags, dd);
      end;
      if dd then
      begin
        if bcd or (fw>0) then
        begin
          if rc>0 then
            RoundRect(aCanvas.Handle,IRF.Left,IRF.Top,IRF.Right,IRF.Bottom,rc,rc)
          else
          if fw>0 then
            Rectangle(aCanvas.Handle,IRF.Left,IRF.Top,IRF.Right,IRF.Bottom)
          else
            FillRect(aCanvas.Handle,IRF,aCanvas.Brush.Handle);
        end;
        aCanvas.Brush.Style:=bsClear;
        FDrawTextProc(aCanvas.Handle, Item.Caption, IR, IFlags);
      end;
    end;
  end;

begin
  if FItems.UpdateCount<=0 then
  begin
    Flags := DT_NOPREFIX or DT_SINGLELINE or DT_BOTTOM or DT_NOCLIP;
    if FFixedColWidth>0 then
      Flags := DT_LEFT or DT_END_ELLIPSIS or Flags
    else
      Flags := DT_CENTER or Flags;
    Flags := DrawTextBiDiModeFlags(Flags);
    aCanvas.Font := Font;

    if FItems.Count=0 then
    begin
      if (csDesigning in ComponentState) then
      begin
        {$IF CompilerVersion>=20}
        tmpRect:=GetItemsRect;
        inflateRect(tmpRect,-10,-10);
        aCanvas.TextRect(tmpRect,aDesignTextInfo,[tfLeft,tfTop,tfWordBreak]);
        {$ELSE}
        aCanvas.TextRect(GetItemsRect,10,10,aDesignTextInfo);
        {$IFEND}
      end;
    end
    else
    begin
      asDisabled:=not Enabled;
      useCustomDraw:=Assigned(FOnCustomDrawItem);
      useAdvCustomDraw:=Assigned(FOnAdvCustomDrawItem);
      useStyler:=Assigned(FStyler) and FStyler.HasDrawItem;
      hItem:=nil;
      sItem:=nil;

      for i:=FFirstDisplayIndex To FItems.Count-1 do
      begin
        cItem:=FItems[i];
        if cItem.FVisible and (cItem.FPageIndex=FPageIndex) and IntersectRect(tmpRect,Rect,cItem.FRectFrameEn) then
        begin
          if cItem=FHoverItem then
            hItem:=cItem
          else
          if cItem=FSelectedItem then
            sItem:=cItem
          else
            DrawItem(cItem);
        end
        else
        if cItem.FPageIndex>FPageIndex then
          break;
      end;
      if sItem<>nil then
        DrawItem(sItem);
      if hItem<>nil then
        DrawItem(hItem);
    end;
  end;
end;

procedure TCustomTagCloud.Paint;
var
  Rect, BR : TRect;
  Buffer : TBitmap;
  tmpCanvas : TCanvas;
  FullR: Boolean;
begin
  Rect := Canvas.ClipRect;
  BR := ClientRect;
  FullR := SameRect(Rect, BR);
  if Parent.DoubleBuffered or Transparent then
  begin
    if Assigned(FOnBeforePaint) then
      FOnBeforePaint(Self, Canvas, Rect, FullR)
    else
    if Assigned(FStyler) and FStyler.HasBeforePaint then
      FStyler.DoBeforePaint(Self, Canvas, Rect, FullR)
    else
    if not Transparent then
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Brush.Style := bsSolid;
      {$IF CompilerVersion>=20}
      if csGlassPaint in ControlState then
        FillGlassRect(Canvas, Rect)
      else
      {$IFEND}
        Canvas.FillRect(Rect);
    end;
    Canvas.Brush.Style := bsClear;
    DoDrawItems(Canvas, Rect);
    if Assigned(FStyler) and FStyler.HasAfterPaint  then
      FStyler.DoAfterPaint(Self, Canvas, Rect, FullR);
    if Assigned(FOnAfterPaint) then
      FOnAfterPaint(Self, Canvas, Rect, FullR);
  end
  else
  begin
    Buffer := TBitmap.Create;
    try
      Buffer.Width := BR.Right-BR.Left;
      Buffer.Height := BR.Bottom-BR.Top;
      tmpCanvas := Buffer.Canvas;
      if Assigned(FOnBeforePaint) then
        FOnBeforePaint(Self, tmpCanvas, Rect, FullR)
      else
      if Assigned(FStyler) and FStyler.HasBeforePaint then
        FStyler.DoBeforePaint(Self, tmpCanvas, Rect, FullR)
      else
      begin
        tmpCanvas.Brush.Color := Self.Color;
        tmpCanvas.Brush.Style := bsSolid;
        {$IF CompilerVersion>=20}
        if csGlassPaint in ControlState then
          FillGlassRect(tmpCanvas, Rect)
        else
        {$IFEND}
          tmpCanvas.FillRect(Rect);
      end;
      tmpCanvas.Brush.Style := bsClear;
      DoDrawItems(tmpCanvas,Rect);
      if Assigned(FStyler) and FStyler.HasAfterPaint then
        FStyler.DoAfterPaint(Self, tmpCanvas, Rect, FullR);
      if Assigned(FOnAfterPaint) then
        FOnAfterPaint(Self, tmpCanvas, Rect, FullR);
      Canvas.CopyRect(Rect, tmpCanvas, Rect);
    finally
      Buffer.Free;
    end;
  end;
end;

procedure TCustomTagCloud._RebuildItemColors(Item:TTagCloudItem; CustomScaleItem:TTagCustomScaleItem = nil);
var
  clridx:Integer;
  clItem:TTagColorItem;
begin
  if CustomScaleItem<>nil then
  begin
    Item.FColor:=CustomScaleItem.FColor;
    Item.FBackColor:=CustomScaleItem.FBackColor;
    Item.FFrameColor:=CustomScaleItem.FFrameColor;
  end
  else
  begin
    if FColors.Count=0 then
    begin
      Item.FColor:=Font.Color;
      Item.FBackColor:=FItemFrame.FBackColor;
      Item.FFrameColor:=FItemFrame.FrameColor;
    end
    else
    begin
      if FLogScale then
        clridx:=round(FColors.Count*(ln((Item.FValue-FMinValue)/FLogDiff+1)/ln(FMinMaxDiff/FLogDiff+1)))
      else
        clridx:=round((FColors.Count*(Item.FValue-FMinValue))/FMinMaxDiff);

      if clridx<0 then
        clItem:=FColors[0]
      else
      if clridx>=FColors.Count then
        clItem:=FColors[FColors.Count-1]
      else
        clItem:=FColors[clridx];

      Item.FColor:=clItem.FColor;
      Item.FBackColor:=clItem.FBackColor;
      Item.FFrameColor:=clItem.FFrameColor;
    end;
  end;
  if (Item.FColor=clNone) {$IF CompilerVersion>=15} or (Item.FColor=clDefault) {$IFEND} then
    Item.FColor:=Font.Color;
  if (Item.FBackColor=clNone) {$IF CompilerVersion>=15} or (Item.FBackColor=clDefault) {$IFEND} then
    Item.FBackColor:=FItemFrame.FBackColor;
  if (Item.FFrameColor=clNone) {$IF CompilerVersion>=15} or (Item.FFrameColor=clDefault) {$IFEND} then
    Item.FFrameColor:=FItemFrame.FFrameColor;

  if Assigned(FOnTagColor) then
    FOnTagColor(Self,Item,Item.FColor);
end;

procedure TCustomTagCloud.RebuildColors(anItem:TTagCloudItem = nil);
var
  i:Integer;
  useCustomScale:Boolean;
  csItem:TTagCustomScaleItem;
  R:TRect;
begin
  if (FItems.UpdateCount<=0) and (not (csLoading in ComponentState)) then
  begin
    useCustomScale:=FCustomScale.Count>0;
    if anItem=nil then
    begin
      for i:=0 To FItems.Count-1 do
      begin
        if useCustomScale and FCustomScale.Find(FItems[i].FValue,csItem) then
          _RebuildItemColors(FItems[i],csItem)
        else
          _RebuildItemColors(FItems[i],nil);
      end;
      Invalidate;
    end
    else
    begin
      if useCustomScale and FCustomScale.Find(anItem.FValue,csItem) then
        _RebuildItemColors(anItem,csItem)
      else
        _RebuildItemColors(anItem,nil);
      if (Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
        Invalidate
      else
      if (not (csDestroying in ComponentState)) and Assigned(Parent) then
      begin
        if FHoverColSpace and (FColSpacing>0) then
        begin
          R:=anItem.FRectFrameEn;
          ValidateItemHoverColSpaceRect(anItem, R);
          CheckInvalidateRect(R);
        end
        else
          CheckInvalidateRect(anItem.FRectFrameEn);
      end;
    end;
  end;
end;

function TCustomTagCloud.Rebuild(DoRedraw:Boolean=True):Integer;
var
  i,p,n,rowStart:Integer;
  R:TRect;
  x,y:Integer;
  maxsize,hmaxsize,minsize:Integer;
  minval,maxval:Int64;
  fsizediff:Integer;
  rowsize:Integer;
  Item:TTagCloudItem;
  pcount:Integer;
  pch:Boolean;
  CalcHRect:Boolean;
  initpidx:Integer;
  validateAll:Boolean;
  useFixedWidth,
  useCustomScale,
  useFrames:Boolean;
  fwSpace:Integer;
  csItem:TTagCustomScaleItem;
  bcm,hbcm,bcr,bcre,lastx:Integer;
  hoverCanvas:TCanvas;
  bdrtlFW:Boolean;
  breakKind:TTagItemBreakKind;
  brX,brY:Integer;
  TM:TTextMetric;
  asf:Boolean;

  procedure _OffsetItem(rItem:TTagCloudItem; ox,oy: Integer);
  begin
    OffsetRect(rItem.FRectFrame,ox,oy); OffsetRect(rItem.FRectFrameEn,ox,oy);
    OffsetRect(rItem.FRect,ox,oy); OffsetRect(rItem.FRectEn,ox,oy);
    OffsetRect(rItem.FRectTxt,ox,oy); OffsetRect(rItem.FRectTxtEn,ox,oy);
  end;

  procedure _SwitchItemRTL(rItem:TTagCloudItem);
  var
    rP,hrP,fP,tP:Integer;
  begin
    fP:=rItem.FRectEn.Right-rItem.FRectFrameEn.Right;
    tP:=rItem.FRectEn.Right-rItem.FRectFrame.Right;
    rP:=-(rItem.FRect.Left-rItem.FRectFrame.Left);
    hrP:=-(rItem.FRect.Left-rItem.FRectFrameEn.Left);
    if rP<>0 then
      OffsetRect(rItem.FRect,rP,0);
    if hrP<>0 then
      OffsetRect(rItem.FRectEn,hrP,0);
    if fP<>0 then
    begin
      if (rItem.FTextSize.cy-rItem.FSize) div 2<=1 then
        OffsetRect(rItem.FRectFrameEn,fP-1,0)
      else
        OffsetRect(rItem.FRectFrameEn,fP,0);
      OffsetRect(rItem.FRectTxtEn,fP,0);
    end;
    if tP<>0 then
    begin
      if (rItem.FTextSize.cy-rItem.FSize) div 2<=1 then
        OffsetRect(rItem.FRectFrame,tP-1,0)
      else
        OffsetRect(rItem.FRectFrame,tP,0);
      OffsetRect(rItem.FRectTxt,tP,0);
    end;
  end;

  procedure _cropItem(rItem:TTagCloudItem; rX: Integer);
  begin
    if rItem.FRectFrameEn.Right>rX then
    begin
      Dec(rItem.FRectTxtEn.Right,rItem.FRectFrameEn.Right-rX);
      rItem.FRectFrameEn.Right:=rX;
    end;
    if rItem.FRectFrame.Right>rX then
    begin
      Dec(rItem.FRectTxt.Right,rItem.FRectFrame.Right-rX);
      rItem.FRectFrame.Right:=rX;
    end;
    if rItem.FRectEn.Right>rX then
      rItem.FRectEn.Right:=rX;
    if rItem.FRect.Right>rX then
      rItem.FRect.Right:=rX;
  end;

  procedure _AlignLastRow(iStart, iEnd, aSpace:integer);
  var
    j:Integer;
    jItem:TTagCloudItem;
    maxY,maxF,maxD:Integer;
  begin
    if FAlignment in [taRightJustify,taCenter] then
    begin
      if FAlignment=taCenter then
      begin
        if useFixedWidth then aSpace:=fwSpace else aSpace:=aSpace div 2;
        if useFrames then
          for j:=iStart To iEnd do
            if FItems[j].Visible then
            begin
              if FItems[j].FRectFrameEn.Left+aSpace<=R.Left then
                Inc(aSpace,2);
              break;
            end;
      end;
    end
    else
      aSpace:=0;

    if FAutoShrinkRows then
    begin
      maxY:=0;
      maxF:=0;
      maxD:=0;
      for j:=iStart To iEnd do
      begin
        jItem:=FItems[j];
        if jItem.FVisible then
        begin
          if jItem.FRect.Bottom>maxY then
            maxY:=jItem.FRect.Bottom;
          if (jItem.FRectTxt.Top-jItem.FRectFrameEn.Top)>maxF then
            maxF:=(jItem.FRectTxt.Top-jItem.FRectFrameEn.Top);
          if FShrinkDiacritic and ((jItem.FRectTxt.Top-jItem.FRectEn.Top)>maxD) then
            maxD:=(jItem.FRectTxt.Top-jItem.FRectEn.Top);
        end;
      end;

      Inc(maxY,maxF);
      maxF:=0;
      for j:=iStart To iEnd do
      begin
        jItem:=FItems[j];
        if jItem.FVisible then
        begin
          jItem.FRowIndex:=FRowCount;
          _OffsetItem(jItem, aSpace, (maxY-jItem.FRect.Bottom - (jItem.FRect.Top-y) - maxD));
          if jItem.FRectFrame.Bottom>maxF then
            maxF:=jItem.FRectFrame.Bottom;
          if bdrtlFW then
            _SwitchItemRTL(jItem);
        end
        else
          jItem.FRowIndex:=-1;
      end;
      y:=maxF - rowsize + FRowSpacing;
    end
    else
    if FAlignment in [taRightJustify,taCenter] then
    begin
      for j:=iStart To iEnd do
      begin
        jItem:=FItems[j];
        if jItem.FVisible then
        begin
          jItem.FRowIndex:=FRowCount;
          _OffsetItem(jItem,aSpace,0);
          if bdrtlFW then
            _SwitchItemRTL(jItem);
        end
        else
          jItem.FRowIndex:=-1;
      end;
    end
    else
      for j:=iStart To iEnd do
      begin
        jItem:=FItems[j];
        if jItem.FVisible then
          jItem.FRowIndex:=FRowCount
        else
          jItem.FRowIndex:=-1;
      end;
    Inc(FRowCount);
  end;

  procedure _checkLastDispIdx;
  begin
    if (FLastDisplayIndex=-1) and (FFirstDisplayIndex>=0) then
    begin
      FLastDisplayIndex:=i-1;
      while FLastDisplayIndex>FFirstDisplayIndex do
      begin
        if FItems[FLastDisplayIndex].FVisible then
          break;
        Dec(FLastDisplayIndex);
      end;
    end;
  end;

begin
  if (Parent=nil) or (FItems.UpdateCount>0) or (csLoading in ComponentState) then
  begin
    Result:=-1; // parent is not assigned - no calculation performed
    Exit
  end
  else
    Result:=0;

  include(FState,tcsRebuilding);
  pch:=False;
  asf:=False;
  hoverCanvas:=TCanvas.Create;
  try
    hoverCanvas.Handle := GetDC(0);
    FVisibleCount:=0;
    if AutoSize then
      initpidx:=0
    else
      initpidx:=-1;

    // check minimal and maximal values
    minval:=MaxInt;
    maxval:=-MaxInt;
    for i:=0 To FItems.Count-1 do
    begin
      Item:=FItems[i];
      if Item.FPageIndex<>initpidx then
        Item.FPageIndex:=initpidx;
      if Item.FValue>maxval then
        maxval:=Item.FValue;
      if Item.FValue<minval then
        minval:=Item.FValue;
    end;
    if (minval<>FMinValue) or (maxval<>FMaxValue) then
    begin
      FMinValue:=minval;
      FMaxValue:=maxval;
      validateAll:=True;
    end
    else
      validateAll:=tcsNeedsValidate in FState;

    FMinMaxDiff:=FMaxValue-FMinValue;
    if FMinMaxDiff=0 then
      FMinMaxDiff:=1;
    FLogDiff:=FMinMaxDiff/10;

    // check minimal and maximal font sizes (with CustomScale in mind)
    minsize:=Font.Size;
    maxsize:=FMaxFontSize;
    useCustomScale:=FCustomScale.Count>0;
    if useCustomScale then
      for i:=0 To FCustomScale.Count-1 do
      begin
        if FCustomScale.Items[i].FFontSize<minsize then
          minsize:=FCustomScale.Items[i].FFontSize
        else
        if FCustomScale.Items[i].FFontSize>maxsize then
          maxsize:=FCustomScale.Items[i].FFontSize;
      end;
    if maxsize<minsize then
      maxsize:=minsize;

    if FAutoScaleFont and (not AutoSize) and (FFixedColCount+FFixedColWidth=0) and (abs(FAutoScaleFontCoef)<>1) then
    begin
      minsize:=round(minsize*abs(FAutoScaleFontCoef));
      maxsize:=round(maxsize*abs(FAutoScaleFontCoef));
    end;

    fsizediff:=maxsize-minsize;

    Canvas.Font := Font;

    // get text metrics for each possible font size (only when needed)
    if FHoverEnlarge then
      hmaxsize:=GetHoveredSize(maxsize)
    else
      hmaxsize:=maxsize;
    if (hmaxsize+1<>Length(FTagFontsMetrics)) or (tcsNeedsTextMetrics in FState) then
    begin
      SetLength(FTagFontsMetrics,hmaxsize+1);
      for i:=minsize To hmaxsize do
      begin
        Canvas.Font.Size:=i;
        GetTextMetrics(Canvas.Handle,TM);
        FTagFontsMetrics[i].Height:=TM.tmHeight;
        FTagFontsMetrics[i].Ascent:=TM.tmAscent;
        FTagFontsMetrics[i].Descent:=TM.tmDescent;
        FTagFontsMetrics[i].InternalLeading:=TM.tmInternalLeading;
      end;
      Exclude(FState,tcsNeedsTextMetrics);
    end;

    // calculate row size (and correct row spacing if it is too negative)
    if FTagFontsMetrics[maxsize].Height+FRowSpacing<0 then
      FRowSpacing:=-FTagFontsMetrics[maxsize].Height+1;
    rowsize:=FTagFontsMetrics[hmaxsize].Height+FRowSpacing-Integer(FShrinkDiacritic)*FTagFontsMetrics[hmaxsize].InternalLeading;
    if rowsize<0 then
      rowsize:=0;

    useFrames:=FramesInUse;
    if FHoverFrame.FFrameMargin>FItemFrame.FFrameMargin then
      Inc(rowsize,(FHoverFrame.FFrameMargin+Integer(useFrames))*2)
    else
    if FItemFrame.FFrameMargin>0 then
      Inc(rowsize,(FItemFrame.FFrameMargin+Integer(useFrames))*2)
    else
      Inc(rowsize,Integer(useFrames)*2);

    R:=GetItemsRect;

    if useFrames and (FAlignment=taCenter) then
      Dec(R.Right,2);

    x:=R.Left;
    y:=R.Top;
    pcount:=1;
    FFirstDisplayIndex:=-1;
    FLastDisplayIndex:=-1;
    FRowCount:=0;
    FVisibleRowCount:=0;

    if FFixedColCount>0 then
    begin
      i := FFixedColWidth;
      FFixedColWidth := ((R.Right - R.Left) - FColSpacing * (FFixedColCount-1)) div FFixedColCount;
      if FFixedColWidth <= 0 then
        FFixedColWidth := 1;
      if (FFixedColWidth <> i) and (not validateall) then
        validateall := True;
    end;
    useFixedWidth:=FFixedColWidth>0;

    if (minsize*1.375<=R.Bottom-R.Top) or AutoSize then
    begin
      bdrtlFW:=useFixedWidth and (BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign]);
      if useFixedWidth then
      begin
        if FFixedColWidth+FColSpacing<>0 then
          fwSpace:=(R.Right-((R.Right-R.Left+FColSpacing) div (FFixedColWidth+FColSpacing))*(FFixedColWidth+FColSpacing)) div 2
        else
          fwSpace:=0;
      end
      else
        fwSpace:=0;
      CalcHRect:=FHoverEnlarge or ((fsBold in FHoverStyle) and (not (fsBold in Font.Style)));
      if CalcHRect then
      begin
        hoverCanvas.Font := Font;
        hoverCanvas.Font.Style:=FHoverStyle;
      end;
      rowStart:=0;
      i:=0;
      while i<FItems.Count do
      begin
        Item:=FItems[i];
        if Item.FVisible then
        begin
          Inc(FVisibleCount);
          if validateAll or Item.FNeedsValidate then
          begin
            // calculate item size and assign a color
            if useCustomScale and FCustomScale.Find(Item.FValue,csItem) then
            begin
              Item.FSize:=csItem.FFontSize;
              _RebuildItemColors(Item,csItem);
            end
            else
            begin
              if FLogScale then
                Item.FSize:=round(minsize+fsizediff*(ln((Item.FValue-FMinValue)/FLogDiff+1)/ln(FMinMaxDiff/FLogDiff+1)))
              else
                Item.FSize:=round(minsize+(fsizediff*(Item.FValue-FMinValue))/FMinMaxDiff);
              _RebuildItemColors(Item,nil);
            end;

            // calculate item dimensions
            Canvas.Font.Size:=Item.FSize;
            if useFixedWidth and (FFixedColFullFrame or (not useFrames)) then
            begin
              Item.FTextSize.cx:=FFixedColWidth;
              Item.FTextSize.cy:=FTagFontsMetrics[Item.FSize].Height;
            end
            else
              {$IF CompilerVersion>=20}
              GetTextExtentPoint32(Canvas.Handle, Item.FCaption, Length(Item.FCaption), Item.FTextSize);
              {$ELSE}
              GetTextExtentPoint32(Canvas.Handle, PChar(Item.FCaption), Length(Item.FCaption), Item.FTextSize);
              {$IFEND}
            if CalcHRect then
            begin
              if FHoverEnlarge then
                hoverCanvas.Font.Size:=GetHoveredSize(Item.FSize)
              else
                hoverCanvas.Font.Size:=Item.FSize;
              if useFixedWidth and (FFixedColFullFrame or (not useFrames)) then
              begin
                Item.FEnSize.cx:=Item.FTextSize.cx;
                Item.FEnSize.cy:=FTagFontsMetrics[hoverCanvas.Font.Size].Height;
              end
              else
                {$IF CompilerVersion>=20}
                GetTextExtentPoint32(hoverCanvas.Handle, Item.FCaption, Length(Item.FCaption), Item.FEnSize);
                {$ELSE}
                GetTextExtentPoint32(hoverCanvas.Handle, PChar(Item.FCaption), Length(Item.FCaption), Item.FEnSize);
                {$IFEND}
            end
            else
              Item.FEnSize:=Item.FTextSize;
            Item.FNeedsValidate:=False;
          end;

          if useFixedWidth and (FDirection=tcdVertical) then
          begin
            // fixed-width tag cloud with vertical direction
            if Assigned(FOnTagPositioning) then
            begin
              if breakKind<>tibNone then breakKind:=tibNone;
              brX:=x; brY:=y;
              FOnTagPositioning(Self,Item,Item.FTextSize.cx,Item.FTextSize.cy,R,brX,brY,breakKind);
              brX:=brX-x; brY:=brY-y;
              case breakKind of
                tibPageBreak:
                  if (x<>R.Left) and (y<>R.Top) and (not AutoSize) then
                  begin
                    Inc(pcount);
                    _checkLastDispIdx;
                    x:=R.Left;
                    y:=R.Top;
                  end;
                tibColRowBreak:
                  if y<>R.Top then
                  begin
                    Inc(x,FFixedColWidth+FColSpacing);
                    y:=R.Top;
                  end;
                tibSpacer:
                  if y<>R.Top then
                    y:=y+Item.FTextSize.cy;
              end;
              Inc(x,brX);
              Inc(y,brY);
            end;

            if (not AutoSize) and (x + FFixedColWidth > R.Right) then
            begin
              Inc(pcount);
              _checkLastDispIdx;
              x:=R.Left;
              y:=R.Top;
            end;

            Item.FPageIndex:=pcount-1;
            if (FFirstDisplayIndex=-1) and (Item.FPageIndex=FPageIndex) then
              FFirstDisplayIndex:=i;

            SetRect(Item.FRect, x, y, x + FFixedColWidth, y + Item.FTextSize.cy );
            Item.FRectEn:=Item.FRect;
            // calculate text rectangles of the item

            if (not FFixedColFullFrame) or useFrames then
            begin
              bcr:=Item.FRect.Left+Item.FTextSize.cx;
              bcre:=Item.FRectEn.Left+Item.FEnSize.cx;
            end
            else
            begin
              bcr:=Item.FRect.Right;
              bcre:=Item.FRectEn.Right;
            end;

            SetRect(Item.FRectTxt, Item.FRect.Left, Item.FRect.Top, bcr, Item.FRect.Top+Item.FTextSize.cy);
            SetRect(Item.FRectTxtEn, Item.FRectEn.Left, Item.FRectEn.Top-(Item.FEnSize.cy-Item.FTextSize.cy), bcre, Item.FRectEn.Top+Item.FTextSize.cy);

            // calculate frame/background rectangles of the item
            Item.FRectFrame:=Item.FRectTxt;
            if FShrinkDiacritic then
            begin
              Inc(Item.FRectTxt.Top,FTagFontsMetrics[Item.FSize].InternalLeading);
              Inc(Item.FRectFrame.Top,FTagFontsMetrics[Item.FSize].InternalLeading);
            end;
            if useFrames then
              InflateRect(Item.FRectFrame, FItemFrame.FFrameMargin*2+1, FItemFrame.FFrameMargin+1)
            else
              InflateRect(Item.FRectFrame, FItemFrame.FFrameMargin*2, FItemFrame.FFrameMargin);

            Item.FRectFrameEn:=Item.FRectTxtEn;
            if FShrinkDiacritic then
            begin
              if CalcHRect and FHoverEnlarge then
                hbcm:=GetHoveredSize(Item.FSize)
              else
                hbcm:=Item.FSize;
              Inc(Item.FRectTxtEn.Top,FTagFontsMetrics[hbcm].InternalLeading);
              Inc(Item.FRectFrameEn.Top,FTagFontsMetrics[hbcm].InternalLeading);
            end;
            if useFrames then
              InflateRect(Item.FRectFrameEn, FHoverFrame.FFrameMargin*2+1, FHoverFrame.FFrameMargin+1)
            else
              InflateRect(Item.FRectFrameEn, FHoverFrame.FFrameMargin*2, FHoverFrame.FFrameMargin);
            UnionRect(Item.FRectFrameEn,Item.FRectFrame,Item.FRectFrameEn);

            if useFrames then
            begin
              if Item.FRectFrameEn.Left<x then
                _OffsetItem(Item,x-Item.FRectFrameEn.Left,0);
            end;
            _cropItem(Item,x+FFixedColWidth);
            if Item.FRectFrameEn.Top<y then
              _OffsetItem(Item,0,y-Item.FRectFrameEn.Top)
            else
            if Item.FRectFrame.Top<y then
              _OffsetItem(Item,0,y-Item.FRectFrame.Top);

            if Item.FRectFrameEn.Bottom>=R.Bottom then
            begin
              if (x<>R.Left) or (y<>R.Top) then
                Dec(i);
              y:=R.Top;
              Inc(x,FFixedColWidth+FColSpacing);
            end
            else
            begin
              if bdrtlFW then
                _SwitchItemRTL(Item);
              y := Item.FRectFrameEn.Bottom+FRowSpacing;
              if y>=R.Bottom then
              begin
                y:=R.Top;
                Inc(x,FFixedColWidth+FColSpacing);
              end;
            end;

          end
          else
          begin
            // horizontal direction

            if Assigned(FOnTagPositioning) then
            begin
              if breakKind<>tibNone then breakKind:=tibNone;
              brX:=x; brY:=y;
              FOnTagPositioning(Self,Item,Item.FTextSize.cx,Item.FTextSize.cy,R,brX,brY,breakKind);
              brX:=brX-x; brY:=brY-y;
              case breakKind of
                tibPageBreak:
                  if (x<>R.Left) and (y<>R.Top) and (not AutoSize) then
                  begin
                    if i>rowstart then
                      _AlignLastRow(rowStart,i-1,R.Right-x+FColSpacing);
                    Inc(pcount);
                    _checkLastDispIdx;
                    rowstart:=i;
                    x:=R.Left;
                    y:=R.Top;
                  end;
                tibColRowBreak:
                  if i<>rowstart then
                  begin
                    _AlignLastRow(rowStart,i-1,R.Right-x+FColSpacing);
                    Inc(y,rowsize);
                    x:=R.Left;
                    rowStart:=i;
                  end;
                tibSpacer:
                  if i<>rowstart then
                    x:=x+Item.FTextSize.cy*2;
              end;
              Inc(x,brX);
              Inc(y,brY);
            end;

            if (i>rowstart) and ((useFixedWidth and (x + FFixedColWidth > R.Right)) or ((not useFixedWidth) and (x + Item.FEnSize.cx > R.Right))) then
            begin
              _AlignLastRow(rowStart,i-1,R.Right-x+FColSpacing);
              Inc(y,rowsize);
              x:=R.Left;
              rowStart:=i;
            end;

            lastx:=x;
            if useFixedWidth then bcr:=FFixedColWidth else bcr:=Item.FTextSize.cx;
            if FAutoShrinkRows then
              SetRect(Item.FRect, x,       y +                     (FTagFontsMetrics[maxsize].Descent-FTagFontsMetrics[Item.FSize].Descent),
                                  x + bcr, y + Item.FTextSize.cy + (FTagFontsMetrics[maxsize].Descent-FTagFontsMetrics[Item.FSize].Descent))
            else
              SetRect(Item.FRect, x,       y,
                                  x + bcr, y + rowsize - (FTagFontsMetrics[maxsize].Descent-FTagFontsMetrics[Item.FSize].Descent));

            if CalcHRect and FHoverEnlarge then hbcm:=GetHoveredSize(Item.FSize) else hbcm:=Item.FSize;
            bcm:=Item.FRect.Bottom+Integer(FAutoShrinkRows)*FTagFontsMetrics[hbcm].Descent+Integer(useFrames);
            if FHoverFrame.FFrameMargin>FItemFrame.FFrameMargin then
              Inc(bcm,FHoverFrame.FFrameMargin+(FHoverFrame.FFrameMargin-FItemFrame.FFrameMargin))
            else
              Inc(bcm,FItemFrame.FFrameMargin);
            if AutoSize or (bcm<=R.Bottom) then
            begin
              Item.FPageIndex:=pcount-1;
              if (FFirstDisplayIndex=-1) and (Item.FPageIndex=FPageIndex) then
                FFirstDisplayIndex:=i;

              // calculate hovered item rectangle
              Item.FRectEn:=Item.FRect;
              if not useFixedWidth then
              begin
                if Item.FEnSize.cx>Item.FTextSize.cx then
                begin
                  Inc(Item.FRectEn.Right,Item.FEnSize.cx-Item.FTextSize.cx);
                  OffsetRect(Item.FRect,(Item.FEnSize.cx-Item.FTextSize.cx) div 2,0);
                end
                else
                if Item.FEnSize.cx<Item.FTextSize.cx then
                  OffsetRect(Item.FRectEn,(Item.FTextSize.cx-Item.FEnSize.cx) div 2,0);
              end;
              // calculate text rectangles of the item
              if useFixedWidth and ((not FFixedColFullFrame) or useFrames) then
              begin
                bcr:=Item.FRect.Left+Item.FTextSize.cx;
                bcre:=Item.FRectEn.Left+Item.FEnSize.cx;
              end
              else
              begin
                bcr:=Item.FRect.Right;
                bcre:=Item.FRectEn.Right;
              end;

              if FAutoShrinkRows then
              begin
                SetRect(Item.FRectTxt, Item.FRect.Left, Item.FRect.Top, bcr, Item.FRect.Top+Item.FTextSize.cy-FTagFontsMetrics[Item.FSize].Descent);
                SetRect(Item.FRectTxtEn, Item.FRectEn.Left, Item.FRectEn.Top-(Item.FEnSize.cy-Item.FTextSize.cy), bcre, Item.FRectEn.Top+Item.FTextSize.cy-FTagFontsMetrics[hbcm].Descent);
              end
              else
              begin
                SetRect(Item.FRectTxt, Item.FRect.Left, Item.FRect.Bottom-Item.FTextSize.cy, bcr, Item.FRect.Bottom-FTagFontsMetrics[Item.FSize].Descent);
                SetRect(Item.FRectTxtEn, Item.FRectEn.Left, Item.FRectEn.Bottom-Item.FEnSize.cy, bcre, Item.FRectEn.Bottom-FTagFontsMetrics[hbcm].Descent);
              end;

              // calculate frame/background rectangles of the item
              SetRect(Item.FRectFrame, Item.FRectTxt.Left,  Item.FRectTxt.Bottom-FTagFontsMetrics[Item.FSize].Ascent,
                                       Item.FRectTxt.Right, Item.FRectTxt.Bottom+FTagFontsMetrics[Item.FSize].Descent);
              if FShrinkDiacritic then
              begin
                Inc(Item.FRectTxt.Top,FTagFontsMetrics[Item.FSize].InternalLeading);
                Inc(Item.FRectFrame.Top,FTagFontsMetrics[Item.FSize].InternalLeading);
              end;
              if useFrames then
                InflateRect(Item.FRectFrame, FItemFrame.FFrameMargin*2+1, FItemFrame.FFrameMargin+1)
              else
                InflateRect(Item.FRectFrame, FItemFrame.FFrameMargin*2, FItemFrame.FFrameMargin);

              SetRect(Item.FRectFrameEn, Item.FRectTxtEn.Left,  Item.FRectTxtEn.Bottom-FTagFontsMetrics[hbcm].Ascent,
                                         Item.FRectTxtEn.Right, Item.FRectTxtEn.Bottom+FTagFontsMetrics[hbcm].Descent);
              if FShrinkDiacritic then
              begin
                Inc(Item.FRectTxtEn.Top,FTagFontsMetrics[hbcm].InternalLeading);
                Inc(Item.FRectFrameEn.Top,FTagFontsMetrics[hbcm].InternalLeading);
              end;
              if useFrames then
                InflateRect(Item.FRectFrameEn, FHoverFrame.FFrameMargin*2+1, FHoverFrame.FFrameMargin+1)
              else
                InflateRect(Item.FRectFrameEn, FHoverFrame.FFrameMargin*2, FHoverFrame.FFrameMargin);
              UnionRect(Item.FRectFrameEn,Item.FRectFrame,Item.FRectFrameEn);

              if useFrames then
              begin
                if Item.FRectFrameEn.Left<x then
                  _OffsetItem(Item,x-Item.FRectFrameEn.Left,0);
                if useFixedWidth then
                begin
                  _cropItem(Item,x+FFixedColWidth);
                  Inc(x,FFixedColWidth+FColSpacing)
                end
                else
                begin
                  if CalcHRect and (i>rowstart) then
                  begin
                    p:=i-1;
                    while p>=rowstart do
                      if FItems[p].FVisible then break else Dec(p);
                    if p>=rowstart then
                    begin
                      bcm:=Item.FRectFrameEn.Left-(FItems[p].FRectFrame.Right+FColSpacing);
                      if Item.FRectFrame.Left-bcm<x then
                        bcm:=Item.FRectFrame.Left-x;
                      if bcm<>0 then
                        _OffsetItem(Item,-bcm,0);
                    end;
                  end;
                  x:=Item.FRectFrameEn.Right+FColSpacing;
                end
              end
              else
              if useFixedWidth then
              begin
                _cropItem(Item,x+FFixedColWidth);
                Inc(x,FFixedColWidth+FColSpacing)
              end
              else
                x:=Item.FRectTxtEn.Right+FColSpacing;

              if (x - FColSpacing > R.Right) then
              begin
                if (i=rowstart) and (lastx=R.Left) then
                begin
                  _AlignLastRow(rowStart, i, 0);
                  rowStart:=i+1;
                end
                else
                begin
                  _AlignLastRow(rowStart, i-1, R.Right-lastx+FColSpacing);
                  rowStart:=i;
                  Dec(i);
                  Dec(FVisibleCount);
                end;
                Inc(y,rowsize);
                x:=R.Left;
              end;
            end
            else
            begin
              if i>rowstart then
                _AlignLastRow(rowStart,i-1,R.Right-x+FColSpacing);
              Inc(pcount);
              _checkLastDispIdx;
              if (i=rowstart) and (y=R.Top) then
              begin
                // just for sure ...
                Item.FPageIndex:=pcount-1;
                rowstart:=i+1;
                OffsetRect(Item.FRect,R.Left-Item.FRect.Left,R.Top-Item.FRect.Top);
                Item.FRectEn:=Item.FRect;
                Item.FRectTxt:=Item.FRect;
                Item.FRectTxtEn:=Item.FRectTxt;
                Item.FRectFrame:=Item.FRectTxt;
                Item.FRectFrameEn:=Item.FRectTxtEn;
              end
              else
              begin
                rowstart:=i;
                Dec(i);
                Dec(FVisibleCount);
              end;
              x:=R.Left;
              y:=R.Top;
            end;
          end;
        end
        else
        begin
          if Item=FHoverItem then
            HoverItem:=nil;
          if validateall and (not Item.FNeedsValidate) then
            Item.FNeedsValidate:=True;
        end;
        Inc(i);
      end;
      if useFixedWidth and (FDirection=tcdVertical) then
      begin
      end
      else
        _AlignLastRow(rowStart,FItems.Count-1,R.Right-x+FColSpacing);
    end
    else
    begin
      Result:=1;
      HoverItem:=nil;
      for i:=0 To FItems.Count-1 do
        if FItems[i].FVisible then
          Inc(FVisibleCount);
    end;
    if FFirstDisplayIndex=-1 then
      FFirstDisplayIndex:=0
    else
    if FLastDisplayIndex=-1 then
    begin
      FLastDisplayIndex:=FItems.Count-1;
      while (FLastDisplayIndex>=0) and (FLastDisplayIndex>FFirstDisplayIndex) do
      begin
        if FItems[FLastDisplayIndex].FVisible then
          break;
        Dec(FLastDisplayIndex);
      end;
    end;
    if (FLastDisplayIndex<FFirstDisplayIndex) and (FItems.Count>0) then
      FLastDisplayIndex:=FFirstDisplayIndex;

    if AutoSize then
    begin
      if (FFixedColWidth>0) and (FDirection=tcdVertical) then
      begin
        // find the most right aligned item of the cloud
        x:=0;
        for i:=FItems.Count-1 downto 0 do
          if FItems[i].FVisible then
          begin
            Item:=FItems[i];
            if Item.FRectFrameEn.Right>x then
              x:=Item.FRectFrameEn.Right
            else
            if Item.FRectFrameEn.Right<x-FFixedColWidth then
              break;
          end;
        {$IF CompilerVersion>=18}
        if x=0 then
          x:=128+FPadding.Left+FPadding.Right
        else
          Inc(x,FPadding.Right);
        {$ELSE}
        if x=0 then
          x:=128+8
        else
          Inc(x,4);
        {$IFEND}
        if Width<>x then
          SetBounds(Left,Top,x,Height);
      end
      else
      begin
        // find the most bottom item at the last row
        y:=0;
        for i:=FItems.Count-1 downto 0 do
          if FItems[i].FVisible then
          begin
            Item:=FItems[i];
            if Item.FRectFrameEn.Bottom>y then
              y:=Item.FRectFrameEn.Bottom
            else
            if Item.FRectFrameEn.Bottom<y-rowsize then
              break;
          end;
        {$IF CompilerVersion>=18}
        if y=0 then
          y:=64+FPadding.Top+FPadding.Bottom
        else
          Inc(y,FPadding.Bottom);
        if (Height<>y) and (y>FPadding.Top+FPadding.Bottom) then
          SetBounds(Left,Top,Width,y);
        {$ELSE}
        if y=0 then
          y:=64+8
        else
          Inc(y,4);
        if (Height<>y) and (y>8) then
          SetBounds(Left,Top,Width,y);
        {$IFEND}
      end
    end
    else
    begin

      if FAutoScaleFont and (FFixedColCount+FFixedColWidth=0) then
      begin
        if (pcount=1) and (FLastDisplayIndex>=0) and (FLastDisplayIndex<FItems.Count) and
          (FItems[FLastDisplayIndex].FPageIndex=0) and (FItems[FLastDisplayIndex].FRectFrameEn.Bottom<(R.Bottom-R.Top)*0.75) then
        begin
          // enlarge the font size coef
          if FAutoScaleFontCoef<0 then
            FAutoScaleFontCoef:=-FAutoScaleFontCoef
          else
          begin
            FAutoScaleFontCoef:=abs(FAutoScaleFontCoef)+0.5;
            asf:=True;
          end;
        end
        else
        if (pcount>1) then
        begin
          // reduce the font size coef
          FAutoScaleFontCoef:=abs(FAutoScaleFontCoef)-0.25;
          if FAutoScaleFontCoef<1 then
            FAutoScaleFontCoef:=-1
          else
          begin
            FAutoScaleFontCoef:=-FAutoScaleFontCoef;
            asf:=True;
          end;
        end;
      end;

      if (FVerticalAlignment in [taAlignBottom, taVerticalCenter]) and (not asf) then
      begin
        p:=-1;
        for i:=0 To FItems.Count-1 do
          if FItems[i].FVisible then
          begin
            Item:=FItems[i];
            if Item.FPageIndex<>p then
            begin
              if p>=0 then
              begin
                // align items vertically
                if FVerticalAlignment = taVerticalCenter then
                  y:=(R.Bottom-R.Top-(y-x)) div 2
                else
                  y:=R.Bottom-y;
                if y<>0 then
                  for n:=i-1 downto 0 do
                    if FItems[n].FPageIndex=p then
                      _OffsetItem(FItems[n],0,y)
                    else
                    if FItems[n].FPageIndex>=0 then
                      break;
              end;
              x:=maxint;
              y:=0;
              p:=Item.FPageIndex;
            end;
            if Item.FRectFrameEn.Top<x then
              x:=Item.FRectFrameEn.Top;
            if Item.FRectFrameEn.Bottom>y then
              y:=Item.FRectFrameEn.Bottom;
          end;
        if p>=0 then
        begin
          // align items vertically
          if FVerticalAlignment = taVerticalCenter then
            y:=(R.Bottom-R.Top-(y-x)) div 2
          else
            y:=R.Bottom-y;
          if y<>0 then
            for n:=FItems.Count-1 downto 0 do
              if FItems[n].FPageIndex=p then
                _OffsetItem(FItems[n],0,y)
              else
              if FItems[n].FPageIndex>=0 then
                break;
        end;
      end;
    end;

    if pcount<>FPageCount then
    begin
      pch:=True;
      FPageCount:=pcount;
    end;

  finally
    if hoverCanvas.Handle<>0 then
    begin
      ReleaseDC(0, hoverCanvas.Handle);
      hoverCanvas.Handle := 0;
    end;
    hoverCanvas.Free;
    Exclude(FState,tcsNeedsValidate);
    if pch and (not asf) then
    begin
      if FPageIndex<0 then
        PageIndex:=0
      else
      if FPageIndex>=FPageCount then
        PageIndex:=FPageCount-1;
    end;
    if FVisibleCount=0 then
      FRowCount:=0;
    if AutoSize then
    begin
      FDisplayCount:=FVisibleCount;
      FVisibleRowCount:=FRowCount;
    end
    else
    if not asf then
    begin
      FDisplayCount:=0;
      p:=-1;
      for i:=FFirstDisplayIndex To FLastDisplayIndex do
      begin
        Item:=FItems[i];
        if Item.FVisible then
        begin
          Inc(FDisplayCount);
          if Item.FRowIndex<>p then
          begin
            Inc(FVisibleRowCount);
            p:=Item.FRowIndex;
          end;
        end;
      end;
    end;
    Exclude(FState,tcsRebuilding);
    if DoRedraw and (not asf) then
      Invalidate;
  end;

  if asf then
  begin
    Include(FState,tcsNeedsValidate);
    Include(FState,tcsNeedsTextMetrics);
    Rebuild;
  end
  else
  begin
    if pch and Assigned(FOnPageCountChanged) then
      FOnPageCountChanged(Self);

    if Assigned(FOnShow) then
      FOnShow(Self);
  end;
end;

procedure TCustomTagCloud.CheckRebuild(Sender:TObject; Param: Integer);
begin
  case param of
    1:RebuildColors;
    2:Invalidate;
    3:Rebuild(True);
    4:Rebuild(False);
    5:begin
        Include(FState,tcsNeedsValidate);
        Rebuild(False);
      end;
  else
    begin
      Include(FState,tcsNeedsValidate);
      Rebuild;
    end;
  end;
end;

procedure TCustomTagCloud.DoFilter;
var
  i:Integer;
  a:Boolean;
  f:string;
begin
  Include(FState,tcsFiltering);
  try
    if Length(FFilter)>0 then
    begin
      if Assigned(FOnFilter) then
      begin
        for i:=0 To FItems.Count-1 do
        begin
          a:=True;
          FOnFilter(Self, FItems[i], a);
          FItems[i].FVisible:=a;
        end;
      end
      else
      begin
        f:=AnsiLowerCase(FFilter);
        for i:=0 To FItems.Count-1 do
        begin
          a:=Pos(f,AnsiLowerCase(FItems[i].FCaption))>0;
          FItems[i].FVisible:=a;
        end;
      end;
    end
    else
      for i:=0 To FItems.Count-1 do
        FItems[i].FVisible:=True;
  finally
    Rebuild;
    Exclude(FState,tcsFiltering);
  end;
end;

function TCustomTagCloud.GetItemAt(X, Y:Integer):TTagCloudItem;
var
  i:Integer;
  p:TPoint;
  ox:Integer;
  R:TRect;
begin
  Result:=nil;

  p.x:=x; p.y:=y;
  if FHoverColSpace and (FColSpacing>0) then
  begin
    case FAlignment of
      taLeftJustify: ox:=FColSpacing;
      taRightJustify: ox:=-FColSpacing;
    else
        if BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign] then
          ox:=-FColSpacing
        else
          ox:=FColSpacing;
    end;
    for i:=FFirstDisplayIndex To FItems.Count-1 do
      if FItems[i].FVisible and (FItems[i].FPageIndex=FPageIndex) then
      begin
        R:=FItems[i].FRectFrameEn;
        if ox>0 then
          Inc(R.Right,ox)
        else
          Inc(R.Left,ox);
        if PtInRect(R,p) then
        begin
          Result:=FItems[i];
          break;
        end;
      end
      else
      if FItems[i].FPageIndex>FPageIndex then
        break;
  end
  else
    for i:=FFirstDisplayIndex To FItems.Count-1 do
      if FItems[i].FVisible and (FItems[i].FPageIndex=FPageIndex) and PtInRect(FItems[i].FRectFrameEn,p) then
      begin
        Result:=FItems[i];
        break;
      end
      else
      if FItems[i].FPageIndex>FPageIndex then
        break;
end;

function TCustomTagCloud.FramesInUse:Boolean;
begin
  Result:=
    (FItemFrame.FrameSize>0)
    or (FHoverFrame.FrameSize>0)
    or (FSelectedFrame.FrameSize>0)
    or ((FItemFrame.BackColor<>clNone) {$IF CompilerVersion>=15} and (FItemFrame.BackColor<>clDefault) {$IFEND})
    or ((FHoverFrame.BackColor<>clNone) {$IF CompilerVersion>=15} and (FHoverFrame.BackColor<>clDefault) {$IFEND})
    or ((FSelectedFrame.BackColor<>clNone) {$IF CompilerVersion>=15} and (FSelectedFrame.BackColor<>clDefault) {$IFEND});
end;

procedure TCustomTagCloud.CheckInvalidateRect(Rect: TRect);
var
  BR:TRect;

  function BackgroundClipped: Boolean;
  var
    R: TRect;
    I,j: Integer;
    C: TControl;
  begin
    Result := True;
    i:=0;
    for j:=0 To Parent.ControlCount-1 do
      if Parent.Controls[j]=Self then
      begin
        i:=j;
        break;
      end;
    while I > 0 do
    begin
      Dec(I);
      C := TControl(Parent.Controls[I]);
      with C do
        if C.Visible and (csOpaque in ControlStyle) then
        begin
          IntersectRect(R, Rect, BoundsRect);
          if EqualRect(R, Rect) then Exit;
        end;
    end;
    Result := False;
  end;

begin
  if (Visible or ((csDesigning in ComponentState) {$IF CompilerVersion>=20} and not (csDesignerHide in ControlState) {$IFEND} )
    and not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and Parent.HandleAllocated then
  begin
    BR:=BoundsRect;
    offsetRect(Rect,BR.Left,BR.Top);
    InvalidateRect(Parent.Handle, @Rect, not ((csOpaque in ControlStyle) or (csOpaque in Parent.ControlStyle) or BackgroundClipped));
  end;
end;

procedure TCustomTagCloud.SetHoverEnlarge(Value: Boolean);
begin
  if FHoverEnlarge<>Value then
  begin
    FHoverEnlarge:=Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetHoverColSpace(Value: Boolean);
begin
  if FHoverColSpace<>Value then
  begin
    FHoverColSpace:=Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetHoverStyle(const Value: TFontStyles);
begin
  if FHoverStyle<>Value then
  begin
    FHoverStyle:=Value;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetHoverItem(const Value:TTagCloudItem);
var
  R:TRect;
begin
  if Value<>FHoverItem then
  begin
    if Assigned(FHoverItem) and (not (csDestroying in ComponentState)) and Assigned(Parent)
      and (not ((Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered)) then
    begin
      R:=FHoverItem.FRectFrameEn;
      if FHoverColSpace and (FColSpacing>0) then
        ValidateItemHoverColSpaceRect(FHoverItem, R);
      FHoverItem:=Value;
      CheckInvalidateRect(R);
    end
    else
      FHoverItem:=Value;
    if Assigned(FHoverItem) then
    begin
      if Cursor<>FHoverCursor then
        FCmpCursor:=Cursor;
      Cursor:=FHoverCursor;
    end
    else
      Cursor:=FCmpCursor;
    if Assigned(FOnHoverChange) then
      FOnHoverChange(Self,FHoverItem);
    if (Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
      Invalidate
    else
    if Assigned(FHoverItem) and (not (csDestroying in ComponentState)) and Assigned(Parent) then
    begin
      if FHoverColSpace and (FColSpacing>0) then
      begin
        R:=FHoverItem.FRectFrameEn;
        ValidateItemHoverColSpaceRect(FHoverItem, R);
        CheckInvalidateRect(R);
      end
      else
        CheckInvalidateRect(FHoverItem.FRectFrameEn);
    end;
  end;
end;

procedure TCustomTagCloud.SetSelectedItem(const Value:TTagCloudItem);
var
  R:TRect;
begin
  if Value<>FSelectedItem then
  begin
    if FItems.UpdateCount>0 then
      FSelectedItem:=Value
    else
    begin
      if Assigned(FSelectedItem) and (not (csDestroying in ComponentState)) and Assigned(Parent)
        and (not ((Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered)) then
      begin
        R:=FSelectedItem.FRectFrameEn;
        if FHoverColSpace and (FColSpacing>0) then
          ValidateItemHoverColSpaceRect(FSelectedItem, R);
        FSelectedItem:=Value;
        CheckInvalidateRect(R);
      end
      else
        FSelectedItem:=Value;
      if (Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
        Invalidate
      else
      if Assigned(FSelectedItem) and (not (csDestroying in ComponentState)) and Assigned(Parent) then
      begin
        if FHoverColSpace and (FColSpacing>0) then
        begin
          R:=FSelectedItem.FRectFrameEn;
          ValidateItemHoverColSpaceRect(FSelectedItem, R);
          CheckInvalidateRect(R);
        end
        else
          CheckInvalidateRect(FSelectedItem.FRectFrameEn);
      end;
    end;
  end;
end;

procedure TCustomTagCloud.SetSelectedColor(const Value: TColor);
var
  R:TRect;
begin
  if FSelectedColor<>Value then
  begin
    FSelectedColor:=Value;
    if FSelectedItem<>nil then
    begin
      if (Transparent or (FGlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
        Invalidate
      else
      if (not (csDestroying in ComponentState)) and Assigned(Parent) then
      begin
        if FHoverColSpace and (FColSpacing>0) then
        begin
          R:=FSelectedItem.FRectFrameEn;
          ValidateItemHoverColSpaceRect(FSelectedItem, R);
          CheckInvalidateRect(R);
        end
        else
          CheckInvalidateRect(FSelectedItem.FRectFrameEn);
      end;
    end;
  end;
end;

procedure TCustomTagCloud.ClearSelection;
var
  i:Integer;
begin
  FItems.BeginUpdate;
  try
    for i:=0 To FItems.Count-1 do
      FItems[i].Selected:=False;
    SelectedItem:=nil;
  finally
    FItems.EndUpdate;
  end;
end;

procedure TCustomTagCloud.SetStyler(Value: TCustomTagCloudStyler);
begin
  if Value <> FStyler then
  begin
    if (Styler <> nil) and (not (csDestroying in Styler.ComponentState)) then
      Styler.FClients.Remove(Self);
    FStyler := Value;
    if Styler <> nil then
    begin
      Styler.FreeNotification(Self);
      Styler.FClients.Add(Self);
      Styler.ApplyStyle(Self);
    end
    else
    if not (csDestroying in ComponentState) then
      Rebuild;
  end;
end;

procedure TCustomTagCloud.SetMaxFontSize(Value: Integer);
begin
  if Value<>FMaxFontSize then
  begin
    if Value<Font.Size then
      FMaxFontSize:=Font.Size
    else
      FMaxFontSize:=Value;
    FAutoScaleFontCoef:=1;
    Include(FState,tcsNeedsValidate);
    Rebuild;
  end;
end;

{$IF CompilerVersion>=18}
procedure TCustomTagCloud.SetPadding(const Value: TPadding);
begin
  FPadding.Assign(Value);
end;
{$IFEND}

procedure TCustomTagCloud.SetOnAfterPaint(Value: TTagCloudCustomPaint);
begin
  FOnAfterPaint := Value;
  Invalidate;
end;

procedure TCustomTagCloud.SetOnBeforePaint(Value: TTagCloudCustomPaint);
begin
  FOnBeforePaint := Value;
  Invalidate;
end;

procedure TCustomTagCloud.SetOnCompareItems(Value: TTagCompareEvent);
begin
  FOnCompareItems := Value;
  if FSorted then
    Sort;
end;

procedure TCustomTagCloud.SetOnCustomDrawItem(Value: TTagCustomDraw);
begin
  FOnCustomDrawItem := Value;
  Invalidate;
end;

procedure TCustomTagCloud.SetOnAdvCustomDrawItem(Value: TTagAdvancedCustomDraw);
begin
  FOnAdvCustomDrawItem := Value;
  Invalidate;
end;

procedure TCustomTagCloud.SetOnTagPositioning(Value: TTagPositioningEvent);
begin
  FOnTagPositioning := Value;
  Rebuild;
end;

procedure TCustomTagCloud.SetPageIndex(Value: Integer);
var
  i,k:Integer;
begin
  if (Value<>FPageIndex) and (Value>=0) and (Value<FPageCount) then
  begin
    FPageIndex:=Value;

    FFirstDisplayIndex:=-1;
    for i:=0 To FItems.Count-1 do
      if (FItems[i].FPageIndex=FPageIndex) and FItems[i].FVisible then
      begin
        FFirstDisplayIndex:=i;
        Break;
      end;
    if FFirstDisplayIndex>=0 then
    begin
      if AutoSize then
        k:=FItems.Count-1
      else
      begin
        k:=FItems.Count-1;
        for i:=FFirstDisplayIndex+1 To FItems.Count-1 do
          if FItems[i].FPageIndex>FPageIndex then
          begin
            k:=i-1;
            Break;
          end;
      end;
      FLastDisplayIndex:=-1;
      for i:=k downto FFirstDisplayIndex+1 do
        if (FItems[i].FPageIndex=FPageIndex) and FItems[i].FVisible then
        begin
          FLastDisplayIndex:=i;
          Break;
        end;
      if (FLastDisplayIndex<FFirstDisplayIndex) and (FItems.Count>0) then
        FLastDisplayIndex:=FFirstDisplayIndex;
    end
    else
    begin
      FFirstDisplayIndex:=0;
      FLastDisplayIndex:=-1;
    end;

    if not (tcsRebuilding in FState) then
    begin
      if AutoSize then
      begin
        FDisplayCount:=FVisibleCount;
        FVisibleRowCount:=FRowCount;
      end
      else
      begin
        FDisplayCount:=0;
        FVisibleRowCount:=0;
        k:=-1;
        for i:=FFirstDisplayIndex To FLastDisplayIndex do
          if FItems[i].FVisible then
          begin
            Inc(FDisplayCount);
            if FItems[i].FRowIndex<>k then
            begin
              Inc(FVisibleRowCount);
              k:=FItems[i].FRowIndex;
            end;
          end;
      end;
      Invalidate;
      if Assigned(FOnShow) then
        FOnShow(Self);
    end;
  end;
end;

procedure TCustomTagCloud.Loaded;
begin
  inherited Loaded;
  Rebuild(False);
end;

procedure TCustomTagCloud.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FMaxFontSize<Font.Size then
    FMaxFontSize:=Font.Size;
  FAutoScaleFontCoef:=1;
  Include(FState,tcsNeedsValidate);
  Include(FState,tcsNeedsTextMetrics);
  Rebuild(False);
end;

procedure TCustomTagCloud.CMBiDiModeChanged(var Message: TMessage);
begin
  Rebuild(not ((SysLocale.MiddleEast) and (Message.wParam = 0)));
  inherited;
end;

procedure TCustomTagCloud.CMHintShow(var Msg: TMessage);
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
        HintInfo.CursorRect:=FHoverItem.FRect;
      end;
      if Assigned(FOnTagHint) then
        FOnTagHint(Self, FHoverItem,HintInfo.HintStr);

      if Length(HintInfo.HintStr)>0 then
        Result := 0
      else
        Result := 1
    end;
  end
end;

procedure TCustomTagCloud.CMMouseLeave(var Msg: TMessage);
begin
  if Assigned(FHoverItem) then
    HoverItem := nil;
  inherited;
end;

procedure TCustomTagCloud.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (not Assigned(FHoverItem)) or (not PtInRect(FHoverItem.FRectFrameEn,Point(x,y))) then
    HoverItem := GetItemAt(X,Y);
end;

procedure TCustomTagCloud.Click;
begin
  if Assigned(FHoverItem) and Assigned(FOnTagClick) then
    FOnTagClick(Self,FHoverItem);
  inherited;
end;

procedure TCustomTagCloud.DblClick;
begin
  if Assigned(FHoverItem) and Assigned(FOnTagDblClick) then
    FOnTagDblClick(Self,FHoverItem);
  inherited;
end;

procedure TCustomTagCloud.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Styler then
      Styler := nil;
  end;
end;

procedure TCustomTagCloud.SetAutoShrinkRows(Value: Boolean);
begin
  if Value<>FAutoShrinkRows then
  begin
    FAutoShrinkRows:=Value;
    Rebuild;
  end;
end;

procedure TCustomTagCloud.SetAutoSize(Value: Boolean);
begin
  {$IF CompilerVersion>=15}
  if Value<>AutoSize then
  begin
    inherited;
    Rebuild(False);
  end;
  {$ELSE}
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then AdjustSize;
    Rebuild(False);
  end;
  {$IFEND}
end;

procedure TCustomTagCloud.Sort;
begin
  FItems.BeginUpdate;
  try
    FItems.Sort;
  finally
    FItems.EndUpdate;
  end;
end;

procedure _StringToList(const S: string; Sep:char; List:TStrings);
var
  P, P1: PChar;
  T: string;
begin
  List.BeginUpdate;
  try
    List.Clear;
    if Length(S)>0 then
    begin
      P := PChar(S);
      {$IF CompilerVersion>=20}
      while CharInSet(P^,[#1..' ']) do P := CharNext(P);
      while P^ <> #0 do
      begin
        if P^ = '"' then
          T := AnsiExtractQuotedStr(P, '"')
        else
        begin
          P1 := P;
          while (P^ >= ' ') and (P^ <> Sep) do P := CharNext(P);
          SetString(T, P1, P - P1);
        end;
        List.Add(T);
        while CharInSet(P^,[#1..' ']) do P := CharNext(P);
        if P^ = Sep then
          repeat
            P := CharNext(P);
          until not CharInSet(P^,[#1..' ']);
      end;
      {$ELSE}
      while P^ in [#1..' '] do P := CharNext(P);
      while P^ <> #0 do
      begin
        if P^ = '"' then
          T := AnsiExtractQuotedStr(P, '"')
        else
        begin
          P1 := P;
          while (P^ >= ' ') and (P^ <> Sep) do P := CharNext(P);
          SetString(T, P1, P - P1);
        end;
        List.Add(T);
        while P^ in [#1..' '] do P := CharNext(P);
        if P^ = Sep then
          repeat
            P := CharNext(P);
          until not (P^ in [#1..' ']);
      end;
      {$IFEND}
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TagCloudRecords_QuickSort(var TagRecords:TTagCloudRecords; L, R: Integer; ByValue:Boolean);
var
  I, J, P: Integer;
  T:PTagCloudRecord;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      if ByValue then
      begin
        // descending order
        while TagRecords[i]^.Value>TagRecords[p]^.Value do Inc(I);
        while TagRecords[j]^.Value<TagRecords[p]^.Value do Dec(J);
      end
      else
      begin
        while AnsiCompareText(TagRecords[i]^.Caption,TagRecords[p]^.Caption) < 0 do Inc(I);
        while AnsiCompareText(TagRecords[j]^.Caption,TagRecords[p]^.Caption) > 0 do Dec(J);
      end;
      if I <= J then
      begin
        if I <> J then
        begin
          T:=TagRecords[I];
          TagRecords[I]:=TagRecords[J];
          TagRecords[J]:=T;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then TagCloudRecords_QuickSort(TagRecords,L, J, ByValue);
    L := I;
  until I >= R;
end;

procedure TCustomTagCloud.LoadFromArray(var Source:TTagCloudRecords);
var
  i:Integer;
begin
  Include(FState,tcsLoadingData);
  FItems.BeginUpdate;
  try
    FItems.Clear;
    if FSorted then
      Include(FState, tcsNeedsSort);
    {$IF CompilerVersion>=18}
    FItems.Capacity:=Length(Source);
    {$IFEND}
    for i:=0 To Length(Source)-1 do
      FItems.Add.AssignRecord(Source[i],False);
  finally
    Exclude(FState,tcsLoadingData);
    Include(FState,tcsNeedsValidate);
    FItems.EndUpdate;
  end;
  if Length(FFilter)>0 then
    DoFilter;
end;

procedure TCustomTagCloud.LoadFromStrings(Source:TStrings);
var
  i,s,c:Integer;
  tmpValue:Int64;
  Fields:TStringList;
  TagList:TTagCloudRecords;
  TagRec:PTagCloudRecord;
  Item:TTagCloudItem;
begin
  Include(FState,tcsLoadingData);
  FItems.BeginUpdate;
  Fields:=TStringList.create;
  try
    FItems.Clear;
    SetLength(TagList,Source.Count);
    c:=0;
    if FLoadingOptions.SkipFirstRow then s:=1 else s:=0;
    for i:=s To Source.Count-1 do
    begin
      _StringToList(Source[i],FLoadingOptions.Separator,Fields);
      if Fields.Count>0 then
      begin
        if (FLoadingOptions.ColValue>=0) and (FLoadingOptions.ColValue<Fields.Count) then
          tmpValue:=StrToTagCloudValue(Fields[FLoadingOptions.ColValue],FLoadingOptions.ValuesGreaterThan)
        else
          tmpValue:=FLoadingOptions.ValuesGreaterThan;
        if tmpValue>FLoadingOptions.ValuesGreaterThan then
        begin
          New(TagRec);
          TagRec^.Value:=tmpValue;
          if (FLoadingOptions.ColCaption>=0) and (FLoadingOptions.ColCaption<Fields.Count) then TagRec^.Caption:=Fields[FLoadingOptions.ColCaption] else TagRec^.Caption:='';
          if (FLoadingOptions.ColHint>=0) and (FLoadingOptions.ColHint<Fields.Count) then TagRec^.Hint:=Fields[FLoadingOptions.ColHint] else TagRec^.Hint:='';
          if (FLoadingOptions.ColTag>=0) and (FLoadingOptions.ColTag<Fields.Count) then TagRec^.Tag:=StrToIntDef(Fields[FLoadingOptions.ColTag],0) else TagRec^.Tag:=0;
          if (FLoadingOptions.ColData>=0) and (FLoadingOptions.ColData<Fields.Count) then TagRec^.Data:=Pointer(StrToIntDef(Fields[FLoadingOptions.ColData],0)) else TagRec^.Data:=nil;
          TagList[c]:=TagRec;
          Inc(c);
        end;
      end;
    end;
    if c>0 then
    begin
      SetLength(TagList,c);
      if (FLoadingOptions.MaxItemsCount>0) and (FLoadingOptions.MaxItemsCount<c) then
      begin
        TagCloudRecords_QuickSort(TagList, 0, c-1, True); // sort by value
        c:=FLoadingOptions.MaxItemsCount;
        for i:=c To Length(TagList)-1 do
        begin
          TagRec:=TagList[i];
          if TagRec<>nil then
            Dispose(TagRec);
        end;
        SetLength(TagList,c);
        if FSorted and Assigned(FOnCompareItems) then
          Include(FState, tcsNeedsSort) // let the Items.EndUpdate to execute the custom sort
        else
        begin
          TagCloudRecords_QuickSort(TagList, 0, c-1, False); // sort by caption
          Exclude(FState, tcsNeedsSort);
        end;
      end
      else
      if FLoadingOptions.AlphaSort then
      begin
        if FSorted and Assigned(FOnCompareItems) then
          Include(FState, tcsNeedsSort) // let the Items.EndUpdate to execute the custom sort
        else
        begin
          TagCloudRecords_QuickSort(TagList, 0, c-1, False); // sort by caption
          Exclude(FState, tcsNeedsSort);
        end;
      end
      else
      if FSorted then
        Include(FState, tcsNeedsSort); // let the Items.EndUpdate to execute the Sort method

      // fill TagCloudItems collection
      {$IF CompilerVersion>=18}
      FItems.Capacity:=c;
      {$IFEND}
      for i:=0 To c-1 do
      begin
        TagRec:=TagList[i];
        Item:=FItems.Add;
        Item.AssignRecord(TagRec,False);
        if FLoadingOptions.FLowerCase then
          Item.FCaption := AnsiLowerCase(TagRec^.Caption);
        Dispose(TagRec);
      end;
      SetLength(TagList,0);
    end;
  finally
    Fields.Free;
    Exclude(FState,tcsLoadingData);
    Include(FState,tcsNeedsValidate);
    FItems.EndUpdate;
  end;
  if Length(FFilter)>0 then
    DoFilter;
end;

procedure TCustomTagCloud.LoadFromFile(const FileName:string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomTagCloud.LoadFromStream(const Stream:TStream);
var
  S:TStringList;
begin
  S:=TStringList.Create;
  try
    S.LoadFromStream(Stream);
    LoadFromStrings(S);
  finally
    S.Free;
  end;
end;

procedure TCustomTagCloud.SaveToArray(var Dest:TTagCloudRecords);
var
  i:Integer;
  TagRec:PTagCloudRecord;
begin
  SetLength(Dest,FItems.Count);
  for i:=0 To FItems.Count-1 do
  begin
    New(TagRec);
    FItems[i].AssignToRecord(TagRec);
  end;
end;

procedure TCustomTagCloud.SaveToStrings(Source:TStrings);
const
  qchar = '"';
var
  i:Integer;
  tmp:string;
  Item:TTagCloudItem;
begin
  Source.BeginUpdate;
  try
    Source.Clear;
    for i:=0 To FItems.Count-1 do
    begin
      Item:=FItems[i];
      if FLoadingOptions.ColCaption=0 then
        tmp:=qchar+Item.FCaption+qchar+FLoadingOptions.FSeparator+IntToStr(Item.FValue)+FLoadingOptions.FSeparator
      else
        tmp:=IntToStr(Item.FValue)+FLoadingOptions.FSeparator+qchar+Item.FCaption+qchar+FLoadingOptions.FSeparator;
      if FLoadingOptions.ColHint>=0 then
        tmp:=tmp+FLoadingOptions.FSeparator+qchar+Item.FHint+qchar;
      if FLoadingOptions.ColTag>=0 then
        tmp:=tmp+FLoadingOptions.FSeparator+IntToStr(Item.FTag);
      if FLoadingOptions.ColData>=0 then
        tmp:=tmp+FLoadingOptions.FSeparator+IntToStr(Int64(Item.FData));
      Source.Add(tmp);
    end;
  finally
    Source.EndUpdate;
  end;
end;

procedure TCustomTagCloud.SaveToStream(const Stream:TStream);
var
  S:TStringList;
begin
  S:=TStringList.Create;
  try
    SaveToStrings(S);
    S.SaveToStream(Stream);
  finally
    S.Free;
  end;
end;

procedure TCustomTagCloud.SaveToFile(const FileName:string);
var
  S:TStringList;
begin
  S:=TStringList.Create;
  try
    SaveToStrings(S);
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TCustomTagCloud.GetItemMetrics(Item:TTagCloudItem; var M:TTagCloudItemMetrics);
begin
  if Assigned(Item) then
  begin
    M.PageIndex:=Item.FPageIndex;
    M.FontSize:=Item.FSize;
    M.FrameRect:=Item.FRectFrame;
    M.FrameRectHovered:=Item.FRectFrameEn;
    M.TextRect:=Item.FRectTxt;
    M.TextRectHovered:=Item.FRectTxtEn;
    M.DrawRect:=Item.FRect;
    M.DrawRectHovered:=Item.FRectEn;
  end;
end;

function TCustomTagCloud.GetItemPageIndex(Item:TTagCloudItem):Integer;
begin
  if Assigned(Item) then
    Result:=Item.FPageIndex
  else
    Result:=-1;
end;

function TCustomTagCloud.GetItemRowIndex(Item:TTagCloudItem):Integer;
begin
  if Assigned(Item) then
    Result:=Item.FRowIndex
  else
    Result:=-1;
end;

function TCustomTagCloud.ValidateItemHoverColSpaceRect(Item:TTagCloudItem; var FrameRect:TRect):boolean;
var
  m:Integer;
begin
  if FHoverColSpace and (FColSpacing>0) and Assigned(Item) and (Item=FHoverItem) then
  begin
    m:=FColSpacing - (FColSpacing div 6 + 1);
    case FAlignment of
      taLeftJustify: Inc(FrameRect.Right,m);
      taRightJustify: Dec(FrameRect.Left,m);
    else
      if BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign] then
        Dec(FrameRect.Left,m)
      else
        Inc(FrameRect.Right,m);
    end;
    Result:=True;
  end
  else
    Result:=False;
end;

end.

