{******************************************************************************}
{                                                                              }
{  Unit: ListView with groups and Alpha skins support                          }
{                                                                              }
{  Author:  Kolpakchi Stanislav S.(EN) (c)                                     }
{  E-mail:  SSKolpakchi@gmail.com                                              }
{  Version: v0.6.1 (28 may 2010)                                               }
{                                                                              }
{******************************************************************************}

unit aceListView;
{$I sDefs.inc}

interface

uses
  // Delphi units
  SysUtils, Classes, Controls, ImgList, Forms, Windows, Graphics, ComCtrls,
  Messages, StdCtrls, CommCtrl, {$IFNDEF DELPHI5}GraphUtil, {$ENDIF}
  // AC units
  sSkinManager, sGraphUtils, sCommonData, sVCLUtils, sStyleSimply, sSkinProps,
  sConst, sDefaults, acntUtils, acSBUtils, sAlphaGraph, sMessages,
  {$IFDEF DELPHI_XE2}UITypes, {$ENDIF}
  sHTMLParse;

type
  TGridLines = (glBoth, glHorizontal, glNone, glVertical);
  TSortMode = (smAutomatic, smManual);
  TSortKind = (skASC, skDESC, skNone);
  TRegularBack = (rbAll, rbEven, rbOdd, rbNone);
  TItemWidth = (iwClientWidth, iwColumnsWidth);
  TDataType = (dtString, dtNumber, dtDataTime);
  TListCheckBoxes = (lcCheck, lcRadio, lcNone);
  TGroupCheckBoxes = (gcCheck, gcRadio, gcNone, gcInherit);
  TItemAttribute = (iaSelect, iaCheck, iaVisible, iaGroup, iaGroupVisible);
  TSpaceKeyAction = (skDoNothing, skDoSelectFocused, skDoInvertCheckFocused, skDoInvertCheckSelected);

  TItemAttributes = set of TItemAttribute;
  TacPointerList = array of Pointer;

  TacListView = class;
  TacListElement = class;
  TacListItem = class;
  TacListGroup = class;

  TGetCountEvent = procedure (Sender: TacListGroup; var CountString: string) of object;
  TSetItemCheckEvent = procedure (Sender: TacListItem) of object;

  TacListElement = class(TCollectionItem)
  private
    FOwner: TacListView;
    FOrderTag: Integer;
    FCaption: string;
    FVisible: boolean;
    FTag: Integer;
    FHint: string;
    FShowHint: boolean;
    FBounds: TRect;
    FAllowReorder: boolean;
    procedure DoChange; virtual; abstract;
    procedure CalcBounds(var Pos: TPoint); virtual; abstract;
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: boolean);
  protected
    procedure Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean); virtual; abstract;
    procedure Click(MousePos: TPoint); virtual; abstract;
    procedure PaintGridLines(ElementCanvas, ListViewCanvas: TCanvas; PaintBounds: TRect);
  public
    constructor Create(Collection: TCollection); override;
    function GetPrevVisible: TacListElement;
    function GetNextVisible: TacListElement;
    property KssListView: TacListView read FOwner;
    property Bounds: TRect read FBounds;
  published
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write FHint;
    property ShowHint: boolean read FShowHint write FShowHint default false;
    property Visible: boolean read FVisible write SetVisible default true;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TacListGroup = class(TacListElement)
  private
    FDescription: string;
    FFooter: string;
    FImageIndex: TImageIndex;
    FExpanded: boolean;
    FCheckBoxes: TGroupCheckBoxes;
    FCheckState: TCheckBoxState;
    FCheckBounds: TRect;
    FFooterBounds: TRect;
    procedure DoChange; override;
    procedure CalcBounds(var Pos: TPoint); override;
    procedure CalcFooterBounds(var Pos: TPoint);
    procedure SetFooter(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetExpanded(const Value: boolean);
    procedure SetImageIndex(Value: TImageIndex);
    function GetSelectedItem: TacListItem;
    procedure SetSelectedItem(Value: TacListItem);
    procedure SetSelected(Value: boolean);
    function  GetSelected: boolean;
    procedure SetCheckBoxes(Value: TGroupCheckBoxes);
    function  GetChecked: boolean;
    procedure SetChecked(Value: boolean);
    procedure UpdateCheck;
  protected
    function  GetButtonRect: TRect;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean); override;
    procedure PaintFooter(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean);
    procedure Click(MousePos: TPoint); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetItem(Index: Integer): TacListItem;
    function GetNextVisibleItem(Item: TacListItem): TacListItem;
    function GetPrevVisibleItem(Item: TacListItem): TacListItem;
    function FirstItem: TacListItem;
    function FirstVisibleItem: TacListItem;
    function LastItem: TacListItem;
    function LastVisibleItem: TacListItem;
    function SelectItems(FromItem, ToItem: TacListItem): integer;
    function SelectItemsToLast(FromItem: TacListItem): integer;
    function SelectItemsToFirst(FromItem: TacListItem): integer;
    procedure SelectAll;
    function Add(Caption: string = ''; ImageIndex: integer = -1): TacListItem;
    function Count: integer; overload;
    function Count(ItemAttributes: TItemAttributes): integer; overload;
    function GetCheckBoxesFinal: TGroupCheckBoxes;
  published
    property CheckBoxes: TGroupCheckBoxes read FCheckBoxes write SetCheckBoxes default gcInherit;
    property Footer: string read FFooter write SetFooter;
    property Description: string read FDescription write SetDescription;
    property Expanded: boolean read FExpanded write SetExpanded;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property SelectedItem: TacListItem read GetSelectedItem write SetSelectedItem;
    property Selected: boolean read GetSelected write SetSelected;
    property Checked: boolean read GetChecked write SetChecked default false;
  end;

  TacListColumn = class(TacListElement)
  private
    FImageIndex: TImageIndex;
    FWidth: integer;
    FSortMode: TSortMode;
    FSortKind: TSortKind;
    procedure DoChange; override;
    procedure SetWidth(const Value: integer);
    procedure SetImageIndex(Value: TImageIndex);
    procedure CalcBounds(var Pos: TPoint); override;
    procedure SetSortKind(Value: TSortKind);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean); override;
    procedure Click(MousePos: TPoint); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure SetOptimalWidth;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: integer read FWidth write SetWidth default 100;
    property SortMode: TSortMode read FSortMode write FSortMode default smAutomatic;
    property SortKind: TSortKind read FSortKind write SetSortKind default skNone;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  TacListItem = class(TacListElement)
  private
    FImageIndex: TImageIndex;
    FStateIndex: TImageIndex;
    FSelected: boolean;
    FData: Pointer;
    FSubItems: TStrings;
    FGroup: TacListGroup;
    FGroupIndex: integer;
    FShowProgress: boolean;
    FProgressPosition: integer;
    FChecked: boolean;
    FCheckBounds: TRect;
    procedure DoChange; override;
    procedure SetSelected(const Value: boolean);
    procedure SetShowProgress(const Value: boolean);
    procedure SetProgressPosition(const Value: integer);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetStateIndex(Value: TImageIndex);
    procedure SetGroup(Value: TacListGroup);
    procedure SetGroupIndex(const Value: integer);
    procedure SetSubItems(Value: TStrings);
    procedure CalcBounds(var Pos: TPoint); override;
    procedure SetChecked(Value: boolean);
    function  GetHidden: boolean;
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean); override;
    procedure Click(MousePos: TPoint); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetCheckBoxesFinal: TGroupCheckBoxes;
    function IndexInGroup: integer;
    function GetWidth: integer;
    function GetSubItemWidth(Index: integer): integer;
    property Data: Pointer read FData write FData;
    property Group: TacListGroup read FGroup write SetGroup;
    property Hidden: boolean read GetHidden;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property StateIndex: TImageIndex read FStateIndex write SetStateIndex default -1;
    property Selected: boolean read FSelected write SetSelected default false;
    property ShowProgress: boolean read FShowProgress write SetShowProgress default false;
    property SubItems: TStrings read FSubItems write SetSubItems;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default -1;
    property ProgressPosition: Integer read FProgressPosition write SetProgressPosition default 0;
    property Checked: boolean read FChecked write SetChecked default false;
  end;

  TacListGroups = class(TCollection)
  private
    FOwner: TacListView;
    function GetItem(Index: Integer): TacListGroup;
    procedure SetItem(Index: Integer; Value: TacListGroup);
    procedure UpdateGroups;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TacListView);
    function Add(Caption: string = ''; ImageIndex: integer = -1): TacListGroup;
    function Owner: TacListView;
    function ItemByCaption(Caption: string): TacListGroup;
    property Items[Index: Integer]: TacListGroup read GetItem write SetItem; default;
  end;

  TacListColumns = class(TCollection)
  private
    FOwner: TacListView;
    function GetItem(Index: Integer): TacListColumn;
    procedure SetItem(Index: Integer; Value: TacListColumn);
    procedure UpdateColumns;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TacListView);
    function Add: TacListColumn;
    function Owner: TacListView;
    function GetLastVisible: TacListColumn;
    function WidthOfVisible: integer;
    property Items[Index: Integer]: TacListColumn read GetItem write SetItem; default;
  end;

  TacListItems = class(TCollection)
  private
    FOwner: TacListView;
    function GetItem(Index: Integer): TacListItem;
    procedure SetItem(Index: Integer; Value: TacListItem);
    procedure UpdateItems;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TacListView);
    function Add(Caption: string = ''; ImageIndex: integer = -1): TacListItem;
    function Owner: TacListView;
    function Count: integer; overload;
    function Count(ItemAttributes: TItemAttributes): integer; overload;
    property Items[Index: Integer]: TacListItem read GetItem write SetItem; default;
  end;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TacListView = class(TCustomControl)
  private
    //AC skins
    FSkinData: TsScrollWndData;
    FBoundLabel: TsBoundLabel;
    FDisabledKind: TsDisabledKind;
    FGroupSkin: string;
    FColumnSkin: string;
    FItemSkin: string;
    //Colors
    FHotItemColor: TColor;
    FSelectItemColor: TColor;
    FRegularItemColor: TColor;
    FProgressItemColor: TColor;
    FGridColor: TColor;
    FSelectionFrameColor: TColor;
    //Fonts
    FGroupFont: TFont;
    FColumnFont: TFont;
    FItemFont: TFont;
    //Images
    FStateImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FGroupImages: TCustomImageList;
    FStateChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FGroupChangeLink: TChangeLink;
    //Collections
    FListGroups: TacListGroups;
    FListColumns: TacListColumns;
    FListItems: TacListItems;
    //Elements states
    FHotElement: TacListElement;
    FDownElement: TacListElement;
    FDragElement: TacListElement;
    FSizeColumn: TacListColumn;
    FFocusedElement: TacListElement;
    //Scroll
    FVScrollPos: integer;
    FVScrollMax: integer;
    FHScrollPos: integer;
    FHScrollMax: integer;
    FVScrollBarShown: Boolean;
    FHScrollBarShown: Boolean;
    //Element metrics
    FScale: single;
    FGroupHeight: integer;
    FColumnHeight: integer;
    FItemHeight: integer;
    // Show
    FShowGroupItemsCount: boolean;
    FShowColumnHeaders: boolean;
    FShowGroups: boolean;
    FShowGroupButtons: boolean;
    // Selection
    FMultiSelect: boolean;
    FSelectionFrame: boolean;
    FSelectionRect: TRect;
    FAllowSelection: boolean;
    // Events
    FGetCountEvent: TGetCountEvent;
    // Actions
    FSpaceKeyAction: TSpaceKeyAction;
    FDragStarted: boolean;
    FDragStartPos: TPoint;
    FDragStartShift: TShiftState;
    FDblClicked: boolean;
    FRegularBack: TRegularBack;
    FItemWidth: TItemWidth;
    FSizeStartWidth: integer;
    FUpdateCount: integer;
    FCheckBoxes: TListCheckBoxes;
    FCheckOnClik: boolean;
    FColumnsBackBounds: TRect;
    FCaptionOnEmpty: string;
    FGridLines: TGridLines;
    FBorderStyle: TBorderStyle;
    //AC skins
    procedure SetSkinData(Value: TsScrollWndData);
    procedure SetGroupSkin(Value: string);
    procedure SetColumnSkin(Value: string);
    procedure SetItemSkin(Value: string);
    // Colors
    procedure SetHotItemColor(Value: TColor);
    procedure SetSelectItemColor(Value: TColor);
    procedure SetRegularItemColor(Value: TColor);
    procedure SetProgressItemColor(Value: TColor);
    procedure SetGridColor(Value: TColor);
    procedure SetSelectionFrameColor(Value: TColor);
    //Fonts
    procedure SetGroupFont(Value: TFont);
    procedure SetColumnFont(Value: TFont);
    procedure SetItemFont(Value: TFont);
    //Images
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetSmallImages(Value: TCustomImageList);
    procedure SetGroupImages(Value: TCustomImageList);
    //Collections
    procedure SetListGroups(Value: TacListGroups);
    procedure SetListColumns(Value: TacListColumns);
    procedure SetListItems(Value: TacListItems);
    //Updates
    procedure UpdateGroup(AnIndex: integer);
    procedure UpdateColumn(AnIndex: integer);
    procedure UpdateItem(AnIndex: integer);
    procedure UpdateGroups;
    procedure UpdateColumns;
    procedure UpdateItems;
    //Elements states
    procedure SetHotElement(Value: TacListElement);
    function  GetSelectedItem: TacListItem;
    procedure SetSelectedItem(Value: TacListItem);
    function  GetCheckedItem: TacListItem;
    procedure SetCheckedItem(Value: TacListItem);
    function  GetFocusedItem: TacListItem;
    function  GetFocusedGroup: TacListGroup;
    //Scroll
    procedure ShowScrollBar(const ScrollBarKind: TScrollBarKind; const Visible: Boolean);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure ScrollPosChanged(ScrollBarKind: TScrollBarKind; ScrollCode: TScrollCode; ScrollPos: Integer);
    //Element metrics
    procedure SetGroupHeight(Value: integer);
    procedure SetSpaceKeyAction(Value: TSpaceKeyAction);
    procedure SetColumnHeight(Value: integer);
    procedure SetItemHeight(Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetShowGroupItemsCount(const Value: boolean);
    procedure SetShowColumnHeaders(const Value: boolean);
    procedure SetShowGroups(const Value: boolean);
    procedure SetShowGroupButtons(const Value: boolean);
    procedure SetMultiSelect(const Value: boolean);
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    Procedure WMLastFocus(var Message : TMessage); message  WM_KILLFOCUS;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetRegularBack(Value: TRegularBack);
    function GetListRect: TRect;
    procedure SetGridLines(Value: TGridLines);
    function GetItemsArray: TacPointerList;
    procedure QuickListSort(SortList: TacPointerList; L, R: Integer; ColumnIndex: integer);
    procedure SetItemWidth(Value: TItemWidth);
    procedure SetCheckBoxes(Value: TListCheckBoxes);
    procedure SetCheckOnClik(Value: boolean);
    procedure SetAllowSelection(Value: boolean);
    procedure SetCaptionOnEmpty(Value: string);
  protected
    ListSW : TacScrollWnd;
    procedure Paint; override;
    procedure Resize; override;
    procedure PaintGridLines(GridLines: TGridLines; Canvas: TCanvas; Region: TRect);
    procedure PaintSelectionFrame(Bitmap: TBitmap; Region: TRect);
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DblClick; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure ChangeScale(M: Integer; D: Integer); override;
  public
    procedure AfterConstruction; override; // v7
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetElementAt(MousePos: TPoint): TacListElement;
    function CompareItems(Item1, Item2: TacListItem; ColumnIndex: integer): integer;
    function GetUpperVisibleElement(Element: TacListElement): TacListElement;
    function GetBottomVisibleElement(Element: TacListElement): TacListElement;
    procedure SortItems(SortKind: TSortKind; ColumnIndex: integer; DataType: TDataType);
    property SelectedItem: TacListItem read GetSelectedItem write SetSelectedItem;
    property CheckedItem: TacListItem read GetCheckedItem write SetCheckedItem;
    property FocusedItem: TacListItem read GetFocusedItem;
    property FocusedGroup: TacListGroup read GetFocusedGroup;
    function SelectItems(FromItem, ToItem: TacListItem): integer;
    function ItemByCaption(const Caption : string; SelectIt : boolean = False) : TacListItem;
    procedure BeginUpdate;
    procedure EndUpdate(Repaint: boolean = true);
    procedure ScrollToElement(Element: TacListElement);
    procedure Clear;
    procedure UnSelectAll;
    procedure SelectAll;
    procedure UnCheckAll;
    procedure ExpandAll(Repaint : boolean);
    procedure CollapseAll(Repaint : boolean);
  published
    //Additional
    property Groups: TacListGroups read FListGroups write SetListGroups;
    property Columns: TacListColumns read FListColumns write SetListColumns;
    property Items: TacListItems read FListItems write SetListItems;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ShowGroupItemsCount: boolean read FShowGroupItemsCount write SetShowGroupItemsCount default true;
    property ShowColumnHeaders: boolean read FShowColumnHeaders write SetShowColumnHeaders default true;
    property ShowGroups: boolean read FShowGroups write SetShowGroups default true;
    property ShowGroupButtons: boolean read FShowGroupButtons write SetShowGroupButtons default true;
    property SpaceKeyAction: TSpaceKeyAction read FSpaceKeyAction write SetSpaceKeyAction default skDoInvertCheckFocused;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property GroupImages: TCustomImageList read FGroupImages write SetGroupImages;
    property GroupHeight: integer read FGroupHeight write SetGroupHeight default 20;
    property ColumnHeight: integer read FColumnHeight write SetColumnHeight default 20;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 20;
    property HotItemColor: TColor read FHotItemColor write SetHotItemColor default $00F8EBE4;
    property SelectItemColor: TColor read FSelectItemColor write SetSelectItemColor default $00EFD3C6;
    property RegularItemColor: TColor read FRegularItemColor write SetRegularItemColor default clWindow;
    property ProgressItemColor: TColor read FProgressItemColor write SetProgressItemColor default clGreen;
    property GridColor: TColor read FGridColor write SetGridColor default cl3DLight;
    property SelectionFrameColor: TColor read FSelectionFrameColor write SetSelectionFrameColor default clHighLight;
    property SkinData: TsScrollWndData read FSkinData write SetSkinData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property GroupSkin: string read FGroupSkin write SetGroupSkin;
    property ColumnSkin: string read FColumnSkin write SetColumnSkin;
    property ItemSkin: string read FItemSkin write SetItemSkin;
    property GroupFont: TFont read FGroupFont write SetGroupFont;
    property ColumnFont: TFont read FColumnFont write SetColumnFont;
    property ItemFont: TFont read FItemFont write SetItemFont;
    property ListRect: TRect read GetListRect;
    property GridLines: TGridLines read FGridLines write SetGridLines default glNone;
    property RegularBack: TRegularBack read FRegularBack write SetRegularBack default rbEven;
    property ItemWidth: TItemWidth read FItemWidth write SetItemWidth default iwColumnsWidth;
    property CheckBoxes: TListCheckBoxes read FCheckBoxes write SetCheckBoxes default lcNone;
    property CheckOnClik: boolean read FCheckOnClik write SetCheckOnClik default false;
    property AllowSelection: boolean read FAllowSelection write SetAllowSelection default true;
    property CaptionOnEmpty: string read FCaptionOnEmpty write SetCaptionOnEmpty;
    property OnGetGroupCount: TGetCountEvent read FGetCountEvent write FGetCountEvent;
    //Standart
    property Action;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF D2005} // v7
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses {$IFDEF DELPHI6UP}Types, {$ENDIF}sGradient, math;

function RectIntersecRect(Rect1, Rect2: TRect): boolean;
begin
  Result := (
              (
                Between(Rect1.Left, Rect2.Left, Rect2.Right) or
                Between(Rect1.Right, Rect2.Left, Rect2.Right)
              )
              and
              (
                Between(Rect1.Top, Rect2.Top, Rect2.Bottom) or
                Between(Rect1.Bottom, Rect2.Top, Rect2.Bottom) or
                ((Rect1.Bottom < Rect2.Top) and (Rect1.Top > Rect2.Bottom))
              )
            )
            or
            (
              (
                Between(Rect2.Left, Rect1.Left, Rect1.Right) or
                Between(Rect2.Right, Rect1.Left, Rect1.Right)
              )
              and
              (
                Between(Rect2.Top, Rect1.Top, Rect1.Bottom) or
                Between(Rect2.Bottom, Rect1.Top, Rect1.Bottom) or
                ((Rect2.Top < Rect1.Top) and (Rect2.Bottom > Rect1.Bottom))
              )
            );
end;

procedure DrawCheck(Canvas: TCanvas; Bounds: TRect; State: integer);
var
  NowBrushColor: TColor;
begin
  Canvas.Rectangle(Bounds);
  case State of
    1 : begin
          Canvas.MoveTo(Bounds.Left + 2, Bounds.Top - 1 + HeightOf(Bounds) div 2);
          Canvas.LineTo(Bounds.Left + WidthOf(Bounds) div 3, Bounds.Bottom - 3);
          Canvas.LineTo(Bounds.Right - 2, Bounds.Top + 2);

          Canvas.MoveTo(Bounds.Left + 2, Bounds.Top - 2 + HeightOf(Bounds) div 2);
          Canvas.LineTo(Bounds.Left + WidthOf(Bounds) div 3, Bounds.Bottom - 4);
          Canvas.LineTo(Bounds.Right - 2, Bounds.Top + 1);
        end;
    2 : begin
          NowBrushColor := Canvas.Brush.Color;
          Canvas.Brush.Color := Canvas.Pen.Color;
          Canvas.FillRect(Rect(Bounds.Left + 3, Bounds.Top + 3,
                               Bounds.Right - 3, Bounds.Bottom - 3));
          Canvas.Brush.Color:= NowBrushColor;
        end;
  end;
end;

procedure DrawRadio(Canvas: TCanvas; Bounds: TRect; State: integer);
var
  NowBrushColor: TColor;
begin
  Canvas.Ellipse(Bounds);
  if State = 1 then begin
    NowBrushColor := Canvas.Brush.Color;
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.Ellipse(Rect(Bounds.Left + 3, Bounds.Top + 3,
                         Bounds.Right - 3, Bounds.Bottom - 3));
    Canvas.Brush.Color:= NowBrushColor;
  end;
end;

procedure DrawExpandBtn(Canvas: TCanvas; Bounds: TRect; Expanded: boolean);
begin
  Canvas.Rectangle(Bounds);
  Canvas.MoveTo(Bounds.Left + 2, Bounds.Top + HeightOf(Bounds) div 2);
  Canvas.LineTo(Bounds.Right - 2, Bounds.Top + HeightOf(Bounds) div 2);

  if not Expanded then begin
    Canvas.MoveTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Top + 2);
    Canvas.LineTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Bottom - 2);
  end;
end;

procedure DrawArrow(Canvas: TCanvas; Bounds: TRect; SortKind: TSortKind);
begin
  if SortKind = skASC then begin
    Canvas.MoveTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Top);
    Canvas.LineTo(Bounds.Left, Bounds.Bottom);
    Canvas.LineTo(Bounds.Right, Bounds.Bottom);
    Canvas.LineTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Top);
  end
  else if SortKind = skDESC then begin
    Canvas.MoveTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Bottom);
    Canvas.LineTo(Bounds.Left, Bounds.Top);
    Canvas.LineTo(Bounds.Right, Bounds.Top);
    Canvas.LineTo(Bounds.Left + WidthOf(Bounds) div 2, Bounds.Bottom);
  end
end;

procedure DrawText(Bitmap: TBitmap; Text: string; Rect: TRect; HTMLMode: boolean; SkinIndex: integer; Hot: boolean; SkinManager : TsSkinManager);
var
  sHTML: TsHtml;
  Links: TacLinks;
begin
  if HTMLMode then begin
    sHTML := TsHtml.Create;
    sHTML.Init(Bitmap, Text, Rect);
    sHTML.HtmlText(Links);
    FreeAndNil(sHTML);
  end
  else if SkinManager.IsValidSkinIndex(SkinIndex)
    then WriteTextEx(Bitmap.Canvas, PChar(Text), true, Rect, DT_LEFT, SkinIndex, Hot, SkinManager)
    else Bitmap.Canvas.TextOut(Rect.Left, Rect.Top, Text);
end;

{$IFDEF D2005}  // v7
{$region 'TacListElement'}
{$ENDIF}

constructor TacListElement.Create(Collection: TCollection);
begin
  FOrderTag := Collection.Count;

  if Collection is TacListGroups then
    FOwner := TacListGroups(Collection).Owner
  else if Collection is TacListColumns then
    FOwner := TacListColumns(Collection).Owner
  else if Collection is TacListItems then
    FOwner := TacListItems(Collection).Owner;

  inherited Create(Collection);

  FVisible := true;
  FShowHint := false;
  FAllowReorder := false;
  FHint := '';
end;

function TacListElement.GetPrevVisible: TacListElement;
var
  i: Integer;
begin
  Result := nil;
  for i := Index - 1 downto 0 do
    if TacListElement(Collection.Items[i]).Visible then begin
      Result := TacListElement(Collection.Items[i]);
      Break;
    end;
end;

function TacListElement.GetNextVisible: TacListElement;
var
  i: Integer;
begin
  Result := nil;
  for i := Index + 1 to Collection.Count-1 do
    if TacListElement(Collection.Items[i]).Visible then begin
      Result := TacListElement(Collection.Items[i]);
      Break;
    end;
end;

procedure TacListElement.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    DoChange;
  end;
end;

procedure TacListElement.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
    if not ((Self is TacListColumn) and (TacListColumn(Self).Index = 0)) then begin
      FVisible := Value;
      DoChange;
    end;
end;

procedure TacListElement.PaintGridLines(ElementCanvas, ListViewCanvas: TCanvas; PaintBounds: TRect);
var
  GridRect: TRect;
begin
  if (KssListView.GridLines = glVertical) or (KssListView.GridLines = glBoth) then begin
    GridRect := ElementCanvas.ClipRect;
    GridRect.Left := GridRect.Left - PaintBounds.Left;
    KssListView.PaintGridLines(glVertical, ElementCanvas, GridRect);
  end;
  if (KssListView.GridLines = glHorizontal) or (KssListView.GridLines = glBoth) then begin
    GridRect := PaintBounds;
    GridRect.Right := KssListView.ClientWidth;
    KssListView.PaintGridLines(glHorizontal, ListViewCanvas, GridRect);
  end;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListGroup'}
{$ENDIF}

constructor TacListGroup.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFooter := '';
  FExpanded := true;
  FImageIndex := -1;
  FCaption := 'Group ' + IntToStr(FOrderTag + 1);
  FCheckBoxes := gcInherit;
end;

destructor TacListGroup.Destroy;
var
  LG: TacListGroups;
begin
  LG := TacListGroups(Collection);
  inherited Destroy;
  LG.UpdateGroups;
end;

procedure TacListGroup.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

procedure TacListGroup.DoChange;
begin
  TacListGroups(Collection).UpdateGroups;
end;

procedure TacListGroup.SetIndex(Value: Integer);
var
  GroupOrder: array of Integer;
  I: Integer;
begin
  inherited SetIndex(Value);
  SetLength(GroupOrder, Collection.Count);
  for I := 0 to Collection.Count - 1 do
    GroupOrder[I] := TacListGroup(Collection.Items[I]).FOrderTag;
  DoChange;
end;

function  TacListGroup.GetButtonRect: TRect;
begin
  Result := Rect(FBounds.Left, FBounds.Top, FBounds.Left, FBounds.Top);

  if KssListView.FShowGroupButtons then begin
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 1) then
      Result := Rect(FBounds.Left, FBounds.Top, FBounds.Left + KssListView.StateImages.Width, FBounds.Top + KssListView.StateImages.Height)
    else
      Result := Rect(FBounds.Left + 3, FBounds.Top + (KssListView.GroupHeight - 9) div 2, FBounds.Left + 17, FBounds.Top + 9 + (KssListView.GroupHeight - 9) div 2)
  end
  else
    Result := Rect(FBounds.Left + 3, FBounds.Top + (KssListView.GroupHeight - 9) div 2, FBounds.Left + 9, FBounds.Top + 9 + (KssListView.GroupHeight - 9) div 2);
end;

procedure TacListGroup.CalcBounds(var Pos: TPoint);
begin
  FBounds := Rect(Pos.X, Pos.Y, KssListView.ClientWidth, Pos.Y + KssListView.GroupHeight);
  Pos.Y := Pos.Y + KssListView.GroupHeight;
  if (KssListView.GridLines = glBoth) or (KssListView.GridLines = glHorizontal) then
    Pos.Y := Pos.Y + 1;
end;

procedure TacListGroup.CalcFooterBounds(var Pos: TPoint);
begin
  FFooterBounds := Rect(Pos.X, Pos.Y, KssListView.ClientWidth, Pos.Y + KssListView.ItemHeight);
  Pos.Y := Pos.Y + KssListView.ItemHeight;
  if (KssListView.GridLines = glBoth) or (KssListView.GridLines = glHorizontal) then
    Pos.Y := Pos.Y + 1;
end;

{$IFNDEF DELPHI6UP}
function GetShadowColor(const Color: TColor; Luminance: Integer): TColor;
begin
  Result := BlendColors(clBlack, Color, abs(Luminance) * MAXBYTE div 100);
end;
{$ENDIF}

procedure TacListGroup.Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean);
var
  OutBitmap, BackBitmap: TBitmap;
  BitmapRect, TxtRec, DesRec, LineRect, ClearRect, ImgRec: TRect;
  OutText, OutDesText, CountString: string;
  OutCanvas: TCanvas;
  ci: TCacheInfo;
  SkinSections: TStringList;
  OffsetX, SkinIndex, ItemIndex, CheckIndex, DividerSkinIndex, State: integer;
  FontHeightDif, DesFontHeightDif, LabelsTop: integer;
  CheckSection: string;
begin
  if not FVisible then
    Exit;

  State := 0;
  ItemIndex := -1;
  DividerSkinIndex := -1;
  CheckIndex := -1;

  BitmapRect := Rect(0, 0, WidthOf(FBounds, true), HeightOf(FBounds));
  OutBitmap := CreateBmp32(BitmapRect.Right, BitmapRect.Bottom);

  OutCanvas := OutBitmap.Canvas;
  OutCanvas.Font := KssListView.GroupFont;
  FontHeightDif := Abs(OutCanvas.Font.Height) - OutCanvas.Font.Size;
  DesFontHeightDif := Abs(KssListView.ItemFont.Height) - KssListView.ItemFont.Size;

  SkinIndex := -1;
  if KssListView.SkinData.SkinManager.IsValidSkinIndex(KssListView.SkinData.SkinIndex) then begin // v7
    SkinSections := TStringList.Create;
    KssListView.SkinData.SkinManager.GetSkinSections(SkinSections);
    SkinIndex := SkinSections.IndexOf(KssListView.GroupSkin);
    ItemIndex := SkinSections.IndexOf(KssListView.ItemSkin);
    DividerSkinIndex := SkinSections.IndexOf(s_DIVIDERV);
    SkinSections.Free;
    ci := MakeCacheInfo(KssListView.FSkinData.FCacheBmp, FBounds.Left, FBounds.Top);
  end;

  // Clear background
  if BgErase then begin
    if SkinIndex < 0 then begin
      OutCanvas.Brush.Style := bsSolid;
      OutCanvas.Brush.Color := KssListView.Color;
      OutCanvas.FillRect(BitmapRect);
    end else
      OutCanvas.CopyRect(OutCanvas.ClipRect, KssListView.FSkinData.FCacheBmp.Canvas,
                      Rect(FBounds.Left+1, FBounds.Top+2,
                           FBounds.Right+1, FBounds.Bottom+2)
                     );
  end;

  OffsetX := -FBounds.Left + GetButtonRect.Right;

  // Checkbox rect
  if GetCheckBoxesFinal <> gcNone then
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 5) then begin
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.GroupHeight - KssListView.StateImages.Height) div 2,
                     OffsetX + KssListView.StateImages.Width,
                     BitmapRect.Top + KssListView.GroupHeight - (KssListView.GroupHeight - KssListView.StateImages.Height) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end
    else if SkinIndex < 0 then begin
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.GroupHeight - 12) div 2,
                     OffsetX + 12,
                     BitmapRect.Top + KssListView.GroupHeight - (KssListView.GroupHeight - 12) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end
    else begin
      if GetCheckBoxesFinal = gcCheck then begin
        case FCheckState of
          cbUnchecked : CheckSection := s_CheckBoxUnChecked;
          cbChecked   : CheckSection := s_CheckBoxChecked;
          cbGrayed    : CheckSection := s_CheckBoxGrayed;
        end;
      end
      else if GetCheckBoxesFinal = gcRadio then begin
        if Checked then
          CheckSection := s_RadioButtonChecked
        else
          CheckSection := s_RadioButtonUnChecked;
      end;
      with KssListView.SkinData do begin
        CheckIndex := SkinManager.GetMaskIndex(SkinManager.SkinCommonInfo.IndexGlobalInfo, s_GlobalInfo, CheckSection, CommonSkinData);
        ClearRect.Left := WidthOf(CommonSkinData.ma[CheckIndex].R) div CommonSkinData.ma[CheckIndex].ImageCount;
        ClearRect.Top := HeightOf(CommonSkinData.ma[CheckIndex].R) div (CommonSkinData.ma[CheckIndex].MaskType + 1);
      end;
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.GroupHeight - ClearRect.Top) div 2,
                     OffsetX + ClearRect.Left,
                     BitmapRect.Top + KssListView.GroupHeight - (KssListView.GroupHeight - ClearRect.Top) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end;

  // Image rect
  ImgRec := Rect(OffsetX, 0, OffsetX, 0);
  if (FImageIndex >= 0) and Assigned(KssListView.GroupImages) and (KssListView.GroupImages.Count > 0) then begin
    ImgRec := Rect(OffsetX,
                   BitmapRect.Top + (KssListView.GroupHeight - KssListView.GroupImages.Height) div 2,
                   OffsetX + KssListView.GroupImages.Width,
                   BitmapRect.Top + KssListView.GroupHeight - (KssListView.GroupHeight - KssListView.GroupImages.Height) div 2
                  );
    OffsetX := ImgRec.Right + 3;
  end;

  // Text rect
  if Description <> '' then
    LabelsTop := BitmapRect.Top - (FontHeightDif + DesFontHeightDif) + (KssListView.GroupHeight - (OutCanvas.Font.Size + KssListView.ItemFont.Size + 1)) div 2
  else
    LabelsTop := BitmapRect.Top - FontHeightDif + (KssListView.GroupHeight - OutCanvas.Font.Size) div 2;

  TxtRec := Rect(OffsetX + 2, LabelsTop, BitmapRect.Right, LabelsTop + abs(OutCanvas.Font.Height) + FontHeightDif);
  DesRec := Rect(OffsetX + 2, TxtRec.Bottom + 1, KssListView.ClientWidth, TxtRec.Bottom + 1 + abs(KssListView.ItemFont.Height) + DesFontHeightDif);

  OutCanvas.Font := KssListView.GroupFont;

  OutText := Caption;
  if KssListView.ShowGroupItemsCount then begin
    if Assigned(KssListView.FGetCountEvent) then begin
      CountString := '';
      KssListView.FGetCountEvent(Self, CountString);
      OutText := OutText + ' (' + CountString + ')'
    end
    else
      OutText := OutText + ' (' + IntToStr(Count) + ')';
  end;
  OutText := CutText(OutCanvas, OutText, KssListView.ClientWidth - 10);

  // Line rect
  LineRect := Rect(TxtRec.Left + GetStringSize(KssListView.GroupFont.Handle, OutText).cx + 4, TxtRec.Top + 7, TxtRec.Right - 4, TxtRec.Top + 9);

  OutCanvas.Font := KssListView.ItemFont;
  OutDesText := CutText(OutCanvas, Description, KssListView.ClientWidth - 10);

  // Hot and selected item
  if (Self = KssListView.FHotElement) and Selected then
    if SkinIndex < 0 then begin
      OutCanvas.Brush.Color := AverageColor(KssListView.HotItemColor, KssListView.SelectItemColor);
      OutCanvas.Pen.Color := BlendColors(OutCanvas.Brush.Color, clBlack, 127);
    end
    else
      State := 3
  // Hot item
  else
    if Self = KssListView.FHotElement then
      if SkinIndex < 0 then begin
        OutCanvas.Brush.Color := KssListView.HotItemColor;
        OutCanvas.Pen.Color := BlendColors(KssListView.HotItemColor, clBlack, 127);
      end
      else
        State := 1
    // Selected item
    else
      if Selected then
        if SkinIndex < 0 then begin
          OutCanvas.Brush.Color := KssListView.SelectItemColor;
          OutCanvas.Pen.Color := BlendColors(KssListView.SelectItemColor, clBlack, 127);
        end
        else
          State := 2
      // Normal item
      else
        if SkinIndex < 0 then begin
          OutCanvas.Pen.Color := KssListView.Color;
          OutCanvas.Brush.Style := bsClear;
        end
        else
          State := 0;

  if SkinIndex < 0 then begin
    OutCanvas.Rectangle(BitmapRect);
    PaintGridLines(OutCanvas, Canvas, FBounds);
    OutCanvas.Font := KssListView.GroupFont;
    OutCanvas.TextOut(TxtRec.Left, TxtRec.Top, OutText);
    OutCanvas.Font := KssListView.ItemFont;
    OutCanvas.TextOut(DesRec.Left, DesRec.Top, OutDesText);
    // Line
    OutCanvas.Pen.Color := BlendColors(OutCanvas.Pen.Color, clBlack, 127);
    OutCanvas.MoveTo(LineRect.Left, LineRect.Top);
    OutCanvas.LineTo(LineRect.Right, LineRect.Top);
  end
  else begin
    ci.Bmp := OutBitmap;
    ci.X := 0;
    ci.Y := 0;
    if Selected then
      PaintItem(SkinIndex, ci, true, State, BitmapRect, Point(1, 1), OutCanvas.Handle, DefaultManager.CommonSkinData);

    if Self = KssListView.FHotElement then begin
      BackBitmap := TBitmap.Create;
      BackBitmap.Assign(OutBitmap);
      PaintItem(SkinIndex, ci, true, State, BitmapRect, Point(1, 1), BackBitmap.Canvas.Handle, DefaultManager.CommonSkinData);
      BlendTransRectangle(OutBitmap, 0, 0, BackBitmap, BitmapRect, byte(127));
      BackBitmap.Free;
    end;
    PaintGridLines(OutCanvas, Canvas, FBounds);
    OutCanvas.Font := KssListView.GroupFont;
    WriteTextEx(OutCanvas, PChar(OutText), true, TxtRec, DT_LEFT, SkinIndex, Boolean(State > 0));
    OutCanvas.Font := KssListView.ItemFont;
    WriteTextEx(OutCanvas, PChar(OutDesText), true, DesRec, DT_LEFT, ItemIndex, Boolean(State > 0));
    // Line
    PaintItem(DividerSkinIndex, ci, true, State, LineRect, Point(LineRect.Left, 0), OutCanvas.Handle, DefaultManager.CommonSkinData);
  end;

  // Expand button
  if KssListView.FShowGroupButtons then begin
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 1) then begin
      if FExpanded then
        KssListView.StateImages.Draw(OutCanvas, 0, BitmapRect.Top + (KssListView.GroupHeight - KssListView.StateImages.Height) div 2, 0, true)
      else
        KssListView.StateImages.Draw(OutCanvas, 0, BitmapRect.Top + (KssListView.GroupHeight - KssListView.StateImages.Height) div 2, 1, true)
    end
    else begin
      OutCanvas.Pen.Color := OutCanvas.Font.Color;
      DrawExpandBtn(OutCanvas, Rect(3, BitmapRect.Top + (KssListView.GroupHeight - 9) div 2, 12, BitmapRect.Top + 9 + (KssListView.GroupHeight - 9) div 2), FExpanded);
    end;
  end;

  // Draw checks
  if GetCheckBoxesFinal <> gcNone then
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 5) then
      if Checked then
        KssListView.StateImages.Draw(OutCanvas, FCheckBounds.Left, FCheckBounds.Top, 5, true)
      else
        KssListView.StateImages.Draw(OutCanvas, FCheckBounds.Left, FCheckBounds.Top, 4, true)
    else
      if SkinIndex < 0 then begin
        OutCanvas.Pen.Color := OutCanvas.Font.Color;
        if GetCheckBoxesFinal = gcCheck then
          DrawCheck(OutCanvas, FCheckBounds, integer(FCheckState))
        else
          DrawRadio(OutCanvas, FCheckBounds, integer(Checked));
      end
      else
        DrawSkinGlyph(OutBitmap, Point(FCheckBounds.Left, FCheckBounds.Top), 0, 1, KssListView.SkinData.CommonSkinData.ma[CheckIndex], MakeCacheInfo(OutBitmap));

  // Draw image
  if Assigned(KssListView.GroupImages) and (ImageIndex > -1) then begin
    if (KssListView.GroupImages.Count >= ImageIndex) then
      KssListView.GroupImages.Draw(OutCanvas, ImgRec.Left, ImgRec.Top, ImageIndex, true)
  end;

  // Selection Frame
  if KssListView.FSelectionFrame and PaintSelectionFrame then
    KssListView.PaintSelectionFrame(OutBitmap, FBounds);

  if FBounds.Top < KssListView.GetListRect.Top then begin
    ClearRect.Left := FBounds.Left;
    ClearRect.Right := FBounds.Right;
    ClearRect.Top := KssListView.GetListRect.Top;
    ClearRect.Bottom := ClearRect.Top + HeightOf(FBounds) - (KssListView.GetListRect.Top - FBounds.Top);

    BitmapRect.Top := KssListView.GetListRect.Top - FBounds.Top;
    Canvas.CopyRect(ClearRect, OutCanvas, BitmapRect);
  end
  else
    Canvas.Draw(FBounds.Left, FBounds.Top, OutBitmap);
    
  OutBitmap.Free;

  FCheckBounds.Top := FCheckBounds.Top + FBounds.Top;
  FCheckBounds.Bottom := FCheckBounds.Bottom + FBounds.Top;
  FCheckBounds.Left := FCheckBounds.Left + FBounds.Left;
  FCheckBounds.Right := FCheckBounds.Right + FBounds.Left;
end;

procedure TacListGroup.PaintFooter(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean);
var
  OutBitmap: TBitmap;
  BitmapRect,
  TxtRec,
  ClearRect: TRect;
  OutText: string;
  OutCanvas: TCanvas;
  FontHeightDif: integer;
begin
  BitmapRect := Rect(0, 0, WidthOf(FFooterBounds), HeightOf(FFooterBounds));
  OutBitmap := CreateBmp32(BitmapRect.Right, BitmapRect.Bottom);

  OutCanvas := OutBitmap.Canvas;
  OutCanvas.Font := KssListView.ItemFont;
  FontHeightDif := Abs(OutCanvas.Font.Height) - OutCanvas.Font.Size;

  // Clear background
  if BgErase then begin
    if KssListView.SkinData.SkinIndex < 0 then begin
      OutCanvas.Brush.Style := bsSolid;
      OutCanvas.Brush.Color := KssListView.Color;
      OutCanvas.FillRect(BitmapRect);
    end else
      OutCanvas.CopyRect(OutCanvas.ClipRect, KssListView.FSkinData.FCacheBmp.Canvas,
                      Rect(FFooterBounds.Left+1, FFooterBounds.Top+2,
                           FFooterBounds.Right+1, FFooterBounds.Bottom+2)
                     );
  end;

  // Text rect
  TxtRec := Rect(2, BitmapRect.Top - FontHeightDif + (KssListView.ItemHeight - OutCanvas.Font.Size) div 2, KssListView.ClientWidth, BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - Abs(OutCanvas.Font.Height)) div 2);

  OutText := Footer;
  OutText := CutText(OutCanvas, OutText, KssListView.ClientWidth-10);

  PaintGridLines(OutCanvas, Canvas, FFooterBounds);
  OutCanvas.Brush.Style := bsClear;
  OutCanvas.TextOut(TxtRec.Left, TxtRec.Top, OutText);

  if FBounds.Top < KssListView.GetListRect.Top then begin
    ClearRect.Left := FFooterBounds.Left;
    ClearRect.Right := FFooterBounds.Right;
    ClearRect.Top := KssListView.GetListRect.Top;
    ClearRect.Bottom := ClearRect.Top + HeightOf(FFooterBounds) - (KssListView.GetListRect.Top - FFooterBounds.Top);

    BitmapRect.Top := KssListView.GetListRect.Top - FFooterBounds.Top;
    Canvas.CopyRect(ClearRect, OutCanvas, BitmapRect);
  end
  else
    Canvas.Draw(FFooterBounds.Left, FFooterBounds.Top, OutBitmap);

  OutBitmap.Free;
end;

procedure TacListGroup.SetSelectedItem(Value: TacListItem);
begin
  KssListView.SelectedItem := Value;
end;

procedure TacListGroup.SetSelected(Value: boolean);
var
  i: integer;
begin
  if KssListView.AllowSelection then begin
    if KssListView.MultiSelect then begin
      KssListView.UnSelectAll;
      for i := 0 to KssListView.Items.Count - 1 do begin
        if KssListView.Items[i].Group = Self then begin
          KssListView.Items[i].FSelected := true;
        end;
      end;
      DoChange;
    end
    else
      KssListView.SelectedItem := GetItem(0);
  end;
end;

function  TacListGroup.GetSelectedItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      if KssListView.Items[i].Selected then begin
        Result := KssListView.Items[i];
        Break;
      end;
    end;
  end;
end;

function TacListGroup.Add(Caption: string = ''; ImageIndex: integer = -1): TacListItem;
begin
  Result := KssListView.Items.Add(Caption, ImageIndex);
  Result.Group := Self;
end;

function TacListGroup.GetCheckBoxesFinal: TGroupCheckBoxes;
begin
  Result := FCheckBoxes;
  if Result = gcInherit then
    Result := TGroupCheckBoxes(KssListView.CheckBoxes);
end;

procedure TacListGroup.SetCheckBoxes(Value: TGroupCheckBoxes);
begin
  if FCheckBoxes <> Value then begin
    FCheckBoxes := Value;
    DoChange;
  end;
end;

function  TacListGroup.GetChecked: boolean;
begin
  Result := false;
  if FCheckState = cbChecked then
    Result := true;
end;

procedure TacListGroup.SetChecked(Value: boolean);
var
  i: integer;
  Changed: boolean;
begin
  Changed := False;
  if Value and (FCheckState <> cbChecked) then begin
    FCheckState := cbChecked;
    Changed := true;
  end;

  if not Value and (FCheckState <> cbUnChecked) then begin
    FCheckState := cbUnChecked;
    Changed := true;
  end;
  
  if Changed then begin
    KssListView.BeginUpdate;
    if (FCheckState = cbChecked) and (GetCheckBoxesFinal = gcRadio) and (Count > 0) then
      GetItem(0).FChecked := true
    else
      for i := 0 to KssListView.Items.Count - 1 do
        if KssListView.Items[i].Group = Self then
          KssListView.Items[i].FChecked := Checked;
    KssListView.EndUpdate;

    DoChange;
  end;
end;

procedure TacListGroup.UpdateCheck;
var
  i: integer;
  CheckCount, Count: integer;
begin
  Count := 0;
  CheckCount := 0;
  for i := 0 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      inc(Count);
      if KssListView.Items[i].Checked then
        inc(CheckCount);
    end;
  end;

  if CheckCount = Count then
    FCheckState := cbChecked
  else
  if (CheckCount > 0) and (GetCheckBoxesFinal = gcRadio) then
    FCheckState := cbChecked
  else
  if CheckCount > 0 then
    FCheckState := cbGrayed
  else
    FCheckState := cbUnChecked;
end;

function  TacListGroup.GetSelected: boolean;
begin
  Result := false;
  if Assigned(SelectedItem) and not Expanded then Result := true;
end;

procedure TacListGroup.Click(MousePos: TPoint);
begin
  if PtInRect(GetButtonRect, MousePos) then
    Expanded := not Expanded
  else
  if PtInRect(FCheckBounds, MousePos) then
    Checked := not Checked
  else
    Selected := true;
end;

procedure TacListGroup.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

procedure TacListGroup.SetFooter(const Value: string);
begin
  if FFooter <> Value then
  begin
    FFooter := Value;
    DoChange;
  end;
end;

procedure TacListGroup.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    DoChange;
  end;
end;

procedure TacListGroup.SetExpanded(const Value: boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    DoChange;
  end;
end;

procedure TacListGroup.Assign(Source: TPersistent);
var
  Group: TacListGroup;
begin
  if Source is TacListGroup then
  begin
    Group := TacListGroup(Source);
    Caption := Group.Caption;
    ImageIndex := Group.ImageIndex;
  end
  else inherited Assign(Source);
end;

function TacListGroup.GetItem(Index: Integer): TacListItem;
var
  i: integer;
  iCount: integer;
begin
  Result := nil;
  iCount := 0;
  for i := 0 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      if iCount = Index then begin
        Result := KssListView.Items[i];
        Break;
      end;
      inc(iCount);
    end;
  end;
end;

function TacListGroup.GetNextVisibleItem(Item: TacListItem): TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := Item.Index + 1 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      if KssListView.Items[i].Visible then begin
        Result := KssListView.Items[i];
        Break;
      end;
    end;
  end;
end;

function TacListGroup.GetPrevVisibleItem(Item: TacListItem): TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := Item.Index - 1 downto 0 do begin
    if KssListView.Items[i].Group = Self then begin
      if KssListView.Items[i].Visible then begin
        Result := KssListView.Items[i];
        Break;
      end;
    end;
  end;
end;

function TacListGroup.FirstItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      Result := KssListView.Items[i];
      Break;
    end;
  end;
end;

function TacListGroup.FirstVisibleItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to KssListView.Items.Count - 1 do begin
    if KssListView.Items[i].Group = Self then begin
      if KssListView.Items[i].Visible then begin
        Result := KssListView.Items[i];
        Break;
      end;
    end;
  end;
end;

function TacListGroup.LastItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := KssListView.Items.Count - 1 downto 0 do begin
    if KssListView.Items[i].Group = Self then begin
      Result := KssListView.Items[i];
      Break;
    end;
  end;
end;

function TacListGroup.LastVisibleItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := KssListView.Items.Count - 1 downto 0 do begin
    if KssListView.Items[i].Group = Self then begin
      if KssListView.Items[i].Visible then begin
        Result := KssListView.Items[i];
        Break;
      end;
    end;
  end;
end;

function TacListGroup.Count: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to KssListView.Items.Count - 1 do
    if KssListView.Items[i].Group = Self then
      inc(Result);
end;

function TacListGroup.Count(ItemAttributes: TItemAttributes): integer;
var
  i: integer;
  Pass: boolean;
begin
  Result := 0;
  for i := 0 to KssListView.Items.Count - 1 do
    if KssListView.Items[i].Group = Self then begin
      Pass := true;
      if (iaSelect in ItemAttributes) and not KssListView.Items[i].Selected then
        Pass := false;

      if (iaCheck in ItemAttributes) and not KssListView.Items[i].Checked then
        Pass := false;

      if (iaVisible in ItemAttributes) and not KssListView.Items[i].Visible then
        Pass := false;

      if (iaGroupVisible in ItemAttributes) and not Visible then
        Pass := false;

      if Pass then
        inc(Result);
    end;
end;

function TacListGroup.SelectItems(FromItem, ToItem: TacListItem): integer;
var
  i: Integer;
  MinIndex,
  MaxIndex: Integer;
begin
  Result := 0;
  if FromItem = ToItem then begin
    FromItem.Selected := true;
    Result := 1;
  end
  else if not Assigned(FromItem) then begin
    ToItem.Selected := true;
    Result := 1;
  end
  else if not Assigned(ToItem) then begin
    FromItem.Selected := true;
    Result := 1;
  end
  else if not KssListView.Multiselect then begin
    ToItem.Selected := true;
    Result := 1;
  end
  else if ToItem.Group <> Self then begin
    Result := SelectItemsToLast(FromItem);
  end
  else if FromItem.Group <> Self then begin
    Result := SelectItemsToFirst(ToItem);
  end
  else begin
    MinIndex := FromItem.Index;
    MaxIndex := ToItem.Index;
    if MinIndex > MaxIndex then begin
      MinIndex := ToItem.Index;
      MaxIndex := FromItem.Index;
    end;
    for i := MinIndex to MaxIndex do begin
      if (KssListView.Items[i].Group = Self) then begin
        KssListView.Items[i].FSelected := true;
        inc(Result);
      end;
    end;
    DoChange;
  end;
end;

function TacListGroup.SelectItemsToLast(FromItem: TacListItem): integer;
var
  i: integer;
begin
  Result := 0;
  if KssListView.MultiSelect then begin
    for i := FromItem.Index to KssListView.Items.Count - 1 do begin
      if (KssListView.Items[i].Group = Self) then begin
        KssListView.Items[i].FSelected := true;
        inc(Result);
      end;
    end;
    DoChange;
  end
  else begin
    KssListView.SelectedItem := FromItem;
    Result := 1;
  end;
end;

function TacListGroup.SelectItemsToFirst(FromItem: TacListItem): integer;
var
  i: integer;
begin
  Result := 0;
  if KssListView.MultiSelect then begin
    for i := 0 to FromItem.Index do begin
      if (KssListView.Items[i].Group = Self) then begin
        KssListView.Items[i].FSelected := true;
        inc(Result);
      end;
    end;
    DoChange;
  end
  else begin
    KssListView.SelectedItem := FromItem;
    Result := 1;
  end;
end;

procedure TacListGroup.SelectAll;
var
  i: integer;
begin
  if KssListView.MultiSelect then begin
    for i := 0 to KssListView.Items.Count - 1 do begin
      if KssListView.Items[i].Group = Self then begin
        KssListView.Items[i].FSelected := true;
      end;
    end;
    DoChange;
  end
  else
    KssListView.SelectedItem := GetItem(0);
end;

function TacListGroup.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

{$IFDEF D2005}  // v7
{$endregion}
{$region 'TacListColumn'}
{$ENDIF}

constructor TacListColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWidth := 100;
  FCaption := 'Column ' + IntToStr(FOrderTag + 1);
  FImageIndex := -1;
  FSortMode := smAutomatic;
  FSortKind := skNone;
  FAllowReorder := true;
end;

destructor TacListColumn.Destroy;
var
  LG: TacListColumns;
begin
  LG := TacListColumns(Collection);
  inherited Destroy;
  LG.UpdateColumns;
end;

procedure TacListColumn.DoChange;
begin
  TacListColumns(Collection).UpdateColumns;
end;

procedure TacListColumn.SetIndex(Value: Integer);
var
  ColumnOrder: array of Integer;
  I: Integer;
begin
  inherited SetIndex(Value);
  SetLength(ColumnOrder, Collection.Count);
  for I := 0 to Collection.Count - 1 do
    ColumnOrder[I] := TacListColumn(Collection.Items[I]).FOrderTag;
end;

procedure TacListColumn.Click(MousePos: TPoint);
begin
  if FSortMode = smAutomatic then
    case FSortKind of
      skASC  : SortKind := skDESC;
      skDESC : SortKind := skASC;
      skNone : SortKind := skASC;
    end;
end;

procedure TacListColumn.CalcBounds(var Pos: TPoint);
begin
  FBounds := Rect(Pos.X, Pos.Y, Pos.X + Width, Pos.Y + KssListView.ColumnHeight);
  Pos.X := Pos.X + Width;
end;

procedure TacListColumn.SetSortKind(Value: TSortKind);
var
  i: integer;
begin
  if FSortKind <> Value then
  begin
    if Value <> skNone then
      for i := 0 to KssListView.Columns.Count - 1 do
        KssListView.Columns[i].FSortKind := skNone;
    FSortKind := Value;
    KssListView.SortItems(FSortKind, Index, dtString);
    DoChange;
  end;
end;

procedure TacListColumn.Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean);
var
  OutBitmap: TBitmap;
  ci: TCacheInfo;
  BitmapRect: TRect;
  TxtRec: TRect;
  OutText: string;
  OutCanvas: TCanvas;
  SkinSections: TStringList;
  SkinIndex: integer;
  State: integer;
  FontHeightDif: integer;
  GradArray : TsGradArray;
begin
  if not Visible then Exit;

  State := 0;
  OutBitmap := CreateBmp32(WidthOf(FBounds, true), HeightOf(FBounds, True));
  BitmapRect := Rect(0, 0, OutBitmap.Width, OutBitmap.Height);
  OutCanvas := OutBitmap.Canvas;
  OutCanvas.Font := KssListView.ColumnFont;
  FontHeightDif := Abs(OutCanvas.Font.Height) - OutCanvas.Font.Size;

  SkinIndex := -1;
  if KssListView.SkinData.Skinned then begin
    SkinSections := TStringList.Create;
    KssListView.SkinData.SkinManager.GetSkinSections(SkinSections);
    SkinIndex := SkinSections.IndexOf(KssListView.ColumnSkin);
    if SkinIndex < 0 then
      SkinIndex := SkinSections.IndexOf(s_BUTTON);
    FreeAndNil(SkinSections);
  end;

  if BgErase then
    if SkinIndex < 0 then
      FillDC(OutCanvas.Handle, FBounds, KssListView.Color);

  OutText := CutText(OutCanvas, Caption, WidthOf(FBounds)-10);

  // Text rect
  TxtRec := Rect(5, BitmapRect.Top - FontHeightDif + (KssListView.ColumnHeight - OutCanvas.Font.Size) div 2, KssListView.ClientWidth, BitmapRect.Top + KssListView.ColumnHeight - (KssListView.ColumnHeight - Abs(OutCanvas.Font.Height)) div 2);

  // Hot item
  if Self = KssListView.FHotElement then begin
    if SkinIndex < 0 then begin
      OutCanvas.Brush.Color := KssListView.SelectItemColor;
      OutCanvas.Pen.Color := KssListView.SelectItemColor;
    end
    else begin
      State := 1;
    end;
  end
  // Normal item
  else begin
    if SkinIndex < 0 then begin
      OutCanvas.Pen.Color := clBtnFace;
      OutCanvas.Brush.Color := ColorToRGB(clBtnFace);
    end
    else begin
      State := 0;
    end;
  end;

  if SkinIndex < 0 then begin
    SetLength(GradArray, 2);

    GradArray[0].Color.C := ColorToRGB(OutCanvas.Brush.Color);
    GradArray[0].Mode := 0;
    GradArray[0].Percent := 0;

    GradArray[1].Color.C := ColorToRGB(KssListView.Color);
    GradArray[1].Mode := 0;
    GradArray[1].Percent := 100;

    PaintGrad(OutBitmap, BitmapRect, GradArray);
    OutCanvas.Brush.Style := bsClear;
    OutCanvas.Rectangle(BitmapRect);
    OutCanvas.TextRect(BitmapRect, TxtRec.Left, TxtRec.Top, OutText);
  end
  else begin
    ci := MakeCacheInfo(KssListView.FSkinData.FCacheBmp, FBounds.Left, FBounds.Top);
    PaintItem(SkinIndex, ci, true, State, BitmapRect, Point(1, 1), OutBitmap, DefaultManager.CommonSkinData);
    WriteTextEx(OutCanvas, PChar(OutText), true, TxtRec, DT_LEFT, SkinIndex, Boolean(State > 0));
  end;

  // Sort button
  if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 2) then begin
    if (FSortKind = skASC) then
      KssListView.StateImages.Draw(OutCanvas, BitmapRect.Left + (FWidth - KssListView.StateImages.Width) div 2, 0, 2, true)
    else
      if (FSortKind = skDESC) then
        KssListView.StateImages.Draw(OutCanvas, BitmapRect.Left + (FWidth - KssListView.StateImages.Width) div 2, 0, 3, true)
  end
  else begin
    OutCanvas.Pen.Color := OutCanvas.Font.Color;
    DrawArrow(OutCanvas, Rect(BitmapRect.Left + (FWidth - 6) div 2, 0, BitmapRect.Left + 6 + (FWidth - 6) div 2, 3), FSortKind);
  end;
  Canvas.Draw(FBounds.Left, FBounds.Top, OutBitmap);
  OutBitmap.Free;
end;

procedure TacListColumn.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

procedure TacListColumn.SetWidth(const Value: integer);
begin
  if FWidth <> Value then begin
    FWidth := max(30, Value);
    DoChange;
  end;
end;

procedure TacListColumn.Assign(Source: TPersistent);
var
  Column: TacListColumn;
begin
  if Source is TacListColumn then
  begin
    Column := TacListColumn(Source);
    Caption := Column.Caption;
    ImageIndex := Column.ImageIndex;
  end
  else inherited Assign(Source);
end;

procedure TacListColumn.SetOptimalWidth;
var
  i: Integer;
  MaxWidth,
  CurWidth: integer;
begin
  MaxWidth := 0;

  for i := 0 to KssListView.Items.Count - 1 do begin
    if Index = 0 then
      CurWidth := KssListView.Items[i].GetWidth
    else
      CurWidth := KssListView.Items[i].GetSubItemWidth(Index-1);

    if MaxWidth < CurWidth then
      MaxWidth := CurWidth;
  end;

  Width := MaxWidth;
end;

function TacListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListItem'}
{$ENDIF}

constructor TacListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSubItems := TStringList.Create;
  FSelected := false;
  FCaption := 'Item ' + IntToStr(FOrderTag + 1);
  FGroup := nil;
  FGroupIndex := -1;
  FImageIndex := -1;
  FStateIndex := -1;
  FShowProgress := false;
  FProgressPosition := 0;
  FChecked := false;
end;

destructor TacListItem.Destroy;
var
  LG: TacListItems;
begin
  LG := TacListItems(Collection);
  FSubItems.Free;
  inherited Destroy;
  LG.UpdateItems;
end;

procedure TacListItem.DoChange;
begin
  TacListItems(Collection).UpdateItems;
end;

procedure TacListItem.CalcBounds(var Pos: TPoint);
begin
  if (KssListView.ItemWidth = iwClientWidth) or (KssListView.Columns.Count = 0) then
    FBounds := Rect(Pos.X + 0, Pos.Y + 0, KssListView.ClientWidth, Pos.Y + 0 + KssListView.ItemHeight)
  else if KssListView.ItemWidth = iwColumnsWidth then
    FBounds := Rect(Pos.X + 0, Pos.Y + 0, KssListView.Columns.GetLastVisible.Bounds.Right-1, Pos.Y + 0 + KssListView.ItemHeight);

  Pos.Y := Pos.Y + KssListView.ItemHeight;
  if (KssListView.GridLines = glBoth) or (KssListView.GridLines = glHorizontal) then
    Pos.Y := Pos.Y + 1;
end;

procedure TacListItem.SetChecked(Value: boolean);
begin
  if FChecked <> Value then begin
    if (Value = false) or (GetCheckBoxesFinal = gcCheck) then
      FChecked := Value
    else begin
      KssListView.UnCheckAll;
      FChecked := Value
    end;
    if Assigned(Group) then
      Group.UpdateCheck;

    DoChange;
  end;
end;

function  TacListItem.GetHidden: boolean;
begin
  Result := not FVisible;
  if Assigned(Group) and KssListView.ShowGroups then
    if not Group.Visible or not Group.Expanded then
      Result := true;
end;

{$IFDEF DELPHI7UP}
  {$DEFINE USELUMIN}
{$ELSE}
  {$IFNDEF DELPHI6UP}
    {$DEFINE USELUMIN}
  {$ENDIF}
{$ENDIF}

procedure TacListItem.Paint(Canvas: TCanvas; BgErase, PaintSelectionFrame: boolean);
var
  OutBitmap: TBitmap;
  BackBitmap: TBitmap;
  BitmapRect: TRect;
  FullWidthRect: TRect;
  TxtRec: TRect;
  ImgRec: TRect;
  ClearRect: TRect;
  OffsetX: integer;
  FontHeightDif: integer;
  OutText: string;
  ci: TCacheInfo;
  pX, i: Integer;
  SkinSections: TStringList;
  SkinIndex: integer;
  ProgressSkinIndex: integer;
  CheckIndex: integer;
  State: integer;
  CheckSection: string;
  Color: TsColor;
  OutCanvas: TCanvas;
begin
  if not FVisible then Exit;
  if KssListView.Columns.Count = 0 then Exit;
  CheckIndex := -1;
  ProgressSkinIndex := -1;

  OutBitmap := CreateBmp32(WidthOf(FBounds)+1, HeightOf(FBounds));
  BitmapRect := Rect(0, 0, OutBitmap.Width, OutBitmap.Height);  // width +1 for grid
  FullWidthRect := Rect(FBounds.Left+1, FBounds.Top + 2, WidthOf(FBounds)+1, FBounds.Bottom+2);

  OutCanvas := OutBitmap.Canvas;
  OutCanvas.Font := KssListView.ItemFont;
  FontHeightDif := Abs(OutCanvas.Font.Height) - OutCanvas.Font.Size;

  SkinIndex := -1;
  if KssListView.SkinData.Skinned then begin
    SkinSections := TStringList.Create;
    KssListView.SkinData.SkinManager.GetSkinSections(SkinSections);
    SkinIndex := SkinSections.IndexOf(KssListView.ItemSkin);
    ProgressSkinIndex := SkinSections.IndexOf(s_PROGRESSH);
    FreeAndNil(SkinSections);
    ci.Bmp := KssListView.FSkinData.FCacheBmp;
    ci.X := FBounds.Left;
    ci.Y := FBounds.Top;
    ci.Ready := true;
  end;

  // Clear background
  if BgErase then
    if SkinIndex < 0 then begin
      ClearRect := BitmapRect;
      ClearRect.Right := KssListView.ClientWidth;
      FillDC(OutCanvas.Handle, ClearRect, KssListView.Color);
    end
    else
      OutCanvas.CopyRect(OutCanvas.ClipRect, KssListView.FSkinData.FCacheBmp.Canvas,
      Rect(FBounds.Left + 1, FBounds.Top + 2, FBounds.Right + 1, FBounds.Bottom + 2));

  if (KssListView.RegularBack = rbAll) or
       ((KssListView.RegularBack = rbEven) and (IndexInGroup mod 2 = 1)) or
       ((KssListView.RegularBack = rbOdd)  and (IndexInGroup mod 2 = 0)) then
    if SkinIndex < 0 then
      FillDC(OutCanvas.Handle, BitmapRect, KssListView.RegularItemColor)
    else
      PaintItem(SkinIndex, ci, true, 0, BitmapRect, Point(2, 2), OutCanvas.Handle, DefaultManager.CommonSkinData);

  if not Assigned(Group) or not KssListView.ShowGroups then
    OffsetX := 3
  else
    OffsetX := -FBounds.Left + Group.GetButtonRect.Right;

  // Checkbox rect
  if GetCheckBoxesFinal <> gcNone then
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 5) then begin
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.ItemHeight - KssListView.StateImages.Height) div 2,
                     OffsetX + KssListView.StateImages.Width,
                     BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - KssListView.StateImages.Height) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end
    else if SkinIndex < 0 then begin
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.ItemHeight - 12) div 2,
                     OffsetX + 12,
                     BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - 12) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end
    else begin
      if GetCheckBoxesFinal = gcCheck then begin
        if Checked then
          CheckSection := s_CheckBoxChecked
        else
          CheckSection := s_CheckBoxUnChecked;
      end
      else if GetCheckBoxesFinal = gcRadio then begin
        if Checked then
          CheckSection := s_RadioButtonChecked
        else
          CheckSection := s_RadioButtonUnChecked;
      end;
      with KssListView.SkinData do begin
        CheckIndex := SkinManager.GetMaskIndex(SkinManager.SkinCommonInfo.IndexGlobalInfo, s_GlobalInfo, CheckSection, CommonSkinData);
        ClearRect.Left := WidthOf(CommonSkinData.ma[CheckIndex].R) div CommonSkinData.ma[CheckIndex].ImageCount;
        ClearRect.Top := HeightOf(CommonSkinData.ma[CheckIndex].R) div (CommonSkinData.ma[CheckIndex].MaskType + 1);
      end;
      FCheckBounds := Rect(OffsetX,
                     BitmapRect.Top + (KssListView.ItemHeight - ClearRect.Top) div 2,
                     OffsetX + ClearRect.Left,
                     BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - ClearRect.Top) div 2
                    );
      OffsetX := FCheckBounds.Right + 5;
    end;

  // Image rect
  ImgRec := Rect(OffsetX, 0, OffsetX, 0);
  if Assigned(KssListView.SmallImages) and (KssListView.SmallImages.Count > 0) then begin
    ImgRec := Rect(OffsetX,
                   BitmapRect.Top + (KssListView.ItemHeight - KssListView.SmallImages.Height) div 2,
                   OffsetX + KssListView.SmallImages.Width,
                   BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - KssListView.SmallImages.Height) div 2
                  );
    OffsetX := ImgRec.Right + 5;
  end;

  // Text rect
  TxtRec := Rect(OffsetX, BitmapRect.Top - FontHeightDif + (KssListView.ItemHeight - OutCanvas.Font.Size) div 2, BitmapRect.Right, BitmapRect.Top + KssListView.ItemHeight - (KssListView.ItemHeight - Abs(OutCanvas.Font.Height)) div 2);

  OutText := Caption;
  if KssListView.Columns.Count > 0 then
    OutText := CutText(OutCanvas, OutText, KssListView.Columns[0].Width-OffsetX)
  else
    OutText := CutText(OutCanvas, OutText, KssListView.ClientWidth-OffsetX);

  State := 0;
  // Hot and selected item
  if (Self = KssListView.FHotElement) and Selected then begin
    if SkinIndex < 0 then begin
      OutCanvas.Brush.Color := AverageColor(KssListView.HotItemColor, KssListView.SelectItemColor);
      OutCanvas.Pen.Color := BlendColors(OutCanvas.Brush.Color, clBlack, 127);
    end
    else begin
      State := 3;
    end;
  end
  // Hot item
  else if Self = KssListView.FHotElement then begin
    if SkinIndex < 0 then begin
      OutCanvas.Brush.Color := KssListView.HotItemColor;
      OutCanvas.Pen.Color := BlendColors(KssListView.HotItemColor, clBlack, 127);
    end
    else begin
      State := 1;
    end;
  end
  // Selected item
  else if Selected then begin
    if SkinIndex < 0 then begin
      if not KssListView.Focused then
        OutCanvas.Brush.Color := clSilver
      else
        OutCanvas.Brush.Color := KssListView.SelectItemColor;
      OutCanvas.Pen.Color := BlendColors(OutCanvas.Brush.Color, clBlack, 127);
    end
    else begin
      State := 2;
    end;
  end
  // Normal item
  else begin
    if SkinIndex < 0 then begin
      OutCanvas.Pen.Color := KssListView.Color;
      OutCanvas.Brush.Style := bsClear;
    end
    else begin
      State := 0;
    end;
  end;

  if SkinIndex < 0 then begin
    OutCanvas.Rectangle(BitmapRect);
    if FShowProgress then begin
      OutCanvas.Brush.Color := AverageColor(OutCanvas.Brush.Color, KssListView.FProgressItemColor);
      OutCanvas.Pen.Color := GetShadowColor(OutCanvas.Brush.Color{$IFDEF USELUMIN}, -25{$ENDIF});
      ClearRect := BitmapRect;
      ClearRect.Right := (WidthOf(FBounds) * FProgressPosition) div 100;
      OutCanvas.FillRect(ClearRect);
      OutCanvas.Brush.Style := bsClear;
      OutCanvas.Rectangle(BitmapRect);
    end;
  end
  else begin
    if Selected then begin
      BackBitmap := CreateBmp32(WidthOf(BitmapRect), BitmapRect.Bottom);
      ci := MakeCacheInfo(OutBitmap);
      Color.I := clGray;   
      PaintItem(SkinIndex, ci, true, State, BitmapRect, Point(0, 0), BackBitmap.Canvas.Handle, DefaultManager.CommonSkinData);
      if not KssListView.Focused then
        BlendTransRectangle(OutBitmap, 0, 0, BackBitmap, BitmapRect, byte(127))
      else
        OutCanvas.Draw(0, 0, BackBitmap);

      BackBitmap.Free;
    end;

    if FShowProgress then begin
      ClearRect := BitmapRect;
      ClearRect.Right := (WidthOf(FBounds) * FProgressPosition) div 100;
      BackBitmap := TBitmap.Create;
      BackBitmap.Assign(OutBitmap);
      ci.Bmp := BackBitmap;
      ci.X := 0;
      ci.Y := 0;
      PaintItem(ProgressSkinIndex, ci, true, State, ClearRect, Point(0, 0), OutCanvas.Handle, DefaultManager.CommonSkinData);
      BackBitmap.Free;
    end;

    if Self = KssListView.FHotElement then begin
      BackBitmap := TBitmap.Create;
      BackBitmap.Assign(OutBitmap);
      ci.Bmp := OutBitmap;
      ci.X := 0;
      ci.Y := 0;
      PaintItem(SkinIndex, ci, true, State, BitmapRect, Point(1, 1), BackBitmap.Canvas.Handle, DefaultManager.CommonSkinData);
      Color.I := clGray;
      BlendTransRectangle(OutBitmap, 0, 0, BackBitmap, BitmapRect, byte(127));
      BackBitmap.Free;
    end;
  end;

  DrawText(OutBitmap, OutText, TxtRec, false, SkinIndex, Boolean(State > 0), FOwner.SkinData.SkinManager);

  // Draw checks
  if GetCheckBoxesFinal <> gcNone then
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 5) then begin
      if Checked then
        KssListView.StateImages.Draw(OutCanvas, FCheckBounds.Left, FCheckBounds.Top, 5, true)
      else
        KssListView.StateImages.Draw(OutCanvas, FCheckBounds.Left, FCheckBounds.Top, 4, true);
    end
    else if SkinIndex < 0 then begin
      OutCanvas.Pen.Color := OutCanvas.Font.Color;
      if GetCheckBoxesFinal = gcCheck then
        DrawCheck(OutCanvas, FCheckBounds, integer(Checked))
      else
        DrawRadio(OutCanvas, FCheckBounds, integer(Checked));
    end
    else begin
      DrawSkinGlyph(OutBitmap, Point(FCheckBounds.Left, FCheckBounds.Top), 0 {State (0,1,2)}, 1, KssListView.SkinData.CommonSkinData.ma[CheckIndex], MakeCacheInfo(OutBitmap));
    end;

  // Draw image
  if Assigned(KssListView.SmallImages) and (ImageIndex > -1) then begin
    if (KssListView.SmallImages.Count >= ImageIndex) then
      KssListView.SmallImages.Draw(OutCanvas, ImgRec.Left, ImgRec.Top, ImageIndex, true)
  end;

  //SubItems
  pX := KssListView.Columns[0].Width;
  for i := 0 to SubItems.Count - 1 do
    if (KssListView.Columns.Count > i+1) and KssListView.Columns[i+1].Visible then begin
      OutText := CutText(KssListView.Canvas, SubItems[i], KssListView.Columns[i+1].Width-10);
      OutCanvas.Brush.Style := bsClear;
      TxtRec.Left := pX + 5;
      if SkinIndex < 0 then
        OutCanvas.TextOut(TxtRec.Left, TxtRec.Top, OutText)
      else
        WriteTextEx(OutCanvas, PChar(OutText), true, TxtRec, DT_LEFT, SkinIndex, Boolean(State > 0));
      pX := pX + KssListView.Columns[i+1].Width;
    end;

  PaintGridLines(OutCanvas, Canvas, FBounds);

  // Selection Frame
  if (KssListView.FSelectionFrame) and PaintSelectionFrame then
    KssListView.PaintSelectionFrame(OutBitmap, FBounds);

  // Paint OutBitmap to OutCanvas
  if FBounds.Top < KssListView.GetListRect.Top then begin
    ClearRect.Left := FBounds.Left;
    ClearRect.Right := FBounds.Right;
    ClearRect.Top := KssListView.GetListRect.Top;
    ClearRect.Bottom := ClearRect.Top + HeightOf(FBounds) - (KssListView.GetListRect.Top - FBounds.Top);

    BitmapRect.Top := KssListView.GetListRect.Top - FBounds.Top;
    BitmapRect.Right := OutBitmap.Width;
    Canvas.CopyRect(ClearRect, OutCanvas, BitmapRect);
  end
  else
    Canvas.Draw(FBounds.Left, FBounds.Top, OutBitmap);

  OutBitmap.Free;
  OffsetRect(FCheckBounds, FBounds.Left, FBounds.Top);
end;

procedure TacListItem.Click(MousePos: TPoint);
begin
  if PtInRect(FCheckBounds, MousePos) then
    Checked := not checked
  else begin
    if KssListView.FDragStartShift = [ssShift, ssLeft] then
      KssListView.SelectItems(KssListView.FocusedItem, Self)
    else begin
      Selected := not Selected;
      if KssListView.CheckOnClik then
        Checked := not checked
    end;
  end;
end;

procedure TacListItem.SetIndex(Value: Integer);
var
  ColumnOrder: array of Integer;
  I: Integer;
begin
  inherited SetIndex(Value);
  SetLength(ColumnOrder, Collection.Count);
  for I := 0 to Collection.Count - 1 do
    ColumnOrder[I] := TacListItem(Collection.Items[I]).FOrderTag;
end;

procedure TacListItem.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

procedure TacListItem.SetStateIndex(Value: TImageIndex);
begin
  if FStateIndex <> Value then
  begin
    FStateIndex := Value;
    DoChange;
  end;
end;

procedure TacListItem.SetGroup(Value: TacListGroup);
begin
  if FGroup <> Value then
  begin
    FGroup := Value;
    if FGroup = nil then
      FGroupIndex := -1
    else
      FGroupIndex := FGroup.Index;
    DoChange;
  end;
end;

procedure TacListItem.SetGroupIndex(const Value: integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    FGroup := KssListView.Groups[FGroupIndex];
    DoChange;
  end;
end;

procedure TacListItem.SetSubItems(Value: TStrings);
begin
  FSubItems.Assign(Value);
end;

procedure TacListItem.SetSelected(const Value: boolean);
begin
  if (FSelected <> Value) and KssListView.AllowSelection then
  begin
    if not KssListView.MultiSelect then
      KssListView.UnSelectAll;
    FSelected := Value;
    KssListView.FFocusedElement := Self;
    DoChange;
  end;
end;

procedure TacListItem.SetShowProgress(const Value: boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    DoChange;
  end;
end;

procedure TacListItem.SetProgressPosition(const Value: integer);
begin
  if FProgressPosition <> Value then
  begin
    FProgressPosition := Value;
    if FShowProgress then
      Paint(KssListView.Canvas, true, true);
  end;
end;

procedure TacListItem.Assign(Source: TPersistent);
var
  Item: TacListItem;
begin
  if Source is TacListItem then
  begin
    Item := TacListItem(Source);
    Caption := Item.Caption;
    ImageIndex := Item.ImageIndex;
  end
  else inherited Assign(Source);
end;

function TacListItem.IndexInGroup: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to KssListView.Items.Count - 1 do
    if KssListView.Items[i].Group = Group then begin
      if KssListView.Items[i] = Self then
        Break;
      inc(Result);
    end;
end;

function TacListItem.GetWidth: integer;
var
  Bitmap: TBitmap;
  SkinIndex,
  CheckIndex: integer;
  SkinSections: TStringList;
  CheckSection: string;
begin
  SkinIndex := -1;
  if KssListView.SkinData.Skinned then begin
    SkinSections := TStringList.Create;
    KssListView.SkinData.SkinManager.GetSkinSections(SkinSections);
    SkinIndex := SkinSections.IndexOf(KssListView.ItemSkin);
    FreeAndNil(SkinSections);
  end;

  Result := 2;
  if Assigned(Group) then
    Result := Group.GetButtonRect.Right;

  // Checkbox rect
  if GetCheckBoxesFinal <> gcNone then
    if Assigned(KssListView.StateImages) and (KssListView.StateImages.Count > 5) then
      Result := Result + KssListView.StateImages.Width + 5
    else
      if SkinIndex < 0 then
        Result := Result + 17 {12 + 5;}
      else begin
        if GetCheckBoxesFinal = gcCheck then
          if Checked then
            CheckSection := s_CheckBoxChecked
          else
            CheckSection := s_CheckBoxUnChecked
        else
          if GetCheckBoxesFinal = gcRadio then
            if Checked then
              CheckSection := s_RadioButtonChecked
            else
              CheckSection := s_RadioButtonUnChecked;

          with KssListView.SkinData do begin
            CheckIndex := SkinManager.GetMaskIndex(SkinManager.SkinCommonInfo.IndexGlobalInfo, s_GlobalInfo, CheckSection, CommonSkinData);
            Result := Result + WidthOf(CommonSkinData.ma[CheckIndex].R) div CommonSkinData.ma[CheckIndex].ImageCount + 5;
          end;
      end;

  // Image rect
  if Assigned(KssListView.SmallImages) and (KssListView.SmallImages.Count > 0) then
    Result := Result + KssListView.SmallImages.Width + 5;

  // Text rect
  Bitmap := TBitmap.Create;
  Bitmap.Canvas.Font := KssListView.ItemFont;
  Result := Result + Bitmap.Canvas.TextWidth(Caption) + 5;
  Bitmap.Free;
end;

function TacListItem.GetSubItemWidth(Index: integer): integer;
var
  Bitmap: TBitmap;
begin
  Result := 0;
  if (FSubItems.Count - 1 >= Index) and (FSubItems[Index] <> '') then begin
    Bitmap := TBitmap.Create;
    Bitmap.Canvas.Font := KssListView.ItemFont;

    Result := Bitmap.Canvas.TextWidth(FSubItems[Index]) + 10;

    Bitmap.Free;
  end;
end;

function TacListItem.GetCheckBoxesFinal: TGroupCheckBoxes;
begin
  if Assigned(Group) then Result := Group.GetCheckBoxesFinal
  else Result := TGroupCheckBoxes(KssListView.CheckBoxes);
end;

function TacListItem.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListGroups'}
{$ENDIF}

constructor TacListGroups.Create(AOwner: TacListView);
begin
  inherited Create(TacListGroup);
  FOwner := AOwner;
end;

function TacListGroups.GetItem(Index: Integer): TacListGroup;
begin
  Result := TacListGroup(inherited GetItem(Index));
end;

procedure TacListGroups.SetItem(Index: Integer; Value: TacListGroup);
begin
  inherited SetItem(Index, Value);
end;

function TacListGroups.ItemByCaption(Caption: string): TacListGroup;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if Items[i].Caption = Caption then begin
      Result := Items[i];
      Break;
    end;
end;

function TacListGroups.Add(Caption: string = ''; ImageIndex: integer = -1): TacListGroup;
begin
  Result := TacListGroup(inherited Add);

  if Caption <> '' then
    Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;

  UpdateGroups;
end;

function TacListGroups.Owner: TacListView;
begin
  Result := FOwner;
end;

function TacListGroups.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TacListGroups.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Owner.UpdateGroup(Item.Index) else
  Owner.UpdateGroups;
end;

procedure TacListGroups.UpdateGroups;
//var
//  i: Integer; v7
begin
  if not Owner.HandleAllocated then Exit;
  BeginUpdate;
  try
    Owner.UpdateGroups;
  finally
    EndUpdate;
  end;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListColumns'}
{$ENDIF}

constructor TacListColumns.Create(AOwner: TacListView);
begin
  inherited Create(TacListColumn);
  FOwner := AOwner;
end;

function TacListColumns.GetItem(Index: Integer): TacListColumn;
begin
  Result := TacListColumn(inherited GetItem(Index));
end;

procedure TacListColumns.SetItem(Index: Integer; Value: TacListColumn);
begin
  inherited SetItem(Index, Value);
end;

function TacListColumns.Add: TacListColumn;
begin
  Result := TacListColumn(inherited Add);
  UpdateColumns;
end;

function TacListColumns.Owner: TacListView;
begin
  Result := FOwner;
end;

function TacListColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TacListColumns.GetLastVisible: TacListColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := Count-1 downto 0 do
    if Items[i].Visible then begin
      Result := Items[i];
      Break;
    end;
end;

function TacListColumns.WidthOfVisible: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if Items[i].Visible then begin
      Result := Result + Items[i].Width;
    end;
end;

procedure TacListColumns.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Owner.UpdateColumn(Item.Index) else
  Owner.UpdateColumns;
end;

procedure TacListColumns.UpdateColumns;
begin
  if not Owner.HandleAllocated or (csDestroying in Owner.ComponentState) then Exit;
  BeginUpdate;
  try
    Owner.UpdateColumns;
  finally
    EndUpdate;
  end;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListItems'}
{$ENDIF}

constructor TacListItems.Create(AOwner: TacListView);
begin
  inherited Create(TacListItem);
  FOwner := AOwner;
end;

function TacListItems.GetItem(Index: Integer): TacListItem;
begin
  Result := TacListItem(inherited GetItem(Index));
end;

procedure TacListItems.SetItem(Index: Integer; Value: TacListItem);
begin
  inherited SetItem(Index, Value);
end;

function TacListItems.Add(Caption: string = ''; ImageIndex: integer = -1): TacListItem;
begin
  Result := TacListItem(inherited Add);

  if Caption <> '' then
    Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;

  if FOwner.FFocusedElement = nil then
    FOwner.FFocusedElement := Result;
  UpdateItems;
end;

function TacListItems.Owner: TacListView;
begin
  Result := FOwner;
end;

function TacListItems.Count: integer;
begin
  Result := inherited Count;
end;

function TacListItems.Count(ItemAttributes: TItemAttributes): integer;
var
  i: integer;
  Pass: boolean;
begin
  Result := 0;
  for i := 0 to Count - 1 do begin
    Pass := true;
    if (iaSelect in ItemAttributes) and not Items[i].Selected then
      Pass := false;
    if (iaCheck in ItemAttributes) and not Items[i].Checked then
      Pass := false;
    if (iaVisible in ItemAttributes) and not Items[i].Visible then
      Pass := false;
    if (iaGroup in ItemAttributes) and not Assigned(Items[i].Group) then
      Pass := false;
    if (iaGroupVisible in ItemAttributes) and not (Assigned(Items[i].Group) and Items[i].Group.Visible) then
      Pass := false;
    if Pass then
      inc(Result);
  end;
end;

function TacListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TacListItems.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Owner.UpdateItem(Item.Index) else
  Owner.UpdateItems;
end;

procedure TacListItems.UpdateItems;
begin
  if not Owner.HandleAllocated then Exit;
  BeginUpdate;
  try
    Owner.UpdateItems;
  finally
    EndUpdate;
  end;
end;

{$IFDEF D2005} // v7
{$endregion}
{$region 'TacListView'}
{$ENDIF}

constructor TacListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListGroups := TacListGroups.Create(Self);
  FListColumns := TacListColumns.Create(Self);
  FListItems := TacListItems.Create(Self);
  //AC skins
  FSkinData := TsScrollWndData.Create(Self, True);
  FSkinData.COC := COC_TsEdit;
  FSkinData.SkinSection := s_Edit;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FSkinData);
  FGroupSkin := s_MENUITEM;
  FColumnSkin := s_COLHEADER;
  FItemSkin := s_MENUITEM;
  //Colors
  Color := clWindow;
  FHotItemColor := $00F8EBE4;
  FSelectItemColor := $00EFD3C6;
  FRegularItemColor := clWindow;
  FGridColor := cl3DLight;
  FSelectionFrameColor := clHighLight;
  FProgressItemColor := clGreen;
  //Fonts
  FGroupFont := TFont.Create;
  FColumnFont := TFont.Create;
  FItemFont := TFont.Create;
  //Images
  FStateChangeLink := TChangeLink.Create;
  FSmallChangeLink := TChangeLink.Create;
  FGroupChangeLink := TChangeLink.Create;
  FHotElement := nil;
  FBorderStyle := bsSingle;
  FShowGroupItemsCount := true;
  FShowColumnHeaders := true;
  FShowGroups := true;
  FShowGroupButtons := true;
  FMultiSelect := false;
  FGroupHeight := 20;
  FColumnHeight := 20;
  FItemHeight := 20;
  FVScrollPos := 0;
  FVScrollMax := 0;
  FHScrollPos := 0;
  FHScrollMax := 0;
  FGridLines := glNone;
  FRegularBack := rbEven;
  FItemWidth := iwColumnsWidth;
  FSizeStartWidth := -1;
  FDblClicked := false;
  FSizeColumn := nil;
  FUpdateCount := 0;
  FCheckBoxes := lcNone;
  FCheckOnClik := false;
  FFocusedElement := nil;
  FColumnsBackBounds := Rect(0, 0, 0, 20);
  FSelectionFrame := false;
  FAllowSelection := true;
  FCaptionOnEmpty := 'List is empty now';
  FSpaceKeyAction := skDoInvertCheckFocused;
end;

destructor TacListView.Destroy;
begin
  if ListSW <> nil then
    FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  if Assigned(FSkinData) then
    FreeAndNil(FSkinData);

  FListGroups.Free;
  FListColumns.Free;
  FListItems.Free;
  FGroupFont.Free;
  FColumnFont.Free;
  FItemFont.Free;
  FStateChangeLink.Free;
  FSmallChangeLink.Free;
  FGroupChangeLink.Free;
  inherited Destroy;
end;

procedure TacListView.Clear;
begin
  BeginUpdate;
  FHotElement := nil;
  Items.Clear;
  Groups.Clear;
  EndUpdate;
end;

procedure TacListView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TacListView.EndUpdate(Repaint: boolean = true);
begin
  if FUpdateCount > 0 then begin
    dec(FUpdateCount);
    if (FUpdateCount = 0) and Repaint then Paint;
  end;
end;

procedure TacListView.ScrollToElement(Element: TacListElement);
var
  ItemsTop: integer;
begin
  if Element is TacListColumn then Exit;

  if FShowColumnHeaders then
    ItemsTop := FColumnHeight
  else
    ItemsTop := 0;

  if (Element.Bounds.Top < ItemsTop) or (Element.Bounds.Bottom < ItemsTop) then begin
    FVScrollPos := FVScrollPos + Element.Bounds.Top - ItemsTop;
    Paint;
  end
  else
  if (Element.Bounds.Top > ClientHeight) or (Element.Bounds.Bottom > ClientHeight) then begin
    FVScrollPos := FVScrollPos + Element.Bounds.Bottom - ClientHeight;
    Paint;
  end;
end;

function TacListView.SelectItems(FromItem, ToItem: TacListItem): integer;
var
  i: Integer;
  MinIndex,
  MaxIndex: Integer;
  MinGrIndex,
  MaxGrIndex: Integer;
  MinGrItem,
  MaxGrItem: TacListItem;
begin
  Result := 0;
  if FromItem = ToItem then begin
    FromItem.Selected := true;
    Result := 1;
  end
  else if not Assigned(FromItem) then begin
    ToItem.Selected := true;
    Result := 1;
  end
  else if not Assigned(ToItem) then begin
    FromItem.Selected := true;
    Result := 1;
  end
  else if not Multiselect then begin
    ToItem.Selected := true;
    Result := 1;
  end
  else begin
    BeginUpdate;

    if FShowGroups then begin
      MinGrIndex := FromItem.GroupIndex;
      MinGrItem := FromItem;
      MaxGrIndex := ToItem.GroupIndex;
      MaxGrItem := ToItem;
      if MinGrIndex = MaxGrIndex then
        FromItem.Group.SelectItems(FromItem, ToItem)
      else begin
        if MinGrIndex > MaxGrIndex then begin
          MinGrIndex := ToItem.GroupIndex;
          MinGrItem := ToItem;
          MaxGrIndex := FromItem.GroupIndex;
          MaxGrItem := FromItem;
        end;
        Groups[MinGrIndex].SelectItemsToLast(MinGrItem);
        Groups[MaxGrIndex].SelectItemsToFirst(MaxGrItem);
        for i := MinGrIndex + 1 to MaxGrIndex - 1 do
          Groups[i].SelectAll;
      end;
    end
    else begin
      MinIndex := FromItem.Index;
      MaxIndex := ToItem.Index;
      if MinIndex > MaxIndex then begin
        MinIndex := ToItem.Index;
        MaxIndex := FromItem.Index;
      end;
      for i := MinIndex to MaxIndex do begin
        Items[i].Selected := true;
        inc(Result);
      end;
    end;
    
    EndUpdate;
  end;
end;

procedure TacListView.SetListGroups(Value: TacListGroups);
begin
  FListGroups.Assign(Value);
end;

procedure TacListView.SetListColumns(Value: TacListColumns);
begin
  FListColumns.Assign(Value);
end;

procedure TacListView.SetListItems(Value: TacListItems);
begin
  FListItems.Assign(Value);
end;

procedure TacListView.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_REMOVESKIN : if Message.LParam = LPARAM(SkinData.SkinManager) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FSkinData);
      if not FSkinData.CustomColor then Color := clWindow;
      if not FSkinData.CustomFont then Font.Color := clWindowText;
      RecreateWnd;
      exit
    end;
    AC_REFRESH : if (Message.LParam = LPARAM(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FSkinData);
      if FSkinData.Skinned then begin
        if not FSkinData.CustomColor then Color := FSkinData.CommonSkinData.gd[FSkinData.SkinIndex].Props[0].Color;
        if not FSkinData.CustomFont then Font.Color := FSkinData.CommonSkinData.gd[FSkinData.SkinIndex].Props[0].FontColor.Color;
      end;
      Repaint;
      RefreshEditScrolls(SkinData, ListSW);
      exit
    end;
    AC_ENDPARENTUPDATE : if FSkinData.Updating then begin
      FSkinData.Updating := False;
      Repaint;
      Exit;
    end;
    AC_MOUSELEAVE : WMMouseLeave(Message); // v7
    AC_SETNEWSKIN : if (Message.LParam = LPARAM(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FSkinData);
      exit
    end
  end;
  if not ControlIsReady(Self) or not FSkinData.Skinned(True) then inherited else begin
    case Message.Msg of
      CN_DRAWITEM : Exit;
      WM_ERASEBKGND : begin // v7 Remove blinking which occurs sometimes
        Message.Result := 1;
        Exit;
      end;
      WM_SETFOCUS, CM_ENTER : if CanFocus then begin
        inherited;
        if Focused then begin
          FSkinData.FFocused := True;
          FSkinData.FMouseAbove := False;
          FSkinData.BGChanged := True;
        end;
      end;
      WM_KILLFOCUS, CM_EXIT: begin
        FSkinData.FFocused := False;
        FSkinData.FMouseAbove := False;
        FSkinData.BGChanged := True;
      end;
    end;
    CommonWndProc(Message, FSkinData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED : RefreshEditScrolls(SkinData, ListSW);
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT : begin
        FSkinData.Invalidate;
      end;
    end;
  end;
  // Aligning of the bound label
  if Assigned(BoundLabel) and Assigned(BoundLabel.FtheLabel) then case Message.Msg of
    WM_SIZE, WM_WINDOWPOSCHANGED : BoundLabel.AlignLabel;
    CM_VISIBLECHANGED : begin BoundLabel.FtheLabel.Visible := Visible; BoundLabel.AlignLabel end;
    CM_ENABLEDCHANGED : begin BoundLabel.FtheLabel.Enabled := Enabled or not (dkBlended in DisabledKind); BoundLabel.AlignLabel end;
    CM_BIDIMODECHANGED : begin BoundLabel.FtheLabel.BiDiMode := BiDiMode; BoundLabel.AlignLabel end;
  end;
end;

procedure TacListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_HEADER);
  if (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TacListView.AfterConstruction;
begin
  inherited AfterConstruction;
  FSkinData.Loaded;
end;

procedure TacListView.Loaded;
begin
  inherited Loaded;
  FSkinData.Loaded;
  RefreshEditScrolls(SkinData, ListSW);
end;

procedure TacListView.ChangeScale(M: Integer; D: Integer);
var
  i: Integer;
begin
  FScale := M / D;

  FGroupHeight := MulDiv(FGroupHeight, M, D);
  FColumnHeight := MulDiv(FColumnHeight, M, D);
  FItemHeight := MulDiv(FItemHeight, M, D);

  FItemFont.Size := MulDiv(FItemFont.Size, M, D);
  FColumnFont.Size := MulDiv(FColumnFont.Size, M, D);
  FGroupFont.Size := MulDiv(FGroupFont.Size, M, D);

  for i := 0 to FListColumns.Count - 1 do
    FListColumns[i].FWidth := MulDiv(FListColumns[i].FWidth, M, D);

  inherited ChangeScale(M,D);
end;

procedure TacListView.SetGroupFont(Value: TFont);
begin
  FGroupFont.Assign(Value);
end;

procedure TacListView.SetColumnFont(Value: TFont);
begin
  FColumnFont.Assign(Value);
end;

procedure TacListView.SetItemFont(Value: TFont);
begin
  FItemFont.Assign(Value);
end;

procedure TacListView.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FSkinData.Invalidate;
  end;
end;

procedure TacListView.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TacListView.UpdateGroup(AnIndex: Integer);
begin
  //
end;

procedure TacListView.UpdateColumn(AnIndex: integer);
begin
  //
end;

procedure TacListView.UpdateItem(AnIndex: integer);
begin
  //
end;

procedure TacListView.UpdateGroups;
begin          
  if HandleAllocated then
    Paint;
end;

procedure TacListView.UpdateColumns;
begin
  if HandleAllocated and not (csDestroying in Owner.ComponentState) then Paint;
end;

procedure TacListView.UpdateItems;
begin
  if HandleAllocated then
    Paint;
end;

procedure TacListView.SetHotElement(Value: TacListElement);
var
  OldHot: TacListElement;
begin
  if FHotElement <> Value then begin
    OldHot := FHotElement;
    FHotElement := Value;
    Application.CancelHint;

    if Assigned(OldHot) then
      OldHot.Paint(Canvas, true, true);

    if Assigned(FHotElement) then begin
      FHotElement.Paint(Canvas, true, true);

      if FHotElement.ShowHint then begin
        ShowHint := true;
        Hint := FHotElement.Hint;
      end
      else begin
        ShowHint := false;
        Hint := '';
      end;

    end
    else begin
      ShowHint := false;
      Hint := '';
    end;
  end;
end;

procedure TacListView.SetShowGroupItemsCount(const Value: boolean);
begin
  if FShowGroupItemsCount <> Value then
  begin
    FShowGroupItemsCount := Value;
    UpdateGroups;
  end;
end;

procedure TacListView.SetShowColumnHeaders(const Value: boolean);
begin
  if FShowColumnHeaders <> Value then
  begin
    FShowColumnHeaders := Value;
    UpdateColumns;
  end;
end;

procedure TacListView.SetShowGroups(const Value: boolean);
begin
  if FShowGroups <> Value then
  begin
    FShowGroups := Value;
    UpdateGroups;
  end;
end;

procedure TacListView.SetShowGroupButtons(const Value: boolean);
begin
  if FShowGroupButtons <> Value then
  begin
    FShowGroupButtons := Value;
    UpdateGroups;
  end;
end;

procedure TacListView.SetAllowSelection(Value: boolean);
begin
  if FAllowSelection <> Value then
  begin
    FAllowSelection := Value;
    if not FAllowSelection then
      UnSelectAll;
  end;
end;

procedure TacListView.SetCaptionOnEmpty(Value: string);
begin
  if FCaptionOnEmpty <> Value then
  begin
    FCaptionOnEmpty := Value;
    if Items.Count = 0 then
      Paint;
  end;
end;

procedure TacListView.SetMultiSelect(const Value: boolean);
var
  NowSelected: TacListItem;
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    NowSelected := SelectedItem;
    if not FMultiSelect and (NowSelected <> nil) then begin
      UnSelectAll;
      NowSelected.FSelected := true;
    end;
    Paint;
  end;
end;

procedure TacListView.SetGroupHeight(Value: integer);
begin
  if FGroupHeight <> Value then
  begin
    FGroupHeight := Value;
    UpdateGroups;
  end;
end;

procedure TacListView.SetItemHeight(Value: integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateItems;
  end;
end;

procedure TacListView.SetColumnHeight(Value: integer);
begin
  if FColumnHeight <> Value then
  begin
    FColumnHeight := Value;
    UpdateColumns;
  end;
end;

procedure TacListView.SetSpaceKeyAction(Value: TSpaceKeyAction);
begin
  if FSpaceKeyAction <> Value then
  begin
    FSpaceKeyAction := Value;
    Paint;
  end;
end;

procedure TacListView.SetRegularBack(Value: TRegularBack);
begin
  if FRegularBack <> Value then
  begin
    FRegularBack := Value;
    Paint;
  end;
end;

procedure TacListView.SetHotItemColor(Value: TColor);
begin
  if FHotItemColor <> Value then
  begin
    FHotItemColor := Value;
    if Assigned(FHotElement) then
      Paint;
  end;
end;

procedure TacListView.SetSelectItemColor(Value: TColor);
begin
  if FSelectItemColor <> Value then
  begin
    FSelectItemColor := Value;
    if Assigned(SelectedItem) then
      Paint;
  end;
end;

procedure TacListView.SetRegularItemColor(Value: TColor);
begin
  if FRegularItemColor <> Value then
  begin
    FRegularItemColor := Value;
    if FRegularBack <> rbNone then
      Paint;
  end;
end;

procedure TacListView.SetProgressItemColor(Value: TColor);
begin
  if FProgressItemColor <> Value then
  begin
    FProgressItemColor := Value;
  //  if FRegularBack <> rbNone then
      Paint;
  end;
end;

procedure TacListView.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if GridLines <> glNone then
      Paint;
  end;
end;

procedure TacListView.SetSelectionFrameColor(Value: TColor);
begin
  if FSelectionFrameColor <> Value then
  begin
    FSelectionFrameColor := Value;
    if FSelectionFrame then
      Paint;
  end;
end;

procedure TacListView.SetItemWidth(Value: TItemWidth);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Paint;
  end;
end;

procedure TacListView.SetCheckBoxes(Value: TListCheckBoxes);
var
  NowChecked: TacListItem;
  i: Integer;
begin
  if FCheckBoxes <> Value then begin
    FCheckBoxes := Value;
    if FCheckBoxes = lcRadio then begin
      NowChecked := CheckedItem;
      UnCheckAll;
      CheckedItem := NowChecked;
    end;
    for i := 0 to Groups.Count - 1 do
      Groups[i].UpdateCheck;
    Paint;
  end;
end;

procedure TacListView.SetCheckOnClik(Value: boolean);
begin
  if FCheckOnClik <> Value then
  begin
    FCheckOnClik := Value;
  end;
end;

procedure TacListView.SetSkinData(Value: TsScrollWndData);
begin
  FSkinData.Assign(Value);
end;

procedure TacListView.SetGroupSkin(Value: string);
begin
  if FGroupSkin <> Value then
  begin
    FGroupSkin := Value;
    UpdateGroups;
  end;
end;

procedure TacListView.SetColumnSkin(Value: string);
begin
  if FColumnSkin <> Value then
  begin
    FColumnSkin := Value;
    UpdateColumns;
  end;
end;

procedure TacListView.SetItemSkin(Value: string);
begin
  if FItemSkin <> Value then
  begin
    FItemSkin := Value;
    UpdateItems;
  end;
end;

procedure TacListView.SetGridLines(Value: TGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Paint;
  end;
end;

procedure TacListView.ShowScrollBar(const ScrollBarKind: TScrollBarKind; const Visible: Boolean);
begin
  if ScrollBarKind = sbVertical then begin
    if Visible <> FVScrollBarShown then begin
      FVScrollBarShown := Visible;
      Windows.ShowScrollBar(Handle, SB_VERT, Visible);
    end;
  end
  else begin
    if Visible <> FHScrollBarShown then begin
      FHScrollBarShown := Visible;
      Windows.ShowScrollBar(Handle, SB_HORZ, Visible);
    end;
  end;
end;

procedure TacListView.WMVScroll(var Message: TWMVScroll);
var
  ScrollInfo: TScrollInfo;
  ScrollPos: integer;
begin
  if TScrollCode(Message.ScrollCode) = scTrack then begin
    ZeroMemory(@ScrollInfo, SizeOf(ScrollInfo));
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask  := SIF_TRACKPOS;
    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    ScrollPos := ScrollInfo.nTrackPos;
  end
  else
    ScrollPos := GetScrollPos(Handle, SB_VERT);

  ScrollPosChanged(sbVertical, TScrollCode(Message.ScrollCode), ScrollPos);
end;

procedure TacListView.WMHScroll(var Message: TWMHScroll);
var
  ScrollInfo: TScrollInfo;
  ScrollPos: integer;
begin
  if TScrollCode(Message.ScrollCode) = scTrack then begin
    ZeroMemory(@ScrollInfo, SizeOf(ScrollInfo));
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask  := SIF_TRACKPOS;
    GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
    ScrollPos := ScrollInfo.nTrackPos;
  end
  else
    ScrollPos := GetScrollPos(Handle, SB_HORZ);

  ScrollPosChanged(sbHorizontal, TScrollCode(Message.ScrollCode), ScrollPos);
end;

procedure TacListView.ScrollPosChanged(ScrollBarKind: TScrollBarKind; ScrollCode: TScrollCode; ScrollPos: Integer);
var
  NowScrollPos: integer;
  FScrollPos: ^integer;
  FScrollMax: ^integer;
  LineValue: integer;
begin
  case ScrollBarKind of
    sbHorizontal : begin
                     FScrollPos := @FHScrollPos;
                     FScrollMax := @FHScrollMax;
                     LineValue := 1;
                   end
    else begin
                     FScrollPos := @FVScrollPos;
                     FScrollMax := @FVScrollMax;
                     LineValue := FItemHeight;
                   end;
  end;

  case ScrollCode of
    scTrack    : begin
                   if (FScrollPos^ <> ScrollPos) then begin
                     if ScrollPos > FScrollMax^ then
                       FScrollPos^ := FScrollMax^
                     else if ScrollPos < 0 then
                       FScrollPos^ := 0
                     else
                       FScrollPos^ := ScrollPos;
                     Paint;
                   end;
                 end;
    scLineDown : begin
                   NowScrollPos := FScrollPos^;
                   inc(FScrollPos^, LineValue);
                   if FScrollPos^ > FScrollMax^ then
                     FScrollPos^ := FScrollMax^;
                   if FScrollPos^ <> NowScrollPos then
                     Paint;
                 end;
    scLineUp   : begin
                   NowScrollPos := FScrollPos^;
                   dec(FScrollPos^, LineValue);
                   if FScrollPos^ < 0 then
                     FScrollPos^ := 0;
                   if FScrollPos^ <> NowScrollPos then
                     Paint;
                 end;
  end;

  case ScrollBarKind of
    sbHorizontal : SetScrollPos(Handle, sb_HORZ, FHScrollPos, True);
    sbVertical   : SetScrollPos(Handle, sb_VERT, FVScrollPos, True);
  end;
end;

procedure TacListView.UnSelectAll;
var
  i: integer;
begin
  for i := 0 to FListItems.Count - 1 do
    FListItems[i].FSelected := false;
  Paint;
end;

procedure TacListView.UnCheckAll;
var
  i: integer;
begin
  for i := 0 to FListItems.Count - 1 do
    FListItems[i].FChecked := false;
  Paint;
end;

procedure TacListView.SelectAll;
var
  i: integer;
begin
  if FMultiSelect then begin
    for i := 0 to FListItems.Count - 1 do
      FListItems[i].FSelected := true;
  end
  else
    if not Assigned(SelectedItem) and (Items.Count > 0) then
      Items[0].FSelected := true;
  Paint;
end;

function  TacListView.GetListRect: TRect;
begin
  Result := Rect(0, 0, ClientWidth, ClientHeight);
  if FShowColumnHeaders then
    Result.Top := FColumnHeight;
end;

procedure TacListView.SetSelectedItem(Value: TacListItem);
var
  NowSelected: TacListItem;
begin
  NowSelected := SelectedItem;

  if NowSelected <> Value then begin
    if (Value = nil) and (NowSelected <> nil) then
      UnSelectAll
    else begin
      if not MultiSelect then UnSelectAll;
      Value.FSelected := true;
    end;
    Paint;
  end;
end;

function  TacListView.GetSelectedItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FListItems.Count - 1 do
    if FListItems[i].Selected then begin
      Result := FListItems[i];
      Break;
    end;
end;

function  TacListView.GetCheckedItem: TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FListItems.Count - 1 do
    if FListItems[i].Checked then begin
      Result := FListItems[i];
      Break;
    end;
end;

procedure TacListView.SetCheckedItem(Value: TacListItem);
var
  NowChecked: TacListItem;
begin
  NowChecked := SelectedItem;

  if NowChecked <> Value then begin
    if (Value = nil) and (NowChecked <> nil) then
      UnCheckAll
    else begin
      Value.Checked := true;
    end;
    Paint;
  end;
end;

function  TacListView.GetFocusedItem: TacListItem;
begin
  Result := nil;
  if Assigned(FFocusedElement) and (FFocusedElement is TacListItem) then
    Result := TacListItem(FFocusedElement);
end;

function  TacListView.GetFocusedGroup: TacListGroup;
begin
  Result := nil;
  if Assigned(FFocusedElement) and (FFocusedElement is TacListGroup) then
    Result := TacListGroup(FFocusedElement);
end;

procedure TacListView.SetStateImages(Value: TCustomImageList);
begin
  if FStateImages <> Value then
  begin
    if FStateImages <> nil then
      FStateImages.UnRegisterChanges(FStateChangeLink);
    FStateImages := Value;
    if FStateImages <> nil then
    begin
      FStateImages.RegisterChanges(FStateChangeLink);
      FStateImages.FreeNotification(Self);
    end;
    Paint;
  end;
end;

procedure TacListView.SetSmallImages(Value: TCustomImageList);
begin
  if FSmallImages <> Value then
  begin
    if FSmallImages <> nil then
      FSmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if FSmallImages <> nil then
    begin
      FSmallImages.RegisterChanges(FSmallChangeLink);
      FSmallImages.FreeNotification(Self);
    end;
    Paint;
  end;
end;

procedure TacListView.SetGroupImages(Value: TCustomImageList);
begin
  if FGroupImages <> Value then
  begin
    if FGroupImages <> nil then
      FGroupImages.UnRegisterChanges(FGroupChangeLink);
    FGroupImages := Value;
    if FGroupImages <> nil then
    begin
      FGroupImages.RegisterChanges(FGroupChangeLink);
      FGroupImages.FreeNotification(Self);
    end;
    Paint;
  end;
end;

procedure TacListView.Paint;
var
  i, j: integer;
  ItemsTop: integer;
  iP: TPoint;
  ScrollInfo: TScrollInfo;
  HiddenElementsU: integer;
  HiddenElementsD: integer;
  OutBitmap: TBitmap;
begin
  if not Visible or (csDestroying in ComponentState) or (csLoading in ComponentState) then Exit;
  if FUpdateCount = 0 then begin
    HiddenElementsU := 0;
    HiddenElementsD := 0;
    ItemsTop := 0;

    // Clear background
    OutBitmap := CreateBmp32(ClientWidth, ClientHeight);
    if SkinData.SkinIndex < 0
      then FillDC(OutBitmap.Canvas.Handle, OutBitmap.Canvas.ClipRect, Color)
      else OutBitmap.Canvas.CopyRect(OutBitmap.Canvas.ClipRect, SkinData.FCacheBmp.Canvas, Rect(2, 1, OutBitmap.Width+2, OutBitmap.Height+2));

    // Columns
    if FShowColumnHeaders then begin
      iP.X := 0;
      if FHScrollPos > 0 then iP.X := -FHScrollPos;
      iP.Y := 0;
      for i := 0 to FListColumns.Count - 1 do
      if FListColumns[i].Visible then begin
        FListColumns[i].CalcBounds(iP);
        FListColumns[i].Paint(OutBitmap.Canvas, true, false);
      end;
      FHScrollMax := Columns.WidthOfVisible - ClientWidth;
      // Columns background
      FColumnsBackBounds := Rect(iP.X, 0, ClientWidth, ColumnHeight);

      ItemsTop := FColumnHeight;
    end;
    iP.Y := ItemsTop - FVScrollPos;
    // Empty caption
    if (Groups.Count = 0) and (Items.Count = 0) then begin
      OutBitmap.Canvas.Font.Color := clGray;
      OutBitmap.Canvas.Brush.Style := bsClear;
      iP.X := (ClientWidth - OutBitmap.Canvas.TextWidth(FCaptionOnEmpty)) div 2;
      OutBitmap.Canvas.TextOut(iP.X, ItemsTop+5, FCaptionOnEmpty);
    end;
    // Groups
    if FShowGroups then begin
      for i := 0 to FListGroups.Count - 1 do begin
        iP.X := -FHScrollPos;
        if FListGroups[i].Visible then begin
          FListGroups[i].CalcBounds(iP);
          if FListGroups[i].Bounds.Top < ItemsTop then begin
            if HiddenElementsU = 0 then
              HiddenElementsU := Abs(FListGroups[i].Bounds.Top - ItemsTop)
          end
          else if FListGroups[i].Bounds.Bottom > ClientHeight then
            HiddenElementsD := FListGroups[i].Bounds.Bottom - ClientHeight;
          if (FListGroups[i].Bounds.Bottom > ItemsTop) and
             (FListGroups[i].Bounds.Top < ClientHeight) then begin
               FListGroups[i].Paint(OutBitmap.Canvas, true, false);
             end;
          // Group items
          if FListGroups[i].Expanded then begin
            for j := 0 to FListItems.Count - 1 do begin
              if (FListItems[j].Group = FListGroups[i]) and FListItems[j].Visible then begin
                FListItems[j].CalcBounds(iP);
                if FListItems[j].Bounds.Top < ItemsTop then begin
                  if HiddenElementsU = 0 then
                    HiddenElementsU := Abs(FListItems[j].Bounds.Top - ItemsTop)
                end
                else if FListItems[j].Bounds.Bottom > ClientHeight then
                  HiddenElementsD := FListItems[j].Bounds.Bottom - ClientHeight;
                if (FListItems[j].Bounds.Bottom > ItemsTop) and (FListItems[j].Bounds.Top < ClientHeight) then begin
                  FListItems[j].Paint(OutBitmap.Canvas, true, false);
                end;
              end;
            end;
            // Group footer
            if FListGroups[i].FFooter <> '' then begin
              FListGroups[i].CalcFooterBounds(iP);
              if FListGroups[i].FFooterBounds.Top < ItemsTop then begin
                if HiddenElementsU = 0 then
                  HiddenElementsU := Abs(FListGroups[i].FFooterBounds.Top - ItemsTop)
              end
              else if FListGroups[i].FFooterBounds.Bottom > ClientHeight then
                HiddenElementsD := FListGroups[i].FFooterBounds.Bottom - ClientHeight;
              if (FListGroups[i].FFooterBounds.Bottom > ItemsTop) and (FListGroups[i].FFooterBounds.Top < ClientHeight) then begin
                FListGroups[i].PaintFooter(OutBitmap.Canvas, true, false);
              end;
            end;
          end; {FListGroups[i].Expanded}
        end;
      end;
    end
    // Items (without groups)
    else begin
      iP.X := -FHScrollPos;
      for i := 0 to FListItems.Count - 1 do begin
        FListItems[i].CalcBounds(iP);
        if FListItems[i].Bounds.Top < ItemsTop then begin
          if HiddenElementsU = 0 then
            HiddenElementsU := Abs(FListItems[i].Bounds.Top - ItemsTop)
        end
        else if FListItems[i].Bounds.Bottom > ClientHeight then
          HiddenElementsD := FListItems[i].Bounds.Bottom - ClientHeight;
        if (FListItems[i].Bounds.Bottom > ItemsTop) and (FListItems[i].Bounds.Top < ClientHeight) then begin
          FListItems[i].Paint(OutBitmap.Canvas, true, false);
        end;
      end;
    end;

    FVScrollMax := HiddenElementsU + HiddenElementsD;

    if FVScrollMax = 0 then
      for i := 0 to (ClientHeight - iP.Y) div ItemHeight do
        PaintGridLines(GridLines, OutBitmap.Canvas, Rect(0, iP.Y + ItemHeight*i, ClientWidth, iP.Y + ItemHeight*(i+1)));

    // Selection Frame
    if FSelectionFrame then begin
      PaintSelectionFrame(OutBitmap, ClientRect);
    end;

    Canvas.Draw(0, 0, OutBitmap);
    OutBitmap.Free;

    if FHScrollMax > 0 then begin
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := FHScrollMax;
      ScrollInfo.nPos := FHScrollPos;
      ScrollInfo.nPage := 1;

      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, False);

      ShowScrollBar(sbHorizontal, True);
    end
    else begin
      FHScrollPos := 0;
      ShowScrollBar(sbHorizontal, false);
    end;

    if FVScrollMax > 0 then begin
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := FVScrollMax + ClientHeight;
      ScrollInfo.nPos := HiddenElementsU;
      ScrollInfo.nPage := ClientHeight;

      SetScrollInfo(Handle, SB_VERT, ScrollInfo, False);

      ShowScrollBar(sbVertical, True);
    end
    else begin
      FVScrollPos := 0;
      ShowScrollBar(sbVertical, false);
    end;
  end;
end;

procedure TacListView.PaintGridLines(GridLines: TGridLines; Canvas: TCanvas; Region: TRect);
var
  i: integer;
begin
  Canvas.Pen.Color := GridColor;

  if (GridLines = glVertical) or (GridLines = glBoth) then begin
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Visible then begin
        Canvas.MoveTo(Region.Left + Columns[i].FBounds.Right-1, Region.Top);
        Canvas.LineTo(Region.Left + Columns[i].FBounds.Right-1, Region.Bottom);
      end;
  end;

  if (GridLines = glHorizontal) or (GridLines = glBoth) then begin
    Self.Canvas.Pen.Color := GridColor;
    Canvas.MoveTo(Region.Left, Region.Bottom);
    Canvas.LineTo(Region.Right-1, Region.Bottom);
  end;
end;

procedure TacListView.PaintSelectionFrame(Bitmap: TBitmap; Region: TRect);
var
  ClearRect: TRect;
  BackBitmap: TBitmap;
begin
  BackBitmap := CreateBmp32(Bitmap.Width, Bitmap.Height);
  FillDC(BackBitmap.Canvas.Handle, Rect(0, 0, Bitmap.Width, Bitmap.Height), clFuchsia);

  ClearRect.Left := FSelectionRect.Left - Region.Left;
  ClearRect.Right := FSelectionRect.Right - Region.Left;
  ClearRect.Top := FSelectionRect.Top - Region.Top;
  ClearRect.Bottom := FSelectionRect.Bottom - Region.Top;

  BackBitmap.Canvas.Brush.Color := SelectionFrameColor;
  BackBitmap.Canvas.Brush.Style := bsSolid;
  BackBitmap.Canvas.Pen.Color := GetShadowColor(SelectionFrameColor{$IFDEF USELUMIN}, -40{$ENDIF});

  BackBitmap.Canvas.Rectangle(ClearRect);

  BlendTransRectangle(Bitmap, 0, 0, BackBitmap, BackBitmap.Canvas.ClipRect, byte(127));

  BackBitmap.Free;
end;

procedure TacListView.Resize;
begin
  Paint;
end;

procedure TacListView.KeyDown(var Key: Word; Shift: TShiftState);
var
  ListElement: TacListElement;
  i: Integer;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_DOWN  : begin
                 ListElement := GetBottomVisibleElement(FFocusedElement);
                 if Assigned(ListElement) and (ListElement <> FFocusedElement) then begin
                   FFocusedElement := ListElement;

                   if Assigned(FocusedItem) then begin
                     UnSelectAll;
                     SelectedItem := FocusedItem;
                   end;
                   if Assigned(FocusedGroup) then begin
                     UnSelectAll;
                     FocusedGroup.SelectAll;
                   end;

                   ScrollToElement(FFocusedElement);
                 end;
               end;
    VK_UP    : begin
                 ListElement := GetUpperVisibleElement(FFocusedElement);
                 if Assigned(ListElement) and (ListElement <> FFocusedElement) then begin
                   FFocusedElement := ListElement;

                   if Assigned(FocusedItem) then begin
                     UnSelectAll;
                     SelectedItem := FocusedItem;
                   end;
                   if Assigned(FocusedGroup) then begin
                     UnSelectAll;
                     FocusedGroup.SelectAll;
                   end;

                   ScrollToElement(FFocusedElement);
                 end;
               end;
    VK_SPACE : case SpaceKeyAction of
                 skDoSelectFocused      : begin
                   if Assigned(FocusedItem) then
                     FocusedItem.Selected := true;
                 end;
                 skDoInvertCheckFocused : begin
                   if Assigned(FocusedItem) then
                     FocusedItem.Checked := not FocusedItem.Checked;
                 end;
                 skDoInvertCheckSelected: begin
                   BeginUpdate;
                   for i := 0 to Items.Count - 1 do
                     if Items[i].Selected then
                       Items[i].Checked := not Items[i].Checked;
                   EndUpdate;
                 end;
               end;
  end;
end;

function TacListView.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  ClientMousePos: TPoint;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    ClientMousePos := ScreenToClient(MousePos);
    inc(ClientMousePos.Y, GetListRect.Top);
    SetHotElement(GetElementAt(ClientMousePos));
    Result := True;
    if (FVScrollMax > 0) and (Shift = []) then
      ScrollPosChanged(sbVertical, scLineDown, 0)
    else if (FVScrollMax > 0) and (ssCtrl in Shift) then
      ScrollPosChanged(sbVertical, scPageDown, 0)
  end;
end;

function TacListView.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  ClientMousePos: TPoint;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    ClientMousePos := ScreenToClient(MousePos);
    dec(ClientMousePos.Y, GetListRect.Top);
    SetHotElement(GetElementAt(ClientMousePos));
    Result := True;
    if (FVScrollMax > 0) and (Shift = []) then
      ScrollPosChanged(sbVertical, scLineUp, 0)
    else if (FVScrollMax > 0) and (ssCtrl in Shift) then
      ScrollPosChanged(sbVertical, scPageUp, 0)
  end;
end;

procedure TacListView.DblClick;
begin
  inherited;
  FDblClicked := true;
end;

procedure TacListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then begin
    if not Focused then
      Windows.SetFocus(Handle);

    FDragStartPos := Point(X, Y);
    FDragStarted := False;
    FDownElement := GetElementAt(FDragStartPos);

    if FDownElement <> nil then begin
      FDragStartShift := Shift;
      if (FDownElement is TacListColumn) and (FDownElement <> Columns[0]) and
         (X < TacListColumn(FDownElement).FBounds.Left + 10) then
      begin
        FSizeColumn := TacListColumn(TacListColumn(FDownElement).GetPrevVisible);
        if FDblClicked then
          FSizeColumn.SetOptimalWidth
        else
          FSizeStartWidth := FSizeColumn.Width;
      end
      else begin
        if FDownElement.FAllowReorder then
          FDragElement := FDownElement;
      end;
    end
    else if PtInRect(FColumnsBackBounds, Point(X, Y)) then begin
      if (Columns.Count > 0) and (X < FColumnsBackBounds.Left + 10) then
      begin
          FSizeColumn := Columns.GetLastVisible;
          if FDblClicked then
            FSizeColumn.SetOptimalWidth
          else
            FSizeStartWidth := FSizeColumn.Width;
      end;
    end
    // Mouse down on empty place
    else begin
      BeginUpdate;
      UnSelectAll;
      EndUpdate(false);
      if FMultiselect and FAllowSelection then begin
        FSelectionFrame := true;
        FSelectionRect.TopLeft := FDragStartPos;
      end;
    end;
  end;

  if FDblClicked then
      FDblClicked := false;
end;

procedure TacListView.WMMouseLeave(var Message: TMessage);
var
  NowHot: TacListElement;
begin
  if FHotElement <> nil then
  begin
    NowHot := FHotElement;
    FHotElement := nil;
    NowHot.Paint(Canvas, true, true);
  end;
end;

procedure TacListView.CMHintShow(var Message: TMessage);
begin
  Inherited;
end;

procedure TacListView.WMLastFocus(var Message : TMessage);
begin
  Paint;
  Inherited;
end;

function TacListView.GetElementAt(MousePos: TPoint): TacListElement;
var
  i: integer;
begin
  Result := nil;
  // Columns
  if FShowColumnHeaders then
    for i := 0 to FListColumns.Count - 1 do
      if FListColumns[i].Visible and PtInRect(FListColumns[i].Bounds, MousePos) then
        Result := FListColumns[i];
  // Groups
  if FShowGroups and (Result = nil) then
    for i := 0 to FListGroups.Count - 1 do
      if FListGroups[i].Visible and PtInRect(FListGroups[i].Bounds, MousePos) then
        Result := FListGroups[i];
  // Items
  if Result = nil then
    for i := 0 to FListItems.Count - 1 do
      if not FListItems[i].Hidden and PtInRect(FListItems[i].Bounds, MousePos) then
        Result := FListItems[i];
end;

function TacListView.CompareItems(Item1, Item2: TacListItem; ColumnIndex: integer): integer;
begin
  Result := 0;
  if Item1 = Item2 then //(Item1 = nil) and (Item2 = nil)
    Exit
  else if Item1 = nil then
    Result := -1
  else if Item2 = nil then
    Result := 1
  else begin
    if ColumnIndex = 0 then begin
      if Item1.Caption > Item2.Caption then
        Result := 1
      else if Item1.Caption < Item2.Caption then
        Result := -1
    end else begin
      if (Item1.SubItems.Count < ColumnIndex) and (Item2.SubItems.Count < ColumnIndex) then
        Exit
      else if Item1.SubItems.Count < ColumnIndex then
        Result := -1
      else if Item2.SubItems.Count < ColumnIndex then
        Result := 1
      else begin
        if Item1.SubItems[ColumnIndex-1] > Item2.SubItems[ColumnIndex-1] then
          Result := 1
        else if Item1.SubItems[ColumnIndex-1] < Item2.SubItems[ColumnIndex-1] then
          Result := -1
      end;
    end;
  end;
end;

function TacListView.GetUpperVisibleElement(Element: TacListElement): TacListElement;
var
  Group: TacListGroup;
  Item: TacListItem;
begin
  Result := nil;
  if Element is TacListGroup then begin
    Group := TacListGroup(Element);
    Group := TacListGroup(Group.GetPrevVisible);
    if Assigned(Group) then begin
      Result := Group.LastVisibleItem;
      if not Assigned(Result) then
        Result := Group;
    end;
  end
  else
  if Element is TacListItem then begin
    Item := TacListItem(Element);
    if Assigned(Item.Group) then begin
      Result := Item.Group.GetPrevVisibleItem(Item);
      if not Assigned(Result) then
        Result := Item.Group;
    end
    else
      Result := Item.GetPrevVisible;
  end;
end;

function TacListView.GetBottomVisibleElement(Element: TacListElement): TacListElement;
var
  Group: TacListGroup;
  Item: TacListItem;
begin
  Result := nil;
  if Element is TacListGroup then begin
    Group := TacListGroup(Element);
    if Group.Count([iaVisible]) > 0 then
      Result := Group.FirstVisibleItem
    else
      Result := Group.GetNextVisible;
  end
  else
  if Element is TacListItem then begin
    Item := TacListItem(Element);
    if Assigned(Item.Group) then begin
      Result := Item.Group.GetNextVisibleItem(Item);
      if not Assigned(Result) then
        Result := Item.Group.GetNextVisible;
    end
    else
      Result := Item.GetNextVisible;
  end;
end;

function TacListView.GetItemsArray: TacPointerList;
var
  i: integer;
begin
  SetLength(Result, Items.Count);
  for i := 0 to Items.Count - 1 do
    Result[i] := Pointer(Items[i]);
end;

procedure TacListView.QuickListSort(SortList: TacPointerList; L, R: Integer; ColumnIndex: integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while CompareItems(SortList[I], P, ColumnIndex) < 0 do
        Inc(I);
      while CompareItems(SortList[J], P, ColumnIndex) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList[I];
        SortList[I] := SortList[J];
        SortList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickListSort(SortList, L, J, ColumnIndex);
    L := I;
  until I >= R;
end;

procedure TacListView.SortItems(SortKind: TSortKind; ColumnIndex: integer; DataType: TDataType);
var
  i: integer;
  ItemsArray: TacPointerList;
begin
  if Items.Count > 0 then begin
    BeginUpdate;
    ItemsArray := GetItemsArray;
    QuickListSort(ItemsArray, 0, Items.Count - 1, ColumnIndex);
    if SortKind = skASC then begin
      for i := 0 to Items.Count - 1 do
        TacListItem(ItemsArray[i]).Index := i;
    end
    else
      if SortKind = skDESC then
        for i := Items.Count - 1 downto 0 do
          TacListItem(ItemsArray[i]).Index := Items.Count - 1 - i;

    SetLength(ItemsArray, 0);
    EndUpdate;
  end;
end;

procedure TacListView.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  i: integer;
begin
  inherited;
  SetHotElement(GetElementAt(Point(X, Y)));
  if Assigned(FHotElement) and (FHotElement is TacListColumn) then begin
    if (FHotElement <> Columns[0]) and (X < TacListColumn(FHotElement).FBounds.Left + 10) then
      Cursor := crHSplit
    else
      Cursor := crDefault
  end
  else if PtInRect(FColumnsBackBounds, Point(X, Y)) and
          (Columns.Count > 0) and (X < FColumnsBackBounds.Left + 10) then
    Cursor := crHSplit
  else
    Cursor := crDefault;

  if Assigned(FSizeColumn) then
    FSizeColumn.Width := FSizeStartWidth + X - FDragStartPos.X;

  if FSelectionFrame then begin
    FSelectionRect.Right := X;
    FSelectionRect.Bottom := Y;
    if FSelectionRect.Bottom < GetListRect.Top then
      FSelectionRect.Bottom := GetListRect.Top;
    for i := 0 to Items.Count-1 do
      if not Items[i].GetHidden and RectIntersecRect(FSelectionRect, Items[i].FBounds) then
        Items[i].FSelected := true
      else
        Items[i].FSelected := false;
    Paint;
  end;
end;

procedure TacListView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  LastDown: TacListElement;
begin
  inherited;
  if (Button = mbLeft) and (not FDragStarted) then
  begin
    LastDown := FDownElement;
    FDownElement := nil;
    FDragElement := nil;
    if (LastDown <> nil) and (GetElementAt(Point(X, Y)) = LastDown)
      and (FDragElement = nil) and (FSizeColumn = nil) then
    begin
      LastDown.Click(Point(X, Y));
    end;
    FSizeStartWidth := -1;
    FSizeColumn := nil;
    if Assigned(OnClick) then
      OnClick(Self);
  end;

  FDragStarted := False;

  if FSelectionFrame then begin
    FSelectionFrame := false;
    Paint;
  end;
end;

{$IFDEF D2005} // v7
{$endregion}
{$ENDIF}

function TacListView.ItemByCaption(const Caption: string; SelectIt: boolean): TacListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do if Items[i].Caption = Caption then begin
    Result := Items[i];
    Break;
  end;
  if SelectIt then begin
    if (Result <> nil) then SelectedItem := Result else UnSelectAll;
  end;
end;

procedure TacListView.CollapseAll(Repaint : boolean);
var
  i : integer;
begin
  BeginUpdate;
  for i := 0 to Groups.Count - 1 do Groups[i].Expanded := False;
  EndUpdate(Repaint);
end;

procedure TacListView.ExpandAll(Repaint : boolean);
var
  i : integer;
begin
  BeginUpdate;
  for i := 0 to Groups.Count - 1 do Groups[i].Expanded := True;
  EndUpdate(Repaint);
end;

end.
