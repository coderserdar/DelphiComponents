{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit DBUCellTypes;

interface

uses
  Classes,
{$ifndef LINUX}
  Windows, Graphics, Controls, Grids, StdCtrls,
{$else LINUX}
  Types, Qt, QGraphics, QGrids, QControls, QStdCtrls,
{$endif LINUX}
  DBUInterfaces, DBUTypes, DataType;

type

  TDBUCellType = class
  private
    FReadOnly : Boolean;
  protected
    constructor Create; virtual;

    procedure DoDrawCell(ACell : IInfoCell; var Params : TDrawParams); virtual;
    procedure DoDrawEditor( ACell : IEditorCell; var Params : TDrawParams ); virtual;
    procedure SetReadOnly(const AValue: Boolean);
  public
    destructor Destroy; override;

    procedure DrawCell(ACell : IGridCell; var Params : TDrawParams);
    procedure DrawEditor( ACell : IEditorCell; var Params : TDrawParams );

    procedure HandleMessage( ACell : IEditorCell; var Params : THandleWMParams ); virtual;
    procedure HandleCancelMode( ACell : IEditorCell; var Params : THandleCMParams ); virtual;
    procedure GetCursor( ACell : IEditorCell; APoint : TPoint; var ACursor : TCursor ); virtual;

    function GetEditText( ACell : IEditorCell ) : String; virtual;
    procedure SetEditText( ACell : IEditorCell; AValue: string); virtual;

    procedure KeyDown(ACell : IGridCell; var NewKeyState : TKeyState); virtual;
    procedure KeyUp(ACell : IGridCell; var NewKeyState : TKeyState); virtual;

    procedure EditorKeyDown(ACell : IEditorCell; var NewKeyState : TKeyState); virtual;
    procedure EditorKeyUp(ACell : IEditorCell; var NewKeyState : TKeyState); virtual;

    procedure MouseDown(ACell : IGridCell; NewMouseState: TMouseState); virtual;
    procedure MouseMove(ACell : IGridCell; NewMouseState: TMouseMoveState); virtual;
    procedure MouseUp(ACell : IGridCell; NewMouseState: TMouseState); virtual;

    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); virtual;
    procedure EditorMouseMove(ACell : IEditorCell; NewMouseState: TMouseMoveState); virtual;
    procedure EditorMouseUp(ACell : IEditorCell; NewMouseState: TMouseState); virtual;

    function CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer; virtual;
    function CreateDrawCustomizer( ACell : IInfoCell ) : TDBUCustomizer; virtual;

    function ShowEditor(ACell: IInfoCell; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;

    property ReadOnly : Boolean read FReadOnly;
  end;

  TDefaultCustomizer = class( TDBUEditCustomizer )
  private
    FMaxLength : Integer;
    FInplaceEdit : TInplaceEdit;
    FCell : IEditorCell;
    FCellType : TDBUCellType;

    constructor Create( ACell : IEditorCell; ACellType : TDBUCellType );
    procedure SetMaxLength(const Value: Integer);
  public
    property MaxLength : Integer read FMaxLength write SetMaxLength;
  end;

//  TEditableCellType = class(TDBUCellType)
//  end;

  TNormalCellType = class(TDBUCellType)
  end;

  TBlankCellType = class(TDBUCellType)
  protected
    procedure DoDrawCell(ACell : IInfoCell; var Params : TDrawParams); override;
    procedure DoDrawEditor( ACell : IEditorCell; var Params : TDrawParams ); override;

    constructor Create; override;
  end;

  TCheckCellTypeCustomizer = class( TDBUCustomizer )
  private
    FCheckInfoSupplier: ICheckInfoSupplier;
    procedure SetCheckInfoSupplier(const Value: ICheckInfoSupplier);
  public
    property CheckInfoSupplier : ICheckInfoSupplier read FCheckInfoSupplier write SetCheckInfoSupplier;
  end;

  TCheckCellType = class(TDBUCellType)
  private
    FCheckedBitmap : TBitmap;
    FUncheckedBitmap : TBitmap;
    FDisabledCheckedBitmap : TBitmap;
    FDisabledUncheckedBitmap : TBitmap;

    function OverIcon( IconRect : TRect; X, Y: Integer): Boolean;
    function IconRect( EditRect : TRect; ACell : IInfoCell ): TRect;
    procedure DrawCheckBox(Canvas: TCanvas; ACell: IInfoCell;
      var ARect: TRect);
    procedure SetEditorValue(ACell: IEditorCell; NewValue: TValue);
  protected
    constructor Create; override;
  public
    destructor Destroy; override;

    function GetEditText( ACell : IEditorCell ) : String; override;
    procedure SetEditText( ACell : IEditorCell; AValue: string); override;

    procedure DoDrawEditor( ACell : IEditorCell; var Params : TDrawParams ); override;
    procedure DoDrawCell(ACell : IInfoCell; var Params : TDrawParams); override;
    procedure GetCursor( ACell : IEditorCell; APoint : TPoint; var ACursor : TCursor ); override;

    procedure EditorKeyDown(ACell : IEditorCell; var NewKeyState : TKeyState); override;
    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); override;

    function CreateDrawCustomizer( ACell : IInfoCell ) : TDBUCustomizer; override;
    function CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer; override;

    function ShowEditor(ACell: IInfoCell; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
  end;

  TCustomButtonCellType = class(TNormalCellType)
  protected
    function OverButton( BtnRect : TRect; X, Y: Integer): Boolean;

    function ButtonRect( EditRect : TRect; ACell : IInfoCell ): TRect; virtual;

    procedure DrawButton( ACell: IInfoCell; var Params: TDrawParams; IsDown : Boolean  );
    function DoDrawButton( ACell: IInfoCell; var Params: TDrawParams; BtnRect: TRect; IsDown : Boolean  ) : Boolean; virtual;
    procedure DrawButtonEdge( ACell: IInfoCell; var Params: TDrawParams;
      var DrawRect: TRect; IsDown: Boolean); virtual; abstract;
    procedure DrawButtonFace( ACell: IInfoCell; var Params: TDrawParams;
      DrawRect: TRect; IsDown : Boolean  ); virtual; abstract;
  public

    function CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer; override;
    procedure DoDrawEditor( ACell : IEditorCell; var Params : TDrawParams ); override;
    procedure GetCursor( ACell : IEditorCell; APoint : TPoint; var ACursor : TCursor ); override;

    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); override;
    procedure EditorMouseMove(ACell : IEditorCell; NewMouseState: TMouseMoveState); override;
    procedure EditorMouseUp(ACell : IEditorCell; NewMouseState: TMouseState); override;

		procedure EditorKeyDown(ACell : IEditorCell; var NewKeyState : TKeyState); override;
  end;

  THeaderCellType = class(TCustomButtonCellType)
  protected
    constructor Create; override;

    function ButtonRect( EditRect : TRect; ACell : IInfoCell ): TRect; override;
    procedure DrawButtonEdge( ACell: IInfoCell; var Params: TDrawParams;
      var DrawRect: TRect; IsDown: Boolean); override;
    procedure DrawButtonFace( ACell: IInfoCell; var Params: TDrawParams;
      DrawRect: TRect; IsDown : Boolean  ); override;
    procedure DrawHeaderFace( ACell: IInfoCell; var Params : TDrawParams{; IsDown : Boolean }); virtual;
    procedure DoDrawCell(ACell : IInfoCell; var Params : TDrawParams); override;
  public
    function CreateDrawCustomizer( ACell : IInfoCell ) : TDBUCustomizer; override;
  end;

  TClickableHeaderCellType = class(THeaderCellType)
  protected
    function DoDrawButton( ACell: IInfoCell; var Params: TDrawParams;
      BtnRect: TRect; IsDown : Boolean  ) : Boolean; override;
  public
    function CreateDrawCustomizer( ACell : IInfoCell ) : TDBUCustomizer; override;

    procedure MouseDown(ACell : IGridCell; NewMouseState: TMouseState); override;
    procedure MouseMove(ACell : IGridCell; NewMouseState: TMouseMoveState); override;
    procedure MouseUp(ACell : IGridCell; NewMouseState: TMouseState); override;
 end;

  TButtonCellType = class(TCustomButtonCellType)
  private
    FFacePic : TBitmap;
  protected
    constructor Create; override;

    procedure DrawButtonEdge( ACell: IInfoCell; var Params: TDrawParams;
      var DrawRect: TRect; IsDown: Boolean); override;
    procedure DrawButtonFace( ACell: IInfoCell; var Params: TDrawParams;
      DrawRect: TRect; IsDown : Boolean  ); override;

    procedure DoDrawCell(ACell: IInfoCell; var Params: TDrawParams); override;
    function DoDrawButton( ACell: IInfoCell; var Params: TDrawParams;
      BtnRect: TRect; IsDown : Boolean  ) : Boolean; override;
    procedure DoDrawButtonFace( ACell: IInfoCell; var Params: TDrawParams;
      DrawRect: TRect; IsDown : Boolean  );

    procedure LoadBtnFace( APic : TBitmap ); virtual;
    function OverEditButton(ACell: IEditorCell; X, Y: Integer): Boolean;
  public
    destructor Destroy; override;

    procedure GetCursor( ACell : IEditorCell; APoint : TPoint; var ACursor : TCursor ); override;
  end;

  TComboCellType = class(TButtonCellType)
  protected
    constructor Create; override;
  public
    function CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer; override;

    function GetEditText( ACell : IEditorCell ) : String; override;
    procedure SetEditText( ACell : IEditorCell; AValue: string); override;

    procedure HandleMessage( ACell : IEditorCell; var Params : THandleWMParams ); override;
    procedure HandleCancelMode( ACell : IEditorCell; var Params : THandleCMParams ); override;

    procedure MouseDown(ACell : IGridCell; NewMouseState: TMouseState); override;
    procedure MouseMove(ACell : IGridCell; NewMouseState: TMouseMoveState); override;
    procedure MouseUp(ACell : IGridCell; NewMouseState: TMouseState); override;

    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); override;
    procedure EditorMouseMove(ACell : IEditorCell; NewMouseState: TMouseMoveState); override;
    procedure EditorMouseUp(ACell : IEditorCell; NewMouseState: TMouseState); override;
  end;

  TEllipsisCellType = class(TButtonCellType)
  protected
    constructor Create; override;
  end;

  TDBUComboBox = class;
  TDBUComboBoxClass = class of TDBUComboBox;

  THeaderDrawCustomizer = class( TDBUCustomizer )
  private
    FCellType : TDBUCellType;
    FCell : IInfoCell;
    FIconSupplier : IIconSupplier;
    procedure SetIconSupplier(const Value: IIconSupplier);
  public
    constructor Create( ACell : IInfoCell; ACellType : TDBUCellType ); virtual;

    property IconSupplier : IIconSupplier read FIconSupplier write SetIconSupplier;
  end;

  TClickEvent = procedure( Sender : TObject; ACell : IInfoCell; AMouseState : TMouseState ) of object;
  TEditClickEvent = procedure( Sender : TObject; ACell : IInfoCell; AMouseState : TMouseState; var Value : TValue ) of object;

  TButtonTracker = class
  private
    FBtnRect : TRect;
    FTracking : Boolean;
    FPressed : Boolean;
    FOnInvalidate : TNotifyEvent;
    function OverButton( X, Y : Integer ) : Boolean;
  protected
    constructor Create( OnInvalidate : TNotifyEvent );
    procedure StartTracking( BtnRect : TRect );
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);

    property Pressed : Boolean read FPressed;
    property Tracking : Boolean read FTracking;
  end;

  TClickableHeaderDrawCustomizer = class( THeaderDrawCustomizer )
  private
    FCol, FRow : Integer;
    FTracker : TButtonTracker;

    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnClick: TClickEvent;

    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseMove(const Value: TMouseMoveEvent);
    procedure SetOnMouseUp(const Value: TMouseEvent);
    procedure SetCol(const Value: Integer);
    procedure SetRow(const Value: Integer);
    procedure SetOnClick(const Value: TClickEvent);

    procedure Invalidate(Sender: TObject);
    function GetIsDown: Boolean;
    function GetTracking: Boolean;
  protected
    property Col : Integer read FCol write SetCol;
    property Row : Integer read FRow write SetRow;
    property Tracking : Boolean read GetTracking;
    property Tracker : TButtonTracker read FTracker;

    procedure MouseDown(ACell: IGridCell; NewMouseState: TMouseState);
    procedure MouseMove(ACell: IGridCell; NewMouseState: TMouseMoveState);
    procedure MouseUp(ACell: IGridCell; NewMouseState: TMouseState);

    property IsDown : Boolean read GetIsDown;
  public
    constructor Create( ACell : IInfoCell; ACellType : TDBUCellType ); override;
    destructor Destroy; override;

    procedure Reset; override;

    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnClick: TClickEvent read FOnClick write SetOnClick;
  end;

  TSelectionChangeEvent = procedure ( Cell : IEditorCell; var SetValue : TValue ) of object;

  TButtonCellTypeCustomizer = class( TDefaultCustomizer )
  private
    FTracker : TButtonTracker;
    FOnClick: TEditClickEvent;
    FOnSelectionChange: TSelectionChangeEvent;

    procedure SetOnClick(const Value: TEditClickEvent);
    function GetTracking: Boolean;
    procedure SetOnSelectionChange(const Value: TSelectionChangeEvent);
  protected
    procedure Invalidate( Sender : TObject );
    procedure TrackButton(X, Y: Integer);
    procedure StopTracking;
    procedure SelectionChange(ACell: IEditorCell; var AValue: TValue); virtual;
    procedure ButtonClick(ACell: IEditorCell; NewMouseState: TMouseState);

    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); virtual;
    procedure EditorMouseMove(ACell : IEditorCell; NewMouseState: TMouseMoveState); virtual;
    procedure EditorMouseUp(ACell : IEditorCell; NewMouseState: TMouseState); virtual;

    procedure EditorKeyDown(ACell : IEditorCell; var NewKeyState : TKeyState); virtual;
	 	 procedure EditorKeyUp(ACell : IEditorCell; var NewKeyState : TKeyState); virtual;

    property Tracking : Boolean read GetTracking;
    property Tracker : TButtonTracker read FTracker;
    property OnSelectionChange : TSelectionChangeEvent read FOnSelectionChange write SetOnSelectionChange;
  public
    constructor Create( ACell : IEditorCell; ACellType : TDBUCellType );
    destructor Destroy; override;

    procedure Reset; override;
    procedure KillFocus; override;

    property OnClick : TEditClickEvent read FOnClick write SetOnClick;
  end;

  TEllipsisButtonCustomizer = class( TButtonCellTypeCustomizer )
  public
    property OnClick;
    property OnSelectionChange;
  end;

  TComboCellTypeCustomizer = class; // forward
  TComboCellTypeCreateCombo = procedure ( Cell : IEditorCell; Identifier : TObject;
    var ACombo : TDBUComboBox ) of object;

  TPickListOrganizer = class
  private
    FOnCreateCombo: TComboCellTypeCreateCombo;
    procedure SetOnCreateCombo(const Value: TComboCellTypeCreateCombo);
    function CreateCombo(Identifier: TObject): TDBUComboBox;
  private
    FCustomizer : TComboCellTypeCustomizer;
    FCombos : TList;
    FIdentifiers : TList;

    function GetCombos(Identifier: TObject): TDBUComboBox;
    constructor Create(ACustomizer: TComboCellTypeCustomizer);
    property OnCreateCombo : TComboCellTypeCreateCombo read FOnCreateCombo write SetOnCreateCombo;
  public
    destructor Destroy; override;

    property Combos[ Identifier : TObject ] : TDBUComboBox read GetCombos; default;
    procedure AddCombo( Identifier : TObject; ACombo : TDBUComboBox );
  end;

  TComboCellTypeGetStrings = procedure ( Cell : IEditorCell; Combos : TPickListOrganizer;
    var ACombo : TDBUComboBox ) of object;
  TComboSelectionChangeEvent = procedure ( Cell : IEditorCell; ACombo : TDBUComboBox;
    var SetValue : TValue ) of object;

  TComboCellTypeCustomizer = class( TButtonCellTypeCustomizer )
  private
    FPickLists : TPickListOrganizer;
    FComboBox : TDBUComboBox;
    FListVisible: Boolean;
    FOnGetStrings: TComboCellTypeGetStrings;
    FOnComboSelectionChange : TComboSelectionChangeEvent;
    FKillWMChar : Boolean;
    FOldEditStr : String;

    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    function GetPickList: TDBUComboBox;
    procedure SetOnGetStrings(const AValue: TComboCellTypeGetStrings);
    procedure SetOnComboSelectionChange(const Value: TComboSelectionChangeEvent);
    function SelectTextInPickList(ACell: IEditorCell; const AValue: string) :  String;
    function MatchTextInPickList(ACell: IEditorCell; const AValue: string;
      var matchStr, wholeStr: String): String;
    procedure SetOnCreateCombo(const Value: TComboCellTypeCreateCombo);
    function GetOnCreateCombo: TComboCellTypeCreateCombo;
  protected
    property PickList : TDBUComboBox read GetPickList;
    procedure HandleMessage( var Params : THandleWMParams );
    procedure SetEditText(ACell: IEditorCell; AValue: string);
    function FindMatch( Strings : TStrings; var matchStr, resultStr : String;
        var ItemIndex : Integer; FindPartial, CaseSensitive : Boolean ) : Boolean;
    procedure SelectionChange(ACell: IEditorCell; var AValue: TValue); override;
  public
    constructor Create( ACell : IEditorCell; ACellType : TDBUCellType );
    destructor Destroy; override;

    procedure Reset; override;
    procedure KillFocus; override;

    procedure EditorMouseDown(ACell : IEditorCell; NewMouseState: TMouseState); override;
    procedure EditorMouseMove(ACell : IEditorCell; NewMouseState: TMouseMoveState); override;
    procedure EditorMouseUp(ACell : IEditorCell; NewMouseState: TMouseState); override;

    procedure EditorKeyDown(ACell : IEditorCell; var NewKeyState : TKeyState); override;

    property OnCreateCombo : TComboCellTypeCreateCombo read GetOnCreateCombo write SetOnCreateCombo;
    property OnGetStrings : TComboCellTypeGetStrings read FOnGetStrings write SetOnGetStrings;
    property OnComboSelectionChange : TComboSelectionChangeEvent read FOnComboSelectionChange write SetOnComboSelectionChange;
  end;

  TDBUListBox = class( TCustomListBox )
  private
    FDBUComboBox: TDBUComboBox;
{$ifdef LINUX}
    FDontClose : Boolean;
{$else LINUX}
    FSearchText: String;
    FSearchTickCount: Longint;

    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
{$endif LINUX}
    procedure SetDBUComboBox(const Value: TDBUComboBox);
  protected
{$ifndef LINUX}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
{$else LINUX}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure InitWidget; override;
    function WidgetFlags: Integer; override;
{$endif LINUX}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DBUComboBox : TDBUComboBox read FDBUComboBox write SetDBUComboBox;
  end;

  TDBUStrings = class( TStringList )
  private
    FDependent : TStrings;
    FIsChanging : Boolean;
    FIsChanged : Boolean;
    FHasRunChanging : Boolean;

    procedure DoChanged;
    procedure DoChanging;
    procedure Initialize( DependentStrings : TStrings );
  protected
    procedure SetUpdateState(Updating: Boolean); override;
    procedure Changing; override;
    procedure Changed; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDBUComboBox = class
  private
    FCustomizer : TComboCellTypeCustomizer;
    FCacheValues : Boolean;
    FOnlyLegalValues : Boolean;
    FAutoComplete : Boolean;
    FListBox : TDBUListBox;
    FItems: TDBUStrings;
    FCaseSensitive: Boolean;
    FEmptyItem: TEmptyItem;

    procedure SetCacheValues(const AValue: Boolean);
    procedure SetOnlyLegalValues(const Value: Boolean);
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure Initialize( ACustomizer : TComboCellTypeCustomizer );
    function GetItems: TStrings;
    function CreateStrings : TDBUStrings;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetEmptyItem(const Value: TEmptyItem);
  protected
    constructor Create;
  public
    destructor Destroy; override;

    property CacheValues : Boolean read FCacheValues write SetCacheValues;
    property OnlyLegalValues : Boolean read FOnlyLegalValues write SetOnlyLegalValues;
    property AutoComplete : Boolean read FAutoComplete write SetAutoComplete;
    property CaseSensitive : Boolean read FCaseSensitive write SetCaseSensitive;
    property Items : TStrings read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property EmptyItem : TEmptyItem read FEmptyItem write SetEmptyItem;
  end;

var
  NormalCellType : TNormalCellType;
  HeaderCellType : THeaderCellType;
  ClickableHeaderCellType : TClickableHeaderCellType;
  CheckCellType : TCheckCellType;
  ComboCellType : TComboCellType;
  ButtonCellType : TEllipsisCellType;
  EllipsisCellType : TEllipsisCellType;
  BlankCellType : TBlankCellType;

implementation

uses
  Math, SysUtils,
{$ifndef LINUX}
  Forms, Messages,
{$else}
  QForms, QButtons,
{$endif LINUX}
  CommonLib, DerivedDataType, DBUFormatter;

const
  MARGIN_MIN = 2;

procedure ValueContentHeightAndWidth( AValue : TValue; ACanvas : TCanvas;
  var Height, Width : Integer );
var
  APic : TPicture;
  AStr : String;
begin
  if AValue.DataType is TPictureType then
  begin
    APic := AsPicture(AValue);
    if APic = nil then
    begin
      Height := 0;
      Width := 0;
    end
    else
    begin
      Height := APic.Height;
      Width := APic.Width;
    end;
  end
  else
  begin
    AStr := AsString(AValue);
    Height := ACanvas.TextHeight( AStr );
    Width := ACanvas.TextWidth( AStr );
    if fsItalic in ACanvas.Font.Style then
      Inc( Width, 3 * Abs(ACanvas.Font.Size) div 4 );
  end;
end;

var
  TmpPic : TPicture;

function GetStartCoord( AMin, AMax, ASize, AMargin : Integer; Align : Integer ) : Integer;
var
  coord : Integer;
begin
  coord := -100;
  case Align of
    -1 : coord := AMin + AMargin;
    0 : coord := AMin + Max( (AMax-AMin-ASize-AMargin) div 2, 0 );
    1 : coord := AMin + Max( (AMax-AMin-ASize-AMargin), 0 );
    else
      Assert( False );
  end;
  Result := coord;
end;

function GetTopCoord( Top, Bottom, Height, Margin : Integer; Align : TVerticalAlignment ) : Integer;
var
  idx : Integer;
begin
  idx := -2;
  case Align of
    vaTop : idx := -1;
    vaMiddle : idx := 0;
    vaBottom : idx := 1;
    else
      Assert( False );
  end;

  Result := GetStartCoord( Top, Bottom, Height, Margin, idx );
end;

function GetLeftCoord( Left, Right, Width, Margin : Integer; Align : TAlignment ) : Integer;
var
  idx : Integer;
begin
  idx := -2;
  case Align of
    taLeftJustify : idx := -1;
    taCenter : idx := 0;
    taRightJustify : idx := 1;
    else
      Assert( False );
  end;

  Result := GetStartCoord( Left, Right, Width, Margin, idx );
end;

procedure RenderPic( Canvas : TCanvas; APic : TPicture; ARect : TRect; TopLeft : TPoint );

  procedure AdjustPic( var APic : TPicture; ThisRect : TRect );
  var
    TooHigh, TooWide : Boolean;
    NewHeight, NewWidth : Integer;
  begin
    with ARect do
    begin
      NewHeight := Bottom - Top;
      NewWidth := Right - Left;
    end;

    if not APic.Graphic.Empty then
    begin
      TooHigh := APic.Height > NewHeight;
      TooWide := APic.Width > NewWidth;
      if TooHigh or TooWide then
      begin
        if TmpPic <> APic then
          TmpPic.Assign( APic );

        if TooHigh then
          TmpPic.Graphic.Height := NewHeight;
        if TooWide then
          TmpPic.Graphic.Width := NewWidth;

        APic := TmpPic;
      end;
    end;
  end;

begin
{$ifdef LINUX}
  Exit;
{$endif LINUX}

  if APic <> nil then
  begin
    AdjustPic( APic, ARect );
    Canvas.Draw( TopLeft.X, TopLeft.Y, APic.Graphic );
  end;
end;

procedure RenderValue( Canvas : TCanvas; ARect : TRect; Align : TDBUAlign;
  Margin : TDBUMargin; AValue : TValue );
var
  Height, Width, ALeft, ATop : Integer;
  NewRect : TRect;
begin
  ValueContentHeightAndWidth( AValue, Canvas, Height, Width );

  with ARect do
  begin
//    if AValue.DataType is TPictureType then
//      Margin.H := 0;

    ALeft := GetLeftCoord( Left, Right, Width, Margin.H, Align.H );
    ATop := GetTopCoord( Top, Bottom, Height, Margin.V, Align.V );

    with Canvas do
    begin
      NewRect := Rect( ALeft, ATop, Right, Bottom );

      if Align.H = taRightJustify then
        NewRect.Right := NewRect.Right-Margin.H
      else
        NewRect.Right := NewRect.Right-MARGIN_MIN;

      with NewRect do
        if (Left >= Right) or (Top >= Bottom) then
          Exit;

      if AValue.DataType is TPictureType then
        RenderPic( Canvas, AsPicture(AValue), NewRect, Point( ALeft, ATop ) )
      else
        TextRect(NewRect, ALeft, ATop, AsString( AValue ));
    end;
  end;
end;

procedure RenderIcon(Canvas : TCanvas; var ARect: TRect; Align : TDBUAlign;
    Margin : TDBUMargin; APic : TPicture );
var
  ALeft, ATop, Width, Height : Integer;
begin
  TmpPic.Assign( APic );
  Width := APic.Width;
  Height := APic.Height;
  with ARect do
  begin
    ALeft := GetLeftCoord( Left, Right, Width, Margin.H, Align.H );
    ATop := GetTopCoord( Top, Bottom, Height, Margin.V, Align.V );
  end;

  RenderPic( Canvas, TmpPic, ARect, Point( ALeft, ATop ) );

  if Align.H = taLeftJustify then
    ARect.Left := Min( ALeft + Width + Margin.H, ARect.Right )
  else
    ARect.Right := Max( ALeft - Margin.H, ARect.Left );
end;

function EditorRect( ACell : IEditorCell ) : TRect;
begin
  Result := Rect( 0, 0, ACell.GetEditor.GetWidth, ACell.GetEditor.GetHeight )
end;

{$ifndef LINUX}
function DoRenderButtonEdge(DC: THandle; const Client: TRect;
  IsDown, IsHeader : Boolean): TRect;
const
  NewStyle = True;
var
  R : TRect;
begin
  R := Client;

  if IsDown then
  begin
    DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_ADJUST);              { grey //black     }
    DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT or BF_ADJUST);          { white //btnhilite }
  end
  else
  begin
     // Headers don't stand out as much
    if not IsHeader then
      DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT or BF_ADJUST);        { black }

    DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT or BF_ADJUST);             { white //btnhilite }
    DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_ADJUST);         { grey // btnshadow }

    if not IsHeader then
    begin
      Inc( R.Left );
      Inc( R.Top );
    end;
  end;

(*  if IsHeader then
  begin
    Dec( R.Right );
    Dec( R.Bottom );
  end;
 *)
  Result := R;
  if IsDown then OffsetRect(Result, 1, 1);
end;
{$else LINUX}
function DoRenderButtonEdge(Canvas: TCanvas; const Client: TRect;
  IsDown, IsHeader : Boolean): TRect;
const
  NewStyle = True;
var
  R : TRect;
begin
  R := Client;

  if IsDown then
  begin
    DrawEdge(Canvas, R, esNone, esLowered, [ebLeft, ebTop, ebRight, ebBottom]);
    InflateRect( R, -1, -1 );
  end
  else
  begin
     // Headers don't stand out as much
    if not IsHeader then
    begin
      DrawEdge(Canvas, R, esRaised, esRaised, [ebLeft, ebTop, ebRight, ebBottom]);
      Dec( R.Right );
      Dec( R.Bottom );
    end
    else
    begin
      DrawEdge(Canvas, R, esNone, esRaised, [ebLeft, ebTop, ebRight, ebBottom]);
//  DrawEdge(Canvas, R, esRaised, esNone, [ebRight, ebBottom]);
    end;
    
    InflateRect( R, -1, -1 );

    if not IsHeader then
    begin
      Inc( R.Left );
      Inc( R.Top );
    end;
  end;

  if IsHeader then
  begin
    Dec( R.Right );
    Dec( R.Bottom );
  end;

  Result := R;
  if IsDown then OffsetRect(Result, 1, 1);
end;
(*
function DoRenderButtonEdge(Canvas: TCanvas; const Client: TRect;
  IsDown, IsHeader : Boolean): TRect;
const
  NewStyle = True;
var
  R : TRect;
begin
  R := Client;

  if IsDown then
  begin
    DrawEdge(Canvas, R, esLowered, esNone, [ebLeft, ebTop]);
    DrawEdge(Canvas, R, esLowered, esNone, [ebRight, ebBottom]);
    InflateRect( R, -1, -1 );
  end
  else
  begin
     // Headers don't stand out as much
    if not IsHeader then
    begin
      DrawEdge(Canvas, R, esRaised, esNone, [ebRight, ebBottom]);
      Dec( R.Right );
      Dec( R.Bottom );
    end;

    DrawEdge(Canvas, R, esRaised, esNone, [ebLeft, ebTop]);
    DrawEdge(Canvas, R, esRaised, esNone, [ebRight, ebBottom]);
    InflateRect( R, -1, -1 );

    if not IsHeader then
    begin
      Inc( R.Left );
      Inc( R.Top );
    end;
  end;

  if IsHeader then
  begin
    Dec( R.Right );
    Dec( R.Bottom );
  end;

  Result := R;
  if IsDown then OffsetRect(Result, 1, 1);
end;
*)
{$endif LINUX}

function RenderButtonEdge(Canvas: TCanvas; const Client: TRect;
  IsDown, IsHeader, FillButton : Boolean): TRect;
begin
  if FillButton then
    Canvas.FillRect( Client );
{$ifndef LINUX}
  Result := DoRenderButtonEdge( Canvas.Handle, Client, IsDown, IsHeader );
{$else LINUX}
  Result := DoRenderButtonEdge( Canvas, Client, IsDown, IsHeader );
{$endif LINUX}
end;

{ TDBUCellType }

constructor TDBUCellType.Create;
begin
  inherited Create;
end;

function TDBUCellType.CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer;
begin
  Result := TDefaultCustomizer.Create(ACell, Self);// nil;
end;

function TDBUCellType.CreateDrawCustomizer(
  ACell: IInfoCell): TDBUCustomizer;
begin
  Result := nil;
end;

destructor TDBUCellType.Destroy;
begin
  inherited Destroy;
end;

function TDBUCellType.ShowEditor(ACell: IInfoCell; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
begin
  Result := False;
end;

procedure TDBUCellType.DoDrawCell(ACell : IInfoCell; var Params : TDrawParams);
var
  AFormatter : TDBUFormatter;
  AValue : TValue;
begin
  AFormatter := ACell.GetFormatter( Params.DrawState );
  AValue := ACell.GetDrawValue;

  with Params do
    RenderValue( Canvas, Rect, AFormatter.Align, AFormatter.Margin, AValue );
end;

procedure TDBUCellType.DoDrawEditor(ACell: IEditorCell;
  var Params: TDrawParams);
var
  NewRect : TRect;
begin
  with Params.Rect do
    NewRect := Rect( Left + 2, Max( Top, Min( Bottom-15, Top + 2 ) ), Right -2, Bottom );
  Params.Rect := NewRect;
end;

procedure TDBUCellType.DrawCell(ACell : IGridCell; var Params : TDrawParams);
var
  SavedStyle : TDBUTextStyle;
  AFormatter : TDBUFormatter;
begin
  SavedStyle := AssignCanvasToTextStyle(Params.Canvas);

  AFormatter := ACell.GetFormatter( Params.DrawState );
  AFormatter.PrepareCanvas( Params.Canvas );
  AFormatter.DrawBackground( Params.Canvas, Params.Rect );

  DoDrawCell( ACell, Params );
  ApplyTextStyleToCanvas( SavedStyle, Params.Canvas, Params.DrawState, False );
end;

procedure TDBUCellType.DrawEditor(ACell: IEditorCell;
  var Params: TDrawParams);
begin
  DoDrawEditor( ACell, Params );
end;

procedure TDBUCellType.EditorKeyDown(ACell: IEditorCell;
  var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TDBUCellType.EditorKeyUp(ACell: IEditorCell;
  var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TDBUCellType.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  // nothing
end;

procedure TDBUCellType.EditorMouseMove(ACell: IEditorCell;
  NewMouseState: TMouseMoveState);
begin
  // nothing
end;

procedure TDBUCellType.EditorMouseUp(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  // nothing
end;

procedure TDBUCellType.GetCursor(ACell: IEditorCell; APoint: TPoint;
  var ACursor: TCursor);
begin
  // nothing
end;

function TDBUCellType.GetEditText(ACell: IEditorCell): String;
begin
  Result := AsString( ACell.GetEditorValue );
end;

procedure TDBUCellType.HandleCancelMode(ACell : IEditorCell; var Params: THandleCMParams);
begin
  // nothing
end;

procedure TDBUCellType.HandleMessage(ACell : IEditorCell; var Params: THandleWMParams);
begin
  // nothing
end;

procedure TDBUCellType.KeyDown(ACell: IGridCell; var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TDBUCellType.KeyUp(ACell: IGridCell; var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TDBUCellType.MouseDown(ACell: IGridCell;
  NewMouseState: TMouseState);
begin
  // nothing
end;

procedure TDBUCellType.MouseMove(ACell: IGridCell;
  NewMouseState: TMouseMoveState);
begin
  // nothing
end;

procedure TDBUCellType.MouseUp(ACell: IGridCell;
  NewMouseState: TMouseState);
begin
  // nothing
end;

procedure TDBUCellType.SetEditText(ACell: IEditorCell;
  AValue: string);
var
  ACustomizer : TDBUCustomizer;
  idxCursor, Start, Diff : Integer;
begin
  ACustomizer := ACell.GetCustomizer;
  if (ACustomizer is TDefaultCustomizer) and
     (TDefaultCustomizer(ACustomizer).MaxLength > 0) then
    with ACustomizer as TDefaultCustomizer do
    begin
      if Assigned(FInplaceEdit) then
      begin
        Diff := Length(AValue) - MaxLength;
        if Diff > 0 then
        begin
          Start := Max(1, FInplaceEdit.SelStart - Diff + 1);
          Delete(AValue, Start, Diff);
          idxCursor := FInplaceEdit.SelStart;
          ACell.SetInplaceEditText( AValue );
          FInplaceEdit.SelStart := idxCursor - Diff;
        end;
      end;
    end;

  ACell.SetEditorValue( ValueFromString( AValue ) );
end;

procedure TDBUCellType.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
end;

{ TCustomButtonCellType }

function TCustomButtonCellType.OverButton(BtnRect: TRect; X,
  Y: Integer): Boolean;
begin
  Result := PtInRect( BtnRect, Point( X, Y ) );
end;

function TCustomButtonCellType.ButtonRect( EditRect : TRect; ACell : IInfoCell ): TRect;
begin
  with EditRect do
    Result := Rect( Max(Right - 16, Left  ), Top, Right, Bottom );
end;

procedure TCustomButtonCellType.DrawButton( ACell: IInfoCell;
  var Params: TDrawParams; IsDown : Boolean );
var
  NewRect, BtnRect : TRect;
begin
  BtnRect := ButtonRect( Params.Rect, ACell );
  NewRect := Rect( Params.Rect.Left, Params.Rect.Top, Max( BtnRect.Left, Params.Rect.Left + 2), Max(BtnRect.Bottom, Params.Rect.Top + 2) );
  if DoDrawButton( ACell, Params, BtnRect, IsDown ) then
    Params.Rect := NewRect;
end;

function TCustomButtonCellType.DoDrawButton( ACell: IInfoCell; var Params:
  TDrawParams; BtnRect: TRect; IsDown : Boolean  ) : Boolean;
var
  SavedStyle : TDBUTextStyle;
  ACanvas : TCanvas;
begin
  ACanvas := Params.Canvas;

  SavedStyle := AssignCanvasToTextStyle(ACanvas);
  ACanvas.Brush.Color := clBtnFace;
  DrawButtonEdge( ACell, Params, BtnRect, IsDown );
  DrawButtonFace( ACell, Params, BtnRect, IsDown );
  ApplyTextStyleToCanvas( SavedStyle, ACanvas, Params.DrawState, False );
  Result := True;
end;

procedure TCustomButtonCellType.DoDrawEditor(ACell: IEditorCell;
  var Params: TDrawParams);
var
  IsDown : Boolean;
begin
  if ACell.GetCustomizer <> nil then
    IsDown := TButtonCellTypeCustomizer( ACell.GetCustomizer ).FTracker.Pressed
  else
    IsDown := False;

  DrawButton( ACell, Params, IsDown );
  inherited;
end;

function TCustomButtonCellType.CreateEditCustomizer(ACell: IEditorCell): TDBUCustomizer;
begin
  Result := TButtonCellTypeCustomizer.Create( ACell, Self );
end;

procedure TCustomButtonCellType.GetCursor(ACell: IEditorCell; APoint: TPoint;
  var ACursor: TCursor);
begin
  ACursor := crArrow;
end;

procedure TCustomButtonCellType.EditorKeyDown(ACell: IEditorCell;
  var NewKeyState: TKeyState);
var
  ACustomizer : TDBUCustomizer;
begin
  inherited;

  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TButtonCellTypeCustomizer then
    TButtonCellTypeCustomizer(ACustomizer).EditorKeyDown( ACell, NewKeyState );
end;

procedure TCustomButtonCellType.EditorMouseMove(ACell: IEditorCell;
  NewMouseState: TMouseMoveState);
var
  ACustomizer : TDBUCustomizer;
begin
  inherited;

  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TButtonCellTypeCustomizer then
    TButtonCellTypeCustomizer(ACustomizer).EditorMouseMove( ACell, NewMouseState );
end;

procedure TCustomButtonCellType.EditorMouseUp(ACell: IEditorCell;
  NewMouseState: TMouseState);
var
  ACustomizer : TDBUCustomizer;
begin
  inherited;

  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TButtonCellTypeCustomizer then
    TButtonCellTypeCustomizer(ACustomizer).EditorMouseUp( ACell, NewMouseState );
end;

procedure TCustomButtonCellType.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
var
  ACustomizer : TDBUCustomizer;
begin
  inherited;

  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TButtonCellTypeCustomizer then
    TButtonCellTypeCustomizer(ACustomizer).EditorMouseDown( ACell, NewMouseState );
end;

{ THeaderCellType }

constructor THeaderCellType.Create;
begin
  inherited;
  SetReadOnly( True );
end;

procedure THeaderCellType.DoDrawCell(ACell: IInfoCell; var Params : TDrawParams );
begin
  DrawButton( ACell, Params, False );
end;

function THeaderCellType.CreateDrawCustomizer(
  ACell: IInfoCell): TDBUCustomizer;
begin
  Result := THeaderDrawCustomizer.Create( ACell, Self );
end;

procedure THeaderCellType.DrawButtonEdge( ACell: IInfoCell;
  var Params: TDrawParams; var DrawRect: TRect; IsDown: Boolean);
begin
  DrawRect := RenderButtonEdge( Params.Canvas, DrawRect, IsDown, True, True );
end;

procedure THeaderCellType.DrawButtonFace(ACell: IInfoCell;
  var Params: TDrawParams; DrawRect: TRect; IsDown: Boolean);
begin
  Params.Rect := DrawRect;
  DrawHeaderFace( ACell, Params );
end;

procedure THeaderCellType.DrawHeaderFace( ACell: IInfoCell; var Params : TDrawParams );
var
  OldColor : TColor;
  OldRect : TRect;
  DrawCustomizer : TDBUCustomizer;
  APic : TPicture;
  Align : TDBUAlign;
  Margin : TDBUMargin;
begin
  DrawCustomizer := ACell.GetCustomizer;
  if DrawCustomizer is THeaderDrawCustomizer then
    with DrawCustomizer as THeaderDrawCustomizer, Params do
    begin
      if Assigned(IconSupplier) and
         IconSupplier.HasIcon( ACell ) then
      begin
        APic := TmpPic;
        Align := DBUAlign( vaMiddle, taRightJustify );
        Margin := DBUMargin( 0, 0 );
        IconSupplier.GetIcon( ACell, TmpPic, Align, Margin );
        RenderIcon( Canvas, Rect, Align, Margin, APic );
      end;
    end;

  OldRect := Params.Rect;
  if not ACell.GetEnabled then
  begin
    with OldRect do
      Params.Rect := Rect( Left+1, Top+1, Right, Bottom );
    OldColor := Params.Canvas.Font.Color;
    Params.Canvas.Font.Color := clCaptionText;
    inherited DoDrawCell( ACell, Params );
    Params.Canvas.Brush.Style := bsClear;
    Params.Canvas.Font.Color := OldColor;
    OffsetRect( Params.Rect, -1, -1 );
  end;

  inherited DoDrawCell( ACell, Params );
end;

function THeaderCellType.ButtonRect(EditRect: TRect;
  ACell: IInfoCell): TRect;
begin
  Result := EditRect;
end;

{ TClickableHeaderCellType }

function TClickableHeaderCellType.CreateDrawCustomizer(
  ACell: IInfoCell): TDBUCustomizer;
begin
  Result := TClickableHeaderDrawCustomizer.Create( ACell, Self );
end;

function TClickableHeaderCellType.DoDrawButton(ACell: IInfoCell;
  var Params: TDrawParams; BtnRect: TRect; IsDown: Boolean) : Boolean;
var
  DrawCustomizer : TDBUCustomizer;
begin
  DrawCustomizer := ACell.GetCustomizer;
  if DrawCustomizer is TClickableHeaderDrawCustomizer then
      IsDown := TClickableHeaderDrawCustomizer( DrawCustomizer ).IsDown;

  Result := inherited DoDrawButton(ACell, Params, BtnRect, IsDown);
end;

procedure TClickableHeaderCellType.MouseDown(ACell: IGridCell;
  NewMouseState: TMouseState);
var
  DrawCustomizer : TDBUCustomizer;
begin
  inherited;

  DrawCustomizer := ACell.GetCustomizer;
  if DrawCustomizer is TClickableHeaderDrawCustomizer then
    TClickableHeaderDrawCustomizer( DrawCustomizer ).MouseDown( ACell, NewMouseState );
end;

procedure TClickableHeaderCellType.MouseMove(ACell: IGridCell;
  NewMouseState: TMouseMoveState);
var
  DrawCustomizer : TDBUCustomizer;
begin
  inherited;

  DrawCustomizer := ACell.GetCustomizer;
  if DrawCustomizer is TClickableHeaderDrawCustomizer then
    TClickableHeaderDrawCustomizer( DrawCustomizer ).MouseMove( ACell, NewMouseState );
end;

procedure TClickableHeaderCellType.MouseUp(ACell: IGridCell;
  NewMouseState: TMouseState);
var
  DrawCustomizer : TDBUCustomizer;
begin
  inherited;

  DrawCustomizer := ACell.GetCustomizer;
  if DrawCustomizer is TClickableHeaderDrawCustomizer then
    TClickableHeaderDrawCustomizer( DrawCustomizer ).MouseUp( ACell, NewMouseState );
end;

{ TButtonCellType }

constructor TButtonCellType.Create;
begin
  inherited;

  FFacePic := TBitmap.Create;
end;

destructor TButtonCellType.Destroy;
begin
  inherited;

  FFacePic.Free;
end;

function TButtonCellType.OverEditButton(ACell: IEditorCell;
  X, Y : Integer): Boolean;
begin
  Result := OverButton( ButtonRect( EditorRect(ACell), ACell ), X, Y  );
end;

procedure TButtonCellType.GetCursor(ACell: IEditorCell; APoint: TPoint;
  var ACursor: TCursor);
begin
  if OverEditButton( ACell, APoint.X, APoint.Y ) then
    ACursor := crArrow;
end;

procedure TButtonCellType.DoDrawCell(ACell: IInfoCell; var Params : TDrawParams );
begin
  if (gdFocused in Params.DrawState) and
     ACell.CanEditModify then
    DrawButton( ACell, Params, False );

  inherited;
end;

procedure TButtonCellType.LoadBtnFace(APic: TBitmap);
begin
  // Nothing;
end;

procedure TButtonCellType.DrawButtonFace( ACell: IInfoCell;
  var Params: TDrawParams; DrawRect: TRect; IsDown : Boolean  );
begin
  DoDrawButtonFace(ACell, Params, DrawRect, IsDown);
end;

procedure TButtonCellType.DoDrawButtonFace(ACell: IInfoCell;
  var Params: TDrawParams; DrawRect: TRect; IsDown: Boolean);
{$ifndef LINUX}
var
  MaxHeight, MaxWidth, PicHeight, PicWidth, ALeft, ATop, ARight, ABottom : Integer;
  OldCM : TCopyMode;
{$endif LINUX}
begin
  LoadBtnFace( FFacePic );

{$ifndef LINUX}
  if FFacePic <> nil then
  begin
    MaxWidth := Max( (DrawRect.Right - DrawRect.Left) , 0);
    MaxHeight := Max( (DrawRect.Bottom - DrawRect.Top) , 0);

    PicHeight := FFacePic.Height;
    PicWidth := FFacePic.Width;

    ALeft := DrawRect.Left + (MaxWidth - PicWidth) div 2;
    ATop := DrawRect.Top + (MaxHeight - PicHeight) div 2;
    ARight := ALeft + Min( PicWidth, MaxWidth );
    ABottom := ATop + Min( PicHeight, MaxHeight );

    with Params.Canvas do
    begin
      OldCM := CopyMode;
      CopyMode := cmSrcAnd;
      CopyRect( Rect( ALeft, ATop, ARight, ABottom ), FFacePic.Canvas, Rect( 0, 0, PicWidth, PicHeight ) );
      CopyMode := OldCM;
    end;
  end;
{$endif LINUX}
end;

function TButtonCellType.DoDrawButton(ACell: IInfoCell;
  var Params: TDrawParams; BtnRect: TRect; IsDown: Boolean) : Boolean;
begin
  // FIXA MVJ: LGE vill rita bara knapp!
  if BtnRect.Left >= {10 +} Params.Rect.Left then
  begin
    InflateRect( BtnRect, -1, -1 );
    Result := inherited DoDrawButton(ACell, Params, BtnRect, IsDown);
  end
  else
    Result := False;
end;

procedure TButtonCellType.DrawButtonEdge(ACell: IInfoCell;
  var Params: TDrawParams; var DrawRect: TRect; IsDown: Boolean);
begin
  DrawRect := RenderButtonEdge( Params.Canvas, DrawRect, IsDown, False, True );
end;

{ TComboCellType }

constructor TComboCellType.Create;
{$ifndef LINUX}
var
  TmpBtm : TBitmap;
{$else}
{$endif LINUX}
begin
  inherited;

{$ifndef LINUX}
  TmpBtm := TBitmap.Create;
  TmpBtm.Handle := LoadBitmap(0, PChar( OBM_COMBO ) ); {DELPHI2: 32738}
  FFacePic.Width := 8;
  FFacePic.Height := 4;
  FFacePic.Canvas.CopyRect(Rect(0,0,8,4), TmpBtm.Canvas, Rect(2,4,10,8));
  TmpBtm.Free;
{$endif}
end;

function TComboCellType.CreateEditCustomizer( ACell : IEditorCell ) : TDBUCustomizer;
begin
  Result := TComboCellTypeCustomizer.Create( ACell, Self );
end;

procedure TComboCellType.MouseDown(ACell : IGridCell; NewMouseState: TMouseState);
begin
  inherited MouseDown( ACell, NewMouseState );
end;

procedure TComboCellType.MouseMove(ACell : IGridCell; NewMouseState: TMouseMoveState);
begin
  inherited MouseMove( ACell, NewMouseState );
end;

procedure TComboCellType.MouseUp(ACell : IGridCell; NewMouseState: TMouseState);
begin
  inherited MouseUp( ACell, NewMouseState );
end;

procedure TComboCellType.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  inherited EditorMouseDown(ACell, NewMouseState);
end;

procedure TComboCellType.EditorMouseMove(ACell: IEditorCell;
  NewMouseState: TMouseMoveState);
begin
  inherited EditorMouseMove( ACell, NewMouseState );
end;

procedure TComboCellType.EditorMouseUp(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  inherited EditorMouseUp(ACell, NewMouseState);
end;

procedure TComboCellType.HandleCancelMode(ACell : IEditorCell; var Params: THandleCMParams);
{$ifndef LINUX}
var
  ACustomizer : TDBUCustomizer;
  ASender : TControl;
{$endif LINUX}
begin
{$ifndef LINUX}
  ACustomizer := ACell.GetCustomizer;
  ASender := Params.Message.Sender;

  if (ASender <> ACell.GetEditor.SelfEdit) and
     (ACustomizer is TComboCellTypeCustomizer) then
  begin
    with ACell.GetCustomizer as TComboCellTypeCustomizer do
    begin
      if not Assigned( FComboBox ) or
         (ASender <> FComboBox.FListBox) then
        CloseUp(False);
    end;
  end;
{$endif LINUX}
end;

procedure TComboCellType.HandleMessage(ACell : IEditorCell; var Params: THandleWMParams);
var
  ACustomizer : TDBUCustomizer;
begin
  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TComboCellTypeCustomizer then
    case Params.WMType of
      CancelMode : TComboCellTypeCustomizer(ACustomizer).StopTracking;
      KillFocus : TComboCellTypeCustomizer(ACustomizer).CloseUp(False);
    else
        TComboCellTypeCustomizer(ACustomizer).HandleMessage( Params );
    end;
end;

procedure TComboCellType.SetEditText(ACell: IEditorCell;
  AValue: string);
var
  ACustomizer : TDBUCustomizer;
begin
  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TComboCellTypeCustomizer then
    TComboCellTypeCustomizer(ACustomizer).SetEditText(ACell, AValue);
end;

function TComboCellType.GetEditText(ACell: IEditorCell): String;
var
  ACustomizer : TDBUCustomizer;
begin
  Result := inherited GetEditText(ACell);
  ACustomizer := ACell.GetCustomizer;
  if ACustomizer is TComboCellTypeCustomizer then
    TComboCellTypeCustomizer(ACustomizer).FOldEditStr := Result;
end;

{ THeaderDrawCustomizer }

constructor THeaderDrawCustomizer.Create( ACell : IInfoCell; ACellType : TDBUCellType );
begin
  inherited Create;
  FCell := ACell;
  FCellType := ACellType;
  Reset;
end;

procedure THeaderDrawCustomizer.SetIconSupplier(
  const Value: IIconSupplier);
begin
  FIconSupplier := Value;
end;

{ TClickableHeaderDrawCustomizer }

constructor TClickableHeaderDrawCustomizer.Create( ACell : IInfoCell; ACellType : TDBUCellType );
begin
  inherited;
  FTracker := TButtonTracker.Create( Invalidate );
end;

destructor TClickableHeaderDrawCustomizer.Destroy;
begin
  inherited;
  FTracker.Destroy;
end;

function TClickableHeaderDrawCustomizer.GetIsDown: Boolean;
var
  ARect : TRect;
begin
  ARect := FCell.GetGrid.CellRect( FCell.GetCol, FCell.GetRow );
  Result := FTracker.Pressed and EqualRect(FTracker.FBtnRect, ARect);
end;

procedure TClickableHeaderDrawCustomizer.Invalidate( Sender : TObject );
begin
  FCell.GetGrid.InvalidateCell( FCell.Col, FCell.Row );
end;

procedure TClickableHeaderDrawCustomizer.Reset;
begin
  inherited;
//  FCol := -1;
//  FRow := -1;
//  FTracking := False;
//  FIsDown := False;
end;

procedure TClickableHeaderDrawCustomizer.SetCol(const Value: Integer);
begin
  FCol := Value;
end;

procedure TClickableHeaderDrawCustomizer.SetOnClick(
  const Value: TClickEvent);
begin
  FOnClick := Value;
end;

procedure TClickableHeaderDrawCustomizer.SetOnMouseDown(
  const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
end;

procedure TClickableHeaderDrawCustomizer.SetOnMouseMove(
  const Value: TMouseMoveEvent);
begin
  FOnMouseMove := Value;
end;

procedure TClickableHeaderDrawCustomizer.SetOnMouseUp(
  const Value: TMouseEvent);
begin
  FOnMouseUp := Value;
end;

procedure TClickableHeaderDrawCustomizer.SetRow(const Value: Integer);
begin
  FRow := Value;
end;

procedure TClickableHeaderDrawCustomizer.MouseDown(ACell : IGridCell; NewMouseState: TMouseState);
var
  R : TRect;
begin
  if (NewMouseState.Button = mbLeft) then
  begin
    Assert( FCellType is TCustomButtonCellType );
    R := ACell.GetGrid.CellRect( ACell.GetCol, ACell.GetRow );
    if ( FCellType as TCustomButtonCellType ).OverButton( R, NewMouseState.X, NewMouseState.Y ) then
    begin
      Tracker.StartTracking( R );
      Tracker.TrackButton(NewMouseState.X, NewMouseState.Y);
    end;
  end;

  if Assigned( OnMouseDown ) then
    with NewMouseState do
      OnMouseDown( Self, Button, Shift, X, Y );
end;

procedure TClickableHeaderDrawCustomizer.MouseMove(ACell : IGridCell; NewMouseState: TMouseMoveState);
begin
  if Tracking then
  begin
    if not (ssLeft in NewMouseState.Shift ) then
    begin
      Tracker.StopTracking;
      if Assigned( OnMouseUp ) then
        with NewMouseState do
         OnMouseUp( Self, mbLeft, Shift, X, Y );
    end
    else
      Tracker.TrackButton( NewMouseState.X, NewMouseState.Y );
  end;

  if Assigned( OnMouseMove ) then
     with NewMouseState do
       OnMouseMove( Self, Shift, X, Y );
end;

procedure TClickableHeaderDrawCustomizer.MouseUp(ACell : IGridCell; NewMouseState: TMouseState);
var
  DoClick : Boolean;
begin
  if Tracking then
  begin
    DoClick := Tracker.Pressed;
    Tracker.StopTracking;

    if DoClick and
       Assigned( OnClick ) then
      OnClick( Self, ACell, NewMouseState );

  end;

  if Assigned( OnMouseUp ) then
    with NewMouseState do
      OnMouseUp( Self, Button, Shift, X, Y );
end;

function TClickableHeaderDrawCustomizer.GetTracking: Boolean;
begin
  Result := Tracker.Tracking;
end;

{ TDBUListBox }

constructor TDBUListBox.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  Visible := False;
  ItemHeight := 11;
{$ifndef LINUX}
  IntegralHeight := True;
{$else LINUX}
  FDontClose := False;
{$endif LINUX}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
end;

{$ifndef LINUX}
procedure TDBUListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TDBUListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TDBUListBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDBUComboBox.FCustomizer.FInplaceEdit) then
     FDBUComboBox.FCustomizer.CloseUp(False);
end;

procedure TDBUListBox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
    begin
      TickCount := GetTickCount;
      if TickCount - FSearchTickCount > 1500 then
        FSearchText := '';
      FSearchTickCount := TickCount;
      if Length(FSearchText) < 32 then
        FSearchText := FSearchText + Key;
      SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
      Key := #0;
    end;
  end;
  inherited Keypress(Key);
end;
{$else LINUX}
procedure TDBUListBox.InitWidget;
begin
  inherited InitWidget;
  Visible := False;
end;

function TDBUListBox.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags
              or Integer(WidgetFlags_WType_Popup)
              or Integer(WidgetFlags_WStyle_Tool)
              or Integer(WidgetFlags_WNorthWestGravity);
end;

procedure TDBUListBox.KeyDown(var Key: Word; Shift: TShiftState); 
begin
  inherited;
  if Visible then
  begin
    case Key of
      Key_Up : ItemIndex := Max(ItemIndex - 1, 0);
      Key_Down : ItemIndex := Min(ItemIndex + 1, Items.Count -1);
      Key_Next : ItemIndex := Min(ItemIndex + 10, Items.Count -1);
      Key_Prior : ItemIndex := Max(ItemIndex - 10, 0);
    else
      FDBUComboBox.FCustomizer.DoDropDownKeys(Key, Shift);
    end;
    Key := 0;
  end;
end;
{$endif LINUX}

procedure TDBUListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  InListBox : Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);

  InListBox := PtInRect(ClientRect, Point(X, Y));
{$ifdef LINUX}
  if not FDontClose or InListBox then
{$endif LINUX}
    FDBUComboBox.FCustomizer.CloseUp(InListBox);

{$ifdef LINUX}
  FDontClose := False;
{$endif LINUX}
end;

procedure TDBUListBox.SetDBUComboBox(const Value: TDBUComboBox);
begin
  FDBUComboBox := Value;
  Parent := FDBUComboBox.FCustomizer.FInplaceEdit;
end;

destructor TDBUListBox.Destroy;
begin
  if DBUComboBox <> nil then
    DBUComboBox.FListBox := nil;

  inherited;
end;

{ TDBUComboBox }

constructor TDBUComboBox.Create;
begin
  inherited Create;
  FCustomizer := nil;
  FListBox := nil;
  FItems := CreateStrings;
  FCaseSensitive := False;
  FCacheValues := False;
  FAutoComplete := True;
  FOnlyLegalValues := True;
  FEmptyItem := eiNoEmpty;
end;

destructor TDBUComboBox.Destroy;
begin
  inherited;
  FListBox.Free;
  FItems.Free; // LAA-tillsatt
end;

function TDBUComboBox.CreateStrings: TDBUStrings;
begin
  Result := TDBUStrings.Create;
end;

procedure TDBUComboBox.Initialize(ACustomizer: TComboCellTypeCustomizer);
begin
  Assert( FCustomizer = nil );
  Assert( FListBox = nil );

  FCustomizer := ACustomizer;
  FListBox := TDBUListBox.Create( ACustomizer.FInplaceEdit );
  FListBox.DBUComboBox := Self;
  FItems.Initialize( FListBox.Items );
end;

procedure TDBUComboBox.SetCacheValues(const AValue: Boolean);
begin
  FCacheValues := AValue;
end;

procedure TDBUComboBox.SetOnlyLegalValues(const Value: Boolean);
begin
  FOnlyLegalValues := Value;
end;

procedure TDBUComboBox.SetAutoComplete(const Value: Boolean);
begin
  FAutoComplete := Value;
end;

procedure TDBUComboBox.SetItems(const Value: TStrings);
begin
  FItems.Assign( Value );
end;

function TDBUComboBox.GetItemIndex: Integer;
begin
  Result := FListBox.ItemIndex;
end;

procedure TDBUComboBox.SetItemIndex(const Value: Integer);
begin
  FListBox.ItemIndex := Value;
end;

function TDBUComboBox.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TDBUComboBox.SetCaseSensitive(const Value: Boolean);
begin
  FCaseSensitive := Value;
end;

procedure TDBUComboBox.SetEmptyItem(const Value: TEmptyItem);
begin
  FEmptyItem := Value;
end;

{ TDefaultCustomizer }

constructor TDefaultCustomizer.Create(ACell: IEditorCell;
  ACellType: TDBUCellType);
begin
  inherited Create;

  FMaxLength := -1;
  FCellType := ACellType;
  FCell := ACell;
  if ACell.GetEditor <> nil then
    FInplaceEdit := ACell.GetEditor.SelfEdit
  else
    FInplaceEdit := nil;
end;

procedure TDefaultCustomizer.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
end;

{ TButtonCellTypeCustomizer }

constructor TButtonCellTypeCustomizer.Create(ACell: IEditorCell; ACellType : TDBUCellType );
begin
  inherited Create( ACell, ACellType );

  FTracker := TButtonTracker.Create( Invalidate );
end;

procedure TButtonCellTypeCustomizer.Invalidate( Sender : TObject );
begin
  if Assigned(FInplaceEdit) then
    FInplaceEdit.Invalidate;
end;

procedure TButtonCellTypeCustomizer.StopTracking;
begin
  if FTracker.Tracking then
  begin
    FCell.GetEditor.SetMouseCapture( False );
    FTracker.StopTracking;
  end;
end;

procedure TButtonCellTypeCustomizer.TrackButton(X,Y: Integer);
begin
  FTracker.TrackButton( X, Y );
end;

procedure TButtonCellTypeCustomizer.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  if (NewMouseState.Button = mbLeft) and
     ( FCellType as TButtonCellType ).OverEditButton( ACell, NewMouseState.X, NewMouseState.Y ) then
  begin
    ACell.GetEditor.SetMouseCapture( True );
    Tracker.StartTracking( TButtonCellType( FCellType ).ButtonRect( EditorRect( FCell ), FCell ) );
    TrackButton(NewMouseState.X, NewMouseState.Y);
  end;
end;

procedure TButtonCellTypeCustomizer.EditorMouseMove(ACell: IEditorCell;
  NewMouseState: TMouseMoveState);
begin
  if Tracking then
    TrackButton(NewMouseState.X, NewMouseState.Y);
end;

procedure TButtonCellTypeCustomizer.ButtonClick(ACell: IEditorCell; NewMouseState: TMouseState);
var
  AValue : TValue;
  AStr : String;
begin
  AValue := ACell.GetEditorKeyValue;
  OnClick( Self, ACell, NewMouseState, AValue );
  if not AValue.DataType.Equals( AValue, ACell.GetEditorKeyValue ) then
  begin
    SelectionChange(ACell, AValue);
    ACell.SetCell;

    AStr := AsString( ACell.GetEditorValue );
    ACell.SetInplaceEditText( AStr );
    if Assigned(FInplaceEdit) then
    begin
      FInplaceEdit.SelStart := 0;
      FInplaceEdit.SelLength := Length( AStr );
    end;
  end;
end;

procedure TButtonCellTypeCustomizer.EditorMouseUp(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  if Assigned( OnClick ) and
     Tracking and (NewMouseState.Button = mbLeft) and
    ( FCellType as TButtonCellType ).OverEditButton( ACell, NewMouseState.X, NewMouseState.Y ) then
  begin
    ButtonClick(ACell, NewMouseState);
  end;
  StopTracking;
end;

procedure TButtonCellTypeCustomizer.Reset;
begin
  StopTracking;
end;

procedure TButtonCellTypeCustomizer.KillFocus;
begin
  // Nothing;
end;

procedure TButtonCellTypeCustomizer.EditorKeyDown(ACell: IEditorCell;
  var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TButtonCellTypeCustomizer.EditorKeyUp(ACell: IEditorCell;
  var NewKeyState: TKeyState);
begin
  // nothing
end;

procedure TButtonCellTypeCustomizer.SetOnClick(const Value: TEditClickEvent);
begin
  FOnClick := Value;
end;

destructor TButtonCellTypeCustomizer.Destroy;
begin
  inherited;
  FTracker.Destroy;
end;

function TButtonCellTypeCustomizer.GetTracking: Boolean;
begin
  Result := Tracker.Tracking;
end;

procedure TButtonCellTypeCustomizer.SelectionChange(ACell: IEditorCell;
  var AValue: TValue);
begin
  if Assigned( OnSelectionChange ) then
    OnSelectionChange( ACell, AValue );

  ACell.SetEditorValue( AValue );
end;

procedure TButtonCellTypeCustomizer.SetOnSelectionChange(
  const Value: TSelectionChangeEvent);
begin
  FOnSelectionChange := Value;
end;

{ TPickListOrganizer }

constructor TPickListOrganizer.Create( ACustomizer : TComboCellTypeCustomizer );
begin
  inherited Create;

  FCustomizer := ACustomizer;
  FCombos := TList.Create;
  FIdentifiers := TList.Create;
end;

destructor TPickListOrganizer.Destroy;
begin
  inherited;

  FreeListWithObjects( FCombos );
  FIdentifiers.Free;
end;

function TPickListOrganizer.GetCombos(Identifier: TObject): TDBUComboBox;
var
  idx : Integer;
begin
  idx := FIdentifiers.IndexOf( Identifier );
  if idx >= 0 then
    Result := TDBUComboBox( FCombos[idx] )
  else
  begin
    Result := CreateCombo( Identifier );
    FIdentifiers.Add( Identifier );
    FCombos.Add( Result );
  end;
end;

function TPickListOrganizer.CreateCombo(Identifier: TObject): TDBUComboBox;
begin
  Result := nil;

  if Assigned( OnCreateCombo ) then
    OnCreateCombo( FCustomizer.FCell, Identifier, Result );

  if Result = nil then
    Result := TDBUComboBox.Create;

  Result.Initialize( FCustomizer );
end;

procedure TPickListOrganizer.AddCombo(Identifier: TObject;
  ACombo: TDBUComboBox);
var
  idx : Integer;
begin
  idx := FIdentifiers.IndexOf( Identifier );
  if idx >= 0 then
  begin
    TObject(FCombos[idx]).Free;
    FCombos[idx] := ACombo;
  end
  else
  begin
    FIdentifiers.Add( Identifier );
    FCombos.Add( ACombo );
  end;
end;

procedure TPickListOrganizer.SetOnCreateCombo(const Value: TComboCellTypeCreateCombo);
begin
  FOnCreateCombo := Value;
end;

{ TComboCellTypeCustomizer }

constructor TComboCellTypeCustomizer.Create( ACell : IEditorCell; ACellType : TDBUCellType );
begin
  inherited Create( ACell, ACellType );

  FKillWMChar := False;
  FPickLists := TPickListOrganizer.Create( Self );
end;

destructor TComboCellTypeCustomizer.Destroy;
begin
  FPickLists.Free;
  inherited;
end;

procedure TComboCellTypeCustomizer.Reset;
begin
  CloseUp( False );
  if Assigned( FComboBox ) then
  begin
    FComboBox := nil;
  end;

  inherited;
end;

procedure TComboCellTypeCustomizer.SetOnComboSelectionChange(
  const Value: TComboSelectionChangeEvent);
begin
  FOnComboSelectionChange := Value;
end;

procedure TComboCellTypeCustomizer.SetEditText(ACell: IEditorCell;
  AValue: string);
begin
  if MaxLength > 0 then
    AValue := Copy(AValue, 1, MaxLength);

  if AValue = ACell.GetInplaceEditText then
    Exit;

  SelectTextInPickList( ACell, AValue );
end;

procedure TComboCellTypeCustomizer.SelectionChange(ACell: IEditorCell;
  var AValue: TValue);
begin
  if Assigned( OnComboSelectionChange ) then
    OnComboSelectionChange( ACell, PickList, AValue );
  inherited SelectionChange(ACell, AValue);
//  ACell.SetEditorValue( ValueFromString( AValue ) );
end;

function TComboCellTypeCustomizer.SelectTextInPickList(ACell: IEditorCell;
  const AValue: string) : String;
var
  totLen, selLen : Integer;
  matchStr, resultStr, oldStr : String;
  hasDeleted : Boolean;
  TmpValue : TValue;
begin
  matchStr := AValue;
  resultStr := AValue;

  oldStr := FOldEditStr;

  hasDeleted := ( ( Length(AValue) < Length(oldStr) ) or ( AValue = oldStr ) ) and
                ( ( AValue = '' ) or ( Pos(AValue, oldStr) >= 1 ) );

//  if not hasDeleted then
    Result := MatchTextInPickList( ACell, AValue, matchStr, resultStr );
  if hasDeleted then
  begin
    Result := '';
    resultStr := '';
  end;

  FOldEditStr := matchStr;

  TmpValue := ValueFromString(Result);
  SelectionChange(ACell, TmpValue);
  Result := AsString(TmpValue);

  if Assigned(FInplaceEdit) then
  begin
    if not hasDeleted and
      ( (AValue <> resultStr) or
        ( AValue <> ACell.GetInplaceEditText ) ) then
    begin
      selLen := FInplaceEdit.SelStart;
      ACell.SetInplaceEditText( resultStr );
      if (PickList.ItemIndex = -1) then
        FInplaceEdit.SelStart := selLen
      else
      begin
        totLen := Length( resultStr );
        selLen := totLen - Length( matchStr );
        FInplaceEdit.SelStart := totLen;
        FInplaceEdit.SelLength := -selLen;
      end
    end
    else if AValue = '' then
      ACell.SetInplaceEditText( resultStr );

    FInplaceEdit.Modified := True;
    FInplaceEdit.Invalidate;
  end;
end;

function TComboCellTypeCustomizer.MatchTextInPickList(ACell: IEditorCell;
  const AValue: string; var matchStr, wholeStr : String ) : String;
var
  AItemIndex : Integer;
begin
  matchStr := AValue;
  wholeStr := AValue;

  with PickList do
  begin
    if OnlyLegalValues or AutoComplete then
    begin
      if (Items.Count = 0) or
         (not FindMatch( Items, matchStr, wholeStr, AItemIndex, OnlyLegalValues, CaseSensitive )) then
      begin
        if OnlyLegalValues then
        begin
          if Items.Count = 0 then
            wholeStr := ''
          else
          begin
            ItemIndex := 0;
            wholeStr := Items[0];
          end;

          matchStr := '';
        end
        else
        begin
          ItemIndex := -1;
          wholeStr := AValue;
          matchStr := AValue;
        end;
      end
      else
        ItemIndex := AItemIndex;
    end;

    if OnlyLegalValues then
      Result := wholeStr
    else
      Result := matchStr;
  end;
end;

function TComboCellTypeCustomizer.FindMatch( Strings : TStrings; var matchStr, resultStr : String;
  var ItemIndex : Integer; FindPartial, CaseSensitive : Boolean ) : Boolean;
var
  strLen : Integer;
begin
  ItemIndex := -1;
  repeat
    resultStr := matchStr;
    Result := FindString( Strings, resultStr, ItemIndex, CaseSensitive );
    if not Result then
    begin
      strLen := Length( matchStr );
      if strLen = 0 then
        FindPartial := False
      else if FindPartial then
        Delete( matchStr, strLen, 1 );
    end
    else
      matchStr := Copy( resultStr, 1, Length( matchStr ) );
  until( not FindPartial or Result);
end;

procedure TComboCellTypeCustomizer.CloseUp(Accept: Boolean);
var
  ListValue : String;
  APickList: TDBUComboBox;
begin
  if FListVisible then
  begin
    APickList := PickList;
//    SetMouseGrabControl(nil);
    if APickList.ItemIndex <> -1 then
      ListValue := APickList.Items[APickList.ItemIndex];
    APickList.FListBox.Visible := False;
    FListVisible := False;
    if Accept then
      SelectTextInPickList( FCell, ListValue )
    else if Assigned(FInplaceEdit) then
      FInplaceEdit.Invalidate;
  end;
end;

procedure TComboCellTypeCustomizer.DropDown;
var
  matchStr, resultStr: String;
  P: TPoint;
  Y: Integer;
  APickList: TDBUComboBox;
begin
  if Assigned(FInplaceEdit) then
  begin
    APickList := PickList;
    if not FListVisible and Assigned(APickList) then
    begin
      with FInplaceEdit do
      begin
        APickList.FListBox.Width := Width;

        if APickList.Items.Count >= 10 then
          APickList.FListBox.Height := 10 * APickList.FListBox.ItemHeight + 4
        else
          APickList.FListBox.Height := APickList.Items.Count * APickList.FListBox.ItemHeight + 4;

        MatchTextInPickList( FCell, Text, matchStr, resultStr );
      end;

      P := FInplaceEdit.Parent.ClientToScreen(Point(FInplaceEdit.Left, FInplaceEdit.Top));
      Y := P.Y + FInplaceEdit.Height;

      if Y + APickList.FListBox.Height > Screen.Height then
        Y := P.Y - APickList.FListBox.Height;

      APickList.FListBox.Top := Y;
      APickList.FListBox.Left := P.X;
      APickList.FListBox.Visible := True;
      APickList.FListBox.BringToFront;
      FListVisible := True;
      FInplaceEdit.Invalidate;
    end;
  end;
end;

procedure TComboCellTypeCustomizer.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  if (NewMouseState.Button = mbLeft) and
     ( FCellType as TButtonCellType ).OverEditButton( FCell, NewMouseState.X, NewMouseState.Y ) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      inherited;
      DropDown;
{$ifdef LINUX}
      PickList.FListBox.FDontClose := True;
{$endif LINUX}
    end;
  end;
end;

procedure TComboCellTypeCustomizer.EditorMouseMove(ACell: IEditorCell;
  NewMouseState: TMouseMoveState);
var
  ListPos, NewPos: TPoint;
{$ifndef LINUX}
  MousePos: TSmallPoint;
{$endif LINUX}
  APickList: TDBUComboBox;
begin
  inherited;

  if Assigned(FInplaceEdit) and
     Tracking and
     FListVisible then
  begin
    APickList := PickList;
    NewPos := Point( NewMouseState.X, NewMouseState.Y );
    NewPos := FInplaceEdit.ClientToScreen( NewPos );
    ListPos := APickList.FListBox.ScreenToClient( NewPos );
    if PtInRect(APickList.FListBox.ClientRect, ListPos) then
    begin
      StopTracking;
{$ifndef LINUX}
      MousePos := PointToSmallPoint(ListPos);
      SendMessage(APickList.FListBox.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
{$endif LINUX}
    end;
  end;
end;

procedure TComboCellTypeCustomizer.EditorMouseUp(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  inherited;
end;

procedure TComboCellTypeCustomizer.KillFocus;
begin
  CloseUp( False );
end;

function TComboCellTypeCustomizer.GetPickList: TDBUComboBox;
var
  cnt : Integer;
begin
  if not ( Assigned(FComboBox) {and  // FIXA MVJ: Cacha värden
           FComboBox.CacheValues} ) then
  begin
    if Assigned( OnGetStrings ) then
    begin
      OnGetStrings( FCell, FPickLists, FComboBox );
      case FComboBox.EmptyItem of
        eiEmptyFirst:
        begin
          if (FComboBox.Items.Count = 0) or
             ( (FComboBox.Items[0] <> '') and
               (FComboBox.Items.Objects[0] <> nil) ) then
          begin
            FComboBox.Items.InsertObject(0, '', nil);
          end;
        end;
        eiEmptyLast :
        begin
          cnt := FComboBox.Items.Count;
          if (cnt = 0) or
             ( (FComboBox.Items[cnt -1] <> '') and
               (FComboBox.Items.Objects[cnt -1] <> nil) ) then
          begin
            FComboBox.Items.AddObject('', nil);
          end;
        end;
      end;
    end
    else
      raise Exception.Create( Self.ClassName + '.GetPickList: You have to define the OnGetStrings event!' );
  end;

  Result := FComboBox;
end;

procedure TComboCellTypeCustomizer.SetOnGetStrings(
  const AValue: TComboCellTypeGetStrings);
begin
  FOnGetStrings := AValue;
end;

procedure TComboCellTypeCustomizer.EditorKeyDown(ACell: IEditorCell;
  var NewKeyState: TKeyState);
var
  IsKeyUpDown : Boolean;
begin
  case NewKeyState.Key of
    Key_Up, Key_Down : IsKeyUpDown := True;
  else
    IsKeyUpDown := False;
  end;

  if (ssAlt in NewKeyState.Shift) and
     IsKeyUpDown then
  begin
    if not ACell.EditorMode then
      ACell.EditorMode := True;

    NewKeyState.Key := 0;
    if FListVisible then
      CloseUp(False)
    else
      DropDown;
  end
  else
    PickList.FListBox.KeyDown(NewKeyState.Key, NewKeyState.Shift);
//    DoDropDownKeys(NewKeyState.Key, NewKeyState.Shift);
end;

procedure TComboCellTypeCustomizer.DoDropDownKeys(var Key: Word;
  Shift: TShiftState);
begin
  if FKillWMChar then
  begin
    Key := 0;
    FKillWMChar := False;
  end
  else
    case Key of
      Key_Up, Key_Down :
        if ssAlt in Shift then
        begin
          if FListVisible then
            CloseUp(PickList.ItemIndex >= 0)
          else
            DropDown;
          Key := 0;
        end;
      Key_Return :
        if FListVisible and
           not (ssAlt in Shift) then
        begin
{$ifndef LINUX}
          FKillWMChar := True;
{$endif LINUX}
          CloseUp(PickList.ItemIndex >= 0);
          Key := 0;
        end;
      Key_Escape :
        if FListVisible and
           not (ssAlt in Shift) then
        begin
          CloseUp(False);
          Key := 0;
        end;
    end;
end;

procedure TComboCellTypeCustomizer.HandleMessage(
  var Params: THandleWMParams);
begin
{$ifndef LINUX}
  case Params.Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      with TWMKey(Params.Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        Params.RunInh := (CharCode <> 0);
        if Params.RunInh and FListVisible then
        begin
          with TMessage(Params.Message) do
            SendMessage(PickList.FListBox.Handle, Msg, WParam, LParam);
          Params.RunInh := False;
        end;
      end;
//    WM_UNDO: doUndo;
  end;
{$endif LINUX}
end;

procedure TComboCellTypeCustomizer.SetOnCreateCombo(
  const Value: TComboCellTypeCreateCombo);
begin
  FPickLists.OnCreateCombo := Value;
end;

function TComboCellTypeCustomizer.GetOnCreateCombo: TComboCellTypeCreateCombo;
begin
  Result := FPickLists.OnCreateCombo;
end;

{ TCheckCellType }

constructor TCheckCellType.Create;
{$ifndef LINUX}
var
  APicture : TBitmap;
{$endif LINUX}
begin
  inherited Create;

  FCheckedBitmap := TBitmap.Create;
  FUncheckedBitmap := TBitmap.Create;
  FDisabledCheckedBitmap := TBitmap.Create;
  FDisabledUncheckedBitmap := TBitmap.Create;
{$ifndef LINUX}
  APicture := TBitmap.Create;
  { checkBitmap = bitmap containing windows radio 111& checkboxes in various states of checking}
  APicture.Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES)); {DELPHI2 32759}
  FUncheckedBitmap.Height := 12;
  FUncheckedBitmap.Width := 12;
  FUncheckedBitmap.Canvas.CopyRect(Rect(0,0,12,12), APicture.Canvas, Rect(0,0,12,12));
  FCheckedBitmap.Height := 12;
  FCheckedBitmap.Width := 12;
  FCheckedBitmap.Canvas.CopyRect(Rect(0,0,12,12), APicture.Canvas, Rect(13,0,25,12));
  FDisabledUncheckedBitmap.Height := 12;
  FDisabledUncheckedBitmap.Width := 12;
  FDisabledUncheckedBitmap.Canvas.CopyRect(Rect(0,0,12,12), APicture.Canvas, Rect(26,0,38,12));
  FDisabledCheckedBitmap.Height := 12;
  FDisabledCheckedBitmap.Width := 12;
  FDisabledCheckedBitmap.Canvas.CopyRect(Rect(0,0,12,12), APicture.Canvas, Rect(39,26,51,38));
  APicture.Free;
{$endif LINUX}
end;

destructor TCheckCellType.Destroy;
begin
  inherited Destroy;

  FCheckedBitmap.Free;
  FUncheckedBitmap.Free;
  FDisabledCheckedBitmap.Free;
  FDisabledUncheckedBitmap.Free;
end;

function TCheckCellType.ShowEditor(ACell: IInfoCell; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
var
  R : TRect;
begin
  R := ACell.GetGrid.CellRect( ACell.GetCol, ACell.GetRow );
  Result := OverIcon(IconRect(R, ACell), X, Y);
end;

procedure TCheckCellType.DoDrawCell(ACell: IInfoCell; var Params : TDrawParams );
var
  AFormatter : TDBUFormatter;
  Customizer : TDBUCustomizer;
  Descr : TValue;
  InfoSupp : ICheckInfoSupplier;
begin
  AFormatter := ACell.GetFormatter( Params.DrawState );

  AFormatter.PrepareCanvas( Params.Canvas );
  AFormatter.DrawBackground( Params.Canvas, Params.Rect );

  DrawCheckBox(Params.Canvas, ACell, Params.Rect);

  Customizer := ACell.GetCustomizer;

  if (Customizer is TCheckCellTypeCustomizer) then
    InfoSupp := TCheckCellTypeCustomizer( Customizer ).CheckInfoSupplier;

  if Assigned(InfoSupp) then
  begin
    Descr := EmptyString;
    InfoSupp.GetDescription( ACell, Descr );
    with Params do
      RenderValue( Canvas, Rect, AFormatter.Align, AFormatter.Margin, Descr );
  end;
end;

procedure TCheckCellType.DrawCheckBox(Canvas : TCanvas; ACell: IInfoCell; var ARect: TRect);
var
  APic : TBitmap;
begin
  if AsBoolean( ACell.GetValue ) then
  begin
    if ACell.GetEnabled then
      APic := FCheckedBitmap
    else
      APic := FDisabledCheckedBitmap;
  end
  else
  begin
    if ACell.GetEnabled then
      APic := FUnCheckedBitmap
    else
      APic := FDisabledUncheckedBitmap;
  end;

  TmpPic.Assign( APic );
  RenderIcon( Canvas, ARect, DBUAlign( vaMiddle, taLeftJustify ), DBUMargin( 2, 4 ), TmpPic );
end;

procedure TCheckCellType.DoDrawEditor(ACell: IEditorCell;
  var Params: TDrawParams);
var
  ARect : TRect;
begin
  ARect := Params.Rect;

  DoDrawCell( ACell, Params );
  OffsetRect( Params.Rect, -100, -100 );

{$ifndef LINUX}
  DrawFocusRect( Params.Canvas.Handle, ARect );
{$else LINUX}
  Params.Canvas.DrawFocusRect(ARect)
{$endif LINUX}
end;

procedure TCheckCellType.GetCursor(ACell: IEditorCell; APoint: TPoint;
  var ACursor: TCursor);
begin
  ACursor := crArrow;
end;

function TCheckCellType.GetEditText(ACell: IEditorCell): String;
begin
  Result := '';
end;

procedure TCheckCellType.SetEditText(ACell: IEditorCell;
  AValue: string);
begin
  // nothing
end;

procedure TCheckCellType.SetEditorValue(ACell: IEditorCell; NewValue : TValue );
begin
  ACell.SetEditorValue( NewValue );
  ACell.SetCell;
  ACell.GetEditor.Invalidate;
end;

procedure TCheckCellType.EditorKeyDown(ACell: IEditorCell;
  var NewKeyState: TKeyState);
const
  Key_T = 84;
  Key_F = 70;
begin
  case NewKeyState.Key of
    Key_T, Key_F, Key_Space :
    begin
      case NewKeyState.Key of
        Key_T : SetEditorValue( ACell, TrueValue );
        Key_F : SetEditorValue( ACell, FalseValue );
        Key_Space : SetEditorValue( ACell, ValueFromBoolean( not AsBoolean( ACell.GetValue ) ) );
      end;

      NewKeyState.Key := 0;
    end;
  end;
end;

procedure TCheckCellType.EditorMouseDown(ACell: IEditorCell;
  NewMouseState: TMouseState);
begin
  if OverIcon( IconRect( ACell.GetEditor.ClientRect, ACell ), NewMouseState.X, NewMouseState.Y ) then
    SetEditorValue( ACell, ValueFromBoolean( not AsBoolean( ACell.GetValue ) ) );
end;

function TCheckCellType.CreateDrawCustomizer(
  ACell: IInfoCell): TDBUCustomizer;
begin
  Result := TCheckCellTypeCustomizer.Create;
end;

function TCheckCellType.CreateEditCustomizer(ACell: IEditorCell): TDBUCustomizer;
begin
  Result := TCheckCellTypeCustomizer.Create;
end;

function TCheckCellType.IconRect(EditRect: TRect; ACell: IInfoCell): TRect;
begin
  with EditRect do
    Result := Rect( Left, Top, Left + 12 + 2 * 4, Bottom );
end;

function TCheckCellType.OverIcon(IconRect: TRect; X, Y: Integer): Boolean;
begin
  Result := PtInRect( IconRect, Point( X, Y ) );
end;

{ TEllipsisCellType }

constructor TEllipsisCellType.Create;
begin
  inherited;

{$ifndef LINUX}
  with FFacePic, FFacePic.Canvas do
  begin
    Width := 10;
    Height := 2;

    Brush.Color := clNone;
    Brush.Style := bsClear;
    FloodFill( 0, 0, clNone, fsSurface );

    Pen.Style := psSolid;
    Pen.Color := clBlack;

    Rectangle( 0, 0, 2, 2 );
    Rectangle( 4, 0, 6, 2 );
    Rectangle( 8, 0, 10, 2 );
  end;
{$endif LINUX}
end;

{TCheckCellTypeCustomizer }

procedure TCheckCellTypeCustomizer.SetCheckInfoSupplier(
  const Value: ICheckInfoSupplier);
begin
  FCheckInfoSupplier := Value;
end;

{ TButtonTracker }

constructor TButtonTracker.Create(OnInvalidate: TNotifyEvent);
begin
  Assert( Assigned( OnInvalidate ) );

  FOnInvalidate := OnInvalidate;
  FPressed := False;
end;

function TButtonTracker.OverButton(X, Y: Integer): Boolean;
begin
  Result := PtInRect(FBtnRect, Point(X, Y));
end;

procedure TButtonTracker.StartTracking(BtnRect: TRect);
begin
  FTracking := True;
  FBtnRect := BtnRect;
end;

procedure TButtonTracker.StopTracking;
begin
  TrackButton(-1, -1);
  FTracking := False;
end;

procedure TButtonTracker.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  Assert( Tracking );
  NewState := OverButton( X, Y );
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    FOnInvalidate( Self );
  end;
end;

{ TDBUStrings }

procedure TDBUStrings.Changed;
begin
  if not FIsChanging then
    DoChanged
  else
    FIsChanged := True;
end;

procedure TDBUStrings.DoChanged;
begin
  FDependent.Assign( Self );

  if Assigned(OnChange) then
    OnChange(Self);
  FIsChanged := False;
end;

procedure TDBUStrings.Changing;
begin
  if not FHasRunChanging then
    DoChanging;
end;

procedure TDBUStrings.DoChanging;
begin
  if Assigned(OnChanging) then
    OnChanging(Self);
  FHasRunChanging := FIsChanging;
end;

constructor TDBUStrings.Create;
begin
  inherited Create;
  FDependent := nil;
  FIsChanging := False;
  FIsChanged := False;
  FHasRunChanging := False;
end;

destructor TDBUStrings.Destroy;
begin
  inherited ;
end;

procedure TDBUStrings.SetUpdateState(Updating: Boolean);
begin
  FIsChanging := Updating;
  if Updating then
    FHasRunChanging := False
  else if FIsChanged then
    DoChanged;

  FIsChanged := False
end;

procedure TDBUStrings.Initialize(DependentStrings: TStrings);
begin
  Assert( FDependent = nil );
  FDependent := DependentStrings;
end;

{ TBlankCellType }

constructor TBlankCellType.Create;
begin
  inherited Create;
  SetReadOnly( True );
end;

procedure TBlankCellType.DoDrawCell(ACell: IInfoCell;
  var Params: TDrawParams);
begin
  // nothing
end;

procedure TBlankCellType.DoDrawEditor(ACell: IEditorCell;
  var Params: TDrawParams);
begin
  // nothing
end;

initialization
  TmpPic := TPicture.Create;
  NormalCellType := TNormalCellType.Create;
  ButtonCellType := TEllipsisCellType.Create;
  HeaderCellType := THeaderCellType.Create;
  ClickableHeaderCellType := TClickableHeaderCellType.Create;
  CheckCellType := TCheckCellType.Create;
  ComboCellType := TComboCellType.Create;
  BlankCellType := TBlankCellType.Create;

finalization
  TmpPic.Free;
  NormalCellType.Free;
  ButtonCellType.Free;
  HeaderCellType.Free;
  ClickableHeaderCellType.Free;
  CheckCellType.Free;
  ComboCellType.Free;
  BlankCellType.Free;

end.

