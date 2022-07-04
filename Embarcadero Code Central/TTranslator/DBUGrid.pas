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

{ $Id: DBUGrid.pas,v 1.50 2003/03/31 09:10:09 mvj Exp $}

unit DBUGrid;

interface

uses
  Classes,
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls,  ExtCtrls,
  Grids,
{$else LINUX}
  Types, Qt, QGraphics, QControls, QExtCtrls, QGrids,
{$endif LINUX}
  DBUTypes, DBUInterfaces, DBUFormatter;

type
  TDBUCustomEditorCell = class; // forward declaration
  TDBUCustomGridCell = class; // forward declaration
  TDBUGrid = class; // forward declaration
  TDBUInplaceEdit = class; // forward declaration

  TOnCreateCellEvent = function ( Sender: TObject ) : TDBUCustomGridCell of object;
  TOnCreateFormatterEvent = function ( Sender: TObject ) : TDBUFormatter of object;
  TOnCreateInplaceEditEvent = function ( Sender: TObject ) : TDBUInplaceEdit of object;
  TOnCreateEditorCellEvent = function ( Sender: TObject ) : TDBUCustomEditorCell of object;
  TOnCreateEditorFormatterEvent = function ( Sender: TObject ) : TDBUFormatter of object;
  TOnScrollEvent = procedure(Sender : TObject; ScrollCode, Pos : SmallInt) of object;

  TDBUInplaceEdit = class( TInplaceEdit, IInfoEditor )
  private
    FDBUGrid : TDBUGrid;
    FCell : TDBUCustomEditorCell;
{$ifdef LINUX}
    FCanvas: TCanvas;
{$endif LINUX}
    FForwardMovement : Boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function SelfEdit : TInplaceEdit;
    procedure SetMouseCapture( Value : Boolean );
    function GetWidth : Integer;
    function GetHeight : Integer;

{$ifndef LINUX}
    function HandleMessage(AWMType: TWMType; var Message: TMessage) : Boolean;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
{$endif LINUX}
    function GetEditorMode: Boolean;
    procedure SetEditorMode(const Value: Boolean);
  protected
    function EditorRect: TRect;
    function ClientRect: TRect;
{$ifndef LINUX}
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
{$else}
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint;
{$endif LINUX}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function MoveFocus(ACol, ARow: Integer) : Boolean;
    procedure UpdateContents; override;
    procedure UndoChanges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CommitChanges;
    procedure Flush;
    procedure DeletePress;

    property Cell : TDBUCustomEditorCell read FCell;
    property DBUGrid : TDBUGrid read FDBUGrid;
    property EditorMode: Boolean read GetEditorMode write SetEditorMode;
    property ForwardMovement : Boolean read FForwardMovement write FForwardMovement;
  end;

  TDBUGridOption = (dgVertLine, dgHorzLine, dgColSizing, dgRowSizing,  dgTabs,
    dgDrawFocusSelected, dgEditing, dgAlwaysShowEditor, dgRowSelect, dgMultiSelect, dgThumbTracking);
  TDBUGridOptions = set of TDBUGridOption;

  TDBUGrid = class(TCustomGrid , IInfoGrid, IAntGrid)
  private
    FCell : TDBUCustomGridCell;
    FDBUOptions: TDBUGridOptions;
    FAncestor : TWinControl;
    FDisabledKeys : TList;

    // Kill invalidates when changin Options in Mouse and Key events
    FKillInvalidate : Boolean;
    FUpdate : Boolean;
    FKilling : Boolean;
    FFocusedOnDisable : Boolean;

    // Grid specific text style
    FGridStyle: TDBUTextStyle;

    FOnCreateCell : TOnCreateCellEvent;
    FOnCreateEditorCell : TOnCreateEditorCellEvent;
    FOnCreateInplaceEdit : TOnCreateInplaceEditEvent;
    FOnCreateFormatter : TOnCreateFormatterEvent;
    FOnCreateEditorFormatter: TOnCreateEditorFormatterEvent;
    FOnVScroll: TOnScrollEvent;
    FOnHScroll: TOnScrollEvent;
    FOnTopLeftChanged: TNotifyEvent;

    FInhFontChanged : TNotifyEvent;
    procedure DoFontChanged(Sender: TObject);

    function GetCell: TDBUCustomGridCell;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure SetDBUOptions(Value: TDBUGridOptions);
    function DBUToGridOptions(const Value: TDBUGridOptions): TGridOptions;
    function GetDBUInplaceEdit: TDBUInplaceEdit;
    function IndexOfKeyComb( Key: Word; Shift: TShiftState ) : Integer;

    procedure SetOnCreateCell(const Value: TOnCreateCellEvent);
    procedure SetOnCreateEditorCell(
      const Value: TOnCreateEditorCellEvent);
    procedure SetOnCreateInplaceEdit(
      const Value: TOnCreateInplaceEditEvent);
    procedure SetOnCreateEditorFormatter(
      const Value: TOnCreateEditorFormatterEvent);
    procedure SetOnCreateFormatter(const Value: TOnCreateFormatterEvent);
    procedure RemoveSelection;
    procedure SetKilling( Value : Boolean );
    procedure ForceInplaceCreation;
  protected
{$ifndef LINUX}
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
{$else LINUX}
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetParent(const AParent: TWidgetControl); override;
{$endif LINUX}

    procedure DoExit; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure DoDrawCell(ACol, ARow: Longint; var ARect: TRect;
      AState: TGridDrawState);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;

{$ifndef LINUX}
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
{$else}
    function GetEditText(ACol, ARow: Longint): WideString; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: WideString); override;
{$endif LINUX}

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    function CreateEditor: TInplaceEdit; override;
    function CreateCell: TDBUCustomGridCell; virtual;
    function CreateEditorCell( AInplaceEdit : TDBUInplaceEdit ) : TDBUCustomEditorCell; virtual;

    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    function CellEditable(ACol, ARow : Integer) : Boolean;
    function CellShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;

    property Cell : TDBUCustomGridCell read GetCell;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;

{$ifndef LINUX}
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
{$endif LINUX}
    procedure TopLeftChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsCellSelected(ACol, ARow: Integer) : Boolean;
    function HasFocus : Boolean;

    procedure DeleteColumn(ACol: Longint); override;
    procedure InsertColumn(ACol, AWidth: Longint);

    procedure BeforeDestruction; override;
    procedure Invalidate; override;
    procedure RedrawCell(ACol, ARow: Longint);
    procedure RedrawCol(ACol: Longint);
    procedure RedrawRow(ARow: Longint);
    procedure RedrawRect(ARect: TGridRect);
    procedure DisableKeyCombiantion( Key: Word; Shift: TShiftState );
    procedure EnableKeyCombiantion( Key: Word; Shift: TShiftState );

    procedure DeletePress;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure ShowEditor;
    procedure HideEditor;

    function CreateFormatter: TDBUFormatter; virtual;
    function CreateEditorFormatter: TDBUFormatter; virtual;
    function CommitChanges : Boolean;

    class function FlushActiveControl(AbortOnFailiure : Boolean = True) : Boolean;

    property DBUInplaceEdit : TDBUInplaceEdit read GetDBUInplaceEdit;
    property GridStyle : TDBUTextStyle read FGridStyle;
    property Selection;
    property RowHeights;
    property ColWidths;
    property VisibleColCount;
    property VisibleRowCount;
    property Killing : Boolean read FKilling write SetKilling;
  published
    property Font;
    property Color;
    property FixedColor;

    property Align;
    property Anchors;

    property Col;
    property Row;
    property ColCount;
    property RowCount;
    property FixedCols;
    property FixedRows;
    property LeftCol;
    property TopRow;
    property Visible;
    property PopupMenu;
    property Canvas;

    property Enabled;
    property DefaultRowHeight;
    property DefaultColWidth;
    property DBUOptions : TDBUGridOptions read FDBUOptions write SetDBUOptions
      default [dgEditing, dgVertLine, dgHorzLine, dgMultiSelect, dgThumbTracking];
    property ScrollBars;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;

    property OnVScroll : TOnScrollEvent read FOnVScroll write FOnVScroll;
    property OnHScroll : TOnScrollEvent read FOnHScroll write FOnHScroll;
    property OnCreateCell : TOnCreateCellEvent read FOnCreateCell write SetOnCreateCell;
    property OnCreateEditorCell : TOnCreateEditorCellEvent read FOnCreateEditorCell write SetOnCreateEditorCell;
    property OnCreateInplaceEdit : TOnCreateInplaceEditEvent read FOnCreateInplaceEdit write SetOnCreateInplaceEdit;
    property OnCreateFormatter : TOnCreateFormatterEvent read FOnCreateFormatter write SetOnCreateFormatter;
    property OnCreateEditorFormatter: TOnCreateEditorFormatterEvent read FOnCreateEditorFormatter write SetOnCreateEditorFormatter;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  end;

  TAbstractDBUCell = class
  private
    FGrid : TDBUGrid;
    FCol : Integer;
    FRow : Integer;
    FReadOnly: Boolean;
    FKilling : Boolean;

    FKeyStateDown : TKeyState;
    FKeyStateUp : TKeyState;
    FMouseStateDown: TMouseState;
    FMouseStateMove: TMouseMoveState;
    FMouseStateUp: TMouseState;
    FMouseBtnStates: TMouseBtnStates;

    procedure SetMouseStateDown(const Value: TMouseState);
    procedure SetMouseStateMove(const Value: TMouseMoveState);
    procedure SetMouseStateUp(const Value: TMouseState);
    function GetKeyStateDown: TKeyState;
    function GetKeyStateUp: TKeyState;
    procedure SetKeyStateDown(const Value: TKeyState);
    procedure SetKeyStateUp(const Value: TKeyState);
    procedure SetKilling(const Value: Boolean);
  protected
    FFormatter : TDBUFormatter;

    constructor Create; virtual;

    function GetCol: Integer; virtual;
    function GetRow: Integer; virtual;
    function GetEnabled : Boolean;
    function CanEditModify : Boolean;

    function GetMouseStateDown: TMouseState;
    function GetMouseStateMove: TMouseMoveState;
    function GetMouseStateUp: TMouseState;
    function GetMouseBtnStates: TMouseBtnStates;

    procedure GetCell(ACol, ARow: Integer); virtual;
    procedure ResetCaches; virtual;
    procedure ResetValues; virtual;

    function CanEdit( ACol, ARow : Integer) : Boolean; virtual;
    function ShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;

    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    function GetFormatter(DrawState: TGridDrawState): TDBUFormatter; virtual;
    procedure SetFormatter( AFormatter : TDBUFormatter );

    // Key events called by the editor or by the grid
    procedure RunKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure RunKeyUp(var Key: Word; Shift: TShiftState); virtual;
    // The RunKeyXXX function runs the following after it has created the state parameter
    procedure KeyDown( var NewKeyState : TKeyState ); virtual;
    procedure KeyUp( var NewKeyState : TKeyState ); virtual;

    // Mouse events called by the editor or by the grid
    procedure RunMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure RunMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure RunMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    // The RunMouseXXX function runs the following after it has created the state parameter
    procedure MouseDown(NewMouseState : TMouseState); virtual;
    procedure MouseMove(NewMouseState : TMouseMoveState); virtual;
    procedure MouseUp(NewMouseState : TMouseState); virtual;

    property KeyStateDown: TKeyState read GetKeyStateDown write SetKeyStateDown;
    property KeyStateUp: TKeyState read GetKeyStateUp write SetKeyStateUp;
    property MouseStateDown: TMouseState read GetMouseStateDown write SetMouseStateDown;
    property MouseStateMove: TMouseMoveState read GetMouseStateMove write SetMouseStateMove;
    property MouseStateUp: TMouseState read GetMouseStateUp write SetMouseStateUp;
    property MouseBtnStates : TMouseBtnStates read GetMouseBtnStates;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    destructor Destroy; override;
    procedure Reset; virtual;

    property Col : Integer read GetCol;
    property Row : Integer read GetRow;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property Grid : TDBUGrid read FGrid write FGrid;
    property Formatter : TDBUFormatter read FFormatter;
    property Killing : Boolean read FKilling write SetKilling;
  end;

  TDBUCustomEditorCell = class(TAbstractDBUCell)
  private
    FInplaceEdit : TDBUInplaceEdit;
    FModified : Boolean;
    FInplaceText : String;

    procedure SetInplaceText(const Value: String);
  protected
    constructor Create; override;

    function SetCell : Boolean; virtual;
    function GetEditText : String;
    function DoGetEditText : String; virtual;
    procedure SetEditText(const AValue: string); virtual;

    function MoveFocus(ACol, ARow: Integer) : Boolean; virtual;

    procedure DrawEditor( var Params : TDrawParams ); virtual;
    procedure HandleMessage( var Params : THandleWMParams ); virtual;
    procedure HandleCancelMode( var Params : THandleCMParams ); virtual;

    procedure GetCursor( APoint : TPoint; var ACursor : TCursor ); virtual;
    procedure DoUndoChanges; virtual;
    procedure UndoChanges;

		procedure RunKeyDown(var Key: Word; Shift: TShiftState); override;
		procedure RunKeyUp(var Key: Word; Shift: TShiftState); override;

    procedure RunMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RunMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure RunMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property InplaceEdit : TDBUInplaceEdit read FInplaceEdit;
    property Modified : Boolean read FModified write FModified;

    property InplaceText : String read FInplaceText write SetInplaceText;
  end;

  TDBUCustomGridCell = class(TAbstractDBUCell)
  protected
    constructor Create; override;

    procedure DrawCell( var Params : TDrawParams ); virtual;

		procedure RunKeyDown(var Key: Word; Shift: TShiftState); override;
		procedure RunKeyUp(var Key: Word; Shift: TShiftState); override;

    procedure RunMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RunMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure RunMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TAbstractGridDrawer = class
  private
    FTimer : TTimer;
    FGrid : TDBUGrid;
    FSaveBrush : TBrush;
    FSavePen : TPen;
    FDrawOnFixed : Boolean;

    procedure SaveState(Canvas : TCanvas);
    procedure RestoreState(Canvas : TCanvas);
    procedure OnTimer( Sender : Tobject );
    procedure DoDraw(Canvas : TCanvas);
    procedure SetDrawOnFixed(const Value: Boolean);
  protected
    procedure Draw(Canvas : TCanvas); virtual; abstract;
    property Grid : TDBUGrid read FGrid;
  public
    constructor Create(AGrid : TDBUGrid);
    destructor Destroy; override;

    property DrawOnFixed : Boolean read FDrawOnFixed write SetDrawOnFixed;
  end;

  TCustomFrameDrawer = class(TAbstractGridDrawer)
  private
    FCellFrames : TList;
    FLineWidth: Integer;
    FFGColor: TColor;
    FBGColor: TColor;
    FStyle: TPenStyle;

    procedure AddRectToList(ARect: TGridRect; AList: TList);
    procedure SetBGColor(const Value: TColor);
    procedure SetFGColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
    procedure SetStyle(const Value: TPenStyle);
  protected
    function BoxRect(CellFrame: TGridRect): TRect; virtual;
    procedure RedrawArea(ACellFrame : TGridRect);
    procedure AddCellFrame(ACellFrame : TGridRect);
  public
    constructor Create(AGrid : TDBUGrid);
    destructor Destroy; override;

    procedure Redraw;

    property LineWidth : Integer read FLineWidth write SetLineWidth;
    property FGColor : TColor read FFGColor write SetFGColor;
    property BGColor : TColor read FBGColor write SetBGColor;
    property Style : TPenStyle read FStyle write SetStyle;
  end;

  TAntFrameDrawer = class(TCustomFrameDrawer)
  private
    FBool : Boolean;
  protected
    procedure Draw(Canvas : TCanvas); override;
  public
    constructor Create( AGrid : TDBUGrid; ACellFrames : array of TGridRect );

    procedure AddFrame(ACellFrame : TGridRect);
  end;

  TLineDrawer = class(TCustomFrameDrawer)
  private
    fOffset: integer;
  protected
    procedure Draw(Canvas : TCanvas); override;
  public
    constructor Create(AGrid : TDBUGrid; ARows : array of Integer);

    procedure AddLine(AIdx : Integer); virtual;
    property OffSet : integer read fOffset write fOffset;
  end;

  TVerticalLineDrawer = class(TLineDrawer)
  protected
    procedure Draw(Canvas : TCanvas); override;
  public
    procedure AddLine(AIdx : Integer); override;
  end;

  function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
  function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
  function GridCoord( AX, AY : LongInt) : TGridCoord;



implementation

uses
{$ifndef LINUX}
  Forms,
{$else}
  QForms,
{$endif LINUX}
  SysUtils, Math, CommonLib;



function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
begin
  Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
    and (Row <= Rect.Bottom);
end;

function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;

function GridCoord( AX, AY : LongInt) : TGridCoord;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

{ TDBUGrid }

constructor TDBUGrid.Create(AOwner: TComponent);
begin
  BeginUpdate;
  FAncestor := nil;

  inherited Create( AOwner );

  FCell := nil;
  FKillInvalidate := False;

  FGridStyle := DBUTextStyle( Font, Color );

  if csDesigning in ComponentState then
    DefaultDrawing := True
  else
    DefaultDrawing := False;

  DefaultRowHeight := 16;
  DBUOptions := [dgEditing, dgVertLine, dgHorzLine, dgMultiSelect, dgColSizing, dgTabs, dgThumbTracking];
  FInhFontChanged := Font.OnChange;
  Font.OnChange := DoFontChanged;
  FDisabledKeys := TList.Create;

  EndUpdate;
end;

destructor TDBUGrid.Destroy;
begin
  try
    if Assigned( InplaceEditor ) then
      DBUInplaceEdit.Flush;

    Visible := False;

    inherited Destroy;

    if Assigned( FCell ) then
      FCell.Free;

    while FDisabledKeys.Count > 0 do
    begin
      Dispose( PDBUKeyComb(FDisabledKeys[0]) );
      FDisabledKeys.Delete(0);
    end;

    FDisabledKeys.Free;
  finally
  end;
end;

procedure TDBUGrid.BeginUpdate;
begin
  FUpdate := True;
end;

procedure TDBUGrid.EndUpdate;
begin
  FUpdate := False;
  Invalidate;
end;

procedure TDBUGrid.ShowEditor;
var
  NewOptions : TGridOptions;
begin
  if (dgEditing in DBUOptions) and
     CanEditModify then
  begin
    NewOptions := Options - [goRangeSelect] + [goEditing];
    if dgAlwaysShowEditor in DBUOptions then
      Include(NewOptions, goAlwaysShowEditor);
    RemoveSelection;
    if NewOptions <> Options then
    begin
      FKillInvalidate := True;
      Options := NewOptions;
    end;
  end;

  inherited ShowEditor;
end;

procedure TDBUGrid.HideEditor;
begin
  inherited HideEditor;
end;

function TDBUGrid.IsCellSelected(ACol, ARow: Integer) : Boolean;
begin
  Result := PointInGridRect( ACol, ARow, Selection );
end;

procedure TDBUGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
//  try
    DoDrawCell( ACol, ARow, ARect, AState );
//  except
//    on E:EConvertError do
//     ;
//  end;
end;

procedure TDBUGrid.DoDrawCell(ACol, ARow: Integer; var ARect: TRect;
  AState: TGridDrawState);

  function NoSelection : Boolean;
  var
    Sel : TGridRect;
  begin
    Sel := Selection;
    Result := (Sel.Top = ARow) and
              (Sel.Left = ACol) and
              (Sel.Bottom = ARow) and
              (Sel.Right = ACol);
  end;

var
  Param : TDrawParams;
  OldRect : TRect;
begin
  FGridStyle.BGColor := Color;
  Cell.Formatter.AssignTextStyle( GridStyle );

  if not Enabled then
  begin
    if gdFixed in AState then
    begin
      Cell.Formatter.BGColor := FixedColor;
      Cell.Formatter.Font.Assign(DisabledHeaderFont);
    end
    else
      Cell.Formatter.Font.Assign(DisabledFont);
  end
  else if (gdSelected in AState) and
    (not (gdFocused in AState) or
    ([dgDrawFocusSelected, dgRowSelect] * DBUOptions <> [])) then
  begin
    Cell.Formatter.BGColor := clHighlight;
    Cell.Formatter.Font.Color := clHighlightText;
  end
  else if gdFixed in AState then
    Cell.Formatter.BGColor := FixedColor;

  if csDesigning in ComponentState then
  begin
    Cell.Formatter.PrepareCanvas( Canvas );
    Canvas.FillRect(ARect);
  end
  else
  begin
    OldRect := ARect;
    if Assigned( Cell ) then
    begin
      Param := DrawParams( Canvas, ACol, ARow, ARect, AState );
      Cell.DrawCell( Param );
      ARect := Param.Rect;
    end;

    if EqualRect( OldRect, ARect ) and
       (gdFixed in AState) then
    begin
{$ifndef LINUX}
      DrawEdge(Canvas.Handle, OldRect, BDR_RAISEDINNER, BF_TOPLEFT);
      DrawEdge(Canvas.Handle, OldRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
{$else LINUX}
      DrawEdge(Canvas, OldRect, esRaised, esNone, [ebLeft, ebTop]);
      DrawEdge(Canvas, OldRect, esRaised, esNone, [ebRight, ebBottom]);
{$endif LINUX}
    end
    else if not (csDesigning in ComponentState) and
      (gdFocused in AState) and
      ([goEditing, goAlwaysShowEditor] * Options <>
      [goEditing, goAlwaysShowEditor])
      and not (goRowSelect in Options) then
    begin
{$ifndef LINUX}
      DrawFocusRect(Canvas.Handle, OldRect);
{$else LINUX}
      Canvas.DrawFocusRect(OldRect)
{$endif LINUX}
    end;
  end;
end;

function TDBUGrid.GetCell: TDBUCustomGridCell;
begin
  if not Assigned(FCell) then
    FCell := CreateCell;

  Result := FCell;
end;

function TDBUGrid.CreateCell : TDBUCustomGridCell;
begin
  if Assigned( OnCreateCell ) and
     not (csDesigning in ComponentState) then
    Result := OnCreateCell( Self )
  else
    Result := TDBUCustomGridCell.Create;

  Result.FGrid := Self;
  Result.FFormatter := CreateFormatter;
end;

function TDBUGrid.CreateEditorCell( AInplaceEdit : TDBUInplaceEdit ) : TDBUCustomEditorCell;
begin
  if Assigned( OnCreateEditorCell ) and
     not (csDesigning in ComponentState) then
    Result := OnCreateEditorCell( Self )
  else
    Result := TDBUCustomEditorCell.Create;

  Result.FGrid := Self;
  Result.FInplaceEdit := AInplaceEdit;
  Result.FFormatter := CreateEditorFormatter;
end;

function TDBUGrid.CreateEditor: TInplaceEdit;
var
  AEditor : TDBUInplaceEdit;
begin
  if Assigned( OnCreateInplaceEdit ) then
    AEditor := OnCreateInplaceEdit( Self )
  else
    AEditor := TDBUInplaceEdit.Create( Self );

  AEditor.FDBUGrid := Self;
  AEditor.FCell := CreateEditorCell( AEditor );

  Result := AEditor;
end;

function TDBUGrid.CreateFormatter: TDBUFormatter;
begin
  if Assigned( OnCreateFormatter ) then
    Result := OnCreateFormatter( Self )
  else
    Result := TDBUFormatter.Create;
end;

function TDBUGrid.CreateEditorFormatter: TDBUFormatter;
begin
  if Assigned( OnCreateEditorFormatter ) then
    Result := OnCreateEditorFormatter( Self )
  else
    Result := TDBUFormatter.Create;
end;

procedure TDBUGrid.DeleteColumn(ACol: Longint);
begin
  inherited DeleteColumn(ACol);
end;

procedure TDBUGrid.InsertColumn(ACol, AWidth: Longint);
begin
  ColCount := ColCount + 1;
  ColWidths[ColCount -1] := AWidth;
  MoveColumn(ColCount -1, Max(Min(ACol, ColCount -1), 0));
end;

procedure TDBUGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  CellHit: TGridCoord;
  HitOnActive, doDbl, doShowEdit : Boolean;
  NewOptions : TGridOptions;
begin
  CellHit := GridCoord(-1, -1);
  doDbl := False;
  doShowEdit := False;
  
  if csDesigning in ComponentState then
    inherited
  else
  begin
    if (Shift = [ssLeft]) and
       (Button = mbLeft) and
       (GetFocus <> Handle) then
    begin
      Windows.SetFocus(Handle);
    end;

    HitOnActive := False;

    if ([dgEditing, dgMultiSelect] * DBUOptions = [dgEditing, dgMultiSelect]) then
    begin
      NewOptions := Options;
      
      CellHit := MouseCoord( X, Y );
      if CellEditable(CellHit.X, CellHit.Y) then
      begin
        HitOnActive := (CellHit.X = Col) and (CellHit.Y = Row);

        doShowEdit := (dgAlwaysShowEditor in DBUOptions) or CellShowEditor(Button, Shift, X, Y);
        if doShowEdit then
        begin
          NewOptions := Options - [goRangeSelect] + [goEditing, goAlwaysShowEditor];
          RemoveSelection;
        end
        else if not EditorMode then
        begin
          if HitOnActive  then
          begin
            NewOptions := Options - [goRangeSelect] + [goEditing];
//            if (dgAlwaysShowEditor in DBUOptions) or CellShowEditor(Button, Shift, X, Y) then
//              Include(NewOptions, goAlwaysShowEditor);
            RemoveSelection;
          end
          else
            NewOptions := Options + [goRangeSelect] - [goEditing, goAlwaysShowEditor];
        end
        else if not HitOnActive then
          NewOptions := Options + [goRangeSelect] - [goEditing, goAlwaysShowEditor];
      end
      else
        NewOptions := Options + [goRangeSelect] - [goEditing, goAlwaysShowEditor];

      if NewOptions <> Options then
      begin
        FKillInvalidate := True;
        Options := NewOptions;
      end;
    end;

    if (Button = mbLeft) and (ssDouble in Shift) then
    begin
      doDbl := True;
      Exclude(Shift, ssDouble);
    end
    else
      doDbl := False;

    inherited;

    if FGridState = gsNormal then
    begin
      if (HitOnActive or
          (doShowEdit and (Col = CellHit.X) and (Row = CellHit.Y))) and
         EditorMode and
         CanEditModify then
      begin
        ForceInplaceCreation;

        DBUInplaceEdit.Cell.RunMouseDown( Button, Shift, X - InplaceEditor.Left, Y - InplaceEditor.Top );
      end
      else
        Cell.RunMouseDown( Button, Shift, X, Y );
    end
    else if EditorMode and (FGridState in  [gsColSizing, gsRowSizing]) then
    begin
      DBUInplaceEdit.Flush;
      HideEditor;
    end;
  end;
  
  if doDbl then
    DblClick;
end;

procedure TDBUGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if not (csDesigning in ComponentState) and
     (FGridState = gsNormal) then
    Cell.RunMouseMove( Shift, X, Y );
end;

procedure TDBUGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  AGridState : TGridState;
  CellHit : TGridCoord;
  NewOptions : TGridOptions;
  DBUEditOptions : TDBUGridOptions;
begin
  if (DBUOptions * [dgAlwaysShowEditor, dgEditing] = [dgAlwaysShowEditor, dgEditing]) then
  begin
    CellHit := MouseCoord( X, Y );
    if (CellHit.X = Col) and
       (CellHit.Y = Row) and
       CanEditModify then
    begin
      DBUEditOptions := DBUOptions*[dgAlwaysShowEditor, dgEditing];
      NewOptions := Options + DBUToGridOptions( DBUEditOptions );
      if NewOptions <> Options then
      begin
        FKillInvalidate := True;
        Options := NewOptions;
      end;

      RemoveSelection;
      ShowEditor;
      DBUInplaceEdit.Cell.RunMouseDown( Button, Shift, X - InplaceEditor.Left, Y - InplaceEditor.Top );
    end;
  end;

  AGridState := FGridState;
  inherited;
  if not (csDesigning in ComponentState) and
     (AGridState = gsNormal) then
    Cell.RunMouseUp( Button, Shift, X, Y );
end;

function TDBUGrid._AddRef: Integer;
begin
  Result := 1;
end;

function TDBUGrid._Release: Integer;
begin
  Result := 1;
end;

procedure TDBUGrid.SetOnCreateCell(const Value: TOnCreateCellEvent);
begin
  FOnCreateCell := Value;
end;

procedure TDBUGrid.SetOnCreateEditorCell(
  const Value: TOnCreateEditorCellEvent);
begin
  FOnCreateEditorCell := Value;
end;

procedure TDBUGrid.SetOnCreateInplaceEdit(
  const Value: TOnCreateInplaceEditEvent);
begin
  FOnCreateInplaceEdit := Value;
end;

function TDBUGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  CanEdit : Boolean;
  NewOptions : TGridOptions;
  DBUEditOptions : TDBUGridOptions;
begin
  if not (csDesigning in ComponentState) and
     (DBUInplaceEdit <> nil) then
  begin
    Result := DBUInplaceEdit.MoveFocus( ACol, ARow );

    if not Result then
      ShowEditor  
    else if (FGridState = gsNormal) then
    begin
      CanEdit := Cell.CanEdit( ACol, ARow );

      DBUEditOptions := DBUOptions*[dgAlwaysShowEditor, dgEditing];
      if not CanEdit then
        NewOptions := Options - DBUToGridOptions( DBUEditOptions )
      else
        NewOptions := Options + DBUToGridOptions( DBUEditOptions );

      if NewOptions <> Options then
      begin
        FKillInvalidate := True;
        Options := NewOptions;
      end;
    end;
  end
  else
    Result := True;

  if not Result then
    Abort;
end;

function TDBUGrid.CanEditModify: Boolean;
begin
  Result := not FUpdate and
            inherited CanEditModify and
            (dgEditing in DBUOptions) and
            Cell.CanEdit( Col, Row );
end;

function TDBUGrid.CellEditable(ACol, ARow : Integer) : Boolean;
begin
  if InplaceEditor <> nil then
    DBUInplaceEdit.Cell.ResetCaches;

  if [csLoading, csFixups, csReading]*ComponentState <> [] then
    Result := False
  else
    Result := Cell.CanEdit( Col, Row );
end;

function TDBUGrid.CanEditShow: Boolean;
begin
(*  if InplaceEditor <> nil then
    DBUInplaceEdit.Cell.ResetCaches;
*)
  if [csLoading, csFixups, csReading]*ComponentState <> [] then
    Result := False
  else
    Result := not FUpdate and
              CellEditable( Col, Row ) and  // Before inherited in order to force a GetCell
//              Cell.CanEdit( Col, Row ) and  // Before inherited in order to force a GetCell
              inherited CanEditShow;
end;

function TDBUGrid.CellShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
begin
  Result := Cell.ShowEditor(Button, Shift, X, Y);
end;

procedure TDBUGrid.RedrawRect(ARect: TGridRect);
var
  iRow, iCol : Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  for iRow := Max(ARect.Top, 0) to Min(ARect.Bottom, RowCount) do
    for iCol := Max(ARect.Left, 0) to Min(ARect.Right, ColCount) do
       RedrawCell(iCol, iRow);
end;

procedure TDBUGrid.ForceInplaceCreation;
var
  AddEditing : Boolean;
begin
  if (dgEditing in DBUOptions) and
     not Assigned( DBUInplaceEdit ) then
  begin
    AddEditing := not (goEditing in Options);
    if AddEditing then
    begin
      FKillInvalidate := True;
      Options := Options + [goEditing]
    end;

    ShowEditor;
    HideEditor;

    if AddEditing then
    begin
      FKillInvalidate := True;
      Options := Options - [goEditing];
    end;
  end;
end;

procedure TDBUGrid.DeletePress;
begin
  if Assigned(DBUInplaceEdit) and
     EditorMode then
    DBUInplaceEdit.DeletePress;
end;

function TDBUGrid.IndexOfKeyComb( Key: Word; Shift: TShiftState ) : Integer;
var
  i : Integer;
  aComb : PDBUKeyComb;
begin
  Result := -1;
  for i := 0 to FDisabledKeys.Count -1 do
  begin
    aComb := PDBUKeyComb(FDisabledKeys[i]);
    if (aComb^.Key = Key) and
       (aComb^.Shift = Shift) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TDBUGrid.DisableKeyCombiantion( Key: Word; Shift: TShiftState );
var
  KeyComb : PDBUKeyComb;
begin
  new( KeyComb );
  KeyComb^ := DBUKeyComb( Key, Shift );
  FDisabledKeys.Add( KeyComb );
end;

procedure TDBUGrid.EnableKeyCombiantion( Key: Word; Shift: TShiftState );
var
  idx : Integer;
begin
  idx := IndexOfKeyComb( Key, Shift );
  if idx >= 0 then
  begin
    Dispose(FDisabledKeys[idx]);
    FDisabledKeys.Delete(idx);
  end;
end;

function TDBUGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (IndexOfKeyComb(Key, Shift) = -1);
end;

procedure TDBUGrid.KeyPress(var Key: Char);
begin
  if not CanGridAcceptKey( Ord(Key), [] ) then
    Key := #0;

  inherited;
end;

procedure TDBUGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewOptions : TGridOptions;
  IsPrintable : Boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
    IsPrintable := (Key >= 45) or (Key in [9, 32]);

    if (DBUOptions * [dgEditing, dgMultiSelect] = [dgEditing, dgMultiSelect]) then
    begin
      if (ssShift in Shift) and
         (not EditorMode or (dgAlwaysShowEditor in DBUOptions) ) and
         not IsPrintable then
      begin
        NewOptions := Options + [goRangeSelect] - [goEditing, goAlwaysShowEditor];
        if dgAlwaysShowEditor in DBUOptions then
          HideEditor;
      end
      else if IsPrintable then
      begin
        NewOptions := Options - [goRangeSelect] + [goEditing];
        if dgAlwaysShowEditor in DBUOptions then
          Include(NewOptions, goAlwaysShowEditor);
        RemoveSelection;
      end
      else
      begin
        NewOptions := Options - [goRangeSelect] + [goEditing];
        if dgAlwaysShowEditor in DBUOptions then
          Include(NewOptions, goAlwaysShowEditor);
      end;

      if NewOptions <> Options then
      begin
        FKillInvalidate := True;
        Options := NewOptions;
      end;
    end;

    if FGridState = gsNormal then
    begin
      if CanEditModify then
      begin
        ForceInplaceCreation;

        if Assigned( DBUInplaceEdit ) then
          DBUInplaceEdit.Cell.RunKeyDown( Key, Shift )
      end
      else
      begin
        Cell.GetCell( Col, Row );
        Cell.RunKeyDown( Key, Shift );
      end;
    end;
  end;

  inherited;
end;

procedure TDBUGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TDBUGrid.RemoveSelection;
var
  ASelection : TGridRect;
begin
  ASelection.Bottom := Row;
  ASelection.Top := Row;
  ASelection.Left := Col;
  ASelection.Right := Col;
  Selection := ASelection;
end;

procedure TDBUGrid.SetOnCreateEditorFormatter(
  const Value: TOnCreateEditorFormatterEvent);
begin
  FOnCreateEditorFormatter := Value;
end;

procedure TDBUGrid.SetOnCreateFormatter(const Value: TOnCreateFormatterEvent);
begin
  FOnCreateFormatter := Value;
end;

procedure TDBUGrid.SetDBUOptions(Value: TDBUGridOptions);
begin
  if dgRowSelect in Value then
  begin
    Exclude(Value, dgEditing);
    Exclude(Value, dgAlwaysShowEditor);
  end;

  if FDBUOptions <> Value then
  begin
    Options := DBUToGridOptions( Value ) + [goFixedVertLine, goFixedHorzLine] - [goEditing, goAlwaysShowEditor];
    FDBUOptions := Value;
  end;
end;

function TDBUGrid.DBUToGridOptions(const Value: TDBUGridOptions) : TGridOptions;
var
  iOp : TDBUGridOption;
  iGOp : TGridOption;
  DoContinue : Boolean;
begin
  Result := [];
  for iOp := Low( TDBUGridOption ) to High( TDBUGridOption ) do
  begin
    DoContinue := False;
    if iOp in Value then
    begin
      case iOp of
        dgVertLine          : iGOp := goVertLine;
        dgHorzLine          : iGOp := goHorzLine;
        dgMultiSelect       : iGOp := goRangeSelect;
        dgDrawFocusSelected : iGOp := goDrawFocusSelected;
        dgRowSizing         : iGOp := goRowSizing;
        dgColSizing         : iGOp := goColSizing;
        dgEditing           : iGOp := goEditing;
        dgTabs              : iGOp := goTabs;
        dgAlwaysShowEditor  : iGOp := goAlwaysShowEditor;
        dgThumbTracking     : iGOp := goThumbTracking;
        dgRowSelect         : iGOp := goRowSelect;
      else
        begin
          iGOp := goFixedHorzLine;
          DoContinue := True;
        end;
      end;

      if DoContinue then
        Continue
      else
        Include( Result, IGOp );
    end;
  end;
end;

{$ifndef LINUX}
function TDBUGrid.GetEditText(ACol, ARow: Integer): string;
{$else}
function TDBUGrid.GetEditText(ACol, ARow: Longint): WideString;
{$endif LINUX}
begin
  if Assigned( DBUInplaceEdit ) then
  begin
    if (DBUInplaceEdit.Cell.Col <> ACol) or
       (DBUInplaceEdit.Cell.Row <> ARow) then
      raise Exception.Create( Self.ClassName + '.GetEditText: Editor in wrong cell!' )
    else
      Result := DBUInplaceEdit.Cell.GetEditText;
  end;
end;

{$ifndef LINUX}
procedure TDBUGrid.SetEditText(ACol, ARow: Integer; const Value: string);
{$else}
procedure TDBUGrid.SetEditText(ACol, ARow: Longint; const Value: WideString); 
{$endif LINUX}
begin
  if Assigned( DBUInplaceEdit ) then
  begin
    if (DBUInplaceEdit.Cell.Col = ACol) and
       (DBUInplaceEdit.Cell.Row = ARow) then
      DBUInplaceEdit.Cell.SetEditText( Value );
  end;
end;

function TDBUGrid.GetDBUInplaceEdit: TDBUInplaceEdit;
begin
  if InplaceEditor is TDBUInplaceEdit then
    Result := TDBUInplaceEdit( InplaceEditor )
  else
    Result := nil;
end;

procedure TDBUGrid.Invalidate;
begin
  if not (FKillInvalidate or FUpdate) then
    inherited
  else
    FKillInvalidate := False;
end;

procedure TDBUGrid.RedrawCell(ACol, ARow: Longint);
begin
  InvalidateCell( ACol, ARow );
end;

procedure TDBUGrid.RedrawCol(ACol: Longint);
var
  iRow : Integer;
begin
  if not HandleAllocated then Exit;

  for iRow := 0 to FixedRows -1 do
    InvalidateCell( ACol, iRow );

  for iRow := TopRow to TopRow + VisibleRowCount +1 do
    InvalidateCell( ACol, iRow );
end;

procedure TDBUGrid.RedrawRow(ARow: Longint);
var
  iCol : Integer;
begin
  if not HandleAllocated then Exit;

  for iCol := 0 to FixedCols -1 do
    InvalidateCell( iCol, ARow );

  for iCol := LeftCol to LeftCol + VisibleColCount +1 do
    InvalidateCell( iCol, ARow );
end;

procedure TDBUGrid.DoExit;
begin
  try
    if Assigned(InplaceEditor) and
       InplaceEditor.Modified then
      DBUInplaceEdit.CommitChanges;
  except
    if CanFocus then
      SetFocus;
    raise;
  end;

  inherited;
end;

procedure TDBUGrid.SetKilling( Value : Boolean );
begin
  FKilling := Value;
  Cell.Killing := Value;
  if Assigned( DBUInplaceEdit ) then
    DBUInplaceEdit.Cell.Killing := True;
end;

{$ifndef LINUX}
procedure TDBUGrid.SetEnabled(Value: Boolean);
{$else LINUX}
procedure TDBUGrid.SetEnabled(const Value: Boolean);
{$endif LINUX}
begin
  if not Value then
    FFocusedOnDisable := HasFocus;

  inherited;

  if not Value then
  begin
    if Assigned( DBUInplaceEdit ) then
      DBUInplaceEdit.Flush;
    Options := Options - [goEditing];
  end
  else if dgEditing in DBUOptions then
    Options := Options + [goEditing];

  if Value and FFocusedOnDisable and
     CanFocus and Showing and
     ([csDestroying, csFreeNotification] * ComponentState = []) then
  begin
    SetFocus;
  end;
end;

{$ifndef LINUX}
procedure TDBUGrid.SetParent(AParent: TWinControl);
{$else LINUX}
procedure TDBUGrid.SetParent(const AParent: TWidgetControl);
{$endif LINUX}
begin
  inherited;

  if Assigned(AParent) then
  begin
    FAncestor := Self;
    repeat
      FAncestor := FAncestor.Parent;
    until (FAncestor is TForm) or not Assigned( FAncestor.Parent );
  end
  else if Assigned(FAncestor) then
    FAncestor.FreeNotification( Self );
end;

procedure TDBUGrid.DoFontChanged(Sender: TObject);
begin
  if Assigned( FInhFontChanged ) then
    FInhFontChanged( Sender );

  if DefaultRowHeight < Abs( Font.Height ) + 4 then
    DefaultRowHeight := Abs( Font.Height ) + 4;
end;

procedure TDBUGrid.BeforeDestruction;
begin
  inherited;
  // nothing
end;

procedure TDBUGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FAncestor) and
     (Operation = opRemove ) and
     Assigned( DBUInplaceEdit ) then
    DBUInplaceEdit.Flush;

  inherited;
end;

function TDBUGrid.HasFocus : Boolean;
begin
  Result := Focused or ( (InplaceEditor <> nil) and InplaceEditor.Focused );
end;

{/** Standard processing + generate OnVScroll */}

{$ifndef LINUX}
procedure TDBUGrid.WMVScroll(var Msg: TWMScroll);
begin
  inherited;

  if Assigned(FOnVScroll) then
    FOnVScroll(Self, Msg.ScrollCode, Msg.Pos);
end;

{/** Standard processing + generate OnHScroll */}

procedure TDBUGrid.WMHScroll(var Msg: TWMScroll);
begin
  inherited;

  if Assigned(FOnHScroll) then
    FOnHScroll(Self, Msg.ScrollCode, Msg.Pos);
end;
{$endif LINUX}

procedure TDBUGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;

  if Assigned(OnTopLeftChanged) then
    OnTopLeftChanged(Self);
end;

function TDBUGrid.CommitChanges : Boolean;
begin
  Result := True;
  try
    if Assigned(InplaceEditor) and
       InplaceEditor.Modified then
    begin
      DBUInplaceEdit.CommitChanges;
      Result := not DBUInplaceEdit.Cell.Modified;
    end;
  except
    SetFocus;
    raise;
  end;
end;

class function TDBUGrid.FlushActiveControl(AbortOnFailiure : Boolean = True) : Boolean;
var
  ACtrl : TWinControl;
  AGrid : TDBUGrid;
begin
  Result := True;
  ACtrl := Screen.ActiveControl;
  if (ACtrl is TDBUInplaceEdit) then
    AGrid := TDBUInplaceEdit(ACtrl).FDBUGrid
  else if (ACtrl is TDBUGrid) then
    AGrid := TDBUGrid(ACtrl)
  else
    AGrid := nil;

  if Assigned(AGrid) then
  begin
    try
      Result := AGrid.CommitChanges;
    except
      Result := False;
    end;

    if AbortOnFailiure and not Result then
      Abort;
  end;
end;

{ TAbstractDBUCell }

constructor TAbstractDBUCell.Create;
begin
  inherited Create;

  FCol := -1;
  FRow := -1;
end;

destructor TAbstractDBUCell.Destroy;
begin
  FreeAndNil( FFormatter );
  inherited Destroy;
end;

function TAbstractDBUCell.GetMouseBtnStates: TMouseBtnStates;
begin
  Result := FMouseBtnStates;
end;

function TAbstractDBUCell.GetMouseStateDown: TMouseState;
begin
  Result := FMouseStateDown;
end;

function TAbstractDBUCell.GetMouseStateMove: TMouseMoveState;
begin
  Result := FMouseStateMove;
end;

function TAbstractDBUCell.GetMouseStateUp: TMouseState;
begin
  Result := FMouseStateUp;
end;

procedure TAbstractDBUCell.MouseDown(NewMouseState: TMouseState);
begin
  MouseStateDown := NewMouseState;
end;

procedure TAbstractDBUCell.MouseMove(NewMouseState: TMouseMoveState);
begin
  MouseStateMove := NewMouseState;
end;

procedure TAbstractDBUCell.MouseUp(NewMouseState: TMouseState);
begin
  MouseStateUp := NewMouseState;
end;

procedure TAbstractDBUCell.KeyDown( var NewKeyState: TKeyState);
begin
  KeyStateDown := NewKeyState;
end;

procedure TAbstractDBUCell.KeyUp( var NewKeyState: TKeyState);
begin
  KeyStateUp := NewKeyState;
end;

procedure TAbstractDBUCell.RunMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Nothing
end;

procedure TAbstractDBUCell.RunMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Nothing
end;

procedure TAbstractDBUCell.RunMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Nothing
end;

procedure TAbstractDBUCell.RunKeyDown(var Key: Word; Shift: TShiftState);
begin
  // Nothing
end;

procedure TAbstractDBUCell.RunKeyUp(var Key: Word; Shift: TShiftState);
begin
  // Nothing
end;

procedure TAbstractDBUCell.SetMouseStateDown(const Value: TMouseState);
begin
  FMouseStateDown := Value;
end;

procedure TAbstractDBUCell.SetMouseStateMove(const Value: TMouseMoveState);
begin
  FMouseStateMove := Value;
end;

procedure TAbstractDBUCell.SetMouseStateUp(const Value: TMouseState);
begin
  FMouseStateUp := Value;
end;

function TAbstractDBUCell.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TAbstractDBUCell._AddRef: Integer;
begin
  result := 1;
end;

function TAbstractDBUCell._Release: Integer;
begin
  result := 1;
end;

function TAbstractDBUCell.GetCol: Integer;
begin
  Result := FCol;
end;

function TAbstractDBUCell.GetRow: Integer;
begin
  Result := FRow;
end;

function TAbstractDBUCell.GetKeyStateDown: TKeyState;
begin
  Result := FKeyStateDown;
end;

function TAbstractDBUCell.GetKeyStateUp: TKeyState;
begin
  Result := FKeyStateUp;
end;

procedure TAbstractDBUCell.SetKeyStateDown(const Value: TKeyState);
begin
  FKeyStateDown := Value;
end;

procedure TAbstractDBUCell.SetKeyStateUp(const Value: TKeyState);
begin
  FKeyStateUp := Value;
end;

procedure TAbstractDBUCell.Reset;
begin
  ResetCaches;
  FCol := -1;
  FRow := -1;
  if Assigned( Formatter ) then
    Formatter.Reset;
end;

procedure TAbstractDBUCell.ResetCaches;
begin
  // nothing
end;

procedure TAbstractDBUCell.ResetValues;
begin
  // nothing
end;

procedure TAbstractDBUCell.GetCell(ACol, ARow: Integer);
begin
  FCol := ACol;
  FRow := ARow;
end;

function TAbstractDBUCell.CanEdit(ACol, ARow: Integer): Boolean;
begin
  GetCell( ACol, ARow );
  Result := not ReadOnly;
end;

function TAbstractDBUCell.ShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
begin
  Result := False;
end;

function TAbstractDBUCell.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TAbstractDBUCell.GetFormatter(DrawState: TGridDrawState): TDBUFormatter;
begin
  Result := FFormatter;
end;

procedure TAbstractDBUCell.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

function TAbstractDBUCell.GetEnabled: Boolean;
begin
  Result := Grid.Enabled;
end;

function TAbstractDBUCell.CanEditModify: Boolean;
begin
  Result := Grid.CanEditModify;
end;

procedure TAbstractDBUCell.SetFormatter(AFormatter: TDBUFormatter);
begin
  if Formatter <> nil then
    Formatter.Free;

  FFormatter := AFormatter;
end;

procedure TAbstractDBUCell.SetKilling(const Value: Boolean);
begin
  FKilling := Value;
end;

{ TDBUInplaceEdit }

constructor TDBUInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;

  FForwardMovement := True;
{$ifdef LINUX}
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  MaxLength := 100;
{$endif LINUX}
end;

destructor TDBUInplaceEdit.Destroy;
begin
  Flush;
  inherited;

  Cell.Free;
{$ifdef LINUX}
  FCanvas.Free;
{$endif LINUX}
end;

procedure TDBUInplaceEdit.UndoChanges;
begin
  Cell.DoUndoChanges;
  Text := Cell.GetEditText;
  SelectAll;
  Modified := False;
end;

procedure TDBUInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Cell.RunMouseDown( Button, Shift, X, Y );

  inherited;
end;

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

procedure TDBUInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    DBUGrid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := DBUGrid.OnKeyDown;
    if Assigned(GridKeyDown) then GridKeyDown(Grid, Key, Shift);
  end;

  function DoForwardMovement: Boolean;
  begin
    Result := ForwardMovement and not (dgAlwaysShowEditor in DBUGrid.DBUOptions);
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    Result.StartPos := SelStart;
    Result.EndPos := SelStart + SelLength;
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := ((StartPos = 0) or (EndPos = StartPos)) and
        (EndPos = GetTextLen);
   end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (StartPos = 0) and ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  if (Key = Key_Escape) then
    UndoChanges
  else
    Cell.RunKeyDown( Key, Shift );

  case Key of
    Key_Up: if DoForwardMovement and (Ctrl or LeftSide) then SendToParent;
    Key_Right: if DoForwardMovement and (Ctrl or RightSide) then SendToParent;
    Key_Home: if DoForwardMovement and (Ctrl or LeftSide) then SendToParent;
    Key_End: if DoForwardMovement and (Ctrl or RightSide) then SendToParent;
  end;

  if Key <> 0 then
    inherited;
end;

procedure TDBUInplaceEdit.DeletePress;
var
  AStr : String;
  CursPos : Integer;
begin
  if SelLength > 0 then
    SelText := ''
  else
  begin
    CursPos := SelStart;
    AStr := Text;
    Delete( AStr, SelStart+1, 1 );
    Text := AStr;
    SelStart := CursPos;
  end;
  Cell.SetEditText( Text );
end;

procedure TDBUInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Cell.RunKeyUp( Key, Shift );

  inherited;
end;

procedure TDBUInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);

begin
  Cell.RunMouseMove( Shift, X, Y );

  inherited;
end;

procedure TDBUInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Cell.RunMouseUp( Button, Shift, X, Y );

  inherited;
end;

function TDBUInplaceEdit.EditorRect : TRect;
begin
  Result := BoundsRect;
end;

function TDBUInplaceEdit.ClientRect: TRect;
begin
  Result := inherited ClientRect;
end;

function TDBUInplaceEdit._AddRef: Integer;
begin
  Result := 1;
end;

function TDBUInplaceEdit._Release: Integer;
begin
  Result := 1;
end;

{$ifndef LINUX}
procedure TDBUInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
var
  AParam : THandleCMParams;
begin
  if Assigned( DBUGrid ) and Assigned( Cell ) then
  begin
    AParam := HandleCMParams( Message, DBUGrid.Col, DBUGrid.Row );
    Cell.HandleCancelMode( AParam );
    Message := AParam.Message;
  end;
end;

procedure TDBUInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  HandleMessage( CancelMode, Message );

  inherited;
end;

procedure TDBUInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  inherited;

  HandleMessage( KillFocus, Message );
end;

procedure TDBUInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler( Message );
end;

procedure TDBUInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P : TPoint;
  ACursor : TCursor;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);

  ACursor := crDefault;
  Cell.GetCursor( P, ACursor );

  if ACursor = crDefault then
    inherited
  else
    Windows.SetCursor(Screen.Cursors[ACursor] );
end;

function TDBUInplaceEdit.HandleMessage( AWMType : TWMType; var Message: TMessage) : Boolean;
var
  AParam : THandleWMParams;
begin
  if Assigned( DBUGrid ) and Assigned( Cell ) then
  begin
    AParam := HandleWMParams( AWMType, Message, DBUGrid.Col, DBUGrid.Row  );
    Cell.HandleMessage( AParam );
  end;

  Message := AParam.Message;
  Result := not AParam.RunInh;
end;

procedure TDBUInplaceEdit.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_UNDO  then
  begin
    UndoChanges;
    inherited;
  end
  else if not HandleMessage( AnyMessage, Message ) then
    inherited;
end;

procedure TDBUInplaceEdit.PaintWindow(DC: HDC);
var
  Param : TDrawParams;
  EditRect : TRect;
  Canvas : TCanvas;
  AHandle : THandle;
begin
 if (csDesigning in ComponentState) then
   Exit;

  if Assigned( DBUGrid ) and Assigned( Cell ) then
  begin
    Canvas :=  DBUGrid.Canvas;
    Param := DrawParams( Canvas, DBUGrid.Col, DBUGrid.Row, EditorRect, [gdFocused] );
    Cell.DrawEditor( Param );
    EditRect := Param.Rect;
    AHandle := Handle;
    MapWindowPoints( Grid.Handle, AHandle, EditRect, 2);
    SendMessage( AHandle, EM_SETRECTNP, 0, LongInt(@EditRect));
    SendMessage( AHandle, EM_SCROLLCARET, 0, 0);
  end;

  inherited;
end;
{$else LINUX}
procedure TDBUInplaceEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  inherited;
  TControlCanvas(FCanvas).StartPaint;
  try
    QPainter_setClipRegion(FCanvas.Handle, EventRegion);
    Paint;
  finally
    TControlCanvas(FCanvas).StopPaint;
  end;
end;

procedure TDBUInplaceEdit.Paint;
var
  Param : TDrawParams;
begin
 if (csDesigning in ComponentState) then
   Exit;

  if Assigned( DBUGrid ) and Assigned( Cell ) then
  begin
    Param := DrawParams( FCanvas, DBUGrid.Col, DBUGrid.Row, EditorRect, [gdFocused] );
    Cell.DrawEditor( Param );
  end;

  inherited;
end;
{$endif LINUX}

function TDBUInplaceEdit.SelfEdit: TInplaceEdit;
begin
  Result := Self;
end;

procedure TDBUInplaceEdit.SetMouseCapture(Value: Boolean);
begin
  MouseCapture := Value;
end;

function TDBUInplaceEdit.GetHeight: Integer;
begin
  Result := Height;
end;

function TDBUInplaceEdit.GetWidth: Integer;
begin
  Result := Width;
end;

function TDBUInplaceEdit.MoveFocus(ACol, ARow: Integer): Boolean;
begin
{$ifndef LINUX}
  if Showing or
     Visible or
     Cell.Modified or
     Focused then
{$endif LINUX}
  begin
    Result := Cell.MoveFocus(ACol, ARow);
    if Result then
    begin
{$ifndef LINUX}
      Text := Cell.GetEditText;
{$endif LINUX}
      Modified := Cell.Modified;
    end;
  end
{$ifndef LINUX}
  else
    Result := True;
{$endif LINUX}
end;

procedure TDBUInplaceEdit.UpdateContents;
begin
  MoveFocus( DBUGrid.Col, DBUGrid.Row );
  Text :=  Cell.GetEditText;
end;

function TDBUInplaceEdit.GetEditorMode: Boolean;
begin
  Result := DBUGrid.EditorMode;
end;

procedure TDBUInplaceEdit.SetEditorMode(const Value: Boolean);
begin
  DBUGrid.EditorMode := Value;
end;

procedure TDBUInplaceEdit.CommitChanges;
begin
  if not Cell.ReadOnly and
     Modified then
  begin
    try
      Cell.SetEditText( Text );

      if Cell.Modified then
        Cell.SetCell;
    finally
      Modified := Cell.Modified;
    end;
  end;
end;

procedure TDBUInplaceEdit.Flush;
var
  OldKilling : Boolean;
begin
  if not Cell.ReadOnly then
  begin
    if Modified then
      Cell.SetEditText( Text );

    if Cell.Modified then
    begin
      OldKilling := Cell.Killing;
      Cell.Killing := True;
      try
        Cell.SetCell;
      finally
        Cell.Killing := OldKilling;
      end;
    end;
  end;

  Cell.Reset;
  Cell.GetCell(DBUGrid.Col, DBUGrid.Row);
  Modified := False;
end;

procedure TDBUInplaceEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

{ TDBUCustomGridCell }

constructor TDBUCustomGridCell.Create;
begin
  inherited Create;
end;

procedure TDBUCustomGridCell.DrawCell( var Params : TDrawParams );
begin
  Formatter.PrepareCanvas( Params.Canvas );
  Params.Canvas.FillRect( Params.Rect );
end;

procedure TDBUCustomGridCell.RunMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CellHit : TGridCoord;
  NewMouseStateDown : TMouseState;
begin
  CellHit := Grid.MouseCoord(X, Y);

  NewMouseStateDown := MouseState( Button, Shift, X, Y, CellHit.X, CellHit.Y );
  FMouseBtnStates := MouseBtnStates + [Button];
  MouseDown( NewMouseStateDown );
  MouseStateMove := MouseMoveState( Shift, X, Y, CellHit.X, CellHit.Y );
end;

procedure TDBUCustomGridCell.RunMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  CellHit : TGridCoord;
  NewMouseStateMove : TMouseMoveState;
begin
  CellHit := Grid.MouseCoord(X, Y);

  NewMouseStateMove := MouseMoveState( Shift, X, Y, CellHit.X, CellHit.Y );
  MouseMove(NewMouseStateMove);
end;

procedure TDBUCustomGridCell.RunMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CellHit : TGridCoord;
  NewMouseStateUp : TMouseState;
begin
  CellHit := Grid.MouseCoord(X, Y);

  NewMouseStateUp := MouseState( Button, Shift, X, Y, CellHit.X, CellHit.Y );
  FMouseBtnStates := MouseBtnStates -[Button];

  MouseUp( NewMouseStateUp );
end;

procedure TDBUCustomGridCell.RunKeyDown(var Key: Word; Shift: TShiftState);
var
  NewKeyStateDown : TKeyState;
begin
  NewKeyStateDown := KeyState( Key, Shift, Col, Row );
  KeyDown( NewKeyStateDown );
  Key := NewKeyStateDown.Key;
end;

procedure TDBUCustomGridCell.RunKeyUp(var Key: Word; Shift: TShiftState);
var
  NewKeyStateUp : TKeyState;
begin
  NewKeyStateUp := KeyState( Key, Shift, Col, Row );
  KeyUp( NewKeyStateUp );
  Key := NewKeyStateUp.Key;
end;

{ TDBUCustomEditorCell }

constructor TDBUCustomEditorCell.Create;
begin
  inherited Create;
  FInplaceText := '';
  FModified := False;
end;

procedure TDBUCustomEditorCell.DrawEditor( var Params : TDrawParams );
begin
  // Nothing
end;

function TDBUCustomEditorCell.SetCell: Boolean;
begin
  Result := True;
  FModified := False;
end;

procedure TDBUCustomEditorCell.HandleCancelMode(
  var Params: THandleCMParams);
begin
  // Nothing
end;

procedure TDBUCustomEditorCell.HandleMessage(var Params: THandleWMParams);
begin
  // Nothing
end;

function TDBUCustomEditorCell.MoveFocus(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if (ACol = FCol) and
     (ARow = FRow) then
    Exit;

  try
    if not Modified or SetCell then
      GetCell(ACol, ARow)
    else
      Result := False;
  except
    Result := False;
  end;
end;

procedure TDBUCustomEditorCell.RunKeyDown(var Key: Word; Shift: TShiftState);
var
  NewKeyStateDown : TKeyState;
begin
  NewKeyStateDown := KeyState( Key, Shift, Col, Row );
  KeyDown( NewKeyStateDown );
  Key := NewKeyStateDown.Key;
end;

procedure TDBUCustomEditorCell.RunKeyUp(var Key: Word; Shift: TShiftState);
var
  NewKeyStateUp : TKeyState;
begin
  NewKeyStateUp := KeyState( Key, Shift, Col, Row );
  KeyUp( NewKeyStateUp );
  Key := NewKeyStateUp.Key;
end;

procedure TDBUCustomEditorCell.RunMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewMouseStateDown : TMouseState;
begin
  NewMouseStateDown := MouseState( Button, Shift, X, Y, Col, Row);
  FMouseBtnStates := MouseBtnStates + [Button];
  MouseDown( NewMouseStateDown );
  MouseStateMove := MouseMoveState( Shift, X, Y, Col, Row );
end;

procedure TDBUCustomEditorCell.RunMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  NewMouseStateMove : TMouseMoveState;
begin
  NewMouseStateMove := MouseMoveState( Shift, X, Y, Col, Row );
  MouseMove( NewMouseStateMove );
end;

procedure TDBUCustomEditorCell.RunMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewMouseStateUp : TMouseState;
begin
  NewMouseStateUp := MouseState( Button, Shift, X, Y, Col, Row );
  FMouseBtnStates := MouseBtnStates -[Button];
  MouseUp( NewMouseStateUp );
end;

function TDBUCustomEditorCell.GetEditText: String;
begin
  Result := DoGetEditText;
  InplaceText := Result;
end;

function TDBUCustomEditorCell.DoGetEditText: String;
begin
  Result := '';
end;

procedure TDBUCustomEditorCell.GetCursor(APoint: TPoint;
  var ACursor: TCursor);
begin
  // Nothing
end;

procedure TDBUCustomEditorCell.SetEditText(const AValue: string);
begin
  // Nothing
end;

procedure TDBUCustomEditorCell.UndoChanges;
begin
  InplaceEdit.UndoChanges;
end;

procedure TDBUCustomEditorCell.DoUndoChanges;
begin
  FModified := False;
end;

procedure TDBUCustomEditorCell.SetInplaceText(const Value: String);
begin
  FInplaceText := Value;
end;

{ TAbstractGridDrawer }

constructor TAbstractGridDrawer.Create(AGrid: TDBUGrid);
begin
  FSaveBrush := TBrush.Create;
  FSavePen := TPen.Create;
  FDrawOnFixed := False;

  inherited Create;

  FGrid := AGrid;

  FTimer := TTimer.Create( FGrid );
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
end;

destructor TAbstractGridDrawer.Destroy;
begin
  inherited;

  FTimer.Free;
  FSaveBrush.Free;
  FSavePen.Free;
end;

procedure TAbstractGridDrawer.DoDraw(Canvas: TCanvas);
begin
  SaveState(Canvas);
  try
    Draw(Canvas);
  finally
    RestoreState(Canvas);
  end;
end;

procedure TAbstractGridDrawer.OnTimer(Sender: Tobject);
begin
  DoDraw(Grid.Canvas);
end;

procedure TAbstractGridDrawer.RestoreState(Canvas: TCanvas);
begin
  Canvas.Brush.Assign(FSaveBrush);
  Canvas.Pen.Assign(FSavePen);
end;

procedure TAbstractGridDrawer.SaveState(Canvas: TCanvas);
begin
  FSaveBrush.Assign(Canvas.Brush);
  FSavePen.Assign(Canvas.Pen);
end;

procedure TAbstractGridDrawer.SetDrawOnFixed(const Value: Boolean);
begin
  FDrawOnFixed := Value;
end;

{ TCustomFrameDrawer }

type
  PGridRect = ^TGridRect;

constructor TCustomFrameDrawer.Create(AGrid: TDBUGrid);
begin
  LineWidth := 2;
  BGColor := clWhite;
  FGColor := clBlack;
  Style := psDot;

  FCellFrames := TList.Create;

  inherited Create(AGrid);
end;

destructor TCustomFrameDrawer.Destroy;
begin
  inherited;

  Redraw;
  FreeListWithPointers(FCellFrames);
end;

procedure TCustomFrameDrawer.AddRectToList(ARect : TGridRect; AList : TList);
var
  aPtr : PGridRect;
begin
  New(aptr);
  aPtr^ := ARect;
  AList.Add(aPtr);
end;

function TCustomFrameDrawer.BoxRect(CellFrame : TGridRect) : TRect;
begin
  with CellFrame do
    if DrawOnFixed then
      Result := FGrid.BoxRect( Left, Top, Right, Bottom )
    else
      Result := FGrid.BoxRect( Max(FGrid.FixedCols, Left), Max(FGrid.FixedRows, Top), Right, Bottom );
end;

procedure TCustomFrameDrawer.RedrawArea(ACellFrame : TGridRect);
var
  tmpRect : TGridRect;
begin
  Dec(ACellFrame.Top);
  Dec(ACellFrame.Left);
  FGrid.RedrawRect( ACellFrame );

  tmpRect := ACellFrame;
  tmpRect.Left := 0;
  tmpRect.Right := FGrid.FixedCols -1;
  FGrid.RedrawRect( tmpRect );

  tmpRect := ACellFrame;
  tmpRect.Top := 0;
  tmpRect.Bottom := FGrid.FixedRows -1;
  FGrid.RedrawRect( tmpRect );
end;

procedure TCustomFrameDrawer.AddCellFrame(ACellFrame: TGridRect);
begin
  AddRectToList(ACellFrame, FCellFrames);
end;

procedure TCustomFrameDrawer.Redraw;
var
  i : Integer;
begin
  for i := 0 to FCellFrames.Count -1 do
    RedrawArea(PGridRect(FCellFrames[i])^);
end;

procedure TCustomFrameDrawer.SetBGColor(const Value: TColor);
begin
  FBGColor := Value;
end;

procedure TCustomFrameDrawer.SetFGColor(const Value: TColor);
begin
  FFGColor := Value;
end;

procedure TCustomFrameDrawer.SetLineWidth(const Value: Integer);
begin
  FLineWidth := Value;
end;

procedure TCustomFrameDrawer.SetStyle(const Value: TPenStyle);
begin
  FStyle := Value;
end;

{ TAntFrameDrawer }

procedure TAntFrameDrawer.AddFrame(ACellFrame: TGridRect);
begin
  AddCellFrame(ACellFrame);
end;

constructor TAntFrameDrawer.Create(AGrid: TDBUGrid;
  ACellFrames: array of TGridRect);
var
  i : Integer;
begin
  FBool := False;

  inherited Create(AGrid);

  Style := psDot;

  for i := Low(ACellFrames) to High(ACellFrames) do
    AddRectToList(ACellFrames[i], FCellFrames);

  Redraw;
end;

procedure TAntFrameDrawer.Draw(Canvas : TCanvas);
var
  i, j, k : Integer;
  ARect : TRect;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Mode := pmCopy;
    Pen.Width := 1;

    for i := 0 to FCellFrames.Count -1 do
    begin
      ARect := BoxRect(PGridRect(FCellFrames[i])^);
      Inc( ARect.Right );
      Inc( ARect.Bottom );

      Pen.Style := psSolid;
      if FBool then
        Pen.Color := FGColor
      else
        Pen.Color := BGColor;

      for j := 0 to 1 do
      begin
        for k := 0 to LineWidth -1 do
        begin
          Rectangle(ARect);
          OffsetRect(ARect, -1, -1);
        end;

        Rectangle(ARect);
        OffsetRect(ARect, LineWidth, LineWidth);

        Pen.Style := Style;
        if FBool then
          Pen.Color := BGColor
        else
          Pen.Color := FGColor;
      end;
    end;
  end;

  FBool := not FBool;
end;

{ TLineDrawer }

procedure TLineDrawer.AddLine(AIdx: Integer);
var
  ARect : TGridRect;
begin
  with ARect do
  begin
    Top := AIdx;
    Bottom := AIdx;
    Left := 0;
    Right := Grid.ColCount;
  end;

  AddCellFrame(ARect);
end;

constructor TLineDrawer.Create(AGrid: TDBUGrid; ARows: array of Integer);
var
  i : Integer;
begin
  inherited Create(AGrid);
  fOffSet := 0;

  for i := Low(ARows) to High(ARows) do
    AddLine(ARows[i]);
end;

procedure TLineDrawer.Draw(Canvas : TCanvas);
var
  i, j, k : Integer;
  ARect : TRect;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Mode := pmCopy;

    for i := 0 to FCellFrames.Count -1 do
    begin
      ARect := BoxRect(PGridRect(FCellFrames[i])^);
      Inc( ARect.Right );
      Inc( ARect.Bottom );

      Pen.Color := BGColor;
      Pen.Style := psSolid;
      Pen.Width := 1;

      for j := 0 to 1 do
      begin
        for k := 0 to LineWidth -1 do
        begin
          PenPos := Point(ARect.Left, ARect.Bottom - k + fOffset);
          LineTo(ARect.Right, PenPos.y);
        end;

        Pen.Style := Style;
        Pen.Color := FGColor;
      end;
    end;
  end;
end;

{ TVerticalLineDrawer }

procedure TVerticalLineDrawer.AddLine(AIdx: Integer);
var
  ARect : TGridRect;
begin
  with ARect do
  begin
    Top := 0;
    Bottom := Grid.RowCount;
    Left := AIdx;
    Right := AIdx;
  end;

  AddCellFrame(ARect);
end;

procedure TVerticalLineDrawer.Draw(Canvas: TCanvas);
var
  i, j, k : Integer;
  ARect : TRect;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Mode := pmCopy;

    for i := 0 to FCellFrames.Count -1 do
    begin
      ARect := BoxRect(PGridRect(FCellFrames[i])^);
//      Inc( ARect.Right );
//      Inc( ARect.Bottom );

      Pen.Color := BGColor;
      Pen.Style := psSolid;
      Pen.Width := 1;

      for j := 0 to 1 do
      begin
        for k := 0 to LineWidth -1 do
        begin
          PenPos := Point(ARect.Left-k + fOffset, ARect.Top);
          LineTo(PenPos.x, ARect.Bottom);
        end;

        Pen.Style := Style;
        Pen.Color := FGColor;
      end;
    end;
  end;
end;

end.


