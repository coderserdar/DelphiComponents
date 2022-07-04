unit HeadListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  comctrls, stdctrls, Menus;

type
  THLBDrawCellEvent = procedure(Control: TWinControl; Index, Col: Integer;
    Rect: TRect; State: TOwnerDrawState) of object;

  THLBStyle = (hlbStandard, hlbOwnerDraw);

  THeadListBox = class(TWinControl)
  private
    { Private declarations }
    FHeader: THeaderControl;
    FListBox: TListBox;
    FSplitChar: Char;
    FLBStyle: THLBStyle;
    FPopupMenu: TPopupMenu;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnEndDrag: TEndDragEvent;
    FOnStartDrag : TStartDragEvent;
    FOnDrawCell: THLBDrawCellEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp : TMouseEvent;
    FOnSectionClick: TSectionNotifyEvent;
    LastPosLine: Integer;
    FSingleLine: Boolean;
    function ReturnLocalPoint(Control: TControl; X, Y: Integer): TPoint;
    // przelicza punkty kontrolek sk³adowych na komp. Self
    procedure DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
                       State: TOwnerDrawState);
    procedure DrawSectionLine(HeaderControl: THeaderControl;Section: THeaderSection;
                    Width: Integer; State: TSectionTrackState);
    procedure FClick(Sender: TObject);
    procedure FDblClick(Sender: TObject);
    procedure FDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
                        var Accept: Boolean);
    procedure FEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FMouseDown(Sender: TObject; Button: TMouseButton;
                       Shift: TShiftState; X, Y: Integer);
    procedure FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FMouseUp(Sender: TObject; Button: TMouseButton;
                       Shift: TShiftState; X, Y: Integer);
    procedure FSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
    function GetExtendedSelect: Boolean;
    function GetHCanvas: TCanvas;
    function GetHHandle: hWnd;
    function GetIntegralHeight: Boolean;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetLBCanvas: TCanvas;
    function GetLBHandle: hWnd;
    function GetMultiSelect: Boolean;
    function GetSections: THeaderSections;
    function GetSelCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetSorted: Boolean;
    function GetTopIndex: Integer;
    procedure SetExtendedSelect(const Value: Boolean);
    procedure SetIntegralHeight(const Value: Boolean);
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetSections(const Value: THeaderSections);
    procedure SetSelected(Index: Integer; const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
    procedure SetSplitChar(const Value: Char);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetTopIndex(const Value: Integer);
    function GetDragMode: TDragMode;
    procedure SetDragMode_(const Value: TDragMode);
    function GetDragCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    procedure SetLBStyle(const Value: THLBStyle);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetOnKeyDown: TKeyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnKeyUp: TKeyEvent;
    procedure SetOnKeyDown(const Value: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnKeyUp(const Value: TKeyEvent);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Nowe w³aœciwosci
    property LBCanvas:  TCanvas read GetLBCanvas;
    property HCanvas: TCanvas read GetHCanvas;
    property LBHandle: hWnd read GetLBHandle;
    property HHandle: hWnd read GetHHandle;
    //publiczne w³aœciwosci TListBox
    property SelCount: Integer read GetSelCount;
    property Selected[Index: Integer]: Boolean read GetSelected
                                               write SetSelected;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Enabled;
    property Color: TColor read GetColor write SetColor;
    property Font;
    property Hint;
    property ParentFont;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property SingleLine: Boolean read FSingleLine write FSingleLine default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnStartDrag : TStartDragEvent read FOnStartDrag write FOnStartDrag;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
//    FOnDrawItem: TDrawItemEvent;
//    FOnMeasaureItem: TMenuMeasureItemEvent;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
    // W³aœciwoœci ListBox
    property DragCursor: TCursor read GetDragCursor write SetCursor;
    property DragMode: TDragMode read GetDragMode write SetDragMode_;
    property ExtendedSelect: Boolean read GetExtendedSelect
                                     write SetExtendedSelect;
    property IntegralHeight: Boolean read GetIntegralHeight
                                     write SetIntegralHeight;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read GetItems write SetItems;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property Sorted: Boolean read GetSorted write SetSorted;
    // W³aœciwoœci Header
    property Sections: THeaderSections read GetSections write SetSections;
    property OnSectionClick: TSectionNotifyEvent read FOnSectionClick write FOnSectionClick;
    // Nowe w³aœciwoœci
    property SplitChar: Char read FSplitChar write SetSplitChar;
    property LBStyle: THLBStyle read FLBStyle write SetLBStyle;
    property OnDrawCell: THLBDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;

implementation

{ THeadListBox }

constructor THeadListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle - [csSetCaption];
  FSingleLine:= True;
  Width:=150;
  Height:=100;
  FSplitChar:=#32;
  BevelKind:=bkTile;
  BevelInner:=bvLowered;
  BevelOuter:=bvLowered;
  FHeader:=THeaderControl.Create(Self);
  with FHeader do begin
    Align:=alTop;
    Parent:=Self;
    OnMouseDown:= FMouseDown;
    OnMouseUp:= FMouseUp;
    OnMouseMove:= FMouseMove;
    OnSectionTrack:= DrawSectionLine;
    OnSectionClick:= FSectionClick;
  end;
  FListBox:=TListBox.Create(Self);
  with FListBox do begin
    Align:=alClient;
    BorderStyle:=bsNone;
    Parent:=Self;
    Style:= lbOwnerDrawFixed;
    OnDrawItem:=DrawItem;
    OnClick:= FClick;
    OnDblClick:= FDblClick;
    OnMouseDown:= FMouseDown;
    OnMouseUp:= FMouseUp;
    OnMouseMove:= FMouseMove;
    OnStartDrag:= FStartDrag;
    OnEndDrag:= FEndDrag;
    OnDragOver:= FDragOver;
    OnDragDrop:= FDragDrop;
  end;
end;

destructor THeadListBox.Destroy;
begin
  FHeader.Free;
  FListBox.Free;
  inherited Destroy;
end;

procedure THeadListBox.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var i, CurPos   : Integer;
    IIndex      : string;

function GetNextStr: string;
var TmpPos : Integer;
begin
  TmpPos:= Pos(FSplitChar, IIndex);
  if (TmpPos > 0) and (CurPos <= Length(IIndex)) then begin
    Result:= Copy(IIndex, CurPos, TmpPos - CurPos);
    IIndex[TmpPos]:= Chr(Ord(FSplitChar) + 1);
    CurPos:= TmpPos + 1;
  end
  else if TmpPos = 0 then
       begin
         Result:= Copy(IIndex, CurPos, Length(IIndex));
         CurPos:= Length(IIndex) + 1;
       end
end;

const Alignment: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
      Single_Line: array[Boolean] of Word = (0, DT_SINGLELINE);
begin
  IIndex:=Items[Index];
  CurPos:=1;
  LBCanvas.FillRect(Rect);
  for i:=0 to FHeader.Sections.Count - 1 do begin
    Rect.Left:= FHeader.Sections[i].Left;
    Rect.Right:= FHeader.Sections[i].Right;
    if LBStyle = hlbStandard
      then DrawText(LBCanvas.Handle,PChar(GetNextStr), -1, Rect,
            Single_Line[FSingleLine] or DT_VCENTER or Alignment[Sections[i].Alignment])
      else if Assigned(FOnDrawCell)
            then FOnDrawCell(Self, Index, i, Rect,State);
  end;
end;

procedure THeadListBox.FClick(Sender: TObject);
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure THeadListBox.FDblClick(Sender: TObject);
begin
  if Assigned(FONDblClick) then FOnDblClick(Self);
end;

procedure THeadListBox.FDragDrop(Sender, Source: TObject; X, Y: Integer);
var P: TPoint;
begin
  if Sender is TListBox then begin
    P:= ReturnLocalPoint(Sender as TControl, X, Y);
    if Assigned(FOnDragDrop) then FOnDragDrop(Self, Source, P.X, P.Y);
  end;
end;

procedure THeadListBox.FDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var P: TPoint;
begin
  if Sender is TListBox then begin
    P:= ReturnLocalPoint(Sender as TControl, X, Y);
    if Assigned(FOnDragOver) then FOnDragOver(Self, Source, P.X, P.Y,
                                   State, Accept);
  end;
end;

procedure THeadListBox.FEndDrag(Sender, Target: TObject; X, Y: Integer);
var P: TPoint;
begin
  if Sender is TListBox then begin
    P:= ReturnLocalPoint(Sender as TControl, X, Y);
    if Assigned(FOnEndDrag) then FOnEndDrag(Self, Target, P.X, P.Y);
  end;
end;

procedure THeadListBox.FStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if Sender is TListBox
    then if Assigned(FOnStartDrag) then FOnStartDrag(Self, DragObject);
end;

procedure THeadListBox.FMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  P:= ReturnLocalPoint(Sender as TControl, X, Y);
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, P.X, P.Y);
end;

procedure THeadListBox.FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  P:= ReturnLocalPoint(Sender as TControl, X, Y);
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, P.X, P.Y);
end;

procedure THeadListBox.FMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  P:= ReturnLocalPoint(Sender as TControl, X, Y);
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, P.X, P.Y);
end;

function THeadListBox.GetExtendedSelect: Boolean;
begin
  Result:= FListBox.ExtendedSelect;
end;

function THeadListBox.GetHCanvas: TCanvas;
begin
  Result:= FHeader.Canvas;
end;

function THeadListBox.GetHHandle: hWnd;
begin
  Result:= FHeader.Handle;
end;

function THeadListBox.GetIntegralHeight: Boolean;
begin
  Result:=FListBox.IntegralHeight;
end;

function THeadListBox.GetItemHeight: Integer;
begin
  Result:= FListBox.ItemHeight;
end;

function THeadListBox.GetItemIndex: Integer;
begin
  Result:= FListBox.ItemIndex;
end;

function THeadListBox.GetItems: TStrings;
begin
  Result:=FListBox.Items;
end;

function THeadListBox.GetLBCanvas: TCanvas;
begin
  Result:= FListBox.Canvas;
end;

function THeadListBox.GetLBHandle: hWnd;
begin
  Result:= FListBox.Handle;
end;

function THeadListBox.GetMultiSelect: Boolean;
begin
  Result:= FListBox.MultiSelect;
end;

function THeadListBox.GetSections: THeaderSections;
begin
  Result:= FHeader.Sections;
end;

function THeadListBox.GetSelCount: Integer;
begin
  Result:= FListBox.SelCount;
end;

function THeadListBox.GetSelected(Index: Integer): Boolean;
begin
  Result:= FListBox.Selected[Index];
end;

function THeadListBox.GetSorted: Boolean;
begin
  Result:= FListBox.Sorted;
end;

function THeadListBox.GetTopIndex: Integer;
begin
  Result:= FListBox.TopIndex;
end;

function THeadListBox.ReturnLocalPoint(Control: TControl; X,
  Y: Integer): TPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result:=ScreenToClient(Control.ClientToScreen(Result));
end;

procedure THeadListBox.SetExtendedSelect(const Value: Boolean);
begin
  FListBox.ExtendedSelect:= Value;
end;

procedure THeadListBox.SetIntegralHeight(const Value: Boolean);
begin
  FListBox.IntegralHeight:= Value;
end;

procedure THeadListBox.SetItemHeight(const Value: Integer);
begin
  FListBox.ItemHeight:= Value;
end;

procedure THeadListBox.SetItemIndex(const Value: Integer);
begin
  FListBox.ItemIndex:= Value;
end;

procedure THeadListBox.SetItems(const Value: TStrings);
begin
  FListBox.Items.Assign(Value);
end;

procedure THeadListBox.SetMultiSelect(const Value: Boolean);
begin
  FListBox.MultiSelect:= Value;
end;

procedure THeadListBox.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  FListBox.PopupMenu:= Value;
  FHeader.PopupMenu:= Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure THeadListBox.SetSections(const Value: THeaderSections);
begin
  FHeader.Sections.Assign(Value);
end;

procedure THeadListBox.SetSelected(Index: Integer; const Value: Boolean);
begin
  FListBox.Selected[Index]:= Value;
end;

procedure THeadListBox.SetSorted(const Value: Boolean);
begin
  FListBox.Sorted:= Value;
end;

procedure THeadListBox.SetSplitChar(const Value: Char);
begin
  FSplitChar := Value;
end;

procedure THeadListBox.SetTopIndex(const Value: Integer);
begin
  FListBox.TopIndex:= Value;
end;

function THeadListBox.GetDragMode: TDragMode;
begin
  Result:=FListBox.DragMode;
end;

procedure THeadListBox.SetDragMode_(const Value: TDragMode);
begin
  FListBox.DragMode:= Value;
end;

function THeadListBox.GetDragCursor: TCursor;
begin
  Result:= FListBox.DragCursor;
end;

procedure THeadListBox.SetCursor(const Value: TCursor);
begin
  FListBox.DragCursor:= Value;
end;

procedure THeadListBox.SetLBStyle(const Value: THLBStyle);
begin
  FLBStyle := Value;
end;

procedure THeadListBox.DrawSectionLine(HeaderControl: THeaderControl;
  Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  FListBox.Canvas.Pen.Style:=psDash;
  FListBox.Canvas.Pen.Mode:=pmNot;
  if LastPosLine > 0 then
  begin
    FListBox.Canvas.MoveTo(LastPosLine, 0);
    FListBox.Canvas.LineTo(LastPosLine, FListBox.ClientHeight);
  end;
  if State = tsTrackEnd then begin
    FListBox.Invalidate;
    LastPosline:=0;
  end
  else begin
    LastPosLine:=Section.Left + Section.Width;
    FListBox.Canvas.MoveTo(LastPosLine, 0);
    FListBox.Canvas.LineTo(LastPosLine, FListBox.ClientHeight);
  end;
end;

function THeadListBox.GetColor: TColor;
begin
  Result:=FListBox.Color;
end;

procedure THeadListBox.SetColor(const Value: TColor);
begin
  FListBox.Color:= Value;
end;

procedure THeadListBox.FSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin 
  if Assigned(FOnSectionClick) then FOnSectionClick(FHeader, Section);
end;

function THeadListBox.GetOnKeyDown: TKeyEvent;
begin
  Result:= FListBox.OnKeyDown;
end;

function THeadListBox.GetOnKeyPress: TKeyPressEvent;
begin
  Result:= FListBox.OnKeyPress;
end;

function THeadListBox.GetOnKeyUp: TKeyEvent;
begin
  Result:= FListBox.OnKeyUp;
end;

procedure THeadListBox.SetOnKeyDown(const Value: TKeyEvent);
begin
  FListBox.OnKeyDown:= Value;
end;

procedure THeadListBox.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  FListBox.OnKeyPress:= Value;
end;

procedure THeadListBox.SetOnKeyUp(const Value: TKeyEvent);
begin
  FListBox.OnKeyUp:= Value;
end;

end.
