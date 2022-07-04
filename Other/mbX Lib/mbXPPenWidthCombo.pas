unit mbXPPenWidthCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPPenWidthCombo = class(TComboBox)
  private
   FDrawLabel: TDrawItemLabelEvent;

   function GetSelection: integer;
   procedure SetSelection(Value: integer);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   property Selection: integer read GetSelection write SetSelection;
  published
   property Color;
   property DragMode;
   property DragCursor;
   property DropDownCount;
   property Enabled;
   property Font;
   property ItemHeight;
   property ItemIndex default 0;
   property MaxLength;
   property ParentFont;
   property ParentColor default false;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Text;
   property Visible;

   property OnDrawItemLabel: TDrawItemLabelEvent read FDrawLabel write FDrawLabel;
   property OnChange;
   property OnClick;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnDrawItem;
   property OnDropDown;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMeasureItem;
   property OnStartDrag;
  end;

implementation

const PenWidthArray: array[0..5] of string = ('1', '2', '3', '4', '5','6');

constructor TmbXPPenWidthCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
end;

destructor TmbXPPenWidthCombo.Destroy;
begin
 inherited Destroy;
end;

function TmbXPPenWidthCombo.GetSelection: integer;
begin
 Result := ItemIndex;
end;

procedure TmbXPPenWidthCombo.SetSelection(Value: integer);
begin
 ItemIndex := Value - 1;
end;

procedure TmbXPPenWidthCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 y, x: integer;
 tc: TColor;
 rgn: HRGN;
begin
 rgn := CreateRectRgnIndirect(Rect);
 SelectClipRgn(Canvas.Handle, rgn);
 Canvas.Font := Font;
 if odSelected in State then
  begin
   Canvas.Brush.Color := clHighlight;
   Canvas.Pen.Color := clHighlightText;
   Canvas.Font.Color := clHighlightText;
  end
 else
  begin
   Canvas.Brush.Color := Color;
   Canvas.Pen.Color := clWindowText;
   Canvas.Font.Color := clWindowText;
  end;
 tc := Canvas.Font.Color;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 if Canvas.Font.Color <> tc then
  Canvas.Pen.Color := Canvas.Font.Color;
 Canvas.FillRect(Rect);
 // draw text
 Inc(Rect.Left, 4);
 DrawText(Canvas.Handle, PChar(IntToStr(Index + 1) + 'px'), Length(IntToStr(Index + 1) + 'px'), Rect, DT_SINGLELINE or DT_VCENTER);
 // draw line
 x := 4 + Canvas.TextWidth(IntToStr(Index + 1) + 'px');
 Canvas.Pen.Width := Index + 1;
 y := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
 Canvas.MoveTo(x + 6, y);
 Canvas.LineTo(Rect.Right - 8, y);
 Canvas.Pen.Color := Canvas.Brush.Color;
 Canvas.Pen.Width := 6;
 Canvas.Brush.Style := bsClear;
 Canvas.Rectangle(x + 6, y - 8, Rect.Right - 6, y + 9);
 Canvas.Brush.Style := bsSolid;
 DeleteObject(rgn);
 Canvas.Pen.Width := 1;
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPPenWidthCombo.CreateWnd;
var
 c: integer;
begin
 inherited CreateWnd;
 Items.Clear;
 for c := 0 to High(PenWidthArray) do
  Items.Add(PenWidthArray[c]);
 ItemIndex := 0;
end;

end.
