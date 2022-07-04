unit mbXPBrushStyleCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPBrushStyleCombo = class(TComboBox)
  private
   FDrawLabel: TDrawItemLabelEvent;
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   function GetSelection: TBrushStyle;
   procedure SetSelection(Value: TBrushStyle);
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Color;
   property DragMode;
   property DragCursor;
   property DropDownCount;
   property Enabled;
   property Font;
   property ItemHeight;
   property MaxLength;
   property ParentColor default false;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Text;
   property Visible;
   property Selection: TBrushStyle read GetSelection write SetSelection default bsSolid;

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

const BrushStyleArray: array[0..7] of string = ('Solid', 'Clear', 'Horizontal', 'Vertical', 'Diagonal Right', 'Diagonal Left', 'Cross', 'Diagonal Cross');

constructor TmbXPBrushStyleCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
end;

destructor TmbXPBrushStyleCombo.Destroy;
begin
 inherited Destroy;
end;

function TmbXPBrushStyleCombo.GetSelection: TBrushStyle;
begin
 Result := TBrushStyle(ItemIndex);
end;

procedure TmbXPBrushStyleCombo.SetSelection(Value: TBrushStyle);
var
 i: integer;
begin
 for i:=0 to 7 do
  if TBrushStyle(i) = Value then
   begin
    ItemIndex := i;
    Exit;
   end;
end;

procedure TmbXPBrushStyleCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R, IR: TRect;
 tc: TColor;
begin
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
 Canvas.FillRect(Rect);
 // set icon rect
 R := Rect;
 Inc(R.Left, 1);
 Inc(R.Top, 1);
 Dec(R.Bottom, 1);
 R.Right := R.Left + (R.Bottom - R.Top);
 // draw icon
 Canvas.Brush.Color := clWindowText;
 Canvas.Brush.Style := TBrushStyle(Index);
 tc := Canvas.Font.Color;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 if Canvas.Font.Color <> tc then
  Canvas.Brush.Color := Canvas.Font.Color;
 Canvas.Rectangle(R);
 // draw text
 Canvas.Brush.Style := bsClear;
 IR := Rect;
 IR.Left := R.Right + 4;
 DrawText(Canvas.Handle, PChar(Items.Strings[index]), Length(Items.Strings[index]), IR, DT_VCENTER or DT_SINGLELINE);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPBrushStyleCombo.CreateWnd;
var
 c: integer;
begin
 inherited CreateWnd;
 Items.Clear;
 for c := 0 to High(BrushStyleArray) do
  Items.Add(BrushStyleArray[c]);
 SetSelection(bsSolid);
end;

procedure TmbXPBrushStyleCombo.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
