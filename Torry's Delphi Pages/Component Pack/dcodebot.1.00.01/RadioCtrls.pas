unit RadioCtrls;

interface

uses
  Classes, Controls, Graphics, SysUtils, Messages, Windows;

type
  TRadioGridLayout = (glHorz, glVert);

  TRadioGrid = class(TCustomControl)
  private
    FCaptionBottom: Integer;
    FDrawFocus: Boolean;
    FDrawItems: Boolean;
    FDrawBottom: Integer;
    FIndent: Integer;
    FItemIndex: Integer;
    FItems: TStrings;
    FLayout: TRadioGridLayout;
    FMultiSelect: Boolean;
    FReadOnly: Boolean;
    FSelection: array of Boolean;
    FSpacing: Integer;
    FOnChange: TNotifyEvent;
    procedure CalculateCaptionBottom;
    procedure ItemsChange(Sender: TObject);
    procedure SetDrawFocus(Value: Boolean);
    procedure SetDrawItems(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetLayout(Value: TRadioGridLayout);
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMWindowPosChanged(var Message: TMessage); message WM_WINDOWPOSCHANGED;
  protected
    function ItemRect(Index: Integer): TRect;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DrawBottom: Integer read FDrawBottom;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
  published
    property Caption;
    property DrawFocus: Boolean read FDrawFocus write SetDrawFocus default True;
    property DrawItems: Boolean read FDrawItems write SetDrawItems;
    property Indent: Integer read FIndent write SetIndent;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: TStrings read FItems write SetItems;
    property Layout: TRadioGridLayout read FLayout write SetLayout;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Spacing: Integer read FSpacing write SetSpacing;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TRadioGrid.Create(AOwner: TComponent);
var
  Strings: TStringList;
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 40;
  TabStop := True;
  FCaptionBottom := -1;
  FDrawItems := True;
  FIndent := 75;
  FItemIndex := -1;
  Strings := TStringList.Create;
  Strings.OnChange := ItemsChange;
  FItems := Strings;
  FSpacing := 24;
end;

destructor TRadioGrid.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TRadioGrid.CalculateCaptionBottom;
var
  Rect: TRect;
begin
  if HandleAllocated and (FCaptionBottom < 0) then
  begin
    Rect := Classes.Rect(2, 2, Width - 4, 1);
    FCaptionBottom := DrawText(Canvas.Handle, PChar(Caption), -1, Rect,
      DT_LEFT or DT_TOP or DT_WORDBREAK or DT_CALCRECT) + 8;
  end;
end;

procedure TRadioGrid.SetDrawFocus(Value: Boolean);
begin
  if Value <> FDrawFocus then
  begin
    FDrawFocus := Value;
    if Focused then
      Invalidate;
  end;
end;

procedure TRadioGrid.SetDrawItems(Value: Boolean);
begin
  if Value <> FDrawItems then
  begin
    FDrawItems := Value;
    Invalidate;
  end;
end;

procedure TRadioGrid.ItemsChange(Sender: TObject);
var
  I: Integer;
begin
  SetLength(FSelection, FItems.Count);
  for I := Low(FSelection) to High(FSelection) do
    FSelection[I] := False;
  FItemIndex := -1;
  Invalidate;
end;

function TRadioGrid.ItemRect(Index: Integer): TRect;
begin
  SetRectEmpty(Result);
  if (Index > -1) and (Index < FItems.Count) then
  begin
    Result := ClientRect;
    case FLayout of
      glHorz:
        begin
          FCaptionBottom := -1;
          Result.Left := FIndent + FSpacing * Index;
          Result.Right := Result.Left + FSpacing;
        end;
      glVert:
        begin
          CalculateCaptionBottom;
          Result.Left := FIndent;
          Result.Top := FCaptionBottom + FSpacing * Index;
          Result.Bottom := Result.Top + FSpacing;
        end;
    end;
  end;
end;

procedure TRadioGrid.KeyPress(var Key: Char);
var
  KeyIndex: Integer;
begin
  inherited KeyPress(Key);
  KeyIndex :=  Ord(Key) - Ord('0') - 1;
  if (KeyIndex > -2) and (KeyIndex < FItems.Count) then
    if MultiSelect then
      if KeyIndex > -1 then
        Selected[KeyIndex] := not Selected[KeyIndex]
      else
    else
      ItemIndex := KeyIndex;
end;

procedure TRadioGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Point: TPoint;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetFocus;
    if ssDouble in Shift then
      DblClick;
    Point.X := X;
    Point.Y := Y;
    if FReadOnly then Exit;
    for I := 0 to FItems.Count - 1 do
      if PtInRect(ItemRect(I), Point) then
      begin
        if FMultiSelect then
          FSelection[I] := not FSelection[I]
        else if I = FItemIndex then
          ItemIndex := -1
        else
          ItemIndex := I;
        Invalidate;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRadioGrid.Paint;
const
  CircleRadius = 7;
var
  Rect: TRect;
  CircleRect: TRect;
  I: Integer;
begin
  Rect := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);
  Canvas.Pen.Color := clWindowText;
  if FLayout = glHorz then
  begin
    InflateRect(Rect, -2, 0);
    DrawText(Canvas.Handle, PChar(Caption), -1, Rect, DT_LEFT or DT_VCENTER or
      DT_SINGLELINE or DT_NOCLIP);
    InflateRect(Rect, 2, 0);
    Rect.Top := Rect.Bottom div 2 - Canvas.TextHeight('Wg') div 2 - 2;
    Rect.Bottom := Rect.Top + Canvas.TextHeight('Wg') + 4;
    Rect.Right := Canvas.TextWidth(Caption) + 4;
    if (Caption <> '') and Focused then
      Canvas.DrawFocusRect(Rect);
    Rect := ItemRect(0);
    CircleRect := Rect;
    with CircleRect do
    begin
      Left := Left + (Right - Left) div 2 - CircleRadius;
      Right := Left + CircleRadius * 2 + 1;
      Top := Top + (Bottom - Top) div 2 - CircleRadius;
      Bottom := Top + CircleRadius * 2 + 1;
    end;
    for I := 0 to FItems.Count - 1 do
    begin
      if FDrawItems then
        DrawText(Canvas.Handle, PChar(FItems[I]), -1, Rect, DT_CENTER or DT_TOP or
          DT_SINGLELINE or DT_NOCLIP);
      Canvas.Ellipse(CircleRect);
      if (I = FItemIndex) or (FMultiSelect and FSelection[I]) then
      begin
        InflateRect(CircleRect, -4, -4);
        Canvas.Brush.Color := clWindowText;
        Canvas.Ellipse(CircleRect);
        Canvas.Brush.Color := Color;
        InflateRect(CircleRect, 4, 4);
      end;
      OffsetRect(Rect, FSpacing, 0);
      OffsetRect(CircleRect, FSpacing, 0);
    end;
  end
  else
  begin
    CalculateCaptionBottom;
    InflateRect(Rect, -2, -2);
    DrawText(Canvas.Handle, PChar(Caption), -1, Rect, DT_LEFT or DT_TOP or
      DT_WORDBREAK);
    InflateRect(Rect, 2, 2);
    Rect.Bottom := FCaptionBottom - 2;
    if (Caption <> '') and Focused then
      Canvas.DrawFocusRect(Rect);
    Rect := ItemRect(0);
    CircleRect := Rect;
    with CircleRect do
    begin
      Right := Left + CircleRadius * 2 + 1;
      Top := Top + (Bottom - Top) div 2 - CircleRadius;
      Bottom := Top + CircleRadius * 2 + 1;
    end;
    Rect.Left := CircleRect.Right + 8;
    for I := 0 to FItems.Count - 1 do
    begin
      if FDrawItems then
        DrawText(Canvas.Handle, PChar(FItems[I]), -1, Rect, DT_LEFT or DT_VCENTER or
          DT_SINGLELINE or DT_NOCLIP);
      Canvas.Ellipse(CircleRect);
      if (I = FItemIndex) or (FMultiSelect and FSelection[I]) then
      begin
        InflateRect(CircleRect, -4, -4);
        Canvas.Brush.Color := clWindowText;
        Canvas.Ellipse(CircleRect);
        Canvas.Brush.Color := Color;
        InflateRect(CircleRect, 4, 4);
      end;
      OffsetRect(Rect, 0, FSpacing);
      OffsetRect(CircleRect, 0, FSpacing);
    end;
    FDrawBottom := Rect.Bottom;
  end;
end;

procedure TRadioGrid.SetIndent(Value: Integer);
begin
  if Value <> FIndent then
  begin
    if Value < 0 then
      Value := 0
    else if Value > 1000 then
      Value := 1000;
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TRadioGrid.SetItemIndex(Value: Integer);
begin
  if Value <> FitemIndex then
  begin
    if Value < -1 then
      Value := -1
    else if Value > FItems.Count - 1 then
      Value := FItems.Count - 1;
    FitemIndex := Value;
    Invalidate;
  end;
end;

procedure TRadioGrid.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TRadioGrid.SetLayout(Value: TRadioGridLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    FCaptionBottom := -1;
    Invalidate;
  end;
end;

function TRadioGrid.GetSelected(Index: Integer): Boolean;
begin
  Result := FSelection[Index];
end;

procedure TRadioGrid.SetSelected(Index: Integer; Value: Boolean);
begin
  FSelection[Index] := Value;
  if FMultiSelect then
    Invalidate;
end;

procedure TRadioGrid.SetSpacing(Value: Integer);
begin
  if Value <> FIndent then
  begin
    if Value < 0 then
      Value := 0
    else if Value > 1000 then
      Value := 1000;
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TRadioGrid.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  Invalidate;
end;

procedure TRadioGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FCaptionBottom := -1;
  Invalidate;
end;

procedure TRadioGrid.CMTextChanged(var Message: TMessage);
begin
  inherited;
  FCaptionBottom := -1;
  Invalidate;
end;

procedure TRadioGrid.WMWindowPosChanged(var Message: TMessage);
begin
  inherited;
  FCaptionBottom := -1;
  Invalidate;
end;

end.
