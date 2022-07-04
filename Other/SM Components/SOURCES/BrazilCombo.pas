{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit BrazilCombo;

interface

uses
  Classes, Messages, Windows, Controls, StdCtrls;

type
  { TBrazilStateCombo }
  TBrStates = (brAC, brAL, brAM, brAP, brBA, brCE, brDF, brES,
               brGO, brMA, brMG, brMS, brMT, brPA, brPB, brPE,
               brPI, brPR, brRJ, brRO, brRN, brRR, brRS, brSC,
               brSE, brSP, brTO);

  TBrazilStateCombo = class(TCustomComboBox)
  private
    { Private declarations }
    FStateValue: TBrStates;
    FStateNames: TStrings;

    FOnChange: TNotifyEvent;
    procedure SetStateValue(NewValue: TBrStates);
    procedure SetStateNames(Value: TStrings);
    procedure StateNamesChanged(Sender: TObject);
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure BuildList; virtual;
    procedure DoChange; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text;
  published
    { Published declarations }
    property StateValue: TBrStates read FStateValue write SetStateValue;
    property StateNames: TStrings read FStateNames write SetStateNames;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
{$R *.RES}
uses Graphics, SysUtils, TypInfo;

procedure Register;
begin
  RegisterComponents('SMComponents', [TBrazilStateCombo]);
end;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 1;
end;

{ TBrazilStateCombo }
constructor TBrazilStateCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Style := csOwnerDrawFixed;
  FStateValue := brAC;
  FStateNames := TStringList.Create;
  TStringList(FStateNames).OnChange := StateNamesChanged;
end;

destructor TBrazilStateCombo.Destroy;
begin
  TStringList(FStateNames).OnChange := nil;
  FStateNames.Free;
  FStateNames := nil;

  inherited Destroy;
end;

procedure TBrazilStateCombo.CreateWnd;
begin
  inherited CreateWnd;

  BuildList;
  SetStateValue(FStateValue);
end;

procedure TBrazilStateCombo.BuildList;
var i: Integer;
    StateName: string;
begin
  Clear;
  for i := Ord(Low(TBrStates)) to Ord(High(TBrStates)) do
  begin
    if (i <= Pred(FStateNames.Count)) and (FStateNames[i] <> '') then
      StateName := FStateNames[i]
    else
      { delete two first characters which prefix "cl" educated }
      StateName := Copy(GetEnumName(TypeInfo(TBrStates), i), 3, MaxInt);
    Items.AddObject(StateName, TObject(i));
  end;
end;

procedure TBrazilStateCombo.StateNamesChanged(Sender: TObject);
begin
  if HandleAllocated then
  begin
    FStateValue := StateValue;
    RecreateWnd;
  end;
end;

procedure TBrazilStateCombo.SetStateNames(Value: TStrings);
begin
  FStateNames.Assign(Value);
end;

procedure TBrazilStateCombo.SetStateValue(NewValue: TBrStates);
var i: Integer;
    CurrentState: TBrStates;
begin
  if (ItemIndex < 0) or (NewValue <> FStateValue) then
    { change selected item }
    for i := 0 to Pred(Items.Count) do
    begin
      CurrentState := TBrStates(Items.Objects[i]);
      if CurrentState = NewValue then
      begin
        FStateValue := NewValue;
        if ItemIndex <> i then
          ItemIndex := i;
        DoChange;
        Break;
      end;
    end;
end;

procedure TBrazilStateCombo.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  FlagWidth = 22;
var strText: string;
    bmp: TBitmap;
begin
  Canvas.FillRect(Rect);
  Canvas.Brush.Style := bsClear;
  strText := Items[Index];
  if Index > -1 then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Handle := LoadBitmap(hInstance, PChar(UpperCase(GetEnumName(TypeInfo(TBrStates), Index))));
      if bmp <> nil then
        Canvas.StretchDraw(Bounds(Rect.Left + 2, Rect.Top, bmp.Width, bmp.Height), bmp);
    finally
      bmp.Free
    end
  end;
  Canvas.TextOut(Rect.Left + 36, Rect.Top, strText)
end;

procedure TBrazilStateCombo.Click;
begin
  if ItemIndex >= 0 then
    StateValue := TBrStates(Items.Objects[ItemIndex]);

  inherited Click;
end;

procedure TBrazilStateCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;

  ResetItemHeight;
  RecreateWnd;
end;

procedure TBrazilStateCombo.ResetItemHeight;
var i: Integer;
begin
  i := GetItemHeight(Font);
  if i > 9 then
    ItemHeight := i
  else
    ItemHeight := 9;
end;

procedure TBrazilStateCombo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
