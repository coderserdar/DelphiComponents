//---------------------------------------------------------------------------
//  TVolgaPeriod - inherited from TWinControl
//  Component for changing periods(ranges) od date
//  Turn over months, quarters, halfyears, years
//  properties StartDate, EndDate, Text, KindRange, Year, Month
//  Autopopup menu for change Kind of range
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
unit VolPeriod;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Buttons, ExtCtrls, StdCtrls, ComCtrls, menus, VolDBConst;

type
  TKindRange = (ksMonth, ksQuarter, ksHalfYear, ksYear);
  TVolgaPeriod = class(TWinControl)
  private
    { Private declarations }
    FUpDownButton: TUpDown;
    FButtonWidth: byte;
    FColor: TColor;
    FPanel: TPanel;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FEnablePopup: Boolean;
    FKindRange: TKindRange;
    FPopup: TPopupMenu;
    FmnuItems: array[0..3] of TMenuItem;
    FDay, FMonth, FYear: word;
    FOnChange: TNotifyEvent;
    FBeforeChange: TNotifyEvent;
    function GetText: string;
    procedure SetButtonWidth(Value: byte);
    function CreateButton: TUpDown;
    procedure SetColor(Value: TColor);
    procedure SetYear(Value: word);
    procedure SetMonth(Value: word);
    procedure SetKindRange(Value: TKindRange);
    procedure SetEnablePopup(Value: Boolean);
    procedure UpDownButtonClick(Sender: TObject; Button: TUDBtnType);
    procedure MonthPopup(Sender: TObject);
    procedure SetStartDate(const Value: TDateTime);
  protected
    { Protected declarations }
    procedure Change; dynamic;
    procedure BeforeChanged; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property EndDate: TDateTime read FEndDate;
    property Text: string read GetText;
  published
    { Published declarations }
    property KindRange: TKindRange read FKindRange write SetKindRange;
    property Year: word read FYear write SetYear;
    property Month: word read FMonth write SetMonth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property ButtonWidth: byte read FButtonWidth write SetButtonWidth;
    property Color: TColor read FColor write SetColor default clWindow;
    property Enabled;
    property EnablePopup: Boolean read FEnablePopup write SetEnablePopup;
    property Font;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property BeforeChange: TNotifyEvent read FBeforeChange write FBeforeChange;
  end;

implementation

constructor TVolgaPeriod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  if FPanel = nil then
  begin
    FPanel := TPanel.Create(Self);        {owner -> TVolgaPeriod}
    FPanel.Parent := Self;                {parent -> тот же,какой у TVolgaPeriod}
    FPanel.BorderStyle := bsNone;
    FPanel.BevelInner := bvLowered;
{  FPanel.BevelWidth:=2;}
    FPanel.BevelOuter := bvNone;
    FPanel.Color := clWindow;
    DecodeDate(Date, FYear, FMonth, FDay);
    FPanel.Visible := true;
    FPanel.Invalidate;
    FUpDownButton := CreateButton;
    FButtonWidth := 17;
    FColor := clWindow;
    Width := 200;
    Height := 26;
  {конструируем Popup}
    FmnuItems[0] := NewItem(V_KINDMON, 0, false, true, MonthPopup, 0, 'mnuMonth');
    FmnuItems[0].GroupIndex := 1;
    FmnuItems[0].RadioItem := true;
    FmnuItems[0].Checked := true;
    FmnuItems[1] := NewItem(V_KINDQUART, 0, false, true, MonthPopup, 0, 'mnuQuarter');
    FmnuItems[1].GroupIndex := 1;
    FmnuItems[1].RadioItem := true;
    FmnuItems[2] := NewItem(V_KINDHALF, 0, false, true, MonthPopup, 0, 'mnuHalfYear');
    FmnuItems[2].GroupIndex := 1;
    FmnuItems[2].RadioItem := true;
    FmnuItems[3] := NewItem(V_KINDYEAR, 0, false, true, MonthPopup, 0, 'mnuYear');
    FmnuItems[3].GroupIndex := 1;
    FmnuItems[3].RadioItem := true;
    FPopup := NewPopupMenu(Self, 'MonPopup', paLeft, true, FmnuItems);
    FEnablePopup := true;
    FPanel.PopupMenu := FPopup;
    FKindRange := ksMonth;
  end;
  Visible := true;
  Change;
end;

destructor TVolgaPeriod.Destroy;
begin
  try
    FPanel.PopupMenu := nil;
    FmnuItems[0].Free;
    FmnuItems[1].Free;
    FmnuItems[2].Free;
    FmnuItems[3].Free;
    FPopup.Free;
    FPopup := nil;
  finally
    inherited Destroy;
  end;
end;

function TVolgaPeriod.CreateButton: TUpDown;
begin
  Result := TUpDown.Create(Self);
  Result.OnClick := UpDownButtonClick;
  Result.Visible := True;
  Result.Enabled := True;
  Result.Parent := Self;
  Result.Min := -200;
  Result.Max := 200;
  Result.Position := 0;
end;

procedure TVolgaPeriod.MonthPopup(Sender: TObject);
var i: integer;
begin
  {снять "галочку" со всех строк}
  for i := 0 to 3 do
    FmnuItems[i].Checked := false;
  {пометить только кликнутую}
  TMenuItem(Sender).Checked := true;
  if Sender = FmnuItems[0] then
    KindRange := ksMonth
  else if Sender = FmnuItems[1] then
    KindRange := ksQuarter
  else if Sender = FmnuItems[2] then
    KindRange := ksHalfYear
  else
    KindRange := ksYear;
end;

procedure TVolgaPeriod.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if W < 20 then W := 20;
  if H < 18 then H := 18;
  if (FUpDownButton <> nil) and (FPanel <> nil) then
  begin
    FPanel.SetBounds(0, 0, W - FButtonWidth, AHeight);
    FUpDownButton.SetBounds(W - FButtonWidth, 0, FButtonWidth, H);
  end;
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TVolgaPeriod.SetEnablePopup(Value: Boolean);
begin
  if FEnablePopup <> Value then
  begin
    FEnablePopup := Value;
    FPopup.AutoPopup := Value;
  end;
end;

procedure TVolgaPeriod.SetKindRange(Value: TKindRange);
var i: integer;
begin
  if FKindRange <> Value then
  begin
    BeforeChanged;                        {до всех изменений}
    FKindRange := Value;
    {снять "галочку" со всех строк}
    for i := 0 to 3 do
      FmnuItems[i].Checked := false;
    {пометить только кликнутую}
    FmnuItems[ord(FKindRange)].Checked := true;
    Change;
  end;
end;

procedure TVolgaPeriod.SetYear(Value: word);
begin
  if FYear <> Value then
  begin
    BeforeChanged;                        {до всех изменений}
    FYear := Value;
    Change;
  end;
end;

procedure TVolgaPeriod.SetMonth(Value: word);
begin
  if (Value >= 1) and (Value <= 12) then
    if (FMonth <> Value) or (FStartDate <> EncodeDate(FYear, FMonth, 1)) then
    begin
      BeforeChanged;                      {до всех изменений}
      FMonth := Value;
      Change;
    end;
end;

procedure TVolgaPeriod.Change;
var yy, mm, dd: word;
begin
  case FKindRange of
    ksMonth:
      begin
        FPanel.Caption := LongMonthNames[FMonth] + ' ' + IntToStr(FYear) + V_SHORTYEAR;
        FEndDate := EncodeDate(FYear, FMonth, 28) + 4;
      end;
    ksQuarter:
      begin
        case FMonth of
          1..3:
            begin
              FMonth := 1;
              FPanel.Caption := Format(V_FRMTQUART, [1, FYear]);
            end;
          4..6:
            begin
              FMonth := 4;
              FPanel.Caption := Format(V_FRMTQUART, [2, FYear]);
            end;
          7..9:
            begin
              FMonth := 7;
              FPanel.Caption := Format(V_FRMTQUART, [3, FYear]);
            end;
          10..12:
            begin
              FMonth := 10;
              FPanel.Caption := Format(V_FRMTQUART, [4, FYear]);
            end;
        end;
        FEndDate := EncodeDate(FYear, FMonth + 2, 28) + 4;
      end;
    ksHalfYear:
      begin
        case FMonth of
          1..6:
            begin
              FMonth := 1;
              FPanel.Caption := Format(V_FRMTHALFYEAR, [1,FYear]);
            end;
          7..12:
            begin
              FMonth := 7;
              FPanel.Caption := Format(V_FRMTHALFYEAR, [2,FYear]);
            end;
        end;
        FEndDate := EncodeDate(FYear, FMonth + 5, 28) + 4;
      end;
    ksYear:
      begin
        FMonth := 1;
        FPanel.Caption := IntToStr(FYear) + V_LONGYEAR;
        FEndDate := EncodeDate(FYear, 12, 31) + 1;
      end;
  end;
  DecodeDate(FEndDate, yy, mm, dd);
  FEndDate := FEndDate - dd;
  FStartDate := EncodeDate(FYear, FMonth, 1);
  if assigned(FOnChange) then FOnChange(Self);
end;

procedure TVolgaPeriod.UpDownButtonClick(Sender: TObject; Button: TUDBtnType);
begin
  BeforeChanged;                          {до всех изменений}
  if Button = btNext then
  begin
    case FKindRange of
      ksMonth:
        if FMonth < 12 then
          Inc(FMonth)
        else
        begin
          FMonth := 1;
          Inc(FYear);
        end;
      ksQuarter:
        case FMonth of
          1..3: FMonth := 4;
          4..6: FMonth := 7;
          7..9: FMonth := 10;
          10..12:
            begin
              FMonth := 1;
              Inc(FYear);
            end;
        end;
      ksHalfYear:
        case FMonth of
          1..6: FMonth := 7;
          7..12:
            begin
              FMonth := 1;
              Inc(FYear);
            end;
        end;
      ksYear: Inc(FYear);
    end;
  end
  else
  begin
    case FKindRange of
      ksMonth:
        if FMonth > 1 then
          Dec(FMonth)
        else
        begin
          FMonth := 12;
          Dec(FYear);
        end;
      ksQuarter:
        case FMonth of
          1..3:
            begin
              FMonth := 10;
              Dec(FYear);
            end;
          4..6: FMonth := 1;
          7..9: FMonth := 4;
          10..12: FMonth := 7;
        end;
      ksHalfYear:
        case FMonth of
          1..6:
            begin
              FMonth := 7;
              Dec(FYear);
            end;
          7..12: FMonth := 1;
        end;
      ksYear: Dec(FYear);
    end;
  end;
  Change;                                 {после всех изменений}
end;

procedure TVolgaPeriod.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FPanel.Color := FColor;
  end;
end;

function TVolgaPeriod.GetText: string;
begin
  Result := FPanel.Caption;
end;

procedure TVolgaPeriod.SetButtonWidth(Value: byte);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TVolgaPeriod.SetStartDate(const Value: TDateTime);
var god, mes, den: word;
begin
  BeforeChanged;                          {до всех изменений}
  DecodeDate(Value, god, mes, den);
  FYear := god;
  Month := mes;                           {вызывается SetMonth}
end;

procedure TVolgaPeriod.BeforeChanged;
begin
  if assigned(FBeforeChange) then FBeforeChange(Self);
end;

end.

