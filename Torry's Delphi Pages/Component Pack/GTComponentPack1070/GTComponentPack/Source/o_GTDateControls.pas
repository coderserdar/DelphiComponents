{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       GT Date Controls                                }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTDateControls;

interface
uses
   Classes
  ,Controls
  ,StdCtrls
  ;
{$I ../../GTDIRECTIVES.inc}
type
{------------------------------------------------------------------------------}
  TgtMonthComboNameType =
                         (
                           mctLong
                          ,mctShort
                          );
{------------------------------------------------------------------------------}
  TgtMonthCombo = class(TCustomComboBox)
  private
    FMonth: Integer;
    FNameType: TgtMonthComboNameType;
    FOnValueChange: TNotifyEvent;
    procedure SetMonth(const Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyPress(var Key : Char);override;
    procedure Initialize;
    procedure InternalOnSelect(Sender : TObject);
    procedure SetParent(AParent : TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure Refresh;
  published
    { Published declarations}
    property Month    : Integer               read FMonth         write SetMonth;
    property NameType : TgtMonthComboNameType read FNameType      write FNameType;
    property OnValueChange : TNotifyEvent     read FOnValueChange write FOnValueChange;
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    {$IFNDEF DELPHI6}
      property AutoCloseUp default False;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
{------------------------------------------------------------------------------}
  TgtDayCombo = class(TCustomComboBox)
  private
    FDay: Integer;
    FOnValueChange: TNotifyEvent;
    procedure SetDay(const Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyPress(var Key : Char);override;
    procedure Initialize;
    procedure InternalOnSelect(Sender : TObject);
    procedure SetParent(AParent : TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   Refresh;
  published
    { Published declarations}
    property Day : Integer read FDay write SetDay;
    property OnValueChange : TNotifyEvent     read FOnValueChange write FOnValueChange;
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    {$IFNDEF DELPHI6}
      property AutoCloseUp default False;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
{------------------------------------------------------------------------------}
  TgtYearCombo = class(TCustomComboBox)
  private
    FYearStart: Integer;
    FYearEnd: Integer;
    FYear: Integer;
    FOnValueChange: TNotifyEvent;
    procedure SetYear(const Value: Integer);
    procedure SetYearStart(const Value: Integer);
    procedure SetYearEnd(const Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyPress(var Key : Char);override;
    procedure Initialize;
    procedure InternalOnSelect(Sender : TObject);
    procedure SetParent(AParent : TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   Refresh;
  published
    { Published declarations}
    property YearStart : Integer read FYearStart write SetYearStart;
    property YearEnd   : Integer read FYearEnd   write SetYearEnd;
    property Year      : Integer read FYear      write SetYear;
    property OnValueChange : TNotifyEvent     read FOnValueChange write FOnValueChange;
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    {$IFNDEF DELPHI6}
      property AutoCloseUp default False;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;  
  end;
{------------------------------------------------------------------------------}
  TgtDateCtrlManager = class(TComponent)
  private
    FDayCombo  : TgtDayCombo;
    FMonthCombo: TgtMonthCombo;
    FYearCombo : TgtYearCombo;
    FDate      : string;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDayCombo(const Value: TgtDayCombo);
    procedure SetMonthCombo(const Value: TgtMonthCombo);
    procedure SetYearCombo(const Value: TgtYearCombo);
    function GetDateTime: TDateTime;
    procedure SetDate(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    FYear   : Word;
    FMonth  : Word;
    FDay    : Word;
    FHour   : Word;
    FMinute : Word;
    FSecond : Word;
    FMiliSec: Word;
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
    function ControlsAssigned:Boolean;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    property    DateTime   : TDateTime     read GetDateTime  write SetDateTime;
  published
    { Published declarations}
    property DayCombo   : TgtDayCombo   read FDayCombo    write SetDayCombo;
    property MonthCombo : TgtMonthCombo read FMonthCombo  write SetMonthCombo;
    property YearCombo  : TgtYearCombo  read FYearCombo   write SetYearCombo;
    property Date       : string        read FDate        write SetDate;
  end;






implementation

uses
   SysUtils
  ,DateUtils
  ;

const
  ERR_NOT_ALL_CONTROLS_ARE_SET =
  'Error not all date controls are assigned!';


{ TgtMonthCombo }
{------------------------------------------------------------------------------}
constructor TgtMonthCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtMonthCombo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.Initialize;
var
  i : Integer;
begin
  Items.Clear;
  case NameType of
    mctLong  :
      begin
        for i:= 1 to 12 do
        begin
          Items.Add(SysUtils.LongMonthNames[i]);
        end;
      end;
    mctShort :
        for i:= 1 to 12 do
        begin
          Items.Add(SysUtils.ShortMonthNames[i]);
        end;
 end;
end;
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.InternalOnSelect(Sender: TObject);
begin
  if ItemIndex <> -1 then
  begin
    FMonth := ItemIndex + 1;
    if Assigned(FOnValueChange) then
      FOnValueChange(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  Key := #0;
end;
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.Refresh;
begin
  Initialize;
end;
{------------------------------------------------------------------------------}


//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.SetMonth(const Value: Integer);
begin
  if (Value > 0) and (Value <= 12) then
  begin
    FMonth    := Value;
    ItemIndex := FMonth - 1;
  end
  else
    ItemIndex := -1;
end;
{------------------------------------------------------------------------------}
procedure TgtMonthCombo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(Parent) then
  begin
    Initialize;
    ItemIndex     := -1;
    DropDownCount := 12;
    FMonth        := ItemIndex;
    FNameType     := mctLong;
    Text          := '';
  end;
end;
{------------------------------------------------------------------------------}

{ TgtDayCombo }
{------------------------------------------------------------------------------}
constructor TgtDayCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtDayCombo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDayCombo.Initialize;
var
  i :Integer;
begin
  Items.Clear;
  for i:=1 to 31 do
    Items.Add(Format('%.2d',[i]));
end;
{------------------------------------------------------------------------------}
procedure TgtDayCombo.InternalOnSelect(Sender: TObject);
begin
  if ItemIndex <> -1 then
  begin
    FDay := ItemIndex + 1;
    if Assigned(FOnValueChange) then
      FOnValueChange(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDayCombo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  Key := #0;
end;
{------------------------------------------------------------------------------}
procedure TgtDayCombo.Refresh;
begin
  Initialize;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDayCombo.SetDay(const Value: Integer);
begin
  if (Value > 0) and (Value <=31) then
  begin
    FDay      := Value;
    ItemIndex := FDay - 1;
  end
  else
    ItemIndex := -1;
end;
{------------------------------------------------------------------------------}
procedure TgtDayCombo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(Parent) then
  begin
    Initialize;
    ItemIndex     := -1;
    Text          := '';
  end;
end;
{------------------------------------------------------------------------------}


{ TgtYearCombo }
{------------------------------------------------------------------------------}
constructor TgtYearCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FYearStart := YearOf(Now);
  FYearEnd   := FYearStart + 50;
end;
{------------------------------------------------------------------------------}
destructor TgtYearCombo.Destroy;
begin

  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.Initialize;
var
  i : Integer;
begin
  Items.Clear;
  for i:= YearStart to YearEnd do
  begin
    Items.Add(IntToStr(i))
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.InternalOnSelect(Sender: TObject);
begin
  if ItemIndex <> -1 then
  begin
    FYear := StrToInt(Items[ItemIndex]);
    if Assigned(FOnValueChange) then
      FOnValueChange(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  Key := #0;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.Refresh;
begin
  Initialize;
end;
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
procedure TgtYearCombo.SetYear(const Value: Integer);
var
  Idx : Integer;
begin
  Idx := -1;
  if (Value >= FYearStart) and (Value <= FYearEnd) then
  begin
    FYear     := Value;
    Idx       := Items.IndexOf(IntToStr(FYear));
  end;
  ItemIndex := Idx;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.SetYearStart(const Value: Integer);
begin
  if Value > 0 then
  begin
    if FYearStart <> Value then
    begin
      FYearStart := Value;
      Initialize;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.SetYearEnd(const Value: Integer);
begin
  if Value > 0 then
  begin
    if FYearEnd <> Value then
    begin
      FYearEnd := Value;
      Initialize;
    end;
  end;  
end;
{------------------------------------------------------------------------------}
procedure TgtYearCombo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(Parent) then
  begin
    Initialize;
    ItemIndex     := -1;
    Text          := '';
  end;
end;
{------------------------------------------------------------------------------}









{ TgtDateCtrlManager }
{------------------------------------------------------------------------------}
constructor TgtDateCtrlManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDate  := DateToStr(Now);
end;
{------------------------------------------------------------------------------}
destructor TgtDateCtrlManager.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FYearCombo  then YearCombo  := nil;
    if AComponent = FMonthCombo then MonthCombo := nil;
    if AComponent = FDayCombo   then DayCombo   := nil;
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
function TgtDateCtrlManager.ControlsAssigned: Boolean;
begin
  Result := Assigned(FYearCombo) and Assigned(FMonthCombo) and Assigned(FDayCombo);
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.SetDate(const Value: string);
var
  TempDT: TDateTime;
begin
  FDate    := Value;
  if  TryStrToDate(Value,TempDT) then
    DateTime := TempDT
  else
    DateTime := Now;
end;
{------------------------------------------------------------------------------}
function TgtDateCtrlManager.GetDateTime: TDateTime;
begin
  Result := 0;
  if ControlsAssigned then
  begin
   if (FYearCombo.Year > 0) and (FMonthCombo.Month > 0) and (FDayCombo.Day > 0) then
     Result := EncodeDateTime(FYearCombo.Year,FMonthCombo.Month,FDayCombo.Day,0,0,0,0);
  end;
  {else
    raise Exception.Create(ERR_NOT_ALL_CONTROLS_ARE_SET);}
end;
{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.SetDateTime(const Value: TDateTime);
begin
  if ControlsAssigned then
  begin
    if Value <> 0 then
    begin
      DecodeDateTime(Value,FYear,FMonth,FDay,FHour,FMinute,FSecond,FMiliSec);
      FYearCombo.Year   := FYear;
      FMonthCombo.Month := FMonth;
      FDayCombo.Day     := FDay;
    end;
  end;
  {else
    raise Exception.Create(ERR_NOT_ALL_CONTROLS_ARE_SET);}
end;
{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.SetDayCombo(const Value: TgtDayCombo);
begin
  if Assigned(FDayCombo) then
    FDayCombo.RemoveFreeNotification(Self);

  FDayCombo := Value;

  if Assigned(FDayCombo) then
    FDayCombo.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.SetMonthCombo(const Value: TgtMonthCombo);
begin
  if Assigned(FMonthCombo) then
    FMonthCombo.RemoveFreeNotification(Self);

  FMonthCombo := Value;

  if Assigned(FMonthCombo) then
    FMonthCombo.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtDateCtrlManager.SetYearCombo(const Value: TgtYearCombo);
begin
  if Assigned(FYearCombo) then
    FYearCombo.RemoveFreeNotification(Self);

  FYearCombo := Value;

  if Assigned(FYearCombo) then
    FYearCombo.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}





end.

