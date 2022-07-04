unit CHDateDialog;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Graphics, __SRCal, types,
  Forms, Buttons, Messages;

type
  TPosition = (psUser, psFormCenter, psWinControl);

  TCHDateDialog = class;

  TCalendarColors = class(TPersistent)
  private
    FHeaders,
    FHoliday,
    FMarked,
    FSelected,
    FStandard,
    FToday,
    FWeekend   : TColor;
  published
    property Headers: TColor read FHeaders write FHeaders;
    property Holiday: TColor read FHoliday write FHoliday;
    property Marked: TColor read FMarked write FMarked;
    property Selected: TColor read FSelected write FSelected;
    property Standard: TColor read FStandard write FStandard;
    property Today: TColor read FToday write FToday;
    property Weekend: TColor read FWeekend write FWeekend;
  end;

  TCalendar = class(TPersistent)
  private
    FOwner : TCHDateDialog;
    FLongitude: single;
    FLatitude: single;
    FBackgroundColors: TCalendarColors;
    FDrawStyle: TCalendarDrawStyle;
    FCalendarOptions: TCalendarOptions;
    FGermanState: TGermanState;
    FDateElement: Integer;
    FSaturdayAsSunday: boolean;
    FTextColors: TCalendarColors;
    FStartOfWeek: TDayOfWeek;
    function GetDateElement(const Index: Integer): Integer;

  protected

  public
    constructor Create(AOwner : TCHDateDialog); virtual;
  published
    property BackgroundColors: TCalendarColors read FBackgroundColors write FBackgroundColors;
    property GermanState: TGermanState read FGermanState write FGermanState;
    property CalendarOptions: TCalendarOptions read FCalendarOptions write FCalendarOptions;
    property Day: Integer index 3 read GetDateElement write FDateElement stored False;
    property DrawStyle: TCalendarDrawStyle read FDrawStyle write FDrawStyle;
    property Latitude: single read FLatitude write FLatitude;
    property Longitude: single read FLongitude write FLongitude;
    property Month: Integer index 2 read GetDateElement write FDateElement stored False;
    property SaturdayAsSunday: boolean read FSaturdayAsSunday write FSaturdayAsSunday;
    property StartOfWeek: TDayOfWeek read FStartOfWeek write FStartOfWeek;
    property TextColors: TCalendarColors read FTextColors write FTextColors;
    property Year: Integer index 1  read GetDateElement write FDateElement stored False;
  end;


  TCHDateDialog = class(TComponent)
  private
    FForm : TCustomForm;
    FDatePanel : TPanel;
    FBtnMonthNext : TSpeedButton;
    FBtnMonthLast : TSpeedButton;
    FBtnYearNext : TSpeedButton;
    FBtnYearLast : TSpeedButton;
    FMonth : TLabel;
    FYear : TLabel;
    FPosition: TPosition;
    FNavColor: TColor;
    FPastDate: Boolean;
    FFutureDate: Boolean;
    FMonthColor: TColor;
    FYearColor: TColor;
    FTitel: string;
    FInit : Boolean;
    FUserTop: Integer;
    FUserLeft: Integer;
    FEditControl : TCustomEdit;
    FFormat: string;
    FBorder: Boolean;
    FCalendarTmp: TCalendar;
    FDate : TDateTime;
    FOnClick: TNotifyEvent;
    FOnAfterChange: TNotifyEvent;
    FOnBeforeChange: TNotifyEvent;
    FOnKeyDown: TNotifyEvent;
    FOnKeyPress: TNotifyEvent;
    FOnYearChange: TNotifyEvent;
    FOnKeyUp: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnMouseDown: TNotifyEvent;
    FOnMonthChange: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseUp: TNotifyEvent;
    FOnMouseMove: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;

    procedure DoOnAfterChange(Sender: TObject);
    procedure DoOnBeforeChange(Sender: TObject);
    procedure DoOnClick(Sender: TObject);
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnKeyPress(Sender: TObject; var Key : Char);
    procedure DoOnYearChange(Sender: TObject);
    procedure DoOnEnter(Sender: TObject);
    procedure DoOnExit(Sender: TObject);
    procedure DoOnMonthChange(Sender: TObject);
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
    procedure DoOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
    procedure DoOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoOnClose(Sender: TObject);
    procedure DoOnCreate(Sender: TObject);

    procedure SetPos;
    procedure SetUserLeft(const Value: Integer);
    procedure SetUserTop(const Value: Integer);
    procedure SetPosition(const Value: TPosition);
    procedure SetMonthColor(const Value: TColor);
    procedure SetYearColor(const Value: TColor);
    procedure MonthLastClick(Sender: TObject);
    procedure MonthNextClick(Sender: TObject);
    procedure YearLastClick(Sender: TObject);
    procedure YearNextClick(Sender: TObject);
    procedure DoCalDbl(Sender: TObject);
    procedure SetTitel(const Value: string);
    procedure SetBorder(const Value: Boolean);
    procedure GetLeftTop(WC : TWinControl; var LT : TPoint);
    //function GetFont: TFont;
//    procedure SetFont(const Value: TFont);
  protected

  public
    FWork : TForm;
    FCalendar: TSRCalendar;
    Height, Width : Integer;
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    procedure Execute; overload;
    procedure Execute(WC : TCustomEdit); overload;
    procedure SetDialogPos(WC : TCustomEdit);
  published
    property Position : TPosition read FPosition Write SetPosition;
    property Left : Integer read FUserLeft Write SetUserLeft;
    property Top : Integer read FUserTop Write SetUserTop;
    property NavColor : TColor read FNavColor Write FNavColor;
    property DateCanFuture : Boolean read FFutureDate Write FFutureDate;
    property DateCanPast : Boolean read FPastDate Write FPastDate;
    property NavMonthColor : TColor read FMonthColor Write SetMonthColor;
    property NavYearColor : TColor read FYearColor Write SetYearColor;
    property Titel : string read FTitel Write SetTitel;
    property Format : string read FFormat Write FFormat;
    property Border : Boolean read FBorder Write SetBorder;
    property Calendar : TCalendar read FCalendarTmp Write FCalendarTmp;
    //property Font : TFont read GetFont write SetFont;

    property OnBeforeChange: TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property OnAfterChange: TNotifyEvent read FOnAfterChange write FOnAfterChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TNotifyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TNotifyEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TNotifyEvent read FOnKeyUp write FOnKeyUp;
    property OnMonthChange: TNotifyEvent read FOnMonthChange write FOnMonthChange;
    property OnMouseDown: TNotifyEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TNotifyEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TNotifyEvent read FOnMouseUp write FOnMouseUp;
    property OnYearChange: TNotifyEvent read FOnYearChange write FOnYearChange;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
  end;

procedure Register;

implementation

uses CHEditBtn;

{$R CHCalender.res}


procedure Register;
begin
  RegisterComponents('CH Pack', [TCHDateDialog]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHDateDialog.Create(AOwner: TComponent);
begin
  inherited;
  FForm := GetParentForm(TControl(AOwner));
  FNavColor := clActiveCaption;
  FPosition := psUser;
  FUserLeft := 0;
  FUserTop := 0;
  Width := 202;
  Height := 145;
  FFutureDate := True;
  FPastDate := True;
  FMonthColor := clWhite;
  FYearColor := clWhite;
  FEditControl := nil;
  FFormat := 'dd.MM.yyyy';
  FBorder := True;
  FTitel := 'Select Date';
  FCalendarTmp := TCalendar.Create(Self);
  FDate := Now;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHDateDialog.Destroy;
begin
  FCalendarTmp.Free;
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.Execute(WC: TCustomEdit);
begin
  FInit := True;
  FEditControl := WC;
  if FWork = nil then
  begin
    FWork := TForm.Create(FForm);
    with FWork do
    begin
      Parent := FForm;
      if FBorder then
        BorderStyle := bsToolWindow
      else
        BorderStyle := bsNone;
      ClientWidth := 202;
      ClientHeight := 145;
      Caption := FTitel;
    end;


    FDatePanel := TPanel.Create(FWork);
    with FDatePanel do
    begin
      Parent := FWork;
      Left := 0;
      Top := 0;
      Width := 202;
      Height := 145;
      BevelOuter := bvSpace;
      BevelInner := bvSpace;
      Color := NavColor;
    end;

    FBtnMonthLast := TSpeedButton.Create(FDatePanel);
    with FBtnMonthLast do
    begin
      Parent := FDatePanel;
      Left := 5;
      Top := 4;
      Height := 18;
      Width := 18;
      Caption := '';
      Flat := True;
      Transparent := False;
      Glyph.Handle := LoadBitmap(hInstance, 'LAST');
      OnClick := MonthLastClick;
    end;

    FBtnMonthNext := TSpeedButton.Create(FDatePanel);
    with FBtnMonthNext do
    begin
      Parent := FDatePanel;
      Left := FBtnMonthLast.Left + FBtnMonthLast.Width +2;
      Top := 4;
      Height := 18;
      Width := 18;
      Caption := '';
      Flat := True;
      Transparent := False;
      Glyph.Handle := LoadBitmap(hInstance, 'NEXT');
      OnClick := MonthNextClick;
    end;

    FBtnYearNext := TSpeedButton.Create(FDatePanel);
    with FBtnYearNext do
    begin
      Parent := FDatePanel;
      Left := FDatePanel.Width - 24;
      Top := 4;
      Height := 18;
      Width := 18;
      Caption := '';
      Flat := True;
      Transparent := False;
      Glyph.Handle := LoadBitmap(hInstance, 'NEXT');
      OnClick := YearNextClick;
    end;

    FBtnYearLast := TSpeedButton.Create(FDatePanel);
    with FBtnYearLast do
    begin
      Parent := FDatePanel;
      Left := FBtnYearNext.Left - 20;
      Top := 4;
      Height := 18;
      Width := 18;
      Caption := '';
      Flat := True;
      Transparent := False;
      Glyph.Handle := LoadBitmap(hInstance, 'LAST');
      OnClick := YearLastClick;
    end;

    FCalendar := TSRCalendar.Create(FDatePanel);
    with FCalendar do
    begin
      Parent := FDatePanel;
      Width := 192;
      Height := 115;
      Left := 5;
      Top := 25;
      OnChange := DoOnAfterChange;
      OnDblClick := DoCalDbl;
      OnClick := DoOnClick;
      OnBeforeChange := DoOnBeforeChange;
      OnEnter := DoOnEnter;
      OnExit := DoOnExit;
      OnMonthChange := DoOnMonthChange;
      OnYearChange := DoOnYearChange;
      OnMouseDown := DoOnMouseDown;
      OnMouseUp := DoOnMouseUp;
      OnMouseMove := DoOnMouseMove;
      OnKeyPress := DoOnKeyPress;
      OnKeyUp := DoOnKeyUp;
      OnKeyDown := DoOnKeyDown;

      //Font.Name := Self.Font.Name;
//      Font.Size := Self.Font.Size;
      StartOfWeek := FCalendarTmp.StartOfWeek;
      BackgroundColors.Headers := FCalendarTmp.BackgroundColors.Headers;
      BackgroundColors.Holiday := FCalendarTmp.BackgroundColors.Holiday;
      BackgroundColors.Marked := FCalendarTmp.BackgroundColors.Marked;
      BackgroundColors.Selected := FCalendarTmp.BackgroundColors.Selected;
      BackgroundColors.Standard := FCalendarTmp.BackgroundColors.Standard;
      BackgroundColors.Today := FCalendarTmp.BackgroundColors.Today;
      BackgroundColors.Weekend := FCalendarTmp.BackgroundColors.Weekend;
      GermanState := FCalendarTmp.GermanState;
      CalendarOptions:= FCalendarTmp.CalendarOptions;
      if TCustomEdit(FEditControl).Text  <> '' then
      begin
        Date := StrToDateDef(TCustomEdit(FEditControl).Text, Date);
      end
      else
      begin
        Day:= FCalendarTmp.Day;
        Month:= FCalendarTmp.Month;
        Year:= FCalendarTmp.Year;
      end;
      DrawStyle:= FCalendarTmp.DrawStyle;
      Latitude:= FCalendarTmp.Latitude;
      Longitude:= FCalendarTmp.Longitude;
      SaturdayAsSunday:= FCalendarTmp.SaturdayAsSunday;
      StartOfWeek:= FCalendarTmp.StartOfWeek;
      TextColors.Headers:= FCalendarTmp.TextColors.Headers;
      TextColors.Holiday := FCalendarTmp.TextColors.Holiday;
      TextColors.Marked := FCalendarTmp.TextColors.Marked;
      TextColors.Selected := FCalendarTmp.TextColors.Selected;
      TextColors.Standard := FCalendarTmp.TextColors.Standard;
      TextColors.Today := FCalendarTmp.TextColors.Today;
      TextColors.Weekend := FCalendarTmp.TextColors.Weekend;

    end;

    FMonth := TLabel.Create(FDatePanel);
    with FMonth do
    begin
      Parent := FDatePanel;
      Top := 6;
      Left := FBtnMonthNext.Left + FBtnMonthNext.Width +10;
      Font.Style := [fsBold];
      Font.Color := FMonthColor;
      Caption := LongMonthNames[FCalendar.Month];
    end;

    FYear := TLabel.Create(FDatePanel);
    with FYear do
    begin
      Parent := FDatePanel;
      Left := FBtnYearLast.Left - 40;
      Top := 6;
      Font.Style := [fsBold];
      Font.Color := FYearColor;
      Caption := IntToStr(FCalendar.Year);
    end;
  end;
  SetPos;
  DoOnCreate(Self);
  FWork.Show;
  FCalendar.SetFocus;
  FInit := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.Execute;
begin
  Execute(nil);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetPos;
begin
  if FWork <> nil then
  begin
    if FPosition = psFormCenter then
    begin
      FWork.Left := (FForm.Width div 2) - (FWork.Width div 2);
      FWork.Top := (FForm.Height div 2) - (FWork.Height div 2);
    end
    else if (FPosition = psUser) or (FPosition = psWinControl) then
    begin
      FWork.Left := FUserLeft;
      FWork.Top := FUserTop;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetUserLeft(const Value: Integer);
begin
  FUserLeft := Value;
  SetPos;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetUserTop(const Value: Integer);
begin
  FUserTop := Value;
  SetPos;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
  SetPos;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetMonthColor(const Value: TColor);
begin
  FMonthColor := Value;
  if FDatePanel <> nil then
    FMonth.Font.Color := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetYearColor(const Value: TColor);
begin
  FYearColor := Value;
  if FDatePanel <> nil then
    FYear.Font.Color := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.MonthLastClick(Sender: TObject);
begin
  FCalendar.PrevMonth;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.MonthNextClick(Sender: TObject);
begin
  FCalendar.NextMonth;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.YearLastClick(Sender: TObject);
begin
  FCalendar.PrevYear;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.YearNextClick(Sender: TObject);
begin
  FCalendar.NextYear;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetTitel(const Value: string);
begin
  FTitel := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetBorder(const Value: Boolean);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    if FWork <> nil then
    begin
      if FBorder then
        FWork.BorderStyle := bsToolWindow
      else
        FWork.BorderStyle := bsNone;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.DoCalDbl(Sender: TObject);
begin
  DoOnDblClick(Sender);
  DoOnExit(Sender);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
//function TCHDateDialog.GetFont: TFont;
//begin
//  //Result := Self.Font;
//end;
//
//{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
//procedure TCHDateDialog.SetFont(const Value: TFont);
//begin
//  //FWork.Font := Value;
//end;

{ TCalendar }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCalendar.Create(AOwner: TCHDateDialog);
begin
  inherited Create;
  FOwner := AOwner;

  FBackgroundColors := TCalendarColors.Create;
  FTextColors := TCalendarColors.Create;
  with FBackgroundColors do begin
    Headers := clBtnFace;
    Holiday := clWindow;
    Marked := clAqua;
    Selected := clHighlight;
    Standard := clWindow;
    Today := clWindow;
    Weekend := clWindow;
  end;
  with FTextColors do begin
    Headers := clBtnText;
    Holiday := clRed;
    Marked := clWindowText;
    Selected := clHighlightText;
    Standard := clWindowText;
    Today := clBlue;
    Weekend := clMaroon;
  end;

  FGermanState:=gsBayern;
  FLatitude:=GermanStateLat[ord(gsNordrhein_Westfalen)];
  FLongitude:=GermanStateLong[ord(gsNordrhein_Westfalen)];
  FDrawStyle := cdsColorGrid;
  FCalendarOptions := [coAutoDeleteMarks, coCalcAstroData, coCalcHolidays,
                       coGridLines, coFrameSelection, coShowMarks, coUseCurrentDate];
  FStartOfWeek := dowMonday;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCalendar.GetDateElement(const Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FOwner.FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

procedure TCHDateDialog.DoOnBeforeChange(Sender: TObject);
begin
  if Assigned(FOnBeforeChange) then
    FOnBeforeChange(Self);
end;

procedure TCHDateDialog.DoOnAfterChange(Sender: TObject);
begin
  if (not FInit) or (TCustomEdit(FEditControl).Text = '') then
  begin
    if FDatePanel <> nil then
    begin
      FMonth.Caption := LongMonthNames[FCalendar.Month];
      FYear.Caption := IntToStr(FCalendar.Year);
      if FEditControl <> nil then
      begin
        TCustomEdit(FEditControl).Text := FormatDateTime(FFormat, FCalendar.Date);
      end;
    end;

    if Assigned(FOnAfterChange) then
      FOnAfterChange(Self);
  end;
end;

procedure TCHDateDialog.DoOnClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCHDateDialog.DoOnDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);

  if (FEditControl <> nil) and (FEditControl is TCHEditBtn) then
  begin
    if Assigned(TCHEditBtn(FEditControl).FOnDialogOK) then
      TCHEditBtn(FEditControl).OnDialogOK(Self);
  end;

end;

procedure TCHDateDialog.DoOnClose(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TCHDateDialog.DoOnEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TCHDateDialog.DoOnExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);

  if (FEditControl <> nil) and (FEditControl is TCHEditBtn) then
    if not TCHEditBtn(FEditControl).CheckDate then
      Exit;


  DoOnClose(Self);
  FWork.Close;
  FWork := nil;
end;

procedure TCHDateDialog.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self);
end;

procedure TCHDateDialog.DoOnKeyPress(Sender: TObject; var Key : Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self);
end;

procedure TCHDateDialog.DoOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self);
end;

procedure TCHDateDialog.DoOnMonthChange(Sender: TObject);
begin
  if Assigned(FOnMonthChange) then
    FOnMonthChange(Self);
end;

procedure TCHDateDialog.DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TCHDateDialog.DoOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TCHDateDialog.DoOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self);
end;

procedure TCHDateDialog.DoOnYearChange(Sender: TObject);
begin
  if Assigned(FOnYearChange) then
    FOnYearChange(Self);
end;

procedure TCHDateDialog.DoOnCreate(Sender: TObject);
begin
  if Assigned(FOnCreate) then
    FOnCreate(Self);

  if (TEdit(FEditControl).Text = '') then
  begin
    if FDatePanel <> nil then
    begin
      if FEditControl <> nil then
      begin
        TEdit(FEditControl).Text := FormatDateTime(FFormat, FCalendar.Date);
      end;
    end;
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.SetDialogPos(WC: TCustomEdit);
var
  nLeft, nTop : Integer;
  nLT : TPoint;
begin
  if WC <> nil then
  begin
    nLT.X := 0;
    nLT.Y := 0;
    GetLeftTop(WC, nLT);

    nTop := nLT.Y;
    nLeft := nLT.X;

    // Top
    if (nTop + Self.Height) < FForm.ClientHeight then
      Self.Top := WC.Height + nTop
    else
      Self.Top := nTop - Self.Height;

    // Left
    if (nLeft + Self.Width) < FForm.ClientWidth then
      Self.Left := nLeft
    else
      Self.Left := nLeft - (Self.Width - WC.Width);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHDateDialog.GetLeftTop(WC : TWinControl; var LT : TPoint);
begin
  if WC = FForm then
  begin
    Exit;
  end
  else
  begin
    LT.X := LT.X + WC.Left;
    LT.Y := LT.Y + WC.Top;
    GetLeftTop(WC.Parent, LT);
  end;
end;

end.
