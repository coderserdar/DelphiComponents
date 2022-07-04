{*********************************************************}
{*                VPMONTHVIEW.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpMonthView;

interface

uses
  Windows, Classes, Graphics, Controls, ComCtrls, ExtCtrls, Messages, StdCtrls,
  VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst, VpCanvasUtils, Menus;

type
  TVpMonthdayRec = packed record
    Rec     : TRect;
    Date    : TDateTime;
    OffDay  : Boolean;
  end;

type
  TVpMonthdayArray = array of TVpMonthdayRec;

  { Forward Declarations }
  TVpMonthView = class;

  TVpMVDayNameStyle = (dsLong, dsShort, dsLetter);

  TVpOnEventClick =                                                      
    procedure(Sender: TObject; Event: TVpEvent) of object;               

  TVpDayHeadAttr = class(TPersistent)
  protected{private}
    FMonthView: TVpMonthView;
    FFont: TFont;
    FColor: TColor;
    procedure SetColor (Value: TColor);
    procedure SetFont (Value: TFont);
  public
    constructor Create(AOwner: TVpMonthView);
    destructor Destroy; override;
    property MonthView: TVpMonthView read FMonthView;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
  end;

  TVpMonthView = class(TVpLinkableControl)
  protected{ private }
    FKBNavigate        : Boolean;
    FColumnWidth       : Integer;
    FColor             : TColor;
    FLineColor         : TColor;
    FLineCount         : Integer;
    FVisibleLines      : Integer;
    FDayNameStyle      : TVpMVDayNameStyle;
    FOffDayColor       : TColor;
    FSelectedDayColor  : TColor;
    FWeekStartsOn      : TVpDayType;
    FShowEvents        : Boolean;
    FEventDayStyle     : TFontStyles;
    FDateLabelFormat   : string;
    FShowEventTime     : Boolean;
    FTopLine           : Integer;
    FDayHeadAttributes : TVpDayHeadAttr;
    FDayNumberFont     : TFont;
    FEventFont         : TFont;
    FTimeFormat        : TVpTimeFormat;
    FDrawingStyle      : TVpDrawingStyle;
    FDate              : TDateTime;
    FDefaultPopup      : TPopupMenu;
    FRightClickChangeDate : Boolean;                                     
    { event variables }
    FOwnerDrawCells    : TVpOwnerDrawDayEvent;
    FOnEventClick      : TVpOnEventClick;                                
    FOnEventDblClick   : TVpOnEventClick;                                
    { internal variables }
    mvDayNumberHeight  : Integer;
    mvEventTextHeight  : Integer;
    mvLoaded           : Boolean;
    mvInLinkHandler    : Boolean;
    mvRowHeight        : Integer;
    mvLineHeight       : Integer;
    mvColWidth         : Integer;
    mvDayHeadHeight    : Integer;
    mvSpinButtons      : TUpDown;
    mvEventArray       : TVpEventArray;
    mvMonthDayArray    : TVpMonthdayArray;
    mvActiveEvent      : TVpEvent;
    mvActiveEventRec   : TRect;
    mvEventList        : TList;
    mvCreatingEditor   : Boolean;
    mvPainting         : Boolean;
    mvVScrollDelta     : Integer;
    mvHotPoint         : TPoint;
    mvVisibleEvents    : Integer;                                        

    { property methods }
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetOffDayColor(Value: TColor);
    procedure SetDateLabelFormat(Value: string);
    procedure SetShowEvents(Value: Boolean);
    procedure SetEventDayStyle(Value: TFontStyles);
    procedure SetDayNameStyle(Value: TVpMVDayNameStyle);
    procedure SetDayNumberFont(Value: TFont);
    procedure SetEventFont(Value: TFont);
    procedure SetSelectedDayColor(Value: TColor);
    procedure SetShowEventTime(Value: Boolean);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetDate(Value: TDateTime);
    procedure SetRightClickChangeDate (const v : Boolean);               
    procedure SetWeekStartsOn(Value: TVpDayType);
    { internal methods }
    procedure mvHookUp;
    procedure mvFontChanged(Sender: TObject);

    procedure Paint; override;
    procedure Loaded; override;
    procedure InitializeDefaultPopup;
    procedure mvPopulate;
    procedure mvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMLButtonDown(var Msg : TWMLButtonDown);
      message WM_LBUTTONDOWN;
    procedure WMLButtonDblClick(var Msg: TWMLButtonDblClk);              
      message WM_LBUTTONDBLCLK;                                          
    { - renamed from EditEventAtCoord and re-written}
    function  SelectEventAtCoord(Point: TPoint): Boolean;
    procedure mvSetDateByCoord(Point: TPoint);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { message handlers }
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure PopupToday (Sender : TObject);
    procedure PopupNextMonth (Sender : TObject);
    procedure PopupPrevMonth (Sender : TObject);
    procedure PopupNextYear (Sender : TObject);
    procedure PopupPrevYear (Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType;
      const Value: Variant); override;
    function GetControlType : TVpItemType; override;
    procedure PaintToCanvas (ACanvas : TCanvas;
                             ARect   : TRect;
                             Angle   : TVpRotationAngle;
                             ADate   : TDateTime);
    procedure RenderToCanvas (RenderCanvas : TCanvas;
                              RenderIn     : TRect;
                              Angle        : TVpRotationAngle;
                              Scale        : Extended;
                              RenderDate   : TDateTime;
                              StartLine    : Integer;
                              StopLine     : Integer;
                              UseGran      : TVpGranularity;
                              DisplayOnly  : Boolean); override;

    property Date: TDateTime read FDate write SetDate;
  published
    { inherited properties }
    property Align;
    property Anchors;
    property TabStop;
    property TabOrder;
    property KBNavigation: Boolean
      read FKBNavigate write FKBNavigate;
    property Color: TColor
      read FColor write SetColor;
    property DateLabelFormat:
      string read FDateLabelFormat write SetDateLabelFormat;
    property DayHeadAttributes: TVpDayHeadAttr
      read FDayHeadAttributes write FDayHeadAttributes;
    property DayNameStyle: TVpMVDayNameStyle
      read FDayNameStyle write SetDayNameStyle;
    property DayNumberFont: TFont
      read FDayNumberFont write SetDayNumberFont;
    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle;
    property EventDayStyle: TFontStyles
      read FEventDayStyle write SetEventDayStyle;
    property EventFont: TFont
      read FEventFont write SetEventFont;
    property LineColor: TColor
      read FLineColor write SetLineColor;
    property TimeFormat: TVpTimeFormat
      read FTimeFormat write SetTimeFormat;
    property OffDayColor: TColor
      read FOffDayColor write SetOffDayColor;
    property OwnerDrawCells: TVpOwnerDrawDayEvent
      read FOwnerDrawCells write FOwnerDrawCells;
    property RightClickChangeDate : Boolean                              
             read FRightClickChangeDate write SetRightClickChangeDate    
             default vpDefWVRClickChangeDate;                            
    property SelectedDayColor: TColor
      read FSelectedDayColor write SetSelectedDayColor;
    property ShowEvents: Boolean
      read FShowEvents write SetShowEvents;
    property ShowEventTime: Boolean
      read FShowEventTime write SetShowEventTime;
    property WeekStartsOn : TVpDayType
      read FWeekStartsOn write SetWeekStartsOn;
    {events}
    property OnEventClick: TVpOnEventClick                               
      read FOnEventClick write FOnEventClick;                            
    property OnEventDblClick: TVpOnEventClick                            
      read FOnEventDblClick write FOnEventDblClick;                      
  end;

implementation

uses
  SysUtils, Math, Forms, Dialogs, VpEvntEditDlg;

(*****************************************************************************)
{ TVpContactHeadAttr }
constructor TVpDayHeadAttr.Create(AOwner: TVpMonthView);
begin
  inherited Create;
  FMonthView := AOwner;
  FFont := TFont.Create;
  FFont.Assign(FMonthView.Font);
  FFont.Size := 8;
  FColor := clSilver;
end;
{=====}

destructor TVpDayHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpDayHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    MonthView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetFont(Value: TFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    MonthView.Invalidate;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpMonthView }

constructor TVpMonthView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  { Create internal classes and stuff }
  FDayHeadAttributes := TVpDayHeadAttr.Create(self);
  mvEventList := TList.Create;
  mvSpinButtons := TUpDown.Create(self);

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered := true;
  {$ENDIF}
  FShowEvents := true;
  FEventDayStyle := [];
  FShowEventTime := false;
  FDayNameStyle :=dsShort;
  FKBNavigate := true;
  mvInLinkHandler := false;
  mvSpinButtons.OnClick := mvSpinButtonClick;
  mvSpinButtons.Orientation := udHorizontal;
  mvSpinButtons.Min := -32768;
  mvSpinButtons.Max := 32767;
  mvCreatingEditor := false;
  FSelectedDayColor := clRed;
  FDrawingStyle := ds3d;
  mvPainting := false;
  FColor := clWindow;
  FOffDayColor := clSilver;
  FLineColor := clGray;
  FDate := Trunc(Now);
  FTimeFormat := tf12Hour;
  FDateLabelFormat := 'mmmm yyyy';
  FColumnWidth := 200;
  FRightClickChangeDate := vpDefWVRClickChangeDate;                      
  mvVisibleEvents := 0;                                                  

  { set up fonts and colors }
  FDayHeadAttributes.Font.Name := 'Tahoma';
  FDayHeadAttributes.Font.Size := 10;
  FDayHeadAttributes.Font.Style := [];
  FDayHeadAttributes.Color := clBtnFace;

  { Assign default font to DayNumberFont and EventFont }
  FDayNumberFont := TFont.Create;
  FDayNumberFont.Assign(Font);
  FDayNumberFont.OnChange := mvFontChanged;
  FEventFont := TFont.Create;
  FEventFont.Assign(Font);
  FEventFont.OnChange := mvFontChanged;

  SetLength(mvEventArray, MaxVisibleEvents);
  SetLength(mvMonthdayArray, 45);

  { size }
  Height := 225;
  Width := 300;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  mvHookUp;
end;
{=====}

destructor TVpMonthView.Destroy;
begin
  FDayHeadAttributes.Free;
  FDayNumberFont.Free;
  FEventFont.Free;
  mvSpinButtons.Free;
  mvEventList.Free;
  FDefaultPopup.Free;
  inherited;
end;
{=====}

procedure TVpMonthView.Invalidate;
begin
  inherited;
end;
{=====}

procedure TVpMonthView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  mvInLinkHandler := true;
  try
    case NotificationType of
      neDateChange: Date := Value;
      neDataStoreChange: Invalidate;
      neInvalidate: Invalidate;
    end;
  finally
    mvInLinkHandler := false;
  end;
end;
{=====}

procedure TVpMonthView.mvHookUp;
var
  I: Integer;
begin
  { If the component is being dropped on a form at designtime, then }
  { automatically hook up to the first datastore component found    }
  if csDesigning in ComponentState then
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore(Owner.Components[I]);
        Exit;
      end;
    end;
end;
{=====}

procedure TVpMonthView.mvFontChanged(Sender: TObject);
begin
  Invalidate;
end;
{=====}

procedure TVpMonthView.Loaded;
begin
  inherited;
  mvLoaded := true;
  mvPopulate;
end;
{=====}

function TVpMonthView.GetControlType : TVpItemType;
begin
  Result := itMonthView;
end;

procedure TVpMonthView.Paint;
begin
  RenderToCanvas (Canvas,
                  Rect (0, 0, Width, Height),
                  ra0,
                  1,
                  Self.Date,
                  -1,
                  -1,
                  gr30Min,
                  False);
end;
{=====}
procedure TVpMonthView.PaintToCanvas (ACanvas : TCanvas;
                                       ARect   : TRect;
                                       Angle   : TVpRotationAngle;
                                       ADate   : TDateTime);
begin
  RenderToCanvas (ACanvas, ARect, Angle, 1, ADate,
                  -1, -1, gr30Min, True);
end;
{=====}
procedure TVpMonthView.RenderToCanvas (RenderCanvas : TCanvas;
                                       RenderIn     : TRect;
                                       Angle        : TVpRotationAngle;
                                       Scale        : Extended;
                                       RenderDate   : TDateTime;
                                       StartLine    : Integer;
                                       StopLine     : Integer;
                                       UseGran      : TVpGranularity;
                                       DisplayOnly  : Boolean);
var
  HeadRect       : TRect;
  SaveBrushColor : TColor;
  SavePenStyle   : TPenStyle;
  SavePenColor   : TColor;
  DisplayDate    : TDateTime;

  RealWidth      : Integer;
  RealHeight     : Integer;
  RealLeft       : Integer;
  RealRight      : Integer;
  RealTop        : Integer;
  RealBottom     : Integer;
  Rgn            : HRGN;

  RealColor        : TColor;
  BevelHighlight   : TColor;
  BevelShadow      : TColor;
  BevelDarkShadow  : TColor;
  BevelFace        : TColor;
  DayHeadAttrColor : TColor;
  RealLineColor    : TColor;
  RealOffDayColor  : TColor;
  RealSelDayColor  : TColor;
  EventFontColor   : TColor;
  DotDotDotColor   : TColor;

  procedure Clear;
  begin
    RenderCanvas.Brush.Color := RealColor;
    RenderCanvas.FillRect(RenderIn);
  end;
  {-}

  procedure SetMeasurements;
  begin
    RealWidth  := TPSViewportWidth (Angle, RenderIn);
    RealHeight := TPSViewportHeight (Angle, RenderIn);
    RealLeft   := TPSViewportLeft (Angle, RenderIn);
    RealRight  := TPSViewportRight (Angle, RenderIn);
    RealTop    := TPSViewportTop (Angle, RenderIn);
    RealBottom := TPSViewportBottom (Angle, RenderIn);

    if RenderDate = 0 then
      DisplayDate := Date
    else
      DisplayDate := RenderDate;

    { we use the VpProductName because is is a good representation of some }
    { generic text }
    RenderCanvas.Font.Assign(FDayHeadAttributes.Font);
    mvDayHeadHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + 2;
    RenderCanvas.Font.Assign(FDayNumberFont);
    mvDayNumberHeight := RenderCanvas.TextHeight('00');
    RenderCanvas.Font.Assign(FEventFont);
    mvEventTextHeight := RenderCanvas.TextHeight(VpProductName);
    RenderCanvas.Font.Assign(Font);
    mvLineHeight := RenderCanvas.TextHeight(VpProductName) + 2;
    mvColWidth   := (RealWidth - 4) div 7;
  end;
  {-}

  procedure DrawHeader;
  var
    HeadTextRect: TRect;
    HeadStr: string;
    HeadStrLen : Integer;
  begin
    RenderCanvas.Brush.Color := DayHeadAttrColor;
    { draw the header cell and borders }

    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      HeadRect.Left := RealLeft + 1;
      HeadRect.Top := RealTop + 1;
      HeadRect.Right := RealRight - 1;
      HeadRect.Bottom := RealTop + mvDayHeadHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, HeadRect),
                     BevelHighlight, BevelShadow);
    end else if FDrawingStyle = ds3d then begin
      { draw a 3d bevel }
      HeadRect.Left := RealLeft + 2;
      HeadRect.Top := RealTop + 2;
      HeadRect.Right := RealRight - 3;
      HeadRect.Bottom := RealTop + mvDayHeadHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, HeadRect),
                     BevelHighlight, BevelDarkShadow);
    end;

    { Acquire startdate and end date }
    HeadStr := FormatDateTime(DateLabelFormat, DisplayDate);

    { draw the text }
    if (DisplayOnly) and
       (RenderCanvas.TextWidth (HeadStr) >= RealWidth) then
      HeadTextRect.TopLeft:= Point (RealLeft + TextMargin * 2,
                                    HeadRect.Top)
    else if DisplayOnly then
      HeadTextRect.TopLeft := Point (RealLeft +
                                     (RealWidth -
                                     RenderCanvas.TextWidth (HeadStr)) div 2,
                                     HeadRect.Top)
    else
      HeadTextRect.TopLeft := Point (RealLeft + 30 + TextMargin * 2,
                                     HeadRect.Top);
    HeadTextRect.BottomRight := HeadRect.BottomRight;

    { Fix Header String }
    HeadStrLen := RenderCanvas.TextWidth(HeadStr);

    if HeadStrLen > HeadTextRect.Right - HeadTextRect.Left then begin
      HeadStr := GetDisplayString(RenderCanvas, HeadStr, 0,
        HeadTextRect.Right - HeadTextRect.Left - TextMargin);
    end;

    { position the spinner }
    mvSpinButtons.Height := Trunc(mvDayHeadHeight * 0.8);
    mvSpinButtons.Width := mvSpinButtons.Height * 2;
    mvSpinButtons.Left := TextMargin;
    mvSpinButtons.Top := (mvDayHeadHeight - mvSpinButtons.Height) div 2 + 2;

    RenderCanvas.Font.Assign (FDayHeadAttributes.Font);
    TPSTextOut (RenderCanvas, Angle, RenderIn,
                RealLeft + mvSpinButtons.Width + TextMargin * 2,
                HeadTextRect.Top + TextMargin, HeadStr);
  end;
  {-}

  procedure DrawDayHead;
  var
    dhRect : TRect;
    I      : Integer;
    DayTag : Integer;
    Str    : string;
    StrL   : Integer;
  begin
    { clear day head area }
    RenderCanvas.Font.Assign(DayHeadAttributes.Font);
    RenderCanvas.Brush.Color := DayHeadAttrColor;

    { build rect }
    if DrawingStyle = ds3D then begin
      dhRect.Left := RealLeft + 1;
      dhRect.Top := RealTop + mvDayHeadHeight + 3;
      dhRect.Right := RealRight - 3;
      dhRect.Bottom := dhRect.Top + mvDayHeadHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, dhRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, dhRect),
                     BevelHighlight, BevelDarkShadow);
    end else begin
      dhRect.Left := RealLeft + 1;
      dhRect.Top := RealTop + mvDayHeadHeight + 2;
      dhRect.Right := RealRight - 1;
      dhRect.Bottom := dhRect.Top + mvDayHeadHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, dhRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, dhRect),
                     BevelHighlight, BevelShadow);
    end;

    DayTag := Ord(WeekStartsOn);
    dhRect.Right := dhRect.Left + mvColWidth;
    for I := 0 to 6 do begin
      { draw the little vertical lines between each day }
      if I < 6 then
        DrawBevelRect (RenderCanvas,
                       TPSRotateRectangle (Angle, RenderIn,
                                           Rect (dhRect.Right,
                                                 dhRect.Top + 3,
                                                 dhRect.Right + 1,
                                                 dhRect.Bottom - 3)),
                       BevelShadow, BevelHighlight);

      if FDayNameStyle = dsLong then
        { Draw each day's full caption... }
        case DayTag of
          0: str := RSSunday;
          1: str := RSMonday;
          2: str := RSTuesday;
          3: str := RSWednesday;
          4: str := RSThursday;
          5: str := RSFriday;
          6: str := RSSaturday;
        end
      else if FDayNameStyle = dsShort then
        { Draw each day's abbreviated caption... }
        case DayTag of
          0: str := RSASunday;
          1: str := RSAMonday;
          2: str := RSATuesday;
          3: str := RSAWednesday;
          4: str := RSAThursday;
          5: str := RSAFriday;
          6: str := RSASaturday;
        end
      else if FDayNameStyle = dsLetter then
        { Draw each day's first letter only }
        case DayTag of
          0: str := RSLSunday;
          1: str := RSLMonday;
          2: str := RSLTuesday;
          3: str := RSLWednesday;
          4: str := RSLThursday;
          5: str := RSLFriday;
          6: str := RSLSaturday;
        end;

      { Fix Header String }
      StrL := RenderCanvas.TextWidth(Str);
      if (StrL > mvColWidth - (TextMargin * 2)) then begin
        Str := GetDisplayString (RenderCanvas, Str, 0,
                                 mvColWidth - (TextMargin * 2));
      end;
      StrL := RenderCanvas.TextWidth(Str);

      TPSTextOut (RenderCanvas, Angle, RenderIn,
                  dhRect.Left + (dhRect.Right - dhRect.Left) div 2 -
                  (Strl div 2), dhRect.Top + TextMargin - 1, Str);

      if DayTag = 6 then
        DayTag := 0
      else
        Inc(DayTag);
      dhRect.Left := dhRect.Right;
      dhRect.Right := dhRect.Left + mvColWidth;
    end;

  end;
  {-}

  procedure DrawDays;
  var
    TextRect      : TRect;
    Col, Row      : Integer;
    DayNumber     : Integer;
    M, D, Y, Tmp  : Word;
    MonthStartsOn : Integer;
    DayTag        : Integer;
    DayOffset     : Integer;
    StartingDate  : TDateTime;
    ThisDate      : TDateTime;
    Str           : string;
    StrLn         : Integer;
    I, J          : Integer;
    EventList     : TList;
    Drawn         : Boolean;
    TextAdjust    : Integer;
    FontStyle     : TFontStyles;
    OldBrush      : TBrush;
    OldPen        : TPen;
    OldFont       : TFont;
  begin
    { initialize the MonthDayArray }
    for I := 0 to Pred(Length(mvMonthDayArray)) do begin
      mvMonthDayArray[I].Rec     := Rect(-1, -1, -1, -1);
      mvMonthDayArray[I].Date    := 0.0;
    end;

    RenderCanvas.Pen.Color := RealLineColor;
    RenderCanvas.Brush.Color := RealColor;
    mvRowHeight := (RealHeight - (mvDayHeadHeight * 2) - 4) div 6;
    TextRect.TopLeft := Point (RealLeft + 1,
                               RealTop + (mvDayHeadHeight * 2) + 4);
    TextRect.BottomRight := Point (TextRect.Left +  mvColWidth,
                                   TextRect.Top + mvRowHeight);

    { Determine the starting date and offset }
    DecodeDate(DisplayDate, Y, M, D);
    StartingDate := EncodeDate(Y, M, 1);
    MonthStartsOn := DayOfWeek(StartingDate);
    DayTag := Ord(WeekStartsOn);
    DayOffset := DayTag - MonthStartsOn;

    I := 0;
    DayNumber := DayOffset + 1;

    { iterate through each column, row by row, drawing each day in numerical }
    { order.                                                                 }

    OldBrush := TBrush.Create;
    try
      OldPen := TPen.Create;
      try
        OldFont := TFont.Create;
        try
          for Row := 0 to 5 do begin
            for Col := 0 to 6 do begin
              if (Col = 6) then begin
                { draws the far right day for this week }
                ThisDate := trunc(StartingDate + DayNumber);
                DecodeDate(ThisDate, Y, Tmp, D);

                { Allow the user to draw the day }
                Drawn  := false;
                if Assigned(FOwnerDrawCells) then begin
                  OldBrush.Assign (Canvas.Brush);
                  OldPen.Assign (Canvas.Pen);
                  OldFont.Assign (Canvas.Font);
                  try
                    FOwnerDrawCells(self, RenderCanvas, TextRect, D, Drawn);
                    if Drawn then continue;
                  finally
                    Canvas.Brush.Assign (OldBrush);
                    Canvas.Pen.Assign (OldPen);
                    Canvas.Font.Assign (OldFont);
                  end;
                end;

                TextRect.Right := TextRect.Right + 8;
                if Tmp <> M then begin
                  RenderCanvas.Brush.Color := RealOffDayColor;
                  if TextRect.Bottom > RealBottom then
                    TPSFillRect (RenderCanvas, Angle, RenderIn,
                                 Rect (TextRect.Left, TextRect.Top,
                                       RealRight, RealBottom))
                  else
                    TPSFillRect (RenderCanvas, Angle, RenderIn,
                                 Rect (TextRect.Left, TextRect.Top,
                                       RealRight, TextRect.Bottom));
                end else
                  RenderCanvas.Brush.Color := RealColor;
                { draw bottom line }
                TPSMoveTo (RenderCanvas, Angle, RenderIn,
                           TextRect.Left, TextRect.Bottom);
                TPSLineTo (RenderCanvas, Angle, RenderIn, RealRight - 2,
                           TextRect.Bottom);
                { Paint the day number }
                Str := FormatDateTime('d', ThisDate);

                { set the proper font and style }
                RenderCanvas.Font.Assign(FDayNumberFont);
                if (DisplayDate = ThisDate) then begin
                  if Focused then begin
                    TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                                      Rect (TextRect.Left - 2,
                                            TextRect.Top - 2,
                                            TextRect.Right + 2,
                                            TextRect.Bottom + 2));
                    TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                                      Rect (TextRect.Left + 2,
                                            TextRect.Top + 2,
                                            TextRect.Right - 2,
                                            TextRect.Bottom - 2));
                  end;
                  RenderCanvas.Font.Color := RealSelDayColor;
                  RenderCanvas.Font.Style := FDayNumberFont.Style + [fsBold];
                  if (FEventDayStyle <> []) and (DataStore.Resource <> nil)
                  and (DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                  then
                    RenderCanvas.Font.Style := RenderCanvas.Font.Style
                      + FEventDayStyle;
                end else begin
                  { Set the font style for days which have events. }
                  if (FEventDayStyle <> []) and (DataStore.Resource <> nil)
                  and (DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                  then
                    RenderCanvas.Font.Style := RenderCanvas.Font.Style
                      + FEventDayStyle
                  else begin
                    RenderCanvas.Font.Color := EventFontColor;
                    RenderCanvas.Font.Style := FDayNumberFont.Style;
                  end;
                end;

                FontStyle := RenderCanvas.Font.Style;
                RenderCanvas.Font.Style := [fsBold, fsItalic];
                TextAdjust := RenderCanvas.TextWidth (Str);
                RenderCanvas.Font.Style := FontStyle;

                { write the day number at the top of the square. }
                if fsItalic in RenderCanvas.Font.Style then
                  TPSTextOut (RenderCanvas, Angle, RenderIn,
                              TextRect.Left + mvColWidth - TextAdjust -
                              TextMargin - 2,
                              TextRect.Top + (TextMargin div 2), Str)
                else
                  TPSTextOut (RenderCanvas, Angle, RenderIn,
                              TextRect.Left + mvColWidth - TextAdjust
                                - TextMargin, TextRect.Top + (TextMargin div 2),
                                Str);


                { Update MonthDayArray }
                mvMonthDayArray[I].Rec := TextRect;
                mvMonthDayArray[I].Date := ThisDate;
                mvMonthDayArray[I].OffDay := Tmp <> M;
                Inc(DayNumber);
                Inc(I);

                { drop rect down one row and all the way to the left }
                TextRect.TopLeft := Point(RealLeft + 1, TextRect.Bottom + 1);
                TextRect.BottomRight := Point(TextRect.Left + mvColWidth,
                  TextRect.Top + mvRowHeight);
              end else begin
                { draws all days for the week, except the far right one }
                ThisDate := Trunc(StartingDate + DayNumber);
                DecodeDate(ThisDate, Y, Tmp, D);

                { Allow the user to draw the day }
                Drawn  := false;
                if Assigned(FOwnerDrawCells) then begin
                  OldBrush.Assign (Canvas.Brush);
                  OldPen.Assign (Canvas.Pen);
                  OldFont.Assign (Canvas.Font);
                  try
                    FOwnerDrawCells(self, RenderCanvas, TextRect, D, Drawn);
                    if Drawn then continue;
                  finally
                    Canvas.Brush.Assign (OldBrush);
                    Canvas.Pen.Assign (OldPen);
                    Canvas.Font.Assign (OldFont);
                  end;
                end;

                if Tmp <> M then begin
                  RenderCanvas.Brush.Color := RealOffDayColor;
                  TPSFillRect (RenderCanvas, Angle, RenderIn, TextRect);
                end else
                  RenderCanvas.Brush.Color := RealColor;
                { draw right side and bottom lines }
                TPSMoveTo (RenderCanvas, Angle, RenderIn, TextRect.Right,
                  TextRect.top);
                if TextRect.Bottom > RealBottom then begin
                  TPSLineTo (RenderCanvas, Angle, RenderIn, TextRect.Right,
                    RealBottom);
                  TPSLineTo (RenderCanvas, Angle, RenderIn, TextRect.Left - 1,
                    RealBottom);
                end else begin
                  TPSLineTo (RenderCanvas, Angle, RenderIn, TextRect.Right,
                    TextRect.Bottom);
                  TPSLineTo (RenderCanvas, Angle, RenderIn, TextRect.Left - 1,
                    TextRect.Bottom);
                end;
                { paint the day number }
                Str := FormatDateTime('d', ThisDate);

                { set the proper font and style }
                RenderCanvas.Font.Assign(FDayNumberFont);
                if (DisplayDate = ThisDate) then begin
                  if Focused then begin
                    TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                                      Rect (TextRect.Left - 2,
                                            TextRect.Top - 2,
                                            TextRect.Right + 2,
                                            TextRect.Bottom + 2));
                    TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                                      Rect (TextRect.Left + 2,
                                            TextRect.Top + 2,
                                            TextRect.Right - 2,
                                            TextRect.Bottom - 2));
                  end;
                  RenderCanvas.Font.Color := RealSelDayColor;
                  RenderCanvas.Font.Style := FDayNumberFont.Style + [fsBold];
                  if (FEventDayStyle <> []) and (DataStore.Resource <> nil)
                  and (DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                  then
                    RenderCanvas.Font.Style := RenderCanvas.Font.Style
                      + FEventDayStyle;
                end else begin
                  { Set the font style for days which have events. }
                  if (FEventDayStyle <> []) and (DataStore.Resource <> nil)
                  and (DataStore.Resource.Schedule.EventCountByDay(ThisDate) > 0)
                  then
                    RenderCanvas.Font.Style := RenderCanvas.Font.Style
                      + FEventDayStyle
                  else begin
                    RenderCanvas.Font.Color := EventFontColor;
                    RenderCanvas.Font.Style := FDayNumberFont.Style;
                  end;
                end;

                FontStyle := RenderCanvas.Font.Style;
                RenderCanvas.Font.Style := [fsBold, fsItalic];
                TextAdjust := RenderCanvas.TextWidth (Str);
                RenderCanvas.Font.Style := FontStyle;

                if fsItalic in RenderCanvas.Font.Style then
                  TPSTextOut (RenderCanvas, Angle, RenderIn,
                              TextRect.Right - TextAdjust - TextMargin - 2,
                              TextRect.Top + (TextMargin div 2), Str)
                else
                  TPSTextOut (RenderCanvas, Angle, RenderIn,
                              TextRect.Right - TextAdjust - TextMargin,
                              TextRect.Top + (TextMargin div 2), Str);

                { Update Array }
                mvMonthDayArray[I].Rec := TextRect;
                mvMonthDayArray[I].Date := ThisDate;
                mvMonthDayArray[I].OffDay := Tmp <> M;
                Inc(DayNumber);
                Inc(I);
                { slide rect one column to the right }
                TextRect.Left := TextRect.Right + 1;
                TextRect.Right := TextRect.Right + mvColWidth;
              end;
            end;
          end;

        finally
          OldFont.Free;
        end;
      finally
        OldPen.Free;
      end;
    finally
      OldBrush.Free;
    end;

    RenderCanvas.Pen.Color := RealLineColor;
    RenderCanvas.Pen.Style := psSolid;
    RenderCanvas.Brush.Color := RealColor;

    { write the events }
    if (DataStore <> nil) and FShowEvents and (DataStore.Resource <> nil)
    and (DataStore.Resource.Schedule.EventCount <> 0) then begin
      EventList := TList.Create;
      try
        for I := 0 to 43 do begin
          EventList.Clear;
          DataStore.Resource.Schedule.EventsByDate(mvMonthDayArray[I].Date, EventList);
          if EventList.Count > 0 then begin
            { there are events scheduled for this day }

            { initialize TextRect for this day }
            TextRect.TopLeft := Point(mvMonthDayArray[I].Rec.Left,
              mvMonthDayArray[I].Rec.Top);
            TextRect.BottomRight := Point(TextRect.Left + mvColWidth,
              TextRect.Top + mvEventTextHeight + (TextMargin div 2));

            { set canvas color }
            if mvMonthDayArray[I].OffDay
            then RenderCanvas.Brush.Color := RealOffDayColor
            else RenderCanvas.Brush.Color := RealColor;

            { spin through the events and paint them }
            for J := 0 to Pred(EventList.Count) do begin

              if (TextRect.Bottom > mvMonthDayArray[I].Rec.Bottom)
              and (J <= Pred(EventList.Count))
              then begin
                { draw a little red square with a (...) at the bottom right }
                { corner of the day to indicate that there are more events  }
                { than can be listed in the available space.                }
                RenderCanvas.Brush.Color := DotDotDotColor;
                { draw dot dot dot }
                TPSFillRect (RenderCanvas, Angle, RenderIn,
                             Rect(mvMonthDayArray[I].Rec.Right - 20,
                                     mvMonthDayArray[I].Rec.Bottom - 7,
                                     mvMonthDayArray[I].Rec.Right - 17,
                                     mvMonthDayArray[I].Rec.Bottom - 4));
                TPSFillRect (RenderCanvas, Angle, RenderIn,
                             Rect(mvMonthDayArray[I].Rec.Right - 13,
                                     mvMonthDayArray[I].Rec.Bottom - 7,
                                     mvMonthDayArray[I].Rec.Right - 10,
                                     mvMonthDayArray[I].Rec.Bottom - 4));
                TPSFillRect (RenderCanvas, Angle, RenderIn,
                             Rect(mvMonthDayArray[I].Rec.Right -  6,
                                     mvMonthDayArray[I].Rec.Bottom - 7,
                                     mvMonthDayArray[I].Rec.Right -  3,
                                     mvMonthDayArray[I].Rec.Bottom - 4));
                Break;
              end;

              { shorten events that are next to the day number, in order }
              { to give the day number enough room }
              if (TextRect.Top < mvMonthDayArray[I].Rec.Top
                + mvDayNumberHeight + (TextMargin div 2))
              then
                TextRect.Right := TextRect.Left + mvColWidth
                  - mvDayNumberHeight - TextMargin
              else
                TextRect.Right := TextRect.Left + mvColWidth;

              { format the display text }
              if ShowEventTime then begin
                if (TimeFormat = tf24Hour) then
                  Str := FormatDateTime('hh:mm',
                    TVpEvent(EventList.List^[j]).StartTime)
                else
                  Str := FormatDateTime('hh:mm AM/PM',
                    TVpEvent(EventList.List^[j]).StartTime);
                Str := Str + ' - ' + TVpEvent(EventList.List^[j]).Description;
              end else
                Str := TVpEvent(EventList.List^[j]).Description;

              { set the event font }
              RenderCanvas.Font.Assign(FEventFont);

              StrLn := RenderCanvas.TextWidth(Str);
              if (StrLn > TextRect.Right - TextRect.Left - (TextMargin * 2)) then
              begin
                Str := GetDisplayString(RenderCanvas, Str, 0, TextRect.Right -
                  TextRect.Left - (TextMargin * 2));
              end;

              { write the event text }
              TPSTextOut (RenderCanvas, Angle, RenderIn, TextRect.Left + (TextMargin div 2),
                TextRect.Top + (TextMargin div 2), Str);

              { - begin block}
              Inc(mvVisibleEvents);
              mvEventArray[mvVisibleEvents - 1].Rec := TextRect;
              mvEventArray[mvVisibleEvents - 1].Event := TVpEvent(EventList.List^[j]);
              { - end block}

              { Move TextRect down one line for the next item... }
              TextRect.Top := TextRect.Bottom + 1;
              TextRect.Bottom := TextRect.Top + mvLineHeight;
            end;
          end;
        end;
      finally
        EventList.Free;
      end;
    end;
  end;
  {-}


  procedure DrawBorders;
  begin
    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle,
                                         RenderIn,
                                         Rect (RealLeft,
                                               RealTop,
                                               RealRight - 1,
                                               RealBottom - 1)),
                     BevelShadow,
                     BevelShadow);
    end else if FDrawingStyle = ds3d then begin
    { draw a 3d bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle,
                                         RenderIn,
                                         Rect (RealLeft,
                                               RealTop,
                                               RealRight - 1,
                                               RealBottom - 1)),
                     BevelShadow,
                     BevelHighlight);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle,
                                         RenderIn,
                                         Rect (RealLeft + 1,
                                               RealTop +  1,
                                               RealRight - 2,
                                               RealBottom - 2)),
                     BevelDarkShadow,
                     BevelFace);
    end;
  end;
  {-}
begin
  if DisplayOnly then begin
    BevelHighlight   := clBlack;
    BevelShadow      := clBlack;
    BevelDarkShadow  := clBlack;
    BevelFace        := clBlack;
    RealColor        := clWhite;
    DayHeadAttrColor := clSilver;
    RealLineColor    := clBlack;
    RealOffDayColor  := clSilver;
    RealSelDayColor  := clWhite;
    EventFontColor   := clBlack;
  end else begin
    BevelHighlight   := clBtnHighlight;
    BevelShadow      := clBtnShadow;
    BevelDarkShadow  := cl3DDkShadow;
    BevelFace        := clBtnFace;
    RealColor        := Color;
    DayHeadAttrColor := DayHeadAttributes.Color;
    RealLineColor    := LineColor;
    RealOffDayColor  := OffDayColor;
    RealSelDayColor  := FSelectedDayColor;
    EventFontColor   := FDayNumberFont.Color;
  end;
  DotDotDotColor := clBlack;

  mvPainting := true;
  SavePenStyle := RenderCanvas.Pen.Style;
  SaveBrushColor := RenderCanvas.Brush.Color;
  SavePenColor := RenderCanvas.Pen.Color;

  RenderCanvas.Pen.Style   := psSolid;
  RenderCanvas.Pen.Width   := 1;
  RenderCanvas.Pen.Mode    := pmCopy;
  RenderCanvas.Brush.Style := bsSolid;

  Rgn := CreateRectRgn (RenderIn.Left, RenderIn.Top,
                        RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn (RenderCanvas.Handle, Rgn);

    { clear client area }
    Clear;

    { measure the row heights }
    SetMeasurements;

    { draw headers }
    DrawHeader;
    DrawDayHead;

    { draw days }
    mvVisibleEvents := 0;                                                
    DrawDays;

    { draw the borders }
    DrawBorders;

  finally
    SelectClipRgn (RenderCanvas.Handle, 0);
    DeleteObject (Rgn);
  end;

  { reinstate canvas settings}
  RenderCanvas.Pen.Style := SavePenStyle;
  RenderCanvas.Brush.Color := SaveBrushColor;
  RenderCanvas.Pen.Color := SavePenColor;
  mvPainting := false; 
end;

procedure TVpMonthView.mvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FDate;
end;
{=====}

procedure TVpMonthView.mvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
var
  M, D, Y : Word;
begin
  DecodeDate(Date, Y, M, D);
  if Button = btNext then begin
    if M = 12 then begin
      M := 1;
      Y := Y + 1;
    end else
      M := M + 1;
  end else begin
    if M = 1 then begin
      M := 12;
      Y := Y - 1;
    end else
      M := M - 1;
  end;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpMonthView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpMonthView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpMonthView.SetOffDayColor(Value: TColor);
begin
  if Value <> FOffDayColor then begin
    FOffDayColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetShowEvents(Value: Boolean);
begin
  if FShowEvents <> Value then begin
    FShowEvents := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetEventDayStyle(Value: TFontStyles);
begin
  if FEventDayStyle <> Value then begin
    FEventDayStyle := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDayNameStyle(Value: TVpMVDayNameStyle);
begin
  if FDayNameStyle <> Value then begin
    FDayNameStyle := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDayNumberFont(Value: TFont);
begin
  FDayNumberFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpMonthView.SetEventFont(Value: TFont);
begin
  FEventFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpMonthView.SetSelectedDayColor(Value: TColor);
begin
  if Value <> FSelectedDayColor then begin
    FSelectedDayColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetShowEventTime(Value: Boolean);
begin
  if Value <> FShowEventTime then begin
    FShowEventTime := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpMonthView.SetDate(Value: TDateTime);
begin
  if FDate <> Trunc(Value) then begin
    FDate := Trunc(Value);

    if DataStore <> nil then
      DataStore.Date := FDate;

    if mvLoaded then
      mvPopulate;
    Invalidate;

    if ControlLink <> nil then
      ControlLink.Notify(self, neDateChange, FDate);
  end;
end;
{=====}

procedure TVpMonthView.WMSize(var Msg: TWMSize);
begin
  inherited;
  { force a repaint on resize }
  Invalidate;
end;
{=====}

procedure TVpMonthView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    WindowClass.style := CS_DBLCLKS;
  end;
end;
{=====}

procedure TVpMonthView.CreateWnd;
begin
  inherited;
  mvSpinButtons.Parent := self;
end;
{=====}

procedure TVpMonthView.WMLButtonDown(var Msg : TWMLButtonDown);
begin
  inherited;
  // if the mouse was pressed down in the client area, then select the cell.
  if not focused then SetFocus;

  if (Msg.YPos > mvDayHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    MvSetDateByCoord(Point(Msg.XPos, Msg.YPos));
    { Did the mouse click land on an event? }
    if SelectEventAtCoord(Point(Msg.XPos, Msg.YPos))                     
    and (Assigned(FOnEventClick)) then                                   
        FOnEventClick(self, mvActiveEvent);                              
  end;
end;
{=====}

procedure TVpMonthView.WMLButtonDblClick(var Msg: TWMLButtonDblClk);     
begin                                                                    
  inherited;                                                             
  // if the mouse was pressed down in the client area, then select the   
  // cell.                                                               
  if not focused then SetFocus;                                          
                                                                         
  if (Msg.YPos > mvDayHeadHeight) then                                   
  begin                                                                  
    { The mouse click landed inside the client area }                    
    MvSetDateByCoord(Point(Msg.XPos, Msg.YPos));                         
    { Did the mouse click land on an event? }                            
    if SelectEventAtCoord(Point(Msg.XPos, Msg.YPos))                     
    and (Assigned(FOnEventDblClick)) then                                
      FOnEventDblClick(self, mvActiveEvent);                             
  end;                                                                   
end;                                                                     
{=====}                                                                  

procedure TVpMonthView.WMSetFocus(var Msg : TWMSetFocus);
begin
  // if active event is nil then set active event to the first diaplsyed one.
end;
{=====}

procedure TVpMonthView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{=====}

procedure TVpMonthView.WMRButtonDown(var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;

begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not focused then
      SetFocus;
    if FRightClickChangeDate then                                        
      mvSetDateByCoord (Point (Msg.XPos, Msg.YPos));                     
    ClientOrigin := GetClientOrigin;

    FDefaultPopup.Popup (Msg.XPos + ClientOrigin.x,
                         Msg.YPos + ClientOrigin.y);
  end;
end;
{=====}

procedure TVpMonthView.InitializeDefaultPopup;
var
  NewItem : TMenuItem;

begin
  if RSMonthPopupToday <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSMonthPopupToday;
    NewItem.OnClick := PopupToday;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupNextMonth <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSMonthPopupNextMonth;
    NewItem.OnClick := PopupNextMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupPrevMonth <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSMonthPopupPrevMonth;
    NewItem.OnClick := PopupPrevMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupNextYear <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSMonthPopupNextYear;
    NewItem.OnClick := PopupNextYear;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSMonthPopupPrevYear <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSMonthPopupPrevYear;
    NewItem.OnClick := PopupPrevYear;
    FDefaultPopup.Items.Add (NewItem);
  end;
end;
{=====}

procedure TVpMonthView.PopupToday (Sender : TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpMonthView.PopupNextMonth (Sender : TObject);
begin
  mvSpinButtonClick (self, btNext);
end;
{=====}

procedure TVpMonthView.PopupPrevMonth (Sender : TObject);
begin
  mvSpinButtonClick (self, btPrev);
end;
{=====}

procedure TVpMonthView.PopupNextYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y + 1, M, 1);
end;
{=====}

procedure TVpMonthView.PopupPrevYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y - 1, M, 1);
end;
{=====}

{ - renamed from EditEventAtCoord and re-written}
function TVpMonthView.SelectEventAtCoord(Point: TPoint): Boolean;        
var
  I: Integer;
begin
  result := false;
  I := 0;
  while I < Length(mvEventArray) do begin
    if mvEventArray[I].Event = nil then begin
      Inc(I);
      Break;
    end else begin
      if (Point.X > mvEventArray[I].Rec.Left)
      and (Point.X < mvEventArray[I].Rec.Right)
      and (Point.Y > mvEventArray[I].Rec.Top)
      and (Point.Y < mvEventArray[I].Rec.Bottom) then begin
        result := true;
        Break;
      end else
        Inc(I);
    end;
  end;

  if result then begin
    mvActiveEvent := TVpEvent(mvEventArray[I].Event);
    mvActiveEventRec := mvEventArray[I].Rec;
  end;
end;
{=====}

procedure TVpMonthView.mvSetDateByCoord(Point: TPoint);
var
  I: Integer;
begin
  for I := 0 to pred(Length(mvMonthdayArray)) do begin
    if (Point.X >= mvMonthdayArray[I].Rec.Left)
    and (Point.X <= mvMonthdayArray[I].Rec.Right)
    and (Point.Y >= mvMonthdayArray[I].Rec.Top)
    and (Point.Y <= mvMonthdayArray[I].Rec.Bottom) then
      Date := mvMonthdayArray[I].Date;
  end;
end;
{=====}

procedure TVpMonthView.KeyDown(var Key: Word; Shift: TShiftState);
var
  M, D, Y    : Word;
  PopupPoint : TPoint;

begin
  if FKBNavigate then
    case Key of
      VK_UP    :
        if ssCtrl in Shift then begin
          DecodeDate(Date, Y, M, D);
          Date := EncodeDate(Y - 1, M, 1);
        end else
          Date := Date - 7;
      VK_DOWN  :
        if ssCtrl in Shift then begin
          DecodeDate(Date, Y, M, D);
          Date := EncodeDate(Y + 1, M, 1);
        end else
          Date := Date + 7;
      VK_NEXT  : mvSpinButtonClick(self, btNext);
      VK_PRIOR : mvSpinButtonClick(self, btPrev);
      VK_LEFT  :
        if ssCtrl in Shift then
          mvSpinButtonClick(self, btPrev)
        else
          Date := Date - 1;
      VK_RIGHT :
        if ssCtrl in Shift then
          mvSpinButtonClick(self, btNext)
        else
          Date := Date + 1;
      VK_HOME  : begin
        DecodeDate(Date, Y, M, D);
        if D = 1 then
          mvSpinButtonClick(self, btPrev)
        else
          Date := EncodeDate(Y, M, 1);
      end;
      VK_END   : begin
        DecodeDate(Date, Y, M, D);
        if D = DaysInMonth(Y, M) then begin
          if M = 12 then begin
            M := 1;
            Inc(Y);
          end else
            Inc(M);
        end;
        Date := EncodeDate(Y, M, DaysInMonth(Y, M));
      end;
      VK_TAB   :
        if ssShift in Shift then
          Windows.SetFocus (GetNextDlgTabItem (GetParent (Handle), Handle, False))
        else
          Windows.SetFocus (GetNextDlgTabItem (GetParent (Handle), Handle, True));
      VK_F10   :
        if (ssShift in Shift) and not (Assigned (PopupMenu)) then begin
          PopupPoint := GetClientOrigin;
          FDefaultPopup.Popup (PopupPoint.x + 10,
                               PopupPoint.y + 10);
        end;
      VK_APPS  :
        if not Assigned (PopupMenu) then begin
          PopupPoint := GetClientOrigin;
          FDefaultPopup.Popup (PopupPoint.x + 10,
                               PopupPoint.y + 10);
        end;
    end;
end;
{=====}
procedure TVpMonthView.SetRightClickChangeDate (const v : Boolean);      
begin                                                                    
  if v <> FRightClickChangeDate then                                     
    FRightClickChangeDate := v;                                          
end;                                                                     
{=====}
procedure TVpMonthView.SetWeekStartsOn(Value: TVpDayType);
begin
  if Value <> FWeekStartsOn then begin
    FWeekStartsOn := Value;
    Invalidate;
  end;
end;
{=====}

end.
