{*********************************************************}
{*                 VPWEEKVIEW.PAS 1.03                   *}
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

{
  This unit handles the TVpWeekView component as well as it's inline editor
  and navigation.

  The rendering of Visual PlanIt components is a bit involved.  The component's
  Paint method calls RenderToCanvas.  The RenderToCanvas method of each of
  the visual VisualPlanIt controls is repsonsible both for drawing to the
  screen (both design and run time) as well as printing.  In the case of
  printing, the component needs to render itself to an arbitrary rectangle
  and possibly rotated (for the screen the rectangle is the ClientRect
  and the rotation angle is always zero).  To achieve that goal, the
  functions in VpCanvasUtils are used to go between the rendering of the
  control and the TCanvas that it needs to render to.  
}
{$I Vp.INC}

unit VpWeekView;

interface

uses
  Windows, Classes, Graphics, Controls, ComCtrls, ExtCtrls, Messages, StdCtrls,
  VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst, VpCanvasUtils, Menus,
  VpDayView;

type
  TVpWeekdayRec = packed record
    Rec    : TRect;
    Day    : TDateTime;
  end;

type
  TVpWeekdayArray = array of TVpWeekdayRec;

  { Forward Declarations }
  TVpWeekView = class;

  TVpWvInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Move(const Loc: TRect; Redraw: Boolean);
  end;

  TVpWvHeadAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpWeekView;
    FColor: TColor;
    FFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpWeekView);
    destructor Destroy; override;
    property Owner: TVpWeekView read FOwner;
  published
    property Font: TVpFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpDayHeadAttr = class(TPersistent)
  protected{private}
    FWeekView: TVpWeekView;
    FFont: TFont;
    FDateFormat: string;
    FColor: TColor;
    FBordered: Boolean;
    procedure SetColor (Value: TColor);
    procedure SetFont (Value: TFont);
    procedure SetBordered (Value: Boolean);
    procedure SetDateFormat(Value: string);
  public
    constructor Create(AOwner: TVpWeekView);
    destructor Destroy; override;
    property WeekView: TVpWeekView read FWeekView;
  published
    property Color: TColor read FColor write SetColor;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property Font: TFont read FFont write SetFont;
    property Bordered: Boolean read FBordered write SetBordered;
  end;

  TVpWeekView = class(TVpLinkableControl)
  protected{ private }
    FActiveDate        : TDateTime;
    FColumnWidth       : Integer;
    FColor             : TColor;
    FDateLabelFormat   : string;
    FDayHeadAttributes : TVpDayHeadAttr;
    FDrawingStyle      : TVpDrawingStyle;
    FActiveEvent       : TVpEvent;
    FHeadAttr          : TVpWvHeadAttributes;
    FEventFont         : TFont;
    FLineColor         : TColor;
    FLineCount         : Integer;
    FTimeFormat        : TVpTimeFormat;
    FShowEventTime     : Boolean;
    FVisibleLines      : Integer;
    FWeekStartsOn      : TVpDayType;
    FDefaultPopup      : TPopupMenu;
    FAllDayEventAttr   : TVpAllDayEventAttributes;
    { event variables }
    FBeforeEdit        : TVpBeforeEditEvent;
    FAfterEdit         : TVpAfterEditEvent;
    FOwnerEditEvent    : TVpEditEvent;
    FOnAddEvent        : TVpOnAddNewEvent;                               
    { internal variables }
    wvInLinkHandler    : Boolean;
    wvClickTimer       : TTimer;
    wvLoaded           : Boolean;
    wvRowHeight        : Integer;
    wvDayHeadHeight    : Integer;
    wvHeaderHeight     : Integer;
    wvStartDate        : TDateTime;
    wvSpinButtons      : TUpDown;
    wvEventList        : TList;
    wvEventArray       : TVpEventArray;
    wvWeekdayArray     : TVpWeekdayArray;
    wvActiveEventRec   : TRect;
    wvInPlaceEditor    : TVpWvInPlaceEdit;
    wvCreatingEditor   : Boolean;
    wvPainting         : Boolean;
    wvHotPoint         : TPoint;

    { property methods }
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetDateLabelFormat(Value: string);
    procedure SetEventFont(Value: TFont);
    procedure SetShowEventTime(Value: Boolean);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetActiveDate(Value: TDateTime);
    procedure SetWeekStartsOn(Value: TVpDayType);
    { internal methods }
    procedure wvEditInPlace(Sender: TObject);
    procedure wvHookUp;
    procedure PopupAddEvent (Sender : TObject);
    procedure PopupDeleteEvent (Sender : TObject);
    procedure PopupEditEvent (Sender : TObject);
    procedure PopupToday (Sender : TObject);
    procedure PopupNextWeek (Sender : TObject);
    procedure PopupPrevWeek (Sender : TObject);
    procedure PopupNextMonth (Sender : TObject);
    procedure PopupPrevMonth(Sender : TObject);
    procedure PopupNextYear (Sender : TObject);
    procedure PopupPrevYear (Sender : TObject);
    procedure InitializeDefaultPopup;
    procedure Paint; override;
    procedure Loaded; override;
    procedure wvSpawnEventEditDialog(NewEvent: Boolean);
    procedure wvPopulate;
    procedure wvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function EventAtCoord(Pt: TPoint): Boolean;
    procedure wvSetDateByCoord(Point: TPoint);
    procedure EditEvent;
    procedure EndEdit(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { message handlers }
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;

    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
              message CM_WANTSPECIALKEY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteActiveEvent(Verify: Boolean);
    procedure Invalidate; override;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType;
      const Value: Variant); override;
    function GetControlType : TVpItemType; override;
    procedure EditSelectedEvent;
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
    property ActiveEvent: TVpEvent read FActiveEvent;
    property Date: TDateTime read FActiveDate write SetActiveDate;
    property VisibleLines: Integer read FVisibleLines;
  published
    property AllDayEventAttributes: TVpAllDayEventAttributes
      read FAllDayEventAttr write FAllDayEventAttr;
    {inherited properties}
    property Align;
    property Anchors;
    property TabStop;
    property TabOrder;

    property Color: TColor
      read FColor write SetColor;

    property DateLabelFormat: string
      read FDateLabelFormat write SetDateLabelFormat;

    property DayHeadAttributes: TVpDayHeadAttr
      read FDayHeadAttributes write FDayHeadAttributes;

    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle;

    property EventFont: TFont
      read FEventFont write SetEventFont;

    property HeadAttributes: TVpWvHeadAttributes
      read FHeadAttr write FHeadAttr;

    property LineColor: TColor
      read FLineColor write SetLineColor;

    property TimeFormat: TVpTimeFormat
      read FTimeFormat write SetTimeFormat;

    property ShowEventTime: Boolean
      read FShowEventTime write SetShowEventTime;

    property WeekStartsOn: TVpDayType
      read FWeekStartsOn write SetWeekStartsOn;

    {events}
    property AfterEdit : TVpAfterEditEvent
      read FAfterEdit write FAfterEdit;

    property BeforeEdit: TVpBeforeEditEvent
      read FBeforeEdit write FBeforeEdit;

    property OnAddEvent: TVpOnAddNewEvent                                
      read FOnAddEvent write FOnAddEvent;                                

    property OnOwnerEditEvent: TVpEditEvent
      read FOwnerEditEvent write FOwnerEditEvent;

  end;

implementation

uses
  SysUtils, Math, Forms, Dialogs, VpEvntEditDlg;

(*****************************************************************************)
{ TVpTGInPlaceEdit }

constructor TVpWvInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  {$IFDEF VERSION4}
  DoubleBuffered := False;
  {$ENDIF}
end;
{=====}

procedure TVpWvInPlaceEdit.Move(const Loc: TRect; Redraw: Boolean);
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible(Handle);
  with Loc do
    SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
      SWP_SHOWWINDOW or SWP_NOREDRAW);
  if Redraw then Invalidate;
  SetFocus;
end;
{=====}

procedure TVpWvInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;
{=====}

procedure TVpWvInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Grid : TVpWeekView;
begin
  Grid := TVpWeekView(Owner);

  case Key of
  VK_RETURN: begin
    Key := 0;
    Grid.EndEdit(Self);
  end;

  VK_UP: begin
    Key := 0;
    Grid.EndEdit(Self);
  end;

  VK_DOWN: begin
    Key := 0;
    Grid.EndEdit(Self);
  end;

  VK_ESCAPE: begin
    Key := 0;
    Grid.EndEdit(self);
  end;

  else
    inherited;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpContactHeadAttr }
constructor TVpDayHeadAttr.Create(AOwner: TVpWeekView);
begin
  inherited Create;
  FWeekView := AOwner;
  FDateFormat := 'dddd mmmm, dd';
  FFont := TFont.Create;
  FFont.Assign(FWeekView.Font);
  FFont.Size := 8;
  FColor := clSilver;
  FBordered := true;
end;
{=====}

destructor TVpDayHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpDayHeadAttr.SetBordered(Value: Boolean);
begin
  if Value <> FBordered then begin
    FBordered := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetDateFormat(Value: string);
begin
  if Value <> FDateFormat then begin
    FDateFormat := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetFont(Value: TFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    WeekView.Invalidate;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpWeekView }

constructor TVpWeekView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  { Create internal classes and stuff }
  FDayHeadAttributes := TVpDayHeadAttr.Create(self);
  FHeadAttr := TVpWvHeadAttributes.Create(self);
  FAllDayEventAttr := TVpAllDayEventAttributes.Create (self);

  FEventFont := TFont.Create;
  FEventFont.Assign(Font);
  FShowEventTime := true;
  wvInLinkHandler := false;
  wvEventList := TList.Create;
  wvClickTimer := TTimer.Create(self);
  wvSpinButtons := TUpDown.Create(self);
  wvSpinButtons.OnClick := wvSpinButtonClick;
  wvSpinButtons.Orientation := udHorizontal;
  wvSpinButtons.Min := -32768;
  wvSpinButtons.Max := 32767;
  wvHotPoint := Point(0, 0);

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered := true;
  {$ENDIF}

  FWeekStartsOn := dtMonday;
  wvClickTimer.Enabled := false;
  wvClickTimer.Interval := ClickDelay;
  wvClickTimer.OnTimer := wvEditInPlace;
  wvCreatingEditor := false;
  FDrawingStyle := ds3d;
  wvPainting := false;
  FColor := clWindow;
  FLineColor := clGray;
  FActiveDate := Now;
  wvStartDate := trunc(GetStartOfWeek(Now, FWeekStartsOn));
  FTimeFormat := tf12Hour;
  FDateLabelFormat := 'dddd, mmmm dd, yyyy';
  FColumnWidth := 200;

  { set up fonts and colors }
  FDayHeadAttributes.Font.Name := 'Tahoma';
  FDayHeadAttributes.Font.Size := 10;
  FDayHeadAttributes.Font.Style := [];
  FDayHeadAttributes.Color := clBtnFace;
  FDayHeadAttributes.Bordered := true;

  SetLength(wvEventArray, MaxVisibleEvents);
  SetLength(wvWeekdayArray, 7);

  { size }
  Height := 225;
  Width := 300;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  FAllDayEventAttr.BackgroundColor := Color;
  FAllDayEventAttr.EventBackgroundColor := clBtnFace;
  FAllDayEventAttr.EventBorderColor := LineColor;
  FAllDayEventAttr.Font.Assign (Font);

  wvHookUp;
end;
{=====}

destructor TVpWeekView.Destroy;
begin
  FDayHeadAttributes.Free;
  FAllDayEventAttr.Free;
  FHeadAttr.Free;
  wvClickTimer.Free;
  FEventFont.Free;
  wvSpinButtons.Free;
  wvEventList.Free;
  FDefaultPopup.Free;
  inherited;
end;
{=====}

procedure TVpWeekView.Invalidate;
begin
  inherited;
end;
{=====}

procedure TVpWeekView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  wvInLinkHandler := true;
  try
    case NotificationType of
      neDateChange: begin
        Date := Value;
      end;
      neDataStoreChange: Invalidate;
      neInvalidate: Invalidate;
    end;
  finally
    wvInLinkHandler := false;
  end;
end;
{=====}

procedure TVpWeekView.wvHookUp;
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

procedure TVpWeekView.Loaded;
begin
  inherited;
  wvLoaded := true;
  wvPopulate;
end;
{=====}

function TVpWeekView.GetControlType : TVpItemType;
begin
  Result := itWeekView;
end;

procedure TVpWeekView.Paint;
begin
  RenderToCanvas (Canvas,                      // Paint Canvas
                  Rect (0, 0, Width, Height),  // Paint Rectangle
                  ra0,
                  1,                           // Scale
                  wvStartDate,                 // Date
                  -1,                          // Start At
                  -1,                          // End At
                  gr30Min,
                  False);                      // Display Only
end;
{=====}
procedure TVpWeekView.PaintToCanvas (ACanvas : TCanvas;
                                      ARect   : TRect;
                                      Angle   : TVpRotationAngle;
                                      ADate   : TDateTime);
begin
  RenderToCanvas (ACanvas, ARect, Angle, 1, ADate,
                  -1, -1, gr30Min, True);
end;
{=====}

procedure TVpWeekView.RenderToCanvas (RenderCanvas : TCanvas;
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
  DayRectHeight  : Integer;
  StrLn          : Integer;
  StartDate      : TDateTime;
  RealWidth      : Integer;
  RealHeight     : Integer;
  RealLeft       : Integer;
  RealRight      : Integer;
  RealTop        : Integer;
  RealBottom     : Integer;
  ADEventsRect   : TRect;
  Rgn            : HRGN;

  DotDotDotColor         : TColor;
  BevelHighlightColor    : TColor;
  BevelShadowColor       : TColor;
  BevelDarkShadow        : TColor;
  BevelButtonFace        : TColor;
  RealLineColor          : TColor;
  RealDayHeadAttrColor   : TColor;
  RealColor              : TColor;
  RealHeadAttrColor      : TColor;
  ADBackgroundColor      : TColor;
  ADEventBackgroundColor : TColor;
  ADEventBorderColor     : TColor;

  function DrawAllDayEvents (    ADate   : TDateTime;
                                 DayRect : TRect;
                             var EAIndex : Integer) : Boolean;
  var
    ADEventsList       : TList;
    TempList           : TList;
    I, J, K            : Integer;
    Event              : TVpEvent;
    ADEventRect        : TRect;
    StartsBeforeRange  : Boolean;
    MaxADEvents        : Integer;
    Skip               : Boolean;
    ADTextHeight       : Integer;
    EventStr           : string;

  begin
    Result := False;
    { initialize the All Day Events area... }
    ADEventsRect := DayRect;

    if (DataStore = nil) or (DataStore.Resource = nil) then
      Exit;

    { Collect all of the events for this range and determine the maximum     }
    { number of all day events for the range of days covered by the control. }
    MaxADEvents := 0;

    ADEventsList := TList.Create;
    try
      TempList := TList.Create;
      try
        { get the all day events for the day specified by ADate + I }
        DataStore.Resource.Schedule.AllDayEventsByDate(ADate, TempList);

        { Iterate through these events and place them in ADEventsList    }
        Skip := false;
        for J := 0 to pred(TempList.Count) do begin
          if AdEventsList.Count > 0 then begin
            for K := 0 to pred(AdEventsList.Count) do begin
              if TVpEvent(AdEventsList[K]) = TVpEvent(TempList[J]) then begin
                Skip := true;
                Break;
              end;
            end;
            if not Skip then
              AdEventsList.Add(TempList[J]);
          end else
            AdEventsList.Add(TempList[J]);
        end;

        if TempList.Count > MaxADEvents then
          MaxADEvents := TempList.Count;
      finally
        TempList.Free;
      end;

      if MaxADEvents > 0 then begin
        { Set attributes }
        RenderCanvas.Brush.Color := ADBackgroundColor;
        RenderCanvas.Font.Assign (AllDayEventAttributes.Font);

        { Measure the AllDayEvent TextHeight }
        ADTextHeight := RenderCanvas.TextHeight(VpProductName) +
                        TextMargin + TextMargin div 2;

        { Build the AllDayEvent rect based on the value of MaxADEvents }
        if AdEventsRect.Top + (MaxADEvents * ADTextHeight) +
           TextMargin * 2 > DayRect.Bottom then
          ADeventsrect.Bottom := DayRect.Bottom
        else
          ADEventsRect.Bottom := AdEventsRect.Top +
                                 (MaxADEvents * ADTextHeight) + TextMargin * 2;

        { Clear the AllDayEvents area }
        TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

        StartsBeforeRange  := false;
        { Cycle through the all day events and draw them appropriately }
        for I := 0 to pred(ADEventsList.Count) do begin

          Event := ADEventsList[I];

          { set the top of the event's rect }
          AdEventRect.Top := ADEventsRect.Top + TextMargin +
                             (I  * ADTextHeight);

          if ADEventsRect.Top + TextMargin + ((I + 1)  * ADTextHeight) -
             TextMargin > DayRect.Bottom then begin
            RenderCanvas.Brush.Color := DotDotDotColor;
            { draw dot dot dot }
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right - 20,  DayRect.Bottom - 7,
                               DayRect.Right - 17,  DayRect.Bottom - 4));
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right - 13,  DayRect.Bottom - 7,
                               DayRect.Right - 10,  DayRect.Bottom - 4));
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right -  6,  DayRect.Bottom - 7,
                               DayRect.Right -  3,  DayRect.Bottom - 4));
            break;
          end;

          { see if the event began before the start of the range }
          if (Event.StartTime < trunc(RenderDate)) then
            StartsBeforeRange := true;

          AdEventRect.Bottom := ADEventRect.Top + ADTextHeight;
          AdEventRect.Left := AdEventsRect.Left + (TextMargin div 2);
          AdEventRect.Right := DayRect.Right;

          if (StartsBeforeRange) then
            EventStr := '>> '
          else
            EventStr := '';

          EventStr := EventStr + Event.Description;

          RenderCanvas.Brush.Color := ADEventBackgroundColor;
          RenderCanvas.Pen.Color := ADEventBorderColor;
          TPSRectangle (RenderCanvas, Angle, RenderIn,
                        ADEventRect.Left + TextMargin,
                        ADEventRect.Top + TextMargin div 2,
                        ADEventRect.Right - TextMargin,
                        ADEventRect.Top + ADTextHeight + TextMargin div 2);
          TPSTextOut (RenderCanvas,Angle, RenderIn,
                      AdEventRect.Left + TextMargin * 2 + TextMargin div 2,
                      AdEventRect.Top + TextMargin,
                      EventStr);
          Result := True;
          wvEventArray[EAIndex].Rec := Rect (ADEventRect.Left + TextMargin,
                                             ADEventRect.Top + TextMargin,
                                             ADEventRect.Right - TextMargin,
                                             ADEventRect.Bottom);
          wvEventArray[EAIndex].Event := Event;
          Inc (EAIndex);
        end; { for I := 0 to pred(ADEventsList.Count) do ... }

      end;   { if MaxADEvents > 0 }

    finally
      ADEventsList.Free;
    end;
  end;
    
  procedure DrawDays;
  var
    DayRect  : TRect;
    TextRect : TRect;
    I, J, SL : Integer;
    EAIndex  : Integer;
    DayStr   : string;
    EventList: TList;
  begin
    RenderCanvas.Pen.Color := RealLineColor;
    RenderCanvas.Pen.Style := psSolid;
    { initialize WeekdayArray }
    for I := 0 to pred(Length(wvWeekdayArray)) do begin
      wvWeekdayArray[I].Rec.TopLeft := Point(-1, -1);
      wvWeekdayArray[I].Rec.BottomRight := Point(-1, -1);
      wvWeekdayArray[I].Day := 0;
    end;

    { initialize Event Array }
    EAIndex := 0;
    for I := 0 to pred(Length(wvEventArray)) do begin
      wvEventArray[I].Rec.TopLeft := Point(-1, -1);
      wvEventArray[I].Rec.BottomRight := Point(-1, -1);
      wvEventArray[I].Event := nil;
    end;

    RenderCanvas.Pen.Color := RealLineColor;
    { build the first dayrect }
    DayRectHeight := (RealBottom - RealTop - wvHeaderHeight) div 3;
    if DrawingStyle = ds3D then
      DayRect.TopLeft := Point (RealLeft + 1,
                                RealTop + wvHeaderHeight + 3)
    else
      DayRect.TopLeft := Point (RealLeft + 1,
                                RealTop + wvHeaderHeight + 2);
    DayRect.BottomRight := Point ((RealLeft + (RealRight - RealLeft) div 2) + 1,
                                  RealTop + wvHeaderHeight + DayRectHeight);
    { draw the day frames }
    for I := 0 to 6 do begin
      { draw day head}
      RenderCanvas.Font.Assign(FDayHeadAttributes.Font);
      RenderCanvas.Brush.Color := RealDayHeadAttrColor;
      TextRect := Rect(DayRect.Left, DayRect.Top, DayRect.Right, DayRect.Top
        + wvDayHeadHeight);
      TPSFillRect (RenderCanvas, Angle, RenderIn, TextRect);
      if FDayHeadAttributes.Bordered then
        TPSRectangle (RenderCanvas, Angle, RenderIn, TextRect);
      { Fix Header String }
      DayStr := FormatDateTime(FDayHeadAttributes.DateFormat, StartDate + I);
      SL := RenderCanvas.TextWidth(DayStr);
      if SL > TextRect.Right - TextRect.Left then begin
        DayStr := GetDisplayString(RenderCanvas, DayStr, 0, TextRect.Right -
        TextRect.Left - TextMargin);
      end;
      SL := RenderCanvas.TextWidth(DayStr);
      TextRect.Left := TextRect.Right - SL - TextMargin;
      TPSTextOut (RenderCanvas, Angle, RenderIn,
                  TextRect.Left, TextRect.Top + TextMargin - 1, DayStr);

      if (DataStore <> nil) and (DataStore.Resource <> nil)
      and (DataStore.Resource.Schedule.EventCountByDay(StartDate + I) > 0)
      and (DayRect.Bottom - DayRect.Top >= (TextMargin * 2) + wvDayHeadHeight) then
      begin
        { events exist for this day }
        EventList := TList.Create;
        try
          { populate the eventlist with events for this day }
          DataStore.Resource.Schedule.EventsByDate(StartDate + I, EventList);
          { initialize TextRect for this day }
          TextRect.TopLeft := Point (DayRect.Left,
                                     DayRect.Top + wvDayHeadHeight);
          TextRect.BottomRight := Point (DayRect.Right,
                                         TextRect.Top + wvRowHeight);

          { Handle All Day Events }
          if DrawAllDayEvents (StartDate + I,
                               Rect (TextRect.Left,
                                     TextRect.Top,
                                     TextRect.Right,
                                     DayRect.Bottom),
                               EAIndex) then begin
            TextRect.Bottom := TextRect.Bottom + (ADEventsRect.Bottom - TextRect.Top);
            TextRect.Top := ADEventsRect.Bottom;
          end;

          { Discard AllDayEvents, because they are drawn above. }
          for J := pred(EventList.Count) downto 0 do
            if TVpEvent (EventList[J]).AllDayEvent then
              EventList.Delete(J);

          { iterate the events, painting them one by one }
          for J := 0 to pred(EventList.Count) do begin
            { if the TextRect extends below the available space then draw a   }
            { dot dot dot to indicate there are more events than can be drawn }
            { in the available space }
            if TextRect.Bottom - TextMargin > DayRect.Bottom then begin
              RenderCanvas.Brush.Color := DotDotDotColor;
              { draw dot dot dot }
              TPSFillRect (RenderCanvas, Angle, RenderIn,
                           Rect (DayRect.Right - 20,  DayRect.Bottom - 7,
                                 DayRect.Right - 17,  DayRect.Bottom - 4));
              TPSFillRect (RenderCanvas, Angle, RenderIn,
                           Rect (DayRect.Right - 13,  DayRect.Bottom - 7,
                                 DayRect.Right - 10,  DayRect.Bottom - 4));
              TPSFillRect (RenderCanvas, Angle, RenderIn,
                           Rect (DayRect.Right -  6,  DayRect.Bottom - 7,
                                 DayRect.Right -  3,  DayRect.Bottom - 4));
              break;
            end;

            { format the display text }
            DayStr := '';
            if ShowEventTime then begin
              if TimeFormat = tf24Hour then
                DayStr := FormatDateTime('hh:mm',
                  TVpEvent(EventList.List^[j]).StartTime)
                  + ' - ' + FormatDateTime('hh:mm',
                  TVpEvent(EventList.List^[j]).EndTime) + ': '
              else
                DayStr := FormatDateTime('hh:mm AM/PM',
                  TVpEvent(EventList.List^[j]).StartTime)
                  + ' - ' + FormatDateTime('hh:mm AM/PM',
                  TVpEvent(EventList.List^[j]).EndTime) + ': ';
            end;

            if DayStr = '' then
              DayStr := TVpEvent(EventList.List^[j]).Description
            else
              DayStr := DayStr + ' '
                + TVpEvent(EventList.List^[j]).Description;

            { set the event font }
            RenderCanvas.Font.Assign(FEventFont);
            RenderCanvas.Brush.Color := RealColor;

            StrLn := RenderCanvas.TextWidth(DayStr);
            if (StrLn > TextRect.Right - TextRect.Left - TextMargin) then
            begin
              DayStr := GetDisplayString(RenderCanvas, DayStr, 0, TextRect.Right -
                TextRect.Left - (TextMargin * 2));
            end;

            { write the event text }
            TPSTextOut (RenderCanvas, Angle, RenderIn,
                        TextRect.Left + TextMargin,
                        TextRect.Top + (TextMargin div 2), DayStr);

            { update the EventArray }
            wvEventArray[EAIndex].Rec := TextRect;
            wvEventArray[EAIndex].Event := TVpEvent(EventList.List^[j]);
            Inc(EAIndex);

            TextRect.Top := TextRect.Bottom;
            TextRect.Bottom := TextRect.Top + wvRowHeight;
          end; { for loop }
        finally
          EventList.Free;
        end;
      end;

      { Draw focus rect if this is the current day }

      if (not DisplayOnly) and                                           
         (StartDate + I = Trunc (FActiveDate)) and                       
         (Focused) then                                                  
        TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                          Rect (DayRect.Left + 2,
                                DayRect.Top + wvDayHeadHeight + 2,
                                DayRect.Right - 2,
                                DayRect.Bottom - 2));

      { update WeekdayArray }
      wvWeekdayArray[I].Rec := DayRect;
      wvWeekdayArray[I].Day := StartDate + I;
      { adjust the DayRect for the next day }
      if (I = 2) then begin
        { move the dayrect to the top of the next column }
        if DrawingStyle = ds3D then begin
          DayRect.TopLeft := Point (RealLeft + (RealRight - RealLeft) div 2,
                                    RealTop + wvHeaderHeight + 3);
          DayRect.BottomRight := Point (RealRight - 2,
                                        RealTop + wvHeaderHeight + DayRectHeight);
        end
        else begin
          DayRect.TopLeft := Point (RealLeft + (RealRight - RealLeft) div 2,
                                    RealTop + wvHeaderHeight + 2);
          DayRect.BottomRight := Point (RealRight - 1,
                                        RealTop + wvHeaderHeight + DayRectHeight);
        end;
      end

      else if (I = 4 {Friday}) then begin
        { shrink DayRect for weekend days }
        DayRectHeight := DayRectHeight div 2;
        DayRect.Top := DayRect.Bottom;
        DayRect.Bottom := DayRect.Top + DayRectHeight;
      end
      else begin
        DayRect.Top := DayRect.Bottom;
        DayRect.Bottom := DayRect.Top + DayRectHeight;
      end;

    end;

    { Draw the center vertical line }
    RenderCanvas.Pen.Color := RealLineColor;
    TPSMoveTo (RenderCanvas, Angle, RenderIn,
               RealLeft + (RealRight - RealLeft) div 2,
                         RealTop + wvHeaderHeight + 2);
    TPSLineTo (RenderCanvas, Angle, RenderIn,
               RealLeft + (RealRight - RealLeft) div 2,
                         RealBottom - 1);

    if (DataStore = nil)
    or (DataStore.Resource = nil)
    or (DataStore.Resource.Tasks.Count = 0)
    then Exit;
  end;
  {-}

  procedure Clear;
  begin
    RenderCanvas.Brush.Color := RealColor;
    RenderCanvas.FillRect (RenderIn);
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
      StartDate := GetStartOfWeek (wvStartDate, FWeekStartsOn)
    else
      StartDate := GetStartOfWeek (RenderDate, FWeekStartsOn);

    RenderCanvas.Font.Assign(FDayHeadAttributes.Font);
    wvDayHeadHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + 2 ;
    RenderCanvas.Font.Assign(FEventFont);
    wvRowHeight := RenderCanvas.TextHeight(VpProductName) + (TextMargin div 2);
    RenderCanvas.Font.Assign(TFont(FHeadAttr.Font));
    wvHeaderHeight := RenderCanvas.TextHeight(VpProductName) + (TextMargin * 2);
  end;
  {-}

  procedure DrawHeader;
  var
    HeadTextRect: TRect;
    HeadStr: string;
    HeadStrLen : Integer;
  begin
    RenderCanvas.Brush.Color := RealHeadAttrColor;
    RenderCanvas.Font.Assign(TFont(FHeadAttr.Font));
    { draw the header cell and borders }
    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      HeadRect.Left := RealLeft + 1;
      HeadRect.Top := RealTop + 1;
      HeadRect.Right := RealRight - 1;
      HeadRect.Bottom := HeadRect.Top + wvHeaderHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, HeadRect),
                     BevelHighlightColor, BevelShadowColor);
    end else if FDrawingStyle = ds3d then begin
      { draw a 3d bevel }
      HeadRect.Left := RealLeft + 2;
      HeadRect.Top := RealTop + 2;
      HeadRect.Right := RealRight - 3;
      HeadRect.Bottom := RealTop + wvHeaderHeight;
      TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn, HeadRect),
                     BevelHighlightColor, BevelDarkShadow);
    end;
    { build header caption }
    HeadStr := HeadStr + RSWeekof + ' '
      + FormatDateTime(DateLabelFormat, StartDate);
    { draw the text }
    if (DisplayOnly) and
       (RenderCanvas.TextWidth (HeadStr) >= RenderIn.Right - RenderIn.Left) then
      HeadTextRect.TopLeft:= Point (RealLeft + TextMargin * 2,
                                    HeadRect.Top)
    else if DisplayOnly then
      HeadTextRect.TopLeft := Point (RealLeft + (RealRight - RealLeft -
                                     RenderCanvas.TextWidth (HeadStr)) div 2,
                                     HeadRect.Top)
    else
      HeadTextRect.TopLeft := Point (RealLeft + 30 + TextMargin * 2,
                                     HeadRect.Top);
    HeadTextRect.BottomRight := HeadRect.BottomRight;
    { Fix Header String }
    HeadStrLen := RenderCanvas.TextWidth(HeadStr);
    if HeadStrLen > HeadTextRect.Right - HeadTextRect.Left - TextMargin then
    begin
      HeadStr := GetDisplayString(RenderCanvas, HeadStr, 0,
        HeadTextRect.Right - HeadTextRect.Left - TextMargin );
    end;
    { position the spinner }
    wvSpinButtons.Height := Trunc(wvHeaderHeight * 0.8);
    wvSpinButtons.Width := wvSpinButtons.Height * 2;
    wvSpinButtons.Left := TextMargin;
    wvSpinButtons.Top := (wvHeaderHeight - wvSpinButtons.Height) div 2 + 2;
    TPSTextOut (RenderCanvas, Angle, RenderIn, HeadTextRect.Left + TextMargin,
      HeadTextRect.Top + TextMargin, HeadStr);
  end;
  {-}

  procedure DrawBorders;
  begin
    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft, RealTop,
                                               RealRight - 1, RealBottom - 1)),
                     BevelShadowColor,
                     BevelHighlightColor);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1, RealTop + 1,
                                               RealRight - 2, RealBottom - 2)),
                     BevelShadowColor,
                     BevelHighlightColor);
    end else if FDrawingStyle = ds3d then begin
    { draw a 3d bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft, RealTop,
                                               RealRight - 1, RealBottom - 1)),
                     BevelShadowColor,
                     BevelShadowColor);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1, RealTop + 1,
                                               RealRight - 2, RealBottom - 2)),
                     BevelDarkShadow,
                     BevelButtonFace);
    end;
  end;
  {-}
begin

  if DisplayOnly then begin
    BevelHighlightColor    := clBlack;
    BevelShadowColor       := clBlack;
    BevelDarkShadow        := clBlack;
    BevelButtonFace        := clBlack;
    RealLineColor          := clBlack;
    RealColor              := clWhite;
    RealDayHeadAttrColor   := clSilver;
    RealHeadAttrColor      := clSilver;
    ADBackgroundColor      := clWhite;
    ADEventBackgroundColor := clWhite;
    ADEventBorderColor     := clSilver;
  end else begin
    BevelHighlightColor    := clBtnHighlight;
    BevelShadowColor       := clBtnShadow;
    BevelDarkShadow        := cl3DDkShadow;
    BevelButtonFace        := clBtnFace;
    RealLineColor          := LineColor;
    RealColor              := Color;
    RealDayHeadAttrColor   := FDayHeadAttributes.Color;
    RealHeadAttrColor      := FHeadAttr.Color;
    ADBackgroundColor      := AllDayEventAttributes.BackgroundColor;
    ADEventBackgroundColor := AllDayEventAttributes.EventBackgroundColor;
    ADEventBorderColor     := AllDayEventAttributes.EventBorderColor;
  end;
  DotDotDotColor           := clBlack;

  wvPainting := true;
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

    { draw header }
    DrawHeader;

    { draw days }
    DrawDays;

    { draw the borders }
    DrawBorders;

    { reinstate canvas settings}

  finally
    SelectClipRgn (RenderCanvas.Handle, 0);
    DeleteObject (Rgn);
  end;

  RenderCanvas.Pen.Style := SavePenStyle;
  RenderCanvas.Brush.Color := SaveBrushColor;
  RenderCanvas.Pen.Color := SavePenColor;
  wvPainting := false;
end;

{=====}

procedure TVpWeekView.wvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FActiveDate;
end;
{=====}

procedure TVpWeekView.DeleteActiveEvent(Verify: Boolean);
var
  Str: string;
  DoIt: Boolean;
begin
  DoIt := not Verify;

  if FActiveEvent <> nil then begin
    Str := '"' + FActiveEvent.Description + '"';

    if Verify then
      DoIt := (MessageDlg(RSDelete + ' ' + Str + ' ' + RSFromSchedule
        + #13#10#10 + RSPermanent, mtconfirmation,
        [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveEvent.Deleted := true;
      FActiveEvent := nil;
      DataStore.PostEvents;
      Invalidate;
    end;
  end;
end;
{=====}


procedure TVpWeekView.wvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then
    Date := Date + 7
  else
    Date := Date - 7;
end;
{=====}

procedure TVpWeekView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpWeekView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpWeekView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetEventFont(Value: TFont);
begin
  FEventFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpWeekView.SetShowEventTime(Value: Boolean);
begin
  if Value <> FShowEventTIme then begin
    FShowEventTime := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetActiveDate(Value: TDateTime);
begin
  if FActiveDate <> Value then begin
    FActiveDate := Value;

    if (Value < wvStartDate) or (Value >= wvStartDate + 7) then
      wvStartDate := Trunc(GetStartOfWeek(Value, FWeekStartsOn));

    if wvStartDate > Value then
      wvStartDate := wvStartDate - 7;

    if wvLoaded then
      wvPopulate;

    Invalidate;

    if (not wvInLinkHandler) and (ControlLink <> nil) then
      ControlLink.Notify(self, neDateChange, FActiveDate);
  end;
end;
{=====}

procedure TVpWeekView.SetWeekStartsOn(Value: TVpDayType);
begin
  if FWeekStartsOn <> Value then begin
    FWeekStartsOn := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.WMSize(var Msg: TWMSize);
begin
  inherited;
  { force a repaint on resize }
  Invalidate;
end;
{=====}

procedure TVpWeekView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    WindowClass.style := CS_DBLCLKS;
  end;
end;
{=====}

procedure TVpWeekView.CreateWnd;
begin
  inherited;
  wvSpinButtons.Parent := self;
end;
{=====}

procedure TVpWeekView.WMLButtonDown(var Msg : TWMLButtonDown);
begin
  inherited;

  if not Focused then SetFocus; 

  if wvInPlaceEditor <> nil then
    EndEdit(Self);

  if (Msg.YPos > wvHeaderHeight) then
  begin
    { If an active event was clicked, then enable the click timer.  If the }
    { item is double clicked before the click timer fires, then the edit   }
    { dialog will appear, otherwise the in-place editor will appear.       }
    if EventAtCoord(Point(Msg.XPos, Msg.YPos)) then
      wvClickTimer.Enabled := true;

    { The mouse click landed inside the client area }
    wvSetDateByCoord(Point(Msg.XPos, Msg.YPos));
  end;
end;
{=====}

procedure TVpWeekView.WMLButtonDblClk(var Msg : TWMLButtonDblClk);
var
  StartTime, EndTime: TDateTime;
begin
  inherited;
  wvClickTimer.Enabled := false;

  if not CheckCreateResource then                                      
    Exit;                                                              

  if DataStore = nil then
    Exit;

  // if the mouse was pressed down in the client area, then select the cell.
  if not focused then SetFocus; 

  if (Msg.YPos > wvHeaderHeight) then
  begin
    { The mouse click landed inside the client area }
    { If we have hit an active event then we must want to edit it }
    if FActiveEvent <> nil then begin
      { edit this event }
      wvSpawnEventEditDialog(False);
    end
    else if (DataStore.Resource <> nil) then begin
      { otherwise, we must want to create a new event }
      StartTime := trunc(Date) + 1 / 2; { default to 12:00 noon }
      EndTime := StartTime + (30 / MinutesInDay); { StartTime + 30 minutes }
      FActiveEvent := DataStore.Resource.Schedule.AddEvent(
        DataStore.GetNextID('Events'), StartTime, EndTime);
      { edit this new event }
      wvSpawnEventEditDialog(True);
    end;
  end;
end;
{=====}

procedure TVpWeekView.WMRButtonDown(var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;
  i            : Integer;

begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not focused then 
      SetFocus;
    { The mouse click landed inside the client area }
    wvSetDateByCoord(Point(Msg.XPos, Msg.YPos));
    EventAtCoord (Point (Msg.XPos, Msg.YPos));
    wvClickTimer.Enabled := false;
    ClientOrigin := GetClientOrigin;

    if not Assigned (FActiveEvent) then
      for i := 0 to FDefaultPopup.Items.Count - 1 do begin
        if (FDefaultPopup.Items[i].Tag = 1) or (ReadOnly) then           
          FDefaultPopup.Items[i].Enabled := False;
      end
    else
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;

    FDefaultPopup.Popup (Msg.XPos + ClientOrigin.x,
                         Msg.YPos + ClientOrigin.y);
  end;
end;
{=====}

procedure TVpWeekView.InitializeDefaultPopup;
var
  NewItem    : TMenuItem;
  NewSubItem : TMenuItem;

begin
  if RSWeekPopupAdd <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSWeekPopupAdd;
    NewItem.OnClick := PopupAddEvent;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSWeekPopupEdit <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSWeekPopupEdit;
    NewItem.OnClick := PopupEditEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSWeekPopupDelete <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSWeekPopupDelete;
    NewItem.OnClick := PopupDeleteEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSWeekPopupNav <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSWeekPopupNav;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);

    if RSWeekPopupNavToday <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavToday;
      NewSubItem.OnClick := PopupToday;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavNextWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavNextWeek;
      NewSubItem.OnClick := PopupNextWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavPrevWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavPrevWeek;
      NewSubItem.OnClick := PopupPrevWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavNextMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavNextMonth;
      NewSubItem.OnClick := PopupNextMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavPrevMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavPrevMonth;
      NewSubItem.OnClick := PopupPrevMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavNextYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavNextYear;
      NewSubItem.OnClick := PopupNextYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSWeekPopupNavPrevYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSWeekPopupNavPrevYear;
      NewSubItem.OnClick := PopupPrevYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;
  end;
end;
{=====}

procedure TVpWeekView.PopupAddEvent (Sender : TObject);
var
  StartTime : TDateTime;
  EndTime   : TDateTime;

begin
  if ReadOnly then                                                     
    Exit;                                                              
  if not CheckCreateResource then                                      
    Exit;                                                              
  if not Assigned (DataStore) then                                     
    Exit;                                                              
  if not Assigned (DataStore.Resource) then                            
    Exit;                                                              
  StartTime := trunc(Date) + 1 / 2; { default to 12:00 noon }
  EndTime := StartTime + (30 / MinutesInDay); { StartTime + 30 minutes }
  FActiveEvent := DataStore.Resource.Schedule.AddEvent (
                      DataStore.GetNextID ('Events'), StartTime, EndTime);
  { edit this new event }
  wvSpawnEventEditDialog (True);
end;
{=====}

procedure TVpWeekView.PopupDeleteEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveEvent <> nil then
    DeleteActiveEvent (True);
end;
{=====}

procedure TVpWeekView.PopupEditEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveEvent <> nil then
    { edit this Event }
    wvSpawnEventEditDialog(False);
end;
{=====}

procedure TVpWeekView.EditSelectedEvent;
begin
  if FActiveEvent <> nil then
    wvSpawnEventEditDialog(false);
end;
{=====}

procedure TVpWeekView.PopupToday (Sender : TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpWeekView.PopupNextWeek (Sender : TObject);
begin
  Date := Date + 7;
end;
{=====}

procedure TVpWeekView.PopupPrevWeek (Sender : TObject);
begin
  Date := Date - 7;
end;
{=====}

procedure TVpWeekView.PopupNextMonth (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate(Date, Y, M, D);
  if M = 12 then begin
    M := 1;
    Y := Y + 1;
  end else
    M := M + 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpWeekView.PopupPrevMonth(Sender : TObject);
var
  M, D, Y : Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 1 then begin
    M := 12;
    Y := Y - 1;
  end else
    M := M - 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpWeekView.PopupNextYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y + 1, M, 1);
end;
{=====}

procedure TVpWeekView.PopupPrevYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y - 1, M, 1);
end;
{=====}

procedure TVpWeekView.wvSpawnEventEditDialog(NewEvent: Boolean);
var
  AllowIt: Boolean;
  EventDlg : TVpEventEditDialog;
begin
  if DataStore = nil then Exit;

  AllowIt := false;
  if Assigned(FOwnerEditEvent) then
    FOwnerEditEvent(self, FActiveEvent, DataStore.Resource, AllowIt)
  else begin
    EventDlg := TVpEventEditDialog.Create(nil);
    try
      EventDlg.DataStore := DataStore;
      AllowIt := EventDlg.Execute(FActiveEvent, FTimeFormat);
    finally
      EventDlg.Free;
    end;
  end;

  if AllowIt then begin
    FActiveEvent.Changed := true;
    DataStore.PostEvents;
    if Assigned(FOnAddEvent) then                                        
      FOnAddEvent(self, FActiveEvent);                                   
    Invalidate;
  end else begin
    if NewEvent then begin
      DataStore.Resource.Schedule.DeleteEvent(FActiveEvent);
      FActiveEvent := nil;
    end;
    DataStore.PostEvents;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{=====}

procedure TVpWeekView.wvSetDateByCoord(Point: TPoint);
var
  I: Integer;
begin
  for I := 0 to pred(Length(wvWeekdayArray)) do begin
    if (Point.X >= wvWeekdayArray[I].Rec.Left)
    and (Point.X <= wvWeekdayArray[I].Rec.Right)
    and (Point.Y >= wvWeekdayArray[I].Rec.Top)
    and (Point.Y <= wvWeekdayArray[I].Rec.Bottom) then begin             
      Date := wvWeekdayArray[I].Day;                                     
      Invalidate;                                                        
      Exit;                                                              
    end;                                                                 
  end;
end;
{=====}

function TVpWeekView.EventAtCoord(Pt: TPoint): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := 0 to pred(Length(wvEventArray)) do begin
    if wvEventArray[I].Event = nil then begin
      { we've hit the end of visible events without finding a match }
      FActiveEvent := nil;
      wvActiveEventRec.Top := 0;
      wvActiveEventRec.Bottom := 0;
      wvActiveEventRec.Right := 0;
      wvActiveEventRec.Left := 0;
      result := false;
      Exit;
    end;

    if (Pt.X > wvEventArray[I].Rec.Left)
    and (Pt.X < wvEventArray[I].Rec.Right)
    and (Pt.Y > wvEventArray[I].Rec.Top)
    and (Pt.Y < wvEventArray[I].Rec.Bottom) then begin
      { point falls inside this event's rectangle }
      wvHotPoint := Pt;
      FActiveEvent := TVpEvent(wvEventArray[I].Event);
      wvActiveEventRec := wvEventArray[I].Rec;
      result := true;
      Exit;
    end

    else begin
      { point is not within the boundaries of this event's rectangle. }
      FActiveEvent := nil;
      wvActiveEventRec.Top := 0;
      wvActiveEventRec.Bottom := 0;
      wvActiveEventRec.Right := 0;
      wvActiveEventRec.Left := 0;
      result := false;
    end;
  end;
end;
{=====}

procedure TVpWeekView.wvEditInPlace(Sender: TObject);
begin
  { this is the timer event which spawns an in-place editor }
  { if the event is doublecliked before this timer fires, then the }
  { event is edited in a dialog based editor. }
  wvClickTimer.Enabled := false;
  EditEvent;
end;
{=====}

procedure TVpWeekView.EditEvent;
var
  AllowIt: Boolean;
begin
  if FActiveEvent <> nil then begin
    AllowIt := true;
    { call the user defined BeforeEdit event }
    if Assigned(FBeforeEdit) then
      FBeforeEdit(Self, FActiveEvent, AllowIt);

    if AllowIt then begin
      { create and spawn the in-place editor }
      wvInPlaceEditor := TVpWvInPlaceEdit.Create(Self);
      wvInPlaceEditor.Parent := self;
      wvInPlaceEditor.OnExit := EndEdit;
      wvInPlaceEditor.Move(Rect(wvActiveEventRec.Left + TextMargin,
        wvActiveEventRec.Top, wvActiveEventRec.Right - TextMargin,
        wvActiveEventRec.Bottom), true);
      wvInPlaceEditor.Text := FActiveEvent.Description;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpWeekView.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint : TPoint;

begin
  case Key of
    VK_DELETE : DeleteActiveEvent(true);
    VK_RIGHT  : if Shift = [ssShift] then
                  PopupNextWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupNextMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupNextYear (Self)
                else if Shift = [] then begin
                  case DayOfWeek (FActiveDate) of
                    1 : FActiveDate := FActiveDate - 4;
                    2 : FActiveDate := FActiveDate + 3;
                    3 : FActiveDate := FActiveDate + 3;
                    4 : FActiveDate := FActiveDate + 3;
                    5 : FActiveDate := FActiveDate - 3;
                    6 : FActiveDate := FActiveDate - 3;
                    7 : FActiveDate := FActiveDate - 3;
                  end;
                  Invalidate;
                end;
    VK_LEFT   : if Shift = [ssShift] then
                  PopupPrevWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupPrevMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupPrevYear (Self)
                else if Shift = [] then begin
                  case DayOfWeek (FActiveDate) of
                    1 : FActiveDate := FActiveDate - 4;
                    2 : FActiveDate := FActiveDate + 3;
                    3 : FActiveDate := FActiveDate + 3;
                    4 : FActiveDate := FActiveDate + 3;
                    5 : FActiveDate := FActiveDate - 3;
                    6 : FActiveDate := FActiveDate - 3;
                    7 : FActiveDate := FActiveDate - 3;
                  end;
                  Invalidate;
                end;
    VK_UP     : begin
                  if Shift = [] then
                    case DayOfWeek (FActiveDate) of
                      1 : FActiveDate := FActiveDate - 1;
                      2 : FActiveDate := FActiveDate + 2;
                      3 : FActiveDate := FActiveDate - 1;
                      4 : FActiveDate := FActiveDate - 1;
                      5 : FActiveDate := FActiveDate + 3;
                      6 : FActiveDate := FActiveDate - 1;
                      7 : FActiveDate := FActiveDate - 1;
                    end;
                  Invalidate;
                end;
    VK_DOWN   : begin
                  if Shift = [] then
                    case DayOfWeek (FActiveDate) of
                      1 : FActiveDate := FActiveDate - 3;
                      2 : FActiveDate := FActiveDate + 1;
                      3 : FActiveDate := FActiveDate + 1;
                      4 : FActiveDate := FActiveDate - 2;
                      5 : FActiveDate := FActiveDate + 1;
                      6 : FActiveDate := FActiveDate + 1;
                      7 : FActiveDate := FActiveDate + 1;
                    end;
                  Invalidate;
                end;
    VK_INSERT : PopupAddEvent (Self);
    VK_TAB    :
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, True));
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

procedure TVpWeekView.EndEdit(Sender: TObject);
begin
  if wvInPlaceEditor <> nil then begin
    if wvInPlaceEditor.Text <> FActiveEvent.Description then begin
      FActiveEvent.Description := wvInPlaceEditor.Text;
      FActiveEvent.Changed := true;
      if Assigned(FAfterEdit) then
        FAfterEdit(self, FActiveEvent);
      DataStore.PostEvents;
    end;
    wvInPlaceEditor.Free;
    wvInPlaceEditor := nil;
    Invalidate;
    SetFocus;
  end;
end;
{=====}

{ TVpWvHeadAttributes }

constructor TVpWvHeadAttributes.Create(AOwner: TVpWeekView);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBtnFace;
  FFont := TVpFont.Create(AOwner);
end;
{=====}

destructor TVpWvHeadAttributes.Destroy;
begin
  FFont.Free;
  inherited;
end;
{=====}

procedure TVpWvHeadAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpWvHeadAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
end;
{=====}

end.
