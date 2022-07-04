{*********************************************************}
{*                 VPDAYVIEW.PAS 1.03                    *}
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
  This unit contains everything needed for the TVpDayView component (including
  the inline editor).

  The rendering of Visual PlanIt components is a bit involved.  The component's
  Paint method calls RenderToCanvas.  The RenderToCanvas method of each of
  the visual VisualPlanIt controls is repsonsible both for drawing to the
  screen (both design and run time) as well as printing.  In the case of
  printing, the component needs to render itself to an arbitrary rectangle
  and possibly rotated (for the screen the rectangle is the ClientRect
  and the rotation angle is always zero).  To achieve that goal, the
  functions in VpCanvasUtils are used to go between the rendering of the
  control and the TCanvas that it needs to render to.

  The rendering of the DayView is complex.  Look at the other components
  (MonthView and TaskList are probably the best places to start) before making
  changes to the DayView rendering.

  The in place editor is currently based off the TCustomEdit class.  This can
  probably be changed to use a TCustomMemo as its base class.  This will
  provide multi-line editing capabilities.
}

{$I Vp.INC}

{.$DEFINE DEBUGDV} { Causes the DayView to operate in debug mode }

unit VpDayView;

interface

uses
  Windows, Classes, Graphics, Controls, ComCtrls, ExtCtrls, Messages, StdCtrls,
  Buttons, VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpConst,
  VpCanvasUtils, Menus;

type
  TVpLineRec = packed record
    Hour   : TVpHours;
    Minute : Integer;
    Time   : TDateTime;
    Rec    : TRect;
  end;

  TVpColRec = packed record
    Rec    : TRect;
    Date   : TDateTime;
  end;

type
  TVpLineArray = array of TVpLineRec;

type
  TVpLineMatrix = array of TVpLineArray;
  TVpColRectArray = array of TVpColRec;

  TVpDVIconData = record                                                 
    Show   : Boolean;                                                    
    Bitmap : TBitmap;                                                    
  end;                                                                   
  TVpDVIconTypes = (itAlarm, itRecurring, itCategory, itCustom);         
  TVpDVIcons = array [itAlarm..itCustom] of TVpDVIconData;               

  TVpOnDVBeforeDrawEvent = procedure (Sender    : TObject;               
                                      Event     : TVpEvent;              
                                      Active    : Boolean;               
                                      ACanvas   : TCanvas;               
                                      EventRect : TRect;                 
                                      IconRect  : TRect) of object;      
  TVpOnDVAfterDrawEvent = procedure (Sender    : TObject;                
                                     Event     : TVpEvent;               
                                     Active    : Boolean;                
                                     ACanvas   : TCanvas;                
                                     EventRect : TRect;                  
                                     IconRect  : TRect) of object;       
  TVpOnDVDrawIcons = procedure (Sender : TObject;                        
                                Event  : TVpEvent;                       
                                var Icons  : TVpDVIcons) of object;      

  TVpDVWrapStyle = (wsNone, wsIconFlow, wsNoFlow);                       

  { Forward Declarations }
  TVpDayView = class;

  TVpDvInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Move(const Loc: TRect; Redraw: Boolean);
  end;

  TVpRHAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpDayView;
    FColor: TColor;
    FHourFont: TVpFont;
    FMinuteFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetHourFont(Value: TVpFont);
    procedure SetMinuteFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpDayView);
    destructor Destroy; override;
    property Owner : TVpDayView read FOwner;
  published
    property HourFont: TVpFont read FHourFont write SetHourFont;
    property MinuteFont: TVpFont read FMinuteFont write SetMinuteFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpAllDayEventAttributes = class(TPersistent)
  protected {Private}
    FOwner                : TWinControl;
    FBackgroundColor      : TColor;
    FEventBackgroundColor : TColor;
    FEventBorderColor     : TColor;
    FFont                 : TVpFont;
  public
    constructor Create (AOwner : TWinControl);
    destructor Destroy; override;
    procedure SetBackGroundColor(Value: TColor);
    procedure SetEventBackgroundColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
    procedure SetEventBorderColor(Value: TColor);
  published
    property BackgroundColor: TColor
      read FBackgroundColor write SetBackGroundColor;
    property EventBorderColor: TColor
      read FEventBorderColor write SetEventBorderColor;
    property EventBackgroundColor: TColor
      read FEventBackgroundColor write SetEventBackgroundColor;
    property Font: TVpFont
      read FFont write SetFont;
  end;

  TVpCHAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpDayView;
    FColor: TColor;
    FFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpDayView);
    destructor Destroy; override;
    property Owner: TVpDayView read FOwner;
  published
    property Font: TVpFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpDayViewIconAttributes = class (TPersistent)                         
    private                                                              
      FShowAlarmBitmap     : Boolean;                                    
      FShowCategoryBitmap  : Boolean;                                    
      FShowRecurringBitmap : Boolean;                                    
      FAlarmBitmap         : TBitmap;                                    
      FRecurringBitmap     : TBitmap;                                    
      FOwner               : TVpLinkableControl;                         

    protected                                                            
      procedure SetAlarmBitmap (v : TBitmap);                            
      procedure SetRecurringBitmap (v : TBitmap);                        
      procedure SetShowAlarmBitmap (const v : Boolean);                  
      procedure SetShowCategoryBitmap (const v : Boolean);               
      procedure SetShowRecurringBitmap (const v : Boolean);              

    public                                                               
      constructor Create (AOwner : TVpLinkableControl);                  
      destructor Destroy; override;                                      

    published                                                            
      property AlarmBitmap : TBitmap                                     
               read FAlarmBitmap write SetAlarmBitmap;                   
      property RecurringBitmap : TBitmap                                 
               read FRecurringBitmap write SetRecurringBitmap;           
      property ShowAlarmBitmap : Boolean                                 
               read FShowAlarmBitmap write SetShowAlarmBitmap            
               default True;                                             
      property ShowCategoryBitmap : Boolean                              
               read FShowCategoryBitmap write SetShowCategoryBitmap      
               default True;                                             
      property ShowRecurringBitmap : Boolean                             
               read FShowRecurringBitmap write SetShowRecurringBitmap    
               default True;                                             
  end;                                                                   

  TVpDayView = class(TVpLinkableControl)
  protected{ private }
    FGranularity       : TVpGranularity;
    FColumnWidth       : Integer;
    FColor             : TColor;
    FLineColor         : TColor;
    FDefTopHour        : TVpHours;
    FTopHour           : TVpHours;
    FDateLabelFormat   : string;
    FShowResourceName  : Boolean;
    FTopLine           : Integer;
    FActiveRow         : Integer;
    FActiveCol         : Integer;
    FActiveEvent       : TVpEvent;
    FGutterWidth       : Integer;
    FDefaultPopup      : TPopupMenu;
    FLineCount         : Integer;
    FVisibleLines      : Integer;
    FTimeFormat        : TVpTimeFormat;
    FDrawingStyle      : TVpDrawingStyle;
    FTimeSlotColors    : TVpTimeSlotColor;
    FRowHeadAttr       : TVpRHAttributes;
    FHeadAttr          : TVpCHAttributes;
    FAllDayEventAttr   : TVpAllDayEventAttributes;
    FDisplayDate       : TDateTime;
    FScrollBars        : TScrollStyle;
    FIconAttributes    : TVpDayViewIconAttributes;                       
    FWrapStyle         : TVpDVWrapStyle;                                 
    FDotDotDotColor    : TColor;                                         
    FShowEventTimes    : Boolean;                                        
    { event variables }
    FOwnerDrawRowHead  : TVpOwnerDrawRowEvent;
    FOwnerDrawCells    : TVpOwnerDrawRowEvent;
    FOwnerDrawColHead  : TVpOwnerDrawEvent;
    FBeforeEdit        : TVpBeforeEditEvent;
    FAfterEdit         : TVpAfterEditEvent;
    FOwnerEditEvent    : TVpEditEvent;
    FOnDrawIcons       : TVpOnDVDrawIcons;                               
    FOnBeforeDrawEvent : TVpOnDVBeforeDrawEvent;                         
    FOnAfterDrawEvent  : TVpOnDVAfterDrawEvent;                          
    FOnAddEvent        : TVpOnAddNewEvent;                               
    FNumDays           : Integer;
    FIncludeWeekends   : Boolean;
    { internal variables }
    dvClickTimer       : TTimer;
    dvLoaded           : Boolean;
    dvInLinkHandler    : Boolean;
    dvRowHeight        : Integer;
    dvColHeadHeight    : Integer;
    dvRowHeadWidth     : Integer;
    dvClientVArea      : Integer;
    dvMouseDownPoint   : TPoint;
    dvMouseDown        : Boolean;
    dvDragging         : Boolean;
    dvEndingEditing    : Boolean;                                        

    { Nav Buttons }
    dvDayUpBtn         : TSpeedButton;
    dvDayDownBtn       : TSpeedButton;
    dvTodayBtn         : TSpeedButton;
    dvWeekUpBtn        : TSpeedButton;
    dvWeekDownBtn      : TSpeedButton;

    dvDragStartTime    : TDateTime;
    dvLineMatrix       : TVpLineMatrix;
    dvColRectArray     : TVpColRectArray;
    dvEventArray       : TVpEventArray;
    dvActiveEventRec   : TRect;
    dvActiveIconRec    : TRect;                                                                                           
    dvInPlaceEditor    : TVpDvInPlaceEdit;
    dvCreatingEditor   : Boolean;
    { the granularity based time increment for each row }
    dvTimeIncSize      : double;
    dvPainting         : Boolean;
    dvVScrollDelta     : Integer;
    dvHotPoint         : TPoint;

    { property methods }
    function GetLastVisibleDate : TDateTime;                             
    function GetRealNumDays (WorkDate : TDateTime) : Integer;            
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetTopHour(Value: TVpHours);
    procedure SetTopLine(Value: Integer);
    procedure SetDateLabelFormat(Value: string);
    procedure SetGutterWidth(Value: Integer);
    procedure SetDefTopHour(Value: TVpHours);
    procedure SetGranularity(Value: TVpGranularity);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetNumDays(Value : Integer);
    procedure SetIncludeWeekends(Value : Boolean);
    procedure SetDisplayDate(Value: TDateTime);
    procedure SetVScrollPos;
    procedure SetShowResourceName(Value: Boolean);
    procedure SetActiveRow(Value: Integer);
    procedure SetActiveCol(Value: Integer);
    procedure SetWrapStyle (const v : TVpDVWrapStyle);                   
    procedure SetDotDotDotColor (const v : TColor);                      
    procedure SetShowEventTimes(Value: Boolean);                         
    { drag-drop methods }
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    { internal methods }
    function dvCalcRowHeight (Scale   : Extended;
                              UseGran : TVpGranularity) : Integer;
    function dvCalcVisibleLines (RenderHeight  : Integer;
                                 ColHeadHeight : Integer;
                                 RowHeight     : Integer;
                                 Scale         : Extended;
                                 StartLine     : Integer;
                                 StopLine      : Integer) : Integer;
    function dvCalcColHeadHeight (Scale : Extended) : Integer;
    procedure dvEditInPlace(Sender: TObject);
    procedure dvHookUp;
    procedure PopupAddEvent (Sender : TObject);
    procedure PopupDeleteEvent (Sender : TObject);
    procedure PopupEditEvent (Sender : TObject);
    procedure PopupToday (Sender : TObject);
    procedure PopupTomorrow (Sender : TObject);
    procedure PopupYesterday (Sender : TObject);
    procedure PopupNextDay (Sender : TObject);
    procedure PopupPrevDay (Sender : TObject);
    procedure PopupNextWeek (Sender : TObject);
    procedure PopupPrevWeek (Sender : TObject);
    procedure PopupNextMonth (Sender : TObject);
    procedure PopupPrevMonth(Sender : TObject);
    procedure PopupNextYear (Sender : TObject);
    procedure PopupPrevYear (Sender : TObject);
    procedure InitializeDefaultPopup;
    procedure Paint; override;
    procedure Loaded; override;
    procedure dvSpawnEventEditDialog(NewEvent: Boolean);
    procedure dvSetActiveRowByCoord (Pnt    : TPoint;                   
                                     Sloppy : Boolean);                 
    procedure dvSetActiveColByCoord(Pnt: TPoint);
    procedure dvPopulate;
    procedure dvNavButtonsClick(Sender: TObject);
    procedure dvScrollVertical(Lines: Integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure SetActiveEventByCoord (APoint : TPoint);                   
    function EditEventAtCoord(Point: TPoint): Boolean;
    function GetEventAtCoord(Point: TPoint): TVpEvent;
    procedure EditEvent;
    procedure EndEdit(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetTimeIntervals (UseGran : TVpGranularity);
    { message handlers }
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMEraseBackground (var Msg : TWMERASEBKGND);
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure VpDayViewInit (var Msg : TMessage); Message Vp_DayViewInit; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DeleteActiveEvent(Verify: Boolean);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function HourToLine (const Value   : TVpHours;
                         const UseGran : TVpGranularity) : Integer;
    procedure Invalidate; override;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType;
      const Value: Variant); override;
    procedure EditSelectedEvent;

    function GetControlType : TVpItemType; override;
    procedure AutoScaledPaintToCanvas (PaintCanvas : TCanvas;
                                       PaintTo     : TRect;
                                       Angle       : TVpRotationAngle;
                                       RenderDate  : TDateTime;
                                       StartLine   : Integer;
                                       StopLine    : Integer;
                                       UseGran     : TVpGranularity);
    procedure PaintToCanvas (ACanvas   : TCanvas;
                             ARect     : TRect;
                             Angle     : TVpRotationAngle;
                             ADate     : TDateTime;
                             StartHour : TVpHours;
                             EndHour   : TVpHours;
                             UseGran   : TVpGranularity);
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
    property TopHour: TVpHours read FTopHour write SetTopHour;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LineCount: Integer read FLineCount;
    property ActiveRow: Integer read FActiveRow write SetActiveRow;
    property ActiveCol: Integer read FActiveCol write SetActiveCol;
    property Date: TDateTime read FDisplayDate write SetDisplayDate;
    property LastVisibleDate : TDateTime read GetLastVisibleDate;        
    property VisibleLines: Integer read FVisibleLines;
  published
    property Align;
    property Anchors;
    property Constraints;
    property ReadOnly;
    property TabStop;
    property TabOrder;
    property Font;
    property AllDayEventAttributes: TVpAllDayEventAttributes
      read FAllDayEventAttr write FAllDayEventAttr;

    property DotDotDotColor : TColor                                     
      read FDotDotDotColor write SetDotDotDotColor default clBlack;      

    property ShowEventTimes: Boolean                                     
      read FShowEventTimes write SetShowEventTimes default true;         

    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle;
    property TimeSlotColors: TVpTimeSlotColor
      read FTimeSlotColors write FTimeSlotColors;
    property HeadAttributes: TVpCHAttributes
      read FHeadAttr write FHeadAttr;
    property RowHeadAttributes: TVpRHAttributes
      read FRowHeadAttr write FRowHeadAttr;
    property IconAttributes : TVpDayViewIconAttributes                   
             read FIconAttributes write FIconAttributes;                 
    property Color: TColor read FColor write SetColor;
    property OwnerDrawRowHeader: TVpOwnerDrawRowEvent
      read FOwnerDrawRowHead write FOwnerDrawRowHead;
    property OwnerDrawColHeader: TVpOwnerDrawEvent
      read FOwnerDrawColHead write FOwnerDrawColHead;
    property OwnerDrawCells: TVpOwnerDrawRowEvent
      read FOwnerDrawCells write FOwnerDrawCells;
    property ShowResourceName: Boolean
      read FShowResourceName write SetShowResourceName;
    property LineColor: TColor read FLineColor write SetLineColor;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
    property DateLabelFormat:
      string read FDateLabelFormat write SetDateLabelFormat;
    Property Granularity: TVpGranularity read FGranularity write SetGranularity;
    property DefaultTopHour: TVpHours read FDefTopHour write SetDefTopHour;
    property TimeFormat: TVpTimeFormat read FTimeFormat write SetTimeFormat;
    {events}
    property AfterEdit : TVpAfterEditEvent read FAfterEdit write FAfterEdit;
    property BeforeEdit: TVpBeforeEditEvent read FBeforeEdit write FBeforeEdit;
    property IncludeWeekends : Boolean
             read FIncludeWeekends write SetIncludeWeekends default True;
    property NumDays : Integer read FNumDays write SetNumDays default 1;
    property WrapStyle : TVpDVWrapStyle                                  
             read FWrapStyle Write SetWrapStyle default wsIconFlow;      

    property OnAddEvent: TVpOnAddNewEvent                                
      read FOnAddEvent write FOnAddEvent;                                

    property OnAfterDrawEvent : TVpOnDVAfterDrawEvent                    
             read FOnAfterDrawEvent write FOnAfterDrawEvent;             
    property OnBeforeDrawEvent : TVpOnDVBeforeDrawEvent                  
             read FOnBeforeDrawEvent write FOnBeforeDrawEvent;           
    property OnDrawIcons : TVpOnDVDrawIcons                              
             read FOnDrawIcons Write FOnDrawIcons;                       
    property OnClick;
    property OnOwnerEditEvent: TVpEditEvent
      read FOwnerEditEvent write FOwnerEditEvent;
  end;

implementation

uses
  SysUtils, Math, Forms, Dialogs, VpEvntEditDlg;

(*****************************************************************************)
{ TVpTGInPlaceEdit }

constructor TVpDvInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := False;
end;
{=====}

procedure TVpDvInPlaceEdit.Move(const Loc: TRect; Redraw: Boolean);
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible(Handle);
  Invalidate;
  with Loc do
    SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
      SWP_SHOWWINDOW or SWP_NOREDRAW);
  if Redraw then Invalidate;
  Windows.SetFocus(Handle);
end;
{=====}

procedure TVpDvInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;
{=====}

procedure TVpDvInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!! WARNING
// !!!!
// !!!! Experimental change below.  Verify this change before releasing
// !!!! VP 1.03
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
begin
  case Key of
    VK_RETURN: begin
      Key := 0;
      TVpDayView(Owner).EndEdit(Self);
    end;

    VK_UP: begin
      Key := 0;
      TVpDayView(Owner).ActiveRow := TVpDayView(Owner).ActiveRow - 1;
// !!!! TVpDayView(Owner).EndEdit(Self); !!!!  !!!!!!!!!!!!!!!!!!!!!!!!!
    end;

    VK_DOWN: begin
      Key := 0;
      TVpDayView(Owner).ActiveRow := TVpDayView(Owner).ActiveRow + 1;
// !!!! TVpDayView(Owner).EndEdit(Self); !!!!  !!!!!!!!!!!!!!!!!!!!!!!!!
    end;

    VK_ESCAPE: begin
      Key := 0;
      TVpDayView(Owner).SetFocus;
    end;

  else
    inherited;
  end;
end;
{=====}

procedure TVpDvInPlaceEdit.WMKillFocus(var Msg : TWMKillFocus);
begin
  TVpDayView(Owner).EndEdit(self);
end;
{=====}


{ TVpAllDayEventAttributes }

constructor TVpAllDayEventAttributes.Create (AOwner : TWinControl);
begin
  FOwner:= AOwner;
  FFont := TVpFont.Create(AOwner);
  FBackgroundColor := clBtnShadow;
  FEventBackgroundColor := clBtnFace;
  FEventBorderColor := cl3dDkShadow;
end;
{=====}

destructor TVpAllDayEventAttributes.Destroy;
begin
  inherited;
  FFont.Free;
end;
{=====}

procedure TVpAllDayEventAttributes.SetBackGroundColor(Value: TColor);
begin
  FBackgroundColor := Value;
  FOwner.Invalidate;
end;
{=====}

procedure TVpAllDayEventAttributes.SetEventBackgroundColor(Value: TColor);
begin
  FEventBackgroundColor := Value;
  FOwner.Invalidate;
end;
{=====}

procedure TVpAllDayEventAttributes.SetEventBorderColor(Value: TColor);
begin
  FEventBorderColor := Value;
  FOwner.Invalidate;
end;

procedure TVpAllDayEventAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
  FFont.Owner := FOwner;
end;
{=====}

(*****************************************************************************)
{ TVpDayViewIconAttributes }                                             

constructor TVpDayViewIconAttributes.Create (                            
                AOwner : TVpLinkableControl);                            
begin                                                                    
  inherited Create;                                                      

  FOwner               := AOwner;                                                                                              

  FAlarmBitmap         := TBitmap.Create;                                
  FRecurringBitmap     := TBitmap.Create;                                

  FShowAlarmBitmap     := True;                                          
  FShowCategoryBitmap  := True;                                          
  FShowRecurringBitmap := True;                                          
end;                                                                     

destructor TVpDayViewIconAttributes.Destroy;                             
begin                                                                    
  FAlarmBitmap.Free;                                                     
  FRecurringBitmap.Free;                                                 

  inherited Destroy;                                                     
end;                                                                     

procedure TVpDayViewIconAttributes.SetAlarmBitmap (v : TBitmap);         
begin                                                                    
  FAlarmBitmap.Assign (v);                                               
  if Assigned (FOwner) then                                              
    FOwner.Invalidate;                                                   
end;                                                                     

procedure TVpDayViewIconAttributes.SetRecurringBitmap (v : TBitmap);     
begin                                                                    
  FRecurringBitmap.Assign (v);                                           
  if Assigned (FOwner) then                                              
    FOwner.Invalidate;                                                   
end;                                                                     

procedure TVpDayViewIconAttributes.SetShowAlarmBitmap (                  
              const v : Boolean);                                        
begin                                                                    
  if FShowAlarmBitmap <> v then begin                                    
    FShowAlarmBitmap := v;                                               
    if Assigned (FOwner) then                                            
      FOwner.Invalidate;                                                 
  end;                                                                   
end;                                                                     

procedure TVpDayViewIconAttributes.SetShowCategoryBitmap (               
              const v : Boolean);                                        
begin                                                                    
  if FShowCategoryBitmap <> v then begin                                 
    FShowCategoryBitmap := v;                                            
    if Assigned (FOwner) then                                            
      FOwner.Invalidate;                                                 
  end;                                                                   
end;                                                                     

procedure TVpDayViewIconAttributes.SetShowRecurringBitmap (              
              const v : Boolean);                                        
begin                                                                    
  if FShowRecurringBitmap <> v then begin                                
    FShowRecurringBitmap := v;                                           
    if Assigned (FOwner) then                                            
      FOwner.Invalidate;                                                 
  end;                                                                   
end;                                                                     

(*****************************************************************************)
{ TVpDayView }

constructor TVpDayView.Create(AOwner: TComponent);

begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  { Create internal classes and stuff }
  FTimeSlotColors                := TVpTimeSlotColor.Create (self);
  FHeadAttr                      := TVpCHAttributes.Create (self);
  FRowHeadAttr                   := TVpRHAttributes.Create (self);
  FAllDayEventAttr               := TVpAllDayEventAttributes.Create(self);
  dvClickTimer                   := TTimer.Create (self);
  FIconAttributes                := TVpDayViewIconAttributes.Create (Self); 

  { create Nav buttons }
  dvDayUpBtn                     := TSpeedButton.Create(self);
  dvDayUpBtn.Parent              := self;
  dvDayDownBtn                   := TSpeedButton.Create(self);
  dvDayDownBtn.Parent            := self;
  dvTodayBtn                     := TSpeedButton.Create(self);
  dvTodayBtn.Parent              := self;
  dvWeekDownBtn                  := TSpeedButton.Create(self);
  dvWeekDownBtn.Parent           := self;
  dvWeekUpBtn                    := TSpeedButton.Create(self);
  dvWeekUpBtn.Parent             := self;
  { flat }
  dvTodayBtn.Flat                := true;
  dvWeekDownBtn.Flat             := true;
  dvDayDownBtn.Flat              := true;
  dvDayUpBtn.Flat                := true;
  dvWeekUpBtn.Flat               := true;
  { transparent }
  dvTodayBtn.Transparent         := true;
  dvWeekDownBtn.Transparent      := true;
  dvDayDownBtn.Transparent       := true;
  dvDayUpBtn.Transparent         := true;
  dvWeekUpBtn.Transparent        := true;
  { load their images }
  dvDayUpBtn.Glyph.Handle        := LoadBaseBitmap('VPRIGHTARROW');
  dvDayDownBtn.Glyph.Handle      := LoadBaseBitmap('VPLEFTARROW');
  dvTodayBtn.Glyph.Handle        := LoadBaseBitmap('VPTODAY');
  dvWeekUpBtn.Glyph.Handle       := LoadBaseBitmap('VPRIGHTARROWS');
  dvWeekDownBtn.Glyph.Handle     := LoadBaseBitmap('VPLEFTARROWS');
  { set their OnClick handler }
  dvDayUpBtn.OnClick             := dvNavButtonsClick;
  dvDayDownBtn.OnClick           := dvNavButtonsClick;
  dvTodayBtn.OnClick             := dvNavButtonsClick;
  dvWeekUpBtn.OnClick            := dvNavButtonsClick;
  dvWeekDownBtn.OnClick          := dvNavButtonsClick;
  { Set up the hints }                                                   
  dvDayUpBtn.ShowHint            := True;                                
  dvDayDownBtn.ShowHint          := True;                                
  dvTodayBtn.ShowHint            := True;                                
  dvWeekUpBtn.ShowHint           := True;                                
  dvWeekDownBtn.ShowHint         := True;                                
  dvDayUpBtn.Hint                := rsHintTomorrow;                      
  dvDayDownBtn.Hint              := rsHintYesterday;                     
  dvTodayBtn.Hint                := rsHintToday;                         
  dvWeekUpBtn.Hint               := rsHintNextWeek;                      
  dvWeekDownBtn.Hint             := rsHintPrevWeek;                      

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered                 := true;
  {$ENDIF}
  NumDays                        := 1;
  dvInLinkHandler                := false;
  dvClickTimer.Enabled           := false;
  dvClickTimer.Interval          := ClickDelay;
  dvClickTimer.OnTimer           := dvEditInPlace;

  dvCreatingEditor               := false;
  FDrawingStyle                  := ds3d;
  dvPainting                     := false;
  FShowResourceName              := true;
  FColor                         := clWindow;
  FLineColor                     := clGray;
  Granularity                    := gr30min;
  FDefTopHour                    := h_07;
  FDisplayDate                   := Now;
  TopHour                        := FDefTopHour;
  FTimeFormat                    := tf12Hour;
  FDateLabelFormat               := 'dddd, mmmm dd, yyyy';
  FColumnWidth                   := 200;
  FScrollBars                    := ssVertical;
  FActiveRow                     := -1;
  FGutterWidth                   := 7;
  dvEndingEditing                := False;                               
  FWrapStyle                     := wsIconFlow;                          
  FDotDotDotColor                := clBlack;                             
  FIncludeWeekends               := True;

  { set up fonts and colors }
  FHeadAttr.Font.Name            := 'Tahoma';
  FHeadAttr.Font.Size            := 10;
  FHeadAttr.Font.Style           := [];
  FHeadAttr.Color                := clBtnFace;

  FRowHeadAttr.FHourFont.Name    := 'Tahoma';
  FRowHeadAttr.FHourFont.Size    := 18;
  FRowHeadAttr.FHourFont.Style   := [];
  FRowHeadAttr.FMinuteFont.Name  := 'Tahoma';
  FRowHeadAttr.FMinuteFont.Size  := 9;
  FRowHeadAttr.FMinuteFont.Style := [];
  FRowHeadAttr.Color             := clBtnFace;

  SetLength(dvEventArray, MaxVisibleEvents);

  DragMode         := dmManual;
  dvMouseDownPoint := Point(0, 0);
  dvMouseDown      := false;
  dvDragging       := false;

  { size }
  Height                         := 225;
  Width                          := 265;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  dvHookUp;
end;
{=====}

destructor TVpDayView.Destroy;

begin
  FTimeSlotColors.Free;
  FHeadAttr.Free;
  FRowHeadAttr.Free;
  FAllDayEventAttr.Free;
  dvClickTimer.Free;
  FDefaultPopup.Free;
  FIconAttributes.Free;                                                   

  dvDayUpBtn.Free;
  dvDayDownBtn.Free;
  dvTodayBtn.Free;
  dvWeekUpBtn.Free;
  dvWeekDownBtn.Free;

  inherited;
end;
{=====}

procedure TVpDayView.DeleteActiveEvent(Verify: Boolean);
var
  Str: string;
  DoIt: Boolean;
begin
  if ReadOnly then                                                       
    Exit;
  dvClickTimer.Enabled := false;
  EndEdit(self);

  DoIt := not Verify;

  if FActiveEvent <> nil then begin
    Str := '"' + FActiveEvent.Description + '"';

    if Verify then
      DoIt := (MessageDlg(RSDelete + ' ' + Str + ' ' + RSFromSchedule
        + #13#10#10 + RSPermanent, mtconfirmation,
        [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveEvent.Deleted := true;
      DataStore.PostEvents;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpDayView.Invalidate;
begin
  inherited;
end;
{=====}

procedure TVpDayView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  dvInLinkHandler := true;
  try
    case NotificationType of
      neDateChange: Date := Value;
      neDataStoreChange: Invalidate;
      neInvalidate: Invalidate;
    end;
  finally
    dvInLinkHandler := false;
  end; 
end;
{=====}

procedure TVpDayView.dvHookUp;
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

procedure TVpDayView.InitializeDefaultPopup;
var
  NewItem    : TMenuItem;
  NewSubItem : TMenuItem;

begin
  if RSDayPopupAdd <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupAdd;
    NewItem.OnClick := PopupAddEvent;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupEdit <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupEdit;
    NewItem.OnClick := PopupEditEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupDelete <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupDelete;
    NewItem.OnClick := PopupDeleteEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupNav <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupNav;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);

    if RSDayPopupNavToday <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavToday;
      NewSubItem.OnClick := PopupToday;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavYesterday <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavYesterday;
      NewSubItem.OnClick := PopupYesterday;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavTomorrow <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavTomorrow;
      NewSubItem.OnClick := PopupTomorrow;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextDay <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextDay;
      NewSubItem.OnClick := PopupNextDay;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevDay <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevDay;
      NewSubItem.OnClick := PopupPrevDay;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextWeek;
      NewSubItem.OnClick := PopupNextWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevWeek;
      NewSubItem.OnClick := PopupPrevWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextMonth;
      NewSubItem.OnClick := PopupNextMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevMonth;
      NewSubItem.OnClick := PopupPrevMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextYear;
      NewSubItem.OnClick := PopupNextYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevYear;
      NewSubItem.OnClick := PopupPrevYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;
  end;
end;
{=====}

procedure TVpDayView.PopupAddEvent (Sender : TObject);
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

  StartTime := trunc(FDisplayDate + ActiveCol) +
               dvLineMatrix[ActiveCol, ActiveRow].Time;
  EndTime := StartTime + dvTimeIncSize;
  FActiveEvent := DataStore.Resource.Schedule.AddEvent (
                      DataStore.GetNextID (EventsTableName),
                      StartTime, EndTime);

  Repaint;                                                             
  { edit this new event }
  dvSpawnEventEditDialog(True);
end;
{=====}

procedure TVpDayView.PopupDeleteEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  Repaint;                                                             
  if FActiveEvent <> nil then
    DeleteActiveEvent (True);
end;
{=====}

procedure TVpDayView.PopupEditEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  Repaint;                                                             
  if FActiveEvent <> nil then
    { edit this Event }
    dvSpawnEventEditDialog(False);
end;
{=====}

procedure TVpDayView.PopupToday (Sender : TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpDayView.PopupTomorrow (Sender : TObject);
begin
  Date := Now + 1;
end;
{=====}

procedure TVpDayView.PopupYesterday (Sender : TObject);
begin
  Date := Now - 1;
end;
{=====}

procedure TVpDayView.PopupNextDay (Sender : TObject);
begin
  Date := Date + 1;
end;
{=====}

procedure TVpDayView.PopupPrevDay (Sender : TObject);
begin
  Date := Date - 1;
end;
{=====}

procedure TVpDayView.PopupNextWeek (Sender : TObject);
begin
  Date := Date + 7;
end;
{=====}

procedure TVpDayView.PopupPrevWeek (Sender : TObject);
begin
  Date := Date - 7;
end;
{=====}

procedure TVpDayView.PopupNextMonth (Sender : TObject);
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

procedure TVpDayView.PopupPrevMonth(Sender : TObject);
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

procedure TVpDayView.PopupNextYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y + 1, M, 1);
end;
{=====}

procedure TVpDayView.PopupPrevYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y - 1, M, 1);
end;
{=====}

procedure TVpDayView.Loaded;
begin
  inherited;
  TopHour := DefaultTopHour;
  dvLoaded := true;
  dvPopulate;
end;
{=====}

procedure TVpDayView.Paint;
begin
  RenderToCanvas (Canvas,
                  Rect (0, 0, Width, Height),
                  ra0,
                  1,
                  FDisplayDate,
                  TopLine,
                  -1,
                  FGranularity,
                  False);
  SetVScrollPos; 
end;
{=====}

procedure TVpDayView.dvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FDisplayDate; 
end;
{=====}

procedure TVpDayView.dvNavButtonsClick(Sender: TObject);
begin
  { set the value of Date based on which button was pressed. }
  if Sender = dvDayUpBtn then
    Date := Date + 1
  else if Sender = dvDayDownBtn then
    Date := Date - 1
  else if Sender = dvTodayBtn then
    Date := trunc(Now)
  else if Sender = dvWeekUpBtn then
    Date := Date + 7
  else if Sender = dvWeekDownBtn then
    Date := Date - 7;
end;
{=====}

function TVpDayView.dvCalcVisibleLines (RenderHeight  : Integer;
                                        ColHeadHeight : Integer;
                                        RowHeight     : Integer;
                                        Scale         : Extended;
                                        StartLine     : Integer;
                                        StopLine      : Integer) : Integer;
var
  vertical: integer;

begin
  if StartLine < 0 then
    StartLine := TopLine;

  { take into account the number lines that are allowed! }
  vertical := Round (RenderHeight - (ColHeadHeight * Scale) - 2);
  Result := trunc (Vertical div RowHeight) + 2;
  if Result > FLineCount then
    Result := FLineCOunt;

  if (StopLine > 0) and (StopLine > StartLine) then
    if Result > StopLine - StartLine then
      Result := StopLine - StartLine + 2;
  FVisibleLines := Result;
end;
{=====}

procedure TVpDayView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

function TVpDayView.dvCalcColHeadHeight (Scale : Extended) : Integer;
var
  TextHeight : Integer;

begin
  Canvas.Font.Assign (FHeadAttr.Font);

  if FShowResourceName and (DataStore <> nil) and
     (DataStore.Resource <> nil) then
    TextHeight := (Canvas.TextHeight(RSTallShortChars) * 2) +
                  (TextMargin * 3)
  else
    TextHeight := Canvas.TextHeight(RSTallShortChars) + (TextMargin * 2);
  Result := Round (TextHeight * Scale);
  dvColHeadHeight := Result; 
end;
{=====}

procedure TVpDayView.DoStartDrag(var DragObject: TDragObject);
begin
  DvDragStartTime := 0.0;

  if ReadOnly then
    Exit;
  if FActiveEvent <> nil then begin
    { Set the time from which this event was dragged }
    DvDragStartTime := trunc(Date + ActiveCol)
      + dvLineMatrix[ActiveCol, ActiveRow].Time;

    DragObject := TVpEventDragObject.Create;
    TVpEventDragObject(DragObject).Event := FActiveEvent;
  end
  else
    EndDrag(false);
end;
{=====}

procedure TVpDayView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  if ReadOnly then
    Exit;
  TVpEventDragObject(Target).Free;
end;
{=====}

procedure TVpDayView.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if ReadOnly then begin
    Accept := False;
    Exit;
  end;
  if (X > dvRowHeadWidth + GutterWidth) and (Y > dvColHeadHeight) then begin
    { The mouse is dragging over the client area }
    dvSetActiveColByCoord(Point(X, Y));
    dvSetActiveRowByCoord(Point(X, Y), False);                           
    Accept := true;
  end else
    Accept := false;
end;
{=====}

procedure TVpDayView.DragDrop(Source: TObject; X, Y: Integer);
var
  Event       : TVpEvent;
  Duration    : TDateTime;
  DragToTime  : TDateTime;
  i           : Integer;                                                 

begin
  if ReadOnly then
    Exit;
  Event := TVpEventDragObject(Source).Event;

  if Event <> nil then begin
    Duration := Event.EndTime - Event.StartTime;
    DragToTime := trunc(Date + ActiveCol)
      + dvLineMatrix[ActiveCol, ActiveRow].Time;

    if Ord(Event.RepeatCode) = 0 then
      { if this is not a recurring event then just drop it here          }
      Event.StartTime := DragToTime
    else
      { if this is a recurring event, then modify the event's start time }
      { according to how far the event was dragged                       }
      Event.StartTime := Event.StartTime + (DragToTime - DvDragStartTime);

    Event.EndTime := Event.StartTime + Duration;

    DataStore.PostEvents;

    { Force a repaint.  This will update the rectangles for the event }
    Repaint;                                                             

    { Reset the active event rectangle }
    for I := 0 to pred(Length(dvEventArray)) do begin                    
      if dvEventArray[I].Event = nil then                                
        Break;                                                           

      if dvEventArray[i].Event = Event then begin                        
        dvActiveEventRec := dvEventArray[I].Rec;                         
        dvActiveIconRec  := dvEventArray[I].IconRect;                    
        Break;                                                           
      end;                                                               
    end;                                                                 

    { Invalidate;                                                      } 
  end;

  EndDrag(False);
end;
{=====}

function TVpDayView.dvCalcRowHeight (Scale   : Extended;
                                     UseGran : TVpGranularity) : Integer;
var
  SaveFont : TFont;
  Temp     : Integer;

begin
  { Calculates row height based on the largest of the RowHead's Minute }
  { font, the standard client font, and a sample character string.     }
  SaveFont := Canvas.Font;
  Canvas.Font.Assign(FRowHeadAttr.FMinuteFont);
  Result := Canvas.TextHeight(RSTallShortChars);
  Canvas.Font.Assign(SaveFont);
  Temp := Canvas.TextHeight(RSTallShortChars);
  if Temp > Result then
    Result := Temp;
  Result := Result + TextMargin * 2;

  Result := Round (Result * Scale);

  case UseGran of
    gr60Min : dvClientVArea := Result * 24;
    gr30Min : dvClientVArea := Result * 48;
    gr20Min : dvClientVArea := Result * 72;
    gr15Min : dvClientVArea := Result * 96;
    gr10Min : dvClientVArea := Result * 144;
    gr06Min : dvClientVArea := Result * 240;
    gr05Min : dvClientVArea := Result * 288;
  end;
  dvRowHeight := Result;
end;
{=====}
function TVpDayView.GetLastVisibleDate : TDateTime;                      
begin                                                                    
  Result := Date + GetRealNumDays (Date);                                
end;                                                                     
{=====}
function TVpDayView.GetRealNumDays (WorkDate : TDateTime) : Integer;     
var                                                                      
  i : Integer;                                                           

begin                                                                    
  if not FIncludeWeekends then begin                                     
    Result := 0;                                                         
    i := 0;                                                              
    while i < FNumDays do begin                                          
      if (DayOfWeek (WorkDate) <> 1) and                                 
         (DayOfWeek (WorkDate) <> 7) then                                
        Inc (i);                                                         
      WorkDate := WorkDate + 1;                                          
      Inc (Result);                                                      
    end;                                                                 
  end else                                                               
    Result := FNumDays;                                                  
end;                                                                     
{=====}
function TVpDayView.HourToLine (const Value   : TVpHours;
                                 const UseGran : TVpGranularity) : Integer;
begin
  case UseGran of
    gr60Min : Result := Ord (Value);
    gr30Min : Result := Ord (Value) * 2;
    gr20Min : Result := Ord (Value) * 3;
    gr15Min : Result := Ord (Value) * 4;
    gr10Min : Result := Ord (Value) * 6;
    gr06Min : Result := Ord (Value) * 10;
    gr05Min : Result := Ord (Value) * 12;
    else
      Result := Ord (Value) * 2; { Default to 30 minutes }
  end;
end;

procedure TVpDayView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpDayView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpDayView.SetTopHour (Value : TVpHours);
begin
  if FTopHour <> Value then begin
    FTopHour := Value;
    TopLine := HourToLine (FTopHour, FGranularity);
  end;
end;
{=====}

procedure TVpDayView.SetTopLine(Value: Integer);
begin
  if Value <> FTopLine then begin
    if Value + VisibleLines >= pred(LineCount) then begin
      if Granularity = gr60Min then
        FTopLine := pred(LineCount) - VisibleLines + 2
      else
        FTopLine := pred(LineCount) - VisibleLines + 2;
      { prevent the control from hanging at the bottom }
      if (Value < FTopLine) and (Value > 0) then
        FTopLine := Value;
    end
    else if Value < 0 then
      FTopLine := 0
    else
      FTopLine := Value;
    Invalidate;
    SetVScrollPos;
  end;
end;
{=====}

procedure TVpDayView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetGutterWidth(Value: Integer);
begin
  if (Value <> FGutterWidth)
  and (Value > -1)
  and (Value < (Width div 10)) then begin
    FGutterWidth := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetDefTopHour(Value: TVpHours);
begin
  if Value <> FDefTopHour then begin
    FDefTopHour := Value;
    if csDesigning in ComponentState then
    TopHour := Value;
  end;
end;
{=====}

procedure TVpDayView.SetTimeIntervals (UseGran : TVpGranularity);
var
  I, J : Integer;
begin
  case UseGran of
    gr60Min: begin
      FLineCount := 24;
      dvTimeIncSize :=  60 / MinutesInDay;                               
    end;
    gr30Min: begin
      FLineCount := 48;
      dvTimeIncSize :=  30 / MinutesInDay;
    end;
    gr20Min: begin
      FLineCount := 72;
      dvTimeIncSize :=  20 / MinutesInDay;
    end;
    gr15Min: begin
      FLineCount := 96;
      dvTimeIncSize :=  15 / MinutesInDay;
    end;
    gr10Min: begin
      FLineCount := 144;
      dvTimeIncSize :=  10 / MinutesInDay;
    end;
    gr06Min : begin
      FLineCount := 240;
      dvTimeIncSize :=   6 / MinutesInDay;
    end;
    gr05Min : begin
      FLineCount := 288;
      dvTimeIncSize :=   5 / MinutesInDay;
    end;
  end;

  SetLength(dvLineMatrix, NumDays);
  for I := 0 to pred(NumDays) do
    SetLength(dvLineMatrix[I], LineCount + 1);

  for I := 0 to pred(NumDays) do begin
    for J := 0 to pred(LineCount) do begin
      dvLineMatrix[I,J].Time := 0.0;
      if J = 0 then begin
        dvLineMatrix[I,J].Hour := TVpHours(0);
        dvLineMatrix[I,J].Minute := 0;
      end
      else begin
        case UseGran of
          gr60Min: begin
            dvLineMatrix[I,J].Time := J * (60 / MinutesInDay);
            dvLineMatrix[I,J].Hour := TVpHours(J);
            dvLineMatrix[I,J].Minute := 0;
          end;

          gr30Min: begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 2);
            case (J mod 2) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 2) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour)
                  * (60 / MinutesInDay)
                  + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 30;
              end;
            end;
          end;

          gr20Min: begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 3);
            case (J mod 3) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 3) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour)
                  * (60 / MinutesInDay) + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 20;
              end;
              2: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour)
                  * (60 / MinutesInDay) + (dvTimeIncSize * 2));
                dvLineMatrix[I,J].Minute := 40;
              end;
            end;
          end;

          gr15Min: begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 4);
            case (J mod 4) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 4) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour)
                  * (60 / MinutesInDay) + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 15;
              end;
              2: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 2));
                dvLineMatrix[I,J].Minute := 30;
              end;
              3: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 3));
                dvLineMatrix[I,J].Minute := 45;
              end;
            end;
          end;

          gr10Min: begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 6);
            case (J mod 6) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 6) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 10;
              end;
              2: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 2));
                dvLineMatrix[I,J].Minute := 20;
              end;
              3: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 3));
                dvLineMatrix[I,J].Minute := 30;
              end;
              4: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 4));
                dvLineMatrix[I,J].Minute := 40;
              end;
              5: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 5));
                dvLineMatrix[I,J].Minute := 50;
              end;
            end;
          end;

          gr06Min : begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 10);
            case (J mod 10) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 10) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 6;
              end;
              2: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 2));
                dvLineMatrix[I,J].Minute := 12;
              end;
              3: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 3));
                dvLineMatrix[I,J].Minute := 18;
              end;
              4: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 4));
                dvLineMatrix[I,J].Minute := 24;
              end;
              5: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 5));
                dvLineMatrix[I,J].Minute := 30;
              end;
              6: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 6));
                dvLineMatrix[I,J].Minute := 36;
              end;
              7: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 7));
                dvLineMatrix[I,J].Minute := 42;
              end;
              8: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 8));
                dvLineMatrix[I,J].Minute := 48;
              end;
              9: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 9));
                dvLineMatrix[I,J].Minute := 54;
              end;
            end;
          end;

          gr05Min : begin
            dvLineMatrix[I,J].Hour := TVpHours(J div 12);
            case (J mod 12) of
              0: begin
                dvLineMatrix[I,J].Time := (J div 12) * (60 / MinutesInDay);
                dvLineMatrix[I,J].Minute := 0;
              end;
              1: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + dvTimeIncSize);
                dvLineMatrix[I,J].Minute := 5;
              end;
              2: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 2));
                dvLineMatrix[I,J].Minute := 10;
              end;
              3: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 3));
                dvLineMatrix[I,J].Minute := 15;
              end;
              4: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 4));
                dvLineMatrix[I,J].Minute := 20;
              end;
              5: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 5));
                dvLineMatrix[I,J].Minute := 25;
              end;
              6: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 6));
                dvLineMatrix[I,J].Minute := 30;
              end;
              7: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 7));
                dvLineMatrix[I,J].Minute := 35;
              end;
              8: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 8));
                dvLineMatrix[I,J].Minute := 40;
              end;
              9: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 9));
                dvLineMatrix[I,J].Minute := 45;
              end;
              10: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 10));
                dvLineMatrix[I,J].Minute := 50;
              end;
              11: begin
                dvLineMatrix[I,J].Time := (Ord(dvLineMatrix[I,J].Hour) * (60 / MinutesInDay)
                  + (dvTimeIncSize * 11));
                dvLineMatrix[I,J].Minute := 55;
              end;
            end;
          end;
        end;
      end;
    end; {for J...}
  end; {for I...}

  if FLineCount <= FVisibleLines then
    FTopLine := HourToLine (h_00, FGranularity);

  SetVScrollPos;
end;

procedure TVpDayView.SetGranularity(Value: TVpGranularity);
begin
  FGranularity := Value;

  SetTimeIntervals (FGranularity);
  FTopLine := HourToLine (FTopHour, FGranularity);                       

  Invalidate;
end;
{=====}

procedure TVpDayView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetDisplayDate(Value: TDateTime);
begin
  if FDisplayDate <> Value then begin
    EndEdit(self);
    FDisplayDate := Value;
    if dvLoaded then
      dvPopulate;
    Invalidate;

    if (not dvInLinkHandler) and (ControlLink <> nil) then
      ControlLink.Notify(self, neDateChange, Date);
  end;
end;
{=====}

procedure TVpDayView.WMSize(var Msg: TWMSize);
var
  MaxLinesToDraw: Integer;
  EmptyLines: Integer;
begin
  inherited;
  { How many lines are there between TopLine and the last line of the day. }
  MaxLinesToDraw := Length(dvLineMatrix[0]) - TopLine;
  EmptyLines := FVisibleLines - MaxLinesToDraw;

  if EmptyLines > 0 then
    TopLine := TopLine - EmptyLines
  else
    Invalidate; 
end;
{=====}

procedure TVpDayView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or WS_TABSTOP;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
    WindowClass.style := CS_DBLCLKS;
  end; 
end;
{=====}

procedure TVpDayView.CreateWnd;
begin
  inherited;

  PostMessage (Handle, Vp_DayViewInit, 0, 0);                           
end;
{=====}

procedure TVpDayView.WMLButtonDown(var Msg : TWMLButtonDown);
begin
  inherited;

  dvMouseDownPoint := Point(Msg.XPos, Msg.YPos);
  dvMouseDown      := true;

  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus;

  if (Msg.XPos > dvRowHeadWidth - 9) and (Msg.YPos > dvColHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    dvSetActiveColByCoord(Point(Msg.XPos, Msg.YPos));
    dvSetActiveRowByCoord(Point(Msg.XPos, Msg.YPos), True);              
    if not ReadOnly then
      EditEventAtCoord(Point(Msg.XPos, Msg.YPos));
  end else if Msg.YPos > dvColHeadHeight then                            
    dvSetActiveRowByCoord (Point (Msg.XPos, Msg.YPos), True);            

  if Assigned(OnClick) then
    OnClick(self);
end;
{=====}

procedure TVpDayView.WMRButtonDown(var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;
  i            : Integer;
begin
  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then
    SetFocus;

  if (Msg.XPos > dvRowHeadWidth - 9) and (Msg.YPos > dvColHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    dvSetActiveColByCoord(Point(Msg.XPos, Msg.YPos));
    dvSetActiveRowByCoord(Point(Msg.XPos, Msg.YPos), True);              
  end;

  EditEventAtCoord (Point (Msg.XPos, Msg.YPos));
  dvClickTimer.Enabled := false;

  if not Assigned (PopupMenu) then begin
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

  inherited;
end;
{=====}

procedure TVpDayView.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  dvMouseDownPoint   := Point(0, 0);
  dvMouseDown        := false;
  dvDragging         := false;
  inherited;
end;
{=====}

procedure TVpDayView.WMLButtonDblClk(var Msg : TWMLButtonDblClk);
var
  StartTime, EndTime: TDateTime;
begin
  inherited;
  dvClickTimer.Enabled := false;
  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus;

  if (Msg.XPos > dvRowHeadWidth - 9) and (Msg.YPos > dvColHeadHeight) then
  begin
    { The mouse click landed inside the client area }
    dvSetActiveRowByCoord(Point(Msg.XPos, Msg.YPos), True);              
    { See if we hit an active event }
    if (FActiveEvent <> nil) and (not ReadOnly) then begin
      { edit this event }
      dvSpawnEventEditDialog(False);
    end else if not ReadOnly then begin
      if not CheckCreateResource then                                  
        Exit;                                                          
      if (DataStore = nil) or (DataStore.Resource = nil) then
        Exit;
      { otherwise, we must want to create a new event }
      StartTime := trunc(FDisplayDate + ActiveCol)
        + dvLineMatrix[ActiveCol, ActiveRow].Time;
      EndTime := StartTime + dvTimeIncSize;
      FActiveEvent := DataStore.Resource.Schedule.AddEvent(
        DataStore.GetNextID(EventsTableName), StartTime, EndTime);
      { edit this new event }
      dvSpawnEventEditDialog(True);
    end;
  end;
end;
{=====}

procedure TVpDayView.WMMouseMove(var Msg : TWMMouseMove);
begin

  if (FActiveEvent <> nil) and (not ReadOnly) then begin
    if (not dvDragging) and dvMouseDown
    and ((dvMouseDownPoint.x <> Msg.XPos) or (dvMouseDownPoint.y <> Msg.YPos))
    then begin
      dvDragging := true;
      dvClickTimer.Enabled := false;
      BeginDrag(true);
    end;
  end;

  inherited;
end;
{=====}

procedure TVpDayView.EditSelectedEvent;
begin
  if ReadOnly then
    Exit;
  if FActiveEvent <> nil then
    dvSpawnEventEditDialog(false);
end;
{=====}

procedure TVpDayView.dvSpawnEventEditDialog(NewEvent: Boolean);
var
  AllowIt: Boolean;
  EventDlg : TVpEventEditDialog;
begin
  if (DataStore = nil) or (DataStore.Resource = nil) or ReadOnly then
    Exit;

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
      FActiveEvent.Deleted := true;
      DataStore.PostEvents;
      FActiveEvent := nil;
      dvActiveEventRec := Rect(0, 0, 0, 0);
      dvActiveIconRec  := Rect(0, 0, 0, 0);                                  
    end else
      DataStore.PostEvents;

    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.WMSetFocus(var Msg : TWMSetFocus);
begin
  if ActiveRow = -1 then ActiveRow := TopLine;
end;
{=====}

procedure TVpDayView.WMEraseBackground (var Msg : TWMERASEBKGND);
begin
  Msg.Result := 1;
end;
{=====}

procedure TVpDayView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{=====}

procedure TVpDayView.SetActiveEventByCoord (APoint : TPoint);            
var                                                                      
  I : Integer;                                                                                                                             

begin                                                                    
  for I := 0 to pred(Length(dvEventArray)) do begin                      

    if dvEventArray[I].Event = nil then                                  
      Exit;                                                              

    if (APoint.X > dvEventArray[I].Rec.Left)   and                       
       (APoint.X < dvEventArray[I].Rec.Right)  and                       
       (APoint.Y > dvEventArray[I].Rec.Top)    and                       
       (APoint.Y < dvEventArray[I].Rec.Bottom) then begin                
      FActiveEvent := TVpEvent(dvEventArray[I].Event);                   
      dvActiveEventRec := dvEventArray[I].Rec;                           
      dvActiveIconRec  := dvEventArray[I].IconRect;                      
      Exit;                                                              
    end;                                                                 
  end;                                                                   
end;                                                                     

function TVpDayView.EditEventAtCoord(Point: TPoint): Boolean;
var
  I: Integer;
begin
  result := false;
  if ReadOnly then
    Exit;
  for I := 0 to pred(Length(dvEventArray)) do begin
    if dvEventArray[I].Event = nil then begin
      { we've hit the end of visible events without finding a match }
      FActiveEvent := nil;
      dvActiveEventRec.Top := 0;
      dvActiveEventRec.Bottom := 0;
      dvActiveEventRec.Right := 0;
      dvActiveEventRec.Left := 0;
      dvActiveIconRec  := Rect (0, 0, 0, 0);                             
      Exit;
    end;
    if (Point.X > dvEventArray[I].Rec.Left)
    and (Point.X < dvEventArray[I].Rec.Right)
    and (Point.Y > dvEventArray[I].Rec.Top)
    and (Point.Y < dvEventArray[I].Rec.Bottom) then begin
      FActiveEvent := TVpEvent(dvEventArray[I].Event);
      dvActiveEventRec := dvEventArray[I].Rec;
      dvActiveIconRec  := dvEventArray[I].IconRect;                      
      dvClickTimer.Enabled := true;
      result := true;
      Break;
    end else begin
      FActiveEvent := nil;
      dvActiveEventRec.Top := 0;
      dvActiveEventRec.Bottom := 0;
      dvActiveEventRec.Right := 0;
      dvActiveEventRec.Left := 0;
      dvActiveIconRec  := Rect (0, 0, 0, 0);                             
    end;
  end;
end;
{=====}

function TVpDayView.GetEventAtCoord(Point: TPoint): TVpEvent;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to pred(Length(dvEventArray)) do begin

    if dvEventArray[I].Event = nil then
      Exit;

    if (Point.X > dvEventArray[I].Rec.Left)
    and (Point.X < dvEventArray[I].Rec.Right)
    and (Point.Y > dvEventArray[I].Rec.Top)
    and (Point.Y < dvEventArray[I].Rec.Bottom) then begin
      result := TVpEvent(dvEventArray[I].Event);
      Exit;
    end;
  end;
end;
{=====}

procedure TVpDayView.dvEditInPlace(Sender: TObject);
begin
  { this is the timer event which spawns an in-place editor }
  { if the event is doublecliked before this timer fires, then the }
  { event is edited in a dialog based editor. }
  dvClickTimer.Enabled := false;
  EditEvent;
end;
{=====}

procedure TVpDayView.EditEvent;
var
  AllowIt: Boolean;
begin
  if ReadOnly then
    Exit;
  AllowIt := true;
  { call the user defined BeforeEdit event }
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self, FActiveEvent, AllowIt);

  if AllowIt then begin
    { create and spawn the in-place editor }
    dvInPlaceEditor := TVpDvInPlaceEdit.Create(Self);
    dvInPlaceEditor.Parent := self;
    dvInPlaceEditor.OnExit := EndEdit;
    dvInPlaceEditor.Move (Rect (dvActiveIconRec.Right + FGutterWidth +   
                                    TextMargin,                          
                                dvActiveEventRec.Top + TextMargin,       
                                dvActiveEventRec.Right,                  
                                dvActiveEventRec.Bottom - 1),            
                          true);                                         
    dvInPlaceEditor.Text := FActiveEvent.Description;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.EndEdit(Sender: TObject);
begin
  if dvEndingEditing then                                                
    Exit;                                                                
  dvEndingEditing := True;                                               
  try                                                                    
    if dvInPlaceEditor <> nil then begin
      if dvInPlaceEditor.Text <> FActiveEvent.Description then begin
        FActiveEvent.Description := dvInPlaceEditor.Text;
        FActiveEvent.Changed := true;
        DataStore.PostEvents;
        if Assigned(FAfterEdit) then
          FAfterEdit(self, FActiveEvent);
      end;
      try
        dvInPlaceEditor.Free;
        dvInPlaceEditor := nil;
      except
        // The editor was already freed.
      end;
      Invalidate;
    end;
  finally                                                                
    dvEndingEditing := False;                                            
  end;                                                                   
end;
{=====}

procedure TVpDayView.KeyDown(var Key: Word; Shift: TShiftState);

var
  PopupPoint : TPoint;

begin
  case Key of
    VK_UP      : ActiveRow := ActiveRow - 1;
    VK_DOWN    : ActiveRow := ActiveRow + 1;
    VK_NEXT    : ActiveRow := ActiveRow + FVisibleLines;
    VK_PRIOR   : ActiveRow := ActiveRow - FVisibleLines;
    VK_LEFT    : Date := Date - 1;
    VK_RIGHT   : Date := Date + 1;
    VK_HOME    : ActiveRow := 0;
    VK_END     : ActiveRow := LineCount;
    VK_DELETE  : if not ReadOnly then
                  DeleteActiveEvent(true);
    VK_TAB     :
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, True));
    VK_F10    :
      if (ssShift in Shift) and not (Assigned (PopupMenu)) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
    VK_APPS   :
      if not Assigned (PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
    VK_RETURN : PopupEditEvent (Self);
    VK_INSERT : PopupAddEvent (Self);
    VK_F2     : if Assigned (FActiveEvent) then
                  dvEditInPlace (Self)                                   
                else begin                                               
                  PopupPoint := dvLineMatrix[ActiveCol,                  
                                             ActiveRow].Rec.TopLeft;     
                  PopupPoint.x := PopupPoint.x + 1;                      
                  PopupPoint.y := PopupPoint.y + 1;                      
                  SetActiveEventByCoord (PopupPoint);                    
                  if Assigned (FActiveEvent) then                        
                    dvEditInPlace (Self);                                
                end;                                                     
  end;
end;
{=====}

procedure TVpDayView.WMVScroll(var Msg: TWMVScroll);
begin
  { for simplicity, bail out of editing while scrolling. }
  EndEdit(Self);

  if dvInPlaceEditor <> nil then Exit;

  case Msg.ScrollCode of
    SB_LINEUP    : dvScrollVertical(-1);
    SB_LINEDOWN  : dvScrollVertical(1);
    SB_PAGEUP    : dvScrollVertical(-FVisibleLines);
    SB_PAGEDOWN  : dvScrollVertical(FVisibleLines);
    SB_THUMBPOSITION, SB_THUMBTRACK : TopLine := Msg.Pos;
  end;
end;
{=====}

procedure TVpDayView.dvScrollVertical(Lines: Integer);
begin
  TopLine := TopLine + Lines;
end;
{=====}

procedure TVpDayView.SetVScrollPos;
var
  SI : TScrollInfo;
begin
  if not HandleAllocated then
    Exit;
  with SI do begin
    cbSize := SizeOf(SI);
    fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    nMin := 0;
    nMax := FLineCount;
    if FVisibleLines >= FLineCount then
      nPage := nMax
    else
      nPage := FVisibleLines;
    if FTopLine = pred(LineCount) - VisibleLines then
      nPos := LineCount
    else
      nPos := FTopLine;
    nTrackPos := nPos;
  end;
  SetScrollInfo (Handle, SB_VERT, SI, True);
end;
{=====}
procedure TVpDayView.SetShowResourceName(Value: Boolean);
begin
  if Value <> FShowResourceName then begin
    FShowResourceName := Value;
    Invalidate;
  end;
end;

procedure TVpDayView.SetNumDays (Value : Integer);
begin
  if (Value <> FNumDays)
  and (Value > 0)
  and (Value < 31) then begin
    FNumDays := Value;
    SetLength(dvColRectArray, FNumDays);
    SetTimeIntervals(Granularity);
    ActiveCol := 0;
    Invalidate;
  end;
end;

procedure TVpDayView.SetIncludeWeekends(Value : Boolean);
begin
  if Value <> FIncludeWeekends then begin
    FIncludeWeekends := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetActiveRow(Value: Integer);
var
  OldActiveRow: Integer;
begin
  if dvClickTimer.Enabled then
    dvClickTimer.Enabled := false;

  if not Focused then SetFocus;
  OldActiveRow := FActiveRow;
  { set active row }
  if (Value < 0) then
    FActiveRow := 0
  else if (Value >= pred(LineCount)) then
    FActiveRow := pred(LineCount)
  else
    FActiveRow := Value;

  { clamp in view }
  if (FActiveRow < FTopLine) then
    TopLine := FActiveRow
  else if (FActiveRow >= FTopLine + FVisibleLines) then
    TopLine := FActiveRow - FVisibleLines + 1;

  if (OldActiveRow <> FActiveRow) then begin
    Invalidate;
  end;
end;
{=====}

procedure TVpDayView.SetActiveCol(Value: Integer);
begin
  if FActiveCol <> Value then begin
    if Value < 0 then
      FActiveCol := 0
    else if Value > pred(NumDays) then
      FActiveCol := pred(NumDays)
    else
      FActiveCol := Value;
    Invalidate;
  end;
end;
{=====}
procedure TVpDayView.SetDotDotDotColor (const v : TColor);               
begin                                                                    
  if v <> FDotDotDotColor then begin                                     
    FDotDotDotColor := v;                                                
    Invalidate;                                                          
  end;                                                                   
end;                                                                     
{=====}

procedure TVpDayView.SetShowEventTimes(Value: Boolean);                  
begin                                                                    
  if Value <> FShowEventTimes then begin                                 
    FShowEventTimes := Value;                                            
    Invalidate;                                                          
  end;                                                                   
end;                                                                     
{=====}                                                                  

procedure TVpDayView.SetWrapStyle (const v : TVpDVWrapStyle);            
begin                                                                    
  if v <> FWrapStyle then begin                                          
    FWrapStyle := v;                                                     
    Invalidate;                                                          
  end;                                                                   
end;                                                                     
{=====}

procedure TVpDayView.dvSetActiveRowByCoord (Pnt    : TPoint;             
                                            Sloppy : Boolean);           
var
  I : Integer;
begin
  if dvClickTimer.Enabled  then
    dvClickTimer.Enabled := false;
  for I := 0 to pred(LineCount) do begin
    if (Sloppy) and                                                      
            (Pnt.y <= dvLineMatrix[ActiveCol, I].Rec.Bottom) and         
            (Pnt.y > dvLineMatrix[ActiveCol, I].Rec.Top) then begin      
      ActiveRow := I;                                                    
      Exit;                                                              
    end else if (Pnt.x > dvLineMatrix[ActiveCol, I].Rec.Left) and        
                (Pnt.x < dvLineMatrix[ActiveCol, I].Rec.Right) and       
                (Pnt.y <= dvLineMatrix[ActiveCol, I].Rec.Bottom) and     
                (Pnt.y > dvLineMatrix[ActiveCol, I].Rec.Top) then begin  
      ActiveRow := I;                                        
      Exit;
    end;
  end;
end;
{=====}

procedure TVpDayView.dvSetActiveColByCoord(Pnt: TPoint);
var
  I : Integer;
begin
  for I := 0 to pred(length(dvColRectArray)) do begin
    if (Pnt.x > dvColRectArray[I].Rec.Left)
    and (Pnt.x < dvColRectArray[I].Rec.Right)
    and (Pnt.y < dvColRectArray[I].Rec.Bottom)
    and (Pnt.y > dvColRectArray[I].Rec.Top) then begin
      ActiveCol := I;
      Exit;
    end;
  end;
end;
{=====}

function TVpDayView.GetControlType : TVpItemType;
begin
  Result := itDayView;
end;

procedure TVpDayView.AutoScaledPaintToCanvas (PaintCanvas : TCanvas;
                                               PaintTo     : TRect;
                                               Angle       : TVpRotationAngle;
                                               RenderDate  : TDateTime;
                                               StartLine   : Integer;
                                               StopLine    : Integer;
                                               UseGran     : TVpGranularity);
var
  SrcResY  : Integer;
  DestResY : Integer;
  Scale    : Extended;

begin
  SrcResY  := GetDeviceCaps (Canvas.Handle,      LOGPIXELSY);
  DestResY := GetDeviceCaps (PaintCanvas.Handle, LOGPIXELSY);

  Scale    := DestResY / SrcResY;

  RenderToCanvas (PaintCanvas, PaintTo, Angle, Scale, RenderDate,
                        StartLine, StopLine, UseGran, True);
end;

procedure TVpDayView.PaintToCanvas (ACanvas   : TCanvas;
                                     ARect     : TRect;
                                     Angle     : TVpRotationAngle;
                                     ADate     : TDateTime;
                                     StartHour : TVpHours;
                                     EndHour   : TVpHours;
                                     UseGran   : TVpGranularity);
begin
  RenderToCanvas (ACanvas, ARect, Angle, 1, ADate,
                  HourToLine (StartHour, UseGran),
                  HourToLine (EndHour, UseGran),
                  UseGran, True);
end;

procedure TVpDayView.RenderToCanvas (RenderCanvas : TCanvas;
                                     RenderIn     : TRect;
                                     Angle        : TVpRotationAngle;
                                     Scale        : Extended;
                                     RenderDate   : TDateTime;
                                     StartLine    : Integer;
                                     StopLine     : Integer;
                                     UseGran      : TVpGranularity;
                                     DisplayOnly  : Boolean);

  {function GetRealNumDays (WorkDate : TDateTime) : Integer;           } 
  {var                                                                 } 
  {  i        : Integer;                                               } 
  {begin                                                               } 
  {  if not FIncludeWeekends then begin                                } 
  {    Result := 0;                                                    } 
  {    i := 0;                                                         } 
  {    while i < FNumDays do begin                                     } 
  {      if (DayOfWeek (WorkDate) <> 1) and                            } 
  {         (DayOfWeek (WorkDate) <> 7) then                           } 
  {        Inc (i);                                                    } 
  {      WorkDate := WorkDate + 1;                                     } 
  {      Inc (Result);                                                 } 
  {    end;                                                            } 
  {  end else                                                          } 
  {    Result := FNumDays;                                             } 
  {end;                                                                } 

var
  TextWidth       : Integer;
  ColHeadRect     : TRect;
  CellsRect       : TRect;
  RowHeadRect     : TRect;
  ADEventsRect    : TRect;
  SaveBrushColor  : TColor;
  SavePenStyle    : TPenStyle;
  SavePenColor    : TColor;
  Drawn           : Boolean;
  ScrollBarOffset : Integer;
  EventCount      : Integer;
  RealWidth       : Integer;
  RealHeight      : Integer;
  RealLeft        : Integer;
  RealRight       : Integer;
  RealTop         : Integer;
  RealBottom      : Integer;
  DayWidth        : Integer;
  RealNumDays     : Integer;
  Rgn             : HRGN;
  RealRowHeight   : Integer;
  RealColHeadHeight : Integer;
  RealRowHeadWidth  : Integer;
  RealVisibleLines  : Integer;

  BevelShadow          : TColor;
  BevelHighlight       : TColor;
  BevelDarkShadow      : TColor;
  WindowColor          : TColor;
  HighlightText        : TColor;
  RealHeadAttrColor    : TColor;
  RealRowHeadAttrColor : TColor;
  RealLineColor        : TColor;
  RealColor            : TColor;
  BevelFace            : TColor;
  HighlightBkg         : TColor;
  RealADEventBkgColor  : TColor;                                         
  ADEventAttrBkgColor  : TColor;                                         
  ADEventBorderColor   : TColor;                                         

  procedure SetMeasurements;
  begin
    RealWidth  := TPSViewportWidth (Angle, RenderIn);
    RealHeight := TPSViewportHeight (Angle, RenderIn);
    RealLeft   := TPSViewportLeft (Angle, RenderIn);
    RealRight  := TPSViewportRight (Angle, RenderIn);
    RealTop    := TPSViewportTop (Angle, RenderIn);
    RealBottom := TPSViewportBottom (Angle, RenderIn);
    dvCalcColHeadHeight (Scale);
  end;

  procedure dvDrawColHeader(R : TRect; RenderDate : TDateTime; Col: Integer);
  var
    SaveFont : TFont;
    DateStr, ResStr: string;
    DateStrLen, ResStrLen: integer;
    StrHt: Integer;
    TextRect: TRect;
    X, Y: Integer;
  begin
    SaveFont := TFont.Create;
    try
      SaveFont.Assign(RenderCanvas.Font);
      { Draw Column Header }
      RenderCanvas.Font.Assign(FHeadAttr.FFont);
      RenderCanvas.Brush.Color := RealHeadAttrColor;
      RenderCanvas.Pen.Style := psClear;
      TPSRectangle (RenderCanvas, Angle, RenderIn, R);
      RenderCanvas.Pen.Style := psSolid;

      { Size text rect }
      TextRect.TopLeft := R.TopLeft;
      TextRect.BottomRight := R.BottomRight;
      TextRect.Right := TextRect.Right - 3;
      TextRect.Left := TextRect.Left + 2;

      { Fix Date String }
      DateStr := FormatDateTime(FDateLabelFormat, RenderDate);        
      DateStrLen := RenderCanvas.TextWidth(DateStr);
      StrHt := RenderCanvas.TextHeight(DateStr);
      if DateStrLen > TextRect.Right - TextRect.Left then begin
        DateStr := GetDisplayString(RenderCanvas, DateStr, 0,
          TextRect.Right - TextRect.Left);
        DateStrLen := RenderCanvas.TextWidth(DateStr);
      end;

      if (DataStore <> nil)
      and (DataStore.Resource <> nil)
      and FShowResourceName then begin
        { fix Res String }
        ResStr := DataStore.Resource.Description;
        ResStrLen := RenderCanvas.TextWidth(ResStr);
        if ResStrLen > TextRect.Right - TextRect.Left then begin
          ResStr := GetDisplayString(RenderCanvas, ResStr, 0,
            TextRect.Right - TextRect.Left);
          ResStrLen := RenderCanvas.TextWidth(ResStr);
        end;
        { center and write the resource name in the first column }
        if (Col = 0) then begin
          X := TextRect.Left + ((TextRect.Right - TextRect.Left) div 2)
            - ResStrLen div 2;
          Y := TextRect.Top + TextMargin;
          TPSTextOut (RenderCanvas, Angle, RenderIn,
                      X, Y, DataStore.Resource.Description);
        end;
        { center and write the date string }
        X := TextRect.Left + ((TextRect.Right - TextRect.Left) div 2)
          - DateStrLen div 2;
        Y := TextRect.Top + (TextMargin * 2) + StrHt;
        TPSTextOut (RenderCanvas, Angle, RenderIn,
                    X, Y, DateStr);
      end else begin
        { center and write the date string }
        Y := TextRect.Top + TextMargin;
        X := TextRect.Left + ((TextRect.Right - TextRect.Left) div 2)
          - DateStrLen div 2;
        TPSTextOut (RenderCanvas, Angle, RenderIn, X, Y, DateStr);
      end;

      {Draw Column Head Borders }
      if FDrawingStyle = dsFlat then begin

        RenderCanvas.Pen.color := BevelShadow;
        {bottom}
        TPSMoveTo (RenderCanvas, Angle, RenderIn, R.Right, R.Bottom);
        TPSLineTo (RenderCanvas, Angle, RenderIn, R.Left - 1, R.Bottom);
        {right side}
        TPSMoveTo (RenderCanvas, Angle, RenderIn, R.Right, R.Bottom - 4);
        TPSLineTo (RenderCanvas, Angle, RenderIn, R.Right, R.Top + 3);
        RenderCanvas.Pen.color := BevelHighlight;
        {left side}
        TPSMoveTo (RenderCanvas, Angle, RenderIn, R.Left, R.Bottom - 4);
        TPSLineTo (RenderCanvas, Angle, RenderIn, R.Left, R.Top + 3);
      end
      else if FDrawingStyle = ds3d then begin
        DrawBevelRect (RenderCanvas,
                       TPSRotateRectangle (Angle, RenderIn,
                                           Rect (R.Left, R.Top,
                                                 R.Right, R.Bottom)),
                       BevelHighlight, BevelDarkShadow);
      end;
      RenderCanvas.Font.Assign(SaveFont);
    finally
      SaveFont.Free;
    end;
  end;

  procedure dvDrawRowHeader (R : TRect);
  var
    Temp , I: Integer;
    LineRect: TRect;
    LastHour, Hour: Integer;
    MinuteStr, HourStr: string;
    SaveFont: TFont;

  begin
    if StartLine < 0 then
      StartLine := TopLine;

    SaveFont := TFont.Create;
    try
      RenderCanvas.Pen.Style := psClear;
      RenderCanvas.Brush.Color := RealRowHeadAttrColor;
      TPSFillRect(RenderCanvas, Angle, RenderIn, R);
      RenderCanvas.Pen.Style := psSolid;

      RenderCanvas.Font.Assign (FRowHeadAttr.MinuteFont);
      RealVisibleLines := dvCalcVisibleLines (R.Bottom - R.Top,
                                              RealColHeadHeight, RealRowHeight,
                                              Scale, StartLine, StopLine);
      Temp := RenderCanvas.TextWidth('33');
      Temp := Temp + 10;
      RenderCanvas.Pen.Style := psSolid;
      RenderCanvas.Pen.Color := RealLineColor;
      LineRect := Rect (R.Left, R.Top, R.Right, R.Top + RealRowHeight);
      Hour := Ord(dvLineMatrix[0, StartLine].Hour);

      for I := 0 to RealVisibleLines do begin
        { prevent any extranneous drawing below the last hour }
        if (I + FTopLine >= FLineCount)
        or (Hour > 23)
        then Break;

        if I = 0 then begin
          if Hour < 12 then
            MinuteStr := 'am'
          else
            MinuteStr := 'pm';
        end
        else if Ord(Hour) = 12 then
          MinuteStr := 'pm'
        else
          MinuteStr := '00';

        if TimeFormat = tf24Hour then
          MinuteStr := '00';

        { Position the rect }
        LineRect.Top := R.Top + i * RealRowHeight;
        LineRect.Bottom := LineRect.Top + RealRowHeight;

        if (Hour > 12) and (TimeFormat = tf12Hour) then
          HourStr := IntToStr(Hour - 12)
        else begin
          HourStr := IntToStr(Hour);
          if (TimeFormat = tf12Hour) and (HourStr = '0') then
            HourStr := '12';
        end;

        if UseGran = gr60Min then begin
          { Paint time }
          RenderCanvas.Font.Assign(FRowHeadAttr.MinuteFont);
          TPSTextOut (RenderCanvas, Angle, RenderIn,
                      LineRect.Right -
                      RenderCanvas.TextWidth(HourStr + ':' + MinuteStr) - 7,
                      LineRect.Top + TextMargin, HourStr + ':' + MinuteStr);
          LastHour := Hour;
          Inc(Hour);
        end else begin
          { Paint Minute Text}
          if dvLineMatrix[0, StartLine + i].Minute = 0 then begin
            RenderCanvas.Font.Assign(FRowHeadAttr.MinuteFont);
            TPSTextOut (RenderCanvas, Angle, RenderIn,
                        LineRect.Right - RenderCanvas.TextWidth(MinuteStr) - 7,
                        LineRect.Top + TextMargin, MinuteStr);
            { Paint Hour Text }
            RenderCanvas.Font.Assign(FRowHeadAttr.HourFont);
            TPSTextOut (RenderCanvas, Angle, RenderIn,
                        LineRect.Right - RenderCanvas.TextWidth(HourStr) - 2
                          - Temp, LineRect.Top + TextMargin - 2, HourStr);
          end;
          LastHour := Hour;
          Hour := Ord(dvLineMatrix[0, StartLine + i + 1].Hour);
        end;

        TPSMoveTo (RenderCanvas, Angle, RenderIn,
                   LineRect.Right-6, LineRect.Bottom);
        if LastHour <> Hour then
          TPSLineTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Left + 6, LineRect.Bottom)
        else
          TPSLineTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Right-Temp, LineRect.Bottom);
      end; {for}

      { Draw Row Header Borders }
      if FDrawingStyle = dsFlat then begin
        DrawBevelRect (RenderCanvas, TPSRotateRectangle (Angle, RenderIn,
          Rect (R.Left - 1, R.Top, R.Right - 1, R.Bottom - 2)), BevelHighlight,
          BevelShadow);
      end
      else if FDrawingStyle = ds3d then begin
        DrawBevelRect (RenderCanvas, TPSRotateRectangle (Angle, RenderIn,
          Rect (R.Left + 1, R.Top, R.Right - 1, R.Bottom - 3)), BevelHighlight,
          BevelDarkShadow);
      end;

      RenderCanvas.Font.Assign(SaveFont);

    finally
      SaveFont.Free;
    end;
  end;
  {=====}


  { Returns the time duration of one row of the DayView }
  function RowDuration: Double;
  begin
    case Granularity of
      gr60Min : result := 24 / MinutesInDay;
      gr30Min : result := 30 / MinutesInDay;
      gr20Min : result := 20 / MinutesInDay;
      gr15Min : result := 15 / MinutesInDay;
      gr10Min : result := 10 / MinutesInDay;
      gr06Min : result :=  6 / MinutesInDay;
      gr05Min : result :=  5 / MinutesInDay;
    else
      result := 0.0;
    end;
  end;

  { Draws the all-day events at the top of the DayView in a special manner }
  procedure DrawAllDayEvents;
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
    if (DataStore = nil) or (DataStore.Resource = nil) then
      Exit;

    { Collect all of the events for this range and determine the maximum     }
    { number of all day events for the range of days covered by the control. }
    MaxADEvents := 0;

    ADEventsList := TList.Create;
    try
      TempList := TList.Create;
      try
        for I := 0 to pred(RealNumDays) do begin
          { skip weekends }
          if ((DayOfWeek (RenderDate + i) = 1) or
              (DayOfWeek (RenderDate + i) = 7)) and
              (not FIncludeWeekends) then
            Continue;

          { get the all day events for the day specified by RenderDate + I }
          DataStore.Resource.Schedule.AllDayEventsByDate(RenderDate + I,
            TempList);

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
        end;
      finally
        TempList.Free;
      end;

      if MaxADEvents > 0 then begin
        { Set attributes }
        RenderCanvas.Brush.Color := RealADEventBkgColor;                 
        RenderCanvas.Font.Assign (AllDayEventAttributes.Font);

        { Measure the AllDayEvent TextHeight }
        ADTextHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin;

        { Build the AllDayEvent rect based on the value of MaxADEvents }
        ADEventsRect.Bottom := AdEventsRect.Top
          + (MaxADEvents * ADTextHeight) + TextMargin * 2;

        { Clear the AllDayEvents area }
        TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

        StartsBeforeRange  := false;
        { Cycle through the all day events and draw them appropriately }
        for I := 0 to pred(ADEventsList.Count) do begin

          Event := ADEventsList[I];

          { set the top of the event's rect }
          AdEventRect.Top := ADEventsRect.Top + TextMargin
            + (I  * ADTextHeight);

          { see if the event began before the start of the range }
          if (Event.StartTime < trunc(RenderDate)) then
            StartsBeforeRange := true;

          AdEventRect.Bottom := ADEventRect.Top + ADTextHeight;
          AdEventRect.Left := AdEventsRect.Left + (TextMargin div 2);
          AdEventRect.Right := RealRight;                                

          if (StartsBeforeRange) then
            EventStr := '>> '
          else
            EventStr := '';

          EventStr := EventStr + Event.Description;

          RenderCanvas.Brush.Color := ADEventAttrBkgColor;               
          RenderCanvas.Pen.Color := ADEventBorderColor;                  
          TPSRectangle (RenderCanvas, Angle, RenderIn,
                        ADEventRect.Left + TextMargin,
                        ADEventRect.Top + TextMargin div 2,
                        ADEventRect.Right - TextMargin,
                        ADEventRect.Top + ADTextHeight + TextMargin div 2);
          TPSTextOut (RenderCanvas,Angle, RenderIn,
                      AdEventRect.Left + TextMargin * 2 + TextMargin div 2,
                      AdEventRect.Top + TextMargin div 2,
                      EventStr);

          dvEventArray[EventCount].Rec := Rect (ADEventRect.Left,
                                                ADEventRect.Top - 2,
                                                ADEventRect.Right - TextMargin,
                                                ADEventRect.Bottom);
          dvEventArray[EventCount].Event := Event;
          Inc (EventCount);
        end; { for I := 0 to pred(ADEventsList.Count) do ... }

      end;   { if MaxADEvents > 0 }

    finally
      ADEventsList.Free;
    end;
  end;


  procedure DrawEvents (RenderDate : TDateTime; Col: Integer);
  type
    { Defines matrix of event records for managing how events overlap }
    { with each other.                                                }
    TVpDvEventRec = packed record
      Event        : Pointer;
      Level        : Integer;
      OLLevels     : Integer; { The number of levels which overlap with the }
                              { event represented by this record.           }
      WidthDivisor : Integer; { the maximum OLEvents of all of this event's }
                              { overlapping neighbors.                      }
    end;
  type
    TVpDvEventArray = array of TVpDvEventRec;

  var
    I,J, StartPixelOffset, EndPixelOffset      : Integer;
    Level, EventWidth, EventSLine, EventELine  : Integer;
    EventLineCount                             : Integer;
    EventSTime, EventETime, ThisTime           : Double;
    EventDuration, LineDuration, PixelDuration : Double;
    StartOffset, EndOffset, STime, ETime       : Double;
    EventRect, VisibleRect, GutterRect         : TRect;
    EventString, Format                        : string;

    Event      : TVpEvent;
    SaveFont   : TFont;
    SaveColor  : TColor;
    EventArray : TVpDvEventArray;
    EventList  : TList;

    IconRect                                   : TRect;                  
    dvBmpRecurring     : TBitmap;                                        
    dvBmpCategory      : TBitmap;                                        
    dvBmpAlarm         : TBitmap;                                        
    dvBmpCustom        : TBitmap;                                        
    RecurringW         : Integer;                                        
    RecurringH         : Integer;                                        
    CategoryW          : Integer;                                        
    CategoryH          : Integer;                                        
    AlarmW             : Integer;                                        
    AlarmH             : Integer;                                        
    CustomW            : Integer;                                        
    CustomH            : Integer;                                        

    {$IFDEF DEBUGDV}
    SL : TStringList;
    {$ENDIF}


    { returns the number of events which overlap the specified event }
    function CountOverlappingEvents(Event: TVpEvent;
      const EArray: TVpDvEventArray): Integer;
    var
      K, SelfLevel: Integer;
      Tmp: TVpEvent;
      Levels: array of Integer;
    begin
      { initialize the levels array }
      SetLength(Levels, MaxEventDepth);
      for K := 0 to pred(MaxEventDepth) do
        Levels[K] := 0;
      result := 0;
      { First, simply count the number of overlapping events. }
      K := 0;
      SelfLevel := -1;
      Tmp := TVpEvent(EArray[K].Event);
      while Tmp <> nil do begin
        if Tmp = Event then begin
          SelfLevel := K;
          Inc(K);
          Tmp := TVpEvent(EArray[K].Event);
          Continue;
        end;
        { if the Tmp event's StartTime or EndTime falls within the range of }
        { Event... }
        if (TimeInRange(Tmp.StartTime, Event.StartTime, Event.EndTime, false)
          or TimeInRange(Tmp.EndTime, Event.StartTime, Event.EndTime, false))
        { or the Tmp event's StartTime is before or equal to the Event's  }
        { start time AND its end time is after or equal to the Event's    }
        { end time, then the events overlap and we will need to increment }
        { the value of K.          }
        or ((Tmp.StartTime <= Event.StartTime)
          and (Tmp.EndTime >= Event.EndTime))
        then begin
          { Count this event at this level }
          Inc(Levels[EArray[K].Level]);
          Inc(result);
        end;
        Inc(K);
        Tmp := TVpEvent(EArray[K].Event);
      end;
      { Then adjust count for overlapping events which share a level. }
      for K := 0 to pred(MaxEventDepth) do begin
        if K = SelfLevel then Continue;
        if Levels[K] = 0 then Continue;
        result := result - (Levels[K] - 1);
      end;
    end;
    {---}


    { returns the maximum OLEvents value from all overlapping neighbors }
    function GetMaxOLEvents(Event: TVpEvent;
      const EArray: TVpDvEventArray): Integer;
    var
      K: Integer;
      Tmp: TVpEvent;
    begin
      result := 1;
      K := 0;
      Tmp := TVpEvent(EArray[K].Event);
      while Tmp <> nil do begin
        { if the Tmp event's StartTime or EndTime falls within the range of }
        { Event... }
        if (TimeInRange(Tmp.StartTime, Event.StartTime, Event.EndTime, false)
          or TimeInRange(Tmp.EndTime, Event.StartTime, Event.EndTime, false))
        { or the Tmp event's StartTime is before or equal to the Event's  }
        { start time AND its end time is after or equal to the Event's    }
        { end time, then the events overlap and we will need to check the }
        { value of OLLevels. If it is bigger than result, then modify     }
        { Result accordingly. }
        or ((Tmp.StartTime <= Event.StartTime)
          and (Tmp.EndTime >= Event.EndTime))
        then begin
          if EArray[K].OLLevels > result then
            Result := EArray[K].OLLevels;
        end;
        Inc(K);
        Tmp := TVpEvent(EArray[K].Event);
      end;
    end;
    {---}


    procedure VerifyMaxWidthDivisors;
    var
      I, K: Integer;
      Event1, Event2: TVpEvent;
    begin

      for I := 0 to pred(MaxVisibleEvents) do begin
        { if we hit a null event, then we're through }
        if EventArray[I].Event = nil then
          Break;

        { otherwise keep going }
        Event1 := EventArray[I].Event;

        { initialize the WidthDivisor for this record }
        EventArray[I].WidthDivisor := 1;

        {now iterate through all events and get the maximum OLEvents value of }
        { all the overlapping events }
        for K := 0 to pred(MaxVisibleEvents) do begin
          { if we hit a null event, then we're through }
          if EventArray[K].Event = nil then
            Break;

          Event2 := EventArray[K].Event;

          { if the Tmp event overlaps with Event, then check it's Width divisor }
          if (TimeInRange(Event2.StartTime, Event1.StartTime, Event1.EndTime, false)
            or TimeInRange(Event2.EndTime, Event1.StartTime, Event1.EndTime, false))
          or ((Event2.StartTime <= Event1.StartTime)
            and (Event2.EndTime >= Event1.EndTime))
          then begin
            if EventArray[I].WidthDivisor < EventArray[K].WidthDivisor
              Then EventArray[I].WidthDivisor := EventArray[K].WidthDivisor;
          end;
        end;
      end;
    end;
    {---}                                                                

    procedure CreateBitmaps;                                             
    begin                                                                
      dvBmpRecurring := TBitmap.Create;                                  
      dvBmpCategory  := TBitmap.Create;                                  
      dvBmpAlarm     := TBitmap.Create;                                  
      dvBmpCustom    := TBitmap.Create;                                  
    end;                                                                 

    procedure FreeBitmaps;                                               
    begin                                                                
      dvBmpRecurring.Free;                                               
      dvBmpCategory.Free;                                                
      dvBmpAlarm.Free;                                                   
      dvBmpCustom.Free;                                                  
    end;                                                                 

    procedure GetIcons (Event : TVpEvent);                               
    var                                                                  
      ShowAlarm     : Boolean;                                           
      ShowRecurring : Boolean;                                           
      ShowCategory  : Boolean;                                           
      ShowCustom    : Boolean;                                           
      Icons         : TVpDVIcons;                                        

    begin                                                                
      ShowAlarm     := False;                                            
      ShowRecurring := False;                                            
      ShowCategory  := False;                                            
      ShowCustom    := False;                                            

      if Event.AlarmSet then begin                                       
        dvBmpAlarm.Assign (IconAttributes.AlarmBitmap);                  
        ShowAlarm := (dvBmpAlarm.Width <> 0) and                         
                     (dvBmpAlarm.Height <> 0);                           
      end;                                                               

      if Event.RepeatCode <> rtNone then begin                           
        dvBmpRecurring.Assign (IconAttributes.RecurringBitmap);          
        ShowRecurring := (dvBmpRecurring.Width <> 0) and                 
                         (dvBmpRecurring.Height <> 0);                   
      end;                                                               

      if Assigned (DataStore) then begin                                 
        case Event.Category of                                           
          0 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category0.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category0.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category0.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          1 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category1.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category1.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category1.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          2 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category2.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category2.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category2.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          3 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category3.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category3.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category3.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          4 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category4.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category4.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category4.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          5 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category5.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category5.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category5.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          6 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category6.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category6.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category6.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          7 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category7.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category7.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category7.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          8 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category8.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category8.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category8.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          9 : begin                                                      
            dvBmpCategory.Width  :=                                      
                DataStore.CategoryColorMap.Category9.Bitmap.Width;       
            dvBmpCategory.Height :=                                      
                DataStore.CategoryColorMap.Category9.Bitmap.Height;      
            dvBmpCategory.Canvas.CopyRect (                              
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height),  
                DataStore.CategoryColorMap.Category9.Bitmap.Canvas,      
                Rect (0, 0, dvBmpCategory.Width, dvBmpCategory.Height)); 
          end;                                                           

          else begin                                                     
            dvBmpCategory.Width  := 0;                                   
            dvBmpCategory.Height := 0;                                   
          end;                                                           
        end;                                                             
        ShowCategory := (dvBmpCategory.Width <> 0) and                   
                        (dvBmpCategory.Height <> 0);                     
      end;                                                               

      dvBmpCustom.Width  := 0;                                           
      dvBmpCustom.Height := 0;                                           

      if not IconAttributes.ShowAlarmBitmap then                         
        ShowAlarm := False;                                              
      if not IconAttributes.ShowCategoryBitmap then                      
        ShowCategory := False;                                           
      if not IconAttributes.ShowRecurringBitmap then                     
        ShowRecurring := False;                                          

      if Assigned (FOnDrawIcons) then begin                              
        Icons[itAlarm].Show := ShowAlarm;                                
        Icons[itAlarm].Bitmap := dvBmpAlarm;                             
        Icons[itRecurring].Show := ShowRecurring;                        
        Icons[itRecurring].Bitmap := dvBmpRecurring;                     
        Icons[itCategory].Show := ShowCategory;                          
        Icons[itCategory].Bitmap := dvBmpCategory;                       
        Icons[itCustom].Show := ShowCustom;                              
        Icons[itCustom].Bitmap := dvBmpCustom;                           

        FOnDrawIcons (Self, Event, Icons);                               

        ShowAlarm := Icons[itAlarm].Show;                                
        ShowRecurring := Icons[itRecurring].Show;                        
        ShowCategory := Icons[itCategory].Show;                          
        ShowCustom := Icons[itCustom].Show;                              
      end;                                                               

      if not ShowAlarm then begin                                        
        dvBmpAlarm.Width  := 0;                                          
        dvBmpAlarm.Height := 0;                                          
      end;                                                               

      if not ShowRecurring then begin                                    
        dvBmpRecurring.Width  := 0;                                      
        dvBmpRecurring.Height := 0;                                      
      end;                                                               

      if not ShowCategory then begin                                     
        dvBmpCategory.Width  := 0;                                       
        dvBmpCategory.Height := 0;                                       
      end;                                                               

      if not ShowCustom then begin                                       
        dvBmpCustom.Width  := 0;                                         
        dvBmpCustom.Height := 0;                                         
      end;                                                               

      AlarmW     := dvBmpAlarm.Width;                                    
      RecurringW := dvBmpRecurring.Width;                                
      CategoryW  := dvBmpCategory.Width;                                 
      CustomW    := dvBmpCustom.Width;                                   
      AlarmH     := dvBmpAlarm.Height;                                   
      RecurringH := dvBmpRecurring.Height;                               
      CategoryH  := dvBmpCategory.Height;                                
      CustomH    := dvBmpCustom.Height;                                  
    end;                                                                 
    {---}                                                                

    procedure ScaleIcons (EventRect : TRect);                            
    begin                                                                
      if (dvBmpAlarm.Height >                                            
          EventRect.Bottom - EventRect.Top - 2) and                      
         (dvBmpAlarm.Height * dvBmpAlarm.Width <> 0) then begin          
        AlarmW := Trunc (((EventRect.Bottom - EventRect.Top - 2) /       
                                 dvBmpAlarm.Height) *                    
                                dvBmpAlarm.Width);                       
        AlarmH := EventRect.Bottom - EventRect.Top - 2;                  
      end;                                                               

      if (dvBmpRecurring.Height >                                        
          EventRect.Bottom - EventRect.Top - 2) and                      
         (dvBmpRecurring.Height * dvBmpRecurring.Width <> 0) then begin  
        RecurringW := Trunc (((EventRect.Bottom - EventRect.Top - 2) /   
                                 dvBmpRecurring.Height) *                
                                dvBmpRecurring.Width);                   
        RecurringH := EventRect.Bottom - EventRect.Top - 2;              
      end;                                                               

      if (dvBmpCategory.Height >                                         
          EventRect.Bottom - EventRect.Top - 2) and                      
         (dvBmpCategory.Height * dvBmpCategory.Width <> 0) then begin    
        CategoryW := Trunc (((EventRect.Bottom - EventRect.Top - 2) /    
                                 dvBmpCategory.Height) *                 
                                dvBmpCategory.Width);                    
        CategoryH := EventRect.Bottom - EventRect.Top - 2;               
      end;                                                               

      if (dvBmpCustom.Height >                                           
          EventRect.Bottom - EventRect.Top - 2) and                      
         (dvBmpCustom.Height * dvBmpCustom.Width <> 0) then begin        
        CustomW := Trunc (((EventRect.Bottom - EventRect.Top - 2) /      
                                 dvBmpCustom.Height) *                   
                                dvBmpCustom.Width);                      
        CustomH := EventRect.Bottom - EventRect.Top - 2;                 
      end;                                                               
    end;                                                                 

    procedure DetermineIconSize (     EventRect : TRect;                 
                                      Event     : TVpEvent);             
    var                                                                  
      MaxHeight : Integer;                                               

    begin                                                                
      IconRect.Left := EventRect.Left;                                   
      IconRect.Right := EventRect.Left;                                  
      IconRect.Top := EventRect.Top;                                     
      IconRect.Bottom := EventRect.Bottom;                               

      IconRect.Right := IconRect.Right + AlarmW +                        
                        RecurringW + CategoryW +                         
                        CustomW + 2;                                     

      MaxHeight := AlarmH;                                               
      if RecurringH > MaxHeight then                                     
        MaxHeight := dvBmpRecurring.Height;                              
      if CategoryH > MaxHeight then                                      
        MaxHeight := dvBmpCategory.Height;                               
      if CustomH > MaxHeight then                                        
        MaxHeight := dvBmpCustom.Height;                                 
      if MaxHeight > EventRect.Bottom - EventRect.Top then               
        MaxHeight := EventRect.Bottom - EventRect.Top;                   
      IconRect.Bottom := EventRect.Top + MaxHeight;                      
      if IconRect.Right > EventRect.Right then                           
        IconRect.Right := EventRect.Right;                               
    end;                                                                 
    {---}                                                                

    procedure DrawIcons;                                                 
    var                                                                  
      DrawPos : Integer;                                                 

    begin                                                                
      DrawPos := 1;                                                      

      if (dvBmpCustom.Width <> 0) and                                    
         (dvBmpCustom.Height <> 0) then begin                            
        Canvas.CopyRect (Rect (IconRect.Left + 1,                        
                               IconRect.Top + 1,                         
                               IconRect.Left + CustomW + 1,              
                               IconRect.Top + CustomH + 1),              
                         dvBmpCustom.Canvas,                             
                         Rect (0,                                        
                               0,                                        
                               dvBmpCustom.Width,                        
                               dvBmpCustom.Height));                     
        DrawPos := CustomW + 1;                                          
      end;                                                               

      if (dvBmpCategory.Width <> 0) and                                  
         (dvBmpCategory.Height <> 0) then begin                          
        Canvas.CopyRect (Rect (IconRect.Left + DrawPos,                  
                               IconRect.Top + 1,                         
                               IconRect.Left + DrawPos + CategoryW + 1,  
                               IconRect.Top + CategoryH + 1),            
                         dvBmpCategory.Canvas,                           
                         Rect (0,                                        
                               0,                                        
                               dvBmpCategory.Width,                      
                               dvBmpCategory.Height));                   
        DrawPos := DrawPos + CategoryW;                                  
      end;                                                               

      if (dvBmpAlarm.Width <> 0) and (dvBmpAlarm.Height <> 0) then begin 
        Canvas.CopyRect (Rect (IconRect.Left + DrawPos,                  
                               IconRect.Top + 1,                         
                               IconRect.Left + DrawPos + AlarmW + 1,     
                               IconRect.Top + AlarmH + 1),               
                         dvBmpAlarm.Canvas,                              
                         Rect (0,                                        
                               0,                                        
                               dvBmpAlarm.Width,                         
                               dvBmpAlarm.Height));                      
        DrawPos := DrawPos + AlarmW;                                     
      end;                                                               

      if (dvBmpRecurring.Width <> 0) and                                 
         (dvBmpRecurring.Height <> 0) then                               
        Canvas.CopyRect (Rect (IconRect.Left + DrawPos,                  
                               IconRect.Top + 1,                         
                               IconRect.Left + DrawPos + RecurringW + 1, 
                               IconRect.Top + RecurringH + 1),           
                         dvBmpRecurring.Canvas,                          
                         Rect (0,                                        
                               0,                                        
                               dvBmpRecurring.Width,                     
                               dvBmpRecurring.Height));                  
    end;                                                                 
    {---}                                                                

var
  OKToDrawEditFrame : Boolean;
  TextRegion        : HRGN;                                              
  WorkRegion1       : HRGN;                                              
  WorkRegion2       : HRGN;                                              
  CW                : Integer;                                           
  EventIsEditing    : Boolean;                                           
  OldPen            : TPen;                                              
  OldBrush          : TBrush;                                            
  OldFont           : TFont;                                             

begin
  if (DataStore = nil)
  or (DataStore.Resource = nil)
  or (not DataStore.Connected) then
    Exit;

  { Save the canvas color and font }
  SaveColor := RenderCanvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(RenderCanvas.Font);

  { Initialize some stuff }
  if TimeFormat = tf24Hour then
    Format := 'h:mm'
  else
    Format := 'h:mmam/pm';

  { set the event array's size }
  SetLength(EventArray, MaxVisibleEvents);

  { Initialize the new matrix }
  for I := 0 to pred(MaxVisibleEvents) do begin
    EventArray[I].Event           := nil;
    EventArray[I].Level           := 0;
    EventArray[I].OLLevels        := 0;
    EventArray[I].WidthDivisor    := 0;
  end;

  EventList := TList.Create;
  try
    {Get all of the events for this day}
    DataStore.Resource.Schedule.EventsByDate(RenderDate, EventList);

    { Discard AllDayEvents, because they are drawn above. }
    for I := pred(EventList.Count) downto 0 do begin
      Event := EventList[I];
      if Event.AllDayEvent then begin
        EventList.Delete(I);
      end;
    end;

    { Arrange this day's events in the event matrix }
    Level := 0;
    I := 0;
    while EventList.Count > 0 do begin
      { Iterate through the events, and place them all in the proper     }
      { place in the EventMatrix, according to their start and end times }
      J := 0;
      ThisTime := 0.0;//Trunc(RenderDate);
      while (J < EventList.Count) and (J < MaxVisibleEvents) do begin
        Event := EventList[J];
        if Event.StartTime - Trunc(Event.StartTime) >= ThisTime then begin
          ThisTime := Event.EndTime - Trunc(Event.EndTime);
          { Handle end times of midnight }
          if ThisTime = 0 then                                           
            ThisTime := EncodeTime (23, 59, 59, 0);                      
          EventList.Delete(J);
          EventArray[I].Event := Event;
          EventArray[I].Level := Level;
          Inc(I);
          Continue;
        end
        else
          Inc(J);
      end;
      Inc(Level);
    end;

  finally
    EventList.Free;
  end;

  { Count the number of events which all share some of the same time }
  for I := 0 to pred(MaxVisibleEvents) do begin
    if EventArray[I].Event = nil then
      Break;
    EventArray[I].OLLevels := 1 + { it is necessary to count this event too }
      CountOverlappingEvents(TVpEvent(EventArray[I].Event), EventArray);
  end;

  { Calculate the largest width divisor of all overlapping events, }
  { for each event. }
  for I := 0 to pred(MaxVisibleEvents) do begin
    if EventArray[I].Event = nil then
      Break;
    EventArray[I].WidthDivisor := GetMaxOLEvents(
      TVpEvent(EventArray[I].Event), EventArray);
  end;

  {Make one last pass, to make sure that we have set up the width }
  { divisors properly }
  VerifyMaxWidthDivisors;

/////// Debug Code /////////
  { Dump a debug report to drive C }
  {$IFDEF DEBUGDV}
  SL := TStringList.Create;
  try
    I := 0;
    while EventArray[I].Event <> nil do begin
      SL.Add('Description: ' + TVpEvent(EventArray[I].Event).Description
        + #13#10 + '   Level: ' + IntToStr(EventArray[I].Level)
        + #13#10 + '   OLLevels: ' + IntToStr(EventArray[I].OLLevels)
        + #13#10 + '   WidthDivisor: ' + IntToStr(EventArray[I].WidthDivisor));
      Inc(I);
    end;
    SL.SaveToFile('C:\EventList' + IntToStr(Col) + '.txt');
  finally
    Sl.Free;
  end;
  {$ENDIF}
/////// Debug Code /////////

  { Time to paint 'em. Let's see if we calculated their placements correctly   }
  IconRect := Rect (0, 0, 0, 0);                                         
  CreateBitmaps;                                                         
  OldFont  := TFont.Create;                                              
  OldPen   := TPen.Create;                                               
  OldBrush := TBrush.Create;                                             
  try
    { get a rectangle of the visible area }
    VisibleRect := dvLineMatrix[Col, StartLine].Rec;
    VisibleRect.Bottom := ClientRect.Bottom;

    STime := dvLineMatrix[0, StartLine].Time;
    ETime := dvLineMatrix[0, StartLine + RealVisibleLines].Time;

    LineDuration := GetLineDuration(Granularity);
    { Determine how much time is represented by one pixel. It is the   }
    { amount of time represented by one line, divided by the height of }
    { a line in pixels. }
    PixelDuration := (LineDuration
       / (dvLineMatrix[Col, StartLine].Rec.Bottom -
          dvLineMatrix[Col, StartLine].Rec.Top));

    { Iterate through events and paint them }
    for I := 0 to pred(MaxVisibleEvents) do begin
      { get the next event }
      Event := TVpEvent(EventArray[I].Event);

      { if we have hit the end of the events, then bail out }
      if Event = nil then
        Break;

      { Find the line on which this event starts }
      EventSLine := GetStartLine(Event.StartTime, Granularity);
      { remove the date portion from the start and end times }
      EventSTime := Event.StartTime - trunc(Event.StartTime);
      EventETime := Event.EndTime - trunc(Event.EndTime);
      { Handle End Times of Midnight }
      if EventETime = 0 then                                             
        EventETime := EncodeTime (23, 59, 59, 0);                        

      { calculate the number of lines this event will cover }
      EventELine := GetEndLine(EventETime {Event.EndTime}, Granularity); 
      EventLineCount := EventELine - EventSLine + 1;
      EventDuration := EventETime - EventSTime;

      { if the event doesn't occupy area that is currently visible, }
      { then skip it. }
      if (EventELine < StartLine)
      or (EventSLine > StartLine + RealVisibleLines) then
        Continue;

      { Build the rectangle in which the event will be painted. }
      EventRect := dvLineMatrix[Col, EventSLine].Rec;
      if EventRect.Left < VisibleRect.Left then
        EventRect.Left := VisibleRect.Left;
      if EventRect.Top < VisibleRect.Top then
        EventRect.Top := VisibleRect.Top;
      EventRect.Bottom := dvLineMatrix[Col, EventELine].Rec.Bottom;
      if EventRect.Bottom < VisibleRect.Top then
        EventRect.Bottom := VisibleRect.Bottom;
      EventWidth := (VisibleRect.Right - VisibleRect.Left)
        div EventArray[I].WidthDivisor;

      { Slide the rect over to correspond with the level }
      if EventArray[I].Level > 0 then
        EventRect.Left := EventRect.Left + (EventWidth * EventArray[I].Level)
      { added because level 0 events were one pixel too far to the right }
      else                                                               
        EventRect.Left := EventRect.Left - 1;                            

      EventRect.Right := EventRect.Left + EventWidth - GutterWidth;

      { Draw the event rectangle }
      { paint Event text area clWindow                        }
      if Assigned (DataStore) then                                       
        case Event.Category of                                           
          0 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category0.BackgroundColor;  
          1 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category1.BackgroundColor;  
          2 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category2.BackgroundColor;  
          3 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category3.BackgroundColor;  
          4 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category4.BackgroundColor;  
          5 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category5.BackgroundColor;  
          6 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category6.BackgroundColor;  
          7 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category7.BackgroundColor;  
          8 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category8.BackgroundColor;  
          9 : RenderCanvas.Brush.Color :=                                
                  DataStore.CategoryColorMap.Category9.BackgroundColor;  
          else                                                           
            RenderCanvas.Brush.Color := WindowColor;                     
        end                                                              
      else                                                               
        RenderCanvas.Brush.Color := WindowColor;
      TPSFillRect (RenderCanvas, Angle, RenderIn, EventRect);

      { paint the little area to the left of the text the color }
      { corresponding to the event's category                   }
      { These colors are used even when printing }
      if Assigned (DataStore) then                                       
        RenderCanvas.Brush.Color := DataStore.CategoryColorMap.GetColor(
            Event.Category);

      { find the pixel offset to use for determining where to start and }
      { stop drawing colored area according to the start time and end   }
      { time of the event. }
      StartPixelOffset := 0;
      EndPixelOffset := 0;

      if (PixelDuration > 0)
      and (EventDuration < GetLineDuration(Granularity) * EventLineCount)
      then begin
        if (EventSLine >= StartLine)
        and (EventSTime > dvLineMatrix[0, EventSLine].Time)
        then begin
          { Get the start offset in TDateTime format }
          StartOffset := EventSTime - dvLineMatrix[0, EventSLine].Time;

          { determine how many pixels to scooch down before painting the   }
          { event's color code. }
          StartPixelOffset := trunc(StartOffset / PixelDuration);
        end;

        if (EventELine <= StartLine + RealVisibleLines)
        and (EventETime < dvLineMatrix[0, EventELine + 1].Time ) then
        begin
          { Get the end offset in TDateTime format }
          EndOffset := dvLineMatrix[0, EventELine + 1].Time  - EventETime;

          { determine how many pixels to scooch down before painting the   }
          { event's color code. }
          EndPixelOffset := trunc(EndOffset / PixelDuration);
        end;
      end;

      { Paint the gutter inside the EventRect all events }
      if (EventArray[I].Level = 0) then
        GutterRect.Left := EventRect.Left - Trunc (FGutterWidth * Scale)
      else
        GutterRect.Left := EventRect.Left;

      GutterRect.Right := GutterRect.Left + Round (FGutterWidth * Scale);
      GutterRect.Top := EventRect.Top + StartPixelOffset;
      GutterRect.Bottom := EventRect.Bottom - EndPixelOffset;

      TPSFillRect (RenderCanvas, Angle, RenderIn, GutterRect);

      RenderCanvas.Brush.Color := WindowColor;

      if (dvInPlaceEditor <> nil) then begin                             
        if FActiveEvent = Event then                                     
          EventIsEditing := True                                         
        else                                                             
          EventIsEditing := False;                                       
      end else                                                           
        EventIsEditing := False;                                         
      { build the event string }

      IconRect.Left   := EventRect.Left;                                 
      IconRect.Top    := EventRect.Top;                                  
      IconRect.Right  := EventRect.Left;                                 
      IconRect.Bottom := EventRect.Top;                                  
      if not DisplayOnly then begin                                      
        GetIcons (Event);                                                
        if EventArray[I].Level = 0 then begin                            
          ScaleIcons (EventRect);                                        
          DetermineIconSize (EventRect, Event);                          
        end else begin                                                   
          ScaleIcons (Rect (EventRect.Left + GutterWidth,                
                            EventRect.Top, EventRect.Right,              
                            EventRect.Bottom));                          
          DetermineIconSize (Rect (EventRect.Left + GutterWidth,         
                                   EventRect.Top, EventRect.Right,       
                                   EventRect.Bottom), Event);            
        end;                                                             
      end;                                                               

      OldPen.Assign (Canvas.Pen);                                        
      OldBrush.Assign (Canvas.Brush);                                    
      OldFont.Assign (Canvas.Font);                                      
      if Assigned (FOnBeforeDrawEvent) and                               
         (EventArray[I].Level = 0) then                                  
        FOnBeforeDrawEvent (Self, Event, FActiveEvent = Event,           
                            RenderCanvas, EventRect, IconRect)           
      else if Assigned (FOnBeforeDrawEvent) then                         
        FOnBeforeDrawEvent (Self, Event, FActiveEvent = Event,           
                            RenderCanvas,                                
                            Rect (EventRect.Left + FGutterWidth,         
                                  EventRect.Top, EventRect.Right,        
                                  EventRect.Bottom),                     
                            IconRect);                                   

      if not DisplayOnly then                                            
        DrawIcons;                                                       

      if ShowEventTimes then                                             
        EventString := FormatDateTime(Format, Event.StartTime) + '-' +
          FormatDateTime(Format, Event.EndTime) + ' ' + Event.Description
      else                                                               
        EventString := Event.Description;                                

      if WrapStyle = wsNone then begin                                   
        { if the string is longer than the availble space then chop    } 
        { off the and and place those little '...'s at the end         } 

      if RenderCanvas.TextWidth (EventString) >                          
             (EventRect.Right - IconRect.Right -                         
             Round (FGutterWidth * Scale) - TextMargin) then             
        EventString := GetDisplayString (                                
            RenderCanvas, EventString, 0,                                
            EventRect.Right - IconRect.Right -                           
            Round (FGutterWidth * Scale) - TextMargin);                  
      end;                                                               

      if (WrapStyle <> wsNone) and (not EventIsEditing) then begin       
        if (EventRect.Bottom <> IconRect.Bottom) and                     
           (EventRect.Left <> IconRect.Right) then begin                 
           if WrapStyle = wsIconFlow then begin
              WorkRegion1 := CreateRectRgn (IconRect.Right,              
                                            EventRect.Top,               
                                            EventRect.Right,             
                                            IconRect.Bottom);            
              WorkRegion2 := CreateRectRgn (EventRect.Left + GutterWidth,
                                            IconRect.Bottom,             
                                            EventRect.Right,             
                                            EventRect.Bottom);           
              TextRegion  := CreateRectRgn (IconRect.Right,              
                                            EventRect.Top,               
                                            EventRect.Right,             
                                            IconRect.Bottom);            
              CombineRgn (TextRegion, WorkRegion1, WorkRegion2, RGN_OR); 
           end else                                                      
             TextRegion := CreateRectRgn (IconRect.Right, EventRect.Top, 
                                          EventRect.Right,               
                                          EventRect.Bottom);             
        end else                                                         
          TextRegion := CreateRectRgn (IconRect.Right + GutterWidth,     
                                       EventRect.Top,                    
                                       EventRect.Right,                  
                                       EventRect.Bottom);                
        try                                                              
          CW := RenderTextToRegion (RenderCanvas, Angle, RenderIn,       
                                    TextRegion, EventString);            
          { write the event string to the proper spot in the EventRect } 
          if CW < Length (EventString) then begin                        
            RenderCanvas.Brush.Color := DotDotDotColor;                  
            { draw dot dot dot }                                         
            TPSFillRect (RenderCanvas, Angle, RenderIn,                  
                         Rect (EventRect.Right - 20,                     
                               EventRect.Bottom - 7,                     
                               EventRect.Right - 17,                     
                               EventRect.Bottom - 4));                   
            TPSFillRect (RenderCanvas, Angle, RenderIn,                  
                         Rect (EventRect.Right - 13,                     
                               EventRect.Bottom - 7,                     
                               EventRect.Right - 10,                     
                               EventRect.Bottom - 4));                   
            TPSFillRect (RenderCanvas, Angle, RenderIn,                  
                         Rect (EventRect.Right -  6,                     
                               EventRect.Bottom - 7,                     
                               EventRect.Right -  3,                     
                               EventRect.Bottom - 4));                   
          end;                                                           

        finally                                                          
          if ((EventRect.Bottom > IconRect.Bottom) and                   
              (EventRect.Left > IconRect.Right)) or                      
              (WrapStyle = wsNoFlow) then begin                          
            DeleteObject (WorkRegion1);                                  
            DeleteObject (WorkRegion2);                                  
            DeleteObject (TextRegion);                                   
          end else begin                                                 
            DeleteObject (TextRegion);                                   
          end;                                                           
        end;                                                             
      end else if (not EventIsEditing) then begin                        
        if EventArray[I].Level = 0 then                                  
          { don't draw the gutter in the EventRest for level 0 events. } 
          TPSTextOut (RenderCanvas, Angle, RenderIn,                     
                      IconRect.Right + GutterWidth + TextMargin,         
                      EventRect.Top + TextMargin, EventString)           
        else                                                             
          TPSTextOut (RenderCanvas, Angle, RenderIn,                     
                      IconRect.Right + GutterWidth + TextMargin,         
                      EventRect.Top + TextMargin, EventString);          
      end;                                                               

      { paint the borders around the event text area }
      TPSPolyline (RenderCanvas, Angle, RenderIn,
                   [Point (EventRect.Left, EventRect.Top),
                    Point (EventRect.Right, EventRect.Top),
                    Point (EventRect.Right, EventRect.Bottom),
                    Point (EventRect.Left, EventRect.Bottom),
                    Point (EventRect.Left, EventRect.Top)]);
      { don't paint gutter area on level 0 items }
      if EventArray[I].Level > 0 then begin
        TPSMoveTo (RenderCanvas, Angle, RenderIn,
                   EventRect.Left + Round (FGutterWidth * Scale),
                   EventRect.Top);
        TPSLineTo (RenderCanvas, Angle, RenderIn,
                   EventRect.Left + Round (FGutterWidth * Scale),
                   EventRect.Bottom);
      end;

      if Assigned (FOnAfterDrawEvent) and                                
         (EventArray[I].Level = 0) then                                  
        FOnAfterDrawEvent (Self, Event, FActiveEvent = Event,            
                           RenderCanvas, EventRect, IconRect)            
      else if Assigned (FOnAfterDrawEvent) then                          
        FOnAfterDrawEvent (Self, Event, FActiveEvent = Event,            
                           RenderCanvas,                                 
                           Rect (EventRect.Left + FGutterWidth,          
                                 EventRect.Top, EventRect.Right,         
                                 EventRect.Bottom),                      
                           IconRect);                                    

      Canvas.Brush.Assign (OldBrush);                                    
      Canvas.Pen.Assign (OldPen);                                        
      Canvas.Font.Assign (OldFont);                                      

      dvEventArray[EventCount].Rec := Rect (EventRect.Left,              
                                            EventRect.Top,               
                                            EventRect.Right,             
                                            EventRect.Bottom + 1);       
      dvEventArray[EventCount].IconRect := IconRect;                     
      dvEventArray[EventCount].Event := Event;
      Inc(EventCount);
    end;

    OKToDrawEditFrame := True;
    if Assigned (FActiveEvent) then
      OKToDrawEditFrame := not (FActiveEvent.AllDayEvent);

    if (dvInPlaceEditor <> nil) and (OKToDrawEditFrame) then begin
      { paint extra borders around the editor }
      if Assigned (DataStore) then                                       
        RenderCanvas.Brush.Color := DataStore.CategoryColorMap.GetColor(
          FActiveEvent.Category);
      RenderCanvas.Pen.Color := clWindowFrame;
      TPSFillRect (RenderCanvas, Angle, RenderIn,
                   Rect(dvActiveEventRec.Left,
                   dvActiveEventRec.Top - FGutterWidth,
                   dvActiveEventRec.Right,
                   dvActiveEventRec.Top));
      TPSPolyline (RenderCanvas, Angle, RenderIn,
                   [Point(dvActiveEventRec.Left, dvActiveEventRec.Top),
                    Point(dvActiveEventRec.Left,
                      dvActiveEventRec.Top - FGutterWidth),
                    Point(dvActiveEventRec.Right,
                      dvActiveEventRec.Top - FGutterWidth),
                    Point(dvActiveEventRec.Right, dvActiveEventRec.Top)]);
      TPSFillRect (RenderCanvas, Angle, RenderIn,
                   Rect(dvActiveEventRec.Left,
                   dvActiveEventRec.Bottom, dvActiveEventRec.Right,
                   dvActiveEventRec.Bottom + FGutterWidth));
      TPSPolyline (RenderCanvas, Angle, RenderIn,
                   [Point(dvActiveEventRec.Left, dvActiveEventRec.Bottom),
                    Point(dvActiveEventRec.Left,
                      dvActiveEventRec.Bottom + FGutterWidth),
                    Point(dvActiveEventRec.Right,
                      dvActiveEventRec.Bottom + FGutterWidth),
                    Point(dvActiveEventRec.Right, dvActiveEventRec.Bottom)]);
    end;

      { Clean Up }
    finally
      try                                                                
        FreeBitmaps;                                                     
      finally                                                            
        { restore canvas color and font }
        RenderCanvas.Brush.Color := SaveColor;
        RenderCanvas.Font.Assign(SaveFont);
        SaveFont.Free;
        OldFont.Free;                                                    
        OldPen.Free;                                                     
        OldBrush.Free;                                                   
      end;                                                               
    end;
  end;

  procedure DrawCells (R : TRect; ColDate: TDateTime; Col: Integer);
  var
    I             : Integer;
    LineRect      : TRect;
    SavedFont     : TFont;
    GutterRect    : TRect;
    LineStartTime : Double;

  begin
    if StartLine < 0 then
      StartLine := TopLine;

    { Set GutterRect size }
    GutterRect.Left := R.Left;
    GutterRect.Top := R.Top;
    GutterRect.Bottom := R.Bottom;
    GutterRect.Right := GutterRect.Left + Round (GutterWidth * Scale);
    R.Left := R.Left + Round (GutterWidth * Scale) + 1;

    { paint gutter area }
    RenderCanvas.Brush.Color := RealColor;
    TPSFillRect (RenderCanvas, Angle, RenderIn, GutterRect);
    { draw the line down the right side of the gutter }
    RenderCanvas.Pen.Color := BevelShadow;
    RenderCanvas.Pen.Style := psSolid;
    TPSMoveTo(RenderCanvas, Angle, RenderIn, GutterRect.Right, GutterRect.Top);
    TPSLineTo(RenderCanvas, Angle, RenderIn, GutterRect.Right, GutterRect.Bottom);

    for I := 0 to LineCount do begin
      dvLineMatrix[Col, I].Rec.Left := -1;
      dvLineMatrix[Col, I].Rec.Top := -1;
      dvLineMatrix[Col, I].Rec.Right := -1;
      dvLineMatrix[Col, I].Rec.Bottom := -1;
    end;

    SavedFont := TFont.Create;
    SavedFont.Assign(RenderCanvas.Font);
    try
      RenderCanvas.Font.Assign(Font);
      RenderCanvas.Brush.Color := RealColor;
      TPSFillRect (RenderCanvas, Angle, RenderIn, R);

      LineRect := Rect(R.left, R.top, R.Right, R.Top + RealRowHeight);
      RenderCanvas.Pen.Style := psSolid;
      RenderCanvas.Pen.Color := LineColor;

      { Paint the client area }
      for I := 0 to RealVisibleLines do begin

        if (I > pred(FLineCount)) then
          Break;

        if TopLine + i >= FLineCount then
          Break;

        RenderCanvas.Brush.Color := RealColor;
        RenderCanvas.Font.Assign(SavedFont);
        LineRect.Top := Round (R.Top + (i * RealRowHeight));
        LineRect.Bottom := Round (LineRect.Top + (RealRowHeight));
        if I + StartLine < LineCount then
          dvLineMatrix[Col, I + StartLine].Rec := LineRect;

        { color-code cells }

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // !!!! This causes problems at design time - implement a better   !!!!
        // !!!! Fix - check the value after the component is streamed in   !!!!
        // !!!! May be a good use for ... loaded or in my message          !!!!
        // !!!! Handler (the message handler would be better               !!!!
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//        if ActiveRow = -1 then                                         
//         ActiveRow := TopLine;                                         

        if not DisplayOnly then begin
          if (Focused)
          and (FActiveCol = col)
          and (FActiveRow = StartLine + I)
          then begin
            { Paint background hilight color }
            RenderCanvas.Brush.Color := HighlightBkg;
            RenderCanvas.Font.Color := HighlightText;
            TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
          end else begin
            { paint the active, inactive, weekend, and holiday colors }

            { HOLIDAY COLORS ARE NOT IMPLEMENTED YET }

            { if ColDate is a weekend, then paint all rows the weekend }
            { color. }
            if (DayOfWeek(ColDate) = 1) or (DayOfWeek(ColDate) = 7) then begin
              { this is a weekend }
              RenderCanvas.Brush.Color := TimeSlotColors.Weekend;
              TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
            end

            else begin
            { ColDate is a weekday, so check to see if the active     }
            { range is set. If it isn't then paint all rows the color }
            { corresponding to Weekday. If it is, then paint inactive }
            { rows the color corresponding to inactive and the active }
            { rows the color corresponding to Active Rows.            }
              if TimeSlotColors.ActiveRange.RangeBegin
                = TimeSlotColors.ActiveRange.RangeEnd then begin
                { there is no active range, so all time slots are to be }
                { painted the color of Weekday }
                RenderCanvas.Brush.Color := TimeSlotColors.Weekday;
                TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
              end

              else begin
                { there is an active range defined, so we need to see if }
                { the current line falls in the active range or not, and }
                { paint it accordingly }
                LineStartTime := dvLineMatrix[Col, StartLine + I].Time;
                if TimeInRange(LineStartTime,
                  TimeSlotColors.ActiveRange.StartTime,
                  TimeSlotColors.ActiveRange.EndTime - (1/MinutesInDay), true)
                then begin
                  RenderCanvas.Brush.Color := TimeSlotColors.Active;
                  TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
                end else begin
                  RenderCanvas.Brush.Color := TimeSlotColors.Inactive;
                  TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
                end;
              end;
            end;
          end;
        end;

        { Draw the lines }
        if I + StartLine <= LineCount then begin
          RenderCanvas.Pen.Color := LineColor;
          TPSMoveTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Left, LineRect.Top);
          TPSLineTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Right - 1, LineRect.Top);
          TPSMoveTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Left, LineRect.Bottom);
          TPSLineTo (RenderCanvas, Angle, RenderIn,
                     LineRect.Right - 1, LineRect.Bottom);
        end;
      end;

      { Draw a line down the right side of the column to close the }
      { cells right sides }
      RenderCanvas.Pen.Color := BevelShadow;
      RenderCanvas.Pen.Style := psSolid;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, R.Right - 1, R.Bottom);
      TPSLineTo (RenderCanvas, Angle, RenderIn, R.Right - 1, R.Top - 1);

      RenderCanvas.Font.Assign(SavedFont);
    finally
      SavedFont.Free;
    end;
  end;


  procedure DrawAllDays;
  var
    i           : Integer;
    RPos        : Integer;
    AllDayWidth : Integer;
    ExtraSpace  : Integer;
    DrawMe      : Boolean;
    RealDay     : Integer;

  begin
    if RealNumDays = 0 then begin
      while (DayOfWeek (RenderDate) = 1) or (DayOfWeek (RenderDate) = 7) do
        RenderDate := RenderDate + 1;
      RealNumDays := FNumDays;
    end;
    AllDayWidth := RealWidth - RealRowHeadWidth - 1 - ScrollBarOffset;

    DayWidth := AllDayWidth div FNumDays;
    ExtraSpace := AllDayWidth mod FNumDays;

    RPos := RowHeadRect.Right;

    RealDay := 0;
    for i := 0 to RealNumDays - 1 do begin
      DrawMe := True;
      if not FIncludeWeekends then begin
        if (DayOfWeek (RenderDate + i) = 1) or
           (DayOfWeek (RenderDate + i) = 7) then
          DrawMe := False
      end;
      if DrawMe then begin
        { Draw Column Header }
        ColHeadRect := Rect (RPos,
                             RealTop + 2,
                             RPos + DayWidth - 1,
                             RealTop + RealColHeadHeight);

        if (i = RealNumDays - 1) and (ExtraSpace > 0) then
          ColHeadRect.Right := ColHeadRect.Right + ExtraSpace;

        if Assigned(FOwnerDrawColHead) then begin
          Drawn := false;
          FOwnerDrawColHead (self, RenderCanvas, ColHeadRect, Drawn);
          if not Drawn then
            dvDrawColHeader (ColHeadRect, RenderDate + i, RealDay);
        end else
          dvDrawColHeader (ColHeadRect, RenderDate + i, RealDay);

        { Calculate the column rect for this day }
        RenderCanvas.Font.Assign(Font);
        CellsRect := Rect (RPos,
                           ADEventsRect.Bottom + 1,
                           RPos + DayWidth,
                           RealBottom - 2);

        if (i = RealNumDays - 1) and (ExtraSpace > 0) then
          CellsRect.Right := CellsRect.Right + ExtraSpace;

        { set the ColRectArray }
        dvColRectArray[RealDay].Rec := CellsRect;
        dvColRectArray[RealDay].Date := RenderDate + i;

        { Draw the cells }
        if Assigned(FOwnerDrawCells) then begin
          FOwnerDrawCells(self, RenderCanvas, CellsRect, RealRowHeight, Drawn);
          if not Drawn then
            DrawCells (CellsRect, RenderDate + i, RealDay);
        end else
          DrawCells (CellsRect, RenderDate + i, RealDay);

        { Draw the regular events }
        DrawEvents(RenderDate + i, RealDay); 

        Inc (RPos, DayWidth);
        Inc (RealDay);
      end;
    end;
  end;

  procedure InitializeEventRectangles;
  var
    I : Integer;

  begin
    EventCount := 0;
    for I := 0 to pred(Length(dvEventArray)) do begin
      dvEventArray[I].Rec.Left := -1;
      dvEventArray[I].Rec.Top := -1;
      dvEventArray[I].Rec.Right := -1;
      dvEventArray[I].Rec.Bottom := -1;
      dvEventArray[I].Event := nil;
    end;
  end;

begin
  if DisplayOnly then begin
    BevelShadow          := clBlack;
    BevelHighlight       := clBlack;
    BevelDarkShadow      := clBlack;
    BevelFace            := clBlack;
    WindowColor          := clWhite;
    HighlightText        := clBlack;
    RealHeadAttrColor    := clSilver;
    RealRowHeadAttrColor := clSilver;
    RealLineColor        := clBlack;
    RealColor            := clWhite;
    HighlightBkg         := clWhite;
    RealADEventBkgColor  := clWhite;                                     
    ADEventAttrBkgColor  := clWhite;                                     
    ADEventBorderColor   := clBlack;                                     
  end else begin
    BevelShadow          := clBtnShadow;
    BevelHighlight       := clBtnHighlight;
    BevelDarkShadow      := cl3DDkShadow;
    BevelFace            := clBtnFace;
    WindowColor          := clWindow;
    HighlightText        := clHighlightText;
    HighlightBkg         := clHighlight;
    RealHeadAttrColor    := FHeadAttr.Color;
    RealRowHeadAttrColor := FRowHeadAttr.Color;
    RealLineColor        := LineColor;
    RealColor            := Color;
    RealADEventBkgColor  := AllDayEventAttributes.BackgroundColor;       
    ADEventAttrBkgColor  := AllDayEventAttributes.EventBackgroundColor;  
    ADEventBorderColor   := AllDayEventAttributes.EventBorderColor;      
  end;

  SetMeasurements;

  if StartLine < 0 then
    StartLine := TopLine;

  if DisplayOnly then
    ScrollBarOffset := 2
  else
    ScrollBarOffset := 14;

  dvPainting     := true;
  SavePenStyle   := RenderCanvas.Pen.Style;
  SaveBrushColor := RenderCanvas.Brush.Color;
  SavePenColor   := RenderCanvas.Pen.Color;

  Rgn := CreateRectRgn (RenderIn.Left, RenderIn.Top,
                        RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn (RenderCanvas.Handle, Rgn);

    { Calculate Row Header }
    RealRowHeight := dvCalcRowHeight (Scale, UseGran);
    RealColHeadHeight := dvCalcColHeadHeight (Scale);

    RenderCanvas.Font.Assign(FRowHeadAttr.FHourFont);
    TextWidth := RenderCanvas.TextWidth('33');
    RealRowHeadWidth := TextWidth * 2 + 10;

    { initialize the All Day Events area... }
    ADEventsRect.Left   := RealLeft + 3 + RealRowHeadWidth;
    ADEventsRect.Top    := RealTop + RealColHeadHeight;
    ADEventsRect.Right  := ClientRect.Right;
    ADEventsRect.Bottom := AdEventsRect.Top;

    { Calculate the RealNumDays (The number of days the control covers) }
    RealNumDays := GetRealNumDays (RenderDate);

    InitializeEventRectangles;

    { Draw the All Day Events }
    DrawAllDayEvents;

    { draw the area in the top left corner, where the nav buttons go. }
    RowHeadRect := Rect(RealLeft + 1,
                        RealTop,
                        RealLeft + 3 + RealRowHeadWidth,
                        RealTop + RealColHeadHeight + 2);

    RenderCanvas.Brush.Color := RealHeadAttrColor;
    TPSFillRect(RenderCanvas, Angle, RenderIn, RowHeadRect);

    if DrawingStyle = ds3d then
      DrawBevelRect(RenderCanvas, TPSRotateRectangle (Angle, RenderIn,
        Rect (RowHeadRect.Left + 1, RowHeadRect.Top + 2, RowHeadRect.Right - 2,
          RowHeadRect.Bottom - 2)), BevelHighlight, BevelShadow)
    else begin
      RenderCanvas.Pen.Color := BevelShadow;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2,
        RowHeadRect.Bottom - 2);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Left,
        RowHeadRect.Bottom - 2);
      RenderCanvas.Pen.Color := BevelHighlight;
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Left,
        RowHeadRect.Top);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2,
        RowHeadRect.Top);
      RenderCanvas.Pen.Color := BevelShadow;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2,
        RowHeadRect.Top + 6);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2,
        RowHeadRect.Bottom - 5);
    end;


    RenderCanvas.Font.Assign(FRowHeadAttr.FHourFont);
    if DrawingStyle = dsFlat then
      RowHeadRect := Rect(RealLeft + 2,
                          ADEventsRect.Bottom + 1,
                          RealLeft + 2 + RealRowHeadWidth,
                          RealBottom)
    else
      RowHeadRect := Rect (RealLeft + 1,
                           ADEventsRect.Bottom + 1,
                           RealLeft + 2 + RealRowHeadWidth,
                           RealBottom);

    if Assigned(FOwnerDrawRowHead) then begin
      Drawn := false;
      FOwnerDrawRowHead (self, RenderCanvas, RowHeadRect, RealRowHeight, Drawn);
      if not Drawn then
        dvDrawRowHeader (RowHeadRect);
    end else
      dvDrawRowHeader (RowHeadRect);

    { Draw the regular events }
    DrawAllDays; 

    { Draw Borders }
    if FDrawingStyle = dsFlat then begin
      { Draw an outer and inner bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft,
                                               RealTop,
                                               RealRight - 1,
                                               RealBottom - 1)),
                     BevelShadow, BevelHighlight);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1,
                                               RealTop + 1,
                                               RealRight - 2,
                                               RealBottom - 2)),
                     BevelHighlight, BevelShadow);
    end else if FDrawingStyle = ds3d then begin
    { Draw a 3d bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft,
                                               RealTop,
                                               RealRight - 1,
                                               RealBottom - 1)),
                     BevelShadow, BevelHighlight);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1,
                                               RealTop + 1,
                                               RealRight - 2,
                                               RealBottom - 2)),
                     BevelDarkShadow, BevelFace);
    end;

    { Place navigation buttons               }
    { size and place the Today button first. }
    dvTodayBtn.Height := trunc(RealColHeadHeight div 2);
    if DrawingStyle = dsFlat then begin
      dvTodayBtn.Left := 1;
      dvTodayBtn.Top := 1;
      dvTodayBtn.Width := RealRowHeadWidth + 1;
    end else begin
      dvTodayBtn.Left := 2;
      dvTodayBtn.Top := 2;
      dvTodayBtn.Width := RealRowHeadWidth;
    end;
    { size and place the WeekDown button     }
    dvWeekDownBtn.Height := dvTodayBtn.Height;
    dvWeekDownBtn.Width := trunc(RealRowHeadWidth * 0.25) + 2;
    dvWeekDownBtn.Left := dvTodayBtn.Left;
    dvWeekDownBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
    { size and place the DayDown button      }
    dvDayDownBtn.Height := dvTodayBtn.Height;
    dvDayDownBtn.Width := dvWeekDownBtn.Width - 4;
    dvDayDownBtn.Left := dvWeekDownBtn.Left + dvWeekDownBtn.Width;
    dvDayDownBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
    { size and place the DayUp button        }
    dvDayUpBtn.Height := dvTodayBtn.Height;
    dvDayUpBtn.Width := dvWeekDownBtn.Width - 4;
    dvDayUpBtn.Left := dvDayDownBtn.Left + dvDayDownBtn.Width;
    dvDayUpBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
    { size and place the WeekUp button       }
    dvWeekUpBtn.Height := dvTodayBtn.Height;
    dvWeekUpBtn.Width := dvTodayBtn.Width - dvWeekDownBtn.Width
      - dvDayDownBtn.Width - dvDayUpBtn.Width;
    dvWeekUpBtn.Left := dvDayUpBtn.Left + dvDayUpBtn.Width;
    dvWeekUpBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;

    { Reinstate RenderCanvas settings        }
    RenderCanvas.Pen.Style := SavePenStyle;
    RenderCanvas.Brush.Color := SaveBrushColor;
    RenderCanvas.Pen.Color := SavePenColor;

  finally
    SelectClipRgn (RenderCanvas.Handle, 0);
    DeleteObject (Rgn);
  end;

  dvPainting := false;
end;
{=====}

procedure TVpDayView.VpDayViewInit (var Msg : TMessage);               
begin                                                                  
  if csLoading in ComponentState then begin                            
    PostMessage (Handle, Vp_DayViewInit, 0, 0);                        
    Exit;                                                              
  end;                                                                 

  dvCalcColHeadHeight (1);                                             
  dvCalcRowHeight (1, FGranularity);                                   
  dvCalcVisibleLines (Height, dvColHeadHeight, dvRowHeight, 1,         
                      TopLine, -1);                                    
  SetVScrollPos;                                                       
end;                                                                   

(*****************************************************************************)
{ TVpCHAttributes }

constructor TVpCHAttributes.Create(AOwner: TVpDayView);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TVpFont.Create(AOwner);
end;
{=====}

destructor TVpCHAttributes.Destroy;
begin
  FFont.Free;
  inherited;
end;
{=====}

procedure TVpCHAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpCHAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
end;
{=====}

(*****************************************************************************)
{ TVpRHAttributes }

constructor TVpRHAttributes.Create(AOwner: TVpDayView);
begin
  inherited Create;
  FOwner := AOwner;
  FHourFont := TVpFont.Create(AOwner);
  FHourFont.Name := 'Tahoma';
  FMinuteFont := TVpFont.Create(AOwner);
  FMinuteFont.Name := 'Tahoma';
end;
{=====}

destructor TVpRHAttributes.Destroy;
begin
  FHourFont.Free;
  FMinuteFont.Free;
  inherited;
end;
{=====}

procedure TVpRHAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpRHAttributes.SetHourFont(Value: TVpFont);
begin
  if Value <> FHourFont then begin
    FHourFont.Assign(Value);
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpRHAttributes.SetMinuteFont(Value: TVpFont);
begin
  if Value <> FMinuteFont then begin
    FMinuteFont.Assign(Value);
    FOwner.Invalidate;
  end;
end;
{=====}

end.
